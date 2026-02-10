🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 21.7 Message Queues et Event Bus

## Introduction

Les **Message Queues** (files de messages) et les **Event Bus** (bus d'événements) sont des patterns de communication asynchrone qui permettent aux composants d'une application de communiquer sans couplage direct.

### Qu'est-ce qu'une Message Queue ?

Une **message queue** est une file d'attente où les messages sont stockés temporairement avant d'être traités.

```
PRODUCTEUR → [Queue] → CONSOMMATEUR
              │
              ├─ Message 1
              ├─ Message 2
              └─ Message 3
```

**Analogie :** Comme une boîte aux lettres. L'expéditeur dépose le courrier, le destinataire le récupère quand il peut.

### Qu'est-ce qu'un Event Bus ?

Un **event bus** est un canal de communication centralisé où les composants publient et s'abonnent à des événements.

```
    Composant A ──┐
                  │
    Composant B ──┼──► [EVENT BUS] ──┬──► Composant D
                  │                   │
    Composant C ──┘                   └──► Composant E
```

**Analogie :** Comme une station de radio. Les émetteurs diffusent, les auditeurs écoutent ce qui les intéresse.

### Pourquoi utiliser ces patterns ?

**Avantages :**
- ✅ **Découplage** : Les composants ne se connaissent pas directement
- ✅ **Asynchrone** : Pas d'attente bloquante
- ✅ **Scalabilité** : Traitement parallèle possible
- ✅ **Résilience** : Si un composant plante, les autres continuent
- ✅ **Flexibilité** : Ajouter/retirer des composants facilement

**Cas d'usage :**
- Communication entre microservices
- Traitement de tâches en arrière-plan
- Notifications en temps réel
- Architecture événementielle
- Intégration de systèmes

## Message Queue de base

### 1. Structure d'un message

```pascal
unit MessageQueue.Types;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson;

type
  // Priorité du message
  TMessagePriority = (mpLow, mpNormal, mpHigh, mpCritical);

  // Message générique
  TMessage = class
  private
    FId: string;
    FType: string;
    FPayload: TJSONObject;
    FTimestamp: TDateTime;
    FPriority: TMessagePriority;
    FRetryCount: Integer;
  public
    constructor Create(const AType: string; APayload: TJSONObject;
                      APriority: TMessagePriority = mpNormal);
    destructor Destroy; override;

    property Id: string read FId;
    property MessageType: string read FType;
    property Payload: TJSONObject read FPayload;
    property Timestamp: TDateTime read FTimestamp;
    property Priority: TMessagePriority read FPriority write FPriority;
    property RetryCount: Integer read FRetryCount write FRetryCount;
  end;

implementation

constructor TMessage.Create(const AType: string; APayload: TJSONObject;
  APriority: TMessagePriority);
var
  G: TGUID;
begin
  inherited Create;
  CreateGUID(G);
  FId := GUIDToString(G);
  FType := AType;
  FPayload := APayload;
  FTimestamp := Now;
  FPriority := APriority;
  FRetryCount := 0;
end;

destructor TMessage.Destroy;  
begin
  FPayload.Free;
  inherited;
end;

end.
```

### 2. Message Queue thread-safe

```pascal
unit MessageQueue.Queue;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, SyncObjs,
  MessageQueue.Types;

type
  // File de messages thread-safe
  TMessageQueue = class
  private
    FMessages: TThreadList;
    FMaxSize: Integer;
    FSemaphore: TSemaphore;
    FLock: TCriticalSection;
  public
    constructor Create(AMaxSize: Integer = 1000);
    destructor Destroy; override;

    // Ajout de messages
    function Enqueue(AMessage: TMessage): Boolean;
    function TryEnqueue(AMessage: TMessage; ATimeout: Cardinal): Boolean;

    // Récupération de messages
    function Dequeue: TMessage;
    function TryDequeue(out AMessage: TMessage; ATimeout: Cardinal): Boolean;

    // Informations
    function Count: Integer;
    function IsEmpty: Boolean;
    function IsFull: Boolean;

    // Gestion
    procedure Clear;
  end;

implementation

constructor TMessageQueue.Create(AMaxSize: Integer);  
begin
  inherited Create;
  FMessages := TThreadList.Create;
  FMaxSize := AMaxSize;
  FSemaphore := TSemaphore.Create(nil, 0, AMaxSize, '');
  FLock := TCriticalSection.Create;
end;

destructor TMessageQueue.Destroy;  
var
  List: TList;
  i: Integer;
begin
  // Libérer tous les messages restants
  List := FMessages.LockList;
  try
    for i := 0 to List.Count - 1 do
      TMessage(List[i]).Free;
    List.Clear;
  finally
    FMessages.UnlockList;
  end;

  FSemaphore.Free;
  FLock.Free;
  FMessages.Free;
  inherited;
end;

function TMessageQueue.Enqueue(AMessage: TMessage): Boolean;  
var
  List: TList;
begin
  Result := False;

  FLock.Enter;
  try
    List := FMessages.LockList;
    try
      if List.Count >= FMaxSize then
      begin
        WriteLn('[Queue] File pleine, message rejeté');
        Exit(False);
      end;

      List.Add(AMessage);
      Result := True;

      WriteLn(Format('[Queue] Message ajouté: %s (Count: %d)',
        [AMessage.MessageType, List.Count]));
    finally
      FMessages.UnlockList;
    end;

    if Result then
      FSemaphore.Release;
  finally
    FLock.Leave;
  end;
end;

function TMessageQueue.TryEnqueue(AMessage: TMessage; ATimeout: Cardinal): Boolean;  
begin
  // Pour simplifier, même implémentation que Enqueue
  Result := Enqueue(AMessage);
end;

function TMessageQueue.Dequeue: TMessage;  
var
  List: TList;
begin
  Result := nil;

  // Attendre qu'un message soit disponible
  FSemaphore.WaitFor(INFINITE);

  FLock.Enter;
  try
    List := FMessages.LockList;
    try
      if List.Count > 0 then
      begin
        Result := TMessage(List[0]);
        List.Delete(0);

        WriteLn(Format('[Queue] Message récupéré: %s (Restant: %d)',
          [Result.MessageType, List.Count]));
      end;
    finally
      FMessages.UnlockList;
    end;
  finally
    FLock.Leave;
  end;
end;

function TMessageQueue.TryDequeue(out AMessage: TMessage; ATimeout: Cardinal): Boolean;  
var
  List: TList;
begin
  Result := False;
  AMessage := nil;

  // Attendre avec timeout
  if FSemaphore.WaitFor(ATimeout) <> wrSignaled then
    Exit(False);

  FLock.Enter;
  try
    List := FMessages.LockList;
    try
      if List.Count > 0 then
      begin
        AMessage := TMessage(List[0]);
        List.Delete(0);
        Result := True;

        WriteLn(Format('[Queue] Message récupéré: %s', [AMessage.MessageType]));
      end;
    finally
      FMessages.UnlockList;
    end;
  finally
    FLock.Leave;
  end;
end;

function TMessageQueue.Count: Integer;  
var
  List: TList;
begin
  List := FMessages.LockList;
  try
    Result := List.Count;
  finally
    FMessages.UnlockList;
  end;
end;

function TMessageQueue.IsEmpty: Boolean;  
begin
  Result := Count = 0;
end;

function TMessageQueue.IsFull: Boolean;  
begin
  Result := Count >= FMaxSize;
end;

procedure TMessageQueue.Clear;  
var
  List: TList;
  i: Integer;
begin
  FLock.Enter;
  try
    List := FMessages.LockList;
    try
      for i := 0 to List.Count - 1 do
        TMessage(List[i]).Free;
      List.Clear;

      WriteLn('[Queue] File vidée');
    finally
      FMessages.UnlockList;
    end;
  finally
    FLock.Leave;
  end;
end;

end.
```

### 3. Message Producer (Producteur)

```pascal
unit MessageQueue.Producer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson,
  MessageQueue.Types, MessageQueue.Queue;

type
  TMessageProducer = class
  private
    FQueue: TMessageQueue;
    FProducerName: string;
  public
    constructor Create(const AName: string; AQueue: TMessageQueue);

    procedure SendMessage(const AType: string; AData: TJSONObject;
                         APriority: TMessagePriority = mpNormal);
    procedure SendSimpleMessage(const AType, AText: string);

    property ProducerName: string read FProducerName;
  end;

implementation

constructor TMessageProducer.Create(const AName: string; AQueue: TMessageQueue);  
begin
  inherited Create;
  FProducerName := AName;
  FQueue := AQueue;
end;

procedure TMessageProducer.SendMessage(const AType: string; AData: TJSONObject;
  APriority: TMessagePriority);
var
  Message: TMessage;
begin
  WriteLn(Format('[Producer %s] Envoi message: %s', [FProducerName, AType]));

  Message := TMessage.Create(AType, AData, APriority);

  if not FQueue.Enqueue(Message) then
  begin
    WriteLn('[Producer] ERREUR: Impossible d''ajouter le message');
    Message.Free;
  end;
end;

procedure TMessageProducer.SendSimpleMessage(const AType, AText: string);  
var
  Data: TJSONObject;
begin
  Data := TJSONObject.Create;
  Data.Add('text', AText);
  Data.Add('producer', FProducerName);
  Data.Add('timestamp', FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));

  SendMessage(AType, Data);
end;

end.
```

### 4. Message Consumer (Consommateur)

```pascal
unit MessageQueue.Consumer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  MessageQueue.Types, MessageQueue.Queue;

type
  // Callback pour traiter les messages
  TMessageHandler = procedure(AMessage: TMessage) of object;

  TMessageConsumer = class(TThread)
  private
    FQueue: TMessageQueue;
    FConsumerName: string;
    FHandler: TMessageHandler;
    FRunning: Boolean;
  protected
    procedure Execute; override;
  public
    constructor Create(const AName: string; AQueue: TMessageQueue;
                      AHandler: TMessageHandler);

    procedure Stop;

    property ConsumerName: string read FConsumerName;
    property Running: Boolean read FRunning;
  end;

implementation

constructor TMessageConsumer.Create(const AName: string;
  AQueue: TMessageQueue; AHandler: TMessageHandler);
begin
  inherited Create(False);
  FConsumerName := AName;
  FQueue := AQueue;
  FHandler := AHandler;
  FRunning := True;
  FreeOnTerminate := False;
end;

procedure TMessageConsumer.Execute;  
var
  Message: TMessage;
begin
  WriteLn(Format('[Consumer %s] Démarré', [FConsumerName]));

  while FRunning and not Terminated do
  begin
    // Essayer de récupérer un message (timeout 1 seconde)
    if FQueue.TryDequeue(Message, 1000) then
    begin
      try
        WriteLn(Format('[Consumer %s] Traitement: %s',
          [FConsumerName, Message.MessageType]));

        // Appeler le handler
        if Assigned(FHandler) then
          FHandler(Message);

      except
        on E: Exception do
          WriteLn(Format('[Consumer %s] ERREUR: %s', [FConsumerName, E.Message]));
      end;

      Message.Free;
    end;
  end;

  WriteLn(Format('[Consumer %s] Arrêté', [FConsumerName]));
end;

procedure TMessageConsumer.Stop;  
begin
  FRunning := False;
  WriteLn(Format('[Consumer %s] Arrêt demandé', [FConsumerName]));
end;

end.
```

### 5. Démonstration Message Queue

```pascal
program MessageQueueDemo;

{$mode objfpc}{$H+}

uses
  SysUtils,
  MessageQueue.Types, MessageQueue.Queue,
  MessageQueue.Producer, MessageQueue.Consumer;

type
  TMessageProcessor = class
  public
    procedure ProcessMessage(AMessage: TMessage);
  end;

procedure TMessageProcessor.ProcessMessage(AMessage: TMessage);  
var
  Text: string;
begin
  Text := AMessage.Payload.Get('text', '');
  WriteLn(Format('  → Traitement: "%s"', [Text]));

  // Simuler un traitement
  Sleep(Random(500) + 100);

  WriteLn('  ✓ Terminé');
end;

var
  Queue: TMessageQueue;
  Producer1, Producer2: TMessageProducer;
  Consumer1, Consumer2: TMessageConsumer;
  Processor: TMessageProcessor;
  i: Integer;

begin
  Randomize;

  WriteLn('═══════════════════════════════════════════');
  WriteLn('  Démonstration Message Queue             ');
  WriteLn('═══════════════════════════════════════════');
  WriteLn;

  // Créer la queue
  Queue := TMessageQueue.Create(100);
  Processor := TMessageProcessor.Create;
  try
    // Créer les producteurs
    Producer1 := TMessageProducer.Create('Producer-1', Queue);
    Producer2 := TMessageProducer.Create('Producer-2', Queue);

    // Créer les consommateurs
    Consumer1 := TMessageConsumer.Create('Consumer-1', Queue, @Processor.ProcessMessage);
    Consumer2 := TMessageConsumer.Create('Consumer-2', Queue, @Processor.ProcessMessage);

    WriteLn('=== Système démarré ===');
    WriteLn;
    Sleep(500);

    // Envoyer des messages
    WriteLn('=== Envoi de messages ===');
    for i := 1 to 10 do
    begin
      if i mod 2 = 0 then
        Producer1.SendSimpleMessage('task', Format('Tâche %d du producteur 1', [i]))
      else
        Producer2.SendSimpleMessage('task', Format('Tâche %d du producteur 2', [i]));

      Sleep(100);
    end;

    WriteLn;
    WriteLn('=== Attente traitement ===');

    // Attendre que tous les messages soient traités
    while not Queue.IsEmpty do
      Sleep(100);

    Sleep(1000);

    // Arrêter les consommateurs
    WriteLn;
    WriteLn('=== Arrêt des consommateurs ===');
    Consumer1.Stop;
    Consumer2.Stop;

    Consumer1.WaitFor;
    Consumer2.WaitFor;

    Consumer1.Free;
    Consumer2.Free;
    Producer1.Free;
    Producer2.Free;

  finally
    Processor.Free;
    Queue.Free;
  end;

  WriteLn;
  WriteLn('═══════════════════════════════════════════');
  WriteLn('  Démonstration terminée                   ');
  WriteLn('═══════════════════════════════════════════');

  WriteLn;
  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
```

## Event Bus

### 1. Structure d'un événement

```pascal
unit EventBus.Events;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson;

type
  // Événement de base
  TEvent = class
  private
    FId: string;
    FEventType: string;
    FSource: string;
    FData: TJSONObject;
    FTimestamp: TDateTime;
  public
    constructor Create(const AEventType, ASource: string; AData: TJSONObject);
    destructor Destroy; override;

    property Id: string read FId;
    property EventType: string read FEventType;
    property Source: string read FSource;
    property Data: TJSONObject read FData;
    property Timestamp: TDateTime read FTimestamp;
  end;

  // Événements spécifiques
  TUserCreatedEvent = class(TEvent)
  public
    constructor Create(const ASource, AUserId, AUsername: string);

    function GetUserId: string;
    function GetUsername: string;
  end;

  TOrderPlacedEvent = class(TEvent)
  public
    constructor Create(const ASource, AOrderId: string; AAmount: Currency);

    function GetOrderId: string;
    function GetAmount: Currency;
  end;

implementation

// TEvent

constructor TEvent.Create(const AEventType, ASource: string; AData: TJSONObject);  
var
  G: TGUID;
begin
  inherited Create;
  CreateGUID(G);
  FId := GUIDToString(G);
  FEventType := AEventType;
  FSource := ASource;
  FData := AData;
  FTimestamp := Now;
end;

destructor TEvent.Destroy;  
begin
  FData.Free;
  inherited;
end;

// TUserCreatedEvent

constructor TUserCreatedEvent.Create(const ASource, AUserId, AUsername: string);  
var
  Data: TJSONObject;
begin
  Data := TJSONObject.Create;
  Data.Add('user_id', AUserId);
  Data.Add('username', AUsername);

  inherited Create('user.created', ASource, Data);
end;

function TUserCreatedEvent.GetUserId: string;  
begin
  Result := Data.Get('user_id', '');
end;

function TUserCreatedEvent.GetUsername: string;  
begin
  Result := Data.Get('username', '');
end;

// TOrderPlacedEvent

constructor TOrderPlacedEvent.Create(const ASource, AOrderId: string;
  AAmount: Currency);
var
  Data: TJSONObject;
begin
  Data := TJSONObject.Create;
  Data.Add('order_id', AOrderId);
  Data.Add('amount', AAmount);

  inherited Create('order.placed', ASource, Data);
end;

function TOrderPlacedEvent.GetOrderId: string;  
begin
  Result := Data.Get('order_id', '');
end;

function TOrderPlacedEvent.GetAmount: Currency;  
begin
  Result := Data.Get('amount', 0.0);
end;

end.
```

### 2. Event Bus central

```pascal
unit EventBus.Bus;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, SyncObjs,
  EventBus.Events;

type
  // Callback pour les événements
  TEventHandler = procedure(AEvent: TEvent) of object;

  // Souscription
  TSubscription = class
  private
    FSubscriberId: string;
    FEventType: string;
    FHandler: TEventHandler;
  public
    constructor Create(const ASubscriberId, AEventType: string;
                      AHandler: TEventHandler);

    property SubscriberId: string read FSubscriberId;
    property EventType: string read FEventType;
    property Handler: TEventHandler read FHandler;
  end;

  TEventBus = class;  // Forward declaration

  // Thread de dispatch pour l'Event Bus
  TDispatchThread = class(TThread)
  private
    FBus: TEventBus;
  protected
    procedure Execute; override;
  public
    constructor Create(ABus: TEventBus);
  end;

  // Event Bus
  TEventBus = class
  private
    FSubscriptions: TObjectList<TSubscription>;
    FEventQueue: TThreadedQueue<TEvent>;
    FDispatchThread: TThread;
    FRunning: Boolean;
    FLock: TCriticalSection;

    procedure DispatchEvents;
  public
    constructor Create;
    destructor Destroy; override;

    // Souscription
    procedure Subscribe(const ASubscriberId, AEventType: string;
                       AHandler: TEventHandler);
    procedure Unsubscribe(const ASubscriberId, AEventType: string);
    procedure UnsubscribeAll(const ASubscriberId: string);

    // Publication
    procedure Publish(AEvent: TEvent);
    procedure PublishSync(AEvent: TEvent);

    // Contrôle
    procedure Start;
    procedure Stop;
  end;

implementation

// TDispatchThread

constructor TDispatchThread.Create(ABus: TEventBus);  
begin
  inherited Create(True);  // Créé suspendu
  FBus := ABus;
  FreeOnTerminate := False;
end;

procedure TDispatchThread.Execute;  
begin
  FBus.DispatchEvents;
end;

// TSubscription

constructor TSubscription.Create(const ASubscriberId, AEventType: string;
  AHandler: TEventHandler);
begin
  inherited Create;
  FSubscriberId := ASubscriberId;
  FEventType := AEventType;
  FHandler := AHandler;
end;

// TEventBus

constructor TEventBus.Create;  
begin
  inherited Create;
  FSubscriptions := TObjectList<TSubscription>.Create(True);
  FEventQueue := TThreadedQueue<TEvent>.Create(1000, 10000, 100);
  FLock := TCriticalSection.Create;
  FRunning := False;
end;

destructor TEventBus.Destroy;  
var
  Event: TEvent;
begin
  Stop;

  // Vider la queue
  while FEventQueue.PopItem(Event) = wrSignaled do
    Event.Free;

  FEventQueue.Free;
  FLock.Free;
  FSubscriptions.Free;
  inherited;
end;

procedure TEventBus.Subscribe(const ASubscriberId, AEventType: string;
  AHandler: TEventHandler);
var
  Subscription: TSubscription;
begin
  FLock.Enter;
  try
    Subscription := TSubscription.Create(ASubscriberId, AEventType, AHandler);
    FSubscriptions.Add(Subscription);

    WriteLn(Format('[EventBus] %s souscrit à: %s', [ASubscriberId, AEventType]));
  finally
    FLock.Leave;
  end;
end;

procedure TEventBus.Unsubscribe(const ASubscriberId, AEventType: string);  
var
  i: Integer;
begin
  FLock.Enter;
  try
    for i := FSubscriptions.Count - 1 downto 0 do
    begin
      if (FSubscriptions[i].SubscriberId = ASubscriberId) and
         (FSubscriptions[i].EventType = AEventType) then
      begin
        FSubscriptions.Delete(i);
        WriteLn(Format('[EventBus] %s désinscrit de: %s',
          [ASubscriberId, AEventType]));
      end;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TEventBus.UnsubscribeAll(const ASubscriberId: string);  
var
  i: Integer;
begin
  FLock.Enter;
  try
    for i := FSubscriptions.Count - 1 downto 0 do
    begin
      if FSubscriptions[i].SubscriberId = ASubscriberId then
        FSubscriptions.Delete(i);
    end;

    WriteLn(Format('[EventBus] %s complètement désinscrit', [ASubscriberId]));
  finally
    FLock.Leave;
  end;
end;

procedure TEventBus.Publish(AEvent: TEvent);  
begin
  FEventQueue.PushItem(AEvent);

  WriteLn(Format('[EventBus] Événement publié: %s (de %s)',
    [AEvent.EventType, AEvent.Source]));
end;

procedure TEventBus.PublishSync(AEvent: TEvent);  
var
  Subscription: TSubscription;
begin
  WriteLn(Format('[EventBus] Événement publié (sync): %s', [AEvent.EventType]));

  FLock.Enter;
  try
    for Subscription in FSubscriptions do
    begin
      if (Subscription.EventType = AEvent.EventType) or
         (Subscription.EventType = '*') then
      begin
        try
          WriteLn(Format('[EventBus] → Notification: %s',
            [Subscription.SubscriberId]));
          Subscription.Handler(AEvent);
        except
          on E: Exception do
            WriteLn(Format('[EventBus] ERREUR handler %s: %s',
              [Subscription.SubscriberId, E.Message]));
        end;
      end;
    end;
  finally
    FLock.Leave;
  end;

  AEvent.Free;
end;

procedure TEventBus.DispatchEvents;  
var
  Event: TEvent;
begin
  while FRunning do
  begin
    if FEventQueue.PopItem(Event) = wrSignaled then
    begin
      try
        PublishSync(Event);
      except
        on E: Exception do
        begin
          WriteLn('[EventBus] ERREUR dispatch: ', E.Message);
          Event.Free;
        end;
      end;
    end;
  end;
end;

procedure TEventBus.Start;  
begin
  if FRunning then Exit;

  FRunning := True;

  FDispatchThread := TDispatchThread.Create(Self);
  FDispatchThread.Start;

  WriteLn('[EventBus] Démarré');
end;

procedure TEventBus.Stop;  
begin
  if not FRunning then Exit;

  FRunning := False;

  if Assigned(FDispatchThread) then
  begin
    FDispatchThread.WaitFor;
    FDispatchThread.Free;
    FDispatchThread := nil;
  end;

  WriteLn('[EventBus] Arrêté');
end;

end.
```

### 3. Subscribers (Abonnés)

```pascal
unit EventBus.Subscribers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  EventBus.Events, EventBus.Bus;

type
  // Abonné générique
  TEventSubscriber = class
  private
    FSubscriberId: string;
    FEventBus: TEventBus;
  protected
    procedure HandleEvent(AEvent: TEvent); virtual; abstract;
  public
    constructor Create(const AId: string; AEventBus: TEventBus);
    destructor Destroy; override;

    procedure SubscribeTo(const AEventType: string);

    property SubscriberId: string read FSubscriberId;
  end;

  // Abonné spécifique : Email
  TEmailNotifier = class(TEventSubscriber)
  protected
    procedure HandleEvent(AEvent: TEvent); override;
  public
    constructor Create(AEventBus: TEventBus);
  end;

  // Abonné spécifique : Logger
  TEventLogger = class(TEventSubscriber)
  protected
    procedure HandleEvent(AEvent: TEvent); override;
  public
    constructor Create(AEventBus: TEventBus);
  end;

  // Abonné spécifique : Analytics
  TAnalyticsTracker = class(TEventSubscriber)
  protected
    procedure HandleEvent(AEvent: TEvent); override;
  public
    constructor Create(AEventBus: TEventBus);
  end;

implementation

// TEventSubscriber

constructor TEventSubscriber.Create(const AId: string; AEventBus: TEventBus);  
begin
  inherited Create;
  FSubscriberId := AId;
  FEventBus := AEventBus;
end;

destructor TEventSubscriber.Destroy;  
begin
  if Assigned(FEventBus) then
    FEventBus.UnsubscribeAll(FSubscriberId);
  inherited;
end;

procedure TEventSubscriber.SubscribeTo(const AEventType: string);  
begin
  FEventBus.Subscribe(FSubscriberId, AEventType, @HandleEvent);
end;

// TEmailNotifier

constructor TEmailNotifier.Create(AEventBus: TEventBus);  
begin
  inherited Create('EmailNotifier', AEventBus);

  // S'abonner aux événements intéressants
  SubscribeTo('user.created');
  SubscribeTo('order.placed');
end;

procedure TEmailNotifier.HandleEvent(AEvent: TEvent);  
begin
  WriteLn('  [EmailNotifier] Envoi email pour: ', AEvent.EventType);

  if AEvent is TUserCreatedEvent then
  begin
    WriteLn('    → Email de bienvenue à: ',
      TUserCreatedEvent(AEvent).GetUsername);
  end
  else if AEvent is TOrderPlacedEvent then
  begin
    WriteLn('    → Email confirmation commande: ',
      TOrderPlacedEvent(AEvent).GetOrderId);
  end;
end;

// TEventLogger

constructor TEventLogger.Create(AEventBus: TEventBus);  
begin
  inherited Create('EventLogger', AEventBus);

  // S'abonner à TOUS les événements
  SubscribeTo('*');
end;

procedure TEventLogger.HandleEvent(AEvent: TEvent);  
begin
  WriteLn(Format('  [Logger] %s | Type: %s | Source: %s',
    [FormatDateTime('hh:nn:ss', AEvent.Timestamp),
     AEvent.EventType,
     AEvent.Source]));
end;

// TAnalyticsTracker

constructor TAnalyticsTracker.Create(AEventBus: TEventBus);  
begin
  inherited Create('AnalyticsTracker', AEventBus);

  // S'abonner aux événements métier
  SubscribeTo('user.created');
  SubscribeTo('order.placed');
end;

procedure TAnalyticsTracker.HandleEvent(AEvent: TEvent);  
begin
  WriteLn('  [Analytics] Enregistrement métrique: ', AEvent.EventType);

  if AEvent is TOrderPlacedEvent then
  begin
    WriteLn(Format('    → Montant: %.2f €',
      [TOrderPlacedEvent(AEvent).GetAmount]));
  end;
end;

end.
```

### Démonstration Event Bus complète

```pascal
program EventBusDemo;

{$mode objfpc}{$H+}

uses
  SysUtils,
  EventBus.Events, EventBus.Bus, EventBus.Subscribers;

var
  Bus: TEventBus;
  EmailNotifier: TEmailNotifier;
  Logger: TEventLogger;
  Analytics: TAnalyticsTracker;

begin
  WriteLn('═══════════════════════════════════════════');
  WriteLn('  Démonstration Event Bus                 ');
  WriteLn('═══════════════════════════════════════════');
  WriteLn;

  // Créer l'Event Bus
  Bus := TEventBus.Create;
  try
    // Créer les abonnés
    EmailNotifier := TEmailNotifier.Create(Bus);
    Logger := TEventLogger.Create(Bus);
    Analytics := TAnalyticsTracker.Create(Bus);

    // Démarrer l'Event Bus
    Bus.Start;

    WriteLn('=== Système démarré ===');
    WriteLn;
    Sleep(500);

    // Publier des événements
    WriteLn('=== Publication d''événements ===');
    WriteLn;

    WriteLn('1. Création utilisateur');
    Bus.Publish(TUserCreatedEvent.Create('UserService', 'user-123', 'alice@example.com'));
    Sleep(500);
    WriteLn;

    WriteLn('2. Création commande');
    Bus.Publish(TOrderPlacedEvent.Create('OrderService', 'order-456', 99.99));
    Sleep(500);
    WriteLn;

    WriteLn('3. Autre création utilisateur');
    Bus.Publish(TUserCreatedEvent.Create('UserService', 'user-789', 'bob@example.com'));
    Sleep(500);
    WriteLn;

    WriteLn('=== Attente traitement ===');
    Sleep(2000);

    // Arrêter l'Event Bus
    WriteLn;
    WriteLn('=== Arrêt du système ===');
    Bus.Stop;

    // Nettoyer
    Analytics.Free;
    Logger.Free;
    EmailNotifier.Free;

  finally
    Bus.Free;
  end;

  WriteLn;
  WriteLn('═══════════════════════════════════════════');
  WriteLn('  Démonstration terminée                   ');
  WriteLn('═══════════════════════════════════════════');

  WriteLn;
  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
```

## Patterns avancés

### 1. Dead Letter Queue (File des messages morts)

Pour gérer les messages qui échouent après plusieurs tentatives.

```pascal
unit MessageQueue.DeadLetter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  MessageQueue.Types, MessageQueue.Queue;

type
  TDeadLetterQueue = class
  private
    FQueue: TMessageQueue;
    FMaxRetries: Integer;
  public
    constructor Create(AMaxRetries: Integer = 3);
    destructor Destroy; override;

    function ShouldRetry(AMessage: TMessage): Boolean;
    procedure SendToDeadLetter(AMessage: TMessage; const AReason: string);

    property DeadLetters: TMessageQueue read FQueue;
  end;

implementation

constructor TDeadLetterQueue.Create(AMaxRetries: Integer);  
begin
  inherited Create;
  FQueue := TMessageQueue.Create(1000);
  FMaxRetries := AMaxRetries;
end;

destructor TDeadLetterQueue.Destroy;  
begin
  FQueue.Free;
  inherited;
end;

function TDeadLetterQueue.ShouldRetry(AMessage: TMessage): Boolean;  
begin
  Result := AMessage.RetryCount < FMaxRetries;

  if Result then
  begin
    Inc(AMessage.FRetryCount);
    WriteLn(Format('[DeadLetter] Retry %d/%d pour message: %s',
      [AMessage.RetryCount, FMaxRetries, AMessage.MessageType]));
  end
  else
  begin
    WriteLn(Format('[DeadLetter] Maximum retries atteint pour: %s',
      [AMessage.MessageType]));
  end;
end;

procedure TDeadLetterQueue.SendToDeadLetter(AMessage: TMessage; const AReason: string);  
var
  DeadMessage: TMessage;
  Data: TJSONObject;
begin
  WriteLn(Format('[DeadLetter] Message envoyé: %s (Raison: %s)',
    [AMessage.MessageType, AReason]));

  // Créer une copie avec les informations d'échec
  Data := TJSONObject.Create;
  Data.Add('original_type', AMessage.MessageType);
  Data.Add('reason', AReason);
  Data.Add('retry_count', AMessage.RetryCount);
  Data.Add('original_timestamp', FormatDateTime('yyyy-mm-dd hh:nn:ss', AMessage.Timestamp));

  DeadMessage := TMessage.Create('dead_letter', Data, mpCritical);
  FQueue.Enqueue(DeadMessage);
end;

end.
```

### 2. Message Priority Queue

File avec priorités pour traiter les messages urgents en premier.

```pascal
unit MessageQueue.Priority;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, SyncObjs,
  MessageQueue.Types;

type
  TPriorityMessageQueue = class
  private
    FQueues: array[TMessagePriority] of TThreadList;
    FSemaphore: TSemaphore;
    FLock: TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Enqueue(AMessage: TMessage);
    function Dequeue: TMessage;
    function Count: Integer;
  end;

implementation

constructor TPriorityMessageQueue.Create;  
var
  Priority: TMessagePriority;
begin
  inherited Create;

  for Priority := Low(TMessagePriority) to High(TMessagePriority) do
    FQueues[Priority] := TThreadList.Create;

  FSemaphore := TSemaphore.Create(nil, 0, 10000, '');
  FLock := TCriticalSection.Create;
end;

destructor TPriorityMessageQueue.Destroy;  
var
  Priority: TMessagePriority;
  List: TList;
  i: Integer;
begin
  // Libérer tous les messages
  for Priority := Low(TMessagePriority) to High(TMessagePriority) do
  begin
    List := FQueues[Priority].LockList;
    try
      for i := 0 to List.Count - 1 do
        TMessage(List[i]).Free;
      List.Clear;
    finally
      FQueues[Priority].UnlockList;
    end;

    FQueues[Priority].Free;
  end;

  FSemaphore.Free;
  FLock.Free;
  inherited;
end;

procedure TPriorityMessageQueue.Enqueue(AMessage: TMessage);  
var
  List: TList;
begin
  FLock.Enter;
  try
    List := FQueues[AMessage.Priority].LockList;
    try
      List.Add(AMessage);
      WriteLn(Format('[PriorityQueue] Message ajouté (Priorité: %d): %s',
        [Ord(AMessage.Priority), AMessage.MessageType]));
    finally
      FQueues[AMessage.Priority].UnlockList;
    end;

    FSemaphore.Release;
  finally
    FLock.Leave;
  end;
end;

function TPriorityMessageQueue.Dequeue: TMessage;  
var
  Priority: TMessagePriority;
  List: TList;
begin
  Result := nil;

  // Attendre un message
  FSemaphore.WaitFor(INFINITE);

  FLock.Enter;
  try
    // Chercher le message de plus haute priorité
    for Priority := High(TMessagePriority) downto Low(TMessagePriority) do
    begin
      List := FQueues[Priority].LockList;
      try
        if List.Count > 0 then
        begin
          Result := TMessage(List[0]);
          List.Delete(0);

          WriteLn(Format('[PriorityQueue] Message récupéré (Priorité: %d): %s',
            [Ord(Priority), Result.MessageType]));
          Break;
        end;
      finally
        FQueues[Priority].UnlockList;
      end;
    end;
  finally
    FLock.Leave;
  end;
end;

function TPriorityMessageQueue.Count: Integer;  
var
  Priority: TMessagePriority;
  List: TList;
begin
  Result := 0;

  FLock.Enter;
  try
    for Priority := Low(TMessagePriority) to High(TMessagePriority) do
    begin
      List := FQueues[Priority].LockList;
      try
        Inc(Result, List.Count);
      finally
        FQueues[Priority].UnlockList;
      end;
    end;
  finally
    FLock.Leave;
  end;
end;

end.
```

### 3. Request-Reply Pattern

Communication bidirectionnelle avec réponse.

```pascal
unit MessageQueue.RequestReply;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs, Generics.Collections, fpjson,
  MessageQueue.Types, MessageQueue.Queue;

type
  // Message avec corrélation
  TRequestMessage = class(TMessage)
  private
    FCorrelationId: string;
    FReplyTo: string;
  public
    constructor Create(const AType: string; APayload: TJSONObject;
                      const AReplyTo: string);

    property CorrelationId: string read FCorrelationId;
    property ReplyTo: string read FReplyTo;
  end;

  TReplyMessage = class(TMessage)
  private
    FCorrelationId: string;
  public
    constructor Create(const AType: string; APayload: TJSONObject;
                      const ACorrelationId: string);

    property CorrelationId: string read FCorrelationId;
  end;

  // Gestionnaire Request-Reply
  TRequestReplyManager = class
  private
    FRequestQueue: TMessageQueue;
    FReplyQueues: TDictionary<string, TMessageQueue>;
    FLock: TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;

    function SendRequest(const AType: string; APayload: TJSONObject;
                        ATimeoutMs: Cardinal = 5000): TReplyMessage;
    procedure SendReply(const ACorrelationId: string; APayload: TJSONObject);

    function GetNextRequest: TRequestMessage;
  end;

implementation

// TRequestMessage

constructor TRequestMessage.Create(const AType: string; APayload: TJSONObject;
  const AReplyTo: string);
var
  G: TGUID;
begin
  inherited Create(AType, APayload);
  CreateGUID(G);
  FCorrelationId := GUIDToString(G);
  FReplyTo := AReplyTo;
end;

// TReplyMessage

constructor TReplyMessage.Create(const AType: string; APayload: TJSONObject;
  const ACorrelationId: string);
begin
  inherited Create(AType, APayload);
  FCorrelationId := ACorrelationId;
end;

// TRequestReplyManager

constructor TRequestReplyManager.Create;  
begin
  inherited Create;
  FRequestQueue := TMessageQueue.Create;
  FReplyQueues := TDictionary<string, TMessageQueue>.Create;
  FLock := TCriticalSection.Create;
end;

destructor TRequestReplyManager.Destroy;  
var
  Queue: TMessageQueue;
begin
  FLock.Enter;
  try
    for Queue in FReplyQueues.Values do
      Queue.Free;
    FReplyQueues.Free;
  finally
    FLock.Leave;
  end;

  FRequestQueue.Free;
  FLock.Free;
  inherited;
end;

function TRequestReplyManager.SendRequest(const AType: string;
  APayload: TJSONObject; ATimeoutMs: Cardinal): TReplyMessage;
var
  Request: TRequestMessage;
  ReplyQueue: TMessageQueue;
  ReplyMsg: TMessage;
begin
  Result := nil;

  // Créer une queue temporaire pour la réponse
  ReplyQueue := TMessageQueue.Create(10);

  FLock.Enter;
  try
    Request := TRequestMessage.Create(AType, APayload, 'temp-reply');
    FReplyQueues.Add(Request.CorrelationId, ReplyQueue);
  finally
    FLock.Leave;
  end;

  try
    // Envoyer la requête
    FRequestQueue.Enqueue(Request);
    WriteLn(Format('[RequestReply] Requête envoyée: %s (Correlation: %s)',
      [AType, Request.CorrelationId]));

    // Attendre la réponse
    if ReplyQueue.TryDequeue(ReplyMsg, ATimeoutMs) then
    begin
      Result := ReplyMsg as TReplyMessage;
      WriteLn(Format('[RequestReply] Réponse reçue: %s', [Result.MessageType]));
    end
    else
      WriteLn('[RequestReply] Timeout - Aucune réponse');

  finally
    FLock.Enter;
    try
      FReplyQueues.Remove(Request.CorrelationId);
    finally
      FLock.Leave;
    end;

    ReplyQueue.Free;
  end;
end;

procedure TRequestReplyManager.SendReply(const ACorrelationId: string;
  APayload: TJSONObject);
var
  Reply: TReplyMessage;
  ReplyQueue: TMessageQueue;
begin
  FLock.Enter;
  try
    if not FReplyQueues.TryGetValue(ACorrelationId, ReplyQueue) then
    begin
      WriteLn('[RequestReply] ERREUR: Queue de réponse introuvable');
      Exit;
    end;
  finally
    FLock.Leave;
  end;

  Reply := TReplyMessage.Create('reply', APayload, ACorrelationId);
  ReplyQueue.Enqueue(Reply);

  WriteLn(Format('[RequestReply] Réponse envoyée pour: %s', [ACorrelationId]));
end;

function TRequestReplyManager.GetNextRequest: TRequestMessage;  
var
  Msg: TMessage;
begin
  Msg := FRequestQueue.Dequeue;
  Result := Msg as TRequestMessage;
end;

end.
```

### 4. Publish-Subscribe Pattern

Extension de l'Event Bus avec topics.

```pascal
unit EventBus.PubSub;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections,
  EventBus.Events, EventBus.Bus;

type
  // Publisher
  TPublisher = class
  private
    FPublisherId: string;
    FEventBus: TEventBus;
  public
    constructor Create(const AId: string; AEventBus: TEventBus);

    procedure Publish(const ATopic: string; AData: TJSONObject);

    property PublisherId: string read FPublisherId;
  end;

  // Subscriber avec filtre
  TTopicSubscriber = class
  private
    FSubscriberId: string;
    FEventBus: TEventBus;
    FTopics: TStringList;

    procedure HandleEvent(AEvent: TEvent);
  protected
    procedure ProcessEvent(const ATopic: string; AData: TJSONObject); virtual; abstract;
  public
    constructor Create(const AId: string; AEventBus: TEventBus);
    destructor Destroy; override;

    procedure SubscribeTo(const ATopic: string);
    procedure UnsubscribeFrom(const ATopic: string);

    property SubscriberId: string read FSubscriberId;
  end;

implementation

// TPublisher

constructor TPublisher.Create(const AId: string; AEventBus: TEventBus);  
begin
  inherited Create;
  FPublisherId := AId;
  FEventBus := AEventBus;
end;

procedure TPublisher.Publish(const ATopic: string; AData: TJSONObject);  
var
  Event: TEvent;
begin
  WriteLn(Format('[Publisher %s] Publication sur topic: %s',
    [FPublisherId, ATopic]));

  Event := TEvent.Create(ATopic, FPublisherId, AData);
  FEventBus.Publish(Event);
end;

// TTopicSubscriber

constructor TTopicSubscriber.Create(const AId: string; AEventBus: TEventBus);  
begin
  inherited Create;
  FSubscriberId := AId;
  FEventBus := AEventBus;
  FTopics := TStringList.Create;
  FTopics.Sorted := True;
  FTopics.Duplicates := dupIgnore;
end;

destructor TTopicSubscriber.Destroy;  
begin
  // Se désabonner de tout
  FEventBus.UnsubscribeAll(FSubscriberId);
  FTopics.Free;
  inherited;
end;

procedure TTopicSubscriber.SubscribeTo(const ATopic: string);  
begin
  FTopics.Add(ATopic);
  FEventBus.Subscribe(FSubscriberId, ATopic, @HandleEvent);

  WriteLn(Format('[Subscriber %s] Abonné au topic: %s',
    [FSubscriberId, ATopic]));
end;

procedure TTopicSubscriber.UnsubscribeFrom(const ATopic: string);  
begin
  FTopics.Delete(FTopics.IndexOf(ATopic));
  FEventBus.Unsubscribe(FSubscriberId, ATopic);
end;

procedure TTopicSubscriber.HandleEvent(AEvent: TEvent);  
begin
  if FTopics.IndexOf(AEvent.EventType) >= 0 then
  begin
    WriteLn(Format('[Subscriber %s] Traitement: %s',
      [FSubscriberId, AEvent.EventType]));
    ProcessEvent(AEvent.EventType, AEvent.Data);
  end;
end;

end.
```

## Intégration avec des systèmes externes

### 1. RabbitMQ (Message Broker externe)

```pascal
unit MessageQueue.RabbitMQ;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient, fpjson, jsonparser;

type
  TRabbitMQClient = class
  private
    FHost: string;
    FPort: Integer;
    FUsername: string;
    FPassword: string;
    FVirtualHost: string;
    FHTTPClient: TFPHTTPClient;

    function GetBaseURL: string;
    function BasicAuth: string;
  public
    constructor Create(const AHost: string; APort: Integer;
                      const AUsername, APassword: string);
    destructor Destroy; override;

    procedure DeclareQueue(const AQueueName: string);
    procedure PublishMessage(const AQueueName, AMessage: string);
    function ConsumeMessage(const AQueueName: string): string;
  end;

implementation

uses
  base64;

constructor TRabbitMQClient.Create(const AHost: string; APort: Integer;
  const AUsername, APassword: string);
begin
  inherited Create;
  FHost := AHost;
  FPort := APort;
  FUsername := AUsername;
  FPassword := APassword;
  FVirtualHost := '/';

  FHTTPClient := TFPHTTPClient.Create(nil);
end;

destructor TRabbitMQClient.Destroy;  
begin
  FHTTPClient.Free;
  inherited;
end;

function TRabbitMQClient.GetBaseURL: string;  
begin
  Result := Format('http://%s:%d/api', [FHost, FPort]);
end;

function TRabbitMQClient.BasicAuth: string;  
begin
  Result := 'Basic ' + EncodeStringBase64(FUsername + ':' + FPassword);
end;

procedure TRabbitMQClient.DeclareQueue(const AQueueName: string);  
var
  URL: string;
  Body: TJSONObject;
begin
  URL := Format('%s/queues/%s/%s', [GetBaseURL, FVirtualHost, AQueueName]);

  Body := TJSONObject.Create;
  try
    Body.Add('durable', True);
    Body.Add('auto_delete', False);

    FHTTPClient.AddHeader('Authorization', BasicAuth);
    FHTTPClient.AddHeader('Content-Type', 'application/json');
    FHTTPClient.RequestBody := TStringStream.Create(Body.AsJSON);

    FHTTPClient.Put(URL);

    WriteLn(Format('[RabbitMQ] Queue déclarée: %s', [AQueueName]));
  finally
    Body.Free;
  end;
end;

procedure TRabbitMQClient.PublishMessage(const AQueueName, AMessage: string);  
var
  URL: string;
  Body: TJSONObject;
begin
  URL := Format('%s/exchanges/%s/amq.default/publish',
    [GetBaseURL, FVirtualHost]);

  Body := TJSONObject.Create;
  try
    Body.Add('routing_key', AQueueName);
    Body.Add('payload', AMessage);
    Body.Add('payload_encoding', 'string');

    FHTTPClient.AddHeader('Authorization', BasicAuth);
    FHTTPClient.RequestBody := TStringStream.Create(Body.AsJSON);

    FHTTPClient.Post(URL);

    WriteLn(Format('[RabbitMQ] Message publié sur: %s', [AQueueName]));
  finally
    Body.Free;
  end;
end;

function TRabbitMQClient.ConsumeMessage(const AQueueName: string): string;  
var
  URL: string;
  Response: string;
  Parser: TJSONParser;
  JSON: TJSONArray;
begin
  Result := '';

  URL := Format('%s/queues/%s/%s/get', [GetBaseURL, FVirtualHost, AQueueName]);

  FHTTPClient.AddHeader('Authorization', BasicAuth);

  Response := FHTTPClient.Post(URL, '{"count":1,"ackmode":"ack_requeue_false"}');

  Parser := TJSONParser.Create(Response, [joUTF8]);
  try
    JSON := Parser.Parse as TJSONArray;
    try
      if JSON.Count > 0 then
      begin
        Result := (JSON.Objects[0] as TJSONObject).Get('payload', '');
        WriteLn(Format('[RabbitMQ] Message consommé de: %s', [AQueueName]));
      end;
    finally
      JSON.Free;
    end;
  finally
    Parser.Free;
  end;
end;

end.
```

### 2. Redis Pub/Sub

```pascal
unit EventBus.Redis;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Sockets;

type
  TRedisClient = class
  private
    FHost: string;
    FPort: Integer;
    FSocket: TInetSocket;
    FConnected: Boolean;

    function SendCommand(const ACommand: string): string;
  public
    constructor Create(const AHost: string; APort: Integer = 6379);
    destructor Destroy; override;

    procedure Connect;
    procedure Disconnect;

    procedure Publish(const AChannel, AMessage: string);
    procedure Subscribe(const AChannel: string);
    function GetMessage: string;
  end;

implementation

constructor TRedisClient.Create(const AHost: string; APort: Integer);  
begin
  inherited Create;
  FHost := AHost;
  FPort := APort;
  FConnected := False;
end;

destructor TRedisClient.Destroy;  
begin
  if FConnected then
    Disconnect;
  inherited;
end;

procedure TRedisClient.Connect;  
begin
  // Implémentation simplifiée
  // Dans un cas réel, utiliser une bibliothèque Redis
  FConnected := True;
  WriteLn(Format('[Redis] Connecté à %s:%d', [FHost, FPort]));
end;

procedure TRedisClient.Disconnect;  
begin
  FConnected := False;
  WriteLn('[Redis] Déconnecté');
end;

function TRedisClient.SendCommand(const ACommand: string): string;  
begin
  // Implémentation simplifiée du protocole Redis
  Result := 'OK';
end;

procedure TRedisClient.Publish(const AChannel, AMessage: string);  
begin
  if not FConnected then
    raise Exception.Create('Non connecté à Redis');

  SendCommand(Format('PUBLISH %s "%s"', [AChannel, AMessage]));
  WriteLn(Format('[Redis] Message publié sur: %s', [AChannel]));
end;

procedure TRedisClient.Subscribe(const AChannel: string);  
begin
  if not FConnected then
    raise Exception.Create('Non connecté à Redis');

  SendCommand(Format('SUBSCRIBE %s', [AChannel]));
  WriteLn(Format('[Redis] Abonné au canal: %s', [AChannel]));
end;

function TRedisClient.GetMessage: string;  
begin
  // Attendre et récupérer un message
  Result := '';
end;

end.
```

## Bonnes pratiques

### 1. Idempotence des messages

Les messages doivent pouvoir être traités plusieurs fois sans effet de bord.

```pascal
// ✅ BON : Idempotent
procedure ProcessOrder(AOrderId: string);  
begin
  if not OrderExists(AOrderId) then
    CreateOrder(AOrderId)
  else
    UpdateOrder(AOrderId);
end;

// ❌ MAUVAIS : Non-idempotent
procedure ProcessOrder(AOrderId: string);  
begin
  CreateOrder(AOrderId);  // Échoue si déjà existe
end;
```

### 2. Messages immutables

Les messages ne doivent jamais être modifiés après création.

```pascal
// ✅ BON : Immutable
type
  TMessage = class
  private
    FData: TJSONObject;
  public
    constructor Create(AData: TJSONObject);
    property Data: TJSONObject read FData;  // Lecture seule
  end;

// ❌ MAUVAIS : Mutable
type
  TMessage = class
  public
    Data: TJSONObject;  // Public, modifiable
  end;
```

### 3. Gestion des erreurs

```pascal
procedure ProcessMessage(AMessage: TMessage);  
begin
  try
    // Traiter le message
    DoWork(AMessage);

    // Confirmer le traitement
    AcknowledgeMessage(AMessage);
  except
    on E: Exception do
    begin
      WriteLn('ERREUR: ', E.Message);

      // Retry ou Dead Letter
      if DeadLetter.ShouldRetry(AMessage) then
        RequeueMessage(AMessage)
      else
        DeadLetter.SendToDeadLetter(AMessage, E.Message);
    end;
  end;
end;
```

### 4. Monitoring et métriques

```pascal
type
  TQueueMetrics = class
  private
    FMessagesPublished: Int64;
    FMessagesConsumed: Int64;
    FMessagesErrors: Int64;
  public
    procedure RecordPublished;
    procedure RecordConsumed;
    procedure RecordError;

    function GetMetrics: TJSONObject;
  end;

procedure TQueueMetrics.RecordPublished;  
begin
  InterlockedIncrement64(FMessagesPublished);
end;

function TQueueMetrics.GetMetrics: TJSONObject;  
begin
  Result := TJSONObject.Create;
  Result.Add('published', FMessagesPublished);
  Result.Add('consumed', FMessagesConsumed);
  Result.Add('errors', FMessagesErrors);
  Result.Add('pending', FMessagesPublished - FMessagesConsumed);
end;
```

## Comparaison des approches

| Aspect | Message Queue | Event Bus |
|--------|---------------|-----------|
| **Communication** | Point-à-point | Publish-Subscribe |
| **Consommateurs** | Un seul (FIFO) | Plusieurs abonnés |
| **Couplage** | Faible | Très faible |
| **Ordre** | Garanti | Non garanti |
| **Performance** | Haute | Moyenne |
| **Cas d'usage** | Tâches, Jobs | Notifications, Events |

## Quand utiliser quoi ?

### ✅ Utilisez Message Queue quand :

- **Traitement de tâches** : Jobs en arrière-plan
- **Ordre important** : FIFO requis
- **Charge élevée** : Besoin de scalabilité
- **Un seul consommateur** : Pas de duplication
- **Résilience** : Retry et Dead Letter Queue
- **Découplage temporel** : Producteur/consommateur asynchrones

**Exemples :**
- Envoi d'emails en masse
- Traitement d'images
- Import de fichiers
- Calculs longs
- Génération de rapports

### ✅ Utilisez Event Bus quand :

- **Notifications** : Plusieurs composants intéressés
- **Architecture événementielle** : Réaction aux changements
- **Découplage maximal** : Composants indépendants
- **Flexibilité** : Abonnés ajoutés dynamiquement
- **Broadcast** : Même info pour plusieurs destinations
- **Coordination** : Orchestration de services

**Exemples :**
- Notification utilisateurs
- Logging centralisé
- Analytics
- Audit trail
- Synchronisation de caches
- Workflow métier

### ❌ Évitez ces patterns quand :

- **Communication synchrone requise** : Utiliser des appels directs
- **Faible latence critique** : Overhead inacceptable
- **Application simple** : Overhead inutile
- **Pas de découplage nécessaire** : Communication directe suffit
- **Débogage critique** : Flux asynchrone difficile à tracer

## Architecture complète : E-commerce

Exemple d'architecture combinant Message Queue et Event Bus.

```pascal
unit ECommerce.Architecture;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  MessageQueue.Queue, MessageQueue.Types,
  EventBus.Bus, EventBus.Events;

type
  // Architecture E-commerce
  TECommerceSystem = class
  private
    // Message Queues pour les tâches
    FOrderProcessingQueue: TMessageQueue;
    FEmailQueue: TMessageQueue;
    FPaymentQueue: TMessageQueue;

    // Event Bus pour les notifications
    FEventBus: TEventBus;

    // Workers
    FOrderWorker: TThread;
    FEmailWorker: TThread;
    FPaymentWorker: TThread;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Start;
    procedure Stop;

    // Opérations
    procedure PlaceOrder(const AClientId, AProduitId: string; AQuantite: Integer);
    procedure ProcessPayment(const AOrderId: string; AMontant: Currency);

    property EventBus: TEventBus read FEventBus;
  end;

implementation

constructor TECommerceSystem.Create;  
begin
  inherited Create;

  // Créer les queues
  FOrderProcessingQueue := TMessageQueue.Create(1000);
  FEmailQueue := TMessageQueue.Create(1000);
  FPaymentQueue := TMessageQueue.Create(1000);

  // Créer l'Event Bus
  FEventBus := TEventBus.Create;
end;

destructor TECommerceSystem.Destroy;  
begin
  Stop;

  FPaymentQueue.Free;
  FEmailQueue.Free;
  FOrderProcessingQueue.Free;
  FEventBus.Free;

  inherited;
end;

procedure TECommerceSystem.Start;  
begin
  WriteLn('[ECommerce] Démarrage du système...');

  // Démarrer l'Event Bus
  FEventBus.Start;

  // Démarrer les workers (simplifié)
  WriteLn('[ECommerce] Workers démarrés');
  WriteLn('[ECommerce] Système opérationnel');
end;

procedure TECommerceSystem.Stop;  
begin
  WriteLn('[ECommerce] Arrêt du système...');

  // Arrêter l'Event Bus
  FEventBus.Stop;

  WriteLn('[ECommerce] Système arrêté');
end;

procedure TECommerceSystem.PlaceOrder(const AClientId, AProduitId: string;
  AQuantite: Integer);
var
  OrderData: TJSONObject;
  Message: TMessage;
  Event: TOrderPlacedEvent;
  OrderId: string;
  G: TGUID;
begin
  CreateGUID(G);
  OrderId := GUIDToString(G);

  WriteLn(Format('[ECommerce] Nouvelle commande: %s', [OrderId]));

  // 1. Ajouter à la queue de traitement
  OrderData := TJSONObject.Create;
  OrderData.Add('order_id', OrderId);
  OrderData.Add('client_id', AClientId);
  OrderData.Add('produit_id', AProduitId);
  OrderData.Add('quantite', AQuantite);

  Message := TMessage.Create('process_order', OrderData, mpHigh);
  FOrderProcessingQueue.Enqueue(Message);

  // 2. Publier un événement
  Event := TOrderPlacedEvent.Create('OrderService', OrderId, 99.99);
  FEventBus.Publish(Event);

  WriteLn('[ECommerce] Commande enregistrée et événement publié');
end;

procedure TECommerceSystem.ProcessPayment(const AOrderId: string; AMontant: Currency);  
var
  PaymentData: TJSONObject;
  Message: TMessage;
begin
  WriteLn(Format('[ECommerce] Traitement paiement: %.2f €', [AMontant]));

  // Ajouter à la queue de paiement
  PaymentData := TJSONObject.Create;
  PaymentData.Add('order_id', AOrderId);
  PaymentData.Add('amount', AMontant);

  Message := TMessage.Create('process_payment', PaymentData, mpCritical);
  FPaymentQueue.Enqueue(Message);
end;

end.
```

## Performance et scalabilité

### 1. Batch Processing

Traiter plusieurs messages en lot pour améliorer les performances.

```pascal
unit MessageQueue.Batch;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils, Generics.Collections,
  MessageQueue.Types, MessageQueue.Queue;

type
  TBatchProcessor = class
  private
    FQueue: TMessageQueue;
    FBatchSize: Integer;
    FBatchTimeout: Cardinal;

    function CollectBatch: TList<TMessage>;
  public
    constructor Create(AQueue: TMessageQueue; ABatchSize: Integer;
                      ABatchTimeout: Cardinal);

    procedure ProcessBatch(ABatch: TList<TMessage>); virtual; abstract;
    procedure Run;
  end;

implementation

constructor TBatchProcessor.Create(AQueue: TMessageQueue; ABatchSize: Integer;
  ABatchTimeout: Cardinal);
begin
  inherited Create;
  FQueue := AQueue;
  FBatchSize := ABatchSize;
  FBatchTimeout := ABatchTimeout;
end;

function TBatchProcessor.CollectBatch: TList<TMessage>;  
var
  Message: TMessage;
  StartTime: TDateTime;
begin
  Result := TList<TMessage>.Create;
  StartTime := Now;

  while (Result.Count < FBatchSize) and
        (MilliSecondsBetween(Now, StartTime) < FBatchTimeout) do
  begin
    if FQueue.TryDequeue(Message, FBatchTimeout) then
      Result.Add(Message)
    else
      Break;
  end;

  WriteLn(Format('[Batch] %d messages collectés', [Result.Count]));
end;

procedure TBatchProcessor.Run;  
var
  Batch: TList<TMessage>;
begin
  while True do
  begin
    Batch := CollectBatch;
    try
      if Batch.Count > 0 then
        ProcessBatch(Batch);
    finally
      Batch.Free;
    end;
  end;
end;

end.
```

### 2. Partitionnement

Distribuer les messages sur plusieurs queues pour paralléliser le traitement.

```pascal
unit MessageQueue.Partitioned;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections,
  MessageQueue.Types, MessageQueue.Queue;

type
  TPartitionedQueue = class
  private
    FPartitions: TObjectList<TMessageQueue>;
    FPartitionCount: Integer;

    function GetPartitionIndex(AMessage: TMessage): Integer;
  public
    constructor Create(APartitionCount: Integer);
    destructor Destroy; override;

    procedure Enqueue(AMessage: TMessage);
    function DequeueFrom(APartitionIndex: Integer): TMessage;

    property PartitionCount: Integer read FPartitionCount;
  end;

implementation

constructor TPartitionedQueue.Create(APartitionCount: Integer);  
var
  i: Integer;
begin
  inherited Create;
  FPartitionCount := APartitionCount;
  FPartitions := TObjectList<TMessageQueue>.Create(True);

  for i := 0 to APartitionCount - 1 do
    FPartitions.Add(TMessageQueue.Create(1000));

  WriteLn(Format('[PartitionedQueue] Créé avec %d partitions', [APartitionCount]));
end;

destructor TPartitionedQueue.Destroy;  
begin
  FPartitions.Free;
  inherited;
end;

function TPartitionedQueue.GetPartitionIndex(AMessage: TMessage): Integer;  
var
  HashValue: Cardinal;
  i: Integer;
begin
  // Hash simple basé sur les caractères de l'ID
  HashValue := 0;
  for i := 1 to Length(AMessage.Id) do
    HashValue := HashValue * 31 + Ord(AMessage.Id[i]);
  Result := HashValue mod Cardinal(FPartitionCount);
end;

procedure TPartitionedQueue.Enqueue(AMessage: TMessage);  
var
  PartitionIndex: Integer;
begin
  PartitionIndex := GetPartitionIndex(AMessage);
  FPartitions[PartitionIndex].Enqueue(AMessage);

  WriteLn(Format('[PartitionedQueue] Message ajouté à la partition %d',
    [PartitionIndex]));
end;

function TPartitionedQueue.DequeueFrom(APartitionIndex: Integer): TMessage;  
begin
  if (APartitionIndex >= 0) and (APartitionIndex < FPartitionCount) then
    Result := FPartitions[APartitionIndex].Dequeue
  else
    Result := nil;
end;

end.
```

### 3. Compression des messages

Pour réduire la taille des messages volumineux.

```pascal
unit MessageQueue.Compression;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, zstream;

type
  TMessageCompressor = class
  public
    class function Compress(const AData: string): TBytes;
    class function Decompress(const AData: TBytes): string;
  end;

implementation

class function TMessageCompressor.Compress(const AData: string): TBytes;  
var
  Input: TStringStream;
  Output: TMemoryStream;
  Compressor: TCompressionStream;
begin
  Input := TStringStream.Create(AData);
  Output := TMemoryStream.Create;
  try
    Compressor := TCompressionStream.Create(clMax, Output);
    try
      Compressor.CopyFrom(Input, Input.Size);
    finally
      Compressor.Free;
    end;

    SetLength(Result, Output.Size);
    Output.Position := 0;
    Output.Read(Result[0], Output.Size);

    WriteLn(Format('[Compression] %d → %d bytes (%.1f%%)',
      [Input.Size, Output.Size, (Output.Size / Input.Size) * 100]));
  finally
    Output.Free;
    Input.Free;
  end;
end;

class function TMessageCompressor.Decompress(const AData: TBytes): string;  
var
  Input: TMemoryStream;
  Output: TStringStream;
  Decompressor: TDecompressionStream;
begin
  Input := TMemoryStream.Create;
  Output := TStringStream.Create('');
  try
    Input.Write(AData[0], Length(AData));
    Input.Position := 0;

    Decompressor := TDecompressionStream.Create(Input);
    try
      Output.CopyFrom(Decompressor, 0);
    finally
      Decompressor.Free;
    end;

    Result := Output.DataString;

    WriteLn(Format('[Decompression] %d → %d bytes',
      [Length(AData), Length(Result)]));
  finally
    Output.Free;
    Input.Free;
  end;
end;

end.
```

## Débogage et monitoring

### 1. Message Tracer

Suivre le parcours d'un message dans le système.

```pascal
unit MessageQueue.Tracer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections,
  MessageQueue.Types;

type
  TMessageTrace = record
    Timestamp: TDateTime;
    Component: string;
    Action: string;
    Details: string;
  end;

  TMessageTracer = class
  private
    FTraces: TDictionary<string, TList<TMessageTrace>>;
    FLock: TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;

    procedure TraceMessage(const AMessageId, AComponent, AAction, ADetails: string);
    function GetTrace(const AMessageId: string): TArray<TMessageTrace>;
    procedure PrintTrace(const AMessageId: string);
  end;

var
  GlobalTracer: TMessageTracer;

implementation

constructor TMessageTracer.Create;  
begin
  inherited Create;
  FTraces := TDictionary<string, TList<TMessageTrace>>.Create;
  FLock := TCriticalSection.Create;
end;

destructor TMessageTracer.Destroy;  
var
  List: TList<TMessageTrace>;
begin
  FLock.Enter;
  try
    for List in FTraces.Values do
      List.Free;
    FTraces.Free;
  finally
    FLock.Leave;
  end;

  FLock.Free;
  inherited;
end;

procedure TMessageTracer.TraceMessage(const AMessageId, AComponent,
  AAction, ADetails: string);
var
  Trace: TMessageTrace;
  List: TList<TMessageTrace>;
begin
  Trace.Timestamp := Now;
  Trace.Component := AComponent;
  Trace.Action := AAction;
  Trace.Details := ADetails;

  FLock.Enter;
  try
    if not FTraces.TryGetValue(AMessageId, List) then
    begin
      List := TList<TMessageTrace>.Create;
      FTraces.Add(AMessageId, List);
    end;

    List.Add(Trace);
  finally
    FLock.Leave;
  end;
end;

function TMessageTracer.GetTrace(const AMessageId: string): TArray<TMessageTrace>;  
var
  List: TList<TMessageTrace>;
begin
  FLock.Enter;
  try
    if FTraces.TryGetValue(AMessageId, List) then
      Result := List.ToArray
    else
      SetLength(Result, 0);
  finally
    FLock.Leave;
  end;
end;

procedure TMessageTracer.PrintTrace(const AMessageId: string);  
var
  Traces: TArray<TMessageTrace>;
  Trace: TMessageTrace;
begin
  Traces := GetTrace(AMessageId);

  WriteLn(Format('=== Trace du message: %s ===', [AMessageId]));

  for Trace in Traces do
  begin
    WriteLn(Format('[%s] %s: %s - %s',
      [FormatDateTime('hh:nn:ss.zzz', Trace.Timestamp),
       Trace.Component,
       Trace.Action,
       Trace.Details]));
  end;

  WriteLn('=== Fin du trace ===');
end;

initialization
  GlobalTracer := TMessageTracer.Create;

finalization
  GlobalTracer.Free;

end.
```

### 2. Health Check

Vérifier la santé du système de messaging.

```pascal
unit MessageQueue.HealthCheck;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson,
  MessageQueue.Queue;

type
  THealthStatus = (hsHealthy, hsDegraded, hsUnhealthy);

  TQueueHealth = record
    Status: THealthStatus;
    QueueSize: Integer;
    Utilization: Double;
    OldestMessage: TDateTime;
  end;

  THealthChecker = class
  private
    FQueue: TMessageQueue;
    FMaxQueueSize: Integer;
    FWarningThreshold: Double;
    FCriticalThreshold: Double;
  public
    constructor Create(AQueue: TMessageQueue; AMaxQueueSize: Integer);

    function CheckHealth: TQueueHealth;
    function ToJSON: TJSONObject;
  end;

implementation

uses
  DateUtils;

constructor THealthChecker.Create(AQueue: TMessageQueue; AMaxQueueSize: Integer);  
begin
  inherited Create;
  FQueue := AQueue;
  FMaxQueueSize := AMaxQueueSize;
  FWarningThreshold := 0.7;  // 70%
  FCriticalThreshold := 0.9; // 90%
end;

function THealthChecker.CheckHealth: TQueueHealth;  
var
  QueueSize: Integer;
  Utilization: Double;
begin
  QueueSize := FQueue.Count;
  Utilization := QueueSize / FMaxQueueSize;

  Result.QueueSize := QueueSize;
  Result.Utilization := Utilization;
  Result.OldestMessage := Now; // Simplification

  if Utilization >= FCriticalThreshold then
    Result.Status := hsUnhealthy
  else if Utilization >= FWarningThreshold then
    Result.Status := hsDegraded
  else
    Result.Status := hsHealthy;
end;

function THealthChecker.ToJSON: TJSONObject;  
var
  Health: TQueueHealth;
  StatusStr: string;
begin
  Health := CheckHealth;

  case Health.Status of
    hsHealthy: StatusStr := 'healthy';
    hsDegraded: StatusStr := 'degraded';
    hsUnhealthy: StatusStr := 'unhealthy';
  end;

  Result := TJSONObject.Create;
  Result.Add('status', StatusStr);
  Result.Add('queue_size', Health.QueueSize);
  Result.Add('utilization', Health.Utilization);
  Result.Add('max_size', FMaxQueueSize);
end;

end.
```

## Conclusion

Les **Message Queues** et les **Event Bus** sont des patterns fondamentaux pour créer des architectures découplées, scalables et résilientes.

### Points clés à retenir :

1. **Message Queue** : Communication point-à-point asynchrone
2. **Event Bus** : Communication publish-subscribe
3. **Découplage** : Les composants ne se connaissent pas
4. **Asynchrone** : Pas d'attente bloquante
5. **Résilience** : Gestion des erreurs et retry
6. **Scalabilité** : Traitement parallèle possible

### Bénéfices avec FreePascal :

- **Threads natifs** : Gestion simple du parallélisme
- **Performance** : Code natif rapide
- **Portabilité** : Même code Windows/Linux
- **Contrôle** : Gestion mémoire maîtrisée
- **Interfaces** : Abstraction élégante

### Recommandations :

**Pour démarrer :**
1. Commencer avec une queue simple en mémoire
2. Ajouter le threading pour les consumers
3. Implémenter retry et dead letter
4. Ajouter monitoring et métriques
5. Migrer vers un broker externe si nécessaire

**Pour production :**
1. Utiliser un broker éprouvé (RabbitMQ, Redis, Kafka)
2. Implémenter idempotence
3. Monitoring complet
4. Tests de charge
5. Plan de disaster recovery

### Comparaison finale :

| Besoin | Solution recommandée |
|--------|---------------------|
| Jobs en arrière-plan | Message Queue |
| Notifications multiples | Event Bus |
| Orchestration services | Event Bus + Message Queue |
| Haute disponibilité | Message Queue + Broker externe |
| Time-to-market rapide | Event Bus simple |
| Performance critique | Message Queue optimisée |

### Évolution progressive :

```
Étape 1: Queue en mémoire (prototype)
    ↓
Étape 2: Queue persistée (fichiers)
    ↓
Étape 3: Queue distribuée (Redis)
    ↓
Étape 4: Broker professionnel (RabbitMQ)
    ↓
Étape 5: Streaming (Kafka)
```

Les Message Queues et Event Bus sont essentiels pour les **architectures modernes**, les **microservices**, et les **systèmes distribués**. FreePascal offre tous les outils nécessaires pour les implémenter efficacement, avec la performance native et la portabilité en bonus. 📨

Ces patterns permettent de construire des systèmes qui peuvent **croître**, **évoluer** et **résister aux pannes** tout en restant **maintenables** et **compréhensibles**.

⏭️ [Saga pattern et transactions distribuées](/21-architecture-logicielle-avancee/08-saga-pattern-transactions-distribuees.md)
