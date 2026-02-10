🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 11.8 Acteurs et passage de messages

## Introduction au modèle des acteurs

Le **modèle des acteurs** est un paradigme de programmation concurrent où les "acteurs" sont des entités indépendantes qui communiquent uniquement par **passage de messages**. C'est une alternative aux threads traditionnels avec verrous.

### Qu'est-ce qu'un acteur ?

Un **acteur** est une entité autonome qui :
1. Possède son propre état (données privées)
2. Reçoit des messages de manière asynchrone
3. Traite les messages un par un (séquentiellement)
4. Peut envoyer des messages à d'autres acteurs
5. Peut créer de nouveaux acteurs

**Analogie** : Imaginez un bureau de poste. Chaque employé (acteur) :
- A sa propre pile de courrier (file de messages)
- Traite une lettre à la fois
- Ne partage jamais son espace de travail avec d'autres
- Peut envoyer des lettres à d'autres employés
- Peut embaucher de nouveaux employés si nécessaire

### Pourquoi utiliser le modèle des acteurs ?

**Avantages :**
- ✅ **Pas de verrous** : Pas de deadlocks, pas de race conditions
- ✅ **Isolation** : Chaque acteur est indépendant
- ✅ **Scalabilité** : Facile à distribuer sur plusieurs machines
- ✅ **Résilience** : Un acteur peut crasher sans affecter les autres
- ✅ **Simplicité** : Pas besoin de gérer la synchronisation

**Inconvénients :**
- ❌ Plus d'overhead que les threads directs
- ❌ Nécessite une nouvelle façon de penser
- ❌ Débogage plus complexe (messages asynchrones)

### Comparaison avec les threads traditionnels

```
Threads avec verrous          Acteurs avec messages
━━━━━━━━━━━━━━━━━━━━━━━━     ━━━━━━━━━━━━━━━━━━━━━━━━
Thread 1 ──┐                  Acteur 1 ──> [Message] ──> Acteur 2
           ▼                         │
      [Lock CS]                      └──> [Message] ──> Acteur 3
           │
Thread 2 ──X (attend)          (Pas de verrous, pas d'attente)
```

## Concepts fondamentaux

### Messages

Un **message** est une donnée envoyée d'un acteur à un autre. Les messages sont :
- **Immuables** : Ne peuvent pas être modifiés
- **Asynchrones** : L'envoi ne bloque pas
- **Ordonnés** : Reçus dans l'ordre d'envoi (pour un même expéditeur)

```pascal
type
  // Message de base
  TMessage = class
  private
    FSender: TActor;
    FTimestamp: TDateTime;
  public
    constructor Create(Sender: TActor);
    property Sender: TActor read FSender;
    property Timestamp: TDateTime read FTimestamp;
  end;

  // Message spécifique
  TTextMessage = class(TMessage)
  private
    FText: string;
  public
    constructor Create(Sender: TActor; const AText: string);
    property Text: string read FText;
  end;
```

### Mailbox (Boîte aux lettres)

La **mailbox** est la file d'attente des messages d'un acteur.

```pascal
type
  TMailbox = class
  private
    FMessages: TThreadedQueue<TMessage>;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Post(Msg: TMessage);
    function Receive(out Msg: TMessage; Timeout: Cardinal): Boolean;
    function Count: Integer;
  end;
```

### Contexte d'acteur

Le **contexte** fournit des services à l'acteur (création d'autres acteurs, supervision, etc.).

```pascal
type
  TActorContext = class
  public
    function CreateActor(ActorClass: TActorClass): TActor;
    procedure Send(Target: TActor; Msg: TMessage);
    procedure Stop(Target: TActor);
  end;
```

## Implémentation de base d'un système d'acteurs

### Structure de l'acteur

> **Note :** Les unités utilisant des types génériques (`TThreadedQueue<T>`, `TDictionary<K,V>`, `TList<T>`, etc.) de `Generics.Collections` sont compilées en `{$mode delphi}` pour simplifier la syntaxe des génériques. En `{$mode objfpc}`, il faudrait préfixer chaque utilisation avec le mot-clé `specialize`.

```pascal
unit ActorSystem;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs, Generics.Collections;

type
  TActor = class;
  TActorClass = class of TActor;

  // Message de base
  TMessage = class
  private
    FSender: TActor;
  public
    constructor Create(ASender: TActor);
    property Sender: TActor read FSender;
  end;

  // Mailbox
  TMailbox = class
  private
    FQueue: TThreadedQueue<TMessage>;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Post(Msg: TMessage);
    function Receive(out Msg: TMessage; Timeout: Cardinal = INFINITE): Boolean;
    function IsEmpty: Boolean;
    function Count: Integer;
  end;

  // Acteur de base
  TActor = class
  private
    FMailbox: TMailbox;
    FThread: TThread;
    FRunning: Boolean;
    FName: string;

    procedure ProcessMessages;
  protected
    procedure Receive(Msg: TMessage); virtual; abstract;
    procedure PreStart; virtual;
    procedure PostStop; virtual;
  public
    constructor Create(const AName: string);
    destructor Destroy; override;

    procedure Start;
    procedure Stop;
    procedure Send(Msg: TMessage);

    property Name: string read FName;
    property Mailbox: TMailbox read FMailbox;
  end;

implementation

{ TMessage }

constructor TMessage.Create(ASender: TActor);  
begin
  inherited Create;
  FSender := ASender;
end;

{ TMailbox }

constructor TMailbox.Create;  
begin
  inherited Create;
  FQueue := TThreadedQueue<TMessage>.Create(1000, INFINITE, INFINITE);
end;

destructor TMailbox.Destroy;  
var
  Msg: TMessage;
begin
  // Nettoyer les messages restants
  while FQueue.PopItem(Msg) = wrSignaled do
    Msg.Free;

  FQueue.Free;
  inherited;
end;

procedure TMailbox.Post(Msg: TMessage);  
begin
  FQueue.PushItem(Msg);
end;

function TMailbox.Receive(out Msg: TMessage; Timeout: Cardinal): Boolean;  
begin
  Result := FQueue.PopItem(Msg, Timeout) = wrSignaled;
end;

function TMailbox.IsEmpty: Boolean;  
begin
  Result := FQueue.TotalItemsPushed = FQueue.TotalItemsPopped;
end;

function TMailbox.Count: Integer;  
begin
  Result := FQueue.TotalItemsPushed - FQueue.TotalItemsPopped;
end;

{ TActor }

constructor TActor.Create(const AName: string);  
begin
  inherited Create;
  FName := AName;
  FMailbox := TMailbox.Create;
  FRunning := False;
end;

destructor TActor.Destroy;  
begin
  Stop;
  FMailbox.Free;
  inherited;
end;

procedure TActor.Start;  
begin
  if FRunning then
    Exit;

  FRunning := True;

  // Appeler le hook de démarrage
  PreStart;

  // Créer le thread de traitement
  // Note : On utilise CreateAnonymousThread avec une procédure anonyme
  // (supporté en {$mode delphi}), qui capture Self pour appeler ProcessMessages
  FThread := TThread.CreateAnonymousThread(
    procedure
    begin
      ProcessMessages;
    end
  );
  FThread.FreeOnTerminate := False;
  FThread.Start;
end;

procedure TActor.Stop;  
begin
  if not FRunning then
    Exit;

  FRunning := False;

  if Assigned(FThread) then
  begin
    FThread.WaitFor;
    FThread.Free;
    FThread := nil;
  end;

  // Appeler le hook d'arrêt
  PostStop;
end;

procedure TActor.Send(Msg: TMessage);  
begin
  FMailbox.Post(Msg);
end;

procedure TActor.ProcessMessages;  
var
  Msg: TMessage;
begin
  while FRunning do
  begin
    if FMailbox.Receive(Msg, 100) then
    begin
      try
        Receive(Msg);
      finally
        Msg.Free;
      end;
    end;
  end;
end;

procedure TActor.PreStart;  
begin
  // Hook vide, à surcharger si nécessaire
end;

procedure TActor.PostStop;  
begin
  // Hook vide, à surcharger si nécessaire
end;

end.
```

## Exemple : Acteur simple

### Acteur qui compte

```pascal
type
  // Messages
  TIncrementMessage = class(TMessage);
  TDecrementMessage = class(TMessage);
  TGetValueMessage = class(TMessage);
  TValueResponseMessage = class(TMessage)
  private
    FValue: Integer;
  public
    constructor Create(ASender: TActor; AValue: Integer);
    property Value: Integer read FValue;
  end;

  // Acteur compteur
  TCounterActor = class(TActor)
  private
    FValue: Integer;
  protected
    procedure Receive(Msg: TMessage); override;
  public
    constructor Create(const AName: string);
  end;

implementation

constructor TValueResponseMessage.Create(ASender: TActor; AValue: Integer);  
begin
  inherited Create(ASender);
  FValue := AValue;
end;

constructor TCounterActor.Create(const AName: string);  
begin
  inherited Create(AName);
  FValue := 0;
end;

procedure TCounterActor.Receive(Msg: TMessage);  
begin
  if Msg is TIncrementMessage then
  begin
    Inc(FValue);
    WriteLn(Format('[%s] Incrémenté : %d', [Name, FValue]));
  end
  else if Msg is TDecrementMessage then
  begin
    Dec(FValue);
    WriteLn(Format('[%s] Décrémenté : %d', [Name, FValue]));
  end
  else if Msg is TGetValueMessage then
  begin
    // Répondre à l'expéditeur
    if Assigned(Msg.Sender) then
      Msg.Sender.Send(TValueResponseMessage.Create(Self, FValue));
  end;
end;
```

### Utilisation

```pascal
var
  Counter: TCounterActor;
begin
  Counter := TCounterActor.Create('Compteur1');
  try
    Counter.Start;

    // Envoyer des messages
    Counter.Send(TIncrementMessage.Create(nil));
    Counter.Send(TIncrementMessage.Create(nil));
    Counter.Send(TDecrementMessage.Create(nil));

    // Attendre un peu pour le traitement
    Sleep(100);
  finally
    Counter.Free;
  end;
end;
```

## Pattern Request-Reply

Communication synchrone entre acteurs avec attente de réponse.

```pascal
unit RequestReplyPattern;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs, Generics.Collections, ActorSystem;

type
  // Message de requête avec ID
  TRequestMessage = class(TMessage)
  private
    FRequestID: TGUID;
  public
    constructor Create(ASender: TActor);
    property RequestID: TGUID read FRequestID;
  end;

  // Message de réponse
  TReplyMessage = class(TMessage)
  private
    FRequestID: TGUID;
    FResult: Variant;
  public
    constructor Create(ASender: TActor; const ARequestID: TGUID; const AResult: Variant);
    property RequestID: TGUID read FRequestID;
    property Result: Variant read FResult;
  end;

  // Gestionnaire de requêtes/réponses
  TRequestReplyManager = class
  private
    FPendingRequests: TDictionary<TGUID, TEvent>;
    FResponses: TDictionary<TGUID, Variant>;
    FCS: TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;

    function SendAndWait(Target: TActor; Request: TRequestMessage;
      Timeout: Cardinal = 5000): Variant;
    procedure HandleReply(Reply: TReplyMessage);
  end;

implementation

{ TRequestMessage }

constructor TRequestMessage.Create(ASender: TActor);  
begin
  inherited Create(ASender);
  CreateGUID(FRequestID);
end;

{ TReplyMessage }

constructor TReplyMessage.Create(ASender: TActor; const ARequestID: TGUID;
  const AResult: Variant);
begin
  inherited Create(ASender);
  FRequestID := ARequestID;
  FResult := AResult;
end;

{ TRequestReplyManager }

constructor TRequestReplyManager.Create;  
begin
  inherited Create;
  FPendingRequests := TDictionary<TGUID, TEvent>.Create;
  FResponses := TDictionary<TGUID, Variant>.Create;
  FCS := TCriticalSection.Create;
end;

destructor TRequestReplyManager.Destroy;  
begin
  FPendingRequests.Free;
  FResponses.Free;
  FCS.Free;
  inherited;
end;

function TRequestReplyManager.SendAndWait(Target: TActor;
  Request: TRequestMessage; Timeout: Cardinal): Variant;
var
  Event: TEvent;
begin
  Event := TEvent.Create(nil, True, False, '');

  FCS.Enter;
  try
    FPendingRequests.Add(Request.RequestID, Event);
  finally
    FCS.Leave;
  end;

  try
    // Envoyer la requête
    Target.Send(Request);

    // Attendre la réponse
    if Event.WaitFor(Timeout) = wrSignaled then
    begin
      FCS.Enter;
      try
        Result := FResponses[Request.RequestID];
        FResponses.Remove(Request.RequestID);
      finally
        FCS.Leave;
      end;
    end
    else
      raise Exception.Create('Timeout : pas de réponse reçue');
  finally
    FCS.Enter;
    try
      FPendingRequests.Remove(Request.RequestID);
    finally
      FCS.Leave;
    end;
    Event.Free;
  end;
end;

procedure TRequestReplyManager.HandleReply(Reply: TReplyMessage);  
var
  Event: TEvent;
begin
  FCS.Enter;
  try
    if FPendingRequests.TryGetValue(Reply.RequestID, Event) then
    begin
      FResponses.Add(Reply.RequestID, Reply.Result);
      Event.SetEvent;
    end;
  finally
    FCS.Leave;
  end;
end;

end.
```

## Supervision et résilience

Le modèle des acteurs permet de créer des hiérarchies de supervision pour la gestion des erreurs.

```pascal
unit SupervisionPattern;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, ActorSystem, Generics.Collections;

type
  TSupervisionStrategy = (ssRestart, ssStop, ssResume);

  // Message d'erreur
  TErrorMessage = class(TMessage)
  private
    FError: Exception;
    FFailedActor: TActor;
  public
    constructor Create(AFailedActor: TActor; AError: Exception);
    property Error: Exception read FError;
    property FailedActor: TActor read FFailedActor;
  end;

  // Superviseur d'acteurs
  TSupervisorActor = class(TActor)
  private
    FChildren: TList<TActor>;
    FStrategy: TSupervisionStrategy;
  protected
    procedure Receive(Msg: TMessage); override;
    procedure HandleChildError(Child: TActor; Error: Exception); virtual;
  public
    constructor Create(const AName: string; Strategy: TSupervisionStrategy);
    destructor Destroy; override;

    procedure AddChild(Child: TActor);
    procedure RemoveChild(Child: TActor);
  end;

implementation

{ TErrorMessage }

constructor TErrorMessage.Create(AFailedActor: TActor; AError: Exception);  
begin
  inherited Create(nil);
  FFailedActor := AFailedActor;
  FError := AError;
end;

{ TSupervisorActor }

constructor TSupervisorActor.Create(const AName: string; Strategy: TSupervisionStrategy);  
begin
  inherited Create(AName);
  FChildren := TList<TActor>.Create;
  FStrategy := Strategy;
end;

destructor TSupervisorActor.Destroy;  
var
  Child: TActor;
begin
  for Child in FChildren do
    Child.Free;
  FChildren.Free;
  inherited;
end;

procedure TSupervisorActor.AddChild(Child: TActor);  
begin
  FChildren.Add(Child);
  Child.Start;
end;

procedure TSupervisorActor.RemoveChild(Child: TActor);  
begin
  FChildren.Remove(Child);
  Child.Stop;
end;

procedure TSupervisorActor.Receive(Msg: TMessage);  
begin
  if Msg is TErrorMessage then
    HandleChildError(TErrorMessage(Msg).FailedActor, TErrorMessage(Msg).Error);
end;

procedure TSupervisorActor.HandleChildError(Child: TActor; Error: Exception);  
begin
  WriteLn(Format('[%s] Enfant %s a échoué : %s', [Name, Child.Name, Error.Message]));

  case FStrategy of
    ssRestart:
    begin
      WriteLn(Format('[%s] Redémarrage de %s', [Name, Child.Name]));
      Child.Stop;
      Child.Start;
    end;

    ssStop:
    begin
      WriteLn(Format('[%s] Arrêt de %s', [Name, Child.Name]));
      RemoveChild(Child);
    end;

    ssResume:
    begin
      WriteLn(Format('[%s] Reprise de %s', [Name, Child.Name]));
      // L'acteur continue comme si de rien n'était
    end;
  end;
end;

end.
```

## Pattern Publish-Subscribe

Communication un-vers-plusieurs avec abonnements.

```pascal
unit PubSubPattern;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, ActorSystem, Generics.Collections;

type
  // Messages de gestion des abonnements
  TSubscribeMessage = class(TMessage)
  private
    FTopic: string;
  public
    constructor Create(ASender: TActor; const ATopic: string);
    property Topic: string read FTopic;
  end;

  TUnsubscribeMessage = class(TMessage)
  private
    FTopic: string;
  public
    constructor Create(ASender: TActor; const ATopic: string);
    property Topic: string read FTopic;
  end;

  // Message publié
  TPublishMessage = class(TMessage)
  private
    FTopic: string;
    FData: Variant;
  public
    constructor Create(ASender: TActor; const ATopic: string; const AData: Variant);
    property Topic: string read FTopic;
    property Data: Variant read FData;
  end;

  // Acteur Event Bus
  TEventBusActor = class(TActor)
  private
    FSubscribers: TDictionary<string, TList<TActor>>;
  protected
    procedure Receive(Msg: TMessage); override;
  public
    constructor Create(const AName: string);
    destructor Destroy; override;
  end;

implementation

{ TSubscribeMessage }

constructor TSubscribeMessage.Create(ASender: TActor; const ATopic: string);  
begin
  inherited Create(ASender);
  FTopic := ATopic;
end;

{ TUnsubscribeMessage }

constructor TUnsubscribeMessage.Create(ASender: TActor; const ATopic: string);  
begin
  inherited Create(ASender);
  FTopic := ATopic;
end;

{ TPublishMessage }

constructor TPublishMessage.Create(ASender: TActor; const ATopic: string;
  const AData: Variant);
begin
  inherited Create(ASender);
  FTopic := ATopic;
  FData := AData;
end;

{ TEventBusActor }

constructor TEventBusActor.Create(const AName: string);  
begin
  inherited Create(AName);
  FSubscribers := TDictionary<string, TList<TActor>>.Create;
end;

destructor TEventBusActor.Destroy;  
var
  List: TList<TActor>;
begin
  for List in FSubscribers.Values do
    List.Free;
  FSubscribers.Free;
  inherited;
end;

procedure TEventBusActor.Receive(Msg: TMessage);  
var
  Subscribers: TList<TActor>;
  Subscriber: TActor;
begin
  if Msg is TSubscribeMessage then
  begin
    // S'abonner à un topic
    if not FSubscribers.TryGetValue(TSubscribeMessage(Msg).Topic, Subscribers) then
    begin
      Subscribers := TList<TActor>.Create;
      FSubscribers.Add(TSubscribeMessage(Msg).Topic, Subscribers);
    end;

    if not Subscribers.Contains(Msg.Sender) then
    begin
      Subscribers.Add(Msg.Sender);
      WriteLn(Format('[%s] %s s''abonne à "%s"',
        [Name, Msg.Sender.Name, TSubscribeMessage(Msg).Topic]));
    end;
  end
  else if Msg is TUnsubscribeMessage then
  begin
    // Se désabonner d'un topic
    if FSubscribers.TryGetValue(TUnsubscribeMessage(Msg).Topic, Subscribers) then
    begin
      Subscribers.Remove(Msg.Sender);
      WriteLn(Format('[%s] %s se désabonne de "%s"',
        [Name, Msg.Sender.Name, TUnsubscribeMessage(Msg).Topic]));
    end;
  end
  else if Msg is TPublishMessage then
  begin
    // Publier un message sur un topic
    if FSubscribers.TryGetValue(TPublishMessage(Msg).Topic, Subscribers) then
    begin
      WriteLn(Format('[%s] Publication sur "%s" vers %d abonnés',
        [Name, TPublishMessage(Msg).Topic, Subscribers.Count]));

      for Subscriber in Subscribers do
        Subscriber.Send(TPublishMessage.Create(Self,
          TPublishMessage(Msg).Topic,
          TPublishMessage(Msg).Data));
    end;
  end;
end;

end.
```

### Utilisation du Pub/Sub

```pascal
type
  TSubscriberActor = class(TActor)
  protected
    procedure Receive(Msg: TMessage); override;
  end;

procedure TSubscriberActor.Receive(Msg: TMessage);  
begin
  if Msg is TPublishMessage then
    WriteLn(Format('[%s] Reçu sur "%s" : %s',
      [Name, TPublishMessage(Msg).Topic,
       VarToStr(TPublishMessage(Msg).Data)])); // VarToStr nécessite uses Variants
end;

var
  EventBus: TEventBusActor;
  Sub1, Sub2: TSubscriberActor;
begin
  EventBus := TEventBusActor.Create('EventBus');
  Sub1 := TSubscriberActor.Create('Abonné1');
  Sub2 := TSubscriberActor.Create('Abonné2');

  try
    EventBus.Start;
    Sub1.Start;
    Sub2.Start;

    // S'abonner
    EventBus.Send(TSubscribeMessage.Create(Sub1, 'news'));
    EventBus.Send(TSubscribeMessage.Create(Sub2, 'news'));

    // Publier
    EventBus.Send(TPublishMessage.Create(nil, 'news', 'Breaking news!'));

    Sleep(100);
  finally
    Sub2.Free;
    Sub1.Free;
    EventBus.Free;
  end;
end;
```

## Exemple complet : Chat système

Un système de chat distribué utilisant le modèle des acteurs.

```pascal
unit ChatSystem;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, ActorSystem, Generics.Collections;

type
  // Messages
  TJoinRoomMessage = class(TMessage)
  private
    FRoomName: string;
  public
    constructor Create(ASender: TActor; const ARoomName: string);
    property RoomName: string read FRoomName;
  end;

  TLeaveRoomMessage = class(TMessage)
  private
    FRoomName: string;
  public
    constructor Create(ASender: TActor; const ARoomName: string);
    property RoomName: string read FRoomName;
  end;

  TChatMessage = class(TMessage)
  private
    FRoomName: string;
    FText: string;
  public
    constructor Create(ASender: TActor; const ARoomName, AText: string);
    property RoomName: string read FRoomName;
    property Text: string read FText;
  end;

  // Acteur utilisateur
  TUserActor = class(TActor)
  private
    FUsername: string;
  protected
    procedure Receive(Msg: TMessage); override;
  public
    constructor Create(const AUsername: string);
  end;

  // Acteur salle de chat
  TChatRoomActor = class(TActor)
  private
    FUsers: TList<TActor>;
  protected
    procedure Receive(Msg: TMessage); override;
  public
    constructor Create(const ARoomName: string);
    destructor Destroy; override;
  end;

  // Acteur serveur de chat
  TChatServerActor = class(TActor)
  private
    FRooms: TDictionary<string, TChatRoomActor>;
  protected
    procedure Receive(Msg: TMessage); override;
  public
    constructor Create;
    destructor Destroy; override;

    function GetOrCreateRoom(const RoomName: string): TChatRoomActor;
  end;

implementation

{ TJoinRoomMessage }

constructor TJoinRoomMessage.Create(ASender: TActor; const ARoomName: string);  
begin
  inherited Create(ASender);
  FRoomName := ARoomName;
end;

{ TLeaveRoomMessage }

constructor TLeaveRoomMessage.Create(ASender: TActor; const ARoomName: string);  
begin
  inherited Create(ASender);
  FRoomName := ARoomName;
end;

{ TChatMessage }

constructor TChatMessage.Create(ASender: TActor; const ARoomName, AText: string);  
begin
  inherited Create(ASender);
  FRoomName := ARoomName;
  FText := AText;
end;

{ TUserActor }

constructor TUserActor.Create(const AUsername: string);  
begin
  inherited Create(AUsername);
  FUsername := AUsername;
end;

procedure TUserActor.Receive(Msg: TMessage);  
begin
  if Msg is TChatMessage then
  begin
    WriteLn(Format('[%s] Message dans %s : %s',
      [FUsername, TChatMessage(Msg).RoomName, TChatMessage(Msg).Text]));
  end;
end;

{ TChatRoomActor }

constructor TChatRoomActor.Create(const ARoomName: string);  
begin
  inherited Create(ARoomName);
  FUsers := TList<TActor>.Create;
end;

destructor TChatRoomActor.Destroy;  
begin
  FUsers.Free;
  inherited;
end;

procedure TChatRoomActor.Receive(Msg: TMessage);  
var
  User: TActor;
begin
  if Msg is TJoinRoomMessage then
  begin
    if not FUsers.Contains(Msg.Sender) then
    begin
      FUsers.Add(Msg.Sender);
      WriteLn(Format('[%s] %s a rejoint la salle', [Name, Msg.Sender.Name]));
    end;
  end
  else if Msg is TLeaveRoomMessage then
  begin
    FUsers.Remove(Msg.Sender);
    WriteLn(Format('[%s] %s a quitté la salle', [Name, Msg.Sender.Name]));
  end
  else if Msg is TChatMessage then
  begin
    // Diffuser le message à tous les utilisateurs
    for User in FUsers do
      if User <> Msg.Sender then
        User.Send(TChatMessage.Create(Msg.Sender, Name, TChatMessage(Msg).Text));
  end;
end;

{ TChatServerActor }

constructor TChatServerActor.Create;  
begin
  inherited Create('ChatServer');
  FRooms := TDictionary<string, TChatRoomActor>.Create;
end;

destructor TChatServerActor.Destroy;  
var
  Room: TChatRoomActor;
begin
  for Room in FRooms.Values do
    Room.Free;
  FRooms.Free;
  inherited;
end;

function TChatServerActor.GetOrCreateRoom(const RoomName: string): TChatRoomActor;  
begin
  if not FRooms.TryGetValue(RoomName, Result) then
  begin
    Result := TChatRoomActor.Create(RoomName);
    Result.Start;
    FRooms.Add(RoomName, Result);
    WriteLn(Format('[%s] Salle "%s" créée', [Name, RoomName]));
  end;
end;

procedure TChatServerActor.Receive(Msg: TMessage);  
var
  Room: TChatRoomActor;
begin
  if Msg is TJoinRoomMessage then
  begin
    Room := GetOrCreateRoom(TJoinRoomMessage(Msg).RoomName);
    Room.Send(Msg);
  end
  else if Msg is TLeaveRoomMessage then
  begin
    if FRooms.TryGetValue(TLeaveRoomMessage(Msg).RoomName, Room) then
      Room.Send(Msg);
  end
  else if Msg is TChatMessage then
  begin
    if FRooms.TryGetValue(TChatMessage(Msg).RoomName, Room) then
      Room.Send(Msg);
  end;
end;

end.
```

### Utilisation du système de chat

```pascal
program ChatDemo;

{$mode objfpc}{$H+}

uses
  SysUtils, ActorSystem, ChatSystem;

var
  Server: TChatServerActor;
  Alice, Bob, Charlie: TUserActor;

begin
  // Créer le serveur et les utilisateurs
  Server := TChatServerActor.Create;
  Alice := TUserActor.Create('Alice');
  Bob := TUserActor.Create('Bob');
  Charlie := TUserActor.Create('Charlie');

  try
    // Démarrer tous les acteurs
    Server.Start;
    Alice.Start;
    Bob.Start;
    Charlie.Start;

    // Alice et Bob rejoignent la salle "général"
    Server.Send(TJoinRoomMessage.Create(Alice, 'général'));
    Server.Send(TJoinRoomMessage.Create(Bob, 'général'));

    Sleep(100);

    // Alice envoie un message
    Server.Send(TChatMessage.Create(Alice, 'général', 'Salut tout le monde!'));

    Sleep(100);

    // Charlie rejoint
    Server.Send(TJoinRoomMessage.Create(Charlie, 'général'));

    Sleep(100);

    // Bob répond
    Server.Send(TChatMessage.Create(Bob, 'général', 'Salut Alice!'));

    Sleep(100);

    // Alice part
    Server.Send(TLeaveRoomMessage.Create(Alice, 'général'));

    Sleep(100);

  finally
    Charlie.Free;
    Bob.Free;
    Alice.Free;
    Server.Free;
  end;
end.
```

## Patterns avancés

### 1. Router pattern (Routage de messages)

Distribution intelligente des messages vers plusieurs acteurs.

```pascal
unit RouterPattern;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, ActorSystem, Generics.Collections;

type
  TRoutingStrategy = (rsRoundRobin, rsRandom, rsSmallestMailbox);

  TRouterActor = class(TActor)
  private
    FRoutees: TList<TActor>;
    FStrategy: TRoutingStrategy;
    FCurrentIndex: Integer;

    function SelectRoutee: TActor;
  protected
    procedure Receive(Msg: TMessage); override;
  public
    constructor Create(const AName: string; Strategy: TRoutingStrategy);
    destructor Destroy; override;

    procedure AddRoutee(Routee: TActor);
  end;

implementation

constructor TRouterActor.Create(const AName: string; Strategy: TRoutingStrategy);  
begin
  inherited Create(AName);
  FRoutees := TList<TActor>.Create;
  FStrategy := Strategy;
  FCurrentIndex := 0;
end;

destructor TRouterActor.Destroy;  
begin
  FRoutees.Free;
  inherited;
end;

procedure TRouterActor.AddRoutee(Routee: TActor);  
begin
  FRoutees.Add(Routee);
end;

function TRouterActor.SelectRoutee: TActor;  
var
  i, MinSize, Size: Integer;
begin
  Result := nil;

  if FRoutees.Count = 0 then
    Exit;

  case FStrategy of
    rsRoundRobin:
    begin
      Result := FRoutees[FCurrentIndex];
      FCurrentIndex := (FCurrentIndex + 1) mod FRoutees.Count;
    end;

    rsRandom:
    begin
      Result := FRoutees[Random(FRoutees.Count)];
    end;

    rsSmallestMailbox:
    begin
      MinSize := MaxInt;
      for i := 0 to FRoutees.Count - 1 do
      begin
        Size := FRoutees[i].Mailbox.Count;
        if Size < MinSize then
        begin
          MinSize := Size;
          Result := FRoutees[i];
        end;
      end;
    end;
  end;
end;

procedure TRouterActor.Receive(Msg: TMessage);  
var
  Routee: TActor;
begin
  Routee := SelectRoutee;
  if Assigned(Routee) then
  begin
    WriteLn(Format('[%s] Routage vers %s', [Name, Routee.Name]));
    Routee.Send(Msg);
  end;
end;

end.
```

### 2. Pipeline pattern

Traitement séquentiel par plusieurs acteurs.

```pascal
unit PipelinePattern;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ActorSystem, Generics.Collections;

type
  TPipelineMessage = class(TMessage)
  private
    FData: string;
    FStageNumber: Integer;
  public
    constructor Create(ASender: TActor; const AData: string; AStage: Integer);
    property Data: string read FData write FData;
    property StageNumber: Integer read FStageNumber;
  end;

  TPipelineStageActor = class(TActor)
  private
    FNextStage: TActor;
    FStageNumber: Integer;
  protected
    procedure Receive(Msg: TMessage); override;
    function ProcessData(const Data: string): string; virtual; abstract;
  public
    constructor Create(const AName: string; AStageNumber: Integer);
    procedure SetNextStage(NextStage: TActor);
  end;

  // Étape 1 : Convertir en majuscules
  TUpperCaseStage = class(TPipelineStageActor)
  protected
    function ProcessData(const Data: string): string; override;
  end;

  // Étape 2 : Ajouter un préfixe
  TPrefixStage = class(TPipelineStageActor)
  protected
    function ProcessData(const Data: string): string; override;
  end;

  // Étape 3 : Inverser
  TReverseStage = class(TPipelineStageActor)
  protected
    function ProcessData(const Data: string): string; override;
  end;

implementation

{ TPipelineMessage }

constructor TPipelineMessage.Create(ASender: TActor; const AData: string; AStage: Integer);  
begin
  inherited Create(ASender);
  FData := AData;
  FStageNumber := AStage;
end;

{ TPipelineStageActor }

constructor TPipelineStageActor.Create(const AName: string; AStageNumber: Integer);  
begin
  inherited Create(AName);
  FStageNumber := AStageNumber;
  FNextStage := nil;
end;

procedure TPipelineStageActor.SetNextStage(NextStage: TActor);  
begin
  FNextStage := NextStage;
end;

procedure TPipelineStageActor.Receive(Msg: TMessage);  
var
  PipeMsg: TPipelineMessage;
  ProcessedData: string;
begin
  if Msg is TPipelineMessage then
  begin
    PipeMsg := TPipelineMessage(Msg);

    WriteLn(Format('[%s] Étape %d : "%s"', [Name, FStageNumber, PipeMsg.Data]));

    // Traiter les données
    ProcessedData := ProcessData(PipeMsg.Data);

    // Passer à l'étape suivante
    if Assigned(FNextStage) then
      FNextStage.Send(TPipelineMessage.Create(Self, ProcessedData, FStageNumber + 1))
    else
      WriteLn(Format('[%s] Résultat final : "%s"', [Name, ProcessedData]));
  end;
end;

{ TUpperCaseStage }

function TUpperCaseStage.ProcessData(const Data: string): string;  
begin
  Result := UpperCase(Data);
end;

{ TPrefixStage }

function TPrefixStage.ProcessData(const Data: string): string;  
begin
  Result := '>>> ' + Data;
end;

{ TReverseStage }

function TReverseStage.ProcessData(const Data: string): string;  
var
  i: Integer;
begin
  Result := '';
  for i := Length(Data) downto 1 do
    Result := Result + Data[i];
end;

end.
```

### Utilisation du pipeline

```pascal
var
  Stage1: TUpperCaseStage;
  Stage2: TPrefixStage;
  Stage3: TReverseStage;
begin
  Stage1 := TUpperCaseStage.Create('UpperCase', 1);
  Stage2 := TPrefixStage.Create('Prefix', 2);
  Stage3 := TReverseStage.Create('Reverse', 3);

  try
    // Configurer le pipeline
    Stage1.SetNextStage(Stage2);
    Stage2.SetNextStage(Stage3);

    // Démarrer
    Stage1.Start;
    Stage2.Start;
    Stage3.Start;

    // Envoyer des données dans le pipeline
    Stage1.Send(TPipelineMessage.Create(nil, 'Hello World', 0));

    Sleep(100);
  finally
    Stage3.Free;
    Stage2.Free;
    Stage1.Free;
  end;
end;
```

### 3. Scatter-Gather pattern

Distribuer un travail, puis agréger les résultats.

```pascal
unit ScatterGatherPattern;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ActorSystem, Generics.Collections;

type
  TWorkMessage = class(TMessage)
  private
    FWorkID: Integer;
    FData: Integer;
  public
    constructor Create(ASender: TActor; AWorkID, AData: Integer);
    property WorkID: Integer read FWorkID;
    property Data: Integer read FData;
  end;

  TResultMessage = class(TMessage)
  private
    FWorkID: Integer;
    FResult: Integer;
  public
    constructor Create(ASender: TActor; AWorkID, AResult: Integer);
    property WorkID: Integer read FWorkID;
    property Result: Integer read FResult;
  end;

  TWorkerActor = class(TActor)
  protected
    procedure Receive(Msg: TMessage); override;
  end;

  TAggregatorActor = class(TActor)
  private
    FExpectedResults: Integer;
    FReceivedResults: Integer;
    FTotalResult: Integer;
  protected
    procedure Receive(Msg: TMessage); override;
  public
    constructor Create(const AName: string; ExpectedResults: Integer);
  end;

implementation

{ TWorkMessage }

constructor TWorkMessage.Create(ASender: TActor; AWorkID, AData: Integer);  
begin
  inherited Create(ASender);
  FWorkID := AWorkID;
  FData := AData;
end;

{ TResultMessage }

constructor TResultMessage.Create(ASender: TActor; AWorkID, AResult: Integer);  
begin
  inherited Create(ASender);
  FWorkID := AWorkID;
  FResult := AResult;
end;

{ TWorkerActor }

procedure TWorkerActor.Receive(Msg: TMessage);  
var
  WorkMsg: TWorkMessage;
  Result: Integer;
begin
  if Msg is TWorkMessage then
  begin
    WorkMsg := TWorkMessage(Msg);

    WriteLn(Format('[%s] Traitement du travail %d', [Name, WorkMsg.WorkID]));

    // Simuler un calcul
    Sleep(Random(500));
    Result := WorkMsg.Data * WorkMsg.Data;

    // Renvoyer le résultat
    Msg.Sender.Send(TResultMessage.Create(Self, WorkMsg.WorkID, Result));
  end;
end;

{ TAggregatorActor }

constructor TAggregatorActor.Create(const AName: string; ExpectedResults: Integer);  
begin
  inherited Create(AName);
  FExpectedResults := ExpectedResults;
  FReceivedResults := 0;
  FTotalResult := 0;
end;

procedure TAggregatorActor.Receive(Msg: TMessage);  
var
  ResultMsg: TResultMessage;
begin
  if Msg is TResultMessage then
  begin
    ResultMsg := TResultMessage(Msg);

    WriteLn(Format('[%s] Résultat %d reçu : %d',
      [Name, ResultMsg.WorkID, ResultMsg.Result]));

    FTotalResult := FTotalResult + ResultMsg.Result;
    Inc(FReceivedResults);

    if FReceivedResults >= FExpectedResults then
      WriteLn(Format('[%s] Tous les résultats reçus. Total : %d',
        [Name, FTotalResult]));
  end;
end;

end.
```

### Utilisation du Scatter-Gather

```pascal
var
  Aggregator: TAggregatorActor;
  Workers: array[0..3] of TWorkerActor;
  i: Integer;
begin
  // Créer l'agrégateur
  Aggregator := TAggregatorActor.Create('Aggregator', 4);

  // Créer les workers
  for i := 0 to 3 do
    Workers[i] := TWorkerActor.Create('Worker' + IntToStr(i + 1));

  try
    Aggregator.Start;
    for i := 0 to 3 do
      Workers[i].Start;

    // Scatter : distribuer le travail
    for i := 0 to 3 do
      Workers[i].Send(TWorkMessage.Create(Aggregator, i + 1, (i + 1) * 10));

    // Gather : attendre les résultats
    Sleep(2000);
  finally
    for i := 0 to 3 do
      Workers[i].Free;
    Aggregator.Free;
  end;
end;
```

## Performance et optimisations

### Mesurer les performances

```pascal
unit ActorPerformance;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils, ActorSystem;

type
  TPerformanceMetrics = record
    MessagesSent: Int64;
    MessagesProcessed: Int64;
    AverageLatency: Double;
    Throughput: Double;
  end;

function BenchmarkActor(Actor: TActor; MessageCount: Integer): TPerformanceMetrics;

implementation

type
  TBenchmarkMessage = class(TMessage)
  private
    FTimestamp: TDateTime;
  public
    constructor Create(ASender: TActor);
    property Timestamp: TDateTime read FTimestamp;
  end;

constructor TBenchmarkMessage.Create(ASender: TActor);  
begin
  inherited Create(ASender);
  FTimestamp := Now;
end;

function BenchmarkActor(Actor: TActor; MessageCount: Integer): TPerformanceMetrics;  
var
  i: Integer;
  StartTime, EndTime: TDateTime;
begin
  StartTime := Now;

  // Envoyer les messages
  for i := 1 to MessageCount do
    Actor.Send(TBenchmarkMessage.Create(nil));

  // Attendre le traitement
  while not Actor.Mailbox.IsEmpty do
    Sleep(10);

  EndTime := Now;

  // Calculer les métriques
  Result.MessagesSent := MessageCount;
  Result.MessagesProcessed := MessageCount;
  Result.AverageLatency := MilliSecondsBetween(EndTime, StartTime) / MessageCount;
  Result.Throughput := MessageCount / (MilliSecondsBetween(EndTime, StartTime) / 1000);

  WriteLn('=== Métriques de performance ===');
  WriteLn('Messages traités : ', Result.MessagesProcessed);
  WriteLn('Latence moyenne : ', Result.AverageLatency:0:3, ' ms');
  WriteLn('Débit : ', Result.Throughput:0:0, ' msg/sec');
end;

end.
```

### Optimisations

#### 1. Pooling d'objets messages

```pascal
unit MessagePool;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs, Generics.Collections, ActorSystem;

type
  TMessagePool<T: TMessage, constructor> = class
  private
    FPool: TStack<T>;
    FCS: TCriticalSection;
  public
    constructor Create(InitialSize: Integer);
    destructor Destroy; override;

    function Acquire: T;
    procedure Release(Msg: T);
  end;

implementation

constructor TMessagePool<T>.Create(InitialSize: Integer);  
var
  i: Integer;
begin
  inherited Create;
  FPool := TStack<T>.Create;
  FCS := TCriticalSection.Create;

  // Pré-allouer des messages
  for i := 1 to InitialSize do
    FPool.Push(T.Create(nil));
end;

destructor TMessagePool<T>.Destroy;  
var
  Msg: T;
begin
  while FPool.Count > 0 do
  begin
    Msg := FPool.Pop;
    Msg.Free;
  end;

  FPool.Free;
  FCS.Free;
  inherited;
end;

function TMessagePool<T>.Acquire: T;  
begin
  FCS.Enter;
  try
    if FPool.Count > 0 then
      Result := FPool.Pop
    else
      Result := T.Create(nil);
  finally
    FCS.Leave;
  end;
end;

procedure TMessagePool<T>.Release(Msg: T);  
begin
  FCS.Enter;
  try
    FPool.Push(Msg);
  finally
    FCS.Leave;
  end;
end;

end.
```

#### 2. Batching de messages

```pascal
type
  TBatchMessage = class(TMessage)
  private
    FMessages: TList<TMessage>;
  public
    constructor Create(ASender: TActor);
    destructor Destroy; override;

    procedure Add(Msg: TMessage);
    property Messages: TList<TMessage> read FMessages;
  end;

constructor TBatchMessage.Create(ASender: TActor);  
begin
  inherited Create(ASender);
  FMessages := TList<TMessage>.Create;
end;

destructor TBatchMessage.Destroy;  
var
  Msg: TMessage;
begin
  for Msg in FMessages do
    Msg.Free;
  FMessages.Free;
  inherited;
end;

procedure TBatchMessage.Add(Msg: TMessage);  
begin
  FMessages.Add(Msg);
end;

// Acteur qui traite par batch
procedure TBatchActor.Receive(Msg: TMessage);  
var
  BatchMsg: TBatchMessage;
  SubMsg: TMessage;
begin
  if Msg is TBatchMessage then
  begin
    BatchMsg := TBatchMessage(Msg);
    WriteLn(Format('[%s] Traitement d''un batch de %d messages',
      [Name, BatchMsg.Messages.Count]));

    for SubMsg in BatchMsg.Messages do
      ProcessSingleMessage(SubMsg);
  end;
end;
```

## Différences Windows/Ubuntu

Le modèle des acteurs fonctionne de manière identique sur les deux plateformes, mais quelques considérations :

### Threading sous-jacent

```pascal
{$IFDEF WINDOWS}
// Windows : Utilise les threads Win32
// Bon pour les applications desktop
{$ENDIF}

{$IFDEF LINUX}
// Linux : Utilise pthreads
// Excellent pour les serveurs haute performance
{$ENDIF}
```

### Configuration optimale

```pascal
unit PlatformOptimization;

{$mode objfpc}{$H+}

interface

procedure OptimizeActorSystem;

implementation

uses
  {$IFDEF WINDOWS}Windows,{$ENDIF}
  {$IFDEF LINUX}BaseUnix,{$ENDIF}
  Classes, SysUtils;

procedure OptimizeActorSystem;  
begin
  {$IFDEF WINDOWS}
  // Windows : Augmenter la priorité si nécessaire
  SetPriorityClass(GetCurrentProcess, ABOVE_NORMAL_PRIORITY_CLASS);
  {$ENDIF}

  {$IFDEF LINUX}
  // Linux : Ajuster la nice value
  FpNice(-5); // Peut nécessiter des privilèges
  {$ENDIF}

  WriteLn('Système d''acteurs optimisé pour la plateforme');
end;

end.
```

## Debugging et monitoring

### Logger les messages

```pascal
unit ActorLogger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs, ActorSystem;

type
  TLogLevel = (llDebug, llInfo, llWarning, llError);

  TActorLogger = class
  private
    FLogFile: TextFile;
    FCS: TCriticalSection;
  public
    constructor Create(const FileName: string);
    destructor Destroy; override;

    procedure Log(Level: TLogLevel; Actor: TActor; const Msg: string);
    procedure LogMessage(Actor: TActor; Msg: TMessage);
  end;

var
  GlobalLogger: TActorLogger;

implementation

constructor TActorLogger.Create(const FileName: string);  
begin
  inherited Create;
  FCS := TCriticalSection.Create;
  AssignFile(FLogFile, FileName);
  Rewrite(FLogFile);
end;

destructor TActorLogger.Destroy;  
begin
  CloseFile(FLogFile);
  FCS.Free;
  inherited;
end;

procedure TActorLogger.Log(Level: TLogLevel; Actor: TActor; const Msg: string);  
var
  LevelStr: string;
begin
  case Level of
    llDebug:   LevelStr := 'DEBUG';
    llInfo:    LevelStr := 'INFO';
    llWarning: LevelStr := 'WARNING';
    llError:   LevelStr := 'ERROR';
  end;

  FCS.Enter;
  try
    WriteLn(FLogFile, Format('[%s] [%s] [%s] %s',
      [FormatDateTime('yyyy-mm-dd hh:nn:ss', Now),
       LevelStr,
       Actor.Name,
       Msg]));
    Flush(FLogFile);
  finally
    FCS.Leave;
  end;
end;

procedure TActorLogger.LogMessage(Actor: TActor; Msg: TMessage);  
begin
  Log(llDebug, Actor, Format('Message reçu : %s', [Msg.ClassName]));
end;

end.
```

### Monitorer les acteurs

```pascal
type
  TActorMonitor = class(TActor)
  private
    FMonitoredActors: TList<TActor>;
  protected
    procedure Receive(Msg: TMessage); override;
  public
    constructor Create(const AName: string);
    destructor Destroy; override;

    procedure Monitor(Actor: TActor);
    procedure PrintStats;
  end;

procedure TActorMonitor.PrintStats;  
var
  Actor: TActor;
begin
  WriteLn('=== Statistiques des acteurs ===');
  for Actor in FMonitoredActors do
    WriteLn(Format('%s : %d messages en attente',
      [Actor.Name, Actor.Mailbox.Count]));
end;
```

## Bonnes pratiques

### 1. Messages immuables

```pascal
// ❌ MAUVAIS - Message mutable
type
  TMutableMessage = class(TMessage)
  public
    Data: string; // Public et modifiable !
  end;

// ✅ BON - Message immuable
type
  TImmutableMessage = class(TMessage)
  private
    FData: string;
  public
    constructor Create(ASender: TActor; const AData: string);
    property Data: string read FData; // Lecture seule
  end;
```

### 2. Éviter le blocage dans Receive

```pascal
// ❌ MAUVAIS - Blocage dans Receive
procedure TBadActor.Receive(Msg: TMessage);  
begin
  Sleep(5000); // Bloque le traitement !
end;

// ✅ BON - Traitement rapide
procedure TGoodActor.Receive(Msg: TMessage);  
begin
  // Traitement rapide
  ProcessQuickly(Msg);

  // Si besoin de temps, déléguer
  if Msg is TLongTaskMessage then
    CreateWorkerActor().Send(Msg);
end;
```

### 3. Limiter la taille des mailboxes

```pascal
type
  TBoundedMailbox = class(TMailbox)
  private
    FMaxSize: Integer;
  public
    constructor Create(MaxSize: Integer);
    function Post(Msg: TMessage): Boolean; // Retourne False si pleine
  end;
```

### 4. Gérer les erreurs correctement

```pascal
procedure TResilientActor.Receive(Msg: TMessage);  
begin
  try
    ProcessMessage(Msg);
  except
    on E: Exception do
    begin
      // Logger l'erreur
      WriteLn(Format('[%s] Erreur : %s', [Name, E.Message]));

      // Notifier le superviseur si présent
      if Assigned(FSupervisor) then
        FSupervisor.Send(TErrorMessage.Create(Self, E));

      // Continuer à fonctionner
    end;
  end;
end;
```

## Résumé

Le **modèle des acteurs** offre une approche élégante de la programmation concurrente :

**Concepts clés :**
- **Acteurs** : Entités isolées avec état privé
- **Messages** : Communication asynchrone immuable
- **Mailbox** : File d'attente de messages
- **Pas de verrous** : Pas de deadlocks, pas de race conditions

**Patterns essentiels :**
- Request-Reply : Communication synchrone
- Pub-Sub : Un vers plusieurs
- Supervision : Résilience et gestion d'erreurs
- Router : Distribution de charge
- Pipeline : Traitement séquentiel
- Scatter-Gather : Parallélisation et agrégation

**Avantages :**
- ✅ Pas de problèmes de synchronisation
- ✅ Isolation et modularité
- ✅ Scalabilité et distribution
- ✅ Résilience aux pannes

**Bonnes pratiques :**
1. Messages immuables
2. Traitement rapide dans Receive
3. Supervision pour la résilience
4. Logging et monitoring
5. Limiter la taille des mailboxes

Le modèle des acteurs est particulièrement adapté aux systèmes distribués, aux serveurs haute performance et aux applications nécessitant une forte résilience !

⏭️ [Coroutines et fibers](/11-multithreading-concurrence/09-coroutines-fibers.md)
