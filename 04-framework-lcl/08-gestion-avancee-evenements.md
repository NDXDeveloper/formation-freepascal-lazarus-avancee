🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 4.8 Gestion avancée des événements

## Introduction : Comprendre les événements

Les événements sont le cœur battant de toute application graphique. Chaque fois que vous cliquez sur un bouton, tapez du texte ou déplacez la souris, un événement est déclenché. C'est ce qui rend votre application interactive et vivante.

Imaginez les événements comme des sonnettes : quand quelqu'un appuie sur la sonnette (l'utilisateur fait une action), cela déclenche une réaction (votre code s'exécute). Dans Lazarus, vous pouvez installer autant de "sonnettes" que nécessaire et définir exactement ce qui se passe quand elles sont activées.

## Les bases : Rappel sur les événements simples

### Structure d'un événement

Un événement en Pascal est simplement un pointeur vers une méthode :

```pascal
type
  // Type d'événement simple
  TNotifyEvent = procedure(Sender: TObject) of object;

  // Utilisation dans un composant
  TMyButton = class(TButton)
  private
    FOnSpecialClick: TNotifyEvent;
  published
    property OnSpecialClick: TNotifyEvent read FOnSpecialClick write FOnSpecialClick;
  end;

// Méthode qui sera appelée
procedure TForm1.MyButtonSpecialClick(Sender: TObject);
begin
  ShowMessage('Clic spécial détecté !');
end;

// Connexion de l'événement
MyButton.OnSpecialClick := @MyButtonSpecialClick;
```

Le paramètre `Sender` est toujours l'objet qui a déclenché l'événement, ce qui permet d'utiliser la même méthode pour plusieurs composants.

## Créer vos propres types d'événements

### Événements avec paramètres personnalisés

Les événements peuvent transporter des informations supplémentaires :

```pascal
unit CustomEvents;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  // Événement avec des données
  TDataEvent = procedure(Sender: TObject; const Data: string; Value: Integer) of object;

  // Événement avec validation
  TValidationEvent = procedure(Sender: TObject; const Value: string;
                              var IsValid: Boolean; var ErrorMessage: string) of object;

  // Événement avec annulation possible
  TCancelableEvent = procedure(Sender: TObject; var CanContinue: Boolean) of object;

  // Événement avec résultat
  TResultEvent = function(Sender: TObject; Input: Double): Double of object;

  // Événement avec progression
  TProgressEvent = procedure(Sender: TObject; Progress, Total: Integer;
                            const Status: string) of object;

  // Composant exemple utilisant ces événements
  TDataProcessor = class(TComponent)
  private
    FOnDataReceived: TDataEvent;
    FOnValidate: TValidationEvent;
    FOnBeforeProcess: TCancelableEvent;
    FOnCalculate: TResultEvent;
    FOnProgress: TProgressEvent;

    procedure DoDataReceived(const Data: string; Value: Integer);
    function DoValidate(const Value: string): Boolean;
    function DoBeforeProcess: Boolean;
    function DoCalculate(Input: Double): Double;
    procedure DoProgress(Progress, Total: Integer; const Status: string);
  public
    procedure ProcessData(const Data: string);

  published
    property OnDataReceived: TDataEvent read FOnDataReceived write FOnDataReceived;
    property OnValidate: TValidationEvent read FOnValidate write FOnValidate;
    property OnBeforeProcess: TCancelableEvent read FOnBeforeProcess write FOnBeforeProcess;
    property OnCalculate: TResultEvent read FOnCalculate write FOnCalculate;
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
  end;

implementation

procedure TDataProcessor.DoDataReceived(const Data: string; Value: Integer);
begin
  if Assigned(FOnDataReceived) then
    FOnDataReceived(Self, Data, Value);
end;

function TDataProcessor.DoValidate(const Value: string): Boolean;
var
  ErrorMsg: string;
begin
  Result := True;
  ErrorMsg := '';

  if Assigned(FOnValidate) then
  begin
    FOnValidate(Self, Value, Result, ErrorMsg);

    if not Result and (ErrorMsg <> '') then
      ShowMessage('Erreur de validation : ' + ErrorMsg);
  end;
end;

function TDataProcessor.DoBeforeProcess: Boolean;
begin
  Result := True;

  if Assigned(FOnBeforeProcess) then
    FOnBeforeProcess(Self, Result);
end;

function TDataProcessor.DoCalculate(Input: Double): Double;
begin
  if Assigned(FOnCalculate) then
    Result := FOnCalculate(Self, Input)
  else
    Result := Input; // Valeur par défaut
end;

procedure TDataProcessor.DoProgress(Progress, Total: Integer; const Status: string);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Self, Progress, Total, Status);
end;

procedure TDataProcessor.ProcessData(const Data: string);
var
  i: Integer;
  CalculatedValue: Double;
begin
  // Vérifier si on peut continuer
  if not DoBeforeProcess then
  begin
    ShowMessage('Traitement annulé');
    Exit;
  end;

  // Valider les données
  if not DoValidate(Data) then
    Exit;

  // Traiter les données avec notification de progression
  for i := 1 to 100 do
  begin
    DoProgress(i, 100, Format('Traitement : %d%%', [i]));

    // Calcul avec événement
    CalculatedValue := DoCalculate(i * 1.5);

    // Notification de données reçues
    DoDataReceived(Data, Round(CalculatedValue));

    Sleep(10); // Simulation
  end;
end;
```

## Chaînage d'événements

### Intercepter et étendre des événements existants

```pascal
unit EventChaining;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, Forms;

type
  // Composant qui intercepte et étend les événements
  TSmartEdit = class(TEdit)
  private
    FOriginalOnChange: TNotifyEvent;
    FBeforeChange: TNotifyEvent;
    FAfterChange: TNotifyEvent;
    FOnValidInput: TNotifyEvent;
    FOnInvalidInput: TNotifyEvent;
    FOldText: string;
    FIsValidating: Boolean;

    procedure InternalOnChange(Sender: TObject);
    procedure SetOnChange(Value: TNotifyEvent);
    function GetOnChange: TNotifyEvent;
  protected
    procedure DoBeforeChange; virtual;
    procedure DoAfterChange; virtual;
    function ValidateInput: Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Loaded; override;

  published
    property BeforeChange: TNotifyEvent read FBeforeChange write FBeforeChange;
    property AfterChange: TNotifyEvent read FAfterChange write FAfterChange;
    property OnValidInput: TNotifyEvent read FOnValidInput write FOnValidInput;
    property OnInvalidInput: TNotifyEvent read FOnInvalidInput write FOnInvalidInput;
    property OnChange: TNotifyEvent read GetOnChange write SetOnChange;
  end;

implementation

constructor TSmartEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIsValidating := False;
  FOldText := '';
end;

procedure TSmartEdit.Loaded;
begin
  inherited Loaded;

  // Intercepter l'événement OnChange après le chargement
  if Assigned(inherited OnChange) then
  begin
    FOriginalOnChange := inherited OnChange;
    inherited OnChange := @InternalOnChange;
  end;
end;

procedure TSmartEdit.SetOnChange(Value: TNotifyEvent);
begin
  FOriginalOnChange := Value;

  if not (csLoading in ComponentState) then
    inherited OnChange := @InternalOnChange;
end;

function TSmartEdit.GetOnChange: TNotifyEvent;
begin
  Result := FOriginalOnChange;
end;

procedure TSmartEdit.InternalOnChange(Sender: TObject);
begin
  if FIsValidating then Exit;

  FIsValidating := True;
  try
    // Avant le changement
    DoBeforeChange;

    // Valider l'entrée
    if ValidateInput then
    begin
      if Assigned(FOnValidInput) then
        FOnValidInput(Self);

      // Appeler l'événement original
      if Assigned(FOriginalOnChange) then
        FOriginalOnChange(Self);

      FOldText := Text;
    end
    else
    begin
      if Assigned(FOnInvalidInput) then
        FOnInvalidInput(Self);

      // Restaurer l'ancienne valeur
      Text := FOldText;
    end;

    // Après le changement
    DoAfterChange;

  finally
    FIsValidating := False;
  end;
end;

procedure TSmartEdit.DoBeforeChange;
begin
  if Assigned(FBeforeChange) then
    FBeforeChange(Self);
end;

procedure TSmartEdit.DoAfterChange;
begin
  if Assigned(FAfterChange) then
    FAfterChange(Self);
end;

function TSmartEdit.ValidateInput: Boolean;
var
  Dummy: Integer;
begin
  // Validation de base - peut être surchargée
  Result := True;

  // Exemple : accepter uniquement les nombres
  if Tag = 1 then // Tag utilisé comme indicateur de validation numérique
    Result := TryStrToInt(Text, Dummy);
end;
```

## Gestion multi-cast des événements

### Un événement, plusieurs gestionnaires

```pascal
unit MulticastEvents;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs;

type
  // Gestionnaire d'événements multiples
  TMulticastNotifyEvent = class
  private
    FHandlers: TList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(Handler: TNotifyEvent);
    procedure Remove(Handler: TNotifyEvent);
    procedure Clear;
    procedure Execute(Sender: TObject);

    function Count: Integer;
    function Contains(Handler: TNotifyEvent): Boolean;
  end;

  // Composant utilisant des événements multicast
  TBroadcaster = class(TComponent)
  private
    FOnBroadcast: TMulticastNotifyEvent;
    FMessage: string;
  protected
    procedure DoBroadcast;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Broadcast(const Msg: string);
    procedure Subscribe(Handler: TNotifyEvent);
    procedure Unsubscribe(Handler: TNotifyEvent);

    property Message: string read FMessage;
    property OnBroadcast: TMulticastNotifyEvent read FOnBroadcast;
  end;

implementation

{ TMulticastNotifyEvent }

constructor TMulticastNotifyEvent.Create;
begin
  FHandlers := TList.Create;
end;

destructor TMulticastNotifyEvent.Destroy;
begin
  FHandlers.Free;
  inherited;
end;

procedure TMulticastNotifyEvent.Add(Handler: TNotifyEvent);
var
  Method: ^TMethod;
begin
  if not Contains(Handler) then
  begin
    New(Method);
    Method^ := TMethod(Handler);
    FHandlers.Add(Method);
  end;
end;

procedure TMulticastNotifyEvent.Remove(Handler: TNotifyEvent);
var
  i: Integer;
  Method: ^TMethod;
begin
  for i := FHandlers.Count - 1 downto 0 do
  begin
    Method := FHandlers[i];
    if (Method^.Code = TMethod(Handler).Code) and
       (Method^.Data = TMethod(Handler).Data) then
    begin
      Dispose(Method);
      FHandlers.Delete(i);
      Break;
    end;
  end;
end;

procedure TMulticastNotifyEvent.Clear;
var
  i: Integer;
begin
  for i := 0 to FHandlers.Count - 1 do
    Dispose(PMethod(FHandlers[i]));
  FHandlers.Clear;
end;

procedure TMulticastNotifyEvent.Execute(Sender: TObject);
var
  i: Integer;
  Handler: TNotifyEvent;
begin
  for i := 0 to FHandlers.Count - 1 do
  begin
    TMethod(Handler) := PMethod(FHandlers[i])^;
    if Assigned(Handler) then
      Handler(Sender);
  end;
end;

function TMulticastNotifyEvent.Count: Integer;
begin
  Result := FHandlers.Count;
end;

function TMulticastNotifyEvent.Contains(Handler: TNotifyEvent): Boolean;
var
  i: Integer;
  Method: ^TMethod;
begin
  Result := False;
  for i := 0 to FHandlers.Count - 1 do
  begin
    Method := FHandlers[i];
    if (Method^.Code = TMethod(Handler).Code) and
       (Method^.Data = TMethod(Handler).Data) then
    begin
      Result := True;
      Break;
    end;
  end;
end;

{ TBroadcaster }

constructor TBroadcaster.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOnBroadcast := TMulticastNotifyEvent.Create;
end;

destructor TBroadcaster.Destroy;
begin
  FOnBroadcast.Free;
  inherited;
end;

procedure TBroadcaster.Broadcast(const Msg: string);
begin
  FMessage := Msg;
  DoBroadcast;
end;

procedure TBroadcaster.DoBroadcast;
begin
  FOnBroadcast.Execute(Self);
end;

procedure TBroadcaster.Subscribe(Handler: TNotifyEvent);
begin
  FOnBroadcast.Add(Handler);
end;

procedure TBroadcaster.Unsubscribe(Handler: TNotifyEvent);
begin
  FOnBroadcast.Remove(Handler);
end;
```

## Événements asynchrones

### Événements différés et files d'événements

```pascal
unit AsyncEvents;

{$mode objfpc}{$H+}
{$modeswitch anonymousfunctions}

interface

uses
  Classes, SysUtils, SyncObjs, Contnrs, Forms;

type
  // Message pour événement asynchrone
  TEventMessage = class
  public
    Sender: TObject;
    EventType: string;
    Data: Variant;
    TimeStamp: TDateTime;
  end;

  // Gestionnaire d'événements asynchrones
  TAsyncEventManager = class(TThread)
  private
    FEventQueue: TQueue;
    FQueueLock: TCriticalSection;
    FNewEventSignal: TEvent;
    FOnEventProcessed: TNotifyEvent;

    procedure ProcessEvent(Event: TEventMessage);
    procedure NotifyEventProcessed;
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure PostEvent(Sender: TObject; const EventType: string; const Data: Variant);
    procedure PostDelayedEvent(Sender: TObject; const EventType: string;
                              const Data: Variant; DelayMS: Integer);

    property OnEventProcessed: TNotifyEvent read FOnEventProcessed write FOnEventProcessed;
  end;

  // Timer pour événements différés
  TDelayedEventTimer = class(TComponent)
  private
    FTimers: TList;
    FOnTimerExpired: TNotifyEvent;

    procedure CheckTimers;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure AddDelayedEvent(Proc: TNotifyEvent; DelayMS: Integer);
    procedure CancelAll;

    property OnTimerExpired: TNotifyEvent read FOnTimerExpired write FOnTimerExpired;
  end;

implementation

uses
  DateUtils;

{ TAsyncEventManager }

constructor TAsyncEventManager.Create;
begin
  inherited Create(True); // Créé suspendu
  FEventQueue := TQueue.Create;
  FQueueLock := TCriticalSection.Create;
  FNewEventSignal := TEvent.Create(nil, False, False, '');
  FreeOnTerminate := False;
end;

destructor TAsyncEventManager.Destroy;
var
  Event: TEventMessage;
begin
  Terminate;
  FNewEventSignal.SetEvent; // Réveiller le thread
  WaitFor;

  // Nettoyer la file
  FQueueLock.Enter;
  try
    while FEventQueue.Count > 0 do
    begin
      Event := TEventMessage(FEventQueue.Pop);
      Event.Free;
    end;
  finally
    FQueueLock.Leave;
  end;

  FEventQueue.Free;
  FQueueLock.Free;
  FNewEventSignal.Free;

  inherited;
end;

procedure TAsyncEventManager.Execute;
var
  Event: TEventMessage;
begin
  while not Terminated do
  begin
    // Attendre un nouvel événement
    if FNewEventSignal.WaitFor(100) = wrSignaled then
    begin
      FQueueLock.Enter;
      try
        while FEventQueue.Count > 0 do
        begin
          Event := TEventMessage(FEventQueue.Pop);
          try
            ProcessEvent(Event);
          finally
            Event.Free;
          end;
        end;
      finally
        FQueueLock.Leave;
      end;

      Synchronize(@NotifyEventProcessed);
    end;
  end;
end;

procedure TAsyncEventManager.PostEvent(Sender: TObject; const EventType: string;
                                       const Data: Variant);
var
  Event: TEventMessage;
begin
  Event := TEventMessage.Create;
  Event.Sender := Sender;
  Event.EventType := EventType;
  Event.Data := Data;
  Event.TimeStamp := Now;

  FQueueLock.Enter;
  try
    FEventQueue.Push(Event);
  finally
    FQueueLock.Leave;
  end;

  FNewEventSignal.SetEvent; // Signaler un nouvel événement
end;

procedure TAsyncEventManager.PostDelayedEvent(Sender: TObject; const EventType: string;
                                              const Data: Variant; DelayMS: Integer);
begin
  // Utiliser un timer ou Sleep dans un thread séparé
  TThread.CreateAnonymousThread(procedure
  begin
    Sleep(DelayMS);
    PostEvent(Sender, EventType, Data);
  end).Start;
end;

procedure TAsyncEventManager.ProcessEvent(Event: TEventMessage);
begin
  // Traiter l'événement selon son type
  // Note : case...of ne supporte que les types ordinaux en FPC,
  // on utilise donc if...else if pour les chaînes
  if Event.EventType = 'DATA_UPDATE' then
  begin
    // Traitement des données
  end
  else if Event.EventType = 'CALCULATION' then
  begin
    // Calcul asynchrone
  end
  else if Event.EventType = 'NOTIFICATION' then
  begin
    // Notification
  end;
end;

procedure TAsyncEventManager.NotifyEventProcessed;
begin
  if Assigned(FOnEventProcessed) then
    FOnEventProcessed(Self);
end;

{ TDelayedEventTimer }

type
  TDelayedEvent = record
    Proc: TNotifyEvent;
    ExecuteTime: TDateTime;
  end;
  PDelayedEvent = ^TDelayedEvent;

constructor TDelayedEventTimer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTimers := TList.Create;

  // Timer pour vérifier les événements
  TThread.CreateAnonymousThread(procedure
  begin
    while not Application.Terminated do
    begin
      CheckTimers;
      Sleep(100);
    end;
  end).Start;
end;

destructor TDelayedEventTimer.Destroy;
begin
  CancelAll;
  FTimers.Free;
  inherited;
end;

procedure TDelayedEventTimer.AddDelayedEvent(Proc: TNotifyEvent; DelayMS: Integer);
var
  Event: PDelayedEvent;
begin
  New(Event);
  Event^.Proc := Proc;
  Event^.ExecuteTime := IncMilliSecond(Now, DelayMS);
  FTimers.Add(Event);
end;

procedure TDelayedEventTimer.CheckTimers;
var
  i: Integer;
  Event: PDelayedEvent;
begin
  for i := FTimers.Count - 1 downto 0 do
  begin
    Event := FTimers[i];
    if Now >= Event^.ExecuteTime then
    begin
      if Assigned(Event^.Proc) then
        Event^.Proc(Self);

      Dispose(Event);
      FTimers.Delete(i);

      if Assigned(FOnTimerExpired) then
        FOnTimerExpired(Self);
    end;
  end;
end;

procedure TDelayedEventTimer.CancelAll;
var
  i: Integer;
begin
  for i := 0 to FTimers.Count - 1 do
    Dispose(PDelayedEvent(FTimers[i]));
  FTimers.Clear;
end;
```

## Bubbling et capture d'événements

### Propagation d'événements dans une hiérarchie

```pascal
unit EventBubbling;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Forms;

type
  TEventPhase = (epCapture, epTarget, epBubble);

  // Événement qui peut se propager
  TBubbleEvent = class
  public
    EventType: string;
    Target: TControl;
    CurrentTarget: TControl;
    Phase: TEventPhase;
    Canceled: Boolean;
    Data: Variant;

    procedure StopPropagation;
    procedure PreventDefault;
  end;

  // Interface pour les composants qui gèrent le bubbling
  IBubbleEventHandler = interface
    ['{D4E5F6A7-B8C9-0123-DEFF-456789ABCDEF}']
    function HandleBubbleEvent(Event: TBubbleEvent): Boolean;
  end;

  // Conteneur avec support du bubbling
  TBubblePanel = class(TPanel, IBubbleEventHandler)
  private
    FOnBubbleEvent: TNotifyEvent;
    FParentHandler: IBubbleEventHandler;

    procedure FindParentHandler;
  protected
    procedure SetParent(NewParent: TWinControl); override;
    function DispatchEvent(Event: TBubbleEvent): Boolean;

    // Phase de capture (descendante)
    procedure CaptureEvent(Event: TBubbleEvent); virtual;
    // Phase de bubbling (remontante)
    procedure BubbleEvent(Event: TBubbleEvent); virtual;
  public
    function HandleBubbleEvent(Event: TBubbleEvent): Boolean;
    procedure TriggerEvent(const EventType: string; const Data: Variant);

    property OnBubbleEvent: TNotifyEvent read FOnBubbleEvent write FOnBubbleEvent;
  end;

implementation

{ TBubbleEvent }

procedure TBubbleEvent.StopPropagation;
begin
  Canceled := True;
end;

procedure TBubbleEvent.PreventDefault;
begin
  // Empêcher l'action par défaut
  Canceled := True;
end;

{ TBubblePanel }

procedure TBubblePanel.SetParent(NewParent: TWinControl);
begin
  inherited SetParent(NewParent);
  FindParentHandler;
end;

procedure TBubblePanel.FindParentHandler;
var
  Control: TWinControl;
begin
  FParentHandler := nil;
  Control := Parent;

  while Assigned(Control) do
  begin
    if Supports(Control, IBubbleEventHandler, FParentHandler) then
      Break;
    Control := Control.Parent;
  end;
end;

function TBubblePanel.HandleBubbleEvent(Event: TBubbleEvent): Boolean;
begin
  Result := not Event.Canceled;

  if not Result then Exit;

  Event.CurrentTarget := Self;

  case Event.Phase of
    epCapture:
      begin
        CaptureEvent(Event);

        // Propager vers le bas
        if not Event.Canceled then
          Result := DispatchEvent(Event);
      end;

    epBubble:
      begin
        BubbleEvent(Event);

        // Propager vers le haut
        if not Event.Canceled and Assigned(FParentHandler) then
          Result := FParentHandler.HandleBubbleEvent(Event);
      end;

    epTarget:
      begin
        // Traiter sur la cible
        if Assigned(FOnBubbleEvent) then
          FOnBubbleEvent(Self);
      end;
  end;
end;

function TBubblePanel.DispatchEvent(Event: TBubbleEvent): Boolean;
var
  i: Integer;
  Child: TControl;
  ChildHandler: IBubbleEventHandler;
begin
  Result := True;

  // Phase de capture - descendre dans l'arbre
  for i := 0 to ControlCount - 1 do
  begin
    Child := Controls[i];
    if Supports(Child, IBubbleEventHandler, ChildHandler) then
    begin
      Event.Phase := epCapture;
      Result := ChildHandler.HandleBubbleEvent(Event);

      if not Result then Break;
    end;
  end;

  // Phase cible
  if Result then
  begin
    Event.Phase := epTarget;
    HandleBubbleEvent(Event);
  end;

  // Phase de bubbling - remonter dans l'arbre
  if Result and not Event.Canceled then
  begin
    Event.Phase := epBubble;

    for i := ControlCount - 1 downto 0 do
    begin
      Child := Controls[i];
      if Supports(Child, IBubbleEventHandler, ChildHandler) then
      begin
        Result := ChildHandler.HandleBubbleEvent(Event);
        if not Result then Break;
      end;
    end;
  end;
end;

procedure TBubblePanel.CaptureEvent(Event: TBubbleEvent);
begin
  // Traitement en phase de capture
  // Override dans les classes dérivées
end;

procedure TBubblePanel.BubbleEvent(Event: TBubbleEvent);
begin
  // Traitement en phase de bubbling
  // Override dans les classes dérivées

  if Assigned(FOnBubbleEvent) then
    FOnBubbleEvent(Self);
end;

procedure TBubblePanel.TriggerEvent(const EventType: string; const Data: Variant);
var
  Event: TBubbleEvent;
begin
  Event := TBubbleEvent.Create;
  try
    Event.EventType := EventType;
    Event.Target := Self;
    Event.CurrentTarget := Self;
    Event.Phase := epCapture;
    Event.Canceled := False;
    Event.Data := Data;

    // Démarrer la propagation
    HandleBubbleEvent(Event);
  finally
    Event.Free;
  end;
end;
```

## Filtrage et interception d'événements

### Hook d'événements au niveau application

```pascal
unit EventFiltering;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, LMessages;

type
  // Filtre d'événements global
  TEventFilter = class
  private
    FActive: Boolean;
    FBlockedEvents: TStringList;
    FLogEvents: Boolean;
    FEventLog: TStringList;
    FOldAppMessageHandler: TMessageEvent;

    procedure AppMessageHandler(var Msg: TMsg; var Handled: Boolean);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Activate;
    procedure Deactivate;

    procedure BlockEvent(const EventName: string);
    procedure UnblockEvent(const EventName: string);
    procedure BlockAllMouseEvents;
    procedure BlockAllKeyEvents;

    procedure StartLogging;
    procedure StopLogging;
    procedure SaveLog(const FileName: string);

    property Active: Boolean read FActive;
    property EventLog: TStringList read FEventLog;
  end;

  // Intercepteur d'événements pour un contrôle spécifique
  TControlEventInterceptor = class(TComponent)
  private
    FControl: TControl;
    FOriginalWndProc: TWndMethod;
    FOnBeforeEvent: TNotifyEvent;
    FOnAfterEvent: TNotifyEvent;
    FBlockedMessages: array of Cardinal;

    procedure NewWndProc(var Message: TMessage);
    procedure SetControl(Value: TControl);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function IsMessageBlocked(MsgId: Cardinal): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure BlockMessage(MsgId: Cardinal);
    procedure UnblockMessage(MsgId: Cardinal);
    procedure BlockMouseMessages;
    procedure BlockKeyMessages;

  published
    property Control: TControl read FControl write SetControl;
    property OnBeforeEvent: TNotifyEvent read FOnBeforeEvent write FOnBeforeEvent;
    property OnAfterEvent: TNotifyEvent read FOnAfterEvent write FOnAfterEvent;
  end;

implementation

uses
  LCLType, LCLIntf;

{ TEventFilter }

constructor TEventFilter.Create;
begin
  FActive := False;
  FBlockedEvents := TStringList.Create;
  FEventLog := TStringList.Create;
  FLogEvents := False;
end;

destructor TEventFilter.Destroy;
begin
  if FActive then
    Deactivate;

  FBlockedEvents.Free;
  FEventLog.Free;
  inherited;
end;

procedure TEventFilter.Activate;
begin
  if not FActive then
  begin
    FOldAppMessageHandler := Application.OnMessage;
    Application.OnMessage := @AppMessageHandler;
    FActive := True;
  end;
end;

procedure TEventFilter.Deactivate;
begin
  if FActive then
  begin
    Application.OnMessage := FOldAppMessageHandler;
    FActive := False;
  end;
end;

procedure TEventFilter.AppMessageHandler(var Msg: TMsg; var Handled: Boolean);
var
  EventName: string;
begin
  // Identifier le type de message
  case Msg.message of
    LM_LBUTTONDOWN: EventName := 'LBUTTONDOWN';
    LM_LBUTTONUP: EventName := 'LBUTTONUP';
    LM_RBUTTONDOWN: EventName := 'RBUTTONDOWN';
    LM_RBUTTONUP: EventName := 'RBUTTONUP';
    LM_MOUSEMOVE: EventName := 'MOUSEMOVE';
    LM_KEYDOWN: EventName := 'KEYDOWN';
    LM_KEYUP: EventName := 'KEYUP';
    LM_CHAR: EventName := 'CHAR';
  else
    EventName := 'MSG_' + IntToStr(Msg.message);
  end;

  // Logger si activé
  if FLogEvents then
  begin
    FEventLog.Add(Format('[%s] %s - HWND: %d, wParam: %d, lParam: %d',
      [TimeToStr(Now), EventName, Msg.hwnd, Msg.wParam, Msg.lParam]));
  end;

  // Bloquer si nécessaire
  if FBlockedEvents.IndexOf(EventName) >= 0 then
  begin
    Handled := True;
    Exit;
  end;

  // Appeler l'ancien handler
  if Assigned(FOldAppMessageHandler) then
    FOldAppMessageHandler(Msg, Handled);
end;

procedure TEventFilter.BlockEvent(const EventName: string);
begin
  if FBlockedEvents.IndexOf(EventName) < 0 then
    FBlockedEvents.Add(EventName);
end;

procedure TEventFilter.UnblockEvent(const EventName: string);
var
  Index: Integer;
begin
  Index := FBlockedEvents.IndexOf(EventName);
  if Index >= 0 then
    FBlockedEvents.Delete(Index);
end;

procedure TEventFilter.BlockAllMouseEvents;
begin
  BlockEvent('LBUTTONDOWN');
  BlockEvent('LBUTTONUP');
  BlockEvent('RBUTTONDOWN');
  BlockEvent('RBUTTONUP');
  BlockEvent('MBUTTONDOWN');
  BlockEvent('MBUTTONUP');
  BlockEvent('MOUSEMOVE');
  BlockEvent('MOUSEWHEEL');
end;

procedure TEventFilter.BlockAllKeyEvents;
begin
  BlockEvent('KEYDOWN');
  BlockEvent('KEYUP');
  BlockEvent('CHAR');
  BlockEvent('SYSKEYDOWN');
  BlockEvent('SYSKEYUP');
end;

procedure TEventFilter.StartLogging;
begin
  FLogEvents := True;
  FEventLog.Add('=== Début du log : ' + DateTimeToStr(Now) + ' ===');
end;

procedure TEventFilter.StopLogging;
begin
  FLogEvents := False;
  FEventLog.Add('=== Fin du log : ' + DateTimeToStr(Now) + ' ===');
end;

procedure TEventFilter.SaveLog(const FileName: string);
begin
  FEventLog.SaveToFile(FileName);
end;

{ TControlEventInterceptor }

constructor TControlEventInterceptor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetLength(FBlockedMessages, 0);
end;

destructor TControlEventInterceptor.Destroy;
begin
  if Assigned(FControl) then
    SetControl(nil);
  inherited;
end;

procedure TControlEventInterceptor.SetControl(Value: TControl);
begin
  if FControl <> Value then
  begin
    // Restaurer l'ancien contrôle
    if Assigned(FControl) then
    begin
      FControl.WindowProc := FOriginalWndProc;
      FControl.RemoveFreeNotification(Self);
    end;

    FControl := Value;

    // Intercepter le nouveau contrôle
    if Assigned(FControl) then
    begin
      FControl.FreeNotification(Self);
      FOriginalWndProc := FControl.WindowProc;
      FControl.WindowProc := @NewWndProc;
    end;
  end;
end;

procedure TControlEventInterceptor.NewWndProc(var Message: TMessage);
begin
  // Avant l'événement
  if Assigned(FOnBeforeEvent) then
    FOnBeforeEvent(Self);

  // Bloquer si nécessaire
  if not IsMessageBlocked(Message.Msg) then
  begin
    // Appeler la procédure originale
    if Assigned(FOriginalWndProc) then
      FOriginalWndProc(Message);
  end
  else
  begin
    // Message bloqué - ne rien faire
    Message.Result := 0;
  end;

  // Après l'événement
  if Assigned(FOnAfterEvent) then
    FOnAfterEvent(Self);
end;

procedure TControlEventInterceptor.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;

  if (Operation = opRemove) and (AComponent = FControl) then
    FControl := nil;
end;

function TControlEventInterceptor.IsMessageBlocked(MsgId: Cardinal): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to High(FBlockedMessages) do
  begin
    if FBlockedMessages[i] = MsgId then
    begin
      Result := True;
      Break;
    end;
  end;
end;

procedure TControlEventInterceptor.BlockMessage(MsgId: Cardinal);
begin
  if not IsMessageBlocked(MsgId) then
  begin
    SetLength(FBlockedMessages, Length(FBlockedMessages) + 1);
    FBlockedMessages[High(FBlockedMessages)] := MsgId;
  end;
end;

procedure TControlEventInterceptor.UnblockMessage(MsgId: Cardinal);
var
  i, j: Integer;
begin
  for i := 0 to High(FBlockedMessages) do
  begin
    if FBlockedMessages[i] = MsgId then
    begin
      // Décaler les éléments
      for j := i to High(FBlockedMessages) - 1 do
        FBlockedMessages[j] := FBlockedMessages[j + 1];

      SetLength(FBlockedMessages, Length(FBlockedMessages) - 1);
      Break;
    end;
  end;
end;

procedure TControlEventInterceptor.BlockMouseMessages;
begin
  BlockMessage(LM_LBUTTONDOWN);
  BlockMessage(LM_LBUTTONUP);
  BlockMessage(LM_RBUTTONDOWN);
  BlockMessage(LM_RBUTTONUP);
  BlockMessage(LM_MBUTTONDOWN);
  BlockMessage(LM_MBUTTONUP);
  BlockMessage(LM_MOUSEMOVE);
  BlockMessage(LM_MOUSEWHEEL);
end;

procedure TControlEventInterceptor.BlockKeyMessages;
begin
  BlockMessage(LM_KEYDOWN);
  BlockMessage(LM_KEYUP);
  BlockMessage(LM_CHAR);
  BlockMessage(LM_SYSKEYDOWN);
  BlockMessage(LM_SYSKEYUP);
end;
```

## Pattern Observer avec événements

### Implémentation du pattern Observer

```pascal
unit ObserverPattern;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs;

type
  // Interface pour les observateurs
  IObserver = interface
    ['{E5F6A7B8-C9D0-1234-EFAB-6789CDEF0123}']
    procedure Update(const Subject: TObject; const EventData: Variant);
  end;

  // Interface pour les sujets observables
  ISubject = interface
    ['{F6A7B8C9-D0E1-2345-FABC-789DEF012345}']
    procedure Attach(Observer: IObserver);
    procedure Detach(Observer: IObserver);
    procedure Notify(const EventData: Variant);
  end;

  // Sujet observable de base
  TObservableSubject = class(TComponent, ISubject)
  private
    FObservers: TInterfaceList;
    FUpdateCount: Integer;
    FChanged: Boolean;
  protected
    procedure NotifyObservers(const EventData: Variant);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // ISubject
    procedure Attach(Observer: IObserver);
    procedure Detach(Observer: IObserver);
    procedure Notify(const EventData: Variant);

    // Mise à jour par lot
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Changed;

    property UpdateCount: Integer read FUpdateCount;
  end;

  // Observateur de base
  TObserver = class(TComponent, IObserver)
  private
    FOnUpdate: TNotifyEvent;
  protected
    procedure DoUpdate(const Subject: TObject; const EventData: Variant); virtual;
  public
    procedure Update(const Subject: TObject; const EventData: Variant);

  published
    property OnUpdate: TNotifyEvent read FOnUpdate write FOnUpdate;
  end;

  // Exemple concret : Modèle de données observable
  TDataModel = class(TObservableSubject)
  private
    FData: TStringList;
    FSelectedIndex: Integer;

    procedure SetSelectedIndex(Value: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure AddItem(const Item: string);
    procedure RemoveItem(Index: Integer);
    procedure UpdateItem(Index: Integer; const NewValue: string);
    procedure Clear;

    property Data: TStringList read FData;
    property SelectedIndex: Integer read FSelectedIndex write SetSelectedIndex;
  end;

  // Vue qui observe le modèle
  TDataView = class(TObserver)
  private
    FModel: TDataModel;
    FAutoRefresh: Boolean;
  protected
    procedure DoUpdate(const Subject: TObject; const EventData: Variant); override;
    procedure RefreshView; virtual;
  public
    procedure SetModel(AModel: TDataModel);

    property Model: TDataModel read FModel;
    property AutoRefresh: Boolean read FAutoRefresh write FAutoRefresh default True;
  end;

implementation

{ TObservableSubject }

constructor TObservableSubject.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FObservers := TInterfaceList.Create;
  FUpdateCount := 0;
  FChanged := False;
end;

destructor TObservableSubject.Destroy;
begin
  FObservers.Free;
  inherited;
end;

procedure TObservableSubject.Attach(Observer: IObserver);
begin
  if FObservers.IndexOf(Observer) < 0 then
    FObservers.Add(Observer);
end;

procedure TObservableSubject.Detach(Observer: IObserver);
begin
  FObservers.Remove(Observer);
end;

procedure TObservableSubject.Notify(const EventData: Variant);
begin
  NotifyObservers(EventData);
end;

procedure TObservableSubject.NotifyObservers(const EventData: Variant);
var
  i: Integer;
begin
  if FUpdateCount > 0 then
  begin
    FChanged := True;
    Exit;
  end;

  for i := 0 to FObservers.Count - 1 do
    IObserver(FObservers[i]).Update(Self, EventData);

  FChanged := False;
end;

procedure TObservableSubject.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TObservableSubject.EndUpdate;
begin
  Dec(FUpdateCount);
  if (FUpdateCount = 0) and FChanged then
    NotifyObservers('BatchUpdate');
end;

procedure TObservableSubject.Changed;
begin
  FChanged := True;
  if FUpdateCount = 0 then
    NotifyObservers('Changed');
end;

{ TObserver }

procedure TObserver.Update(const Subject: TObject; const EventData: Variant);
begin
  DoUpdate(Subject, EventData);

  if Assigned(FOnUpdate) then
    FOnUpdate(Self);
end;

procedure TObserver.DoUpdate(const Subject: TObject; const EventData: Variant);
begin
  // Override dans les classes dérivées
end;

{ TDataModel }

constructor TDataModel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FData := TStringList.Create;
  FSelectedIndex := -1;
end;

destructor TDataModel.Destroy;
begin
  FData.Free;
  inherited;
end;

procedure TDataModel.SetSelectedIndex(Value: Integer);
begin
  if (Value >= -1) and (Value < FData.Count) and (FSelectedIndex <> Value) then
  begin
    FSelectedIndex := Value;
    NotifyObservers('SelectionChanged');
  end;
end;

procedure TDataModel.AddItem(const Item: string);
begin
  FData.Add(Item);
  NotifyObservers('ItemAdded');
end;

procedure TDataModel.RemoveItem(Index: Integer);
begin
  if (Index >= 0) and (Index < FData.Count) then
  begin
    FData.Delete(Index);
    if FSelectedIndex >= FData.Count then
      FSelectedIndex := FData.Count - 1;
    NotifyObservers('ItemRemoved');
  end;
end;

procedure TDataModel.UpdateItem(Index: Integer; const NewValue: string);
begin
  if (Index >= 0) and (Index < FData.Count) then
  begin
    FData[Index] := NewValue;
    NotifyObservers('ItemUpdated');
  end;
end;

procedure TDataModel.Clear;
begin
  FData.Clear;
  FSelectedIndex := -1;
  NotifyObservers('Cleared');
end;

{ TDataView }

procedure TDataView.SetModel(AModel: TDataModel);
begin
  if FModel <> AModel then
  begin
    if Assigned(FModel) then
      FModel.Detach(Self);

    FModel := AModel;

    if Assigned(FModel) then
    begin
      FModel.Attach(Self);
      RefreshView;
    end;
  end;
end;

procedure TDataView.DoUpdate(const Subject: TObject; const EventData: Variant);
begin
  if FAutoRefresh and (Subject = FModel) then
    RefreshView;
end;

procedure TDataView.RefreshView;
begin
  // Override dans les classes dérivées pour mettre à jour l'affichage
end;
```

## Gestion des événements tactiles et gestuelles

### Support des événements touch et multi-touch

```pascal
unit TouchEvents;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Forms, Graphics;

type
  TTouchPhase = (tpBegan, tpMoved, tpStationary, tpEnded, tpCancelled);

  // Information sur un point de contact
  TTouchPoint = record
    ID: Integer;
    Position: TPoint;
    LastPosition: TPoint;
    StartPosition: TPoint;
    Phase: TTouchPhase;
    Pressure: Single;
    Timestamp: TDateTime;
  end;

  // Événement tactile
  TTouchEvent = procedure(Sender: TObject; const TouchPoint: TTouchPoint) of object;

  // Événement multi-touch
  TMultiTouchEvent = procedure(Sender: TObject;
    const TouchPoints: array of TTouchPoint) of object;

  // Type de geste
  TGestureType = (gtTap, gtDoubleTap, gtLongPress, gtSwipe, gtPinch, gtRotate);

  // Information sur un geste
  TGestureInfo = record
    GestureType: TGestureType;
    StartPoint: TPoint;
    EndPoint: TPoint;
    Direction: Single;  // Angle en radians
    Distance: Single;   // Distance parcourue
    Scale: Single;      // Pour pinch (zoom)
    Rotation: Single;   // Pour rotation
    Velocity: Single;   // Vitesse du geste
    Duration: Integer;  // Durée en millisecondes
  end;

  // Événement de geste
  TGestureEvent = procedure(Sender: TObject; const Gesture: TGestureInfo) of object;

  // Composant avec support tactile
  TTouchControl = class(TCustomControl)
  private
    FTouchPoints: array of TTouchPoint;
    FGestureRecognizer: TObject;
    FOnTouch: TTouchEvent;
    FOnMultiTouch: TMultiTouchEvent;
    FOnGesture: TGestureEvent;

    procedure ProcessTouchBegin(const Point: TPoint; ID: Integer);
    procedure ProcessTouchMove(const Point: TPoint; ID: Integer);
    procedure ProcessTouchEnd(ID: Integer);
    procedure RecognizeGesture;
    function FindTouchPoint(ID: Integer): Integer;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function TouchPointCount: Integer;
    function GetTouchPoint(Index: Integer): TTouchPoint;

  published
    property OnTouch: TTouchEvent read FOnTouch write FOnTouch;
    property OnMultiTouch: TMultiTouchEvent read FOnMultiTouch write FOnMultiTouch;
    property OnGesture: TGestureEvent read FOnGesture write FOnGesture;
  end;

  // Reconnaisseur de gestes
  TGestureRecognizer = class
  private
    FTouchPoints: array of TTouchPoint;
    FGestureThreshold: Integer;
    FLongPressDelay: Integer;
    FDoubleTapDelay: Integer;
    FLastTapTime: TDateTime;
    FLastTapPoint: TPoint;

    function CalculateDistance(P1, P2: TPoint): Single;
    function CalculateAngle(P1, P2: TPoint): Single;
    function RecognizeSwipe: TGestureInfo;
    function RecognizePinch: TGestureInfo;
    function RecognizeRotation: TGestureInfo;
  public
    constructor Create;

    function RecognizeGesture(const TouchPoints: array of TTouchPoint;
      out Gesture: TGestureInfo): Boolean;

    property GestureThreshold: Integer read FGestureThreshold write FGestureThreshold;
    property LongPressDelay: Integer read FLongPressDelay write FLongPressDelay;
    property DoubleTapDelay: Integer read FDoubleTapDelay write FDoubleTapDelay;
  end;

implementation

uses
  Math, DateUtils;

{ TTouchControl }

constructor TTouchControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetLength(FTouchPoints, 0);
  FGestureRecognizer := TGestureRecognizer.Create;
  DoubleBuffered := True;
end;

destructor TTouchControl.Destroy;
begin
  FGestureRecognizer.Free;
  inherited;
end;

procedure TTouchControl.ProcessTouchBegin(const Point: TPoint; ID: Integer);
var
  Index: Integer;
  TouchPoint: TTouchPoint;
begin
  Index := FindTouchPoint(ID);

  if Index < 0 then
  begin
    // Nouveau point de contact
    SetLength(FTouchPoints, Length(FTouchPoints) + 1);
    Index := High(FTouchPoints);
  end;

  TouchPoint.ID := ID;
  TouchPoint.Position := Point;
  TouchPoint.LastPosition := Point;
  TouchPoint.StartPosition := Point;
  TouchPoint.Phase := tpBegan;
  TouchPoint.Pressure := 1.0;
  TouchPoint.Timestamp := Now;

  FTouchPoints[Index] := TouchPoint;

  if Assigned(FOnTouch) then
    FOnTouch(Self, TouchPoint);

  if Assigned(FOnMultiTouch) and (Length(FTouchPoints) > 1) then
    FOnMultiTouch(Self, FTouchPoints);

  Invalidate;
end;

procedure TTouchControl.ProcessTouchMove(const Point: TPoint; ID: Integer);
var
  Index: Integer;
begin
  Index := FindTouchPoint(ID);

  if Index >= 0 then
  begin
    FTouchPoints[Index].LastPosition := FTouchPoints[Index].Position;
    FTouchPoints[Index].Position := Point;
    FTouchPoints[Index].Phase := tpMoved;

    if Assigned(FOnTouch) then
      FOnTouch(Self, FTouchPoints[Index]);

    RecognizeGesture;
    Invalidate;
  end;
end;

procedure TTouchControl.ProcessTouchEnd(ID: Integer);
var
  Index, i: Integer;
begin
  Index := FindTouchPoint(ID);

  if Index >= 0 then
  begin
    FTouchPoints[Index].Phase := tpEnded;

    if Assigned(FOnTouch) then
      FOnTouch(Self, FTouchPoints[Index]);

    // Retirer le point de contact
    for i := Index to High(FTouchPoints) - 1 do
      FTouchPoints[i] := FTouchPoints[i + 1];

    SetLength(FTouchPoints, Length(FTouchPoints) - 1);

    RecognizeGesture;
    Invalidate;
  end;
end;

procedure TTouchControl.RecognizeGesture;
var
  Gesture: TGestureInfo;
begin
  if TGestureRecognizer(FGestureRecognizer).RecognizeGesture(FTouchPoints, Gesture) then
  begin
    if Assigned(FOnGesture) then
      FOnGesture(Self, Gesture);
  end;
end;

function TTouchControl.FindTouchPoint(ID: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(FTouchPoints) do
  begin
    if FTouchPoints[i].ID = ID then
    begin
      Result := i;
      Break;
    end;
  end;
end;

procedure TTouchControl.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;

  // Simuler un touch avec la souris
  if Button = mbLeft then
    ProcessTouchBegin(Point(X, Y), 0);
end;

procedure TTouchControl.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  // Simuler un mouvement touch avec la souris
  if ssLeft in Shift then
    ProcessTouchMove(Point(X, Y), 0);
end;

procedure TTouchControl.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;

  // Simuler la fin du touch avec la souris
  if Button = mbLeft then
    ProcessTouchEnd(0);
end;

procedure TTouchControl.Paint;
var
  i: Integer;
begin
  inherited;

  Canvas.Brush.Color := clWhite;
  Canvas.FillRect(ClientRect);

  // Dessiner les points de contact
  for i := 0 to High(FTouchPoints) do
  begin
    Canvas.Brush.Color := clBlue;
    Canvas.Pen.Color := clNavy;
    Canvas.Ellipse(
      FTouchPoints[i].Position.X - 20,
      FTouchPoints[i].Position.Y - 20,
      FTouchPoints[i].Position.X + 20,
      FTouchPoints[i].Position.Y + 20
    );

    // Ligne depuis le point de départ
    Canvas.Pen.Style := psDot;
    Canvas.Line(
      FTouchPoints[i].StartPosition.X,
      FTouchPoints[i].StartPosition.Y,
      FTouchPoints[i].Position.X,
      FTouchPoints[i].Position.Y
    );
  end;
end;

function TTouchControl.TouchPointCount: Integer;
begin
  Result := Length(FTouchPoints);
end;

function TTouchControl.GetTouchPoint(Index: Integer): TTouchPoint;
begin
  if (Index >= 0) and (Index < Length(FTouchPoints)) then
    Result := FTouchPoints[Index];
end;

{ TGestureRecognizer }

constructor TGestureRecognizer.Create;
begin
  FGestureThreshold := 10;
  FLongPressDelay := 500;
  FDoubleTapDelay := 300;
  FLastTapTime := 0;
  FLastTapPoint := Point(0, 0);
end;

function TGestureRecognizer.CalculateDistance(P1, P2: TPoint): Single;
begin
  Result := Sqrt(Sqr(P2.X - P1.X) + Sqr(P2.Y - P1.Y));
end;

function TGestureRecognizer.CalculateAngle(P1, P2: TPoint): Single;
begin
  Result := ArcTan2(P2.Y - P1.Y, P2.X - P1.X);
end;

function TGestureRecognizer.RecognizeGesture(const TouchPoints: array of TTouchPoint;
  out Gesture: TGestureInfo): Boolean;
var
  CurrentTime: TDateTime;
  TimeDiff: Integer;
begin
  Result := False;

  if Length(TouchPoints) = 0 then
    Exit;

  // Tap ou double tap pour un seul doigt
  if Length(TouchPoints) = 1 then
  begin
    if TouchPoints[0].Phase = tpEnded then
    begin
      CurrentTime := Now;
      TimeDiff := MilliSecondsBetween(CurrentTime, FLastTapTime);

      if (TimeDiff < FDoubleTapDelay) and
         (CalculateDistance(TouchPoints[0].Position, FLastTapPoint) < FGestureThreshold) then
      begin
        // Double tap
        Gesture.GestureType := gtDoubleTap;
        Gesture.StartPoint := TouchPoints[0].Position;
        Gesture.EndPoint := TouchPoints[0].Position;
        Result := True;
      end
      else
      begin
        // Simple tap
        Gesture.GestureType := gtTap;
        Gesture.StartPoint := TouchPoints[0].Position;
        Gesture.EndPoint := TouchPoints[0].Position;
        Result := True;
      end;

      FLastTapTime := CurrentTime;
      FLastTapPoint := TouchPoints[0].Position;
    end
    else if TouchPoints[0].Phase = tpMoved then
    begin
      // Vérifier pour un swipe
      Gesture := RecognizeSwipe;
      if Gesture.Distance > FGestureThreshold then
      begin
        Result := True;
        Gesture.StartPoint := TouchPoints[0].StartPosition;
        Gesture.EndPoint := TouchPoints[0].Position;
      end;
    end
    else if TouchPoints[0].Phase = tpStationary then
    begin
      // Vérifier pour un long press
      TimeDiff := MilliSecondsBetween(Now, TouchPoints[0].Timestamp);
      if TimeDiff > FLongPressDelay then
      begin
        Gesture.GestureType := gtLongPress;
        Gesture.StartPoint := TouchPoints[0].Position;
        Gesture.EndPoint := TouchPoints[0].Position;
        Gesture.Duration := TimeDiff;
        Result := True;
      end;
    end;
  end
  // Gestes multi-touch
  else if Length(TouchPoints) = 2 then
  begin
    // Pinch (zoom) ou rotation
    if (TouchPoints[0].Phase = tpMoved) or (TouchPoints[1].Phase = tpMoved) then
    begin
      // Essayer de reconnaître un pinch
      Gesture := RecognizePinch;
      if Gesture.Scale <> 1.0 then
      begin
        Result := True;
        Exit;
      end;

      // Essayer de reconnaître une rotation
      Gesture := RecognizeRotation;
      if Abs(Gesture.Rotation) > 0.1 then
      begin
        Result := True;
        Exit;
      end;
    end;
  end;
end;

function TGestureRecognizer.RecognizeSwipe: TGestureInfo;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.GestureType := gtSwipe;

  if Length(FTouchPoints) > 0 then
  begin
    Result.Distance := CalculateDistance(
      FTouchPoints[0].StartPosition,
      FTouchPoints[0].Position
    );

    Result.Direction := CalculateAngle(
      FTouchPoints[0].StartPosition,
      FTouchPoints[0].Position
    );

    // Calculer la vitesse
    Result.Duration := MilliSecondsBetween(Now, FTouchPoints[0].Timestamp);
    if Result.Duration > 0 then
      Result.Velocity := Result.Distance / Result.Duration * 1000; // pixels/seconde
  end;
end;

function TGestureRecognizer.RecognizePinch: TGestureInfo;
var
  OldDistance, NewDistance: Single;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.GestureType := gtPinch;
  Result.Scale := 1.0;

  if Length(FTouchPoints) >= 2 then
  begin
    // Distance entre les deux doigts au début
    OldDistance := CalculateDistance(
      FTouchPoints[0].StartPosition,
      FTouchPoints[1].StartPosition
    );

    // Distance actuelle
    NewDistance := CalculateDistance(
      FTouchPoints[0].Position,
      FTouchPoints[1].Position
    );

    if OldDistance > 0 then
      Result.Scale := NewDistance / OldDistance;
  end;
end;

function TGestureRecognizer.RecognizeRotation: TGestureInfo;
var
  OldAngle, NewAngle: Single;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.GestureType := gtRotate;
  Result.Rotation := 0;

  if Length(FTouchPoints) >= 2 then
  begin
    // Angle initial entre les deux doigts
    OldAngle := CalculateAngle(
      FTouchPoints[0].StartPosition,
      FTouchPoints[1].StartPosition
    );

    // Angle actuel
    NewAngle := CalculateAngle(
      FTouchPoints[0].Position,
      FTouchPoints[1].Position
    );

    Result.Rotation := NewAngle - OldAngle;

    // Normaliser entre -PI et PI
    while Result.Rotation > Pi do
      Result.Rotation := Result.Rotation - 2 * Pi;
    while Result.Rotation < -Pi do
      Result.Rotation := Result.Rotation + 2 * Pi;
  end;
end;
```

## Système d'événements basé sur les messages

### Architecture de messagerie événementielle

```pascal
unit MessageEventSystem;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs;

type
  // Types de messages
  TMessageType = (
    mtInfo,
    mtWarning,
    mtError,
    mtData,
    mtCommand,
    mtRequest,
    mtResponse,
    mtBroadcast
  );

  // Priorité des messages
  TMessagePriority = (mpLow, mpNormal, mpHigh, mpCritical);

  // Message de base
  TEventMessage = class
  private
    FID: TGUID;
    FType: TMessageType;
    FPriority: TMessagePriority;
    FSender: TObject;
    FTarget: TObject;
    FTimestamp: TDateTime;
    FData: Variant;
    FProcessed: Boolean;
    FResponse: Variant;
  public
    constructor Create(AType: TMessageType; ASender: TObject = nil);

    property ID: TGUID read FID;
    property MessageType: TMessageType read FType write FType;
    property Priority: TMessagePriority read FPriority write FPriority;
    property Sender: TObject read FSender write FSender;
    property Target: TObject read FTarget write FTarget;
    property Timestamp: TDateTime read FTimestamp;
    property Data: Variant read FData write FData;
    property Processed: Boolean read FProcessed write FProcessed;
    property Response: Variant read FResponse write FResponse;
  end;

  // Gestionnaire de messages
  TMessageHandler = procedure(Message: TEventMessage) of object;

  // Interface pour les objets qui peuvent recevoir des messages
  IMessageReceiver = interface
    ['{A7B8C9D0-E1F2-3456-789A-BCDEF0123456}']
    procedure ReceiveMessage(Message: TEventMessage);
    function AcceptMessage(Message: TEventMessage): Boolean;
  end;

  // Bus de messages central
  TMessageBus = class
  private
    FSubscribers: TObjectList;
    FMessageQueue: TQueue;
    FHandlers: TStringList;
    FProcessing: Boolean;
    FAsyncMode: Boolean;

    procedure ProcessQueue;
    procedure DeliverMessage(Message: TEventMessage);
  public
    constructor Create;
    destructor Destroy; override;

    // Abonnements
    procedure Subscribe(Receiver: IMessageReceiver;
                       MessageType: TMessageType = mtBroadcast);
    procedure Unsubscribe(Receiver: IMessageReceiver);

    // Handlers typés
    procedure RegisterHandler(const MessageID: string; Handler: TMessageHandler);
    procedure UnregisterHandler(const MessageID: string);

    // Envoi de messages
    procedure SendMessage(Message: TEventMessage);
    procedure PostMessage(Message: TEventMessage);
    procedure BroadcastMessage(MessageType: TMessageType; const Data: Variant);

    // Requête avec réponse
    function SendRequest(Target: TObject; const Data: Variant;
                        TimeoutMS: Integer = 5000): Variant;

    property AsyncMode: Boolean read FAsyncMode write FAsyncMode;
  end;

  // Composant receveur de messages
  TMessageComponent = class(TComponent, IMessageReceiver)
  private
    FOnMessage: TMessageHandler;
    FAcceptedTypes: set of TMessageType;
  protected
    procedure HandleMessage(Message: TEventMessage); virtual;
  public
    constructor Create(AOwner: TComponent); override;

    // IMessageReceiver
    procedure ReceiveMessage(Message: TEventMessage);
    function AcceptMessage(Message: TEventMessage): Boolean;

    procedure SubscribeTo(MessageBus: TMessageBus);
    procedure UnsubscribeFrom(MessageBus: TMessageBus);

  published
    property OnMessage: TMessageHandler read FOnMessage write FOnMessage;
  end;

// Singleton global
function GlobalMessageBus: TMessageBus;

implementation

var
  GMessageBus: TMessageBus = nil;

function GlobalMessageBus: TMessageBus;
begin
  if not Assigned(GMessageBus) then
    GMessageBus := TMessageBus.Create;
  Result := GMessageBus;
end;

{ TEventMessage }

constructor TEventMessage.Create(AType: TMessageType; ASender: TObject);
begin
  CreateGUID(FID);
  FType := AType;
  FPriority := mpNormal;
  FSender := ASender;
  FTarget := nil;
  FTimestamp := Now;
  FProcessed := False;
end;

{ TMessageBus }

constructor TMessageBus.Create;
begin
  FSubscribers := TObjectList.Create(False);
  FMessageQueue := TQueue.Create;
  FHandlers := TStringList.Create;
  FProcessing := False;
  FAsyncMode := False;
end;

destructor TMessageBus.Destroy;
var
  Msg: TEventMessage;
begin
  // Vider la file de messages
  while FMessageQueue.Count > 0 do
  begin
    Msg := TEventMessage(FMessageQueue.Pop);
    Msg.Free;
  end;

  FMessageQueue.Free;
  FSubscribers.Free;
  FHandlers.Free;

  inherited;
end;

procedure TMessageBus.Subscribe(Receiver: IMessageReceiver;
                               MessageType: TMessageType);
begin
  if FSubscribers.IndexOf(TObject(Receiver)) < 0 then
    FSubscribers.Add(TObject(Receiver));
end;

procedure TMessageBus.Unsubscribe(Receiver: IMessageReceiver);
begin
  FSubscribers.Remove(TObject(Receiver));
end;

procedure TMessageBus.RegisterHandler(const MessageID: string;
                                     Handler: TMessageHandler);
begin
  FHandlers.AddObject(MessageID, TObject(@Handler));
end;

procedure TMessageBus.UnregisterHandler(const MessageID: string);
var
  Index: Integer;
begin
  Index := FHandlers.IndexOf(MessageID);
  if Index >= 0 then
    FHandlers.Delete(Index);
end;

procedure TMessageBus.SendMessage(Message: TEventMessage);
begin
  if FAsyncMode then
    PostMessage(Message)
  else
    DeliverMessage(Message);
end;

procedure TMessageBus.PostMessage(Message: TEventMessage);
begin
  FMessageQueue.Push(Message);

  if not FProcessing then
    ProcessQueue;
end;

procedure TMessageBus.BroadcastMessage(MessageType: TMessageType;
                                      const Data: Variant);
var
  Msg: TEventMessage;
begin
  Msg := TEventMessage.Create(MessageType);
  Msg.Data := Data;
  SendMessage(Msg);
end;

procedure TMessageBus.ProcessQueue;
var
  Msg: TEventMessage;
begin
  FProcessing := True;
  try
    while FMessageQueue.Count > 0 do
    begin
      Msg := TEventMessage(FMessageQueue.Pop);
      try
        DeliverMessage(Msg);
      finally
        Msg.Free;
      end;
    end;
  finally
    FProcessing := False;
  end;
end;

procedure TMessageBus.DeliverMessage(Message: TEventMessage);
var
  i: Integer;
  Receiver: IMessageReceiver;
  Handler: TMessageHandler;
  HandlerPtr: Pointer;
begin
  // Livrer aux abonnés
  for i := 0 to FSubscribers.Count - 1 do
  begin
    if Supports(FSubscribers[i], IMessageReceiver, Receiver) then
    begin
      if Receiver.AcceptMessage(Message) then
        Receiver.ReceiveMessage(Message);
    end;
  end;

  // Appeler les handlers enregistrés
  i := FHandlers.IndexOf(GUIDToString(Message.ID));
  if i >= 0 then
  begin
    HandlerPtr := FHandlers.Objects[i];
    if Assigned(HandlerPtr) then
    begin
      TMethod(Handler).Code := HandlerPtr;
      TMethod(Handler).Data := nil;
      Handler(Message);
    end;
  end;

  Message.Processed := True;
end;

function TMessageBus.SendRequest(Target: TObject; const Data: Variant;
                                TimeoutMS: Integer): Variant;
var
  RequestMsg: TEventMessage;
  StartTime: TDateTime;
begin
  RequestMsg := TEventMessage.Create(mtRequest, Self);
  RequestMsg.Target := Target;
  RequestMsg.Data := Data;

  SendMessage(RequestMsg);

  StartTime := Now;
  while not RequestMsg.Processed do
  begin
    Application.ProcessMessages;

    if MilliSecondsBetween(Now, StartTime) > TimeoutMS then
      raise Exception.Create('Request timeout');
  end;

  Result := RequestMsg.Response;
  RequestMsg.Free;
end;

{ TMessageComponent }

constructor TMessageComponent.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAcceptedTypes := [mtBroadcast];
end;

procedure TMessageComponent.ReceiveMessage(Message: TEventMessage);
begin
  HandleMessage(Message);

  if Assigned(FOnMessage) then
    FOnMessage(Message);
end;

function TMessageComponent.AcceptMessage(Message: TEventMessage): Boolean;
begin
  Result := (Message.MessageType in FAcceptedTypes) or
            (Message.Target = Self);
end;

procedure TMessageComponent.HandleMessage(Message: TEventMessage);
begin
  // Override dans les classes dérivées
end;

procedure TMessageComponent.SubscribeTo(MessageBus: TMessageBus);
begin
  MessageBus.Subscribe(Self);
end;

procedure TMessageComponent.UnsubscribeFrom(MessageBus: TMessageBus);
begin
  MessageBus.Unsubscribe(Self);
end;
```

## Optimisation et bonnes pratiques

### Gestionnaire d'événements optimisé

```pascal
unit OptimizedEventHandling;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs, ExtCtrls;

type
  // Pool d'objets événements pour éviter les allocations
  TEventPool = class
  private
    FPool: TStack;
    FMaxSize: Integer;
    FCreateCount: Integer;
    FReuseCount: Integer;
  public
    constructor Create(AMaxSize: Integer = 100);
    destructor Destroy; override;

    function Acquire: TObject;
    procedure Release(Obj: TObject);

    property CreateCount: Integer read FCreateCount;
    property ReuseCount: Integer read FReuseCount;
  end;

  // Gestionnaire d'événements avec cache
  TCachedEventManager = class
  private
    FEventCache: TStringList;
    FCacheSize: Integer;
    FCacheHits: Integer;
    FCacheMisses: Integer;

    procedure AddToCache(const Key: string; Value: TObject);
    function GetFromCache(const Key: string): TObject;
    procedure ClearCache;
  public
    constructor Create(ACacheSize: Integer = 50);
    destructor Destroy; override;

    function ProcessEvent(const EventID: string): TObject;

    property CacheHits: Integer read FCacheHits;
    property CacheMisses: Integer read FCacheMisses;
    function CacheHitRate: Double;
  end;

  // Throttler pour limiter la fréquence des événements
  TEventThrottler = class
  private
    FLastEventTime: TDateTime;
    FMinInterval: Integer; // En millisecondes
    FEventQueue: TQueue;
    FTimer: TTimer;

    procedure ProcessQueuedEvents(Sender: TObject);
  public
    constructor Create(AMinInterval: Integer = 100);
    destructor Destroy; override;

    function ShouldProcess: Boolean;
    procedure QueueEvent(Event: TNotifyEvent);
  end;

  // Debouncer pour retarder l'exécution
  TEventDebouncer = class
  private
    FDelay: Integer;
    FTimer: TTimer;
    FPendingEvent: TNotifyEvent;

    procedure ExecutePendingEvent(Sender: TObject);
  public
    constructor Create(ADelay: Integer = 500);
    destructor Destroy; override;

    procedure TriggerEvent(Event: TNotifyEvent);
    procedure Cancel;
  end;

implementation

uses
  DateUtils;

{ TEventPool }

constructor TEventPool.Create(AMaxSize: Integer);
begin
  FPool := TStack.Create;
  FMaxSize := AMaxSize;
  FCreateCount := 0;
  FReuseCount := 0;
end;

destructor TEventPool.Destroy;
var
  Obj: TObject;
begin
  while FPool.Count > 0 do
  begin
    Obj := TObject(FPool.Pop);
    Obj.Free;
  end;
  FPool.Free;
  inherited;
end;

function TEventPool.Acquire: TObject;
begin
  if FPool.Count > 0 then
  begin
    Result := TObject(FPool.Pop);
    Inc(FReuseCount);
  end
  else
  begin
    Result := TObject.Create;
    Inc(FCreateCount);
  end;
end;

procedure TEventPool.Release(Obj: TObject);
begin
  if FPool.Count < FMaxSize then
  begin
    // Réinitialiser l'objet si nécessaire
    FPool.Push(Obj);
  end
  else
    Obj.Free;
end;

{ TCachedEventManager }

constructor TCachedEventManager.Create(ACacheSize: Integer);
begin
  FEventCache := TStringList.Create;
  FEventCache.Sorted := True;
  FCacheSize := ACacheSize;
  FCacheHits := 0;
  FCacheMisses := 0;
end;

destructor TCachedEventManager.Destroy;
begin
  ClearCache;
  FEventCache.Free;
  inherited;
end;

procedure TCachedEventManager.AddToCache(const Key: string; Value: TObject);
begin
  // Limiter la taille du cache
  if FEventCache.Count >= FCacheSize then
  begin
    // Supprimer l'élément le plus ancien (FIFO)
    FEventCache.Objects[0].Free;
    FEventCache.Delete(0);
  end;

  FEventCache.AddObject(Key, Value);
end;

function TCachedEventManager.GetFromCache(const Key: string): TObject;
var
  Index: Integer;
begin
  Result := nil;
  Index := FEventCache.IndexOf(Key);

  if Index >= 0 then
  begin
    Result := FEventCache.Objects[Index];
    Inc(FCacheHits);
  end
  else
    Inc(FCacheMisses);
end;

procedure TCachedEventManager.ClearCache;
var
  i: Integer;
begin
  for i := 0 to FEventCache.Count - 1 do
    FEventCache.Objects[i].Free;
  FEventCache.Clear;
end;

function TCachedEventManager.ProcessEvent(const EventID: string): TObject;
begin
  // Vérifier le cache d'abord
  Result := GetFromCache(EventID);

  if not Assigned(Result) then
  begin
    // Créer/calculer le résultat
    Result := TObject.Create; // Remplacer par le traitement réel
    AddToCache(EventID, Result);
  end;
end;

function TCachedEventManager.CacheHitRate: Double;
var
  Total: Integer;
begin
  Total := FCacheHits + FCacheMisses;
  if Total > 0 then
    Result := FCacheHits / Total * 100
  else
    Result := 0;
end;

{ TEventThrottler }

constructor TEventThrottler.Create(AMinInterval: Integer);
begin
  FMinInterval := AMinInterval;
  FLastEventTime := 0;
  FEventQueue := TQueue.Create;

  FTimer := TTimer.Create(nil);
  FTimer.Interval := FMinInterval;
  FTimer.Enabled := False;
  FTimer.OnTimer := @ProcessQueuedEvents;
end;

destructor TEventThrottler.Destroy;
begin
  FTimer.Free;
  FEventQueue.Free;
  inherited;
end;

function TEventThrottler.ShouldProcess: Boolean;
var
  CurrentTime: TDateTime;
  ElapsedMS: Integer;
begin
  CurrentTime := Now;

  if FLastEventTime = 0 then
  begin
    FLastEventTime := CurrentTime;
    Result := True;
  end
  else
  begin
    ElapsedMS := MilliSecondsBetween(CurrentTime, FLastEventTime);
    Result := ElapsedMS >= FMinInterval;

    if Result then
      FLastEventTime := CurrentTime;
  end;
end;

procedure TEventThrottler.QueueEvent(Event: TNotifyEvent);
begin
  FEventQueue.Push(TObject(@Event));

  if not FTimer.Enabled then
    FTimer.Enabled := True;
end;

procedure TEventThrottler.ProcessQueuedEvents(Sender: TObject);
var
  Event: TNotifyEvent;
begin
  if FEventQueue.Count > 0 then
  begin
    TMethod(Event).Code := FEventQueue.Pop;
    TMethod(Event).Data := nil;

    if Assigned(Event) then
      Event(Self);

    if FEventQueue.Count = 0 then
      FTimer.Enabled := False;
  end;
end;

{ TEventDebouncer }

constructor TEventDebouncer.Create(ADelay: Integer);
begin
  FDelay := ADelay;

  FTimer := TTimer.Create(nil);
  FTimer.Interval := FDelay;
  FTimer.Enabled := False;
  FTimer.OnTimer := @ExecutePendingEvent;
end;

destructor TEventDebouncer.Destroy;
begin
  FTimer.Free;
  inherited;
end;

procedure TEventDebouncer.TriggerEvent(Event: TNotifyEvent);
begin
  FPendingEvent := Event;

  // Réinitialiser le timer
  FTimer.Enabled := False;
  FTimer.Enabled := True;
end;

procedure TEventDebouncer.ExecutePendingEvent(Sender: TObject);
begin
  FTimer.Enabled := False;

  if Assigned(FPendingEvent) then
  begin
    FPendingEvent(Self);
    FPendingEvent := nil;
  end;
end;

procedure TEventDebouncer.Cancel;
begin
  FTimer.Enabled := False;
  FPendingEvent := nil;
end;
```

## Conclusion et points clés

La gestion avancée des événements est un aspect crucial du développement d'applications interactives et réactives. Voici les concepts essentiels à retenir :

### Points fondamentaux

1. **Types d'événements personnalisés** : Créez vos propres types d'événements avec les paramètres dont vous avez besoin

2. **Chaînage d'événements** : Interceptez et étendez les événements existants sans perdre leur fonctionnalité d'origine

3. **Multi-cast** : Un événement peut déclencher plusieurs gestionnaires, permettant une architecture plus flexible

4. **Événements asynchrones** : Gérez les événements de manière asynchrone pour ne pas bloquer l'interface

5. **Bubbling et capture** : Implémentez la propagation d'événements dans une hiérarchie de composants

6. **Filtrage** : Contrôlez quels événements sont traités et lesquels sont bloqués

7. **Pattern Observer** : Découple les objets qui génèrent des événements de ceux qui les consomment

8. **Événements tactiles** : Support moderne pour les interfaces tactiles et les gestes

9. **Bus de messages** : Architecture centralisée pour la communication entre composants

10. **Optimisation** : Utilisez des techniques comme le pooling, le caching, le throttling et le debouncing

### Bonnes pratiques

- **Nommage cohérent** : Utilisez des conventions claires (OnBeforeX, OnAfterX, OnXChanged)
- **Documentation** : Documentez quand vos événements sont déclenchés
- **Gestion d'erreurs** : Les événements ne doivent pas faire crasher l'application
- **Performance** : Évitez les traitements lourds dans les gestionnaires d'événements
- **Mémoire** : Détachez toujours les gestionnaires d'événements pour éviter les fuites
- **Thread-safety** : Synchronisez correctement les événements multi-thread

La maîtrise de ces concepts vous permettra de créer des applications robustes, réactives et maintenables, capables de gérer des interactions complexes tout en restant performantes et élégantes dans leur architecture !

⏭️ [Thèmes et apparence personnalisée](/04-framework-lcl/09-themes-apparence-personnalisee.md)
