🔝 Retour au [Sommaire](/SOMMAIRE.md)

# Coroutines et programmation asynchrone en FreePascal/Lazarus

## Introduction : Qu'est-ce qu'une coroutine ?

Une coroutine est une fonction spéciale qui peut suspendre son exécution et la reprendre plus tard, exactement là où elle s'était arrêtée. Imaginez que vous lisez un livre : vous pouvez poser un marque-page, faire autre chose, puis reprendre exactement à la page où vous vous étiez arrêté. Les coroutines fonctionnent de la même façon.

La **programmation asynchrone** permet d'exécuter plusieurs tâches "en même temps" sans bloquer votre programme. C'est comme cuisiner : pendant que les pâtes cuisent, vous préparez la sauce au lieu d'attendre devant la casserole.

En FreePascal, bien qu'il n'y ait pas de support natif complet des coroutines comme dans certains langages modernes, nous pouvons implémenter des patterns similaires avec des techniques créatives.

## Concepts de base de l'asynchrone

### Comprendre le blocage et le non-blocage

```pascal
uses
  SysUtils, Classes;

// Opération bloquante traditionnelle
procedure BlockingOperation;
begin
  WriteLn('Début de l''opération...');
  Sleep(2000);  // Bloque le programme pendant 2 secondes
  WriteLn('Opération terminée');
  // Rien d'autre ne peut s'exécuter pendant ce temps
end;

// Simulation d'une opération non-bloquante
type
  TAsyncOperation = class(TThread)
  private
    FOnComplete: TNotifyEvent;
  protected
    procedure Execute; override;
  public
    property OnComplete: TNotifyEvent read FOnComplete write FOnComplete;
  end;

procedure TAsyncOperation.Execute;
begin
  WriteLn('Opération asynchrone démarrée...');
  Sleep(2000);  // Simule un travail long
  WriteLn('Opération asynchrone terminée');

  if Assigned(FOnComplete) then
    Synchronize(procedure
                begin
                  FOnComplete(Self);
                end);
end;

procedure NonBlockingOperation;
var
  AsyncOp: TAsyncOperation;
begin
  AsyncOp := TAsyncOperation.Create(True);
  AsyncOp.FreeOnTerminate := True;
  AsyncOp.OnComplete := procedure(Sender: TObject)
                        begin
                          WriteLn('Callback : opération terminée !');
                        end;
  AsyncOp.Start;

  // Le programme peut continuer immédiatement
  WriteLn('Le programme continue pendant l''opération...');
end;
```

## Implémentation simple de coroutines

### Coroutine basée sur un état

```pascal
type
  // État d'une coroutine
  TCoroutineState = (csReady, csRunning, csSuspended, csCompleted);

  // Coroutine simple avec yield
  TSimpleCoroutine = class
  private
    FState: TCoroutineState;
    FCurrentStep: Integer;
    FYieldValue: Variant;
  protected
    procedure ExecuteStep; virtual;
  public
    constructor Create;

    function Resume: Boolean; // Retourne False si terminé
    function GetYieldValue: Variant;
    procedure Reset;

    property State: TCoroutineState read FState;
  end;

constructor TSimpleCoroutine.Create;
begin
  FState := csReady;
  FCurrentStep := 0;
end;

function TSimpleCoroutine.Resume: Boolean;
begin
  if FState = csCompleted then
    Exit(False);

  FState := csRunning;
  ExecuteStep;

  if FState = csRunning then
    FState := csSuspended;

  Result := FState <> csCompleted;
end;

function TSimpleCoroutine.GetYieldValue: Variant;
begin
  Result := FYieldValue;
end;

procedure TSimpleCoroutine.Reset;
begin
  FState := csReady;
  FCurrentStep := 0;
  FYieldValue := Null;
end;

// Exemple : Compteur comme coroutine
type
  TCounterCoroutine = class(TSimpleCoroutine)
  private
    FMax: Integer;
  protected
    procedure ExecuteStep; override;
  public
    constructor Create(AMax: Integer);
  end;

constructor TCounterCoroutine.Create(AMax: Integer);
begin
  inherited Create;
  FMax := AMax;
end;

procedure TCounterCoroutine.ExecuteStep;
begin
  case FCurrentStep of
    0..999:  // Limite arbitraire pour éviter une boucle infinie
      begin
        if FCurrentStep < FMax then
        begin
          FYieldValue := FCurrentStep;
          Inc(FCurrentStep);
          FState := csSuspended;
        end
        else
          FState := csCompleted;
      end;
  else
    FState := csCompleted;
  end;
end;

// Utilisation
procedure UseSimpleCoroutine;
var
  Counter: TCounterCoroutine;
begin
  Counter := TCounterCoroutine.Create(5);
  try
    WriteLn('Coroutine compteur :');
    while Counter.Resume do
      WriteLn('Valeur : ', Counter.GetYieldValue);
    WriteLn('Coroutine terminée');
  finally
    Counter.Free;
  end;
end;
```

### Générateur avec yield

```pascal
type
  // Générateur générique
  TGenerator<T> = class
  private
    type
      TGeneratorProc = reference to procedure(Yield: reference to procedure(Value: T));
    var
      FProc: TGeneratorProc;
      FValues: TList<T>;
      FIndex: Integer;
      FGenerated: Boolean;

    procedure CollectValues;
  public
    constructor Create(AProc: TGeneratorProc);
    destructor Destroy; override;

    function MoveNext: Boolean;
    function GetCurrent: T;
    procedure Reset;

    property Current: T read GetCurrent;
  end;

constructor TGenerator<T>.Create(AProc: TGeneratorProc);
begin
  FProc := AProc;
  FValues := TList<T>.Create;
  FIndex := -1;
  FGenerated := False;
end;

destructor TGenerator<T>.Destroy;
begin
  FValues.Free;
  inherited;
end;

procedure TGenerator<T>.CollectValues;
begin
  if not FGenerated then
  begin
    FProc(procedure(Value: T)
          begin
            FValues.Add(Value);
          end);
    FGenerated := True;
  end;
end;

function TGenerator<T>.MoveNext: Boolean;
begin
  CollectValues;
  Inc(FIndex);
  Result := FIndex < FValues.Count;
end;

function TGenerator<T>.GetCurrent: T;
begin
  if (FIndex >= 0) and (FIndex < FValues.Count) then
    Result := FValues[FIndex]
  else
    Result := Default(T);
end;

procedure TGenerator<T>.Reset;
begin
  FIndex := -1;
end;

// Types spécialisés
type
  TIntGenerator = specialize TGenerator<Integer>;
  TStringGenerator = specialize TGenerator<string>;

// Utilisation
procedure UseGenerators;
var
  Numbers: TIntGenerator;
  Fibonacci: TIntGenerator;
  Words: TStringGenerator;
begin
  // Générateur de nombres
  Numbers := TIntGenerator.Create(
    procedure(Yield: reference to procedure(Value: Integer))
    var
      I: Integer;
    begin
      for I := 1 to 5 do
        Yield(I * I);  // Génère les carrés
    end
  );

  try
    WriteLn('Carrés de 1 à 5 :');
    while Numbers.MoveNext do
      Write(Numbers.Current, ' ');
    WriteLn;
  finally
    Numbers.Free;
  end;

  // Générateur de Fibonacci
  Fibonacci := TIntGenerator.Create(
    procedure(Yield: reference to procedure(Value: Integer))
    var
      A, B, C, Count: Integer;
    begin
      A := 0;
      B := 1;
      Count := 0;

      while Count < 10 do
      begin
        Yield(B);
        C := A + B;
        A := B;
        B := C;
        Inc(Count);
      end;
    end
  );

  try
    WriteLn('10 premiers nombres de Fibonacci :');
    while Fibonacci.MoveNext do
      Write(Fibonacci.Current, ' ');
    WriteLn;
  finally
    Fibonacci.Free;
  end;
end;
```

## Tâches asynchrones et Futures

### Pattern Future/Promise

> **Note :** Les types `TProc<T>` et `TFunc<T>` utilisés ci-dessous sont des types Delphi qui n'existent pas en FPC. En mode ObjFPC avec `{$modeswitch functionreferences}`, vous devez les déclarer :
> ```pascal
> type
>   TProc = reference to procedure;
>   TIntProc = reference to procedure(Value: Integer);
>   TStringProc = reference to procedure(const Value: string);
>   TIntFunc = reference to function: Integer;
> ```

```pascal
// Nécessite : uses SyncObjs;  (pour TRTLCriticalSection)
type
  // Types de callbacks (à déclarer en FPC ObjFPC)
  TIntProc = reference to procedure(Value: Integer);
  TStringProc = reference to procedure(const Value: string);
  TIntFunc = reference to function: Integer;

  // État d'un Future
  TFutureState = (fsPending, fsResolved, fsRejected);

  // Future simple pour Integer
  TIntFuture = class
  private
    FState: TFutureState;
    FValue: Integer;
    FError: string;
    FOnComplete: TIntProc;
    FOnError: TStringProc;
    FLock: TRTLCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Resolve(Value: Integer);
    procedure Reject(const Error: string);

    function Then_(OnComplete: TIntProc): TIntFuture;
    function Catch(OnError: TStringProc): TIntFuture;

    function IsComplete: Boolean;
    function GetValue: Integer;
    function GetError: string;

    property State: TFutureState read FState;
  end;

constructor TIntFuture.Create;
begin
  InitializeCriticalSection(FLock);
  FState := fsPending;
  FValue := 0;
  FError := '';
end;

destructor TIntFuture.Destroy;
begin
  DeleteCriticalSection(FLock);
  inherited;
end;

procedure TIntFuture.Resolve(Value: Integer);
begin
  EnterCriticalSection(FLock);
  try
    if FState = fsPending then
    begin
      FValue := Value;
      FState := fsResolved;

      if Assigned(FOnComplete) then
        FOnComplete(FValue);
    end;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TIntFuture.Reject(const Error: string);
begin
  EnterCriticalSection(FLock);
  try
    if FState = fsPending then
    begin
      FError := Error;
      FState := fsRejected;

      if Assigned(FOnError) then
        FOnError(FError);
    end;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

function TIntFuture.Then_(OnComplete: TIntProc): TIntFuture;
begin
  Result := Self;

  EnterCriticalSection(FLock);
  try
    FOnComplete := OnComplete;

    // Si déjà résolu, appeler immédiatement
    if FState = fsResolved then
      OnComplete(FValue);
  finally
    LeaveCriticalSection(FLock);
  end;
end;

function TIntFuture.Catch(OnError: TStringProc): TIntFuture;
begin
  Result := Self;

  EnterCriticalSection(FLock);
  try
    FOnError := OnError;

    // Si déjà rejeté, appeler immédiatement
    if FState = fsRejected then
      OnError(FError);
  finally
    LeaveCriticalSection(FLock);
  end;
end;

function TIntFuture.IsComplete: Boolean;
begin
  EnterCriticalSection(FLock);
  try
    Result := FState <> fsPending;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

function TIntFuture.GetValue: Integer;
begin
  EnterCriticalSection(FLock);
  try
    if FState = fsResolved then
      Result := FValue
    else
      raise Exception.Create('Future not resolved');
  finally
    LeaveCriticalSection(FLock);
  end;
end;

function TIntFuture.GetError: string;
begin
  EnterCriticalSection(FLock);
  try
    Result := FError;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

// Tâche asynchrone retournant un Future
type
  TAsyncTask = class(TThread)
  private
    FFuture: TIntFuture;
    FComputation: TIntFunc;
  protected
    procedure Execute; override;
  public
    constructor Create(Computation: TIntFunc);
    property Future: TIntFuture read FFuture;
  end;

constructor TAsyncTask.Create(Computation: TIntFunc);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FFuture := TIntFuture.Create;
  FComputation := Computation;
end;

procedure TAsyncTask.Execute;
begin
  try
    // Simule un calcul long
    Sleep(Random(2000) + 1000);

    FFuture.Resolve(FComputation());
  except
    on E: Exception do
      FFuture.Reject(E.Message);
  end;
end;

// Utilisation
procedure UseFutures;
var
  Task1, Task2: TAsyncTask;
  Future1, Future2: TIntFuture;
begin
  WriteLn('Lancement de tâches asynchrones...');

  // Première tâche
  Task1 := TAsyncTask.Create(
    function: Integer
    begin
      Result := 42;
    end
  );
  Future1 := Task1.Future;

  Future1
    .Then_(procedure(Value: Integer)
           begin
             WriteLn('Tâche 1 terminée : ', Value);
           end)
    .Catch(procedure(Error: string)
           begin
             WriteLn('Erreur tâche 1 : ', Error);
           end);

  Task1.Start;

  // Deuxième tâche
  Task2 := TAsyncTask.Create(
    function: Integer
    begin
      Result := Random(100);
    end
  );
  Future2 := Task2.Future;

  Future2.Then_(procedure(Value: Integer)
                begin
                  WriteLn('Tâche 2 terminée : ', Value);
                end);

  Task2.Start;

  // Attendre la fin (dans un vrai programme, on ferait autre chose)
  while not (Future1.IsComplete and Future2.IsComplete) do
  begin
    Write('.');
    Sleep(100);
  end;
  WriteLn;
  WriteLn('Toutes les tâches sont terminées');
end;
```

## Async/Await simulé

### Pattern Async/Await

```pascal
type
  TProc = reference to procedure;

  // Contexte d'exécution asynchrone
  TAsyncContext = class
  private
    FContinuation: TProc;
    FIsComplete: Boolean;
  public
    procedure SetContinuation(Continuation: TProc);
    procedure Complete;

    property IsComplete: Boolean read FIsComplete;
  end;

procedure TAsyncContext.SetContinuation(Continuation: TProc);
begin
  FContinuation := Continuation;
  if FIsComplete and Assigned(FContinuation) then
    FContinuation();
end;

procedure TAsyncContext.Complete;
begin
  FIsComplete := True;
  if Assigned(FContinuation) then
    FContinuation();
end;

// Simulateur Async/Await
type
  TAsyncFunction = class
  private
    FSteps: array of TProc;
    FCurrentStep: Integer;
    FContext: TAsyncContext;
  protected
    procedure ExecuteNextStep;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddStep(Step: TProc);
    procedure Start;
    function Await(AsyncOp: TProc): TAsyncContext;
  end;

constructor TAsyncFunction.Create;
begin
  FCurrentStep := 0;
  FContext := TAsyncContext.Create;
end;

destructor TAsyncFunction.Destroy;
begin
  FContext.Free;
  inherited;
end;

procedure TAsyncFunction.AddStep(Step: TProc);
var
  Len: Integer;
begin
  Len := Length(FSteps);
  SetLength(FSteps, Len + 1);
  FSteps[Len] := Step;
end;

procedure TAsyncFunction.Start;
begin
  FCurrentStep := 0;
  ExecuteNextStep;
end;

procedure TAsyncFunction.ExecuteNextStep;
begin
  if FCurrentStep < Length(FSteps) then
  begin
    FSteps[FCurrentStep]();
    Inc(FCurrentStep);
  end;
end;

function TAsyncFunction.Await(AsyncOp: TProc): TAsyncContext;
begin
  Result := TAsyncContext.Create;

  // Exécuter l'opération asynchrone
  TThread.CreateAnonymousThread(
    procedure
    begin
      AsyncOp();

      TThread.Synchronize(nil,
        procedure
        begin
          Result.Complete;
          ExecuteNextStep;  // Continuer avec l'étape suivante
        end
      );
    end
  ).Start;
end;

// Exemple d'utilisation
procedure AsyncAwaitExample;
var
  AsyncFunc: TAsyncFunction;
  Data: string;
begin
  Data := '';
  AsyncFunc := TAsyncFunction.Create;
  try
    // Étape 1 : Début
    AsyncFunc.AddStep(
      procedure
      begin
        WriteLn('Étape 1 : Démarrage de l''opération asynchrone');
      end
    );

    // Étape 2 : Première opération async
    AsyncFunc.AddStep(
      procedure
      var
        Context: TAsyncContext;
      begin
        WriteLn('Étape 2 : Chargement des données...');
        Context := AsyncFunc.Await(
          procedure
          begin
            Sleep(1000);  // Simule une opération longue
            Data := 'Données chargées';
          end
        );
      end
    );

    // Étape 3 : Traitement
    AsyncFunc.AddStep(
      procedure
      begin
        WriteLn('Étape 3 : Traitement : ', Data);
      end
    );

    // Étape 4 : Deuxième opération async
    AsyncFunc.AddStep(
      procedure
      var
        Context: TAsyncContext;
      begin
        WriteLn('Étape 4 : Sauvegarde...');
        Context := AsyncFunc.Await(
          procedure
          begin
            Sleep(500);
            Data := Data + ' et sauvegardées';
          end
        );
      end
    );

    // Étape 5 : Fin
    AsyncFunc.AddStep(
      procedure
      begin
        WriteLn('Étape 5 : Terminé - ', Data);
      end
    );

    // Démarrer l'exécution
    AsyncFunc.Start;

    // Attendre la fin (normalement on ferait autre chose)
    Sleep(2000);

  finally
    AsyncFunc.Free;
  end;
end;
```

## Canaux de communication

### Implémentation de canaux Go-like

```pascal
uses
  Generics.Collections;

type
  // Canal générique thread-safe
  generic TChannel<T> = class
  private
    FQueue: specialize TThreadedQueue<T>;
    FClosed: Boolean;
    FLock: TRTLCriticalSection;
  public
    constructor Create(BufferSize: Integer = 0);
    destructor Destroy; override;

    procedure Send(const Value: T);
    function Receive(out Value: T; Timeout: Cardinal = INFINITE): Boolean;
    function TryReceive(out Value: T): Boolean;
    procedure Close;

    property IsClosed: Boolean read FClosed;
  end;

constructor TChannel<T>.Create(BufferSize: Integer);
begin
  InitializeCriticalSection(FLock);
  FQueue := specialize TThreadedQueue<T>.Create(BufferSize, INFINITE, 0);
  FClosed := False;
end;

destructor TChannel<T>.Destroy;
begin
  Close;
  FQueue.Free;
  DeleteCriticalSection(FLock);
  inherited;
end;

procedure TChannel<T>.Send(const Value: T);
begin
  EnterCriticalSection(FLock);
  try
    if FClosed then
      raise Exception.Create('Canal fermé');
  finally
    LeaveCriticalSection(FLock);
  end;

  FQueue.PushItem(Value);
end;

function TChannel<T>.Receive(out Value: T; Timeout: Cardinal): Boolean;
begin
  Result := FQueue.PopItem(Value) = wrSignaled;
end;

function TChannel<T>.TryReceive(out Value: T): Boolean;
begin
  Result := Receive(Value, 0);
end;

procedure TChannel<T>.Close;
begin
  EnterCriticalSection(FLock);
  try
    FClosed := True;
  finally
    LeaveCriticalSection(FLock);
  end;
  FQueue.DoShutDown;
end;

// Types spécialisés
type
  TIntChannel = specialize TChannel<Integer>;
  TStringChannel = specialize TChannel<string>;

// Pattern producteur-consommateur
type
  TProducer = class(TThread)
  private
    FChannel: TIntChannel;
    FCount: Integer;
  protected
    procedure Execute; override;
  public
    constructor Create(Channel: TIntChannel; Count: Integer);
  end;

  TConsumer = class(TThread)
  private
    FChannel: TIntChannel;
    FName: string;
  protected
    procedure Execute; override;
  public
    constructor Create(Channel: TIntChannel; const Name: string);
  end;

constructor TProducer.Create(Channel: TIntChannel; Count: Integer);
begin
  inherited Create(True);
  FChannel := Channel;
  FCount := Count;
end;

procedure TProducer.Execute;
var
  I: Integer;
begin
  for I := 1 to FCount do
  begin
    FChannel.Send(I);
    WriteLn('[Producteur] Envoyé : ', I);
    Sleep(100);
  end;
  WriteLn('[Producteur] Terminé');
end;

constructor TConsumer.Create(Channel: TIntChannel; const Name: string);
begin
  inherited Create(True);
  FChannel := Channel;
  FName := Name;
end;

procedure TConsumer.Execute;
var
  Value: Integer;
begin
  while not Terminated do
  begin
    if FChannel.Receive(Value, 200) then
      WriteLn('[', FName, '] Reçu : ', Value)
    else if FChannel.IsClosed then
      Break;
  end;
  WriteLn('[', FName, '] Terminé');
end;

// Utilisation
procedure UseChannels;
var
  Channel: TIntChannel;
  Producer: TProducer;
  Consumer1, Consumer2: TConsumer;
begin
  WriteLn('=== Communication par canaux ===');

  Channel := TIntChannel.Create(5);  // Buffer de 5 éléments
  try
    Producer := TProducer.Create(Channel, 10);
    Consumer1 := TConsumer.Create(Channel, 'Consommateur 1');
    Consumer2 := TConsumer.Create(Channel, 'Consommateur 2');

    // Démarrer les threads
    Producer.Start;
    Consumer1.Start;
    Consumer2.Start;

    // Attendre le producteur
    Producer.WaitFor;
    Producer.Free;

    // Fermer le canal
    Sleep(500);  // Laisser le temps aux consommateurs
    Channel.Close;

    // Attendre les consommateurs
    Consumer1.Terminate;
    Consumer2.Terminate;
    Consumer1.WaitFor;
    Consumer2.WaitFor;
    Consumer1.Free;
    Consumer2.Free;

  finally
    Channel.Free;
  end;
end;
```

## Pattern Actor Model

### Implémentation d'acteurs

```pascal
type
  // Message pour les acteurs
  TActorMessage = record
    Sender: TObject;
    MessageType: string;
    Data: Variant;
  end;

  // Acteur de base
  TActor = class(TThread)
  private
    FMailbox: specialize TThreadedQueue<TActorMessage>;
    FName: string;
    FRunning: Boolean;
  protected
    procedure Execute; override;
    procedure HandleMessage(const Msg: TActorMessage); virtual; abstract;
  public
    constructor Create(const AName: string);
    destructor Destroy; override;

    procedure Send(const Msg: TActorMessage);
    procedure Stop;

    property Name: string read FName;
  end;

constructor TActor.Create(const AName: string);
begin
  inherited Create(True);
  FName := AName;
  FMailbox := specialize TThreadedQueue<TActorMessage>.Create(100, INFINITE, 0);
  FRunning := True;
end;

destructor TActor.Destroy;
begin
  FMailbox.Free;
  inherited;
end;

procedure TActor.Execute;
var
  Msg: TActorMessage;
begin
  while FRunning do
  begin
    if FMailbox.PopItem(Msg) = wrSignaled then
    begin
      if Msg.MessageType = 'STOP' then
        FRunning := False
      else
        HandleMessage(Msg);
    end;
  end;
end;

procedure TActor.Send(const Msg: TActorMessage);
begin
  FMailbox.PushItem(Msg);
end;

procedure TActor.Stop;
var
  StopMsg: TActorMessage;
begin
  StopMsg.MessageType := 'STOP';
  Send(StopMsg);
end;

// Exemple d'acteur concret : Calculateur
type
  TCalculatorActor = class(TActor)
  private
    FResult: Double;
  protected
    procedure HandleMessage(const Msg: TActorMessage); override;
  public
    constructor Create;
  end;

constructor TCalculatorActor.Create;
begin
  inherited Create('Calculator');
  FResult := 0;
end;

procedure TCalculatorActor.HandleMessage(const Msg: TActorMessage);
begin
  if Msg.MessageType = 'ADD' then
  begin
    FResult := FResult + Double(Msg.Data);
    WriteLn('[', FName, '] Résultat après addition : ', FResult:0:2);
  end
  else if Msg.MessageType = 'MULTIPLY' then
  begin
    FResult := FResult * Double(Msg.Data);
    WriteLn('[', FName, '] Résultat après multiplication : ', FResult:0:2);
  end
  else if Msg.MessageType = 'RESET' then
  begin
    FResult := 0;
    WriteLn('[', FName, '] Réinitialisé');
  end
  else if Msg.MessageType = 'PRINT' then
  begin
    WriteLn('[', FName, '] Valeur actuelle : ', FResult:0:2);
  end;
end;

// Acteur Logger
type
  TLoggerActor = class(TActor)
  private
    FLogFile: TextFile;
  protected
    procedure HandleMessage(const Msg: TActorMessage); override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

constructor TLoggerActor.Create;
begin
  inherited Create('Logger');
  AssignFile(FLogFile, 'actor_log.txt');
  Rewrite(FLogFile);
end;

destructor TLoggerActor.Destroy;
begin
  CloseFile(FLogFile);
  inherited;
end;

procedure TLoggerActor.HandleMessage(const Msg: TActorMessage);
begin
  WriteLn(FLogFile, Format('[%s] %s: %s',
    [DateTimeToStr(Now), Msg.MessageType, VarToStr(Msg.Data)]));
  Flush(FLogFile);
  WriteLn('[Logger] Message enregistré : ', Msg.MessageType);
end;

// Système d'acteurs
type
  TActorSystem = class
  private
    FActors: TStringList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure RegisterActor(Actor: TActor);
    function GetActor(const Name: string): TActor;
    procedure StartAll;
    procedure StopAll;
  end;

constructor TActorSystem.Create;
begin
  FActors := TStringList.Create;
  FActors.OwnsObjects := True;
end;

destructor TActorSystem.Destroy;
begin
  StopAll;
  FActors.Free;
  inherited;
end;

procedure TActorSystem.RegisterActor(Actor: TActor);
begin
  FActors.AddObject(Actor.Name, Actor);
end;

function TActorSystem.GetActor(const Name: string): TActor;
var
  Index: Integer;
begin
  Index := FActors.IndexOf(Name);
  if Index >= 0 then
    Result := TActor(FActors.Objects[Index])
  else
    Result := nil;
end;

procedure TActorSystem.StartAll;
var
  I: Integer;
begin
  for I := 0 to FActors.Count - 1 do
    TActor(FActors.Objects[I]).Start;
end;

procedure TActorSystem.StopAll;
var
  I: Integer;
  Actor: TActor;
begin
  // Envoyer le signal d'arrêt
  for I := 0 to FActors.Count - 1 do
  begin
    Actor := TActor(FActors.Objects[I]);
    Actor.Stop;
  end;

  // Attendre l'arrêt
  for I := 0 to FActors.Count - 1 do
  begin
    Actor := TActor(FActors.Objects[I]);
    Actor.WaitFor;
  end;
end;

// Utilisation du système d'acteurs
procedure UseActorSystem;
var
  System: TActorSystem;
  CalcMsg, LogMsg: TActorMessage;
  Calculator: TActor;
  Logger: TActor;
begin
  WriteLn('=== Système d''acteurs ===');

  System := TActorSystem.Create;
  try
    // Créer et enregistrer les acteurs
    System.RegisterActor(TCalculatorActor.Create);
    System.RegisterActor(TLoggerActor.Create);

    // Démarrer tous les acteurs
    System.StartAll;

    // Récupérer les références
    Calculator := System.GetActor('Calculator');
    Logger := System.GetActor('Logger');

    // Envoyer des messages au calculateur
    CalcMsg.Sender := nil;
    CalcMsg.MessageType := 'ADD';
    CalcMsg.Data := 10.5;
    Calculator.Send(CalcMsg);

    CalcMsg.MessageType := 'ADD';
    CalcMsg.Data := 20.3;
    Calculator.Send(CalcMsg);

    CalcMsg.MessageType := 'MULTIPLY';
    CalcMsg.Data := 2.0;
    Calculator.Send(CalcMsg);

    CalcMsg.MessageType := 'PRINT';
    CalcMsg.Data := Null;
    Calculator.Send(CalcMsg);

    // Logger les opérations
    LogMsg.Sender := Calculator;
    LogMsg.MessageType := 'LOG';
    LogMsg.Data := 'Calculs effectués';
    Logger.Send(LogMsg);

    // Attendre un peu pour voir les résultats
    Sleep(500);

    // Arrêter le système
    System.StopAll;

  finally
    System.Free;
  end;

  WriteLn('Système d''acteurs arrêté');
end;
```

## Schedulers et boucles d'événements

### Implémentation d'un scheduler simple

```pascal
type
  // Nécessite : uses DateUtils;  (pour MSecsPerDay)

  // Tâche planifiée
  TScheduledTask = class
  private
    FName: string;
    FAction: TProc;
    FNextRun: TDateTime;
    FInterval: Integer; // en millisecondes
    FRepeating: Boolean;
    FEnabled: Boolean;
  public
    constructor Create(const AName: string; AAction: TProc;
                      AInterval: Integer; ARepeating: Boolean = True);

    procedure Execute;
    procedure UpdateNextRun;
    function ShouldRun: Boolean;

    property Name: string read FName;
    property Enabled: Boolean read FEnabled write FEnabled;
    property NextRun: TDateTime read FNextRun write FNextRun;
    property Repeating: Boolean read FRepeating;
  end;

constructor TScheduledTask.Create(const AName: string; AAction: TProc;
                                 AInterval: Integer; ARepeating: Boolean);
begin
  FName := AName;
  FAction := AAction;
  FInterval := AInterval;
  FRepeating := ARepeating;
  FEnabled := True;
  FNextRun := Now + (FInterval / MSecsPerDay);
end;

procedure TScheduledTask.Execute;
begin
  if Assigned(FAction) then
    FAction();
end;

procedure TScheduledTask.UpdateNextRun;
begin
  if FRepeating then
    FNextRun := Now + (FInterval / MSecsPerDay)
  else
    FEnabled := False;
end;

function TScheduledTask.ShouldRun: Boolean;
begin
  Result := FEnabled and (Now >= FNextRun);
end;

// Scheduler principal
type
  TTaskScheduler = class(TThread)
  private
    FTasks: TThreadList;
    FRunning: Boolean;
  protected
    procedure Execute; override;
    procedure ProcessTasks;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddTask(Task: TScheduledTask);
    procedure RemoveTask(const Name: string);
    procedure Stop;

    // Méthodes utilitaires
    procedure RunOnce(const Name: string; Delay: Integer; Action: TProc);
    procedure RunEvery(const Name: string; Interval: Integer; Action: TProc);
  end;

constructor TTaskScheduler.Create;
begin
  inherited Create(True);
  FTasks := TThreadList.Create;
  FRunning := True;
end;

destructor TTaskScheduler.Destroy;
var
  List: TList;
  I: Integer;
begin
  List := FTasks.LockList;
  try
    for I := 0 to List.Count - 1 do
      TScheduledTask(List[I]).Free;
  finally
    FTasks.UnlockList;
  end;
  FTasks.Free;
  inherited;
end;

procedure TTaskScheduler.Execute;
begin
  while FRunning do
  begin
    ProcessTasks;
    Sleep(10); // Check toutes les 10ms
  end;
end;

procedure TTaskScheduler.ProcessTasks;
var
  List: TList;
  I: Integer;
  Task: TScheduledTask;
begin
  List := FTasks.LockList;
  try
    for I := List.Count - 1 downto 0 do
    begin
      Task := TScheduledTask(List[I]);
      if Task.ShouldRun then
      begin
        Task.Execute;
        Task.UpdateNextRun;

        // Supprimer les tâches non répétitives terminées
        if not Task.Enabled then
        begin
          List.Delete(I);
          Task.Free;
        end;
      end;
    end;
  finally
    FTasks.UnlockList;
  end;
end;

procedure TTaskScheduler.AddTask(Task: TScheduledTask);
var
  List: TList;
begin
  List := FTasks.LockList;
  try
    List.Add(Task);
  finally
    FTasks.UnlockList;
  end;
end;

procedure TTaskScheduler.RemoveTask(const Name: string);
var
  List: TList;
  I: Integer;
  Task: TScheduledTask;
begin
  List := FTasks.LockList;
  try
    for I := List.Count - 1 downto 0 do
    begin
      Task := TScheduledTask(List[I]);
      if Task.Name = Name then
      begin
        List.Delete(I);
        Task.Free;
        Break;
      end;
    end;
  finally
    FTasks.UnlockList;
  end;
end;

procedure TTaskScheduler.Stop;
begin
  FRunning := False;
end;

procedure TTaskScheduler.RunOnce(const Name: string; Delay: Integer; Action: TProc);
var
  Task: TScheduledTask;
begin
  Task := TScheduledTask.Create(Name, Action, Delay, False);
  AddTask(Task);
end;

procedure TTaskScheduler.RunEvery(const Name: string; Interval: Integer; Action: TProc);
var
  Task: TScheduledTask;
begin
  Task := TScheduledTask.Create(Name, Action, Interval, True);
  AddTask(Task);
end;

// Boucle d'événements personnalisée
type
  TEventLoop = class
  private
    FEvents: TThreadList;
    FRunning: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Post(Event: TProc);
    procedure ProcessEvents;
    procedure Run;
    procedure Stop;
  end;

constructor TEventLoop.Create;
begin
  FEvents := TThreadList.Create;
  FRunning := False;
end;

destructor TEventLoop.Destroy;
begin
  FEvents.Free;
  inherited;
end;

procedure TEventLoop.Post(Event: TProc);
var
  List: TList;
  P: Pointer;
begin
  List := FEvents.LockList;
  try
    P := Pointer(@Event);
    List.Add(P);
  finally
    FEvents.UnlockList;
  end;
end;

procedure TEventLoop.ProcessEvents;
var
  List: TList;
  Event: TProc;
begin
  List := FEvents.LockList;
  try
    while List.Count > 0 do
    begin
      Event := TProc(List[0]);
      List.Delete(0);

      if Assigned(Event) then
        Event();
    end;
  finally
    FEvents.UnlockList;
  end;
end;

procedure TEventLoop.Run;
begin
  FRunning := True;
  while FRunning do
  begin
    ProcessEvents;
    Sleep(1);
  end;
end;

procedure TEventLoop.Stop;
begin
  FRunning := False;
end;

// Utilisation
procedure UseScheduler;
var
  Scheduler: TTaskScheduler;
  Counter: Integer;
begin
  WriteLn('=== Scheduler de tâches ===');
  Counter := 0;

  Scheduler := TTaskScheduler.Create;
  try
    // Tâche unique après 1 seconde
    Scheduler.RunOnce('Welcome', 1000,
      procedure
      begin
        WriteLn('[1s] Message de bienvenue !');
      end
    );

    // Tâche répétitive toutes les 500ms
    Scheduler.RunEvery('Counter', 500,
      procedure
      begin
        Inc(Counter);
        WriteLn('[500ms] Compteur : ', Counter);
      end
    );

    // Tâche répétitive toutes les 2 secondes
    Scheduler.RunEvery('Status', 2000,
      procedure
      begin
        WriteLn('[2s] Statut : En cours d''exécution...');
      end
    );

    // Démarrer le scheduler
    Scheduler.Start;

    // Laisser tourner pendant 5 secondes
    Sleep(5000);

    // Arrêter
    Scheduler.Stop;
    Scheduler.WaitFor;

  finally
    Scheduler.Free;
  end;

  WriteLn('Scheduler arrêté');
end;
```

## Reactive Programming

### Pattern Observer réactif

```pascal
type
  // Observable générique
  generic TObservable<T> = class
  private
    type
      TObserver = reference to procedure(Value: T);
    var
      FObservers: array of TObserver;
      FValue: T;
  public
    procedure Subscribe(Observer: TObserver);
    procedure Emit(Value: T);
    function Map<U>(Transform: specialize TFunc<T, U>): specialize TObservable<U>;
    function Filter(Predicate: specialize TPredicate<T>): TObservable<T>;

    property Value: T read FValue;
  end;

procedure TObservable<T>.Subscribe(Observer: TObserver);
var
  Len: Integer;
begin
  Len := Length(FObservers);
  SetLength(FObservers, Len + 1);
  FObservers[Len] := Observer;
end;

procedure TObservable<T>.Emit(Value: T);
var
  Observer: TObserver;
begin
  FValue := Value;
  for Observer in FObservers do
    if Assigned(Observer) then
      Observer(Value);
end;

function TObservable<T>.Map<U>(Transform: specialize TFunc<T, U>): specialize TObservable<U>;
var
  Result_: specialize TObservable<U>;
  Self_: TObservable<T>;
begin
  Result_ := specialize TObservable<U>.Create;
  Self_ := Self;

  Subscribe(procedure(Value: T)
            begin
              Result_.Emit(Transform(Value));
            end);

  Result := Result_;
end;

function TObservable<T>.Filter(Predicate: specialize TPredicate<T>): TObservable<T>;
var
  Result_: TObservable<T>;
begin
  Result_ := TObservable<T>.Create;

  Subscribe(procedure(Value: T)
            begin
              if Predicate(Value) then
                Result_.Emit(Value);
            end);

  Result := Result_;
end;

// Subject (Observable + Observer)
type
  generic TSubject<T> = class(specialize TObservable<T>)
  public
    procedure Next(Value: T);
    procedure Error(const ErrorMsg: string);
    procedure Complete;
  end;

procedure TSubject<T>.Next(Value: T);
begin
  Emit(Value);
end;

procedure TSubject<T>.Error(const ErrorMsg: string);
begin
  WriteLn('[ERROR] ', ErrorMsg);
  // Notifier les observateurs de l'erreur
end;

procedure TSubject<T>.Complete;
begin
  WriteLn('[COMPLETE]');
  // Notifier les observateurs de la fin
end;

// Types spécialisés
type
  TIntObservable = specialize TObservable<Integer>;
  TStringObservable = specialize TObservable<string>;
  TIntSubject = specialize TSubject<Integer>;

// Utilisation
procedure UseReactiveProgramming;
var
  Numbers: TIntSubject;
  Doubled: TIntObservable;
  EvenNumbers: TIntObservable;
begin
  WriteLn('=== Programmation Réactive ===');

  Numbers := TIntSubject.Create;
  try
    // Observer simple
    Numbers.Subscribe(
      procedure(Value: Integer)
      begin
        WriteLn('Valeur reçue : ', Value);
      end
    );

    // Transformation : doubler les valeurs
    Doubled := Numbers.Map<Integer>(
      function(X: Integer): Integer
      begin
        Result := X * 2;
      end
    );

    Doubled.Subscribe(
      procedure(Value: Integer)
      begin
        WriteLn('Valeur doublée : ', Value);
      end
    );

    // Filtrage : nombres pairs seulement
    EvenNumbers := Numbers.Filter(
      function(X: Integer): Boolean
      begin
        Result := (X mod 2) = 0;
      end
    );

    EvenNumbers.Subscribe(
      procedure(Value: Integer)
      begin
        WriteLn('Nombre pair : ', Value);
      end
    );

    // Émettre des valeurs
    WriteLn('--- Émission de valeurs ---');
    Numbers.Next(1);
    Numbers.Next(2);
    Numbers.Next(3);
    Numbers.Next(4);
    Numbers.Next(5);

    Numbers.Complete;

  finally
    Numbers.Free;
    Doubled.Free;
    EvenNumbers.Free;
  end;
end;
```

## Synchronisation et coordination

### Barrières et latches

```pascal
type
  // Nécessite : uses SyncObjs;  (pour TEvent, TCriticalSection)

  // CountDownLatch - attendre que N événements se produisent
  TCountDownLatch = class
  private
    FCount: Integer;
    FEvent: TEvent;
  public
    constructor Create(InitialCount: Integer);
    destructor Destroy; override;

    procedure CountDown;
    procedure Await;
    function GetCount: Integer;
  end;

constructor TCountDownLatch.Create(InitialCount: Integer);
begin
  FCount := InitialCount;
  FEvent := TEvent.Create(nil, True, False, '');
end;

destructor TCountDownLatch.Destroy;
begin
  FEvent.Free;
  inherited;
end;

procedure TCountDownLatch.CountDown;
begin
  if InterlockedDecrement(FCount) = 0 then
    FEvent.SetEvent;
end;

procedure TCountDownLatch.Await;
begin
  FEvent.WaitFor(INFINITE);
end;

function TCountDownLatch.GetCount: Integer;
begin
  Result := FCount;
end;

// CyclicBarrier - synchronisation de N threads
type
  TCyclicBarrier = class
  private
    FParties: Integer;
    FCount: Integer;
    FGeneration: Integer;
    FLock: TCriticalSection;
    FEvent: TEvent;
  public
    constructor Create(Parties: Integer);
    destructor Destroy; override;

    procedure Await;
    procedure Reset;
  end;

constructor TCyclicBarrier.Create(Parties: Integer);
begin
  FParties := Parties;
  FCount := Parties;
  FGeneration := 0;
  FLock := TCriticalSection.Create;
  FEvent := TEvent.Create(nil, True, False, '');
end;

destructor TCyclicBarrier.Destroy;
begin
  FEvent.Free;
  FLock.Free;
  inherited;
end;

procedure TCyclicBarrier.Await;
var
  LocalGen: Integer;
begin
  FLock.Acquire;
  try
    LocalGen := FGeneration;
    Dec(FCount);

    if FCount = 0 then
    begin
      // Dernier thread arrivé, réveiller tous les autres
      Inc(FGeneration);
      FCount := FParties;
      FEvent.SetEvent;
    end;
  finally
    FLock.Release;
  end;

  // Attendre que tous arrivent
  if LocalGen = FGeneration then
  begin
    FEvent.WaitFor(INFINITE);
    FEvent.ResetEvent;
  end;
end;

procedure TCyclicBarrier.Reset;
begin
  FLock.Acquire;
  try
    FCount := FParties;
    Inc(FGeneration);
    FEvent.ResetEvent;
  finally
    FLock.Release;
  end;
end;

// Worker avec barrière
type
  TBarrierWorker = class(TThread)
  private
    FName: string;
    FBarrier: TCyclicBarrier;
  protected
    procedure Execute; override;
  public
    constructor Create(const AName: string; Barrier: TCyclicBarrier);
  end;

constructor TBarrierWorker.Create(const AName: string; Barrier: TCyclicBarrier);
begin
  inherited Create(True);
  FName := AName;
  FBarrier := Barrier;
end;

procedure TBarrierWorker.Execute;
var
  I: Integer;
begin
  for I := 1 to 3 do
  begin
    // Phase de travail
    WriteLn('[', FName, '] Travail phase ', I);
    Sleep(Random(1000) + 500);

    WriteLn('[', FName, '] Attente à la barrière...');
    FBarrier.Await;

    WriteLn('[', FName, '] Barrière franchie !');
  end;
end;

// Utilisation
procedure UseSynchronization;
var
  Latch: TCountDownLatch;
  Barrier: TCyclicBarrier;
  Workers: array[0..2] of TBarrierWorker;
  I: Integer;
begin
  WriteLn('=== Test CountDownLatch ===');

  Latch := TCountDownLatch.Create(3);
  try
    // Simuler 3 tâches asynchrones
    for I := 1 to 3 do
    begin
      TThread.CreateAnonymousThread(
        procedure
        begin
          Sleep(Random(1000) + 500);
          WriteLn('Tâche ', I, ' terminée');
          Latch.CountDown;
        end
      ).Start;
    end;

    WriteLn('Attente de 3 tâches...');
    Latch.Await;
    WriteLn('Toutes les tâches sont terminées !');

  finally
    Latch.Free;
  end;

  WriteLn;
  WriteLn('=== Test CyclicBarrier ===');

  Barrier := TCyclicBarrier.Create(3);
  try
    // Créer et démarrer les workers
    for I := 0 to 2 do
    begin
      Workers[I] := TBarrierWorker.Create('Worker' + IntToStr(I + 1), Barrier);
      Workers[I].Start;
    end;

    // Attendre la fin
    for I := 0 to 2 do
    begin
      Workers[I].WaitFor;
      Workers[I].Free;
    end;

  finally
    Barrier.Free;
  end;
end;
```

## Patterns de concurrence avancés

### Pipeline asynchrone

```pascal
type
  // Étape de pipeline
  TPipelineStage<TIn, TOut> = class(TThread)
  private
    FInput: specialize TChannel<TIn>;
    FOutput: specialize TChannel<TOut>;
    FProcessor: specialize TFunc<TIn, TOut>;
    FName: string;
  protected
    procedure Execute; override;
  public
    constructor Create(const AName: string;
                      Input: specialize TChannel<TIn>;
                      Output: specialize TChannel<TOut>;
                      Processor: specialize TFunc<TIn, TOut>);
  end;

constructor TPipelineStage<TIn, TOut>.Create(const AName: string;
                                            Input: specialize TChannel<TIn>;
                                            Output: specialize TChannel<TOut>;
                                            Processor: specialize TFunc<TIn, TOut>);
begin
  inherited Create(True);
  FName := AName;
  FInput := Input;
  FOutput := Output;
  FProcessor := Processor;
end;

procedure TPipelineStage<TIn, TOut>.Execute;
var
  InputValue: TIn;
  OutputValue: TOut;
begin
  while not Terminated do
  begin
    if FInput.Receive(InputValue, 100) then
    begin
      WriteLn('[', FName, '] Traitement...');
      OutputValue := FProcessor(InputValue);
      FOutput.Send(OutputValue);
    end
    else if FInput.IsClosed then
      Break;
  end;

  WriteLn('[', FName, '] Terminé');
  FOutput.Close;
end;

// Types spécialisés pour le pipeline
type
  TStringIntStage = specialize TPipelineStage<string, Integer>;
  TIntIntStage = specialize TPipelineStage<Integer, Integer>;
  TIntStringStage = specialize TPipelineStage<Integer, string>;

// Fan-out/Fan-in pattern
type
  TFanOutManager = class
  private
    FWorkers: array of TThread;
    FInputChannel: TIntChannel;
    FOutputChannel: TIntChannel;
  public
    constructor Create(WorkerCount: Integer);
    destructor Destroy; override;

    procedure Start;
    procedure Stop;

    property InputChannel: TIntChannel read FInputChannel;
    property OutputChannel: TIntChannel read FOutputChannel;
  end;

constructor TFanOutManager.Create(WorkerCount: Integer);
var
  I: Integer;
begin
  FInputChannel := TIntChannel.Create(10);
  FOutputChannel := TIntChannel.Create(10);

  SetLength(FWorkers, WorkerCount);
  for I := 0 to WorkerCount - 1 do
  begin
    FWorkers[I] := TThread.CreateAnonymousThread(
      procedure
      var
        Value, Result: Integer;
      begin
        while not TThread.CurrentThread.Terminated do
        begin
          if FInputChannel.Receive(Value, 100) then
          begin
            // Traitement simulé
            Result := Value * Value;
            Sleep(Random(500) + 100);
            FOutputChannel.Send(Result);
          end
          else if FInputChannel.IsClosed then
            Break;
        end;
      end
    );
  end;
end;

destructor TFanOutManager.Destroy;
begin
  Stop;
  FInputChannel.Free;
  FOutputChannel.Free;
  inherited;
end;

procedure TFanOutManager.Start;
var
  Worker: TThread;
begin
  for Worker in FWorkers do
    Worker.Start;
end;

procedure TFanOutManager.Stop;
var
  Worker: TThread;
begin
  FInputChannel.Close;

  for Worker in FWorkers do
  begin
    Worker.Terminate;
    Worker.WaitFor;
    Worker.Free;
  end;

  FOutputChannel.Close;
end;
```

## Conclusion

La programmation asynchrone et les coroutines en FreePascal/Lazarus permettent de créer des applications réactives et efficaces. Voici les points essentiels à retenir :

### Concepts clés maîtrisés

1. **Coroutines simulées** : Implémentation avec machines à états
2. **Futures/Promises** : Gestion des résultats asynchrones
3. **Async/Await simulé** : Pattern pour code asynchrone lisible
4. **Canaux** : Communication thread-safe entre goroutines
5. **Actor Model** : Isolation et passage de messages
6. **Schedulers** : Planification de tâches asynchrones
7. **Reactive Programming** : Flux de données observables
8. **Synchronisation** : Barrières, latches et coordination

### Avantages de la programmation asynchrone

✅ **Bénéfices** :
- Applications plus réactives
- Meilleure utilisation des ressources
- Scalabilité améliorée
- Séparation des préoccupations
- Code non-bloquant

### Défis et limitations

⚠️ **Attention à** :
- Complexité accrue du débogage
- Gestion des erreurs plus difficile
- Synchronisation délicate
- Risques de race conditions
- Overhead de performance possible

### Bonnes pratiques

1. **Isolation** : Minimiser le partage d'état
2. **Immutabilité** : Préférer les données immutables
3. **Canaux** : Communiquer par messages, pas par mémoire partagée
4. **Timeouts** : Toujours prévoir des timeouts
5. **Graceful shutdown** : Arrêt propre des ressources
6. **Tests** : Tester les scénarios de concurrence

### Quand utiliser ces patterns

**Utilisez l'asynchrone pour** :
- I/O intensif (réseau, fichiers)
- Interfaces utilisateur réactives
- Traitement parallèle de données
- Systèmes distribués
- Serveurs haute performance

**Évitez pour** :
- Calculs CPU-intensifs simples
- Logique séquentielle simple
- Prototypes rapides
- Code à déboguer fréquemment

La programmation asynchrone en FreePascal, bien que nécessitant des implémentations manuelles, offre la flexibilité nécessaire pour créer des applications modernes et performantes.

⏭️ [Optimisations du compilateur](/03-langage-object-pascal-avance/11-optimisations-compilateur.md)
