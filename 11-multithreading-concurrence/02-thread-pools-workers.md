🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 11.2 Thread pools et workers

## Introduction aux thread pools

Dans le chapitre précédent, nous avons vu comment créer des threads individuels. Mais que se passe-t-il si vous devez traiter 1000 tâches en parallèle ? Créer 1000 threads serait inefficace et pourrait même saturer votre système.

C'est là qu'interviennent les **thread pools** (pools de threads).

### Qu'est-ce qu'un thread pool ?

Un **thread pool** est un groupe de threads réutilisables qui attendent des tâches à exécuter. Au lieu de créer et détruire des threads constamment, vous réutilisez les mêmes threads pour différentes tâches.

**Analogie** : Imaginez un restaurant avec des serveurs. Au lieu d'embaucher un serveur pour chaque client (créer un thread par tâche), vous avez un nombre fixe de serveurs (pool) qui servent plusieurs clients à tour de rôle.

### Avantages des thread pools

1. **Performance** : Réutilisation des threads évite le coût de création/destruction
2. **Contrôle des ressources** : Limite le nombre de threads actifs
3. **Simplicité** : Sépare la logique métier de la gestion des threads
4. **Scalabilité** : Adapté pour traiter un grand nombre de tâches

### Inconvénients à connaître

1. **Complexité initiale** : Plus complexe à mettre en place qu'un simple thread
2. **Overhead** : Pour des tâches très simples, peut être contre-productif
3. **Ordre d'exécution** : Les tâches peuvent être exécutées dans un ordre imprévisible

## Concepts fondamentaux

### Worker threads

Un **worker thread** est un thread dans le pool qui attend des tâches et les exécute. C'est un "travailleur" qui prend du travail dans une file d'attente.

### Queue de tâches

Les tâches à exécuter sont placées dans une **file d'attente** (queue). Les workers piochent dans cette file pour obtenir du travail.

```
[Tâche 1] [Tâche 2] [Tâche 3] [Tâche 4] [Tâche 5]
     ↓          ↓          ↓
[Worker 1]  [Worker 2]  [Worker 3]
```

## Implémentation simple d'un thread pool

Commençons par créer un thread pool basique pour comprendre le mécanisme.

### Structure de base

```pascal
unit SimpleThreadPool;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs;

type
  // Type pour une tâche simple
  TTaskProc = procedure of object;

  // Classe représentant une tâche
  TTask = class
  public
    TaskProc: TTaskProc;
    constructor Create(AProc: TTaskProc);
  end;

  // Worker thread qui exécute les tâches
  TWorkerThread = class(TThread)
  private
    FPool: TObject; // Référence au pool (déclaré comme TObject pour éviter la dépendance circulaire)
  protected
    procedure Execute; override;
  public
    constructor Create(APool: TObject);
  end;

  // Le thread pool principal
  TSimpleThreadPool = class
  private
    FWorkers: array of TWorkerThread;
    FTasks: TThreadList;
    FTaskAvailable: TEvent;
    FShutdown: Boolean;
    FWorkerCount: Integer;
  public
    constructor Create(AWorkerCount: Integer);
    destructor Destroy; override;

    procedure AddTask(ATask: TTaskProc);
    function GetTask: TTask;
    procedure Shutdown;
  end;

implementation

{ TTask }

constructor TTask.Create(AProc: TTaskProc);
begin
  inherited Create;
  TaskProc := AProc;
end;

{ TWorkerThread }

constructor TWorkerThread.Create(APool: TObject);
begin
  inherited Create(False); // Démarrer immédiatement
  FPool := APool;
  FreeOnTerminate := False;
end;

procedure TWorkerThread.Execute;
var
  Task: TTask;
  Pool: TSimpleThreadPool;
begin
  Pool := TSimpleThreadPool(FPool);

  while not Terminated do
  begin
    // Attendre qu'une tâche soit disponible
    Pool.FTaskAvailable.WaitFor(1000); // Timeout 1 seconde

    if Terminated or Pool.FShutdown then
      Break;

    // Récupérer une tâche
    Task := Pool.GetTask;

    if Assigned(Task) then
    begin
      try
        // Exécuter la tâche
        if Assigned(Task.TaskProc) then
          Task.TaskProc();
      finally
        Task.Free;
      end;
    end;
  end;
end;

{ TSimpleThreadPool }

constructor TSimpleThreadPool.Create(AWorkerCount: Integer);
var
  i: Integer;
begin
  inherited Create;

  FWorkerCount := AWorkerCount;
  FShutdown := False;

  // Créer la liste thread-safe pour les tâches
  FTasks := TThreadList.Create;

  // Créer l'événement pour signaler les tâches disponibles
  FTaskAvailable := TEvent.Create(nil, True, False, '');

  // Créer les workers
  SetLength(FWorkers, AWorkerCount);
  for i := 0 to AWorkerCount - 1 do
    FWorkers[i] := TWorkerThread.Create(Self);
end;

destructor TSimpleThreadPool.Destroy;
begin
  Shutdown;
  inherited;
end;

procedure TSimpleThreadPool.AddTask(ATask: TTaskProc);
var
  Task: TTask;
  List: TList;
begin
  if FShutdown then
    Exit;

  // Créer l'objet tâche
  Task := TTask.Create(ATask);

  // Ajouter à la liste thread-safe
  List := FTasks.LockList;
  try
    List.Add(Task);
  finally
    FTasks.UnlockList;
  end;

  // Signaler qu'une tâche est disponible
  FTaskAvailable.SetEvent;
end;

function TSimpleThreadPool.GetTask: TTask;
var
  List: TList;
begin
  Result := nil;

  List := FTasks.LockList;
  try
    if List.Count > 0 then
    begin
      Result := TTask(List[0]);
      List.Delete(0);

      // Si plus de tâches, réinitialiser l'événement
      if List.Count = 0 then
        FTaskAvailable.ResetEvent;
    end;
  finally
    FTasks.UnlockList;
  end;
end;

procedure TSimpleThreadPool.Shutdown;
var
  i: Integer;
  List: TList;
  Task: TTask;
begin
  FShutdown := True;
  FTaskAvailable.SetEvent; // Réveiller tous les workers

  // Attendre que tous les workers se terminent
  for i := 0 to High(FWorkers) do
  begin
    if Assigned(FWorkers[i]) then
    begin
      FWorkers[i].Terminate;
      FWorkers[i].WaitFor;
      FWorkers[i].Free;
    end;
  end;

  // Nettoyer les tâches restantes
  List := FTasks.LockList;
  try
    for i := 0 to List.Count - 1 do
      TTask(List[i]).Free;
    List.Clear;
  finally
    FTasks.UnlockList;
  end;

  FTasks.Free;
  FTaskAvailable.Free;
end;

end.
```

### Utilisation du thread pool simple

```pascal
procedure TForm1.ButtonStartClick(Sender: TObject);
var
  Pool: TSimpleThreadPool;
  i: Integer;
begin
  // Créer un pool avec 4 workers
  Pool := TSimpleThreadPool.Create(4);

  try
    // Ajouter 10 tâches
    for i := 1 to 10 do
    begin
      Pool.AddTask(@TacheExemple);
    end;

    // Attendre que toutes les tâches soient terminées
    Sleep(5000);
  finally
    Pool.Free; // Shutdown automatique dans le destructeur
  end;
end;

procedure TForm1.TacheExemple;
begin
  // Cette méthode sera exécutée par un worker
  Sleep(1000); // Simuler un traitement

  // Attention : Synchronize nécessaire pour l'interface
  TThread.Synchronize(nil, @AfficherMessage);
end;

procedure TForm1.AfficherMessage;
begin
  Memo1.Lines.Add('Tâche terminée à ' + TimeToStr(Now));
end;
```

## Thread pool avancé avec priorités

Ajoutons maintenant des fonctionnalités plus avancées comme les priorités.

```pascal
unit AdvancedThreadPool;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs, Generics.Collections;

type
  // Énumération des priorités
  TTaskPriority = (tpLow, tpNormal, tpHigh, tpCritical);

  // Type pour une procédure sans paramètre
  TPoolProc = procedure of object;

  // Tâche avancée avec priorité et données
  TAdvancedTask = class
  private
    FPriority: TTaskPriority;
    FProc: TPoolProc;
    FData: Pointer;
  public
    constructor Create(AProc: TPoolProc; APriority: TTaskPriority = tpNormal; AData: Pointer = nil);
    procedure Execute;

    property Priority: TTaskPriority read FPriority;
    property Data: Pointer read FData;
  end;

  // Comparateur pour trier les tâches par priorité
  TTaskComparer = class(TComparer<TAdvancedTask>)
  public
    function Compare(const Left, Right: TAdvancedTask): Integer; override;
  end;

  // Pool avancé
  TAdvancedThreadPool = class
  private
    FWorkers: array of TThread;
    FTasks: TThreadList<TAdvancedTask>;
    FTaskAvailable: TEvent;
    FShutdown: Boolean;
    FActiveTaskCount: Integer;
    FCS: TCriticalSection;
  public
    constructor Create(AWorkerCount: Integer);
    destructor Destroy; override;

    procedure Execute(AProc: TPoolProc; APriority: TTaskPriority = tpNormal);
    procedure WaitForAll;
    function GetActiveTaskCount: Integer;
  end;

implementation

{ TAdvancedTask }

constructor TAdvancedTask.Create(AProc: TPoolProc; APriority: TTaskPriority; AData: Pointer);
begin
  inherited Create;
  FProc := AProc;
  FPriority := APriority;
  FData := AData;
end;

procedure TAdvancedTask.Execute;
begin
  if Assigned(FProc) then
    FProc();
end;

{ TTaskComparer }

function TTaskComparer.Compare(const Left, Right: TAdvancedTask): Integer;
begin
  // Priorité plus élevée = exécution en premier
  Result := Ord(Right.Priority) - Ord(Left.Priority);
end;

{ TAdvancedThreadPool }

constructor TAdvancedThreadPool.Create(AWorkerCount: Integer);
var
  i: Integer;
begin
  inherited Create;

  FShutdown := False;
  FActiveTaskCount := 0;
  FCS := TCriticalSection.Create;

  // Utiliser TThreadList générique
  FTasks := TThreadList<TAdvancedTask>.Create;
  FTaskAvailable := TEvent.Create(nil, True, False, '');

  // Créer les workers (implémentation simplifiée pour l'exemple)
  SetLength(FWorkers, AWorkerCount);
  for i := 0 to AWorkerCount - 1 do
  begin
    // Ici, vous créeriez des TWorkerThread similaires à l'exemple précédent
    // mais adaptés pour utiliser TAdvancedTask
  end;
end;

destructor TAdvancedThreadPool.Destroy;
begin
  WaitForAll;
  FShutdown := True;

  // Nettoyer les workers et les ressources
  FTasks.Free;
  FTaskAvailable.Free;
  FCS.Free;

  inherited;
end;

procedure TAdvancedThreadPool.Execute(AProc: TPoolProc; APriority: TTaskPriority);
var
  Task: TAdvancedTask;
  List: TList<TAdvancedTask>;
begin
  Task := TAdvancedTask.Create(AProc, APriority);

  List := FTasks.LockList;
  try
    List.Add(Task);
    // Trier par priorité
    List.Sort(TTaskComparer.Create);
  finally
    FTasks.UnlockList;
  end;

  FCS.Enter;
  try
    Inc(FActiveTaskCount);
  finally
    FCS.Leave;
  end;

  FTaskAvailable.SetEvent;
end;

procedure TAdvancedThreadPool.WaitForAll;
begin
  // Attendre que toutes les tâches soient terminées
  while GetActiveTaskCount > 0 do
    Sleep(100);
end;

function TAdvancedThreadPool.GetActiveTaskCount: Integer;
begin
  FCS.Enter;
  try
    Result := FActiveTaskCount;
  finally
    FCS.Leave;
  end;
end;

end.
```

## Pattern Producer-Consumer

Un pattern très courant avec les thread pools est le **Producer-Consumer** (Producteur-Consommateur).

```pascal
unit ProducerConsumerExample;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs;

type
  // Élément de travail
  TWorkItem = class
  public
    ID: Integer;
    Data: string;
    constructor Create(AID: Integer; const AData: string);
  end;

  // Thread consommateur
  TConsumerThread = class(TThread)
  private
    FWorkQueue: TThreadList;
    FWorkAvailable: TEvent;
  protected
    procedure Execute; override;
  public
    constructor Create(AQueue: TThreadList; AEvent: TEvent);
    procedure ProcessItem(Item: TWorkItem);
  end;

  // Gestionnaire Producer-Consumer
  TProducerConsumer = class
  private
    FWorkQueue: TThreadList;
    FWorkAvailable: TEvent;
    FConsumers: array of TConsumerThread;
  public
    constructor Create(AConsumerCount: Integer);
    destructor Destroy; override;

    procedure Produce(const AData: string);
  end;

implementation

{ TWorkItem }

constructor TWorkItem.Create(AID: Integer; const AData: string);
begin
  inherited Create;
  ID := AID;
  Data := AData;
end;

{ TConsumerThread }

constructor TConsumerThread.Create(AQueue: TThreadList; AEvent: TEvent);
begin
  inherited Create(False);
  FWorkQueue := AQueue;
  FWorkAvailable := AEvent;
  FreeOnTerminate := False;
end;

procedure TConsumerThread.Execute;
var
  Item: TWorkItem;
  List: TList;
begin
  while not Terminated do
  begin
    // Attendre du travail
    FWorkAvailable.WaitFor(1000);

    if Terminated then Break;

    // Récupérer un élément
    List := FWorkQueue.LockList;
    try
      if List.Count > 0 then
      begin
        Item := TWorkItem(List[0]);
        List.Delete(0);

        if List.Count = 0 then
          FWorkAvailable.ResetEvent;
      end
      else
        Item := nil;
    finally
      FWorkQueue.UnlockList;
    end;

    // Traiter l'élément
    if Assigned(Item) then
    begin
      try
        ProcessItem(Item);
      finally
        Item.Free;
      end;
    end;
  end;
end;

procedure TConsumerThread.ProcessItem(Item: TWorkItem);
begin
  // Traitement de l'élément
  WriteLn(Format('Thread %d traite item %d: %s',
    [ThreadID, Item.ID, Item.Data]));
  Sleep(Random(1000)); // Simuler un traitement variable
end;

{ TProducerConsumer }

constructor TProducerConsumer.Create(AConsumerCount: Integer);
var
  i: Integer;
begin
  inherited Create;

  FWorkQueue := TThreadList.Create;
  FWorkAvailable := TEvent.Create(nil, True, False, '');

  // Créer les consommateurs
  SetLength(FConsumers, AConsumerCount);
  for i := 0 to AConsumerCount - 1 do
    FConsumers[i] := TConsumerThread.Create(FWorkQueue, FWorkAvailable);
end;

destructor TProducerConsumer.Destroy;
var
  i: Integer;
  List: TList;
begin
  // Arrêter les consommateurs
  for i := 0 to High(FConsumers) do
  begin
    FConsumers[i].Terminate;
    FConsumers[i].WaitFor;
    FConsumers[i].Free;
  end;

  // Nettoyer la queue
  List := FWorkQueue.LockList;
  try
    for i := 0 to List.Count - 1 do
      TWorkItem(List[i]).Free;
    List.Clear;
  finally
    FWorkQueue.UnlockList;
  end;

  FWorkQueue.Free;
  FWorkAvailable.Free;

  inherited;
end;

procedure TProducerConsumer.Produce(const AData: string);
var
  Item: TWorkItem;
  List: TList;
  NextID: Integer;
begin
  List := FWorkQueue.LockList;
  try
    NextID := List.Count + 1;
  finally
    FWorkQueue.UnlockList;
  end;

  Item := TWorkItem.Create(NextID, AData);

  List := FWorkQueue.LockList;
  try
    List.Add(Item);
  finally
    FWorkQueue.UnlockList;
  end;

  FWorkAvailable.SetEvent;
end;

end.
```

### Utilisation du Producer-Consumer

```pascal
procedure TForm1.TestProducerConsumer;
var
  PC: TProducerConsumer;
  i: Integer;
begin
  // Créer avec 3 consommateurs
  PC := TProducerConsumer.Create(3);
  try
    // Produire 20 éléments de travail
    for i := 1 to 20 do
    begin
      PC.Produce(Format('Travail numéro %d', [i]));
      Sleep(100); // Simuler la production progressive
    end;

    // Attendre que tout soit traité
    Sleep(5000);
  finally
    PC.Free;
  end;
end;
```

## Thread pool avec callback

Souvent, vous voulez être notifié quand une tâche est terminée. Voici comment implémenter des callbacks.

```pascal
type
  // Type de callback
  TTaskCallback = procedure(Success: Boolean; const ErrorMsg: string) of object;

  // Tâche avec callback
  TCallbackTask = class
  private
    FProc: TTaskProc;  // TTaskProc = procedure of object (défini plus haut)
    FCallback: TTaskCallback;
    FOnMainThread: Boolean;
  public
    constructor Create(AProc: TTaskProc; ACallback: TTaskCallback;
      AOnMainThread: Boolean = True);
    procedure Execute;
  end;

constructor TCallbackTask.Create(AProc: TTaskProc; ACallback: TTaskCallback;
  AOnMainThread: Boolean);
begin
  inherited Create;
  FProc := AProc;
  FCallback := ACallback;
  FOnMainThread := AOnMainThread;
end;

procedure TCallbackTask.Execute;
var
  Success: Boolean;
  ErrorMsg: string;
begin
  Success := False;
  ErrorMsg := '';

  try
    if Assigned(FProc) then
      FProc();
    Success := True;
  except
    on E: Exception do
    begin
      Success := False;
      ErrorMsg := E.Message;
    end;
  end;

  // Stocker les résultats pour le callback synchronisé
  FLastSuccess := Success;
  FLastError := ErrorMsg;

  // Exécuter le callback
  if Assigned(FCallback) then
  begin
    if FOnMainThread then
      TThread.Synchronize(nil, @DoCallback)
    else
      FCallback(Success, ErrorMsg);
  end;
end;

procedure TCallbackTask.DoCallback;
begin
  if Assigned(FCallback) then
    FCallback(FLastSuccess, FLastError);
end;
```

> **Note :** Les procédures anonymes (`procedure ... begin ... end`) nécessitent `{$modeswitch anonymousfunctions}` et `{$modeswitch functionreferences}` en mode ObjFPC (FPC 3.3.1+). Sans ces directives, utilisez des méthodes nommées comme `DoCallback` ci-dessus.

### Utilisation avec callback

```pascal
procedure TForm1.TraitementLong;
begin
  Sleep(2000); // Traitement long
end;

procedure TForm1.OnTaskComplete(Success: Boolean; const ErrorMsg: string);
begin
  if Success then
    ShowMessage('Tâche terminée avec succès')
  else
    ShowMessage('Erreur : ' + ErrorMsg);
end;

procedure TForm1.ExecuterAvecCallback;
var
  Task: TCallbackTask;
begin
  Task := TCallbackTask.Create(@TraitementLong, @OnTaskComplete, True);
  Task.Execute;
end;
```

## Dimensionnement optimal du pool

### Combien de workers créer ?

Le nombre optimal de workers dépend de plusieurs facteurs :

#### Pour des tâches CPU-intensives

```pascal
// Nombre de cœurs logiques disponibles
var
  OptimalWorkers: Integer;
begin
  OptimalWorkers := TThread.ProcessorCount;
  // Typiquement : ProcessorCount ou ProcessorCount + 1
end;
```

#### Pour des tâches I/O (réseau, disque)

```pascal
// Plus de workers car ils passent du temps à attendre
var
  OptimalWorkers: Integer;
begin
  OptimalWorkers := TThread.ProcessorCount * 2;
  // Ou même plus selon la latence I/O
end;
```

#### Formule générale

```
Nombre optimal = Nombre de cœurs × (1 + Temps d'attente / Temps CPU)
```

### Exemple d'auto-dimensionnement

```pascal
type
  TWorkloadType = (wtCPU, wtIO, wtMixed);

function GetOptimalWorkerCount(WorkloadType: TWorkloadType): Integer;
var
  CoreCount: Integer;
begin
  CoreCount := TThread.ProcessorCount;

  case WorkloadType of
    wtCPU:   Result := CoreCount;
    wtIO:    Result := CoreCount * 3;
    wtMixed: Result := CoreCount * 2;
  else
    Result := CoreCount;
  end;

  // Minimum 2, maximum 32
  if Result < 2 then Result := 2;
  if Result > 32 then Result := 32;
end;
```

## Gestion des exceptions dans le pool

Il est crucial de gérer les exceptions dans les workers pour éviter les crashs.

```pascal
procedure TWorkerThread.Execute;
var
  Task: TTask;
begin
  while not Terminated do
  begin
    Task := Pool.GetTask;

    if Assigned(Task) then
    begin
      try
        try
          Task.Execute;
        except
          on E: Exception do
          begin
            // Logger l'erreur
            WriteLn('Erreur dans le worker: ' + E.Message);

            // Optionnel : notifier via callback
            if Assigned(Task.OnError) then
            begin
              FLastErrorMsg := E.Message;
              TThread.Synchronize(nil, @NotifyError);
            end;
          end;
        end;
      finally
        Task.Free;
      end;
    end;
  end;
end;
```

## Métriques et monitoring

Ajoutons des métriques pour surveiller les performances du pool.

```pascal
type
  TThreadPoolStats = record
    ActiveWorkers: Integer;
    QueuedTasks: Integer;
    CompletedTasks: Int64;
    FailedTasks: Int64;
    AverageTaskTime: Double;
  end;

  TMonitoredThreadPool = class
  private
    FStats: TThreadPoolStats;
    FStatsCS: TCriticalSection;

    procedure UpdateStats;
  public
    function GetStats: TThreadPoolStats;
    procedure ResetStats;
  end;

function TMonitoredThreadPool.GetStats: TThreadPoolStats;
begin
  FStatsCS.Enter;
  try
    Result := FStats;
  finally
    FStatsCS.Leave;
  end;
end;

procedure TMonitoredThreadPool.ResetStats;
begin
  FStatsCS.Enter;
  try
    FillChar(FStats, SizeOf(FStats), 0);
  finally
    FStatsCS.Leave;
  end;
end;
```

## Différences Windows/Ubuntu

### Gestion des threads système

Les thread pools fonctionnent de manière identique sur Windows et Ubuntu grâce à l'abstraction de FreePascal, mais il existe des différences au niveau système :

#### Windows

```pascal
{$IFDEF WINDOWS}
uses
  Windows;

// Pool de threads natif Windows
var
  ThreadPool: PTP_POOL;
  CallbackEnv: TP_CALLBACK_ENVIRON;
begin
  // Utiliser le thread pool natif Windows (Vista+)
  ThreadPool := CreateThreadpool(nil);
  SetThreadpoolThreadMaximum(ThreadPool, 8);
  SetThreadpoolThreadMinimum(ThreadPool, 2);
end;
{$ENDIF}
```

#### Ubuntu/Linux

```pascal
{$IFDEF LINUX}
uses
  BaseUnix;

// Sur Linux, pas de pool natif intégré
// Utiliser notre implémentation ou des bibliothèques tierces
{$ENDIF}
```

### Priorités et ordonnancement

```pascal
procedure ConfigurerPriorite(Thread: TThread);
begin
  {$IFDEF WINDOWS}
  // Sous Windows : priorités plus fines
  Thread.Priority := tpHigher;
  {$ENDIF}

  {$IFDEF LINUX}
  // Sous Linux : nécessite souvent des privilèges
  // pour modifier les priorités temps-réel
  try
    Thread.Priority := tpHigher;
  except
    // Silencieusement ignorer si pas les droits
  end;
  {$ENDIF}
end;
```

## Bonnes pratiques

### 1. Choisir la bonne taille de pool

```pascal
// ❌ Trop de workers
Pool := TThreadPool.Create(1000); // Overhead énorme !

// ✅ Nombre raisonnable
Pool := TThreadPool.Create(TThread.ProcessorCount * 2);
```

### 2. Éviter les tâches trop courtes

```pascal
// ❌ Tâches trop simples
for i := 1 to 10000 do
  Pool.AddTask(@CalculerDouble); // Trop simple pour justifier un thread

// ✅ Grouper les petites tâches en un seul appel
Pool.AddTask(@CalculerTousLesDoubles);
```

### 3. Toujours nettoyer proprement

```pascal
procedure TForm1.FormDestroy(Sender: TObject);
begin
  if Assigned(FPool) then
  begin
    FPool.Shutdown; // Ou WaitForAll selon l'implémentation
    FPool.Free;
  end;
end;
```

### 4. Gérer les timeouts

```pascal
uses
  SysUtils, DateUtils;  // MilliSecondsBetween nécessite DateUtils

// Implémenter un timeout pour les tâches longues
type
  TTimeoutTask = class
  private
    FTimeout: Cardinal;
    FStartTime: TDateTime;
  public
    function IsTimeout: Boolean;
  end;

function TTimeoutTask.IsTimeout: Boolean;
begin
  Result := MilliSecondsBetween(Now, FStartTime) > FTimeout;
end;
```

### 5. Utiliser des queues appropriées

```pascal
// Pour l'ordre FIFO strict : TQueue
// Pour les priorités : TList avec tri
// Pour le vol de travail (work stealing) : TThreadedQueue
```

## Patterns avancés

### Work Stealing Pool

Un pool où les workers peuvent "voler" du travail aux autres workers quand ils sont inactifs.

```pascal
// Chaque worker a sa propre queue
type
  TWorkStealingWorker = class(TThread)
  private
    FLocalQueue: TQueue<TTask>;
    FPool: TWorkStealingPool;
  protected
    procedure Execute; override;
    function StealWork: TTask;
  end;

function TWorkStealingWorker.StealWork: TTask;
var
  i: Integer;
  OtherWorker: TWorkStealingWorker;
begin
  Result := nil;

  // Essayer de voler du travail aux autres workers
  for i := 0 to FPool.WorkerCount - 1 do
  begin
    OtherWorker := FPool.Workers[i];
    if OtherWorker <> Self then
    begin
      Result := OtherWorker.FLocalQueue.ExtractMin;
      if Assigned(Result) then
        Break;
    end;
  end;
end;
```

### Fork-Join Pattern

Pour diviser une tâche en sous-tâches et attendre leur complétion.

```pascal
type
  TForkJoinTask = class
  public
    procedure Fork; // Diviser en sous-tâches
    function Join: TResult; // Attendre et combiner
  end;
```

## Résumé

- **Thread pools** réutilisent des threads pour exécuter de nombreuses tâches
- **Workers** sont les threads qui exécutent les tâches dans le pool
- **Producer-Consumer** est un pattern classique avec les pools
- Le **dimensionnement** dépend du type de charge (CPU vs I/O)
- Les **callbacks** permettent de notifier la fin des tâches
- La **gestion des exceptions** est cruciale dans les workers
- FreePascal abstrait les différences Windows/Ubuntu

Les thread pools sont essentiels pour les applications performantes traitant de nombreuses tâches concurrentes.

## Exemples pratiques complets

### Exemple 1 : Traitement de fichiers en masse

Imaginons que vous devez traiter 100 fichiers images (redimensionnement, conversion, etc.).

```pascal
unit BatchImageProcessor;

{$mode objfpc}{$H+}
{$modeswitch anonymousfunctions}  // Nécessaire pour les procédures anonymes

interface

uses
  Classes, SysUtils, SyncObjs, Graphics;

type
  TImageTask = class
  public
    InputFile: string;
    OutputFile: string;
    TargetWidth: Integer;
    TargetHeight: Integer;
  end;

  TImageProcessorPool = class
  private
    FPool: TSimpleThreadPool;
    FTotalFiles: Integer;
    FProcessedFiles: Integer;
    FCS: TCriticalSection;
    FOnProgress: TNotifyEvent;

    procedure ProcessImage(Task: TImageTask);
    procedure UpdateProgress;
  public
    constructor Create(AWorkerCount: Integer);
    destructor Destroy; override;

    procedure ProcessDirectory(const AInputDir, AOutputDir: string;
      AWidth, AHeight: Integer);
    function GetProgress: Integer;

    property OnProgress: TNotifyEvent read FOnProgress write FOnProgress;
  end;

implementation

uses
  FPImage, FPReadJPEG, FPWriteJPEG;

constructor TImageProcessorPool.Create(AWorkerCount: Integer);
begin
  inherited Create;
  FPool := TSimpleThreadPool.Create(AWorkerCount);
  FCS := TCriticalSection.Create;
  FTotalFiles := 0;
  FProcessedFiles := 0;
end;

destructor TImageProcessorPool.Destroy;
begin
  FPool.Free;
  FCS.Free;
  inherited;
end;

procedure TImageProcessorPool.ProcessImage(Task: TImageTask);
var
  Image: TFPMemoryImage;
  Reader: TFPReaderJPEG;
  Writer: TFPWriterJPEG;
  Resized: TFPMemoryImage;
begin
  try
    // Charger l'image
    Image := TFPMemoryImage.Create(0, 0);
    Reader := TFPReaderJPEG.Create;
    try
      Image.LoadFromFile(Task.InputFile, Reader);

      // Redimensionner (logique simplifiée)
      Resized := TFPMemoryImage.Create(Task.TargetWidth, Task.TargetHeight);
      try
        // Code de redimensionnement ici...

        // Sauvegarder
        Writer := TFPWriterJPEG.Create;
        try
          Resized.SaveToFile(Task.OutputFile, Writer);
        finally
          Writer.Free;
        end;
      finally
        Resized.Free;
      end;
    finally
      Reader.Free;
      Image.Free;
    end;

    // Mettre à jour la progression
    FCS.Enter;
    try
      Inc(FProcessedFiles);
    finally
      FCS.Leave;
    end;

    // Notifier (sur le thread principal)
    TThread.Synchronize(nil, @UpdateProgress);

  finally
    Task.Free;
  end;
end;

procedure TImageProcessorPool.UpdateProgress;
begin
  if Assigned(FOnProgress) then
    FOnProgress(Self);
end;

procedure TImageProcessorPool.ProcessDirectory(const AInputDir, AOutputDir: string;
  AWidth, AHeight: Integer);
var
  SearchRec: TSearchRec;
  Task: TImageTask;
begin
  FTotalFiles := 0;
  FProcessedFiles := 0;

  // Compter les fichiers
  if FindFirst(AInputDir + '*.jpg', faAnyFile, SearchRec) = 0 then
  begin
    repeat
      Inc(FTotalFiles);
    until FindNext(SearchRec) <> 0;
    FindClose(SearchRec);
  end;

  // Créer les tâches
  if FindFirst(AInputDir + '*.jpg', faAnyFile, SearchRec) = 0 then
  begin
    repeat
      Task := TImageTask.Create;
      Task.InputFile := AInputDir + SearchRec.Name;
      Task.OutputFile := AOutputDir + SearchRec.Name;
      Task.TargetWidth := AWidth;
      Task.TargetHeight := AHeight;

      FPool.AddTask(procedure
      begin
        ProcessImage(Task);
      end);
    until FindNext(SearchRec) <> 0;
    FindClose(SearchRec);
  end;
end;

function TImageProcessorPool.GetProgress: Integer;
begin
  FCS.Enter;
  try
    if FTotalFiles > 0 then
      Result := Round((FProcessedFiles / FTotalFiles) * 100)
    else
      Result := 0;
  finally
    FCS.Leave;
  end;
end;

end.
```

#### Utilisation du processeur d'images

```pascal
procedure TForm1.ButtonProcessClick(Sender: TObject);
begin
  FProcessor := TImageProcessorPool.Create(4);
  FProcessor.OnProgress := @OnImageProgress;

  FProcessor.ProcessDirectory(
    'C:\Input\',
    'C:\Output\',
    800,  // Largeur
    600   // Hauteur
  );
end;

procedure TForm1.OnImageProgress(Sender: TObject);
begin
  ProgressBar1.Position := FProcessor.GetProgress;
  LabelStatus.Caption := Format('Progression : %d%%', [FProcessor.GetProgress]);
end;
```

### Exemple 2 : Serveur web multi-thread

Un serveur HTTP simple utilisant un thread pool pour gérer les connexions.

```pascal
unit SimpleWebServer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BlckSock, SyncObjs;

type
  THTTPRequest = class
  public
    Method: string;
    URI: string;
    Headers: TStringList;
    Body: string;

    constructor Create;
    destructor Destroy; override;
  end;

  THTTPResponse = class
  public
    StatusCode: Integer;
    StatusText: string;
    Headers: TStringList;
    Body: string;

    constructor Create;
    destructor Destroy; override;
    function ToString: string;
  end;

  TRequestHandler = procedure(Request: THTTPRequest; Response: THTTPResponse) of object;

  TWebServer = class;

  // Thread d'écoute dédié (au lieu de CreateAnonymousThread)
  TListenThread = class(TThread)
  private
    FServer: TWebServer;
  protected
    procedure Execute; override;
  public
    constructor Create(AServer: TWebServer);
  end;

  TWebServer = class
  private
    FPool: TSimpleThreadPool;
    FSocket: TTCPBlockSocket;
    FPort: Integer;
    FRunning: Boolean;
    FListenThread: TListenThread;
    FHandler: TRequestHandler;

    procedure HandleConnection(ClientSocket: TTCPBlockSocket);
    procedure ParseRequest(const Data: string; Request: THTTPRequest);
  public
    constructor Create(APort: Integer; AWorkerCount: Integer);
    destructor Destroy; override;

    procedure Start;
    procedure Stop;

    property OnRequest: TRequestHandler read FHandler write FHandler;
  end;

implementation

{ THTTPRequest }

constructor THTTPRequest.Create;
begin
  inherited Create;
  Headers := TStringList.Create;
end;

destructor THTTPRequest.Destroy;
begin
  Headers.Free;
  inherited;
end;

{ THTTPResponse }

constructor THTTPResponse.Create;
begin
  inherited Create;
  Headers := TStringList.Create;
  StatusCode := 200;
  StatusText := 'OK';
end;

destructor THTTPResponse.Destroy;
begin
  Headers.Free;
  inherited;
end;

function THTTPResponse.ToString: string;
var
  i: Integer;
begin
  Result := Format('HTTP/1.1 %d %s'#13#10, [StatusCode, StatusText]);

  for i := 0 to Headers.Count - 1 do
    Result := Result + Headers[i] + #13#10;

  Result := Result + 'Content-Length: ' + IntToStr(Length(Body)) + #13#10;
  Result := Result + #13#10 + Body;
end;

{ TWebServer }

constructor TWebServer.Create(APort: Integer; AWorkerCount: Integer);
begin
  inherited Create;
  FPort := APort;
  FPool := TSimpleThreadPool.Create(AWorkerCount);
  FSocket := TTCPBlockSocket.Create;
  FRunning := False;
end;

destructor TWebServer.Destroy;
begin
  Stop;
  FPool.Free;
  FSocket.Free;
  inherited;
end;

{ TListenThread }

constructor TListenThread.Create(AServer: TWebServer);
begin
  inherited Create(True); // Créer suspendu
  FServer := AServer;
  FreeOnTerminate := False;
end;

procedure TListenThread.Execute;
var
  ClientSocket: TTCPBlockSocket;
begin
  FServer.FSocket.CreateSocket;
  FServer.FSocket.SetLinger(True, 10000);
  FServer.FSocket.Bind('0.0.0.0', IntToStr(FServer.FPort));
  FServer.FSocket.Listen;

  while FServer.FRunning and (not Terminated) do
  begin
    if FServer.FSocket.CanRead(1000) then
    begin
      ClientSocket := TTCPBlockSocket.Create;
      ClientSocket.Socket := FServer.FSocket.Accept;

      if FServer.FSocket.LastError = 0 then
        FServer.HandleConnection(ClientSocket)
      else
        ClientSocket.Free;
    end;
  end;
end;

{ TWebServer }

procedure TWebServer.Start;
begin
  FRunning := True;
  FListenThread := TListenThread.Create(Self);
  FListenThread.Start;
end;

procedure TWebServer.Stop;
begin
  FRunning := False;

  if Assigned(FListenThread) then
  begin
    FListenThread.WaitFor;
    FListenThread.Free;
  end;
end;

procedure TWebServer.HandleConnection(ClientSocket: TTCPBlockSocket);
var
  Request: THTTPRequest;
  Response: THTTPResponse;
  Data: string;
begin
  try
    // Lire la requête
    Data := ClientSocket.RecvPacket(5000);

    if ClientSocket.LastError = 0 then
    begin
      Request := THTTPRequest.Create;
      Response := THTTPResponse.Create;
      try
        ParseRequest(Data, Request);

        // Appeler le gestionnaire personnalisé
        if Assigned(FHandler) then
          FHandler(Request, Response)
        else
        begin
          Response.StatusCode := 404;
          Response.StatusText := 'Not Found';
          Response.Body := '<h1>404 - Page not found</h1>';
        end;

        // Envoyer la réponse
        ClientSocket.SendString(Response.ToString);
      finally
        Request.Free;
        Response.Free;
      end;
    end;
  finally
    ClientSocket.Free;
  end;
end;

procedure TWebServer.ParseRequest(const Data: string; Request: THTTPRequest);
var
  Lines: TStringList;
  FirstLine: string;
  SpacePos: Integer;
begin
  Lines := TStringList.Create;
  try
    Lines.Text := Data;

    if Lines.Count > 0 then
    begin
      FirstLine := Lines[0];

      // Parser "GET /path HTTP/1.1" manuellement
      SpacePos := Pos(' ', FirstLine);
      if SpacePos > 0 then
      begin
        Request.Method := Copy(FirstLine, 1, SpacePos - 1);
        Delete(FirstLine, 1, SpacePos);

        SpacePos := Pos(' ', FirstLine);
        if SpacePos > 0 then
          Request.URI := Copy(FirstLine, 1, SpacePos - 1)
        else
          Request.URI := FirstLine;
      end;

      // Parser les headers (simplifié)
      // ...
    end;
  finally
    Lines.Free;
  end;
end;

end.
```

#### Utilisation du serveur web

```pascal
procedure TForm1.StartServer;
begin
  FServer := TWebServer.Create(8080, 8); // Port 8080, 8 workers

  FServer.OnRequest := @HandleHTTPRequest;
  FServer.Start;

  ShowMessage('Serveur démarré sur http://localhost:8080');
end;

procedure TForm1.HandleHTTPRequest(Request: THTTPRequest; Response: THTTPResponse);
begin
  // Gérer différentes routes
  if Request.URI = '/' then
  begin
    Response.Body := '<html><body><h1>Bienvenue!</h1></body></html>';
    Response.Headers.Add('Content-Type: text/html');
  end
  else if Request.URI = '/api/data' then
  begin
    Response.Body := '{"status":"ok","data":"valeur"}';
    Response.Headers.Add('Content-Type: application/json');
  end
  else
  begin
    Response.StatusCode := 404;
    Response.StatusText := 'Not Found';
    Response.Body := '<h1>404 - Page introuvable</h1>';
  end;
end;
```

### Exemple 3 : Traitement de données scientifiques

Calculs mathématiques intensifs en parallèle.

```pascal
unit ParallelMath;

{$mode objfpc}{$H+}
{$modeswitch anonymousfunctions}  // Nécessaire pour les procédures anonymes ci-dessous

interface

uses
  Classes, SysUtils, Math, SyncObjs;

type
  TDataRange = record
    StartIndex: Integer;
    EndIndex: Integer;
  end;

  // Type de fonction de transformation (remplace TFunc<Double, Double> de Delphi)
  TDoubleTransformFunc = function(const Value: Double): Double;

  TParallelCalculator = class
  private
    FPool: TSimpleThreadPool;
    FResults: array of Double;
    FCS: TCriticalSection;

    procedure CalculateRange(Range: TDataRange);
  public
    constructor Create(AWorkerCount: Integer);
    destructor Destroy; override;

    function ParallelSum(const Data: array of Double): Double;
    procedure ParallelTransform(var Data: array of Double;
      TransformFunc: TDoubleTransformFunc);
  end;

implementation

constructor TParallelCalculator.Create(AWorkerCount: Integer);
begin
  inherited Create;
  FPool := TSimpleThreadPool.Create(AWorkerCount);
  FCS := TCriticalSection.Create;
end;

destructor TParallelCalculator.Destroy;
begin
  FPool.Free;
  FCS.Free;
  inherited;
end;

function TParallelCalculator.ParallelSum(const Data: array of Double): Double;
var
  i, ChunkSize, WorkerCount: Integer;
  Range: TDataRange;
  PartialSums: array of Double;
begin
  WorkerCount := 4; // Ou TThread.ProcessorCount
  ChunkSize := Length(Data) div WorkerCount;

  SetLength(PartialSums, WorkerCount);
  SetLength(FResults, WorkerCount);

  // Diviser le travail
  for i := 0 to WorkerCount - 1 do
  begin
    Range.StartIndex := i * ChunkSize;

    if i = WorkerCount - 1 then
      Range.EndIndex := Length(Data) - 1
    else
      Range.EndIndex := (i + 1) * ChunkSize - 1;

    FPool.AddTask(procedure
    var
      j, idx: Integer;
      Sum: Double;
    begin
      idx := Range.StartIndex div ChunkSize;
      Sum := 0;

      for j := Range.StartIndex to Range.EndIndex do
        Sum := Sum + Data[j];

      FCS.Enter;
      try
        FResults[idx] := Sum;
      finally
        FCS.Leave;
      end;
    end);
  end;

  // Attendre la fin (simplifié)
  Sleep(1000);

  // Sommer les résultats partiels
  Result := 0;
  for i := 0 to High(FResults) do
    Result := Result + FResults[i];
end;

procedure TParallelCalculator.ParallelTransform(var Data: array of Double;
  TransformFunc: TDoubleTransformFunc);
var
  i: Integer;
begin
  // Chaque élément peut être transformé indépendamment
  for i := 0 to High(Data) do
  begin
    FPool.AddTask(procedure
    var
      Index: Integer;
    begin
      Index := i; // Capturer l'index
      Data[Index] := TransformFunc(Data[Index]);
    end);
  end;
end;

end.
```

## Debugging de thread pools

### Techniques de débogage

```pascal
type
  TDebugThreadPool = class(TSimpleThreadPool)
  private
    FLogCS: TCriticalSection;
    procedure Log(const Msg: string);
  public
    constructor Create(AWorkerCount: Integer);
    destructor Destroy; override;
  end;

constructor TDebugThreadPool.Create(AWorkerCount: Integer);
begin
  inherited;
  FLogCS := TCriticalSection.Create;
end;

destructor TDebugThreadPool.Destroy;
begin
  FLogCS.Free;
  inherited;
end;

procedure TDebugThreadPool.Log(const Msg: string);
begin
  FLogCS.Enter;
  try
    WriteLn(Format('[%s] Thread %d: %s',
      [FormatDateTime('hh:nn:ss.zzz', Now), GetCurrentThreadId, Msg]));
  finally
    FLogCS.Leave;
  end;
end;
```

### Détection de deadlocks

```pascal
uses
  SysUtils, SyncObjs, DateUtils;  // MilliSecondsBetween nécessite DateUtils

type
  TDeadlockDetector = class
  private
    FTimeout: Cardinal;
    FLastActivity: TDateTime;
    FCS: TCriticalSection;
  public
    constructor Create(ATimeoutMs: Cardinal);
    procedure RecordActivity;
    function IsDeadlocked: Boolean;
  end;

function TDeadlockDetector.IsDeadlocked: Boolean;
begin
  FCS.Enter;
  try
    Result := MilliSecondsBetween(Now, FLastActivity) > FTimeout;
  finally
    FCS.Leave;
  end;
end;
```

## Performance et optimisations

### Mesurer les performances

```pascal
uses
  SysUtils, DateUtils;  // MilliSecondsBetween nécessite DateUtils

type
  TPoolBenchmark = class
  private
    FStartTime: TDateTime;
    FEndTime: TDateTime;
    FTaskCount: Integer;
  public
    procedure Start;
    procedure Stop;
    function GetThroughput: Double; // Tâches par seconde
    function GetAverageTime: Double; // Temps moyen par tâche
  end;

function TPoolBenchmark.GetThroughput: Double;
var
  Duration: Double;
begin
  Duration := MilliSecondsBetween(FEndTime, FStartTime) / 1000;
  if Duration > 0 then
    Result := FTaskCount / Duration
  else
    Result := 0;
end;
```

### Optimisation : Lock-free queue

Pour des performances maximales, utiliser une queue sans verrous.

```pascal
// Utiliser TThreadedQueue de FCL (Free Component Library)
uses
  RTLConsts, SyncObjs;

type
  TLockFreePool = class
  private
    FQueue: TThreadedQueue<TTask>;
  public
    constructor Create(AQueueDepth: Integer);
    procedure Push(Task: TTask);
    function Pop(var Task: TTask; Timeout: Cardinal): Boolean;
  end;

constructor TLockFreePool.Create(AQueueDepth: Integer);
begin
  inherited Create;
  FQueue := TThreadedQueue<TTask>.Create(AQueueDepth,
    INFINITE, // Timeout push
    INFINITE  // Timeout pop
  );
end;

procedure TLockFreePool.Push(Task: TTask);
begin
  FQueue.PushItem(Task);
end;

function TLockFreePool.Pop(var Task: TTask; Timeout: Cardinal): Boolean;
begin
  Result := (FQueue.PopItem(Task, Timeout) = wrSignaled);
end;
```

## Ressources et références

### Documentation officielle

- **FreePascal RTL** : https://www.freepascal.org/docs-html/rtl/
- **Classes.TThread** : Documentation de la classe TThread
- **SyncObjs** : Documentation des objets de synchronisation

### Bibliothèques recommandées

- **MTProcs** : Bibliothèque de multi-threading avancé pour FreePascal
- **OmniThreadLibrary** : Thread pool avancé (portage partiel vers FPC)
- **AsyncPro** : Composants asynchrones

### Outils de profilage

**Windows :**
- Visual Studio Profiler
- Intel VTune
- Very Sleepy (gratuit)

**Ubuntu/Linux :**
- Valgrind avec Callgrind
- perf (Linux performance tools)
- gprof

### Lectures complémentaires

- "The Art of Multiprocessor Programming" - Maurice Herlihy
- "Concurrent Programming on Windows" - Joe Duffy
- Documentation POSIX threads (pthreads)

## Conclusion

Les thread pools et les workers sont des outils puissants pour:

- **Améliorer les performances** en exploitant le parallélisme
- **Contrôler l'utilisation des ressources** en limitant le nombre de threads
- **Simplifier le code** en séparant la logique métier de la gestion des threads
- **Construire des applications scalables** capables de traiter de grandes charges

Maîtriser les thread pools vous permet de créer des applications réactives et performantes, que ce soit pour du traitement de données, des serveurs réseau, ou des applications desktop modernes.

N'oubliez pas que le multi-threading ajoute de la complexité : commencez simple, testez rigoureusement, et n'optimisez que lorsque c'est nécessaire !

⏭️ [Structures de données thread-safe](/11-multithreading-concurrence/03-structures-donnees-thread-safe.md)
