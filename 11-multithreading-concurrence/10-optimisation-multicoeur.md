🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 11.10 Optimisation multicœur

## Introduction à l'optimisation multicœur

L'**optimisation multicœur** consiste à adapter votre code pour exploiter au maximum les processeurs modernes qui possèdent plusieurs cœurs. Un processeur quad-core (4 cœurs) peut théoriquement faire 4 fois plus de travail qu'un processeur simple cœur.

### Qu'est-ce qu'un cœur ?

Un **cœur** (core) est une unité de traitement indépendante dans un processeur. Chaque cœur peut exécuter des instructions en parallèle.

**Analogie** : Imaginez une cuisine :
- **Processeur simple cœur** = 1 cuisinier qui fait tout séquentiellement
- **Processeur quad-core** = 4 cuisiniers qui peuvent préparer différents plats simultanément
- **Optimisation multicœur** = Organiser les tâches pour que les 4 cuisiniers soient occupés en même temps

### Détection des cœurs disponibles

```pascal
uses
  Classes;

procedure DetectCPUCores;
var
  CoreCount: Integer;
begin
  CoreCount := TThread.ProcessorCount;

  WriteLn('=== Informations CPU ===');
  WriteLn('Nombre de cœurs logiques : ', CoreCount);

  {$IFDEF WINDOWS}
  WriteLn('Plateforme : Windows');
  {$ENDIF}

  {$IFDEF LINUX}
  WriteLn('Plateforme : Linux');
  {$ENDIF}

  // Recommandations
  if CoreCount >= 8 then
    WriteLn('Configuration : Excellente pour le parallélisme')
  else if CoreCount >= 4 then
    WriteLn('Configuration : Bonne pour le parallélisme')
  else if CoreCount >= 2 then
    WriteLn('Configuration : Suffisante pour le parallélisme basique')
  else
    WriteLn('Configuration : Mono-cœur (parallélisme limité)');
end;
```

### Loi d'Amdahl

La **loi d'Amdahl** prédit le gain de performance maximal possible avec le parallélisme.

```
Speedup = 1 / (S + P/N)

Où :
- S = Portion séquentielle (non parallélisable)
- P = Portion parallélisable
- N = Nombre de cœurs
```

**Exemple :**
```pascal
// Si 90% du code est parallélisable (S=0.1, P=0.9)
// Avec 4 cœurs :
Speedup = 1 / (0.1 + 0.9/4) = 1 / 0.325 = 3.08x

// Avec 8 cœurs :
Speedup = 1 / (0.1 + 0.9/8) = 1 / 0.2125 = 4.71x

// Speedup maximum (nombre infini de cœurs) :
Speedup_max = 1 / 0.1 = 10x
```

## Principes fondamentaux

### 1. Diviser pour régner

Décomposer un problème en sous-problèmes indépendants.

```pascal
// ❌ Séquentiel : Traiter 1000 éléments un par un
procedure ProcessSequential(Data: array of Integer);
var
  i: Integer;
begin
  for i := 0 to High(Data) do
    ProcessElement(Data[i]);
end;

// ✅ Parallèle : Diviser en 4 groupes de 250
procedure ProcessParallel(Data: array of Integer);
var
  Threads: array[0..3] of TThread;
  ChunkSize, i: Integer;
begin
  ChunkSize := Length(Data) div 4;

  for i := 0 to 3 do
  begin
    Threads[i] := TThread.CreateAnonymousThread(
      procedure
      var
        StartIdx, EndIdx, j: Integer;
      begin
        StartIdx := i * ChunkSize;
        EndIdx := StartIdx + ChunkSize - 1;
        if i = 3 then
          EndIdx := High(Data); // Dernier thread prend le reste

        for j := StartIdx to EndIdx do
          ProcessElement(Data[j]);
      end
    );
    Threads[i].Start;
  end;

  // Attendre la fin
  for i := 0 to 3 do
  begin
    Threads[i].WaitFor;
    Threads[i].Free;
  end;
end;
```

### 2. Granularité optimale

La **granularité** est la taille des tâches parallèles.

```
Granularité fine   : Beaucoup de petites tâches (overhead élevé)
Granularité moyenne: Équilibre optimal
Granularité grossière: Peu de grandes tâches (déséquilibre possible)
```

**Règle empirique :**
```pascal
OptimalChunkSize := TotalElements div (ProcessorCount * 2 à 4)
```

**Exemple :**
```pascal
procedure OptimalGranularity(Data: array of Integer);
var
  CoreCount, ChunkSize, ChunkCount: Integer;
  i: Integer;
begin
  CoreCount := TThread.ProcessorCount;

  // Créer 2-4x plus de chunks que de cœurs
  ChunkCount := CoreCount * 3;
  ChunkSize := Length(Data) div ChunkCount;

  WriteLn('Cœurs : ', CoreCount);
  WriteLn('Chunks : ', ChunkCount);
  WriteLn('Taille par chunk : ', ChunkSize);

  // Traiter en parallèle avec MTProcs
  ProcThreadPool.DoParallel(
    procedure(ChunkIndex: Integer)
    var
      StartIdx, EndIdx, j: Integer;
    begin
      StartIdx := ChunkIndex * ChunkSize;
      EndIdx := Min(StartIdx + ChunkSize - 1, High(Data));

      for j := StartIdx to EndIdx do
        ProcessElement(Data[j]);
    end,
    0, ChunkCount - 1
  );
end;
```

### 3. Minimiser la synchronisation

La synchronisation (verrous, sections critiques) limite le parallélisme.

```pascal
// ❌ MAUVAIS : Synchronisation excessive
var
  SharedSum: Integer;
  CS: TCriticalSection;

procedure BadParallel(Data: array of Integer);
begin
  ProcThreadPool.DoParallel(
    procedure(Index: Integer)
    begin
      CS.Enter; // Tous les threads attendent ici !
      try
        SharedSum := SharedSum + Data[Index];
      finally
        CS.Leave;
      end;
    end,
    0, High(Data)
  );
end;

// ✅ BON : Réduction locale puis agrégation
procedure GoodParallel(Data: array of Integer);
var
  PartialSums: array of Integer;
  i: Integer;
begin
  SetLength(PartialSums, TThread.ProcessorCount);

  ProcThreadPool.DoParallel(
    procedure(Index: Integer)
    var
      ThreadID: Integer;
    begin
      ThreadID := Index mod Length(PartialSums);

      // Pas de verrou : chaque thread a sa propre variable
      PartialSums[ThreadID] := PartialSums[ThreadID] + Data[Index];
    end,
    0, High(Data)
  );

  // Agrégation finale (séquentiel mais rapide)
  SharedSum := 0;
  for i := 0 to High(PartialSums) do
    SharedSum := SharedSum + PartialSums[i];
end;
```

## Architecture des processeurs modernes

### Cache hierarchy

Les processeurs modernes ont plusieurs niveaux de cache.

```
CPU Core 1          CPU Core 2          CPU Core 3          CPU Core 4
    |                   |                   |                   |
  L1 Cache            L1 Cache            L1 Cache            L1 Cache
   32 KB               32 KB               32 KB               32 KB
   ~4 cycles          ~4 cycles           ~4 cycles           ~4 cycles
    |                   |                   |                   |
    +-------------------+-------------------+-------------------+
                            |
                        L2 Cache (256 KB per core)
                        ~12 cycles
                            |
                        L3 Cache (shared, 8-32 MB)
                        ~40 cycles
                            |
                        RAM (8-64 GB)
                        ~200 cycles
```

### Cache line et false sharing

Une **cache line** fait typiquement 64 bytes. Le **false sharing** se produit quand deux threads modifient des variables différentes sur la même cache line.

```pascal
// ❌ MAUVAIS : False sharing
type
  TCounters = record
    Counter1: Integer; // Offset 0
    Counter2: Integer; // Offset 4
    Counter3: Integer; // Offset 8
    Counter4: Integer; // Offset 12
  end; // Tous sur la même cache line !

var
  Counters: TCounters;

// Thread 1 modifie Counter1
// Thread 2 modifie Counter2
// → Invalidations constantes de cache = LENT

// ✅ BON : Padding pour séparer les cache lines
type
  TPaddedCounter = record
    Value: Integer;
    Padding: array[0..59] of Byte; // 64 bytes total
  end;

  TPaddedCounters = record
    Counter1: TPaddedCounter;
    Counter2: TPaddedCounter;
    Counter3: TPaddedCounter;
    Counter4: TPaddedCounter;
  end;

var
  Counters: TPaddedCounters;

// Maintenant chaque compteur est sur sa propre cache line
```

### NUMA (Non-Uniform Memory Access)

Sur les systèmes multi-socket, l'accès mémoire n'est pas uniforme.

```pascal
{$IFDEF LINUX}
unit NUMAOptimization;

{$mode objfpc}{$H+}

interface

uses
  BaseUnix;

// Lier un thread à un nœud NUMA
procedure BindToNUMANode(NodeID: Integer);

// Allouer de la mémoire sur un nœud NUMA spécifique
function AllocateOnNUMANode(Size: size_t; NodeID: Integer): Pointer;

implementation

procedure BindToNUMANode(NodeID: Integer);
begin
  // Utiliser numa_run_on_node ou pthread_setaffinity_np
  // Implémentation simplifiée
  WriteLn('Binding thread to NUMA node ', NodeID);
end;

function AllocateOnNUMANode(Size: size_t; NodeID: Integer): Pointer;
begin
  // Utiliser numa_alloc_onnode
  // Implémentation simplifiée
  Result := GetMem(Size);
end;

end.
{$ENDIF}
```

## Patterns d'optimisation

### 1. Parallel Map

Appliquer une fonction à tous les éléments d'un tableau.

```pascal
unit ParallelMap;

{$mode delphi}{$H+}

interface

uses
  Classes, MTProcs;

type
  TMapFunc<T> = function(const Item: T): T;

procedure ParallelMap<T>(var Data: array of T; MapFunc: TMapFunc<T>);

implementation

procedure ParallelMap<T>(var Data: array of T; MapFunc: TMapFunc<T>);
begin
  ProcThreadPool.DoParallel(
    procedure(Index: Integer)
    begin
      Data[Index] := MapFunc(Data[Index]);
    end,
    0, High(Data)
  );
end;

end.
```

**Utilisation :**
```pascal
function Square(N: Integer): Integer;
begin
  Result := N * N;
end;

var
  Numbers: array[0..999] of Integer;
  i: Integer;
begin
  // Initialiser
  for i := 0 to High(Numbers) do
    Numbers[i] := i + 1;

  // Calculer les carrés en parallèle
  ParallelMap<Integer>(Numbers, @Square);

  WriteLn('Carré de 10 = ', Numbers[9]); // 100
end;
```

### 2. Parallel Reduce

Agréger tous les éléments d'un tableau.

```pascal
unit ParallelReduce;

{$mode delphi}{$H+}

interface

uses
  Classes, MTProcs, SyncObjs;

type
  TReduceFunc<T> = function(const A, B: T): T;

function ParallelReduce<T>(const Data: array of T;
  ReduceFunc: TReduceFunc<T>; InitValue: T): T;

implementation

function ParallelReduce<T>(const Data: array of T;
  ReduceFunc: TReduceFunc<T>; InitValue: T): T;
var
  PartialResults: array of T;
  CoreCount, i: Integer;
  CS: TCriticalSection;
begin
  CoreCount := TThread.ProcessorCount;
  SetLength(PartialResults, CoreCount);

  // Initialiser
  for i := 0 to CoreCount - 1 do
    PartialResults[i] := InitValue;

  CS := TCriticalSection.Create;
  try
    // Réduction partielle parallèle
    ProcThreadPool.DoParallel(
      procedure(Index: Integer)
      var
        ThreadID: Integer;
        LocalResult: T;
      begin
        ThreadID := Index mod CoreCount;
        LocalResult := Data[Index];

        CS.Enter;
        try
          PartialResults[ThreadID] := ReduceFunc(PartialResults[ThreadID], LocalResult);
        finally
          CS.Leave;
        end;
      end,
      0, High(Data)
    );

    // Réduction finale
    Result := PartialResults[0];
    for i := 1 to High(PartialResults) do
      Result := ReduceFunc(Result, PartialResults[i]);
  finally
    CS.Free;
  end;
end;

end.
```

**Utilisation :**
```pascal
function Add(const A, B: Integer): Integer;
begin
  Result := A + B;
end;

var
  Numbers: array[0..999] of Integer;
  Sum: Integer;
  i: Integer;
begin
  // Initialiser
  for i := 0 to High(Numbers) do
    Numbers[i] := i + 1;

  // Somme en parallèle
  Sum := ParallelReduce<Integer>(Numbers, @Add, 0);

  WriteLn('Somme de 1 à 1000 = ', Sum); // 500500
end;
```

### 3. Parallel Sort

Tri rapide parallèle.

```pascal
unit ParallelQuickSort;

{$mode objfpc}{$H+}
{$modeswitch anonymousfunctions}

interface

uses
  Classes, MTProcs;

procedure ParallelQuickSort(var Data: array of Integer; Left, Right: Integer);

implementation

const
  PARALLEL_THRESHOLD = 1000; // En dessous, tri séquentiel

procedure SequentialQuickSort(var Data: array of Integer; Left, Right: Integer);
var
  Pivot, Temp, i, j: Integer;
begin
  if Left >= Right then
    Exit;

  Pivot := Data[(Left + Right) div 2];
  i := Left;
  j := Right;

  while i <= j do
  begin
    while Data[i] < Pivot do Inc(i);
    while Data[j] > Pivot do Dec(j);

    if i <= j then
    begin
      Temp := Data[i];
      Data[i] := Data[j];
      Data[j] := Temp;
      Inc(i);
      Dec(j);
    end;
  end;

  if Left < j then
    SequentialQuickSort(Data, Left, j);
  if i < Right then
    SequentialQuickSort(Data, i, Right);
end;

procedure ParallelQuickSort(var Data: array of Integer; Left, Right: Integer);
var
  Pivot, Temp, i, j: Integer;
begin
  if Left >= Right then
    Exit;

  // Si trop petit, tri séquentiel
  if (Right - Left) < PARALLEL_THRESHOLD then
  begin
    SequentialQuickSort(Data, Left, Right);
    Exit;
  end;

  // Partition
  Pivot := Data[(Left + Right) div 2];
  i := Left;
  j := Right;

  while i <= j do
  begin
    while Data[i] < Pivot do Inc(i);
    while Data[j] > Pivot do Dec(j);

    if i <= j then
    begin
      Temp := Data[i];
      Data[i] := Data[j];
      Data[j] := Temp;
      Inc(i);
      Dec(j);
    end;
  end;

  // Trier les partitions en parallèle
  ProcThreadPool.DoParallelLocalProc(
    procedure
    begin
      if Left < j then
        ParallelQuickSort(Data, Left, j);
    end,
    procedure
    begin
      if i < Right then
        ParallelQuickSort(Data, i, Right);
    end
  );
end;

end.
```

### 4. Work Stealing

Répartition dynamique du travail entre les threads.

```pascal
> **Note :** Les unités utilisant des types génériques (`TThreadedQueue<T>`, etc.) de `Generics.Collections` sont compilées en `{$mode delphi}` pour simplifier la syntaxe. Les unités utilisant des procédures anonymes avec `ProcThreadPool.DoParallel` nécessitent `{$modeswitch anonymousfunctions}` en `{$mode objfpc}` (FPC 3.3.1+).

unit WorkStealing;

{$mode delphi}{$H+}

interface

uses
  Classes, SyncObjs, Generics.Collections;

type
  TWorkItem = procedure;

  TWorkQueue = class
  private
    FQueue: TThreadedQueue<TWorkItem>;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Push(Item: TWorkItem);
    function Pop(out Item: TWorkItem): Boolean;
    function TrySteal(out Item: TWorkItem): Boolean;
    function IsEmpty: Boolean;
  end;

  TWorkStealingPool = class
  private
    FQueues: array of TWorkQueue;
    FWorkers: array of TThread;
    FRunning: Boolean;

    procedure WorkerProc(WorkerID: Integer);
  public
    constructor Create(WorkerCount: Integer);
    destructor Destroy; override;

    procedure Submit(Work: TWorkItem);
    procedure Start;
    procedure Stop;
  end;

implementation

{ TWorkQueue }

constructor TWorkQueue.Create;
begin
  inherited Create;
  FQueue := TThreadedQueue<TWorkItem>.Create(1000, INFINITE, INFINITE);
end;

destructor TWorkQueue.Destroy;
begin
  FQueue.Free;
  inherited;
end;

procedure TWorkQueue.Push(Item: TWorkItem);
begin
  FQueue.PushItem(Item);
end;

function TWorkQueue.Pop(out Item: TWorkItem): Boolean;
begin
  Result := FQueue.PopItem(Item, 0) = wrSignaled;
end;

function TWorkQueue.TrySteal(out Item: TWorkItem): Boolean;
begin
  // Dans une vraie implémentation, on volerait par la fin
  Result := Pop(Item);
end;

function TWorkQueue.IsEmpty: Boolean;
begin
  Result := FQueue.TotalItemsPushed = FQueue.TotalItemsPopped;
end;

{ TWorkStealingPool }

constructor TWorkStealingPool.Create(WorkerCount: Integer);
var
  i: Integer;
begin
  inherited Create;

  SetLength(FQueues, WorkerCount);
  SetLength(FWorkers, WorkerCount);

  for i := 0 to WorkerCount - 1 do
    FQueues[i] := TWorkQueue.Create;

  FRunning := False;
end;

destructor TWorkStealingPool.Destroy;
var
  i: Integer;
begin
  Stop;

  for i := 0 to High(FQueues) do
    FQueues[i].Free;

  inherited;
end;

procedure TWorkStealingPool.WorkerProc(WorkerID: Integer);
var
  Work: TWorkItem;
  i, VictimID: Integer;
begin
  while FRunning do
  begin
    // 1. Essayer sa propre queue
    if FQueues[WorkerID].Pop(Work) then
    begin
      Work();
      Continue;
    end;

    // 2. Essayer de voler du travail
    for i := 1 to High(FQueues) do
    begin
      VictimID := (WorkerID + i) mod Length(FQueues);
      if FQueues[VictimID].TrySteal(Work) then
      begin
        Work();
        Break;
      end;
    end;

    // 3. Attendre un peu
    Sleep(1);
  end;
end;

procedure TWorkStealingPool.Submit(Work: TWorkItem);
var
  TargetQueue: Integer;
begin
  // Round-robin simple
  TargetQueue := Random(Length(FQueues));
  FQueues[TargetQueue].Push(Work);
end;

procedure TWorkStealingPool.Start;
var
  i: Integer;

  // Fonction locale pour capturer la valeur de i correctement
  function CreateWorkerThread(WorkerID: Integer): TThread;
  begin
    Result := TThread.CreateAnonymousThread(
      procedure
      begin
        WorkerProc(WorkerID);
      end
    );
    Result.FreeOnTerminate := False; // Nécessaire pour WaitFor+Free dans Stop
  end;

begin
  FRunning := True;

  for i := 0 to High(FWorkers) do
  begin
    FWorkers[i] := CreateWorkerThread(i);
    FWorkers[i].Start;
  end;
end;

procedure TWorkStealingPool.Stop;
var
  i: Integer;
begin
  FRunning := False;

  for i := 0 to High(FWorkers) do
  begin
    if Assigned(FWorkers[i]) then
    begin
      FWorkers[i].WaitFor;
      FWorkers[i].Free;
    end;
  end;
end;

end.
```

## Techniques avancées

### 1. SIMD (Single Instruction Multiple Data)

Traiter plusieurs données avec une seule instruction.

```pascal
{$mode objfpc}{$H+}

// Activer les optimisations vectorielles
{$OPTIMIZATION LEVEL3}
{$OPTIMIZATION AUTOVECTORIZATION}

procedure VectorAdd(const A, B: array of Single; var Result: array of Single);
var
  i: Integer;
begin
  // Le compilateur peut vectoriser cette boucle
  for i := 0 to High(A) do
    Result[i] := A[i] + B[i];
end;

// Version manuelle avec assembleur inline (x64)
procedure VectorAddSSE(const A, B: array of Single; var Result: array of Single);
var
  i: Integer;
begin
  {$IFDEF CPUX64}
  // Utiliser les instructions SSE
  asm
    // Code assembleur pour SSE
  end;
  {$ELSE}
  // Fallback standard
  for i := 0 to High(A) do
    Result[i] := A[i] + B[i];
  {$ENDIF}
end;
```

### 2. Loop tiling (Blocking)

Optimiser pour le cache en traitant les données par blocs.

```pascal
procedure MatrixMultiplyTiled(const A, B: array of array of Double;
  var C: array of array of Double; N, BlockSize: Integer);
var
  i, j, k, ii, jj, kk: Integer;
  Sum: Double;
begin
  // Traiter par blocs de BlockSize x BlockSize
  for ii := 0 to (N - 1) div BlockSize do
    for jj := 0 to (N - 1) div BlockSize do
      for kk := 0 to (N - 1) div BlockSize do
      begin
        // Traiter un bloc
        for i := ii * BlockSize to Min((ii + 1) * BlockSize - 1, N - 1) do
          for j := jj * BlockSize to Min((jj + 1) * BlockSize - 1, N - 1) do
          begin
            Sum := C[i][j];
            for k := kk * BlockSize to Min((kk + 1) * BlockSize - 1, N - 1) do
              Sum := Sum + A[i][k] * B[k][j];
            C[i][j] := Sum;
          end;
      end;
end;
```

### 3. Prefetching

Pré-charger les données dans le cache avant utilisation.

```pascal
procedure ProcessWithPrefetch(Data: PInteger; Count: Integer);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    // Précharger les données futures
    {$IFDEF CPUX64}
    if i + 8 < Count then
    begin
      asm
        mov rax, Data
        add rax, (i + 8) * 4
        prefetchnta [rax]
      end;
    end;
    {$ENDIF}

    // Traiter les données actuelles
    ProcessElement(Data[i]);
  end;
end;
```

## Mesure et profilage

### Speedup measurement

```pascal
unit SpeedupMeasurement;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils;

type
  TSpeedupResult = record
    SequentialTime: Double;
    ParallelTime: Double;
    Speedup: Double;
    Efficiency: Double;
    Cores: Integer;
  end;

function MeasureSpeedup(SequentialProc, ParallelProc: TProcedure): TSpeedupResult;

implementation

function MeasureSpeedup(SequentialProc, ParallelProc: TProcedure): TSpeedupResult;
var
  StartTime: TDateTime;
begin
  Result.Cores := TThread.ProcessorCount;

  // Mesurer version séquentielle
  WriteLn('Exécution séquentielle...');
  StartTime := Now;
  SequentialProc();
  Result.SequentialTime := MilliSecondsBetween(Now, StartTime);

  // Mesurer version parallèle
  WriteLn('Exécution parallèle...');
  StartTime := Now;
  ParallelProc();
  Result.ParallelTime := MilliSecondsBetween(Now, StartTime);

  // Calculer métriques
  Result.Speedup := Result.SequentialTime / Result.ParallelTime;
  Result.Efficiency := Result.Speedup / Result.Cores;

  WriteLn;
  WriteLn('=== Résultats ===');
  WriteLn('Cœurs utilisés : ', Result.Cores);
  WriteLn('Temps séquentiel : ', Result.SequentialTime:0:2, ' ms');
  WriteLn('Temps parallèle : ', Result.ParallelTime:0:2, ' ms');
  WriteLn('Speedup : ', Result.Speedup:0:2, 'x');
  WriteLn('Efficacité : ', (Result.Efficiency * 100):0:1, '%');

  // Interprétation
  WriteLn;
  if Result.Speedup >= Result.Cores * 0.8 then
    WriteLn('Excellent : Speedup proche de l''idéal')
  else if Result.Speedup >= Result.Cores * 0.5 then
    WriteLn('Bon : Speedup correct')
  else if Result.Speedup >= 2.0 then
    WriteLn('Acceptable : Gain modéré')
  else
    WriteLn('Faible : Parallélisation peu efficace');
end;

end.
```

### Détection des goulots d'étranglement

```pascal
unit BottleneckDetection;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils;

type
  TBottleneckReport = record
    TotalTime: Double;
    SyncTime: Double;
    ComputeTime: Double;
    IdleTime: Double;
  end;

procedure DetectBottlenecks(var Report: TBottleneckReport);

implementation

var
  SyncCounter: Int64 = 0;
  ComputeCounter: Int64 = 0;

procedure DetectBottlenecks(var Report: TBottleneckReport);
begin
  Report.TotalTime := 1000.0; // Example
  Report.SyncTime := (SyncCounter / Report.TotalTime) * 100;
  Report.ComputeTime := (ComputeCounter / Report.TotalTime) * 100;
  Report.IdleTime := 100 - Report.SyncTime - Report.ComputeTime;

  WriteLn('=== Analyse des goulots ===');
  WriteLn('Temps de synchronisation : ', Report.SyncTime:0:1, '%');
  WriteLn('Temps de calcul : ', Report.ComputeTime:0:1, '%');
  WriteLn('Temps d''inactivité : ', Report.IdleTime:0:1, '%');

  if Report.SyncTime > 20 then
    WriteLn('⚠ Trop de synchronisation - Réduire les verrous')
  else if Report.IdleTime > 20 then
    WriteLn('⚠ Threads inactifs - Améliorer la répartition de charge')
  else
    WriteLn('✓ Bon équilibre');
end;

end.
```

## Différences Windows/Linux

### Affinité CPU

```pascal
unit CPUAffinity;

{$mode objfpc}{$H+}

interface

uses
  Classes
  {$IFDEF WINDOWS}, Windows{$ENDIF}
  {$IFDEF LINUX}, BaseUnix, Unix{$ENDIF};

procedure SetThreadAffinity(ThreadHandle: TThreadID; CoreMask: NativeUInt);
procedure PinToCorePortable(CoreNumber: Integer);

implementation

procedure SetThreadAffinity(ThreadHandle: TThreadID; CoreMask: NativeUInt);
begin
  {$IFDEF WINDOWS}
  SetThreadAffinityMask(ThreadHandle, CoreMask);
  {$ENDIF}

  {$IFDEF LINUX}
  // Utiliser sched_setaffinity
  // Implémentation avec cpu_set_t
  {$ENDIF}
end;

procedure PinToCorePortable(CoreNumber: Integer);
var
  Mask: NativeUInt;
begin
  Mask := 1 shl CoreNumber;

  {$IFDEF WINDOWS}
  SetThreadAffinityMask(GetCurrentThread, Mask);
  WriteLn('Thread lié au cœur ', CoreNumber, ' (Windows)');
  {$ENDIF}

  {$IFDEF LINUX}
  // Code Linux pour sched_setaffinity
  WriteLn('Thread lié au cœur ', CoreNumber, ' (Linux)');
  {$ENDIF}
end;

end.
```

### Topologie CPU

```pascal
unit CPUTopology;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TCPUInfo = record
    PhysicalCores: Integer;
    LogicalCores: Integer;
    HyperThreading: Boolean;
    CacheLine: Integer;
    L1Cache: Integer;
    L2Cache: Integer;
    L3Cache: Integer;
  end;

function GetCPUInfo: TCPUInfo;
procedure PrintCPUTopology;

implementation

function GetCPUInfo: TCPUInfo;
begin
  Result.LogicalCores := TThread.ProcessorCount;
  Result.CacheLine := 64; // Typique

  {$IFDEF WINDOWS}
  // Utiliser GetLogicalProcessorInformation
  Result.PhysicalCores := Result.LogicalCores; // Simplifié
  {$ENDIF}

  {$IFDEF LINUX}
  // Lire /proc/cpuinfo
  Result.PhysicalCores := Result.LogicalCores; // Simplifié
  {$ENDIF}

  Result.HyperThreading := Result.LogicalCores > Result.PhysicalCores;

  // Valeurs typiques (nécessiterait une vraie détection)
  Result.L1Cache := 32 * 1024;      // 32 KB
  Result.L2Cache := 256 * 1024;     // 256 KB
  Result.L3Cache := 8 * 1024 * 1024; // 8 MB
end;

procedure PrintCPUTopology;
var
  Info: TCPUInfo;
begin
  Info := GetCPUInfo;

  WriteLn('=== Topologie CPU ===');
  WriteLn('Cœurs physiques : ', Info.PhysicalCores);
  WriteLn('Cœurs logiques : ', Info.LogicalCores);
  WriteLn('Hyper-Threading : ', BoolToStr(Info.HyperThreading, True));
  WriteLn('Taille cache line : ', Info.CacheLine, ' bytes');
  WriteLn('L1 Cache : ', Info.L1Cache div 1024, ' KB');
  WriteLn('L2 Cache : ', Info.L2Cache div 1024, ' KB');
  WriteLn('L3 Cache : ', Info.L3Cache div (1024 * 1024), ' MB');
end;

end.
```

### Performance Windows vs Linux

```pascal
procedure ComparePerformance;
var
  StartTime: TDateTime;
  i, Sum: Integer;
begin
  WriteLn('=== Comparaison de performance ===');

  {$IFDEF WINDOWS}
  WriteLn('Plateforme : Windows');
  WriteLn('Ordonnanceur : Priorités multiples');
  {$ENDIF}

  {$IFDEF LINUX}
  WriteLn('Plateforme : Linux');
  WriteLn('Ordonnanceur : CFS (Completely Fair Scheduler)');
  {$ENDIF}

  // Test simple
  StartTime := Now;
  Sum := 0;

  ProcThreadPool.DoParallel(
    procedure(Index: Integer)
    var
      j, LocalSum: Integer;
    begin
      LocalSum := 0;
      for j := 1 to 1000000 do
        LocalSum := LocalSum + j;
      InterlockedExchangeAdd(Sum, LocalSum);
    end,
    0, TThread.ProcessorCount - 1
  );

  WriteLn('Temps : ', MilliSecondsBetween(Now, StartTime), ' ms');

  {$IFDEF WINDOWS}
  WriteLn('Note : Windows optimisé pour desktop/interactivité');
  {$ENDIF}

  {$IFDEF LINUX}
  WriteLn('Note : Linux optimisé pour serveurs/débit');
  {$ENDIF}
end;
```

## Cas pratiques d'optimisation

### 1. Traitement d'images

```pascal
unit ImageProcessingMulticore;

{$mode objfpc}{$H+}
{$modeswitch anonymousfunctions}

interface

uses
  Classes, SysUtils, Math, Graphics, MTProcs;

type
  TRGB = record
    R, G, B: Byte;
  end;

  TImageData = array of array of TRGB;

procedure ApplyFilterParallel(var Image: TImageData);
procedure ConvertToGrayscaleParallel(var Image: TImageData);
procedure BlurParallel(var Image: TImageData; Radius: Integer);

implementation

procedure ApplyFilterParallel(var Image: TImageData);
var
  Height: Integer;
begin
  Height := Length(Image);

  ProcThreadPool.DoParallel(
    procedure(Y: Integer)
    var
      X: Integer;
    begin
      for X := 0 to High(Image[Y]) do
      begin
        // Appliquer un filtre (exemple : augmenter le contraste)
        Image[Y][X].R := Min(255, Image[Y][X].R * 2);
        Image[Y][X].G := Min(255, Image[Y][X].G * 2);
        Image[Y][X].B := Min(255, Image[Y][X].B * 2);
      end;
    end,
    0, Height - 1
  );
end;

procedure ConvertToGrayscaleParallel(var Image: TImageData);
var
  Height: Integer;
begin
  Height := Length(Image);

  ProcThreadPool.DoParallel(
    procedure(Y: Integer)
    var
      X, Gray: Integer;
    begin
      for X := 0 to High(Image[Y]) do
      begin
        // Formule de luminance
        Gray := Round(0.299 * Image[Y][X].R +
                     0.587 * Image[Y][X].G +
                     0.114 * Image[Y][X].B);

        Image[Y][X].R := Gray;
        Image[Y][X].G := Gray;
        Image[Y][X].B := Gray;
      end;
    end,
    0, Height - 1
  );
end;

procedure BlurParallel(var Image: TImageData; Radius: Integer);
var
  Height, Width: Integer;
  Temp: TImageData;
begin
  Height := Length(Image);
  Width := Length(Image[0]);

  // Copier l'image temporaire
  SetLength(Temp, Height, Width);

  ProcThreadPool.DoParallel(
    procedure(Y: Integer)
    var
      X, DX, DY, Count: Integer;
      SumR, SumG, SumB: Integer;
    begin
      for X := 0 to Width - 1 do
      begin
        SumR := 0; SumG := 0; SumB := 0; Count := 0;

        // Moyenner les pixels voisins
        for DY := -Radius to Radius do
          for DX := -Radius to Radius do
          begin
            if (Y + DY >= 0) and (Y + DY < Height) and
               (X + DX >= 0) and (X + DX < Width) then
            begin
              SumR := SumR + Image[Y + DY][X + DX].R;
              SumG := SumG + Image[Y + DY][X + DX].G;
              SumB := SumB + Image[Y + DY][X + DX].B;
              Inc(Count);
            end;
          end;

        Temp[Y][X].R := SumR div Count;
        Temp[Y][X].G := SumG div Count;
        Temp[Y][X].B := SumB div Count;
      end;
    end,
    0, Height - 1
  );

  // Copier le résultat
  Image := Temp;
end;

end.
```

### 2. Multiplication de matrices

```pascal
unit MatrixMultiplication;

{$mode objfpc}{$H+}
{$modeswitch anonymousfunctions}

interface

uses
  Classes, SysUtils, Math, MTProcs;

type
  TMatrix = array of array of Double;

procedure MultiplyMatricesParallel(const A, B: TMatrix; var C: TMatrix);
procedure MultiplyMatricesTiled(const A, B: TMatrix; var C: TMatrix; BlockSize: Integer);

implementation

procedure MultiplyMatricesParallel(const A, B: TMatrix; var C: TMatrix);
var
  N: Integer;
begin
  N := Length(A);
  SetLength(C, N, N);

  ProcThreadPool.DoParallel(
    procedure(i: Integer)
    var
      j, k: Integer;
      Sum: Double;
    begin
      for j := 0 to N - 1 do
      begin
        Sum := 0;
        for k := 0 to N - 1 do
          Sum := Sum + A[i][k] * B[k][j];
        C[i][j] := Sum;
      end;
    end,
    0, N - 1
  );
end;

procedure MultiplyMatricesTiled(const A, B: TMatrix; var C: TMatrix; BlockSize: Integer);
var
  N: Integer;
begin
  N := Length(A);
  SetLength(C, N, N);

  // Initialiser C à zéro
  ProcThreadPool.DoParallel(
    procedure(i: Integer)
    var
      j: Integer;
    begin
      for j := 0 to N - 1 do
        C[i][j] := 0;
    end,
    0, N - 1
  );

  // Multiplication par blocs (cache-friendly)
  ProcThreadPool.DoParallel(
    procedure(BlockRow: Integer)
    var
      BlockCol, BlockK: Integer;
      i, j, k: Integer;
      iStart, iEnd, jStart, jEnd, kStart, kEnd: Integer;
      Sum: Double;
    begin
      iStart := BlockRow * BlockSize;
      iEnd := Min(iStart + BlockSize - 1, N - 1);

      for BlockCol := 0 to (N - 1) div BlockSize do
      begin
        jStart := BlockCol * BlockSize;
        jEnd := Min(jStart + BlockSize - 1, N - 1);

        for BlockK := 0 to (N - 1) div BlockSize do
        begin
          kStart := BlockK * BlockSize;
          kEnd := Min(kStart + BlockSize - 1, N - 1);

          // Multiplier le bloc
          for i := iStart to iEnd do
            for j := jStart to jEnd do
            begin
              Sum := C[i][j];
              for k := kStart to kEnd do
                Sum := Sum + A[i][k] * B[k][j];
              C[i][j] := Sum;
            end;
        end;
      end;
    end,
    0, (N - 1) div BlockSize
  );
end;

end.
```

### 3. Recherche parallèle

```pascal
unit ParallelSearch;

{$mode objfpc}{$H+}
{$modeswitch anonymousfunctions}

interface

uses
  Classes, SysUtils, Math, MTProcs, SyncObjs;

function ParallelLinearSearch(const Data: array of Integer;
  Target: Integer): Integer;

function ParallelBinarySearch(const Data: array of Integer;
  Target: Integer): Integer;

implementation

function ParallelLinearSearch(const Data: array of Integer;
  Target: Integer): Integer;
var
  Found: Boolean;
  FoundIndex: Integer;
  CS: TCriticalSection;
begin
  Found := False;
  FoundIndex := -1;
  CS := TCriticalSection.Create;

  try
    ProcThreadPool.DoParallel(
      procedure(Index: Integer)
      begin
        if Found then
          Exit; // Un autre thread a déjà trouvé

        if Data[Index] = Target then
        begin
          CS.Enter;
          try
            if not Found then
            begin
              Found := True;
              FoundIndex := Index;
            end;
          finally
            CS.Leave;
          end;
        end;
      end,
      0, High(Data)
    );

    Result := FoundIndex;
  finally
    CS.Free;
  end;
end;

function ParallelBinarySearch(const Data: array of Integer;
  Target: Integer): Integer;
var
  CoreCount, ChunkSize: Integer;
  Results: array of Integer;
  i: Integer;
begin
  CoreCount := TThread.ProcessorCount;
  ChunkSize := (Length(Data) + CoreCount - 1) div CoreCount;
  SetLength(Results, CoreCount);

  // Initialiser les résultats
  for i := 0 to CoreCount - 1 do
    Results[i] := -1;

  // Recherche binaire dans chaque chunk
  ProcThreadPool.DoParallel(
    procedure(ChunkIndex: Integer)
    var
      Left, Right, Mid: Integer;
    begin
      Left := ChunkIndex * ChunkSize;
      Right := Min(Left + ChunkSize - 1, High(Data));

      // Recherche binaire standard
      while Left <= Right do
      begin
        Mid := (Left + Right) div 2;

        if Data[Mid] = Target then
        begin
          Results[ChunkIndex] := Mid;
          Exit;
        end
        else if Data[Mid] < Target then
          Left := Mid + 1
        else
          Right := Mid - 1;
      end;
    end,
    0, CoreCount - 1
  );

  // Trouver le premier résultat trouvé
  Result := -1;
  for i := 0 to High(Results) do
    if Results[i] >= 0 then
    begin
      Result := Results[i];
      Break;
    end;
end;

end.
```

### 4. Compression parallèle

```pascal
unit ParallelCompression;

{$mode objfpc}{$H+}
{$modeswitch anonymousfunctions}

interface

uses
  Classes, SysUtils, MTProcs, zstream;

type
  TCompressedChunk = record
    Data: TBytes;
    OriginalSize: Integer;
    CompressedSize: Integer;
  end;

function CompressDataParallel(const Data: TBytes;
  ChunkSize: Integer): array of TCompressedChunk;

function DecompressDataParallel(const Chunks: array of TCompressedChunk): TBytes;

implementation

function CompressDataParallel(const Data: TBytes;
  ChunkSize: Integer): array of TCompressedChunk;
var
  ChunkCount, i: Integer;
begin
  ChunkCount := (Length(Data) + ChunkSize - 1) div ChunkSize;
  SetLength(Result, ChunkCount);

  ProcThreadPool.DoParallel(
    procedure(Index: Integer)
    var
      StartIdx, EndIdx, Size: Integer;
      Input, Output: TMemoryStream;
      Compressor: TCompressionStream;
    begin
      StartIdx := Index * ChunkSize;
      EndIdx := Min(StartIdx + ChunkSize - 1, High(Data));
      Size := EndIdx - StartIdx + 1;

      Input := TMemoryStream.Create;
      Output := TMemoryStream.Create;
      try
        // Écrire les données d'entrée
        Input.Write(Data[StartIdx], Size);
        Input.Position := 0;

        // Comprimer
        Compressor := TCompressionStream.Create(clDefault, Output);
        try
          Compressor.CopyFrom(Input, Size);
        finally
          Compressor.Free;
        end;

        // Sauvegarder le résultat
        Result[Index].OriginalSize := Size;
        Result[Index].CompressedSize := Output.Size;
        SetLength(Result[Index].Data, Output.Size);
        Output.Position := 0;
        Output.Read(Result[Index].Data[0], Output.Size);
      finally
        Output.Free;
        Input.Free;
      end;
    end,
    0, ChunkCount - 1
  );
end;

function DecompressDataParallel(const Chunks: array of TCompressedChunk): TBytes;
var
  TotalSize, i, Offset: Integer;
  DecompressedChunks: array of TBytes;
begin
  SetLength(DecompressedChunks, Length(Chunks));

  // Calculer la taille totale
  TotalSize := 0;
  for i := 0 to High(Chunks) do
    TotalSize := TotalSize + Chunks[i].OriginalSize;

  // Décompresser en parallèle
  ProcThreadPool.DoParallel(
    procedure(Index: Integer)
    var
      Input, Output: TMemoryStream;
      Decompressor: TDecompressionStream;
    begin
      Input := TMemoryStream.Create;
      Output := TMemoryStream.Create;
      try
        // Écrire les données compressées
        Input.Write(Chunks[Index].Data[0], Chunks[Index].CompressedSize);
        Input.Position := 0;

        // Décompresser
        Decompressor := TDecompressionStream.Create(Input);
        try
          Output.CopyFrom(Decompressor, Chunks[Index].OriginalSize);
        finally
          Decompressor.Free;
        end;

        // Sauvegarder le résultat
        SetLength(DecompressedChunks[Index], Chunks[Index].OriginalSize);
        Output.Position := 0;
        Output.Read(DecompressedChunks[Index][0], Chunks[Index].OriginalSize);
      finally
        Output.Free;
        Input.Free;
      end;
    end,
    0, High(Chunks)
  );

  // Fusionner les chunks
  SetLength(Result, TotalSize);
  Offset := 0;
  for i := 0 to High(DecompressedChunks) do
  begin
    Move(DecompressedChunks[i][0], Result[Offset], Length(DecompressedChunks[i]));
    Offset := Offset + Length(DecompressedChunks[i]);
  end;
end;

end.
```

## Checklist d'optimisation

### ✅ Avant d'optimiser

1. **Profiler d'abord**
   - Identifier les vraies goulots d'étranglement
   - Mesurer les performances de base
   - Ne pas optimiser prématurément

2. **Vérifier la parallélisabilité**
   - Y a-t-il des dépendances entre les itérations ?
   - Quelle est la portion parallélisable (loi d'Amdahl) ?
   - La granularité est-elle appropriée ?

3. **Estimer le gain potentiel**
   - Speedup théorique maximal
   - Overhead du parallélisme
   - Coût/bénéfice de l'optimisation

### ✅ Pendant l'optimisation

1. **Choisir la bonne stratégie**
   - Data parallelism (MTProcs, parallel for)
   - Task parallelism (thread pool, acteurs)
   - Pipeline (coroutines, producteur-consommateur)

2. **Optimiser pour le cache**
   - Éviter le false sharing (padding)
   - Utiliser le loop tiling si nécessaire
   - Localité des données

3. **Minimiser la synchronisation**
   - Réduction locale puis agrégation
   - Structures lock-free si possible
   - Éviter les verrous dans les boucles serrées

### ✅ Après l'optimisation

1. **Mesurer le speedup réel**
   - Comparer avec la version séquentielle
   - Vérifier l'efficacité (speedup / cores)
   - Tester avec différents nombres de threads

2. **Valider la correction**
   - Tests unitaires
   - Vérifier l'absence de race conditions
   - Tests avec ThreadSanitizer si disponible

3. **Documenter**
   - Expliquer la stratégie choisie
   - Noter les compromis
   - Indiquer les limitations

## Pièges courants

### ❌ Piège 1 : Overhead > Gain

```pascal
// Trop de parallélisation pour un travail léger
ProcThreadPool.DoParallel(
  procedure(Index: Integer)
  begin
    Data[Index] := Data[Index] * 2; // Trop simple !
  end,
  0, 99 // Seulement 100 éléments
);

// Solution : Traiter par blocs ou séquentiellement
```

### ❌ Piège 2 : False sharing

```pascal
// Variables adjacentes modifiées par différents threads
type
  TCounters = record
    Counter1: Integer; // Cache line 0
    Counter2: Integer; // Cache line 0 aussi !
  end;

// Solution : Padding de 64 bytes entre les variables
```

### ❌ Piège 3 : Déséquilibre de charge

```pascal
// Tâches de durées très variables
for i := 0 to 3 do
  Threads[i] := TThread.Create(...);

// Thread 1 : 1 seconde
// Thread 2 : 10 secondes  ← Goulot !
// Thread 3 : 2 secondes
// Thread 4 : 1 seconde
// Temps total : 10 secondes (au lieu de 3.5)

// Solution : Work stealing ou plus de chunks
```

### ❌ Piège 4 : Trop de threads

```pascal
// Créer 1000 threads sur une machine 4 cœurs
SetLength(Threads, 1000); // Overhead énorme !

// Solution : Limiter au nombre de cœurs
ThreadCount := TThread.ProcessorCount;
```

## Bonnes pratiques finales

### 1. Utiliser des bibliothèques testées

```pascal
// ✅ Utiliser MTProcs plutôt que gérer manuellement
uses
  MTProcs;

ProcThreadPool.DoParallel(@ProcessElement, 0, High(Data));
```

### 2. Scalabilité avant performance absolue

```pascal
// Viser un bon speedup sur différentes configurations
// 2 cœurs : 1.8x
// 4 cœurs : 3.5x
// 8 cœurs : 6.5x
// Plutôt qu'optimiser pour une seule machine
```

### 3. Mesurer sur du matériel réel

```pascal
// Tester sur :
// - Machine de développement (8 cœurs)
// - Machine de production (32 cœurs)
// - Machine bas de gamme (2 cœurs)
```

### 4. Documenter les choix

```pascal
{
  Optimisation multicœur - Traitement d'images

  STRATÉGIE :
  - Parallélisation par ligne (data parallelism)
  - Utilisation de MTProcs pour la gestion automatique
  - Padding pour éviter le false sharing

  PERFORMANCES :
  - Séquentiel : 450 ms
  - Parallèle (4 cœurs) : 125 ms (speedup 3.6x)
  - Efficacité : 90%

  LIMITATIONS :
  - Nécessite au moins 2 cœurs pour un gain
  - Images < 1000x1000 : overhead non négligeable
}
```

## Résumé

L'**optimisation multicœur** permet d'exploiter la puissance des processeurs modernes :

**Principes clés :**
- **Diviser pour régner** : Décomposer en tâches indépendantes
- **Granularité optimale** : Équilibrer overhead et parallélisme
- **Minimiser la synchronisation** : Réduction locale puis agrégation
- **Cache-friendly** : Éviter le false sharing, utiliser le tiling

**Techniques essentielles :**
- Parallel Map/Reduce
- Work Stealing
- Loop tiling
- SIMD (quand possible)

**Outils :**
- **MTProcs** : Bibliothèque parallèle pour FreePascal
- **TThread.ProcessorCount** : Détecter les cœurs
- **Profilers** : Identifier les goulots

**Checklist :**
1. ✅ Profiler avant d'optimiser
2. ✅ Vérifier la parallélisabilité (loi d'Amdahl)
3. ✅ Choisir la bonne granularité
4. ✅ Minimiser la synchronisation
5. ✅ Optimiser pour le cache
6. ✅ Mesurer le speedup réel
7. ✅ Valider la correction
8. ✅ Documenter

**Règle d'or :** Ne pas optimiser prématurément. Mesurer d'abord, optimiser ensuite, valider toujours !

L'optimisation multicœur bien faite peut multiplier les performances par 4-8x sur du matériel moderne !

⏭️ [Affinité processeur par OS](/11-multithreading-concurrence/11-affinite-processeur-par-os.md)
