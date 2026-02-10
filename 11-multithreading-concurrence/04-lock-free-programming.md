🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 11.4 Lock-free programming

## Introduction au lock-free

Le **lock-free programming** (programmation sans verrous) est une technique avancée de programmation concurrente qui permet à plusieurs threads d'accéder à des données partagées sans utiliser de mutex ou de critical sections traditionnels.

### Qu'est-ce que le lock-free ?

**Lock-free** signifie que les algorithmes garantissent qu'au moins un thread progresse, même si d'autres threads sont suspendus ou retardés. Aucun thread ne peut bloquer les autres indéfiniment.

**Analogie** : Imaginez une bibliothèque où, au lieu d'avoir une seule porte avec un verrou (critical section), chaque personne peut essayer de prendre un livre simultanément. Si deux personnes veulent le même livre, l'une d'elles réussit immédiatement et l'autre réessaie avec un autre livre. Personne n'attend devant une porte fermée.

### Pourquoi utiliser le lock-free ?

**Avantages :**
- **Performance** : Pas d'attente sur des verrous
- **Scalabilité** : Meilleure utilisation des processeurs multi-cœurs
- **Pas de deadlocks** : Impossible par définition
- **Latence prévisible** : Pas de variations dues aux verrous

**Inconvénients :**
- **Complexité élevée** : Très difficile à implémenter correctement
- **Bugs subtils** : Les erreurs sont difficiles à détecter et reproduire
- **Portabilité** : Dépend des garanties du matériel
- **Maintenance difficile** : Code complexe à comprendre et modifier

### Quand utiliser le lock-free ?

✅ **Utilisez le lock-free pour :**
- Structures de données très sollicitées (millions d'accès/sec)
- Systèmes temps réel où la latence est critique
- Optimisations de performance après profiling

❌ **N'utilisez PAS le lock-free pour :**
- Code métier standard
- Prototypes ou développement rapide
- Quand la performance actuelle est suffisante
- Si vous n'êtes pas certain de bien comprendre les concepts

> **Note importante** : Le lock-free est une optimisation avancée. Commencez toujours par des verrous classiques, mesurez les performances, et n'utilisez le lock-free que si vous avez un problème de performance prouvé.

## Concepts fondamentaux

### Opérations atomiques

Une **opération atomique** s'exécute complètement ou pas du tout, sans possibilité d'interruption partielle. C'est la base du lock-free.

```pascal
var
  Counter: Integer = 0;

// ❌ NON-ATOMIQUE : Peut être interrompu entre lecture et écriture
Inc(Counter);  // Équivaut à : Counter := Counter + 1

// ✅ ATOMIQUE : S'exécute en une seule instruction CPU
InterlockedIncrement(Counter);
```

### Compare-And-Swap (CAS)

L'opération **Compare-And-Swap** est l'instruction fondamentale du lock-free.

**Principe :**
```
function CompareAndSwap(var Target; Expected, NewValue: Value): Boolean;  
begin
  if Target = Expected then
  begin
    Target := NewValue;
    Result := True;  // Succès
  end
  else
    Result := False; // Échec (valeur changée par un autre thread)
end;
```

En FreePascal, cette opération s'appelle `InterlockedCompareExchange`.

### ABA Problem

Un problème classique du lock-free où une valeur change de A → B → A, donnant l'illusion qu'elle n'a pas changé.

**Exemple du problème :**
```
Thread 1                    Thread 2  
Lit A
                           Change A en B
                           Change B en A
CAS réussit (croit que rien n'a changé)  
Mais en réalité, des changements ont eu lieu !
```

**Solutions :**
- Utiliser des **version numbers** (compteurs de modifications)
- Utiliser des **pointeurs avec tags** (ABA-safe pointers)
- Utiliser le **garbage collection** différé

### Memory ordering

Les processeurs modernes peuvent réordonner les instructions pour optimiser les performances. Il faut contrôler cet ordre avec des **memory barriers**.

```pascal
// Sans barrier : ordre non garanti
Write(Data);  
Write(Flag);  // Un autre thread peut voir Flag = true avant que Data soit écrit !

// Avec barrier : ordre garanti
Write(Data);  
WriteBarrier;  // Garantit que Data est écrit avant Flag  
Write(Flag);
```

## Opérations atomiques en FreePascal

FreePascal fournit plusieurs fonctions pour les opérations atomiques.

### InterlockedIncrement / InterlockedDecrement

```pascal
var
  Counter: Integer;
  OldValue, NewValue: Integer;
begin
  Counter := 10;

  // Incrémenter atomiquement
  NewValue := InterlockedIncrement(Counter);
  // Counter = 11, NewValue = 11

  // Décrémenter atomiquement
  NewValue := InterlockedDecrement(Counter);
  // Counter = 10, NewValue = 10
end;
```

### InterlockedExchange

```pascal
var
  Value: Integer;
  OldValue: Integer;
begin
  Value := 42;

  // Échanger atomiquement
  OldValue := InterlockedExchange(Value, 100);
  // Value = 100, OldValue = 42
end;
```

### InterlockedExchangeAdd

```pascal
var
  Value: Integer;
  OldValue: Integer;
begin
  Value := 10;

  // Ajouter atomiquement
  OldValue := InterlockedExchangeAdd(Value, 5);
  // Value = 15, OldValue = 10
end;
```

### InterlockedCompareExchange

L'opération la plus importante pour le lock-free.

```pascal
var
  Value: Integer;
  Expected, Desired, Actual: Integer;
begin
  Value := 42;
  Expected := 42;
  Desired := 100;

  // Compare et échange si égal
  Actual := InterlockedCompareExchange(Value, Desired, Expected);

  if Actual = Expected then
    WriteLn('Succès : Value est maintenant ', Desired)
  else
    WriteLn('Échec : Value était ', Actual, ' au lieu de ', Expected);
end;
```

## Compteur lock-free

Implémentons un compteur thread-safe sans verrous.

```pascal
unit LockFreeCounter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TLockFreeCounter = class
  private
    FValue: Integer;
  public
    constructor Create(InitialValue: Integer = 0);

    function Increment: Integer;
    function Decrement: Integer;
    function Add(Delta: Integer): Integer;
    function Get: Integer;
    procedure SetValue(NewValue: Integer);

    // Opération conditionnelle
    function IncrementIfLessThan(Limit: Integer): Boolean;
  end;

implementation

{ TLockFreeCounter }

constructor TLockFreeCounter.Create(InitialValue: Integer);  
begin
  inherited Create;
  FValue := InitialValue;
end;

function TLockFreeCounter.Increment: Integer;  
begin
  // InterlockedIncrement retourne la NOUVELLE valeur
  Result := InterlockedIncrement(FValue);
end;

function TLockFreeCounter.Decrement: Integer;  
begin
  Result := InterlockedDecrement(FValue);
end;

function TLockFreeCounter.Add(Delta: Integer): Integer;  
begin
  // InterlockedExchangeAdd retourne l'ANCIENNE valeur
  // Donc on ajoute Delta pour obtenir la nouvelle
  Result := InterlockedExchangeAdd(FValue, Delta) + Delta;
end;

function TLockFreeCounter.Get: Integer;  
begin
  // La lecture d'un Integer aligné est atomique sur x86/x64
  Result := FValue;

  // Alternativement, pour être explicite :
  // Result := InterlockedCompareExchange(FValue, 0, 0);
end;

procedure TLockFreeCounter.SetValue(NewValue: Integer);  
begin
  InterlockedExchange(FValue, NewValue);
end;

function TLockFreeCounter.IncrementIfLessThan(Limit: Integer): Boolean;  
var
  OldValue, NewValue: Integer;
begin
  repeat
    OldValue := FValue;

    // Vérifier la condition
    if OldValue >= Limit then
      Exit(False);

    NewValue := OldValue + 1;

    // Essayer de mettre à jour avec CAS
  until InterlockedCompareExchange(FValue, NewValue, OldValue) = OldValue;

  Result := True;
end;

end.
```

### Utilisation du compteur lock-free

```pascal
var
  Counter: TLockFreeCounter;
  i: Integer;

// Plusieurs threads peuvent faire ceci simultanément
procedure WorkerThread;  
var
  i: Integer;
begin
  for i := 1 to 1000 do
    Counter.Increment;
end;

begin
  Counter := TLockFreeCounter.Create(0);
  try
    // Créer 10 threads qui incrémentent le compteur
    // ... création des threads ...

    // Résultat final : 10 * 1000 = 10000 garanti !
    WriteLn('Total : ', Counter.Get);
  finally
    Counter.Free;
  end;
end;
```

## Stack lock-free (Treiber Stack)

Une pile lock-free classique, aussi appelée **Treiber Stack**.

```pascal
unit LockFreeStack;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  // Nœud de la pile
  PLockFreeNode = ^TLockFreeNode;
  TLockFreeNode = record
    Data: Pointer;
    Next: PLockFreeNode;
  end;

  TLockFreeStack = class
  private
    FTop: PLockFreeNode;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Push(Data: Pointer);
    function Pop(out Data: Pointer): Boolean;
    function IsEmpty: Boolean;
  end;

implementation

{ TLockFreeStack }

constructor TLockFreeStack.Create;  
begin
  inherited Create;
  FTop := nil;
end;

destructor TLockFreeStack.Destroy;  
var
  Node: PLockFreeNode;
  Data: Pointer;
begin
  // Vider la pile
  while Pop(Data) do
    ; // Les données doivent être libérées par l'utilisateur

  inherited;
end;

procedure TLockFreeStack.Push(Data: Pointer);  
var
  NewNode: PLockFreeNode;
  OldTop: PLockFreeNode;
begin
  // Créer le nouveau nœud
  New(NewNode);
  NewNode^.Data := Data;

  // Boucle CAS pour insérer en haut de la pile
  repeat
    OldTop := FTop;
    NewNode^.Next := OldTop;
  until InterlockedCompareExchange(
    Pointer(FTop),
    Pointer(NewNode),
    Pointer(OldTop)
  ) = Pointer(OldTop);
end;

function TLockFreeStack.Pop(out Data: Pointer): Boolean;  
var
  OldTop, NewTop: PLockFreeNode;
begin
  repeat
    OldTop := FTop;

    // Pile vide ?
    if OldTop = nil then
      Exit(False);

    NewTop := OldTop^.Next;

    // Essayer de mettre à jour le sommet
  until InterlockedCompareExchange(
    Pointer(FTop),
    Pointer(NewTop),
    Pointer(OldTop)
  ) = Pointer(OldTop);

  // Succès : extraire les données
  Data := OldTop^.Data;
  Dispose(OldTop);
  Result := True;
end;

function TLockFreeStack.IsEmpty: Boolean;  
begin
  Result := FTop = nil;
end;

end.
```

### Problème ABA dans la stack

⚠️ **Attention** : Cette implémentation simple souffre du problème ABA !

**Scénario problématique :**
```
État initial : A → B → C

Thread 1 : Lit A, veut le retirer  
Thread 2 : Pop A, Pop B, Push A  (A est recyclé)  
Thread 1 : CAS réussit (A est toujours là) mais Next pointe sur une mauvaise adresse !
```

**Solution : Version avec compteur**

```pascal
type
  TVersionedPointer = record
    Pointer: PLockFreeNode;
    Version: Int64;
  end;

  TLockFreeStackSafe = class
  private
    FTop: TVersionedPointer;
  public
    procedure Push(Data: Pointer);
    function Pop(out Data: Pointer): Boolean;
  end;

// Compare-And-Swap pour structure 128-bit (x64)
function CAS128(var Target: TVersionedPointer;
  const Expected, Desired: TVersionedPointer): Boolean;
begin
  // Nécessite support CPU pour CMPXCHG16B (x64)
  // Implémentation spécifique à la plateforme
  {$IFDEF CPUX64}
  // Utiliser l'instruction CMPXCHG16B via assembleur inline
  {$ELSE}
  // Fallback avec verrou pour plateformes sans support
  {$ENDIF}
end;
```

## Queue lock-free (Michael-Scott Queue)

File d'attente FIFO lock-free, plus complexe que la pile.

```pascal
unit LockFreeQueue;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  PLockFreeQueueNode = ^TLockFreeQueueNode;
  TLockFreeQueueNode = record
    Data: Pointer;
    Next: PLockFreeQueueNode;
  end;

  TLockFreeQueue = class
  private
    FHead: PLockFreeQueueNode;
    FTail: PLockFreeQueueNode;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Enqueue(Data: Pointer);
    function Dequeue(out Data: Pointer): Boolean;
    function IsEmpty: Boolean;
  end;

implementation

{ TLockFreeQueue }

constructor TLockFreeQueue.Create;  
var
  DummyNode: PLockFreeQueueNode;
begin
  inherited Create;

  // Créer un nœud factice (dummy node)
  New(DummyNode);
  DummyNode^.Data := nil;
  DummyNode^.Next := nil;

  FHead := DummyNode;
  FTail := DummyNode;
end;

destructor TLockFreeQueue.Destroy;  
var
  Node, NextNode: PLockFreeQueueNode;
begin
  // Libérer tous les nœuds
  Node := FHead;
  while Node <> nil do
  begin
    NextNode := Node^.Next;
    Dispose(Node);
    Node := NextNode;
  end;

  inherited;
end;

procedure TLockFreeQueue.Enqueue(Data: Pointer);  
var
  NewNode: PLockFreeQueueNode;
  OldTail, OldNext: PLockFreeQueueNode;
begin
  // Créer le nouveau nœud
  New(NewNode);
  NewNode^.Data := Data;
  NewNode^.Next := nil;

  while True do
  begin
    OldTail := FTail;
    OldNext := OldTail^.Next;

    // Vérifier que Tail n'a pas changé
    if OldTail = FTail then
    begin
      if OldNext = nil then
      begin
        // Tail pointe vraiment sur le dernier nœud
        if InterlockedCompareExchange(
          Pointer(OldTail^.Next),
          Pointer(NewNode),
          Pointer(OldNext)
        ) = Pointer(OldNext) then
        begin
          // Succès : tenter de mettre à jour Tail
          InterlockedCompareExchange(
            Pointer(FTail),
            Pointer(NewNode),
            Pointer(OldTail)
          );
          Break;
        end;
      end
      else
      begin
        // Tail était en retard, l'avancer
        InterlockedCompareExchange(
          Pointer(FTail),
          Pointer(OldNext),
          Pointer(OldTail)
        );
      end;
    end;
  end;
end;

function TLockFreeQueue.Dequeue(out Data: Pointer): Boolean;  
var
  OldHead, OldTail, OldNext: PLockFreeQueueNode;
begin
  while True do
  begin
    OldHead := FHead;
    OldTail := FTail;
    OldNext := OldHead^.Next;

    // Vérifier cohérence
    if OldHead = FHead then
    begin
      if OldHead = OldTail then
      begin
        // Queue vide ou Tail en retard
        if OldNext = nil then
          Exit(False);  // Queue vraiment vide

        // Avancer Tail
        InterlockedCompareExchange(
          Pointer(FTail),
          Pointer(OldNext),
          Pointer(OldTail)
        );
      end
      else
      begin
        // Lire la valeur avant CAS
        Data := OldNext^.Data;

        // Essayer de retirer le nœud
        if InterlockedCompareExchange(
          Pointer(FHead),
          Pointer(OldNext),
          Pointer(OldHead)
        ) = Pointer(OldHead) then
        begin
          Dispose(OldHead);  // Libérer l'ancien dummy
          Exit(True);
        end;
      end;
    end;
  end;
end;

function TLockFreeQueue.IsEmpty: Boolean;  
begin
  Result := FHead^.Next = nil;
end;

end.
```

### Utilisation de la queue lock-free

```pascal
var
  Queue: TLockFreeQueue;
  Data: Pointer;
  Value: Integer;

// Thread producteur
procedure Producer;  
var
  i: Integer;
begin
  for i := 1 to 100 do
  begin
    Queue.Enqueue(Pointer(PtrInt(i)));
    Sleep(10);
  end;
end;

// Thread consommateur
procedure Consumer;  
var
  Data: Pointer;
  Value: Integer;
begin
  while not Terminated do
  begin
    if Queue.Dequeue(Data) then
    begin
      Value := PtrInt(Data);
      WriteLn('Consommé : ', Value);
    end
    else
      Sleep(1);
  end;
end;
```

## Hash table lock-free

Table de hachage sans verrous avec chaining.

```pascal
unit LockFreeHashTable;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  HASH_TABLE_SIZE = 1024;

type
  PHashNode = ^THashNode;
  THashNode = record
    Key: Integer;
    Value: Pointer;
    Next: PHashNode;
  end;

  TLockFreeHashTable = class
  private
    FBuckets: array[0..HASH_TABLE_SIZE-1] of PHashNode;

    function Hash(Key: Integer): Integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Insert(Key: Integer; Value: Pointer);
    function Find(Key: Integer; out Value: Pointer): Boolean;
    function Delete(Key: Integer): Boolean;
  end;

implementation

{ TLockFreeHashTable }

constructor TLockFreeHashTable.Create;  
var
  i: Integer;
begin
  inherited Create;
  for i := 0 to High(FBuckets) do
    FBuckets[i] := nil;
end;

destructor TLockFreeHashTable.Destroy;  
var
  i: Integer;
  Node, NextNode: PHashNode;
begin
  // Libérer tous les nœuds
  for i := 0 to High(FBuckets) do
  begin
    Node := FBuckets[i];
    while Node <> nil do
    begin
      NextNode := Node^.Next;
      Dispose(Node);
      Node := NextNode;
    end;
  end;

  inherited;
end;

function TLockFreeHashTable.Hash(Key: Integer): Integer;  
begin
  // Simple hash function
  Result := (Key * 2654435761) mod HASH_TABLE_SIZE;
end;

procedure TLockFreeHashTable.Insert(Key: Integer; Value: Pointer);  
var
  BucketIndex: Integer;
  NewNode, OldHead: PHashNode;
begin
  BucketIndex := Hash(Key);

  // Créer le nouveau nœud
  New(NewNode);
  NewNode^.Key := Key;
  NewNode^.Value := Value;

  // Insérer en tête de liste avec CAS
  repeat
    OldHead := FBuckets[BucketIndex];
    NewNode^.Next := OldHead;
  until InterlockedCompareExchange(
    Pointer(FBuckets[BucketIndex]),
    Pointer(NewNode),
    Pointer(OldHead)
  ) = Pointer(OldHead);
end;

function TLockFreeHashTable.Find(Key: Integer; out Value: Pointer): Boolean;  
var
  BucketIndex: Integer;
  Node: PHashNode;
begin
  BucketIndex := Hash(Key);
  Node := FBuckets[BucketIndex];

  // Parcourir la liste chaînée
  while Node <> nil do
  begin
    if Node^.Key = Key then
    begin
      Value := Node^.Value;
      Exit(True);
    end;
    Node := Node^.Next;
  end;

  Result := False;
end;

function TLockFreeHashTable.Delete(Key: Integer): Boolean;  
var
  BucketIndex: Integer;
  Prev, Current, Next: PHashNode;
begin
  BucketIndex := Hash(Key);

  // Implémentation simplifiée (version complète nécessite plus de CAS)
  // Dans une vraie implémentation, utiliseriez des marqueurs pour la suppression

  Result := False;
  // ... logique de suppression avec CAS ...
end;

end.
```

## Techniques avancées

### Memory Barriers

Contrôler l'ordre d'exécution des instructions mémoire.

```pascal
procedure WriteBarrier; inline;  
begin
  {$IFDEF CPUX86}
  asm
    sfence
  end;
  {$ENDIF}
  {$IFDEF CPUX64}
  asm
    sfence
  end;
  {$ENDIF}
end;

procedure ReadBarrier; inline;  
begin
  {$IFDEF CPUX86}
  asm
    lfence
  end;
  {$ENDIF}
  {$IFDEF CPUX64}
  asm
    lfence
  end;
  {$ENDIF}
end;

procedure FullBarrier; inline;  
begin
  {$IFDEF CPUX86}
  asm
    mfence
  end;
  {$ENDIF}
  {$IFDEF CPUX64}
  asm
    mfence
  end;
  {$ENDIF}
end;
```

### Backoff strategies

Réduire la contention lors des CAS répétés.

```pascal
type
  TBackoffStrategy = class
  private
    FCurrentWait: Integer;
    FMaxWait: Integer;
  public
    constructor Create(MaxWait: Integer = 1000);
    procedure Wait;
    procedure Reset;
  end;

constructor TBackoffStrategy.Create(MaxWait: Integer);  
begin
  inherited Create;
  FMaxWait := MaxWait;
  FCurrentWait := 1;
end;

procedure TBackoffStrategy.Wait;  
begin
  // Attente exponentielle
  Sleep(FCurrentWait);
  FCurrentWait := Min(FCurrentWait * 2, FMaxWait); // uses Math
end;

procedure TBackoffStrategy.Reset;  
begin
  FCurrentWait := 1;
end;

// Utilisation dans une boucle CAS
procedure OperationAvecBackoff;  
var
  Backoff: TBackoffStrategy;
begin
  Backoff := TBackoffStrategy.Create;
  try
    while not CASOperation() do
    begin
      Backoff.Wait;
    end;
    Backoff.Reset; // Succès
  finally
    Backoff.Free;
  end;
end;
```

### Hazard Pointers

Technique pour la gestion mémoire sûre dans les structures lock-free.

```pascal
unit HazardPointers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections;

type
  THazardPointer = class
  private
    FProtected: TThreadSafeList<Pointer>;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Protect(P: Pointer);
    procedure Unprotect(P: Pointer);
    function IsSafeToDelete(P: Pointer): Boolean;
  end;

implementation

constructor THazardPointer.Create;  
begin
  inherited;
  FProtected := TThreadSafeList<Pointer>.Create;
end;

destructor THazardPointer.Destroy;  
begin
  FProtected.Free;
  inherited;
end;

procedure THazardPointer.Protect(P: Pointer);  
begin
  if P <> nil then
    FProtected.Add(P);
end;

procedure THazardPointer.Unprotect(P: Pointer);  
begin
  if P <> nil then
    FProtected.Remove(P);
end;

function THazardPointer.IsSafeToDelete(P: Pointer): Boolean;  
begin
  Result := not FProtected.Contains(P);
end;

end.
```

## Patterns et bonnes pratiques

### 1. Toujours utiliser CAS dans une boucle

```pascal
// ❌ INCORRECT - Une seule tentative
if InterlockedCompareExchange(Value, New, Old) = Old then
  WriteLn('Succès');

// ✅ CORRECT - Boucle jusqu'au succès
repeat
  Old := Value;
  New := CalculateNew(Old);
until InterlockedCompareExchange(Value, New, Old) = Old;
```

### 2. Lire avant d'écrire

```pascal
// ❌ INEFFICACE - Écrit toujours
InterlockedExchange(Flag, True);

// ✅ EFFICACE - Lit d'abord
if Flag <> True then
  InterlockedExchange(Flag, True);
```

### 3. Attention aux faux partages (False Sharing)

```pascal
// ❌ MAUVAIS - Variables sur la même ligne de cache
type
  TCounters = record
    Counter1: Integer;  // Peut être sur la même ligne de cache
    Counter2: Integer;  // que Counter1
  end;

// ✅ BON - Padding pour séparer les lignes de cache
type
  TCounters = record
    Counter1: Integer;
    Padding1: array[0..63] of Byte;  // 64 bytes = ligne de cache typique
    Counter2: Integer;
    Padding2: array[0..63] of Byte;
  end;
```

### 4. Tester exhaustivement

> **Note** : Les exemples de tests ci-dessous utilisent `TThread.CreateAnonymousThread` avec des procédures anonymes, ce qui nécessite `{$modeswitch anonymousfunctions}` (FPC 3.3.1+). En alternative, créer des classes `TThread` nommées.

```pascal
// Test de stress avec plusieurs threads
procedure StressTest;  
var
  Counter: TLockFreeCounter;
  Threads: array[1..100] of TThread;
  i: Integer;
begin
  Counter := TLockFreeCounter.Create(0);
  try
    // Créer 100 threads qui incrémentent 10000 fois
    for i := 1 to 100 do
      Threads[i] := TThread.CreateAnonymousThread(procedure
      var
        j: Integer;
      begin
        for j := 1 to 10000 do
          Counter.Increment;
      end);

    // Démarrer tous les threads
    for i := 1 to 100 do
      Threads[i].Start;

    // Attendre la fin
    for i := 1 to 100 do
      Threads[i].WaitFor;

    // Vérifier le résultat
    if Counter.Get = 1000000 then
      WriteLn('Test réussi!')
    else
      WriteLn('ERREUR : ', Counter.Get, ' au lieu de 1000000');
  finally
    Counter.Free;
  end;
end;
```

## Débogage du code lock-free

### Techniques de débogage

```pascal
// 1. Logging atomique
procedure AtomicLog(const Msg: string);  
var
  CS: TCriticalSection;
begin
  CS.Enter;
  try
    WriteLn(Format('[%d] %s', [GetCurrentThreadId, Msg]));
  finally
    CS.Leave;
  end;
end;

// 2. Compteurs de statistiques
var
  CASAttempts: Integer = 0;   // Integer car InterlockedIncrement = Longint
  CASSuccesses: Integer = 0;

procedure TrackCAS(Success: Boolean);  
begin
  InterlockedIncrement(CASAttempts);
  if Success then
    InterlockedIncrement(CASSuccesses);
end;

// 3. Invariants
procedure CheckInvariant(Condition: Boolean; const Msg: string);  
begin
  if not Condition then
    raise Exception.Create('Invariant violé : ' + Msg);
end;
```

### Outils de détection

- **Thread Sanitizer** : Détecte les data races
- **Valgrind/Helgrind** : Analyse de concurrence (Linux)
- **Intel Inspector** : Détection de threading issues

## Performance et optimisations

### Mesurer la performance

```pascal
type
  TPerformanceMeter = class
  private
    FStartTime: Int64;
    FOperationCount: Int64;
  public
    procedure Start;
    procedure RecordOperation;
    function GetThroughput: Double;
    function GetAverageLatency: Double;
  end;

implementation

uses
  SysUtils;

procedure TPerformanceMeter.Start;  
begin
  FStartTime := GetTickCount64;
  FOperationCount := 0;
end;

procedure TPerformanceMeter.RecordOperation;  
begin
  InterlockedIncrement(FOperationCount);
end;

function TPerformanceMeter.GetThroughput: Double;  
var
  ElapsedMs: Int64;
begin
  ElapsedMs := GetTickCount64 - FStartTime;
  if ElapsedMs > 0 then
    Result := (FOperationCount * 1000.0) / ElapsedMs
  else
    Result := 0;
end;

function TPerformanceMeter.GetAverageLatency: Double;  
var
  ElapsedMs: Int64;
begin
  ElapsedMs := GetTickCount64 - FStartTime;
  if FOperationCount > 0 then
    Result := ElapsedMs / FOperationCount
  else
    Result := 0;
end;
```

### Comparaison Lock vs Lock-free

```pascal
procedure CompareLockVsLockFree;  
var
  LockCounter: Integer;
  LockFreeCounter: TLockFreeCounter;
  CS: TCriticalSection;
  Meter: TPerformanceMeter;
  i, ThreadCount: Integer;
  Threads: array of TThread;
begin
  ThreadCount := 4;
  SetLength(Threads, ThreadCount);
  Meter := TPerformanceMeter.Create;
  CS := TCriticalSection.Create;
  LockFreeCounter := TLockFreeCounter.Create;

  try
    // Test avec verrous
    WriteLn('=== Test avec Critical Section ===');
    LockCounter := 0;
    Meter.Start;

    for i := 0 to ThreadCount - 1 do
      Threads[i] := TThread.CreateAnonymousThread(procedure
      var
        j: Integer;
      begin
        for j := 1 to 100000 do
        begin
          CS.Enter;
          try
            Inc(LockCounter);
          finally
            CS.Leave;
          end;
          Meter.RecordOperation;
        end;
      end);

    for i := 0 to ThreadCount - 1 do
      Threads[i].Start;
    for i := 0 to ThreadCount - 1 do
      Threads[i].WaitFor;

    WriteLn(Format('Throughput: %.0f ops/sec', [Meter.GetThroughput]));
    WriteLn(Format('Résultat: %d', [LockCounter]));
    WriteLn;

    // Test lock-free
    WriteLn('=== Test Lock-Free ===');
    LockFreeCounter.SetValue(0);
    Meter.Start;

    for i := 0 to ThreadCount - 1 do
      Threads[i] := TThread.CreateAnonymousThread(procedure
      var
        j: Integer;
      begin
        for j := 1 to 100000 do
        begin
          LockFreeCounter.Increment;
          Meter.RecordOperation;
        end;
      end);

    for i := 0 to ThreadCount - 1 do
      Threads[i].Start;
    for i := 0 to ThreadCount - 1 do
      Threads[i].WaitFor;

    WriteLn(Format('Throughput: %.0f ops/sec', [Meter.GetThroughput]));
    WriteLn(Format('Résultat: %d', [LockFreeCounter.Get]));

  finally
    LockFreeCounter.Free;
    CS.Free;
    Meter.Free;
  end;
end;
```

### Résultats typiques

Sur un processeur quad-core moderne, vous pourriez observer :

```
=== Test avec Critical Section ===
Throughput: 1500000 ops/sec  
Résultat: 400000

=== Test Lock-Free ===
Throughput: 8000000 ops/sec  
Résultat: 400000
```

Le lock-free peut être **5x plus rapide** dans des scénarios de forte contention !

## Cas d'usage réels

### 1. Pool de mémoire lock-free

Gestionnaire de mémoire haute performance pour allocations fréquentes.

```pascal
unit LockFreeMemoryPool;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  POOL_SIZE = 1000;
  BLOCK_SIZE = 256;

type
  PMemoryBlock = ^TMemoryBlock;
  TMemoryBlock = record
    Data: array[0..BLOCK_SIZE-1] of Byte;
    Next: PMemoryBlock;
  end;

  TLockFreeMemoryPool = class
  private
    FFreeList: PMemoryBlock;
    FAllocatedCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    function Allocate: PMemoryBlock;
    procedure Deallocate(Block: PMemoryBlock);

    function GetAllocatedCount: Integer;
  end;

implementation

{ TLockFreeMemoryPool }

constructor TLockFreeMemoryPool.Create;  
var
  i: Integer;
  Block: PMemoryBlock;
begin
  inherited Create;

  FFreeList := nil;
  FAllocatedCount := 0;

  // Pré-allouer les blocs
  for i := 1 to POOL_SIZE do
  begin
    New(Block);
    Block^.Next := FFreeList;
    FFreeList := Block;
  end;
end;

destructor TLockFreeMemoryPool.Destroy;  
var
  Block, NextBlock: PMemoryBlock;
begin
  // Libérer tous les blocs
  Block := FFreeList;
  while Block <> nil do
  begin
    NextBlock := Block^.Next;
    Dispose(Block);
    Block := NextBlock;
  end;

  inherited;
end;

function TLockFreeMemoryPool.Allocate: PMemoryBlock;  
var
  OldHead: PMemoryBlock;
begin
  repeat
    Result := FFreeList;

    if Result = nil then
    begin
      // Pool épuisé, allouer un nouveau bloc
      New(Result);
      Result^.Next := nil;
      InterlockedIncrement(FAllocatedCount);
      Exit;
    end;

    OldHead := Result;
  until InterlockedCompareExchange(
    Pointer(FFreeList),
    Pointer(Result^.Next),
    Pointer(OldHead)
  ) = Pointer(OldHead);

  InterlockedIncrement(FAllocatedCount);
end;

procedure TLockFreeMemoryPool.Deallocate(Block: PMemoryBlock);  
var
  OldHead: PMemoryBlock;
begin
  if Block = nil then
    Exit;

  repeat
    OldHead := FFreeList;
    Block^.Next := OldHead;
  until InterlockedCompareExchange(
    Pointer(FFreeList),
    Pointer(Block),
    Pointer(OldHead)
  ) = Pointer(OldHead);

  InterlockedDecrement(FAllocatedCount);
end;

function TLockFreeMemoryPool.GetAllocatedCount: Integer;  
begin
  Result := FAllocatedCount;
end;

end.
```

### 2. Logger lock-free

Système de logging haute performance sans blocage.

```pascal
unit LockFreeLogger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs;

type
  TLogLevel = (llDebug, llInfo, llWarning, llError);

  PLogEntry = ^TLogEntry;
  TLogEntry = record
    Level: TLogLevel;
    Timestamp: TDateTime;
    ThreadID: TThreadID;
    Message: string;
    Next: PLogEntry;
  end;

  TLockFreeLogger = class;

  { Thread d'écriture dédié (CreateAnonymousThread ne peut
    pas prendre une méthode d'objet comme paramètre) }
  TLogWriterThread = class(TThread)
  private
    FLogger: TLockFreeLogger;
  protected
    procedure Execute; override;
  public
    constructor Create(ALogger: TLockFreeLogger);
  end;

  TLockFreeLogger = class
  private
    FHead: PLogEntry;
    FWriterThread: TLogWriterThread;
    FRunning: Boolean;
    FFile: TextFile;
  public
    constructor Create(const FileName: string);
    destructor Destroy; override;

    procedure Log(Level: TLogLevel; const Msg: string);
    procedure Flush;
  end;

implementation

{ TLogWriterThread }

constructor TLogWriterThread.Create(ALogger: TLockFreeLogger);  
begin
  inherited Create(False); // Démarrer immédiatement
  FLogger := ALogger;
  FreeOnTerminate := False;
end;

procedure TLogWriterThread.Execute;  
var
  Entry, NextEntry: PLogEntry;
  LevelStr: string;
begin
  while FLogger.FRunning do
  begin
    // Extraire toutes les entrées
    Entry := InterlockedExchange(Pointer(FLogger.FHead), nil);

    // Écrire dans le fichier
    while Entry <> nil do
    begin
      case Entry^.Level of
        llDebug:   LevelStr := 'DEBUG';
        llInfo:    LevelStr := 'INFO';
        llWarning: LevelStr := 'WARNING';
        llError:   LevelStr := 'ERROR';
      end;

      WriteLn(FLogger.FFile, Format('[%s] [%s] [Thread %d] %s',
        [FormatDateTime('yyyy-mm-dd hh:nn:ss', Entry^.Timestamp),
         LevelStr,
         Entry^.ThreadID,
         Entry^.Message]));

      NextEntry := Entry^.Next;
      Dispose(Entry);
      Entry := NextEntry;
    end;

    System.Flush(FLogger.FFile);
    Sleep(10);
  end;
end;

{ TLockFreeLogger }

constructor TLockFreeLogger.Create(const FileName: string);  
begin
  inherited Create;

  FHead := nil;
  FRunning := True;

  AssignFile(FFile, FileName);
  Rewrite(FFile);

  // Thread d'écriture dédié
  FWriterThread := TLogWriterThread.Create(Self);
end;

destructor TLockFreeLogger.Destroy;  
begin
  FRunning := False;
  FWriterThread.WaitFor;
  FWriterThread.Free;

  Flush;
  CloseFile(FFile);

  inherited;
end;

procedure TLockFreeLogger.Log(Level: TLogLevel; const Msg: string);  
var
  Entry: PLogEntry;
  OldHead: PLogEntry;
begin
  // Créer l'entrée
  New(Entry);
  Entry^.Level := Level;
  Entry^.Timestamp := Now;
  Entry^.ThreadID := GetCurrentThreadId;
  Entry^.Message := Msg;

  // Ajouter à la liste lock-free
  repeat
    OldHead := FHead;
    Entry^.Next := OldHead;
  until InterlockedCompareExchange(
    Pointer(FHead),
    Pointer(Entry),
    Pointer(OldHead)
  ) = Pointer(OldHead);
end;

procedure TLockFreeLogger.Flush;  
var
  Entry, NextEntry: PLogEntry;
begin
  // Force l'écriture des entrées restantes
  Entry := InterlockedExchange(Pointer(FHead), nil);

  while Entry <> nil do
  begin
    WriteLn(FFile, Entry^.Message);
    NextEntry := Entry^.Next;
    Dispose(Entry);
    Entry := NextEntry;
  end;

  System.Flush(FFile);
end;

end.
```

### Utilisation du logger

```pascal
var
  Logger: TLockFreeLogger;

begin
  Logger := TLockFreeLogger.Create('app.log');
  try
    // Plusieurs threads peuvent logger simultanément sans blocage
    Logger.Log(llInfo, 'Application démarrée');
    Logger.Log(llDebug, 'Variable X = ' + IntToStr(X));
    Logger.Log(llError, 'Erreur de connexion');

    // Le logger écrit en arrière-plan de manière asynchrone
    Sleep(1000); // Laisser le temps d'écrire
  finally
    Logger.Free; // Flush automatique dans le destructeur
  end;
end;
```

### 3. Ring Buffer lock-free

Buffer circulaire pour streaming de données.

```pascal
unit LockFreeRingBuffer;

{$mode delphi}{$H+}  // mode delphi pour déclaration de type générique

interface

uses
  Classes, SysUtils;

type
  TLockFreeRingBuffer<T> = class
  private
    FBuffer: array of T;
    FCapacity: Integer;
    FReadIndex: Integer;
    FWriteIndex: Integer;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;

    function Write(const Item: T): Boolean;
    function Read(out Item: T): Boolean;

    function Count: Integer;
    function IsFull: Boolean;
    function IsEmpty: Boolean;
  end;

implementation

{ TLockFreeRingBuffer<T> }

constructor TLockFreeRingBuffer<T>.Create(ACapacity: Integer);  
begin
  inherited Create;
  FCapacity := ACapacity + 1; // +1 pour distinguer plein/vide
  SetLength(FBuffer, FCapacity);
  FReadIndex := 0;
  FWriteIndex := 0;
end;

destructor TLockFreeRingBuffer<T>.Destroy;  
begin
  inherited;
end;

function TLockFreeRingBuffer<T>.Write(const Item: T): Boolean;  
var
  CurrentWrite, NextWrite, CurrentRead: Integer;
begin
  repeat
    CurrentWrite := FWriteIndex;
    CurrentRead := FReadIndex;

    NextWrite := (CurrentWrite + 1) mod FCapacity;

    // Buffer plein ?
    if NextWrite = CurrentRead then
      Exit(False);

  until InterlockedCompareExchange(FWriteIndex, NextWrite, CurrentWrite) = CurrentWrite;

  // Écrire les données
  FBuffer[CurrentWrite] := Item;
  Result := True;
end;

function TLockFreeRingBuffer<T>.Read(out Item: T): Boolean;  
var
  CurrentRead, NextRead, CurrentWrite: Integer;
begin
  repeat
    CurrentRead := FReadIndex;
    CurrentWrite := FWriteIndex;

    // Buffer vide ?
    if CurrentRead = CurrentWrite then
      Exit(False);

    NextRead := (CurrentRead + 1) mod FCapacity;

  until InterlockedCompareExchange(FReadIndex, NextRead, CurrentRead) = CurrentRead;

  // Lire les données
  Item := FBuffer[CurrentRead];
  Result := True;
end;

function TLockFreeRingBuffer<T>.Count: Integer;  
var
  W, R: Integer;
begin
  W := FWriteIndex;
  R := FReadIndex;

  if W >= R then
    Result := W - R
  else
    Result := FCapacity - R + W;
end;

function TLockFreeRingBuffer<T>.IsFull: Boolean;  
begin
  Result := ((FWriteIndex + 1) mod FCapacity) = FReadIndex;
end;

function TLockFreeRingBuffer<T>.IsEmpty: Boolean;  
begin
  Result := FWriteIndex = FReadIndex;
end;

end.
```

## Différences Windows/Ubuntu

Les opérations atomiques sont supportées sur les deux plateformes, mais avec quelques nuances.

### Windows

```pascal
{$IFDEF WINDOWS}
uses
  Windows;

// Windows fournit des interlocked operations natives
// InterlockedIncrement, InterlockedCompareExchange, etc.

// Opérations 64-bit disponibles depuis Windows XP
function InterlockedIncrement64(var Target: Int64): Int64;  
begin
  Result := Windows.InterlockedIncrement64(Target);
end;
{$ENDIF}
```

### Ubuntu/Linux

```pascal
{$IFDEF LINUX}
uses
  BaseUnix;

// Linux utilise les atomic builtins GCC
// FreePascal les mappe automatiquement

// Support des opérations atomiques via GCC intrinsics
{$ENDIF}
```

### Support des architectures

```pascal
{$IFDEF CPUX86}
  // x86 32-bit : Support complet des opérations atomiques 32-bit
  // CAS 64-bit disponible via CMPXCHG8B (Pentium+)
{$ENDIF}

{$IFDEF CPUX64}
  // x64 : Support complet y compris CAS 128-bit (CMPXCHG16B)
{$ENDIF}

{$IFDEF CPUARM}
  // ARM : Support via LDREX/STREX
  // Attention : peut nécessiter kernel récent
{$ENDIF}
```

## Pièges courants et solutions

### 1. Le problème ABA

**Problème :**
```pascal
// Thread 1 lit A
Value := Pointer; // A

// Thread 2 fait: Pop(A), Pop(B), Push(A)
// A est maintenant à une adresse différente mais même valeur

// Thread 1 fait CAS et réussit
// Mais le pointeur Next de A est maintenant invalide !
```

**Solution : Tagged pointers**
```pascal
type
  TTaggedPointer = record
    Pointer: Pointer;
    Tag: Int64; // Compteur qui augmente à chaque modification
  end;

// Utiliser InterlockedCompareExchange128 (x64)
```

### 2. Memory Ordering

**Problème :**
```pascal
// Thread 1
Data := 42;  
Ready := True;  // Un autre thread peut voir Ready=True avant Data=42 !

// Thread 2
if Ready then
  Process(Data);  // Data pourrait ne pas être 42 !
```

**Solution : Memory barriers**
```pascal
// Thread 1
Data := 42;  
WriteBarrier;  // Garantit l'ordre  
Ready := True;

// Thread 2
if Ready then  
begin
  ReadBarrier;  // Garantit l'ordre
  Process(Data);
end;
```

### 3. False Sharing

**Problème :**
```pascal
type
  TCounters = record
    Counter1: Integer;  // Ligne de cache 0
    Counter2: Integer;  // Ligne de cache 0 aussi !
  end;

// Les deux compteurs sont sur la même ligne de cache
// Modifications de Counter1 invalident le cache de Counter2 !
```

**Solution : Padding**
```pascal
type
  TCounters = record
    Counter1: Integer;
    Padding1: array[0..59] of Byte;  // Force nouvelle ligne de cache
    Counter2: Integer;
    Padding2: array[0..59] of Byte;
  end;
```

### 4. Livelock

**Problème :**
```pascal
// Tous les threads échouent et réessaient en boucle
while not CAS(...) do
  ; // Boucle infinie si forte contention !
```

**Solution : Backoff exponentiel**
```pascal
var
  Backoff: Integer;
begin
  Backoff := 1;
  while not CAS(...) do
  begin
    Sleep(Backoff);
    Backoff := Min(Backoff * 2, 100); // uses Math
  end;
end;
```

## Vérification et tests

### Tests de correction

```pascal
procedure TestCorrectness;  
const
  THREAD_COUNT = 10;
  OPS_PER_THREAD = 100000;
var
  Counter: TLockFreeCounter;
  Threads: array[0..THREAD_COUNT-1] of TThread;
  i: Integer;
begin
  Counter := TLockFreeCounter.Create(0);
  try
    // Chaque thread incrémente
    for i := 0 to THREAD_COUNT - 1 do
      Threads[i] := TThread.CreateAnonymousThread(procedure
      var
        j: Integer;
      begin
        for j := 1 to OPS_PER_THREAD do
          Counter.Increment;
      end);

    for i := 0 to THREAD_COUNT - 1 do
      Threads[i].Start;

    for i := 0 to THREAD_COUNT - 1 do
      Threads[i].WaitFor;

    // Vérifier le résultat
    Assert(Counter.Get = THREAD_COUNT * OPS_PER_THREAD,
      'Compteur incorrect !');

    WriteLn('Test de correction : OK');
  finally
    Counter.Free;
  end;
end;
```

### Tests de stress

```pascal
procedure StressTest;  
const
  DURATION_SEC = 60;
var
  Stack: TLockFreeStack;
  StartTime: TDateTime;
  Threads: array[0..7] of TThread;
  i: Integer;
  Operations: Integer;  // Integer car InterlockedIncrement = Longint
begin
  Stack := TLockFreeStack.Create;
  Operations := 0;

  try
    StartTime := Now;

    // Threads qui font Push/Pop en continu
    for i := 0 to 7 do
      Threads[i] := TThread.CreateAnonymousThread(procedure
      var
        Data: Pointer;
      begin
        while SecondsBetween(Now, StartTime) < DURATION_SEC do // uses DateUtils
        begin
          if Random(2) = 0 then
            Stack.Push(Pointer(Random(1000)))
          else
            Stack.Pop(Data);

          InterlockedIncrement(Operations);
        end;
      end);

    for i := 0 to 7 do
      Threads[i].Start;

    for i := 0 to 7 do
      Threads[i].WaitFor;

    WriteLn(Format('Stress test: %d opérations en %d secondes',
      [Operations, DURATION_SEC]));
    WriteLn(Format('Throughput: %.0f ops/sec',
      [Operations / DURATION_SEC]));

  finally
    Stack.Free;
  end;
end;
```

### Tests de régression

```pascal
// Enregistrer les résultats de référence
type
  TBenchmarkResult = record
    TestName: string;
    Throughput: Double;
    Latency: Double;
  end;

procedure SaveBenchmark(const Result: TBenchmarkResult);  
var
  F: TextFile;
begin
  AssignFile(F, 'benchmarks.txt');
  if FileExists('benchmarks.txt') then
    Append(F)
  else
    Rewrite(F);

  try
    WriteLn(F, Format('%s,%s,%.2f,%.6f',
      [FormatDateTime('yyyy-mm-dd hh:nn:ss', Now),
       Result.TestName,
       Result.Throughput,
       Result.Latency]));
  finally
    CloseFile(F);
  end;
end;

procedure CheckRegression(const Current, Baseline: TBenchmarkResult);  
const
  THRESHOLD = 0.10; // 10% de régression acceptable
var
  Change: Double;
begin
  Change := (Baseline.Throughput - Current.Throughput) / Baseline.Throughput;

  if Change > THRESHOLD then
    WriteLn(Format('AVERTISSEMENT: Régression de %.1f%% détectée !',
      [Change * 100]))
  else if Change < -THRESHOLD then
    WriteLn(Format('AMÉLIORATION: +%.1f%% de performance !',
      [-Change * 100]))
  else
    WriteLn('Performance stable');
end;
```

## Alternatives et bibliothèques

### Bibliothèques existantes

1. **MTProcs** - Multi-threading library pour FreePascal
   - Fournit des structures lock-free de base
   - Abstractions de plus haut niveau

2. **Intel TBB** - Threading Building Blocks
   - Bindings possibles pour FreePascal
   - Structures lock-free industrielles

3. **Boost.Lockfree** (C++)
   - Référence pour les algorithmes
   - Peut servir d'inspiration

### Quand utiliser une bibliothèque

✅ **Utilisez une bibliothèque si :**
- Vous avez besoin de structures standard (queue, stack, etc.)
- Le code est critique et doit être fiable
- Vous voulez gagner du temps de développement

❌ **Implémentez vous-même si :**
- Vous avez des besoins très spécifiques
- Vous voulez comprendre les mécanismes en profondeur
- Les bibliothèques disponibles ne correspondent pas

## Bonnes pratiques finales

### Checklist de développement

- [ ] Commencer avec des verrous classiques
- [ ] Mesurer et identifier les goulots d'étranglement
- [ ] Considérer le lock-free uniquement si nécessaire
- [ ] Concevoir l'algorithme sur papier d'abord
- [ ] Implémenter progressivement
- [ ] Tester exhaustivement (correction, stress, performance)
- [ ] Documenter les invariants et les cas limites
- [ ] Code review par un expert si possible
- [ ] Maintenir des tests de régression

### Documentation obligatoire

```pascal
{
  TLockFreeStack - Pile lock-free (Treiber Stack)

  INVARIANTS:
  - FTop pointe toujours sur un nœud valide ou NIL
  - La chaîne Next forme une liste simplement chaînée

  GARANTIES:
  - Lock-free : au moins un thread progresse toujours
  - Thread-safe : peut être utilisé par plusieurs threads

  PROBLÈMES CONNUS:
  - Souffre du problème ABA (utiliser version avec tags pour production)
  - Pas de gestion automatique de la mémoire

  PERFORMANCE:
  - O(1) pour Push et Pop
  - Throughput typique : 5-10M ops/sec (4 threads, x64)
}
type
  TLockFreeStack = class
  ...
```

## Résumé

Le **lock-free programming** est une technique avancée qui offre :

**Avantages :**
- ✅ Performances excellentes en forte contention
- ✅ Pas de deadlocks possibles
- ✅ Latence prévisible
- ✅ Meilleure scalabilité multi-cœurs

**Inconvénients :**
- ❌ Complexité élevée
- ❌ Difficile à déboguer
- ❌ Risques subtils (ABA, memory ordering)
- ❌ Maintenance difficile

**Recommandations :**
1. N'utilisez le lock-free que si vous avez un problème de performance **prouvé**
2. Commencez toujours par des structures avec verrous
3. Utilisez des bibliothèques testées quand possible
4. Testez exhaustivement
5. Documentez tout

**Structures de base implémentables :**
- Compteurs atomiques (facile)
- Stack lock-free (moyen)
- Queue lock-free (difficile)
- Hash table lock-free (très difficile)

Le lock-free est un outil puissant, mais avec une grande puissance vient une grande responsabilité. Utilisez-le avec précaution et expertise !

⏭️ [Parallel programming library](/11-multithreading-concurrence/05-parallel-programming-library.md)
