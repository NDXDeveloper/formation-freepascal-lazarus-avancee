🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 11.3 Structures de données thread-safe

## Introduction aux structures thread-safe

Quand plusieurs threads accèdent simultanément à la même structure de données (liste, dictionnaire, file d'attente, etc.), des problèmes peuvent survenir si cette structure n'est pas **thread-safe** (sûre pour les threads).

### Qu'est-ce qu'une structure thread-safe ?

Une structure de données est **thread-safe** quand elle peut être utilisée en toute sécurité par plusieurs threads simultanément, sans corruption de données ni comportement imprévisible.

**Analogie** : Imaginez une boîte aux lettres partagée. Si plusieurs personnes essaient d'y mettre des lettres en même temps sans coordination, les lettres peuvent se mélanger ou se perdre. Une boîte aux lettres thread-safe serait comme avoir un mécanisme de verrouillage : une seule personne à la fois peut l'ouvrir.

### Pourquoi les structures standards ne sont-elles pas thread-safe ?

```pascal
// ❌ DANGER : TList n'est PAS thread-safe
var
  Liste: TList;

// Thread 1
procedure Thread1.Execute;  
begin
  Liste.Add(Pointer(1)); // Peut causer une corruption !
end;

// Thread 2
procedure Thread2.Execute;  
begin
  Liste.Add(Pointer(2)); // Peut causer une corruption !
end;
```

**Problèmes potentiels :**
- **Race condition** : Les deux threads modifient la structure en même temps
- **Corruption de données** : L'état interne devient incohérent
- **Exceptions** : Accès à des zones mémoire invalides
- **Résultats imprévisibles** : Le comportement change d'une exécution à l'autre

## Types de protection

Il existe plusieurs approches pour rendre une structure thread-safe :

### 1. Verrouillage global (Coarse-grained locking)

Protéger toute la structure avec un seul verrou.

**Avantages :**
- Simple à implémenter
- Garantit la cohérence

**Inconvénients :**
- Limite la concurrence
- Peut créer des goulots d'étranglement

### 2. Verrouillage fin (Fine-grained locking)

Utiliser plusieurs verrous pour différentes parties de la structure.

**Avantages :**
- Meilleure concurrence
- Meilleures performances

**Inconvénients :**
- Complexe à implémenter
- Risque de deadlocks

### 3. Lock-free (Sans verrous)

Utiliser des opérations atomiques du processeur.

**Avantages :**
- Performances maximales
- Pas de deadlocks

**Inconvénients :**
- Très complexe à implémenter correctement
- Nécessite une compréhension approfondie

## TThreadList : Liste thread-safe basique

FreePascal fournit `TThreadList`, une liste thread-safe simple.

### Utilisation de base

```pascal
uses
  Classes, SyncObjs;

var
  Liste: TThreadList;

begin
  // Créer la liste
  Liste := TThreadList.Create;
  try
    // Ajouter des éléments de manière thread-safe
    Liste.Add(Pointer(1));
    Liste.Add(Pointer(2));

    // Accès protégé à la liste interne
    with Liste.LockList do
    try
      // Pendant le verrouillage, on peut manipuler la liste
      WriteLn('Nombre d''éléments : ', Count);

      // Parcourir
      for i := 0 to Count - 1 do
        WriteLn('Élément : ', PtrInt(Items[i]));
    finally
      Liste.UnlockList; // TOUJOURS déverrouiller !
    end;
  finally
    Liste.Free;
  end;
end;
```

### Exemple complet avec TThreadList

```pascal
unit ThreadSafeListExample;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs;

type
  TProducerThread = class(TThread)
  private
    FList: TThreadList;
    FID: Integer;
  protected
    procedure Execute; override;
  public
    constructor Create(AList: TThreadList; AID: Integer);
  end;

  TConsumerThread = class(TThread)
  private
    FList: TThreadList;
  protected
    procedure Execute; override;
  public
    constructor Create(AList: TThreadList);
  end;

implementation

{ TProducerThread }

constructor TProducerThread.Create(AList: TThreadList; AID: Integer);  
begin
  inherited Create(False);
  FList := AList;
  FID := AID;
  FreeOnTerminate := True;
end;

procedure TProducerThread.Execute;  
var
  i: Integer;
  Value: PtrInt;
begin
  for i := 1 to 10 do
  begin
    Value := FID * 100 + i;
    FList.Add(Pointer(Value));

    WriteLn(Format('Producteur %d a ajouté : %d', [FID, Value]));
    Sleep(Random(100));
  end;
end;

{ TConsumerThread }

constructor TConsumerThread.Create(AList: TThreadList);  
begin
  inherited Create(False);
  FList := AList;
  FreeOnTerminate := True;
end;

procedure TConsumerThread.Execute;  
var
  InternalList: TList;
  Value: PtrInt;
begin
  while not Terminated do
  begin
    InternalList := FList.LockList;
    try
      if InternalList.Count > 0 then
      begin
        Value := PtrInt(InternalList[0]);
        InternalList.Delete(0);

        WriteLn(Format('Consommateur a retiré : %d', [Value]));
      end;
    finally
      FList.UnlockList;
    end;

    Sleep(Random(150));
  end;
end;

end.
```

### Utilisation

```pascal
procedure TestThreadSafeList;  
var
  SharedList: TThreadList;
  Producer1, Producer2: TProducerThread;
  Consumer: TConsumerThread;
begin
  SharedList := TThreadList.Create;
  try
    // Créer les producteurs
    Producer1 := TProducerThread.Create(SharedList, 1);
    Producer2 := TProducerThread.Create(SharedList, 2);

    // Créer le consommateur
    Consumer := TConsumerThread.Create(SharedList);

    // Attendre la fin
    Sleep(5000);
    Consumer.Terminate;

  finally
    SharedList.Free;
  end;
end;
```

## Implémentation d'une liste thread-safe personnalisée

Créons notre propre liste thread-safe pour mieux comprendre les mécanismes.

> **Note** : Les unités qui définissent leurs propres types génériques utilisent `{$mode delphi}` car la syntaxe des génériques y est plus naturelle (`TMyClass<T>` au lieu de `generic TMyClass<T>` / `specialize TMyClass<T>` en mode ObjFPC).

```pascal
unit CustomThreadSafeList;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs, Generics.Collections;

type
  // Liste thread-safe générique
  TThreadSafeList<T> = class
  private
    FList: TList<T>;
    FLock: TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;

    // Opérations basiques thread-safe
    procedure Add(const Item: T);
    function Remove(const Item: T): Boolean;
    procedure Clear;

    // Accès sécurisé
    function Count: Integer;
    function Get(Index: Integer): T;
    procedure Put(Index: Integer; const Value: T);

    // Accès direct avec verrouillage manuel
    function Lock: TList<T>;
    procedure Unlock;

    // Opérations avancées
    function Contains(const Item: T): Boolean;
    function IndexOf(const Item: T): Integer;
    procedure Sort(const Comparer: IComparer<T>);
  end;

implementation

{ TThreadSafeList<T> }

constructor TThreadSafeList<T>.Create;  
begin
  inherited Create;
  FList := TList<T>.Create;
  FLock := TCriticalSection.Create;
end;

destructor TThreadSafeList<T>.Destroy;  
begin
  FLock.Enter;
  try
    FList.Free;
  finally
    FLock.Leave;
    FLock.Free;
  end;
  inherited;
end;

procedure TThreadSafeList<T>.Add(const Item: T);  
begin
  FLock.Enter;
  try
    FList.Add(Item);
  finally
    FLock.Leave;
  end;
end;

function TThreadSafeList<T>.Remove(const Item: T): Boolean;  
var
  Index: Integer;
begin
  FLock.Enter;
  try
    Index := FList.IndexOf(Item);
    Result := Index >= 0;
    if Result then
      FList.Delete(Index);
  finally
    FLock.Leave;
  end;
end;

procedure TThreadSafeList<T>.Clear;  
begin
  FLock.Enter;
  try
    FList.Clear;
  finally
    FLock.Leave;
  end;
end;

function TThreadSafeList<T>.Count: Integer;  
begin
  FLock.Enter;
  try
    Result := FList.Count;
  finally
    FLock.Leave;
  end;
end;

function TThreadSafeList<T>.Get(Index: Integer): T;  
begin
  FLock.Enter;
  try
    Result := FList[Index];
  finally
    FLock.Leave;
  end;
end;

procedure TThreadSafeList<T>.Put(Index: Integer; const Value: T);  
begin
  FLock.Enter;
  try
    FList[Index] := Value;
  finally
    FLock.Leave;
  end;
end;

function TThreadSafeList<T>.Lock: TList<T>;  
begin
  FLock.Enter;
  Result := FList;
end;

procedure TThreadSafeList<T>.Unlock;  
begin
  FLock.Leave;
end;

function TThreadSafeList<T>.Contains(const Item: T): Boolean;  
begin
  FLock.Enter;
  try
    Result := FList.Contains(Item);
  finally
    FLock.Leave;
  end;
end;

function TThreadSafeList<T>.IndexOf(const Item: T): Integer;  
begin
  FLock.Enter;
  try
    Result := FList.IndexOf(Item);
  finally
    FLock.Leave;
  end;
end;

procedure TThreadSafeList<T>.Sort(const Comparer: IComparer<T>);  
begin
  FLock.Enter;
  try
    FList.Sort(Comparer);
  finally
    FLock.Leave;
  end;
end;

end.
```

### Utilisation de la liste générique thread-safe

```pascal
var
  Liste: TThreadSafeList<Integer>;
  i: Integer;
begin
  Liste := TThreadSafeList<Integer>.Create;
  try
    // Ajouter des éléments
    Liste.Add(42);
    Liste.Add(17);
    Liste.Add(99);

    // Lire de manière thread-safe
    WriteLn('Premier élément : ', Liste.Get(0));
    WriteLn('Nombre d''éléments : ', Liste.Count);

    // Accès direct pour opérations multiples
    with Liste.Lock do
    try
      WriteLn('Parcours de la liste :');
      for i := 0 to Count - 1 do
        WriteLn('  ', Items[i]);
    finally
      Liste.Unlock;
    end;
  finally
    Liste.Free;
  end;
end;
```

## Dictionnaire thread-safe

Un dictionnaire (clé-valeur) est très utile pour stocker des données indexées. Créons une version thread-safe.

```pascal
unit ThreadSafeDictionary;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs, Generics.Collections;

type
  TThreadSafeDictionary<TKey, TValue> = class
  private
    FDictionary: TDictionary<TKey, TValue>;
    FLock: TMultiReadExclusiveWriteSynchronizer; // Permet lectures multiples
  public
    constructor Create;
    destructor Destroy; override;

    // Opérations d'écriture (exclusives)
    procedure Add(const Key: TKey; const Value: TValue);
    procedure AddOrSetValue(const Key: TKey; const Value: TValue);
    function Remove(const Key: TKey): Boolean;
    procedure Clear;

    // Opérations de lecture (peuvent être multiples)
    function TryGetValue(const Key: TKey; out Value: TValue): Boolean;
    function ContainsKey(const Key: TKey): Boolean;
    function Count: Integer;

    // Accès direct avec verrouillage manuel
    procedure BeginRead;
    procedure EndRead;
    procedure BeginWrite;
    procedure EndWrite;

    property Dictionary: TDictionary<TKey, TValue> read FDictionary;
  end;

implementation

{ TThreadSafeDictionary<TKey, TValue> }

constructor TThreadSafeDictionary<TKey, TValue>.Create;  
begin
  inherited Create;
  FDictionary := TDictionary<TKey, TValue>.Create;
  FLock := TMultiReadExclusiveWriteSynchronizer.Create;
end;

destructor TThreadSafeDictionary<TKey, TValue>.Destroy;  
begin
  FLock.BeginWrite;
  try
    FDictionary.Free;
  finally
    FLock.EndWrite;
    FLock.Free;
  end;
  inherited;
end;

procedure TThreadSafeDictionary<TKey, TValue>.Add(const Key: TKey; const Value: TValue);  
begin
  FLock.BeginWrite;
  try
    FDictionary.Add(Key, Value);
  finally
    FLock.EndWrite;
  end;
end;

procedure TThreadSafeDictionary<TKey, TValue>.AddOrSetValue(const Key: TKey; const Value: TValue);  
begin
  FLock.BeginWrite;
  try
    FDictionary.AddOrSetValue(Key, Value);
  finally
    FLock.EndWrite;
  end;
end;

function TThreadSafeDictionary<TKey, TValue>.Remove(const Key: TKey): Boolean;  
begin
  FLock.BeginWrite;
  try
    Result := FDictionary.Remove(Key);
  finally
    FLock.EndWrite;
  end;
end;

procedure TThreadSafeDictionary<TKey, TValue>.Clear;  
begin
  FLock.BeginWrite;
  try
    FDictionary.Clear;
  finally
    FLock.EndWrite;
  end;
end;

function TThreadSafeDictionary<TKey, TValue>.TryGetValue(const Key: TKey; out Value: TValue): Boolean;  
begin
  FLock.BeginRead;
  try
    Result := FDictionary.TryGetValue(Key, Value);
  finally
    FLock.EndRead;
  end;
end;

function TThreadSafeDictionary<TKey, TValue>.ContainsKey(const Key: TKey): Boolean;  
begin
  FLock.BeginRead;
  try
    Result := FDictionary.ContainsKey(Key);
  finally
    FLock.EndRead;
  end;
end;

function TThreadSafeDictionary<TKey, TValue>.Count: Integer;  
begin
  FLock.BeginRead;
  try
    Result := FDictionary.Count;
  finally
    FLock.EndRead;
  end;
end;

procedure TThreadSafeDictionary<TKey, TValue>.BeginRead;  
begin
  FLock.BeginRead;
end;

procedure TThreadSafeDictionary<TKey, TValue>.EndRead;  
begin
  FLock.EndRead;
end;

procedure TThreadSafeDictionary<TKey, TValue>.BeginWrite;  
begin
  FLock.BeginWrite;
end;

procedure TThreadSafeDictionary<TKey, TValue>.EndWrite;  
begin
  FLock.EndWrite;
end;

end.
```

### Exemple d'utilisation du dictionnaire

```pascal
var
  Cache: TThreadSafeDictionary<string, Integer>;
  Value: Integer;
begin
  Cache := TThreadSafeDictionary<string, Integer>.Create;
  try
    // Ajouter des valeurs
    Cache.Add('utilisateur1', 100);
    Cache.Add('utilisateur2', 250);

    // Lire des valeurs
    if Cache.TryGetValue('utilisateur1', Value) then
      WriteLn('Score utilisateur1 : ', Value);

    // Mise à jour
    Cache.AddOrSetValue('utilisateur1', 150);

    // Vérifier l'existence
    if Cache.ContainsKey('utilisateur3') then
      WriteLn('Utilisateur trouvé')
    else
      WriteLn('Utilisateur introuvable');
  finally
    Cache.Free;
  end;
end;
```

## File d'attente thread-safe (Queue)

Les files d'attente sont essentielles pour la communication entre threads (pattern producteur-consommateur).

```pascal
unit ThreadSafeQueue;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs, DateUtils, Generics.Collections;

type
  TThreadSafeQueue<T> = class
  private
    FQueue: TQueue<T>;
    FLock: TCriticalSection;
    FItemAvailable: TEvent;
    FMaxSize: Integer;
  public
    constructor Create(AMaxSize: Integer = 0); // 0 = illimité
    destructor Destroy; override;

    function Enqueue(const Item: T; Timeout: Cardinal = INFINITE): Boolean;
    function Dequeue(out Item: T; Timeout: Cardinal = INFINITE): Boolean;
    function TryDequeue(out Item: T): Boolean;

    function Count: Integer;
    function IsEmpty: Boolean;
    function IsFull: Boolean;
    procedure Clear;
  end;

implementation

{ TThreadSafeQueue<T> }

constructor TThreadSafeQueue<T>.Create(AMaxSize: Integer);  
begin
  inherited Create;
  FQueue := TQueue<T>.Create;
  FLock := TCriticalSection.Create;
  FItemAvailable := TEvent.Create(nil, True, False, '');
  FMaxSize := AMaxSize;
end;

destructor TThreadSafeQueue<T>.Destroy;  
begin
  FLock.Enter;
  try
    FQueue.Free;
    FItemAvailable.Free;
  finally
    FLock.Leave;
    FLock.Free;
  end;
  inherited;
end;

function TThreadSafeQueue<T>.Enqueue(const Item: T; Timeout: Cardinal): Boolean;  
var
  StartTime: TDateTime;
begin
  Result := False;
  StartTime := Now;

  while not Result do
  begin
    FLock.Enter;
    try
      // Vérifier si on peut ajouter
      if (FMaxSize = 0) or (FQueue.Count < FMaxSize) then
      begin
        FQueue.Enqueue(Item);
        FItemAvailable.SetEvent;
        Result := True;
      end;
    finally
      FLock.Leave;
    end;

    // Si la file est pleine et timeout non dépassé, attendre un peu
    if not Result then
    begin
      if (Timeout <> INFINITE) and
         (MilliSecondsBetween(Now, StartTime) >= Timeout) then
        Exit(False);

      Sleep(10);
    end;
  end;
end;

function TThreadSafeQueue<T>.Dequeue(out Item: T; Timeout: Cardinal): Boolean;  
begin
  Result := False;

  // Attendre qu'un élément soit disponible
  if FItemAvailable.WaitFor(Timeout) = wrSignaled then
  begin
    FLock.Enter;
    try
      if FQueue.Count > 0 then
      begin
        Item := FQueue.Dequeue;
        Result := True;

        // Si la file est vide, réinitialiser l'événement
        if FQueue.Count = 0 then
          FItemAvailable.ResetEvent;
      end;
    finally
      FLock.Leave;
    end;
  end;
end;

function TThreadSafeQueue<T>.TryDequeue(out Item: T): Boolean;  
begin
  FLock.Enter;
  try
    Result := FQueue.Count > 0;
    if Result then
    begin
      Item := FQueue.Dequeue;
      if FQueue.Count = 0 then
        FItemAvailable.ResetEvent;
    end;
  finally
    FLock.Leave;
  end;
end;

function TThreadSafeQueue<T>.Count: Integer;  
begin
  FLock.Enter;
  try
    Result := FQueue.Count;
  finally
    FLock.Leave;
  end;
end;

function TThreadSafeQueue<T>.IsEmpty: Boolean;  
begin
  FLock.Enter;
  try
    Result := FQueue.Count = 0;
  finally
    FLock.Leave;
  end;
end;

function TThreadSafeQueue<T>.IsFull: Boolean;  
begin
  FLock.Enter;
  try
    Result := (FMaxSize > 0) and (FQueue.Count >= FMaxSize);
  finally
    FLock.Leave;
  end;
end;

procedure TThreadSafeQueue<T>.Clear;  
begin
  FLock.Enter;
  try
    FQueue.Clear;
    FItemAvailable.ResetEvent;
  finally
    FLock.Leave;
  end;
end;

end.
```

### Exemple avec la queue thread-safe

```pascal
type
  TMessage = record
    ID: Integer;
    Text: string;
    Timestamp: TDateTime;
  end;

var
  MessageQueue: TThreadSafeQueue<TMessage>;
  Msg: TMessage;

// Thread producteur
procedure ProducerThread;  
var
  i: Integer;
begin
  for i := 1 to 10 do
  begin
    Msg.ID := i;
    Msg.Text := Format('Message %d', [i]);
    Msg.Timestamp := Now;

    MessageQueue.Enqueue(Msg);
    Sleep(100);
  end;
end;

// Thread consommateur
procedure ConsumerThread;  
var
  Msg: TMessage;
begin
  while not Terminated do
  begin
    if MessageQueue.Dequeue(Msg, 1000) then
      WriteLn(Format('Reçu: %s à %s', [Msg.Text, TimeToStr(Msg.Timestamp)]));
  end;
end;
```

## Pile thread-safe (Stack)

Une pile LIFO (Last In, First Out) thread-safe.

```pascal
unit ThreadSafeStack;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs, Generics.Collections;

type
  TThreadSafeStack<T> = class
  private
    FStack: TStack<T>;
    FLock: TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Push(const Item: T);
    function Pop(out Item: T): Boolean;
    function TryPeek(out Item: T): Boolean;

    function Count: Integer;
    function IsEmpty: Boolean;
    procedure Clear;
  end;

implementation

{ TThreadSafeStack<T> }

constructor TThreadSafeStack<T>.Create;  
begin
  inherited Create;
  FStack := TStack<T>.Create;
  FLock := TCriticalSection.Create;
end;

destructor TThreadSafeStack<T>.Destroy;  
begin
  FLock.Enter;
  try
    FStack.Free;
  finally
    FLock.Leave;
    FLock.Free;
  end;
  inherited;
end;

procedure TThreadSafeStack<T>.Push(const Item: T);  
begin
  FLock.Enter;
  try
    FStack.Push(Item);
  finally
    FLock.Leave;
  end;
end;

function TThreadSafeStack<T>.Pop(out Item: T): Boolean;  
begin
  FLock.Enter;
  try
    Result := FStack.Count > 0;
    if Result then
      Item := FStack.Pop;
  finally
    FLock.Leave;
  end;
end;

function TThreadSafeStack<T>.TryPeek(out Item: T): Boolean;  
begin
  FLock.Enter;
  try
    Result := FStack.Count > 0;
    if Result then
      Item := FStack.Peek;
  finally
    FLock.Leave;
  end;
end;

function TThreadSafeStack<T>.Count: Integer;  
begin
  FLock.Enter;
  try
    Result := FStack.Count;
  finally
    FLock.Leave;
  end;
end;

function TThreadSafeStack<T>.IsEmpty: Boolean;  
begin
  FLock.Enter;
  try
    Result := FStack.Count = 0;
  finally
    FLock.Leave;
  end;
end;

procedure TThreadSafeStack<T>.Clear;  
begin
  FLock.Enter;
  try
    FStack.Clear;
  finally
    FLock.Leave;
  end;
end;

end.
```

## Compteur atomique

Pour des opérations simples comme incrémenter un compteur, les opérations atomiques sont plus efficaces que les verrous.

```pascal
unit AtomicCounter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TAtomicCounter = class
  private
    FValue: Integer;
  public
    constructor Create(InitialValue: Integer = 0);

    function Increment: Integer;
    function Decrement: Integer;
    function Add(Delta: Integer): Integer;
    function Get: Integer;
    procedure &Set(Value: Integer);

    function CompareExchange(NewValue, Comparand: Integer): Integer;
  end;

implementation

{ TAtomicCounter }

constructor TAtomicCounter.Create(InitialValue: Integer);  
begin
  inherited Create;
  FValue := InitialValue;
end;

function TAtomicCounter.Increment: Integer;  
begin
  Result := InterlockedIncrement(FValue);
end;

function TAtomicCounter.Decrement: Integer;  
begin
  Result := InterlockedDecrement(FValue);
end;

function TAtomicCounter.Add(Delta: Integer): Integer;  
begin
  Result := InterlockedExchangeAdd(FValue, Delta) + Delta;
end;

function TAtomicCounter.Get: Integer;  
begin
  // La lecture d'un Integer est atomique sur la plupart des architectures
  Result := FValue;
end;

procedure TAtomicCounter.&Set(Value: Integer);  
begin
  InterlockedExchange(FValue, Value);
end;

function TAtomicCounter.CompareExchange(NewValue, Comparand: Integer): Integer;  
begin
  Result := InterlockedCompareExchange(FValue, NewValue, Comparand);
end;

end.
```

### Utilisation du compteur atomique

```pascal
var
  Counter: TAtomicCounter;
  OldValue: Integer;
begin
  Counter := TAtomicCounter.Create(0);
  try
    // Plusieurs threads peuvent faire ceci en toute sécurité
    Counter.Increment;  // Thread-safe
    Counter.Add(5);     // Thread-safe

    WriteLn('Valeur actuelle : ', Counter.Get);

    // Opération conditionnelle atomique
    OldValue := Counter.CompareExchange(100, 0);
    // Si Counter était 0, il devient 100. Retourne l'ancienne valeur.
  finally
    Counter.Free;
  end;
end;
```

## Cache thread-safe avec expiration

Un exemple plus complexe : un cache avec gestion d'expiration.

```pascal
unit ThreadSafeCache;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs, Generics.Collections, DateUtils;

type
  TCacheItem<T> = record
    Value: T;
    ExpirationTime: TDateTime;
  end;

  TThreadSafeCache<TKey, TValue> = class
  private
    FCache: TDictionary<TKey, TCacheItem<TValue>>;
    FLock: TMultiReadExclusiveWriteSynchronizer;
    FDefaultTTL: Integer; // Time To Live en secondes

    procedure RemoveExpired;
  public
    constructor Create(DefaultTTLSeconds: Integer = 300);
    destructor Destroy; override;

    procedure Add(const Key: TKey; const Value: TValue; TTLSeconds: Integer = -1);
    function TryGetValue(const Key: TKey; out Value: TValue): Boolean;
    function ContainsKey(const Key: TKey): Boolean;
    procedure Remove(const Key: TKey);
    procedure Clear;

    function Count: Integer;
  end;

implementation

{ TThreadSafeCache<TKey, TValue> }

constructor TThreadSafeCache<TKey, TValue>.Create(DefaultTTLSeconds: Integer);  
begin
  inherited Create;
  FCache := TDictionary<TKey, TCacheItem<TValue>>.Create;
  FLock := TMultiReadExclusiveWriteSynchronizer.Create;
  FDefaultTTL := DefaultTTLSeconds;
end;

destructor TThreadSafeCache<TKey, TValue>.Destroy;  
begin
  FLock.BeginWrite;
  try
    FCache.Free;
  finally
    FLock.EndWrite;
    FLock.Free;
  end;
  inherited;
end;

procedure TThreadSafeCache<TKey, TValue>.RemoveExpired;  
var
  KeysToRemove: TList<TKey>;
  Pair: TPair<TKey, TCacheItem<TValue>>;
  Key: TKey;
begin
  // Collecte les clés expirées
  KeysToRemove := TList<TKey>.Create;
  try
    for Pair in FCache do
    begin
      if Now > Pair.Value.ExpirationTime then
        KeysToRemove.Add(Pair.Key);
    end;

    // Supprime les clés expirées
    for Key in KeysToRemove do
      FCache.Remove(Key);
  finally
    KeysToRemove.Free;
  end;
end;

procedure TThreadSafeCache<TKey, TValue>.Add(const Key: TKey; const Value: TValue; TTLSeconds: Integer);  
var
  Item: TCacheItem<TValue>;
  TTL: Integer;
begin
  if TTLSeconds < 0 then
    TTL := FDefaultTTL
  else
    TTL := TTLSeconds;

  Item.Value := Value;
  Item.ExpirationTime := IncSecond(Now, TTL);

  FLock.BeginWrite;
  try
    RemoveExpired; // Nettoyer les éléments expirés
    FCache.AddOrSetValue(Key, Item);
  finally
    FLock.EndWrite;
  end;
end;

function TThreadSafeCache<TKey, TValue>.TryGetValue(const Key: TKey; out Value: TValue): Boolean;  
var
  Item: TCacheItem<TValue>;
begin
  FLock.BeginRead;
  try
    Result := FCache.TryGetValue(Key, Item);

    if Result then
    begin
      // Vérifier si l'élément n'est pas expiré
      if Now > Item.ExpirationTime then
        Result := False
      else
        Value := Item.Value;
    end;
  finally
    FLock.EndRead;
  end;
end;

function TThreadSafeCache<TKey, TValue>.ContainsKey(const Key: TKey): Boolean;  
var
  Value: TValue;
begin
  Result := TryGetValue(Key, Value);
end;

procedure TThreadSafeCache<TKey, TValue>.Remove(const Key: TKey);  
begin
  FLock.BeginWrite;
  try
    FCache.Remove(Key);
  finally
    FLock.EndWrite;
  end;
end;

procedure TThreadSafeCache<TKey, TValue>.Clear;  
begin
  FLock.BeginWrite;
  try
    FCache.Clear;
  finally
    FLock.EndWrite;
  end;
end;

function TThreadSafeCache<TKey, TValue>.Count: Integer;  
begin
  FLock.BeginRead;
  try
    Result := FCache.Count;
  finally
    FLock.EndRead;
  end;
end;

end.
```

### Exemple d'utilisation du cache

```pascal
var
  UserCache: TThreadSafeCache<string, TUserData>;
  User: TUserData;
begin
  UserCache := TThreadSafeCache<string, TUserData>.Create(300); // TTL 5 minutes
  try
    // Ajouter au cache
    User.Name := 'Jean Dupont';
    User.Email := 'jean@example.com';
    UserCache.Add('user123', User);

    // Lire depuis le cache
    if UserCache.TryGetValue('user123', User) then
      WriteLn('Utilisateur trouvé: ', User.Name)
    else
      WriteLn('Utilisateur non trouvé ou expiré');

    // Ajouter avec TTL personnalisé (1 heure)
    UserCache.Add('user456', User, 3600);
  finally
    UserCache.Free;
  end;
end;
```

## Object Pool thread-safe

Un pool d'objets réutilisables pour éviter les créations/destructions fréquentes.

```pascal
unit ThreadSafeObjectPool;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs, Generics.Collections;

type
  TObjectFactory<T: class> = function: T;

  TThreadSafeObjectPool<T: class> = class
  private
    FAvailable: TThreadSafeQueue<T>;
    FInUse: TThreadSafeList<T>;
    FFactory: TObjectFactory<T>;
    FMaxSize: Integer;
    FCurrentSize: Integer;
    FLock: TCriticalSection;
  public
    constructor Create(AFactory: TObjectFactory<T>; AMaxSize: Integer = 10);
    destructor Destroy; override;

    function Acquire(Timeout: Cardinal = INFINITE): T;
    procedure Release(Obj: T);

    function AvailableCount: Integer;
    function InUseCount: Integer;
  end;

implementation

{ TThreadSafeObjectPool<T> }

constructor TThreadSafeObjectPool<T>.Create(AFactory: TObjectFactory<T>; AMaxSize: Integer);  
begin
  inherited Create;
  FFactory := AFactory;
  FMaxSize := AMaxSize;
  FCurrentSize := 0;

  FAvailable := TThreadSafeQueue<T>.Create;
  FInUse := TThreadSafeList<T>.Create;
  FLock := TCriticalSection.Create;
end;

destructor TThreadSafeObjectPool<T>.Destroy;  
var
  Obj: T;
  List: TList<T>;
  i: Integer;
begin
  // Détruire tous les objets disponibles
  while FAvailable.TryDequeue(Obj) do
    Obj.Free;

  // Détruire tous les objets en cours d'utilisation
  List := FInUse.Lock;
  try
    for i := 0 to List.Count - 1 do
      List[i].Free;
    List.Clear;
  finally
    FInUse.Unlock;
  end;

  FAvailable.Free;
  FInUse.Free;
  FLock.Free;

  inherited;
end;

function TThreadSafeObjectPool<T>.Acquire(Timeout: Cardinal): T;  
var
  CanCreate: Boolean;
begin
  Result := nil;

  // Essayer de récupérer un objet disponible
  if FAvailable.TryDequeue(Result) then
  begin
    FInUse.Add(Result);
    Exit;
  end;

  // Vérifier si on peut créer un nouvel objet
  FLock.Enter;
  try
    CanCreate := FCurrentSize < FMaxSize;
    if CanCreate then
      Inc(FCurrentSize);
  finally
    FLock.Leave;
  end;

  if CanCreate then
  begin
    // Créer un nouvel objet
    Result := FFactory();
    FInUse.Add(Result);
  end
  else
  begin
    // Attendre qu'un objet soit libéré
    if FAvailable.Dequeue(Result, Timeout) then
      FInUse.Add(Result);
  end;
end;

procedure TThreadSafeObjectPool<T>.Release(Obj: T);  
begin
  if not Assigned(Obj) then
    Exit;

  // Retirer de la liste "en utilisation"
  FInUse.Remove(Obj);

  // Remettre dans la liste disponible
  FAvailable.Enqueue(Obj);
end;

function TThreadSafeObjectPool<T>.AvailableCount: Integer;  
begin
  Result := FAvailable.Count;
end;

function TThreadSafeObjectPool<T>.InUseCount: Integer;  
begin
  Result := FInUse.Count;
end;

end.
```

### Exemple d'utilisation de l'object pool

```pascal
type
  TDatabaseConnection = class
  public
    ConnectionString: string;
    procedure Connect;
    procedure Disconnect;
  end;

function CreateConnection: TDatabaseConnection;  
begin
  Result := TDatabaseConnection.Create;
  Result.ConnectionString := 'Server=localhost;Database=test';
  Result.Connect;
end;

var
  ConnectionPool: TThreadSafeObjectPool<TDatabaseConnection>;
  Connection: TDatabaseConnection;
begin
  // Créer le pool avec 5 connexions max
  ConnectionPool := TThreadSafeObjectPool<TDatabaseConnection>.Create(
    @CreateConnection,
    5
  );
  try
    // Acquérir une connexion
    Connection := ConnectionPool.Acquire;
    if Assigned(Connection) then
    begin
      try
        // Utiliser la connexion
        // ... exécuter des requêtes ...
      finally
        // TOUJOURS libérer la connexion
        ConnectionPool.Release(Connection);
      end;
    end;
  finally
    ConnectionPool.Free;
  end;
end;
```

## Buffer circulaire thread-safe

Utile pour les flux de données en temps réel (audio, vidéo, capteurs).

```pascal
unit ThreadSafeCircularBuffer;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs;

type
  TThreadSafeCircularBuffer<T> = class
  private
    FBuffer: array of T;
    FCapacity: Integer;
    FReadPos: Integer;
    FWritePos: Integer;
    FCount: Integer;
    FLock: TCriticalSection;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;

    function Write(const Item: T): Boolean;
    function Read(out Item: T): Boolean;
    function Peek(out Item: T): Boolean;

    function Count: Integer;
    function Available: Integer; // Espace disponible
    function IsFull: Boolean;
    function IsEmpty: Boolean;
    procedure Clear;
  end;

implementation

{ TThreadSafeCircularBuffer<T> }

constructor TThreadSafeCircularBuffer<T>.Create(ACapacity: Integer);  
begin
  inherited Create;
  FCapacity := ACapacity;
  SetLength(FBuffer, FCapacity);
  FReadPos := 0;
  FWritePos := 0;
  FCount := 0;
  FLock := TCriticalSection.Create;
end;

destructor TThreadSafeCircularBuffer<T>.Destroy;  
begin
  FLock.Free;
  inherited;
end;

function TThreadSafeCircularBuffer<T>.Write(const Item: T): Boolean;  
begin
  FLock.Enter;
  try
    Result := FCount < FCapacity;

    if Result then
    begin
      FBuffer[FWritePos] := Item;
      FWritePos := (FWritePos + 1) mod FCapacity;
      Inc(FCount);
    end;
  finally
    FLock.Leave;
  end;
end;

function TThreadSafeCircularBuffer<T>.Read(out Item: T): Boolean;  
begin
  FLock.Enter;
  try
    Result := FCount > 0;

    if Result then
    begin
      Item := FBuffer[FReadPos];
      FReadPos := (FReadPos + 1) mod FCapacity;
      Dec(FCount);
    end;
  finally
    FLock.Leave;
  end;
end;

function TThreadSafeCircularBuffer<T>.Peek(out Item: T): Boolean;  
begin
  FLock.Enter;
  try
    Result := FCount > 0;

    if Result then
      Item := FBuffer[FReadPos];
  finally
    FLock.Leave;
  end;
end;

function TThreadSafeCircularBuffer<T>.Count: Integer;  
begin
  FLock.Enter;
  try
    Result := FCount;
  finally
    FLock.Leave;
  end;
end;

function TThreadSafeCircularBuffer<T>.Available: Integer;  
begin
  FLock.Enter;
  try
    Result := FCapacity - FCount;
  finally
    FLock.Leave;
  end;
end;

function TThreadSafeCircularBuffer<T>.IsFull: Boolean;  
begin
  FLock.Enter;
  try
    Result := FCount >= FCapacity;
  finally
    FLock.Leave;
  end;
end;

function TThreadSafeCircularBuffer<T>.IsEmpty: Boolean;  
begin
  FLock.Enter;
  try
    Result := FCount = 0;
  finally
    FLock.Leave;
  end;
end;

procedure TThreadSafeCircularBuffer<T>.Clear;  
begin
  FLock.Enter;
  try
    FReadPos := 0;
    FWritePos := 0;
    FCount := 0;
  finally
    FLock.Leave;
  end;
end;

end.
```

### Utilisation du buffer circulaire

```pascal
var
  AudioBuffer: TThreadSafeCircularBuffer<SmallInt>;
  Sample: SmallInt;

// Thread producteur (capture audio)
procedure AudioCaptureThread;  
begin
  while not Terminated do
  begin
    Sample := CaptureSample(); // Capturer un échantillon

    if not AudioBuffer.Write(Sample) then
      WriteLn('Buffer plein! Échantillon perdu.');
  end;
end;

// Thread consommateur (lecture audio)
procedure AudioPlaybackThread;  
var
  Sample: SmallInt;
begin
  while not Terminated do
  begin
    if AudioBuffer.Read(Sample) then
      PlaySample(Sample)
    else
      Sleep(1); // Buffer vide, attendre
  end;
end;
```

## Comparaison des approches de synchronisation

### Tableau comparatif

| Méthode | Performance | Complexité | Concurrence | Cas d'usage |
|---------|-------------|------------|-------------|-------------|
| **TCriticalSection** | Moyenne | Faible | Faible | Usage général |
| **TMultiReadExclusiveWrite** | Bonne | Moyenne | Bonne | Lectures fréquentes |
| **Opérations atomiques** | Excellente | Faible | Excellente | Compteurs simples |
| **Lock-free** | Excellente | Élevée | Excellente | Haute performance |
| **TThreadList** | Moyenne | Faible | Faible | Listes simples |

### Quand utiliser quoi ?

#### TCriticalSection
```pascal
// Bon pour : modifications simples, courte durée
FCS.Enter;  
try
  Inc(FCounter);
  FList.Add(Item);
finally
  FCS.Leave;
end;
```

#### TMultiReadExclusiveWriteSynchronizer
```pascal
// Bon pour : beaucoup de lectures, peu d'écritures
// Lecture (plusieurs threads simultanés)
FLock.BeginRead;  
try
  Result := FData[Key];
finally
  FLock.EndRead;
end;

// Écriture (exclusif)
FLock.BeginWrite;  
try
  FData[Key] := Value;
finally
  FLock.EndWrite;
end;
```

#### Opérations atomiques
```pascal
// Bon pour : opérations très simples et fréquentes
InterlockedIncrement(FCounter);  
InterlockedExchange(FValue, NewValue);
```

## Patterns de conception avec structures thread-safe

### Pattern Producer-Consumer amélioré

```pascal
type
  TProducerConsumerSystem<T> = class
  private
    FQueue: TThreadSafeQueue<T>;
    FProducers: array of TThread;
    FConsumers: array of TThread;
    FShutdown: Boolean;
  public
    constructor Create(ProducerCount, ConsumerCount, QueueSize: Integer);
    destructor Destroy; override;

    procedure Start;
    procedure Stop;
  end;
```

### Pattern Observer thread-safe

```pascal
type
  { TProc<T> n'existe pas en FreePascal - on définit un type imbriqué }
  TThreadSafeObserver<T> = class
  public type
    TNotifyProc = procedure(const Data: T);
  private
    FObservers: TThreadSafeList<TNotifyProc>;
  public
    procedure Subscribe(Observer: TNotifyProc);
    procedure Unsubscribe(Observer: TNotifyProc);
    procedure Notify(const Data: T);
  end;

procedure TThreadSafeObserver<T>.Notify(const Data: T);  
var
  List: TList<TNotifyProc>;
  Observer: TNotifyProc;
begin
  List := FObservers.Lock;
  try
    for Observer in List do
      Observer(Data);
  finally
    FObservers.Unlock;
  end;
end;
```

## Bonnes pratiques

### 1. Toujours utiliser try...finally

```pascal
// ❌ INCORRECT - Risque de deadlock si exception
FLock.Enter;  
DoSomething(); // Peut lever une exception  
FLock.Leave;

// ✅ CORRECT
FLock.Enter;  
try
  DoSomething();
finally
  FLock.Leave;
end;
```

### 2. Minimiser le temps de verrouillage

```pascal
// ❌ INCORRECT - Verrouillage trop long
FLock.Enter;  
try
  Data := LoadFromDatabase(); // Opération lente !
  FCache.Add(Key, Data);
finally
  FLock.Leave;
end;

// ✅ CORRECT - Charger hors du verrou
Data := LoadFromDatabase();  
FLock.Enter;  
try
  FCache.Add(Key, Data);
finally
  FLock.Leave;
end;
```

### 3. Éviter les verrous imbriqués

```pascal
// ❌ DANGER - Risque de deadlock
FLock1.Enter;  
try
  FLock2.Enter; // Ordre différent ailleurs = deadlock possible
  try
    // ...
  finally
    FLock2.Leave;
  end;
finally
  FLock1.Leave;
end;

// ✅ MIEUX - Un seul verrou ou ordre constant
```

### 4. Documenter les invariants

```pascal
type
  TThreadSafeCounter = class
  private
    FValue: Integer; // Protégé par FLock
    FLock: TCriticalSection;
    // INVARIANT: FValue >= 0 toujours
  public
    function Increment: Integer; // Thread-safe
  end;
```

### 5. Tester avec ThreadSanitizer

```pascal
// Compiler avec options de débogage
// Utiliser Valgrind/Helgrind sur Linux
// Utiliser Thread Sanitizer sur Windows/Linux moderne
```

## Débogage des problèmes de concurrence

### Détecter les race conditions

```pascal
type
  TRaceDetector = class
  private
    FAccessThread: TThreadID;
    FAccessCount: Integer;
  public
    procedure CheckAccess;
  end;

procedure TRaceDetector.CheckAccess;  
var
  CurrentThread: TThreadID;
begin
  CurrentThread := GetCurrentThreadId;

  if (FAccessThread <> 0) and (FAccessThread <> CurrentThread) then
    raise Exception.Create('Race condition détectée!');

  FAccessThread := CurrentThread;
  Inc(FAccessCount);
end;
```

### Logger les accès concurrents

```pascal
procedure LogThreadAccess(const Operation: string);  
begin
  WriteLn(Format('[%s] Thread %d: %s',
    [FormatDateTime('hh:nn:ss.zzz', Now),
     GetCurrentThreadId,
     Operation]));
end;
```

## Différences Windows/Ubuntu

Les structures thread-safe fonctionnent de manière identique sur Windows et Ubuntu grâce à l'abstraction de FreePascal, mais il existe des différences de performance au niveau système :

### Windows

- **Critical Sections** : Optimisées avec spin-locks
- **Slim Reader/Writer Locks** : Disponibles depuis Vista
- **Interlocked operations** : Support natif complet

### Ubuntu/Linux

- **pthread_mutex** : Mutex POSIX
- **pthread_rwlock** : Reader/Writer locks POSIX
- **Atomic operations** : Via GCC builtins

```pascal
{$IFDEF WINDOWS}
// Utiliser les primitives Windows si nécessaire
uses Windows;
{$ENDIF}

{$IFDEF LINUX}
// Utiliser les primitives POSIX si nécessaire
uses BaseUnix, pthreads;
{$ENDIF}
```

## Performance et optimisations

### Mesurer la contention

```pascal
type
  TLockStatistics = class
  private
    FWaitCount: Int64;
    FTotalWaitTime: Int64;
  public
    procedure RecordWait(WaitTime: Int64);
    function AverageWaitTime: Double;
  end;
```

### Techniques d'optimisation

1. **Réduire la granularité des verrous**
2. **Utiliser Read/Write locks pour lectures fréquentes**
3. **Préférer les opérations atomiques quand possible**
4. **Considérer le lock-free pour haute performance**
5. **Éviter les allocations mémoire dans les sections critiques**

## Ressources complémentaires

### Documentation

- **FreePascal RTL** : Classes, SyncObjs
- **Intel Threading Building Blocks** : Concepts applicables
- **C++ Concurrency in Action** : Principes universels

### Outils

- **Valgrind/Helgrind** : Détection de race conditions (Linux)
- **Thread Sanitizer** : Détection de bugs concurrents
- **Intel Inspector** : Analyse de threading (Windows)

## Résumé

Les structures de données thread-safe sont essentielles pour la programmation multi-thread :

- **TThreadList** : Liste simple thread-safe fournie par FreePascal
- **Généricité** : Créer des structures réutilisables avec `<T>`
- **Différents types de verrous** : Critical Section, Read/Write, Atomiques
- **Patterns** : Queue, Stack, Dictionary, Cache, Object Pool
- **Bonnes pratiques** : try...finally, minimiser les verrous, éviter l'imbrication

Une bonne maîtrise des structures thread-safe vous permet de créer des applications multi-thread robustes et performantes !

⏭️ [Lock-free programming](/11-multithreading-concurrence/04-lock-free-programming.md)
