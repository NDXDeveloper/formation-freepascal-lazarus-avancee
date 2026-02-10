🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 20.5 Structures de données optimales

## Introduction

Le choix de la structure de données appropriée est souvent **plus important** que n'importe quelle micro-optimisation. Une mauvaise structure peut rendre un algorithme O(n²) alors qu'une bonne structure le rend O(n) ou même O(1). Ce chapitre explore les structures de données disponibles en FreePascal/Lazarus et comment choisir la bonne pour chaque situation.

**Règle d'or** : Choisir la bonne structure de données peut améliorer les performances de 10x à 1000x, alors qu'optimiser le code d'une mauvaise structure ne donnera qu'un gain de 10% à 50%.

## Complexité algorithmique (rappel)

Avant de choisir une structure, il faut comprendre la **notation Big-O** :

| Notation | Nom | Exemple | Performance |
|----------|-----|---------|-------------|
| **O(1)** | Constant | Accès tableau par index | Excellent ⭐⭐⭐⭐⭐ |
| **O(log n)** | Logarithmique | Recherche binaire | Très bon ⭐⭐⭐⭐ |
| **O(n)** | Linéaire | Parcours de liste | Acceptable ⭐⭐⭐ |
| **O(n log n)** | Log-linéaire | Tri rapide | Acceptable ⭐⭐⭐ |
| **O(n²)** | Quadratique | Tri à bulles | Mauvais ⭐⭐ |
| **O(2ⁿ)** | Exponentiel | Certains algorithmes récursifs | Très mauvais ⭐ |

**Exemple concret** :
```
Pour n = 1,000,000 éléments :

O(1)      : 1 opération  
O(log n)  : 20 opérations  
O(n)      : 1,000,000 opérations  
O(n log n): 20,000,000 opérations  
O(n²)     : 1,000,000,000,000 opérations (1 trillion!)
```

## Tableaux dynamiques (TList, TFPList)

### Caractéristiques

```pascal
uses Classes;

var
  List: TList;
begin
  List := TList.Create;
  try
    List.Add(Pointer(123));
    List.Add(Pointer(456));
    WriteLn('Count: ', List.Count);
  finally
    List.Free;
  end;
end;
```

**Complexité** :
- Accès par index : **O(1)** ✅
- Ajout à la fin : **O(1)** amortisé ✅
- Insertion au début : **O(n)** ❌
- Recherche : **O(n)** ❌
- Suppression : **O(n)** ❌

### Quand utiliser

✅ **Bon pour** :
- Accès fréquent par index
- Ajout à la fin (append)
- Parcours séquentiel
- Tri (après avoir ajouté tous les éléments)

❌ **Mauvais pour** :
- Insertions/suppressions fréquentes au début ou milieu
- Recherches fréquentes d'éléments
- Grande taille avec réallocations fréquentes

### Optimisations

**Pré-allocation** :
```pascal
// ❌ Mauvais : réallocation à chaque ajout
var
  List: TList;
  i: Integer;
begin
  List := TList.Create;
  try
    for i := 1 to 1000000 do
      List.Add(Pointer(i));  // Réallocations multiples
  finally
    List.Free;
  end;
end;
// Temps : ~150 ms

// ✅ Bon : pré-allouer
var
  List: TList;
  i: Integer;
begin
  List := TList.Create;
  try
    List.Capacity := 1000000;  // Allouer d'avance
    for i := 1 to 1000000 do
      List.Add(Pointer(i));    // Pas de réallocation
  finally
    List.Free;
  end;
end;
// Temps : ~50 ms (3x plus rapide)
```

**TFPList générique** :
```pascal
uses FGL;

type
  TIntegerList = specialize TFPGList<Integer>;

var
  List: TIntegerList;
begin
  List := TIntegerList.Create;
  try
    List.Capacity := 1000;  // Pré-allocation
    List.Add(123);
    WriteLn('Item: ', List[0]);
  finally
    List.Free;
  end;
end;
```

## Listes chaînées (TFPList as LinkedList)

### Caractéristiques

```pascal
type
  PNode = ^TNode;
  TNode = record
    Data: Integer;
    Next: PNode;
  end;

var
  Head, Current: PNode;
begin
  // Créer une liste chaînée
  New(Head);
  Head^.Data := 1;
  Head^.Next := nil;

  New(Current);
  Current^.Data := 2;
  Head^.Next := Current;
  Current^.Next := nil;
end;
```

**Complexité** :
- Accès par index : **O(n)** ❌
- Insertion au début : **O(1)** ✅
- Insertion au milieu : **O(1)** (si on a le pointeur) ✅
- Recherche : **O(n)** ❌
- Suppression : **O(1)** (si on a le pointeur) ✅

### Quand utiliser

✅ **Bon pour** :
- Insertions/suppressions fréquentes
- Liste de taille inconnue
- Queue (FIFO) ou pile (LIFO)

❌ **Mauvais pour** :
- Accès aléatoire fréquent
- Cache-unfriendly (voir section 20.2)
- Overhead mémoire (pointeurs)

### Comparaison Array vs Linked List

```pascal
program CompareListTypes;

uses
  Classes, SysUtils, DateUtils;

const
  N = 100000;

// Test 1 : Ajout à la fin
procedure TestArrayAppend;  
var
  List: TList;
  i: Integer;
  StartTime: TDateTime;
begin
  List := TList.Create;
  try
    List.Capacity := N;
    StartTime := Now;
    for i := 1 to N do
      List.Add(Pointer(i));
    WriteLn('Array append: ', MilliSecondsBetween(Now, StartTime), ' ms');
  finally
    List.Free;
  end;
end;

// Test 2 : Insertion au début
procedure TestArrayPrepend;  
var
  List: TList;
  i: Integer;
  StartTime: TDateTime;
begin
  List := TList.Create;
  try
    StartTime := Now;
    for i := 1 to N do
      List.Insert(0, Pointer(i));  // Toujours au début
    WriteLn('Array prepend: ', MilliSecondsBetween(Now, StartTime), ' ms');
  finally
    List.Free;
  end;
end;

// Test 3 : Linked list prepend
procedure TestLinkedListPrepend;  
type
  PNode = ^TNode;
  TNode = record
    Data: Integer;
    Next: PNode;
  end;
var
  Head, NewNode: PNode;
  i: Integer;
  StartTime: TDateTime;
begin
  Head := nil;
  StartTime := Now;
  for i := 1 to N do
  begin
    New(NewNode);
    NewNode^.Data := i;
    NewNode^.Next := Head;
    Head := NewNode;
  end;
  WriteLn('LinkedList prepend: ', MilliSecondsBetween(Now, StartTime), ' ms');

  // Libérer la mémoire
  while Head <> nil do
  begin
    NewNode := Head;
    Head := Head^.Next;
    Dispose(NewNode);
  end;
end;

begin
  TestArrayAppend;        // ~50 ms
  TestArrayPrepend;       // ~15000 ms (300x plus lent!)
  TestLinkedListPrepend;  // ~80 ms
end.

// Résultats typiques :
// Array append:        50 ms      ✅ Excellent
// Array prepend:       15000 ms   ❌ Très mauvais (O(n²))
// LinkedList prepend:  80 ms      ✅ Bon
```

## Tables de hachage (TFPHashList, TDictionary)

### Principe

Une table de hachage utilise une **fonction de hachage** pour mapper les clés vers des indices :

```
Clé "Alice" → Hash(Alice) = 12345 → Index = 12345 mod TableSize
```

### TFPHashList (unité FGL)

```pascal
uses FGL;

type
  TStringIntHash = specialize TFPGMap<string, Integer>;

var
  Hash: TStringIntHash;
begin
  Hash := TStringIntHash.Create;
  try
    // Ajouter des paires clé-valeur
    Hash.Add('Alice', 25);
    Hash.Add('Bob', 30);
    Hash.Add('Charlie', 35);

    // Recherche O(1)
    WriteLn('Age de Bob: ', Hash['Bob']);

    // Vérifier existence
    if Hash.IndexOf('Alice') >= 0 then
      WriteLn('Alice existe');
  finally
    Hash.Free;
  end;
end;
```

### TDictionary (unité Generics.Collections)

```pascal
uses Generics.Collections;

type
  TPersonDict = TDictionary<string, Integer>;

var
  Dict: TPersonDict;
begin
  Dict := TPersonDict.Create;
  try
    Dict.Add('Alice', 25);
    Dict.Add('Bob', 30);

    // Recherche sécurisée
    if Dict.ContainsKey('Alice') then
      WriteLn('Age: ', Dict['Alice']);

    // Ou avec TryGetValue
    var Age: Integer;
    if Dict.TryGetValue('Bob', Age) then
      WriteLn('Age de Bob: ', Age);
  finally
    Dict.Free;
  end;
end;
```

**Complexité** :
- Insertion : **O(1)** moyen ✅
- Recherche : **O(1)** moyen ✅
- Suppression : **O(1)** moyen ✅
- Parcours : **O(n)** ⚠️ (non ordonné)

### Quand utiliser

✅ **Bon pour** :
- Recherches fréquentes par clé
- Associer des valeurs à des clés uniques
- Cache de résultats
- Comptage d'occurrences

❌ **Mauvais pour** :
- Parcours ordonné
- Recherche par valeur (seulement par clé)
- Petites collections (< 10 éléments)

### Exemple : Comptage de mots

```pascal
// ❌ Avec TStringList (O(n²))
procedure CountWordsWithStringList(const Words: TStringList);  
var
  Counts: TStringList;
  i, Index: Integer;
begin
  Counts := TStringList.Create;
  try
    for i := 0 to Words.Count - 1 do
    begin
      Index := Counts.IndexOf(Words[i]);  // O(n) recherche
      if Index >= 0 then
        Counts.Objects[Index] := TObject(Integer(Counts.Objects[Index]) + 1)
      else
        Counts.AddObject(Words[i], TObject(1));
    end;
  finally
    Counts.Free;
  end;
end;
// Temps pour 100,000 mots : ~8000 ms

// ✅ Avec TDictionary (O(n))
uses Generics.Collections;

procedure CountWordsWithDictionary(const Words: TStringList);  
var
  Counts: TDictionary<string, Integer>;
  i, Count: Integer;
  Word: string;
begin
  Counts := TDictionary<string, Integer>.Create;
  try
    for i := 0 to Words.Count - 1 do
    begin
      Word := Words[i];
      if Counts.TryGetValue(Word, Count) then
        Counts[Word] := Count + 1  // O(1)
      else
        Counts.Add(Word, 1);       // O(1)
    end;
  finally
    Counts.Free;
  end;
end;
// Temps pour 100,000 mots : ~50 ms (160x plus rapide!)
```

## Ensembles (TFPHashSet, THashSet)

### Caractéristiques

Un ensemble stocke des valeurs **uniques** sans ordre particulier.

```pascal
uses Generics.Collections;

type
  TIntegerSet = THashSet<Integer>;

var
  Numbers: TIntegerSet;
begin
  Numbers := TIntegerSet.Create;
  try
    Numbers.Add(1);
    Numbers.Add(2);
    Numbers.Add(1);  // Ignoré (déjà présent)

    WriteLn('Count: ', Numbers.Count);  // 2

    if Numbers.Contains(1) then
      WriteLn('1 est présent');
  finally
    Numbers.Free;
  end;
end;
```

**Complexité** :
- Insertion : **O(1)** moyen ✅
- Recherche : **O(1)** moyen ✅
- Suppression : **O(1)** moyen ✅

### Quand utiliser

✅ **Bon pour** :
- Éliminer les doublons
- Test d'appartenance rapide
- Opérations ensemblistes (union, intersection)

### Exemple : Éliminer les doublons

```pascal
// ❌ Avec TList + recherche linéaire (O(n²))
procedure RemoveDuplicatesSlow(var List: TList);  
var
  i, j: Integer;
begin
  for i := List.Count - 1 downto 1 do
    for j := i - 1 downto 0 do
      if List[i] = List[j] then
      begin
        List.Delete(i);
        Break;
      end;
end;
// Temps pour 10,000 éléments : ~5000 ms

// ✅ Avec THashSet (O(n))
uses Generics.Collections;

procedure RemoveDuplicatesFast(var List: TList);  
var
  Seen: THashSet<Pointer>;
  i: Integer;
  NewList: TList;
begin
  Seen := THashSet<Pointer>.Create;
  NewList := TList.Create;
  try
    for i := 0 to List.Count - 1 do
      if not Seen.Contains(List[i]) then
      begin
        Seen.Add(List[i]);
        NewList.Add(List[i]);
      end;

    List.Clear;
    List.AddList(NewList);
  finally
    Seen.Free;
    NewList.Free;
  end;
end;
// Temps pour 10,000 éléments : ~10 ms (500x plus rapide!)
```

## Arbres (TAvlTree, TMap)

### Arbres binaires de recherche équilibrés (AVL)

```pascal
uses AVL_Tree;

type
  TIntegerTree = class(TAVLTree)
    function Compare(Item1, Item2: Pointer): Integer; override;
  end;

function TIntegerTree.Compare(Item1, Item2: Pointer): Integer;  
begin
  Result := Integer(Item1) - Integer(Item2);
end;

var
  Tree: TIntegerTree;
begin
  Tree := TIntegerTree.Create;
  try
    Tree.Add(Pointer(5));
    Tree.Add(Pointer(3));
    Tree.Add(Pointer(7));

    if Tree.Find(Pointer(3)) <> nil then
      WriteLn('3 trouvé');
  finally
    Tree.Free;
  end;
end;
```

**Complexité** :
- Insertion : **O(log n)** ✅
- Recherche : **O(log n)** ✅
- Suppression : **O(log n)** ✅
- Parcours ordonné : **O(n)** ✅

### TMap (unité Generics.Collections)

```pascal
{$mode delphi}
uses Generics.Collections;

type
  TSortedDict = TDictionary<Integer, string>;

var
  Map: TSortedDict;
  Pair: TPair<Integer, string>;
begin
  Map := TSortedDict.Create;
  try
    Map.Add(3, 'Three');
    Map.Add(1, 'One');
    Map.Add(2, 'Two');

    // Parcours (non ordonné avec TDictionary)
    for Pair in Map do
      WriteLn(Pair.Key, ': ', Pair.Value);
  finally
    Map.Free;
  end;
end;
```

### Quand utiliser

✅ **Bon pour** :
- Données triées
- Recherches rapides avec ordre maintenu
- Trouver min/max efficacement
- Recherche par plage (range queries)

❌ **Mauvais pour** :
- Accès aléatoire par index (O(log n) au lieu de O(1))
- Petites collections (overhead de l'arbre)

## Files et piles (Queue, Stack)

### File (FIFO - First In, First Out)

```pascal
uses Generics.Collections;

type
  TIntegerQueue = TQueue<Integer>;

var
  Queue: TIntegerQueue;
begin
  Queue := TIntegerQueue.Create;
  try
    Queue.Enqueue(1);
    Queue.Enqueue(2);
    Queue.Enqueue(3);

    while Queue.Count > 0 do
      WriteLn('Dequeue: ', Queue.Dequeue);
    // Sortie : 1, 2, 3 (ordre d'entrée)
  finally
    Queue.Free;
  end;
end;
```

**Complexité** :
- Enqueue (ajout) : **O(1)** ✅
- Dequeue (retrait) : **O(1)** ✅

### Pile (LIFO - Last In, First Out)

```pascal
uses Generics.Collections;

type
  TIntegerStack = TStack<Integer>;

var
  Stack: TIntegerStack;
begin
  Stack := TIntegerStack.Create;
  try
    Stack.Push(1);
    Stack.Push(2);
    Stack.Push(3);

    while Stack.Count > 0 do
      WriteLn('Pop: ', Stack.Pop);
    // Sortie : 3, 2, 1 (ordre inverse)
  finally
    Stack.Free;
  end;
end;
```

**Complexité** :
- Push (ajout) : **O(1)** ✅
- Pop (retrait) : **O(1)** ✅
- Peek (consulter) : **O(1)** ✅

### Quand utiliser

**File (Queue)** :
- Traitement par ordre d'arrivée
- BFS (Breadth-First Search)
- Files d'attente de tâches
- Buffering

**Pile (Stack)** :
- DFS (Depth-First Search)
- Évaluation d'expressions
- Undo/Redo
- Gestion de la récursion

## Structures de données spécialisées

### Priority Queue (File de priorité)

```pascal
// Implémentation simple avec TList + tri
type
  TPriorityQueue<T> = class
  private
    FList: TList<T>;
    FComparer: IComparer<T>;
  public
    constructor Create(AComparer: IComparer<T>);
    destructor Destroy; override;
    procedure Enqueue(const Item: T);
    function Dequeue: T;
    function Count: Integer;
  end;

constructor TPriorityQueue<T>.Create(AComparer: IComparer<T>);  
begin
  inherited Create;
  FList := TList<T>.Create;
  FComparer := AComparer;
end;

destructor TPriorityQueue<T>.Destroy;  
begin
  FList.Free;
  inherited;
end;

procedure TPriorityQueue<T>.Enqueue(const Item: T);  
begin
  FList.Add(Item);
  FList.Sort(FComparer);  // Maintenir l'ordre
end;

function TPriorityQueue<T>.Dequeue: T;  
begin
  if FList.Count = 0 then
    raise Exception.Create('Queue vide');
  Result := FList[0];
  FList.Delete(0);
end;

function TPriorityQueue<T>.Count: Integer;  
begin
  Result := FList.Count;
end;

// Utilisation
type
  TTask = record
    Name: string;
    Priority: Integer;
  end;

var
  PQ: TPriorityQueue<TTask>;
  Task: TTask;
begin
  PQ := TPriorityQueue<TTask>.Create(
    TComparer<TTask>.Construct(
      function(const A, B: TTask): Integer
      begin
        Result := B.Priority - A.Priority;  // Plus haute priorité d'abord
      end
    )
  );
  try
    Task.Name := 'Low'; Task.Priority := 1;
    PQ.Enqueue(Task);

    Task.Name := 'High'; Task.Priority := 10;
    PQ.Enqueue(Task);

    Task.Name := 'Medium'; Task.Priority := 5;
    PQ.Enqueue(Task);

    while PQ.Count > 0 do
    begin
      Task := PQ.Dequeue;
      WriteLn(Task.Name, ' (', Task.Priority, ')');
    end;
    // Sortie : High (10), Medium (5), Low (1)
  finally
    PQ.Free;
  end;
end;
```

**Complexité** (implémentation naïve ci-dessus) :
- Enqueue : **O(n log n)** (tri à chaque insertion)
- Dequeue : **O(1)**

**Complexité** (avec tas binaire - Binary Heap) :
- Enqueue : **O(log n)** ✅
- Dequeue : **O(log n)** ✅

### Bloom Filter (Filtre de Bloom)

Structure probabiliste pour tester l'appartenance (peut donner des faux positifs, jamais de faux négatifs).

```pascal
type
  TBloomFilter = class
  private
    FBits: TBits;
    FSize: Integer;
    FHashCount: Integer;
    function Hash(const S: string; Seed: Integer): Integer;
  public
    constructor Create(Size, HashCount: Integer);
    destructor Destroy; override;
    procedure Add(const S: string);
    function MightContain(const S: string): Boolean;
  end;

constructor TBloomFilter.Create(Size, HashCount: Integer);  
begin
  inherited Create;
  FSize := Size;
  FHashCount := HashCount;
  FBits := TBits.Create(Size);
end;

destructor TBloomFilter.Destroy;  
begin
  FBits.Free;
  inherited;
end;

function TBloomFilter.Hash(const S: string; Seed: Integer): Integer;  
var
  i: Integer;
begin
  Result := Seed;
  for i := 1 to Length(S) do
    Result := ((Result shl 5) + Result) + Ord(S[i]);
  Result := Abs(Result) mod FSize;
end;

procedure TBloomFilter.Add(const S: string);  
var
  i: Integer;
begin
  for i := 0 to FHashCount - 1 do
    FBits[Hash(S, i)] := True;
end;

function TBloomFilter.MightContain(const S: string): Boolean;  
var
  i: Integer;
begin
  Result := True;
  for i := 0 to FHashCount - 1 do
    if not FBits[Hash(S, i)] then
      Exit(False);
end;

// Utilisation
var
  Filter: TBloomFilter;
begin
  Filter := TBloomFilter.Create(10000, 3);  // 10k bits, 3 hash functions
  try
    Filter.Add('Alice');
    Filter.Add('Bob');

    if Filter.MightContain('Alice') then
      WriteLn('Alice est probablement présente');

    if not Filter.MightContain('Charlie') then
      WriteLn('Charlie est définitivement absent');
  finally
    Filter.Free;
  end;
end;
```

**Avantages** :
- Très compact en mémoire
- O(1) insertion et recherche
- Utile pour filtrer avant recherche coûteuse

**Inconvénients** :
- Faux positifs possibles
- Pas de suppression
- Pas d'énumération

### LRU Cache (Least Recently Used)

```pascal
uses Generics.Collections;

type
  TLRUCache<TKey, TValue> = class
  private
    type
      TCacheNode = record
        Key: TKey;
        Value: TValue;
      end;
  private
    FCapacity: Integer;
    FMap: TDictionary<TKey, Integer>;  // Key → Index dans liste
    FList: TList<TCacheNode>;          // Liste ordonnée par utilisation
    procedure MoveToFront(Index: Integer);
  public
    constructor Create(Capacity: Integer);
    destructor Destroy; override;
    procedure Put(const Key: TKey; const Value: TValue);
    function Get(const Key: TKey; out Value: TValue): Boolean;
  end;

constructor TLRUCache<TKey, TValue>.Create(Capacity: Integer);  
begin
  inherited Create;
  FCapacity := Capacity;
  FMap := TDictionary<TKey, Integer>.Create;
  FList := TList<TCacheNode>.Create;
end;

destructor TLRUCache<TKey, TValue>.Destroy;  
begin
  FMap.Free;
  FList.Free;
  inherited;
end;

procedure TLRUCache<TKey, TValue>.MoveToFront(Index: Integer);  
var
  Node: TCacheNode;
begin
  if Index = 0 then Exit;
  Node := FList[Index];
  FList.Delete(Index);
  FList.Insert(0, Node);
  // Mettre à jour les indices dans la map
  FMap[Node.Key] := 0;
end;

procedure TLRUCache<TKey, TValue>.Put(const Key: TKey; const Value: TValue);  
var
  Node: TCacheNode;
  Index: Integer;
begin
  if FMap.TryGetValue(Key, Index) then
  begin
    // Mettre à jour valeur existante
    Node := FList[Index];
    Node.Value := Value;
    FList[Index] := Node;
    MoveToFront(Index);
  end
  else
  begin
    // Nouvelle entrée
    Node.Key := Key;
    Node.Value := Value;

    if FList.Count >= FCapacity then
    begin
      // Supprimer le moins récemment utilisé (dernier)
      FMap.Remove(FList[FList.Count - 1].Key);
      FList.Delete(FList.Count - 1);
    end;

    FList.Insert(0, Node);
    FMap.Add(Key, 0);
  end;
end;

function TLRUCache<TKey, TValue>.Get(const Key: TKey; out Value: TValue): Boolean;  
var
  Index: Integer;
begin
  Result := FMap.TryGetValue(Key, Index);
  if Result then
  begin
    Value := FList[Index].Value;
    MoveToFront(Index);
  end;
end;

// Utilisation
var
  Cache: TLRUCache<string, Integer>;
  Value: Integer;
begin
  Cache := TLRUCache<string, Integer>.Create(3);  // Capacité = 3
  try
    Cache.Put('A', 1);
    Cache.Put('B', 2);
    Cache.Put('C', 3);

    if Cache.Get('A', Value) then
      WriteLn('A = ', Value);

    Cache.Put('D', 4);  // B est évincé (least recently used)

    if not Cache.Get('B', Value) then
      WriteLn('B n''est plus dans le cache');
  finally
    Cache.Free;
  end;
end;
```

## Tableau comparatif des structures

| Structure | Accès | Recherche | Insertion | Suppression | Ordonné | Usage mémoire |
|-----------|-------|-----------|-----------|-------------|---------|---------------|
| **TList** | O(1) | O(n) | O(n) | O(n) | Non | Faible |
| **Linked List** | O(n) | O(n) | O(1)* | O(1)* | Non | Moyen |
| **TDictionary** | O(1) | O(1) | O(1) | O(1) | Non | Moyen |
| **THashSet** | - | O(1) | O(1) | O(1) | Non | Moyen |
| **AVL Tree** | O(log n) | O(log n) | O(log n) | O(log n) | Oui | Moyen |
| **TQueue** | - | - | O(1) | O(1) | FIFO | Faible |
| **TStack** | - | - | O(1) | O(1) | LIFO | Faible |

\* Si on a le pointeur sur le nœud

## Guide de sélection

### Diagramme de décision

```
Besoin d'accès par clé unique ?
├─ Oui → Besoin d'ordre ?
│  ├─ Oui → AVL Tree / TMap
│  └─ Non → TDictionary / THashSet
└─ Non → Accès par index fréquent ?
   ├─ Oui → Insertions au début fréquentes ?
   │  ├─ Oui → Linked List
   │  └─ Non → TList / Array
   └─ Non → FIFO ou LIFO ?
      ├─ FIFO → TQueue
      ├─ LIFO → TStack
      └─ Autre → Priorité ?
         ├─ Oui → Priority Queue
         └─ Non → Cas spécial (Bloom Filter, LRU, etc.)
```

### Questions à se poser

1. **Quelle opération est la plus fréquente ?**
   - Lecture → Structure optimisée pour lecture (Array, Hash)
   - Écriture → Structure optimisée pour écriture (Linked List, Queue)
   - Les deux → Compromis (Hash Table)

2. **Quelle est la taille typique des données ?**
   - Petit (< 100) → Structure simple (Array)
   - Moyen (100-10,000) → Structure équilibrée (Hash, Tree)
   - Grand (> 10,000) → Structure performante (Hash, B-Tree)

3. **Les données doivent-elles être ordonnées ?**
   - Oui → Tree, Sorted List
   - Non → Hash, Array

4. **Y a-t-il des contraintes mémoire ?**
   - Oui → Array (compact)
   - Non → Hash ou Tree (overhead acceptable)

## Cas pratiques d'utilisation

### Cas 1 : Cache de résultats de calcul

**Problème** : Fonction coûteuse appelée souvent avec les mêmes paramètres.

**Solution** : TDictionary comme cache

```pascal
uses Generics.Collections;

type
  TCalculator = class
  private
    FCache: TDictionary<Integer, Double>;
  public
    constructor Create;
    destructor Destroy; override;
    function ExpensiveCalculation(N: Integer): Double;
  end;

constructor TCalculator.Create;  
begin
  inherited;
  FCache := TDictionary<Integer, Double>.Create;
end;

destructor TCalculator.Destroy;  
begin
  FCache.Free;
  inherited;
end;

function TCalculator.ExpensiveCalculation(N: Integer): Double;  
var
  CachedResult: Double;
  i: Integer;
begin
  // Vérifier le cache
  if FCache.TryGetValue(N, CachedResult) then
  begin
    WriteLn('Cache hit pour N=', N);
    Exit(CachedResult);
  end;

  // Calcul coûteux (simulation)
  WriteLn('Calcul pour N=', N);
  Result := 0;
  for i := 1 to N do
    Result := Result + Sqrt(i) * Sin(i);

  // Mettre en cache
  FCache.Add(N, Result);
end;

// Utilisation
var
  Calc: TCalculator;
begin
  Calc := TCalculator.Create;
  try
    WriteLn(Calc.ExpensiveCalculation(1000):0:2);  // Calcul
    WriteLn(Calc.ExpensiveCalculation(1000):0:2);  // Cache hit!
    WriteLn(Calc.ExpensiveCalculation(2000):0:2);  // Calcul
    WriteLn(Calc.ExpensiveCalculation(1000):0:2);  // Cache hit!
  finally
    Calc.Free;
  end;
end;

// Sortie :
// Calcul pour N=1000
// 15234.56
// Cache hit pour N=1000
// 15234.56
// Calcul pour N=2000
// 42876.89
// Cache hit pour N=1000
// 15234.56
```

### Cas 2 : File d'attente de tâches

**Problème** : Traiter des tâches dans l'ordre d'arrivée.

**Solution** : TQueue

```pascal
uses Generics.Collections;

type
  TTask = record
    ID: Integer;
    Description: string;
  end;

  TTaskQueue = class
  private
    FQueue: TQueue<TTask>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddTask(ID: Integer; const Description: string);
    function ProcessNextTask: Boolean;
  end;

constructor TTaskQueue.Create;  
begin
  inherited;
  FQueue := TQueue<TTask>.Create;
end;

destructor TTaskQueue.Destroy;  
begin
  FQueue.Free;
  inherited;
end;

procedure TTaskQueue.AddTask(ID: Integer; const Description: string);  
var
  Task: TTask;
begin
  Task.ID := ID;
  Task.Description := Description;
  FQueue.Enqueue(Task);
  WriteLn('Tâche ajoutée: ', Description);
end;

function TTaskQueue.ProcessNextTask: Boolean;  
var
  Task: TTask;
begin
  Result := FQueue.Count > 0;
  if Result then
  begin
    Task := FQueue.Dequeue;
    WriteLn('Traitement: ', Task.Description);
    // Traiter la tâche ici
    Sleep(100);  // Simulation
  end;
end;

// Utilisation
var
  Queue: TTaskQueue;
begin
  Queue := TTaskQueue.Create;
  try
    Queue.AddTask(1, 'Charger fichier');
    Queue.AddTask(2, 'Traiter données');
    Queue.AddTask(3, 'Sauvegarder résultat');

    WriteLn;
    while Queue.ProcessNextTask do
      WriteLn('Restant: ', Queue.FQueue.Count);
  finally
    Queue.Free;
  end;
end;

// Sortie :
// Tâche ajoutée: Charger fichier
// Tâche ajoutée: Traiter données
// Tâche ajoutée: Sauvegarder résultat
//
// Traitement: Charger fichier
// Restant: 2
// Traitement: Traiter données
// Restant: 1
// Traitement: Sauvegarder résultat
// Restant: 0
```

### Cas 3 : Annuaire avec recherche rapide

**Problème** : Annuaire de milliers de contacts avec recherche par nom.

**Solution** : TDictionary

```pascal
uses Generics.Collections, SysUtils, DateUtils;

type
  TContact = record
    Name: string;
    Email: string;
    Phone: string;
  end;

  TAddressBook = class
  private
    FContacts: TDictionary<string, TContact>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddContact(const Name, Email, Phone: string);
    function FindContact(const Name: string; out Contact: TContact): Boolean;
    function Count: Integer;
  end;

constructor TAddressBook.Create;  
begin
  inherited;
  FContacts := TDictionary<string, TContact>.Create;
end;

destructor TAddressBook.Destroy;  
begin
  FContacts.Free;
  inherited;
end;

procedure TAddressBook.AddContact(const Name, Email, Phone: string);  
var
  Contact: TContact;
begin
  Contact.Name := Name;
  Contact.Email := Email;
  Contact.Phone := Phone;
  FContacts.AddOrSetValue(Name, Contact);
end;

function TAddressBook.FindContact(const Name: string; out Contact: TContact): Boolean;  
begin
  Result := FContacts.TryGetValue(Name, Contact);
end;

function TAddressBook.Count: Integer;  
begin
  Result := FContacts.Count;
end;

// Utilisation
var
  Book: TAddressBook;
  Contact: TContact;
  i: Integer;
  StartTime: TDateTime;
begin
  Book := TAddressBook.Create;
  try
    // Ajouter 100,000 contacts
    WriteLn('Ajout de 100,000 contacts...');
    StartTime := Now;
    for i := 1 to 100000 do
      Book.AddContact('Person' + IntToStr(i),
                      'person' + IntToStr(i) + '@email.com',
                      '555-' + IntToStr(i));
    WriteLn('Temps: ', MilliSecondsBetween(Now, StartTime), ' ms');

    // Recherche rapide
    WriteLn;
    WriteLn('Recherche de Person50000...');
    StartTime := Now;
    if Book.FindContact('Person50000', Contact) then
    begin
      WriteLn('Temps: ', MilliSecondsBetween(Now, StartTime), ' ms');
      WriteLn('Email: ', Contact.Email);
      WriteLn('Phone: ', Contact.Phone);
    end;
  finally
    Book.Free;
  end;
end;

// Sortie typique :
// Ajout de 100,000 contacts...
// Temps: 85 ms
//
// Recherche de Person50000...
// Temps: 0 ms  (< 1ms, O(1)!)
// Email: person50000@email.com
// Phone: 555-50000
```

### Cas 4 : Historique avec Undo/Redo

**Problème** : Implémenter undo/redo dans un éditeur.

**Solution** : Deux TStack (undo et redo)

```pascal
uses Generics.Collections;

type
  TCommand = record
    Action: string;
    Data: string;
  end;

  TCommandHistory = class
  private
    FUndoStack: TStack<TCommand>;
    FRedoStack: TStack<TCommand>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute(const Action, Data: string);
    function Undo: Boolean;
    function Redo: Boolean;
    function CanUndo: Boolean;
    function CanRedo: Boolean;
  end;

constructor TCommandHistory.Create;  
begin
  inherited;
  FUndoStack := TStack<TCommand>.Create;
  FRedoStack := TStack<TCommand>.Create;
end;

destructor TCommandHistory.Destroy;  
begin
  FUndoStack.Free;
  FRedoStack.Free;
  inherited;
end;

procedure TCommandHistory.Execute(const Action, Data: string);  
var
  Cmd: TCommand;
begin
  Cmd.Action := Action;
  Cmd.Data := Data;
  FUndoStack.Push(Cmd);
  FRedoStack.Clear;  // Nouvelle action annule le redo
  WriteLn('Exécuté: ', Action, ' (', Data, ')');
end;

function TCommandHistory.Undo: Boolean;  
var
  Cmd: TCommand;
begin
  Result := FUndoStack.Count > 0;
  if Result then
  begin
    Cmd := FUndoStack.Pop;
    FRedoStack.Push(Cmd);
    WriteLn('Annulé: ', Cmd.Action, ' (', Cmd.Data, ')');
  end;
end;

function TCommandHistory.Redo: Boolean;  
var
  Cmd: TCommand;
begin
  Result := FRedoStack.Count > 0;
  if Result then
  begin
    Cmd := FRedoStack.Pop;
    FUndoStack.Push(Cmd);
    WriteLn('Refait: ', Cmd.Action, ' (', Cmd.Data, ')');
  end;
end;

function TCommandHistory.CanUndo: Boolean;  
begin
  Result := FUndoStack.Count > 0;
end;

function TCommandHistory.CanRedo: Boolean;  
begin
  Result := FRedoStack.Count > 0;
end;

// Utilisation
var
  History: TCommandHistory;
begin
  History := TCommandHistory.Create;
  try
    History.Execute('Insert', 'Hello');
    History.Execute('Insert', ' World');
    History.Execute('Delete', '!');

    WriteLn;
    History.Undo;  // Annule Delete
    History.Undo;  // Annule Insert World

    WriteLn;
    History.Redo;  // Refait Insert World

    WriteLn;
    History.Execute('Insert', '!!!');  // Redo effacé

    WriteLn;
    WriteLn('Can Undo: ', History.CanUndo);
    WriteLn('Can Redo: ', History.CanRedo);
  finally
    History.Free;
  end;
end;
```

### Cas 5 : Index de recherche de texte

**Problème** : Rechercher efficacement des mots dans une grande collection de documents.

**Solution** : TDictionary<string, TList<Integer>> (Index inversé)

```pascal
uses Generics.Collections, Classes, SysUtils;

type
  TDocumentIndex = class
  private
    FIndex: TDictionary<string, TList<Integer>>;  // Mot → Liste de DocIDs
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddDocument(DocID: Integer; const Content: string);
    function Search(const Word: string): TList<Integer>;
  end;

constructor TDocumentIndex.Create;  
begin
  inherited;
  FIndex := TDictionary<string, TList<Integer>>.Create;
end;

destructor TDocumentIndex.Destroy;  
var
  DocList: TList<Integer>;
begin
  // Libérer toutes les listes
  for DocList in FIndex.Values do
    DocList.Free;
  FIndex.Free;
  inherited;
end;

procedure TDocumentIndex.AddDocument(DocID: Integer; const Content: string);  
var
  Words: TStringList;
  i: Integer;
  Word: string;
  DocList: TList<Integer>;
begin
  Words := TStringList.Create;
  try
    // Séparer en mots (simplification)
    Words.Delimiter := ' ';
    Words.DelimitedText := LowerCase(Content);

    for i := 0 to Words.Count - 1 do
    begin
      Word := Trim(Words[i]);
      if Word = '' then Continue;

      // Ajouter le mot à l'index
      if not FIndex.TryGetValue(Word, DocList) then
      begin
        DocList := TList<Integer>.Create;
        FIndex.Add(Word, DocList);
      end;

      // Ajouter le document à la liste (si pas déjà présent)
      if DocList.IndexOf(DocID) < 0 then
        DocList.Add(DocID);
    end;
  finally
    Words.Free;
  end;
end;

function TDocumentIndex.Search(const Word: string): TList<Integer>;  
begin
  if not FIndex.TryGetValue(LowerCase(Word), Result) then
    Result := nil;
end;

// Utilisation
var
  Index: TDocumentIndex;
  Results: TList<Integer>;
  DocID: Integer;
begin
  Index := TDocumentIndex.Create;
  try
    // Indexer des documents
    Index.AddDocument(1, 'FreePascal is a great programming language');
    Index.AddDocument(2, 'Lazarus IDE makes FreePascal easy to use');
    Index.AddDocument(3, 'Programming in Pascal is fun');

    // Rechercher
    WriteLn('Recherche de "FreePascal":');
    Results := Index.Search('FreePascal');
    if Results <> nil then
      for DocID in Results do
        WriteLn('  Trouvé dans document ', DocID);

    WriteLn;
    WriteLn('Recherche de "programming":');
    Results := Index.Search('programming');
    if Results <> nil then
      for DocID in Results do
        WriteLn('  Trouvé dans document ', DocID);
  finally
    Index.Free;
  end;
end;

// Sortie :
// Recherche de "FreePascal":
//   Trouvé dans document 1
//   Trouvé dans document 2
//
// Recherche de "programming":
//   Trouvé dans document 1
//   Trouvé dans document 3
```

## Optimisations de structures de données

### 1. Réduire les allocations

**❌ Mauvais : Allocations multiples**
```pascal
type
  TPoint = class
    X, Y: Double;
  end;

var
  Points: TList<TPoint>;
  i: Integer;
begin
  Points := TList<TPoint>.Create;
  try
    for i := 1 to 10000 do
      Points.Add(TPoint.Create);  // 10,000 allocations
  finally
    for i := 0 to Points.Count - 1 do
      Points[i].Free;
    Points.Free;
  end;
end;
```

**✅ Bon : Record au lieu de classe**
```pascal
type
  TPoint = record
    X, Y: Double;
  end;

var
  Points: TList<TPoint>;
  Point: TPoint;
  i: Integer;
begin
  Points := TList<TPoint>.Create;
  try
    Points.Capacity := 10000;  // Pré-allouer
    for i := 1 to 10000 do
    begin
      Point.X := i;
      Point.Y := i * 2;
      Points.Add(Point);  // Copie du record (pas d'allocation)
    end;
  finally
    Points.Free;
  end;
end;
// Gain : 10x à 50x plus rapide
```

### 2. Utiliser des structures compactes

```pascal
// ❌ Mauvais : Gaspillage mémoire
type
  TPersonBad = record
    Name: string;        // ~16 bytes (pointeur + compteur)
    Age: Integer;        // 4 bytes
    IsActive: Boolean;   // 1 byte
                         // Padding: 3 bytes
  end;  // Total : ~24 bytes par personne

// ✅ Bon : Compact
type
  TPersonGood = record
    Name: ShortString;   // 256 bytes (mais stack, pas heap)
    Age: Byte;           // 1 byte (suffisant pour 0-255)
    IsActive: Boolean;   // 1 byte
  end;  // Total : 258 bytes mais pas d'allocations heap

// ✅ Encore mieux pour grandes collections
type
  TPersonCompact = packed record
    NameIndex: Word;     // Index dans un tableau de noms
    Age: Byte;
    Flags: Byte;         // Peut stocker plusieurs booléens
  end;  // Total : 4 bytes par personne
```

### 3. Exploiter la localité de cache

**Structure of Arrays (SoA) vs Array of Structures (AoS)**

```pascal
// ❌ Array of Structures (AoS)
type
  TParticle = record
    X, Y, Z: Single;
    VelX, VelY, VelZ: Single;
    Mass: Single;
  end;

var
  Particles: array[0..9999] of TParticle;

// Mise à jour positions
for i := 0 to 9999 do  
begin
  Particles[i].X := Particles[i].X + Particles[i].VelX;
  Particles[i].Y := Particles[i].Y + Particles[i].VelY;
  Particles[i].Z := Particles[i].Z + Particles[i].VelZ;
end;
// Charge aussi Mass (inutile ici) → gaspillage de cache

// ✅ Structure of Arrays (SoA)
type
  TParticleSystem = record
    X, Y, Z: array[0..9999] of Single;
    VelX, VelY, VelZ: array[0..9999] of Single;
    Mass: array[0..9999] of Single;
  end;

var
  System: TParticleSystem;

// Mise à jour positions
for i := 0 to 9999 do  
begin
  System.X[i] := System.X[i] + System.VelX[i];
  System.Y[i] := System.Y[i] + System.VelY[i];
  System.Z[i] := System.Z[i] + System.VelZ[i];
end;
// Ne charge que les données nécessaires → meilleur cache
// + Vectorisation SIMD possible

// Gain : 2x à 3x plus rapide
```

### 4. Pooling d'objets

```pascal
type
  TObjectPool<T: class, constructor> = class
  private
    FPool: TStack<T>;
    FMaxSize: Integer;
  public
    constructor Create(MaxSize: Integer);
    destructor Destroy; override;
    function Acquire: T;
    procedure Release(Obj: T);
  end;

constructor TObjectPool<T>.Create(MaxSize: Integer);  
begin
  inherited Create;
  FMaxSize := MaxSize;
  FPool := TStack<T>.Create;
end;

destructor TObjectPool<T>.Destroy;  
var
  Obj: T;
begin
  while FPool.Count > 0 do
  begin
    Obj := FPool.Pop;
    Obj.Free;
  end;
  FPool.Free;
  inherited;
end;

function TObjectPool<T>.Acquire: T;  
begin
  if FPool.Count > 0 then
    Result := FPool.Pop
  else
    Result := T.Create;
end;

procedure TObjectPool<T>.Release(Obj: T);  
begin
  if FPool.Count < FMaxSize then
    FPool.Push(Obj)
  else
    Obj.Free;
end;

// Utilisation
type
  TMyObject = class
    Data: array[0..999] of Integer;
    procedure Reset;
  end;

var
  Pool: TObjectPool<TMyObject>;
  Obj: TMyObject;
  i: Integer;
begin
  Pool := TObjectPool<TMyObject>.Create(100);
  try
    for i := 1 to 10000 do
    begin
      Obj := Pool.Acquire;
      try
        Obj.Reset;
        // Utiliser Obj
      finally
        Pool.Release(Obj);
      end;
    end;
  finally
    Pool.Free;
  end;
end;
// Gain : Crée seulement ~100 objets au lieu de 10,000
```

## Structures de données custom

### Exemple : Circular Buffer (Ring Buffer)

```pascal
type
  TCircularBuffer<T> = class
  private
    FBuffer: array of T;
    FCapacity: Integer;
    FHead: Integer;  // Position d'écriture
    FTail: Integer;  // Position de lecture
    FCount: Integer;
  public
    constructor Create(Capacity: Integer);
    function Push(const Item: T): Boolean;
    function Pop(out Item: T): Boolean;
    function IsFull: Boolean;
    function IsEmpty: Boolean;
    property Count: Integer read FCount;
  end;

constructor TCircularBuffer<T>.Create(Capacity: Integer);  
begin
  inherited Create;
  FCapacity := Capacity;
  SetLength(FBuffer, Capacity);
  FHead := 0;
  FTail := 0;
  FCount := 0;
end;

function TCircularBuffer<T>.Push(const Item: T): Boolean;  
begin
  Result := not IsFull;
  if Result then
  begin
    FBuffer[FHead] := Item;
    FHead := (FHead + 1) mod FCapacity;
    Inc(FCount);
  end;
end;

function TCircularBuffer<T>.Pop(out Item: T): Boolean;  
begin
  Result := not IsEmpty;
  if Result then
  begin
    Item := FBuffer[FTail];
    FTail := (FTail + 1) mod FCapacity;
    Dec(FCount);
  end;
end;

function TCircularBuffer<T>.IsFull: Boolean;  
begin
  Result := FCount = FCapacity;
end;

function TCircularBuffer<T>.IsEmpty: Boolean;  
begin
  Result := FCount = 0;
end;

// Utilisation
var
  Buffer: TCircularBuffer<Integer>;
  Value: Integer;
begin
  Buffer := TCircularBuffer<Integer>.Create(5);
  try
    Buffer.Push(1);
    Buffer.Push(2);
    Buffer.Push(3);

    if Buffer.Pop(Value) then
      WriteLn('Pop: ', Value);  // 1

    Buffer.Push(4);
    Buffer.Push(5);
    Buffer.Push(6);

    while Buffer.Pop(Value) do
      WriteLn('Pop: ', Value);  // 2, 3, 4, 5, 6
  finally
    Buffer.Free;
  end;
end;
```

**Avantages** :
- Pas de réallocation
- O(1) push et pop
- Idéal pour buffers audio/vidéo

## Profiling de structures de données

### Mesurer l'utilisation mémoire

```pascal
function GetMemoryUsed: Int64;
{$IFDEF WINDOWS}
var
  MemCounters: TProcessMemoryCounters;
begin
  MemCounters.cb := SizeOf(MemCounters);
  if GetProcessMemoryInfo(GetCurrentProcess, @MemCounters, SizeOf(MemCounters)) then
    Result := MemCounters.WorkingSetSize
  else
    Result := 0;
end;
{$ELSE}
// Linux : lire /proc/self/status
begin
  Result := 0;  // Implémentation simplifiée
end;
{$ENDIF}

// Utilisation
var
  MemBefore, MemAfter: Int64;
  List: TList<Integer>;
  i: Integer;
begin
  MemBefore := GetMemoryUsed;

  List := TList<Integer>.Create;
  try
    List.Capacity := 1000000;
    for i := 1 to 1000000 do
      List.Add(i);

    MemAfter := GetMemoryUsed;
    WriteLn('Mémoire utilisée: ', (MemAfter - MemBefore) div 1024, ' KB');
  finally
    List.Free;
  end;
end;
```

### Comparer les performances

```pascal
procedure BenchmarkStructure(const Name: string; Proc: TProcedure);  
var
  StartTime: TDateTime;
  ElapsedMs: Int64;
begin
  StartTime := Now;
  Proc();
  ElapsedMs := MilliSecondsBetween(Now, StartTime);
  WriteLn(Format('%-20s: %6d ms', [Name, ElapsedMs]));
end;

// Utilisation
const
  N = 100000;

begin
  WriteLn('Benchmark des structures (', N, ' éléments)');
  WriteLn;

  BenchmarkStructure('TList Insert',
    procedure
    var List: TList; i: Integer;
    begin
      List := TList.Create;
      try
        for i := 1 to N do
          List.Insert(0, Pointer(i));
      finally
        List.Free;
      end;
    end);

  BenchmarkStructure('TList Append',
    procedure
    var List: TList; i: Integer;
    begin
      List := TList.Create;
      try
        List.Capacity := N;
        for i := 1 to N do
          List.Add(Pointer(i));
      finally
        List.Free;
      end;
    end);

  BenchmarkStructure('TDictionary',
    procedure
    var Dict: TDictionary<Integer, Integer>; i: Integer;
    begin
      Dict := TDictionary<Integer, Integer>.Create;
      try
        for i := 1 to N do
          Dict.Add(i, i);
      finally
        Dict.Free;
      end;
    end);
end;

// Sortie typique :
// Benchmark des structures (100000 éléments)
//
// TList Insert        :  15234 ms
// TList Append        :     52 ms
// TDictionary         :     68 ms
```

## Checklist de sélection

### Avant de choisir une structure

- [ ] Quelle est l'opération la plus fréquente ?
- [ ] Combien d'éléments typiquement ?
- [ ] Les données doivent-elles être ordonnées ?
- [ ] Y a-t-il des contraintes mémoire ?
- [ ] Besoin d'accès concurrent (multi-threading) ?

### Après avoir choisi

- [ ] Pré-allouer si la taille est connue
- [ ] Utiliser des records au lieu de classes quand possible
- [ ] Profiler l'utilisation mémoire
- [ ] Mesurer les performances réelles
- [ ] Envisager le pooling pour objets fréquemment créés/détruits

## Résumé

### Structures par cas d'usage

| Cas d'usage | Structure recommandée | Complexité |
|-------------|----------------------|------------|
| **Liste simple** | TList, Array | O(1) accès |
| **Recherche par clé** | TDictionary | O(1) |
| **Ensemble** | THashSet | O(1) |
| **Tri maintenu** | AVL Tree | O(log n) |
| **FIFO** | TQueue | O(1) |
| **LIFO** | TStack | O(1) |
| **Priorités** | Priority Queue | O(log n) |
| **Cache** | LRU Cache | O(1) |
| **Index texte** | Inverted Index | O(1) recherche |

### Gains typiques

| Optimisation | Gain |
|--------------|------|
| **Structure adaptée** | 10x-1000x |
| **Pré-allocation** | 2x-5x |
| **Record vs Class** | 5x-20x |
| **SoA vs AoS** | 2x-3x |
| **Object Pooling** | 10x-50x |
| **Localité cache** | 2x-10x |

### Points clés

✅ **Choisir d'abord la bonne structure** (impact maximal)

✅ **Complexité O() avant micro-optimisations**

✅ **Pré-allouer quand la taille est connue**

✅ **Records > Classes pour données simples**

✅ **Hash tables pour recherches fréquentes**

✅ **Arrays pour accès par index**

✅ **Profiler avant d'optimiser**

## Structures spécifiques Windows vs Linux

### Différences de performance

Les structures de données génériques (TList, TDictionary) ont des performances similaires sur Windows et Linux. Cependant, certaines opérations système diffèrent :

| Opération | Windows | Linux | Recommandation |
|-----------|---------|-------|----------------|
| **Allocation mémoire** | HeapAlloc | malloc | Similaire |
| **Lecture fichier** | ReadFile | read | Linux plus rapide pour petites lectures |
| **Thread-safe collections** | Critical Sections | pthread_mutex | Similaire |
| **Memory mapping** | CreateFileMapping | mmap | Linux plus flexible |

### Structures système-spécifiques

**Windows : TRegistry**
```pascal
{$IFDEF WINDOWS}
uses Registry;

procedure ReadFromRegistry;  
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey('Software\MyApp', False) then
    begin
      WriteLn('Setting: ', Reg.ReadString('Setting'));
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;
{$ENDIF}
```

**Linux : Configuration files (INI, JSON, XML)**
```pascal
{$IFDEF LINUX}
uses IniFiles;

procedure ReadFromConfig;  
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create('/etc/myapp/config.ini');
  try
    WriteLn('Setting: ', Ini.ReadString('General', 'Setting', 'default'));
  finally
    Ini.Free;
  end;
end;
{$ENDIF}
```

## Structures pour cas particuliers

### 1. Géospatial : QuadTree

Pour données avec coordonnées 2D (carte, jeu vidéo).

```pascal
type
  TPoint2D = record
    X, Y: Double;
    Data: Pointer;
  end;

  PQuadNode = ^TQuadNode;
  TQuadNode = record
    Bounds: record
      MinX, MinY, MaxX, MaxY: Double;
    end;
    Points: TList;
    NW, NE, SW, SE: PQuadNode;  // Sous-arbres
    IsLeaf: Boolean;
  end;

  TQuadTree = class
  private
    FRoot: PQuadNode;
    FMaxPoints: Integer;  // Points par nœud avant subdivision
    procedure Split(Node: PQuadNode);
    procedure InsertIntoNode(Node: PQuadNode; const Point: TPoint2D);
  public
    constructor Create(MinX, MinY, MaxX, MaxY: Double; MaxPoints: Integer = 4);
    destructor Destroy; override;
    procedure Insert(const Point: TPoint2D);
    function QueryRange(MinX, MinY, MaxX, MaxY: Double): TList;
  end;

constructor TQuadTree.Create(MinX, MinY, MaxX, MaxY: Double; MaxPoints: Integer);  
begin
  inherited Create;
  FMaxPoints := MaxPoints;
  New(FRoot);
  FRoot^.Bounds.MinX := MinX;
  FRoot^.Bounds.MinY := MinY;
  FRoot^.Bounds.MaxX := MaxX;
  FRoot^.Bounds.MaxY := MaxY;
  FRoot^.Points := TList.Create;
  FRoot^.NW := nil;
  FRoot^.NE := nil;
  FRoot^.SW := nil;
  FRoot^.SE := nil;
  FRoot^.IsLeaf := True;
end;

procedure TQuadTree.Split(Node: PQuadNode);  
var
  MidX, MidY: Double;
begin
  if not Node^.IsLeaf then Exit;

  MidX := (Node^.Bounds.MinX + Node^.Bounds.MaxX) / 2;
  MidY := (Node^.Bounds.MinY + Node^.Bounds.MaxY) / 2;

  // Créer 4 sous-quadrants
  New(Node^.NW);
  Node^.NW^.Bounds.MinX := Node^.Bounds.MinX;
  Node^.NW^.Bounds.MinY := MidY;
  Node^.NW^.Bounds.MaxX := MidX;
  Node^.NW^.Bounds.MaxY := Node^.Bounds.MaxY;
  Node^.NW^.Points := TList.Create;
  Node^.NW^.IsLeaf := True;

  // ... créer NE, SW, SE similairement ...

  Node^.IsLeaf := False;
end;

procedure TQuadTree.Insert(const Point: TPoint2D);  
begin
  InsertIntoNode(FRoot, Point);
end;

// Utilisation : Recherche de points dans une région
var
  Tree: TQuadTree;
  Point: TPoint2D;
  Results: TList;
begin
  Tree := TQuadTree.Create(0, 0, 1000, 1000);
  try
    // Insérer des points
    Point.X := 100; Point.Y := 200;
    Tree.Insert(Point);

    Point.X := 500; Point.Y := 600;
    Tree.Insert(Point);

    // Rechercher dans une région
    Results := Tree.QueryRange(0, 0, 300, 300);
    WriteLn('Points trouvés: ', Results.Count);
  finally
    Tree.Free;
  end;
end;
```

**Complexité** :
- Insertion : O(log n) moyen
- Recherche par région : O(log n + k) où k = résultats
- Très efficace pour données spatiales

### 2. Graphes

```pascal
uses Generics.Collections;

type
  TGraph<T> = class
  private
    FAdjacencyList: TDictionary<T, TList<T>>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddVertex(const Vertex: T);
    procedure AddEdge(const From, ToVertex: T);
    function GetNeighbors(const Vertex: T): TList<T>;
    function BFS(const Start, Target: T): Boolean;  // Breadth-First Search
  end;

constructor TGraph<T>.Create;  
begin
  inherited;
  FAdjacencyList := TDictionary<T, TList<T>>.Create;
end;

destructor TGraph<T>.Destroy;  
var
  List: TList<T>;
begin
  for List in FAdjacencyList.Values do
    List.Free;
  FAdjacencyList.Free;
  inherited;
end;

procedure TGraph<T>.AddVertex(const Vertex: T);  
begin
  if not FAdjacencyList.ContainsKey(Vertex) then
    FAdjacencyList.Add(Vertex, TList<T>.Create);
end;

procedure TGraph<T>.AddEdge(const From, ToVertex: T);  
var
  List: TList<T>;
begin
  AddVertex(From);
  AddVertex(ToVertex);

  List := FAdjacencyList[From];
  if List.IndexOf(ToVertex) < 0 then
    List.Add(ToVertex);
end;

function TGraph<T>.BFS(const Start, Target: T): Boolean;  
var
  Queue: TQueue<T>;
  Visited: THashSet<T>;
  Current, Neighbor: T;
  Neighbors: TList<T>;
begin
  Result := False;
  Queue := TQueue<T>.Create;
  Visited := THashSet<T>.Create;
  try
    Queue.Enqueue(Start);
    Visited.Add(Start);

    while Queue.Count > 0 do
    begin
      Current := Queue.Dequeue;

      if Current = Target then
        Exit(True);

      if FAdjacencyList.TryGetValue(Current, Neighbors) then
        for Neighbor in Neighbors do
          if not Visited.Contains(Neighbor) then
          begin
            Visited.Add(Neighbor);
            Queue.Enqueue(Neighbor);
          end;
    end;
  finally
    Queue.Free;
    Visited.Free;
  end;
end;

// Utilisation : Réseau social
var
  SocialGraph: TGraph<string>;
begin
  SocialGraph := TGraph<string>.Create;
  try
    // Créer des connexions
    SocialGraph.AddEdge('Alice', 'Bob');
    SocialGraph.AddEdge('Bob', 'Charlie');
    SocialGraph.AddEdge('Alice', 'David');
    SocialGraph.AddEdge('Charlie', 'David');

    // Rechercher un chemin
    if SocialGraph.BFS('Alice', 'Charlie') then
      WriteLn('Alice et Charlie sont connectés')
    else
      WriteLn('Pas de connexion');
  finally
    SocialGraph.Free;
  end;
end;
```

### 3. Trie (Arbre de préfixes)

Pour autocomplétion, dictionnaires, recherche de mots.

```pascal
type
  PTrieNode = ^TTrieNode;
  TTrieNode = record
    Children: array['a'..'z'] of PTrieNode;
    IsEndOfWord: Boolean;
  end;

  TTrie = class
  private
    FRoot: PTrieNode;
    function CreateNode: PTrieNode;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Insert(const Word: string);
    function Search(const Word: string): Boolean;
    function StartsWith(const Prefix: string): Boolean;
  end;

constructor TTrie.Create;  
begin
  inherited;
  FRoot := CreateNode;
end;

function TTrie.CreateNode: PTrieNode;  
var
  C: Char;
begin
  New(Result);
  for C := 'a' to 'z' do
    Result^.Children[C] := nil;
  Result^.IsEndOfWord := False;
end;

procedure TTrie.Insert(const Word: string);  
var
  Node: PTrieNode;
  i: Integer;
  C: Char;
begin
  Node := FRoot;
  for i := 1 to Length(Word) do
  begin
    C := LowerCase(Word[i])[1];
    if not (C in ['a'..'z']) then Continue;

    if Node^.Children[C] = nil then
      Node^.Children[C] := CreateNode;

    Node := Node^.Children[C];
  end;
  Node^.IsEndOfWord := True;
end;

function TTrie.Search(const Word: string): Boolean;  
var
  Node: PTrieNode;
  i: Integer;
  C: Char;
begin
  Node := FRoot;
  for i := 1 to Length(Word) do
  begin
    C := LowerCase(Word[i])[1];
    if not (C in ['a'..'z']) then Exit(False);

    Node := Node^.Children[C];
    if Node = nil then Exit(False);
  end;
  Result := Node^.IsEndOfWord;
end;

function TTrie.StartsWith(const Prefix: string): Boolean;  
var
  Node: PTrieNode;
  i: Integer;
  C: Char;
begin
  Node := FRoot;
  for i := 1 to Length(Prefix) do
  begin
    C := LowerCase(Prefix[i])[1];
    if not (C in ['a'..'z']) then Exit(False);

    Node := Node^.Children[C];
    if Node = nil then Exit(False);
  end;
  Result := True;
end;

// Utilisation : Dictionnaire avec autocomplétion
var
  Dictionary: TTrie;
begin
  Dictionary := TTrie.Create;
  try
    Dictionary.Insert('hello');
    Dictionary.Insert('help');
    Dictionary.Insert('hero');
    Dictionary.Insert('world');

    WriteLn('Search "hello": ', Dictionary.Search('hello'));     // True
    WriteLn('Search "hell": ', Dictionary.Search('hell'));       // False
    WriteLn('Starts with "hel": ', Dictionary.StartsWith('hel')); // True
  finally
    Dictionary.Free;
  end;
end;
```

**Complexité** :
- Insert : O(m) où m = longueur du mot
- Search : O(m)
- Très efficace pour préfixes

## Anti-patterns à éviter

### 1. Mauvaise structure pour le problème

```pascal
// ❌ Utiliser TStringList pour recherche fréquente
var
  Users: TStringList;
  i: Integer;
begin
  Users := TStringList.Create;
  try
    // Ajouter 100,000 utilisateurs
    for i := 1 to 100000 do
      Users.Add('User' + IntToStr(i));

    // Recherche O(n) à chaque fois !
    if Users.IndexOf('User50000') >= 0 then
      WriteLn('Trouvé');
  finally
    Users.Free;
  end;
end;
// Temps de recherche : ~50 ms par recherche

// ✅ Utiliser THashSet pour recherche O(1)
var
  Users: THashSet<string>;
  i: Integer;
begin
  Users := THashSet<string>.Create;
  try
    for i := 1 to 100000 do
      Users.Add('User' + IntToStr(i));

    if Users.Contains('User50000') then
      WriteLn('Trouvé');
  finally
    Users.Free;
  end;
end;
// Temps de recherche : < 0.001 ms (50,000x plus rapide!)
```

### 2. Pas de pré-allocation

```pascal
// ❌ Réallocation constante
var
  List: TList;
  i: Integer;
begin
  List := TList.Create;
  try
    for i := 1 to 1000000 do
      List.Add(Pointer(i));  // Réallocation multiple
  finally
    List.Free;
  end;
end;
// Temps : ~150 ms

// ✅ Pré-allocation
var
  List: TList;
  i: Integer;
begin
  List := TList.Create;
  try
    List.Capacity := 1000000;  // Une seule allocation
    for i := 1 to 1000000 do
      List.Add(Pointer(i));
  finally
    List.Free;
  end;
end;
// Temps : ~50 ms (3x plus rapide)
```

### 3. Copie excessive

```pascal
// ❌ Copie de grandes structures
type
  TBigData = record
    Data: array[0..9999] of Double;
  end;

function ProcessData(D: TBigData): Double;  // Copie 80 KB !  
var i: Integer;  
begin
  Result := 0;
  for i := 0 to 9999 do
    Result := Result + D.Data[i];
end;

// ✅ Passage par référence
function ProcessDataFast(const D: TBigData): Double;  // Pas de copie  
var i: Integer;  
begin
  Result := 0;
  for i := 0 to 9999 do
    Result := Result + D.Data[i];
end;
// Gain : 100x plus rapide pour grandes structures
```

### 4. Allocation en boucle

```pascal
// ❌ Créer/détruire en boucle
for i := 1 to 10000 do  
begin
  List := TList.Create;
  try
    // Utiliser List
  finally
    List.Free;
  end;
end;
// 10,000 allocations/libérations

// ✅ Réutiliser
List := TList.Create;  
try
  for i := 1 to 10000 do
  begin
    List.Clear;
    // Utiliser List
  end;
finally
  List.Free;
end;
// 1 allocation/libération
```

### 5. Structures imbriquées inefficaces

```pascal
// ❌ Recherche O(n²)
type
  TUserGroup = record
    GroupName: string;
    Users: TStringList;
  end;

var
  Groups: TList;  // Liste de TUserGroup

function IsUserInGroup(const UserName, GroupName: string): Boolean;  
var
  i, j: Integer;
  Group: TUserGroup;
begin
  Result := False;
  for i := 0 to Groups.Count - 1 do  // O(n)
  begin
    Group := TUserGroup(Groups[i]^);
    if Group.GroupName = GroupName then
    begin
      for j := 0 to Group.Users.Count - 1 do  // O(m)
        if Group.Users[j] = UserName then
          Exit(True);
    end;
  end;
end;
// Complexité : O(n × m)

// ✅ Index avec TDictionary
var
  GroupIndex: TDictionary<string, THashSet<string>>;

function IsUserInGroupFast(const UserName, GroupName: string): Boolean;  
var
  Users: THashSet<string>;
begin
  Result := GroupIndex.TryGetValue(GroupName, Users) and
            Users.Contains(UserName);
end;
// Complexité : O(1)
```

## Outils de débogage

### Visualiser les structures

```pascal
// Afficher le contenu d'une TDictionary
procedure PrintDictionary(Dict: TDictionary<string, Integer>);  
var
  Pair: TPair<string, Integer>;
begin
  WriteLn('=== Dictionary Contents ===');
  for Pair in Dict do
    WriteLn(Format('  %s: %d', [Pair.Key, Pair.Value]));
  WriteLn('Total: ', Dict.Count, ' items');
end;

// Afficher les statistiques d'une THashSet
procedure PrintHashSetStats(HashSet: THashSet<Integer>);  
begin
  WriteLn('=== HashSet Statistics ===');
  WriteLn('  Count: ', HashSet.Count);
  WriteLn('  Capacity: ', HashSet.Capacity);
  WriteLn('  Load Factor: ', (HashSet.Count / HashSet.Capacity):0:2);
end;
```

### Détecter les fuites

```pascal
// Utiliser HeapTrc pour détecter les fuites
{$IFDEF DEBUG}
{$DEFINE HEAPTRC}
{$ENDIF}

program TestLeaks;

{$IFDEF HEAPTRC}
uses
  HeapTrc;
{$ENDIF}

var
  List: TList;
begin
  {$IFDEF HEAPTRC}
  SetHeapTraceOutput('heaptrace.log');
  {$ENDIF}

  List := TList.Create;
  List.Add(Pointer(123));
  // Oubli de List.Free; → détecté dans heaptrace.log
end.
```

## Ressources et documentation

### Documentation FreePascal

- **FCL (Free Component Library)** : https://www.freepascal.org/docs-html/fcl/
- **RTL (Run-Time Library)** : https://www.freepascal.org/docs-html/rtl/
- **Generics.Collections** : Documentation dans le code source

### Livres et références

- "Introduction to Algorithms" (Cormen et al.) - CLRS
- "Data Structures and Algorithm Analysis" (Mark Allen Weiss)
- "The Algorithm Design Manual" (Steven Skiena)

### Bibliothèques tierces

**Collections avancées** :
- **Fundamentals** : Bibliothèque complète de structures
- **DeCAL** : Delphi Container and Algorithm Library (compatible FPC)
- **Generics.Collections** : Inclus dans FPC 3.2+

**Structures spécialisées** :
- **BGRABitmap** : Structures graphiques optimisées
- **DCPCrypt** : Structures crypto
- **Synopse mORMot** : Collections haute performance

## Exercices de réflexion

Pour chaque scénario, quelle structure choisiriez-vous et pourquoi ?

### Scénario 1 : Gestionnaire de cache web
- Besoin : Stocker les 1000 pages web les plus récemment consultées
- Opérations : Ajout, recherche par URL, éviction des plus anciennes
- **Réponse** : LRU Cache (combinaison TDictionary + TList)

### Scénario 2 : Autocomplétion de recherche
- Besoin : Suggérer des mots commençant par le préfixe tapé
- Opérations : Recherche de préfixes, insertion de nouveaux mots
- **Réponse** : Trie (arbre de préfixes)

### Scénario 3 : Système de tags pour articles
- Besoin : Associer plusieurs tags à chaque article, rechercher par tag
- Opérations : Ajouter tags, trouver articles avec un tag donné
- **Réponse** : TDictionary<string, THashSet<Integer>> (tag → article IDs)

### Scénario 4 : File de priorité pour tâches
- Besoin : Traiter les tâches par ordre de priorité
- Opérations : Ajouter tâche, obtenir tâche la plus prioritaire
- **Réponse** : Priority Queue (Binary Heap)

### Scénario 5 : Carte de jeu 2D
- Besoin : Trouver rapidement les objets dans une région de la carte
- Opérations : Insertion d'objets, requête par zone rectangulaire
- **Réponse** : QuadTree

## Conclusion

### Hiérarchie des optimisations

1. **Algorithme et structure de données** (gain : 10x-1000x) ⭐⭐⭐⭐⭐
2. **Pré-allocation et pooling** (gain : 2x-50x) ⭐⭐⭐⭐
3. **Localité de cache** (gain : 2x-10x) ⭐⭐⭐
4. **Optimisations compilateur** (gain : 1.1x-2x) ⭐⭐
5. **Micro-optimisations** (gain : 1.05x-1.2x) ⭐

### Règles d'or

1. **Choisir la bonne structure AVANT d'optimiser le code**
2. **Complexité O() est plus importante que les constantes**
3. **Profiler pour identifier les vrais problèmes**
4. **Pré-allouer quand possible**
5. **Records > Classes pour données simples**
6. **Hash tables pour recherches fréquentes**
7. **Arrays pour accès séquentiels**
8. **Ne pas sur-optimiser** (maintenir la lisibilité)

### Tableau récapitulatif final

| Besoin | Structure | Complexité typique | Quand éviter |
|--------|-----------|-------------------|--------------|
| **Accès index** | Array, TList | O(1) | Insertions fréquentes au début |
| **Recherche clé** | TDictionary | O(1) | Ordre important |
| **Ensemble unique** | THashSet | O(1) | Besoin d'ordre |
| **Ordre maintenu** | AVL Tree | O(log n) | Accès aléatoire fréquent |
| **FIFO** | TQueue | O(1) | Accès par index |
| **LIFO** | TStack | O(1) | Accès par index |
| **Priorités** | Priority Queue | O(log n) | Toutes même priorité |
| **Cache LRU** | LRU Cache | O(1) | Taille illimitée |
| **Spatial 2D** | QuadTree | O(log n) | Données 1D |
| **Préfixes** | Trie | O(m) | Mots très longs |
| **Graphe** | Adjacency List | O(V+E) | Graphe très dense |

### Le mot de la fin

Le choix de la structure de données est **la décision d'optimisation la plus importante**. Un mauvais choix rendra toutes les autres optimisations inefficaces, tandis qu'un bon choix peut rendre les optimisations inutiles.

**Règle de Donald Knuth** : "Premature optimization is the root of all evil" - mais choisir la bonne structure n'est PAS de l'optimisation prématurée, c'est du **bon design**.

Mesurez, comparez, et choisissez en connaissance de cause !

---

*Note : Ce tutoriel fait partie de la formation "FreePascal/Lazarus - Niveau Développeur Avancé - Edition Multi-plateforme Windows/Ubuntu"*

⏭️ [Algorithmes haute performance](/20-optimisation-performance/06-algorithmes-haute-performance.md)
