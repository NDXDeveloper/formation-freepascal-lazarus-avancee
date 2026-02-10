🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 20.6 Algorithmes haute performance

## Introduction

Un algorithme est une séquence d'instructions pour résoudre un problème. Le choix de l'algorithme est **crucial** pour les performances : un bon algorithme peut être des milliers de fois plus rapide qu'un mauvais, indépendamment du langage ou du matériel utilisé.

Ce chapitre explore les algorithmes classiques optimisés et comment les implémenter efficacement en FreePascal/Lazarus pour les rendre encore plus performants.

**Principe fondamental** : Un algorithme O(n log n) sur un ordinateur lent bat toujours un algorithme O(n²) sur un ordinateur rapide, dès que n devient suffisamment grand.

## Complexité algorithmique : rappels essentiels

### Notation Big-O

La notation Big-O décrit comment le temps d'exécution évolue avec la taille des données.

**Comparaison visuelle** (pour n = 1,000,000) :
```
O(1)        : 1 opération                    ⚡ Instantané  
O(log n)    : 20 opérations                  ⚡ Très rapide  
O(n)        : 1,000,000 opérations          ✅ Acceptable  
O(n log n)  : 20,000,000 opérations         ✅ Bon  
O(n²)       : 1,000,000,000,000 opérations  ❌ Inacceptable (1 trillion!)  
O(2ⁿ)       : Impossible à calculer         ❌ Catastrophique
```

### Exemples concrets

```pascal
// O(1) - Temps constant
Result := Array[Index];

// O(log n) - Logarithmique
function BinarySearch(A: array of Integer; Value: Integer): Integer;

// O(n) - Linéaire
for i := 0 to High(A) do
  Sum := Sum + A[i];

// O(n log n) - Log-linéaire
QuickSort(A);

// O(n²) - Quadratique
for i := 0 to High(A) do
  for j := 0 to High(A) do
    Matrix[i, j] := A[i] * A[j];

// O(2ⁿ) - Exponentiel (à éviter absolument!)
function Fibonacci(n: Integer): Integer;  
begin
  if n <= 1 then Exit(n);
  Result := Fibonacci(n-1) + Fibonacci(n-2);  // Très inefficace
end;
```

## Algorithmes de tri

### 1. QuickSort (Tri rapide)

**Complexité** : O(n log n) en moyenne, O(n²) au pire  
**Usage** : Tri général, le plus utilisé  

```pascal
procedure QuickSort(var A: array of Integer; Left, Right: Integer);  
var
  I, J, Pivot, Temp: Integer;
begin
  if Left >= Right then Exit;

  I := Left;
  J := Right;
  Pivot := A[(Left + Right) div 2];

  repeat
    while A[I] < Pivot do Inc(I);
    while A[J] > Pivot do Dec(J);

    if I <= J then
    begin
      Temp := A[I];
      A[I] := A[J];
      A[J] := Temp;
      Inc(I);
      Dec(J);
    end;
  until I > J;

  if Left < J then QuickSort(A, Left, J);
  if I < Right then QuickSort(A, I, Right);
end;

// Utilisation
var
  Numbers: array[0..9] of Integer = (5, 2, 8, 1, 9, 3, 7, 4, 6, 0);
begin
  QuickSort(Numbers, 0, High(Numbers));
  // Numbers = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
end;
```

**Optimisation : Choisir un bon pivot**

```pascal
// ❌ Mauvais : toujours le milieu (prévisible)
Pivot := A[(Left + Right) div 2];

// ✅ Bon : médiane de trois
function MedianOfThree(var A: array of Integer; Left, Mid, Right: Integer): Integer;  
begin
  if A[Left] > A[Mid] then Swap(A[Left], A[Mid]);
  if A[Left] > A[Right] then Swap(A[Left], A[Right]);
  if A[Mid] > A[Right] then Swap(A[Mid], A[Right]);
  Result := A[Mid];
end;

// Dans QuickSort :
Pivot := MedianOfThree(A, Left, (Left + Right) div 2, Right);
```

**Optimisation : Insertion sort pour petits tableaux**

```pascal
procedure QuickSortOptimized(var A: array of Integer; Left, Right: Integer);  
const
  InsertionThreshold = 10;  // Seuil empirique
begin
  if Right - Left < InsertionThreshold then
  begin
    InsertionSort(A, Left, Right);  // Plus rapide pour petits tableaux
    Exit;
  end;

  // QuickSort normal pour grands tableaux
  // ...
end;
```

### 2. MergeSort (Tri fusion)

**Complexité** : O(n log n) garanti (pas de pire cas O(n²))  
**Usage** : Quand la stabilité est importante, ou pour garantir O(n log n)  

```pascal
procedure Merge(var A: array of Integer; Left, Mid, Right: Integer);  
var
  Temp: array of Integer;
  I, J, K: Integer;
begin
  SetLength(Temp, Right - Left + 1);
  I := Left;
  J := Mid + 1;
  K := 0;

  // Fusionner les deux moitiés
  while (I <= Mid) and (J <= Right) do
  begin
    if A[I] <= A[J] then
    begin
      Temp[K] := A[I];
      Inc(I);
    end
    else
    begin
      Temp[K] := A[J];
      Inc(J);
    end;
    Inc(K);
  end;

  // Copier les éléments restants
  while I <= Mid do
  begin
    Temp[K] := A[I];
    Inc(I);
    Inc(K);
  end;

  while J <= Right do
  begin
    Temp[K] := A[J];
    Inc(J);
    Inc(K);
  end;

  // Copier dans le tableau original
  for I := 0 to K - 1 do
    A[Left + I] := Temp[I];
end;

procedure MergeSort(var A: array of Integer; Left, Right: Integer);  
var
  Mid: Integer;
begin
  if Left >= Right then Exit;

  Mid := (Left + Right) div 2;
  MergeSort(A, Left, Mid);
  MergeSort(A, Mid + 1, Right);
  Merge(A, Left, Mid, Right);
end;
```

**Avantage** : Stable (conserve l'ordre relatif des éléments égaux)

### 3. HeapSort (Tri par tas)

**Complexité** : O(n log n) garanti  
**Usage** : Tri en place (pas de mémoire supplémentaire)  

```pascal
procedure Heapify(var A: array of Integer; N, I: Integer);  
var
  Largest, Left, Right, Temp: Integer;
begin
  Largest := I;
  Left := 2 * I + 1;
  Right := 2 * I + 2;

  if (Left < N) and (A[Left] > A[Largest]) then
    Largest := Left;

  if (Right < N) and (A[Right] > A[Largest]) then
    Largest := Right;

  if Largest <> I then
  begin
    Temp := A[I];
    A[I] := A[Largest];
    A[Largest] := Temp;
    Heapify(A, N, Largest);
  end;
end;

procedure HeapSort(var A: array of Integer);  
var
  N, I, Temp: Integer;
begin
  N := Length(A);

  // Construire le tas
  for I := N div 2 - 1 downto 0 do
    Heapify(A, N, I);

  // Extraire les éléments du tas
  for I := N - 1 downto 1 do
  begin
    Temp := A[0];
    A[0] := A[I];
    A[I] := Temp;
    Heapify(A, I, 0);
  end;
end;
```

### Comparaison des tris

| Algorithme | Meilleur cas | Moyen | Pire cas | Mémoire | Stable |
|------------|--------------|-------|----------|---------|--------|
| **QuickSort** | O(n log n) | O(n log n) | O(n²) | O(log n) | Non |
| **MergeSort** | O(n log n) | O(n log n) | O(n log n) | O(n) | Oui |
| **HeapSort** | O(n log n) | O(n log n) | O(n log n) | O(1) | Non |
| **InsertionSort** | O(n) | O(n²) | O(n²) | O(1) | Oui |
| **BubbleSort** | O(n) | O(n²) | O(n²) | O(1) | Oui |

**Recommandations** :
- **Usage général** : QuickSort (le plus rapide en pratique)
- **Stabilité requise** : MergeSort
- **Mémoire limitée** : HeapSort
- **Petit tableau (< 50)** : InsertionSort
- **Presque trié** : InsertionSort

### Benchmark de tri

```pascal
program SortBenchmark;

uses SysUtils, DateUtils;

const
  N = 100000;

var
  Data, DataCopy: array of Integer;
  StartTime: TDateTime;
  i: Integer;

procedure InitData;  
var i: Integer;  
begin
  SetLength(Data, N);
  Randomize;
  for i := 0 to N - 1 do
    Data[i] := Random(N * 10);
end;

procedure CopyData;  
begin
  SetLength(DataCopy, Length(Data));
  Move(Data[0], DataCopy[0], Length(Data) * SizeOf(Integer));
end;

procedure BenchSort(const Name: string; SortProc: procedure(var A: array of Integer));  
var Ms: Int64;  
begin
  CopyData;
  StartTime := Now;
  SortProc(DataCopy);
  Ms := MilliSecondsBetween(Now, StartTime);
  WriteLn(Format('%-15s: %6d ms', [Name, Ms]));
end;

begin
  InitData;

  WriteLn('Tri de ', N, ' éléments aléatoires:');
  WriteLn;

  BenchSort('QuickSort', @QuickSort);
  BenchSort('MergeSort', @MergeSort);
  BenchSort('HeapSort', @HeapSort);

  ReadLn;
end.

// Résultats typiques :
// Tri de 100000 éléments aléatoires:
//
// QuickSort      :     45 ms
// MergeSort      :     68 ms
// HeapSort       :     95 ms
```

## Algorithmes de recherche

### 1. Recherche linéaire

**Complexité** : O(n)  
**Usage** : Tableau non trié, ou petit tableau  

```pascal
function LinearSearch(const A: array of Integer; Value: Integer): Integer;  
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(A) do
    if A[i] = Value then
      Exit(i);
end;

// Temps pour 1,000,000 éléments : ~500,000 comparaisons en moyenne
```

### 2. Recherche binaire

**Complexité** : O(log n)  
**Usage** : Tableau **trié**  

```pascal
function BinarySearch(const A: array of Integer; Value: Integer): Integer;  
var
  Left, Right, Mid: Integer;
begin
  Left := 0;
  Right := High(A);

  while Left <= Right do
  begin
    Mid := (Left + Right) div 2;

    if A[Mid] = Value then
      Exit(Mid)
    else if A[Mid] < Value then
      Left := Mid + 1
    else
      Right := Mid - 1;
  end;

  Result := -1;  // Non trouvé
end;

// Temps pour 1,000,000 éléments : ~20 comparaisons maximum !
// Gain : 25,000x plus rapide que recherche linéaire
```

**Optimisation : Recherche interpolation**

Pour données uniformément distribuées :

```pascal
function InterpolationSearch(const A: array of Integer; Value: Integer): Integer;  
var
  Low, High, Pos: Integer;
begin
  Low := 0;
  High := High(A);

  while (Low <= High) and (Value >= A[Low]) and (Value <= A[High]) do
  begin
    if Low = High then
    begin
      if A[Low] = Value then
        Exit(Low)
      else
        Exit(-1);
    end;

    // Interpolation
    Pos := Low + ((Value - A[Low]) * (High - Low)) div (A[High] - A[Low]);

    if A[Pos] = Value then
      Exit(Pos)
    else if A[Pos] < Value then
      Low := Pos + 1
    else
      High := Pos - 1;
  end;

  Result := -1;
end;

// Complexité : O(log log n) pour données uniformes
// Encore plus rapide que recherche binaire dans ce cas
```

### 3. Recherche dans une table de hachage

**Complexité** : O(1) en moyenne  
**Usage** : Le plus rapide pour recherches fréquentes  

```pascal
uses Generics.Collections;

var
  HashTable: TDictionary<Integer, string>;
  Value: string;
begin
  HashTable := TDictionary<Integer, string>.Create;
  try
    // Ajouter 1,000,000 éléments
    for i := 1 to 1000000 do
      HashTable.Add(i, 'Value' + IntToStr(i));

    // Recherche O(1) !
    if HashTable.TryGetValue(500000, Value) then
      WriteLn('Trouvé: ', Value);
  finally
    HashTable.Free;
  end;
end;

// Temps de recherche : < 0.001 ms (instantané)
```

## Algorithmes de chaînes de caractères

### 1. Recherche naïve

```pascal
function NaiveStringSearch(const Text, Pattern: string): Integer;  
var
  i, j: Integer;
  Match: Boolean;
begin
  Result := 0;

  for i := 1 to Length(Text) - Length(Pattern) + 1 do
  begin
    Match := True;
    for j := 1 to Length(Pattern) do
      if Text[i + j - 1] <> Pattern[j] then
      begin
        Match := False;
        Break;
      end;

    if Match then
      Exit(i);
  end;
end;

// Complexité : O(n × m) où n = longueur texte, m = longueur pattern
```

### 2. Boyer-Moore (algorithme optimisé)

```pascal
function BoyerMooreSearch(const Text, Pattern: string): Integer;  
var
  BadChar: array[Char] of Integer;
  i, j, Shift: Integer;
  C: Char;
begin
  // Initialiser la table des mauvais caractères
  for C := Low(Char) to High(Char) do
    BadChar[C] := Length(Pattern);

  for i := 1 to Length(Pattern) - 1 do
    BadChar[Pattern[i]] := Length(Pattern) - i;

  // Recherche
  i := Length(Pattern);
  while i <= Length(Text) do
  begin
    j := Length(Pattern);
    while (j > 0) and (Text[i - Length(Pattern) + j] = Pattern[j]) do
      Dec(j);

    if j = 0 then
      Exit(i - Length(Pattern) + 1);

    Shift := BadChar[Text[i]];
    Inc(i, Shift);
  end;

  Result := 0;
end;

// Complexité : O(n/m) dans le meilleur cas (sous-linéaire!)
// Peut sauter plusieurs caractères à la fois
```

**Comparaison** :
```
Recherche de "NEEDLE" dans un texte de 1,000,000 caractères :

Naïve (Pos)     : ~150 ms  
Boyer-Moore     : ~30 ms (5x plus rapide)
```

### 3. Knuth-Morris-Pratt (KMP)

```pascal
procedure ComputeLPSArray(const Pattern: string; var LPS: array of Integer);  
var
  Len, i: Integer;
begin
  Len := 0;
  LPS[1] := 0;
  i := 2;

  while i <= Length(Pattern) do
  begin
    if Pattern[i] = Pattern[Len + 1] then
    begin
      Inc(Len);
      LPS[i] := Len;
      Inc(i);
    end
    else
    begin
      if Len <> 0 then
        Len := LPS[Len]
      else
      begin
        LPS[i] := 0;
        Inc(i);
      end;
    end;
  end;
end;

function KMPSearch(const Text, Pattern: string): Integer;  
var
  LPS: array of Integer;
  i, j: Integer;
begin
  SetLength(LPS, Length(Pattern) + 1);
  ComputeLPSArray(Pattern, LPS);

  i := 1;
  j := 1;

  while i <= Length(Text) do
  begin
    if Pattern[j] = Text[i] then
    begin
      Inc(i);
      Inc(j);
    end;

    if j > Length(Pattern) then
      Exit(i - j + 1);

    if (i <= Length(Text)) and (Pattern[j] <> Text[i]) then
    begin
      if j <> 1 then
        j := LPS[j - 1] + 1
      else
        Inc(i);
    end;
  end;

  Result := 0;
end;

// Complexité : O(n + m) garanti (linéaire)
// Pas de retour en arrière dans le texte
```

## Algorithmes sur les graphes

### 1. Parcours en largeur (BFS - Breadth-First Search)

```pascal
uses Generics.Collections;

type
  TGraph = TDictionary<Integer, TList<Integer>>;

function BFS(Graph: TGraph; Start, Target: Integer): Boolean;  
var
  Queue: TQueue<Integer>;
  Visited: THashSet<Integer>;
  Current, Neighbor: Integer;
  Neighbors: TList<Integer>;
begin
  Queue := TQueue<Integer>.Create;
  Visited := THashSet<Integer>.Create;
  try
    Queue.Enqueue(Start);
    Visited.Add(Start);

    while Queue.Count > 0 do
    begin
      Current := Queue.Dequeue;

      if Current = Target then
        Exit(True);

      if Graph.TryGetValue(Current, Neighbors) then
        for Neighbor in Neighbors do
          if not Visited.Contains(Neighbor) then
          begin
            Visited.Add(Neighbor);
            Queue.Enqueue(Neighbor);
          end;
    end;

    Result := False;
  finally
    Queue.Free;
    Visited.Free;
  end;
end;

// Complexité : O(V + E) où V = sommets, E = arêtes
// Usage : Plus court chemin (non pondéré), connectivité
```

### 2. Parcours en profondeur (DFS - Depth-First Search)

```pascal
function DFS(Graph: TGraph; Start, Target: Integer; Visited: THashSet<Integer>): Boolean;  
var
  Neighbors: TList<Integer>;
  Neighbor: Integer;
begin
  if Start = Target then
    Exit(True);

  Visited.Add(Start);

  if Graph.TryGetValue(Start, Neighbors) then
    for Neighbor in Neighbors do
      if not Visited.Contains(Neighbor) then
        if DFS(Graph, Neighbor, Target, Visited) then
          Exit(True);

  Result := False;
end;

// Utilisation
var
  Visited: THashSet<Integer>;
begin
  Visited := THashSet<Integer>.Create;
  try
    if DFS(Graph, StartNode, TargetNode, Visited) then
      WriteLn('Chemin trouvé');
  finally
    Visited.Free;
  end;
end;

// Complexité : O(V + E)
// Usage : Détection de cycles, tri topologique
```

### 3. Dijkstra (Plus court chemin pondéré)

```pascal
type
  TEdge = record
    Target: Integer;
    Weight: Integer;
  end;
  TWeightedGraph = TDictionary<Integer, TList<TEdge>>;

function Dijkstra(Graph: TWeightedGraph; Start, Target: Integer): Integer;  
type
  TNode = record
    Vertex: Integer;
    Distance: Integer;
  end;
var
  Distances: TDictionary<Integer, Integer>;
  PQ: TList<TNode>;  // Priority Queue simplifiée
  Current: TNode;
  Edges: TList<TEdge>;
  Edge: TEdge;
  NewDist, i: Integer;
begin
  Distances := TDictionary<Integer, Integer>.Create;
  PQ := TList<TNode>.Create;
  try
    // Initialiser
    Distances.Add(Start, 0);
    Current.Vertex := Start;
    Current.Distance := 0;
    PQ.Add(Current);

    while PQ.Count > 0 do
    begin
      // Extraire le nœud avec distance minimale
      Current := PQ[0];
      PQ.Delete(0);

      if Current.Vertex = Target then
        Exit(Current.Distance);

      if not Graph.TryGetValue(Current.Vertex, Edges) then
        Continue;

      for Edge in Edges do
      begin
        NewDist := Current.Distance + Edge.Weight;

        if not Distances.ContainsKey(Edge.Target) or
           (NewDist < Distances[Edge.Target]) then
        begin
          Distances.AddOrSetValue(Edge.Target, NewDist);
          Current.Vertex := Edge.Target;
          Current.Distance := NewDist;
          PQ.Add(Current);
        end;
      end;
    end;

    Result := -1;  // Pas de chemin
  finally
    Distances.Free;
    PQ.Free;
  end;
end;

// Complexité : O((V + E) log V) avec priority queue
// Usage : GPS, routage réseau, pathfinding jeux
```

## Algorithmes de programmation dynamique

### 1. Fibonacci avec mémoïsation

```pascal
// ❌ Version récursive naïve (O(2ⁿ))
function FibonacciSlow(N: Integer): Int64;  
begin
  if N <= 1 then Exit(N);
  Result := FibonacciSlow(N - 1) + FibonacciSlow(N - 2);
end;
// Fib(40) = ~2 secondes

// ✅ Version avec mémoïsation (O(n))
var
  FibCache: array[0..100] of Int64;

function FibonacciFast(N: Integer): Int64;  
begin
  if N <= 1 then
  begin
    FibCache[N] := N;
    Exit(N);
  end;

  if FibCache[N] <> 0 then
    Exit(FibCache[N]);

  FibCache[N] := FibonacciFast(N - 1) + FibonacciFast(N - 2);
  Result := FibCache[N];
end;
// Fib(40) = < 0.001 secondes (2000x plus rapide!)

// ✅✅ Version itérative (O(n), pas de récursion)
function FibonacciIterative(N: Integer): Int64;  
var
  A, B, i: Int64;
begin
  if N <= 1 then Exit(N);

  A := 0;
  B := 1;
  for i := 2 to N do
  begin
    Result := A + B;
    A := B;
    B := Result;
  end;
end;
// Fib(40) = < 0.0001 secondes
```

### 2. Plus longue sous-séquence commune (LCS)

```pascal
function LCS(const S1, S2: string): Integer;  
var
  DP: array of array of Integer;
  i, j: Integer;
begin
  SetLength(DP, Length(S1) + 1, Length(S2) + 1);

  for i := 1 to Length(S1) do
    for j := 1 to Length(S2) do
      if S1[i] = S2[j] then
        DP[i][j] := DP[i-1][j-1] + 1
      else
        DP[i][j] := Max(DP[i-1][j], DP[i][j-1]);

  Result := DP[Length(S1)][Length(S2)];
end;

// Exemple
WriteLn(LCS('ABCDGH', 'AEDFHR'));  // 3 (ADH)

// Complexité : O(n × m)
// Usage : Diff de fichiers, similitude de textes
```

### 3. Problème du sac à dos (Knapsack)

```pascal
type
  TItem = record
    Weight: Integer;
    Value: Integer;
  end;

function Knapsack(Items: array of TItem; Capacity: Integer): Integer;  
var
  DP: array of Integer;
  i, w: Integer;
begin
  SetLength(DP, Capacity + 1);

  for i := 0 to High(Items) do
    for w := Capacity downto Items[i].Weight do
      DP[w] := Max(DP[w], DP[w - Items[i].Weight] + Items[i].Value);

  Result := DP[Capacity];
end;

// Utilisation
var
  Items: array[0..2] of TItem;
begin
  Items[0].Weight := 10; Items[0].Value := 60;
  Items[1].Weight := 20; Items[1].Value := 100;
  Items[2].Weight := 30; Items[2].Value := 120;

  WriteLn('Valeur maximale: ', Knapsack(Items, 50));  // 220
end;

// Complexité : O(n × W) où W = capacité
// Usage : Optimisation de ressources, allocation
```

## Algorithmes numériques

### 1. Exponentiation rapide

```pascal
// ❌ Naïf : O(n)
function PowerSlow(Base, Exp: Int64): Int64;  
var i: Integer;  
begin
  Result := 1;
  for i := 1 to Exp do
    Result := Result * Base;
end;

// ✅ Exponentiation rapide : O(log n)
function PowerFast(Base, Exp: Int64): Int64;  
begin
  if Exp = 0 then Exit(1);
  if Exp = 1 then Exit(Base);

  if Exp mod 2 = 0 then
    Result := PowerFast(Base * Base, Exp div 2)
  else
    Result := Base * PowerFast(Base * Base, Exp div 2);
end;

// PowerFast(2, 1000) = ~10 multiplications au lieu de 1000!
```

### 2. PGCD (Plus Grand Commun Diviseur) - Euclide

```pascal
function GCD(A, B: Integer): Integer;  
begin
  while B <> 0 do
  begin
    Result := A mod B;
    A := B;
    B := Result;
  end;
  Result := A;
end;

// Complexité : O(log min(A, B))
// Très rapide même pour de grands nombres
```

### 3. Crible d'Ératosthène (Nombres premiers)

```pascal
function SieveOfEratosthenes(N: Integer): TList<Integer>;  
var
  IsPrime: array of Boolean;
  i, j: Integer;
begin
  Result := TList<Integer>.Create;
  SetLength(IsPrime, N + 1);

  // Initialiser tous à True
  for i := 2 to N do
    IsPrime[i] := True;

  // Marquer les non-premiers
  for i := 2 to Trunc(Sqrt(N)) do
    if IsPrime[i] then
      for j := i * i to N step i do
        IsPrime[j] := False;

  // Collecter les premiers
  for i := 2 to N do
    if IsPrime[i] then
      Result.Add(i);
end;

// Trouver tous les premiers jusqu'à 1,000,000
var Primes: TList<Integer>;  
begin
  Primes := SieveOfEratosthenes(1000000);
  WriteLn('Nombres premiers trouvés: ', Primes.Count);  // 78,498
  Primes.Free;
end;

// Complexité : O(n log log n)
// Temps : ~50 ms pour 1,000,000
```

## Optimisations d'algorithmes

### 1. Éviter les calculs redondants

```pascal
// ❌ Calcul répété
for i := 0 to N - 1 do
  for j := 0 to N - 1 do
    Matrix[i, j] := Sqrt(i) + Sqrt(j);  // Sqrt calculé N fois par valeur

// ✅ Précalculer
var
  SqrtCache: array of Double;
begin
  SetLength(SqrtCache, N);
  for i := 0 to N - 1 do
    SqrtCache[i] := Sqrt(i);

  for i := 0 to N - 1 do
    for j := 0 to N - 1 do
      Matrix[i, j] := SqrtCache[i] + SqrtCache[j];
end;

// Gain : N² appels à Sqrt → 2N appels
// Pour N=1000 : 1,000,000 → 2,000 (500x plus rapide!)
```

### 2. Early exit (Sortie anticipée)

```pascal
// ❌ Toujours parcourir tout
function ContainsDuplicate(const A: array of Integer): Boolean;  
var
  i, j: Integer;
  HasDuplicate: Boolean;
begin
  HasDuplicate := False;
  for i := 0 to High(A) do
    for j := i + 1 to High(A) do
      if A[i] = A[j] then
        HasDuplicate := True;  // Continue quand même!
  Result := HasDuplicate;
end;

// ✅ Sortir dès qu'on trouve
function ContainsDuplicateFast(const A: array of Integer): Boolean;  
var
  i, j: Integer;
begin
  for i := 0 to High(A) do
    for j := i + 1 to High(A) do
      if A[i] = A[j] then
        Exit(True);  // Sortie immédiate
  Result := False;
end;

// ✅✅ Encore mieux : HashSet O(n) au lieu de O(n²)
uses Generics.Collections;

function ContainsDuplicateBest(const A: array of Integer): Boolean;  
var
  Seen: THashSet<Integer>;
  i: Integer;
begin
  Seen := THashSet<Integer>.Create;
  try
    for i := 0 to High(A) do
      if not Seen.Add(A[i]) then  // Add retourne False si déjà présent
        Exit(True);
    Result := False;
  finally
    Seen.Free;
  end;
end;
```

### 3. Loop invariant code motion

```pascal
// ❌ Calcul dans la boucle
for i := 0 to N - 1 do
  A[i] := A[i] * (B + C) / (D - E);  // (B+C)/(D-E) constant, calculé N fois

// ✅ Sortir de la boucle
var
  Factor: Double;
begin
  Factor := (B + C) / (D - E);  // Calculé une fois
  for i := 0 to N - 1 do
    A[i] := A[i] * Factor;
end;
```

### 4. Strength reduction

```pascal
// ❌ Multiplications répétées
for i := 0 to N - 1 do
  A[i] := i * 7;  // N multiplications

// ✅ Addition incrémentale
var
  Value: Integer;
begin
  Value := 0;
  for i := 0 to N - 1 do
  begin
    A[i] := Value;
    Value := Value + 7;  // N additions (plus rapide)
  end;
end;
```

### 5. Loop unrolling (Déroulement de boucle)

```pascal
// ❌ Overhead de boucle important
for i := 0 to 999 do
  A[i] := A[i] + 1;

// ✅ Déroulé (moins d'overhead)
i := 0;  
while i <= 996 do  
begin
  A[i] := A[i] + 1;
  A[i+1] := A[i+1] + 1;
  A[i+2] := A[i+2] + 1;
  A[i+3] := A[i+3] + 1;  // 4 itérations par boucle
  Inc(i, 4);
end;
// Traiter les éléments restants
while i <= 999 do  
begin
  A[i] := A[i] + 1;
  Inc(i);
end;

// Gain : 10-20% pour grandes boucles simples
// Note : Le compilateur avec -O3 fait ça automatiquement
```

## Algorithmes parallèles

### 1. Tri parallèle (Merge Sort)

```pascal
uses SysUtils, Classes;

procedure ParallelMergeSort(var A: array of Integer; Left, Right: Integer);  
var
  Mid: Integer;
  Thread1, Thread2: TThread;
begin
  if Right - Left < 10000 then  // Seuil pour parallélisation
  begin
    MergeSort(A, Left, Right);  // Version séquentielle
    Exit;
  end;

  Mid := (Left + Right) div 2;

  // Lancer deux threads pour les deux moitiés
  Thread1 := TThread.CreateAnonymousThread(
    procedure
    begin
      ParallelMergeSort(A, Left, Mid);
    end
  );

  Thread2 := TThread.CreateAnonymousThread(
    procedure
    begin
      ParallelMergeSort(A, Mid + 1, Right);
    end
  );

  Thread1.Start;
  Thread2.Start;
  Thread1.WaitFor;
  Thread2.WaitFor;
  Thread1.Free;
  Thread2.Free;

  Merge(A, Left, Mid, Right);
end;

// Gain : ~2x sur dual-core, ~4x sur quad-core
```

### 2. Map-Reduce pattern

```pascal
uses System.Threading;

type
  TMapReduceTask<TInput, TOutput> = class
  public
    class function Execute(
      const Data: array of TInput;
      MapFunc: TFunc<TInput, TOutput>;
      ReduceFunc: TFunc<TOutput, TOutput, TOutput>;
      InitialValue: TOutput
    ): TOutput;
  end;

class function TMapReduceTask<TInput, TOutput>.Execute(
  const Data: array of TInput;
  MapFunc: TFunc<TInput, TOutput>;
  ReduceFunc: TFunc<TOutput, TOutput, TOutput>;
  InitialValue: TOutput
): TOutput;
var
  MappedResults: array of TOutput;
  i: Integer;
begin
  SetLength(MappedResults, Length(Data));

  // Phase Map (parallèle)
  TParallel.For(0, High(Data),
    procedure(Index: Integer)
    begin
      MappedResults[Index] := MapFunc(Data[Index]);
    end
  );

  // Phase Reduce (séquentiel pour simplifier)
  Result := InitialValue;
  for i := 0 to High(MappedResults) do
    Result := ReduceFunc(Result, MappedResults[i]);
end;

// Utilisation : Somme des carrés
var
  Numbers: array of Integer;
  Sum: Integer;
begin
  SetLength(Numbers, 1000000);
  for i := 0 to High(Numbers) do
    Numbers[i] := i;

  Sum := TMapReduceTask<Integer, Integer>.Execute(
    Numbers,
    function(X: Integer): Integer
    begin
      Result := X * X;  // Map : carré
    end,
    function(A, B: Integer): Integer
    begin
      Result := A + B;  // Reduce : addition
    end,
    0  // Valeur initiale
  );

  WriteLn('Somme des carrés: ', Sum);
end;
```

## Algorithmes d'approximation

### 1. Monte Carlo (Calcul de π)

```pascal
function EstimatePi(Iterations: Integer): Double;  
var
  InsideCircle, i: Integer;
  X, Y: Double;
begin
  Randomize;
  InsideCircle := 0;

  for i := 1 to Iterations do
  begin
    X := Random;
    Y := Random;
    if (X * X + Y * Y) <= 1 then
      Inc(InsideCircle);
  end;

  Result := 4.0 * InsideCircle / Iterations;
end;

// 1,000,000 itérations → π ≈ 3.141592 (précision 0.0001)
// Trade-off : Précision vs temps de calcul
```

### 2. Algorithme glouton (Knapsack approximation)

```pascal
type
  TItem = record
    Weight: Integer;
    Value: Integer;
    Ratio: Double;  // Value/Weight
  end;

function GreedyKnapsack(Items: array of TItem; Capacity: Integer): Integer;  
var
  i, CurrentWeight: Integer;
begin
  // Trier par ratio valeur/poids décroissant
  QuickSortByRatio(Items);

  Result := 0;
  CurrentWeight := 0;

  for i := 0 to High(Items) do
  begin
    if CurrentWeight + Items[i].Weight <= Capacity then
    begin
      Inc(CurrentWeight, Items[i].Weight);
      Inc(Result, Items[i].Value);
    end;
  end;
end;

// Complexité : O(n log n) vs O(n × W) pour solution exacte
// Approximation souvent < 2% de l'optimal
```

## Benchmarking d'algorithmes

### Template de comparaison

```pascal
program AlgorithmBenchmark;

uses SysUtils, DateUtils;

type
  TAlgorithmProc = procedure(var A: array of Integer);

const
  Sizes: array[0..4] of Integer = (100, 1000, 10000, 100000, 1000000);

var
  TestData: array of Integer;

procedure InitData(Size: Integer);  
var i: Integer;  
begin
  SetLength(TestData, Size);
  Randomize;
  for i := 0 to Size - 1 do
    TestData[i] := Random(Size * 10);
end;

procedure BenchmarkAlgorithm(const Name: string; Algo: TAlgorithmProc; Size: Integer);  
var
  StartTime: TDateTime;
  Ms: Int64;
  Data: array of Integer;
begin
  // Copier les données
  SetLength(Data, Size);
  Move(TestData[0], Data[0], Size * SizeOf(Integer));

  // Mesurer
  StartTime := Now;
  Algo(Data);
  Ms := MilliSecondsBetween(Now, StartTime);

  WriteLn(Format('  %-20s N=%-7d: %6d ms', [Name, Size, Ms]));
end;

var
  Size: Integer;

begin
  WriteLn('=== Benchmark de tris ===');
  WriteLn;

  for Size in Sizes do
  begin
    WriteLn('Taille: ', Size);
    InitData(Size);

    BenchmarkAlgorithm('QuickSort', @QuickSort, Size);
    BenchmarkAlgorithm('MergeSort', @MergeSort, Size);
    BenchmarkAlgorithm('HeapSort', @HeapSort, Size);

    WriteLn;
  end;

  ReadLn;
end.

// Exemple de sortie :
// === Benchmark de tris ===
//
// Taille: 100
//   QuickSort            N=100    :      0 ms
//   MergeSort            N=100    :      0 ms
//   HeapSort             N=100    :      0 ms
//
// Taille: 100000
//   QuickSort            N=100000 :     45 ms
//   MergeSort            N=100000 :     68 ms
//   HeapSort             N=100000 :     95 ms
//
// Taille: 1000000
//   QuickSort            N=1000000:    520 ms
//   MergeSort            N=1000000:    780 ms
//   HeapSort             N=1000000:   1150 ms
```

### Validation des résultats

```pascal
function IsSorted(const A: array of Integer): Boolean;  
var i: Integer;  
begin
  for i := 0 to High(A) - 1 do
    if A[i] > A[i + 1] then
      Exit(False);
  Result := True;
end;

function ArraysEqual(const A, B: array of Integer): Boolean;  
var i: Integer;  
begin
  if Length(A) <> Length(B) then Exit(False);
  for i := 0 to High(A) do
    if A[i] <> B[i] then
      Exit(False);
  Result := True;
end;

// Valider qu'un algorithme de tri est correct
procedure ValidateSort(SortProc: TAlgorithmProc);  
var
  Original, Sorted1, Sorted2: array of Integer;
  i: Integer;
begin
  // Créer données de test
  SetLength(Original, 1000);
  for i := 0 to 999 do
    Original[i] := Random(10000);

  // Trier avec algorithme à tester
  SetLength(Sorted1, 1000);
  Move(Original[0], Sorted1[0], 1000 * SizeOf(Integer));
  SortProc(Sorted1);

  // Trier avec algorithme de référence
  SetLength(Sorted2, 1000);
  Move(Original[0], Sorted2[0], 1000 * SizeOf(Integer));
  QuickSort(Sorted2, 0, High(Sorted2));

  // Vérifier
  if not IsSorted(Sorted1) then
    WriteLn('ERREUR: Résultat non trié')
  else if not ArraysEqual(Sorted1, Sorted2) then
    WriteLn('ERREUR: Résultat différent de la référence')
  else
    WriteLn('OK: Algorithme correct');
end;
```

## Cas pratiques

### Cas 1 : Recherche de doublons rapide

```pascal
// Problème : Trouver si un tableau contient des doublons
// Solution naïve : O(n²)
// Solution optimale : O(n) avec HashSet

uses Generics.Collections;

function HasDuplicates(const A: array of Integer): Boolean;  
var
  Seen: THashSet<Integer>;
  i: Integer;
begin
  Seen := THashSet<Integer>.Create;
  try
    for i := 0 to High(A) do
      if not Seen.Add(A[i]) then
        Exit(True);
    Result := False;
  finally
    Seen.Free;
  end;
end;

// Benchmark :
// N=100,000
// Naïf O(n²)  : 15000 ms
// HashSet O(n):    50 ms
// Gain : 300x plus rapide
```

### Cas 2 : Top K éléments

```pascal
// Problème : Trouver les K plus grands éléments
// Solution : Tri partiel avec Quick Select

function QuickSelect(var A: array of Integer; Left, Right, K: Integer): Integer;  
var
  Pivot, i, j, Temp: Integer;
begin
  if Left = Right then Exit(A[Left]);

  Pivot := A[(Left + Right) div 2];
  i := Left;
  j := Right;

  while i <= j do
  begin
    while A[i] < Pivot do Inc(i);
    while A[j] > Pivot do Dec(j);

    if i <= j then
    begin
      Temp := A[i];
      A[i] := A[j];
      A[j] := Temp;
      Inc(i);
      Dec(j);
    end;
  end;

  if K <= j then
    Result := QuickSelect(A, Left, j, K)
  else if K >= i then
    Result := QuickSelect(A, i, Right, K)
  else
    Result := A[K];
end;

function FindTopK(const A: array of Integer; K: Integer): TArray<Integer>;  
var
  ACopy: array of Integer;
  i: Integer;
begin
  SetLength(ACopy, Length(A));
  Move(A[0], ACopy[0], Length(A) * SizeOf(Integer));

  QuickSelect(ACopy, 0, High(ACopy), Length(A) - K);

  SetLength(Result, K);
  for i := 0 to K - 1 do
    Result[i] := ACopy[Length(A) - K + i];
end;

// Complexité : O(n) en moyenne vs O(n log n) pour tri complet
```

### Cas 3 : Détection de cycle dans une liste chaînée

```pascal
type
  PNode = ^TNode;
  TNode = record
    Data: Integer;
    Next: PNode;
  end;

// Algorithme de Floyd (Tortue et Lièvre)
function HasCycle(Head: PNode): Boolean;  
var
  Slow, Fast: PNode;
begin
  if Head = nil then Exit(False);

  Slow := Head;
  Fast := Head;

  while (Fast <> nil) and (Fast^.Next <> nil) do
  begin
    Slow := Slow^.Next;           // Avance de 1
    Fast := Fast^.Next^.Next;     // Avance de 2

    if Slow = Fast then           // Se rejoignent → cycle
      Exit(True);
  end;

  Result := False;
end;

// Complexité : O(n) temps, O(1) espace
// Très élégant et efficace
```

### Cas 4 : Sliding Window Maximum

```pascal
// Problème : Maximum dans chaque fenêtre de taille K

uses Generics.Collections;

function SlidingWindowMaximum(const A: array of Integer; K: Integer): TArray<Integer>;  
var
  Deque: TList<Integer>;  // Index des candidats
  i, Count: Integer;
begin
  Deque := TList<Integer>.Create;
  try
    Count := 0;
    SetLength(Result, Length(A) - K + 1);

    for i := 0 to High(A) do
    begin
      // Supprimer les éléments hors fenêtre
      while (Deque.Count > 0) and (Deque[0] <= i - K) do
        Deque.Delete(0);

      // Supprimer les éléments plus petits (inutiles)
      while (Deque.Count > 0) and (A[Deque[Deque.Count - 1]] <= A[i]) do
        Deque.Delete(Deque.Count - 1);

      Deque.Add(i);

      // Enregistrer le maximum
      if i >= K - 1 then
      begin
        Result[Count] := A[Deque[0]];
        Inc(Count);
      end;
    end;
  finally
    Deque.Free;
  end;
end;

// Exemple : A = [1,3,-1,-3,5,3,6,7], K = 3
// Result = [3,3,5,5,6,7]
// Complexité : O(n) au lieu de O(n × K) naïf
```

## Guide de sélection d'algorithmes

### Tri

| Situation | Algorithme | Raison |
|-----------|------------|--------|
| **Usage général** | QuickSort | Plus rapide en pratique |
| **Données presque triées** | InsertionSort | O(n) dans ce cas |
| **Stabilité requise** | MergeSort | Préserve l'ordre |
| **Mémoire limitée** | HeapSort | Tri en place |
| **Petit tableau (< 50)** | InsertionSort | Moins d'overhead |
| **Grandes données externes** | External Sort | Données sur disque |

### Recherche

| Situation | Algorithme | Complexité |
|-----------|------------|------------|
| **Tableau non trié** | Linear Search | O(n) |
| **Tableau trié** | Binary Search | O(log n) |
| **Recherches fréquentes** | Hash Table | O(1) |
| **Recherche de préfixe** | Trie | O(m) |
| **Recherche floue** | Levenshtein | O(nm) |

### Graphes

| Problème | Algorithme | Complexité |
|----------|------------|------------|
| **Chemin (non pondéré)** | BFS | O(V + E) |
| **Chemin (pondéré)** | Dijkstra | O((V+E) log V) |
| **Tous les chemins** | Floyd-Warshall | O(V³) |
| **Arbre couvrant min** | Kruskal/Prim | O(E log V) |
| **Flot maximum** | Ford-Fulkerson | O(E × maxflow) |

## Erreurs courantes

### 1. Mauvaise complexité choisie

```pascal
// ❌ O(n²) quand O(n) existe
for i := 0 to High(A) do
  for j := 0 to High(A) do
    if A[j] = Target then
      Found := True;

// ✅ O(n) avec une passe
for i := 0 to High(A) do
  if A[i] = Target then
  begin
    Found := True;
    Break;
  end;
```

### 2. Ne pas utiliser les structures appropriées

```pascal
// ❌ TList pour recherche O(n)
if List.IndexOf(Item) >= 0 then ...

// ✅ THashSet pour recherche O(1)
if HashSet.Contains(Item) then ...
```

### 3. Récursion inefficace sans mémoïsation

```pascal
// ❌ Fibonacci récursif O(2ⁿ)
function Fib(n: Integer): Integer;  
begin
  if n <= 1 then Exit(n);
  Result := Fib(n-1) + Fib(n-2);
end;

// ✅ Avec cache O(n)
var Cache: array[0..100] of Integer;  
function FibMemo(n: Integer): Integer;  
begin
  if n <= 1 then Exit(n);
  if Cache[n] <> 0 then Exit(Cache[n]);
  Cache[n] := FibMemo(n-1) + FibMemo(n-2);
  Result := Cache[n];
end;
```

## Ressources et références

### Livres essentiels

- **"Introduction to Algorithms"** (CLRS) - La référence absolue
- **"The Algorithm Design Manual"** (Skiena) - Pratique et appliqué
- **"Programming Pearls"** (Bentley) - Techniques d'optimisation
- **"Algorithms"** (Sedgewick & Wayne) - Implémentations détaillées

### Sites web

- **LeetCode** : https://leetcode.com - Pratique d'algorithmes
- **HackerRank** : https://www.hackerrank.com - Challenges
- **Visualgo** : https://visualgo.net - Visualisation d'algorithmes
- **Big-O Cheat Sheet** : https://www.bigocheatsheet.com

### Bibliothèques FreePascal

- **FCL (Free Component Library)** - Collections et algorithmes de base
- **Generics.Collections** - Structures de données génériques
- **LazUtils** - Utilitaires Lazarus

## Checklist d'optimisation

### Avant d'implémenter

- [ ] Quelle est la complexité théorique minimale ?
- [ ] Existe-t-il un algorithme standard pour ce problème ?
- [ ] Quelle est la taille typique des données ?
- [ ] Les données ont-elles des propriétés particulières (triées, etc.) ?

### Pendant l'implémentation

- [ ] Éviter les calculs redondants
- [ ] Utiliser les bonnes structures de données
- [ ] Sortir tôt des boucles si possible
- [ ] Éviter les allocations en boucle

### Après l'implémentation

- [ ] Valider la correction (tests unitaires)
- [ ] Benchmarker sur données réalistes
- [ ] Comparer avec implémentations de référence
- [ ] Documenter la complexité

## Conclusion

### Hiérarchie d'importance

1. **Complexité algorithmique** (O notation) - Impact : 10x-1000x ⭐⭐⭐⭐⭐
2. **Structures de données** - Impact : 10x-100x ⭐⭐⭐⭐⭐
3. **Optimisations algorithmiques** - Impact : 2x-10x ⭐⭐⭐⭐
4. **Parallélisation** - Impact : 2x-8x ⭐⭐⭐
5. **Optimisations de code** - Impact : 1.1x-2x ⭐⭐

### Règles d'or

✅ **Choisir le bon algorithme AVANT d'optimiser le code**

✅ **O(n log n) > O(n²)** même avec du code non optimisé

✅ **Mesurer avant d'optimiser** (profiling)

✅ **Trade-off temps/espace** : Parfois, utiliser plus de mémoire pour gagner en vitesse

✅ **Simplicité d'abord** : Code simple et correct, puis optimiser si nécessaire

✅ **Connaître les classiques** : Beaucoup de problèmes ont des solutions bien connues

### Gains typiques

| Optimisation | Gain |
|--------------|------|
| O(n²) → O(n log n) | 100x-1000x |
| O(n) → O(log n) | 100x-10000x |
| O(n) → O(1) (hash) | 1000x-1000000x |
| Parallélisation | 2x-8x (selon CPU) |
| Cache/Mémoïsation | 10x-10000x |
| Meilleur algo | Variable, souvent majeur |

### Le mot de la fin

**L'algorithme est plus important que le langage ou le matériel.** Un mauvais algorithme sur un super-ordinateur sera battu par un bon algorithme sur un Raspberry Pi dès que les données deviennent suffisamment grandes.

Investissez du temps pour apprendre les algorithmes classiques : tri, recherche, graphes, programmation dynamique. Ces connaissances sont le meilleur investissement pour écrire du code performant.

**Rappelez-vous** : "Premature optimization is the root of all evil, but that's no excuse for sloppy thinking" - Donald Knuth

---

*Note : Ce tutoriel fait partie de la formation "FreePascal/Lazarus - Niveau Développeur Avancé - Edition Multi-plateforme Windows/Ubuntu"*

⏭️ [Memory pools et allocateurs custom](/20-optimisation-performance/07-memory-pools-allocateurs-custom.md)
