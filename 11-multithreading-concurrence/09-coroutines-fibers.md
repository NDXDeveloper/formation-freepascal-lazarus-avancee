🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 11.9 Coroutines et fibers

## Introduction aux coroutines

Les **coroutines** sont des fonctions qui peuvent suspendre leur exécution et la reprendre plus tard, conservant leur état entre les appels. C'est une forme de multitâche **coopératif** où les tâches cèdent volontairement le contrôle.

### Qu'est-ce qu'une coroutine ?

Une **coroutine** est une fonction qui peut :
1. S'arrêter en plein milieu (yield)
2. Reprendre là où elle s'était arrêtée
3. Conserver son état (variables locales) entre les appels
4. Retourner plusieurs valeurs successivement

**Analogie** : Imaginez lire un livre. Vous pouvez :
- Mettre un marque-page (yield) et fermer le livre
- Reprendre plus tard exactement où vous étiez
- Vous souvenez du contexte (qui sont les personnages, l'intrigue)
- Lire chapitre par chapitre (yield après chaque chapitre)

### Coroutines vs Threads

```
Thread                          Coroutine
━━━━━━━━━━━━━━━━━━━━━━━━       ━━━━━━━━━━━━━━━━━━━━━━━━
Préemptif                       Coopératif  
Ordonnancé par l'OS             Ordonnancé par le programme  
Overhead élevé                  Overhead minimal  
Parallèle (multi-cœur)          Concurrent (mono-cœur)  
Context switch coûteux          Context switch léger  
Risque de race conditions       Pas de race conditions
```

**Exemple simple :**

```pascal
// Thread : exécution parallèle
Thread1: A1 → A2 → A3 → A4  
Thread2: B1 → B2 → B3 → B4
(En même temps sur différents cœurs)

// Coroutine : exécution intercalée
Main: A1 → [yield] → B1 → [yield] → A2 → [yield] → B2 → ...
(Sur un seul cœur, mais avec changements rapides)
```

## Concepts fondamentaux

### Yield (Céder le contrôle)

**Yield** suspend l'exécution de la coroutine et rend le contrôle à l'appelant.

```pascal
procedure MonGenerateur;  
begin
  WriteLn('Début');
  Yield;  // Suspend ici, retourne à l'appelant
  WriteLn('Milieu');
  Yield;  // Suspend à nouveau
  WriteLn('Fin');
end;
```

### Resume (Reprendre l'exécution)

**Resume** reprend l'exécution d'une coroutine suspendue.

```pascal
var
  Coro: TCoroutine;
begin
  Coro := CreateCoroutine(@MonGenerateur);

  Coro.Resume; // Affiche "Début", puis suspend
  Coro.Resume; // Affiche "Milieu", puis suspend
  Coro.Resume; // Affiche "Fin", puis termine
end;
```

### État d'une coroutine

Une coroutine peut être dans différents états :
- **Created** : Créée mais pas encore démarrée
- **Running** : En cours d'exécution
- **Suspended** : Suspendue (après un yield)
- **Dead** : Terminée

## Fibers (Fibres)

Les **fibers** sont l'implémentation bas niveau des coroutines. Ce sont des threads légers gérés en espace utilisateur plutôt que par l'OS.

### Différence Fiber vs Thread

```
Thread                          Fiber
━━━━━━━━━━━━━━━━━━━━━━━━       ━━━━━━━━━━━━━━━━━━━━━━━━
Géré par l'OS                   Géré par l'application  
Stack : ~1 MB                   Stack : ~64 KB (configurable)  
Context switch : ~1-2 µs        Context switch : ~100 ns  
Création coûteuse               Création légère  
Scheduling automatique          Scheduling manuel
```

### Support des fibers en FreePascal

FreePascal ne dispose pas de support natif des fibers, mais nous pouvons les implémenter ou utiliser des bibliothèques.

## Implémentation basique de coroutines

### Structure de base

```pascal
unit BasicCoroutines;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TCoroutineState = (csCreated, csRunning, csSuspended, csDead);
  TCoroutineProc = procedure;

  TCoroutine = class
  private
    FState: TCoroutineState;
    FProc: TCoroutineProc;
    FYieldPoint: Integer;
  public
    constructor Create(Proc: TCoroutineProc);

    procedure Resume;
    function IsDead: Boolean;

    property State: TCoroutineState read FState;
  end;

var
  CurrentCoroutine: TCoroutine = nil;

procedure Yield;

implementation

constructor TCoroutine.Create(Proc: TCoroutineProc);  
begin
  inherited Create;
  FProc := Proc;
  FState := csCreated;
  FYieldPoint := 0;
end;

procedure TCoroutine.Resume;  
var
  PreviousCoroutine: TCoroutine;
begin
  if FState = csDead then
    raise Exception.Create('Cannot resume dead coroutine');

  PreviousCoroutine := CurrentCoroutine;
  CurrentCoroutine := Self;

  try
    FState := csRunning;

    // Exécuter la procédure
    // Note : Cette implémentation simplifiée ne gère pas vraiment
    // la suspension/reprise. Pour cela, il faudrait sauvegarder
    // la pile d'exécution (stack).
    FProc;

    FState := csDead;
  finally
    CurrentCoroutine := PreviousCoroutine;
  end;
end;

function TCoroutine.IsDead: Boolean;  
begin
  Result := FState = csDead;
end;

procedure Yield;  
begin
  if Assigned(CurrentCoroutine) then
    CurrentCoroutine.FState := csSuspended;
  // Ici, nous devrions sauvegarder le contexte et retourner
  // Pour une vraie implémentation, voir les bibliothèques spécialisées
end;

end.
```

### Générateurs avec coroutines

Un **générateur** est une coroutine qui produit une séquence de valeurs.

> **Note :** Les unités utilisant des types génériques (`TGenerator<T>`, `TList<T>`, etc.) sont compilées en `{$mode delphi}` pour simplifier la syntaxe des génériques. En `{$mode objfpc}`, il faudrait préfixer chaque utilisation avec le mot-clé `specialize`.

```pascal
unit Generators;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils;

type
  TIntArray = array of Integer; // Type utilisé par TPermutationGenerator

  TGenerator<T> = class
  protected
    FCurrentValue: T;  // protected : accessible par les classes dérivées
  private
    FFinished: Boolean;
    FIndex: Integer;
  protected
    function GetNext: Boolean; virtual; abstract;
  public
    constructor Create;

    function MoveNext: Boolean;
    function Current: T;

    property Finished: Boolean read FFinished;
  end;

  // Générateur de nombres
  TRangeGenerator = class(TGenerator<Integer>)
  private
    FStart, FStop, FStep: Integer;
  protected
    function GetNext: Boolean; override;
  public
    constructor Create(AStart, AStop: Integer; AStep: Integer = 1);
  end;

implementation

{ TGenerator<T> }

constructor TGenerator<T>.Create;  
begin
  inherited Create;
  FFinished := False;
  FIndex := 0;
end;

function TGenerator<T>.MoveNext: Boolean;  
begin
  Result := GetNext;
  if Result then
    Inc(FIndex)
  else
    FFinished := True;
end;

function TGenerator<T>.Current: T;  
begin
  Result := FCurrentValue;
end;

{ TRangeGenerator }

constructor TRangeGenerator.Create(AStart, AStop, AStep: Integer);  
begin
  inherited Create;
  FStart := AStart;
  FStop := AStop;
  FStep := AStep;
  FCurrentValue := AStart - FStep; // Commence avant le premier
end;

function TRangeGenerator.GetNext: Boolean;  
begin
  FCurrentValue := FCurrentValue + FStep;

  if FStep > 0 then
    Result := FCurrentValue <= FStop
  else
    Result := FCurrentValue >= FStop;
end;

end.
```

### Utilisation des générateurs

```pascal
uses
  Generators;

procedure TestRangeGenerator;  
var
  Gen: TRangeGenerator;
begin
  Gen := TRangeGenerator.Create(1, 10, 2);
  try
    WriteLn('Nombres impairs de 1 à 10 :');

    while Gen.MoveNext do
      WriteLn(Gen.Current);
  finally
    Gen.Free;
  end;

  // Affiche : 1, 3, 5, 7, 9
end;
```

## Générateurs avancés

### Générateur de Fibonacci

```pascal
type
  TFibonacciGenerator = class(TGenerator<Int64>)
  private
    FA, FB: Int64;
  protected
    function GetNext: Boolean; override;
  public
    constructor Create;
  end;

constructor TFibonacciGenerator.Create;  
begin
  inherited Create;
  FA := 0;
  FB := 1;
end;

function TFibonacciGenerator.GetNext: Boolean;  
var
  Temp: Int64;
begin
  FCurrentValue := FA;
  Temp := FA;
  FA := FB;
  FB := Temp + FB;

  Result := True; // Infini (limiter avec un compteur si nécessaire)
end;

// Utilisation
var
  Fib: TFibonacciGenerator;
  i: Integer;
begin
  Fib := TFibonacciGenerator.Create;
  try
    WriteLn('Les 15 premiers nombres de Fibonacci :');

    for i := 1 to 15 do
    begin
      Fib.MoveNext;
      WriteLn(Fib.Current);
    end;
  finally
    Fib.Free;
  end;
end;
```

### Générateur de nombres premiers

```pascal
type
  TPrimeGenerator = class(TGenerator<Integer>)
  private
    function IsPrime(N: Integer): Boolean;
  protected
    function GetNext: Boolean; override;
  public
    constructor Create;
  end;

constructor TPrimeGenerator.Create;  
begin
  inherited Create;
  FCurrentValue := 1; // Commence avant 2 (premier nombre premier)
end;

function TPrimeGenerator.IsPrime(N: Integer): Boolean;  
var
  i: Integer;
begin
  if N < 2 then
    Exit(False);
  if N = 2 then
    Exit(True);
  if N mod 2 = 0 then
    Exit(False);

  i := 3;
  while i * i <= N do
  begin
    if N mod i = 0 then
      Exit(False);
    Inc(i, 2);
  end;

  Result := True;
end;

function TPrimeGenerator.GetNext: Boolean;  
begin
  repeat
    Inc(FCurrentValue);
  until IsPrime(FCurrentValue);

  Result := True;
end;

// Utilisation
var
  Primes: TPrimeGenerator;
  i: Integer;
begin
  Primes := TPrimeGenerator.Create;
  try
    WriteLn('Les 20 premiers nombres premiers :');

    for i := 1 to 20 do
    begin
      Primes.MoveNext;
      Write(Primes.Current, ' ');
    end;
    WriteLn;
  finally
    Primes.Free;
  end;
end;
```

### Générateur de permutations

```pascal
type
  TPermutationGenerator = class(TGenerator<TIntArray>)
  private
    FElements: TIntArray;
    FIndices: TIntArray;
    FFirst: Boolean;

    procedure Swap(var A, B: Integer);
    function NextPermutation: Boolean;
  protected
    function GetNext: Boolean; override;
  public
    constructor Create(const Elements: array of Integer);
  end;

constructor TPermutationGenerator.Create(const Elements: array of Integer);  
var
  i: Integer;
begin
  inherited Create;

  SetLength(FElements, Length(Elements));
  SetLength(FIndices, Length(Elements));

  for i := 0 to High(Elements) do
  begin
    FElements[i] := Elements[i];
    FIndices[i] := i;
  end;

  FFirst := True;
end;

procedure TPermutationGenerator.Swap(var A, B: Integer);  
var
  Temp: Integer;
begin
  Temp := A;
  A := B;
  B := Temp;
end;

function TPermutationGenerator.NextPermutation: Boolean;  
var
  i, j, k: Integer;
begin
  // Algorithme de génération de permutations
  // (implémentation simplifiée)

  // Trouver le plus grand i tel que array[i] < array[i+1]
  i := High(FIndices) - 1;
  while (i >= 0) and (FIndices[i] >= FIndices[i + 1]) do
    Dec(i);

  if i < 0 then
    Exit(False); // Plus de permutations

  // Trouver le plus grand j tel que array[i] < array[j]
  j := High(FIndices);
  while FIndices[i] >= FIndices[j] do
    Dec(j);

  // Échanger
  Swap(FIndices[i], FIndices[j]);

  // Inverser le suffixe
  k := i + 1;
  j := High(FIndices);
  while k < j do
  begin
    Swap(FIndices[k], FIndices[j]);
    Inc(k);
    Dec(j);
  end;

  Result := True;
end;

function TPermutationGenerator.GetNext: Boolean;  
var
  i: Integer;
begin
  if FFirst then
  begin
    FFirst := False;
    Result := True;
  end
  else
    Result := NextPermutation;

  if Result then
  begin
    SetLength(FCurrentValue, Length(FElements));
    for i := 0 to High(FIndices) do
      FCurrentValue[i] := FElements[FIndices[i]];
  end;
end;

// Utilisation
var
  Perms: TPermutationGenerator;
  Perm: TIntArray;
  i: Integer;
begin
  Perms := TPermutationGenerator.Create([1, 2, 3]);
  try
    WriteLn('Toutes les permutations de [1, 2, 3] :');

    while Perms.MoveNext do
    begin
      Perm := Perms.Current;
      Write('[');
      for i := 0 to High(Perm) do
      begin
        Write(Perm[i]);
        if i < High(Perm) then
          Write(', ');
      end;
      WriteLn(']');
    end;
  finally
    Perms.Free;
  end;
end;
```

## Itérateurs

Les **itérateurs** sont une forme de générateurs qui parcourent des collections.

```pascal
unit CustomIterators;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections;

type
  // TPredicate<T> n'existe pas en FPC (type Delphi System.SysUtils)
  TPredicate<T> = reference to function(const Value: T): Boolean;

  TIterator<T> = class
  protected
    function GetCurrent: T; virtual; abstract;
    function MoveNext: Boolean; virtual; abstract;
  public
    property Current: T read GetCurrent;
  end;

  // Itérateur pour liste
  TListIterator<T> = class(TIterator<T>)
  private
    FList: TList<T>;
    FIndex: Integer;
  protected
    function GetCurrent: T; override;
    function MoveNext: Boolean; override;
  public
    constructor Create(AList: TList<T>);
  end;

  // Itérateur filtré
  TFilterIterator<T> = class(TIterator<T>)
  private
    FSource: TIterator<T>;
    FPredicate: TPredicate<T>;
    FCurrent: T;
    FHasCurrent: Boolean;
  protected
    function GetCurrent: T; override;
    function MoveNext: Boolean; override;
  public
    constructor Create(Source: TIterator<T>; Predicate: TPredicate<T>);
  end;

implementation

{ TListIterator<T> }

constructor TListIterator<T>.Create(AList: TList<T>);  
begin
  inherited Create;
  FList := AList;
  FIndex := -1;
end;

function TListIterator<T>.GetCurrent: T;  
begin
  Result := FList[FIndex];
end;

function TListIterator<T>.MoveNext: Boolean;  
begin
  Inc(FIndex);
  Result := FIndex < FList.Count;
end;

{ TFilterIterator<T> }

constructor TFilterIterator<T>.Create(Source: TIterator<T>; Predicate: TPredicate<T>);  
begin
  inherited Create;
  FSource := Source;
  FPredicate := Predicate;
  FHasCurrent := False;
end;

function TFilterIterator<T>.GetCurrent: T;  
begin
  if not FHasCurrent then
    raise Exception.Create('No current value');
  Result := FCurrent;
end;

function TFilterIterator<T>.MoveNext: Boolean;  
begin
  while FSource.MoveNext do
  begin
    if FPredicate(FSource.Current) then
    begin
      FCurrent := FSource.Current;
      FHasCurrent := True;
      Exit(True);
    end;
  end;

  FHasCurrent := False;
  Result := False;
end;

end.
```

### Utilisation des itérateurs

```pascal
uses
  CustomIterators, Generics.Collections;

var
  Numbers: TList<Integer>;
  Iterator: TListIterator<Integer>;
  FilteredIterator: TFilterIterator<Integer>;

function IsEven(N: Integer): Boolean;  
begin
  Result := N mod 2 = 0;
end;

begin
  Numbers := TList<Integer>.Create;
  try
    // Ajouter des nombres
    Numbers.AddRange([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]);

    // Itérateur simple
    WriteLn('Tous les nombres :');
    Iterator := TListIterator<Integer>.Create(Numbers);
    try
      while Iterator.MoveNext do
        Write(Iterator.Current, ' ');
      WriteLn;
    finally
      Iterator.Free;
    end;

    // Itérateur filtré (nombres pairs)
    WriteLn('Nombres pairs :');
    Iterator := TListIterator<Integer>.Create(Numbers);
    FilteredIterator := TFilterIterator<Integer>.Create(Iterator, @IsEven);
    try
      while FilteredIterator.MoveNext do
        Write(FilteredIterator.Current, ' ');
      WriteLn;
    finally
      FilteredIterator.Free;
      Iterator.Free;
    end;
  finally
    Numbers.Free;
  end;
end;
```

## Coroutines pour l'asynchrone

Les coroutines peuvent simplifier le code asynchrone.

```pascal
unit AsyncCoroutines;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TAsyncOperation = class
  private
    FCompleted: Boolean;
    FResult: Variant;
  public
    procedure Execute; virtual; abstract;
    function IsCompleted: Boolean;
    function GetResult: Variant;
  end;

  TAsyncCoroutine = class
  private
    FOperations: array of TAsyncOperation;
    FCurrentOp: Integer;
  public
    procedure AddOperation(Op: TAsyncOperation);
    function Step: Boolean; // Exécute une étape
    function Run: Variant;  // Exécute jusqu'à la fin
  end;

implementation

{ TAsyncOperation }

function TAsyncOperation.IsCompleted: Boolean;  
begin
  Result := FCompleted;
end;

function TAsyncOperation.GetResult: Variant;  
begin
  if not FCompleted then
    raise Exception.Create('Operation not completed');
  Result := FResult;
end;

{ TAsyncCoroutine }

procedure TAsyncCoroutine.AddOperation(Op: TAsyncOperation);  
begin
  SetLength(FOperations, Length(FOperations) + 1);
  FOperations[High(FOperations)] := Op;
end;

function TAsyncCoroutine.Step: Boolean;  
begin
  if FCurrentOp >= Length(FOperations) then
    Exit(False);

  if not FOperations[FCurrentOp].IsCompleted then
  begin
    FOperations[FCurrentOp].Execute;
    Exit(True);
  end;

  Inc(FCurrentOp);
  Result := FCurrentOp < Length(FOperations);
end;

function TAsyncCoroutine.Run: Variant;  
begin
  FCurrentOp := 0;

  while Step do
    Sleep(10); // Yield

  if FCurrentOp > 0 then
    Result := FOperations[FCurrentOp - 1].GetResult
  else
    Result := Null;
end;

end.
```

## Cas d'usage pratiques

### 1. Parsing incrémental

Parser un fichier ligne par ligne sans tout charger en mémoire.

```pascal
type
  TLineReader = class(TGenerator<string>)
  private
    FFile: TextFile;
    FFileName: string;
    FOpened: Boolean;
  protected
    function GetNext: Boolean; override;
  public
    constructor Create(const FileName: string);
    destructor Destroy; override;
  end;

constructor TLineReader.Create(const FileName: string);  
begin
  inherited Create;
  FFileName := FileName;
  FOpened := False;
end;

destructor TLineReader.Destroy;  
begin
  if FOpened then
    CloseFile(FFile);
  inherited;
end;

function TLineReader.GetNext: Boolean;  
begin
  if not FOpened then
  begin
    AssignFile(FFile, FFileName);
    Reset(FFile);
    FOpened := True;
  end;

  if not Eof(FFile) then
  begin
    ReadLn(FFile, FCurrentValue);
    Result := True;
  end
  else
  begin
    CloseFile(FFile);
    FOpened := False;
    Result := False;
  end;
end;

// Utilisation
var
  Reader: TLineReader;
  LineCount: Integer;
begin
  Reader := TLineReader.Create('largefile.txt');
  try
    LineCount := 0;

    while Reader.MoveNext do
    begin
      Inc(LineCount);
      // Traiter la ligne : Reader.Current
      if Pos('ERROR', Reader.Current) > 0 then
        WriteLn('Ligne ', LineCount, ': ', Reader.Current);
    end;
  finally
    Reader.Free;
  end;
end;
```

### 2. Pipeline de traitement

Chaîner plusieurs générateurs pour transformer des données.

```pascal
type
  // TFunc<TIn, TOut> n'existe pas en FPC (type Delphi)
  TFunc<TIn, TOut> = reference to function(const Value: TIn): TOut;

  TMapGenerator<TIn, TOut> = class(TGenerator<TOut>)
  private
    FSource: TGenerator<TIn>;
    FMapFunc: TFunc<TIn, TOut>;
  protected
    function GetNext: Boolean; override;
  public
    constructor Create(Source: TGenerator<TIn>; MapFunc: TFunc<TIn, TOut>);
  end;

function TMapGenerator<TIn, TOut>.GetNext: Boolean;  
begin
  Result := FSource.MoveNext;
  if Result then
    FCurrentValue := FMapFunc(FSource.Current);
end;

// Pipeline : nombres → carrés → filtrer pairs
var
  Numbers: TRangeGenerator;
  Squares: TMapGenerator<Integer, Integer>;
  EvenSquares: TFilterIterator<Integer>;

function Square(N: Integer): Integer;  
begin
  Result := N * N;
end;

function IsEven(N: Integer): Boolean;  
begin
  Result := N mod 2 = 0;
end;

begin
  Numbers := TRangeGenerator.Create(1, 10);
  Squares := TMapGenerator<Integer, Integer>.Create(Numbers, @Square);
  EvenSquares := TFilterIterator<Integer>.Create(Squares, @IsEven);

  try
    WriteLn('Carrés pairs de 1 à 10 :');
    while EvenSquares.MoveNext do
      Write(EvenSquares.Current, ' ');
    WriteLn;
    // Affiche : 4 16 36 64 100
  finally
    EvenSquares.Free;
    Squares.Free;
    Numbers.Free;
  end;
end;
```

### 3. State machine avec coroutines

Implémenter une machine à états avec des coroutines.

```pascal
type
  TStateMachine = class
  private
    FState: Integer;
    FRunning: Boolean;
  public
    procedure Start;
    function Step: Boolean;
    procedure Stop;
  end;

procedure TStateMachine.Start;  
begin
  FState := 0;
  FRunning := True;
end;

function TStateMachine.Step: Boolean;  
begin
  if not FRunning then
    Exit(False);

  case FState of
    0:
    begin
      WriteLn('État: Initialisation');
      FState := 1;
    end;

    1:
    begin
      WriteLn('État: Chargement');
      FState := 2;
    end;

    2:
    begin
      WriteLn('État: Traitement');
      FState := 3;
    end;

    3:
    begin
      WriteLn('État: Finalisation');
      FRunning := False;
    end;
  end;

  Result := FRunning;
end;

procedure TStateMachine.Stop;  
begin
  FRunning := False;
end;

// Utilisation
var
  SM: TStateMachine;
begin
  SM := TStateMachine.Create;
  try
    SM.Start;

    while SM.Step do
      Sleep(100); // Pause entre les états
  finally
    SM.Free;
  end;
end;
```

## Différences Windows/Ubuntu

Les coroutines et générateurs fonctionnent de manière identique sur les deux plateformes car ils n'utilisent pas de fonctionnalités système spécifiques.

### Considérations de performance

```pascal
{$IFDEF WINDOWS}
// Windows : Stack par défaut 1 MB
// Bon pour les générateurs récursifs profonds
{$ENDIF}

{$IFDEF LINUX}
// Linux : Stack configurable (ulimit -s)
// Par défaut : 8 MB
// Excellent pour les algorithmes récursifs
{$ENDIF}
```

### Optimisations

```pascal
unit PlatformOptimization;

{$mode objfpc}{$H+}

interface

procedure OptimizeForPlatform;

implementation

procedure OptimizeForPlatform;  
begin
  {$IFDEF WINDOWS}
  WriteLn('Optimisation Windows : utilisation de fibers natifs possible');
  // Windows a ConvertThreadToFiber, CreateFiber, etc.
  {$ENDIF}

  {$IFDEF LINUX}
  WriteLn('Optimisation Linux : utilisation de ucontext possible');
  // Linux a getcontext, setcontext, swapcontext
  {$ENDIF}
end;

end.
```

## Avantages et limitations

### Avantages des coroutines

✅ **Simplicité**
```pascal
// Sans coroutine : callbacks imbriqués
GetDataAsync(function(data)  
begin
  ProcessAsync(data, function(result)
  begin
    SaveAsync(result, function(success)
    begin
      WriteLn('Done');
    end);
  end);
end);

// Avec coroutine : code linéaire
data := Await(GetDataAsync());  
result := Await(ProcessAsync(data));  
success := Await(SaveAsync(result));  
WriteLn('Done');
```

✅ **Efficacité mémoire**
- Pas de stack par coroutine (contrairement aux threads)
- Faible overhead

✅ **Pas de race conditions**
- Exécution coopérative
- Pas de préemption

### Limitations

❌ **Mono-cœur**
- N'utilise pas le parallélisme
- Pas adapté aux calculs CPU-intensifs

❌ **Blocking bloque tout**
```pascal
// Si une coroutine bloque, toutes bloquent
procedure BadCoroutine;  
begin
  Sleep(1000); // Bloque TOUTES les coroutines !
  Yield;
end;
```

❌ **Support limité**
- FreePascal n'a pas de support natif async/await
- Nécessite des bibliothèques ou implémentations manuelles

## Bonnes pratiques

### 1. Ne pas bloquer dans une coroutine

```pascal
// ❌ MAUVAIS
procedure BadCoroutine;  
begin
  Sleep(1000); // Bloque tout
  Yield;
end;

// ✅ BON
procedure GoodCoroutine;  
var
  StartTime: TDateTime;
begin
  StartTime := Now;
  while MilliSecondsBetween(Now, StartTime) < 1000 do
  begin
    Yield; // Cède régulièrement
    Sleep(10);
  end;
end;
```

### 2. Libérer les ressources

```pascal
// ✅ BON
type
  TResourceGenerator = class(TGenerator<TData>)
  private
    FFile: TFileStream;
  public
    constructor Create(const FileName: string);
    destructor Destroy; override;
  end;

destructor TResourceGenerator.Destroy;  
begin
  FFile.Free; // TOUJOURS libérer
  inherited;
end;
```

### 3. Limiter la profondeur

```pascal
// ❌ MAUVAIS - Récursion infinie
function InfiniteGenerator: TGenerator<Integer>;  
begin
  while True do
  begin
    Result := GetNext();
    Yield;
  end;
end;

// ✅ BON - Avec limite
type
  TLimitedGenerator = class(TGenerator<Integer>)
  private
    FMaxCount: Integer;
    FCount: Integer;
  protected
    function GetNext: Boolean; override;
  public
    constructor Create(MaxCount: Integer);
  end;

constructor TLimitedGenerator.Create(MaxCount: Integer);  
begin
  inherited Create;
  FMaxCount := MaxCount;
  FCount := 0;
end;

function TLimitedGenerator.GetNext: Boolean;  
begin
  Inc(FCount);
  Result := FCount <= FMaxCount;

  if Result then
    FCurrentValue := FCount;
end;
```

### 4. Documenter les yield points

```pascal
type
  TDocumentedGenerator = class(TGenerator<Integer>)
  protected
    function GetNext: Boolean; override;
  end;

function TDocumentedGenerator.GetNext: Boolean;  
begin
  // Yield point 1 : Après initialisation
  FCurrentValue := InitializeData();

  // Yield point 2 : Après chargement
  LoadData();
  FCurrentValue := ProcessData();

  // Yield point 3 : Fin du traitement
  Result := HasMoreData();
end;
```

### 5. Gérer les exceptions

```pascal
type
  TSafeGenerator = class(TGenerator<Integer>)
  protected
    function GetNext: Boolean; override;
  end;

function TSafeGenerator.GetNext: Boolean;  
begin
  try
    // Traitement qui peut échouer
    FCurrentValue := ComputeNext();
    Result := True;
  except
    on E: Exception do
    begin
      WriteLn('Erreur dans le générateur : ', E.Message);
      Result := False; // Arrêter le générateur
    end;
  end;
end;

// Utilisation avec gestion d'erreurs
var
  Gen: TSafeGenerator;
begin
  Gen := TSafeGenerator.Create;
  try
    while Gen.MoveNext do
    begin
      try
        ProcessValue(Gen.Current);
      except
        on E: Exception do
          WriteLn('Erreur de traitement : ', E.Message);
      end;
    end;
  finally
    Gen.Free;
  end;
end;
```

## Bibliothèques et frameworks

### LibCo (Portable Coroutine Library)

**LibCo** est une bibliothèque C légère pour les coroutines, utilisable depuis FreePascal via FFI.

```pascal
unit LibCoBindings;

{$mode objfpc}{$H+}
{$linklib co}

interface

uses
  ctypes;

type
  cothread_t = Pointer;

// Fonctions LibCo
function co_active: cothread_t; cdecl; external;  
function co_create(size: cuint; entry: Pointer): cothread_t; cdecl; external;  
procedure co_delete(thread: cothread_t); cdecl; external;  
procedure co_switch(thread: cothread_t); cdecl; external;

implementation

end.
```

### Utilisation de LibCo

```pascal
uses
  LibCoBindings;

var
  MainThread: cothread_t;
  WorkerThread: cothread_t;

procedure WorkerProc; cdecl;  
begin
  WriteLn('Worker: Début');
  co_switch(MainThread); // Yield vers le thread principal

  WriteLn('Worker: Milieu');
  co_switch(MainThread);

  WriteLn('Worker: Fin');
end;

begin
  MainThread := co_active();
  WorkerThread := co_create(65536, @WorkerProc);

  WriteLn('Main: Démarrage');
  co_switch(WorkerThread); // Passer au worker

  WriteLn('Main: Entre les yields');
  co_switch(WorkerThread);

  WriteLn('Main: Après worker');
  co_switch(WorkerThread);

  co_delete(WorkerThread);
end.
```

### Alternative : ucontext (Linux)

Sous Linux, on peut utiliser **ucontext** pour implémenter des coroutines.

```pascal
{$IFDEF LINUX}
unit UContextCoroutines;

{$mode objfpc}{$H+}

interface

uses
  BaseUnix, Unix, ctypes;

type
  ucontext_t = record
    // Structure complexe, simplifiée ici
    uc_link: Pointer;
    uc_stack: record
      ss_sp: Pointer;
      ss_flags: cint;
      ss_size: size_t;
    end;
    // ... autres champs
  end;

// Fonctions ucontext
function getcontext(var ucp: ucontext_t): cint; cdecl; external 'c';  
function setcontext(const ucp: ucontext_t): cint; cdecl; external 'c';  
procedure makecontext(var ucp: ucontext_t; func: Pointer;
  argc: cint); cdecl; varargs; external 'c';
function swapcontext(var oucp: ucontext_t;
  const ucp: ucontext_t): cint; cdecl; external 'c';

implementation

end.
{$ENDIF}
```

## Patterns avancés avec coroutines

### 1. Producer-Consumer avec générateurs

```pascal
unit ProducerConsumerGen;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, Generators;

type
  TProducer<T> = class(TGenerator<T>)
  protected
    function Produce: T; virtual; abstract;
    function GetNext: Boolean; override;
  end;

  TConsumer<T> = class
  public
    procedure Consume(const Item: T); virtual; abstract;
  end;

  TPipeline<T> = class
  private
    FProducer: TProducer<T>;
    FConsumers: TList<TConsumer<T>>;
  public
    constructor Create(Producer: TProducer<T>);
    destructor Destroy; override;

    procedure AddConsumer(Consumer: TConsumer<T>);
    procedure Run;
  end;

implementation

{ TProducer<T> }

function TProducer<T>.GetNext: Boolean;  
begin
  try
    FCurrentValue := Produce;
    Result := True;
  except
    Result := False;
  end;
end;

{ TPipeline<T> }

constructor TPipeline<T>.Create(Producer: TProducer<T>);  
begin
  inherited Create;
  FProducer := Producer;
  FConsumers := TList<TConsumer<T>>.Create;
end;

destructor TPipeline<T>.Destroy;  
begin
  FConsumers.Free;
  inherited;
end;

procedure TPipeline<T>.AddConsumer(Consumer: TConsumer<T>);  
begin
  FConsumers.Add(Consumer);
end;

procedure TPipeline<T>.Run;  
var
  Consumer: TConsumer<T>;
begin
  while FProducer.MoveNext do
  begin
    for Consumer in FConsumers do
      Consumer.Consume(FProducer.Current);
  end;
end;

end.
```

### Exemple d'utilisation

```pascal
type
  TNumberProducer = class(TProducer<Integer>)
  private
    FCount: Integer;
    FMax: Integer;
  protected
    function Produce: Integer; override;
  public
    constructor Create(Max: Integer);
  end;

  TPrinterConsumer = class(TConsumer<Integer>)
  public
    procedure Consume(const Item: Integer); override;
  end;

  TSquareConsumer = class(TConsumer<Integer>)
  public
    procedure Consume(const Item: Integer); override;
  end;

constructor TNumberProducer.Create(Max: Integer);  
begin
  inherited Create;
  FCount := 0;
  FMax := Max;
end;

function TNumberProducer.Produce: Integer;  
begin
  Inc(FCount);
  if FCount > FMax then
    raise Exception.Create('Done');
  Result := FCount;
end;

procedure TPrinterConsumer.Consume(const Item: Integer);  
begin
  WriteLn('Nombre : ', Item);
end;

procedure TSquareConsumer.Consume(const Item: Integer);  
begin
  WriteLn('Carré : ', Item * Item);
end;

// Utilisation
var
  Pipeline: TPipeline<Integer>;
  Producer: TNumberProducer;
  Printer: TPrinterConsumer;
  Square: TSquareConsumer;
begin
  Producer := TNumberProducer.Create(5);
  Printer := TPrinterConsumer.Create;
  Square := TSquareConsumer.Create;

  Pipeline := TPipeline<Integer>.Create(Producer);
  try
    Pipeline.AddConsumer(Printer);
    Pipeline.AddConsumer(Square);

    Pipeline.Run;
  finally
    Square.Free;
    Printer.Free;
    Producer.Free;
    Pipeline.Free;
  end;
end;
```

### 2. Lazy evaluation avec générateurs

Calculer les valeurs seulement quand nécessaire.

```pascal
type
  TLazyList<T> = class
  private
    FGenerator: TGenerator<T>;
    FCache: TList<T>;
    FAllGenerated: Boolean;
  public
    constructor Create(Generator: TGenerator<T>);
    destructor Destroy; override;

    function Get(Index: Integer): T;
    function Take(Count: Integer): TArray<T>;
    function ToArray: TArray<T>;
  end;

constructor TLazyList<T>.Create(Generator: TGenerator<T>);  
begin
  inherited Create;
  FGenerator := Generator;
  FCache := TList<T>.Create;
  FAllGenerated := False;
end;

destructor TLazyList<T>.Destroy;  
begin
  FCache.Free;
  inherited;
end;

function TLazyList<T>.Get(Index: Integer): T;  
begin
  // Générer jusqu'à l'index demandé
  while (FCache.Count <= Index) and not FAllGenerated do
  begin
    if FGenerator.MoveNext then
      FCache.Add(FGenerator.Current)
    else
      FAllGenerated := True;
  end;

  if Index < FCache.Count then
    Result := FCache[Index]
  else
    raise Exception.Create('Index out of range');
end;

function TLazyList<T>.Take(Count: Integer): TArray<T>;  
var
  i: Integer;
begin
  SetLength(Result, Count);
  for i := 0 to Count - 1 do
    Result[i] := Get(i);
end;

function TLazyList<T>.ToArray: TArray<T>;  
begin
  // Générer tout
  while FGenerator.MoveNext do
    FCache.Add(FGenerator.Current);

  Result := FCache.ToArray;
end;

// Utilisation
var
  Fib: TFibonacciGenerator;
  LazyFib: TLazyList<Int64>;
  First10: TArray<Int64>;
begin
  Fib := TFibonacciGenerator.Create;
  LazyFib := TLazyList<Int64>.Create(Fib);
  try
    // Ne génère que les 10 premiers
    First10 := LazyFib.Take(10);

    // Accès direct à l'élément 100 (génère de 11 à 100)
    WriteLn('Fib(100) = ', LazyFib.Get(100));
  finally
    LazyFib.Free;
    Fib.Free;
  end;
end;
```

### 3. Memoization avec générateurs

Mettre en cache les résultats coûteux.

```pascal
type
  TMemoizedGenerator<T> = class(TGenerator<T>)
  private
    FSource: TGenerator<T>;
    FCache: TList<T>;
    FIndex: Integer;
  protected
    function GetNext: Boolean; override;
  public
    constructor Create(Source: TGenerator<T>);
    destructor Destroy; override;

    procedure Reset;
  end;

constructor TMemoizedGenerator<T>.Create(Source: TGenerator<T>);  
begin
  inherited Create;
  FSource := Source;
  FCache := TList<T>.Create;
  FIndex := -1;
end;

destructor TMemoizedGenerator<T>.Destroy;  
begin
  FCache.Free;
  inherited;
end;

function TMemoizedGenerator<T>.GetNext: Boolean;  
begin
  Inc(FIndex);

  // Si déjà en cache
  if FIndex < FCache.Count then
  begin
    FCurrentValue := FCache[FIndex];
    Exit(True);
  end;

  // Sinon, générer et mettre en cache
  Result := FSource.MoveNext;
  if Result then
  begin
    FCurrentValue := FSource.Current;
    FCache.Add(FCurrentValue);
  end;
end;

procedure TMemoizedGenerator<T>.Reset;  
begin
  FIndex := -1;
end;

// Utilisation
var
  Primes: TPrimeGenerator;
  Memoized: TMemoizedGenerator<Integer>;
  i: Integer;
begin
  Primes := TPrimeGenerator.Create;
  Memoized := TMemoizedGenerator<Integer>.Create(Primes);
  try
    // Première itération : génère et met en cache
    WriteLn('Première itération :');
    for i := 1 to 10 do
    begin
      Memoized.MoveNext;
      WriteLn(Memoized.Current);
    end;

    // Deuxième itération : utilise le cache (très rapide)
    WriteLn('Deuxième itération (depuis le cache) :');
    Memoized.Reset;
    for i := 1 to 10 do
    begin
      Memoized.MoveNext;
      WriteLn(Memoized.Current);
    end;
  finally
    Memoized.Free;
    Primes.Free;
  end;
end;
```

## Comparaison avec d'autres approches

### Coroutines vs Threads vs Async/Await

```pascal
// 1. THREADS : Parallélisme vrai
procedure WithThreads;  
var
  Thread1, Thread2: TThread;
begin
  Thread1 := TThread.CreateAnonymousThread(@Task1);
  Thread2 := TThread.CreateAnonymousThread(@Task2);

  Thread1.Start;
  Thread2.Start;

  Thread1.WaitFor;
  Thread2.WaitFor;

  // Avantages : Vrai parallélisme multi-cœur
  // Inconvénients : Overhead, race conditions, complexité
end;

// 2. COROUTINES : Concurrence coopérative
procedure WithCoroutines;  
var
  Coro1, Coro2: TCoroutine;
begin
  Coro1 := TCoroutine.Create(@Task1);
  Coro2 := TCoroutine.Create(@Task2);

  while not (Coro1.IsDead and Coro2.IsDead) do
  begin
    if not Coro1.IsDead then Coro1.Resume;
    if not Coro2.IsDead then Coro2.Resume;
  end;

  // Avantages : Léger, pas de race conditions
  // Inconvénients : Mono-cœur, coopératif (un blocage bloque tout)
end;

// 3. ASYNC/AWAIT : Asynchrone simplifié
procedure WithAsyncAwait;  
begin
  Task1Async()
    .&Then(procedure
    begin
      Task2Async();
    end);

  // Avantages : Code linéaire, gestion automatique
  // Inconvénients : Nécessite support langage
end;
```

### Tableau comparatif

| Aspect | Threads | Coroutines | Async/Await |
|--------|---------|------------|-------------|
| **Parallélisme** | Oui (multi-cœur) | Non (mono-cœur) | Non (mono-cœur) |
| **Overhead** | Élevé (~1 MB/thread) | Faible (~64 KB) | Faible |
| **Context switch** | ~1-2 µs | ~100 ns | ~100 ns |
| **Complexité** | Élevée (verrous) | Moyenne | Faible |
| **Race conditions** | Oui | Non | Non |
| **Support FPC** | Natif | Manuel | Manuel |

## Performance et benchmarks

### Mesurer les performances

```pascal
unit CoroutineBenchmark;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils, Generators;

type
  TBenchmarkResult = record
    Iterations: Integer;
    TotalTime: Double;
    AvgTimePerIteration: Double;
    IterationsPerSecond: Double;
  end;

// Note : TGenerator étant générique, cette fonction est typiquement
// spécialisée pour un type concret, par exemple TGenerator<Integer>
function BenchmarkGenerator(Gen: TRangeGenerator; Iterations: Integer): TBenchmarkResult;

implementation

function BenchmarkGenerator(Gen: TRangeGenerator; Iterations: Integer): TBenchmarkResult;  
var
  StartTime, EndTime: TDateTime;
  i: Integer;
begin
  StartTime := Now;

  for i := 1 to Iterations do
  begin
    if not Gen.MoveNext then
      Break;
  end;

  EndTime := Now;

  Result.Iterations := i - 1;
  Result.TotalTime := MilliSecondsBetween(EndTime, StartTime);
  Result.AvgTimePerIteration := Result.TotalTime / Result.Iterations;
  Result.IterationsPerSecond := Result.Iterations / (Result.TotalTime / 1000);

  WriteLn('=== Résultats du benchmark ===');
  WriteLn('Itérations : ', Result.Iterations);
  WriteLn('Temps total : ', Result.TotalTime:0:2, ' ms');
  WriteLn('Temps/itération : ', Result.AvgTimePerIteration:0:6, ' ms');
  WriteLn('Itérations/sec : ', Result.IterationsPerSecond:0:0);
end;

end.
```

### Comparaison de performances

```pascal
procedure CompareApproaches;  
var
  StartTime: TDateTime;
  i, Sum: Integer;
  Gen: TRangeGenerator;
begin
  WriteLn('=== Comparaison Thread vs Générateur ===');

  // Approche 1 : Boucle simple (baseline)
  StartTime := Now;
  Sum := 0;
  for i := 1 to 1000000 do
    Sum := Sum + i;
  WriteLn('Boucle simple : ', MilliSecondsBetween(Now, StartTime), ' ms');

  // Approche 2 : Générateur
  Gen := TRangeGenerator.Create(1, 1000000);
  try
    StartTime := Now;
    Sum := 0;
    while Gen.MoveNext do
      Sum := Sum + Gen.Current;
    WriteLn('Générateur : ', MilliSecondsBetween(Now, StartTime), ' ms');
  finally
    Gen.Free;
  end;

  // Résultats typiques :
  // Boucle simple : 5-10 ms
  // Générateur : 50-100 ms (10x plus lent mais plus flexible)
end;
```

## Cas d'usage recommandés

### ✅ Utilisez les coroutines/générateurs pour :

1. **Parsing et traitement de flux**
```pascal
// Traiter un gros fichier ligne par ligne
var
  Reader: TLineReader;
begin
  Reader := TLineReader.Create('huge_log.txt');
  try
    while Reader.MoveNext do
      ProcessLine(Reader.Current);
  finally
    Reader.Free;
  end;
end;
```

2. **Séquences infinies ou très longues**
```pascal
// Nombres premiers infinis
var
  Primes: TPrimeGenerator;
  i: Integer;
begin
  Primes := TPrimeGenerator.Create;
  try
    for i := 1 to 100 do
    begin
      Primes.MoveNext;
      if Primes.Current > 1000 then
        Break;
    end;
  finally
    Primes.Free;
  end;
end;
```

3. **Pipelines de transformation**
```pascal
// Chaîner plusieurs transformations
Numbers
  .Map(@Square)
  .Filter(@IsEven)
  .Take(10)
  .ForEach(@Print);
```

4. **State machines complexes**
```pascal
// Machine à états avec yield entre chaque état
FSM.Start;  
while FSM.Step do
  Sleep(100); // Pause entre états
```

### ❌ N'utilisez PAS les coroutines pour :

1. **Calculs CPU-intensifs parallèles**
```pascal
// Utilisez plutôt des threads ou MTProcs
```

2. **Opérations simples**
```pascal
// Overhead inutile pour une simple boucle
```

3. **Code temps réel strict**
```pascal
// Les coroutines peuvent avoir une latence variable
```

## Ressources et bibliothèques

### Bibliothèques recommandées

1. **LibCo** : Coroutines portables en C
   - Site : https://byuu.net/library/libco
   - Très légère et rapide
   - Facile à interfacer avec FreePascal

2. **Boost.Context** : Contextes C++ (coroutines bas niveau)
   - Partie de Boost
   - Haute performance
   - Nécessite des bindings

3. **picoro** : Micro-coroutines ANSI C
   - Extrêmement légère
   - Facile à porter

### Documentation

- **FreePascal Wiki** : https://wiki.freepascal.org
- **Coroutines in C** : Articles de référence
- **Generators in Python** : Concepts similaires

## Résumé

Les **coroutines et fibers** offrent une approche élégante pour la concurrence coopérative :

**Concepts clés :**
- **Coroutines** : Fonctions qui peuvent suspendre et reprendre
- **Yield** : Suspendre l'exécution
- **Générateurs** : Produire des séquences de valeurs
- **Fibers** : Implémentation bas niveau des coroutines

**Avantages :**
- ✅ Overhead minimal (vs threads)
- ✅ Pas de race conditions
- ✅ Code simple et lisible
- ✅ Lazy evaluation naturelle
- ✅ Parfait pour les flux de données

**Limitations :**
- ❌ Mono-cœur (pas de parallélisme)
- ❌ Support limité en FreePascal (nécessite implémentation)
- ❌ Un blocage bloque tout
- ❌ Pas adapté aux calculs intensifs

**Cas d'usage idéaux :**
- Parsing de fichiers volumineux
- Pipelines de transformation
- Séquences infinies
- State machines
- Itération lazy

Les coroutines sont un outil puissant pour simplifier le code asynchrone et gérer efficacement les flux de données, particulièrement quand le parallélisme n'est pas nécessaire !

⏭️ [Optimisation multicœur](/11-multithreading-concurrence/10-optimisation-multicoeur.md)
