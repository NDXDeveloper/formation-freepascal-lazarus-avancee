🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 20.8 Lazy Evaluation et Memoization

## Introduction

La **lazy evaluation** (évaluation paresseuse) et la **memoization** (mémoïsation) sont deux techniques d'optimisation puissantes qui peuvent améliorer considérablement les performances de vos applications FreePascal/Lazarus.

### Qu'est-ce que la Lazy Evaluation ?

La lazy evaluation consiste à **retarder le calcul d'une valeur jusqu'au moment où elle est réellement nécessaire**. Au lieu de calculer immédiatement toutes les valeurs, on attend que le programme en ait vraiment besoin.

**Analogie simple :** Imaginez que vous préparez un buffet. Au lieu de cuisiner tous les plats dès le matin (évaluation stricte), vous préparez chaque plat uniquement quand quelqu'un le demande (évaluation paresseuse). Cela évite de gaspiller du temps et des ressources pour des plats que personne ne mangera.

### Qu'est-ce que la Memoization ?

La memoization consiste à **mémoriser les résultats de calculs coûteux pour éviter de les recalculer** si on a besoin du même résultat plus tard.

**Analogie simple :** C'est comme garder un carnet où vous notez les réponses aux questions difficiles. Si quelqu'un vous pose la même question plus tard, vous consultez simplement votre carnet au lieu de refaire tout le calcul.

---

## 1. Lazy Evaluation en FreePascal

FreePascal n'implémente pas nativement la lazy evaluation comme certains langages fonctionnels (Haskell, par exemple), mais nous pouvons la simuler avec plusieurs techniques.

### 1.1 Utilisation de Propriétés Calculées

La méthode la plus simple pour implémenter la lazy evaluation est d'utiliser des propriétés avec un champ privé et un getter.

```pascal
type
  TDataProcessor = class
  private
    FRawData: string;
    FProcessedData: string;
    FIsProcessed: Boolean;
    function GetProcessedData: string;
  public
    constructor Create(const ARawData: string);
    property ProcessedData: string read GetProcessedData;
  end;

constructor TDataProcessor.Create(const ARawData: string);  
begin
  FRawData := ARawData;
  FIsProcessed := False;
  // On ne traite PAS les données ici !
end;

function TDataProcessor.GetProcessedData: string;  
begin
  // On ne calcule que si nécessaire
  if not FIsProcessed then
  begin
    WriteLn('Calcul en cours... (coûteux)');
    // Simulation d'un traitement coûteux
    Sleep(1000);
    FProcessedData := UpperCase(FRawData);
    FIsProcessed := True;
  end;
  Result := FProcessedData;
end;
```

**Utilisation :**

```pascal
var
  Processor: TDataProcessor;
begin
  Processor := TDataProcessor.Create('hello world');
  try
    WriteLn('Objet créé, mais données non traitées');
    // Le calcul ne se produit QUE maintenant :
    WriteLn(Processor.ProcessedData);
    // Pas de recalcul ici :
    WriteLn(Processor.ProcessedData);
  finally
    Processor.Free;
  end;
end;
```

### 1.2 Lazy Evaluation avec des Fonctions Anonymes

Les méthodes anonymes de FreePascal peuvent être utilisées pour créer des calculs différés.

```pascal
type
  TLazyValue<T> = class
  private
    FValue: T;
    FCalculator: TFunc<T>;
    FIsCalculated: Boolean;
    function GetValue: T;
  public
    constructor Create(ACalculator: TFunc<T>);
    property Value: T read GetValue;
    procedure Reset;
  end;

constructor TLazyValue<T>.Create(ACalculator: TFunc<T>);  
begin
  FCalculator := ACalculator;
  FIsCalculated := False;
end;

function TLazyValue<T>.GetValue: T;  
begin
  if not FIsCalculated then
  begin
    FValue := FCalculator();
    FIsCalculated := True;
  end;
  Result := FValue;
end;

procedure TLazyValue<T>.Reset;  
begin
  FIsCalculated := False;
end;
```

**Exemple d'utilisation :**

```pascal
var
  LazySum: TLazyValue<Integer>;
begin
  LazySum := TLazyValue<Integer>.Create(
    function: Integer
    var
      i, Sum: Integer;
    begin
      WriteLn('Calcul de la somme...');
      Sum := 0;
      for i := 1 to 1000000 do
        Sum := Sum + i;
      Result := Sum;
    end
  );

  try
    WriteLn('LazySum créé, mais pas encore calculé');
    WriteLn('Première utilisation :');
    WriteLn(LazySum.Value); // Calcul effectué ici
    WriteLn('Deuxième utilisation :');
    WriteLn(LazySum.Value); // Pas de recalcul
  finally
    LazySum.Free;
  end;
end;
```

### 1.3 Avantages de la Lazy Evaluation

- **Économie de ressources** : Ne calcule que ce qui est nécessaire
- **Amélioration des temps de démarrage** : Les initialisations coûteuses sont différées
- **Gestion de grandes structures de données** : Permet de travailler avec des données infinies ou très grandes

---

## 2. Memoization en FreePascal

La memoization est particulièrement utile pour les fonctions récursives ou les calculs répétitifs.

### 2.1 Memoization Simple avec un Dictionnaire

Voici un exemple classique : le calcul de la suite de Fibonacci.

**Sans memoization (très lent) :**

```pascal
function FibonacciNaif(n: Integer): Int64;  
begin
  if n <= 1 then
    Result := n
  else
    Result := FibonacciNaif(n-1) + FibonacciNaif(n-2);
  // Cette fonction recalcule les mêmes valeurs de nombreuses fois !
end;
```

**Avec memoization (beaucoup plus rapide) :**

```pascal
uses
  Generics.Collections;

type
  TFibonacciMemo = class
  private
    FCache: TDictionary<Integer, Int64>;
  public
    constructor Create;
    destructor Destroy; override;
    function Calculate(n: Integer): Int64;
  end;

constructor TFibonacciMemo.Create;  
begin
  FCache := TDictionary<Integer, Int64>.Create;
end;

destructor TFibonacciMemo.Destroy;  
begin
  FCache.Free;
  inherited;
end;

function TFibonacciMemo.Calculate(n: Integer): Int64;  
begin
  // Vérifier si déjà calculé
  if FCache.ContainsKey(n) then
  begin
    Result := FCache[n];
    Exit;
  end;

  // Cas de base
  if n <= 1 then
    Result := n
  else
    Result := Calculate(n-1) + Calculate(n-2);

  // Stocker dans le cache
  FCache.Add(n, Result);
end;
```

**Comparaison de performance :**

```pascal
var
  Memo: TFibonacciMemo;
  Start: TDateTime;
begin
  // Sans memoization
  Start := Now;
  WriteLn('Fibonacci(40) sans memo : ', FibonacciNaif(40));
  WriteLn('Temps : ', MilliSecondsBetween(Now, Start), ' ms');

  // Avec memoization
  Memo := TFibonacciMemo.Create;
  try
    Start := Now;
    WriteLn('Fibonacci(40) avec memo : ', Memo.Calculate(40));
    WriteLn('Temps : ', MilliSecondsBetween(Now, Start), ' ms');
  finally
    Memo.Free;
  end;
end;
```

### 2.2 Classe Générique de Memoization

Créons une classe réutilisable pour mémoïser n'importe quelle fonction :

```pascal
type
  TMemoizer<TKey, TValue> = class
  private
    FCache: TDictionary<TKey, TValue>;
    FCalculator: TFunc<TKey, TValue>;
  public
    constructor Create(ACalculator: TFunc<TKey, TValue>);
    destructor Destroy; override;
    function Get(const Key: TKey): TValue;
    procedure Clear;
    function IsCached(const Key: TKey): Boolean;
  end;

constructor TMemoizer<TKey, TValue>.Create(ACalculator: TFunc<TKey, TValue>);  
begin
  FCache := TDictionary<TKey, TValue>.Create;
  FCalculator := ACalculator;
end;

destructor TMemoizer<TKey, TValue>.Destroy;  
begin
  FCache.Free;
  inherited;
end;

function TMemoizer<TKey, TValue>.Get(const Key: TKey): TValue;  
begin
  if not FCache.ContainsKey(Key) then
    FCache.Add(Key, FCalculator(Key));
  Result := FCache[Key];
end;

procedure TMemoizer<TKey, TValue>.Clear;  
begin
  FCache.Clear;
end;

function TMemoizer<TKey, TValue>.IsCached(const Key: TKey): Boolean;  
begin
  Result := FCache.ContainsKey(Key);
end;
```

**Utilisation :**

```pascal
var
  SquareMemo: TMemoizer<Integer, Integer>;
begin
  SquareMemo := TMemoizer<Integer, Integer>.Create(
    function(x: Integer): Integer
    begin
      WriteLn('Calcul de ', x, '²');
      Result := x * x;
    end
  );

  try
    WriteLn(SquareMemo.Get(5));  // Calcul effectué
    WriteLn(SquareMemo.Get(5));  // Valeur en cache
    WriteLn(SquareMemo.Get(10)); // Nouveau calcul
  finally
    SquareMemo.Free;
  end;
end;
```

### 2.3 Memoization avec Durée de Vie (Cache Expirant)

Pour éviter que le cache ne devienne trop volumineux, on peut ajouter une durée de vie aux entrées :

```pascal
uses
  Generics.Collections, SysUtils, DateUtils;

type
  TCachedValue<T> = record
    Value: T;
    Timestamp: TDateTime;
  end;

  TTimedMemoizer<TKey, TValue> = class
  private
    FCache: TDictionary<TKey, TCachedValue<TValue>>;
    FCalculator: TFunc<TKey, TValue>;
    FCacheDuration: Integer; // en secondes
  public
    constructor Create(ACalculator: TFunc<TKey, TValue>;
                       ACacheDuration: Integer = 60);
    destructor Destroy; override;
    function Get(const Key: TKey): TValue;
    procedure CleanExpired;
  end;

constructor TTimedMemoizer<TKey, TValue>.Create(
  ACalculator: TFunc<TKey, TValue>; ACacheDuration: Integer);
begin
  FCache := TDictionary<TKey, TCachedValue<TValue>>.Create;
  FCalculator := ACalculator;
  FCacheDuration := ACacheDuration;
end;

destructor TTimedMemoizer<TKey, TValue>.Destroy;  
begin
  FCache.Free;
  inherited;
end;

function TTimedMemoizer<TKey, TValue>.Get(const Key: TKey): TValue;  
var
  CachedVal: TCachedValue<TValue>;
  NewVal: TCachedValue<TValue>;
begin
  if FCache.TryGetValue(Key, CachedVal) then
  begin
    // Vérifier si le cache est encore valide
    if SecondsBetween(Now, CachedVal.Timestamp) < FCacheDuration then
    begin
      Result := CachedVal.Value;
      Exit;
    end
    else
      FCache.Remove(Key); // Cache expiré
  end;

  // Calculer nouvelle valeur
  NewVal.Value := FCalculator(Key);
  NewVal.Timestamp := Now;
  FCache.Add(Key, NewVal);
  Result := NewVal.Value;
end;

procedure TTimedMemoizer<TKey, TValue>.CleanExpired;  
var
  KeysToRemove: TList<TKey>;
  Pair: TPair<TKey, TCachedValue<TValue>>;
  Key: TKey;
begin
  KeysToRemove := TList<TKey>.Create;
  try
    for Pair in FCache do
      if SecondsBetween(Now, Pair.Value.Timestamp) >= FCacheDuration then
        KeysToRemove.Add(Pair.Key);

    for Key in KeysToRemove do
      FCache.Remove(Key);
  finally
    KeysToRemove.Free;
  end;
end;
```

---

## 3. Cas d'Usage Pratiques

### 3.1 Chargement Paresseux de Ressources

```pascal
type
  TImageManager = class
  private
    FImageCache: TDictionary<string, TBitmap>;
    function LoadImage(const FileName: string): TBitmap;
  public
    constructor Create;
    destructor Destroy; override;
    function GetImage(const FileName: string): TBitmap;
  end;

function TImageManager.GetImage(const FileName: string): TBitmap;  
begin
  if not FImageCache.ContainsKey(FileName) then
    FImageCache.Add(FileName, LoadImage(FileName));
  Result := FImageCache[FileName];
end;
```

### 3.2 Calculs Mathématiques Coûteux

```pascal
type
  TPrimeChecker = class
  private
    FPrimeCache: TMemoizer<Integer, Boolean>;
    function IsPrimeCalculation(n: Integer): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function IsPrime(n: Integer): Boolean;
  end;

function TPrimeChecker.IsPrimeCalculation(n: Integer): Boolean;  
var
  i: Integer;
begin
  if n < 2 then Exit(False);
  if n = 2 then Exit(True);
  if n mod 2 = 0 then Exit(False);

  for i := 3 to Trunc(Sqrt(n)) do
    if n mod i = 0 then Exit(False);

  Result := True;
end;

constructor TPrimeChecker.Create;  
begin
  FPrimeCache := TMemoizer<Integer, Boolean>.Create(IsPrimeCalculation);
end;

function TPrimeChecker.IsPrime(n: Integer): Boolean;  
begin
  Result := FPrimeCache.Get(n);
end;
```

### 3.3 Requêtes Base de Données avec Cache

```pascal
type
  TUserRepository = class
  private
    FConnection: TSQLConnection;
    FUserCache: TTimedMemoizer<Integer, TUser>;
    function LoadUserFromDB(UserID: Integer): TUser;
  public
    constructor Create(AConnection: TSQLConnection);
    destructor Destroy; override;
    function GetUser(UserID: Integer): TUser;
    procedure InvalidateCache;
  end;

function TUserRepository.GetUser(UserID: Integer): TUser;  
begin
  // Cache de 5 minutes
  Result := FUserCache.Get(UserID);
end;
```

---

## 4. Considérations Multi-plateforme (Windows/Ubuntu)

### 4.1 Gestion de la Mémoire

Sur **Windows** et **Ubuntu**, la gestion mémoire est similaire, mais quelques points à noter :

- **Windows** : Utilise un gestionnaire de mémoire optimisé pour le bureau
- **Ubuntu** : Peut être plus strict sur la libération de mémoire dans les serveurs

**Bonne pratique :** Toujours libérer les caches volumineux et implémenter des limites de taille.

```pascal
type
  TLimitedCache<TKey, TValue> = class
  private
    FMaxSize: Integer;
    procedure EnforceLimit;
  public
    constructor Create(AMaxSize: Integer = 1000);
  end;
```

### 4.2 Thread-Safety

Sur les deux plateformes, si vous utilisez du multithreading, protégez vos caches :

```pascal
type
  TThreadSafeMemoizer<TKey, TValue> = class
  private
    FLock: TCriticalSection;
    FCache: TDictionary<TKey, TValue>;
  public
    constructor Create;
    destructor Destroy; override;
    function Get(const Key: TKey): TValue;
  end;

function TThreadSafeMemoizer<TKey, TValue>.Get(const Key: TKey): TValue;  
begin
  FLock.Enter;
  try
    // ... accès au cache ...
  finally
    FLock.Leave;
  end;
end;
```

---

## 5. Bonnes Pratiques

### ✅ À Faire

- Utiliser la lazy evaluation pour les initialisations coûteuses
- Mémoïser les fonctions pures (même entrée = même sortie)
- Implémenter des limites de taille de cache
- Nettoyer les caches expirés régulièrement
- Profiler avant et après pour mesurer les gains réels

### ❌ À Éviter

- Mémoïser des fonctions avec effets de bord
- Créer des caches sans limite de taille
- Oublier de libérer les ressources en mémoire
- Utiliser la memoization pour des calculs très rapides (overhead inutile)
- Mémoïser avec des clés trop complexes

---

## 6. Mesure des Performances

Utilisez toujours des benchmarks pour valider vos optimisations :

```pascal
procedure BenchmarkMemoization;  
var
  StartTime: TDateTime;
  i: Integer;
  Memo: TFibonacciMemo;
begin
  WriteLn('=== Sans Memoization ===');
  StartTime := Now;
  for i := 1 to 10 do
    FibonacciNaif(30);
  WriteLn('Temps : ', MilliSecondsBetween(Now, StartTime), ' ms');

  WriteLn('=== Avec Memoization ===');
  Memo := TFibonacciMemo.Create;
  try
    StartTime := Now;
    for i := 1 to 10 do
      Memo.Calculate(30);
    WriteLn('Temps : ', MilliSecondsBetween(Now, StartTime), ' ms');
  finally
    Memo.Free;
  end;
end;
```

---

## Conclusion

La **lazy evaluation** et la **memoization** sont des techniques puissantes pour optimiser vos applications FreePascal/Lazarus :

- **Lazy evaluation** : Retarde les calculs jusqu'à ce qu'ils soient nécessaires
- **Memoization** : Mémorise les résultats pour éviter les recalculs

Ces techniques sont particulièrement utiles pour :
- Les calculs mathématiques coûteux
- Le chargement de ressources
- Les requêtes répétitives
- Les fonctions récursives

Sur **Windows** et **Ubuntu**, les implémentations sont identiques grâce à la portabilité de FreePascal. Pensez simplement à la gestion mémoire et au thread-safety dans vos applications multi-plateformes !

⏭️ [Benchmarking systématique](/20-optimisation-performance/09-benchmarking-systematique.md)
