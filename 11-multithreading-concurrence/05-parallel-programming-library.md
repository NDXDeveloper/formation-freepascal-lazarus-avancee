🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 11.5 Parallel programming library

## Introduction à la programmation parallèle

La **programmation parallèle** consiste à diviser un problème en sous-tâches qui peuvent être exécutées simultanément sur plusieurs cœurs de processeur. Au lieu de gérer manuellement des threads, une bibliothèque de programmation parallèle simplifie ce processus.

### Qu'est-ce qu'une bibliothèque de programmation parallèle ?

Une **parallel programming library** fournit des abstractions de haut niveau pour exécuter du code en parallèle sans se soucier des détails de gestion des threads.

**Analogie** : Au lieu de coordonner manuellement une équipe de travailleurs (créer des threads, les synchroniser, gérer les erreurs), vous donnez simplement le travail à un chef d'équipe (la bibliothèque) qui s'occupe de tout.

### Avantages d'une bibliothèque parallèle

**Par rapport aux threads manuels :**
- ✅ **Simplicité** : Code plus lisible et maintenable
- ✅ **Automatisation** : Gestion automatique des threads
- ✅ **Optimisation** : Utilisation optimale des ressources
- ✅ **Sécurité** : Moins de bugs liés à la concurrence
- ✅ **Portabilité** : S'adapte au nombre de cœurs disponibles

### Concepts fondamentaux

#### Data Parallelism (Parallélisme de données)

Même opération appliquée à différentes données en parallèle.

```
Séquentiel:          Parallèle (4 cœurs):
[1,2,3,4,5,6,7,8]    Core 1: [1,2]
↓ traitement         Core 2: [3,4]
[2,4,6,8,10,12,14,16] Core 3: [5,6]
                     Core 4: [7,8]
                     → [2,4,6,8,10,12,14,16]
```

#### Task Parallelism (Parallélisme de tâches)

Différentes opérations exécutées en parallèle.

```
Tâche A: Charger données    } En parallèle  
Tâche B: Calculer statistiques }  
Tâche C: Générer rapport    }
```

## MTProcs : La bibliothèque parallèle pour FreePascal

**MTProcs** (Multi-Threading Procedures) est une bibliothèque légère pour FreePascal qui simplifie la programmation parallèle.

### Installation de MTProcs

#### Via le gestionnaire de packages Lazarus

1. Ouvrir Lazarus
2. Menu **Package** → **Online Package Manager**
3. Rechercher "mtprocs"
4. Cliquer sur **Install**
5. Recompiler l'IDE

#### Installation manuelle

1. Télécharger depuis : https://wiki.freepascal.org/MTProcs
2. Extraire dans votre dossier de projet
3. Ajouter `mtprocs` dans les uses

```pascal
uses
  Classes, SysUtils, MTProcs;
```

> **Note** : Certains exemples de ce chapitre utilisent des procédures anonymes (`procedure(Index: Integer) begin ... end`) avec `DoParallel`. En mode ObjFPC, cela nécessite `{$modeswitch anonymousfunctions}` (FPC 3.3.1+). L'API réelle de MTProcs utilise des callbacks avec la signature `procedure(Index: PtrInt; Data: Pointer; Item: TMultiThreadProcItem)`. Les exemples sont simplifiés pour la lisibilité.

### Configuration de base

```pascal
program ParallelDemo;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, MTProcs;

begin
  // La bibliothèque détecte automatiquement le nombre de cœurs
  WriteLn('Nombre de processeurs : ', ProcThreadPool.MaxThreadCount);

  // Optionnel : définir manuellement le nombre de threads
  // ProcThreadPool.MaxThreadCount := 4;
end.
```

## Boucles parallèles

### ParallelFor : La base de la parallélisation

La fonction `ParallelFor` exécute une boucle en parallèle.

#### Syntaxe de base

```pascal
procedure ParallelFor(AStartIndex, AEndIndex: Integer;
  const AProc: TProcedureIndex);
```

#### Exemple simple

```pascal
uses
  MTProcs;

procedure TraiterElement(Index: Integer);  
begin
  WriteLn('Traitement de l''élément ', Index, ' par thread ', GetCurrentThreadId);
  Sleep(100); // Simuler un traitement
end;

begin
  // Exécuter en parallèle pour Index de 0 à 9
  ProcThreadPool.DoParallel(@TraiterElement, 0, 9);

  WriteLn('Tous les éléments ont été traités');
end.
```

#### Exemple avec des données réelles

```pascal
uses
  MTProcs;

var
  Nombres: array[0..999] of Integer;
  i: Integer;

procedure InitialiserDonnees;  
var
  i: Integer;
begin
  for i := 0 to High(Nombres) do
    Nombres[i] := i;
end;

procedure DoublerNombre(Index: Integer);  
begin
  Nombres[Index] := Nombres[Index] * 2;
end;

begin
  InitialiserDonnees;

  WriteLn('Doublement des nombres en parallèle...');
  ProcThreadPool.DoParallel(@DoublerNombre, 0, High(Nombres));

  WriteLn('Premiers résultats : ', Nombres[0], ', ', Nombres[1], ', ', Nombres[2]);
  // Affiche : 0, 2, 4
end.
```

### Traitement de tableaux

#### Map : Transformer tous les éléments

```pascal
uses
  MTProcs, Math;

type
  TDoubleArray = array of Double;

procedure MapParallel(var Data: TDoubleArray; Func: function(X: Double): Double);  
var
  i: Integer;
begin
  ProcThreadPool.DoParallel(
    procedure(Index: Integer)
    begin
      Data[Index] := Func(Data[Index]);
    end,
    0, High(Data)
  );
end;

function Carre(X: Double): Double;  
begin
  Result := X * X;
end;

var
  Nombres: TDoubleArray;
  i: Integer;
begin
  // Initialiser avec 1000 nombres
  SetLength(Nombres, 1000);
  for i := 0 to High(Nombres) do
    Nombres[i] := i + 1;

  // Calculer les carrés en parallèle
  MapParallel(Nombres, @Carre);

  WriteLn('Carré de 10 = ', Nombres[9]:0:0);  // 100
  WriteLn('Carré de 100 = ', Nombres[99]:0:0); // 10000
end.
```

#### Filter : Filtrer les éléments

```pascal
uses
  MTProcs, Generics.Collections;

type
  TIntegerList = specialize TList<Integer>;

function FilterParallel(const Data: array of Integer;
  Predicate: function(X: Integer): Boolean): TIntegerList;
var
  Results: array of TIntegerList;
  i, ThreadCount: Integer;
begin
  ThreadCount := ProcThreadPool.MaxThreadCount;
  SetLength(Results, ThreadCount);

  // Créer une liste par thread
  for i := 0 to ThreadCount - 1 do
    Results[i] := TIntegerList.Create;

  try
    // Chaque thread filtre sa portion
    ProcThreadPool.DoParallel(
      procedure(Index: Integer)
      var
        ThreadID: Integer;
      begin
        ThreadID := Index mod ThreadCount;
        if Predicate(Data[Index]) then
          Results[ThreadID].Add(Data[Index]);
      end,
      0, High(Data)
    );

    // Fusionner les résultats
    Result := TIntegerList.Create;
    for i := 0 to ThreadCount - 1 do
    begin
      Result.AddRange(Results[i]);
      Results[i].Free;
    end;
  except
    for i := 0 to ThreadCount - 1 do
      Results[i].Free;
    raise;
  end;
end;

function EstPair(X: Integer): Boolean;  
begin
  Result := (X mod 2) = 0;
end;

var
  Nombres: array[0..999] of Integer;
  Pairs: TIntegerList;
  i: Integer;
begin
  // Initialiser
  for i := 0 to High(Nombres) do
    Nombres[i] := i;

  // Filtrer les nombres pairs en parallèle
  Pairs := FilterParallel(Nombres, @EstPair);
  try
    WriteLn('Nombres pairs trouvés : ', Pairs.Count);
    WriteLn('Premiers pairs : ', Pairs[0], ', ', Pairs[1], ', ', Pairs[2]);
  finally
    Pairs.Free;
  end;
end.
```

#### Reduce : Agréger les résultats

```pascal
uses
  MTProcs, SyncObjs;

function SumParallel(const Data: array of Integer): Int64;  
var
  PartialSums: array of Int64;
  CS: TCriticalSection;
  i: Integer;
begin
  SetLength(PartialSums, ProcThreadPool.MaxThreadCount);
  CS := TCriticalSection.Create;

  try
    // Chaque thread calcule sa somme partielle
    ProcThreadPool.DoParallel(
      procedure(Index: Integer)
      var
        ThreadID: Integer;
      begin
        ThreadID := Index mod Length(PartialSums);

        CS.Enter;
        try
          PartialSums[ThreadID] := PartialSums[ThreadID] + Data[Index];
        finally
          CS.Leave;
        end;
      end,
      0, High(Data)
    );

    // Additionner les sommes partielles
    Result := 0;
    for i := 0 to High(PartialSums) do
      Result := Result + PartialSums[i];
  finally
    CS.Free;
  end;
end;

var
  Nombres: array[0..9999] of Integer;
  i: Integer;
  Somme: Int64;
begin
  // Initialiser avec 1, 2, 3, ..., 10000
  for i := 0 to High(Nombres) do
    Nombres[i] := i + 1;

  // Calculer la somme en parallèle
  Somme := SumParallel(Nombres);

  WriteLn('Somme de 1 à 10000 = ', Somme);
  // Résultat attendu : 50005000
end.
```

## Tâches parallèles

### Exécuter plusieurs tâches indépendantes

```pascal
uses
  MTProcs;

procedure Tache1;  
begin
  WriteLn('Tâche 1 : Chargement des données...');
  Sleep(1000);
  WriteLn('Tâche 1 : Terminée');
end;

procedure Tache2;  
begin
  WriteLn('Tâche 2 : Calcul des statistiques...');
  Sleep(1500);
  WriteLn('Tâche 2 : Terminée');
end;

procedure Tache3;  
begin
  WriteLn('Tâche 3 : Génération du rapport...');
  Sleep(800);
  WriteLn('Tâche 3 : Terminée');
end;

begin
  WriteLn('Démarrage des tâches parallèles...');

  ProcThreadPool.DoParallelLocalProc(
    procedure
    begin
      Tache1;
    end,
    procedure
    begin
      Tache2;
    end,
    procedure
    begin
      Tache3;
    end
  );

  WriteLn('Toutes les tâches sont terminées');
end.
```

### Fork-Join pattern

Diviser une tâche, traiter en parallèle, puis fusionner les résultats.

```pascal
uses
  MTProcs, Classes;

type
  TResultat = record
    Min: Integer;
    Max: Integer;
    Somme: Int64;
  end;

function AnalyserTableau(const Data: array of Integer;
  StartIndex, EndIndex: Integer): TResultat;
var
  i: Integer;
begin
  Result.Min := Data[StartIndex];
  Result.Max := Data[StartIndex];
  Result.Somme := 0;

  for i := StartIndex to EndIndex do
  begin
    if Data[i] < Result.Min then
      Result.Min := Data[i];
    if Data[i] > Result.Max then
      Result.Max := Data[i];
    Result.Somme := Result.Somme + Data[i];
  end;
end;

function AnalyserParallel(const Data: array of Integer): TResultat;  
var
  ThreadCount, ChunkSize, i: Integer;
  Results: array of TResultat;
begin
  ThreadCount := ProcThreadPool.MaxThreadCount;
  ChunkSize := (Length(Data) + ThreadCount - 1) div ThreadCount;
  SetLength(Results, ThreadCount);

  // FORK : Diviser le travail
  ProcThreadPool.DoParallel(
    procedure(ThreadIndex: Integer)
    var
      StartIdx, EndIdx: Integer;
    begin
      StartIdx := ThreadIndex * ChunkSize;
      EndIdx := Min(StartIdx + ChunkSize - 1, High(Data));

      if StartIdx <= High(Data) then
        Results[ThreadIndex] := AnalyserTableau(Data, StartIdx, EndIdx);
    end,
    0, ThreadCount - 1
  );

  // JOIN : Fusionner les résultats
  Result := Results[0];
  for i := 1 to ThreadCount - 1 do
  begin
    if Results[i].Min < Result.Min then
      Result.Min := Results[i].Min;
    if Results[i].Max > Result.Max then
      Result.Max := Results[i].Max;
    Result.Somme := Result.Somme + Results[i].Somme;
  end;
end;

var
  Nombres: array[0..9999] of Integer;
  i: Integer;
  Stats: TResultat;
begin
  // Initialiser avec des nombres aléatoires
  Randomize;
  for i := 0 to High(Nombres) do
    Nombres[i] := Random(1000);

  Stats := AnalyserParallel(Nombres);

  WriteLn('Analyse parallèle terminée :');
  WriteLn('  Minimum : ', Stats.Min);
  WriteLn('  Maximum : ', Stats.Max);
  WriteLn('  Somme : ', Stats.Somme);
  WriteLn('  Moyenne : ', Stats.Somme / Length(Nombres):0:2);
end.
```

## Cas d'usage pratiques

### 1. Traitement d'images en parallèle

```pascal
uses
  MTProcs, Graphics, Classes;

type
  TRGB = record
    R, G, B: Byte;
  end;

  TImageData = array of array of TRGB;

procedure ConvertirEnNoirEtBlanc(var Image: TImageData);  
var
  Height: Integer;
begin
  Height := Length(Image);

  ProcThreadPool.DoParallel(
    procedure(Y: Integer)
    var
      X, Gris: Integer;
    begin
      for X := 0 to High(Image[Y]) do
      begin
        // Formule de luminance
        Gris := Round(0.299 * Image[Y][X].R +
                     0.587 * Image[Y][X].G +
                     0.114 * Image[Y][X].B);

        Image[Y][X].R := Gris;
        Image[Y][X].G := Gris;
        Image[Y][X].B := Gris;
      end;
    end,
    0, Height - 1
  );
end;

procedure AppliquerFlou(var Image: TImageData; Rayon: Integer);  
var
  Height: Integer;
  Temp: TImageData;
begin
  Height := Length(Image);
  SetLength(Temp, Height, Length(Image[0]));

  ProcThreadPool.DoParallel(
    procedure(Y: Integer)
    var
      X, DX, DY, Count: Integer;
      SommeR, SommeG, SommeB: Integer;
    begin
      for X := 0 to High(Image[Y]) do
      begin
        SommeR := 0; SommeG := 0; SommeB := 0; Count := 0;

        // Moyenner les pixels voisins
        for DY := -Rayon to Rayon do
          for DX := -Rayon to Rayon do
            if (Y + DY >= 0) and (Y + DY < Height) and
               (X + DX >= 0) and (X + DX <= High(Image[Y])) then
            begin
              SommeR := SommeR + Image[Y + DY][X + DX].R;
              SommeG := SommeG + Image[Y + DY][X + DX].G;
              SommeB := SommeB + Image[Y + DY][X + DX].B;
              Inc(Count);
            end;

        Temp[Y][X].R := SommeR div Count;
        Temp[Y][X].G := SommeG div Count;
        Temp[Y][X].B := SommeB div Count;
      end;
    end,
    0, Height - 1
  );

  Image := Temp;
end;

var
  Image: TImageData;
begin
  // Charger l'image...
  // SetLength(Image, hauteur, largeur);

  WriteLn('Conversion en noir et blanc...');
  ConvertirEnNoirEtBlanc(Image);

  WriteLn('Application du flou...');
  AppliquerFlou(Image, 2);

  WriteLn('Traitement terminé');
end.
```

### 2. Tri parallèle (Parallel Merge Sort)

```pascal
uses
  MTProcs, Math, SysUtils, DateUtils;

procedure MergeSort(var Data: array of Integer; Left, Right: Integer);  
var
  Middle: Integer;

  procedure Merge(L, M, R: Integer);
  var
    Temp: array of Integer;
    i, j, k: Integer;
  begin
    SetLength(Temp, R - L + 1);
    i := L;
    j := M + 1;
    k := 0;

    while (i <= M) and (j <= R) do
    begin
      if Data[i] <= Data[j] then
      begin
        Temp[k] := Data[i];
        Inc(i);
      end
      else
      begin
        Temp[k] := Data[j];
        Inc(j);
      end;
      Inc(k);
    end;

    while i <= M do
    begin
      Temp[k] := Data[i];
      Inc(i);
      Inc(k);
    end;

    while j <= R do
    begin
      Temp[k] := Data[j];
      Inc(j);
      Inc(k);
    end;

    for k := 0 to High(Temp) do
      Data[L + k] := Temp[k];
  end;

begin
  if Left < Right then
  begin
    Middle := (Left + Right) div 2;

    // Trier en parallèle si le tableau est assez grand
    if (Right - Left > 1000) and (ProcThreadPool.MaxThreadCount > 1) then
    begin
      ProcThreadPool.DoParallelLocalProc(
        procedure
        begin
          MergeSort(Data, Left, Middle);
        end,
        procedure
        begin
          MergeSort(Data, Middle + 1, Right);
        end
      );
    end
    else
    begin
      MergeSort(Data, Left, Middle);
      MergeSort(Data, Middle + 1, Right);
    end;

    Merge(Left, Middle, Right);
  end;
end;

var
  Nombres: array[0..99999] of Integer;
  i: Integer;
  Start: TDateTime;
begin
  // Générer des nombres aléatoires
  Randomize;
  for i := 0 to High(Nombres) do
    Nombres[i] := Random(1000000);

  WriteLn('Tri de ', Length(Nombres), ' éléments...');
  Start := Now;

  MergeSort(Nombres, 0, High(Nombres));

  WriteLn('Terminé en ', MilliSecondsBetween(Now, Start), ' ms');
  WriteLn('Premier : ', Nombres[0], ', Dernier : ', Nombres[High(Nombres)]);
end.
```

### 3. Calcul de Pi par méthode Monte Carlo

```pascal
uses
  MTProcs, Math, SysUtils, DateUtils, SyncObjs;

function CalculerPiParallel(NombreEchantillons: Int64): Double;  
var
  PointsDansCercle: Int64;
  CS: TCriticalSection;
  ThreadCount, i: Integer;
  EchantillonsParThread: Int64;
begin
  PointsDansCercle := 0;
  CS := TCriticalSection.Create;
  ThreadCount := ProcThreadPool.MaxThreadCount;
  EchantillonsParThread := NombreEchantillons div ThreadCount;

  try
    ProcThreadPool.DoParallel(
      procedure(ThreadIndex: Integer)
      var
        i: Int64;
        X, Y, Distance: Double;
        LocalCount: Int64;
      begin
        LocalCount := 0;

        // Chaque thread génère ses échantillons
        for i := 1 to EchantillonsParThread do
        begin
          X := Random;
          Y := Random;
          Distance := Sqrt(X*X + Y*Y);

          if Distance <= 1.0 then
            Inc(LocalCount);
        end;

        // Ajouter au total de manière thread-safe
        CS.Enter;
        try
          PointsDansCercle := PointsDansCercle + LocalCount;
        finally
          CS.Leave;
        end;
      end,
      0, ThreadCount - 1
    );

    // Pi ≈ 4 * (points dans cercle / points totaux)
    Result := 4.0 * PointsDansCercle / NombreEchantillons;
  finally
    CS.Free;
  end;
end;

var
  PiEstime: Double;
  Start: TDateTime;
begin
  Randomize;

  WriteLn('Calcul de Pi par méthode Monte Carlo...');
  WriteLn('Utilisation de ', ProcThreadPool.MaxThreadCount, ' threads');

  Start := Now;
  PiEstime := CalculerPiParallel(100000000); // 100 millions d'échantillons

  WriteLn('Pi estimé : ', PiEstime:0:6);
  WriteLn('Pi réel   : ', Pi:0:6);
  WriteLn('Erreur    : ', Abs(Pi - PiEstime):0:6);
  WriteLn('Temps     : ', MilliSecondsBetween(Now, Start), ' ms');
end.
```

### 4. Recherche parallèle dans des fichiers

```pascal
uses
  MTProcs, Classes, SysUtils, FileUtil; // FileUtil pour FindAllFiles

type
  TSearchResult = record
    FileName: string;
    LineNumber: Integer;
    Line: string;
  end;

  TSearchResults = array of TSearchResult;

function RechercherDansFichier(const FileName, SearchText: string): TSearchResults;  
var
  Lines: TStringList;
  i: Integer;
  Results: TSearchResults;
  Count: Integer;
begin
  SetLength(Results, 0);
  Count := 0;

  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(FileName);

    for i := 0 to Lines.Count - 1 do
    begin
      if Pos(SearchText, Lines[i]) > 0 then
      begin
        SetLength(Results, Count + 1);
        Results[Count].FileName := FileName;
        Results[Count].LineNumber := i + 1;
        Results[Count].Line := Lines[i];
        Inc(Count);
      end;
    end;
  finally
    Lines.Free;
  end;

  Result := Results;
end;

function RechercherDansRepertoire(const Directory, SearchText: string): TSearchResults;  
var
  FileList: TStringList;
  AllResults: array of TSearchResults;
  i, j, TotalCount: Integer;
  FinalResults: TSearchResults;
begin
  // Obtenir la liste des fichiers
  FileList := TStringList.Create;
  try
    FindAllFiles(FileList, Directory, '*.pas;*.pp;*.inc', True);

    SetLength(AllResults, FileList.Count);

    // Rechercher en parallèle dans tous les fichiers
    ProcThreadPool.DoParallel(
      procedure(Index: Integer)
      begin
        AllResults[Index] := RechercherDansFichier(FileList[Index], SearchText);
      end,
      0, FileList.Count - 1
    );

    // Compter les résultats totaux
    TotalCount := 0;
    for i := 0 to High(AllResults) do
      TotalCount := TotalCount + Length(AllResults[i]);

    // Fusionner tous les résultats
    SetLength(FinalResults, TotalCount);
    TotalCount := 0;

    for i := 0 to High(AllResults) do
      for j := 0 to High(AllResults[i]) do
      begin
        FinalResults[TotalCount] := AllResults[i][j];
        Inc(TotalCount);
      end;

    Result := FinalResults;
  finally
    FileList.Free;
  end;
end;

var
  Results: TSearchResults;
  i: Integer;
begin
  WriteLn('Recherche de "procedure" dans tous les fichiers Pascal...');

  Results := RechercherDansRepertoire('C:\MonProjet', 'procedure');

  WriteLn(Length(Results), ' occurrences trouvées');

  for i := 0 to Min(9, High(Results)) do
    WriteLn(Format('  %s:%d: %s',
      [Results[i].FileName, Results[i].LineNumber, Trim(Results[i].Line)]));

  if Length(Results) > 10 then
    WriteLn('  ... et ', Length(Results) - 10, ' autres');
end.
```

## Performance et optimisation

### Mesurer le speedup

Le **speedup** mesure le gain de performance du code parallèle par rapport au code séquentiel.

```pascal
uses
  MTProcs, SysUtils, DateUtils;

type
  TBenchmarkResult = record
    SequentialTime: Int64;
    ParallelTime: Int64;
    Speedup: Double;
    Efficiency: Double;
  end;

function BenchmarkParallel(SequentialProc, ParallelProc: TProcedure): TBenchmarkResult;  
var
  Start: TDateTime;
  ThreadCount: Integer;
begin
  ThreadCount := ProcThreadPool.MaxThreadCount;

  // Mesurer le temps séquentiel
  WriteLn('Exécution séquentielle...');
  Start := Now;
  SequentialProc();
  Result.SequentialTime := MilliSecondsBetween(Now, Start);

  // Mesurer le temps parallèle
  WriteLn('Exécution parallèle (', ThreadCount, ' threads)...');
  Start := Now;
  ParallelProc();
  Result.ParallelTime := MilliSecondsBetween(Now, Start);

  // Calculer les métriques
  Result.Speedup := Result.SequentialTime / Result.ParallelTime;
  Result.Efficiency := Result.Speedup / ThreadCount;

  WriteLn('Résultats :');
  WriteLn('  Temps séquentiel : ', Result.SequentialTime, ' ms');
  WriteLn('  Temps parallèle  : ', Result.ParallelTime, ' ms');
  WriteLn('  Speedup          : ', Result.Speedup:0:2, 'x');
  WriteLn('  Efficacité       : ', Result.Efficiency * 100:0:1, '%');
end;
```

### Loi d'Amdahl

La **loi d'Amdahl** prédit le speedup maximal possible.

```
Speedup max = 1 / (S + P/N)

Où:
- S = portion séquentielle (non parallélisable)
- P = portion parallélisable
- N = nombre de processeurs
```

**Exemple :**
```pascal
// Si 90% du code est parallélisable (S=0.1, P=0.9)
// Avec 4 cœurs:
Speedup = 1 / (0.1 + 0.9/4) = 1 / 0.325 = 3.08x

// Avec un nombre infini de cœurs:
Speedup max = 1 / 0.1 = 10x
```

### Quand ne PAS paralléliser

❌ **N'utilisez PAS la parallélisation si :**

1. **Tâche trop petite**
```pascal
// ❌ MAUVAIS - Overhead > gain
ProcThreadPool.DoParallel(
  procedure(Index: Integer)
  begin
    Result[Index] := Data[Index] * 2; // Trop simple !
  end,
  0, 99  // Seulement 100 éléments
);

// ✅ BON - Faire séquentiellement
for i := 0 to 99 do
  Result[i] := Data[i] * 2;
```

2. **Dépendances entre itérations**
```pascal
// ❌ IMPOSSIBLE à paralléliser
for i := 1 to 999 do
  Data[i] := Data[i-1] + 1; // Dépend de l'itération précédente !
```

3. **Accès concurrents complexes**
```pascal
// ❌ MAUVAIS - Beaucoup de synchronisation
ProcThreadPool.DoParallel(
  procedure(Index: Integer)
  begin
    CS.Enter; // Tous les threads attendent !
    try
      SharedList.Add(Data[Index]);
    finally
      CS.Leave;
    end;
  end,
  0, 999
);

// ✅ BON - Utiliser des listes locales puis fusionner
var
  LocalLists: array of TList;
begin
  SetLength(LocalLists, ProcThreadPool.MaxThreadCount);

  ProcThreadPool.DoParallel(
    procedure(Index: Integer)
    var
      ThreadID: Integer;
    begin
      ThreadID := Index mod Length(LocalLists);
      LocalLists[ThreadID].Add(Data[Index]); // Pas de verrou !
    end,
    0, 999
  );

  // Fusionner les listes locales (séquentiel, mais rapide)
  for i := 0 to High(LocalLists) do
    SharedList.AddRange(LocalLists[i]);
end;
```

4. **I/O intensif**
```pascal
// ❌ PEU UTILE - Les threads attendent l'I/O
ProcThreadPool.DoParallel(
  procedure(Index: Integer)
  begin
    // La plupart du temps est passé à attendre le disque
    Data[Index] := LoadFromFile(Files[Index]);
  end,
  0, High(Files)
);

// ✅ MIEUX - Utiliser l'I/O asynchrone ou un pool de taille limitée
```

### Optimisation de la taille des chunks

La **granularité** (taille des tâches) impacte fortement les performances.

```pascal
uses
  MTProcs;

type
  TGranularity = (gFine, gMedium, gCoarse);

procedure ProcessWithGranularity(const Data: array of Integer;
  Granularity: TGranularity);
var
  ChunkSize: Integer;
begin
  case Granularity of
    gFine:   ChunkSize := 1;      // Une tâche par élément
    gMedium: ChunkSize := 100;    // 100 éléments par tâche
    gCoarse: ChunkSize := 10000;  // 10000 éléments par tâche
  end;

  // Traiter par chunks
  ProcThreadPool.DoParallel(
    procedure(ChunkIndex: Integer)
    var
      StartIdx, EndIdx, i: Integer;
    begin
      StartIdx := ChunkIndex * ChunkSize;
      EndIdx := Min(StartIdx + ChunkSize - 1, High(Data));

      for i := StartIdx to EndIdx do
        ProcessElement(Data[i]);
    end,
    0, (Length(Data) + ChunkSize - 1) div ChunkSize - 1
  );
end;
```

**Règles empiriques :**
- **Granularité fine** : Meilleure répartition mais overhead élevé
- **Granularité grossière** : Moins d'overhead mais déséquilibre possible
- **Optimal** : 2-10x plus de tâches que de threads

### Load balancing (Équilibrage de charge)

Éviter que certains threads restent inactifs pendant que d'autres travaillent.

```pascal
uses
  MTProcs;

// ❌ MAUVAIS - Déséquilibre si les tâches ont des durées variables
procedure MauvaisEquilibrage(const Tasks: array of Integer);  
var
  ChunkSize, ThreadCount: Integer;
begin
  ThreadCount := ProcThreadPool.MaxThreadCount;
  ChunkSize := Length(Tasks) div ThreadCount;

  ProcThreadPool.DoParallel(
    procedure(ThreadIndex: Integer)
    var
      StartIdx, EndIdx, i: Integer;
    begin
      StartIdx := ThreadIndex * ChunkSize;
      EndIdx := Min(StartIdx + ChunkSize - 1, High(Tasks));

      for i := StartIdx to EndIdx do
        ProcessTask(Tasks[i]); // Durée variable !
    end,
    0, ThreadCount - 1
  );
end;

// ✅ BON - Work stealing : les threads piochent dans une queue commune
procedure BonEquilibrage(const Tasks: array of Integer);  
begin
  // MTProcs fait du work stealing automatiquement
  ProcThreadPool.DoParallel(
    procedure(Index: Integer)
    begin
      ProcessTask(Tasks[Index]);
    end,
    0, High(Tasks)
  );
end;
```

## Patterns avancés

### Pipeline parallèle

Traiter des données par étapes, chaque étape s'exécutant en parallèle.

```pascal
uses
  MTProcs, SyncObjs, Generics.Collections;

type
  TDataItem = Integer;
  TStage1Queue = specialize TThreadedQueue<TDataItem>;
  TStage2Queue = specialize TThreadedQueue<TDataItem>;

  TPipeline = class;

  { Thread générique pour les étapes du pipeline
    (CreateAnonymousThread ne peut pas prendre une méthode) }
  TPipelineStageThread = class(TThread)
  private
    FPipeline: TPipeline;
    FStageIndex: Integer;
  protected
    procedure Execute; override;
  public
    constructor Create(APipeline: TPipeline; AStageIndex: Integer);
  end;

  TPipeline = class
  private
    FStage1Queue: TStage1Queue;
    FStage2Queue: TStage2Queue;
    FOutputQueue: TStage2Queue;
    FWorkers: array[0..2] of TPipelineStageThread;
    FRunning: Boolean;

    procedure Stage1Worker;
    procedure Stage2Worker;
    procedure Stage3Worker;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Start;
    procedure Stop;
    procedure AddInput(Data: TDataItem);
    function GetOutput(out Data: TDataItem; Timeout: Cardinal): Boolean;
  end;

implementation

{ TPipelineStageThread }

constructor TPipelineStageThread.Create(APipeline: TPipeline; AStageIndex: Integer);  
begin
  inherited Create(False); // Démarrer immédiatement
  FPipeline := APipeline;
  FStageIndex := AStageIndex;
  FreeOnTerminate := False;
end;

procedure TPipelineStageThread.Execute;  
begin
  case FStageIndex of
    0: FPipeline.Stage1Worker;
    1: FPipeline.Stage2Worker;
    2: FPipeline.Stage3Worker;
  end;
end;

{ TPipeline }

constructor TPipeline.Create;  
begin
  inherited Create;

  FStage1Queue := TStage1Queue.Create(1000, INFINITE, INFINITE);
  FStage2Queue := TStage2Queue.Create(1000, INFINITE, INFINITE);
  FOutputQueue := TStage2Queue.Create(1000, INFINITE, INFINITE);
  FRunning := False;
end;

destructor TPipeline.Destroy;  
begin
  Stop;
  FStage1Queue.Free;
  FStage2Queue.Free;
  FOutputQueue.Free;
  inherited;
end;

procedure TPipeline.Stage1Worker;  
var
  Data: TDataItem;
begin
  while FRunning do
  begin
    if FStage1Queue.PopItem(Data) = wrSignaled then
    begin
      // Étape 1 : Validation des données
      if Data > 0 then
      begin
        Data := Data * 2;
        FStage2Queue.PushItem(Data);
      end;
    end;
  end;
end;

procedure TPipeline.Stage2Worker;  
var
  Data: TDataItem;
begin
  while FRunning do
  begin
    if FStage2Queue.PopItem(Data) = wrSignaled then
    begin
      // Étape 2 : Transformation
      Data := Data + 100;
      FOutputQueue.PushItem(Data);
    end;
  end;
end;

procedure TPipeline.Stage3Worker;  
var
  Data: TDataItem;
begin
  while FRunning do
  begin
    if FOutputQueue.PopItem(Data) = wrSignaled then
    begin
      // Étape 3 : Logging ou sauvegarde
      WriteLn('Résultat final : ', Data);
    end;
  end;
end;

procedure TPipeline.Start;  
begin
  FRunning := True;

  FWorkers[0] := TPipelineStageThread.Create(Self, 0);
  FWorkers[1] := TPipelineStageThread.Create(Self, 1);
  FWorkers[2] := TPipelineStageThread.Create(Self, 2);
end;

procedure TPipeline.Stop;  
var
  i: Integer;
begin
  FRunning := False;

  for i := 0 to 2 do
    if Assigned(FWorkers[i]) then
    begin
      FWorkers[i].WaitFor;
      FWorkers[i].Free;
    end;
end;

procedure TPipeline.AddInput(Data: TDataItem);  
begin
  FStage1Queue.PushItem(Data);
end;

function TPipeline.GetOutput(out Data: TDataItem; Timeout: Cardinal): Boolean;  
begin
  Result := FOutputQueue.PopItem(Data, Timeout) = wrSignaled;
end;

// Utilisation
var
  Pipeline: TPipeline;
  i: Integer;
begin
  Pipeline := TPipeline.Create;
  try
    Pipeline.Start;

    // Injecter des données
    for i := 1 to 100 do
      Pipeline.AddInput(i);

    Sleep(5000); // Laisser le pipeline traiter
  finally
    Pipeline.Free;
  end;
end;
```

### Map-Reduce parallèle

Pattern classique pour traiter de grandes quantités de données.

```pascal
uses
  MTProcs, Generics.Collections;

type
  TMapFunc<TInput, TOutput> = function(const Input: TInput): TOutput;
  TReduceFunc<T> = function(const A, B: T): T;

// Map : Appliquer une fonction à tous les éléments
function ParallelMap<TInput, TOutput>(
  const Inputs: array of TInput;
  MapFunc: TMapFunc<TInput, TOutput>): specialize TArray<TOutput>;
var
  Outputs: specialize TArray<TOutput>;
begin
  SetLength(Outputs, Length(Inputs));

  ProcThreadPool.DoParallel(
    procedure(Index: Integer)
    begin
      Outputs[Index] := MapFunc(Inputs[Index]);
    end,
    0, High(Inputs)
  );

  Result := Outputs;
end;

// Reduce : Combiner tous les éléments
function ParallelReduce<T>(
  const Inputs: array of T;
  ReduceFunc: TReduceFunc<T>;
  InitialValue: T): T;
var
  ThreadCount, i: Integer;
  PartialResults: array of T;
begin
  ThreadCount := ProcThreadPool.MaxThreadCount;
  SetLength(PartialResults, ThreadCount);

  // Initialiser avec la valeur initiale
  for i := 0 to ThreadCount - 1 do
    PartialResults[i] := InitialValue;

  // Réduction partielle par thread
  ProcThreadPool.DoParallel(
    procedure(Index: Integer)
    var
      ThreadID: Integer;
    begin
      ThreadID := Index mod ThreadCount;
      PartialResults[ThreadID] := ReduceFunc(
        PartialResults[ThreadID],
        Inputs[Index]
      );
    end,
    0, High(Inputs)
  );

  // Réduction finale (séquentielle)
  Result := PartialResults[0];
  for i := 1 to High(PartialResults) do
    Result := ReduceFunc(Result, PartialResults[i]);
end;

// Fonctions d'exemple
function Carre(const X: Integer): Integer;  
begin
  Result := X * X;
end;

function Additionner(const A, B: Integer): Integer;  
begin
  Result := A + B;
end;

var
  Nombres: array[0..999] of Integer;
  Carres: array of Integer;
  Somme: Integer;
  i: Integer;
begin
  // Initialiser
  for i := 0 to High(Nombres) do
    Nombres[i] := i + 1;

  // Map : Calculer les carrés
  Carres := ParallelMap<Integer, Integer>(Nombres, @Carre);

  // Reduce : Calculer la somme
  Somme := ParallelReduce<Integer>(Carres, @Additionner, 0);

  WriteLn('Somme des carrés de 1 à 1000 = ', Somme);
  // Résultat attendu : 333833500
end.
```

### Parallel Quicksort

Implémentation parallèle du tri rapide.

```pascal
uses
  MTProcs, SysUtils, DateUtils;

procedure ParallelQuickSort(var Data: array of Integer; Left, Right: Integer);  
const
  THRESHOLD = 1000; // En dessous de ce seuil, tri séquentiel
var
  Pivot, Temp, i, j: Integer;
begin
  if Left >= Right then
    Exit;

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

  // Trier les partitions
  if (Right - Left > THRESHOLD) and (ProcThreadPool.MaxThreadCount > 1) then
  begin
    // Parallèle pour grandes partitions
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
  end
  else
  begin
    // Séquentiel pour petites partitions
    if Left < j then
      ParallelQuickSort(Data, Left, j);
    if i < Right then
      ParallelQuickSort(Data, i, Right);
  end;
end;

var
  Nombres: array[0..99999] of Integer;
  i: Integer;
  Start: TDateTime;
begin
  // Générer des nombres aléatoires
  Randomize;
  for i := 0 to High(Nombres) do
    Nombres[i] := Random(1000000);

  WriteLn('Tri parallèle de ', Length(Nombres), ' éléments...');
  Start := Now;

  ParallelQuickSort(Nombres, 0, High(Nombres));

  WriteLn('Terminé en ', MilliSecondsBetween(Now, Start), ' ms');

  // Vérifier que c'est trié
  for i := 0 to High(Nombres) - 1 do
    if Nombres[i] > Nombres[i + 1] then
    begin
      WriteLn('ERREUR : Tableau mal trié !');
      Exit;
    end;

  WriteLn('Tri correct : ', Nombres[0], ' ... ', Nombres[High(Nombres)]);
end.
```

## Gestion des exceptions

### Capturer les exceptions dans les threads

```pascal
uses
  MTProcs, SysUtils;

type
  TParallelException = class(Exception)
  private
    FExceptions: array of Exception;
  public
    constructor Create(const Exceptions: array of Exception);
    destructor Destroy; override;

    function GetExceptionCount: Integer;
    function GetException(Index: Integer): Exception;
  end;

procedure SafeParallelFor(StartIndex, EndIndex: Integer;
  Proc: TProcedureIndex);
var
  Exceptions: array of Exception;
  CS: TCriticalSection;
begin
  SetLength(Exceptions, 0);
  CS := TCriticalSection.Create;

  try
    ProcThreadPool.DoParallel(
      procedure(Index: Integer)
      begin
        try
          Proc(Index);
        except
          on E: Exception do
          begin
            CS.Enter;
            try
              SetLength(Exceptions, Length(Exceptions) + 1);
              Exceptions[High(Exceptions)] := Exception.Create(E.Message);
            finally
              CS.Leave;
            end;
          end;
        end;
      end,
      StartIndex, EndIndex
    );

    // Si des exceptions ont été capturées, les relancer
    if Length(Exceptions) > 0 then
      raise TParallelException.Create(Exceptions);
  finally
    CS.Free;
  end;
end;

// Utilisation
procedure TraiterAvecGestionErreurs;  
begin
  try
    SafeParallelFor(0, 99,
      procedure(Index: Integer)
      begin
        if Index = 50 then
          raise Exception.Create('Erreur à l''index 50');

        WriteLn('Traitement ', Index);
      end
    );
  except
    on E: TParallelException do
    begin
      WriteLn(E.GetExceptionCount, ' erreurs capturées :');
      for i := 0 to E.GetExceptionCount - 1 do
        WriteLn('  ', E.GetException(i).Message);
    end;
  end;
end;
```

## Debugging et profilage

### Visualiser l'activité des threads

```pascal
uses
  MTProcs, SyncObjs, SysUtils, DateUtils;

type
  TThreadActivity = record
    ThreadID: TThreadID;
    StartTime: TDateTime;
    EndTime: TDateTime;
    TaskIndex: Integer;
  end;

  TActivityLogger = class
  private
    FActivities: array of TThreadActivity;
    FCS: TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LogStart(TaskIndex: Integer);
    procedure LogEnd(TaskIndex: Integer);
    procedure PrintReport;
  end;

constructor TActivityLogger.Create;  
begin
  inherited;
  SetLength(FActivities, 0);
  FCS := TCriticalSection.Create;
end;

destructor TActivityLogger.Destroy;  
begin
  FCS.Free;
  inherited;
end;

procedure TActivityLogger.LogStart(TaskIndex: Integer);  
var
  Activity: TThreadActivity;
begin
  Activity.ThreadID := GetCurrentThreadId;
  Activity.StartTime := Now;
  Activity.TaskIndex := TaskIndex;

  FCS.Enter;
  try
    SetLength(FActivities, Length(FActivities) + 1);
    FActivities[High(FActivities)] := Activity;
  finally
    FCS.Leave;
  end;
end;

procedure TActivityLogger.LogEnd(TaskIndex: Integer);  
var
  i: Integer;
  ThreadID: TThreadID;
begin
  ThreadID := GetCurrentThreadId;

  FCS.Enter;
  try
    for i := High(FActivities) downto 0 do
      if (FActivities[i].ThreadID = ThreadID) and
         (FActivities[i].TaskIndex = TaskIndex) and
         (FActivities[i].EndTime = 0) then
      begin
        FActivities[i].EndTime := Now;
        Break;
      end;
  finally
    FCS.Leave;
  end;
end;

procedure TActivityLogger.PrintReport;  
var
  i: Integer;
  Duration: Double;
  TotalDuration, MaxDuration, MinDuration: Double;
begin
  WriteLn('=== Rapport d''activité ===');

  TotalDuration := 0;
  MaxDuration := 0;
  MinDuration := 999999;

  for i := 0 to High(FActivities) do
  begin
    Duration := MilliSecondsBetween(
      FActivities[i].EndTime,
      FActivities[i].StartTime
    );

    WriteLn(Format('Thread %d, Tâche %d: %.2f ms',
      [FActivities[i].ThreadID, FActivities[i].TaskIndex, Duration]));

    TotalDuration := TotalDuration + Duration;
    if Duration > MaxDuration then MaxDuration := Duration;
    if Duration < MinDuration then MinDuration := Duration;
  end;

  WriteLn('---');
  WriteLn('Durée moyenne : ', (TotalDuration / Length(FActivities)):0:2, ' ms');
  WriteLn('Durée min     : ', MinDuration:0:2, ' ms');
  WriteLn('Durée max     : ', MaxDuration:0:2, ' ms');
end;

// Utilisation
var
  Logger: TActivityLogger;
begin
  Logger := TActivityLogger.Create;
  try
    ProcThreadPool.DoParallel(
      procedure(Index: Integer)
      begin
        Logger.LogStart(Index);
        try
          // Traitement avec durée variable
          Sleep(Random(100));
        finally
          Logger.LogEnd(Index);
        end;
      end,
      0, 99
    );

    Logger.PrintReport;
  finally
    Logger.Free;
  end;
end;
```

### Détecter les problèmes de performance

```pascal
uses
  MTProcs;

type
  TPerformanceIssue = (piLoadImbalance, piTooFineGrained, piTooCoarseGrained);

  TPerformanceAnalyzer = class
  private
    FTaskDurations: array of Int64;
    FCS: TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;

    procedure RecordTaskDuration(Duration: Int64);
    function Analyze: TPerformanceIssue;
    procedure PrintSuggestions;
  end;

function TPerformanceAnalyzer.Analyze: TPerformanceIssue;  
var
  i: Integer;
  Sum, Mean, Variance, StdDev: Double;
begin
  // Calculer la moyenne
  Sum := 0;
  for i := 0 to High(FTaskDurations) do
    Sum := Sum + FTaskDurations[i];
  Mean := Sum / Length(FTaskDurations);

  // Calculer l'écart-type
  Variance := 0;
  for i := 0 to High(FTaskDurations) do
    Variance := Variance + Sqr(FTaskDurations[i] - Mean);
  Variance := Variance / Length(FTaskDurations);
  StdDev := Sqrt(Variance);

  // Analyser
  if StdDev / Mean > 0.5 then
    Result := piLoadImbalance
  else if Mean < 1 then
    Result := piTooFineGrained
  else if Mean > 1000 then
    Result := piTooCoarseGrained
  else
    Result := piLoadImbalance; // Par défaut
end;

procedure TPerformanceAnalyzer.PrintSuggestions;  
begin
  case Analyze of
    piLoadImbalance:
      WriteLn('⚠ Déséquilibre de charge détecté. Considérez le work stealing.');
    piTooFineGrained:
      WriteLn('⚠ Tâches trop petites. Augmentez la granularité.');
    piTooCoarseGrained:
      WriteLn('⚠ Tâches trop grandes. Réduisez la granularité.');
  end;
end;
```

## Différences Windows/Ubuntu

MTProcs fonctionne de manière identique sur Windows et Ubuntu, mais il existe des différences de performance.

### Configuration spécifique par OS

```pascal
{$IFDEF WINDOWS}
uses
  Windows;

procedure OptimizeForWindows;  
begin
  // Définir l'affinité processeur
  SetProcessAffinityMask(GetCurrentProcess, $FF); // Tous les cœurs

  // Augmenter la priorité du processus
  SetPriorityClass(GetCurrentProcess, HIGH_PRIORITY_CLASS);
end;
{$ENDIF}

{$IFDEF LINUX}
uses
  BaseUnix, Unix;

procedure OptimizeForLinux;  
begin
  // Définir la politique de scheduling
  // Nice value : -20 (haute priorité) à 19 (basse priorité)
  FpNice(-10);
end;
{$ENDIF}

begin
  {$IFDEF WINDOWS}
  OptimizeForWindows;
  {$ENDIF}

  {$IFDEF LINUX}
  OptimizeForLinux;
  {$ENDIF}

  // Code parallèle...
end.
```

### Différences de performance

**Windows :**
- Thread pool natif optimisé (depuis Vista)
- Context switching rapide
- Bon support NUMA

**Ubuntu/Linux :**
- Scheduler très efficace (CFS)
- Meilleur contrôle des priorités
- Excellent support des cgroups

```pascal
// Benchmark multi-plateforme
procedure BenchmarkPlatform;  
var
  Start: TDateTime;
  i: Integer;
begin
  WriteLn('Plateforme : ');
  {$IFDEF WINDOWS}
  WriteLn('  Windows');
  {$ENDIF}
  {$IFDEF LINUX}
  WriteLn('  Linux');
  {$ENDIF}

  WriteLn('Processeurs : ', ProcThreadPool.MaxThreadCount);

  Start := Now;
  ProcThreadPool.DoParallel(
    procedure(Index: Integer)
    var
      j, Sum: Integer;
    begin
      Sum := 0;
      for j := 1 to 1000000 do
        Sum := Sum + j;
    end,
    0, 99
  );

  WriteLn('Temps : ', MilliSecondsBetween(Now, Start), ' ms');
end;
```

## Alternatives à MTProcs

### 1. OmniThreadLibrary (Delphi, portage partiel FPC)

```pascal
// OmniThreadLibrary offre plus de fonctionnalités
uses
  OtlParallel;

Parallel.ForEach(0, 999)
  .NumTasks(4)
  .Execute(
    procedure(const Value: Integer)
    begin
      ProcessElement(Value);
    end
  );
```

### 2. Implémentation manuelle simple

```pascal
unit SimpleParallel;

{$mode objfpc}{$H+}

interface

type
  TProcedureIndex = procedure(Index: Integer);

  TSimpleParallel = class
  public
    class procedure &For(StartIndex, EndIndex: Integer; Proc: TProcedureIndex);
  end;

implementation

uses
  Classes, Math;

type
  { Thread nommé au lieu de CreateAnonymousThread
    (évite crash WaitFor/Free et bug de closure sur i) }
  TParallelForThread = class(TThread)
  private
    FStartIdx, FEndIdx: Integer;
    FProc: TProcedureIndex;
  protected
    procedure Execute; override;
  public
    constructor Create(AStartIdx, AEndIdx: Integer; AProc: TProcedureIndex);
  end;

constructor TParallelForThread.Create(AStartIdx, AEndIdx: Integer;
  AProc: TProcedureIndex);
begin
  inherited Create(False);
  FStartIdx := AStartIdx;
  FEndIdx := AEndIdx;
  FProc := AProc;
  FreeOnTerminate := False;
end;

procedure TParallelForThread.Execute;  
var
  j: Integer;
begin
  for j := FStartIdx to FEndIdx do
    FProc(j);
end;

class procedure TSimpleParallel.&For(StartIndex, EndIndex: Integer;
  Proc: TProcedureIndex);
var
  ThreadCount, ChunkSize, i: Integer;
  StartIdx, EndIdx: Integer;
  Threads: array of TParallelForThread;
begin
  ThreadCount := TThread.ProcessorCount;
  ChunkSize := (EndIndex - StartIndex + 1 + ThreadCount - 1) div ThreadCount;
  SetLength(Threads, ThreadCount);

  for i := 0 to ThreadCount - 1 do
  begin
    StartIdx := StartIndex + i * ChunkSize;
    EndIdx := Min(StartIdx + ChunkSize - 1, EndIndex);
    Threads[i] := TParallelForThread.Create(StartIdx, EndIdx, Proc);
  end;

  for i := 0 to ThreadCount - 1 do
    Threads[i].WaitFor;

  for i := 0 to ThreadCount - 1 do
    Threads[i].Free;
end;

end.
```

## Bonnes pratiques

### Checklist pour la parallélisation

- [ ] Profiler le code séquentiel d'abord
- [ ] Identifier les hotspots (parties lentes)
- [ ] Vérifier l'absence de dépendances entre itérations
- [ ] Estimer le speedup potentiel (loi d'Amdahl)
- [ ] Choisir la granularité appropriée
- [ ] Minimiser la synchronisation
- [ ] Tester avec différents nombres de threads
- [ ] Mesurer le speedup réel
- [ ] Vérifier l'absence de race conditions
- [ ] Documenter les choix de parallélisation

### Guidelines de code

```pascal
// ✅ BON : Code parallélisable
ProcThreadPool.DoParallel(
  procedure(Index: Integer)
  var
    LocalResult: Double;
  begin
    // Variables locales : pas de problème
    LocalResult := Calculate(Data[Index]);
    Results[Index] := LocalResult; // Écriture indépendante
  end,
  0, High(Data)
);

// ❌ MAUVAIS : Variables partagées sans protection
var
  SharedCounter: Integer;
begin
  SharedCounter := 0;
  ProcThreadPool.DoParallel(
    procedure(Index: Integer)
    begin
      Inc(SharedCounter); // RACE CONDITION !
    end,
    0, 999
  );
end;

// ✅ BON : Utiliser InterlockedIncrement
var
  SharedCounter: Integer;
begin
  SharedCounter := 0;
  ProcThreadPool.DoParallel(
    procedure(Index: Integer)
    begin
      InterlockedIncrement(SharedCounter); // Thread-safe
    end,
    0, 999
  );
end;
```

## Résumé

Les **bibliothèques de programmation parallèle** simplifient grandement le multi-threading :

**Avantages clés :**
- ✅ Abstraction de haut niveau
- ✅ Gestion automatique des threads
- ✅ Optimisations intégrées (work stealing, load balancing)
- ✅ Code plus lisible et maintenable

**MTProcs pour FreePascal :**
- Simple à utiliser
- Léger et efficace
- Portable Windows/Linux
- Idéal pour le parallélisme de données

**Patterns essentiels :**
- **ParallelFor** : Boucles parallèles
- **Map-Reduce** : Transformation et agrégation
- **Fork-Join** : Diviser et fusionner
- **Pipeline** : Traitement par étapes

**Règles d'or :**
1. Profiler avant de paralléliser
2. Choisir la bonne granularité
3. Minimiser la synchronisation
4. Mesurer le speedup obtenu
5. Tester exhaustivement

La programmation parallèle avec MTProcs rend accessible la puissance du multi-cœur sans la complexité de la gestion manuelle des threads !

⏭️ [Async/Await patterns](/11-multithreading-concurrence/06-async-await-patterns.md)
