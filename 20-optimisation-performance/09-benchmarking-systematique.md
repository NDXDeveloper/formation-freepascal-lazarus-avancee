🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 20.9 Benchmarking Systématique

## Introduction

Le **benchmarking** est l'art de **mesurer précisément les performances** de votre code pour identifier les goulots d'étranglement et valider vos optimisations. C'est une compétence essentielle pour tout développeur souhaitant créer des applications performantes.

### Pourquoi faire du benchmarking ?

**Analogie simple :** Imaginez que vous voulez améliorer votre temps sur un parcours de course. Sans chronomètre, comment sauriez-vous si vous progressez ? Le benchmarking, c'est votre chronomètre pour le code.

**Règle d'or :** "On ne peut optimiser que ce qu'on mesure" - Ne jamais optimiser à l'aveugle !

### Les trois piliers du benchmarking

1. **Mesure** : Collecter des données précises sur les performances
2. **Comparaison** : Évaluer différentes approches
3. **Validation** : S'assurer que les optimisations fonctionnent réellement

---

## 1. Mesure du Temps d'Exécution

### 1.1 Méthode Simple avec Now()

La méthode la plus basique pour mesurer le temps :

```pascal
uses
  SysUtils, DateUtils;

procedure BenchmarkSimple;  
var
  StartTime, EndTime: TDateTime;
  i, Sum: Integer;
begin
  StartTime := Now;

  // Code à mesurer
  Sum := 0;
  for i := 1 to 10000000 do
    Sum := Sum + i;

  EndTime := Now;

  WriteLn('Temps d''exécution : ', MilliSecondsBetween(EndTime, StartTime), ' ms');
  WriteLn('Résultat : ', Sum);
end;
```

**Avantages :**
- Très simple à utiliser
- Fonctionne sur Windows et Ubuntu

**Inconvénients :**
- Précision limitée (millisecondes)
- Pas adapté pour les opérations très rapides

### 1.2 Utilisation de GetTickCount64 (Plus Précis)

Pour une meilleure précision, utilisez `GetTickCount64` :

```pascal
{$IFDEF WINDOWS}
uses
  Windows;
{$ELSE}
uses
  Unix, BaseUnix;

function GetTickCount64: QWord;  
var
  ts: TTimeSpec;
begin
  clock_gettime(CLOCK_MONOTONIC, @ts);
  Result := (QWord(ts.tv_sec) * 1000) + (ts.tv_nsec div 1000000);
end;
{$ENDIF}

procedure BenchmarkPrecis;  
var
  StartTicks, EndTicks: QWord;
  i: Integer;
begin
  StartTicks := GetTickCount64;

  // Code à mesurer
  for i := 1 to 1000000 do
    Sqrt(i);

  EndTicks := GetTickCount64;

  WriteLn('Temps : ', EndTicks - StartTicks, ' ms');
end;
```

### 1.3 Haute Précision avec QueryPerformanceCounter (Windows)

Pour des mesures extrêmement précises sur Windows :

```pascal
{$IFDEF WINDOWS}
uses
  Windows;

function GetPreciseTime: Double;  
var
  Frequency, Counter: Int64;
begin
  QueryPerformanceFrequency(Frequency);
  QueryPerformanceCounter(Counter);
  Result := Counter / Frequency; // Temps en secondes
end;
{$ENDIF}

procedure BenchmarkHautePrecision;  
var
  StartTime, EndTime: Double;
begin
  {$IFDEF WINDOWS}
  StartTime := GetPreciseTime;

  // Code à mesurer
  Sleep(100);

  EndTime := GetPreciseTime;

  WriteLn('Temps : ', FormatFloat('0.000000', (EndTime - StartTime) * 1000), ' ms');
  {$ENDIF}
end;
```

### 1.4 Classe Universelle de Chronométrage

Créons une classe réutilisable qui fonctionne sur toutes les plateformes :

```pascal
type
  TStopwatch = class
  private
    FStartTime: QWord;
    FElapsedTime: QWord;
    FIsRunning: Boolean;
    function GetCurrentTicks: QWord;
  public
    constructor Create;
    procedure Start;
    procedure Stop;
    procedure Reset;
    function ElapsedMilliseconds: QWord;
    function ElapsedSeconds: Double;
    property IsRunning: Boolean read FIsRunning;
  end;

constructor TStopwatch.Create;  
begin
  FIsRunning := False;
  FElapsedTime := 0;
end;

function TStopwatch.GetCurrentTicks: QWord;  
begin
  {$IFDEF WINDOWS}
  Result := GetTickCount64;
  {$ELSE}
  var ts: TTimeSpec;
  clock_gettime(CLOCK_MONOTONIC, @ts);
  Result := (QWord(ts.tv_sec) * 1000) + (ts.tv_nsec div 1000000);
  {$ENDIF}
end;

procedure TStopwatch.Start;  
begin
  if not FIsRunning then
  begin
    FStartTime := GetCurrentTicks;
    FIsRunning := True;
  end;
end;

procedure TStopwatch.Stop;  
begin
  if FIsRunning then
  begin
    FElapsedTime := FElapsedTime + (GetCurrentTicks - FStartTime);
    FIsRunning := False;
  end;
end;

procedure TStopwatch.Reset;  
begin
  FElapsedTime := 0;
  FIsRunning := False;
end;

function TStopwatch.ElapsedMilliseconds: QWord;  
begin
  Result := FElapsedTime;
  if FIsRunning then
    Result := Result + (GetCurrentTicks - FStartTime);
end;

function TStopwatch.ElapsedSeconds: Double;  
begin
  Result := ElapsedMilliseconds / 1000.0;
end;
```

**Utilisation :**

```pascal
var
  SW: TStopwatch;
  i: Integer;
begin
  SW := TStopwatch.Create;
  try
    SW.Start;

    // Code à mesurer
    for i := 1 to 1000000 do
      Sqrt(i);

    SW.Stop;

    WriteLn('Temps écoulé : ', SW.ElapsedMilliseconds, ' ms');
    WriteLn('Soit : ', FormatFloat('0.00', SW.ElapsedSeconds), ' secondes');
  finally
    SW.Free;
  end;
end;
```

---

## 2. Framework de Benchmarking Complet

### 2.1 Classe de Benchmark Réutilisable

Créons un système professionnel pour comparer plusieurs implémentations :

```pascal
type
  TBenchmarkProc = procedure;

  TBenchmarkResult = record
    Name: string;
    ExecutionTime: QWord;  // en millisecondes
    Iterations: Integer;
    TimePerIteration: Double;  // en microsecondes
  end;

  TBenchmarkSuite = class
  private
    FResults: array of TBenchmarkResult;
    FIterations: Integer;
    FWarmupRuns: Integer;
  public
    constructor Create(AIterations: Integer = 1000; AWarmupRuns: Integer = 10);
    procedure AddBenchmark(const AName: string; AProc: TBenchmarkProc);
    procedure Run;
    procedure PrintResults;
    procedure PrintComparison;
  end;

constructor TBenchmarkSuite.Create(AIterations, AWarmupRuns: Integer);  
begin
  FIterations := AIterations;
  FWarmupRuns := AWarmupRuns;
  SetLength(FResults, 0);
end;

procedure TBenchmarkSuite.AddBenchmark(const AName: string; AProc: TBenchmarkProc);  
var
  SW: TStopwatch;
  i: Integer;
  Idx: Integer;
begin
  Idx := Length(FResults);
  SetLength(FResults, Idx + 1);

  FResults[Idx].Name := AName;
  FResults[Idx].Iterations := FIterations;

  // Warm-up (important pour éviter les faux résultats)
  WriteLn('Warm-up pour "', AName, '"...');
  for i := 1 to FWarmupRuns do
    AProc();

  // Mesure réelle
  WriteLn('Benchmark de "', AName, '"...');
  SW := TStopwatch.Create;
  try
    SW.Start;
    for i := 1 to FIterations do
      AProc();
    SW.Stop;

    FResults[Idx].ExecutionTime := SW.ElapsedMilliseconds;
    FResults[Idx].TimePerIteration :=
      (SW.ElapsedMilliseconds * 1000.0) / FIterations; // en microsecondes
  finally
    SW.Free;
  end;
end;

procedure TBenchmarkSuite.Run;  
begin
  WriteLn('=== Démarrage de la suite de benchmarks ===');
  WriteLn('Itérations par test : ', FIterations);
  WriteLn('Runs de warm-up : ', FWarmupRuns);
  WriteLn;
end;

procedure TBenchmarkSuite.PrintResults;  
var
  i: Integer;
begin
  WriteLn;
  WriteLn('=== RÉSULTATS ===');
  WriteLn;

  for i := 0 to High(FResults) do
  begin
    WriteLn('Test : ', FResults[i].Name);
    WriteLn('  Temps total   : ', FResults[i].ExecutionTime, ' ms');
    WriteLn('  Par itération : ', FormatFloat('0.000', FResults[i].TimePerIteration), ' μs');
    WriteLn;
  end;
end;

procedure TBenchmarkSuite.PrintComparison;  
var
  i: Integer;
  BestTime: QWord;
  Ratio: Double;
begin
  if Length(FResults) = 0 then Exit;

  // Trouver le meilleur temps
  BestTime := FResults[0].ExecutionTime;
  for i := 1 to High(FResults) do
    if FResults[i].ExecutionTime < BestTime then
      BestTime := FResults[i].ExecutionTime;

  WriteLn('=== COMPARAISON ===');
  WriteLn;

  for i := 0 to High(FResults) do
  begin
    Ratio := FResults[i].ExecutionTime / BestTime;
    WriteLn(FResults[i].Name);
    if FResults[i].ExecutionTime = BestTime then
      WriteLn('  ✓ PLUS RAPIDE (référence)')
    else
      WriteLn('  ', FormatFloat('0.00', Ratio), 'x plus lent');
    WriteLn;
  end;
end;
```

### 2.2 Exemple d'Utilisation Pratique

Comparons différentes méthodes pour calculer la somme d'un tableau :

```pascal
const
  ARRAY_SIZE = 10000;

var
  TestArray: array[0..ARRAY_SIZE-1] of Integer;

procedure InitTestArray;  
var
  i: Integer;
begin
  for i := 0 to ARRAY_SIZE-1 do
    TestArray[i] := i + 1;
end;

procedure SumWithForLoop;  
var
  i, Sum: Integer;
begin
  Sum := 0;
  for i := 0 to ARRAY_SIZE-1 do
    Sum := Sum + TestArray[i];
end;

procedure SumWithWhileLoop;  
var
  i, Sum: Integer;
begin
  Sum := 0;
  i := 0;
  while i < ARRAY_SIZE do
  begin
    Sum := Sum + TestArray[i];
    Inc(i);
  end;
end;

procedure SumWithPointer;  
var
  P: PInteger;
  PEnd: PInteger;
  Sum: Integer;
begin
  Sum := 0;
  P := @TestArray[0];
  PEnd := @TestArray[ARRAY_SIZE];
  while P < PEnd do
  begin
    Sum := Sum + P^;
    Inc(P);
  end;
end;

procedure RunSumBenchmark;  
var
  Suite: TBenchmarkSuite;
begin
  InitTestArray;

  Suite := TBenchmarkSuite.Create(10000, 100);
  try
    Suite.Run;

    Suite.AddBenchmark('For Loop', @SumWithForLoop);
    Suite.AddBenchmark('While Loop', @SumWithWhileLoop);
    Suite.AddBenchmark('Pointer Arithmetic', @SumWithPointer);

    Suite.PrintResults;
    Suite.PrintComparison;
  finally
    Suite.Free;
  end;
end;
```

---

## 3. Benchmarking de Structures de Données

Comparons les performances de différentes structures :

```pascal
type
  TDataStructureBenchmark = class
  private
    FItemCount: Integer;
  public
    constructor Create(AItemCount: Integer = 10000);
    procedure BenchmarkTList;
    procedure BenchmarkTStringList;
    procedure BenchmarkTDictionary;
    procedure BenchmarkArray;
    procedure RunAll;
  end;

constructor TDataStructureBenchmark.Create(AItemCount: Integer);  
begin
  FItemCount := AItemCount;
end;

procedure TDataStructureBenchmark.BenchmarkTList;  
var
  List: TList;
  i: Integer;
  SW: TStopwatch;
begin
  SW := TStopwatch.Create;
  List := TList.Create;
  try
    // Insertion
    SW.Start;
    for i := 0 to FItemCount-1 do
      List.Add(Pointer(i));
    SW.Stop;
    WriteLn('TList - Insertion : ', SW.ElapsedMilliseconds, ' ms');

    // Recherche
    SW.Reset;
    SW.Start;
    for i := 0 to FItemCount-1 do
      List.IndexOf(Pointer(i div 2));
    SW.Stop;
    WriteLn('TList - Recherche : ', SW.ElapsedMilliseconds, ' ms');

  finally
    List.Free;
    SW.Free;
  end;
end;

procedure TDataStructureBenchmark.BenchmarkTDictionary;  
var
  Dict: TDictionary<Integer, Integer>;
  i, Val: Integer;
  SW: TStopwatch;
begin
  SW := TStopwatch.Create;
  Dict := TDictionary<Integer, Integer>.Create;
  try
    // Insertion
    SW.Start;
    for i := 0 to FItemCount-1 do
      Dict.Add(i, i * 2);
    SW.Stop;
    WriteLn('TDictionary - Insertion : ', SW.ElapsedMilliseconds, ' ms');

    // Recherche
    SW.Reset;
    SW.Start;
    for i := 0 to FItemCount-1 do
      Dict.TryGetValue(i div 2, Val);
    SW.Stop;
    WriteLn('TDictionary - Recherche : ', SW.ElapsedMilliseconds, ' ms');

  finally
    Dict.Free;
    SW.Free;
  end;
end;

procedure TDataStructureBenchmark.RunAll;  
begin
  WriteLn('=== Benchmark des structures de données ===');
  WriteLn('Nombre d''éléments : ', FItemCount);
  WriteLn;

  BenchmarkArray;
  WriteLn;
  BenchmarkTList;
  WriteLn;
  BenchmarkTStringList;
  WriteLn;
  BenchmarkTDictionary;
end;
```

---

## 4. Profiling Mémoire

### 4.1 Mesure de la Consommation Mémoire

Sur Windows et Ubuntu, on peut surveiller la mémoire utilisée :

```pascal
uses
  {$IFDEF WINDOWS}
  Windows,
  {$ELSE}
  Unix, BaseUnix,
  {$ENDIF}
  SysUtils;

function GetMemoryUsage: Int64;
{$IFDEF WINDOWS}
var
  MemCounters: TProcessMemoryCounters;
{$ENDIF}
begin
  {$IFDEF WINDOWS}
  MemCounters.cb := SizeOf(MemCounters);
  if GetProcessMemoryInfo(GetCurrentProcess, @MemCounters, SizeOf(MemCounters)) then
    Result := MemCounters.WorkingSetSize
  else
    Result := 0;
  {$ELSE}
  // Sur Linux, lecture depuis /proc/self/status
  // Implémentation simplifiée
  Result := 0; // À implémenter selon vos besoins
  {$ENDIF}
end;

procedure BenchmarkMemory;  
var
  MemBefore, MemAfter: Int64;
  List: TList;
  i: Integer;
begin
  MemBefore := GetMemoryUsage;

  // Allocation mémoire
  List := TList.Create;
  try
    for i := 0 to 1000000 do
      List.Add(Pointer(i));

    MemAfter := GetMemoryUsage;

    WriteLn('Mémoire avant : ', MemBefore div 1024, ' KB');
    WriteLn('Mémoire après : ', MemAfter div 1024, ' KB');
    WriteLn('Différence : ', (MemAfter - MemBefore) div 1024, ' KB');
  finally
    List.Free;
  end;
end;
```

### 4.2 Détection de Fuites Mémoire avec HeapTrc

FreePascal inclut un excellent outil de détection de fuites :

```pascal
{$IFDEF DEBUG}
  {$APPTYPE CONSOLE}
  {$DEFINE HEAPTRC}
{$ENDIF}

program MemoryLeakTest;

uses
  {$IFDEF HEAPTRC}
  HeapTrc,
  {$ENDIF}
  Classes, SysUtils;

var
  List: TStringList;
begin
  // Configurer HeapTrc
  {$IFDEF HEAPTRC}
  SetHeapTraceOutput('heap.trc');
  {$ENDIF}

  // Code avec fuite volontaire
  List := TStringList.Create;
  List.Add('Test');
  // Oubli volontaire du Free !

  WriteLn('Fin du programme');
  // HeapTrc affichera un rapport de fuite
end.
```

Le fichier `heap.trc` contiendra un rapport détaillé des fuites.

---

## 5. Benchmarking Multi-plateforme

### 5.1 Considérations Windows vs Ubuntu

Les performances peuvent varier entre les plateformes :

```pascal
procedure BenchmarkPlatformDifferences;  
var
  SW: TStopwatch;
  i: Integer;
  F: TextFile;
begin
  WriteLn('Plateforme : ');
  {$IFDEF WINDOWS}
  WriteLn('  Windows');
  {$ENDIF}
  {$IFDEF UNIX}
  WriteLn('  Unix/Linux');
  {$ENDIF}
  WriteLn('Architecture : ', SizeOf(Pointer) * 8, ' bits');
  WriteLn;

  SW := TStopwatch.Create;
  try
    // Test I/O fichier
    SW.Start;
    AssignFile(F, 'test.txt');
    Rewrite(F);
    for i := 1 to 10000 do
      WriteLn(F, 'Ligne ', i);
    CloseFile(F);
    SW.Stop;

    WriteLn('Écriture fichier : ', SW.ElapsedMilliseconds, ' ms');

    // Nettoyage
    DeleteFile('test.txt');
  finally
    SW.Free;
  end;
end;
```

### 5.2 Script de Benchmark Automatisé

Créez un script pour lancer les benchmarks sur les deux OS :

**Windows (benchmark.bat) :**
```batch
@echo off
echo === Benchmarks Windows ===  
fpc -O3 benchmark.pas  
benchmark.exe > results_windows.txt  
echo Résultats sauvegardés dans results_windows.txt
```

**Ubuntu (benchmark.sh) :**
```bash
#!/bin/bash
echo "=== Benchmarks Ubuntu ==="  
fpc -O3 benchmark.pas
./benchmark > results_ubuntu.txt
echo "Résultats sauvegardés dans results_ubuntu.txt"
```

---

## 6. Analyse et Visualisation des Résultats

### 6.1 Export des Résultats en CSV

```pascal
type
  TBenchmarkExporter = class
  public
    class procedure ExportToCSV(const Results: array of TBenchmarkResult;
                                const FileName: string);
  end;

class procedure TBenchmarkExporter.ExportToCSV(
  const Results: array of TBenchmarkResult; const FileName: string);
var
  F: TextFile;
  i: Integer;
begin
  AssignFile(F, FileName);
  Rewrite(F);
  try
    // En-tête
    WriteLn(F, 'Test,Temps Total (ms),Iterations,Temps/Iteration (μs)');

    // Données
    for i := 0 to High(Results) do
      WriteLn(F,
        Results[i].Name, ',',
        Results[i].ExecutionTime, ',',
        Results[i].Iterations, ',',
        FormatFloat('0.000', Results[i].TimePerIteration)
      );

    WriteLn('Résultats exportés vers : ', FileName);
  finally
    CloseFile(F);
  end;
end;
```

### 6.2 Génération de Graphiques avec TAChart

```pascal
uses
  TAGraph, TASeries, TAChartUtils;

procedure GenerateBenchmarkChart(const Results: array of TBenchmarkResult);  
var
  Chart: TChart;
  Series: TBarSeries;
  i: Integer;
begin
  Chart := TChart.Create(nil);
  Series := TBarSeries.Create(Chart);
  try
    Series.Title := 'Temps d''exécution';

    for i := 0 to High(Results) do
      Series.AddXY(i, Results[i].ExecutionTime, Results[i].Name);

    Chart.AddSeries(Series);
    Chart.Title.Text.Text := 'Comparaison des performances';

    // Sauvegarder le graphique
    Chart.SaveToFile(TPortableNetworkGraphic, 'benchmark_chart.png');
    WriteLn('Graphique sauvegardé : benchmark_chart.png');
  finally
    Chart.Free;
  end;
end;
```

---

## 7. Bonnes Pratiques de Benchmarking

### ✅ À Faire

1. **Toujours faire un warm-up** : Les premières exécutions sont souvent plus lentes (cache CPU, JIT, etc.)

2. **Exécuter plusieurs itérations** : Une seule mesure n'est pas fiable

3. **Mesurer dans des conditions réelles** : Mode Release, optimisations activées

```pascal
// Compiler avec optimisations
// fpc -O3 -CX -XX benchmark.pas
```

4. **Isoler le code à mesurer** : Ne mesurez que ce qui vous intéresse

5. **Désactiver les autres processus** : Fermer les applications gourmandes pendant les tests

6. **Comparer des choses comparables** : Même quantité de travail, même complexité

7. **Documenter l'environnement** :
```pascal
procedure PrintEnvironment;  
begin
  WriteLn('=== Environnement ===');
  WriteLn('OS : ', {$I %FPCTARGETOS%});
  WriteLn('CPU : ', {$I %FPCTARGETCPU%});
  WriteLn('Compilateur : FPC ', {$I %FPCVERSION%});
  WriteLn('Date : ', DateTimeToStr(Now));
  WriteLn;
end;
```

### ❌ À Éviter

1. **Optimiser prématurément** : Mesurez d'abord, optimisez ensuite
2. **Ignorer le warm-up** : Fausses conclusions
3. **Mesurer en mode Debug** : Les résultats ne sont pas représentatifs
4. **Utiliser WriteLn dans le code mesuré** : Les I/O sont très lentes
5. **Oublier de compiler avec -O3** : Sans optimisations, tout est lent

---

## 8. Exemple Complet : Comparaison d'Algorithmes de Tri

```pascal
program SortBenchmark;

uses
  SysUtils, Classes;

const
  ARRAY_SIZE = 10000;

type
  TIntArray = array of Integer;

var
  TestArray: TIntArray;

procedure GenerateRandomArray(var Arr: TIntArray; Size: Integer);  
var
  i: Integer;
begin
  SetLength(Arr, Size);
  Randomize;
  for i := 0 to Size-1 do
    Arr[i] := Random(100000);
end;

procedure BubbleSort(var Arr: TIntArray);  
var
  i, j, Temp: Integer;
begin
  for i := 0 to High(Arr) do
    for j := 0 to High(Arr) - i - 1 do
      if Arr[j] > Arr[j+1] then
      begin
        Temp := Arr[j];
        Arr[j] := Arr[j+1];
        Arr[j+1] := Temp;
      end;
end;

procedure QuickSort(var Arr: TIntArray; Left, Right: Integer);  
var
  i, j, Pivot, Temp: Integer;
begin
  if Left < Right then
  begin
    Pivot := Arr[(Left + Right) div 2];
    i := Left;
    j := Right;
    while i <= j do
    begin
      while Arr[i] < Pivot do Inc(i);
      while Arr[j] > Pivot do Dec(j);
      if i <= j then
      begin
        Temp := Arr[i];
        Arr[i] := Arr[j];
        Arr[j] := Temp;
        Inc(i);
        Dec(j);
      end;
    end;
    QuickSort(Arr, Left, j);
    QuickSort(Arr, i, Right);
  end;
end;

procedure RunSortBenchmarks;  
var
  Suite: TBenchmarkSuite;
  TempArray: TIntArray;
begin
  GenerateRandomArray(TestArray, ARRAY_SIZE);

  Suite := TBenchmarkSuite.Create(10, 2);
  try
    Suite.Run;

    // Bubble Sort
    Suite.AddBenchmark('Bubble Sort',
      procedure
      begin
        TempArray := Copy(TestArray);
        BubbleSort(TempArray);
      end
    );

    // Quick Sort
    Suite.AddBenchmark('Quick Sort',
      procedure
      begin
        TempArray := Copy(TestArray);
        QuickSort(TempArray, 0, High(TempArray));
      end
    );

    Suite.PrintResults;
    Suite.PrintComparison;
  finally
    Suite.Free;
  end;
end;

begin
  PrintEnvironment;
  RunSortBenchmarks;
  ReadLn;
end.
```

---

## 9. Outils Externes de Profiling

### 9.1 Sur Windows

**Intel VTune Profiler** (gratuit pour un usage personnel)
- Profiling CPU détaillé
- Analyse des hotspots
- Visualisation graphique

**Visual Studio Profiler**
- Inclus avec Visual Studio Community
- Peut profiler les exécutables FreePascal

### 9.2 Sur Ubuntu/Linux

**Valgrind Callgrind**
```bash
# Compiler avec symboles de debug
fpc -g benchmark.pas

# Profiler avec Callgrind
valgrind --tool=callgrind ./benchmark

# Visualiser avec KCacheGrind
kcachegrind callgrind.out.*
```

**perf** (outil Linux natif)
```bash
# Enregistrer un profil
perf record -g ./benchmark

# Analyser les résultats
perf report
```

**gprof** (GNU Profiler)
```bash
# Compiler avec support gprof
fpc -pg benchmark.pas

# Exécuter
./benchmark

# Analyser
gprof benchmark gmon.out > analysis.txt
```

---

## 10. Intégration Continue des Benchmarks

### 10.1 Script de Benchmark Automatique

```bash
#!/bin/bash
# benchmark_ci.sh

echo "=== Benchmark CI ==="  
echo "Date: $(date)"  
echo "Commit: $(git rev-parse --short HEAD)"  
echo ""

# Compiler
fpc -O3 benchmark.pas

# Exécuter
./benchmark > benchmark_results.txt

# Comparer avec la baseline
if [ -f "baseline_results.txt" ]; then
    echo "Comparaison avec la baseline..."
    # Script de comparaison personnalisé
    python3 compare_benchmarks.py baseline_results.txt benchmark_results.txt
fi

# Archiver les résultats
mkdir -p benchmark_history  
cp benchmark_results.txt "benchmark_history/bench_$(date +%Y%m%d_%H%M%S).txt"
```

### 10.2 GitHub Actions pour Benchmarks Multi-OS

```yaml
name: Benchmarks

on: [push, pull_request]

jobs:
  benchmark:
    strategy:
      matrix:
        os: [ubuntu-latest, windows-latest]

    runs-on: ${{ matrix.os }}

    steps:
      - uses: actions/checkout@v2

      - name: Install FreePascal
        run: |
          # Installation selon l'OS

      - name: Run Benchmarks
        run: |
          fpc -O3 benchmark.pas
          ./benchmark

      - name: Upload Results
        uses: actions/upload-artifact@v2
        with:
          name: benchmark-results-${{ matrix.os }}
          path: benchmark_results.txt
```

---

## Conclusion

Le benchmarking systématique est **essentiel** pour développer des applications performantes :

🎯 **Principes clés :**
- Toujours mesurer avant d'optimiser
- Comparer plusieurs approches
- Documenter l'environnement de test
- Automatiser les benchmarks

📊 **Outils essentiels :**
- TStopwatch pour mesures simples
- TBenchmarkSuite pour comparaisons
- HeapTrc pour la mémoire
- Valgrind/perf sur Linux
- Profilers pour analyse approfondie

🔄 **Workflow recommandé :**
1. **Identifier** le code lent avec un profiler
2. **Mesurer** les performances actuelles
3. **Optimiser** une seule chose à la fois
4. **Re-mesurer** pour valider l'amélioration
5. **Documenter** les résultats

💡 **Rappelez-vous :**
> "Premature optimization is the root of all evil" - Donald Knuth

Ne perdez pas de temps à optimiser du code qui n'est pas un goulot d'étranglement. Mesurez d'abord, optimisez ensuite !

---

## Annexe A : Template de Rapport de Benchmark

```pascal
procedure GenerateBenchmarkReport(const Results: array of TBenchmarkResult);  
var
  F: TextFile;
  i: Integer;
begin
  AssignFile(F, 'benchmark_report.txt');
  Rewrite(F);
  try
    WriteLn(F, '╔════════════════════════════════════════════════════════╗');
    WriteLn(F, '║          RAPPORT DE BENCHMARK                          ║');
    WriteLn(F, '╚════════════════════════════════════════════════════════╝');
    WriteLn(F);

    WriteLn(F, 'Date        : ', DateTimeToStr(Now));
    WriteLn(F, 'Compilateur : FPC ', {$I %FPCVERSION%});
    WriteLn(F, 'Plateforme  : ', {$I %FPCTARGETOS%}, ' - ', {$I %FPCTARGETCPU%});
    WriteLn(F, 'Optimisation: -O3 -CX -XX');
    WriteLn(F);
    WriteLn(F, '─────────────────────────────────────────────────────────');
    WriteLn(F);

    for i := 0 to High(Results) do
    begin
      WriteLn(F, 'Test #', i+1, ' : ', Results[i].Name);
      WriteLn(F, '  • Itérations    : ', Results[i].Iterations);
      WriteLn(F, '  • Temps total   : ', Results[i].ExecutionTime, ' ms');
      WriteLn(F, '  • Temps/iter    : ',
              FormatFloat('0.000', Results[i].TimePerIteration), ' μs');
      WriteLn(F);
    end;

    WriteLn(F, '─────────────────────────────────────────────────────────');
    WriteLn(F, 'Rapport généré automatiquement');

  finally
    CloseFile(F);
  end;

  WriteLn('Rapport sauvegardé : benchmark_report.txt');
end;
```

---

## Annexe B : Checklist du Benchmarking

### Avant de commencer

- [ ] J'ai identifié le code à optimiser (profiling préalable)
- [ ] J'ai défini des métriques claires (temps, mémoire, throughput)
- [ ] J'ai fermé les applications inutiles
- [ ] J'ai désactivé les économies d'énergie

### Configuration du benchmark

- [ ] Mode Release activé
- [ ] Optimisations du compilateur (-O3)
- [ ] Warm-up configuré (au moins 10 itérations)
- [ ] Nombre d'itérations suffisant (>1000 pour petites opérations)
- [ ] Environnement documenté (OS, CPU, RAM, compilateur)

### Pendant l'exécution

- [ ] Résultats cohérents entre les runs
- [ ] Pas de variations anormales
- [ ] Statistiques collectées (min, max, moyenne, écart-type)

### Après les benchmarks

- [ ] Résultats exportés (CSV, JSON)
- [ ] Graphiques générés
- [ ] Comparaison avec baseline
- [ ] Rapport rédigé
- [ ] Code commité avec les résultats

---

## Annexe C : Statistiques Avancées

Pour des benchmarks plus précis, calculez des statistiques :

```pascal
type
  TBenchmarkStats = record
    Min: QWord;
    Max: QWord;
    Mean: Double;
    Median: QWord;
    StdDev: Double;
  end;

function CalculateStats(const Timings: array of QWord): TBenchmarkStats;  
var
  i, j: Integer;
  Sum, SumSquares: Double;
  Temp: QWord;
  SortedTimings: array of QWord;
begin
  if Length(Timings) = 0 then Exit;

  // Min et Max
  Result.Min := Timings[0];
  Result.Max := Timings[0];
  Sum := 0;

  for i := 0 to High(Timings) do
  begin
    if Timings[i] < Result.Min then Result.Min := Timings[i];
    if Timings[i] > Result.Max then Result.Max := Timings[i];
    Sum := Sum + Timings[i];
  end;

  // Moyenne
  Result.Mean := Sum / Length(Timings);

  // Écart-type
  SumSquares := 0;
  for i := 0 to High(Timings) do
    SumSquares := SumSquares + Sqr(Timings[i] - Result.Mean);
  Result.StdDev := Sqrt(SumSquares / Length(Timings));

  // Médiane
  SortedTimings := Copy(Timings);
  // Tri simple (bubble sort suffisant pour benchmarks)
  for i := 0 to High(SortedTimings) do
    for j := 0 to High(SortedTimings) - i - 1 do
      if SortedTimings[j] > SortedTimings[j+1] then
      begin
        Temp := SortedTimings[j];
        SortedTimings[j] := SortedTimings[j+1];
        SortedTimings[j+1] := Temp;
      end;

  Result.Median := SortedTimings[Length(SortedTimings) div 2];
end;

procedure RunBenchmarkWithStats(const Name: string; Proc: TBenchmarkProc;
                                Runs: Integer);
var
  Timings: array of QWord;
  Stats: TBenchmarkStats;
  SW: TStopwatch;
  i: Integer;
  CV: Double;
begin
  SetLength(Timings, Runs);
  SW := TStopwatch.Create;

  try
    WriteLn('Benchmark : ', Name);
    WriteLn('Runs : ', Runs);

    for i := 0 to Runs-1 do
    begin
      SW.Reset;
      SW.Start;
      Proc();
      SW.Stop;
      Timings[i] := SW.ElapsedMilliseconds;
    end;

    Stats := CalculateStats(Timings);

    WriteLn('Résultats :');
    WriteLn('  Min     : ', Stats.Min, ' ms');
    WriteLn('  Max     : ', Stats.Max, ' ms');
    WriteLn('  Moyenne : ', FormatFloat('0.00', Stats.Mean), ' ms');
    WriteLn('  Médiane : ', Stats.Median, ' ms');
    WriteLn('  Écart-σ : ', FormatFloat('0.00', Stats.StdDev), ' ms');

    // Coefficient de variation (stabilité)
    CV := (Stats.StdDev / Stats.Mean) * 100;
    WriteLn('  CV      : ', FormatFloat('0.00', CV), ' %');

    if CV < 5 then
      WriteLn('  ✓ Résultats très stables')
    else if CV < 15 then
      WriteLn('  ⚠ Résultats modérément stables')
    else
      WriteLn('  ✗ Résultats instables - augmenter le nombre de runs');

  finally
    SW.Free;
  end;
end;
```

---

## Annexe D : Benchmarking Réseau et I/O

Pour les opérations réseau et I/O, mesurez également le throughput :

```pascal
type
  TNetworkBenchmark = class
  public
    class procedure BenchmarkFileIO(const FileName: string; SizeMB: Integer);
    class procedure BenchmarkNetworkSpeed(const URL: string);
  end;

class procedure TNetworkBenchmark.BenchmarkFileIO(const FileName: string;
                                                   SizeMB: Integer);
var
  F: File;
  Buffer: array[0..1023] of Byte;
  SW: TStopwatch;
  i, Iterations: Integer;
  ThroughputMBps: Double;
begin
  Iterations := SizeMB * 1024; // 1 KB par itération

  SW := TStopwatch.Create;
  try
    // Écriture
    AssignFile(F, FileName);
    Rewrite(F, 1);

    SW.Start;
    for i := 0 to Iterations-1 do
      BlockWrite(F, Buffer, SizeOf(Buffer));
    SW.Stop;

    CloseFile(F);

    ThroughputMBps := (SizeMB * 1000.0) / SW.ElapsedMilliseconds;

    WriteLn('Écriture fichier :');
    WriteLn('  Taille     : ', SizeMB, ' MB');
    WriteLn('  Temps      : ', SW.ElapsedMilliseconds, ' ms');
    WriteLn('  Throughput : ', FormatFloat('0.00', ThroughputMBps), ' MB/s');

    // Lecture
    SW.Reset;
    Reset(F, 1);

    SW.Start;
    for i := 0 to Iterations-1 do
      BlockRead(F, Buffer, SizeOf(Buffer));
    SW.Stop;

    CloseFile(F);

    ThroughputMBps := (SizeMB * 1000.0) / SW.ElapsedMilliseconds;

    WriteLn('Lecture fichier :');
    WriteLn('  Taille     : ', SizeMB, ' MB');
    WriteLn('  Temps      : ', SW.ElapsedMilliseconds, ' ms');
    WriteLn('  Throughput : ', FormatFloat('0.00', ThroughputMBps), ' MB/s');

    // Nettoyage
    DeleteFile(FileName);

  finally
    SW.Free;
  end;
end;
```

---

## Annexe E : Comparaison Windows vs Ubuntu

Exemple de code qui compare automatiquement les performances sur les deux OS :

```pascal
program CrossPlatformBenchmark;

{$mode objfpc}{$H+}

uses
  SysUtils, DateUtils
  {$IFDEF WINDOWS}, Windows{$ENDIF}
  {$IFDEF UNIX}, Unix, BaseUnix{$ENDIF};

procedure PrintPlatformInfo;  
begin
  WriteLn('╔════════════════════════════════════════════════════════╗');
  WriteLn('║     BENCHMARK MULTI-PLATEFORME                         ║');
  WriteLn('╚════════════════════════════════════════════════════════╝');
  WriteLn;

  {$IFDEF WINDOWS}
  WriteLn('Plateforme    : Windows');
  {$ENDIF}
  {$IFDEF UNIX}
  WriteLn('Plateforme    : Unix/Linux');
  {$ENDIF}

  WriteLn('Architecture  : ', {$I %FPCTARGETCPU%});
  WriteLn('Bits          : ', SizeOf(Pointer) * 8);
  WriteLn('Compilateur   : FPC ', {$I %FPCVERSION%});
  WriteLn('Date          : ', DateTimeToStr(Now));
  WriteLn;
end;

procedure BenchmarkStringOperations;  
var
  SW: TStopwatch;
  i: Integer;
  S: string;
begin
  SW := TStopwatch.Create;
  try
    WriteLn('Test : Concaténation de chaînes');

    SW.Start;
    S := '';
    for i := 1 to 10000 do
      S := S + 'X';
    SW.Stop;

    WriteLn('  Temps : ', SW.ElapsedMilliseconds, ' ms');
  finally
    SW.Free;
  end;
end;

procedure BenchmarkMathOperations;  
var
  SW: TStopwatch;
  i: Integer;
  Sum: Double;
begin
  SW := TStopwatch.Create;
  try
    WriteLn('Test : Opérations mathématiques');

    SW.Start;
    Sum := 0;
    for i := 1 to 1000000 do
      Sum := Sum + Sqrt(i) * Sin(i);
    SW.Stop;

    WriteLn('  Temps : ', SW.ElapsedMilliseconds, ' ms');
    WriteLn('  Total : ', FormatFloat('0.00', Sum));
  finally
    SW.Free;
  end;
end;

begin
  PrintPlatformInfo;
  BenchmarkStringOperations;
  WriteLn;
  BenchmarkMathOperations;
  WriteLn;
  WriteLn('Benchmark terminé.');

  {$IFDEF WINDOWS}
  WriteLn('Appuyez sur Entrée...');
  ReadLn;
  {$ENDIF}
end.
```

---

## Ressources Complémentaires

### Documentation officielle
- **FreePascal Wiki** : https://wiki.freepascal.org/Profiling
- **Lazarus Wiki** : https://wiki.lazarus.freepascal.org/Performance

### Outils recommandés

**Windows :**
- Intel VTune Profiler
- AMD μProf
- Very Sleepy (profiler simple et gratuit)

**Ubuntu/Linux :**
- Valgrind (callgrind, cachegrind, massif)
- perf (Linux Performance Events)
- gperftools (Google Performance Tools)

### Livres et articles
- "Systems Performance" - Brendan Gregg
- "The Art of Computer Programming" - Donald Knuth (Vol. 1, Chapitre 1.3)

---

## Points Clés à Retenir

✨ **Le benchmarking est une science** qui nécessite rigueur et méthodologie

🔬 **Mesurer, pas deviner** : Les intuitions sont souvent fausses

📈 **Automatiser** : Intégrer les benchmarks dans votre CI/CD

🎯 **Cibler** : Optimiser là où ça compte vraiment (loi de Pareto)

⚖️ **Équilibrer** : Performance vs lisibilité vs maintenabilité

🌍 **Tester sur les deux OS** : Les performances peuvent varier significativement

📊 **Documenter** : Vos résultats sont précieux pour l'équipe

---

**Note finale :** Le benchmarking n'est pas une activité ponctuelle, mais un processus continu. Intégrez-le dans votre workflow de développement pour créer des applications FreePascal/Lazarus rapides et efficaces sur Windows et Ubuntu !

**Prochaines étapes :**
- Identifier les parties critiques de votre application
- Mettre en place un framework de benchmarking
- Créer une baseline de performance
- Optimiser progressivement
- Mesurer et documenter les améliorations

**Bonne optimisation ! 🚀**

⏭️ [Optimisation pour différentes architectures](/20-optimisation-performance/10-optimisation-differentes-architectures.md)
