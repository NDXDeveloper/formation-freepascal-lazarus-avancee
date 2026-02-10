🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 18.6 Tests de performance et benchmarking

## Introduction

Le **benchmarking** (ou test de performance) consiste à mesurer les performances d'un programme ou d'une partie de code pour :

- Identifier les parties lentes (goulots d'étranglement)
- Comparer différentes implémentations d'un algorithme
- Vérifier que les optimisations améliorent réellement les performances
- S'assurer que le programme respecte les exigences de rapidité

**Analogie simple :** C'est comme chronométrer un coureur pour savoir s'il est assez rapide, identifier où il perd du temps, et vérifier si son nouvel entraînement l'a rendu plus performant.

## Pourquoi faire des tests de performance ?

### Raisons principales

1. **Détecter les ralentissements** : Trouver ce qui prend du temps
2. **Optimiser intelligemment** : Se concentrer sur ce qui compte vraiment
3. **Éviter les régressions** : S'assurer que les modifications n'ont pas ralenti le programme
4. **Respecter les exigences** : Garantir un temps de réponse acceptable
5. **Comparer les solutions** : Choisir la meilleure implémentation

### La règle des 80/20

**Principe de Pareto appliqué à la performance :**
- 80% du temps d'exécution est passé dans 20% du code
- Il faut donc identifier et optimiser ces 20% critiques

## Mesure du temps en FreePascal

### 1. Utilisation de `Now` (précision : millisecondes)

La fonction `Now` retourne la date et l'heure actuelles.

```pascal
program SimpleTimer;

uses
  SysUtils, DateUtils;

var
  debut, fin: TDateTime;
  i, somme: Integer;

begin
  debut := Now;

  // Code à mesurer
  somme := 0;
  for i := 1 to 1000000 do
    somme := somme + i;

  fin := Now;

  WriteLn('Temps écoulé: ', MilliSecondsBetween(fin, debut), ' ms');
  WriteLn('Résultat: ', somme);
end.
```

**Avantages :**
- Simple à utiliser
- Disponible partout

**Inconvénients :**
- Précision limitée (millisecondes)
- Peut être affecté par les changements d'heure système

### 2. Utilisation de `GetTickCount64` (précision : millisecondes)

Mesure le temps écoulé depuis le démarrage du système.

```pascal
program TickTimer;

uses
  SysUtils;

var
  debut, fin: QWord;
  i: Integer;

begin
  debut := GetTickCount64;

  // Code à mesurer
  for i := 1 to 10000000 do
    ; // Boucle vide

  fin := GetTickCount64;

  WriteLn('Temps écoulé: ', fin - debut, ' ms');
end.
```

**Avantages :**
- Monotone (ne recule jamais)
- Multi-plateforme (Windows et Linux)
- Simple

**Inconvénients :**
- Précision limitée à la milliseconde

### 3. Utilisation de `GetTickCount` (Windows uniquement)

Version 32 bits, attention au débordement après ~49 jours.

```pascal
{$IFDEF WINDOWS}
var
  debut, fin: LongWord;
begin
  debut := GetTickCount;
  // Code à mesurer
  fin := GetTickCount;
  WriteLn('Temps: ', fin - debut, ' ms');
end;
{$ENDIF}
```

**À éviter** : Préférer `GetTickCount64` qui ne déborde pas.

### 4. Haute précision avec `QueryPerformanceCounter` (Windows)

Pour des mesures très précises (microsecondes).

```pascal
program HighPrecisionWindows;

{$IFDEF WINDOWS}
uses
  Windows, SysUtils;

var
  freq, debut, fin: Int64;
  tempsMs: Double;

begin
  // Obtenir la fréquence du compteur haute résolution
  QueryPerformanceFrequency(freq);

  // Début de la mesure
  QueryPerformanceCounter(debut);

  // Code à mesurer
  Sleep(100);  // Attendre 100ms pour tester

  // Fin de la mesure
  QueryPerformanceCounter(fin);

  // Calculer le temps en millisecondes
  tempsMs := ((fin - debut) * 1000.0) / freq;

  WriteLn('Temps écoulé: ', tempsMs:0:3, ' ms');
end.
{$ENDIF}
```

### 5. Haute précision sous Linux avec `clock_gettime`

Équivalent Linux de `QueryPerformanceCounter`.

```pascal
program HighPrecisionLinux;

{$IFDEF UNIX}
uses
  BaseUnix, Unix, SysUtils;

type
  TTimeSpec = record
    tv_sec: clong;   // Secondes
    tv_nsec: clong;  // Nanosecondes
  end;

function clock_gettime(clk_id: Integer; var tp: TTimeSpec): Integer; cdecl; external 'c' name 'clock_gettime';

const
  CLOCK_MONOTONIC = 1;

var
  debut, fin: TTimeSpec;
  tempsNs: Int64;
  tempsMs: Double;

begin
  // Début de la mesure
  clock_gettime(CLOCK_MONOTONIC, debut);

  // Code à mesurer
  Sleep(100);  // Attendre 100ms pour tester

  // Fin de la mesure
  clock_gettime(CLOCK_MONOTONIC, fin);

  // Calculer le temps en nanosecondes puis en millisecondes
  tempsNs := (fin.tv_sec - debut.tv_sec) * 1000000000 + (fin.tv_nsec - debut.tv_nsec);
  tempsMs := tempsNs / 1000000.0;

  WriteLn('Temps écoulé: ', tempsMs:0:3, ' ms');
end.
{$ENDIF}
```

### 6. Classe utilitaire multi-plateforme

Pour simplifier, créons une classe qui fonctionne sur Windows et Linux.

```pascal
unit BenchmarkUnit;

{$mode objfpc}{$H+}

interface

type
  TBenchmark = class
  private
    {$IFDEF WINDOWS}
    FFrequency: Int64;
    FStartTime: Int64;
    {$ELSE}
    FStartTime: QWord;
    {$ENDIF}
  public
    constructor Create;
    procedure Start;
    function Stop: Double; // Retourne le temps en millisecondes
  end;

implementation

uses
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  SysUtils;

constructor TBenchmark.Create;  
begin
  inherited Create;
  {$IFDEF WINDOWS}
  QueryPerformanceFrequency(FFrequency);
  {$ENDIF}
end;

procedure TBenchmark.Start;  
begin
  {$IFDEF WINDOWS}
  QueryPerformanceCounter(FStartTime);
  {$ELSE}
  FStartTime := GetTickCount64;
  {$ENDIF}
end;

function TBenchmark.Stop: Double;
{$IFDEF WINDOWS}
var
  endTime: Int64;
{$ELSE}
var
  endTime: QWord;
{$ENDIF}
begin
  {$IFDEF WINDOWS}
  QueryPerformanceCounter(endTime);
  Result := ((endTime - FStartTime) * 1000.0) / FFrequency;
  {$ELSE}
  endTime := GetTickCount64;
  Result := endTime - FStartTime;
  {$ENDIF}
end;

end.
```

**Utilisation :**

```pascal
program TestBenchmark;

uses
  BenchmarkUnit, SysUtils;

var
  bench: TBenchmark;
  i, somme: Integer;
  temps: Double;

begin
  bench := TBenchmark.Create;
  try
    bench.Start;

    // Code à mesurer
    somme := 0;
    for i := 1 to 10000000 do
      somme := somme + i;

    temps := bench.Stop;
    WriteLn('Temps: ', temps:0:3, ' ms');
    WriteLn('Résultat: ', somme);
  finally
    bench.Free;
  end;
end.
```

## Comparaison d'algorithmes

### Exemple : Recherche dans un tableau

Comparons trois méthodes de recherche d'un élément.

```pascal
program CompareSearch;

uses
  SysUtils, BenchmarkUnit;

const
  TAILLE = 100000;

var
  tableau: array of Integer;
  i, recherche: Integer;
  bench: TBenchmark;
  temps: Double;

// Méthode 1: Recherche linéaire simple
function RechercheLineaire(valeur: Integer): Boolean;  
var
  i: Integer;
begin
  Result := False;
  for i := 0 to High(tableau) do
    if tableau[i] = valeur then
    begin
      Result := True;
      Exit;
    end;
end;

// Méthode 2: Recherche avec Break
function RechercheAvecBreak(valeur: Integer): Boolean;  
var
  i: Integer;
begin
  Result := False;
  for i := 0 to High(tableau) do
    if tableau[i] = valeur then
    begin
      Result := True;
      Break;
    end;
end;

// Méthode 3: Recherche dichotomique (tableau trié)
function RechercheDichotomique(valeur: Integer): Boolean;  
var
  gauche, droite, milieu: Integer;
begin
  gauche := 0;
  droite := High(tableau);

  while gauche <= droite do
  begin
    milieu := (gauche + droite) div 2;
    if tableau[milieu] = valeur then
      Exit(True)
    else if tableau[milieu] < valeur then
      gauche := milieu + 1
    else
      droite := milieu - 1;
  end;

  Result := False;
end;

begin
  // Initialisation du tableau
  SetLength(tableau, TAILLE);
  for i := 0 to High(tableau) do
    tableau[i] := i;

  recherche := TAILLE - 1;  // Chercher le dernier élément (pire cas)

  bench := TBenchmark.Create;
  try
    // Test 1: Recherche linéaire
    bench.Start;
    RechercheLineaire(recherche);
    temps := bench.Stop;
    WriteLn('Recherche linéaire:      ', temps:8:3, ' ms');

    // Test 2: Recherche avec Break
    bench.Start;
    RechercheAvecBreak(recherche);
    temps := bench.Stop;
    WriteLn('Recherche avec Break:    ', temps:8:3, ' ms');

    // Test 3: Recherche dichotomique
    bench.Start;
    RechercheDichotomique(recherche);
    temps := bench.Stop;
    WriteLn('Recherche dichotomique:  ', temps:8:3, ' ms');

  finally
    bench.Free;
  end;
end.
```

**Résultats typiques :**
```
Recherche linéaire:          0.850 ms  
Recherche avec Break:        0.850 ms  
Recherche dichotomique:      0.002 ms
```

**Conclusion :** La recherche dichotomique est ~425 fois plus rapide !

## Benchmark de structures de données

### Comparaison TList vs TFPList

```pascal
program CompareLists;

uses
  SysUtils, Classes, fgl, BenchmarkUnit;

const
  NB_OPERATIONS = 100000;

var
  bench: TBenchmark;
  temps: Double;
  i: Integer;

procedure TestTList;  
var
  liste: TList;
  i: Integer;
begin
  liste := TList.Create;
  try
    for i := 1 to NB_OPERATIONS do
      liste.Add(Pointer(i));
  finally
    liste.Free;
  end;
end;

procedure TestTFPList;  
var
  liste: TFPList;
  i: Integer;
begin
  liste := TFPList.Create;
  try
    for i := 1 to NB_OPERATIONS do
      liste.Add(Pointer(i));
  finally
    liste.Free;
  end;
end;

begin
  bench := TBenchmark.Create;
  try
    WriteLn('Ajout de ', NB_OPERATIONS, ' éléments:');
    WriteLn;

    // Test TList
    bench.Start;
    TestTList;
    temps := bench.Stop;
    WriteLn('TList:   ', temps:8:2, ' ms');

    // Test TFPList
    bench.Start;
    TestTFPList;
    temps := bench.Stop;
    WriteLn('TFPList: ', temps:8:2, ' ms');

  finally
    bench.Free;
  end;
end.
```

## Profiling : Identifier les goulots d'étranglement

Le **profiling** permet d'identifier quelles fonctions consomment le plus de temps.

### Profiling manuel avec instrumentation

```pascal
program ManualProfiling;

uses
  SysUtils, BenchmarkUnit;

var
  tempsTotal, tempsCalcul, tempsAffichage: Double;
  bench: TBenchmark;

procedure FonctionCouteuse;  
var
  i, j, somme: Integer;
begin
  bench.Start;

  somme := 0;
  for i := 1 to 1000 do
    for j := 1 to 1000 do
      somme := somme + i * j;

  tempsCalcul := tempsCalcul + bench.Stop;
end;

procedure AfficherResultat(valeur: Integer);  
begin
  bench.Start;

  WriteLn('Résultat: ', valeur);

  tempsAffichage := tempsAffichage + bench.Stop;
end;

begin
  bench := TBenchmark.Create;
  try
    tempsCalcul := 0;
    tempsAffichage := 0;

    bench.Start;

    // Programme principal
    FonctionCouteuse;
    AfficherResultat(42);
    FonctionCouteuse;
    AfficherResultat(100);

    tempsTotal := bench.Stop;

    WriteLn;
    WriteLn('=== Profil d''exécution ===');
    WriteLn('Temps total:      ', tempsTotal:8:2, ' ms (100.0%)');
    WriteLn('Temps calcul:     ', tempsCalcul:8:2, ' ms (',
            (tempsCalcul/tempsTotal*100):5:1, '%)');
    WriteLn('Temps affichage:  ', tempsAffichage:8:2, ' ms (',
            (tempsAffichage/tempsTotal*100):5:1, '%)');

  finally
    bench.Free;
  end;
end.
```

### Profiling avec gprof (Linux)

**gprof** est un outil de profiling disponible sous Linux.

#### Compilation avec profiling

```bash
# Compiler avec l'option -pg
fpc -pg -O2 MonProgramme.pas

# Exécuter le programme
./MonProgramme

# Analyser les résultats
gprof MonProgramme gmon.out > profil.txt
```

#### Lecture du rapport

```
  %   cumulative   self              self     total
 time   seconds   seconds    calls  ms/call  ms/call  name
 60.00      0.03     0.03      100     0.30     0.30  FonctionCouteuse
 30.00      0.05     0.02     1000     0.02     0.02  CalculerSomme
 10.00      0.06     0.01       50     0.20     0.20  AfficherResultat
```

**Interprétation :**
- `FonctionCouteuse` consomme 60% du temps total
- C'est la cible prioritaire pour l'optimisation

### Profiling sous Windows

#### Avec AMD CodeAnalyst / Intel VTune

Ces outils professionnels offrent un profiling détaillé mais sont complexes.

#### Solution simple : Sampling manuel

```pascal
program SimpleSampler;

uses
  Windows, SysUtils;

var
  compteurs: array[1..10] of Integer;

procedure Zone1;  
begin
  Inc(compteurs[1]);
  Sleep(10);
end;

procedure Zone2;  
begin
  Inc(compteurs[2]);
  Sleep(50);
end;

procedure Zone3;  
begin
  Inc(compteurs[3]);
  Sleep(5);
end;

var
  i: Integer;

begin
  FillChar(compteurs, SizeOf(compteurs), 0);

  // Simulation d'activité
  for i := 1 to 100 do
  begin
    Zone1;
    Zone2;
    Zone3;
  end;

  WriteLn('=== Échantillonnage ===');
  WriteLn('Zone 1: ', compteurs[1], ' appels');
  WriteLn('Zone 2: ', compteurs[2], ' appels');
  WriteLn('Zone 3: ', compteurs[3], ' appels');
end.
```

## Framework de benchmarking complet

Créons un framework réutilisable pour automatiser les benchmarks.

```pascal
unit BenchmarkFramework;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TBenchmarkProc = procedure;

  TBenchmarkResult = record
    Nom: string;
    TempsMs: Double;
    Iterations: Integer;
  end;

  TBenchmarkSuite = class
  private
    FResultats: array of TBenchmarkResult;
  public
    procedure Ajouter(const Nom: string; Proc: TBenchmarkProc; Iterations: Integer = 1);
    procedure Executer;
    procedure AfficherResultats;
  end;

implementation

uses
  BenchmarkUnit;

procedure TBenchmarkSuite.Ajouter(const Nom: string; Proc: TBenchmarkProc; Iterations: Integer);  
var
  idx: Integer;
begin
  idx := Length(FResultats);
  SetLength(FResultats, idx + 1);
  FResultats[idx].Nom := Nom;
  FResultats[idx].Iterations := Iterations;
end;

procedure TBenchmarkSuite.Executer;  
var
  i, j: Integer;
  bench: TBenchmark;
  proc: TBenchmarkProc;
begin
  bench := TBenchmark.Create;
  try
    for i := 0 to High(FResultats) do
    begin
      Write('Exécution: ', FResultats[i].Nom, '...');

      bench.Start;
      for j := 1 to FResultats[i].Iterations do
      begin
        // Exécuter le benchmark
        // Note: stockage de la procédure nécessiterait des méthodes de classe
      end;
      FResultats[i].TempsMs := bench.Stop;

      WriteLn(' OK');
    end;
  finally
    bench.Free;
  end;
end;

procedure TBenchmarkSuite.AfficherResultats;  
var
  i: Integer;
  tempsMoyen: Double;
begin
  WriteLn;
  WriteLn('=== Résultats des Benchmarks ===');
  WriteLn;
  WriteLn('Nom':30, 'Temps total':15, 'Iterations':12, 'Temps/iter':15);
  WriteLn(StringOfChar('-', 72));

  for i := 0 to High(FResultats) do
  begin
    tempsMoyen := FResultats[i].TempsMs / FResultats[i].Iterations;
    WriteLn(FResultats[i].Nom:30,
            FResultats[i].TempsMs:14:3, ' ms',
            FResultats[i].Iterations:12,
            tempsMoyen:14:6, ' ms');
  end;
end;

end.
```

## Bonnes pratiques de benchmarking

### 1. Préchauffage (Warm-up)

Les premières exécutions sont souvent plus lentes (cache CPU, compilation JIT, etc.).

```pascal
procedure BenchmarkAvecPrechauffage;  
var
  i: Integer;
  bench: TBenchmark;
  temps: Double;
begin
  // Préchauffage: 10 itérations ignorées
  for i := 1 to 10 do
    FonctionATester;

  // Mesure réelle
  bench := TBenchmark.Create;
  try
    bench.Start;

    for i := 1 to 1000 do
      FonctionATester;

    temps := bench.Stop;
    WriteLn('Temps moyen: ', (temps/1000):0:6, ' ms par appel');
  finally
    bench.Free;
  end;
end;
```

### 2. Multiples itérations

Une seule mesure n'est pas fiable. Il faut moyenner plusieurs exécutions.

```pascal
procedure BenchmarkMultiple;  
const
  NB_TESTS = 10;
var
  i: Integer;
  temps, somme, minimum, maximum, moyenne: Double;
  bench: TBenchmark;
begin
  bench := TBenchmark.Create;
  try
    somme := 0;
    minimum := MaxDouble;
    maximum := 0;

    for i := 1 to NB_TESTS do
    begin
      bench.Start;
      FonctionATester;
      temps := bench.Stop;

      somme := somme + temps;
      if temps < minimum then minimum := temps;
      if temps > maximum then maximum := temps;
    end;

    moyenne := somme / NB_TESTS;

    WriteLn('Minimum: ', minimum:0:3, ' ms');
    WriteLn('Maximum: ', maximum:0:3, ' ms');
    WriteLn('Moyenne: ', moyenne:0:3, ' ms');

  finally
    bench.Free;
  end;
end;
```

### 3. Isoler le code à mesurer

```pascal
// MAUVAIS : Mesure trop de choses
bench.Start;  
liste := TStringList.Create;  
try
  for i := 1 to 1000 do
    liste.Add(IntToStr(i));
finally
  liste.Free;
end;  
temps := bench.Stop;

// BON : Mesure uniquement l'ajout
liste := TStringList.Create;  
try
  bench.Start;
  for i := 1 to 1000 do
    liste.Add(IntToStr(i));
  temps := bench.Stop;
finally
  liste.Free;
end;
```

### 4. Éviter les optimisations du compilateur

Le compilateur peut éliminer du code "inutile".

```pascal
// MAUVAIS : Le compilateur peut optimiser la boucle
var
  i, resultat: Integer;
begin
  bench.Start;
  for i := 1 to 1000000 do
    resultat := i * 2;  // Variable jamais utilisée après
  temps := bench.Stop;
end;

// BON : Utiliser le résultat
var
  i, resultat: Integer;
begin
  bench.Start;
  resultat := 0;
  for i := 1 to 1000000 do
    resultat := resultat + i * 2;
  temps := bench.Stop;
  WriteLn(resultat);  // Forcer l'utilisation
end;
```

### 5. Désactiver les optimisations pour des tests équitables

Lors de la comparaison d'algorithmes, compiler avec les mêmes options.

```bash
# Sans optimisation
fpc -O- programme.pas

# Optimisation niveau 2
fpc -O2 programme.pas

# Optimisation maximale
fpc -O3 programme.pas
```

## Différences de performance Windows vs Linux

### Facteurs influençant les performances

```pascal
program CompareOS;

uses
  SysUtils, BenchmarkUnit;

var
  bench: TBenchmark;
  i, somme: Integer;
  temps: Double;

begin
  bench := TBenchmark.Create;
  try
    WriteLn('Système: ', {$IFDEF WINDOWS}'Windows'{$ELSE}'Linux'{$ENDIF});
    WriteLn;

    // Test 1: Opérations CPU
    bench.Start;
    somme := 0;
    for i := 1 to 100000000 do
      somme := somme + i;
    temps := bench.Stop;
    WriteLn('Opérations CPU:    ', temps:8:2, ' ms');

    // Test 2: Allocation mémoire
    bench.Start;
    for i := 1 to 100000 do
      GetMem(Pointer(somme), 1024);  // Allouer sans libérer (test uniquement)
    temps := bench.Stop;
    WriteLn('Allocations:       ', temps:8:2, ' ms');

    // Test 3: Opérations fichiers
    bench.Start;
    for i := 1 to 1000 do
      FileExists('test.txt');  // Appels système
    temps := bench.Stop;
    WriteLn('Appels système:    ', temps:8:2, ' ms');

  finally
    bench.Free;
  end;
end.
```

### Résultats typiques

**Windows :**
```
Opérations CPU:      850.00 ms  
Allocations:          45.00 ms  
Appels système:       12.00 ms
```

**Linux :**
```
Opérations CPU:      820.00 ms  
Allocations:          38.00 ms  
Appels système:        8.00 ms
```

**Observations :**
- Linux souvent plus rapide pour les appels système
- Windows peut être plus rapide avec certaines opérations mémoire (selon la configuration)
- Les différences varient selon le matériel et la version de l'OS

## Tests de charge (Load Testing)

Tester comment le programme se comporte sous charge.

```pascal
program LoadTest;

uses
  SysUtils, Classes, BenchmarkUnit;

type
  TWorkerThread = class(TThread)
  private
    FIterations: Integer;
    FTempsTotal: Double;
  protected
    procedure Execute; override;
  public
    constructor Create(Iterations: Integer);
    property TempsTotal: Double read FTempsTotal;
  end;

constructor TWorkerThread.Create(Iterations: Integer);  
begin
  inherited Create(False);
  FreeOnTerminate := False;
  FIterations := Iterations;
end;

procedure TWorkerThread.Execute;  
var
  i, j, somme: Integer;
  bench: TBenchmark;
begin
  bench := TBenchmark.Create;
  try
    bench.Start;

    somme := 0;
    for i := 1 to FIterations do
      for j := 1 to 1000 do
        somme := somme + i * j;

    FTempsTotal := bench.Stop;
  finally
    bench.Free;
  end;
end;

var
  threads: array[1..10] of TWorkerThread;
  i: Integer;
  tempsDebut, tempsFin: QWord;
  tempsTotal, tempsMoyen: Double;

begin
  WriteLn('Test de charge avec 10 threads...');
  WriteLn;

  tempsDebut := GetTickCount64;

  // Créer et démarrer les threads
  for i := 1 to 10 do
    threads[i] := TWorkerThread.Create(10000);

  // Attendre la fin de tous les threads
  for i := 1 to 10 do
  begin
    threads[i].WaitFor;
    WriteLn('Thread ', i:2, ': ', threads[i].TempsTotal:8:2, ' ms');
  end;

  tempsFin := GetTickCount64;
  tempsTotal := tempsFin - tempsDebut;

  // Calculer le temps moyen par thread
  tempsMoyen := 0;
  for i := 1 to 10 do
    tempsMoyen := tempsMoyen + threads[i].TempsTotal;
  tempsMoyen := tempsMoyen / 10;

  WriteLn;
  WriteLn('Temps total:       ', tempsTotal:8:2, ' ms');
  WriteLn('Temps moyen/thread:', tempsMoyen:8:2, ' ms');

  // Libérer les threads
  for i := 1 to 10 do
    threads[i].Free;
end.
```

## Visualisation des résultats

### Génération de rapport HTML

```pascal
program BenchmarkReport;

uses
  SysUtils, Classes;

type
  TBenchResult = record
    Nom: string;
    Temps: Double;
  end;

var
  resultats: array of TBenchResult;
  html: TStringList;
  i: Integer;
  tempsMax: Double;
  largeurBarre: Integer;

begin
  // Simuler quelques résultats
  SetLength(resultats, 5);
  resultats[0].Nom := 'Algorithme A';
  resultats[0].Temps := 45.3;
  resultats[1].Nom := 'Algorithme B';
  resultats[1].Temps := 23.7;
  resultats[2].Nom := 'Algorithme C';
  resultats[2].Temps := 89.2;
  resultats[3].Nom := 'Algorithme D';
  resultats[3].Temps := 12.5;
  resultats[4].Nom := 'Algorithme E';
  resultats[4].Temps := 56.8;

  // Trouver le temps maximum pour l'échelle
  tempsMax := 0;
  for i := 0 to High(resultats) do
    if resultats[i].Temps > tempsMax then
      tempsMax := resultats[i].Temps;

  // Générer le rapport HTML
  html := TStringList.Create;
  try
    html.Add('<!DOCTYPE html>');
    html.Add('<html>');
    html.Add('<head>');
    html.Add('  <meta charset="UTF-8">');
    html.Add('  <title>Rapport de Benchmark</title>');
    html.Add('  <style>');
    html.Add('    body { font-family: Arial, sans-serif; margin: 20px; }');
    html.Add('    h1 { color: #333; }');
    html.Add('    table { border-collapse: collapse; width: 100%; margin-top: 20px; }');
    html.Add('    th, td { border: 1px solid #ddd; padding: 12px; text-align: left; }');
    html.Add('    th { background-color: #4CAF50; color: white; }');
    html.Add('    tr:nth-child(even) { background-color: #f2f2f2; }');
    html.Add('    .barre { background-color: #2196F3; height: 20px; }');
    html.Add('    .fastest { background-color: #4CAF50; }');
    html.Add('  </style>');
    html.Add('</head>');
    html.Add('<body>');
    html.Add('  <h1>Rapport de Benchmark</h1>');
    html.Add('  <p>Généré le: ' + DateTimeToStr(Now) + '</p>');
    html.Add('  <table>');
    html.Add('    <tr>');
    html.Add('      <th>Algorithme</th>');
    html.Add('      <th>Temps (ms)</th>');
    html.Add('      <th>Visualisation</th>');
    html.Add('    </tr>');

    for i := 0 to High(resultats) do
    begin
      html.Add('    <tr>');
      html.Add('      <td>' + resultats[i].Nom + '</td>');
      html.Add('      <td>' + FormatFloat('0.00', resultats[i].Temps) + '</td>');
      html.Add('      <td>');

      // Calculer la largeur de la barre (max 500px)
      largeurBarre := Round((resultats[i].Temps / tempsMax) * 500);

      html.Add('        <div class="barre" style="width: ' +
               IntToStr(largeurBarre) + 'px;"></div>');
      html.Add('      </td>');
      html.Add('    </tr>');
    end;

    html.Add('  </table>');
    html.Add('</body>');
    html.Add('</html>');

    // Sauvegarder le fichier
    html.SaveToFile('benchmark_report.html');
    WriteLn('Rapport généré: benchmark_report.html');

  finally
    html.Free;
  end;
end.
```

### Graphique en console (ASCII art)

```pascal
program ConsoleChart;

uses
  SysUtils;

type
  TBenchResult = record
    Nom: string;
    Temps: Double;
  end;

procedure AfficherGraphique(const resultats: array of TBenchResult);  
const
  LARGEUR_MAX = 60;
var
  i, j, nbBarres: Integer;
  tempsMax: Double;
begin
  // Trouver le maximum
  tempsMax := 0;
  for i := 0 to High(resultats) do
    if resultats[i].Temps > tempsMax then
      tempsMax := resultats[i].Temps;

  WriteLn;
  WriteLn('=== Graphique de performance ===');
  WriteLn;

  for i := 0 to High(resultats) do
  begin
    // Afficher le nom (aligné à gauche sur 20 caractères)
    Write(resultats[i].Nom:20, ' | ');

    // Calculer le nombre de barres à afficher
    nbBarres := Round((resultats[i].Temps / tempsMax) * LARGEUR_MAX);

    // Afficher les barres
    for j := 1 to nbBarres do
      Write('█');

    // Afficher le temps
    WriteLn(' ', resultats[i].Temps:6:2, ' ms');
  end;

  WriteLn;
  Write(' ':20, ' | ');
  for j := 1 to LARGEUR_MAX do
    Write('-');
  WriteLn;
  Write(' ':20, ' 0');
  Write(' ':LARGEUR_MAX-10);
  WriteLn(tempsMax:8:2, ' ms');
end;

var
  resultats: array[0..4] of TBenchResult;

begin
  // Données de test
  resultats[0].Nom := 'Algo A';
  resultats[0].Temps := 45.3;
  resultats[1].Nom := 'Algo B';
  resultats[1].Temps := 23.7;
  resultats[2].Nom := 'Algo C';
  resultats[2].Temps := 89.2;
  resultats[3].Nom := 'Algo D';
  resultats[3].Temps := 12.5;
  resultats[4].Nom := 'Algo E';
  resultats[4].Temps := 56.8;

  AfficherGraphique(resultats);
end.
```

**Sortie :**
```
=== Graphique de performance ===

              Algo A | ████████████████████████████  45.30 ms
              Algo B | ███████████████  23.70 ms
              Algo C | ████████████████████████████████████████████████████████  89.20 ms
              Algo D | ████████  12.50 ms
              Algo E | ██████████████████████████████████  56.80 ms

                     | ------------------------------------------------------------
                     0                                                       89.20 ms
```

## Automatisation avec scripts

### Script bash pour benchmarks automatiques (Linux)

```bash
#!/bin/bash
# benchmark_auto.sh

PROJECT_DIR="/chemin/vers/projet"  
OUTPUT_DIR="$PROJECT_DIR/benchmark_results"  
DATE=$(date +%Y%m%d_%H%M%S)

echo "=== Benchmark Automatique ==="  
echo "Date: $(date)"  
echo

# Créer le dossier de sortie
mkdir -p "$OUTPUT_DIR"

# Compiler avec optimisation
echo "Compilation..."  
cd "$PROJECT_DIR"  
fpc -O3 -XX benchmark_suite.pas

if [ $? -ne 0 ]; then
    echo "Erreur de compilation!"
    exit 1
fi

# Exécuter les benchmarks
echo "Exécution des benchmarks..."
./benchmark_suite > "$OUTPUT_DIR/results_$DATE.txt"

# Afficher les résultats
echo  
echo "=== Résultats ==="  
cat "$OUTPUT_DIR/results_$DATE.txt"

# Comparer avec les résultats précédents
PREV_FILE=$(ls -t "$OUTPUT_DIR"/results_*.txt 2>/dev/null | sed -n '2p')

if [ -n "$PREV_FILE" ]; then
    echo
    echo "=== Comparaison avec l'exécution précédente ==="
    echo "Fichier précédent: $PREV_FILE"

    # Analyse simple des différences
    # (nécessiterait un script plus sophistiqué pour une vraie comparaison)
    diff "$PREV_FILE" "$OUTPUT_DIR/results_$DATE.txt" || true
fi

echo  
echo "Résultats sauvegardés dans: $OUTPUT_DIR/results_$DATE.txt"
```

### Script PowerShell pour benchmarks automatiques (Windows)

```powershell
# benchmark_auto.ps1

$ProjectDir = "C:\Projets\MonProjet"
$OutputDir = "$ProjectDir\benchmark_results"
$Date = Get-Date -Format "yyyyMMdd_HHmmss"

Write-Host "=== Benchmark Automatique ===" -ForegroundColor Green  
Write-Host "Date: $(Get-Date)"  
Write-Host ""

# Créer le dossier de sortie
New-Item -ItemType Directory -Force -Path $OutputDir | Out-Null

# Compiler avec optimisation
Write-Host "Compilation..." -ForegroundColor Yellow  
Set-Location $ProjectDir
& "C:\lazarus\fpc\bin\x86_64-win64\fpc.exe" -O3 -XX benchmark_suite.pas

if ($LASTEXITCODE -ne 0) {
    Write-Host "Erreur de compilation!" -ForegroundColor Red
    exit 1
}

# Exécuter les benchmarks
Write-Host "Exécution des benchmarks..." -ForegroundColor Yellow
$Output = & ".\benchmark_suite.exe"
$Output | Out-File -FilePath "$OutputDir\results_$Date.txt"

# Afficher les résultats
Write-Host ""  
Write-Host "=== Résultats ===" -ForegroundColor Green  
Write-Host $Output

# Comparer avec les résultats précédents
$PrevFile = Get-ChildItem -Path $OutputDir -Filter "results_*.txt" |
            Sort-Object LastWriteTime -Descending |
            Select-Object -Skip 1 -First 1

if ($PrevFile) {
    Write-Host ""
    Write-Host "=== Comparaison avec l'exécution précédente ===" -ForegroundColor Cyan
    Write-Host "Fichier précédent: $($PrevFile.Name)"

    Compare-Object (Get-Content $PrevFile.FullName) (Get-Content "$OutputDir\results_$Date.txt")
}

Write-Host ""  
Write-Host "Résultats sauvegardés dans: $OutputDir\results_$Date.txt" -ForegroundColor Green
```

## Benchmarking de bases de données

### Comparaison de requêtes SQL

```pascal
program DatabaseBenchmark;

{$mode objfpc}{$H+}

uses
  SysUtils, sqldb, sqlite3conn, BenchmarkUnit;

var
  conn: TSQLite3Connection;
  trans: TSQLTransaction;
  query: TSQLQuery;
  bench: TBenchmark;
  temps: Double;
  i: Integer;

procedure InitDatabase;  
begin
  conn := TSQLite3Connection.Create(nil);
  trans := TSQLTransaction.Create(nil);
  query := TSQLQuery.Create(nil);

  conn.DatabaseName := 'benchmark.db';
  trans.DataBase := conn;
  conn.Transaction := trans;
  query.DataBase := conn;

  conn.Open;
  trans.Active := True;

  // Créer une table de test
  query.SQL.Text := 'DROP TABLE IF EXISTS test_data';
  query.ExecSQL;

  query.SQL.Text := 'CREATE TABLE test_data (id INTEGER PRIMARY KEY, valeur INTEGER, texte TEXT)';
  query.ExecSQL;

  // Insérer 10000 lignes
  for i := 1 to 10000 do
  begin
    query.SQL.Text := 'INSERT INTO test_data (valeur, texte) VALUES (:val, :txt)';
    query.ParamByName('val').AsInteger := Random(1000);
    query.ParamByName('txt').AsString := 'Texte ' + IntToStr(i);
    query.ExecSQL;
  end;

  trans.Commit;
  WriteLn('Base de données initialisée avec 10000 enregistrements');
  WriteLn;
end;

procedure TestSansIndex;  
begin
  query.SQL.Text := 'SELECT * FROM test_data WHERE valeur = 500';
  query.Open;
  query.Close;
end;

procedure TestAvecIndex;  
begin
  query.SQL.Text := 'SELECT * FROM test_data WHERE id = 500';
  query.Open;
  query.Close;
end;

procedure TestCount;  
begin
  query.SQL.Text := 'SELECT COUNT(*) FROM test_data';
  query.Open;
  query.Close;
end;

begin
  bench := TBenchmark.Create;
  try
    InitDatabase;

    WriteLn('=== Benchmarks SQL ===');
    WriteLn;

    // Test 1: Recherche sans index
    bench.Start;
    for i := 1 to 100 do
      TestSansIndex;
    temps := bench.Stop;
    WriteLn('Recherche sans index (100x): ', temps:8:2, ' ms');

    // Créer un index
    query.SQL.Text := 'CREATE INDEX idx_valeur ON test_data(valeur)';
    query.ExecSQL;
    trans.Commit;
    WriteLn('Index créé sur colonne "valeur"');

    // Test 2: Recherche avec index
    bench.Start;
    for i := 1 to 100 do
      TestSansIndex;
    temps := bench.Stop;
    WriteLn('Recherche avec index (100x): ', temps:8:2, ' ms');

    // Test 3: Recherche par clé primaire
    bench.Start;
    for i := 1 to 100 do
      TestAvecIndex;
    temps := bench.Stop;
    WriteLn('Recherche par PK (100x):     ', temps:8:2, ' ms');

    // Test 4: COUNT
    bench.Start;
    for i := 1 to 100 do
      TestCount;
    temps := bench.Stop;
    WriteLn('COUNT(*) (100x):             ', temps:8:2, ' ms');

  finally
    query.Free;
    trans.Free;
    conn.Free;
    bench.Free;
  end;
end.
```

## Benchmarking réseau

### Test de latence et débit

```pascal
program NetworkBenchmark;

{$mode objfpc}{$H+}

uses
  SysUtils, Sockets, BenchmarkUnit;

function PingServer(const Host: string; Port: Word): Double;  
var
  sock: TSocket;
  addr: TInetSockAddr;
  bench: TBenchmark;
begin
  bench := TBenchmark.Create;
  try
    sock := fpSocket(AF_INET, SOCK_STREAM, 0);
    try
      addr.sin_family := AF_INET;
      addr.sin_port := htons(Port);
      addr.sin_addr := StrToNetAddr(Host);

      bench.Start;
      if fpConnect(sock, @addr, SizeOf(addr)) = 0 then
        Result := bench.Stop
      else
        Result := -1;
    finally
      CloseSocket(sock);
    end;
  finally
    bench.Free;
  end;
end;

var
  i: Integer;
  latence, somme, minimum, maximum: Double;

begin
  WriteLn('=== Benchmark Réseau ===');
  WriteLn;
  WriteLn('Test de latence vers localhost:80...');
  WriteLn;

  somme := 0;
  minimum := MaxDouble;
  maximum := 0;

  for i := 1 to 10 do
  begin
    latence := PingServer('127.0.0.1', 80);
    if latence >= 0 then
    begin
      WriteLn('Tentative ', i:2, ': ', latence:6:2, ' ms');
      somme := somme + latence;
      if latence < minimum then minimum := latence;
      if latence > maximum then maximum := latence;
    end
    else
      WriteLn('Tentative ', i:2, ': Échec');
  end;

  WriteLn;
  WriteLn('Minimum: ', minimum:6:2, ' ms');
  WriteLn('Maximum: ', maximum:6:2, ' ms');
  WriteLn('Moyenne: ', (somme/10):6:2, ' ms');
end.
```

## Régression de performance

### Détection automatique de régression

```pascal
program RegressionDetector;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, IniFiles;

const
  SEUIL_REGRESSION = 10.0; // 10% de dégradation acceptable

type
  TBenchmarkHistory = record
    Nom: string;
    TempsPrecedent: Double;
    TempsActuel: Double;
    Difference: Double;
    DifferencePourcent: Double;
  end;

var
  historique: array of TBenchmarkHistory;
  config: TIniFile;

procedure SauvegarderResultats(const Nom: string; Temps: Double);  
begin
  config.WriteFloat('Benchmarks', Nom, Temps);
end;

function ChargerResultatPrecedent(const Nom: string): Double;  
begin
  Result := config.ReadFloat('Benchmarks', Nom, -1);
end;

procedure AnalyserRegression(const Nom: string; TempsActuel: Double);  
var
  tempsPrecedent: Double;
  idx: Integer;
begin
  tempsPrecedent := ChargerResultatPrecedent(Nom);

  if tempsPrecedent > 0 then
  begin
    idx := Length(historique);
    SetLength(historique, idx + 1);

    historique[idx].Nom := Nom;
    historique[idx].TempsPrecedent := tempsPrecedent;
    historique[idx].TempsActuel := TempsActuel;
    historique[idx].Difference := TempsActuel - tempsPrecedent;
    historique[idx].DifferencePourcent :=
      ((TempsActuel - tempsPrecedent) / tempsPrecedent) * 100;
  end;

  SauvegarderResultats(Nom, TempsActuel);
end;

procedure AfficherRapportRegression;  
var
  i: Integer;
  aRegressions: Boolean;
begin
  WriteLn;
  WriteLn('=== Rapport de Régression ===');
  WriteLn;

  if Length(historique) = 0 then
  begin
    WriteLn('Aucune donnée historique disponible.');
    Exit;
  end;

  WriteLn('Test':30, 'Précédent':12, 'Actuel':12, 'Diff':10, 'Diff %':10);
  WriteLn(StringOfChar('-', 74));

  aRegressions := False;

  for i := 0 to High(historique) do
  begin
    Write(historique[i].Nom:30);
    Write(historique[i].TempsPrecedent:11:2, ' ms');
    Write(historique[i].TempsActuel:11:2, ' ms');
    Write(historique[i].Difference:9:2, ' ms');
    Write(historique[i].DifferencePourcent:9:1, '%');

    // Marquer les régressions
    if historique[i].DifferencePourcent > SEUIL_REGRESSION then
    begin
      Write(' ⚠ RÉGRESSION');
      aRegressions := True;
    end
    else if historique[i].DifferencePourcent < -5 then
      Write(' ✓ Amélioration');

    WriteLn;
  end;

  WriteLn;
  if aRegressions then
    WriteLn('⚠ ATTENTION: Des régressions de performance ont été détectées!')
  else
    WriteLn('✓ Aucune régression détectée.');
end;

// Simulation de benchmarks
procedure BenchmarkA;  
var
  i, somme: Integer;
begin
  somme := 0;
  for i := 1 to 10000000 do
    somme := somme + i;
end;

procedure BenchmarkB;  
var
  i: Integer;
  liste: TStringList;
begin
  liste := TStringList.Create;
  try
    for i := 1 to 10000 do
      liste.Add(IntToStr(i));
  finally
    liste.Free;
  end;
end;

var
  bench: TBenchmark;
  temps: Double;

begin
  config := TIniFile.Create('benchmark_history.ini');
  bench := TBenchmark.Create;
  try
    WriteLn('Exécution des benchmarks...');
    WriteLn;

    // Benchmark A
    bench.Start;
    BenchmarkA;
    temps := bench.Stop;
    WriteLn('Benchmark A: ', temps:8:2, ' ms');
    AnalyserRegression('BenchmarkA', temps);

    // Benchmark B
    bench.Start;
    BenchmarkB;
    temps := bench.Stop;
    WriteLn('Benchmark B: ', temps:8:2, ' ms');
    AnalyserRegression('BenchmarkB', temps);

    // Afficher le rapport
    AfficherRapportRegression;

  finally
    bench.Free;
    config.Free;
  end;
end.
```

## Conseils pour des benchmarks fiables

### 1. Environnement stable

**À faire :**
- Fermer les applications inutiles
- Désactiver l'antivirus pendant les tests
- Utiliser un processeur fixé (pas de throttling)
- Tester plusieurs fois et moyenner

**À éviter :**
- Tester sur un portable sur batterie (performance réduite)
- Tester pendant une mise à jour système
- Tester avec d'autres applications actives

### 2. Documenter les conditions

```pascal
program BenchmarkWithContext;

uses
  SysUtils;

begin
  WriteLn('=== Contexte du Benchmark ===');
  WriteLn('Date:          ', DateTimeToStr(Now));
  WriteLn('Système:       ', {$IFDEF WINDOWS}'Windows'{$ELSE}'Linux'{$ENDIF});
  WriteLn('Architecture:  ', {$IFDEF CPU64}'64-bit'{$ELSE}'32-bit'{$ENDIF});
  WriteLn('Compilateur:   FPC ', {$I %FPCVERSION%});
  WriteLn('Optimisation:  ', {$IFDEF DEBUG}'Debug'{$ELSE}'Release'{$ENDIF});
  WriteLn;

  // Benchmarks ici...
end.
```

### 3. Tester sur différentes configurations

**Matrice de tests recommandée :**

| OS        | CPU      | RAM  | Optimisation |
|-----------|----------|------|--------------|
| Windows   | Intel i5 | 8GB  | -O2          |
| Windows   | Intel i5 | 8GB  | -O3          |
| Ubuntu    | Intel i5 | 8GB  | -O2          |
| Ubuntu    | Intel i5 | 8GB  | -O3          |
| Windows   | AMD      | 16GB | -O2          |

## Outils externes de benchmarking

### 1. Hyperfine (multi-plateforme)

**Installation :**

```bash
# Linux
sudo apt install hyperfine

# Windows (avec Scoop)
scoop install hyperfine
```

**Utilisation :**

```bash
# Comparer deux programmes
hyperfine './programme_v1' './programme_v2'

# Avec préchauffage
hyperfine --warmup 5 './mon_programme'

# Exporter les résultats en JSON
hyperfine --export-json results.json './mon_programme'
```

### 2. Perf (Linux uniquement)

```bash
# Compiler avec symboles de débogage
fpc -g -O2 programme.pas

# Profiler avec perf
perf record -g ./programme

# Analyser les résultats
perf report

# Voir les statistiques
perf stat ./programme
```

### 3. Time (universel)

```bash
# Linux
/usr/bin/time -v ./programme

# Windows (PowerShell)
Measure-Command { .\programme.exe }
```

## Conclusion

Le benchmarking est essentiel pour :

✅ **Identifier** les goulots d'étranglement  
✅ **Comparer** différentes implémentations  
✅ **Valider** les optimisations  
✅ **Détecter** les régressions  
✅ **Garantir** les performances requises

**Points clés à retenir :**

1. **Mesurer avant d'optimiser** : Ne pas optimiser à l'aveugle
2. **Utiliser les bons outils** : Précision adaptée aux besoins
3. **Tester de manière reproductible** : Conditions stables et documentées
4. **Comparer équitablement** : Mêmes options de compilation
5. **Surveiller les régressions** : Tests automatiques réguliers
6. **Optimiser intelligemment** : Se concentrer sur les 20% critiques

**Sur Windows et Ubuntu :**
- Les outils de base (GetTickCount64, clock_gettime) sont portables
- Les différences de performance entre OS sont souvent minimes
- L'architecture et le compilateur ont plus d'impact que l'OS
- Toujours tester sur les plateformes cibles

**Prochaines étapes :**
- Intégrer le benchmarking dans votre pipeline CI/CD
- Créer une suite de benchmarks de régression
- Automatiser la génération de rapports
- Établir des objectifs de performance mesurables

⏭️ [Tests de charge et stress](/18-tests-qualite-code/07-tests-charge-stress.md)
