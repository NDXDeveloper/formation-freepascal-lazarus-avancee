🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 15.5 Algorithmes génétiques

## Introduction aux Algorithmes Génétiques

Les algorithmes génétiques (AG) sont des méthodes d'optimisation inspirées de l'évolution naturelle et de la sélection naturelle décrite par Charles Darwin. Ils simulent le processus d'évolution pour résoudre des problèmes complexes d'optimisation.

### Qu'est-ce qu'un algorithme génétique ?

Imaginez que vous devez trouver la meilleure solution à un problème parmi des millions de possibilités. Au lieu de les tester toutes (ce qui prendrait des années), un algorithme génétique :

1. **Crée** une population de solutions aléatoires
2. **Évalue** chaque solution (les meilleures survivent)
3. **Reproduit** les meilleures solutions entre elles
4. **Mute** aléatoirement certaines solutions
5. **Répète** le processus jusqu'à trouver une solution optimale

C'est exactement ce qui se passe dans la nature : les organismes les mieux adaptés survivent et transmettent leurs gènes.

### Pourquoi utiliser les algorithmes génétiques ?

Les AG sont particulièrement utiles pour :
- **Problèmes d'optimisation complexes** (voyageur de commerce, ordonnancement)
- **Espaces de recherche gigantesques** (trop grands pour une exploration exhaustive)
- **Problèmes sans solution mathématique connue**
- **Optimisation multi-objectifs** (plusieurs critères à satisfaire)
- **Apprentissage automatique** (sélection de paramètres, architecture de réseaux de neurones)

### Applications concrètes

- **Logistique** : optimisation de tournées de livraison
- **Finance** : sélection de portefeuilles d'investissement
- **Ingénierie** : design optimal de structures, circuits électroniques
- **Jeux vidéo** : comportement intelligent des personnages non-joueurs
- **Apprentissage automatique** : recherche d'hyperparamètres
- **Bioinformatique** : alignement de séquences ADN

---

## Concepts Fondamentaux

### Vocabulaire de base

**Chromosome (ou Individu)**
- Représente une solution potentielle au problème
- Encodé généralement comme une suite de bits, nombres ou caractères
- Exemple : `[1, 0, 1, 1, 0]` pour un problème binaire

**Gène**
- Un élément du chromosome
- Exemple : dans `[1, 0, 1, 1, 0]`, chaque bit est un gène

**Population**
- Ensemble de chromosomes (solutions)
- Taille typique : 50 à 500 individus

**Fitness (Aptitude)**
- Mesure de la qualité d'une solution
- Plus le fitness est élevé, meilleure est la solution
- Exemple : pour un parcours, le fitness pourrait être 1/distance

**Génération**
- Une itération complète de l'algorithme
- Typiquement : 100 à 10000 générations

### Le cycle de vie d'un algorithme génétique

```
┌─────────────────────────────────────┐
│  1. Initialisation                  │
│     Créer population aléatoire      │
└──────────────┬──────────────────────┘
               ↓
┌──────────────────────────────────────┐
│  2. Évaluation                       │
│     Calculer fitness de chaque       │
│     individu                         │
└──────────────┬───────────────────────┘
               ↓
┌──────────────────────────────────────┐
│  3. Sélection                        │
│     Choisir les meilleurs parents    │
└──────────────┬───────────────────────┘
               ↓
┌──────────────────────────────────────┐
│  4. Croisement (Crossover)           │
│     Combiner les parents             │
└──────────────┬───────────────────────┘
               ↓
┌──────────────────────────────────────┐
│  5. Mutation                         │
│     Modifier aléatoirement           │
└──────────────┬───────────────────────┘
               ↓
┌──────────────────────────────────────┐
│  6. Remplacement                     │
│     Nouvelle génération              │
└──────────────┬───────────────────────┘
               ↓
        ┌──────────────┐
        │ Terminé ?    │
        └──┬───────┬───┘
          NON    OUI
           │      │
           │      ↓
           │   Solution
           │   optimale
           └──────┘
```

---

## Implémentation de Base en FreePascal

### Structure de données fondamentale

```pascal
unit GeneticAlgorithm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math;

type
  // Type pour un gène (peut être adapté selon le problème)
  TGene = Double;

  // Chromosome = tableau de gènes
  TChromosome = array of TGene;

  // Individu avec son chromosome et son fitness
  TIndividual = record
    Chromosome: TChromosome;
    Fitness: Double;
  end;

  // Population = tableau d'individus
  TPopulation = array of TIndividual;

  // Fonction d'évaluation (à définir par l'utilisateur)
  TFitnessFunction = function(const AChromosome: TChromosome): Double;

  // Classe principale de l'algorithme génétique
  TGeneticAlgorithm = class
  private
    FPopulation: TPopulation;
    FPopulationSize: Integer;
    FChromosomeLength: Integer;
    FMutationRate: Double;
    FCrossoverRate: Double;
    FElitismCount: Integer;
    FGeneration: Integer;
    FFitnessFunction: TFitnessFunction;
    FBestIndividual: TIndividual;

    procedure InitializePopulation;
    procedure EvaluatePopulation;
    function SelectParent: TIndividual;
    function Crossover(const AParent1, AParent2: TIndividual): TIndividual;
    procedure Mutate(var AIndividual: TIndividual);
    procedure SortPopulationByFitness;
  public
    constructor Create(APopulationSize, AChromosomeLength: Integer;
                       AFitnessFunction: TFitnessFunction);
    destructor Destroy; override;

    procedure Evolve(AGenerations: Integer);
    procedure EvolveOneGeneration;

    function GetBestIndividual: TIndividual;
    function GetAverageFitness: Double;

    property PopulationSize: Integer read FPopulationSize write FPopulationSize;
    property MutationRate: Double read FMutationRate write FMutationRate;
    property CrossoverRate: Double read FCrossoverRate write FCrossoverRate;
    property ElitismCount: Integer read FElitismCount write FElitismCount;
    property Generation: Integer read FGeneration;
  end;

implementation

constructor TGeneticAlgorithm.Create(APopulationSize, AChromosomeLength: Integer;
                                      AFitnessFunction: TFitnessFunction);
begin
  FPopulationSize := APopulationSize;
  FChromosomeLength := AChromosomeLength;
  FFitnessFunction := AFitnessFunction;

  // Paramètres par défaut
  FMutationRate := 0.01;      // 1% de chance de mutation par gène
  FCrossoverRate := 0.7;      // 70% de chance de croisement
  FElitismCount := 2;         // Garder les 2 meilleurs
  FGeneration := 0;

  SetLength(FPopulation, FPopulationSize);

  // Initialiser le générateur de nombres aléatoires
  Randomize;

  InitializePopulation;
  EvaluatePopulation;
end;

destructor TGeneticAlgorithm.Destroy;  
var
  i: Integer;
begin
  for i := 0 to High(FPopulation) do
    SetLength(FPopulation[i].Chromosome, 0);
  SetLength(FPopulation, 0);
  inherited;
end;

procedure TGeneticAlgorithm.InitializePopulation;  
var
  i, j: Integer;
begin
  for i := 0 to FPopulationSize - 1 do
  begin
    SetLength(FPopulation[i].Chromosome, FChromosomeLength);

    // Initialiser chaque gène avec une valeur aléatoire entre 0 et 1
    for j := 0 to FChromosomeLength - 1 do
      FPopulation[i].Chromosome[j] := Random;

    FPopulation[i].Fitness := 0;
  end;
end;

procedure TGeneticAlgorithm.EvaluatePopulation;  
var
  i: Integer;
begin
  for i := 0 to FPopulationSize - 1 do
    FPopulation[i].Fitness := FFitnessFunction(FPopulation[i].Chromosome);

  SortPopulationByFitness;
  FBestIndividual := FPopulation[0];
end;

procedure TGeneticAlgorithm.SortPopulationByFitness;  
var
  i, j: Integer;
  temp: TIndividual;
begin
  // Tri à bulles simple (par fitness décroissant)
  for i := 0 to FPopulationSize - 2 do
    for j := i + 1 to FPopulationSize - 1 do
      if FPopulation[j].Fitness > FPopulation[i].Fitness then
      begin
        temp := FPopulation[i];
        FPopulation[i] := FPopulation[j];
        FPopulation[j] := temp;
      end;
end;

function TGeneticAlgorithm.SelectParent: TIndividual;  
var
  tournamentSize: Integer;
  i, randomIndex, bestIndex: Integer;
  bestFitness: Double;
begin
  // Sélection par tournoi
  tournamentSize := 3;
  bestIndex := Random(FPopulationSize);
  bestFitness := FPopulation[bestIndex].Fitness;

  for i := 1 to tournamentSize - 1 do
  begin
    randomIndex := Random(FPopulationSize);
    if FPopulation[randomIndex].Fitness > bestFitness then
    begin
      bestIndex := randomIndex;
      bestFitness := FPopulation[randomIndex].Fitness;
    end;
  end;

  Result := FPopulation[bestIndex];
end;

function TGeneticAlgorithm.Crossover(const AParent1, AParent2: TIndividual): TIndividual;  
var
  i, crossoverPoint: Integer;
begin
  SetLength(Result.Chromosome, FChromosomeLength);

  if Random < FCrossoverRate then
  begin
    // Croisement en un point
    crossoverPoint := Random(FChromosomeLength);

    for i := 0 to FChromosomeLength - 1 do
    begin
      if i < crossoverPoint then
        Result.Chromosome[i] := AParent1.Chromosome[i]
      else
        Result.Chromosome[i] := AParent2.Chromosome[i];
    end;
  end
  else
  begin
    // Pas de croisement, copier le premier parent
    for i := 0 to FChromosomeLength - 1 do
      Result.Chromosome[i] := AParent1.Chromosome[i];
  end;

  Result.Fitness := 0;
end;

procedure TGeneticAlgorithm.Mutate(var AIndividual: TIndividual);  
var
  i: Integer;
begin
  for i := 0 to FChromosomeLength - 1 do
  begin
    if Random < FMutationRate then
    begin
      // Mutation : remplacer par une valeur aléatoire
      AIndividual.Chromosome[i] := Random;
    end;
  end;
end;

procedure TGeneticAlgorithm.EvolveOneGeneration;  
var
  newPopulation: TPopulation;
  i: Integer;
  parent1, parent2, child: TIndividual;
begin
  SetLength(newPopulation, FPopulationSize);

  // Élitisme : garder les meilleurs individus
  for i := 0 to FElitismCount - 1 do
    newPopulation[i] := FPopulation[i];

  // Créer le reste de la nouvelle population
  for i := FElitismCount to FPopulationSize - 1 do
  begin
    // Sélection
    parent1 := SelectParent;
    parent2 := SelectParent;

    // Croisement
    child := Crossover(parent1, parent2);

    // Mutation
    Mutate(child);

    newPopulation[i] := child;
  end;

  // Remplacer l'ancienne population
  FPopulation := newPopulation;

  // Évaluer la nouvelle population
  EvaluatePopulation;

  Inc(FGeneration);
end;

procedure TGeneticAlgorithm.Evolve(AGenerations: Integer);  
var
  i: Integer;
begin
  for i := 1 to AGenerations do
    EvolveOneGeneration;
end;

function TGeneticAlgorithm.GetBestIndividual: TIndividual;  
begin
  Result := FBestIndividual;
end;

function TGeneticAlgorithm.GetAverageFitness: Double;  
var
  i: Integer;
  sum: Double;
begin
  sum := 0;
  for i := 0 to FPopulationSize - 1 do
    sum := sum + FPopulation[i].Fitness;

  Result := sum / FPopulationSize;
end;

end.
```

---

## Premier Exemple : Optimiser une Fonction Mathématique

### Problème : Trouver le maximum de f(x) = -x² + 10x

```pascal
program OptimizeFunction;

{$mode objfpc}{$H+}

uses
  SysUtils, GeneticAlgorithm;

// Fonction à optimiser : f(x) = -x² + 10x
// Maximum théorique en x = 5, f(5) = 25
function ObjectiveFunction(const AChromosome: TChromosome): Double;  
var
  x: Double;
begin
  // Convertir le chromosome (valeur entre 0 et 1) en x (entre 0 et 10)
  x := AChromosome[0] * 10;

  // Calculer f(x) = -x² + 10x
  Result := -(x * x) + 10 * x;
end;

var
  ga: TGeneticAlgorithm;
  best: TIndividual;
  i: Integer;
  x: Double;

begin
  WriteLn('=== Optimisation d''une fonction avec AG ===');
  WriteLn('Fonction: f(x) = -x² + 10x');
  WriteLn('Recherche du maximum sur [0, 10]');
  WriteLn;

  // Créer l'algorithme génétique
  // 50 individus, 1 gène par chromosome
  ga := TGeneticAlgorithm.Create(50, 1, @ObjectiveFunction);
  try
    ga.MutationRate := 0.05;
    ga.CrossoverRate := 0.8;
    ga.ElitismCount := 2;

    // Afficher la progression
    WriteLn('Génération | Meilleur X | Meilleur F(X) | Fitness Moyenne');
    WriteLn('----------------------------------------------------------');

    for i := 0 to 50 do
    begin
      if i mod 10 = 0 then
      begin
        best := ga.GetBestIndividual;
        x := best.Chromosome[0] * 10;
        WriteLn(Format('%10d | %10.4f | %13.4f | %15.4f',
          [ga.Generation, x, best.Fitness, ga.GetAverageFitness]));
      end;

      ga.EvolveOneGeneration;
    end;

    WriteLn;
    WriteLn('=== RÉSULTAT FINAL ===');
    best := ga.GetBestIndividual;
    x := best.Chromosome[0] * 10;
    WriteLn(Format('x optimal trouvé: %.4f', [x]));
    WriteLn(Format('f(x) maximum: %.4f', [best.Fitness]));
    WriteLn;
    WriteLn('Solution théorique: x = 5, f(5) = 25');

  finally
    ga.Free;
  end;

  {$IFDEF WINDOWS}
  WriteLn;
  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
  {$ENDIF}
end.
```

**Sortie attendue :**
```
=== Optimisation d'une fonction avec AG ===
Fonction: f(x) = -x² + 10x  
Recherche du maximum sur [0, 10]

Génération | Meilleur X | Meilleur F(X) | Fitness Moyenne
----------------------------------------------------------
         0 |     4.2156 |      22.1022 |         15.3421
        10 |     4.8923 |      24.5234 |         21.2341
        20 |     4.9812 |      24.9102 |         23.4521
        30 |     5.0045 |      24.9889 |         24.3212
        40 |     4.9998 |      24.9999 |         24.7654
        50 |     5.0001 |      25.0000 |         24.8932

=== RÉSULTAT FINAL ===
x optimal trouvé: 5.0001  
f(x) maximum: 25.0000

Solution théorique: x = 5, f(5) = 25
```

---

## Opérateurs Génétiques Détaillés

### 1. Sélection

La sélection détermine quels individus se reproduisent. Plusieurs méthodes existent :

**Sélection par Tournoi (déjà implémentée)**
- Choisir aléatoirement K individus
- Retenir le meilleur

**Sélection par Roulette**
```pascal
function SelectParentRoulette: TIndividual;  
var
  totalFitness, randomPoint, currentSum: Double;
  i: Integer;
begin
  // Calculer la fitness totale
  totalFitness := 0;
  for i := 0 to FPopulationSize - 1 do
    totalFitness := totalFitness + FPopulation[i].Fitness;

  // Choisir un point aléatoire
  randomPoint := Random * totalFitness;

  // Trouver l'individu correspondant
  currentSum := 0;
  for i := 0 to FPopulationSize - 1 do
  begin
    currentSum := currentSum + FPopulation[i].Fitness;
    if currentSum >= randomPoint then
    begin
      Result := FPopulation[i];
      Exit;
    end;
  end;

  // Par défaut, retourner le dernier
  Result := FPopulation[FPopulationSize - 1];
end;
```

**Sélection par Rang**
```pascal
function SelectParentRank: TIndividual;  
var
  totalRank, randomPoint, currentSum: Integer;
  i: Integer;
begin
  // La population est déjà triée par fitness
  // Rang 1 = meilleur, Rang N = moins bon

  // Somme des rangs: N + (N-1) + (N-2) + ... + 1 = N(N+1)/2
  totalRank := FPopulationSize * (FPopulationSize + 1) div 2;

  randomPoint := Random(totalRank);
  currentSum := 0;

  for i := 0 to FPopulationSize - 1 do
  begin
    currentSum := currentSum + (FPopulationSize - i);
    if currentSum >= randomPoint then
    begin
      Result := FPopulation[i];
      Exit;
    end;
  end;

  Result := FPopulation[FPopulationSize - 1];
end;
```

### 2. Croisement (Crossover)

**Croisement en Un Point (déjà implémenté)**
```
Parent 1: [A B C | D E F]  
Parent 2: [a b c | d e f]
          --------+-------
Enfant :  [A B C | d e f]
```

**Croisement en Deux Points**
```pascal
function CrossoverTwoPoint(const AParent1, AParent2: TIndividual): TIndividual;  
var
  i, point1, point2, temp: Integer;
begin
  SetLength(Result.Chromosome, FChromosomeLength);

  if Random < FCrossoverRate then
  begin
    // Choisir deux points de croisement
    point1 := Random(FChromosomeLength);
    point2 := Random(FChromosomeLength);

    // S'assurer que point1 < point2
    if point1 > point2 then
    begin
      temp := point1;
      point1 := point2;
      point2 := temp;
    end;

    for i := 0 to FChromosomeLength - 1 do
    begin
      if (i >= point1) and (i < point2) then
        Result.Chromosome[i] := AParent2.Chromosome[i]
      else
        Result.Chromosome[i] := AParent1.Chromosome[i];
    end;
  end
  else
  begin
    for i := 0 to FChromosomeLength - 1 do
      Result.Chromosome[i] := AParent1.Chromosome[i];
  end;

  Result.Fitness := 0;
end;
```

**Croisement Uniforme**
```pascal
function CrossoverUniform(const AParent1, AParent2: TIndividual): TIndividual;  
var
  i: Integer;
begin
  SetLength(Result.Chromosome, FChromosomeLength);

  for i := 0 to FChromosomeLength - 1 do
  begin
    // 50% de chance de prendre chaque parent
    if Random < 0.5 then
      Result.Chromosome[i] := AParent1.Chromosome[i]
    else
      Result.Chromosome[i] := AParent2.Chromosome[i];
  end;

  Result.Fitness := 0;
end;
```

### 3. Mutation

**Mutation Aléatoire (déjà implémentée)**
- Remplacer par une valeur aléatoire

**Mutation Gaussienne**
```pascal
procedure MutateGaussian(var AIndividual: TIndividual);  
var
  i: Integer;
  delta: Double;
begin
  for i := 0 to FChromosomeLength - 1 do
  begin
    if Random < FMutationRate then
    begin
      // Ajouter un bruit gaussien
      delta := RandG(0, 0.1); // Moyenne 0, écart-type 0.1
      AIndividual.Chromosome[i] := AIndividual.Chromosome[i] + delta;

      // Garder dans [0, 1]
      if AIndividual.Chromosome[i] < 0 then
        AIndividual.Chromosome[i] := 0;
      if AIndividual.Chromosome[i] > 1 then
        AIndividual.Chromosome[i] := 1;
    end;
  end;
end;
```

**Mutation par Inversion (pour problèmes combinatoires)**
```pascal
procedure MutateInversion(var AIndividual: TIndividual);  
var
  i, j, point1, point2, temp: Integer;
begin
  if Random < FMutationRate then
  begin
    point1 := Random(FChromosomeLength);
    point2 := Random(FChromosomeLength);

    if point1 > point2 then
    begin
      temp := point1;
      point1 := point2;
      point2 := temp;
    end;

    // Inverser la séquence entre point1 et point2
    i := point1;
    j := point2;
    while i < j do
    begin
      temp := Round(AIndividual.Chromosome[i]);
      AIndividual.Chromosome[i] := AIndividual.Chromosome[j];
      AIndividual.Chromosome[j] := temp;
      Inc(i);
      Dec(j);
    end;
  end;
end;
```

---

## Exemple Avancé : Problème du Voyageur de Commerce (TSP)

Le problème du voyageur de commerce consiste à trouver le plus court chemin visitant toutes les villes exactement une fois et revenant au point de départ.

```pascal
unit TSPGeneticAlgorithm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math;

type
  TCity = record
    X, Y: Double;
    Name: string;
  end;

  TCityArray = array of TCity;
  TRoute = array of Integer;  // Indices des villes dans l'ordre

  TTSPIndividual = record
    Route: TRoute;
    Distance: Double;
  end;

  TTSPGA = class
  private
    FCities: TCityArray;
    FPopulation: array of TTSPIndividual;
    FPopulationSize: Integer;
    FMutationRate: Double;
    FGeneration: Integer;
    FBestRoute: TTSPIndividual;

    function CalculateDistance(const ARoute: TRoute): Double;
    function GetCityDistance(const ACity1, ACity2: TCity): Double;
    procedure InitializePopulation;
    procedure EvaluatePopulation;
    function SelectParent: TTSPIndividual;
    function OrderCrossover(const AParent1, AParent2: TTSPIndividual): TTSPIndividual;
    procedure SwapMutation(var AIndividual: TTSPIndividual);
    procedure SortPopulation;
  public
    constructor Create(const ACities: TCityArray; APopulationSize: Integer);
    destructor Destroy; override;

    procedure EvolveOneGeneration;
    procedure Evolve(AGenerations: Integer);

    function GetBestRoute: TTSPIndividual;
    function GetAverageDistance: Double;

    property Generation: Integer read FGeneration;
    property MutationRate: Double read FMutationRate write FMutationRate;
  end;

implementation

constructor TTSPGA.Create(const ACities: TCityArray; APopulationSize: Integer);  
begin
  FCities := Copy(ACities);
  FPopulationSize := APopulationSize;
  FMutationRate := 0.02;
  FGeneration := 0;

  SetLength(FPopulation, FPopulationSize);

  Randomize;
  InitializePopulation;
  EvaluatePopulation;
end;

destructor TTSPGA.Destroy;  
var
  i: Integer;
begin
  for i := 0 to High(FPopulation) do
    SetLength(FPopulation[i].Route, 0);
  SetLength(FPopulation, 0);
  SetLength(FCities, 0);
  inherited;
end;

function TTSPGA.GetCityDistance(const ACity1, ACity2: TCity): Double;  
var
  dx, dy: Double;
begin
  dx := ACity2.X - ACity1.X;
  dy := ACity2.Y - ACity1.Y;
  Result := Sqrt(dx * dx + dy * dy);
end;

function TTSPGA.CalculateDistance(const ARoute: TRoute): Double;  
var
  i: Integer;
begin
  Result := 0;

  for i := 0 to High(ARoute) - 1 do
    Result := Result + GetCityDistance(FCities[ARoute[i]], FCities[ARoute[i + 1]]);

  // Retour à la ville de départ
  Result := Result + GetCityDistance(FCities[ARoute[High(ARoute)]], FCities[ARoute[0]]);
end;

procedure TTSPGA.InitializePopulation;  
var
  i, j, k, temp: Integer;
begin
  for i := 0 to FPopulationSize - 1 do
  begin
    SetLength(FPopulation[i].Route, Length(FCities));

    // Initialiser avec l'ordre séquentiel
    for j := 0 to High(FCities) do
      FPopulation[i].Route[j] := j;

    // Mélanger aléatoirement (algorithme de Fisher-Yates)
    for j := High(FCities) downto 1 do
    begin
      k := Random(j + 1);
      temp := FPopulation[i].Route[j];
      FPopulation[i].Route[j] := FPopulation[i].Route[k];
      FPopulation[i].Route[k] := temp;
    end;

    FPopulation[i].Distance := 0;
  end;
end;

procedure TTSPGA.EvaluatePopulation;  
var
  i: Integer;
begin
  for i := 0 to FPopulationSize - 1 do
    FPopulation[i].Distance := CalculateDistance(FPopulation[i].Route);

  SortPopulation;
  FBestRoute := FPopulation[0];
end;

procedure TTSPGA.SortPopulation;  
var
  i, j: Integer;
  temp: TTSPIndividual;
begin
  // Tri par distance croissante (meilleure = plus courte)
  for i := 0 to FPopulationSize - 2 do
    for j := i + 1 to FPopulationSize - 1 do
      if FPopulation[j].Distance < FPopulation[i].Distance then
      begin
        temp := FPopulation[i];
        FPopulation[i] := FPopulation[j];
        FPopulation[j] := temp;
      end;
end;

function TTSPGA.SelectParent: TTSPIndividual;  
var
  tournamentSize, i, randomIndex, bestIndex: Integer;
  bestDistance: Double;
begin
  tournamentSize := 5;
  bestIndex := Random(FPopulationSize);
  bestDistance := FPopulation[bestIndex].Distance;

  for i := 1 to tournamentSize - 1 do
  begin
    randomIndex := Random(FPopulationSize);
    if FPopulation[randomIndex].Distance < bestDistance then
    begin
      bestIndex := randomIndex;
      bestDistance := FPopulation[randomIndex].Distance;
    end;
  end;

  Result := FPopulation[bestIndex];
end;

function TTSPGA.OrderCrossover(const AParent1, AParent2: TTSPIndividual): TTSPIndividual;  
var
  i, j, point1, point2, temp, cityIndex: Integer;
  used: array of Boolean;
begin
  SetLength(Result.Route, Length(FCities));
  SetLength(used, Length(FCities));

  // Initialiser le tableau des villes utilisées
  for i := 0 to High(used) do
    used[i] := False;

  // Choisir deux points de croisement
  point1 := Random(Length(FCities));
  point2 := Random(Length(FCities));

  if point1 > point2 then
  begin
    temp := point1;
    point1 := point2;
    point2 := temp;
  end;

  // Copier le segment du parent 1
  for i := point1 to point2 do
  begin
    Result.Route[i] := AParent1.Route[i];
    used[AParent1.Route[i]] := True;
  end;

  // Remplir le reste avec les villes du parent 2 dans l'ordre
  j := 0;
  for i := 0 to High(Result.Route) do
  begin
    if (i < point1) or (i > point2) then
    begin
      // Trouver la prochaine ville non utilisée du parent 2
      while used[AParent2.Route[j]] do
        Inc(j);

      Result.Route[i] := AParent2.Route[j];
      used[AParent2.Route[j]] := True;
      Inc(j);
    end;
  end;

  Result.Distance := 0;
  SetLength(used, 0);
end;

procedure TTSPGA.SwapMutation(var AIndividual: TTSPIndividual);  
var
  i, j, temp: Integer;
begin
  if Random < FMutationRate then
  begin
    // Choisir deux positions aléatoires
    i := Random(Length(AIndividual.Route));
    j := Random(Length(AIndividual.Route));

    // Échanger les villes
    temp := AIndividual.Route[i];
    AIndividual.Route[i] := AIndividual.Route[j];
    AIndividual.Route[j] := temp;
  end;
end;

procedure TTSPGA.EvolveOneGeneration;  
var
  newPopulation: array of TTSPIndividual;
  i: Integer;
  parent1, parent2, child: TTSPIndividual;
  elitismCount: Integer;
begin
  elitismCount := Max(2, FPopulationSize div 20); // 5% d'élitisme
  SetLength(newPopulation, FPopulationSize);

  // Élitisme
  for i := 0 to elitismCount - 1 do
    newPopulation[i] := FPopulation[i];

  // Créer le reste de la population
  for i := elitismCount to FPopulationSize - 1 do
  begin
    parent1 := SelectParent;
    parent2 := SelectParent;

    child := OrderCrossover(parent1, parent2);
    SwapMutation(child);

    newPopulation[i] := child;
  end;

  FPopulation := newPopulation;
  EvaluatePopulation;
  Inc(FGeneration);
end;

procedure TTSPGA.Evolve(AGenerations: Integer);  
var
  i: Integer;
begin
  for i := 1 to AGenerations do
    EvolveOneGeneration;
end;

function TTSPGA.GetBestRoute: TTSPIndividual;  
begin
  Result := FBestRoute;
end;

function TTSPGA.GetAverageDistance: Double;  
var
  i: Integer;
  sum: Double;
begin
  sum := 0;
  for i := 0 to FPopulationSize - 1 do
    sum := sum + FPopulation[i].Distance;
  Result := sum / FPopulationSize;
end;

end.
```

### Programme de test pour le TSP

```pascal
program TestTSP;

{$mode objfpc}{$H+}

uses
  SysUtils, Math, TSPGeneticAlgorithm;

procedure CreateRandomCities(var ACities: TCityArray; ACount: Integer);  
var
  i: Integer;
begin
  SetLength(ACities, ACount);

  for i := 0 to ACount - 1 do
  begin
    ACities[i].X := Random * 100;
    ACities[i].Y := Random * 100;
    ACities[i].Name := Format('Ville_%d', [i + 1]);
  end;
end;

procedure PrintRoute(const ARoute: TTSPIndividual; const ACities: TCityArray);  
var
  i: Integer;
begin
  Write('Route: ');
  for i := 0 to High(ARoute.Route) do
  begin
    Write(ACities[ARoute.Route[i]].Name);
    if i < High(ARoute.Route) then
      Write(' → ');
  end;
  WriteLn(' → ', ACities[ARoute.Route[0]].Name);
  WriteLn(Format('Distance totale: %.2f', [ARoute.Distance]));
end;

var
  cities: TCityArray;
  ga: TTSPGA;
  i: Integer;
  best: TTSPIndividual;

begin
  WriteLn('========================================');
  WriteLn('  Problème du Voyageur de Commerce    ');
  WriteLn('  avec Algorithme Génétique            ');
  WriteLn('========================================');
  WriteLn;

  Randomize;

  // Créer 10 villes aléatoires
  CreateRandomCities(cities, 10);

  WriteLn('Villes à visiter:');
  for i := 0 to High(cities) do
    WriteLn(Format('  %s: (%.2f, %.2f)',
      [cities[i].Name, cities[i].X, cities[i].Y]));
  WriteLn;

  // Créer l'algorithme génétique
  ga := TTSPGA.Create(cities, 100);
  try
    ga.MutationRate := 0.02;

    WriteLn('Évolution en cours...');
    WriteLn;
    WriteLn('Génération | Meilleure Distance | Distance Moyenne');
    WriteLn('---------------------------------------------------');

    for i := 0 to 200 do
    begin
      if i mod 20 = 0 then
      begin
        best := ga.GetBestRoute;
        WriteLn(Format('%10d | %17.2f | %16.2f',
          [ga.Generation, best.Distance, ga.GetAverageDistance]));
      end;

      ga.EvolveOneGeneration;
    end;

    WriteLn;
    WriteLn('========================================');
    WriteLn('  MEILLEURE SOLUTION TROUVÉE           ');
    WriteLn('========================================');
    WriteLn;

    best := ga.GetBestRoute;
    PrintRoute(best, cities);

  finally
    ga.Free;
  end;

  SetLength(cities, 0);

  {$IFDEF WINDOWS}
  WriteLn;
  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
  {$ENDIF}
end.
```

---

## Encodage des Solutions

Le choix de l'encodage est crucial pour le succès d'un algorithme génétique.

### 1. Encodage Binaire

**Utilisation :** Problèmes d'optimisation discrète, sac à dos, allocation de ressources

```pascal
type
  TBinaryChromosome = array of Boolean;

// Exemple: sac à dos avec 5 objets
// [True, False, True, True, False] = prendre objets 1, 3 et 4

function DecodeBinary(const AChromosome: TBinaryChromosome): Integer;  
var
  i, value: Integer;
begin
  value := 0;
  for i := 0 to High(AChromosome) do
    if AChromosome[i] then
      value := value + (1 shl i);
  Result := value;
end;
```

### 2. Encodage Réel

**Utilisation :** Optimisation continue, réseaux de neurones, calibration de paramètres

```pascal
type
  TRealChromosome = array of Double;

// Exemple: paramètres d'une fonction
// [0.5, 2.3, -1.7] = x=0.5, y=2.3, z=-1.7

function ScaleToRange(AValue, AMin, AMax: Double): Double;  
begin
  Result := AMin + AValue * (AMax - AMin);
end;
```

### 3. Encodage Permutation

**Utilisation :** TSP, ordonnancement, planification

```pascal
type
  TPermutationChromosome = array of Integer;

// Exemple: ordre de visite
// [2, 0, 3, 1] = visiter 3ème, 1ère, 4ème, 2ème

function IsValidPermutation(const AChromosome: TPermutationChromosome): Boolean;  
var
  i, j: Integer;
begin
  Result := True;

  for i := 0 to High(AChromosome) do
    for j := i + 1 to High(AChromosome) do
      if AChromosome[i] = AChromosome[j] then
      begin
        Result := False;
        Exit;
      end;
end;
```

### 4. Encodage Arborescent

**Utilisation :** Programmation génétique, arbres de décision

```pascal
type
  TNodeType = (ntOperator, ntVariable, ntConstant);

  TTreeNode = class
    NodeType: TNodeType;
    Value: string;
    Left, Right: TTreeNode;
  end;

// Exemple: expression mathématique
// Arbre pour (x + 3) * y
//       *
//      / \
//     +   y
//    / \
//   x   3
```

---

## Exemple : Problème du Sac à Dos (Knapsack)

On doit remplir un sac à dos de capacité limitée avec des objets ayant chacun un poids et une valeur, en maximisant la valeur totale.

```pascal
program KnapsackGA;

{$mode objfpc}{$H+}

uses
  SysUtils, Math;

type
  TItem = record
    Name: string;
    Weight: Integer;
    Value: Integer;
  end;

  TItemArray = array of TItem;
  TBinaryChromosome = array of Boolean;

  TKnapsackIndividual = record
    Chromosome: TBinaryChromosome;
    TotalWeight: Integer;
    TotalValue: Integer;
    Fitness: Double;
  end;

var
  Items: TItemArray;
  MaxWeight: Integer;
  Population: array of TKnapsackIndividual;
  PopulationSize: Integer;
  MutationRate: Double;

procedure InitializeItems;  
begin
  SetLength(Items, 8);

  Items[0].Name := 'Ordinateur'; Items[0].Weight := 15; Items[0].Value := 800;
  Items[1].Name := 'Livre';      Items[1].Weight := 2;  Items[1].Value := 50;
  Items[2].Name := 'Appareil';   Items[2].Weight := 8;  Items[2].Value := 500;
  Items[3].Name := 'Vêtements';  Items[3].Weight := 5;  Items[3].Value := 100;
  Items[4].Name := 'Nourriture'; Items[4].Weight := 3;  Items[4].Value := 80;
  Items[5].Name := 'Eau';        Items[5].Weight := 4;  Items[5].Value := 90;
  Items[6].Name := 'Trousse';    Items[6].Weight := 1;  Items[6].Value := 30;
  Items[7].Name := 'Lampe';      Items[7].Weight := 2;  Items[7].Value := 60;

  MaxWeight := 20;
end;

procedure EvaluateIndividual(var AIndividual: TKnapsackIndividual);  
var
  i: Integer;
begin
  AIndividual.TotalWeight := 0;
  AIndividual.TotalValue := 0;

  for i := 0 to High(AIndividual.Chromosome) do
  begin
    if AIndividual.Chromosome[i] then
    begin
      AIndividual.TotalWeight := AIndividual.TotalWeight + Items[i].Weight;
      AIndividual.TotalValue := AIndividual.TotalValue + Items[i].Value;
    end;
  end;

  // Pénalité si dépasse la capacité
  if AIndividual.TotalWeight > MaxWeight then
    AIndividual.Fitness := 0
  else
    AIndividual.Fitness := AIndividual.TotalValue;
end;

procedure InitializePopulation;  
var
  i, j: Integer;
begin
  SetLength(Population, PopulationSize);

  for i := 0 to PopulationSize - 1 do
  begin
    SetLength(Population[i].Chromosome, Length(Items));

    for j := 0 to High(Items) do
      Population[i].Chromosome[j] := Random < 0.5;

    EvaluateIndividual(Population[i]);
  end;
end;

procedure SortPopulation;  
var
  i, j: Integer;
  temp: TKnapsackIndividual;
begin
  for i := 0 to PopulationSize - 2 do
    for j := i + 1 to PopulationSize - 1 do
      if Population[j].Fitness > Population[i].Fitness then
      begin
        temp := Population[i];
        Population[i] := Population[j];
        Population[j] := temp;
      end;
end;

function SelectParent: TKnapsackIndividual;  
var
  i, bestIndex: Integer;
  bestFitness: Double;
begin
  bestIndex := Random(PopulationSize);
  bestFitness := Population[bestIndex].Fitness;

  for i := 1 to 3 do
  begin
    if Population[Random(PopulationSize)].Fitness > bestFitness then
    begin
      bestIndex := Random(PopulationSize);
      bestFitness := Population[bestIndex].Fitness;
    end;
  end;

  Result := Population[bestIndex];
end;

function Crossover(const AParent1, AParent2: TKnapsackIndividual): TKnapsackIndividual;  
var
  i, point: Integer;
begin
  SetLength(Result.Chromosome, Length(Items));
  point := Random(Length(Items));

  for i := 0 to High(Items) do
  begin
    if i < point then
      Result.Chromosome[i] := AParent1.Chromosome[i]
    else
      Result.Chromosome[i] := AParent2.Chromosome[i];
  end;
end;

procedure Mutate(var AIndividual: TKnapsackIndividual);  
var
  i: Integer;
begin
  for i := 0 to High(AIndividual.Chromosome) do
    if Random < MutationRate then
      AIndividual.Chromosome[i] := not AIndividual.Chromosome[i];
end;

procedure EvolveOneGeneration;  
var
  newPopulation: array of TKnapsackIndividual;
  i: Integer;
  parent1, parent2, child: TKnapsackIndividual;
begin
  SetLength(newPopulation, PopulationSize);

  // Élitisme
  newPopulation[0] := Population[0];
  newPopulation[1] := Population[1];

  for i := 2 to PopulationSize - 1 do
  begin
    parent1 := SelectParent;
    parent2 := SelectParent;

    child := Crossover(parent1, parent2);
    Mutate(child);
    EvaluateIndividual(child);

    newPopulation[i] := child;
  end;

  Population := newPopulation;
  SortPopulation;
end;

procedure PrintSolution(const AIndividual: TKnapsackIndividual);  
var
  i: Integer;
begin
  WriteLn('Objets sélectionnés:');
  for i := 0 to High(AIndividual.Chromosome) do
    if AIndividual.Chromosome[i] then
      WriteLn(Format('  ✓ %s (Poids: %d, Valeur: %d)',
        [Items[i].Name, Items[i].Weight, Items[i].Value]));

  WriteLn;
  WriteLn(Format('Poids total: %d / %d kg', [AIndividual.TotalWeight, MaxWeight]));
  WriteLn(Format('Valeur totale: %d €', [AIndividual.TotalValue]));
end;

var
  i, generation: Integer;

begin
  WriteLn('========================================');
  WriteLn('  Problème du Sac à Dos               ');
  WriteLn('  Capacité: 20 kg                     ');
  WriteLn('========================================');
  WriteLn;

  Randomize;
  InitializeItems;

  WriteLn('Objets disponibles:');
  for i := 0 to High(Items) do
    WriteLn(Format('  %s: %d kg, %d €',
      [Items[i].Name, Items[i].Weight, Items[i].Value]));
  WriteLn;

  PopulationSize := 50;
  MutationRate := 0.05;

  InitializePopulation;
  SortPopulation;

  WriteLn('Évolution en cours...');
  WriteLn;

  for generation := 1 to 100 do
  begin
    EvolveOneGeneration;

    if generation mod 20 = 0 then
      WriteLn(Format('Génération %d: Meilleure valeur = %d €',
        [generation, Round(Population[0].Fitness)]));
  end;

  WriteLn;
  WriteLn('========================================');
  WriteLn('  MEILLEURE SOLUTION                   ');
  WriteLn('========================================');
  WriteLn;

  PrintSolution(Population[0]);

  {$IFDEF WINDOWS}
  WriteLn;
  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
  {$ENDIF}
end.
```

---

## Visualisation avec Lazarus

### Interface graphique pour observer l'évolution

```pascal
unit GAVisualization;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, GeneticAlgorithm;

type
  TFormGA = class(TForm)
    ButtonStart: TButton;
    ButtonStop: TButton;
    ButtonReset: TButton;
    ChartPanel: TPanel;
    ProgressBar1: TProgressBar;
    LabelGeneration: TLabel;
    LabelBestFitness: TLabel;
    LabelAvgFitness: TLabel;
    Timer1: TTimer;
    PaintBox1: TPaintBox;

    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
    procedure ButtonResetClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FGA: TGeneticAlgorithm;
    FFitnessHistory: array of Double;
    FRunning: Boolean;
    FMaxGenerations: Integer;

    procedure UpdateChart;
  public
  end;

var
  FormGA: TFormGA;

implementation

{$R *.lfm}

function SimpleTestFunction(const AChromosome: TChromosome): Double;  
var
  x: Double;
begin
  // Fonction test: maximiser f(x) = sin(10*x) * x
  x := AChromosome[0] * 3; // x entre 0 et 3
  Result := Sin(10 * x) * x;
end;

procedure TFormGA.FormCreate(Sender: TObject);  
begin
  FGA := TGeneticAlgorithm.Create(100, 1, @SimpleTestFunction);
  FGA.MutationRate := 0.05;
  FGA.CrossoverRate := 0.8;

  FMaxGenerations := 200;
  SetLength(FFitnessHistory, 0);
  FRunning := False;

  ProgressBar1.Max := FMaxGenerations;
  Timer1.Enabled := False;
end;

procedure TFormGA.FormDestroy(Sender: TObject);  
begin
  FGA.Free;
  SetLength(FFitnessHistory, 0);
end;

procedure TFormGA.ButtonStartClick(Sender: TObject);  
begin
  FRunning := True;
  Timer1.Enabled := True;
  ButtonStart.Enabled := False;
  ButtonStop.Enabled := True;
end;

procedure TFormGA.ButtonStopClick(Sender: TObject);  
begin
  FRunning := False;
  Timer1.Enabled := False;
  ButtonStart.Enabled := True;
  ButtonStop.Enabled := False;
end;

procedure TFormGA.ButtonResetClick(Sender: TObject);  
begin
  FGA.Free;
  FGA := TGeneticAlgorithm.Create(100, 1, @SimpleTestFunction);
  FGA.MutationRate := 0.05;
  FGA.CrossoverRate := 0.8;

  SetLength(FFitnessHistory, 0);
  ProgressBar1.Position := 0;
  PaintBox1.Invalidate;

  LabelGeneration.Caption := 'Génération: 0';
  LabelBestFitness.Caption := 'Meilleur: 0.00';
  LabelAvgFitness.Caption := 'Moyenne: 0.00';
end;

procedure TFormGA.Timer1Timer(Sender: TObject);  
var
  best: TIndividual;
begin
  if FGA.Generation >= FMaxGenerations then
  begin
    ButtonStopClick(nil);
    Exit;
  end;

  FGA.EvolveOneGeneration;

  // Enregistrer l'historique
  SetLength(FFitnessHistory, Length(FFitnessHistory) + 1);
  best := FGA.GetBestIndividual;
  FFitnessHistory[High(FFitnessHistory)] := best.Fitness;

  // Mettre à jour l'interface
  ProgressBar1.Position := FGA.Generation;
  LabelGeneration.Caption := Format('Génération: %d', [FGA.Generation]);
  LabelBestFitness.Caption := Format('Meilleur: %.4f', [best.Fitness]);
  LabelAvgFitness.Caption := Format('Moyenne: %.4f', [FGA.GetAverageFitness]);

  PaintBox1.Invalidate;
end;

procedure TFormGA.PaintBox1Paint(Sender: TObject);  
var
  i: Integer;
  x1, y1, x2, y2: Integer;
  maxFitness, minFitness, scale: Double;
  w, h: Integer;
begin
  w := PaintBox1.Width;
  h := PaintBox1.Height;

  // Fond blanc
  PaintBox1.Canvas.Brush.Color := clWhite;
  PaintBox1.Canvas.FillRect(0, 0, w, h);

  if Length(FFitnessHistory) < 2 then Exit;

  // Trouver min et max
  maxFitness := FFitnessHistory[0];
  minFitness := FFitnessHistory[0];

  for i := 1 to High(FFitnessHistory) do
  begin
    if FFitnessHistory[i] > maxFitness then
      maxFitness := FFitnessHistory[i];
    if FFitnessHistory[i] < minFitness then
      minFitness := FFitnessHistory[i];
  end;

  if maxFitness = minFitness then
    scale := 1
  else
    scale := (h - 20) / (maxFitness - minFitness);

  // Dessiner la courbe
  PaintBox1.Canvas.Pen.Color := clBlue;
  PaintBox1.Canvas.Pen.Width := 2;

  for i := 0 to High(FFitnessHistory) - 1 do
  begin
    x1 := Round((i / Length(FFitnessHistory)) * w);
    y1 := h - 10 - Round((FFitnessHistory[i] - minFitness) * scale);

    x2 := Round(((i + 1) / Length(FFitnessHistory)) * w);
    y2 := h - 10 - Round((FFitnessHistory[i + 1] - minFitness) * scale);

    PaintBox1.Canvas.Line(x1, y1, x2, y2);
  end;

  // Afficher les valeurs
  PaintBox1.Canvas.Font.Size := 8;
  PaintBox1.Canvas.TextOut(5, 5, Format('Max: %.2f', [maxFitness]));
  PaintBox1.Canvas.TextOut(5, h - 20, Format('Min: %.2f', [minFitness]));
end;

end.
```

---

## Paramètres et Réglage

### Taille de la population

**Petite population (10-50)**
- ✅ Rapide
- ✅ Moins de mémoire
- ❌ Risque de convergence prématurée
- ❌ Diversité limitée

**Grande population (100-500)**
- ✅ Plus de diversité
- ✅ Meilleure exploration
- ❌ Plus lent
- ❌ Plus de mémoire

**Recommandation :** 50-100 pour la plupart des problèmes

### Taux de mutation

**Faible (0.001 - 0.01)**
- ✅ Stabilité
- ❌ Peut stagner

**Moyen (0.01 - 0.1)**
- ✅ Bon équilibre
- ✅ Convient à la plupart des cas

**Élevé (0.1 - 0.5)**
- ✅ Grande exploration
- ❌ Instabilité
- ❌ Perd les bonnes solutions

**Recommandation :** 0.01 - 0.05

### Taux de croisement

**Recommandation :** 0.6 - 0.9

Un taux élevé favorise l'exploitation des bonnes solutions.

### Critères d'arrêt

```pascal
type
  TStopCriterion = (
    scMaxGenerations,    // Nombre maximum de générations
    scTargetFitness,     // Fitness cible atteinte
    scNoImprovement,     // Pas d'amélioration depuis N générations
    scTimeLimit          // Limite de temps
  );

function ShouldStop(AGA: TGeneticAlgorithm;
                    ACriterion: TStopCriterion;
                    AValue: Double): Boolean;
var
  noImprovementCount: Integer;
  startTime: TDateTime;
begin
  Result := False;

  case ACriterion of
    scMaxGenerations:
      Result := AGA.Generation >= Round(AValue);

    scTargetFitness:
      Result := AGA.GetBestIndividual.Fitness >= AValue;

    scNoImprovement:
      // Implémentation simplifiée
      Result := False; // À implémenter avec historique

    scTimeLimit:
      // À implémenter avec chronomètre
      Result := False;
  end;
end;
```

---

## Optimisations et Techniques Avancées

### 1. Adaptive Mutation Rate

Ajuster le taux de mutation selon la convergence :

```pascal
procedure AdaptMutationRate(var AGA: TGeneticAlgorithm);  
var
  diversity: Double;
begin
  diversity := CalculateDiversity(AGA);

  if diversity < 0.1 then
    // Population homogène → augmenter mutation
    AGA.MutationRate := Min(0.5, AGA.MutationRate * 1.5)
  else if diversity > 0.8 then
    // Population très diverse → réduire mutation
    AGA.MutationRate := Max(0.001, AGA.MutationRate * 0.8);
end;

function CalculateDiversity(AGA: TGeneticAlgorithm): Double;  
var
  i, j, k: Integer;
  differences: Integer;
  total: Integer;
  pop: TPopulation;
begin
  pop := AGA.GetPopulation;
  differences := 0;
  total := 0;

  // Comparer tous les pairs d'individus
  for i := 0 to Length(pop) - 2 do
    for j := i + 1 to Length(pop) - 1 do
    begin
      for k := 0 to High(pop[i].Chromosome) do
      begin
        if Abs(pop[i].Chromosome[k] - pop[j].Chromosome[k]) > 0.01 then
          Inc(differences);
        Inc(total);
      end;
    end;

  if total = 0 then
    Result := 0
  else
    Result := differences / total;
end;
```

### 2. Island Model (Parallélisation)

Exécuter plusieurs populations en parallèle avec migration périodique :

```pascal
unit IslandGA;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GeneticAlgorithm;

type
  TIslandGA = class
  private
    FIslands: array of TGeneticAlgorithm;
    FIslandCount: Integer;
    FMigrationInterval: Integer;
    FMigrationSize: Integer;

    procedure MigrateIndividuals;
  public
    constructor Create(AIslandCount, APopulationSize, AChromosomeLength: Integer;
                       AFitnessFunction: TFitnessFunction);
    destructor Destroy; override;

    procedure EvolveOneGeneration;
    procedure Evolve(AGenerations: Integer);
    function GetGlobalBest: TIndividual;

    property MigrationInterval: Integer read FMigrationInterval write FMigrationInterval;
    property MigrationSize: Integer read FMigrationSize write FMigrationSize;
  end;

implementation

constructor TIslandGA.Create(AIslandCount, APopulationSize, AChromosomeLength: Integer;
                             AFitnessFunction: TFitnessFunction);
var
  i: Integer;
begin
  FIslandCount := AIslandCount;
  FMigrationInterval := 10;  // Migration toutes les 10 générations
  FMigrationSize := 2;       // Migrer 2 meilleurs individus

  SetLength(FIslands, FIslandCount);

  for i := 0 to FIslandCount - 1 do
    FIslands[i] := TGeneticAlgorithm.Create(APopulationSize, AChromosomeLength, AFitnessFunction);
end;

destructor TIslandGA.Destroy;  
var
  i: Integer;
begin
  for i := 0 to FIslandCount - 1 do
    FIslands[i].Free;
  SetLength(FIslands, 0);
  inherited;
end;

procedure TIslandGA.MigrateIndividuals;  
var
  i, j, nextIsland: Integer;
  migrants: array of TIndividual;
begin
  for i := 0 to FIslandCount - 1 do
  begin
    // Prendre les meilleurs de cette île
    SetLength(migrants, FMigrationSize);
    for j := 0 to FMigrationSize - 1 do
      migrants[j] := FIslands[i].GetPopulation[j];

    // Envoyer vers l'île suivante (topologie en anneau)
    nextIsland := (i + 1) mod FIslandCount;

    // Remplacer les pires de l'île suivante
    for j := 0 to FMigrationSize - 1 do
      FIslands[nextIsland].ReplaceIndividual(
        FIslands[nextIsland].PopulationSize - 1 - j,
        migrants[j]
      );
  end;

  SetLength(migrants, 0);
end;

procedure TIslandGA.EvolveOneGeneration;  
var
  i: Integer;
begin
  // Faire évoluer chaque île
  for i := 0 to FIslandCount - 1 do
    FIslands[i].EvolveOneGeneration;

  // Migration périodique
  if (FIslands[0].Generation mod FMigrationInterval = 0) and
     (FIslands[0].Generation > 0) then
    MigrateIndividuals;
end;

procedure TIslandGA.Evolve(AGenerations: Integer);  
var
  i: Integer;
begin
  for i := 1 to AGenerations do
    EvolveOneGeneration;
end;

function TIslandGA.GetGlobalBest: TIndividual;  
var
  i: Integer;
  best, candidate: TIndividual;
begin
  best := FIslands[0].GetBestIndividual;

  for i := 1 to FIslandCount - 1 do
  begin
    candidate := FIslands[i].GetBestIndividual;
    if candidate.Fitness > best.Fitness then
      best := candidate;
  end;

  Result := best;
end;

end.
```

### 3. Hybridation avec Recherche Locale

Améliorer les solutions trouvées avec une recherche locale (hill climbing) :

```pascal
procedure LocalSearch(var AIndividual: TIndividual;
                      AFitnessFunction: TFitnessFunction;
                      AMaxIterations: Integer);
var
  i, j: Integer;
  improved: Boolean;
  currentFitness, newFitness: Double;
  backup: TGene;
begin
  currentFitness := AFitnessFunction(AIndividual.Chromosome);

  for i := 1 to AMaxIterations do
  begin
    improved := False;

    // Essayer de modifier chaque gène
    for j := 0 to High(AIndividual.Chromosome) do
    begin
      backup := AIndividual.Chromosome[j];

      // Essayer une petite modification
      AIndividual.Chromosome[j] := AIndividual.Chromosome[j] + (Random - 0.5) * 0.1;

      // Garder dans [0, 1]
      if AIndividual.Chromosome[j] < 0 then AIndividual.Chromosome[j] := 0;
      if AIndividual.Chromosome[j] > 1 then AIndividual.Chromosome[j] := 1;

      newFitness := AFitnessFunction(AIndividual.Chromosome);

      if newFitness > currentFitness then
      begin
        currentFitness := newFitness;
        improved := True;
      end
      else
        AIndividual.Chromosome[j] := backup; // Restaurer
    end;

    if not improved then
      Break; // Pas d'amélioration possible
  end;

  AIndividual.Fitness := currentFitness;
end;

// Utilisation dans l'AG
procedure EvolveWithLocalSearch(AGA: TGeneticAlgorithm);  
var
  i: Integer;
  pop: TPopulation;
begin
  AGA.EvolveOneGeneration;

  // Appliquer la recherche locale aux 10% meilleurs
  pop := AGA.GetPopulation;
  for i := 0 to (AGA.PopulationSize div 10) - 1 do
    LocalSearch(pop[i], AGA.FitnessFunction, 20);
end;
```

---

## Algorithmes Génétiques Multi-Objectifs

Parfois, on doit optimiser plusieurs objectifs contradictoires simultanément (ex: minimiser le coût ET maximiser la qualité).

### NSGA-II (Non-dominated Sorting Genetic Algorithm)

```pascal
unit MultiObjectiveGA;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, Generics.Collections;

type
  TObjectiveValues = array of Double;

  TMOIndividual = record
    Chromosome: TChromosome;
    Objectives: TObjectiveValues;
    Rank: Integer;
    CrowdingDistance: Double;
  end;

  TMOPopulation = array of TMOIndividual;
  TObjectiveFunction = function(const AChromosome: TChromosome): Double;

  TNSGA2 = class
  private
    FPopulation: TMOPopulation;
    FObjectiveFunctions: array of TObjectiveFunction;
    FPopulationSize: Integer;
    FChromosomeLength: Integer;

    procedure EvaluateObjectives;
    procedure NonDominatedSort;
    procedure CalculateCrowdingDistance;
    function Dominates(const AIndiv1, AIndiv2: TMOIndividual): Boolean;
  public
    constructor Create(APopulationSize, AChromosomeLength: Integer);
    destructor Destroy; override;

    procedure AddObjective(AFunction: TObjectiveFunction);
    procedure EvolveOneGeneration;
    function GetParetoFront: TMOPopulation;
  end;

implementation

constructor TNSGA2.Create(APopulationSize, AChromosomeLength: Integer);  
begin
  FPopulationSize := APopulationSize;
  FChromosomeLength := AChromosomeLength;
  SetLength(FPopulation, FPopulationSize);
end;

destructor TNSGA2.Destroy;  
begin
  SetLength(FPopulation, 0);
  SetLength(FObjectiveFunctions, 0);
  inherited;
end;

procedure TNSGA2.AddObjective(AFunction: TObjectiveFunction);  
begin
  SetLength(FObjectiveFunctions, Length(FObjectiveFunctions) + 1);
  FObjectiveFunctions[High(FObjectiveFunctions)] := AFunction;
end;

function TNSGA2.Dominates(const AIndiv1, AIndiv2: TMOIndividual): Boolean;  
var
  i: Integer;
  betterInOne, worseInOne: Boolean;
begin
  betterInOne := False;
  worseInOne := False;

  for i := 0 to High(AIndiv1.Objectives) do
  begin
    if AIndiv1.Objectives[i] > AIndiv2.Objectives[i] then
      betterInOne := True
    else if AIndiv1.Objectives[i] < AIndiv2.Objectives[i] then
      worseInOne := True;
  end;

  // Domine si meilleur dans au moins un objectif et pas pire dans les autres
  Result := betterInOne and (not worseInOne);
end;

procedure TNSGA2.EvaluateObjectives;  
var
  i, j: Integer;
begin
  for i := 0 to FPopulationSize - 1 do
  begin
    SetLength(FPopulation[i].Objectives, Length(FObjectiveFunctions));

    for j := 0 to High(FObjectiveFunctions) do
      FPopulation[i].Objectives[j] := FObjectiveFunctions[j](FPopulation[i].Chromosome);
  end;
end;

procedure TNSGA2.NonDominatedSort;  
var
  i, j: Integer;
  currentRank: Integer;
  dominationCount: array of Integer;
  dominatedBy: array of TList<Integer>;
  front: TList<Integer>;
  nextFront: TList<Integer>;
  p, q: Integer;
begin
  SetLength(dominationCount, FPopulationSize);
  SetLength(dominatedBy, FPopulationSize);

  for i := 0 to FPopulationSize - 1 do
  begin
    dominationCount[i] := 0;
    dominatedBy[i] := TList<Integer>.Create;
  end;

  // Calculer les relations de domination
  for i := 0 to FPopulationSize - 2 do
    for j := i + 1 to FPopulationSize - 1 do
    begin
      if Dominates(FPopulation[i], FPopulation[j]) then
      begin
        dominatedBy[i].Add(j);
        Inc(dominationCount[j]);
      end
      else if Dominates(FPopulation[j], FPopulation[i]) then
      begin
        dominatedBy[j].Add(i);
        Inc(dominationCount[i]);
      end;
    end;

  // Trier par fronts
  currentRank := 0;
  front := TList<Integer>.Create;

  try
    // Premier front : non dominés
    for i := 0 to FPopulationSize - 1 do
      if dominationCount[i] = 0 then
      begin
        FPopulation[i].Rank := currentRank;
        front.Add(i);
      end;

    // Fronts suivants
    while front.Count > 0 do
    begin
      nextFront := TList<Integer>.Create;
      try
        for p in front do
          for q in dominatedBy[p] do
          begin
            Dec(dominationCount[q]);
            if dominationCount[q] = 0 then
            begin
              FPopulation[q].Rank := currentRank + 1;
              nextFront.Add(q);
            end;
          end;

        front.Free;
        front := nextFront;
        Inc(currentRank);
      except
        nextFront.Free;
        raise;
      end;
    end;
  finally
    front.Free;
    for i := 0 to FPopulationSize - 1 do
      dominatedBy[i].Free;
    SetLength(dominatedBy, 0);
  end;
end;

procedure TNSGA2.CalculateCrowdingDistance;  
var
  i, j, objIndex: Integer;
  sortedIndices: array of Integer;
  temp: Integer;
  minObj, maxObj: Double;
begin
  // Initialiser à 0
  for i := 0 to FPopulationSize - 1 do
    FPopulation[i].CrowdingDistance := 0;

  // Pour chaque objectif
  for objIndex := 0 to High(FObjectiveFunctions) do
  begin
    // Trier par cet objectif
    SetLength(sortedIndices, FPopulationSize);
    for i := 0 to FPopulationSize - 1 do
      sortedIndices[i] := i;

    for i := 0 to FPopulationSize - 2 do
      for j := i + 1 to FPopulationSize - 1 do
        if FPopulation[sortedIndices[j]].Objectives[objIndex] <
           FPopulation[sortedIndices[i]].Objectives[objIndex] then
        begin
          temp := sortedIndices[i];
          sortedIndices[i] := sortedIndices[j];
          sortedIndices[j] := temp;
        end;

    // Bornes infinies pour les extrêmes
    FPopulation[sortedIndices[0]].CrowdingDistance := Infinity;
    FPopulation[sortedIndices[FPopulationSize - 1]].CrowdingDistance := Infinity;

    // Calculer pour les autres
    minObj := FPopulation[sortedIndices[0]].Objectives[objIndex];
    maxObj := FPopulation[sortedIndices[FPopulationSize - 1]].Objectives[objIndex];

    if maxObj > minObj then
      for i := 1 to FPopulationSize - 2 do
        FPopulation[sortedIndices[i]].CrowdingDistance :=
          FPopulation[sortedIndices[i]].CrowdingDistance +
          (FPopulation[sortedIndices[i + 1]].Objectives[objIndex] -
           FPopulation[sortedIndices[i - 1]].Objectives[objIndex]) /
          (maxObj - minObj);
  end;
end;

procedure TNSGA2.EvolveOneGeneration;  
begin
  NonDominatedSort;
  CalculateCrowdingDistance;
  // Sélection, croisement, mutation (à implémenter)
end;

function TNSGA2.GetParetoFront: TMOPopulation;  
var
  i, count: Integer;
begin
  count := 0;

  for i := 0 to FPopulationSize - 1 do
    if FPopulation[i].Rank = 0 then
      Inc(count);

  SetLength(Result, count);
  count := 0;

  for i := 0 to FPopulationSize - 1 do
    if FPopulation[i].Rank = 0 then
    begin
      Result[count] := FPopulation[i];
      Inc(count);
    end;
end;

end.
```

---

## Applications Pratiques Multi-Plateformes

### Gestion des chemins et ressources

```pascal
unit GAPlatformUtils;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

function GetDataPath: string;  
function GetResultsPath: string;  
procedure SaveResults(const AFileName: string; const AData: string);

implementation

function GetDataPath: string;  
begin
  {$IFDEF WINDOWS}
  Result := ExtractFilePath(ParamStr(0)) + 'data\';
  {$ENDIF}
  {$IFDEF LINUX}
  Result := GetEnvironmentVariable('HOME') + '/.ga_app/data/';
  {$ENDIF}

  if not DirectoryExists(Result) then
    ForceDirectories(Result);
end;

function GetResultsPath: string;  
begin
  {$IFDEF WINDOWS}
  Result := ExtractFilePath(ParamStr(0)) + 'results\';
  {$ENDIF}
  {$IFDEF LINUX}
  Result := GetEnvironmentVariable('HOME') + '/.ga_app/results/';
  {$ENDIF}

  if not DirectoryExists(Result) then
    ForceDirectories(Result);
end;

procedure SaveResults(const AFileName: string; const AData: string);  
var
  fullPath: string;
  f: TextFile;
begin
  fullPath := GetResultsPath + AFileName;

  AssignFile(f, fullPath);
  try
    Rewrite(f);
    Write(f, AData);
  finally
    CloseFile(f);
  end;
end;

end.
```

### Exemple : Optimisation de portefeuille d'investissement

```pascal
program PortfolioOptimization;

{$mode objfpc}{$H+}

uses
  SysUtils, Math, GeneticAlgorithm;

type
  TStock = record
    Name: string;
    ExpectedReturn: Double;  // Rendement attendu annuel
    Risk: Double;            // Volatilité (écart-type)
  end;

var
  Stocks: array of TStock;
  TotalBudget: Double;

procedure InitializeStocks;  
begin
  SetLength(Stocks, 5);

  Stocks[0].Name := 'Actions Tech';   Stocks[0].ExpectedReturn := 0.15; Stocks[0].Risk := 0.25;
  Stocks[1].Name := 'Obligations';    Stocks[1].ExpectedReturn := 0.05; Stocks[1].Risk := 0.05;
  Stocks[2].Name := 'Immobilier';     Stocks[2].ExpectedReturn := 0.10; Stocks[2].Risk := 0.15;
  Stocks[3].Name := 'Matières 1ères'; Stocks[3].ExpectedReturn := 0.08; Stocks[3].Risk := 0.20;
  Stocks[4].Name := 'Actions Value';  Stocks[4].ExpectedReturn := 0.12; Stocks[4].Risk := 0.18;

  TotalBudget := 100000; // 100 000 €
end;

// Fonction fitness : maximiser le ratio de Sharpe
// (rendement - taux sans risque) / risque
function PortfolioFitness(const AChromosome: TChromosome): Double;  
var
  i: Integer;
  totalAllocation, expectedReturn, portfolioRisk: Double;
  allocations: array of Double;
  riskFreeRate: Double;
begin
  riskFreeRate := 0.02; // 2% taux sans risque

  // Normaliser pour que la somme = 1
  SetLength(allocations, Length(Stocks));
  totalAllocation := 0;

  for i := 0 to High(AChromosome) do
    totalAllocation := totalAllocation + AChromosome[i];

  if totalAllocation = 0 then
  begin
    Result := 0;
    Exit;
  end;

  for i := 0 to High(allocations) do
    allocations[i] := AChromosome[i] / totalAllocation;

  // Calculer le rendement attendu du portefeuille
  expectedReturn := 0;
  for i := 0 to High(allocations) do
    expectedReturn := expectedReturn + allocations[i] * Stocks[i].ExpectedReturn;

  // Calculer le risque du portefeuille (simplifié)
  portfolioRisk := 0;
  for i := 0 to High(allocations) do
    portfolioRisk := portfolioRisk + Power(allocations[i] * Stocks[i].Risk, 2);
  portfolioRisk := Sqrt(portfolioRisk);

  // Ratio de Sharpe
  if portfolioRisk > 0 then
    Result := (expectedReturn - riskFreeRate) / portfolioRisk
  else
    Result := 0;

  SetLength(allocations, 0);
end;

procedure PrintPortfolio(const AChromosome: TChromosome);  
var
  i: Integer;
  totalAllocation, allocation: Double;
  expectedReturn, portfolioRisk: Double;
begin
  totalAllocation := 0;
  for i := 0 to High(AChromosome) do
    totalAllocation := totalAllocation + AChromosome[i];

  WriteLn('Allocation du portefeuille:');
  WriteLn('---------------------------');

  expectedReturn := 0;
  portfolioRisk := 0;

  for i := 0 to High(AChromosome) do
  begin
    allocation := (AChromosome[i] / totalAllocation) * 100;
    WriteLn(Format('  %s: %.1f%% (%.0f €)',
      [Stocks[i].Name, allocation, (AChromosome[i] / totalAllocation) * TotalBudget]));

    expectedReturn := expectedReturn + (AChromosome[i] / totalAllocation) * Stocks[i].ExpectedReturn;
    portfolioRisk := portfolioRisk + Power((AChromosome[i] / totalAllocation) * Stocks[i].Risk, 2);
  end;

  portfolioRisk := Sqrt(portfolioRisk);

  WriteLn;
  WriteLn(Format('Rendement attendu: %.2f%%', [expectedReturn * 100]));
  WriteLn(Format('Risque (volatilité): %.2f%%', [portfolioRisk * 100]));
  WriteLn(Format('Ratio de Sharpe: %.2f', [PortfolioFitness(AChromosome)]));
end;

var
  ga: TGeneticAlgorithm;
  i: Integer;
  best: TIndividual;

begin
  WriteLn('=========================================');
  WriteLn('  Optimisation de Portefeuille          ');
  WriteLn('  Budget: 100 000 €                     ');
  WriteLn('=========================================');
  WriteLn;

  Randomize;
  InitializeStocks;

  WriteLn('Actifs disponibles:');
  for i := 0 to High(Stocks) do
    WriteLn(Format('  %s: Rendement %.1f%%, Risque %.1f%%',
      [Stocks[i].Name, Stocks[i].ExpectedReturn * 100, Stocks[i].Risk * 100]));
  WriteLn;

  // Créer l'AG : 5 gènes (un par actif)
  ga := TGeneticAlgorithm.Create(100, Length(Stocks), @PortfolioFitness);
  try
    ga.MutationRate := 0.1;
    ga.CrossoverRate := 0.8;
    ga.ElitismCount := 5;

    WriteLn('Optimisation en cours...');
    WriteLn;
    WriteLn('Génération | Ratio de Sharpe');
    WriteLn('------------------------------');

    for i := 0 to 100 do
    begin
      if i mod 20 = 0 then
      begin
        best := ga.GetBestIndividual;
        WriteLn(Format('%10d | %15.4f', [ga.Generation, best.Fitness]));
      end;

      ga.EvolveOneGeneration;
    end;

    WriteLn;
    WriteLn('=========================================');
    WriteLn('  PORTEFEUILLE OPTIMAL                   ');
    WriteLn('=========================================');
    WriteLn;

    best := ga.GetBestIndividual;
    PrintPortfolio(best.Chromosome);

  finally
    ga.Free;
  end;

  SetLength(Stocks, 0);

  {$IFDEF WINDOWS}
  WriteLn;
  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
  {$ENDIF}
end.
```

---

## Conseils Pratiques et Debugging

### 1. Vérifier la fonction fitness

```pascal
procedure TestFitnessFunction;  
var
  testChromosome: TChromosome;
  fitness: Double;
begin
  SetLength(testChromosome, 3);

  // Test avec valeurs connues
  testChromosome[0] := 0.5;
  testChromosome[1] := 0.3;
  testChromosome[2] := 0.2;

  fitness := MyFitnessFunction(testChromosome);

  WriteLn('Test fitness:');
  WriteLn('  Chromosome: ', testChromosome[0]:0:2, ', ',
                           testChromosome[1]:0:2, ', ',
                           testChromosome[2]:0:2);
  WriteLn('  Fitness: ', fitness:0:4);

  // Vérifier que le fitness est cohérent
  Assert(fitness >= 0, 'Fitness doit être positif');
  Assert(not IsNaN(fitness), 'Fitness ne doit pas être NaN');
end;
```

### 2. Logger l'évolution

```pascal
unit GALogger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TGALogger = class
  private
    FLogFile: TextFile;
    FEnabled: Boolean;
  public
    constructor Create(const AFileName: string);
    destructor Destroy; override;

    procedure LogGeneration(AGeneration: Integer; ABestFitness, AAvgFitness: Double);
    procedure LogBestIndividual(const AIndividual: TIndividual);

    property Enabled: Boolean read FEnabled write FEnabled;
  end;

implementation

constructor TGALogger.Create(const AFileName: string);  
begin
  FEnabled := True;
  AssignFile(FLogFile, AFileName);
  Rewrite(FLogFile);
  WriteLn(FLogFile, 'Generation,BestFitness,AvgFitness,Timestamp');
end;

destructor TGALogger.Destroy;  
begin
  if FEnabled then
    CloseFile(FLogFile);
  inherited;
end;

procedure TGALogger.LogGeneration(AGeneration: Integer; ABestFitness, AAvgFitness: Double);  
begin
  if not FEnabled then Exit;

  WriteLn(FLogFile, Format('%d,%.6f,%.6f,%s',
    [AGeneration, ABestFitness, AAvgFitness, FormatDateTime('yyyy-mm-dd hh:nn:ss', Now)]));
  Flush(FLogFile);
end;

procedure TGALogger.LogBestIndividual(const AIndividual: TIndividual);  
var
  i: Integer;
begin
  if not FEnabled then Exit;

  Write(FLogFile, 'Best: ');
  for i := 0 to High(AIndividual.Chromosome) do
  begin
    Write(FLogFile, AIndividual.Chromosome[i]:0:4);
    if i < High(AIndividual.Chromosome) then
      Write(FLogFile, ',');
  end;
  WriteLn(FLogFile);
  Flush(FLogFile);
end;

end.
```

### 3. Détecter la convergence prématurée

```pascal
function DetectPrematureConvergence(AGA: TGeneticAlgorithm): Boolean;  
var
  diversity: Double;
  avgFitness, bestFitness: Double;
begin
  diversity := CalculateDiversity(AGA);
  bestFitness := AGA.GetBestIndividual.Fitness;
  avgFitness := AGA.GetAverageFitness;

  // Convergence prématurée si:
  // 1. Diversité très faible
  // 2. Fitness moyenne proche de la meilleure
  Result := (diversity < 0.05) and (avgFitness > bestFitness * 0.95);

  if Result then
    WriteLn('⚠ ALERTE: Convergence prématurée détectée!');
end;

procedure HandlePrematureConvergence(var AGA: TGeneticAlgorithm);  
var
  i, j: Integer;
  pop: TPopulation;
begin
  WriteLn('Application de mesures correctives...');

  // Solution 1: Augmenter la mutation
  AGA.MutationRate := Min(0.5, AGA.MutationRate * 2);
  WriteLn('  - Taux de mutation augmenté à ', AGA.MutationRate:0:3);

  // Solution 2: Réintroduire de la diversité
  pop := AGA.GetPopulation;

  // Garder les 10% meilleurs, randomiser le reste
  for i := (AGA.PopulationSize div 10) to AGA.PopulationSize - 1 do
    for j := 0 to High(pop[i].Chromosome) do
      pop[i].Chromosome[j] := Random;

  WriteLn('  - ', (AGA.PopulationSize * 9 div 10), ' individus réinitialisés');
end;
```

### 4. Profiling des performances

```pascal
unit GAProfiler;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, DateUtils;

type
  TGAProfiler = class
  private
    FStartTime: TDateTime;
    FFitnessEvaluations: Int64;
    FGenerationTimes: array of Int64; // en millisecondes

  public
    constructor Create;

    procedure StartGeneration;
    procedure EndGeneration;
    procedure RecordFitnessEvaluation;

    function GetAverageGenerationTime: Double;
    function GetEvaluationsPerSecond: Double;
    procedure PrintStats;
  end;

implementation

constructor TGAProfiler.Create;  
begin
  FStartTime := Now;
  FFitnessEvaluations := 0;
  SetLength(FGenerationTimes, 0);
end;

procedure TGAProfiler.StartGeneration;  
begin
  FStartTime := Now;
end;

procedure TGAProfiler.EndGeneration;  
var
  elapsed: Int64;
begin
  elapsed := MilliSecondsBetween(Now, FStartTime);
  SetLength(FGenerationTimes, Length(FGenerationTimes) + 1);
  FGenerationTimes[High(FGenerationTimes)] := elapsed;
end;

procedure TGAProfiler.RecordFitnessEvaluation;  
begin
  Inc(FFitnessEvaluations);
end;

function TGAProfiler.GetAverageGenerationTime: Double;  
var
  i: Integer;
  sum: Int64;
begin
  if Length(FGenerationTimes) = 0 then
  begin
    Result := 0;
    Exit;
  end;

  sum := 0;
  for i := 0 to High(FGenerationTimes) do
    sum := sum + FGenerationTimes[i];

  Result := sum / Length(FGenerationTimes);
end;

function TGAProfiler.GetEvaluationsPerSecond: Double;  
var
  totalTime: Double;
begin
  totalTime := MilliSecondsBetween(Now, FStartTime) / 1000;

  if totalTime > 0 then
    Result := FFitnessEvaluations / totalTime
  else
    Result := 0;
end;

procedure TGAProfiler.PrintStats;  
begin
  WriteLn('=== Statistiques de Performance ===');
  WriteLn(Format('Générations: %d', [Length(FGenerationTimes)]));
  WriteLn(Format('Temps moyen par génération: %.2f ms', [GetAverageGenerationTime]));
  WriteLn(Format('Évaluations de fitness: %d', [FFitnessEvaluations]));
  WriteLn(Format('Évaluations/seconde: %.0f', [GetEvaluationsPerSecond]));
  WriteLn('===================================');
end;

end.
```

---

## Comparaison avec d'Autres Méthodes d'Optimisation

### Quand utiliser les AG ?

| Méthode | Avantages | Inconvénients | Quand l'utiliser |
|---------|-----------|---------------|------------------|
| **Algorithmes Génétiques** | - Espace de recherche vaste<br>- Pas de dérivée nécessaire<br>- Robuste au bruit | - Lent<br>- Pas de garantie d'optimalité<br>- Nombreux paramètres | Optimisation globale, problèmes combinatoires, pas de structure connue |
| **Descente de gradient** | - Rapide<br>- Efficace si différentiable | - Reste coincé dans minima locaux<br>- Nécessite des dérivées | Fonctions différentiables, optimisation locale |
| **Recherche exhaustive** | - Trouve l'optimum garanti | - Temps exponentiel<br>- Infaisable pour grands espaces | Petits espaces de recherche discrets |
| **Recuit simulé** | - Simple<br>- Échappe aux minima locaux | - Lent<br>- Difficile à paramétrer | Problèmes d'optimisation combinatoire |
| **PSO (Essaim de particules)** | - Simple<br>- Peu de paramètres | - Convergence prématurée possible | Optimisation continue, problèmes numériques |

### Hybridation : Combiner les approches

```pascal
procedure HybridOptimization(var AGA: TGeneticAlgorithm;
                             AGradientFunction: TGradientFunction);
var
  i: Integer;
  pop: TPopulation;
begin
  // Phase 1: Exploration globale avec AG (50 générations)
  WriteLn('Phase 1: Exploration globale (AG)...');
  AGA.Evolve(50);

  // Phase 2: Affinage local avec descente de gradient
  WriteLn('Phase 2: Raffinement local (Gradient)...');
  pop := AGA.GetPopulation;

  for i := 0 to 9 do // 10 meilleurs individus
    GradientDescent(pop[i], AGradientFunction, 100);

  // Phase 3: Dernière phase d'AG pour diversifier
  WriteLn('Phase 3: Diversification finale (AG)...');
  AGA.Evolve(20);
end;
```

---

## Exemples d'Applications Réelles

### 1. Optimisation d'Horaires (Job Scheduling)

```pascal
program JobScheduling;

{$mode objfpc}{$H+}

uses
  SysUtils, Math;

type
  TJob = record
    ID: Integer;
    Duration: Integer;
    Priority: Integer;
    Deadline: Integer;
  end;

  TSchedule = array of Integer; // Ordre d'exécution des jobs

var
  Jobs: array of TJob;

procedure InitializeJobs;  
begin
  SetLength(Jobs, 6);

  Jobs[0].ID := 1; Jobs[0].Duration := 3; Jobs[0].Priority := 5; Jobs[0].Deadline := 8;
  Jobs[1].ID := 2; Jobs[1].Duration := 2; Jobs[1].Priority := 3; Jobs[1].Deadline := 5;
  Jobs[2].ID := 3; Jobs[2].Duration := 4; Jobs[2].Priority := 8; Jobs[2].Deadline := 12;
  Jobs[3].ID := 4; Jobs[3].Duration := 1; Jobs[3].Priority := 2; Jobs[3].Deadline := 3;
  Jobs[4].ID := 5; Jobs[4].Duration := 5; Jobs[4].Priority := 9; Jobs[4].Deadline := 15;
  Jobs[5].ID := 6; Jobs[5].Duration := 2; Jobs[5].Priority := 4; Jobs[5].Deadline := 7;
end;

function EvaluateSchedule(const ASchedule: TChromosome): Double;  
var
  i, currentTime, jobIndex, lateness: Integer;
  totalPenalty: Double;
begin
  currentTime := 0;
  totalPenalty := 0;

  for i := 0 to High(ASchedule) do
  begin
    // Convertir le gène en index de job
    jobIndex := Round(ASchedule[i] * (Length(Jobs) - 1));

    // Exécuter le job
    currentTime := currentTime + Jobs[jobIndex].Duration;

    // Calculer le retard
    if currentTime > Jobs[jobIndex].Deadline then
    begin
      lateness := currentTime - Jobs[jobIndex].Deadline;
      totalPenalty := totalPenalty + lateness * Jobs[jobIndex].Priority;
    end
    else
    begin
      // Bonus pour terminer avant la deadline
      totalPenalty := totalPenalty - Jobs[jobIndex].Priority;
    end;
  end;

  // Fitness = -penalty (on veut minimiser la pénalité)
  Result := -totalPenalty;
end;

// Programme principal similaire aux exemples précédents...
begin
  InitializeJobs;
  // Créer et exécuter l'AG avec EvaluateSchedule comme fonction fitness
  WriteLn('Optimisation d''ordonnancement de tâches...');
end.
```

### 2. Design de Circuit Électronique

```pascal
program CircuitDesign;

{$mode objfpc}{$H+}

type
  TComponent = record
    X, Y: Double;      // Position sur le circuit
    Width, Height: Double;
    ConnectionCost: Double;
  end;

function EvaluateCircuitLayout(const AChromosome: TChromosome): Double;  
var
  i: Integer;
  components: array of TComponent;
  totalWireLength, overlapPenalty: Double;
begin
  // Décoder le chromosome en positions de composants
  SetLength(components, Length(AChromosome) div 2);

  for i := 0 to High(components) do
  begin
    components[i].X := AChromosome[i * 2] * 100;
    components[i].Y := AChromosome[i * 2 + 1] * 100;
  end;

  // Calculer la longueur totale des connexions
  totalWireLength := CalculateTotalWireLength(components);

  // Pénalité pour les chevauchements
  overlapPenalty := CalculateOverlapPenalty(components);

  // Fitness = minimiser la longueur ET les chevauchements
  Result := 1000 / (totalWireLength + overlapPenalty + 1);

  SetLength(components, 0);
end;
```

### 3. Calibration de Modèle de Machine Learning

```pascal
program MLHyperparameters;

{$mode objfpc}{$H+}

// Optimiser les hyperparamètres d'un réseau de neurones
function EvaluateHyperparameters(const AChromosome: TChromosome): Double;  
var
  learningRate, momentum, dropout: Double;
  hiddenLayers, neuronsPerLayer: Integer;
  validationAccuracy: Double;
begin
  // Décoder les hyperparamètres
  learningRate := AChromosome[0] * 0.1;           // 0 à 0.1
  momentum := AChromosome[1] * 0.9 + 0.1;         // 0.1 à 1.0
  dropout := AChromosome[2] * 0.5;                // 0 à 0.5
  hiddenLayers := Round(AChromosome[3] * 4) + 1;  // 1 à 5
  neuronsPerLayer := Round(AChromosome[4] * 256); // 0 à 256

  // Entraîner le modèle avec ces hyperparamètres
  // et retourner la précision sur le jeu de validation
  validationAccuracy := TrainAndEvaluateModel(
    learningRate, momentum, dropout, hiddenLayers, neuronsPerLayer
  );

  Result := validationAccuracy;
end;
```

---

## Variantes et Extensions

### 1. Algorithme Génétique avec Niches

Maintenir la diversité en pénalisant les solutions trop similaires :

```pascal
function CalculateNicheFitness(var APopulation: TPopulation;
                               ASharingRadius: Double): TPopulation;
var
  i, j: Integer;
  distance, sharingValue, nicheCount: Double;
begin
  Result := Copy(APopulation);

  for i := 0 to High(APopulation) do
  begin
    nicheCount := 0;

    for j := 0 to High(APopulation) do
    begin
      distance := ChromosomeDistance(APopulation[i].Chromosome,
                                     APopulation[j].Chromosome);

      if distance < ASharingRadius then
      begin
        sharingValue := 1 - (distance / ASharingRadius);
        nicheCount := nicheCount + sharingValue;
      end;
    end;

    // Ajuster le fitness
    if nicheCount > 0 then
      Result[i].Fitness := APopulation[i].Fitness / nicheCount;
  end;
end;

function ChromosomeDistance(const AChrom1, AChrom2: TChromosome): Double;  
var
  i: Integer;
  sum: Double;
begin
  sum := 0;
  for i := 0 to High(AChrom1) do
    sum := sum + Power(AChrom1[i] - AChrom2[i], 2);
  Result := Sqrt(sum);
end;
```

### 2. Algorithme Mémétique

Combiner AG avec apprentissage local :

```pascal
procedure MemeticAlgorithm(var AGA: TGeneticAlgorithm;
                           ALocalSearchProb: Double);
var
  i: Integer;
  pop: TPopulation;
begin
  AGA.EvolveOneGeneration;

  pop := AGA.GetPopulation;

  // Appliquer la recherche locale à certains individus
  for i := 0 to High(pop) do
  begin
    if Random < ALocalSearchProb then
      LocalSearch(pop[i], AGA.FitnessFunction, 10);
  end;
end;
```

### 3. Differential Evolution

Une variante particulièrement efficace pour l'optimisation continue :

```pascal
unit DifferentialEvolution;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math;

type
  TDEIndividual = record
    Vector: array of Double;
    Fitness: Double;
  end;

  TDifferentialEvolution = class
  private
    FPopulation: array of TDEIndividual;
    FPopulationSize: Integer;
    FDimensions: Integer;
    FCrossoverRate: Double;
    FDifferentialWeight: Double;
    FFitnessFunction: TFitnessFunction;

    function Mutate(AIndex: Integer): TDEIndividual;
    function Crossover(const ATarget, AMutant: TDEIndividual): TDEIndividual;
  public
    constructor Create(APopSize, ADimensions: Integer;
                       AFitnessFunction: TFitnessFunction);
    destructor Destroy; override;

    procedure EvolveOneGeneration;
    function GetBestIndividual: TDEIndividual;

    property CrossoverRate: Double read FCrossoverRate write FCrossoverRate;
    property DifferentialWeight: Double read FDifferentialWeight write FDifferentialWeight;
  end;

implementation

constructor TDifferentialEvolution.Create(APopSize, ADimensions: Integer;
                                           AFitnessFunction: TFitnessFunction);
var
  i, j: Integer;
begin
  FPopulationSize := APopSize;
  FDimensions := ADimensions;
  FFitnessFunction := AFitnessFunction;
  FCrossoverRate := 0.9;
  FDifferentialWeight := 0.8;

  SetLength(FPopulation, FPopulationSize);

  for i := 0 to FPopulationSize - 1 do
  begin
    SetLength(FPopulation[i].Vector, FDimensions);
    for j := 0 to FDimensions - 1 do
      FPopulation[i].Vector[j] := Random;

    FPopulation[i].Fitness := FFitnessFunction(FPopulation[i].Vector);
  end;
end;

destructor TDifferentialEvolution.Destroy;  
begin
  SetLength(FPopulation, 0);
  inherited;
end;

function TDifferentialEvolution.Mutate(AIndex: Integer): TDEIndividual;  
var
  i, r1, r2, r3: Integer;
begin
  // Sélectionner 3 individus différents aléatoirement
  repeat
    r1 := Random(FPopulationSize);
  until r1 <> AIndex;

  repeat
    r2 := Random(FPopulationSize);
  until (r2 <> AIndex) and (r2 <> r1);

  repeat
    r3 := Random(FPopulationSize);
  until (r3 <> AIndex) and (r3 <> r1) and (r3 <> r2);

  // Mutation: V = X_r1 + F * (X_r2 - X_r3)
  SetLength(Result.Vector, FDimensions);
  for i := 0 to FDimensions - 1 do
  begin
    Result.Vector[i] := FPopulation[r1].Vector[i] +
                        FDifferentialWeight *
                        (FPopulation[r2].Vector[i] - FPopulation[r3].Vector[i]);

    // Garder dans [0, 1]
    if Result.Vector[i] < 0 then Result.Vector[i] := 0;
    if Result.Vector[i] > 1 then Result.Vector[i] := 1;
  end;
end;

function TDifferentialEvolution.Crossover(const ATarget, AMutant: TDEIndividual): TDEIndividual;  
var
  i, jRand: Integer;
begin
  SetLength(Result.Vector, FDimensions);
  jRand := Random(FDimensions);

  for i := 0 to FDimensions - 1 do
  begin
    if (Random < FCrossoverRate) or (i = jRand) then
      Result.Vector[i] := AMutant.Vector[i]
    else
      Result.Vector[i] := ATarget.Vector[i];
  end;
end;

procedure TDifferentialEvolution.EvolveOneGeneration;  
var
  i: Integer;
  mutant, trial: TDEIndividual;
begin
  for i := 0 to FPopulationSize - 1 do
  begin
    // Mutation
    mutant := Mutate(i);

    // Croisement
    trial := Crossover(FPopulation[i], mutant);

    // Évaluation
    trial.Fitness := FFitnessFunction(trial.Vector);

    // Sélection
    if trial.Fitness > FPopulation[i].Fitness then
      FPopulation[i] := trial;
  end;
end;

function TDifferentialEvolution.GetBestIndividual: TDEIndividual;  
var
  i: Integer;
begin
  Result := FPopulation[0];
  for i := 1 to FPopulationSize - 1 do
    if FPopulation[i].Fitness > Result.Fitness then
      Result := FPopulation[i];
end;

end.
```

---

## Ressources et Documentation

### Bibliothèques FreePascal pour AG

**1. GA-Lib**
- Bibliothèque générique d'algorithmes génétiques
- Support de différents encodages
- Compatible FreePascal et Delphi

**2. Evolutionary-Algorithms-FPC**
- Package complet pour l'optimisation évolutionnaire
- Inclus AG, PSO, Differential Evolution
- GitHub: rechercher "evolutionary algorithms freepascal"

### Livres et Références

**Livres recommandés :**
1. "Introduction to Genetic Algorithms" - S.N. Sivanandam
2. "Genetic Algorithms in Search, Optimization, and Machine Learning" - David Goldberg
3. "An Introduction to Genetic Algorithms" - Melanie Mitchell

**Articles scientifiques clés :**
- Holland, J.H. (1975) - "Adaptation in Natural and Artificial Systems"
- Goldberg, D.E. (1989) - "Genetic Algorithms in Search"
- Deb, K. (2002) - "A Fast and Elitist Multiobjective Genetic Algorithm: NSGA-II"

### Ressources en ligne

**Tutoriels :**
- AI Junkie: Genetic Algorithms Tutorial
- Free Pascal Wiki: Optimization Algorithms
- YouTube: "Genetic Algorithms Explained"

**Forums et Communautés :**
- FreePascal Forum: https://forum.lazarus.freepascal.org/
- Stack Overflow (tag: genetic-algorithm)
- Reddit: r/genetic_algorithms

---

## Conclusion

### Ce que vous avez appris

Au cours de ce tutoriel, vous avez découvert :

✅ **Concepts fondamentaux**
- Principes de l'évolution artificielle
- Vocabulaire des AG (chromosome, fitness, génération)
- Cycle de vie d'un algorithme génétique

✅ **Implémentation pratique**
- Structure de données en FreePascal
- Opérateurs génétiques (sélection, croisement, mutation)
- Différents types d'encodage

✅ **Applications concrètes**
- Optimisation de fonctions mathématiques
- Problème du voyageur de commerce
- Problème du sac à dos
- Optimisation de portefeuille

✅ **Techniques avancées**
- Mutation adaptative
- Island Model (parallélisation)
- Algorithmes multi-objectifs (NSGA-II)
- Hybridation avec d'autres méthodes

✅ **Bonnes pratiques**
- Choix des paramètres
- Débogage et profiling
- Détection de convergence prématurée
- Optimisation des performances

### Quand utiliser les algorithmes génétiques ?

**✓ Utilisez les AG quand :**
- L'espace de recherche est vaste et complexe
- Vous n'avez pas de méthode analytique
- Le problème est combinatoire
- Vous acceptez une solution "suffisamment bonne"
- Le temps de calcul n'est pas critique

**✗ N'utilisez PAS les AG quand :**
- Une solution exacte existe et est rapide
- L'espace de recherche est petit
- Vous avez besoin de garanties mathématiques
- Le temps de calcul est très limité
- Le problème est facilement différentiable

### Aller plus loin

**Prochaines étapes suggérées :**

1. **Experimenter avec vos propres problèmes**
   - Adaptez les exemples à vos besoins
   - Testez différents paramètres
   - Comparez avec d'autres méthodes

2. **Explorer d'autres métaheuristiques**
   - Optimisation par essaim de particules (PSO)
   - Colonies de fourmis (ACO)
   - Recuit simulé
   - Recherche tabou

3. **Approfondir les AG**
   - Programmation génétique
   - Apprentissage de réseaux de neurones
   - Co-évolution
   - AG quantiques

4. **Contribuer à la communauté**
   - Partager vos implémentations
   - Créer des packages réutilisables
   - Documenter vos expériences

### Code source complet

Tous les exemples de ce tutoriel sont disponibles et peuvent être compilés avec FreePascal 3.2+ et Lazarus 2.0+.

**Structure du projet :**
```
GeneticAlgorithms/
├── src/
│   ├── GeneticAlgorithm.pas
│   ├── TSPGeneticAlgorithm.pas
│   ├── MultiObjectiveGA.pas
│   ├── DifferentialEvolution.pas
│   └── IslandGA.pas
├── examples/
│   ├── OptimizeFunction.pas
│   ├── TestTSP.pas
│   ├── KnapsackGA.pas
│   └── PortfolioOptimization.pas
├── tests/
│   └── TestGA.pas
└── README.md
```

### Message final

Les algorithmes génétiques sont des outils puissants qui ouvrent la porte à la résolution de problèmes complexes d'optimisation. Avec FreePascal/Lazarus, vous disposez d'un environnement performant et multi-plateforme pour implémenter et déployer vos solutions.

**Points clés à retenir :**
- Les AG sont inspirés de l'évolution naturelle
- Ils excellent dans l'exploration de grands espaces de recherche
- Le choix des paramètres est crucial pour le succès
- L'hybridation avec d'autres méthodes améliore les résultats
- FreePascal offre d'excellentes performances pour les AG

**L'évolution ne s'arrête jamais !** Continuez à expérimenter, à optimiser et à faire évoluer vos algorithmes. La nature nous a montré que c'est le meilleur moyen de s'adapter et de progresser.

Bon codage et bonne optimisation avec les algorithmes génétiques ! 🧬🚀

---

*Fin du tutoriel 15.5 - Algorithmes génétiques*

**Prochaine étape recommandée :** 15.6 Apprentissage par renforcement

⏭️ [Apprentissage par renforcement](/15-intelligence-artificielle-machine-learning/06-apprentissage-par-renforcement.md)
