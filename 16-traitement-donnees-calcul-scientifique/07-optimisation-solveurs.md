🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 16.7 Optimisation et solveurs

## Introduction

L'optimisation consiste à trouver la meilleure solution parmi toutes les solutions possibles d'un problème. Que vous cherchiez le chemin le plus court, la configuration la plus efficace, le coût minimal ou le rendement maximal, les techniques d'optimisation sont essentielles dans de nombreux domaines : ingénierie, finance, logistique, intelligence artificielle, et bien d'autres.

## Qu'est-ce que l'optimisation ?

### Définition simple

Optimiser, c'est chercher à :
- **Minimiser** : Coûts, temps, distance, erreurs
- **Maximiser** : Profits, performances, efficacité, qualité

### Exemple concret

Imaginez que vous devez livrer des colis à 10 adresses différentes :
- **Problème** : Dans quel ordre visiter les adresses ?
- **Objectif** : Minimiser la distance totale parcourue
- **Solution** : L'algorithme d'optimisation trouve le meilleur itinéraire

## Types de problèmes d'optimisation

### Classification

1. **Optimisation continue** : Variables réelles (ex: vitesse optimale)
2. **Optimisation discrète** : Variables entières (ex: nombre d'employés)
3. **Optimisation combinatoire** : Choix parmi un ensemble (ex: sélection d'objets)
4. **Optimisation contrainte** : Avec limites/restrictions
5. **Optimisation non contrainte** : Sans restrictions

## Types de données en FreePascal

```pascal
unit UnitOptimisation;

{$mode objfpc}{$H+}

interface

uses
  Math, SysUtils;

type
  TFloat = Double;
  TVecteur = array of TFloat;
  TMatrice = array of array of TFloat;

  // Fonction à optimiser
  TFonctionObjectif = function(const x: TVecteur): TFloat;
  TFonctionObjectifScalaire = function(x: TFloat): TFloat;

  // Gradient (dérivées)
  TGradient = function(const x: TVecteur): TVecteur;

  // Résultat d'optimisation
  TResultatOptimisation = record
    Solution: TVecteur;
    ValeurOptimale: TFloat;
    NbIterations: Integer;
    Converge: Boolean;
    TempsCalcul: TFloat;
  end;

  // Contraintes
  TContrainte = record
    Type_: (ctEgalite, ctInegalite);
    Fonction: TFonctionObjectif;
    Valeur: TFloat;
  end;

implementation

end.
```

## Optimisation unidimensionnelle

### Recherche par dichotomie (bissection)

Trouve le minimum d'une fonction sur un intervalle.

```pascal
function RechercheParDichotomie(f: TFonctionObjectifScalaire;
                               a, b: TFloat;
                               tolerance: TFloat = 1e-6): TFloat;
var
  milieu, f1, f2, f3: TFloat;
  x1, x2: TFloat;
begin
  while Abs(b - a) > tolerance do
  begin
    milieu := (a + b) / 2;

    // Évaluer en trois points
    x1 := a + (b - a) / 4;
    x2 := b - (b - a) / 4;

    f1 := f(x1);
    f2 := f(milieu);
    f3 := f(x2);

    // Réduire l'intervalle
    if (f1 < f2) then
      b := milieu
    else if (f3 < f2) then
      a := milieu
    else
    begin
      a := x1;
      b := x2;
    end;
  end;

  Result := (a + b) / 2;
end;

// Exemple d'utilisation
function MaFonction(x: TFloat): TFloat;  
begin
  Result := Sqr(x - 3) + 2;  // Minimum en x = 3
end;

var
  xMin: TFloat;
begin
  xMin := RechercheParDichotomie(@MaFonction, 0, 10);
  WriteLn(Format('Minimum trouvé en x = %.4f', [xMin]));
  WriteLn(Format('Valeur minimale = %.4f', [MaFonction(xMin)]));
end;
```

### Méthode de la section dorée (Golden section)

Plus efficace que la dichotomie.

```pascal
function SectionDoree(f: TFonctionObjectifScalaire;
                     a, b: TFloat;
                     tolerance: TFloat = 1e-6): TFloat;
const
  PHI = 1.618033988749895;  // Nombre d'or
  RESPHI = 2 - PHI;         // 2 - φ ≈ 0.618
var
  x1, x2, f1, f2: TFloat;
begin
  x1 := a + RESPHI * (b - a);
  x2 := b - RESPHI * (b - a);
  f1 := f(x1);
  f2 := f(x2);

  while Abs(b - a) > tolerance do
  begin
    if f1 < f2 then
    begin
      b := x2;
      x2 := x1;
      f2 := f1;
      x1 := a + RESPHI * (b - a);
      f1 := f(x1);
    end
    else
    begin
      a := x1;
      x1 := x2;
      f1 := f2;
      x2 := b - RESPHI * (b - a);
      f2 := f(x2);
    end;
  end;

  if f1 < f2 then
    Result := x1
  else
    Result := x2;
end;
```

## Optimisation multidimensionnelle sans contraintes

### Descente de gradient

Méthode itérative qui suit la pente descendante.

```pascal
function DescenteGradient(f: TFonctionObjectif;
                         grad: TGradient;
                         const x0: TVecteur;
                         tauxApprentissage: TFloat = 0.01;
                         maxIter: Integer = 1000;
                         tolerance: TFloat = 1e-6): TResultatOptimisation;
var
  x, xNouveau, gradient: TVecteur;
  i, j: Integer;
  norme: TFloat;
begin
  x := Copy(x0);

  for i := 1 to maxIter do
  begin
    // Calculer le gradient
    gradient := grad(x);

    // Mettre à jour x
    SetLength(xNouveau, Length(x));
    for j := 0 to High(x) do
      xNouveau[j] := x[j] - tauxApprentissage * gradient[j];

    // Vérifier la convergence
    norme := 0;
    for j := 0 to High(x) do
      norme := norme + Sqr(xNouveau[j] - x[j]);
    norme := Sqrt(norme);

    x := xNouveau;

    if norme < tolerance then
    begin
      Result.Solution := x;
      Result.ValeurOptimale := f(x);
      Result.NbIterations := i;
      Result.Converge := True;
      Exit;
    end;
  end;

  Result.Solution := x;
  Result.ValeurOptimale := f(x);
  Result.NbIterations := maxIter;
  Result.Converge := False;
end;

// Calcul numérique du gradient (approximation)
function GradientNumerique(f: TFonctionObjectif;
                          const x: TVecteur;
                          epsilon: TFloat = 1e-5): TVecteur;
var
  i: Integer;
  xPlus, xMoins: TVecteur;
begin
  SetLength(Result, Length(x));

  for i := 0 to High(x) do
  begin
    xPlus := Copy(x);
    xMoins := Copy(x);

    xPlus[i] := x[i] + epsilon;
    xMoins[i] := x[i] - epsilon;

    Result[i] := (f(xPlus) - f(xMoins)) / (2 * epsilon);
  end;
end;
```

### Méthode de Newton

Utilise la dérivée seconde (Hessienne) pour converger plus rapidement.

```pascal
type
  THessienne = function(const x: TVecteur): TMatrice;

function MethodeNewton(f: TFonctionObjectif;
                      grad: TGradient;
                      hess: THessienne;
                      const x0: TVecteur;
                      maxIter: Integer = 100;
                      tolerance: TFloat = 1e-6): TResultatOptimisation;
var
  x, xNouveau, gradient, direction: TVecteur;
  hessienne: TMatrice;
  i: Integer;
  norme: TFloat;
begin
  x := Copy(x0);

  for i := 1 to maxIter do
  begin
    // Calculer gradient et hessienne
    gradient := grad(x);
    hessienne := hess(x);

    // Résoudre H * d = -g pour obtenir la direction
    direction := ResoudreSysteme(hessienne, MultiplyByScalar(gradient, -1));

    // Mettre à jour
    xNouveau := Addition(x, direction);

    // Convergence
    norme := Norme(Soustraction(xNouveau, x));

    x := xNouveau;

    if norme < tolerance then
    begin
      Result.Solution := x;
      Result.ValeurOptimale := f(x);
      Result.NbIterations := i;
      Result.Converge := True;
      Exit;
    end;
  end;

  Result.Solution := x;
  Result.ValeurOptimale := f(x);
  Result.NbIterations := maxIter;
  Result.Converge := False;
end;
```

### Gradient conjugué

Plus efficace que la descente de gradient simple.

```pascal
function GradientConjugue(f: TFonctionObjectif;
                         grad: TGradient;
                         const x0: TVecteur;
                         maxIter: Integer = 1000;
                         tolerance: TFloat = 1e-6): TResultatOptimisation;
var
  x, gradient, gradientPrec, direction: TVecteur;
  i, j: Integer;
  alpha, beta: TFloat;
  norme: TFloat;
begin
  x := Copy(x0);
  gradient := grad(x);
  direction := MultiplyByScalar(gradient, -1);

  for i := 1 to maxIter do
  begin
    // Recherche linéaire pour trouver alpha optimal
    alpha := RechercheLineaire(f, x, direction);

    // Mise à jour
    for j := 0 to High(x) do
      x[j] := x[j] + alpha * direction[j];

    // Nouveau gradient
    gradientPrec := gradient;
    gradient := grad(x);

    // Vérifier convergence
    norme := Norme(gradient);
    if norme < tolerance then
    begin
      Result.Solution := x;
      Result.ValeurOptimale := f(x);
      Result.NbIterations := i;
      Result.Converge := True;
      Exit;
    end;

    // Calculer beta (Fletcher-Reeves)
    beta := ProduitScalaire(gradient, gradient) /
            ProduitScalaire(gradientPrec, gradientPrec);

    // Nouvelle direction
    for j := 0 to High(direction) do
      direction[j] := -gradient[j] + beta * direction[j];
  end;

  Result.Solution := x;
  Result.ValeurOptimale := f(x);
  Result.NbIterations := maxIter;
  Result.Converge := False;
end;

function RechercheLineaire(f: TFonctionObjectif;
                          const x, direction: TVecteur;
                          alphaMax: TFloat = 1.0): TFloat;
var
  alpha: TFloat;
  xTest: TVecteur;
  fAlpha: TFonctionObjectifScalaire;
  j: Integer;
begin
  // Créer une fonction 1D pour alpha
  fAlpha := function(a: TFloat): TFloat
  var
    i: Integer;
    xTemp: TVecteur;
  begin
    SetLength(xTemp, Length(x));
    for i := 0 to High(x) do
      xTemp[i] := x[i] + a * direction[i];
    Result := f(xTemp);
  end;

  // Recherche sur [0, alphaMax]
  Result := SectionDoree(fAlpha, 0, alphaMax);
end;
```

## Algorithmes métaheuristiques

### Recuit simulé (Simulated Annealing)

Inspiré du processus de refroidissement des métaux.

```pascal
function RecuitSimule(f: TFonctionObjectif;
                     const x0: TVecteur;
                     temperatureInitiale: TFloat = 100;
                     tauxRefroidissement: TFloat = 0.95;
                     maxIter: Integer = 10000): TResultatOptimisation;
var
  x, xMeilleur, xVoisin: TVecteur;
  fActuelle, fMeilleure, fVoisin: TFloat;
  temperature: TFloat;
  i, j: Integer;
  delta: TFloat;
  accepte: Boolean;
begin
  x := Copy(x0);
  xMeilleur := Copy(x0);
  fActuelle := f(x);
  fMeilleure := fActuelle;
  temperature := temperatureInitiale;

  Randomize;

  for i := 1 to maxIter do
  begin
    // Générer un voisin aléatoire
    SetLength(xVoisin, Length(x));
    for j := 0 to High(x) do
      xVoisin[j] := x[j] + (Random - 0.5) * temperature * 0.1;

    fVoisin := f(xVoisin);
    delta := fVoisin - fActuelle;

    // Critère d'acceptation de Metropolis
    accepte := (delta < 0) or (Random < Exp(-delta / temperature));

    if accepte then
    begin
      x := xVoisin;
      fActuelle := fVoisin;

      if fActuelle < fMeilleure then
      begin
        xMeilleur := x;
        fMeilleure := fActuelle;
      end;
    end;

    // Refroidir
    temperature := temperature * tauxRefroidissement;
  end;

  Result.Solution := xMeilleur;
  Result.ValeurOptimale := fMeilleure;
  Result.NbIterations := maxIter;
  Result.Converge := True;
end;
```

### Algorithme génétique

Inspiré de l'évolution naturelle.

```pascal
type
  TIndividu = record
    Genes: TVecteur;
    Fitness: TFloat;
  end;

  TPopulation = array of TIndividu;

function AlgorithmeGenetique(f: TFonctionObjectif;
                            dimension: Integer;
                            bornesMin, bornesMax: TVecteur;
                            taillePopulation: Integer = 50;
                            nbGenerations: Integer = 100;
                            tauxMutation: TFloat = 0.1): TResultatOptimisation;
var
  population: TPopulation;
  generation, i, j: Integer;
  meilleur: TIndividu;
begin
  SetLength(population, taillePopulation);

  // Initialiser la population
  InitialiserPopulation(population, dimension, bornesMin, bornesMax);

  // Évaluer la fitness
  for i := 0 to High(population) do
    population[i].Fitness := f(population[i].Genes);

  meilleur := population[0];

  // Boucle des générations
  for generation := 1 to nbGenerations do
  begin
    // Sélection
    population := Selection(population);

    // Croisement
    population := Croisement(population);

    // Mutation
    population := Mutation(population, tauxMutation, bornesMin, bornesMax);

    // Évaluation
    for i := 0 to High(population) do
    begin
      population[i].Fitness := f(population[i].Genes);

      if population[i].Fitness < meilleur.Fitness then
        meilleur := population[i];
    end;
  end;

  Result.Solution := meilleur.Genes;
  Result.ValeurOptimale := meilleur.Fitness;
  Result.NbIterations := nbGenerations;
  Result.Converge := True;
end;

procedure InitialiserPopulation(var pop: TPopulation;
                               dimension: Integer;
                               const bornesMin, bornesMax: TVecteur);
var
  i, j: Integer;
begin
  Randomize;
  for i := 0 to High(pop) do
  begin
    SetLength(pop[i].Genes, dimension);
    for j := 0 to dimension - 1 do
      pop[i].Genes[j] := bornesMin[j] +
                        Random * (bornesMax[j] - bornesMin[j]);
  end;
end;

function Selection(const pop: TPopulation): TPopulation;  
var
  i, index1, index2: Integer;
begin
  SetLength(Result, Length(pop));

  // Sélection par tournoi
  for i := 0 to High(Result) do
  begin
    index1 := Random(Length(pop));
    index2 := Random(Length(pop));

    if pop[index1].Fitness < pop[index2].Fitness then
      Result[i] := pop[index1]
    else
      Result[i] := pop[index2];
  end;
end;

function Croisement(const pop: TPopulation): TPopulation;  
var
  i, j, pointCroisement: Integer;
  parent1, parent2: TIndividu;
begin
  SetLength(Result, Length(pop));

  for i := 0 to High(Result) - 1 do
  begin
    parent1 := pop[i];
    parent2 := pop[i + 1];

    // Croisement en un point
    pointCroisement := Random(Length(parent1.Genes));

    SetLength(Result[i].Genes, Length(parent1.Genes));
    SetLength(Result[i + 1].Genes, Length(parent1.Genes));

    for j := 0 to High(parent1.Genes) do
    begin
      if j < pointCroisement then
      begin
        Result[i].Genes[j] := parent1.Genes[j];
        Result[i + 1].Genes[j] := parent2.Genes[j];
      end
      else
      begin
        Result[i].Genes[j] := parent2.Genes[j];
        Result[i + 1].Genes[j] := parent1.Genes[j];
      end;
    end;
  end;
end;

function Mutation(const pop: TPopulation; taux: TFloat;
                 const bornesMin, bornesMax: TVecteur): TPopulation;
var
  i, j: Integer;
begin
  Result := Copy(pop);

  for i := 0 to High(Result) do
  begin
    for j := 0 to High(Result[i].Genes) do
    begin
      if Random < taux then
        Result[i].Genes[j] := bornesMin[j] +
                             Random * (bornesMax[j] - bornesMin[j]);
    end;
  end;
end;
```

### Optimisation par essaim particulaire (PSO)

Inspiré du comportement des oiseaux en vol.

```pascal
type
  TParticule = record
    Position: TVecteur;
    Vitesse: TVecteur;
    MeilleurePosition: TVecteur;
    MeilleureFitness: TFloat;
  end;

  TEssaim = array of TParticule;

function OptimisationEssaimParticulaire(f: TFonctionObjectif;
                                       dimension: Integer;
                                       bornesMin, bornesMax: TVecteur;
                                       nbParticules: Integer = 30;
                                       maxIter: Integer = 100): TResultatOptimisation;
const
  W = 0.7;   // Inertie
  C1 = 1.5;  // Coefficient cognitif
  C2 = 1.5;  // Coefficient social
var
  essaim: TEssaim;
  meilleure PositionGlobale: TVecteur;
  meilleureFitnessGlobale: TFloat;
  i, j, iter: Integer;
  fitness: TFloat;
  r1, r2: TFloat;
begin
  SetLength(essaim, nbParticules);

  // Initialiser l'essaim
  Randomize;
  for i := 0 to nbParticules - 1 do
  begin
    SetLength(essaim[i].Position, dimension);
    SetLength(essaim[i].Vitesse, dimension);
    SetLength(essaim[i].MeilleurePosition, dimension);

    for j := 0 to dimension - 1 do
    begin
      essaim[i].Position[j] := bornesMin[j] +
                              Random * (bornesMax[j] - bornesMin[j]);
      essaim[i].Vitesse[j] := (Random - 0.5) *
                             (bornesMax[j] - bornesMin[j]) * 0.1;
    end;

    essaim[i].MeilleurePosition := Copy(essaim[i].Position);
    essaim[i].MeilleureFitness := f(essaim[i].Position);
  end;

  // Trouver la meilleure position globale
  meilleurePositionGlobale := Copy(essaim[0].MeilleurePosition);
  meilleureFitnessGlobale := essaim[0].MeilleureFitness;

  for i := 1 to nbParticules - 1 do
  begin
    if essaim[i].MeilleureFitness < meilleureFitnessGlobale then
    begin
      meilleurePositionGlobale := Copy(essaim[i].MeilleurePosition);
      meilleureFitnessGlobale := essaim[i].MeilleureFitness;
    end;
  end;

  // Boucle principale
  for iter := 1 to maxIter do
  begin
    for i := 0 to nbParticules - 1 do
    begin
      // Mettre à jour la vitesse
      for j := 0 to dimension - 1 do
      begin
        r1 := Random;
        r2 := Random;

        essaim[i].Vitesse[j] := W * essaim[i].Vitesse[j] +
          C1 * r1 * (essaim[i].MeilleurePosition[j] - essaim[i].Position[j]) +
          C2 * r2 * (meilleurePositionGlobale[j] - essaim[i].Position[j]);
      end;

      // Mettre à jour la position
      for j := 0 to dimension - 1 do
      begin
        essaim[i].Position[j] := essaim[i].Position[j] + essaim[i].Vitesse[j];

        // Limiter aux bornes
        if essaim[i].Position[j] < bornesMin[j] then
          essaim[i].Position[j] := bornesMin[j];
        if essaim[i].Position[j] > bornesMax[j] then
          essaim[i].Position[j] := bornesMax[j];
      end;

      // Évaluer
      fitness := f(essaim[i].Position);

      // Mettre à jour la meilleure position personnelle
      if fitness < essaim[i].MeilleureFitness then
      begin
        essaim[i].MeilleurePosition := Copy(essaim[i].Position);
        essaim[i].MeilleureFitness := fitness;

        // Mettre à jour la meilleure position globale
        if fitness < meilleureFitnessGlobale then
        begin
          meilleurePositionGlobale := Copy(essaim[i].Position);
          meilleureFitnessGlobale := fitness;
        end;
      end;
    end;
  end;

  Result.Solution := meilleurePositionGlobale;
  Result.ValeurOptimale := meilleureFitnessGlobale;
  Result.NbIterations := maxIter;
  Result.Converge := True;
end;
```

## Optimisation avec contraintes

### Méthode des pénalités

Transformer un problème contraint en problème non contraint.

```pascal
type
  TListeContraintes = array of TContrainte;

function FonctionPenalisee(f: TFonctionObjectif;
                          const contraintes: TListeContraintes;
                          const x: TVecteur;
                          penalite: TFloat): TFloat;
var
  i: Integer;
  valeurContrainte, violation: TFloat;
begin
  Result := f(x);

  // Ajouter les pénalités pour violations
  for i := 0 to High(contraintes) do
  begin
    valeurContrainte := contraintes[i].Fonction(x);

    case contraintes[i].Type_ of
      ctEgalite:
      begin
        // g(x) = c
        violation := Abs(valeurContrainte - contraintes[i].Valeur);
        Result := Result + penalite * Sqr(violation);
      end;

      ctInegalite:
      begin
        // g(x) <= c
        violation := Max(0, valeurContrainte - contraintes[i].Valeur);
        Result := Result + penalite * Sqr(violation);
      end;
    end;
  end;
end;

function OptimisationAvecPenalites(f: TFonctionObjectif;
                                  const contraintes: TListeContraintes;
                                  const x0: TVecteur;
                                  penaliteInitiale: TFloat = 1;
                                  facteurAugmentation: TFloat = 10): TResultatOptimisation;
var
  penalite: TFloat;
  fPenalisee: TFonctionObjectif;
  resultat: TResultatOptimisation;
  iteration: Integer;
begin
  penalite := penaliteInitiale;
  Result.Solution := Copy(x0);

  for iteration := 1 to 10 do
  begin
    // Créer la fonction pénalisée
    fPenalisee := function(const x: TVecteur): TFloat
    begin
      Result := FonctionPenalisee(f, contraintes, x, penalite);
    end;

    // Optimiser sans contraintes
    resultat := DescenteGradient(fPenalisee,
      function(const x: TVecteur): TVecteur
      begin
        Result := GradientNumerique(fPenalisee, x);
      end,
      Result.Solution
    );

    Result.Solution := resultat.Solution;
    Result.ValeurOptimale := f(Result.Solution);
    Result.NbIterations := Result.NbIterations + resultat.NbIterations;

    // Augmenter la pénalité
    penalite := penalite * facteurAugmentation;
  end;

  Result.Converge := True;
end;
```

## Fonctions de test classiques

### Fonctions pour benchmark

```pascal
// Fonction de Rosenbrock (banane)
function Rosenbrock(const x: TVecteur): TFloat;  
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to High(x) - 1 do
    Result := Result + 100 * Sqr(x[i + 1] - Sqr(x[i])) + Sqr(1 - x[i]);
end;

// Fonction de Rastrigin
function Rastrigin(const x: TVecteur): TFloat;  
var
  i: Integer;
begin
  Result := 10 * Length(x);
  for i := 0 to High(x) do
    Result := Result + Sqr(x[i]) - 10 * Cos(2 * Pi * x[i]);
end;

// Fonction sphère (simple)
function Sphere(const x: TVecteur): TFloat;  
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to High(x) do
    Result := Result + Sqr(x[i]);
end;

// Fonction d'Ackley
function Ackley(const x: TVecteur): TFloat;  
var
  i, n: Integer;
  somme1, somme2: TFloat;
begin
  n := Length(x);
  somme1 := 0;
  somme2 := 0;

  for i := 0 to High(x) do
  begin
    somme1 := somme1 + Sqr(x[i]);
    somme2 := somme2 + Cos(2 * Pi * x[i]);
  end;

  Result := -20 * Exp(-0.2 * Sqrt(somme1 / n)) -
            Exp(somme2 / n) + 20 + Exp(1);
end;
```

## Fonctions utilitaires

```pascal
// Opérations sur vecteurs
function Addition(const a, b: TVecteur): TVecteur;  
var
  i: Integer;
begin
  SetLength(Result, Length(a));
  for i := 0 to High(a) do
    Result[i] := a[i] + b[i];
end;

function Soustraction(const a, b: TVecteur): TVecteur;  
var
  i: Integer;
begin
  SetLength(Result, Length(a));
  for i := 0 to High(a) do
    Result[i] := a[i] - b[i];
end;

function MultiplyByScalar(const v: TVecteur; scalaire: TFloat): TVecteur;  
var
  i: Integer;
begin
  SetLength(Result, Length(v));
  for i := 0 to High(v) do
    Result[i] := v[i] * scalaire;
end;

function ProduitScalaire(const a, b: TVecteur): TFloat;  
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to High(a) do
    Result := Result + a[i] * b[i];
end;

function Norme(const v: TVecteur): TFloat;  
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to High(v) do
    Result := Result + Sqr(v[i]);
  Result := Sqrt(Result);
end;

// Résolution de système linéaire simple (pour Newton)
function ResoudreSysteme(const A: TMatrice; const b: TVecteur): TVecteur;  
var
  n, i, j, k: Integer;
  augmentee: TMatrice;
  pivot, facteur: TFloat;
begin
  n := Length(A);
  SetLength(Result, n);
  SetLength(augmentee, n, n + 1);

  // Créer la matrice augmentée [A | b]
  for i := 0 to n - 1 do
  begin
    for j := 0 to n - 1 do
      augmentee[i, j] := A[i, j];
    augmentee[i, n] := b[i];
  end;

  // Élimination de Gauss
  for i := 0 to n - 1 do
  begin
    pivot := augmentee[i, i];
    if Abs(pivot) < 1e-10 then
      raise Exception.Create('Matrice singulière');

    for j := i to n do
      augmentee[i, j] := augmentee[i, j] / pivot;

    for k := i + 1 to n - 1 do
    begin
      facteur := augmentee[k, i];
      for j := i to n do
        augmentee[k, j] := augmentee[k, j] - facteur * augmentee[i, j];
    end;
  end;

  // Substitution arrière
  for i := n - 1 downto 0 do
  begin
    Result[i] := augmentee[i, n];
    for j := i + 1 to n - 1 do
      Result[i] := Result[i] - augmentee[i, j] * Result[j];
  end;
end;
```

## Applications pratiques

### Exemple 1 : Régression avec contraintes

```pascal
// Trouver la meilleure droite passant par des points
// avec contrainte de pente positive

type
  TDonnees = record
    X: TVecteur;
    Y: TVecteur;
  end;

function ErreurQuadratique(const params: TVecteur;
                          const donnees: TDonnees): TFloat;
var
  i: Integer;
  a, b, yPred: TFloat;
begin
  a := params[0];  // Pente
  b := params[1];  // Ordonnée à l'origine

  Result := 0;
  for i := 0 to High(donnees.X) do
  begin
    yPred := a * donnees.X[i] + b;
    Result := Result + Sqr(donnees.Y[i] - yPred);
  end;
end;

procedure RegressionContrainte;  
var
  donnees: TDonnees;
  i: Integer;
  f: TFonctionObjectif;
  contraintes: TListeContraintes;
  x0: TVecteur;
  resultat: TResultatOptimisation;
begin
  // Générer des données
  SetLength(donnees.X, 20);
  SetLength(donnees.Y, 20);

  Randomize;
  for i := 0 to 19 do
  begin
    donnees.X[i] := i;
    donnees.Y[i] := 2 * i + 5 + (Random - 0.5) * 5;
  end;

  // Fonction objectif
  f := function(const x: TVecteur): TFloat
  begin
    Result := ErreurQuadratique(x, donnees);
  end;

  // Contrainte : pente positive (a >= 0)
  SetLength(contraintes, 1);
  contraintes[0].Type_ := ctInegalite;
  contraintes[0].Fonction := function(const x: TVecteur): TFloat
  begin
    Result := -x[0];  // -a <= 0, donc a >= 0
  end;
  contraintes[0].Valeur := 0;

  // Point initial
  SetLength(x0, 2);
  x0[0] := 1;  // a
  x0[1] := 0;  // b

  // Optimiser
  resultat := OptimisationAvecPenalites(f, contraintes, x0);

  WriteLn('=== Régression avec contrainte ===');
  WriteLn(Format('a (pente) = %.4f', [resultat.Solution[0]]));
  WriteLn(Format('b (ordonnée) = %.4f', [resultat.Solution[1]]));
  WriteLn(Format('Erreur = %.4f', [resultat.ValeurOptimale]));
end;
```

### Exemple 2 : Problème du voyageur de commerce (TSP)

```pascal
type
  TVille = record
    X: TFloat;
    Y: TFloat;
    Nom: String;
  end;

  TItineraire = array of Integer;

function DistanceVilles(const v1, v2: TVille): TFloat;  
begin
  Result := Sqrt(Sqr(v2.X - v1.X) + Sqr(v2.Y - v1.Y));
end;

function DistanceTotale(const villes: array of TVille;
                       const itineraire: TItineraire): TFloat;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to High(itineraire) - 1 do
    Result := Result + DistanceVilles(villes[itineraire[i]],
                                     villes[itineraire[i + 1]]);

  // Retour à la ville de départ
  Result := Result + DistanceVilles(villes[itineraire[High(itineraire)]],
                                   villes[itineraire[0]]);
end;

function TSPRecuitSimule(const villes: array of TVille;
                        maxIter: Integer = 10000): TItineraire;
var
  itineraire, meilleurItineraire, voisin: TItineraire;
  distActuelle, distMeilleure, distVoisin: TFloat;
  temperature: TFloat;
  i, iter, pos1, pos2, temp: Integer;
  delta: TFloat;
begin
  // Initialiser l'itinéraire
  SetLength(itineraire, Length(villes));
  for i := 0 to High(villes) do
    itineraire[i] := i;

  meilleurItineraire := Copy(itineraire);
  distActuelle := DistanceTotale(villes, itineraire);
  distMeilleure := distActuelle;
  temperature := 100;

  Randomize;

  for iter := 1 to maxIter do
  begin
    // Générer un voisin (échanger deux villes)
    voisin := Copy(itineraire);
    pos1 := Random(Length(voisin));
    pos2 := Random(Length(voisin));

    temp := voisin[pos1];
    voisin[pos1] := voisin[pos2];
    voisin[pos2] := temp;

    distVoisin := DistanceTotale(villes, voisin);
    delta := distVoisin - distActuelle;

    // Acceptation
    if (delta < 0) or (Random < Exp(-delta / temperature)) then
    begin
      itineraire := voisin;
      distActuelle := distVoisin;

      if distActuelle < distMeilleure then
      begin
        meilleurItineraire := itineraire;
        distMeilleure := distActuelle;
      end;
    end;

    temperature := temperature * 0.9995;
  end;

  Result := meilleurItineraire;
  WriteLn(Format('Meilleure distance trouvée : %.2f', [distMeilleure]));
end;
```

### Exemple 3 : Optimisation de portefeuille financier

```pascal
type
  TActif = record
    Nom: String;
    RendementEspere: TFloat;
    Risque: TFloat;  // Volatilité
  end;

function RendementPortefeuille(const poids: TVecteur;
                               const actifs: array of TActif): TFloat;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to High(actifs) do
    Result := Result + poids[i] * actifs[i].RendementEspere;
end;

function RisquePortefeuille(const poids: TVecteur;
                           const actifs: array of TActif): TFloat;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to High(actifs) do
    Result := Result + Sqr(poids[i] * actifs[i].Risque);
  Result := Sqrt(Result);
end;

procedure OptimiserPortefeuille;  
var
  actifs: array[0..4] of TActif;
  contraintes: TListeContraintes;
  f: TFonctionObjectif;
  x0: TVecteur;
  resultat: TResultatOptimisation;
  i: Integer;
  rendement, risque: TFloat;
begin
  // Définir les actifs
  actifs[0].Nom := 'Actions Tech';
  actifs[0].RendementEspere := 0.15;
  actifs[0].Risque := 0.25;

  actifs[1].Nom := 'Obligations';
  actifs[1].RendementEspere := 0.05;
  actifs[1].Risque := 0.05;

  actifs[2].Nom := 'Immobilier';
  actifs[2].RendementEspere := 0.10;
  actifs[2].Risque := 0.15;

  actifs[3].Nom := 'Or';
  actifs[3].RendementEspere := 0.07;
  actifs[3].Risque := 0.10;

  actifs[4].Nom := 'Liquidités';
  actifs[4].RendementEspere := 0.02;
  actifs[4].Risque := 0.01;

  // Objectif : minimiser le risque pour un rendement donné
  f := function(const x: TVecteur): TFloat
  begin
    Result := RisquePortefeuille(x, actifs);
  end;

  // Contraintes
  SetLength(contraintes, 2);

  // Contrainte 1 : somme des poids = 1
  contraintes[0].Type_ := ctEgalite;
  contraintes[0].Fonction := function(const x: TVecteur): TFloat
  var
    i: Integer;
  begin
    Result := 0;
    for i := 0 to High(x) do
      Result := Result + x[i];
  end;
  contraintes[0].Valeur := 1;

  // Contrainte 2 : rendement minimum de 8%
  contraintes[1].Type_ := ctInegalite;
  contraintes[1].Fonction := function(const x: TVecteur): TFloat
  begin
    Result := -RendementPortefeuille(x, actifs);
  end;
  contraintes[1].Valeur := -0.08;

  // Point initial (équipondéré)
  SetLength(x0, 5);
  for i := 0 to 4 do
    x0[i] := 0.2;

  // Optimiser
  resultat := OptimisationAvecPenalites(f, contraintes, x0);

  WriteLn('=== Portefeuille optimal ===');
  for i := 0 to 4 do
    WriteLn(Format('%s : %.1f%%', [actifs[i].Nom, resultat.Solution[i] * 100]));

  rendement := RendementPortefeuille(resultat.Solution, actifs);
  risque := RisquePortefeuille(resultat.Solution, actifs);

  WriteLn;
  WriteLn(Format('Rendement espéré : %.2f%%', [rendement * 100]));
  WriteLn(Format('Risque (volatilité) : %.2f%%', [risque * 100]));
end;
```

### Exemple 4 : Planification de production

```pascal
type
  TProduit = record
    Nom: String;
    Profit: TFloat;
    TempsFabrication: TFloat;
    MatierePremiereNecessaire: TFloat;
  end;

procedure OptimiserProduction;  
var
  produits: array[0..2] of TProduit;
  contraintes: TListeContraintes;
  f: TFonctionObjectif;
  x0: TVecteur;
  resultat: TResultatOptimisation;
  i: Integer;
const
  HEURES_DISPONIBLES = 100;
  MATIERE_DISPONIBLE = 500;
begin
  // Définir les produits
  produits[0].Nom := 'Produit A';
  produits[0].Profit := 50;
  produits[0].TempsFabrication := 2;
  produits[0].MatierePremiereNecessaire := 10;

  produits[1].Nom := 'Produit B';
  produits[1].Profit := 80;
  produits[1].TempsFabrication := 3;
  produits[1].MatierePremiereNecessaire := 15;

  produits[2].Nom := 'Produit C';
  produits[2].Profit := 120;
  produits[2].TempsFabrication := 5;
  produits[2].MatierePremiereNecessaire := 25;

  // Objectif : maximiser le profit (= minimiser -profit)
  f := function(const x: TVecteur): TFloat
  var
    i: Integer;
  begin
    Result := 0;
    for i := 0 to High(produits) do
      Result := Result - x[i] * produits[i].Profit;
  end;

  // Contraintes
  SetLength(contraintes, 2);

  // Contrainte 1 : temps de fabrication <= heures disponibles
  contraintes[0].Type_ := ctInegalite;
  contraintes[0].Fonction := function(const x: TVecteur): TFloat
  var
    i: Integer;
  begin
    Result := 0;
    for i := 0 to High(produits) do
      Result := Result + x[i] * produits[i].TempsFabrication;
  end;
  contraintes[0].Valeur := HEURES_DISPONIBLES;

  // Contrainte 2 : matière première <= disponible
  contraintes[1].Type_ := ctInegalite;
  contraintes[1].Fonction := function(const x: TVecteur): TFloat
  var
    i: Integer;
  begin
    Result := 0;
    for i := 0 to High(produits) do
      Result := Result + x[i] * produits[i].MatierePremiereNecessaire;
  end;
  contraintes[1].Valeur := MATIERE_DISPONIBLE;

  // Point initial
  SetLength(x0, 3);
  for i := 0 to 2 do
    x0[i] := 5;

  // Optimiser
  resultat := OptimisationAvecPenalites(f, contraintes, x0);

  WriteLn('=== Plan de production optimal ===');
  for i := 0 to 2 do
    WriteLn(Format('%s : %.0f unités',
      [produits[i].Nom, Max(0, resultat.Solution[i])]));

  WriteLn(Format('Profit total : %.2f €', [-resultat.ValeurOptimale]));
end;
```

## Visualisation avec TAChart

### Visualiser la convergence

```pascal
uses
  TAGraph, TASeries;

procedure VisualiserConvergence(Chart: TChart; const historique: array of TFloat);  
var
  Serie: TLineSeries;
  i: Integer;
begin
  Serie := TLineSeries.Create(Chart);
  Serie.Title := 'Convergence de l''algorithme';
  Serie.LinePen.Color := clBlue;

  for i := 0 to High(historique) do
    Serie.AddXY(i + 1, historique[i]);

  Chart.AddSeries(Serie);
  Chart.LeftAxis.Title.Caption := 'Valeur de la fonction objectif';
  Chart.BottomAxis.Title.Caption := 'Itération';
  Chart.LeftAxis.Logarithmic := True;  // Échelle log pour voir la convergence
end;
```

### Visualiser l'espace de recherche (2D)

```pascal
procedure VisualiserEspaceRecherche(Chart: TChart;
                                   f: TFonctionObjectif;
                                   xMin, xMax, yMin, yMax: TFloat;
                                   resolution: Integer = 50);
var
  Serie: TPointSeries;
  i, j: Integer;
  x, y, z: TFloat;
  params: TVecteur;
  maxZ, minZ: TFloat;
  couleur: TColor;
begin
  SetLength(params, 2);
  maxZ := -MaxDouble;
  minZ := MaxDouble;

  // Premier passage pour trouver min/max
  for i := 0 to resolution do
  begin
    for j := 0 to resolution do
    begin
      params[0] := xMin + i * (xMax - xMin) / resolution;
      params[1] := yMin + j * (yMax - yMin) / resolution;
      z := f(params);

      if z > maxZ then maxZ := z;
      if z < minZ then minZ := z;
    end;
  end;

  // Deuxième passage pour dessiner
  Serie := TPointSeries.Create(Chart);

  for i := 0 to resolution do
  begin
    for j := 0 to resolution do
    begin
      params[0] := xMin + i * (xMax - xMin) / resolution;
      params[1] := yMin + j * (yMax - yMin) / resolution;
      z := f(params);

      // Couleur basée sur la valeur
      couleur := ValeurVersCouleur(z, minZ, maxZ);

      Serie.AddXY(params[0], params[1]);
      Serie.GetColor := couleur;
    end;
  end;

  Chart.AddSeries(Serie);
end;

function ValeurVersCouleur(valeur, min, max: TFloat): TColor;  
var
  ratio: TFloat;
begin
  ratio := (valeur - min) / (max - min);

  if ratio < 0.5 then
    Result := RGB(0, Round(ratio * 512), 255)
  else
    Result := RGB(Round((ratio - 0.5) * 512), 255, 255 - Round((ratio - 0.5) * 512));
end;
```

## Comparaison des algorithmes

### Benchmark sur fonctions de test

```pascal
procedure BenchmarkAlgorithmes;  
var
  fonctions: array[0..3] of TFonctionObjectif;
  nomsfonctions: array[0..3] of String;
  x0: TVecteur;
  resultat: TResultatOptimisation;
  i: Integer;
  debut, fin: TDateTime;
begin
  // Définir les fonctions de test
  fonctions[0] := @Sphere;
  fonctions[1] := @Rosenbrock;
  fonctions[2] := @Rastrigin;
  fonctions[3] := @Ackley;

  nomsfonctions[0] := 'Sphere';
  nomsfonctions[1] := 'Rosenbrock';
  nomsfonctions[2] := 'Rastrigin';
  nomsfonctions[3] := 'Ackley';

  // Point initial
  SetLength(x0, 5);
  for i := 0 to 4 do
    x0[i] := Random * 10 - 5;

  WriteLn('=== Benchmark des algorithmes d''optimisation ===');
  WriteLn;

  for i := 0 to 3 do
  begin
    WriteLn('Fonction : ', nomsfonctions[i]);
    WriteLn('----------------------------------------');

    // Descente de gradient
    debut := Now;
    resultat := DescenteGradient(fonctions[i],
      function(const x: TVecteur): TVecteur
      begin
        Result := GradientNumerique(fonctions[i], x);
      end,
      x0, 0.01, 1000
    );
    fin := Now;

    WriteLn(Format('Descente gradient : f = %.6f, iter = %d, temps = %d ms',
      [resultat.ValeurOptimale, resultat.NbIterations,
       MilliSecondsBetween(fin, debut)]));

    // Recuit simulé
    debut := Now;
    resultat := RecuitSimule(fonctions[i], x0, 100, 0.95, 5000);
    fin := Now;

    WriteLn(Format('Recuit simulé     : f = %.6f, iter = %d, temps = %d ms',
      [resultat.ValeurOptimale, resultat.NbIterations,
       MilliSecondsBetween(fin, debut)]));

    WriteLn;
  end;
end;
```

## Considérations multi-plateformes

### Gestion du parallélisme

```pascal
{$IFDEF WINDOWS}
uses
  Windows;
{$ENDIF}

{$IFDEF UNIX}
uses
  BaseUnix, Unix;
{$ENDIF}

procedure OptimiserEnParallele(f: TFonctionObjectif;
                              nbThreads: Integer);
var
  threads: array of TThread;
  i: Integer;
begin
  SetLength(threads, nbThreads);

  for i := 0 to nbThreads - 1 do
  begin
    threads[i] := TThreadOptimisation.Create(f, i);
    threads[i].Start;
  end;

  // Attendre tous les threads
  for i := 0 to nbThreads - 1 do
    threads[i].WaitFor;
end;
```

### Utilisation de bibliothèques optimisées

```pascal
// Utiliser BLAS/LAPACK pour algèbre linéaire si disponible
{$IFDEF UNIX}
const
  BLAS_LIB = 'libblas.so.3';
{$ENDIF}

{$IFDEF WINDOWS}
const
  BLAS_LIB = 'libblas.dll';
{$ENDIF}

function UtiliserBLAS: Boolean;  
begin
  Result := FileExists(BLAS_LIB);
end;
```

## Bonnes pratiques

### Checklist pour optimisation

```pascal
(*
CHECKLIST OPTIMISATION :

1. Bien définir le problème
   □ Fonction objectif clairement définie
   □ Variables de décision identifiées
   □ Contraintes listées

2. Choisir l'algorithme approprié
   □ Gradient si différentiable
   □ Métaheuristique si non-différentiable
   □ Contraintes gérées correctement

3. Paramétrage
   □ Point initial raisonnable
   □ Taux d'apprentissage approprié
   □ Critères de convergence définis

4. Validation
   □ Tester sur fonctions connues
   □ Vérifier la convergence
   □ Comparer avec solutions analytiques si possible

5. Performance
   □ Profiler le code
   □ Utiliser bibliothèques optimisées
   □ Paralléliser si possible
*)
```

### Erreurs courantes

```pascal
(*
PIÈGES À ÉVITER :

1. Mauvais choix d'algorithme
   - Gradient pour fonctions non-différentiables
   - Algorithme déterministe pour fonctions multimodales

2. Paramètres inadéquats
   - Taux d'apprentissage trop grand (divergence)
   - Taux d'apprentissage trop petit (convergence lente)
   - Pas assez d'itérations

3. Minimum local vs global
   - Utiliser plusieurs points initiaux
   - Essayer métaheuristiques si suspicion de minima locaux

4. Contraintes mal gérées
   - Pénalités trop faibles (violation)
   - Pénalités trop fortes (instabilité numérique)

5. Échelle des variables
   - Normaliser les variables si nécessaire
   - Adapter taux d'apprentissage par dimension
*)
```

## Ressources et documentation

### Bibliothèques recommandées

**Pour FreePascal** :
- **NumLib** : Fonctions d'optimisation de base
- **LMath** : Optimisation et algèbre linéaire
- **Math Unit** : Fonctions mathématiques standard

**Interopérabilité** :
- **SciPy** (Python) : Bibliothèque complète d'optimisation
- **NLopt** : Bibliothèque C avec bindings
- **Optim** (R) : Optimisation statistique

### Lectures recommandées

**Livres** :
- "Numerical Optimization" - Nocedal & Wright
- "Introduction to Linear Optimization" - Bertsimas & Tsitsiklis
- "Algorithms for Optimization" - Kochenderfer & Wheeler

**Cours en ligne** :
- MIT OpenCourseWare : Nonlinear Optimization
- Coursera : Discrete Optimization
- Stanford : Convex Optimization

### Outils de visualisation

- **Desmos** : Visualiser fonctions 2D
- **GeoGebra** : Géométrie et optimisation
- **Wolfram Alpha** : Analyse de fonctions

## Conclusion

L'optimisation est un domaine vaste et puissant. Avec FreePascal, vous disposez de :

✓ **Algorithmes classiques** : Gradient, Newton, quasi-Newton  
✓ **Métaheuristiques** : Recuit simulé, génétique, essaims  
✓ **Gestion des contraintes** : Pénalités, barrières  
✓ **Performance** : Code compilé natif  
✓ **Portabilité** : Même code sur toutes plateformes

### Points clés à retenir

1. **Choix de l'algorithme** : Dépend de la fonction (différentiable, convexe, etc.)
2. **Point initial** : Crucial pour les algorithmes locaux
3. **Paramètres** : À ajuster selon le problème
4. **Validation** : Tester sur cas connus
5. **Contraintes** : Bien les modéliser
6. **Performance** : Profiler et optimiser

### Liens avec d'autres chapitres

- **16.4 Algèbre linéaire** : Résolution de systèmes, calculs matriciels
- **16.5 Statistiques** : Régression, estimation de paramètres
- **15. Intelligence artificielle** : Apprentissage automatique
- **20. Optimisation et performance** : Optimisation du code lui-même

**Bonne optimisation avec FreePascal !** 📈🎯💡

⏭️ [Calcul parallèle et vectorisation](/16-traitement-donnees-calcul-scientifique/08-calcul-parallele-vectorisation.md)
