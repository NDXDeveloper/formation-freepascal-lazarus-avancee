🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 16.5 Statistiques avancées

## Introduction

Les statistiques sont essentielles pour analyser des données, tirer des conclusions et prendre des décisions éclairées. Que vous analysiez des résultats d'expériences, des données de ventes, des mesures scientifiques ou des sondages, FreePascal offre tous les outils nécessaires pour effectuer des analyses statistiques robustes et professionnelles.

## Qu'est-ce que les statistiques ?

Les statistiques permettent de :

- **Décrire des données** : Résumer l'information (moyenne, variance, etc.)
- **Faire des inférences** : Tirer des conclusions sur une population à partir d'un échantillon
- **Tester des hypothèses** : Vérifier si une théorie est supportée par les données
- **Modéliser des relations** : Comprendre comment les variables sont liées
- **Prédire** : Estimer des valeurs futures basées sur des données passées

### Types d'analyses statistiques

1. **Statistiques descriptives** : Résumer et décrire les données
2. **Statistiques inférentielles** : Généraliser à partir d'échantillons
3. **Tests d'hypothèses** : Valider ou rejeter des suppositions
4. **Régression et corrélation** : Relations entre variables
5. **Analyse de variance (ANOVA)** : Comparer plusieurs groupes

## Types de données et structures

### Définition des types de base

```pascal
unit UnitStatistiques;

{$mode objfpc}{$H+}

interface

uses
  Math, SysUtils;

type
  TFloat = Double;
  TDonnees = array of TFloat;
  TMatriceDonnees = array of array of TFloat;

  // Résultats statistiques descriptifs
  TStatistiquesDescriptives = record
    Effectif: Integer;
    Minimum: TFloat;
    Maximum: TFloat;
    Etendue: TFloat;
    Moyenne: TFloat;
    Mediane: TFloat;
    Mode: TFloat;
    Variance: TFloat;
    EcartType: TFloat;
    EcartTypeEchantillon: TFloat;
    Asymetrie: TFloat;       // Skewness
    Aplatissement: TFloat;   // Kurtosis
    Q1: TFloat;              // Premier quartile
    Q3: TFloat;              // Troisième quartile
    EIQ: TFloat;             // Écart interquartile
  end;

  // Résultats de test d'hypothèse
  TResultatTest = record
    Statistique: TFloat;     // Valeur du test (t, z, F, etc.)
    ValeurP: TFloat;         // P-value
    DegreLiberte: Integer;
    EstSignificatif: Boolean;
    Seuil: TFloat;
  end;

  // Résultats de régression
  TResultatRegression = record
    Coefficients: TDonnees;  // β0, β1, β2, ...
    R2: TFloat;              // Coefficient de détermination
    R2Ajuste: TFloat;
    ErreurStandard: TFloat;
    StatistiqueF: TFloat;
    ValeurP: TFloat;
  end;

implementation

end.
```

## Statistiques descriptives

### Mesures de tendance centrale

#### Moyenne arithmétique

```pascal
function Moyenne(const donnees: TDonnees): TFloat;  
var
  i: Integer;
  somme: TFloat;
begin
  if Length(donnees) = 0 then
    raise Exception.Create('Tableau vide');

  somme := 0;
  for i := 0 to High(donnees) do
    somme := somme + donnees[i];

  Result := somme / Length(donnees);
end;
```

#### Moyenne pondérée

```pascal
function MoyennePonderee(const valeurs, poids: TDonnees): TFloat;  
var
  i: Integer;
  sommeValeurs, sommePoids: TFloat;
begin
  if Length(valeurs) <> Length(poids) then
    raise Exception.Create('Tableaux de tailles différentes');

  sommeValeurs := 0;
  sommePoids := 0;

  for i := 0 to High(valeurs) do
  begin
    sommeValeurs := sommeValeurs + valeurs[i] * poids[i];
    sommePoids := sommePoids + poids[i];
  end;

  if sommePoids = 0 then
    raise Exception.Create('Somme des poids nulle');

  Result := sommeValeurs / sommePoids;
end;
```

#### Médiane

La valeur centrale qui divise les données en deux moitiés égales.

```pascal
function Mediane(const donnees: TDonnees): TFloat;  
var
  tri: TDonnees;
  n: Integer;
begin
  if Length(donnees) = 0 then
    raise Exception.Create('Tableau vide');

  // Copier et trier les données
  tri := Copy(donnees);
  TrierDonnees(tri);

  n := Length(tri);

  if n mod 2 = 1 then
    // Nombre impair : prendre la valeur du milieu
    Result := tri[n div 2]
  else
    // Nombre pair : moyenne des deux valeurs centrales
    Result := (tri[n div 2 - 1] + tri[n div 2]) / 2;
end;

procedure TrierDonnees(var donnees: TDonnees);  
var
  i, j: Integer;
  temp: TFloat;
begin
  // Tri à bulles simple
  for i := 0 to High(donnees) - 1 do
    for j := i + 1 to High(donnees) do
      if donnees[i] > donnees[j] then
      begin
        temp := donnees[i];
        donnees[i] := donnees[j];
        donnees[j] := temp;
      end;
end;
```

#### Mode

La valeur la plus fréquente.

```pascal
function Mode(const donnees: TDonnees): TFloat;  
var
  i, j, compteur, maxCompteur: Integer;
begin
  if Length(donnees) = 0 then
    raise Exception.Create('Tableau vide');

  Result := donnees[0];
  maxCompteur := 0;

  for i := 0 to High(donnees) do
  begin
    compteur := 0;
    for j := 0 to High(donnees) do
      if Abs(donnees[i] - donnees[j]) < 1e-10 then
        Inc(compteur);

    if compteur > maxCompteur then
    begin
      maxCompteur := compteur;
      Result := donnees[i];
    end;
  end;
end;
```

### Mesures de dispersion

#### Variance et écart-type

```pascal
function Variance(const donnees: TDonnees; population: Boolean = True): TFloat;  
var
  i, n: Integer;
  moy, somme: TFloat;
begin
  n := Length(donnees);
  if n = 0 then
    raise Exception.Create('Tableau vide');

  moy := Moyenne(donnees);
  somme := 0;

  for i := 0 to High(donnees) do
    somme := somme + Sqr(donnees[i] - moy);

  if population then
    Result := somme / n
  else
  begin
    if n < 2 then
      raise Exception.Create('Échantillon trop petit');
    Result := somme / (n - 1);  // Correction de Bessel
  end;
end;

function EcartType(const donnees: TDonnees; population: Boolean = True): TFloat;  
begin
  Result := Sqrt(Variance(donnees, population));
end;
```

#### Étendue (Range)

```pascal
function Etendue(const donnees: TDonnees): TFloat;  
var
  min, max: TFloat;
  i: Integer;
begin
  if Length(donnees) = 0 then
    raise Exception.Create('Tableau vide');

  min := donnees[0];
  max := donnees[0];

  for i := 1 to High(donnees) do
  begin
    if donnees[i] < min then min := donnees[i];
    if donnees[i] > max then max := donnees[i];
  end;

  Result := max - min;
end;
```

#### Quartiles et écart interquartile

```pascal
function Quartile(const donnees: TDonnees; q: Integer): TFloat;  
var
  tri: TDonnees;
  position: TFloat;
  index: Integer;
begin
  if (q < 1) or (q > 3) then
    raise Exception.Create('Le quartile doit être 1, 2 ou 3');

  tri := Copy(donnees);
  TrierDonnees(tri);

  // Position du quartile
  position := q * (Length(tri) + 1) / 4;
  index := Trunc(position);

  if index >= Length(tri) then
    Result := tri[High(tri)]
  else if index <= 0 then
    Result := tri[0]
  else
    // Interpolation linéaire
    Result := tri[index - 1] +
              (position - index) * (tri[index] - tri[index - 1]);
end;

function EcartInterquartile(const donnees: TDonnees): TFloat;  
begin
  Result := Quartile(donnees, 3) - Quartile(donnees, 1);
end;
```

### Mesures de forme

#### Asymétrie (Skewness)

Mesure l'asymétrie de la distribution.

```pascal
function Asymetrie(const donnees: TDonnees): TFloat;  
var
  i, n: Integer;
  moy, ecart, somme: TFloat;
begin
  n := Length(donnees);
  if n < 3 then
    raise Exception.Create('Au moins 3 données nécessaires');

  moy := Moyenne(donnees);
  ecart := EcartType(donnees, False);

  if ecart = 0 then
    Exit(0);

  somme := 0;
  for i := 0 to High(donnees) do
    somme := somme + Power((donnees[i] - moy) / ecart, 3);

  Result := (n / ((n - 1) * (n - 2))) * somme;
end;

// Interprétation
procedure InterpreterAsymetrie(asymetrie: TFloat);  
begin
  if Abs(asymetrie) < 0.5 then
    WriteLn('Distribution approximativement symétrique')
  else if asymetrie > 0 then
    WriteLn('Distribution asymétrique à droite (queue à droite)')
  else
    WriteLn('Distribution asymétrique à gauche (queue à gauche)');
end;
```

#### Aplatissement (Kurtosis)

Mesure le caractère pointu de la distribution.

```pascal
function Aplatissement(const donnees: TDonnees): TFloat;  
var
  i, n: Integer;
  moy, ecart, somme: TFloat;
begin
  n := Length(donnees);
  if n < 4 then
    raise Exception.Create('Au moins 4 données nécessaires');

  moy := Moyenne(donnees);
  ecart := EcartType(donnees, False);

  if ecart = 0 then
    Exit(0);

  somme := 0;
  for i := 0 to High(donnees) do
    somme := somme + Power((donnees[i] - moy) / ecart, 4);

  // Kurtosis excès (normal = 0)
  Result := ((n * (n + 1)) / ((n - 1) * (n - 2) * (n - 3))) * somme
            - (3 * Sqr(n - 1)) / ((n - 2) * (n - 3));
end;

procedure InterpreterAplatissement(kurtosis: TFloat);  
begin
  if Abs(kurtosis) < 0.5 then
    WriteLn('Distribution mésokurtique (normale)')
  else if kurtosis > 0 then
    WriteLn('Distribution leptokurtique (pointue, queues épaisses)')
  else
    WriteLn('Distribution platykurtique (aplatie, queues fines)');
end;
```

### Fonction complète de statistiques descriptives

```pascal
function CalculerStatistiquesDescriptives(const donnees: TDonnees): TStatistiquesDescriptives;  
var
  i: Integer;
begin
  if Length(donnees) = 0 then
    raise Exception.Create('Aucune donnée');

  Result.Effectif := Length(donnees);

  // Minimum et maximum
  Result.Minimum := donnees[0];
  Result.Maximum := donnees[0];
  for i := 1 to High(donnees) do
  begin
    if donnees[i] < Result.Minimum then Result.Minimum := donnees[i];
    if donnees[i] > Result.Maximum then Result.Maximum := donnees[i];
  end;

  Result.Etendue := Result.Maximum - Result.Minimum;
  Result.Moyenne := Moyenne(donnees);
  Result.Mediane := Mediane(donnees);
  Result.Mode := Mode(donnees);
  Result.Variance := Variance(donnees, False);
  Result.EcartType := Sqrt(Result.Variance);
  Result.EcartTypeEchantillon := EcartType(donnees, False);

  if Length(donnees) >= 3 then
    Result.Asymetrie := Asymetrie(donnees)
  else
    Result.Asymetrie := 0;

  if Length(donnees) >= 4 then
    Result.Aplatissement := Aplatissement(donnees)
  else
    Result.Aplatissement := 0;

  Result.Q1 := Quartile(donnees, 1);
  Result.Q3 := Quartile(donnees, 3);
  Result.EIQ := Result.Q3 - Result.Q1;
end;

procedure AfficherStatistiquesDescriptives(const stats: TStatistiquesDescriptives);  
begin
  WriteLn('=== Statistiques Descriptives ===');
  WriteLn(Format('Effectif       : %d', [stats.Effectif]));
  WriteLn(Format('Minimum        : %.4f', [stats.Minimum]));
  WriteLn(Format('Q1             : %.4f', [stats.Q1]));
  WriteLn(Format('Médiane        : %.4f', [stats.Mediane]));
  WriteLn(Format('Q3             : %.4f', [stats.Q3]));
  WriteLn(Format('Maximum        : %.4f', [stats.Maximum]));
  WriteLn(Format('Étendue        : %.4f', [stats.Etendue]));
  WriteLn(Format('EIQ            : %.4f', [stats.EIQ]));
  WriteLn(Format('Moyenne        : %.4f', [stats.Moyenne]));
  WriteLn(Format('Mode           : %.4f', [stats.Mode]));
  WriteLn(Format('Variance       : %.4f', [stats.Variance]));
  WriteLn(Format('Écart-type     : %.4f', [stats.EcartType]));
  WriteLn(Format('Asymétrie      : %.4f', [stats.Asymetrie]));
  WriteLn(Format('Aplatissement  : %.4f', [stats.Aplatissement]));
  WriteLn;
end;
```

## Distributions de probabilité

### Distribution normale (Gaussienne)

```pascal
uses
  Math;

// Fonction de densité de probabilité (PDF)
function PDFNormale(x, moyenne, ecartType: TFloat): TFloat;  
var
  z: TFloat;
begin
  z := (x - moyenne) / ecartType;
  Result := (1 / (ecartType * Sqrt(2 * Pi))) * Exp(-0.5 * Sqr(z));
end;

// Fonction de répartition (CDF) - approximation
function CDFNormale(x, moyenne, ecartType: TFloat): TFloat;  
var
  z, t, p: TFloat;
const
  A1 =  0.254829592;
  A2 = -0.284496736;
  A3 =  1.421413741;
  A4 = -1.453152027;
  A5 =  1.061405429;
  P  =  0.3275911;
begin
  z := (x - moyenne) / ecartType;

  if z < 0 then
  begin
    Result := 1 - CDFNormale(-z, 0, 1);
    Exit;
  end;

  t := 1 / (1 + P * z);
  p := 1 - (((((A5 * t + A4) * t) + A3) * t + A2) * t + A1) * t * Exp(-z * z / 2);

  Result := p;
end;

// Génération de nombres aléatoires selon loi normale (Box-Muller)
function AleatoireNormal(moyenne, ecartType: TFloat): TFloat;  
var
  u1, u2, z: TFloat;
begin
  u1 := Random;
  u2 := Random;

  z := Sqrt(-2 * Ln(u1)) * Cos(2 * Pi * u2);
  Result := moyenne + ecartType * z;
end;
```

### Distribution de Student (t)

```pascal
// Fonction de densité de Student
function PDFStudent(t: TFloat; degreLiberte: Integer): TFloat;  
var
  gamma1, gamma2: TFloat;
begin
  gamma1 := GammaLn((degreLiberte + 1) / 2);
  gamma2 := GammaLn(degreLiberte / 2);

  Result := Exp(gamma1 - gamma2) /
            (Sqrt(degreLiberte * Pi) *
             Power(1 + Sqr(t) / degreLiberte, (degreLiberte + 1) / 2));
end;

// Valeur critique de Student (approximation)
function ValeurCritiqueStudent(alpha: TFloat; degreLiberte: Integer): TFloat;  
begin
  // Approximation simple - pour production, utiliser table complète
  if degreLiberte >= 30 then
    Result := ValeurCritiqueNormale(alpha)
  else
  begin
    // Approximation basique
    case degreLiberte of
      1: Result := 12.706;
      2: Result := 4.303;
      3: Result := 3.182;
      5: Result := 2.571;
      10: Result := 2.228;
      20: Result := 2.086;
    else
      Result := 2.0;  // Approximation
    end;
  end;
end;

function ValeurCritiqueNormale(alpha: TFloat): TFloat;  
begin
  // Pour alpha = 0.05 (bilatéral), z = 1.96
  if Abs(alpha - 0.05) < 0.001 then
    Result := 1.96
  else if Abs(alpha - 0.01) < 0.001 then
    Result := 2.576
  else
    // Approximation pour autres valeurs
    Result := Sqrt(2) * ErfInv(1 - alpha);
end;
```

### Distribution du Chi-carré (χ²)

```pascal
function PDFChiCarre(x: TFloat; degreLiberte: Integer): TFloat;  
var
  k: TFloat;
begin
  if x <= 0 then
    Exit(0);

  k := degreLiberte / 2;
  Result := (Power(x, k - 1) * Exp(-x / 2)) /
            (Power(2, k) * Exp(GammaLn(k)));
end;
```

### Distribution F de Fisher

```pascal
function PDFF(x: TFloat; dl1, dl2: Integer): TFloat;  
var
  d1, d2: TFloat;
begin
  if x <= 0 then
    Exit(0);

  d1 := dl1 / 2;
  d2 := dl2 / 2;

  Result := (Exp(GammaLn(d1 + d2) - GammaLn(d1) - GammaLn(d2)) *
            Power(dl1 / dl2, d1) * Power(x, d1 - 1)) /
            Power(1 + (dl1 * x) / dl2, d1 + d2);
end;
```

## Corrélation et covariance

### Covariance

```pascal
function Covariance(const x, y: TDonnees; population: Boolean = True): TFloat;  
var
  i, n: Integer;
  moyX, moyY, somme: TFloat;
begin
  n := Length(x);
  if n <> Length(y) then
    raise Exception.Create('Tableaux de tailles différentes');

  if n = 0 then
    raise Exception.Create('Tableaux vides');

  moyX := Moyenne(x);
  moyY := Moyenne(y);

  somme := 0;
  for i := 0 to n - 1 do
    somme := somme + (x[i] - moyX) * (y[i] - moyY);

  if population then
    Result := somme / n
  else
  begin
    if n < 2 then
      raise Exception.Create('Échantillon trop petit');
    Result := somme / (n - 1);
  end;
end;
```

### Coefficient de corrélation de Pearson

Mesure la relation linéaire entre deux variables (-1 ≤ r ≤ 1).

```pascal
function CorrelationPearson(const x, y: TDonnees): TFloat;  
var
  cov, ecartX, ecartY: TFloat;
begin
  cov := Covariance(x, y, False);
  ecartX := EcartType(x, False);
  ecartY := EcartType(y, False);

  if (ecartX = 0) or (ecartY = 0) then
    raise Exception.Create('Écart-type nul');

  Result := cov / (ecartX * ecartY);
end;

procedure InterpreterCorrelation(r: TFloat);  
begin
  WriteLn(Format('Corrélation de Pearson : r = %.4f', [r]));

  if Abs(r) < 0.3 then
    WriteLn('Corrélation faible')
  else if Abs(r) < 0.7 then
    WriteLn('Corrélation modérée')
  else
    WriteLn('Corrélation forte');

  if r > 0 then
    WriteLn('Relation positive (augmentent ensemble)')
  else if r < 0 then
    WriteLn('Relation négative (l''une augmente, l''autre diminue)');
end;
```

### Coefficient de corrélation de Spearman

Pour relations non linéaires (utilise les rangs).

```pascal
function RangsDonnees(const donnees: TDonnees): TDonnees;  
var
  i, j: Integer;
  tri: TDonnees;
  rang: Integer;
begin
  SetLength(Result, Length(donnees));
  tri := Copy(donnees);
  TrierDonnees(tri);

  for i := 0 to High(donnees) do
  begin
    rang := 0;
    for j := 0 to High(tri) do
      if donnees[i] >= tri[j] then
        Inc(rang);
    Result[i] := rang;
  end;
end;

function CorrelationSpearman(const x, y: TDonnees): TFloat;  
var
  rangsX, rangsY: TDonnees;
begin
  rangsX := RangsDonnees(x);
  rangsY := RangsDonnees(y);
  Result := CorrelationPearson(rangsX, rangsY);
end;
```

### Matrice de corrélation

```pascal
function MatriceCorrelation(const donnees: TMatriceDonnees): TMatriceDonnees;  
var
  i, j, nbVariables: Integer;
  colonneI, colonneJ: TDonnees;
begin
  nbVariables := Length(donnees[0]);
  SetLength(Result, nbVariables, nbVariables);

  for i := 0 to nbVariables - 1 do
  begin
    for j := 0 to nbVariables - 1 do
    begin
      if i = j then
        Result[i, j] := 1.0
      else
      begin
        // Extraire les colonnes
        colonneI := ExtraireColonne(donnees, i);
        colonneJ := ExtraireColonne(donnees, j);
        Result[i, j] := CorrelationPearson(colonneI, colonneJ);
      end;
    end;
  end;
end;

function ExtraireColonne(const donnees: TMatriceDonnees;
                        indiceColonne: Integer): TDonnees;
var
  i: Integer;
begin
  SetLength(Result, Length(donnees));
  for i := 0 to High(donnees) do
    Result[i] := donnees[i, indiceColonne];
end;
```

## Tests d'hypothèses

### Test t de Student (un échantillon)

Tester si la moyenne d'un échantillon diffère d'une valeur théorique.

```pascal
function TestT UnEchantillon(const donnees: TDonnees;
                            moyenneTheorique: TFloat;
                            alpha: TFloat = 0.05): TResultatTest;
var
  n: Integer;
  moy, ecart, erreurStandard: TFloat;
begin
  n := Length(donnees);
  if n < 2 then
    raise Exception.Create('Échantillon trop petit');

  moy := Moyenne(donnees);
  ecart := EcartType(donnees, False);
  erreurStandard := ecart / Sqrt(n);

  // Statistique t
  Result.Statistique := (moy - moyenneTheorique) / erreurStandard;
  Result.DegreLiberte := n - 1;

  // Valeur critique (bilatéral)
  Result.Seuil := ValeurCritiqueStudent(alpha, n - 1);

  // Test bilatéral
  Result.EstSignificatif := Abs(Result.Statistique) > Result.Seuil;

  // Approximation de la p-value (simplifiée)
  if Abs(Result.Statistique) > 3 then
    Result.ValeurP := 0.001
  else if Abs(Result.Statistique) > 2 then
    Result.ValeurP := 0.05
  else
    Result.ValeurP := 0.1;
end;

procedure AfficherResultatTest(const resultat: TResultatTest; nomTest: String);  
begin
  WriteLn('=== ', nomTest, ' ===');
  WriteLn(Format('Statistique    : %.4f', [resultat.Statistique]));
  WriteLn(Format('Degré liberté  : %d', [resultat.DegreLiberte]));
  WriteLn(Format('Valeur P       : %.4f', [resultat.ValeurP]));
  WriteLn(Format('Seuil (α=0.05) : %.4f', [resultat.Seuil]));

  if resultat.EstSignificatif then
    WriteLn('Résultat : SIGNIFICATIF - Rejeter H0')
  else
    WriteLn('Résultat : NON SIGNIFICATIF - Ne pas rejeter H0');
  WriteLn;
end;
```

### Test t de Student (deux échantillons indépendants)

```pascal
function TestTDeuxEchantillons(const groupe1, groupe2: TDonnees;
                              alpha: TFloat = 0.05): TResultatTest;
var
  n1, n2: Integer;
  moy1, moy2, var1, var2, varPooled, se: TFloat;
begin
  n1 := Length(groupe1);
  n2 := Length(groupe2);

  if (n1 < 2) or (n2 < 2) then
    raise Exception.Create('Échantillons trop petits');

  moy1 := Moyenne(groupe1);
  moy2 := Moyenne(groupe2);
  var1 := Variance(groupe1, False);
  var2 := Variance(groupe2, False);

  // Variance poolée
  varPooled := ((n1 - 1) * var1 + (n2 - 1) * var2) / (n1 + n2 - 2);

  // Erreur standard de la différence
  se := Sqrt(varPooled * (1 / n1 + 1 / n2));

  // Statistique t
  Result.Statistique := (moy1 - moy2) / se;
  Result.DegreLiberte := n1 + n2 - 2;

  Result.Seuil := ValeurCritiqueStudent(alpha, Result.DegreLiberte);
  Result.EstSignificatif := Abs(Result.Statistique) > Result.Seuil;

  // Approximation p-value
  if Abs(Result.Statistique) > 3 then
    Result.ValeurP := 0.001
  else if Abs(Result.Statistique) > 2 then
    Result.ValeurP := 0.05
  else
    Result.ValeurP := 0.1;
end;
```

### Test t apparié

Pour échantillons appariés (avant/après).

```pascal
function TestTApparie(const avant, apres: TDonnees;
                     alpha: TFloat = 0.05): TResultatTest;
var
  differences: TDonnees;
  i: Integer;
begin
  if Length(avant) <> Length(apres) then
    raise Exception.Create('Tableaux de tailles différentes');

  // Calculer les différences
  SetLength(differences, Length(avant));
  for i := 0 to High(avant) do
    differences[i] := apres[i] - avant[i];

  // Test t sur les différences (H0: moyenne des différences = 0)
  Result := TestTUnEchantillon(differences, 0, alpha);
end;

// Exemple d'utilisation
procedure ExempleTestTApparie;  
var
  avant, apres: TDonnees;
  resultat: TResultatTest;
begin
  // Données : pression artérielle avant et après traitement
  SetLength(avant, 10);
  SetLength(apres, 10);

  avant[0] := 145; apres[0] := 138;
  avant[1] := 152; apres[1] := 142;
  avant[2] := 148; apres[2] := 140;
  avant[3] := 160; apres[3] := 151;
  avant[4] := 155; apres[4] := 146;
  avant[5] := 142; apres[5] := 139;
  avant[6] := 158; apres[6] := 149;
  avant[7] := 147; apres[7] := 143;
  avant[8] := 153; apres[8] := 145;
  avant[9] := 149; apres[9] := 144;

  resultat := TestTApparie(avant, apres);
  AfficherResultatTest(resultat, 'Test t apparié - Effet du traitement');
end;
```

### Test du Chi-carré (χ²) d'indépendance

Tester l'indépendance entre deux variables catégorielles.

```pascal
type
  TTableContingence = array of array of Integer;

function TestChiCarreIndependance(const tableau: TTableContingence;
                                 alpha: TFloat = 0.05): TResultatTest;
var
  lignes, colonnes, i, j: Integer;
  totauxLignes, totauxColonnes: array of Integer;
  total: Integer;
  attendu, chiCarre: TFloat;
begin
  lignes := Length(tableau);
  colonnes := Length(tableau[0]);

  SetLength(totauxLignes, lignes);
  SetLength(totauxColonnes, colonnes);

  // Calculer les totaux
  total := 0;
  for i := 0 to lignes - 1 do
  begin
    totauxLignes[i] := 0;
    for j := 0 to colonnes - 1 do
    begin
      totauxLignes[i] := totauxLignes[i] + tableau[i, j];
      totauxColonnes[j] := totauxColonnes[j] + tableau[i, j];
      total := total + tableau[i, j];
    end;
  end;

  // Calculer χ²
  chiCarre := 0;
  for i := 0 to lignes - 1 do
  begin
    for j := 0 to colonnes - 1 do
    begin
      attendu := (totauxLignes[i] * totauxColonnes[j]) / total;
      if attendu > 0 then
        chiCarre := chiCarre + Sqr(tableau[i, j] - attendu) / attendu;
    end;
  end;

  Result.Statistique := chiCarre;
  Result.DegreLiberte := (lignes - 1) * (colonnes - 1);

  // Valeur critique du χ² (approximation)
  Result.Seuil := ValeurCritiqueChiCarre(alpha, Result.DegreLiberte);
  Result.EstSignificatif := Result.Statistique > Result.Seuil;

  if Result.Statistique > 10 then
    Result.ValeurP := 0.001
  else if Result.Statistique > 5.99 then
    Result.ValeurP := 0.05
  else
    Result.ValeurP := 0.1;
end;

function ValeurCritiqueChiCarre(alpha: TFloat; dl: Integer): TFloat;  
begin
  // Approximation simple - pour alpha = 0.05
  case dl of
    1: Result := 3.841;
    2: Result := 5.991;
    3: Result := 7.815;
    4: Result := 9.488;
    5: Result := 11.070;
  else
    Result := dl + 2 * Sqrt(2 * dl);  // Approximation
  end;
end;
```

### Test de normalité (Shapiro-Wilk simplifié)

```pascal
function TestNormalite(const donnees: TDonnees): Boolean;  
var
  stats: TStatistiquesDescriptives;
  asymetrie, aplatissement: TFloat;
begin
  stats := CalculerStatistiquesDescriptives(donnees);

  asymetrie := Abs(stats.Asymetrie);
  aplatissement := Abs(stats.Aplatissement);

  // Critères simples
  Result := (asymetrie < 2) and (aplatissement < 7);

  if Result then
    WriteLn('Les données semblent suivre une distribution normale')
  else
    WriteLn('Les données ne semblent pas suivre une distribution normale');
end;
```

## Régression linéaire

### Régression linéaire simple (y = a + bx)

```pascal
type
  TRegressionSimple = record
    Intercept: TFloat;      // a (ordonnée à l'origine)
    Pente: TFloat;          // b (coefficient directeur)
    R2: TFloat;             // Coefficient de détermination
    ErreurStandard: TFloat;
    StatistiqueT: TFloat;
    ValeurP: TFloat;
  end;

function RegressionLineaireSimple(const x, y: TDonnees): TRegressionSimple;  
var
  n, i: Integer;
  sommeX, sommeY, sommeXY, sommeX2: TFloat;
  moyX, moyY, sxx, sxy, sse, sst: TFloat;
  yPred, residu: TFloat;
begin
  n := Length(x);
  if n <> Length(y) then
    raise Exception.Create('Tableaux de tailles différentes');

  if n < 3 then
    raise Exception.Create('Au moins 3 points nécessaires');

  // Calculer les sommes
  sommeX := 0;
  sommeY := 0;
  sommeXY := 0;
  sommeX2 := 0;

  for i := 0 to n - 1 do
  begin
    sommeX := sommeX + x[i];
    sommeY := sommeY + y[i];
    sommeXY := sommeXY + x[i] * y[i];
    sommeX2 := sommeX2 + Sqr(x[i]);
  end;

  moyX := sommeX / n;
  moyY := sommeY / n;

  // Calculer les coefficients
  sxx := sommeX2 - n * Sqr(moyX);
  sxy := sommeXY - n * moyX * moyY;

  Result.Pente := sxy / sxx;
  Result.Intercept := moyY - Result.Pente * moyX;

  // Calculer R²
  sse := 0;  // Somme des carrés des erreurs
  sst := 0;  // Somme totale des carrés

  for i := 0 to n - 1 do
  begin
    yPred := Result.Intercept + Result.Pente * x[i];
    residu := y[i] - yPred;
    sse := sse + Sqr(residu);
    sst := sst + Sqr(y[i] - moyY);
  end;

  Result.R2 := 1 - (sse / sst);
  Result.ErreurStandard := Sqrt(sse / (n - 2));

  // Statistique t pour la pente
  Result.StatistiqueT := Result.Pente / (Result.ErreurStandard / Sqrt(sxx));

  // Approximation p-value
  if Abs(Result.StatistiqueT) > 3 then
    Result.ValeurP := 0.001
  else if Abs(Result.StatistiqueT) > 2 then
    Result.ValeurP := 0.05
  else
    Result.ValeurP := 0.1;
end;

procedure AfficherRegressionSimple(const reg: TRegressionSimple);  
begin
  WriteLn('=== Régression Linéaire Simple ===');
  WriteLn(Format('Équation : y = %.4f + %.4f × x',
                [reg.Intercept, reg.Pente]));
  WriteLn(Format('R²                : %.4f', [reg.R2]));
  WriteLn(Format('Erreur standard   : %.4f', [reg.ErreurStandard]));
  WriteLn(Format('t (pente)         : %.4f', [reg.StatistiqueT]));
  WriteLn(Format('p-value           : %.4f', [reg.ValeurP]));

  if reg.ValeurP < 0.05 then
    WriteLn('La relation est statistiquement significative')
  else
    WriteLn('La relation n''est pas statistiquement significative');

  WriteLn;
end;
```

### Prédiction avec intervalle de confiance

```pascal
function PredireAvecIntervalle(const reg: TRegressionSimple;
                              const x, donnees: TDonnees;
                              xNouveau: TFloat;
                              niveau: TFloat = 0.95): record
  Prediction: TFloat;
  IC_Inf: TFloat;
  IC_Sup: TFloat;
end;  
var
  n: Integer;
  moyX, sxx, erreur: TFloat;
  t: TFloat;
  i: Integer;
begin
  n := Length(x);

  // Calculer la moyenne de x et sxx
  moyX := Moyenne(x);
  sxx := 0;
  for i := 0 to n - 1 do
    sxx := sxx + Sqr(x[i] - moyX);

  // Prédiction ponctuelle
  Result.Prediction := reg.Intercept + reg.Pente * xNouveau;

  // Erreur standard de la prédiction
  erreur := reg.ErreurStandard *
            Sqrt(1 + 1/n + Sqr(xNouveau - moyX) / sxx);

  // Valeur t critique
  t := ValeurCritiqueStudent(1 - niveau, n - 2);

  // Intervalle de confiance
  Result.IC_Inf := Result.Prediction - t * erreur;
  Result.IC_Sup := Result.Prediction + t * erreur;
end;
```

### Régression linéaire multiple

```pascal
uses
  UnitAlgebreLineaire;  // Pour manipulation de matrices

function RegressionLineaireMultiple(const x: TMatrix;
                                   const y: TVector): TResultatRegression;
var
  n, p, i: Integer;
  xT, xTx, xTxInv: TMatrix;
  xTy, yPred, residus: TVector;
  sse, sst, moyY: TFloat;
begin
  n := Length(x);      // Nombre d'observations
  p := Length(x[0]);   // Nombre de variables

  // X^T
  xT := Transposee(x);

  // X^T × X
  xTx := MultiplicationMatrices(xT, x);

  // (X^T × X)^(-1)
  xTxInv := InverseMatrice(xTx);

  // X^T × y
  xTy := MultiplicationMatriceVecteur(xT, y);

  // β = (X^T × X)^(-1) × X^T × y
  Result.Coefficients := MultiplicationMatriceVecteur(xTxInv, xTy);

  // Calculer les prédictions
  yPred := MultiplicationMatriceVecteur(x, Result.Coefficients);

  // Calculer les résidus et R²
  moyY := Moyenne(y);
  sse := 0;
  sst := 0;

  for i := 0 to n - 1 do
  begin
    sse := sse + Sqr(y[i] - yPred[i]);
    sst := sst + Sqr(y[i] - moyY);
  end;

  Result.R2 := 1 - (sse / sst);
  Result.R2Ajuste := 1 - (1 - Result.R2) * (n - 1) / (n - p - 1);
  Result.ErreurStandard := Sqrt(sse / (n - p));

  // Statistique F
  Result.StatistiqueF := (Result.R2 / p) / ((1 - Result.R2) / (n - p - 1));

  if Result.StatistiqueF > 5 then
    Result.ValeurP := 0.001
  else if Result.StatistiqueF > 3 then
    Result.ValeurP := 0.05
  else
    Result.ValeurP := 0.1;
end;
```

### Régression polynomiale

```pascal
function RegressionPolynomiale(const x, y: TDonnees;
                              degre: Integer): TDonnees;
var
  xMatrix: TMatrix;
  i, j, n: Integer;
begin
  n := Length(x);

  // Créer la matrice de design (matrice de Vandermonde)
  SetLength(xMatrix, n, degre + 1);

  for i := 0 to n - 1 do
  begin
    for j := 0 to degre do
      xMatrix[i, j] := Power(x[i], j);
  end;

  // Résoudre par régression multiple
  Result := RegressionLineaireMultiple(xMatrix, y).Coefficients;
end;

function EvaluerPolynome(const coefficients: TDonnees; x: TFloat): TFloat;  
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to High(coefficients) do
    Result := Result + coefficients[i] * Power(x, i);
end;
```

## ANOVA (Analyse de variance)

### ANOVA à un facteur

Comparer les moyennes de plusieurs groupes.

```pascal
type
  TResultatANOVA = record
    SommesCarresInter: TFloat;    // Between groups
    SommesCarresIntra: TFloat;    // Within groups
    SommesCarresTotal: TFloat;
    DLInter: Integer;             // Degrés de liberté inter-groupes
    DLIntra: Integer;             // Degrés de liberté intra-groupe
    MoyenneCarresInter: TFloat;
    MoyenneCarresIntra: TFloat;
    StatistiqueF: TFloat;
    ValeurP: TFloat;
    EstSignificatif: Boolean;
  end;

function ANOVAUnFacteur(const groupes: array of TDonnees;
                       alpha: TFloat = 0.05): TResultatANOVA;
var
  k, i, j, n, ni: Integer;
  moyenneGenerale, moyenneGroupe: TFloat;
  sommeTotal: TFloat;
begin
  k := Length(groupes);  // Nombre de groupes
  if k < 2 then
    raise Exception.Create('Au moins 2 groupes nécessaires');

  // Calculer la moyenne générale et n total
  sommeTotal := 0;
  n := 0;
  for i := 0 to k - 1 do
  begin
    for j := 0 to High(groupes[i]) do
      sommeTotal := sommeTotal + groupes[i, j];
    n := n + Length(groupes[i]);
  end;
  moyenneGenerale := sommeTotal / n;

  // Calculer les sommes des carrés
  Result.SommesCarresInter := 0;
  Result.SommesCarresIntra := 0;
  Result.SommesCarresTotal := 0;

  for i := 0 to k - 1 do
  begin
    ni := Length(groupes[i]);
    moyenneGroupe := Moyenne(groupes[i]);

    // SS inter-groupes
    Result.SommesCarresInter := Result.SommesCarresInter +
                                ni * Sqr(moyenneGroupe - moyenneGenerale);

    // SS intra-groupe
    for j := 0 to ni - 1 do
    begin
      Result.SommesCarresIntra := Result.SommesCarresIntra +
                                  Sqr(groupes[i, j] - moyenneGroupe);
      Result.SommesCarresTotal := Result.SommesCarresTotal +
                                  Sqr(groupes[i, j] - moyenneGenerale);
    end;
  end;

  // Degrés de liberté
  Result.DLInter := k - 1;
  Result.DLIntra := n - k;

  // Moyennes des carrés
  Result.MoyenneCarresInter := Result.SommesCarresInter / Result.DLInter;
  Result.MoyenneCarresIntra := Result.SommesCarresIntra / Result.DLIntra;

  // Statistique F
  Result.StatistiqueF := Result.MoyenneCarresInter / Result.MoyenneCarresIntra;

  // Valeur critique et décision
  // Approximation simple
  if Result.StatistiqueF > 5 then
  begin
    Result.ValeurP := 0.001;
    Result.EstSignificatif := True;
  end
  else if Result.StatistiqueF > 3 then
  begin
    Result.ValeurP := 0.05;
    Result.EstSignificatif := True;
  end
  else
  begin
    Result.ValeurP := 0.1;
    Result.EstSignificatif := False;
  end;
end;

procedure AfficherResultatANOVA(const anova: TResultatANOVA);  
begin
  WriteLn('=== ANOVA à un facteur ===');
  WriteLn;
  WriteLn('Source          SC        DL      MC        F        P');
  WriteLn('--------------------------------------------------------');
  WriteLn(Format('Inter-groupes %8.2f  %3d  %8.2f  %6.2f  %.4f',
    [anova.SommesCarresInter, anova.DLInter,
     anova.MoyenneCarresInter, anova.StatistiqueF, anova.ValeurP]));
  WriteLn(Format('Intra-groupe  %8.2f  %3d  %8.2f',
    [anova.SommesCarresIntra, anova.DLIntra, anova.MoyenneCarresIntra]));
  WriteLn(Format('Total         %8.2f  %3d',
    [anova.SommesCarresTotal, anova.DLInter + anova.DLIntra]));
  WriteLn('--------------------------------------------------------');

  if anova.EstSignificatif then
    WriteLn('Résultat : SIGNIFICATIF - Les moyennes diffèrent')
  else
    WriteLn('Résultat : NON SIGNIFICATIF - Pas de différence significative');
  WriteLn;
end;
```

### Tests post-hoc (Tukey HSD)

Comparer les paires de groupes après ANOVA significative.

```pascal
function TestTukeyHSD(const groupes: array of TDonnees;
                     alpha: TFloat = 0.05): TMatrix;
var
  k, i, j: Integer;
  moyennes: TDonnees;
  mse, hsd, diff: TFloat;
  n: Integer;
begin
  k := Length(groupes);
  SetLength(Result, k, k);
  SetLength(moyennes, k);

  // Calculer les moyennes
  for i := 0 to k - 1 do
    moyennes[i] := Moyenne(groupes[i]);

  // MSE (Mean Square Error) de l'ANOVA
  mse := ANOVAUnFacteur(groupes).MoyenneCarresIntra;

  // n moyen (simplifié - devrait tenir compte des tailles inégales)
  n := Length(groupes[0]);

  // Différence honnêtement significative
  // hsd = q × sqrt(MSE / n)
  // q dépend de k et des degrés de liberté (table de Tukey)
  hsd := 3.5 * Sqrt(mse / n);  // Approximation

  // Comparer toutes les paires
  for i := 0 to k - 1 do
  begin
    for j := 0 to k - 1 do
    begin
      diff := Abs(moyennes[i] - moyennes[j]);
      if diff > hsd then
        Result[i, j] := 1  // Différence significative
      else
        Result[i, j] := 0;
    end;
  end;
end;
```

## Intervalle de confiance

### Intervalle de confiance pour la moyenne

```pascal
function IntervalleConfianceMoyenne(const donnees: TDonnees;
                                   niveau: TFloat = 0.95): record
  Moyenne: TFloat;
  IC_Inf: TFloat;
  IC_Sup: TFloat;
  MargeErreur: TFloat;
end;  
var
  n: Integer;
  moy, ecart, erreurStandard, t: TFloat;
begin
  n := Length(donnees);
  if n < 2 then
    raise Exception.Create('Au moins 2 données nécessaires');

  moy := Moyenne(donnees);
  ecart := EcartType(donnees, False);
  erreurStandard := ecart / Sqrt(n);

  // Valeur t critique
  t := ValeurCritiqueStudent(1 - niveau, n - 1);

  Result.Moyenne := moy;
  Result.MargeErreur := t * erreurStandard;
  Result.IC_Inf := moy - Result.MargeErreur;
  Result.IC_Sup := moy + Result.MargeErreur;
end;

procedure AfficherIntervalleConfiance(const ic: record
  Moyenne: TFloat;
  IC_Inf: TFloat;
  IC_Sup: TFloat;
  MargeErreur: TFloat;
end; niveau: TFloat);  
begin
  WriteLn(Format('Moyenne : %.4f', [ic.Moyenne]));
  WriteLn(Format('IC à %d%% : [%.4f ; %.4f]',
    [Round(niveau * 100), ic.IC_Inf, ic.IC_Sup]));
  WriteLn(Format('Marge d''erreur : ± %.4f', [ic.MargeErreur]));
end;
```

### Intervalle de confiance pour une proportion

```pascal
function IntervalleConfianceProportion(succes, total: Integer;
                                      niveau: TFloat = 0.95): record
  Proportion: TFloat;
  IC_Inf: TFloat;
  IC_Sup: TFloat;
end;  
var
  p, erreurStandard, z: TFloat;
begin
  if total = 0 then
    raise Exception.Create('Total nul');

  p := succes / total;
  erreurStandard := Sqrt(p * (1 - p) / total);

  // Approximation normale
  z := ValeurCritiqueNormale(1 - niveau);

  Result.Proportion := p;
  Result.IC_Inf := p - z * erreurStandard;
  Result.IC_Sup := p + z * erreurStandard;

  // Borner entre 0 et 1
  if Result.IC_Inf < 0 then Result.IC_Inf := 0;
  if Result.IC_Sup > 1 then Result.IC_Sup := 1;
end;
```

## Taille d'échantillon

### Calculer la taille d'échantillon nécessaire

```pascal
function TailleEchantillonMoyenne(ecartType, margeErreur: TFloat;
                                 niveau: TFloat = 0.95): Integer;
var
  z: TFloat;
begin
  z := ValeurCritiqueNormale(1 - niveau);
  Result := Ceil(Sqr(z * ecartType / margeErreur));
end;

function TailleEchantillonProportion(proportion, margeErreur: TFloat;
                                    niveau: TFloat = 0.95): Integer;
var
  z, p: TFloat;
begin
  z := ValeurCritiqueNormale(1 - niveau);

  if proportion <= 0 then
    p := 0.5  // Pire cas : variance maximale
  else
    p := proportion;

  Result := Ceil(Sqr(z) * p * (1 - p) / Sqr(margeErreur));
end;
```

## Bootstrap et rééchantillonnage

### Méthode du bootstrap

Estimation de la distribution d'une statistique par rééchantillonnage.

```pascal
function Bootstrap(const donnees: TDonnees;
                  nbEchantillons: Integer;
                  statistique: function(const d: TDonnees): TFloat): TDonnees;
var
  i, j, n: Integer;
  echantillon: TDonnees;
begin
  n := Length(donnees);
  SetLength(Result, nbEchantillons);
  SetLength(echantillon, n);

  Randomize;

  for i := 0 to nbEchantillons - 1 do
  begin
    // Créer un échantillon avec remise
    for j := 0 to n - 1 do
      echantillon[j] := donnees[Random(n)];

    // Calculer la statistique sur cet échantillon
    Result[i] := statistique(echantillon);
  end;
end;

// Intervalle de confiance par bootstrap
function ICBootstrap(const donnees: TDonnees;
                    statistique: function(const d: TDonnees): TFloat;
                    niveau: TFloat = 0.95): record
  Estimation: TFloat;
  IC_Inf: TFloat;
  IC_Sup: TFloat;
end;  
var
  bootstrapStats: TDonnees;
  alpha: TFloat;
  indexInf, indexSup: Integer;
begin
  // Générer les statistiques bootstrap
  bootstrapStats := Bootstrap(donnees, 1000, statistique);
  TrierDonnees(bootstrapStats);

  // Estimation ponctuelle
  Result.Estimation := statistique(donnees);

  // Percentiles pour l'intervalle
  alpha := (1 - niveau) / 2;
  indexInf := Round(alpha * Length(bootstrapStats));
  indexSup := Round((1 - alpha) * Length(bootstrapStats));

  Result.IC_Inf := bootstrapStats[indexInf];
  Result.IC_Sup := bootstrapStats[indexSup];
end;
```

## Analyses multivariées

### Analyse en composantes principales (PCA) - Version statistique

```pascal
function PCAStatistiques(const donnees: TMatriceDonnees;
                        nbComposantes: Integer): record
  ValeursPropres: TDonnees;
  VarianceExpliquee: TDonnees;
  Scores: TMatriceDonnees;
end;  
var
  i, j, n, p: Integer;
  moyennes: TDonnees;
  donneesCentrees, covariance: TMatriceDonnees;
  valeursP: TDonnees;
  varianceTotale, varianceCumulee: TFloat;
  somme: TFloat;
begin
  n := Length(donnees);      // Observations
  p := Length(donnees[0]);   // Variables

  // Centrer les données
  SetLength(moyennes, p);
  SetLength(donneesCentrees, n, p);

  for j := 0 to p - 1 do
  begin
    somme := 0;
    for i := 0 to n - 1 do
      somme := somme + donnees[i, j];
    moyennes[j] := somme / n;
  end;

  for i := 0 to n - 1 do
    for j := 0 to p - 1 do
      donneesCentrees[i, j] := donnees[i, j] - moyennes[j];

  // Calculer la matrice de covariance
  covariance := MatriceCovariance(donneesCentrees);

  // Calculer les valeurs propres (utilise fonction d'algèbre linéaire)
  ValeursPropreQR(covariance, valeursP);

  // Trier par ordre décroissant
  TrierValeursPropresDec(valeursP);

  // Calculer la variance expliquée
  varianceTotale := 0;
  for i := 0 to High(valeursP) do
    varianceTotale := varianceTotale + valeursP[i];

  SetLength(Result.ValeursPropres, nbComposantes);
  SetLength(Result.VarianceExpliquee, nbComposantes);

  varianceCumulee := 0;
  for i := 0 to nbComposantes - 1 do
  begin
    Result.ValeursPropres[i] := valeursP[i];
    varianceCumulee := varianceCumulee + valeursP[i];
    Result.VarianceExpliquee[i] := 100 * varianceCumulee / varianceTotale;
  end;

  WriteLn('Variance expliquée par composante :');
  for i := 0 to nbComposantes - 1 do
    WriteLn(Format('PC%d : %.2f%%', [i + 1, Result.VarianceExpliquee[i]]));
end;

function MatriceCovariance(const donnees: TMatriceDonnees): TMatriceDonnees;  
var
  n, p, i, j, k: Integer;
  somme: TFloat;
begin
  n := Length(donnees);
  p := Length(donnees[0]);
  SetLength(Result, p, p);

  for i := 0 to p - 1 do
  begin
    for j := i to p - 1 do
    begin
      somme := 0;
      for k := 0 to n - 1 do
        somme := somme + donnees[k, i] * donnees[k, j];

      Result[i, j] := somme / (n - 1);
      Result[j, i] := Result[i, j];  // Matrice symétrique
    end;
  end;
end;
```

### Analyse discriminante linéaire (LDA)

```pascal
type
  TClasse = Integer;
  TDonneesClassees = record
    Donnees: TMatriceDonnees;
    Classes: array of TClasse;
  end;

function AnalyseDiscriminanteLineaire(const donnees: TDonneesClassees): TMatrix;  
var
  nbClasses, i, j: Integer;
  moyenneGenerale, moyennesClasses: TMatriceDonnees;
  matrixSW, matrixSB: TMatrix;  // Within, Between
begin
  nbClasses := CalculerNombreClasses(donnees.Classes);

  // Calculer la moyenne générale
  moyenneGenerale := CalculerMoyenneGenerale(donnees.Donnees);

  // Calculer les moyennes par classe
  moyennesClasses := CalculerMoyennesParClasse(donnees);

  // Matrice de dispersion intra-classe (Within)
  matrixSW := CalculerDispersionIntraClasse(donnees);

  // Matrice de dispersion inter-classe (Between)
  matrixSB := CalculerDispersionInterClasse(donnees, moyennesClasses,
                                            moyenneGenerale);

  // Résoudre SW^(-1) × SB pour obtenir les axes discriminants
  Result := ResoudreDiscriminant(matrixSW, matrixSB);
end;
```

## Séries temporelles

### Moyennes mobiles

```pascal
function MoyenneMobile(const serie: TDonnees; fenetre: Integer): TDonnees;  
var
  i, j: Integer;
  somme: TFloat;
  debut, fin: Integer;
begin
  SetLength(Result, Length(serie));

  for i := 0 to High(serie) do
  begin
    debut := Max(0, i - fenetre div 2);
    fin := Min(High(serie), i + fenetre div 2);

    somme := 0;
    for j := debut to fin do
      somme := somme + serie[j];

    Result[i] := somme / (fin - debut + 1);
  end;
end;

// Moyenne mobile exponentielle
function MoyenneMobileExponentielle(const serie: TDonnees;
                                   alpha: TFloat): TDonnees;
var
  i: Integer;
begin
  SetLength(Result, Length(serie));
  Result[0] := serie[0];

  for i := 1 to High(serie) do
    Result[i] := alpha * serie[i] + (1 - alpha) * Result[i - 1];
end;
```

### Décomposition saisonnière

```pascal
type
  TDecomposition = record
    Tendance: TDonnees;
    Saisonnalite: TDonnees;
    Residu: TDonnees;
  end;

function DecomposerSerie(const serie: TDonnees;
                        periode: Integer): TDecomposition;
var
  i, j, cycle: Integer;
  moyennesCycles: TDonnees;
  somme: TFloat;
begin
  // Calculer la tendance (moyenne mobile)
  Result.Tendance := MoyenneMobile(serie, periode);

  // Calculer la composante saisonnière
  SetLength(Result.Saisonnalite, Length(serie));
  SetLength(moyennesCycles, periode);

  // Moyenne par position dans le cycle
  for j := 0 to periode - 1 do
  begin
    somme := 0;
    cycle := 0;
    i := j;
    while i < Length(serie) do
    begin
      somme := somme + (serie[i] - Result.Tendance[i]);
      Inc(cycle);
      Inc(i, periode);
    end;
    if cycle > 0 then
      moyennesCycles[j] := somme / cycle;
  end;

  // Appliquer la saisonnalité
  for i := 0 to High(serie) do
    Result.Saisonnalite[i] := moyennesCycles[i mod periode];

  // Calculer les résidus
  SetLength(Result.Residu, Length(serie));
  for i := 0 to High(serie) do
    Result.Residu[i] := serie[i] - Result.Tendance[i] - Result.Saisonnalite[i];
end;
```

### Autocorrélation

```pascal
function Autocorrelation(const serie: TDonnees; lag: Integer): TFloat;  
var
  i, n: Integer;
  moy, variance, covariance: TFloat;
begin
  n := Length(serie);

  if (lag < 0) or (lag >= n) then
    raise Exception.Create('Lag invalide');

  moy := Moyenne(serie);

  // Variance
  variance := 0;
  for i := 0 to n - 1 do
    variance := variance + Sqr(serie[i] - moy);

  // Covariance avec le lag
  covariance := 0;
  for i := 0 to n - lag - 1 do
    covariance := covariance + (serie[i] - moy) * (serie[i + lag] - moy);

  if variance = 0 then
    Result := 0
  else
    Result := covariance / variance;
end;

function Correlogramme(const serie: TDonnees;
                      nbLags: Integer): TDonnees;
var
  i: Integer;
begin
  SetLength(Result, nbLags + 1);
  for i := 0 to nbLags do
    Result[i] := Autocorrelation(serie, i);
end;
```

## Tests de stationnarité

### Test de Dickey-Fuller augmenté (simplifié)

```pascal
function TestDickeyFuller(const serie: TDonnees): TResultatTest;  
var
  differences: TDonnees;
  i: Integer;
  regression: TRegressionSimple;
begin
  // Calculer les différences premières
  SetLength(differences, Length(serie) - 1);
  for i := 0 to High(differences) do
    differences[i] := serie[i + 1] - serie[i];

  // Régresser Δy_t sur y_{t-1}
  // (version simplifiée)
  regression := RegressionLineaireSimple(
    Copy(serie, 0, Length(serie) - 1),
    differences
  );

  Result.Statistique := regression.StatistiqueT;
  Result.ValeurP := regression.ValeurP;

  // Valeur critique de Dickey-Fuller (approximation)
  Result.Seuil := -2.86;  // Pour 5%

  Result.EstSignificatif := Result.Statistique < Result.Seuil;

  if Result.EstSignificatif then
    WriteLn('La série est stationnaire')
  else
    WriteLn('La série est non-stationnaire (présence de racine unitaire)');
end;
```

## Méthodes bayésiennes

### Théorème de Bayes

```pascal
function ProbabiliteAPriori(hypothese: String): TFloat;  
begin
  // À définir selon le contexte
  Result := 0.5;  // Cas uniforme
end;

function Vraisemblance(donnees: TDonnees; hypothese: String): TFloat;  
begin
  // Calculer P(données | hypothèse)
  // Implémentation dépend du modèle
  Result := 1.0;
end;

function ProbabiliteAPosteriori(donnees: TDonnees;
                               hypothese: String): TFloat;
var
  priori, vraisemblance, evidence: TFloat;
begin
  priori := ProbabiliteAPriori(hypothese);
  vraisemblance := Vraisemblance(donnees, hypothese);

  // P(H|D) = P(D|H) × P(H) / P(D)
  // Simplification : sans normalisation
  Result := vraisemblance * priori;
end;
```

### Estimation bayésienne de la moyenne

```pascal
function MoyenneBayesienne(const donnees: TDonnees;
                          moyennePriori, variancePriori: TFloat): record
  MoyennePosteriori: TFloat;
  VariancePosteriori: TFloat;
  IC95_Inf: TFloat;
  IC95_Sup: TFloat;
end;  
var
  n: Integer;
  moyenneObservee, varianceObservee: TFloat;
  precisionPriori, precisionObservee, precisionPosteriori: TFloat;
begin
  n := Length(donnees);
  moyenneObservee := Moyenne(donnees);
  varianceObservee := Variance(donnees, False);

  // Travailler avec les précisions (inverse de la variance)
  precisionPriori := 1 / variancePriori;
  precisionObservee := n / varianceObservee;
  precisionPosteriori := precisionPriori + precisionObservee;

  // Moyenne a posteriori (moyenne pondérée)
  Result.MoyennePosteriori :=
    (precisionPriori * moyennePriori + precisionObservee * moyenneObservee) /
    precisionPosteriori;

  Result.VariancePosteriori := 1 / precisionPosteriori;

  // Intervalle de crédibilité à 95%
  Result.IC95_Inf := Result.MoyennePosteriori -
                     1.96 * Sqrt(Result.VariancePosteriori);
  Result.IC95_Sup := Result.MoyennePosteriori +
                     1.96 * Sqrt(Result.VariancePosteriori);
end;
```

## Visualisation avec TAChart

### Histogramme de distribution

```pascal
uses
  TAGraph, TASeries;

procedure CreerHistogramme(Chart: TChart; const donnees: TDonnees;
                          nbClasses: Integer);
var
  Serie: TBarSeries;
  min, max, largeurClasse: TFloat;
  classes: array of Integer;
  i, classe: Integer;
begin
  // Calculer les bornes
  min := MinValue(donnees);
  max := MaxValue(donnees);
  largeurClasse := (max - min) / nbClasses;

  // Compter les occurrences par classe
  SetLength(classes, nbClasses);
  for i := 0 to nbClasses - 1 do
    classes[i] := 0;

  for i := 0 to High(donnees) do
  begin
    classe := Trunc((donnees[i] - min) / largeurClasse);
    if classe >= nbClasses then classe := nbClasses - 1;
    if classe < 0 then classe := 0;
    Inc(classes[classe]);
  end;

  // Créer le graphique
  Serie := TBarSeries.Create(Chart);
  Serie.Title := 'Distribution';

  for i := 0 to nbClasses - 1 do
    Serie.AddXY(min + (i + 0.5) * largeurClasse, classes[i]);

  Chart.AddSeries(Serie);
  Chart.LeftAxis.Title.Caption := 'Fréquence';
  Chart.BottomAxis.Title.Caption := 'Valeur';
end;
```

### Boîte à moustaches (Box plot)

```pascal
procedure CreerBoiteAMoustaches(Chart: TChart; const groupes: array of TDonnees;
                               noms: array of String);
var
  i: Integer;
  stats: TStatistiquesDescriptives;
  Serie: TBoxAndWiskerSeries;
begin
  for i := 0 to High(groupes) do
  begin
    stats := CalculerStatistiquesDescriptives(groupes[i]);

    // TAChart a un support limité pour les box plots
    // Alternative : dessiner manuellement ou utiliser TLineSeries
    // pour représenter les quartiles
  end;
end;
```

### Graphique de corrélation (scatter plot)

```pascal
procedure GraphiqueCorrelation(Chart: TChart; const x, y: TDonnees);  
var
  Serie: TPointSeries;
  SerieRegression: TLineSeries;
  i: Integer;
  regression: TRegressionSimple;
  xMin, xMax: TFloat;
begin
  // Nuage de points
  Serie := TPointSeries.Create(Chart);
  Serie.Title := 'Données';

  for i := 0 to High(x) do
    Serie.AddXY(x[i], y[i]);

  Chart.AddSeries(Serie);

  // Droite de régression
  regression := RegressionLineaireSimple(x, y);
  SerieRegression := TLineSeries.Create(Chart);
  SerieRegression.Title := Format('y = %.2f + %.2fx (R²=%.3f)',
    [regression.Intercept, regression.Pente, regression.R2]);

  xMin := MinValue(x);
  xMax := MaxValue(x);
  SerieRegression.AddXY(xMin, regression.Intercept + regression.Pente * xMin);
  SerieRegression.AddXY(xMax, regression.Intercept + regression.Pente * xMax);

  Chart.AddSeries(SerieRegression);
  Chart.Legend.Visible := True;
end;
```

## Application pratique complète

Voici un exemple d'application d'analyse statistique complète.

```pascal
program AnalyseStatistiqueComplete;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, Grids,
  TAGraph, TASeries, UnitStatistiques;

type
  TFormAnalyse = class(TForm)
    ButtonCharger: TButton;
    ButtonAnalyser: TButton;
    ButtonGraphique: TButton;
    MemoResultats: TMemo;
    Chart1: TChart;
    ComboBoxTest: TComboBox;
    StringGrid1: TStringGrid;

    procedure FormCreate(Sender: TObject);
    procedure ButtonChargerClick(Sender: TObject);
    procedure ButtonAnalyserClick(Sender: TObject);
    procedure ButtonGraphiqueClick(Sender: TObject);
  private
    FDonnees: TDonnees;
    procedure AfficherStatistiquesDescriptives;
    procedure EffectuerTestHypothese;
    procedure CreerVisualisations;
  public

  end;

var
  FormAnalyse: TFormAnalyse;

implementation

{$R *.lfm}

procedure TFormAnalyse.FormCreate(Sender: TObject);  
begin
  ComboBoxTest.Items.Add('Statistiques descriptives');
  ComboBoxTest.Items.Add('Test t (un échantillon)');
  ComboBoxTest.Items.Add('Test de normalité');
  ComboBoxTest.Items.Add('Intervalle de confiance');
  ComboBoxTest.ItemIndex := 0;

  StringGrid1.ColCount := 2;
  StringGrid1.RowCount := 1;
  StringGrid1.Cells[0, 0] := 'Index';
  StringGrid1.Cells[1, 0] := 'Valeur';
end;

procedure TFormAnalyse.ButtonChargerClick(Sender: TObject);  
var
  i: Integer;
begin
  // Charger des données de démonstration
  SetLength(FDonnees, 30);
  Randomize;

  for i := 0 to High(FDonnees) do
    FDonnees[i] := 100 + Random(20) - 10 + Random * 5;

  // Afficher dans la grille
  StringGrid1.RowCount := Length(FDonnees) + 1;
  for i := 0 to High(FDonnees) do
  begin
    StringGrid1.Cells[0, i + 1] := IntToStr(i + 1);
    StringGrid1.Cells[1, i + 1] := Format('%.2f', [FDonnees[i]]);
  end;

  MemoResultats.Lines.Add('Données chargées : ' + IntToStr(Length(FDonnees)) +
                         ' observations');
end;

procedure TFormAnalyse.ButtonAnalyserClick(Sender: TObject);  
begin
  if Length(FDonnees) = 0 then
  begin
    ShowMessage('Chargez d''abord des données');
    Exit;
  end;

  MemoResultats.Clear;

  case ComboBoxTest.ItemIndex of
    0: AfficherStatistiquesDescriptives;
    1: EffectuerTestHypothese;
    2: begin
         if TestNormalite(FDonnees) then
           MemoResultats.Lines.Add('✓ Distribution normale')
         else
           MemoResultats.Lines.Add('✗ Distribution non normale');
       end;
    3: begin
         var ic := IntervalleConfianceMoyenne(FDonnees);
         MemoResultats.Lines.Add(Format('Moyenne : %.2f', [ic.Moyenne]));
         MemoResultats.Lines.Add(Format('IC 95%% : [%.2f ; %.2f]',
           [ic.IC_Inf, ic.IC_Sup]));
       end;
  end;
end;

procedure TFormAnalyse.AfficherStatistiquesDescriptives;  
var
  stats: TStatistiquesDescriptives;
begin
  stats := CalculerStatistiquesDescriptives(FDonnees);

  MemoResultats.Lines.Add('=== STATISTIQUES DESCRIPTIVES ===');
  MemoResultats.Lines.Add('');
  MemoResultats.Lines.Add(Format('Effectif       : %d', [stats.Effectif]));
  MemoResultats.Lines.Add(Format('Minimum        : %.2f', [stats.Minimum]));
  MemoResultats.Lines.Add(Format('Q1             : %.2f', [stats.Q1]));
  MemoResultats.Lines.Add(Format('Médiane        : %.2f', [stats.Mediane]));
  MemoResultats.Lines.Add(Format('Q3             : %.2f', [stats.Q3]));
  MemoResultats.Lines.Add(Format('Maximum        : %.2f', [stats.Maximum]));
  MemoResultats.Lines.Add(Format('Étendue        : %.2f', [stats.Etendue]));
  MemoResultats.Lines.Add('');
  MemoResultats.Lines.Add(Format('Moyenne        : %.2f', [stats.Moyenne]));
  MemoResultats.Lines.Add(Format('Écart-type     : %.2f', [stats.EcartType]));
  MemoResultats.Lines.Add(Format('Variance       : %.2f', [stats.Variance]));
  MemoResultats.Lines.Add('');
  MemoResultats.Lines.Add(Format('Asymétrie      : %.3f', [stats.Asymetrie]));
  MemoResultats.Lines.Add(Format('Aplatissement  : %.3f', [stats.Aplatissement]));
end;

procedure TFormAnalyse.EffectuerTestHypothese;  
var
  resultat: TResultatTest;
  moyenneTheorique: TFloat;
begin
  moyenneTheorique := 100;  // Hypothèse H0: μ = 100

  resultat := TestTUnEchantillon(FDonnees, moyenneTheorique);

  MemoResultats.Lines.Add('=== TEST T (UN ÉCHANTILLON) ===');
  MemoResultats.Lines.Add('');
  MemoResultats.Lines.Add(Format('H0 : μ = %.0f', [moyenneTheorique]));
  MemoResultats.Lines.Add(Format('Moyenne observée : %.2f',
    [Moyenne(FDonnees)]));
  MemoResultats.Lines.Add('');
  MemoResultats.Lines.Add(Format('Statistique t  : %.3f',
    [resultat.Statistique]));
  MemoResultats.Lines.Add(Format('Degrés liberté : %d',
    [resultat.DegreLiberte]));
  MemoResultats.Lines.Add(Format('Valeur p       : %.4f',
    [resultat.ValeurP]));
  MemoResultats.Lines.Add('');

  if resultat.EstSignificatif then
    MemoResultats.Lines.Add('✓ Résultat SIGNIFICATIF (p < 0.05)')
  else
    MemoResultats.Lines.Add('✗ Résultat NON SIGNIFICATIF (p ≥ 0.05)');
end;

procedure TFormAnalyse.ButtonGraphiqueClick(Sender: TObject);  
begin
  CreerVisualisations;
end;

procedure TFormAnalyse.CreerVisualisations;  
begin
  CreerHistogramme(Chart1, FDonnees, 10);
  Chart1.Title.Text.Text := 'Distribution des données';
end;

end.
```

## Considérations multi-plateformes

### Gestion des calculs intensifs

```pascal
{$IFDEF WINDOWS}
uses
  Windows;  // Pour performances
{$ENDIF}

{$IFDEF UNIX}
uses
  BaseUnix;  // Pour performances Linux
{$ENDIF}

procedure OptimiserCalculs;  
begin
  {$IFDEF WINDOWS}
  // Augmenter la priorité du thread
  SetThreadPriority(GetCurrentThread, THREAD_PRIORITY_ABOVE_NORMAL);
  {$ENDIF}

  {$IFDEF UNIX}
  // Optimisations spécifiques Linux
  {$ENDIF}
end;
```

### Export de résultats

```pascal
procedure ExporterResultatsCSV(nomFichier: String; const stats: TStatistiquesDescriptives);  
var
  fichier: TextFile;
  separateur: Char;
begin
  {$IFDEF WINDOWS}
  separateur := ';';  // Excel Windows
  {$ENDIF}
  {$IFDEF UNIX}
  separateur := ',';  // Standard Unix
  {$ENDIF}

  AssignFile(fichier, nomFichier);
  Rewrite(fichier);
  try
    WriteLn(fichier, 'Statistique', separateur, 'Valeur');
    WriteLn(fichier, 'Effectif', separateur, stats.Effectif);
    WriteLn(fichier, 'Moyenne', separateur, FormatFloat('0.0000', stats.Moyenne));
    WriteLn(fichier, 'Médiane', separateur, FormatFloat('0.0000', stats.Mediane));
    WriteLn(fichier, 'Écart-type', separateur, FormatFloat('0.0000', stats.EcartType));
    // etc.
  finally
    CloseFile(fichier);
  end;
end;
```

## Ressources et bibliothèques

### Bibliothèques statistiques pour FreePascal

1. **Math Unit** : Fonctions mathématiques de base (incluse)
2. **NumLib** : Calcul numérique
3. **LMath** : Bibliothèque mathématique étendue avec stats
4. **TAChart** : Visualisations

### Interopérabilité

```pascal
// Appeler R pour analyses avancées
procedure AppelerR(script: String);  
begin
  ExecuteProcess('Rscript', [script], []);
end;

// Appeler Python avec SciPy/NumPy
procedure AppelerPythonStats(donnees: TDonnees);  
var
  i: Integer;
  fichier: TextFile;
begin
  // Sauvegarder les données
  AssignFile(fichier, 'data.csv');
  Rewrite(fichier);
  for i := 0 to High(donnees) do
    WriteLn(fichier, donnees[i]);
  CloseFile(fichier);

  // Appeler le script Python
  ExecuteProcess('python3', ['analyse_stats.py'], []);
end;
```

## Bonnes pratiques

### Validation des données

```pascal
function ValiderDonnees(const donnees: TDonnees): Boolean;  
var
  i: Integer;
begin
  Result := False;

  if Length(donnees) = 0 then
  begin
    WriteLn('Erreur : Aucune donnée');
    Exit;
  end;

  // Vérifier les valeurs manquantes ou aberrantes
  for i := 0 to High(donnees) do
  begin
    if IsNan(donnees[i]) or IsInfinite(donnees[i]) then
    begin
      WriteLn('Erreur : Valeur invalide à l''index ', i);
      Exit;
    end;
  end;

  Result := True;
end;
```

### Documentation des analyses

```pascal
type
  TRapportStatistique = record
    DateAnalyse: TDateTime;
    NombreObservations: Integer;
    Statistiques: TStatistiquesDescriptives;
    TestsEffectues: array of String;
    Conclusions: TStringList;
  end;

procedure GenererRapport(const rapport: TRapportStatistique; nomFichier: String);  
var
  fichier: TextFile;
  i: Integer;
begin
  AssignFile(fichier, nomFichier);
  Rewrite(fichier);
  try
    WriteLn(fichier, '=== RAPPORT D''ANALYSE STATISTIQUE ===');
    WriteLn(fichier, 'Date : ', DateTimeToStr(rapport.DateAnalyse));
    WriteLn(fichier, 'Observations : ', rapport.NombreObservations);
    WriteLn(fichier);

    // Ajouter les statistiques
    // Ajouter les tests
    // Ajouter les conclusions
  finally
    CloseFile(fichier);
  end;
end;
```

## Conclusion

Les statistiques avancées sont essentielles pour l'analyse de données scientifiques. Avec FreePascal, vous disposez de :

✓ **Outils complets** : Statistiques descriptives, inférentielles, tests d'hypothèses  
✓ **Performance** : Code compilé natif, calculs rapides  
✓ **Visualisations** : Integration avec TAChart  
✓ **Portabilité** : Même code sur Windows et Ubuntu  
✓ **Flexibilité** : Du simple calcul aux analyses multivariées complexes  
✓ **Reproductibilité** : Code source clair et maintenable

### Points clés à retenir

1. **Statistiques descriptives** : Toujours commencer par explorer les données
2. **Tests d'hypothèses** : Choisir le test approprié selon le contexte
3. **Vérifier les conditions** : Normalité, indépendance, homoscédasticité
4. **Interpréter avec prudence** : Significativité statistique ≠ importance pratique
5. **Visualiser** : Un graphique vaut mille statistiques
6. **Documenter** : Traçabilité et reproductibilité des analyses

### Quand utiliser quelle méthode ?

**Comparer deux groupes** :
- Test t si données normales
- Test de Mann-Whitney si non normales
- Test t apparié si données appariées

**Comparer plusieurs groupes** :
- ANOVA si données normales et variances égales
- Kruskal-Wallis si non paramétrique
- ANOVA à mesures répétées si appariées

**Étudier des relations** :
- Corrélation de Pearson si relation linéaire
- Corrélation de Spearman si relation monotone
- Régression pour prédiction

**Analyser des proportions** :
- Test du Chi-carré pour indépendance
- Test exact de Fisher si petits effectifs
- Test z pour une proportion

### Erreurs courantes à éviter

1. **Ne pas vérifier les hypothèses** : Chaque test a des conditions
2. **Comparaisons multiples** : Augmentent le risque d'erreur de type I
3. **Confusion corrélation/causalité** : Corrélation n'implique pas causalité
4. **Ignorer la taille d'effet** : La significativité dépend de la taille d'échantillon
5. **Sur-interpréter les p-values** : p = 0.051 vs p = 0.049 n'est pas si différent
6. **Oublier les valeurs aberrantes** : Peuvent fausser les résultats
7. **Ne pas visualiser** : Les graphiques révèlent des patterns invisibles aux tests

### Exemples de flux d'analyse typiques

#### Analyse exploratoire de données

```pascal
procedure AnalyseExploratoire(const donnees: TDonnees);  
var
  stats: TStatistiquesDescriptives;
begin
  // 1. Statistiques descriptives
  stats := CalculerStatistiquesDescriptives(donnees);
  AfficherStatistiquesDescriptives(stats);

  // 2. Test de normalité
  if TestNormalite(donnees) then
    WriteLn('Distribution normale - tests paramétriques OK')
  else
    WriteLn('Distribution non normale - considérer tests non paramétriques');

  // 3. Détection de valeurs aberrantes
  DetecterValeursAberrantes(donnees);

  // 4. Visualisation
  CreerHistogramme(Chart1, donnees, 15);
  CreerBoiteAMoustaches(Chart2, donnees);
end;

procedure DetecterValeursAberrantes(const donnees: TDonnees);  
var
  stats: TStatistiquesDescriptives;
  i: Integer;
  seuilInf, seuilSup: TFloat;
begin
  stats := CalculerStatistiquesDescriptives(donnees);

  // Méthode IQR : valeurs > Q3 + 1.5×IQR ou < Q1 - 1.5×IQR
  seuilInf := stats.Q1 - 1.5 * stats.EIQ;
  seuilSup := stats.Q3 + 1.5 * stats.EIQ;

  WriteLn('Valeurs aberrantes détectées :');
  for i := 0 to High(donnees) do
  begin
    if (donnees[i] < seuilInf) or (donnees[i] > seuilSup) then
      WriteLn(Format('  Index %d : %.2f', [i, donnees[i]]));
  end;
end;
```

#### Comparaison de groupes

```pascal
procedure ComparerGroupes(const groupe1, groupe2: TDonnees);  
var
  resultat: TResultatTest;
  normal1, normal2: Boolean;
begin
  WriteLn('=== COMPARAISON DE DEUX GROUPES ===');
  WriteLn;

  // Vérifier la normalité
  normal1 := TestNormalite(groupe1);
  normal2 := TestNormalite(groupe2);

  if normal1 and normal2 then
  begin
    WriteLn('Les deux groupes suivent une loi normale');
    WriteLn('→ Utilisation du test t de Student');
    resultat := TestTDeuxEchantillons(groupe1, groupe2);
  end
  else
  begin
    WriteLn('Au moins un groupe n''est pas normal');
    WriteLn('→ Considérer le test de Mann-Whitney (non paramétrique)');
    // resultat := TestMannWhitney(groupe1, groupe2);
  end;

  AfficherResultatTest(resultat, 'Comparaison de groupes');

  // Taille d'effet
  CalculerTailleEffet(groupe1, groupe2);
end;

procedure CalculerTailleEffet(const groupe1, groupe2: TDonnees);  
var
  moy1, moy2, ecart1, ecart2, ecartPooled, cohenD: TFloat;
  n1, n2: Integer;
begin
  n1 := Length(groupe1);
  n2 := Length(groupe2);

  moy1 := Moyenne(groupe1);
  moy2 := Moyenne(groupe2);
  ecart1 := EcartType(groupe1, False);
  ecart2 := EcartType(groupe2, False);

  // Cohen's d
  ecartPooled := Sqrt(((n1 - 1) * Sqr(ecart1) + (n2 - 1) * Sqr(ecart2)) /
                      (n1 + n2 - 2));
  cohenD := (moy1 - moy2) / ecartPooled;

  WriteLn(Format('Taille d''effet (Cohen''s d) : %.3f', [Abs(cohenD)]));

  if Abs(cohenD) < 0.2 then
    WriteLn('  → Effet négligeable')
  else if Abs(cohenD) < 0.5 then
    WriteLn('  → Petit effet')
  else if Abs(cohenD) < 0.8 then
    WriteLn('  → Effet moyen')
  else
    WriteLn('  → Grand effet');
end;
```

#### Analyse de régression complète

```pascal
procedure AnalyseRegressionComplete(const x, y: TDonnees);  
var
  regression: TRegressionSimple;
  residus: TDonnees;
  i: Integer;
  yPred: TFloat;
begin
  WriteLn('=== ANALYSE DE RÉGRESSION ===');
  WriteLn;

  // 1. Calculer la régression
  regression := RegressionLineaireSimple(x, y);
  AfficherRegressionSimple(regression);

  // 2. Calculer les résidus
  SetLength(residus, Length(y));
  for i := 0 to High(y) do
  begin
    yPred := regression.Intercept + regression.Pente * x[i];
    residus[i] := y[i] - yPred;
  end;

  // 3. Analyser les résidus
  WriteLn('Analyse des résidus :');
  if TestNormalite(residus) then
    WriteLn('  ✓ Résidus normalement distribués')
  else
    WriteLn('  ✗ Résidus non normaux - modèle possiblement inadéquat');

  // 4. Visualiser
  GraphiqueCorrelation(Chart1, x, y);
  CreerHistogramme(Chart2, residus, 10);
  Chart2.Title.Text.Text := 'Distribution des résidus';
end;
```

### Calcul de puissance statistique

La puissance est la probabilité de détecter un effet s'il existe vraiment.

```pascal
function PuissanceTestT(tailleEffet, alpha: TFloat; n: Integer): TFloat;  
var
  nc: TFloat;  // Non-centrality parameter
  tc: TFloat;  // Critical t value
begin
  // Paramètre de non-centralité
  nc := tailleEffet * Sqrt(n);

  // Valeur critique
  tc := ValeurCritiqueStudent(alpha, n - 1);

  // Approximation de la puissance (simplifiée)
  // Dans la pratique, utiliser des tables ou bibliothèques spécialisées
  if nc > tc + 2 then
    Result := 0.95
  else if nc > tc + 1 then
    Result := 0.80
  else if nc > tc then
    Result := 0.60
  else
    Result := 0.40;
end;

procedure AnalysePuissance;  
var
  taillesEffet: array[0..2] of TFloat = (0.2, 0.5, 0.8);
  tailles: array[0..4] of Integer = (10, 20, 50, 100, 200);
  i, j: Integer;
  puissance: TFloat;
begin
  WriteLn('=== ANALYSE DE PUISSANCE (α = 0.05) ===');
  WriteLn;
  WriteLn('n     d=0.2  d=0.5  d=0.8');
  WriteLn('------------------------------');

  for i := 0 to High(tailles) do
  begin
    Write(Format('%3d   ', [tailles[i]]));
    for j := 0 to High(taillesEffet) do
    begin
      puissance := PuissanceTestT(taillesEffet[j], 0.05, tailles[i]);
      Write(Format('%.2f   ', [puissance]));
    end;
    WriteLn;
  end;
end;
```

### Meta-analyse

Combiner les résultats de plusieurs études.

```pascal
type
  TEtude = record
    Nom: String;
    TailleEffet: TFloat;
    ErreurStandard: TFloat;
    N: Integer;
  end;

function MetaAnalyseEffetsFixes(const etudes: array of TEtude): record
  EffetGlobal: TFloat;
  IC95_Inf: TFloat;
  IC95_Sup: TFloat;
  StatistiqueZ: TFloat;
  ValeurP: TFloat;
end;  
var
  i: Integer;
  poids, sommePoidsEffet, sommePoids: TFloat;
  variance: TFloat;
begin
  sommePoidsEffet := 0;
  sommePoids := 0;

  // Calculer l'effet global (moyenne pondérée)
  for i := 0 to High(etudes) do
  begin
    poids := 1 / Sqr(etudes[i].ErreurStandard);
    sommePoidsEffet := sommePoidsEffet + poids * etudes[i].TailleEffet;
    sommePoids := sommePoids + poids;
  end;

  Result.EffetGlobal := sommePoidsEffet / sommePoids;

  // Variance de l'effet global
  variance := 1 / sommePoids;

  // Intervalle de confiance
  Result.IC95_Inf := Result.EffetGlobal - 1.96 * Sqrt(variance);
  Result.IC95_Sup := Result.EffetGlobal + 1.96 * Sqrt(variance);

  // Test de significativité
  Result.StatistiqueZ := Result.EffetGlobal / Sqrt(variance);

  if Abs(Result.StatistiqueZ) > 2.58 then
    Result.ValeurP := 0.01
  else if Abs(Result.StatistiqueZ) > 1.96 then
    Result.ValeurP := 0.05
  else
    Result.ValeurP := 0.10;
end;
```

### Gestion des données manquantes

```pascal
type
  TMethodeImputation = (miSupprimer, miMoyenne, miMediane, miRegression);

function TraiterDonneesManquantes(const donnees: TDonnees;
                                 methode: TMethodeImputation): TDonnees;
var
  i, j: Integer;
  valeur: TFloat;
begin
  case methode of
    miSupprimer:
    begin
      // Suppression listwise (par défaut géré par IsNan)
      j := 0;
      SetLength(Result, Length(donnees));
      for i := 0 to High(donnees) do
      begin
        if not IsNan(donnees[i]) then
        begin
          Result[j] := donnees[i];
          Inc(j);
        end;
      end;
      SetLength(Result, j);
    end;

    miMoyenne:
    begin
      valeur := MoyenneSansNaN(donnees);
      Result := Copy(donnees);
      for i := 0 to High(Result) do
        if IsNan(Result[i]) then
          Result[i] := valeur;
    end;

    miMediane:
    begin
      valeur := MedianeSansNaN(donnees);
      Result := Copy(donnees);
      for i := 0 to High(Result) do
        if IsNan(Result[i]) then
          Result[i] := valeur;
    end;

    miRegression:
    begin
      // Imputation par régression (plus complexe)
      Result := ImputationParRegression(donnees);
    end;
  end;
end;

function MoyenneSansNaN(const donnees: TDonnees): TFloat;  
var
  i, n: Integer;
  somme: TFloat;
begin
  somme := 0;
  n := 0;

  for i := 0 to High(donnees) do
  begin
    if not IsNan(donnees[i]) then
    begin
      somme := somme + donnees[i];
      Inc(n);
    end;
  end;

  if n = 0 then
    raise Exception.Create('Toutes les valeurs sont manquantes');

  Result := somme / n;
end;
```

### Ressources pour aller plus loin

**Livres de référence** :
- "Statistics" - David Freedman, Robert Pisani, Roger Purves
- "The Elements of Statistical Learning" - Hastie, Tibshirani, Friedman
- "Applied Statistics" - Douglas C. Montgomery
- "Probability and Statistics" - Morris H. DeGroot

**Cours en ligne** :
- Khan Academy : Statistics and Probability
- Coursera : Statistics with R, Data Science Specialization
- MIT OpenCourseWare : Probability and Statistics
- StatQuest (YouTube) : Explications visuelles excellentes

**Outils complémentaires** :
- **R** : Référence pour analyses statistiques
- **Python (SciPy, StatsModels)** : Alternative moderne
- **JASP** : Interface graphique gratuite pour stats
- **jamovi** : Statistiques avec interface intuitive

**Documentation FreePascal** :
- Wiki FreePascal : Section Statistics
- Math Unit Documentation
- Forum Lazarus : Scientific Computing

### Liens avec d'autres chapitres

- **16.1 NumLib** : Calculs numériques de base pour les statistiques
- **16.2 TAChart** : Visualisation des résultats statistiques
- **16.4 Algèbre linéaire** : Régression multiple, PCA, analyse multivariée
- **15. Intelligence artificielle** : Machine learning utilise les statistiques
- **20. Optimisation** : Estimation de paramètres, maximum de vraisemblance

### Conclusion finale

Les statistiques sont un outil puissant pour :
- **Comprendre** : Décrire et résumer des données complexes
- **Décider** : Tester des hypothèses de manière rigoureuse
- **Prédire** : Modéliser des relations et faire des prévisions
- **Communiquer** : Présenter des résultats de façon claire et convaincante

Avec FreePascal/Lazarus, vous avez tous les outils nécessaires pour effectuer des analyses statistiques professionnelles, du simple calcul de moyenne aux modèles multivariés complexes. La syntaxe claire du Pascal rend votre code facile à comprendre et à maintenir, essentiel pour la reproductibilité scientifique.

**N'oubliez pas** : Les statistiques sont un moyen, pas une fin. L'objectif est de répondre à des questions concrètes et de prendre de meilleures décisions basées sur les données.

**Bonne analyse statistique avec FreePascal !** 📊📈🔍

⏭️ [FFT et analyse spectrale](/16-traitement-donnees-calcul-scientifique/06-fft-analyse-spectrale.md)
