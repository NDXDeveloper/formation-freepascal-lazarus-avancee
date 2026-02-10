🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 16.1 NumLib - Calcul numérique

## Introduction

NumLib est une bibliothèque de calcul numérique pour FreePascal qui fournit un ensemble complet de fonctions mathématiques avancées. Elle est particulièrement utile pour les applications scientifiques, l'analyse de données et les calculs techniques.

## Qu'est-ce que NumLib ?

NumLib (Numerical Library) est une collection de routines numériques qui étendent les capacités mathématiques de base de FreePascal. Elle propose des fonctions pour :

- Les calculs matriciels
- L'algèbre linéaire
- Les équations différentielles
- L'interpolation et l'approximation
- Les statistiques
- Les transformations mathématiques

## Installation de NumLib

### Sur Windows

1. **Via le gestionnaire de packages Lazarus** :
   - Ouvrez Lazarus
   - Menu `Package` → `Online Package Manager`
   - Recherchez "NumLib"
   - Cliquez sur `Install`

2. **Installation manuelle** :
   - Téléchargez NumLib depuis le site officiel ou GitHub
   - Extrayez l'archive dans un dossier (ex: `C:\FreePascal\NumLib`)
   - Dans Lazarus : `Package` → `Open Package File (.lpk)`
   - Ouvrez le fichier `numlib.lpk`
   - Cliquez sur `Compile` puis `Use` → `Install`

### Sur Ubuntu/Linux

1. **Via le gestionnaire de packages** :
```bash
sudo apt-get install fp-units-math
```

2. **Installation manuelle** :
```bash
# Télécharger NumLib
git clone https://github.com/numlib/numlib.git  
cd numlib

# Compiler et installer
make  
sudo make install
```

3. **Dans Lazarus** :
   - Même procédure que sur Windows avec le fichier .lpk

## Structure de NumLib

NumLib est organisée en plusieurs unités thématiques :

### Unités principales

| Unité | Description |
|-------|-------------|
| `uTypes` | Types de données de base pour les calculs numériques |
| `uMatrix` | Opérations sur les matrices |
| `uVectors` | Opérations sur les vecteurs |
| `uLinEq` | Résolution d'équations linéaires |
| `uRoots` | Recherche de racines de fonctions |
| `uIntegral` | Intégration numérique |
| `uInterpolation` | Interpolation de données |
| `uStatistics` | Fonctions statistiques |
| `uSpecFun` | Fonctions spéciales (Gamma, Bessel, etc.) |

## Premiers pas avec NumLib

### Configuration du projet

Pour utiliser NumLib dans votre projet, ajoutez les unités nécessaires dans la clause `uses` :

```pascal
program CalculNumerique;

{$mode objfpc}{$H+}

uses
  uTypes,      // Types de base
  uMatrix,     // Matrices
  uVectors,    // Vecteurs
  uLinEq;      // Équations linéaires

begin
  // Votre code ici
end.
```

### Types de données NumLib

NumLib définit ses propres types pour garantir la précision et la portabilité :

```pascal
type
  // Types de base
  TFloat = Double;           // Nombre à virgule flottante
  TVector = array of TFloat; // Vecteur dynamique
  TMatrix = array of array of TFloat; // Matrice dynamique

  // Types pour les indices
  TIntVector = array of Integer;
  TIntMatrix = array of array of Integer;
```

## Opérations de base

### Travail avec les vecteurs

```pascal
uses
  uTypes, uVectors;

var
  v1, v2, resultat: TVector;
  i: Integer;

begin
  // Création d'un vecteur
  SetLength(v1, 5);
  SetLength(v2, 5);

  // Initialisation
  for i := 0 to 4 do
  begin
    v1[i] := i + 1.0;      // [1, 2, 3, 4, 5]
    v2[i] := (i + 1) * 2.0; // [2, 4, 6, 8, 10]
  end;

  // Addition de vecteurs
  resultat := VectorAdd(v1, v2);

  // Produit scalaire
  WriteLn('Produit scalaire : ', DotProduct(v1, v2):0:2);

  // Norme du vecteur
  WriteLn('Norme de v1 : ', VectorNorm(v1):0:2);
end.
```

### Travail avec les matrices

```pascal
uses
  uTypes, uMatrix;

var
  A, B, C: TMatrix;
  i, j: Integer;

begin
  // Création d'une matrice 3x3
  SetLength(A, 3, 3);
  SetLength(B, 3, 3);

  // Initialisation de A
  for i := 0 to 2 do
    for j := 0 to 2 do
      A[i, j] := i * 3 + j + 1;

  // Matrice identité
  B := IdentityMatrix(3);

  // Multiplication de matrices
  C := MatrixMultiply(A, B);

  // Transposée
  C := Transpose(A);

  // Déterminant
  WriteLn('Déterminant de A : ', Determinant(A):0:4);
end.
```

## Résolution d'équations linéaires

NumLib offre plusieurs méthodes pour résoudre des systèmes d'équations linéaires de la forme **Ax = b**.

### Exemple simple : Système 2x2

```pascal
uses
  uTypes, uMatrix, uLinEq;

var
  A: TMatrix;
  b, x: TVector;

begin
  // Système : 2x + 3y = 8
  //           4x - y  = 2

  SetLength(A, 2, 2);
  SetLength(b, 2);

  // Matrice des coefficients
  A[0, 0] := 2;  A[0, 1] := 3;
  A[1, 0] := 4;  A[1, 1] := -1;

  // Vecteur second membre
  b[0] := 8;
  b[1] := 2;

  // Résolution
  x := SolveLinearSystem(A, b);

  WriteLn('Solution :');
  WriteLn('x = ', x[0]:0:4);
  WriteLn('y = ', x[1]:0:4);
end.
```

### Méthodes disponibles

NumLib propose plusieurs algorithmes :

1. **Élimination de Gauss** : Méthode directe, stable
2. **Décomposition LU** : Efficace pour plusieurs seconds membres
3. **Méthode de Cholesky** : Pour matrices symétriques définies positives
4. **Méthodes itératives** : Jacobi, Gauss-Seidel (pour grandes matrices)

```pascal
// Choix de la méthode
x := GaussElimination(A, b);        // Élimination de Gauss  
x := LUDecomposition(A, b);         // Décomposition LU  
x := CholeskyMethod(A, b);          // Cholesky (si applicable)  
x := JacobiMethod(A, b, tolerance); // Méthode itérative
```

## Interpolation de données

L'interpolation permet d'estimer des valeurs entre des points de données connus.

```pascal
uses
  uTypes, uInterpolation;

var
  xData, yData: TVector;
  xInterp, yInterp: TFloat;

begin
  // Points de données
  SetLength(xData, 5);
  SetLength(yData, 5);

  xData[0] := 0;   yData[0] := 0;
  xData[1] := 1;   yData[1] := 1;
  xData[2] := 2;   yData[2] := 4;
  xData[3] := 3;   yData[3] := 9;
  xData[4] := 4;   yData[4] := 16;

  // Interpolation linéaire
  xInterp := 2.5;
  yInterp := LinearInterpolation(xData, yData, xInterp);
  WriteLn('Interpolation linéaire en x=2.5 : ', yInterp:0:2);

  // Interpolation par splines cubiques
  yInterp := CubicSplineInterpolation(xData, yData, xInterp);
  WriteLn('Interpolation spline en x=2.5 : ', yInterp:0:2);
end.
```

## Intégration numérique

Calcul d'intégrales définies par des méthodes numériques.

```pascal
uses
  uTypes, uIntegral;

// Définition de la fonction à intégrer
function MonFonction(x: TFloat): TFloat;  
begin
  Result := x * x; // f(x) = x²
end;

var
  a, b, resultat: TFloat;

begin
  a := 0;   // Borne inférieure
  b := 2;   // Borne supérieure

  // Méthode des trapèzes
  resultat := TrapezoidalRule(@MonFonction, a, b, 100);
  WriteLn('Méthode des trapèzes : ', resultat:0:6);

  // Méthode de Simpson
  resultat := SimpsonRule(@MonFonction, a, b, 100);
  WriteLn('Méthode de Simpson : ', resultat:0:6);

  // Intégrale exacte de x² de 0 à 2 = 8/3 = 2.666667
end.
```

## Recherche de racines

Trouver les zéros d'une fonction (les valeurs de x pour lesquelles f(x) = 0).

```pascal
uses
  uTypes, uRoots;

// Fonction dont on cherche les racines
function Equation(x: TFloat): TFloat;  
begin
  Result := x * x - 4; // x² - 4 = 0, racines : x = ±2
end;

var
  racine: TFloat;
  trouve: Boolean;

begin
  // Méthode de la bissection
  racine := Bisection(@Equation, 0, 5, 1e-6, trouve);
  if trouve then
    WriteLn('Racine trouvée (bissection) : ', racine:0:6);

  // Méthode de Newton-Raphson (nécessite la dérivée)
  racine := NewtonRaphson(@Equation, 1.0, 1e-6, trouve);
  if trouve then
    WriteLn('Racine trouvée (Newton) : ', racine:0:6);
end.
```

## Fonctions statistiques

NumLib inclut des fonctions statistiques de base.

```pascal
uses
  uTypes, uStatistics;

var
  donnees: TVector;
  moyenne, ecartType, variance: TFloat;

begin
  SetLength(donnees, 10);

  // Génération de données
  donnees[0] := 12.5;
  donnees[1] := 15.3;
  donnees[2] := 14.8;
  donnees[3] := 13.2;
  donnees[4] := 16.1;
  donnees[5] := 14.0;
  donnees[6] := 15.7;
  donnees[7] := 13.8;
  donnees[8] := 14.5;
  donnees[9] := 15.0;

  // Calculs statistiques
  moyenne := Mean(donnees);
  variance := Variance(donnees);
  ecartType := StdDev(donnees);

  WriteLn('Moyenne : ', moyenne:0:2);
  WriteLn('Variance : ', variance:0:2);
  WriteLn('Écart-type : ', ecartType:0:2);

  // Médiane
  WriteLn('Médiane : ', Median(donnees):0:2);
end.
```

## Bonnes pratiques

### Gestion de la mémoire

```pascal
var
  matrice: TMatrix;
  vecteur: TVector;

begin
  // Allocation
  SetLength(matrice, 100, 100);
  SetLength(vecteur, 1000);

  try
    // Utilisation
    // ...
  finally
    // Libération (automatique avec les tableaux dynamiques)
    SetLength(matrice, 0, 0);
    SetLength(vecteur, 0);
  end;
end.
```

### Gestion des erreurs

```pascal
uses
  uTypes, uMatrix, SysUtils;

var
  A: TMatrix;
  det: TFloat;

begin
  try
    SetLength(A, 3, 3);
    // Initialisation de A

    det := Determinant(A);

    if Abs(det) < 1e-10 then
      raise Exception.Create('Matrice singulière');

  except
    on E: Exception do
      WriteLn('Erreur : ', E.Message);
  end;
end.
```

### Précision des calculs

```pascal
const
  EPSILON = 1e-10; // Seuil de tolérance

var
  x, y: TFloat;

begin
  x := 1.0 / 3.0;
  y := x * 3.0;

  // Comparaison avec tolérance
  if Abs(y - 1.0) < EPSILON then
    WriteLn('Égaux (à EPSILON près)')
  else
    WriteLn('Différents');
end.
```

## Considérations multi-plateformes

### Compatibilité Windows/Ubuntu

NumLib fonctionne de manière identique sur Windows et Ubuntu. Cependant, quelques points à noter :

**Chemins de fichiers** :
```pascal
{$IFDEF WINDOWS}
  dataPath := 'C:\Data\numlib\';
{$ENDIF}
{$IFDEF UNIX}
  dataPath := '/home/user/data/numlib/';
{$ENDIF}
```

**Performance** :
- Les performances peuvent légèrement varier selon le compilateur et l'OS
- Utilisez les mêmes options de compilation pour des comparaisons équitables
- Activez les optimisations : `-O3` dans les options du projet

**Bibliothèques mathématiques** :
- Windows : utilise généralement les bibliothèques MSVC
- Ubuntu : utilise libm (bibliothèque mathématique GNU)
- Les résultats sont identiques dans la limite de la précision machine

## Ressources supplémentaires

### Documentation officielle

- Site web de NumLib : Consultez la documentation complète
- Wiki FreePascal : Section sur les bibliothèques mathématiques
- Forums Lazarus : Catégorie "Scientific Computing"

### Exemples de projets

NumLib est utilisée dans de nombreux domaines :
- Analyse de données scientifiques
- Simulations physiques
- Traitement du signal
- Finance quantitative
- Ingénierie

### Alternatives et compléments

- **Math Unit** : Bibliothèque mathématique de base de FreePascal
- **LMath** : Autre bibliothèque de calcul numérique
- **TAChart** : Pour la visualisation des résultats
- **Scientific Library** : Collection étendue pour calculs scientifiques

## Conclusion

NumLib est un outil puissant pour le calcul numérique en FreePascal. Elle offre :

✓ Une interface claire et intuitive  
✓ Des algorithmes éprouvés et fiables  
✓ Une compatibilité multi-plateforme  
✓ Des performances satisfaisantes pour la plupart des applications

Pour approfondir vos connaissances, n'hésitez pas à explorer les autres chapitres de cette formation, notamment :
- 16.2 TAChart pour visualisations
- 16.3 Traitement du signal (DSP)
- 16.4 Algèbre linéaire et matrices
- 20. Optimisation et Performance

⏭️ [TAChart pour visualisations](/16-traitement-donnees-calcul-scientifique/02-tachart-visualisations.md)
