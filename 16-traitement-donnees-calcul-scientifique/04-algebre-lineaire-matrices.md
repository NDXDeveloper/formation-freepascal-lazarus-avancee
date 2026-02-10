🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 16.4 Algèbre linéaire et matrices

## Introduction

L'algèbre linéaire est au cœur de nombreuses applications scientifiques et techniques : résolution de systèmes d'équations, graphisme 3D, apprentissage automatique, traitement d'images, simulations physiques, et bien plus encore. Dans ce chapitre, nous allons explorer comment manipuler efficacement les matrices et vecteurs avec FreePascal.

## Qu'est-ce que l'algèbre linéaire ?

L'algèbre linéaire étudie les vecteurs, les matrices et les transformations linéaires. C'est le langage mathématique qui permet de :

- **Résoudre des systèmes d'équations** : Trouver les inconnues dans plusieurs équations simultanées
- **Transformer des coordonnées** : Rotation, mise à l'échelle, translation en 2D/3D
- **Analyser des données** : Réduction de dimensionnalité, classification
- **Modéliser des phénomènes** : Physique, économie, biologie

### Concepts de base

**Vecteur** : Une liste ordonnée de nombres

```
v = [2, 5, 3]  // Vecteur à 3 dimensions
```

**Matrice** : Un tableau rectangulaire de nombres

```
A = [ 1  2  3 ]
    [ 4  5  6 ]
    [ 7  8  9 ]
```

## Types de données en FreePascal

### Définition des types de base

```pascal
unit UnitAlgebreLineaire;

{$mode objfpc}{$H+}

interface

uses
  Math, SysUtils;

type
  // Types de base
  TFloat = Double;  // Utiliser Double pour la précision

  // Vecteurs
  TVector = array of TFloat;
  TVector2D = array[0..1] of TFloat;
  TVector3D = array[0..2] of TFloat;
  TVector4D = array[0..3] of TFloat;

  // Matrices
  TMatrix = array of array of TFloat;
  TMatrix2x2 = array[0..1, 0..1] of TFloat;
  TMatrix3x3 = array[0..2, 0..2] of TFloat;
  TMatrix4x4 = array[0..3, 0..3] of TFloat;

  // Informations sur une matrice
  TMatrixInfo = record
    Lignes: Integer;
    Colonnes: Integer;
    Determinant: TFloat;
    EstCarree: Boolean;
    EstInversible: Boolean;
  end;

implementation

end.
```

### Création et initialisation

```pascal
// Vecteurs dynamiques
var
  v: TVector;
begin
  SetLength(v, 5);  // Vecteur de dimension 5
  v[0] := 1.0;
  v[1] := 2.0;
  v[2] := 3.0;
  v[3] := 4.0;
  v[4] := 5.0;
end;

// Vecteurs statiques (pour performance)
var
  v3: TVector3D;
begin
  v3[0] := 1.0;
  v3[1] := 2.0;
  v3[2] := 3.0;
end;

// Matrices dynamiques
var
  m: TMatrix;
  i, j: Integer;
begin
  SetLength(m, 3, 4);  // Matrice 3 lignes × 4 colonnes

  for i := 0 to 2 do
    for j := 0 to 3 do
      m[i, j] := i * 4 + j + 1;
end;

// Matrices statiques
var
  m3: TMatrix3x3;
begin
  m3[0, 0] := 1; m3[0, 1] := 2; m3[0, 2] := 3;
  m3[1, 0] := 4; m3[1, 1] := 5; m3[1, 2] := 6;
  m3[2, 0] := 7; m3[2, 1] := 8; m3[2, 2] := 9;
end;
```

## Opérations sur les vecteurs

### Addition et soustraction

```pascal
function AdditionVecteurs(const a, b: TVector): TVector;  
var
  i: Integer;
begin
  if Length(a) <> Length(b) then
    raise Exception.Create('Les vecteurs doivent avoir la même dimension');

  SetLength(Result, Length(a));
  for i := 0 to High(a) do
    Result[i] := a[i] + b[i];
end;

function SoustractionVecteurs(const a, b: TVector): TVector;  
var
  i: Integer;
begin
  if Length(a) <> Length(b) then
    raise Exception.Create('Les vecteurs doivent avoir la même dimension');

  SetLength(Result, Length(a));
  for i := 0 to High(a) do
    Result[i] := a[i] - b[i];
end;

// Utilisation
var
  v1, v2, resultat: TVector;
begin
  SetLength(v1, 3);
  SetLength(v2, 3);

  v1[0] := 1; v1[1] := 2; v1[2] := 3;
  v2[0] := 4; v2[1] := 5; v2[2] := 6;

  resultat := AdditionVecteurs(v1, v2);  // [5, 7, 9]
end;
```

### Multiplication par un scalaire

```pascal
function MultiplicationScalaire(const v: TVector; scalaire: TFloat): TVector;  
var
  i: Integer;
begin
  SetLength(Result, Length(v));
  for i := 0 to High(v) do
    Result[i] := v[i] * scalaire;
end;

// Utilisation
var
  v, resultat: TVector;
begin
  SetLength(v, 3);
  v[0] := 1; v[1] := 2; v[2] := 3;

  resultat := MultiplicationScalaire(v, 2.5);  // [2.5, 5.0, 7.5]
end;
```

### Produit scalaire (dot product)

Le produit scalaire mesure à quel point deux vecteurs pointent dans la même direction.

```pascal
function ProduitScalaire(const a, b: TVector): TFloat;  
var
  i: Integer;
begin
  if Length(a) <> Length(b) then
    raise Exception.Create('Les vecteurs doivent avoir la même dimension');

  Result := 0;
  for i := 0 to High(a) do
    Result := Result + a[i] * b[i];
end;

// Utilisation
var
  v1, v2: TVector;
  dot: TFloat;
begin
  SetLength(v1, 3);
  SetLength(v2, 3);

  v1[0] := 1; v1[1] := 2; v1[2] := 3;
  v2[0] := 4; v2[1] := 5; v2[2] := 6;

  dot := ProduitScalaire(v1, v2);  // 1*4 + 2*5 + 3*6 = 32
end;
```

### Norme (longueur) d'un vecteur

```pascal
function Norme(const v: TVector): TFloat;  
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to High(v) do
    Result := Result + Sqr(v[i]);
  Result := Sqrt(Result);
end;

// Norme L1 (somme des valeurs absolues)
function NormeL1(const v: TVector): TFloat;  
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to High(v) do
    Result := Result + Abs(v[i]);
end;

// Norme infinie (valeur maximale)
function NormeInfinie(const v: TVector): TFloat;  
var
  i: Integer;
begin
  Result := Abs(v[0]);
  for i := 1 to High(v) do
    if Abs(v[i]) > Result then
      Result := Abs(v[i]);
end;
```

### Normalisation d'un vecteur

Créer un vecteur de longueur 1 dans la même direction.

```pascal
function Normaliser(const v: TVector): TVector;  
var
  norme: TFloat;
  i: Integer;
begin
  norme := Norme(v);
  if norme = 0 then
    raise Exception.Create('Impossible de normaliser un vecteur nul');

  SetLength(Result, Length(v));
  for i := 0 to High(v) do
    Result[i] := v[i] / norme;
end;
```

### Produit vectoriel (cross product) - 3D uniquement

```pascal
function ProduitVectoriel(const a, b: TVector3D): TVector3D;  
begin
  Result[0] := a[1] * b[2] - a[2] * b[1];
  Result[1] := a[2] * b[0] - a[0] * b[2];
  Result[2] := a[0] * b[1] - a[1] * b[0];
end;

// Utilisation
var
  v1, v2, resultat: TVector3D;
begin
  v1[0] := 1; v1[1] := 0; v1[2] := 0;  // Axe X
  v2[0] := 0; v2[1] := 1; v2[2] := 0;  // Axe Y

  resultat := ProduitVectoriel(v1, v2);  // [0, 0, 1] = Axe Z
end;
```

### Angle entre deux vecteurs

```pascal
function AngleEntreVecteurs(const a, b: TVector): TFloat;  
var
  dot, normeA, normeB: TFloat;
begin
  dot := ProduitScalaire(a, b);
  normeA := Norme(a);
  normeB := Norme(b);

  if (normeA = 0) or (normeB = 0) then
    raise Exception.Create('Un vecteur est nul');

  // cos(theta) = (a·b) / (|a| * |b|)
  Result := ArcCos(dot / (normeA * normeB));
end;

// Convertir en degrés
function AngleEnDegres(const a, b: TVector): TFloat;  
begin
  Result := RadToDeg(AngleEntreVecteurs(a, b));
end;
```

## Opérations sur les matrices

### Création de matrices spéciales

```pascal
// Matrice nulle
function MatriceNulle(lignes, colonnes: Integer): TMatrix;  
var
  i, j: Integer;
begin
  SetLength(Result, lignes, colonnes);
  for i := 0 to lignes - 1 do
    for j := 0 to colonnes - 1 do
      Result[i, j] := 0;
end;

// Matrice identité
function MatriceIdentite(taille: Integer): TMatrix;  
var
  i, j: Integer;
begin
  SetLength(Result, taille, taille);
  for i := 0 to taille - 1 do
    for j := 0 to taille - 1 do
      if i = j then
        Result[i, j] := 1
      else
        Result[i, j] := 0;
end;

// Matrice diagonale
function MatriceDiagonale(const diagonale: TVector): TMatrix;  
var
  i, j, n: Integer;
begin
  n := Length(diagonale);
  SetLength(Result, n, n);

  for i := 0 to n - 1 do
    for j := 0 to n - 1 do
      if i = j then
        Result[i, j] := diagonale[i]
      else
        Result[i, j] := 0;
end;

// Matrice aléatoire
function MatriceAleatoire(lignes, colonnes: Integer;
                         min, max: TFloat): TMatrix;
var
  i, j: Integer;
begin
  SetLength(Result, lignes, colonnes);
  Randomize;

  for i := 0 to lignes - 1 do
    for j := 0 to colonnes - 1 do
      Result[i, j] := min + Random * (max - min);
end;
```

### Addition et soustraction de matrices

```pascal
function AdditionMatrices(const a, b: TMatrix): TMatrix;  
var
  i, j: Integer;
begin
  if (Length(a) <> Length(b)) or
     (Length(a[0]) <> Length(b[0])) then
    raise Exception.Create('Les matrices doivent avoir les mêmes dimensions');

  SetLength(Result, Length(a), Length(a[0]));

  for i := 0 to High(a) do
    for j := 0 to High(a[0]) do
      Result[i, j] := a[i, j] + b[i, j];
end;

function SoustractionMatrices(const a, b: TMatrix): TMatrix;  
var
  i, j: Integer;
begin
  if (Length(a) <> Length(b)) or
     (Length(a[0]) <> Length(b[0])) then
    raise Exception.Create('Les matrices doivent avoir les mêmes dimensions');

  SetLength(Result, Length(a), Length(a[0]));

  for i := 0 to High(a) do
    for j := 0 to High(a[0]) do
      Result[i, j] := a[i, j] - b[i, j];
end;
```

### Multiplication de matrices

```pascal
function MultiplicationMatrices(const a, b: TMatrix): TMatrix;  
var
  i, j, k: Integer;
  somme: TFloat;
  lignesA, colonnesA, lignesB, colonnesB: Integer;
begin
  lignesA := Length(a);
  colonnesA := Length(a[0]);
  lignesB := Length(b);
  colonnesB := Length(b[0]);

  if colonnesA <> lignesB then
    raise Exception.Create('Dimensions incompatibles pour la multiplication');

  SetLength(Result, lignesA, colonnesB);

  for i := 0 to lignesA - 1 do
    for j := 0 to colonnesB - 1 do
    begin
      somme := 0;
      for k := 0 to colonnesA - 1 do
        somme := somme + a[i, k] * b[k, j];
      Result[i, j] := somme;
    end;
end;

// Multiplication matrice × vecteur
function MultiplicationMatriceVecteur(const m: TMatrix;
                                     const v: TVector): TVector;
var
  i, j: Integer;
  somme: TFloat;
begin
  if Length(m[0]) <> Length(v) then
    raise Exception.Create('Dimensions incompatibles');

  SetLength(Result, Length(m));

  for i := 0 to High(m) do
  begin
    somme := 0;
    for j := 0 to High(m[0]) do
      somme := somme + m[i, j] * v[j];
    Result[i] := somme;
  end;
end;
```

### Transposition

```pascal
function Transposee(const m: TMatrix): TMatrix;  
var
  i, j: Integer;
begin
  SetLength(Result, Length(m[0]), Length(m));

  for i := 0 to High(m) do
    for j := 0 to High(m[0]) do
      Result[j, i] := m[i, j];
end;

// Exemple
var
  m, mT: TMatrix;
begin
  SetLength(m, 2, 3);
  // m = [ 1  2  3 ]
  //     [ 4  5  6 ]
  m[0, 0] := 1; m[0, 1] := 2; m[0, 2] := 3;
  m[1, 0] := 4; m[1, 1] := 5; m[1, 2] := 6;

  mT := Transposee(m);
  // mT = [ 1  4 ]
  //      [ 2  5 ]
  //      [ 3  6 ]
end;
```

### Trace (somme de la diagonale)

```pascal
function Trace(const m: TMatrix): TFloat;  
var
  i, n: Integer;
begin
  if Length(m) <> Length(m[0]) then
    raise Exception.Create('La matrice doit être carrée');

  Result := 0;
  n := Length(m);
  for i := 0 to n - 1 do
    Result := Result + m[i, i];
end;
```

## Déterminant

Le déterminant indique si une matrice est inversible (déterminant ≠ 0).

### Déterminant 2×2

```pascal
function Determinant2x2(const m: TMatrix2x2): TFloat;  
begin
  Result := m[0, 0] * m[1, 1] - m[0, 1] * m[1, 0];
end;
```

### Déterminant 3×3

```pascal
function Determinant3x3(const m: TMatrix3x3): TFloat;  
begin
  Result := m[0, 0] * (m[1, 1] * m[2, 2] - m[1, 2] * m[2, 1])
          - m[0, 1] * (m[1, 0] * m[2, 2] - m[1, 2] * m[2, 0])
          + m[0, 2] * (m[1, 0] * m[2, 1] - m[1, 1] * m[2, 0]);
end;
```

### Déterminant général (méthode de Laplace)

```pascal
function Determinant(const m: TMatrix): TFloat;  
var
  n, j: Integer;
  sousMatrice: TMatrix;
  signe: TFloat;
begin
  n := Length(m);

  if n <> Length(m[0]) then
    raise Exception.Create('La matrice doit être carrée');

  // Cas de base
  if n = 1 then
    Exit(m[0, 0]);

  if n = 2 then
    Exit(m[0, 0] * m[1, 1] - m[0, 1] * m[1, 0]);

  // Développement récursif selon la première ligne
  Result := 0;
  signe := 1;

  for j := 0 to n - 1 do
  begin
    sousMatrice := ExtraireSousMatrice(m, 0, j);
    Result := Result + signe * m[0, j] * Determinant(sousMatrice);
    signe := -signe;
  end;
end;

function ExtraireSousMatrice(const m: TMatrix;
                            ligneExclue, colonneExclue: Integer): TMatrix;
var
  i, j, newI, newJ: Integer;
  n: Integer;
begin
  n := Length(m);
  SetLength(Result, n - 1, n - 1);

  newI := 0;
  for i := 0 to n - 1 do
  begin
    if i = ligneExclue then Continue;

    newJ := 0;
    for j := 0 to n - 1 do
    begin
      if j = colonneExclue then Continue;

      Result[newI, newJ] := m[i, j];
      Inc(newJ);
    end;
    Inc(newI);
  end;
end;
```

## Inverse d'une matrice

### Inverse 2×2

```pascal
function Inverse2x2(const m: TMatrix2x2): TMatrix2x2;  
var
  det: TFloat;
begin
  det := Determinant2x2(m);

  if Abs(det) < 1e-10 then
    raise Exception.Create('Matrice singulière (non inversible)');

  Result[0, 0] :=  m[1, 1] / det;
  Result[0, 1] := -m[0, 1] / det;
  Result[1, 0] := -m[1, 0] / det;
  Result[1, 1] :=  m[0, 0] / det;
end;
```

### Inverse 3×3

```pascal
function Inverse3x3(const m: TMatrix3x3): TMatrix3x3;  
var
  det: TFloat;
begin
  det := Determinant3x3(m);

  if Abs(det) < 1e-10 then
    raise Exception.Create('Matrice singulière (non inversible)');

  // Formule de l'inverse par la matrice des cofacteurs
  Result[0, 0] := (m[1, 1] * m[2, 2] - m[1, 2] * m[2, 1]) / det;
  Result[0, 1] := (m[0, 2] * m[2, 1] - m[0, 1] * m[2, 2]) / det;
  Result[0, 2] := (m[0, 1] * m[1, 2] - m[0, 2] * m[1, 1]) / det;

  Result[1, 0] := (m[1, 2] * m[2, 0] - m[1, 0] * m[2, 2]) / det;
  Result[1, 1] := (m[0, 0] * m[2, 2] - m[0, 2] * m[2, 0]) / det;
  Result[1, 2] := (m[0, 2] * m[1, 0] - m[0, 0] * m[1, 2]) / det;

  Result[2, 0] := (m[1, 0] * m[2, 1] - m[1, 1] * m[2, 0]) / det;
  Result[2, 1] := (m[0, 1] * m[2, 0] - m[0, 0] * m[2, 1]) / det;
  Result[2, 2] := (m[0, 0] * m[1, 1] - m[0, 1] * m[1, 0]) / det;
end;
```

### Inverse générale (méthode de Gauss-Jordan)

```pascal
function InverseMatrice(const m: TMatrix): TMatrix;  
var
  n, i, j, k: Integer;
  augmentee: TMatrix;
  pivot, facteur: TFloat;
begin
  n := Length(m);

  if n <> Length(m[0]) then
    raise Exception.Create('La matrice doit être carrée');

  // Créer la matrice augmentée [M | I]
  SetLength(augmentee, n, 2 * n);
  for i := 0 to n - 1 do
  begin
    for j := 0 to n - 1 do
      augmentee[i, j] := m[i, j];
    for j := n to 2 * n - 1 do
      if j - n = i then
        augmentee[i, j] := 1
      else
        augmentee[i, j] := 0;
  end;

  // Élimination de Gauss-Jordan
  for i := 0 to n - 1 do
  begin
    // Trouver le pivot
    pivot := augmentee[i, i];
    if Abs(pivot) < 1e-10 then
      raise Exception.Create('Matrice singulière (non inversible)');

    // Normaliser la ligne du pivot
    for j := 0 to 2 * n - 1 do
      augmentee[i, j] := augmentee[i, j] / pivot;

    // Éliminer les autres lignes
    for k := 0 to n - 1 do
    begin
      if k <> i then
      begin
        facteur := augmentee[k, i];
        for j := 0 to 2 * n - 1 do
          augmentee[k, j] := augmentee[k, j] - facteur * augmentee[i, j];
      end;
    end;
  end;

  // Extraire la partie droite (l'inverse)
  SetLength(Result, n, n);
  for i := 0 to n - 1 do
    for j := 0 to n - 1 do
      Result[i, j] := augmentee[i, n + j];
end;
```

## Décompositions de matrices

### Décomposition LU

Factoriser A = L × U (Lower × Upper)

```pascal
procedure DecompositionLU(const a: TMatrix; out l, u: TMatrix);  
var
  n, i, j, k: Integer;
  somme: TFloat;
begin
  n := Length(a);
  SetLength(l, n, n);
  SetLength(u, n, n);

  // Initialiser L et U
  for i := 0 to n - 1 do
  begin
    l[i, i] := 1;  // Diagonale de L = 1
    for j := 0 to n - 1 do
    begin
      if j < i then
        u[i, j] := 0
      else if i = 0 then
        u[i, j] := a[i, j];
    end;
  end;

  // Calcul de L et U
  for j := 0 to n - 1 do
  begin
    // Colonnes de L
    for i := j + 1 to n - 1 do
    begin
      somme := 0;
      for k := 0 to j - 1 do
        somme := somme + l[i, k] * u[k, j];
      l[i, j] := (a[i, j] - somme) / u[j, j];
    end;

    // Lignes de U
    for i := j + 1 to n - 1 do
    begin
      somme := 0;
      for k := 0 to j - 1 do
        somme := somme + l[j, k] * u[k, i];
      u[j, i] := a[j, i] - somme;
    end;
  end;
end;
```

### Décomposition QR (Gram-Schmidt)

Factoriser A = Q × R (Orthogonale × Triangulaire supérieure)

```pascal
procedure DecompositionQR(const a: TMatrix; out q, r: TMatrix);  
var
  n, m, i, j, k: Integer;
  colonneA, colonneQ: TVector;
  projection, norme: TFloat;
begin
  n := Length(a);      // nombre de lignes
  m := Length(a[0]);   // nombre de colonnes

  SetLength(q, n, m);
  SetLength(r, m, m);

  // Processus de Gram-Schmidt
  for j := 0 to m - 1 do
  begin
    // Extraire la colonne j de A
    SetLength(colonneA, n);
    for i := 0 to n - 1 do
      colonneA[i] := a[i, j];

    // Soustraire les projections sur les colonnes précédentes de Q
    SetLength(colonneQ, n);
    for i := 0 to n - 1 do
      colonneQ[i] := colonneA[i];

    for k := 0 to j - 1 do
    begin
      // Calculer la projection
      projection := 0;
      for i := 0 to n - 1 do
        projection := projection + colonneA[i] * q[i, k];

      r[k, j] := projection;

      // Soustraire la projection
      for i := 0 to n - 1 do
        colonneQ[i] := colonneQ[i] - projection * q[i, k];
    end;

    // Normaliser
    norme := 0;
    for i := 0 to n - 1 do
      norme := norme + Sqr(colonneQ[i]);
    norme := Sqrt(norme);

    r[j, j] := norme;

    if norme > 1e-10 then
      for i := 0 to n - 1 do
        q[i, j] := colonneQ[i] / norme;
  end;
end;
```

### Décomposition de Cholesky

Pour matrices symétriques définies positives : A = L × L^T

```pascal
function DecompositionCholesky(const a: TMatrix): TMatrix;  
var
  n, i, j, k: Integer;
  somme: TFloat;
begin
  n := Length(a);
  SetLength(Result, n, n);

  // Initialiser à zéro
  for i := 0 to n - 1 do
    for j := 0 to n - 1 do
      Result[i, j] := 0;

  // Calcul de la décomposition
  for i := 0 to n - 1 do
  begin
    for j := 0 to i do
    begin
      somme := 0;
      for k := 0 to j - 1 do
        somme := somme + Result[i, k] * Result[j, k];

      if i = j then
      begin
        if a[i, i] - somme < 0 then
          raise Exception.Create('Matrice non définie positive');
        Result[i, j] := Sqrt(a[i, i] - somme);
      end
      else
      begin
        if Result[j, j] = 0 then
          raise Exception.Create('Division par zéro');
        Result[i, j] := (a[i, j] - somme) / Result[j, j];
      end;
    end;
  end;
end;

// Utilisation de Cholesky pour résoudre Ax = b
function ResoudreAvecCholesky(const a: TMatrix; const b: TVector): TVector;  
var
  l: TMatrix;
  y: TVector;
begin
  // A = L × L^T
  l := DecompositionCholesky(a);

  // Résoudre L × y = b (substitution avant)
  y := SubstitutionAvant(l, b);

  // Résoudre L^T × x = y (substitution arrière)
  Result := SubstitutionArriere(Transposee(l), y);
end;
```

## Valeurs propres et vecteurs propres

Les valeurs propres λ et vecteurs propres v satisfont : A × v = λ × v

### Méthode de la puissance itérée

Trouve la valeur propre dominante (plus grande en module).

```pascal
function ValeurPropreDominante(const a: TMatrix; out vecteurPropre: TVector;
                              tolerance: TFloat = 1e-10;
                              maxIterations: Integer = 1000): TFloat;
var
  n, iteration, i: Integer;
  v, vNouveau: TVector;
  lambda, lambdaPrecedent, diff: TFloat;
begin
  n := Length(a);

  // Vecteur initial aléatoire
  SetLength(v, n);
  Randomize;
  for i := 0 to n - 1 do
    v[i] := Random;
  v := Normaliser(v);

  lambda := 0;

  for iteration := 1 to maxIterations do
  begin
    lambdaPrecedent := lambda;

    // v_nouveau = A × v
    vNouveau := MultiplicationMatriceVecteur(a, v);

    // λ = v^T × v_nouveau
    lambda := ProduitScalaire(v, vNouveau);

    // Normaliser
    vNouveau := Normaliser(vNouveau);

    // Vérifier la convergence
    diff := Abs(lambda - lambdaPrecedent);
    if diff < tolerance then
    begin
      vecteurPropre := vNouveau;
      Exit(lambda);
    end;

    v := vNouveau;
  end;

  vecteurPropre := v;
  Result := lambda;
end;

// Exemple d'utilisation
var
  a: TMatrix;
  vecteur: TVector;
  valeur: TFloat;
begin
  SetLength(a, 3, 3);
  a[0, 0] := 4; a[0, 1] := 1; a[0, 2] := 0;
  a[1, 0] := 1; a[1, 1] := 3; a[1, 2] := 1;
  a[2, 0] := 0; a[2, 1] := 1; a[2, 2] := 2;

  valeur := ValeurPropreDominante(a, vecteur);
  WriteLn(Format('Valeur propre dominante: %.6f', [valeur]));
end;
```

### Méthode QR pour toutes les valeurs propres

```pascal
procedure ValeursPropreQR(const a: TMatrix; out valeursPropres: TVector;
                         maxIterations: Integer = 100);
var
  ak, q, r: TMatrix;
  n, iteration, i: Integer;
begin
  n := Length(a);
  ak := CopierMatrice(a);

  for iteration := 1 to maxIterations do
  begin
    // Décomposition QR de A_k
    DecompositionQR(ak, q, r);

    // A_{k+1} = R × Q
    ak := MultiplicationMatrices(r, q);
  end;

  // Les valeurs propres sont sur la diagonale
  SetLength(valeursPropres, n);
  for i := 0 to n - 1 do
    valeursPropres[i] := ak[i, i];
end;

function CopierMatrice(const m: TMatrix): TMatrix;  
var
  i, j: Integer;
begin
  SetLength(Result, Length(m), Length(m[0]));
  for i := 0 to High(m) do
    for j := 0 to High(m[0]) do
      Result[i, j] := m[i, j];
end;
```

## Décomposition en valeurs singulières (SVD)

La SVD décompose une matrice : A = U × Σ × V^T

```pascal
type
  TSVD = record
    U: TMatrix;      // Matrice orthogonale gauche
    Sigma: TVector;  // Valeurs singulières
    VT: TMatrix;     // Matrice orthogonale droite transposée
  end;

procedure DecompositionSVD(const a: TMatrix; out svd: TSVD);  
var
  aTa, aaT: TMatrix;
  valeursPropres: TVector;
  n, m, i: Integer;
begin
  n := Length(a);      // lignes
  m := Length(a[0]);   // colonnes

  // Calculer A^T × A pour obtenir V
  aTa := MultiplicationMatrices(Transposee(a), a);
  ValeursPropreQR(aTa, valeursPropres);

  // Les valeurs singulières sont les racines carrées des valeurs propres
  SetLength(svd.Sigma, Min(n, m));
  for i := 0 to High(svd.Sigma) do
    svd.Sigma[i] := Sqrt(Abs(valeursPropres[i]));

  // Calculer A × A^T pour obtenir U
  aaT := MultiplicationMatrices(a, Transposee(a));
  // ... (implémentation complète nécessite plus de calculs)
end;
```

## Pseudo-inverse (Moore-Penrose)

Pour résoudre des systèmes sur-déterminés ou sous-déterminés.

```pascal
function PseudoInverse(const a: TMatrix): TMatrix;  
var
  aT, aTa: TMatrix;
begin
  // Pour matrice pleine colonne : A^+ = (A^T × A)^(-1) × A^T
  aT := Transposee(a);
  aTa := MultiplicationMatrices(aT, a);

  try
    Result := MultiplicationMatrices(InverseMatrice(aTa), aT);
  except
    on E: Exception do
    begin
      // Utiliser la SVD pour le cas général
      WriteLn('Utilisation de la SVD pour la pseudo-inverse');
      // Result := PseudoInverseSVD(a);
      raise;
    end;
  end;
end;

// Résolution de systèmes par moindres carrés
function MoindresCarres(const a: TMatrix; const b: TVector): TVector;  
var
  aPlus: TMatrix;
begin
  // x = A^+ × b minimise ||Ax - b||²
  aPlus := PseudoInverse(a);
  Result := MultiplicationMatriceVecteur(aPlus, b);
end;
```

## Rang d'une matrice

Le rang est le nombre de lignes/colonnes linéairement indépendantes.

```pascal
function RangMatrice(const a: TMatrix; tolerance: TFloat = 1e-10): Integer;  
var
  m: TMatrix;
  n, i, j, k: Integer;
  pivot, facteur: TFloat;
  lignesNonNulles: Integer;
begin
  m := CopierMatrice(a);
  n := Length(m);

  // Réduction en forme échelonnée
  for i := 0 to n - 1 do
  begin
    // Trouver le pivot
    pivot := 0;
    for j := i to n - 1 do
      if Abs(m[j, i]) > Abs(pivot) then
        pivot := m[j, i];

    if Abs(pivot) < tolerance then
      Continue;

    // Normaliser la ligne du pivot
    for j := 0 to High(m[0]) do
      m[i, j] := m[i, j] / pivot;

    // Éliminer les lignes en dessous
    for k := i + 1 to n - 1 do
    begin
      facteur := m[k, i];
      for j := 0 to High(m[0]) do
        m[k, j] := m[k, j] - facteur * m[i, j];
    end;
  end;

  // Compter les lignes non nulles
  lignesNonNulles := 0;
  for i := 0 to n - 1 do
  begin
    for j := 0 to High(m[0]) do
    begin
      if Abs(m[i, j]) > tolerance then
      begin
        Inc(lignesNonNulles);
        Break;
      end;
    end;
  end;

  Result := lignesNonNulles;
end;
```

## Résolution de systèmes linéaires

### Substitution avant (Forward substitution)

Pour systèmes triangulaires inférieurs : L × x = b

```pascal
function SubstitutionAvant(const l: TMatrix; const b: TVector): TVector;  
var
  n, i, j: Integer;
  somme: TFloat;
begin
  n := Length(l);
  SetLength(Result, n);

  for i := 0 to n - 1 do
  begin
    somme := b[i];
    for j := 0 to i - 1 do
      somme := somme - l[i, j] * Result[j];
    Result[i] := somme / l[i, i];
  end;
end;
```

### Substitution arrière (Backward substitution)

Pour systèmes triangulaires supérieurs : U × x = b

```pascal
function SubstitutionArriere(const u: TMatrix; const b: TVector): TVector;  
var
  n, i, j: Integer;
  somme: TFloat;
begin
  n := Length(u);
  SetLength(Result, n);

  for i := n - 1 downto 0 do
  begin
    somme := b[i];
    for j := i + 1 to n - 1 do
      somme := somme - u[i, j] * Result[j];
    Result[i] := somme / u[i, i];
  end;
end;
```

### Résolution avec décomposition LU

```pascal
function ResoudreSystemeLU(const a: TMatrix; const b: TVector): TVector;  
var
  l, u: TMatrix;
  y: TVector;
begin
  // A = L × U
  DecompositionLU(a, l, u);

  // Résoudre L × y = b
  y := SubstitutionAvant(l, b);

  // Résoudre U × x = y
  Result := SubstitutionArriere(u, y);
end;
```

## Normes de matrices

### Norme de Frobenius

```pascal
function NormeFrobenius(const m: TMatrix): TFloat;  
var
  i, j: Integer;
begin
  Result := 0;
  for i := 0 to High(m) do
    for j := 0 to High(m[0]) do
      Result := Result + Sqr(m[i, j]);
  Result := Sqrt(Result);
end;
```

### Norme infinie (maximum de ligne)

```pascal
function NormeInfinieMatrice(const m: TMatrix): TFloat;  
var
  i, j: Integer;
  sommeLigne: TFloat;
begin
  Result := 0;
  for i := 0 to High(m) do
  begin
    sommeLigne := 0;
    for j := 0 to High(m[0]) do
      sommeLigne := sommeLigne + Abs(m[i, j]);
    if sommeLigne > Result then
      Result := sommeLigne;
  end;
end;
```

### Norme 1 (maximum de colonne)

```pascal
function Norme1Matrice(const m: TMatrix): TFloat;  
var
  i, j: Integer;
  sommeColonne: TFloat;
begin
  Result := 0;
  for j := 0 to High(m[0]) do
  begin
    sommeColonne := 0;
    for i := 0 to High(m) do
      sommeColonne := sommeColonne + Abs(m[i, j]);
    if sommeColonne > Result then
      Result := sommeColonne;
  end;
end;
```

## Nombre de condition

Mesure la sensibilité d'un système aux erreurs.

```pascal
function NombreCondition(const m: TMatrix): TFloat;  
var
  mInv: TMatrix;
  normeA, normeAInv: TFloat;
begin
  try
    mInv := InverseMatrice(m);
    normeA := NormeFrobenius(m);
    normeAInv := NormeFrobenius(mInv);
    Result := normeA * normeAInv;
  except
    Result := Infinity;  // Matrice singulière
  end;
end;

// Interprétation
procedure AnalyserCondition(const m: TMatrix);  
var
  cond: TFloat;
begin
  cond := NombreCondition(m);

  WriteLn(Format('Nombre de condition: %.2e', [cond]));

  if cond < 10 then
    WriteLn('Système bien conditionné')
  else if cond < 1000 then
    WriteLn('Système modérément conditionné')
  else if cond < 1e6 then
    WriteLn('Système mal conditionné')
  else
    WriteLn('Système très mal conditionné - résultats peu fiables');
end;
```

## Transformations géométriques 2D et 3D

### Matrices de rotation 2D

```pascal
function MatriceRotation2D(angleRadians: TFloat): TMatrix3x3;  
var
  c, s: TFloat;
begin
  c := Cos(angleRadians);
  s := Sin(angleRadians);

  // Matrice homogène 3×3 pour rotation 2D
  Result[0, 0] := c;  Result[0, 1] := -s; Result[0, 2] := 0;
  Result[1, 0] := s;  Result[1, 1] :=  c; Result[1, 2] := 0;
  Result[2, 0] := 0;  Result[2, 1] :=  0; Result[2, 2] := 1;
end;

function MatriceTranslation2D(dx, dy: TFloat): TMatrix3x3;  
begin
  Result[0, 0] := 1; Result[0, 1] := 0; Result[0, 2] := dx;
  Result[1, 0] := 0; Result[1, 1] := 1; Result[1, 2] := dy;
  Result[2, 0] := 0; Result[2, 1] := 0; Result[2, 2] := 1;
end;

function MatriceEchelle2D(sx, sy: TFloat): TMatrix3x3;  
begin
  Result[0, 0] := sx; Result[0, 1] := 0;  Result[0, 2] := 0;
  Result[1, 0] := 0;  Result[1, 1] := sy; Result[1, 2] := 0;
  Result[2, 0] := 0;  Result[2, 1] := 0;  Result[2, 2] := 1;
end;
```

### Matrices de rotation 3D

```pascal
// Rotation autour de l'axe X
function MatriceRotationX(angleRadians: TFloat): TMatrix4x4;  
var
  c, s: TFloat;
begin
  c := Cos(angleRadians);
  s := Sin(angleRadians);

  Result[0, 0] := 1; Result[0, 1] := 0; Result[0, 2] :=  0; Result[0, 3] := 0;
  Result[1, 0] := 0; Result[1, 1] := c; Result[1, 2] := -s; Result[1, 3] := 0;
  Result[2, 0] := 0; Result[2, 1] := s; Result[2, 2] :=  c; Result[2, 3] := 0;
  Result[3, 0] := 0; Result[3, 1] := 0; Result[3, 2] :=  0; Result[3, 3] := 1;
end;

// Rotation autour de l'axe Y
function MatriceRotationY(angleRadians: TFloat): TMatrix4x4;  
var
  c, s: TFloat;
begin
  c := Cos(angleRadians);
  s := Sin(angleRadians);

  Result[0, 0] :=  c; Result[0, 1] := 0; Result[0, 2] := s; Result[0, 3] := 0;
  Result[1, 0] :=  0; Result[1, 1] := 1; Result[1, 2] := 0; Result[1, 3] := 0;
  Result[2, 0] := -s; Result[2, 1] := 0; Result[2, 2] := c; Result[2, 3] := 0;
  Result[3, 0] :=  0; Result[3, 1] := 0; Result[3, 2] := 0; Result[3, 3] := 1;
end;

// Rotation autour de l'axe Z
function MatriceRotationZ(angleRadians: TFloat): TMatrix4x4;  
var
  c, s: TFloat;
begin
  c := Cos(angleRadians);
  s := Sin(angleRadians);

  Result[0, 0] := c; Result[0, 1] := -s; Result[0, 2] := 0; Result[0, 3] := 0;
  Result[1, 0] := s; Result[1, 1] :=  c; Result[1, 2] := 0; Result[1, 3] := 0;
  Result[2, 0] := 0; Result[2, 1] :=  0; Result[2, 2] := 1; Result[2, 3] := 0;
  Result[3, 0] := 0; Result[3, 1] :=  0; Result[3, 2] := 0; Result[3, 3] := 1;
end;

// Translation 3D
function MatriceTranslation3D(dx, dy, dz: TFloat): TMatrix4x4;  
begin
  Result[0, 0] := 1; Result[0, 1] := 0; Result[0, 2] := 0; Result[0, 3] := dx;
  Result[1, 0] := 0; Result[1, 1] := 1; Result[1, 2] := 0; Result[1, 3] := dy;
  Result[2, 0] := 0; Result[2, 1] := 0; Result[2, 2] := 1; Result[2, 3] := dz;
  Result[3, 0] := 0; Result[3, 1] := 0; Result[3, 2] := 0; Result[3, 3] := 1;
end;
```

### Application de transformations

```pascal
procedure TransformerPoint2D(var point: TVector2D; const transformation: TMatrix3x3);  
var
  pointHomogene: array[0..2] of TFloat;
  i, j: Integer;
begin
  // Convertir en coordonnées homogènes
  pointHomogene[0] := point[0];
  pointHomogene[1] := point[1];
  pointHomogene[2] := 1;

  // Appliquer la transformation
  for i := 0 to 2 do
  begin
    point[i] := 0;
    for j := 0 to 2 do
      point[i] := point[i] + transformation[i, j] * pointHomogene[j];
  end;
end;

// Exemple : rotation puis translation
var
  point: TVector2D;
  rotation, translation, transformation: TMatrix3x3;
begin
  point[0] := 1;
  point[1] := 0;

  rotation := MatriceRotation2D(Pi / 4);  // 45 degrés
  translation := MatriceTranslation2D(5, 3);

  // Combiner les transformations (ordre important!)
  transformation := MultiplicationMatrices(translation, rotation);

  TransformerPoint2D(point, transformation);
  WriteLn(Format('Point transformé: (%.2f, %.2f)', [point[0], point[1]]));
end;
```

## Utilitaires et fonctions d'affichage

### Affichage formaté

```pascal
procedure AfficherVecteur(const v: TVector; nom: String = '');  
var
  i: Integer;
begin
  if nom <> '' then
    Write(nom, ' = ');
  Write('[');
  for i := 0 to High(v) do
  begin
    Write(Format('%.4f', [v[i]]));
    if i < High(v) then Write(', ');
  end;
  WriteLn(']');
end;

procedure AfficherMatrice(const m: TMatrix; nom: String = '');  
var
  i, j: Integer;
begin
  if nom <> '' then
    WriteLn(nom, ' =');

  for i := 0 to High(m) do
  begin
    Write('[ ');
    for j := 0 to High(m[0]) do
    begin
      Write(Format('%8.4f', [m[i, j]]));
      if j < High(m[0]) then Write('  ');
    end;
    WriteLn(' ]');
  end;
  WriteLn;
end;
```

### Vérification d'égalité avec tolérance

```pascal
function VecteursEgaux(const a, b: TVector; tolerance: TFloat = 1e-10): Boolean;  
var
  i: Integer;
begin
  if Length(a) <> Length(b) then
    Exit(False);

  for i := 0 to High(a) do
    if Abs(a[i] - b[i]) > tolerance then
      Exit(False);

  Result := True;
end;

function MatricesEgales(const a, b: TMatrix; tolerance: TFloat = 1e-10): Boolean;  
var
  i, j: Integer;
begin
  if (Length(a) <> Length(b)) or (Length(a[0]) <> Length(b[0])) then
    Exit(False);

  for i := 0 to High(a) do
    for j := 0 to High(a[0]) do
      if Abs(a[i, j] - b[i, j]) > tolerance then
        Exit(False);

  Result := True;
end;
```

### Propriétés des matrices

```pascal
function EstSymetrique(const m: TMatrix; tolerance: TFloat = 1e-10): Boolean;  
var
  i, j, n: Integer;
begin
  n := Length(m);
  if n <> Length(m[0]) then
    Exit(False);

  for i := 0 to n - 1 do
    for j := i + 1 to n - 1 do
      if Abs(m[i, j] - m[j, i]) > tolerance then
        Exit(False);

  Result := True;
end;

function EstOrthogonale(const m: TMatrix; tolerance: TFloat = 1e-10): Boolean;  
var
  mT, produit, identite: TMatrix;
begin
  if Length(m) <> Length(m[0]) then
    Exit(False);

  mT := Transposee(m);
  produit := MultiplicationMatrices(m, mT);
  identite := MatriceIdentite(Length(m));

  Result := MatricesEgales(produit, identite, tolerance);
end;

function EstDiagonale(const m: TMatrix; tolerance: TFloat = 1e-10): Boolean;  
var
  i, j, n: Integer;
begin
  n := Length(m);
  if n <> Length(m[0]) then
    Exit(False);

  for i := 0 to n - 1 do
    for j := 0 to n - 1 do
      if (i <> j) and (Abs(m[i, j]) > tolerance) then
        Exit(False);

  Result := True;
end;
```

## Applications pratiques

### Exemple 1 : Régression linéaire matricielle

```pascal
program RegressionMatricielle;

uses
  UnitAlgebreLineaire;

// Résoudre y = X × β par moindres carrés
// β = (X^T × X)^(-1) × X^T × y

function RegressionLineaire(const x: TMatrix; const y: TVector): TVector;  
var
  xT, xTx, xTxInv: TMatrix;
  xTy: TVector;
begin
  // X^T
  xT := Transposee(x);

  // X^T × X
  xTx := MultiplicationMatrices(xT, x);

  // (X^T × X)^(-1)
  xTxInv := InverseMatrice(xTx);

  // X^T × y
  xTy := MultiplicationMatriceVecteur(xT, y);

  // β = (X^T × X)^(-1) × X^T × y
  Result := MultiplicationMatriceVecteur(xTxInv, xTy);
end;

var
  x: TMatrix;
  y, beta: TVector;
  i, n: Integer;

begin
  n := 10;

  // Données : y = 2x + 3 + bruit
  SetLength(x, n, 2);  // Colonne 1: constantes, Colonne 2: x
  SetLength(y, n);

  Randomize;
  for i := 0 to n - 1 do
  begin
    x[i, 0] := 1;  // Intercept
    x[i, 1] := i;
    y[i] := 2 * i + 3 + (Random - 0.5) * 2;  // Bruit
  end;

  // Calculer les coefficients
  beta := RegressionLineaire(x, y);

  WriteLn('Équation : y = ', Format('%.2f', [beta[1]]), ' × x + ',
          Format('%.2f', [beta[0]]));
end.
```

### Exemple 2 : Système de masses-ressorts

```pascal
// Résoudre un système de masses reliées par des ressorts
// K × x = F où K est la matrice de raideur

procedure SystemeMassesRessorts;  
var
  k: TMatrix;
  f, x: TVector;
  n, i: Integer;
  raideur: TFloat;
begin
  n := 3;  // 3 masses
  raideur := 100;  // N/m

  // Matrice de raideur
  SetLength(k, n, n);
  k[0, 0] := 2 * raideur; k[0, 1] := -raideur;   k[0, 2] := 0;
  k[1, 0] := -raideur;    k[1, 1] := 2 * raideur; k[1, 2] := -raideur;
  k[2, 0] := 0;           k[2, 1] := -raideur;    k[2, 2] := raideur;

  // Forces appliquées
  SetLength(f, n);
  f[0] := 0;
  f[1] := 10;  // 10 N sur la masse 2
  f[2] := 0;

  // Résoudre K × x = F
  x := ResoudreSystemeLU(k, f);

  WriteLn('Déplacements (m):');
  for i := 0 to n - 1 do
    WriteLn(Format('Masse %d: %.6f', [i + 1, x[i]]));
end;
```

### Exemple 3 : Analyse en composantes principales (PCA)

```pascal
procedure AnalysePCA(const donnees: TMatrix; nbComposantes: Integer);  
var
  moyenne: TVector;
  donneesCentrees, covariance: TMatrix;
  valeursPropres: TVector;
  vecteursPropres: TMatrix;
  i, j, n, m: Integer;
  somme: TFloat;
  varianceExpliquee, varianceTotale: TFloat;
begin
  n := Length(donnees);      // Nombre d'observations
  m := Length(donnees[0]);   // Nombre de variables

  // Étape 1 : Centrer les données (soustraire la moyenne)
  SetLength(moyenne, m);
  SetLength(donneesCentrees, n, m);

  // Calculer les moyennes de chaque colonne
  for j := 0 to m - 1 do
  begin
    somme := 0;
    for i := 0 to n - 1 do
      somme := somme + donnees[i, j];
    moyenne[j] := somme / n;
  end;

  // Centrer les données
  for i := 0 to n - 1 do
    for j := 0 to m - 1 do
      donneesCentrees[i, j] := donnees[i, j] - moyenne[j];

  // Étape 2 : Calculer la matrice de covariance
  // Cov = (1/n) × X^T × X
  covariance := MultiplicationMatrices(
    Transposee(donneesCentrees),
    donneesCentrees
  );

  for i := 0 to m - 1 do
    for j := 0 to m - 1 do
      covariance[i, j] := covariance[i, j] / n;

  // Étape 3 : Calculer les valeurs et vecteurs propres
  ValeursPropreQR(covariance, valeursPropres);
  // vecteursPropres := CalculerVecteursPropres(covariance, valeursPropres);

  // Étape 4 : Calculer la variance expliquée
  varianceTotale := 0;
  for i := 0 to High(valeursPropres) do
    varianceTotale := varianceTotale + valeursPropres[i];

  WriteLn('Analyse en Composantes Principales:');
  WriteLn('------------------------------------');

  varianceExpliquee := 0;
  for i := 0 to Min(nbComposantes - 1, High(valeursPropres)) do
  begin
    varianceExpliquee := varianceExpliquee + valeursPropres[i];
    WriteLn(Format('PC%d: Valeur propre = %.4f, Variance = %.2f%%',
      [i + 1, valeursPropres[i],
       100 * valeursPropres[i] / varianceTotale]));
  end;

  WriteLn(Format('Variance totale expliquée: %.2f%%',
    [100 * varianceExpliquee / varianceTotale]));
end;
```

## Optimisation matricielle

### Gradient descendant matriciel

```pascal
function GradientDescendantMatriciel(const x: TMatrix; const y: TVector;
                                     tauxApprentissage: TFloat;
                                     nbIterations: Integer): TVector;
var
  n, m, iteration, i: Integer;
  beta, gradient: TVector;
  predictions, erreurs: TVector;
  xT: TMatrix;
  coutActuel: TFloat;
begin
  n := Length(x);      // Nombre d'observations
  m := Length(x[0]);   // Nombre de features

  // Initialiser beta à zéro
  SetLength(beta, m);
  for i := 0 to m - 1 do
    beta[i] := 0;

  xT := Transposee(x);

  for iteration := 1 to nbIterations do
  begin
    // Prédictions : y_pred = X × beta
    predictions := MultiplicationMatriceVecteur(x, beta);

    // Erreurs : e = y_pred - y
    SetLength(erreurs, n);
    for i := 0 to n - 1 do
      erreurs[i] := predictions[i] - y[i];

    // Gradient : grad = (1/n) × X^T × e
    gradient := MultiplicationMatriceVecteur(xT, erreurs);
    for i := 0 to m - 1 do
      gradient[i] := gradient[i] / n;

    // Mise à jour : beta = beta - α × grad
    for i := 0 to m - 1 do
      beta[i] := beta[i] - tauxApprentissage * gradient[i];

    // Calculer le coût (optionnel, pour monitoring)
    if iteration mod 100 = 0 then
    begin
      coutActuel := 0;
      for i := 0 to n - 1 do
        coutActuel := coutActuel + Sqr(erreurs[i]);
      coutActuel := coutActuel / (2 * n);
      WriteLn(Format('Itération %d: Coût = %.6f', [iteration, coutActuel]));
    end;
  end;

  Result := beta;
end;
```

### Régularisation (Ridge et Lasso)

```pascal
// Régression Ridge : minimise ||y - Xβ||² + λ||β||²
function RegressionRidge(const x: TMatrix; const y: TVector;
                        lambda: TFloat): TVector;
var
  xT, xTx, identite, xTxPlusLambdaI, xTxInv: TMatrix;
  xTy: TVector;
  i, n: Integer;
begin
  n := Length(x[0]);

  // X^T × X
  xT := Transposee(x);
  xTx := MultiplicationMatrices(xT, x);

  // X^T × X + λI
  identite := MatriceIdentite(n);
  for i := 0 to n - 1 do
    identite[i, i] := lambda;

  xTxPlusLambdaI := AdditionMatrices(xTx, identite);

  // (X^T × X + λI)^(-1)
  xTxInv := InverseMatrice(xTxPlusLambdaI);

  // X^T × y
  xTy := MultiplicationMatriceVecteur(xT, y);

  // β = (X^T × X + λI)^(-1) × X^T × y
  Result := MultiplicationMatriceVecteur(xTxInv, xTy);
end;
```

## Matrices creuses (Sparse Matrices)

Pour matrices avec beaucoup de zéros, utiliser une représentation compacte.

```pascal
type
  TElementCreux = record
    Ligne: Integer;
    Colonne: Integer;
    Valeur: TFloat;
  end;

  TMatriceCreuse = record
    Elements: array of TElementCreux;
    NbLignes: Integer;
    NbColonnes: Integer;
  end;

function CreerMatriceCreuse(lignes, colonnes: Integer): TMatriceCreuse;  
begin
  Result.NbLignes := lignes;
  Result.NbColonnes := colonnes;
  SetLength(Result.Elements, 0);
end;

procedure AjouterElement(var m: TMatriceCreuse;
                        ligne, colonne: Integer; valeur: TFloat);
var
  n: Integer;
begin
  if Abs(valeur) < 1e-10 then Exit;  // Ne pas stocker les zéros

  n := Length(m.Elements);
  SetLength(m.Elements, n + 1);
  m.Elements[n].Ligne := ligne;
  m.Elements[n].Colonne := colonne;
  m.Elements[n].Valeur := valeur;
end;

function ObtenirElement(const m: TMatriceCreuse;
                       ligne, colonne: Integer): TFloat;
var
  i: Integer;
begin
  for i := 0 to High(m.Elements) do
    if (m.Elements[i].Ligne = ligne) and
       (m.Elements[i].Colonne = colonne) then
      Exit(m.Elements[i].Valeur);

  Result := 0;  // Élément non trouvé = zéro
end;

function MultiplicationMatriceCreuseVecteur(const m: TMatriceCreuse;
                                           const v: TVector): TVector;
var
  i: Integer;
begin
  SetLength(Result, m.NbLignes);

  // Initialiser à zéro
  for i := 0 to m.NbLignes - 1 do
    Result[i] := 0;

  // Multiplier uniquement les éléments non nuls
  for i := 0 to High(m.Elements) do
    Result[m.Elements[i].Ligne] := Result[m.Elements[i].Ligne] +
      m.Elements[i].Valeur * v[m.Elements[i].Colonne];
end;
```

## Matrices par blocs

Pour manipuler efficacement de grandes matrices.

```pascal
type
  TBlocMatrice = record
    Debut: record
      Ligne: Integer;
      Colonne: Integer;
    end;
    Taille: record
      Lignes: Integer;
      Colonnes: Integer;
    end;
  end;

function ExtraireBloc(const m: TMatrix; const bloc: TBlocMatrice): TMatrix;  
var
  i, j: Integer;
begin
  SetLength(Result, bloc.Taille.Lignes, bloc.Taille.Colonnes);

  for i := 0 to bloc.Taille.Lignes - 1 do
    for j := 0 to bloc.Taille.Colonnes - 1 do
      Result[i, j] := m[bloc.Debut.Ligne + i, bloc.Debut.Colonne + j];
end;

procedure InsererBloc(var m: TMatrix; const bloc: TMatrix;
                     ligneDebut, colonneDebut: Integer);
var
  i, j: Integer;
begin
  for i := 0 to High(bloc) do
    for j := 0 to High(bloc[0]) do
      m[ligneDebut + i, colonneDebut + j] := bloc[i, j];
end;

// Multiplication par blocs (pour grandes matrices)
function MultiplicationParBlocs(const a, b: TMatrix;
                               tailleBloc: Integer): TMatrix;
var
  n, i, j, k: Integer;
  blocA, blocB, blocC: TMatrix;
begin
  n := Length(a);
  SetLength(Result, n, n);

  // Initialiser à zéro
  for i := 0 to n - 1 do
    for j := 0 to n - 1 do
      Result[i, j] := 0;

  // Multiplier par blocs
  i := 0;
  while i < n do
  begin
    j := 0;
    while j < n do
    begin
      k := 0;
      while k < n do
      begin
        // Extraire les blocs
        // blocA := ExtraireBloc(...);
        // blocB := ExtraireBloc(...);
        // blocC := MultiplicationMatrices(blocA, blocB);
        // Ajouter blocC au résultat

        Inc(k, tailleBloc);
      end;
      Inc(j, tailleBloc);
    end;
    Inc(i, tailleBloc);
  end;
end;
```

## Algorithmes itératifs pour grands systèmes

### Méthode de Jacobi

```pascal
function MethodeJacobi(const a: TMatrix; const b: TVector;
                      x0: TVector; tolerance: TFloat;
                      maxIterations: Integer): TVector;
var
  n, iteration, i, j: Integer;
  xNouveau: TVector;
  somme, erreur: TFloat;
  converge: Boolean;
begin
  n := Length(a);
  SetLength(xNouveau, n);
  Result := x0;

  for iteration := 1 to maxIterations do
  begin
    // Calculer la nouvelle approximation
    for i := 0 to n - 1 do
    begin
      somme := 0;
      for j := 0 to n - 1 do
      begin
        if j <> i then
          somme := somme + a[i, j] * Result[j];
      end;
      xNouveau[i] := (b[i] - somme) / a[i, i];
    end;

    // Vérifier la convergence
    erreur := 0;
    for i := 0 to n - 1 do
      erreur := erreur + Sqr(xNouveau[i] - Result[i]);
    erreur := Sqrt(erreur);

    Result := xNouveau;

    if erreur < tolerance then
    begin
      WriteLn(Format('Convergence atteinte à l''itération %d', [iteration]));
      Exit;
    end;
  end;

  WriteLn('Nombre maximum d''itérations atteint');
end;
```

### Méthode de Gauss-Seidel

```pascal
function MethodeGaussSeidel(const a: TMatrix; const b: TVector;
                           x0: TVector; tolerance: TFloat;
                           maxIterations: Integer): TVector;
var
  n, iteration, i, j: Integer;
  somme, erreur, ancien: TFloat;
begin
  n := Length(a);
  Result := x0;

  for iteration := 1 to maxIterations do
  begin
    erreur := 0;

    for i := 0 to n - 1 do
    begin
      ancien := Result[i];
      somme := 0;

      // Utiliser les valeurs déjà mises à jour
      for j := 0 to n - 1 do
      begin
        if j <> i then
          somme := somme + a[i, j] * Result[j];
      end;

      Result[i] := (b[i] - somme) / a[i, i];
      erreur := erreur + Sqr(Result[i] - ancien);
    end;

    erreur := Sqrt(erreur);

    if erreur < tolerance then
    begin
      WriteLn(Format('Convergence atteinte à l''itération %d', [iteration]));
      Exit;
    end;
  end;

  WriteLn('Nombre maximum d''itérations atteint');
end;
```

### Méthode du gradient conjugué

Pour systèmes symétriques définis positifs (plus rapide que Jacobi/Gauss-Seidel).

```pascal
function GradientConjugue(const a: TMatrix; const b: TVector;
                         x0: TVector; tolerance: TFloat;
                         maxIterations: Integer): TVector;
var
  n, iteration, i: Integer;
  r, p, ap: TVector;
  alpha, beta, rsOld, rsNew: TFloat;
begin
  n := Length(a);
  Result := x0;

  // r = b - A × x0
  r := SoustractionVecteurs(b, MultiplicationMatriceVecteur(a, Result));
  p := r;
  rsOld := ProduitScalaire(r, r);

  for iteration := 1 to maxIterations do
  begin
    // ap = A × p
    ap := MultiplicationMatriceVecteur(a, p);

    // α = r^T × r / (p^T × A × p)
    alpha := rsOld / ProduitScalaire(p, ap);

    // x = x + α × p
    for i := 0 to n - 1 do
      Result[i] := Result[i] + alpha * p[i];

    // r = r - α × A × p
    for i := 0 to n - 1 do
      r[i] := r[i] - alpha * ap[i];

    rsNew := ProduitScalaire(r, r);

    // Test de convergence
    if Sqrt(rsNew) < tolerance then
    begin
      WriteLn(Format('Convergence atteinte à l''itération %d', [iteration]));
      Exit;
    end;

    // β = r_new^T × r_new / r_old^T × r_old
    beta := rsNew / rsOld;

    // p = r + β × p
    for i := 0 to n - 1 do
      p[i] := r[i] + beta * p[i];

    rsOld := rsNew;
  end;

  WriteLn('Nombre maximum d''itérations atteint');
end;
```

## Considérations multi-plateformes

### Optimisations spécifiques

```pascal
{$IFDEF CPUX86_64}
// Utiliser les instructions SIMD si disponibles
{$DEFINE USE_SIMD}
{$ENDIF}

{$IFDEF USE_SIMD}
uses
  cpu;

procedure MultiplicationMatriceOptimisee(const a, b: TMatrix;
                                        out c: TMatrix);
var
  i, j, k: Integer;
  va, vb, vc: TM128;
begin
  // Code optimisé avec instructions SSE/AVX
  // ...
end;
{$ENDIF}
```

### Bibliothèques externes

```pascal
// Utilisation de BLAS/LAPACK
{$IFDEF UNIX}
const
  BLAS_LIB = 'libblas.so.3';
  LAPACK_LIB = 'liblapack.so.3';
{$ENDIF}

{$IFDEF WINDOWS}
const
  BLAS_LIB = 'libblas.dll';
  LAPACK_LIB = 'liblapack.dll';
{$ENDIF}

// Multiplication matrice-vecteur avec BLAS
procedure dgemv(trans: PChar; m, n: PInteger; alpha: PDouble;
               a: PDouble; lda: PInteger; x: PDouble; incx: PInteger;
               beta: PDouble; y: PDouble; incy: PInteger);
               cdecl; external BLAS_LIB;

function MultiplicationAvecBLAS(const a: TMatrix; const v: TVector): TVector;  
var
  m, n, lda, incx, incy: Integer;
  alpha, beta: Double;
begin
  m := Length(a);
  n := Length(a[0]);
  lda := m;
  incx := 1;
  incy := 1;
  alpha := 1.0;
  beta := 0.0;

  SetLength(Result, m);

  dgemv('N', @m, @n, @alpha, @a[0, 0], @lda,
        @v[0], @incx, @beta, @Result[0], @incy);
end;
```

## Tests et validation

### Suite de tests unitaires

```pascal
program TestsAlgebreLineaire;

uses
  UnitAlgebreLineaire;

procedure TestAdditionVecteurs;  
var
  v1, v2, resultat, attendu: TVector;
begin
  SetLength(v1, 3);
  SetLength(v2, 3);

  v1[0] := 1; v1[1] := 2; v1[2] := 3;
  v2[0] := 4; v2[1] := 5; v2[2] := 6;

  resultat := AdditionVecteurs(v1, v2);

  SetLength(attendu, 3);
  attendu[0] := 5; attendu[1] := 7; attendu[2] := 9;

  if VecteursEgaux(resultat, attendu) then
    WriteLn('✓ TestAdditionVecteurs RÉUSSI')
  else
    WriteLn('✗ TestAdditionVecteurs ÉCHOUÉ');
end;

procedure TestInverseMatrice;  
var
  m, mInv, produit, identite: TMatrix;
begin
  SetLength(m, 2, 2);
  m[0, 0] := 4; m[0, 1] := 7;
  m[1, 0] := 2; m[1, 1] := 6;

  mInv := InverseMatrice(m);
  produit := MultiplicationMatrices(m, mInv);
  identite := MatriceIdentite(2);

  if MatricesEgales(produit, identite, 1e-10) then
    WriteLn('✓ TestInverseMatrice RÉUSSI')
  else
    WriteLn('✗ TestInverseMatrice ÉCHOUÉ');
end;

procedure TestDeterminant;  
var
  m: TMatrix;
  det: TFloat;
begin
  SetLength(m, 3, 3);
  m[0, 0] := 6; m[0, 1] := 1; m[0, 2] := 1;
  m[1, 0] := 4; m[1, 1] := -2; m[1, 2] := 5;
  m[2, 0] := 2; m[2, 1] := 8; m[2, 2] := 7;

  det := Determinant(m);

  // det = -306
  if Abs(det - (-306)) < 1e-10 then
    WriteLn('✓ TestDeterminant RÉUSSI')
  else
    WriteLn('✗ TestDeterminant ÉCHOUÉ (calculé: ', det:0:2, ')');
end;

procedure TestDecompositionLU;  
var
  a, l, u, produit: TMatrix;
begin
  SetLength(a, 3, 3);
  a[0, 0] := 2; a[0, 1] := 1; a[0, 2] := 1;
  a[1, 0] := 4; a[1, 1] := 3; a[1, 2] := 3;
  a[2, 0] := 8; a[2, 1] := 7; a[2, 2] := 9;

  DecompositionLU(a, l, u);
  produit := MultiplicationMatrices(l, u);

  if MatricesEgales(produit, a, 1e-10) then
    WriteLn('✓ TestDecompositionLU RÉUSSI')
  else
    WriteLn('✗ TestDecompositionLU ÉCHOUÉ');
end;

begin
  WriteLn('=== Tests d''Algèbre Linéaire ===');
  WriteLn;

  TestAdditionVecteurs;
  TestInverseMatrice;
  TestDeterminant;
  TestDecompositionLU;

  WriteLn;
  WriteLn('Tests terminés.');
end.
```

## Gestion des erreurs et robustesse

```pascal
type
  EAlgebreLineaire = class(Exception);
  EDimensionIncompatible = class(EAlgebreLineaire);
  EMatriceSinguliere = class(EAlgebreLineaire);
  EDivisionParZero = class(EAlgebreLineaire);

procedure VerifierDimensionsMultiplication(const a, b: TMatrix);  
begin
  if Length(a[0]) <> Length(b) then
    raise EDimensionIncompatible.CreateFmt(
      'Multiplication impossible: %d colonnes vs %d lignes',
      [Length(a[0]), Length(b)]);
end;

procedure VerifierMatriceCarree(const m: TMatrix; nom: String);  
begin
  if Length(m) <> Length(m[0]) then
    raise EAlgebreLineaire.CreateFmt(
      'La matrice %s doit être carrée (%d×%d)',
      [nom, Length(m), Length(m[0])]);
end;

function InverseSecurisee(const m: TMatrix): TMatrix;  
var
  det: TFloat;
begin
  VerifierMatriceCarree(m, 'à inverser');

  det := Determinant(m);
  if Abs(det) < 1e-10 then
    raise EMatriceSinguliere.Create(
      'La matrice est singulière (déterminant ≈ 0)');

  Result := InverseMatrice(m);
end;
```

## Performances et benchmarking

```pascal
uses
  SysUtils, DateUtils;

procedure BenchmarkMultiplication;  
var
  a, b, c: TMatrix;
  i, taille: Integer;
  debut, fin: TDateTime;
  duree: Int64;
begin
  WriteLn('=== Benchmark Multiplication de Matrices ===');
  WriteLn;

  for taille := 10 to 100 do
  begin
    if taille mod 10 <> 0 then Continue;

    // Créer des matrices aléatoires
    a := MatriceAleatoire(taille, taille, -10, 10);
    b := MatriceAleatoire(taille, taille, -10, 10);

    // Mesurer le temps
    debut := Now;
    c := MultiplicationMatrices(a, b);
    fin := Now;

    duree := MilliSecondsBetween(fin, debut);
    WriteLn(Format('Taille %d×%d: %d ms', [taille, taille, duree]));
  end;
end;
```

## Ressources et documentation

### Bibliothèques recommandées

**Pour FreePascal** :
- **NumLib** : Calcul numérique de base
- **LMath** : Bibliothèque mathématique complète
- **Math Unit** : Fonctions mathématiques standard (inclus)
- **BLAS/LAPACK** : Algèbre linéaire optimisée (bindings)

**Interopérabilité** :
- **Python + NumPy** : Via process ou bibliothèques
- **MATLAB/Octave** : Export/import de données
- **R** : Communication via fichiers ou pipes

### Lectures recommandées

**Livres** :
- "Linear Algebra and Its Applications" - Gilbert Strang
- "Numerical Linear Algebra" - Trefethen & Bau
- "Matrix Computations" - Golub & Van Loan

**Cours en ligne** :
- MIT OpenCourseWare : Linear Algebra
- Khan Academy : Algèbre linéaire
- 3Blue1Brown : Essence of Linear Algebra (YouTube)

### Documentation FreePascal

- Wiki FreePascal : Section Math
- Documentation Math Unit
- Forum Lazarus : Catégorie "Scientific Computing"

## Conclusion

L'algèbre linéaire est un outil puissant et polyvalent pour le calcul scientifique. Avec FreePascal, vous disposez de :

✓ **Performance native** : Code compilé efficace  
✓ **Contrôle précis** : Gestion fine de la mémoire et des calculs  
✓ **Portabilité** : Code identique sur Windows/Ubuntu  
✓ **Facilité d'utilisation** : Syntaxe claire et lisible  
✓ **Interopérabilité** : Intégration avec bibliothèques optimisées

### Points clés à retenir

1. **Vecteurs et matrices** : Structures de base pour calculs
2. **Opérations fondamentales** : Addition, multiplication, transposition
3. **Décompositions** : LU, QR, Cholesky, SVD
4. **Résolution de systèmes** : Méthodes directes et itératives
5. **Valeurs propres** : Analyse spectrale
6. **Applications** : Régression, PCA, transformations géométriques
7. **Optimisation** : SIMD, bibliothèques externes, algorithmes efficaces

### Prochaines étapes

- **Chapitre 16.5** : Statistiques avancées
- **Chapitre 20** : Optimisation et performance
- **Chapitre 15** : Intelligence artificielle (utilise l'algèbre linéaire)

L'algèbre linéaire est la fondation de nombreux domaines modernes : machine learning, graphisme 3D, traitement d'images, optimisation, et bien plus. Maîtriser ces concepts vous ouvre les portes de nombreuses applications passionnantes !

**Bon calcul matriciel avec FreePascal !** 📐🔢💻

⏭️ [Statistiques avancées](/16-traitement-donnees-calcul-scientifique/05-statistiques-avancees.md)
