🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 15.2 Réseaux de neurones from scratch

## Introduction

Dans cette section, nous allons construire un réseau de neurones **entièrement en FreePascal**, sans utiliser de bibliothèques externes comme TensorFlow. Cette approche "from scratch" (depuis zéro) vous permettra de comprendre en profondeur comment fonctionnent les réseaux de neurones.

### Pourquoi créer un réseau de neurones from scratch ?

**Avantages de cette approche :**

✅ **Compréhension profonde** : Vous comprendrez chaque mécanisme interne  
✅ **Contrôle total** : Aucune dépendance externe, code 100% Pascal  
✅ **Performance** : Code optimisé compilé natif  
✅ **Pédagogique** : Meilleure façon d'apprendre le deep learning  
✅ **Débogage** : Vous maîtrisez tout le code  
✅ **Portabilité** : Fonctionne partout où FreePascal fonctionne

**Cas d'usage appropriés :**
- Apprentissage et enseignement
- Prototypes simples
- Applications embarquées avec contraintes mémoire
- Systèmes critiques nécessitant un contrôle total
- Réseaux de neurones de petite/moyenne taille

**Quand utiliser des bibliothèques externes :**
- Deep learning complexe (CNN, RNN, Transformers)
- Très grands datasets
- Entraînement sur GPU absolument nécessaire
- Production avec modèles très complexes

## Concepts fondamentaux

### Qu'est-ce qu'un neurone artificiel ?

Un neurone artificiel est une unité de calcul simple qui :

1. **Reçoit** plusieurs entrées (x₁, x₂, x₃...)
2. **Multiplie** chaque entrée par un poids (w₁, w₂, w₃...)
3. **Additionne** tous les résultats + un biais (bias)
4. **Applique** une fonction d'activation
5. **Produit** une sortie

```
Schéma d'un neurone :

    x₁ ──→ [w₁] ─┐
    x₂ ──→ [w₂] ─┤
    x₃ ──→ [w₃] ─┼──→ Σ ──→ f(.) ──→ sortie
         [bias] ─┘

Où :
- x = entrées
- w = poids (weights)
- Σ = somme pondérée
- f(.) = fonction d'activation
```

**Équation mathématique :**

```
sortie = f(w₁×x₁ + w₂×x₂ + w₃×x₃ + bias)
```

### Architecture d'un réseau de neurones

Un réseau de neurones est organisé en **couches** :

```
Couche d'entrée → Couches cachées → Couche de sortie

Exemple : Reconnaissance de chiffres (0-9)
[784 entrées]  →  [128]  →  [64]  →  [10 sorties]
   (28×28)      cachée 1   cachée 2    (0 à 9)
```

**Types de couches :**
- **Couche d'entrée** : Reçoit les données brutes
- **Couches cachées** : Apprennent les représentations
- **Couche de sortie** : Produit la prédiction finale

## Implémentation des structures de données

### Type de base : Vecteur

```pascal
unit NeuralTypes;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Math;

type
  // Vecteur de nombres réels
  TVector = array of Double;

  // Matrice de nombres réels
  TMatrix = array of array of Double;

// Fonctions utilitaires pour les vecteurs
function CreateVector(Size: Integer): TVector;
function VectorLength(const V: TVector): Integer;
procedure FillVector(var V: TVector; Value: Double);
procedure RandomizeVector(var V: TVector; MinVal, MaxVal: Double);
function DotProduct(const A, B: TVector): Double;
procedure AddVectors(var Result: TVector; const A, B: TVector);
procedure ScaleVector(var V: TVector; Scale: Double);
function VectorToString(const V: TVector): string;

// Fonctions utilitaires pour les matrices
function CreateMatrix(Rows, Cols: Integer): TMatrix;
procedure FillMatrix(var M: TMatrix; Value: Double);
procedure RandomizeMatrix(var M: TMatrix; MinVal, MaxVal: Double);
function MatrixMultiply(const A: TMatrix; const B: TVector): TVector;
function MatrixToString(const M: TMatrix): string;

implementation

function CreateVector(Size: Integer): TVector;
begin
  SetLength(Result, Size);
end;

function VectorLength(const V: TVector): Integer;
begin
  Result := Length(V);
end;

procedure FillVector(var V: TVector; Value: Double);
var
  i: Integer;
begin
  for i := 0 to High(V) do
    V[i] := Value;
end;

procedure RandomizeVector(var V: TVector; MinVal, MaxVal: Double);
var
  i: Integer;
  Range: Double;
begin
  Range := MaxVal - MinVal;
  for i := 0 to High(V) do
    V[i] := MinVal + Random * Range;
end;

function DotProduct(const A, B: TVector): Double;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to High(A) do
    Result := Result + A[i] * B[i];
end;

procedure AddVectors(var Result: TVector; const A, B: TVector);
var
  i: Integer;
begin
  for i := 0 to High(A) do
    Result[i] := A[i] + B[i];
end;

procedure ScaleVector(var V: TVector; Scale: Double);
var
  i: Integer;
begin
  for i := 0 to High(V) do
    V[i] := V[i] * Scale;
end;

function VectorToString(const V: TVector): string;
var
  i: Integer;
begin
  Result := '[';
  for i := 0 to High(V) do
  begin
    Result := Result + Format('%.4f', [V[i]]);
    if i < High(V) then
      Result := Result + ', ';
  end;
  Result := Result + ']';
end;

function CreateMatrix(Rows, Cols: Integer): TMatrix;
var
  i: Integer;
begin
  SetLength(Result, Rows);
  for i := 0 to Rows - 1 do
    SetLength(Result[i], Cols);
end;

procedure FillMatrix(var M: TMatrix; Value: Double);
var
  i, j: Integer;
begin
  for i := 0 to High(M) do
    for j := 0 to High(M[i]) do
      M[i][j] := Value;
end;

procedure RandomizeMatrix(var M: TMatrix; MinVal, MaxVal: Double);
var
  i, j: Integer;
  Range: Double;
begin
  Range := MaxVal - MinVal;
  for i := 0 to High(M) do
    for j := 0 to High(M[i]) do
      M[i][j] := MinVal + Random * Range;
end;

function MatrixMultiply(const A: TMatrix; const B: TVector): TVector;
var
  i, j: Integer;
  Sum: Double;
begin
  SetLength(Result, Length(A));

  for i := 0 to High(A) do
  begin
    Sum := 0;
    for j := 0 to High(A[i]) do
      Sum := Sum + A[i][j] * B[j];
    Result[i] := Sum;
  end;
end;

function MatrixToString(const M: TMatrix): string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to High(M) do
  begin
    Result := Result + VectorToString(M[i]);
    if i < High(M) then
      Result := Result + sLineBreak;
  end;
end;

end.
```

### Fonctions d'activation

Les fonctions d'activation introduisent de la **non-linéarité** dans le réseau.

```pascal
unit Activations;

{$mode objfpc}{$H+}

interface

uses
  Math;

type
  // Type de fonction d'activation
  TActivationType = (atSigmoid, atTanh, atReLU, atLeakyReLU, atSoftmax);

// Fonctions d'activation
function Sigmoid(x: Double): Double;
function SigmoidDerivative(x: Double): Double;

function Tanh(x: Double): Double;
function TanhDerivative(x: Double): Double;

function ReLU(x: Double): Double;
function ReLUDerivative(x: Double): Double;

function LeakyReLU(x: Double): Double;
function LeakyReLUDerivative(x: Double): Double;

// Application sur des vecteurs
procedure ApplyActivation(var V: array of Double; ActivationType: TActivationType);
procedure ApplyActivationDerivative(var V: array of Double; ActivationType: TActivationType);

implementation

// Sigmoid : sortie entre 0 et 1
function Sigmoid(x: Double): Double;
begin
  Result := 1.0 / (1.0 + Exp(-x));
end;

function SigmoidDerivative(x: Double): Double;
var
  s: Double;
begin
  s := Sigmoid(x);
  Result := s * (1.0 - s);
end;

// Tanh : sortie entre -1 et 1
function Tanh(x: Double): Double;
begin
  Result := (Exp(x) - Exp(-x)) / (Exp(x) + Exp(-x));
end;

function TanhDerivative(x: Double): Double;
var
  t: Double;
begin
  t := Tanh(x);
  Result := 1.0 - t * t;
end;

// ReLU : Max(0, x) - Très populaire
function ReLU(x: Double): Double;
begin
  if x > 0 then
    Result := x
  else
    Result := 0;
end;

function ReLUDerivative(x: Double): Double;
begin
  if x > 0 then
    Result := 1.0
  else
    Result := 0.0;
end;

// Leaky ReLU : Permet un petit gradient pour x < 0
function LeakyReLU(x: Double): Double;
const
  Alpha = 0.01;
begin
  if x > 0 then
    Result := x
  else
    Result := Alpha * x;
end;

function LeakyReLUDerivative(x: Double): Double;
const
  Alpha = 0.01;
begin
  if x > 0 then
    Result := 1.0
  else
    Result := Alpha;
end;

procedure ApplyActivation(var V: array of Double; ActivationType: TActivationType);
var
  i: Integer;
begin
  case ActivationType of
    atSigmoid:
      for i := 0 to High(V) do
        V[i] := Sigmoid(V[i]);

    atTanh:
      for i := 0 to High(V) do
        V[i] := Tanh(V[i]);

    atReLU:
      for i := 0 to High(V) do
        V[i] := ReLU(V[i]);

    atLeakyReLU:
      for i := 0 to High(V) do
        V[i] := LeakyReLU(V[i]);
  end;
end;

procedure ApplyActivationDerivative(var V: array of Double; ActivationType: TActivationType);
var
  i: Integer;
begin
  case ActivationType of
    atSigmoid:
      for i := 0 to High(V) do
        V[i] := SigmoidDerivative(V[i]);

    atTanh:
      for i := 0 to High(V) do
        V[i] := TanhDerivative(V[i]);

    atReLU:
      for i := 0 to High(V) do
        V[i] := ReLUDerivative(V[i]);

    atLeakyReLU:
      for i := 0 to High(V) do
        V[i] := LeakyReLUDerivative(V[i]);
  end;
end;

end.
```

**Visualisation des fonctions d'activation :**

```
Sigmoid:          ReLU:             Leaky ReLU:
    1 |    ╱─        |    ╱          |    ╱
      |   ╱          |   ╱           |   ╱
  0.5 | ─             | ─             | ─
      | ╱             |               |╱
    0 |───────       0|───────       0|───────
     -5  0  5         -5  0  5        -5  0  5
```

## Implémentation d'une couche de neurones

```pascal
unit NeuralLayer;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Math, NeuralTypes, Activations;

type
  TLayer = class
  private
    FInputSize: Integer;
    FOutputSize: Integer;
    FWeights: TMatrix;        // Poids [OutputSize × InputSize]
    FBiases: TVector;         // Biais [OutputSize]
    FActivationType: TActivationType;

    // Pour la rétropropagation
    FLastInput: TVector;
    FLastOutput: TVector;
    FLastWeightedSum: TVector;

  public
    constructor Create(InputSize, OutputSize: Integer;
                      ActivationType: TActivationType);
    destructor Destroy; override;

    procedure Initialize;
    function Forward(const Input: TVector): TVector;
    procedure Backward(const OutputGradient: TVector; LearningRate: Double;
                      out InputGradient: TVector);

    procedure SaveToFile(const FileName: string);
    procedure LoadFromFile(const FileName: string);

    property InputSize: Integer read FInputSize;
    property OutputSize: Integer read FOutputSize;
    property Weights: TMatrix read FWeights;
    property Biases: TVector read FBiases;
  end;

implementation

constructor TLayer.Create(InputSize, OutputSize: Integer;
                         ActivationType: TActivationType);
begin
  inherited Create;

  FInputSize := InputSize;
  FOutputSize := OutputSize;
  FActivationType := ActivationType;

  // Allouer les matrices et vecteurs
  FWeights := CreateMatrix(OutputSize, InputSize);
  SetLength(FBiases, OutputSize);

  SetLength(FLastInput, InputSize);
  SetLength(FLastOutput, OutputSize);
  SetLength(FLastWeightedSum, OutputSize);

  Initialize;
end;

destructor TLayer.Destroy;
begin
  inherited Destroy;
end;

procedure TLayer.Initialize;
var
  Range: Double;
begin
  // Initialisation Xavier/Glorot
  // Les poids sont initialisés dans [-sqrt(6/(n_in+n_out)), sqrt(6/(n_in+n_out))]
  Range := Sqrt(6.0 / (FInputSize + FOutputSize));

  RandomizeMatrix(FWeights, -Range, Range);
  FillVector(FBiases, 0.0);
end;

function TLayer.Forward(const Input: TVector): TVector;
var
  i, j: Integer;
  Sum: Double;
begin
  // Sauvegarder l'entrée pour la rétropropagation
  FLastInput := Copy(Input);

  SetLength(Result, FOutputSize);

  // Calculer la somme pondérée : z = W×x + b
  for i := 0 to FOutputSize - 1 do
  begin
    Sum := FBiases[i];
    for j := 0 to FInputSize - 1 do
      Sum := Sum + FWeights[i][j] * Input[j];

    FLastWeightedSum[i] := Sum;
    Result[i] := Sum;
  end;

  // Appliquer la fonction d'activation
  ApplyActivation(Result, FActivationType);

  FLastOutput := Copy(Result);
end;

procedure TLayer.Backward(const OutputGradient: TVector; LearningRate: Double;
                         out InputGradient: TVector);
var
  i, j: Integer;
  ActivationGradient: TVector;
  WeightGradient: TMatrix;
  BiasGradient: TVector;
begin
  // 1. Calculer le gradient de l'activation
  ActivationGradient := Copy(FLastWeightedSum);
  ApplyActivationDerivative(ActivationGradient, FActivationType);

  // Multiplier par le gradient de sortie
  for i := 0 to High(ActivationGradient) do
    ActivationGradient[i] := ActivationGradient[i] * OutputGradient[i];

  // 2. Calculer le gradient des poids
  WeightGradient := CreateMatrix(FOutputSize, FInputSize);

  for i := 0 to FOutputSize - 1 do
    for j := 0 to FInputSize - 1 do
      WeightGradient[i][j] := ActivationGradient[i] * FLastInput[j];

  // 3. Calculer le gradient des biais
  BiasGradient := Copy(ActivationGradient);

  // 4. Calculer le gradient d'entrée (pour la couche précédente)
  SetLength(InputGradient, FInputSize);
  FillVector(InputGradient, 0.0);

  for j := 0 to FInputSize - 1 do
    for i := 0 to FOutputSize - 1 do
      InputGradient[j] := InputGradient[j] +
        FWeights[i][j] * ActivationGradient[i];

  // 5. Mettre à jour les poids et biais (descente de gradient)
  for i := 0 to FOutputSize - 1 do
  begin
    for j := 0 to FInputSize - 1 do
      FWeights[i][j] := FWeights[i][j] -
        LearningRate * WeightGradient[i][j];

    FBiases[i] := FBiases[i] - LearningRate * BiasGradient[i];
  end;
end;

procedure TLayer.SaveToFile(const FileName: string);
var
  F: TextFile;
  i, j: Integer;
begin
  AssignFile(F, FileName);
  Rewrite(F);
  try
    // Sauvegarder les dimensions
    WriteLn(F, FInputSize, ' ', FOutputSize);

    // Sauvegarder les poids
    for i := 0 to FOutputSize - 1 do
    begin
      for j := 0 to FInputSize - 1 do
        Write(F, FWeights[i][j]:0:8, ' ');
      WriteLn(F);
    end;

    // Sauvegarder les biais
    for i := 0 to FOutputSize - 1 do
      Write(F, FBiases[i]:0:8, ' ');
    WriteLn(F);

  finally
    CloseFile(F);
  end;
end;

procedure TLayer.LoadFromFile(const FileName: string);
var
  F: TextFile;
  i, j: Integer;
  InputSize, OutputSize: Integer;
begin
  AssignFile(F, FileName);
  Reset(F);
  try
    // Charger les dimensions
    ReadLn(F, InputSize, OutputSize);

    if (InputSize <> FInputSize) or (OutputSize <> FOutputSize) then
      raise Exception.Create('Dimensions incompatibles');

    // Charger les poids
    for i := 0 to FOutputSize - 1 do
      for j := 0 to FInputSize - 1 do
        Read(F, FWeights[i][j]);

    // Charger les biais
    for i := 0 to FOutputSize - 1 do
      Read(F, FBiases[i]);

  finally
    CloseFile(F);
  end;
end;

end.
```

## Implémentation du réseau de neurones complet

```pascal
unit NeuralNetwork;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, NeuralTypes, NeuralLayer, Activations;

type
  TNeuralNetwork = class
  private
    FLayers: array of TLayer;
    FLearningRate: Double;
    FEpochs: Integer;

    function MeanSquaredError(const Predicted, Target: TVector): Double;
    function Accuracy(const Predicted, Target: TVector): Boolean;

  public
    constructor Create;
    destructor Destroy; override;

    procedure AddLayer(InputSize, OutputSize: Integer;
                      ActivationType: TActivationType);

    function Predict(const Input: TVector): TVector;
    procedure Train(const Inputs, Targets: array of TVector;
                   Epochs: Integer; LearningRate: Double);

    procedure SaveToFile(const FileName: string);
    procedure LoadFromFile(const FileName: string);

    property LearningRate: Double read FLearningRate write FLearningRate;
  end;

implementation

constructor TNeuralNetwork.Create;
begin
  inherited Create;
  SetLength(FLayers, 0);
  FLearningRate := 0.01;
  FEpochs := 100;
end;

destructor TNeuralNetwork.Destroy;
var
  i: Integer;
begin
  for i := 0 to High(FLayers) do
    FLayers[i].Free;

  inherited Destroy;
end;

procedure TNeuralNetwork.AddLayer(InputSize, OutputSize: Integer;
                                  ActivationType: TActivationType);
var
  NewLayer: TLayer;
  Len: Integer;
begin
  NewLayer := TLayer.Create(InputSize, OutputSize, ActivationType);

  Len := Length(FLayers);
  SetLength(FLayers, Len + 1);
  FLayers[Len] := NewLayer;
end;

function TNeuralNetwork.Predict(const Input: TVector): TVector;
var
  i: Integer;
  Current: TVector;
begin
  if Length(FLayers) = 0 then
    raise Exception.Create('Aucune couche dans le réseau');

  Current := Copy(Input);

  // Propagation avant à travers toutes les couches
  for i := 0 to High(FLayers) do
    Current := FLayers[i].Forward(Current);

  Result := Current;
end;

function TNeuralNetwork.MeanSquaredError(const Predicted, Target: TVector): Double;
var
  i: Integer;
  Sum: Double;
begin
  Sum := 0;
  for i := 0 to High(Predicted) do
    Sum := Sum + Sqr(Predicted[i] - Target[i]);

  Result := Sum / Length(Predicted);
end;

function TNeuralNetwork.Accuracy(const Predicted, Target: TVector): Boolean;
var
  PredictedClass, TargetClass: Integer;
  i: Integer;
  MaxVal: Double;
begin
  // Trouver la classe prédite (index du max)
  MaxVal := Predicted[0];
  PredictedClass := 0;
  for i := 1 to High(Predicted) do
  begin
    if Predicted[i] > MaxVal then
    begin
      MaxVal := Predicted[i];
      PredictedClass := i;
    end;
  end;

  // Trouver la vraie classe
  MaxVal := Target[0];
  TargetClass := 0;
  for i := 1 to High(Target) do
  begin
    if Target[i] > MaxVal then
    begin
      MaxVal := Target[i];
      TargetClass := i;
    end;
  end;

  Result := (PredictedClass = TargetClass);
end;

procedure TNeuralNetwork.Train(const Inputs, Targets: array of TVector;
                              Epochs: Integer; LearningRate: Double);
var
  epoch, i, j: Integer;
  Predicted: TVector;
  Error: Double;
  TotalError: Double;
  OutputGradient: TVector;
  CurrentGradient: TVector;
  CorrectCount: Integer;
begin
  FLearningRate := LearningRate;

  WriteLn('=== Début de l''entraînement ===');
  WriteLn(Format('Exemples: %d, Époques: %d, Taux d''apprentissage: %.4f',
    [Length(Inputs), Epochs, LearningRate]));
  WriteLn('');

  for epoch := 1 to Epochs do
  begin
    TotalError := 0;
    CorrectCount := 0;

    // Pour chaque exemple d'entraînement
    for i := 0 to High(Inputs) do
    begin
      // 1. Propagation avant
      Predicted := Predict(Inputs[i]);

      // 2. Calculer l'erreur
      Error := MeanSquaredError(Predicted, Targets[i]);
      TotalError := TotalError + Error;

      // 3. Calculer la précision
      if Accuracy(Predicted, Targets[i]) then
        Inc(CorrectCount);

      // 4. Calculer le gradient de sortie
      SetLength(OutputGradient, Length(Predicted));
      for j := 0 to High(Predicted) do
        OutputGradient[j] := 2 * (Predicted[j] - Targets[i][j]) / Length(Predicted);

      // 5. Rétropropagation à travers toutes les couches
      CurrentGradient := OutputGradient;

      for j := High(FLayers) downto 0 do
      begin
        FLayers[j].Backward(CurrentGradient, LearningRate, CurrentGradient);
      end;
    end;

    // Afficher les statistiques tous les 10 époques
    if (epoch mod 10 = 0) or (epoch = 1) then
    begin
      WriteLn(Format('Époque %d/%d - Erreur: %.6f - Précision: %.2f%%',
        [epoch, Epochs, TotalError / Length(Inputs),
         (CorrectCount / Length(Inputs)) * 100]));
    end;
  end;

  WriteLn('');
  WriteLn('=== Entraînement terminé ===');
end;

procedure TNeuralNetwork.SaveToFile(const FileName: string);
var
  F: TextFile;
  i: Integer;
begin
  AssignFile(F, FileName);
  Rewrite(F);
  try
    // Sauvegarder le nombre de couches
    WriteLn(F, Length(FLayers));

    // Sauvegarder chaque couche
    for i := 0 to High(FLayers) do
    begin
      FLayers[i].SaveToFile(FileName + Format('.layer%d', [i]));
    end;

  finally
    CloseFile(F);
  end;
end;

procedure TNeuralNetwork.LoadFromFile(const FileName: string);
var
  F: TextFile;
  LayerCount, i: Integer;
begin
  AssignFile(F, FileName);
  Reset(F);
  try
    // Charger le nombre de couches
    ReadLn(F, LayerCount);

    // Charger chaque couche
    for i := 0 to LayerCount - 1 do
    begin
      FLayers[i].LoadFromFile(FileName + Format('.layer%d', [i]));
    end;

  finally
    CloseFile(F);
  end;
end;

end.
```

## Exemple complet : Classification XOR

Le problème XOR est un classique pour tester les réseaux de neurones. C'est un problème non-linéaire qui nécessite au moins une couche cachée.

```pascal
program XORExample;

{$mode objfpc}{$H+}

uses
  SysUtils, NeuralNetwork, NeuralTypes, Activations;

var
  Network: TNeuralNetwork;
  Inputs, Targets: array of TVector;
  TestInput, Output: TVector;
  i: Integer;

begin
  WriteLn('=== Réseau de neurones : Problème XOR ===');
  WriteLn('');

  // Initialiser le générateur aléatoire
  Randomize;

  // Créer le réseau : 2 entrées → 4 cachés → 1 sortie
  Network := TNeuralNetwork.Create;
  try
    Network.AddLayer(2, 4, atReLU);      // Couche cachée avec ReLU
    Network.AddLayer(4, 1, atSigmoid);   // Couche de sortie avec Sigmoid

    WriteLn('Architecture du réseau :');
    WriteLn('  Entrée: 2 neurones');
    WriteLn('  Cachée: 4 neurones (ReLU)');
    WriteLn('  Sortie: 1 neurone (Sigmoid)');
    WriteLn('');

    // Préparer les données d'entraînement XOR
    SetLength(Inputs, 4);
    SetLength(Targets, 4);

    // XOR truth table
    // 0 XOR 0 = 0
    SetLength(Inputs[0], 2);
    Inputs[0][0] := 0; Inputs[0][1] := 0;
    SetLength(Targets[0], 1);
    Targets[0][0] := 0;

    // 0 XOR 1 = 1
    SetLength(Inputs[1], 2);
    Inputs[1][0] := 0; Inputs[1][1] := 1;
    SetLength(Targets[1], 1);
    Targets[1][0] := 1;

    // 1 XOR 0 = 1
    SetLength(Inputs[2], 2);
    Inputs[2][0] := 1; Inputs[2][1] := 0;
    SetLength(Targets[2], 1);
    Targets[2][0] := 1;

    // 1 XOR 1 = 0
    SetLength(Inputs[3], 2);
    Inputs[3][0] := 1; Inputs[3][1] := 1;
    SetLength(Targets[3], 1);
    Targets[3][0] := 0;

    WriteLn('Données d''entraînement (table XOR) :');
    WriteLn('  0 XOR 0 = 0');
    WriteLn('  0 XOR 1 = 1');
    WriteLn('  1 XOR 0 = 1');
    WriteLn('  1 XOR 1 = 0');
    WriteLn('');

    // Entraîner le réseau
    Network.Train(Inputs, Targets, 1000, 0.1);

    // Tester le réseau
    WriteLn('');
    WriteLn('=== Test du réseau entraîné ===');

    for i := 0 to High(Inputs) do
    begin
      Output := Network.Predict(Inputs[i]);
      WriteLn(Format('%.0f XOR %.0f = %.4f (attendu: %.0f)',
        [Inputs[i][0], Inputs[i][1], Output[0], Targets[i][0]]));
    end;

    // Sauvegarder le modèle
    Network.SaveToFile('xor_model.dat');
    WriteLn('');
    WriteLn('Modèle sauvegardé dans : xor_model.dat');

  finally
    Network.Free;
  end;

  ReadLn;
end.
```

**Sortie attendue :**

```
=== Réseau de neurones : Problème XOR ===

Architecture du réseau :
  Entrée: 2 neurones
  Cachée: 4 neurones (ReLU)
  Sortie: 1 neurone (Sigmoid)

Données d'entraînement (table XOR) :
  0 XOR 0 = 0
  0 XOR 1 = 1
  1 XOR 0 = 1
  1 XOR 1 = 0

=== Début de l'entraînement ===
Exemples: 4, Époques: 1000, Taux d'apprentissage: 0.1000

Époque 1/1000 - Erreur: 0.250123 - Précision: 25.00%
Époque 10/1000 - Erreur: 0.241567 - Précision: 50.00%
Époque 20/1000 - Erreur: 0.228945 - Précision: 50.00%
...
Époque 990/1000 - Erreur: 0.001234 - Précision: 100.00%
Époque 1000/1000 - Erreur: 0.001156 - Précision: 100.00%

=== Entraînement terminé ===

=== Test du réseau entraîné ===
0 XOR 0 = 0.0123 (attendu: 0)
0 XOR 1 = 0.9876 (attendu: 1)
1 XOR 0 = 0.9845 (attendu: 1)
1 XOR 1 = 0.0234 (attendu: 0)

Modèle sauvegardé dans : xor_model.dat
```

## Application avec interface Lazarus

Créons une application graphique pour visualiser l'apprentissage.

```pascal
unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Grids, ComCtrls, NeuralNetwork, NeuralTypes, Activations;

type
  TFormMain = class(TForm)
    ButtonCreate: TButton;
    ButtonTrain: TButton;
    ButtonTest: TButton;
    ButtonSave: TButton;
    ButtonLoad: TButton;

    EditInputSize: TEdit;
    EditHiddenSize: TEdit;
    EditOutputSize: TEdit;
    EditEpochs: TEdit;
    EditLearningRate: TEdit;

    LabelArchitecture: TLabel;
    LabelTrainingProgress: TLabel;

    MemoLog: TMemo;
    ProgressBarTraining: TProgressBar;

    PaintBoxVisualization: TPaintBox;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonCreateClick(Sender: TObject);
    procedure ButtonTrainClick(Sender: TObject);
    procedure ButtonTestClick(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
    procedure ButtonLoadClick(Sender: TObject);
    procedure PaintBoxVisualizationPaint(Sender: TObject);

  private
    FNetwork: TNeuralNetwork;
    FTrainingData: array of TVector;
    FTargetData: array of TVector;

    procedure Log(const Msg: string);
    procedure CreateXORDataset;
    procedure DrawNetwork;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FNetwork := nil;

  // Valeurs par défaut
  EditInputSize.Text := '2';
  EditHiddenSize.Text := '4';
  EditOutputSize.Text := '1';
  EditEpochs.Text := '1000';
  EditLearningRate.Text := '0.1';

  Log('Application de réseau de neurones from scratch');
  Log('Prêt à créer un réseau');
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  if FNetwork <> nil then
    FNetwork.Free;
end;

procedure TFormMain.Log(const Msg: string);
begin
  MemoLog.Lines.Add('[' + TimeToStr(Now) + '] ' + Msg);
end;

procedure TFormMain.ButtonCreateClick(Sender: TObject);
var
  InputSize, HiddenSize, OutputSize: Integer;
begin
  if FNetwork <> nil then
    FreeAndNil(FNetwork);

  InputSize := StrToInt(EditInputSize.Text);
  HiddenSize := StrToInt(EditHiddenSize.Text);
  OutputSize := StrToInt(EditOutputSize.Text);

  FNetwork := TNeuralNetwork.Create;
  FNetwork.AddLayer(InputSize, HiddenSize, atReLU);
  FNetwork.AddLayer(HiddenSize, OutputSize, atSigmoid);

  LabelArchitecture.Caption := Format('Architecture : %d → %d → %d',
    [InputSize, HiddenSize, OutputSize]);

  Log(Format('Réseau créé : %d-%d-%d', [InputSize, HiddenSize, OutputSize]));

  ButtonTrain.Enabled := True;
  PaintBoxVisualization.Invalidate;
end;

procedure TFormMain.CreateXORDataset;
begin
  SetLength(FTrainingData, 4);
  SetLength(FTargetData, 4);

  // 0 XOR 0 = 0
  SetLength(FTrainingData[0], 2);
  FTrainingData[0][0] := 0; FTrainingData[0][1] := 0;
  SetLength(FTargetData[0], 1);
  FTargetData[0][0] := 0;

  // 0 XOR 1 = 1
  SetLength(FTrainingData[1], 2);
  FTrainingData[1][0] := 0; FTrainingData[1][1] := 1;
  SetLength(FTargetData[1], 1);
  FTargetData[1][0] := 1;

  // 1 XOR 0 = 1
  SetLength(FTrainingData[2], 2);
  FTrainingData[2][0] := 1; FTrainingData[2][1] := 0;
  SetLength(FTargetData[2], 1);
  FTargetData[2][0] := 1;

  // 1 XOR 1 = 0
  SetLength(FTrainingData[3], 2);
  FTrainingData[3][0] := 1; FTrainingData[3][1] := 1;
  SetLength(FTargetData[3], 1);
  FTargetData[3][0] := 0;
end;

procedure TFormMain.ButtonTrainClick(Sender: TObject);
var
  Epochs: Integer;
  LearningRate: Double;
begin
  if FNetwork = nil then
  begin
    ShowMessage('Veuillez d''abord créer un réseau !');
    Exit;
  end;

  // Créer le dataset XOR
  CreateXORDataset;

  Epochs := StrToInt(EditEpochs.Text);
  LearningRate := StrToFloat(EditLearningRate.Text);

  Log('Début de l''entraînement...');
  Log(Format('Époques : %d, Taux : %.4f', [Epochs, LearningRate]));

  ButtonTrain.Enabled := False;
  ProgressBarTraining.Position := 0;
  ProgressBarTraining.Max := Epochs;

  Application.ProcessMessages;

  try
    // Entraîner (version modifiée pour mettre à jour la barre)
    FNetwork.Train(FTrainingData, FTargetData, Epochs, LearningRate);

    ProgressBarTraining.Position := Epochs;

    Log('Entraînement terminé !');
    ButtonTest.Enabled := True;

  finally
    ButtonTrain.Enabled := True;
  end;
end;

procedure TFormMain.ButtonTestClick(Sender: TObject);
var
  i: Integer;
  Output: TVector;
begin
  if FNetwork = nil then
  begin
    ShowMessage('Veuillez d''abord créer et entraîner un réseau !');
    Exit;
  end;

  Log('');
  Log('=== Tests ===');

  for i := 0 to High(FTrainingData) do
  begin
    Output := FNetwork.Predict(FTrainingData[i]);

    Log(Format('%.0f XOR %.0f = %.4f (attendu: %.0f)',
      [FTrainingData[i][0], FTrainingData[i][1],
       Output[0], FTargetData[i][0]]));
  end;
end;

procedure TFormMain.ButtonSaveClick(Sender: TObject);
var
  SaveDialog: TSaveDialog;
begin
  if FNetwork = nil then
  begin
    ShowMessage('Aucun réseau à sauvegarder !');
    Exit;
  end;

  SaveDialog := TSaveDialog.Create(nil);
  try
    SaveDialog.Filter := 'Modèle NN|*.nn|Tous|*.*';
    SaveDialog.DefaultExt := 'nn';

    if SaveDialog.Execute then
    begin
      FNetwork.SaveToFile(SaveDialog.FileName);
      Log('Modèle sauvegardé : ' + SaveDialog.FileName);
    end;
  finally
    SaveDialog.Free;
  end;
end;

procedure TFormMain.ButtonLoadClick(Sender: TObject);
var
  OpenDialog: TOpenDialog;
begin
  OpenDialog := TOpenDialog.Create(nil);
  try
    OpenDialog.Filter := 'Modèle NN|*.nn|Tous|*.*';

    if OpenDialog.Execute then
    begin
      if FNetwork <> nil then
        FreeAndNil(FNetwork);

      FNetwork := TNeuralNetwork.Create;
      FNetwork.LoadFromFile(OpenDialog.FileName);

      Log('Modèle chargé : ' + OpenDialog.FileName);
      ButtonTest.Enabled := True;
      PaintBoxVisualization.Invalidate;
    end;
  finally
    OpenDialog.Free;
  end;
end;

procedure TFormMain.DrawNetwork;
var
  X, Y, LayerSpacing, NodeSpacing: Integer;
  Layer, Node: Integer;
  NodeX, NodeY: array of array of Integer;
  i, j: Integer;
begin
  if FNetwork = nil then
    Exit;

  with PaintBoxVisualization.Canvas do
  begin
    // Effacer
    Brush.Color := clWhite;
    FillRect(0, 0, PaintBoxVisualization.Width, PaintBoxVisualization.Height);

    // Dessiner une visualisation simple du réseau
    // (implémentation simplifiée pour l'exemple)

    Pen.Color := clBlack;
    Brush.Color := clLightBlue;

    // Dessiner les neurones de chaque couche
    X := 50;
    Y := 100;

    // Couche d'entrée
    Ellipse(X - 10, Y - 10, X + 10, Y + 10);
    TextOut(X - 20, Y + 15, 'In');

    // Couche cachée
    X := 150;
    Ellipse(X - 10, Y - 10, X + 10, Y + 10);
    TextOut(X - 25, Y + 15, 'Hidden');

    // Couche de sortie
    X := 250;
    Ellipse(X - 10, Y - 10, X + 10, Y + 10);
    TextOut(X - 20, Y + 15, 'Out');

    // Dessiner les connexions
    Pen.Color := clGray;
    MoveTo(60, 100);
    LineTo(140, 100);
    MoveTo(160, 100);
    LineTo(240, 100);
  end;
end;

procedure TFormMain.PaintBoxVisualizationPaint(Sender: TObject);
begin
  DrawNetwork;
end;

end.
```

## Exemple avancé : Classification de chiffres (MNIST simplifié)

Pour des données plus complexes comme des images :

```pascal
unit MNISTClassifier;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, NeuralNetwork, NeuralTypes, Activations;

type
  TMNISTClassifier = class
  private
    FNetwork: TNeuralNetwork;

    function ImageToVector(Image: TBitmap): TVector;
    function LabelToOneHot(LabelIndex: Integer): TVector;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Train(Images: array of TBitmap; Labels: array of Integer);
    function Classify(Image: TBitmap): Integer;

    procedure SaveModel(const FileName: string);
    procedure LoadModel(const FileName: string);
  end;

implementation

constructor TMNISTClassifier.Create;
begin
  inherited Create;

  // Réseau pour MNIST : 784 entrées (28×28) → 128 → 64 → 10 sorties
  FNetwork := TNeuralNetwork.Create;
  FNetwork.AddLayer(784, 128, atReLU);
  FNetwork.AddLayer(128, 64, atReLU);
  FNetwork.AddLayer(64, 10, atSigmoid);
end;

destructor TMNISTClassifier.Destroy;
begin
  FNetwork.Free;
  inherited Destroy;
end;

function TMNISTClassifier.ImageToVector(Image: TBitmap): TVector;
var
  x, y, idx: Integer;
  PixelColor: TColor;
  Grayscale: Byte;
begin
  // Convertir une image 28×28 en vecteur de 784 valeurs
  SetLength(Result, 784);
  idx := 0;

  for y := 0 to 27 do
  begin
    for x := 0 to 27 do
    begin
      PixelColor := Image.Canvas.Pixels[x, y];

      // Convertir en niveaux de gris
      Grayscale := (Red(PixelColor) + Green(PixelColor) + Blue(PixelColor)) div 3;

      // Normaliser entre 0 et 1
      Result[idx] := Grayscale / 255.0;
      Inc(idx);
    end;
  end;
end;

function TMNISTClassifier.LabelToOneHot(LabelIndex: Integer): TVector;
var
  i: Integer;
begin
  // Convertir un label (0-9) en vecteur one-hot
  SetLength(Result, 10);

  for i := 0 to 9 do
    Result[i] := 0;

  Result[LabelIndex] := 1;
end;

procedure TMNISTClassifier.Train(Images: array of TBitmap;
                                 Labels: array of Integer);
var
  Inputs, Targets: array of TVector;
  i: Integer;
begin
  SetLength(Inputs, Length(Images));
  SetLength(Targets, Length(Images));

  // Préparer les données
  for i := 0 to High(Images) do
  begin
    Inputs[i] := ImageToVector(Images[i]);
    Targets[i] := LabelToOneHot(Labels[i]);
  end;

  // Entraîner
  FNetwork.Train(Inputs, Targets, 100, 0.01);
end;

function TMNISTClassifier.Classify(Image: TBitmap): Integer;
var
  Input, Output: TVector;
  i: Integer;
  MaxVal: Double;
begin
  // Convertir l'image en vecteur
  Input := ImageToVector(Image);

  // Prédiction
  Output := FNetwork.Predict(Input);

  // Trouver la classe avec la plus haute probabilité
  Result := 0;
  MaxVal := Output[0];

  for i := 1 to 9 do
  begin
    if Output[i] > MaxVal then
    begin
      MaxVal := Output[i];
      Result := i;
    end;
  end;
end;

procedure TMNISTClassifier.SaveModel(const FileName: string);
begin
  FNetwork.SaveToFile(FileName);
end;

procedure TMNISTClassifier.LoadModel(const FileName: string);
begin
  FNetwork.LoadFromFile(FileName);
end;

end.
```

## Optimisations et améliorations

### 1. Mini-batch gradient descent

Au lieu de mettre à jour après chaque exemple, traiter par lots :

```pascal
procedure TNeuralNetwork.TrainMiniBatch(const Inputs, Targets: array of TVector;
                                        Epochs, BatchSize: Integer;
                                        LearningRate: Double);
var
  epoch, i, batchStart: Integer;
  BatchInputs, BatchTargets: array of TVector;
  BatchCount: Integer;
begin
  for epoch := 1 to Epochs do
  begin
    batchStart := 0;

    while batchStart < Length(Inputs) do
    begin
      // Préparer le mini-batch
      BatchCount := Min(BatchSize, Length(Inputs) - batchStart);
      SetLength(BatchInputs, BatchCount);
      SetLength(BatchTargets, BatchCount);

      for i := 0 to BatchCount - 1 do
      begin
        BatchInputs[i] := Inputs[batchStart + i];
        BatchTargets[i] := Targets[batchStart + i];
      end;

      // Entraîner sur ce batch
      TrainBatch(BatchInputs, BatchTargets, LearningRate);

      Inc(batchStart, BatchSize);
    end;
  end;
end;
```

### 2. Régularisation L2

Pour éviter le surapprentissage :

```pascal
procedure TLayer.BackwardWithRegularization(const OutputGradient: TVector;
                                           LearningRate, Lambda: Double;
                                           out InputGradient: TVector);
var
  i, j: Integer;
  RegTerm: Double;
begin
  // Calcul normal du gradient...

  // Ajouter le terme de régularisation L2
  for i := 0 to FOutputSize - 1 do
  begin
    for j := 0 to FInputSize - 1 do
    begin
      RegTerm := Lambda * FWeights[i][j];
      FWeights[i][j] := FWeights[i][j] -
        LearningRate * (WeightGradient[i][j] + RegTerm);
    end;
  end;
end;
```

### 3. Dropout

Désactiver aléatoirement des neurones pendant l'entraînement :

```pascal
procedure TLayer.ForwardWithDropout(const Input: TVector;
                                   DropoutRate: Double): TVector;
var
  i: Integer;
  Mask: array of Boolean;
begin
  Result := Forward(Input);

  // Créer un masque de dropout
  SetLength(Mask, FOutputSize);
  for i := 0 to FOutputSize - 1 do
  begin
    Mask[i] := Random > DropoutRate;
    if not Mask[i] then
      Result[i] := 0;
  end;
end;
```

### 4. Taux d'apprentissage adaptatif (Adam)

```pascal
type
  TAdamOptimizer = class
  private
    FBeta1, FBeta2, FEpsilon: Double;
    FMomentum, FVelocity: TMatrix;
    FTimeStep: Integer;

  public
    constructor Create(Beta1, Beta2, Epsilon: Double);
    procedure UpdateWeights(var Weights: TMatrix; const Gradients: TMatrix;
                           LearningRate: Double);
  end;

procedure TAdamOptimizer.UpdateWeights(var Weights: TMatrix;
                                      const Gradients: TMatrix;
                                      LearningRate: Double);
var
  i, j: Integer;
  MHat, VHat: Double;
begin
  Inc(FTimeStep);

  for i := 0 to High(Weights) do
  begin
    for j := 0 to High(Weights[i]) do
    begin
      // Mettre à jour le momentum
      FMomentum[i][j] := FBeta1 * FMomentum[i][j] +
                        (1 - FBeta1) * Gradients[i][j];

      // Mettre à jour la vélocité
      FVelocity[i][j] := FBeta2 * FVelocity[i][j] +
                        (1 - FBeta2) * Sqr(Gradients[i][j]);

      // Correction de biais
      MHat := FMomentum[i][j] / (1 - Power(FBeta1, FTimeStep));
      VHat := FVelocity[i][j] / (1 - Power(FBeta2, FTimeStep));

      // Mise à jour des poids
      Weights[i][j] := Weights[i][j] -
        LearningRate * MHat / (Sqrt(VHat) + FEpsilon);
    end;
  end;
end;
```

### 5. Normalisation par batch (Batch Normalization)

```pascal
type
  TBatchNormLayer = class
  private
    FMean, FVariance: TVector;
    FGamma, FBeta: TVector;  // Paramètres apprenables
    FEpsilon: Double;

  public
    function Forward(const Input: TVector): TVector;
    procedure Backward(const OutputGradient: TVector;
                      out InputGradient: TVector);
  end;

function TBatchNormLayer.Forward(const Input: TVector): TVector;
var
  i: Integer;
  Normalized: Double;
begin
  SetLength(Result, Length(Input));

  for i := 0 to High(Input) do
  begin
    // Normaliser
    Normalized := (Input[i] - FMean[i]) / Sqrt(FVariance[i] + FEpsilon);

    // Appliquer gamma et beta
    Result[i] := FGamma[i] * Normalized + FBeta[i];
  end;
end;
```

## Visualisation de l'apprentissage

```pascal
procedure TFormMain.VisualizeTraining;
var
  Chart: TChart;
  Series: TLineSeries;
  epoch: Integer;
  Error: Double;
begin
  Chart := TChart.Create(Self);
  Series := TLineSeries.Create(Chart);

  Chart.Parent := Self;
  Chart.AddSeries(Series);

  for epoch := 1 to 100 do
  begin
    // Entraîner une époque
    Error := TrainOneEpoch();

    // Ajouter au graphique
    Series.AddXY(epoch, Error);

    // Rafraîchir
    Application.ProcessMessages;
  end;
end;
```

## Techniques de débogage

### 1. Vérification du gradient

```pascal
function CheckGradient(Layer: TLayer; Input: TVector;
                      Epsilon: Double = 1e-7): Boolean;
var
  i, j: Integer;
  Original, PlusEps, MinusEps: Double;
  NumGrad, AnalyticalGrad: Double;
  Output, Gradient: TVector;
begin
  Result := True;

  for i := 0 to Layer.OutputSize - 1 do
  begin
    for j := 0 to Layer.InputSize - 1 do
    begin
      // Calculer le gradient numérique
      Original := Layer.Weights[i][j];

      Layer.Weights[i][j] := Original + Epsilon;
      PlusEps := ComputeLoss(Layer, Input);

      Layer.Weights[i][j] := Original - Epsilon;
      MinusEps := ComputeLoss(Layer, Input);

      Layer.Weights[i][j] := Original;

      NumGrad := (PlusEps - MinusEps) / (2 * Epsilon);

      // Comparer avec le gradient analytique
      Output := Layer.Forward(Input);
      Layer.Backward(Output, 1.0, Gradient);

      AnalyticalGrad := {gradient calculé};

      if Abs(NumGrad - AnalyticalGrad) > 1e-5 then
      begin
        WriteLn(Format('Erreur gradient[%d][%d]: num=%.6f, anal=%.6f',
          [i, j, NumGrad, AnalyticalGrad]));
        Result := False;
      end;
    end;
  end;
end;
```

### 2. Monitoring des activations

```pascal
procedure MonitorActivations(Network: TNeuralNetwork; Input: TVector);
var
  LayerIdx: Integer;
  Output: TVector;
  Mean, StdDev: Double;
begin
  Output := Input;

  for LayerIdx := 0 to High(Network.Layers) do
  begin
    Output := Network.Layers[LayerIdx].Forward(Output);

    // Calculer statistiques
    Mean := ComputeMean(Output);
    StdDev := ComputeStdDev(Output);

    WriteLn(Format('Couche %d: Mean=%.4f, StdDev=%.4f',
      [LayerIdx, Mean, StdDev]));

    // Vérifier les valeurs explosées ou nulles
    if (Mean > 100) or (Mean < -100) then
      WriteLn('  ATTENTION: Valeurs explosées !');

    if StdDev < 0.001 then
      WriteLn('  ATTENTION: Activations saturées !');
  end;
end;
```

## Conclusion

### Ce que vous avez appris

✅ **Architecture** : Structure complète d'un réseau de neurones en Pascal  
✅ **Mathématiques** : Propagation avant et rétropropagation  
✅ **Activations** : Sigmoid, ReLU, Tanh et leurs dérivées  
✅ **Entraînement** : Descente de gradient et backpropagation  
✅ **Optimisations** : Mini-batch, régularisation, dropout, Adam  
✅ **Applications** : XOR, classification d'images  
✅ **Débogage** : Vérification de gradients, monitoring

### Comparaison avec les bibliothèques externes

| Aspect | From Scratch (Pascal) | TensorFlow/PyTorch |
|--------|----------------------|-------------------|
| **Contrôle** | Total | Limité |
| **Performance CPU** | Excellente | Bonne |
| **Performance GPU** | Non supporté | Excellente |
| **Complexité** | Simple à comprendre | Abstraite |
| **Taille modèles** | Petits/moyens | Tous |
| **Dépendances** | Aucune | Nombreuses |
| **Déploiement** | Très simple | Complexe |
| **Apprentissage** | Excellent | Superficiel |

## Projets complets d'exemple

### Projet 1 : Prédiction de prix immobiliers

```pascal
program HousePricePredictor;

{$mode objfpc}{$H+}

uses
  SysUtils, NeuralNetwork, NeuralTypes, Activations;

type
  THouseData = record
    Surface: Double;        // m²
    Rooms: Integer;         // Nombre de chambres
    Age: Integer;           // Années
    Location: Integer;      // Code postal
    Price: Double;          // Prix en €
  end;

var
  Network: TNeuralNetwork;
  Houses: array of THouseData;
  Inputs, Targets: array of TVector;
  i: Integer;
  TestInput, Prediction: TVector;

procedure LoadHouseData;
begin
  SetLength(Houses, 5);

  // Exemples de maisons
  Houses[0].Surface := 120; Houses[0].Rooms := 3; Houses[0].Age := 10;
  Houses[0].Location := 75001; Houses[0].Price := 500000;

  Houses[1].Surface := 85; Houses[1].Rooms := 2; Houses[1].Age := 5;
  Houses[1].Location := 75002; Houses[1].Price := 350000;

  Houses[2].Surface := 150; Houses[2].Rooms := 4; Houses[2].Age := 20;
  Houses[2].Location := 75016; Houses[2].Price := 750000;

  Houses[3].Surface := 95; Houses[3].Rooms := 2; Houses[3].Age := 3;
  Houses[3].Location := 75011; Houses[3].Price := 400000;

  Houses[4].Surface := 200; Houses[4].Rooms := 5; Houses[4].Age := 1;
  Houses[4].Location := 75007; Houses[4].Price := 1200000;
end;

procedure PrepareData;
var
  i: Integer;
  MaxSurface, MaxAge, MaxLocation, MaxPrice: Double;
begin
  SetLength(Inputs, Length(Houses));
  SetLength(Targets, Length(Houses));

  // Trouver les valeurs maximales pour normalisation
  MaxSurface := 200;
  MaxAge := 50;
  MaxLocation := 100000;
  MaxPrice := 2000000;

  for i := 0 to High(Houses) do
  begin
    // Normaliser les entrées (0-1)
    SetLength(Inputs[i], 4);
    Inputs[i][0] := Houses[i].Surface / MaxSurface;
    Inputs[i][1] := Houses[i].Rooms / 5.0;
    Inputs[i][2] := Houses[i].Age / MaxAge;
    Inputs[i][3] := Houses[i].Location / MaxLocation;

    // Normaliser la cible
    SetLength(Targets[i], 1);
    Targets[i][0] := Houses[i].Price / MaxPrice;
  end;
end;

begin
  WriteLn('=== Prédiction de prix immobiliers ===');
  WriteLn('');

  Randomize;

  // Charger les données
  LoadHouseData;
  PrepareData;

  // Créer le réseau : 4 entrées → 8 → 4 → 1 sortie
  Network := TNeuralNetwork.Create;
  try
    Network.AddLayer(4, 8, atReLU);
    Network.AddLayer(8, 4, atReLU);
    Network.AddLayer(4, 1, atSigmoid);

    WriteLn('Architecture : 4 → 8 → 4 → 1');
    WriteLn('Entrées : Surface, Chambres, Âge, Code postal');
    WriteLn('');

    // Entraîner
    WriteLn('Entraînement...');
    Network.Train(Inputs, Targets, 2000, 0.05);

    WriteLn('');
    WriteLn('=== Prédictions ===');

    // Tester sur les données d'entraînement
    for i := 0 to High(Inputs) do
    begin
      Prediction := Network.Predict(Inputs[i]);

      WriteLn(Format('Maison %d: %.0f m², %d chambres, %d ans',
        [i + 1, Houses[i].Surface, Houses[i].Rooms, Houses[i].Age]));
      WriteLn(Format('  Prix réel : %.0f €', [Houses[i].Price]));
      WriteLn(Format('  Prix prédit : %.0f €', [Prediction[0] * 2000000]));
      WriteLn(Format('  Erreur : %.1f%%',
        [Abs(Prediction[0] * 2000000 - Houses[i].Price) / Houses[i].Price * 100]));
      WriteLn('');
    end;

    // Test sur une nouvelle maison
    WriteLn('=== Nouvelle prédiction ===');
    SetLength(TestInput, 4);
    TestInput[0] := 110 / 200.0;      // 110 m²
    TestInput[1] := 3 / 5.0;          // 3 chambres
    TestInput[2] := 7 / 50.0;         // 7 ans
    TestInput[3] := 75010 / 100000.0; // Code postal 75010

    Prediction := Network.Predict(TestInput);

    WriteLn('Maison de 110 m², 3 chambres, 7 ans, Paris 10e');
    WriteLn(Format('Prix estimé : %.0f €', [Prediction[0] * 2000000]));

  finally
    Network.Free;
  end;

  ReadLn;
end.
```

### Projet 2 : Détection de sentiment (texte)

```pascal
unit SentimentAnalyzer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, NeuralNetwork, NeuralTypes, Activations;

type
  TSentimentAnalyzer = class
  private
    FNetwork: TNeuralNetwork;
    FVocabulary: TStringList;
    FMaxWords: Integer;

    function TextToVector(const Text: string): TVector;
    procedure BuildVocabulary(Texts: array of string);

  public
    constructor Create(MaxWords: Integer = 100);
    destructor Destroy; override;

    procedure Train(Texts: array of string; Sentiments: array of Boolean);
    function Analyze(const Text: string): Double;
  end;

implementation

constructor TSentimentAnalyzer.Create(MaxWords: Integer);
begin
  inherited Create;

  FMaxWords := MaxWords;
  FVocabulary := TStringList.Create;
  FVocabulary.Sorted := True;
  FVocabulary.Duplicates := dupIgnore;

  // Réseau : MaxWords entrées → 64 → 32 → 1 sortie
  FNetwork := TNeuralNetwork.Create;
  FNetwork.AddLayer(MaxWords, 64, atReLU);
  FNetwork.AddLayer(64, 32, atReLU);
  FNetwork.AddLayer(32, 1, atSigmoid);
end;

destructor TSentimentAnalyzer.Destroy;
begin
  FVocabulary.Free;
  FNetwork.Free;
  inherited Destroy;
end;

procedure TSentimentAnalyzer.BuildVocabulary(Texts: array of string);
var
  i, j: Integer;
  Words: TStringList;
  Text: string;
begin
  FVocabulary.Clear;

  for i := 0 to High(Texts) do
  begin
    Text := LowerCase(Texts[i]);

    // Séparer en mots (simplification)
    Words := TStringList.Create;
    try
      Words.Delimiter := ' ';
      Words.StrictDelimiter := True;
      Words.DelimitedText := Text;

      for j := 0 to Words.Count - 1 do
      begin
        if (Length(Words[j]) > 2) and (FVocabulary.Count < FMaxWords) then
          FVocabulary.Add(Words[j]);
      end;
    finally
      Words.Free;
    end;
  end;

  WriteLn(Format('Vocabulaire construit : %d mots', [FVocabulary.Count]));
end;

function TSentimentAnalyzer.TextToVector(const Text: string): TVector;
var
  i, WordIndex: Integer;
  Words: TStringList;
  TextLower: string;
begin
  SetLength(Result, FMaxWords);

  // Initialiser à 0
  for i := 0 to FMaxWords - 1 do
    Result[i] := 0;

  TextLower := LowerCase(Text);

  Words := TStringList.Create;
  try
    Words.Delimiter := ' ';
    Words.StrictDelimiter := True;
    Words.DelimitedText := TextLower;

    // Marquer les mots présents (Bag of Words)
    for i := 0 to Words.Count - 1 do
    begin
      WordIndex := FVocabulary.IndexOf(Words[i]);
      if WordIndex >= 0 then
        Result[WordIndex] := 1;
    end;
  finally
    Words.Free;
  end;
end;

procedure TSentimentAnalyzer.Train(Texts: array of string;
                                   Sentiments: array of Boolean);
var
  Inputs, Targets: array of TVector;
  i: Integer;
begin
  // Construire le vocabulaire
  BuildVocabulary(Texts);

  // Préparer les données
  SetLength(Inputs, Length(Texts));
  SetLength(Targets, Length(Texts));

  for i := 0 to High(Texts) do
  begin
    Inputs[i] := TextToVector(Texts[i]);

    SetLength(Targets[i], 1);
    if Sentiments[i] then
      Targets[i][0] := 1  // Positif
    else
      Targets[i][0] := 0; // Négatif
  end;

  // Entraîner
  WriteLn('Entraînement du modèle...');
  FNetwork.Train(Inputs, Targets, 500, 0.1);
end;

function TSentimentAnalyzer.Analyze(const Text: string): Double;
var
  Input, Output: TVector;
begin
  Input := TextToVector(Text);
  Output := FNetwork.Predict(Input);
  Result := Output[0];
end;

end.
```

**Programme principal pour le sentiment :**

```pascal
program SentimentAnalysis;

{$mode objfpc}{$H+}

uses
  SysUtils, SentimentAnalyzer;

var
  Analyzer: TSentimentAnalyzer;
  Texts: array of string;
  Sentiments: array of Boolean;
  TestTexts: array of string;
  i: Integer;
  Score: Double;

begin
  WriteLn('=== Analyseur de sentiment ===');
  WriteLn('');

  Randomize;

  Analyzer := TSentimentAnalyzer.Create(50);
  try
    // Données d'entraînement
    SetLength(Texts, 6);
    SetLength(Sentiments, 6);

    Texts[0] := 'ce film est excellent j aime beaucoup';
    Sentiments[0] := True;

    Texts[1] := 'tres bon produit je recommande';
    Sentiments[1] := True;

    Texts[2] := 'magnifique performance superbe acteurs';
    Sentiments[2] := True;

    Texts[3] := 'horrible film tres mauvais';
    Sentiments[3] := False;

    Texts[4] := 'produit decevant ne fonctionne pas';
    Sentiments[4] := False;

    Texts[5] := 'nul je deteste totalement';
    Sentiments[5] := False;

    // Entraîner
    Analyzer.Train(Texts, Sentiments);

    WriteLn('');
    WriteLn('=== Tests ===');
    WriteLn('');

    // Tester
    SetLength(TestTexts, 4);
    TestTexts[0] := 'ce produit est vraiment excellent';
    TestTexts[1] := 'film horrible je deteste';
    TestTexts[2] := 'tres bon j aime beaucoup';
    TestTexts[3] := 'totalement nul et decevant';

    for i := 0 to High(TestTexts) do
    begin
      Score := Analyzer.Analyze(TestTexts[i]);

      WriteLn(Format('Texte : "%s"', [TestTexts[i]]));
      WriteLn(Format('Score : %.2f', [Score]));

      if Score > 0.5 then
        WriteLn('Sentiment : POSITIF ✓')
      else
        WriteLn('Sentiment : NÉGATIF ✗');

      WriteLn('');
    end;

  finally
    Analyzer.Free;
  end;

  ReadLn;
end.
```

### Projet 3 : Générateur de nombres (AutoEncoder)

Un autoencoder apprend à compresser puis reconstruire des données.

```pascal
unit AutoEncoder;

{$mode objfpc}{$H+}

interface

uses
  NeuralNetwork, NeuralTypes, Activations;

type
  TAutoEncoder = class
  private
    FEncoder: TNeuralNetwork;
    FDecoder: TNeuralNetwork;
    FLatentDim: Integer;

  public
    constructor Create(InputDim, LatentDim: Integer);
    destructor Destroy; override;

    procedure Train(Data: array of TVector; Epochs: Integer);
    function Encode(const Input: TVector): TVector;
    function Decode(const Latent: TVector): TVector;
    function Reconstruct(const Input: TVector): TVector;
  end;

implementation

constructor TAutoEncoder.Create(InputDim, LatentDim: Integer);
begin
  inherited Create;

  FLatentDim := LatentDim;

  // Encoder : InputDim → LatentDim
  FEncoder := TNeuralNetwork.Create;
  FEncoder.AddLayer(InputDim, 32, atReLU);
  FEncoder.AddLayer(32, LatentDim, atSigmoid);

  // Decoder : LatentDim → InputDim
  FDecoder := TNeuralNetwork.Create;
  FDecoder.AddLayer(LatentDim, 32, atReLU);
  FDecoder.AddLayer(32, InputDim, atSigmoid);
end;

destructor TAutoEncoder.Destroy;
begin
  FEncoder.Free;
  FDecoder.Free;
  inherited Destroy;
end;

procedure TAutoEncoder.Train(Data: array of TVector; Epochs: Integer);
var
  i, epoch: Integer;
  Encoded, Decoded: TVector;
  Loss: Double;
begin
  WriteLn('Entraînement de l''AutoEncoder...');

  for epoch := 1 to Epochs do
  begin
    Loss := 0;

    for i := 0 to High(Data) do
    begin
      // Encoder
      Encoded := FEncoder.Predict(Data[i]);

      // Decoder
      Decoded := FDecoder.Predict(Encoded);

      // Calculer la perte de reconstruction
      Loss := Loss + ComputeMSE(Decoded, Data[i]);

      // Entraîner (simplifié - en pratique il faut backprop à travers les deux)
      FEncoder.Train([Data[i]], [Encoded], 1, 0.01);
      FDecoder.Train([Encoded], [Data[i]], 1, 0.01);
    end;

    if epoch mod 100 = 0 then
      WriteLn(Format('Époque %d - Perte : %.6f', [epoch, Loss / Length(Data)]));
  end;
end;

function TAutoEncoder.Encode(const Input: TVector): TVector;
begin
  Result := FEncoder.Predict(Input);
end;

function TAutoEncoder.Decode(const Latent: TVector): TVector;
begin
  Result := FDecoder.Predict(Latent);
end;

function TAutoEncoder.Reconstruct(const Input: TVector): TVector;
begin
  Result := Decode(Encode(Input));
end;

end.
```

## Techniques avancées

### 1. Early Stopping

Arrêter l'entraînement quand la performance sur validation n'améliore plus :

```pascal
procedure TNeuralNetwork.TrainWithValidation(const TrainInputs, TrainTargets,
                                            ValInputs, ValTargets: array of TVector;
                                            MaxEpochs: Integer;
                                            Patience: Integer = 10);
var
  epoch, patienceCounter: Integer;
  BestValLoss, CurrentValLoss: Double;
  BestWeights: array of TMatrix;
begin
  BestValLoss := 1e10;
  patienceCounter := 0;

  for epoch := 1 to MaxEpochs do
  begin
    // Entraîner une époque
    TrainOneEpoch(TrainInputs, TrainTargets);

    // Évaluer sur validation
    CurrentValLoss := EvaluateLoss(ValInputs, ValTargets);

    if CurrentValLoss < BestValLoss then
    begin
      BestValLoss := CurrentValLoss;
      patienceCounter := 0;

      // Sauvegarder les meilleurs poids
      SaveWeights(BestWeights);

      WriteLn(Format('✓ Époque %d - Nouvelle meilleure validation : %.6f',
        [epoch, BestValLoss]));
    end
    else
    begin
      Inc(patienceCounter);

      if patienceCounter >= Patience then
      begin
        WriteLn(Format('Early stopping à l''époque %d', [epoch]));

        // Restaurer les meilleurs poids
        LoadWeights(BestWeights);
        Break;
      end;
    end;
  end;
end;
```

### 2. Data Augmentation

Pour augmenter artificiellement le dataset :

```pascal
function AugmentImage(const Image: TVector): TVector;
begin
  Result := Copy(Image);

  // Ajouter du bruit
  AddGaussianNoise(Result, 0.1);

  // Rotation aléatoire
  if Random > 0.5 then
    RotateImage(Result, Random * 20 - 10);

  // Flip horizontal
  if Random > 0.5 then
    FlipHorizontal(Result);
end;

procedure TrainWithAugmentation(Network: TNeuralNetwork;
                               const Images, Labels: array of TVector);
var
  AugmentedImages: array of TVector;
  i, j: Integer;
begin
  SetLength(AugmentedImages, Length(Images) * 5);

  // Créer 5 versions augmentées de chaque image
  for i := 0 to High(Images) do
  begin
    for j := 0 to 4 do
      AugmentedImages[i * 5 + j] := AugmentImage(Images[i]);
  end;

  Network.Train(AugmentedImages, Labels, 100, 0.01);
end;
```

### 3. Learning Rate Scheduling

Réduire le taux d'apprentissage au cours du temps :

```pascal
function GetLearningRate(Epoch, MaxEpochs: Integer;
                        InitialLR: Double): Double;
begin
  // Decay exponentiel
  Result := InitialLR * Power(0.95, Epoch div 10);

  // Ou decay linéaire
  // Result := InitialLR * (1 - Epoch / MaxEpochs);

  // Ou step decay
  // if Epoch mod 100 = 0 then
  //   Result := InitialLR * 0.5
  // else
  //   Result := InitialLR;
end;

procedure TrainWithSchedule(Network: TNeuralNetwork;
                           Inputs, Targets: array of TVector;
                           Epochs: Integer);
var
  epoch: Integer;
  LR: Double;
begin
  for epoch := 1 to Epochs do
  begin
    LR := GetLearningRate(epoch, Epochs, 0.1);
    Network.Train(Inputs, Targets, 1, LR);

    if epoch mod 100 = 0 then
      WriteLn(Format('Époque %d - LR : %.6f', [epoch, LR]));
  end;
end;
```

## Benchmark et comparaison de performance

```pascal
procedure BenchmarkNetwork;
var
  Network: TNeuralNetwork;
  Input: TVector;
  StartTime, EndTime: TDateTime;
  i, Iterations: Integer;
  TimeMS: Double;
begin
  WriteLn('=== Benchmark du réseau de neurones ===');
  WriteLn('');

  // Créer un réseau de taille moyenne
  Network := TNeuralNetwork.Create;
  try
    Network.AddLayer(784, 256, atReLU);
    Network.AddLayer(256, 128, atReLU);
    Network.AddLayer(128, 10, atSigmoid);

    SetLength(Input, 784);
    RandomizeVector(Input, 0, 1);

    Iterations := 1000;

    // Benchmark forward pass
    StartTime := Now;
    for i := 1 to Iterations do
      Network.Predict(Input);
    EndTime := Now;

    TimeMS := (EndTime - StartTime) * 24 * 60 * 60 * 1000;

    WriteLn(Format('Forward pass : %d itérations en %.2f ms',
      [Iterations, TimeMS]));
    WriteLn(Format('Moyenne : %.4f ms par prédiction',
      [TimeMS / Iterations]));
    WriteLn(Format('Débit : %.0f prédictions/seconde',
      [Iterations / (TimeMS / 1000)]));

  finally
    Network.Free;
  end;
end;
```

## Conseils pratiques

### 1. Choix de l'architecture

```
Règles générales :
- Commencer simple (1-2 couches cachées)
- Taille couche cachée ≈ moyenne(entrée, sortie)
- Augmenter progressivement si nécessaire

Exemple :
  100 entrées → 10 sorties
  → Essayer : 100 → 50 → 10
  ou : 100 → 70 → 40 → 10
```

### 2. Choix des hyperparamètres

```pascal
// Taux d'apprentissage
// Trop grand : divergence
// Trop petit : apprentissage très lent
// Bon point de départ : 0.01 à 0.1

LearningRate := 0.01;

// Nombre d'époques
// Observer la courbe d'erreur
// Arrêter quand elle stagne

Epochs := 1000;

// Taille de batch
// Plus grand = plus stable mais plus lent
// Plus petit = plus rapide mais bruité

BatchSize := 32;
```

### 3. Normalisation des données

```pascal
procedure NormalizeData(var Data: array of TVector);
var
  i, j: Integer;
  Mean, StdDev: Double;
begin
  // Pour chaque feature
  for j := 0 to High(Data[0]) do
  begin
    // Calculer moyenne
    Mean := 0;
    for i := 0 to High(Data) do
      Mean := Mean + Data[i][j];
    Mean := Mean / Length(Data);

    // Calculer écart-type
    StdDev := 0;
    for i := 0 to High(Data) do
      StdDev := StdDev + Sqr(Data[i][j] - Mean);
    StdDev := Sqrt(StdDev / Length(Data));

    // Normaliser
    for i := 0 to High(Data) do
      Data[i][j] := (Data[i][j] - Mean) / StdDev;
  end;
end;
```

## Ressources et références

### Documentation

- **Deep Learning Book** (Goodfellow et al.) - Référence théorique
- **Neural Networks and Deep Learning** (Michael Nielsen) - Tutoriel accessible
- **CS231n Stanford** - Cours complet en ligne

### Code source complet

Tous les fichiers sources présentés dans ce chapitre sont disponibles et peuvent être utilisés dans vos projets FreePascal.

### Limitations et perspectives

**Limitations de notre implémentation :**
- Pas de support GPU (CPU uniquement)
- Pas de réseaux convolutionnels (CNN)
- Pas de réseaux récurrents (RNN/LSTM)
- Performances limitées pour très grands datasets

**Pour aller plus loin :**
- Implémenter des couches convolutionnelles
- Ajouter le support SIMD pour vectorisation
- Implémenter l'entraînement multi-thread
- Créer des wrappers pour CUDA/OpenCL

## Conclusion finale

### Vous maîtrisez maintenant

✅ **Principes fondamentaux** des réseaux de neurones  
✅ **Implémentation complète** en FreePascal from scratch  
✅ **Mathématiques** : propagation avant et arrière  
✅ **Optimisations** : Adam, dropout, batch normalization  
✅ **Applications pratiques** : classification, régression, autoencoders  
✅ **Débogage** : vérification de gradients, monitoring  
✅ **Déploiement** : sauvegarde/chargement de modèles

### Quand utiliser votre implémentation

**✅ Utiliser votre code Pascal si :**
- Projet éducatif ou apprentissage
- Petit à moyen dataset (<10k exemples)
- Application embarquée ou critique
- Besoin de contrôle total
- Pas de GPU disponible
- Déploiement ultra-simple requis

**❌ Utiliser TensorFlow/PyTorch si :**
- Gros datasets (>100k exemples)
- CNN/RNN complexes nécessaires
- GPU indispensable
- État de l'art requis
- Équipe Python existante

### Prochaines étapes

**Section 15.3** : Computer Vision avec OpenCV  
**Section 15.4** : NLP et traitement de texte  
**Section 15.5** : Algorithmes génétiques  

Vous avez maintenant les fondations solides pour comprendre et implémenter des systèmes d'Intelligence Artificielle en FreePascal ! 🎓🧠🚀

⏭️ [Computer Vision avec OpenCV](/15-intelligence-artificielle-machine-learning/03-computer-vision-opencv.md)
