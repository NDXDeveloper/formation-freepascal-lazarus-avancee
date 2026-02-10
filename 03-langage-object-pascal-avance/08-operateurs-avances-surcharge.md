🔝 Retour au [Sommaire](/SOMMAIRE.md)

# Opérateurs avancés et surcharge en FreePascal/Lazarus

## Introduction : Qu'est-ce que la surcharge d'opérateurs ?

La surcharge d'opérateurs permet de définir le comportement des opérateurs mathématiques (+, -, *, /) et de comparaison (<, >, =) pour vos propres types (classes et records). C'est comme apprendre à votre programme comment "additionner" deux objets personnalisés.

Imaginez que vous avez créé un type `TVecteur` pour représenter des vecteurs mathématiques. Sans surcharge d'opérateurs, vous devriez écrire :
```pascal
V3 := AddVectors(V1, V2);  // Méthode classique
```

Avec la surcharge d'opérateurs, vous pouvez écrire naturellement :
```pascal
V3 := V1 + V2;  // Beaucoup plus intuitif !
```

## Les opérateurs surchargeables

### Liste des opérateurs disponibles

FreePascal permet de surcharger de nombreux opérateurs :

```pascal
// Opérateurs arithmétiques
+    // Addition
-    // Soustraction et négation unaire
*    // Multiplication
/    // Division
div  // Division entière
mod  // Modulo
**   // Puissance

// Opérateurs de comparaison
=    // Égalité
<>   // Différence
<    // Inférieur
>    // Supérieur
<=   // Inférieur ou égal
>=   // Supérieur ou égal

// Opérateurs logiques
and  // ET logique
or   // OU logique
xor  // OU exclusif
not  // NON logique

// Opérateurs bit à bit
shl  // Décalage à gauche
shr  // Décalage à droite

// Opérateurs spéciaux
:=   // Affectation implicite (implicit)
in   // Appartenance
><   // Symétrique (pour intervalles)
```

## Surcharge basique avec des records

### Exemple simple : Un type Fraction

```pascal
type
  TFraction = record
    Numerator: Integer;    // Numérateur
    Denominator: Integer;  // Dénominateur
  end;

// Surcharge de l'opérateur + pour additionner deux fractions
operator + (const A, B: TFraction): TFraction;
begin
  // Addition de fractions : a/b + c/d = (a*d + b*c) / (b*d)
  Result.Numerator := A.Numerator * B.Denominator +
                      B.Numerator * A.Denominator;
  Result.Denominator := A.Denominator * B.Denominator;
end;

// Surcharge de l'opérateur - pour soustraire
operator - (const A, B: TFraction): TFraction;
begin
  Result.Numerator := A.Numerator * B.Denominator -
                      B.Numerator * A.Denominator;
  Result.Denominator := A.Denominator * B.Denominator;
end;

// Surcharge de l'opérateur * pour multiplier
operator * (const A, B: TFraction): TFraction;
begin
  Result.Numerator := A.Numerator * B.Numerator;
  Result.Denominator := A.Denominator * B.Denominator;
end;

// Surcharge de l'opérateur / pour diviser
operator / (const A, B: TFraction): TFraction;
begin
  // Division = multiplication par l'inverse
  Result.Numerator := A.Numerator * B.Denominator;
  Result.Denominator := A.Denominator * B.Numerator;
end;

// Surcharge de l'opérateur = pour comparer
operator = (const A, B: TFraction): Boolean;
begin
  // Deux fractions sont égales si leurs produits croisés sont égaux
  Result := A.Numerator * B.Denominator = B.Numerator * A.Denominator;
end;

// Surcharge de l'opérateur <
operator < (const A, B: TFraction): Boolean;
begin
  Result := A.Numerator * B.Denominator < B.Numerator * A.Denominator;
end;

// Fonction helper pour créer une fraction
function MakeFraction(Num, Denom: Integer): TFraction;
begin
  Result.Numerator := Num;
  Result.Denominator := Denom;
  if Denom = 0 then
    raise Exception.Create('Le dénominateur ne peut pas être zéro');
end;

// Fonction pour simplifier une fraction
procedure SimplifyFraction(var F: TFraction);
var
  GCD: Integer;

  function CalculateGCD(A, B: Integer): Integer;
  begin
    while B <> 0 do
    begin
      Result := B;
      B := A mod B;
      A := Result;
    end;
    Result := A;
  end;

begin
  if F.Denominator = 0 then Exit;

  GCD := CalculateGCD(Abs(F.Numerator), Abs(F.Denominator));
  if GCD > 0 then
  begin
    F.Numerator := F.Numerator div GCD;
    F.Denominator := F.Denominator div GCD;
  end;

  // S'assurer que le signe est au numérateur
  if F.Denominator < 0 then
  begin
    F.Numerator := -F.Numerator;
    F.Denominator := -F.Denominator;
  end;
end;

// Utilisation
procedure UseFractions;
var
  F1, F2, F3: TFraction;
begin
  F1 := MakeFraction(1, 2);  // 1/2
  F2 := MakeFraction(1, 3);  // 1/3

  F3 := F1 + F2;  // 1/2 + 1/3 = 5/6
  SimplifyFraction(F3);
  WriteLn('1/2 + 1/3 = ', F3.Numerator, '/', F3.Denominator);

  F3 := F1 * F2;  // 1/2 * 1/3 = 1/6
  WriteLn('1/2 * 1/3 = ', F3.Numerator, '/', F3.Denominator);

  if F1 > F2 then
    WriteLn('1/2 est plus grand que 1/3');
end;
```

### Exemple avancé : Vecteurs 2D

```pascal
type
  TVector2D = record
    X, Y: Double;
  end;

// Constructeur helper
function Vec2D(AX, AY: Double): TVector2D;
begin
  Result.X := AX;
  Result.Y := AY;
end;

// Addition de vecteurs
operator + (const A, B: TVector2D): TVector2D;
begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
end;

// Soustraction de vecteurs
operator - (const A, B: TVector2D): TVector2D;
begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
end;

// Négation (opérateur unaire -)
operator - (const A: TVector2D): TVector2D;
begin
  Result.X := -A.X;
  Result.Y := -A.Y;
end;

// Multiplication par un scalaire (vecteur * nombre)
operator * (const V: TVector2D; Scalar: Double): TVector2D;
begin
  Result.X := V.X * Scalar;
  Result.Y := V.Y * Scalar;
end;

// Multiplication par un scalaire (nombre * vecteur)
operator * (Scalar: Double; const V: TVector2D): TVector2D;
begin
  Result.X := V.X * Scalar;
  Result.Y := V.Y * Scalar;
end;

// Produit scalaire (dot product)
operator * (const A, B: TVector2D): Double;
begin
  Result := A.X * B.X + A.Y * B.Y;
end;

// Division par un scalaire
operator / (const V: TVector2D; Scalar: Double): TVector2D;
begin
  if Scalar = 0 then
    raise EDivByZero.Create('Division par zéro');
  Result.X := V.X / Scalar;
  Result.Y := V.Y / Scalar;
end;

// Égalité (avec tolérance pour les flottants)
operator = (const A, B: TVector2D): Boolean;
const
  Epsilon = 1E-9;
begin
  Result := (Abs(A.X - B.X) < Epsilon) and
            (Abs(A.Y - B.Y) < Epsilon);
end;

// Conversion implicite depuis un tableau
operator := (const Values: array of Double): TVector2D;
begin
  if Length(Values) >= 2 then
  begin
    Result.X := Values[0];
    Result.Y := Values[1];
  end
  else
    raise Exception.Create('Le tableau doit contenir au moins 2 éléments');
end;

// Méthodes helper pour les vecteurs
function Length(const V: TVector2D): Double;
begin
  Result := Sqrt(V.X * V.X + V.Y * V.Y);
end;

function Normalize(const V: TVector2D): TVector2D;
var
  Len: Double;
begin
  Len := Length(V);
  if Len > 0 then
    Result := V / Len
  else
    Result := V;
end;

function Distance(const A, B: TVector2D): Double;
begin
  Result := Length(B - A);
end;

// Utilisation
procedure UseVectors;
var
  V1, V2, V3: TVector2D;
  DotProduct: Double;
begin
  V1 := Vec2D(3, 4);
  V2 := Vec2D(1, 2);

  // Addition
  V3 := V1 + V2;
  WriteLn('V1 + V2 = (', V3.X:0:2, ', ', V3.Y:0:2, ')');

  // Multiplication par scalaire
  V3 := V1 * 2;
  WriteLn('V1 * 2 = (', V3.X:0:2, ', ', V3.Y:0:2, ')');

  // Produit scalaire
  DotProduct := V1 * V2;
  WriteLn('V1 · V2 = ', DotProduct:0:2);

  // Normalisation
  V3 := Normalize(V1);
  WriteLn('V1 normalisé = (', V3.X:0:2, ', ', V3.Y:0:2, ')');
  WriteLn('Longueur = ', Length(V3):0:2);

  // Conversion implicite depuis tableau
  V3 := [10.5, 20.3];
  WriteLn('V3 = (', V3.X:0:2, ', ', V3.Y:0:2, ')');
end;
```

## Surcharge pour les classes

### Opérateurs pour une classe Complex

```pascal
type
  TComplex = class
  private
    FReal: Double;
    FImaginary: Double;
  public
    constructor Create(AReal, AImaginary: Double);
    property Real: Double read FReal write FReal;
    property Imaginary: Double read FImaginary write FImaginary;

    function ToString: string; override;
  end;

constructor TComplex.Create(AReal, AImaginary: Double);
begin
  FReal := AReal;
  FImaginary := AImaginary;
end;

function TComplex.ToString: string;
begin
  if FImaginary >= 0 then
    Result := Format('%.2f + %.2fi', [FReal, FImaginary])
  else
    Result := Format('%.2f - %.2fi', [FReal, Abs(FImaginary)]);
end;

// Addition de nombres complexes
operator + (const A, B: TComplex): TComplex;
begin
  Result := TComplex.Create(A.Real + B.Real,
                            A.Imaginary + B.Imaginary);
end;

// Soustraction
operator - (const A, B: TComplex): TComplex;
begin
  Result := TComplex.Create(A.Real - B.Real,
                            A.Imaginary - B.Imaginary);
end;

// Multiplication
operator * (const A, B: TComplex): TComplex;
var
  RealPart, ImagPart: Double;
begin
  // (a + bi) * (c + di) = (ac - bd) + (ad + bc)i
  RealPart := A.Real * B.Real - A.Imaginary * B.Imaginary;
  ImagPart := A.Real * B.Imaginary + A.Imaginary * B.Real;
  Result := TComplex.Create(RealPart, ImagPart);
end;

// Division
operator / (const A, B: TComplex): TComplex;
var
  Denominator, RealPart, ImagPart: Double;
begin
  // (a + bi) / (c + di) = ((ac + bd) / (c² + d²)) + ((bc - ad) / (c² + d²))i
  Denominator := B.Real * B.Real + B.Imaginary * B.Imaginary;
  if Denominator = 0 then
    raise EDivByZero.Create('Division par zéro complexe');

  RealPart := (A.Real * B.Real + A.Imaginary * B.Imaginary) / Denominator;
  ImagPart := (A.Imaginary * B.Real - A.Real * B.Imaginary) / Denominator;
  Result := TComplex.Create(RealPart, ImagPart);
end;

// Égalité
operator = (const A, B: TComplex): Boolean;
const
  Epsilon = 1E-9;
begin
  Result := (Abs(A.Real - B.Real) < Epsilon) and
            (Abs(A.Imaginary - B.Imaginary) < Epsilon);
end;

// Module (valeur absolue) d'un nombre complexe
function Abs(const C: TComplex): Double;
begin
  Result := Sqrt(C.Real * C.Real + C.Imaginary * C.Imaginary);
end;

// Conjugué d'un nombre complexe
function Conjugate(const C: TComplex): TComplex;
begin
  Result := TComplex.Create(C.Real, -C.Imaginary);
end;

// Utilisation
procedure UseComplex;
var
  C1, C2, C3: TComplex;
begin
  C1 := TComplex.Create(3, 4);    // 3 + 4i
  C2 := TComplex.Create(1, -2);   // 1 - 2i

  try
    C3 := C1 + C2;
    WriteLn('C1 + C2 = ', C3.ToString);
    C3.Free;

    C3 := C1 * C2;
    WriteLn('C1 * C2 = ', C3.ToString);
    C3.Free;

    WriteLn('|C1| = ', Abs(C1):0:2);

    C3 := Conjugate(C1);
    WriteLn('Conjugué de C1 = ', C3.ToString);
    C3.Free;
  finally
    C1.Free;
    C2.Free;
  end;
end;
```

## Conversions implicites et explicites

### Opérateurs de conversion

```pascal
type
  TTemperature = record
    Celsius: Double;
  end;

// Conversion implicite : affectation automatique
operator := (Value: Double): TTemperature;
begin
  Result.Celsius := Value;
end;

// Conversion implicite : température vers string
operator := (const Temp: TTemperature): string;
begin
  Result := Format('%.1f°C', [Temp.Celsius]);
end;

// Conversion explicite : Celsius vers Fahrenheit
operator Explicit(const Temp: TTemperature): Double;
begin
  Result := Temp.Celsius * 9/5 + 32; // Retourne en Fahrenheit
end;

// Conversion depuis Integer
operator := (Value: Integer): TTemperature;
begin
  Result.Celsius := Value;
end;

// Opérateurs arithmétiques
operator + (const T1, T2: TTemperature): TTemperature;
begin
  Result.Celsius := T1.Celsius + T2.Celsius;
end;

operator - (const T1, T2: TTemperature): TTemperature;
begin
  Result.Celsius := T1.Celsius - T2.Celsius;
end;

// Comparaisons
operator > (const T1, T2: TTemperature): Boolean;
begin
  Result := T1.Celsius > T2.Celsius;
end;

operator < (const T1, T2: TTemperature): Boolean;
begin
  Result := T1.Celsius < T2.Celsius;
end;

// Utilisation
procedure UseTemperature;
var
  T1, T2, T3: TTemperature;
  Fahrenheit: Double;
  Description: string;
begin
  // Conversion implicite depuis Double
  T1 := 25.5;  // 25.5°C

  // Conversion implicite depuis Integer
  T2 := 10;    // 10°C

  // Addition
  T3 := T1 + T2;
  WriteLn('T1 + T2 = ', T3.Celsius:0:1, '°C');

  // Comparaison
  if T1 > T2 then
    WriteLn('T1 est plus chaud que T2');

  // Conversion explicite vers Fahrenheit
  Fahrenheit := Double(T1);
  WriteLn('T1 en Fahrenheit = ', Fahrenheit:0:1, '°F');

  // Conversion implicite vers string
  Description := T1;
  WriteLn('Description : ', Description);
end;
```

## Opérateurs pour types génériques

### Surcharge avec génériques

```pascal
type
  // Type générique pour une paire de valeurs
  generic TPair<T> = record
    First, Second: T;
  end;

  // Spécialisation pour les entiers
  TIntPair = specialize TPair<Integer>;

// Opérateurs pour TIntPair
operator + (const A, B: TIntPair): TIntPair;
begin
  Result.First := A.First + B.First;
  Result.Second := A.Second + B.Second;
end;

operator * (const P: TIntPair; Scalar: Integer): TIntPair;
begin
  Result.First := P.First * Scalar;
  Result.Second := P.Second * Scalar;
end;

operator = (const A, B: TIntPair): Boolean;
begin
  Result := (A.First = B.First) and (A.Second = B.Second);
end;

// Type matrice générique
type
  generic TMatrix<T> = record
  type
    TRow = array of T;
  private
    FData: array of TRow;
    FRows, FCols: Integer;
  public
    procedure Init(ARows, ACols: Integer);
    function GetElement(Row, Col: Integer): T;
    procedure SetElement(Row, Col: Integer; Value: T);
    property Elements[Row, Col: Integer]: T
      read GetElement write SetElement; default;
  end;

  // Spécialisation pour Double
  TDoubleMatrix = specialize TMatrix<Double>;

// Opérateurs pour matrices de Double
operator + (const A, B: TDoubleMatrix): TDoubleMatrix;
var
  I, J: Integer;
begin
  Result.Init(A.FRows, A.FCols);
  for I := 0 to A.FRows - 1 do
    for J := 0 to A.FCols - 1 do
      Result[I, J] := A[I, J] + B[I, J];
end;

operator * (const M: TDoubleMatrix; Scalar: Double): TDoubleMatrix;
var
  I, J: Integer;
begin
  Result.Init(M.FRows, M.FCols);
  for I := 0 to M.FRows - 1 do
    for J := 0 to M.FCols - 1 do
      Result[I, J] := M[I, J] * Scalar;
end;
```

## Opérateurs bit à bit personnalisés

### Type pour manipulation de bits

```pascal
type
  TBitSet = record
  private
    FBits: Cardinal;
  public
    procedure Clear;
    procedure SetBit(Index: Integer);
    procedure ClearBit(Index: Integer);
    function GetBit(Index: Integer): Boolean;
    function Count: Integer;
  end;

procedure TBitSet.Clear;
begin
  FBits := 0;
end;

procedure TBitSet.SetBit(Index: Integer);
begin
  if (Index >= 0) and (Index < 32) then
    FBits := FBits or (1 shl Index);
end;

procedure TBitSet.ClearBit(Index: Integer);
begin
  if (Index >= 0) and (Index < 32) then
    FBits := FBits and not (1 shl Index);
end;

function TBitSet.GetBit(Index: Integer): Boolean;
begin
  if (Index >= 0) and (Index < 32) then
    Result := (FBits and (1 shl Index)) <> 0
  else
    Result := False;
end;

function TBitSet.Count: Integer;
var
  Temp: Cardinal;
begin
  Result := 0;
  Temp := FBits;
  while Temp <> 0 do
  begin
    Inc(Result);
    Temp := Temp and (Temp - 1); // Efface le bit le plus à droite
  end;
end;

// Union (OR)
operator or (const A, B: TBitSet): TBitSet;
begin
  Result.FBits := A.FBits or B.FBits;
end;

// Intersection (AND)
operator and (const A, B: TBitSet): TBitSet;
begin
  Result.FBits := A.FBits and B.FBits;
end;

// Différence symétrique (XOR)
operator xor (const A, B: TBitSet): TBitSet;
begin
  Result.FBits := A.FBits xor B.FBits;
end;

// Complément (NOT)
operator not (const A: TBitSet): TBitSet;
begin
  Result.FBits := not A.FBits;
end;

// Décalage à gauche
operator shl (const A: TBitSet; Shift: Integer): TBitSet;
begin
  Result.FBits := A.FBits shl Shift;
end;

// Décalage à droite
operator shr (const A: TBitSet; Shift: Integer): TBitSet;
begin
  Result.FBits := A.FBits shr Shift;
end;

// Test d'appartenance
operator in (Bit: Integer; const S: TBitSet): Boolean;
begin
  Result := S.GetBit(Bit);
end;

// Égalité
operator = (const A, B: TBitSet): Boolean;
begin
  Result := A.FBits = B.FBits;
end;

// Inclusion
operator <= (const A, B: TBitSet): Boolean;
begin
  // A est inclus dans B si A AND B = A
  Result := (A.FBits and B.FBits) = A.FBits;
end;

// Utilisation
procedure UseBitSet;
var
  Set1, Set2, Set3: TBitSet;
begin
  Set1.Clear;
  Set1.SetBit(0);  // Bit 0
  Set1.SetBit(2);  // Bit 2
  Set1.SetBit(4);  // Bit 4

  Set2.Clear;
  Set2.SetBit(1);  // Bit 1
  Set2.SetBit(2);  // Bit 2
  Set2.SetBit(3);  // Bit 3

  // Union
  Set3 := Set1 or Set2;
  WriteLn('Union : ', Set3.Count, ' bits actifs');

  // Intersection
  Set3 := Set1 and Set2;
  WriteLn('Intersection : ', Set3.Count, ' bits actifs');

  // Test d'appartenance
  if 2 in Set1 then
    WriteLn('Le bit 2 est dans Set1');

  // Inclusion
  if Set3 <= Set1 then
    WriteLn('Set3 est inclus dans Set1');
end;
```

## Opérateurs de comparaison avancés

### Type intervalle avec opérateurs

```pascal
type
  TInterval = record
    Min, Max: Double;
  end;

// Constructeur helper
function Interval(AMin, AMax: Double): TInterval;
begin
  if AMin > AMax then
    raise Exception.Create('Min doit être <= Max');
  Result.Min := AMin;
  Result.Max := AMax;
end;

// Test si une valeur est dans l'intervalle
operator in (Value: Double; const I: TInterval): Boolean;
begin
  Result := (Value >= I.Min) and (Value <= I.Max);
end;

// Test si un intervalle est dans un autre
operator in (const A, B: TInterval): Boolean;
begin
  Result := (A.Min >= B.Min) and (A.Max <= B.Max);
end;

// Nécessite : uses Math;  (pour Max, Min)

// Intersection de deux intervalles
operator * (const A, B: TInterval): TInterval;
begin
  Result.Min := Max(A.Min, B.Min);
  Result.Max := Min(A.Max, B.Max);
  if Result.Min > Result.Max then
    raise Exception.Create('Intervalles disjoints');
end;

// Union de deux intervalles (s'ils se chevauchent)
operator + (const A, B: TInterval): TInterval;
begin
  if (A.Max < B.Min) or (B.Max < A.Min) then
    raise Exception.Create('Intervalles non contigus');
  Result.Min := Min(A.Min, B.Min);
  Result.Max := Max(A.Max, B.Max);
end;

// Comparaison : A < B si A est entièrement avant B
operator < (const A, B: TInterval): Boolean;
begin
  Result := A.Max < B.Min;
end;

// Égalité
operator = (const A, B: TInterval): Boolean;
const
  Epsilon = 1E-9;
begin
  Result := (Abs(A.Min - B.Min) < Epsilon) and
            (Abs(A.Max - B.Max) < Epsilon);
end;

// Opérateur symétrique (chevauche)
operator >< (const A, B: TInterval): Boolean;
begin
  Result := not ((A.Max < B.Min) or (B.Max < A.Min));
end;

// Utilisation
procedure UseIntervals;
var
  I1, I2, I3: TInterval;
  Value: Double;
begin
  I1 := Interval(0, 10);
  I2 := Interval(5, 15);

  Value := 7;
  if Value in I1 then
    WriteLn(Value:0:0, ' est dans [0, 10]');

  if I1 >< I2 then
    WriteLn('Les intervalles se chevauchent');

  try
    I3 := I1 * I2;  // Intersection
    WriteLn('Intersection : [', I3.Min:0:0, ', ', I3.Max:0:0, ']');
  except
    WriteLn('Pas d''intersection');
  end;

  I3 := I1 + I2;  // Union
  WriteLn('Union : [', I3.Min:0:0, ', ', I3.Max:0:0, ']');
end;
```

## Chaînage d'opérateurs et priorités

### Exemple avec un type Money

```pascal
type
  TMoney = record
    Amount: Currency;
    CurrencyCode: string[3];
  end;

// Constructeur
function Money(AAmount: Currency; const ACurrency: string = 'EUR'): TMoney;
begin
  Result.Amount := AAmount;
  Result.CurrencyCode := ACurrency;
end;

// Addition (même devise seulement)
operator + (const A, B: TMoney): TMoney;
begin
  if A.CurrencyCode <> B.CurrencyCode then
    raise Exception.Create('Devises différentes');
  Result.Amount := A.Amount + B.Amount;
  Result.CurrencyCode := A.CurrencyCode;
end;

// Soustraction
operator - (const A, B: TMoney): TMoney;
begin
  if A.CurrencyCode <> B.CurrencyCode then
    raise Exception.Create('Devises différentes');
  Result.Amount := A.Amount - B.Amount;
  Result.CurrencyCode := A.CurrencyCode;
end;

// Multiplication par un nombre
operator * (const M: TMoney; Factor: Double): TMoney;
begin
  Result.Amount := M.Amount * Factor;
  Result.CurrencyCode := M.CurrencyCode;
end;

operator * (Factor: Double; const M: TMoney): TMoney;
begin
  Result := M * Factor;
end;

// Division par un nombre
operator / (const M: TMoney; Divisor: Double): TMoney;
begin
  if Divisor = 0 then
    raise EDivByZero.Create('Division par zéro');
  Result.Amount := M.Amount / Divisor;
  Result.CurrencyCode := M.CurrencyCode;
end;

// Division entre deux montants (ratio)
operator / (const A, B: TMoney): Double;
begin
  if A.CurrencyCode <> B.CurrencyCode then
    raise Exception.Create('Devises différentes');
  if B.Amount = 0 then
    raise EDivByZero.Create('Division par zéro');
  Result := A.Amount / B.Amount;
end;

// Comparaisons
operator = (const A, B: TMoney): Boolean;
begin
  Result := (A.Amount = B.Amount) and (A.CurrencyCode = B.CurrencyCode);
end;

operator < (const A, B: TMoney): Boolean;
begin
  if A.CurrencyCode <> B.CurrencyCode then
    raise Exception.Create('Devises différentes');
  Result := A.Amount < B.Amount;
end;

operator > (const A, B: TMoney): Boolean;
begin
  if A.CurrencyCode <> B.CurrencyCode then
    raise Exception.Create('Devises différentes');
  Result := A.Amount > B.Amount;
end;

operator <= (const A, B: TMoney): Boolean;
begin
  if A.CurrencyCode <> B.CurrencyCode then
    raise Exception.Create('Devises différentes');
  Result := A.Amount <= B.Amount;
end;

operator >= (const A, B: TMoney): Boolean;
begin
  if A.CurrencyCode <> B.CurrencyCode then
    raise Exception.Create('Devises différentes');
  Result := A.Amount >= B.Amount;
end;

// Négation
operator - (const M: TMoney): TMoney;
begin
  Result.Amount := -M.Amount;
  Result.CurrencyCode := M.CurrencyCode;
end;

// Conversion vers string
operator := (const M: TMoney): string;
begin
  Result := Format('%.2f %s', [M.Amount, M.CurrencyCode]);
end;

// Utilisation avec chaînage
procedure UseMoneyChaining;
var
  Price, Tax, Shipping, Total: TMoney;
  Discount: Double;
  Description: string;
begin
  Price := Money(100, 'EUR');
  Tax := Money(20, 'EUR');
  Shipping := Money(5.50, 'EUR');
  Discount := 0.10; // 10%

  // Chaînage d'opérateurs
  Total := (Price + Tax + Shipping) * (1 - Discount);

  WriteLn('Prix : ', string(Price));
  WriteLn('Taxe : ', string(Tax));
  WriteLn('Livraison : ', string(Shipping));
  WriteLn('Réduction : ', (Discount * 100):0:0, '%');
  WriteLn('Total : ', string(Total));

  // Comparaisons
  if Total > Money(100, 'EUR') then
    WriteLn('Le total dépasse 100 EUR');

  // Ratio
  WriteLn('La taxe représente ', (Tax / Price * 100):0:1, '% du prix');
end;
```

## Opérateurs pour types énumérés

### Surcharge pour enum personnalisé

```pascal
type
  TDay = (Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday);

// Opérateur successor (jour suivant)
operator Inc(const Day: TDay): TDay;
begin
  if Day = Sunday then
    Result := Monday  // Retour au début
  else
    Result := Succ(Day);
end;

// Opérateur predecessor (jour précédent)
operator Dec(const Day: TDay): TDay;
begin
  if Day = Monday then
    Result := Sunday  // Retour à la fin
  else
    Result := Pred(Day);
end;

// Addition de jours
operator + (const Day: TDay; Days: Integer): TDay;
var
  I: Integer;
begin
  Result := Day;
  if Days > 0 then
    for I := 1 to Days do
      Inc(Result)
  else if Days < 0 then
    for I := -1 downto Days do
      Dec(Result);
end;

// Soustraction de jours
operator - (const Day: TDay; Days: Integer): TDay;
begin
  Result := Day + (-Days);
end;

// Distance entre deux jours
operator - (const Day1, Day2: TDay): Integer;
var
  D: TDay;
begin
  Result := 0;
  D := Day2;
  while D <> Day1 do
  begin
    Inc(D);
    Inc(Result);
    if Result > 7 then // Sécurité
    begin
      Result := Ord(Day1) - Ord(Day2);
      Break;
    end;
  end;
end;

// Comparaison cyclique
operator < (const Day1, Day2: TDay): Boolean;
begin
  Result := Ord(Day1) < Ord(Day2);
end;

// Type pour permissions
type
  TPermission = (pRead, pWrite, pExecute, pDelete);
  TPermissions = set of TPermission;

// Note : Les opérateurs +, -, * et in sont déjà intégrés pour les types
// ensemble (set of). Il n'est pas nécessaire (ni recommandé) de les surcharger,
// car cela provoquerait une récursion infinie (l'opérateur s'appellerait
// lui-même au lieu d'utiliser l'opérateur natif des ensembles).

// Utilisation
procedure UseEnumOperators;
var
  Today, NextWeek: TDay;
  UserPerms, AdminPerms, EffectivePerms: TPermissions;
begin
  Today := Wednesday;

  // Jour suivant
  Inc(Today);
  // Nécessite : uses TypInfo;
  WriteLn('Demain : ', GetEnumName(TypeInfo(TDay), Ord(Today)));

  // Dans une semaine
  NextWeek := Today + 7;
  WriteLn('Dans une semaine : ', GetEnumName(TypeInfo(TDay), Ord(NextWeek)));

  // Permissions
  UserPerms := [pRead, pWrite];
  AdminPerms := [pRead, pWrite, pExecute, pDelete];

  // Union
  EffectivePerms := UserPerms + [pExecute];

  // Test
  if pDelete in AdminPerms then
    WriteLn('Admin peut supprimer');

  if not (pDelete in UserPerms) then
    WriteLn('User ne peut pas supprimer');
end;
```

## Opérateurs pour pointeurs intelligents

### Smart pointer avec comptage de références

> **Note :** Cet exemple utilise des `class operator` de conversion (`:=`) et de comparaison (`=`) à l'intérieur du record, ce qui nécessite `{$mode delphi}`. En mode ObjFPC, seuls les opérateurs de gestion (`Initialize`, `Finalize`, `Copy`, `AddRef`) peuvent être déclarés dans un record ; les autres opérateurs doivent être globaux.

```pascal
{$mode delphi}
type
  // Pointeur intelligent générique
  TSmartPtr<T: class> = record
  private
    FObject: T;
    FRefCount: ^Integer;
    procedure AddRef;
    procedure Release;
  public
    class operator Initialize(var SP: TSmartPtr<T>);
    class operator Finalize(var SP: TSmartPtr<T>);
    class operator Copy(constref Src: TSmartPtr<T>; var Dst: TSmartPtr<T>);
    class operator AddRef(var SP: TSmartPtr<T>);

    // Opérateurs de conversion
    class operator := (AObject: T): TSmartPtr<T>;
    class operator := (const SP: TSmartPtr<T>): T;

    // Comparaisons
    class operator = (const A, B: TSmartPtr<T>): Boolean;
    class operator = (const SP: TSmartPtr<T>; Obj: T): Boolean;

    // Déréférencement
    function Get: T;
    property Value: T read Get;
  end;

class operator TSmartPtr<T>.Initialize(var SP: TSmartPtr<T>);
begin
  SP.FObject := nil;
  SP.FRefCount := nil;
end;

class operator TSmartPtr<T>.Finalize(var SP: TSmartPtr<T>);
begin
  SP.Release;
end;

procedure TSmartPtr<T>.AddRef;
begin
  if Assigned(FRefCount) then
    Inc(FRefCount^);
end;

procedure TSmartPtr<T>.Release;
begin
  if Assigned(FRefCount) then
  begin
    Dec(FRefCount^);
    if FRefCount^ <= 0 then
    begin
      FreeAndNil(FObject);
      Dispose(FRefCount);
      FRefCount := nil;
    end;
  end;
end;

class operator TSmartPtr<T>.Copy(constref Src: TSmartPtr<T>;
                                 var Dst: TSmartPtr<T>);
begin
  if @Src <> @Dst then
  begin
    Dst.Release;
    Dst.FObject := Src.FObject;
    Dst.FRefCount := Src.FRefCount;
    Dst.AddRef;
  end;
end;

class operator TSmartPtr<T>.AddRef(var SP: TSmartPtr<T>);
begin
  SP.AddRef;
end;

// Conversion depuis un objet
class operator TSmartPtr<T>.:=(AObject: T): TSmartPtr<T>;
begin
  Result.FObject := AObject;
  if Assigned(AObject) then
  begin
    New(Result.FRefCount);
    Result.FRefCount^ := 1;
  end
  else
    Result.FRefCount := nil;
end;

// Conversion vers un objet
class operator TSmartPtr<T>.:=(const SP: TSmartPtr<T>): T;
begin
  Result := SP.FObject;
end;

// Comparaison entre smart pointers
class operator TSmartPtr<T>.=(const A, B: TSmartPtr<T>): Boolean;
begin
  Result := A.FObject = B.FObject;
end;

// Comparaison avec un objet
class operator TSmartPtr<T>.=(const SP: TSmartPtr<T>; Obj: T): Boolean;
begin
  Result := SP.FObject = Obj;
end;

function TSmartPtr<T>.Get: T;
begin
  Result := FObject;
end;

// Utilisation
type
  TMyClass = class
    Name: string;
    constructor Create(const AName: string);
    destructor Destroy; override;
  end;

  TMyClassPtr = TSmartPtr<TMyClass>;

constructor TMyClass.Create(const AName: string);
begin
  Name := AName;
  WriteLn('Création de ', Name);
end;

destructor TMyClass.Destroy;
begin
  WriteLn('Destruction de ', Name);
  inherited;
end;

procedure UseSmartPointers;
var
  P1, P2: TMyClassPtr;
  Obj: TMyClass;
begin
  // Création automatique
  P1 := TMyClass.Create('Objet 1');

  // Copie (partage la référence)
  P2 := P1;

  // Accès à l'objet
  WriteLn('Nom : ', P1.Value.Name);

  // Comparaison
  if P1 = P2 then
    WriteLn('P1 et P2 pointent vers le même objet');

  // Conversion implicite
  Obj := P1;
  if Assigned(Obj) then
    WriteLn('Objet récupéré : ', Obj.Name);

  // Destruction automatique en sortie de scope
end;
```

## Surcharge d'opérateurs pour les matrices

### Opérations matricielles complètes

```pascal
type
  TMatrix3x3 = array[0..2, 0..2] of Double;

// Création d'une matrice identité
function IdentityMatrix: TMatrix3x3;
var
  I, J: Integer;
begin
  for I := 0 to 2 do
    for J := 0 to 2 do
      if I = J then
        Result[I, J] := 1
      else
        Result[I, J] := 0;
end;

// Addition de matrices
operator + (const A, B: TMatrix3x3): TMatrix3x3;
var
  I, J: Integer;
begin
  for I := 0 to 2 do
    for J := 0 to 2 do
      Result[I, J] := A[I, J] + B[I, J];
end;

// Soustraction
operator - (const A, B: TMatrix3x3): TMatrix3x3;
var
  I, J: Integer;
begin
  for I := 0 to 2 do
    for J := 0 to 2 do
      Result[I, J] := A[I, J] - B[I, J];
end;

// Multiplication de matrices
operator * (const A, B: TMatrix3x3): TMatrix3x3;
var
  I, J, K: Integer;
begin
  for I := 0 to 2 do
    for J := 0 to 2 do
    begin
      Result[I, J] := 0;
      for K := 0 to 2 do
        Result[I, J] := Result[I, J] + A[I, K] * B[K, J];
    end;
end;

// Multiplication par un scalaire
operator * (const M: TMatrix3x3; S: Double): TMatrix3x3;
var
  I, J: Integer;
begin
  for I := 0 to 2 do
    for J := 0 to 2 do
      Result[I, J] := M[I, J] * S;
end;

operator * (S: Double; const M: TMatrix3x3): TMatrix3x3;
begin
  Result := M * S;
end;

// Multiplication matrice-vecteur
type
  TVector3 = array[0..2] of Double;

operator * (const M: TMatrix3x3; const V: TVector3): TVector3;
var
  I, J: Integer;
begin
  for I := 0 to 2 do
  begin
    Result[I] := 0;
    for J := 0 to 2 do
      Result[I] := Result[I] + M[I, J] * V[J];
  end;
end;

// Puissance de matrice
operator ** (const M: TMatrix3x3; N: Integer): TMatrix3x3;
var
  I: Integer;
begin
  if N < 0 then
    raise Exception.Create('Exposant négatif non supporté');

  Result := IdentityMatrix;
  for I := 1 to N do
    Result := Result * M;
end;

// Égalité avec tolérance
operator = (const A, B: TMatrix3x3): Boolean;
const
  Epsilon = 1E-9;
var
  I, J: Integer;
begin
  Result := True;
  for I := 0 to 2 do
    for J := 0 to 2 do
      if Abs(A[I, J] - B[I, J]) > Epsilon then
      begin
        Result := False;
        Exit;
      end;
end;

// Fonctions helper
function Det(const M: TMatrix3x3): Double;
begin
  Result := M[0,0] * (M[1,1] * M[2,2] - M[1,2] * M[2,1]) -
            M[0,1] * (M[1,0] * M[2,2] - M[1,2] * M[2,0]) +
            M[0,2] * (M[1,0] * M[2,1] - M[1,1] * M[2,0]);
end;

function Transpose(const M: TMatrix3x3): TMatrix3x3;
var
  I, J: Integer;
begin
  for I := 0 to 2 do
    for J := 0 to 2 do
      Result[I, J] := M[J, I];
end;

// Utilisation
procedure UseMatrixOperators;
var
  M1, M2, M3: TMatrix3x3;
  V1, V2: TVector3;
  I, J: Integer;
begin
  // Initialisation
  M1 := IdentityMatrix;

  // Remplir M2
  for I := 0 to 2 do
    for J := 0 to 2 do
      M2[I, J] := I * 3 + J + 1;

  // Opérations
  M3 := M1 + M2;         // Addition
  M3 := M2 * 2;          // Multiplication scalaire
  M3 := M1 * M2;         // Multiplication matricielle
  M3 := M2 ** 2;         // M2 au carré

  // Vecteur
  V1[0] := 1; V1[1] := 2; V1[2] := 3;
  V2 := M2 * V1;         // Transformation du vecteur

  WriteLn('Déterminant de M2 : ', Det(M2):0:2);

  // Test d'égalité
  if M1 = IdentityMatrix then
    WriteLn('M1 est la matrice identité');
end;
```

## Gestion des erreurs dans les opérateurs

### Opérateurs sûrs avec validation

```pascal
type
  TSafeInt = record
  private
    FValue: Int64;
    FOverflow: Boolean;
  public
    property Value: Int64 read FValue;
    property HasOverflow: Boolean read FOverflow;
    procedure CheckOverflow;
  end;

function SafeInt(Value: Int64): TSafeInt;
begin
  Result.FValue := Value;
  Result.FOverflow := False;
end;

procedure TSafeInt.CheckOverflow;
begin
  if FOverflow then
    raise EOverflow.Create('Dépassement arithmétique détecté');
end;

// Addition sûre
operator + (const A, B: TSafeInt): TSafeInt;
begin
  Result.FOverflow := A.FOverflow or B.FOverflow;

  // Vérifier le dépassement
  if (B.FValue > 0) and (A.FValue > High(Int64) - B.FValue) then
    Result.FOverflow := True
  else if (B.FValue < 0) and (A.FValue < Low(Int64) - B.FValue) then
    Result.FOverflow := True;

  if not Result.FOverflow then
    Result.FValue := A.FValue + B.FValue
  else
    Result.FValue := 0;
end;

// Multiplication sûre
operator * (const A, B: TSafeInt): TSafeInt;
begin
  Result.FOverflow := A.FOverflow or B.FOverflow;

  if (A.FValue <> 0) and (B.FValue <> 0) then
  begin
    if Abs(A.FValue) > High(Int64) div Abs(B.FValue) then
      Result.FOverflow := True;
  end;

  if not Result.FOverflow then
    Result.FValue := A.FValue * B.FValue
  else
    Result.FValue := 0;
end;

// Division sûre
operator / (const A, B: TSafeInt): TSafeInt;
begin
  Result.FOverflow := A.FOverflow or B.FOverflow;

  if B.FValue = 0 then
    Result.FOverflow := True
  else if (A.FValue = Low(Int64)) and (B.FValue = -1) then
    Result.FOverflow := True;  // Cas spécial de dépassement

  if not Result.FOverflow then
    Result.FValue := A.FValue div B.FValue
  else
    Result.FValue := 0;
end;

// Conversion avec validation
operator := (Value: Integer): TSafeInt;
begin
  Result.FValue := Value;
  Result.FOverflow := False;
end;

// Utilisation
procedure UseSafeInt;
var
  A, B, C: TSafeInt;
begin
  A := MaxInt;
  B := 2;

  C := A * B;  // Dépassement potentiel

  if C.HasOverflow then
    WriteLn('Attention : dépassement détecté!')
  else
    WriteLn('Résultat : ', C.Value);

  try
    C.CheckOverflow;  // Lève une exception si overflow
  except
    on E: EOverflow do
      WriteLn('Erreur : ', E.Message);
  end;
end;
```

## Bonnes pratiques et recommandations

### Règles de conception

```pascal
// 1. Cohérence avec les types standards
type
  TMyNumber = record
    Value: Double;
  end;

// ✅ BON : Comportement intuitif
operator + (const A, B: TMyNumber): TMyNumber;
begin
  Result.Value := A.Value + B.Value;
end;

// ❌ MAUVAIS : Comportement surprenant
operator + (const A, B: TMyNumber): TMyNumber;
begin
  Result.Value := A.Value * B.Value; // NON ! + ne doit pas multiplier
end;

// 2. Commutativité quand appropriée
operator * (const A: TMyType; B: Double): TMyType;
begin
  // Implémentation
end;

// Aussi définir l'inverse
operator * (B: Double; const A: TMyType): TMyType;
begin
  Result := A * B;  // Déléguer à la première version
end;

// 3. Gestion des cas limites
operator / (const A, B: TMyType): TMyType;
begin
  if B.IsZero then
    raise EDivByZero.Create('Division par zéro');
  // ... reste de l'implémentation
end;

// 4. Performance : passer par référence
operator + (const A, B: TLargeRecord): TLargeRecord; // const = passage par référence
begin
  // Plus efficace pour les grandes structures
end;

// 5. Documentation claire
{
  Opérateur de multiplication pour TComplex
  Implémente la formule : (a+bi) * (c+di) = (ac-bd) + (ad+bc)i
  @param A Premier nombre complexe
  @param B Second nombre complexe
  @return Produit des deux nombres complexes
  @raises EInvalidOp si l'un des nombres contient NaN
}
operator * (const A, B: TComplex): TComplex;
```

### Exemples de mauvaises pratiques

```pascal
// ❌ ÉVITER : Surcharge confuse
type
  TBadExample = record
    Value: Integer;
  end;

// Utiliser + pour concaténer des entiers !?
operator + (const A, B: TBadExample): string;
begin
  Result := IntToStr(A.Value) + IntToStr(B.Value);
end;

// ❌ ÉVITER : Effets de bord dans les opérateurs
var
  GlobalCounter: Integer = 0;

operator + (const A, B: TMyType): TMyType;
begin
  Inc(GlobalCounter); // NON ! Les opérateurs doivent être purs
  Result.Value := A.Value + B.Value;
end;

// ❌ ÉVITER : Opérateurs non symétriques sans raison
operator = (const A, B: TMyType): Boolean;
begin
  Result := A.Value = B.Value * 2; // Pourquoi multiplier B par 2 ?
end;

// ❌ ÉVITER : Conversions implicites dangereuses
operator := (Value: Pointer): TMyType;
begin
  Result.Data := Value^; // Déréférencement dangereux
end;
```

## Conclusion

La surcharge d'opérateurs en FreePascal/Lazarus est un outil puissant qui permet de créer du code plus expressif et naturel. Voici les points essentiels à retenir :

### Avantages de la surcharge d'opérateurs

1. **Code plus lisible** : `V3 := V1 + V2` est plus clair que `V3 := AddVectors(V1, V2)`
2. **Syntaxe naturelle** : Les types personnalisés se comportent comme les types built-in
3. **Réduction du code** : Moins de fonctions à mémoriser
4. **Expressivité** : Les formules mathématiques restent reconnaissables
5. **Généricité** : Fonctionne avec les templates et génériques

### Limitations et pièges

1. **Pas de nouveaux opérateurs** : Seulement surcharger les existants
2. **Priorité fixe** : Impossible de changer la priorité des opérateurs
3. **Performance** : Peut être plus lent que des fonctions optimisées
4. **Débogage** : Plus difficile à déboguer que des appels de fonctions
5. **Abus possible** : Peut rendre le code confus si mal utilisé

### Bonnes pratiques résumées

✅ **À FAIRE** :
- Respecter la sémantique habituelle des opérateurs
- Implémenter les opérateurs symétriques ensemble
- Gérer les cas d'erreur (division par zéro, overflow)
- Documenter le comportement non évident
- Utiliser `const` pour les paramètres
- Tester exhaustivement

❌ **À ÉVITER** :
- Comportements surprenants ou contre-intuitifs
- Effets de bord dans les opérateurs
- Conversions implicites dangereuses
- Surcharge excessive qui nuit à la clarté
- Ignorer les conventions mathématiques

### Quand utiliser la surcharge

**Utilisez la surcharge pour** :
- Types mathématiques (vecteurs, matrices, nombres complexes)
- Types monétaires et unités
- Types de conteneurs et collections
- Smart pointers et gestion mémoire
- Types domaine métier avec sémantique claire

**Évitez la surcharge pour** :
- Logique métier complexe
- Opérations avec effets de bord
- Conversions ambiguës
- Cas où une méthode serait plus claire

La surcharge d'opérateurs, utilisée judicieusement, rend votre code plus élégant et plus facile à utiliser. C'est un outil puissant qui doit être utilisé avec discernement pour améliorer, et non compliquer, votre code.

⏭️ [Anonymous methods et closures](/03-langage-object-pascal-avance/09-anonymous-methods-closures.md)
