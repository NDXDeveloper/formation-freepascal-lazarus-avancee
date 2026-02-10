🔝 Retour au [Sommaire](/SOMMAIRE.md)

# Helpers de classe et de record en FreePascal/Lazarus

## Introduction : Qu'est-ce qu'un helper ?

Un helper est comme un "assistant" qui ajoute de nouvelles capacités à une classe ou un record existant, sans modifier son code source original. Imaginez que vous avez une voiture (classe existante) et que vous voulez lui ajouter un GPS (nouvelles méthodes). Au lieu de démonter la voiture pour l'installer, vous utilisez un helper qui "colle" le GPS de l'extérieur.

Les helpers permettent d'**étendre** des types existants avec :
- De nouvelles méthodes
- De nouvelles propriétés
- De nouvelles fonctions de classe

C'est particulièrement utile pour :
- Ajouter des fonctionnalités à des classes que vous ne pouvez pas modifier (RTL, bibliothèques tierces)
- Organiser votre code par fonctionnalités
- Éviter l'héritage quand ce n'est pas approprié

## Syntaxe de base des helpers

### Helper pour une classe simple

```pascal
uses
  Classes, SysUtils;

type
  // Helper pour la classe TStringList
  TStringListHelper = class helper for TStringList
  public
    // Nouvelle méthode : ajouter plusieurs lignes d'un coup
    procedure AddLines(const Lines: array of string);

    // Nouvelle méthode : obtenir le texte inversé
    function GetReversedText: string;

    // Nouvelle propriété : première ligne
    function GetFirstLine: string;
    procedure SetFirstLine(const Value: string);
    property FirstLine: string read GetFirstLine write SetFirstLine;

    // Nouvelle propriété : dernière ligne
    function GetLastLine: string;
    procedure SetLastLine(const Value: string);
    property LastLine: string read GetLastLine write SetLastLine;
  end;

// Implémentation du helper
procedure TStringListHelper.AddLines(const Lines: array of string);  
var
  Line: string;
begin
  for Line in Lines do
    Self.Add(Line);  // Self fait référence à l'instance TStringList
end;

function TStringListHelper.GetReversedText: string;  
var
  I: Integer;
  TempList: TStringList;
begin
  TempList := TStringList.Create;
  try
    for I := Self.Count - 1 downto 0 do
      TempList.Add(Self[I]);
    Result := TempList.Text;
  finally
    TempList.Free;
  end;
end;

function TStringListHelper.GetFirstLine: string;  
begin
  if Self.Count > 0 then
    Result := Self[0]
  else
    Result := '';
end;

procedure TStringListHelper.SetFirstLine(const Value: string);  
begin
  if Self.Count > 0 then
    Self[0] := Value
  else
    Self.Add(Value);
end;

function TStringListHelper.GetLastLine: string;  
begin
  if Self.Count > 0 then
    Result := Self[Self.Count - 1]
  else
    Result := '';
end;

procedure TStringListHelper.SetLastLine(const Value: string);  
begin
  if Self.Count > 0 then
    Self[Self.Count - 1] := Value
  else
    Self.Add(Value);
end;

// Utilisation
procedure UseStringListHelper;  
var
  List: TStringList;
begin
  List := TStringList.Create;
  try
    // Utilisation de la nouvelle méthode
    List.AddLines(['Première', 'Deuxième', 'Troisième']);

    // Utilisation des nouvelles propriétés
    WriteLn('Première ligne : ', List.FirstLine);
    WriteLn('Dernière ligne : ', List.LastLine);

    List.FirstLine := 'Nouveau début';
    List.LastLine := 'Nouvelle fin';

    // Obtenir le texte inversé
    WriteLn('Texte inversé :');
    WriteLn(List.GetReversedText);
  finally
    List.Free;
  end;
end;
```

### Helper pour un record

```pascal
type
  TPoint = record
    X, Y: Integer;
  end;

  // Helper pour le record TPoint
  TPointHelper = record helper for TPoint
  public
    // Constructeur-like pour initialiser
    procedure Init(AX, AY: Integer);

    // Méthodes de calcul
    function Distance(const Other: TPoint): Double;
    function ManhattanDistance(const Other: TPoint): Integer;

    // Méthodes de transformation
    procedure Offset(DX, DY: Integer);
    procedure Rotate90;
    procedure Scale(Factor: Integer);

    // Méthodes de conversion
    function ToString: string;
    function IsZero: Boolean;
    function IsPositive: Boolean;

    // Propriétés calculées
    function GetLength: Double;
    property Length: Double read GetLength;

    function GetQuadrant: Integer;
    property Quadrant: Integer read GetQuadrant;
  end;

// Implémentation
procedure TPointHelper.Init(AX, AY: Integer);  
begin
  Self.X := AX;
  Self.Y := AY;
end;

function TPointHelper.Distance(const Other: TPoint): Double;  
var
  DX, DY: Integer;
begin
  DX := Self.X - Other.X;
  DY := Self.Y - Other.Y;
  Result := Sqrt(DX * DX + DY * DY);
end;

function TPointHelper.ManhattanDistance(const Other: TPoint): Integer;  
begin
  Result := Abs(Self.X - Other.X) + Abs(Self.Y - Other.Y);
end;

procedure TPointHelper.Offset(DX, DY: Integer);  
begin
  Inc(Self.X, DX);
  Inc(Self.Y, DY);
end;

procedure TPointHelper.Rotate90;  
var
  Temp: Integer;
begin
  Temp := Self.X;
  Self.X := -Self.Y;
  Self.Y := Temp;
end;

procedure TPointHelper.Scale(Factor: Integer);  
begin
  Self.X := Self.X * Factor;
  Self.Y := Self.Y * Factor;
end;

function TPointHelper.ToString: string;  
begin
  Result := Format('(%d, %d)', [Self.X, Self.Y]);
end;

function TPointHelper.IsZero: Boolean;  
begin
  Result := (Self.X = 0) and (Self.Y = 0);
end;

function TPointHelper.IsPositive: Boolean;  
begin
  Result := (Self.X >= 0) and (Self.Y >= 0);
end;

function TPointHelper.GetLength: Double;  
begin
  Result := Sqrt(Self.X * Self.X + Self.Y * Self.Y);
end;

function TPointHelper.GetQuadrant: Integer;  
begin
  if (X > 0) and (Y > 0) then Result := 1
  else if (X < 0) and (Y > 0) then Result := 2
  else if (X < 0) and (Y < 0) then Result := 3
  else if (X > 0) and (Y < 0) then Result := 4
  else Result := 0; // Sur un axe ou à l'origine
end;

// Utilisation
procedure UsePointHelper;  
var
  P1, P2: TPoint;
begin
  // Initialisation avec le helper
  P1.Init(3, 4);
  P2.Init(6, 8);

  WriteLn('P1 : ', P1.ToString);
  WriteLn('P2 : ', P2.ToString);

  WriteLn('Distance : ', P1.Distance(P2):0:2);
  WriteLn('Distance Manhattan : ', P1.ManhattanDistance(P2));

  WriteLn('Longueur P1 : ', P1.Length:0:2);
  WriteLn('Quadrant P1 : ', P1.Quadrant);

  // Transformation
  P1.Offset(10, 10);
  WriteLn('P1 après offset : ', P1.ToString);

  P1.Rotate90;
  WriteLn('P1 après rotation : ', P1.ToString);
end;
```

## Helpers pour types simples

### Helper pour Integer

```pascal
type
  TIntegerHelper = type helper for Integer
  public
    // Méthodes de conversion
    function ToBinary: string;
    function ToHex: string;
    function ToRoman: string;

    // Méthodes de test
    function IsEven: Boolean;
    function IsOdd: Boolean;
    function IsPrime: Boolean;
    function IsPowerOfTwo: Boolean;

    // Méthodes mathématiques
    function Factorial: Int64;
    function Power(Exponent: Integer): Int64;
    function DigitSum: Integer;
    function ReverseDigits: Integer;

    // Méthodes de limitation
    function Clamp(Min, Max: Integer): Integer;
    function WrapAround(Min, Max: Integer): Integer;
  end;

// Implémentations
function TIntegerHelper.ToBinary: string;  
var
  Value: Integer;
begin
  Result := '';
  Value := Self;
  if Value = 0 then
    Result := '0'
  else
  begin
    while Value > 0 do
    begin
      if Value and 1 = 1 then
        Result := '1' + Result
      else
        Result := '0' + Result;
      Value := Value shr 1;
    end;
  end;
end;

function TIntegerHelper.ToHex: string;  
begin
  Result := IntToHex(Self, 0);
end;

function TIntegerHelper.ToRoman: string;  
const
  Romans: array[1..13] of string =
    ('M', 'CM', 'D', 'CD', 'C', 'XC', 'L', 'XL', 'X', 'IX', 'V', 'IV', 'I');
  Values: array[1..13] of Integer =
    (1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1);
var
  I, Value: Integer;
begin
  Result := '';
  Value := Self;
  for I := 1 to 13 do
  begin
    while Value >= Values[I] do
    begin
      Result := Result + Romans[I];
      Dec(Value, Values[I]);
    end;
  end;
end;

function TIntegerHelper.IsEven: Boolean;  
begin
  Result := (Self mod 2) = 0;
end;

function TIntegerHelper.IsOdd: Boolean;  
begin
  Result := (Self mod 2) <> 0;
end;

function TIntegerHelper.IsPrime: Boolean;  
var
  I: Integer;
begin
  if Self <= 1 then
    Exit(False);
  if Self <= 3 then
    Exit(True);
  if (Self mod 2 = 0) or (Self mod 3 = 0) then
    Exit(False);

  I := 5;
  while I * I <= Self do
  begin
    if (Self mod I = 0) or (Self mod (I + 2) = 0) then
      Exit(False);
    Inc(I, 6);
  end;
  Result := True;
end;

function TIntegerHelper.IsPowerOfTwo: Boolean;  
begin
  Result := (Self > 0) and ((Self and (Self - 1)) = 0);
end;

function TIntegerHelper.Factorial: Int64;  
var
  I: Integer;
begin
  Result := 1;
  for I := 2 to Self do
    Result := Result * I;
end;

function TIntegerHelper.Power(Exponent: Integer): Int64;  
var
  I: Integer;
begin
  Result := 1;
  for I := 1 to Exponent do
    Result := Result * Self;
end;

function TIntegerHelper.DigitSum: Integer;  
var
  Value: Integer;
begin
  Result := 0;
  Value := Abs(Self);
  while Value > 0 do
  begin
    Inc(Result, Value mod 10);
    Value := Value div 10;
  end;
end;

function TIntegerHelper.ReverseDigits: Integer;  
var
  Value: Integer;
begin
  Result := 0;
  Value := Abs(Self);
  while Value > 0 do
  begin
    Result := Result * 10 + (Value mod 10);
    Value := Value div 10;
  end;
  if Self < 0 then
    Result := -Result;
end;

function TIntegerHelper.Clamp(Min, Max: Integer): Integer;  
begin
  if Self < Min then
    Result := Min
  else if Self > Max then
    Result := Max
  else
    Result := Self;
end;

function TIntegerHelper.WrapAround(Min, Max: Integer): Integer;  
var
  Range: Integer;
begin
  Range := Max - Min + 1;
  Result := Self;
  while Result < Min do
    Inc(Result, Range);
  while Result > Max do
    Dec(Result, Range);
end;

// Utilisation
procedure UseIntegerHelper;  
var
  Number: Integer;
begin
  Number := 42;

  WriteLn(Number, ' en binaire : ', Number.ToBinary);
  WriteLn(Number, ' en hexadécimal : ', Number.ToHex);
  WriteLn(Number, ' en chiffres romains : ', Number.ToRoman);

  if Number.IsEven then
    WriteLn(Number, ' est pair');

  Number := 17;
  if Number.IsPrime then
    WriteLn(Number, ' est premier');

  Number := 5;
  WriteLn(Number, '! = ', Number.Factorial);
  WriteLn(Number, '^3 = ', Number.Power(3));

  Number := 12345;
  WriteLn('Somme des chiffres de ', Number, ' = ', Number.DigitSum);
  WriteLn('Inversion de ', Number, ' = ', Number.ReverseDigits);

  Number := 150;
  WriteLn(Number, ' limité à [0..100] = ', Number.Clamp(0, 100));
end;
```

### Helper pour String

```pascal
type
  TStringHelper = type helper for string
  public
    // Méthodes de test
    function IsEmpty: Boolean;
    function IsNumeric: Boolean;
    function IsAlpha: Boolean;
    function IsAlphaNumeric: Boolean;
    function IsPalindrome: Boolean;

    // Méthodes de transformation
    function Reverse: string;
    function RemoveSpaces: string;
    function RemoveDuplicates: string;
    function Capitalize: string;
    function ToCamelCase: string;
    function ToSnakeCase: string;

    // Méthodes de recherche
    function CountOccurrences(const SubStr: string): Integer;
    function IndexOfAny(const Chars: array of Char): Integer;
    function Contains(const SubStr: string): Boolean;
    function StartsWith(const Prefix: string): Boolean;
    function EndsWith(const Suffix: string): Boolean;

    // Méthodes de manipulation
    function Left(Count: Integer): string;
    function Right(Count: Integer): string;
    function PadLeft(TotalWidth: Integer; PadChar: Char = ' '): string;
    function PadRight(TotalWidth: Integer; PadChar: Char = ' '): string;
    function Repeat(Count: Integer): string;

    // Méthodes de parsing
    function ToInteger(DefaultValue: Integer = 0): Integer;
    function ToFloat(DefaultValue: Double = 0.0): Double;
    function ToBoolean: Boolean;
  end;

// Implémentations principales
function TStringHelper.IsEmpty: Boolean;  
begin
  Result := Trim(Self) = '';
end;

function TStringHelper.IsNumeric: Boolean;  
var
  I: Integer;
  DecimalFound: Boolean;
begin
  Result := False;
  if Length(Self) = 0 then Exit;

  DecimalFound := False;
  for I := 1 to Length(Self) do
  begin
    if Self[I] = '.' then
    begin
      if DecimalFound then Exit;
      DecimalFound := True;
    end
    else if not (Self[I] in ['0'..'9', '+', '-']) then
      Exit;
  end;
  Result := True;
end;

function TStringHelper.IsAlpha: Boolean;  
var
  I: Integer;
begin
  Result := False;
  if Length(Self) = 0 then Exit;

  for I := 1 to Length(Self) do
    if not (Self[I] in ['A'..'Z', 'a'..'z']) then
      Exit;
  Result := True;
end;

function TStringHelper.IsPalindrome: Boolean;  
var
  I: Integer;
  Cleaned: string;
begin
  // Nettoyer la chaîne (enlever espaces et ponctuation)
  Cleaned := '';
  for I := 1 to Length(Self) do
    if Self[I] in ['A'..'Z', 'a'..'z', '0'..'9'] then
      Cleaned := Cleaned + UpCase(Self[I]);

  // Vérifier si c'est un palindrome
  Result := True;
  for I := 1 to Length(Cleaned) div 2 do
    if Cleaned[I] <> Cleaned[Length(Cleaned) - I + 1] then
    begin
      Result := False;
      Break;
    end;
end;

function TStringHelper.Reverse: string;  
var
  I: Integer;
begin
  Result := '';
  for I := Length(Self) downto 1 do
    Result := Result + Self[I];
end;

function TStringHelper.Capitalize: string;  
var
  I: Integer;
  NewWord: Boolean;
begin
  Result := LowerCase(Self);
  NewWord := True;
  for I := 1 to Length(Result) do
  begin
    if Result[I] in ['A'..'Z', 'a'..'z'] then
    begin
      if NewWord then
      begin
        Result[I] := UpCase(Result[I]);
        NewWord := False;
      end;
    end
    else
      NewWord := True;
  end;
end;

function TStringHelper.CountOccurrences(const SubStr: string): Integer;  
var
  P: Integer;
  S: string;
begin
  Result := 0;
  S := Self;
  P := Pos(SubStr, S);
  while P > 0 do
  begin
    Inc(Result);
    Delete(S, 1, P + Length(SubStr) - 1);
    P := Pos(SubStr, S);
  end;
end;

function TStringHelper.Contains(const SubStr: string): Boolean;  
begin
  Result := Pos(SubStr, Self) > 0;
end;

function TStringHelper.StartsWith(const Prefix: string): Boolean;  
begin
  Result := Copy(Self, 1, Length(Prefix)) = Prefix;
end;

function TStringHelper.EndsWith(const Suffix: string): Boolean;  
begin
  Result := Copy(Self, Length(Self) - Length(Suffix) + 1, Length(Suffix)) = Suffix;
end;

function TStringHelper.Left(Count: Integer): string;  
begin
  Result := Copy(Self, 1, Count);
end;

function TStringHelper.Right(Count: Integer): string;  
begin
  Result := Copy(Self, Length(Self) - Count + 1, Count);
end;

function TStringHelper.PadLeft(TotalWidth: Integer; PadChar: Char): string;  
begin
  Result := Self;
  while Length(Result) < TotalWidth do
    Result := PadChar + Result;
end;

function TStringHelper.PadRight(TotalWidth: Integer; PadChar: Char): string;  
begin
  Result := Self;
  while Length(Result) < TotalWidth do
    Result := Result + PadChar;
end;

function TStringHelper.Repeat(Count: Integer): string;  
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Count do
    Result := Result + Self;
end;

// Utilisation
procedure UseStringHelper;  
var
  S: string;
begin
  S := 'Hello World';

  WriteLn('Contains "World": ', S.Contains('World'));
  WriteLn('Starts with "Hello": ', S.StartsWith('Hello'));
  WriteLn('Reversed: ', S.Reverse);
  WriteLn('Capitalized: ', S.Capitalize);

  S := '12345';
  if S.IsNumeric then
    WriteLn(S, ' est numérique, valeur = ', S.ToInteger);

  S := 'radar';
  if S.IsPalindrome then
    WriteLn(S, ' est un palindrome');

  S := 'Test';
  WriteLn(S, ' répété 3 fois : ', S.Repeat(3));
  WriteLn(S, ' aligné à gauche : [', S.PadRight(10), ']');
  WriteLn(S, ' aligné à droite : [', S.PadLeft(10), ']');
end;
```

## Helpers pour tableaux

### Helper pour tableaux dynamiques

```pascal
type
  TIntArray = array of Integer;

  TIntArrayHelper = type helper for TIntArray
  public
    // Méthodes d'initialisation
    procedure Init(Size: Integer; DefaultValue: Integer = 0);
    procedure FromArray(const Values: array of Integer);

    // Méthodes d'ajout/suppression
    procedure Add(Value: Integer);
    procedure AddRange(const Values: array of Integer);
    procedure Insert(Index: Integer; Value: Integer);
    procedure Delete(Index: Integer);
    procedure Remove(Value: Integer);
    procedure Clear;

    // Méthodes de recherche
    function IndexOf(Value: Integer): Integer;
    function Contains(Value: Integer): Boolean;
    function Find(Predicate: TFunc<Integer, Boolean>): Integer;

    // Méthodes de tri et manipulation
    procedure Sort;
    procedure Reverse;
    procedure Shuffle;
    procedure Unique;

    // Méthodes de calcul
    function Sum: Int64;
    function Average: Double;
    function Min: Integer;
    function Max: Integer;

    // Méthodes de transformation
    function Map(Transform: TFunc<Integer, Integer>): TIntArray;
    function Filter(Predicate: TFunc<Integer, Boolean>): TIntArray;
    function Reduce(Accumulator: TFunc<Integer, Integer, Integer>;
                    Initial: Integer): Integer;

    // Conversion
    function ToString: string;
    function ToCSV: string;
  end;

// Implémentations
procedure TIntArrayHelper.Init(Size: Integer; DefaultValue: Integer);  
var
  I: Integer;
begin
  SetLength(Self, Size);
  for I := 0 to High(Self) do
    Self[I] := DefaultValue;
end;

procedure TIntArrayHelper.FromArray(const Values: array of Integer);  
var
  I: Integer;
begin
  SetLength(Self, Length(Values));
  for I := 0 to High(Values) do
    Self[I] := Values[I];
end;

procedure TIntArrayHelper.Add(Value: Integer);  
begin
  SetLength(Self, Length(Self) + 1);
  Self[High(Self)] := Value;
end;

procedure TIntArrayHelper.AddRange(const Values: array of Integer);  
var
  I, OldLen: Integer;
begin
  OldLen := Length(Self);
  SetLength(Self, OldLen + Length(Values));
  for I := 0 to High(Values) do
    Self[OldLen + I] := Values[I];
end;

procedure TIntArrayHelper.Insert(Index: Integer; Value: Integer);  
var
  I: Integer;
begin
  SetLength(Self, Length(Self) + 1);
  for I := High(Self) downto Index + 1 do
    Self[I] := Self[I - 1];
  Self[Index] := Value;
end;

procedure TIntArrayHelper.Delete(Index: Integer);  
var
  I: Integer;
begin
  if (Index >= 0) and (Index < Length(Self)) then
  begin
    for I := Index to High(Self) - 1 do
      Self[I] := Self[I + 1];
    SetLength(Self, Length(Self) - 1);
  end;
end;

procedure TIntArrayHelper.Remove(Value: Integer);  
var
  Index: Integer;
begin
  Index := IndexOf(Value);
  if Index >= 0 then
    Delete(Index);
end;

procedure TIntArrayHelper.Clear;  
begin
  SetLength(Self, 0);
end;

function TIntArrayHelper.IndexOf(Value: Integer): Integer;  
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to High(Self) do
    if Self[I] = Value then
    begin
      Result := I;
      Break;
    end;
end;

function TIntArrayHelper.Contains(Value: Integer): Boolean;  
begin
  Result := IndexOf(Value) >= 0;
end;

procedure TIntArrayHelper.Sort;  
var
  I, J, Temp: Integer;
begin
  // Simple bubble sort (pour la démo)
  for I := 0 to High(Self) - 1 do
    for J := I + 1 to High(Self) do
      if Self[I] > Self[J] then
      begin
        Temp := Self[I];
        Self[I] := Self[J];
        Self[J] := Temp;
      end;
end;

procedure TIntArrayHelper.Reverse;  
var
  I, Temp: Integer;
begin
  for I := 0 to (Length(Self) div 2) - 1 do
  begin
    Temp := Self[I];
    Self[I] := Self[High(Self) - I];
    Self[High(Self) - I] := Temp;
  end;
end;

procedure TIntArrayHelper.Shuffle;  
var
  I, J, Temp: Integer;
begin
  Randomize;
  for I := High(Self) downto 1 do
  begin
    J := Random(I + 1);
    Temp := Self[I];
    Self[I] := Self[J];
    Self[J] := Temp;
  end;
end;

function TIntArrayHelper.Sum: Int64;  
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to High(Self) do
    Result := Result + Self[I];
end;

function TIntArrayHelper.Average: Double;  
begin
  if Length(Self) > 0 then
    Result := Sum / Length(Self)
  else
    Result := 0;
end;

function TIntArrayHelper.Min: Integer;  
var
  I: Integer;
begin
  if Length(Self) = 0 then
    Exit(0);
  Result := Self[0];
  for I := 1 to High(Self) do
    if Self[I] < Result then
      Result := Self[I];
end;

function TIntArrayHelper.Max: Integer;  
var
  I: Integer;
begin
  if Length(Self) = 0 then
    Exit(0);
  Result := Self[0];
  for I := 1 to High(Self) do
    if Self[I] > Result then
      Result := Self[I];
end;

function TIntArrayHelper.ToString: string;  
var
  I: Integer;
begin
  Result := '[';
  for I := 0 to High(Self) do
  begin
    if I > 0 then
      Result := Result + ', ';
    Result := Result + IntToStr(Self[I]);
  end;
  Result := Result + ']';
end;

// Utilisation
procedure UseIntArrayHelper;  
var
  Arr: TIntArray;
begin
  // Initialisation
  Arr.FromArray([5, 2, 8, 1, 9, 3]);
  WriteLn('Tableau initial : ', Arr.ToString);

  // Ajout d'éléments
  Arr.Add(7);
  Arr.AddRange([4, 6]);
  WriteLn('Après ajouts : ', Arr.ToString);

  // Tri et calculs
  Arr.Sort;
  WriteLn('Trié : ', Arr.ToString);
  WriteLn('Somme : ', Arr.Sum);
  WriteLn('Moyenne : ', Arr.Average:0:2);
  WriteLn('Min : ', Arr.Min, ', Max : ', Arr.Max);

  // Manipulation
  Arr.Reverse;
  WriteLn('Inversé : ', Arr.ToString);

  Arr.Shuffle;
  WriteLn('Mélangé : ', Arr.ToString);

  // Recherche
  if Arr.Contains(5) then
    WriteLn('Le tableau contient 5 à l''index ', Arr.IndexOf(5));
end;
```

## Helpers avancés et héritage

### Chaîne d'helpers

```pascal
type
  // Classe de base
  TAnimal = class
  protected
    FName: string;
  public
    constructor Create(const AName: string);
    property Name: string read FName;
  end;

  // Classe dérivée
  TDog = class(TAnimal)
  private
    FBreed: string;
  public
    property Breed: string read FBreed write FBreed;
  end;

  // Helper pour la classe de base
  TAnimalHelper = class helper for TAnimal
  public
    function GetDescription: string;
    procedure Speak; virtual;
  end;

  // Helper pour la classe dérivée
  TDogHelper = class helper(TAnimalHelper) for TDog
  public
    procedure Speak; override;
    procedure Bark;
    function GetFullDescription: string;
  end;

// Implémentations
constructor TAnimal.Create(const AName: string);  
begin
  FName := AName;
end;

function TAnimalHelper.GetDescription: string;  
begin
  Result := Format('Animal nommé %s', [Self.Name]);
end;

procedure TAnimalHelper.Speak;  
begin
  WriteLn(Self.Name, ' fait un bruit');
end;

procedure TDogHelper.Speak;  
begin
  WriteLn(Self.Name, ' aboie');
end;

procedure TDogHelper.Bark;  
begin
  WriteLn('Wouaf! Wouaf!');
end;

function TDogHelper.GetFullDescription: string;  
begin
  Result := Format('Chien nommé %s, race : %s', [Self.Name, Self.Breed]);
end;

// Utilisation des helpers avec héritage
procedure UseAnimalHelpers;  
var
  Animal: TAnimal;
  Dog: TDog;
begin
  Animal := TAnimal.Create('Félix');
  try
    WriteLn(Animal.GetDescription);
    Animal.Speak;
  finally
    Animal.Free;
  end;

  Dog := TDog.Create('Rex');
  try
    Dog.Breed := 'Berger Allemand';
    WriteLn(Dog.GetFullDescription);
    Dog.Speak;  // Utilise la version override
    Dog.Bark;   // Méthode spécifique au helper TDog
  finally
    Dog.Free;
  end;
end;
```

## Helpers avec génériques

### Helper générique pour listes

```pascal
uses
  Generics.Collections;

type
  // Helper pour TList<T> générique
  TListHelper<T> = class helper for TList<T>
  public
    // Méthodes de recherche avancée
    function FindAll(const Match: TPredicate<T>): TList<T>;
    function FindFirst(const Match: TPredicate<T>; out Item: T): Boolean;
    function FindLast(const Match: TPredicate<T>; out Item: T): Boolean;

    // Méthodes de transformation
    procedure ForEach(const Action: TProc<T>);
    function Map<TResult>(const Transform: TFunc<T, TResult>): TList<TResult>;
    function Where(const Predicate: TPredicate<T>): TList<T>;

    // Méthodes utilitaires
    function ToArray: TArray<T>;
    procedure Shuffle;
    procedure RemoveDuplicates;
    function GetRandom: T;

    // Méthodes d'agrégation
    function All(const Predicate: TPredicate<T>): Boolean;
    function Any(const Predicate: TPredicate<T>): Boolean;
  end;

// Implémentations
function TListHelper<T>.FindAll(const Match: TPredicate<T>): TList<T>;  
var
  Item: T;
begin
  Result := TList<T>.Create;
  for Item in Self do
    if Match(Item) then
      Result.Add(Item);
end;

function TListHelper<T>.FindFirst(const Match: TPredicate<T>; out Item: T): Boolean;  
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Self.Count - 1 do
    if Match(Self[I]) then
    begin
      Item := Self[I];
      Result := True;
      Exit;
    end;
end;

function TListHelper<T>.FindLast(const Match: TPredicate<T>; out Item: T): Boolean;  
var
  I: Integer;
begin
  Result := False;
  for I := Self.Count - 1 downto 0 do
    if Match(Self[I]) then
    begin
      Item := Self[I];
      Result := True;
      Exit;
    end;
end;

procedure TListHelper<T>.ForEach(const Action: TProc<T>);  
var
  Item: T;
begin
  for Item in Self do
    Action(Item);
end;

function TListHelper<T>.Map<TResult>(const Transform: TFunc<T, TResult>): TList<TResult>;  
var
  Item: T;
begin
  Result := TList<TResult>.Create;
  for Item in Self do
    Result.Add(Transform(Item));
end;

function TListHelper<T>.Where(const Predicate: TPredicate<T>): TList<T>;  
begin
  Result := FindAll(Predicate);
end;

function TListHelper<T>.ToArray: TArray<T>;  
var
  I: Integer;
begin
  SetLength(Result, Self.Count);
  for I := 0 to Self.Count - 1 do
    Result[I] := Self[I];
end;

procedure TListHelper<T>.Shuffle;  
var
  I, J: Integer;
  Temp: T;
begin
  Randomize;
  for I := Self.Count - 1 downto 1 do
  begin
    J := Random(I + 1);
    Temp := Self[I];
    Self[I] := Self[J];
    Self[J] := Temp;
  end;
end;

procedure TListHelper<T>.RemoveDuplicates;  
var
  UniqueList: TList<T>;
  Item: T;
begin
  UniqueList := TList<T>.Create;
  try
    for Item in Self do
      if not UniqueList.Contains(Item) then
        UniqueList.Add(Item);

    Self.Clear;
    Self.AddRange(UniqueList);
  finally
    UniqueList.Free;
  end;
end;

function TListHelper<T>.GetRandom: T;  
begin
  if Self.Count > 0 then
    Result := Self[Random(Self.Count)]
  else
    Result := Default(T);
end;

function TListHelper<T>.All(const Predicate: TPredicate<T>): Boolean;  
var
  Item: T;
begin
  Result := True;
  for Item in Self do
    if not Predicate(Item) then
    begin
      Result := False;
      Exit;
    end;
end;

function TListHelper<T>.Any(const Predicate: TPredicate<T>): Boolean;  
var
  Item: T;
begin
  Result := False;
  for Item in Self do
    if Predicate(Item) then
    begin
      Result := True;
      Exit;
    end;
end;

// Utilisation du helper générique
type
  TPerson = class
    Name: string;
    Age: Integer;
    constructor Create(const AName: string; AAge: Integer);
  end;

constructor TPerson.Create(const AName: string; AAge: Integer);  
begin
  Name := AName;
  Age := AAge;
end;

procedure UseGenericListHelper;  
var
  People: TList<TPerson>;
  Adults: TList<TPerson>;
  Person: TPerson;
  Names: TList<string>;
begin
  People := TList<TPerson>.Create;
  try
    // Ajout de personnes
    People.Add(TPerson.Create('Alice', 25));
    People.Add(TPerson.Create('Bob', 17));
    People.Add(TPerson.Create('Charlie', 30));
    People.Add(TPerson.Create('David', 16));

    // Utilisation du helper - Filtrer les adultes
    Adults := People.Where(
      function(P: TPerson): Boolean
      begin
        Result := P.Age >= 18;
      end
    );

    WriteLn('Adultes trouvés :');
    Adults.ForEach(
      procedure(P: TPerson)
      begin
        WriteLn('  ', P.Name, ' (', P.Age, ' ans)');
      end
    );

    // Mapper vers les noms
    Names := People.Map<string>(
      function(P: TPerson): string
      begin
        Result := P.Name;
      end
    );

    // Vérifier si tous ont plus de 15 ans
    if People.All(function(P: TPerson): Boolean
                   begin Result := P.Age > 15; end) then
      WriteLn('Tous ont plus de 15 ans');

    // Vérifier s''il y a au moins un mineur
    if People.Any(function(P: TPerson): Boolean
                  begin Result := P.Age < 18; end) then
      WriteLn('Il y a au moins un mineur');

  finally
    // Nettoyer
    for Person in People do
      Person.Free;
    People.Free;
    Adults.Free;
    Names.Free;
  end;
end;
```

## Helpers pour interfaces

### Helper pour une interface

```pascal
type
  // Interface de base
  IShape = interface
    ['{12345678-1234-1234-1234-123456789012}']
    function GetArea: Double;
    function GetPerimeter: Double;
    property Area: Double read GetArea;
    property Perimeter: Double read GetPerimeter;
  end;

  // Helper pour l'interface
  IShapeHelper = interface helper for IShape
    function GetDescription: string;
    function IsLargerThan(Other: IShape): Boolean;
    function GetRatio: Double;  // Ratio périmètre/aire
  end;

  // Implémentation du helper
  function IShapeHelper.GetDescription: string;
  begin
    Result := Format('Forme avec aire=%.2f et périmètre=%.2f',
                     [Self.Area, Self.Perimeter]);
  end;

  function IShapeHelper.IsLargerThan(Other: IShape): Boolean;
  begin
    Result := Self.Area > Other.Area;
  end;

  function IShapeHelper.GetRatio: Double;
  begin
    if Self.Area > 0 then
      Result := Self.Perimeter / Self.Area
    else
      Result := 0;
  end;

// Classes implémentant l'interface
type
  TCircle = class(TInterfacedObject, IShape)
  private
    FRadius: Double;
  public
    constructor Create(ARadius: Double);
    function GetArea: Double;
    function GetPerimeter: Double;
  end;

  TSquare = class(TInterfacedObject, IShape)
  private
    FSide: Double;
  public
    constructor Create(ASide: Double);
    function GetArea: Double;
    function GetPerimeter: Double;
  end;

// Implémentations
constructor TCircle.Create(ARadius: Double);  
begin
  FRadius := ARadius;
end;

function TCircle.GetArea: Double;  
begin
  Result := Pi * FRadius * FRadius;
end;

function TCircle.GetPerimeter: Double;  
begin
  Result := 2 * Pi * FRadius;
end;

constructor TSquare.Create(ASide: Double);  
begin
  FSide := ASide;
end;

function TSquare.GetArea: Double;  
begin
  Result := FSide * FSide;
end;

function TSquare.GetPerimeter: Double;  
begin
  Result := 4 * FSide;
end;

// Utilisation
procedure UseInterfaceHelper;  
var
  Circle: IShape;
  Square: IShape;
begin
  Circle := TCircle.Create(5);
  Square := TSquare.Create(4);

  WriteLn('Cercle : ', Circle.GetDescription);
  WriteLn('Carré : ', Square.GetDescription);

  if Circle.IsLargerThan(Square) then
    WriteLn('Le cercle est plus grand')
  else
    WriteLn('Le carré est plus grand');

  WriteLn('Ratio cercle : ', Circle.GetRatio:0:2);
  WriteLn('Ratio carré : ', Square.GetRatio:0:2);
end;
```

## Helpers et propriétés indexées

### Helper avec propriétés indexées

```pascal
type
  TMatrix = class
  private
    FData: array of array of Double;
    FRows, FCols: Integer;
  public
    constructor Create(ARows, ACols: Integer);
    property Rows: Integer read FRows;
    property Cols: Integer read FCols;
  end;

  TMatrixHelper = class helper for TMatrix
  private
    function GetElement(Row, Col: Integer): Double;
    procedure SetElement(Row, Col: Integer; Value: Double);
    function GetRow(Index: Integer): TArray<Double>;
    procedure SetRow(Index: Integer; const Values: TArray<Double>);
    function GetColumn(Index: Integer): TArray<Double>;
    procedure SetColumn(Index: Integer; const Values: TArray<Double>);
  public
    // Propriété indexée pour accès aux éléments
    property Elements[Row, Col: Integer]: Double
      read GetElement write SetElement; default;

    // Propriétés indexées pour lignes et colonnes
    property Row[Index: Integer]: TArray<Double>
      read GetRow write SetRow;
    property Column[Index: Integer]: TArray<Double>
      read GetColumn write SetColumn;

    // Méthodes matricielles
    procedure Fill(Value: Double);
    procedure Identity;
    function Transpose: TMatrix;
    function Add(Other: TMatrix): TMatrix;
    function Multiply(Scalar: Double): TMatrix;
    function ToString: string;
  end;

// Implémentations
constructor TMatrix.Create(ARows, ACols: Integer);  
begin
  FRows := ARows;
  FCols := ACols;
  SetLength(FData, FRows, FCols);
end;

function TMatrixHelper.GetElement(Row, Col: Integer): Double;  
begin
  Result := FData[Row, Col];
end;

procedure TMatrixHelper.SetElement(Row, Col: Integer; Value: Double);  
begin
  FData[Row, Col] := Value;
end;

function TMatrixHelper.GetRow(Index: Integer): TArray<Double>;  
var
  J: Integer;
begin
  SetLength(Result, FCols);
  for J := 0 to FCols - 1 do
    Result[J] := FData[Index, J];
end;

procedure TMatrixHelper.SetRow(Index: Integer; const Values: TArray<Double>);  
var
  J: Integer;
begin
  for J := 0 to Min(FCols - 1, High(Values)) do
    FData[Index, J] := Values[J];
end;

function TMatrixHelper.GetColumn(Index: Integer): TArray<Double>;  
var
  I: Integer;
begin
  SetLength(Result, FRows);
  for I := 0 to FRows - 1 do
    Result[I] := FData[I, Index];
end;

procedure TMatrixHelper.SetColumn(Index: Integer; const Values: TArray<Double>);  
var
  I: Integer;
begin
  for I := 0 to Min(FRows - 1, High(Values)) do
    FData[I, Index] := Values[I];
end;

procedure TMatrixHelper.Fill(Value: Double);  
var
  I, J: Integer;
begin
  for I := 0 to FRows - 1 do
    for J := 0 to FCols - 1 do
      FData[I, J] := Value;
end;

procedure TMatrixHelper.Identity;  
var
  I, J: Integer;
begin
  Fill(0);
  for I := 0 to Min(FRows, FCols) - 1 do
    FData[I, I] := 1;
end;

function TMatrixHelper.Transpose: TMatrix;  
var
  I, J: Integer;
begin
  Result := TMatrix.Create(FCols, FRows);
  for I := 0 to FRows - 1 do
    for J := 0 to FCols - 1 do
      Result[J, I] := Self[I, J];
end;

function TMatrixHelper.ToString: string;  
var
  I, J: Integer;
begin
  Result := '';
  for I := 0 to FRows - 1 do
  begin
    if I > 0 then
      Result := Result + sLineBreak;
    Result := Result + '[';
    for J := 0 to FCols - 1 do
    begin
      if J > 0 then
        Result := Result + ', ';
      Result := Result + Format('%6.2f', [FData[I, J]]);
    end;
    Result := Result + ']';
  end;
end;

// Utilisation
procedure UseMatrixHelper;  
var
  M1, M2: TMatrix;
begin
  M1 := TMatrix.Create(3, 3);
  try
    // Utilisation de la propriété indexée par défaut
    M1[0, 0] := 1; M1[0, 1] := 2; M1[0, 2] := 3;
    M1[1, 0] := 4; M1[1, 1] := 5; M1[1, 2] := 6;
    M1[2, 0] := 7; M1[2, 1] := 8; M1[2, 2] := 9;

    WriteLn('Matrice originale :');
    WriteLn(M1.ToString);

    // Accès aux lignes et colonnes
    WriteLn('Première ligne : ', M1.Row[0][0]:0:0, ', ',
            M1.Row[0][1]:0:0, ', ', M1.Row[0][2]:0:0);

    // Créer une matrice identité
    M2 := TMatrix.Create(3, 3);
    M2.Identity;
    WriteLn('Matrice identité :');
    WriteLn(M2.ToString);

    // Transposer
    M2 := M1.Transpose;
    WriteLn('Matrice transposée :');
    WriteLn(M2.ToString);
  finally
    M1.Free;
    M2.Free;
  end;
end;
```

## Limitations et considérations

### Règles importantes des helpers

```pascal
// 1. Un seul helper actif par type
type
  TMyClass = class
    Value: Integer;
  end;

  // Premier helper
  TMyClassHelper1 = class helper for TMyClass
    procedure Method1;
  end;

  // Deuxième helper (masque le premier!)
  TMyClassHelper2 = class helper for TMyClass
    procedure Method2;
  end;

// Seul TMyClassHelper2 est actif ici
procedure TestHelperScope;  
var
  Obj: TMyClass;
begin
  Obj := TMyClass.Create;
  try
    // Obj.Method1; // ERREUR : Method1 n'est pas accessible
    Obj.Method2;    // OK : Method2 est accessible
  finally
    Obj.Free;
  end;
end;

// 2. Les helpers ne peuvent pas ajouter de champs
type
  // ❌ INCORRECT
  TBadHelper = class helper for TStringList
    // FNewField: Integer; // ERREUR : Les helpers ne peuvent pas avoir de champs
  end;

  // ✅ CORRECT - Utiliser des méthodes et propriétés
  TGoodHelper = class helper for TStringList
  private
    function GetItemCount: Integer;
  public
    property ItemCount: Integer read GetItemCount;
  end;

function TGoodHelper.GetItemCount: Integer;  
begin
  Result := Self.Count; // Utilise les champs existants
end;

// 3. Les helpers ne peuvent pas override de méthodes virtuelles
type
  TBase = class
    procedure VirtualMethod; virtual;
  end;

  TBaseHelper = class helper for TBase
    // procedure VirtualMethod; override; // ERREUR : Impossible
    procedure NewMethod; // OK : Nouvelle méthode
  end;
```

### Portée et visibilité

```pascal
unit Unit1;

interface

type
  TMyClass = class
  end;

  TMyClassHelper = class helper for TMyClass
    procedure PublicMethod;
  end;

implementation

procedure TMyClassHelper.PublicMethod;  
begin
  WriteLn('Méthode publique');
end;

end.

// Dans une autre unité
unit Unit2;

interface

uses Unit1;

// Pour utiliser le helper, il faut que l'unité soit dans le uses
procedure UseHelper;

implementation

procedure UseHelper;  
var
  Obj: TMyClass;
begin
  Obj := TMyClass.Create;
  try
    Obj.PublicMethod; // OK si Unit1 est dans uses
  finally
    Obj.Free;
  end;
end;

end.
```

### Helpers et performances

```pascal
type
  TPerformanceTest = class
    Value: Integer;
  end;

  TPerformanceHelper = class helper for TPerformanceTest
    // Les méthodes inline améliorent les performances
    function GetDouble: Integer; inline;
    procedure SetDouble(AValue: Integer); inline;

    // Éviter les allocations dans les helpers
    function GetDescription: string;
  end;

function TPerformanceHelper.GetDouble: Integer;  
begin
  Result := Self.Value * 2; // Inline = pas d'appel de fonction
end;

procedure TPerformanceHelper.SetDouble(AValue: Integer);  
begin
  Self.Value := AValue div 2;
end;

function TPerformanceHelper.GetDescription: string;  
begin
  // Éviter les allocations répétées
  Result := Format('Value = %d', [Self.Value]);
  // Ne pas créer d'objets temporaires si possible
end;
```

## Cas d'usage pratiques

### Extension de composants VCL/LCL

```pascal
uses
  Forms, StdCtrls, Controls;

type
  // Helper pour TForm
  TFormHelper = class helper for TForm
  public
    procedure CenterOnScreen;
    procedure FadeIn(Duration: Integer = 500);
    procedure FadeOut(Duration: Integer = 500);
    function FindComponentByName<T: TComponent>(const Name: string): T;
    procedure EnableAllControls(Enable: Boolean);
  end;

  // Helper pour TEdit
  TEditHelper = class helper for TEdit
  public
    function IsEmpty: Boolean;
    function IsValidEmail: Boolean;
    function IsValidNumber: Boolean;
    procedure SelectAll;
    procedure ClearIfDefault(const DefaultText: string);
  end;

// Implémentations
procedure TFormHelper.CenterOnScreen;  
begin
  Self.Left := (Screen.Width - Self.Width) div 2;
  Self.Top := (Screen.Height - Self.Height) div 2;
end;

procedure TFormHelper.FadeIn(Duration: Integer);  
var
  I: Integer;
  Steps: Integer;
begin
  Steps := Duration div 10;
  Self.AlphaBlend := True;
  for I := 0 to Steps do
  begin
    Self.AlphaBlendValue := Round((255 * I) / Steps);
    Application.ProcessMessages;
    Sleep(10);
  end;
  Self.AlphaBlendValue := 255;
end;

function TFormHelper.FindComponentByName<T>(const Name: string): T;  
var
  Comp: TComponent;
begin
  Result := nil;
  Comp := Self.FindComponent(Name);
  if Comp is T then
    Result := T(Comp);
end;

procedure TFormHelper.EnableAllControls(Enable: Boolean);  
var
  I: Integer;
begin
  for I := 0 to Self.ComponentCount - 1 do
    if Self.Components[I] is TControl then
      TControl(Self.Components[I]).Enabled := Enable;
end;

function TEditHelper.IsEmpty: Boolean;  
begin
  Result := Trim(Self.Text) = '';
end;

function TEditHelper.IsValidEmail: Boolean;  
var
  AtPos, DotPos: Integer;
begin
  AtPos := Pos('@', Self.Text);
  DotPos := LastDelimiter('.', Self.Text);
  Result := (AtPos > 1) and (DotPos > AtPos + 1) and
            (DotPos < Length(Self.Text));
end;

procedure TEditHelper.SelectAll;  
begin
  Self.SelectAll;
  Self.SetFocus;
end;

// Utilisation dans un formulaire
procedure TMainForm.FormCreate(Sender: TObject);  
begin
  Self.CenterOnScreen;
  Self.FadeIn;

  if EditEmail.IsEmpty then
    ShowMessage('Veuillez entrer un email');

  if not EditEmail.IsValidEmail then
    ShowMessage('Email invalide');
end;
```

### Helper pour la journalisation

```pascal
type
  TLogLevel = (llDebug, llInfo, llWarning, llError);

  TLogHelper = class helper for TObject
  private
    class var FLogFile: TextFile;
    class var FLogOpened: Boolean;
  public
    class constructor Create;
    class destructor Destroy;

    procedure LogDebug(const Msg: string);
    procedure LogInfo(const Msg: string);
    procedure LogWarning(const Msg: string);
    procedure LogError(const Msg: string);
    procedure Log(Level: TLogLevel; const Msg: string);
  end;

class constructor TLogHelper.Create;  
begin
  FLogOpened := False;
end;

class destructor TLogHelper.Destroy;  
begin
  if FLogOpened then
    CloseFile(FLogFile);
end;

procedure TLogHelper.Log(Level: TLogLevel; const Msg: string);  
const
  LevelStr: array[TLogLevel] of string =
    ('DEBUG', 'INFO', 'WARNING', 'ERROR');
var
  LogLine: string;
begin
  if not FLogOpened then
  begin
    AssignFile(FLogFile, 'application.log');
    if FileExists('application.log') then
      Append(FLogFile)
    else
      Rewrite(FLogFile);
    FLogOpened := True;
  end;

  LogLine := Format('[%s] %s - %s: %s',
    [DateTimeToStr(Now), LevelStr[Level], Self.ClassName, Msg]);
  WriteLn(FLogFile, LogLine);
  Flush(FLogFile);
end;

procedure TLogHelper.LogDebug(const Msg: string);  
begin
  Log(llDebug, Msg);
end;

procedure TLogHelper.LogInfo(const Msg: string);  
begin
  Log(llInfo, Msg);
end;

// Utilisation
type
  TMyService = class
    procedure DoWork;
  end;

procedure TMyService.DoWork;  
begin
  Self.LogInfo('Début du traitement');
  try
    // Travail...
    Self.LogDebug('Étape 1 terminée');
  except
    on E: Exception do
      Self.LogError('Erreur : ' + E.Message);
  end;
  Self.LogInfo('Fin du traitement');
end;
```

## Conclusion

Les helpers de classe et de record sont des outils puissants en FreePascal/Lazarus qui permettent d'étendre des types existants sans modification ni héritage. Voici les points clés à retenir :

### Avantages des helpers

1. **Extension sans modification** : Ajouter des fonctionnalités à des classes fermées
2. **Organisation du code** : Séparer les fonctionnalités par domaine
3. **Syntaxe fluide** : Méthodes chainables et code plus lisible
4. **Réutilisabilité** : Partager des fonctionnalités entre projets
5. **Compatibilité** : Étendre des types de bibliothèques tierces

### Limitations à connaître

1. **Un seul helper actif** : Le dernier helper déclaré masque les autres
2. **Pas de champs** : Impossibilité d'ajouter des données d'instance
3. **Pas d'override** : Ne peut pas redéfinir les méthodes virtuelles
4. **Portée limitée** : Doit être dans le scope pour être utilisé
5. **Performance** : Léger overhead possible (utiliser inline si nécessaire)

### Bonnes pratiques

1. **Nommage clair** : Utiliser le suffixe "Helper" (TStringHelper, TListHelper)
2. **Documentation** : Documenter les helpers et leurs méthodes
3. **Cohérence** : Grouper les méthodes liées dans le même helper
4. **Simplicité** : Éviter la complexité excessive dans les helpers
5. **Tests** : Tester les helpers comme tout autre code

### Quand utiliser les helpers

✅ **Utilisez les helpers pour** :
- Ajouter des méthodes utilitaires à des types standards
- Créer des API fluides et expressives
- Étendre des classes de frameworks
- Simplifier du code répétitif
- Améliorer la lisibilité

❌ **Évitez les helpers pour** :
- Logique métier complexe
- Stocker des états (utilisez des classes)
- Remplacer l'héritage quand il est approprié
- Modifier le comportement fondamental d'une classe

Les helpers sont un excellent moyen d'améliorer l'expressivité de votre code tout en gardant une séparation claire des responsabilités. Utilisés judicieusement, ils rendent le code plus agréable à écrire et à lire.

⏭️ [Opérateurs avancés et surcharge](/03-langage-object-pascal-avance/08-operateurs-avances-surcharge.md)
