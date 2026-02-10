🔝 Retour au [Sommaire](/SOMMAIRE.md)

# Types avancés et RTTI (Run-Time Type Information) en FreePascal/Lazarus

## Introduction : Qu'est-ce que le RTTI ?

Le RTTI (Run-Time Type Information) est un mécanisme qui permet à votre programme d'obtenir des informations sur les types de données pendant son exécution. C'est comme avoir un "inspecteur" qui peut examiner vos objets et vous dire de quoi ils sont faits, quelles propriétés ils ont, et quelles méthodes ils peuvent exécuter.

Imaginez que vous recevez une boîte fermée. Sans l'ouvrir, le RTTI vous permet de savoir ce qu'elle contient, sa taille, son type, et même comment l'utiliser. C'est particulièrement utile pour créer des programmes flexibles qui peuvent s'adapter à différents types d'objets.

## Types avancés en FreePascal

Avant d'explorer le RTTI, comprenons les types avancés que FreePascal propose et qui peuvent être inspectés à l'exécution.

### Types énumérés avec valeurs personnalisées

```pascal
type
  // Énumération simple
  TDayOfWeek = (Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday);

  // Énumération avec valeurs explicites
  TErrorCode = (
    ecNone = 0,
    ecFileNotFound = 100,
    ecAccessDenied = 101,
    ecInvalidFormat = 200
  );

  // Ensemble basé sur une énumération
  TWeekDays = set of TDayOfWeek;
```

### Types variants

Les variants peuvent contenir différents types de données et changer de type pendant l'exécution :

```pascal
uses
  Variants;

var
  V: Variant;
begin
  V := 42;           // Contient un entier
  WriteLn(V);

  V := 'Bonjour';    // Maintenant contient une chaîne
  WriteLn(V);

  V := 3.14;         // Maintenant contient un réel
  WriteLn(V);

  // Vérifier le type actuel
  if VarIsStr(V) then
    WriteLn('V contient une chaîne')
  else if VarIsFloat(V) then
    WriteLn('V contient un nombre réel');
end;
```

### Types tableaux ouverts et dynamiques

```pascal
type
  // Tableau dynamique typé
  TIntArray = array of Integer;

  // Tableau ouvert (paramètre de procédure)
  procedure ProcessArray(const Values: array of Integer);
  var
    I: Integer;
  begin
    WriteLn('Le tableau contient ', Length(Values), ' éléments');
    for I := 0 to High(Values) do
      WriteLn('Element ', I, ': ', Values[I]);
  end;

var
  DynArray: TIntArray;
begin
  SetLength(DynArray, 3);
  DynArray[0] := 10;
  DynArray[1] := 20;
  DynArray[2] := 30;

  ProcessArray(DynArray);
  ProcessArray([1, 2, 3, 4, 5]); // Tableau constant
end;
```

### Types procéduraux avancés

```pascal
type
  // Type procédure simple
  TSimpleProc = procedure;

  // Type procédure avec paramètres
  TCalculation = function(A, B: Integer): Integer;

  // Type méthode d'objet
  TNotifyEvent = procedure(Sender: TObject) of object;

  // Type référence de méthode (anonyme)
  TAnonymousFunc = reference to function(Value: Integer): String;

// Utilisation
procedure ExecuteCallback(Callback: TCalculation);  
begin
  WriteLn('Résultat: ', Callback(10, 5));
end;

function Add(A, B: Integer): Integer;  
begin
  Result := A + B;
end;

function Multiply(A, B: Integer): Integer;  
begin
  Result := A * B;
end;

begin
  ExecuteCallback(@Add);      // Affiche 15
  ExecuteCallback(@Multiply); // Affiche 50
end;
```

## Les bases du RTTI

### Obtenir les informations de type

FreePascal utilise l'unité `TypInfo` pour accéder aux informations RTTI :

```pascal
uses
  TypInfo;

type
  TPerson = class
  private
    FName: String;
    FAge: Integer;
  published  // Les propriétés published sont accessibles via RTTI
    property Name: String read FName write FName;
    property Age: Integer read FAge write FAge;
  end;

var
  TypeData: PTypeData;
  TypeInfo: PTypeInfo;
begin
  // Obtenir les informations de type
  TypeInfo := TPerson.ClassInfo;
  TypeData := GetTypeData(TypeInfo);

  WriteLn('Nom de la classe: ', TypeInfo^.Name);
  WriteLn('Taille de l''instance: ', TypeData^.ClassType.InstanceSize, ' octets');
end;
```

### Types d'informations RTTI disponibles

```pascal
uses
  TypInfo;

procedure ShowTypeInfo(ATypeInfo: PTypeInfo);  
begin
  WriteLn('Nom du type: ', ATypeInfo^.Name);

  case ATypeInfo^.Kind of
    tkInteger:     WriteLn('C''est un entier');
    tkChar:        WriteLn('C''est un caractère');
    tkEnumeration: WriteLn('C''est une énumération');
    tkFloat:       WriteLn('C''est un nombre réel');
    tkString:      WriteLn('C''est une chaîne courte');
    tkSet:         WriteLn('C''est un ensemble');
    tkClass:       WriteLn('C''est une classe');
    tkMethod:      WriteLn('C''est une méthode');
    tkWChar:       WriteLn('C''est un caractère large');
    tkLString:     WriteLn('C''est une chaîne longue');
    tkWString:     WriteLn('C''est une chaîne large');
    tkVariant:     WriteLn('C''est un variant');
    tkArray:       WriteLn('C''est un tableau');
    tkRecord:      WriteLn('C''est un record');
    tkInterface:   WriteLn('C''est une interface');
    tkInt64:       WriteLn('C''est un Int64');
    tkDynArray:    WriteLn('C''est un tableau dynamique');
    tkUString:     WriteLn('C''est une chaîne Unicode');
  end;
end;

// Utilisation
begin
  ShowTypeInfo(TypeInfo(Integer));
  ShowTypeInfo(TypeInfo(String));
  ShowTypeInfo(TypeInfo(TPerson));
end;
```

## Accès aux propriétés via RTTI

### Lire et écrire des propriétés dynamiquement

```pascal
uses
  TypInfo;

type
  TProduct = class
  private
    FName: String;
    FPrice: Double;
    FQuantity: Integer;
  published
    property Name: String read FName write FName;
    property Price: Double read FPrice write FPrice;
    property Quantity: Integer read FQuantity write FQuantity;
  end;

procedure SetPropertyValue(Obj: TObject; const PropName: String; const Value: Variant);  
var
  PropInfo: PPropInfo;
begin
  PropInfo := GetPropInfo(Obj, PropName);
  if Assigned(PropInfo) then
  begin
    case PropInfo^.PropType^.Kind of
      tkInteger, tkInt64:
        SetOrdProp(Obj, PropInfo, Value);
      tkFloat:
        SetFloatProp(Obj, PropInfo, Value);
      tkString, tkLString, tkWString, tkUString:
        SetStrProp(Obj, PropInfo, Value);
    end;
  end
  else
    WriteLn('Propriété "', PropName, '" non trouvée');
end;

function GetPropertyValue(Obj: TObject; const PropName: String): Variant;  
var
  PropInfo: PPropInfo;
begin
  Result := Null;
  PropInfo := GetPropInfo(Obj, PropName);
  if Assigned(PropInfo) then
  begin
    case PropInfo^.PropType^.Kind of
      tkInteger, tkInt64:
        Result := GetOrdProp(Obj, PropInfo);
      tkFloat:
        Result := GetFloatProp(Obj, PropInfo);
      tkString, tkLString, tkWString, tkUString:
        Result := GetStrProp(Obj, PropInfo);
    end;
  end;
end;

// Utilisation
var
  Product: TProduct;
begin
  Product := TProduct.Create;
  try
    // Définir les valeurs dynamiquement
    SetPropertyValue(Product, 'Name', 'Ordinateur');
    SetPropertyValue(Product, 'Price', 999.99);
    SetPropertyValue(Product, 'Quantity', 5);

    // Lire les valeurs dynamiquement
    WriteLn('Produit: ', GetPropertyValue(Product, 'Name'));
    WriteLn('Prix: ', GetPropertyValue(Product, 'Price'):0:2);
    WriteLn('Quantité: ', GetPropertyValue(Product, 'Quantity'));
  finally
    Product.Free;
  end;
end;
```

### Énumérer toutes les propriétés d'un objet

```pascal
uses
  TypInfo;

procedure ListProperties(Obj: TObject);  
var
  TypeInfo: PTypeInfo;
  TypeData: PTypeData;
  PropList: PPropList;
  PropCount: Integer;
  I: Integer;
begin
  TypeInfo := Obj.ClassInfo;
  TypeData := GetTypeData(TypeInfo);
  PropCount := TypeData^.PropCount;

  if PropCount > 0 then
  begin
    GetMem(PropList, PropCount * SizeOf(PPropInfo));
    try
      GetPropInfos(TypeInfo, PropList);

      WriteLn('Propriétés de ', Obj.ClassName, ':');
      for I := 0 to PropCount - 1 do
      begin
        WriteLn('  - ', PropList^[I]^.Name,
                ' (Type: ', PropList^[I]^.PropType^.Name, ')');
      end;
    finally
      FreeMem(PropList);
    end;
  end;
end;
```

## RTTI pour les méthodes

### Appeler des méthodes dynamiquement

```pascal
uses
  TypInfo;

type
  TCalculator = class
  published
    function Add(A, B: Integer): Integer;
    function Multiply(A, B: Integer): Integer;
    procedure ShowResult(Value: Integer);
  end;

function TCalculator.Add(A, B: Integer): Integer;  
begin
  Result := A + B;
end;

function TCalculator.Multiply(A, B: Integer): Integer;  
begin
  Result := A * B;
end;

procedure TCalculator.ShowResult(Value: Integer);  
begin
  WriteLn('Résultat: ', Value);
end;

// Appel de méthode via RTTI
procedure CallMethod(Obj: TObject; const MethodName: String;
                    const Args: array of Variant);
var
  Method: TMethod;
  MethodInfo: PMethodInfo;
begin
  Method.Code := Obj.MethodAddress(MethodName);
  Method.Data := Obj;

  if Assigned(Method.Code) then
  begin
    // Ici, vous devriez utiliser les informations de signature
    // pour appeler correctement la méthode
    WriteLn('Méthode "', MethodName, '" trouvée');
  end
  else
    WriteLn('Méthode "', MethodName, '" non trouvée');
end;
```

## RTTI avancé avec TRttiContext

FreePascal moderne offre une API RTTI plus puissante via l'unité `Rtti` :

```pascal
uses
  Rtti;

type
  TEmployee = class
  private
    FName: String;
    FSalary: Double;
    FDepartment: String;
  public
    constructor Create(const AName: String; ASalary: Double);
    function GetAnnualSalary: Double;
  published
    property Name: String read FName write FName;
    property Salary: Double read FSalary write FSalary;
    property Department: String read FDepartment write FDepartment;
  end;

constructor TEmployee.Create(const AName: String; ASalary: Double);  
begin
  FName := AName;
  FSalary := ASalary;
end;

function TEmployee.GetAnnualSalary: Double;  
begin
  Result := FSalary * 12;
end;

procedure ExploreWithRtti(Obj: TObject);  
var
  Context: TRttiContext;
  RttiType: TRttiType;
  RttiProp: TRttiProperty;
  RttiMethod: TRttiMethod;
  Value: TValue;
begin
  Context := TRttiContext.Create;
  try
    RttiType := Context.GetType(Obj.ClassType);

    WriteLn('=== Analyse RTTI de ', RttiType.Name, ' ===');

    // Explorer les propriétés
    WriteLn('Propriétés:');
    for RttiProp in RttiType.GetProperties do
    begin
      Value := RttiProp.GetValue(Obj);
      Write('  ', RttiProp.Name, ': ');

      case Value.Kind of
        tkInteger: WriteLn(Value.AsInteger);
        tkFloat: WriteLn(Value.AsExtended:0:2);
        tkString, tkUString: WriteLn(Value.AsString);
      else
        WriteLn('(type non géré)');
      end;
    end;

    // Explorer les méthodes
    WriteLn('Méthodes:');
    for RttiMethod in RttiType.GetMethods do
    begin
      WriteLn('  ', RttiMethod.Name);
    end;
  finally
    Context.Free;
  end;
end;

// Utilisation
var
  Emp: TEmployee;
begin
  Emp := TEmployee.Create('Alice', 5000);
  Emp.Department := 'IT';
  try
    ExploreWithRtti(Emp);
  finally
    Emp.Free;
  end;
end;
```

## Manipulation de types avec TValue

`TValue` est un conteneur universel qui peut stocker n'importe quelle valeur avec ses informations de type :

```pascal
uses
  Rtti;

procedure DemoTValue;  
var
  V1, V2, V3: TValue;
  IntVal: Integer;
  StrVal: String;
begin
  // Créer des TValue à partir de différents types
  V1 := TValue.From<Integer>(42);
  V2 := TValue.From<String>('Bonjour');
  V3 := TValue.From<Double>(3.14159);

  // Vérifier les types
  if V1.IsType<Integer> then
    WriteLn('V1 est un entier: ', V1.AsInteger);

  if V2.Kind = tkUString then
    WriteLn('V2 est une chaîne: ', V2.AsString);

  // Conversion sûre
  if V1.TryAsType<Integer>(IntVal) then
    WriteLn('Conversion réussie: ', IntVal);

  // Obtenir les informations de type
  WriteLn('Type de V3: ', V3.TypeInfo^.Name);

  // Comparaisons
  if V1.AsInteger > 40 then
    WriteLn('V1 est supérieur à 40');
end;
```

## Création dynamique d'instances

### Créer des objets par nom de classe

```pascal
uses
  TypInfo, Rtti;

type
  TShape = class
  public
    procedure Draw; virtual; abstract;
  end;

  TCircle = class(TShape)
  public
    procedure Draw; override;
  end;

  TSquare = class(TShape)
  public
    procedure Draw; override;
  end;

procedure TCircle.Draw;  
begin
  WriteLn('Dessin d''un cercle');
end;

procedure TSquare.Draw;  
begin
  WriteLn('Dessin d''un carré');
end;

// Table de classes enregistrées
var
  RegisteredClasses: TStringList;

procedure RegisterClass(AClass: TClass; const ClassName: String);  
begin
  if not Assigned(RegisteredClasses) then
    RegisteredClasses := TStringList.Create;
  RegisteredClasses.AddObject(ClassName, TObject(AClass));
end;

function CreateInstanceByName(const ClassName: String): TObject;  
var
  Index: Integer;
  AClass: TClass;
begin
  Result := nil;
  if Assigned(RegisteredClasses) then
  begin
    Index := RegisteredClasses.IndexOf(ClassName);
    if Index >= 0 then
    begin
      AClass := TClass(RegisteredClasses.Objects[Index]);
      Result := AClass.Create;
    end;
  end;
end;

// Utilisation
var
  Shape: TShape;
  ShapeType: String;
begin
  // Enregistrer les classes
  RegisterClass(TCircle, 'TCircle');
  RegisterClass(TSquare, 'TSquare');

  Write('Entrez le type de forme (TCircle ou TSquare): ');
  ReadLn(ShapeType);

  Shape := TShape(CreateInstanceByName(ShapeType));
  if Assigned(Shape) then
  begin
    Shape.Draw;
    Shape.Free;
  end
  else
    WriteLn('Type inconnu');

  RegisteredClasses.Free;
end;
```

## Sérialisation avec RTTI

### Sauvegarder et charger des objets automatiquement

```pascal
uses
  TypInfo, Classes;

type
  TSerializable = class
  public
    procedure SaveToStream(Stream: TStream);
    procedure LoadFromStream(Stream: TStream);
  end;

procedure TSerializable.SaveToStream(Stream: TStream);  
var
  Writer: TWriter;
  PropList: PPropList;
  PropCount, I: Integer;
  PropInfo: PPropInfo;
begin
  Writer := TWriter.Create(Stream, 4096);
  try
    // Écrire le nom de la classe
    Writer.WriteString(ClassName);

    // Obtenir la liste des propriétés
    PropCount := GetPropCount(Self);
    if PropCount > 0 then
    begin
      GetMem(PropList, PropCount * SizeOf(PPropInfo));
      try
        GetPropInfos(Self.ClassInfo, PropList);

        // Sauvegarder chaque propriété
        for I := 0 to PropCount - 1 do
        begin
          PropInfo := PropList^[I];
          Writer.WriteString(PropInfo^.Name);

          case PropInfo^.PropType^.Kind of
            tkInteger:
              Writer.WriteInteger(GetOrdProp(Self, PropInfo));
            tkFloat:
              Writer.WriteFloat(GetFloatProp(Self, PropInfo));
            tkString, tkLString, tkUString:
              Writer.WriteString(GetStrProp(Self, PropInfo));
          end;
        end;
      finally
        FreeMem(PropList);
      end;
    end;
  finally
    Writer.Free;
  end;
end;

procedure TSerializable.LoadFromStream(Stream: TStream);  
var
  Reader: TReader;
  ClassName: String;
  PropName: String;
  PropInfo: PPropInfo;
begin
  Reader := TReader.Create(Stream, 4096);
  try
    // Lire le nom de la classe
    ClassName := Reader.ReadString;

    // Vérifier que c'est la bonne classe
    if ClassName <> Self.ClassName then
      raise Exception.Create('Type de classe incorrect');

    // Lire les propriétés
    while not Reader.EndOfList do
    begin
      PropName := Reader.ReadString;
      PropInfo := GetPropInfo(Self, PropName);

      if Assigned(PropInfo) then
      begin
        case PropInfo^.PropType^.Kind of
          tkInteger:
            SetOrdProp(Self, PropInfo, Reader.ReadInteger);
          tkFloat:
            SetFloatProp(Self, PropInfo, Reader.ReadFloat);
          tkString, tkLString, tkUString:
            SetStrProp(Self, PropInfo, Reader.ReadString);
        end;
      end;
    end;
  finally
    Reader.Free;
  end;
end;
```

## RTTI pour les énumérations et ensembles

### Travailler avec les types énumérés

```pascal
uses
  TypInfo;

type
  TColor = (clRed, clGreen, clBlue, clYellow, clBlack, clWhite);
  TColors = set of TColor;

procedure ShowEnumInfo;  
var
  TypeData: PTypeData;
  I: Integer;
  ColorName: String;
  ColorValue: TColor;
begin
  TypeData := GetTypeData(TypeInfo(TColor));

  WriteLn('Énumération TColor:');
  WriteLn('  Valeur min: ', TypeData^.MinValue);
  WriteLn('  Valeur max: ', TypeData^.MaxValue);

  // Lister toutes les valeurs
  for I := TypeData^.MinValue to TypeData^.MaxValue do
  begin
    ColorName := GetEnumName(TypeInfo(TColor), I);
    WriteLn('  ', I, ': ', ColorName);
  end;

  // Conversion chaîne vers énumération
  ColorValue := TColor(GetEnumValue(TypeInfo(TColor), 'clBlue'));
  WriteLn('clBlue correspond à la valeur: ', Ord(ColorValue));
end;

procedure ShowSetInfo;  
var
  Colors: TColors;
  SetString: String;
begin
  Colors := [clRed, clBlue, clWhite];

  // Convertir un ensemble en chaîne
  SetString := SetToString(TypeInfo(TColors),
                          Integer(Colors), True);
  WriteLn('Ensemble: ', SetString);

  // Convertir une chaîne en ensemble
  Colors := TColors(StringToSet(TypeInfo(TColors),
                                '[clGreen,clYellow]'));

  // Vérifier le contenu
  if clGreen in Colors then
    WriteLn('L''ensemble contient clGreen');
end;
```

## Attributs personnalisés (Custom Attributes)

Les attributs permettent d'ajouter des métadonnées à vos types :

```pascal
uses
  Rtti;

type
  // Définir un attribut personnalisé
  DescriptionAttribute = class(TCustomAttribute)
  private
    FText: String;
  public
    constructor Create(const AText: String);
    property Text: String read FText;
  end;

  ValidationAttribute = class(TCustomAttribute)
  private
    FMinValue: Integer;
    FMaxValue: Integer;
  public
    constructor Create(AMin, AMax: Integer);
    property MinValue: Integer read FMinValue;
    property MaxValue: Integer read FMaxValue;
  end;

constructor DescriptionAttribute.Create(const AText: String);  
begin
  FText := AText;
end;

constructor ValidationAttribute.Create(AMin, AMax: Integer);  
begin
  FMinValue := AMin;
  FMaxValue := AMax;
end;

type
  [Description('Classe représentant un produit')]
  TProduct = class
  private
    FPrice: Double;
    FStock: Integer;
  published
    [Description('Prix du produit en euros')]
    property Price: Double read FPrice write FPrice;

    [Description('Quantité en stock')]
    [Validation(0, 1000)]
    property Stock: Integer read FStock write FStock;
  end;

procedure ReadAttributes(Obj: TObject);  
var
  Context: TRttiContext;
  RttiType: TRttiType;
  RttiProp: TRttiProperty;
  Attr: TCustomAttribute;
begin
  Context := TRttiContext.Create;
  try
    RttiType := Context.GetType(Obj.ClassType);

    // Attributs de la classe
    for Attr in RttiType.GetAttributes do
    begin
      if Attr is DescriptionAttribute then
        WriteLn('Description de la classe: ',
                DescriptionAttribute(Attr).Text);
    end;

    // Attributs des propriétés
    for RttiProp in RttiType.GetProperties do
    begin
      WriteLn('Propriété: ', RttiProp.Name);
      for Attr in RttiProp.GetAttributes do
      begin
        if Attr is DescriptionAttribute then
          WriteLn('  Description: ', DescriptionAttribute(Attr).Text);
        if Attr is ValidationAttribute then
          WriteLn('  Validation: ', ValidationAttribute(Attr).MinValue,
                  ' à ', ValidationAttribute(Attr).MaxValue);
      end;
    end;
  finally
    Context.Free;
  end;
end;
```

## Applications pratiques du RTTI

### Mapping objet-relationnel simple

```pascal
uses
  TypInfo, Classes, SysUtils;

type
  TableAttribute = class(TCustomAttribute)
  private
    FTableName: String;
  public
    constructor Create(const ATableName: String);
    property TableName: String read FTableName;
  end;

  ColumnAttribute = class(TCustomAttribute)
  private
    FColumnName: String;
    FPrimaryKey: Boolean;
  public
    constructor Create(const AColumnName: String;
                      APrimaryKey: Boolean = False);
    property ColumnName: String read FColumnName;
    property PrimaryKey: Boolean read FPrimaryKey;
  end;

[Table('users')]
TUser = class  
private
  FId: Integer;
  FName: String;
  FEmail: String;
published
  [Column('id', True)]
  property Id: Integer read FId write FId;

  [Column('name')]
  property Name: String read FName write FName;

  [Column('email')]
  property Email: String read FEmail write FEmail;
end;

function GenerateInsertSQL(Obj: TObject): String;  
var
  Context: TRttiContext;
  RttiType: TRttiType;
  RttiProp: TRttiProperty;
  Attr: TCustomAttribute;
  TableName, Columns, Values: String;
  ColumnAttr: ColumnAttribute;
begin
  Context := TRttiContext.Create;
  try
    RttiType := Context.GetType(Obj.ClassType);

    // Obtenir le nom de la table
    for Attr in RttiType.GetAttributes do
    begin
      if Attr is TableAttribute then
      begin
        TableName := TableAttribute(Attr).TableName;
        Break;
      end;
    end;

    // Construire la requête SQL
    Columns := '';
    Values := '';

    for RttiProp in RttiType.GetProperties do
    begin
      for Attr in RttiProp.GetAttributes do
      begin
        if Attr is ColumnAttribute then
        begin
          ColumnAttr := ColumnAttribute(Attr);
          if not ColumnAttr.PrimaryKey then // Ignorer la clé primaire
          begin
            if Columns <> '' then
            begin
              Columns := Columns + ', ';
              Values := Values + ', ';
            end;

            Columns := Columns + ColumnAttr.ColumnName;

            case RttiProp.PropertyType.TypeKind of
              tkInteger:
                Values := Values + IntToStr(RttiProp.GetValue(Obj).AsInteger);
              tkString, tkUString:
                Values := Values + QuotedStr(RttiProp.GetValue(Obj).AsString);
            end;
          end;
        end;
      end;
    end;

    Result := Format('INSERT INTO %s (%s) VALUES (%s)',
                    [TableName, Columns, Values]);
  finally
    Context.Free;
  end;
end;

// Utilisation
var
  User: TUser;
  SQL: String;
begin
  User := TUser.Create;
  try
    User.Name := 'Jean Dupont';
    User.Email := 'jean@example.com';

    SQL := GenerateInsertSQL(User);
    WriteLn('SQL généré: ', SQL);
    // Résultat: INSERT INTO users (name, email) VALUES ('Jean Dupont', 'jean@example.com')
  finally
    User.Free;
  end;
end;
```

## Bonnes pratiques et considérations

### Performance

Le RTTI a un coût en termes de performance. Voici quelques conseils :

```pascal
type
  TOptimizedRTTI = class
  private
    // Cache des informations RTTI
    class var FPropCache: TDictionary<String, PPropInfo>;
  public
    class constructor Create;
    class destructor Destroy;

    class function GetCachedPropInfo(Obj: TObject;
                                     const PropName: String): PPropInfo;
  end;

class constructor TOptimizedRTTI.Create;  
begin
  FPropCache := TDictionary<String, PPropInfo>.Create;
end;

class destructor TOptimizedRTTI.Destroy;  
begin
  FPropCache.Free;
end;

class function TOptimizedRTTI.GetCachedPropInfo(Obj: TObject;
                                               const PropName: String): PPropInfo;
var
  Key: String;
begin
  Key := Obj.ClassName + '.' + PropName;

  if not FPropCache.TryGetValue(Key, Result) then
  begin
    Result := GetPropInfo(Obj, PropName);
    if Assigned(Result) then
      FPropCache.Add(Key, Result);
  end;
end;
```

### Sécurité

### Validation sécurisée des propriétés

Attention aux risques de sécurité avec le RTTI :

```pascal
procedure SafeSetProperty(Obj: TObject; const PropName: String;
                         const Value: Variant);
var
  PropInfo: PPropInfo;
begin
  PropInfo := GetPropInfo(Obj, PropName);

  if Assigned(PropInfo) then
  begin
    // Vérifier que la propriété est bien en écriture
    if not Assigned(PropInfo^.SetProc) then
    begin
      raise Exception.CreateFmt('La propriété "%s" est en lecture seule',
                                [PropName]);
    end;

    // Valider le type avant l'assignation
    try
      case PropInfo^.PropType^.Kind of
        tkInteger:
          begin
            // Vérifier les limites pour les entiers
            if VarIsOrdinal(Value) then
              SetOrdProp(Obj, PropInfo, Value)
            else
              raise Exception.Create('Type incompatible: entier attendu');
          end;

        tkFloat:
          begin
            // Vérifier que c'est bien un nombre
            if VarIsNumeric(Value) then
              SetFloatProp(Obj, PropInfo, Value)
            else
              raise Exception.Create('Type incompatible: nombre attendu');
          end;

        tkString, tkLString, tkUString:
          begin
            // Limiter la longueur des chaînes pour éviter les débordements
            if Length(VarToStr(Value)) > 1000 then
              raise Exception.Create('Chaîne trop longue');
            SetStrProp(Obj, PropInfo, VarToStr(Value));
          end;
      else
        raise Exception.CreateFmt('Type de propriété non supporté: %s',
                                  [PropInfo^.PropType^.Name]);
      end;
    except
      on E: Exception do
        raise Exception.CreateFmt('Erreur lors de l''assignation de "%s": %s',
                                  [PropName, E.Message]);
    end;
  end
  else
    raise Exception.CreateFmt('Propriété "%s" introuvable', [PropName]);
end;
```

### Protection contre l'injection de code

```pascal
type
  TSecurePropertyFilter = class
  private
    FAllowedProperties: TStringList;
    FDeniedProperties: TStringList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AllowProperty(const PropName: String);
    procedure DenyProperty(const PropName: String);
    function IsPropertyAllowed(const PropName: String): Boolean;
  end;

constructor TSecurePropertyFilter.Create;  
begin
  FAllowedProperties := TStringList.Create;
  FDeniedProperties := TStringList.Create;

  // Propriétés dangereuses par défaut
  FDeniedProperties.Add('Handle');
  FDeniedProperties.Add('WindowProc');
  FDeniedProperties.Add('ClassType');
  FDeniedProperties.Add('ClassInfo');
end;

destructor TSecurePropertyFilter.Destroy;  
begin
  FAllowedProperties.Free;
  FDeniedProperties.Free;
  inherited;
end;

procedure TSecurePropertyFilter.AllowProperty(const PropName: String);  
begin
  FAllowedProperties.Add(PropName);
end;

procedure TSecurePropertyFilter.DenyProperty(const PropName: String);  
begin
  FDeniedProperties.Add(PropName);
end;

function TSecurePropertyFilter.IsPropertyAllowed(const PropName: String): Boolean;  
begin
  // Si une liste blanche existe, seules ces propriétés sont autorisées
  if FAllowedProperties.Count > 0 then
    Result := FAllowedProperties.IndexOf(PropName) >= 0
  else
    // Sinon, toutes sauf celles de la liste noire
    Result := FDeniedProperties.IndexOf(PropName) < 0;
end;

// Utilisation sécurisée
procedure SecureSetProperty(Obj: TObject; const PropName: String;
                           const Value: Variant; Filter: TSecurePropertyFilter);
begin
  if not Filter.IsPropertyAllowed(PropName) then
    raise Exception.CreateFmt('Accès refusé à la propriété "%s"', [PropName]);

  SafeSetProperty(Obj, PropName, Value);
end;
```

## Gestion de la mémoire avec RTTI

### Libération automatique des ressources

```pascal
uses
  TypInfo, Rtti;

type
  AutoFreeAttribute = class(TCustomAttribute)
  end;

  TResourceManager = class
  private
    [AutoFree]
    FDatabase: TObject;

    [AutoFree]
    FLogger: TObject;

    FConfig: TObject; // Pas d'attribut, ne sera pas libéré automatiquement

    procedure FreeAutoProperties;
  public
    destructor Destroy; override;
  end;

procedure TResourceManager.FreeAutoProperties;  
var
  Context: TRttiContext;
  RttiType: TRttiType;
  Field: TRttiField;
  Attr: TCustomAttribute;
  Obj: TObject;
begin
  Context := TRttiContext.Create;
  try
    RttiType := Context.GetType(ClassType);

    for Field in RttiType.GetFields do
    begin
      for Attr in Field.GetAttributes do
      begin
        if Attr is AutoFreeAttribute then
        begin
          if Field.FieldType.TypeKind = tkClass then
          begin
            Obj := Field.GetValue(Self).AsObject;
            if Assigned(Obj) then
            begin
              Obj.Free;
              Field.SetValue(Self, nil);
            end;
          end;
          Break;
        end;
      end;
    end;
  finally
    Context.Free;
  end;
end;

destructor TResourceManager.Destroy;  
begin
  FreeAutoProperties;
  inherited;
end;
```

### Clonage profond d'objets

```pascal
uses
  TypInfo, Rtti, SysUtils;

type
  TObjectCloner = class
  public
    class function DeepClone<T: class>(Source: T): T;
  end;

class function TObjectCloner.DeepClone<T>(Source: T): T;  
var
  Context: TRttiContext;
  RttiType: TRttiType;
  SourceProp, DestProp: TRttiProperty;
  SourceValue: TValue;
  ClonedObj: TObject;
begin
  if not Assigned(Source) then
    Exit(nil);

  Context := TRttiContext.Create;
  try
    RttiType := Context.GetType(Source.ClassType);

    // Créer une nouvelle instance
    Result := T(RttiType.GetMethod('Create').Invoke(
                RttiType.AsInstance.MetaclassType, []).AsObject);

    // Copier toutes les propriétés
    for SourceProp in RttiType.GetProperties do
    begin
      if SourceProp.IsWritable then
      begin
        DestProp := RttiType.GetProperty(SourceProp.Name);
        SourceValue := SourceProp.GetValue(Source);

        case SourceValue.Kind of
          tkClass:
            begin
              // Clonage récursif pour les objets
              if SourceValue.AsObject <> nil then
              begin
                ClonedObj := DeepClone<TObject>(SourceValue.AsObject);
                DestProp.SetValue(Result, ClonedObj);
              end;
            end;
          else
            // Copie simple pour les types valeur
            DestProp.SetValue(Result, SourceValue);
        end;
      end;
    end;
  finally
    Context.Free;
  end;
end;

// Exemple d'utilisation
type
  TAddress = class
  public
    Street: String;
    City: String;
  end;

  TPerson = class
  public
    Name: String;
    Age: Integer;
    Address: TAddress;

    constructor Create;
    destructor Destroy; override;
  end;

constructor TPerson.Create;  
begin
  Address := TAddress.Create;
end;

destructor TPerson.Destroy;  
begin
  Address.Free;
  inherited;
end;

var
  Original, Clone: TPerson;
begin
  Original := TPerson.Create;
  Original.Name := 'Alice';
  Original.Age := 30;
  Original.Address.City := 'Paris';

  Clone := TObjectCloner.DeepClone<TPerson>(Original);

  // Clone est une copie complète indépendante
  Clone.Address.City := 'Lyon';

  WriteLn('Original: ', Original.Address.City); // Paris
  WriteLn('Clone: ', Clone.Address.City);       // Lyon

  Original.Free;
  Clone.Free;
end;
```

## RTTI et interfaces

### Découverte dynamique des interfaces

```pascal
uses
  TypInfo, Rtti;

type
  IPrintable = interface
    ['{A1B2C3D4-E5F6-1234-5678-90ABCDEF1234}']
    procedure Print;
  end;

  ISaveable = interface
    ['{B2C3D4E5-F6A7-2345-6789-01BCDEF23456}']
    procedure SaveToFile(const FileName: String);
  end;

  TDocument = class(TInterfacedObject, IPrintable, ISaveable)
  public
    procedure Print;
    procedure SaveToFile(const FileName: String);
  end;

procedure TDocument.Print;  
begin
  WriteLn('Impression du document...');
end;

procedure TDocument.SaveToFile(const FileName: String);  
begin
  WriteLn('Sauvegarde dans ', FileName);
end;

procedure DiscoverInterfaces(Obj: TObject);  
var
  Context: TRttiContext;
  RttiType: TRttiType;
  IntfType: TRttiInterfaceType;
  TypeData: PTypeData;
  I: Integer;
  IntfTable: PInterfaceTable;
  IntfEntry: PInterfaceEntry;
begin
  Context := TRttiContext.Create;
  try
    RttiType := Context.GetType(Obj.ClassType);

    WriteLn('Interfaces implémentées par ', RttiType.Name, ':');

    // Méthode 1 : Via RTTI moderne
    if RttiType is TRttiInstanceType then
    begin
      for IntfType in TRttiInstanceType(RttiType).GetImplementedInterfaces do
      begin
        WriteLn('  - ', IntfType.Name, ' (GUID: ',
                GUIDToString(IntfType.GUID), ')');
      end;
    end;

    // Méthode 2 : Via TypeInfo classique
    TypeData := GetTypeData(Obj.ClassInfo);
    IntfTable := TypeData^.IntfTable;
    if Assigned(IntfTable) then
    begin
      WriteLn('Nombre d''interfaces: ', IntfTable^.EntryCount);
      for I := 0 to IntfTable^.EntryCount - 1 do
      begin
        IntfEntry := @IntfTable^.Entries[I];
        WriteLn('  Interface ', I, ': ',
                GUIDToString(IntfEntry^.IID));
      end;
    end;
  finally
    Context.Free;
  end;
end;

// Test d'interface dynamique
procedure TestInterface(Obj: TObject; const IID: TGUID);  
var
  Intf: IInterface;
begin
  if Supports(Obj, IID, Intf) then
  begin
    WriteLn('L''objet supporte l''interface ', GUIDToString(IID));

    // Appel dynamique possible ici
    if Supports(Intf, IPrintable) then
      (Intf as IPrintable).Print;
  end
  else
    WriteLn('L''objet ne supporte pas cette interface');
end;
```

## RTTI pour le débogage et le monitoring

### Inspecteur d'objets en temps réel

```pascal
uses
  TypInfo, Rtti, SysUtils;

type
  TObjectInspector = class
  private
    FWatchedObjects: TList;
    FSnapshots: TDictionary<TObject, TStringList>;
  public
    constructor Create;
    destructor Destroy; override;

    procedure WatchObject(Obj: TObject);
    procedure TakeSnapshot(Obj: TObject);
    function DetectChanges(Obj: TObject): TStringList;
    procedure PrintObjectState(Obj: TObject);
  end;

constructor TObjectInspector.Create;  
begin
  FWatchedObjects := TList.Create;
  FSnapshots := TDictionary<TObject, TStringList>.Create;
end;

destructor TObjectInspector.Destroy;  
var
  Snapshot: TStringList;
begin
  for Snapshot in FSnapshots.Values do
    Snapshot.Free;
  FSnapshots.Free;
  FWatchedObjects.Free;
  inherited;
end;

procedure TObjectInspector.WatchObject(Obj: TObject);  
begin
  if FWatchedObjects.IndexOf(Obj) < 0 then
  begin
    FWatchedObjects.Add(Obj);
    TakeSnapshot(Obj);
  end;
end;

procedure TObjectInspector.TakeSnapshot(Obj: TObject);  
var
  Context: TRttiContext;
  RttiType: TRttiType;
  RttiProp: TRttiProperty;
  Snapshot: TStringList;
  Value: TValue;
  ValueStr: String;
begin
  Context := TRttiContext.Create;
  try
    RttiType := Context.GetType(Obj.ClassType);

    if FSnapshots.ContainsKey(Obj) then
      FSnapshots[Obj].Clear
    else
    begin
      Snapshot := TStringList.Create;
      FSnapshots.Add(Obj, Snapshot);
    end;

    Snapshot := FSnapshots[Obj];

    for RttiProp in RttiType.GetProperties do
    begin
      if RttiProp.IsReadable then
      begin
        Value := RttiProp.GetValue(Obj);

        case Value.Kind of
          tkInteger: ValueStr := IntToStr(Value.AsInteger);
          tkFloat: ValueStr := FloatToStr(Value.AsExtended);
          tkString, tkUString: ValueStr := Value.AsString;
          tkEnumeration: ValueStr := Value.ToString;
        else
          ValueStr := '(complex type)';
        end;

        Snapshot.Values[RttiProp.Name] := ValueStr;
      end;
    end;
  finally
    Context.Free;
  end;
end;

function TObjectInspector.DetectChanges(Obj: TObject): TStringList;  
var
  OldSnapshot, NewSnapshot: TStringList;
  PropName, OldValue, NewValue: String;
  I: Integer;
begin
  Result := TStringList.Create;

  if not FSnapshots.TryGetValue(Obj, OldSnapshot) then
  begin
    Result.Add('Objet non surveillé');
    Exit;
  end;

  // Prendre un nouveau snapshot temporaire
  NewSnapshot := TStringList.Create;
  try
    // Sauvegarder l'ancien
    FSnapshots.Remove(Obj);
    FSnapshots.Add(Obj, NewSnapshot);
    TakeSnapshot(Obj);

    // Comparer
    for I := 0 to NewSnapshot.Count - 1 do
    begin
      PropName := NewSnapshot.Names[I];
      NewValue := NewSnapshot.ValueFromIndex[I];
      OldValue := OldSnapshot.Values[PropName];

      if OldValue <> NewValue then
      begin
        Result.Add(Format('%s: %s -> %s',
                          [PropName, OldValue, NewValue]));
      end;
    end;
  finally
    // Restaurer si nécessaire
  end;
end;

procedure TObjectInspector.PrintObjectState(Obj: TObject);  
var
  Context: TRttiContext;
  RttiType: TRttiType;
  RttiProp: TRttiProperty;
  RttiField: TRttiField;
  Value: TValue;
  Indent: String;
begin
  Context := TRttiContext.Create;
  try
    RttiType := Context.GetType(Obj.ClassType);

    WriteLn('=== État de l''objet ', RttiType.Name, ' ===');
    WriteLn('Adresse mémoire: ', IntToHex(NativeInt(Obj), 8));

    // Propriétés publiques
    WriteLn('Propriétés:');
    for RttiProp in RttiType.GetProperties do
    begin
      if RttiProp.IsReadable then
      begin
        Value := RttiProp.GetValue(Obj);
        Write('  ', RttiProp.Name, ' (',
              RttiProp.PropertyType.Name, '): ');

        case Value.Kind of
          tkInteger: WriteLn(Value.AsInteger);
          tkFloat: WriteLn(Value.AsExtended:0:2);
          tkString, tkUString: WriteLn('"', Value.AsString, '"');
          tkClass:
            if Value.AsObject = nil then
              WriteLn('nil')
            else
              WriteLn('Instance de ', Value.AsObject.ClassName);
        else
          WriteLn('(valeur complexe)');
        end;
      end;
    end;

    // Champs privés (pour débogage approfondi)
    WriteLn('Champs privés:');
    for RttiField in RttiType.GetFields do
    begin
      if RttiField.Visibility = mvPrivate then
      begin
        Value := RttiField.GetValue(Obj);
        Write('  ', RttiField.Name, ': ');
        WriteLn(Value.ToString);
      end;
    end;
  finally
    Context.Free;
  end;
end;
```

## Optimisation et cache RTTI

### Système de cache pour les métadonnées

```pascal
uses
  TypInfo, Rtti, Generics.Collections;

type
  TRttiCache = class
  private
    type
      TPropertyCache = TDictionary<String, TRttiProperty>;
      TMethodCache = TDictionary<String, TRttiMethod>;

    class var
      FContext: TRttiContext;
      FTypeCache: TDictionary<PTypeInfo, TRttiType>;
      FPropertyCaches: TObjectDictionary<TClass, TPropertyCache>;
      FMethodCaches: TObjectDictionary<TClass, TMethodCache>;

    class constructor Create;
    class destructor Destroy;
  public
    class function GetRttiType(AClass: TClass): TRttiType;
    class function GetProperty(AClass: TClass;
                               const PropName: String): TRttiProperty;
    class function GetMethod(AClass: TClass;
                            const MethodName: String): TRttiMethod;
    class procedure ClearCache;
  end;

class constructor TRttiCache.Create;  
begin
  FContext := TRttiContext.Create;
  FTypeCache := TDictionary<PTypeInfo, TRttiType>.Create;
  FPropertyCaches := TObjectDictionary<TClass, TPropertyCache>.Create([doOwnsValues]);
  FMethodCaches := TObjectDictionary<TClass, TMethodCache>.Create([doOwnsValues]);
end;

class destructor TRttiCache.Destroy;  
begin
  FMethodCaches.Free;
  FPropertyCaches.Free;
  FTypeCache.Free;
  FContext.Free;
end;

class function TRttiCache.GetRttiType(AClass: TClass): TRttiType;  
begin
  if not FTypeCache.TryGetValue(AClass.ClassInfo, Result) then
  begin
    Result := FContext.GetType(AClass);
    FTypeCache.Add(AClass.ClassInfo, Result);
  end;
end;

class function TRttiCache.GetProperty(AClass: TClass;
                                      const PropName: String): TRttiProperty;
var
  PropCache: TPropertyCache;
  RttiType: TRttiType;
begin
  if not FPropertyCaches.TryGetValue(AClass, PropCache) then
  begin
    PropCache := TPropertyCache.Create;
    FPropertyCaches.Add(AClass, PropCache);
  end;

  if not PropCache.TryGetValue(PropName, Result) then
  begin
    RttiType := GetRttiType(AClass);
    Result := RttiType.GetProperty(PropName);
    if Assigned(Result) then
      PropCache.Add(PropName, Result);
  end;
end;

class function TRttiCache.GetMethod(AClass: TClass;
                                   const MethodName: String): TRttiMethod;
var
  MethodCache: TMethodCache;
  RttiType: TRttiType;
begin
  if not FMethodCaches.TryGetValue(AClass, MethodCache) then
  begin
    MethodCache := TMethodCache.Create;
    FMethodCaches.Add(AClass, MethodCache);
  end;

  if not MethodCache.TryGetValue(MethodName, Result) then
  begin
    RttiType := GetRttiType(AClass);
    Result := RttiType.GetMethod(MethodName);
    if Assigned(Result) then
      MethodCache.Add(MethodName, Result);
  end;
end;

class procedure TRttiCache.ClearCache;  
begin
  FTypeCache.Clear;
  FPropertyCaches.Clear;
  FMethodCaches.Clear;
end;

// Utilisation avec cache
procedure FastPropertyAccess(Obj: TObject; const PropName: String);  
var
  Prop: TRttiProperty;
  Value: TValue;
  StartTime: TDateTime;
begin
  StartTime := Now;

  // Accès avec cache (rapide après la première fois)
  Prop := TRttiCache.GetProperty(Obj.ClassType, PropName);
  if Assigned(Prop) then
  begin
    Value := Prop.GetValue(Obj);
    WriteLn(PropName, ' = ', Value.ToString);
  end;

  WriteLn('Temps d''accès: ',
          MilliSecondsBetween(Now, StartTime), ' ms');
end;
```

## Conclusion

Le RTTI est un outil extrêmement puissant en FreePascal/Lazarus qui permet :

- **L'introspection** : Examiner la structure des types à l'exécution
- **La manipulation dynamique** : Créer, modifier et appeler des éléments sans connaître leur type à la compilation
- **La sérialisation** : Sauvegarder et charger des objets automatiquement
- **Le mapping** : Créer des correspondances entre objets et bases de données
- **Le débogage avancé** : Inspecter et surveiller l'état des objets
- **L'extensibilité** : Créer des systèmes de plugins et d'extensions

### Points clés à retenir

1. **Performance** : Le RTTI a un coût, utilisez des caches quand possible
2. **Sécurité** : Validez toujours les entrées lors de manipulations dynamiques
3. **Maintenance** : Le code utilisant le RTTI peut être plus difficile à déboguer
4. **Compatibilité** : Vérifiez la disponibilité des fonctionnalités RTTI selon la version de FreePascal
5. **Documentation** : Documentez bien l'utilisation du RTTI dans votre code

Le RTTI ouvre des possibilités de programmation avancée et permet de créer des applications plus flexibles et dynamiques, mais doit être utilisé judicieusement pour maintenir un code performant et maintenable.

⏭️ [Programmation méta avec les attributs](/03-langage-object-pascal-avance/04-programmation-meta-attributs.md)
