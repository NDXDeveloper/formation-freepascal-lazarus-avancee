🔝 Retour au [Sommaire](/SOMMAIRE.md)

# Programmation méta avec les attributs en FreePascal/Lazarus

## Introduction : Qu'est-ce que la programmation méta ?

La programmation méta (ou métaprogrammation) est une technique qui permet à un programme de traiter d'autres programmes (ou lui-même) comme des données. C'est comme avoir un programme qui peut lire, analyser et même modifier du code pendant son exécution.

Les **attributs** (ou annotations) sont des métadonnées que vous attachez à votre code pour donner des informations supplémentaires. Imaginez-les comme des "post-it" que vous collez sur vos classes, méthodes ou propriétés pour indiquer comment elles doivent être traitées.

Par exemple, vous pourriez marquer une propriété avec `[Required]` pour indiquer qu'elle est obligatoire, ou `[MaxLength(50)]` pour limiter sa longueur. Votre programme peut ensuite lire ces attributs et agir en conséquence.

## Les bases des attributs personnalisés

### Créer un attribut simple

En FreePascal, un attribut est simplement une classe qui hérite de `TCustomAttribute` :

```pascal
uses
  Rtti;

type
  // Définition d'un attribut simple
  DescriptionAttribute = class(TCustomAttribute)
  private
    FText: String;
  public
    constructor Create(const AText: String);
    property Text: String read FText;
  end;

constructor DescriptionAttribute.Create(const AText: String);
begin
  inherited Create;
  FText := AText;
end;

// Utilisation de l'attribut
type
  [Description('Cette classe représente un utilisateur du système')]
  TUser = class
  private
    FName: String;
  published
    [Description('Nom complet de l''utilisateur')]
    property Name: String read FName write FName;
  end;
```

### Lire les attributs

Pour lire les attributs attachés à une classe ou propriété :

```pascal
procedure ReadClassAttributes(AClass: TClass);
var
  Context: TRttiContext;
  RttiType: TRttiType;
  Attr: TCustomAttribute;
begin
  Context := TRttiContext.Create;
  try
    RttiType := Context.GetType(AClass);

    // Parcourir tous les attributs de la classe
    for Attr in RttiType.GetAttributes do
    begin
      if Attr is DescriptionAttribute then
      begin
        WriteLn('Description de la classe: ',
                DescriptionAttribute(Attr).Text);
      end;
    end;
  finally
    Context.Free;
  end;
end;

// Utilisation
begin
  ReadClassAttributes(TUser);
  // Affiche: Description de la classe: Cette classe représente un utilisateur du système
end;
```

## Attributs de validation

### Créer un système de validation

Les attributs sont parfaits pour implémenter la validation de données :

```pascal
type
  // Attribut de base pour la validation
  ValidationAttribute = class(TCustomAttribute)
  public
    function Validate(const Value: TValue): Boolean; virtual; abstract;
    function GetErrorMessage: String; virtual; abstract;
  end;

  // Attribut pour valeur obligatoire
  RequiredAttribute = class(ValidationAttribute)
  public
    function Validate(const Value: TValue): Boolean; override;
    function GetErrorMessage: String; override;
  end;

  // Attribut pour longueur maximale
  MaxLengthAttribute = class(ValidationAttribute)
  private
    FMaxLength: Integer;
  public
    constructor Create(AMaxLength: Integer);
    function Validate(const Value: TValue): Boolean; override;
    function GetErrorMessage: String; override;
    property MaxLength: Integer read FMaxLength;
  end;

  // Attribut pour plage de valeurs
  RangeAttribute = class(ValidationAttribute)
  private
    FMin, FMax: Double;
  public
    constructor Create(AMin, AMax: Double);
    function Validate(const Value: TValue): Boolean; override;
    function GetErrorMessage: String; override;
  end;

// Implémentations
function RequiredAttribute.Validate(const Value: TValue): Boolean;
begin
  Result := not Value.IsEmpty;
  if Value.Kind in [tkString, tkUString] then
    Result := Result and (Trim(Value.AsString) <> '');
end;

function RequiredAttribute.GetErrorMessage: String;
begin
  Result := 'Cette valeur est obligatoire';
end;

constructor MaxLengthAttribute.Create(AMaxLength: Integer);
begin
  inherited Create;
  FMaxLength := AMaxLength;
end;

function MaxLengthAttribute.Validate(const Value: TValue): Boolean;
begin
  if Value.Kind in [tkString, tkUString] then
    Result := Length(Value.AsString) <= FMaxLength
  else
    Result := True;
end;

function MaxLengthAttribute.GetErrorMessage: String;
begin
  Result := Format('La longueur maximale est de %d caractères', [FMaxLength]);
end;

constructor RangeAttribute.Create(AMin, AMax: Double);
begin
  inherited Create;
  FMin := AMin;
  FMax := AMax;
end;

function RangeAttribute.Validate(const Value: TValue): Boolean;
var
  NumValue: Double;
begin
  if Value.TryAsType<Double>(NumValue) then
    Result := (NumValue >= FMin) and (NumValue <= FMax)
  else
    Result := True; // Pas un nombre, on laisse passer
end;

function RangeAttribute.GetErrorMessage: String;
begin
  Result := Format('La valeur doit être entre %g et %g', [FMin, FMax]);
end;
```

### Utiliser les attributs de validation

```pascal
type
  TProduct = class
  private
    FName: String;
    FPrice: Double;
    FDescription: String;
    FStock: Integer;
  published
    [Required]
    [MaxLength(100)]
    property Name: String read FName write FName;

    [Required]
    [Range(0.01, 999999.99)]
    property Price: Double read FPrice write FPrice;

    [MaxLength(500)]
    property Description: String read FDescription write FDescription;

    [Range(0, 10000)]
    property Stock: Integer read FStock write FStock;
  end;

// Validateur générique
type
  TValidator = class
  public
    class function ValidateObject(AObject: TObject;
                                  out Errors: TStringList): Boolean;
  end;

class function TValidator.ValidateObject(AObject: TObject;
                                         out Errors: TStringList): Boolean;
var
  Context: TRttiContext;
  RttiType: TRttiType;
  RttiProp: TRttiProperty;
  Attr: TCustomAttribute;
  Value: TValue;
  ValidationAttr: ValidationAttribute;
begin
  Result := True;
  Errors := TStringList.Create;

  Context := TRttiContext.Create;
  try
    RttiType := Context.GetType(AObject.ClassType);

    // Parcourir toutes les propriétés
    for RttiProp in RttiType.GetProperties do
    begin
      if RttiProp.IsReadable then
      begin
        Value := RttiProp.GetValue(AObject);

        // Vérifier tous les attributs de validation
        for Attr in RttiProp.GetAttributes do
        begin
          if Attr is ValidationAttribute then
          begin
            ValidationAttr := ValidationAttribute(Attr);
            if not ValidationAttr.Validate(Value) then
            begin
              Errors.Add(Format('%s: %s',
                [RttiProp.Name, ValidationAttr.GetErrorMessage]));
              Result := False;
            end;
          end;
        end;
      end;
    end;
  finally
    Context.Free;
  end;
end;

// Utilisation
var
  Product: TProduct;
  Errors: TStringList;
  I: Integer;
begin
  Product := TProduct.Create;
  try
    Product.Name := ''; // Vide, va échouer Required
    Product.Price := -10; // Négatif, va échouer Range
    Product.Stock := 100; // OK

    if not TValidator.ValidateObject(Product, Errors) then
    begin
      WriteLn('Erreurs de validation:');
      for I := 0 to Errors.Count - 1 do
        WriteLn('  - ', Errors[I]);
    end;

    Errors.Free;
  finally
    Product.Free;
  end;
end;
```

## Attributs de sérialisation

### Contrôler la sérialisation JSON

```pascal
type
  // Attributs pour contrôler la sérialisation
  JsonPropertyAttribute = class(TCustomAttribute)
  private
    FName: String;
  public
    constructor Create(const AName: String);
    property Name: String read FName;
  end;

  JsonIgnoreAttribute = class(TCustomAttribute)
  end;

  JsonRequiredAttribute = class(TCustomAttribute)
  end;

  DateFormatAttribute = class(TCustomAttribute)
  private
    FFormat: String;
  public
    constructor Create(const AFormat: String);
    property Format: String read FFormat;
  end;

constructor JsonPropertyAttribute.Create(const AName: String);
begin
  inherited Create;
  FName := AName;
end;

constructor DateFormatAttribute.Create(const AFormat: String);
begin
  inherited Create;
  FFormat := AFormat;
end;

// Classe avec attributs de sérialisation
type
  TCustomer = class
  private
    FId: Integer;
    FFullName: String;
    FEmail: String;
    FPassword: String;
    FRegistrationDate: TDateTime;
    FIsActive: Boolean;
  published
    [JsonProperty('customer_id')]
    [JsonRequired]
    property Id: Integer read FId write FId;

    [JsonProperty('full_name')]
    [JsonRequired]
    property FullName: String read FFullName write FFullName;

    [JsonProperty('email_address')]
    property Email: String read FEmail write FEmail;

    [JsonIgnore] // Ne pas sérialiser le mot de passe
    property Password: String read FPassword write FPassword;

    [JsonProperty('registered_on')]
    [DateFormat('yyyy-mm-dd')]
    property RegistrationDate: TDateTime read FRegistrationDate
                                         write FRegistrationDate;

    [JsonProperty('is_active')]
    property IsActive: Boolean read FIsActive write FIsActive;
  end;
```

### Sérialiseur JSON personnalisé

```pascal
uses
  Rtti, SysUtils, DateUtils, fpjson, jsonparser;

type
  TJsonSerializer = class
  private
    class function GetJsonPropertyName(AProp: TRttiProperty): String;
    class function ShouldSerialize(AProp: TRttiProperty): Boolean;
    class function FormatValue(AProp: TRttiProperty;
                               const Value: TValue): TJSONData;
  public
    class function Serialize(AObject: TObject): TJSONObject;
    class function SerializeToString(AObject: TObject): String;
  end;

class function TJsonSerializer.GetJsonPropertyName(AProp: TRttiProperty): String;
var
  Attr: TCustomAttribute;
begin
  Result := AProp.Name; // Nom par défaut

  for Attr in AProp.GetAttributes do
  begin
    if Attr is JsonPropertyAttribute then
    begin
      Result := JsonPropertyAttribute(Attr).Name;
      Break;
    end;
  end;
end;

class function TJsonSerializer.ShouldSerialize(AProp: TRttiProperty): Boolean;
var
  Attr: TCustomAttribute;
begin
  Result := True;

  for Attr in AProp.GetAttributes do
  begin
    if Attr is JsonIgnoreAttribute then
    begin
      Result := False;
      Break;
    end;
  end;
end;

class function TJsonSerializer.FormatValue(AProp: TRttiProperty;
                                          const Value: TValue): TJSONData;
var
  Attr: TCustomAttribute;
  DateFormat: String;
begin
  Result := nil;

  case Value.Kind of
    tkInteger, tkInt64:
      Result := TJSONIntegerNumber.Create(Value.AsInt64);

    tkFloat:
      begin
        // Vérifier si c'est une date
        DateFormat := '';
        for Attr in AProp.GetAttributes do
        begin
          if Attr is DateFormatAttribute then
          begin
            DateFormat := DateFormatAttribute(Attr).Format;
            Break;
          end;
        end;

        if DateFormat <> '' then
          Result := TJSONString.Create(
            FormatDateTime(DateFormat, Value.AsExtended))
        else
          Result := TJSONFloatNumber.Create(Value.AsExtended);
      end;

    tkString, tkUString:
      Result := TJSONString.Create(Value.AsString);

    tkEnumeration:
      begin
        if Value.TypeInfo = TypeInfo(Boolean) then
          Result := TJSONBoolean.Create(Value.AsBoolean)
        else
          Result := TJSONString.Create(Value.ToString);
      end;
  else
    Result := TJSONNull.Create;
  end;
end;

class function TJsonSerializer.Serialize(AObject: TObject): TJSONObject;
var
  Context: TRttiContext;
  RttiType: TRttiType;
  RttiProp: TRttiProperty;
  Value: TValue;
  JsonName: String;
  JsonValue: TJSONData;
begin
  Result := TJSONObject.Create;

  Context := TRttiContext.Create;
  try
    RttiType := Context.GetType(AObject.ClassType);

    for RttiProp in RttiType.GetProperties do
    begin
      if RttiProp.IsReadable and ShouldSerialize(RttiProp) then
      begin
        Value := RttiProp.GetValue(AObject);
        JsonName := GetJsonPropertyName(RttiProp);
        JsonValue := FormatValue(RttiProp, Value);

        if Assigned(JsonValue) then
          Result.Add(JsonName, JsonValue);
      end;
    end;
  finally
    Context.Free;
  end;
end;

class function TJsonSerializer.SerializeToString(AObject: TObject): String;
var
  Json: TJSONObject;
begin
  Json := Serialize(AObject);
  try
    Result := Json.FormatJSON;
  finally
    Json.Free;
  end;
end;

// Utilisation
var
  Customer: TCustomer;
  JsonStr: String;
begin
  Customer := TCustomer.Create;
  try
    Customer.Id := 123;
    Customer.FullName := 'Jean Dupont';
    Customer.Email := 'jean@example.com';
    Customer.Password := 'secret123'; // Ne sera pas sérialisé
    Customer.RegistrationDate := Now;
    Customer.IsActive := True;

    JsonStr := TJsonSerializer.SerializeToString(Customer);
    WriteLn(JsonStr);
    // Sortie :
    // {
    //   "customer_id": 123,
    //   "full_name": "Jean Dupont",
    //   "email_address": "jean@example.com",
    //   "registered_on": "2024-01-15",
    //   "is_active": true
    // }
  finally
    Customer.Free;
  end;
end;
```

## Attributs de mapping ORM

### Créer un mini-ORM avec des attributs

```pascal
type
  // Attributs pour le mapping base de données
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
    FColumnType: String;
    FSize: Integer;
  public
    constructor Create(const AColumnName: String;
                      const AColumnType: String = ''; ASize: Integer = 0);
    property ColumnName: String read FColumnName;
    property ColumnType: String read FColumnType;
    property Size: Integer read FSize;
  end;

  PrimaryKeyAttribute = class(TCustomAttribute)
  private
    FAutoIncrement: Boolean;
  public
    constructor Create(AAutoIncrement: Boolean = True);
    property AutoIncrement: Boolean read FAutoIncrement;
  end;

  ForeignKeyAttribute = class(TCustomAttribute)
  private
    FReferenceTable: String;
    FReferenceColumn: String;
  public
    constructor Create(const AReferenceTable, AReferenceColumn: String);
    property ReferenceTable: String read FReferenceTable;
    property ReferenceColumn: String read FReferenceColumn;
  end;

  IndexAttribute = class(TCustomAttribute)
  private
    FIndexName: String;
    FUnique: Boolean;
  public
    constructor Create(const AIndexName: String; AUnique: Boolean = False);
    property IndexName: String read FIndexName;
    property Unique: Boolean read FUnique;
  end;

// Implémentations des constructeurs
constructor TableAttribute.Create(const ATableName: String);
begin
  inherited Create;
  FTableName := ATableName;
end;

constructor ColumnAttribute.Create(const AColumnName: String;
                                  const AColumnType: String; ASize: Integer);
begin
  inherited Create;
  FColumnName := AColumnName;
  FColumnType := AColumnType;
  FSize := ASize;
end;

constructor PrimaryKeyAttribute.Create(AAutoIncrement: Boolean);
begin
  inherited Create;
  FAutoIncrement := AAutoIncrement;
end;

constructor ForeignKeyAttribute.Create(const AReferenceTable,
                                      AReferenceColumn: String);
begin
  inherited Create;
  FReferenceTable := AReferenceTable;
  FReferenceColumn := AReferenceColumn;
end;

constructor IndexAttribute.Create(const AIndexName: String; AUnique: Boolean);
begin
  inherited Create;
  FIndexName := AIndexName;
  FUnique := AUnique;
end;
```

### Modèle de données avec attributs ORM

```pascal
type
  [Table('categories')]
  TCategory = class
  private
    FId: Integer;
    FName: String;
    FDescription: String;
  published
    [PrimaryKey(True)]
    [Column('category_id', 'INTEGER')]
    property Id: Integer read FId write FId;

    [Column('category_name', 'VARCHAR', 100)]
    [Index('idx_category_name', True)]
    property Name: String read FName write FName;

    [Column('description', 'TEXT')]
    property Description: String read FDescription write FDescription;
  end;

  [Table('products')]
  TProduct = class
  private
    FId: Integer;
    FName: String;
    FPrice: Double;
    FCategoryId: Integer;
    FCreatedAt: TDateTime;
    FIsAvailable: Boolean;
  published
    [PrimaryKey(True)]
    [Column('product_id', 'INTEGER')]
    property Id: Integer read FId write FId;

    [Column('product_name', 'VARCHAR', 200)]
    [Index('idx_product_name')]
    property Name: String read FName write FName;

    [Column('price', 'DECIMAL(10,2)')]
    property Price: Double read FPrice write FPrice;

    [Column('category_id', 'INTEGER')]
    [ForeignKey('categories', 'category_id')]
    [Index('idx_category_id')]
    property CategoryId: Integer read FCategoryId write FCategoryId;

    [Column('created_at', 'TIMESTAMP')]
    property CreatedAt: TDateTime read FCreatedAt write FCreatedAt;

    [Column('is_available', 'BOOLEAN')]
    property IsAvailable: Boolean read FIsAvailable write FIsAvailable;
  end;
```

### Générateur de schéma SQL

```pascal
type
  TSQLGenerator = class
  private
    class function GetTableName(AClass: TClass): String;
    class function GenerateColumnDefinition(AProp: TRttiProperty): String;
    class function GenerateConstraints(AClass: TClass): TStringList;
  public
    class function GenerateCreateTable(AClass: TClass): String;
    class function GenerateInsert(AObject: TObject): String;
    class function GenerateUpdate(AObject: TObject): String;
    class function GenerateSelect(AClass: TClass;
                                  const WhereClause: String = ''): String;
  end;

class function TSQLGenerator.GetTableName(AClass: TClass): String;
var
  Context: TRttiContext;
  RttiType: TRttiType;
  Attr: TCustomAttribute;
begin
  Result := LowerCase(AClass.ClassName);

  Context := TRttiContext.Create;
  try
    RttiType := Context.GetType(AClass);

    for Attr in RttiType.GetAttributes do
    begin
      if Attr is TableAttribute then
      begin
        Result := TableAttribute(Attr).TableName;
        Break;
      end;
    end;
  finally
    Context.Free;
  end;
end;

class function TSQLGenerator.GenerateColumnDefinition(AProp: TRttiProperty): String;
var
  Attr: TCustomAttribute;
  ColumnName, ColumnType: String;
  IsPrimaryKey, IsAutoIncrement: Boolean;
begin
  ColumnName := AProp.Name;
  ColumnType := 'VARCHAR(255)'; // Type par défaut
  IsPrimaryKey := False;
  IsAutoIncrement := False;

  // Lire les attributs
  for Attr in AProp.GetAttributes do
  begin
    if Attr is ColumnAttribute then
    begin
      ColumnName := ColumnAttribute(Attr).ColumnName;
      if ColumnAttribute(Attr).ColumnType <> '' then
        ColumnType := ColumnAttribute(Attr).ColumnType
      else if ColumnAttribute(Attr).Size > 0 then
        ColumnType := Format('VARCHAR(%d)', [ColumnAttribute(Attr).Size]);
    end
    else if Attr is PrimaryKeyAttribute then
    begin
      IsPrimaryKey := True;
      IsAutoIncrement := PrimaryKeyAttribute(Attr).AutoIncrement;
    end;
  end;

  Result := Format('  %s %s', [ColumnName, ColumnType]);

  if IsPrimaryKey then
  begin
    Result := Result + ' PRIMARY KEY';
    if IsAutoIncrement then
      Result := Result + ' AUTOINCREMENT';
  end;
end;

class function TSQLGenerator.GenerateCreateTable(AClass: TClass): String;
var
  Context: TRttiContext;
  RttiType: TRttiType;
  RttiProp: TRttiProperty;
  TableName: String;
  Columns: TStringList;
  Constraints: TStringList;
begin
  Context := TRttiContext.Create;
  Columns := TStringList.Create;
  try
    RttiType := Context.GetType(AClass);
    TableName := GetTableName(AClass);

    // Générer les colonnes
    for RttiProp in RttiType.GetProperties do
    begin
      if RttiProp.Visibility = mvPublished then
        Columns.Add(GenerateColumnDefinition(RttiProp));
    end;

    // Générer les contraintes
    Constraints := GenerateConstraints(AClass);
    try
      if Constraints.Count > 0 then
        Columns.AddStrings(Constraints);

      Result := Format('CREATE TABLE %s (%s%s%s);',
                      [TableName, sLineBreak,
                       Columns.CommaText.Replace(',', ',' + sLineBreak),
                       sLineBreak]);
    finally
      Constraints.Free;
    end;
  finally
    Columns.Free;
    Context.Free;
  end;
end;

// Utilisation
begin
  WriteLn(TSQLGenerator.GenerateCreateTable(TProduct));
  // Génère :
  // CREATE TABLE products (
  //   product_id INTEGER PRIMARY KEY AUTOINCREMENT,
  //   product_name VARCHAR(200),
  //   price DECIMAL(10,2),
  //   category_id INTEGER,
  //   created_at TIMESTAMP,
  //   is_available BOOLEAN,
  //   FOREIGN KEY (category_id) REFERENCES categories(category_id)
  // );
end;
```

## Attributs de configuration

### Système de configuration automatique

```pascal
type
  // Attributs de configuration
  ConfigSectionAttribute = class(TCustomAttribute)
  private
    FSection: String;
  public
    constructor Create(const ASection: String);
    property Section: String read FSection;
  end;

  ConfigKeyAttribute = class(TCustomAttribute)
  private
    FKey: String;
    FDefaultValue: String;
  public
    constructor Create(const AKey: String; const ADefaultValue: String = '');
    property Key: String read FKey;
    property DefaultValue: String read FDefaultValue;
  end;

  EnvironmentVariableAttribute = class(TCustomAttribute)
  private
    FVarName: String;
  public
    constructor Create(const AVarName: String);
    property VarName: String read FVarName;
  end;

// Classe de configuration avec attributs
type
  [ConfigSection('database')]
  TDatabaseConfig = class
  private
    FHost: String;
    FPort: Integer;
    FDatabase: String;
    FUsername: String;
    FPassword: String;
  published
    [ConfigKey('host', 'localhost')]
    [EnvironmentVariable('DB_HOST')]
    property Host: String read FHost write FHost;

    [ConfigKey('port', '5432')]
    [EnvironmentVariable('DB_PORT')]
    property Port: Integer read FPort write FPort;

    [ConfigKey('database', 'myapp')]
    [EnvironmentVariable('DB_NAME')]
    property Database: String read FDatabase write FDatabase;

    [ConfigKey('username', 'user')]
    [EnvironmentVariable('DB_USER')]
    property Username: String read FUsername write FUsername;

    [ConfigKey('password')]
    [EnvironmentVariable('DB_PASSWORD')]
    property Password: String read FPassword write FPassword;
  end;

  [ConfigSection('application')]
  TApplicationConfig = class
  private
    FAppName: String;
    FVersion: String;
    FDebugMode: Boolean;
    FMaxConnections: Integer;
  published
    [ConfigKey('name', 'MyApplication')]
    property AppName: String read FAppName write FAppName;

    [ConfigKey('version', '1.0.0')]
    property Version: String read FVersion write FVersion;

    [ConfigKey('debug', 'false')]
    [EnvironmentVariable('APP_DEBUG')]
    property DebugMode: Boolean read FDebugMode write FDebugMode;

    [ConfigKey('max_connections', '100')]
    property MaxConnections: Integer read FMaxConnections write FMaxConnections;
  end;
```

### Chargeur de configuration

```pascal
uses
  IniFiles, SysUtils;

type
  TConfigLoader = class
  private
    class function GetEnvironmentValue(const VarName: String;
                                       const DefaultValue: String): String;
    class function ConvertValue(const StrValue: String;
                                TypeKind: TTypeKind): TValue;
  public
    class procedure LoadFromIniFile(AObject: TObject; const FileName: String);
    class procedure LoadFromEnvironment(AObject: TObject);
    class procedure SaveToIniFile(AObject: TObject; const FileName: String);
  end;

class function TConfigLoader.GetEnvironmentValue(const VarName: String;
                                                 const DefaultValue: String): String;
begin
  Result := GetEnvironmentVariable(VarName);
  if Result = '' then
    Result := DefaultValue;
end;

class function TConfigLoader.ConvertValue(const StrValue: String;
                                          TypeKind: TTypeKind): TValue;
begin
  case TypeKind of
    tkInteger:
      Result := StrToIntDef(StrValue, 0);
    tkFloat:
      Result := StrToFloatDef(StrValue, 0.0);
    tkEnumeration:
      begin
        // Pour les booléens
        if LowerCase(StrValue) = 'true' then
          Result := True
        else if LowerCase(StrValue) = 'false' then
          Result := False
        else
          Result := StrToIntDef(StrValue, 0) <> 0;
      end;
  else
    Result := StrValue;
  end;
end;

class procedure TConfigLoader.LoadFromIniFile(AObject: TObject;
                                              const FileName: String);
var
  Context: TRttiContext;
  RttiType: TRttiType;
  RttiProp: TRttiProperty;
  Attr: TCustomAttribute;
  IniFile: TIniFile;
  Section, Key, DefaultValue, Value: String;
begin
  if not FileExists(FileName) then
    Exit;

  Context := TRttiContext.Create;
  IniFile := TIniFile.Create(FileName);
  try
    RttiType := Context.GetType(AObject.ClassType);

    // Obtenir la section
    Section := 'default';
    for Attr in RttiType.GetAttributes do
    begin
      if Attr is ConfigSectionAttribute then
      begin
        Section := ConfigSectionAttribute(Attr).Section;
        Break;
      end;
    end;

    // Charger les propriétés
    for RttiProp in RttiType.GetProperties do
    begin
      if RttiProp.IsWritable then
      begin
        Key := RttiProp.Name;
        DefaultValue := '';

        // Chercher l'attribut ConfigKey
        for Attr in RttiProp.GetAttributes do
        begin
          if Attr is ConfigKeyAttribute then
          begin
            Key := ConfigKeyAttribute(Attr).Key;
            DefaultValue := ConfigKeyAttribute(Attr).DefaultValue;
            Break;
          end;
        end;

        // Lire la valeur
        Value := IniFile.ReadString(Section, Key, DefaultValue);

        // Convertir et assigner
        RttiProp.SetValue(AObject,
          ConvertValue(Value, RttiProp.PropertyType.TypeKind));
      end;
    end;
  finally
    IniFile.Free;
    Context.Free;
  end;
end;

class procedure TConfigLoader.LoadFromEnvironment(AObject: TObject);
var
  Context: TRttiContext;
  RttiType: TRttiType;
  RttiProp: TRttiProperty;
  Attr: TCustomAttribute;
  EnvVar, DefaultValue, Value: String;
begin
  Context := TRttiContext.Create;
  try
    RttiType := Context.GetType(AObject.ClassType);

    for RttiProp in RttiType.GetProperties do
    begin
      if RttiProp.IsWritable then
      begin
        EnvVar := '';
        DefaultValue := '';

        // Chercher les attributs
        for Attr in RttiProp.GetAttributes do
        begin
          if Attr is EnvironmentVariableAttribute then
            EnvVar := EnvironmentVariableAttribute(Attr).VarName
          else if Attr is ConfigKeyAttribute then
            DefaultValue := ConfigKeyAttribute(Attr).DefaultValue;
        end;

        // Si une variable d'environnement est définie
        if EnvVar <> '' then
        begin
          Value := GetEnvironmentValue(EnvVar, DefaultValue);
          RttiProp.SetValue(AObject,
            ConvertValue(Value, RttiProp.PropertyType.TypeKind));
        end;
      end;
    end;
  finally
    Context.Free;
  end;
end;

class procedure TConfigLoader.SaveToIniFile(AObject: TObject;
                                           const FileName: String);
var
  Context: TRttiContext;
  RttiType: TRttiType;
  RttiProp: TRttiProperty;
  Attr: TCustomAttribute;
  IniFile: TIniFile;
  Section, Key: String;
  Value: TValue;
begin
  Context := TRttiContext.Create;
  IniFile := TIniFile.Create(FileName);
  try
    RttiType := Context.GetType(AObject.ClassType);

    // Obtenir la section
    Section := 'default';
    for Attr in RttiType.GetAttributes do
    begin
      if Attr is ConfigSectionAttribute then
      begin
        Section := ConfigSectionAttribute(Attr).Section;
        Break;
      end;
    end;

    // Sauvegarder les propriétés
    for RttiProp in RttiType.GetProperties do
    begin
      if RttiProp.IsReadable then
      begin
        Key := RttiProp.Name;

        // Chercher l'attribut ConfigKey
        for Attr in RttiProp.GetAttributes do
        begin
          if Attr is ConfigKeyAttribute then
          begin
            Key := ConfigKeyAttribute(Attr).Key;
            Break;
          end;
        end;

        // Obtenir et écrire la valeur
        Value := RttiProp.GetValue(AObject);
        IniFile.WriteString(Section, Key, Value.ToString);
      end;
    end;
  finally
    IniFile.Free;
    Context.Free;
  end;
end;

// Utilisation
var
  DbConfig: TDatabaseConfig;
begin
  DbConfig := TDatabaseConfig.Create;
  try
    // Charger depuis un fichier INI
    TConfigLoader.LoadFromIniFile(DbConfig, 'config.ini');

    // Surcharger avec les variables d'environnement
    TConfigLoader.LoadFromEnvironment(DbConfig);

    WriteLn('Configuration de la base de données:');
    WriteLn('  Host: ', DbConfig.Host);
    WriteLn('  Port: ', DbConfig.Port);
    WriteLn('  Database: ', DbConfig.Database);

    // Sauvegarder la configuration
    TConfigLoader.SaveToIniFile(DbConfig, 'config_saved.ini');
  finally
    DbConfig.Free;
  end;
end;
```

## Attributs de permissions et sécurité

### Système de contrôle d'accès basé sur les attributs

```pascal
type
  // Attributs de sécurité
  RequireAuthenticationAttribute = class(TCustomAttribute)
  end;

  RequireRoleAttribute = class(TCustomAttribute)
  private
    FRole: String;
  public
    constructor Create(const ARole: String);
    property Role: String read FRole;
  end;

  RequirePermissionAttribute = class(TCustomAttribute)
  private
    FPermission: String;
  public
    constructor Create(const APermission: String);
    property Permission: String read FPermission;
  end;

  RateLimitAttribute = class(TCustomAttribute)
  private
    FMaxRequests: Integer;
    FTimeWindow: Integer; // en secondes
  public
    constructor Create(AMaxRequests: Integer; ATimeWindow: Integer);
    property MaxRequests: Integer read FMaxRequests;
    property TimeWindow: Integer read FTimeWindow;
  end;

constructor RequireRoleAttribute.Create(const ARole: String);
begin
  inherited Create;
  FRole := ARole;
end;

constructor RequirePermissionAttribute.Create(const APermission: String);
begin
  inherited Create;
  FPermission := APermission;
end;

constructor RateLimitAttribute.Create(AMaxRequests: Integer;
                                     ATimeWindow: Integer);
begin
  inherited Create;
  FMaxRequests := AMaxRequests;
  FTimeWindow := ATimeWindow;
end;

// Contrôleur avec attributs de sécurité
type
  TUserController = class
  published
    [RequireAuthentication]
    [RequireRole('user')]
    procedure GetProfile;

    [RequireAuthentication]
    [RequireRole('admin')]
    procedure ListAllUsers;

    [RequireAuthentication]
    [RequirePermission('user.edit')]
    procedure UpdateProfile;

    [RequireAuthentication]
    [RequireRole('admin')]
    [RequirePermission('user.delete')]
    procedure DeleteUser(UserId: Integer);

    [RateLimit(10, 60)] // 10 requêtes par minute
    procedure Login(const Username, Password: String);

    [RateLimit(5, 300)] // 5 requêtes par 5 minutes
    procedure ResetPassword(const Email: String);
  end;

// Système de vérification des permissions
type
  TSecurityContext = class
  private
    FCurrentUser: String;
    FRoles: TStringList;
    FPermissions: TStringList;
  public
    constructor Create;
    destructor Destroy; override;

    property CurrentUser: String read FCurrentUser write FCurrentUser;
    procedure AddRole(const Role: String);
    procedure AddPermission(const Permission: String);
    function HasRole(const Role: String): Boolean;
    function HasPermission(const Permission: String): Boolean;
    function IsAuthenticated: Boolean;
  end;

constructor TSecurityContext.Create;
begin
  FRoles := TStringList.Create;
  FPermissions := TStringList.Create;
end;

destructor TSecurityContext.Destroy;
begin
  FRoles.Free;
  FPermissions.Free;
  inherited;
end;

procedure TSecurityContext.AddRole(const Role: String);
begin
  if FRoles.IndexOf(Role) < 0 then
    FRoles.Add(Role);
end;

procedure TSecurityContext.AddPermission(const Permission: String);
begin
  if FPermissions.IndexOf(Permission) < 0 then
    FPermissions.Add(Permission);
end;

function TSecurityContext.HasRole(const Role: String): Boolean;
begin
  Result := FRoles.IndexOf(Role) >= 0;
end;

function TSecurityContext.HasPermission(const Permission: String): Boolean;
begin
  Result := FPermissions.IndexOf(Permission) >= 0;
end;

function TSecurityContext.IsAuthenticated: Boolean;
begin
  Result := FCurrentUser <> '';
end;

// Intercepteur de sécurité
type
  TSecurityInterceptor = class
  private
    class function CheckMethodAccess(AMethod: TRttiMethod;
                                     Context: TSecurityContext): Boolean;
  public
    class procedure InvokeSecure(AObject: TObject; const MethodName: String;
                                 const Args: array of TValue;
                                 Context: TSecurityContext);
  end;

class function TSecurityInterceptor.CheckMethodAccess(AMethod: TRttiMethod;
                                                      Context: TSecurityContext): Boolean;
var
  Attr: TCustomAttribute;
begin
  Result := True;

  for Attr in AMethod.GetAttributes do
  begin
    // Vérifier l'authentification
    if Attr is RequireAuthenticationAttribute then
    begin
      if not Context.IsAuthenticated then
      begin
        raise Exception.Create('Authentification requise');
      end;
    end

    // Vérifier le rôle
    else if Attr is RequireRoleAttribute then
    begin
      if not Context.HasRole(RequireRoleAttribute(Attr).Role) then
      begin
        raise Exception.CreateFmt('Rôle "%s" requis',
          [RequireRoleAttribute(Attr).Role]);
      end;
    end

    // Vérifier la permission
    else if Attr is RequirePermissionAttribute then
    begin
      if not Context.HasPermission(RequirePermissionAttribute(Attr).Permission) then
      begin
        raise Exception.CreateFmt('Permission "%s" requise',
          [RequirePermissionAttribute(Attr).Permission]);
      end;
    end;
  end;
end;

class procedure TSecurityInterceptor.InvokeSecure(AObject: TObject;
                                                  const MethodName: String;
                                                  const Args: array of TValue;
                                                  Context: TSecurityContext);
var
  RttiContext: TRttiContext;
  RttiType: TRttiType;
  RttiMethod: TRttiMethod;
begin
  RttiContext := TRttiContext.Create;
  try
    RttiType := RttiContext.GetType(AObject.ClassType);
    RttiMethod := RttiType.GetMethod(MethodName);

    if not Assigned(RttiMethod) then
      raise Exception.CreateFmt('Méthode "%s" non trouvée', [MethodName]);

    // Vérifier les permissions
    if CheckMethodAccess(RttiMethod, Context) then
    begin
      // Exécuter la méthode
      RttiMethod.Invoke(AObject, Args);
      WriteLn('Méthode "', MethodName, '" exécutée avec succès');
    end;
  finally
    RttiContext.Free;
  end;
end;

// Utilisation
var
  Controller: TUserController;
  SecurityCtx: TSecurityContext;
begin
  Controller := TUserController.Create;
  SecurityCtx := TSecurityContext.Create;
  try
    // Tentative sans authentification
    try
      TSecurityInterceptor.InvokeSecure(Controller, 'GetProfile', [],
                                        SecurityCtx);
    except
      on E: Exception do
        WriteLn('Erreur: ', E.Message); // "Authentification requise"
    end;

    // S'authentifier et réessayer
    SecurityCtx.CurrentUser := 'john.doe';
    SecurityCtx.AddRole('user');

    TSecurityInterceptor.InvokeSecure(Controller, 'GetProfile', [],
                                      SecurityCtx); // OK

    // Tentative d'accès admin
    try
      TSecurityInterceptor.InvokeSecure(Controller, 'ListAllUsers', [],
                                        SecurityCtx);
    except
      on E: Exception do
        WriteLn('Erreur: ', E.Message); // "Rôle "admin" requis"
    end;
  finally
    Controller.Free;
    SecurityCtx.Free;
  end;
end;
```

## Attributs de cache et performance

### Système de cache avec attributs

```pascal
type
  // Attributs de cache
  CacheableAttribute = class(TCustomAttribute)
  private
    FDuration: Integer; // Durée en secondes
  public
    constructor Create(ADuration: Integer = 300); // 5 minutes par défaut
    property Duration: Integer read FDuration;
  end;

  CacheKeyAttribute = class(TCustomAttribute)
  private
    FKeyPattern: String;
  public
    constructor Create(const AKeyPattern: String);
    property KeyPattern: String read FKeyPattern;
  end;

  InvalidateCacheAttribute = class(TCustomAttribute)
  private
    FPattern: String;
  public
    constructor Create(const APattern: String);
    property Pattern: String read FPattern;
  end;

constructor CacheableAttribute.Create(ADuration: Integer);
begin
  inherited Create;
  FDuration := ADuration;
end;

constructor CacheKeyAttribute.Create(const AKeyPattern: String);
begin
  inherited Create;
  FKeyPattern := AKeyPattern;
end;

constructor InvalidateCacheAttribute.Create(const APattern: String);
begin
  inherited Create;
  FPattern := APattern;
end;

// Service avec cache
type
  TDataService = class
  published
    [Cacheable(600)] // Cache pour 10 minutes
    [CacheKey('users:all')]
    function GetAllUsers: TStringList;

    [Cacheable(300)]
    [CacheKey('user:{0}')] // {0} sera remplacé par le paramètre
    function GetUserById(UserId: Integer): String;

    [InvalidateCache('users:*')] // Invalide tout le cache des utilisateurs
    procedure UpdateUser(UserId: Integer; const Name: String);

    [InvalidateCache('user:{0}')]
    procedure DeleteUser(UserId: Integer);
  end;

// Gestionnaire de cache
type
  TCacheEntry = record
    Value: TValue;
    ExpiresAt: TDateTime;
  end;

  TCacheManager = class
  private
    class var FCache: TDictionary<String, TCacheEntry>;
    class constructor Create;
    class destructor Destroy;
  public
    class function Get(const Key: String; out Value: TValue): Boolean;
    class procedure Put(const Key: String; const Value: TValue;
                       Duration: Integer);
    class procedure Invalidate(const Pattern: String);
    class procedure Clear;
  end;

class constructor TCacheManager.Create;
begin
  FCache := TDictionary<String, TCacheEntry>.Create;
end;

class destructor TCacheManager.Destroy;
begin
  FCache.Free;
end;

class function TCacheManager.Get(const Key: String; out Value: TValue): Boolean;
var
  Entry: TCacheEntry;
begin
  Result := False;

  if FCache.TryGetValue(Key, Entry) then
  begin
    if Entry.ExpiresAt > Now then
    begin
      Value := Entry.Value;
      Result := True;
      WriteLn('[CACHE HIT] ', Key);
    end
    else
    begin
      FCache.Remove(Key);
      WriteLn('[CACHE EXPIRED] ', Key);
    end;
  end
  else
    WriteLn('[CACHE MISS] ', Key);
end;

class procedure TCacheManager.Put(const Key: String; const Value: TValue;
                                  Duration: Integer);
var
  Entry: TCacheEntry;
begin
  Entry.Value := Value;
  Entry.ExpiresAt := IncSecond(Now, Duration);
  FCache.AddOrSetValue(Key, Entry);
  WriteLn('[CACHE PUT] ', Key, ' (expires in ', Duration, 's)');
end;

class procedure TCacheManager.Invalidate(const Pattern: String);
var
  Key: String;
  KeysToDelete: TStringList;
begin
  KeysToDelete := TStringList.Create;
  try
    // Trouver les clés correspondant au pattern
    for Key in FCache.Keys do
    begin
      if Pos(StringReplace(Pattern, '*', '', []), Key) > 0 then
        KeysToDelete.Add(Key);
    end;

    // Supprimer les clés
    for Key in KeysToDelete do
    begin
      FCache.Remove(Key);
      WriteLn('[CACHE INVALIDATE] ', Key);
    end;
  finally
    KeysToDelete.Free;
  end;
end;

class procedure TCacheManager.Clear;
begin
  FCache.Clear;
  WriteLn('[CACHE CLEAR] All cache cleared');
end;

// Proxy de cache
type
  TCacheProxy = class
  private
    class function GenerateCacheKey(const Pattern: String;
                                    const Args: array of TValue): String;
  public
    class function InvokeWithCache(AObject: TObject; const MethodName: String;
                                   const Args: array of TValue): TValue;
  end;

class function TCacheProxy.GenerateCacheKey(const Pattern: String;
                                           const Args: array of TValue): String;
var
  I: Integer;
begin
  Result := Pattern;
  for I := Low(Args) to High(Args) do
  begin
    Result := StringReplace(Result, '{' + IntToStr(I) + '}',
                           Args[I].ToString, [rfReplaceAll]);
  end;
end;

class function TCacheProxy.InvokeWithCache(AObject: TObject;
                                          const MethodName: String;
                                          const Args: array of TValue): TValue;
var
  Context: TRttiContext;
  RttiType: TRttiType;
  RttiMethod: TRttiMethod;
  Attr: TCustomAttribute;
  CacheKey: String;
  CacheDuration: Integer;
  ShouldCache: Boolean;
  InvalidatePattern: String;
begin
  Context := TRttiContext.Create;
  try
    RttiType := Context.GetType(AObject.ClassType);
    RttiMethod := RttiType.GetMethod(MethodName);

    if not Assigned(RttiMethod) then
      raise Exception.CreateFmt('Méthode "%s" non trouvée', [MethodName]);

    ShouldCache := False;
    CacheDuration := 0;
    CacheKey := MethodName;
    InvalidatePattern := '';

    // Analyser les attributs
    for Attr in RttiMethod.GetAttributes do
    begin
      if Attr is CacheableAttribute then
      begin
        ShouldCache := True;
        CacheDuration := CacheableAttribute(Attr).Duration;
      end
      else if Attr is CacheKeyAttribute then
      begin
        CacheKey := GenerateCacheKey(CacheKeyAttribute(Attr).KeyPattern, Args);
      end
      else if Attr is InvalidateCacheAttribute then
      begin
        InvalidatePattern := GenerateCacheKey(
          InvalidateCacheAttribute(Attr).Pattern, Args);
      end;
    end;

    // Invalider le cache si nécessaire
    if InvalidatePattern <> '' then
      TCacheManager.Invalidate(InvalidatePattern);

    // Vérifier le cache
    if ShouldCache then
    begin
      if TCacheManager.Get(CacheKey, Result) then
        Exit; // Retour depuis le cache
    end;

    // Appeler la méthode
    Result := RttiMethod.Invoke(AObject, Args);

    // Mettre en cache si nécessaire
    if ShouldCache then
      TCacheManager.Put(CacheKey, Result, CacheDuration);

  finally
    Context.Free;
  end;
end;
```

## Attributs de documentation et métadonnées

### Génération automatique de documentation

```pascal
type
  // Attributs de documentation
  DocumentationAttribute = class(TCustomAttribute)
  private
    FDescription: String;
  public
    constructor Create(const ADescription: String);
    property Description: String read FDescription;
  end;

  ExampleAttribute = class(TCustomAttribute)
  private
    FCode: String;
  public
    constructor Create(const ACode: String);
    property Code: String read FCode;
  end;

  DeprecatedAttribute = class(TCustomAttribute)
  private
    FReason: String;
    FAlternative: String;
  public
    constructor Create(const AReason: String;
                      const AAlternative: String = '');
    property Reason: String read FReason;
    property Alternative: String read FAlternative;
  end;

  AuthorAttribute = class(TCustomAttribute)
  private
    FName: String;
    FEmail: String;
  public
    constructor Create(const AName, AEmail: String);
    property Name: String read FName;
    property Email: String read FEmail;
  end;

  VersionAttribute = class(TCustomAttribute)
  private
    FVersion: String;
    FDate: TDateTime;
  public
    constructor Create(const AVersion: String; ADate: TDateTime);
    property Version: String read FVersion;
    property Date: TDateTime read FDate;
  end;

  ParamAttribute = class(TCustomAttribute)
  private
    FName: String;
    FDescription: String;
    FRequired: Boolean;
  public
    constructor Create(const AName, ADescription: String;
                      ARequired: Boolean = True);
    property Name: String read FName;
    property Description: String read FDescription;
    property Required: Boolean read FRequired;
  end;

  ReturnAttribute = class(TCustomAttribute)
  private
    FDescription: String;
  public
    constructor Create(const ADescription: String);
    property Description: String read FDescription;
  end;

// Implémentations
constructor DocumentationAttribute.Create(const ADescription: String);
begin
  inherited Create;
  FDescription := ADescription;
end;

constructor ExampleAttribute.Create(const ACode: String);
begin
  inherited Create;
  FCode := ACode;
end;

constructor DeprecatedAttribute.Create(const AReason: String;
                                      const AAlternative: String);
begin
  inherited Create;
  FReason := AReason;
  FAlternative := AAlternative;
end;

constructor AuthorAttribute.Create(const AName, AEmail: String);
begin
  inherited Create;
  FName := AName;
  FEmail := AEmail;
end;

constructor VersionAttribute.Create(const AVersion: String; ADate: TDateTime);
begin
  inherited Create;
  FVersion := AVersion;
  FDate := ADate;
end;

constructor ParamAttribute.Create(const AName, ADescription: String;
                                 ARequired: Boolean);
begin
  inherited Create;
  FName := AName;
  FDescription := ADescription;
  FRequired := ARequired;
end;

constructor ReturnAttribute.Create(const ADescription: String);
begin
  inherited Create;
  FDescription := ADescription;
end;

// API documentée avec attributs
type
  [Documentation('Service de gestion des utilisateurs')]
  [Author('Jean Dupont', 'jean.dupont@example.com')]
  [Version('2.0.0', EncodeDate(2024, 1, 15))]
  TUserService = class
  published
    [Documentation('Récupère un utilisateur par son identifiant')]
    [Param('UserId', 'Identifiant unique de l''utilisateur', True)]
    [Return('Objet utilisateur ou nil si non trouvé')]
    [Example('user := GetUser(123);')]
    function GetUser(UserId: Integer): TObject;

    [Documentation('Crée un nouvel utilisateur dans le système')]
    [Param('Name', 'Nom complet de l''utilisateur', True)]
    [Param('Email', 'Adresse email valide', True)]
    [Return('Identifiant du nouvel utilisateur créé')]
    [Example('newId := CreateUser("John Doe", "john@example.com");')]
    function CreateUser(const Name, Email: String): Integer;

    [Deprecated('Utiliser GetUser à la place', 'GetUser')]
    [Documentation('Ancienne méthode pour récupérer un utilisateur')]
    function FindUser(UserId: Integer): TObject;

    [Documentation('Met à jour les informations d''un utilisateur')]
    [Param('UserId', 'Identifiant de l''utilisateur', True)]
    [Param('Name', 'Nouveau nom', False)]
    [Param('Email', 'Nouvelle adresse email', False)]
    procedure UpdateUser(UserId: Integer; const Name, Email: String);
  end;

// Générateur de documentation
type
  TDocumentationGenerator = class
  private
    class procedure AppendMethodDoc(SB: TStringBuilder;
                                   Method: TRttiMethod);
  public
    class function GenerateMarkdown(AClass: TClass): String;
    class function GenerateHTML(AClass: TClass): String;
    class function GenerateXML(AClass: TClass): String;
  end;

class procedure TDocumentationGenerator.AppendMethodDoc(SB: TStringBuilder;
                                                       Method: TRttiMethod);
var
  Attr: TCustomAttribute;
  HasDeprecation: Boolean;
begin
  SB.AppendLine;
  SB.AppendLine('### ' + Method.Name);

  HasDeprecation := False;

  // Parcourir les attributs
  for Attr in Method.GetAttributes do
  begin
    if Attr is DeprecatedAttribute then
    begin
      HasDeprecation := True;
      SB.AppendLine;
      SB.AppendLine('> ⚠️ **DÉPRÉCIÉ:** ' +
                   DeprecatedAttribute(Attr).Reason);
      if DeprecatedAttribute(Attr).Alternative <> '' then
        SB.AppendLine('> Utiliser `' +
                     DeprecatedAttribute(Attr).Alternative +
                     '` à la place.');
      SB.AppendLine;
    end;
  end;

  // Description
  for Attr in Method.GetAttributes do
  begin
    if Attr is DocumentationAttribute then
    begin
      SB.AppendLine;
      SB.AppendLine(DocumentationAttribute(Attr).Description);
    end;
  end;

  // Paramètres
  SB.AppendLine;
  SB.AppendLine('**Paramètres:**');
  for Attr in Method.GetAttributes do
  begin
    if Attr is ParamAttribute then
    begin
      SB.Append('- `' + ParamAttribute(Attr).Name + '`: ');
      SB.Append(ParamAttribute(Attr).Description);
      if ParamAttribute(Attr).Required then
        SB.Append(' *(requis)*');
      SB.AppendLine;
    end;
  end;

  // Valeur de retour
  for Attr in Method.GetAttributes do
  begin
    if Attr is ReturnAttribute then
    begin
      SB.AppendLine;
      SB.AppendLine('**Retourne:** ' + ReturnAttribute(Attr).Description);
    end;
  end;

  // Exemples
  for Attr in Method.GetAttributes do
  begin
    if Attr is ExampleAttribute then
    begin
      SB.AppendLine;
      SB.AppendLine('**Exemple:**');
      SB.AppendLine('```pascal');
      SB.AppendLine(ExampleAttribute(Attr).Code);
      SB.AppendLine('```');
    end;
  end;
end;

class function TDocumentationGenerator.GenerateMarkdown(AClass: TClass): String;
var
  Context: TRttiContext;
  RttiType: TRttiType;
  RttiMethod: TRttiMethod;
  Attr: TCustomAttribute;
  SB: TStringBuilder;
begin
  Context := TRttiContext.Create;
  SB := TStringBuilder.Create;
  try
    RttiType := Context.GetType(AClass);

    // En-tête
    SB.AppendLine('# ' + RttiType.Name);
    SB.AppendLine;

    // Métadonnées de la classe
    for Attr in RttiType.GetAttributes do
    begin
      if Attr is DocumentationAttribute then
      begin
        SB.AppendLine(DocumentationAttribute(Attr).Description);
        SB.AppendLine;
      end
      else if Attr is AuthorAttribute then
      begin
        SB.AppendLine('**Auteur:** ' + AuthorAttribute(Attr).Name +
                     ' <' + AuthorAttribute(Attr).Email + '>');
      end
      else if Attr is VersionAttribute then
      begin
        SB.AppendLine('**Version:** ' + VersionAttribute(Attr).Version +
                     ' (' + DateToStr(VersionAttribute(Attr).Date) + ')');
      end;
    end;

    // Méthodes
    SB.AppendLine;
    SB.AppendLine('## Méthodes');

    for RttiMethod in RttiType.GetMethods do
    begin
      if RttiMethod.Visibility = mvPublished then
        AppendMethodDoc(SB, RttiMethod);
    end;

    Result := SB.ToString;
  finally
    SB.Free;
    Context.Free;
  end;
end;

// Utilisation
begin
  WriteLn(TDocumentationGenerator.GenerateMarkdown(TUserService));
end;
```

## Attributs pour les tests

### Framework de tests avec attributs

```pascal
type
  // Attributs de test
  TestFixtureAttribute = class(TCustomAttribute)
  private
    FDescription: String;
  public
    constructor Create(const ADescription: String = '');
    property Description: String read FDescription;
  end;

  TestAttribute = class(TCustomAttribute)
  private
    FTestName: String;
  public
    constructor Create(const ATestName: String = '');
    property TestName: String read FTestName;
  end;

  SetupAttribute = class(TCustomAttribute)
  end;

  TearDownAttribute = class(TCustomAttribute)
  end;

  ExpectedExceptionAttribute = class(TCustomAttribute)
  private
    FExceptionClass: ExceptClass;
    FMessage: String;
  public
    constructor Create(AExceptionClass: ExceptClass;
                      const AMessage: String = '');
    property ExceptionClass: ExceptClass read FExceptionClass;
    property Message: String read FMessage;
  end;

  IgnoreAttribute = class(TCustomAttribute)
  private
    FReason: String;
  public
    constructor Create(const AReason: String = '');
    property Reason: String read FReason;
  end;

  TimeoutAttribute = class(TCustomAttribute)
  private
    FMilliseconds: Integer;
  public
    constructor Create(AMilliseconds: Integer);
    property Milliseconds: Integer read FMilliseconds;
  end;

  DataSourceAttribute = class(TCustomAttribute)
  private
    FSourceMethod: String;
  public
    constructor Create(const ASourceMethod: String);
    property SourceMethod: String read FSourceMethod;
  end;

// Implémentations
constructor TestFixtureAttribute.Create(const ADescription: String);
begin
  inherited Create;
  FDescription := ADescription;
end;

constructor TestAttribute.Create(const ATestName: String);
begin
  inherited Create;
  FTestName := ATestName;
end;

constructor ExpectedExceptionAttribute.Create(AExceptionClass: ExceptClass;
                                             const AMessage: String);
begin
  inherited Create;
  FExceptionClass := AExceptionClass;
  FMessage := AMessage;
end;

constructor IgnoreAttribute.Create(const AReason: String);
begin
  inherited Create;
  FReason := AReason;
end;

constructor TimeoutAttribute.Create(AMilliseconds: Integer);
begin
  inherited Create;
  FMilliseconds := AMilliseconds;
end;

constructor DataSourceAttribute.Create(const ASourceMethod: String);
begin
  inherited Create;
  FSourceMethod := ASourceMethod;
end;
```

### Classe de test avec attributs

```pascal
type
  [TestFixture('Tests unitaires pour la calculatrice')]
  TCalculatorTests = class
  private
    FCalculator: TObject; // Votre classe Calculator
  published
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    [Test('Test d''addition simple')]
    procedure TestAddition;

    [Test('Test de soustraction')]
    procedure TestSubtraction;

    [Test]
    [ExpectedException(EDivByZero, 'Division par zéro')]
    procedure TestDivisionByZero;

    [Test]
    [Timeout(1000)] // Maximum 1 seconde
    procedure TestPerformance;

    [Test]
    [Ignore('En cours de développement')]
    procedure TestNewFeature;

    [Test]
    [DataSource('GetTestData')]
    procedure TestWithMultipleInputs(Value1, Value2, Expected: Integer);

    function GetTestData: TArray<TArray<Integer>>;
  end;

procedure TCalculatorTests.Setup;
begin
  // Initialisation avant chaque test
  FCalculator := TObject.Create; // Remplacer par votre classe
  WriteLn('Setup exécuté');
end;

procedure TCalculatorTests.TearDown;
begin
  // Nettoyage après chaque test
  FCalculator.Free;
  WriteLn('TearDown exécuté');
end;

procedure TCalculatorTests.TestAddition;
begin
  // Vos assertions ici
  Assert(2 + 2 = 4, 'L''addition devrait fonctionner');
end;

procedure TCalculatorTests.TestSubtraction;
begin
  Assert(5 - 3 = 2, 'La soustraction devrait fonctionner');
end;

procedure TCalculatorTests.TestDivisionByZero;
begin
  // Ce test doit lever une exception EDivByZero
  raise EDivByZero.Create('Division par zéro');
end;

procedure TCalculatorTests.TestPerformance;
var
  I: Integer;
begin
  // Test qui doit s'exécuter en moins d'1 seconde
  for I := 1 to 1000000 do
    ; // Simulation de calcul
end;

procedure TCalculatorTests.TestNewFeature;
begin
  // Ce test sera ignoré
  Assert(False, 'Pas encore implémenté');
end;

procedure TCalculatorTests.TestWithMultipleInputs(Value1, Value2, Expected: Integer);
begin
  Assert(Value1 + Value2 = Expected,
         Format('%d + %d devrait égaler %d', [Value1, Value2, Expected]));
end;

function TCalculatorTests.GetTestData: TArray<TArray<Integer>>;
begin
  SetLength(Result, 3);
  Result[0] := [1, 2, 3];    // 1 + 2 = 3
  Result[1] := [5, 5, 10];   // 5 + 5 = 10
  Result[2] := [-1, 1, 0];   // -1 + 1 = 0
end;
```

### Moteur d'exécution de tests

```pascal
uses
  Rtti, SysUtils, DateUtils;

type
  TTestResult = record
    TestName: String;
    Success: Boolean;
    ErrorMessage: String;
    Duration: Integer; // en millisecondes
    Skipped: Boolean;
    SkipReason: String;
  end;

  TTestRunner = class
  private
    FResults: TArray<TTestResult>;
    FCurrentFixture: TObject;

    procedure RunSetup;
    procedure RunTearDown;
    function RunSingleTest(Method: TRttiMethod): TTestResult;
    function CheckExpectedException(Method: TRttiMethod;
                                   E: Exception): Boolean;
  public
    function RunTests(TestClass: TClass): TArray<TTestResult>;
    procedure PrintResults(const Results: TArray<TTestResult>);
  end;

procedure TTestRunner.RunSetup;
var
  Context: TRttiContext;
  RttiType: TRttiType;
  Method: TRttiMethod;
  Attr: TCustomAttribute;
begin
  Context := TRttiContext.Create;
  try
    RttiType := Context.GetType(FCurrentFixture.ClassType);

    for Method in RttiType.GetMethods do
    begin
      for Attr in Method.GetAttributes do
      begin
        if Attr is SetupAttribute then
        begin
          Method.Invoke(FCurrentFixture, []);
          Exit;
        end;
      end;
    end;
  finally
    Context.Free;
  end;
end;

procedure TTestRunner.RunTearDown;
var
  Context: TRttiContext;
  RttiType: TRttiType;
  Method: TRttiMethod;
  Attr: TCustomAttribute;
begin
  Context := TRttiContext.Create;
  try
    RttiType := Context.GetType(FCurrentFixture.ClassType);

    for Method in RttiType.GetMethods do
    begin
      for Attr in Method.GetAttributes do
      begin
        if Attr is TearDownAttribute then
        begin
          Method.Invoke(FCurrentFixture, []);
          Exit;
        end;
      end;
    end;
  finally
    Context.Free;
  end;
end;

function TTestRunner.CheckExpectedException(Method: TRttiMethod;
                                           E: Exception): Boolean;
var
  Attr: TCustomAttribute;
  ExpAttr: ExpectedExceptionAttribute;
begin
  Result := False;

  for Attr in Method.GetAttributes do
  begin
    if Attr is ExpectedExceptionAttribute then
    begin
      ExpAttr := ExpectedExceptionAttribute(Attr);
      Result := E.ClassType = ExpAttr.ExceptionClass;

      if Result and (ExpAttr.Message <> '') then
        Result := Pos(ExpAttr.Message, E.Message) > 0;

      Exit;
    end;
  end;
end;

function TTestRunner.RunSingleTest(Method: TRttiMethod): TTestResult;
var
  Attr: TCustomAttribute;
  StartTime: TDateTime;
  Timeout: Integer;
  TestName: String;
begin
  Result.TestName := Method.Name;
  Result.Success := False;
  Result.ErrorMessage := '';
  Result.Skipped := False;
  Result.SkipReason := '';

  // Obtenir le nom du test
  TestName := Method.Name;
  for Attr in Method.GetAttributes do
  begin
    if Attr is TestAttribute then
    begin
      if TestAttribute(Attr).TestName <> '' then
        TestName := TestAttribute(Attr).TestName;
    end;
  end;
  Result.TestName := TestName;

  // Vérifier si le test doit être ignoré
  for Attr in Method.GetAttributes do
  begin
    if Attr is IgnoreAttribute then
    begin
      Result.Skipped := True;
      Result.SkipReason := IgnoreAttribute(Attr).Reason;
      Exit;
    end;
  end;

  // Obtenir le timeout
  Timeout := 0;
  for Attr in Method.GetAttributes do
  begin
    if Attr is TimeoutAttribute then
    begin
      Timeout := TimeoutAttribute(Attr).Milliseconds;
      Break;
    end;
  end;

  // Exécuter le test
  StartTime := Now;
  try
    RunSetup;
    try
      Method.Invoke(FCurrentFixture, []);

      // Vérifier le timeout
      Result.Duration := MilliSecondsBetween(Now, StartTime);
      if (Timeout > 0) and (Result.Duration > Timeout) then
      begin
        Result.Success := False;
        Result.ErrorMessage := Format('Timeout: %dms > %dms',
                                     [Result.Duration, Timeout]);
      end
      else
      begin
        Result.Success := True;
      end;
    finally
      RunTearDown;
    end;
  except
    on E: Exception do
    begin
      Result.Duration := MilliSecondsBetween(Now, StartTime);

      // Vérifier si l'exception était attendue
      if CheckExpectedException(Method, E) then
      begin
        Result.Success := True;
      end
      else
      begin
        Result.Success := False;
        Result.ErrorMessage := E.ClassName + ': ' + E.Message;
      end;
    end;
  end;
end;

function TTestRunner.RunTests(TestClass: TClass): TArray<TTestResult>;
var
  Context: TRttiContext;
  RttiType: TRttiType;
  Method: TRttiMethod;
  Attr: TCustomAttribute;
  IsTest: Boolean;
  ResultIndex: Integer;
begin
  Context := TRttiContext.Create;
  try
    RttiType := Context.GetType(TestClass);

    // Créer une instance de la classe de test
    FCurrentFixture := TestClass.Create;
    try
      SetLength(FResults, 0);
      ResultIndex := 0;

      // Parcourir toutes les méthodes
      for Method in RttiType.GetMethods do
      begin
        IsTest := False;

        // Vérifier si c'est une méthode de test
        for Attr in Method.GetAttributes do
        begin
          if Attr is TestAttribute then
          begin
            IsTest := True;
            Break;
          end;
        end;

        if IsTest then
        begin
          SetLength(FResults, ResultIndex + 1);
          FResults[ResultIndex] := RunSingleTest(Method);
          Inc(ResultIndex);
        end;
      end;

      Result := FResults;
    finally
      FCurrentFixture.Free;
    end;
  finally
    Context.Free;
  end;
end;

procedure TTestRunner.PrintResults(const Results: TArray<TTestResult>);
var
  TestResult: TTestResult;
  TotalTests, PassedTests, FailedTests, SkippedTests: Integer;
  TotalDuration: Integer;
begin
  TotalTests := Length(Results);
  PassedTests := 0;
  FailedTests := 0;
  SkippedTests := 0;
  TotalDuration := 0;

  WriteLn('=== Résultats des tests ===');
  WriteLn;

  for TestResult in Results do
  begin
    if TestResult.Skipped then
    begin
      WriteLn('⊘ ', TestResult.TestName, ' - IGNORÉ');
      if TestResult.SkipReason <> '' then
        WriteLn('  Raison: ', TestResult.SkipReason);
      Inc(SkippedTests);
    end
    else if TestResult.Success then
    begin
      WriteLn('✓ ', TestResult.TestName, ' - SUCCÈS (',
              TestResult.Duration, 'ms)');
      Inc(PassedTests);
      TotalDuration := TotalDuration + TestResult.Duration;
    end
    else
    begin
      WriteLn('✗ ', TestResult.TestName, ' - ÉCHEC (',
              TestResult.Duration, 'ms)');
      if TestResult.ErrorMessage <> '' then
        WriteLn('  Erreur: ', TestResult.ErrorMessage);
      Inc(FailedTests);
      TotalDuration := TotalDuration + TestResult.Duration;
    end;
  end;

  WriteLn;
  WriteLn('=== Résumé ===');
  WriteLn('Total: ', TotalTests, ' tests');
  WriteLn('Réussis: ', PassedTests);
  WriteLn('Échoués: ', FailedTests);
  WriteLn('Ignorés: ', SkippedTests);
  WriteLn('Durée totale: ', TotalDuration, 'ms');

  if FailedTests = 0 then
    WriteLn('✓ Tous les tests sont passés!')
  else
    WriteLn('✗ ', FailedTests, ' test(s) ont échoué.');
end;

// Utilisation
var
  Runner: TTestRunner;
  Results: TArray<TTestResult>;
begin
  Runner := TTestRunner.Create;
  try
    Results := Runner.RunTests(TCalculatorTests);
    Runner.PrintResults(Results);
  finally
    Runner.Free;
  end;
end;
```

## Attributs de traçage et monitoring

### Système de traçage automatique

```pascal
type
  // Attributs de traçage
  TraceAttribute = class(TCustomAttribute)
  private
    FLevel: Integer;
  public
    constructor Create(ALevel: Integer = 1);
    property Level: Integer read FLevel;
  end;

  LogParametersAttribute = class(TCustomAttribute)
  end;

  LogResultAttribute = class(TCustomAttribute)
  end;

  LogExecutionTimeAttribute = class(TCustomAttribute)
  end;

  MetricAttribute = class(TCustomAttribute)
  private
    FMetricName: String;
  public
    constructor Create(const AMetricName: String);
    property MetricName: String read FMetricName;
  end;

constructor TraceAttribute.Create(ALevel: Integer);
begin
  inherited Create;
  FLevel := ALevel;
end;

constructor MetricAttribute.Create(const AMetricName: String);
begin
  inherited Create;
  FMetricName := AMetricName;
end;

// Classe avec traçage
type
  TBusinessService = class
  published
    [Trace(1)]
    [LogParameters]
    [LogResult]
    [LogExecutionTime]
    [Metric('user.login')]
    function Login(const Username, Password: String): Boolean;

    [Trace(2)]
    [LogExecutionTime]
    [Metric('data.process')]
    procedure ProcessData(const Data: TArray<Integer>);

    [Trace(3)]
    [LogParameters]
    [Metric('report.generate')]
    function GenerateReport(ReportType: Integer): String;
  end;

// Intercepteur de traçage
type
  TTraceInterceptor = class
  private
    class var FTraceLevel: Integer;
    class var FMetrics: TDictionary<String, Integer>;

    class constructor Create;
    class destructor Destroy;

    class procedure LogMethodCall(const MethodName: String;
                                  const Args: array of TValue);
    class procedure LogMethodResult(const MethodName: String;
                                   const Result: TValue);
    class procedure LogExecutionTime(const MethodName: String;
                                    Duration: Integer);
    class procedure UpdateMetric(const MetricName: String);
  public
    class property TraceLevel: Integer read FTraceLevel write FTraceLevel;

    class function InvokeWithTrace(AObject: TObject;
                                   const MethodName: String;
                                   const Args: array of TValue): TValue;
    class procedure PrintMetrics;
  end;

class constructor TTraceInterceptor.Create;
begin
  FTraceLevel := 1;
  FMetrics := TDictionary<String, Integer>.Create;
end;

class destructor TTraceInterceptor.Destroy;
begin
  FMetrics.Free;
end;

class procedure TTraceInterceptor.LogMethodCall(const MethodName: String;
                                               const Args: array of TValue);
var
  I: Integer;
  S: String;
begin
  S := Format('[TRACE] Calling %s(', [MethodName]);
  for I := Low(Args) to High(Args) do
  begin
    if I > Low(Args) then
      S := S + ', ';
    S := S + Args[I].ToString;
  end;
  S := S + ')';
  WriteLn(S);
end;

class procedure TTraceInterceptor.LogMethodResult(const MethodName: String;
                                                 const Result: TValue);
begin
  WriteLn(Format('[TRACE] %s returned: %s',
                 [MethodName, Result.ToString]));
end;

class procedure TTraceInterceptor.LogExecutionTime(const MethodName: String;
                                                  Duration: Integer);
begin
  WriteLn(Format('[TRACE] %s executed in %dms',
                 [MethodName, Duration]));
end;

class procedure TTraceInterceptor.UpdateMetric(const MetricName: String);
var
  Count: Integer;
begin
  if FMetrics.TryGetValue(MetricName, Count) then
    FMetrics[MetricName] := Count + 1
  else
    FMetrics.Add(MetricName, 1);
end;

class function TTraceInterceptor.InvokeWithTrace(AObject: TObject;
                                                const MethodName: String;
                                                const Args: array of TValue): TValue;
var
  Context: TRttiContext;
  RttiType: TRttiType;
  RttiMethod: TRttiMethod;
  Attr: TCustomAttribute;
  StartTime: TDateTime;
  Duration: Integer;
  ShouldTrace: Boolean;
  TraceLevel: Integer;
  LogParams, LogResult, LogTime: Boolean;
  MetricName: String;
begin
  Context := TRttiContext.Create;
  try
    RttiType := Context.GetType(AObject.ClassType);
    RttiMethod := RttiType.GetMethod(MethodName);

    if not Assigned(RttiMethod) then
      raise Exception.CreateFmt('Méthode "%s" non trouvée', [MethodName]);

    // Analyser les attributs
    ShouldTrace := False;
    TraceLevel := 0;
    LogParams := False;
    LogResult := False;
    LogTime := False;
    MetricName := '';

    for Attr in RttiMethod.GetAttributes do
    begin
      if Attr is TraceAttribute then
      begin
        ShouldTrace := True;
        TraceLevel := TraceAttribute(Attr).Level;
      end
      else if Attr is LogParametersAttribute then
        LogParams := True
      else if Attr is LogResultAttribute then
        LogResult := True
      else if Attr is LogExecutionTimeAttribute then
        LogTime := True
      else if Attr is MetricAttribute then
        MetricName := MetricAttribute(Attr).MetricName;
    end;

    // Tracer si nécessaire
    if ShouldTrace and (TraceLevel <= FTraceLevel) then
    begin
      if LogParams then
        LogMethodCall(MethodName, Args);

      StartTime := Now;
      Result := RttiMethod.Invoke(AObject, Args);
      Duration := MilliSecondsBetween(Now, StartTime);

      if LogResult then
        LogMethodResult(MethodName, Result);

      if LogTime then
        LogExecutionTime(MethodName, Duration);
    end
    else
    begin
      // Exécution sans traçage
      Result := RttiMethod.Invoke(AObject, Args);
    end;

    // Mettre à jour les métriques
    if MetricName <> '' then
      UpdateMetric(MetricName);

  finally
    Context.Free;
  end;
end;

class procedure TTraceInterceptor.PrintMetrics;
var
  Pair: TPair<String, Integer>;
begin
  WriteLn;
  WriteLn('=== Métriques ===');
  for Pair in FMetrics do
    WriteLn(Format('%s: %d appels', [Pair.Key, Pair.Value]));
end;
```

## Combinaison d'attributs avancée

### Système complet avec plusieurs aspects

```pascal
type
  // Classe utilisant plusieurs types d'attributs
  [Table('orders')]
  [Documentation('Représente une commande dans le système')]
  [Version('1.2.0', EncodeDate(2024, 1, 20))]
  TOrder = class
  private
    FId: Integer;
    FCustomerId: Integer;
    FOrderDate: TDateTime;
    FTotalAmount: Double;
    FStatus: String;
  published
    [PrimaryKey(True)]
    [Column('order_id', 'INTEGER')]
    [JsonProperty('id')]
    [Documentation('Identifiant unique de la commande')]
    property Id: Integer read FId write FId;

    [Column('customer_id', 'INTEGER')]
    [ForeignKey('customers', 'customer_id')]
    [JsonProperty('customer_id')]
    [Required]
    [Documentation('Référence vers le client')]
    property CustomerId: Integer read FCustomerId write FCustomerId;

    [Column('order_date', 'TIMESTAMP')]
    [JsonProperty('order_date')]
    [DateFormat('yyyy-mm-dd hh:nn:ss')]
    [Required]
    [Documentation('Date de la commande')]
    property OrderDate: TDateTime read FOrderDate write FOrderDate;

    [Column('total_amount', 'DECIMAL(10,2)')]
    [JsonProperty('total')]
    [Range(0, 999999.99)]
    [Documentation('Montant total de la commande')]
    property TotalAmount: Double read FTotalAmount write FTotalAmount;

    [Column('status', 'VARCHAR', 50)]
    [JsonProperty('status')]
    [MaxLength(50)]
    [Documentation('Statut de la commande')]
    property Status: String read FStatus write FStatus;
  end;

  // Service utilisant plusieurs aspects
  [Documentation('Service de gestion des commandes')]
  [TestFixture('Tests du service de commandes')]
  TOrderService = class
  published
    [RequireAuthentication]
    [RequireRole('user')]
    [Cacheable(300)]
    [CacheKey('order:{0}')]
    [Trace(1)]
    [LogParameters]
    [LogResult]
    [Metric('order.get')]
    [Documentation('Récupère une commande par son ID')]
    [Test('Test de récupération de commande')]
    function GetOrder(OrderId: Integer): TOrder;

    [RequireAuthentication]
    [RequireRole('user')]
    [InvalidateCache('order:*')]
    [Trace(1)]
    [LogParameters]
    [Metric('order.create')]
    [Documentation('Crée une nouvelle commande')]
    [Test('Test de création de commande')]
    function CreateOrder(CustomerId: Integer; Amount: Double): Integer;

    [RequireAuthentication]
    [RequireRole('admin')]
    [InvalidateCache('order:{0}')]
    [Trace(2)]
    [Metric('order.delete')]
    [Documentation('Supprime une commande')]
    [ExpectedException(EAccessViolation)]
    [Test('Test de suppression avec permissions')]
    procedure DeleteOrder(OrderId: Integer);
  end;
```

## Bonnes pratiques et recommandations

### Organisation des attributs

```pascal
unit MyAttributes;

interface

uses
  Rtti, SysUtils;

type
  // Regrouper les attributs par catégorie

  // === Attributs de validation ===
  TValidationAttribute = class(TCustomAttribute)
  public
    function Validate(const Value: TValue): Boolean; virtual; abstract;
    function GetErrorMessage: String; virtual; abstract;
  end;

  TRequiredAttribute = class(TValidationAttribute)
    // ...
  end;

  // === Attributs de persistance ===
  TPersistenceAttribute = class(TCustomAttribute)
  end;

  TTableAttribute = class(TPersistenceAttribute)
    // ...
  end;

  // === Attributs de sécurité ===
  TSecurityAttribute = class(TCustomAttribute)
  end;

  TRequireRoleAttribute = class(TSecurityAttribute)
    // ...
  end;

implementation
  // ...
end.
```

### Performance et optimisation

```pascal
type
  // Cache pour les attributs
  TAttributeCache = class
  private
    type
      TAttributeInfo = record
        Attributes: TArray<TCustomAttribute>;
        LastAccess: TDateTime;
      end;

    class var FCache: TDictionary<String, TAttributeInfo>;
    class var FCacheSize: Integer;
    class var FMaxCacheSize: Integer;

    class constructor Create;
    class destructor Destroy;
    class procedure CleanupCache;
  public
    class function GetAttributes(AClass: TClass;
                                 const PropName: String): TArray<TCustomAttribute>;
    class procedure SetMaxCacheSize(Size: Integer);
  end;

class constructor TAttributeCache.Create;
begin
  FCache := TDictionary<String, TAttributeInfo>.Create;
  FMaxCacheSize := 100;
  FCacheSize := 0;
end;

class destructor TAttributeCache.Destroy;
begin
  FCache.Free;
end;

class procedure TAttributeCache.CleanupCache;
var
  Key: String;
  OldestKey: String;
  OldestTime: TDateTime;
  Info: TAttributeInfo;
begin
  if FCacheSize <= FMaxCacheSize then
    Exit;

  // Trouver l'entrée la plus ancienne
  OldestTime := Now;
  OldestKey := '';

  for Key in FCache.Keys do
  begin
    if FCache.TryGetValue(Key, Info) then
    begin
      if Info.LastAccess < OldestTime then
      begin
        OldestTime := Info.LastAccess;
        OldestKey := Key;
      end;
    end;
  end;

  // Supprimer l'entrée la plus ancienne
  if OldestKey <> '' then
  begin
    FCache.Remove(OldestKey);
    Dec(FCacheSize);
  end;
end;

class function TAttributeCache.GetAttributes(AClass: TClass;
                                            const PropName: String): TArray<TCustomAttribute>;
var
  Key: String;
  Info: TAttributeInfo;
  Context: TRttiContext;
  RttiType: TRttiType;
  RttiProp: TRttiProperty;
  Attr: TCustomAttribute;
begin
  Key := AClass.ClassName + '.' + PropName;

  // Vérifier le cache
  if FCache.TryGetValue(Key, Info) then
  begin
    Info.LastAccess := Now;
    FCache[Key] := Info;
    Result := Info.Attributes;
    Exit;
  end;

  // Charger les attributs
  Context := TRttiContext.Create;
  try
    RttiType := Context.GetType(AClass);
    RttiProp := RttiType.GetProperty(PropName);

    if Assigned(RttiProp) then
    begin
      SetLength(Info.Attributes, 0);
      for Attr in RttiProp.GetAttributes do
      begin
        SetLength(Info.Attributes, Length(Info.Attributes) + 1);
        Info.Attributes[High(Info.Attributes)] := Attr;
      end;

      Info.LastAccess := Now;
      FCache.Add(Key, Info);
      Inc(FCacheSize);

      CleanupCache;

      Result := Info.Attributes;
    end;
  finally
    Context.Free;
  end;
end;

class procedure TAttributeCache.SetMaxCacheSize(Size: Integer);
begin
  FMaxCacheSize := Size;
  CleanupCache;
end;
```

## Conclusion

La programmation méta avec les attributs en FreePascal/Lazarus offre des possibilités extraordinaires pour créer du code plus expressif, maintenable et flexible. Les attributs permettent de :

### Avantages principaux

1. **Séparation des préoccupations** : Les aspects transversaux (sécurité, cache, logging) sont séparés de la logique métier
2. **Code déclaratif** : Les intentions sont clairement exprimées par des attributs plutôt que par du code impératif
3. **Réutilisabilité** : Les attributs peuvent être appliqués à plusieurs classes et méthodes
4. **Maintenabilité** : Les modifications de comportement se font en changeant les attributs, pas le code
5. **Auto-documentation** : Les attributs servent de documentation vivante du code

### Cas d'usage recommandés

Les attributs sont particulièrement adaptés pour :

- **Validation de données** : Règles de validation déclaratives
- **Mapping ORM** : Configuration du mapping objet-relationnel
- **Sérialisation** : Contrôle de la sérialisation/désérialisation
- **Configuration** : Chargement automatique de configuration
- **Sécurité** : Déclaration des permissions et rôles requis
- **Cache** : Gestion automatique du cache
- **Tests** : Framework de tests avec découverte automatique
- **Documentation** : Génération automatique de documentation
- **Monitoring** : Traçage et métriques automatiques

### Limites et considérations

#### Performance
- Le RTTI a un coût en performance
- Utilisez des caches pour les métadonnées fréquemment accédées
- Évitez l'utilisation excessive d'attributs dans les chemins critiques

#### Complexité
- Ne pas sur-utiliser les attributs pour des cas simples
- Maintenir une documentation claire des attributs personnalisés
- Former l'équipe sur l'utilisation correcte des attributs

#### Débogage
- Le code utilisant intensivement les attributs peut être plus difficile à déboguer
- Prévoir des logs détaillés pour tracer l'exécution
- Utiliser des tests unitaires pour valider le comportement

### Patterns d'implémentation

#### Pattern Intercepteur
```pascal
type
  TInterceptor = class
  public
    class function Intercept(AObject: TObject;
                            const MethodName: String;
                            const Args: array of TValue): TValue;
  end;
```

#### Pattern Décorateur avec attributs
```pascal
type
  TAttributeDecorator = class
  private
    FTarget: TObject;
  public
    constructor Create(ATarget: TObject);
    function Execute(const MethodName: String): TValue;
  end;
```

#### Pattern Pipeline
```pascal
type
  TAttributePipeline = class
  private
    FHandlers: TList<TAttributeHandler>;
  public
    procedure AddHandler(Handler: TAttributeHandler);
    function Process(Context: TContext): TResult;
  end;
```

### Guide de migration

Pour introduire les attributs dans un projet existant :

1. **Commencer petit** : Introduire les attributs pour un aspect simple (ex: validation)
2. **Tester intensivement** : Créer des tests pour valider le comportement
3. **Former l'équipe** : S'assurer que tous comprennent les attributs
4. **Documenter** : Créer un guide des attributs utilisés dans le projet
5. **Mesurer** : Évaluer l'impact sur les performances et la maintenabilité

### Évolutions futures

Les attributs en FreePascal continuent d'évoluer. Les futures versions pourraient apporter :

- Support amélioré des attributs génériques
- Attributs sur les paramètres de méthodes
- Attributs au niveau des unités
- Meilleure intégration avec l'IDE Lazarus
- Optimisations du compilateur pour le RTTI

### Ressources supplémentaires

Pour approfondir vos connaissances :

1. **Documentation officielle FreePascal** sur le RTTI et les attributs
2. **Forums Lazarus** pour les discussions et exemples
3. **Projets open source** utilisant les attributs
4. **Blogs et tutoriels** de la communauté
5. **Code source de la RTL** pour comprendre l'implémentation

### Exemple complet d'intégration

Voici un exemple montrant comment combiner tous les concepts :

```pascal
program CompleteAttributeExample;

uses
  SysUtils, Rtti, TypInfo, Classes;

type
  // Framework d'application avec tous les aspects
  TApplicationFramework = class
  private
    FValidator: TValidator;
    FSerializer: TJsonSerializer;
    FCache: TCacheManager;
    FSecurity: TSecurityInterceptor;
    FLogger: TTraceInterceptor;
    FConfig: TConfigLoader;
  public
    constructor Create;
    destructor Destroy; override;

    // Traiter une requête avec tous les aspects
    function ProcessRequest(Controller: TObject;
                          const MethodName: String;
                          const Args: array of TValue;
                          SecurityContext: TSecurityContext): TValue;
  end;

constructor TApplicationFramework.Create;
begin
  FValidator := TValidator.Create;
  FSerializer := TJsonSerializer.Create;
  FCache := TCacheManager.Create;
  FSecurity := TSecurityInterceptor.Create;
  FLogger := TTraceInterceptor.Create;
  FConfig := TConfigLoader.Create;
end;

destructor TApplicationFramework.Destroy;
begin
  FConfig.Free;
  FLogger.Free;
  FSecurity.Free;
  FCache.Free;
  FSerializer.Free;
  FValidator.Free;
  inherited;
end;

function TApplicationFramework.ProcessRequest(Controller: TObject;
                                             const MethodName: String;
                                             const Args: array of TValue;
                                             SecurityContext: TSecurityContext): TValue;
var
  Context: TRttiContext;
  RttiType: TRttiType;
  RttiMethod: TRttiMethod;
  Errors: TStringList;
begin
  Context := TRttiContext.Create;
  try
    RttiType := Context.GetType(Controller.ClassType);
    RttiMethod := RttiType.GetMethod(MethodName);

    if not Assigned(RttiMethod) then
      raise Exception.CreateFmt('Méthode %s non trouvée', [MethodName]);

    // 1. Vérifier la sécurité
    FSecurity.CheckMethodAccess(RttiMethod, SecurityContext);

    // 2. Valider les paramètres
    // (implémenter la validation des paramètres)

    // 3. Vérifier le cache
    Result := FCache.GetFromCache(Controller, MethodName, Args);
    if not Result.IsEmpty then
      Exit;

    // 4. Logger l'appel
    FLogger.LogMethodCall(MethodName, Args);

    // 5. Exécuter la méthode
    Result := RttiMethod.Invoke(Controller, Args);

    // 6. Mettre en cache le résultat
    FCache.PutInCache(Controller, MethodName, Args, Result);

    // 7. Logger le résultat
    FLogger.LogMethodResult(MethodName, Result);

    // 8. Sérialiser si nécessaire
    // (selon les attributs de la méthode)

  finally
    Context.Free;
  end;
end;

// Utilisation du framework
var
  Framework: TApplicationFramework;
  Controller: TUserController;
  SecurityCtx: TSecurityContext;
  Result: TValue;
begin
  Framework := TApplicationFramework.Create;
  Controller := TUserController.Create;
  SecurityCtx := TSecurityContext.Create;
  try
    // Configuration de la sécurité
    SecurityCtx.CurrentUser := 'admin';
    SecurityCtx.AddRole('admin');

    // Appel avec tous les aspects gérés automatiquement
    Result := Framework.ProcessRequest(
      Controller,
      'GetUser',
      [123],
      SecurityCtx
    );

    WriteLn('Résultat: ', Result.ToString);
  finally
    SecurityCtx.Free;
    Controller.Free;
    Framework.Free;
  end;
end.
```

### Points clés à retenir

1. **Les attributs sont des métadonnées** : Ils enrichissent le code sans le modifier
2. **Le RTTI est la clé** : C'est lui qui permet de lire et exploiter les attributs
3. **Conception modulaire** : Créez des attributs réutilisables et composables
4. **Performance** : Mesurez et optimisez avec des caches
5. **Documentation** : Les attributs bien conçus sont auto-documentés
6. **Tests** : Testez intensivement le comportement des attributs
7. **Évolution** : Les attributs facilitent l'évolution du code

### Checklist de mise en œuvre

Avant d'implémenter des attributs dans votre projet :

- [ ] Identifier les aspects transversaux à extraire
- [ ] Concevoir la hiérarchie d'attributs
- [ ] Implémenter les attributs de base
- [ ] Créer les intercepteurs/processeurs
- [ ] Ajouter la gestion du cache RTTI
- [ ] Écrire les tests unitaires
- [ ] Documenter l'utilisation
- [ ] Former l'équipe
- [ ] Mesurer les performances
- [ ] Prévoir la stratégie de débogage

La programmation méta avec attributs transforme radicalement la façon d'écrire du code en FreePascal/Lazarus, permettant de créer des applications plus modulaires, maintenables et expressives. En maîtrisant ces concepts, vous disposez d'un outil puissant pour gérer la complexité et améliorer la qualité de vos projets.

⏭️ [Gestion mémoire et comptage de références](/03-langage-object-pascal-avance/05-gestion-memoire-comptage-references.md)
