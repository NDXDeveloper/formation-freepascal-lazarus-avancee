🔝 Retour au [Sommaire](/SOMMAIRE.md)

# Configuration via Fichiers Texte avec FreePascal/Lazarus sous Linux

## Introduction : La Philosophie Unix/Linux

Sous Linux/Ubuntu, contrairement à Windows qui utilise principalement le registre, la configuration des applications se fait via des fichiers texte. Cette approche présente plusieurs avantages :

- **Simplicité** : Les fichiers texte peuvent être lus et modifiés avec n'importe quel éditeur
- **Transparence** : On peut voir exactement ce qui est configuré
- **Versioning** : Facile à sauvegarder et versionner avec Git
- **Portabilité** : Copier une configuration = copier un fichier
- **Scriptable** : Modification automatique via scripts

### Où Trouvent-on ces Fichiers ?

Linux organise les fichiers de configuration selon une hiérarchie standard :

```
/etc/                    # Configuration système globale
├── apache2/            # Configuration d'Apache
├── mysql/              # Configuration de MySQL
└── myapp.conf          # Configuration d'une application

/home/user/              # Répertoire utilisateur
├── .config/            # Configuration utilisateur moderne
│   └── myapp/         # Dossier de votre application
├── .myapp/            # Configuration utilisateur classique
└── .myapprc           # Fichier de configuration unique
```

## Formats de Fichiers de Configuration Courants

### 1. Format INI (Le Plus Simple)

Le format INI est familier pour ceux venant de Windows :

```ini
# Fichier de configuration MyApp
# Commentaire avec #
; Ou commentaire avec point-virgule

[General]
AppName=Mon Application
Version=1.0.0
Language=fr

[Database]
Host=localhost
Port=5432
Database=myapp_db
Username=myuser
# Ne jamais stocker les mots de passe en clair !
PasswordFile=/home/user/.config/myapp/db.key

[Display]
Theme=dark
FontSize=12
ShowToolbar=true
WindowState=maximized
```

### 2. Format Clé=Valeur Simple

Format minimaliste utilisé par beaucoup d'applications Unix :

```bash
# Configuration simple
APP_NAME="Mon Application"
DEBUG_MODE=false
LOG_LEVEL=info
MAX_CONNECTIONS=100
TEMP_DIR=/tmp/myapp
```

### 3. Format JSON

De plus en plus populaire pour les applications modernes :

```json
{
  "application": {
    "name": "Mon Application",
    "version": "1.0.0",
    "debug": false
  },
  "database": {
    "host": "localhost",
    "port": 5432,
    "database": "myapp_db"
  },
  "display": {
    "theme": "dark",
    "fontSize": 12,
    "showToolbar": true
  }
}
```

### 4. Format XML

Utilisé par certaines applications complexes :

```xml
<?xml version="1.0" encoding="UTF-8"?>
<configuration>
  <application>
    <name>Mon Application</name>
    <version>1.0.0</version>
  </application>
  <database>
    <host>localhost</host>
    <port>5432</port>
  </database>
</configuration>
```

### 5. Format YAML

Très populaire pour sa lisibilité :

```yaml
application:
  name: Mon Application
  version: 1.0.0
  debug: false

database:
  host: localhost
  port: 5432
  database: myapp_db

display:
  theme: dark
  font_size: 12
  show_toolbar: true
```

## Implémentation en FreePascal

### Lecture/Écriture de Fichiers INI

FreePascal fournit la classe `TIniFile` pour gérer facilement les fichiers INI :

```pascal
unit ConfigManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles;

type
  TAppConfig = class
  private
    FIniFile: TIniFile;
    FConfigPath: string;

    // Propriétés de configuration
    FAppName: string;
    FVersion: string;
    FLanguage: string;
    FDatabaseHost: string;
    FDatabasePort: Integer;
    FTheme: string;
    FFontSize: Integer;
    FShowToolbar: Boolean;

  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadConfig;
    procedure SaveConfig;
    procedure SetDefaults;

    // Propriétés publiques
    property AppName: string read FAppName write FAppName;
    property Version: string read FVersion write FVersion;
    property Language: string read FLanguage write FLanguage;
    property DatabaseHost: string read FDatabaseHost write FDatabaseHost;
    property DatabasePort: Integer read FDatabasePort write FDatabasePort;
    property Theme: string read FTheme write FTheme;
    property FontSize: Integer read FFontSize write FFontSize;
    property ShowToolbar: Boolean read FShowToolbar write FShowToolbar;
  end;

implementation

constructor TAppConfig.Create;
begin
  inherited Create;

  // Déterminer le chemin de configuration selon les standards Linux
  // Utilise la variable d'environnement XDG_CONFIG_HOME si définie
  FConfigPath := GetEnvironmentVariable('XDG_CONFIG_HOME');
  if FConfigPath = '' then
    FConfigPath := GetEnvironmentVariable('HOME') + '/.config';

  // Créer le dossier de l'application s'il n'existe pas
  FConfigPath := FConfigPath + '/myapp';
  if not DirectoryExists(FConfigPath) then
    ForceDirectories(FConfigPath);

  // Chemin complet du fichier de configuration
  FConfigPath := FConfigPath + '/config.ini';

  // Charger la configuration ou créer les valeurs par défaut
  if FileExists(FConfigPath) then
    LoadConfig
  else
  begin
    SetDefaults;
    SaveConfig; // Créer le fichier avec les valeurs par défaut
  end;
end;

destructor TAppConfig.Destroy;
begin
  SaveConfig; // Sauvegarder avant de fermer
  inherited Destroy;
end;

procedure TAppConfig.SetDefaults;
begin
  FAppName := 'Mon Application';
  FVersion := '1.0.0';
  FLanguage := 'fr';
  FDatabaseHost := 'localhost';
  FDatabasePort := 5432;
  FTheme := 'default';
  FFontSize := 12;
  FShowToolbar := True;
end;

procedure TAppConfig.LoadConfig;
begin
  FIniFile := TIniFile.Create(FConfigPath);
  try
    // Section [General]
    FAppName := FIniFile.ReadString('General', 'AppName', 'Mon Application');
    FVersion := FIniFile.ReadString('General', 'Version', '1.0.0');
    FLanguage := FIniFile.ReadString('General', 'Language', 'fr');

    // Section [Database]
    FDatabaseHost := FIniFile.ReadString('Database', 'Host', 'localhost');
    FDatabasePort := FIniFile.ReadInteger('Database', 'Port', 5432);

    // Section [Display]
    FTheme := FIniFile.ReadString('Display', 'Theme', 'default');
    FFontSize := FIniFile.ReadInteger('Display', 'FontSize', 12);
    FShowToolbar := FIniFile.ReadBool('Display', 'ShowToolbar', True);
  finally
    FIniFile.Free;
  end;
end;

procedure TAppConfig.SaveConfig;
begin
  FIniFile := TIniFile.Create(FConfigPath);
  try
    // Section [General]
    FIniFile.WriteString('General', 'AppName', FAppName);
    FIniFile.WriteString('General', 'Version', FVersion);
    FIniFile.WriteString('General', 'Language', FLanguage);

    // Section [Database]
    FIniFile.WriteString('Database', 'Host', FDatabaseHost);
    FIniFile.WriteInteger('Database', 'Port', FDatabasePort);

    // Section [Display]
    FIniFile.WriteString('Display', 'Theme', FTheme);
    FIniFile.WriteInteger('Display', 'FontSize', FFontSize);
    FIniFile.WriteBool('Display', 'ShowToolbar', FShowToolbar);

    // Forcer l'écriture sur disque
    FIniFile.UpdateFile;
  finally
    FIniFile.Free;
  end;
end;

end.
```

### Utilisation de la Classe de Configuration

```pascal
program ConfigExample;

uses
  SysUtils, ConfigManager;

var
  Config: TAppConfig;
begin
  Config := TAppConfig.Create;
  try
    WriteLn('Configuration chargée :');
    WriteLn('Application : ', Config.AppName);
    WriteLn('Version : ', Config.Version);
    WriteLn('Langue : ', Config.Language);
    WriteLn('Thème : ', Config.Theme);

    // Modifier une valeur
    Config.Theme := 'dark';
    Config.FontSize := 14;

    // La sauvegarde est automatique dans le destructeur
  finally
    Config.Free;
  end;
end.
```

## Gestion des Fichiers Clé=Valeur

Pour les fichiers de configuration simples au format clé=valeur :

```pascal
unit SimpleConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl;

type
  TConfigDictionary = specialize TFPGMap<string, string>;

  TSimpleConfig = class
  private
    FConfig: TConfigDictionary;
    FFilePath: string;

    function StripComments(const ALine: string): string;
    function ParseLine(const ALine: string; out AKey, AValue: string): Boolean;
  public
    constructor Create(const AFilePath: string);
    destructor Destroy; override;

    procedure Load;
    procedure Save;

    function GetValue(const AKey: string; const ADefault: string = ''): string;
    procedure SetValue(const AKey, AValue: string);
    function GetBool(const AKey: string; ADefault: Boolean = False): Boolean;
    procedure SetBool(const AKey: string; AValue: Boolean);
    function GetInt(const AKey: string; ADefault: Integer = 0): Integer;
    procedure SetInt(const AKey: string; AValue: Integer);
  end;

implementation

constructor TSimpleConfig.Create(const AFilePath: string);
begin
  inherited Create;
  FConfig := TConfigDictionary.Create;
  FFilePath := AFilePath;

  if FileExists(FFilePath) then
    Load;
end;

destructor TSimpleConfig.Destroy;
begin
  Save;
  FConfig.Free;
  inherited Destroy;
end;

function TSimpleConfig.StripComments(const ALine: string): string;
var
  CommentPos: Integer;
begin
  Result := ALine;

  // Supprimer les commentaires (# ou ;)
  CommentPos := Pos('#', Result);
  if CommentPos > 0 then
    Result := Copy(Result, 1, CommentPos - 1);

  CommentPos := Pos(';', Result);
  if CommentPos > 0 then
    Result := Copy(Result, 1, CommentPos - 1);

  Result := Trim(Result);
end;

function TSimpleConfig.ParseLine(const ALine: string; out AKey, AValue: string): Boolean;
var
  EqualPos: Integer;
  CleanLine: string;
begin
  Result := False;
  CleanLine := StripComments(ALine);

  if CleanLine = '' then
    Exit;

  EqualPos := Pos('=', CleanLine);
  if EqualPos > 0 then
  begin
    AKey := Trim(Copy(CleanLine, 1, EqualPos - 1));
    AValue := Trim(Copy(CleanLine, EqualPos + 1, Length(CleanLine)));

    // Supprimer les guillemets éventuels
    if (Length(AValue) >= 2) and
       (AValue[1] = '"') and
       (AValue[Length(AValue)] = '"') then
    begin
      AValue := Copy(AValue, 2, Length(AValue) - 2);
    end;

    Result := AKey <> '';
  end;
end;

procedure TSimpleConfig.Load;
var
  FileLines: TStringList;
  i: Integer;
  Key, Value: string;
begin
  if not FileExists(FFilePath) then
    Exit;

  FileLines := TStringList.Create;
  try
    FileLines.LoadFromFile(FFilePath);

    FConfig.Clear;
    for i := 0 to FileLines.Count - 1 do
    begin
      if ParseLine(FileLines[i], Key, Value) then
        FConfig[Key] := Value;
    end;
  finally
    FileLines.Free;
  end;
end;

procedure TSimpleConfig.Save;
var
  FileLines: TStringList;
  i: Integer;
begin
  FileLines := TStringList.Create;
  try
    // Ajouter un en-tête
    FileLines.Add('# Configuration file generated by MyApp');
    FileLines.Add('# ' + DateTimeToStr(Now));
    FileLines.Add('');

    // Écrire toutes les paires clé=valeur
    for i := 0 to FConfig.Count - 1 do
    begin
      FileLines.Add(FConfig.Keys[i] + '=' + FConfig.Data[i]);
    end;

    // Créer le répertoire si nécessaire
    ForceDirectories(ExtractFilePath(FFilePath));

    // Sauvegarder
    FileLines.SaveToFile(FFilePath);
  finally
    FileLines.Free;
  end;
end;

function TSimpleConfig.GetValue(const AKey: string; const ADefault: string): string;
var
  Index: Integer;
begin
  Index := FConfig.IndexOf(AKey);
  if Index >= 0 then
    Result := FConfig.Data[Index]
  else
    Result := ADefault;
end;

procedure TSimpleConfig.SetValue(const AKey, AValue: string);
begin
  FConfig[AKey] := AValue;
end;

function TSimpleConfig.GetBool(const AKey: string; ADefault: Boolean): Boolean;
var
  Value: string;
begin
  Value := LowerCase(GetValue(AKey, ''));
  if Value = '' then
    Result := ADefault
  else
    Result := (Value = 'true') or (Value = '1') or
              (Value = 'yes') or (Value = 'on');
end;

procedure TSimpleConfig.SetBool(const AKey: string; AValue: Boolean);
begin
  if AValue then
    SetValue(AKey, 'true')
  else
    SetValue(AKey, 'false');
end;

function TSimpleConfig.GetInt(const AKey: string; ADefault: Integer): Integer;
var
  Value: string;
begin
  Value := GetValue(AKey, '');
  if not TryStrToInt(Value, Result) then
    Result := ADefault;
end;

procedure TSimpleConfig.SetInt(const AKey: string; AValue: Integer);
begin
  SetValue(AKey, IntToStr(AValue));
end;

end.
```

## Gestion des Fichiers JSON

FreePascal offre un excellent support JSON natif :

```pascal
unit JsonConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser;

type
  TJsonConfig = class
  private
    FJsonObject: TJSONObject;
    FFilePath: string;

    function GetJsonPath(const APath: string): TJSONData;
    procedure SetJsonPath(const APath: string; AValue: TJSONData);
  public
    constructor Create(const AFilePath: string);
    destructor Destroy; override;

    procedure Load;
    procedure Save;

    // Méthodes de lecture avec chemin style "section.key"
    function GetString(const APath: string; const ADefault: string = ''): string;
    function GetInt(const APath: string; ADefault: Integer = 0): Integer;
    function GetBool(const APath: string; ADefault: Boolean = False): Boolean;
    function GetFloat(const APath: string; ADefault: Double = 0.0): Double;

    // Méthodes d'écriture
    procedure SetString(const APath, AValue: string);
    procedure SetInt(const APath: string; AValue: Integer);
    procedure SetBool(const APath: string; AValue: Boolean);
    procedure SetFloat(const APath: string; AValue: Double);

    // Gestion des tableaux
    function GetArray(const APath: string): TJSONArray;
    procedure AddToArray(const APath, AValue: string);
  end;

implementation

constructor TJsonConfig.Create(const AFilePath: string);
begin
  inherited Create;
  FFilePath := AFilePath;

  if FileExists(FFilePath) then
    Load
  else
    FJsonObject := TJSONObject.Create;
end;

destructor TJsonConfig.Destroy;
begin
  Save;
  FJsonObject.Free;
  inherited Destroy;
end;

procedure TJsonConfig.Load;
var
  FileStream: TFileStream;
  Parser: TJSONParser;
begin
  if not FileExists(FFilePath) then
  begin
    FJsonObject := TJSONObject.Create;
    Exit;
  end;

  FileStream := TFileStream.Create(FFilePath, fmOpenRead);
  try
    Parser := TJSONParser.Create(FileStream);
    try
      FJsonObject := Parser.Parse as TJSONObject;
    finally
      Parser.Free;
    end;
  finally
    FileStream.Free;
  end;
end;

procedure TJsonConfig.Save;
var
  FileStream: TFileStream;
  JsonString: string;
begin
  // Créer le répertoire si nécessaire
  ForceDirectories(ExtractFilePath(FFilePath));

  // Formatter le JSON avec indentation
  JsonString := FJsonObject.FormatJSON;

  FileStream := TFileStream.Create(FFilePath, fmCreate);
  try
    FileStream.WriteBuffer(JsonString[1], Length(JsonString));
  finally
    FileStream.Free;
  end;
end;

function TJsonConfig.GetJsonPath(const APath: string): TJSONData;
var
  PathParts: TStringArray;
  Current: TJSONData;
  i: Integer;
begin
  Result := nil;

  // Diviser le chemin par les points
  PathParts := APath.Split('.');

  Current := FJsonObject;
  for i := 0 to High(PathParts) do
  begin
    if Current is TJSONObject then
    begin
      Current := (Current as TJSONObject).Find(PathParts[i]);
      if Current = nil then
        Exit;
    end
    else
      Exit;
  end;

  Result := Current;
end;

procedure TJsonConfig.SetJsonPath(const APath: string; AValue: TJSONData);
var
  PathParts: TStringArray;
  Current, Parent: TJSONObject;
  i: Integer;
  Key: string;
begin
  PathParts := APath.Split('.');

  Current := FJsonObject;

  // Naviguer/créer jusqu'à l'avant-dernière partie
  for i := 0 to High(PathParts) - 1 do
  begin
    if Current.Find(PathParts[i]) = nil then
      Current.Add(PathParts[i], TJSONObject.Create);
    Current := Current.Find(PathParts[i]) as TJSONObject;
  end;

  // Définir la valeur finale
  Key := PathParts[High(PathParts)];
  if Current.Find(Key) <> nil then
    Current.Delete(Key);
  Current.Add(Key, AValue);
end;

function TJsonConfig.GetString(const APath: string; const ADefault: string): string;
var
  Data: TJSONData;
begin
  Data := GetJsonPath(APath);
  if Data <> nil then
    Result := Data.AsString
  else
    Result := ADefault;
end;

function TJsonConfig.GetInt(const APath: string; ADefault: Integer): Integer;
var
  Data: TJSONData;
begin
  Data := GetJsonPath(APath);
  if Data <> nil then
    Result := Data.AsInteger
  else
    Result := ADefault;
end;

function TJsonConfig.GetBool(const APath: string; ADefault: Boolean): Boolean;
var
  Data: TJSONData;
begin
  Data := GetJsonPath(APath);
  if Data <> nil then
    Result := Data.AsBoolean
  else
    Result := ADefault;
end;

function TJsonConfig.GetFloat(const APath: string; ADefault: Double): Double;
var
  Data: TJSONData;
begin
  Data := GetJsonPath(APath);
  if Data <> nil then
    Result := Data.AsFloat
  else
    Result := ADefault;
end;

procedure TJsonConfig.SetString(const APath, AValue: string);
begin
  SetJsonPath(APath, TJSONString.Create(AValue));
end;

procedure TJsonConfig.SetInt(const APath: string; AValue: Integer);
begin
  SetJsonPath(APath, TJSONIntegerNumber.Create(AValue));
end;

procedure TJsonConfig.SetBool(const APath: string; AValue: Boolean);
begin
  SetJsonPath(APath, TJSONBoolean.Create(AValue));
end;

procedure TJsonConfig.SetFloat(const APath: string; AValue: Double);
begin
  SetJsonPath(APath, TJSONFloatNumber.Create(AValue));
end;

function TJsonConfig.GetArray(const APath: string): TJSONArray;
var
  Data: TJSONData;
begin
  Data := GetJsonPath(APath);
  if Data is TJSONArray then
    Result := Data as TJSONArray
  else
    Result := nil;
end;

procedure TJsonConfig.AddToArray(const APath, AValue: string);
var
  Data: TJSONData;
  Arr: TJSONArray;
begin
  Data := GetJsonPath(APath);
  if Data is TJSONArray then
    Arr := Data as TJSONArray
  else
  begin
    Arr := TJSONArray.Create;
    SetJsonPath(APath, Arr);
  end;

  Arr.Add(AValue);
end;

end.
```

### Utilisation du Configuration JSON

```pascal
program JsonConfigExample;

uses
  SysUtils, JsonConfig;

var
  Config: TJsonConfig;
  ConfigPath: string;
begin
  // Chemin standard Linux
  ConfigPath := GetEnvironmentVariable('HOME') + '/.config/myapp/settings.json';

  Config := TJsonConfig.Create(ConfigPath);
  try
    // Lire des valeurs
    WriteLn('Application: ', Config.GetString('application.name', 'MyApp'));
    WriteLn('Version: ', Config.GetString('application.version', '1.0.0'));
    WriteLn('Debug: ', Config.GetBool('application.debug', False));

    // Écrire des valeurs
    Config.SetString('application.name', 'Mon Application');
    Config.SetString('application.version', '1.0.1');
    Config.SetBool('application.debug', True);

    // Configuration imbriquée
    Config.SetString('database.host', 'localhost');
    Config.SetInt('database.port', 5432);
    Config.SetString('database.name', 'myapp_db');

    // Tableaux
    Config.AddToArray('recent_files', '/home/user/doc1.txt');
    Config.AddToArray('recent_files', '/home/user/doc2.txt');

    // Sauvegarde automatique dans le destructeur
  finally
    Config.Free;
  end;
end.
```

## Gestion des Permissions et Sécurité

### Permissions des Fichiers

Sous Linux, les permissions sont cruciales pour la sécurité :

```pascal
unit FilePermissions;

{$mode objfpc}{$H+}

interface

uses
  BaseUnix, SysUtils;

type
  TFilePermissions = class
  public
    // Définir les permissions (équivalent à chmod)
    class function SetPermissions(const AFilePath: string; AMode: Integer): Boolean;

    // Permissions courantes
    class function SetReadOnly(const AFilePath: string): Boolean;
    class function SetUserOnly(const AFilePath: string): Boolean;
    class function SetExecutable(const AFilePath: string): Boolean;

    // Vérifier les permissions
    class function IsReadable(const AFilePath: string): Boolean;
    class function IsWritable(const AFilePath: string): Boolean;
    class function IsExecutable(const AFilePath: string): Boolean;

    // Obtenir le propriétaire
    class function GetOwner(const AFilePath: string; out AUser, AGroup: string): Boolean;
  end;

const
  // Constantes de permissions Unix
  S_IRUSR = &0400;  // Lecture propriétaire
  S_IWUSR = &0200;  // Écriture propriétaire
  S_IXUSR = &0100;  // Exécution propriétaire
  S_IRGRP = &0040;  // Lecture groupe
  S_IWGRP = &0020;  // Écriture groupe
  S_IXGRP = &0010;  // Exécution groupe
  S_IROTH = &0004;  // Lecture autres
  S_IWOTH = &0002;  // Écriture autres
  S_IXOTH = &0001;  // Exécution autres

implementation

class function TFilePermissions.SetPermissions(const AFilePath: string; AMode: Integer): Boolean;
begin
  Result := FpChmod(PChar(AFilePath), AMode) = 0;
end;

class function TFilePermissions.SetReadOnly(const AFilePath: string): Boolean;
begin
  // 444 : lecture seule pour tous
  Result := SetPermissions(AFilePath, S_IRUSR or S_IRGRP or S_IROTH);
end;

class function TFilePermissions.SetUserOnly(const AFilePath: string): Boolean;
begin
  // 600 : lecture/écriture pour le propriétaire seulement
  Result := SetPermissions(AFilePath, S_IRUSR or S_IWUSR);
end;

class function TFilePermissions.SetExecutable(const AFilePath: string): Boolean;
begin
  // 755 : rwxr-xr-x
  Result := SetPermissions(AFilePath,
    S_IRUSR or S_IWUSR or S_IXUSR or  // rwx pour propriétaire
    S_IRGRP or S_IXGRP or              // r-x pour groupe
    S_IROTH or S_IXOTH);               // r-x pour autres
end;

class function TFilePermissions.IsReadable(const AFilePath: string): Boolean;
begin
  Result := FpAccess(PChar(AFilePath), R_OK) = 0;
end;

class function TFilePermissions.IsWritable(const AFilePath: string): Boolean;
begin
  Result := FpAccess(PChar(AFilePath), W_OK) = 0;
end;

class function TFilePermissions.IsExecutable(const AFilePath: string): Boolean;
begin
  Result := FpAccess(PChar(AFilePath), X_OK) = 0;
end;

class function TFilePermissions.GetOwner(const AFilePath: string;
  out AUser, AGroup: string): Boolean;
var
  StatBuf: Stat;
begin
  Result := FpStat(PChar(AFilePath), StatBuf) = 0;
  if Result then
  begin
    // Conversion UID/GID en noms (simplifié)
    AUser := IntToStr(StatBuf.st_uid);
    AGroup := IntToStr(StatBuf.st_gid);
  end;
end;

end.
```


## Configuration Sécurisée

```pascal
unit SecureConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, FilePermissions, md5, base64;

type
  TSecureConfig = class
  private
    FConfigPath: string;
    FConfig: TIniFile;

    function GetSecureConfigPath: string;
    function ValidateChecksum: Boolean;
    procedure UpdateChecksum;
  public
    constructor Create(const AAppName: string);
    destructor Destroy; override;

    // Méthodes sécurisées
    procedure SaveSecureString(const ASection, AKey, AValue: string);
    function LoadSecureString(const ASection, AKey: string;
                             const ADefault: string = ''): string;

    // Gestion des mots de passe (jamais en clair !)
    procedure SavePasswordHash(const ASection, AKey, APassword: string);
    function VerifyPassword(const ASection, AKey, APassword: string): Boolean;
  end;

implementation

constructor TSecureConfig.Create(const AAppName: string);
begin
  inherited Create;

  FConfigPath := GetSecureConfigPath + '/' + AAppName;

  // Créer le répertoire avec permissions restrictives
  if not DirectoryExists(FConfigPath) then
  begin
    ForceDirectories(FConfigPath);
    // Permissions 700 : accès propriétaire uniquement
    TFilePermissions.SetPermissions(FConfigPath,
      S_IRUSR or S_IWUSR or S_IXUSR);
  end;

  FConfigPath := FConfigPath + '/config.ini';

  // Si le fichier existe, vérifier les permissions
  if FileExists(FConfigPath) then
  begin
    if not TFilePermissions.SetUserOnly(FConfigPath) then
      WriteLn('Attention: impossible de sécuriser le fichier de configuration');
  end;
end;

destructor TSecureConfig.Destroy;
begin
  if Assigned(FConfig) then
  begin
    UpdateChecksum;
    FConfig.Free;
  end;
  inherited Destroy;
end;

function TSecureConfig.GetSecureConfigPath: string;
begin
  // Utiliser le répertoire standard XDG_CONFIG_HOME
  Result := GetEnvironmentVariable('XDG_CONFIG_HOME');
  if Result = '' then
    Result := GetEnvironmentVariable('HOME') + '/.config';
end;

function TSecureConfig.ValidateChecksum: Boolean;
var
  StoredChecksum, CurrentChecksum: string;
  FileContent: TStringList;
begin
  Result := False;

  if not FileExists(FConfigPath) then
    Exit;

  FConfig := TIniFile.Create(FConfigPath);
  StoredChecksum := FConfig.ReadString('Security', 'Checksum', '');

  if StoredChecksum = '' then
    Exit(True); // Pas de checksum, première utilisation

  // Calculer le checksum actuel (sans la section Security)
  FileContent := TStringList.Create;
  try
    FileContent.LoadFromFile(FConfigPath);
    // Retirer la ligne du checksum pour le calcul
    FileContent.Text := StringReplace(FileContent.Text,
                                     '[Security]', '', []);
    FileContent.Text := StringReplace(FileContent.Text,
                                     'Checksum=' + StoredChecksum, '', []);

    CurrentChecksum := MD5Print(MD5String(FileContent.Text));
    Result := CurrentChecksum = StoredChecksum;
  finally
    FileContent.Free;
  end;
end;

procedure TSecureConfig.UpdateChecksum;
var
  FileContent: TStringList;
  Checksum: string;
begin
  if not Assigned(FConfig) then
    Exit;

  FileContent := TStringList.Create;
  try
    FileContent.LoadFromFile(FConfigPath);
    // Retirer l'ancienne section Security
    FileContent.Text := StringReplace(FileContent.Text, '[Security]', '', []);
    FileContent.Text := StringReplace(FileContent.Text, 'Checksum=', '', []);

    Checksum := MD5Print(MD5String(FileContent.Text));
    FConfig.WriteString('Security', 'Checksum', Checksum);
    FConfig.UpdateFile;
  finally
    FileContent.Free;
  end;
end;

procedure TSecureConfig.SaveSecureString(const ASection, AKey, AValue: string);
begin
  if not Assigned(FConfig) then
    FConfig := TIniFile.Create(FConfigPath);

  // Encoder en base64 pour éviter les caractères problématiques
  FConfig.WriteString(ASection, AKey, EncodeStringBase64(AValue));
  FConfig.UpdateFile;

  // S'assurer que les permissions sont correctes
  TFilePermissions.SetUserOnly(FConfigPath);
end;

function TSecureConfig.LoadSecureString(const ASection, AKey: string;
                                        const ADefault: string): string;
var
  EncodedValue: string;
begin
  if not Assigned(FConfig) then
    FConfig := TIniFile.Create(FConfigPath);

  EncodedValue := FConfig.ReadString(ASection, AKey, '');
  if EncodedValue <> '' then
    Result := DecodeStringBase64(EncodedValue)
  else
    Result := ADefault;
end;

procedure TSecureConfig.SavePasswordHash(const ASection, AKey, APassword: string);
var
  Hash: string;
  Salt: string;
begin
  // Générer un sel aléatoire
  Randomize;
  Salt := MD5Print(MD5String(DateTimeToStr(Now) + IntToStr(Random(MaxInt))));

  // Créer le hash avec sel
  Hash := MD5Print(MD5String(Salt + APassword));

  if not Assigned(FConfig) then
    FConfig := TIniFile.Create(FConfigPath);

  FConfig.WriteString(ASection, AKey + '_Salt', Salt);
  FConfig.WriteString(ASection, AKey + '_Hash', Hash);
  FConfig.UpdateFile;

  TFilePermissions.SetUserOnly(FConfigPath);
end;

function TSecureConfig.VerifyPassword(const ASection, AKey, APassword: string): Boolean;
var
  StoredHash, StoredSalt, TestHash: string;
begin
  Result := False;

  if not Assigned(FConfig) then
    FConfig := TIniFile.Create(FConfigPath);

  StoredSalt := FConfig.ReadString(ASection, AKey + '_Salt', '');
  StoredHash := FConfig.ReadString(ASection, AKey + '_Hash', '');

  if (StoredSalt = '') or (StoredHash = '') then
    Exit;

  TestHash := MD5Print(MD5String(StoredSalt + APassword));
  Result := TestHash = StoredHash;
end;

end.
```

## Surveillance des Modifications de Configuration

Linux offre des mécanismes pour surveiller les modifications de fichiers en temps réel :

### Utilisation d'inotify pour la Surveillance

```pascal
unit ConfigWatcher;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Linux, BaseUnix;

type
  TConfigChangeEvent = procedure(Sender: TObject; const AFileName: string) of object;

  TConfigWatcher = class(TThread)
  private
    FWatchPath: string;
    FOnChange: TConfigChangeEvent;
    FInotifyFD: Integer;
    FWatchDescriptor: Integer;
    FRunning: Boolean;

    procedure NotifyChange(const AFileName: string);
  protected
    procedure Execute; override;
  public
    constructor Create(const APath: string);
    destructor Destroy; override;

    property OnChange: TConfigChangeEvent read FOnChange write FOnChange;
    procedure StartWatching;
    procedure StopWatching;
  end;

const
  // inotify events
  IN_MODIFY = $00000002;
  IN_CREATE = $00000100;
  IN_DELETE = $00000200;
  IN_MOVED_FROM = $00000040;
  IN_MOVED_TO = $00000080;

implementation

type
  inotify_event = record
    wd: Integer;      // Watch descriptor
    mask: Cardinal;   // Event mask
    cookie: Cardinal; // Cookie for synchronize
    len: Cardinal;    // Length of name
    name: array[0..0] of Char; // Filename
  end;
  Pinotify_event = ^inotify_event;

// Déclarations des fonctions inotify
function inotify_init: Integer; cdecl; external 'c';
function inotify_add_watch(fd: Integer; pathname: PChar; mask: Cardinal): Integer;
  cdecl; external 'c';
function inotify_rm_watch(fd: Integer; wd: Integer): Integer; cdecl; external 'c';

constructor TConfigWatcher.Create(const APath: string);
begin
  inherited Create(True); // Créé suspendu
  FWatchPath := APath;
  FRunning := False;
  FreeOnTerminate := False;
end;

destructor TConfigWatcher.Destroy;
begin
  StopWatching;
  inherited Destroy;
end;

procedure TConfigWatcher.StartWatching;
begin
  if FRunning then
    Exit;

  // Initialiser inotify
  FInotifyFD := inotify_init;
  if FInotifyFD < 0 then
    raise Exception.Create('Impossible d''initialiser inotify');

  // Ajouter le watch
  FWatchDescriptor := inotify_add_watch(FInotifyFD, PChar(FWatchPath),
    IN_MODIFY or IN_CREATE or IN_DELETE or IN_MOVED_FROM or IN_MOVED_TO);

  if FWatchDescriptor < 0 then
  begin
    FpClose(FInotifyFD);
    raise Exception.Create('Impossible de surveiller: ' + FWatchPath);
  end;

  FRunning := True;
  Start; // Démarrer le thread
end;

procedure TConfigWatcher.StopWatching;
begin
  if not FRunning then
    Exit;

  FRunning := False;

  // Retirer le watch et fermer
  if FWatchDescriptor >= 0 then
    inotify_rm_watch(FInotifyFD, FWatchDescriptor);

  if FInotifyFD >= 0 then
    FpClose(FInotifyFD);

  Terminate;
  WaitFor;
end;

procedure TConfigWatcher.NotifyChange(const AFileName: string);
begin
  if Assigned(FOnChange) then
    FOnChange(Self, AFileName);
end;

procedure TConfigWatcher.Execute;
var
  Buffer: array[0..4095] of Byte;
  BytesRead: Integer;
  Event: Pinotify_event;
  Offset: Integer;
  FileName: string;
begin
  while FRunning and not Terminated do
  begin
    // Lire les événements (bloquant)
    BytesRead := FpRead(FInotifyFD, Buffer, SizeOf(Buffer));

    if BytesRead > 0 then
    begin
      Offset := 0;
      while Offset < BytesRead do
      begin
        Event := Pinotify_event(@Buffer[Offset]);

        // Extraire le nom du fichier si présent
        if Event^.len > 0 then
          FileName := PChar(@Event^.name)
        else
          FileName := FWatchPath;

        // Notifier selon le type d'événement
        if (Event^.mask and IN_MODIFY) <> 0 then
          Synchronize(procedure begin NotifyChange(FileName + ' (modifié)'); end)
        else if (Event^.mask and IN_CREATE) <> 0 then
          Synchronize(procedure begin NotifyChange(FileName + ' (créé)'); end)
        else if (Event^.mask and IN_DELETE) <> 0 then
          Synchronize(procedure begin NotifyChange(FileName + ' (supprimé)'); end);

        // Passer à l'événement suivant
        Inc(Offset, SizeOf(inotify_event) + Event^.len);
      end;
    end;
  end;
end;

end.
```

### Utilisation du Surveillant de Configuration

```pascal
program WatchConfig;

uses
  SysUtils, ConfigWatcher;

type
  TConfigMonitor = class
    procedure OnConfigChange(Sender: TObject; const AFileName: string);
  end;

procedure TConfigMonitor.OnConfigChange(Sender: TObject; const AFileName: string);
begin
  WriteLn('Configuration modifiée: ', AFileName);
  WriteLn('Rechargement de la configuration...');
  // Recharger la configuration ici
end;

var
  Watcher: TConfigWatcher;
  Monitor: TConfigMonitor;
  ConfigPath: string;
begin
  ConfigPath := GetEnvironmentVariable('HOME') + '/.config/myapp';

  Monitor := TConfigMonitor.Create;
  Watcher := TConfigWatcher.Create(ConfigPath);
  try
    Watcher.OnChange := @Monitor.OnConfigChange;
    Watcher.StartWatching;

    WriteLn('Surveillance de: ', ConfigPath);
    WriteLn('Appuyez sur Entrée pour arrêter...');
    ReadLn;
  finally
    Watcher.Free;
    Monitor.Free;
  end;
end.
```

## Migration de Configuration

Gérer l'évolution du format de configuration au fil des versions :

```pascal
unit ConfigMigration;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, fpjson, jsonparser, DateUtils;

type
  TConfigMigrator = class
  private
    FConfigPath: string;
    FBackupPath: string;

    function GetConfigVersion: Integer;
    procedure SetConfigVersion(AVersion: Integer);
    procedure BackupConfig;

    // Migrations spécifiques
    procedure MigrateV1ToV2;
    procedure MigrateV2ToV3;
    procedure MigrateIniToJson;
  public
    constructor Create(const AConfigPath: string);

    function NeedsMigration: Boolean;
    procedure Migrate;
    procedure RollbackMigration;
  end;

const
  CURRENT_CONFIG_VERSION = 3;

implementation

constructor TConfigMigrator.Create(const AConfigPath: string);
begin
  inherited Create;
  FConfigPath := AConfigPath;
  FBackupPath := ExtractFilePath(AConfigPath) + 'backups/';

  if not DirectoryExists(FBackupPath) then
    ForceDirectories(FBackupPath);
end;

function TConfigMigrator.GetConfigVersion: Integer;
var
  Ini: TIniFile;
  Json: TJSONObject;
  Parser: TJSONParser;
  FileStream: TFileStream;
begin
  Result := 0;

  if not FileExists(FConfigPath) then
    Exit;

  // Déterminer le type de fichier
  if ExtractFileExt(FConfigPath) = '.ini' then
  begin
    Ini := TIniFile.Create(FConfigPath);
    try
      Result := Ini.ReadInteger('Meta', 'ConfigVersion', 1);
    finally
      Ini.Free;
    end;
  end
  else if ExtractFileExt(FConfigPath) = '.json' then
  begin
    FileStream := TFileStream.Create(FConfigPath, fmOpenRead);
    try
      Parser := TJSONParser.Create(FileStream);
      try
        Json := Parser.Parse as TJSONObject;
        try
          Result := Json.GetPath('meta.version').AsInteger;
        finally
          Json.Free;
        end;
      finally
        Parser.Free;
      end;
    finally
      FileStream.Free;
    end;
  end;
end;

procedure TConfigMigrator.SetConfigVersion(AVersion: Integer);
var
  Ini: TIniFile;
begin
  if ExtractFileExt(FConfigPath) = '.ini' then
  begin
    Ini := TIniFile.Create(FConfigPath);
    try
      Ini.WriteInteger('Meta', 'ConfigVersion', AVersion);
      Ini.WriteDateTime('Meta', 'LastMigration', Now);
      Ini.UpdateFile;
    finally
      Ini.Free;
    end;
  end;
  // Pour JSON, la version est mise à jour pendant la migration
end;

procedure TConfigMigrator.BackupConfig;
var
  BackupFile: string;
begin
  BackupFile := FBackupPath +
                'config_backup_' +
                FormatDateTime('yyyymmdd_hhnnss', Now) +
                ExtractFileExt(FConfigPath);

  if not CopyFile(FConfigPath, BackupFile) then
    raise Exception.Create('Impossible de créer la sauvegarde');

  WriteLn('Sauvegarde créée: ', BackupFile);
end;

procedure TConfigMigrator.MigrateV1ToV2;
var
  Ini: TIniFile;
  OldValue: string;
begin
  WriteLn('Migration V1 → V2...');

  Ini := TIniFile.Create(FConfigPath);
  try
    // Exemple: renommer une section
    if Ini.SectionExists('OldSection') then
    begin
      OldValue := Ini.ReadString('OldSection', 'Key1', '');
      Ini.WriteString('NewSection', 'Key1', OldValue);
      Ini.EraseSection('OldSection');
    end;

    // Ajouter de nouvelles valeurs par défaut
    if not Ini.ValueExists('Display', 'DarkMode') then
      Ini.WriteBool('Display', 'DarkMode', False);

    // Convertir les anciennes valeurs
    if Ini.ValueExists('Settings', 'WindowSize') then
    begin
      OldValue := Ini.ReadString('Settings', 'WindowSize', '800x600');
      Ini.WriteInteger('Display', 'Width', 800);
      Ini.WriteInteger('Display', 'Height', 600);
      Ini.DeleteKey('Settings', 'WindowSize');
    end;

    Ini.UpdateFile;
  finally
    Ini.Free;
  end;

  SetConfigVersion(2);
end;

procedure TConfigMigrator.MigrateV2ToV3;
begin
  WriteLn('Migration V2 → V3...');

  // Cette version convertit de INI vers JSON
  MigrateIniToJson;

  // Mise à jour du chemin
  FConfigPath := ChangeFileExt(FConfigPath, '.json');
end;

procedure TConfigMigrator.MigrateIniToJson;
var
  Ini: TIniFile;
  Json: TJSONObject;
  Sections: TStringList;
  Keys: TStringList;
  i, j: Integer;
  SectionObj: TJSONObject;
  Value: string;
  IntValue: Integer;
  BoolValue: Boolean;
  JsonPath: string;
begin
  WriteLn('Conversion INI → JSON...');

  Ini := TIniFile.Create(FConfigPath);
  Json := TJSONObject.Create;
  Sections := TStringList.Create;
  Keys := TStringList.Create;
  try
    // Ajouter les métadonnées
    Json.Add('meta', TJSONObject.Create);
    (Json.Get('meta') as TJSONObject).Add('version', CURRENT_CONFIG_VERSION);
    (Json.Get('meta') as TJSONObject).Add('migrated', FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));

    // Lire toutes les sections
    Ini.ReadSections(Sections);

    for i := 0 to Sections.Count - 1 do
    begin
      if Sections[i] = 'Meta' then
        Continue; // Ignorer l'ancienne section Meta

      SectionObj := TJSONObject.Create;
      Json.Add(LowerCase(Sections[i]), SectionObj);

      // Lire toutes les clés de la section
      Ini.ReadSection(Sections[i], Keys);

      for j := 0 to Keys.Count - 1 do
      begin
        Value := Ini.ReadString(Sections[i], Keys[j], '');

        // Essayer de détecter le type
        if TryStrToInt(Value, IntValue) then
          SectionObj.Add(Keys[j], IntValue)
        else if TryStrToBool(Value, BoolValue) then
          SectionObj.Add(Keys[j], BoolValue)
        else
          SectionObj.Add(Keys[j], Value);
      end;
    end;

    // Sauvegarder en JSON
    JsonPath := ChangeFileExt(FConfigPath, '.json');

    with TFileStream.Create(JsonPath, fmCreate) do
    try
      WriteBuffer(Json.FormatJSON[1], Length(Json.FormatJSON));
    finally
      Free;
    end;

    // Renommer l'ancien fichier INI
    RenameFile(FConfigPath, FConfigPath + '.old');

  finally
    Json.Free;
    Keys.Free;
    Sections.Free;
    Ini.Free;
  end;
end;

function TConfigMigrator.NeedsMigration: Boolean;
begin
  Result := GetConfigVersion < CURRENT_CONFIG_VERSION;
end;

procedure TConfigMigrator.Migrate;
var
  CurrentVersion: Integer;
begin
  if not NeedsMigration then
  begin
    WriteLn('Configuration à jour (version ', CURRENT_CONFIG_VERSION, ')');
    Exit;
  end;

  // Sauvegarde avant migration
  BackupConfig;

  CurrentVersion := GetConfigVersion;
  WriteLn('Migration de la version ', CurrentVersion, ' vers ', CURRENT_CONFIG_VERSION);

  // Appliquer les migrations nécessaires
  while CurrentVersion < CURRENT_CONFIG_VERSION do
  begin
    case CurrentVersion of
      0, 1: MigrateV1ToV2;
      2: MigrateV2ToV3;
      // Ajouter d'autres migrations ici
    end;

    CurrentVersion := GetConfigVersion;
  end;

  WriteLn('Migration terminée avec succès');
end;

procedure TConfigMigrator.RollbackMigration;
var
  BackupFiles: TStringList;
  LatestBackup: string;
begin
  BackupFiles := TStringList.Create;
  try
    // Trouver la dernière sauvegarde
    FindAllFiles(BackupFiles, FBackupPath, 'config_backup_*', False);

    if BackupFiles.Count = 0 then
      raise Exception.Create('Aucune sauvegarde disponible');

    BackupFiles.Sort;
    LatestBackup := BackupFiles[BackupFiles.Count - 1];

    // Restaurer
    if CopyFile(LatestBackup, FConfigPath) then
      WriteLn('Configuration restaurée depuis: ', LatestBackup)
    else
      raise Exception.Create('Impossible de restaurer la configuration');
  finally
    BackupFiles.Free;
  end;
end;

end.
```

## Configuration Multi-Environnements

Gérer différentes configurations selon l'environnement (développement, test, production) :

```pascal
unit EnvironmentConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles;

type
  TEnvironment = (envDevelopment, envTesting, envStaging, envProduction);

  TEnvironmentConfig = class
  private
    FEnvironment: TEnvironment;
    FBaseConfigPath: string;
    FConfig: TIniFile;

    function GetEnvironmentName: string;
    function GetConfigFileName: string;
    procedure LoadEnvironmentConfig;
    procedure LoadDefaultConfig;
    procedure MergeConfigs(ABase, AOverride: TIniFile);
  public
    constructor Create;
    destructor Destroy; override;

    function GetValue(const ASection, AKey, ADefault: string): string;
    function GetDatabaseUrl: string;
    function GetApiEndpoint: string;
    function IsDebugMode: Boolean;

    property Environment: TEnvironment read FEnvironment;
    property EnvironmentName: string read GetEnvironmentName;
  end;

implementation

constructor TEnvironmentConfig.Create;
var
  EnvVar: string;
begin
  inherited Create;

  // Déterminer l'environnement depuis la variable d'environnement
  EnvVar := GetEnvironmentVariable('APP_ENV');

  if EnvVar = 'production' then
    FEnvironment := envProduction
  else if EnvVar = 'staging' then
    FEnvironment := envStaging
  else if EnvVar = 'testing' then
    FEnvironment := envTesting
  else
    FEnvironment := envDevelopment; // Par défaut

  FBaseConfigPath := GetEnvironmentVariable('XDG_CONFIG_HOME');
  if FBaseConfigPath = '' then
    FBaseConfigPath := GetEnvironmentVariable('HOME') + '/.config';
  FBaseConfigPath := FBaseConfigPath + '/myapp/';

  LoadEnvironmentConfig;
end;

destructor TEnvironmentConfig.Destroy;
begin
  if Assigned(FConfig) then
    FConfig.Free;
  inherited Destroy;
end;

function TEnvironmentConfig.GetEnvironmentName: string;
begin
  case FEnvironment of
    envDevelopment: Result := 'development';
    envTesting: Result := 'testing';
    envStaging: Result := 'staging';
    envProduction: Result := 'production';
  end;
end;

function TEnvironmentConfig.GetConfigFileName: string;
begin
  Result := FBaseConfigPath + 'config.' + GetEnvironmentName + '.ini';
end;

procedure TEnvironmentConfig.LoadDefaultConfig;
var
  DefaultConfig: TIniFile;
  DefaultPath: string;
begin
  DefaultPath := FBaseConfigPath + 'config.default.ini';

  if not FileExists(DefaultPath) then
  begin
    // Créer la configuration par défaut
    DefaultConfig := TIniFile.Create(DefaultPath);
    try
      // Valeurs par défaut communes
      DefaultConfig.WriteString('Application', 'Name', 'MyApp');
      DefaultConfig.WriteString('Application', 'Version', '1.0.0');
      DefaultConfig.WriteBool('Application', 'Debug', False);

      DefaultConfig.WriteString('Database', 'Host', 'localhost');
      DefaultConfig.WriteInteger('Database', 'Port', 5432);
      DefaultConfig.WriteString('Database', 'Name', 'myapp');

      DefaultConfig.WriteString('API', 'BaseUrl', 'http://localhost:8080');
      DefaultConfig.WriteInteger('API', 'Timeout', 30000);

      DefaultConfig.WriteString('Logging', 'Level', 'info');
      DefaultConfig.WriteString('Logging', 'Path', '/var/log/myapp/');

      DefaultConfig.UpdateFile;
    finally
      DefaultConfig.Free;
    end;
  end;

  FConfig := TIniFile.Create(DefaultPath);
end;

procedure TEnvironmentConfig.MergeConfigs(ABase, AOverride: TIniFile);
var
  Sections, Keys: TStringList;
  i, j: Integer;
  Value: string;
begin
  Sections := TStringList.Create;
  Keys := TStringList.Create;
  try
    // Copier/surcharger les valeurs
    AOverride.ReadSections(Sections);

    for i := 0 to Sections.Count - 1 do
    begin
      AOverride.ReadSection(Sections[i], Keys);

      for j := 0 to Keys.Count - 1 do
      begin
        Value := AOverride.ReadString(Sections[i], Keys[j], '');
        ABase.WriteString(Sections[i], Keys[j], Value);
      end;
    end;
  finally
    Keys.Free;
    Sections.Free;
  end;
end;

procedure TEnvironmentConfig.LoadEnvironmentConfig;
var
  EnvConfig: TIniFile;
  ConfigFile: string;
begin
  // Charger d'abord la configuration par défaut
  LoadDefaultConfig;

  // Puis surcharger avec l'environnement spécifique
  ConfigFile := GetConfigFileName;

  if FileExists(ConfigFile) then
  begin
    EnvConfig := TIniFile.Create(ConfigFile);
    try
      MergeConfigs(FConfig, EnvConfig);
    finally
      EnvConfig.Free;
    end;
  end
  else
  begin
    // Créer la configuration d'environnement avec des exemples
    EnvConfig := TIniFile.Create(ConfigFile);
    try
      case FEnvironment of
        envDevelopment:
          begin
            EnvConfig.WriteBool('Application', 'Debug', True);
            EnvConfig.WriteString('Database', 'Name', 'myapp_dev');
            EnvConfig.WriteString('Logging', 'Level', 'debug');
          end;

        envTesting:
          begin
            EnvConfig.WriteString('Database', 'Name', 'myapp_test');
            EnvConfig.WriteString('Logging', 'Level', 'warning');
          end;

        envStaging:
          begin
            EnvConfig.WriteString('Database', 'Host', 'staging-db.example.com');
            EnvConfig.WriteString('API', 'BaseUrl', 'https://staging-api.example.com');
          end;

        envProduction:
          begin
            EnvConfig.WriteBool('Application', 'Debug', False);
            EnvConfig.WriteString('Database', 'Host', 'prod-db.example.com');
            EnvConfig.WriteString('API', 'BaseUrl', 'https://api.example.com');
            EnvConfig.WriteString('Logging', 'Level', 'error');
          end;
      end;

      EnvConfig.UpdateFile;

      // Recharger avec les nouvelles valeurs
      MergeConfigs(FConfig, EnvConfig);
    finally
      EnvConfig.Free;
    end;
  end;

  WriteLn('Configuration chargée pour l''environnement: ', GetEnvironmentName);
end;

function TEnvironmentConfig.GetValue(const ASection, AKey, ADefault: string): string;
begin
  if Assigned(FConfig) then
    Result := FConfig.ReadString(ASection, AKey, ADefault)
  else
    Result := ADefault;
end;

function TEnvironmentConfig.GetDatabaseUrl: string;
var
  Host, Port, Name, User, Password: string;
begin
  Host := GetValue('Database', 'Host', 'localhost');
  Port := GetValue('Database', 'Port', '5432');
  Name := GetValue('Database', 'Name', 'myapp');
  User := GetValue('Database', 'User', 'postgres');
  Password := GetValue('Database', 'Password', '');

  // Construction de l'URL de connexion
  Result := Format('postgresql://%s:%s@%s:%s/%s',
                   [User, Password, Host, Port, Name]);
end;

function TEnvironmentConfig.GetApiEndpoint: string;
begin
  Result := GetValue('API', 'BaseUrl', 'http://localhost:8080');
end;

function TEnvironmentConfig.IsDebugMode: Boolean;
var
  DebugStr: string;
begin
  DebugStr := GetValue('Application', 'Debug', 'false');
  Result := LowerCase(DebugStr) = 'true';
end;

end.
```

### Utilisation de la Configuration Multi-Environnements

```pascal
program EnvironmentConfigExample;

uses
  SysUtils, EnvironmentConfig;

var
  Config: TEnvironmentConfig;
begin
  // Définir l'environnement via variable d'environnement
  // export APP_ENV=development  # Dans le terminal avant de lancer

  Config := TEnvironmentConfig.Create;
  try
    WriteLn('=== Configuration Active ===');
    WriteLn('Environnement: ', Config.EnvironmentName);
    WriteLn('Mode Debug: ', Config.IsDebugMode);
    WriteLn('Base de données: ', Config.GetDatabaseUrl);
    WriteLn('API Endpoint: ', Config.GetApiEndpoint);
    WriteLn('Niveau de log: ', Config.GetValue('Logging', 'Level', 'info'));

    // Utilisation conditionnelle selon l'environnement
    if Config.Environment = envProduction then
    begin
      WriteLn('⚠️  Mode Production - Soyez prudent !');
      // Désactiver les fonctionnalités dangereuses
    end
    else if Config.IsDebugMode then
    begin
      WriteLn('🔧 Mode Debug activé');
      // Activer le logging détaillé
    end;
  finally
    Config.Free;
  end;
end;
```

## Gestion des Secrets et Variables d'Environnement

### Fichier .env pour les Secrets

```pascal
unit DotEnvConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, unix;

type
  TEnvVars = specialize TFPGMap<string, string>;

  TDotEnvConfig = class
  private
    FEnvFile: string;
    FVariables: TEnvVars;
    FLoaded: Boolean;

    procedure ParseEnvLine(const ALine: string);
    function ExpandVariables(const AValue: string): string;
  public
    constructor Create(const AEnvFile: string = '.env');
    destructor Destroy; override;

    procedure Load;
    procedure LoadIfExists;
    function GetValue(const AKey: string; const ADefault: string = ''): string;
    procedure SetSystemEnvironment;

    property Loaded: Boolean read FLoaded;
  end;

implementation

constructor TDotEnvConfig.Create(const AEnvFile: string);
begin
  inherited Create;
  FVariables := TEnvVars.Create;

  // Chercher le fichier .env dans plusieurs emplacements
  if AEnvFile = '.env' then
  begin
    // Ordre de priorité
    if FileExists('./.env.local') then
      FEnvFile := './.env.local'
    else if FileExists('./.env') then
      FEnvFile := './.env'
    else if FileExists(GetEnvironmentVariable('HOME') + '/.env') then
      FEnvFile := GetEnvironmentVariable('HOME') + '/.env'
    else
      FEnvFile := AEnvFile;
  end
  else
    FEnvFile := AEnvFile;

  FLoaded := False;
end;

destructor TDotEnvConfig.Destroy;
begin
  FVariables.Free;
  inherited Destroy;
end;

procedure TDotEnvConfig.ParseEnvLine(const ALine: string);
var
  TrimmedLine: string;
  EqualPos: Integer;
  Key, Value: string;
begin
  TrimmedLine := Trim(ALine);

  // Ignorer les lignes vides et les commentaires
  if (TrimmedLine = '') or (TrimmedLine[1] = '#') then
    Exit;

  EqualPos := Pos('=', TrimmedLine);
  if EqualPos > 0 then
  begin
    Key := Trim(Copy(TrimmedLine, 1, EqualPos - 1));
    Value := Copy(TrimmedLine, EqualPos + 1, Length(TrimmedLine));

    // Enlever les guillemets si présents
    if (Length(Value) >= 2) then
    begin
      if ((Value[1] = '"') and (Value[Length(Value)] = '"')) or
         ((Value[1] = '''') and (Value[Length(Value)] = '''')) then
      begin
        Value := Copy(Value, 2, Length(Value) - 2);
      end;
    end;

    // Expansion des variables
    Value := ExpandVariables(Value);

    // Stocker la variable
    FVariables[Key] := Value;
  end;
end;

function TDotEnvConfig.ExpandVariables(const AValue: string): string;
var
  StartPos, EndPos: Integer;
  VarName, VarValue: string;
begin
  Result := AValue;

  // Remplacer ${VAR} ou $VAR
  StartPos := Pos('${', Result);
  while StartPos > 0 do
  begin
    EndPos := Pos('}', Result);
    if EndPos > StartPos then
    begin
      VarName := Copy(Result, StartPos + 2, EndPos - StartPos - 2);

      // Chercher d'abord dans nos variables
      VarValue := GetValue(VarName, '');

      // Sinon dans l'environnement système
      if VarValue = '' then
        VarValue := GetEnvironmentVariable(VarName);

      Result := Copy(Result, 1, StartPos - 1) +
                VarValue +
                Copy(Result, EndPos + 1, Length(Result));
    end
    else
      Break;

    StartPos := Pos('${', Result);
  end;

  // Remplacer $VAR simple
  StartPos := Pos('$', Result);
  if (StartPos > 0) and (StartPos < Length(Result)) then
  begin
    EndPos := StartPos + 1;
    while (EndPos <= Length(Result)) and
          (Result[EndPos] in ['A'..'Z', 'a'..'z', '0'..'9', '_']) do
      Inc(EndPos);

    VarName := Copy(Result, StartPos + 1, EndPos - StartPos - 1);
    VarValue := GetValue(VarName, GetEnvironmentVariable(VarName));

    Result := Copy(Result, 1, StartPos - 1) +
              VarValue +
              Copy(Result, EndPos, Length(Result));
  end;
end;

procedure TDotEnvConfig.Load;
var
  EnvFile: TStringList;
  i: Integer;
begin
  if not FileExists(FEnvFile) then
    raise Exception.Create('Fichier .env introuvable: ' + FEnvFile);

  EnvFile := TStringList.Create;
  try
    EnvFile.LoadFromFile(FEnvFile);

    for i := 0 to EnvFile.Count - 1 do
      ParseEnvLine(EnvFile[i]);

    FLoaded := True;
    WriteLn('Variables d''environnement chargées depuis: ', FEnvFile);
  finally
    EnvFile.Free;
  end;
end;

procedure TDotEnvConfig.LoadIfExists;
begin
  if FileExists(FEnvFile) then
    Load
  else
    WriteLn('Fichier .env non trouvé, utilisation des variables système');
end;

function TDotEnvConfig.GetValue(const AKey: string; const ADefault: string): string;
var
  Index: Integer;
begin
  Index := FVariables.IndexOf(AKey);
  if Index >= 0 then
    Result := FVariables.Data[Index]
  else
  begin
    // Chercher dans l'environnement système
    Result := GetEnvironmentVariable(AKey);
    if Result = '' then
      Result := ADefault;
  end;
end;

procedure TDotEnvConfig.SetSystemEnvironment;
var
  i: Integer;
begin
  // Exporter toutes les variables vers l'environnement système
  // Note : sous Linux, utiliser fpSetEnv de l'unité unix
  for i := 0 to FVariables.Count - 1 do
    fpSetEnv(PChar(FVariables.Keys[i]),
             PChar(FVariables.Data[i]), 1);
end;

end.
```

### Exemple de Fichier .env

```bash
# Fichier .env - NE JAMAIS COMMITER DANS GIT !
# Configuration de développement local

# Application
APP_NAME="Mon Application"
APP_ENV=development
APP_DEBUG=true
APP_PORT=8080

# Base de données
DB_HOST=localhost
DB_PORT=5432
DB_NAME=myapp_dev
DB_USER=postgres
DB_PASSWORD=secret123
DB_URL="postgresql://${DB_USER}:${DB_PASSWORD}@${DB_HOST}:${DB_PORT}/${DB_NAME}"

# API Keys (SECRETS - Ne jamais partager !)
API_KEY=sk_test_abcdef123456789
JWT_SECRET=mon-secret-jwt-super-secure
ENCRYPTION_KEY=0123456789abcdef0123456789abcdef

# Services externes
SMTP_HOST=smtp.mailtrap.io
SMTP_PORT=2525
SMTP_USER=user@example.com
SMTP_PASSWORD=mailpassword

# Chemins
LOG_PATH=/var/log/myapp/
UPLOAD_PATH=${HOME}/uploads/
TEMP_PATH=/tmp/myapp/

# Features flags
FEATURE_NEW_UI=true
FEATURE_BETA_API=false
```

### Utilisation Sécurisée des Secrets

```pascal
unit SecretManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DotEnvConfig, md5, blowfish, base64;

type
  TSecretManager = class
  private
    FDotEnv: TDotEnvConfig;
    FMasterKey: string;

    function GetMasterKey: string;
    function EncryptString(const AValue: string): string;
    function DecryptString(const AValue: string): string;
  public
    constructor Create;
    destructor Destroy; override;

    // Récupération sécurisée
    function GetSecret(const AKey: string; const ADefault: string = ''): string;
    function GetDatabasePassword: string;
    function GetApiKey: string;
    function GetJwtSecret: string;

    // Validation
    function ValidateSecrets: Boolean;
    procedure CheckRequiredSecrets;
  end;

implementation

constructor TSecretManager.Create;
begin
  inherited Create;

  FDotEnv := TDotEnvConfig.Create('.env');
  FDotEnv.LoadIfExists;

  FMasterKey := GetMasterKey;
end;

destructor TSecretManager.Destroy;
begin
  // Effacer les secrets de la mémoire
  FMasterKey := StringOfChar(#0, Length(FMasterKey));

  FDotEnv.Free;
  inherited Destroy;
end;

function TSecretManager.GetMasterKey: string;
var
  KeyFile: string;
  KeyData: TStringList;
begin
  // La clé maître peut venir de plusieurs sources

  // 1. Variable d'environnement
  Result := GetEnvironmentVariable('APP_MASTER_KEY');

  // 2. Fichier de clé séparé (meilleure pratique)
  if Result = '' then
  begin
    KeyFile := GetEnvironmentVariable('HOME') + '/.config/myapp/.master.key';
    if FileExists(KeyFile) then
    begin
      KeyData := TStringList.Create;
      try
        KeyData.LoadFromFile(KeyFile);
        if KeyData.Count > 0 then
          Result := Trim(KeyData[0]);
      finally
        KeyData.Free;
      end;
    end;
  end;

  // 3. Générer une clé si aucune n'existe (développement uniquement)
  if Result = '' then
  begin
    if FDotEnv.GetValue('APP_ENV', 'production') = 'development' then
    begin
      Result := MD5Print(MD5String(DateTimeToStr(Now) + 'dev-key'));
      WriteLn('⚠️  Clé de développement générée - Ne pas utiliser en production!');
    end
    else
      raise Exception.Create('Clé maître manquante en production!');
  end;
end;

function TSecretManager.EncryptString(const AValue: string): string;
var
  Cipher: TBlowFishEncryptStream;
  InStream, OutStream: TStringStream;
begin
  InStream := TStringStream.Create(AValue);
  OutStream := TStringStream.Create('');
  try
    Cipher := TBlowFishEncryptStream.Create(FMasterKey, OutStream);
    try
      Cipher.CopyFrom(InStream, InStream.Size);
    finally
      Cipher.Free;
    end;

    Result := EncodeStringBase64(OutStream.DataString);
  finally
    InStream.Free;
    OutStream.Free;
  end;
end;

function TSecretManager.DecryptString(const AValue: string): string;
var
  Cipher: TBlowFishDecryptStream;
  InStream, OutStream: TStringStream;
  DecodedValue: string;
begin
  DecodedValue := DecodeStringBase64(AValue);
  InStream := TStringStream.Create(DecodedValue);
  OutStream := TStringStream.Create('');
  try
    Cipher := TBlowFishDecryptStream.Create(FMasterKey, InStream);
    try
      OutStream.CopyFrom(Cipher, Cipher.Size);
    finally
      Cipher.Free;
    end;

    Result := OutStream.DataString;
  finally
    InStream.Free;
    OutStream.Free;
  end;
end;

function TSecretManager.GetSecret(const AKey: string; const ADefault: string): string;
begin
  Result := FDotEnv.GetValue(AKey, ADefault);

  // Les secrets critiques ne devraient jamais être vides
  if (Result = '') and (Pos('PASSWORD', UpperCase(AKey)) > 0) then
    WriteLn('⚠️  Attention: Secret vide pour ', AKey);
end;

function TSecretManager.GetDatabasePassword: string;
begin
  Result := GetSecret('DB_PASSWORD', '');

  // En production, le mot de passe pourrait être chiffré
  if FDotEnv.GetValue('APP_ENV', '') = 'production' then
  begin
    if Pos('enc:', Result) = 1 then
    begin
      // Le mot de passe est chiffré, le déchiffrer
      Result := DecryptString(Copy(Result, 5, Length(Result)));
    end;
  end;
end;

function TSecretManager.GetApiKey: string;
begin
  Result := GetSecret('API_KEY', '');

  // Validation du format
  if (Result <> '') and (Length(Result) < 20) then
    WriteLn('⚠️  Clé API semble invalide (trop courte)');
end;

function TSecretManager.GetJwtSecret: string;
begin
  Result := GetSecret('JWT_SECRET', '');

  // S'assurer que le secret JWT est assez fort
  if Length(Result) < 32 then
    raise Exception.Create('JWT secret trop faible (minimum 32 caractères)');
end;

function TSecretManager.ValidateSecrets: Boolean;
var
  Errors: TStringList;
begin
  Result := True;
  Errors := TStringList.Create;
  try
    // Vérifier les secrets requis
    if GetSecret('DB_PASSWORD') = '' then
    begin
      Errors.Add('DB_PASSWORD manquant');
      Result := False;
    end;

    if GetSecret('API_KEY') = '' then
    begin
      Errors.Add('API_KEY manquant');
      Result := False;
    end;

    if GetSecret('JWT_SECRET') = '' then
    begin
      Errors.Add('JWT_SECRET manquant');
      Result := False;
    end;

    // Vérifier la force des secrets
    if Length(GetSecret('JWT_SECRET')) < 32 then
    begin
      Errors.Add('JWT_SECRET trop court (min 32 caractères)');
      Result := False;
    end;

    if not Result then
    begin
      WriteLn('❌ Erreurs de validation des secrets:');
      WriteLn(Errors.Text);
    end
    else
      WriteLn('✅ Tous les secrets sont valides');
  finally
    Errors.Free;
  end;
end;

procedure TSecretManager.CheckRequiredSecrets;
begin
  if not ValidateSecrets then
    raise Exception.Create('Configuration des secrets invalide');
end;

end.
```

## Template de Configuration

Créer des templates pour faciliter le déploiement :

```pascal
unit ConfigTemplate;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser;

type
  TConfigTemplate = class
  private
    FTemplatePath: string;
    FOutputPath: string;
    FVariables: TStringList;

    function ProcessTemplate(const AContent: string): string;
    function PromptForValue(const AVariable, ADescription: string): string;
  public
    constructor Create(const ATemplatePath, AOutputPath: string);
    destructor Destroy; override;

    procedure AddVariable(const AName, AValue: string);
    procedure Generate;
    procedure GenerateInteractive;
  end;

implementation

constructor TConfigTemplate.Create(const ATemplatePath, AOutputPath: string);
begin
  inherited Create;
  FTemplatePath := ATemplatePath;
  FOutputPath := AOutputPath;
  FVariables := TStringList.Create;
end;

destructor TConfigTemplate.Destroy;
begin
  FVariables.Free;
  inherited Destroy;
end;

procedure TConfigTemplate.AddVariable(const AName, AValue: string);
begin
  FVariables.Values[AName] := AValue;
end;

function TConfigTemplate.ProcessTemplate(const AContent: string): string;
var
  i: Integer;
  VarName, VarValue: string;
begin
  Result := AContent;

  // Remplacer les variables {{VAR_NAME}}
  for i := 0 to FVariables.Count - 1 do
  begin
    VarName := '{{' + FVariables.Names[i] + '}}';
    VarValue := FVariables.ValueFromIndex[i];
    Result := StringReplace(Result, VarName, VarValue, [rfReplaceAll]);
  end;
end;

function TConfigTemplate.PromptForValue(const AVariable, ADescription: string): string;
begin
  Write(ADescription, ' [', AVariable, ']: ');
  ReadLn(Result);
end;

procedure TConfigTemplate.Generate;
var
  Template, Output: TStringList;
  ProcessedContent: string;
begin
  if not FileExists(FTemplatePath) then
    raise Exception.Create('Template introuvable: ' + FTemplatePath);

  Template := TStringList.Create;
  Output := TStringList.Create;
  try
    Template.LoadFromFile(FTemplatePath);
    ProcessedContent := ProcessTemplate(Template.Text);

    Output.Text := ProcessedContent;
    Output.SaveToFile(FOutputPath);

    WriteLn('Configuration générée: ', FOutputPath);
  finally
    Template.Free;
    Output.Free;
  end;
end;

procedure TConfigTemplate.GenerateInteractive;
var
  Template: TStringList;
  i: Integer;
  Line, VarName: string;
  StartPos, EndPos: Integer;
  Description: string;
begin
  Template := TStringList.Create;
  try
    Template.LoadFromFile(FTemplatePath);

    WriteLn('=== Génération Interactive de Configuration ===');
    WriteLn('Template: ', FTemplatePath);
    WriteLn('');

    // Rechercher toutes les variables dans le template
    for i := 0 to Template.Count - 1 do
    begin
      Line := Template[i];

      // Chercher les commentaires descriptifs
      if Pos('# {{', Line) > 0 then
      begin
        StartPos := Pos('{{', Line) + 2;
        EndPos := Pos('}}', Line);
        if EndPos > StartPos then
        begin
          VarName := Copy(Line, StartPos, EndPos - StartPos);

          // Extraire la description
          Description := Trim(Copy(Line, EndPos + 2, Length(Line)));
          if Description = '' then
            Description := 'Entrez la valeur pour ' + VarName;

          // Demander la valeur si pas déjà définie
          if FVariables.Values[VarName] = '' then
            AddVariable(VarName, PromptForValue(VarName, Description));
        end;
      end;
    end;

    // Générer le fichier final
    Generate;
  finally
    Template.Free;
  end;
end;

end.
```

### Exemple de Template de Configuration

```ini
# Template de configuration - config.template.ini
# Généré le {{GENERATION_DATE}}

[Application]
Name={{APP_NAME}}  # {{APP_NAME}}: Nom de l'application
Version={{VERSION}}  # {{VERSION}}: Version (ex: 1.0.0)
Environment={{ENVIRONMENT}}  # {{ENVIRONMENT}}: development/staging/production
Debug={{DEBUG_MODE}}  # {{DEBUG_MODE}}: true/false

[Database]
Host={{DB_HOST}}  # {{DB_HOST}}: Serveur de base de données
Port={{DB_PORT}}  # {{DB_PORT}}: Port (défaut: 5432)
Database={{DB_NAME}}  # {{DB_NAME}}: Nom de la base
Username={{DB_USER}}  # {{DB_USER}}: Utilisateur PostgreSQL
# Le mot de passe doit être configuré séparément pour la sécurité

[API]
BaseUrl={{API_URL}}  # {{API_URL}}: URL de base de l'API
Timeout={{API_TIMEOUT}}  # {{API_TIMEOUT}}: Timeout en ms (défaut: 30000)
MaxRetries={{API_RETRIES}}  # {{API_RETRIES}}: Nombre de tentatives (défaut: 3)

[Logging]
Level={{LOG_LEVEL}}  # {{LOG_LEVEL}}: debug/info/warning/error
Path={{LOG_PATH}}  # {{LOG_PATH}}: Chemin des logs
MaxSize={{LOG_MAX_SIZE}}  # {{LOG_MAX_SIZE}}: Taille max en MB
RotationDays={{LOG_ROTATION}}  # {{LOG_ROTATION}}: Rotation en jours

[Email]
SMTPHost={{SMTP_HOST}}  # {{SMTP_HOST}}: Serveur SMTP
SMTPPort={{SMTP_PORT}}  # {{SMTP_PORT}}: Port SMTP (25/465/587)
FromAddress={{EMAIL_FROM}}  # {{EMAIL_FROM}}: Adresse d'envoi
```

## Bonnes Pratiques et Recommandations

### Structure de Répertoires Recommandée

```
/home/user/
└── .config/
    └── myapp/
        ├── config.ini           # Configuration principale
        ├── config.d/            # Configurations modulaires
        │   ├── database.conf
        │   ├── logging.conf
        │   └── features.conf
        ├── environments/        # Par environnement
        │   ├── development.ini
        │   ├── staging.ini
        │   └── production.ini
        ├── backups/            # Sauvegardes automatiques
        │   └── config_20240115_120000.ini
        └── .master.key         # Clé de chiffrement (600)
```

### Script de Déploiement

```bash
#!/bin/bash
# deploy-config.sh - Script de déploiement de configuration

APP_NAME="myapp"
CONFIG_DIR="$HOME/.config/$APP_NAME"

# Créer la structure de répertoires
mkdir -p "$CONFIG_DIR"/{config.d,environments,backups}

# Définir les permissions correctes
chmod 700 "$CONFIG_DIR"

# Copier les templates
cp config.template.ini "$CONFIG_DIR/"

# Générer la configuration selon l'environnement
if [ "$1" == "production" ]; then
    echo "⚠️  Configuration de production"
    chmod 600 "$CONFIG_DIR"/*.ini

    # Vérifier les secrets
    if [ ! -f "$CONFIG_DIR/.master.key" ]; then
        echo "❌ Clé maître manquante!"
        exit 1
    fi
else
    echo "📝 Configuration de développement"
fi

echo "✅ Configuration déployée dans $CONFIG_DIR"
```

### Checklist de Sécurité

```pascal
unit ConfigSecurity;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TSecurityChecker = class
  public
    class function CheckConfigSecurity(const AConfigPath: string): Boolean;
    class procedure GenerateSecurityReport(const AConfigPath: string);
  end;

implementation

class function TSecurityChecker.CheckConfigSecurity(const AConfigPath: string): Boolean;
var
  Issues: TStringList;
begin
  Result := True;
  Issues := TStringList.Create;
  try
    // Vérifier les permissions
    if not TFilePermissions.IsUserOnly(AConfigPath) then
    begin
      Issues.Add('❌ Permissions trop permissives');
      Result := False;
    end;

    // Vérifier l'absence de mots de passe en clair
    // (implémentation simplifiée)

    // Vérifier le chiffrement
    // ...

    if Issues.Count > 0 then
    begin
      WriteLn('Problèmes de sécurité détectés:');
      WriteLn(Issues.Text);
    end
    else
      WriteLn('✅ Configuration sécurisée');
  finally
    Issues.Free;
  end;
end;

class procedure TSecurityChecker.GenerateSecurityReport(const AConfigPath: string);
begin
  WriteLn('=== Rapport de Sécurité ===');
  WriteLn('Fichier: ', AConfigPath);
  WriteLn('Date: ', DateTimeToStr(Now));

  CheckConfigSecurity(AConfigPath);

  // Recommandations
  WriteLn('');
  WriteLn('Recommandations:');
  WriteLn('• Ne jamais commiter les fichiers .env dans Git');
  WriteLn('• Utiliser des permissions 600 pour les fichiers sensibles');
  WriteLn('• Chiffrer les mots de passe stockés');
  WriteLn('• Utiliser des variables d''environnement pour les secrets');
  WriteLn('• Effectuer des rotations régulières des clés');
  WriteLn('• Sauvegarder les configurations (sans les secrets)');
  WriteLn('• Auditer les accès aux fichiers de configuration');
  WriteLn('• Utiliser un gestionnaire de secrets en production');
  WriteLn('=== Fin du Rapport ===');
end;

end.
```

## Gestion Complète de Configuration

Voici une classe complète qui rassemble toutes les fonctionnalités :

```pascal
unit CompleteConfigManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, fpjson, jsonparser, md5, base64,
  FilePermissions, DotEnvConfig, SecretManager, ConfigWatcher;

type
  TConfigFormat = (cfIni, cfJson, cfXml, cfYaml, cfEnv);

  TCompleteConfigManager = class
  private
    FConfigPath: string;
    FFormat: TConfigFormat;
    FWatcher: TConfigWatcher;
    FSecretManager: TSecretManager;
    FDotEnv: TDotEnvConfig;
    FCache: TStringList;
    FLastModified: TDateTime;

    // Méthodes internes
    function DetectFormat: TConfigFormat;
    procedure LoadFromIni;
    procedure LoadFromJson;
    procedure LoadFromEnv;
    procedure SaveToIni;
    procedure SaveToJson;

    // Gestion du cache
    procedure UpdateCache;
    function IsCacheValid: Boolean;

    // Événements
    procedure OnConfigChanged(Sender: TObject; const AFileName: string);
  public
    constructor Create(const AConfigPath: string);
    destructor Destroy; override;

    // Chargement et sauvegarde
    procedure Load;
    procedure Save;
    procedure Reload;

    // Accès aux valeurs
    function GetString(const AKey: string; const ADefault: string = ''): string;
    function GetInt(const AKey: string; ADefault: Integer = 0): Integer;
    function GetBool(const AKey: string; ADefault: Boolean = False): Boolean;
    function GetFloat(const AKey: string; ADefault: Double = 0.0): Double;

    // Modification
    procedure SetString(const AKey, AValue: string);
    procedure SetInt(const AKey: string; AValue: Integer);
    procedure SetBool(const AKey: string; AValue: Boolean);
    procedure SetFloat(const AKey: string; AValue: Double);

    // Gestion des sections (pour INI)
    function GetSectionString(const ASection, AKey: string;
                              const ADefault: string = ''): string;
    procedure SetSectionString(const ASection, AKey, AValue: string);

    // Secrets
    function GetSecret(const AKey: string): string;
    procedure ValidateSecrets;

    // Import/Export
    procedure ImportFrom(const ASourceFile: string);
    procedure ExportTo(const ADestFile: string; AFormat: TConfigFormat);

    // Validation
    function Validate: Boolean;
    procedure ValidateSchema(const ASchemaFile: string);

    // Surveillance
    procedure EnableWatching;
    procedure DisableWatching;

    // Utilitaires
    procedure Backup;
    procedure Restore(const ABackupFile: string);
    procedure PrintConfig;
    function ToJSON: TJSONObject;
  end;

implementation

constructor TCompleteConfigManager.Create(const AConfigPath: string);
begin
  inherited Create;

  FConfigPath := AConfigPath;
  FFormat := DetectFormat;
  FCache := TStringList.Create;
  FLastModified := 0;

  // Initialiser les gestionnaires
  FSecretManager := TSecretManager.Create;

  // Charger le .env si présent
  FDotEnv := TDotEnvConfig.Create;
  FDotEnv.LoadIfExists;

  // Charger la configuration
  Load;
end;

destructor TCompleteConfigManager.Destroy;
begin
  DisableWatching;

  if Assigned(FWatcher) then
    FWatcher.Free;

  FSecretManager.Free;
  FDotEnv.Free;
  FCache.Free;

  inherited Destroy;
end;

function TCompleteConfigManager.DetectFormat: TConfigFormat;
var
  Ext: string;
begin
  Ext := LowerCase(ExtractFileExt(FConfigPath));

  if (Ext = '.ini') or (Ext = '.conf') or (Ext = '.cfg') then
    Result := cfIni
  else if Ext = '.json' then
    Result := cfJson
  else if Ext = '.xml' then
    Result := cfXml
  else if (Ext = '.yaml') or (Ext = '.yml') then
    Result := cfYaml
  else if Ext = '.env' then
    Result := cfEnv
  else
    // Essayer de détecter par le contenu
    Result := cfIni; // Par défaut
end;

procedure TCompleteConfigManager.Load;
begin
  if not FileExists(FConfigPath) then
  begin
    WriteLn('Configuration introuvable, création avec valeurs par défaut');
    Save; // Créer avec les valeurs par défaut
    Exit;
  end;

  case FFormat of
    cfIni: LoadFromIni;
    cfJson: LoadFromJson;
    cfEnv: LoadFromEnv;
  else
    raise Exception.Create('Format non supporté: ' + IntToStr(Ord(FFormat)));
  end;

  UpdateCache;
  FLastModified := Now;

  WriteLn('Configuration chargée: ', FConfigPath);
end;

procedure TCompleteConfigManager.LoadFromIni;
var
  Ini: TIniFile;
  Sections: TStringList;
  Keys: TStringList;
  i, j: Integer;
begin
  Ini := TIniFile.Create(FConfigPath);
  Sections := TStringList.Create;
  Keys := TStringList.Create;
  try
    FCache.Clear;

    Ini.ReadSections(Sections);
    for i := 0 to Sections.Count - 1 do
    begin
      Ini.ReadSection(Sections[i], Keys);
      for j := 0 to Keys.Count - 1 do
      begin
        FCache.Values[Sections[i] + '.' + Keys[j]] :=
          Ini.ReadString(Sections[i], Keys[j], '');
      end;
    end;
  finally
    Keys.Free;
    Sections.Free;
    Ini.Free;
  end;
end;

procedure TCompleteConfigManager.LoadFromJson;
var
  FileStream: TFileStream;
  Parser: TJSONParser;
  Json: TJSONObject;

  procedure ProcessObject(AObj: TJSONObject; const APrefix: string);
  var
    i: Integer;
    Key, FullKey: string;
    Data: TJSONData;
  begin
    for i := 0 to AObj.Count - 1 do
    begin
      Key := AObj.Names[i];
      Data := AObj.Items[i];

      if APrefix = '' then
        FullKey := Key
      else
        FullKey := APrefix + '.' + Key;

      if Data is TJSONObject then
        ProcessObject(Data as TJSONObject, FullKey)
      else
        FCache.Values[FullKey] := Data.AsString;
    end;
  end;

begin
  FileStream := TFileStream.Create(FConfigPath, fmOpenRead);
  try
    Parser := TJSONParser.Create(FileStream);
    try
      Json := Parser.Parse as TJSONObject;
      try
        FCache.Clear;
        ProcessObject(Json, '');
      finally
        Json.Free;
      end;
    finally
      Parser.Free;
    end;
  finally
    FileStream.Free;
  end;
end;

procedure TCompleteConfigManager.LoadFromEnv;
var
  EnvFile: TStringList;
  i: Integer;
  Line, Key, Value: string;
  EqualPos: Integer;
begin
  EnvFile := TStringList.Create;
  try
    EnvFile.LoadFromFile(FConfigPath);
    FCache.Clear;

    for i := 0 to EnvFile.Count - 1 do
    begin
      Line := Trim(EnvFile[i]);

      // Ignorer les commentaires et lignes vides
      if (Line = '') or (Line[1] = '#') then
        Continue;

      EqualPos := Pos('=', Line);
      if EqualPos > 0 then
      begin
        Key := Trim(Copy(Line, 1, EqualPos - 1));
        Value := Trim(Copy(Line, EqualPos + 1, Length(Line)));

        // Enlever les guillemets si présents
        if (Length(Value) >= 2) and
           ((Value[1] = '"') and (Value[Length(Value)] = '"')) then
        begin
          Value := Copy(Value, 2, Length(Value) - 2);
        end;

        FCache.Values[Key] := Value;
      end;
    end;
  finally
    EnvFile.Free;
  end;
end;

procedure TCompleteConfigManager.Save;
begin
  case FFormat of
    cfIni: SaveToIni;
    cfJson: SaveToJson;
  else
    raise Exception.Create('Sauvegarde non supportée pour ce format');
  end;

  // Mettre à jour les permissions
  TFilePermissions.SetUserOnly(FConfigPath);

  FLastModified := Now;
end;

procedure TCompleteConfigManager.SaveToIni;
var
  Ini: TIniFile;
  i: Integer;
  Key, Section, KeyName: string;
  DotPos: Integer;
begin
  Ini := TIniFile.Create(FConfigPath);
  try
    for i := 0 to FCache.Count - 1 do
    begin
      Key := FCache.Names[i];
      DotPos := Pos('.', Key);

      if DotPos > 0 then
      begin
        Section := Copy(Key, 1, DotPos - 1);
        KeyName := Copy(Key, DotPos + 1, Length(Key));
      end
      else
      begin
        Section := 'General';
        KeyName := Key;
      end;

      Ini.WriteString(Section, KeyName, FCache.ValueFromIndex[i]);
    end;

    Ini.UpdateFile;
  finally
    Ini.Free;
  end;
end;

procedure TCompleteConfigManager.SaveToJson;
var
  Json: TJSONObject;
  i: Integer;
  Key, Value: string;
  Parts: TStringArray;
  Current: TJSONObject;
  j: Integer;
begin
  Json := TJSONObject.Create;
  try
    for i := 0 to FCache.Count - 1 do
    begin
      Key := FCache.Names[i];
      Value := FCache.ValueFromIndex[i];

      Parts := Key.Split('.');
      Current := Json;

      // Naviguer/créer la structure
      for j := 0 to High(Parts) - 1 do
      begin
        if Current.Find(Parts[j]) = nil then
          Current.Add(Parts[j], TJSONObject.Create);
        Current := Current.Find(Parts[j]) as TJSONObject;
      end;

      // Ajouter la valeur finale
      Current.Add(Parts[High(Parts)], Value);
    end;

    // Sauvegarder
    with TFileStream.Create(FConfigPath, fmCreate) do
    try
      WriteBuffer(Json.FormatJSON[1], Length(Json.FormatJSON));
    finally
      Free;
    end;
  finally
    Json.Free;
  end;
end;

procedure TCompleteConfigManager.UpdateCache;
begin
  // Le cache est mis à jour lors du chargement
  // Cette méthode peut être étendue pour d'autres besoins
end;

function TCompleteConfigManager.IsCacheValid: Boolean;
begin
  // Vérifier si le fichier a été modifié depuis le dernier chargement
  if FileExists(FConfigPath) then
    Result := FileAge(FConfigPath) <= FLastModified
  else
    Result := False;
end;

procedure TCompleteConfigManager.Reload;
begin
  WriteLn('Rechargement de la configuration...');
  Load;
end;

function TCompleteConfigManager.GetString(const AKey: string;
                                         const ADefault: string): string;
begin
  // D'abord chercher dans le cache
  Result := FCache.Values[AKey];

  // Puis dans les variables d'environnement
  if Result = '' then
    Result := FDotEnv.GetValue(AKey, ADefault);

  if Result = '' then
    Result := ADefault;
end;

function TCompleteConfigManager.GetInt(const AKey: string;
                                       ADefault: Integer): Integer;
var
  StrValue: string;
begin
  StrValue := GetString(AKey, '');
  if not TryStrToInt(StrValue, Result) then
    Result := ADefault;
end;

function TCompleteConfigManager.GetBool(const AKey: string;
                                        ADefault: Boolean): Boolean;
var
  StrValue: string;
begin
  StrValue := LowerCase(GetString(AKey, ''));
  if StrValue = '' then
    Result := ADefault
  else
    Result := (StrValue = 'true') or (StrValue = '1') or
              (StrValue = 'yes') or (StrValue = 'on');
end;

function TCompleteConfigManager.GetFloat(const AKey: string;
                                         ADefault: Double): Double;
var
  StrValue: string;
begin
  StrValue := GetString(AKey, '');
  if not TryStrToFloat(StrValue, Result) then
    Result := ADefault;
end;

procedure TCompleteConfigManager.SetString(const AKey, AValue: string);
begin
  FCache.Values[AKey] := AValue;
end;

procedure TCompleteConfigManager.SetInt(const AKey: string; AValue: Integer);
begin
  SetString(AKey, IntToStr(AValue));
end;

procedure TCompleteConfigManager.SetBool(const AKey: string; AValue: Boolean);
begin
  if AValue then
    SetString(AKey, 'true')
  else
    SetString(AKey, 'false');
end;

procedure TCompleteConfigManager.SetFloat(const AKey: string; AValue: Double);
begin
  SetString(AKey, FloatToStr(AValue));
end;

function TCompleteConfigManager.GetSectionString(const ASection, AKey: string;
                                                 const ADefault: string): string;
begin
  Result := GetString(ASection + '.' + AKey, ADefault);
end;

procedure TCompleteConfigManager.SetSectionString(const ASection, AKey, AValue: string);
begin
  SetString(ASection + '.' + AKey, AValue);
end;

function TCompleteConfigManager.GetSecret(const AKey: string): string;
begin
  Result := FSecretManager.GetSecret(AKey);
end;

procedure TCompleteConfigManager.ValidateSecrets;
begin
  FSecretManager.CheckRequiredSecrets;
end;

procedure TCompleteConfigManager.ImportFrom(const ASourceFile: string);
var
  TempManager: TCompleteConfigManager;
  i: Integer;
begin
  TempManager := TCompleteConfigManager.Create(ASourceFile);
  try
    // Copier toutes les valeurs
    for i := 0 to TempManager.FCache.Count - 1 do
    begin
      FCache.Values[TempManager.FCache.Names[i]] :=
        TempManager.FCache.ValueFromIndex[i];
    end;

    Save;
    WriteLn('Configuration importée depuis: ', ASourceFile);
  finally
    TempManager.Free;
  end;
end;

procedure TCompleteConfigManager.ExportTo(const ADestFile: string;
                                          AFormat: TConfigFormat);
var
  OldPath: string;
  OldFormat: TConfigFormat;
begin
  // Sauvegarder temporairement l'état actuel
  OldPath := FConfigPath;
  OldFormat := FFormat;

  try
    FConfigPath := ADestFile;
    FFormat := AFormat;
    Save;
    WriteLn('Configuration exportée vers: ', ADestFile);
  finally
    // Restaurer l'état
    FConfigPath := OldPath;
    FFormat := OldFormat;
  end;
end;

function TCompleteConfigManager.Validate: Boolean;
var
  Errors: TStringList;
begin
  Result := True;
  Errors := TStringList.Create;
  try
    // Validation de base
    if GetString('Application.Name') = '' then
    begin
      Errors.Add('Application.Name est requis');
      Result := False;
    end;

    if GetString('Application.Version') = '' then
    begin
      Errors.Add('Application.Version est requis');
      Result := False;
    end;

    // Validation des types
    if GetInt('Database.Port', -1) = -1 then
    begin
      Errors.Add('Database.Port doit être un nombre valide');
      Result := False;
    end;

    if Errors.Count > 0 then
    begin
      WriteLn('Erreurs de validation:');
      WriteLn(Errors.Text);
    end;
  finally
    Errors.Free;
  end;
end;

procedure TCompleteConfigManager.ValidateSchema(const ASchemaFile: string);
begin
  // Implémenter la validation contre un schéma JSON Schema
  WriteLn('Validation contre le schéma: ', ASchemaFile);
  // À implémenter selon le format
end;

procedure TCompleteConfigManager.EnableWatching;
begin
  if Assigned(FWatcher) then
    Exit;

  FWatcher := TConfigWatcher.Create(ExtractFilePath(FConfigPath));
  FWatcher.OnChange := @OnConfigChanged;
  FWatcher.StartWatching;

  WriteLn('Surveillance activée pour: ', FConfigPath);
end;

procedure TCompleteConfigManager.DisableWatching;
begin
  if not Assigned(FWatcher) then
    Exit;

  FWatcher.StopWatching;
  FreeAndNil(FWatcher);

  WriteLn('Surveillance désactivée');
end;

procedure TCompleteConfigManager.OnConfigChanged(Sender: TObject;
                                                 const AFileName: string);
begin
  WriteLn('Configuration modifiée, rechargement...');
  Reload;
end;

procedure TCompleteConfigManager.Backup;
var
  BackupDir, BackupFile: string;
begin
  BackupDir := ExtractFilePath(FConfigPath) + 'backups/';
  if not DirectoryExists(BackupDir) then
    ForceDirectories(BackupDir);

  BackupFile := BackupDir +
                ExtractFileName(FConfigPath) + '.' +
                FormatDateTime('yyyymmdd_hhnnss', Now);

  CopyFile(FConfigPath, BackupFile);
  WriteLn('Sauvegarde créée: ', BackupFile);
end;

procedure TCompleteConfigManager.Restore(const ABackupFile: string);
begin
  if not FileExists(ABackupFile) then
    raise Exception.Create('Fichier de sauvegarde introuvable: ' + ABackupFile);

  CopyFile(ABackupFile, FConfigPath);
  Reload;

  WriteLn('Configuration restaurée depuis: ', ABackupFile);
end;

procedure TCompleteConfigManager.PrintConfig;
var
  i: Integer;
begin
  WriteLn('=== Configuration Actuelle ===');
  for i := 0 to FCache.Count - 1 do
  begin
    // Masquer les secrets
    if Pos('PASSWORD', UpperCase(FCache.Names[i])) > 0 then
      WriteLn(FCache.Names[i], '=****')
    else if Pos('SECRET', UpperCase(FCache.Names[i])) > 0 then
      WriteLn(FCache.Names[i], '=****')
    else if Pos('KEY', UpperCase(FCache.Names[i])) > 0 then
      WriteLn(FCache.Names[i], '=****')
    else
      WriteLn(FCache.Names[i], '=', FCache.ValueFromIndex[i]);
  end;
  WriteLn('==============================');
end;

function TCompleteConfigManager.ToJSON: TJSONObject;
var
  i: Integer;
  Key: string;
  Parts: TStringArray;
  Current: TJSONObject;
  j: Integer;
begin
  Result := TJSONObject.Create;

  for i := 0 to FCache.Count - 1 do
  begin
    Key := FCache.Names[i];
    Parts := Key.Split('.');
    Current := Result;

    for j := 0 to High(Parts) - 1 do
    begin
      if Current.Find(Parts[j]) = nil then
        Current.Add(Parts[j], TJSONObject.Create);
      Current := Current.Find(Parts[j]) as TJSONObject;
    end;

    Current.Add(Parts[High(Parts)], FCache.ValueFromIndex[i]);
  end;
end;

end.
```

## Exemple d'Utilisation Complète

```pascal
program CompleteConfigExample;

uses
  SysUtils, CompleteConfigManager;

var
  Config: TCompleteConfigManager;
  ConfigPath: string;
begin
  // Déterminer le chemin de configuration
  ConfigPath := GetEnvironmentVariable('XDG_CONFIG_HOME');
  if ConfigPath = '' then
    ConfigPath := GetEnvironmentVariable('HOME') + '/.config';
  ConfigPath := ConfigPath + '/myapp/config.ini';

  Config := TCompleteConfigManager.Create(ConfigPath);
  try
    // Validation
    if not Config.Validate then
    begin
      WriteLn('Configuration invalide, correction...');
      Config.SetString('Application.Name', 'MonApplication');
      Config.SetString('Application.Version', '1.0.0');
      Config.Save;
    end;

    // Activer la surveillance
    Config.EnableWatching;

    // Lire des valeurs
    WriteLn('Application: ', Config.GetString('Application.Name'));
    WriteLn('Debug: ', Config.GetBool('Application.Debug', False));
    WriteLn('Port DB: ', Config.GetInt('Database.Port', 5432));

    // Secrets
    try
      Config.ValidateSecrets;
      WriteLn('API Key: ', Copy(Config.GetSecret('API_KEY'), 1, 8), '...');
    except
      on E: Exception do
        WriteLn('Erreur secrets: ', E.Message);
    end;

    // Sauvegarde
    Config.Backup;

    // Export vers JSON
    Config.ExportTo(ChangeFileExt(ConfigPath, '.json'), cfJson);

    // Afficher la configuration (masque les secrets)
    Config.PrintConfig;

    WriteLn('');
    WriteLn('Appuyez sur Entrée pour terminer...');
    ReadLn;
  finally
    Config.Free;
  end;
end.
```

## Conseils Finaux et Bonnes Pratiques

### 1. Organisation des Fichiers

- **Séparez** les configurations par environnement
- **Utilisez** des fichiers `.d/` pour les configurations modulaires
- **Ne mélangez pas** secrets et configuration normale
- **Versionnez** les templates, pas les fichiers finaux

### 2. Sécurité

- **Ne jamais** stocker de mots de passe en clair
- **Toujours** utiliser des permissions restrictives (600 ou 640)
- **Chiffrer** les données sensibles
- **Auditer** les accès aux fichiers de configuration
- **Utiliser** un coffre-fort de secrets en production (HashiCorp Vault, etc.)

### 3. Maintenabilité

- **Documentez** chaque paramètre de configuration
- **Validez** les valeurs au chargement
- **Prévoyez** des valeurs par défaut sensées
- **Implémentez** des migrations pour les changements de format
- **Testez** avec différentes configurations

### 4. Performance

- **Cachez** les valeurs fréquemment utilisées
- **Surveillez** les changements avec inotify plutôt que polling
- **Chargez** la configuration une fois au démarrage
- **Minimisez** les accès disque

### 5. Portabilité

- **Respectez** les standards XDG Base Directory
- **Utilisez** des chemins relatifs quand possible
- **Évitez** les dépendances système spécifiques
- **Testez** sur différentes distributions Linux

## Fichier .gitignore Recommandé

```gitignore
# Fichiers de configuration avec secrets
.env
.env.local
*.env
config.local.ini
config.production.ini

# Secrets et clés
*.key
*.pem
*.crt
.master.key
secrets/

# Sauvegardes
backups/
*.backup
*.bak

# Fichiers temporaires
*.tmp
*.swp
*~

# Logs
*.log
logs/

# Fichiers générés
config.generated.ini
```

## Script d'Installation

```bash
#!/bin/bash
# install-config.sh - Installation de la configuration

set -e  # Arrêt sur erreur

APP_NAME="myapp"
CONFIG_DIR="$HOME/.config/$APP_NAME"

echo "=== Installation de la Configuration ==="

# 1. Créer la structure
echo "• Création des répertoires..."
mkdir -p "$CONFIG_DIR"/{config.d,environments,backups,secrets}

# 2. Définir les permissions
echo "• Configuration des permissions..."
chmod 700 "$CONFIG_DIR"
chmod 700 "$CONFIG_DIR/secrets"

# 3. Copier les templates
echo "• Installation des templates..."
cp templates/config.template.ini "$CONFIG_DIR/"
cp templates/.env.example "$CONFIG_DIR/.env.example"

# 4. Générer la configuration initiale
echo "• Génération de la configuration..."
if [ ! -f "$CONFIG_DIR/config.ini" ]; then
    cp "$CONFIG_DIR/config.template.ini" "$CONFIG_DIR/config.ini"
    echo "  Configuration créée: $CONFIG_DIR/config.ini"
else
    echo "  Configuration existante conservée"
fi

# 5. Créer le fichier .env si absent
if [ ! -f "$CONFIG_DIR/.env" ]; then
    echo "• Création du fichier .env..."
    cp "$CONFIG_DIR/.env.example" "$CONFIG_DIR/.env"
    chmod 600 "$CONFIG_DIR/.env"
    echo "  ⚠️  Éditez $CONFIG_DIR/.env pour configurer les secrets"
fi

# 6. Vérification
echo ""
echo "=== Vérification ==="
if [ -f "$CONFIG_DIR/config.ini" ] && [ -f "$CONFIG_DIR/.env" ]; then
    echo "✅ Installation réussie"
    echo ""
    echo "Prochaines étapes:"
    echo "1. Éditez $CONFIG_DIR/.env pour les secrets"
    echo "2. Modifiez $CONFIG_DIR/config.ini selon vos besoins"
    echo "3. Lancez l'application"
else
    echo "❌ Problème lors de l'installation"
    exit 1
fi
```

## Conclusion

La gestion de configuration via fichiers texte sous Linux offre une approche transparente, portable et facilement versionnable. Les concepts clés à retenir :

1. **Standardisation** : Suivez les conventions Linux (XDG, permissions Unix)
2. **Sécurité** : Ne jamais compromettre les secrets, utiliser des permissions strictes
3. **Flexibilité** : Supportez plusieurs formats et environnements
4. **Robustesse** : Validez, sauvegardez, et prévoyez les migrations
5. **Simplicité** : Les fichiers texte restent lisibles et éditables manuellement

Avec FreePascal/Lazarus, vous disposez de tous les outils nécessaires pour implémenter une gestion de configuration professionnelle, sécurisée et maintenable sur Linux/Ubuntu. Le code présenté peut être adapté selon vos besoins spécifiques tout en conservant les principes fondamentaux de sécurité et de bonnes pratiques.

⏭️ [Intégration desktop (GNOME/KDE/XFCE)](/07-specificites-linux-ubuntu/05-integration-desktop-gnome-kde-xfce.md)
