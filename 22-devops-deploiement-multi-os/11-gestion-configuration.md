🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 22.11 Gestion de configuration

## Introduction à la gestion de configuration

### Qu'est-ce que la configuration d'une application ?

La **configuration** regroupe tous les paramètres qui peuvent varier selon l'environnement d'exécution de votre application, sans nécessiter de recompilation du code.

**L'analogie du déménagement** :

Imaginez que vous déménagez dans une nouvelle maison. Vous emportez vos meubles (le code de votre application), mais vous devez adapter certains réglages :
- L'adresse de votre nouvelle maison (URL du serveur)
- Le code de la porte d'entrée (mot de passe de base de données)
- Le numéro de téléphone local (port d'écoute)
- Les horaires d'ouverture des commerces (timeouts)

Vous ne voulez pas acheter de nouveaux meubles à chaque déménagement, vous voulez juste **ajuster les paramètres**. C'est exactement le principe de la gestion de configuration !

### Pourquoi séparer la configuration du code ?

**Sans gestion de configuration** :
```pascal
// ❌ MAUVAIS : Configuration dans le code
const
  DATABASE_HOST = 'localhost';
  DATABASE_PORT = 5432;
  DATABASE_USER = 'admin';
  DATABASE_PASSWORD = 'motdepasse123'; // Dangereux !
  API_KEY = 'sk_live_abc123xyz'; // Exposé dans le code source !
```

**Problèmes** :
- 🔴 Secrets exposés dans le code source
- 🔴 Recompilation nécessaire pour changer un paramètre
- 🔴 Même binaire ne peut pas tourner dans différents environnements
- 🔴 Risque de commit accidentel de secrets dans Git

**Avec gestion de configuration** :
```pascal
// ✅ BON : Configuration externalisée
var
  Config: TAppConfig;
begin
  Config := TAppConfig.Create;
  DatabaseHost := Config.GetString('database.host');
  DatabasePort := Config.GetInteger('database.port');
  DatabaseUser := Config.GetString('database.user');
  DatabasePassword := Config.GetString('database.password');
end;
```

**Avantages** :
- ✅ Secrets hors du code source
- ✅ Changements sans recompilation
- ✅ Un seul binaire pour tous les environnements
- ✅ Configuration différente par environnement (dev, test, prod)

## Types de configurations

### 1. Configuration locale (fichiers)

**Formats courants** :
- **INI** : Simple et lisible
- **JSON** : Structuré et populaire
- **YAML** : Lisible avec hiérarchie
- **XML** : Verbeux mais standard

### 2. Variables d'environnement

Configuration au niveau du système d'exploitation.

**Avantages** :
- Standard sur tous les OS
- Idéal pour les conteneurs Docker
- Facile à modifier sans toucher aux fichiers

### 3. Configuration centralisée

Stockée dans un service externe (Redis, Consul, etcd).

**Avantages** :
- Partagée entre plusieurs instances
- Mise à jour en temps réel
- Historique des changements

### 4. Secrets managers

Services dédiés à la gestion sécurisée des secrets (AWS Secrets Manager, HashiCorp Vault, Azure Key Vault).

## Hiérarchie des configurations

**Principe de priorité** (du plus prioritaire au moins prioritaire) :

```
1. Arguments en ligne de commande    (--database-host=localhost)
2. Variables d'environnement         (DB_HOST=localhost)
3. Fichier de configuration local    (config.ini)
4. Configuration centralisée         (Redis, Consul)
5. Valeurs par défaut dans le code   (const DEFAULT_PORT = 8080)
```

**Exemple** :
Si vous définissez `DB_HOST=localhost` dans config.ini ET `DB_HOST=192.168.1.100` en variable d'environnement, c'est la variable d'environnement qui gagne.

## Implémentation en FreePascal

### Architecture d'un système de configuration

```pascal
unit Configuration;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, IniFiles, fpjson, jsonparser;

type
  // Type de source de configuration
  TConfigSource = (csDefault, csFile, csEnvironment, csCommandLine, csCentralized);

  // Valeur de configuration avec métadonnées
  TConfigValue = record
    Value: string;
    Source: TConfigSource;
    IsSecret: Boolean;
  end;

  TConfigMap = specialize TFPGMap<string, TConfigValue>;

  // Gestionnaire de configuration principal
  TAppConfig = class
  private
    FConfig: TConfigMap;
    FConfigFile: string;
    FEnvironmentPrefix: string;

    procedure LoadDefaults;
    procedure LoadFromFile;
    procedure LoadFromEnvironment;
    procedure LoadFromCommandLine;

    function GetConfigValue(const Key: string): TConfigValue;
  public
    constructor Create(const ConfigFile: string = 'config.ini');
    destructor Destroy; override;

    // Méthodes de lecture
    function GetString(const Key: string; const DefaultValue: string = ''): string;
    function GetInteger(const Key: string; const DefaultValue: Integer = 0): Integer;
    function GetBoolean(const Key: string; const DefaultValue: Boolean = False): Boolean;
    function GetFloat(const Key: string; const DefaultValue: Double = 0.0): Double;

    // Méthodes d'écriture (sauvegarde dans le fichier)
    procedure SetString(const Key, Value: string);
    procedure SetInteger(const Key: string; Value: Integer);
    procedure SetBoolean(const Key: string; Value: Boolean);

    // Gestion des secrets
    function GetSecret(const Key: string): string;

    // Utilitaires
    function HasKey(const Key: string): Boolean;
    function GetSource(const Key: string): TConfigSource;
    procedure Reload;
    procedure SaveToFile;
    function ToJSON: string;

    property EnvironmentPrefix: string read FEnvironmentPrefix write FEnvironmentPrefix;
  end;

implementation

{ TAppConfig }

constructor TAppConfig.Create(const ConfigFile: string);  
begin
  FConfig := TConfigMap.Create;
  FConfigFile := ConfigFile;
  FEnvironmentPrefix := 'APP_'; // Préfixe pour les variables d'environnement

  // Charger dans l'ordre de priorité (inverse)
  LoadDefaults;
  LoadFromFile;
  LoadFromEnvironment;
  LoadFromCommandLine;
end;

destructor TAppConfig.Destroy;  
begin
  FConfig.Free;
  inherited;
end;

procedure TAppConfig.LoadDefaults;  
var
  Val: TConfigValue;
begin
  // Définir les valeurs par défaut
  Val.Source := csDefault;
  Val.IsSecret := False;

  Val.Value := 'localhost';
  FConfig.Add('database.host', Val);

  Val.Value := '5432';
  FConfig.Add('database.port', Val);

  Val.Value := '8080';
  FConfig.Add('server.port', Val);

  Val.Value := 'info';
  FConfig.Add('logging.level', Val);
end;

procedure TAppConfig.LoadFromFile;  
var
  IniFile: TIniFile;
  Sections: TStringList;
  Keys: TStringList;
  i, j: Integer;
  Section, Key, FullKey: string;
  Val: TConfigValue;
begin
  if not FileExists(FConfigFile) then
    Exit;

  IniFile := TIniFile.Create(FConfigFile);
  Sections := TStringList.Create;
  Keys := TStringList.Create;
  try
    IniFile.ReadSections(Sections);

    for i := 0 to Sections.Count - 1 do
    begin
      Section := Sections[i];
      Keys.Clear;
      IniFile.ReadSection(Section, Keys);

      for j := 0 to Keys.Count - 1 do
      begin
        Key := Keys[j];
        FullKey := Section + '.' + Key;

        Val.Value := IniFile.ReadString(Section, Key, '');
        Val.Source := csFile;
        Val.IsSecret := (Pos('password', LowerCase(Key)) > 0) or
                       (Pos('secret', LowerCase(Key)) > 0) or
                       (Pos('key', LowerCase(Key)) > 0);

        // Remplacer ou ajouter
        if FConfig.IndexOf(FullKey) >= 0 then
          FConfig.KeyData[FullKey] := Val
        else
          FConfig.Add(FullKey, Val);
      end;
    end;
  finally
    Keys.Free;
    Sections.Free;
    IniFile.Free;
  end;
end;

procedure TAppConfig.LoadFromEnvironment;  
var
  i: Integer;
  EnvVar, Key: string;
  Val: TConfigValue;
begin
  // Parcourir les variables d'environnement du système
  // Note: GetEnvironmentVariableCount et GetEnvironmentString sont spécifiques à l'OS

  {$IFDEF WINDOWS}
  // Windows : utiliser GetEnvironmentStrings
  // Implémentation simplifiée
  {$ENDIF}

  {$IFDEF UNIX}
  // Unix : variables accessibles via environ
  // Implémentation simplifiée
  {$ENDIF}

  // Exemple de lecture directe
  // Convertir APP_DATABASE_HOST en database.host
  EnvVar := GetEnvironmentVariable('APP_DATABASE_HOST');
  if EnvVar <> '' then
  begin
    Val.Value := EnvVar;
    Val.Source := csEnvironment;
    Val.IsSecret := False;
    Key := 'database.host';

    if FConfig.IndexOf(Key) >= 0 then
      FConfig.KeyData[Key] := Val
    else
      FConfig.Add(Key, Val);
  end;

  // Même chose pour d'autres variables courantes
  EnvVar := GetEnvironmentVariable('APP_DATABASE_PORT');
  if EnvVar <> '' then
  begin
    Val.Value := EnvVar;
    Val.Source := csEnvironment;
    Key := 'database.port';
    if FConfig.IndexOf(Key) >= 0 then
      FConfig.KeyData[Key] := Val
    else
      FConfig.Add(Key, Val);
  end;
end;

procedure TAppConfig.LoadFromCommandLine;  
var
  i: Integer;
  Param, Key, Value: string;
  Val: TConfigValue;
  EqualPos: Integer;
begin
  // Parser les arguments de ligne de commande
  // Format: --key=value ou --key value

  for i := 1 to ParamCount do
  begin
    Param := ParamStr(i);

    // Format --key=value
    if (Length(Param) > 2) and (Copy(Param, 1, 2) = '--') then
    begin
      EqualPos := Pos('=', Param);
      if EqualPos > 0 then
      begin
        Key := Copy(Param, 3, EqualPos - 3);
        Value := Copy(Param, EqualPos + 1, Length(Param));

        // Convertir les tirets en points: database-host -> database.host
        Key := StringReplace(Key, '-', '.', [rfReplaceAll]);

        Val.Value := Value;
        Val.Source := csCommandLine;
        Val.IsSecret := False;

        if FConfig.IndexOf(Key) >= 0 then
          FConfig.KeyData[Key] := Val
        else
          FConfig.Add(Key, Val);
      end;
    end;
  end;
end;

function TAppConfig.GetConfigValue(const Key: string): TConfigValue;  
var
  Index: Integer;
begin
  Index := FConfig.IndexOf(Key);
  if Index >= 0 then
    Result := FConfig.Data[Index]
  else
  begin
    Result.Value := '';
    Result.Source := csDefault;
    Result.IsSecret := False;
  end;
end;

function TAppConfig.GetString(const Key: string; const DefaultValue: string): string;  
var
  Val: TConfigValue;
begin
  Val := GetConfigValue(Key);
  if Val.Value <> '' then
    Result := Val.Value
  else
    Result := DefaultValue;
end;

function TAppConfig.GetInteger(const Key: string; const DefaultValue: Integer): Integer;  
var
  Val: TConfigValue;
begin
  Val := GetConfigValue(Key);
  if Val.Value <> '' then
    Result := StrToIntDef(Val.Value, DefaultValue)
  else
    Result := DefaultValue;
end;

function TAppConfig.GetBoolean(const Key: string; const DefaultValue: Boolean): Boolean;  
var
  Val: TConfigValue;
  StrVal: string;
begin
  Val := GetConfigValue(Key);
  if Val.Value <> '' then
  begin
    StrVal := LowerCase(Val.Value);
    Result := (StrVal = 'true') or (StrVal = '1') or (StrVal = 'yes') or (StrVal = 'on');
  end
  else
    Result := DefaultValue;
end;

function TAppConfig.GetFloat(const Key: string; const DefaultValue: Double): Double;  
var
  Val: TConfigValue;
  fs: TFormatSettings;
begin
  Val := GetConfigValue(Key);
  if Val.Value <> '' then
  begin
    fs := DefaultFormatSettings;
    fs.DecimalSeparator := '.';
    Result := StrToFloatDef(Val.Value, DefaultValue, fs);
  end
  else
    Result := DefaultValue;
end;

procedure TAppConfig.SetString(const Key, Value: string);  
var
  Val: TConfigValue;
begin
  Val.Value := Value;
  Val.Source := csFile; // Sera sauvegardé dans le fichier
  Val.IsSecret := (Pos('password', LowerCase(Key)) > 0) or
                 (Pos('secret', LowerCase(Key)) > 0);

  if FConfig.IndexOf(Key) >= 0 then
    FConfig.KeyData[Key] := Val
  else
    FConfig.Add(Key, Val);
end;

procedure TAppConfig.SetInteger(const Key: string; Value: Integer);  
begin
  SetString(Key, IntToStr(Value));
end;

procedure TAppConfig.SetBoolean(const Key: string; Value: Boolean);  
begin
  if Value then
    SetString(Key, 'true')
  else
    SetString(Key, 'false');
end;

function TAppConfig.GetSecret(const Key: string): string;  
var
  Val: TConfigValue;
begin
  Val := GetConfigValue(Key);
  Val.IsSecret := True; // Marquer comme secret
  Result := Val.Value;
end;

function TAppConfig.HasKey(const Key: string): Boolean;  
begin
  Result := FConfig.IndexOf(Key) >= 0;
end;

function TAppConfig.GetSource(const Key: string): TConfigSource;  
var
  Val: TConfigValue;
begin
  Val := GetConfigValue(Key);
  Result := Val.Source;
end;

procedure TAppConfig.Reload;  
begin
  FConfig.Clear;
  LoadDefaults;
  LoadFromFile;
  LoadFromEnvironment;
  LoadFromCommandLine;
end;

procedure TAppConfig.SaveToFile;  
var
  IniFile: TIniFile;
  i: Integer;
  Key, Section, SubKey: string;
  Val: TConfigValue;
  DotPos: Integer;
begin
  IniFile := TIniFile.Create(FConfigFile);
  try
    // Sauvegarder uniquement les valeurs de source csFile
    for i := 0 to FConfig.Count - 1 do
    begin
      Val := FConfig.Data[i];

      if Val.Source = csFile then
      begin
        Key := FConfig.Keys[i];
        DotPos := Pos('.', Key);

        if DotPos > 0 then
        begin
          Section := Copy(Key, 1, DotPos - 1);
          SubKey := Copy(Key, DotPos + 1, Length(Key));

          // Ne pas sauvegarder les secrets en clair
          if not Val.IsSecret then
            IniFile.WriteString(Section, SubKey, Val.Value);
        end;
      end;
    end;
  finally
    IniFile.Free;
  end;
end;

function TAppConfig.ToJSON: string;  
var
  JSON: TJSONObject;
  i: Integer;
  Key: string;
  Val: TConfigValue;
begin
  JSON := TJSONObject.Create;
  try
    for i := 0 to FConfig.Count - 1 do
    begin
      Key := FConfig.Keys[i];
      Val := FConfig.Data[i];

      // Masquer les secrets
      if Val.IsSecret then
        JSON.Add(Key, '***HIDDEN***')
      else
        JSON.Add(Key, Val.Value);
    end;

    Result := JSON.FormatJSON;
  finally
    JSON.Free;
  end;
end;

end.
```

### Utilisation dans votre application

```pascal
program MyApp;

uses
  SysUtils, Configuration;

var
  Config: TAppConfig;
  DatabaseHost: string;
  DatabasePort: Integer;
  DebugMode: Boolean;

begin
  // Initialiser la configuration
  Config := TAppConfig.Create('config.ini');
  try
    // Lire les valeurs
    DatabaseHost := Config.GetString('database.host', 'localhost');
    DatabasePort := Config.GetInteger('database.port', 5432);
    DebugMode := Config.GetBoolean('app.debug', False);

    WriteLn('Configuration chargée:');
    WriteLn('  Database Host: ', DatabaseHost, ' (source: ',
            Integer(Config.GetSource('database.host')), ')');
    WriteLn('  Database Port: ', DatabasePort);
    WriteLn('  Debug Mode: ', DebugMode);

    // Afficher toute la configuration en JSON
    WriteLn;
    WriteLn('Configuration complète:');
    WriteLn(Config.ToJSON);

    // Modifier une valeur et sauvegarder
    Config.SetInteger('server.port', 9000);
    Config.SaveToFile;

  finally
    Config.Free;
  end;

  ReadLn;
end.
```

## Fichier de configuration INI

### Structure de base

```ini
; config.ini - Configuration de l'application

[app]
name = MyFreePascalApp  
version = 1.0.0  
debug = false  
environment = production

[server]
host = 0.0.0.0  
port = 8080  
max_connections = 1000  
timeout = 30

[database]
host = localhost  
port = 5432  
name = myappdb  
user = appuser
; Ne JAMAIS mettre le mot de passe ici ! Utiliser les variables d'environnement
; password =

[redis]
host = localhost  
port = 6379  
db = 0

[logging]
level = info  
file = /var/log/myapp/app.log  
max_size_mb = 100  
max_files = 10

[features]
enable_caching = true  
enable_analytics = true  
enable_debug_endpoints = false
```

### Configuration par environnement

Utilisez des fichiers séparés :

```
config/
├── config.ini                  # Configuration de base (commune)
├── config.development.ini      # Surcharges pour développement
├── config.staging.ini          # Surcharges pour staging
└── config.production.ini       # Surcharges pour production
```

**Chargement selon l'environnement** :

```pascal
function GetConfigFileName: string;  
var
  Environment: string;
begin
  Environment := GetEnvironmentVariable('APP_ENVIRONMENT');

  if Environment = '' then
    Environment := 'development';

  Result := 'config.' + Environment + '.ini';

  // Fallback sur config.ini si le fichier spécifique n'existe pas
  if not FileExists(Result) then
    Result := 'config.ini';
end;

// Utilisation
Config := TAppConfig.Create(GetConfigFileName);
```

## Variables d'environnement

### Définir des variables d'environnement

**Windows (PowerShell)** :
```powershell
# Temporaire (session courante)
$env:APP_DATABASE_HOST = "192.168.1.100"
$env:APP_DATABASE_PORT = "5432"
$env:APP_DATABASE_PASSWORD = "secretpassword"

# Permanent (utilisateur)
[System.Environment]::SetEnvironmentVariable("APP_DATABASE_HOST", "192.168.1.100", "User")

# Permanent (système, nécessite admin)
[System.Environment]::SetEnvironmentVariable("APP_DATABASE_HOST", "192.168.1.100", "Machine")
```

**Windows (CMD)** :
```cmd
REM Temporaire  
set APP_DATABASE_HOST=192.168.1.100  
set APP_DATABASE_PORT=5432

REM Permanent via setx  
setx APP_DATABASE_HOST "192.168.1.100"
```

**Ubuntu/Linux** :
```bash
# Temporaire (session courante)
export APP_DATABASE_HOST="192.168.1.100"  
export APP_DATABASE_PORT="5432"  
export APP_DATABASE_PASSWORD="secretpassword"

# Permanent (utilisateur) - ajouter dans ~/.bashrc ou ~/.profile
echo 'export APP_DATABASE_HOST="192.168.1.100"' >> ~/.bashrc  
source ~/.bashrc

# Permanent (système) - ajouter dans /etc/environment (nécessite sudo)
echo 'APP_DATABASE_HOST="192.168.1.100"' | sudo tee -a /etc/environment
```

### Fichier .env pour le développement

Créez un fichier `.env` à la racine du projet :

```bash
# .env - NE PAS COMMIT DANS GIT !

APP_ENVIRONMENT=development  
APP_DEBUG=true

DATABASE_HOST=localhost  
DATABASE_PORT=5432  
DATABASE_NAME=myapp_dev  
DATABASE_USER=devuser  
DATABASE_PASSWORD=devpassword123

REDIS_HOST=localhost  
REDIS_PORT=6379

LOG_LEVEL=debug
```

**Ajouter à .gitignore** :
```
# .gitignore
.env
.env.local
config/*.local.ini  
secrets/
```

**Parser le fichier .env en FreePascal** :

```pascal
unit DotEnvLoader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure LoadDotEnv(const FileName: string = '.env');

implementation

uses
  {$IFDEF WINDOWS}
  Windows
  {$ENDIF}
  {$IFDEF UNIX}
  Unix
  {$ENDIF};

procedure LoadDotEnv(const FileName: string);  
var
  EnvFile: TStringList;
  i: Integer;
  Line, Key, Value: string;
  EqualPos: Integer;
begin
  if not FileExists(FileName) then
    Exit;

  EnvFile := TStringList.Create;
  try
    EnvFile.LoadFromFile(FileName);

    for i := 0 to EnvFile.Count - 1 do
    begin
      Line := Trim(EnvFile[i]);

      // Ignorer les commentaires et lignes vides
      if (Line = '') or (Copy(Line, 1, 1) = '#') then
        Continue;

      EqualPos := Pos('=', Line);
      if EqualPos > 0 then
      begin
        Key := Trim(Copy(Line, 1, EqualPos - 1));
        Value := Trim(Copy(Line, EqualPos + 1, Length(Line)));

        // Supprimer les guillemets si présents
        if (Length(Value) >= 2) and (Value[1] = '"') and (Value[Length(Value)] = '"') then
          Value := Copy(Value, 2, Length(Value) - 2);

        // Définir la variable d'environnement
        {$IFDEF WINDOWS}
        SetEnvironmentVariable(PChar(Key), PChar(Value));
        {$ENDIF}
        {$IFDEF UNIX}
        fpSetEnv(PChar(Key), PChar(Value), 1);
        {$ENDIF}
      end;
    end;
  finally
    EnvFile.Free;
  end;
end;

end.
```

**Utilisation** :
```pascal
program MyApp;

uses
  DotEnvLoader, Configuration;

begin
  // Charger les variables d'environnement depuis .env
  LoadDotEnv('.env');

  // Puis charger la configuration normalement
  Config := TAppConfig.Create('config.ini');
  // Les variables d'environnement auront la priorité
end.
```

## Configuration JSON

### Structure JSON hiérarchique

```json
{
  "app": {
    "name": "MyFreePascalApp",
    "version": "1.0.0",
    "debug": false,
    "environment": "production"
  },
  "server": {
    "host": "0.0.0.0",
    "port": 8080,
    "ssl": {
      "enabled": true,
      "certificate": "/etc/ssl/certs/app.crt",
      "key": "/etc/ssl/private/app.key"
    },
    "timeouts": {
      "connect": 5,
      "read": 30,
      "write": 30
    }
  },
  "database": {
    "primary": {
      "host": "db-primary.example.com",
      "port": 5432,
      "name": "myappdb",
      "user": "appuser",
      "pool": {
        "min": 5,
        "max": 20
      }
    },
    "replica": {
      "host": "db-replica.example.com",
      "port": 5432
    }
  },
  "logging": {
    "level": "info",
    "outputs": [
      {
        "type": "file",
        "path": "/var/log/myapp/app.log",
        "maxSizeMB": 100
      },
      {
        "type": "syslog",
        "host": "syslog.example.com",
        "port": 514
      }
    ]
  },
  "features": {
    "caching": true,
    "analytics": true,
    "rateLimit": {
      "enabled": true,
      "requestsPerMinute": 100
    }
  }
}
```

### Parser du JSON en FreePascal

```pascal
unit JSONConfiguration;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser;

type
  TJSONConfig = class
  private
    FJSON: TJSONObject;
    FFileName: string;

    function NavigateToPath(const Path: string): TJSONData;
  public
    constructor Create(const FileName: string);
    destructor Destroy; override;

    function GetString(const Path: string; const DefaultValue: string = ''): string;
    function GetInteger(const Path: string; const DefaultValue: Integer = 0): Integer;
    function GetBoolean(const Path: string; const DefaultValue: Boolean = False): Boolean;
    function GetFloat(const Path: string; const DefaultValue: Double = 0.0): Double;
    function GetArray(const Path: string): TJSONArray;
    function GetObject(const Path: string): TJSONObject;

    procedure Reload;
  end;

implementation

constructor TJSONConfig.Create(const FileName: string);  
var
  FileContent: string;
  Parser: TJSONParser;
  Stream: TFileStream;
begin
  FFileName := FileName;

  if not FileExists(FileName) then
    raise Exception.CreateFmt('Config file not found: %s', [FileName]);

  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    SetLength(FileContent, Stream.Size);
    Stream.ReadBuffer(FileContent[1], Stream.Size);
  finally
    Stream.Free;
  end;

  Parser := TJSONParser.Create(FileContent, [joUTF8]);
  try
    FJSON := Parser.Parse as TJSONObject;
  finally
    Parser.Free;
  end;
end;

destructor TJSONConfig.Destroy;  
begin
  FJSON.Free;
  inherited;
end;

function TJSONConfig.NavigateToPath(const Path: string): TJSONData;  
var
  Parts: TStringList;
  i: Integer;
  Current: TJSONData;
begin
  Parts := TStringList.Create;
  try
    Parts.Delimiter := '.';
    Parts.StrictDelimiter := True;
    Parts.DelimitedText := Path;
    Current := FJSON;

    for i := 0 to Parts.Count - 1 do
    begin
      if Current is TJSONObject then
        Current := TJSONObject(Current).Find(Parts[i])
      else
        Exit(nil);

      if Current = nil then
        Exit(nil);
    end;

    Result := Current;
  finally
    Parts.Free;
  end;
end;

function TJSONConfig.GetString(const Path: string; const DefaultValue: string): string;  
var
  Data: TJSONData;
begin
  Data := NavigateToPath(Path);
  if Assigned(Data) and (Data.JSONType = jtString) then
    Result := Data.AsString
  else
    Result := DefaultValue;
end;

function TJSONConfig.GetInteger(const Path: string; const DefaultValue: Integer): Integer;  
var
  Data: TJSONData;
begin
  Data := NavigateToPath(Path);
  if Assigned(Data) and (Data.JSONType in [jtNumber, jtString]) then
    Result := Data.AsInteger
  else
    Result := DefaultValue;
end;

function TJSONConfig.GetBoolean(const Path: string; const DefaultValue: Boolean): Boolean;  
var
  Data: TJSONData;
begin
  Data := NavigateToPath(Path);
  if Assigned(Data) and (Data.JSONType = jtBoolean) then
    Result := Data.AsBoolean
  else
    Result := DefaultValue;
end;

function TJSONConfig.GetFloat(const Path: string; const DefaultValue: Double): Double;  
var
  Data: TJSONData;
begin
  Data := NavigateToPath(Path);
  if Assigned(Data) and (Data.JSONType in [jtNumber, jtString]) then
    Result := Data.AsFloat
  else
    Result := DefaultValue;
end;

function TJSONConfig.GetArray(const Path: string): TJSONArray;  
var
  Data: TJSONData;
begin
  Data := NavigateToPath(Path);
  if Assigned(Data) and (Data.JSONType = jtArray) then
    Result := Data as TJSONArray
  else
    Result := nil;
end;

function TJSONConfig.GetObject(const Path: string): TJSONObject;  
var
  Data: TJSONData;
begin
  Data := NavigateToPath(Path);
  if Assigned(Data) and (Data.JSONType = jtObject) then
    Result := Data as TJSONObject
  else
    Result := nil;
end;

procedure TJSONConfig.Reload;  
var
  OldJSON: TJSONObject;
begin
  OldJSON := FJSON;
  try
    // Recharger depuis le fichier
    Create(FFileName);
  except
    // En cas d'erreur, restaurer l'ancienne config
    FJSON := OldJSON;
    raise;
  end;
  OldJSON.Free;
end;

end.
```

### Utilisation de la configuration JSON

```pascal
program UseJSONConfig;

uses
  SysUtils, JSONConfiguration;

var
  Config: TJSONConfig;
  ServerPort: Integer;
  SSLEnabled: Boolean;
  DBHost: string;
  LogOutputs: TJSONArray;
  i: Integer;
  Output: TJSONObject;

begin
  Config := TJSONConfig.Create('config.json');
  try
    // Lecture de valeurs simples
    ServerPort := Config.GetInteger('server.port', 8080);
    SSLEnabled := Config.GetBoolean('server.ssl.enabled', False);
    DBHost := Config.GetString('database.primary.host', 'localhost');

    WriteLn('Configuration:');
    WriteLn('  Server Port: ', ServerPort);
    WriteLn('  SSL Enabled: ', SSLEnabled);
    WriteLn('  Database Host: ', DBHost);
    WriteLn;

    // Lecture d'un tableau
    WriteLn('Logging Outputs:');
    LogOutputs := Config.GetArray('logging.outputs');
    if Assigned(LogOutputs) then
    begin
      for i := 0 to LogOutputs.Count - 1 do
      begin
        Output := LogOutputs.Objects[i];
        WriteLn('  - Type: ', Output.Get('type', ''));
        WriteLn('    Path: ', Output.Get('path', 'N/A'));
      end;
    end;

  finally
    Config.Free;
  end;

  ReadLn;
end.
```

## Gestion sécurisée des secrets

### Le problème des secrets en clair

**❌ Ne JAMAIS faire** :
```ini
[database]
password = MonMotDePasseEnClair123!
```

**❌ Ne JAMAIS faire** :
```pascal
const
  API_KEY = 'sk_live_abc123xyz456';
```

### Solutions pour gérer les secrets

#### 1. Variables d'environnement

**✅ Recommandé pour la plupart des cas** :

```bash
# Variables d'environnement (jamais dans le code ou fichiers versionnés)
export DATABASE_PASSWORD="MonMotDePasse123!"  
export API_KEY="sk_live_abc123xyz456"
```

```pascal
var
  DBPassword: string;
begin
  DBPassword := GetEnvironmentVariable('DATABASE_PASSWORD');
  if DBPassword = '' then
    raise Exception.Create('DATABASE_PASSWORD non défini');
end;
```

#### 2. Fichiers de secrets séparés

**Structure** :
```
project/
├── config/
│   ├── config.ini              # Versionné dans Git
│   └── secrets.ini             # NON versionné (dans .gitignore)
└── .gitignore
```

**secrets.ini** :
```ini
[database]
password = MonMotDePasseSecret

[api]
key = sk_live_abc123xyz  
token = jwt_secret_token
```

**.gitignore** :
```
# Ne JAMAIS versionner les secrets
secrets.ini  
config/secrets.ini
*.secret.*
.env
```

**Chargement** :
```pascal
var
  MainConfig, SecretsConfig: TAppConfig;
begin
  // Charger la config principale
  MainConfig := TAppConfig.Create('config.ini');

  // Charger les secrets séparément
  if FileExists('secrets.ini') then
  begin
    SecretsConfig := TAppConfig.Create('secrets.ini');
    try
      DBPassword := SecretsConfig.GetString('database.password');
    finally
      SecretsConfig.Free;
    end;
  end;
end;
```

#### 3. Chiffrement des secrets

**Unit de chiffrement simple** :

```pascal
unit SecretManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, base64, md5;

type
  TSecretManager = class
  private
    FMasterKey: string;
    function XOREncryptDecrypt(const Data, Key: string): string;
  public
    constructor Create(const MasterKey: string);

    function Encrypt(const PlainText: string): string;
    function Decrypt(const CipherText: string): string;

    procedure EncryptFile(const SourceFile, DestFile: string);
    procedure DecryptFile(const SourceFile, DestFile: string);
  end;

implementation

constructor TSecretManager.Create(const MasterKey: string);  
begin
  if MasterKey = '' then
    raise Exception.Create('Master key cannot be empty');

  // Utiliser un hash du master key pour plus de sécurité
  FMasterKey := MD5Print(MD5String(MasterKey));
end;

function TSecretManager.XOREncryptDecrypt(const Data, Key: string): string;  
var
  i: Integer;
  KeyLen: Integer;
begin
  SetLength(Result, Length(Data));
  KeyLen := Length(Key);

  for i := 1 to Length(Data) do
    Result[i] := Chr(Ord(Data[i]) xor Ord(Key[((i - 1) mod KeyLen) + 1]));
end;

function TSecretManager.Encrypt(const PlainText: string): string;  
var
  Encrypted: string;
begin
  Encrypted := XOREncryptDecrypt(PlainText, FMasterKey);
  Result := EncodeStringBase64(Encrypted);
end;

function TSecretManager.Decrypt(const CipherText: string): string;  
var
  Decoded: string;
begin
  Decoded := DecodeStringBase64(CipherText);
  Result := XOREncryptDecrypt(Decoded, FMasterKey);
end;

procedure TSecretManager.EncryptFile(const SourceFile, DestFile: string);  
var
  Source, Dest: TStringList;
  i: Integer;
begin
  Source := TStringList.Create;
  Dest := TStringList.Create;
  try
    Source.LoadFromFile(SourceFile);

    for i := 0 to Source.Count - 1 do
      Dest.Add(Encrypt(Source[i]));

    Dest.SaveToFile(DestFile);
  finally
    Dest.Free;
    Source.Free;
  end;
end;

procedure TSecretManager.DecryptFile(const SourceFile, DestFile: string);  
var
  Source, Dest: TStringList;
  i: Integer;
begin
  Source := TStringList.Create;
  Dest := TStringList.Create;
  try
    Source.LoadFromFile(SourceFile);

    for i := 0 to Source.Count - 1 do
      Dest.Add(Decrypt(Source[i]));

    Dest.SaveToFile(DestFile);
  finally
    Dest.Free;
    Source.Free;
  end;
end;

end.
```

**Utilisation** :

```pascal
program EncryptSecrets;

uses
  SysUtils, SecretManager;

var
  SecMgr: TSecretManager;
  Encrypted, Decrypted: string;

begin
  // La clé maître doit être fournie de manière sécurisée
  // (variable d'environnement, fichier protégé, etc.)
  SecMgr := TSecretManager.Create(GetEnvironmentVariable('MASTER_KEY'));
  try
    // Chiffrer un secret
    Encrypted := SecMgr.Encrypt('MonMotDePasseSecret123!');
    WriteLn('Encrypted: ', Encrypted);

    // Déchiffrer
    Decrypted := SecMgr.Decrypt(Encrypted);
    WriteLn('Decrypted: ', Decrypted);

    // Chiffrer un fichier entier
    SecMgr.EncryptFile('secrets.txt', 'secrets.encrypted');

    // Déchiffrer un fichier
    SecMgr.DecryptFile('secrets.encrypted', 'secrets.decrypted.txt');

  finally
    SecMgr.Free;
  end;
end.
```

**Note importante** : Cet exemple utilise un chiffrement simple XOR. Pour de vrais secrets en production, utilisez des bibliothèques de chiffrement robustes comme DCPCrypt ou OpenSSL.

#### 4. Secrets managers externes

**Windows - DPAPI (Data Protection API)** :

```pascal
unit WindowsSecrets;

{$mode objfpc}{$H+}

{$IFDEF WINDOWS}
interface

uses
  Windows, SysUtils;

function ProtectData(const PlainText: string): string;  
function UnprotectData(const CipherText: string): string;

implementation

function ProtectData(const PlainText: string): string;  
var
  DataIn, DataOut: DATA_BLOB;
  Success: Boolean;
begin
  // Convertir la chaîne en blob
  DataIn.cbData := Length(PlainText);
  DataIn.pbData := PByte(PlainText);

  // Chiffrer avec DPAPI
  Success := CryptProtectData(@DataIn, nil, nil, nil, nil,
                               CRYPTPROTECT_UI_FORBIDDEN, @DataOut);

  if Success then
  begin
    // Convertir en base64 pour stockage
    SetLength(Result, DataOut.cbData);
    Move(DataOut.pbData^, Result[1], DataOut.cbData);
    Result := EncodeStringBase64(Result);
    LocalFree(HLOCAL(DataOut.pbData));
  end
  else
    raise Exception.Create('Failed to protect data');
end;

function UnprotectData(const CipherText: string): string;  
var
  DataIn, DataOut: DATA_BLOB;
  Decoded: string;
  Success: Boolean;
begin
  // Décoder depuis base64
  Decoded := DecodeStringBase64(CipherText);

  DataIn.cbData := Length(Decoded);
  DataIn.pbData := PByte(Decoded);

  // Déchiffrer avec DPAPI
  Success := CryptUnprotectData(@DataIn, nil, nil, nil, nil,
                                 CRYPTPROTECT_UI_FORBIDDEN, @DataOut);

  if Success then
  begin
    SetLength(Result, DataOut.cbData);
    Move(DataOut.pbData^, Result[1], DataOut.cbData);
    LocalFree(HLOCAL(DataOut.pbData));
  end
  else
    raise Exception.Create('Failed to unprotect data');
end;

{$ENDIF}
end.
```

**Linux - Keyring via Secret Service API** :

```pascal
unit LinuxSecrets;

{$mode objfpc}{$H+}

{$IFDEF UNIX}
interface

uses
  Classes, SysUtils, Process;

function StoreSecret(const Key, Value: string): Boolean;  
function RetrieveSecret(const Key: string): string;

implementation

function StoreSecret(const Key, Value: string): Boolean;  
var
  AProcess: TProcess;
begin
  AProcess := TProcess.Create(nil);
  try
    AProcess.Executable := 'secret-tool';
    AProcess.Parameters.Add('store');
    AProcess.Parameters.Add('--label=' + Key);
    AProcess.Parameters.Add('application');
    AProcess.Parameters.Add('myfreepascalapp');
    AProcess.Parameters.Add('key');
    AProcess.Parameters.Add(Key);
    AProcess.Options := [poWaitOnExit, poUsePipes];

    AProcess.Execute;
    AProcess.Input.Write(Value[1], Length(Value));
    AProcess.CloseInput;

    Result := AProcess.ExitStatus = 0;
  finally
    AProcess.Free;
  end;
end;

function RetrieveSecret(const Key: string): string;  
var
  AProcess: TProcess;
  Output: TStringList;
begin
  AProcess := TProcess.Create(nil);
  Output := TStringList.Create;
  try
    AProcess.Executable := 'secret-tool';
    AProcess.Parameters.Add('lookup');
    AProcess.Parameters.Add('application');
    AProcess.Parameters.Add('myfreepascalapp');
    AProcess.Parameters.Add('key');
    AProcess.Parameters.Add(Key);
    AProcess.Options := [poWaitOnExit, poUsePipes];

    AProcess.Execute;
    Output.LoadFromStream(AProcess.Output);

    if AProcess.ExitStatus = 0 then
      Result := Trim(Output.Text)
    else
      Result := '';
  finally
    Output.Free;
    AProcess.Free;
  end;
end;

{$ENDIF}
end.
```

## Configuration centralisée avec Redis

### Architecture

```
┌─────────────┐     ┌─────────────┐     ┌─────────────┐
│  Instance   │     │  Instance   │     │  Instance   │
│  App #1     │     │  App #2     │     │  App #3     │
│             │     │             │     │             │
│  [Config]───┼─────┼──[Config]───┼─────┼──[Config]   │
└─────────────┘     └─────────────┘     └─────────────┘
       │                   │                   │
       └───────────────────┼───────────────────┘
                           ↓
                    ┌──────────────┐
                    │    Redis     │
                    │  (Config     │
                    │   Store)     │
                    └──────────────┘
                           ↑
                    ┌──────────────┐
                    │   Admin UI   │
                    │   ou CLI     │
                    └──────────────┘
```

### Implémentation avec Redis

```pascal
unit RedisConfiguration;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Redis, fpjson, jsonparser, SyncObjs;

type
  TRedisConfig = class
  private
    FRedis: TRedisClient;
    FPrefix: string;
    FCache: TStringList;
    FCacheLock: TCriticalSection;
    FCacheTTL: Integer;
    FLastRefresh: TDateTime;

    procedure RefreshCache;
    function GetFromRedis(const Key: string): string;
  public
    constructor Create(const RedisHost: string; RedisPort: Integer;
                      const Prefix: string = 'config:');
    destructor Destroy; override;

    function GetString(const Key, DefaultValue: string): string;
    function GetInteger(const Key: string; DefaultValue: Integer): Integer;
    function GetBoolean(const Key: string; DefaultValue: Boolean): Boolean;

    procedure SetString(const Key, Value: string);
    procedure SetInteger(const Key: string; Value: Integer);
    procedure SetBoolean(const Key: string; Value: Boolean);

    procedure Subscribe(const Pattern: string);
    procedure Invalidate;

    property CacheTTL: Integer read FCacheTTL write FCacheTTL;
  end;

implementation

uses
  DateUtils;

constructor TRedisConfig.Create(const RedisHost: string; RedisPort: Integer;
                                const Prefix: string);
begin
  FRedis := TRedisClient.Create(RedisHost, RedisPort);
  FPrefix := Prefix;
  FCache := TStringList.Create;
  FCache.NameValueSeparator := '=';
  FCacheLock := TCriticalSection.Create;
  FCacheTTL := 60; // 60 secondes par défaut
  RefreshCache;
end;

destructor TRedisConfig.Destroy;  
begin
  FCacheLock.Free;
  FCache.Free;
  FRedis.Free;
  inherited;
end;

procedure TRedisConfig.RefreshCache;  
var
  Keys: TStringList;
  i: Integer;
  Key, Value: string;
begin
  // Vérifier si le cache est encore valide
  if (SecondsBetween(Now, FLastRefresh) < FCacheTTL) and (FCache.Count > 0) then
    Exit;

  FCacheLock.Enter;
  try
    FCache.Clear;

    // Charger toutes les clés de configuration depuis Redis
    Keys := FRedis.Keys(FPrefix + '*');
    try
      for i := 0 to Keys.Count - 1 do
      begin
        Key := StringReplace(Keys[i], FPrefix, '', []);
        Value := FRedis.Get(Keys[i]);
        FCache.Values[Key] := Value;
      end;
    finally
      Keys.Free;
    end;

    FLastRefresh := Now;
  finally
    FCacheLock.Leave;
  end;
end;

function TRedisConfig.GetFromRedis(const Key: string): string;  
begin
  Result := FRedis.Get(FPrefix + Key);
end;

function TRedisConfig.GetString(const Key, DefaultValue: string): string;  
begin
  RefreshCache;

  FCacheLock.Enter;
  try
    Result := FCache.Values[Key];
    if Result = '' then
      Result := DefaultValue;
  finally
    FCacheLock.Leave;
  end;
end;

function TRedisConfig.GetInteger(const Key: string; DefaultValue: Integer): Integer;  
var
  StrValue: string;
begin
  StrValue := GetString(Key, '');
  if StrValue <> '' then
    Result := StrToIntDef(StrValue, DefaultValue)
  else
    Result := DefaultValue;
end;

function TRedisConfig.GetBoolean(const Key: string; DefaultValue: Boolean): Boolean;  
var
  StrValue: string;
begin
  StrValue := LowerCase(GetString(Key, ''));
  if StrValue <> '' then
    Result := (StrValue = 'true') or (StrValue = '1') or (StrValue = 'yes')
  else
    Result := DefaultValue;
end;

procedure TRedisConfig.SetString(const Key, Value: string);  
begin
  FRedis.SetValue(FPrefix + Key, Value);

  // Mettre à jour le cache local
  FCacheLock.Enter;
  try
    FCache.Values[Key] := Value;
  finally
    FCacheLock.Leave;
  end;

  // Publier la modification
  FRedis.Publish(FPrefix + 'updates', Key);
end;

procedure TRedisConfig.SetInteger(const Key: string; Value: Integer);  
begin
  SetString(Key, IntToStr(Value));
end;

procedure TRedisConfig.SetBoolean(const Key: string; Value: Boolean);  
begin
  if Value then
    SetString(Key, 'true')
  else
    SetString(Key, 'false');
end;

procedure TRedisConfig.Subscribe(const Pattern: string);  
begin
  // S'abonner aux notifications de changement
  FRedis.Subscribe(FPrefix + Pattern);
end;

procedure TRedisConfig.Invalidate;  
begin
  FCacheLock.Enter;
  try
    FCache.Clear;
    FLastRefresh := 0; // Force refresh au prochain appel
  finally
    FCacheLock.Leave;
  end;
end;

end.
```

### Utilisation de la configuration Redis

```pascal
program UseRedisConfig;

uses
  SysUtils, RedisConfiguration;

var
  Config: TRedisConfig;
  ServerPort: Integer;
  DBHost: string;
  DebugMode: Boolean;

begin
  // Se connecter à Redis
  Config := TRedisConfig.Create('localhost', 6379, 'myapp:config:');
  try
    Config.CacheTTL := 30; // Cache de 30 secondes

    // Lire la configuration
    ServerPort := Config.GetInteger('server.port', 8080);
    DBHost := Config.GetString('database.host', 'localhost');
    DebugMode := Config.GetBoolean('app.debug', False);

    WriteLn('Configuration depuis Redis:');
    WriteLn('  Server Port: ', ServerPort);
    WriteLn('  Database Host: ', DBHost);
    WriteLn('  Debug Mode: ', DebugMode);

    // Modifier la configuration (visible par toutes les instances)
    Config.SetInteger('server.port', 9000);
    WriteLn('Port changé à 9000');

  finally
    Config.Free;
  end;

  ReadLn;
end.
```

### CLI pour gérer la configuration Redis

```bash
#!/bin/bash
# config-manager.sh - Outil CLI pour gérer la configuration Redis

REDIS_HOST="localhost"  
REDIS_PORT="6379"  
CONFIG_PREFIX="myapp:config:"

function config_get() {
    redis-cli -h $REDIS_HOST -p $REDIS_PORT GET "${CONFIG_PREFIX}$1"
}

function config_set() {
    redis-cli -h $REDIS_HOST -p $REDIS_PORT SET "${CONFIG_PREFIX}$1" "$2"
    redis-cli -h $REDIS_HOST -p $REDIS_PORT PUBLISH "${CONFIG_PREFIX}updates" "$1"
}

function config_list() {
    redis-cli -h $REDIS_HOST -p $REDIS_PORT KEYS "${CONFIG_PREFIX}*" | while read KEY; do
        VALUE=$(redis-cli -h $REDIS_HOST -p $REDIS_PORT GET "$KEY")
        CLEAN_KEY=$(echo "$KEY" | sed "s/${CONFIG_PREFIX}//")
        echo "$CLEAN_KEY = $VALUE"
    done
}

function config_delete() {
    redis-cli -h $REDIS_HOST -p $REDIS_PORT DEL "${CONFIG_PREFIX}$1"
}

case "$1" in
    get)
        config_get "$2"
        ;;
    set)
        config_set "$2" "$3"
        echo "✓ Configuration mise à jour: $2 = $3"
        ;;
    list)
        config_list
        ;;
    delete)
        config_delete "$2"
        echo "✓ Configuration supprimée: $2"
        ;;
    *)
        echo "Usage: $0 {get|set|list|delete} [key] [value]"
        echo ""
        echo "Exemples:"
        echo "  $0 list"
        echo "  $0 get server.port"
        echo "  $0 set server.port 9000"
        echo "  $0 delete server.port"
        exit 1
        ;;
esac
```

**Utilisation** :

```bash
# Lister toute la configuration
./config-manager.sh list

# Lire une valeur
./config-manager.sh get database.host

# Définir une valeur
./config-manager.sh set database.host "db-prod.example.com"

# Supprimer une valeur
./config-manager.sh delete old.setting
```

## Validation de la configuration

### Schéma de validation

```pascal
unit ConfigValidator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl;

type
  TValidationType = (vtString, vtInteger, vtBoolean, vtFloat, vtEmail, vtURL, vtIPAddress);

  TConfigRule = record
    Key: string;
    Required: Boolean;
    ValueType: TValidationType;
    MinValue: Integer;
    MaxValue: Integer;
    AllowedValues: TStringList;
    Pattern: string; // Regex
  end;

  TValidationError = record
    Key: string;
    Message: string;
  end;

  TValidationErrors = array of TValidationError;

  TConfigValidator = class
  private
    FRules: array of TConfigRule;

    function ValidateString(const Value: string; const Rule: TConfigRule): Boolean;
    function ValidateInteger(const Value: string; const Rule: TConfigRule): Boolean;
    function ValidateBoolean(const Value: string): Boolean;
    function ValidateEmail(const Value: string): Boolean;
    function ValidateURL(const Value: string): Boolean;
    function ValidateIPAddress(const Value: string): Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddRule(const Key: string; ValueType: TValidationType;
                     Required: Boolean = True);
    procedure AddRangeRule(const Key: string; MinValue, MaxValue: Integer);
    procedure AddEnumRule(const Key: string; const AllowedValues: array of string);

    function Validate(Config: TAppConfig; out Errors: TValidationErrors): Boolean;
    function ValidateAndRaise(Config: TAppConfig): Boolean;
  end;

implementation

uses
  RegExpr;

constructor TConfigValidator.Create;  
begin
  SetLength(FRules, 0);
end;

destructor TConfigValidator.Destroy;  
var
  i: Integer;
begin
  for i := 0 to High(FRules) do
    if Assigned(FRules[i].AllowedValues) then
      FRules[i].AllowedValues.Free;

  inherited;
end;

procedure TConfigValidator.AddRule(const Key: string; ValueType: TValidationType;
                                  Required: Boolean);
var
  Rule: TConfigRule;
begin
  Rule.Key := Key;
  Rule.Required := Required;
  Rule.ValueType := ValueType;
  Rule.MinValue := 0;
  Rule.MaxValue := MaxInt;
  Rule.AllowedValues := nil;
  Rule.Pattern := '';

  SetLength(FRules, Length(FRules) + 1);
  FRules[High(FRules)] := Rule;
end;

procedure TConfigValidator.AddRangeRule(const Key: string; MinValue, MaxValue: Integer);  
var
  i: Integer;
begin
  for i := 0 to High(FRules) do
  begin
    if FRules[i].Key = Key then
    begin
      FRules[i].MinValue := MinValue;
      FRules[i].MaxValue := MaxValue;
      Exit;
    end;
  end;
end;

procedure TConfigValidator.AddEnumRule(const Key: string; const AllowedValues: array of string);  
var
  i, j: Integer;
begin
  for i := 0 to High(FRules) do
  begin
    if FRules[i].Key = Key then
    begin
      FRules[i].AllowedValues := TStringList.Create;
      for j := 0 to High(AllowedValues) do
        FRules[i].AllowedValues.Add(AllowedValues[j]);
      Exit;
    end;
  end;
end;

function TConfigValidator.ValidateString(const Value: string; const Rule: TConfigRule): Boolean;  
begin
  Result := True;

  // Vérifier si la valeur est dans la liste autorisée
  if Assigned(Rule.AllowedValues) then
    Result := Rule.AllowedValues.IndexOf(Value) >= 0;
end;

function TConfigValidator.ValidateInteger(const Value: string; const Rule: TConfigRule): Boolean;  
var
  IntValue: Integer;
begin
  Result := TryStrToInt(Value, IntValue);

  if Result then
    Result := (IntValue >= Rule.MinValue) and (IntValue <= Rule.MaxValue);
end;

function TConfigValidator.ValidateBoolean(const Value: string): Boolean;  
var
  LowerValue: string;
begin
  LowerValue := LowerCase(Value);
  Result := (LowerValue = 'true') or (LowerValue = 'false') or
            (LowerValue = '1') or (LowerValue = '0') or
            (LowerValue = 'yes') or (LowerValue = 'no');
end;

function TConfigValidator.ValidateEmail(const Value: string): Boolean;  
var
  Regex: TRegExpr;
begin
  Regex := TRegExpr.Create;
  try
    Regex.Expression := '^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$';
    Result := Regex.Exec(Value);
  finally
    Regex.Free;
  end;
end;

function TConfigValidator.ValidateURL(const Value: string): Boolean;  
begin
  Result := (Pos('http://', Value) = 1) or (Pos('https://', Value) = 1);
end;

function TConfigValidator.ValidateIPAddress(const Value: string): Boolean;  
var
  Parts: TStringList;
  i, PartValue: Integer;
begin
  Parts := TStringList.Create;
  try
    Parts.Delimiter := '.';
    Parts.StrictDelimiter := True;
    Parts.DelimitedText := Value;
    Result := Parts.Count = 4;

    if Result then
    begin
      for i := 0 to 3 do
      begin
        if not TryStrToInt(Parts[i], PartValue) then
          Exit(False);

        if (PartValue < 0) or (PartValue > 255) then
          Exit(False);
      end;
    end;
  finally
    Parts.Free;
  end;
end;

function TConfigValidator.Validate(Config: TAppConfig; out Errors: TValidationErrors): Boolean;  
var
  i: Integer;
  Rule: TConfigRule;
  Value: string;
  ErrorCount: Integer;

  procedure AddError(const Msg: string);
  begin
    SetLength(Errors, ErrorCount + 1);
    Errors[ErrorCount].Key := Rule.Key;
    Errors[ErrorCount].Message := Msg;
    Inc(ErrorCount);
  end;

begin
  SetLength(Errors, 0);
  ErrorCount := 0;

  for i := 0 to High(FRules) do
  begin
    Rule := FRules[i];

    // Vérifier si la clé existe
    if not Config.HasKey(Rule.Key) then
    begin
      if Rule.Required then
        AddError('Clé requise manquante');
      Continue;
    end;

    Value := Config.GetString(Rule.Key);

    // Vérifier selon le type
    case Rule.ValueType of
      vtString:
        if not ValidateString(Value, Rule) then
          AddError('Valeur invalide pour le type string');

      vtInteger:
        if not ValidateInteger(Value, Rule) then
          AddError(Format('Valeur doit être un entier entre %d et %d',
                         [Rule.MinValue, Rule.MaxValue]));

      vtBoolean:
        if not ValidateBoolean(Value) then
          AddError('Valeur doit être true/false, 1/0, yes/no');

      vtEmail:
        if not ValidateEmail(Value) then
          AddError('Adresse email invalide');

      vtURL:
        if not ValidateURL(Value) then
          AddError('URL invalide (doit commencer par http:// ou https://)');

      vtIPAddress:
        if not ValidateIPAddress(Value) then
          AddError('Adresse IP invalide');
    end;
  end;

  Result := ErrorCount = 0;
end;

function TConfigValidator.ValidateAndRaise(Config: TAppConfig): Boolean;  
var
  Errors: TValidationErrors;
  i: Integer;
  ErrorMsg: string;
begin
  Result := Validate(Config, Errors);

  if not Result then
  begin
    ErrorMsg := 'Erreurs de validation de configuration:' + LineEnding;
    for i := 0 to High(Errors) do
      ErrorMsg := ErrorMsg + Format('  - %s: %s', [Errors[i].Key, Errors[i].Message]) + LineEnding;

    raise Exception.Create(ErrorMsg);
  end;
end;

end.
```

### Utilisation du validateur

```pascal
program ValidateConfig;

uses
  SysUtils, Configuration, ConfigValidator;

var
  Config: TAppConfig;
  Validator: TConfigValidator;
  Errors: TValidationErrors;
  i: Integer;

begin
  Config := TAppConfig.Create('config.ini');
  Validator := TConfigValidator.Create;
  try
    // Définir les règles de validation
    Validator.AddRule('database.host', vtString, True);
    Validator.AddRule('database.port', vtInteger, True);
    Validator.AddRangeRule('database.port', 1, 65535);

    Validator.AddRule('server.port', vtInteger, True);
    Validator.AddRangeRule('server.port', 1024, 65535);

    Validator.AddRule('app.environment', vtString, True);
    Validator.AddEnumRule('app.environment', ['development', 'staging', 'production']);

    Validator.AddRule('logging.level', vtString, True);
    Validator.AddEnumRule('logging.level', ['debug', 'info', 'warning', 'error', 'critical']);

    Validator.AddRule('admin.email', vtEmail, True);
    Validator.AddRule('api.url', vtURL, False);

    // Valider
    if Validator.Validate(Config, Errors) then
    begin
      WriteLn('✓ Configuration valide!');
    end
    else
    begin
      WriteLn('✗ Erreurs de validation:');
      for i := 0 to High(Errors) do
        WriteLn('  - ', Errors[i].Key, ': ', Errors[i].Message);
    end;

  finally
    Validator.Free;
    Config.Free;
  end;

  ReadLn;
end.
```

## Hot Reload - Rechargement à chaud

### Surveillance des modifications de fichiers

```pascal
unit ConfigWatcher;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Configuration, DateUtils;

type
  TConfigChangeEvent = procedure(Sender: TObject) of object;

  TConfigWatcher = class(TThread)
  private
    FConfig: TAppConfig;
    FFileName: string;
    FLastModified: TDateTime;
    FCheckInterval: Integer; // Millisecondes
    FOnConfigChanged: TConfigChangeEvent;

    function GetFileModifiedTime(const FileName: string): TDateTime;
  protected
    procedure Execute; override;
  public
    constructor Create(AConfig: TAppConfig; const AFileName: string;
                      ACheckInterval: Integer = 5000);

    property OnConfigChanged: TConfigChangeEvent read FOnConfigChanged write FOnConfigChanged;
  end;

implementation

constructor TConfigWatcher.Create(AConfig: TAppConfig; const AFileName: string;
                                 ACheckInterval: Integer);
begin
  inherited Create(False);
  FreeOnTerminate := False;

  FConfig := AConfig;
  FFileName := AFileName;
  FCheckInterval := ACheckInterval;
  FLastModified := GetFileModifiedTime(FFileName);
end;

function TConfigWatcher.GetFileModifiedTime(const FileName: string): TDateTime;  
var
  FileAge: LongInt;
begin
  FileAge := FileAge(FileName);
  if FileAge <> -1 then
    Result := FileDateToDateTime(FileAge)
  else
    Result := 0;
end;

procedure TConfigWatcher.Execute;  
var
  CurrentModified: TDateTime;
begin
  while not Terminated do
  begin
    CurrentModified := GetFileModifiedTime(FFileName);

    // Vérifier si le fichier a été modifié
    if CurrentModified > FLastModified then
    begin
      // Recharger la configuration
      FConfig.Reload;
      FLastModified := CurrentModified;

      // Déclencher l'événement
      if Assigned(FOnConfigChanged) then
        Synchronize(@FOnConfigChanged);
    end;

    Sleep(FCheckInterval);
  end;
end;

end.
```

### Utilisation du hot reload

```pascal
program HotReloadExample;

uses
  SysUtils, Configuration, ConfigWatcher;

var
  Config: TAppConfig;
  Watcher: TConfigWatcher;

procedure OnConfigChanged(Sender: TObject);  
begin
  WriteLn('[', FormatDateTime('hh:nn:ss', Now), '] Configuration rechargée!');
  WriteLn('  Nouveau port: ', Config.GetInteger('server.port'));
end;

begin
  Config := TAppConfig.Create('config.ini');
  try
    WriteLn('Application démarrée avec surveillance de configuration');
    WriteLn('Port initial: ', Config.GetInteger('server.port'));
    WriteLn('Modifiez config.ini pour voir le rechargement automatique...');
    WriteLn;

    // Démarrer la surveillance
    Watcher := TConfigWatcher.Create(Config, 'config.ini', 2000);
    Watcher.OnConfigChanged := @OnConfigChanged;
    try
      WriteLn('Appuyez sur Entrée pour quitter...');
      ReadLn;
    finally
      Watcher.Terminate;
      Watcher.WaitFor;
      Watcher.Free;
    end;
  finally
    Config.Free;
  end;
end.
```

## Configuration multi-plateforme

### Gestion des différences Windows/Ubuntu

```pascal
unit PlatformConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Configuration;

type
  TPlatformConfig = class(TAppConfig)
  private
    function GetPlatformSpecificPath(const Path: string): string;
  public
    function GetLogPath: string;
    function GetDataPath: string;
    function GetTempPath: string;
    function GetConfigPath: string;

    function GetExecutablePath: string;
  end;

implementation

uses
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  {$IFDEF UNIX}
  BaseUnix,
  {$ENDIF}
  FileUtil;

function TPlatformConfig.GetPlatformSpecificPath(const Path: string): string;  
begin
  Result := Path;

  // Remplacer les séparateurs selon la plateforme
  {$IFDEF WINDOWS}
  Result := StringReplace(Result, '/', '\', [rfReplaceAll]);
  {$ELSE}
  Result := StringReplace(Result, '\', '/', [rfReplaceAll]);
  {$ENDIF}

  // Étendre les chemins spéciaux
  Result := StringReplace(Result, '~', GetEnvironmentVariable('HOME'), [rfReplaceAll]);
  Result := StringReplace(Result, '%APPDATA%', GetEnvironmentVariable('APPDATA'), [rfReplaceAll]);
end;

function TPlatformConfig.GetLogPath: string;  
begin
  {$IFDEF WINDOWS}
  Result := GetString('logging.path.windows', '%APPDATA%\MyApp\logs');
  {$ELSE}
  Result := GetString('logging.path.linux', '/var/log/myapp');
  {$ENDIF}

  Result := GetPlatformSpecificPath(Result);

  // Créer le répertoire s'il n'existe pas
  if not DirectoryExists(Result) then
    ForceDirectories(Result);
end;

function TPlatformConfig.GetDataPath: string;  
begin
  {$IFDEF WINDOWS}
  Result := GetString('data.path.windows', '%APPDATA%\MyApp\data');
  {$ELSE}
  Result := GetString('data.path.linux', '~/.local/share/myapp');
  {$ENDIF}

  Result := GetPlatformSpecificPath(Result);

  if not DirectoryExists(Result) then
    ForceDirectories(Result);
end;

function TPlatformConfig.GetTempPath: string;  
begin
  {$IFDEF WINDOWS}
  Result := GetEnvironmentVariable('TEMP');
  {$ELSE}
  Result := '/tmp';
  {$ENDIF}

  Result := IncludeTrailingPathDelimiter(Result) + 'myapp';

  if not DirectoryExists(Result) then
    ForceDirectories(Result);
end;

function TPlatformConfig.GetConfigPath: string;  
begin
  {$IFDEF WINDOWS}
  Result := GetString('config.path.windows', '%APPDATA%\MyApp');
  {$ELSE}
  Result := GetString('config.path.linux', '~/.config/myapp');
  {$ENDIF}

  Result := GetPlatformSpecificPath(Result);
end;

function TPlatformConfig.GetExecutablePath: string;  
begin
  Result := ExtractFilePath(ParamStr(0));
end;

end.
```

### Fichier de configuration multi-plateforme

```ini
; config.ini - Configuration multi-plateforme

[app]
name = MyApp  
version = 1.0.0

[database]
host = localhost  
port = 5432  
name = myappdb

[paths.windows]
logs = %APPDATA%\MyApp\logs  
data = %APPDATA%\MyApp\data  
temp = %TEMP%\myapp

[paths.linux]
logs = /var/log/myapp  
data = ~/.local/share/myapp  
temp = /tmp/myapp

[service.windows]
name = MyAppService  
display_name = My Application Service  
description = Service for MyApp

[service.linux]
name = myapp  
description = My Application Daemon  
user = myapp  
group = myapp

[executable.windows]
binary = myapp.exe  
service_binary = myapp_service.exe

[executable.linux]
binary = myapp  
daemon_binary = myappd
```

## Bonnes pratiques

### 1. Principe des 12 facteurs (12-Factor App)

**Les configurations doivent être** :

✅ **Externalisées** : Jamais dans le code
```pascal
// ❌ MAUVAIS
const API_KEY = 'abc123';

// ✅ BON
APIKey := Config.GetString('api.key');
```

✅ **Par environnement** : Dev, staging, production différents
```bash
# Développement
export APP_ENVIRONMENT=development  
export DATABASE_HOST=localhost

# Production
export APP_ENVIRONMENT=production  
export DATABASE_HOST=db-prod.example.com
```

✅ **Variables d'environnement** : Format standard
```bash
export APP_DATABASE_HOST=localhost  
export APP_DATABASE_PORT=5432
```

### 2. Ne jamais versionner les secrets

**.gitignore** :
```
# Fichiers de configuration avec secrets
.env
.env.local
.env.*.local
config/*.secret.*  
secrets/
*.key
*.pem

# Sauf les templates
!.env.example
!config/config.example.ini
```

**.env.example** (à versionner) :
```bash
# .env.example - Template de configuration

# Application
APP_ENVIRONMENT=development  
APP_DEBUG=true

# Base de données
DATABASE_HOST=localhost  
DATABASE_PORT=5432  
DATABASE_NAME=myapp  
DATABASE_USER=user  
DATABASE_PASSWORD=changeme

# Redis
REDIS_HOST=localhost  
REDIS_PORT=6379

# API Keys (à remplacer par vos vraies clés)
API_KEY=your_api_key_here  
SECRET_KEY=your_secret_key_here
```

### 3. Valider au démarrage

```pascal
program MyApp;

uses
  SysUtils, Configuration, ConfigValidator;

var
  Config: TAppConfig;
  Validator: TConfigValidator;

begin
  try
    // Charger la configuration
    Config := TAppConfig.Create('config.ini');
    try
      // Valider
      Validator := TConfigValidator.Create;
      try
        // Ajouter les règles
        Validator.AddRule('database.host', vtString, True);
        Validator.AddRule('database.port', vtInteger, True);
        Validator.AddRangeRule('database.port', 1, 65535);

        // Valider et lever une exception si invalide
        Validator.ValidateAndRaise(Config);

        WriteLn('✓ Configuration valide');

        // Démarrer l'application
        StartApplication(Config);
      finally
        Validator.Free;
      end;
    finally
      Config.Free;
    end;
  except
    on E: Exception do
    begin
      WriteLn('✗ Erreur de configuration: ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
```

### 4. Logger les sources de configuration

```pascal
procedure LogConfigSources(Config: TAppConfig);  
var
  Keys: TStringList;
  i: Integer;
  Key, Value: string;
  Source: TConfigSource;
  SourceStr: string;
begin
  WriteLn('Configuration chargée:');
  WriteLn;

  Keys := TStringList.Create;
  try
    // Obtenir toutes les clés (à implémenter dans TAppConfig)

    for i := 0 to Keys.Count - 1 do
    begin
      Key := Keys[i];
      Value := Config.GetString(Key);
      Source := Config.GetSource(Key);

      case Source of
        csDefault: SourceStr := 'DEFAULT';
        csFile: SourceStr := 'FILE';
        csEnvironment: SourceStr := 'ENV';
        csCommandLine: SourceStr := 'CLI';
        csCentralized: SourceStr := 'REDIS';
      end;

      // Masquer les valeurs sensibles
      if (Pos('password', LowerCase(Key)) > 0) or
         (Pos('secret', LowerCase(Key)) > 0) or
         (Pos('key', LowerCase(Key)) > 0) then
        Value := '***HIDDEN***';

      WriteLn(Format('  %-30s = %-30s [%s]', [Key, Value, SourceStr]));
    end;
  finally
    Keys.Free;
  end;
end;
```

**Exemple de sortie** :
```
Configuration chargée:

  app.name                       = MyFreePascalApp              [FILE]
  app.debug                      = true                         [ENV]
  database.host                  = localhost                    [FILE]
  database.port                  = 5432                         [DEFAULT]
  database.password              = ***HIDDEN***                 [ENV]
  server.port                    = 9000                         [CLI]
  logging.level                  = debug                        [ENV]
```

### 5. Documentation de configuration

Créez un fichier **CONFIG.md** dans votre projet :

````markdown
# Guide de Configuration

## Variables d'environnement

### Obligatoires

| Variable | Description | Exemple |
|----------|-------------|---------|
| `APP_ENVIRONMENT` | Environnement d'exécution | `production` |
| `DATABASE_PASSWORD` | Mot de passe DB | `secretpassword` |
| `API_KEY` | Clé API externe | `sk_live_abc123` |

### Optionnelles

| Variable | Description | Défaut | Exemple |
|----------|-------------|--------|---------|
| `APP_DEBUG` | Mode debug | `false` | `true` |
| `DATABASE_HOST` | Hôte DB | `localhost` | `db.example.com` |
| `DATABASE_PORT` | Port DB | `5432` | `5432` |
| `SERVER_PORT` | Port serveur | `8080` | `9000` |

## Fichiers de configuration

### config.ini

Fichier principal de configuration. Ne doit PAS contenir de secrets.

```ini
[app]
name = MyApp  
version = 1.0.0

[database]
host = localhost  
port = 5432
```

### secrets.ini

Fichier de secrets (NON versionné). Créez depuis `secrets.ini.example`.

```ini
[database]
password = your_db_password

[api]
key = your_api_key
```

## Configuration par environnement

### Développement

```bash
export APP_ENVIRONMENT=development  
export APP_DEBUG=true  
export DATABASE_HOST=localhost
```

### Production

```bash
export APP_ENVIRONMENT=production  
export APP_DEBUG=false  
export DATABASE_HOST=db-prod.example.com  
export DATABASE_PASSWORD=strong_password
```

## Validation

Au démarrage, l'application valide:
- Présence des variables obligatoires
- Format des valeurs (port entre 1-65535, email valide, etc.)
- Cohérence entre les paramètres

En cas d'erreur, l'application affiche un message clair et s'arrête.
````

## Outils CLI pour la configuration

### Script de configuration interactive

```bash
#!/bin/bash
# setup-config.sh - Assistant de configuration interactive

CONFIG_FILE="config.ini"  
ENV_FILE=".env"

echo "=== Configuration de l'application ==="  
echo

# Fonction pour demander une valeur
ask() {
    local prompt="$1"
    local default="$2"
    local var_name="$3"
    local is_secret="$4"

    if [ -n "$default" ]; then
        prompt="$prompt [$default]"
    fi

    if [ "$is_secret" = "secret" ]; then
        read -sp "$prompt: " value
        echo
    else
        read -p "$prompt: " value
    fi

    # Utiliser la valeur par défaut si vide
    if [ -z "$value" ] && [ -n "$default" ]; then
        value="$default"
    fi

    # Définir la variable
    eval "$var_name='$value'"
}

# Environnement
echo "## Environnement"  
ask "Environnement (development/staging/production)" "development" "ENVIRONMENT"  
ask "Mode debug (true/false)" "false" "DEBUG"  
echo

# Base de données
echo "## Base de données"  
ask "Hôte" "localhost" "DB_HOST"  
ask "Port" "5432" "DB_PORT"  
ask "Nom de la base" "myapp" "DB_NAME"  
ask "Utilisateur" "postgres" "DB_USER"  
ask "Mot de passe" "" "DB_PASSWORD" "secret"  
echo

# Serveur
echo "## Serveur"  
ask "Port d'écoute" "8080" "SERVER_PORT"  
echo

# Générer le fichier .env
echo "# Génération du fichier $ENV_FILE..."  
cat > $ENV_FILE << EOF
# Configuration générée le $(date)

# Application
APP_ENVIRONMENT=$ENVIRONMENT  
APP_DEBUG=$DEBUG

# Base de données
DATABASE_HOST=$DB_HOST  
DATABASE_PORT=$DB_PORT  
DATABASE_NAME=$DB_NAME  
DATABASE_USER=$DB_USER  
DATABASE_PASSWORD=$DB_PASSWORD

# Serveur
SERVER_PORT=$SERVER_PORT  
EOF

chmod 600 $ENV_FILE  
echo "✓ Fichier $ENV_FILE créé avec succès"  
echo

# Générer config.ini (sans secrets)
echo "# Génération du fichier $CONFIG_FILE..."  
cat > $CONFIG_FILE << EOF
; Configuration générée le $(date)

[app]
name = MyApp  
environment = $ENVIRONMENT  
debug = $DEBUG

[database]
host = $DB_HOST  
port = $DB_PORT  
name = $DB_NAME  
user = $DB_USER

[server]
port = $SERVER_PORT  
EOF

echo "✓ Fichier $CONFIG_FILE créé avec succès"  
echo  
echo "Configuration terminée!"  
echo  
echo "Pour démarrer l'application:"  
echo "  1. Charger les variables: source $ENV_FILE"  
echo "  2. Lancer: ./myapp"
```

### Vérification de configuration

```bash
#!/bin/bash
# check-config.sh - Vérifie que la configuration est valide

ERRORS=0

check_env() {
    local var_name="$1"
    local description="$2"

    if [ -z "${!var_name}" ]; then
        echo "✗ $var_name ($description) n'est pas défini"
        ((ERRORS++))
    else
        echo "✓ $var_name est défini"
    fi
}

check_file() {
    local file="$1"

    if [ ! -f "$file" ]; then
        echo "✗ Fichier $file manquant"
        ((ERRORS++))
    else
        echo "✓ Fichier $file présent"
    fi
}

check_port() {
    local port="$1"
    local name="$2"

    if ! [[ "$port" =~ ^[0-9]+$ ]] || [ "$port" -lt 1 ] || [ "$port" -gt 65535 ]; then
        echo "✗ $name: port invalide ($port)"
        ((ERRORS++))
    else
        echo "✓ $name: port valide ($port)"
    fi
}

echo "=== Vérification de la configuration ==="  
echo

echo "## Variables d'environnement"  
check_env "APP_ENVIRONMENT" "Environnement"  
check_env "DATABASE_HOST" "Hôte de la base de données"  
check_env "DATABASE_PORT" "Port de la base de données"  
check_env "DATABASE_PASSWORD" "Mot de passe DB"  
echo

echo "## Fichiers"  
check_file "config.ini"  
check_file ".env"  
echo

echo "## Validation des valeurs"  
check_port "$DATABASE_PORT" "DATABASE_PORT"  
check_port "$SERVER_PORT" "SERVER_PORT"  
echo

if [ $ERRORS -eq 0 ]; then
    echo "✓ Configuration valide!"
    exit 0
else
    echo "✗ $ERRORS erreur(s) trouvée(s)"
    exit 1
fi
```

## Configuration pour Docker

### Dockerfile avec configuration

```dockerfile
FROM ubuntu:22.04

# Installer FreePascal
RUN apt-get update && apt-get install -y fpc

# Créer le répertoire de l'application
WORKDIR /app

# Copier les fichiers
COPY myapp /app/  
COPY config.ini /app/

# Créer les répertoires nécessaires
RUN mkdir -p /app/logs /app/data

# Variables d'environnement par défaut
ENV APP_ENVIRONMENT=production \
    APP_DEBUG=false \
    SERVER_PORT=8080 \
    DATABASE_HOST=db \
    DATABASE_PORT=5432

# Port exposé
EXPOSE 8080

# Commande de démarrage
CMD ["./myapp"]
```

### docker-compose.yml

```yaml
version: '3.8'

services:
  app:
    build: .
    ports:
      - "8080:8080"
    environment:
      - APP_ENVIRONMENT=production
      - APP_DEBUG=false
      - DATABASE_HOST=db
      - DATABASE_PORT=5432
      - DATABASE_NAME=myapp
      - DATABASE_USER=postgres
      - DATABASE_PASSWORD=${DB_PASSWORD}  # Depuis .env
      - REDIS_HOST=redis
      - REDIS_PORT=6379
    volumes:
      - ./logs:/app/logs
      - ./data:/app/data
    depends_on:
      - db
      - redis
    restart: unless-stopped

  db:
    image: postgres:15
    environment:
      - POSTGRES_DB=myapp
      - POSTGRES_USER=postgres
      - POSTGRES_PASSWORD=${DB_PASSWORD}
    volumes:
      - postgres_data:/var/lib/postgresql/data
    restart: unless-stopped

  redis:
    image: redis:7
    restart: unless-stopped

volumes:
  postgres_data:
```

**.env pour Docker** :
```bash
# .env pour docker-compose
DB_PASSWORD=strongpassword123
```

**Démarrage** :
```bash
# Créer le fichier .env
echo "DB_PASSWORD=strongpassword123" > .env

# Démarrer
docker-compose up -d

# Logs
docker-compose logs -f app
```

## Récapitulatif et checklist

### Checklist de gestion de configuration

**✅ Séparation code/configuration**
- [ ] Aucun secret dans le code source
- [ ] Configuration externalisée (fichiers, env vars)
- [ ] Un seul binaire pour tous les environnements

**✅ Hiérarchie des sources**
- [ ] Valeurs par défaut définies
- [ ] Fichier de configuration pour les valeurs communes
- [ ] Variables d'environnement pour les valeurs spécifiques
- [ ] Arguments CLI pour les overrides temporaires

**✅ Sécurité**
- [ ] Secrets hors du dépôt Git
- [ ] .gitignore configuré
- [ ] Chiffrement des secrets sensibles
- [ ] Permissions fichiers restrictives (600)

**✅ Validation**
- [ ] Validation au démarrage
- [ ] Messages d'erreur clairs
- [ ] Types et plages vérifiés
- [ ] Valeurs requises présentes

**✅ Documentation**
- [ ] README avec guide de configuration
- [ ] Fichiers .example versionnés
- [ ] Variables documentées
- [ ] Valeurs par défaut documentées

**✅ Multi-plateforme**
- [ ] Chemins adaptés Windows/Linux
- [ ] Séparateurs de chemin corrects
- [ ] Variables d'environnement portables

**✅ Opérations**
- [ ] Hot reload implémenté si nécessaire
- [ ] Logs des sources de configuration
- [ ] Outils CLI pour gérer la config
- [ ] Tests de configuration automatisés

## Conclusion

Une gestion de configuration robuste est essentielle pour des applications FreePascal/Lazarus professionnelles multi-plateformes. Les principes clés à retenir :

🔑 **Séparer le code de la configuration** : Facilite les déploiements et la sécurité

🔐 **Ne jamais versionner les secrets** : Utilisez des variables d'environnement ou des secrets managers

✅ **Valider au démarrage** : Évite les surprises en production

🔄 **Hot reload quand nécessaire** : Changements sans redémarrage

📝 **Documenter** : Facilite l'onboarding et réduit les erreurs

🌍 **Penser multi-plateforme** : Windows et Ubuntu ont des conventions différentes

Avec ces pratiques, vos applications sont plus sûres, plus flexibles et plus faciles à déployer dans différents environnements !

---

**Ressources complémentaires** :
- 12-Factor App Methodology : https://12factor.net/
- OWASP Secrets Management : https://owasp.org/www-community/vulnerabilities/
- Docker Configuration Best Practices : https://docs.docker.com/develop/dev-best-practices/

⏭️ [Disaster recovery](/22-devops-deploiement-multi-os/12-disaster-recovery.md)
