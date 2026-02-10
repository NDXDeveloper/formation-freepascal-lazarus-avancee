🔝 Retour au [Sommaire](/SOMMAIRE.md)

# Gestion Unifiée de la Configuration Multi-plateforme avec FreePascal/Lazarus

## Introduction

Lorsque vous développez une application qui doit fonctionner à la fois sur Windows et Linux/Ubuntu, l'un des défis majeurs est de gérer la configuration de manière cohérente. Chaque système d'exploitation a ses propres conventions pour stocker les paramètres : Windows utilise traditionnellement le registre ou des fichiers INI, tandis que Linux préfère les fichiers de configuration dans des répertoires spécifiques.

Ce tutoriel vous montrera comment créer un système de configuration unifié qui fonctionne de manière transparente sur les deux plateformes.

## Comprendre les différences entre plateformes

### Où sont stockées les configurations ?

#### Sur Windows

Windows offre plusieurs emplacements standard :

- **Registre Windows** : Base de données système centralisée
  - `HKEY_CURRENT_USER\Software\VotreApplication` pour les paramètres utilisateur
  - `HKEY_LOCAL_MACHINE\Software\VotreApplication` pour les paramètres système

- **Fichiers de configuration** :
  - `C:\Users\[Username]\AppData\Roaming\VotreApplication\` pour les données utilisateur
  - `C:\ProgramData\VotreApplication\` pour les données partagées
  - Fichiers INI dans le dossier de l'application

#### Sur Linux/Ubuntu

Linux suit la philosophie "tout est fichier" :

- **Répertoires standard** :
  - `~/.config/votreapplication/` pour la configuration utilisateur (standard XDG)
  - `/etc/votreapplication/` pour la configuration système
  - `~/.votreapplication/` (ancien style, encore utilisé)

- **Format des fichiers** :
  - Fichiers texte simples (INI, conf)
  - JSON pour les applications modernes
  - XML pour les configurations complexes

## Architecture d'une solution unifiée

### Principe de base

L'idée est de créer une couche d'abstraction qui cache les différences entre plateformes. Votre application utilise une API unique, et la bibliothèque de configuration s'occupe des détails spécifiques à chaque OS.

```pascal
// Votre code reste identique sur toutes les plateformes
Config.SetValue('General', 'Language', 'fr');
Language := Config.GetValue('General', 'Language', 'en');
```

## Création d'une classe de configuration portable

### Structure de base

Voici une classe de configuration qui fonctionne sur Windows et Linux :

```pascal
unit UnifiedConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles
  {$IFDEF WINDOWS}
  , Registry, Windows
  {$ENDIF}
  {$IFDEF UNIX}
  , BaseUnix
  {$ENDIF};

type
  // Types de stockage disponibles
  TConfigStorage = (
    csAutoDetect,    // Choisir automatiquement selon l'OS
    csIniFile,       // Fichier INI (portable)
    csRegistry,      // Registre Windows (Windows uniquement)
    csJSON,          // Fichier JSON (portable)
    csXML            // Fichier XML (portable)
  );

  { TUnifiedConfig }
  TUnifiedConfig = class
  private
    FStorage: TConfigStorage;
    FAppName: string;
    FConfigPath: string;
    FIniFile: TIniFile;
    {$IFDEF WINDOWS}
    FRegistry: TRegistry;
    {$ENDIF}

    procedure InitializeStorage;
    function GetConfigPath: string;
    function GetDefaultStorage: TConfigStorage;

  public
    constructor Create(const AppName: string; Storage: TConfigStorage = csAutoDetect);
    destructor Destroy; override;

    // Méthodes principales
    procedure SetValue(const Section, Key, Value: string);
    function GetValue(const Section, Key, Default: string): string;

    procedure SetInteger(const Section, Key: string; Value: Integer);
    function GetInteger(const Section, Key: string; Default: Integer): Integer;

    procedure SetBoolean(const Section, Key: string; Value: Boolean);
    function GetBoolean(const Section, Key: string; Default: Boolean): Boolean;

    procedure SetFloat(const Section, Key: string; Value: Double);
    function GetFloat(const Section, Key: string; Default: Double): Double;

    // Gestion des sections
    procedure GetSections(Sections: TStrings);
    procedure GetKeys(const Section: string; Keys: TStrings);
    procedure DeleteKey(const Section, Key: string);
    procedure DeleteSection(const Section: string);

    // Utilitaires
    procedure Save;
    procedure Reload;
    function BackupConfig(const BackupPath: string = ''): Boolean;
    function RestoreConfig(const BackupPath: string): Boolean;

    property ConfigPath: string read FConfigPath;
    property Storage: TConfigStorage read FStorage;
  end;

implementation

uses
  {$IFDEF UNIX}
  fpjson, jsonparser, jsonscanner,
  {$ENDIF}
  FileUtil, LazFileUtils;

{ TUnifiedConfig }

constructor TUnifiedConfig.Create(const AppName: string; Storage: TConfigStorage);
begin
  inherited Create;
  FAppName := AppName;

  if Storage = csAutoDetect then
    FStorage := GetDefaultStorage
  else
    FStorage := Storage;

  FConfigPath := GetConfigPath;
  InitializeStorage;
end;

destructor TUnifiedConfig.Destroy;
begin
  Save;  // Sauvegarder automatiquement à la destruction

  FreeAndNil(FIniFile);
  {$IFDEF WINDOWS}
  FreeAndNil(FRegistry);
  {$ENDIF}

  inherited Destroy;
end;

function TUnifiedConfig.GetDefaultStorage: TConfigStorage;
begin
  {$IFDEF WINDOWS}
    // Sur Windows, utiliser le registre par défaut pour les applications installées
    // ou INI pour les applications portables
    if DirectoryExists(ExtractFilePath(ParamStr(0)) + 'portable') then
      Result := csIniFile  // Mode portable détecté
    else
      Result := csRegistry;  // Mode installé
  {$ELSE}
    // Sur Linux, utiliser les fichiers INI par défaut
    Result := csIniFile;
  {$ENDIF}
end;

function TUnifiedConfig.GetConfigPath: string;
begin
  {$IFDEF WINDOWS}
    case FStorage of
      csRegistry:
        Result := 'Software\' + FAppName;

      csIniFile, csJSON, csXML:
        begin
          // Utiliser AppData\Roaming sur Windows
          Result := GetEnvironmentVariable('APPDATA');
          if Result = '' then
            Result := GetEnvironmentVariable('USERPROFILE') + '\AppData\Roaming';
          Result := IncludeTrailingPathDelimiter(Result) + FAppName;
        end;
    end;
  {$ENDIF}

  {$IFDEF UNIX}
    // Suivre le standard XDG Base Directory
    Result := GetEnvironmentVariable('XDG_CONFIG_HOME');
    if Result = '' then
      Result := GetEnvironmentVariable('HOME') + '/.config';
    Result := IncludeTrailingPathDelimiter(Result) + LowerCase(FAppName);
  {$ENDIF}

  // Créer le répertoire s'il n'existe pas
  if (FStorage <> csRegistry) and not DirectoryExists(Result) then
    ForceDirectories(Result);
end;

procedure TUnifiedConfig.InitializeStorage;
var
  ConfigFile: string;
begin
  case FStorage of
    csIniFile:
      begin
        ConfigFile := IncludeTrailingPathDelimiter(FConfigPath) + 'config.ini';
        FIniFile := TIniFile.Create(ConfigFile);
      end;

    {$IFDEF WINDOWS}
    csRegistry:
      begin
        FRegistry := TRegistry.Create;
        FRegistry.RootKey := HKEY_CURRENT_USER;
        if not FRegistry.OpenKey(FConfigPath, True) then
          raise Exception.Create('Impossible d''ouvrir la clé de registre');
      end;
    {$ENDIF}

    csJSON, csXML:
      begin
        // Pour JSON et XML, on utilisera des fichiers INI comme fallback
        // Dans une version complète, implémenter les parsers JSON/XML
        ConfigFile := IncludeTrailingPathDelimiter(FConfigPath) + 'config.ini';
        FIniFile := TIniFile.Create(ConfigFile);
      end;
  end;
end;

procedure TUnifiedConfig.SetValue(const Section, Key, Value: string);
begin
  case FStorage of
    csIniFile, csJSON, csXML:
      begin
        if Assigned(FIniFile) then
          FIniFile.WriteString(Section, Key, Value);
      end;

    {$IFDEF WINDOWS}
    csRegistry:
      begin
        if Assigned(FRegistry) then
        begin
          // Dans le registre, créer une sous-clé pour la section
          if FRegistry.OpenKey(FConfigPath + '\' + Section, True) then
          begin
            FRegistry.WriteString(Key, Value);
            FRegistry.OpenKey(FConfigPath, False);  // Revenir à la clé racine
          end;
        end;
      end;
    {$ENDIF}
  end;
end;

function TUnifiedConfig.GetValue(const Section, Key, Default: string): string;
begin
  Result := Default;

  case FStorage of
    csIniFile, csJSON, csXML:
      begin
        if Assigned(FIniFile) then
          Result := FIniFile.ReadString(Section, Key, Default);
      end;

    {$IFDEF WINDOWS}
    csRegistry:
      begin
        if Assigned(FRegistry) then
        begin
          if FRegistry.OpenKey(FConfigPath + '\' + Section, False) then
          begin
            if FRegistry.ValueExists(Key) then
              Result := FRegistry.ReadString(Key)
            else
              Result := Default;
            FRegistry.OpenKey(FConfigPath, False);
          end;
        end;
      end;
    {$ENDIF}
  end;
end;

procedure TUnifiedConfig.SetInteger(const Section, Key: string; Value: Integer);
begin
  SetValue(Section, Key, IntToStr(Value));
end;

function TUnifiedConfig.GetInteger(const Section, Key: string; Default: Integer): Integer;
var
  S: string;
begin
  S := GetValue(Section, Key, IntToStr(Default));
  Result := StrToIntDef(S, Default);
end;

procedure TUnifiedConfig.SetBoolean(const Section, Key: string; Value: Boolean);
begin
  SetValue(Section, Key, BoolToStr(Value, True));
end;

function TUnifiedConfig.GetBoolean(const Section, Key: string; Default: Boolean): Boolean;
var
  S: string;
begin
  S := GetValue(Section, Key, BoolToStr(Default, True));
  Result := StrToBoolDef(S, Default);
end;

procedure TUnifiedConfig.SetFloat(const Section, Key: string; Value: Double);
begin
  SetValue(Section, Key, FloatToStr(Value));
end;

function TUnifiedConfig.GetFloat(const Section, Key: string; Default: Double): Double;
var
  S: string;
begin
  S := GetValue(Section, Key, FloatToStr(Default));
  Result := StrToFloatDef(S, Default);
end;

procedure TUnifiedConfig.GetSections(Sections: TStrings);
begin
  Sections.Clear;

  case FStorage of
    csIniFile, csJSON, csXML:
      begin
        if Assigned(FIniFile) then
          FIniFile.ReadSections(Sections);
      end;

    {$IFDEF WINDOWS}
    csRegistry:
      begin
        if Assigned(FRegistry) then
          FRegistry.GetKeyNames(Sections);
      end;
    {$ENDIF}
  end;
end;

procedure TUnifiedConfig.GetKeys(const Section: string; Keys: TStrings);
begin
  Keys.Clear;

  case FStorage of
    csIniFile, csJSON, csXML:
      begin
        if Assigned(FIniFile) then
          FIniFile.ReadSection(Section, Keys);
      end;

    {$IFDEF WINDOWS}
    csRegistry:
      begin
        if Assigned(FRegistry) then
        begin
          if FRegistry.OpenKey(FConfigPath + '\' + Section, False) then
          begin
            FRegistry.GetValueNames(Keys);
            FRegistry.OpenKey(FConfigPath, False);
          end;
        end;
      end;
    {$ENDIF}
  end;
end;

procedure TUnifiedConfig.DeleteKey(const Section, Key: string);
begin
  case FStorage of
    csIniFile, csJSON, csXML:
      begin
        if Assigned(FIniFile) then
          FIniFile.DeleteKey(Section, Key);
      end;

    {$IFDEF WINDOWS}
    csRegistry:
      begin
        if Assigned(FRegistry) then
        begin
          if FRegistry.OpenKey(FConfigPath + '\' + Section, False) then
          begin
            FRegistry.DeleteValue(Key);
            FRegistry.OpenKey(FConfigPath, False);
          end;
        end;
      end;
    {$ENDIF}
  end;
end;

procedure TUnifiedConfig.DeleteSection(const Section: string);
begin
  case FStorage of
    csIniFile, csJSON, csXML:
      begin
        if Assigned(FIniFile) then
          FIniFile.EraseSection(Section);
      end;

    {$IFDEF WINDOWS}
    csRegistry:
      begin
        if Assigned(FRegistry) then
          FRegistry.DeleteKey(Section);
      end;
    {$ENDIF}
  end;
end;

procedure TUnifiedConfig.Save;
begin
  // Forcer l'écriture sur disque
  case FStorage of
    csIniFile, csJSON, csXML:
      begin
        if Assigned(FIniFile) then
          FIniFile.UpdateFile;
      end;

    {$IFDEF WINDOWS}
    csRegistry:
      begin
        // Le registre est sauvegardé automatiquement
      end;
    {$ENDIF}
  end;
end;

procedure TUnifiedConfig.Reload;
begin
  // Recharger la configuration depuis le stockage
  FreeAndNil(FIniFile);
  {$IFDEF WINDOWS}
  FreeAndNil(FRegistry);
  {$ENDIF}

  InitializeStorage;
end;

function TUnifiedConfig.BackupConfig(const BackupPath: string): Boolean;
var
  BackupFile: string;
  SourceFile: string;
begin
  Result := False;

  try
    if BackupPath = '' then
      BackupFile := FConfigPath + '.backup'
    else
      BackupFile := BackupPath;

    case FStorage of
      csIniFile:
        begin
          SourceFile := IncludeTrailingPathDelimiter(FConfigPath) + 'config.ini';
          Result := CopyFile(SourceFile, BackupFile);
        end;

      {$IFDEF WINDOWS}
      csRegistry:
        begin
          // Exporter la clé de registre
          // Nécessite des droits d'administration
          Result := FRegistry.SaveKey(FConfigPath, BackupFile);
        end;
      {$ENDIF}
    end;
  except
    Result := False;
  end;
end;

function TUnifiedConfig.RestoreConfig(const BackupPath: string): Boolean;
var
  BackupFile: string;
  TargetFile: string;
begin
  Result := False;

  try
    if BackupPath = '' then
      BackupFile := FConfigPath + '.backup'
    else
      BackupFile := BackupPath;

    case FStorage of
      csIniFile:
        begin
          TargetFile := IncludeTrailingPathDelimiter(FConfigPath) + 'config.ini';
          Result := CopyFile(BackupFile, TargetFile);
          if Result then
            Reload;
        end;

      {$IFDEF WINDOWS}
      csRegistry:
        begin
          // Restaurer la clé de registre
          Result := FRegistry.LoadKey(FConfigPath, BackupFile);
          if Result then
            Reload;
        end;
      {$ENDIF}
    end;
  except
    Result := False;
  end;
end;

end.
```

## Utilisation pratique de la configuration unifiée

### Initialisation simple

```pascal
program ConfigExample;

uses
  UnifiedConfig;

var
  Config: TUnifiedConfig;
begin
  // Création avec détection automatique du meilleur stockage
  Config := TUnifiedConfig.Create('MonApplication');
  try
    // La configuration fonctionne identiquement sur Windows et Linux

    // Première exécution : définir les valeurs par défaut
    if Config.GetBoolean('General', 'FirstRun', True) then
    begin
      Config.SetBoolean('General', 'FirstRun', False);
      Config.SetValue('General', 'Language', 'fr');
      Config.SetInteger('Display', 'WindowWidth', 800);
      Config.SetInteger('Display', 'WindowHeight', 600);
    end;

    // Lire les paramètres
    WriteLn('Langue : ', Config.GetValue('General', 'Language', 'en'));
    WriteLn('Largeur : ', Config.GetInteger('Display', 'WindowWidth', 640));

  finally
    Config.Free;  // Sauvegarde automatique à la libération
  end;
end.
```

## Gestion avancée des chemins de configuration

### Hiérarchie de configuration

Pour les applications complexes, il est utile d'avoir plusieurs niveaux de configuration :

```pascal
unit ConfigPaths;

{$mode objfpc}{$H+}

interface

type
  TConfigLevel = (
    clSystem,      // Configuration système (tous les utilisateurs)
    clUser,        // Configuration utilisateur
    clPortable,    // Configuration portable (dans le dossier de l'app)
    clTemporary    // Configuration temporaire (session)
  );

  TConfigPaths = class
  public
    class function GetConfigPath(Level: TConfigLevel; const AppName: string): string;
    class function GetDataPath(const AppName: string): string;
    class function GetCachePath(const AppName: string): string;
    class function GetLogPath(const AppName: string): string;
  end;

implementation

uses
  SysUtils, FileUtil
  {$IFDEF WINDOWS}
  , Windows, ShlObj
  {$ENDIF}
  {$IFDEF UNIX}
  , BaseUnix
  {$ENDIF};

class function TConfigPaths.GetConfigPath(Level: TConfigLevel; const AppName: string): string;
begin
  {$IFDEF WINDOWS}
  case Level of
    clSystem:
      begin
        // C:\ProgramData\AppName
        Result := GetEnvironmentVariable('ProgramData');
        if Result = '' then
          Result := 'C:\ProgramData';
      end;

    clUser:
      begin
        // C:\Users\Username\AppData\Roaming\AppName
        Result := GetEnvironmentVariable('APPDATA');
        if Result = '' then
          Result := GetEnvironmentVariable('USERPROFILE') + '\AppData\Roaming';
      end;

    clPortable:
      begin
        // Dossier de l'exécutable
        Result := ExtractFilePath(ParamStr(0));
      end;

    clTemporary:
      begin
        // C:\Users\Username\AppData\Local\Temp\AppName
        Result := GetTempDir;
      end;
  end;
  {$ENDIF}

  {$IFDEF UNIX}
  case Level of
    clSystem:
      begin
        // /etc/appname
        Result := '/etc';
      end;

    clUser:
      begin
        // ~/.config/appname (standard XDG)
        Result := GetEnvironmentVariable('XDG_CONFIG_HOME');
        if Result = '' then
          Result := GetEnvironmentVariable('HOME') + '/.config';
      end;

    clPortable:
      begin
        // Dossier de l'exécutable
        Result := ExtractFilePath(ParamStr(0));
      end;

    clTemporary:
      begin
        // /tmp/appname
        Result := '/tmp';
      end;
  end;
  {$ENDIF}

  Result := IncludeTrailingPathDelimiter(Result) + AppName;

  // Créer le répertoire s'il n'existe pas (sauf pour système qui nécessite des droits)
  if (Level <> clSystem) and not DirectoryExists(Result) then
    ForceDirectories(Result);
end;

class function TConfigPaths.GetDataPath(const AppName: string): string;
begin
  {$IFDEF WINDOWS}
    // C:\Users\Username\AppData\Local\AppName
    Result := GetEnvironmentVariable('LOCALAPPDATA');
    if Result = '' then
      Result := GetEnvironmentVariable('USERPROFILE') + '\AppData\Local';
  {$ENDIF}

  {$IFDEF UNIX}
    // ~/.local/share/appname (standard XDG)
    Result := GetEnvironmentVariable('XDG_DATA_HOME');
    if Result = '' then
      Result := GetEnvironmentVariable('HOME') + '/.local/share';
  {$ENDIF}

  Result := IncludeTrailingPathDelimiter(Result) + AppName;

  if not DirectoryExists(Result) then
    ForceDirectories(Result);
end;

class function TConfigPaths.GetCachePath(const AppName: string): string;
begin
  {$IFDEF WINDOWS}
    // C:\Users\Username\AppData\Local\AppName\Cache
    Result := GetDataPath(AppName) + PathDelim + 'Cache';
  {$ENDIF}

  {$IFDEF UNIX}
    // ~/.cache/appname (standard XDG)
    Result := GetEnvironmentVariable('XDG_CACHE_HOME');
    if Result = '' then
      Result := GetEnvironmentVariable('HOME') + '/.cache';
    Result := IncludeTrailingPathDelimiter(Result) + AppName;
  {$ENDIF}

  if not DirectoryExists(Result) then
    ForceDirectories(Result);
end;

class function TConfigPaths.GetLogPath(const AppName: string): string;
begin
  {$IFDEF WINDOWS}
    // C:\Users\Username\AppData\Local\AppName\Logs
    Result := GetDataPath(AppName) + PathDelim + 'Logs';
  {$ENDIF}

  {$IFDEF UNIX}
    // ~/.local/share/appname/logs ou /var/log/appname (si droits root)
    if fpGetUID = 0 then  // Si root
      Result := '/var/log/' + LowerCase(AppName)
    else
      Result := GetDataPath(AppName) + '/logs';
  {$ENDIF}

  if not DirectoryExists(Result) then
    ForceDirectories(Result);
end;

end.
```

## Format de configuration JSON portable

Pour une portabilité maximale, le format JSON est un excellent choix :

```pascal
unit JSONConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser;

type
  TJSONConfig = class
  private
    FFileName: string;
    FRoot: TJSONObject;
    FModified: Boolean;

    function GetPath(const Path: string): TJSONData;
    procedure SetPath(const Path: string; AValue: TJSONData);

  public
    constructor Create(const FileName: string);
    destructor Destroy; override;

    // Méthodes de lecture/écriture
    procedure SetValue(const Path: string; const Value: string); overload;
    procedure SetValue(const Path: string; Value: Integer); overload;
    procedure SetValue(const Path: string; Value: Boolean); overload;
    procedure SetValue(const Path: string; Value: Double); overload;

    function GetValue(const Path: string; const Default: string): string; overload;
    function GetValue(const Path: string; Default: Integer): Integer; overload;
    function GetValue(const Path: string; Default: Boolean): Boolean; overload;
    function GetValue(const Path: string; Default: Double): Double; overload;

    // Gestion des tableaux
    procedure AddToArray(const Path: string; const Value: string);
    function GetArray(const Path: string): TStringArray;

    // Utilitaires
    procedure Save;
    procedure Load;
    procedure Clear;

    property Modified: Boolean read FModified;
  end;

implementation

constructor TJSONConfig.Create(const FileName: string);
begin
  inherited Create;
  FFileName := FileName;
  FRoot := TJSONObject.Create;
  FModified := False;

  if FileExists(FFileName) then
    Load
  else
  begin
    // Créer la structure de base
    FRoot.Add('version', '1.0');
    FRoot.Add('settings', TJSONObject.Create);
    Save;
  end;
end;

destructor TJSONConfig.Destroy;
begin
  if FModified then
    Save;

  FRoot.Free;
  inherited Destroy;
end;

function TJSONConfig.GetPath(const Path: string): TJSONData;
var
  Parts: TStringArray;
  Current: TJSONData;
  i: Integer;
begin
  Result := nil;
  Parts := Path.Split(['/']);

  Current := FRoot;
  for i := 0 to High(Parts) do
  begin
    if Current is TJSONObject then
    begin
      Current := TJSONObject(Current).Find(Parts[i]);
      if Current = nil then
        Exit;
    end
    else
      Exit;
  end;

  Result := Current;
end;

procedure TJSONConfig.SetPath(const Path: string; AValue: TJSONData);
var
  Parts: TStringArray;
  Current, Parent: TJSONObject;
  i: Integer;
  Key: string;
begin
  Parts := Path.Split(['/']);

  Current := FRoot;

  // Créer le chemin si nécessaire
  for i := 0 to High(Parts) - 1 do
  begin
    if Current.Find(Parts[i]) = nil then
      Current.Add(Parts[i], TJSONObject.Create);
    Current := Current.Objects[Parts[i]];
  end;

  // Définir la valeur finale
  Key := Parts[High(Parts)];
  if Current.Find(Key) <> nil then
    Current.Delete(Key);
  Current.Add(Key, AValue);

  FModified := True;
end;

procedure TJSONConfig.SetValue(const Path: string; const Value: string);
begin
  SetPath(Path, TJSONString.Create(Value));
end;

procedure TJSONConfig.SetValue(const Path: string; Value: Integer);
begin
  SetPath(Path, TJSONIntegerNumber.Create(Value));
end;

procedure TJSONConfig.SetValue(const Path: string; Value: Boolean);
begin
  SetPath(Path, TJSONBoolean.Create(Value));
end;

procedure TJSONConfig.SetValue(const Path: string; Value: Double);
begin
  SetPath(Path, TJSONFloatNumber.Create(Value));
end;

function TJSONConfig.GetValue(const Path: string; const Default: string): string;
var
  Data: TJSONData;
begin
  Data := GetPath(Path);
  if Data <> nil then
    Result := Data.AsString
  else
  begin
    Result := Default;
    SetValue(Path, Default);  // Créer avec la valeur par défaut
  end;
end;

function TJSONConfig.GetValue(const Path: string; Default: Integer): Integer;
var
  Data: TJSONData;
begin
  Data := GetPath(Path);
  if Data <> nil then
    Result := Data.AsInteger
  else
  begin
    Result := Default;
    SetValue(Path, Default);
  end;
end;

function TJSONConfig.GetValue(const Path: string; Default: Boolean): Boolean;
var
  Data: TJSONData;
begin
  Data := GetPath(Path);
  if Data <> nil then
    Result := Data.AsBoolean
  else
  begin
    Result := Default;
    SetValue(Path, Default);
  end;
end;

function TJSONConfig.GetValue(const Path: string; Default: Double): Double;
var
  Data: TJSONData;
begin
  Data := GetPath(Path);
  if Data <> nil then
    Result := Data.AsFloat
  else
  begin
    Result := Default;
    SetValue(Path, Default);
  end;
end;

procedure TJSONConfig.AddToArray(const Path: string; const Value: string);
var
  Data: TJSONData;
  Arr: TJSONArray;
begin
  Data := GetPath(Path);

  if Data is TJSONArray then
    Arr := TJSONArray(Data)
  else
  begin
    Arr := TJSONArray.Create;
    SetPath(Path, Arr);
  end;

  Arr.Add(Value);
  FModified := True;
end;

function TJSONConfig.GetArray(const Path: string): TStringArray;
var
  Data: TJSONData;
  Arr: TJSONArray;
  i: Integer;
begin
  SetLength(Result, 0);

  Data := GetPath(Path);
  if Data is TJSONArray then
  begin
    Arr := TJSONArray(Data);
    SetLength(Result, Arr.Count);
    for i := 0 to Arr.Count - 1 do
      Result[i] := Arr[i].AsString;
  end;
end;

procedure TJSONConfig.Save;
var
  Stream: TFileStream;
  JSONStr: string;
begin
  if not FModified then
    Exit;

  // Créer le répertoire si nécessaire
  ForceDirectories(ExtractFileDir(FFileName));

  // Sauvegarder en JSON formaté (lisible)
  JSONStr := FRoot.FormatJSON();

  Stream := TFileStream.Create(FFileName, fmCreate or fmOpenWrite);
  try
    Stream.Size := 0;  // Vider le fichier existant
    Stream.WriteBuffer(JSONStr[1], Length(JSONStr));
    FModified := False;
  finally
    Stream.Free;
  end;
end;

procedure TJSONConfig.Load;
var
  Stream: TFileStream;
  Parser: TJSONParser;
begin
  if not FileExists(FFileName) then
    Exit;

  Stream := TFileStream.Create(FFileName, fmOpenRead);
  try
    Parser := TJSONParser.Create(Stream);
    try
      FreeAndNil(FRoot);
      FRoot := Parser.Parse as TJSONObject;
      FModified := False;
    finally
      Parser.Free;
    end;
  finally
    Stream.Free;
  end;
end;

procedure TJSONConfig.Clear;
begin
  FreeAndNil(FRoot);
  FRoot := TJSONObject.Create;
  FRoot.Add('version', '1.0');
  FRoot.Add('settings', TJSONObject.Create);
  FModified := True;
end;

end.
```

## Migration de configuration entre versions

Lorsque votre application évolue, vous devez parfois migrer les configurations anciennes vers un nouveau format :

```pascal
unit ConfigMigration;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UnifiedConfig;

type
  TConfigVersion = record
    Major: Integer;
    Minor: Integer;
    Build: Integer;
  end;

  { TConfigMigrator }
  TConfigMigrator = class
  private
    FConfig: TUnifiedConfig;
    FCurrentVersion: TConfigVersion;

    function GetStoredVersion: TConfigVersion;
    procedure SetStoredVersion(const Version: TConfigVersion);

    // Méthodes de migration spécifiques
    procedure MigrateFrom1_0To2_0;
    procedure MigrateFrom2_0To2_1;
    procedure MigrateFrom2_1To3_0;

  public
    constructor Create(Config: TUnifiedConfig);

    procedure CheckAndMigrate;
    function NeedsMigration: Boolean;

    class function VersionToString(const Version: TConfigVersion): string;
    class function StringToVersion(const VersionStr: string): TConfigVersion;
  end;

implementation

{ TConfigMigrator }

constructor TConfigMigrator.Create(Config: TUnifiedConfig);
begin
  inherited Create;
  FConfig := Config;

  // Version actuelle de l'application
  FCurrentVersion.Major := 3;
  FCurrentVersion.Minor := 0;
  FCurrentVersion.Build := 0;
end;

function TConfigMigrator.GetStoredVersion: TConfigVersion;
var
  VersionStr: string;
begin
  VersionStr := FConfig.GetValue('Meta', 'ConfigVersion', '1.0.0');
  Result := StringToVersion(VersionStr);
end;

procedure TConfigMigrator.SetStoredVersion(const Version: TConfigVersion);
begin
  FConfig.SetValue('Meta', 'ConfigVersion', VersionToString(Version));
end;

function TConfigMigrator.NeedsMigration: Boolean;
var
  StoredVer: TConfigVersion;
begin
  StoredVer := GetStoredVersion;

  Result := (StoredVer.Major < FCurrentVersion.Major) or
            ((StoredVer.Major = FCurrentVersion.Major) and
             (StoredVer.Minor < FCurrentVersion.Minor));
end;

procedure TConfigMigrator.CheckAndMigrate;
var
  StoredVer: TConfigVersion;
  BackupPath: string;
begin
  if not NeedsMigration then
    Exit;

  StoredVer := GetStoredVersion;

  // Faire une sauvegarde avant migration
  BackupPath := FConfig.ConfigPath + '.backup_' +
                FormatDateTime('yyyymmdd_hhnnss', Now);
  FConfig.BackupConfig(BackupPath);

  WriteLn('Migration de la configuration de ', VersionToString(StoredVer),
          ' vers ', VersionToString(FCurrentVersion));

  // Appliquer les migrations dans l'ordre
  if (StoredVer.Major = 1) and (StoredVer.Minor = 0) then
  begin
    MigrateFrom1_0To2_0;
    StoredVer.Major := 2;
    StoredVer.Minor := 0;
  end;

  if (StoredVer.Major = 2) and (StoredVer.Minor = 0) then
  begin
    MigrateFrom2_0To2_1;
    StoredVer.Minor := 1;
  end;

  if (StoredVer.Major = 2) and (StoredVer.Minor = 1) then
  begin
    MigrateFrom2_1To3_0;
    StoredVer.Major := 3;
    StoredVer.Minor := 0;
  end;

  // Mettre à jour la version stockée
  SetStoredVersion(FCurrentVersion);
  FConfig.Save;

  WriteLn('Migration terminée avec succès');
end;

procedure TConfigMigrator.MigrateFrom1_0To2_0;
begin
  // Exemple : Renommer des clés

  // Ancienne structure : Settings/WindowPosX, WindowPosY
  // Nouvelle structure : Display/Window/X, Display/Window/Y

  if FConfig.GetValue('Settings', 'WindowPosX', '') <> '' then
  begin
    FConfig.SetInteger('Display', 'WindowX',
      FConfig.GetInteger('Settings', 'WindowPosX', 0));
    FConfig.DeleteKey('Settings', 'WindowPosX');
  end;

  if FConfig.GetValue('Settings', 'WindowPosY', '') <> '' then
  begin
    FConfig.SetInteger('Display', 'WindowY',
      FConfig.GetInteger('Settings', 'WindowPosY', 0));
    FConfig.DeleteKey('Settings', 'WindowPosY');
  end;

  // Ajouter de nouvelles valeurs par défaut
  FConfig.SetBoolean('Features', 'AutoUpdate', True);
  FConfig.SetValue('Features', 'UpdateChannel', 'stable');
end;

procedure TConfigMigrator.MigrateFrom2_0To2_1;
var
  OldLang: string;
  NewLang: string;
begin
  // Exemple : Convertir des valeurs

  // Convertir l'ancien format de langue (code 2 lettres) vers nouveau (code complet)
  OldLang := FConfig.GetValue('General', 'Language', 'en');

  if OldLang = 'en' then
    NewLang := 'en-US'
  else if OldLang = 'fr' then
    NewLang := 'fr-FR'
  else if OldLang = 'de' then
    NewLang := 'de-DE'
  else if OldLang = 'es' then
    NewLang := 'es-ES'
  else
    NewLang := OldLang + '-' + UpperCase(OldLang);

  FConfig.SetValue('General', 'Language', NewLang);

  // Ajouter support pour thèmes
  FConfig.SetValue('Appearance', 'Theme', 'default');
  FConfig.SetBoolean('Appearance', 'DarkMode', False);
end;

procedure TConfigMigrator.MigrateFrom2_1To3_0;
var
  Sections: TStringList;
  Keys: TStringList;
  i, j: Integer;
begin
  // Exemple : Restructuration majeure

  // Déplacer tous les paramètres d'interface vers une nouvelle section
  Sections := TStringList.Create;
  Keys := TStringList.Create;
  try
    FConfig.GetSections(Sections);

    for i := 0 to Sections.Count - 1 do
    begin
      if Sections[i].StartsWith('UI_') then
      begin
        FConfig.GetKeys(Sections[i], Keys);

        for j := 0 to Keys.Count - 1 do
        begin
          // Copier vers la nouvelle structure
          FConfig.SetValue('UserInterface/' + Sections[i].Substring(3),
                          Keys[j],
                          FConfig.GetValue(Sections[i], Keys[j], ''));
        end;

        // Supprimer l'ancienne section
        FConfig.DeleteSection(Sections[i]);
      end;
    end;
  finally
    Keys.Free;
    Sections.Free;
  end;

  // Ajouter des paramètres pour nouvelles fonctionnalités
  FConfig.SetBoolean('Advanced', 'EnablePlugins', False);
  FConfig.SetValue('Advanced', 'PluginDirectory', '');
end;

class function TConfigMigrator.VersionToString(const Version: TConfigVersion): string;
begin
  Result := Format('%d.%d.%d', [Version.Major, Version.Minor, Version.Build]);
end;

class function TConfigMigrator.StringToVersion(const VersionStr: string): TConfigVersion;
var
  Parts: TStringArray;
begin
  Parts := VersionStr.Split(['.']);

  Result.Major := 1;
  Result.Minor := 0;
  Result.Build := 0;

  if Length(Parts) > 0 then
    Result.Major := StrToIntDef(Parts[0], 1);
  if Length(Parts) > 1 then
    Result.Minor := StrToIntDef(Parts[1], 0);
  if Length(Parts) > 2 then
    Result.Build := StrToIntDef(Parts[2], 0);
end;

end.
```

## Configuration avec validation et schéma

Pour garantir l'intégrité de la configuration, il est important d'implémenter une validation :

```pascal
unit ConfigValidator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UnifiedConfig, Generics.Collections;

type
  TConfigDataType = (
    cdtString,
    cdtInteger,
    cdtFloat,
    cdtBoolean,
    cdtEnum,
    cdtPath,
    cdtEmail,
    cdtURL
  );

  TConfigRule = record
    Section: string;
    Key: string;
    DataType: TConfigDataType;
    Required: Boolean;
    DefaultValue: string;
    MinValue: Double;
    MaxValue: Double;
    AllowedValues: TStringArray;
    Description: string;
  end;

  { TConfigValidator }
  TConfigValidator = class
  private
    FRules: array of TConfigRule;
    FErrors: TStringList;

    function ValidateString(const Value: string; const Rule: TConfigRule): Boolean;
    function ValidateInteger(const Value: string; const Rule: TConfigRule): Boolean;
    function ValidateFloat(const Value: string; const Rule: TConfigRule): Boolean;
    function ValidateBoolean(const Value: string; const Rule: TConfigRule): Boolean;
    function ValidateEnum(const Value: string; const Rule: TConfigRule): Boolean;
    function ValidatePath(const Value: string; const Rule: TConfigRule): Boolean;
    function ValidateEmail(const Value: string; const Rule: TConfigRule): Boolean;
    function ValidateURL(const Value: string; const Rule: TConfigRule): Boolean;

  public
    constructor Create;
    destructor Destroy; override;

    // Définition des règles
    procedure AddRule(const Section, Key: string; DataType: TConfigDataType;
                      Required: Boolean = False; const DefaultValue: string = '');
    procedure AddIntegerRule(const Section, Key: string;
                            MinValue, MaxValue: Integer;
                            Required: Boolean = False; DefaultValue: Integer = 0);
    procedure AddEnumRule(const Section, Key: string;
                         const AllowedValues: array of string;
                         Required: Boolean = False; const DefaultValue: string = '');

    // Validation
    function Validate(Config: TUnifiedConfig): Boolean;
    procedure ApplyDefaults(Config: TUnifiedConfig);

    // Rapport d'erreurs
    function GetErrors: TStringList;
    procedure ClearErrors;
  end;

implementation

uses
  RegExpr;

{ TConfigValidator }

constructor TConfigValidator.Create;
begin
  inherited Create;
  FErrors := TStringList.Create;
  SetLength(FRules, 0);
end;

destructor TConfigValidator.Destroy;
begin
  FErrors.Free;
  inherited Destroy;
end;

procedure TConfigValidator.AddRule(const Section, Key: string;
  DataType: TConfigDataType; Required: Boolean; const DefaultValue: string);
var
  Rule: TConfigRule;
begin
  Rule.Section := Section;
  Rule.Key := Key;
  Rule.DataType := DataType;
  Rule.Required := Required;
  Rule.DefaultValue := DefaultValue;
  Rule.MinValue := -MaxDouble;
  Rule.MaxValue := MaxDouble;
  SetLength(Rule.AllowedValues, 0);

  SetLength(FRules, Length(FRules) + 1);
  FRules[High(FRules)] := Rule;
end;

procedure TConfigValidator.AddIntegerRule(const Section, Key: string;
  MinValue, MaxValue: Integer; Required: Boolean; DefaultValue: Integer);
var
  Rule: TConfigRule;
begin
  Rule.Section := Section;
  Rule.Key := Key;
  Rule.DataType := cdtInteger;
  Rule.Required := Required;
  Rule.DefaultValue := IntToStr(DefaultValue);
  Rule.MinValue := MinValue;
  Rule.MaxValue := MaxValue;
  SetLength(Rule.AllowedValues, 0);

  SetLength(FRules, Length(FRules) + 1);
  FRules[High(FRules)] := Rule;
end;

procedure TConfigValidator.AddEnumRule(const Section, Key: string;
  const AllowedValues: array of string; Required: Boolean;
  const DefaultValue: string);
var
  Rule: TConfigRule;
  i: Integer;
begin
  Rule.Section := Section;
  Rule.Key := Key;
  Rule.DataType := cdtEnum;
  Rule.Required := Required;
  Rule.DefaultValue := DefaultValue;

  SetLength(Rule.AllowedValues, Length(AllowedValues));
  for i := 0 to High(AllowedValues) do
    Rule.AllowedValues[i] := AllowedValues[i];

  SetLength(FRules, Length(FRules) + 1);
  FRules[High(FRules)] := Rule;
end;

function TConfigValidator.Validate(Config: TUnifiedConfig): Boolean;
var
  i: Integer;
  Value: string;
  IsValid: Boolean;
begin
  ClearErrors;
  Result := True;

  for i := 0 to High(FRules) do
  begin
    Value := Config.GetValue(FRules[i].Section, FRules[i].Key, '');

    // Vérifier si requis
    if FRules[i].Required and (Value = '') then
    begin
      FErrors.Add(Format('Paramètre requis manquant : [%s] %s',
                        [FRules[i].Section, FRules[i].Key]));
      Result := False;
      Continue;
    end;

    // Si pas de valeur et pas requis, passer
    if Value = '' then
      Continue;

    // Valider selon le type
    case FRules[i].DataType of
      cdtString:  IsValid := ValidateString(Value, FRules[i]);
      cdtInteger: IsValid := ValidateInteger(Value, FRules[i]);
      cdtFloat:   IsValid := ValidateFloat(Value, FRules[i]);
      cdtBoolean: IsValid := ValidateBoolean(Value, FRules[i]);
      cdtEnum:    IsValid := ValidateEnum(Value, FRules[i]);
      cdtPath:    IsValid := ValidatePath(Value, FRules[i]);
      cdtEmail:   IsValid := ValidateEmail(Value, FRules[i]);
      cdtURL:     IsValid := ValidateURL(Value, FRules[i]);
    else
      IsValid := True;
    end;

    if not IsValid then
      Result := False;
  end;
end;

procedure TConfigValidator.ApplyDefaults(Config: TUnifiedConfig);
var
  i: Integer;
  Value: string;
begin
  for i := 0 to High(FRules) do
  begin
    Value := Config.GetValue(FRules[i].Section, FRules[i].Key, '');

    if (Value = '') and (FRules[i].DefaultValue <> '') then
    begin
      Config.SetValue(FRules[i].Section, FRules[i].Key, FRules[i].DefaultValue);
    end;
  end;
end;

function TConfigValidator.ValidateString(const Value: string;
  const Rule: TConfigRule): Boolean;
begin
  Result := True;  // Les chaînes sont toujours valides sauf contraintes spécifiques

  // Vérifier la longueur si nécessaire
  if (Rule.MinValue > 0) and (Length(Value) < Rule.MinValue) then
  begin
    FErrors.Add(Format('[%s] %s : chaîne trop courte (min: %d caractères)',
                      [Rule.Section, Rule.Key, Trunc(Rule.MinValue)]));
    Result := False;
  end;

  if (Rule.MaxValue < MaxDouble) and (Length(Value) > Rule.MaxValue) then
  begin
    FErrors.Add(Format('[%s] %s : chaîne trop longue (max: %d caractères)',
                      [Rule.Section, Rule.Key, Trunc(Rule.MaxValue)]));
    Result := False;
  end;
end;

function TConfigValidator.ValidateInteger(const Value: string;
  const Rule: TConfigRule): Boolean;
var
  IntVal: Integer;
begin
  Result := TryStrToInt(Value, IntVal);

  if not Result then
  begin
    FErrors.Add(Format('[%s] %s : valeur entière invalide ("%s")',
                      [Rule.Section, Rule.Key, Value]));
    Exit;
  end;

  if IntVal < Rule.MinValue then
  begin
    FErrors.Add(Format('[%s] %s : valeur trop petite (min: %d)',
                      [Rule.Section, Rule.Key, Trunc(Rule.MinValue)]));
    Result := False;
  end;

  if IntVal > Rule.MaxValue then
  begin
    FErrors.Add(Format('[%s] %s : valeur trop grande (max: %d)',
                      [Rule.Section, Rule.Key, Trunc(Rule.MaxValue)]));
    Result := False;
  end;
end;

function TConfigValidator.ValidateFloat(const Value: string;
  const Rule: TConfigRule): Boolean;
var
  FloatVal: Double;
begin
  Result := TryStrToFloat(Value, FloatVal);

  if not Result then
  begin
    FErrors.Add(Format('[%s] %s : valeur décimale invalide ("%s")',
                      [Rule.Section, Rule.Key, Value]));
    Exit;
  end;

  if FloatVal < Rule.MinValue then
  begin
    FErrors.Add(Format('[%s] %s : valeur trop petite (min: %f)',
                      [Rule.Section, Rule.Key, Rule.MinValue]));
    Result := False;
  end;

  if FloatVal > Rule.MaxValue then
  begin
    FErrors.Add(Format('[%s] %s : valeur trop grande (max: %f)',
                      [Rule.Section, Rule.Key, Rule.MaxValue]));
    Result := False;
  end;
end;

function TConfigValidator.ValidateBoolean(const Value: string;
  const Rule: TConfigRule): Boolean;
begin
  Result := (LowerCase(Value) = 'true') or (LowerCase(Value) = 'false') or
            (Value = '1') or (Value = '0');

  if not Result then
  begin
    FErrors.Add(Format('[%s] %s : valeur booléenne invalide ("%s")',
                      [Rule.Section, Rule.Key, Value]));
  end;
end;

function TConfigValidator.ValidateEnum(const Value: string;
  const Rule: TConfigRule): Boolean;
var
  i: Integer;
begin
  Result := False;

  for i := 0 to High(Rule.AllowedValues) do
  begin
    if Value = Rule.AllowedValues[i] then
    begin
      Result := True;
      Break;
    end;
  end;

  if not Result then
  begin
    FErrors.Add(Format('[%s] %s : valeur non autorisée ("%s")',
                      [Rule.Section, Rule.Key, Value]));
  end;
end;

function TConfigValidator.ValidatePath(const Value: string;
  const Rule: TConfigRule): Boolean;
var
  ExpandedPath: string;
begin
  // Vérifier si le chemin existe ou est valide
  Result := DirectoryExists(Value) or FileExists(Value);

  if not Result then
  begin
    // Accepter aussi les chemins avec variables d'environnement
    ExpandedPath := ExpandFileName(Value);
    Result := DirectoryExists(ExpandedPath) or FileExists(ExpandedPath);

    if not Result then
      FErrors.Add(Format('[%s] %s : chemin introuvable ("%s")',
                        [Rule.Section, Rule.Key, Value]));
  end;
end;

function TConfigValidator.ValidateEmail(const Value: string;
  const Rule: TConfigRule): Boolean;
var
  RegEx: TRegExpr;
begin
  RegEx := TRegExpr.Create;
  try
    RegEx.Expression := '^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$';
    Result := RegEx.Exec(Value);

    if not Result then
      FErrors.Add(Format('[%s] %s : adresse email invalide ("%s")',
                        [Rule.Section, Rule.Key, Value]));
  finally
    RegEx.Free;
  end;
end;

function TConfigValidator.ValidateURL(const Value: string;
  const Rule: TConfigRule): Boolean;
var
  RegEx: TRegExpr;
begin
  RegEx := TRegExpr.Create;
  try
    RegEx.Expression := '^(https?|ftp)://[^\s/$.?#].[^\s]*$';
    Result := RegEx.Exec(Value);

    if not Result then
      FErrors.Add(Format('[%s] %s : URL invalide ("%s")',
                        [Rule.Section, Rule.Key, Value]));
  finally
    RegEx.Free;
  end;
end;

function TConfigValidator.GetErrors: TStringList;
begin
  Result := FErrors;
end;

procedure TConfigValidator.ClearErrors;
begin
  FErrors.Clear;
end;

end.
```

## Exemple d'utilisation complète

Voici comment utiliser toutes ces classes ensemble dans une application réelle :

```pascal
program CompleteConfigExample;

{$mode objfpc}{$H+}

uses
  SysUtils,
  UnifiedConfig,
  ConfigMigration,
  ConfigValidator,
  JSONConfig;

var
  Config: TUnifiedConfig;
  Migrator: TConfigMigrator;
  Validator: TConfigValidator;
  JsonConfig: TJSONConfig;

procedure InitializeConfiguration;
var
  i: Integer;
begin
  // Créer la configuration unifiée
  Config := TUnifiedConfig.Create('MonApplication');

  // Vérifier et effectuer les migrations si nécessaire
  Migrator := TConfigMigrator.Create(Config);
  try
    if Migrator.NeedsMigration then
    begin
      WriteLn('Migration de configuration nécessaire...');
      Migrator.CheckAndMigrate;
    end;
  finally
    Migrator.Free;
  end;

  // Définir les règles de validation
  Validator := TConfigValidator.Create;

  // Paramètres généraux
  Validator.AddEnumRule('General', 'Language',
                       ['fr-FR', 'en-US', 'de-DE', 'es-ES'],
                       True, 'en-US');
  Validator.AddIntegerRule('General', 'AutoSaveInterval',
                          1, 3600, False, 300);

  // Paramètres d'affichage
  Validator.AddIntegerRule('Display', 'WindowWidth',
                          640, 3840, False, 1024);
  Validator.AddIntegerRule('Display', 'WindowHeight',
                          480, 2160, False, 768);
  Validator.AddBoolean('Display', 'FullScreen', False, 'false');

  // Paramètres réseau
  Validator.AddURL('Network', 'ServerURL', False, '');
  Validator.AddIntegerRule('Network', 'Port',
                          1, 65535, False, 8080);
  Validator.AddIntegerRule('Network', 'Timeout',
                          1, 300, False, 30);

  // Appliquer les valeurs par défaut si nécessaire
  Validator.ApplyDefaults(Config);

  // Valider la configuration
  if not Validator.Validate(Config) then
  begin
    WriteLn('Erreurs de configuration détectées :');
    for i := 0 to Validator.GetErrors.Count - 1 do
      WriteLn('  - ', Validator.GetErrors[i]);
  end;

  Validator.Free;
end;

procedure DemonstrateJSONConfig;
var
  RecentFiles: TStringArray;
  i: Integer;
begin
  WriteLn('--- Configuration JSON ---');

  // Créer une configuration JSON portable
  JsonConfig := TJSONConfig.Create('config.json');
  try
    // Définir des valeurs
    JsonConfig.SetValue('app/name', 'Mon Application');
    JsonConfig.SetValue('app/version', '1.0.0');

    JsonConfig.SetValue('user/preferences/theme', 'dark');
    JsonConfig.SetValue('user/preferences/fontSize', 12);
    JsonConfig.SetValue('user/preferences/autoSave', True);

    // Ajouter des éléments à un tableau
    JsonConfig.AddToArray('user/recentFiles', 'document1.txt');
    JsonConfig.AddToArray('user/recentFiles', 'document2.txt');

    // Lire les valeurs
    WriteLn('Thème : ', JsonConfig.GetValue('user/preferences/theme', 'light'));
    WriteLn('Taille police : ', JsonConfig.GetValue('user/preferences/fontSize', 10));

    // Lire un tableau
    RecentFiles := JsonConfig.GetArray('user/recentFiles');
    WriteLn('Fichiers récents :');
    for i := 0 to High(RecentFiles) do
      WriteLn('  - ', RecentFiles[i]);

    // Sauvegarder
    JsonConfig.Save;
  finally
    JsonConfig.Free;
  end;
end;

procedure ShowConfigurationInfo;
var
  Sections: TStringList;
  Keys: TStringList;
  i, j: Integer;
begin
  WriteLn('=== Information de Configuration ===');
  WriteLn('Chemin de configuration : ', Config.ConfigPath);
  WriteLn('Type de stockage : ',
    {$IFDEF WINDOWS}
    'Windows - ' +
    {$ELSE}
    'Linux - ' +
    {$ENDIF}
    case Config.Storage of
      csIniFile: 'Fichier INI';
      csRegistry: 'Registre Windows';
      csJSON: 'Fichier JSON';
      csXML: 'Fichier XML';
    else
      'Inconnu'
    end);

  WriteLn;
  WriteLn('--- Paramètres actuels ---');
  WriteLn('Langue : ', Config.GetValue('General', 'Language', 'non définie'));
  WriteLn('Fenêtre : ',
    Config.GetInteger('Display', 'WindowWidth', 800), 'x',
    Config.GetInteger('Display', 'WindowHeight', 600));
  WriteLn('Plein écran : ', Config.GetBoolean('Display', 'FullScreen', False));

  // Afficher toutes les sections
  Sections := TStringList.Create;
  Keys := TStringList.Create;
  try
    Config.GetSections(Sections);
    WriteLn;
    WriteLn('--- Toutes les sections ---');
    for i := 0 to Sections.Count - 1 do
    begin
      WriteLn('[', Sections[i], ']');
      Config.GetKeys(Sections[i], Keys);
      for j := 0 to Keys.Count - 1 do
        WriteLn('  ', Keys[j], ' = ',
                Config.GetValue(Sections[i], Keys[j], ''));
    end;
  finally
    Sections.Free;
    Keys.Free;
  end;
end;

begin
  try
    InitializeConfiguration;
    ShowConfigurationInfo;

    WriteLn;
    DemonstrateJSONConfig;

    // Sauvegarder la configuration
    Config.Save;

    WriteLn;
    WriteLn('Configuration sauvegardée avec succès');

  finally
    Config.Free;
  end;

  WriteLn;
  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
```

## Bonnes pratiques et conseils

### 1. Organisation de la configuration

Structurez votre configuration de manière logique et hiérarchique :

```
[General]           # Paramètres généraux de l'application
  Language          # Langue de l'interface
  FirstRun          # Premier lancement
  Version           # Version de configuration

[Display]           # Paramètres d'affichage
  WindowX           # Position fenêtre
  WindowY
  WindowWidth       # Dimensions
  WindowHeight
  Maximized         # État de la fenêtre
  Theme             # Thème visuel

[Network]           # Paramètres réseau
  ProxyEnabled      # Configuration proxy
  ProxyHost
  ProxyPort
  Timeout           # Délais réseau

[Advanced]          # Paramètres avancés
  DebugMode         # Mode débogage
  LogLevel          # Niveau de journalisation

[Plugins]           # Configuration des plugins
  Enabled           # Plugins actifs
  Directory         # Répertoire des plugins
```

### 2. Gestion des paramètres sensibles

Pour les données sensibles (mots de passe, clés API), utilisez le chiffrement :

```pascal
unit SecureConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UnifiedConfig, DCPrijndael, DCPsha256;

type
  { TSecureConfig }
  TSecureConfig = class
  private
    FConfig: TUnifiedConfig;
    FMasterKey: string;

    function EncryptString(const Plain: string): string;
    function DecryptString(const Encrypted: string): string;
    function GenerateKeyFromPassword(const Password: string): string;

  public
    constructor Create(Config: TUnifiedConfig; const MasterPassword: string);

    // Méthodes sécurisées
    procedure SetSecureValue(const Section, Key, Value: string);
    function GetSecureValue(const Section, Key, Default: string): string;

    procedure SetCredentials(const Section, Username, Password: string);
    function GetUsername(const Section: string): string;
    function GetPassword(const Section: string): string;
  end;

implementation

uses
  Base64;

constructor TSecureConfig.Create(Config: TUnifiedConfig; const MasterPassword: string);
begin
  inherited Create;
  FConfig := Config;
  FMasterKey := GenerateKeyFromPassword(MasterPassword);
end;

function TSecureConfig.GenerateKeyFromPassword(const Password: string): string;
var
  Hash: TDCP_sha256;
  Digest: array[0..31] of byte;
  i: Integer;
begin
  Hash := TDCP_sha256.Create(nil);
  try
    Hash.Init;
    Hash.UpdateStr(Password);
    Hash.Final(Digest);

    Result := '';
    for i := 0 to 31 do
      Result := Result + IntToHex(Digest[i], 2);
  finally
    Hash.Free;
  end;
end;

function TSecureConfig.EncryptString(const Plain: string): string;
var
  Cipher: TDCP_rijndael;
  Data: string;
begin
  if Plain = '' then
  begin
    Result := '';
    Exit;
  end;

  Cipher := TDCP_rijndael.Create(nil);
  try
    Cipher.InitStr(FMasterKey, TDCP_sha256);
    Data := Cipher.EncryptString(Plain);
    Result := EncodeStringBase64(Data);
  finally
    Cipher.Free;
  end;
end;

function TSecureConfig.DecryptString(const Encrypted: string): string;
var
  Cipher: TDCP_rijndael;
  Data: string;
begin
  if Encrypted = '' then
  begin
    Result := '';
    Exit;
  end;

  try
    Data := DecodeStringBase64(Encrypted);

    Cipher := TDCP_rijndael.Create(nil);
    try
      Cipher.InitStr(FMasterKey, TDCP_sha256);
      Result := Cipher.DecryptString(Data);
    finally
      Cipher.Free;
    end;
  except
    // En cas d'erreur de déchiffrement, retourner une chaîne vide
    Result := '';
  end;
end;

procedure TSecureConfig.SetSecureValue(const Section, Key, Value: string);
var
  Encrypted: string;
begin
  Encrypted := EncryptString(Value);
  FConfig.SetValue(Section, Key, Encrypted);

  // Marquer comme chiffré
  FConfig.SetBoolean(Section, Key + '_Encrypted', True);
end;

function TSecureConfig.GetSecureValue(const Section, Key, Default: string): string;
var
  Encrypted: string;
  IsEncrypted: Boolean;
begin
  IsEncrypted := FConfig.GetBoolean(Section, Key + '_Encrypted', False);

  if IsEncrypted then
  begin
    Encrypted := FConfig.GetValue(Section, Key, '');
    Result := DecryptString(Encrypted);

    // Si le déchiffrement échoue, utiliser la valeur par défaut
    if Result = '' then
      Result := Default;
  end
  else
  begin
    // Non chiffré, lire normalement
    Result := FConfig.GetValue(Section, Key, Default);
  end;
end;

procedure TSecureConfig.SetCredentials(const Section, Username, Password: string);
begin
  FConfig.SetValue(Section, 'Username', Username);  // Username en clair
  SetSecureValue(Section, 'Password', Password);     // Password chiffré
end;

function TSecureConfig.GetUsername(const Section: string): string;
begin
  Result := FConfig.GetValue(Section, 'Username', '');
end;

function TSecureConfig.GetPassword(const Section: string): string;
begin
  Result := GetSecureValue(Section, 'Password', '');
end;

end.
```

### 3. Configuration pour environnements multiples

Gérez différentes configurations selon l'environnement (développement, test, production) :

```pascal
unit MultiEnvConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UnifiedConfig;

type
  TEnvironment = (envDevelopment, envTesting, envStaging, envProduction);

  { TMultiEnvironmentConfig }
  TMultiEnvironmentConfig = class
  private
    FCurrentEnv: TEnvironment;
    FBaseConfig: TUnifiedConfig;
    FEnvConfig: TUnifiedConfig;

    function GetEnvironmentName(Env: TEnvironment): string;
    function DetectEnvironment: TEnvironment;

  public
    constructor Create(const AppName: string);
    destructor Destroy; override;

    // Obtenir une valeur (priorité : env > base)
    function GetValue(const Section, Key, Default: string): string;
    procedure SetValue(const Section, Key, Value: string; ForAllEnv: Boolean = False);

    // Gestion des environnements
    procedure SwitchEnvironment(Env: TEnvironment);
    property CurrentEnvironment: TEnvironment read FCurrentEnv;
  end;

implementation

constructor TMultiEnvironmentConfig.Create(const AppName: string);
begin
  inherited Create;

  // Configuration de base (commune à tous les environnements)
  FBaseConfig := TUnifiedConfig.Create(AppName);

  // Détecter l'environnement actuel
  FCurrentEnv := DetectEnvironment;

  // Charger la configuration spécifique à l'environnement
  FEnvConfig := TUnifiedConfig.Create(AppName + '.' + GetEnvironmentName(FCurrentEnv));
end;

destructor TMultiEnvironmentConfig.Destroy;
begin
  FBaseConfig.Free;
  FEnvConfig.Free;
  inherited Destroy;
end;

function TMultiEnvironmentConfig.DetectEnvironment: TEnvironment;
var
  EnvVar: string;
begin
  // Détection basée sur une variable d'environnement
  EnvVar := GetEnvironmentVariable('APP_ENVIRONMENT');

  if EnvVar = '' then
    EnvVar := GetEnvironmentVariable('NODE_ENV');  // Compatibilité Node.js

  EnvVar := LowerCase(EnvVar);

  if (EnvVar = 'development') or (EnvVar = 'dev') then
    Result := envDevelopment
  else if (EnvVar = 'testing') or (EnvVar = 'test') then
    Result := envTesting
  else if EnvVar = 'staging' then
    Result := envStaging
  else if (EnvVar = 'production') or (EnvVar = 'prod') then
    Result := envProduction
  else
  begin
    // Par défaut : développement en debug, production sinon
    {$IFDEF DEBUG}
      Result := envDevelopment;
    {$ELSE}
      Result := envProduction;
    {$ENDIF}
  end;
end;

function TMultiEnvironmentConfig.GetEnvironmentName(Env: TEnvironment): string;
begin
  case Env of
    envDevelopment: Result := 'dev';
    envTesting:     Result := 'test';
    envStaging:     Result := 'staging';
    envProduction:  Result := 'prod';
  end;
end;

function TMultiEnvironmentConfig.GetValue(const Section, Key, Default: string): string;
begin
  // D'abord chercher dans la config de l'environnement
  Result := FEnvConfig.GetValue(Section, Key, '');

  // Si pas trouvé, chercher dans la config de base
  if Result = '' then
    Result := FBaseConfig.GetValue(Section, Key, Default);
end;

procedure TMultiEnvironmentConfig.SetValue(const Section, Key, Value: string;
  ForAllEnv: Boolean);
begin
  if ForAllEnv then
    FBaseConfig.SetValue(Section, Key, Value)  // Config commune
  else
    FEnvConfig.SetValue(Section, Key, Value);   // Config spécifique
end;

procedure TMultiEnvironmentConfig.SwitchEnvironment(Env: TEnvironment);
begin
  if Env = FCurrentEnv then
    Exit;

  // Sauvegarder la configuration actuelle
  FEnvConfig.Save;

  // Libérer et recharger avec le nouvel environnement
  FEnvConfig.Free;
  FCurrentEnv := Env;
  FEnvConfig := TUnifiedConfig.Create(
    ExtractFileName(FBaseConfig.ConfigPath) + '.' + GetEnvironmentName(FCurrentEnv)
  );
end;

end.
```

### 4. Surveillance des modifications de configuration

Implémentez un système pour détecter et réagir aux changements de configuration :

```pascal
unit ConfigWatcher;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UnifiedConfig, fptimer;

type
  TConfigChangeEvent = procedure(Sender: TObject; const Section, Key: string) of object;

  { TConfigWatcher }
  TConfigWatcher = class
  private
    FConfig: TUnifiedConfig;
    FTimer: TFPTimer;
    FLastModified: TDateTime;
    FOnChange: TConfigChangeEvent;
    FSnapshot: TStringList;

    procedure TimerTick(Sender: TObject);
    procedure TakeSnapshot;
    function CompareSnapshots: Boolean;
    function GetFileModificationTime: TDateTime;

  public
    constructor Create(Config: TUnifiedConfig; CheckInterval: Integer = 1000);
    destructor Destroy; override;

    procedure Start;
    procedure Stop;

    property OnChange: TConfigChangeEvent read FOnChange write FOnChange;
  end;

implementation

uses
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  {$IFDEF UNIX}
  BaseUnix, Unix,
  {$ENDIF}
  DateUtils;

constructor TConfigWatcher.Create(Config: TUnifiedConfig; CheckInterval: Integer);
begin
  inherited Create;

  FConfig := Config;
  FSnapshot := TStringList.Create;

  FTimer := TFPTimer.Create(nil);
  FTimer.Interval := CheckInterval;
  FTimer.OnTimer := @TimerTick;
  FTimer.Enabled := False;

  TakeSnapshot;
  FLastModified := GetFileModificationTime;
end;

destructor TConfigWatcher.Destroy;
begin
  Stop;
  FTimer.Free;
  FSnapshot.Free;
  inherited Destroy;
end;

procedure TConfigWatcher.Start;
begin
  FTimer.Enabled := True;
end;

procedure TConfigWatcher.Stop;
begin
  FTimer.Enabled := False;
end;

function TConfigWatcher.GetFileModificationTime: TDateTime;
var
  ConfigFile: string;
  {$IFDEF WINDOWS}
  SearchRec: TSearchRec;
  {$ENDIF}
  {$IFDEF UNIX}
  Info: Stat;
  {$ENDIF}
begin
  Result := 0;

  // Ne fonctionne que pour les configurations basées sur fichiers
  if FConfig.Storage = csRegistry then
    Exit;

  ConfigFile := FConfig.ConfigPath + PathDelim + 'config.ini';

  if not FileExists(ConfigFile) then
    Exit;

  {$IFDEF WINDOWS}
  if FindFirst(ConfigFile, faAnyFile, SearchRec) = 0 then
  begin
    Result := FileDateToDateTime(SearchRec.Time);
    FindClose(SearchRec);
  end;
  {$ENDIF}

  {$IFDEF UNIX}
  if FpStat(ConfigFile, Info) = 0 then
  begin
    Result := UnixToDateTime(Info.st_mtime);
  end;
  {$ENDIF}
end;

procedure TConfigWatcher.TakeSnapshot;
var
  Sections, Keys: TStringList;
  i, j: Integer;
  Value: string;
begin
  FSnapshot.Clear;

  Sections := TStringList.Create;
  Keys := TStringList.Create;
  try
    FConfig.GetSections(Sections);

    for i := 0 to Sections.Count - 1 do
    begin
      FConfig.GetKeys(Sections[i], Keys);

      for j := 0 to Keys.Count - 1 do
      begin
        Value := FConfig.GetValue(Sections[i], Keys[j], '');
        FSnapshot.Add(Format('%s/%s=%s', [Sections[i], Keys[j], Value]));
      end;
    end;

    FSnapshot.Sort;
  finally
    Sections.Free;
    Keys.Free;
  end;
end;

function TConfigWatcher.CompareSnapshots: Boolean;
var
  NewSnapshot: TStringList;
  i: Integer;
  OldValue, NewValue: string;
  Parts: TStringArray;
begin
  Result := False;

  NewSnapshot := TStringList.Create;
  try
    // Sauvegarder l'ancien snapshot
    NewSnapshot.Assign(FSnapshot);

    // Prendre un nouveau snapshot
    TakeSnapshot;

    // Comparer
    if NewSnapshot.Count <> FSnapshot.Count then
    begin
      Result := True;
    end
    else
    begin
      for i := 0 to FSnapshot.Count - 1 do
      begin
        if NewSnapshot[i] <> FSnapshot[i] then
        begin
          Result := True;

          // Notifier du changement spécifique
          if Assigned(FOnChange) then
          begin
            Parts := FSnapshot[i].Split(['/', '=']);
            if Length(Parts) >= 2 then
              FOnChange(Self, Parts[0], Parts[1]);
          end;
        end;
      end;
    end;
  finally
    NewSnapshot.Free;
  end;
end;

procedure TConfigWatcher.TimerTick(Sender: TObject);
var
  CurrentModified: TDateTime;
begin
  CurrentModified := GetFileModificationTime;

  // Vérifier si le fichier a été modifié
  if (CurrentModified > 0) and (CurrentModified <> FLastModified) then
  begin
    FLastModified := CurrentModified;

    // Recharger la configuration
    FConfig.Reload;

    // Comparer et notifier des changements
    if CompareSnapshots and Assigned(FOnChange) then
    begin
      FOnChange(Self, '', '');  // Notification générale
    end;
  end;
end;

end.
```

### 5. Import/Export de configuration

Permettez aux utilisateurs d'importer et exporter leurs paramètres :

```pascal
unit ConfigPortability;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UnifiedConfig, fpjson, zipper;

type
  { TConfigExporter }
  TConfigExporter = class
  private
    FConfig: TUnifiedConfig;

  public
    constructor Create(Config: TUnifiedConfig);

    // Export simple
    function ExportToINI(const FileName: string): Boolean;
    function ExportToJSON(const FileName: string): Boolean;
    function ExportToXML(const FileName: string): Boolean;

    // Export complet avec métadonnées
    function ExportBundle(const FileName: string;
                         IncludeMetadata: Boolean = True;
                         IncludeBackups: Boolean = False): Boolean;

    // Import
    function ImportFromINI(const FileName: string): Boolean;
    function ImportFromJSON(const FileName: string): Boolean;
    function ImportBundle(const FileName: string): Boolean;

    // Comparaison
    function CompareWithFile(const FileName: string): TStringList;
  end;

implementation

uses
  DOM, XMLWrite, XMLRead, jsonparser, IniFiles, DateUtils;

constructor TConfigExporter.Create(Config: TUnifiedConfig);
begin
  inherited Create;
  FConfig := Config;
end;

function TConfigExporter.ExportToINI(const FileName: string): Boolean;
var
  Ini: TIniFile;
  Sections, Keys: TStringList;
  i, j: Integer;
begin
  Result := False;

  Sections := TStringList.Create;
  Keys := TStringList.Create;
  Ini := TIniFile.Create(FileName);
  try
    FConfig.GetSections(Sections);

    for i := 0 to Sections.Count - 1 do
    begin
      FConfig.GetKeys(Sections[i], Keys);

      for j := 0 to Keys.Count - 1 do
      begin
        Ini.WriteString(Sections[i], Keys[j],
          FConfig.GetValue(Sections[i], Keys[j], ''));
      end;
    end;

    Result := True;
  finally
    Ini.Free;
    Sections.Free;
    Keys.Free;
  end;
end;

function TConfigExporter.ExportToJSON(const FileName: string): Boolean;
var
  Root: TJSONObject;
  SectionObj: TJSONObject;
  Sections, Keys: TStringList;
  i, j: Integer;
  Stream: TFileStream;
  JSONStr: string;
begin
  Result := False;

  Root := TJSONObject.Create;
  Sections := TStringList.Create;
  Keys := TStringList.Create;
  try
    // Ajouter les métadonnées
    Root.Add('metadata', TJSONObject.Create);
    TJSONObject(Root.Find('metadata')).Add('exported',
      FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));
    TJSONObject(Root.Find('metadata')).Add('application',
      ExtractFileName(ParamStr(0)));
    TJSONObject(Root.Find('metadata')).Add('platform',
      {$IFDEF WINDOWS}'Windows'{$ELSE}'Linux'{$ENDIF});

    // Ajouter les données de configuration
    Root.Add('configuration', TJSONObject.Create);

    FConfig.GetSections(Sections);

    for i := 0 to Sections.Count - 1 do
    begin
      SectionObj := TJSONObject.Create;
      FConfig.GetKeys(Sections[i], Keys);

      for j := 0 to Keys.Count - 1 do
      begin
        SectionObj.Add(Keys[j],
          FConfig.GetValue(Sections[i], Keys[j], ''));
      end;

      TJSONObject(Root.Find('configuration')).Add(Sections[i], SectionObj);
    end;

    // Écrire le fichier
    JSONStr := Root.FormatJSON();

    Stream := TFileStream.Create(FileName, fmCreate);
    try
      Stream.WriteBuffer(JSONStr[1], Length(JSONStr));
      Result := True;
    finally
      Stream.Free;
    end;
  finally
    Root.Free;
    Sections.Free;
    Keys.Free;
  end;
end;

function TConfigExporter.ExportToXML(const FileName: string): Boolean;
var
  Doc: TXMLDocument;
  Root, SectionNode, KeyNode: TDOMNode;
  Sections, Keys: TStringList;
  i, j: Integer;
begin
  Result := False;

  Doc := TXMLDocument.Create;
  Sections := TStringList.Create;
  Keys := TStringList.Create;
  try
    // Créer la structure XML
    Root := Doc.CreateElement('configuration');
    Doc.AppendChild(Root);

    // Ajouter les métadonnées
    TDOMElement(Root).SetAttribute('exported',
      FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));
    TDOMElement(Root).SetAttribute('platform',
      {$IFDEF WINDOWS}'Windows'{$ELSE}'Linux'{$ENDIF});

    FConfig.GetSections(Sections);

    for i := 0 to Sections.Count - 1 do
    begin
      SectionNode := Doc.CreateElement('section');
      TDOMElement(SectionNode).SetAttribute('name', Sections[i]);
      Root.AppendChild(SectionNode);

      FConfig.GetKeys(Sections[i], Keys);

      for j := 0 to Keys.Count - 1 do
      begin
        KeyNode := Doc.CreateElement('key');
        TDOMElement(KeyNode).SetAttribute('name', Keys[j]);
        TDOMElement(KeyNode).SetAttribute('value',
          FConfig.GetValue(Sections[i], Keys[j], ''));
        SectionNode.AppendChild(KeyNode);
      end;
    end;

    // Sauvegarder le document
    WriteXMLFile(Doc, FileName);
    Result := True;
  finally
    Doc.Free;
    Sections.Free;
    Keys.Free;
  end;
end;

function TConfigExporter.ExportBundle(const FileName: string;
  IncludeMetadata: Boolean; IncludeBackups: Boolean): Boolean;
var
  Zip: TZipper;
  TempDir: string;
  ConfigFile, MetaFile: string;
  Meta: TStringList;
begin
  Result := False;

  TempDir := GetTempDir + 'config_export_' +
             FormatDateTime('yyyymmddhhnnss', Now) + PathDelim;
  ForceDirectories(TempDir);

  try
    // Exporter la configuration principale
    ConfigFile := TempDir + 'config.json';
    if not ExportToJSON(ConfigFile) then
      Exit;

    // Créer le fichier de métadonnées si demandé
    if IncludeMetadata then
    begin
      MetaFile := TempDir + 'metadata.txt';
      Meta := TStringList.Create;
      try
        Meta.Add('Export Configuration Bundle');
        Meta.Add('========================');
        Meta.Add('Date: ' + DateTimeToStr(Now));
        Meta.Add('Platform: ' + {$IFDEF WINDOWS}'Windows'{$ELSE}'Linux'{$ENDIF});
        Meta.Add('Application: ' + ExtractFileName(ParamStr(0)));
        Meta.Add('Config Version: ' + FConfig.GetValue('Meta', 'ConfigVersion', '1.0.0'));
        Meta.SaveToFile(MetaFile);
      finally
        Meta.Free;
      end;
    end;

    // Créer l'archive ZIP
    Zip := TZipper.Create;
    try
      Zip.FileName := FileName;
      Zip.Entries.AddFileEntry(ConfigFile, 'config.json');

      if IncludeMetadata and FileExists(MetaFile) then
        Zip.Entries.AddFileEntry(MetaFile, 'metadata.txt');

      Zip.ZipAllFiles;
      Result := True;
    finally
      Zip.Free;
    end;

  finally
    // Nettoyer les fichiers temporaires
    if FileExists(ConfigFile) then
      DeleteFile(ConfigFile);
    if FileExists(MetaFile) then
      DeleteFile(MetaFile);
    RemoveDir(TempDir);
  end;
end;

function TConfigExporter.ImportFromINI(const FileName: string): Boolean;
var
  Ini: TIniFile;
  Sections, Keys: TStringList;
  i, j: Integer;
begin
  Result := False;

  if not FileExists(FileName) then
    Exit;

  // Faire une sauvegarde avant import
  FConfig.BackupConfig(FConfig.ConfigPath + '.before_import');

  Ini := TIniFile.Create(FileName);
  Sections := TStringList.Create;
  Keys := TStringList.Create;
  try
    Ini.ReadSections(Sections);

    for i := 0 to Sections.Count - 1 do
    begin
      Ini.ReadSection(Sections[i], Keys);

      for j := 0 to Keys.Count - 1 do
      begin
        FConfig.SetValue(Sections[i], Keys[j],
          Ini.ReadString(Sections[i], Keys[j], ''));
      end;
    end;

    FConfig.Save;
    Result := True;
  finally
    Ini.Free;
    Sections.Free;
    Keys.Free;
  end;
end;

function TConfigExporter.ImportFromJSON(const FileName: string): Boolean;
var
  Stream: TFileStream;
  Parser: TJSONParser;
  Root, Config, Section: TJSONObject;
  Enum: TJSONEnum;
  KeyEnum: TJSONEnum;
begin
  Result := False;

  if not FileExists(FileName) then
    Exit;

  Stream := TFileStream.Create(FileName, fmOpenRead);
  try
    Parser := TJSONParser.Create(Stream);
    try
      Root := Parser.Parse as TJSONObject;
      try
        // Chercher la section configuration
        Config := Root.Find('configuration') as TJSONObject;
        if Config = nil then
          Config := Root;  // Essayer la racine directement

        // Parcourir les sections
        for Enum in Config do
        begin
          if Enum.Value is TJSONObject then
          begin
            Section := Enum.Value as TJSONObject;

            // Parcourir les clés
            for KeyEnum in Section do
            begin
              FConfig.SetValue(Enum.Key, KeyEnum.Key,
                KeyEnum.Value.AsString);
            end;
          end;
        end;

        FConfig.Save;
        Result := True;
      finally
        Root.Free;
      end;
    finally
      Parser.Free;
    end;
  finally
    Stream.Free;
  end;
end;

function TConfigExporter.ImportBundle(const FileName: string): Boolean;
var
  UnZip: TUnZipper;
  TempDir: string;
  ConfigFile: string;
begin
  Result := False;

  if not FileExists(FileName) then
    Exit;

  TempDir := GetTempDir + 'config_import_' +
             FormatDateTime('yyyymmddhhnnss', Now) + PathDelim;
  ForceDirectories(TempDir);

  try
    // Extraire l'archive
    UnZip := TUnZipper.Create;
    try
      UnZip.FileName := FileName;
      UnZip.OutputPath := TempDir;
      UnZip.UnZipAllFiles;
    finally
      UnZip.Free;
    end;

    // Importer la configuration
    ConfigFile := TempDir + 'config.json';
    if FileExists(ConfigFile) then
      Result := ImportFromJSON(ConfigFile)
    else
    begin
      ConfigFile := TempDir + 'config.ini';
      if FileExists(ConfigFile) then
        Result := ImportFromINI(ConfigFile);
    end;

  finally
    // Nettoyer
    if DirectoryExists(TempDir) then
      RemoveDir(TempDir);
  end;
end;

function TConfigExporter.CompareWithFile(const FileName: string): TStringList;
var
  TempConfig: TUnifiedConfig;
  Exporter: TConfigExporter;
  Sections, Keys: TStringList;
  i, j: Integer;
  Value1, Value2: string;
begin
  Result := TStringList.Create;

  // Créer une configuration temporaire
  TempConfig := TUnifiedConfig.Create('temp_compare');
  Exporter := TConfigExporter.Create(TempConfig);
  Sections := TStringList.Create;
  Keys := TStringList.Create;
  try
    // Importer le fichier à comparer
    if not Exporter.ImportFromJSON(FileName) then
      if not Exporter.ImportFromINI(FileName) then
      begin
        Result.Add('Erreur : Impossible de lire le fichier');
        Exit;
      end;

    // Comparer les configurations
    FConfig.GetSections(Sections);

    for i := 0 to Sections.Count - 1 do
    begin
      FConfig.GetKeys(Sections[i], Keys);

      for j := 0 to Keys.Count - 1 do
      begin
        Value1 := FConfig.GetValue(Sections[i], Keys[j], '');
        Value2 := TempConfig.GetValue(Sections[i], Keys[j], '');

        if Value1 <> Value2 then
        begin
          Result.Add(Format('[%s] %s : "%s" → "%s"',
            [Sections[i], Keys[j], Value1, Value2]));
        end;
      end;
    end;

    // Vérifier les clés présentes dans le fichier mais pas dans la config actuelle
    TempConfig.GetSections(Sections);

    for i := 0 to Sections.Count - 1 do
    begin
      TempConfig.GetKeys(Sections[i], Keys);

      for j := 0 to Keys.Count - 1 do
      begin
        Value1 := FConfig.GetValue(Sections[i], Keys[j], '###NOT_FOUND###');

        if Value1 = '###NOT_FOUND###' then
        begin
          Value2 := TempConfig.GetValue(Sections[i], Keys[j], '');
          Result.Add(Format('[%s] %s : (nouveau) → "%s"',
            [Sections[i], Keys[j], Value2]));
        end;
      end;
    end;

  finally
    Exporter.Free;
    TempConfig.Free;
    Sections.Free;
    Keys.Free;
  end;
end;

end.
```

## Gestion de la configuration dans une application GUI

Voici un exemple complet d'intégration dans une application Lazarus avec interface graphique :

```pascal
unit ConfigForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, Grids, Buttons, UnifiedConfig, ConfigValidator,
  ConfigWatcher, SecureConfig, ConfigPortability;

type
  { TFormConfig }
  TFormConfig = class(TForm)
    PageControl: TPageControl;
    TabGeneral: TTabSheet;
    TabDisplay: TTabSheet;
    TabNetwork: TTabSheet;
    TabAdvanced: TTabSheet;
    TabImportExport: TTabSheet;

    // Onglet Général
    GroupLanguage: TGroupBox;
    ComboLanguage: TComboBox;
    CheckAutoStart: TCheckBox;
    CheckAutoUpdate: TCheckBox;

    // Onglet Affichage
    GroupWindow: TGroupBox;
    SpinWindowWidth: TSpinEdit;
    SpinWindowHeight: TSpinEdit;
    CheckFullScreen: TCheckBox;
    ComboTheme: TComboBox;
    CheckDarkMode: TCheckBox;

    // Onglet Réseau
    GroupProxy: TGroupBox;
    CheckUseProxy: TCheckBox;
    EditProxyHost: TEdit;
    SpinProxyPort: TSpinEdit;
    EditProxyUser: TEdit;
    EditProxyPassword: TEdit;

    // Onglet Avancé
    CheckDebugMode: TCheckBox;
    ComboLogLevel: TComboBox;
    EditConfigPath: TEdit;
    BtnBrowseConfig: TButton;
    MemoConfigRaw: TMemo;

    // Onglet Import/Export
    BtnExportINI: TButton;
    BtnExportJSON: TButton;
    BtnExportBundle: TButton;
    BtnImport: TButton;
    BtnCompare: TButton;
    ListCompareResults: TListBox;

    // Boutons principaux
    PanelButtons: TPanel;
    BtnOK: TBitBtn;
    BtnCancel: TBitBtn;
    BtnApply: TBitBtn;

    // Barre d'état
    StatusBar: TStatusBar;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);

    procedure BtnOKClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnApplyClick(Sender: TObject);

    procedure CheckUseProxyChange(Sender: TObject);
    procedure BtnBrowseConfigClick(Sender: TObject);

    procedure BtnExportINIClick(Sender: TObject);
    procedure BtnExportJSONClick(Sender: TObject);
    procedure BtnExportBundleClick(Sender: TObject);
    procedure BtnImportClick(Sender: TObject);
    procedure BtnCompareClick(Sender: TObject);

    procedure ConfigChanged(Sender: TObject; const Section, Key: string);

  private
    FConfig: TUnifiedConfig;
    FValidator: TConfigValidator;
    FWatcher: TConfigWatcher;
    FSecureConfig: TSecureConfig;
    FExporter: TConfigExporter;
    FModified: Boolean;

    procedure LoadConfiguration;
    procedure SaveConfiguration;
    procedure ValidateConfiguration;
    procedure UpdateProxyControls;
    procedure SetModified(Value: Boolean);

  public
    property Modified: Boolean read FModified write SetModified;
  end;

var
  FormConfig: TFormConfig;

implementation

{$R *.lfm}

uses
  FileUtil, LCLType;

{ TFormConfig }

procedure TFormConfig.FormCreate(Sender: TObject);
var
  MasterPassword: string;
begin
  // Créer les objets de configuration
  FConfig := TUnifiedConfig.Create('MonApplication');
  FValidator := TConfigValidator.Create;
  FExporter := TConfigExporter.Create(FConfig);

  // Demander le mot de passe principal pour la configuration sécurisée
  MasterPassword := InputBox('Sécurité',
    'Entrez le mot de passe principal :', '');
  if MasterPassword <> '' then
    FSecureConfig := TSecureConfig.Create(FConfig, MasterPassword)
  else
    FSecureConfig := nil;

  // Créer le surveillant de configuration
  FWatcher := TConfigWatcher.Create(FConfig);
  FWatcher.OnChange := @ConfigChanged;
  FWatcher.Start;

  // Configurer le validateur
  InitializeValidator;

  // Initialiser les contrôles
  InitializeControls;

  FModified := False;
end;

procedure TFormConfig.FormDestroy(Sender: TObject);
begin
  FWatcher.Stop;
  FWatcher.Free;
  FExporter.Free;
  FSecureConfig.Free;
  FValidator.Free;
  FConfig.Free;
end;

procedure TFormConfig.FormShow(Sender: TObject);
begin
  LoadConfiguration;
  UpdateProxyControls;
end;

procedure TFormConfig.InitializeValidator;
begin
  // Définir les règles de validation
  FValidator.AddEnumRule('General', 'Language',
    ['fr-FR', 'en-US', 'de-DE', 'es-ES'], True, 'en-US');

  FValidator.AddIntegerRule('Display', 'WindowWidth',
    640, 3840, False, 1024);
  FValidator.AddIntegerRule('Display', 'WindowHeight',
    480, 2160, False, 768);

  FValidator.AddRule('Display', 'Theme', cdtEnum, False, 'default');

  FValidator.AddIntegerRule('Network', 'ProxyPort',
    1, 65535, False, 8080);

  FValidator.AddEnumRule('Advanced', 'LogLevel',
    ['Debug', 'Info', 'Warning', 'Error', 'Fatal'], False, 'Info');
end;

procedure TFormConfig.InitializeControls;
begin
  // Remplir les listes déroulantes
  ComboLanguage.Items.Clear;
  ComboLanguage.Items.Add('Français (fr-FR)');
  ComboLanguage.Items.Add('English (en-US)');
  ComboLanguage.Items.Add('Deutsch (de-DE)');
  ComboLanguage.Items.Add('Español (es-ES)');

  ComboTheme.Items.Clear;
  ComboTheme.Items.Add('Default');
  ComboTheme.Items.Add('Light');
  ComboTheme.Items.Add('Dark');
  ComboTheme.Items.Add('Blue');

  ComboLogLevel.Items.Clear;
  ComboLogLevel.Items.Add('Debug');
  ComboLogLevel.Items.Add('Info');
  ComboLogLevel.Items.Add('Warning');
  ComboLogLevel.Items.Add('Error');
  ComboLogLevel.Items.Add('Fatal');

  // Configurer les limites
  SpinWindowWidth.MinValue := 640;
  SpinWindowWidth.MaxValue := 3840;
  SpinWindowHeight.MinValue := 480;
  SpinWindowHeight.MaxValue := 2160;
  SpinProxyPort.MinValue := 1;
  SpinProxyPort.MaxValue := 65535;

  // État initial
  EditConfigPath.Text := FConfig.ConfigPath;
end;

procedure TFormConfig.LoadConfiguration;
var
  Lang: string;
  Theme: string;
  LogLevel: string;
begin
  // Charger les paramètres généraux
  Lang := FConfig.GetValue('General', 'Language', 'en-US');
  if Lang = 'fr-FR' then
    ComboLanguage.ItemIndex := 0
  else if Lang = 'en-US' then
    ComboLanguage.ItemIndex := 1
  else if Lang = 'de-DE' then
    ComboLanguage.ItemIndex := 2
  else if Lang = 'es-ES' then
    ComboLanguage.ItemIndex := 3
  else
    ComboLanguage.ItemIndex := 1;

  CheckAutoStart.Checked := FConfig.GetBoolean('General', 'AutoStart', False);
  CheckAutoUpdate.Checked := FConfig.GetBoolean('General', 'AutoUpdate', True);

  // Charger les paramètres d'affichage
  SpinWindowWidth.Value := FConfig.GetInteger('Display', 'WindowWidth', 1024);
  SpinWindowHeight.Value := FConfig.GetInteger('Display', 'WindowHeight', 768);
  CheckFullScreen.Checked := FConfig.GetBoolean('Display', 'FullScreen', False);

  Theme := FConfig.GetValue('Display', 'Theme', 'Default');
  ComboTheme.ItemIndex := ComboTheme.Items.IndexOf(Theme);
  if ComboTheme.ItemIndex = -1 then
    ComboTheme.ItemIndex := 0;

  CheckDarkMode.Checked := FConfig.GetBoolean('Display', 'DarkMode', False);

  // Charger les paramètres réseau
  CheckUseProxy.Checked := FConfig.GetBoolean('Network', 'UseProxy', False);
  EditProxyHost.Text := FConfig.GetValue('Network', 'ProxyHost', '');
  SpinProxyPort.Value := FConfig.GetInteger('Network', 'ProxyPort', 8080);

  // Utiliser la configuration sécurisée pour les identifiants
  if Assigned(FSecureConfig) then
  begin
    EditProxyUser.Text := FSecureConfig.GetUsername('Network');
    EditProxyPassword.Text := FSecureConfig.GetPassword('Network');
  end;

  // Charger les paramètres avancés
  CheckDebugMode.Checked := FConfig.GetBoolean('Advanced', 'DebugMode', False);

  LogLevel := FConfig.GetValue('Advanced', 'LogLevel', 'Info');
  ComboLogLevel.ItemIndex := ComboLogLevel.Items.IndexOf(LogLevel);
  if ComboLogLevel.ItemIndex = -1 then
    ComboLogLevel.ItemIndex := 1;

  // Afficher la configuration brute
  LoadRawConfiguration;
end;

procedure TFormConfig.LoadRawConfiguration;
var
  Sections, Keys: TStringList;
  i, j: Integer;
begin
  MemoConfigRaw.Clear;

  Sections := TStringList.Create;
  Keys := TStringList.Create;
  try
    FConfig.GetSections(Sections);

    for i := 0 to Sections.Count - 1 do
    begin
      MemoConfigRaw.Lines.Add('[' + Sections[i] + ']');

      FConfig.GetKeys(Sections[i], Keys);
      for j := 0 to Keys.Count - 1 do
      begin
        MemoConfigRaw.Lines.Add(Keys[j] + '=' +
          FConfig.GetValue(Sections[i], Keys[j], ''));
      end;

      MemoConfigRaw.Lines.Add('');
    end;
  finally
    Sections.Free;
    Keys.Free;
  end;
end;

procedure TFormConfig.SaveConfiguration;
begin
  // Sauvegarder les paramètres généraux
  case ComboLanguage.ItemIndex of
    0: FConfig.SetValue('General', 'Language', 'fr-FR');
    1: FConfig.SetValue('General', 'Language', 'en-US');
    2: FConfig.SetValue('General', 'Language', 'de-DE');
    3: FConfig.SetValue('General', 'Language', 'es-ES');
  end;

  FConfig.SetBoolean('General', 'AutoStart', CheckAutoStart.Checked);
  FConfig.SetBoolean('General', 'AutoUpdate', CheckAutoUpdate.Checked);

  // Sauvegarder les paramètres d'affichage
  FConfig.SetInteger('Display', 'WindowWidth', SpinWindowWidth.Value);
  FConfig.SetInteger('Display', 'WindowHeight', SpinWindowHeight.Value);
  FConfig.SetBoolean('Display', 'FullScreen', CheckFullScreen.Checked);

  if ComboTheme.ItemIndex >= 0 then
    FConfig.SetValue('Display', 'Theme', ComboTheme.Items[ComboTheme.ItemIndex]);

  FConfig.SetBoolean('Display', 'DarkMode', CheckDarkMode.Checked);

  // Sauvegarder les paramètres réseau
  FConfig.SetBoolean('Network', 'UseProxy', CheckUseProxy.Checked);
  FConfig.SetValue('Network', 'ProxyHost', EditProxyHost.Text);
  FConfig.SetInteger('Network', 'ProxyPort', SpinProxyPort.Value);

  // Utiliser la configuration sécurisée pour les identifiants
  if Assigned(FSecureConfig) then
  begin
    FSecureConfig.SetCredentials('Network',
      EditProxyUser.Text, EditProxyPassword.Text);
  end;

  // Sauvegarder les paramètres avancés
  FConfig.SetBoolean('Advanced', 'DebugMode', CheckDebugMode.Checked);

  if ComboLogLevel.ItemIndex >= 0 then
    FConfig.SetValue('Advanced', 'LogLevel',
      ComboLogLevel.Items[ComboLogLevel.ItemIndex]);

  // Forcer l'écriture
  FConfig.Save;
end;

procedure TFormConfig.ValidateConfiguration;
var
  Errors: TStringList;
  ErrorMsg: string;
  i: Integer;
begin
  // Appliquer les valeurs par défaut si nécessaire
  FValidator.ApplyDefaults(FConfig);

  // Valider la configuration
  if not FValidator.Validate(FConfig) then
  begin
    Errors := FValidator.GetErrors;
    ErrorMsg := 'Erreurs de configuration détectées :' + #13#10;

    for i := 0 to Errors.Count - 1 do
      ErrorMsg := ErrorMsg + '• ' + Errors[i] + #13#10;

    MessageDlg('Validation', ErrorMsg, mtWarning, [mbOK], 0);
  end;
end;

procedure TFormConfig.UpdateProxyControls;
begin
  EditProxyHost.Enabled := CheckUseProxy.Checked;
  SpinProxyPort.Enabled := CheckUseProxy.Checked;
  EditProxyUser.Enabled := CheckUseProxy.Checked;
  EditProxyPassword.Enabled := CheckUseProxy.Checked;
end;

procedure TFormConfig.SetModified(Value: Boolean);
begin
  FModified := Value;
  BtnApply.Enabled := FModified;

  if FModified then
    StatusBar.SimpleText := 'Configuration modifiée'
  else
    StatusBar.SimpleText := '';
end;

procedure TFormConfig.BtnOKClick(Sender: TObject);
begin
  SaveConfiguration;
  ValidateConfiguration;
  ModalResult := mrOK;
end;

procedure TFormConfig.BtnCancelClick(Sender: TObject);
begin
  if FModified then
  begin
    if MessageDlg('Confirmation',
      'Des modifications n''ont pas été sauvegardées. Voulez-vous vraiment annuler ?',
      mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      ModalResult := mrCancel;
  end
  else
    ModalResult := mrCancel;
end;

procedure TFormConfig.BtnApplyClick(Sender: TObject);
begin
  SaveConfiguration;
  ValidateConfiguration;
  SetModified(False);
  LoadRawConfiguration;

  StatusBar.SimpleText := 'Configuration appliquée';
end;

procedure TFormConfig.CheckUseProxyChange(Sender: TObject);
begin
  UpdateProxyControls;
  SetModified(True);
end;

procedure TFormConfig.BtnBrowseConfigClick(Sender: TObject);
var
  Dir: string;
begin
  Dir := FConfig.ConfigPath;
  if SelectDirectory('Sélectionner le répertoire de configuration', '', Dir) then
  begin
    EditConfigPath.Text := Dir;
    SetModified(True);
  end;
end;

procedure TFormConfig.BtnExportINIClick(Sender: TObject);
var
  SaveDialog: TSaveDialog;
begin
  SaveDialog := TSaveDialog.Create(nil);
  try
    SaveDialog.Filter := 'Fichiers INI|*.ini|Tous les fichiers|*.*';
    SaveDialog.DefaultExt := 'ini';
    SaveDialog.FileName := 'config_export.ini';

    if SaveDialog.Execute then
    begin
      if FExporter.ExportToINI(SaveDialog.FileName) then
        ShowMessage('Configuration exportée avec succès vers ' + SaveDialog.FileName)
      else
        ShowMessage('Erreur lors de l''export');
    end;
  finally
    SaveDialog.Free;
  end;
end;

procedure TFormConfig.BtnExportJSONClick(Sender: TObject);
var
  SaveDialog: TSaveDialog;
begin
  SaveDialog := TSaveDialog.Create(nil);
  try
    SaveDialog.Filter := 'Fichiers JSON|*.json|Tous les fichiers|*.*';
    SaveDialog.DefaultExt := 'json';
    SaveDialog.FileName := 'config_export.json';

    if SaveDialog.Execute then
    begin
      if FExporter.ExportToJSON(SaveDialog.FileName) then
        ShowMessage('Configuration exportée avec succès vers ' + SaveDialog.FileName)
      else
        ShowMessage('Erreur lors de l''export');
    end;
  finally
    SaveDialog.Free;
  end;
end;

procedure TFormConfig.BtnExportBundleClick(Sender: TObject);
var
  SaveDialog: TSaveDialog;
begin
  SaveDialog := TSaveDialog.Create(nil);
  try
    SaveDialog.Filter := 'Archive ZIP|*.zip|Tous les fichiers|*.*';
    SaveDialog.DefaultExt := 'zip';
    SaveDialog.FileName := 'config_bundle.zip';

    if SaveDialog.Execute then
    begin
      if FExporter.ExportBundle(SaveDialog.FileName, True, False) then
        ShowMessage('Bundle de configuration créé avec succès : ' + SaveDialog.FileName)
      else
        ShowMessage('Erreur lors de la création du bundle');
    end;
  finally
    SaveDialog.Free;
  end;
end;

procedure TFormConfig.BtnImportClick(Sender: TObject);
var
  OpenDialog: TOpenDialog;
  Success: Boolean;
  Ext: string;
begin
  OpenDialog := TOpenDialog.Create(nil);
  try
    OpenDialog.Filter := 'Fichiers de config|*.ini;*.json;*.zip|' +
                        'Fichiers INI|*.ini|' +
                        'Fichiers JSON|*.json|' +
                        'Archives ZIP|*.zip|' +
                        'Tous les fichiers|*.*';

    if OpenDialog.Execute then
    begin
      // Faire une sauvegarde avant import
      FConfig.BackupConfig(FConfig.ConfigPath + '.before_import');

      Success := False;
      Ext := LowerCase(ExtractFileExt(OpenDialog.FileName));

      if Ext = '.ini' then
        Success := FExporter.ImportFromINI(OpenDialog.FileName)
      else if Ext = '.json' then
        Success := FExporter.ImportFromJSON(OpenDialog.FileName)
      else if Ext = '.zip' then
        Success := FExporter.ImportBundle(OpenDialog.FileName);

      if Success then
      begin
        LoadConfiguration;
        ShowMessage('Configuration importée avec succès');
      end
      else
        ShowMessage('Erreur lors de l''import');
    end;
  finally
    OpenDialog.Free;
  end;
end;

procedure TFormConfig.BtnCompareClick(Sender: TObject);
var
  OpenDialog: TOpenDialog;
  Differences: TStringList;
begin
  OpenDialog := TOpenDialog.Create(nil);
  try
    OpenDialog.Filter := 'Fichiers de config|*.ini;*.json|' +
                        'Tous les fichiers|*.*';

    if OpenDialog.Execute then
    begin
      Differences := FExporter.CompareWithFile(OpenDialog.FileName);
      try
        ListCompareResults.Clear;

        if Differences.Count = 0 then
          ListCompareResults.Items.Add('Les configurations sont identiques')
        else
        begin
          ListCompareResults.Items.Add('Différences trouvées :');
          ListCompareResults.Items.AddStrings(Differences);
        end;
      finally
        Differences.Free;
      end;
    end;
  finally
    OpenDialog.Free;
  end;
end;

procedure TFormConfig.ConfigChanged(Sender: TObject; const Section, Key: string);
begin
  // La configuration a été modifiée en externe
  if MessageDlg('Configuration modifiée',
    'La configuration a été modifiée en externe. Voulez-vous recharger ?',
    mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    LoadConfiguration;
    LoadRawConfiguration;
  end;
end;

end.
```

## Conclusion

La gestion unifiée de la configuration est un élément essentiel pour créer des applications multi-plateformes robustes et maintenables. Les concepts clés à retenir sont :

### Points essentiels

1. **Abstraction** : Créez une couche d'abstraction qui masque les différences entre plateformes
2. **Flexibilité** : Supportez plusieurs formats de stockage (INI, JSON, XML, Registre)
3. **Sécurité** : Chiffrez les données sensibles
4. **Validation** : Vérifiez l'intégrité et la cohérence des paramètres
5. **Migration** : Gérez l'évolution du schéma de configuration
6. **Portabilité** : Permettez l'import/export des paramètres

### Avantages de cette approche

- **Code unique** : Un seul code pour Windows et Linux
- **Maintenance simplifiée** : Modifications centralisées
- **Expérience utilisateur cohérente** : Interface identique sur toutes les plateformes
- **Évolutivité** : Ajout facile de nouvelles plateformes
- **Robustesse** : Validation et gestion d'erreurs intégrées

### Recommandations finales

1. **Commencez simple** : Utilisez d'abord les fichiers INI, puis évoluez selon les besoins
2. **Documentez** : Maintenez une documentation claire du schéma de configuration
3. **Versionnez** : Utilisez un système de versions pour la configuration
4. **Testez** : Vérifiez le comportement sur chaque plateforme cible
5. **Sauvegardez** : Toujours créer des sauvegardes avant les modifications importantes

Avec cette approche unifiée, vous pouvez créer des applications FreePascal/Lazarus vraiment portables, offrant une expérience utilisateur cohérente tout en respectant les conventions de chaque système d'exploitation.

⏭️ [Spécificités Windows](/06-specificites-windows/README.md)
