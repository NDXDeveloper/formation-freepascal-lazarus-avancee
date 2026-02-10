🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 21.6 Plugin Architecture Portable

## Introduction

Une **architecture plugin** (ou architecture par modules) permet d'étendre les fonctionnalités d'une application sans modifier son code source. Les plugins sont des modules chargés dynamiquement au moment de l'exécution.

### Qu'est-ce qu'un plugin ?

Un **plugin** est un composant logiciel :
- **Indépendant** : Développé séparément de l'application principale
- **Chargeable dynamiquement** : Ajouté sans recompiler l'application
- **Respecte un contrat** : Implémente une interface définie
- **Optionnel** : L'application fonctionne sans lui

```
APPLICATION PRINCIPALE
┌─────────────────────────────┐
│  Cœur de l'application      │
│  ┌──────────────────────┐   │
│  │  Plugin Manager      │   │
│  └──────────┬───────────┘   │
└─────────────┼───────────────┘
              │
    ┌─────────┼─────────┐
    │         │         │
┌───▼───┐ ┌───▼───┐ ┌───▼───┐
│Plugin │ │Plugin │ │Plugin │
│  PDF  │ │  XML  │ │  JSON │
└───────┘ └───────┘ └───────┘
```

### Pourquoi utiliser des plugins ?

**Avantages :**
- ✅ **Extensibilité** : Ajouter des fonctionnalités sans modifier le code
- ✅ **Modularité** : Développement indépendant des modules
- ✅ **Distribution** : Distribuer des fonctionnalités séparément
- ✅ **Maintenance** : Corriger/améliorer un plugin sans toucher au reste
- ✅ **Personnalisation** : Chaque utilisateur active les plugins voulus
- ✅ **Marketplace** : Écosystème de plugins tiers

**Exemples d'applications à plugins :**
- **Navigateurs** : Extensions Chrome, Firefox
- **Éditeurs** : Plugins VSCode, Sublime Text
- **DAW** : Plugins VST pour la musique
- **CMS** : WordPress, Joomla
- **IDE** : Lazarus lui-même !

## Architecture de base

### 1. Interface du plugin

Le **contrat** que tous les plugins doivent respecter.

```pascal
unit Plugin.Interfaces;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  // Métadonnées du plugin
  TPluginInfo = record
    Name: string;
    Version: string;
    Author: string;
    Description: string;
    Website: string;
  end;

  // Interface que tout plugin doit implémenter
  IPlugin = interface
    ['{A1B2C3D4-E5F6-7890-ABCD-EF1234567890}']

    // Informations sur le plugin
    function GetInfo: TPluginInfo;

    // Cycle de vie
    procedure Initialize;
    procedure Finalize;

    // Fonctionnalité principale
    procedure Execute;
  end;

  // Factory pour créer des instances de plugins
  TPluginFactory = function: IPlugin;

implementation

end.
```

### 2. Plugin Manager

Le **gestionnaire** qui découvre, charge et gère les plugins.

```pascal
unit Plugin.Manager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, dynlibs,
  Plugin.Interfaces;

type
  // Gestionnaire de plugins
  TPluginManager = class
  private
    FPlugins: TList<IPlugin>;
    FLibraries: TList<TLibHandle>;
    FPluginPath: string;

    function LoadPluginFromLibrary(const ALibPath: string): IPlugin;
    procedure ScanPluginDirectory;
  public
    constructor Create(const APluginPath: string);
    destructor Destroy; override;

    // Gestion des plugins
    procedure LoadPlugins;
    procedure UnloadPlugins;

    // Accès aux plugins
    function GetPluginCount: Integer;
    function GetPlugin(AIndex: Integer): IPlugin;
    function FindPlugin(const AName: string): IPlugin;

    // Exécution
    procedure ExecuteAll;
    procedure ExecutePlugin(const AName: string);

    property PluginPath: string read FPluginPath write FPluginPath;
  end;

implementation

uses
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  {$IFDEF UNIX}
  dl,
  {$ENDIF}
  FileUtil;

constructor TPluginManager.Create(const APluginPath: string);
begin
  inherited Create;
  FPlugins := TList<IPlugin>.Create;
  FLibraries := TList<TLibHandle>.Create;
  FPluginPath := APluginPath;
end;

destructor TPluginManager.Destroy;
begin
  UnloadPlugins;
  FLibraries.Free;
  FPlugins.Free;
  inherited;
end;

function TPluginManager.LoadPluginFromLibrary(const ALibPath: string): IPlugin;
var
  LibHandle: TLibHandle;
  CreatePluginFunc: TPluginFactory;
  FuncName: string;
begin
  Result := nil;

  WriteLn(Format('[PluginManager] Chargement: %s', [ALibPath]));

  // Charger la bibliothèque dynamique
  LibHandle := LoadLibrary(PChar(ALibPath));

  if LibHandle = 0 then
  begin
    WriteLn(Format('[PluginManager] ERREUR: Impossible de charger %s', [ALibPath]));
    Exit;
  end;

  FLibraries.Add(LibHandle);

  // Chercher la fonction d'export CreatePlugin
  {$IFDEF WINDOWS}
  FuncName := 'CreatePlugin';
  {$ELSE}
  FuncName := 'CreatePlugin';
  {$ENDIF}

  CreatePluginFunc := TPluginFactory(GetProcAddress(LibHandle, PChar(FuncName)));

  if not Assigned(CreatePluginFunc) then
  begin
    WriteLn(Format('[PluginManager] ERREUR: Fonction %s non trouvée', [FuncName]));
    Exit;
  end;

  // Créer l'instance du plugin
  try
    Result := CreatePluginFunc();
    WriteLn(Format('[PluginManager] Plugin chargé: %s', [Result.GetInfo.Name]));
  except
    on E: Exception do
      WriteLn(Format('[PluginManager] ERREUR création plugin: %s', [E.Message]));
  end;
end;

procedure TPluginManager.ScanPluginDirectory;
var
  FileList: TStringList;
  FileName: string;
  Plugin: IPlugin;
  Extension: string;
begin
  if not DirectoryExists(FPluginPath) then
  begin
    WriteLn(Format('[PluginManager] Répertoire plugins non trouvé: %s', [FPluginPath]));
    Exit;
  end;

  FileList := TStringList.Create;
  try
    // Extension selon la plateforme
    {$IFDEF WINDOWS}
    Extension := '*.dll';
    {$ENDIF}
    {$IFDEF LINUX}
    Extension := '*.so';
    {$ENDIF}
    {$IFDEF DARWIN}
    Extension := '*.dylib';
    {$ENDIF}

    // Lister les fichiers de plugins
    FindAllFiles(FileList, FPluginPath, Extension, False);

    WriteLn(Format('[PluginManager] %d plugin(s) trouvé(s)', [FileList.Count]));

    for FileName in FileList do
    begin
      Plugin := LoadPluginFromLibrary(FileName);
      if Assigned(Plugin) then
        FPlugins.Add(Plugin);
    end;

  finally
    FileList.Free;
  end;
end;

procedure TPluginManager.LoadPlugins;
var
  Plugin: IPlugin;
begin
  WriteLn('[PluginManager] Chargement des plugins...');

  ScanPluginDirectory;

  // Initialiser tous les plugins
  for Plugin in FPlugins do
  begin
    try
      Plugin.Initialize;
      WriteLn(Format('[PluginManager] Plugin initialisé: %s', [Plugin.GetInfo.Name]));
    except
      on E: Exception do
        WriteLn(Format('[PluginManager] ERREUR initialisation: %s', [E.Message]));
    end;
  end;

  WriteLn(Format('[PluginManager] %d plugin(s) chargé(s)', [FPlugins.Count]));
end;

procedure TPluginManager.UnloadPlugins;
var
  Plugin: IPlugin;
  LibHandle: TLibHandle;
begin
  WriteLn('[PluginManager] Déchargement des plugins...');

  // Finaliser tous les plugins
  for Plugin in FPlugins do
  begin
    try
      Plugin.Finalize;
    except
      on E: Exception do
        WriteLn(Format('[PluginManager] ERREUR finalisation: %s', [E.Message]));
    end;
  end;

  FPlugins.Clear;

  // Décharger les bibliothèques
  for LibHandle in FLibraries do
    UnloadLibrary(LibHandle);

  FLibraries.Clear;

  WriteLn('[PluginManager] Plugins déchargés');
end;

function TPluginManager.GetPluginCount: Integer;
begin
  Result := FPlugins.Count;
end;

function TPluginManager.GetPlugin(AIndex: Integer): IPlugin;
begin
  if (AIndex >= 0) and (AIndex < FPlugins.Count) then
    Result := FPlugins[AIndex]
  else
    Result := nil;
end;

function TPluginManager.FindPlugin(const AName: string): IPlugin;
var
  Plugin: IPlugin;
begin
  Result := nil;

  for Plugin in FPlugins do
  begin
    if Plugin.GetInfo.Name = AName then
    begin
      Result := Plugin;
      Break;
    end;
  end;
end;

procedure TPluginManager.ExecuteAll;
var
  Plugin: IPlugin;
begin
  WriteLn('[PluginManager] Exécution de tous les plugins...');

  for Plugin in FPlugins do
  begin
    try
      Plugin.Execute;
    except
      on E: Exception do
        WriteLn(Format('[PluginManager] ERREUR exécution: %s', [E.Message]));
    end;
  end;
end;

procedure TPluginManager.ExecutePlugin(const AName: string);
var
  Plugin: IPlugin;
begin
  Plugin := FindPlugin(AName);

  if Assigned(Plugin) then
  begin
    WriteLn(Format('[PluginManager] Exécution plugin: %s', [AName]));
    Plugin.Execute;
  end
  else
    WriteLn(Format('[PluginManager] Plugin non trouvé: %s', [AName]));
end;

end.
```

### 3. Exemple de plugin simple

```pascal
library PluginHelloWorld;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils,
  Plugin.Interfaces;

type
  // Implémentation du plugin
  THelloWorldPlugin = class(TInterfacedObject, IPlugin)
  private
    FInfo: TPluginInfo;
  public
    constructor Create;

    function GetInfo: TPluginInfo;
    procedure Initialize;
    procedure Finalize;
    procedure Execute;
  end;

constructor THelloWorldPlugin.Create;
begin
  inherited Create;

  FInfo.Name := 'Hello World';
  FInfo.Version := '1.0.0';
  FInfo.Author := 'Votre Nom';
  FInfo.Description := 'Plugin de démonstration';
  FInfo.Website := 'https://example.com';
end;

function THelloWorldPlugin.GetInfo: TPluginInfo;
begin
  Result := FInfo;
end;

procedure THelloWorldPlugin.Initialize;
begin
  WriteLn('[Plugin HelloWorld] Initialisation...');
end;

procedure THelloWorldPlugin.Finalize;
begin
  WriteLn('[Plugin HelloWorld] Finalisation...');
end;

procedure THelloWorldPlugin.Execute;
begin
  WriteLn('═══════════════════════════════════════');
  WriteLn('  HELLO WORLD depuis un plugin !      ');
  WriteLn('  Version: ', FInfo.Version);
  WriteLn('  Auteur: ', FInfo.Author);
  WriteLn('═══════════════════════════════════════');
end;

// Fonction exportée pour créer le plugin
function CreatePlugin: IPlugin; stdcall;
begin
  Result := THelloWorldPlugin.Create;
end;

exports
  CreatePlugin;

begin
end.
```

**Compilation du plugin :**
```bash
# Windows
fpc -o plugins/HelloWorld.dll PluginHelloWorld.lpr

# Linux
fpc -o plugins/HelloWorld.so PluginHelloWorld.lpr

# macOS
fpc -o plugins/HelloWorld.dylib PluginHelloWorld.lpr
```

### 4. Application hôte

```pascal
program PluginHost;

{$mode objfpc}{$H+}

uses
  SysUtils,
  Plugin.Manager;

var
  Manager: TPluginManager;
  i: Integer;
  Plugin: IPlugin;
  Info: TPluginInfo;
begin
  WriteLn('═══════════════════════════════════════════');
  WriteLn('  Application avec support de plugins     ');
  WriteLn('═══════════════════════════════════════════');
  WriteLn;

  try
    // Créer le gestionnaire de plugins
    Manager := TPluginManager.Create('plugins');
    try
      // Charger tous les plugins du répertoire
      Manager.LoadPlugins;
      WriteLn;

      // Afficher les plugins chargés
      WriteLn('=== PLUGINS CHARGÉS ===');
      for i := 0 to Manager.GetPluginCount - 1 do
      begin
        Plugin := Manager.GetPlugin(i);
        Info := Plugin.GetInfo;

        WriteLn(Format('%d. %s v%s', [i + 1, Info.Name, Info.Version]));
        WriteLn('   Description: ', Info.Description);
        WriteLn('   Auteur: ', Info.Author);
        WriteLn;
      end;

      // Exécuter tous les plugins
      WriteLn('=== EXÉCUTION DES PLUGINS ===');
      Manager.ExecuteAll;
      WriteLn;

      // Décharger les plugins
      Manager.UnloadPlugins;

    finally
      Manager.Free;
    end;

  except
    on E: Exception do
      WriteLn('ERREUR: ', E.Message);
  end;

  WriteLn;
  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
```

## Plugins avancés

### 1. Plugin avec configuration

```pascal
unit Plugin.Interfaces.Extended;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson,
  Plugin.Interfaces;

type
  // Interface étendue avec configuration
  IConfigurablePlugin = interface(IPlugin)
    ['{B2C3D4E5-F6A7-8901-BCDE-F12345678901}']

    procedure LoadConfig(AConfig: TJSONObject);
    procedure SaveConfig(AConfig: TJSONObject);
    function GetDefaultConfig: TJSONObject;
  end;

implementation

end.
```

**Plugin configurable :**

```pascal
type
  TCalculatorPlugin = class(TInterfacedObject, IConfigurablePlugin)
  private
    FInfo: TPluginInfo;
    FPrecision: Integer;
    FUseDegrees: Boolean;
  public
    constructor Create;

    // IPlugin
    function GetInfo: TPluginInfo;
    procedure Initialize;
    procedure Finalize;
    procedure Execute;

    // IConfigurablePlugin
    procedure LoadConfig(AConfig: TJSONObject);
    procedure SaveConfig(AConfig: TJSONObject);
    function GetDefaultConfig: TJSONObject;
  end;

procedure TCalculatorPlugin.LoadConfig(AConfig: TJSONObject);
begin
  FPrecision := AConfig.Get('precision', 2);
  FUseDegrees := AConfig.Get('use_degrees', True);

  WriteLn(Format('[Calculator] Configuration chargée: Precision=%d, Degrees=%s',
    [FPrecision, BoolToStr(FUseDegrees, True)]));
end;

procedure TCalculatorPlugin.SaveConfig(AConfig: TJSONObject);
begin
  AConfig.Add('precision', FPrecision);
  AConfig.Add('use_degrees', FUseDegrees);
end;

function TCalculatorPlugin.GetDefaultConfig: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.Add('precision', 2);
  Result.Add('use_degrees', True);
end;
```

### 2. Plugin avec événements

```pascal
unit Plugin.Events;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  // Type d'événement
  TPluginEventType = (
    petBeforeExecute,
    petAfterExecute,
    petOnError,
    petOnProgress
  );

  // Données d'événement
  TPluginEventData = class
  private
    FEventType: TPluginEventType;
    FPluginName: string;
    FMessage: string;
    FProgress: Integer;
  public
    property EventType: TPluginEventType read FEventType write FEventType;
    property PluginName: string read FPluginName write FPluginName;
    property Message: string read FMessage write FMessage;
    property Progress: Integer read FProgress write FProgress;
  end;

  // Callback d'événement
  TPluginEventCallback = procedure(AData: TPluginEventData) of object;

  // Interface avec événements
  IEventfulPlugin = interface(IPlugin)
    ['{C3D4E5F6-A7B8-9012-CDEF-123456789012}']

    procedure SetEventCallback(ACallback: TPluginEventCallback);
  end;

implementation

end.
```

**Plugin avec événements :**

```pascal
type
  TProgressPlugin = class(TInterfacedObject, IEventfulPlugin)
  private
    FInfo: TPluginInfo;
    FEventCallback: TPluginEventCallback;

    procedure NotifyEvent(AEventType: TPluginEventType;
                         const AMessage: string;
                         AProgress: Integer = 0);
  public
    constructor Create;

    function GetInfo: TPluginInfo;
    procedure Initialize;
    procedure Finalize;
    procedure Execute;

    procedure SetEventCallback(ACallback: TPluginEventCallback);
  end;

procedure TProgressPlugin.NotifyEvent(AEventType: TPluginEventType;
  const AMessage: string; AProgress: Integer);
var
  EventData: TPluginEventData;
begin
  if not Assigned(FEventCallback) then
    Exit;

  EventData := TPluginEventData.Create;
  try
    EventData.EventType := AEventType;
    EventData.PluginName := FInfo.Name;
    EventData.Message := AMessage;
    EventData.Progress := AProgress;

    FEventCallback(EventData);
  finally
    EventData.Free;
  end;
end;

procedure TProgressPlugin.Execute;
var
  i: Integer;
begin
  NotifyEvent(petBeforeExecute, 'Démarrage du traitement');

  for i := 1 to 10 do
  begin
    Sleep(100);  // Simuler un traitement
    NotifyEvent(petOnProgress, 'Traitement en cours', i * 10);
  end;

  NotifyEvent(petAfterExecute, 'Traitement terminé', 100);
end;

procedure TProgressPlugin.SetEventCallback(ACallback: TPluginEventCallback);
begin
  FEventCallback := ACallback;
end;
```

### 3. Plugin avec dépendances

```pascal
unit Plugin.Dependencies;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections,
  Plugin.Interfaces;

type
  // Dépendance de plugin
  TPluginDependency = record
    Name: string;
    MinVersion: string;
    Required: Boolean;
  end;

  // Interface avec dépendances
  IPluginWithDependencies = interface(IPlugin)
    ['{D4E5F6A7-B8C9-0123-DEF1-234567890123}']

    function GetDependencies: TArray<TPluginDependency>;
  end;

  // Résolveur de dépendances
  TDependencyResolver = class
  private
    FPlugins: TList<IPlugin>;

    function CompareVersions(const V1, V2: string): Integer;
    function CheckDependency(const ADep: TPluginDependency): Boolean;
  public
    constructor Create(APlugins: TList<IPlugin>);

    function ResolveDependencies: Boolean;
    function GetLoadOrder: TList<IPlugin>;
  end;

implementation

uses
  StrUtils;

constructor TDependencyResolver.Create(APlugins: TList<IPlugin>);
begin
  inherited Create;
  FPlugins := APlugins;
end;

function TDependencyResolver.CompareVersions(const V1, V2: string): Integer;
var
  Parts1, Parts2: TStringList;
  i, Val1, Val2: Integer;
begin
  Parts1 := TStringList.Create;
  Parts2 := TStringList.Create;
  try
    Parts1.Delimiter := '.';
    Parts1.StrictDelimiter := True;
    Parts1.DelimitedText := V1;
    Parts2.Delimiter := '.';
    Parts2.StrictDelimiter := True;
    Parts2.DelimitedText := V2;

    for i := 0 to Min(Parts1.Count, Parts2.Count) - 1 do
    begin
      Val1 := StrToIntDef(Parts1[i], 0);
      Val2 := StrToIntDef(Parts2[i], 0);

      if Val1 < Val2 then Exit(-1);
      if Val1 > Val2 then Exit(1);
    end;

    Result := 0;
  finally
    Parts1.Free;
    Parts2.Free;
  end;
end;

function TDependencyResolver.CheckDependency(const ADep: TPluginDependency): Boolean;
var
  Plugin: IPlugin;
  Info: TPluginInfo;
begin
  Result := False;

  for Plugin in FPlugins do
  begin
    Info := Plugin.GetInfo;

    if Info.Name = ADep.Name then
    begin
      // Vérifier la version
      if CompareVersions(Info.Version, ADep.MinVersion) >= 0 then
        Result := True;
      Break;
    end;
  end;
end;

function TDependencyResolver.ResolveDependencies: Boolean;
var
  Plugin: IPlugin;
  DepPlugin: IPluginWithDependencies;
  Dependencies: TArray<TPluginDependency>;
  Dep: TPluginDependency;
begin
  Result := True;

  for Plugin in FPlugins do
  begin
    if Supports(Plugin, IPluginWithDependencies, DepPlugin) then
    begin
      Dependencies := DepPlugin.GetDependencies;

      for Dep in Dependencies do
      begin
        if not CheckDependency(Dep) then
        begin
          if Dep.Required then
          begin
            WriteLn(Format('[Dependencies] ERREUR: Dépendance manquante: %s >= %s',
              [Dep.Name, Dep.MinVersion]));
            Result := False;
          end
          else
            WriteLn(Format('[Dependencies] Avertissement: Dépendance optionnelle manquante: %s',
              [Dep.Name]));
        end;
      end;
    end;
  end;
end;

function TDependencyResolver.GetLoadOrder: TList<IPlugin>;
begin
  // Algorithme de tri topologique pour ordonner les plugins
  // selon leurs dépendances
  // (Implémentation simplifiée ici)
  Result := TList<IPlugin>.Create;
  Result.AddRange(FPlugins);
end;

end.
```

## Sécurité des plugins

### 1. Sandbox (Bac à sable)

Limiter ce qu'un plugin peut faire.

```pascal
unit Plugin.Sandbox;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Plugin.Interfaces;

type
  // Permissions de plugin
  TPluginPermission = (
    ppReadFiles,
    ppWriteFiles,
    ppNetwork,
    ppExecuteCommands,
    ppAccessRegistry
  );

  TPluginPermissions = set of TPluginPermission;

  // Interface avec permissions
  ISecurePlugin = interface(IPlugin)
    ['{E5F6A7B8-C9D0-1234-E5F6-A7B8C9D0E1F2}']

    function GetRequiredPermissions: TPluginPermissions;
  end;

  // Sandbox executor
  TSandboxExecutor = class
  private
    FAllowedPermissions: TPluginPermissions;

    function CheckPermissions(APlugin: IPlugin): Boolean;
  public
    constructor Create(AAllowedPermissions: TPluginPermissions);

    procedure ExecutePlugin(APlugin: IPlugin);
  end;

implementation

constructor TSandboxExecutor.Create(AAllowedPermissions: TPluginPermissions);
begin
  inherited Create;
  FAllowedPermissions := AAllowedPermissions;
end;

function TSandboxExecutor.CheckPermissions(APlugin: IPlugin): Boolean;
var
  SecurePlugin: ISecurePlugin;
  Required: TPluginPermissions;
  Perm: TPluginPermission;
begin
  Result := True;

  if not Supports(APlugin, ISecurePlugin, SecurePlugin) then
  begin
    WriteLn('[Sandbox] Plugin non sécurisé, exécution refusée');
    Exit(False);
  end;

  Required := SecurePlugin.GetRequiredPermissions;

  for Perm in Required do
  begin
    if not (Perm in FAllowedPermissions) then
    begin
      WriteLn(Format('[Sandbox] Permission refusée: %d', [Ord(Perm)]));
      Result := False;
      Break;
    end;
  end;
end;

procedure TSandboxExecutor.ExecutePlugin(APlugin: IPlugin);
begin
  if not CheckPermissions(APlugin) then
  begin
    WriteLn('[Sandbox] Exécution refusée pour: ', APlugin.GetInfo.Name);
    Exit;
  end;

  WriteLn('[Sandbox] Exécution autorisée pour: ', APlugin.GetInfo.Name);

  try
    APlugin.Execute;
  except
    on E: Exception do
      WriteLn('[Sandbox] Erreur capturée: ', E.Message);
  end;
end;

end.
```

### 2. Signature de plugins

Vérifier l'authenticité des plugins.

```pascal
unit Plugin.Signature;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DCPsha256, base64;

type
  TPluginSignature = class
  private
    FPublicKey: string;

    function CalculateHash(const AFileName: string): string;
    function VerifySignature(const AHash, ASignature: string): Boolean;
  public
    constructor Create(const APublicKey: string);

    function VerifyPlugin(const AFileName, ASignatureFile: string): Boolean;
  end;

implementation

constructor TPluginSignature.Create(const APublicKey: string);
begin
  inherited Create;
  FPublicKey := APublicKey;
end;

function TPluginSignature.CalculateHash(const AFileName: string): string;
var
  FileStream: TFileStream;
  Hash: TDCP_sha256;
  Digest: array[0..31] of Byte;
begin
  FileStream := TFileStream.Create(AFileName, fmOpenRead);
  try
    Hash := TDCP_sha256.Create(nil);
    try
      Hash.Init;
      Hash.UpdateStream(FileStream, FileStream.Size);
      Hash.Final(Digest);

      Result := EncodeStringBase64(PChar(@Digest[0]), 32);
    finally
      Hash.Free;
    end;
  finally
    FileStream.Free;
  end;
end;

function TPluginSignature.VerifySignature(const AHash, ASignature: string): Boolean;
begin
  // Vérification simplifiée ici
  // Dans un cas réel, utiliser RSA ou autre algorithme asymétrique
  Result := AHash = ASignature;
end;

function TPluginSignature.VerifyPlugin(const AFileName, ASignatureFile: string): Boolean;
var
  Hash, Signature: string;
  SigFile: TStringList;
begin
  Result := False;

  if not FileExists(ASignatureFile) then
  begin
    WriteLn('[Signature] Fichier de signature manquant');
    Exit;
  end;

  // Calculer le hash du plugin
  Hash := CalculateHash(AFileName);

  // Lire la signature
  SigFile := TStringList.Create;
  try
    SigFile.LoadFromFile(ASignatureFile);
    if SigFile.Count > 0 then
      Signature := SigFile[0];
  finally
    SigFile.Free;
  end;

  // Vérifier
  Result := VerifySignature(Hash, Signature);

  if Result then
    WriteLn('[Signature] Plugin vérifié: ', ExtractFileName(AFileName))
  else
    WriteLn('[Signature] ALERTE: Signature invalide pour: ', ExtractFileName(AFileName));
end;

end.
```

## Communication entre plugins

### Message Bus pour plugins

```pascal
unit Plugin.MessageBus;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, fpjson;

type
  // Message inter-plugin
  TPluginMessage = class
  private
    FSender: string;
    FTopic: string;
    FData: TJSONObject;
    FTimestamp: TDateTime;
  public
    constructor Create(const ASender, ATopic: string; AData: TJSONObject);
    destructor Destroy; override;

    property Sender: string read FSender;
    property Topic: string read FTopic;
    property Data: TJSONObject read FData;
    property Timestamp: TDateTime read FTimestamp;
  end;

  // Callback pour les messages
  TMessageCallback = procedure(AMessage: TPluginMessage) of object;

  // Souscripteur
  TMessageSubscriber = class
  private
    FPluginName: string;
    FTopic: string;
    FCallback: TMessageCallback;
  public
    constructor Create(const APluginName, ATopic: string;
                      ACallback: TMessageCallback);

    property PluginName: string read FPluginName;
    property Topic: string read FTopic;
    property Callback: TMessageCallback read FCallback;
  end;

  // Bus de messages
  TPluginMessageBus = class
  private
    FSubscribers: TObjectList<TMessageSubscriber>;
    FMessageQueue: TThreadedQueue<TPluginMessage>;
    FLock: TRTLCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Subscribe(const APluginName, ATopic: string;
                       ACallback: TMessageCallback);
    procedure Unsubscribe(const APluginName, ATopic: string);

    procedure Publish(const ASender, ATopic: string; AData: TJSONObject);
    procedure ProcessMessages;
  end;

implementation

// TPluginMessage

constructor TPluginMessage.Create(const ASender, ATopic: string; AData: TJSONObject);
begin
  inherited Create;
  FSender := ASender;
  FTopic := ATopic;
  FData := AData;
  FTimestamp := Now;
end;

destructor TPluginMessage.Destroy;
begin
  FData.Free;
  inherited;
end;

// TMessageSubscriber

constructor TMessageSubscriber.Create(const APluginName, ATopic: string;
  ACallback: TMessageCallback);
begin
  inherited Create;
  FPluginName := APluginName;
  FTopic := ATopic;
  FCallback := ACallback;
end;

// TPluginMessageBus

constructor TPluginMessageBus.Create;
begin
  inherited Create;
  FSubscribers := TObjectList<TMessageSubscriber>.Create(True);
  FMessageQueue := TThreadedQueue<TPluginMessage>.Create(100, 1000, 100);
  InitCriticalSection(FLock);
end;

destructor TPluginMessageBus.Destroy;
var
  Msg: TPluginMessage;
begin
  // Vider la queue
  while FMessageQueue.PopItem(Msg) = wrSignaled do
    Msg.Free;

  FMessageQueue.Free;
  FSubscribers.Free;
  DoneCriticalSection(FLock);
  inherited;
end;

procedure TPluginMessageBus.Subscribe(const APluginName, ATopic: string;
  ACallback: TMessageCallback);
var
  Subscriber: TMessageSubscriber;
begin
  EnterCriticalSection(FLock);
  try
    Subscriber := TMessageSubscriber.Create(APluginName, ATopic, ACallback);
    FSubscribers.Add(Subscriber);

    WriteLn(Format('[MessageBus] %s souscrit au topic: %s', [APluginName, ATopic]));
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TPluginMessageBus.Unsubscribe(const APluginName, ATopic: string);
var
  i: Integer;
begin
  EnterCriticalSection(FLock);
  try
    for i := FSubscribers.Count - 1 downto 0 do
    begin
      if (FSubscribers[i].PluginName = APluginName) and
         (FSubscribers[i].Topic = ATopic) then
      begin
        FSubscribers.Delete(i);
        WriteLn(Format('[MessageBus] %s désinscrit du topic: %s',
          [APluginName, ATopic]));
      end;
    end;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TPluginMessageBus.Publish(const ASender, ATopic: string; AData: TJSONObject);
var
  Message: TPluginMessage;
begin
  Message := TPluginMessage.Create(ASender, ATopic, AData);
  FMessageQueue.PushItem(Message);

  WriteLn(Format('[MessageBus] Message publié par %s sur topic: %s',
    [ASender, ATopic]));
end;

procedure TPluginMessageBus.ProcessMessages;
var
  Message: TPluginMessage;
  Subscriber: TMessageSubscriber;
begin
  // Traiter tous les messages en attente
  while FMessageQueue.PopItem(Message) = wrSignaled do
  begin
    try
      EnterCriticalSection(FLock);
      try
        // Distribuer aux souscripteurs
        for Subscriber in FSubscribers do
        begin
          if (Subscriber.Topic = Message.Topic) or (Subscriber.Topic = '*') then
          begin
            try
              Subscriber.Callback(Message);
            except
              on E: Exception do
                WriteLn(Format('[MessageBus] Erreur callback %s: %s',
                  [Subscriber.PluginName, E.Message]));
            end;
          end;
        end;
      finally
        LeaveCriticalSection(FLock);
      end;
    finally
      Message.Free;
    end;
  end;
end;

end.
```

**Plugin utilisant le MessageBus :**

```pascal
type
  TChatPlugin = class(TInterfacedObject, IPlugin)
  private
    FInfo: TPluginInfo;
    FMessageBus: TPluginMessageBus;

    procedure OnMessageReceived(AMessage: TPluginMessage);
  public
    constructor Create(AMessageBus: TPluginMessageBus);

    function GetInfo: TPluginInfo;
    procedure Initialize;
    procedure Finalize;
    procedure Execute;

    procedure SendMessage(const AText: string);
  end;

constructor TChatPlugin.Create(AMessageBus: TPluginMessageBus);
begin
  inherited Create;
  FMessageBus := AMessageBus;

  FInfo.Name := 'Chat Plugin';
  FInfo.Version := '1.0.0';
  FInfo.Author := 'Votre Nom';
  FInfo.Description := 'Plugin de chat inter-plugin';
end;

procedure TChatPlugin.Initialize;
begin
  // S'abonner aux messages
  FMessageBus.Subscribe(FInfo.Name, 'chat', @OnMessageReceived);
  WriteLn('[ChatPlugin] Initialisé et abonné');
end;

procedure TChatPlugin.Finalize;
begin
  // Se désabonner
  FMessageBus.Unsubscribe(FInfo.Name, 'chat');
  WriteLn('[ChatPlugin] Finalisé');
end;

procedure TChatPlugin.Execute;
begin
  SendMessage('Hello from ' + FInfo.Name);
end;

procedure TChatPlugin.OnMessageReceived(AMessage: TPluginMessage);
var
  Text: string;
begin
  if AMessage.Sender = FInfo.Name then
    Exit; // Ignorer nos propres messages

  Text := AMessage.Data.Get('text', '');
  WriteLn(Format('[ChatPlugin] Message reçu de %s: %s',
    [AMessage.Sender, Text]));
end;

procedure TChatPlugin.SendMessage(const AText: string);
var
  Data: TJSONObject;
begin
  Data := TJSONObject.Create;
  Data.Add('text', AText);

  FMessageBus.Publish(FInfo.Name, 'chat', Data);
end;
```

## Hot-Reload (Rechargement à chaud)

Recharger un plugin sans redémarrer l'application.

```pascal
unit Plugin.HotReload;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections,
  Plugin.Interfaces, Plugin.Manager;

type
  TPluginWatcher = class
  private
    FPluginPath: string;
    FManager: TPluginManager;
    FFileStates: TDictionary<string, TDateTime>;
    FTimer: TTimer;

    procedure CheckForChanges(Sender: TObject);
    procedure ReloadPlugin(const APluginFile: string);
  public
    constructor Create(AManager: TPluginManager; const APluginPath: string);
    destructor Destroy; override;

    procedure StartWatching;
    procedure StopWatching;
  end;

implementation

uses
  FileUtil, DateUtils;

constructor TPluginWatcher.Create(AManager: TPluginManager; const APluginPath: string);
begin
  inherited Create;
  FManager := AManager;
  FPluginPath := APluginPath;
  FFileStates := TDictionary<string, TDateTime>.Create;

  FTimer := TTimer.Create(nil);
  FTimer.Interval := 1000; // Vérifier chaque seconde
  FTimer.OnTimer := @CheckForChanges;
  FTimer.Enabled := False;
end;

destructor TPluginWatcher.Destroy;
begin
  StopWatching;
  FTimer.Free;
  FFileStates.Free;
  inherited;
end;

procedure TPluginWatcher.StartWatching;
var
  FileList: TStringList;
  FileName: string;
begin
  WriteLn('[HotReload] Surveillance activée');

  // Initialiser l'état des fichiers
  FileList := TStringList.Create;
  try
    {$IFDEF WINDOWS}
    FindAllFiles(FileList, FPluginPath, '*.dll', False);
    {$ELSE}
    FindAllFiles(FileList, FPluginPath, '*.so', False);
    {$ENDIF}

    for FileName in FileList do
      FFileStates.AddOrSetValue(FileName, FileAge(FileName));
  finally
    FileList.Free;
  end;

  FTimer.Enabled := True;
end;

procedure TPluginWatcher.StopWatching;
begin
  FTimer.Enabled := False;
  WriteLn('[HotReload] Surveillance désactivée');
end;

procedure TPluginWatcher.CheckForChanges(Sender: TObject);
var
  FileList: TStringList;
  FileName: string;
  CurrentTime, StoredTime: TDateTime;
begin
  FileList := TStringList.Create;
  try
    {$IFDEF WINDOWS}
    FindAllFiles(FileList, FPluginPath, '*.dll', False);
    {$ELSE}
    FindAllFiles(FileList, FPluginPath, '*.so', False);
    {$ENDIF}

    for FileName in FileList do
    begin
      CurrentTime := FileAge(FileName);

      if FFileStates.TryGetValue(FileName, StoredTime) then
      begin
        // Fichier existant - vérifier s'il a changé
        if CompareDateTime(CurrentTime, StoredTime) <> 0 then
        begin
          WriteLn(Format('[HotReload] Changement détecté: %s',
            [ExtractFileName(FileName)]));
          ReloadPlugin(FileName);
          FFileStates.AddOrSetValue(FileName, CurrentTime);
        end;
      end
      else
      begin
        // Nouveau fichier
        WriteLn(Format('[HotReload] Nouveau plugin détecté: %s',
          [ExtractFileName(FileName)]));
        FFileStates.Add(FileName, CurrentTime);
        ReloadPlugin(FileName);
      end;
    end;
  finally
    FileList.Free;
  end;
end;

procedure TPluginWatcher.ReloadPlugin(const APluginFile: string);
begin
  WriteLn('[HotReload] Rechargement: ', ExtractFileName(APluginFile));

  // Décharger puis recharger le plugin
  // (Implémentation simplifiée - dans la vraie vie, c'est plus complexe)
  FManager.UnloadPlugins;
  Sleep(100); // Attendre que le fichier soit libéré
  FManager.LoadPlugins;
end;

end.
```

## Marketplace de plugins

### Métadonnées étendues pour marketplace

```pascal
unit Plugin.Marketplace;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson;

type
  TPluginCategory = (
    pcUtility,
    pcGraphics,
    pcAudio,
    pcDevelopment,
    pcProductivity,
    pcGames,
    pcOther
  );

  TPluginLicense = (
    plMIT,
    plGPLv3,
    plApache,
    plBSD,
    plProprietary,
    plFreeware
  );

  TMarketplacePluginInfo = class
  private
    FName: string;
    FVersion: string;
    FAuthor: string;
    FDescription: string;
    FLongDescription: string;
    FWebsite: string;
    FCategory: TPluginCategory;
    FLicense: TPluginLicense;
    FPrice: Currency;
    FDownloadCount: Integer;
    FRating: Double;
    FScreenshots: TStringList;
    FTags: TStringList;
  public
    constructor Create;
    destructor Destroy; override;

    function ToJSON: TJSONObject;
    procedure FromJSON(AJSON: TJSONObject);

    property Name: string read FName write FName;
    property Version: string read FVersion write FVersion;
    property Author: string read FAuthor write FAuthor;
    property Description: string read FDescription write FDescription;
    property LongDescription: string read FLongDescription write FLongDescription;
    property Website: string read FWebsite write FWebsite;
    property Category: TPluginCategory read FCategory write FCategory;
    property License: TPluginLicense read FLicense write FLicense;
    property Price: Currency read FPrice write FPrice;
    property DownloadCount: Integer read FDownloadCount write FDownloadCount;
    property Rating: Double read FRating write FRating;
    property Screenshots: TStringList read FScreenshots;
    property Tags: TStringList read FTags;
  end;

implementation

constructor TMarketplacePluginInfo.Create;
begin
  inherited Create;
  FScreenshots := TStringList.Create;
  FTags := TStringList.Create;
  FPrice := 0;
  FDownloadCount := 0;
  FRating := 0;
end;

destructor TMarketplacePluginInfo.Destroy;
begin
  FTags.Free;
  FScreenshots.Free;
  inherited;
end;

function TMarketplacePluginInfo.ToJSON: TJSONObject;
var
  ScreenshotsArray, TagsArray: TJSONArray;
  i: Integer;
begin
  Result := TJSONObject.Create;

  Result.Add('name', FName);
  Result.Add('version', FVersion);
  Result.Add('author', FAuthor);
  Result.Add('description', FDescription);
  Result.Add('long_description', FLongDescription);
  Result.Add('website', FWebsite);
  Result.Add('category', Ord(FCategory));
  Result.Add('license', Ord(FLicense));
  Result.Add('price', FPrice);
  Result.Add('download_count', FDownloadCount);
  Result.Add('rating', FRating);

  // Screenshots
  ScreenshotsArray := TJSONArray.Create;
  for i := 0 to FScreenshots.Count - 1 do
    ScreenshotsArray.Add(FScreenshots[i]);
  Result.Add('screenshots', ScreenshotsArray);

  // Tags
  TagsArray := TJSONArray.Create;
  for i := 0 to FTags.Count - 1 do
    TagsArray.Add(FTags[i]);
  Result.Add('tags', TagsArray);
end;

procedure TMarketplacePluginInfo.FromJSON(AJSON: TJSONObject);
var
  ScreenshotsArray, TagsArray: TJSONArray;
  i: Integer;
begin
  FName := AJSON.Get('name', '');
  FVersion := AJSON.Get('version', '');
  FAuthor := AJSON.Get('author', '');
  FDescription := AJSON.Get('description', '');
  FLongDescription := AJSON.Get('long_description', '');
  FWebsite := AJSON.Get('website', '');
  FCategory := TPluginCategory(AJSON.Get('category', 0));
  FLicense := TPluginLicense(AJSON.Get('license', 0));
  FPrice := AJSON.Get('price', 0.0);
  FDownloadCount := AJSON.Get('download_count', 0);
  FRating := AJSON.Get('rating', 0.0);

  // Screenshots
  FScreenshots.Clear;
  ScreenshotsArray := AJSON.Get('screenshots', TJSONArray(nil));
  if Assigned(ScreenshotsArray) then
  begin
    for i := 0 to ScreenshotsArray.Count - 1 do
      FScreenshots.Add(ScreenshotsArray.Strings[i]);
  end;

  // Tags
  FTags.Clear;
  TagsArray := AJSON.Get('tags', TJSONArray(nil));
  if Assigned(TagsArray) then
  begin
    for i := 0 to TagsArray.Count - 1 do
      FTags.Add(TagsArray.Strings[i]);
  end;
end;

end.
```

## GUI pour gestion de plugins

Interface graphique pour gérer les plugins.

```pascal
unit Plugin.GUI;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, StdCtrls, ExtCtrls, ComCtrls,
  Plugin.Manager, Plugin.Interfaces;

type
  TPluginManagerForm = class(TForm)
    ListView: TListView;
    PanelInfo: TPanel;
    LabelName: TLabel;
    LabelVersion: TLabel;
    LabelAuthor: TLabel;
    MemoDescription: TMemo;
    ButtonEnable: TButton;
    ButtonDisable: TButton;
    ButtonConfigure: TButton;
    ButtonReload: TButton;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListViewSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure ButtonEnableClick(Sender: TObject);
    procedure ButtonDisableClick(Sender: TObject);
    procedure ButtonConfigureClick(Sender: TObject);
    procedure ButtonReloadClick(Sender: TObject);
  private
    FManager: TPluginManager;
    FEnabledPlugins: TStringList;

    procedure RefreshPluginList;
    procedure ShowPluginInfo(APlugin: IPlugin);
  public
    constructor Create(AOwner: TComponent; AManager: TPluginManager); reintroduce;
  end;

implementation

constructor TPluginManagerForm.Create(AOwner: TComponent; AManager: TPluginManager);
begin
  inherited Create(AOwner);
  FManager := AManager;
  FEnabledPlugins := TStringList.Create;
end;

procedure TPluginManagerForm.FormCreate(Sender: TObject);
begin
  Caption := 'Gestionnaire de Plugins';
  Width := 800;
  Height := 600;

  // Configurer le ListView
  ListView.ViewStyle := vsReport;
  ListView.Columns.Add.Caption := 'Nom';
  ListView.Columns.Add.Caption := 'Version';
  ListView.Columns.Add.Caption := 'Auteur';
  ListView.Columns.Add.Caption := 'Statut';

  RefreshPluginList;
end;

procedure TPluginManagerForm.FormDestroy(Sender: TObject);
begin
  FEnabledPlugins.Free;
  inherited;
end;

procedure TPluginManagerForm.RefreshPluginList;
var
  i: Integer;
  Plugin: IPlugin;
  Info: TPluginInfo;
  Item: TListItem;
  Status: string;
begin
  ListView.Items.Clear;

  for i := 0 to FManager.GetPluginCount - 1 do
  begin
    Plugin := FManager.GetPlugin(i);
    Info := Plugin.GetInfo;

    if FEnabledPlugins.IndexOf(Info.Name) >= 0 then
      Status := 'Activé'
    else
      Status := 'Désactivé';

    Item := ListView.Items.Add;
    Item.Caption := Info.Name;
    Item.SubItems.Add(Info.Version);
    Item.SubItems.Add(Info.Author);
    Item.SubItems.Add(Status);
    Item.Data := Pointer(i);
  end;
end;

procedure TPluginManagerForm.ListViewSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
var
  Index: Integer;
  Plugin: IPlugin;
begin
  if not Selected then
    Exit;

  Index := PtrInt(Item.Data);
  Plugin := FManager.GetPlugin(Index);

  if Assigned(Plugin) then
    ShowPluginInfo(Plugin);
end;

procedure TPluginManagerForm.ShowPluginInfo(APlugin: IPlugin);
var
  Info: TPluginInfo;
  IsEnabled: Boolean;
begin
  Info := APlugin.GetInfo;
  IsEnabled := FEnabledPlugins.IndexOf(Info.Name) >= 0;

  LabelName.Caption := 'Nom: ' + Info.Name;
  LabelVersion.Caption := 'Version: ' + Info.Version;
  LabelAuthor.Caption := 'Auteur: ' + Info.Author;
  MemoDescription.Text := Info.Description;

  ButtonEnable.Enabled := not IsEnabled;
  ButtonDisable.Enabled := IsEnabled;
end;

procedure TPluginManagerForm.ButtonEnableClick(Sender: TObject);
var
  Item: TListItem;
  Index: Integer;
  Plugin: IPlugin;
  Info: TPluginInfo;
begin
  Item := ListView.Selected;
  if not Assigned(Item) then Exit;

  Index := PtrInt(Item.Data);
  Plugin := FManager.GetPlugin(Index);
  Info := Plugin.GetInfo;

  if FEnabledPlugins.IndexOf(Info.Name) < 0 then
  begin
    FEnabledPlugins.Add(Info.Name);
    Plugin.Initialize;
    ShowMessage('Plugin activé: ' + Info.Name);
    RefreshPluginList;
  end;
end;

procedure TPluginManagerForm.ButtonDisableClick(Sender: TObject);
var
  Item: TListItem;
  Index: Integer;
  Plugin: IPlugin;
  Info: TPluginInfo;
begin
  Item := ListView.Selected;
  if not Assigned(Item) then Exit;

  Index := PtrInt(Item.Data);
  Plugin := FManager.GetPlugin(Index);
  Info := Plugin.GetInfo;

  Index := FEnabledPlugins.IndexOf(Info.Name);
  if Index >= 0 then
  begin
    FEnabledPlugins.Delete(Index);
    Plugin.Finalize;
    ShowMessage('Plugin désactivé: ' + Info.Name);
    RefreshPluginList;
  end;
end;

procedure TPluginManagerForm.ButtonConfigureClick(Sender: TObject);
begin
  ShowMessage('Configuration du plugin (à implémenter)');
end;

procedure TPluginManagerForm.ButtonReloadClick(Sender: TObject);
begin
  FManager.UnloadPlugins;
  FManager.LoadPlugins;
  RefreshPluginList;
  ShowMessage('Plugins rechargés');
end;

end.
```

## Portabilité Windows/Linux/macOS

### Différences de compilation

```pascal
// Directives de compilation pour la portabilité
{$IFDEF WINDOWS}
  {$DEFINE PLUGIN_EXT := '.dll'}
  {$DEFINE STDCALL_EXPORT}
{$ENDIF}

{$IFDEF LINUX}
  {$DEFINE PLUGIN_EXT := '.so'}
  {$DEFINE CDECL_EXPORT}
{$ENDIF}

{$IFDEF DARWIN}
  {$DEFINE PLUGIN_EXT := '.dylib'}
  {$DEFINE CDECL_EXPORT}
{$ENDIF}

// Convention d'appel portable
{$IFDEF STDCALL_EXPORT}
function CreatePlugin: IPlugin; stdcall;
{$ELSE}
function CreatePlugin: IPlugin; cdecl;
{$ENDIF}
```

### Script de build multi-plateforme

```bash
#!/bin/bash
# build-plugins.sh

PLUGIN_NAME=$1
SOURCE_FILE="${PLUGIN_NAME}.lpr"

if [ -z "$PLUGIN_NAME" ]; then
    echo "Usage: $0 <plugin-name>"
    exit 1
fi

echo "=== Compilation du plugin: $PLUGIN_NAME ==="

# Déterminer la plateforme
case "$(uname -s)" in
    Linux*)
        PLATFORM=Linux
        EXT=so
        ;;
    Darwin*)
        PLATFORM=Mac
        EXT=dylib
        ;;
    MINGW*|MSYS*)
        PLATFORM=Windows
        EXT=dll
        ;;
    *)
        echo "Plateforme non supportée"
        exit 1
        ;;
esac

echo "Plateforme détectée: $PLATFORM"

# Créer le répertoire de sortie
mkdir -p plugins

# Compiler
echo "Compilation en cours..."
fpc -o"plugins/${PLUGIN_NAME}.${EXT}" "$SOURCE_FILE"

if [ $? -eq 0 ]; then
    echo "✓ Plugin compilé avec succès: plugins/${PLUGIN_NAME}.${EXT}"
else
    echo "✗ Erreur de compilation"
    exit 1
fi
```

## Bonnes pratiques

### 1. Versionner les interfaces

```pascal
// Version 1
type
  IPluginV1 = interface
    ['{...GUID_V1...}']
    procedure DoSomething;
  end;

// Version 2 (ajoute des fonctionnalités)
type
  IPluginV2 = interface(IPluginV1)
    ['{...GUID_V2...}']
    procedure DoSomethingNew;
  end;

// L'hôte supporte les deux versions
function LoadPlugin(ALibPath: string): IInterface;
var
  PluginV2: IPluginV2;
  PluginV1: IPluginV1;
begin
  Result := CreatePluginFunc();

  // Essayer la version la plus récente
  if Supports(Result, IPluginV2, PluginV2) then
  begin
    WriteLn('Plugin V2 détecté');
    Exit;
  end;

  // Fallback sur V1
  if Supports(Result, IPluginV1, PluginV1) then
  begin
    WriteLn('Plugin V1 détecté');
    Exit;
  end;

  raise Exception.Create('Version de plugin non supportée');
end;
```

### 2. Isoler les crashes de plugins

```pascal
procedure SafeExecutePlugin(APlugin: IPlugin);
begin
  try
    APlugin.Execute;
  except
    on E: Exception do
    begin
      WriteLn(Format('[SafeExecute] Plugin crashed: %s', [E.Message]));
      // L'application continue même si le plugin plante
    end;
  end;
end;
```

### 3. Documentation du contrat

```pascal
type
  // CONTRAT DU PLUGIN
  // =================
  // 1. Doit implémenter IPlugin
  // 2. Doit exporter CreatePlugin
  // 3. Ne doit pas bloquer dans Execute
  // 4. Doit libérer ses ressources dans Finalize
  // 5. Doit être thread-safe si utilisé en parallèle
  IPlugin = interface
    ['{...}']

    // Appelé UNE FOIS au chargement
    procedure Initialize;

    // Appelé à chaque utilisation (doit être rapide !)
    procedure Execute;

    // Appelé UNE FOIS au déchargement
    procedure Finalize;
  end;
```

### 4. Tester les plugins

```pascal
unit Tests.PluginSystem;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  Plugin.Manager, Plugin.Interfaces;

type
  TTestPluginSystem = class(TTestCase)
  private
    FManager: TPluginManager;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestLoadPlugins;
    procedure TestExecutePlugin;
    procedure TestPluginCrash;
  end;

implementation

procedure TTestPluginSystem.SetUp;
begin
  FManager := TPluginManager.Create('test_plugins');
end;

procedure TTestPluginSystem.TearDown;
begin
  FManager.Free;
end;

procedure TTestPluginSystem.TestLoadPlugins;
begin
  FManager.LoadPlugins;
  AssertTrue('Au moins un plugin chargé', FManager.GetPluginCount > 0);
end;

procedure TTestPluginSystem.TestExecutePlugin;
var
  Plugin: IPlugin;
begin
  FManager.LoadPlugins;
  Plugin := FManager.GetPlugin(0);

  AssertNotNull('Plugin existe', Plugin);

  // Ne doit pas planter
  Plugin.Initialize;
  Plugin.Execute;
  Plugin.Finalize;
end;

procedure TTestPluginSystem.TestPluginCrash;
var
  Plugin: IPlugin;
begin
  // Tester qu'un crash de plugin n'affecte pas l'application
  // (avec un plugin de test qui plante volontairement)

  FManager.LoadPlugins;
  Plugin := FManager.FindPlugin('CrashTestPlugin');

  if not Assigned(Plugin) then
  begin
    // Plugin de test non présent, on skip
    Exit;
  end;

  // Essayer d'exécuter le plugin qui crash
  try
    Plugin.Execute;
    Fail('Le plugin aurait dû crasher');
  except
    on E: Exception do
    begin
      // C'est attendu
      AssertTrue('Exception capturée', True);
    end;
  end;

  // Vérifier que le manager fonctionne encore
  AssertTrue('Manager toujours opérationnel', FManager.GetPluginCount > 0);
end;

initialization
  RegisterTest(TTestPluginSystem);

end.
```

## Exemple complet : Application avec plugins

### Structure du projet

```
MonApplication/
├── src/
│   ├── Main.pas                    ← Application principale
│   ├── Plugin.Interfaces.pas       ← Interfaces communes
│   ├── Plugin.Manager.pas          ← Gestionnaire
│   └── Plugin.GUI.pas              ← Interface graphique
│
├── plugins/
│   ├── HelloWorld/
│   │   └── PluginHelloWorld.lpr   ← Plugin simple
│   ├── Calculator/
│   │   └── PluginCalculator.lpr   ← Plugin calculatrice
│   └── FileExporter/
│       └── PluginFileExporter.lpr ← Plugin export
│
├── build/
│   └── plugins/                    ← Plugins compilés
│       ├── HelloWorld.dll/.so
│       ├── Calculator.dll/.so
│       └── FileExporter.dll/.so
│
└── docs/
    └── plugin-api.md              ← Documentation API
```

### Application principale complète

```pascal
program PluginHostApp;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, Forms, SysUtils,
  Plugin.Manager, Plugin.GUI;

var
  Manager: TPluginManager;
  PluginForm: TPluginManagerForm;
begin
  Application.Title := 'Application avec Plugins';
  Application.Initialize;

  try
    // Créer le gestionnaire de plugins
    Manager := TPluginManager.Create('plugins');

    // Charger les plugins
    Manager.LoadPlugins;

    // Créer l'interface de gestion
    PluginForm := TPluginManagerForm.Create(Application, Manager);
    PluginForm.Show;

    // Lancer l'application
    Application.Run;

    // Nettoyer
    Manager.UnloadPlugins;
    Manager.Free;

  except
    on E: Exception do
    begin
      WriteLn('ERREUR: ', E.Message);
      Halt(1);
    end;
  end;
end.
```

### Plugin complet : Calculatrice

```pascal
library PluginCalculator;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls,
  Plugin.Interfaces;

type
  TCalculatorPlugin = class(TInterfacedObject, IPlugin)
  private
    FInfo: TPluginInfo;
    FForm: TForm;
    FDisplay: TEdit;
    FCurrentValue: Double;
    FOperator: Char;

    procedure CreateUI;
    procedure ButtonClick(Sender: TObject);
    procedure OperatorClick(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;

    function GetInfo: TPluginInfo;
    procedure Initialize;
    procedure Finalize;
    procedure Execute;
  end;

constructor TCalculatorPlugin.Create;
begin
  inherited Create;

  FInfo.Name := 'Calculator';
  FInfo.Version := '1.0.0';
  FInfo.Author := 'Plugin Developer';
  FInfo.Description := 'Calculatrice simple';
  FInfo.Website := 'https://example.com';

  FCurrentValue := 0;
  FOperator := ' ';
end;

destructor TCalculatorPlugin.Destroy;
begin
  if Assigned(FForm) then
    FForm.Free;
  inherited;
end;

function TCalculatorPlugin.GetInfo: TPluginInfo;
begin
  Result := FInfo;
end;

procedure TCalculatorPlugin.Initialize;
begin
  WriteLn('[Calculator] Plugin initialisé');
  CreateUI;
end;

procedure TCalculatorPlugin.Finalize;
begin
  if Assigned(FForm) then
    FreeAndNil(FForm);
  WriteLn('[Calculator] Plugin finalisé');
end;

procedure TCalculatorPlugin.Execute;
begin
  if Assigned(FForm) then
    FForm.Show;
end;

procedure TCalculatorPlugin.CreateUI;
var
  Panel: TPanel;
  Button: TButton;
  i: Integer;
  ButtonNumbers: array[0..9] of string =
    ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9');
  ButtonOps: array[0..3] of string = ('+', '-', '*', '/');
begin
  FForm := TForm.Create(nil);
  FForm.Caption := 'Calculatrice Plugin';
  FForm.Width := 300;
  FForm.Height := 400;
  FForm.Position := poScreenCenter;

  // Affichage
  FDisplay := TEdit.Create(FForm);
  FDisplay.Parent := FForm;
  FDisplay.Align := alTop;
  FDisplay.Height := 40;
  FDisplay.Text := '0';
  FDisplay.ReadOnly := True;
  FDisplay.Font.Size := 16;
  FDisplay.Alignment := taRightJustify;

  // Panel pour les boutons
  Panel := TPanel.Create(FForm);
  Panel.Parent := FForm;
  Panel.Align := alClient;

  // Boutons numériques (0-9)
  for i := 0 to 9 do
  begin
    Button := TButton.Create(Panel);
    Button.Parent := Panel;
    Button.Caption := ButtonNumbers[i];
    Button.Tag := i;
    Button.OnClick := @ButtonClick;
    Button.Width := 60;
    Button.Height := 60;
    Button.Left := 10 + (i mod 3) * 70;
    Button.Top := 10 + (i div 3) * 70;
  end;

  // Boutons opérateurs
  for i := 0 to 3 do
  begin
    Button := TButton.Create(Panel);
    Button.Parent := Panel;
    Button.Caption := ButtonOps[i];
    Button.OnClick := @OperatorClick;
    Button.Width := 60;
    Button.Height := 60;
    Button.Left := 220;
    Button.Top := 10 + i * 70;
  end;

  // Bouton égal
  Button := TButton.Create(Panel);
  Button.Parent := Panel;
  Button.Caption := '=';
  Button.OnClick := @OperatorClick;
  Button.Width := 130;
  Button.Height := 60;
  Button.Left := 10;
  Button.Top := 220;

  // Bouton Clear
  Button := TButton.Create(Panel);
  Button.Parent := Panel;
  Button.Caption := 'C';
  Button.OnClick := @OperatorClick;
  Button.Width := 60;
  Button.Height := 60;
  Button.Left := 150;
  Button.Top := 220;
end;

procedure TCalculatorPlugin.ButtonClick(Sender: TObject);
var
  Digit: string;
begin
  Digit := (Sender as TButton).Caption;

  if FDisplay.Text = '0' then
    FDisplay.Text := Digit
  else
    FDisplay.Text := FDisplay.Text + Digit;
end;

procedure TCalculatorPlugin.OperatorClick(Sender: TObject);
var
  Op: string;
  CurrentDisplay: Double;
begin
  Op := (Sender as TButton).Caption;
  CurrentDisplay := StrToFloatDef(FDisplay.Text, 0);

  if Op = 'C' then
  begin
    FDisplay.Text := '0';
    FCurrentValue := 0;
    FOperator := ' ';
  end
  else if Op = '=' then
  begin
    case FOperator of
      '+': FCurrentValue := FCurrentValue + CurrentDisplay;
      '-': FCurrentValue := FCurrentValue - CurrentDisplay;
      '*': FCurrentValue := FCurrentValue * CurrentDisplay;
      '/': if CurrentDisplay <> 0 then
             FCurrentValue := FCurrentValue / CurrentDisplay;
    end;
    FDisplay.Text := FloatToStr(FCurrentValue);
    FOperator := ' ';
  end
  else
  begin
    if FOperator <> ' ' then
    begin
      // Calculer l'opération précédente
      case FOperator of
        '+': FCurrentValue := FCurrentValue + CurrentDisplay;
        '-': FCurrentValue := FCurrentValue - CurrentDisplay;
        '*': FCurrentValue := FCurrentValue * CurrentDisplay;
        '/': if CurrentDisplay <> 0 then
               FCurrentValue := FCurrentValue / CurrentDisplay;
      end;
    end
    else
      FCurrentValue := CurrentDisplay;

    FOperator := Op[1];
    FDisplay.Text := '0';
  end;
end;

// Fonction exportée
function CreatePlugin: IPlugin;
{$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
begin
  Result := TCalculatorPlugin.Create;
end;

exports
  CreatePlugin;

begin
end.
```

### Plugin : Export de fichiers

```pascal
library PluginFileExporter;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, Dialogs, fpjson,
  Plugin.Interfaces, Plugin.Interfaces.Extended;

type
  TExportFormat = (efCSV, efJSON, efXML);

  TFileExporterPlugin = class(TInterfacedObject, IConfigurablePlugin)
  private
    FInfo: TPluginInfo;
    FDefaultFormat: TExportFormat;
    FDefaultPath: string;

    procedure ExportToCSV(const AData: TStringList; const AFileName: string);
    procedure ExportToJSON(const AData: TStringList; const AFileName: string);
    procedure ExportToXML(const AData: TStringList; const AFileName: string);
  public
    constructor Create;

    // IPlugin
    function GetInfo: TPluginInfo;
    procedure Initialize;
    procedure Finalize;
    procedure Execute;

    // IConfigurablePlugin
    procedure LoadConfig(AConfig: TJSONObject);
    procedure SaveConfig(AConfig: TJSONObject);
    function GetDefaultConfig: TJSONObject;

    // Fonctionnalités propres
    procedure ExportData(const AData: TStringList; AFormat: TExportFormat);
  end;

constructor TFileExporterPlugin.Create;
begin
  inherited Create;

  FInfo.Name := 'File Exporter';
  FInfo.Version := '1.0.0';
  FInfo.Author := 'Plugin Developer';
  FInfo.Description := 'Export de données en CSV, JSON ou XML';
  FInfo.Website := 'https://example.com';

  FDefaultFormat := efJSON;
  FDefaultPath := GetCurrentDir;
end;

function TFileExporterPlugin.GetInfo: TPluginInfo;
begin
  Result := FInfo;
end;

procedure TFileExporterPlugin.Initialize;
begin
  WriteLn('[FileExporter] Plugin initialisé');
end;

procedure TFileExporterPlugin.Finalize;
begin
  WriteLn('[FileExporter] Plugin finalisé');
end;

procedure TFileExporterPlugin.Execute;
var
  SampleData: TStringList;
begin
  // Démonstration avec des données d'exemple
  SampleData := TStringList.Create;
  try
    SampleData.Add('Nom,Âge,Ville');
    SampleData.Add('Alice,30,Paris');
    SampleData.Add('Bob,25,Lyon');
    SampleData.Add('Charlie,35,Marseille');

    ExportData(SampleData, FDefaultFormat);
  finally
    SampleData.Free;
  end;
end;

procedure TFileExporterPlugin.LoadConfig(AConfig: TJSONObject);
begin
  FDefaultFormat := TExportFormat(AConfig.Get('default_format', 1));
  FDefaultPath := AConfig.Get('default_path', GetCurrentDir);

  WriteLn(Format('[FileExporter] Configuration chargée: Format=%d, Path=%s',
    [Ord(FDefaultFormat), FDefaultPath]));
end;

procedure TFileExporterPlugin.SaveConfig(AConfig: TJSONObject);
begin
  AConfig.Add('default_format', Ord(FDefaultFormat));
  AConfig.Add('default_path', FDefaultPath);
end;

function TFileExporterPlugin.GetDefaultConfig: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.Add('default_format', 1); // JSON par défaut
  Result.Add('default_path', GetCurrentDir);
end;

procedure TFileExporterPlugin.ExportData(const AData: TStringList;
  AFormat: TExportFormat);
var
  SaveDialog: TSaveDialog;
  FileName: string;
begin
  SaveDialog := TSaveDialog.Create(nil);
  try
    case AFormat of
      efCSV:
        begin
          SaveDialog.Filter := 'Fichiers CSV (*.csv)|*.csv';
          SaveDialog.DefaultExt := 'csv';
        end;
      efJSON:
        begin
          SaveDialog.Filter := 'Fichiers JSON (*.json)|*.json';
          SaveDialog.DefaultExt := 'json';
        end;
      efXML:
        begin
          SaveDialog.Filter := 'Fichiers XML (*.xml)|*.xml';
          SaveDialog.DefaultExt := 'xml';
        end;
    end;

    SaveDialog.InitialDir := FDefaultPath;

    if SaveDialog.Execute then
    begin
      FileName := SaveDialog.FileName;

      case AFormat of
        efCSV: ExportToCSV(AData, FileName);
        efJSON: ExportToJSON(AData, FileName);
        efXML: ExportToXML(AData, FileName);
      end;

      ShowMessage('Export réussi: ' + FileName);
    end;
  finally
    SaveDialog.Free;
  end;
end;

procedure TFileExporterPlugin.ExportToCSV(const AData: TStringList;
  const AFileName: string);
begin
  AData.SaveToFile(AFileName);
  WriteLn('[FileExporter] Export CSV: ', AFileName);
end;

procedure TFileExporterPlugin.ExportToJSON(const AData: TStringList;
  const AFileName: string);
var
  JSON: TJSONArray;
  Row: TJSONObject;
  i: Integer;
  Fields, Headers: TStringList;
begin
  if AData.Count < 2 then Exit;

  Headers := TStringList.Create;
  Fields := TStringList.Create;
  JSON := TJSONArray.Create;
  try
    // La première ligne contient les en-têtes
    Headers.CommaText := AData[0];

    // Convertir les lignes en objets JSON
    for i := 1 to AData.Count - 1 do
    begin
      Fields.CommaText := AData[i];
      Row := TJSONObject.Create;

      if Fields.Count = Headers.Count then
      begin
        Row.Add(Headers[0], Fields[0]);
        Row.Add(Headers[1], StrToIntDef(Fields[1], 0));
        Row.Add(Headers[2], Fields[2]);
      end;

      JSON.Add(Row);
    end;

    // Sauvegarder
    with TStringList.Create do
    try
      Text := JSON.FormatJSON;
      SaveToFile(AFileName);
    finally
      Free;
    end;

  finally
    JSON.Free;
    Headers.Free;
    Fields.Free;
  end;

  WriteLn('[FileExporter] Export JSON: ', AFileName);
end;

procedure TFileExporterPlugin.ExportToXML(const AData: TStringList;
  const AFileName: string);
var
  XML, Fields, Headers: TStringList;
  i: Integer;
begin
  if AData.Count < 2 then Exit;

  XML := TStringList.Create;
  Headers := TStringList.Create;
  Fields := TStringList.Create;
  try
    XML.Add('<?xml version="1.0" encoding="UTF-8"?>');
    XML.Add('<data>');

    Headers.CommaText := AData[0];

    for i := 1 to AData.Count - 1 do
    begin
      Fields.CommaText := AData[i];

      if Fields.Count = Headers.Count then
      begin
        XML.Add('  <item>');
        XML.Add(Format('    <%s>%s</%s>', [Headers[0], Fields[0], Headers[0]]));
        XML.Add(Format('    <%s>%s</%s>', [Headers[1], Fields[1], Headers[1]]));
        XML.Add(Format('    <%s>%s</%s>', [Headers[2], Fields[2], Headers[2]]));
        XML.Add('  </item>');
      end;
    end;

    XML.Add('</data>');
    XML.SaveToFile(AFileName);
  finally
    XML.Free;
    Headers.Free;
    Fields.Free;
  end;

  WriteLn('[FileExporter] Export XML: ', AFileName);
end;

// Fonction exportée
function CreatePlugin: IPlugin;
{$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
begin
  Result := TFileExporterPlugin.Create;
end;

exports
  CreatePlugin;

begin
end.
```

## Avantages et inconvénients

### ✅ Avantages

1. **Extensibilité maximale**
   - Nouvelles fonctionnalités sans recompilation
   - Écosystème de plugins tiers possible

2. **Modularité**
   - Développement indépendant
   - Tests isolés

3. **Distribution flexible**
   - Fonctionnalités optionnelles
   - Versions différentes par utilisateur

4. **Maintenance facilitée**
   - Corriger un plugin sans toucher à l'application
   - Mises à jour indépendantes

5. **Personnalisation**
   - Chaque utilisateur active ce dont il a besoin
   - Réduction de la complexité apparente

### ❌ Inconvénients

1. **Complexité technique**
   - Gestion du chargement dynamique
   - Versioning des interfaces
   - Compatibilité

2. **Performance**
   - Overhead du chargement dynamique
   - Communication inter-plugins

3. **Sécurité**
   - Plugins malveillants possibles
   - Nécessite sandbox et validation

4. **Documentation**
   - API à documenter
   - Exemples nécessaires

5. **Support**
   - Plus difficile de déboguer
   - Problèmes de compatibilité

## Quand utiliser une architecture plugin ?

### ✅ Utilisez des plugins quand :

- **Extensibilité requise** : Application qui évoluera beaucoup
- **Écosystème** : Vous voulez permettre les contributions tierces
- **Modularité** : Fonctionnalités clairement séparées
- **Distribution** : Certaines fonctions sont optionnelles/payantes
- **Personnalisation** : Chaque utilisateur a des besoins différents
- **Long terme** : Projet qui vivra plusieurs années

### ❌ Évitez les plugins quand :

- **Application simple** : Fonctionnalités fixes et limitées
- **Performance critique** : Overhead inacceptable
- **Petite équipe** : Trop de complexité
- **Prototype** : MVP rapide nécessaire
- **Sécurité critique** : Risques inacceptables
- **Monolithique** : Tout doit être intégré

## Exemples réels avec FreePascal/Lazarus

### 1. Lazarus IDE lui-même

Lazarus utilise une architecture de packages/plugins :

```pascal
// Package Lazarus
unit MyLazPackage;

interface

uses
  Classes, SysUtils, LazIDEIntf, MenuIntf;

procedure Register;

implementation

procedure DoSomething(Sender: TObject);
begin
  ShowMessage('Action du plugin !');
end;

procedure Register;
begin
  RegisterIDEMenuCommand(itmSourceInsertions,
    'MyPluginCommand',
    'Mon Plugin',
    @DoSomething);
end;

end.
```

### 2. Double Commander

Gestionnaire de fichiers avec plugins pour :
- Packers/Unpackers
- File system plugins
- Content plugins
- Lister plugins

### 3. Applications multimedia

Plugins VST (Virtual Studio Technology) pour effets audio.

## Conclusion

L'**architecture plugin** est un pattern puissant pour créer des applications extensibles et modulaires.

### Points clés à retenir :

1. **Interface commune** : Contrat que tous les plugins respectent
2. **Chargement dynamique** : DLL/SO chargées au runtime
3. **Isolation** : Chaque plugin est indépendant
4. **Gestion centralisée** : Plugin Manager coordonne
5. **Portabilité** : Même concept sur Windows/Linux/macOS

### Bénéfices avec FreePascal :

- **Interfaces natives** : Support parfait des contrats
- **Chargement dynamique** : dynlibs fonctionne bien
- **Performance** : Overhead minimal
- **Portabilité** : Même code multi-plateforme
- **Stabilité** : Compilation native fiable

### Recommandations :

**Pour démarrer :**
1. Définir des interfaces simples et claires
2. Créer un plugin manager basique
3. Développer 2-3 plugins de test
4. Documenter l'API
5. Itérer et améliorer

**Pour production :**
1. Ajouter la gestion des versions
2. Implémenter la sécurité (sandbox, signatures)
3. Créer un système de dépendances
4. Fournir des outils de développement
5. Construire une communauté

L'architecture plugin est particulièrement adaptée aux **applications d'entreprise**, **outils de développement**, **éditeurs**, et **applications extensibles** où la flexibilité et l'évolutivité sont primordiales. 🔌

FreePascal offre tous les outils nécessaires pour implémenter cette architecture de manière robuste et portable sur toutes les plateformes majeures.

⏭️ [Message queues et event bus](/21-architecture-logicielle-avancee/07-message-queues-event-bus.md)
