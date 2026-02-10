🔝 Retour au [Sommaire](/SOMMAIRE.md)

# Les Standards Freedesktop.org avec FreePascal/Lazarus

## Introduction aux Standards Freedesktop.org

### Qu'est-ce que Freedesktop.org ?

Freedesktop.org (anciennement connu sous le nom de XDG - X Desktop Group) est un projet qui vise à améliorer l'interopérabilité et la cohérence entre les différents environnements de bureau Linux et Unix. Ces standards permettent aux développeurs de créer des applications qui fonctionnent de manière uniforme, que l'utilisateur soit sous GNOME, KDE, XFCE ou tout autre environnement de bureau.

Imaginez que vous développez une application. Sans standards communs, vous devriez écrire du code différent pour chaque environnement de bureau : un code pour sauvegarder les préférences sous GNOME, un autre pour KDE, etc. Grâce aux standards Freedesktop.org, vous écrivez une seule fois et votre application fonctionne partout.

### Pourquoi ces standards sont-ils importants ?

Les standards Freedesktop.org garantissent que votre application FreePascal/Lazarus :
- Apparaît correctement dans les menus de tous les environnements de bureau
- Stocke ses fichiers aux bons endroits
- S'intègre visuellement avec le thème du système
- Peut communiquer avec d'autres applications
- Gère correctement les types de fichiers
- Respecte les préférences de l'utilisateur

## XDG Base Directory Specification

### Le principe fondamental

La spécification XDG Base Directory définit où votre application doit stocker ses différents types de fichiers. C'est l'un des standards les plus importants à comprendre et respecter.

Avant cette standardisation, chaque application stockait ses fichiers où elle voulait, créant un désordre dans le répertoire home de l'utilisateur. Maintenant, il existe des emplacements standardisés pour chaque type de données.

### Les variables d'environnement XDG

Le système définit plusieurs variables d'environnement qui indiquent où stocker les fichiers :

```pascal
program ShowXDGDirectories;  
uses
  SysUtils;

procedure ShowXDGPaths;  
var
  ConfigHome, DataHome, CacheHome, RuntimeDir: string;
begin
  // Configuration (paramètres, préférences)
  ConfigHome := GetEnvironmentVariable('XDG_CONFIG_HOME');
  if ConfigHome = '' then
    ConfigHome := GetEnvironmentVariable('HOME') + '/.config';
  WriteLn('Configuration : ', ConfigHome);

  // Données (fichiers de données de l'application)
  DataHome := GetEnvironmentVariable('XDG_DATA_HOME');
  if DataHome = '' then
    DataHome := GetEnvironmentVariable('HOME') + '/.local/share';
  WriteLn('Données : ', DataHome);

  // Cache (fichiers temporaires réutilisables)
  CacheHome := GetEnvironmentVariable('XDG_CACHE_HOME');
  if CacheHome = '' then
    CacheHome := GetEnvironmentVariable('HOME') + '/.cache';
  WriteLn('Cache : ', CacheHome);

  // Runtime (fichiers temporaires de session)
  RuntimeDir := GetEnvironmentVariable('XDG_RUNTIME_DIR');
  if RuntimeDir <> '' then
    WriteLn('Runtime : ', RuntimeDir);
end;

begin
  ShowXDGPaths;
end.
```

### Utilisation pratique dans votre application

Voici une unité réutilisable pour gérer les chemins XDG dans vos applications Lazarus :

```pascal
unit XDGDirectories;

interface

uses
  SysUtils, FileUtil;

type
  TXDGManager = class
  private
    FAppName: string;
    function GetBaseDir(const EnvVar, DefaultPath: string): string;
  public
    constructor Create(const AppName: string);

    function GetConfigDir: string;
    function GetDataDir: string;
    function GetCacheDir: string;
    function GetRuntimeDir: string;

    // Méthodes utilitaires
    function GetConfigFile(const FileName: string): string;
    function GetDataFile(const FileName: string): string;
    procedure EnsureDirectoriesExist;
  end;

implementation

constructor TXDGManager.Create(const AppName: string);  
begin
  FAppName := AppName;
  EnsureDirectoriesExist;
end;

function TXDGManager.GetBaseDir(const EnvVar, DefaultPath: string): string;  
begin
  Result := GetEnvironmentVariable(EnvVar);
  if Result = '' then
    Result := GetEnvironmentVariable('HOME') + '/' + DefaultPath;
end;

function TXDGManager.GetConfigDir: string;  
begin
  Result := GetBaseDir('XDG_CONFIG_HOME', '.config') + '/' + FAppName;
end;

function TXDGManager.GetDataDir: string;  
begin
  Result := GetBaseDir('XDG_DATA_HOME', '.local/share') + '/' + FAppName;
end;

function TXDGManager.GetCacheDir: string;  
begin
  Result := GetBaseDir('XDG_CACHE_HOME', '.cache') + '/' + FAppName;
end;

function TXDGManager.GetRuntimeDir: string;  
var
  RuntimeBase: string;
begin
  RuntimeBase := GetEnvironmentVariable('XDG_RUNTIME_DIR');
  if RuntimeBase <> '' then
    Result := RuntimeBase + '/' + FAppName
  else
    Result := '/tmp/' + FAppName + '-' + GetEnvironmentVariable('USER');
end;

function TXDGManager.GetConfigFile(const FileName: string): string;  
begin
  Result := GetConfigDir + '/' + FileName;
end;

function TXDGManager.GetDataFile(const FileName: string): string;  
begin
  Result := GetDataDir + '/' + FileName;
end;

procedure TXDGManager.EnsureDirectoriesExist;  
begin
  ForceDirectories(GetConfigDir);
  ForceDirectories(GetDataDir);
  ForceDirectories(GetCacheDir);

  // Le répertoire runtime peut nécessiter des permissions spéciales
  if GetEnvironmentVariable('XDG_RUNTIME_DIR') <> '' then
    ForceDirectories(GetRuntimeDir);
end;

end.
```

### Que stocker dans chaque répertoire ?

**Config ($XDG_CONFIG_HOME)** : Les fichiers de configuration et préférences
- Paramètres de l'application
- Préférences utilisateur
- Thèmes personnalisés
- Configuration des plugins

**Data ($XDG_DATA_HOME)** : Les données créées ou modifiées par l'utilisateur
- Documents créés
- Bases de données locales
- Fichiers de sauvegarde
- Modèles personnalisés

**Cache ($XDG_CACHE_HOME)** : Les fichiers temporaires réutilisables
- Images miniatures
- Fichiers téléchargés temporairement
- Données compilées
- Index de recherche

**Runtime ($XDG_RUNTIME_DIR)** : Les fichiers de session temporaires
- Sockets Unix
- Verrous de fichiers
- Fichiers PID
- Données de session

### Exemple d'utilisation complète

```pascal
program MyApplication;  
uses
  XDGDirectories, IniFiles;

var
  XDG: TXDGManager;
  Config: TIniFile;

procedure LoadSettings;  
begin
  Config := TIniFile.Create(XDG.GetConfigFile('settings.ini'));
  try
    // Charger les paramètres
    WindowWidth := Config.ReadInteger('Window', 'Width', 800);
    WindowHeight := Config.ReadInteger('Window', 'Height', 600);
    LastOpenFile := Config.ReadString('Recent', 'LastFile', '');
  finally
    Config.Free;
  end;
end;

procedure SaveSettings;  
begin
  Config := TIniFile.Create(XDG.GetConfigFile('settings.ini'));
  try
    Config.WriteInteger('Window', 'Width', WindowWidth);
    Config.WriteInteger('Window', 'Height', WindowHeight);
    Config.WriteString('Recent', 'LastFile', LastOpenFile);
  finally
    Config.Free;
  end;
end;

begin
  XDG := TXDGManager.Create('myapplication');
  try
    LoadSettings;
    // ... Code de l'application ...
    SaveSettings;
  finally
    XDG.Free;
  end;
end.
```

## Desktop Entry Specification

### Le fichier .desktop

Le fichier `.desktop` est la carte d'identité de votre application pour le système. Il indique au système comment afficher et lancer votre application.

### Structure de base d'un fichier .desktop

```ini
[Desktop Entry]
# Version de la spécification (toujours 1.0 actuellement)
Version=1.0

# Type d'entrée (Application, Link, ou Directory)
Type=Application

# Nom de l'application
Name=Mon Application  
Name[fr]=Mon Application  
Name[es]=Mi Aplicación

# Description courte
Comment=Une application créée avec Lazarus  
Comment[fr]=Une application créée avec Lazarus  
Comment[es]=Una aplicación creada con Lazarus

# Commande pour lancer l'application
Exec=/usr/bin/monapplication %f

# Icône de l'application
Icon=monapplication

# Ouvrir dans un terminal ?
Terminal=false

# Catégories (voir la liste officielle)
Categories=Office;Utility;

# Types MIME supportés
MimeType=application/x-monapplication;text/plain;

# Mots-clés pour la recherche
Keywords=office;document;text;  
Keywords[fr]=bureau;document;texte;
```

### Catégories standards

Les catégories principales définies par la spécification :

```pascal
const
  // Catégories principales
  CATEGORY_AUDIO_VIDEO = 'AudioVideo';
  CATEGORY_AUDIO = 'Audio';
  CATEGORY_VIDEO = 'Video';
  CATEGORY_DEVELOPMENT = 'Development';
  CATEGORY_EDUCATION = 'Education';
  CATEGORY_GAME = 'Game';
  CATEGORY_GRAPHICS = 'Graphics';
  CATEGORY_NETWORK = 'Network';
  CATEGORY_OFFICE = 'Office';
  CATEGORY_SCIENCE = 'Science';
  CATEGORY_SETTINGS = 'Settings';
  CATEGORY_SYSTEM = 'System';
  CATEGORY_UTILITY = 'Utility';
```

### Génération automatique du fichier .desktop

Créez une classe pour générer automatiquement le fichier .desktop :

```pascal
unit DesktopEntry;

interface

uses
  Classes, SysUtils;

type
  TDesktopEntry = class
  private
    FEntries: TStringList;
    procedure AddEntry(const Key, Value: string);
    procedure AddLocalizedEntry(const Key, Value, Lang: string);
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetBasicInfo(const Name, Comment, Exec, Icon: string);
    procedure AddName(const Lang, Value: string);
    procedure AddComment(const Lang, Value: string);
    procedure AddCategory(const Category: string);
    procedure AddMimeType(const MimeType: string);
    procedure AddKeyword(const Keyword: string);
    procedure AddKeywordLocalized(const Lang, Keyword: string);

    procedure SaveToFile(const FileName: string);
    function GenerateContent: string;
  end;

implementation

constructor TDesktopEntry.Create;  
begin
  FEntries := TStringList.Create;
  FEntries.Add('[Desktop Entry]');
  AddEntry('Version', '1.0');
  AddEntry('Type', 'Application');
  AddEntry('Terminal', 'false');
end;

destructor TDesktopEntry.Destroy;  
begin
  FEntries.Free;
  inherited;
end;

procedure TDesktopEntry.AddEntry(const Key, Value: string);  
begin
  FEntries.Add(Key + '=' + Value);
end;

procedure TDesktopEntry.AddLocalizedEntry(const Key, Value, Lang: string);  
begin
  FEntries.Add(Key + '[' + Lang + ']=' + Value);
end;

procedure TDesktopEntry.SetBasicInfo(const Name, Comment, Exec, Icon: string);  
begin
  AddEntry('Name', Name);
  AddEntry('Comment', Comment);
  AddEntry('Exec', Exec);
  AddEntry('Icon', Icon);
end;

procedure TDesktopEntry.AddName(const Lang, Value: string);  
begin
  AddLocalizedEntry('Name', Value, Lang);
end;

procedure TDesktopEntry.AddComment(const Lang, Value: string);  
begin
  AddLocalizedEntry('Comment', Value, Lang);
end;

procedure TDesktopEntry.AddCategory(const Category: string);  
var
  Index: Integer;
  CurrentCategories: string;
begin
  // Chercher si Categories existe déjà
  for Index := 0 to FEntries.Count - 1 do
  begin
    if Pos('Categories=', FEntries[Index]) = 1 then
    begin
      CurrentCategories := Copy(FEntries[Index], 12, MaxInt);
      if Pos(Category, CurrentCategories) = 0 then
      begin
        if CurrentCategories[Length(CurrentCategories)] <> ';' then
          CurrentCategories := CurrentCategories + ';';
        FEntries[Index] := 'Categories=' + CurrentCategories + Category + ';';
      end;
      Exit;
    end;
  end;
  // Si Categories n'existe pas, l'ajouter
  AddEntry('Categories', Category + ';');
end;

procedure TDesktopEntry.AddMimeType(const MimeType: string);  
var
  Index: Integer;
  CurrentMimeTypes: string;
begin
  for Index := 0 to FEntries.Count - 1 do
  begin
    if Pos('MimeType=', FEntries[Index]) = 1 then
    begin
      CurrentMimeTypes := Copy(FEntries[Index], 10, MaxInt);
      if Pos(MimeType, CurrentMimeTypes) = 0 then
      begin
        if CurrentMimeTypes[Length(CurrentMimeTypes)] <> ';' then
          CurrentMimeTypes := CurrentMimeTypes + ';';
        FEntries[Index] := 'MimeType=' + CurrentMimeTypes + MimeType + ';';
      end;
      Exit;
    end;
  end;
  AddEntry('MimeType', MimeType + ';');
end;

procedure TDesktopEntry.AddKeyword(const Keyword: string);  
var
  Index: Integer;
  CurrentKeywords: string;
begin
  for Index := 0 to FEntries.Count - 1 do
  begin
    if Pos('Keywords=', FEntries[Index]) = 1 then
    begin
      CurrentKeywords := Copy(FEntries[Index], 10, MaxInt);
      FEntries[Index] := 'Keywords=' + CurrentKeywords + Keyword + ';';
      Exit;
    end;
  end;
  AddEntry('Keywords', Keyword + ';');
end;

procedure TDesktopEntry.AddKeywordLocalized(const Lang, Keyword: string);  
begin
  AddLocalizedEntry('Keywords', Keyword + ';', Lang);
end;

procedure TDesktopEntry.SaveToFile(const FileName: string);  
begin
  FEntries.SaveToFile(FileName);
end;

function TDesktopEntry.GenerateContent: string;  
begin
  Result := FEntries.Text;
end;

end.
```

### Installation du fichier .desktop

```pascal
procedure InstallDesktopFile;  
var
  Desktop: TDesktopEntry;
  InstallPath: string;
begin
  Desktop := TDesktopEntry.Create;
  try
    Desktop.SetBasicInfo(
      'Mon Application',
      'Une super application Lazarus',
      '/usr/bin/monapplication',
      'monapplication'
    );

    Desktop.AddName('fr', 'Mon Application');
    Desktop.AddName('es', 'Mi Aplicación');
    Desktop.AddComment('fr', 'Une super application Lazarus');
    Desktop.AddComment('es', 'Una gran aplicación Lazarus');

    Desktop.AddCategory('Office');
    Desktop.AddCategory('Utility');

    Desktop.AddMimeType('application/x-monapplication');
    Desktop.AddMimeType('text/plain');

    Desktop.AddKeyword('office');
    Desktop.AddKeyword('document');
    Desktop.AddKeywordLocalized('fr', 'bureau');
    Desktop.AddKeywordLocalized('fr', 'document');

    // Installation pour l'utilisateur courant
    InstallPath := GetEnvironmentVariable('HOME') +
                   '/.local/share/applications/monapplication.desktop';
    Desktop.SaveToFile(InstallPath);

    // Mise à jour de la base de données des applications
    ExecuteProcess('update-desktop-database',
                   [GetEnvironmentVariable('HOME') + '/.local/share/applications']);
  finally
    Desktop.Free;
  end;
end;
```

## MIME Types et Associations de Fichiers

### Qu'est-ce qu'un type MIME ?

MIME (Multipurpose Internet Mail Extensions) est un standard pour identifier le type d'un fichier. Sous Linux, ce système est utilisé pour associer des fichiers à des applications.

### Définir un nouveau type MIME

Créez un fichier XML pour définir votre type MIME personnalisé :

```xml
<?xml version="1.0" encoding="UTF-8"?>
<mime-info xmlns="http://www.freedesktop.org/standards/shared-mime-info">
  <mime-type type="application/x-monapplication-document">
    <comment>Document Mon Application</comment>
    <comment xml:lang="fr">Document Mon Application</comment>
    <comment xml:lang="es">Documento Mi Aplicación</comment>

    <!-- Correspondance par extension -->
    <glob pattern="*.madoc"/>
    <glob pattern="*.madoc.bak" weight="40"/>

    <!-- Correspondance par contenu (magic) -->
    <magic priority="50">
      <match type="string" offset="0" value="MADOC"/>
    </magic>

    <!-- Icône pour ce type de fichier -->
    <icon name="application-x-monapplication-document"/>

    <!-- Type parent (optionnel) -->
    <sub-class-of type="application/xml"/>
  </mime-type>
</mime-info>
```

### Classe pour gérer les types MIME

```pascal
unit MimeTypeManager;

interface

uses
  Classes, SysUtils, DOM, XMLWrite;

type
  TMimeTypeManager = class
  private
    FMimeType: string;
    FComment: string;
    FExtensions: TStringList;
    FLocalizedComments: TStringList;
    FMagicRules: TStringList;
  public
    constructor Create(const MimeType: string);
    destructor Destroy; override;

    procedure SetComment(const Comment: string; const Lang: string = '');
    procedure AddExtension(const Extension: string; Weight: Integer = 50);
    procedure AddMagic(const Offset: Integer; const Value: string);
    procedure SetIcon(const IconName: string);
    procedure SetParentType(const ParentMime: string);

    function GenerateXML: string;
    procedure SaveToFile(const FileName: string);
    procedure Install;
  end;

implementation

uses
  Process;

constructor TMimeTypeManager.Create(const MimeType: string);  
begin
  FMimeType := MimeType;
  FExtensions := TStringList.Create;
  FLocalizedComments := TStringList.Create;
  FMagicRules := TStringList.Create;
end;

destructor TMimeTypeManager.Destroy;  
begin
  FExtensions.Free;
  FLocalizedComments.Free;
  FMagicRules.Free;
  inherited;
end;

procedure TMimeTypeManager.SetComment(const Comment: string; const Lang: string = '');  
begin
  if Lang = '' then
    FComment := Comment
  else
    FLocalizedComments.Add(Lang + '=' + Comment);
end;

procedure TMimeTypeManager.AddExtension(const Extension: string; Weight: Integer = 50);  
begin
  FExtensions.Add(Format('%s=%d', [Extension, Weight]));
end;

procedure TMimeTypeManager.AddMagic(const Offset: Integer; const Value: string);  
begin
  FMagicRules.Add(Format('%d=%s', [Offset, Value]));
end;

function TMimeTypeManager.GenerateXML: string;  
var
  XML: TStringList;
  i: Integer;
  Parts: TStringList;
begin
  XML := TStringList.Create;
  Parts := TStringList.Create;
  try
    XML.Add('<?xml version="1.0" encoding="UTF-8"?>');
    XML.Add('<mime-info xmlns="http://www.freedesktop.org/standards/shared-mime-info">');
    XML.Add(Format('  <mime-type type="%s">', [FMimeType]));

    // Commentaire principal
    if FComment <> '' then
      XML.Add(Format('    <comment>%s</comment>', [FComment]));

    // Commentaires localisés
    for i := 0 to FLocalizedComments.Count - 1 do
    begin
      Parts.Clear;
      Parts.Delimiter := '=';
      Parts.DelimitedText := FLocalizedComments[i];
      if Parts.Count = 2 then
        XML.Add(Format('    <comment xml:lang="%s">%s</comment>',
                       [Parts[0], Parts[1]]));
    end;

    // Extensions
    for i := 0 to FExtensions.Count - 1 do
    begin
      Parts.Clear;
      Parts.Delimiter := '=';
      Parts.DelimitedText := FExtensions[i];
      if Parts.Count = 2 then
      begin
        if Parts[1] = '50' then
          XML.Add(Format('    <glob pattern="*.%s"/>', [Parts[0]]))
        else
          XML.Add(Format('    <glob pattern="*.%s" weight="%s"/>',
                         [Parts[0], Parts[1]]));
      end;
    end;

    // Magic rules
    if FMagicRules.Count > 0 then
    begin
      XML.Add('    <magic priority="50">');
      for i := 0 to FMagicRules.Count - 1 do
      begin
        Parts.Clear;
        Parts.Delimiter := '=';
        Parts.DelimitedText := FMagicRules[i];
        if Parts.Count = 2 then
          XML.Add(Format('      <match type="string" offset="%s" value="%s"/>',
                         [Parts[0], Parts[1]]));
      end;
      XML.Add('    </magic>');
    end;

    XML.Add('  </mime-type>');
    XML.Add('</mime-info>');

    Result := XML.Text;
  finally
    XML.Free;
    Parts.Free;
  end;
end;

procedure TMimeTypeManager.SaveToFile(const FileName: string);  
var
  Content: TStringList;
begin
  Content := TStringList.Create;
  try
    Content.Text := GenerateXML;
    Content.SaveToFile(FileName);
  finally
    Content.Free;
  end;
end;

procedure TMimeTypeManager.Install;  
var
  Process: TProcess;
  TempFile: string;
begin
  // Sauvegarder dans un fichier temporaire
  TempFile := '/tmp/' + ExtractFileName(FMimeType) + '.xml';
  SaveToFile(TempFile);

  Process := TProcess.Create(nil);
  try
    // Installer le type MIME
    Process.Executable := 'xdg-mime';
    Process.Parameters.Add('install');
    Process.Parameters.Add(TempFile);
    Process.Options := [poWaitOnExit];
    Process.Execute;

    // Mettre à jour la base de données
    Process.Parameters.Clear;
    Process.Executable := 'update-mime-database';
    Process.Parameters.Add(GetEnvironmentVariable('HOME') + '/.local/share/mime');
    Process.Execute;
  finally
    Process.Free;
    DeleteFile(TempFile);
  end;
end;

end.
```

## Icon Theme Specification

### Structure des thèmes d'icônes

Les icônes sous Linux suivent une hiérarchie standardisée :

```
~/.local/share/icons/
└── hicolor/
    ├── 16x16/
    │   └── apps/
    │       └── monapplication.png
    ├── 22x22/
    │   └── apps/
    │       └── monapplication.png
    ├── 32x32/
    │   └── apps/
    │       └── monapplication.png
    ├── 48x48/
    │   └── apps/
    │       └── monapplication.png
    ├── 64x64/
    │   └── apps/
    │       └── monapplication.png
    ├── 128x128/
    │   └── apps/
    │       └── monapplication.png
    ├── 256x256/
    │   └── apps/
    │       └── monapplication.png
    └── scalable/
        └── apps/
            └── monapplication.svg
```

### Gestionnaire d'icônes pour votre application

```pascal
unit IconManager;

interface

uses
  Classes, SysUtils, Graphics, FPImage, FPWritePNG;

type
  TIconManager = class
  private
    FAppName: string;
    FSourceIcon: TPicture;
    function GetIconPath(Size: Integer; IsScalable: Boolean = False): string;
  public
    constructor Create(const AppName: string);
    destructor Destroy; override;

    procedure LoadSourceIcon(const FileName: string);
    procedure GenerateIconSizes;
    procedure InstallIcon(Size: Integer);
    procedure InstallAllIcons;
    procedure UpdateIconCache;
  end;

implementation

uses
  Process, GraphType, IntfGraphics, FPCanvas;

constructor TIconManager.Create(const AppName: string);  
begin
  FAppName := AppName;
  FSourceIcon := TPicture.Create;
end;

destructor TIconManager.Destroy;  
begin
  FSourceIcon.Free;
  inherited;
end;

function TIconManager.GetIconPath(Size: Integer; IsScalable: Boolean = False): string;  
var
  BaseDir, SizeDir: string;
begin
  BaseDir := GetEnvironmentVariable('HOME') + '/.local/share/icons/hicolor/';

  if IsScalable then
    SizeDir := 'scalable'
  else
    SizeDir := IntToStr(Size) + 'x' + IntToStr(Size);

  Result := BaseDir + SizeDir + '/apps/';
  ForceDirectories(Result);
  Result := Result + FAppName + '.png';
  if IsScalable then
    Result := StringReplace(Result, '.png', '.svg', []);
end;

procedure TIconManager.LoadSourceIcon(const FileName: string);  
begin
  FSourceIcon.LoadFromFile(FileName);
end;

procedure TIconManager.GenerateIconSizes;  
const
  IconSizes: array[0..7] of Integer = (16, 22, 24, 32, 48, 64, 128, 256);
var
  i: Integer;
  DestBitmap: TBitmap;
  DestRect: TRect;
begin
  for i := 0 to High(IconSizes) do
  begin
    DestBitmap := TBitmap.Create;
    try
      DestBitmap.Width := IconSizes[i];
      DestBitmap.Height := IconSizes[i];

      // Redimensionner l'image source
      DestRect := Rect(0, 0, IconSizes[i], IconSizes[i]);
      DestBitmap.Canvas.StretchDraw(DestRect, FSourceIcon.Graphic);

      // Sauvegarder au bon emplacement
      DestBitmap.SaveToFile(GetIconPath(IconSizes[i]));
    finally
      DestBitmap.Free;
    end;
  end;
end;

procedure TIconManager.InstallIcon(Size: Integer);  
var
  SourceFile, DestFile: string;
begin
  SourceFile := ExtractFilePath(ParamStr(0)) + 'icons/' +
                IntToStr(Size) + 'x' + IntToStr(Size) + '/' +
                FAppName + '.png';
  DestFile := GetIconPath(Size);

  if FileExists(SourceFile) then
    CopyFile(SourceFile, DestFile);
end;

procedure TIconManager.InstallAllIcons;  
const
  StandardSizes: array[0..7] of Integer = (16, 22, 24, 32, 48, 64, 128, 256);
var
  i: Integer;
begin
  for i := 0 to High(StandardSizes) do
    InstallIcon(StandardSizes[i]);

  UpdateIconCache;
end;

procedure TIconManager.UpdateIconCache;  
var
  Process: TProcess;
begin
  Process := TProcess.Create(nil);
  try
    Process.Executable := 'gtk-update-icon-cache';
    Process.Parameters.Add('-f');
    Process.Parameters.Add('-t');
    Process.Parameters.Add(GetEnvironmentVariable('HOME') +
                           '/.local/share/icons/hicolor');
    Process.Options := [poWaitOnExit];
    Process.Execute;
  finally
    Process.Free;
  end;
end;

end.
```

## Autostart Specification

### Lancement automatique des applications

Le standard Autostart permet à votre application de se lancer automatiquement à la connexion de l'utilisateur.

### Fichier autostart

Les fichiers autostart sont des fichiers .desktop placés dans un répertoire spécial :

```pascal
unit AutostartManager;

interface

uses
  Classes, SysUtils;

type
  TAutostartManager = class
  private
    FAppName: string;
    FDesktopEntry: TStringList;
    function GetAutostartPath: string;
  public
    constructor Create(const AppName: string);
    destructor Destroy; override;

    procedure EnableAutostart(const ExecPath, Name, Comment: string);
    procedure DisableAutostart;
    function IsAutostartEnabled: Boolean;
    procedure SetAutostartDelay(Seconds: Integer);
    procedure SetAutostartCondition(const Condition: string);
  end;

implementation

constructor TAutostartManager.Create(const AppName: string);  
begin
  FAppName := AppName;
  FDesktopEntry := TStringList.Create;
end;

destructor TAutostartManager.Destroy;  
begin
  FDesktopEntry.Free;
  inherited;
end;

function TAutostartManager.GetAutostartPath: string;  
begin
  // Le chemin standard pour les applications autostart de l'utilisateur
  Result := GetEnvironmentVariable('HOME') + '/.config/autostart/';
  ForceDirectories(Result);
  Result := Result + FAppName + '.desktop';
end;

procedure TAutostartManager.EnableAutostart(const ExecPath, Name, Comment: string);  
begin
  FDesktopEntry.Clear;
  FDesktopEntry.Add('[Desktop Entry]');
  FDesktopEntry.Add('Version=1.0');
  FDesktopEntry.Add('Type=Application');
  FDesktopEntry.Add('Name=' + Name);
  FDesktopEntry.Add('Comment=' + Comment);
  FDesktopEntry.Add('Exec=' + ExecPath);
  FDesktopEntry.Add('Icon=' + FAppName);
  FDesktopEntry.Add('Terminal=false');
  FDesktopEntry.Add('StartupNotify=false');
  FDesktopEntry.Add('Hidden=false');

  FDesktopEntry.SaveToFile(GetAutostartPath);
end;

procedure TAutostartManager.DisableAutostart;  
var
  FilePath: string;
begin
  FilePath := GetAutostartPath;
  if FileExists(FilePath) then
  begin
    // Méthode 1 : Supprimer le fichier
    DeleteFile(FilePath);

    // Méthode 2 : Désactiver sans supprimer (préférable)
    // En ajoutant Hidden=true au fichier existant
    if FileExists(FilePath) then
    begin
      FDesktopEntry.LoadFromFile(FilePath);
      FDesktopEntry.Values['Hidden'] := 'true';
      FDesktopEntry.SaveToFile(FilePath);
    end;
  end;
end;

function TAutostartManager.IsAutostartEnabled: Boolean;  
var
  FilePath: string;
begin
  FilePath := GetAutostartPath;
  Result := FileExists(FilePath);

  if Result then
  begin
    // Vérifier si le fichier n'est pas désactivé
    FDesktopEntry.LoadFromFile(FilePath);
    Result := FDesktopEntry.Values['Hidden'] <> 'true';
  end;
end;

procedure TAutostartManager.SetAutostartDelay(Seconds: Integer);  
var
  FilePath: string;
begin
  FilePath := GetAutostartPath;
  if FileExists(FilePath) then
  begin
    FDesktopEntry.LoadFromFile(FilePath);
    FDesktopEntry.Values['X-GNOME-Autostart-Delay'] := IntToStr(Seconds);
    FDesktopEntry.SaveToFile(FilePath);
  end;
end;

procedure TAutostartManager.SetAutostartCondition(const Condition: string);  
var
  FilePath: string;
begin
  FilePath := GetAutostartPath;
  if FileExists(FilePath) then
  begin
    FDesktopEntry.LoadFromFile(FilePath);
    // Conditions spécifiques aux environnements
    FDesktopEntry.Values['AutostartCondition'] := Condition;
    // Pour GNOME
    FDesktopEntry.Values['X-GNOME-Autostart-enabled'] := 'true';
    // Pour KDE
    FDesktopEntry.Values['X-KDE-autostart-condition'] := Condition;
    FDesktopEntry.SaveToFile(FilePath);
  end;
end;

end.
```

### Utilisation pratique de l'autostart

```pascal
program ConfigureAutostart;  
uses
  AutostartManager, SysUtils;

var
  Autostart: TAutostartManager;
  AppPath: string;
begin
  AppPath := ParamStr(0);
  Autostart := TAutostartManager.Create('monapplication');
  try
    // Activer le démarrage automatique
    Autostart.EnableAutostart(
      AppPath + ' --minimized',  // Commande avec options
      'Mon Application',          // Nom
      'Lance Mon Application au démarrage'  // Description
    );

    // Définir un délai de 10 secondes avant le lancement
    Autostart.SetAutostartDelay(10);

    // Vérifier si l'autostart est activé
    if Autostart.IsAutostartEnabled then
      WriteLn('Autostart activé')
    else
      WriteLn('Autostart désactivé');

  finally
    Autostart.Free;
  end;
end.
```

## Recent Files Specification

### Gestion des fichiers récents

Le standard Recent Files permet à votre application d'ajouter des fichiers à la liste des documents récents du système.

```pascal
unit RecentFilesManager;

interface

uses
  Classes, SysUtils, DateUtils, DOM, XMLWrite, XMLRead;

type
  TRecentFile = record
    URI: string;
    MimeType: string;
    Timestamp: TDateTime;
    Groups: TStringList;
    IsPrivate: Boolean;
  end;

  TRecentFilesManager = class
  private
    FAppName: string;
    FRecentFiles: TList;
    FMaxItems: Integer;
    function GetRecentFilesPath: string;
    function DateTimeToUnixTime(DateTime: TDateTime): Int64;
    function UnixTimeToDateTime(UnixTime: Int64): TDateTime;
  public
    constructor Create(const AppName: string);
    destructor Destroy; override;

    procedure AddRecentFile(const FileName, MimeType: string);
    procedure RemoveRecentFile(const FileName: string);
    procedure ClearRecentFiles;
    function GetRecentFiles(Count: Integer = 10): TStringList;
    procedure LoadRecentFiles;
    procedure SaveRecentFiles;
  end;

implementation

const
  UnixStartDate: TDateTime = 25569.0; // 01/01/1970

constructor TRecentFilesManager.Create(const AppName: string);  
begin
  FAppName := AppName;
  FRecentFiles := TList.Create;
  FMaxItems := 50; // Limite par défaut
  LoadRecentFiles;
end;

destructor TRecentFilesManager.Destroy;  
begin
  ClearRecentFiles;
  FRecentFiles.Free;
  inherited;
end;

function TRecentFilesManager.GetRecentFilesPath: string;  
begin
  Result := GetEnvironmentVariable('HOME') + '/.local/share/recently-used.xbel';
end;

function TRecentFilesManager.DateTimeToUnixTime(DateTime: TDateTime): Int64;  
begin
  Result := Round((DateTime - UnixStartDate) * 86400);
end;

function TRecentFilesManager.UnixTimeToDateTime(UnixTime: Int64): TDateTime;  
begin
  Result := UnixStartDate + (UnixTime / 86400);
end;

procedure TRecentFilesManager.AddRecentFile(const FileName, MimeType: string);  
var
  RecentFile: ^TRecentFile;
  URI: string;
begin
  // Convertir le chemin en URI
  URI := 'file://' + FileName;

  // Créer une nouvelle entrée
  New(RecentFile);
  RecentFile^.URI := URI;
  RecentFile^.MimeType := MimeType;
  RecentFile^.Timestamp := Now;
  RecentFile^.Groups := TStringList.Create;
  RecentFile^.Groups.Add(FAppName);
  RecentFile^.IsPrivate := False;

  // Ajouter à la liste
  FRecentFiles.Insert(0, RecentFile);

  // Limiter le nombre d'éléments
  while FRecentFiles.Count > FMaxItems do
  begin
    RecentFile := FRecentFiles[FRecentFiles.Count - 1];
    RecentFile^.Groups.Free;
    Dispose(RecentFile);
    FRecentFiles.Delete(FRecentFiles.Count - 1);
  end;

  SaveRecentFiles;
end;

procedure TRecentFilesManager.LoadRecentFiles;  
var
  XMLDoc: TXMLDocument;
  RootNode, BookmarkNode, InfoNode, MetadataNode: TDOMNode;
  ApplicationNode: TDOMNode;
  RecentFile: ^TRecentFile;
  i: Integer;
begin
  if not FileExists(GetRecentFilesPath) then
    Exit;

  try
    ReadXMLFile(XMLDoc, GetRecentFilesPath);

    RootNode := XMLDoc.DocumentElement;
    if RootNode.NodeName <> 'xbel' then
      Exit;

    // Parcourir les bookmarks
    BookmarkNode := RootNode.FirstChild;
    while BookmarkNode <> nil do
    begin
      if BookmarkNode.NodeName = 'bookmark' then
      begin
        New(RecentFile);
        RecentFile^.Groups := TStringList.Create;

        // Lire les attributs
        if BookmarkNode.Attributes.GetNamedItem('href') <> nil then
          RecentFile^.URI := BookmarkNode.Attributes.GetNamedItem('href').NodeValue;

        if BookmarkNode.Attributes.GetNamedItem('modified') <> nil then
          RecentFile^.Timestamp := UnixTimeToDateTime(
            StrToInt64(BookmarkNode.Attributes.GetNamedItem('modified').NodeValue)
          );

        // Lire les métadonnées
        InfoNode := BookmarkNode.FirstChild;
        while InfoNode <> nil do
        begin
          if InfoNode.NodeName = 'info' then
          begin
            MetadataNode := InfoNode.FirstChild;
            while MetadataNode <> nil do
            begin
              if MetadataNode.NodeName = 'metadata' then
              begin
                ApplicationNode := MetadataNode.FirstChild;
                while ApplicationNode <> nil do
                begin
                  if ApplicationNode.NodeName = 'bookmark:application' then
                  begin
                    if ApplicationNode.Attributes.GetNamedItem('name') <> nil then
                      RecentFile^.Groups.Add(
                        ApplicationNode.Attributes.GetNamedItem('name').NodeValue
                      );
                  end;
                  if ApplicationNode.NodeName = 'mime:mime-type' then
                  begin
                    if ApplicationNode.Attributes.GetNamedItem('type') <> nil then
                      RecentFile^.MimeType :=
                        ApplicationNode.Attributes.GetNamedItem('type').NodeValue;
                  end;
                  ApplicationNode := ApplicationNode.NextSibling;
                end;
              end;
              MetadataNode := MetadataNode.NextSibling;
            end;
          end;
          InfoNode := InfoNode.NextSibling;
        end;

        FRecentFiles.Add(RecentFile);
      end;
      BookmarkNode := BookmarkNode.NextSibling;
    end;

    XMLDoc.Free;
  except
    // Gérer les erreurs silencieusement
  end;
end;

procedure TRecentFilesManager.SaveRecentFiles;  
var
  XMLDoc: TXMLDocument;
  RootNode, BookmarkNode, InfoNode, MetadataNode: TDOMElement;
  ApplicationNode, MimeNode: TDOMElement;
  RecentFile: ^TRecentFile;
  i, j: Integer;
begin
  XMLDoc := TXMLDocument.Create;
  try
    // Créer la structure XML
    RootNode := XMLDoc.CreateElement('xbel');
    RootNode.SetAttribute('version', '1.0');
    RootNode.SetAttribute('xmlns:bookmark', 'http://www.freedesktop.org/standards/desktop-bookmarks');
    RootNode.SetAttribute('xmlns:mime', 'http://www.freedesktop.org/standards/shared-mime-info');
    XMLDoc.AppendChild(RootNode);

    // Ajouter chaque fichier récent
    for i := 0 to FRecentFiles.Count - 1 do
    begin
      RecentFile := FRecentFiles[i];

      BookmarkNode := XMLDoc.CreateElement('bookmark');
      BookmarkNode.SetAttribute('href', RecentFile^.URI);
      BookmarkNode.SetAttribute('added', IntToStr(DateTimeToUnixTime(RecentFile^.Timestamp)));
      BookmarkNode.SetAttribute('modified', IntToStr(DateTimeToUnixTime(RecentFile^.Timestamp)));
      BookmarkNode.SetAttribute('visited', IntToStr(DateTimeToUnixTime(RecentFile^.Timestamp)));

      InfoNode := XMLDoc.CreateElement('info');
      MetadataNode := XMLDoc.CreateElement('metadata');
      MetadataNode.SetAttribute('owner', 'http://freedesktop.org');

      // Ajouter le type MIME
      MimeNode := XMLDoc.CreateElement('mime:mime-type');
      MimeNode.SetAttribute('type', RecentFile^.MimeType);
      MetadataNode.AppendChild(MimeNode);

      // Ajouter les applications
      for j := 0 to RecentFile^.Groups.Count - 1 do
      begin
        ApplicationNode := XMLDoc.CreateElement('bookmark:application');
        ApplicationNode.SetAttribute('name', RecentFile^.Groups[j]);
        ApplicationNode.SetAttribute('exec', ''''+ParamStr(0)+' %u''');
        ApplicationNode.SetAttribute('modified', IntToStr(DateTimeToUnixTime(RecentFile^.Timestamp)));
        ApplicationNode.SetAttribute('count', '1');
        MetadataNode.AppendChild(ApplicationNode);
      end;

      InfoNode.AppendChild(MetadataNode);
      BookmarkNode.AppendChild(InfoNode);
      RootNode.AppendChild(BookmarkNode);
    end;

    WriteXMLFile(XMLDoc, GetRecentFilesPath);
  finally
    XMLDoc.Free;
  end;
end;

function TRecentFilesManager.GetRecentFiles(Count: Integer = 10): TStringList;  
var
  i: Integer;
  RecentFile: ^TRecentFile;
  FileName: string;
begin
  Result := TStringList.Create;

  for i := 0 to Min(Count - 1, FRecentFiles.Count - 1) do
  begin
    RecentFile := FRecentFiles[i];
    // Convertir l'URI en chemin de fichier
    FileName := RecentFile^.URI;
    if Pos('file://', FileName) = 1 then
      Delete(FileName, 1, 7);
    Result.Add(FileName);
  end;
end;

procedure TRecentFilesManager.RemoveRecentFile(const FileName: string);  
var
  i: Integer;
  RecentFile: ^TRecentFile;
  URI: string;
begin
  URI := 'file://' + FileName;

  for i := FRecentFiles.Count - 1 downto 0 do
  begin
    RecentFile := FRecentFiles[i];
    if RecentFile^.URI = URI then
    begin
      RecentFile^.Groups.Free;
      Dispose(RecentFile);
      FRecentFiles.Delete(i);
    end;
  end;

  SaveRecentFiles;
end;

procedure TRecentFilesManager.ClearRecentFiles;  
var
  i: Integer;
  RecentFile: ^TRecentFile;
begin
  for i := FRecentFiles.Count - 1 downto 0 do
  begin
    RecentFile := FRecentFiles[i];
    RecentFile^.Groups.Free;
    Dispose(RecentFile);
  end;
  FRecentFiles.Clear;
  SaveRecentFiles;
end;

end.
```

## Thumbnail Managing Standard

### Gestion des miniatures

Le standard de gestion des miniatures permet de créer et gérer des aperçus d'images et de documents.

```pascal
unit ThumbnailManager;

interface

uses
  Classes, SysUtils, Graphics, MD5, FPImage, FPWritePNG;

type
  TThumbnailSize = (tsNormal, tsLarge, tsFailed);

  TThumbnailManager = class
  private
    FAppName: string;
    function GetThumbnailDir(Size: TThumbnailSize): string;
    function GetFileURI(const FileName: string): string;
    function GetFileMD5(const URI: string): string;
    function GetThumbnailPath(const FileName: string; Size: TThumbnailSize): string;
  public
    constructor Create(const AppName: string);

    function GenerateThumbnail(const FileName: string; Size: TThumbnailSize): Boolean;
    function GetThumbnail(const FileName: string; Size: TThumbnailSize): string;
    function ThumbnailExists(const FileName: string; Size: TThumbnailSize): Boolean;
    function ThumbnailNeedsUpdate(const FileName: string; Size: TThumbnailSize): Boolean;
    procedure CleanOldThumbnails(DaysOld: Integer = 30);
  end;

implementation

uses
  FileUtil, DateUtils;

constructor TThumbnailManager.Create(const AppName: string);  
begin
  FAppName := AppName;
end;

function TThumbnailManager.GetThumbnailDir(Size: TThumbnailSize): string;  
var
  BaseDir: string;
begin
  BaseDir := GetEnvironmentVariable('HOME') + '/.cache/thumbnails/';

  case Size of
    tsNormal: Result := BaseDir + 'normal/';
    tsLarge: Result := BaseDir + 'large/';
    tsFailed: Result := BaseDir + 'fail/' + FAppName + '/';
  end;

  ForceDirectories(Result);
end;

function TThumbnailManager.GetFileURI(const FileName: string): string;  
begin
  Result := 'file://' + ExpandFileName(FileName);
end;

function TThumbnailManager.GetFileMD5(const URI: string): string;  
begin
  Result := MD5Print(MD5String(URI));
end;

function TThumbnailManager.GetThumbnailPath(const FileName: string;
  Size: TThumbnailSize): string;
var
  URI, Hash: string;
begin
  URI := GetFileURI(FileName);
  Hash := GetFileMD5(URI);
  Result := GetThumbnailDir(Size) + Hash + '.png';
end;

function TThumbnailManager.GenerateThumbnail(const FileName: string;
  Size: TThumbnailSize): Boolean;
var
  SourceImage, ThumbImage: TPicture;
  ThumbPath: string;
  MaxSize: Integer;
  NewWidth, NewHeight: Integer;
  AspectRatio: Double;
  PNG: TPortableNetworkGraphic;
  FileInfo: TSearchRec;
begin
  Result := False;

  if not FileExists(FileName) then
    Exit;

  // Déterminer la taille maximale
  case Size of
    tsNormal: MaxSize := 128;
    tsLarge: MaxSize := 256;
    else Exit;
  end;

  SourceImage := TPicture.Create;
  ThumbImage := TPicture.Create;
  PNG := TPortableNetworkGraphic.Create;
  try
    try
      // Charger l'image source
      SourceImage.LoadFromFile(FileName);

      // Calculer les nouvelles dimensions
      if (SourceImage.Width > MaxSize) or (SourceImage.Height > MaxSize) then
      begin
        AspectRatio := SourceImage.Width / SourceImage.Height;
        if AspectRatio > 1 then
        begin
          NewWidth := MaxSize;
          NewHeight := Round(MaxSize / AspectRatio);
        end
        else
        begin
          NewHeight := MaxSize;
          NewWidth := Round(MaxSize * AspectRatio);
        end;
      end
      else
      begin
        NewWidth := SourceImage.Width;
        NewHeight := SourceImage.Height;
      end;

      // Créer la miniature
      PNG.Width := NewWidth;
      PNG.Height := NewHeight;
      PNG.Canvas.StretchDraw(Rect(0, 0, NewWidth, NewHeight), SourceImage.Graphic);

      // Ajouter les métadonnées requises
      // Note : En pratique, vous devriez utiliser une bibliothèque PNG
      // pour ajouter les chunks tEXt avec les métadonnées suivantes :
      // - Thumb::URI - L'URI du fichier original
      // - Thumb::MTime - Le timestamp de modification
      // - Thumb::Size - La taille du fichier original
      // - Thumb::Mimetype - Le type MIME

      // Sauvegarder la miniature
      ThumbPath := GetThumbnailPath(FileName, Size);
      PNG.SaveToFile(ThumbPath);

      // Définir les permissions (lecture/écriture pour l'utilisateur seulement)
      FileSetAttr(ThumbPath, $180); // 0600 en octal

      Result := True;
    except
      on E: Exception do
      begin
        // En cas d'échec, créer une entrée dans le répertoire "fail"
        ThumbPath := GetThumbnailPath(FileName, tsFailed);
        // Créer un fichier vide pour marquer l'échec
        FileClose(FileCreate(ThumbPath));
      end;
    end;
  finally
    SourceImage.Free;
    ThumbImage.Free;
    PNG.Free;
  end;
end;

function TThumbnailManager.GetThumbnail(const FileName: string;
  Size: TThumbnailSize): string;
begin
  Result := GetThumbnailPath(FileName, Size);

  if not FileExists(Result) or ThumbnailNeedsUpdate(FileName, Size) then
  begin
    if GenerateThumbnail(FileName, Size) then
      Result := GetThumbnailPath(FileName, Size)
    else
      Result := '';
  end;
end;

function TThumbnailManager.ThumbnailExists(const FileName: string;
  Size: TThumbnailSize): Boolean;
begin
  Result := FileExists(GetThumbnailPath(FileName, Size));
end;

function TThumbnailManager.ThumbnailNeedsUpdate(const FileName: string;
  Size: TThumbnailSize): Boolean;
var
  ThumbPath: string;
  FileAge, ThumbAge: TDateTime;
begin
  Result := True;

  if not FileExists(FileName) then
    Exit;

  ThumbPath := GetThumbnailPath(FileName, Size);
  if not FileExists(ThumbPath) then
    Exit;

  // Comparer les dates de modification
  FileAge := FileDateToDateTime(FileAge(FileName));
  ThumbAge := FileDateToDateTime(FileAge(ThumbPath));

  Result := FileAge > ThumbAge;
end;

procedure TThumbnailManager.CleanOldThumbnails(DaysOld: Integer = 30);  
var
  SearchRec: TSearchRec;
  ThumbDir: string;
  ThumbAge: TDateTime;
  CutoffDate: TDateTime;
begin
  CutoffDate := Now - DaysOld;

  // Nettoyer les miniatures normales
  ThumbDir := GetThumbnailDir(tsNormal);
  if FindFirst(ThumbDir + '*.png', faAnyFile, SearchRec) = 0 then
  begin
    repeat
      ThumbAge := FileDateToDateTime(SearchRec.Time);
      if ThumbAge < CutoffDate then
        DeleteFile(ThumbDir + SearchRec.Name);
    until FindNext(SearchRec) <> 0;
    FindClose(SearchRec);
  end;

  // Nettoyer les grandes miniatures
  ThumbDir := GetThumbnailDir(tsLarge);
  if FindFirst(ThumbDir + '*.png', faAnyFile, SearchRec) = 0 then
  begin
    repeat
      ThumbAge := FileDateToDateTime(SearchRec.Time);
      if ThumbAge < CutoffDate then
        DeleteFile(ThumbDir + SearchRec.Name);
    until FindNext(SearchRec) <> 0;
    FindClose(SearchRec);
  end;
end;

end.
```

## Trash Specification

### Gestion de la corbeille

Le standard Trash permet de déplacer des fichiers vers la corbeille au lieu de les supprimer définitivement.

```pascal
unit TrashManager;

interface

uses
  Classes, SysUtils;

type
  TTrashManager = class
  private
    function GetTrashDir: string;
    function GetTrashInfoDir: string;
    function GetTrashFilesDir: string;
    function GenerateUniqueTrashName(const OriginalName: string): string;
  public
    constructor Create;

    function MoveToTrash(const FileName: string): Boolean;
    function RestoreFromTrash(const TrashName: string): Boolean;
    function EmptyTrash: Boolean;
    function GetTrashedFiles: TStringList;
    function GetOriginalPath(const TrashName: string): string;
  end;

implementation

uses
  FileUtil, DateUtils;

constructor TTrashManager.Create;  
begin
  // Assurer que les répertoires de la corbeille existent
  ForceDirectories(GetTrashFilesDir);
  ForceDirectories(GetTrashInfoDir);
end;

function TTrashManager.GetTrashDir: string;  
begin
  Result := GetEnvironmentVariable('XDG_DATA_HOME');
  if Result = '' then
    Result := GetEnvironmentVariable('HOME') + '/.local/share';
  Result := Result + '/Trash/';
end;

function TTrashManager.GetTrashInfoDir: string;  
begin
  Result := GetTrashDir + 'info/';
end;

function TTrashManager.GetTrashFilesDir: string;  
begin
  Result := GetTrashDir + 'files/';
end;

function TTrashManager.GenerateUniqueTrashName(const OriginalName: string): string;  
var
  BaseName, Extension: string;
  Counter: Integer;
begin
  BaseName := ExtractFileName(OriginalName);
  Extension := ExtractFileExt(BaseName);
  BaseName := Copy(BaseName, 1, Length(BaseName) - Length(Extension));

  Result := BaseName + Extension;
  Counter := 1;

  // Générer un nom unique
  while FileExists(GetTrashFilesDir + Result) do
  begin
    Result := Format('%s.%d%s', [BaseName, Counter, Extension]);
    Inc(Counter);
  end;
end;

function TTrashManager.MoveToTrash(const FileName: string): Boolean;  
var
  TrashName: string;
  InfoFile: TStringList;
  InfoFileName: string;
begin
  Result := False;

  if not FileExists(FileName) and not DirectoryExists(FileName) then
    Exit;

  TrashName := GenerateUniqueTrashName(FileName);

  // Créer le fichier .trashinfo
  InfoFile := TStringList.Create;
  try
    InfoFile.Add('[Trash Info]');
    InfoFile.Add('Path=' + ExpandFileName(FileName));
    InfoFile.Add('DeletionDate=' + FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', Now));

    InfoFileName := GetTrashInfoDir + TrashName + '.trashinfo';
    InfoFile.SaveToFile(InfoFileName);
  finally
    InfoFile.Free;
  end;

  // Déplacer le fichier vers la corbeille
  try
    if DirectoryExists(FileName) then
      Result := RenameFile(FileName, GetTrashFilesDir + TrashName)
    else
      Result := RenameFile(FileName, GetTrashFilesDir + TrashName);
  except
    // Si le déplacement échoue, essayer de copier puis supprimer
    try
      if DirectoryExists(FileName) then
      begin
        CopyDirTree(FileName, GetTrashFilesDir + TrashName);
        DeleteDirectory(FileName, False);
      end
      else
      begin
        CopyFile(FileName, GetTrashFilesDir + TrashName);
        DeleteFile(FileName);
      end;
      Result := True;
    except
      Result := False;
    end;
  end;

  // Si le déplacement a échoué, supprimer le fichier .trashinfo
  if not Result and FileExists(InfoFileName) then
    DeleteFile(InfoFileName);
end;

function TTrashManager.RestoreFromTrash(const TrashName: string): Boolean;  
var
  InfoFile: TStringList;
  InfoFileName: string;
  OriginalPath: string;
  i: Integer;
begin
  Result := False;

  InfoFileName := GetTrashInfoDir + TrashName + '.trashinfo';
  if not FileExists(InfoFileName) then
    Exit;

  InfoFile := TStringList.Create;
  try
    InfoFile.LoadFromFile(InfoFileName);

    // Trouver le chemin original
    for i := 0 to InfoFile.Count - 1 do
    begin
      if Pos('Path=', InfoFile[i]) = 1 then
      begin
        OriginalPath := Copy(InfoFile[i], 6, MaxInt);
        Break;
      end;
    end;

    if OriginalPath = '' then
      Exit;

    // Restaurer le fichier
    Result := RenameFile(GetTrashFilesDir + TrashName, OriginalPath);

    if Result then
    begin
      // Supprimer le fichier .trashinfo
      DeleteFile(InfoFileName);
    end;
  finally
    InfoFile.Free;
  end;
end;

function TTrashManager.EmptyTrash: Boolean;  
var
  SearchRec: TSearchRec;
begin
  Result := True;

  // Supprimer tous les fichiers dans la corbeille
  if FindFirst(GetTrashFilesDir + '*', faAnyFile, SearchRec) = 0 then
  begin
    repeat
      if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
      begin
        if (SearchRec.Attr and faDirectory) = faDirectory then
          DeleteDirectory(GetTrashFilesDir + SearchRec.Name, True)
        else
          DeleteFile(GetTrashFilesDir + SearchRec.Name);
      end;
    until FindNext(SearchRec) <> 0;
    FindClose(SearchRec);
  end;

  // Supprimer tous les fichiers .trashinfo
  if FindFirst(GetTrashInfoDir + '*.trashinfo', faAnyFile, SearchRec) = 0 then
  begin
    repeat
      DeleteFile(GetTrashInfoDir + SearchRec.Name);
    until FindNext(SearchRec) <> 0;
    FindClose(SearchRec);
  end;
end;

function TTrashManager.GetTrashedFiles: TStringList;  
var
  SearchRec: TSearchRec;
  InfoFile: TStringList;
  InfoFileName: string;
  OriginalPath, DeletionDate: string;
  i: Integer;
begin
  Result := TStringList.Create;
  InfoFile := TStringList.Create;

  try
    if FindFirst(GetTrashInfoDir + '*.trashinfo', faAnyFile, SearchRec) = 0 then
    begin
      repeat
        InfoFileName := GetTrashInfoDir + SearchRec.Name;
        InfoFile.LoadFromFile(InfoFileName);

        OriginalPath := '';
        DeletionDate := '';

        for i := 0 to InfoFile.Count - 1 do
        begin
          if Pos('Path=', InfoFile[i]) = 1 then
            OriginalPath := Copy(InfoFile[i], 6, MaxInt)
          else if Pos('DeletionDate=', InfoFile[i]) = 1 then
            DeletionDate := Copy(InfoFile[i], 14, MaxInt);
        end;

        // Retirer l'extension .trashinfo du nom
        Result.Add(Copy(SearchRec.Name, 1, Length(SearchRec.Name) - 10) +
                   '=' + OriginalPath + '|' + DeletionDate);
      until FindNext(SearchRec) <> 0;
      FindClose(SearchRec);
    end;
  finally
    InfoFile.Free;
  end;
end;

function TTrashManager.GetOriginalPath(const TrashName: string): string;  
var
  InfoFile: TStringList;
  InfoFileName: string;
  i: Integer;
begin
  Result := '';

  InfoFileName := GetTrashInfoDir + TrashName + '.trashinfo';
  if not FileExists(InfoFileName) then
    Exit;

  InfoFile := TStringList.Create;
  try
    InfoFile.LoadFromFile(InfoFileName);

    for i := 0 to InfoFile.Count - 1 do
    begin
      if Pos('Path=', InfoFile[i]) = 1 then
      begin
        Result := Copy(InfoFile[i], 6, MaxInt);
        Break;
      end;
    end;
  finally
    InfoFile.Free;
  end;
end;
```

### Utilisation pratique de la corbeille

```pascal
program TrashExample;  
uses
  TrashManager, SysUtils;

var
  Trash: TTrashManager;
  TrashedFiles: TStringList;
  i: Integer;
begin
  Trash := TTrashManager.Create;
  try
    // Déplacer un fichier vers la corbeille
    if Trash.MoveToTrash('/home/user/document.txt') then
      WriteLn('Fichier déplacé vers la corbeille')
    else
      WriteLn('Erreur lors du déplacement');

    // Lister les fichiers dans la corbeille
    TrashedFiles := Trash.GetTrashedFiles;
    try
      WriteLn('Fichiers dans la corbeille :');
      for i := 0 to TrashedFiles.Count - 1 do
        WriteLn('  - ', TrashedFiles[i]);
    finally
      TrashedFiles.Free;
    end;

    // Restaurer un fichier
    if Trash.RestoreFromTrash('document.txt') then
      WriteLn('Fichier restauré');

    // Vider la corbeille
    if MessageDlg('Vider la corbeille ?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      Trash.EmptyTrash;
  finally
    Trash.Free;
  end;
end.
```

## Desktop Notifications Specification

### Système de notifications

Le standard Desktop Notifications permet d'afficher des notifications système via D-Bus.

```pascal
unit NotificationManager;

interface

uses
  Classes, SysUtils;

type
  TNotificationUrgency = (nuLow, nuNormal, nuCritical);

  TNotificationAction = record
    Key: string;
    Label_: string;
    Callback: TNotifyEvent;
  end;

  TNotificationManager = class
  private
    FAppName: string;
    FLastNotificationID: Cardinal;
    FSupportsActions: Boolean;
    FSupportsBody: Boolean;
    FSupportsIcon: Boolean;
    function GetCapabilities: TStringList;
  public
    constructor Create(const AppName: string);

    function ShowNotification(const Summary, Body: string;
      const Icon: string = ''; TimeoutMS: Integer = -1): Cardinal;
    function ShowNotificationAdvanced(const Summary, Body, Icon: string;
      const Actions: array of TNotificationAction;
      Urgency: TNotificationUrgency = nuNormal;
      TimeoutMS: Integer = -1): Cardinal;
    procedure CloseNotification(NotificationID: Cardinal);
    function IsNotificationServerAvailable: Boolean;

    property SupportsActions: Boolean read FSupportsActions;
    property SupportsBody: Boolean read FSupportsBody;
    property SupportsIcon: Boolean read FSupportsIcon;
  end;

implementation

uses
  Process;

constructor TNotificationManager.Create(const AppName: string);  
var
  Capabilities: TStringList;
begin
  FAppName := AppName;
  FLastNotificationID := 0;

  // Vérifier les capacités du serveur de notifications
  Capabilities := GetCapabilities;
  try
    FSupportsActions := Capabilities.IndexOf('actions') >= 0;
    FSupportsBody := Capabilities.IndexOf('body') >= 0;
    FSupportsIcon := Capabilities.IndexOf('icon-static') >= 0;
  finally
    Capabilities.Free;
  end;
end;

function TNotificationManager.GetCapabilities: TStringList;  
var
  Process: TProcess;
  Output: TStringList;
begin
  Result := TStringList.Create;

  Process := TProcess.Create(nil);
  Output := TStringList.Create;
  try
    // Utiliser gdbus pour interroger les capacités
    Process.Executable := 'gdbus';
    Process.Parameters.Add('call');
    Process.Parameters.Add('--session');
    Process.Parameters.Add('--dest=org.freedesktop.Notifications');
    Process.Parameters.Add('--object-path=/org/freedesktop/Notifications');
    Process.Parameters.Add('--method=org.freedesktop.Notifications.GetCapabilities');
    Process.Options := [poUsePipes, poWaitOnExit];

    try
      Process.Execute;
      Output.LoadFromStream(Process.Output);
      // Parser la sortie pour extraire les capacités
      // Format: (['action', 'body', 'icon-static', ...],)
      Result.CommaText := Output.Text;
    except
      // En cas d'erreur, retourner une liste vide
    end;
  finally
    Process.Free;
    Output.Free;
  end;
end;

function TNotificationManager.ShowNotification(const Summary, Body: string;
  const Icon: string = ''; TimeoutMS: Integer = -1): Cardinal;
begin
  Result := ShowNotificationAdvanced(Summary, Body, Icon, [], nuNormal, TimeoutMS);
end;

function TNotificationManager.ShowNotificationAdvanced(
  const Summary, Body, Icon: string;
  const Actions: array of TNotificationAction;
  Urgency: TNotificationUrgency = nuNormal;
  TimeoutMS: Integer = -1): Cardinal;
var
  Process: TProcess;
  UrgencyStr: string;
  i: Integer;
  ActionsStr: string;
begin
  Inc(FLastNotificationID);
  Result := FLastNotificationID;

  Process := TProcess.Create(nil);
  try
    // Méthode 1 : Utiliser notify-send (plus simple mais moins de contrôle)
    Process.Executable := 'notify-send';

    // Ajouter l'icône si spécifiée
    if Icon <> '' then
    begin
      Process.Parameters.Add('-i');
      Process.Parameters.Add(Icon);
    end;

    // Définir l'urgence
    case Urgency of
      nuLow: UrgencyStr := 'low';
      nuNormal: UrgencyStr := 'normal';
      nuCritical: UrgencyStr := 'critical';
    end;
    Process.Parameters.Add('-u');
    Process.Parameters.Add(UrgencyStr);

    // Définir le timeout
    if TimeoutMS >= 0 then
    begin
      Process.Parameters.Add('-t');
      Process.Parameters.Add(IntToStr(TimeoutMS));
    end;

    // Ajouter le nom de l'application
    Process.Parameters.Add('-a');
    Process.Parameters.Add(FAppName);

    // Ajouter le résumé et le corps
    Process.Parameters.Add(Summary);
    if Body <> '' then
      Process.Parameters.Add(Body);

    Process.Execute;

    // Note : Pour un contrôle plus avancé avec actions et callbacks,
    // il faudrait utiliser D-Bus directement via une bibliothèque D-Bus Pascal
  finally
    Process.Free;
  end;
end;

procedure TNotificationManager.CloseNotification(NotificationID: Cardinal);  
var
  Process: TProcess;
begin
  Process := TProcess.Create(nil);
  try
    Process.Executable := 'gdbus';
    Process.Parameters.Add('call');
    Process.Parameters.Add('--session');
    Process.Parameters.Add('--dest=org.freedesktop.Notifications');
    Process.Parameters.Add('--object-path=/org/freedesktop/Notifications');
    Process.Parameters.Add('--method=org.freedesktop.Notifications.CloseNotification');
    Process.Parameters.Add(IntToStr(NotificationID));
    Process.Execute;
  finally
    Process.Free;
  end;
end;

function TNotificationManager.IsNotificationServerAvailable: Boolean;  
var
  Process: TProcess;
begin
  Result := False;
  Process := TProcess.Create(nil);
  try
    Process.Executable := 'which';
    Process.Parameters.Add('notify-send');
    Process.Options := [poWaitOnExit];
    Process.Execute;
    Result := Process.ExitStatus = 0;
  finally
    Process.Free;
  end;
end;
```

### Exemple d'utilisation des notifications

```pascal
program NotificationExample;  
uses
  NotificationManager, SysUtils;

var
  Notifier: TNotificationManager;
  NotifID: Cardinal;
begin
  Notifier := TNotificationManager.Create('MonApplication');
  try
    // Vérifier la disponibilité
    if not Notifier.IsNotificationServerAvailable then
    begin
      WriteLn('Serveur de notifications non disponible');
      Exit;
    end;

    // Notification simple
    NotifID := Notifier.ShowNotification(
      'Bienvenue',
      'Application démarrée avec succès'
    );

    // Notification avec icône
    Notifier.ShowNotification(
      'Nouveau message',
      'Vous avez reçu un nouveau message',
      'mail-unread'
    );

    // Notification urgente
    Notifier.ShowNotificationAdvanced(
      'Attention',
      'Espace disque faible',
      'dialog-warning',
      [],
      nuCritical,
      0  // Pas de timeout
    );

    // Notification temporaire
    Notifier.ShowNotification(
      'Sauvegarde',
      'Document sauvegardé',
      'document-save',
      3000  // 3 secondes
    );

    WriteLn('Capacités du serveur de notifications :');
    WriteLn('  Actions : ', Notifier.SupportsActions);
    WriteLn('  Corps : ', Notifier.SupportsBody);
    WriteLn('  Icônes : ', Notifier.SupportsIcon);
  finally
    Notifier.Free;
  end;
end.
```

## Clipboard Specification

### Gestion du presse-papiers

Le standard Clipboard définit comment les applications doivent interagir avec le presse-papiers système.

```pascal
unit ClipboardManager;

interface

uses
  Classes, SysUtils, Clipbrd;

type
  TClipboardFormat = (cfText, cfHTML, cfImage, cfFiles, cfCustom);

  TClipboardManager = class
  private
    FAppName: string;
    function GetMimeType(Format: TClipboardFormat): string;
  public
    constructor Create(const AppName: string);

    // Méthodes de base
    procedure SetText(const Text: string);
    function GetText: string;
    procedure SetHTML(const HTML: string);
    function GetHTML: string;
    procedure SetFiles(const Files: TStringList);
    function GetFiles: TStringList;

    // Méthodes avancées
    procedure SetData(const Data: TStream; Format: TClipboardFormat;
      const MimeType: string = '');
    function GetData(Format: TClipboardFormat): TStream;
    function HasFormat(Format: TClipboardFormat): Boolean;
    procedure Clear;

    // Support de plusieurs formats simultanés
    procedure SetMultiFormat(const Text, HTML: string; Files: TStringList = nil);

    // Surveillance du presse-papiers
    procedure StartMonitoring(OnChange: TNotifyEvent);
    procedure StopMonitoring;
  end;

implementation

uses
  LCLIntf, LCLType;

constructor TClipboardManager.Create(const AppName: string);  
begin
  FAppName := AppName;
end;

function TClipboardManager.GetMimeType(Format: TClipboardFormat): string;  
begin
  case Format of
    cfText: Result := 'text/plain';
    cfHTML: Result := 'text/html';
    cfImage: Result := 'image/png';
    cfFiles: Result := 'text/uri-list';
    cfCustom: Result := 'application/octet-stream';
  end;
end;

procedure TClipboardManager.SetText(const Text: string);  
begin
  Clipboard.AsText := Text;
end;

function TClipboardManager.GetText: string;  
begin
  if Clipboard.HasFormat(CF_TEXT) then
    Result := Clipboard.AsText
  else
    Result := '';
end;

procedure TClipboardManager.SetHTML(const HTML: string);  
var
  HTMLFormat: TClipboardFormat;
  Stream: TMemoryStream;
  HTMLData: string;
begin
  // Format HTML pour le presse-papiers
  HTMLData := 'Version:0.9' + #13#10 +
              'StartHTML:00000000' + #13#10 +
              'EndHTML:00000000' + #13#10 +
              'StartFragment:00000000' + #13#10 +
              'EndFragment:00000000' + #13#10 +
              '<!DOCTYPE html>' + #13#10 +
              '<html><body>' + #13#10 +
              '<!--StartFragment-->' +
              HTML +
              '<!--EndFragment-->' + #13#10 +
              '</body></html>';

  // Mettre à jour les offsets
  HTMLData := StringReplace(HTMLData, 'StartHTML:00000000',
    Format('StartHTML:%.8d', [Pos('<!DOCTYPE', HTMLData) - 1]), []);
  HTMLData := StringReplace(HTMLData, 'EndHTML:00000000',
    Format('EndHTML:%.8d', [Length(HTMLData)]), []);
  HTMLData := StringReplace(HTMLData, 'StartFragment:00000000',
    Format('StartFragment:%.8d', [Pos('<!--StartFragment-->', HTMLData) + 19]), []);
  HTMLData := StringReplace(HTMLData, 'EndFragment:00000000',
    Format('EndFragment:%.8d', [Pos('<!--EndFragment-->', HTMLData) - 1]), []);

  Stream := TMemoryStream.Create;
  try
    Stream.WriteBuffer(HTMLData[1], Length(HTMLData));
    Stream.Position := 0;

    // Enregistrer le format HTML si nécessaire
    HTMLFormat := RegisterClipboardFormat('HTML Format');
    Clipboard.AddFormat(HTMLFormat, Stream);
  finally
    Stream.Free;
  end;

  // Ajouter aussi en texte brut
  SetText(HTML);
end;

function TClipboardManager.GetHTML: string;  
var
  HTMLFormat: TClipboardFormat;
  Stream: TMemoryStream;
  HTMLData: string;
  StartPos, EndPos: Integer;
begin
  Result := '';

  HTMLFormat := RegisterClipboardFormat('HTML Format');
  if Clipboard.HasFormat(HTMLFormat) then
  begin
    Stream := TMemoryStream.Create;
    try
      Clipboard.GetFormat(HTMLFormat, Stream);
      Stream.Position := 0;
      SetLength(HTMLData, Stream.Size);
      Stream.ReadBuffer(HTMLData[1], Stream.Size);

      // Extraire le fragment HTML
      StartPos := Pos('<!--StartFragment-->', HTMLData);
      EndPos := Pos('<!--EndFragment-->', HTMLData);
      if (StartPos > 0) and (EndPos > 0) then
      begin
        StartPos := StartPos + 20;
        Result := Copy(HTMLData, StartPos, EndPos - StartPos);
      end;
    finally
      Stream.Free;
    end;
  end;
end;

procedure TClipboardManager.SetFiles(const Files: TStringList);  
var
  URIList: string;
  i: Integer;
begin
  URIList := '';
  for i := 0 to Files.Count - 1 do
  begin
    // Convertir en URI
    URIList := URIList + 'file://' + Files[i] + #10;
  end;

  // Placer dans le presse-papiers
  Clipboard.AsText := URIList;

  // Enregistrer aussi avec le format spécifique
  Clipboard.AddFormat(RegisterClipboardFormat('text/uri-list'), URIList);
end;

function TClipboardManager.GetFiles: TStringList;  
var
  URIList: string;
  Lines: TStringList;
  i: Integer;
begin
  Result := TStringList.Create;

  if Clipboard.HasFormat(RegisterClipboardFormat('text/uri-list')) then
  begin
    URIList := Clipboard.AsText;
    Lines := TStringList.Create;
    try
      Lines.Text := URIList;
      for i := 0 to Lines.Count - 1 do
      begin
        if Pos('file://', Lines[i]) = 1 then
        begin
          // Retirer le préfixe file://
          Result.Add(Copy(Lines[i], 8, MaxInt));
        end;
      end;
    finally
      Lines.Free;
    end;
  end;
end;

procedure TClipboardManager.SetData(const Data: TStream;
  Format: TClipboardFormat; const MimeType: string = '');
var
  ClipFormat: TClipboardFormat;
  UseMimeType: string;
begin
  if MimeType <> '' then
    UseMimeType := MimeType
  else
    UseMimeType := GetMimeType(Format);

  ClipFormat := RegisterClipboardFormat(PChar(UseMimeType));
  Clipboard.AddFormat(ClipFormat, Data);
end;

function TClipboardManager.GetData(Format: TClipboardFormat): TStream;  
var
  ClipFormat: TClipboardFormat;
begin
  Result := TMemoryStream.Create;

  ClipFormat := RegisterClipboardFormat(PChar(GetMimeType(Format)));
  if Clipboard.HasFormat(ClipFormat) then
    Clipboard.GetFormat(ClipFormat, Result);

  Result.Position := 0;
end;

function TClipboardManager.HasFormat(Format: TClipboardFormat): Boolean;  
var
  ClipFormat: TClipboardFormat;
begin
  ClipFormat := RegisterClipboardFormat(PChar(GetMimeType(Format)));
  Result := Clipboard.HasFormat(ClipFormat);
end;

procedure TClipboardManager.Clear;  
begin
  Clipboard.Clear;
end;

procedure TClipboardManager.SetMultiFormat(const Text, HTML: string;
  Files: TStringList = nil);
begin
  Clipboard.Open;
  try
    Clipboard.Clear;

    if Text <> '' then
      SetText(Text);

    if HTML <> '' then
      SetHTML(HTML);

    if (Files <> nil) and (Files.Count > 0) then
      SetFiles(Files);
  finally
    Clipboard.Close;
  end;
end;

procedure TClipboardManager.StartMonitoring(OnChange: TNotifyEvent);  
begin
  // Note : L'implémentation complète nécessiterait un timer ou
  // l'enregistrement d'un viewer de presse-papiers
  // Ceci est une version simplifiée
end;

procedure TClipboardManager.StopMonitoring;  
begin
  // Arrêter la surveillance
end;
```

## Application Bundle Specification

### Création de bundles d'application

Un bundle d'application est un format portable pour distribuer des applications.

```pascal
unit AppBundleManager;

interface

uses
  Classes, SysUtils;

type
  TAppBundleManager = class
  private
    FAppName: string;
    FVersion: string;
    FBundlePath: string;
    procedure CreateDirectoryStructure;
    procedure CopyBinary(const SourcePath: string);
    procedure CopyLibraries;
    procedure CreateLauncher;
  public
    constructor Create(const AppName, Version: string);

    procedure AddFile(const SourcePath, DestPath: string);
    procedure AddLibrary(const LibraryPath: string);
    procedure SetIcon(const IconPath: string);
    procedure SetMetadata(const Key, Value: string);
    procedure Build(const OutputPath: string);
    procedure CreateAppImage;
    procedure CreateFlatpak;
    procedure CreateSnap;
  end;

implementation

uses
  FileUtil, Process;

constructor TAppBundleManager.Create(const AppName, Version: string);  
begin
  FAppName := AppName;
  FVersion := Version;
  FBundlePath := '/tmp/' + FAppName + '-bundle/';
end;

procedure TAppBundleManager.CreateDirectoryStructure;  
begin
  // Structure standard d'un bundle
  ForceDirectories(FBundlePath + 'bin/');
  ForceDirectories(FBundlePath + 'lib/');
  ForceDirectories(FBundlePath + 'share/applications/');
  ForceDirectories(FBundlePath + 'share/icons/');
  ForceDirectories(FBundlePath + 'share/metainfo/');
end;

procedure TAppBundleManager.CreateLauncher;  
var
  Launcher: TStringList;
  LauncherPath: string;
begin
  Launcher := TStringList.Create;
  try
    Launcher.Add('#!/bin/bash');
    Launcher.Add('# Launcher script for ' + FAppName);
    Launcher.Add('');
    Launcher.Add('# Déterminer le répertoire du script');
    Launcher.Add('BUNDLE_DIR="$(dirname "$(readlink -f "$0")")"');
    Launcher.Add('');
    Launcher.Add('# Configurer les chemins des bibliothèques');
    Launcher.Add('export LD_LIBRARY_PATH="${BUNDLE_DIR}/lib:${LD_LIBRARY_PATH}"');
    Launcher.Add('');
    Launcher.Add('# Lancer l''application');
    Launcher.Add('"${BUNDLE_DIR}/bin/' + FAppName + '" "$@"');

    LauncherPath := FBundlePath + FAppName;
    Launcher.SaveToFile(LauncherPath);

    // Rendre le launcher exécutable
    FileSetAttr(LauncherPath, $755);
  finally
    Launcher.Free;
  end;
end;

procedure TAppBundleManager.CreateAppImage;  
var
  Process: TProcess;
  AppDirPath: string;
begin
  AppDirPath := FBundlePath + 'AppDir/';

  // Créer la structure AppImage
  ForceDirectories(AppDirPath);
  ForceDirectories(AppDirPath + 'usr/bin/');
  ForceDirectories(AppDirPath + 'usr/lib/');
  ForceDirectories(AppDirPath + 'usr/share/');

  // Copier les fichiers
  CopyDirTree(FBundlePath + 'bin/', AppDirPath + 'usr/bin/');
  CopyDirTree(FBundlePath + 'lib/', AppDirPath + 'usr/lib/');
  CopyDirTree(FBundlePath + 'share/', AppDirPath + 'usr/share/');

  // Créer AppRun
  CreateLauncher;
  CopyFile(FBundlePath + FAppName, AppDirPath + 'AppRun');

  // Créer le fichier .desktop à la racine
  CopyFile(FBundlePath + 'share/applications/' + FAppName + '.desktop',
           AppDirPath + FAppName + '.desktop');

  // Copier l'icône à la racine
  CopyFile(FBundlePath + 'share/icons/' + FAppName + '.png',
           AppDirPath + FAppName + '.png');

  // Utiliser appimagetool pour créer l'AppImage
  Process := TProcess.Create(nil);
  try
    Process.Executable := 'appimagetool';
    Process.Parameters.Add(AppDirPath);
    Process.Parameters.Add(FBundlePath + FAppName + '-' + FVersion + '.AppImage');
    Process.Options := [poWaitOnExit];
    Process.Execute;
  finally
    Process.Free;
  end;
end;
```

## Conclusion

Les standards Freedesktop.org constituent la base de l'intégration desktop sous Linux. En les respectant dans vos applications FreePascal/Lazarus, vous garantissez :

### Avantages de suivre ces standards

1. **Portabilité** : Votre application fonctionne sur tous les environnements de bureau Linux
2. **Intégration native** : L'application s'intègre naturellement dans le système
3. **Expérience utilisateur cohérente** : Les utilisateurs retrouvent des comportements familiers
4. **Maintenance simplifiée** : Code standardisé et documenté
5. **Compatibilité future** : Les standards évoluent de manière rétrocompatible

### Points clés à retenir

- **XDG Base Directory** : Toujours utiliser les chemins XDG pour stocker vos fichiers
- **Desktop Entry** : Créer un fichier .desktop pour chaque application
- **MIME Types** : Déclarer correctement vos types de fichiers personnalisés
- **Icons** : Fournir des icônes dans plusieurs tailles
- **Notifications** : Utiliser le système de notifications standard
- **Trash** : Implémenter la corbeille plutôt que supprimer directement

### Ressources supplémentaires

Pour approfondir votre connaissance des standards Freedesktop.org :

- **Site officiel** : https://www.freedesktop.org/
- **Spécifications** : https://specifications.freedesktop.org/
- **Documentation XDG** : https://wiki.archlinux.org/title/XDG_Base_Directory
- **Forums Lazarus** : https://forum.lazarus.freepascal.org/

### Exemple d'application complète

```pascal
unit FreedesktopCompliantApp;

interface

uses
  Classes, SysUtils, XDGDirectories, DesktopEntry, MimeTypeManager,
  IconManager, AutostartManager, RecentFilesManager, ThumbnailManager,
  TrashManager, NotificationManager, ClipboardManager, AppBundleManager;

type
  TFreedesktopApp = class
  private
    FAppName: string;
    FAppVersion: string;
    FAppExecutable: string;

    // Gestionnaires des différents standards
    FXDG: TXDGManager;
    FDesktop: TDesktopEntry;
    FMimeType: TMimeTypeManager;
    FIcon: TIconManager;
    FAutostart: TAutostartManager;
    FRecentFiles: TRecentFilesManager;
    FThumbnails: TThumbnailManager;
    FTrash: TTrashManager;
    FNotifications: TNotificationManager;
    FClipboard: TClipboardManager;
    FBundle: TAppBundleManager;

    procedure InitializeManagers;
    procedure CleanupManagers;
  public
    constructor Create(const AppName, AppVersion: string);
    destructor Destroy; override;

    // Installation et configuration
    procedure InstallApplication;
    procedure UninstallApplication;
    procedure RegisterFileTypes;
    procedure ConfigureAutostart(Enable: Boolean);

    // Opérations courantes
    procedure SaveConfiguration(const Config: TStringList);
    function LoadConfiguration: TStringList;
    procedure AddToRecentFiles(const FileName: string);
    procedure ShowNotification(const Title, Message: string);
    function MoveFileToTrash(const FileName: string): Boolean;

    // Propriétés
    property AppName: string read FAppName;
    property AppVersion: string read FAppVersion;
  end;

implementation

constructor TFreedesktopApp.Create(const AppName, AppVersion: string);  
begin
  FAppName := AppName;
  FAppVersion := AppVersion;
  FAppExecutable := ParamStr(0);

  InitializeManagers;
end;

destructor TFreedesktopApp.Destroy;  
begin
  CleanupManagers;
  inherited;
end;

procedure TFreedesktopApp.InitializeManagers;  
begin
  FXDG := TXDGManager.Create(FAppName);
  FDesktop := TDesktopEntry.Create;
  FMimeType := TMimeTypeManager.Create('application/x-' + LowerCase(FAppName));
  FIcon := TIconManager.Create(FAppName);
  FAutostart := TAutostartManager.Create(FAppName);
  FRecentFiles := TRecentFilesManager.Create(FAppName);
  FThumbnails := TThumbnailManager.Create(FAppName);
  FTrash := TTrashManager.Create;
  FNotifications := TNotificationManager.Create(FAppName);
  FClipboard := TClipboardManager.Create(FAppName);
  FBundle := TAppBundleManager.Create(FAppName, FAppVersion);
end;

procedure TFreedesktopApp.CleanupManagers;  
begin
  FBundle.Free;
  FClipboard.Free;
  FNotifications.Free;
  FTrash.Free;
  FThumbnails.Free;
  FRecentFiles.Free;
  FAutostart.Free;
  FIcon.Free;
  FMimeType.Free;
  FDesktop.Free;
  FXDG.Free;
end;

procedure TFreedesktopApp.InstallApplication;  
var
  DesktopFilePath: string;
begin
  // 1. Créer et installer le fichier .desktop
  FDesktop.SetBasicInfo(
    FAppName,
    'Application créée avec FreePascal/Lazarus',
    FAppExecutable,
    FAppName
  );
  FDesktop.AddCategory('Utility');
  FDesktop.AddCategory('Application');

  DesktopFilePath := GetEnvironmentVariable('HOME') +
    '/.local/share/applications/' + LowerCase(FAppName) + '.desktop';
  FDesktop.SaveToFile(DesktopFilePath);

  // 2. Installer les icônes
  FIcon.InstallAllIcons;

  // 3. Enregistrer les types MIME personnalisés
  RegisterFileTypes;

  // 4. Créer les répertoires XDG nécessaires
  FXDG.EnsureDirectoriesExist;

  // 5. Afficher une notification de succès
  ShowNotification('Installation réussie',
    FAppName + ' a été installé avec succès');
end;

procedure TFreedesktopApp.UninstallApplication;  
var
  DesktopFilePath: string;
begin
  // 1. Supprimer le fichier .desktop
  DesktopFilePath := GetEnvironmentVariable('HOME') +
    '/.local/share/applications/' + LowerCase(FAppName) + '.desktop';
  if FileExists(DesktopFilePath) then
    DeleteFile(DesktopFilePath);

  // 2. Désactiver l'autostart si activé
  FAutostart.DisableAutostart;

  // 3. Nettoyer les miniatures
  FThumbnails.CleanOldThumbnails(0);

  // 4. Notification
  ShowNotification('Désinstallation',
    FAppName + ' a été désinstallé');
end;

procedure TFreedesktopApp.RegisterFileTypes;  
begin
  // Enregistrer un type de fichier personnalisé
  FMimeType.SetComment('Document ' + FAppName);
  FMimeType.AddExtension(LowerCase(Copy(FAppName, 1, 3)) + 'doc');
  FMimeType.AddMagic(0, UpperCase(Copy(FAppName, 1, 4)));
  FMimeType.Install;

  // Associer le type MIME à l'application
  FDesktop.AddMimeType('application/x-' + LowerCase(FAppName));
end;

procedure TFreedesktopApp.ConfigureAutostart(Enable: Boolean);  
begin
  if Enable then
    FAutostart.EnableAutostart(
      FAppExecutable + ' --minimized',
      FAppName,
      'Démarrer ' + FAppName + ' automatiquement'
    )
  else
    FAutostart.DisableAutostart;
end;

procedure TFreedesktopApp.SaveConfiguration(const Config: TStringList);  
var
  ConfigPath: string;
begin
  ConfigPath := FXDG.GetConfigFile('settings.conf');
  Config.SaveToFile(ConfigPath);
end;

function TFreedesktopApp.LoadConfiguration: TStringList;  
var
  ConfigPath: string;
begin
  Result := TStringList.Create;
  ConfigPath := FXDG.GetConfigFile('settings.conf');
  if FileExists(ConfigPath) then
    Result.LoadFromFile(ConfigPath);
end;

procedure TFreedesktopApp.AddToRecentFiles(const FileName: string);  
begin
  FRecentFiles.AddRecentFile(FileName,
    'application/x-' + LowerCase(FAppName));
end;

procedure TFreedesktopApp.ShowNotification(const Title, Message: string);  
begin
  FNotifications.ShowNotification(Title, Message, FAppName);
end;

function TFreedesktopApp.MoveFileToTrash(const FileName: string): Boolean;  
begin
  Result := FTrash.MoveToTrash(FileName);
  if Result then
    ShowNotification('Fichier supprimé',
      'Le fichier a été déplacé vers la corbeille');
end;
```

## Intégration complète dans une application Lazarus

### Formulaire principal avec tous les standards

```pascal
unit MainForm;

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, StdCtrls, ComCtrls, FreedesktopCompliantApp;

type
  TFormMain = class(TForm)
    MainMenu: TMainMenu;
    MenuFile: TMenuItem;
    MenuEdit: TMenuItem;
    MenuHelp: TMenuItem;
    MenuFileOpen: TMenuItem;
    MenuFileSave: TMenuItem;
    MenuFileRecent: TMenuItem;
    MenuFileExit: TMenuItem;
    MenuEditCopy: TMenuItem;
    MenuEditPaste: TMenuItem;
    MenuHelpAbout: TMenuItem;
    TrayIcon: TTrayIcon;
    StatusBar: TStatusBar;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure MenuFileOpenClick(Sender: TObject);
    procedure MenuFileSaveClick(Sender: TObject);
    procedure MenuFileExitClick(Sender: TObject);
    procedure MenuEditCopyClick(Sender: TObject);
    procedure MenuEditPasteClick(Sender: TObject);
    procedure TrayIconClick(Sender: TObject);
  private
    FApp: TFreedesktopApp;
    FCurrentFile: string;
    FSettings: TStringList;

    procedure LoadSettings;
    procedure SaveSettings;
    procedure UpdateRecentFilesMenu;
    procedure OpenRecentFile(Sender: TObject);
    procedure SetupFileDialogs;
    procedure HandleDroppedFiles(const Files: array of string);
  public
    procedure ProcessCommandLine;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

procedure TFormMain.FormCreate(Sender: TObject);  
begin
  // Initialiser l'application conforme Freedesktop
  FApp := TFreedesktopApp.Create('MonApplication', '1.0.0');
  FSettings := TStringList.Create;

  // Charger les paramètres
  LoadSettings;

  // Configurer l'interface
  SetupFileDialogs;
  UpdateRecentFilesMenu;

  // Configurer l'icône de zone de notification
  TrayIcon.Visible := True;
  TrayIcon.Hint := FApp.AppName;

  // Traiter la ligne de commande
  ProcessCommandLine;

  // Notification de démarrage
  FApp.ShowNotification('Bienvenue',
    FApp.AppName + ' est prêt à l'emploi');
end;

procedure TFormMain.FormDestroy(Sender: TObject);  
begin
  SaveSettings;
  FSettings.Free;
  FApp.Free;
end;

procedure TFormMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);  
var
  Response: Integer;
begin
  // Vérifier les modifications non sauvegardées
  if FCurrentFile <> '' then
  begin
    Response := MessageDlg('Sauvegarder les modifications ?',
      'Des modifications non sauvegardées existent. Voulez-vous les sauvegarder ?',
      mtConfirmation, [mbYes, mbNo, mbCancel], 0);

    case Response of
      mrYes:
        begin
          MenuFileSaveClick(nil);
          CanClose := True;
        end;
      mrNo:
        CanClose := True;
      mrCancel:
        CanClose := False;
    end;
  end
  else
    CanClose := True;
end;

procedure TFormMain.LoadSettings;  
begin
  FSettings := FApp.LoadConfiguration;

  // Appliquer les paramètres
  if FSettings.Values['WindowState'] = 'Maximized' then
    WindowState := wsMaximized
  else
  begin
    Width := StrToIntDef(FSettings.Values['WindowWidth'], 800);
    Height := StrToIntDef(FSettings.Values['WindowHeight'], 600);
    Left := StrToIntDef(FSettings.Values['WindowLeft'], 100);
    Top := StrToIntDef(FSettings.Values['WindowTop'], 100);
  end;

  // Autostart
  if FSettings.Values['Autostart'] = 'True' then
    FApp.ConfigureAutostart(True);
end;

procedure TFormMain.SaveSettings;  
begin
  // Sauvegarder l'état de la fenêtre
  if WindowState = wsMaximized then
    FSettings.Values['WindowState'] := 'Maximized'
  else
  begin
    FSettings.Values['WindowState'] := 'Normal';
    FSettings.Values['WindowWidth'] := IntToStr(Width);
    FSettings.Values['WindowHeight'] := IntToStr(Height);
    FSettings.Values['WindowLeft'] := IntToStr(Left);
    FSettings.Values['WindowTop'] := IntToStr(Top);
  end;

  FApp.SaveConfiguration(FSettings);
end;

procedure TFormMain.SetupFileDialogs;  
begin
  // Configurer les dialogues avec le type MIME personnalisé
  OpenDialog.Filter := FApp.AppName + ' Files|*.' +
    LowerCase(Copy(FApp.AppName, 1, 3)) + 'doc|All Files|*.*';
  SaveDialog.Filter := OpenDialog.Filter;
  SaveDialog.DefaultExt := LowerCase(Copy(FApp.AppName, 1, 3)) + 'doc';
end;

procedure TFormMain.MenuFileOpenClick(Sender: TObject);  
begin
  if OpenDialog.Execute then
  begin
    FCurrentFile := OpenDialog.FileName;

    // Charger le fichier
    // ... code de chargement ...

    // Ajouter aux fichiers récents
    FApp.AddToRecentFiles(FCurrentFile);
    UpdateRecentFilesMenu;

    // Mettre à jour la barre d'état
    StatusBar.SimpleText := 'Fichier ouvert : ' + ExtractFileName(FCurrentFile);
  end;
end;

procedure TFormMain.MenuFileSaveClick(Sender: TObject);  
begin
  if FCurrentFile = '' then
  begin
    if SaveDialog.Execute then
      FCurrentFile := SaveDialog.FileName
    else
      Exit;
  end;

  // Sauvegarder le fichier
  // ... code de sauvegarde ...

  // Notification
  FApp.ShowNotification('Sauvegarde',
    'Le document a été sauvegardé avec succès');

  // Ajouter aux fichiers récents
  FApp.AddToRecentFiles(FCurrentFile);
  UpdateRecentFilesMenu;
end;

procedure TFormMain.MenuFileExitClick(Sender: TObject);  
begin
  Close;
end;

procedure TFormMain.MenuEditCopyClick(Sender: TObject);  
var
  ClipMgr: TClipboardManager;
begin
  ClipMgr := TClipboardManager.Create(FApp.AppName);
  try
    // Copier le contenu sélectionné
    ClipMgr.SetText('Contenu à copier');

    // Optionnellement, copier en plusieurs formats
    ClipMgr.SetMultiFormat(
      'Texte brut',
      '<b>Texte HTML</b>',
      nil
    );
  finally
    ClipMgr.Free;
  end;
end;

procedure TFormMain.MenuEditPasteClick(Sender: TObject);  
var
  ClipMgr: TClipboardManager;
  PastedText: string;
begin
  ClipMgr := TClipboardManager.Create(FApp.AppName);
  try
    if ClipMgr.HasFormat(cfText) then
    begin
      PastedText := ClipMgr.GetText;
      // Utiliser le texte collé
      // ...
    end;
  finally
    ClipMgr.Free;
  end;
end;

procedure TFormMain.UpdateRecentFilesMenu;  
var
  RecentMgr: TRecentFilesManager;
  RecentFiles: TStringList;
  MenuItem: TMenuItem;
  i: Integer;
begin
  // Nettoyer le menu actuel
  MenuFileRecent.Clear;

  RecentMgr := TRecentFilesManager.Create(FApp.AppName);
  RecentFiles := RecentMgr.GetRecentFiles(5);
  try
    for i := 0 to RecentFiles.Count - 1 do
    begin
      MenuItem := TMenuItem.Create(MenuFileRecent);
      MenuItem.Caption := ExtractFileName(RecentFiles[i]);
      MenuItem.Tag := i;
      MenuItem.OnClick := @OpenRecentFile;
      MenuFileRecent.Add(MenuItem);
    end;

    MenuFileRecent.Enabled := RecentFiles.Count > 0;
  finally
    RecentFiles.Free;
    RecentMgr.Free;
  end;
end;

procedure TFormMain.OpenRecentFile(Sender: TObject);  
var
  RecentMgr: TRecentFilesManager;
  RecentFiles: TStringList;
  FileName: string;
begin
  RecentMgr := TRecentFilesManager.Create(FApp.AppName);
  RecentFiles := RecentMgr.GetRecentFiles(5);
  try
    if (Sender as TMenuItem).Tag < RecentFiles.Count then
    begin
      FileName := RecentFiles[(Sender as TMenuItem).Tag];
      if FileExists(FileName) then
      begin
        FCurrentFile := FileName;
        // Charger le fichier
        // ...
      end
      else
      begin
        FApp.ShowNotification('Fichier introuvable',
          'Le fichier n''existe plus : ' + ExtractFileName(FileName));
        RecentMgr.RemoveRecentFile(FileName);
        UpdateRecentFilesMenu;
      end;
    end;
  finally
    RecentFiles.Free;
    RecentMgr.Free;
  end;
end;

procedure TFormMain.TrayIconClick(Sender: TObject);  
begin
  // Afficher/masquer la fenêtre principale
  if Visible then
  begin
    Hide;
    FApp.ShowNotification('Application minimisée',
      FApp.AppName + ' continue en arrière-plan');
  end
  else
  begin
    Show;
    BringToFront;
  end;
end;

procedure TFormMain.HandleDroppedFiles(const Files: array of string);  
var
  i: Integer;
begin
  // Gérer les fichiers déposés
  for i := 0 to High(Files) do
  begin
    if FileExists(Files[i]) then
    begin
      FCurrentFile := Files[i];
      // Charger le fichier
      // ...
      FApp.AddToRecentFiles(FCurrentFile);
      Break; // Ne charger que le premier fichier
    end;
  end;
  UpdateRecentFilesMenu;
end;

procedure TFormMain.ProcessCommandLine;  
var
  i: Integer;
begin
  for i := 1 to ParamCount do
  begin
    if FileExists(ParamStr(i)) then
    begin
      FCurrentFile := ParamStr(i);
      // Charger le fichier
      // ...
      FApp.AddToRecentFiles(FCurrentFile);
      UpdateRecentFilesMenu;
      Break;
    end
    else if ParamStr(i) = '--install' then
    begin
      FApp.InstallApplication;
      Application.Terminate;
    end
    else if ParamStr(i) = '--uninstall' then
    begin
      FApp.UninstallApplication;
      Application.Terminate;
    end
    else if ParamStr(i) = '--autostart' then
    begin
      FApp.ConfigureAutostart(True);
    end
    else if ParamStr(i) = '--no-autostart' then
    begin
      FApp.ConfigureAutostart(False);
    end
    else if ParamStr(i) = '--minimized' then
    begin
      Application.Minimize;
    end;
  end;
end;
```

## Script d'installation pour distribution

### Script Bash pour installer l'application

```bash
#!/bin/bash
# install.sh - Script d'installation pour MonApplication

APP_NAME="monapplication"  
APP_VERSION="1.0.0"  
INSTALL_PREFIX="${HOME}/.local"

echo "Installation de $APP_NAME v$APP_VERSION..."

# Créer les répertoires nécessaires
mkdir -p "$INSTALL_PREFIX/bin"  
mkdir -p "$INSTALL_PREFIX/share/applications"  
mkdir -p "$INSTALL_PREFIX/share/icons/hicolor/48x48/apps"  
mkdir -p "$INSTALL_PREFIX/share/icons/hicolor/scalable/apps"  
mkdir -p "$INSTALL_PREFIX/share/metainfo"

# Copier l'exécutable
cp "$APP_NAME" "$INSTALL_PREFIX/bin/"  
chmod +x "$INSTALL_PREFIX/bin/$APP_NAME"

# Copier les icônes
cp "icons/48x48/$APP_NAME.png" "$INSTALL_PREFIX/share/icons/hicolor/48x48/apps/"  
cp "icons/scalable/$APP_NAME.svg" "$INSTALL_PREFIX/share/icons/hicolor/scalable/apps/"

# Créer le fichier .desktop
cat > "$INSTALL_PREFIX/share/applications/$APP_NAME.desktop" << EOF
[Desktop Entry]
Version=1.0  
Type=Application  
Name=Mon Application  
Comment=Application créée avec FreePascal/Lazarus  
Exec=$INSTALL_PREFIX/bin/$APP_NAME %f  
Icon=$APP_NAME  
Terminal=false  
Categories=Utility;Application;  
MimeType=application/x-monapplication;  
EOF

# Installer le type MIME
cat > "$INSTALL_PREFIX/share/mime/packages/$APP_NAME.xml" << EOF
<?xml version="1.0" encoding="UTF-8"?>
<mime-info xmlns="http://www.freedesktop.org/standards/shared-mime-info">
  <mime-type type="application/x-monapplication">
    <comment>Document Mon Application</comment>
    <glob pattern="*.madoc"/>
    <icon name="$APP_NAME"/>
  </mime-type>
</mime-info>
EOF

# Mettre à jour les bases de données
update-desktop-database "$INSTALL_PREFIX/share/applications" 2>/dev/null  
update-mime-database "$INSTALL_PREFIX/share/mime" 2>/dev/null  
gtk-update-icon-cache -f -t "$INSTALL_PREFIX/share/icons/hicolor" 2>/dev/null

# Lancer l'installation depuis l'application elle-même
"$INSTALL_PREFIX/bin/$APP_NAME" --install

echo "Installation terminée !"  
echo "Vous pouvez lancer l'application avec : $APP_NAME"
```

## Bonnes pratiques et recommandations finales

### Checklist de conformité Freedesktop

Avant de distribuer votre application, vérifiez cette liste :

- ✅ **XDG Base Directory** : Utilisation correcte des répertoires XDG
- ✅ **Fichier .desktop** : Présent et valide
- ✅ **Icônes** : Fournies dans plusieurs tailles
- ✅ **Types MIME** : Déclarés pour les formats personnalisés
- ✅ **Notifications** : Utilisation du système standard
- ✅ **Corbeille** : Support de la spécification Trash
- ✅ **Fichiers récents** : Intégration avec le système
- ✅ **Presse-papiers** : Support des formats multiples
- ✅ **Autostart** : Option d'activation/désactivation
- ✅ **Localisation** : Traductions dans les fichiers .desktop

### Tests multi-environnements

Testez votre application sur :
- GNOME (Ubuntu, Fedora)
- KDE Plasma (Kubuntu, openSUSE)
- XFCE (Xubuntu, Manjaro XFCE)
- Cinnamon (Linux Mint)
- MATE (Ubuntu MATE)

### Performance et optimisation

- Chargez les gestionnaires à la demande
- Mettez en cache les chemins XDG
- Utilisez des threads pour les opérations longues
- Minimisez les appels système

### Sécurité

- Validez tous les chemins de fichiers
- Utilisez des permissions restrictives (0600 pour les configs)
- Évitez d'exécuter des commandes shell directement
- Échappez les caractères spéciaux dans les URIs

## Conclusion

En suivant les standards Freedesktop.org, votre application FreePascal/Lazarus s'intégrera parfaitement dans l'écosystème Linux. Ces standards garantissent une expérience utilisateur cohérente et professionnelle, quel que soit l'environnement de bureau utilisé.

Les exemples de code fournis dans ce tutoriel constituent une base solide pour créer des applications desktop Linux modernes et bien intégrées. N'hésitez pas à adapter ces exemples à vos besoins spécifiques tout en respectant l'esprit des standards.

Le respect de ces normes est un investissement qui facilitera la maintenance, améliorera l'adoption de votre application et garantira sa compatibilité avec les futures versions des environnements de bureau Linux.

⏭️ [Paquets DEB et PPA](/07-specificites-linux-ubuntu/07-paquets-deb-ppa.md)
