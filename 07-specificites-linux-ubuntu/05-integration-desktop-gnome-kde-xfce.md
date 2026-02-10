🔝 Retour au [Sommaire](/SOMMAIRE.md)

# Intégration Desktop Linux (GNOME/KDE/XFCE) avec FreePascal/Lazarus

## Introduction

Lorsque vous développez une application avec FreePascal/Lazarus sous Linux, l'intégration avec l'environnement de bureau est essentielle pour offrir une expérience utilisateur native et cohérente. Les trois principaux environnements de bureau Linux - GNOME, KDE et XFCE - ont chacun leurs spécificités, mais partagent des standards communs qui facilitent le développement d'applications bien intégrées.

## Comprendre les environnements de bureau Linux

### Qu'est-ce qu'un environnement de bureau ?

Un environnement de bureau (Desktop Environment ou DE) est une interface graphique complète qui facilite l'utilisation du système d'exploitation. Il comprend :
- Un gestionnaire de fenêtres
- Une barre des tâches ou un panneau
- Un menu d'applications
- Des zones de notification système
- Des thèmes et styles visuels
- Des utilitaires système intégrés

### Les trois principaux environnements

**GNOME** : L'environnement moderne et épuré, utilisé par défaut sur Ubuntu standard, Fedora et d'autres distributions majeures. Il privilégie la simplicité et l'élégance.

**KDE Plasma** : L'environnement le plus personnalisable et riche en fonctionnalités, utilisé par Kubuntu, openSUSE et KDE Neon. Il offre de nombreuses options de configuration.

**XFCE** : L'environnement léger et rapide, parfait pour les machines moins puissantes, utilisé par Xubuntu et de nombreuses distributions légères. Il reste néanmoins complet et fonctionnel.

## Les standards Freedesktop.org

Avant d'aborder les spécificités de chaque environnement, il est important de comprendre les standards communs qui permettent à votre application de fonctionner correctement partout.

### Le fichier .desktop

Le fichier `.desktop` est la clé de l'intégration de votre application dans le menu des applications. C'est un simple fichier texte qui décrit votre application au système.

```ini
[Desktop Entry]
Version=1.0
Type=Application
Name=Mon Application
Name[fr]=Mon Application
Comment=Une application créée avec Lazarus
Comment[fr]=Une application créée avec Lazarus
Exec=/usr/bin/monapplication
Icon=monapplication
Terminal=false
Categories=Office;Utility;
```

Ce fichier doit être placé dans :
- `/usr/share/applications/` pour une installation système
- `~/.local/share/applications/` pour une installation utilisateur

### Structure des répertoires XDG

Le standard XDG Base Directory définit où stocker les différents types de fichiers :

```pascal
uses
  SysUtils;

function GetConfigDir: string;
begin
  // Récupère le dossier de configuration
  Result := GetEnvironmentVariable('XDG_CONFIG_HOME');
  if Result = '' then
    Result := GetEnvironmentVariable('HOME') + '/.config';
  Result := Result + '/monapplication/';
end;

function GetDataDir: string;
begin
  // Récupère le dossier de données
  Result := GetEnvironmentVariable('XDG_DATA_HOME');
  if Result = '' then
    Result := GetEnvironmentVariable('HOME') + '/.local/share';
  Result := Result + '/monapplication/';
end;

function GetCacheDir: string;
begin
  // Récupère le dossier de cache
  Result := GetEnvironmentVariable('XDG_CACHE_HOME');
  if Result = '' then
    Result := GetEnvironmentVariable('HOME') + '/.cache';
  Result := Result + '/monapplication/';
end;
```

## Intégration avec la zone de notification

### Utilisation du System Tray

Lazarus fournit le composant `TTrayIcon` qui fonctionne avec tous les environnements de bureau :

```pascal
uses
  ExtCtrls, Menus;

procedure TMainForm.FormCreate(Sender: TObject);
var
  TrayIcon: TTrayIcon;
  PopupMenu: TPopupMenu;
  MenuItem: TMenuItem;
begin
  // Création de l'icône de zone de notification
  TrayIcon := TTrayIcon.Create(Self);
  TrayIcon.Visible := True;
  TrayIcon.Hint := 'Mon Application';
  TrayIcon.Icon.LoadFromFile('/usr/share/icons/monapplication.png');

  // Création d'un menu contextuel
  PopupMenu := TPopupMenu.Create(Self);

  MenuItem := TMenuItem.Create(PopupMenu);
  MenuItem.Caption := 'Afficher';
  MenuItem.OnClick := @ShowApplicationClick;
  PopupMenu.Items.Add(MenuItem);

  MenuItem := TMenuItem.Create(PopupMenu);
  MenuItem.Caption := 'Quitter';
  MenuItem.OnClick := @QuitApplicationClick;
  PopupMenu.Items.Add(MenuItem);

  TrayIcon.PopUpMenu := PopupMenu;
  TrayIcon.OnClick := @TrayIconClick;
end;
```

### Notifications desktop

Pour afficher des notifications système, vous pouvez utiliser la bibliothèque libnotify via une interface simple :

```pascal
uses
  Process;

procedure ShowNotification(const Title, Message: string);
var
  Process: TProcess;
begin
  Process := TProcess.Create(nil);
  try
    Process.Executable := 'notify-send';
    Process.Parameters.Add(Title);
    Process.Parameters.Add(Message);
    Process.Parameters.Add('-i');
    Process.Parameters.Add('monapplication');
    Process.Execute;
  finally
    Process.Free;
  end;
end;
```

## Gestion des thèmes et apparence

### Détection de l'environnement actuel

```pascal
function GetDesktopEnvironment: string;
begin
  Result := GetEnvironmentVariable('XDG_CURRENT_DESKTOP');
  if Result = '' then
    Result := GetEnvironmentVariable('DESKTOP_SESSION');

  // Normalisation du résultat
  Result := LowerCase(Result);
  if Pos('gnome', Result) > 0 then
    Result := 'gnome'
  else if Pos('kde', Result) > 0 then
    Result := 'kde'
  else if Pos('plasma', Result) > 0 then
    Result := 'kde'
  else if Pos('xfce', Result) > 0 then
    Result := 'xfce';
end;
```

### Adaptation au thème sombre/clair

De plus en plus d'utilisateurs utilisent des thèmes sombres. Votre application peut détecter et s'adapter :

```pascal
function IsDarkTheme: Boolean;
var
  Process: TProcess;
  Output: TStringList;
begin
  Result := False;

  if GetDesktopEnvironment = 'gnome' then
  begin
    Process := TProcess.Create(nil);
    Output := TStringList.Create;
    try
      Process.Executable := 'gsettings';
      Process.Parameters.Add('get');
      Process.Parameters.Add('org.gnome.desktop.interface');
      Process.Parameters.Add('gtk-theme');
      Process.Options := [poUsePipes, poWaitOnExit];
      Process.Execute;
      Output.LoadFromStream(Process.Output);
      Result := Pos('dark', LowerCase(Output.Text)) > 0;
    finally
      Process.Free;
      Output.Free;
    end;
  end;
  // Ajoutez d'autres environnements selon les besoins
end;
```

## Intégration spécifique à GNOME

### GNOME Shell Extensions

Bien que vous ne puissiez pas créer d'extensions GNOME Shell directement avec FreePascal, votre application peut interagir avec l'environnement GNOME :

```pascal
// Ajout aux favoris GNOME
procedure AddToGnomeFavorites;
var
  Process: TProcess;
begin
  Process := TProcess.Create(nil);
  try
    Process.Executable := 'gsettings';
    Process.Parameters.Add('set');
    Process.Parameters.Add('org.gnome.shell');
    Process.Parameters.Add('favorite-apps');
    Process.Parameters.Add('["monapplication.desktop"]');
    Process.Execute;
  finally
    Process.Free;
  end;
end;
```

### Intégration avec GNOME Settings

Votre application peut respecter les paramètres système GNOME :

```pascal
function GetGnomeAccentColor: TColor;
var
  Process: TProcess;
  Output: TStringList;
begin
  Process := TProcess.Create(nil);
  Output := TStringList.Create;
  try
    Process.Executable := 'gsettings';
    Process.Parameters.Add('get');
    Process.Parameters.Add('org.gnome.desktop.interface');
    Process.Parameters.Add('color-scheme');
    Process.Options := [poUsePipes, poWaitOnExit];
    Process.Execute;
    Output.LoadFromStream(Process.Output);
    // Traiter la réponse pour obtenir la couleur
    // Implémenter la logique selon vos besoins
  finally
    Process.Free;
    Output.Free;
  end;
end;
```

## Intégration spécifique à KDE

### KDE Global Shortcuts

KDE permet l'enregistrement de raccourcis globaux via D-Bus :

```pascal
uses
  dbus;

procedure RegisterKDEGlobalShortcut(const Action, Shortcut: string);
var
  Connection: PDBusConnection;
  Message: PDBusMessage;
begin
  // Connexion à D-Bus
  Connection := dbus_bus_get(DBUS_BUS_SESSION, nil);
  if Connection = nil then Exit;

  // Création du message
  Message := dbus_message_new_method_call(
    'org.kde.kglobalaccel',
    '/kglobalaccel',
    'org.kde.kglobalaccel',
    'setShortcut'
  );

  // Ajout des paramètres
  // ... (implémentation détaillée selon vos besoins)

  // Envoi du message
  dbus_connection_send(Connection, Message, nil);
  dbus_message_unref(Message);
  dbus_connection_unref(Connection);
end;
```

### KDE Activities

KDE permet de gérer des "activités" (contextes de travail). Votre application peut s'y intégrer :

```pascal
function GetCurrentKDEActivity: string;
var
  Process: TProcess;
  Output: TStringList;
begin
  Process := TProcess.Create(nil);
  Output := TStringList.Create;
  try
    Process.Executable := 'qdbus';
    Process.Parameters.Add('org.kde.ActivityManager');
    Process.Parameters.Add('/ActivityManager/Activities');
    Process.Parameters.Add('CurrentActivity');
    Process.Options := [poUsePipes, poWaitOnExit];
    Process.Execute;
    Output.LoadFromStream(Process.Output);
    Result := Trim(Output.Text);
  finally
    Process.Free;
    Output.Free;
  end;
end;
```

## Intégration spécifique à XFCE

### Panel Plugins

XFCE permet de créer des plugins de panneau. Bien que complexe, c'est possible avec FreePascal :

```pascal
// Structure de base pour un plugin XFCE
type
  TXfcePanelPlugin = class
  private
    FWidget: PGtkWidget;
  public
    constructor Create;
    procedure Initialize;
    procedure UpdateContent;
  end;
```

### XFCE Settings

XFCE utilise xfconf pour ses paramètres :

```pascal
function GetXfceSettings(const Channel, Property: string): string;
var
  Process: TProcess;
  Output: TStringList;
begin
  Process := TProcess.Create(nil);
  Output := TStringList.Create;
  try
    Process.Executable := 'xfconf-query';
    Process.Parameters.Add('-c');
    Process.Parameters.Add(Channel);
    Process.Parameters.Add('-p');
    Process.Parameters.Add(Property);
    Process.Options := [poUsePipes, poWaitOnExit];
    Process.Execute;
    Output.LoadFromStream(Process.Output);
    Result := Trim(Output.Text);
  finally
    Process.Free;
    Output.Free;
  end;
end;
```

## Gestion des associations de fichiers

### Déclaration MIME Type

Créez un fichier XML pour déclarer votre type de fichier personnalisé :

```xml
<?xml version="1.0" encoding="UTF-8"?>
<mime-info xmlns="http://www.freedesktop.org/standards/shared-mime-info">
  <mime-type type="application/x-monapplication">
    <comment>Fichier Mon Application</comment>
    <glob pattern="*.monapp"/>
    <icon name="monapplication-file"/>
  </mime-type>
</mime-info>
```

### Association dans le fichier .desktop

Ajoutez la ligne MimeType dans votre fichier .desktop :

```ini
MimeType=application/x-monapplication;
```

### Enregistrement de l'association

```pascal
procedure RegisterMimeType;
var
  Process: TProcess;
begin
  Process := TProcess.Create(nil);
  try
    // Installer le type MIME
    Process.Executable := 'xdg-mime';
    Process.Parameters.Add('install');
    Process.Parameters.Add('monapplication-mime.xml');
    Process.Execute;
    Process.WaitOnExit;

    // Définir comme application par défaut
    Process.Parameters.Clear;
    Process.Parameters.Add('default');
    Process.Parameters.Add('monapplication.desktop');
    Process.Parameters.Add('application/x-monapplication');
    Process.Execute;
  finally
    Process.Free;
  end;
end;
```

## Accessibilité

### Support des lecteurs d'écran

L'accessibilité est importante pour tous les environnements de bureau :

```pascal
procedure SetAccessibleDescription(Component: TControl; const Description: string);
begin
  // Utilisation de ATK (Accessibility Toolkit) pour GTK
  if Component.HandleAllocated then
  begin
    // Définir les propriétés d'accessibilité
    Component.Hint := Description;
    // Pour une implémentation complète, utilisez les APIs ATK
  end;
end;
```

## Intégration avec les menus globaux

### Unity/Ubuntu Global Menu

Pour les distributions utilisant des menus globaux (comme Ubuntu avec Unity ou certaines configurations KDE) :

```pascal
procedure EnableGlobalMenu(Form: TForm);
begin
  // Le support des menus globaux est généralement automatique
  // avec les bonnes bibliothèques GTK installées
  // Assurez-vous que votre MenuBar est bien configuré
  if Form.Menu <> nil then
  begin
    // Les menus seront automatiquement exportés via DBusMenu
    Form.Menu.OwnerDraw := False; // Important pour la compatibilité
  end;
end;
```

## Bonnes pratiques générales

### Respect des préférences utilisateur

Votre application doit toujours respecter les choix de l'utilisateur :

```pascal
procedure ApplyUserPreferences;
begin
  // Police système
  Application.DefaultFont.Name := GetSystemFontName;
  Application.DefaultFont.Size := GetSystemFontSize;

  // Thème d'icônes
  LoadIconTheme(GetSystemIconTheme);

  // Comportement des fenêtres
  Application.SingleInstance := GetSystemSingleInstancePref;
end;
```

### Gestion multi-environnement

Créez une classe pour gérer les différents environnements :

```pascal
type
  TDesktopIntegration = class
  private
    FEnvironment: string;
  public
    constructor Create;
    function GetConfigPath: string;
    function GetDataPath: string;
    procedure ShowNotification(const Title, Text: string);
    procedure RegisterFileAssociation(const Extension, MimeType: string);
    function IsDarkMode: Boolean;
  end;

implementation

constructor TDesktopIntegration.Create;
begin
  FEnvironment := GetDesktopEnvironment;
end;

function TDesktopIntegration.GetConfigPath: string;
begin
  Result := GetEnvironmentVariable('XDG_CONFIG_HOME');
  if Result = '' then
    Result := GetEnvironmentVariable('HOME') + '/.config';
  Result := Result + '/monapplication/';
  ForceDirectories(Result);
end;
```

## Packaging et distribution

### Création d'un paquet DEB (Ubuntu/Debian)

Structure minimale pour un paquet debian :

```
monapplication/
├── DEBIAN/
│   ├── control
│   └── postinst
├── usr/
│   ├── bin/
│   │   └── monapplication
│   └── share/
│       ├── applications/
│       │   └── monapplication.desktop
│       └── icons/
│           └── hicolor/
│               └── 48x48/
│                   └── apps/
│                       └── monapplication.png
```

### Support multi-résolution des icônes

Fournissez des icônes dans plusieurs résolutions :
- 16x16, 22x22, 24x24 (petites icônes)
- 32x32, 48x48 (icônes moyennes)
- 64x64, 128x128 (grandes icônes)
- 256x256, 512x512 (très grandes icônes)
- Format SVG pour la scalabilité

## Conclusion

L'intégration desktop sous Linux avec FreePascal/Lazarus nécessite de comprendre les standards communs (Freedesktop.org) tout en gérant les spécificités de chaque environnement. En suivant ces principes et en utilisant les composants appropriés de Lazarus, vous pouvez créer des applications qui s'intègrent naturellement dans GNOME, KDE ou XFCE, offrant une expérience utilisateur native et professionnelle.

Les points clés à retenir :
- Utilisez les standards Freedesktop.org pour une compatibilité maximale
- Respectez les préférences et thèmes de l'utilisateur
- Testez votre application dans les trois environnements principaux
- Fournissez des fichiers .desktop et des icônes appropriées
- Gérez correctement les chemins XDG pour les données et configurations

Votre application FreePascal/Lazarus peut ainsi offrir une expérience aussi intégrée et native que n'importe quelle application écrite spécifiquement pour Linux.

⏭️ [Freedesktop.org standards](/07-specificites-linux-ubuntu/06-freedesktop-org-standards.md)
