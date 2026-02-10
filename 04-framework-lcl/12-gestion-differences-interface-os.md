🔝 Retour au [Sommaire](/SOMMAIRE.md)

# Gestion des différences d'interface OS dans Lazarus/FreePascal

## Introduction : Pourquoi les interfaces diffèrent selon l'OS ?

### Les philosophies d'interface

Chaque système d'exploitation a sa propre philosophie d'interface :
- **Windows** : Fenêtres flottantes, barre des tâches, menu Démarrer
- **macOS** : Barre de menu unifiée en haut, Dock, design minimaliste
- **Linux/Ubuntu** : Variable selon l'environnement (GNOME, KDE, XFCE)

### L'importance de respecter les conventions

Les utilisateurs s'attendent à ce que votre application **se comporte comme les autres** sur leur système. Une application Windows qui ressemble à macOS sur Windows sera déroutante pour les utilisateurs.

### Le défi du développement multi-plateforme

Créer une application qui :
- Fonctionne sur tous les OS
- Respecte les conventions de chaque plateforme
- Partage le même code source
- Reste maintenable

## Différences visuelles fondamentales

### Position des boutons dans les dialogues

**Windows et Linux** : Les boutons sont généralement alignés à droite
```
[                                    ] [OK] [Annuler]
```

**macOS** : L'ordre est inversé
```
[                           ] [Annuler] [OK]
```

### Menus et barres de menu

**Windows** : Chaque fenêtre a sa propre barre de menu
```pascal
// Menu intégré dans la fenêtre
MainMenu1.Parent := Form1;
```

**macOS** : Une seule barre de menu en haut de l'écran
```pascal
// Le menu s'affiche automatiquement en haut sur macOS
```

**Ubuntu/Linux** : Dépend de l'environnement
- Unity/GNOME : Peut avoir un menu global comme macOS
- KDE/XFCE : Comme Windows

### Icônes et barres d'outils

**Windows** : Barres d'outils avec texte et icônes, style plat ou 3D  
**macOS** : Icônes minimalistes, souvent monochromes  
**Linux** : Variable, souvent suit le thème système  

## Détecter le système d'exploitation

### Utiliser les directives de compilation

```pascal
function ObtenirOS: string;
begin
  {$IFDEF WINDOWS}
    Result := 'Windows';
  {$ENDIF}

  {$IFDEF LINUX}
    Result := 'Linux';
  {$ENDIF}

  {$IFDEF DARWIN}
    Result := 'macOS';
  {$ENDIF}

  {$IFDEF FREEBSD}
    Result := 'FreeBSD';
  {$ENDIF}
end;
```

### Détection à l'exécution

```pascal
uses
  {$IFDEF UNIX}
  BaseUnix,
  {$ENDIF}
  SysUtils;

function DetecterEnvironnement: string;
begin
  {$IFDEF WINDOWS}
    // Note : TOSVersion n'existe pas en FPC (Delphi uniquement)
    Result := 'Windows';
  {$ENDIF}

  {$IFDEF LINUX}
    // Détecter l'environnement de bureau
    if GetEnvironmentVariable('XDG_CURRENT_DESKTOP') <> '' then
      Result := 'Linux - ' + GetEnvironmentVariable('XDG_CURRENT_DESKTOP')
    else if GetEnvironmentVariable('DESKTOP_SESSION') <> '' then
      Result := 'Linux - ' + GetEnvironmentVariable('DESKTOP_SESSION')
    else
      Result := 'Linux';
  {$ENDIF}
end;
```

### Détection avancée pour Linux

```pascal
function DetecterEnvironnementLinux: string;
var
  Desktop: string;
begin
  Desktop := GetEnvironmentVariable('XDG_CURRENT_DESKTOP');

  if Pos('GNOME', UpperCase(Desktop)) > 0 then
    Result := 'GNOME'
  else if Pos('KDE', UpperCase(Desktop)) > 0 then
    Result := 'KDE'
  else if Pos('XFCE', UpperCase(Desktop)) > 0 then
    Result := 'XFCE'
  else if Pos('UNITY', UpperCase(Desktop)) > 0 then
    Result := 'Unity'
  else
    Result := 'Autre';
end;
```

## Adapter l'interface selon l'OS

### Ordre des boutons dans les dialogues

```pascal
type
  TButtonOrder = (boWindowsStyle, boMacStyle);

function GetButtonOrder: TButtonOrder;
begin
  {$IFDEF DARWIN}
    Result := boMacStyle;
  {$ELSE}
    Result := boWindowsStyle;
  {$ENDIF}
end;

procedure OrganiserBoutons(BtnOK, BtnAnnuler: TButton; Parent: TWinControl);
var
  Marge: Integer;
begin
  Marge := 8;

  case GetButtonOrder of
    boWindowsStyle:
    begin
      // Windows/Linux : OK à gauche, Annuler à droite
      BtnAnnuler.Parent := Parent;
      BtnAnnuler.Anchors := [akRight, akBottom];
      BtnAnnuler.AnchorSide[akRight].Control := Parent;
      BtnAnnuler.AnchorSide[akRight].Side := asrRight;
      BtnAnnuler.BorderSpacing.Right := Marge;

      BtnOK.Parent := Parent;
      BtnOK.Anchors := [akRight, akBottom];
      BtnOK.AnchorSide[akRight].Control := BtnAnnuler;
      BtnOK.AnchorSide[akRight].Side := asrLeft;
      BtnOK.BorderSpacing.Right := Marge;
    end;

    boMacStyle:
    begin
      // macOS : Annuler à gauche, OK à droite
      BtnOK.Parent := Parent;
      BtnOK.Anchors := [akRight, akBottom];
      BtnOK.AnchorSide[akRight].Control := Parent;
      BtnOK.AnchorSide[akRight].Side := asrRight;
      BtnOK.BorderSpacing.Right := Marge;

      BtnAnnuler.Parent := Parent;
      BtnAnnuler.Anchors := [akRight, akBottom];
      BtnAnnuler.AnchorSide[akRight].Control := BtnOK;
      BtnAnnuler.AnchorSide[akRight].Side := asrLeft;
      BtnAnnuler.BorderSpacing.Right := Marge;
    end;
  end;
end;
```

### Raccourcis clavier spécifiques à l'OS

```pascal
function GetRaccourciCopier: TShortCut;
begin
  {$IFDEF DARWIN}
    // macOS utilise Cmd au lieu de Ctrl
    Result := ShortCut(Ord('C'), [ssMeta]);
  {$ELSE}
    // Windows/Linux utilisent Ctrl
    Result := ShortCut(Ord('C'), [ssCtrl]);
  {$ENDIF}
end;

procedure ConfigurerRaccourcis;
begin
  // Configuration adaptative des raccourcis
  ActionCopier.ShortCut := GetRaccourciCopier;
  ActionColler.ShortCut := GetRaccourciColler;
  ActionCouper.ShortCut := GetRaccourciCouper;

  {$IFDEF DARWIN}
    ActionQuitter.ShortCut := ShortCut(Ord('Q'), [ssMeta]);
    ActionPreferences.ShortCut := ShortCut(Ord(','), [ssMeta]);
  {$ELSE}
    ActionQuitter.ShortCut := ShortCut(VK_F4, [ssAlt]);
    ActionPreferences.ShortCut := 0; // Pas de standard Windows/Linux
  {$ENDIF}
end;
```

## Chemins et emplacements de fichiers

### Dossiers système différents

```pascal
uses
  FileUtil, LazFileUtils;

function GetDossierConfiguration: string;
begin
  {$IFDEF WINDOWS}
    // Windows : %APPDATA%\MonApp
    Result := GetAppConfigDir(False);
  {$ENDIF}

  {$IFDEF UNIX}
    // Linux/macOS : ~/.config/monapp ou ~/Library/Application Support/MonApp
    Result := GetAppConfigDir(False);
  {$ENDIF}
end;

function GetDossierDocuments: string;
begin
  {$IFDEF WINDOWS}
    Result := GetWindowsSpecialDir(CSIDL_PERSONAL); // Mes Documents
  {$ENDIF}

  {$IFDEF UNIX}
    Result := GetEnvironmentVariable('HOME') + DirectorySeparator + 'Documents';
    if not DirectoryExists(Result) then
      Result := GetEnvironmentVariable('HOME');
  {$ENDIF}
end;

function GetDossierTemp: string;
begin
  Result := GetTempDir; // Fonction cross-platform de Lazarus
  // Windows : %TEMP%
  // Linux : /tmp ou /var/tmp
end;
```

### Chemins d'installation

```pascal
function GetCheminInstallation: string;
begin
  {$IFDEF WINDOWS}
    // Windows : Program Files
    Result := ExtractFilePath(Application.ExeName);
  {$ENDIF}

  {$IFDEF LINUX}
    // Linux : /usr/local/bin ou /opt/monapp
    if FileExists('/opt/monapp/monapp') then
      Result := '/opt/monapp/'
    else
      Result := ExtractFilePath(Application.ExeName);
  {$ENDIF}

  {$IFDEF DARWIN}
    // macOS : /Applications/MonApp.app/Contents/MacOS/
    Result := ExtractFilePath(Application.ExeName);
  {$ENDIF}
end;
```

## Boîtes de dialogue natives

### Utiliser les dialogues système

```pascal
procedure OuvrirFichier;
var
  OpenDialog: TOpenDialog;
begin
  OpenDialog := TOpenDialog.Create(nil);
  try
    // Configuration commune
    OpenDialog.Title := 'Ouvrir un fichier';
    OpenDialog.Filter := 'Tous les fichiers (*.*)|*.*|Fichiers texte (*.txt)|*.txt';

    {$IFDEF WINDOWS}
      OpenDialog.DefaultExt := 'txt';
      OpenDialog.Options := OpenDialog.Options + [ofFileMustExist];
    {$ENDIF}

    {$IFDEF LINUX}
      // Utiliser le dialogue GTK natif si disponible
      OpenDialog.Options := OpenDialog.Options + [ofFileMustExist];
    {$ENDIF}

    {$IFDEF DARWIN}
      // macOS a ses propres options
      OpenDialog.Options := OpenDialog.Options + [ofFileMustExist];
    {$ENDIF}

    if OpenDialog.Execute then
    begin
      // Traiter le fichier
    end;
  finally
    OpenDialog.Free;
  end;
end;
```

### Messages et notifications

```pascal
procedure AfficherNotification(const Titre, Message: string);
begin
  {$IFDEF WINDOWS}
    // Windows : utiliser les notifications balloon ou toast
    Application.ShowHint := True;
    TrayIcon1.BalloonTitle := Titre;
    TrayIcon1.BalloonHint := Message;
    TrayIcon1.ShowBalloonHint;
  {$ENDIF}

  {$IFDEF LINUX}
    // Linux : utiliser notify-send si disponible
    if FileExists('/usr/bin/notify-send') then
      ExecuteProcess('/usr/bin/notify-send', [Titre, Message])
    else
      ShowMessage(Titre + #13#10 + Message);
  {$ENDIF}

  {$IFDEF DARWIN}
    // macOS : utiliser osascript pour les notifications
    ExecuteProcess('/usr/bin/osascript',
      ['-e', Format('display notification "%s" with title "%s"', [Message, Titre])]);
  {$ENDIF}
end;
```

## Icônes et images système

### Formats d'icônes par OS

```pascal
procedure ChargerIconeApplication;
var
  CheminIcone: string;
begin
  {$IFDEF WINDOWS}
    // Windows : ICO
    CheminIcone := 'icones/app.ico';
    Application.Icon.LoadFromFile(CheminIcone);
  {$ENDIF}

  {$IFDEF LINUX}
    // Linux : PNG ou SVG
    CheminIcone := 'icones/app.png';
    if FileExists(CheminIcone) then
      Application.Icon.LoadFromFile(CheminIcone);
  {$ENDIF}

  {$IFDEF DARWIN}
    // macOS : ICNS
    CheminIcone := 'icones/app.icns';
    // macOS gère automatiquement l'icône dans le bundle
  {$ENDIF}
end;
```

### Tailles d'icônes recommandées

```pascal
function GetTailleIconeStandard: Integer;
begin
  {$IFDEF WINDOWS}
    Result := 16; // ou 32 pour les grandes icônes
  {$ENDIF}

  {$IFDEF LINUX}
    // Dépend du thème
    if DetecterEnvironnementLinux = 'GNOME' then
      Result := 24
    else
      Result := 22;
  {$ENDIF}

  {$IFDEF DARWIN}
    Result := 16; // Retina : prévoir aussi 32
  {$ENDIF}
end;
```

## Polices système

### Obtenir les polices par défaut

```pascal
function GetPoliceSysteme: string;
begin
  {$IFDEF WINDOWS}
    Result := 'Segoe UI'; // Windows 10/11
    // Fallback pour anciennes versions
    if Screen.Fonts.IndexOf(Result) < 0 then
      Result := 'Tahoma';
  {$ENDIF}

  {$IFDEF LINUX}
    // Note : case...of ne supporte que les types ordinaux en FPC
    if DetecterEnvironnementLinux = 'GNOME' then
      Result := 'Ubuntu'
    else if DetecterEnvironnementLinux = 'KDE' then
      Result := 'Noto Sans'
    else
      Result := 'Liberation Sans';
    // Fallback
    if Screen.Fonts.IndexOf(Result) < 0 then
      Result := 'Sans';
  {$ENDIF}

  {$IFDEF DARWIN}
    Result := 'San Francisco'; // ou 'Helvetica Neue'
    if Screen.Fonts.IndexOf(Result) < 0 then
      Result := 'Lucida Grande';
  {$ENDIF}
end;

procedure AppliquerPoliceSysteme(Control: TControl);
begin
  Control.Font.Name := GetPoliceSysteme;
end;
```

### Tailles de police appropriées

```pascal
function GetTaillePoliceDefaut: Integer;
begin
  {$IFDEF WINDOWS}
    Result := 9; // Points
  {$ENDIF}

  {$IFDEF LINUX}
    Result := 10; // Souvent un peu plus grand sur Linux
  {$ENDIF}

  {$IFDEF DARWIN}
    Result := 13; // macOS utilise des polices plus grandes
  {$ENDIF}
end;
```

## Comportements spécifiques

### Fermeture d'application

```pascal
procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  {$IFDEF DARWIN}
    // Sur macOS, fermer la fenêtre ne quitte pas l'app
    Hide;
    CanClose := False;
    // L'app reste dans le Dock
  {$ELSE}
    // Windows/Linux : fermer = quitter
    CanClose := MessageDlg('Voulez-vous vraiment quitter ?',
      mtConfirmation, [mbYes, mbNo], 0) = mrYes;
  {$ENDIF}
end;
```

### Minimisation et systray

```pascal
procedure GererMinimisation;
begin
  {$IFDEF WINDOWS}
    // Windows : minimiser dans le systray
    TrayIcon1.Visible := True;
    TrayIcon1.Show;
    Hide;
  {$ENDIF}

  {$IFDEF LINUX}
    // Linux : dépend de l'environnement
    if SupportSystemTray then
    begin
      TrayIcon1.Visible := True;
      Hide;
    end
    else
      WindowState := wsMinimized;
  {$ENDIF}

  {$IFDEF DARWIN}
    // macOS : minimiser dans le Dock
    WindowState := wsMinimized;
  {$ENDIF}
end;
```

## Thèmes et apparence

### Respecter le thème système

```pascal
procedure AppliquerThemeSysteme;
begin
  {$IFDEF WINDOWS}
    // Windows : détecter le mode sombre (Windows 10+)
    if EstModeSombreWindows then
    begin
      Form1.Color := clBlack;
      Form1.Font.Color := clWhite;
    end;
  {$ENDIF}

  {$IFDEF LINUX}
    // Linux : GTK applique automatiquement le thème
    // Mais on peut détecter le thème sombre
    if EstThemeSombreGTK then
    begin
      // Adapter les couleurs custom
    end;
  {$ENDIF}
end;

function EstModeSombreWindows: Boolean;
{$IFDEF WINDOWS}
var
  Reg: TRegistry;
{$ENDIF}
begin
  Result := False;
  {$IFDEF WINDOWS}
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKeyReadOnly('Software\Microsoft\Windows\CurrentVersion\Themes\Personalize') then
    begin
      if Reg.ValueExists('AppsUseLightTheme') then
        Result := Reg.ReadInteger('AppsUseLightTheme') = 0;
    end;
  finally
    Reg.Free;
  end;
  {$ENDIF}
end;
```

### Couleurs système

```pascal
procedure UtiliserCouleursSysteme;
begin
  // Utiliser les couleurs système plutôt que des couleurs fixes
  Panel1.Color := clBtnFace;        // Couleur de fond des boutons
  Edit1.Color := clWindow;          // Couleur de fond des fenêtres
  Label1.Font.Color := clWindowText; // Couleur du texte

  // Éviter les couleurs codées en dur
  // Panel1.Color := $00F0F0F0; // Mauvais !
end;
```

## Gestion des fichiers et extensions

### Associations de fichiers

```pascal
function GetIconePourExtension(const Extension: string): TIcon;
begin
  Result := TIcon.Create;

  {$IFDEF WINDOWS}
    // Windows : extraire l'icône du registre
    // Code pour extraire l'icône associée
  {$ENDIF}

  {$IFDEF LINUX}
    // Linux : utiliser les icônes du thème
    if LowerCase(Extension) = '.txt' then
      Result.LoadFromFile('/usr/share/icons/hicolor/48x48/mimetypes/text-plain.png')
    else if LowerCase(Extension) = '.pdf' then
      Result.LoadFromFile('/usr/share/icons/hicolor/48x48/mimetypes/application-pdf.png');
  {$ENDIF}
end;
```

### Ouverture de fichiers avec l'application par défaut

```pascal
uses
  LCLIntf; // Pour OpenDocument

procedure OuvrirAvecAppParDefaut(const Fichier: string);
begin
  // OpenDocument est cross-platform
  OpenDocument(Fichier);

  // Ou avec plus de contrôle :
  {$IFDEF WINDOWS}
    ShellExecute(0, 'open', PChar(Fichier), nil, nil, SW_SHOW);
  {$ENDIF}

  {$IFDEF LINUX}
    ExecuteProcess('/usr/bin/xdg-open', [Fichier]);
  {$ENDIF}

  {$IFDEF DARWIN}
    ExecuteProcess('/usr/bin/open', [Fichier]);
  {$ENDIF}
end;
```

## Menus contextuels

### Position et comportement

```pascal
procedure TForm1.ShowContextMenu(X, Y: Integer);
begin
  {$IFDEF DARWIN}
    // macOS : Ctrl+Click au lieu du clic droit
    // Le menu contextuel s'affiche différemment
  {$ENDIF}

  PopupMenu1.Popup(X, Y);
end;

procedure TForm1.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbRight then
    ShowContextMenu(X, Y)
  {$IFDEF DARWIN}
  else if (Button = mbLeft) and (ssCtrl in Shift) then
    ShowContextMenu(X, Y);
  {$ENDIF}
end;
```

## Classe helper pour gérer les différences

```pascal
type
  TOSHelper = class
  public
    class function IsWindows: Boolean;
    class function IsLinux: Boolean;
    class function IsMacOS: Boolean;
    class function GetPlatformName: string;
    class function GetConfigDir: string;
    class function GetDocumentsDir: string;
    class function GetDesktopDir: string;
    class function GetDefaultFont: string;
    class function GetDefaultFontSize: Integer;
    class function UsesDarkTheme: Boolean;
    class procedure ConfigureForPlatform(AForm: TForm);
    class function GetFileDialogFilter(const Description, Extension: string): string;
  end;

class function TOSHelper.IsWindows: Boolean;
begin
  {$IFDEF WINDOWS}
    Result := True;
  {$ELSE}
    Result := False;
  {$ENDIF}
end;

class function TOSHelper.GetPlatformName: string;
begin
  {$IFDEF WINDOWS}
    Result := 'Windows';
  {$ENDIF}
  {$IFDEF LINUX}
    Result := 'Linux';
  {$ENDIF}
  {$IFDEF DARWIN}
    Result := 'macOS';
  {$ENDIF}
  {$IFDEF FREEBSD}
    Result := 'FreeBSD';
  {$ENDIF}
end;

class procedure TOSHelper.ConfigureForPlatform(AForm: TForm);
begin
  // Configuration automatique selon l'OS
  AForm.Font.Name := GetDefaultFont;
  AForm.Font.Size := GetDefaultFontSize;

  {$IFDEF DARWIN}
    // Configuration spécifique macOS
    AForm.Menu := nil; // Menu global
  {$ENDIF}

  {$IFDEF LINUX}
    // Configuration spécifique Linux
    if DetecterEnvironnementLinux = 'Unity' then
      AForm.Menu := nil; // Unity a un menu global
  {$ENDIF}
end;

class function TOSHelper.GetFileDialogFilter(const Description, Extension: string): string;
begin
  {$IFDEF WINDOWS}
    // Windows utilise | comme séparateur
    Result := Format('%s (*%s)|*%s|Tous les fichiers (*.*)|*.*',
      [Description, Extension, Extension]);
  {$ELSE}
    // Linux/macOS peuvent utiliser un format différent
    Result := Format('%s (*%s)|*%s|All Files (*)|*',
      [Description, Extension, Extension]);
  {$ENDIF}
end;
```

## Exemple complet : Application adaptative

```pascal
unit MainUnit;

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls;

type
  TMainForm = class(TForm)
    MainMenu: TMainMenu;
    FileMenu: TMenuItem;
    EditMenu: TMenuItem;
    BtnOK: TButton;
    BtnCancel: TButton;
    StatusLabel: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    procedure ConfigurePourOS;
    procedure ConfigurerMenus;
    procedure ConfigurerBoutons;
    procedure ConfigurerRaccourcis;
  end;

var
  MainForm: TMainForm;

implementation

procedure TMainForm.FormCreate(Sender: TObject);
begin
  ConfigurePourOS;
end;

procedure TMainForm.ConfigurePourOS;
begin
  // Titre adapté
  Caption := 'MonApp - ' + TOSHelper.GetPlatformName;

  // Configuration spécifique
  ConfigurerMenus;
  ConfigurerBoutons;
  ConfigurerRaccourcis;

  // Police système
  Font.Name := TOSHelper.GetDefaultFont;
  Font.Size := TOSHelper.GetDefaultFontSize;

  // Thème
  if TOSHelper.UsesDarkTheme then
  begin
    Color := clBlack;
    Font.Color := clWhite;
  end;

  // Message de statut
  StatusLabel.Caption := Format('Configuré pour %s', [TOSHelper.GetPlatformName]);
end;

procedure TMainForm.ConfigurerMenus;
{$IFDEF WINDOWS}
var
  HelpItem: TMenuItem;
{$ENDIF}
begin
  {$IFDEF DARWIN}
    // macOS : Certains menus sont automatiques
    // Le menu "Pomme" est géré par l'OS
  {$ENDIF}

  {$IFDEF WINDOWS}
    // Windows : Ajouter le menu Aide à droite
    HelpItem := TMenuItem.Create(MainMenu);
    HelpItem.Caption := '&Aide';
    MainMenu.Items.Add(HelpItem);
  {$ENDIF}
end;

procedure TMainForm.ConfigurerBoutons;
begin
  // Ordre des boutons selon l'OS
  {$IFDEF DARWIN}
    // macOS : Annuler-OK
    BtnCancel.TabOrder := 0;
    BtnOK.TabOrder := 1;
  {$ELSE}
    // Windows/Linux : OK-Annuler
    BtnOK.TabOrder := 0;
    BtnCancel.TabOrder := 1;
  {$ENDIF}
end;

procedure TMainForm.ConfigurerRaccourcis;
begin
  {$IFDEF DARWIN}
    // macOS utilise Cmd (Meta)
    FileMenu.Caption := 'Fichier';
    // Pas de & pour les raccourcis sur macOS
  {$ELSE}
    FileMenu.Caption := '&Fichier';
    // Le & indique le raccourci Alt+F
  {$ENDIF}
end;

end.
```

## Tests multi-plateformes

### Stratégie de test

1. **Machine virtuelle** : Tester sur VM Windows/Linux
2. **Dual boot** : Installation native pour tests réels
3. **CI/CD** : Tests automatisés sur différents OS
4. **Beta testeurs** : Utilisateurs sur différentes plateformes

### Points de vérification

- **Apparence** : L'interface suit-elle les conventions de l'OS ?
- **Navigation** : Les raccourcis fonctionnent-ils correctement ?
- **Fichiers** : Les chemins sont-ils corrects ?
- **Dialogues** : Les boutons sont-ils dans le bon ordre ?
- **Polices** : Le texte est-il lisible avec les polices système ?
- **Thème** : L'application respecte-t-elle le thème système ?

## Bonnes pratiques récapitulatives

### 1. Utiliser l'abstraction
- Créer des fonctions helper pour les différences OS
- Éviter le code spécifique OS dans la logique métier
- Centraliser les adaptations dans des unités dédiées

### 2. Respecter les conventions
- Suivre les guidelines de chaque plateforme
- Utiliser les composants natifs quand possible
- Ne pas forcer un style d'un OS sur un autre

### 3. Tester régulièrement
- Tester sur chaque OS cible
- Vérifier avec différentes versions d'OS
- Tester avec différents thèmes/configurations

### 4. Documentation
- Documenter les différences de comportement
- Créer des guides utilisateur par plateforme
- Maintenir une liste des limitations par OS

## Conclusion

Gérer les différences d'interface entre systèmes d'exploitation est essentiel pour créer une application véritablement multi-plateforme. Les points clés :

1. **Détecter l'OS** et adapter le comportement en conséquence
2. **Respecter les conventions** de chaque plateforme
3. **Utiliser les APIs natives** via des abstractions
4. **Tester sur chaque plateforme** régulièrement
5. **Centraliser les adaptations** dans des classes helper

Avec ces techniques, votre application Lazarus sera naturelle et agréable à utiliser, que ce soit sur Windows, Linux ou macOS. Les utilisateurs apprécieront une application qui "parle leur langue" visuelle et comportementale.

⏭️ [Développement Multi-plateforme Approfondi](/05-developpement-multiplateforme-approfondi/README.md)
