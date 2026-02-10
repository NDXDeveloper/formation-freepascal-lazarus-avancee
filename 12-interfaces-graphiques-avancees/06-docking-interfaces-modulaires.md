🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 12.6 Docking et interfaces modulaires

## Introduction

Le **docking** (ancrage) est une fonctionnalité qui permet aux utilisateurs de réorganiser l'interface d'une application en déplaçant des panneaux, fenêtres ou composants. Vous avez probablement déjà utilisé ce type d'interface dans des logiciels comme Visual Studio, Photoshop ou l'IDE Lazarus lui-même !

Une **interface modulaire** permet de diviser votre application en sections indépendantes que l'utilisateur peut personnaliser selon ses besoins. Cela améliore grandement l'ergonomie et l'expérience utilisateur.

## Pourquoi utiliser le docking ?

### Avantages pour l'utilisateur
- **Personnalisation** : chacun peut organiser son espace de travail comme il le souhaite
- **Flexibilité** : adaptation à différentes tailles d'écran et résolutions
- **Productivité** : accès rapide aux outils fréquemment utilisés
- **Confort** : disposition adaptée au workflow personnel

### Avantages pour le développeur
- **Interface professionnelle** : aspect moderne et soigné
- **Modularité du code** : séparation claire des fonctionnalités
- **Évolutivité** : ajout facile de nouveaux modules
- **Sauvegarde** : possibilité de mémoriser la disposition

## Concepts de base

### Qu'est-ce qu'un site de docking ?

Un **site de docking** (docking site) est une zone où un composant peut être ancré. Imaginez-le comme un "emplacement d'accueil" pour vos panneaux.

### Types d'ancrage

1. **Docking latéral** : ancrage sur les côtés (gauche, droite, haut, bas)
2. **Docking flottant** : fenêtre indépendante qui peut se déplacer librement
3. **Docking par onglets** : plusieurs panneaux superposés avec des onglets
4. **Docking imbriqué** : panneaux à l'intérieur d'autres panneaux

### États d'un panneau dockable

- **Ancré** (Docked) : fixé dans un site de docking
- **Flottant** (Floating) : fenêtre indépendante
- **Masqué** (Hidden) : non visible mais toujours en mémoire
- **Auto-masquage** (Auto-hide) : apparaît au survol, se cache automatiquement

## Implémentation dans Lazarus

### Méthode 1 : Utiliser AnchorDocking (recommandé pour débuter)

Lazarus inclut le package **AnchorDocking** qui facilite grandement la création d'interfaces avec docking.

#### Installation du package

1. Dans Lazarus, allez dans **Paquets** → **Installer/Désinstaller des paquets**
2. Cherchez **AnchorDockingDsgn** dans la liste de gauche
3. Cliquez sur **Ajouter** pour le déplacer à droite
4. Cliquez sur **Enregistrer et reconstruire l'IDE**
5. Lazarus redémarrera avec le package installé

#### Création d'une interface dockable simple

```pascal
unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  AnchorDocking, AnchorDockStorage;

type
  TfrmMain = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    FDockMaster: TAnchorDockMaster;
    FLeftPanel: TForm;
    FRightPanel: TForm;
    FBottomPanel: TForm;
  public
    procedure CreateDockablePanels;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

procedure TfrmMain.FormCreate(Sender: TObject);  
begin
  // Création du gestionnaire de docking
  FDockMaster := TAnchorDockMaster.Create(Self);
  FDockMaster.MakeDockable(Self);

  // Création des panneaux dockables
  CreateDockablePanels;
end;

procedure TfrmMain.CreateDockablePanels;  
var
  Memo: TMemo;
  ListBox: TListBox;
  Panel: TPanel;
begin
  // Panneau gauche : explorateur de fichiers
  FLeftPanel := TForm.Create(Self);
  FLeftPanel.Caption := 'Explorateur';
  FLeftPanel.Width := 200;

  ListBox := TListBox.Create(FLeftPanel);
  ListBox.Parent := FLeftPanel;
  ListBox.Align := alClient;
  ListBox.Items.Add('Document1.txt');
  ListBox.Items.Add('Document2.txt');
  ListBox.Items.Add('Image.png');

  FDockMaster.MakeDockable(FLeftPanel);
  FLeftPanel.Show;

  // Panneau droit : propriétés
  FRightPanel := TForm.Create(Self);
  FRightPanel.Caption := 'Propriétés';
  FRightPanel.Width := 250;

  Memo := TMemo.Create(FRightPanel);
  Memo.Parent := FRightPanel;
  Memo.Align := alClient;
  Memo.Lines.Add('Nom: Document1.txt');
  Memo.Lines.Add('Taille: 1.2 Ko');
  Memo.Lines.Add('Modifié: 03/10/2025');

  FDockMaster.MakeDockable(FRightPanel);
  FRightPanel.Show;

  // Panneau bas : sortie/console
  FBottomPanel := TForm.Create(Self);
  FBottomPanel.Caption := 'Console de sortie';
  FBottomPanel.Height := 150;

  Memo := TMemo.Create(FBottomPanel);
  Memo.Parent := FBottomPanel;
  Memo.Align := alClient;
  Memo.Lines.Add('Application démarrée...');
  Memo.Lines.Add('Prêt.');

  FDockMaster.MakeDockable(FBottomPanel);
  FBottomPanel.Show;
end;

end.
```

### Méthode 2 : Docking manuel avec TPanel

Pour un contrôle plus précis, vous pouvez implémenter le docking manuellement.

```pascal
unit SimpleDocking;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type
  TfrmSimpleDock = class(TForm)
    pnlMain: TPanel;
    splLeft: TSplitter;
    splRight: TSplitter;
    splBottom: TSplitter;
    pnlLeft: TPanel;
    pnlRight: TPanel;
    pnlBottom: TPanel;
    pnlCenter: TPanel;
    procedure FormCreate(Sender: TObject);
  private
    procedure SetupPanels;
  end;

var
  frmSimpleDock: TfrmSimpleDock;

implementation

{$R *.lfm}

procedure TfrmSimpleDock.FormCreate(Sender: TObject);  
begin
  SetupPanels;
end;

procedure TfrmSimpleDock.SetupPanels;  
var
  Memo: TMemo;
  ListBox: TListBox;
  Label1: TLabel;
begin
  // Configuration du panneau principal
  pnlMain.Align := alClient;

  // Panneau gauche
  pnlLeft.Align := alLeft;
  pnlLeft.Width := 200;
  pnlLeft.Caption := '';

  Label1 := TLabel.Create(Self);
  Label1.Parent := pnlLeft;
  Label1.Align := alTop;
  Label1.Caption := ' Fichiers';
  Label1.Font.Style := [fsBold];

  ListBox := TListBox.Create(Self);
  ListBox.Parent := pnlLeft;
  ListBox.Align := alClient;

  // Séparateur gauche
  splLeft.Align := alLeft;
  splLeft.Width := 5;

  // Panneau droit
  pnlRight.Align := alRight;
  pnlRight.Width := 250;
  pnlRight.Caption := '';

  Label1 := TLabel.Create(Self);
  Label1.Parent := pnlRight;
  Label1.Align := alTop;
  Label1.Caption := ' Propriétés';
  Label1.Font.Style := [fsBold];

  Memo := TMemo.Create(Self);
  Memo.Parent := pnlRight;
  Memo.Align := alClient;
  Memo.ScrollBars := ssVertical;

  // Séparateur droit
  splRight.Align := alRight;
  splRight.Width := 5;

  // Panneau bas
  pnlBottom.Align := alBottom;
  pnlBottom.Height := 150;
  pnlBottom.Caption := '';

  Label1 := TLabel.Create(Self);
  Label1.Parent := pnlBottom;
  Label1.Align := alTop;
  Label1.Caption := ' Console';
  Label1.Font.Style := [fsBold];

  Memo := TMemo.Create(Self);
  Memo.Parent := pnlBottom;
  Memo.Align := alClient;
  Memo.ScrollBars := ssBoth;

  // Séparateur bas
  splBottom.Align := alBottom;
  splBottom.Height := 5;

  // Zone centrale
  pnlCenter.Align := alClient;
  pnlCenter.Caption := 'Zone de travail principale';
end;

end.
```

## Fonctionnalités avancées

### Sauvegarde et restauration de la disposition

Il est important de permettre aux utilisateurs de sauvegarder leur disposition personnalisée.

```pascal
uses
  IniFiles;

procedure TfrmMain.SaveLayout(const AFileName: string);  
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(AFileName);
  try
    // Sauvegarde des tailles de panneaux
    Ini.WriteInteger('Layout', 'LeftPanelWidth', pnlLeft.Width);
    Ini.WriteInteger('Layout', 'RightPanelWidth', pnlRight.Width);
    Ini.WriteInteger('Layout', 'BottomPanelHeight', pnlBottom.Height);

    // Sauvegarde de la visibilité
    Ini.WriteBool('Layout', 'LeftPanelVisible', pnlLeft.Visible);
    Ini.WriteBool('Layout', 'RightPanelVisible', pnlRight.Visible);
    Ini.WriteBool('Layout', 'BottomPanelVisible', pnlBottom.Visible);

    // Sauvegarde de la position de la fenêtre
    Ini.WriteInteger('Window', 'Left', Self.Left);
    Ini.WriteInteger('Window', 'Top', Self.Top);
    Ini.WriteInteger('Window', 'Width', Self.Width);
    Ini.WriteInteger('Window', 'Height', Self.Height);
    Ini.WriteInteger('Window', 'State', Ord(Self.WindowState));
  finally
    Ini.Free;
  end;
end;

procedure TfrmMain.LoadLayout(const AFileName: string);  
var
  Ini: TIniFile;
begin
  if not FileExists(AFileName) then Exit;

  Ini := TIniFile.Create(AFileName);
  try
    // Restauration des tailles
    pnlLeft.Width := Ini.ReadInteger('Layout', 'LeftPanelWidth', 200);
    pnlRight.Width := Ini.ReadInteger('Layout', 'RightPanelWidth', 250);
    pnlBottom.Height := Ini.ReadInteger('Layout', 'BottomPanelHeight', 150);

    // Restauration de la visibilité
    pnlLeft.Visible := Ini.ReadBool('Layout', 'LeftPanelVisible', True);
    pnlRight.Visible := Ini.ReadBool('Layout', 'RightPanelVisible', True);
    pnlBottom.Visible := Ini.ReadBool('Layout', 'BottomPanelVisible', True);

    // Restauration de la position
    Self.Left := Ini.ReadInteger('Window', 'Left', 100);
    Self.Top := Ini.ReadInteger('Window', 'Top', 100);
    Self.Width := Ini.ReadInteger('Window', 'Width', 1024);
    Self.Height := Ini.ReadInteger('Window', 'Height', 768);
    Self.WindowState := TWindowState(Ini.ReadInteger('Window', 'State', Ord(wsNormal)));
  finally
    Ini.Free;
  end;
end;

// Utilisation
procedure TfrmMain.FormCreate(Sender: TObject);  
begin
  LoadLayout(GetAppConfigFile(False));
end;

procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);  
begin
  SaveLayout(GetAppConfigFile(False));
end;
```

### Menu de gestion des panneaux

Ajoutez un menu pour permettre aux utilisateurs de montrer/cacher les panneaux.

```pascal
procedure TfrmMain.CreateViewMenu;  
var
  MenuItem: TMenuItem;
begin
  // Menu Vue
  mnuView := TMenuItem.Create(Self);
  mnuView.Caption := 'Vue';
  MainMenu1.Items.Add(mnuView);

  // Panneau gauche
  MenuItem := TMenuItem.Create(Self);
  MenuItem.Caption := 'Explorateur';
  MenuItem.Checked := pnlLeft.Visible;
  MenuItem.OnClick := @ToggleLeftPanel;
  mnuView.Add(MenuItem);

  // Panneau droit
  MenuItem := TMenuItem.Create(Self);
  MenuItem.Caption := 'Propriétés';
  MenuItem.Checked := pnlRight.Visible;
  MenuItem.OnClick := @ToggleRightPanel;
  mnuView.Add(MenuItem);

  // Panneau bas
  MenuItem := TMenuItem.Create(Self);
  MenuItem.Caption := 'Console';
  MenuItem.Checked := pnlBottom.Visible;
  MenuItem.OnClick := @ToggleBottomPanel;
  mnuView.Add(MenuItem);

  // Séparateur
  mnuView.Add(TMenuItem.Create(Self));

  // Réinitialiser la disposition
  MenuItem := TMenuItem.Create(Self);
  MenuItem.Caption := 'Réinitialiser la disposition';
  MenuItem.OnClick := @ResetLayout;
  mnuView.Add(MenuItem);
end;

procedure TfrmMain.ToggleLeftPanel(Sender: TObject);  
begin
  pnlLeft.Visible := not pnlLeft.Visible;
  splLeft.Visible := pnlLeft.Visible;
  (Sender as TMenuItem).Checked := pnlLeft.Visible;
end;

procedure TfrmMain.ToggleRightPanel(Sender: TObject);  
begin
  pnlRight.Visible := not pnlRight.Visible;
  splRight.Visible := pnlRight.Visible;
  (Sender as TMenuItem).Checked := pnlRight.Visible;
end;

procedure TfrmMain.ToggleBottomPanel(Sender: TObject);  
begin
  pnlBottom.Visible := not pnlBottom.Visible;
  splBottom.Visible := pnlBottom.Visible;
  (Sender as TMenuItem).Checked := pnlBottom.Visible;
end;

procedure TfrmMain.ResetLayout(Sender: TObject);  
begin
  // Réinitialiser aux valeurs par défaut
  pnlLeft.Width := 200;
  pnlRight.Width := 250;
  pnlBottom.Height := 150;

  pnlLeft.Visible := True;
  pnlRight.Visible := True;
  pnlBottom.Visible := True;

  splLeft.Visible := True;
  splRight.Visible := True;
  splBottom.Visible := True;
end;
```

### Panneaux avec onglets

Pour créer des panneaux avec plusieurs onglets (comme dans l'IDE Lazarus) :

```pascal
uses
  ComCtrls; // Pour TPageControl

procedure TfrmMain.CreateTabbedPanel;  
var
  PageControl: TPageControl;
  TabSheet: TTabSheet;
  Memo: TMemo;
begin
  // Création du contrôle à onglets
  PageControl := TPageControl.Create(Self);
  PageControl.Parent := pnlBottom;
  PageControl.Align := alClient;

  // Onglet 1 : Messages
  TabSheet := TTabSheet.Create(PageControl);
  TabSheet.PageControl := PageControl;
  TabSheet.Caption := 'Messages';

  Memo := TMemo.Create(TabSheet);
  Memo.Parent := TabSheet;
  Memo.Align := alClient;
  Memo.ReadOnly := True;
  Memo.Lines.Add('Application démarrée avec succès.');

  // Onglet 2 : Erreurs
  TabSheet := TTabSheet.Create(PageControl);
  TabSheet.PageControl := PageControl;
  TabSheet.Caption := 'Erreurs';

  Memo := TMemo.Create(TabSheet);
  Memo.Parent := TabSheet;
  Memo.Align := alClient;
  Memo.ReadOnly := True;
  Memo.Font.Color := clRed;

  // Onglet 3 : Historique
  TabSheet := TTabSheet.Create(PageControl);
  TabSheet.PageControl := PageControl;
  TabSheet.Caption := 'Historique';

  Memo := TMemo.Create(TabSheet);
  Memo.Parent := TabSheet;
  Memo.Align := alClient;
  Memo.ReadOnly := True;
  Memo.ScrollBars := ssBoth;
end;
```

## Compatibilité multi-plateforme (Windows/Ubuntu)

### Différences à prendre en compte

Le docking fonctionne de manière similaire sur Windows et Ubuntu, mais il y a quelques spécificités :

#### Windows
- Les bordures de fenêtre sont légèrement plus épaisses
- L'effet "snap" (accrochage) est natif dans Windows 10/11
- Les splitters peuvent avoir un aspect différent selon le thème

#### Ubuntu/Linux
- Les gestionnaires de fenêtres (GNOME, KDE, XFCE) ont des comportements différents
- Le redimensionnement peut être moins fluide selon le widgetset utilisé (GTK2 vs GTK3 vs Qt)
- Les raccourcis clavier peuvent différer

### Code portable

```pascal
procedure TfrmMain.ApplyPlatformSpecificSettings;  
begin
  {$IFDEF WINDOWS}
  // Ajustements pour Windows
  splLeft.Width := 5;
  splRight.Width := 5;
  splBottom.Height := 5;
  {$ENDIF}

  {$IFDEF UNIX}
    {$IFDEF LINUX}
    // Ajustements pour Linux/Ubuntu
    splLeft.Width := 3;
    splRight.Width := 3;
    splBottom.Height := 3;
    {$ENDIF}
  {$ENDIF}
end;
```

### Sauvegarde du layout avec chemins compatibles

```pascal
function TfrmMain.GetConfigFilePath: string;  
begin
  {$IFDEF WINDOWS}
  // Windows : %APPDATA%\MonApplication
  Result := GetEnvironmentVariable('APPDATA') + '\MonApplication\layout.ini';
  {$ENDIF}

  {$IFDEF UNIX}
  // Linux : ~/.config/MonApplication
  Result := GetEnvironmentVariable('HOME') + '/.config/MonApplication/layout.ini';
  {$ENDIF}

  // Créer le répertoire s'il n'existe pas
  ForceDirectories(ExtractFilePath(Result));
end;
```

## Bonnes pratiques

### 1. Performance
- Ne créez pas trop de panneaux dockables (maximum 10-15)
- Utilisez `BeginUpdate/EndUpdate` lors de réorganisations massives
- Chargez le contenu des panneaux uniquement quand ils sont visibles

### 2. Ergonomie
- Fournissez une disposition par défaut logique
- Permettez toujours la réinitialisation de la disposition
- Gardez les fonctions principales toujours accessibles
- Ajoutez des tooltips pour expliquer le rôle de chaque panneau

### 3. Sauvegarde
- Sauvegardez la disposition à la fermeture de l'application
- Offrez des "workspaces" prédéfinis (ex: "Développement", "Débogage", "Design")
- Permettez l'export/import de dispositions

### 4. Accessibilité
- Assurez-vous que tous les panneaux sont accessibles au clavier
- Fournissez des raccourcis pour afficher/masquer les panneaux
- Utilisez des couleurs contrastées pour les séparateurs

## Exemple complet : Application de type IDE

Voici un exemple simplifié d'une application de type éditeur avec docking :

```pascal
unit IDEMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls, Menus, SynEdit;

type
  TfrmIDEMain = class(TForm)
    MainMenu1: TMainMenu;
    StatusBar1: TStatusBar;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    // Panneaux
    pnlLeft, pnlRight, pnlBottom, pnlCenter: TPanel;
    splLeft, splRight, splBottom: TSplitter;

    // Composants
    FProjectTree: TTreeView;
    FEditor: TSynEdit;
    FProperties: TListView;
    FOutput: TMemo;

    // Menus
    mnuFile, mnuView: TMenuItem;

    procedure SetupInterface;
    procedure CreatePanels;
    procedure CreateMenus;
    procedure LoadSettings;
    procedure SaveSettings;

    // Gestionnaires d'événements
    procedure OnTogglePanel(Sender: TObject);
    procedure OnResetLayout(Sender: TObject);
  end;

var
  frmIDEMain: TfrmIDEMain;

implementation

{$R *.lfm}

uses
  IniFiles;

procedure TfrmIDEMain.FormCreate(Sender: TObject);  
begin
  Caption := 'Mon IDE - Interface modulaire';
  Width := 1200;
  Height := 800;
  Position := poScreenCenter;

  SetupInterface;
  LoadSettings;
end;

procedure TfrmIDEMain.SetupInterface;  
begin
  CreatePanels;
  CreateMenus;
end;

procedure TfrmIDEMain.CreatePanels;  
begin
  // Panneau gauche : Explorateur de projet
  pnlLeft := TPanel.Create(Self);
  pnlLeft.Parent := Self;
  pnlLeft.Align := alLeft;
  pnlLeft.Width := 250;
  pnlLeft.Caption := '';
  pnlLeft.BevelOuter := bvNone;

  FProjectTree := TTreeView.Create(Self);
  FProjectTree.Parent := pnlLeft;
  FProjectTree.Align := alClient;

  // Séparateur gauche
  splLeft := TSplitter.Create(Self);
  splLeft.Parent := Self;
  splLeft.Align := alLeft;

  // Panneau droit : Propriétés
  pnlRight := TPanel.Create(Self);
  pnlRight.Parent := Self;
  pnlRight.Align := alRight;
  pnlRight.Width := 300;
  pnlRight.Caption := '';
  pnlRight.BevelOuter := bvNone;

  FProperties := TListView.Create(Self);
  FProperties.Parent := pnlRight;
  FProperties.Align := alClient;
  FProperties.ViewStyle := vsReport;

  // Séparateur droit
  splRight := TSplitter.Create(Self);
  splRight.Parent := Self;
  splRight.Align := alRight;

  // Panneau bas : Sortie
  pnlBottom := TPanel.Create(Self);
  pnlBottom.Parent := Self;
  pnlBottom.Align := alBottom;
  pnlBottom.Height := 200;
  pnlBottom.Caption := '';
  pnlBottom.BevelOuter := bvNone;

  FOutput := TMemo.Create(Self);
  FOutput.Parent := pnlBottom;
  FOutput.Align := alClient;
  FOutput.ScrollBars := ssBoth;
  FOutput.ReadOnly := True;

  // Séparateur bas
  splBottom := TSplitter.Create(Self);
  splBottom.Parent := Self;
  splBottom.Align := alBottom;

  // Zone centrale : Éditeur
  pnlCenter := TPanel.Create(Self);
  pnlCenter.Parent := Self;
  pnlCenter.Align := alClient;
  pnlCenter.Caption := '';
  pnlCenter.BevelOuter := bvNone;

  FEditor := TSynEdit.Create(Self);
  FEditor.Parent := pnlCenter;
  FEditor.Align := alClient;
end;

procedure TfrmIDEMain.CreateMenus;  
var
  MenuItem: TMenuItem;
begin
  // Menu Vue
  mnuView := TMenuItem.Create(Self);
  mnuView.Caption := '&Vue';
  MainMenu1.Items.Add(mnuView);

  MenuItem := TMenuItem.Create(Self);
  MenuItem.Caption := 'Explorateur de projet';
  MenuItem.Tag := 1; // Identifiant du panneau
  MenuItem.OnClick := @OnTogglePanel;
  mnuView.Add(MenuItem);

  MenuItem := TMenuItem.Create(Self);
  MenuItem.Caption := 'Propriétés';
  MenuItem.Tag := 2;
  MenuItem.OnClick := @OnTogglePanel;
  mnuView.Add(MenuItem);

  MenuItem := TMenuItem.Create(Self);
  MenuItem.Caption := 'Sortie';
  MenuItem.Tag := 3;
  MenuItem.OnClick := @OnTogglePanel;
  mnuView.Add(MenuItem);

  mnuView.AddSeparator;

  MenuItem := TMenuItem.Create(Self);
  MenuItem.Caption := 'Réinitialiser la disposition';
  MenuItem.OnClick := @OnResetLayout;
  mnuView.Add(MenuItem);
end;

procedure TfrmIDEMain.OnTogglePanel(Sender: TObject);  
var
  Tag: Integer;
begin
  Tag := (Sender as TMenuItem).Tag;

  case Tag of
    1: begin
      pnlLeft.Visible := not pnlLeft.Visible;
      splLeft.Visible := pnlLeft.Visible;
    end;
    2: begin
      pnlRight.Visible := not pnlRight.Visible;
      splRight.Visible := pnlRight.Visible;
    end;
    3: begin
      pnlBottom.Visible := not pnlBottom.Visible;
      splBottom.Visible := pnlBottom.Visible;
    end;
  end;

  (Sender as TMenuItem).Checked := not (Sender as TMenuItem).Checked;
end;

procedure TfrmIDEMain.OnResetLayout(Sender: TObject);  
begin
  pnlLeft.Width := 250;
  pnlRight.Width := 300;
  pnlBottom.Height := 200;

  pnlLeft.Visible := True;
  pnlRight.Visible := True;
  pnlBottom.Visible := True;
end;

procedure TfrmIDEMain.LoadSettings;  
var
  Ini: TIniFile;
  ConfigFile: string;
begin
  {$IFDEF WINDOWS}
  ConfigFile := GetEnvironmentVariable('APPDATA') + '\MonIDE\layout.ini';
  {$ELSE}
  ConfigFile := GetEnvironmentVariable('HOME') + '/.config/MonIDE/layout.ini';
  {$ENDIF}

  if not FileExists(ConfigFile) then Exit;

  Ini := TIniFile.Create(ConfigFile);
  try
    pnlLeft.Width := Ini.ReadInteger('Layout', 'LeftWidth', 250);
    pnlRight.Width := Ini.ReadInteger('Layout', 'RightWidth', 300);
    pnlBottom.Height := Ini.ReadInteger('Layout', 'BottomHeight', 200);

    pnlLeft.Visible := Ini.ReadBool('Layout', 'LeftVisible', True);
    pnlRight.Visible := Ini.ReadBool('Layout', 'RightVisible', True);
    pnlBottom.Visible := Ini.ReadBool('Layout', 'BottomVisible', True);
  finally
    Ini.Free;
  end;
end;

procedure TfrmIDEMain.SaveSettings;  
var
  Ini: TIniFile;
  ConfigFile: string;
begin
  {$IFDEF WINDOWS}
  ConfigFile := GetEnvironmentVariable('APPDATA') + '\MonIDE\layout.ini';
  {$ELSE}
  ConfigFile := GetEnvironmentVariable('HOME') + '/.config/MonIDE/layout.ini';
  {$ENDIF}

  ForceDirectories(ExtractFilePath(ConfigFile));

  Ini := TIniFile.Create(ConfigFile);
  try
    Ini.WriteInteger('Layout', 'LeftWidth', pnlLeft.Width);
    Ini.WriteInteger('Layout', 'RightWidth', pnlRight.Width);
    Ini.WriteInteger('Layout', 'BottomHeight', pnlBottom.Height);

    Ini.WriteBool('Layout', 'LeftVisible', pnlLeft.Visible);
    Ini.WriteBool('Layout', 'RightVisible', pnlRight.Visible);
    Ini.WriteBool('Layout', 'BottomVisible', pnlBottom.Visible);

    // Sauvegarde de la position de la fenêtre
    Ini.WriteInteger('Window', 'Left', Self.Left);
    Ini.WriteInteger('Window', 'Top', Self.Top);
    Ini.WriteInteger('Window', 'Width', Self.Width);
    Ini.WriteInteger('Window', 'Height', Self.Height);
  finally
    Ini.Free;
  end;
end;

procedure TfrmIDEMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);  
begin
  SaveSettings;
end;

end.
```

## Système de plugins modulaires

Une interface modulaire peut également servir de base pour un système de plugins. Voici comment créer une architecture extensible :

### Définition d'une interface de plugin

```pascal
unit PluginInterface;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, Forms, Menus;

type
  // Interface de base pour tous les plugins
  IPlugin = interface
    ['{A1B2C3D4-E5F6-7890-ABCD-EF1234567890}']
    function GetName: string;
    function GetVersion: string;
    function GetDescription: string;
    function GetAuthor: string;

    procedure Initialize;
    procedure Finalize;

    function CreatePanel(AParent: TWinControl): TPanel;
    procedure UpdatePanel;
  end;

  // Classe de base pour faciliter l'implémentation
  TBasePlugin = class(TInterfacedObject, IPlugin)
  private
    FPanel: TPanel;
  protected
    function GetName: string; virtual; abstract;
    function GetVersion: string; virtual;
    function GetDescription: string; virtual; abstract;
    function GetAuthor: string; virtual;
  public
    procedure Initialize; virtual;
    procedure Finalize; virtual;
    function CreatePanel(AParent: TWinControl): TPanel; virtual; abstract;
    procedure UpdatePanel; virtual;
  end;

implementation

function TBasePlugin.GetVersion: string;  
begin
  Result := '1.0.0';
end;

function TBasePlugin.GetAuthor: string;  
begin
  Result := 'Anonyme';
end;

procedure TBasePlugin.Initialize;  
begin
  // Initialisation par défaut
end;

procedure TBasePlugin.Finalize;  
begin
  // Nettoyage par défaut
  if Assigned(FPanel) then
    FPanel.Free;
end;

procedure TBasePlugin.UpdatePanel;  
begin
  // Mise à jour par défaut
end;

end.
```

### Gestionnaire de plugins

```pascal
unit PluginManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PluginInterface, Controls, ExtCtrls;

type
  TPluginManager = class
  private
    FPlugins: TInterfaceList;
    FLoadedPanels: TList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure RegisterPlugin(APlugin: IPlugin);
    procedure UnregisterPlugin(APlugin: IPlugin);

    function GetPluginCount: Integer;
    function GetPlugin(Index: Integer): IPlugin;

    procedure LoadAllPlugins(AParentForm: TForm);
    procedure UnloadAllPlugins;

    property Plugins[Index: Integer]: IPlugin read GetPlugin;
    property Count: Integer read GetPluginCount;
  end;

implementation

constructor TPluginManager.Create;  
begin
  inherited Create;
  FPlugins := TInterfaceList.Create;
  FLoadedPanels := TList.Create;
end;

destructor TPluginManager.Destroy;  
begin
  UnloadAllPlugins;
  FPlugins.Free;
  FLoadedPanels.Free;
  inherited Destroy;
end;

procedure TPluginManager.RegisterPlugin(APlugin: IPlugin);  
begin
  if FPlugins.IndexOf(APlugin) = -1 then
  begin
    FPlugins.Add(APlugin);
    APlugin.Initialize;
  end;
end;

procedure TPluginManager.UnregisterPlugin(APlugin: IPlugin);  
var
  Index: Integer;
begin
  Index := FPlugins.IndexOf(APlugin);
  if Index <> -1 then
  begin
    APlugin.Finalize;
    FPlugins.Delete(Index);
  end;
end;

function TPluginManager.GetPluginCount: Integer;  
begin
  Result := FPlugins.Count;
end;

function TPluginManager.GetPlugin(Index: Integer): IPlugin;  
begin
  Result := FPlugins[Index] as IPlugin;
end;

procedure TPluginManager.LoadAllPlugins(AParentForm: TForm);  
var
  i: Integer;
  Plugin: IPlugin;
  Panel: TPanel;
begin
  for i := 0 to FPlugins.Count - 1 do
  begin
    Plugin := GetPlugin(i);
    Panel := Plugin.CreatePanel(AParentForm);
    FLoadedPanels.Add(Panel);
  end;
end;

procedure TPluginManager.UnloadAllPlugins;  
var
  i: Integer;
begin
  for i := 0 to FPlugins.Count - 1 do
    GetPlugin(i).Finalize;

  FLoadedPanels.Clear;
end;

end.
```

### Exemple de plugin concret

```pascal
unit LoggerPlugin;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ExtCtrls, StdCtrls, ComCtrls,
  PluginInterface;

type
  TLoggerPlugin = class(TBasePlugin)
  private
    FLogMemo: TMemo;
    FToolBar: TToolBar;
    FBtnClear: TToolButton;
    FBtnSave: TToolButton;

    procedure OnClearClick(Sender: TObject);
    procedure OnSaveClick(Sender: TObject);
  protected
    function GetName: string; override;
    function GetDescription: string; override;
    function GetAuthor: string; override;
  public
    procedure Initialize; override;
    function CreatePanel(AParent: TWinControl): TPanel; override;
    procedure UpdatePanel; override;

    procedure AddLog(const AMessage: string);
  end;

implementation

uses
  Dialogs;

function TLoggerPlugin.GetName: string;  
begin
  Result := 'Logger';
end;

function TLoggerPlugin.GetDescription: string;  
begin
  Result := 'Journal de bord de l''application';
end;

function TLoggerPlugin.GetAuthor: string;  
begin
  Result := 'Votre Nom';
end;

procedure TLoggerPlugin.Initialize;  
begin
  inherited Initialize;
  AddLog('Plugin Logger initialisé');
end;

function TLoggerPlugin.CreatePanel(AParent: TWinControl): TPanel;  
var
  Panel: TPanel;
begin
  // Création du panneau principal
  Panel := TPanel.Create(AParent);
  Panel.Parent := AParent;
  Panel.Align := alBottom;
  Panel.Height := 200;
  Panel.Caption := '';
  Panel.BevelOuter := bvNone;

  // Barre d'outils
  FToolBar := TToolBar.Create(Panel);
  FToolBar.Parent := Panel;
  FToolBar.Align := alTop;

  FBtnClear := TToolButton.Create(FToolBar);
  FBtnClear.Parent := FToolBar;
  FBtnClear.Caption := 'Effacer';
  FBtnClear.OnClick := @OnClearClick;

  FBtnSave := TToolButton.Create(FToolBar);
  FBtnSave.Parent := FToolBar;
  FBtnSave.Caption := 'Sauvegarder';
  FBtnSave.OnClick := @OnSaveClick;

  // Zone de texte pour les logs
  FLogMemo := TMemo.Create(Panel);
  FLogMemo.Parent := Panel;
  FLogMemo.Align := alClient;
  FLogMemo.ScrollBars := ssBoth;
  FLogMemo.ReadOnly := True;
  FLogMemo.Font.Name := 'Courier New';

  Result := Panel;
end;

procedure TLoggerPlugin.UpdatePanel;  
begin
  inherited UpdatePanel;
  // Mise à jour si nécessaire
end;

procedure TLoggerPlugin.AddLog(const AMessage: string);  
var
  TimeStamp: string;
begin
  if Assigned(FLogMemo) then
  begin
    TimeStamp := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);
    FLogMemo.Lines.Add(Format('[%s] %s', [TimeStamp, AMessage]));
  end;
end;

procedure TLoggerPlugin.OnClearClick(Sender: TObject);  
begin
  if MessageDlg('Confirmation', 'Effacer tous les logs ?',
     mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    FLogMemo.Clear;
    AddLog('Logs effacés');
  end;
end;

procedure TLoggerPlugin.OnSaveClick(Sender: TObject);  
var
  SaveDialog: TSaveDialog;
begin
  SaveDialog := TSaveDialog.Create(nil);
  try
    SaveDialog.Filter := 'Fichiers texte (*.txt)|*.txt|Tous les fichiers (*.*)|*.*';
    SaveDialog.DefaultExt := 'txt';
    SaveDialog.FileName := 'logs_' + FormatDateTime('yyyymmdd_hhnnss', Now) + '.txt';

    if SaveDialog.Execute then
    begin
      FLogMemo.Lines.SaveToFile(SaveDialog.FileName);
      AddLog('Logs sauvegardés dans : ' + SaveDialog.FileName);
    end;
  finally
    SaveDialog.Free;
  end;
end;

end.
```

### Utilisation du système de plugins

```pascal
unit MainFormWithPlugins;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus,
  PluginInterface, PluginManager, LoggerPlugin;

type
  TfrmMainPlugins = class(TForm)
    MainMenu1: TMainMenu;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FPluginManager: TPluginManager;
    FLoggerPlugin: TLoggerPlugin;

    procedure InitializePlugins;
    procedure CreatePluginsMenu;
  end;

var
  frmMainPlugins: TfrmMainPlugins;

implementation

{$R *.lfm}

procedure TfrmMainPlugins.FormCreate(Sender: TObject);  
begin
  Caption := 'Application avec système de plugins';
  Width := 1000;
  Height := 700;
  Position := poScreenCenter;

  InitializePlugins;
  CreatePluginsMenu;
end;

procedure TfrmMainPlugins.FormDestroy(Sender: TObject);  
begin
  FPluginManager.Free;
end;

procedure TfrmMainPlugins.InitializePlugins;  
begin
  // Création du gestionnaire de plugins
  FPluginManager := TPluginManager.Create;

  // Enregistrement des plugins
  FLoggerPlugin := TLoggerPlugin.Create;
  FPluginManager.RegisterPlugin(FLoggerPlugin);

  // Vous pouvez ajouter d'autres plugins ici
  // FPluginManager.RegisterPlugin(TMonAutrePlugin.Create);

  // Chargement des panneaux des plugins
  FPluginManager.LoadAllPlugins(Self);

  // Test du logger
  FLoggerPlugin.AddLog('Application démarrée');
end;

procedure TfrmMainPlugins.CreatePluginsMenu;  
var
  mnuPlugins: TMenuItem;
  MenuItem: TMenuItem;
  i: Integer;
  Plugin: IPlugin;
begin
  // Menu Plugins
  mnuPlugins := TMenuItem.Create(Self);
  mnuPlugins.Caption := '&Plugins';
  MainMenu1.Items.Add(mnuPlugins);

  // Liste des plugins
  for i := 0 to FPluginManager.Count - 1 do
  begin
    Plugin := FPluginManager.Plugins[i];

    MenuItem := TMenuItem.Create(Self);
    MenuItem.Caption := Plugin.GetName;
    MenuItem.Hint := Plugin.GetDescription;
    mnuPlugins.Add(MenuItem);
  end;
end;

end.
```

## Gestion avancée du docking avec drag & drop

Pour permettre aux utilisateurs de réorganiser les panneaux par glisser-déposer :

```pascal
unit DragDropDocking;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ExtCtrls, Graphics, Forms;

type
  TDockablePanel = class(TPanel)
  private
    FDragging: Boolean;
    FStartPos: TPoint;
    FDragImage: TImage;

    procedure StartDrag(X, Y: Integer);
    procedure DoDrag(X, Y: Integer);
    procedure EndDrag;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
                       X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
                     X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

constructor TDockablePanel.Create(AOwner: TComponent);  
begin
  inherited Create(AOwner);
  FDragging := False;
  Cursor := crSizeAll;
end;

procedure TDockablePanel.StartDrag(X, Y: Integer);  
begin
  FDragging := True;
  FStartPos := Point(X, Y);

  // Création d'une image fantôme pour le drag
  FDragImage := TImage.Create(nil);
  FDragImage.Width := Width;
  FDragImage.Height := Height;
  FDragImage.Picture.Bitmap.Width := Width;
  FDragImage.Picture.Bitmap.Height := Height;
  FDragImage.Canvas.CopyRect(Rect(0, 0, Width, Height),
                             Canvas, ClientRect);
end;

procedure TDockablePanel.DoDrag(X, Y: Integer);  
var
  DeltaX, DeltaY: Integer;
begin
  if not FDragging then Exit;

  DeltaX := X - FStartPos.X;
  DeltaY := Y - FStartPos.Y;

  // Déplacement visuel
  Left := Left + DeltaX;
  Top := Top + DeltaY;
end;

procedure TDockablePanel.EndDrag;  
begin
  FDragging := False;

  if Assigned(FDragImage) then
  begin
    FDragImage.Free;
    FDragImage := nil;
  end;

  // Détection de la zone de drop et ancrage
  // (Code à adapter selon vos besoins)
end;

procedure TDockablePanel.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);

  if Button = mbLeft then
    StartDrag(X, Y);
end;

procedure TDockablePanel.MouseMove(Shift: TShiftState; X, Y: Integer);  
begin
  inherited MouseMove(Shift, X, Y);

  if ssLeft in Shift then
    DoDrag(X, Y);
end;

procedure TDockablePanel.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);

  if Button = mbLeft then
    EndDrag;
end;

end.
```

## Thèmes et apparence personnalisée

### Application de thèmes aux panneaux dockables

```pascal
unit DockingThemes;

{$mode objfpc}{$H+}

interface

uses
  Classes, Graphics, Controls, ExtCtrls;

type
  TDockingTheme = (dtLight, dtDark, dtBlue, dtCustom);

  TThemeColors = record
    Background: TColor;
    Foreground: TColor;
    Border: TColor;
    Splitter: TColor;
    ActiveCaption: TColor;
    InactiveCaption: TColor;
  end;

  TDockingThemeManager = class
  private
    FCurrentTheme: TDockingTheme;
    FCustomColors: TThemeColors;

    function GetThemeColors: TThemeColors;
  public
    constructor Create;

    procedure ApplyTheme(APanel: TPanel);
    procedure ApplyThemeToForm(AForm: TForm);

    property CurrentTheme: TDockingTheme read FCurrentTheme write FCurrentTheme;
    property CustomColors: TThemeColors read FCustomColors write FCustomColors;
  end;

implementation

constructor TDockingThemeManager.Create;  
begin
  inherited Create;
  FCurrentTheme := dtLight;
end;

function TDockingThemeManager.GetThemeColors: TThemeColors;  
begin
  case FCurrentTheme of
    dtLight:
      begin
        Result.Background := clWhite;
        Result.Foreground := clBlack;
        Result.Border := clGray;
        Result.Splitter := clSilver;
        Result.ActiveCaption := clActiveCaption;
        Result.InactiveCaption := clInactiveCaption;
      end;

    dtDark:
      begin
        Result.Background := RGB(30, 30, 30);
        Result.Foreground := RGB(220, 220, 220);
        Result.Border := RGB(60, 60, 60);
        Result.Splitter := RGB(45, 45, 45);
        Result.ActiveCaption := RGB(0, 122, 204);
        Result.InactiveCaption := RGB(60, 60, 60);
      end;

    dtBlue:
      begin
        Result.Background := RGB(240, 244, 248);
        Result.Foreground := RGB(0, 0, 0);
        Result.Border := RGB(0, 120, 215);
        Result.Splitter := RGB(200, 220, 240);
        Result.ActiveCaption := RGB(0, 120, 215);
        Result.InactiveCaption := RGB(180, 200, 220);
      end;

    dtCustom:
      Result := FCustomColors;
  end;
end;

procedure TDockingThemeManager.ApplyTheme(APanel: TPanel);  
var
  Colors: TThemeColors;
begin
  Colors := GetThemeColors;

  APanel.Color := Colors.Background;
  APanel.Font.Color := Colors.Foreground;
  APanel.BorderColor := Colors.Border;
end;

procedure TDockingThemeManager.ApplyThemeToForm(AForm: TForm);  
var
  i: Integer;
  Colors: TThemeColors;
begin
  Colors := GetThemeColors;

  AForm.Color := Colors.Background;
  AForm.Font.Color := Colors.Foreground;

  // Application aux composants enfants
  for i := 0 to AForm.ComponentCount - 1 do
  begin
    if AForm.Components[i] is TPanel then
      ApplyTheme(AForm.Components[i] as TPanel)
    else if AForm.Components[i] is TSplitter then
    begin
      (AForm.Components[i] as TSplitter).Color := Colors.Splitter;
    end;
  end;
end;

end.
```

### Utilisation du gestionnaire de thèmes

```pascal
procedure TfrmMain.CreateThemeMenu;  
var
  mnuTheme: TMenuItem;
  MenuItem: TMenuItem;
begin
  FThemeManager := TDockingThemeManager.Create;

  mnuTheme := TMenuItem.Create(Self);
  mnuTheme.Caption := '&Thème';
  MainMenu1.Items.Add(mnuTheme);

  MenuItem := TMenuItem.Create(Self);
  MenuItem.Caption := 'Clair';
  MenuItem.Tag := Ord(dtLight);
  MenuItem.OnClick := @OnThemeChange;
  mnuTheme.Add(MenuItem);

  MenuItem := TMenuItem.Create(Self);
  MenuItem.Caption := 'Sombre';
  MenuItem.Tag := Ord(dtDark);
  MenuItem.OnClick := @OnThemeChange;
  mnuTheme.Add(MenuItem);

  MenuItem := TMenuItem.Create(Self);
  MenuItem.Caption := 'Bleu';
  MenuItem.Tag := Ord(dtBlue);
  MenuItem.OnClick := @OnThemeChange;
  mnuTheme.Add(MenuItem);
end;

procedure TfrmMain.OnThemeChange(Sender: TObject);  
begin
  FThemeManager.CurrentTheme := TDockingTheme((Sender as TMenuItem).Tag);
  FThemeManager.ApplyThemeToForm(Self);
end;
```

## Gestion des raccourcis clavier

Pour améliorer l'accessibilité et l'efficacité, ajoutez des raccourcis clavier :

```pascal
procedure TfrmMain.SetupKeyboardShortcuts;  
var
  ActionList: TActionList;
  Action: TAction;
begin
  ActionList := TActionList.Create(Self);

  // F2 : Afficher/masquer explorateur
  Action := TAction.Create(Self);
  Action.ActionList := ActionList;
  Action.ShortCut := VK_F2;
  Action.OnExecute := @ToggleLeftPanel;

  // F3 : Afficher/masquer propriétés
  Action := TAction.Create(Self);
  Action.ActionList := ActionList;
  Action.ShortCut := VK_F3;
  Action.OnExecute := @ToggleRightPanel;

  // F4 : Afficher/masquer console
  Action := TAction.Create(Self);
  Action.ActionList := ActionList;
  Action.ShortCut := VK_F4;
  Action.OnExecute := @ToggleBottomPanel;

  // Ctrl+Shift+R : Réinitialiser layout
  Action := TAction.Create(Self);
  Action.ActionList := ActionList;
  Action.ShortCut := ShortCut(VK_R, [ssCtrl, ssShift]);
  Action.OnExecute := @ResetLayout;

  // Alt+1, Alt+2, etc. : Naviguer entre les panneaux
  Action := TAction.Create(Self);
  Action.ActionList := ActionList;
  Action.ShortCut := ShortCut(VK_1, [ssAlt]);
  Action.OnExecute := @FocusLeftPanel;

  Action := TAction.Create(Self);
  Action.ActionList := ActionList;
  Action.ShortCut := ShortCut(VK_2, [ssAlt]);
  Action.OnExecute := @FocusRightPanel;
end;

procedure TfrmMain.FocusLeftPanel(Sender: TObject);  
begin
  if pnlLeft.Visible then
  begin
    pnlLeft.SetFocus;
    // Trouver le premier contrôle focusable dans le panneau
    if pnlLeft.ControlCount > 0 then
      pnlLeft.Controls[0].SetFocus;
  end;
end;
```

## Performances et optimisations

### Chargement paresseux (Lazy Loading)

Pour les applications avec de nombreux panneaux, chargez le contenu uniquement quand nécessaire :

```pascal
type
  TLazyPanel = class(TPanel)
  private
    FLoaded: Boolean;
    FOnLoad: TNotifyEvent;

    procedure CheckAndLoad;
  protected
    procedure SetVisible(Value: Boolean); override;
  public
    property OnLoad: TNotifyEvent read FOnLoad write FOnLoad;
    property Loaded: Boolean read FLoaded;
  end;

procedure TLazyPanel.CheckAndLoad;  
begin
  if not FLoaded and Assigned(FOnLoad) then
  begin
    FOnLoad(Self);
    FLoaded := True;
  end;
end;

procedure TLazyPanel.SetVisible(Value: Boolean);  
begin
  if Value then
    CheckAndLoad;

  inherited SetVisible(Value);
end;

// Utilisation
procedure TfrmMain.CreateLazyPanel;  
var
  LazyPanel: TLazyPanel;
begin
  LazyPanel := TLazyPanel.Create(Self);
  LazyPanel.Parent := Self;
  LazyPanel.Align := alLeft;
  LazyPanel.Width := 250;
  LazyPanel.Visible := False;
  LazyPanel.OnLoad := @LoadPanelContent;
end;

procedure TfrmMain.LoadPanelContent(Sender: TObject);  
var
  ListBox: TListBox;
  i: Integer;
begin
  // Chargement du contenu seulement maintenant
  ListBox := TListBox.Create(Sender as TPanel);
  ListBox.Parent := Sender as TPanel;
  ListBox.Align := alClient;

  // Remplissage avec des données (peut être coûteux)
  for i := 1 to 1000 do
    ListBox.Items.Add('Item ' + IntToStr(i));
end;
```

### Mise en cache des dispositions

```pascal
type
  TLayoutCache = class
  private
    FLayouts: TStringList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SaveLayout(const AName: string; ALayout: TStringList);
    function LoadLayout(const AName: string): TStringList;
    function HasLayout(const AName: string): Boolean;
    procedure DeleteLayout(const AName: string);
    function GetLayoutNames: TStringArray;
  end;

constructor TLayoutCache.Create;  
begin
  inherited Create;
  FLayouts := TStringList.Create;
  FLayouts.OwnsObjects := True;
end;

destructor TLayoutCache.Destroy;  
begin
  FLayouts.Free;
  inherited Destroy;
end;

procedure TLayoutCache.SaveLayout(const AName: string; ALayout: TStringList);  
var
  Index: Integer;
  StoredLayout: TStringList;
begin
  Index := FLayouts.IndexOf(AName);

  if Index <> -1 then
  begin
    // Mise à jour du layout existant
    StoredLayout := FLayouts.Objects[Index] as TStringList;
    StoredLayout.Assign(ALayout);
  end
  else
  begin
    // Création d'un nouveau layout
    StoredLayout := TStringList.Create;
    StoredLayout.Assign(ALayout);
    FLayouts.AddObject(AName, StoredLayout);
  end;
end;

function TLayoutCache.LoadLayout(const AName: string): TStringList;  
var
  Index: Integer;
begin
  Result := TStringList.Create;
  Index := FLayouts.IndexOf(AName);

  if Index <> -1 then
    Result.Assign(FLayouts.Objects[Index] as TStringList);
end;

function TLayoutCache.HasLayout(const AName: string): Boolean;  
begin
  Result := FLayouts.IndexOf(AName) <> -1;
end;

procedure TLayoutCache.DeleteLayout(const AName: string);  
var
  Index: Integer;
begin
  Index := FLayouts.IndexOf(AName);
  if Index <> -1 then
    FLayouts.Delete(Index);
end;

function TLayoutCache.GetLayoutNames: TStringArray;  
var
  i: Integer;
begin
  SetLength(Result, FLayouts.Count);
  for i := 0 to FLayouts.Count - 1 do
    Result[i] := FLayouts[i];
end;
```

## Workspaces prédéfinis

Permettez aux utilisateurs de basculer entre différentes dispositions prédéfinies :

```pascal
type
  TWorkspace = (wsDefault, wsDevelopment, wsDebug, wsDesign, wsCustom);

procedure TfrmMain.ApplyWorkspace(AWorkspace: TWorkspace);  
begin
  case AWorkspace of
    wsDefault:
      begin
        pnlLeft.Width := 250;
        pnlRight.Width := 300;
        pnlBottom.Height := 200;

        pnlLeft.Visible := True;
        pnlRight.Visible := True;
        pnlBottom.Visible := True;
      end;

    wsDevelopment:
      begin
        // Maximiser l'éditeur
        pnlLeft.Width := 200;
        pnlRight.Visible := False;
        pnlBottom.Height := 150;

        pnlLeft.Visible := True;
        pnlBottom.Visible := True;
      end;

    wsDebug:
      begin
        // Mettre en avant la console et les variables
        pnlLeft.Width := 200;
        pnlRight.Width := 350;
        pnlBottom.Height := 300;

        pnlLeft.Visible := True;
        pnlRight.Visible := True;
        pnlBottom.Visible := True;
      end;

    wsDesign:
      begin
        // Maximiser les outils de design
        pnlLeft.Visible := False;
        pnlRight.Width := 400;
        pnlBottom.Visible := False;

        pnlRight.Visible := True;
      end;

    wsCustom:
      begin
        // Charger la configuration personnalisée de l'utilisateur
        LoadLayout(GetConfigFilePath);
      end;
  end;

  // Mise à jour de l'interface
  UpdateWorkspaceMenu(AWorkspace);
end;

procedure TfrmMain.CreateWorkspaceMenu;  
var
  mnuWorkspace: TMenuItem;
  MenuItem: TMenuItem;
begin
  mnuWorkspace := TMenuItem.Create(Self);
  mnuWorkspace.Caption := '&Espace de travail';
  MainMenu1.Items.Add(mnuWorkspace);

  // Workspace par défaut
  MenuItem := TMenuItem.Create(Self);
  MenuItem.Caption := 'Par défaut';
  MenuItem.Tag := Ord(wsDefault);
  MenuItem.OnClick := @OnWorkspaceChange;
  mnuWorkspace.Add(MenuItem);

  // Workspace développement
  MenuItem := TMenuItem.Create(Self);
  MenuItem.Caption := 'Développement';
  MenuItem.Tag := Ord(wsDevelopment);
  MenuItem.OnClick := @OnWorkspaceChange;
  mnuWorkspace.Add(MenuItem);

  // Workspace debug
  MenuItem := TMenuItem.Create(Self);
  MenuItem.Caption := 'Débogage';
  MenuItem.Tag := Ord(wsDebug);
  MenuItem.OnClick := @OnWorkspaceChange;
  mnuWorkspace.Add(MenuItem);

  // Workspace design
  MenuItem := TMenuItem.Create(Self);
  MenuItem.Caption := 'Design';
  MenuItem.Tag := Ord(wsDesign);
  MenuItem.OnClick := @OnWorkspaceChange;
  mnuWorkspace.Add(MenuItem);

  mnuWorkspace.AddSeparator;

  // Sauvegarder workspace actuel
  MenuItem := TMenuItem.Create(Self);
  MenuItem.Caption := 'Sauvegarder comme personnalisé';
  MenuItem.OnClick := @OnSaveCustomWorkspace;
  mnuWorkspace.Add(MenuItem);
end;

procedure TfrmMain.OnWorkspaceChange(Sender: TObject);  
var
  Workspace: TWorkspace;
begin
  Workspace := TWorkspace((Sender as TMenuItem).Tag);
  ApplyWorkspace(Workspace);
end;

procedure TfrmMain.OnSaveCustomWorkspace(Sender: TObject);  
begin
  SaveLayout(GetConfigFilePath);
  ShowMessage('Espace de travail personnalisé sauvegardé !');
end;

procedure TfrmMain.UpdateWorkspaceMenu(AWorkspace: TWorkspace);  
var
  i: Integer;
  MenuItem: TMenuItem;
begin
  // Mettre à jour les coches dans le menu
  for i := 0 to mnuWorkspace.Count - 1 do
  begin
    MenuItem := mnuWorkspace.Items[i];
    if MenuItem.Tag >= 0 then
      MenuItem.Checked := (MenuItem.Tag = Ord(AWorkspace));
  end;
end;
```

## Animation et transitions fluides

Pour rendre l'interface plus agréable, ajoutez des animations lors du redimensionnement :

```pascal
unit AnimatedDocking;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, Graphics, Forms, ExtendedTimer;

type
  TAnimatedPanel = class(TPanel)
  private
    FTargetWidth: Integer;
    FTargetHeight: Integer;
    FAnimating: Boolean;
    FAnimationTimer: TTimer;
    FAnimationSpeed: Integer;

    procedure OnAnimationTimer(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure AnimateWidth(ANewWidth: Integer);
    procedure AnimateHeight(ANewHeight: Integer);
    procedure AnimateSize(ANewWidth, ANewHeight: Integer);

    property AnimationSpeed: Integer read FAnimationSpeed write FAnimationSpeed;
  end;

implementation

constructor TAnimatedPanel.Create(AOwner: TComponent);  
begin
  inherited Create(AOwner);

  FAnimating := False;
  FAnimationSpeed := 20; // Pixels par frame

  FAnimationTimer := TTimer.Create(Self);
  FAnimationTimer.Interval := 16; // ~60 FPS
  FAnimationTimer.Enabled := False;
  FAnimationTimer.OnTimer := @OnAnimationTimer;
end;

destructor TAnimatedPanel.Destroy;  
begin
  FAnimationTimer.Free;
  inherited Destroy;
end;

procedure TAnimatedPanel.AnimateWidth(ANewWidth: Integer);  
begin
  FTargetWidth := ANewWidth;
  FTargetHeight := Height;
  FAnimating := True;
  FAnimationTimer.Enabled := True;
end;

procedure TAnimatedPanel.AnimateHeight(ANewHeight: Integer);  
begin
  FTargetWidth := Width;
  FTargetHeight := ANewHeight;
  FAnimating := True;
  FAnimationTimer.Enabled := True;
end;

procedure TAnimatedPanel.AnimateSize(ANewWidth, ANewHeight: Integer);  
begin
  FTargetWidth := ANewWidth;
  FTargetHeight := ANewHeight;
  FAnimating := True;
  FAnimationTimer.Enabled := True;
end;

procedure TAnimatedPanel.OnAnimationTimer(Sender: TObject);  
var
  DeltaWidth, DeltaHeight: Integer;
  StepWidth, StepHeight: Integer;
begin
  if not FAnimating then
  begin
    FAnimationTimer.Enabled := False;
    Exit;
  end;

  // Calcul des deltas
  DeltaWidth := FTargetWidth - Width;
  DeltaHeight := FTargetHeight - Height;

  // Si on est arrivé à destination
  if (Abs(DeltaWidth) < 2) and (Abs(DeltaHeight) < 2) then
  begin
    Width := FTargetWidth;
    Height := FTargetHeight;
    FAnimating := False;
    FAnimationTimer.Enabled := False;
    Exit;
  end;

  // Calcul des pas d'animation
  if DeltaWidth > 0 then
    StepWidth := Min(FAnimationSpeed, DeltaWidth)
  else
    StepWidth := Max(-FAnimationSpeed, DeltaWidth);

  if DeltaHeight > 0 then
    StepHeight := Min(FAnimationSpeed, DeltaHeight)
  else
    StepHeight := Max(-FAnimationSpeed, DeltaHeight);

  // Application
  Width := Width + StepWidth;
  Height := Height + StepHeight;

  // Forcer le redessin
  Application.ProcessMessages;
end;

end.
```

### Utilisation des animations

```pascal
procedure TfrmMain.ToggleLeftPanelAnimated(Sender: TObject);  
var
  AnimPanel: TAnimatedPanel;
begin
  if pnlLeft is TAnimatedPanel then
  begin
    AnimPanel := pnlLeft as TAnimatedPanel;

    if AnimPanel.Visible then
    begin
      // Fermeture animée
      AnimPanel.AnimateWidth(0);
      // Masquer après l'animation
      TTimer.Create(Self).OnTimer := procedure(Sender: TObject)
      begin
        AnimPanel.Visible := False;
        (Sender as TTimer).Free;
      end;
    end
    else
    begin
      // Ouverture animée
      AnimPanel.Width := 0;
      AnimPanel.Visible := True;
      AnimPanel.AnimateWidth(250);
    end;
  end;
end;
```

## Gestion de l'état et persistance avancée

### Système de configuration complet

```pascal
unit DockingConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, fpjson, jsonparser;

type
  TPanelConfig = record
    Name: string;
    Visible: Boolean;
    Width: Integer;
    Height: Integer;
    DockSide: TAlign;
  end;

  TDockingConfiguration = class
  private
    FPanels: array of TPanelConfig;
    FConfigFile: string;
  public
    constructor Create(const AConfigFile: string);

    procedure AddPanel(const AName: string; AVisible: Boolean;
                      AWidth, AHeight: Integer; ADockSide: TAlign);
    function GetPanelConfig(const AName: string): TPanelConfig;

    procedure SaveToINI;
    procedure LoadFromINI;

    procedure SaveToJSON;
    procedure LoadFromJSON;

    procedure SaveToXML;
    procedure LoadFromXML;
  end;

implementation

uses
  DOM, XMLRead, XMLWrite;

constructor TDockingConfiguration.Create(const AConfigFile: string);  
begin
  inherited Create;
  FConfigFile := AConfigFile;
  SetLength(FPanels, 0);
end;

procedure TDockingConfiguration.AddPanel(const AName: string;
  AVisible: Boolean; AWidth, AHeight: Integer; ADockSide: TAlign);
var
  Index: Integer;
begin
  Index := Length(FPanels);
  SetLength(FPanels, Index + 1);

  FPanels[Index].Name := AName;
  FPanels[Index].Visible := AVisible;
  FPanels[Index].Width := AWidth;
  FPanels[Index].Height := AHeight;
  FPanels[Index].DockSide := ADockSide;
end;

function TDockingConfiguration.GetPanelConfig(const AName: string): TPanelConfig;  
var
  i: Integer;
begin
  for i := 0 to High(FPanels) do
  begin
    if FPanels[i].Name = AName then
    begin
      Result := FPanels[i];
      Exit;
    end;
  end;

  // Configuration par défaut
  Result.Name := AName;
  Result.Visible := True;
  Result.Width := 250;
  Result.Height := 200;
  Result.DockSide := alLeft;
end;

procedure TDockingConfiguration.SaveToINI;  
var
  Ini: TIniFile;
  i: Integer;
  Section: string;
begin
  Ini := TIniFile.Create(FConfigFile);
  try
    Ini.WriteInteger('General', 'PanelCount', Length(FPanels));

    for i := 0 to High(FPanels) do
    begin
      Section := 'Panel_' + IntToStr(i);
      Ini.WriteString(Section, 'Name', FPanels[i].Name);
      Ini.WriteBool(Section, 'Visible', FPanels[i].Visible);
      Ini.WriteInteger(Section, 'Width', FPanels[i].Width);
      Ini.WriteInteger(Section, 'Height', FPanels[i].Height);
      Ini.WriteInteger(Section, 'DockSide', Ord(FPanels[i].DockSide));
    end;
  finally
    Ini.Free;
  end;
end;

procedure TDockingConfiguration.LoadFromINI;  
var
  Ini: TIniFile;
  i, Count: Integer;
  Section: string;
begin
  if not FileExists(FConfigFile) then Exit;

  Ini := TIniFile.Create(FConfigFile);
  try
    Count := Ini.ReadInteger('General', 'PanelCount', 0);
    SetLength(FPanels, Count);

    for i := 0 to Count - 1 do
    begin
      Section := 'Panel_' + IntToStr(i);
      FPanels[i].Name := Ini.ReadString(Section, 'Name', '');
      FPanels[i].Visible := Ini.ReadBool(Section, 'Visible', True);
      FPanels[i].Width := Ini.ReadInteger(Section, 'Width', 250);
      FPanels[i].Height := Ini.ReadInteger(Section, 'Height', 200);
      FPanels[i].DockSide := TAlign(Ini.ReadInteger(Section, 'DockSide', Ord(alLeft)));
    end;
  finally
    Ini.Free;
  end;
end;

procedure TDockingConfiguration.SaveToJSON;  
var
  JSONArray: TJSONArray;
  JSONPanel: TJSONObject;
  i: Integer;
  FileStream: TFileStream;
begin
  JSONArray := TJSONArray.Create;
  try
    for i := 0 to High(FPanels) do
    begin
      JSONPanel := TJSONObject.Create;
      JSONPanel.Add('name', FPanels[i].Name);
      JSONPanel.Add('visible', FPanels[i].Visible);
      JSONPanel.Add('width', FPanels[i].Width);
      JSONPanel.Add('height', FPanels[i].Height);
      JSONPanel.Add('dockSide', Ord(FPanels[i].DockSide));

      JSONArray.Add(JSONPanel);
    end;

    FileStream := TFileStream.Create(FConfigFile, fmCreate);
    try
      FileStream.WriteBuffer(JSONArray.AsJSON[0], Length(JSONArray.AsJSON));
    finally
      FileStream.Free;
    end;
  finally
    JSONArray.Free;
  end;
end;

procedure TDockingConfiguration.LoadFromJSON;  
var
  JSONData: TJSONData;
  JSONArray: TJSONArray;
  JSONPanel: TJSONObject;
  i: Integer;
  FileContent: string;
begin
  if not FileExists(FConfigFile) then Exit;

  with TStringList.Create do
  try
    LoadFromFile(FConfigFile);
    FileContent := Text;
  finally
    Free;
  end;

  JSONData := GetJSON(FileContent);
  try
    if JSONData is TJSONArray then
    begin
      JSONArray := JSONData as TJSONArray;
      SetLength(FPanels, JSONArray.Count);

      for i := 0 to JSONArray.Count - 1 do
      begin
        JSONPanel := JSONArray.Objects[i];
        FPanels[i].Name := JSONPanel.Get('name', '');
        FPanels[i].Visible := JSONPanel.Get('visible', True);
        FPanels[i].Width := JSONPanel.Get('width', 250);
        FPanels[i].Height := JSONPanel.Get('height', 200);
        FPanels[i].DockSide := TAlign(JSONPanel.Get('dockSide', Ord(alLeft)));
      end;
    end;
  finally
    JSONData.Free;
  end;
end;

procedure TDockingConfiguration.SaveToXML;  
var
  Doc: TXMLDocument;
  RootNode, PanelNode: TDOMNode;
  i: Integer;
begin
  Doc := TXMLDocument.Create;
  try
    RootNode := Doc.CreateElement('DockingConfiguration');
    Doc.AppendChild(RootNode);

    for i := 0 to High(FPanels) do
    begin
      PanelNode := Doc.CreateElement('Panel');

      TDOMElement(PanelNode).SetAttribute('name', FPanels[i].Name);
      TDOMElement(PanelNode).SetAttribute('visible', BoolToStr(FPanels[i].Visible, True));
      TDOMElement(PanelNode).SetAttribute('width', IntToStr(FPanels[i].Width));
      TDOMElement(PanelNode).SetAttribute('height', IntToStr(FPanels[i].Height));
      TDOMElement(PanelNode).SetAttribute('dockSide', IntToStr(Ord(FPanels[i].DockSide)));

      RootNode.AppendChild(PanelNode);
    end;

    WriteXMLFile(Doc, FConfigFile);
  finally
    Doc.Free;
  end;
end;

procedure TDockingConfiguration.LoadFromXML;  
var
  Doc: TXMLDocument;
  RootNode, PanelNode: TDOMNode;
  i, Count: Integer;
begin
  if not FileExists(FConfigFile) then Exit;

  ReadXMLFile(Doc, FConfigFile);
  try
    RootNode := Doc.DocumentElement;
    Count := RootNode.ChildNodes.Count;
    SetLength(FPanels, Count);

    for i := 0 to Count - 1 do
    begin
      PanelNode := RootNode.ChildNodes[i];

      FPanels[i].Name := TDOMElement(PanelNode).GetAttribute('name');
      FPanels[i].Visible := StrToBool(TDOMElement(PanelNode).GetAttribute('visible'));
      FPanels[i].Width := StrToInt(TDOMElement(PanelNode).GetAttribute('width'));
      FPanels[i].Height := StrToInt(TDOMElement(PanelNode).GetAttribute('height'));
      FPanels[i].DockSide := TAlign(StrToInt(TDOMElement(PanelNode).GetAttribute('dockSide')));
    end;
  finally
    Doc.Free;
  end;
end;

end.
```

## Gestion des événements de docking

### Événements personnalisés

```pascal
type
  TDockEvent = procedure(Sender: TObject; APanel: TPanel) of object;
  TDockingEvent = procedure(Sender: TObject; APanel: TPanel;
                           var AllowDock: Boolean) of object;

type
  TEnhancedDockManager = class
  private
    FOnBeforeDock: TDockingEvent;
    FOnAfterDock: TDockEvent;
    FOnBeforeUndock: TDockingEvent;
    FOnAfterUndock: TDockEvent;
    FOnPanelResize: TDockEvent;
    FOnPanelShow: TDockEvent;
    FOnPanelHide: TDockEvent;
  public
    procedure DockPanel(APanel: TPanel; ADockSite: TPanel);
    procedure UndockPanel(APanel: TPanel);

    property OnBeforeDock: TDockingEvent read FOnBeforeDock write FOnBeforeDock;
    property OnAfterDock: TDockEvent read FOnAfterDock write FOnAfterDock;
    property OnBeforeUndock: TDockingEvent read FOnBeforeUndock write FOnBeforeUndock;
    property OnAfterUndock: TDockEvent read FOnAfterUndock write FOnAfterUndock;
    property OnPanelResize: TDockEvent read FOnPanelResize write FOnPanelResize;
    property OnPanelShow: TDockEvent read FOnPanelShow write FOnPanelShow;
    property OnPanelHide: TDockEvent read FOnPanelHide write FOnPanelHide;
  end;

procedure TEnhancedDockManager.DockPanel(APanel: TPanel; ADockSite: TPanel);  
var
  AllowDock: Boolean;
begin
  AllowDock := True;

  // Événement avant l'ancrage
  if Assigned(FOnBeforeDock) then
    FOnBeforeDock(Self, APanel, AllowDock);

  if not AllowDock then Exit;

  // Effectuer l'ancrage
  APanel.Parent := ADockSite;
  APanel.Align := alClient;

  // Événement après l'ancrage
  if Assigned(FOnAfterDock) then
    FOnAfterDock(Self, APanel);
end;

procedure TEnhancedDockManager.UndockPanel(APanel: TPanel);  
var
  AllowUndock: Boolean;
begin
  AllowUndock := True;

  // Événement avant le désancrage
  if Assigned(FOnBeforeUndock) then
    FOnBeforeUndock(Self, APanel, AllowUndock);

  if not AllowUndock then Exit;

  // Effectuer le désancrage (créer une fenêtre flottante)
  APanel.Parent := nil;

  // Événement après le désancrage
  if Assigned(FOnAfterUndock) then
    FOnAfterUndock(Self, APanel);
end;
```

### Utilisation des événements

```pascal
procedure TfrmMain.SetupDockManager;  
begin
  FDockManager := TEnhancedDockManager.Create;

  // Gestionnaires d'événements
  FDockManager.OnBeforeDock := @OnBeforePanelDock;
  FDockManager.OnAfterDock := @OnAfterPanelDock;
  FDockManager.OnPanelShow := @OnPanelShow;
  FDockManager.OnPanelHide := @OnPanelHide;
end;

procedure TfrmMain.OnBeforePanelDock(Sender: TObject; APanel: TPanel;
  var AllowDock: Boolean);
begin
  // Validation avant l'ancrage
  if APanel.Tag = 999 then // Panneau verrouillé
  begin
    AllowDock := False;
    ShowMessage('Ce panneau ne peut pas être déplacé');
  end;
end;

procedure TfrmMain.OnAfterPanelDock(Sender: TObject; APanel: TPanel);  
begin
  // Mise à jour après l'ancrage
  UpdateStatusBar('Panneau ' + APanel.Caption + ' ancré');
  SaveLayout(GetConfigFilePath);
end;

procedure TfrmMain.OnPanelShow(Sender: TObject; APanel: TPanel);  
begin
  // Chargement paresseux du contenu
  if not APanel.Tag = 1 then // Pas encore chargé
  begin
    LoadPanelContent(APanel);
    APanel.Tag := 1; // Marquer comme chargé
  end;

  UpdateStatusBar('Panneau ' + APanel.Caption + ' affiché');
end;

procedure TfrmMain.OnPanelHide(Sender: TObject; APanel: TPanel);  
begin
  UpdateStatusBar('Panneau ' + APanel.Caption + ' masqué');
end;
```

## Barres d'outils dockables

Créez des barres d'outils qui peuvent être déplacées et ancrées :

```pascal
type
  TDockableToolBar = class(TToolBar)
  private
    FDockable: Boolean;
    FDragStartPos: TPoint;
    FDragging: Boolean;
    FFloatingForm: TForm;

    procedure CreateFloatingForm;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
                       X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
                     X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;

    procedure MakeFloating;
    procedure DockTo(AParent: TWinControl);

    property Dockable: Boolean read FDockable write FDockable;
  end;

constructor TDockableToolBar.Create(AOwner: TComponent);  
begin
  inherited Create(AOwner);
  FDockable := True;
  FDragging := False;
  EdgeBorders := [ebLeft, ebTop, ebRight, ebBottom];
  ShowCaptions := True;
end;

procedure TDockableToolBar.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);

  if FDockable and (Button = mbLeft) then
  begin
    FDragging := True;
    FDragStartPos := Point(X, Y);
  end;
end;

procedure TDockableToolBar.MouseMove(Shift: TShiftState; X, Y: Integer);  
begin
  inherited MouseMove(Shift, X, Y);

  if FDragging and (ssLeft in Shift) then
  begin
    // Si déplacement significatif, créer une fenêtre flottante
    if (Abs(X - FDragStartPos.X) > 10) or
       (Abs(Y - FDragStartPos.Y) > 10) then
    begin
      MakeFloating;
      FDragging := False;
    end;
  end;
end;

procedure TDockableToolBar.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  FDragging := False;
end;

procedure TDockableToolBar.CreateFloatingForm;  
begin
  FFloatingForm := TForm.Create(nil);
  FFloatingForm.BorderStyle := bsSizeToolWin;
  FFloatingForm.Caption := Self.Caption;
  FFloatingForm.Width := Self.Width + 20;
  FFloatingForm.Height := Self.Height + 40;
  FFloatingForm.Position := poDesigned;
end;

procedure TDockableToolBar.MakeFloating;  
var
  ScreenPos: TPoint;
begin
  if not Assigned(FFloatingForm) then
    CreateFloatingForm;

  // Position de la fenêtre flottante
  ScreenPos := ClientToScreen(Point(0, 0));
  FFloatingForm.Left := ScreenPos.X;
  FFloatingForm.Top := ScreenPos.Y;

  // Déplacer la toolbar vers la fenêtre flottante
  Self.Parent := FFloatingForm;
  Self.Align := alClient;

  FFloatingForm.Show;
end;

procedure TDockableToolBar.DockTo(AParent: TWinControl);  
begin
  if Assigned(FFloatingForm) then
  begin
    FFloatingForm.Hide;
    Self.Parent := AParent;
  end;
end;
```

## Gestion des multi-moniteurs

Pour les utilisateurs avec plusieurs écrans :

```pascal
uses
  Forms;

type
  TMultiMonitorDockManager = class
  private
    procedure GetMonitorInfo(out MonitorCount: Integer;
                            out Monitors: array of TMonitor);
  public
    procedure SaveWindowPosition(AForm: TForm; const AConfigFile: string);
    procedure RestoreWindowPosition(AForm: TForm; const AConfigFile: string);

    function IsPositionValid(ALeft, ATop, AWidth, AHeight: Integer): Boolean;
    procedure EnsureVisible(AForm: TForm);
  end;

procedure TMultiMonitorDockManager.SaveWindowPosition(AForm: TForm;
  const AConfigFile: string);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(AConfigFile);
  try
    Ini.WriteInteger('Window', 'Left', AForm.Left);
    Ini.WriteInteger('Window', 'Top', AForm.Top);
    Ini.WriteInteger('Window', 'Width', AForm.Width);
    Ini.WriteInteger('Window', 'Height', AForm.Height);
    Ini.WriteInteger('Window', 'Monitor', AForm.Monitor.MonitorNum);
    Ini.WriteInteger('Window', 'State', Ord(AForm.WindowState));
  finally
    Ini.Free;
  end;
end;

procedure TMultiMonitorDockManager.RestoreWindowPosition(AForm: TForm;
  const AConfigFile: string);
var
  Ini: TIniFile;
  Left, Top, Width, Height, MonitorNum: Integer;
begin
  if not FileExists(AConfigFile) then Exit;

  Ini := TIniFile.Create(AConfigFile);
  try
    Left := Ini.ReadInteger('Window', 'Left', 100);
    Top := Ini.ReadInteger('Window', 'Top', 100);
    Width := Ini.ReadInteger('Window', 'Width', 1024);
    Height := Ini.ReadInteger('Window', 'Height', 768);
    MonitorNum := Ini.ReadInteger('Window', 'Monitor', 0);

    // Vérifier que la position est valide
    if IsPositionValid(Left, Top, Width, Height) then
    begin
      AForm.Left := Left;
      AForm.Top := Top;
      AForm.Width := Width;
      AForm.Height := Height;

      // Vérifier que le moniteur existe toujours
      if (MonitorNum >= 0) and (MonitorNum < Screen.MonitorCount) then
      begin
        // La fenêtre sera sur le bon moniteur
      end
      else
      begin
        // Utiliser le moniteur principal
        EnsureVisible(AForm);
      end;
    end
    else
    begin
      // Position par défaut
      AForm.Position := poScreenCenter;
    end;

    // Restaurer l'état de la fenêtre
    AForm.WindowState := TWindowState(Ini.ReadInteger('Window', 'State', Ord(wsNormal)));
  finally
    Ini.Free;
  end;
end;

function TMultiMonitorDockManager.IsPositionValid(ALeft, ATop,
  AWidth, AHeight: Integer): Boolean;
var
  i: Integer;
  Monitor: TMonitor;
  FormRect: TRect;
begin
  Result := False;
  FormRect := Rect(ALeft, ATop, ALeft + AWidth, ATop + AHeight);

  // Vérifier si la fenêtre est au moins partiellement visible sur un écran
  for i := 0 to Screen.MonitorCount - 1 do
  begin
    Monitor := Screen.Monitors[i];
    if IntersectRect(FormRect, Monitor.BoundsRect, FormRect) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

procedure TMultiMonitorDockManager.EnsureVisible(AForm: TForm);  
var
  Monitor: TMonitor;
begin
  // Trouver le moniteur principal ou celui contenant le curseur
  Monitor := Screen.MonitorFromPoint(Mouse.CursorPos);

  // Centrer la fenêtre sur ce moniteur
  AForm.Left := Monitor.Left + (Monitor.Width - AForm.Width) div 2;
  AForm.Top := Monitor.Top + (Monitor.Height - AForm.Height) div 2;
end;
```

## Auto-masquage des panneaux (Auto-hide)

Comme dans Visual Studio, les panneaux peuvent se masquer automatiquement pour gagner de l'espace :

```pascal
type
  TAutoHidePanel = class(TPanel)
  private
    FAutoHide: Boolean;
    FExpanded: Boolean;
    FCollapsedWidth: Integer;
    FExpandedWidth: Integer;
    FTabButton: TSpeedButton;
    FHideTimer: TTimer;

    procedure OnMouseEnterTab(Sender: TObject);
    procedure OnMouseLeavePanel(Sender: TObject);
    procedure OnHideTimer(Sender: TObject);
    procedure CreateTabButton;
  public
    constructor Create(AOwner: TComponent); override;

    procedure EnableAutoHide;
    procedure DisableAutoHide;
    procedure Expand;
    procedure Collapse;

    property AutoHide: Boolean read FAutoHide;
    property Expanded: Boolean read FExpanded;
  end;

constructor TAutoHidePanel.Create(AOwner: TComponent);  
begin
  inherited Create(AOwner);

  FAutoHide := False;
  FExpanded := True;
  FCollapsedWidth := 25;
  FExpandedWidth := 250;

  FHideTimer := TTimer.Create(Self);
  FHideTimer.Interval := 1000; // 1 seconde avant masquage
  FHideTimer.Enabled := False;
  FHideTimer.OnTimer := @OnHideTimer;

  OnMouseLeave := @OnMouseLeavePanel;
end;

procedure TAutoHidePanel.CreateTabButton;  
begin
  FTabButton := TSpeedButton.Create(Self);
  FTabButton.Parent := Self.Parent;
  FTabButton.Width := FCollapsedWidth;
  FTabButton.Height := 100;
  FTabButton.Caption := Self.Caption;
  FTabButton.OnMouseEnter := @OnMouseEnterTab;
  FTabButton.Visible := False;
end;

procedure TAutoHidePanel.EnableAutoHide;  
begin
  if not FAutoHide then
  begin
    FAutoHide := True;
    if not Assigned(FTabButton) then
      CreateTabButton;
    Collapse;
  end;
end;

procedure TAutoHidePanel.DisableAutoHide;  
begin
  if FAutoHide then
  begin
    FAutoHide := False;
    FHideTimer.Enabled := False;
    Expand;
    if Assigned(FTabButton) then
      FTabButton.Visible := False;
  end;
end;

procedure TAutoHidePanel.Expand;  
begin
  if not FExpanded then
  begin
    Width := FExpandedWidth;
    FExpanded := True;
    if Assigned(FTabButton) then
      FTabButton.Visible := False;
    BringToFront;
  end;
end;

procedure TAutoHidePanel.Collapse;  
begin
  if FExpanded and FAutoHide then
  begin
    FExpandedWidth := Width; // Sauvegarder la largeur actuelle
    Width := FCollapsedWidth;
    FExpanded := False;
    if Assigned(FTabButton) then
    begin
      FTabButton.Visible := True;
      FTabButton.BringToFront;
    end;
  end;
end;

procedure TAutoHidePanel.OnMouseEnterTab(Sender: TObject);  
begin
  if FAutoHide and not FExpanded then
  begin
    FHideTimer.Enabled := False;
    Expand;
  end;
end;

procedure TAutoHidePanel.OnMouseLeavePanel(Sender: TObject);  
begin
  if FAutoHide and FExpanded then
  begin
    FHideTimer.Enabled := True;
  end;
end;

procedure TAutoHidePanel.OnHideTimer(Sender: TObject);  
var
  MousePos: TPoint;
begin
  // Vérifier si la souris est toujours dans le panneau
  MousePos := ScreenToClient(Mouse.CursorPos);

  if not PtInRect(ClientRect, MousePos) then
  begin
    FHideTimer.Enabled := False;
    Collapse;
  end;
end;
```

## Gestion de l'historique des dispositions

Permettez aux utilisateurs de naviguer dans l'historique de leurs dispositions :

```pascal
type
  TLayoutHistory = class
  private
    FHistory: TList;
    FCurrentIndex: Integer;
    FMaxHistorySize: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddLayout(ALayout: TDockingConfiguration);
    function CanUndo: Boolean;
    function CanRedo: Boolean;
    function Undo: TDockingConfiguration;
    function Redo: TDockingConfiguration;
    procedure Clear;

    property MaxHistorySize: Integer read FMaxHistorySize write FMaxHistorySize;
  end;

constructor TLayoutHistory.Create;  
begin
  inherited Create;
  FHistory := TList.Create;
  FCurrentIndex := -1;
  FMaxHistorySize := 20;
end;

destructor TLayoutHistory.Destroy;  
begin
  Clear;
  FHistory.Free;
  inherited Destroy;
end;

procedure TLayoutHistory.AddLayout(ALayout: TDockingConfiguration);  
var
  i: Integer;
begin
  // Supprimer tout ce qui vient après l'index actuel
  for i := FHistory.Count - 1 downto FCurrentIndex + 1 do
  begin
    TDockingConfiguration(FHistory[i]).Free;
    FHistory.Delete(i);
  end;

  // Ajouter le nouveau layout
  FHistory.Add(ALayout);
  Inc(FCurrentIndex);

  // Limiter la taille de l'historique
  while FHistory.Count > FMaxHistorySize do
  begin
    TDockingConfiguration(FHistory[0]).Free;
    FHistory.Delete(0);
    Dec(FCurrentIndex);
  end;
end;

function TLayoutHistory.CanUndo: Boolean;  
begin
  Result := FCurrentIndex > 0;
end;

function TLayoutHistory.CanRedo: Boolean;  
begin
  Result := FCurrentIndex < FHistory.Count - 1;
end;

function TLayoutHistory.Undo: TDockingConfiguration;  
begin
  Result := nil;
  if CanUndo then
  begin
    Dec(FCurrentIndex);
    Result := TDockingConfiguration(FHistory[FCurrentIndex]);
  end;
end;

function TLayoutHistory.Redo: TDockingConfiguration;  
begin
  Result := nil;
  if CanRedo then
  begin
    Inc(FCurrentIndex);
    Result := TDockingConfiguration(FHistory[FCurrentIndex]);
  end;
end;

procedure TLayoutHistory.Clear;  
var
  i: Integer;
begin
  for i := 0 to FHistory.Count - 1 do
    TDockingConfiguration(FHistory[i]).Free;
  FHistory.Clear;
  FCurrentIndex := -1;
end;
```

## Tests et débogage

### Tests unitaires pour le système de docking

```pascal
unit DockingTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, DockingConfig, DockingThemes;

type
  TDockingConfigTest = class(TTestCase)
  published
    procedure TestCreateConfiguration;
    procedure TestAddPanel;
    procedure TestSaveLoadINI;
    procedure TestSaveLoadJSON;
  end;

implementation

procedure TDockingConfigTest.TestCreateConfiguration;  
var
  Config: TDockingConfiguration;
begin
  Config := TDockingConfiguration.Create('test.ini');
  try
    AssertNotNull('Configuration should be created', Config);
  finally
    Config.Free;
  end;
end;

procedure TDockingConfigTest.TestAddPanel;  
var
  Config: TDockingConfiguration;
  PanelConfig: TPanelConfig;
begin
  Config := TDockingConfiguration.Create('test.ini');
  try
    Config.AddPanel('TestPanel', True, 250, 200, alLeft);
    PanelConfig := Config.GetPanelConfig('TestPanel');

    AssertEquals('Panel name should match', 'TestPanel', PanelConfig.Name);
    AssertTrue('Panel should be visible', PanelConfig.Visible);
    AssertEquals('Panel width should be 250', 250, PanelConfig.Width);
  finally
    Config.Free;
  end;
end;

procedure TDockingConfigTest.TestSaveLoadINI;  
var
  Config1, Config2: TDockingConfiguration;
  Panel1, Panel2: TPanelConfig;
  TestFile: string;
begin
  TestFile := GetTempDir + 'test_config.ini';

  Config1 := TDockingConfiguration.Create(TestFile);
  try
    Config1.AddPanel('Panel1', True, 300, 150, alLeft);
    Config1.SaveToINI;
  finally
    Config1.Free;
  end;

  Config2 := TDockingConfiguration.Create(TestFile);
  try
    Config2.LoadFromINI;
    Panel2 := Config2.GetPanelConfig('Panel1');

    AssertEquals('Loaded panel width should match', 300, Panel2.Width);
    AssertEquals('Loaded panel height should match', 150, Panel2.Height);
  finally
    Config2.Free;
    DeleteFile(TestFile);
  end;
end;

procedure TDockingConfigTest.TestSaveLoadJSON;  
var
  Config1, Config2: TDockingConfiguration;
  Panel1, Panel2: TPanelConfig;
  TestFile: string;
begin
  TestFile := GetTempDir + 'test_config.json';

  Config1 := TDockingConfiguration.Create(TestFile);
  try
    Config1.AddPanel('Panel1', True, 300, 150, alLeft);
    Config1.SaveToJSON;
  finally
    Config1.Free;
  end;

  Config2 := TDockingConfiguration.Create(TestFile);
  try
    Config2.LoadFromJSON;
    Panel2 := Config2.GetPanelConfig('Panel1');

    AssertEquals('Loaded panel width should match', 300, Panel2.Width);
  finally
    Config2.Free;
    DeleteFile(TestFile);
  end;
end;

initialization
  RegisterTest(TDockingConfigTest);

end.
```

## Conseils de débogage

### Logger pour le docking

```pascal
type
  TDockingLogger = class
  private
    FLogFile: TextFile;
    FEnabled: Boolean;
  public
    constructor Create(const ALogFileName: string);
    destructor Destroy; override;

    procedure Log(const AMessage: string); overload;
    procedure Log(const AFormat: string; const AArgs: array of const); overload;
    procedure LogPanelState(APanel: TPanel);

    property Enabled: Boolean read FEnabled write FEnabled;
  end;

constructor TDockingLogger.Create(const ALogFileName: string);  
begin
  inherited Create;
  FEnabled := True;
  AssignFile(FLogFile, ALogFileName);
  {$I-}
  Rewrite(FLogFile);
  {$I+}
  if IOResult <> 0 then
    FEnabled := False;
end;

destructor TDockingLogger.Destroy;  
begin
  if FEnabled then
    CloseFile(FLogFile);
  inherited Destroy;
end;

procedure TDockingLogger.Log(const AMessage: string);  
var
  TimeStamp: string;
begin
  if not FEnabled then Exit;

  TimeStamp := FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now);
  WriteLn(FLogFile, Format('[%s] %s', [TimeStamp, AMessage]));
  Flush(FLogFile);
end;

procedure TDockingLogger.Log(const AFormat: string; const AArgs: array of const);  
begin
  Log(Format(AFormat, AArgs));
end;

procedure TDockingLogger.LogPanelState(APanel: TPanel);  
begin
  if not FEnabled then Exit;

  Log('--- Panel State: %s ---', [APanel.Name]);
  Log('  Caption: %s', [APanel.Caption]);
  Log('  Visible: %s', [BoolToStr(APanel.Visible, True)]);
  Log('  Position: (%d, %d)', [APanel.Left, APanel.Top]);
  Log('  Size: %d x %d', [APanel.Width, APanel.Height]);
  Log('  Align: %d', [Ord(APanel.Align)]);
  Log('  Parent: %s', [APanel.Parent.Name]);
  Log('------------------------');
end;
```

## Exemple complet : Application professionnelle

Voici un exemple complet d'application avec toutes les fonctionnalités :

```pascal
unit ProfessionalDockingApp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls, Menus, ActnList,
  DockingConfig, DockingThemes, PluginManager, PluginInterface;

type
  TfrmProfessionalApp = class(TForm)
    MainMenu1: TMainMenu;
    StatusBar1: TStatusBar;
    ActionList1: TActionList;

    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
    // Gestionnaires
    FDockingConfig: TDockingConfiguration;
    FThemeManager: TDockingThemeManager;
    FPluginManager: TPluginManager;
    FLayoutHistory: TLayoutHistory;
    FLogger: TDockingLogger;

    // Panneaux
    pnlLeft, pnlRight, pnlBottom, pnlCenter: TPanel;
    splLeft, splRight, splBottom: TSplitter;

    // Auto-hide panels
    FAutoHidePanels: TList;

    // Menus
    mnuFile, mnuView, mnuWorkspace, mnuTheme, mnuPlugins, mnuHelp: TMenuItem;

    // Méthodes privées
    procedure CreateInterface;
    procedure CreatePanels;
    procedure CreateMenus;
    procedure CreateToolbars;
    procedure SetupActions;
    procedure LoadPlugins;

    procedure SaveCurrentLayout;
    procedure LoadSavedLayout;

    // Gestionnaires d'événements
    procedure OnTogglePanel(Sender: TObject);
    procedure OnWorkspaceChange(Sender: TObject);
    procedure OnThemeChange(Sender: TObject);
    procedure OnResetLayout(Sender: TObject);
    procedure OnUndo(Sender: TObject);
    procedure OnRedo(Sender: TObject);
    procedure OnAbout(Sender: TObject);

  public
    procedure UpdateStatusBar(const AMessage: string);
  end;

var
  frmProfessionalApp: TfrmProfessionalApp;

implementation

{$R *.lfm}

uses
  IniFiles;

procedure TfrmProfessionalApp.FormCreate(Sender: TObject);  
begin
  Caption := 'Application Professionnelle avec Docking';
  Width := 1400;
  Height := 900;
  Position := poScreenCenter;

  // Initialisation des gestionnaires
  FDockingConfig := TDockingConfiguration.Create(
    GetAppConfigDir(False) + 'layout.ini');
  FThemeManager := TDockingThemeManager.Create;
  FPluginManager := TPluginManager.Create;
  FLayoutHistory := TLayoutHistory.Create;
  FLogger := TDockingLogger.Create(
    GetAppConfigDir(False) + 'docking.log');
  FAutoHidePanels := TList.Create;

  FLogger.Log('Application started');

  CreateInterface;
  LoadPlugins;
end;

procedure TfrmProfessionalApp.FormShow(Sender: TObject);  
begin
  LoadSavedLayout;
  FLogger.Log('Layout loaded');
  UpdateStatusBar('Prêt');
end;

procedure TfrmProfessionalApp.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  FLogger.Log('Application closing');
  SaveCurrentLayout;

  // Libération des ressources
  FAutoHidePanels.Free;
  FLogger.Free;
  FLayoutHistory.Free;
  FPluginManager.Free;
  FThemeManager.Free;
  FDockingConfig.Free;
end;

procedure TfrmProfessionalApp.CreateInterface;  
begin
  CreatePanels;
  CreateMenus;
  CreateToolbars;
  SetupActions;
end;

procedure TfrmProfessionalApp.CreatePanels;  
var
  TreeView: TTreeView;
  Memo: TMemo;
  ListView: TListView;
begin
  // Panneau gauche - Explorateur
  pnlLeft := TPanel.Create(Self);
  pnlLeft.Parent := Self;
  pnlLeft.Align := alLeft;
  pnlLeft.Width := 250;
  pnlLeft.Caption := '';
  pnlLeft.BevelOuter := bvNone;

  TreeView := TTreeView.Create(pnlLeft);
  TreeView.Parent := pnlLeft;
  TreeView.Align := alClient;

  splLeft := TSplitter.Create(Self);
  splLeft.Parent := Self;
  splLeft.Align := alLeft;
  splLeft.Width := 5;

  // Panneau droit - Propriétés
  pnlRight := TPanel.Create(Self);
  pnlRight.Parent := Self;
  pnlRight.Align := alRight;
  pnlRight.Width := 300;
  pnlRight.Caption := '';
  pnlRight.BevelOuter := bvNone;

  ListView := TListView.Create(pnlRight);
  ListView.Parent := pnlRight;
  ListView.Align := alClient;
  ListView.ViewStyle := vsReport;

  splRight := TSplitter.Create(Self);
  splRight.Parent := Self;
  splRight.Align := alRight;
  splRight.Width := 5;

  // Panneau bas - Console
  pnlBottom := TPanel.Create(Self);
  pnlBottom.Parent := Self;
  pnlBottom.Align := alBottom;
  pnlBottom.Height := 200;
  pnlBottom.Caption := '';
  pnlBottom.BevelOuter := bvNone;

  Memo := TMemo.Create(pnlBottom);
  Memo.Parent := pnlBottom;
  Memo.Align := alClient;
  Memo.ScrollBars := ssBoth;
  Memo.Font.Name := 'Courier New';

  splBottom := TSplitter.Create(Self);
  splBottom.Parent := Self;
  splBottom.Align := alBottom;
  splBottom.Height := 5;

  // Zone centrale
  pnlCenter := TPanel.Create(Self);
  pnlCenter.Parent := Self;
  pnlCenter.Align := alClient;
  pnlCenter.Caption := 'Zone de travail principale';
  pnlCenter.BevelOuter := bvNone;
end;

procedure TfrmProfessionalApp.CreateMenus;  
var
  MenuItem: TMenuItem;
begin
  // Menu Fichier
  mnuFile := TMenuItem.Create(Self);
  mnuFile.Caption := '&Fichier';
  MainMenu1.Items.Add(mnuFile);

  MenuItem := TMenuItem.Create(Self);
  MenuItem.Caption := '&Quitter';
  MenuItem.ShortCut := ShortCut(VK_Q, [ssCtrl]);
  MenuItem.OnClick := @Close;
  mnuFile.Add(MenuItem);

  // Menu Vue
  mnuView := TMenuItem.Create(Self);
  mnuView.Caption := '&Vue';
  MainMenu1.Items.Add(mnuView);

  MenuItem := TMenuItem.Create(Self);
  MenuItem.Caption := 'Explorateur';
  MenuItem.Tag := 1;
  MenuItem.Checked := True;
  MenuItem.OnClick := @OnTogglePanel;
  mnuView.Add(MenuItem);

  MenuItem := TMenuItem.Create(Self);
  MenuItem.Caption := 'Propriétés';
  MenuItem.Tag := 2;
  MenuItem.Checked := True;
  MenuItem.OnClick := @OnTogglePanel;
  mnuView.Add(MenuItem);

  MenuItem := TMenuItem.Create(Self);
  MenuItem.Caption := 'Console';
  MenuItem.Tag := 3;
  MenuItem.Checked := True;
  MenuItem.OnClick := @OnTogglePanel;
  mnuView.Add(MenuItem);

  mnuView.AddSeparator;

  MenuItem := TMenuItem.Create(Self);
  MenuItem.Caption := 'Réinitialiser la disposition';
  MenuItem.OnClick := @OnResetLayout;
  mnuView.Add(MenuItem);

  // Menu Espace de travail
  mnuWorkspace := TMenuItem.Create(Self);
  mnuWorkspace.Caption := '&Espace de travail';
  MainMenu1.Items.Add(mnuWorkspace);

  MenuItem := TMenuItem.Create(Self);
  MenuItem.Caption := 'Annuler disposition';
  MenuItem.ShortCut := ShortCut(VK_Z, [ssCtrl]);
  MenuItem.OnClick := @OnUndo;
  mnuWorkspace.Add(MenuItem);

  MenuItem := TMenuItem.Create(Self);
  MenuItem.Caption := 'Refaire disposition';
  MenuItem.ShortCut := ShortCut(VK_Y, [ssCtrl]);
  MenuItem.OnClick := @OnRedo;
  mnuWorkspace.Add(MenuItem);

  // Menu Thème
  mnuTheme := TMenuItem.Create(Self);
  mnuTheme.Caption := '&Thème';
  MainMenu1.Items.Add(mnuTheme);

  MenuItem := TMenuItem.Create(Self);
  MenuItem.Caption := 'Clair';
  MenuItem.Tag := Ord(dtLight);
  MenuItem.Checked := True;
  MenuItem.OnClick := @OnThemeChange;
  mnuTheme.Add(MenuItem);

  MenuItem := TMenuItem.Create(Self);
  MenuItem.Caption := 'Sombre';
  MenuItem.Tag := Ord(dtDark);
  MenuItem.OnClick := @OnThemeChange;
  mnuTheme.Add(MenuItem);

  // Menu Aide
  mnuHelp := TMenuItem.Create(Self);
  mnuHelp.Caption := '&?';
  MainMenu1.Items.Add(mnuHelp);

  MenuItem := TMenuItem.Create(Self);
  MenuItem.Caption := 'À propos';
  MenuItem.OnClick := @OnAbout;
  mnuHelp.Add(MenuItem);
end;

procedure TfrmProfessionalApp.CreateToolbars;  
begin
  // À implémenter si nécessaire
end;

procedure TfrmProfessionalApp.SetupActions;  
begin
  // À implémenter si nécessaire
end;

procedure TfrmProfessionalApp.LoadPlugins;  
begin
  // Charger les plugins disponibles
  FPluginManager.LoadAllPlugins(Self);
  FLogger.Log('Plugins loaded: %d', [FPluginManager.Count]);
end;

procedure TfrmProfessionalApp.SaveCurrentLayout;  
begin
  FDockingConfig.AddPanel('Left', pnlLeft.Visible,
    pnlLeft.Width, pnlLeft.Height, pnlLeft.Align);
  FDockingConfig.AddPanel('Right', pnlRight.Visible,
    pnlRight.Width, pnlRight.Height, pnlRight.Align);
  FDockingConfig.AddPanel('Bottom', pnlBottom.Visible,
    pnlBottom.Width, pnlBottom.Height, pnlBottom.Align);

  FDockingConfig.SaveToINI;
  FLogger.Log('Layout saved');
end;

procedure TfrmProfessionalApp.LoadSavedLayout;  
var
  LeftConfig, RightConfig, BottomConfig: TPanelConfig;
begin
  FDockingConfig.LoadFromINI;

  LeftConfig := FDockingConfig.GetPanelConfig('Left');
  pnlLeft.Width := LeftConfig.Width;
  pnlLeft.Visible := LeftConfig.Visible;

  RightConfig := FDockingConfig.GetPanelConfig('Right');
  pnlRight.Width := RightConfig.Width;
  pnlRight.Visible := RightConfig.Visible;

  BottomConfig := FDockingConfig.GetPanelConfig('Bottom');
  pnlBottom.Height := BottomConfig.Height;
  pnlBottom.Visible := BottomConfig.Visible;
end;

procedure TfrmProfessionalApp.OnTogglePanel(Sender: TObject);  
var
  Tag: Integer;
  MenuItem: TMenuItem;
begin
  MenuItem := Sender as TMenuItem;
  Tag := MenuItem.Tag;

  case Tag of
    1: begin
      pnlLeft.Visible := not pnlLeft.Visible;
      splLeft.Visible := pnlLeft.Visible;
      MenuItem.Checked := pnlLeft.Visible;
      FLogger.Log('Left panel toggled: %s', [BoolToStr(pnlLeft.Visible, True)]);
    end;
    2: begin
      pnlRight.Visible := not pnlRight.Visible;
      splRight.Visible := pnlRight.Visible;
      MenuItem.Checked := pnlRight.Visible;
      FLogger.Log('Right panel toggled: %s', [BoolToStr(pnlRight.Visible, True)]);
    end;
    3: begin
      pnlBottom.Visible := not pnlBottom.Visible;
      splBottom.Visible := pnlBottom.Visible;
      MenuItem.Checked := pnlBottom.Visible;
      FLogger.Log('Bottom panel toggled: %s', [BoolToStr(pnlBottom.Visible, True)]);
    end;
  end;

  UpdateStatusBar('Panneau mis à jour');
end;

procedure TfrmProfessionalApp.OnWorkspaceChange(Sender: TObject);  
begin
  // Implémenter le changement d'espace de travail
  FLogger.Log('Workspace changed');
end;

procedure TfrmProfessionalApp.OnThemeChange(Sender: TObject);  
var
  Theme: TDockingTheme;
begin
  Theme := TDockingTheme((Sender as TMenuItem).Tag);
  FThemeManager.CurrentTheme := Theme;
  FThemeManager.ApplyThemeToForm(Self);

  FLogger.Log('Theme changed to: %d', [Ord(Theme)]);
  UpdateStatusBar('Thème modifié');
end;

procedure TfrmProfessionalApp.OnResetLayout(Sender: TObject);  
begin
  pnlLeft.Width := 250;
  pnlRight.Width := 300;
  pnlBottom.Height := 200;

  pnlLeft.Visible := True;
  pnlRight.Visible := True;
  pnlBottom.Visible := True;

  splLeft.Visible := True;
  splRight.Visible := True;
  splBottom.Visible := True;

  FLogger.Log('Layout reset to defaults');
  UpdateStatusBar('Disposition réinitialisée');

  // Sauvegarder dans l'historique
  SaveCurrentLayout;
end;

procedure TfrmProfessionalApp.OnUndo(Sender: TObject);  
var
  Config: TDockingConfiguration;
begin
  if FLayoutHistory.CanUndo then
  begin
    Config := FLayoutHistory.Undo;
    if Assigned(Config) then
    begin
      // Appliquer la configuration précédente
      LoadSavedLayout;
      FLogger.Log('Layout undone');
      UpdateStatusBar('Disposition annulée');
    end;
  end
  else
  begin
    UpdateStatusBar('Rien à annuler');
  end;
end;

procedure TfrmProfessionalApp.OnRedo(Sender: TObject);  
var
  Config: TDockingConfiguration;
begin
  if FLayoutHistory.CanRedo then
  begin
    Config := FLayoutHistory.Redo;
    if Assigned(Config) then
    begin
      // Appliquer la configuration suivante
      LoadSavedLayout;
      FLogger.Log('Layout redone');
      UpdateStatusBar('Disposition refaite');
    end;
  end
  else
  begin
    UpdateStatusBar('Rien à refaire');
  end;
end;

procedure TfrmProfessionalApp.OnAbout(Sender: TObject);  
begin
  ShowMessage('Application Professionnelle avec Docking' + LineEnding +
              'Version 1.0' + LineEnding +
              'Développée avec FreePascal/Lazarus' + LineEnding + LineEnding +
              'Fonctionnalités :' + LineEnding +
              '- Interface modulaire avec docking' + LineEnding +
              '- Thèmes personnalisables' + LineEnding +
              '- Système de plugins' + LineEnding +
              '- Sauvegarde de la disposition' + LineEnding +
              '- Historique des modifications');
end;

procedure TfrmProfessionalApp.UpdateStatusBar(const AMessage: string);  
begin
  if Assigned(StatusBar1) and (StatusBar1.Panels.Count > 0) then
    StatusBar1.Panels[0].Text := AMessage
  else
    StatusBar1.SimpleText := AMessage;
end;

end.
```

## Résumé des fonctionnalités implémentées

### 1. Gestion de base du docking
- ✅ Panneaux ancrables sur les 4 côtés
- ✅ Séparateurs redimensionnables (Splitters)
- ✅ Affichage/masquage des panneaux
- ✅ Disposition par défaut et réinitialisation

### 2. Sauvegarde et restauration
- ✅ Sauvegarde en format INI
- ✅ Sauvegarde en format JSON
- ✅ Sauvegarde en format XML
- ✅ Gestion multi-plateforme (Windows/Ubuntu)
- ✅ Gestion multi-moniteurs

### 3. Interface utilisateur
- ✅ Menus contextuels
- ✅ Raccourcis clavier
- ✅ Barre d'état
- ✅ Actions et ActionList

### 4. Fonctionnalités avancées
- ✅ Système de plugins
- ✅ Thèmes (clair, sombre, personnalisé)
- ✅ Auto-masquage des panneaux
- ✅ Animations fluides
- ✅ Historique des dispositions (Undo/Redo)
- ✅ Espaces de travail prédéfinis

### 5. Outils de développement
- ✅ Système de logging
- ✅ Tests unitaires
- ✅ Débogage facilité

## Meilleures pratiques à retenir

### 1. Performance
```pascal
// ✅ BON : Suspendre les mises à jour pendant les modifications
procedure TfrmMain.ReorganizePanels;  
begin
  DisableAutoSizing;
  try
    // Modifications multiples
    pnlLeft.Width := 300;
    pnlRight.Width := 250;
    pnlBottom.Height := 200;
  finally
    EnableAutoSizing;
  end;
end;

// ❌ MAUVAIS : Modifications sans suspension
procedure TfrmMain.ReorganizePanelsBad;  
begin
  pnlLeft.Width := 300;      // Redessin
  pnlRight.Width := 250;     // Redessin
  pnlBottom.Height := 200;   // Redessin
end;
```

### 2. Sauvegarde sûre
```pascal
// ✅ BON : Gestion des erreurs lors de la sauvegarde
procedure TfrmMain.SafeSaveLayout;  
var
  TempFile, FinalFile: string;
begin
  FinalFile := GetConfigFilePath;
  TempFile := FinalFile + '.tmp';

  try
    // Sauvegarder dans un fichier temporaire
    FDockingConfig.SaveToFile(TempFile);

    // Si succès, remplacer l'ancien fichier
    if FileExists(FinalFile) then
      DeleteFile(FinalFile);
    RenameFile(TempFile, FinalFile);
  except
    on E: Exception do
    begin
      FLogger.Log('Error saving layout: %s', [E.Message]);
      if FileExists(TempFile) then
        DeleteFile(TempFile);
    end;
  end;
end;
```

### 3. Compatibilité multi-plateforme
```pascal
// ✅ BON : Chemins compatibles
function GetConfigPath: string;  
begin
  {$IFDEF WINDOWS}
  Result := GetEnvironmentVariable('APPDATA') + PathDelim +
            'MyApp' + PathDelim;
  {$ENDIF}

  {$IFDEF UNIX}
  Result := GetEnvironmentVariable('HOME') + PathDelim +
            '.config' + PathDelim + 'myapp' + PathDelim;
  {$ENDIF}

  ForceDirectories(Result);
end;

// ❌ MAUVAIS : Chemins codés en dur
function GetConfigPathBad: string;  
begin
  Result := 'C:\Users\Admin\AppData\MyApp\'; // Windows uniquement !
end;
```

### 4. Gestion mémoire
```pascal
// ✅ BON : Libération correcte des ressources
destructor TDockingManager.Destroy;  
begin
  // Libérer dans l'ordre inverse de création
  FLayoutHistory.Free;
  FPluginManager.Free;
  FThemeManager.Free;
  FDockingConfig.Free;

  inherited Destroy;
end;

// ❌ MAUVAIS : Oubli de libération
destructor TDockingManagerBad.Destroy;  
begin
  inherited Destroy; // Fuite mémoire !
end;
```

## Pièges courants à éviter

### 1. Ordre des propriétés Align
```pascal
// ❌ PROBLÈME : Mauvais ordre d'alignement
pnlCenter.Align := alClient;  // Créé en premier  
pnlLeft.Align := alLeft;      // Le centre est déjà plein !

// ✅ SOLUTION : Bon ordre
pnlLeft.Align := alLeft;      // Créer d'abord les côtés  
pnlRight.Align := alRight;  
pnlBottom.Align := alBottom;  
pnlCenter.Align := alClient;  // Le centre prend ce qui reste
```

### 2. Splitters et panneaux
```pascal
// ❌ PROBLÈME : Splitter créé avant le panneau
splLeft := TSplitter.Create(Self);  
splLeft.Align := alLeft;  
pnlLeft := TPanel.Create(Self);  
pnlLeft.Align := alLeft;      // Le splitter ne fonctionnera pas bien

// ✅ SOLUTION : Panneau créé avant le splitter
pnlLeft := TPanel.Create(Self);  
pnlLeft.Align := alLeft;  
splLeft := TSplitter.Create(Self);  
splLeft.Align := alLeft;      // Le splitter s'attache au panneau
```

### 3. Visibilité et splitters
```pascal
// ✅ BON : Synchroniser la visibilité
procedure ToggleLeftPanel;  
begin
  pnlLeft.Visible := not pnlLeft.Visible;
  splLeft.Visible := pnlLeft.Visible;  // Synchroniser !
end;

// ❌ MAUVAIS : Oublier le splitter
procedure ToggleLeftPanelBad;  
begin
  pnlLeft.Visible := not pnlLeft.Visible;
  // Le splitter reste visible !
end;
```

## Checklist avant déploiement

### Fonctionnalités essentielles
- [ ] Tous les panneaux peuvent être affichés/masqués
- [ ] Les séparateurs fonctionnent correctement
- [ ] La disposition se sauvegarde à la fermeture
- [ ] La disposition se restaure au démarrage
- [ ] La réinitialisation fonctionne
- [ ] Les raccourcis clavier sont opérationnels

### Multi-plateforme
- [ ] Testé sur Windows (Win32/Win64)
- [ ] Testé sur Ubuntu (GTK2/GTK3)
- [ ] Les chemins de fichiers sont portables
- [ ] Les encodages sont gérés correctement
- [ ] Les thèmes s'appliquent sur les deux OS

### Robustesse
- [ ] Gestion des erreurs lors de la sauvegarde
- [ ] Gestion des fichiers de configuration corrompus
- [ ] Pas de fuite mémoire
- [ ] Les panneaux restent utilisables après redimensionnement
- [ ] Multi-moniteurs géré correctement

### Expérience utilisateur
- [ ] Interface intuitive
- [ ] Animations fluides (si activées)
- [ ] Aide contextuelle disponible
- [ ] Messages d'erreur clairs
- [ ] Performance acceptable même avec de nombreux panneaux

## Ressources et documentation

### Packages Lazarus utiles
- **AnchorDocking** : Système de docking intégré
- **AnchorDockingDsgn** : Designer pour AnchorDocking
- **BGRABitmap** : Pour les graphiques avancés
- **LazControls** : Contrôles supplémentaires

### Lecture recommandée
- Documentation officielle de Lazarus sur les docking
- Wiki Lazarus : "Docking"
- Forum Lazarus : Section "LCL"
- Exemples dans le répertoire `examples/docking` de Lazarus

### Projets open source inspirants
- **Lazarus IDE** : Excellente implémentation de docking
- **Double Commander** : Gestionnaire de fichiers avec panels
- **Dev-C++** : IDE avec interface modulaire

## Évolutions possibles

### Court terme
1. **Drag & drop visuel** : Améliorer l'expérience de réorganisation
2. **Prévisualisation** : Montrer où le panneau va s'ancrer
3. **Fenêtres flottantes** : Support complet du mode fenêtre
4. **Panneaux à onglets** : Superposition de panneaux

### Moyen terme
1. **Synchronisation cloud** : Sauvegarder les dispositions en ligne
2. **Partage de layouts** : Exporter/importer des configurations
3. **Profils utilisateur** : Différents layouts par utilisateur
4. **API de plugins** : Interface standardisée pour extensions

### Long terme
1. **Machine learning** : Suggestions de disposition basées sur l'usage
2. **Collaboration** : Layouts partagés en temps réel
3. **Accessibilité avancée** : Support complet des lecteurs d'écran
4. **WebAssembly** : Port de l'interface vers le web

## Conclusion

Le système de docking et d'interfaces modulaires est un élément crucial pour créer des applications professionnelles modernes. Bien implémenté, il offre :

### Avantages pour l'utilisateur
- **Flexibilité** : Chacun organise son espace de travail
- **Productivité** : Accès rapide aux outils fréquents
- **Confort** : Interface adaptée aux besoins
- **Persistance** : La disposition est mémorisée

### Avantages pour le développeur
- **Modularité** : Code mieux organisé
- **Extensibilité** : Ajout facile de fonctionnalités
- **Maintenance** : Isolation des composants
- **Professionnalisme** : Apparence moderne

### Points clés à retenir

1. **Simplicité avant tout** : Commencez simple avec des panneaux de base
2. **Testez tôt** : Vérifiez la compatibilité multi-plateforme dès le début
3. **Écoutez les utilisateurs** : Leur feedback est précieux pour l'ergonomie
4. **Documentez** : Le code de docking peut devenir complexe
5. **Pensez performance** : Même avec de nombreux panneaux

### Code minimal fonctionnel

Pour terminer, voici le strict minimum pour un système de docking fonctionnel :

```pascal
unit MinimalDocking;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls;

type
  TfrmMinimal = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    pnlLeft, pnlCenter: TPanel;
    splLeft: TSplitter;
  end;

var
  frmMinimal: TfrmMinimal;

implementation

procedure TfrmMinimal.FormCreate(Sender: TObject);  
begin
  // Panneau gauche
  pnlLeft := TPanel.Create(Self);
  pnlLeft.Parent := Self;
  pnlLeft.Align := alLeft;
  pnlLeft.Width := 200;
  pnlLeft.Caption := 'Panneau Gauche';

  // Séparateur
  splLeft := TSplitter.Create(Self);
  splLeft.Parent := Self;
  splLeft.Align := alLeft;

  // Zone centrale
  pnlCenter := TPanel.Create(Self);
  pnlCenter.Parent := Self;
  pnlCenter.Align := alClient;
  pnlCenter.Caption := 'Zone Centrale';
end;

end.
```

**C'est tout !** Vous avez maintenant une interface dockable fonctionnelle en moins de 40 lignes de code. À partir de là, vous pouvez ajouter progressivement les fonctionnalités avancées présentées dans ce tutoriel.

## Exercice final suggéré

Pour consolider vos connaissances, essayez de créer :

1. **Une application de prise de notes** avec :
   - Un panneau gauche pour la liste des notes
   - Un panneau central pour l'éditeur
   - Un panneau droit pour les propriétés (tags, date, etc.)
   - Sauvegarde de la disposition

2. **Un visualiseur de données** avec :
   - Un panneau pour les filtres
   - Un panneau pour le graphique
   - Un panneau pour les statistiques
   - Support de différents thèmes

3. **Un IDE simple** avec :
   - Explorateur de fichiers
   - Éditeur de code
   - Console de sortie
   - Panneau de débogage
   - Système de plugins

Bon développement avec FreePascal et Lazarus ! 🚀

---

**Note importante** : Ce tutoriel couvre les principes fondamentaux du docking dans Lazarus. Pour des applications en production, considérez l'utilisation du package **AnchorDocking** qui est inclus avec Lazarus et offre des fonctionnalités avancées prêtes à l'emploi, tout en permettant la personnalisation.

⏭️ [Éditeurs et designers intégrés](/12-interfaces-graphiques-avancees/07-editeurs-designers-integres.md)
