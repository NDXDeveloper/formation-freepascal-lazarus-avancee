🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 4.4 Création de composants personnalisés portables

## Introduction : Pourquoi créer ses propres composants ?

Imaginez que vous construisez plusieurs applications qui ont toutes besoin d'un champ de saisie d'email avec validation automatique, ou d'un panneau qui affiche l'heure avec un style particulier. Plutôt que de copier-coller le même code dans chaque projet, vous pouvez créer votre propre composant réutilisable.

Créer des composants personnalisés, c'est comme fabriquer vos propres briques LEGO® spécialisées. Une fois créées, vous pouvez les utiliser dans tous vos projets, les partager avec d'autres développeurs, et même les faire évoluer au fil du temps.

Le défi supplémentaire ici est la **portabilité** : vos composants doivent fonctionner aussi bien sur Windows que sur Linux, macOS et autres systèmes supportés par Lazarus.

## Les bases de la création de composants

### Comprendre l'héritage

Tout composant personnalisé hérite d'un composant existant. C'est comme prendre une voiture de base et y ajouter vos propres options.

```pascal
// Hiérarchie de base
TComponent          // Le grand-père de tous
    ↓
TControl           // Peut s'afficher
    ↓
TWinControl        // Peut contenir d'autres composants
    ↓
TCustomControl     // Base idéale pour vos composants visuels
```

### Choisir la bonne classe parent

Le choix de la classe parent est crucial :

```pascal
// Pour un composant non-visuel (timer, gestionnaire de données...)
TMyDataManager = class(TComponent)

// Pour un composant visuel simple (ne contient pas d'autres composants)
TMyIndicator = class(TGraphicControl)

// Pour un composant conteneur (peut avoir des enfants)
TMyPanel = class(TCustomControl)

// Pour étendre un composant existant
TMyButton = class(TButton)
```

## Créer votre premier composant : Un label amélioré

Commençons par un exemple simple : un label qui peut clignoter pour attirer l'attention.

### Étape 1 : Définir la classe

```pascal
unit BlinkingLabel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, ExtCtrls, StdCtrls;

type
  TBlinkingLabel = class(TCustomLabel)
  private
    FBlinking: Boolean;
    FBlinkInterval: Integer;
    FBlinkTimer: TTimer;
    FOriginalColor: TColor;
    FBlinkColor: TColor;
    FIsHighlighted: Boolean;

    procedure SetBlinking(AValue: Boolean);
    procedure SetBlinkInterval(AValue: Integer);
    procedure SetBlinkColor(AValue: TColor);
    procedure OnBlink(Sender: TObject);
  protected
    // Méthodes protégées pour les classes dérivées
    procedure UpdateBlinking; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    // Propriétés visibles dans l'inspecteur d'objets
    property Blinking: Boolean read FBlinking write SetBlinking default False;
    property BlinkInterval: Integer read FBlinkInterval write SetBlinkInterval default 500;
    property BlinkColor: TColor read FBlinkColor write SetBlinkColor default clRed;

    // Exposer les propriétés héritées
    property Caption;
    property Color;
    property Font;
    property Align;
    property Visible;
    property OnClick;
  end;
```

### Étape 2 : Implémenter les méthodes

```pascal
implementation

constructor TBlinkingLabel.Create(AOwner: TComponent);  
begin
  inherited Create(AOwner);

  // Initialisation des valeurs par défaut
  FBlinking := False;
  FBlinkInterval := 500;
  FBlinkColor := clRed;
  FIsHighlighted := False;

  // Créer le timer interne
  FBlinkTimer := TTimer.Create(Self);
  FBlinkTimer.Enabled := False;
  FBlinkTimer.Interval := FBlinkInterval;
  FBlinkTimer.OnTimer := @OnBlink;

  // Sauvegarder la couleur originale
  FOriginalColor := Font.Color;
end;

destructor TBlinkingLabel.Destroy;  
begin
  FBlinkTimer.Enabled := False;
  // FBlinkTimer est détruit automatiquement car Self est son propriétaire
  inherited Destroy;
end;

procedure TBlinkingLabel.SetBlinking(AValue: Boolean);  
begin
  if FBlinking <> AValue then
  begin
    FBlinking := AValue;
    UpdateBlinking;
  end;
end;

procedure TBlinkingLabel.SetBlinkInterval(AValue: Integer);  
begin
  if (AValue > 0) and (FBlinkInterval <> AValue) then
  begin
    FBlinkInterval := AValue;
    FBlinkTimer.Interval := AValue;
  end;
end;

procedure TBlinkingLabel.SetBlinkColor(AValue: TColor);  
begin
  if FBlinkColor <> AValue then
  begin
    FBlinkColor := AValue;
    // Si actuellement en surbrillance, appliquer immédiatement
    if FIsHighlighted then
      Font.Color := FBlinkColor;
  end;
end;

procedure TBlinkingLabel.OnBlink(Sender: TObject);  
begin
  FIsHighlighted := not FIsHighlighted;

  if FIsHighlighted then
    Font.Color := FBlinkColor
  else
    Font.Color := FOriginalColor;

  Invalidate;  // Forcer le rafraîchissement
end;

procedure TBlinkingLabel.UpdateBlinking;  
begin
  FBlinkTimer.Enabled := FBlinking;

  if not FBlinking then
  begin
    // Restaurer la couleur originale
    Font.Color := FOriginalColor;
    FIsHighlighted := False;
    Invalidate;
  end;
end;
```

### Étape 3 : Enregistrer le composant

```pascal
procedure Register;  
begin
  RegisterComponents('Mes Composants', [TBlinkingLabel]);
end;

initialization
  // Code d'initialisation si nécessaire

finalization
  // Code de nettoyage si nécessaire

end.
```

## Créer un composant conteneur portable

Voyons maintenant un composant plus complexe : un panneau avec titre intégré qui s'adapte au système.

### Le panneau titré multi-plateforme

```pascal
unit TitledPanel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, ExtCtrls, LCLType, LCLIntf,
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  {$IFDEF UNIX}
  Unix,
  {$ENDIF}
  InterfaceBase;

type
  TTitlePosition = (tpTop, tpLeft, tpBottom, tpRight);

  TTitledPanel = class(TCustomControl)
  private
    FTitle: string;
    FTitleHeight: Integer;
    FTitleColor: TColor;
    FTitleFont: TFont;
    FTitlePosition: TTitlePosition;
    FBodyColor: TColor;
    FBorderStyle: TBorderStyle;

    procedure SetTitle(const AValue: string);
    procedure SetTitleHeight(AValue: Integer);
    procedure SetTitleColor(AValue: TColor);
    procedure SetTitleFont(AValue: TFont);
    procedure SetTitlePosition(AValue: TTitlePosition);
    procedure SetBodyColor(AValue: TColor);
    procedure SetBorderStyle(AValue: TBorderStyle);

    function GetClientRect: TRect; override;
    procedure AdjustClientRect(var ARect: TRect); override;
  protected
    procedure Paint; override;
    procedure Resize; override;

    // Méthodes spécifiques par plateforme
    procedure DrawTitleBar; virtual;
    procedure DrawBorder; virtual;

    // Gestion du thème système
    function GetSystemTitleHeight: Integer; virtual;
    function GetSystemColors: Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // Méthodes publiques
    procedure UpdateTheme;
    function GetContentArea: TRect;
  published
    property Title: string read FTitle write SetTitle;
    property TitleHeight: Integer read FTitleHeight write SetTitleHeight default 24;
    property TitleColor: TColor read FTitleColor write SetTitleColor default clActiveCaption;
    property TitleFont: TFont read FTitleFont write SetTitleFont;
    property TitlePosition: TTitlePosition read FTitlePosition write SetTitlePosition default tpTop;
    property BodyColor: TColor read FBodyColor write SetBodyColor default clBtnFace;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;

    // Propriétés héritées
    property Align;
    property Anchors;
    property Color;
    property Enabled;
    property Visible;
    property OnClick;
    property OnResize;
  end;
```

### Implémentation avec gestion multi-plateforme

```pascal
implementation

constructor TTitledPanel.Create(AOwner: TComponent);  
begin
  inherited Create(AOwner);

  // Configuration de base
  ControlStyle := ControlStyle + [csAcceptsControls];

  // Initialisation des propriétés
  FTitle := 'Panel Title';
  FTitleHeight := GetSystemTitleHeight;  // Adaptatif au système
  FTitlePosition := tpTop;
  FBorderStyle := bsSingle;

  // Création de la police du titre
  FTitleFont := TFont.Create;
  FTitleFont.Style := [fsBold];

  // Couleurs adaptatives au système
  if GetSystemColors then
  begin
    FTitleColor := clActiveCaption;
    FBodyColor := clBtnFace;
    FTitleFont.Color := clCaptionText;
  end
  else
  begin
    // Couleurs par défaut si pas de thème système
    FTitleColor := $00D77800;  // Bleu
    FBodyColor := clWhite;
    FTitleFont.Color := clWhite;
  end;

  Width := 200;
  Height := 150;
end;

destructor TTitledPanel.Destroy;  
begin
  FTitleFont.Free;
  inherited Destroy;
end;

function TTitledPanel.GetSystemTitleHeight: Integer;  
begin
  // Adapter la hauteur selon le système
  {$IFDEF WINDOWS}
  Result := GetSystemMetrics(SM_CYCAPTION);
  if Result = 0 then Result := 24;
  {$ELSE}
    {$IFDEF LINUX}
    // Sur Linux, utiliser une valeur basée sur le DPI
    Result := MulDiv(24, Screen.PixelsPerInch, 96);
    {$ELSE}
    Result := 24;  // Valeur par défaut
    {$ENDIF}
  {$ENDIF}
end;

function TTitledPanel.GetSystemColors: Boolean;  
begin
  // Détecter si on doit utiliser les couleurs système
  {$IFDEF WINDOWS}
  Result := True;  // Windows a toujours un thème
  {$ELSE}
    {$IFDEF LINUX}
    // Linux : détecter le gestionnaire de fenêtres
    Result := WidgetSet.LCLPlatform in [lpGtk2, lpGtk3, lpQt5];
    {$ELSE}
    Result := False;
    {$ENDIF}
  {$ENDIF}
end;

procedure TTitledPanel.Paint;  
var
  R: TRect;
begin
  R := ClientRect;

  with Canvas do
  begin
    // Dessiner le fond
    Brush.Color := FBodyColor;
    FillRect(R);

    // Dessiner la barre de titre
    DrawTitleBar;

    // Dessiner la bordure
    if FBorderStyle <> bsNone then
      DrawBorder;
  end;
end;

procedure TTitledPanel.DrawTitleBar;  
var
  TitleRect: TRect;
  TextStyle: TTextStyle;
begin
  TitleRect := ClientRect;

  // Calculer la zone du titre selon sa position
  case FTitlePosition of
    tpTop:
      TitleRect.Bottom := TitleRect.Top + FTitleHeight;
    tpBottom:
      TitleRect.Top := TitleRect.Bottom - FTitleHeight;
    tpLeft:
      TitleRect.Right := TitleRect.Left + FTitleHeight;
    tpRight:
      TitleRect.Left := TitleRect.Right - FTitleHeight;
  end;

  with Canvas do
  begin
    // Fond du titre
    Brush.Color := FTitleColor;
    FillRect(TitleRect);

    // Gradient optionnel (Windows Vista+, Linux moderne)
    {$IFDEF WINDOWS}
    if CheckWin32Version(6, 0) then  // Vista ou plus récent
    begin
      // Ajouter un gradient simple
      GradientFill(TitleRect, FTitleColor,
                   MixColors(FTitleColor, clWhite, 128), gdVertical);
    end;
    {$ENDIF}

    // Texte du titre
    Font := FTitleFont;
    TextStyle := Canvas.TextStyle;
    TextStyle.Alignment := taCenter;
    TextStyle.Layout := tlCenter;
    TextStyle.SingleLine := True;

    if FTitlePosition in [tpLeft, tpRight] then
    begin
      // Texte vertical pour les titres latéraux
      Font.Orientation := 900;  // 90 degrés
    end;

    TextRect(TitleRect, 0, 0, FTitle, TextStyle);
  end;
end;

procedure TTitledPanel.DrawBorder;  
var
  R: TRect;
begin
  R := ClientRect;

  with Canvas do
  begin
    Pen.Color := clBtnShadow;

    case FBorderStyle of
      bsSingle:
        begin
          Pen.Width := 1;
          Brush.Style := bsClear;
          Rectangle(R);
        end;

      bsRaised, bsLowered:
        begin
          // Effet 3D adaptatif
          if FBorderStyle = bsRaised then
            Frame3D(Canvas, R, clBtnHighlight, clBtnShadow, 1)
          else
            Frame3D(Canvas, R, clBtnShadow, clBtnHighlight, 1);
        end;
    end;
  end;
end;

procedure TTitledPanel.AdjustClientRect(var ARect: TRect);  
begin
  inherited AdjustClientRect(ARect);

  // Réduire la zone client pour exclure le titre
  case FTitlePosition of
    tpTop:
      Inc(ARect.Top, FTitleHeight);
    tpBottom:
      Dec(ARect.Bottom, FTitleHeight);
    tpLeft:
      Inc(ARect.Left, FTitleHeight);
    tpRight:
      Dec(ARect.Right, FTitleHeight);
  end;

  // Ajuster pour la bordure
  if FBorderStyle <> bsNone then
    InflateRect(ARect, -1, -1);
end;

function TTitledPanel.GetContentArea: TRect;  
begin
  Result := ClientRect;
  AdjustClientRect(Result);
end;

procedure TTitledPanel.UpdateTheme;  
begin
  // Mettre à jour les couleurs selon le thème système actuel
  if GetSystemColors then
  begin
    {$IFDEF WINDOWS}
    // Windows : utiliser les couleurs système
    FTitleColor := GetSysColor(COLOR_ACTIVECAPTION);
    FTitleFont.Color := GetSysColor(COLOR_CAPTIONTEXT);
    FBodyColor := GetSysColor(COLOR_BTNFACE);
    {$ENDIF}

    {$IFDEF LINUX}
    // Linux : essayer de détecter le thème GTK/Qt
    if WidgetSet.LCLPlatform = lpGtk2 then
    begin
      // Utiliser les couleurs GTK
      FTitleColor := clActiveCaption;
      FBodyColor := clBtnFace;
    end;
    {$ENDIF}
  end;

  Invalidate;
end;

// Setters avec invalidation

procedure TTitledPanel.SetTitle(const AValue: string);  
begin
  if FTitle <> AValue then
  begin
    FTitle := AValue;
    Invalidate;
  end;
end;

procedure TTitledPanel.SetTitleHeight(AValue: Integer);  
begin
  if (AValue > 0) and (FTitleHeight <> AValue) then
  begin
    FTitleHeight := AValue;
    Realign;  // Réajuster les composants enfants
    Invalidate;
  end;
end;

// ... Autres setters similaires
```

## Composant non-visuel portable : Gestionnaire de configuration

Créons un gestionnaire de configuration qui s'adapte au système.

```pascal
unit ConfigManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  {$IFDEF WINDOWS}
  Registry, Windows,
  {$ENDIF}
  IniFiles, FileUtil, LazFileUtils;

type
  TConfigLocation = (clPortable, clUser, clSystem);

  TConfigManager = class(TComponent)
  private
    FLocation: TConfigLocation;
    FAppName: string;
    FAutoSave: Boolean;
    FFileName: string;
    FIniFile: TIniFile;
    {$IFDEF WINDOWS}
    FRegistry: TRegistry;
    FUseRegistry: Boolean;
    {$ENDIF}

    function GetConfigPath: string;
    procedure InitializeStorage;
    procedure SetLocation(AValue: TConfigLocation);
  protected
    function GetStoragePath: string; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // Méthodes universelles
    function ReadString(const Section, Key, Default: string): string;
    procedure WriteString(const Section, Key, Value: string);
    function ReadInteger(const Section, Key: string; Default: Integer): Integer;
    procedure WriteInteger(const Section, Key: string; Value: Integer);
    function ReadBool(const Section, Key: string; Default: Boolean): Boolean;
    procedure WriteBool(const Section, Key: string; Value: Boolean);

    // Méthodes utilitaires
    procedure Save;
    procedure Reload;
    function BackupConfig: Boolean;
    procedure RestoreDefaults;

  published
    property Location: TConfigLocation read FLocation write SetLocation default clUser;
    property AppName: string read FAppName write FAppName;
    property AutoSave: Boolean read FAutoSave write FAutoSave default True;
    {$IFDEF WINDOWS}
    property UseRegistry: Boolean read FUseRegistry write FUseRegistry default False;
    {$ENDIF}
  end;
```

### Implémentation multi-plateforme

```pascal
implementation

constructor TConfigManager.Create(AOwner: TComponent);  
begin
  inherited Create(AOwner);

  FLocation := clUser;
  FAutoSave := True;
  FAppName := ChangeFileExt(ExtractFileName(ParamStr(0)), '');

  {$IFDEF WINDOWS}
  FUseRegistry := False;
  FRegistry := nil;
  {$ENDIF}

  InitializeStorage;
end;

destructor TConfigManager.Destroy;  
begin
  if FAutoSave then
    Save;

  FIniFile.Free;
  {$IFDEF WINDOWS}
  FRegistry.Free;
  {$ENDIF}

  inherited Destroy;
end;

function TConfigManager.GetStoragePath: string;  
begin
  case FLocation of
    clPortable:
      // Dans le dossier de l'application
      Result := ExtractFilePath(ParamStr(0));

    clUser:
      begin
        // Dossier utilisateur selon l'OS
        {$IFDEF WINDOWS}
        Result := GetEnvironmentVariable('APPDATA');
        {$ENDIF}
        {$IFDEF UNIX}
        Result := GetEnvironmentVariable('HOME');
        if Result <> '' then
          Result := Result + '/.config';
        {$ENDIF}

        if Result <> '' then
          Result := Result + PathDelim + FAppName
        else
          Result := ExtractFilePath(ParamStr(0));
      end;

    clSystem:
      begin
        // Dossier système
        {$IFDEF WINDOWS}
        Result := GetEnvironmentVariable('PROGRAMDATA');
        {$ENDIF}
        {$IFDEF UNIX}
        Result := '/etc';
        {$ENDIF}

        if Result <> '' then
          Result := Result + PathDelim + FAppName
        else
          Result := ExtractFilePath(ParamStr(0));
      end;
  end;

  // Créer le dossier si nécessaire
  if not DirectoryExists(Result) then
    ForceDirectories(Result);
end;

procedure TConfigManager.InitializeStorage;  
var
  ConfigPath: string;
begin
  {$IFDEF WINDOWS}
  if FUseRegistry then
  begin
    if not Assigned(FRegistry) then
      FRegistry := TRegistry.Create;

    case FLocation of
      clUser:
        FRegistry.RootKey := HKEY_CURRENT_USER;
      clSystem:
        FRegistry.RootKey := HKEY_LOCAL_MACHINE;
      else
        FUseRegistry := False;  // Pas de registre en mode portable
    end;

    if FUseRegistry then
      FRegistry.OpenKey('Software\' + FAppName, True);
  end
  else
  {$ENDIF}
  begin
    ConfigPath := GetStoragePath;
    FFileName := ConfigPath + PathDelim + FAppName + '.ini';

    FreeAndNil(FIniFile);
    FIniFile := TIniFile.Create(FFileName);
  end;
end;

function TConfigManager.ReadString(const Section, Key, Default: string): string;  
begin
  {$IFDEF WINDOWS}
  if FUseRegistry and Assigned(FRegistry) then
  begin
    if FRegistry.OpenKey('Software\' + FAppName + '\' + Section, False) then
    begin
      if FRegistry.ValueExists(Key) then
        Result := FRegistry.ReadString(Key)
      else
        Result := Default;
      FRegistry.CloseKey;
    end
    else
      Result := Default;
  end
  else
  {$ENDIF}
  begin
    Result := FIniFile.ReadString(Section, Key, Default);
  end;
end;

procedure TConfigManager.WriteString(const Section, Key, Value: string);  
begin
  {$IFDEF WINDOWS}
  if FUseRegistry and Assigned(FRegistry) then
  begin
    if FRegistry.OpenKey('Software\' + FAppName + '\' + Section, True) then
    begin
      FRegistry.WriteString(Key, Value);
      FRegistry.CloseKey;
    end;
  end
  else
  {$ENDIF}
  begin
    FIniFile.WriteString(Section, Key, Value);
    if FAutoSave then
      FIniFile.UpdateFile;
  end;
end;

// Méthodes similaires pour Integer et Bool...

function TConfigManager.BackupConfig: Boolean;  
var
  BackupName: string;
begin
  Result := False;

  {$IFDEF WINDOWS}
  if FUseRegistry then
  begin
    // Exporter la clé de registre
    if FRegistry.OpenKey('Software\' + FAppName, False) then
    begin
      BackupName := GetStoragePath + PathDelim + FAppName + '_backup.reg';
      Result := FRegistry.SaveKey('Software\' + FAppName, BackupName);
      FRegistry.CloseKey;
    end;
  end
  else
  {$ENDIF}
  begin
    if FileExists(FFileName) then
    begin
      BackupName := ChangeFileExt(FFileName, '.bak');
      Result := CopyFile(FFileName, BackupName);
    end;
  end;
end;
```

## Techniques avancées pour la portabilité

### Gestion des différences de rendu

```pascal
type
  TCustomButton = class(TCustomControl)
  protected
    procedure DrawButton; virtual;
    function GetButtonStyle: Integer;
  end;

procedure TCustomButton.DrawButton;  
begin
  {$IFDEF WINDOWS}
  // Utiliser le thème Windows
  if ThemeServices.ThemesEnabled then
  begin
    ThemeServices.DrawElement(Canvas.Handle,
      ThemeServices.GetElementDetails(tbPushButtonNormal),
      ClientRect);
  end
  else
  {$ENDIF}
  {$IFDEF LINUX}
  // Utiliser le style GTK/Qt
  if WidgetSet.LCLPlatform = lpGtk2 then
  begin
    // Dessiner avec le style GTK
    DrawGtkButton(Canvas, ClientRect);
  end
  else
  {$ENDIF}
  begin
    // Style par défaut multi-plateforme (utilise Frame3D de la LCL)
    Canvas.Brush.Color := clBtnFace;
    Canvas.FillRect(ClientRect);
    Frame3D(Canvas, ClientRect, clBtnHighlight, clBtnShadow, 1);
  end;
end;
```

### Gestion des chemins et fichiers

```pascal
type
  TFileHelper = class
  public
    class function GetUserDataPath: string;
    class function GetTempPath: string;
    class function NormalizePath(const Path: string): string;
  end;

class function TFileHelper.GetUserDataPath: string;  
begin
  {$IFDEF WINDOWS}
  Result := GetEnvironmentVariable('APPDATA');
  {$ENDIF}
  {$IFDEF DARWIN}
  Result := GetEnvironmentVariable('HOME') + '/Library/Application Support';
  {$ENDIF}
  {$IFDEF LINUX}
  Result := GetEnvironmentVariable('HOME') + '/.local/share';
  {$ENDIF}

  if Result = '' then
    Result := ExtractFilePath(ParamStr(0));
end;

class function TFileHelper.NormalizePath(const Path: string): string;  
begin
  Result := Path;
  // Remplacer les séparateurs
  {$IFDEF WINDOWS}
  Result := StringReplace(Result, '/', '\', [rfReplaceAll]);
  {$ELSE}
  Result := StringReplace(Result, '\', '/', [rfReplaceAll]);
  {$ENDIF}
end;
```

### Gestion des DPI et mise à l'échelle

```pascal
type
  TScalableControl = class(TCustomControl)
  private
    FDesignDPI: Integer;
    FScaleFactor: Double;
  protected
    procedure ScaleControl; virtual;
    function ScaleValue(Value: Integer): Integer;
  public
    constructor Create(AOwner: TComponent); override;
    procedure UpdateDPI;
  end;

constructor TScalableControl.Create(AOwner: TComponent);  
begin
  inherited;
  FDesignDPI := 96;  // DPI de conception standard
  UpdateDPI;
end;

procedure TScalableControl.UpdateDPI;  
begin
  if Screen.PixelsPerInch <> FDesignDPI then
  begin
    FScaleFactor := Screen.PixelsPerInch / FDesignDPI;
    ScaleControl;
  end
  else
    FScaleFactor := 1.0;
end;

function TScalableControl.ScaleValue(Value: Integer): Integer;  
begin
  Result := Round(Value * FScaleFactor);
end;

procedure TScalableControl.ScaleControl;  
begin
  // Adapter les dimensions
  Width := ScaleValue(Width);
  Height := ScaleValue(Height);

  // Adapter la police
  Font.Size := ScaleValue(Font.Size);

  // Adapter les marges
  if Assigned(Parent) then
  begin
    Left := ScaleValue(Left);
    Top := ScaleValue(Top);
  end;
end;
```

## Installation et distribution des composants

### Créer un package pour vos composants

```pascal
// MonPackage.lpk
package MonPackage;

{$mode objfpc}{$H+}

interface

uses
  BlinkingLabel, TitledPanel, ConfigManager,
  LCLBase, LazarusPackageIntf;

implementation

procedure Register;  
begin
  RegisterUnit('BlinkingLabel', @BlinkingLabel.Register);
  RegisterUnit('TitledPanel', @TitledPanel.Register);
  RegisterUnit('ConfigManager', @ConfigManager.Register);
end;

initialization
  RegisterPackage('MonPackage', @Register);
end.
```

### Structure de fichiers recommandée

```
MonComposant/
├── source/
│   ├── BlinkingLabel.pas
│   ├── TitledPanel.pas
│   └── ConfigManager.pas
├── package/
│   └── MonPackage.lpk
├── demo/
│   ├── DemoProject.lpr
│   └── MainForm.pas
├── docs/
│   └── README.md
└── resources/
    ├── icons/
    └── images/
```

### Installation dans Lazarus

1. **Ouvrir le package** : Paquet → Ouvrir un fichier paquet (.lpk)
2. **Compiler** : Cliquer sur "Compiler"
3. **Installer** : Cliquer sur "Installer" (redémarre l'IDE)
4. **Vérifier** : Les composants apparaissent dans la palette

## Tests et validation multi-plateforme

### Framework de test pour composants

```pascal
unit ComponentTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FPCUnit, TestRegistry, BlinkingLabel;

type
  TBlinkingLabelTest = class(TTestCase)
  private
    FComponent: TBlinkingLabel;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCreation;
    procedure TestBlinking;
    procedure TestProperties;
    procedure TestMemoryLeaks;
  end;

implementation

procedure TBlinkingLabelTest.SetUp;  
begin
  FComponent := TBlinkingLabel.Create(nil);
end;

procedure TBlinkingLabelTest.TearDown;  
begin
  FComponent.Free;
  FComponent := nil;
end;

procedure TBlinkingLabelTest.TestCreation;  
begin
  AssertNotNull('Le composant doit être créé', FComponent);
  AssertEquals('Intervalle par défaut', 500, FComponent.BlinkInterval);
  AssertFalse('Clignotement désactivé par défaut', FComponent.Blinking);
end;

procedure TBlinkingLabelTest.TestBlinking;  
begin
  FComponent.Blinking := True;
  AssertTrue('Clignotement activé', FComponent.Blinking);

  FComponent.BlinkInterval := 1000;
  AssertEquals('Nouvel intervalle', 1000, FComponent.BlinkInterval);

  FComponent.Blinking := False;
  AssertFalse('Clignotement désactivé', FComponent.Blinking);
end;

procedure TBlinkingLabelTest.TestProperties;  
begin
  FComponent.Caption := 'Test';
  AssertEquals('Caption défini', 'Test', FComponent.Caption);

  FComponent.BlinkColor := clRed;
  AssertEquals('Couleur de clignotement', clRed, FComponent.BlinkColor);
end;

procedure TBlinkingLabelTest.TestMemoryLeaks;  
var
  InitialMemory: PtrUInt;
  i: Integer;
  TempComponent: TBlinkingLabel;
begin
  InitialMemory := GetHeapStatus.TotalAllocated;

  for i := 1 to 100 do
  begin
    TempComponent := TBlinkingLabel.Create(nil);
    TempComponent.Blinking := True;
    TempComponent.Free;
  end;

  // Vérifier qu'il n'y a pas de fuite mémoire significative
  AssertTrue('Pas de fuite mémoire',
    GetHeapStatus.TotalAllocated - InitialMemory < 1024);
end;

initialization
  RegisterTest(TBlinkingLabelTest);
end.
```

### Tests spécifiques par plateforme

```pascal
unit PlatformTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FPCUnit, TestRegistry, TitledPanel, InterfaceBase;

type
  TPlatformSpecificTest = class(TTestCase)
  private
    FPanel: TTitledPanel;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestWindowsRendering;
    procedure TestLinuxRendering;
    procedure TestMacOSRendering;
    procedure TestDPIScaling;
    procedure TestThemeAdaptation;
  end;

implementation

procedure TPlatformSpecificTest.SetUp;  
begin
  FPanel := TTitledPanel.Create(nil);
end;

procedure TPlatformSpecificTest.TearDown;  
begin
  FPanel.Free;
end;

procedure TPlatformSpecificTest.TestWindowsRendering;  
begin
  {$IFDEF WINDOWS}
  FPanel.UpdateTheme;

  // Vérifier que les couleurs système sont appliquées
  AssertTrue('Couleur Windows valide',
    FPanel.TitleColor <> clNone);

  // Vérifier la hauteur du titre système
  AssertTrue('Hauteur titre Windows',
    FPanel.TitleHeight > 0);
  {$ELSE}
  Ignore('Test Windows uniquement');
  {$ENDIF}
end;

procedure TPlatformSpecificTest.TestLinuxRendering;  
begin
  {$IFDEF LINUX}
  // Tester selon le widgetset
  case WidgetSet.LCLPlatform of
    lpGtk2, lpGtk3:
      begin
        AssertTrue('GTK initialisé', True);
        AssertTrue('Thème GTK détecté', FPanel.GetSystemColors);
      end;
    lpQt5:
      begin
        AssertTrue('Qt5 initialisé', True);
      end;
  end;
  {$ELSE}
  Ignore('Test Linux uniquement');
  {$ENDIF}
end;

procedure TPlatformSpecificTest.TestMacOSRendering;  
begin
  {$IFDEF DARWIN}
  // Tests spécifiques macOS
  AssertTrue('Cocoa widgetset', WidgetSet.LCLPlatform = lpCocoa);
  {$ELSE}
  Ignore('Test macOS uniquement');
  {$ENDIF}
end;

procedure TPlatformSpecificTest.TestDPIScaling;  
var
  OriginalDPI: Integer;
begin
  OriginalDPI := Screen.PixelsPerInch;

  // Simuler différents DPI
  // Note : en réalité, on ne peut pas changer le DPI dynamiquement
  // Ce test vérifie juste que le composant réagit correctement

  FPanel.Width := 200;
  FPanel.Height := 150;

  if OriginalDPI <> 96 then
  begin
    AssertTrue('Composant adapté au DPI',
      FPanel.TitleHeight <> 24);
  end;
end;

procedure TPlatformSpecificTest.TestThemeAdaptation;  
begin
  FPanel.UpdateTheme;

  // Vérifier que les couleurs sont définies
  AssertTrue('TitleColor défini', FPanel.TitleColor <> clNone);
  AssertTrue('BodyColor défini', FPanel.BodyColor <> clNone);

  // Vérifier que le titre est lisible
  AssertTrue('Contraste titre',
    FPanel.TitleColor <> FPanel.TitleFont.Color);
end;

initialization
  RegisterTest(TPlatformSpecificTest);
end.
```

## Optimisation et bonnes pratiques

### Gestion efficace des ressources

```pascal
type
  TResourceAwareComponent = class(TCustomControl)
  private
    FBitmap: TBitmap;
    FResourcesLoaded: Boolean;
    procedure LoadResources;
    procedure FreeResources;
  protected
    procedure SetParent(NewParent: TWinControl); override;
    procedure Loaded; override;
  public
    destructor Destroy; override;
  end;

procedure TResourceAwareComponent.LoadResources;  
begin
  if FResourcesLoaded then Exit;

  // Charger les ressources seulement quand nécessaire
  FBitmap := TBitmap.Create;

  // Charger depuis les ressources embarquées
  {$IFDEF WINDOWS}
  FBitmap.LoadFromResourceName(HInstance, 'WINDOWS_ICON');
  {$ENDIF}
  {$IFDEF LINUX}
  FBitmap.LoadFromResourceName(HInstance, 'LINUX_ICON');
  {$ENDIF}

  FResourcesLoaded := True;
end;

procedure TResourceAwareComponent.FreeResources;  
begin
  if not FResourcesLoaded then Exit;

  FBitmap.Free;
  FBitmap := nil;
  FResourcesLoaded := False;
end;

procedure TResourceAwareComponent.SetParent(NewParent: TWinControl);  
begin
  if NewParent <> nil then
    LoadResources
  else
    FreeResources;

  inherited SetParent(NewParent);
end;

procedure TResourceAwareComponent.Loaded;  
begin
  inherited;
  if Parent <> nil then
    LoadResources;
end;

destructor TResourceAwareComponent.Destroy;  
begin
  FreeResources;
  inherited;
end;
```

### Cache et optimisation du rendu

```pascal
type
  TOptimizedControl = class(TCustomControl)
  private
    FBufferBitmap: TBitmap;
    FNeedRepaint: Boolean;
    FLastWidth, FLastHeight: Integer;
  protected
    procedure Paint; override;
    procedure Resize; override;
    procedure InvalidateBuffer;
    procedure PaintBuffer; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

constructor TOptimizedControl.Create(AOwner: TComponent);  
begin
  inherited;
  FBufferBitmap := TBitmap.Create;
  FNeedRepaint := True;
end;

destructor TOptimizedControl.Destroy;  
begin
  FBufferBitmap.Free;
  inherited;
end;

procedure TOptimizedControl.InvalidateBuffer;  
begin
  FNeedRepaint := True;
  Invalidate;
end;

procedure TOptimizedControl.Resize;  
begin
  inherited;
  if (Width <> FLastWidth) or (Height <> FLastHeight) then
  begin
    FLastWidth := Width;
    FLastHeight := Height;
    FBufferBitmap.SetSize(Width, Height);
    InvalidateBuffer;
  end;
end;

procedure TOptimizedControl.Paint;  
begin
  if FNeedRepaint or (FBufferBitmap.Width <> Width) or
     (FBufferBitmap.Height <> Height) then
  begin
    FBufferBitmap.SetSize(Width, Height);
    PaintBuffer;
    FNeedRepaint := False;
  end;

  // Copier le buffer sur le canvas
  Canvas.Draw(0, 0, FBufferBitmap);
end;

procedure TOptimizedControl.PaintBuffer;  
begin
  // Dessiner dans le buffer
  with FBufferBitmap.Canvas do
  begin
    // Effacer le fond
    Brush.Color := Self.Color;
    FillRect(Rect(0, 0, FBufferBitmap.Width, FBufferBitmap.Height));

    // Dessiner le contenu complexe une seule fois
    // ...
  end;
end;
```

### Gestion thread-safe pour composants multi-thread

```pascal
type
  TThreadSafeComponent = class(TComponent)
  private
    FLock: TCriticalSection;
    FData: TStringList;
    FUpdatePending: Boolean;
    procedure DoUpdateUI;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // Méthodes thread-safe
    procedure AddData(const Value: string);
    function GetData: TStringList;
    procedure UpdateFromThread;
  end;

constructor TThreadSafeComponent.Create(AOwner: TComponent);  
begin
  inherited;
  FLock := TCriticalSection.Create;
  FData := TStringList.Create;
  FUpdatePending := False;
end;

destructor TThreadSafeComponent.Destroy;  
begin
  FLock.Free;
  FData.Free;
  inherited;
end;

procedure TThreadSafeComponent.AddData(const Value: string);  
begin
  FLock.Enter;
  try
    FData.Add(Value);
    FUpdatePending := True;
  finally
    FLock.Leave;
  end;

  // Si on est dans le thread principal, mettre à jour immédiatement
  if MainThreadID = GetCurrentThreadId then
    DoUpdateUI
  else
    // Sinon, programmer la mise à jour
    TThread.Synchronize(nil, @DoUpdateUI);
end;

function TThreadSafeComponent.GetData: TStringList;  
begin
  Result := TStringList.Create;
  FLock.Enter;
  try
    Result.Assign(FData);
  finally
    FLock.Leave;
  end;
end;

procedure TThreadSafeComponent.DoUpdateUI;  
begin
  if not FUpdatePending then Exit;

  FLock.Enter;
  try
    FUpdatePending := False;
    // Déclencher un événement ou mettre à jour l'interface
    if Assigned(Owner) and (Owner is TForm) then
      TForm(Owner).Invalidate;
  finally
    FLock.Leave;
  end;
end;

procedure TThreadSafeComponent.UpdateFromThread;  
begin
  if MainThreadID = GetCurrentThreadId then
    DoUpdateUI
  else
    TThread.Synchronize(nil, @DoUpdateUI);
end;
```

## Documentation des composants

### Commentaires XML pour l'aide intégrée

```pascal
unit DocumentedComponent;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls;

type
  {**
   * @abstract Composant exemple bien documenté
   * @author Votre Nom
   * @version 1.0.0
   * @description Ce composant montre comment documenter correctement
   *              pour l'aide contextuelle et la génération de documentation
   *}
  TDocumentedComponent = class(TCustomControl)
  private
    FValue: Integer;
    FOnValueChanged: TNotifyEvent;

    {** Définit la valeur et déclenche l'événement OnValueChanged }
    procedure SetValue(AValue: Integer);
  protected
    {** Surcharge pour personnaliser le rendu
     * @param Canvas Le canvas sur lequel dessiner
     * @note Appelé automatiquement, ne pas appeler directement
     *}
    procedure Paint; override;
  public
    {** Crée une nouvelle instance du composant
     * @param AOwner Le propriétaire du composant
     * @raises EOutOfMemory Si pas assez de mémoire
     *}
    constructor Create(AOwner: TComponent); override;

    {** Réinitialise le composant à son état initial }
    procedure Reset;

    {** Valeur actuelle (lecture seule en runtime) }
    property Value: Integer read FValue;
  published
    {** Valeur entière du composant (0-100)
     * @default 0
     *}
    property PublishedValue: Integer read FValue write SetValue default 0;

    {** Événement déclenché quand la valeur change }
    property OnValueChanged: TNotifyEvent read FOnValueChanged write FOnValueChanged;
  end;
```

### Génération de la documentation

```bash
# Utiliser PasDoc pour générer la documentation HTML
pasdoc --format html --output docs/ *.pas

# Ou fpdoc pour une documentation plus complète
fpdoc --input=MonComposant.pas --output=docs/
```

## Patterns de conception pour composants

### Pattern Observer pour notifications

```pascal
type
  IObserver = interface
    procedure Update(Subject: TObject);
  end;

  TObservableComponent = class(TComponent)
  private
    FObservers: TInterfaceList;
  protected
    procedure NotifyObservers; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure AttachObserver(Observer: IObserver);
    procedure DetachObserver(Observer: IObserver);
  end;

constructor TObservableComponent.Create(AOwner: TComponent);  
begin
  inherited;
  FObservers := TInterfaceList.Create;
end;

destructor TObservableComponent.Destroy;  
begin
  FObservers.Free;
  inherited;
end;

procedure TObservableComponent.AttachObserver(Observer: IObserver);  
begin
  if FObservers.IndexOf(Observer) = -1 then
    FObservers.Add(Observer);
end;

procedure TObservableComponent.DetachObserver(Observer: IObserver);  
begin
  FObservers.Remove(Observer);
end;

procedure TObservableComponent.NotifyObservers;  
var
  i: Integer;
begin
  for i := 0 to FObservers.Count - 1 do
    IObserver(FObservers[i]).Update(Self);
end;
```

### Pattern Command pour actions réversibles

```pascal
type
  TCommand = class
  public
    procedure Execute; virtual; abstract;
    procedure Undo; virtual; abstract;
  end;

  TCommandComponent = class(TComponent)
  private
    FCommands: TList;
    FCurrentIndex: Integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ExecuteCommand(Command: TCommand);
    procedure Undo;
    procedure Redo;
    function CanUndo: Boolean;
    function CanRedo: Boolean;
  end;

procedure TCommandComponent.ExecuteCommand(Command: TCommand);  
var
  i: Integer;
begin
  // Supprimer les commandes après l'index actuel
  for i := FCommands.Count - 1 downto FCurrentIndex + 1 do
  begin
    TCommand(FCommands[i]).Free;
    FCommands.Delete(i);
  end;

  // Ajouter et exécuter la nouvelle commande
  FCommands.Add(Command);
  Command.Execute;
  Inc(FCurrentIndex);
end;

procedure TCommandComponent.Undo;  
begin
  if CanUndo then
  begin
    TCommand(FCommands[FCurrentIndex]).Undo;
    Dec(FCurrentIndex);
  end;
end;

procedure TCommandComponent.Redo;  
begin
  if CanRedo then
  begin
    Inc(FCurrentIndex);
    TCommand(FCommands[FCurrentIndex]).Execute;
  end;
end;
```

## Débogage et diagnostic

### Composant de diagnostic intégré

```pascal
type
  TDiagnosticComponent = class(TComponent)
  private
    FLogFile: TextFile;
    FLogging: Boolean;
    FLogFileName: string;
    procedure OpenLog;
    procedure CloseLog;
  protected
    procedure Log(const Message: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure StartLogging(const FileName: string);
    procedure StopLogging;
    procedure LogComponentState(Component: TComponent);
    procedure LogSystemInfo;
  published
    property Logging: Boolean read FLogging;
  end;

procedure TDiagnosticComponent.LogComponentState(Component: TComponent);  
var
  i: Integer;
  PropInfo: PPropInfo;
  PropList: PPropList;
  PropCount: Integer;
begin
  if not FLogging then Exit;

  Log('=== Component State: ' + Component.Name + ' ===');
  Log('Class: ' + Component.ClassName);

  // Lister toutes les propriétés publiées
  PropCount := GetTypeData(Component.ClassInfo)^.PropCount;
  GetMem(PropList, PropCount * SizeOf(Pointer));
  try
    GetPropInfos(Component.ClassInfo, PropList);

    for i := 0 to PropCount - 1 do
    begin
      PropInfo := PropList^[i];
      if PropInfo <> nil then
      begin
        case PropInfo^.PropType^.Kind of
          tkInteger, tkInt64:
            Log(Format('  %s: %d', [PropInfo^.Name,
                 GetOrdProp(Component, PropInfo)]));
          tkFloat:
            Log(Format('  %s: %f', [PropInfo^.Name,
                 GetFloatProp(Component, PropInfo)]));
          tkString, tkLString, tkAString:
            Log(Format('  %s: %s', [PropInfo^.Name,
                 GetStrProp(Component, PropInfo)]));
          tkBool:
            Log(Format('  %s: %s', [PropInfo^.Name,
                 BoolToStr(GetOrdProp(Component, PropInfo) <> 0, True)]));
        end;
      end;
    end;
  finally
    FreeMem(PropList);
  end;
end;

procedure TDiagnosticComponent.LogSystemInfo;  
begin
  if not FLogging then Exit;

  Log('=== System Information ===');
  Log('OS: ' + {$I %FPCTARGETOS%});
  Log('CPU: ' + {$I %FPCTARGETCPU%});
  Log('LCL Platform: ' + LCLPlatformDisplayNames[WidgetSet.LCLPlatform]);
  Log('Screen DPI: ' + IntToStr(Screen.PixelsPerInch));
  Log('Screen Size: ' + Format('%dx%d', [Screen.Width, Screen.Height]));

  {$IFDEF WINDOWS}
  Log('Windows Version: ' + GetWindowsVersion);
  {$ENDIF}
  {$IFDEF LINUX}
  Log('Desktop Environment: ' + GetEnvironmentVariable('DESKTOP_SESSION'));
  {$ENDIF}
end;
```

## Conseils finaux pour des composants robustes

### Liste de vérification avant publication

1. **Tests multi-plateformes** : Testez sur Windows, Linux et macOS si possible
2. **Gestion mémoire** : Vérifiez les fuites avec HeapTrc
3. **Documentation** : Commentez toutes les propriétés et méthodes publiques
4. **Exemples** : Fournissez au moins un projet de démonstration
5. **Licence** : Choisissez une licence appropriée (LGPL, MIT, etc.)
6. **Versioning** : Utilisez un système de version sémantique (1.0.0)
7. **Compatibilité** : Testez avec différentes versions de Lazarus
8. **Performance** : Profilez sur des machines peu puissantes
9. **Accessibilité** : Vérifiez le support des lecteurs d'écran
10. **Localisation** : Préparez les chaînes pour la traduction

### Ressources pour approfondir

- Wiki Lazarus : Documentation officielle des composants
- Forum Lazarus : Communauté active pour l'aide
- GitHub : Exemples de composants open source
- DelphiDabbler : Techniques transposables à Lazarus

## Conclusion

Créer des composants personnalisés portables est un art qui combine :
- **Maîtrise technique** : Comprendre l'architecture LCL
- **Sensibilité aux plateformes** : Respecter les conventions de chaque OS
- **Rigueur** : Tests et documentation exhaustifs
- **Créativité** : Imaginer des solutions élégantes

Avec ces bases, vous pouvez créer des composants qui enrichissent non seulement vos projets, mais aussi la communauté Lazarus tout entière. Vos composants peuvent devenir les briques fondamentales des applications de demain !

⏭️ [Propriétés publiées et streaming](/04-framework-lcl/05-proprietes-publiees-streaming.md)
