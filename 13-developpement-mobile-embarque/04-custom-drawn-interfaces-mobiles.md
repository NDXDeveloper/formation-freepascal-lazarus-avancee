🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 13.4 Custom Drawn pour interfaces mobiles

## Introduction

**Custom Drawn** est une approche unique du développement d'interfaces graphiques dans Lazarus qui permet de créer des applications véritablement multi-plateformes avec un code unique. Contrairement aux widgetsets traditionnels qui s'appuient sur les composants natifs de chaque système, Custom Drawn dessine l'interface entièrement en code, offrant ainsi un contrôle total et une apparence cohérente sur toutes les plateformes.

### Pourquoi Custom Drawn ?

**Avantages** :
- ✓ **Code unique** : Une seule base de code pour toutes les plateformes
- ✓ **Apparence cohérente** : L'interface est identique partout
- ✓ **Contrôle total** : Vous maîtrisez chaque pixel de l'interface
- ✓ **Légèreté** : Pas de dépendance aux bibliothèques natives lourdes
- ✓ **Flexibilité** : Créez des designs personnalisés sans limites
- ✓ **Performance** : Rendu optimisé pour le mobile

**Inconvénients** :
- ✗ Plus de travail pour reproduire l'apparence native
- ✗ Les utilisateurs peuvent préférer l'apparence native
- ✗ Nécessite plus de code pour les animations complexes
- ✗ Communauté plus restreinte que LCL standard

### Quand utiliser Custom Drawn ?

**Cas d'usage idéaux** :
- Applications avec design unique et brand fort
- Jeux et applications créatives
- Outils cross-platform nécessitant une UI identique
- Prototypes et MVPs rapides
- Applications où la cohérence prime sur l'apparence native

**Quand préférer les composants natifs** :
- Applications devant respecter strictement les guidelines de la plateforme
- Apps nécessitant l'intégration profonde avec le système
- Projets avec une seule plateforme cible

## Architecture Custom Drawn

### Principe de fonctionnement

```
┌──────────────────────────────────────────┐
│    Application FreePascal                │
├──────────────────────────────────────────┤
│                                          │
│  ┌────────────────────────────────────┐  │
│  │  Code de l'interface (Pascal)      │  │
│  │  - Composants Custom Drawn         │  │
│  │  - Gestionnaires d'événements      │  │
│  └──────────────┬─────────────────────┘  │
│                 │                        │
│  ┌──────────────▼─────────────────────┐  │
│  │  Moteur Custom Drawn               │  │
│  │  - Rendu des composants            │  │
│  │  - Gestion des événements tactiles │  │
│  │  - Styles et thèmes                │  │
│  └──────────────┬─────────────────────┘  │
│                 │                        │
│  ┌──────────────▼─────────────────────┐  │
│  │  Canvas graphique                  │  │
│  │  - Dessin 2D                       │  │
│  │  - Primitives graphiques           │  │
│  └──────────────┬─────────────────────┘  │
│                 │                        │
└─────────────────┼────────────────────────┘
                  │
    ┌─────────────┴──────────────┬─────────────┐
    │                            │             │
┌───▼────┐                   ┌───▼────┐    ┌───▼────┐
│Android │                   │  iOS   │    │Windows │
│ Canvas │                   │ Canvas │    │  GDI   │
└────────┘                   └────────┘    └────────┘
```

### Hiérarchie des composants

```pascal
TCustomDrawnControl              // Base abstraite
├── TCustomDrawnButton           // Bouton
├── TCustomDrawnEdit             // Champ de saisie
├── TCustomDrawnCheckBox         // Case à cocher
├── TCustomDrawnRadioButton      // Bouton radio
├── TCustomDrawnComboBox         // Liste déroulante
├── TCustomDrawnListBox          // Liste
├── TCustomDrawnScrollBar        // Barre de défilement
├── TCustomDrawnProgressBar      // Barre de progression
├── TCustomDrawnTrackBar         // Curseur
└── TCustomDrawnPanel            // Conteneur
```

## Configuration du projet Custom Drawn

### Créer un projet Custom Drawn

**Étapes dans Lazarus** :

1. **File → New → Project → Custom Drawn Application**

2. **Configurer les options du projet** :
```pascal
{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

uses
  Classes, SysUtils,
  customdrawnint,      // Interface Custom Drawn
  customdrawndrawers,  // Dessinateurs de composants
  IntfGraphics,        // Primitives graphiques
  FPImage;             // Manipulation d'images
```

3. **Sélectionner les plateformes cibles** :
   - Android (via LAMW ou FPC)
   - iOS (expérimental)
   - Windows
   - Linux
   - macOS

### Structure d'un projet Custom Drawn

```
MonProjetCD/
├── src/
│   ├── MainForm.pas              ← Formulaire principal
│   ├── MainForm.lfm              ← Layout du formulaire
│   ├── CustomComponents.pas      ← Composants personnalisés
│   ├── Styles.pas                ← Styles et thèmes
│   └── Utils.pas                 ← Utilitaires
├── resources/
│   ├── images/                   ← Images de l'interface
│   ├── fonts/                    ← Polices personnalisées
│   └── themes/                   ← Fichiers de thèmes
└── project.lpr                   ← Programme principal
```

### Formulaire Custom Drawn de base

```pascal
unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  customdrawncontrols, customdrawndrawers;

type
  TFormMain = class(TForm)
    CDButton1: TCustomDrawnButton;
    CDEdit1: TCustomDrawnEdit;
    CDLabel1: TCustomDrawnLabel;
    procedure CDButton1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure ConfigurerInterface;
  public
  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

procedure TFormMain.FormCreate(Sender: TObject);  
begin
  ConfigurerInterface;
end;

procedure TFormMain.ConfigurerInterface;  
begin
  // Configuration du label
  CDLabel1.Caption := 'Bienvenue dans Custom Drawn';
  CDLabel1.Font.Size := 18;
  CDLabel1.Font.Style := [fsBold];

  // Configuration du champ de saisie
  CDEdit1.Text := '';
  CDEdit1.HintText := 'Entrez votre nom';

  // Configuration du bouton
  CDButton1.Caption := 'Valider';
  CDButton1.Color := clBlue;
  CDButton1.Font.Color := clWhite;
end;

procedure TFormMain.CDButton1Click(Sender: TObject);  
begin
  ShowMessage('Bonjour ' + CDEdit1.Text + ' !');
end;

end.
```

## Composants Custom Drawn

### Boutons (TCustomDrawnButton)

```pascal
type
  TFormMain = class(TForm)
    CDButton1: TCustomDrawnButton;
  end;

procedure ConfigurerBoutons;  
begin
  // Bouton standard
  CDButton1.Caption := 'Cliquez-moi';
  CDButton1.Width := 150;
  CDButton1.Height := 44;
  CDButton1.Color := clBlue;
  CDButton1.Font.Color := clWhite;
  CDButton1.Font.Size := 14;

  // Coins arrondis
  CDButton1.Rounded := True;
  CDButton1.RoundedRadius := 8;

  // Ombre
  CDButton1.ShowShadow := True;
  CDButton1.ShadowColor := clGray;
  CDButton1.ShadowOffset := 2;
end;

procedure CDButton1Click(Sender: TObject);  
begin
  // Action au clic
  ProcesserAction;
end;

// Bouton avec icône
procedure AjouterIcone;  
var
  icon: TBitmap;
begin
  icon := TBitmap.Create;
  try
    icon.LoadFromFile('images/icon.png');
    CDButton1.Glyph := icon;
    CDButton1.GlyphAlignment := gaLeft;  // Icône à gauche
    CDButton1.Spacing := 8;              // Espacement icône-texte
  finally
    // Ne pas libérer, le bouton prend possession
  end;
end;
```

### Champs de saisie (TCustomDrawnEdit)

```pascal
type
  TFormMain = class(TForm)
    CDEdit1: TCustomDrawnEdit;
  end;

procedure ConfigurerChampsSaisie;  
begin
  // Configuration de base
  CDEdit1.Text := '';
  CDEdit1.HintText := 'Entrez votre email';
  CDEdit1.Font.Size := 14;

  // Style
  CDEdit1.BorderStyle := bsSingle;
  CDEdit1.BorderColor := clGray;
  CDEdit1.BorderWidth := 1;

  // Padding interne
  CDEdit1.LeftPadding := 10;
  CDEdit1.RightPadding := 10;

  // Hauteur
  CDEdit1.Height := 40;

  // Coins arrondis
  CDEdit1.Rounded := True;
  CDEdit1.RoundedRadius := 5;
end;

// Champ mot de passe
procedure ConfigurerMotDePasse;  
begin
  CDEdit1.PasswordChar := '•';
  CDEdit1.HintText := 'Mot de passe';
end;

// Champ numérique
procedure ConfigurerChampNumerique;  
begin
  CDEdit1.NumbersOnly := True;
  CDEdit1.HintText := 'Entrez votre âge';
end;

// Validation en temps réel
procedure CDEdit1Change(Sender: TObject);  
begin
  if not IsValidEmail(CDEdit1.Text) then
  begin
    CDEdit1.BorderColor := clRed;
    CDEdit1.BorderWidth := 2;
  end
  else
  begin
    CDEdit1.BorderColor := clGreen;
    CDEdit1.BorderWidth := 2;
  end;
end;

function IsValidEmail(const email: string): Boolean;  
begin
  Result := (Pos('@', email) > 0) and (Pos('.', email) > Pos('@', email));
end;
```

### Labels (TCustomDrawnLabel)

```pascal
type
  TFormMain = class(TForm)
    CDLabel1: TCustomDrawnLabel;
  end;

procedure ConfigurerLabels;  
begin
  // Configuration de base
  CDLabel1.Caption := 'Mon Application';
  CDLabel1.Font.Size := 24;
  CDLabel1.Font.Style := [fsBold];
  CDLabel1.Font.Color := clBlack;

  // Alignement
  CDLabel1.Alignment := taCenter;
  CDLabel1.Layout := tlCenter;

  // Arrière-plan
  CDLabel1.Transparent := False;
  CDLabel1.Color := clLightGray;

  // Bordure
  CDLabel1.BorderStyle := bsSingle;
  CDLabel1.BorderColor := clBlack;
end;

// Label avec texte multilignes
procedure ConfigurerLabelMultiligne;  
begin
  CDLabel1.WordWrap := True;
  CDLabel1.AutoSize := True;
  CDLabel1.Caption := 'Ceci est un texte long qui sera ' +
                      'automatiquement réparti sur plusieurs lignes.';
end;

// Label avec ombre portée
procedure AjouterOmbre;  
begin
  CDLabel1.ShowShadow := True;
  CDLabel1.ShadowColor := clGray;
  CDLabel1.ShadowOffsetX := 2;
  CDLabel1.ShadowOffsetY := 2;
end;
```

### Cases à cocher (TCustomDrawnCheckBox)

```pascal
type
  TFormMain = class(TForm)
    CDCheckBox1: TCustomDrawnCheckBox;
  end;

procedure ConfigurerCheckBox;  
begin
  CDCheckBox1.Caption := 'Accepter les conditions';
  CDCheckBox1.Checked := False;
  CDCheckBox1.Font.Size := 14;

  // Style de la case
  CDCheckBox1.CheckBoxSize := 24;
  CDCheckBox1.CheckColor := clBlue;
  CDCheckBox1.BoxColor := clWhite;
  CDCheckBox1.BorderColor := clGray;
end;

procedure CDCheckBox1Change(Sender: TObject);  
begin
  if CDCheckBox1.Checked then
    CDButton1.Enabled := True
  else
    CDButton1.Enabled := False;
end;

// CheckBox avec état intermédiaire
procedure ConfigurerTroisEtats;  
begin
  CDCheckBox1.AllowGrayed := True;
  CDCheckBox1.State := cbGrayed;  // cbUnchecked, cbChecked, cbGrayed
end;
```

### Boutons radio (TCustomDrawnRadioButton)

```pascal
type
  TFormMain = class(TForm)
    CDRadioGroup1: TCustomDrawnRadioGroup;
    CDRadio1, CDRadio2, CDRadio3: TCustomDrawnRadioButton;
  end;

procedure ConfigurerRadioButtons;  
begin
  // Option 1
  CDRadio1.Caption := 'Option 1';
  CDRadio1.Checked := True;
  CDRadio1.GroupIndex := 1;  // Même groupe = exclusif

  // Option 2
  CDRadio2.Caption := 'Option 2';
  CDRadio2.Checked := False;
  CDRadio2.GroupIndex := 1;

  // Option 3
  CDRadio3.Caption := 'Option 3';
  CDRadio3.Checked := False;
  CDRadio3.GroupIndex := 1;

  // Style
  CDRadio1.RadioSize := 20;
  CDRadio1.RadioColor := clBlue;
end;

procedure CDRadio1Click(Sender: TObject);  
begin
  case GetSelectedRadio of
    1: TraiterOption1;
    2: TraiterOption2;
    3: TraiterOption3;
  end;
end;

function GetSelectedRadio: Integer;  
begin
  if CDRadio1.Checked then Result := 1
  else if CDRadio2.Checked then Result := 2
  else if CDRadio3.Checked then Result := 3
  else Result := 0;
end;
```

### Listes (TCustomDrawnListBox)

```pascal
type
  TFormMain = class(TForm)
    CDListBox1: TCustomDrawnListBox;
  end;

procedure ConfigurerListBox;  
begin
  // Ajouter des items
  CDListBox1.Items.Add('Item 1');
  CDListBox1.Items.Add('Item 2');
  CDListBox1.Items.Add('Item 3');
  CDListBox1.Items.Add('Item 4');

  // Style
  CDListBox1.ItemHeight := 40;
  CDListBox1.Font.Size := 14;

  // Couleurs alternées
  CDListBox1.AlternateColor := clLightGray;
  CDListBox1.UseAlternateColor := True;

  // Sélection
  CDListBox1.ItemIndex := 0;  // Sélectionner le premier
end;

procedure CDListBox1Click(Sender: TObject);  
var
  selectedItem: string;
begin
  if CDListBox1.ItemIndex >= 0 then
  begin
    selectedItem := CDListBox1.Items[CDListBox1.ItemIndex];
    ShowMessage('Vous avez sélectionné : ' + selectedItem);
  end;
end;

// Liste avec images
procedure AjouterImagesListe;  
var
  imageList: TImageList;
begin
  imageList := TImageList.Create(Self);
  imageList.Width := 32;
  imageList.Height := 32;

  // Charger les images
  imageList.AddIcon(LoadIcon('icon1.ico'));
  imageList.AddIcon(LoadIcon('icon2.ico'));

  CDListBox1.Images := imageList;
  CDListBox1.ItemsWithImages := True;
end;
```

### Barres de progression (TCustomDrawnProgressBar)

```pascal
type
  TFormMain = class(TForm)
    CDProgressBar1: TCustomDrawnProgressBar;
    CDTimer1: TTimer;
  end;

procedure ConfigurerProgressBar;  
begin
  // Configuration
  CDProgressBar1.Min := 0;
  CDProgressBar1.Max := 100;
  CDProgressBar1.Position := 0;

  // Style
  CDProgressBar1.BarColor := clBlue;
  CDProgressBar1.BackgroundColor := clLightGray;
  CDProgressBar1.BorderStyle := bsSingle;

  // Hauteur
  CDProgressBar1.Height := 8;

  // Coins arrondis
  CDProgressBar1.Rounded := True;
  CDProgressBar1.RoundedRadius := 4;
end;

// Progression animée
procedure CDTimer1Timer(Sender: TObject);  
begin
  if CDProgressBar1.Position < CDProgressBar1.Max then
    CDProgressBar1.Position := CDProgressBar1.Position + 1
  else
  begin
    CDTimer1.Enabled := False;
    ShowMessage('Terminé !');
  end;
end;

procedure DemarrerProgression;  
begin
  CDProgressBar1.Position := 0;
  CDTimer1.Interval := 100;  // 100ms
  CDTimer1.Enabled := True;
end;

// Barre de progression circulaire
procedure ConfigurerProgressBarCirculaire;  
begin
  CDProgressBar1.Style := pbsCircular;
  CDProgressBar1.Width := 100;
  CDProgressBar1.Height := 100;
  CDProgressBar1.LineWidth := 8;
end;
```

### Curseurs (TCustomDrawnTrackBar)

```pascal
type
  TFormMain = class(TForm)
    CDTrackBar1: TCustomDrawnTrackBar;
  end;

procedure ConfigurerTrackBar;  
begin
  // Configuration
  CDTrackBar1.Min := 0;
  CDTrackBar1.Max := 100;
  CDTrackBar1.Position := 50;
  CDTrackBar1.Frequency := 10;  // Marques tous les 10

  // Style
  CDTrackBar1.ThumbColor := clBlue;
  CDTrackBar1.TrackColor := clLightGray;
  CDTrackBar1.ThumbSize := 20;

  // Orientation
  CDTrackBar1.Orientation := trHorizontal;  // ou trVertical
end;

procedure CDTrackBar1Change(Sender: TObject);  
begin
  CDLabel1.Caption := Format('Valeur: %d', [CDTrackBar1.Position]);
end;

// Curseur de volume avec icônes
procedure ConfigurerCurseurVolume;  
begin
  CDTrackBar1.Min := 0;
  CDTrackBar1.Max := 100;
  CDTrackBar1.ShowTicks := True;

  // Afficher une icône selon la valeur
  if CDTrackBar1.Position = 0 then
    CDImage1.LoadFromFile('mute.png')
  else if CDTrackBar1.Position < 50 then
    CDImage1.LoadFromFile('volume_low.png')
  else
    CDImage1.LoadFromFile('volume_high.png');
end;
```

## Création de composants personnalisés

### Composant de base personnalisé

```pascal
type
  TCustomCard = class(TCustomDrawnControl)
  private
    FTitle: string;
    FContent: string;
    FCardColor: TColor;
    procedure SetTitle(const Value: string);
    procedure SetContent(const Value: string);
  protected
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
                       X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Title: string read FTitle write SetTitle;
    property Content: string read FContent write SetContent;
    property CardColor: TColor read FCardColor write FCardColor;
  end;

constructor TCustomCard.Create(AOwner: TComponent);  
begin
  inherited Create(AOwner);

  // Valeurs par défaut
  Width := 200;
  Height := 150;
  FCardColor := clWhite;
  FTitle := 'Titre';
  FContent := 'Contenu';
end;

procedure TCustomCard.SetTitle(const Value: string);  
begin
  if FTitle <> Value then
  begin
    FTitle := Value;
    Invalidate;  // Redessiner
  end;
end;

procedure TCustomCard.SetContent(const Value: string);  
begin
  if FContent <> Value then
  begin
    FContent := Value;
    Invalidate;
  end;
end;

procedure TCustomCard.Paint;  
var
  rect: TRect;
begin
  inherited Paint;

  Canvas.Brush.Color := FCardColor;
  Canvas.Pen.Color := clGray;
  Canvas.Pen.Width := 1;

  // Dessiner le fond de la carte
  Canvas.RoundRect(0, 0, Width, Height, 10, 10);

  // Dessiner le titre
  Canvas.Font.Size := 16;
  Canvas.Font.Style := [fsBold];
  Canvas.Font.Color := clBlack;
  Canvas.TextOut(10, 10, FTitle);

  // Dessiner le contenu
  Canvas.Font.Size := 12;
  Canvas.Font.Style := [];
  Canvas.Font.Color := clDarkGray;

  rect := Rect(10, 40, Width - 10, Height - 10);
  Canvas.TextRect(rect, 10, 40, FContent);

  // Dessiner une ombre (simulation simple)
  Canvas.Pen.Color := clSilver;
  Canvas.MoveTo(5, Height + 2);
  Canvas.LineTo(Width - 5, Height + 2);
end;

procedure TCustomCard.MouseDown(Button: TMouseButton; Shift: TShiftState;
                                X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);

  if Button = mbLeft then
  begin
    // Animation de clic (changement de couleur temporaire)
    FCardColor := clLightGray;
    Invalidate;

    // Déclencher l'événement OnClick
    if Assigned(OnClick) then
      OnClick(Self);

    // Restaurer la couleur après un délai
    Application.ProcessMessages;
    Sleep(100);
    FCardColor := clWhite;
    Invalidate;
  end;
end;
```

### Utilisation du composant personnalisé

```pascal
type
  TFormMain = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    FCard1, FCard2: TCustomCard;
  end;

procedure TFormMain.FormCreate(Sender: TObject);  
begin
  // Créer la première carte
  FCard1 := TCustomCard.Create(Self);
  FCard1.Parent := Self;
  FCard1.Left := 20;
  FCard1.Top := 20;
  FCard1.Title := 'Carte 1';
  FCard1.Content := 'Ceci est le contenu de la première carte.';
  FCard1.OnClick := @Card1Click;

  // Créer la deuxième carte
  FCard2 := TCustomCard.Create(Self);
  FCard2.Parent := Self;
  FCard2.Left := 240;
  FCard2.Top := 20;
  FCard2.Title := 'Carte 2';
  FCard2.Content := 'Contenu de la seconde carte.';
  FCard2.CardColor := clMoneyGreen;
  FCard2.OnClick := @Card2Click;
end;

procedure TFormMain.Card1Click(Sender: TObject);  
begin
  ShowMessage('Carte 1 cliquée !');
end;

procedure TFormMain.Card2Click(Sender: TObject);  
begin
  ShowMessage('Carte 2 cliquée !');
end;
```

## Styles et thèmes

### Système de thèmes

Un système de thèmes permet de définir l'apparence globale de votre application et de basculer facilement entre différents styles (clair, sombre, personnalisé).

```pascal
type
  TThemeStyle = (tsLight, tsDark, tsCustom);

  TAppTheme = class
  private
    FCurrentStyle: TThemeStyle;
    // Couleurs
    FPrimaryColor: TColor;
    FSecondaryColor: TColor;
    FBackgroundColor: TColor;
    FTextColor: TColor;
    FBorderColor: TColor;
    // Dimensions
    FButtonHeight: Integer;
    FBorderRadius: Integer;
    FFontSize: Integer;
  public
    constructor Create;
    procedure ApplyTheme(AStyle: TThemeStyle);
    procedure ApplyToControl(AControl: TControl);

    property CurrentStyle: TThemeStyle read FCurrentStyle;
    property PrimaryColor: TColor read FPrimaryColor;
    property SecondaryColor: TColor read FSecondaryColor;
    property BackgroundColor: TColor read FBackgroundColor;
    property TextColor: TColor read FTextColor;
    property BorderColor: TColor read FBorderColor;
    property ButtonHeight: Integer read FButtonHeight;
    property BorderRadius: Integer read FBorderRadius;
    property FontSize: Integer read FFontSize;
  end;

constructor TAppTheme.Create;  
begin
  inherited Create;
  // Thème clair par défaut
  ApplyTheme(tsLight);
end;

procedure TAppTheme.ApplyTheme(AStyle: TThemeStyle);  
begin
  FCurrentStyle := AStyle;

  case AStyle of
    tsLight:
    begin
      FPrimaryColor := $FF2196F3;      // Bleu Material
      FSecondaryColor := $FFFF4081;    // Rose
      FBackgroundColor := clWhite;
      FTextColor := $FF212121;         // Noir presque pur
      FBorderColor := $FFE0E0E0;       // Gris clair
    end;

    tsDark:
    begin
      FPrimaryColor := $FF1976D2;      // Bleu foncé
      FSecondaryColor := $FFF50057;    // Rose foncé
      FBackgroundColor := $FF121212;   // Noir Material
      FTextColor := $FFFFFFFF;         // Blanc
      FBorderColor := $FF424242;       // Gris foncé
    end;

    tsCustom:
    begin
      // Charger depuis les préférences ou fichier de config
      LoadCustomTheme;
    end;
  end;

  // Dimensions communes
  FButtonHeight := 48;
  FBorderRadius := 8;
  FFontSize := 14;
end;

procedure TAppTheme.ApplyToControl(AControl: TControl);  
begin
  if AControl is TCustomDrawnButton then
  begin
    with TCustomDrawnButton(AControl) do
    begin
      Color := FPrimaryColor;
      Font.Color := clWhite;
      Font.Size := FFontSize;
      Height := FButtonHeight;
      Rounded := True;
      RoundedRadius := FBorderRadius;
    end;
  end
  else if AControl is TCustomDrawnEdit then
  begin
    with TCustomDrawnEdit(AControl) do
    begin
      Font.Color := FTextColor;
      Font.Size := FFontSize;
      BorderColor := FBorderColor;
      Color := FBackgroundColor;
      Rounded := True;
      RoundedRadius := FBorderRadius;
    end;
  end
  else if AControl is TCustomDrawnLabel then
  begin
    with TCustomDrawnLabel(AControl) do
    begin
      Font.Color := FTextColor;
      Font.Size := FFontSize;
      Color := FBackgroundColor;
    end;
  end;
end;

// Utilisation globale
var
  AppTheme: TAppTheme;

procedure InitializeTheme;  
begin
  AppTheme := TAppTheme.Create;
  AppTheme.ApplyTheme(tsLight);
end;

procedure ApplyThemeToForm(AForm: TForm);  
var
  i: Integer;
begin
  AForm.Color := AppTheme.BackgroundColor;

  for i := 0 to AForm.ComponentCount - 1 do
  begin
    if AForm.Components[i] is TControl then
      AppTheme.ApplyToControl(TControl(AForm.Components[i]));
  end;
end;

procedure ToggleTheme;  
begin
  if AppTheme.CurrentStyle = tsLight then
    AppTheme.ApplyTheme(tsDark)
  else
    AppTheme.ApplyTheme(tsLight);

  // Réappliquer à tous les formulaires ouverts
  ApplyThemeToForm(FormMain);
end;
```

### Thème avec fichier de configuration

```pascal
type
  TThemeConfig = record
    Name: string;
    PrimaryColor: TColor;
    SecondaryColor: TColor;
    BackgroundColor: TColor;
    TextColor: TColor;
    // ... autres propriétés
  end;

procedure SaveThemeToFile(const ATheme: TThemeConfig; const AFileName: string);  
var
  json: TJSONObject;
  fs: TFileStream;
begin
  json := TJSONObject.Create;
  try
    json.Add('name', ATheme.Name);
    json.Add('primary_color', ColorToHex(ATheme.PrimaryColor));
    json.Add('secondary_color', ColorToHex(ATheme.SecondaryColor));
    json.Add('background_color', ColorToHex(ATheme.BackgroundColor));
    json.Add('text_color', ColorToHex(ATheme.TextColor));

    fs := TFileStream.Create(AFileName, fmCreate);
    try
      WriteJSONToStream(json, fs);
    finally
      fs.Free;
    end;
  finally
    json.Free;
  end;
end;

function LoadThemeFromFile(const AFileName: string): TThemeConfig;  
var
  json: TJSONObject;
  fs: TFileStream;
begin
  fs := TFileStream.Create(AFileName, fmOpenRead);
  try
    json := TJSONObject(GetJSON(fs));
    try
      Result.Name := json.Get('name', 'Custom');
      Result.PrimaryColor := HexToColor(json.Get('primary_color', '#2196F3'));
      Result.SecondaryColor := HexToColor(json.Get('secondary_color', '#FF4081'));
      Result.BackgroundColor := HexToColor(json.Get('background_color', '#FFFFFF'));
      Result.TextColor := HexToColor(json.Get('text_color', '#212121'));
    finally
      json.Free;
    end;
  finally
    fs.Free;
  end;
end;

function ColorToHex(AColor: TColor): string;  
begin
  Result := '#' + IntToHex(Red(AColor), 2) +
                  IntToHex(Green(AColor), 2) +
                  IntToHex(Blue(AColor), 2);
end;

function HexToColor(const AHex: string): TColor;  
var
  hex: string;
begin
  hex := AHex;
  if hex[1] = '#' then
    Delete(hex, 1, 1);

  Result := RGB(
    StrToInt('$' + Copy(hex, 1, 2)),
    StrToInt('$' + Copy(hex, 3, 2)),
    StrToInt('$' + Copy(hex, 5, 2))
  );
end;
```

## Gestion tactile avancée

### Événements tactiles de base

```pascal
type
  TFormMain = class(TForm)
  private
    FTouchStartX, FTouchStartY: Integer;
    FTouchStartTime: TDateTime;
    FIsSwiping: Boolean;
  protected
    procedure TouchDown(X, Y: Integer); virtual;
    procedure TouchMove(X, Y: Integer); virtual;
    procedure TouchUp(X, Y: Integer); virtual;
  end;

procedure TFormMain.TouchDown(X, Y: Integer);  
begin
  FTouchStartX := X;
  FTouchStartY := Y;
  FTouchStartTime := Now;
  FIsSwiping := False;
end;

procedure TFormMain.TouchMove(X, Y: Integer);  
var
  deltaX, deltaY: Integer;
begin
  deltaX := X - FTouchStartX;
  deltaY := Y - FTouchStartY;

  // Détecter un glissement (swipe)
  if (Abs(deltaX) > 50) or (Abs(deltaY) > 50) then
  begin
    FIsSwiping := True;

    // Déterminer la direction
    if Abs(deltaX) > Abs(deltaY) then
    begin
      // Glissement horizontal
      if deltaX > 0 then
        OnSwipeRight
      else
        OnSwipeLeft;
    end
    else
    begin
      // Glissement vertical
      if deltaY > 0 then
        OnSwipeDown
      else
        OnSwipeUp;
    end;
  end;
end;

procedure TFormMain.TouchUp(X, Y: Integer);  
var
  duration: Double;
  deltaX, deltaY: Integer;
begin
  duration := (Now - FTouchStartTime) * 24 * 3600;  // en secondes
  deltaX := Abs(X - FTouchStartX);
  deltaY := Abs(Y - FTouchStartY);

  // Détection d'un tap (simple clic)
  if (not FIsSwiping) and (duration < 0.3) and
     (deltaX < 10) and (deltaY < 10) then
  begin
    OnTap(FTouchStartX, FTouchStartY);
  end

  // Détection d'un long press
  else if (not FIsSwiping) and (duration >= 0.5) and
          (deltaX < 10) and (deltaY < 10) then
  begin
    OnLongPress(FTouchStartX, FTouchStartY);
  end;
end;

// Gestionnaires d'événements
procedure TFormMain.OnSwipeLeft;  
begin
  // Navigation vers la page suivante
  ShowNextPage;
end;

procedure TFormMain.OnSwipeRight;  
begin
  // Navigation vers la page précédente
  ShowPreviousPage;
end;

procedure TFormMain.OnSwipeUp;  
begin
  // Défiler vers le haut
  ScrollUp;
end;

procedure TFormMain.OnSwipeDown;  
begin
  // Actualiser (pull to refresh)
  RefreshContent;
end;

procedure TFormMain.OnTap(X, Y: Integer);  
begin
  // Gérer le clic simple
  HandleClick(X, Y);
end;

procedure TFormMain.OnLongPress(X, Y: Integer);  
begin
  // Afficher un menu contextuel
  ShowContextMenu(X, Y);
end;
```

### Geste multi-touch (pinch to zoom)

```pascal
type
  TTouchPoint = record
    X, Y: Integer;
    ID: Integer;
  end;

  TFormMain = class(TForm)
  private
    FTouchPoints: array[0..9] of TTouchPoint;
    FTouchCount: Integer;
    FInitialDistance: Single;
    FCurrentScale: Single;
  protected
    procedure MultiTouchDown(const Points: array of TTouchPoint);
    procedure MultiTouchMove(const Points: array of TTouchPoint);
    procedure MultiTouchUp;
  end;

procedure TFormMain.MultiTouchDown(const Points: array of TTouchPoint);  
var
  i: Integer;
begin
  FTouchCount := Length(Points);

  for i := 0 to FTouchCount - 1 do
    FTouchPoints[i] := Points[i];

  // Calculer la distance initiale pour pinch-to-zoom
  if FTouchCount = 2 then
  begin
    FInitialDistance := CalculateDistance(FTouchPoints[0], FTouchPoints[1]);
  end;
end;

procedure TFormMain.MultiTouchMove(const Points: array of TTouchPoint);  
var
  currentDistance, scale: Single;
begin
  if FTouchCount = 2 then
  begin
    currentDistance := CalculateDistance(Points[0], Points[1]);
    scale := currentDistance / FInitialDistance;

    // Appliquer le zoom
    ApplyZoom(scale);
    FCurrentScale := scale;
  end;
end;

function CalculateDistance(const P1, P2: TTouchPoint): Single;  
begin
  Result := Sqrt(Sqr(P2.X - P1.X) + Sqr(P2.Y - P1.Y));
end;

procedure ApplyZoom(scale: Single);  
begin
  // Limiter le zoom
  if scale < 0.5 then scale := 0.5;
  if scale > 3.0 then scale := 3.0;

  // Appliquer à l'image ou au contenu
  CDImage1.Scale := scale;
  CDImage1.Invalidate;
end;
```

### Feedback tactile

```pascal
procedure ProvideTouchFeedback(const FeedbackType: TTouchFeedbackType);  
begin
  case FeedbackType of
    tfClick:
    begin
      // Vibration courte
      {$IFDEF ANDROID}
      Vibrate(50);  // 50ms
      {$ENDIF}

      // Effet visuel
      ShowRippleEffect;
    end;

    tfLongPress:
    begin
      // Vibration moyenne
      {$IFDEF ANDROID}
      Vibrate(100);
      {$ENDIF}
    end;

    tfError:
    begin
      // Vibration pattern
      {$IFDEF ANDROID}
      VibratePattern([0, 100, 50, 100]);
      {$ENDIF}
    end;
  end;
end;

procedure ShowRippleEffect;  
var
  ripple: TRippleAnimation;
begin
  ripple := TRippleAnimation.Create(Self);
  ripple.Center := Point(FTouchStartX, FTouchStartY);
  ripple.Duration := 300;  // ms
  ripple.Color := clLightGray;
  ripple.Start;
end;
```

## Animations

### Animation de base

```pascal
type
  TAnimationType = (atFadeIn, atFadeOut, atSlideIn, atSlideOut, atScale);

  TCustomAnimation = class
  private
    FControl: TControl;
    FAnimationType: TAnimationType;
    FDuration: Integer;
    FStartTime: TDateTime;
    FStartValue, FEndValue: Single;
    FTimer: TTimer;
    procedure OnTimerTick(Sender: TObject);
  public
    constructor Create(AControl: TControl; AType: TAnimationType;
                      ADuration: Integer);
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
  end;

constructor TCustomAnimation.Create(AControl: TControl;
                                    AType: TAnimationType;
                                    ADuration: Integer);
begin
  inherited Create;
  FControl := AControl;
  FAnimationType := AType;
  FDuration := ADuration;

  FTimer := TTimer.Create(nil);
  FTimer.Interval := 16;  // ~60 FPS
  FTimer.OnTimer := @OnTimerTick;
  FTimer.Enabled := False;
end;

destructor TCustomAnimation.Destroy;  
begin
  FTimer.Free;
  inherited Destroy;
end;

procedure TCustomAnimation.Start;  
begin
  FStartTime := Now;

  case FAnimationType of
    atFadeIn:
    begin
      FStartValue := 0;
      FEndValue := 1;
      FControl.Visible := True;
    end;

    atFadeOut:
    begin
      FStartValue := 1;
      FEndValue := 0;
    end;

    atSlideIn:
    begin
      FStartValue := -FControl.Width;
      FEndValue := FControl.Left;
    end;

    atScale:
    begin
      FStartValue := 0.5;
      FEndValue := 1.0;
    end;
  end;

  FTimer.Enabled := True;
end;

procedure TCustomAnimation.OnTimerTick(Sender: TObject);  
var
  elapsed, progress: Double;
  currentValue: Single;
begin
  elapsed := (Now - FStartTime) * 24 * 3600 * 1000;  // en ms
  progress := elapsed / FDuration;

  if progress >= 1.0 then
  begin
    progress := 1.0;
    FTimer.Enabled := False;

    if FAnimationType = atFadeOut then
      FControl.Visible := False;
  end;

  // Interpolation (ease-out)
  progress := 1 - Sqr(1 - progress);

  currentValue := FStartValue + (FEndValue - FStartValue) * progress;

  case FAnimationType of
    atFadeIn, atFadeOut:
      FControl.Alpha := Round(currentValue * 255);

    atSlideIn:
      FControl.Left := Round(currentValue);

    atScale:
    begin
      FControl.Width := Round(FControl.Width * currentValue);
      FControl.Height := Round(FControl.Height * currentValue);
    end;
  end;

  FControl.Invalidate;
end;

procedure TCustomAnimation.Stop;  
begin
  FTimer.Enabled := False;
end;

// Utilisation
procedure AnimerAffichage;  
var
  anim: TCustomAnimation;
begin
  anim := TCustomAnimation.Create(CDPanel1, atFadeIn, 500);
  try
    anim.Start;
  finally
    // Ne pas libérer immédiatement, attendre la fin de l'animation
  end;
end;
```

### Transitions entre pages

```pascal
type
  TPageTransition = (ptSlideLeft, ptSlideRight, ptFade, ptZoom);

  TPageManager = class
  private
    FCurrentPage: TCustomDrawnPanel;
    FPages: TList;
    procedure TransitionTo(APage: TCustomDrawnPanel;
                          ATransition: TPageTransition);
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddPage(APage: TCustomDrawnPanel);
    procedure ShowPage(AIndex: Integer; ATransition: TPageTransition);
  end;

constructor TPageManager.Create;  
begin
  inherited Create;
  FPages := TList.Create;
end;

destructor TPageManager.Destroy;  
begin
  FPages.Free;
  inherited Destroy;
end;

procedure TPageManager.AddPage(APage: TCustomDrawnPanel);  
begin
  FPages.Add(APage);
  APage.Visible := FPages.Count = 1;  // Première page visible

  if FPages.Count = 1 then
    FCurrentPage := APage;
end;

procedure TPageManager.ShowPage(AIndex: Integer; ATransition: TPageTransition);  
var
  newPage: TCustomDrawnPanel;
begin
  if (AIndex < 0) or (AIndex >= FPages.Count) then
    Exit;

  newPage := TCustomDrawnPanel(FPages[AIndex]);

  if newPage = FCurrentPage then
    Exit;

  TransitionTo(newPage, ATransition);
end;

procedure TPageManager.TransitionTo(APage: TCustomDrawnPanel;
                                    ATransition: TPageTransition);
var
  oldPage: TCustomDrawnPanel;
begin
  oldPage := FCurrentPage;

  case ATransition of
    ptSlideLeft:
    begin
      // Positionner la nouvelle page à droite
      APage.Left := oldPage.Width;
      APage.Visible := True;

      // Animer les deux pages
      AnimateSlide(oldPage, -oldPage.Width, 300);
      AnimateSlide(APage, 0, 300);
    end;

    ptSlideRight:
    begin
      APage.Left := -oldPage.Width;
      APage.Visible := True;

      AnimateSlide(oldPage, oldPage.Width, 300);
      AnimateSlide(APage, 0, 300);
    end;

    ptFade:
    begin
      APage.Alpha := 0;
      APage.Visible := True;

      AnimateFade(oldPage, 0, 300);
      AnimateFade(APage, 255, 300);
    end;

    ptZoom:
    begin
      APage.Visible := True;
      AnimateZoom(APage, 0.0, 1.0, 300);
    end;
  end;

  FCurrentPage := APage;

  // Masquer l'ancienne page après la transition
  TThread.CreateAnonymousThread(
    procedure
    begin
      Sleep(300);
      TThread.Synchronize(nil,
        procedure
        begin
          oldPage.Visible := False;
        end);
    end
  ).Start;
end;

procedure AnimateSlide(AControl: TControl; ATargetLeft: Integer;
                      ADuration: Integer);
var
  anim: TCustomAnimation;
begin
  // Implémentation de l'animation de glissement
  // ...
end;

procedure AnimateFade(AControl: TControl; ATargetAlpha: Byte;
                     ADuration: Integer);
var
  anim: TCustomAnimation;
begin
  // Implémentation de l'animation de fondu
  // ...
end;
```

## Layouts responsive

### Adaptation à la taille d'écran

```pascal
type
  TScreenSize = (ssSmall, ssMedium, ssLarge, ssXLarge);
  TOrientation = (orPortrait, orLandscape);

  TResponsiveLayout = class
  private
    FForm: TForm;
    FCurrentScreenSize: TScreenSize;
    FCurrentOrientation: TOrientation;
    procedure DetectScreenSize;
    procedure DetectOrientation;
  public
    constructor Create(AForm: TForm);
    procedure ApplyLayout;
    procedure OnResize;
  end;

constructor TResponsiveLayout.Create(AForm: TForm);  
begin
  inherited Create;
  FForm := AForm;
  DetectScreenSize;
  DetectOrientation;
end;

procedure TResponsiveLayout.DetectScreenSize;  
var
  screenWidth: Integer;
begin
  screenWidth := Screen.Width;

  if screenWidth < 360 then
    FCurrentScreenSize := ssSmall
  else if screenWidth < 600 then
    FCurrentScreenSize := ssMedium
  else if screenWidth < 960 then
    FCurrentScreenSize := ssLarge
  else
    FCurrentScreenSize := ssXLarge;
end;

procedure TResponsiveLayout.DetectOrientation;  
begin
  if Screen.Width > Screen.Height then
    FCurrentOrientation := orLandscape
  else
    FCurrentOrientation := orPortrait;
end;

procedure TResponsiveLayout.ApplyLayout;  
var
  i: Integer;
  ctrl: TControl;
  margin, fontSize: Integer;
begin
  // Définir les marges et tailles selon l'écran
  case FCurrentScreenSize of
    ssSmall:
    begin
      margin := 8;
      fontSize := 12;
    end;
    ssMedium:
    begin
      margin := 12;
      fontSize := 14;
    end;
    ssLarge:
    begin
      margin := 16;
      fontSize := 16;
    end;
    ssXLarge:
    begin
      margin := 24;
      fontSize := 18;
    end;
  end;

  // Appliquer aux composants
  for i := 0 to FForm.ComponentCount - 1 do
  begin
    if FForm.Components[i] is TCustomDrawnControl then
    begin
      ctrl := TCustomDrawnControl(FForm.Components[i]);

      // Ajuster les marges
      ctrl.BorderSpacing.Around := margin;

      // Ajuster la police
      if ctrl is TCustomDrawnButton then
        TCustomDrawnButton(ctrl).Font.Size := fontSize
      else if ctrl is TCustomDrawnLabel then
        TCustomDrawnLabel(ctrl).Font.Size := fontSize;
    end;
  end;

  // Layout spécifique selon l'orientation
  if FCurrentOrientation = orPortrait then
    ApplyPortraitLayout
  else
    ApplyLandscapeLayout;
end;

procedure TResponsiveLayout.ApplyPortraitLayout;  
begin
  // Disposer les éléments verticalement
  // Une colonne
  // ...
end;

procedure TResponsiveLayout.ApplyLandscapeLayout;  
begin
  // Disposer les éléments horizontalement
  // Deux colonnes
  // ...
end;

procedure TResponsiveLayout.OnResize;  
var
  oldSize: TScreenSize;
  oldOrientation: TOrientation;
begin
  oldSize := FCurrentScreenSize;
  oldOrientation := FCurrentOrientation;

  DetectScreenSize;
  DetectOrientation;

  // Réappliquer le layout si changement
  if (oldSize <> FCurrentScreenSize) or
     (oldOrientation <> FCurrentOrientation) then
  begin
    ApplyLayout;
  end;
end;

// Utilisation
var
  ResponsiveLayout: TResponsiveLayout;

procedure TFormMain.FormCreate(Sender: TObject);  
begin
  ResponsiveLayout := TResponsiveLayout.Create(Self);
  ResponsiveLayout.ApplyLayout;
end;

procedure TFormMain.FormResize(Sender: TObject);  
begin
  ResponsiveLayout.OnResize;
end;
```

### Grille responsive

```pascal
type
  TResponsiveGrid = class(TCustomDrawnPanel)
  private
    FColumns: Integer;
    FSpacing: Integer;
    FItems: TList;
    procedure CalculateColumns;
    procedure ArrangeItems;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddItem(AItem: TControl);
    procedure Refresh;
    property Columns: Integer read FColumns write FColumns;
    property Spacing: Integer read FSpacing write FSpacing;
  end;

constructor TResponsiveGrid.Create(AOwner: TComponent);  
begin
  inherited Create(AOwner);
  FItems := TList.Create;
  FSpacing := 8;
  CalculateColumns;
end;

destructor TResponsiveGrid.Destroy;  
begin
  FItems.Free;
  inherited Destroy;
end;

procedure TResponsiveGrid.CalculateColumns;  
var
  availableWidth: Integer;
  minItemWidth: Integer;
begin
  availableWidth := Width;
  minItemWidth := 150;  // Largeur minimale d'un item

  FColumns := availableWidth div (minItemWidth + FSpacing);
  if FColumns < 1 then
    FColumns := 1;
end;

procedure TResponsiveGrid.AddItem(AItem: TControl);  
begin
  AItem.Parent := Self;
  FItems.Add(AItem);
  ArrangeItems;
end;

procedure TResponsiveGrid.ArrangeItems;  
var
  i, row, col: Integer;
  itemWidth, itemHeight: Integer;
  x, y: Integer;
  ctrl: TControl;
begin
  if FItems.Count = 0 then
    Exit;

  CalculateColumns;

  itemWidth := (Width - (FColumns + 1) * FSpacing) div FColumns;
  itemHeight := itemWidth;  // Items carrés

  for i := 0 to FItems.Count - 1 do
  begin
    row := i div FColumns;
    col := i mod FColumns;

    x := FSpacing + col * (itemWidth + FSpacing);
    y := FSpacing + row * (itemHeight + FSpacing);

    ctrl := TControl(FItems[i]);
    ctrl.SetBounds(x, y, itemWidth, itemHeight);
  end;

  // Ajuster la hauteur du panel
  Height := FSpacing + ((FItems.Count div FColumns) + 1) *
            (itemHeight + FSpacing);
end;

procedure TResponsiveGrid.Refresh;  
begin
  ArrangeItems;
  Invalidate;
end;

// Utilisation
procedure CreerGrillePhotos;  
var
  grid: TResponsiveGrid;
  i: Integer;
  photo: TCustomDrawnImage;
begin
  grid := TResponsiveGrid.Create(Self);
  grid.Parent := ScrollBox1;
  grid.Align := alTop;
  grid.Spacing := 12;

  for i := 1 to 20 do
  begin
    photo := TCustomDrawnImage.Create(Self);
    photo.LoadFromFile(Format('photo%d.jpg', [i]));
    photo.ScaleMode := smStretch;
    grid.AddItem(photo);
  end;
end;
```

## Performance et optimisation

### Dessin optimisé

```pascal
type
  TOptimizedDrawing = class
  private
    FBuffer: TBitmap;
    FDirty: Boolean;
  public
    constructor Create(AWidth, AHeight: Integer);
    destructor Destroy; override;
    procedure MarkDirty;
    procedure Draw(ACanvas: TCanvas);
  end;

constructor TOptimizedDrawing.Create(AWidth, AHeight: Integer);  
begin
  inherited Create;
  FBuffer := TBitmap.Create;
  FBuffer.SetSize(AWidth, AHeight);
  FDirty := True;
end;

destructor TOptimizedDrawing.Destroy;  
begin
  FBuffer.Free;
  inherited Destroy;
end;

procedure TOptimizedDrawing.MarkDirty;  
begin
  FDirty := True;
end;

procedure TOptimizedDrawing.Draw(ACanvas: TCanvas);  
begin
  if FDirty then
  begin
    // Redessiner dans le buffer
    DrawToBuffer(FBuffer.Canvas);
    FDirty := False;
  end;

  // Copier le buffer vers le canvas final
  ACanvas.Draw(0, 0, FBuffer);
end;

// Dans un composant
type
  TOptimizedPanel = class(TCustomDrawnPanel)
  private
    FDrawingBuffer: TOptimizedDrawing;
  protected
    procedure Paint; override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure InvalidateContent;
  end;

constructor TOptimizedPanel.Create(AOwner: TComponent);  
begin
  inherited Create(AOwner);
  FDrawingBuffer := TOptimizedDrawing.Create(Width, Height);
  DoubleBuffered := True;  // Double buffering système
end;

destructor TOptimizedPanel.Destroy;  
begin
  FDrawingBuffer.Free;
  inherited Destroy;
end;

procedure TOptimizedPanel.Paint;  
begin
  inherited Paint;
  FDrawingBuffer.Draw(Canvas);
end;

procedure TOptimizedPanel.Resize;  
begin
  inherited Resize;
  FDrawingBuffer.Free;
  FDrawingBuffer := TOptimizedDrawing.Create(Width, Height);
  FDrawingBuffer.MarkDirty;
end;

procedure TOptimizedPanel.InvalidateContent;  
begin
  FDrawingBuffer.MarkDirty;
  Invalidate;
end;
```

### Limitation du taux de rafraîchissement

```pascal
type
  TFrameRateLimiter = class
  private
    FTargetFPS: Integer;
    FLastFrameTime: TDateTime;
    FFrameInterval: Double;
  public
    constructor Create(AFPS: Integer);
    function ShouldRender: Boolean;
  end;

constructor TFrameRateLimiter.Create(AFPS: Integer);  
begin
  inherited Create;
  FTargetFPS := AFPS;
  FFrameInterval := 1.0 / AFPS;
  FLastFrameTime := Now;
end;

function TFrameRateLimiter.ShouldRender: Boolean;  
var
  currentTime: TDateTime;
  elapsed: Double;
begin
  currentTime := Now;
  elapsed := (currentTime - FLastFrameTime) * 24 * 3600;  // en secondes

  Result := elapsed >= FFrameInterval;

  if Result then
    FLastFrameTime := currentTime;
end;

// Utilisation dans une animation
type
  TAnimatedPanel = class(TCustomDrawnPanel)
  private
    FTimer: TTimer;
    FFrameLimiter: TFrameRateLimiter;
    FAngle: Single;
  protected
    procedure OnTimerTick(Sender: TObject);
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

constructor TAnimatedPanel.Create(AOwner: TComponent);  
begin
  inherited Create(AOwner);

  FFrameLimiter := TFrameRateLimiter.Create(60);  // 60 FPS max
  FAngle := 0;

  FTimer := TTimer.Create(Self);
  FTimer.Interval := 1;  // Vérifier très fréquemment
  FTimer.OnTimer := @OnTimerTick;
  FTimer.Enabled := True;
end;

destructor TAnimatedPanel.Destroy;  
begin
  FFrameLimiter.Free;
  inherited Destroy;
end;

procedure TAnimatedPanel.OnTimerTick(Sender: TObject);  
begin
  if FFrameLimiter.ShouldRender then
  begin
    FAngle := FAngle + 2;
    if FAngle >= 360 then
      FAngle := 0;

    Invalidate;  // Redessiner seulement si le frame limiter le permet
  end;
end;

procedure TAnimatedPanel.Paint;  
var
  centerX, centerY: Integer;
  radius: Integer;
begin
  inherited Paint;

  centerX := Width div 2;
  centerY := Height div 2;
  radius := Min(Width, Height) div 3;

  // Dessiner un cercle qui tourne
  Canvas.Brush.Color := clBlue;
  Canvas.Ellipse(
    centerX - radius + Round(radius * Cos(FAngle * Pi / 180)),
    centerY - radius + Round(radius * Sin(FAngle * Pi / 180)),
    centerX + radius + Round(radius * Cos(FAngle * Pi / 180)),
    centerY + radius + Round(radius * Sin(FAngle * Pi / 180))
  );
end;
```

### Recyclage de vues (ViewHolder pattern)

```pascal
type
  TViewHolder = class
    TextLabel: TCustomDrawnLabel;
    IconImage: TCustomDrawnImage;
    ActionButton: TCustomDrawnButton;
  end;

  TOptimizedListView = class(TCustomDrawnControl)
  private
    FItems: TStringList;
    FViewHolders: TList;
    FVisibleStartIndex: Integer;
    FVisibleEndIndex: Integer;
    procedure RecycleViews;
    function GetOrCreateViewHolder: TViewHolder;
  protected
    procedure Paint; override;
    procedure Scroll(Delta: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetItems(AItems: TStringList);
  end;

constructor TOptimizedListView.Create(AOwner: TComponent);  
begin
  inherited Create(AOwner);
  FItems := TStringList.Create;
  FViewHolders := TList.Create;
  FVisibleStartIndex := 0;
end;

destructor TOptimizedListView.Destroy;  
var
  i: Integer;
begin
  for i := 0 to FViewHolders.Count - 1 do
    TViewHolder(FViewHolders[i]).Free;
  FViewHolders.Free;
  FItems.Free;
  inherited Destroy;
end;

function TOptimizedListView.GetOrCreateViewHolder: TViewHolder;  
var
  holder: TViewHolder;
begin
  // Réutiliser un holder existant si possible
  if FViewHolders.Count > 0 then
  begin
    Result := TViewHolder(FViewHolders[0]);
    FViewHolders.Delete(0);
  end
  else
  begin
    // Créer un nouveau holder
    holder := TViewHolder.Create;
    holder.TextLabel := TCustomDrawnLabel.Create(Self);
    holder.TextLabel.Parent := Self;
    holder.IconImage := TCustomDrawnImage.Create(Self);
    holder.IconImage.Parent := Self;
    holder.ActionButton := TCustomDrawnButton.Create(Self);
    holder.ActionButton.Parent := Self;
    Result := holder;
  end;
end;

procedure TOptimizedListView.RecycleViews;  
var
  itemHeight: Integer;
  visibleCount: Integer;
  i: Integer;
  holder: TViewHolder;
  y: Integer;
begin
  itemHeight := 60;
  visibleCount := (Height div itemHeight) + 2;  // +2 pour le défilement

  FVisibleStartIndex := Max(0, FVisibleStartIndex);
  FVisibleEndIndex := Min(FItems.Count - 1,
                          FVisibleStartIndex + visibleCount - 1);

  // Afficher seulement les items visibles
  y := 0;
  for i := FVisibleStartIndex to FVisibleEndIndex do
  begin
    holder := GetOrCreateViewHolder;

    // Configurer le holder avec les données
    holder.TextLabel.Caption := FItems[i];
    holder.TextLabel.Top := y;
    holder.TextLabel.Left := 50;
    holder.TextLabel.Visible := True;

    holder.IconImage.Top := y + 5;
    holder.IconImage.Left := 5;
    holder.IconImage.Visible := True;

    Inc(y, itemHeight);
  end;
end;

procedure TOptimizedListView.Paint;  
begin
  inherited Paint;
  RecycleViews;
end;

procedure TOptimizedListView.Scroll(Delta: Integer);  
begin
  FVisibleStartIndex := FVisibleStartIndex + Delta;
  if FVisibleStartIndex < 0 then
    FVisibleStartIndex := 0;
  if FVisibleStartIndex > FItems.Count - 1 then
    FVisibleStartIndex := FItems.Count - 1;

  Invalidate;
end;
```

## Déploiement multi-plateforme

### Compilation conditionnelle

```pascal
unit PlatformSpecific;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils
  {$IFDEF ANDROID}
  , jni, android
  {$ENDIF}
  {$IFDEF IOS}
  , iPhoneAll
  {$ENDIF}
  {$IFDEF WINDOWS}
  , Windows
  {$ENDIF}
  {$IFDEF UNIX}
  , BaseUnix
  {$ENDIF}
  ;

type
  TPlatformHelper = class
  public
    class function GetPlatformName: string;
    class function GetScreenDPI: Integer;
    class procedure Vibrate(Duration: Integer);
    class procedure ShowNotification(const Title, Message: string);
    class function GetDeviceInfo: string;
  end;

implementation

class function TPlatformHelper.GetPlatformName: string;  
begin
  {$IFDEF ANDROID}
  Result := 'Android';
  {$ENDIF}
  {$IFDEF IOS}
  Result := 'iOS';
  {$ENDIF}
  {$IFDEF WINDOWS}
  Result := 'Windows';
  {$ENDIF}
  {$IFDEF LINUX}
  Result := 'Linux';
  {$ENDIF}
  {$IFDEF DARWIN}
  Result := 'macOS';
  {$ENDIF}
end;

class function TPlatformHelper.GetScreenDPI: Integer;  
begin
  {$IFDEF ANDROID}
  Result := GetAndroidDPI;
  {$ENDIF}
  {$IFDEF IOS}
  Result := GetiOSScale * 160;
  {$ENDIF}
  {$IFDEF WINDOWS}
  Result := GetDeviceCaps(GetDC(0), LOGPIXELSX);
  {$ENDIF}
  {$IFDEF LINUX}
  Result := 96;  // DPI par défaut
  {$ENDIF}
end;

class procedure TPlatformHelper.Vibrate(Duration: Integer);  
begin
  {$IFDEF ANDROID}
  AndroidVibrate(Duration);
  {$ENDIF}
  {$IFDEF IOS}
  iOSVibrate;
  {$ENDIF}
  // Windows et Linux : pas de vibration
end;

class procedure TPlatformHelper.ShowNotification(const Title, Message: string);  
begin
  {$IFDEF ANDROID}
  ShowAndroidNotification(Title, Message);
  {$ENDIF}
  {$IFDEF IOS}
  ShowiOSNotification(Title, Message);
  {$ENDIF}
  {$IFDEF WINDOWS}
  ShowWindowsToast(Title, Message);
  {$ENDIF}
  {$IFDEF LINUX}
  ShowLinuxNotify(Title, Message);
  {$ENDIF}
end;

class function TPlatformHelper.GetDeviceInfo: string;  
begin
  Result := Format('Platform: %s, DPI: %d',
                   [GetPlatformName, GetScreenDPI]);

  {$IFDEF ANDROID}
  Result := Result + Format(', Android API: %d', [GetAndroidAPILevel]);
  {$ENDIF}
  {$IFDEF IOS}
  Result := Result + Format(', iOS Version: %s', [GetiOSVersion]);
  {$ENDIF}
end;

end.
```

### Adaptation des ressources

```pascal
unit ResourceManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics;

type
  TResourceScale = (rs1x, rs2x, rs3x);

  TResourceManager = class
  private
    FBasePath: string;
    FCurrentScale: TResourceScale;
    function DetermineScale: TResourceScale;
    function GetScaledPath(const AFileName: string): string;
  public
    constructor Create(const ABasePath: string);
    function LoadImage(const AName: string): TBitmap;
    function LoadIcon(const AName: string; ASize: Integer): TBitmap;
    property CurrentScale: TResourceScale read FCurrentScale;
  end;

implementation

constructor TResourceManager.Create(const ABasePath: string);  
begin
  inherited Create;
  FBasePath := ABasePath;
  FCurrentScale := DetermineScale;
end;

function TResourceManager.DetermineScale: TResourceScale;  
var
  dpi: Integer;
begin
  dpi := TPlatformHelper.GetScreenDPI;

  if dpi <= 160 then
    Result := rs1x        // mdpi
  else if dpi <= 240 then
    Result := rs2x        // hdpi/xhdpi
  else
    Result := rs3x;       // xxhdpi/xxxhdpi
end;

function TResourceManager.GetScaledPath(const AFileName: string): string;  
var
  name, ext: string;
  scaleSuffix: string;
begin
  name := ChangeFileExt(AFileName, '');
  ext := ExtractFileExt(AFileName);

  case FCurrentScale of
    rs1x: scaleSuffix := '';
    rs2x: scaleSuffix := '@2x';
    rs3x: scaleSuffix := '@3x';
  end;

  Result := FBasePath + name + scaleSuffix + ext;

  // Fallback si le fichier n'existe pas
  if not FileExists(Result) then
    Result := FBasePath + AFileName;
end;

function TResourceManager.LoadImage(const AName: string): TBitmap;  
var
  path: string;
begin
  Result := TBitmap.Create;
  path := GetScaledPath(AName);

  try
    Result.LoadFromFile(path);
  except
    on E: Exception do
    begin
      WriteLn('Erreur chargement image: ', path, ' - ', E.Message);
      Result.Free;
      Result := nil;
    end;
  end;
end;

function TResourceManager.LoadIcon(const AName: string; ASize: Integer): TBitmap;  
var
  original: TBitmap;
begin
  original := LoadImage(AName);
  if original = nil then
    Exit(nil);

  try
    Result := TBitmap.Create;
    Result.SetSize(ASize, ASize);
    Result.Canvas.StretchDraw(Rect(0, 0, ASize, ASize), original);
  finally
    original.Free;
  end;
end;

// Utilisation globale
var
  Resources: TResourceManager;

initialization
  Resources := TResourceManager.Create('resources/images/');

finalization
  Resources.Free;

end.
```

### Configuration par plateforme

```pascal
unit AppConfig;

{$mode objfpc}{$H+}

interface

type
  TPlatformConfig = record
    DefaultFontSize: Integer;
    DefaultMargin: Integer;
    ButtonHeight: Integer;
    UseNativeControls: Boolean;
    EnableAnimations: Boolean;
  end;

function GetPlatformConfig: TPlatformConfig;

implementation

function GetPlatformConfig: TPlatformConfig;  
begin
  {$IFDEF ANDROID}
  Result.DefaultFontSize := 14;
  Result.DefaultMargin := 16;
  Result.ButtonHeight := 48;
  Result.UseNativeControls := False;
  Result.EnableAnimations := True;
  {$ENDIF}

  {$IFDEF IOS}
  Result.DefaultFontSize := 15;
  Result.DefaultMargin := 20;
  Result.ButtonHeight := 44;
  Result.UseNativeControls := False;
  Result.EnableAnimations := True;
  {$ENDIF}

  {$IFDEF WINDOWS}
  Result.DefaultFontSize := 10;
  Result.DefaultMargin := 8;
  Result.ButtonHeight := 32;
  Result.UseNativeControls := True;
  Result.EnableAnimations := False;
  {$ENDIF}

  {$IFDEF LINUX}
  Result.DefaultFontSize := 10;
  Result.DefaultMargin := 8;
  Result.ButtonHeight := 32;
  Result.UseNativeControls := True;
  Result.EnableAnimations := True;
  {$ENDIF}
end;

end.
```

## Exemples d'applications complètes

### Application de liste de tâches

```pascal
unit TodoApp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  customdrawncontrols, customdrawndrawers, fgl;

type
  TTodoItem = class
    ID: Integer;
    Title: string;
    Completed: Boolean;
    CreatedAt: TDateTime;
  end;

  TTodoList = specialize TFPGObjectList<TTodoItem>;

  TFormTodo = class(TForm)
    CDEditNewTask: TCustomDrawnEdit;
    CDButtonAdd: TCustomDrawnButton;
    CDPanelList: TCustomDrawnPanel;
    CDScrollBox1: TCustomDrawnScrollBox;

    procedure FormCreate(Sender: TObject);
    procedure CDButtonAddClick(Sender: TObject);
  private
    FTodoList: TTodoList;
    FNextID: Integer;
    procedure LoadTasks;
    procedure SaveTasks;
    procedure AddTask(const ATitle: string);
    procedure ToggleTask(AID: Integer);
    procedure DeleteTask(AID: Integer);
    procedure RefreshUI;
    procedure CreateTaskCard(ATask: TTodoItem);
  public
  end;

var
  FormTodo: TFormTodo;

implementation

{$R *.lfm}

uses
  fpjson, jsonparser;

procedure TFormTodo.FormCreate(Sender: TObject);  
begin
  FTodoList := TTodoList.Create(True);  // True = possède les objets
  FNextID := 1;

  // Configuration de l'interface
  AppTheme.ApplyTheme(tsLight);
  ApplyThemeToForm(Self);

  // Configuration du champ de saisie
  CDEditNewTask.HintText := 'Nouvelle tâche...';
  CDEditNewTask.Font.Size := 16;

  // Configuration du bouton
  CDButtonAdd.Caption := '+';
  CDButtonAdd.Font.Size := 24;
  CDButtonAdd.Font.Style := [fsBold];

  LoadTasks;
  RefreshUI;
end;

procedure TFormTodo.CDButtonAddClick(Sender: TObject);  
begin
  if Trim(CDEditNewTask.Text) <> '' then
  begin
    AddTask(CDEditNewTask.Text);
    CDEditNewTask.Text := '';
    SaveTasks;
    RefreshUI;
  end;
end;

procedure TFormTodo.AddTask(const ATitle: string);  
var
  task: TTodoItem;
begin
  task := TTodoItem.Create;
  task.ID := FNextID;
  Inc(FNextID);
  task.Title := ATitle;
  task.Completed := False;
  task.CreatedAt := Now;
  FTodoList.Add(task);
end;

procedure TFormTodo.ToggleTask(AID: Integer);  
var
  i: Integer;
begin
  for i := 0 to FTodoList.Count - 1 do
  begin
    if FTodoList[i].ID = AID then
    begin
      FTodoList[i].Completed := not FTodoList[i].Completed;
      SaveTasks;
      RefreshUI;
      Break;
    end;
  end;
end;

procedure TFormTodo.DeleteTask(AID: Integer);  
var
  i: Integer;
begin
  for i := 0 to FTodoList.Count - 1 do
  begin
    if FTodoList[i].ID = AID then
    begin
      FTodoList.Delete(i);
      SaveTasks;
      RefreshUI;
      Break;
    end;
  end;
end;

procedure TFormTodo.RefreshUI;  
var
  i: Integer;
begin
  // Vider le conteneur
  while CDScrollBox1.ControlCount > 0 do
    CDScrollBox1.Controls[0].Free;

  // Recréer les cartes de tâches
  for i := 0 to FTodoList.Count - 1 do
    CreateTaskCard(FTodoList[i]);
end;

procedure TFormTodo.CreateTaskCard(ATask: TTodoItem);  
var
  card: TCustomCard;
  checkbox: TCustomDrawnCheckBox;
  deleteBtn: TCustomDrawnButton;
  y: Integer;
begin
  y := FTodoList.IndexOf(ATask) * 70;

  // Créer la carte
  card := TCustomCard.Create(CDScrollBox1);
  card.Parent := CDScrollBox1;
  card.Left := 8;
  card.Top := y;
  card.Width := CDScrollBox1.Width - 16;
  card.Height := 60;
  card.Title := '';
  card.Content := '';

  // Case à cocher
  checkbox := TCustomDrawnCheckBox.Create(card);
  checkbox.Parent := card;
  checkbox.Left := 10;
  checkbox.Top := 15;
  checkbox.Width := card.Width - 80;
  checkbox.Caption := ATask.Title;
  checkbox.Checked := ATask.Completed;
  checkbox.Tag := ATask.ID;
  checkbox.OnChange := @OnTaskCheckboxChange;

  if ATask.Completed then
  begin
    checkbox.Font.Style := [fsStrikeOut];
    checkbox.Font.Color := clGray;
  end;

  // Bouton supprimer
  deleteBtn := TCustomDrawnButton.Create(card);
  deleteBtn.Parent := card;
  deleteBtn.Left := card.Width - 60;
  deleteBtn.Top := 10;
  deleteBtn.Width := 50;
  deleteBtn.Height := 40;
  deleteBtn.Caption := '🗑';
  deleteBtn.Tag := ATask.ID;
  deleteBtn.OnClick := @OnDeleteButtonClick;
end;

procedure TFormTodo.OnTaskCheckboxChange(Sender: TObject);  
var
  checkbox: TCustomDrawnCheckBox;
begin
  checkbox := TCustomDrawnCheckBox(Sender);
  ToggleTask(checkbox.Tag);
end;

procedure TFormTodo.OnDeleteButtonClick(Sender: TObject);  
var
  btn: TCustomDrawnButton;
begin
  btn := TCustomDrawnButton(Sender);

  if MessageDlg('Confirmation',
                'Supprimer cette tâche ?',
                mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    DeleteTask(btn.Tag);
  end;
end;

procedure TFormTodo.LoadTasks;  
var
  json: TJSONObject;
  tasksArray: TJSONArray;
  taskObj: TJSONObject;
  task: TTodoItem;
  i: Integer;
  filePath: string;
begin
  filePath := GetAppConfigDir(False) + 'tasks.json';

  if not FileExists(filePath) then
    Exit;

  try
    json := TJSONObject(GetJSON(TFileStream.Create(filePath, fmOpenRead)));
    try
      FNextID := json.Get('next_id', 1);
      tasksArray := TJSONArray(json.Get('tasks'));

      for i := 0 to tasksArray.Count - 1 do
      begin
        taskObj := TJSONObject(tasksArray[i]);
        task := TTodoItem.Create;
        task.ID := taskObj.Get('id', 0);
        task.Title := taskObj.Get('title', '');
        task.Completed := taskObj.Get('completed', False);
        task.CreatedAt := StrToDateTime(taskObj.Get('created_at',
                                        DateTimeToStr(Now)));
        FTodoList.Add(task);
      end;
    finally
      json.Free;
    end;
  except
    on E: Exception do
      ShowMessage('Erreur chargement: ' + E.Message);
  end;
end;

procedure TFormTodo.SaveTasks;  
var
  json: TJSONObject;
  tasksArray: TJSONArray;
  taskObj: TJSONObject;
  i: Integer;
  filePath: string;
  fs: TFileStream;
begin
  json := TJSONObject.Create;
  tasksArray := TJSONArray.Create;

  try
    json.Add('next_id', FNextID);

    for i := 0 to FTodoList.Count - 1 do
    begin
      taskObj := TJSONObject.Create;
      taskObj.Add('id', FTodoList[i].ID);
      taskObj.Add('title', FTodoList[i].Title);
      taskObj.Add('completed', FTodoList[i].Completed);
      taskObj.Add('created_at', DateTimeToStr(FTodoList[i].CreatedAt));
      tasksArray.Add(taskObj);
    end;

    json.Add('tasks', tasksArray);

    filePath := GetAppConfigDir(False) + 'tasks.json';
    ForceDirectories(ExtractFilePath(filePath));

    fs := TFileStream.Create(filePath, fmCreate);
    try
      WriteJSONToStream(json, fs);
    finally
      fs.Free;
    end;
  finally
    json.Free;
  end;
end;

end.
```

### Application calculatrice

```pascal
unit CalculatorApp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  customdrawncontrols, Math;

type
  TOperation = (opNone, opAdd, opSubtract, opMultiply, opDivide);

  TFormCalculator = class(TForm)
    CDLabelDisplay: TCustomDrawnLabel;

    procedure FormCreate(Sender: TObject);
  private
    FCurrentValue: Double;
    FStoredValue: Double;
    FCurrentOperation: TOperation;
    FNewNumber: Boolean;

    procedure CreateButtons;
    procedure OnNumberClick(Sender: TObject);
    procedure OnOperationClick(Sender: TObject);
    procedure OnEqualsClick(Sender: TObject);
    procedure OnClearClick(Sender: TObject);
    procedure UpdateDisplay(const AValue: string);
    function Calculate: Double;
  public
  end;

var
  FormCalculator: TFormCalculator;

implementation

{$R *.lfm}

procedure TFormCalculator.FormCreate(Sender: TObject);  
begin
  // Configuration de l'affichage
  CDLabelDisplay.Alignment := taRightJustify;
  CDLabelDisplay.Font.Size := 32;
  CDLabelDisplay.Font.Style := [fsBold];
  CDLabelDisplay.Color := clBlack;
  CDLabelDisplay.Font.Color := clWhite;
  CDLabelDisplay.Height := 80;
  CDLabelDisplay.Align := alTop;

  FCurrentValue := 0;
  FStoredValue := 0;
  FCurrentOperation := opNone;
  FNewNumber := True;

  UpdateDisplay('0');
  CreateButtons;
end;

procedure TFormCalculator.CreateButtons;  
const
  ButtonLabels: array[0..19] of string = (
    '7', '8', '9', '÷',
    '4', '5', '6', '×',
    '1', '2', '3', '-',
    'C', '0', '=', '+'
  );
var
  i, row, col: Integer;
  btn: TCustomDrawnButton;
  x, y, btnWidth, btnHeight: Integer;
  label: string;
begin
  btnWidth := (Width - 50) div 4;
  btnHeight := (Height - CDLabelDisplay.Height - 50) div 4;

  for i := 0 to High(ButtonLabels) do
  begin
    row := i div 4;
    col := i mod 4;

    x := 10 + col * (btnWidth + 5);
    y := CDLabelDisplay.Height + 10 + row * (btnHeight + 5);

    btn := TCustomDrawnButton.Create(Self);
    btn.Parent := Self;
    btn.SetBounds(x, y, btnWidth, btnHeight);

    label := ButtonLabels[i];
    btn.Caption := label;
    btn.Font.Size := 24;
    btn.Rounded := True;
    btn.RoundedRadius := 8;

    // Couleurs selon le type de bouton
    if label[1] in ['0'..'9'] then
    begin
      btn.Color := clWhite;
      btn.Font.Color := clBlack;
      btn.OnClick := @OnNumberClick;
    end
    else if label = 'C' then
    begin
      btn.Color := clRed;
      btn.Font.Color := clWhite;
      btn.OnClick := @OnClearClick;
    end
    else if label = '=' then
    begin
      btn.Color := clGreen;
      btn.Font.Color := clWhite;
      btn.OnClick := @OnEqualsClick;
    end
    else
    begin
      btn.Color := clOrange;
      btn.Font.Color := clWhite;
      btn.OnClick := @OnOperationClick;
    end;
  end;
end;

procedure TFormCalculator.OnNumberClick(Sender: TObject);  
var
  digit: string;
  currentText: string;
begin
  digit := TCustomDrawnButton(Sender).Caption;
  currentText := CDLabelDisplay.Caption;

  if FNewNumber then
  begin
    currentText := digit;
    FNewNumber := False;
  end
  else
  begin
    if currentText = '0' then
      currentText := digit
    else
      currentText := currentText + digit;
  end;

  UpdateDisplay(currentText);
  FCurrentValue := StrToFloatDef(currentText, 0);
end;

procedure TFormCalculator.OnOperationClick(Sender: TObject);  
var
  op: string;
begin
  op := TCustomDrawnButton(Sender).Caption;

  if FCurrentOperation <> opNone then
    FStoredValue := Calculate
  else
    FStoredValue := FCurrentValue;

  // Déterminer l'opération
  case op of
    '+': FCurrentOperation := opAdd;
    '-': FCurrentOperation := opSubtract;
    '×': FCurrentOperation := opMultiply;
    '÷': FCurrentOperation := opDivide;
  end;

  FNewNumber := True;
  UpdateDisplay(FloatToStr(FStoredValue));
end;

procedure TFormCalculator.OnEqualsClick(Sender: TObject);  
var
  result: Double;
begin
  if FCurrentOperation = opNone then
    Exit;

  result := Calculate;
  UpdateDisplay(FloatToStr(result));

  FCurrentValue := result;
  FStoredValue := 0;
  FCurrentOperation := opNone;
  FNewNumber := True;
end;

procedure TFormCalculator.OnClearClick(Sender: TObject);  
begin
  FCurrentValue := 0;
  FStoredValue := 0;
  FCurrentOperation := opNone;
  FNewNumber := True;
  UpdateDisplay('0');
end;

function TFormCalculator.Calculate: Double;  
begin
  Result := 0;

  case FCurrentOperation of
    opAdd:
      Result := FStoredValue + FCurrentValue;
    opSubtract:
      Result := FStoredValue - FCurrentValue;
    opMultiply:
      Result := FStoredValue * FCurrentValue;
    opDivide:
      if FCurrentValue <> 0 then
        Result := FStoredValue / FCurrentValue
      else
      begin
        ShowMessage('Division par zéro impossible');
        Result := FStoredValue;
      end;
  end;
end;

procedure TFormCalculator.UpdateDisplay(const AValue: string);  
var
  displayValue: string;
begin
  displayValue := AValue;

  // Limiter la longueur
  if Length(displayValue) > 12 then
    displayValue := Copy(displayValue, 1, 12);

  // Formater les grands nombres
  if Pos('.', displayValue) = 0 then
  begin
    if StrToFloatDef(displayValue, 0) > 999999 then
      displayValue := FormatFloat('#,##0', StrToFloat(displayValue));
  end;

  CDLabelDisplay.Caption := displayValue;
end;

end.
```

### Application de dessin

```pascal
unit DrawingApp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  customdrawncontrols, fgl;

type
  TDrawPoint = record
    X, Y: Integer;
    Color: TColor;
    Size: Integer;
  end;

  TDrawPath = specialize TFPGList<TDrawPoint>;

  TFormDrawing = class(TForm)
    CDPanelCanvas: TCustomDrawnPanel;
    CDPanelToolbar: TCustomDrawnPanel;
    CDButtonClear: TCustomDrawnButton;
    CDTrackBarSize: TCustomDrawnTrackBar;

    procedure FormCreate(Sender: TObject);
    procedure CDPanelCanvasPaint(Sender: TObject);
    procedure CDPanelCanvasMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CDPanelCanvasMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure CDPanelCanvasMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CDButtonClearClick(Sender: TObject);
  private
    FCurrentPath: TDrawPath;
    FAllPaths: TFPList;
    FIsDrawing: Boolean;
    FCurrentColor: TColor;
    FCurrentSize: Integer;

    procedure CreateColorButtons;
    procedure OnColorButtonClick(Sender: TObject);
    procedure SaveDrawing(const AFileName: string);
    procedure LoadDrawing(const AFileName: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  FormDrawing: TFormDrawing;

implementation

{$R *.lfm}

constructor TFormDrawing.Create(AOwner: TComponent);  
begin
  inherited Create(AOwner);
  FAllPaths := TFPList.Create;
  FCurrentColor := clBlack;
  FCurrentSize := 5;
  FIsDrawing := False;
end;

destructor TFormDrawing.Destroy;  
var
  i: Integer;
begin
  // Libérer tous les chemins
  for i := 0 to FAllPaths.Count - 1 do
    TDrawPath(FAllPaths[i]).Free;
  FAllPaths.Free;

  inherited Destroy;
end;

procedure TFormDrawing.FormCreate(Sender: TObject);  
begin
  // Configuration du canvas
  CDPanelCanvas.Color := clWhite;
  CDPanelCanvas.Align := alClient;

  // Configuration de la barre d'outils
  CDPanelToolbar.Height := 60;
  CDPanelToolbar.Align := alBottom;
  CDPanelToolbar.Color := clLightGray;

  // Bouton effacer
  CDButtonClear.Caption := 'Effacer';
  CDButtonClear.Color := clRed;
  CDButtonClear.Font.Color := clWhite;

  // Curseur de taille
  CDTrackBarSize.Min := 1;
  CDTrackBarSize.Max := 20;
  CDTrackBarSize.Position := FCurrentSize;

  CreateColorButtons;
end;

procedure TFormDrawing.CreateColorButtons;  
const
  Colors: array[0..7] of TColor = (
    clBlack, clRed, clGreen, clBlue,
    clYellow, clOrange, clPurple, clWhite
  );
var
  i: Integer;
  btn: TCustomDrawnButton;
begin
  for i := 0 to High(Colors) do
  begin
    btn := TCustomDrawnButton.Create(CDPanelToolbar);
    btn.Parent := CDPanelToolbar;
    btn.Left := 10 + i * 50;
    btn.Top := 10;
    btn.Width := 40;
    btn.Height := 40;
    btn.Color := Colors[i];
    btn.Tag := Integer(Colors[i]);
    btn.OnClick := @OnColorButtonClick;
    btn.Rounded := True;
    btn.RoundedRadius := 20;  // Circulaire

    // Bordure pour le bouton blanc
    if Colors[i] = clWhite then
    begin
      btn.BorderStyle := bsSingle;
      btn.BorderColor := clGray;
    end;
  end;
end;

procedure TFormDrawing.OnColorButtonClick(Sender: TObject);  
begin
  FCurrentColor := TColor(TCustomDrawnButton(Sender).Tag);
end;

procedure TFormDrawing.CDPanelCanvasMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  point: TDrawPoint;
begin
  if Button = mbLeft then
  begin
    FIsDrawing := True;
    FCurrentPath := TDrawPath.Create;

    point.X := X;
    point.Y := Y;
    point.Color := FCurrentColor;
    point.Size := CDTrackBarSize.Position;

    FCurrentPath.Add(point);
  end;
end;

procedure TFormDrawing.CDPanelCanvasMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  point: TDrawPoint;
begin
  if FIsDrawing then
  begin
    point.X := X;
    point.Y := Y;
    point.Color := FCurrentColor;
    point.Size := CDTrackBarSize.Position;

    FCurrentPath.Add(point);
    CDPanelCanvas.Invalidate;
  end;
end;

procedure TFormDrawing.CDPanelCanvasMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if FIsDrawing then
  begin
    FAllPaths.Add(FCurrentPath);
    FIsDrawing := False;
    CDPanelCanvas.Invalidate;
  end;
end;

procedure TFormDrawing.CDPanelCanvasPaint(Sender: TObject);  
var
  i, j: Integer;
  path: TDrawPath;
  point, prevPoint: TDrawPoint;
begin
  with CDPanelCanvas.Canvas do
  begin
    // Dessiner tous les chemins sauvegardés
    for i := 0 to FAllPaths.Count - 1 do
    begin
      path := TDrawPath(FAllPaths[i]);

      if path.Count > 0 then
      begin
        for j := 1 to path.Count - 1 do
        begin
          prevPoint := path[j - 1];
          point := path[j];

          Pen.Color := point.Color;
          Pen.Width := point.Size;
          Pen.Style := psSolid;

          MoveTo(prevPoint.X, prevPoint.Y);
          LineTo(point.X, point.Y);
        end;
      end;
    end;

    // Dessiner le chemin en cours
    if FIsDrawing and Assigned(FCurrentPath) and (FCurrentPath.Count > 0) then
    begin
      for j := 1 to FCurrentPath.Count - 1 do
      begin
        prevPoint := FCurrentPath[j - 1];
        point := FCurrentPath[j];

        Pen.Color := point.Color;
        Pen.Width := point.Size;

        MoveTo(prevPoint.X, prevPoint.Y);
        LineTo(point.X, point.Y);
      end;
    end;
  end;
end;

procedure TFormDrawing.CDButtonClearClick(Sender: TObject);  
var
  i: Integer;
begin
  if MessageDlg('Confirmation',
                'Effacer tout le dessin ?',
                mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    // Libérer tous les chemins
    for i := 0 to FAllPaths.Count - 1 do
      TDrawPath(FAllPaths[i]).Free;
    FAllPaths.Clear;

    CDPanelCanvas.Invalidate;
  end;
end;

procedure TFormDrawing.SaveDrawing(const AFileName: string);  
var
  fs: TFileStream;
  i, j, pathCount, pointCount: Integer;
  path: TDrawPath;
  point: TDrawPoint;
begin
  fs := TFileStream.Create(AFileName, fmCreate);
  try
    // Écrire le nombre de chemins
    pathCount := FAllPaths.Count;
    fs.Write(pathCount, SizeOf(pathCount));

    // Écrire chaque chemin
    for i := 0 to FAllPaths.Count - 1 do
    begin
      path := TDrawPath(FAllPaths[i]);
      pointCount := path.Count;
      fs.Write(pointCount, SizeOf(pointCount));

      // Écrire chaque point
      for j := 0 to path.Count - 1 do
      begin
        point := path[j];
        fs.Write(point, SizeOf(TDrawPoint));
      end;
    end;
  finally
    fs.Free;
  end;
end;

procedure TFormDrawing.LoadDrawing(const AFileName: string);  
var
  fs: TFileStream;
  i, j, pathCount, pointCount: Integer;
  path: TDrawPath;
  point: TDrawPoint;
begin
  // Effacer le dessin actuel
  for i := 0 to FAllPaths.Count - 1 do
    TDrawPath(FAllPaths[i]).Free;
  FAllPaths.Clear;

  fs := TFileStream.Create(AFileName, fmOpenRead);
  try
    // Lire le nombre de chemins
    fs.Read(pathCount, SizeOf(pathCount));

    // Lire chaque chemin
    for i := 0 to pathCount - 1 do
    begin
      path := TDrawPath.Create;
      fs.Read(pointCount, SizeOf(pointCount));

      // Lire chaque point
      for j := 0 to pointCount - 1 do
      begin
        fs.Read(point, SizeOf(TDrawPoint));
        path.Add(point);
      end;

      FAllPaths.Add(path);
    end;
  finally
    fs.Free;
  end;

  CDPanelCanvas.Invalidate;
end;

end.
```

## Bonnes pratiques Custom Drawn

### Structure du code

```pascal
// Organisation recommandée

// 1. Séparer les composants personnalisés
unit MyCustomComponents;  
interface
  type
    TMyButton = class(TCustomDrawnButton)
    // ...
    end;

// 2. Séparer les styles et thèmes
unit MyThemes;  
interface
  type
    TMyTheme = class
    // ...
    end;

// 3. Séparer la logique métier
unit MyBusinessLogic;  
interface
  type
    TDataManager = class
    // ...
    end;

// 4. Formulaires minimalistes
unit MyForm;  
interface
  uses
    MyCustomComponents, MyThemes, MyBusinessLogic;
  type
    TFormMain = class(TForm)
    // Seulement l'UI et les événements
    end;
```

### Tests et débogage

```pascal
unit TestCustomDrawn;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  customdrawncontrols;

type
  TTestCustomComponents = class(TTestCase)
  private
    FClicked: Boolean;
    procedure HandleButtonClick(Sender: TObject);
  published
    procedure TestButtonClick;
    procedure TestEditValidation;
    procedure TestThemeApplication;
  end;

implementation

procedure TTestCustomComponents.HandleButtonClick(Sender: TObject);  
begin
  FClicked := True;
end;

procedure TTestCustomComponents.TestButtonClick;  
var
  button: TCustomDrawnButton;
begin
  FClicked := False;

  button := TCustomDrawnButton.Create(nil);
  try
    button.OnClick := @HandleButtonClick;

    // Simuler un clic
    button.Click;

    AssertTrue('Le bouton devrait être cliqué', FClicked);
  finally
    button.Free;
  end;
end;

procedure TTestCustomComponents.TestEditValidation;  
var
  edit: TCustomDrawnEdit;
begin
  edit := TCustomDrawnEdit.Create(nil);
  try
    edit.Text := 'test@example.com';
    AssertTrue('Email devrait être valide', IsValidEmail(edit.Text));

    edit.Text := 'invalid';
    AssertFalse('Email invalide devrait être rejeté', IsValidEmail(edit.Text));
  finally
    edit.Free;
  end;
end;

procedure TTestCustomComponents.TestThemeApplication;  
var
  theme: TAppTheme;
  button: TCustomDrawnButton;
begin
  theme := TAppTheme.Create;
  button := TCustomDrawnButton.Create(nil);
  try
    theme.ApplyTheme(tsDark);
    theme.ApplyToControl(button);

    AssertEquals('La couleur devrait être celle du thème sombre',
                 theme.PrimaryColor, button.Color);
  finally
    button.Free;
    theme.Free;
  end;
end;

initialization
  RegisterTest(TTestCustomComponents);

end.
```

### Documentation

```pascal
/// <summary>
/// Composant de carte personnalisé avec titre et contenu
/// </summary>
/// <remarks>
/// Ce composant est compatible avec toutes les plateformes
/// et respecte le thème de l'application
/// </remarks>
type
  TCustomCard = class(TCustomDrawnControl)
  private
    FTitle: string;
    FContent: string;
    /// <summary>
    /// Définit le titre de la carte
    /// </summary>
    procedure SetTitle(const Value: string);
  public
    /// <summary>
    /// Crée une nouvelle instance de TCustomCard
    /// </summary>
    /// <param name="AOwner">Propriétaire du composant</param>
    constructor Create(AOwner: TComponent); override;
  published
    /// <summary>
    /// Titre affiché en haut de la carte
    /// </summary>
    property Title: string read FTitle write SetTitle;

    /// <summary>
    /// Contenu principal de la carte
    /// </summary>
    property Content: string read FContent write FContent;
  end;
```

## Comparaison avec d'autres approches

### Custom Drawn vs LCL natif

| Aspect | Custom Drawn | LCL Natif |
|--------|--------------|-----------|
| **Apparence** | Identique partout | Native à chaque OS |
| **Performance** | Très bonne | Excellente |
| **Taille binaire** | Petite | Moyenne à grande |
| **Flexibilité** | Maximale | Limitée aux widgets OS |
| **Temps de dev** | Plus long | Plus rapide |
| **Maintenance** | Un seul code | Code par plateforme |
| **Courbe d'apprentissage** | Moyenne | Faible |

### Custom Drawn vs LAMW (Android natif)

| Aspect | Custom Drawn | LAMW |
|--------|--------------|------|
| **Look & Feel** | Personnalisé | Android natif |
| **Plateformes** | Multi-plateforme | Android uniquement |
| **Complexité** | Moyenne | Élevée (JNI) |
| **Réutilisation code** | 100% | 60-70% |
| **Intégration système** | Limitée | Complète |

### Quand utiliser quoi ?

**Utilisez Custom Drawn si** :
- ✓ Vous voulez une apparence unique et cohérente
- ✓ Vous ciblez plusieurs plateformes mobiles
- ✓ Vous avez besoin de contrôle total sur le rendu
- ✓ Vous créez une application de type "jeu" ou créative
- ✓ Vous voulez minimiser la taille de l'application

**Utilisez LCL natif si** :
- ✓ L'apparence native est importante
- ✓ Vous ciblez principalement desktop
- ✓ Vous voulez un développement rapide
- ✓ Vous utilisez beaucoup de composants standards

**Utilisez LAMW si** :
- ✓ Android est votre cible principale
- ✓ Vous avez besoin d'intégration système profonde
- ✓ Vous voulez respecter Material Design
- ✓ Vous utilisez des services Android spécifiques

## Ressources et communauté

### Documentation officielle

**Sites web** :
- Wiki Lazarus : https://wiki.lazarus.freepascal.org/Custom_Drawn
- Forum Lazarus : Section Custom Drawn
- GitHub : lazarus-ccr/lazarus

**Tutoriels** :
- Custom Drawn Getting Started
- Building Mobile Apps with Custom Drawn
- Custom Drawn Components Tutorial

### Exemples de projets

**Dans le dossier examples de Lazarus** :
```
lazarus/examples/customdrawn/
├── basic/              ← Exemples de base
├── android/            ← Spécifique Android
├── ios/                ← Spécifique iOS
└── multiplatform/      ← Applications complètes
```

**Projets communautaires** :
- Custom Drawn Games (GitHub)
- Mobile UI Toolkit (SourceForge)
- Cross-Platform Demos (Lazarus Forum)

### Bibliothèques complémentaires

**Graphiques** :
- BGRABitmap : Graphiques avancés 2D
- AggPas : Anti-aliasing de qualité
- ZenGL : Moteur 2D pour jeux

**Utilitaires** :
- LazUtils : Fonctions utilitaires
- FCL : Free Component Library
- LazUnicode : Support Unicode complet

## Conclusion

Custom Drawn représente une approche puissante et flexible pour le développement d'interfaces mobiles multi-plateformes avec FreePascal. En maîtrisant cette technologie, vous pouvez :

### Avantages clés

**✓ Code unique, apparence cohérente**
- Écrivez une fois, déployez partout
- Contrôle total sur le design
- Pas de surprises entre plateformes

**✓ Performance optimale**
- Rendu direct sans surcouche
- Gestion mémoire efficace
- Adapté au mobile

**✓ Flexibilité maximale**
- Créez des designs uniques
- Pas de limites des widgets natifs
- Animations personnalisées

**✓ Maintenabilité**
- Une seule base de code
- Tests simplifiés
- Évolution facilitée

### Points d'attention

**⚠ Investissement initial**
- Courbe d'apprentissage
- Développement de composants personnalisés
- Temps de mise en place

**⚠ Apparence non-native**
- Peut dépayser certains utilisateurs
- Nécessite un bon design
- Attention aux conventions de chaque OS

**⚠ Intégration système**
- Moins directe que les composants natifs
- Nécessite du code spécifique par plateforme
- Abstraction nécessaire

### Recommandations finales

1. **Commencez simple** : Maîtrisez les composants de base avant de créer les vôtres
2. **Pensez responsive** : Testez sur différentes tailles d'écran dès le début
3. **Optimisez tôt** : La performance mobile est critique
4. **Testez sur vrais appareils** : Les émulateurs ne suffisent pas
5. **Documentez** : Facilitez la maintenance future
6. **Partagez** : Contribuez à la communauté

### Prochaines étapes

Pour approfondir Custom Drawn :

- **Pratiquer** : Créez des applications complètes
- **Étudier** : Analysez le code source des exemples
- **Contribuer** : Partagez vos composants
- **Explorer** : Testez les limites de la technologie
- **Combiner** : Intégrez avec d'autres approches si nécessaire

**Custom Drawn vous offre la liberté de créer des applications mobiles exactement comme vous les imaginez, avec la puissance et l'efficacité de FreePascal. À vous de jouer ! 🎨📱**

---

**Sections connexes** :
- 13.1 LAMW - Lazarus Android Module Wizard
- 13.2 Architecture Android et JNI
- 13.3 Interfaces natives Android
- 13.5 Capteurs et périphériques mobiles
- 20. Optimisation et Performance

**Ressources complémentaires** :
- BGRABitmap pour graphiques avancés
- Castle Game Engine pour jeux
- mORMot pour backend et services

⏭️ [Capteurs et périphériques mobiles](/13-developpement-mobile-embarque/05-capteurs-peripheriques-mobiles.md)
