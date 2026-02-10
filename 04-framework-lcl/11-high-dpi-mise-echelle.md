🔝 Retour au [Sommaire](/SOMMAIRE.md)

# High-DPI et mise à l'échelle dans Lazarus/FreePascal

## Introduction : Comprendre le High-DPI

### Qu'est-ce que le DPI ?

**DPI** signifie "Dots Per Inch" (points par pouce). C'est une mesure de la densité de pixels d'un écran :
- **96 DPI** : Résolution standard traditionnelle (100%)
- **120 DPI** : 125% de mise à l'échelle
- **144 DPI** : 150% de mise à l'échelle
- **192 DPI** : 200% de mise à l'échelle

### Le problème du High-DPI

Avec les écrans modernes haute résolution (4K, Retina, etc.), les applications conçues pour 96 DPI apparaissent **minuscules et illisibles**. Sans adaptation, un bouton de 100 pixels sur un écran 4K sera deux fois plus petit qu'sur un écran Full HD de même taille physique.

### Exemples concrets d'écrans High-DPI
- **Laptops modernes** : Souvent 150% ou 200% de mise à l'échelle
- **Écrans 4K de bureau** : Généralement 150% de mise à l'échelle
- **Tablettes Surface** : Jusqu'à 250% de mise à l'échelle
- **Smartphones** : 300% et plus (mais gérés différemment)

## Concepts fondamentaux de la mise à l'échelle

### Pixels physiques vs pixels logiques

- **Pixels physiques** : Les vrais pixels de l'écran
- **Pixels logiques** : Les unités utilisées dans votre code

Sur un écran à 200% de mise à l'échelle :
- 1 pixel logique = 2×2 pixels physiques
- Un bouton de 100×30 pixels logiques occupera 200×60 pixels physiques

### Types de mise à l'échelle

1. **Mise à l'échelle système** : Windows/Linux agrandit toute l'application
2. **Mise à l'échelle par application** : L'application gère elle-même l'adaptation
3. **Mise à l'échelle par moniteur** : Différente échelle selon l'écran (multi-moniteur)

## Activation du support High-DPI dans Lazarus

### Configuration du projet

#### Étape 1 : Options du projet

Dans Lazarus, allez dans **Projet → Options du projet → Application** :

```
☑ Utiliser le manifest DPI (Windows)
☑ DPI awareness: True
```

#### Étape 2 : Propriétés du formulaire

Pour chaque formulaire (TForm) :

```pascal
// Dans l'inspecteur d'objets ou dans le code
Form1.Scaled := True; // Active la mise à l'échelle automatique
```

### Le fichier manifest (Windows)

Lazarus peut générer automatiquement un manifest pour Windows. Si vous voulez le personnaliser :

```xml
<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<assembly xmlns="urn:schemas-microsoft-com:asm.v1" manifestVersion="1.0">
  <application xmlns="urn:schemas-microsoft-com:asm.v3">
    <windowsSettings>
      <dpiAware xmlns="http://schemas.microsoft.com/SMI/2005/WindowsSettings">true/PM</dpiAware>
      <dpiAwareness xmlns="http://schemas.microsoft.com/SMI/2016/WindowsSettings">PerMonitorV2</dpiAwareness>
    </windowsSettings>
  </application>
</assembly>
```

## Propriétés essentielles pour le High-DPI

### La propriété Scaled

```pascal
Form1.Scaled := True; // Active la mise à l'échelle automatique
```

Quand `Scaled = True`, Lazarus ajuste automatiquement :
- Les dimensions des contrôles
- Les tailles de police
- Les marges et espacements

### La propriété DesignTimePPI

```pascal
Form1.DesignTimePPI := 96; // DPI lors de la conception
```

Indique le DPI utilisé lors de la conception du formulaire. Lazarus calcule le ratio de mise à l'échelle basé sur cette valeur.

### Obtenir le DPI actuel

```pascal
procedure TForm1.FormCreate(Sender: TObject);  
var
  CurrentDPI: Integer;
  ScaleFactor: Double;
begin
  CurrentDPI := Screen.PixelsPerInch;
  ScaleFactor := CurrentDPI / 96; // Ratio par rapport au DPI standard

  Caption := Format('Application - DPI: %d (×%.2f)', [CurrentDPI, ScaleFactor]);
end;
```

## Gestion des images et icônes

### Le problème des images bitmap

Les images bitmap (BMP, PNG, JPG) deviennent **floues** quand elles sont agrandies. Solutions :

#### Solution 1 : Images multiples

```pascal
type
  TImageManager = class
  private
    FImages96: TImageList;   // Images pour 96 DPI (100%)
    FImages144: TImageList;  // Images pour 144 DPI (150%)
    FImages192: TImageList;  // Images pour 192 DPI (200%)
  public
    function GetImageList: TImageList;
  end;

function TImageManager.GetImageList: TImageList;  
var
  CurrentDPI: Integer;
begin
  CurrentDPI := Screen.PixelsPerInch;

  if CurrentDPI <= 120 then
    Result := FImages96
  else if CurrentDPI <= 168 then
    Result := FImages144
  else
    Result := FImages192;
end;
```

#### Solution 2 : Images vectorielles (SVG)

Les images SVG peuvent être redimensionnées sans perte de qualité :

```pascal
uses
  BGRABitmap, BGRASVG; // Bibliothèque pour SVG

procedure ChargerIconeSVG(const Fichier: string; Taille: Integer);  
var
  SVG: TBGRASVG;
  Bitmap: TBGRABitmap;
begin
  SVG := TBGRASVG.Create(Fichier);
  try
    // Redimensionne selon le DPI
    Bitmap := TBGRABitmap.Create(Taille, Taille);
    SVG.Draw(Bitmap.Canvas2D, 0, 0, Taille, Taille);
    Image1.Picture.Assign(Bitmap);
  finally
    SVG.Free;
    Bitmap.Free;
  end;
end;
```

### Configuration des TImageList

```pascal
procedure ConfigurerImageList;  
var
  ScaleFactor: Double;
  NouvellesTailles: Integer;
begin
  ScaleFactor := Screen.PixelsPerInch / 96;

  // Adapter la taille des images
  NouvellesTailles := Round(16 * ScaleFactor); // 16 pixels de base
  ImageList1.Width := NouvellesTailles;
  ImageList1.Height := NouvellesTailles;
end;
```

## Polices et texte

### Mise à l'échelle automatique des polices

Avec `Scaled := True`, les polices sont automatiquement ajustées. Mais vous pouvez aussi le faire manuellement :

```pascal
procedure AjusterTaillePolice(Control: TControl);  
var
  ScaleFactor: Double;
  NouvelleTaille: Integer;
begin
  ScaleFactor := Screen.PixelsPerInch / 96;

  if Control is TLabel then
  begin
    NouvelleTaille := Round(9 * ScaleFactor); // Taille de base 9
    TLabel(Control).Font.Size := NouvelleTaille;
  end;
end;
```

### Utiliser des unités indépendantes du DPI

```pascal
// Convertir des pixels en unités indépendantes
function PixelsToPoints(Pixels: Integer): Integer;  
begin
  Result := MulDiv(Pixels, 72, Screen.PixelsPerInch);
end;

function PointsToPixels(Points: Integer): Integer;  
begin
  Result := MulDiv(Points, Screen.PixelsPerInch, 72);
end;
```

## Mise à l'échelle manuelle des contrôles

### Fonction de mise à l'échelle générique

```pascal
procedure MettreAEchelle(Control: TControl; DPIBase: Integer = 96);  
var
  Ratio: Double;
  i: Integer;
begin
  Ratio := Screen.PixelsPerInch / DPIBase;

  Control.Left := Round(Control.Left * Ratio);
  Control.Top := Round(Control.Top * Ratio);
  Control.Width := Round(Control.Width * Ratio);
  Control.Height := Round(Control.Height * Ratio);

  // Si c'est un conteneur, traiter les enfants
  if Control is TWinControl then
  begin
    for i := 0 to TWinControl(Control).ControlCount - 1 do
      MettreAEchelle(TWinControl(Control).Controls[i], DPIBase);
  end;
end;
```

### Gestion des marges et espacements

```pascal
type
  TDPIHelper = class
  private
    class var FBaseDPI: Integer;
  public
    class function Scale(Value: Integer): Integer;
    class function ScaleMargin(Value: Integer): Integer;
  end;

class function TDPIHelper.Scale(Value: Integer): Integer;  
begin
  if FBaseDPI = 0 then FBaseDPI := 96;
  Result := MulDiv(Value, Screen.PixelsPerInch, FBaseDPI);
end;

class function TDPIHelper.ScaleMargin(Value: Integer): Integer;  
begin
  Result := Scale(Value);
  // Assurer un minimum de 1 pixel
  if (Result = 0) and (Value > 0) then
    Result := 1;
end;

// Utilisation
procedure TForm1.FormCreate(Sender: TObject);  
begin
  Panel1.BorderSpacing.Around := TDPIHelper.ScaleMargin(8);
  Button1.Margins.Left := TDPIHelper.ScaleMargin(4);
end;
```

## Spécificités Windows

### Niveaux de DPI awareness sous Windows

Windows propose plusieurs niveaux :

1. **Non-DPI aware** : Windows met à l'échelle toute l'application (flou possible)
2. **System DPI aware** : L'application s'adapte au DPI système au démarrage
3. **Per-Monitor DPI aware** : L'application s'adapte quand elle change d'écran
4. **Per-Monitor V2** : Version améliorée (Windows 10 1703+)

### Configuration dans le code

```pascal
uses
  Windows;

procedure ConfigurerDPIWindows;  
type
  TSetProcessDpiAwareness = function(value: Integer): HRESULT; stdcall;
var
  SetProcessDpiAwareness: TSetProcessDpiAwareness;
  ShcoreLib: THandle;
begin
  // Pour Windows 8.1 et plus
  ShcoreLib := LoadLibrary('Shcore.dll');
  if ShcoreLib <> 0 then
  begin
    SetProcessDpiAwareness := GetProcAddress(ShcoreLib, 'SetProcessDpiAwareness');
    if Assigned(SetProcessDpiAwareness) then
      SetProcessDpiAwareness(2); // PROCESS_PER_MONITOR_DPI_AWARE
    FreeLibrary(ShcoreLib);
  end;
end;
```

### Détecter les changements de DPI (Windows)

```pascal
type
  TForm1 = class(TForm)
  protected
    procedure WMDpiChanged(var Message: TMessage); message WM_DPICHANGED;
  end;

procedure TForm1.WMDpiChanged(var Message: TMessage);  
var
  NewDPI: Integer;
  OldDPI: Integer;
  ScaleFactor: Double;
begin
  NewDPI := HiWord(Message.WParam);
  OldDPI := Screen.PixelsPerInch;
  ScaleFactor := NewDPI / OldDPI;

  // Note : Screen.PixelsPerInch est en lecture seule dans Lazarus.
  // La mise à l'échelle se fait via ScaleBy.
  ScaleBy(Round(ScaleFactor * 100), 100);
end;
```

## Spécificités Linux/Ubuntu

### Configuration pour GTK

Sous Linux avec GTK, la mise à l'échelle est gérée par l'environnement de bureau :

```pascal
procedure ConfigurerDPILinux;  
var
  ScaleFactor: string;
begin
  // Lire la variable d'environnement GDK_SCALE
  ScaleFactor := GetEnvironmentVariable('GDK_SCALE');
  if ScaleFactor <> '' then
  begin
    // Utiliser ce facteur pour adapter l'interface
    ShowMessage('Facteur d''échelle GTK : ' + ScaleFactor);
  end;
end;
```

### Paramètres GNOME/KDE

Les utilisateurs configurent le DPI dans les paramètres système :
- **GNOME** : Paramètres → Affichage → Échelle
- **KDE** : Configuration système → Affichage → Échelle globale

### Forcer un DPI spécifique (Linux)

```bash
# Lancer l'application avec un DPI forcé
GDK_SCALE=2 ./monapplication
```

Ou depuis le code Pascal (sous Linux, avec l'unité `unix`) :

```pascal
uses unix;
// ...
fpSetEnv('GDK_SCALE', '2', 1);
```

## Gestion multi-moniteur

### Détecter les différents DPI

```pascal
procedure DetecterDPIMoniteurs;  
var
  i: Integer;
  Moniteur: TMonitor;
begin
  for i := 0 to Screen.MonitorCount - 1 do
  begin
    Moniteur := Screen.Monitors[i];
    Memo1.Lines.Add(Format('Moniteur %d : %d',
      [i, Moniteur.MonitorNum]));
    // Note: Le DPI par moniteur nécessite des API spécifiques
  end;
end;
```

### Adapter lors du changement de moniteur

```pascal
procedure TForm1.FormMonitorChanged(Sender: TObject);  
begin
  // Recalculer la mise à l'échelle
  if Scaled then
  begin
    // Lazarus gère automatiquement si Scaled = True
    // Sinon, implémenter la logique manuelle
  end;
end;
```

## Composants personnalisés et High-DPI

### Créer un composant DPI-aware

```pascal
type
  TMonBoutonDPI = class(TCustomControl)
  private
    FBaseDPI: Integer;
    FIcone: TPicture;
    procedure SetIcone(Value: TPicture);
  protected
    procedure Paint; override;
    procedure DoScaleChange; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ScaleForDPI(NewDPI: Integer);
  published
    property Icone: TPicture read FIcone write SetIcone;
  end;

constructor TMonBoutonDPI.Create(AOwner: TComponent);  
begin
  inherited Create(AOwner);
  FBaseDPI := 96;
  FIcone := TPicture.Create;
end;

procedure TMonBoutonDPI.ScaleForDPI(NewDPI: Integer);  
var
  Ratio: Double;
begin
  if FBaseDPI = 0 then FBaseDPI := 96;
  Ratio := NewDPI / FBaseDPI;

  Width := Round(Width * Ratio);
  Height := Round(Height * Ratio);

  DoScaleChange;
  FBaseDPI := NewDPI;
end;

procedure TMonBoutonDPI.Paint;  
var
  IconSize: Integer;
begin
  inherited Paint;

  // Dessiner avec prise en compte du DPI
  IconSize := MulDiv(16, Screen.PixelsPerInch, 96);

  if Assigned(FIcone.Graphic) then
  begin
    Canvas.StretchDraw(
      Rect(2, 2, IconSize + 2, IconSize + 2),
      FIcone.Graphic
    );
  end;
end;
```

## Anchors et Align avec High-DPI

### Utilisation correcte des Anchors

```pascal
// Les anchors fonctionnent bien avec le High-DPI
Button1.Anchors := [akRight, akBottom]; // Reste en bas à droite  
Panel1.Align := alClient; // Remplit tout l'espace

// Éviter les positions absolues
Button1.Left := 500; // Problématique en High-DPI
// Préférer
Button1.AnchorSideRight.Control := Form1;  
Button1.AnchorSideRight.Side := asrRight;  
Button1.BorderSpacing.Right := TDPIHelper.Scale(10);
```

### Layouts responsifs

```pascal
// Utiliser TFlowPanel pour disposition automatique
FlowPanel1.Align := alTop;  
FlowPanel1.AutoSize := True;  
FlowPanel1.AutoWrap := True;

// Les contrôles s'adaptent automatiquement
for i := 0 to 5 do  
begin
  with TButton.Create(FlowPanel1) do
  begin
    Parent := FlowPanel1;
    Width := TDPIHelper.Scale(100);
    Height := TDPIHelper.Scale(30);
    Caption := 'Bouton ' + IntToStr(i);
  end;
end;
```

## Tests et débogage

### Simuler différents DPI

```pascal
procedure SimulerDPI(NouveauDPI: Integer);  
begin
  {$IFDEF DEBUG}
  // Note : Screen.PixelsPerInch est en lecture seule dans Lazarus.
  // On simule en appliquant le ratio directement via ScaleBy.
  ShowMessage(Format('DPI simulé : %d', [NouveauDPI]));

  Application.ProcessMessages;
  Form1.ScaleBy(NouveauDPI, Screen.PixelsPerInch);
  {$ENDIF}
end;
```

### Menu de test DPI

```pascal
procedure TForm1.CreerMenuTestDPI;  
var
  MenuItem: TMenuItem;
  DPIValues: array[0..3] of Integer = (96, 120, 144, 192);
  i: Integer;
begin
  for i := 0 to High(DPIValues) do
  begin
    MenuItem := TMenuItem.Create(MenuTest);
    MenuItem.Caption := Format('%d DPI (%d%%)',
      [DPIValues[i], DPIValues[i] * 100 div 96]);
    MenuItem.Tag := DPIValues[i];
    MenuItem.OnClick := @MenuDPIClick;
    MenuTest.Add(MenuItem);
  end;
end;

procedure TForm1.MenuDPIClick(Sender: TObject);  
begin
  SimulerDPI(TMenuItem(Sender).Tag);
end;
```

## Problèmes courants et solutions

### Problème 1 : Texte coupé

**Symptôme** : Le texte est tronqué dans les labels ou boutons

**Solution** :
```pascal
// Activer l'auto-dimensionnement
Label1.AutoSize := True;  
Button1.AutoSize := True;

// Ou prévoir une marge
Button1.Width := Canvas.TextWidth(Button1.Caption) + TDPIHelper.Scale(20);
```

### Problème 2 : Images floues

**Symptôme** : Les icônes sont pixellisées en High-DPI

**Solution** :
```pascal
// Utiliser des images haute résolution
// ou des formats vectoriels (SVG)
// ou plusieurs versions d'images
```

### Problème 3 : Espacement incohérent

**Symptôme** : Les marges ne s'adaptent pas correctement

**Solution** :
```pascal
// Toujours utiliser la mise à l'échelle pour les espacements
BorderSpacing.Around := TDPIHelper.Scale(8);
// Au lieu de valeurs fixes
BorderSpacing.Around := 8; // Éviter
```

### Problème 4 : Formulaires trop grands

**Symptôme** : Le formulaire dépasse l'écran en High-DPI

**Solution** :
```pascal
procedure TForm1.FormCreate(Sender: TObject);  
begin
  // Limiter la taille maximale
  Constraints.MaxWidth := Screen.Width - 50;
  Constraints.MaxHeight := Screen.Height - 50;

  // Ou utiliser un pourcentage de l'écran
  Width := Round(Screen.Width * 0.8);
  Height := Round(Screen.Height * 0.8);
end;
```

## Bonnes pratiques récapitulatives

### 1. Configuration initiale
- Toujours activer `Scaled := True` sur les formulaires
- Configurer le manifest DPI pour Windows
- Tester sur différentes résolutions

### 2. Design adaptatif
- Utiliser les Anchors et Align plutôt que les positions absolues
- Prévoir des marges suffisantes
- Activer AutoSize quand c'est pertinent

### 3. Images et ressources
- Préparer plusieurs résolutions d'images
- Privilégier les formats vectoriels
- Adapter dynamiquement les TImageList

### 4. Code robuste
- Utiliser des fonctions de mise à l'échelle helpers
- Éviter les valeurs en pixels codées en dur
- Tester régulièrement sur différents DPI

### 5. Tests
- Tester sur écrans réels High-DPI
- Simuler différents DPI pendant le développement
- Vérifier le comportement multi-moniteur

## Exemple complet : Application DPI-aware

```pascal
unit MainForm;

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    Label1: TLabel;
    Edit1: TEdit;
    procedure FormCreate(Sender: TObject);
  private
    procedure ConfigurerPourDPI;
  public
  end;

var
  Form1: TForm1;

implementation

procedure TForm1.FormCreate(Sender: TObject);  
begin
  // Configuration de base
  Scaled := True;

  // Afficher les infos DPI
  Caption := Format('Application DPI-aware - %d DPI (%.0f%%)',
    [Screen.PixelsPerInch, Screen.PixelsPerInch / 96 * 100]);

  ConfigurerPourDPI;
end;

procedure TForm1.ConfigurerPourDPI;  
var
  ScaleFactor: Double;
begin
  ScaleFactor := Screen.PixelsPerInch / 96;

  // Adapter les marges
  Panel1.BorderSpacing.Around := Round(8 * ScaleFactor);

  // Adapter la police si nécessaire
  if ScaleFactor > 1.5 then
  begin
    Font.Size := Round(Font.Size * 1.1);
  end;

  // Configuration des anchors pour responsive
  Button1.Anchors := [akRight, akBottom];
  Edit1.Anchors := [akLeft, akTop, akRight];

  // S'assurer que le formulaire reste visible
  if Width > Screen.Width then
    Width := Screen.Width - 50;
  if Height > Screen.Height then
    Height := Screen.Height - 50;
end;

end.
```

## Conclusion

La gestion du High-DPI est essentielle pour créer des applications modernes qui s'affichent correctement sur tous les écrans. Les points clés à retenir :

1. **Activez toujours** `Scaled := True` pour bénéficier de la mise à l'échelle automatique
2. **Testez régulièrement** sur différentes résolutions et DPI
3. **Utilisez des helpers** pour calculer les dimensions adaptées
4. **Préparez vos ressources** en plusieurs résolutions
5. **Privilégiez les layouts flexibles** avec Anchors et Align

Avec ces techniques, vos applications Lazarus seront nettes et lisibles sur tous les écrans, des vieux moniteurs 1080p aux écrans 4K modernes, sous Windows comme sous Linux.

⏭️ [Gestion des différences d'interface OS](/04-framework-lcl/12-gestion-differences-interface-os.md)
