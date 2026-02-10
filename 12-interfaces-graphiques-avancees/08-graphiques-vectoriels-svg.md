🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 12.8 Graphiques vectoriels SVG

## Introduction

**SVG** (Scalable Vector Graphics) est un format de graphiques vectoriels basé sur XML. Contrairement aux images bitmap (PNG, JPEG) qui sont composées de pixels, les images SVG sont définies par des formes géométriques qui peuvent être agrandies à l'infini sans perte de qualité.

### Pourquoi utiliser SVG ?

#### Avantages
- **Qualité parfaite** : Aucune perte lors du redimensionnement
- **Taille réduite** : Fichiers souvent plus petits que les bitmaps
- **Modifiable** : Format texte facilement éditable
- **Interactif** : Peut contenir des scripts et animations
- **Accessible** : Indexable par les moteurs de recherche

#### Cas d'usage
- Icônes et logos
- Diagrammes et graphiques
- Cartes et plans
- Illustrations techniques
- Interfaces utilisateur responsive
- Export de visualisations

### SVG vs Bitmap

```
┌─────────────────────────────────────────────────────────┐
│              SVG (Vectoriel)                            │
│  • Redimensionnable sans perte                          │
│  • Taille fichier indépendante de la résolution         │
│  • Idéal pour : logos, icônes, diagrammes               │
│  • Format : XML (texte)                                 │
└─────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────┐
│              Bitmap (Raster)                            │
│  • Qualité dépend de la résolution                      │
│  • Taille augmente avec la résolution                   │
│  • Idéal pour : photos, textures complexes              │
│  • Formats : PNG, JPEG, BMP                             │
└─────────────────────────────────────────────────────────┘
```

## Structure d'un fichier SVG

### Document SVG minimal

```xml
<?xml version="1.0" encoding="UTF-8"?>
<svg width="200" height="200" xmlns="http://www.w3.org/2000/svg">
  <!-- Votre contenu ici -->
  <circle cx="100" cy="100" r="50" fill="blue"/>
</svg>
```

### Éléments de base

```xml
<!-- Rectangle -->
<rect x="10" y="10" width="100" height="50" fill="red"/>

<!-- Cercle -->
<circle cx="50" cy="50" r="40" fill="green"/>

<!-- Ellipse -->
<ellipse cx="100" cy="100" rx="80" ry="40" fill="yellow"/>

<!-- Ligne -->
<line x1="0" y1="0" x2="100" y2="100" stroke="black" stroke-width="2"/>

<!-- Polyligne -->
<polyline points="0,0 50,25 50,75 100,100" stroke="blue" fill="none"/>

<!-- Polygone -->
<polygon points="50,0 100,50 50,100 0,50" fill="purple"/>

<!-- Texte -->
<text x="50" y="50" font-size="20" fill="black">Hello SVG</text>

<!-- Chemin (path) -->
<path d="M 10 10 L 90 90 L 10 90 Z" fill="orange"/>
```

## Lecture et affichage de SVG dans Lazarus

### Méthode 1 : Utiliser TBGRASVGviewer

```pascal
unit SVGViewer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Menus, BGRASVG, BGRABitmap, BGRABitmapTypes;

type
  TfrmSVGViewer = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FSVG: TBGRASVG;
    FImagePanel: TPanel;
    FPaintBox: TPaintBox;

    procedure SetupInterface;
    procedure OnPaintBoxPaint(Sender: TObject);
    procedure OnOpenSVGClick(Sender: TObject);

  public
    procedure LoadSVGFile(const AFileName: string);
    procedure LoadSVGString(const ASVGContent: string);
    procedure RenderSVG(AWidth, AHeight: Integer);
  end;

var
  frmSVGViewer: TfrmSVGViewer;

implementation

{$R *.lfm}

procedure TfrmSVGViewer.FormCreate(Sender: TObject);  
begin
  Caption := 'Visualiseur SVG';
  Width := 800;
  Height := 600;
  Position := poScreenCenter;

  FSVG := TBGRASVG.Create;

  SetupInterface;
end;

procedure TfrmSVGViewer.FormDestroy(Sender: TObject);  
begin
  FSVG.Free;
end;

procedure TfrmSVGViewer.SetupInterface;  
var
  MainMenu: TMainMenu;
  MenuItem, SubItem: TMenuItem;
begin
  // Menu
  MainMenu := TMainMenu.Create(Self);
  Self.Menu := MainMenu;

  MenuItem := TMenuItem.Create(MainMenu);
  MenuItem.Caption := '&Fichier';
  MainMenu.Items.Add(MenuItem);

  SubItem := TMenuItem.Create(MenuItem);
  SubItem.Caption := '&Ouvrir SVG...';
  SubItem.OnClick := @OnOpenSVGClick;
  MenuItem.Add(SubItem);

  // Panel pour l'affichage
  FImagePanel := TPanel.Create(Self);
  FImagePanel.Parent := Self;
  FImagePanel.Align := alClient;
  FImagePanel.BevelOuter := bvNone;
  FImagePanel.Color := clWhite;

  // PaintBox pour le rendu
  FPaintBox := TPaintBox.Create(FImagePanel);
  FPaintBox.Parent := FImagePanel;
  FPaintBox.Align := alClient;
  FPaintBox.OnPaint := @OnPaintBoxPaint;
end;

procedure TfrmSVGViewer.OnOpenSVGClick(Sender: TObject);  
var
  OpenDialog: TOpenDialog;
begin
  OpenDialog := TOpenDialog.Create(nil);
  try
    OpenDialog.Filter := 'Fichiers SVG (*.svg)|*.svg|Tous les fichiers (*.*)|*.*';
    if OpenDialog.Execute then
      LoadSVGFile(OpenDialog.FileName);
  finally
    OpenDialog.Free;
  end;
end;

procedure TfrmSVGViewer.LoadSVGFile(const AFileName: string);  
begin
  if FileExists(AFileName) then
  begin
    FSVG.LoadFromFile(AFileName);
    Caption := 'Visualiseur SVG - ' + ExtractFileName(AFileName);
    FPaintBox.Invalidate;
  end;
end;

procedure TfrmSVGViewer.LoadSVGString(const ASVGContent: string);  
var
  Stream: TStringStream;
begin
  Stream := TStringStream.Create(ASVGContent);
  try
    FSVG.LoadFromStream(Stream);
    FPaintBox.Invalidate;
  finally
    Stream.Free;
  end;
end;

procedure TfrmSVGViewer.OnPaintBoxPaint(Sender: TObject);  
var
  Bitmap: TBGRABitmap;
begin
  if not Assigned(FSVG) then Exit;

  Bitmap := TBGRABitmap.Create(FPaintBox.Width, FPaintBox.Height, BGRAWhite);
  try
    // Dessiner le SVG sur le bitmap
    FSVG.StretchDraw(Bitmap.Canvas2D, 0, 0, FPaintBox.Width, FPaintBox.Height);

    // Afficher le bitmap
    Bitmap.Draw(FPaintBox.Canvas, 0, 0, True);
  finally
    Bitmap.Free;
  end;
end;

procedure TfrmSVGViewer.RenderSVG(AWidth, AHeight: Integer);  
begin
  FPaintBox.Width := AWidth;
  FPaintBox.Height := AHeight;
  FPaintBox.Invalidate;
end;

end.
```

## Création de SVG par programmation

### Générateur SVG simple

```pascal
unit SVGGenerator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics;

type
  TSVGGenerator = class
  private
    FSVG: TStringList;
    FWidth: Integer;
    FHeight: Integer;

    function ColorToSVG(AColor: TColor): string;
  public
    constructor Create(AWidth, AHeight: Integer);
    destructor Destroy; override;

    // Formes de base
    procedure AddRectangle(X, Y, Width, Height: Integer;
                          FillColor, StrokeColor: TColor;
                          StrokeWidth: Integer = 1);
    procedure AddCircle(CX, CY, Radius: Integer;
                       FillColor, StrokeColor: TColor;
                       StrokeWidth: Integer = 1);
    procedure AddEllipse(CX, CY, RX, RY: Integer;
                        FillColor, StrokeColor: TColor;
                        StrokeWidth: Integer = 1);
    procedure AddLine(X1, Y1, X2, Y2: Integer;
                     StrokeColor: TColor; StrokeWidth: Integer = 1);
    procedure AddText(X, Y: Integer; const AText: string;
                     FontSize: Integer; TextColor: TColor);
    procedure AddPath(const APathData: string;
                     FillColor, StrokeColor: TColor;
                     StrokeWidth: Integer = 1);

    // Groupes et transformations
    procedure BeginGroup(const AID: string = '');
    procedure EndGroup;
    procedure AddTransform(const ATransform: string);

    // Export
    function GetSVG: string;
    procedure SaveToFile(const AFileName: string);
    procedure Clear;
  end;

implementation

constructor TSVGGenerator.Create(AWidth, AHeight: Integer);  
begin
  inherited Create;
  FSVG := TStringList.Create;
  FWidth := AWidth;
  FHeight := AHeight;

  // En-tête SVG
  FSVG.Add('<?xml version="1.0" encoding="UTF-8"?>');
  FSVG.Add(Format('<svg width="%d" height="%d" xmlns="http://www.w3.org/2000/svg">',
                  [FWidth, FHeight]));
end;

destructor TSVGGenerator.Destroy;  
begin
  FSVG.Free;
  inherited Destroy;
end;

function TSVGGenerator.ColorToSVG(AColor: TColor): string;  
var
  R, G, B: Byte;
begin
  if AColor = clNone then
    Result := 'none'
  else
  begin
    RedGreenBlue(ColorToRGB(AColor), R, G, B);
    Result := Format('rgb(%d,%d,%d)', [R, G, B]);
  end;
end;

procedure TSVGGenerator.AddRectangle(X, Y, Width, Height: Integer;
  FillColor, StrokeColor: TColor; StrokeWidth: Integer);
var
  Line: string;
begin
  Line := Format('<rect x="%d" y="%d" width="%d" height="%d" fill="%s" stroke="%s" stroke-width="%d"/>',
                 [X, Y, Width, Height,
                  ColorToSVG(FillColor),
                  ColorToSVG(StrokeColor),
                  StrokeWidth]);
  FSVG.Add('  ' + Line);
end;

procedure TSVGGenerator.AddCircle(CX, CY, Radius: Integer;
  FillColor, StrokeColor: TColor; StrokeWidth: Integer);
var
  Line: string;
begin
  Line := Format('<circle cx="%d" cy="%d" r="%d" fill="%s" stroke="%s" stroke-width="%d"/>',
                 [CX, CY, Radius,
                  ColorToSVG(FillColor),
                  ColorToSVG(StrokeColor),
                  StrokeWidth]);
  FSVG.Add('  ' + Line);
end;

procedure TSVGGenerator.AddEllipse(CX, CY, RX, RY: Integer;
  FillColor, StrokeColor: TColor; StrokeWidth: Integer);
var
  Line: string;
begin
  Line := Format('<ellipse cx="%d" cy="%d" rx="%d" ry="%d" fill="%s" stroke="%s" stroke-width="%d"/>',
                 [CX, CY, RX, RY,
                  ColorToSVG(FillColor),
                  ColorToSVG(StrokeColor),
                  StrokeWidth]);
  FSVG.Add('  ' + Line);
end;

procedure TSVGGenerator.AddLine(X1, Y1, X2, Y2: Integer;
  StrokeColor: TColor; StrokeWidth: Integer);
var
  Line: string;
begin
  Line := Format('<line x1="%d" y1="%d" x2="%d" y2="%d" stroke="%s" stroke-width="%d"/>',
                 [X1, Y1, X2, Y2, ColorToSVG(StrokeColor), StrokeWidth]);
  FSVG.Add('  ' + Line);
end;

procedure TSVGGenerator.AddText(X, Y: Integer; const AText: string;
  FontSize: Integer; TextColor: TColor);
var
  Line: string;
begin
  Line := Format('<text x="%d" y="%d" font-size="%d" fill="%s">%s</text>',
                 [X, Y, FontSize, ColorToSVG(TextColor), AText]);
  FSVG.Add('  ' + Line);
end;

procedure TSVGGenerator.AddPath(const APathData: string;
  FillColor, StrokeColor: TColor; StrokeWidth: Integer);
var
  Line: string;
begin
  Line := Format('<path d="%s" fill="%s" stroke="%s" stroke-width="%d"/>',
                 [APathData,
                  ColorToSVG(FillColor),
                  ColorToSVG(StrokeColor),
                  StrokeWidth]);
  FSVG.Add('  ' + Line);
end;

procedure TSVGGenerator.BeginGroup(const AID: string);  
begin
  if AID <> '' then
    FSVG.Add(Format('  <g id="%s">', [AID]))
  else
    FSVG.Add('  <g>');
end;

procedure TSVGGenerator.EndGroup;  
begin
  FSVG.Add('  </g>');
end;

procedure TSVGGenerator.AddTransform(const ATransform: string);  
begin
  FSVG.Add(Format('  <g transform="%s">', [ATransform]));
end;

function TSVGGenerator.GetSVG: string;  
begin
  Result := FSVG.Text + '</svg>';
end;

procedure TSVGGenerator.SaveToFile(const AFileName: string);  
var
  Output: TStringList;
begin
  Output := TStringList.Create;
  try
    Output.Text := GetSVG;
    Output.SaveToFile(AFileName);
  finally
    Output.Free;
  end;
end;

procedure TSVGGenerator.Clear;  
begin
  FSVG.Clear;
  FSVG.Add('<?xml version="1.0" encoding="UTF-8"?>');
  FSVG.Add(Format('<svg width="%d" height="%d" xmlns="http://www.w3.org/2000/svg">',
                  [FWidth, FHeight]));
end;

end.
```

## Exemples d'utilisation

### Créer un logo simple

```pascal
procedure CreateSimpleLogo;  
var
  SVG: TSVGGenerator;
begin
  SVG := TSVGGenerator.Create(200, 200);
  try
    // Fond
    SVG.AddRectangle(0, 0, 200, 200, clWhite, clNone);

    // Cercle extérieur
    SVG.AddCircle(100, 100, 80, clSkyBlue, clNavy, 3);

    // Cercle intérieur
    SVG.AddCircle(100, 100, 50, clYellow, clOrange, 2);

    // Texte
    SVG.AddText(70, 110, 'LOGO', 24, clNavy);

    // Sauvegarder
    SVG.SaveToFile('logo.svg');
  finally
    SVG.Free;
  end;
end;
```

### Créer un graphique en barres

```pascal
procedure CreateBarChart;  
var
  SVG: TSVGGenerator;
  i: Integer;
  Values: array[0..4] of Integer = (45, 78, 62, 90, 55);
  Colors: array[0..4] of TColor = (clRed, clGreen, clBlue, clYellow, clPurple);
  X, BarWidth, MaxValue: Integer;
  BarHeight: Double;
begin
  SVG := TSVGGenerator.Create(500, 300);
  try
    // Fond blanc
    SVG.AddRectangle(0, 0, 500, 300, clWhite, clNone);

    // Titre
    SVG.AddText(150, 30, 'Graphique en Barres', 20, clBlack);

    // Axes
    SVG.AddLine(50, 50, 50, 250, clBlack, 2);  // Axe Y
    SVG.AddLine(50, 250, 450, 250, clBlack, 2); // Axe X

    // Barres
    BarWidth := 60;
    MaxValue := 100;

    for i := 0 to High(Values) do
    begin
      X := 80 + (i * 80);
      BarHeight := (Values[i] / MaxValue) * 180;

      // Barre
      SVG.AddRectangle(X, Round(250 - BarHeight), BarWidth, Round(BarHeight),
                       Colors[i], clBlack, 1);

      // Valeur
      SVG.AddText(X + 15, Round(240 - BarHeight), IntToStr(Values[i]),
                  14, clBlack);

      // Label
      SVG.AddText(X + 10, 270, 'Cat' + IntToStr(i + 1), 12, clBlack);
    end;

    SVG.SaveToFile('chart.svg');
  finally
    SVG.Free;
  end;
end;
```

### Créer un diagramme de flux

```pascal
procedure CreateFlowchart;  
var
  SVG: TSVGGenerator;
begin
  SVG := TSVGGenerator.Create(400, 500);
  try
    // Fond
    SVG.AddRectangle(0, 0, 400, 500, RGB(240, 240, 240), clNone);

    // Début (ellipse)
    SVG.AddEllipse(200, 50, 80, 30, clLime, clBlack, 2);
    SVG.AddText(175, 55, 'Début', 14, clBlack);

    // Flèche
    SVG.AddLine(200, 80, 200, 120, clBlack, 2);

    // Processus 1 (rectangle)
    SVG.AddRectangle(120, 120, 160, 60, clSkyBlue, clBlack, 2);
    SVG.AddText(145, 155, 'Processus 1', 14, clBlack);

    // Flèche
    SVG.AddLine(200, 180, 200, 220, clBlack, 2);

    // Décision (losange)
    SVG.AddPath('M 200 220 L 280 280 L 200 340 L 120 280 Z',
                clYellow, clBlack, 2);
    SVG.AddText(170, 285, 'Condition?', 12, clBlack);

    // Flèche Oui
    SVG.AddLine(280, 280, 330, 280, clBlack, 2);
    SVG.AddText(300, 275, 'Oui', 12, clBlack);

    // Processus 2
    SVG.AddRectangle(330, 250, 60, 60, clSkyBlue, clBlack, 2);
    SVG.AddText(340, 285, 'Action', 12, clBlack);

    // Flèche Non
    SVG.AddLine(200, 340, 200, 380, clBlack, 2);
    SVG.AddText(210, 360, 'Non', 12, clBlack);

    // Fin (ellipse)
    SVG.AddEllipse(200, 420, 80, 30, clRed, clBlack, 2);
    SVG.AddText(180, 425, 'Fin', 14, clWhite);

    SVG.SaveToFile('flowchart.svg');
  finally
    SVG.Free;
  end;
end;
```

## Chemins SVG (Path)

Les chemins sont l'élément le plus puissant de SVG, permettant de dessiner n'importe quelle forme.

### Commandes de chemin

```
M = Move To (déplacer vers)  
L = Line To (ligne vers)  
H = Horizontal Line To  
V = Vertical Line To  
C = Cubic Bézier Curve  
S = Smooth Cubic Bézier Curve  
Q = Quadratic Bézier Curve  
T = Smooth Quadratic Bézier Curve  
A = Elliptical Arc  
Z = Close Path (fermer le chemin)
```

### Exemples de chemins

```pascal
procedure CreatePathExamples;  
var
  SVG: TSVGGenerator;
begin
  SVG := TSVGGenerator.Create(600, 400);
  try
    // Triangle
    SVG.AddPath('M 50 50 L 150 50 L 100 150 Z', clRed, clBlack, 2);

    // Étoile
    SVG.AddPath('M 250 50 L 270 100 L 320 100 L 280 130 L 300 180 L 250 150 L 200 180 L 220 130 L 180 100 L 230 100 Z',
                clYellow, clOrange, 2);

    // Courbe de Bézier
    SVG.AddPath('M 50 250 Q 150 150 250 250 T 450 250',
                clNone, clBlue, 3);

    SVG.SaveToFile('paths.svg');
  finally
    SVG.Free;
  end;
end;
```

## Transformations

### Types de transformations

```pascal
procedure CreateTransformations;  
var
  SVG: TSVGGenerator;
begin
  SVG := TSVGGenerator.Create(600, 400);
  try
    // Rectangle original
    SVG.AddRectangle(50, 50, 100, 60, clLightBlue, clBlue, 2);

    // Translation
    SVG.BeginGroup;
    SVG.AddTransform('translate(150, 0)');
    SVG.AddRectangle(50, 50, 100, 60, clLightGreen, clGreen, 2);
    SVG.EndGroup;

    // Rotation
    SVG.BeginGroup;
    SVG.AddTransform('rotate(45 350 80)');
    SVG.AddRectangle(300, 50, 100, 60, clLightCoral, clRed, 2);
    SVG.EndGroup;

    // Échelle
    SVG.BeginGroup;
    SVG.AddTransform('scale(1.5)');
    SVG.AddRectangle(50, 150, 100, 60, clLightYellow, clOrange, 2);
    SVG.EndGroup;

    // Transformation multiple
    SVG.BeginGroup;
    SVG.AddTransform('translate(300, 200) rotate(30) scale(0.8)');
    SVG.AddRectangle(0, 0, 100, 60, clLavender, clPurple, 2);
    SVG.EndGroup;

    SVG.SaveToFile('transformations.svg');
  finally
    SVG.Free;
  end;
end;
```

## Gradients et motifs

### Gradient linéaire

```pascal
procedure CreateGradientExample;  
var
  SVG: TStringList;
begin
  SVG := TStringList.Create;
  try
    SVG.Add('<?xml version="1.0" encoding="UTF-8"?>');
    SVG.Add('<svg width="400" height="300" xmlns="http://www.w3.org/2000/svg">');

    // Définition du gradient
    SVG.Add('  <defs>');
    SVG.Add('    <linearGradient id="grad1" x1="0%" y1="0%" x2="100%" y2="0%">');
    SVG.Add('      <stop offset="0%" style="stop-color:rgb(255,255,0);stop-opacity:1" />');
    SVG.Add('      <stop offset="100%" style="stop-color:rgb(255,0,0);stop-opacity:1" />');
    SVG.Add('    </linearGradient>');
    SVG.Add('  </defs>');

    // Rectangle avec gradient
    SVG.Add('  <rect x="50" y="50" width="300" height="200" fill="url(#grad1)"/>');

    SVG.Add('</svg>');
    SVG.SaveToFile('gradient.svg');
  finally
    SVG.Free;
  end;
end;
```

### Gradient radial

```pascal
procedure CreateRadialGradient;  
var
  SVG: TStringList;
begin
  SVG := TStringList.Create;
  try
    SVG.Add('<?xml version="1.0" encoding="UTF-8"?>');
    SVG.Add('<svg width="400" height="400" xmlns="http://www.w3.org/2000/svg">');

    // Gradient radial
    SVG.Add('  <defs>');
    SVG.Add('    <radialGradient id="grad2">');
    SVG.Add('      <stop offset="0%" style="stop-color:rgb(255,255,255);stop-opacity:1" />');
    SVG.Add('      <stop offset="100%" style="stop-color:rgb(0,0,255);stop-opacity:1" />');
    SVG.Add('    </radialGradient>');
    SVG.Add('  </defs>');

    // Cercle avec gradient
    SVG.Add('  <circle cx="200" cy="200" r="150" fill="url(#grad2)"/>');

    SVG.Add('</svg>');
    SVG.SaveToFile('radial_gradient.svg');
  finally
    SVG.Free;
  end;
end;
```

## Conversion Canvas vers SVG

Convertir des dessins Canvas en SVG :

```pascal
unit CanvasToSVG;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, SVGGenerator;

type
  TCanvasRecorder = class
  private
    FSVG: TSVGGenerator;
    FRecording: Boolean;
  public
    constructor Create(AWidth, AHeight: Integer);
    destructor Destroy; override;

    procedure StartRecording;
    procedure StopRecording;

    // Méthodes de dessin
    procedure MoveTo(X, Y: Integer);
    procedure LineTo(X, Y: Integer);
    procedure Rectangle(X1, Y1, X2, Y2: Integer);
    procedure Ellipse(X1, Y1, X2, Y2: Integer);
    procedure TextOut(X, Y: Integer; const Text: string);

    function GetSVG: string;
    procedure SaveToFile(const AFileName: string);

    property Recording: Boolean read FRecording;
  end;

implementation

constructor TCanvasRecorder.Create(AWidth, AHeight: Integer);  
begin
  inherited Create;
  FSVG := TSVGGenerator.Create(AWidth, AHeight);
  FRecording := False;
end;

destructor TCanvasRecorder.Destroy;  
begin
  FSVG.Free;
  inherited Destroy;
end;

procedure TCanvasRecorder.StartRecording;  
begin
  FRecording := True;
  FSVG.Clear;
end;

procedure TCanvasRecorder.StopRecording;  
begin
  FRecording := False;
end;

procedure TCanvasRecorder.Rectangle(X1, Y1, X2, Y2: Integer);  
begin
  if FRecording then
    FSVG.AddRectangle(X1, Y1, X2 - X1, Y2 - Y1, clWhite, clBlack, 1);
end;

procedure TCanvasRecorder.Ellipse(X1, Y1, X2, Y2: Integer);  
var
  CX, CY, RX, RY: Integer;
begin
  if FRecording then
  begin
    CX := (X1 + X2) div 2;
    CY := (Y1 + Y2) div 2;
    RX := Abs(X2 - X1) div 2;
    RY := Abs(Y2 - Y1) div 2;
    FSVG.AddEllipse(CX, CY, RX, RY, clWhite, clBlack, 1);
  end;
end;

procedure TCanvasRecorder.TextOut(X, Y: Integer; const Text: string);  
begin
  if FRecording then
    FSVG.AddText(X, Y, Text, 12, clBlack);
end;

function TCanvasRecorder.GetSVG: string;  
begin
  Result := FSVG.GetSVG;
end;

procedure TCanvasRecorder.SaveToFile(const AFileName: string);  
begin
  FSVG.SaveToFile(AFileName);
end;

procedure TCanvasRecorder.MoveTo(X, Y: Integer);  
begin
  // Pour implémenter le dessin de chemins
  // Stocker la position actuelle
end;

procedure TCanvasRecorder.LineTo(X, Y: Integer);  
begin
  // Dessiner une ligne depuis la dernière position
  if FRecording then
  begin
    // À implémenter avec gestion de la position courante
  end;
end;

end.
```

## Éditeur SVG interactif

Créons un éditeur simple pour créer des SVG de manière interactive :

```pascal
unit SVGEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Menus, Buttons, ComCtrls, SVGGenerator;

type
  TDrawingTool = (dtNone, dtRectangle, dtCircle, dtLine, dtText);

  TSVGShape = class
    ShapeType: TDrawingTool;
    X1, Y1, X2, Y2: Integer;
    FillColor: TColor;
    StrokeColor: TColor;
    StrokeWidth: Integer;
    Text: string;
  end;

  TfrmSVGEditor = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FCanvas: TPaintBox;
    FShapes: TList;
    FCurrentTool: TDrawingTool;
    FDrawing: Boolean;
    FStartPoint: TPoint;
    FCurrentShape: TSVGShape;
    FFillColor: TColor;
    FStrokeColor: TColor;
    FStrokeWidth: Integer;

    procedure SetupInterface;
    procedure CreateToolbar;
    procedure CreateCanvas;
    procedure CreatePropertiesPanel;

    procedure OnCanvasMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OnCanvasMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure OnCanvasMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OnCanvasPaint(Sender: TObject);

    procedure OnToolSelect(Sender: TObject);
    procedure OnColorChange(Sender: TObject);
    procedure OnExportSVGClick(Sender: TObject);
    procedure OnClearAllClick(Sender: TObject);
    procedure OnWidthSpinChange(Sender: TObject);

    procedure DrawShape(ACanvas: TCanvas; AShape: TSVGShape);
    procedure ExportToSVG(const AFileName: string);

  public
    procedure ClearCanvas;
  end;

var
  frmSVGEditor: TfrmSVGEditor;

implementation

{$R *.lfm}

procedure TfrmSVGEditor.FormCreate(Sender: TObject);  
begin
  Caption := 'Éditeur SVG';
  Width := 1000;
  Height := 700;
  Position := poScreenCenter;

  FShapes := TList.Create;
  FCurrentTool := dtNone;
  FDrawing := False;
  FFillColor := clWhite;
  FStrokeColor := clBlack;
  FStrokeWidth := 2;

  SetupInterface;
end;

procedure TfrmSVGEditor.FormDestroy(Sender: TObject);  
var
  i: Integer;
begin
  for i := 0 to FShapes.Count - 1 do
    TSVGShape(FShapes[i]).Free;
  FShapes.Free;
end;

procedure TfrmSVGEditor.SetupInterface;  
begin
  CreateToolbar;
  CreatePropertiesPanel;
  CreateCanvas;
end;

procedure TfrmSVGEditor.CreateToolbar;  
var
  Toolbar: TToolBar;
  Btn: TToolButton;

  procedure AddToolButton(ACaption: string; ATool: TDrawingTool);
  begin
    Btn := TToolButton.Create(Toolbar);
    Btn.Parent := Toolbar;
    Btn.Caption := ACaption;
    Btn.Tag := Ord(ATool);
    Btn.Grouped := True;
    Btn.OnClick := @OnToolSelect;
  end;

begin
  Toolbar := TToolBar.Create(Self);
  Toolbar.Parent := Self;
  Toolbar.Align := alTop;
  Toolbar.ShowCaptions := True;
  Toolbar.Height := 40;

  AddToolButton('Sélection', dtNone);
  AddToolButton('Rectangle', dtRectangle);
  AddToolButton('Cercle', dtCircle);
  AddToolButton('Ligne', dtLine);
  AddToolButton('Texte', dtText);

  // Séparateur
  Btn := TToolButton.Create(Toolbar);
  Btn.Parent := Toolbar;
  Btn.Style := tbsSeparator;

  // Bouton Export
  Btn := TToolButton.Create(Toolbar);
  Btn.Parent := Toolbar;
  Btn.Caption := 'Exporter SVG';
  Btn.Style := tbsButton;
  Btn.OnClick := @OnExportSVGClick;

  // Bouton Effacer
  Btn := TToolButton.Create(Toolbar);
  Btn.Parent := Toolbar;
  Btn.Caption := 'Effacer tout';
  Btn.Style := tbsButton;
  Btn.OnClick := @OnClearAllClick;
end;

procedure TfrmSVGEditor.CreateCanvas;  
var
  Panel: TPanel;
begin
  Panel := TPanel.Create(Self);
  Panel.Parent := Self;
  Panel.Align := alClient;
  Panel.BevelOuter := bvNone;
  Panel.Color := clWhite;

  FCanvas := TPaintBox.Create(Panel);
  FCanvas.Parent := Panel;
  FCanvas.Align := alClient;
  FCanvas.OnMouseDown := @OnCanvasMouseDown;
  FCanvas.OnMouseMove := @OnCanvasMouseMove;
  FCanvas.OnMouseUp := @OnCanvasMouseUp;
  FCanvas.OnPaint := @OnCanvasPaint;
  FCanvas.Color := clWhite;
end;

procedure TfrmSVGEditor.CreatePropertiesPanel;  
var
  Panel: TPanel;
  Label1: TLabel;
  FillColorBox, StrokeColorBox: TColorBox;
  WidthSpin: TSpinEdit;
  Y: Integer;
begin
  Panel := TPanel.Create(Self);
  Panel.Parent := Self;
  Panel.Align := alRight;
  Panel.Width := 200;
  Panel.Caption := '';

  with TLabel.Create(Panel) do
  begin
    Parent := Panel;
    Align := alTop;
    Caption := ' Propriétés';
    Font.Style := [fsBold];
    Height := 25;
  end;

  Y := 30;

  // Couleur de remplissage
  Label1 := TLabel.Create(Panel);
  Label1.Parent := Panel;
  Label1.Left := 10;
  Label1.Top := Y;
  Label1.Caption := 'Remplissage :';

  FillColorBox := TColorBox.Create(Panel);
  FillColorBox.Parent := Panel;
  FillColorBox.Left := 10;
  FillColorBox.Top := Y + 20;
  FillColorBox.Width := 180;
  FillColorBox.Selected := FFillColor;
  FillColorBox.OnChange := @OnColorChange;
  FillColorBox.Name := 'FillColor';

  Inc(Y, 60);

  // Couleur de contour
  Label1 := TLabel.Create(Panel);
  Label1.Parent := Panel;
  Label1.Left := 10;
  Label1.Top := Y;
  Label1.Caption := 'Contour :';

  StrokeColorBox := TColorBox.Create(Panel);
  StrokeColorBox.Parent := Panel;
  StrokeColorBox.Left := 10;
  StrokeColorBox.Top := Y + 20;
  StrokeColorBox.Width := 180;
  StrokeColorBox.Selected := FStrokeColor;
  StrokeColorBox.OnChange := @OnColorChange;
  StrokeColorBox.Name := 'StrokeColor';

  Inc(Y, 60);

  // Épaisseur du contour
  Label1 := TLabel.Create(Panel);
  Label1.Parent := Panel;
  Label1.Left := 10;
  Label1.Top := Y;
  Label1.Caption := 'Épaisseur :';

  WidthSpin := TSpinEdit.Create(Panel);
  WidthSpin.Parent := Panel;
  WidthSpin.Left := 10;
  WidthSpin.Top := Y + 20;
  WidthSpin.Width := 100;
  WidthSpin.MinValue := 1;
  WidthSpin.MaxValue := 20;
  WidthSpin.Value := FStrokeWidth;
  WidthSpin.OnChange := @OnWidthSpinChange;
end;

procedure TfrmSVGEditor.OnExportSVGClick(Sender: TObject);  
var
  SaveDialog: TSaveDialog;
begin
  SaveDialog := TSaveDialog.Create(nil);
  try
    SaveDialog.Filter := 'Fichiers SVG (*.svg)|*.svg';
    SaveDialog.DefaultExt := 'svg';
    if SaveDialog.Execute then
      ExportToSVG(SaveDialog.FileName);
  finally
    SaveDialog.Free;
  end;
end;

procedure TfrmSVGEditor.OnClearAllClick(Sender: TObject);  
begin
  if MessageDlg('Confirmation', 'Effacer tout ?',
     mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    ClearCanvas;
end;

procedure TfrmSVGEditor.OnWidthSpinChange(Sender: TObject);  
begin
  FStrokeWidth := TSpinEdit(Sender).Value;
end;

procedure TfrmSVGEditor.OnToolSelect(Sender: TObject);  
begin
  FCurrentTool := TDrawingTool((Sender as TToolButton).Tag);
  FCanvas.Cursor := crCross;

  if FCurrentTool = dtNone then
    FCanvas.Cursor := crDefault;
end;

procedure TfrmSVGEditor.OnColorChange(Sender: TObject);  
begin
  if (Sender as TColorBox).Name = 'FillColor' then
    FFillColor := (Sender as TColorBox).Selected
  else if (Sender as TColorBox).Name = 'StrokeColor' then
    FStrokeColor := (Sender as TColorBox).Selected;
end;

procedure TfrmSVGEditor.OnCanvasMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    FDrawing := True;
    FStartPoint := Point(X, Y);

    FCurrentShape := TSVGShape.Create;
    FCurrentShape.ShapeType := FCurrentTool;
    FCurrentShape.X1 := X;
    FCurrentShape.Y1 := Y;
    FCurrentShape.FillColor := FFillColor;
    FCurrentShape.StrokeColor := FStrokeColor;
    FCurrentShape.StrokeWidth := FStrokeWidth;
  end;
end;

procedure TfrmSVGEditor.OnCanvasMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if FDrawing and Assigned(FCurrentShape) then
  begin
    FCurrentShape.X2 := X;
    FCurrentShape.Y2 := Y;
    FCanvas.Invalidate;
  end;
end;

procedure TfrmSVGEditor.OnCanvasMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    FDrawing := False;

    if Assigned(FCurrentShape) then
    begin
      FCurrentShape.X2 := X;
      FCurrentShape.Y2 := Y;

      // Demander le texte si c'est un outil texte
      if FCurrentTool = dtText then
      begin
        FCurrentShape.Text := InputBox('Texte', 'Entrez le texte :', '');
        if FCurrentShape.Text = '' then
        begin
          FCurrentShape.Free;
          FCurrentShape := nil;
          FCanvas.Invalidate;
          Exit;
        end;
      end;

      FShapes.Add(FCurrentShape);
      FCurrentShape := nil;
      FCanvas.Invalidate;
    end;
  end;
end;

procedure TfrmSVGEditor.OnCanvasPaint(Sender: TObject);  
var
  i: Integer;
begin
  // Dessiner toutes les formes
  for i := 0 to FShapes.Count - 1 do
    DrawShape(FCanvas.Canvas, TSVGShape(FShapes[i]));

  // Dessiner la forme en cours de création
  if FDrawing and Assigned(FCurrentShape) then
    DrawShape(FCanvas.Canvas, FCurrentShape);
end;

procedure TfrmSVGEditor.DrawShape(ACanvas: TCanvas; AShape: TSVGShape);  
var
  R: TRect;
  CX, CY, Radius: Integer;
begin
  ACanvas.Pen.Color := AShape.StrokeColor;
  ACanvas.Pen.Width := AShape.StrokeWidth;
  ACanvas.Brush.Color := AShape.FillColor;

  case AShape.ShapeType of
    dtRectangle:
      begin
        R := Rect(AShape.X1, AShape.Y1, AShape.X2, AShape.Y2);
        ACanvas.Rectangle(R);
      end;

    dtCircle:
      begin
        CX := (AShape.X1 + AShape.X2) div 2;
        CY := (AShape.Y1 + AShape.Y2) div 2;
        Radius := Round(Sqrt(Sqr(AShape.X2 - AShape.X1) +
                            Sqr(AShape.Y2 - AShape.Y1)) / 2);
        ACanvas.Ellipse(CX - Radius, CY - Radius,
                       CX + Radius, CY + Radius);
      end;

    dtLine:
      begin
        ACanvas.MoveTo(AShape.X1, AShape.Y1);
        ACanvas.LineTo(AShape.X2, AShape.Y2);
      end;

    dtText:
      begin
        ACanvas.Brush.Style := bsClear;
        ACanvas.Font.Size := 12;
        ACanvas.Font.Color := AShape.StrokeColor;
        ACanvas.TextOut(AShape.X1, AShape.Y1, AShape.Text);
      end;
  end;
end;

procedure TfrmSVGEditor.ExportToSVG(const AFileName: string);  
var
  SVG: TSVGGenerator;
  i: Integer;
  Shape: TSVGShape;
  CX, CY, Radius: Integer;
begin
  SVG := TSVGGenerator.Create(FCanvas.Width, FCanvas.Height);
  try
    // Fond blanc
    SVG.AddRectangle(0, 0, FCanvas.Width, FCanvas.Height, clWhite, clNone);

    // Ajouter toutes les formes
    for i := 0 to FShapes.Count - 1 do
    begin
      Shape := TSVGShape(FShapes[i]);

      case Shape.ShapeType of
        dtRectangle:
          SVG.AddRectangle(Shape.X1, Shape.Y1,
                          Shape.X2 - Shape.X1, Shape.Y2 - Shape.Y1,
                          Shape.FillColor, Shape.StrokeColor,
                          Shape.StrokeWidth);

        dtCircle:
          begin
            CX := (Shape.X1 + Shape.X2) div 2;
            CY := (Shape.Y1 + Shape.Y2) div 2;
            Radius := Round(Sqrt(Sqr(Shape.X2 - Shape.X1) +
                                Sqr(Shape.Y2 - Shape.Y1)) / 2);
            SVG.AddCircle(CX, CY, Radius, Shape.FillColor,
                         Shape.StrokeColor, Shape.StrokeWidth);
          end;

        dtLine:
          SVG.AddLine(Shape.X1, Shape.Y1, Shape.X2, Shape.Y2,
                     Shape.StrokeColor, Shape.StrokeWidth);

        dtText:
          SVG.AddText(Shape.X1, Shape.Y1, Shape.Text, 12,
                     Shape.StrokeColor);
      end;
    end;

    SVG.SaveToFile(AFileName);
    ShowMessage('SVG exporté avec succès !');
  finally
    SVG.Free;
  end;
end;

procedure TfrmSVGEditor.ClearCanvas;  
var
  i: Integer;
begin
  for i := 0 to FShapes.Count - 1 do
    TSVGShape(FShapes[i]).Free;
  FShapes.Clear;
  FCanvas.Invalidate;
end;

end.
```

## Optimisation de fichiers SVG

### Réduction de la taille

```pascal
unit SVGOptimizer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TSVGOptimizer = class
  public
    class function OptimizeSVG(const ASVG: string): string;
    class function RemoveComments(const ASVG: string): string;
    class function SimplifyNumbers(const ASVG: string): string;
    class function RemoveWhitespace(const ASVG: string): string;
  end;

implementation

uses
  RegExpr;

class function TSVGOptimizer.RemoveComments(const ASVG: string): string;  
var
  Regex: TRegExpr;
begin
  Regex := TRegExpr.Create;
  try
    Regex.Expression := '<!--.*?-->';
    Regex.ModifierS := True; // . correspond à tout, y compris les sauts de ligne
    Result := Regex.Replace(ASVG, '', True);
  finally
    Regex.Free;
  end;
end;

class function TSVGOptimizer.SimplifyNumbers(const ASVG: string): string;  
var
  Regex: TRegExpr;
begin
  Result := ASVG;
  Regex := TRegExpr.Create;
  try
    // Supprimer les zéros inutiles (0.500 -> 0.5)
    Regex.Expression := '(\d+)\.(\d*?)0+(["\s>])';
    Result := Regex.Replace(Result, '$1.$2$3', True);

    // Supprimer le point si c'est .0 (5.0 -> 5)
    Regex.Expression := '(\d+)\.0(["\s>])';
    Result := Regex.Replace(Result, '$1$2', True);
  finally
    Regex.Free;
  end;
end;

class function TSVGOptimizer.RemoveWhitespace(const ASVG: string): string;  
var
  Lines: TStringList;
  i: Integer;
begin
  Lines := TStringList.Create;
  try
    Lines.Text := ASVG;

    // Supprimer les lignes vides et les espaces en début/fin
    for i := Lines.Count - 1 downto 0 do
    begin
      Lines[i] := Trim(Lines[i]);
      if Lines[i] = '' then
        Lines.Delete(i);
    end;

    Result := Lines.Text;
  finally
    Lines.Free;
  end;
end;

class function TSVGOptimizer.OptimizeSVG(const ASVG: string): string;  
begin
  Result := ASVG;
  Result := RemoveComments(Result);
  Result := SimplifyNumbers(Result);
  Result := RemoveWhitespace(Result);
end;

end.
```

## Animation SVG

Les SVG peuvent contenir des animations :

```pascal
procedure CreateAnimatedSVG;  
var
  SVG: TStringList;
begin
  SVG := TStringList.Create;
  try
    SVG.Add('<?xml version="1.0" encoding="UTF-8"?>');
    SVG.Add('<svg width="400" height="400" xmlns="http://www.w3.org/2000/svg">');

    // Cercle qui se déplace
    SVG.Add('  <circle cx="50" cy="200" r="20" fill="red">');
    SVG.Add('    <animate attributeName="cx" from="50" to="350" dur="3s" repeatCount="indefinite"/>');
    SVG.Add('  </circle>');

    // Rectangle qui change de couleur
    SVG.Add('  <rect x="100" y="100" width="50" height="50" fill="blue">');
    SVG.Add('    <animate attributeName="fill" values="blue;red;green;blue" dur="4s" repeatCount="indefinite"/>');
    SVG.Add('  </rect>');

    // Cercle qui grossit et rétrécit
    SVG.Add('  <circle cx="200" cy="300" r="20" fill="green">');
    SVG.Add('    <animate attributeName="r" values="20;40;20" dur="2s" repeatCount="indefinite"/>');
    SVG.Add('  </circle>');

    SVG.Add('</svg>');
    SVG.SaveToFile('animated.svg');
  finally
    SVG.Free;
  end;
end;
```

## Filtres SVG

Les filtres permettent des effets visuels avancés :

```pascal
procedure CreateSVGWithFilters;  
var
  SVG: TStringList;
begin
  SVG := TStringList.Create;
  try
    SVG.Add('<?xml version="1.0" encoding="UTF-8"?>');
    SVG.Add('<svg width="400" height="400" xmlns="http://www.w3.org/2000/svg">');

    // Définition des filtres
    SVG.Add('  <defs>');

    // Filtre d''ombre
    SVG.Add('    <filter id="shadow">');
    SVG.Add('      <feGaussianBlur in="SourceAlpha" stdDeviation="3"/>');
    SVG.Add('      <feOffset dx="2" dy="2" result="offsetblur"/>');
    SVG.Add('      <feMerge>');
    SVG.Add('        <feMergeNode/>');
    SVG.Add('        <feMergeNode in="SourceGraphic"/>');
    SVG.Add('      </feMerge>');
    SVG.Add('    </filter>');

    // Filtre de flou
    SVG.Add('    <filter id="blur">');
    SVG.Add('      <feGaussianBlur in="SourceGraphic" stdDeviation="5"/>');
    SVG.Add('    </filter>');

    SVG.Add('  </defs>');

    // Rectangle avec ombre
    SVG.Add('  <rect x="50" y="50" width="100" height="80" fill="blue" filter="url(#shadow)"/>');

    // Cercle avec flou
    SVG.Add('  <circle cx="250" cy="90" r="40" fill="red" filter="url(#blur)"/>');

    SVG.Add('</svg>');
    SVG.SaveToFile('filters.svg');
  finally
    SVG.Free;
  end;
end;
```

## Conversion de formats

### Bitmap vers SVG (tracé)

```pascal
unit BitmapToSVG;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, SVGGenerator;

type
  TBitmapTracer = class
  public
    class function TraceSimple(ABitmap: TBitmap): string;
    class procedure SaveAsSVG(ABitmap: TBitmap; const AFileName: string);
  end;

implementation

class function TBitmapTracer.TraceSimple(ABitmap: TBitmap): string;  
var
  SVG: TSVGGenerator;
  x, y: Integer;
  PixelColor: TColor;
  LastColor: TColor;
  RectX, RectY, RectWidth: Integer;
begin
  SVG := TSVGGenerator.Create(ABitmap.Width, ABitmap.Height);
  try
    LastColor := clNone;
    RectX := 0;
    RectY := 0;
    RectWidth := 0;

    // Parcourir l''image pixel par pixel (simpliste)
    for y := 0 to ABitmap.Height - 1 do
    begin
      for x := 0 to ABitmap.Width - 1 do
      begin
        PixelColor := ABitmap.Canvas.Pixels[x, y];

        // Grouper les pixels de même couleur horizontalement
        if PixelColor = LastColor then
          Inc(RectWidth)
        else
        begin
          if (LastColor <> clNone) and (RectWidth > 0) then
            SVG.AddRectangle(RectX, RectY, RectWidth, 1,
                           LastColor, clNone, 0);

          RectX := x;
          RectY := y;
          RectWidth := 1;
          LastColor := PixelColor;
        end;
      end;

      // Fin de ligne
      if RectWidth > 0 then
        SVG.AddRectangle(RectX, RectY, RectWidth, 1,
                        LastColor, clNone, 0);

      LastColor := clNone;
      RectWidth := 0;
    end;

    Result := SVG.GetSVG;
  finally
    SVG.Free;
  end;
end;

class procedure TBitmapTracer.SaveAsSVG(ABitmap: TBitmap;
  const AFileName: string);
var
  SVGContent: string;
  Output: TStringList;
begin
  SVGContent := TraceSimple(ABitmap);

  Output := TStringList.Create;
  try
    Output.Text := SVGContent;
    Output.SaveToFile(AFileName);
  finally
    Output.Free;
  end;
end;

end.
```

## Manipulation avancée de SVG

### Parser SVG

```pascal
unit SVGParser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DOM, XMLRead;

type
  TSVGElement = class
    TagName: string;
    Attributes: TStringList;
    Children: TList;
    constructor Create;
    destructor Destroy; override;
  end;

  TSVGParser = class
  private
    FRoot: TSVGElement;
    procedure ParseNode(ANode: TDOMNode; AParent: TSVGElement);
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadFromFile(const AFileName: string);
    procedure LoadFromString(const ASVG: string);

    property Root: TSVGElement read FRoot;
  end;

implementation

constructor TSVGElement.Create;  
begin
  inherited Create;
  Attributes := TStringList.Create;
  Children := TList.Create;
end;

destructor TSVGElement.Destroy;  
var
  i: Integer;
begin
  for i := 0 to Children.Count - 1 do
    TSVGElement(Children[i]).Free;
  Children.Free;
  Attributes.Free;
  inherited Destroy;
end;

constructor TSVGParser.Create;  
begin
  inherited Create;
  FRoot := nil;
end;

destructor TSVGParser.Destroy;  
begin
  if Assigned(FRoot) then
    FRoot.Free;
  inherited Destroy;
end;

procedure TSVGParser.LoadFromFile(const AFileName: string);  
var
  Doc: TXMLDocument;
begin
  if Assigned(FRoot) then
  begin
    FRoot.Free;
    FRoot := nil;
  end;

  ReadXMLFile(Doc, AFileName);
  try
    FRoot := TSVGElement.Create;
    ParseNode(Doc.DocumentElement, FRoot);
  finally
    Doc.Free;
  end;
end;

procedure TSVGParser.LoadFromString(const ASVG: string);  
var
  Stream: TStringStream;
  Doc: TXMLDocument;
begin
  if Assigned(FRoot) then
  begin
    FRoot.Free;
    FRoot := nil;
  end;

  Stream := TStringStream.Create(ASVG);
  try
    ReadXMLFile(Doc, Stream);
    try
      FRoot := TSVGElement.Create;
      ParseNode(Doc.DocumentElement, FRoot);
    finally
      Doc.Free;
    end;
  finally
    Stream.Free;
  end;
end;

procedure TSVGParser.ParseNode(ANode: TDOMNode; AParent: TSVGElement);  
var
  i: Integer;
  Element: TSVGElement;
  AttrNode: TDOMNode;
begin
  if not Assigned(ANode) then Exit;

  AParent.TagName := ANode.NodeName;

  // Parser les attributs
  if Assigned(ANode.Attributes) then
  begin
    for i := 0 to ANode.Attributes.Length - 1 do
    begin
      AttrNode := ANode.Attributes.Item[i];
      AParent.Attributes.Values[AttrNode.NodeName] := AttrNode.NodeValue;
    end;
  end;

  // Parser les enfants
  if Assigned(ANode.ChildNodes) then
  begin
    for i := 0 to ANode.ChildNodes.Count - 1 do
    begin
      if ANode.ChildNodes[i].NodeType = ELEMENT_NODE then
      begin
        Element := TSVGElement.Create;
        ParseNode(ANode.ChildNodes[i], Element);
        AParent.Children.Add(Element);
      end;
    end;
  end;
end;

end.
```

## Compatibilité multi-plateforme

### Gestion des chemins et des polices

```pascal
unit SVGPlatform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics;

type
  TSVGExportSettings = record
    FontName: string;
    FontPath: string;
    EmbedFonts: Boolean;
  end;

  TSVGPlatformHelper = class
  public
    class function GetDefaultFont: string;
    class function GetFontPath(const AFontName: string): string;
    class function NormalizePath(const APath: string): string;
    class procedure ConfigureSVGExport(var ASettings: TSVGExportSettings);
  end;

implementation

class function TSVGPlatformHelper.GetDefaultFont: string;  
begin
  {$IFDEF WINDOWS}
  Result := 'Arial';
  {$ENDIF}

  {$IFDEF UNIX}
  Result := 'DejaVu Sans';
  {$ENDIF}
end;

class function TSVGPlatformHelper.GetFontPath(const AFontName: string): string;  
begin
  {$IFDEF WINDOWS}
  Result := 'C:\Windows\Fonts\' + AFontName + '.ttf';
  {$ENDIF}

  {$IFDEF UNIX}
  Result := '/usr/share/fonts/truetype/' + LowerCase(AFontName) + '.ttf';
  {$ENDIF}

  if not FileExists(Result) then
    Result := '';
end;

class function TSVGPlatformHelper.NormalizePath(const APath: string): string;  
begin
  Result := StringReplace(APath, '\', '/', [rfReplaceAll]);
end;

class procedure TSVGPlatformHelper.ConfigureSVGExport(var ASettings: TSVGExportSettings);  
begin
  // Configuration spécifique à la plateforme
  ASettings.FontName := GetDefaultFont;
  ASettings.FontPath := GetFontPath(ASettings.FontName);

  {$IFDEF WINDOWS}
  ASettings.EmbedFonts := True;
  {$ENDIF}

  {$IFDEF UNIX}
  ASettings.EmbedFonts := False; // fontconfig gère les polices
  {$ENDIF}
end;

end.
```

## Bibliothèque complète de formes

### Formes géométriques avancées

```pascal
unit SVGShapes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Math, SVGGenerator;

type
  TSVGShapeLibrary = class
  public
    // Formes de base
    class procedure AddStar(ASVG: TSVGGenerator; CX, CY, OuterRadius, InnerRadius: Integer;
                           Points: Integer; FillColor, StrokeColor: TColor);
    class procedure AddPolygon(ASVG: TSVGGenerator; CX, CY, Radius: Integer;
                              Sides: Integer; FillColor, StrokeColor: TColor);
    class procedure AddArrow(ASVG: TSVGGenerator; X1, Y1, X2, Y2: Integer;
                            HeadSize: Integer; StrokeColor: TColor);
    class procedure AddRoundedRect(ASVG: TSVGGenerator; X, Y, Width, Height: Integer;
                                  CornerRadius: Integer; FillColor, StrokeColor: TColor);

    // Formes spéciales
    class procedure AddGear(ASVG: TSVGGenerator; CX, CY, OuterRadius, InnerRadius: Integer;
                           Teeth: Integer; FillColor: TColor);
    class procedure AddHeart(ASVG: TSVGGenerator; CX, CY, Size: Integer;
                            FillColor: TColor);
    class procedure AddCloud(ASVG: TSVGGenerator; X, Y, Width, Height: Integer;
                            FillColor: TColor);
  end;

implementation

class procedure TSVGShapeLibrary.AddStar(ASVG: TSVGGenerator;
  CX, CY, OuterRadius, InnerRadius: Integer; Points: Integer;
  FillColor, StrokeColor: TColor);
var
  PathData: string;
  i: Integer;
  Angle, AngleStep: Double;
  X, Y: Double;
begin
  PathData := '';
  AngleStep := 2 * Pi / Points;

  for i := 0 to Points * 2 - 1 do
  begin
    Angle := i * AngleStep / 2 - Pi / 2;

    if i mod 2 = 0 then
    begin
      X := CX + OuterRadius * Cos(Angle);
      Y := CY + OuterRadius * Sin(Angle);
    end
    else
    begin
      X := CX + InnerRadius * Cos(Angle);
      Y := CY + InnerRadius * Sin(Angle);
    end;

    if i = 0 then
      PathData := Format('M %.2f %.2f', [X, Y])
    else
      PathData := PathData + Format(' L %.2f %.2f', [X, Y]);
  end;

  PathData := PathData + ' Z';
  ASVG.AddPath(PathData, FillColor, StrokeColor, 2);
end;

class procedure TSVGShapeLibrary.AddPolygon(ASVG: TSVGGenerator;
  CX, CY, Radius: Integer; Sides: Integer; FillColor, StrokeColor: TColor);
var
  PathData: string;
  i: Integer;
  Angle, AngleStep: Double;
  X, Y: Double;
begin
  PathData := '';
  AngleStep := 2 * Pi / Sides;

  for i := 0 to Sides - 1 do
  begin
    Angle := i * AngleStep - Pi / 2;
    X := CX + Radius * Cos(Angle);
    Y := CY + Radius * Sin(Angle);

    if i = 0 then
      PathData := Format('M %.2f %.2f', [X, Y])
    else
      PathData := PathData + Format(' L %.2f %.2f', [X, Y]);
  end;

  PathData := PathData + ' Z';
  ASVG.AddPath(PathData, FillColor, StrokeColor, 2);
end;

class procedure TSVGShapeLibrary.AddArrow(ASVG: TSVGGenerator;
  X1, Y1, X2, Y2: Integer; HeadSize: Integer; StrokeColor: TColor);
var
  Angle: Double;
  ArrowPath: string;
  X3, Y3, X4, Y4: Double;
begin
  // Ligne principale
  ASVG.AddLine(X1, Y1, X2, Y2, StrokeColor, 2);

  // Calculer l'angle de la flèche
  Angle := ArcTan2(Y2 - Y1, X2 - X1);

  // Pointes de la flèche
  X3 := X2 - HeadSize * Cos(Angle - Pi / 6);
  Y3 := Y2 - HeadSize * Sin(Angle - Pi / 6);

  X4 := X2 - HeadSize * Cos(Angle + Pi / 6);
  Y4 := Y2 - HeadSize * Sin(Angle + Pi / 6);

  ArrowPath := Format('M %d %d L %.2f %.2f M %d %d L %.2f %.2f',
                     [X2, Y2, X3, Y3, X2, Y2, X4, Y4]);

  ASVG.AddPath(ArrowPath, clNone, StrokeColor, 2);
end;

class procedure TSVGShapeLibrary.AddRoundedRect(ASVG: TSVGGenerator;
  X, Y, Width, Height: Integer; CornerRadius: Integer;
  FillColor, StrokeColor: TColor);
var
  PathData: string;
begin
  PathData := Format(
    'M %d %d ' +
    'L %d %d ' +
    'Q %d %d %d %d ' +
    'L %d %d ' +
    'Q %d %d %d %d ' +
    'L %d %d ' +
    'Q %d %d %d %d ' +
    'L %d %d ' +
    'Q %d %d %d %d Z',
    [
      X + CornerRadius, Y,
      X + Width - CornerRadius, Y,
      X + Width, Y, X + Width, Y + CornerRadius,
      X + Width, Y + Height - CornerRadius,
      X + Width, Y + Height, X + Width - CornerRadius, Y + Height,
      X + CornerRadius, Y + Height,
      X, Y + Height, X, Y + Height - CornerRadius,
      X, Y + CornerRadius,
      X, Y, X + CornerRadius, Y
    ]
  );

  ASVG.AddPath(PathData, FillColor, StrokeColor, 2);
end;

class procedure TSVGShapeLibrary.AddGear(ASVG: TSVGGenerator;
  CX, CY, OuterRadius, InnerRadius: Integer; Teeth: Integer; FillColor: TColor);
var
  PathData: string;
  i: Integer;
  Angle, AngleStep, ToothWidth: Double;
  X1, Y1, X2, Y2, X3, Y3, X4, Y4: Double;
begin
  PathData := '';
  AngleStep := 2 * Pi / Teeth;
  ToothWidth := AngleStep * 0.3;

  for i := 0 to Teeth - 1 do
  begin
    Angle := i * AngleStep;

    // Point intérieur gauche
    X1 := CX + InnerRadius * Cos(Angle - ToothWidth / 2);
    Y1 := CY + InnerRadius * Sin(Angle - ToothWidth / 2);

    // Point extérieur gauche
    X2 := CX + OuterRadius * Cos(Angle - ToothWidth / 2);
    Y2 := CY + OuterRadius * Sin(Angle - ToothWidth / 2);

    // Point extérieur droit
    X3 := CX + OuterRadius * Cos(Angle + ToothWidth / 2);
    Y3 := CY + OuterRadius * Sin(Angle + ToothWidth / 2);

    // Point intérieur droit
    X4 := CX + InnerRadius * Cos(Angle + ToothWidth / 2);
    Y4 := CY + InnerRadius * Sin(Angle + ToothWidth / 2);

    if i = 0 then
      PathData := Format('M %.2f %.2f', [X1, Y1])
    else
      PathData := PathData + Format(' L %.2f %.2f', [X1, Y1]);

    PathData := PathData + Format(' L %.2f %.2f L %.2f %.2f L %.2f %.2f',
                                  [X2, Y2, X3, Y3, X4, Y4]);
  end;

  PathData := PathData + ' Z';
  ASVG.AddPath(PathData, FillColor, clBlack, 2);

  // Trou central
  ASVG.AddCircle(CX, CY, InnerRadius div 2, clWhite, clBlack, 2);
end;

class procedure TSVGShapeLibrary.AddHeart(ASVG: TSVGGenerator;
  CX, CY, Size: Integer; FillColor: TColor);
var
  PathData: string;
begin
  PathData := Format(
    'M %d %d ' +
    'C %d %d %d %d %d %d ' +
    'C %d %d %d %d %d %d Z',
    [
      CX, CY + Size div 4,
      CX - Size, CY - Size div 2, CX - Size div 2, CY + Size,
      CX, CY + Size,
      CX + Size div 2, CY + Size, CX + Size, CY - Size div 2,
      CX, CY + Size div 4
    ]
  );

  ASVG.AddPath(PathData, FillColor, clBlack, 2);
end;

class procedure TSVGShapeLibrary.AddCloud(ASVG: TSVGGenerator;
  X, Y, Width, Height: Integer; FillColor: TColor);
var
  PathData: string;
  R1, R2, R3: Integer;
begin
  R1 := Height div 2;
  R2 := Round(Height * 0.6);
  R3 := Round(Height * 0.4);

  PathData := Format(
    'M %d %d ' +
    'Q %d %d %d %d ' +
    'Q %d %d %d %d ' +
    'Q %d %d %d %d ' +
    'Q %d %d %d %d Z',
    [
      X + R1, Y + Height,
      X, Y + Height - R1, X + R1, Y + Height - R1 * 2,
      X + Width div 3, Y, X + Width * 2 div 3, Y + R2,
      X + Width, Y + Height - R3, X + Width - R3, Y + Height,
      X + Width div 2, Y + Height + R3, X + R1, Y + Height
    ]
  );

  ASVG.AddPath(PathData, FillColor, clBlack, 2);
end;

end.
```

## Exemples d'utilisation avancée

### Créer un tableau de bord SVG

```pascal
procedure CreateDashboard;  
var
  SVG: TSVGGenerator;
begin
  SVG := TSVGGenerator.Create(800, 600);
  try
    // Fond
    SVG.AddRectangle(0, 0, 800, 600, RGB(240, 240, 240), clNone);

    // Titre
    SVG.AddText(300, 40, 'Tableau de Bord', 28, clNavy);

    // Indicateurs avec étoiles
    TSVGShapeLibrary.AddStar(SVG, 150, 150, 40, 20, 5, clYellow, clOrange);
    SVG.AddText(130, 210, 'Performance', 14, clBlack);

    // Engrenage pour paramètres
    TSVGShapeLibrary.AddGear(SVG, 400, 150, 50, 30, 12, clGray);
    SVG.AddText(375, 210, 'Paramètres', 14, clBlack);

    // Cœur pour favoris
    TSVGShapeLibrary.AddHeart(SVG, 650, 150, 40, clRed);
    SVG.AddText(630, 210, 'Favoris', 14, clBlack);

    // Graphique en barres simplifié
    SVG.AddRectangle(50, 300, 40, 100, clBlue, clNavy, 2);
    SVG.AddRectangle(120, 300, 40, 150, clGreen, clDarkGreen, 2);
    SVG.AddRectangle(190, 300, 40, 80, clRed, clMaroon, 2);
    SVG.AddRectangle(260, 300, 40, 120, clYellow, clOlive, 2);

    // Légendes
    SVG.AddText(55, 420, 'Jan', 12, clBlack);
    SVG.AddText(125, 420, 'Fév', 12, clBlack);
    SVG.AddText(195, 420, 'Mar', 12, clBlack);
    SVG.AddText(265, 420, 'Avr', 12, clBlack);

    // Flèches d'indication
    TSVGShapeLibrary.AddArrow(SVG, 400, 350, 500, 350, 15, clBlack);
    SVG.AddText(420, 340, 'Tendance', 12, clBlack);

    // Polygone pour statistiques
    TSVGShapeLibrary.AddPolygon(SVG, 600, 380, 80, 6, clSkyBlue, clBlue);
    SVG.AddText(575, 480, 'Statistiques', 12, clBlack);

    SVG.SaveToFile('dashboard.svg');
  finally
    SVG.Free;
  end;
end;
```

### Créer une carte mentale

```pascal
procedure CreateMindMap;  
var
  SVG: TSVGGenerator;
begin
  SVG := TSVGGenerator.Create(1000, 700);
  try
    // Fond
    SVG.AddRectangle(0, 0, 1000, 700, clWhite, clNone);

    // Nœud central
    TSVGShapeLibrary.AddRoundedRect(SVG, 400, 300, 200, 80, 20,
                            RGB(255, 200, 100), clOrange);
    SVG.AddText(445, 345, 'Idée Centrale', 16, clBlack);

    // Branches principales

    // Branche 1 (haut gauche)
    TSVGShapeLibrary.AddArrow(SVG, 450, 300, 350, 200, 12, clBlue);
    TSVGShapeLibrary.AddRoundedRect(SVG, 250, 170, 150, 60, 15,
                            clLightBlue, clBlue);
    SVG.AddText(295, 205, 'Concept 1', 14, clBlack);

    // Branche 2 (haut droite)
    TSVGShapeLibrary.AddArrow(SVG, 550, 300, 650, 200, 12, clGreen);
    TSVGShapeLibrary.AddRoundedRect(SVG, 600, 170, 150, 60, 15,
                            clLightGreen, clGreen);
    SVG.AddText(645, 205, 'Concept 2', 14, clBlack);

    // Branche 3 (bas gauche)
    TSVGShapeLibrary.AddArrow(SVG, 450, 380, 350, 500, 12, clRed);
    TSVGShapeLibrary.AddRoundedRect(SVG, 250, 470, 150, 60, 15,
                            RGB(255, 200, 200), clRed);
    SVG.AddText(295, 505, 'Concept 3', 14, clBlack);

    // Branche 4 (bas droite)
    TSVGShapeLibrary.AddArrow(SVG, 550, 380, 650, 500, 12, clPurple);
    TSVGShapeLibrary.AddRoundedRect(SVG, 600, 470, 150, 60, 15,
                            clLavender, clPurple);
    SVG.AddText(645, 505, 'Concept 4', 14, clBlack);

    // Sous-branches
    TSVGShapeLibrary.AddArrow(SVG, 250, 200, 150, 150, 10, clBlue);
    SVG.AddCircle(150, 150, 30, clLightBlue, clBlue, 2);
    SVG.AddText(135, 155, 'A', 12, clBlack);

    TSVGShapeLibrary.AddArrow(SVG, 250, 200, 150, 250, 10, clBlue);
    SVG.AddCircle(150, 250, 30, clLightBlue, clBlue, 2);
    SVG.AddText(135, 255, 'B', 12, clBlack);

    SVG.SaveToFile('mindmap.svg');
  finally
    SVG.Free;
  end;
end;
```

## Bonnes pratiques

### 1. Organisation du code SVG

```pascal
// ✅ BON : Utiliser des groupes pour organiser
procedure CreateOrganizedSVG;  
var
  SVG: TStringList;
begin
  SVG := TStringList.Create;
  try
    SVG.Add('<?xml version="1.0"?>');
    SVG.Add('<svg width="400" height="400">');

    // Groupe pour le fond
    SVG.Add('  <g id="background">');
    SVG.Add('    <rect width="400" height="400" fill="white"/>');
    SVG.Add('  </g>');

    // Groupe pour le contenu
    SVG.Add('  <g id="content">');
    SVG.Add('    <circle cx="200" cy="200" r="50" fill="blue"/>');
    SVG.Add('  </g>');

    SVG.Add('</svg>');
    SVG.SaveToFile('organized.svg');
  finally
    SVG.Free;
  end;
end;

// ❌ MAUVAIS : Tout mélangé sans organisation
procedure CreateMessySVG;  
var
  SVG: TStringList;
begin
  SVG := TStringList.Create;
  try
    SVG.Add('<svg><rect.../><circle.../><text.../>...</svg>');
    SVG.SaveToFile('messy.svg');
  finally
    SVG.Free;
  end;
end;
```

### 2. Optimisation de la taille

```pascal
// ✅ BON : Réutiliser les définitions
var
  SVG: TStringList;
begin
  SVG := TStringList.Create;
  try
    SVG.Add('<svg>');
    SVG.Add('  <defs>');
    SVG.Add('    <circle id="dot" r="5" fill="red"/>');
    SVG.Add('  </defs>');
    SVG.Add('  <use href="#dot" x="10" y="10"/>');
    SVG.Add('  <use href="#dot" x="20" y="20"/>');
    SVG.Add('  <use href="#dot" x="30" y="30"/>');
    SVG.Add('</svg>');
  finally
    SVG.Free;
  end;
end;

// ❌ MAUVAIS : Répéter les mêmes éléments
var
  SVG: TStringList;
begin
  SVG := TStringList.Create;
  try
    SVG.Add('<svg>');
    SVG.Add('  <circle cx="10" cy="10" r="5" fill="red"/>');
    SVG.Add('  <circle cx="20" cy="20" r="5" fill="red"/>');
    SVG.Add('  <circle cx="30" cy="30" r="5" fill="red"/>');
    SVG.Add('</svg>');
  finally
    SVG.Free;
  end;
end;
```

### 3. Accessibilité

```pascal
// ✅ BON : Ajouter des descriptions
SVG.Add('<svg>');  
SVG.Add('  <title>Logo de l''entreprise</title>');  
SVG.Add('  <desc>Un cercle bleu avec le texte "ACME" au centre</desc>');  
SVG.Add('  <circle cx="100" cy="100" r="50" fill="blue"/>');  
SVG.Add('  <text x="75" y="105">ACME</text>');  
SVG.Add('</svg>');
```

## Conclusion

### Récapitulatif des points clés

1. **Format vectoriel** : SVG permet un redimensionnement sans perte de qualité
2. **Basé sur XML** : Facile à générer et manipuler par programmation
3. **Portable** : Fonctionne sur Windows, Linux, et dans les navigateurs web
4. **Extensible** : Supporte animations, filtres, et interactions
5. **Optimisable** : Taille de fichier réduite avec les bonnes pratiques

### Quand utiliser SVG ?

**Utilisez SVG pour :**
- ✅ Logos et icônes
- ✅ Diagrammes et graphiques
- ✅ Interfaces utilisateur scalables
- ✅ Illustrations techniques
- ✅ Cartes et plans
- ✅ Export de visualisations de données

**Évitez SVG pour :**
- ❌ Photographies
- ❌ Images avec beaucoup de détails complexes
- ❌ Textures réalistes
- ❌ Effets photographiques

### Ressources et outils

#### Bibliothèques FreePascal/Lazarus
- **BGRASVG** : Rendu SVG avec BGRABitmap
- **fpvectorial** : Lecture/écriture de formats vectoriels
- **LazSVG** : Composants SVG pour Lazarus

#### Éditeurs SVG
- **Inkscape** : Éditeur SVG gratuit et complet
- **SVGOMG** : Optimiseur SVG en ligne
- **Method Draw** : Éditeur SVG simple en ligne

#### Documentation
- **Spécification SVG W3C** : https://www.w3.org/TR/SVG/
- **MDN SVG Tutorial** : Documentation complète
- **SVG Path Reference** : Guide des chemins SVG

### Exemple complet final

Voici un exemple intégrant plusieurs concepts :

```pascal
program SVGMasterExample;

{$mode objfpc}{$H+}

uses
  SysUtils, SVGGenerator, SVGShapes;

procedure CreateCompleteSVG;  
var
  SVG: TSVGGenerator;
begin
  SVG := TSVGGenerator.Create(800, 600);
  try
    // Fond avec gradient (simulé avec rectangles)
    SVG.AddRectangle(0, 0, 800, 600, RGB(230, 240, 255), clNone);

    // Titre
    SVG.AddText(250, 50, 'Démonstration SVG Complète', 24, clNavy);

    // Formes géométriques
    SVG.BeginGroup('shapes');
    TSVGShapeLibrary.AddStar(SVG, 100, 150, 40, 20, 5, clYellow, clOrange);
    TSVGShapeLibrary.AddPolygon(SVG, 200, 150, 40, 6, clLightBlue, clBlue);
    TSVGShapeLibrary.AddHeart(SVG, 300, 150, 30, clRed);
    TSVGShapeLibrary.AddGear(SVG, 400, 150, 40, 25, 10, clGray);
    SVG.EndGroup;

    // Graphique
    SVG.BeginGroup('chart');
    SVG.AddRectangle(50, 300, 60, 150, clBlue, clNavy, 2);
    SVG.AddRectangle(130, 300, 60, 200, clGreen, clDarkGreen, 2);
    SVG.AddRectangle(210, 300, 60, 100, clRed, clMaroon, 2);
    SVG.AddRectangle(290, 300, 60, 250, clOrange, clRed, 2);
    SVG.EndGroup;

    // Flèches et annotations
    TSVGShapeLibrary.AddArrow(SVG, 400, 400, 550, 350, 15, clBlack);
    SVG.AddText(450, 390, 'Tendance', 12, clBlack);

    // Rectangle arrondi avec texte
    TSVGShapeLibrary.AddRoundedRect(SVG, 500, 450, 250, 100, 15,
                            RGB(255, 255, 200), clOrange);
    SVG.AddText(520, 490, 'Créé avec FreePascal', 16, clBlack);
    SVG.AddText(520, 515, 'et Lazarus', 16, clBlack);

    SVG.SaveToFile('complete_example.svg');
    WriteLn('SVG créé avec succès : complete_example.svg');
  finally
    SVG.Free;
  end;
end;

begin
  try
    CreateCompleteSVG;
  except
    on E: Exception do
      WriteLn('Erreur : ', E.Message);
  end;
end.
```

## Points à retenir pour la suite

1. **Pratiquez** : Créez des SVG simples puis augmentez la complexité
2. **Optimisez** : Utilisez les groupes et les définitions réutilisables
3. **Testez** : Vérifiez vos SVG dans différents navigateurs et visionneuses
4. **Documentez** : Ajoutez des titres et descriptions pour l'accessibilité
5. **Réutilisez** : Créez une bibliothèque de formes personnalisées

### Checklist projet SVG

Avant de déployer des fonctionnalités SVG dans votre application :

#### Fonctionnalités de base
- [ ] Génération de formes simples (rectangle, cercle, ligne)
- [ ] Gestion des couleurs (remplissage et contour)
- [ ] Export vers fichier SVG
- [ ] Validation du XML généré
- [ ] Visualisation dans l'application

#### Fonctionnalités avancées
- [ ] Chemins complexes (path)
- [ ] Transformations (rotation, translation, échelle)
- [ ] Groupes et organisation hiérarchique
- [ ] Gradients linéaires et radiaux
- [ ] Filtres et effets
- [ ] Texte et polices
- [ ] Import de SVG existants

#### Qualité et performance
- [ ] Optimisation de la taille des fichiers
- [ ] Gestion de grandes quantités d'éléments
- [ ] Cache de rendu si nécessaire
- [ ] Validation des coordonnées
- [ ] Gestion des erreurs

#### Multi-plateforme
- [ ] Testé sur Windows
- [ ] Testé sur Ubuntu/Linux
- [ ] Chemins de polices adaptés
- [ ] Encodage UTF-8 correct
- [ ] Compatibilité navigateurs web

#### Documentation
- [ ] API documentée
- [ ] Exemples d'utilisation
- [ ] Guide de référence des formes
- [ ] Tutoriels pour débutants

## Comparaison des approches

### Génération manuelle vs Bibliothèque

| Critère | Manuelle (TStringList) | Bibliothèque (TSVGGenerator) |
|---------|------------------------|------------------------------|
| **Contrôle** | ⭐⭐⭐⭐⭐ Total | ⭐⭐⭐ Bon |
| **Simplicité** | ⭐⭐ Complexe | ⭐⭐⭐⭐⭐ Simple |
| **Maintenance** | ⭐⭐ Difficile | ⭐⭐⭐⭐ Facile |
| **Flexibilité** | ⭐⭐⭐⭐⭐ Maximale | ⭐⭐⭐⭐ Bonne |
| **Performance** | ⭐⭐⭐⭐ Rapide | ⭐⭐⭐⭐ Rapide |
| **Courbe d'apprentissage** | ⭐⭐ Raide | ⭐⭐⭐⭐⭐ Douce |

**Recommandation** : Utilisez une bibliothèque pour commencer, passez au manuel pour des besoins très spécifiques.

## Cas d'usage réels

### 1. Génération de rapports

```pascal
unit ReportSVG;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SVGGenerator;

type
  TReportGenerator = class
  private
    FSVG: TSVGGenerator;
    FCurrentY: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure StartReport(const ATitle: string);
    procedure AddSection(const ATitle: string);
    procedure AddParagraph(const AText: string);
    procedure AddBarChart(AValues: array of Integer; ALabels: array of string);
    procedure AddTable(AData: array of array of string);

    procedure SaveReport(const AFileName: string);
  end;

implementation

constructor TReportGenerator.Create;  
begin
  inherited Create;
  FSVG := TSVGGenerator.Create(800, 1200);
  FCurrentY := 50;
end;

destructor TReportGenerator.Destroy;  
begin
  FSVG.Free;
  inherited Destroy;
end;

procedure TReportGenerator.StartReport(const ATitle: string);  
begin
  // Fond
  FSVG.AddRectangle(0, 0, 800, 1200, clWhite, clNone);

  // Titre principal
  FSVG.AddText(50, FCurrentY, ATitle, 28, clNavy);
  FCurrentY := FCurrentY + 50;

  // Ligne de séparation
  FSVG.AddLine(50, FCurrentY, 750, FCurrentY, clGray, 2);
  FCurrentY := FCurrentY + 30;
end;

procedure TReportGenerator.AddSection(const ATitle: string);  
begin
  FSVG.AddText(50, FCurrentY, ATitle, 20, clBlack);
  FCurrentY := FCurrentY + 35;
end;

procedure TReportGenerator.AddParagraph(const AText: string);  
begin
  FSVG.AddText(50, FCurrentY, AText, 12, clBlack);
  FCurrentY := FCurrentY + 20;
end;

procedure TReportGenerator.AddBarChart(AValues: array of Integer;
  ALabels: array of string);
var
  i, MaxValue, BarWidth, X: Integer;
  BarHeight: Double;
  Colors: array[0..4] of TColor = (clRed, clGreen, clBlue, clYellow, clPurple);
begin
  MaxValue := 0;
  for i := 0 to High(AValues) do
    if AValues[i] > MaxValue then
      MaxValue := AValues[i];

  BarWidth := 60;

  for i := 0 to High(AValues) do
  begin
    X := 100 + (i * 100);
    BarHeight := (AValues[i] / MaxValue) * 150;

    FSVG.AddRectangle(X, FCurrentY + Round(150 - BarHeight),
                     BarWidth, Round(BarHeight),
                     Colors[i mod 5], clBlack, 1);

    FSVG.AddText(X + 10, FCurrentY + 170, ALabels[i], 10, clBlack);
  end;

  FCurrentY := FCurrentY + 200;
end;

procedure TReportGenerator.AddTable(AData: array of array of string);  
var
  Row, Col: Integer;
  CellWidth, CellHeight: Integer;
  X, Y: Integer;
begin
  CellWidth := 150;
  CellHeight := 25;

  for Row := 0 to High(AData) do
  begin
    for Col := 0 to High(AData[Row]) do
    begin
      X := 50 + (Col * CellWidth);
      Y := FCurrentY + (Row * CellHeight);

      // Cellule
      if Row = 0 then
        FSVG.AddRectangle(X, Y, CellWidth, CellHeight,
                         RGB(200, 200, 200), clBlack, 1)
      else
        FSVG.AddRectangle(X, Y, CellWidth, CellHeight,
                         clWhite, clBlack, 1);

      // Texte
      FSVG.AddText(X + 5, Y + 17, AData[Row][Col], 11, clBlack);
    end;
  end;

  FCurrentY := FCurrentY + ((High(AData) + 1) * CellHeight) + 30;
end;

procedure TReportGenerator.SaveReport(const AFileName: string);  
begin
  FSVG.SaveToFile(AFileName);
end;

end.
```

### 2. Visualisation de données en temps réel

```pascal
unit LiveDataSVG;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SVGGenerator;

type
  TDataPoint = record
    Time: TDateTime;
    Value: Double;
  end;

  TLiveChartSVG = class
  private
    FDataPoints: array of TDataPoint;
    FMaxPoints: Integer;
  public
    constructor Create(AMaxPoints: Integer = 100);

    procedure AddDataPoint(AValue: Double);
    procedure ClearData;
    function GenerateSVG: string;
    procedure SaveToFile(const AFileName: string);
  end;

implementation

uses
  DateUtils;

constructor TLiveChartSVG.Create(AMaxPoints: Integer);  
begin
  inherited Create;
  FMaxPoints := AMaxPoints;
  SetLength(FDataPoints, 0);
end;

procedure TLiveChartSVG.AddDataPoint(AValue: Double);  
var
  Len: Integer;
begin
  Len := Length(FDataPoints);

  // Limiter le nombre de points
  if Len >= FMaxPoints then
  begin
    // Décaler tous les points
    Move(FDataPoints[1], FDataPoints[0], (Len - 1) * SizeOf(TDataPoint));
    FDataPoints[Len - 1].Time := Now;
    FDataPoints[Len - 1].Value := AValue;
  end
  else
  begin
    SetLength(FDataPoints, Len + 1);
    FDataPoints[Len].Time := Now;
    FDataPoints[Len].Value := AValue;
  end;
end;

procedure TLiveChartSVG.ClearData;  
begin
  SetLength(FDataPoints, 0);
end;

function TLiveChartSVG.GenerateSVG: string;  
var
  SVG: TSVGGenerator;
  i: Integer;
  X, Y: Integer;
  MinValue, MaxValue, Range: Double;
  PathData: string;
  ScaleX, ScaleY: Double;
begin
  SVG := TSVGGenerator.Create(800, 400);
  try
    // Fond
    SVG.AddRectangle(0, 0, 800, 400, clWhite, clNone);

    // Titre
    SVG.AddText(300, 30, 'Données en temps réel', 18, clBlack);

    if Length(FDataPoints) < 2 then
    begin
      SVG.AddText(300, 200, 'Pas assez de données', 14, clGray);
      Result := SVG.GetSVG;
      Exit;
    end;

    // Trouver min et max
    MinValue := FDataPoints[0].Value;
    MaxValue := FDataPoints[0].Value;

    for i := 1 to High(FDataPoints) do
    begin
      if FDataPoints[i].Value < MinValue then
        MinValue := FDataPoints[i].Value;
      if FDataPoints[i].Value > MaxValue then
        MaxValue := FDataPoints[i].Value;
    end;

    Range := MaxValue - MinValue;
    if Range = 0 then Range := 1;

    // Axes
    SVG.AddLine(50, 50, 50, 350, clBlack, 2);
    SVG.AddLine(50, 350, 750, 350, clBlack, 2);

    // Labels axes
    SVG.AddText(10, 355, Format('%.1f', [MinValue]), 10, clBlack);
    SVG.AddText(10, 55, Format('%.1f', [MaxValue]), 10, clBlack);

    // Tracer la courbe
    ScaleX := 700 / FMaxPoints;
    ScaleY := 300 / Range;

    PathData := '';
    for i := 0 to High(FDataPoints) do
    begin
      X := 50 + Round(i * ScaleX);
      Y := 350 - Round((FDataPoints[i].Value - MinValue) * ScaleY);

      if i = 0 then
        PathData := Format('M %d %d', [X, Y])
      else
        PathData := PathData + Format(' L %d %d', [X, Y]);
    end;

    SVG.AddPath(PathData, clNone, clBlue, 2);

    // Points
    for i := 0 to High(FDataPoints) do
    begin
      X := 50 + Round(i * ScaleX);
      Y := 350 - Round((FDataPoints[i].Value - MinValue) * ScaleY);
      SVG.AddCircle(X, Y, 3, clRed, clRed, 1);
    end;

    // Timestamp
    SVG.AddText(600, 385, FormatDateTime('hh:nn:ss', Now), 10, clGray);

    Result := SVG.GetSVG;
  finally
    SVG.Free;
  end;
end;

procedure TLiveChartSVG.SaveToFile(const AFileName: string);  
var
  Output: TStringList;
begin
  Output := TStringList.Create;
  try
    Output.Text := GenerateSVG;
    Output.SaveToFile(AFileName);
  finally
    Output.Free;
  end;
end;

end.
```

### 3. Export de diagrammes UML

```pascal
unit UMLDiagramSVG;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SVGGenerator, SVGShapes;

type
  TUMLClass = class
    Name: string;
    Attributes: TStringList;
    Methods: TStringList;
    X, Y: Integer;
    destructor Destroy; override;
  end;

  TRelationType = (rtAssociation, rtInheritance, rtComposition);

  TUMLRelation = record
    FromClass: TUMLClass;
    ToClass: TUMLClass;
    RelationType: TRelationType;
  end;

  TUMLDiagramGenerator = class
  private
    FClasses: TList;
    FRelations: array of TUMLRelation;

    procedure DrawClass(ASVG: TSVGGenerator; AClass: TUMLClass);
    procedure DrawRelation(ASVG: TSVGGenerator; const ARelation: TUMLRelation);
  public
    constructor Create;
    destructor Destroy; override;

    function AddClass(const AName: string; X, Y: Integer): TUMLClass;
    procedure AddRelation(AFrom, ATo: TUMLClass;
                         AType: TRelationType);

    procedure GenerateDiagram(const AFileName: string);
  end;

implementation

destructor TUMLClass.Destroy;  
begin
  Attributes.Free;
  Methods.Free;
  inherited Destroy;
end;

constructor TUMLDiagramGenerator.Create;  
begin
  inherited Create;
  FClasses := TList.Create;
  SetLength(FRelations, 0);
end;

destructor TUMLDiagramGenerator.Destroy;  
var
  i: Integer;
begin
  for i := 0 to FClasses.Count - 1 do
    TUMLClass(FClasses[i]).Free;
  FClasses.Free;
  inherited Destroy;
end;

function TUMLDiagramGenerator.AddClass(const AName: string;
  X, Y: Integer): TUMLClass;
begin
  Result := TUMLClass.Create;
  Result.Name := AName;
  Result.Attributes := TStringList.Create;
  Result.Methods := TStringList.Create;
  Result.X := X;
  Result.Y := Y;

  FClasses.Add(Result);
end;

procedure TUMLDiagramGenerator.AddRelation(AFrom, ATo: TUMLClass;
  AType: TRelationType);
var
  Len: Integer;
begin
  Len := Length(FRelations);
  SetLength(FRelations, Len + 1);

  FRelations[Len].FromClass := AFrom;
  FRelations[Len].ToClass := ATo;
  FRelations[Len].RelationType := AType;
end;

procedure TUMLDiagramGenerator.DrawClass(ASVG: TSVGGenerator;
  AClass: TUMLClass);
var
  Width, Height: Integer;
  Y, i: Integer;
begin
  Width := 200;
  Height := 80 + (AClass.Attributes.Count + AClass.Methods.Count) * 20;

  // Rectangle principal
  ASVG.AddRectangle(AClass.X, AClass.Y, Width, Height,
                   clWhite, clBlack, 2);

  // Nom de la classe
  ASVG.AddRectangle(AClass.X, AClass.Y, Width, 30,
                   RGB(200, 220, 255), clBlack, 2);
  ASVG.AddText(AClass.X + 10, AClass.Y + 20, AClass.Name, 14, clBlack);

  Y := AClass.Y + 35;

  // Séparateur
  ASVG.AddLine(AClass.X, Y, AClass.X + Width, Y, clBlack, 1);
  Y := Y + 10;

  // Attributs
  for i := 0 to AClass.Attributes.Count - 1 do
  begin
    ASVG.AddText(AClass.X + 10, Y, '- ' + AClass.Attributes[i], 11, clBlack);
    Y := Y + 20;
  end;

  // Séparateur
  ASVG.AddLine(AClass.X, Y, AClass.X + Width, Y, clBlack, 1);
  Y := Y + 10;

  // Méthodes
  for i := 0 to AClass.Methods.Count - 1 do
  begin
    ASVG.AddText(AClass.X + 10, Y, '+ ' + AClass.Methods[i], 11, clBlack);
    Y := Y + 20;
  end;
end;

procedure TUMLDiagramGenerator.DrawRelation(ASVG: TSVGGenerator;
  const ARelation: TUMLRelation);
var
  X1, Y1, X2, Y2: Integer;
begin
  // Points de connexion (centres des classes)
  X1 := ARelation.FromClass.X + 100;
  Y1 := ARelation.FromClass.Y + 40;
  X2 := ARelation.ToClass.X + 100;
  Y2 := ARelation.ToClass.Y + 40;

  case ARelation.RelationType of
    rtAssociation:
      begin
        ASVG.AddLine(X1, Y1, X2, Y2, clBlack, 2);
      end;

    rtInheritance:
      begin
        ASVG.AddLine(X1, Y1, X2, Y2, clBlack, 2);
        // Triangle pour l'héritage
        TSVGShapeLibrary.AddArrow(ASVG, X1, Y1, X2, Y2, 15, clBlack);
      end;

    rtComposition:
      begin
        ASVG.AddLine(X1, Y1, X2, Y2, clBlack, 2);
        // Losange pour la composition
        ASVG.AddCircle(X1, Y1, 8, clBlack, clBlack, 2);
      end;
  end;
end;

procedure TUMLDiagramGenerator.GenerateDiagram(const AFileName: string);  
var
  SVG: TSVGGenerator;
  i: Integer;
  MaxX, MaxY: Integer;
begin
  // Calculer la taille nécessaire
  MaxX := 0;
  MaxY := 0;

  for i := 0 to FClasses.Count - 1 do
  begin
    if TUMLClass(FClasses[i]).X + 250 > MaxX then
      MaxX := TUMLClass(FClasses[i]).X + 250;
    if TUMLClass(FClasses[i]).Y + 200 > MaxY then
      MaxY := TUMLClass(FClasses[i]).Y + 200;
  end;

  SVG := TSVGGenerator.Create(MaxX, MaxY);
  try
    // Fond
    SVG.AddRectangle(0, 0, MaxX, MaxY, RGB(250, 250, 250), clNone);

    // Dessiner les relations d'abord (en arrière-plan)
    for i := 0 to High(FRelations) do
      DrawRelation(SVG, FRelations[i]);

    // Dessiner les classes
    for i := 0 to FClasses.Count - 1 do
      DrawClass(SVG, TUMLClass(FClasses[i]));

    SVG.SaveToFile(AFileName);
  finally
    SVG.Free;
  end;
end;

end.
```

## Débogage et dépannage

### Problèmes courants

#### 1. SVG ne s'affiche pas

```pascal
// ✅ Vérifier la structure XML
procedure ValidateSVG(const AFileName: string);  
var
  Doc: TXMLDocument;
begin
  try
    ReadXMLFile(Doc, AFileName);
    try
      WriteLn('SVG valide !');
    finally
      Doc.Free;
    end;
  except
    on E: Exception do
      WriteLn('Erreur XML : ', E.Message);
  end;
end;
```

#### 2. Coordonnées incorrectes

```pascal
// ✅ Ajouter des vérifications
procedure SafeAddRectangle(ASVG: TSVGGenerator;
  X, Y, Width, Height: Integer);
begin
  if (Width <= 0) or (Height <= 0) then
  begin
    WriteLn('Warning: Invalid dimensions');
    Exit;
  end;

  ASVG.AddRectangle(X, Y, Width, Height, clWhite, clBlack, 1);
end;
```

#### 3. Fichiers trop volumineux

```pascal
// ✅ Optimiser avant sauvegarde
procedure OptimizeAndSave(ASVG: TSVGGenerator; const AFileName: string);  
var
  SVGContent: string;
  Optimizer: TSVGOptimizer;
begin
  SVGContent := ASVG.GetSVG;
  SVGContent := Optimizer.OptimizeSVG(SVGContent);

  with TStringList.Create do
  try
    Text := SVGContent;
    SaveToFile(AFileName);
  finally
    Free;
  end;
end;
```

## Conclusion finale

### Ce que vous avez appris

1. **Structure SVG** : Format XML, éléments de base
2. **Génération** : Création programmatique de SVG
3. **Formes avancées** : Chemins, transformations, gradients
4. **Optimisation** : Réduction de taille, réutilisation
5. **Cas pratiques** : Rapports, graphiques, diagrammes
6. **Multi-plateforme** : Windows et Linux

### Avantages de SVG dans vos applications

- 📊 **Visualisation de données** : Graphiques scalables et interactifs
- 📄 **Génération de rapports** : Documents vectoriels de haute qualité
- 🎨 **Interfaces riches** : Icônes et illustrations sans perte
- 📱 **Responsive design** : S'adapte à toutes les résolutions
- 💾 **Export universel** : Compatible avec tous les systèmes

### Prochaines étapes recommandées

1. **Créez une bibliothèque personnalisée** de formes réutilisables
2. **Implémentez des animations** pour rendre vos SVG vivants
3. **Ajoutez l'interactivité** avec des événements SVG
4. **Intégrez avec votre application** existante
5. **Explorez les filtres avancés** pour des effets visuels

### Ressources pour approfondir

#### Tutoriels en ligne
- **MDN Web Docs** : Guide SVG complet
- **SVG Tutorial** : W3Schools
- **SVG Pocket Guide** : Livre gratuit en ligne

#### Outils
- **Inkscape** : Pour concevoir visuellement
- **SVGOMG** : Optimiseur en ligne
- **SVG Path Editor** : Éditeur de chemins

#### Communauté
- **Forum Lazarus** : Section graphiques
- **Stack Overflow** : Tag "svg" et "freepascal"
- **GitHub** : Projets SVG en Pascal

---

**Félicitations !** Vous maîtrisez maintenant les bases et les concepts avancés des graphiques vectoriels SVG avec FreePascal et Lazarus. N'hésitez pas à expérimenter et à créer vos propres visualisations !

🎉 **Fin du chapitre 12.8 - Graphiques vectoriels SVG**

**Pour continuer**, explorez le chapitre suivant :
- **12.9 Traitement d'images avancé**
- **12.10 Vision par ordinateur avec OpenCV**
- **12.11 Accélération GPU (CUDA/OpenCL)**

⏭️ [Traitement d'images avancé](/12-interfaces-graphiques-avancees/09-traitement-images-avance.md)
