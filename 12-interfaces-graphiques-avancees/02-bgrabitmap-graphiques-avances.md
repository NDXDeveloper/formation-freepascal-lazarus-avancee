🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 12.2 BGRABitmap pour graphiques avancés

## Introduction

**BGRABitmap** est une bibliothèque puissante et mature pour FreePascal/Lazarus qui étend considérablement les capacités graphiques du Canvas standard. Elle offre des fonctionnalités avancées comme l'anti-aliasing complet, la transparence alpha, les dégradés complexes, les effets visuels et bien plus encore.

Contrairement au Canvas standard qui est limité par les API natives (GDI sur Windows, Cairo sur Linux), BGRABitmap fournit un moteur de rendu logiciel uniforme qui garantit un **rendu identique sur toutes les plateformes** (Windows, Ubuntu/Linux, macOS).

---

## Pourquoi utiliser BGRABitmap ?

### Avantages par rapport au Canvas standard

| Fonctionnalité | Canvas standard | BGRABitmap |
|----------------|-----------------|------------|
| **Anti-aliasing** | Limité/inexistant | Complet sur tout |
| **Transparence alpha** | Partielle | Complète (canal alpha 8 bits) |
| **Dégradés** | Basiques | Avancés (linéaires, radiaux, coniques) |
| **Effets visuels** | Aucun | Nombreux (flou, ombre, lueur, etc.) |
| **Performance** | Variable selon OS | Optimisée et cohérente |
| **Portabilité** | Rendu différent par OS | Rendu identique partout |
| **Transformations** | Limitées | Rotation, redimensionnement, perspective |

### Cas d'usage typiques

- Applications graphiques (éditeurs d'images, dessins)
- Jeux 2D avec graphismes de qualité
- Visualisation de données avec rendu professionnel
- Tableaux de bord et interfaces modernes
- Génération d'images dynamiques (rapports, exports)
- Effets visuels temps réel

---

## Installation de BGRABitmap

### Via l'Online Package Manager (OPM)

La méthode la plus simple :

1. **Ouvrir Lazarus**
2. Menu **Paquet** → **Online Package Manager**
3. Rechercher `bgrabitmap`
4. Sélectionner **BGRABitmap** et cliquer sur **Installer**
5. Recompiler l'IDE quand demandé

### Installation manuelle

**Sur Windows et Ubuntu** :

```bash
# 1. Télécharger depuis GitHub
git clone https://github.com/bgrabitmap/bgrabitmap.git

# 2. Dans Lazarus : Paquet → Ouvrir un fichier paquet (.lpk)
# 3. Ouvrir : bgrabitmap/bgrabitmap/bgrabitmappack.lpk
# 4. Cliquer sur "Compiler" puis "Utiliser" → "Installer"
# 5. Recompiler l'IDE
```

### Vérification de l'installation

Après installation, vous devriez avoir accès aux unités :

```pascal
uses
  BGRABitmap,           // Types de base et bitmap
  BGRABitmapTypes,      // Types et constantes
  BGRACanvas,           // Canvas compatible BGRABitmap
  BGRACanvas2D,         // Canvas style HTML5
  BGRAGradients,        // Dégradés
  BGRATransform,        // Transformations
  BGRAFilters,          // Filtres et effets
  BGRAPath,             // Chemins vectoriels
  BGRAText;             // Rendu de texte avancé
```

---

## Les bases de TBGRABitmap

### Créer un TBGRABitmap

```pascal
uses
  BGRABitmap, BGRABitmapTypes;

var
  Bmp: TBGRABitmap;
begin
  // Création avec dimensions
  Bmp := TBGRABitmap.Create(800, 600);
  try
    // Utiliser le bitmap...
  finally
    Bmp.Free;  // Toujours libérer !
  end;
end;
```

### Couleurs avec canal alpha

BGRABitmap utilise le type `TBGRAPixel` pour représenter les couleurs RGBA :

```pascal
var
  Color: TBGRAPixel;
begin
  // Création d'une couleur avec alpha
  Color := BGRA(255, 0, 0, 255);      // Rouge opaque (R, G, B, Alpha)
  Color := BGRA(0, 255, 0, 128);      // Vert semi-transparent
  Color := BGRA(0, 0, 255, 0);        // Bleu complètement transparent

  // Couleurs prédéfinies
  Color := BGRAWhite;
  Color := BGRABlack;
  Color := BGRAPixelTransparent;

  // Conversion depuis TColor
  Color := ColorToBGRA(clRed);
  Color := ColorToBGRA(clBlue, 128);  // Avec alpha personnalisé
end;
```

**Composantes d'un TBGRAPixel** :
- `red` : 0-255 (composante rouge)
- `green` : 0-255 (composante verte)
- `blue` : 0-255 (composante bleue)
- `alpha` : 0-255 (0 = transparent, 255 = opaque)

### Remplir avec une couleur

```pascal
Bmp.Fill(BGRA(200, 220, 255));          // Fond bleu clair  
Bmp.Fill(BGRA(255, 255, 255, 128));     // Blanc semi-transparent  
Bmp.FillTransparent;                     // Remplir en transparent
```

---

## Dessin de base avec anti-aliasing

### Lignes lissées

```pascal
// Ligne simple avec anti-aliasing
Bmp.DrawLineAntialias(10, 10, 200, 150, BGRA(255, 0, 0), 2);  // Rouge, épaisseur 2

// Ligne avec transparence
Bmp.DrawLineAntialias(10, 10, 200, 150, BGRA(0, 0, 255, 128), 3);
```

### Rectangles et ellipses

```pascal
// Rectangle avec contour lissé
Bmp.RectangleAntialias(50, 50, 200, 150, BGRA(255, 0, 0), 2);

// Rectangle rempli avec anti-aliasing
Bmp.FillRectAntialias(50, 50, 200, 150, BGRA(0, 255, 0, 128));

// Ellipse lissée
Bmp.EllipseAntialias(125, 100, 75, 50, BGRA(0, 0, 255), 2);

// Ellipse remplie
Bmp.FillEllipseAntialias(125, 100, 75, 50, BGRA(255, 255, 0, 200));
```

### Cercles parfaits

```pascal
// Cercle (ellipse avec rayon égal)
Bmp.EllipseAntialias(150, 150, 80, 80, BGRA(255, 0, 255), 3);

// Cercle rempli
Bmp.FillEllipseAntialias(150, 150, 80, 80, BGRA(0, 255, 255));
```

### Polygones

```pascal
var
  Points: array of TPointF;
begin
  SetLength(Points, 3);
  Points[0] := PointF(100, 50);
  Points[1] := PointF(50, 150);
  Points[2] := PointF(150, 150);

  // Polygone avec contour lissé
  Bmp.DrawPolygonAntialias(Points, BGRA(255, 0, 0), 2);

  // Polygone rempli
  Bmp.FillPolyAntialias(Points, BGRA(0, 255, 0, 128));
end;
```

**Note** : BGRABitmap utilise `TPointF` (coordonnées flottantes) pour un positionnement sub-pixel et un meilleur anti-aliasing.

---

## Dégradés avancés

### Dégradé linéaire

```pascal
// Dégradé du rouge au bleu (horizontal)
Bmp.GradientFill(
  0, 0, Bmp.Width, Bmp.Height,
  BGRA(255, 0, 0),      // Couleur de départ
  BGRA(0, 0, 255),      // Couleur de fin
  gtLinear,             // Type : dégradé linéaire
  PointF(0, 0),         // Point de départ
  PointF(Bmp.Width, 0), // Point d'arrivée
  dmSet                 // Mode de dessin
);

// Dégradé vertical
Bmp.GradientFill(
  0, 0, Bmp.Width, Bmp.Height,
  BGRA(255, 255, 0),
  BGRA(255, 0, 255),
  gtLinear,
  PointF(0, 0),
  PointF(0, Bmp.Height),
  dmSet
);
```

### Dégradé radial

```pascal
// Dégradé du centre vers l'extérieur
Bmp.GradientFill(
  0, 0, Bmp.Width, Bmp.Height,
  BGRAWhite,
  BGRA(0, 100, 200),
  gtRadial,                                    // Type radial
  PointF(Bmp.Width / 2, Bmp.Height / 2),      // Centre
  PointF(Bmp.Width, Bmp.Height / 2),          // Rayon
  dmSet
);
```

### Dégradé multi-couleurs

```pascal
uses
  BGRAGradients;

var
  Gradient: TBGRAGradientScanner;
begin
  Gradient := TBGRAGradientScanner.Create(
    BGRA(255, 0, 0),    // Rouge
    BGRA(255, 255, 0),  // Jaune
    gtLinear
  );
  try
    // Ajouter des couleurs intermédiaires
    Gradient.AddColor(BGRA(0, 255, 0), 0.5);  // Vert à 50%
    Gradient.AddColor(BGRA(0, 0, 255), 0.75); // Bleu à 75%

    Bmp.FillRect(0, 0, Bmp.Width, Bmp.Height, Gradient, dmSet);
  finally
    Gradient.Free;
  end;
end;
```

### Types de dégradés disponibles

- `gtLinear` : Dégradé linéaire
- `gtRadial` : Dégradé radial (du centre)
- `gtReflected` : Dégradé réfléchi
- `gtDiamond` : Dégradé en forme de diamant
- `gtAngular` : Dégradé angulaire (conique)

---

## Texte avec rendu de qualité

### Texte simple

```pascal
Bmp.TextOut(50, 50, 'Hello BGRABitmap!', BGRA(0, 0, 0));
```

### Texte avec police personnalisée

```pascal
Bmp.FontName := 'Arial';  
Bmp.FontHeight := 24;  
Bmp.FontStyle := [fsBold];  
Bmp.FontQuality := fqFineAntialiasing;  // Meilleur anti-aliasing

Bmp.TextOut(50, 50, 'Texte de qualité', BGRA(0, 0, 128));
```

### Texte avec ombre

```pascal
// Ombre
Bmp.TextOut(52, 52, 'Avec ombre', BGRA(0, 0, 0, 100));
// Texte principal
Bmp.TextOut(50, 50, 'Avec ombre', BGRA(255, 255, 255));
```

### Texte avec contour

```pascal
uses
  BGRAText, BGRATextFX;

var
  TextEffect: TBGRATextEffect;
begin
  TextEffect := TBGRATextEffect.Create('Texte avec contour', 'Arial', 36, True, False);
  try
    TextEffect.DrawOutline(Bmp, 100, 100, BGRA(255, 0, 0), 2);  // Contour rouge
    TextEffect.Draw(Bmp, 100, 100, BGRA(255, 255, 255));        // Texte blanc
  finally
    TextEffect.Free;
  end;
end;
```

### Mesurer le texte

```pascal
var
  TextSize: TSize;
begin
  Bmp.FontName := 'Arial';
  Bmp.FontHeight := 20;
  TextSize := Bmp.TextSize('Mon texte');

  ShowMessage(Format('Largeur: %d, Hauteur: %d', [TextSize.cx, TextSize.cy]));
end;
```

---

## Transformations géométriques

### Rotation

```pascal
var
  Original, Rotated: TBGRABitmap;
begin
  Original := TBGRABitmap.Create(200, 100);
  Original.Fill(BGRA(255, 200, 200));
  Original.TextOut(50, 40, 'Rotation', BGRABlack);

  // Rotation de 45 degrés
  Rotated := Original.RotateCW;  // 90° sens horaire
  // OU
  Rotated := Original.RotateAffine(45, 0, 0);  // Angle personnalisé

  try
    Bmp.PutImage(100, 100, Rotated, dmDrawWithTransparency);
  finally
    Original.Free;
    Rotated.Free;
  end;
end;
```

### Redimensionnement de qualité

```pascal
var
  Original, Resized: TBGRABitmap;
begin
  Original := TBGRABitmap.Create('photo.jpg');

  // Redimensionnement avec haute qualité
  Resized := Original.Resample(400, 300, rmFineResample);

  try
    Bmp.PutImage(0, 0, Resized, dmSet);
  finally
    Original.Free;
    Resized.Free;
  end;
end;
```

**Modes de redimensionnement** :
- `rmSimpleStretch` : Rapide mais qualité moyenne
- `rmFineResample` : Haute qualité, recommandé
- `rmBestQuality` : Qualité maximale (plus lent)

### Miroir et retournement

```pascal
var
  Original, Flipped: TBGRABitmap;
begin
  Original := TBGRABitmap.Create(200, 100);
  Original.Fill(BGRA(200, 255, 200));
  Original.TextOut(50, 40, 'Miroir', BGRABlack);

  // Miroir horizontal
  Flipped := Original.Duplicate;
  Flipped.HorizontalFlip;

  // Miroir vertical
  // Flipped.VerticalFlip;

  try
    Bmp.PutImage(0, 0, Flipped, dmSet);
  finally
    Original.Free;
    Flipped.Free;
  end;
end;
```

---

## Effets visuels

### Flou (Blur)

```pascal
uses
  BGRAFilters;

// Flou gaussien
BGRAReplace(Bmp, Bmp.FilterBlurRadial(10, rbFast));

// Flou directionnel
BGRAReplace(Bmp, Bmp.FilterBlurMotion(20, 45, False));

// Flou de boîte (Box Blur) - plus rapide
BGRAReplace(Bmp, Bmp.FilterBlurBox(5, 5, 1));
```

**Note** : `BGRAReplace` remplace le bitmap existant par la version filtrée et libère l'ancien.

### Ombre portée (Drop Shadow)

```pascal
var
  Shadow: TBGRABitmap;
  Original: TBGRABitmap;
begin
  Original := TBGRABitmap.Create(200, 100);
  Original.FillTransparent;
  Original.FillEllipseAntialias(100, 50, 80, 40, BGRA(255, 200, 0));

  // Créer l'ombre
  Shadow := Original.FilterBlurRadial(5, rbFast);
  Shadow.ApplyGlobalOpacity(128);  // Semi-transparente

  // Dessiner ombre puis forme
  Bmp.PutImage(55, 55, Shadow, dmDrawWithTransparency);
  Bmp.PutImage(50, 50, Original, dmDrawWithTransparency);

  Original.Free;
  Shadow.Free;
end;
```

### Lueur (Glow)

```pascal
var
  Glow: TBGRABitmap;
begin
  // Créer un cercle
  Bmp.FillEllipseAntialias(150, 150, 50, 50, BGRA(255, 255, 0));

  // Créer la lueur
  Glow := Bmp.Duplicate;
  BGRAReplace(Glow, Glow.FilterBlurRadial(15, rbFast));

  // Appliquer la lueur
  Bmp.BlendImage(0, 0, Glow, boAdditive);  // Mode additif pour la lueur

  Glow.Free;
end;
```

### Accentuation (Sharpen)

```pascal
BGRAReplace(Bmp, Bmp.FilterSharpen(10));
```

### Ajustements de couleur

```pascal
// Luminosité (-255 à +255)
Bmp.ApplyGlobalOpacity(200);  // Plus clair  
Bmp.LinearNegative;           // Négatif

// Contraste
BGRAReplace(Bmp, Bmp.FilterContour);

// Niveaux de gris
BGRAReplace(Bmp, Bmp.FilterGrayscale);
```

### Effets artistiques

```pascal
// Effet huile (Oil painting)
BGRAReplace(Bmp, Bmp.FilterMedian(moLowSmooth));

// Effet posterize
Bmp.ApplyGlobalOpacity(200);

// Pixelisation
BGRAReplace(Bmp, Bmp.FilterPixelate(10, False, True));
```

---

## Chemins vectoriels (Paths)

Les chemins permettent de créer des formes complexes et de les réutiliser.

### Créer un chemin

```pascal
uses
  BGRAPath;

var
  Path: TBGRAPath;
begin
  Path := TBGRAPath.Create;
  try
    // Démarrer au point (50, 50)
    Path.moveTo(50, 50);

    // Lignes
    Path.lineTo(150, 50);
    Path.lineTo(150, 150);
    Path.lineTo(50, 150);
    Path.closePath;  // Fermer le chemin

    // Dessiner le chemin
    Bmp.DrawPath(Path, BGRA(255, 0, 0), 2);
    Bmp.FillPath(Path, BGRA(255, 255, 0, 128));
  finally
    Path.Free;
  end;
end;
```

### Courbes de Bézier

```pascal
Path := TBGRAPath.Create;  
try
  Path.moveTo(50, 150);

  // Courbe de Bézier quadratique
  Path.quadraticCurveTo(
    150, 50,   // Point de contrôle
    250, 150   // Point d'arrivée
  );

  // Courbe de Bézier cubique
  Path.bezierCurveTo(
    300, 50,   // Premier point de contrôle
    400, 50,   // Deuxième point de contrôle
    450, 150   // Point d'arrivée
  );

  Bmp.DrawPath(Path, BGRA(0, 0, 255), 3);
finally
  Path.Free;
end;
```

### Arcs et cercles

```pascal
Path := TBGRAPath.Create;  
try
  // Arc
  Path.arc(150, 150, 80, 0, Pi);  // Centre, rayon, angle début, angle fin

  // Cercle complet
  Path.circle(300, 150, 60);

  Bmp.DrawPath(Path, BGRA(255, 0, 255), 2);
finally
  Path.Free;
end;
```

---

## Canvas2D : API style HTML5

BGRABitmap offre une API inspirée du Canvas HTML5 pour ceux qui connaissent le web.

### Initialisation

```pascal
uses
  BGRACanvas2D;

var
  Canvas2D: TBGRACanvas2D;
begin
  Canvas2D := TBGRACanvas2D.Create(Bmp);
  try
    // Utiliser l'API Canvas2D
  finally
    Canvas2D.Free;
  end;
end;
```

### Exemples d'utilisation

```pascal
Canvas2D := TBGRACanvas2D.Create(Bmp);  
try
  // Styles
  Canvas2D.fillStyle := 'rgba(255, 0, 0, 0.5)';
  Canvas2D.strokeStyle := 'blue';
  Canvas2D.lineWidth := 3;

  // Rectangle
  Canvas2D.fillRect(50, 50, 150, 100);
  Canvas2D.strokeRect(50, 50, 150, 100);

  // Chemin
  Canvas2D.beginPath;
  Canvas2D.moveTo(100, 200);
  Canvas2D.lineTo(200, 250);
  Canvas2D.lineTo(100, 300);
  Canvas2D.closePath;
  Canvas2D.fill;
  Canvas2D.stroke;

  // Texte
  Canvas2D.font := 'bold 24px Arial';
  Canvas2D.fillStyle := 'black';
  Canvas2D.fillText('Hello Canvas2D!', 50, 400);
finally
  Canvas2D.Free;
end;
```

---

## Performances et optimisations

### Conseils généraux

1. **Réutiliser les TBGRABitmap** plutôt que de les recréer constamment
2. **Utiliser les bonnes dimensions** : ne pas créer de bitmaps trop grands
3. **Libérer la mémoire** : toujours `Free` les TBGRABitmap
4. **Clipper les zones** : ne redessiner que ce qui est nécessaire
5. **Choisir le bon filtre** : `rbFast` vs `rbPrecise` pour le flou

### Exemple optimisé

```pascal
type
  TMyForm = class(TForm)
  private
    FBuffer: TBGRABitmap;  // Buffer réutilisé
  public
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Redraw;
  end;

procedure TMyForm.FormCreate(Sender: TObject);  
begin
  FBuffer := TBGRABitmap.Create(ClientWidth, ClientHeight);
end;

procedure TMyForm.FormDestroy(Sender: TObject);  
begin
  FBuffer.Free;
end;

procedure TMyForm.Redraw;  
begin
  // Réutiliser le même buffer
  FBuffer.Fill(BGRAWhite);
  FBuffer.DrawLineAntialias(0, 0, FBuffer.Width, FBuffer.Height, BGRABlack, 2);

  // Afficher sur le formulaire
  FBuffer.Draw(Canvas, 0, 0, True);
end;
```

### Modes de dessin (DrawMode)

Comprendre les modes de dessin améliore les performances :

- `dmSet` : Remplace les pixels (le plus rapide)
- `dmDrawWithTransparency` : Respecte le canal alpha (standard)
- `dmSetExceptTransparent` : Ignore les pixels transparents
- `dmLinearBlend` : Mélange linéaire
- `dmXor` : XOR logique

---

## Intégration avec le Canvas standard

### Dessiner un TBGRABitmap sur un Canvas

```pascal
procedure TForm1.FormPaint(Sender: TObject);  
var
  Bmp: TBGRABitmap;
begin
  Bmp := TBGRABitmap.Create(400, 300);
  try
    Bmp.Fill(BGRA(200, 220, 255));
    Bmp.FillEllipseAntialias(200, 150, 100, 80, BGRA(255, 200, 0));

    // Dessiner sur le Canvas du formulaire
    Bmp.Draw(Canvas, 0, 0, True);  // True = invalider la zone
  finally
    Bmp.Free;
  end;
end;
```

### Utiliser avec TPaintBox

```pascal
procedure TForm1.PaintBox1Paint(Sender: TObject);  
var
  Bmp: TBGRABitmap;
begin
  Bmp := TBGRABitmap.Create(PaintBox1.Width, PaintBox1.Height);
  try
    Bmp.FillTransparent;
    Bmp.DrawLineAntialias(0, 0, Bmp.Width, Bmp.Height, BGRA(255, 0, 0), 3);

    Bmp.Draw(PaintBox1.Canvas, 0, 0, False);
  finally
    Bmp.Free;
  end;
end;
```

---

## Charger et sauvegarder des images

### Formats supportés

BGRABitmap supporte de nombreux formats :
- PNG (avec transparence complète)
- JPEG
- BMP
- GIF
- TIFF
- TGA
- ICO
- Et plus...

### Charger une image

```pascal
var
  Bmp: TBGRABitmap;
begin
  Bmp := TBGRABitmap.Create('photo.png');
  try
    // Utiliser l'image...
  finally
    Bmp.Free;
  end;
end;
```

### Sauvegarder une image

```pascal
// PNG (recommandé pour préserver la transparence)
Bmp.SaveToFile('output.png');

// JPEG (pas de transparence)
Bmp.SaveToFile('output.jpg');

// BMP
Bmp.SaveToFile('output.bmp');
```

### Charger depuis un stream

```pascal
var
  Stream: TMemoryStream;
  Bmp: TBGRABitmap;
begin
  Stream := TMemoryStream.Create;
  Bmp := TBGRABitmap.Create;
  try
    // Charger les données dans le stream...
    Stream.LoadFromFile('image.png');
    Stream.Position := 0;

    Bmp.LoadFromStream(Stream);
  finally
    Bmp.Free;
    Stream.Free;
  end;
end;
```

---

## Exemple complet : Éditeur d'image simple

Voici un exemple complet combinant plusieurs concepts :

```pascal
unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, ExtCtrls, StdCtrls, Dialogs,
  BGRABitmap, BGRABitmapTypes;

type
  TForm1 = class(TForm)
    PaintBox1: TPaintBox;
    Panel1: TPanel;
    LoadButton: TButton;
    SaveButton: TButton;
    BlurButton: TButton;
    GrayscaleButton: TButton;
    RotateButton: TButton;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure LoadButtonClick(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    procedure BlurButtonClick(Sender: TObject);
    procedure GrayscaleButtonClick(Sender: TObject);
    procedure RotateButtonClick(Sender: TObject);
  private
    FImage: TBGRABitmap;
    procedure UpdateDisplay;
  end;

var
  Form1: TForm1;

implementation

uses
  BGRAFilters;

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);  
begin
  FImage := TBGRABitmap.Create(PaintBox1.Width, PaintBox1.Height);
  FImage.Fill(BGRAWhite);
  FImage.TextOut(50, 50, 'Chargez une image...', BGRABlack);
end;

procedure TForm1.FormDestroy(Sender: TObject);  
begin
  FImage.Free;
end;

procedure TForm1.PaintBox1Paint(Sender: TObject);  
begin
  FImage.Draw(PaintBox1.Canvas, 0, 0, False);
end;

procedure TForm1.UpdateDisplay;  
begin
  PaintBox1.Invalidate;
end;

procedure TForm1.LoadButtonClick(Sender: TObject);  
var
  LoadedImage: TBGRABitmap;
begin
  if OpenDialog1.Execute then
  begin
    LoadedImage := TBGRABitmap.Create(OpenDialog1.FileName);
    try
      // Redimensionner pour tenir dans le PaintBox
      FImage.Free;
      FImage := LoadedImage.Resample(
        PaintBox1.Width,
        PaintBox1.Height,
        rmFineResample
      );
    finally
      LoadedImage.Free;
    end;
    UpdateDisplay;
  end;
end;

procedure TForm1.SaveButtonClick(Sender: TObject);  
begin
  if SaveDialog1.Execute then
  begin
    FImage.SaveToFile(SaveDialog1.FileName);
    ShowMessage('Image sauvegardée !');
  end;
end;

procedure TForm1.BlurButtonClick(Sender: TObject);  
begin
  // Appliquer un flou gaussien
  BGRAReplace(FImage, FImage.FilterBlurRadial(10, rbFast));
  UpdateDisplay;
end;

procedure TForm1.GrayscaleButtonClick(Sender: TObject);  
begin
  // Convertir en niveaux de gris
  BGRAReplace(FImage, FImage.FilterGrayscale);
  UpdateDisplay;
end;

procedure TForm1.RotateButtonClick(Sender: TObject);  
var
  Rotated: TBGRABitmap;
begin
  // Rotation de 90 degrés
  Rotated := FImage.RotateCW;
  FImage.Free;
  FImage := Rotated;
  UpdateDisplay;
end;

end.
```

### Configuration du fichier .lfm (interface)

```pascal
object Form1: TForm1
  Left = 300
  Height = 600
  Top = 200
  Width = 800
  Caption = 'Éditeur BGRABitmap'
  OnCreate = FormCreate
  OnDestroy = FormDestroy

  object Panel1: TPanel
    Left = 0
    Height = 50
    Top = 0
    Width = 800
    Align = alTop

    object LoadButton: TButton
      Left = 10
      Top = 10
      Width = 100
      Height = 30
      Caption = 'Charger'
      OnClick = LoadButtonClick
    end

    object SaveButton: TButton
      Left = 120
      Top = 10
      Width = 100
      Height = 30
      Caption = 'Sauvegarder'
      OnClick = SaveButtonClick
    end

    object BlurButton: TButton
      Left = 230
      Top = 10
      Width = 100
      Height = 30
      Caption = 'Flou'
      OnClick = BlurButtonClick
    end

    object GrayscaleButton: TButton
      Left = 340
      Top = 10
      Width = 100
      Height = 30
      Caption = 'Noir & Blanc'
      OnClick = GrayscaleButtonClick
    end

    object RotateButton: TButton
      Left = 450
      Top = 10
      Width = 100
      Height = 30
      Caption = 'Rotation 90°'
      OnClick = RotateButtonClick
    end
  end

  object PaintBox1: TPaintBox
    Left = 0
    Height = 550
    Top = 50
    Width = 800
    Align = alClient
    OnPaint = PaintBox1Paint
  end

  object OpenDialog1: TOpenDialog
    Filter = 'Images|*.png;*.jpg;*.jpeg;*.bmp;*.gif'
  end

  object SaveDialog1: TSaveDialog
    Filter = 'PNG|*.png|JPEG|*.jpg|BMP|*.bmp'
    DefaultExt = '.png'
  end
end
```

---

## Techniques avancées

### 1. Transparence et composition

#### Modes de mélange (Blend Modes)

BGRABitmap offre de nombreux modes de mélange similaires à Photoshop :

```pascal
var
  Background, Foreground: TBGRABitmap;
begin
  Background := TBGRABitmap.Create(400, 300, BGRA(100, 150, 200));
  Foreground := TBGRABitmap.Create(200, 150, BGRA(255, 100, 100, 180));

  try
    // Mode normal (alpha blending standard)
    Background.BlendImage(100, 75, Foreground, boTransparent);

    // Mode multiplicatif (assombrit)
    Background.BlendImage(100, 75, Foreground, boMultiply);

    // Mode additif (éclaircit, pour les lumières)
    Background.BlendImage(100, 75, Foreground, boAdditive);

    // Mode écran (Screen - inverse du multiplicatif)
    Background.BlendImage(100, 75, Foreground, boScreen);

    // Mode overlay (contraste augmenté)
    Background.BlendImage(100, 75, Foreground, boOverlay);

    // Mode différence (effet artistique)
    Background.BlendImage(100, 75, Foreground, boDifference);

    Background.Draw(Canvas, 0, 0, True);
  finally
    Background.Free;
    Foreground.Free;
  end;
end;
```

**Liste complète des modes de mélange** :
- `boTransparent` : Alpha blending normal
- `boLinearBlend` : Mélange linéaire
- `boMultiply` : Multiplie les couleurs (assombrit)
- `boAdditive` : Addition (pour effets de lumière)
- `boScreen` : Écran (éclaircit)
- `boOverlay` : Superposition
- `boSoftLight` : Lumière douce
- `boHardLight` : Lumière crue
- `boDifference` : Différence absolue
- `boNegation` : Négation
- `boReflect` : Réflexion
- `boGlow` : Lueur
- `boXor` : XOR logique

#### Opacité globale

```pascal
// Appliquer une opacité à tout le bitmap
Bmp.ApplyGlobalOpacity(128);  // 50% transparent (0-255)

// Version avec préservation des bords
Bmp.ApplyGlobalOpacity(180);
```

#### Masques alpha

```pascal
var
  Image, Mask: TBGRABitmap;
begin
  Image := TBGRABitmap.Create('photo.png');
  Mask := TBGRABitmap.Create('mask.png');

  try
    // Appliquer le masque (blanc = opaque, noir = transparent)
    Image.ApplyMask(Mask);

    Image.Draw(Canvas, 0, 0, True);
  finally
    Image.Free;
    Mask.Free;
  end;
end;
```

---

### 2. Effets de lumière avancés

#### Éclairage directionnel

```pascal
uses
  BGRALayers;

var
  Layer: TBGRABitmap;
  LightPos: TPointF;
begin
  Layer := TBGRABitmap.Create(400, 300, BGRA(100, 100, 150));

  try
    // Position de la source lumineuse
    LightPos := PointF(200, 50);

    // Créer un dégradé radial pour simuler la lumière
    Layer.GradientFill(
      0, 0, Layer.Width, Layer.Height,
      BGRA(255, 255, 200, 100),  // Lumière chaude
      BGRAPixelTransparent,
      gtRadial,
      LightPos,
      PointF(LightPos.x + 200, LightPos.y),
      dmDrawWithTransparency
    );

    Layer.Draw(Canvas, 0, 0, True);
  finally
    Layer.Free;
  end;
end;
```

#### Effet de lueur colorée

```pascal
procedure AddColorGlow(ABitmap: TBGRABitmap; AGlowColor: TBGRAPixel;
  ARadius: Integer);
var
  Glow: TBGRABitmap;
begin
  // Créer une copie pour la lueur
  Glow := ABitmap.Duplicate;
  try
    // Appliquer la couleur de lueur
    Glow.ReplaceColor(BGRABlack, BGRAPixelTransparent);
    Glow.ReplaceTransparent(AGlowColor);

    // Flouter
    BGRAReplace(Glow, Glow.FilterBlurRadial(ARadius, rbFast));

    // Appliquer en mode additif
    ABitmap.BlendImage(0, 0, Glow, boAdditive);
  finally
    Glow.Free;
  end;
end;

// Utilisation
var
  Bmp: TBGRABitmap;
begin
  Bmp := TBGRABitmap.Create(400, 300);
  Bmp.Fill(BGRA(20, 20, 40));

  // Dessiner un cercle
  Bmp.FillEllipseAntialias(200, 150, 50, 50, BGRA(255, 200, 0));

  // Ajouter une lueur orange
  AddColorGlow(Bmp, BGRA(255, 150, 0, 100), 20);

  Bmp.Draw(Canvas, 0, 0, True);
  Bmp.Free;
end;
```

---

### 3. Sprites et animations

#### Gestion de sprite sheets

```pascal
type
  TSpriteSheet = class
  private
    FSheet: TBGRABitmap;
    FSpriteWidth: Integer;
    FSpriteHeight: Integer;
    FColumns: Integer;
  public
    constructor Create(const AFileName: string; ASpriteWidth, ASpriteHeight: Integer);
    destructor Destroy; override;
    procedure DrawSprite(ACanvas: TCanvas; AIndex, X, Y: Integer);
    function GetSprite(AIndex: Integer): TBGRABitmap;
  end;

constructor TSpriteSheet.Create(const AFileName: string;
  ASpriteWidth, ASpriteHeight: Integer);
begin
  FSheet := TBGRABitmap.Create(AFileName);
  FSpriteWidth := ASpriteWidth;
  FSpriteHeight := ASpriteHeight;
  FColumns := FSheet.Width div FSpriteWidth;
end;

destructor TSpriteSheet.Destroy;  
begin
  FSheet.Free;
  inherited;
end;

function TSpriteSheet.GetSprite(AIndex: Integer): TBGRABitmap;  
var
  Col, Row: Integer;
  SourceRect: TRect;
begin
  Col := AIndex mod FColumns;
  Row := AIndex div FColumns;

  Result := TBGRABitmap.Create(FSpriteWidth, FSpriteHeight);

  SourceRect := Rect(
    Col * FSpriteWidth,
    Row * FSpriteHeight,
    (Col + 1) * FSpriteWidth,
    (Row + 1) * FSpriteHeight
  );

  Result.PutImage(0, 0, FSheet, dmSet);
  Result.ClipRect := SourceRect;
end;

procedure TSpriteSheet.DrawSprite(ACanvas: TCanvas; AIndex, X, Y: Integer);  
var
  Sprite: TBGRABitmap;
begin
  Sprite := GetSprite(AIndex);
  try
    Sprite.Draw(ACanvas, X, Y, False);
  finally
    Sprite.Free;
  end;
end;
```

#### Animation simple

```pascal
type
  TAnimatedSprite = class
  private
    FSpriteSheet: TSpriteSheet;
    FCurrentFrame: Integer;
    FFrameCount: Integer;
    FTimer: TTimer;
    FX, FY: Integer;
    FPaintBox: TPaintBox;
  public
    constructor Create(ASpriteSheet: TSpriteSheet; AFrameCount: Integer;
      APaintBox: TPaintBox; AFrameRate: Integer);
    destructor Destroy; override;
    procedure OnTimer(Sender: TObject);
    procedure Draw(ACanvas: TCanvas);
  end;

constructor TAnimatedSprite.Create(ASpriteSheet: TSpriteSheet;
  AFrameCount: Integer; APaintBox: TPaintBox; AFrameRate: Integer);
begin
  FSpriteSheet := ASpriteSheet;
  FFrameCount := AFrameCount;
  FCurrentFrame := 0;
  FPaintBox := APaintBox;
  FX := 100;
  FY := 100;

  FTimer := TTimer.Create(nil);
  FTimer.Interval := 1000 div AFrameRate;  // FPS vers millisecondes
  FTimer.OnTimer := @OnTimer;
  FTimer.Enabled := True;
end;

destructor TAnimatedSprite.Destroy;  
begin
  FTimer.Free;
  inherited;
end;

procedure TAnimatedSprite.OnTimer(Sender: TObject);  
begin
  FCurrentFrame := (FCurrentFrame + 1) mod FFrameCount;
  FPaintBox.Invalidate;
end;

procedure TAnimatedSprite.Draw(ACanvas: TCanvas);  
begin
  FSpriteSheet.DrawSprite(ACanvas, FCurrentFrame, FX, FY);
end;
```

---

### 4. Particules et effets visuels

#### Système de particules simple

```pascal
type
  TParticle = record
    X, Y: Single;
    VX, VY: Single;  // Vélocité
    Life: Single;
    Color: TBGRAPixel;
    Size: Single;
  end;

  TParticleSystem = class
  private
    FParticles: array of TParticle;
    FCount: Integer;
  public
    constructor Create;
    procedure AddParticle(X, Y: Single; Color: TBGRAPixel);
    procedure Update(DeltaTime: Single);
    procedure Draw(ABitmap: TBGRABitmap);
  end;

constructor TParticleSystem.Create;  
begin
  SetLength(FParticles, 1000);
  FCount := 0;
end;

procedure TParticleSystem.AddParticle(AX, AY: Single; AColor: TBGRAPixel);  
begin
  if FCount >= Length(FParticles) then Exit;

  { Note : ne pas utiliser with ici car les champs X, Y, Color
    masqueraient les paramètres de même nom (bug classique). }
  FParticles[FCount].X := AX;
  FParticles[FCount].Y := AY;
  FParticles[FCount].VX := (Random - 0.5) * 100;  // Vélocité aléatoire
  FParticles[FCount].VY := (Random - 0.5) * 100;
  FParticles[FCount].Life := 1.0;
  FParticles[FCount].Color := AColor;
  FParticles[FCount].Size := 3 + Random * 3;

  Inc(FCount);
end;

procedure TParticleSystem.Update(DeltaTime: Single);  
var
  i: Integer;
begin
  for i := FCount - 1 downto 0 do
  begin
    with FParticles[i] do
    begin
      // Mise à jour position
      X := X + VX * DeltaTime;
      Y := Y + VY * DeltaTime;

      // Gravité
      VY := VY + 98 * DeltaTime;

      // Durée de vie
      Life := Life - DeltaTime;

      // Supprimer si morte
      if Life <= 0 then
      begin
        FParticles[i] := FParticles[FCount - 1];
        Dec(FCount);
      end;
    end;
  end;
end;

procedure TParticleSystem.Draw(ABitmap: TBGRABitmap);  
var
  i: Integer;
  C: TBGRAPixel;
begin
  for i := 0 to FCount - 1 do
  begin
    with FParticles[i] do
    begin
      // Ajuster l'alpha selon la durée de vie
      C := Color;
      C.alpha := Round(Life * 255);

      // Dessiner la particule
      ABitmap.FillEllipseAntialias(X, Y, Size, Size, C);
    end;
  end;
end;
```

---

### 5. Textures et motifs

#### Créer une texture procédurale

```pascal
function CreateNoiseTexture(AWidth, AHeight: Integer): TBGRABitmap;  
var
  x, y: Integer;
  Noise: Byte;
begin
  Result := TBGRABitmap.Create(AWidth, AHeight);

  for y := 0 to AHeight - 1 do
    for x := 0 to AWidth - 1 do
    begin
      Noise := Random(256);
      Result.SetPixel(x, y, BGRA(Noise, Noise, Noise, 255));
    end;
end;

// Utilisation
var
  Texture: TBGRABitmap;
begin
  Texture := CreateNoiseTexture(200, 200);
  try
    Bmp.PutImage(0, 0, Texture, dmSet);
  finally
    Texture.Free;
  end;
end;
```

#### Texture répétée (tiling)

```pascal
procedure TileTexture(ATarget: TBGRABitmap; ATexture: TBGRABitmap);  
var
  x, y: Integer;
begin
  y := 0;
  while y < ATarget.Height do
  begin
    x := 0;
    while x < ATarget.Width do
    begin
      ATarget.PutImage(x, y, ATexture, dmDrawWithTransparency);
      x := x + ATexture.Width;
    end;
    y := y + ATexture.Height;
  end;
end;
```

#### Motifs géométriques

```pascal
function CreateCheckerboard(AWidth, AHeight, ASquareSize: Integer;
  AColor1, AColor2: TBGRAPixel): TBGRABitmap;
var
  x, y, row, col: Integer;
begin
  Result := TBGRABitmap.Create(AWidth, AHeight);

  for y := 0 to AHeight - 1 do
    for x := 0 to AWidth - 1 do
    begin
      row := y div ASquareSize;
      col := x div ASquareSize;

      if (row + col) mod 2 = 0 then
        Result.SetPixel(x, y, AColor1)
      else
        Result.SetPixel(x, y, AColor2);
    end;
end;

// Utilisation
var
  Pattern: TBGRABitmap;
begin
  Pattern := CreateCheckerboard(400, 300, 20, BGRAWhite, BGRA(200, 200, 200));
  try
    Bmp.PutImage(0, 0, Pattern, dmSet);
  finally
    Pattern.Free;
  end;
end;
```

---

### 6. Filtres personnalisés

#### Matrice de convolution

```pascal
procedure ApplyConvolutionFilter(ABitmap: TBGRABitmap;
  const AKernel: array of array of Integer; ADivisor: Integer);
var
  x, y, kx, ky: Integer;
  r, g, b: Integer;
  Pixel: TBGRAPixel;
  Original: TBGRABitmap;
  KernelSize: Integer;
begin
  Original := ABitmap.Duplicate;
  KernelSize := Length(AKernel);

  try
    for y := KernelSize div 2 to ABitmap.Height - KernelSize div 2 - 1 do
      for x := KernelSize div 2 to ABitmap.Width - KernelSize div 2 - 1 do
      begin
        r := 0; g := 0; b := 0;

        for ky := 0 to KernelSize - 1 do
          for kx := 0 to KernelSize - 1 do
          begin
            Pixel := Original.GetPixel(
              x + kx - KernelSize div 2,
              y + ky - KernelSize div 2
            );

            r := r + Pixel.red * AKernel[ky][kx];
            g := g + Pixel.green * AKernel[ky][kx];
            b := b + Pixel.blue * AKernel[ky][kx];
          end;

        ABitmap.SetPixel(x, y, BGRA(
          EnsureRange(r div ADivisor, 0, 255),
          EnsureRange(g div ADivisor, 0, 255),
          EnsureRange(b div ADivisor, 0, 255),
          255
        ));
      end;
  finally
    Original.Free;
  end;
end;

// Exemples de kernels

// Flou
const
  BlurKernel: array[0..2] of array[0..2] of Integer = (
    (1, 1, 1),
    (1, 1, 1),
    (1, 1, 1)
  );

// Accentuation
const
  SharpenKernel: array[0..2] of array[0..2] of Integer = (
    ( 0, -1,  0),
    (-1,  5, -1),
    ( 0, -1,  0)
  );

// Détection de contours
const
  EdgeKernel: array[0..2] of array[0..2] of Integer = (
    (-1, -1, -1),
    (-1,  8, -1),
    (-1, -1, -1)
  );

// Utilisation
ApplyConvolutionFilter(Bmp, SharpenKernel, 1);
```

---

### 7. Performance et optimisation

#### Accès direct aux pixels

Pour des opérations massives, l'accès direct est plus rapide :

```pascal
procedure InvertColors(ABitmap: TBGRABitmap);  
var
  p: PBGRAPixel;
  n: Integer;
begin
  p := ABitmap.Data;  // Accès direct au buffer
  n := ABitmap.NbPixels;

  while n > 0 do
  begin
    p^.red := 255 - p^.red;
    p^.green := 255 - p^.green;
    p^.blue := 255 - p^.blue;
    // Alpha reste inchangé

    Inc(p);
    Dec(n);
  end;

  ABitmap.InvalidateBitmap;  // Marquer comme modifié
end;
```

#### Scan lines pour traitement par ligne

```pascal
procedure AdjustBrightness(ABitmap: TBGRABitmap; ADelta: Integer);  
var
  y, x: Integer;
  p: PBGRAPixel;
begin
  for y := 0 to ABitmap.Height - 1 do
  begin
    p := ABitmap.ScanLine[y];  // Accès à la ligne

    for x := 0 to ABitmap.Width - 1 do
    begin
      p^.red := EnsureRange(p^.red + ADelta, 0, 255);
      p^.green := EnsureRange(p^.green + ADelta, 0, 255);
      p^.blue := EnsureRange(p^.blue + ADelta, 0, 255);
      Inc(p);
    end;
  end;

  ABitmap.InvalidateBitmap;
end;
```

#### Benchmark et mesures

```pascal
uses
  DateUtils;

procedure BenchmarkOperation;  
var
  Bmp: TBGRABitmap;
  StartTime, EndTime: TDateTime;
  Duration: Int64;
begin
  Bmp := TBGRABitmap.Create(1920, 1080);
  try
    Bmp.Fill(BGRAWhite);

    StartTime := Now;

    // Opération à mesurer
    BGRAReplace(Bmp, Bmp.FilterBlurRadial(15, rbFast));

    EndTime := Now;
    Duration := MilliSecondsBetween(EndTime, StartTime);

    ShowMessage(Format('Durée: %d ms', [Duration]));
  finally
    Bmp.Free;
  end;
end;
```

---

## Compatibilité multi-plateforme

### Différences Windows vs Linux

BGRABitmap est conçu pour être identique sur toutes les plateformes, mais quelques points à noter :

| Aspect | Windows | Ubuntu/Linux | Remarques |
|--------|---------|--------------|-----------|
| **Rendu** | Identique | Identique | C'est l'avantage principal |
| **Performance** | Bonne | Bonne | Peut varier selon le CPU |
| **Polices** | TrueType | FreeType | Rendu légèrement différent |
| **Fichiers** | Tous formats | Tous formats | Dépendances identiques |
| **Threads** | Support complet | Support complet | API identique |

### Code 100% portable

```pascal
{$mode objfpc}{$H+}

uses
  BGRABitmap, BGRABitmapTypes;

var
  Bmp: TBGRABitmap;
begin
  // Ce code fonctionne identiquement sur Windows et Linux
  Bmp := TBGRABitmap.Create(800, 600);
  try
    Bmp.Fill(BGRA(200, 220, 255));
    Bmp.FillEllipseAntialias(400, 300, 150, 100, BGRA(255, 200, 0));
    Bmp.DrawLineAntialias(0, 0, 800, 600, BGRA(255, 0, 0), 3);

    {$IFDEF WINDOWS}
    Bmp.SaveToFile('C:\output.png');
    {$ENDIF}

    {$IFDEF LINUX}
    Bmp.SaveToFile('/home/user/output.png');
    {$ENDIF}
  finally
    Bmp.Free;
  end;
end;
```

---

## Ressources et documentation

### Documentation officielle

- **Site officiel** : http://bgrabitmap.github.io/
- **GitHub** : https://github.com/bgrabitmap/bgrabitmap
- **Wiki** : https://wiki.freepascal.org/BGRABitmap
- **Forum** : https://forum.lazarus.freepascal.org/ (section Graphics)

### Exemples inclus

BGRABitmap est livré avec de nombreux exemples dans le dossier `test` :
- Dessins de base
- Effets et filtres
- Animations
- Jeux simples
- Éditeurs graphiques

### Packages connexes

- **BGRAControls** : Composants visuels stylisés utilisant BGRABitmap
- **BGRABitmap-Games** : Exemples de jeux
- **MaterialControls** : Interface Material Design

---

## Bonnes pratiques

### ✅ À faire

1. **Toujours libérer** les TBGRABitmap avec `Free`
2. **Utiliser BGRAReplace** pour remplacer un bitmap filtré
3. **Réutiliser les bitmaps** plutôt que de les recréer constamment
4. **Activer l'anti-aliasing** pour un rendu professionnel
5. **Tester sur les deux plateformes** (Windows et Linux)
6. **Utiliser les modes de dessin appropriés** pour les performances
7. **Privilégier les coordonnées flottantes** (TPointF) pour la précision

### ❌ À éviter

1. ❌ Oublier de libérer les bitmaps (fuites mémoire)
2. ❌ Créer des bitmaps géants sans raison (ralentissements)
3. ❌ Utiliser GetPixel/SetPixel dans des boucles (très lent)
4. ❌ Ignorer la transparence dans les calculs
5. ❌ Filtrer plusieurs fois le même bitmap sans raison
6. ❌ Supposer que les chemins de fichiers sont identiques sur tous les OS

---

## Comparaison avec d'autres bibliothèques

| Bibliothèque | Avantages | Inconvénients |
|--------------|-----------|---------------|
| **BGRABitmap** | Anti-aliasing, portable, effets | Rendu logiciel (CPU) |
| **LazOpenGLContext** | Accélération GPU | Plus complexe |
| **fpImage** | Intégré FPC | Fonctionnalités limitées |
| **AggPas** | Haute qualité | Complexe, moins maintenu |
| **Cairo** | Standard Linux | Dépendance externe |

**Recommandation** : Pour la plupart des applications 2D avec besoin de qualité visuelle et de portabilité, BGRABitmap est le meilleur choix.

---

## Conclusion

**BGRABitmap** est une bibliothèque exceptionnelle pour FreePascal/Lazarus qui permet de créer des graphiques de qualité professionnelle tout en restant portable entre Windows et Ubuntu/Linux.

### Points clés à retenir

- ✅ Anti-aliasing complet sur tous les dessins
- ✅ Transparence alpha totale (canal 8 bits)
- ✅ Dégradés avancés (linéaires, radiaux, multi-couleurs)
- ✅ Nombreux effets visuels (flou, ombre, lueur, etc.)
- ✅ Transformations (rotation, redimensionnement, miroir)
- ✅ Modes de mélange avancés (comme Photoshop)
- ✅ API Canvas2D compatible HTML5
- ✅ Rendu identique sur toutes les plateformes
- ✅ Performance correcte pour la plupart des usages
- ✅ Documentation et exemples riches

### Quand utiliser BGRABitmap

- Applications graphiques et éditeurs
- Jeux 2D avec graphismes de qualité
- Visualisation de données
- Interfaces modernes avec effets
- Génération d'images (rapports, exports)
- Prototypage rapide avec rendu professionnel

### Prochaines étapes

Maintenant que vous maîtrisez BGRABitmap, vous pouvez explorer :
- **12.3 OpenGL** pour l'accélération 3D/GPU
- **23. Développement de jeux** pour créer des jeux complets
- **BGRAControls** pour des composants visuels stylisés

BGRABitmap est un outil puissant qui ouvre de nombreuses possibilités créatives tout en restant accessible et portable !

# 12.2 BGRABitmap - Projets pratiques et cas d'usage avancés

## Projet 1 : Créateur de miniatures d'images

Un outil pour générer automatiquement des miniatures avec bordures et effets.

### Code complet

```pascal
unit ThumbnailGenerator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, ExtCtrls, StdCtrls, Dialogs,
  BGRABitmap, BGRABitmapTypes, BGRATransform;

type
  TThumbStyle = (tsSimple, tsBordered, tsShadow, tsRounded);

  TThumbnailMaker = class
  private
    FWidth: Integer;
    FHeight: Integer;
    FQuality: Integer;
    FStyle: TThumbStyle;
  public
    constructor Create;
    function CreateThumbnail(const ASourceFile: string): TBGRABitmap;
    function CreateThumbnailWithStyle(ASource: TBGRABitmap): TBGRABitmap;
    procedure SaveThumbnail(const ASourceFile, ADestFile: string);
    property Width: Integer read FWidth write FWidth;
    property Height: Integer read FHeight write FHeight;
    property Style: TThumbStyle read FStyle write FStyle;
  end;

implementation

constructor TThumbnailMaker.Create;  
begin
  FWidth := 200;
  FHeight := 150;
  FQuality := 80;
  FStyle := tsBordered;
end;

function TThumbnailMaker.CreateThumbnail(const ASourceFile: string): TBGRABitmap;  
var
  Original, Resized: TBGRABitmap;
  Ratio: Double;
  NewWidth, NewHeight: Integer;
begin
  Original := TBGRABitmap.Create(ASourceFile);
  try
    // Calculer les dimensions en préservant le ratio
    Ratio := Original.Width / Original.Height;

    if Ratio > (FWidth / FHeight) then
    begin
      NewWidth := FWidth;
      NewHeight := Round(FWidth / Ratio);
    end
    else
    begin
      NewHeight := FHeight;
      NewWidth := Round(FHeight * Ratio);
    end;

    // Redimensionner avec haute qualité
    Resized := Original.Resample(NewWidth, NewHeight, rmFineResample);

    try
      Result := CreateThumbnailWithStyle(Resized);
    finally
      Resized.Free;
    end;
  finally
    Original.Free;
  end;
end;

function TThumbnailMaker.CreateThumbnailWithStyle(ASource: TBGRABitmap): TBGRABitmap;  
var
  Shadow: TBGRABitmap;
  Mask, Rounded: TBGRABitmap;
  X, Y: Integer;
begin
  Result := TBGRABitmap.Create(FWidth, FHeight);
  Result.Fill(BGRAWhite);

  // Centrer l'image
  X := (FWidth - ASource.Width) div 2;
  Y := (FHeight - ASource.Height) div 2;

  case FStyle of
    tsSimple:
      Result.PutImage(X, Y, ASource, dmDrawWithTransparency);

    tsBordered:
      begin
        // Dessiner une bordure
        Result.Rectangle(X - 2, Y - 2, X + ASource.Width + 2,
          Y + ASource.Height + 2, BGRABlack, dmSet);
        Result.PutImage(X, Y, ASource, dmDrawWithTransparency);
      end;

    tsShadow:
      begin
        // Créer une ombre portée
        Shadow := ASource.Duplicate;
        try
          Shadow.ApplyGlobalOpacity(80);
          BGRAReplace(Shadow, Shadow.FilterBlurRadial(5, rbFast));

          Result.PutImage(X + 5, Y + 5, Shadow, dmDrawWithTransparency);
          Result.PutImage(X, Y, ASource, dmDrawWithTransparency);
        finally
          Shadow.Free;
        end;
      end;

    tsRounded:
      begin
        // Coins arrondis avec masque
        Rounded := ASource.Duplicate;
          Mask := TBGRABitmap.Create(ASource.Width, ASource.Height);
          try
            Mask.Fill(BGRAPixelTransparent);
            Mask.RoundRectAntialias(0, 0, Mask.Width, Mask.Height,
              15, 15, BGRAWhite, 1, BGRAWhite);

            Rounded.ApplyMask(Mask);
            Result.PutImage(X, Y, Rounded, dmDrawWithTransparency);
          finally
            Mask.Free;
            Rounded.Free;
          end;
      end;
  end;
end;

procedure TThumbnailMaker.SaveThumbnail(const ASourceFile, ADestFile: string);  
var
  Thumb: TBGRABitmap;
begin
  Thumb := CreateThumbnail(ASourceFile);
  try
    Thumb.SaveToFile(ADestFile);
  finally
    Thumb.Free;
  end;
end;

end.
```

### Utilisation

```pascal
var
  ThumbMaker: TThumbnailMaker;
begin
  ThumbMaker := TThumbnailMaker.Create;
  try
    ThumbMaker.Width := 300;
    ThumbMaker.Height := 200;
    ThumbMaker.Style := tsShadow;

    ThumbMaker.SaveThumbnail('photo_grande.jpg', 'photo_mini.png');
  finally
    ThumbMaker.Free;
  end;
end;
```

---

## Projet 2 : Générateur de graphiques

Création de graphiques en barres et camemberts avec BGRABitmap.

### Graphique en barres

```pascal
unit ChartGenerator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRABitmap, BGRABitmapTypes, Math;

type
  TChartData = record
    Caption: string;  // Note : Label est un mot réservé en Pascal
    Value: Double;
    Color: TBGRAPixel;
  end;

  TChartDataArray = array of TChartData;

  TBarChart = class
  private
    FWidth: Integer;
    FHeight: Integer;
    FTitle: string;
    FMargin: Integer;
    FBarSpacing: Integer;
  public
    constructor Create(AWidth, AHeight: Integer);
    function Generate(const AData: TChartDataArray): TBGRABitmap;
    property Title: string read FTitle write FTitle;
    property Margin: Integer read FMargin write FMargin;
  end;

implementation

constructor TBarChart.Create(AWidth, AHeight: Integer);  
begin
  FWidth := AWidth;
  FHeight := AHeight;
  FMargin := 50;
  FBarSpacing := 10;
  FTitle := 'Graphique';
end;

function TBarChart.Generate(const AData: TChartDataArray): TBGRABitmap;  
var
  i: Integer;
  MaxValue: Double;
  BarWidth: Integer;
  ChartHeight: Integer;
  X, Y, BarHeight: Integer;
  TextSize: TSize;
begin
  Result := TBGRABitmap.Create(FWidth, FHeight);

  // Fond blanc
  Result.Fill(BGRAWhite);

  // Titre
  Result.FontName := 'Arial';
  Result.FontHeight := 20;
  Result.FontStyle := [fsBold];
  Result.FontQuality := fqFineAntialiasing;

  TextSize := Result.TextSize(FTitle);
  Result.TextOut((FWidth - TextSize.cx) div 2, 10, FTitle, BGRABlack);

  // Trouver la valeur maximale
  MaxValue := 0;
  for i := 0 to High(AData) do
    if AData[i].Value > MaxValue then
      MaxValue := AData[i].Value;

  // Calculer les dimensions
  ChartHeight := FHeight - FMargin * 2;
  BarWidth := (FWidth - FMargin * 2 - FBarSpacing * Length(AData)) div Length(AData);

  // Dessiner les barres
  Result.FontHeight := 12;
  Result.FontStyle := [];

  for i := 0 to High(AData) do
  begin
    X := FMargin + i * (BarWidth + FBarSpacing);
    BarHeight := Round((AData[i].Value / MaxValue) * ChartHeight);
    Y := FHeight - FMargin - BarHeight;

    // Barre avec dégradé
    Result.GradientFill(
      X, Y, X + BarWidth, FHeight - FMargin,
      AData[i].Color,
      BGRA(AData[i].Color.red div 2, AData[i].Color.green div 2,
           AData[i].Color.blue div 2),
      gtLinear,
      PointF(X, Y),
      PointF(X, FHeight - FMargin),
      dmSet
    );

    // Contour de la barre
    Result.RectangleAntialias(X, Y, X + BarWidth, FHeight - FMargin,
      BGRA(100, 100, 100), 1);

    // Valeur au-dessus de la barre
    Result.TextOut(X + 5, Y - 20, FloatToStrF(AData[i].Value, ffFixed, 10, 1),
      BGRABlack);

    // Label
    TextSize := Result.TextSize(AData[i].Caption);
    Result.TextOut(X + (BarWidth - TextSize.cx) div 2,
      FHeight - FMargin + 5, AData[i].Caption, BGRABlack);
  end;

  // Axes
  Result.DrawLineAntialias(FMargin, FHeight - FMargin,
    FWidth - FMargin, FHeight - FMargin, BGRABlack, 2);
  Result.DrawLineAntialias(FMargin, FMargin,
    FMargin, FHeight - FMargin, BGRABlack, 2);
end;

end.
```

### Graphique camembert (Pie Chart)

```pascal
type
  TPieChart = class
  private
    FWidth: Integer;
    FHeight: Integer;
    FTitle: string;
  public
    constructor Create(AWidth, AHeight: Integer);
    function Generate(const AData: TChartDataArray): TBGRABitmap;
    property Title: string read FTitle write FTitle;
  end;

function TPieChart.Generate(const AData: TChartDataArray): TBGRABitmap;  
var
  i, j: Integer;
  Total, CurrentAngle, SliceAngle: Double;
  CenterX, CenterY, Radius: Integer;
  StartAngle, EndAngle: Double;
  LegendX, LegendY: Integer;
  TextSize: TSize;
  Points: array of TPointF;
  Angle: Double;
  Steps: Integer;
  LabelAngle: Double;
  LabelX, LabelY: Integer;
  Percentage: string;
begin
  Result := TBGRABitmap.Create(FWidth, FHeight);
  Result.Fill(BGRAWhite);

  // Titre
  Result.FontName := 'Arial';
  Result.FontHeight := 20;
  Result.FontStyle := [fsBold];
  Result.FontQuality := fqFineAntialiasing;

  TextSize := Result.TextSize(FTitle);
  Result.TextOut((FWidth - TextSize.cx) div 2, 10, FTitle, BGRABlack);

  // Calculer le total
  Total := 0;
  for i := 0 to High(AData) do
    Total := Total + AData[i].Value;

  // Paramètres du cercle
  CenterX := FWidth div 3;
  CenterY := FHeight div 2;
  Radius := Min(CenterX, CenterY) - 50;

  // Dessiner les tranches
  CurrentAngle := -Pi / 2;  // Commencer en haut
  Result.FontHeight := 12;
  Result.FontStyle := [];

  for i := 0 to High(AData) do
  begin
    SliceAngle := (AData[i].Value / Total) * 2 * Pi;
    EndAngle := CurrentAngle + SliceAngle;

    // Dessiner la tranche avec dégradé
    Steps := Max(3, Round(Abs(SliceAngle) * 20));
      SetLength(Points, Steps + 2);

      Points[0] := PointF(CenterX, CenterY);

      for j := 0 to Steps do
      begin
        Angle := CurrentAngle + (SliceAngle * j / Steps);
        Points[j + 1] := PointF(
          CenterX + Cos(Angle) * Radius,
          CenterY + Sin(Angle) * Radius
        );
      end;

      Result.FillPolyAntialias(Points, AData[i].Color);
      Result.DrawPolygonAntialias(Points, BGRA(100, 100, 100), 2);

    // Pourcentage dans la tranche
    LabelAngle := CurrentAngle + SliceAngle / 2;
      LabelX := Round(CenterX + Cos(LabelAngle) * Radius * 0.7);
      LabelY := Round(CenterY + Sin(LabelAngle) * Radius * 0.7);

      Percentage := Format('%.1f%%', [(AData[i].Value / Total) * 100]);
      TextSize := Result.TextSize(Percentage);
      Result.TextOut(LabelX - TextSize.cx div 2, LabelY - TextSize.cy div 2,
        Percentage, BGRAWhite);

    CurrentAngle := EndAngle;
  end;

  // Légende
  LegendX := FWidth * 2 div 3;
  LegendY := 60;

  for i := 0 to High(AData) do
  begin
    // Carré de couleur
    Result.FillRectAntialias(LegendX, LegendY, LegendX + 20, LegendY + 20,
      AData[i].Color);
    Result.RectangleAntialias(LegendX, LegendY, LegendX + 20, LegendY + 20,
      BGRA(100, 100, 100), 1);

    // Label et valeur
    Result.TextOut(LegendX + 30, LegendY + 3,
      Format('%s: %.1f', [AData[i].Caption, AData[i].Value]), BGRABlack);

    LegendY := LegendY + 30;
  end;
end;
```

### Utilisation des graphiques

```pascal
var
  Data: TChartDataArray;
  BarChart: TBarChart;
  PieChart: TPieChart;
  Chart: TBGRABitmap;
begin
  // Préparer les données
  SetLength(Data, 5);
  Data[0].Caption := 'Jan'; Data[0].Value := 120; Data[0].Color := BGRA(255, 100, 100);
  Data[1].Caption := 'Fév'; Data[1].Value := 180; Data[1].Color := BGRA(100, 255, 100);
  Data[2].Caption := 'Mar'; Data[2].Value := 150; Data[2].Color := BGRA(100, 100, 255);
  Data[3].Caption := 'Avr'; Data[3].Value := 220; Data[3].Color := BGRA(255, 255, 100);
  Data[4].Caption := 'Mai'; Data[4].Value := 190; Data[4].Color := BGRA(255, 100, 255);

  // Graphique en barres
  BarChart := TBarChart.Create(800, 600);
  try
    BarChart.Title := 'Ventes mensuelles';
    Chart := BarChart.Generate(Data);
    try
      Chart.SaveToFile('barchart.png');
    finally
      Chart.Free;
    end;
  finally
    BarChart.Free;
  end;

  // Graphique camembert
  PieChart := TPieChart.Create(800, 600);
  try
    PieChart.Title := 'Répartition des ventes';
    Chart := PieChart.Generate(Data);
    try
      Chart.SaveToFile('piechart.png');
    finally
      Chart.Free;
    end;
  finally
    PieChart.Free;
  end;
end;
```

---

## Projet 3 : Filigrane (Watermark) automatique

Outil pour ajouter un filigrane sur des images en batch.

```pascal
unit WatermarkTool;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRABitmap, BGRABitmapTypes, BGRATextFX;

type
  TWatermarkPosition = (wpTopLeft, wpTopRight, wpBottomLeft, wpBottomRight, wpCenter);

  TWatermarkMaker = class
  private
    FText: string;
    FPosition: TWatermarkPosition;
    FOpacity: Byte;
    FFontSize: Integer;
    FMargin: Integer;
  public
    constructor Create;
    procedure ApplyTextWatermark(ABitmap: TBGRABitmap);
    procedure ApplyImageWatermark(ABitmap: TBGRABitmap; const AWatermarkFile: string);
    procedure ProcessFolder(const ASourceFolder, ADestFolder, AWatermarkText: string);
    property Text: string read FText write FText;
    property Position: TWatermarkPosition read FPosition write FPosition;
    property Opacity: Byte read FOpacity write FOpacity;
    property FontSize: Integer read FFontSize write FFontSize;
  end;

implementation

uses
  FileUtil;

constructor TWatermarkMaker.Create;  
begin
  FText := '© Copyright';
  FPosition := wpBottomRight;
  FOpacity := 128;
  FFontSize := 24;
  FMargin := 20;
end;

procedure TWatermarkMaker.ApplyTextWatermark(ABitmap: TBGRABitmap);  
var
  X, Y: Integer;
  TextSize: TSize;
  WatermarkLayer: TBGRABitmap;
begin
  // Créer le calque de filigrane
  WatermarkLayer := TBGRABitmap.Create(ABitmap.Width, ABitmap.Height);
  try
    WatermarkLayer.FillTransparent;

    // Configurer la police
    WatermarkLayer.FontName := 'Arial';
    WatermarkLayer.FontHeight := FFontSize;
    WatermarkLayer.FontStyle := [fsBold];
    WatermarkLayer.FontQuality := fqFineAntialiasing;

    // Mesurer le texte
    TextSize := WatermarkLayer.TextSize(FText);

    // Calculer la position
    case FPosition of
      wpTopLeft:
        begin
          X := FMargin;
          Y := FMargin;
        end;
      wpTopRight:
        begin
          X := ABitmap.Width - TextSize.cx - FMargin;
          Y := FMargin;
        end;
      wpBottomLeft:
        begin
          X := FMargin;
          Y := ABitmap.Height - TextSize.cy - FMargin;
        end;
      wpBottomRight:
        begin
          X := ABitmap.Width - TextSize.cx - FMargin;
          Y := ABitmap.Height - TextSize.cy - FMargin;
        end;
      wpCenter:
        begin
          X := (ABitmap.Width - TextSize.cx) div 2;
          Y := (ABitmap.Height - TextSize.cy) div 2;
        end;
    end;

    // Dessiner le texte avec ombre
    WatermarkLayer.TextOut(X + 2, Y + 2, FText, BGRA(0, 0, 0, FOpacity div 2));
    WatermarkLayer.TextOut(X, Y, FText, BGRA(255, 255, 255, FOpacity));

    // Appliquer le filigrane
    ABitmap.BlendImage(0, 0, WatermarkLayer, boTransparent);
  finally
    WatermarkLayer.Free;
  end;
end;

procedure TWatermarkMaker.ApplyImageWatermark(ABitmap: TBGRABitmap;
  const AWatermarkFile: string);
var
  Watermark: TBGRABitmap;
  X, Y: Integer;
  ScaledWatermark: TBGRABitmap;
  MaxSize: Integer;
  Ratio: Double;
  NewWidth, NewHeight: Integer;
begin
  Watermark := TBGRABitmap.Create(AWatermarkFile);
  try
    // Redimensionner le filigrane si nécessaire (max 20% de l'image)
    MaxSize := Min(ABitmap.Width, ABitmap.Height) div 5;

    if (Watermark.Width > MaxSize) or (Watermark.Height > MaxSize) then
    begin
      Ratio := Watermark.Width / Watermark.Height;

      if Watermark.Width > Watermark.Height then
      begin
        NewWidth := MaxSize;
        NewHeight := Round(MaxSize / Ratio);
      end
      else
      begin
        NewHeight := MaxSize;
        NewWidth := Round(MaxSize * Ratio);
      end;

      ScaledWatermark := Watermark.Resample(NewWidth, NewHeight, rmFineResample);
    end
    else
      ScaledWatermark := Watermark.Duplicate;

    try
      // Appliquer l'opacité
      ScaledWatermark.ApplyGlobalOpacity(FOpacity);

      // Calculer la position
      case FPosition of
        wpTopLeft:
          begin
            X := FMargin;
            Y := FMargin;
          end;
        wpTopRight:
          begin
            X := ABitmap.Width - ScaledWatermark.Width - FMargin;
            Y := FMargin;
          end;
        wpBottomLeft:
          begin
            X := FMargin;
            Y := ABitmap.Height - ScaledWatermark.Height - FMargin;
          end;
        wpBottomRight:
          begin
            X := ABitmap.Width - ScaledWatermark.Width - FMargin;
            Y := ABitmap.Height - ScaledWatermark.Height - FMargin;
          end;
        wpCenter:
          begin
            X := (ABitmap.Width - ScaledWatermark.Width) div 2;
            Y := (ABitmap.Height - ScaledWatermark.Height) div 2;
          end;
      end;

      // Appliquer le filigrane
      ABitmap.PutImage(X, Y, ScaledWatermark, dmDrawWithTransparency);
    finally
      if ScaledWatermark <> Watermark then
        ScaledWatermark.Free;
    end;
  finally
    Watermark.Free;
  end;
end;

procedure TWatermarkMaker.ProcessFolder(const ASourceFolder, ADestFolder,
  AWatermarkText: string);
var
  Files: TStringList;
  i: Integer;
  SourceFile, DestFile: string;
  Image: TBGRABitmap;
begin
  Files := TStringList.Create;
  try
    // Trouver toutes les images
    FindAllFiles(Files, ASourceFolder, '*.jpg;*.jpeg;*.png;*.bmp', False);

    FText := AWatermarkText;

    for i := 0 to Files.Count - 1 do
    begin
      SourceFile := Files[i];
      DestFile := IncludeTrailingPathDelimiter(ADestFolder) +
                  ExtractFileName(SourceFile);

      try
        Image := TBGRABitmap.Create(SourceFile);
        try
          ApplyTextWatermark(Image);
          Image.SaveToFile(DestFile);
        finally
          Image.Free;
        end;
      except
        on E: Exception do
          WriteLn('Erreur sur ', SourceFile, ': ', E.Message);
      end;
    end;
  finally
    Files.Free;
  end;
end;

end.
```

### Utilisation

```pascal
var
  Watermarker: TWatermarkMaker;
  Image: TBGRABitmap;
begin
  Watermarker := TWatermarkMaker.Create;
  try
    Watermarker.Text := '© Mon Site 2025';
    Watermarker.Position := wpBottomRight;
    Watermarker.Opacity := 150;
    Watermarker.FontSize := 28;

    // Une seule image
    Image := TBGRABitmap.Create('photo.jpg');
    try
      Watermarker.ApplyTextWatermark(Image);
      Image.SaveToFile('photo_watermarked.jpg');
    finally
      Image.Free;
    end;

    // Traitement en batch
    Watermarker.ProcessFolder('C:\Photos', 'C:\Photos_Watermarked',
      '© Mon Site 2025');
  finally
    Watermarker.Free;
  end;
end;
```

---

## Projet 4 : Générateur de boutons stylisés

Création automatique de boutons avec effets modernes.

```pascal
unit ButtonGenerator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, BGRABitmap, BGRABitmapTypes, BGRAGradients;

type
  TButtonStyle = (bsFlat, bsGradient, bsGlass, bs3D, bsRounded);
  TButtonState = (bsNormal, bsHover, bsPressed, bsDisabled);

  TStylizedButton = class
  private
    FWidth: Integer;
    FHeight: Integer;
    FCaption: string;
    FStyle: TButtonStyle;
    FBaseColor: TBGRAPixel;
  public
    constructor Create(AWidth, AHeight: Integer);
    function Generate(AState: TButtonState): TBGRABitmap;
    property Caption: string read FCaption write FCaption;
    property Style: TButtonStyle read FStyle write FStyle;
    property BaseColor: TBGRAPixel read FBaseColor write FBaseColor;
  end;

implementation

constructor TStylizedButton.Create(AWidth, AHeight: Integer);  
begin
  FWidth := AWidth;
  FHeight := AHeight;
  FCaption := 'Button';
  FStyle := bsGradient;
  FBaseColor := BGRA(70, 130, 200);
end;

function TStylizedButton.Generate(AState: TButtonState): TBGRABitmap;  
var
  Color1, Color2: TBGRAPixel;
  TextSize: TSize;
  TextX, TextY: Integer;
  Mask: TBGRABitmap;
begin
  Result := TBGRABitmap.Create(FWidth, FHeight);
  Result.FillTransparent;

  // Ajuster les couleurs selon l'état
  case AState of
    bsNormal:
      begin
        Color1 := FBaseColor;
        Color2 := BGRA(FBaseColor.red div 2, FBaseColor.green div 2,
                       FBaseColor.blue div 2);
      end;
    bsHover:
      begin
        Color1 := BGRA(Min(255, FBaseColor.red + 30),
                      Min(255, FBaseColor.green + 30),
                      Min(255, FBaseColor.blue + 30));
        Color2 := FBaseColor;
      end;
    bsPressed:
      begin
        Color1 := BGRA(FBaseColor.red div 2, FBaseColor.green div 2,
                       FBaseColor.blue div 2);
        Color2 := FBaseColor;
      end;
    bsDisabled:
      begin
        Color1 := BGRA(150, 150, 150);
        Color2 := BGRA(100, 100, 100);
      end;
  end;

  // Dessiner selon le style
  case FStyle of
    bsFlat:
      Result.Fill(Color1);

    bsGradient:
      Result.GradientFill(0, 0, FWidth, FHeight, Color1, Color2, gtLinear,
        PointF(0, 0), PointF(0, FHeight), dmSet);

    bsGlass:
      begin
        // Dégradé de base
        Result.GradientFill(0, 0, FWidth, FHeight, Color1, Color2, gtLinear,
          PointF(0, 0), PointF(0, FHeight), dmSet);

        // Effet de brillance (haut du bouton)
        Result.GradientFill(0, 0, FWidth, FHeight div 2,
          BGRA(255, 255, 255, 80), BGRAPixelTransparent, gtLinear,
          PointF(0, 0), PointF(0, FHeight div 2), dmDrawWithTransparency);
      end;

    bs3D:
      begin
        // Fond
        Result.RoundRectAntialias(2, 2, FWidth - 2, FHeight - 2,
          5, 5, Color1, 1, Color1);

        // Bordure 3D
        if AState <> bsPressed then
        begin
          // Haut et gauche clairs
          Result.DrawLineAntialias(5, 2, FWidth - 5, 2,
            BGRA(255, 255, 255, 100), 1);
          Result.DrawLineAntialias(2, 5, 2, FHeight - 5,
            BGRA(255, 255, 255, 100), 1);

          // Bas et droite sombres
          Result.DrawLineAntialias(5, FHeight - 2, FWidth - 5, FHeight - 2,
            BGRA(0, 0, 0, 100), 1);
          Result.DrawLineAntialias(FWidth - 2, 5, FWidth - 2, FHeight - 5,
            BGRA(0, 0, 0, 100), 1);
        end;
      end;

    bsRounded:
      begin
        Result.RoundRectAntialias(0, 0, FWidth, FHeight, FHeight div 2,
          FHeight div 2, Color1, 1, Color1);

        // Dégradé
        Mask := TBGRABitmap.Create(FWidth, FHeight);
          try
            Mask.RoundRectAntialias(0, 0, FWidth, FHeight, FHeight div 2,
              FHeight div 2, BGRAWhite, 1, BGRAWhite);

            Result.GradientFill(0, 0, FWidth, FHeight, Color1, Color2, gtLinear,
              PointF(0, 0), PointF(0, FHeight), dmSet);
            Result.ApplyMask(Mask);
          finally
            Mask.Free;
          end;
      end;
  end;

  // Contour
  if FStyle <> bs3D then
    Result.RectangleAntialias(0, 0, FWidth, FHeight,
      BGRA(0, 0, 0, 150), 1.5);

  // Texte
  Result.FontName := 'Arial';
  Result.FontHeight := FHeight div 3;
  Result.FontStyle := [fsBold];
  Result.FontQuality := fqFineAntialiasing;

  TextSize := Result.TextSize(FCaption);
  TextX := (FWidth - TextSize.cx) div 2;
  TextY := (FHeight - TextSize.cy) div 2;

  // Ombre du texte
  if AState <> bsDisabled then
    Result.TextOut(TextX + 1, TextY + 1, FCaption, BGRA(0, 0, 0, 100));

  // Texte principal
  if AState = bsDisabled then
    Result.TextOut(TextX, TextY, FCaption, BGRA(180, 180, 180))
  else
    Result.TextOut(TextX, TextY, FCaption, BGRAWhite);
end;

end.
```

### Utilisation du générateur de boutons

```pascal
var
  ButtonGen: TStylizedButton;
  NormalBtn, HoverBtn, PressedBtn: TBGRABitmap;
begin
  ButtonGen := TStylizedButton.Create(200, 50);
  try
    ButtonGen.Caption := 'Click Me!';
    ButtonGen.Style := bsGlass;
    ButtonGen.BaseColor := BGRA(30, 150, 30);

    // Générer les différents états
    NormalBtn := ButtonGen.Generate(bsNormal);
    HoverBtn := ButtonGen.Generate(bsHover);
    PressedBtn := ButtonGen.Generate(bsPressed);

    try
      NormalBtn.SaveToFile('button_normal.png');
      HoverBtn.SaveToFile('button_hover.png');
      PressedBtn.SaveToFile('button_pressed.png');
    finally
      NormalBtn.Free;
      HoverBtn.Free;
      PressedBtn.Free;
    end;
  finally
    ButtonGen.Free;
  end;
end;
```

---

## Projet 5 : Visionneuse d'images avec effets en temps réel

Une application complète avec prévisualisation et application d'effets.

```pascal
unit ImageViewer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, Menus, BGRABitmap, BGRABitmapTypes, BGRAFilters;

type
  TMainForm = class(TForm)
    PaintBox1: TPaintBox;
    Panel1: TPanel;
    Panel2: TPanel;
    OpenButton: TButton;
    SaveButton: TButton;
    ResetButton: TButton;
    EffectComboBox: TComboBox;
    ApplyButton: TButton;
    BrightnessTrackBar: TTrackBar;
    ContrastTrackBar: TTrackBar;
    Label1: TLabel;
    Label2: TLabel;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure OpenButtonClick(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    procedure ResetButtonClick(Sender: TObject);
    procedure ApplyButtonClick(Sender: TObject);
    procedure BrightnessTrackBarChange(Sender: TObject);
    procedure ContrastTrackBarChange(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
  private
    FOriginalImage: TBGRABitmap;
    FDisplayImage: TBGRABitmap;
    FModifiedImage: TBGRABitmap;
    procedure LoadImage(const AFileName: string);
    procedure UpdateDisplay;
    procedure ApplyEffect(const AEffectName: string);
    procedure ApplyBrightnessContrast;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

procedure TMainForm.FormCreate(Sender: TObject);  
begin
  FOriginalImage := nil;
  FDisplayImage := TBGRABitmap.Create(PaintBox1.Width, PaintBox1.Height);
  FModifiedImage := nil;

  // Remplir la liste des effets
  EffectComboBox.Items.Add('Aucun');
  EffectComboBox.Items.Add('Flou');
  EffectComboBox.Items.Add('Accentuation');
  EffectComboBox.Items.Add('Noir et blanc');
  EffectComboBox.Items.Add('Sépia');
  EffectComboBox.Items.Add('Négatif');
  EffectComboBox.Items.Add('Contours');
  EffectComboBox.Items.Add('Pixelisation');
  EffectComboBox.ItemIndex := 0;

  // Valeurs par défaut
  BrightnessTrackBar.Position := 0;
  ContrastTrackBar.Position := 0;
end;

procedure TMainForm.FormDestroy(Sender: TObject);  
begin
  if Assigned(FOriginalImage) then
    FOriginalImage.Free;
  if Assigned(FModifiedImage) then
    FModifiedImage.Free;
  FDisplayImage.Free;
end;

procedure TMainForm.LoadImage(const AFileName: string);  
var
  Loaded: TBGRABitmap;
  Ratio: Double;
  NewWidth, NewHeight: Integer;
begin
  Loaded := TBGRABitmap.Create(AFileName);
  try
    if Assigned(FOriginalImage) then
      FOriginalImage.Free;

    // Redimensionner pour tenir dans le PaintBox
    if (Loaded.Width > PaintBox1.Width) or (Loaded.Height > PaintBox1.Height) then
    begin
      Ratio := Loaded.Width / Loaded.Height;

      if (Loaded.Width / PaintBox1.Width) > (Loaded.Height / PaintBox1.Height) then
      begin
        NewWidth := PaintBox1.Width;
        NewHeight := Round(PaintBox1.Width / Ratio);
      end
      else
      begin
        NewHeight := PaintBox1.Height;
        NewWidth := Round(PaintBox1.Height * Ratio);
      end;

      FOriginalImage := Loaded.Resample(NewWidth, NewHeight, rmFineResample);
    end
    else
      FOriginalImage := Loaded.Duplicate;

    if Assigned(FModifiedImage) then
      FModifiedImage.Free;

    FModifiedImage := FOriginalImage.Duplicate;
    UpdateDisplay;
  finally
    Loaded.Free;
  end;
end;

procedure TMainForm.UpdateDisplay;  
var
  X, Y: Integer;
begin
  FDisplayImage.Fill(BGRA(240, 240, 240));

  if Assigned(FModifiedImage) then
  begin
    X := (PaintBox1.Width - FModifiedImage.Width) div 2;
    Y := (PaintBox1.Height - FModifiedImage.Height) div 2;
    FDisplayImage.PutImage(X, Y, FModifiedImage, dmSet);
  end;

  PaintBox1.Invalidate;
end;

procedure TMainForm.ApplyEffect(const AEffectName: string);  
var
  p: PBGRAPixel;
  n: Integer;
begin
  if not Assigned(FOriginalImage) then Exit;

  if Assigned(FModifiedImage) then
    FModifiedImage.Free;

  FModifiedImage := FOriginalImage.Duplicate;

  if AEffectName = 'Flou' then
    BGRAReplace(FModifiedImage, FModifiedImage.FilterBlurRadial(10, rbFast))
  else if AEffectName = 'Accentuation' then
    BGRAReplace(FModifiedImage, FModifiedImage.FilterSharpen(10))
  else if AEffectName = 'Noir et blanc' then
    BGRAReplace(FModifiedImage, FModifiedImage.FilterGrayscale)
  else if AEffectName = 'Sépia' then
  begin
    BGRAReplace(FModifiedImage, FModifiedImage.FilterGrayscale);
    p := FModifiedImage.Data;
    n := FModifiedImage.NbPixels;
    while n > 0 do
    begin
      p^.red := Min(255, Round(p^.red * 1.2));
      p^.green := Min(255, Round(p^.green * 1.0));
      p^.blue := Min(255, Round(p^.blue * 0.8));
      Inc(p);
      Dec(n);
    end;
    FModifiedImage.InvalidateBitmap;
  end
  else if AEffectName = 'Négatif' then
    FModifiedImage.LinearNegative
  else if AEffectName = 'Contours' then
    BGRAReplace(FModifiedImage, FModifiedImage.FilterContour)
  else if AEffectName = 'Pixelisation' then
    BGRAReplace(FModifiedImage, FModifiedImage.FilterPixelate(10, False, True));

  ApplyBrightnessContrast;
  UpdateDisplay;
end;

procedure TMainForm.ApplyBrightnessContrast;  
var
  p: PBGRAPixel;
  n: Integer;
  Brightness, Contrast: Integer;
  r, g, b: Integer;
begin
  if not Assigned(FModifiedImage) then Exit;

  Brightness := BrightnessTrackBar.Position;
  Contrast := ContrastTrackBar.Position;

  if (Brightness = 0) and (Contrast = 0) then Exit;

  p := FModifiedImage.Data;
  n := FModifiedImage.NbPixels;

  while n > 0 do
  begin
    // Luminosité
    r := p^.red + Brightness;
    g := p^.green + Brightness;
    b := p^.blue + Brightness;

    // Contraste
    if Contrast <> 0 then
    begin
      r := Round((r - 128) * (1 + Contrast / 100) + 128);
      g := Round((g - 128) * (1 + Contrast / 100) + 128);
      b := Round((b - 128) * (1 + Contrast / 100) + 128);
    end;

    p^.red := EnsureRange(r, 0, 255);
    p^.green := EnsureRange(g, 0, 255);
    p^.blue := EnsureRange(b, 0, 255);

    Inc(p);
    Dec(n);
  end;

  FModifiedImage.InvalidateBitmap;
end;

procedure TMainForm.OpenButtonClick(Sender: TObject);  
begin
  if OpenDialog1.Execute then
  begin
    LoadImage(OpenDialog1.FileName);
    ResetButtonClick(nil);
  end;
end;

procedure TMainForm.SaveButtonClick(Sender: TObject);  
begin
  if not Assigned(FModifiedImage) then
  begin
    ShowMessage('Aucune image à sauvegarder.');
    Exit;
  end;

  if SaveDialog1.Execute then
    FModifiedImage.SaveToFile(SaveDialog1.FileName);
end;

procedure TMainForm.ResetButtonClick(Sender: TObject);  
begin
  if not Assigned(FOriginalImage) then Exit;

  EffectComboBox.ItemIndex := 0;
  BrightnessTrackBar.Position := 0;
  ContrastTrackBar.Position := 0;

  if Assigned(FModifiedImage) then
    FModifiedImage.Free;

  FModifiedImage := FOriginalImage.Duplicate;
  UpdateDisplay;
end;

procedure TMainForm.ApplyButtonClick(Sender: TObject);  
begin
  ApplyEffect(EffectComboBox.Text);
end;

procedure TMainForm.BrightnessTrackBarChange(Sender: TObject);  
begin
  if not Assigned(FOriginalImage) then Exit;

  if Assigned(FModifiedImage) then
    FModifiedImage.Free;

  FModifiedImage := FOriginalImage.Duplicate;
  ApplyEffect(EffectComboBox.Text);
end;

procedure TMainForm.ContrastTrackBarChange(Sender: TObject);  
begin
  BrightnessTrackBarChange(Sender);
end;

procedure TMainForm.PaintBox1Paint(Sender: TObject);  
begin
  FDisplayImage.Draw(PaintBox1.Canvas, 0, 0, False);
end;

end.
```

---

## Astuces et techniques avancées

### 1. Cache pour améliorer les performances

```pascal
type
  TImageCache = class
  private
    FCache: TStringList;
    FMaxSize: Integer;
    FCacheSize: Int64;
  public
    constructor Create(AMaxSizeMB: Integer);
    destructor Destroy; override;
    function GetImage(const AKey: string): TBGRABitmap;
    procedure AddImage(const AKey: string; AImage: TBGRABitmap);
    procedure Clear;
  end;

constructor TImageCache.Create(AMaxSizeMB: Integer);  
begin
  FCache := TStringList.Create;
  FCache.OwnsObjects := True;
  FMaxSize := AMaxSizeMB * 1024 * 1024;
  FCacheSize := 0;
end;

destructor TImageCache.Destroy;  
begin
  Clear;
  FCache.Free;
  inherited;
end;

function TImageCache.GetImage(const AKey: string): TBGRABitmap;  
var
  Index: Integer;
begin
  Index := FCache.IndexOf(AKey);
  if Index >= 0 then
    Result := TBGRABitmap(FCache.Objects[Index]).Duplicate
  else
    Result := nil;
end;

procedure TImageCache.AddImage(const AKey: string; AImage: TBGRABitmap);  
var
  Cached: TBGRABitmap;
  ImageSize: Int64;
begin
  ImageSize := AImage.Width * AImage.Height * 4;

  // Vérifier si on dépasse la limite
  while (FCacheSize + ImageSize > FMaxSize) and (FCache.Count > 0) do
  begin
    FCacheSize := FCacheSize -
      (TBGRABitmap(FCache.Objects[0]).Width *
       TBGRABitmap(FCache.Objects[0]).Height * 4);
    FCache.Delete(0);
  end;

  Cached := AImage.Duplicate;
  FCache.AddObject(AKey, Cached);
  FCacheSize := FCacheSize + ImageSize;
end;

procedure TImageCache.Clear;  
begin
  FCache.Clear;
  FCacheSize := 0;
end;
```

### 2. Traitement en arrière-plan (threads)

```pascal
type
  TImageProcessThread = class(TThread)
  private
    FInputFile: string;
    FOutputFile: string;
    FEffect: string;
    FOnComplete: TNotifyEvent;
  protected
    procedure Execute; override;
    procedure DoComplete;
  public
    constructor Create(const AInputFile, AOutputFile, AEffect: string;
      AOnComplete: TNotifyEvent);
  end;

constructor TImageProcessThread.Create(const AInputFile, AOutputFile,
  AEffect: string; AOnComplete: TNotifyEvent);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FInputFile := AInputFile;
  FOutputFile := AOutputFile;
  FEffect := AEffect;
  FOnComplete := AOnComplete;
end;

procedure TImageProcessThread.Execute;  
var
  Image: TBGRABitmap;
begin
  Image := TBGRABitmap.Create(FInputFile);
  try
    // Appliquer l'effet
    if FEffect = 'blur' then
      BGRAReplace(Image, Image.FilterBlurRadial(15, rbFast))
    else if FEffect = 'grayscale' then
      BGRAReplace(Image, Image.FilterGrayscale);

    Image.SaveToFile(FOutputFile);
  finally
    Image.Free;
  end;

  Synchronize(@DoComplete);
end;

procedure TImageProcessThread.DoComplete;  
begin
  if Assigned(FOnComplete) then
    FOnComplete(Self);
end;

// Utilisation
procedure TForm1.ProcessImageAsync;  
begin
  TImageProcessThread.Create('input.jpg', 'output.jpg', 'blur',
    @OnProcessComplete);
end;

procedure TForm1.OnProcessComplete(Sender: TObject);  
begin
  ShowMessage('Traitement terminé !');
end;
```

### 3. Export vers PDF (avec utilisation de bibliothèques tierces)

```pascal
// Exemple conceptuel - nécessite fpReport ou une bibliothèque PDF
procedure ExportImageToPDF(AImage: TBGRABitmap; const APDFFile: string);  
var
  TempFile: string;
begin
  // Sauvegarder temporairement en PNG
  TempFile := GetTempDir + 'temp_image.png';
  AImage.SaveToFile(TempFile);

  // Utiliser une bibliothèque PDF pour intégrer l'image
  // (code dépendant de la bibliothèque utilisée)

  DeleteFile(TempFile);
end;
```

### 4. Détection de visages (approche simplifiée)

```pascal
// Détection basique de zones de couleur chair
function DetectSkinTones(AImage: TBGRABitmap): TBGRABitmap;  
var
  x, y: Integer;
  p: TBGRAPixel;
begin
  Result := TBGRABitmap.Create(AImage.Width, AImage.Height);
  Result.FillTransparent;

  for y := 0 to AImage.Height - 1 do
    for x := 0 to AImage.Width - 1 do
    begin
      p := AImage.GetPixel(x, y);

      // Détection simpliste de teinte chair
      if (p.red > 95) and (p.green > 40) and (p.blue > 20) and
         (p.red > p.green) and (p.red > p.blue) and
         (Abs(p.red - p.green) > 15) then
      begin
        Result.SetPixel(x, y, BGRA(255, 0, 0, 128));
      end;
    end;
end;
```

### 5. Création de GIF animés

```pascal
// Nécessite BGRAAnimatedGif (package séparé)
uses
  BGRAAnimatedGif;

procedure CreateAnimatedGif(const AFrames: array of TBGRABitmap;
  const AOutputFile: string; ADelayMS: Integer);
var
  Gif: TBGRAAnimatedGif;
  i: Integer;
begin
  Gif := TBGRAAnimatedGif.Create;
  try
    for i := 0 to High(AFrames) do
      Gif.AddFrame(AFrames[i], ADelayMS);

    Gif.SaveToFile(AOutputFile);
  finally
    Gif.Free;
  end;
end;
```

---

## Intégration avec d'autres technologies

### 1. Serveur web avec images générées

```pascal
// Exemple avec fpWeb
procedure TMyWebModule.HandleImageRequest(ARequest: TRequest;
  AResponse: TResponse);
var
  Image: TBGRABitmap;
  Stream: TMemoryStream;
begin
  Image := TBGRABitmap.Create(400, 300);
  Stream := TMemoryStream.Create;
  try
    // Générer l'image
    Image.Fill(BGRA(200, 220, 255));
    Image.TextOut(100, 150, 'Généré dynamiquement', BGRABlack);

    // Sauvegarder dans le stream
    Image.SaveToStreamAsPng(Stream);
    Stream.Position := 0;

    // Envoyer la réponse
    AResponse.ContentType := 'image/png';
    AResponse.ContentStream := Stream;
    AResponse.SendContent;
  finally
    Image.Free;
  end;
end;
```

### 2. Base de données avec images BLOB

```pascal
procedure SaveImageToDatabase(AImage: TBGRABitmap; AQuery: TSQLQuery);  
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    AImage.SaveToStreamAsPng(Stream);
    Stream.Position := 0;

    AQuery.SQL.Text := 'INSERT INTO images (data) VALUES (:data)';
    AQuery.ParamByName('data').LoadFromStream(Stream, ftBlob);
    AQuery.ExecSQL;
  finally
    Stream.Free;
  end;
end;

function LoadImageFromDatabase(AQuery: TSQLQuery): TBGRABitmap;  
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    TBlobField(AQuery.FieldByName('data')).SaveToStream(Stream);
    Stream.Position := 0;
    Result := TBGRABitmap.Create(Stream);
  finally
    Stream.Free;
  end;
end;
```

---

## Conclusion des projets pratiques

Ces projets démontrent la puissance et la flexibilité de BGRABitmap pour :

✅ **Traitement d'images en batch** (miniatures, filigranes)  
✅ **Génération de graphiques** (barres, camemberts)  
✅ **Création d'éléments d'interface** (boutons stylisés)  
✅ **Applications interactives** (visionneuse avec effets)  
✅ **Optimisation des performances** (cache, threads)  
✅ **Intégration système** (web, base de données)

### Points clés à retenir

- BGRABitmap excelle dans la génération d'images dynamiques
- Les performances sont bonnes pour la plupart des usages courants
- La portabilité Windows/Linux est garantie
- L'intégration avec d'autres composants Lazarus est simple
- Le code reste lisible et maintenable

### Pour aller plus loin

- **BGRAControls** : Composants visuels basés sur BGRABitmap
- **BGRABitmap-Games** : Frameworks pour jeux 2D
- **Documentation officielle** : http://bgrabitmap.github.io/
- **Exemples du dépôt** : https://github.com/bgrabitmap/bgrabitmap

Vous êtes maintenant équipé pour créer des applications graphiques professionnelles avec FreePascal/Lazarus !

⏭️ [OpenGL multi-plateforme](/12-interfaces-graphiques-avancees/03-opengl-multiplateforme.md)
