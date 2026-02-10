🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 12.9 Traitement d'images avancé avec FreePascal/Lazarus

## Introduction

Le traitement d'images est un domaine fascinant qui permet de manipuler, analyser et transformer des images numériques. Avec FreePascal et Lazarus, vous disposez d'outils puissants pour travailler avec les images de manière professionnelle, que ce soit pour de la retouche, de l'analyse scientifique, de la vision par ordinateur ou des effets visuels.

## Bibliothèques disponibles

### BGRABitmap - La référence pour Lazarus

**BGRABitmap** est la bibliothèque la plus utilisée pour le traitement d'images avancé sous Lazarus. Elle offre :

- Support complet de la transparence (canal alpha)
- Anti-aliasing de haute qualité
- Transformations géométriques avancées
- Filtres et effets visuels
- Dessin vectoriel
- Performance optimisée
- Compatible Windows et Ubuntu

**Installation :**
```pascal
// Via le gestionnaire de packages Lazarus :
// Package → Installer/Désinstaller des paquets → BGRABitmap
```

### Graphics32 - Performance et précision

**Graphics32** est une alternative axée sur la performance avec un contrôle au pixel près. Particulièrement adaptée pour :

- Applications nécessitant des rendus rapides
- Manipulation directe des pixels
- Jeux 2D
- Éditeurs graphiques

### FPImage - Natif FreePascal

**FPImage** fait partie de la FCL (Free Component Library) et offre des fonctionnalités de base sans dépendances externes.

## Formats d'images supportés

### Formats courants

| Format | Lecture | Écriture | Transparence | Usage typique |
|--------|---------|----------|--------------|---------------|
| PNG | ✓ | ✓ | Oui | Web, transparence |
| JPEG | ✓ | ✓ | Non | Photos, compression |
| BMP | ✓ | ✓ | Non | Windows natif |
| GIF | ✓ | ✓ | Oui | Animations |
| TIFF | ✓ | ✓ | Oui | Professionnel |
| WebP | ✓ | ✓ | Oui | Web moderne |

### Chargement d'images multi-format

```pascal
uses
  BGRABitmap, BGRABitmapTypes;

procedure TForm1.LoadImageFile(const FileName: string);  
var
  Bitmap: TBGRABitmap;
begin
  Bitmap := TBGRABitmap.Create(FileName);
  try
    // L'image est chargée, format détecté automatiquement
    Image1.Picture.Assign(Bitmap.Bitmap);
  finally
    Bitmap.Free;
  end;
end;
```

## Manipulation de base

### Redimensionnement avec qualité

Le redimensionnement d'images nécessite différents algorithmes selon l'usage :

```pascal
procedure ResizeImage(Source: TBGRABitmap; NewWidth, NewHeight: Integer): TBGRABitmap;  
begin
  // Méthode 1 : Redimensionnement simple (rapide)
  Result := Source.Resample(NewWidth, NewHeight);

  // Méthode 2 : Redimensionnement haute qualité (lent)
  Result := Source.Resample(NewWidth, NewHeight, rmFineResample);
end;
```

**Algorithmes de redimensionnement :**

- **rmSimpleStretch** : Le plus rapide, qualité basique
- **rmFineResample** : Haute qualité, interpolation avancée
- **rmLanczos** : Excellent pour réductions importantes
- **rmBicubic** : Bon compromis qualité/vitesse

### Rotation et transformations

```pascal
procedure RotateImage(Source: TBGRABitmap; AngleDegrees: Single): TBGRABitmap;  
begin
  // Rotation avec anti-aliasing
  Result := Source.RotateCW(AngleDegrees);
end;

procedure FlipImage(Source: TBGRABitmap; Horizontal: Boolean): TBGRABitmap;  
begin
  Result := TBGRABitmap.Create(Source.Width, Source.Height);
  if Horizontal then
    Result.HorizontalFlip(Source)
  else
    Result.VerticalFlip(Source);
end;
```

### Recadrage (Crop)

```pascal
function CropImage(Source: TBGRABitmap; X, Y, Width, Height: Integer): TBGRABitmap;  
var
  CropRect: TRect;
begin
  CropRect := Rect(X, Y, X + Width, Y + Height);
  Result := Source.GetPart(CropRect) as TBGRABitmap;
end;
```

## Filtres et effets visuels

### Flou (Blur)

Le flou adoucit l'image en mélangeant les pixels adjacents :

```pascal
procedure ApplyBlur(Bitmap: TBGRABitmap; Radius: Single);  
begin
  // Flou gaussien - naturel et professionnel
  BGRAReplace(Bitmap, Bitmap.FilterBlurRadial(Radius, rbFast));

  // Alternative : flou de mouvement
  // BGRAReplace(Bitmap, Bitmap.FilterBlurMotion(Radius, 0, True));
end;
```

**Types de flou :**
- **Gaussien** : Le plus naturel, simule une mise au point
- **Motion blur** : Effet de mouvement directionnel
- **Radial** : Flou circulaire depuis un point central
- **Box blur** : Rapide mais moins naturel

### Netteté (Sharpen)

La netteté accentue les contours et détails :

```pascal
procedure ApplySharpen(Bitmap: TBGRABitmap; Amount: Single);  
begin
  // Amount : 0.1 à 2.0 typiquement
  BGRAReplace(Bitmap, Bitmap.FilterSharpen(Amount));
end;
```

### Ajustements colorimétriques

#### Luminosité et Contraste

```pascal
procedure AdjustBrightnessContrast(Bitmap: TBGRABitmap;
  Brightness: Integer; Contrast: Integer);
var
  x, y: Integer;
  p: PBGRAPixel;
  factor: Single;
begin
  // Brightness : -255 à +255
  // Contrast : -100 à +100

  factor := (100.0 + Contrast) / 100.0;

  for y := 0 to Bitmap.Height - 1 do
  begin
    p := Bitmap.ScanLine[y];
    for x := 0 to Bitmap.Width - 1 do
    begin
      // Ajuster chaque composante de couleur
      p^.red := EnsureRange(Round((p^.red - 128) * factor + 128 + Brightness), 0, 255);
      p^.green := EnsureRange(Round((p^.green - 128) * factor + 128 + Brightness), 0, 255);
      p^.blue := EnsureRange(Round((p^.blue - 128) * factor + 128 + Brightness), 0, 255);
      Inc(p);
    end;
  end;
  Bitmap.InvalidateBitmap;
end;
```

#### Teinte, Saturation, Luminosité (HSL)

```pascal
procedure AdjustHSL(Bitmap: TBGRABitmap;
  HueShift: Integer; SaturationPercent: Integer; LightnessPercent: Integer);
var
  x, y: Integer;
  p: PBGRAPixel;
  hsl: THSLAPixel;
begin
  for y := 0 to Bitmap.Height - 1 do
  begin
    p := Bitmap.ScanLine[y];
    for x := 0 to Bitmap.Width - 1 do
    begin
      // Convertir RGB vers HSL
      hsl := BGRAToHSLA(p^);

      // Ajuster les valeurs
      hsl.hue := (hsl.hue + HueShift) mod 65536;
      hsl.saturation := EnsureRange(
        Round(hsl.saturation * (100 + SaturationPercent) / 100), 0, 65535);
      hsl.lightness := EnsureRange(
        Round(hsl.lightness * (100 + LightnessPercent) / 100), 0, 65535);

      // Reconvertir vers RGB
      p^ := HSLAToBGRA(hsl);
      Inc(p);
    end;
  end;
  Bitmap.InvalidateBitmap;
end;
```

### Effets artistiques

#### Effet Sépia (ancien)

```pascal
procedure ApplySepia(Bitmap: TBGRABitmap);  
var
  x, y: Integer;
  p: PBGRAPixel;
  tr, tg, tb: Integer;
begin
  for y := 0 to Bitmap.Height - 1 do
  begin
    p := Bitmap.ScanLine[y];
    for x := 0 to Bitmap.Width - 1 do
    begin
      tr := Round(p^.red * 0.393 + p^.green * 0.769 + p^.blue * 0.189);
      tg := Round(p^.red * 0.349 + p^.green * 0.686 + p^.blue * 0.168);
      tb := Round(p^.red * 0.272 + p^.green * 0.534 + p^.blue * 0.131);

      p^.red := Min(tr, 255);
      p^.green := Min(tg, 255);
      p^.blue := Min(tb, 255);
      Inc(p);
    end;
  end;
  Bitmap.InvalidateBitmap;
end;
```

#### Niveaux de gris

```pascal
procedure ConvertToGrayscale(Bitmap: TBGRABitmap);  
var
  x, y: Integer;
  p: PBGRAPixel;
  gray: Byte;
begin
  for y := 0 to Bitmap.Height - 1 do
  begin
    p := Bitmap.ScanLine[y];
    for x := 0 to Bitmap.Width - 1 do
    begin
      // Formule perceptuelle (l'œil humain est plus sensible au vert)
      gray := Round(p^.red * 0.299 + p^.green * 0.587 + p^.blue * 0.114);
      p^.red := gray;
      p^.green := gray;
      p^.blue := gray;
      Inc(p);
    end;
  end;
  Bitmap.InvalidateBitmap;
end;
```

#### Effet Négatif

```pascal
procedure ApplyNegative(Bitmap: TBGRABitmap);  
var
  x, y: Integer;
  p: PBGRAPixel;
begin
  for y := 0 to Bitmap.Height - 1 do
  begin
    p := Bitmap.ScanLine[y];
    for x := 0 to Bitmap.Width - 1 do
    begin
      p^.red := 255 - p^.red;
      p^.green := 255 - p^.green;
      p^.blue := 255 - p^.blue;
      Inc(p);
    end;
  end;
  Bitmap.InvalidateBitmap;
end;
```

## Détection et analyse

### Détection de contours

La détection de contours identifie les zones de changement rapide d'intensité :

```pascal
function DetectEdges(Source: TBGRABitmap): TBGRABitmap;  
var
  Sobel: TBGRABitmap;
begin
  // Appliquer le filtre Sobel pour détecter les contours
  Sobel := Source.FilterSobel;
  Result := Sobel;
end;
```

**Algorithmes de détection :**
- **Sobel** : Détection directionnelle, robuste
- **Prewitt** : Similaire à Sobel, moins sensible au bruit
- **Canny** : Détection multi-étapes, très précis
- **Laplacien** : Détection isotropique

### Histogramme

L'histogramme représente la distribution des valeurs de pixels :

```pascal
type
  THistogram = array[0..255] of Integer;

procedure CalculateHistogram(Bitmap: TBGRABitmap;
  var RedHist, GreenHist, BlueHist: THistogram);
var
  x, y: Integer;
  p: PBGRAPixel;
begin
  // Initialiser
  FillChar(RedHist, SizeOf(THistogram), 0);
  FillChar(GreenHist, SizeOf(THistogram), 0);
  FillChar(BlueHist, SizeOf(THistogram), 0);

  // Compter les pixels
  for y := 0 to Bitmap.Height - 1 do
  begin
    p := Bitmap.ScanLine[y];
    for x := 0 to Bitmap.Width - 1 do
    begin
      Inc(RedHist[p^.red]);
      Inc(GreenHist[p^.green]);
      Inc(BlueHist[p^.blue]);
      Inc(p);
    end;
  end;
end;
```

### Égalisation d'histogramme

Améliore le contraste en redistribuant les valeurs :

```pascal
procedure EqualizeHistogram(Bitmap: TBGRABitmap);  
var
  Hist: THistogram;
  CDF: array[0..255] of Integer;
  LUT: array[0..255] of Byte;
  x, y, i: Integer;
  p: PBGRAPixel;
  TotalPixels, MinCDF: Integer;
begin
  // Calculer l'histogramme en niveaux de gris
  FillChar(Hist, SizeOf(THistogram), 0);

  for y := 0 to Bitmap.Height - 1 do
  begin
    p := Bitmap.ScanLine[y];
    for x := 0 to Bitmap.Width - 1 do
    begin
      // Convertir en niveau de gris
      Inc(Hist[Round(p^.red * 0.299 + p^.green * 0.587 + p^.blue * 0.114)]);
      Inc(p);
    end;
  end;

  // Calculer la fonction de distribution cumulative (CDF)
  TotalPixels := Bitmap.Width * Bitmap.Height;
  CDF[0] := Hist[0];
  for i := 1 to 255 do
    CDF[i] := CDF[i - 1] + Hist[i];

  // Trouver le minimum non-zéro
  MinCDF := CDF[0];
  for i := 0 to 255 do
    if (CDF[i] > 0) and (CDF[i] < MinCDF) then
      MinCDF := CDF[i];

  // Créer la table de correspondance (Look-Up Table)
  for i := 0 to 255 do
    LUT[i] := Round((CDF[i] - MinCDF) / (TotalPixels - MinCDF) * 255);

  // Appliquer la transformation
  for y := 0 to Bitmap.Height - 1 do
  begin
    p := Bitmap.ScanLine[y];
    for x := 0 to Bitmap.Width - 1 do
    begin
      p^.red := LUT[p^.red];
      p^.green := LUT[p^.green];
      p^.blue := LUT[p^.blue];
      Inc(p);
    end;
  end;

  Bitmap.InvalidateBitmap;
end;
```

## Masques et sélections

### Créer un masque

Un masque permet d'appliquer des effets à certaines zones seulement :

```pascal
type
  TMask = class
  private
    FData: array of Byte;
    FWidth, FHeight: Integer;
  public
    constructor Create(AWidth, AHeight: Integer);
    procedure SetPixel(X, Y: Integer; Value: Byte);
    function GetPixel(X, Y: Integer): Byte;
    procedure Fill(Value: Byte);
    procedure InvertMask;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
  end;

type
  TEffectProcedure = procedure(ABitmap: TBGRABitmap);

procedure ApplyMaskEffect(Bitmap: TBGRABitmap; Mask: TMask;
  EffectProc: TEffectProcedure);
var
  x, y: Integer;
  p: PBGRAPixel;
  MaskValue: Byte;
  Temp: TBGRABitmap;
begin
  // Créer une copie avec l'effet appliqué
  Temp := Bitmap.Duplicate;
  EffectProc(Temp);

  // Mélanger selon le masque
  for y := 0 to Bitmap.Height - 1 do
  begin
    p := Bitmap.ScanLine[y];
    for x := 0 to Bitmap.Width - 1 do
    begin
      MaskValue := Mask.GetPixel(x, y);
      // Mélanger proportionnellement au masque
      p^ := MergeBGRA(p^, Temp.GetPixel(x, y), MaskValue);
      Inc(p);
    end;
  end;

  Temp.Free;
  Bitmap.InvalidateBitmap;
end;
```

## Composition et superposition

### Modes de fusion (Blend Modes)

```pascal
type
  TBlendMode = (bmNormal, bmMultiply, bmScreen, bmOverlay, bmAdd, bmSubtract);

function BlendPixels(Background, Foreground: TBGRAPixel;
  Mode: TBlendMode; Opacity: Byte): TBGRAPixel;
var
  r, g, b: Integer;
begin
  case Mode of
    bmNormal:
      Result := MergeBGRA(Background, Foreground, Opacity);

    bmMultiply:
      begin
        r := (Background.red * Foreground.red) div 255;
        g := (Background.green * Foreground.green) div 255;
        b := (Background.blue * Foreground.blue) div 255;
        Result := BGRA(r, g, b, 255);
      end;

    bmScreen:
      begin
        r := 255 - ((255 - Background.red) * (255 - Foreground.red)) div 255;
        g := 255 - ((255 - Background.green) * (255 - Foreground.green)) div 255;
        b := 255 - ((255 - Background.blue) * (255 - Foreground.blue)) div 255;
        Result := BGRA(r, g, b, 255);
      end;

    bmAdd:
      begin
        r := Min(Background.red + Foreground.red, 255);
        g := Min(Background.green + Foreground.green, 255);
        b := Min(Background.blue + Foreground.blue, 255);
        Result := BGRA(r, g, b, 255);
      end;
  end;
end;
```

### Calques (Layers)

```pascal
type
  TImageLayer = class
  private
    FBitmap: TBGRABitmap;
    FOpacity: Byte;
    FBlendMode: TBlendMode;
    FVisible: Boolean;
    FX, FY: Integer;
  public
    constructor Create(AWidth, AHeight: Integer);
    destructor Destroy; override;
    property Bitmap: TBGRABitmap read FBitmap;
    property Opacity: Byte read FOpacity write FOpacity;
    property BlendMode: TBlendMode read FBlendMode write FBlendMode;
    property Visible: Boolean read FVisible write FVisible;
    property X: Integer read FX write FX;
    property Y: Integer read FY write FY;
  end;

procedure ComposeLayers(Layers: TList; Output: TBGRABitmap);  
var
  i: Integer;
  Layer: TImageLayer;
begin
  // Fond transparent
  Output.Fill(BGRAPixelTransparent);

  // Composer chaque calque
  for i := 0 to Layers.Count - 1 do
  begin
    Layer := TImageLayer(Layers[i]);
    if Layer.Visible then
    begin
      Output.PutImage(Layer.X, Layer.Y, Layer.Bitmap,
        Layer.BlendMode, Layer.Opacity);
    end;
  end;
end;
```

## Optimisation des performances

### Traitement multi-thread

Pour les images volumineuses, le multi-threading accélère considérablement le traitement :

```pascal
uses
  MTProcs; // Multi-Threading Procedures

{ Note : la procédure anonyme ci-dessous nécessite
  {$modeswitch anonymousfunctions} en mode ObjFPC (FPC 3.3.1+),
  ou {$mode delphi}. }

procedure ParallelProcessImage(Bitmap: TBGRABitmap;
  ProcessLine: TProcessLineProc);
begin
  ProcThreadPool.DoParallel(
    procedure(Index: PtrInt; Data: Pointer; Item: TMultiThreadProcItem)
    var
      y: Integer;
    begin
      y := Index;
      ProcessLine(Bitmap.ScanLine[y], Bitmap.Width);
    end,
    0, Bitmap.Height - 1
  );
  Bitmap.InvalidateBitmap;
end;
```

### Cache et réutilisation

```pascal
type
  TImageCache = class
  private
    FCache: TStringList;
    FMaxSize: Integer;
  public
    constructor Create(AMaxSize: Integer);
    destructor Destroy; override;
    function GetImage(const Key: string): TBGRABitmap;
    procedure PutImage(const Key: string; Image: TBGRABitmap);
    procedure Clear;
  end;
```

### Optimisation mémoire

```pascal
procedure ProcessLargeImage(const FileName: string);  
var
  Stream: TFileStream;
  Reader: TFPReaderPNG;
  Img: TFPMemoryImage;
  TileSize: Integer;
  x, y: Integer;
begin
  TileSize := 512; // Traiter par tuiles de 512x512

  Stream := TFileStream.Create(FileName, fmOpenRead);
  try
    Reader := TFPReaderPNG.Create;
    try
      // Charger uniquement les métadonnées
      Reader.ImageRead(Stream, Img);

      // Traiter tuile par tuile
      for y := 0 to (Img.Height div TileSize) do
        for x := 0 to (Img.Width div TileSize) do
          ProcessTile(Img, x * TileSize, y * TileSize, TileSize);

    finally
      Reader.Free;
    end;
  finally
    Stream.Free;
  end;
end;
```

## Différences Windows / Ubuntu

### Chemins et bibliothèques

```pascal
{$IFDEF WINDOWS}
  const LibImageMagick = 'CORE_RL_magick_.dll';
{$ENDIF}
{$IFDEF UNIX}
  const LibImageMagick = 'libMagickCore.so';
{$ENDIF}
```

### Polices et rendu de texte

```pascal
procedure SetupFontRendering;  
begin
  {$IFDEF WINDOWS}
  // Windows : GDI+
  Bitmap.FontRenderer := TBGRADefaultFontRenderer.Create;
  {$ENDIF}

  {$IFDEF UNIX}
  // Ubuntu : FreeType
  Bitmap.FontRenderer := TBGRAFreeTypeFontRenderer.Create;
  {$ENDIF}
end;
```

### Accélération matérielle

```pascal
procedure EnableHardwareAcceleration;  
begin
  {$IFDEF WINDOWS}
  // Direct2D sur Windows
  BGRABitmap.UseDirect2D := True;
  {$ENDIF}

  {$IFDEF UNIX}
  // OpenGL sur Linux
  BGRABitmap.UseOpenGL := True;
  {$ENDIF}
end;
```

## Exemple complet : Éditeur d'images simple

```pascal
unit ImageEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  BGRABitmap, BGRABitmapTypes;

type
  TFormImageEditor = class(TForm)
    ImageDisplay: TImage;
    BtnLoad: TButton;
    BtnSave: TButton;
    BtnBlur: TButton;
    BtnSharpen: TButton;
    BtnGrayscale: TButton;
    TrackBrightness: TTrackBar;
    TrackContrast: TTrackBar;
    procedure BtnLoadClick(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
    procedure BtnBlurClick(Sender: TObject);
    procedure ApplyAdjustments(Sender: TObject);
  private
    FOriginal: TBGRABitmap;
    FCurrent: TBGRABitmap;
    procedure UpdateDisplay;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

procedure TFormImageEditor.BtnLoadClick(Sender: TObject);  
var
  OpenDialog: TOpenDialog;
begin
  OpenDialog := TOpenDialog.Create(nil);
  try
    OpenDialog.Filter := 'Images|*.png;*.jpg;*.bmp;*.gif';
    if OpenDialog.Execute then
    begin
      FOriginal.Free;
      FOriginal := TBGRABitmap.Create(OpenDialog.FileName);
      ApplyAdjustments(nil);
    end;
  finally
    OpenDialog.Free;
  end;
end;

procedure TFormImageEditor.ApplyAdjustments(Sender: TObject);  
begin
  if FOriginal = nil then Exit;

  FCurrent.Free;
  FCurrent := FOriginal.Duplicate;

  // Appliquer luminosité et contraste
  AdjustBrightnessContrast(FCurrent,
    TrackBrightness.Position,
    TrackContrast.Position);

  UpdateDisplay;
end;

procedure TFormImageEditor.UpdateDisplay;  
begin
  if FCurrent <> nil then
    ImageDisplay.Picture.Assign(FCurrent.Bitmap);
end;

end.
```

## Ressources et bibliothèques complémentaires

### Bibliothèques externes

- **ImageMagick** : Traitement professionnel d'images
- **OpenCV** : Vision par ordinateur (voir chapitre 12.10)
- **LibGD** : Génération d'images dynamiques
- **FreeImage** : Support étendu de formats

### Documentation

- **BGRABitmap Wiki** : https://wiki.freepascal.org/BGRABitmap
- **Graphics32** : http://www.graphics32.org
- **Forum Lazarus** : https://forum.lazarus.freepascal.org

## Conclusion

Le traitement d'images avec FreePascal et Lazarus offre des possibilités quasi illimitées, de la simple retouche aux applications professionnelles d'analyse d'images. BGRABitmap, en particulier, constitue une base solide et performante pour développer vos applications multi-plateformes.

Les concepts présentés ici vous permettent de :
- Manipuler efficacement les images (chargement, transformation, sauvegarde)
- Appliquer des filtres et effets avancés
- Analyser le contenu des images
- Optimiser les performances pour de grandes images
- Créer des applications portables Windows/Ubuntu

N'hésitez pas à expérimenter et à combiner ces techniques pour créer vos propres algorithmes de traitement d'images !

⏭️ [Vision par ordinateur avec OpenCV](/12-interfaces-graphiques-avancees/10-vision-ordinateur-opencv.md)
