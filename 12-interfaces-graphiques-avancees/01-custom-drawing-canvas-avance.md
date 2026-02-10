🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 12.1 Custom Drawing et Canvas Avancé

## Introduction

Le **custom drawing** (dessin personnalisé) est une technique fondamentale qui vous permet de créer vos propres éléments graphiques au lieu d'utiliser uniquement les composants visuels standard de Lazarus. Le **Canvas** est l'objet principal qui vous permet de dessiner : lignes, formes, textes, images, etc.

Cette section vous guidera à travers les concepts essentiels pour maîtriser le dessin personnalisé dans vos applications FreePascal/Lazarus, que vous développiez sur Windows ou Ubuntu.

---

## Qu'est-ce que le Canvas ?

Le **Canvas** est une surface de dessin abstraite fournie par la LCL (Lazarus Component Library). Il représente une zone sur laquelle vous pouvez dessiner en utilisant des méthodes et des propriétés simples.

### Caractéristiques principales

- **Abstraction multi-plateforme** : Le Canvas fonctionne de manière identique sur Windows (Win32/Win64) et Ubuntu/Linux (GTK2/GTK3/Qt5)
- **Accès via les composants** : Presque tous les composants visuels ont une propriété `Canvas`
- **Événement OnPaint** : C'est l'événement clé pour effectuer du dessin personnalisé
- **Double buffering** : Peut être activé pour éviter les scintillements

---

## Les bases du dessin

### Accéder au Canvas

Le Canvas est accessible principalement dans deux contextes :

1. **Lors de l'événement OnPaint d'un composant**
2. **En dehors de OnPaint** (moins recommandé, peut causer des problèmes de rafraîchissement)

```pascal
procedure TForm1.FormPaint(Sender: TObject);  
begin
  // Le Canvas du formulaire est accessible directement
  Canvas.TextOut(10, 10, 'Bonjour depuis le Canvas !');
end;
```

### L'événement OnPaint

C'est **l'événement le plus important** pour le custom drawing. Il est déclenché automatiquement lorsque :

- Le composant doit être redessiné (première affichage, redimensionnement, déplacement)
- Vous appelez la méthode `Invalidate` ou `Refresh`

```pascal
procedure TForm1.PaintBox1Paint(Sender: TObject);  
begin
  with PaintBox1.Canvas do
  begin
    // Votre code de dessin ici
    Brush.Color := clWhite;
    FillRect(ClipRect);

    Pen.Color := clBlue;
    Pen.Width := 2;
    Rectangle(10, 10, 100, 100);
  end;
end;
```

**Conseil** : Utilisez toujours `OnPaint` pour dessiner, jamais de code de dessin dans d'autres événements (sauf cas particuliers).

---

## Les propriétés essentielles du Canvas

### Le Pen (Stylo)

Le `Pen` contrôle **comment sont dessinées les lignes et les contours** des formes.

```pascal
Canvas.Pen.Color := clRed;        // Couleur du trait  
Canvas.Pen.Width := 3;             // Épaisseur du trait (en pixels)  
Canvas.Pen.Style := psSolid;       // Style : psSolid, psDash, psDot, psDashDot, etc.
```

**Styles disponibles** :
- `psSolid` : Ligne continue
- `psDash` : Tirets
- `psDot` : Pointillés
- `psDashDot` : Tiret-point alternés
- `psClear` : Invisible (pas de contour)

### Le Brush (Pinceau)

Le `Brush` contrôle **comment sont remplies les formes** (l'intérieur).

```pascal
Canvas.Brush.Color := clYellow;    // Couleur de remplissage  
Canvas.Brush.Style := bsSolid;     // Style de remplissage
```

**Styles de Brush** :
- `bsSolid` : Remplissage uni
- `bsClear` : Pas de remplissage (transparent)
- `bsHorizontal`, `bsVertical` : Lignes horizontales/verticales
- `bsFDiagonal`, `bsBDiagonal` : Diagonales
- `bsCross`, `bsDiagCross` : Quadrillages

### Le Font (Police)

Le `Font` contrôle l'apparence du texte.

```pascal
Canvas.Font.Name := 'Arial';       // Nom de la police  
Canvas.Font.Size := 14;            // Taille en points  
Canvas.Font.Color := clBlack;      // Couleur du texte  
Canvas.Font.Style := [fsBold];     // Style : fsBold, fsItalic, fsUnderline, fsStrikeOut
```

---

## Les méthodes de dessin de base

### Dessiner des lignes

```pascal
// Ligne simple de (x1,y1) à (x2,y2)
Canvas.Line(10, 10, 200, 150);

// OU en déplaçant le "curseur" puis en traçant
Canvas.MoveTo(10, 10);      // Déplace le point de départ  
Canvas.LineTo(200, 150);    // Trace jusqu'au point d'arrivée
```

### Dessiner des rectangles

```pascal
// Rectangle avec contour et remplissage
Canvas.Rectangle(50, 50, 200, 150);  // (x1, y1, x2, y2)

// Rectangle rempli sans contour
Canvas.FillRect(Rect(50, 50, 200, 150));  // Utilise Brush uniquement

// Rectangle avec coins arrondis
Canvas.RoundRect(50, 50, 200, 150, 20, 20);  // Les deux derniers = rayon des coins
```

### Dessiner des cercles et ellipses

```pascal
// Ellipse inscrite dans un rectangle
Canvas.Ellipse(50, 50, 200, 150);

// Cercle (ellipse avec largeur = hauteur)
Canvas.Ellipse(100, 100, 200, 200);
```

### Dessiner des polygones

```pascal
var
  Points: array of TPoint;
begin
  SetLength(Points, 3);  // Triangle
  Points[0] := Point(100, 50);
  Points[1] := Point(50, 150);
  Points[2] := Point(150, 150);

  Canvas.Polygon(Points);  // Trace et remplit automatiquement
end;
```

### Dessiner des arcs et secteurs

```pascal
// Arc (portion de cercle/ellipse)
Canvas.Arc(50, 50, 200, 200, 200, 125, 125, 50);

// Secteur (camembert)
Canvas.Pie(50, 50, 200, 200, 200, 125, 125, 50);
```

### Afficher du texte

```pascal
// Texte simple à une position
Canvas.TextOut(100, 100, 'Mon texte');

// Texte dans un rectangle (avec alignement possible)
var
  R: TRect;
begin
  R := Rect(50, 50, 200, 150);
  Canvas.TextRect(R, 50, 50, 'Texte dans zone');
end;

// Obtenir les dimensions d'un texte
var
  TextWidth, TextHeight: Integer;
begin
  TextWidth := Canvas.TextWidth('Mon texte');
  TextHeight := Canvas.TextHeight('Mon texte');
end;
```

---

## Dessiner des images

### Charger et afficher une image

```pascal
var
  MyPicture: TPicture;
begin
  MyPicture := TPicture.Create;
  try
    MyPicture.LoadFromFile('mon_image.png');
    Canvas.Draw(10, 10, MyPicture.Graphic);  // Affiche à la position (10, 10)
  finally
    MyPicture.Free;
  end;
end;
```

### Redimensionner une image lors du dessin

```pascal
// StretchDraw : étire l'image dans un rectangle
var
  DestRect: TRect;
begin
  DestRect := Rect(0, 0, 200, 150);
  Canvas.StretchDraw(DestRect, MyPicture.Graphic);
end;
```

### Copier une zone d'un Canvas à un autre

```pascal
// CopyRect : copie une zone rectangulaire
Canvas.CopyRect(
  Rect(0, 0, 100, 100),          // Rectangle destination
  SourceCanvas,                   // Canvas source
  Rect(50, 50, 150, 150)         // Rectangle source
);
```

---

## Techniques avancées

### Le Double Buffering

Le **double buffering** évite les scintillements lors du dessin en dessinant d'abord hors écran, puis en affichant le résultat final d'un coup.

**Activation automatique** :

```pascal
// Pour un formulaire
Form1.DoubleBuffered := True;

// Pour un composant spécifique
PaintBox1.DoubleBuffered := True;
```

**Double buffering manuel** (pour plus de contrôle) :

```pascal
var
  Buffer: TBitmap;
begin
  Buffer := TBitmap.Create;
  try
    Buffer.SetSize(PaintBox1.Width, PaintBox1.Height);

    // Dessiner sur le buffer
    Buffer.Canvas.Brush.Color := clWhite;
    Buffer.Canvas.FillRect(Rect(0, 0, Buffer.Width, Buffer.Height));
    Buffer.Canvas.Pen.Color := clBlue;
    Buffer.Canvas.Rectangle(10, 10, 100, 100);

    // Copier le buffer sur le Canvas visible
    PaintBox1.Canvas.Draw(0, 0, Buffer);
  finally
    Buffer.Free;
  end;
end;
```

### Le ClipRect

Le `ClipRect` est la zone qui doit être redessinée. Optimisez vos dessins en ne redessinant que cette zone.

```pascal
procedure TForm1.PaintBox1Paint(Sender: TObject);  
var
  ClipR: TRect;
begin
  ClipR := PaintBox1.Canvas.ClipRect;

  // Ne dessiner que dans la zone ClipR
  PaintBox1.Canvas.Brush.Color := clWhite;
  PaintBox1.Canvas.FillRect(ClipR);

  // Dessiner vos éléments...
end;
```

### Modes de dessin (CopyMode)

Le `CopyMode` définit comment les pixels sont combinés lors du dessin.

```pascal
Canvas.CopyMode := cmSrcCopy;    // Mode par défaut : copie directe  
Canvas.CopyMode := cmSrcAnd;     // ET logique  
Canvas.CopyMode := cmSrcInvert;  // Inverse les couleurs  
Canvas.CopyMode := cmSrcPaint;   // OU logique
```

### Anti-aliasing et qualité de dessin

```pascal
// Activer l'anti-aliasing (lissage) - disponible selon le widgetset
{$IFDEF WINDOWS}
Canvas.Font.Quality := fqClearType;  // Ou fqAntialiased
{$ENDIF}

{$IFDEF LINUX}
// GTK gère l'anti-aliasing automatiquement dans la plupart des cas
{$ENDIF}
```

### Transformations géométriques

FreePascal/Lazarus offre des transformations basiques selon la plateforme :

```pascal
// Exemple de rotation manuelle (calculs trigonométriques)
function RotatePoint(const Point: TPoint; const Center: TPoint; Angle: Double): TPoint;  
var
  S, C: Double;
  DX, DY: Integer;
begin
  S := Sin(Angle);
  C := Cos(Angle);
  DX := Point.X - Center.X;
  DY := Point.Y - Center.Y;
  Result.X := Round(DX * C - DY * S) + Center.X;
  Result.Y := Round(DX * S + DY * C) + Center.Y;
end;
```

---

## Gestion des couleurs

### Couleurs prédéfinies

Lazarus fournit de nombreuses couleurs prédéfinies :

```pascal
clBlack, clWhite, clRed, clGreen, clBlue, clYellow, clAqua, clFuchsia,  
clGray, clMaroon, clNavy, clOlive, clPurple, clTeal, clSilver, clLime
```

### Créer des couleurs personnalisées

```pascal
// Fonction RGBToColor
Canvas.Brush.Color := RGBToColor(255, 128, 64);  // R, G, B

// Extraction des composantes
var
  R, G, B: Byte;
  MyColor: TColor;
begin
  MyColor := clRed;
  RedGreenBlue(MyColor, R, G, B);
  ShowMessage(Format('R=%d, G=%d, B=%d', [R, G, B]));
end;
```

### Transparence et canal alpha

```pascal
// TColor supporte le canal alpha (mais support limité selon widgetset)
var
  ColorWithAlpha: TColor;
begin
  // Format : $AARRGGBB (AA = alpha, 00 = transparent, FF = opaque)
  ColorWithAlpha := $80FF0000;  // Rouge semi-transparent
end;
```

---

## Optimisation du dessin

### Conseils généraux

1. **Minimiser les appels de dessin** : Regroupez les opérations similaires
2. **Utiliser le ClipRect** : Ne redessinez que ce qui est nécessaire
3. **Double buffering** : Toujours pour les animations ou dessins complexes
4. **Éviter les allocations** : Créez les objets bitmap/brushes une seule fois si possible
5. **Mesurer les performances** : Utilisez des outils de profiling

### Exemple d'optimisation

```pascal
// ❌ MAUVAIS : création répétée d'objets
procedure TForm1.PaintBox1Paint(Sender: TObject);  
var
  Bmp: TBitmap;
  i: Integer;
begin
  for i := 0 to 100 do
  begin
    Bmp := TBitmap.Create;  // Allocation à chaque itération !
    try
      // Dessiner quelque chose
      Bmp.SetSize(50, 50);
      Canvas.Draw(i * 60, 10, Bmp);
    finally
      Bmp.Free;
    end;
  end;
end;

// ✅ BON : réutilisation d'objets
procedure TForm1.PaintBox1Paint(Sender: TObject);  
var
  Bmp: TBitmap;
  i: Integer;
begin
  Bmp := TBitmap.Create;
  try
    Bmp.SetSize(50, 50);
    for i := 0 to 100 do
    begin
      // Réutiliser le même bitmap
      Canvas.Draw(i * 60, 10, Bmp);
    end;
  finally
    Bmp.Free;
  end;
end;
```

---

## Différences multi-plateformes

### Windows vs Linux

Le Canvas est bien abstrait par la LCL, mais quelques différences subsistent :

| Aspect | Windows | Linux (GTK) |
|--------|---------|-------------|
| **Anti-aliasing** | Via Font.Quality | Automatique (selon configuration système) |
| **Performance** | GDI/GDI+ natif | Cairo (GTK3) généralement plus lent |
| **Rendu des polices** | ClearType disponible | FreeType/Fontconfig |
| **Transparence** | Support limité alpha | Meilleur support alpha (GTK3) |

### Code portable

```pascal
procedure DrawSomething(ACanvas: TCanvas);  
begin
  // Ce code fonctionne identiquement sur Windows et Linux
  ACanvas.Brush.Color := clLightBlue;
  ACanvas.FillRect(Rect(0, 0, 100, 100));
  ACanvas.Font.Size := 12;
  ACanvas.TextOut(10, 10, 'Cross-platform !');
end;
```

### Spécificités à gérer

```pascal
{$IFDEF WINDOWS}
  // Code spécifique Windows si nécessaire
  Canvas.Font.Quality := fqClearType;
{$ENDIF}

{$IFDEF LINUX}
  // Code spécifique Linux si nécessaire
  // GTK gère différemment certains aspects
{$ENDIF}
```

---

## Composants utiles pour le custom drawing

### TPaintBox

Le composant idéal pour faire du custom drawing pur.

```pascal
// Création en code
var
  PB: TPaintBox;
begin
  PB := TPaintBox.Create(Self);
  PB.Parent := Form1;
  PB.SetBounds(10, 10, 300, 200);
  PB.OnPaint := @MyPaintBoxPaint;
end;

procedure TForm1.MyPaintBoxPaint(Sender: TObject);  
begin
  with (Sender as TPaintBox).Canvas do
  begin
    // Votre dessin ici
  end;
end;
```

**Avantages de TPaintBox** :
- Léger (pas de fenêtre système)
- Performant
- Facile à manipuler

### TImage

Permet d'afficher et manipuler des images avec accès au Canvas.

```pascal
Image1.Picture.LoadFromFile('photo.jpg');  
Image1.Canvas.Pen.Color := clRed;  
Image1.Canvas.Rectangle(10, 10, 100, 100);  // Dessiner par-dessus l'image
```

### TDrawGrid

Pour dessiner dans une grille (tableaux personnalisés).

```pascal
procedure TForm1.DrawGrid1DrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
begin
  with DrawGrid1.Canvas do
  begin
    Brush.Color := clWhite;
    FillRect(aRect);
    TextOut(aRect.Left + 5, aRect.Top + 5, Format('Cell[%d,%d]', [aCol, aRow]));
  end;
end;
```

---

## Exemple complet : Éditeur de dessin simple

Voici un exemple complet qui combine plusieurs concepts :

```pascal
unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, ExtCtrls, StdCtrls;

type
  TForm1 = class(TForm)
    PaintBox1: TPaintBox;
    ColorButton: TColorButton;
    WidthTrackBar: TTrackBar;
    ClearButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure PaintBox1Paint(Sender: TObject);
    procedure ClearButtonClick(Sender: TObject);
  private
    FDrawing: Boolean;
    FLastPoint: TPoint;
    FBuffer: TBitmap;  // Double buffering
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);  
begin
  FDrawing := False;
  FBuffer := TBitmap.Create;
  FBuffer.SetSize(PaintBox1.Width, PaintBox1.Height);

  // Fond blanc initial
  FBuffer.Canvas.Brush.Color := clWhite;
  FBuffer.Canvas.FillRect(Rect(0, 0, FBuffer.Width, FBuffer.Height));

  // Note : TPaintBox est un TGraphicControl (pas de handle de fenêtre),
  // DoubleBuffered n'a pas d'effet. On utilise FBuffer à la place.
end;

procedure TForm1.FormDestroy(Sender: TObject);  
begin
  FBuffer.Free;
end;

procedure TForm1.PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    FDrawing := True;
    FLastPoint := Point(X, Y);
  end;
end;

procedure TForm1.PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    FDrawing := False;
end;

procedure TForm1.PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);  
begin
  if FDrawing then
  begin
    // Dessiner sur le buffer
    FBuffer.Canvas.Pen.Color := ColorButton.ButtonColor;
    FBuffer.Canvas.Pen.Width := WidthTrackBar.Position;
    FBuffer.Canvas.MoveTo(FLastPoint.X, FLastPoint.Y);
    FBuffer.Canvas.LineTo(X, Y);

    FLastPoint := Point(X, Y);
    PaintBox1.Invalidate;  // Demander un rafraîchissement
  end;
end;

procedure TForm1.PaintBox1Paint(Sender: TObject);  
begin
  // Copier le buffer sur le PaintBox
  PaintBox1.Canvas.Draw(0, 0, FBuffer);
end;

procedure TForm1.ClearButtonClick(Sender: TObject);  
begin
  FBuffer.Canvas.Brush.Color := clWhite;
  FBuffer.Canvas.FillRect(Rect(0, 0, FBuffer.Width, FBuffer.Height));
  PaintBox1.Invalidate;
end;

end.
```

**Ce que fait cet exemple** :
- Dessine à la souris avec une couleur et épaisseur personnalisables
- Utilise le double buffering pour éviter les scintillements
- Permet d'effacer le dessin
- Fonctionne identiquement sur Windows et Ubuntu

---

## Ressources et bonnes pratiques

### Bonnes pratiques

1. ✅ **Toujours dessiner dans OnPaint** (sauf cas très spécifiques)
2. ✅ **Utiliser le double buffering** pour animations et dessins complexes
3. ✅ **Libérer les ressources** (Free sur les TBitmap, TPicture, etc.)
4. ✅ **Tester sur les deux plateformes** (Windows et Linux) régulièrement
5. ✅ **Optimiser** : ne redessiner que ce qui est nécessaire

### Erreurs courantes

1. ❌ Dessiner en dehors de OnPaint sans rafraîchissement
2. ❌ Oublier de libérer les TBitmap et autres objets graphiques
3. ❌ Ne pas activer DoubleBuffered pour des animations
4. ❌ Créer des objets dans OnPaint (ralentissements)
5. ❌ Supposer que le rendu sera identique sur toutes les plateformes sans tester

---

## Conclusion

Le **custom drawing** avec le Canvas est une compétence fondamentale pour créer des interfaces riches et personnalisées. Vous avez maintenant les bases pour :

- Comprendre le fonctionnement du Canvas et de OnPaint
- Dessiner formes, lignes, textes et images
- Optimiser vos dessins avec le double buffering
- Gérer les différences entre Windows et Ubuntu
- Créer des applications graphiques portables

La prochaine section abordera **BGRABitmap** pour aller encore plus loin avec des graphiques avancés, notamment l'anti-aliasing complet, les dégradés, les effets de transparence, et bien plus encore !

---

**Points clés à retenir** :
- Le Canvas est votre outil principal pour dessiner
- OnPaint est l'événement clé (toujours dessiner dedans)
- Le double buffering évite les scintillements
- Le code Canvas est largement portable Windows/Linux
- Optimisez en réutilisant les objets et en utilisant ClipRect

⏭️ [BGRABitmap pour graphiques avancés](/12-interfaces-graphiques-avancees/02-bgrabitmap-graphiques-avances.md)
