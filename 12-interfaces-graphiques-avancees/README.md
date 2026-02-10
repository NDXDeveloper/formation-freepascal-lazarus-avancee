🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 12. Interfaces Graphiques Avancées

## Introduction

Bienvenue dans ce chapitre consacré aux **interfaces graphiques avancées** avec FreePascal et Lazarus. Après avoir maîtrisé les bases de la LCL (Lazarus Component Library) et le développement d'applications multi-plateformes, il est temps d'explorer les techniques qui vous permettront de créer des interfaces visuellement riches, performantes et professionnelles.

---

## Pourquoi des interfaces graphiques avancées ?

### Au-delà des composants standard

Les composants standard de Lazarus (boutons, labels, grilles, etc.) sont parfaits pour des applications classiques, mais ils montrent leurs limites lorsque vous souhaitez :

- **Créer des visualisations de données sophistiquées** (graphiques, diagrammes, tableaux de bord)
- **Développer des éditeurs graphiques** (dessin, image, CAO)
- **Construire des jeux 2D ou 3D**
- **Proposer des interfaces modernes** avec animations et effets visuels
- **Optimiser les performances** pour des applications gourmandes en ressources graphiques
- **Garantir un rendu identique** sur toutes les plateformes (Windows, Ubuntu, macOS)

### Les défis du graphisme avancé

Le développement d'interfaces graphiques avancées présente plusieurs défis :

1. **Performance** : Dessiner des milliers d'objets à l'écran sans ralentissements
2. **Qualité visuelle** : Anti-aliasing, transparence, effets visuels
3. **Portabilité** : Rendu cohérent sur Windows, Linux et autres OS
4. **Interactivité** : Gestion fluide des interactions utilisateur
5. **Accessibilité** : Support haute résolution (HiDPI/4K), thèmes système
6. **Maintenabilité** : Code organisé et réutilisable

---

## Vue d'ensemble du chapitre

Ce chapitre couvre l'ensemble des technologies et techniques pour créer des interfaces graphiques de niveau professionnel :

### 12.1 Custom Drawing et Canvas Avancé
Les fondations du dessin personnalisé avec le Canvas standard de la LCL. Vous apprendrez à :
- Maîtriser les outils de dessin de base (Pen, Brush, Font)
- Dessiner des formes, lignes, textes et images
- Optimiser avec le double buffering
- Gérer les différences Windows/Linux

### 12.2 BGRABitmap pour graphiques avancés
Une bibliothèque puissante qui étend les capacités du Canvas avec :
- Anti-aliasing complet sur tous les dessins
- Transparence alpha totale (canal 8 bits)
- Dégradés avancés (linéaires, radiaux, multi-couleurs)
- Effets visuels (flou, ombre, lueur, transformations)
- Rendu identique sur toutes les plateformes

### 12.3 OpenGL multi-plateforme
L'accélération GPU pour des performances maximales :
- Configuration OpenGL sur Windows et Linux
- Rendu 3D et 2D accéléré
- Shaders et effets avancés
- Intégration avec Lazarus

### 12.4 Vulkan et rendu moderne
Les technologies de nouvelle génération :
- API Vulkan pour un contrôle total du GPU
- Performance maximale et multi-threading
- Portabilité Windows/Linux/macOS

### 12.5 Animations et transitions
Donner vie aux interfaces :
- Interpolations et courbes d'animation
- Transitions fluides entre états
- Animations procédurales
- Optimisation des performances

### 12.6 Docking et interfaces modulaires
Interfaces professionnelles personnalisables :
- Fenêtres ancrables (comme Visual Studio, Photoshop)
- Sauvegarde/restauration des dispositions
- Interfaces MDI et SDI

### 12.7 Éditeurs et designers intégrés
Créer des outils d'édition dans vos applications :
- Éditeurs de texte avec coloration syntaxique
- Designers visuels
- Éditeurs de propriétés personnalisés

### 12.8 Graphiques vectoriels SVG
Images vectorielles évolutives :
- Chargement et affichage de SVG
- Manipulation programmatique
- Export vers SVG

### 12.9 Traitement d'images avancé
Manipulation d'images bitmap :
- Filtres et effets
- Transformations géométriques
- Détection et analyse

### 12.10 Vision par ordinateur avec OpenCV
Intelligence artificielle pour la vision :
- Détection d'objets et de visages
- Reconnaissance de formes
- Traitement vidéo en temps réel

### 12.11 Accélération GPU (CUDA/OpenCL)
Calcul haute performance sur GPU :
- CUDA pour cartes NVIDIA
- OpenCL pour compatibilité multi-vendeurs
- Applications scientifiques et calcul parallèle

---

## Technologies et bibliothèques

### Bibliothèques graphiques principales

| Bibliothèque | Usage | Avantages | Plateformes |
|--------------|-------|-----------|-------------|
| **LCL Canvas** | Dessin de base | Intégré, simple | Windows, Linux, macOS |
| **BGRABitmap** | Graphiques 2D avancés | Anti-aliasing, effets, portable | Toutes |
| **LazOpenGL** | 3D accéléré | Performance GPU | Windows, Linux, macOS |
| **fpImage** | Manipulation d'images | Intégré FPC | Toutes |
| **Cairo** | Graphiques vectoriels | Qualité professionnelle | Linux (natif), Windows |
| **AggPas** | Rendu 2D haute qualité | Anti-aliasing avancé | Toutes |

### Widgetsets et rendu natif

Lazarus utilise différents **widgetsets** selon la plateforme pour un rendu natif :

#### Windows
- **Win32/Win64** : API Windows native (GDI/GDI+)
- Apparence système automatique
- Performance optimale

#### Linux/Ubuntu
- **GTK2** : Interface GNOME classique
- **GTK3** : Interface GNOME moderne (Cairo)
- **Qt5** : Interface KDE/Qt (rendu alternatif)
- **Custom Drawn** : Rendu personnalisé indépendant

#### Tableau comparatif

| Widgetset | Windows | Linux | Rendu | Performance |
|-----------|---------|-------|-------|-------------|
| Win32/64 | ✅ Natif | ❌ | GDI | Excellente |
| GTK2 | ⚠️ Émulation | ✅ Natif | X11 | Bonne |
| GTK3 | ⚠️ Émulation | ✅ Natif | Cairo | Bonne |
| Qt5 | ✅ | ✅ | Qt | Excellente |
| Custom Drawn | ✅ | ✅ | Personnalisé | Variable |

---

## Concepts fondamentaux

### Le modèle de dessin

Comprendre comment fonctionne le dessin graphique est essentiel :

```
┌─────────────────────────────────────┐
│   Application Lazarus               │
├─────────────────────────────────────┤
│   LCL (Abstraction)                 │
│   - TCanvas                         │
│   - TBitmap                         │
│   - TPicture                        │
├─────────────────────────────────────┤
│   Widgetset (Windows/GTK/Qt)        │
│   - Appels système                  │
├─────────────────────────────────────┤
│   API Système                       │
│   Windows: GDI/GDI+                 │
│   Linux: X11/Cairo/Wayland          │
├─────────────────────────────────────┤
│   Pilotes graphiques                │
│   - CPU (rendu logiciel)            │
│   - GPU (accélération matérielle)   │
└─────────────────────────────────────┘
```

### Événements de dessin

Le cycle de vie du dessin dans une application Lazarus :

```pascal
1. Système déclenche besoin de redessin
   ↓
2. Message WM_PAINT (Windows) ou Expose (X11)
   ↓
3. LCL appelle OnPaint du composant
   ↓
4. Votre code dessine sur Canvas
   ↓
5. Canvas traduit vers API système
   ↓
6. Affichage à l'écran
```

**Événement OnPaint** : C'est le point d'entrée principal pour tout dessin personnalisé.

```pascal
procedure TForm1.FormPaint(Sender: TObject);  
begin
  // Votre code de dessin ici
  Canvas.TextOut(10, 10, 'Hello World');
end;
```

### Coordonnées et systèmes de référence

#### Coordonnées écran
- Origine (0,0) en haut à gauche
- X augmente vers la droite
- Y augmente vers le bas

```
(0,0) ────────────► X
  │
  │
  │
  ▼
  Y
```

#### Coordonnées client
Relatives au composant (formulaire, panel, etc.), pas à l'écran entier.

```pascal
var
  ScreenPos, ClientPos: TPoint;
begin
  // Position écran
  ScreenPos := Mouse.CursorPos;

  // Conversion vers coordonnées client
  ClientPos := ScreenToClient(ScreenPos);
end;
```

---

## Stratégies de rendu

### 1. Rendu immédiat (Immediate Mode)

Dessiner directement à chaque frame :

```pascal
procedure TForm1.PaintBox1Paint(Sender: TObject);  
begin
  // Effacer le fond (TCanvas n'a pas de méthode Clear)
  Canvas.Brush.Color := clWhite;
  Canvas.FillRect(Canvas.ClipRect);
  // Redessiner tout à chaque fois
  Canvas.Ellipse(10, 10, 100, 100);
  Canvas.Rectangle(120, 10, 210, 100);
end;
```

**Avantages** : Simple, flexible  
**Inconvénients** : Peut être lent si beaucoup d'objets  

### 2. Rendu retenu (Retained Mode)

Maintenir une structure de données des objets graphiques :

```pascal
{ Note : TObjectList<T> nécessite Generics.Collections et {$mode delphi}
  ou bien specialize TObjectList<TGraphicObject> en {$mode objfpc}. }

type
  TGraphicObject = class
    procedure Draw(ACanvas: TCanvas); virtual; abstract;
  end;

var
  Objects: TObjectList<TGraphicObject>;

procedure TForm1.PaintBox1Paint(Sender: TObject);  
var
  Obj: TGraphicObject;
begin
  for Obj in Objects do
    Obj.Draw(Canvas);
end;
```

**Avantages** : Organisé, optimisable  
**Inconvénients** : Plus complexe  

### 3. Double Buffering

Dessiner hors écran puis copier d'un coup :

```pascal
var
  Buffer: TBitmap;

procedure TForm1.PaintBox1Paint(Sender: TObject);  
begin
  if not Assigned(Buffer) then
    Buffer := TBitmap.Create;

  Buffer.SetSize(Width, Height);

  // Dessiner sur le buffer
  Buffer.Canvas.Brush.Color := clWhite;
  Buffer.Canvas.FillRect(Rect(0, 0, Width, Height));
  Buffer.Canvas.Ellipse(10, 10, 100, 100);

  // Copier vers l'écran
  Canvas.Draw(0, 0, Buffer);
end;
```

**Avantages** : Pas de scintillement  
**Inconvénients** : Utilise plus de mémoire  

---

## Performances graphiques

### Facteurs de performance

1. **Nombre d'objets dessinés** : Plus vous dessinez, plus c'est lent
2. **Complexité des opérations** : Anti-aliasing et effets coûtent cher
3. **Redessins inutiles** : Ne redessiner que ce qui a changé
4. **Type de rendu** : CPU vs GPU
5. **Résolution** : HiDPI/4K nécessite 4× plus de pixels

### Techniques d'optimisation

#### 1. ClipRect - Dessiner uniquement la zone visible

```pascal
procedure TForm1.Paint;  
var
  ClipR: TRect;
begin
  ClipR := Canvas.ClipRect;

  // Ne dessiner que dans ClipR
  if RectIntersects(ClipR, MyObjectRect) then
    DrawMyObject(Canvas);
end;
```

#### 2. Dirty Rectangles - Marquer les zones modifiées

```pascal
type
  TMyControl = class(TCustomControl)
  private
    FDirtyRegion: TRect;
  public
    procedure InvalidateRect(const R: TRect);
  end;

procedure TMyControl.InvalidateRect(const R: TRect);  
begin
  FDirtyRegion := R;
  Invalidate;  // Demander un redessin
end;
```

#### 3. Caching - Mémoriser les résultats

```pascal
var
  CachedBitmap: TBitmap;
  NeedRedraw: Boolean;

procedure TForm1.Paint;  
begin
  if NeedRedraw then
  begin
    // Redessiner dans le cache
    RenderToCache(CachedBitmap);
    NeedRedraw := False;
  end;

  // Copier depuis le cache (rapide)
  Canvas.Draw(0, 0, CachedBitmap);
end;
```

#### 4. Culling - Ne pas traiter les objets hors écran

```pascal
procedure DrawVisibleObjects;  
var
  Obj: TGraphicObject;
begin
  for Obj in AllObjects do
  begin
    // Ignorer si hors écran
    if not Obj.IsVisible(ViewportRect) then
      Continue;

    Obj.Draw(Canvas);
  end;
end;
```

---

## Multi-plateforme : spécificités

### Windows

#### Points forts
- Performance GDI/GDI+ excellente
- Support DirectX natif
- Polices TrueType/ClearType

#### Points d'attention
```pascal
{$IFDEF WINDOWS}
// Code spécifique Windows
Canvas.Font.Quality := fqClearType;
{$ENDIF}
```

### Linux/Ubuntu

#### Points forts
- Support GTK/Qt mature
- Cairo pour rendu de qualité
- X11 et Wayland

#### Points d'attention
```pascal
{$IFDEF LINUX}
// Configuration spécifique Linux
// GTK nécessite parfois des appels manuels
{$ENDIF}
```

### Code véritablement portable

```pascal
// Ce code fonctionne identiquement partout
procedure DrawCrossPlatform(ACanvas: TCanvas);  
begin
  ACanvas.Pen.Color := clBlue;
  ACanvas.Pen.Width := 2;
  ACanvas.MoveTo(0, 0);
  ACanvas.LineTo(100, 100);
  ACanvas.Ellipse(50, 50, 150, 150);
end;
```

---

## HiDPI et haute résolution

### Le problème du HiDPI

Les écrans modernes (4K, 5K, Retina) ont une densité de pixels élevée :
- **96 DPI** : Écran standard classique
- **144 DPI** : Écran haute résolution (1.5×)
- **192 DPI** : Écran très haute résolution (2×)
- **288 DPI** : Retina/4K (3×)

### Support dans Lazarus

```pascal
// Activer le support HiDPI
{$IFDEF WINDOWS}
Application.Scaled := True;
{$ENDIF}

// Détecter le facteur d'échelle
var
  ScaleFactor: Double;
begin
  ScaleFactor := Screen.PixelsPerInch / 96;  // 96 = DPI standard

  // Ajuster les dimensions
  MyWidth := Round(100 * ScaleFactor);
  MyHeight := Round(50 * ScaleFactor);
end;
```

### Ressources adaptatives

Fournir plusieurs résolutions d'images :

```
images/
  icon@1x.png   (32×32)
  icon@2x.png   (64×64)
  icon@3x.png   (96×96)
```

```pascal
function LoadScaledImage(const ABaseName: string): TPicture;  
var
  ScaleFactor: Integer;
  FileName: string;
begin
  Result := TPicture.Create;
  ScaleFactor := Round(Screen.PixelsPerInch / 96);
  FileName := Format('%s@%dx.png', [ABaseName, ScaleFactor]);

  if FileExists(FileName) then
    Result.LoadFromFile(FileName)
  else
    Result.LoadFromFile(ABaseName + '.png');
end;
```

---

## Accessibilité

### Support des thèmes système

```pascal
// Utiliser les couleurs système
Canvas.Brush.Color := clWindow;        // Fond de fenêtre  
Canvas.Font.Color := clWindowText;     // Texte de fenêtre  
Canvas.Pen.Color := clBtnFace;         // Face de bouton
```

### Contraste et lisibilité

```pascal
// Vérifier le contraste
function HasGoodContrast(ForeColor, BackColor: TColor): Boolean;  
var
  ForeRGB, BackRGB: TColorRGB;
  Luminance1, Luminance2: Double;
  Ratio: Double;
begin
  ForeRGB := ColorToRGB(ForeColor);
  BackRGB := ColorToRGB(BackColor);

  // Calculer la luminance relative
  Luminance1 := CalculateLuminance(ForeRGB);
  Luminance2 := CalculateLuminance(BackRGB);

  // Ratio de contraste (WCAG recommande ≥ 4.5:1)
  if Luminance1 > Luminance2 then
    Ratio := (Luminance1 + 0.05) / (Luminance2 + 0.05)
  else
    Ratio := (Luminance2 + 0.05) / (Luminance1 + 0.05);

  Result := Ratio >= 4.5;
end;
```

---

## Architecture pour interfaces complexes

### Pattern MVC/MVP pour graphiques

```pascal
// Model : Données
type
  TDoubleArray = array of Double;

  TDataModel = class
  private
    FData: TDoubleArray;
  public
    property Data: TDoubleArray read FData;
  end;

// View : Affichage
type
  TChartView = class(TCustomControl)
  private
    FModel: TDataModel;
  protected
    procedure Paint; override;
  public
    property Model: TDataModel read FModel write FModel;
  end;

// Presenter/Controller : Logique
type
  TChartController = class
  private
    FModel: TDataModel;
    FView: TChartView;
  public
    procedure UpdateData(const NewData: array of Double);
  end;
```

### Composants personnalisés réutilisables

```pascal
type
  TCustomChart = class(TGraphicControl)
  private
    FData: TChartData;
    FColors: TChartColors;
  protected
    procedure Paint; override;
  published
    property Data: TChartData read FData write FData;
    property Colors: TChartColors read FColors write FColors;
  end;

// Enregistrement pour utilisation dans l'IDE
procedure Register;  
begin
  RegisterComponents('MyComponents', [TCustomChart]);
end;
```

---

## Outils et debugging

### Mesurer les performances

```pascal
uses
  DateUtils;

procedure MeasureRenderTime;  
var
  StartTime, EndTime: TDateTime;
  Duration: Int64;
begin
  StartTime := Now;

  // Opération à mesurer
  RenderComplexScene;

  EndTime := Now;
  Duration := MilliSecondsBetween(EndTime, StartTime);

  WriteLn(Format('Rendu en %d ms (%.1f FPS)',
    [Duration, 1000 / Duration]));
end;
```

### Profiling graphique

```pascal
// Compter les appels de dessin
var
  DrawCallCount: Integer;

procedure IncrementDrawCalls;  
begin
  Inc(DrawCallCount);
end;

procedure ResetCounters;  
begin
  DrawCallCount := 0;
end;

// Dans OnPaint
procedure TForm1.Paint;  
begin
  ResetCounters;

  // Dessiner...

  Caption := Format('Draw calls: %d', [DrawCallCount]);
end;
```

---

## Ressources et apprentissage

### Documentation officielle

- **Lazarus Wiki** : https://wiki.lazarus.freepascal.org/
- **LCL Graphics** : https://wiki.lazarus.freepascal.org/Graphics
- **FreePascal RTL** : https://www.freepascal.org/docs.html

### Forums et communauté

- **Forum Lazarus** : https://forum.lazarus.freepascal.org/
- **Reddit r/lazarus** : https://reddit.com/r/lazarus
- **Stack Overflow** : Tag `lazarus` ou `freepascal`

### Bibliothèques tierces

- **BGRABitmap** : http://bgrabitmap.github.io/
- **TAChart** : Graphiques intégrés à Lazarus
- **LazOpenGL** : OpenGL pour Lazarus
- **mORMot** : Framework complet avec graphiques

---

## Prérequis pour ce chapitre

Avant de commencer, assurez-vous de maîtriser :

✅ **Bases de Lazarus** : Création de formulaires, composants standard  
✅ **Object Pascal** : Classes, héritage, événements  
✅ **Programmation événementielle** : Gestion des événements souris/clavier  
✅ **Bases du dessin** : Comprendre les pixels, couleurs, coordonnées  
✅ **Multi-plateforme** : Compilation conditionnelle, différences OS

---

## Structure des sections suivantes

Chaque section de ce chapitre suivra cette structure pédagogique :

1. **Introduction** : Présentation de la technologie
2. **Concepts fondamentaux** : Théorie et principes
3. **Exemples simples** : Code de base pour démarrer
4. **Techniques avancées** : Aller plus loin
5. **Optimisations** : Performances et bonnes pratiques
6. **Multi-plateforme** : Spécificités Windows/Linux
7. **Projets pratiques** : Applications complètes
8. **Ressources** : Documentation et liens utiles

---

## Progression recommandée

Pour tirer le meilleur parti de ce chapitre, suivez cet ordre :

```
1. Canvas avancé (12.1)        ← Fondations
   ↓
2. BGRABitmap (12.2)           ← Graphiques 2D de qualité
   ↓
3. Animations (12.5)            ← Donner vie aux interfaces
   ↓
4. OpenGL (12.3)               ← Accélération GPU
   ↓
5. Autres sections             ← Selon vos besoins
```

**Débutants** : Commencez par 12.1 et 12.2, puis explorez selon vos projets  
**Intermédiaires** : Concentrez-vous sur 12.2, 12.5 et 12.6  
**Avancés** : OpenGL (12.3), Vulkan (12.4), OpenCV (12.10)  

---

## Objectifs d'apprentissage

À la fin de ce chapitre, vous serez capable de :

✅ Créer des interfaces graphiques personnalisées et professionnelles  
✅ Implémenter des visualisations de données complexes  
✅ Optimiser les performances graphiques de vos applications  
✅ Développer des applications portables Windows/Linux avec rendu identique  
✅ Intégrer des bibliothèques graphiques tierces (BGRABitmap, OpenGL)  
✅ Créer des animations fluides et des transitions élégantes  
✅ Construire des éditeurs et designers intégrés  
✅ Manipuler des images et appliquer des effets avancés  
✅ Tirer parti de l'accélération GPU pour les performances maximales

---

## Avertissements et bonnes pratiques

### ⚠️ Attention aux performances

Les opérations graphiques peuvent être coûteuses. Toujours :
- Profiler avant d'optimiser
- Utiliser le double buffering pour éviter les scintillements
- Minimiser les redesinages (utiliser `Invalidate` judicieusement)
- Tester sur du matériel de différentes capacités

### ⚠️ Gestion de la mémoire

Les objets graphiques (TBitmap, TBGRABitmap, etc.) consomment beaucoup de mémoire :
- Toujours libérer avec `Free` ou `FreeAndNil`
- Utiliser des try-finally pour garantir la libération
- Surveiller l'utilisation mémoire avec des outils de profiling

### ⚠️ Portabilité

Testez régulièrement sur toutes vos plateformes cibles :
- Windows et Linux peuvent avoir des comportements différents
- Les polices et rendus peuvent varier
- Utilisez des chemins et séparateurs portables

### ✅ Bonnes pratiques générales

1. **Séparer logique et affichage** : Architecture MVC/MVP
2. **Composants réutilisables** : Créer des classes pour code commun
3. **Documentation** : Commenter les algorithmes complexes
4. **Tests** : Tester sur différentes résolutions et configurations
5. **Accessibilité** : Toujours penser aux utilisateurs avec besoins spécifiques

---

## Prêt à commencer ?

Vous avez maintenant une vue d'ensemble complète de ce qui vous attend dans ce chapitre sur les **Interfaces Graphiques Avancées**. Les sections suivantes vont approfondir chaque technologie avec :

- 📚 Explications détaillées accessibles aux débutants
- 💻 Exemples de code complets et testés
- 🎯 Projets pratiques pour mettre en application
- ⚡ Techniques d'optimisation pour les performances
- 🌐 Support multi-plateforme Windows/Linux

Commençons par les fondations avec le **Custom Drawing et Canvas Avancé** dans la section suivante !

---

**Note importante** : Ce chapitre suppose que vous avez déjà une connaissance de base de Lazarus et FreePascal. Si certains concepts vous semblent flous, n'hésitez pas à consulter les chapitres précédents du cours, notamment :
- Chapitre 3 : Langage Object Pascal Avancé
- Chapitre 4 : Framework LCL
- Chapitre 5 : Développement Multi-plateforme

Bonne exploration des interfaces graphiques avancées ! 🚀

⏭️ [Custom drawing et Canvas avancé](/12-interfaces-graphiques-avancees/01-custom-drawing-canvas-avance.md)
