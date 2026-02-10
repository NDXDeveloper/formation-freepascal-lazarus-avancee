🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 16.2 TAChart pour visualisations

## Introduction

TAChart (The Advanced Chart) est la bibliothèque de graphiques intégrée à Lazarus. Elle permet de créer facilement des visualisations de données professionnelles et interactives. TAChart est totalement gratuit, open source et fonctionne de manière identique sur Windows, Ubuntu et macOS.

## Qu'est-ce que TAChart ?

TAChart est un composant visuel qui transforme vos données en graphiques élégants :

- **Courbes et graphiques** : Lignes, barres, points, aires, camemberts, etc.
- **Interactif** : Zoom, déplacement, info-bulles
- **Personnalisable** : Couleurs, styles, légendes, axes
- **Performant** : Capable d'afficher des milliers de points
- **Multi-plateforme** : Fonctionne sur Windows, Linux, macOS

## Installation de TAChart

### TAChart est déjà inclus !

Bonne nouvelle : TAChart est installé par défaut avec Lazarus. Vous n'avez rien à télécharger ou configurer.

### Vérification de l'installation

1. Ouvrez Lazarus
2. Regardez la palette de composants
3. Cherchez l'onglet **"Chart"**
4. Vous devriez voir les composants : `TChart`, `TChartSeries`, etc.

Si vous ne voyez pas l'onglet Chart :
- Menu `Package` → `Install/Uninstall Packages`
- Dans la liste de droite, cherchez `TAChartLazarusPkg`
- S'il est dans la liste de gauche, déplacez-le à droite
- Cliquez sur `Save and rebuild IDE`

## Structure de TAChart

### Composants principaux

| Composant | Description |
|-----------|-------------|
| `TChart` | Le conteneur principal qui affiche les graphiques |
| `TLineSeries` | Série de données pour graphiques en ligne |
| `TBarSeries` | Série de données pour graphiques en barres |
| `TAreaSeries` | Série de données pour graphiques en aires |
| `TPieSeries` | Série de données pour diagrammes circulaires |
| `TPointSeries` | Série de données pour nuages de points |
| `TListChartSource` | Source de données manuelle |
| `TChartLegend` | Légende du graphique |
| `TChartAxisTransformations` | Transformations d'axes (log, etc.) |

### Architecture de base

```
TChart (conteneur)
  ├── Series (séries de données)
  │   ├── TLineSeries
  │   ├── TBarSeries
  │   └── ...
  ├── ChartSource (sources de données)
  │   ├── TListChartSource
  │   └── ...
  └── Axes (axes X et Y)
      ├── BottomAxis
      ├── LeftAxis
      └── ...
```

## Premier graphique : étape par étape

### Création d'un projet simple

1. **Créer un nouveau projet** :
   - `Fichier` → `Nouveau` → `Application`
   - Sauvegardez le projet (ex: "MonPremierGraphique")

2. **Ajouter un TChart** :
   - Dans la palette, onglet `Chart`
   - Cliquez sur `TChart`
   - Cliquez sur votre formulaire
   - Redimensionnez le composant pour qu'il occupe une bonne partie du formulaire

3. **Ajouter une série** :
   - Double-cliquez sur le `TChart`
   - Ou clic droit → `Edit Series`
   - Cliquez sur `Add` → Choisissez `TLineSeries`
   - Cliquez sur `OK`

4. **Ajouter des données** :
   - Sélectionnez votre série dans l'éditeur
   - Onglet `Source`
   - Clic droit dans la liste → `Add point`
   - Ajoutez plusieurs points (X, Y)
   - Exemple : (0, 0), (1, 2), (2, 4), (3, 6), (4, 8)

5. **Exécuter** :
   - Appuyez sur `F9`
   - Admirez votre premier graphique !

### Code correspondant

Voici ce que Lazarus a généré automatiquement :

```pascal
unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, TAGraph, TASeries;

type
  TForm1 = class(TForm)
    Chart1: TChart;
    LineSeries1: TLineSeries;
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

end.
```

## Ajouter des données par code

### Méthode 1 : Ajout direct à la série

```pascal
procedure TForm1.FormCreate(Sender: TObject);  
var
  i: Integer;
begin
  // Effacer les données existantes
  LineSeries1.Clear;

  // Ajouter des points un par un
  LineSeries1.AddXY(0, 0);
  LineSeries1.AddXY(1, 1);
  LineSeries1.AddXY(2, 4);
  LineSeries1.AddXY(3, 9);
  LineSeries1.AddXY(4, 16);

  // Ou en boucle
  for i := 0 to 10 do
    LineSeries1.AddXY(i, i * i); // y = x²
end;
```

### Méthode 2 : Utilisation d'un ChartSource

```pascal
uses
  TAGraph, TASeries, TAChartListbox;

procedure TForm1.FormCreate(Sender: TObject);  
var
  Source: TListChartSource;
  i: Integer;
begin
  // Créer une source de données
  Source := TListChartSource.Create(Self);

  // Remplir la source
  for i := 0 to 20 do
    Source.Add(i, Sin(i * 0.5)); // Fonction sinus

  // Lier la source à la série
  LineSeries1.Source := Source;
end;
```

### Méthode 3 : Depuis un tableau

```pascal
procedure TForm1.ChargerDonnees;  
var
  valeurs: array of Double;
  i: Integer;
begin
  // Préparer les données
  SetLength(valeurs, 100);
  for i := 0 to 99 do
    valeurs[i] := Random(100); // Valeurs aléatoires

  // Remplir le graphique
  LineSeries1.Clear;
  for i := 0 to High(valeurs) do
    LineSeries1.AddXY(i, valeurs[i]);
end;
```

## Types de graphiques

### Graphique en ligne (TLineSeries)

Le plus courant, idéal pour montrer des tendances.

```pascal
procedure TForm1.CreerGraphiqueLigne;  
var
  Serie: TLineSeries;
  i: Integer;
begin
  Serie := TLineSeries.Create(Chart1);
  Serie.Title := 'Température';

  // Style de la ligne
  Serie.LinePen.Color := clRed;
  Serie.LinePen.Width := 2;
  Serie.LinePen.Style := psSolid;

  // Marqueurs de points
  Serie.ShowPoints := True;
  Serie.Pointer.Style := psCircle;
  Serie.Pointer.Brush.Color := clRed;

  // Données
  for i := 0 to 12 do
    Serie.AddXY(i, 15 + Random(15)); // Températures aléatoires

  Chart1.AddSeries(Serie);
end;
```

### Graphique en barres (TBarSeries)

Parfait pour comparer des catégories.

```pascal
procedure TForm1.CreerGraphiqueBarres;  
var
  Serie: TBarSeries;
begin
  Serie := TBarSeries.Create(Chart1);
  Serie.Title := 'Ventes par mois';

  // Couleurs des barres
  Serie.BarBrush.Color := clSkyBlue;
  Serie.BarPen.Color := clBlue;

  // Largeur des barres
  Serie.BarWidthPercent := 70;

  // Données
  Serie.AddXY(1, 150, 'Janvier');
  Serie.AddXY(2, 180, 'Février');
  Serie.AddXY(3, 220, 'Mars');
  Serie.AddXY(4, 190, 'Avril');
  Serie.AddXY(5, 240, 'Mai');

  Chart1.AddSeries(Serie);
end;
```

### Graphique en aires (TAreaSeries)

Pour montrer des volumes ou des proportions.

```pascal
procedure TForm1.CreerGraphiqueAire;  
var
  Serie: TAreaSeries;
  i: Integer;
begin
  Serie := TAreaSeries.Create(Chart1);
  Serie.Title := 'Production';

  // Couleur de remplissage
  Serie.AreaBrush.Color := clMoneyGreen;
  Serie.AreaBrush.Style := bsSolid;

  // Ligne de contour
  Serie.AreaLinesPen.Color := clGreen;
  Serie.AreaLinesPen.Width := 2;

  // Données
  for i := 0 to 10 do
    Serie.AddXY(i, 50 + Random(50));

  Chart1.AddSeries(Serie);
end;
```

### Diagramme circulaire (TPieSeries)

Pour montrer des parts d'un tout.

```pascal
procedure TForm1.CreerDiagrammeCirculaire;  
var
  Serie: TPieSeries;
begin
  Serie := TPieSeries.Create(Chart1);
  Serie.Title := 'Répartition des ventes';

  // Explosion d'une part (optionnel)
  Serie.Exploded := True;
  Serie.ExplodedPercentage := 10;

  // Données (valeur, étiquette)
  Serie.AddPie(350, 'Produit A', clRed);
  Serie.AddPie(280, 'Produit B', clBlue);
  Serie.AddPie(420, 'Produit C', clGreen);
  Serie.AddPie(190, 'Produit D', clYellow);

  Chart1.AddSeries(Serie);
end;
```

### Nuage de points (TPointSeries)

Pour visualiser la corrélation entre deux variables.

```pascal
procedure TForm1.CreerNuagePoints;  
var
  Serie: TPointSeries;
  i: Integer;
begin
  Serie := TPointSeries.Create(Chart1);
  Serie.Title := 'Corrélation';

  // Style des points
  Serie.Pointer.Style := psCross;
  Serie.Pointer.Brush.Color := clPurple;
  Serie.Pointer.Pen.Color := clPurple;

  // Données aléatoires avec corrélation
  for i := 0 to 50 do
    Serie.AddXY(i + Random(10), i * 2 + Random(20));

  Chart1.AddSeries(Serie);
end;
```

## Personnalisation du graphique

### Titres et légendes

```pascal
procedure TForm1.PersonnaliserTitres;  
begin
  // Titre principal
  Chart1.Title.Text.Text := 'Évolution des ventes 2024';
  Chart1.Title.Font.Size := 14;
  Chart1.Title.Font.Style := [fsBold];
  Chart1.Title.Visible := True;

  // Pied de page
  Chart1.Foot.Text.Text := 'Source : Service commercial';
  Chart1.Foot.Font.Size := 8;
  Chart1.Foot.Visible := True;

  // Légende
  Chart1.Legend.Visible := True;
  Chart1.Legend.Alignment := laBottomCenter;
  Chart1.Legend.ColumnCount := 2;
  Chart1.Legend.Frame.Visible := True;
end;
```

### Configuration des axes

```pascal
procedure TForm1.ConfigurerAxes;  
begin
  // Axe horizontal (X)
  with Chart1.BottomAxis do
  begin
    Title.Caption := 'Mois';
    Title.Font.Style := [fsBold];
    Marks.LabelFont.Size := 9;
    Grid.Visible := True;
    Grid.Style := psDot;
  end;

  // Axe vertical (Y)
  with Chart1.LeftAxis do
  begin
    Title.Caption := 'Ventes (€)';
    Title.Font.Style := [fsBold];
    Marks.Format := '%.0f €'; // Format des valeurs
    Grid.Visible := True;
    Grid.Color := clSilver;
  end;

  // Limites manuelles (optionnel)
  Chart1.Extent.UseYMin := True;
  Chart1.Extent.YMin := 0;
  Chart1.Extent.UseYMax := True;
  Chart1.Extent.YMax := 500;
end;
```

### Couleurs et style

```pascal
procedure TForm1.PersonnaliserStyle;  
begin
  // Fond du graphique
  Chart1.BackColor := clWhite;
  Chart1.Color := clCream;

  // Cadre
  Chart1.Frame.Visible := True;
  Chart1.Frame.Color := clGray;
  Chart1.Frame.Width := 2;

  // Grille
  Chart1.LeftAxis.Grid.Color := clSilver;
  Chart1.LeftAxis.Grid.Style := psDot;
  Chart1.BottomAxis.Grid.Color := clSilver;
  Chart1.BottomAxis.Grid.Style := psDot;
end;
```

## Fonctionnalités interactives

### Zoom et navigation

```pascal
uses
  TATools;

procedure TForm1.ActiverZoom;  
var
  ZoomTool: TChartToolset;
  ZoomDrag: TZoomDragTool;
  PanDrag: TPanDragTool;
begin
  // Créer le toolset
  ZoomTool := TChartToolset.Create(Chart1);
  Chart1.Toolset := ZoomTool;

  // Outil de zoom par glisser-déposer
  ZoomDrag := TZoomDragTool.Create(Chart1);
  ZoomDrag.Shift := [ssLeft]; // Bouton gauche
  ZoomTool.Tools.Add(ZoomDrag);

  // Outil de déplacement
  PanDrag := TPanDragTool.Create(Chart1);
  PanDrag.Shift := [ssRight]; // Bouton droit
  ZoomTool.Tools.Add(PanDrag);
end;
```

### Info-bulles personnalisées

```pascal
uses
  TATools;

procedure TForm1.ActiverInfoBulles;  
var
  DataTool: TDataPointHintTool;
begin
  // Créer l'outil d'info-bulles
  if Chart1.Toolset = nil then
    Chart1.Toolset := TChartToolset.Create(Chart1);

  DataTool := TDataPointHintTool.Create(Chart1);
  DataTool.UseDefaultHintText := False;
  DataTool.OnHint := @MonInfoBulle;
  Chart1.Toolset.Tools.Add(DataTool);
end;

procedure TForm1.MonInfoBulle(ATool: TDataPointHintTool;
  const APoint: TPoint; var AHint: String);
var
  x, y: Double;
begin
  with ATool.PointData do
  begin
    x := Point.X;
    y := Point.Y;
    AHint := Format('X: %.2f'#13'Y: %.2f', [x, y]);
  end;
end;
```

### Sélection de points

```pascal
uses
  TATools;

procedure TForm1.ActiverSelection;  
var
  SelectTool: TDataPointClickTool;
begin
  if Chart1.Toolset = nil then
    Chart1.Toolset := TChartToolset.Create(Chart1);

  SelectTool := TDataPointClickTool.Create(Chart1);
  SelectTool.OnPointClick := @PointClique;
  Chart1.Toolset.Tools.Add(SelectTool);
end;

procedure TForm1.PointClique(ATool: TChartTool; APoint: TPoint);  
var
  x, y: Double;
begin
  with (ATool as TDataPointClickTool).PointData do
  begin
    x := Point.X;
    y := Point.Y;
    ShowMessage(Format('Point cliqué : (%.2f, %.2f)', [x, y]));
  end;
end;
```

## Graphiques multiples

### Plusieurs séries sur un graphique

```pascal
procedure TForm1.CreerGraphiqueMultiple;  
var
  Serie1, Serie2, Serie3: TLineSeries;
  i: Integer;
begin
  Chart1.Title.Text.Text := 'Comparaison de trois produits';

  // Première série
  Serie1 := TLineSeries.Create(Chart1);
  Serie1.Title := 'Produit A';
  Serie1.SeriesColor := clRed;
  Serie1.LinePen.Width := 2;
  for i := 0 to 12 do
    Serie1.AddXY(i, 100 + Random(50));
  Chart1.AddSeries(Serie1);

  // Deuxième série
  Serie2 := TLineSeries.Create(Chart1);
  Serie2.Title := 'Produit B';
  Serie2.SeriesColor := clBlue;
  Serie2.LinePen.Width := 2;
  for i := 0 to 12 do
    Serie2.AddXY(i, 80 + Random(40));
  Chart1.AddSeries(Serie2);

  // Troisième série
  Serie3 := TLineSeries.Create(Chart1);
  Serie3.Title := 'Produit C';
  Serie3.SeriesColor := clGreen;
  Serie3.LinePen.Width := 2;
  for i := 0 to 12 do
    Serie3.AddXY(i, 120 + Random(30));
  Chart1.AddSeries(Serie3);

  // Activer la légende
  Chart1.Legend.Visible := True;
end;
```

### Deux axes Y

```pascal
procedure TForm1.CreerGraphiqueDeuxAxes;  
var
  Serie1, Serie2: TLineSeries;
  i: Integer;
begin
  Chart1.Title.Text.Text := 'Ventes et bénéfices';

  // Série 1 : Axe gauche (ventes)
  Serie1 := TLineSeries.Create(Chart1);
  Serie1.Title := 'Ventes (€)';
  Serie1.AxisIndexY := 0; // Axe gauche
  Serie1.SeriesColor := clBlue;
  for i := 0 to 12 do
    Serie1.AddXY(i, 10000 + Random(5000));
  Chart1.AddSeries(Serie1);

  // Série 2 : Axe droit (bénéfices)
  Serie2 := TLineSeries.Create(Chart1);
  Serie2.Title := 'Bénéfices (€)';
  Serie2.AxisIndexY := 1; // Axe droit
  Serie2.SeriesColor := clGreen;
  for i := 0 to 12 do
    Serie2.AddXY(i, 1000 + Random(500));
  Chart1.AddSeries(Serie2);

  // Configuration de l'axe droit
  Chart1.AxisList[1].Visible := True;
  Chart1.AxisList[1].Title.Caption := 'Bénéfices (€)';
  Chart1.AxisList[1].Alignment := calRight;

  Chart1.Legend.Visible := True;
end;
```

## Exemple pratique complet

Voici un exemple d'application complète qui lit des données et les affiche.

```pascal
unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  TAGraph, TASeries, TATools;

type
  TForm1 = class(TForm)
    Chart1: TChart;
    Button1: TButton;
    Button2: TButton;
    ComboBox1: TComboBox;
    LineSeries1: TLineSeries;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
  private
    procedure ChargerDonnees;
    procedure ChangerTypeGraphique;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);  
begin
  // Configuration initiale
  Chart1.Title.Text.Text := 'Analyseur de données';
  Chart1.Legend.Visible := True;
  Chart1.LeftAxis.Title.Caption := 'Valeur';
  Chart1.BottomAxis.Title.Caption := 'Index';

  // Options de type de graphique
  ComboBox1.Items.Add('Ligne');
  ComboBox1.Items.Add('Barres');
  ComboBox1.Items.Add('Aires');
  ComboBox1.Items.Add('Points');
  ComboBox1.ItemIndex := 0;
end;

procedure TForm1.Button1Click(Sender: TObject);  
begin
  ChargerDonnees;
end;

procedure TForm1.Button2Click(Sender: TObject);  
begin
  Chart1.SaveToFile(TPortableNetworkGraphic, 'graphique.png');
  ShowMessage('Graphique sauvegardé : graphique.png');
end;

procedure TForm1.ComboBox1Change(Sender: TObject);  
begin
  ChangerTypeGraphique;
end;

procedure TForm1.ChargerDonnees;  
var
  i: Integer;
begin
  LineSeries1.Clear;

  // Génération de données simulées
  Randomize;
  for i := 0 to 20 do
  begin
    LineSeries1.AddXY(i, 50 + Random(100));
  end;

  Chart1.Title.Text.Text :=
    Format('Données chargées : %d points', [LineSeries1.Count]);
end;

procedure TForm1.ChangerTypeGraphique;  
var
  OldSeries: TBasicChartSeries;
  NewSeries: TBasicChartSeries;
  i: Integer;
begin
  // Sauvegarder les données
  OldSeries := Chart1.Series[0];

  // Créer la nouvelle série selon le type
  case ComboBox1.ItemIndex of
    0: NewSeries := TLineSeries.Create(Chart1);
    1: NewSeries := TBarSeries.Create(Chart1);
    2: NewSeries := TAreaSeries.Create(Chart1);
    3: NewSeries := TPointSeries.Create(Chart1);
    else
      Exit;
  end;

  // Copier les données
  for i := 0 to OldSeries.Count - 1 do
    with OldSeries[i]^ do
      NewSeries.AddXY(X, Y);

  // Remplacer la série
  Chart1.DeleteSeries(OldSeries);
  Chart1.AddSeries(NewSeries);
end;

end.
```

## Export et sauvegarde

### Exporter en image

```pascal
uses
  TADrawerCanvas;

procedure TForm1.ExporterEnPNG;  
begin
  Chart1.SaveToFile(TPortableNetworkGraphic, 'mon_graphique.png');
  ShowMessage('Graphique sauvegardé en PNG');
end;

procedure TForm1.ExporterEnJPEG;  
begin
  Chart1.SaveToFile(TJPEGImage, 'mon_graphique.jpg');
end;

procedure TForm1.ExporterEnBMP;  
begin
  Chart1.SaveToFile(TBitmap, 'mon_graphique.bmp');
end;
```

### Copier dans le presse-papiers

```pascal
uses
  Clipbrd;

procedure TForm1.CopierPressePapier;  
var
  Bitmap: TBitmap;
begin
  Bitmap := TBitmap.Create;
  try
    Bitmap.Width := Chart1.Width;
    Bitmap.Height := Chart1.Height;
    Chart1.DrawOnCanvas(Bitmap.Canvas.Handle, Rect(0, 0,
      Bitmap.Width, Bitmap.Height));
    Clipboard.Assign(Bitmap);
    ShowMessage('Graphique copié dans le presse-papiers');
  finally
    Bitmap.Free;
  end;
end;
```

## Graphiques en temps réel

```pascal
type
  TForm1 = class(TForm)
    Chart1: TChart;
    LineSeries1: TLineSeries;
    Timer1: TTimer;
    procedure Timer1Timer(Sender: TObject);
  private
    FCompteur: Integer;
  end;

procedure TForm1.Timer1Timer(Sender: TObject);  
begin
  Inc(FCompteur);

  // Ajouter un nouveau point
  LineSeries1.AddXY(FCompteur, 50 + Random(50));

  // Limiter le nombre de points affichés
  if LineSeries1.Count > 50 then
    LineSeries1.Delete(0);

  // Ajuster les limites de l'axe X
  Chart1.Extent.UseXMin := True;
  Chart1.Extent.XMin := FCompteur - 50;
  Chart1.Extent.UseXMax := True;
  Chart1.Extent.XMax := FCompteur;
end;
```

## Astuces et bonnes pratiques

### Performance avec beaucoup de données

```pascal
procedure TForm1.ChargerBeaucoupDeDonnees;  
var
  i: Integer;
begin
  // Désactiver le redessin pendant le chargement
  Chart1.DisableRedrawing;
  try
    LineSeries1.Clear;
    for i := 0 to 10000 do
      LineSeries1.AddXY(i, Sin(i * 0.01) * 100);
  finally
    Chart1.EnableRedrawing;
  end;
end;
```

### Gestion de la mémoire

```pascal
procedure TForm1.FormDestroy(Sender: TObject);  
begin
  // Les séries sont automatiquement libérées avec le Chart
  // Mais si vous créez des sources de données :
  if Assigned(MonChartSource) then
    MonChartSource.Free;
end;
```

### Adapter le graphique à la taille

```pascal
procedure TForm1.FormResize(Sender: TObject);  
begin
  Chart1.Width := ClientWidth - 20;
  Chart1.Height := ClientHeight - 20;

  // Ajuster la taille de la police
  if Chart1.Width < 400 then
    Chart1.Title.Font.Size := 10
  else
    Chart1.Title.Font.Size := 14;
end;
```

## Considérations multi-plateformes

### Compatibilité Windows/Ubuntu

TAChart fonctionne de manière identique sur tous les systèmes, mais quelques détails :

**Rendu** :
```pascal
{$IFDEF WINDOWS}
  // Windows utilise GDI par défaut
  Chart1.BackColor := clWhite;
{$ENDIF}
{$IFDEF UNIX}
  // Linux utilise GTK/Qt
  Chart1.BackColor := clWhite; // Même code !
{$ENDIF}
```

**Polices** :
```pascal
procedure TForm1.ConfigurerPolice;  
begin
  {$IFDEF WINDOWS}
    Chart1.Title.Font.Name := 'Segoe UI';
  {$ENDIF}
  {$IFDEF UNIX}
    Chart1.Title.Font.Name := 'Ubuntu'; // ou 'DejaVu Sans'
  {$ENDIF}
  Chart1.Title.Font.Size := 12;
end;
```

**Export de fichiers** :
```pascal
procedure TForm1.ExporterGraphique;  
var
  CheminFichier: String;
begin
  {$IFDEF WINDOWS}
    CheminFichier := 'C:\Mes Documents\graphique.png';
  {$ENDIF}
  {$IFDEF UNIX}
    CheminFichier := GetUserDir + 'graphique.png';
  {$ENDIF}

  Chart1.SaveToFile(TPortableNetworkGraphic, CheminFichier);
end;
```

**Performances** :
- Les performances sont similaires sur Windows et Ubuntu
- Le rendu peut être légèrement plus rapide sur Windows avec GDI
- Sur Ubuntu, GTK2 est généralement plus rapide que GTK3 pour les graphiques

## Intégration avec NumLib

TAChart s'intègre parfaitement avec NumLib pour visualiser les résultats de calculs.

```pascal
uses
  uTypes, uMatrix, TAGraph, TASeries;

procedure TForm1.VisualiserResultats;  
var
  vecteur: TVector;
  i: Integer;
begin
  // Calculs avec NumLib
  SetLength(vecteur, 100);
  for i := 0 to 99 do
    vecteur[i] := Sin(i * 0.1) * Exp(-i * 0.01);

  // Visualisation avec TAChart
  LineSeries1.Clear;
  for i := 0 to High(vecteur) do
    LineSeries1.AddXY(i, vecteur[i]);

  Chart1.Title.Text.Text := 'Résultats du calcul';
end;
```

## Ressources supplémentaires

### Documentation

- **Wiki Lazarus** : Section TAChart avec tutoriels et exemples
- **Forum Lazarus** : Catégorie "Graphics and Multimedia"
- **Exemples inclus** : Dans le dossier `examples/TAChart` de votre installation Lazarus
- **API Reference** : Documentation complète des classes TAChart

### Exemples fournis avec Lazarus

Lazarus inclut de nombreux exemples TAChart que vous pouvez étudier :

**Emplacement** :
- Windows : `C:\lazarus\examples\tachart\`
- Ubuntu : `/usr/share/lazarus/examples/tachart/`

**Exemples intéressants** :
- `simple` : Graphiques de base
- `func` : Graphiques de fonctions mathématiques
- `financial` : Graphiques financiers (chandelles)
- `animation` : Animations et mises à jour dynamiques
- `tools` : Outils interactifs (zoom, pan, etc.)

### Bibliothèques complémentaires

- **BGRABitmap** : Pour des effets visuels avancés
- **LazMapViewer** : Pour afficher des graphiques sur des cartes
- **fpSpreadsheet** : Pour lire des données depuis Excel et les graphiquer

## Exemples avancés

### Graphique financier (chandelles)

```pascal
uses
  TAGraph, TASeries, TAChartUtils;

procedure TForm1.CreerGraphiqueChandelles;  
var
  Serie: TOpenHighLowCloseSeries;
  i: Integer;
  ouverture, max, min, fermeture: Double;
begin
  Serie := TOpenHighLowCloseSeries.Create(Chart1);
  Serie.Title := 'Cours de bourse';

  // Style
  Serie.UpLinePen.Color := clGreen;
  Serie.DownLinePen.Color := clRed;

  // Générer des données de bourse simulées
  ouverture := 100;
  for i := 1 to 20 do
  begin
    fermeture := ouverture + Random(10) - 5;
    max := Max(ouverture, fermeture) + Random(5);
    min := Min(ouverture, fermeture) - Random(5);

    Serie.AddXOHLC(i, ouverture, max, min, fermeture);
    ouverture := fermeture;
  end;

  Chart1.AddSeries(Serie);
  Chart1.BottomAxis.Title.Caption := 'Jours';
  Chart1.LeftAxis.Title.Caption := 'Prix (€)';
end;
```

### Graphique avec zones d'alerte

```pascal
uses
  TAGraph, TASeries, TAChartAxis;

procedure TForm1.CreerGraphiqueAvecZones;  
var
  SerieDonnees: TLineSeries;
  ZoneVerte, ZoneRouge: TAreaSeries;
  i: Integer;
begin
  Chart1.Title.Text.Text := 'Température avec seuils';

  // Zone dangereuse (rouge) - au-dessus de 30°C
  ZoneRouge := TAreaSeries.Create(Chart1);
  ZoneRouge.AreaBrush.Color := clRed;
  ZoneRouge.AreaBrush.Style := bsFDiagonal;
  ZoneRouge.ZPosition := zpBack;
  for i := 0 to 24 do
    ZoneRouge.AddXY(i, 30);
  Chart1.AddSeries(ZoneRouge);

  // Zone optimale (verte) - entre 18 et 24°C
  ZoneVerte := TAreaSeries.Create(Chart1);
  ZoneVerte.AreaBrush.Color := clLime;
  ZoneVerte.AreaBrush.Style := bsBDiagonal;
  ZoneVerte.ZPosition := zpBack;
  for i := 0 to 24 do
  begin
    ZoneVerte.AddXY(i, 18);
    ZoneVerte.AddXY(i, 24);
  end;
  Chart1.AddSeries(ZoneVerte);

  // Données réelles
  SerieDonnees := TLineSeries.Create(Chart1);
  SerieDonnees.Title := 'Température';
  SerieDonnees.LinePen.Width := 3;
  SerieDonnees.SeriesColor := clBlue;
  for i := 0 to 24 do
    SerieDonnees.AddXY(i, 15 + Random(20));
  Chart1.AddSeries(SerieDonnees);

  Chart1.BottomAxis.Title.Caption := 'Heures';
  Chart1.LeftAxis.Title.Caption := 'Température (°C)';
end;
```

### Histogramme de distribution

```pascal
uses
  TAGraph, TASeries, Math;

procedure TForm1.CreerHistogramme;  
var
  Serie: TBarSeries;
  donnees: array of Double;
  bins: array of Integer;
  i, bin, nbBins: Integer;
  minVal, maxVal, largeurBin: Double;
begin
  // Générer des données aléatoires (distribution normale simulée)
  SetLength(donnees, 1000);
  for i := 0 to 999 do
    donnees[i] := 50 + (Random - 0.5) * 20 + (Random - 0.5) * 20;

  // Calculer l'histogramme
  nbBins := 20;
  SetLength(bins, nbBins);
  minVal := MinValue(donnees);
  maxVal := MaxValue(donnees);
  largeurBin := (maxVal - minVal) / nbBins;

  // Compter les occurrences dans chaque bin
  for i := 0 to High(donnees) do
  begin
    bin := Trunc((donnees[i] - minVal) / largeurBin);
    if bin >= nbBins then bin := nbBins - 1;
    if bin < 0 then bin := 0;
    Inc(bins[bin]);
  end;

  // Créer le graphique
  Serie := TBarSeries.Create(Chart1);
  Serie.Title := 'Distribution';
  Serie.BarBrush.Color := clSkyBlue;

  for i := 0 to nbBins - 1 do
    Serie.AddXY(minVal + i * largeurBin + largeurBin / 2, bins[i]);

  Chart1.AddSeries(Serie);
  Chart1.Title.Text.Text := 'Histogramme de distribution';
  Chart1.BottomAxis.Title.Caption := 'Valeur';
  Chart1.LeftAxis.Title.Caption := 'Fréquence';
end;
```

### Graphique radar

```pascal
uses
  TAGraph, TASeries, TARadialSeries;

procedure TForm1.CreerGraphiqueRadar;  
var
  Serie: TPolarSeries;
  angles: array[0..5] of Double;
  valeurs: array[0..5] of Double;
  i: Integer;
begin
  // Données pour 6 dimensions
  angles[0] := 0;    valeurs[0] := 80;  // Performance
  angles[1] := 60;   valeurs[1] := 65;  // Qualité
  angles[2] := 120;  valeurs[2] := 90;  // Innovation
  angles[3] := 180;  valeurs[3] := 75;  // Service
  angles[4] := 240;  valeurs[4] := 70;  // Prix
  angles[5] := 300;  valeurs[5] := 85;  // Support

  Serie := TPolarSeries.Create(Chart1);
  Serie.Title := 'Évaluation produit';

  for i := 0 to 5 do
    Serie.AddXY(angles[i], valeurs[i]);

  // Fermer le polygone
  Serie.AddXY(360, valeurs[0]);

  Chart1.AddSeries(Serie);
  Chart1.Title.Text.Text := 'Analyse multi-critères';
end;
```

### Graphique avec double échelle logarithmique

```pascal
uses
  TAGraph, TASeries, TATransformations;

procedure TForm1.CreerGraphiqueLog;  
var
  Serie: TLineSeries;
  TransfoY: TLogarithmAxisTransform;
  i: Integer;
begin
  // Transformation logarithmique de l'axe Y
  TransfoY := TLogarithmAxisTransform.Create(Chart1);
  TransfoY.Base := 10;
  Chart1.LeftAxis.Transformations := TChartAxisTransformations.Create(Chart1);
  Chart1.LeftAxis.Transformations.Add(TransfoY);

  // Série avec croissance exponentielle
  Serie := TLineSeries.Create(Chart1);
  Serie.Title := 'Croissance exponentielle';

  for i := 1 to 20 do
    Serie.AddXY(i, Power(2, i)); // 2^i

  Chart1.AddSeries(Serie);
  Chart1.Title.Text.Text := 'Échelle logarithmique';
  Chart1.BottomAxis.Title.Caption := 'n';
  Chart1.LeftAxis.Title.Caption := 'log(2^n)';
end;
```

## Animation et mise à jour dynamique

### Animation fluide

```pascal
type
  TForm1 = class(TForm)
    Chart1: TChart;
    Timer1: TTimer;
    LineSeries1: TLineSeries;
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FPhase: Double;
  end;

procedure TForm1.FormCreate(Sender: TObject);  
begin
  FPhase := 0;
  Timer1.Interval := 50; // 20 FPS
  Timer1.Enabled := True;
end;

procedure TForm1.Timer1Timer(Sender: TObject);  
var
  i: Integer;
  x, y: Double;
begin
  Chart1.DisableRedrawing;
  try
    LineSeries1.Clear;

    // Générer une onde sinusoïdale animée
    for i := 0 to 100 do
    begin
      x := i * 0.1;
      y := Sin(x + FPhase) * 50;
      LineSeries1.AddXY(x, y);
    end;

    FPhase := FPhase + 0.1;
    if FPhase > 2 * Pi then
      FPhase := FPhase - 2 * Pi;
  finally
    Chart1.EnableRedrawing;
  end;
end;
```

### Transition progressive

```pascal
type
  TForm1 = class(TForm)
    procedure AnimerTransition;
  private
    FEtapeAnimation: Integer;
  end;

procedure TForm1.AnimerTransition;  
var
  i: Integer;
  facteur: Double;
begin
  facteur := FEtapeAnimation / 100; // 0 à 1

  Chart1.DisableRedrawing;
  try
    LineSeries1.Clear;

    for i := 0 to 50 do
    begin
      // Interpolation entre deux états
      LineSeries1.AddXY(i,
        (1 - facteur) * ValeurInitiale[i] + facteur * ValeurFinale[i]);
    end;
  finally
    Chart1.EnableRedrawing;
  end;

  Inc(FEtapeAnimation);
  if FEtapeAnimation > 100 then
    Timer1.Enabled := False;
end;
```

## Intégration avec bases de données

### Afficher des données depuis une base

```pascal
uses
  TAGraph, TASeries, DB, SQLDB;

procedure TForm1.ChargerDepuisBaseDeDonnees;  
var
  Serie: TLineSeries;
begin
  Serie := TLineSeries.Create(Chart1);
  Serie.Title := 'Ventes mensuelles';

  // Ouvrir la requête
  SQLQuery1.SQL.Text := 'SELECT mois, ventes FROM statistiques ORDER BY mois';
  SQLQuery1.Open;

  // Remplir le graphique
  while not SQLQuery1.EOF do
  begin
    Serie.AddXY(
      SQLQuery1.FieldByName('mois').AsInteger,
      SQLQuery1.FieldByName('ventes').AsFloat
    );
    SQLQuery1.Next;
  end;

  Chart1.AddSeries(Serie);
  SQLQuery1.Close;
end;
```

### Liaison directe avec TDataSource

```pascal
uses
  TAGraph, TADbSource;

procedure TForm1.LierAvecDataSource;  
var
  DBSource: TDbChartSource;
  Serie: TLineSeries;
begin
  // Créer une source TAChart liée à la base
  DBSource := TDbChartSource.Create(Self);
  DBSource.DataSource := DataSource1;
  DBSource.FieldX := 'date';
  DBSource.FieldY := 'valeur';

  // Créer la série
  Serie := TLineSeries.Create(Chart1);
  Serie.Source := DBSource;
  Serie.Title := 'Données temps réel';

  Chart1.AddSeries(Serie);
end;
```

## Impression de graphiques

```pascal
uses
  Printers, TADrawUtils;

procedure TForm1.ImprimerGraphique;  
var
  R: TRect;
begin
  if PrintDialog1.Execute then
  begin
    Printer.BeginDoc;
    try
      // Définir la zone d'impression (toute la page)
      R := Rect(0, 0, Printer.PageWidth, Printer.PageHeight);

      // Dessiner le graphique
      Chart1.Draw(TCanvasDrawer.Create(Printer.Canvas), R);

    finally
      Printer.EndDoc;
    end;
  end;
end;

procedure TForm1.AperçuAvantImpression;  
var
  Bitmap: TBitmap;
begin
  Bitmap := TBitmap.Create;
  try
    // Créer une image haute résolution
    Bitmap.Width := 1920;
    Bitmap.Height := 1080;

    Chart1.DrawOnCanvas(Bitmap.Canvas.Handle,
      Rect(0, 0, Bitmap.Width, Bitmap.Height));

    // Afficher dans une fenêtre d'aperçu
    Image1.Picture.Assign(Bitmap);
  finally
    Bitmap.Free;
  end;
end;
```

## Dépannage

### Problèmes courants et solutions

**Le graphique ne s'affiche pas** :
```pascal
// Vérifier que la série a des données
if LineSeries1.Count = 0 then
  ShowMessage('Aucune donnée dans la série');

// Vérifier que la série est ajoutée au chart
if Chart1.SeriesCount = 0 then
  ShowMessage('Aucune série dans le graphique');

// Forcer le redessin
Chart1.Invalidate;
```

**Les axes ne sont pas corrects** :
```pascal
// Réinitialiser l'étendue automatique
Chart1.Extent.UseXMin := False;  
Chart1.Extent.UseXMax := False;  
Chart1.Extent.UseYMin := False;  
Chart1.Extent.UseYMax := False;

// Ou définir manuellement
Chart1.Extent.UseYMin := True;  
Chart1.Extent.YMin := 0;  
Chart1.Extent.UseYMax := True;  
Chart1.Extent.YMax := 100;
```

**Performance lente avec beaucoup de données** :
```pascal
// Utiliser DisableRedrawing
Chart1.DisableRedrawing;  
try
  // Ajouter toutes les données
finally
  Chart1.EnableRedrawing;
end;

// Réduire la fréquence de mise à jour
Timer1.Interval := 100; // Au lieu de 10

// Limiter le nombre de points affichés
if Serie.Count > 1000 then
  Serie.Delete(0); // Supprimer le plus ancien
```

**Mémoire qui augmente** :
```pascal
// Libérer les anciennes séries
procedure TForm1.NettoyerSeries;  
var
  i: Integer;
begin
  for i := Chart1.SeriesCount - 1 downto 0 do
    Chart1.DeleteSeries(Chart1.Series[i]);
end;

// Limiter la taille des données
while LineSeries1.Count > 1000 do
  LineSeries1.Delete(0);
```

## Conclusion

TAChart est un outil puissant et flexible pour créer des visualisations de données en FreePascal/Lazarus. Ses points forts :

✓ **Facile à utiliser** : Interface intuitive et bien documentée  
✓ **Complet** : Tous les types de graphiques courants  
✓ **Performant** : Gère efficacement des milliers de points  
✓ **Personnalisable** : Contrôle total sur l'apparence  
✓ **Interactif** : Zoom, pan, sélection, info-bulles  
✓ **Multi-plateforme** : Fonctionne identiquement sur Windows, Ubuntu, macOS  
✓ **Gratuit** : Open source, aucune limitation

### Pour aller plus loin

- **Chapitre 16.1** : NumLib pour les calculs numériques à visualiser
- **Chapitre 16.3** : Traitement du signal (DSP) avec visualisation
- **Chapitre 12** : Interfaces graphiques avancées avec BGRABitmap
- **Chapitre 20** : Optimisation des performances pour graphiques complexes

### Ressources en ligne

- **Wiki Lazarus TAChart** : https://wiki.freepascal.org/TAChart
- **Forum Lazarus** : Section Graphics and Multimedia
- **GitHub** : Exemples communautaires de TAChart
- **Stack Overflow** : Tag `lazarus` et `tachart`

N'hésitez pas à expérimenter et à explorer les nombreuses possibilités offertes par TAChart. La meilleure façon d'apprendre est de pratiquer avec vos propres données et de découvrir progressivement toutes les fonctionnalités disponibles !

⏭️ [Traitement du signal (DSP)](/16-traitement-donnees-calcul-scientifique/03-traitement-signal-dsp.md)
