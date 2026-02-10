🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 15.3 Computer Vision avec OpenCV

## Introduction à la Vision par Ordinateur

La vision par ordinateur (Computer Vision) est le domaine de l'informatique qui permet aux machines de "voir" et d'interpréter les images et vidéos, tout comme le fait l'œil humain. OpenCV (Open Source Computer Vision Library) est la bibliothèque de référence dans ce domaine, utilisée dans l'industrie, la recherche et les applications grand public.

### Qu'est-ce qu'OpenCV ?

OpenCV est une bibliothèque open source créée par Intel en 1999, qui contient plus de 2500 algorithmes optimisés pour :
- Le traitement d'images
- La détection d'objets
- La reconnaissance faciale
- Le suivi de mouvement
- La reconstruction 3D
- L'apprentissage automatique appliqué à la vision

### Pourquoi utiliser OpenCV avec FreePascal/Lazarus ?

FreePascal permet d'utiliser OpenCV via des bindings (interfaces) qui donnent accès à toute la puissance d'OpenCV tout en profitant de la simplicité et de la performance de Pascal. C'est particulièrement intéressant pour :
- Créer des applications de vision portable (Windows/Linux)
- Développer des prototypes rapidement
- Intégrer la vision dans des applications existantes
- Apprendre la vision par ordinateur avec un langage accessible

---

## Installation et Configuration

### Installation sur Windows

**Étape 1 : Télécharger OpenCV**

1. Rendez-vous sur le site officiel : https://opencv.org/releases/
2. Téléchargez la version Windows (par exemple opencv-4.x.x-windows.exe)
3. Exécutez l'installateur et extrayez dans `C:\opencv`

**Étape 2 : Configurer les variables d'environnement**

1. Ajoutez au PATH système : `C:\opencv\build\x64\vc15\bin`
2. Redémarrez votre ordinateur pour appliquer les changements

**Étape 3 : Installer les bindings Pascal**

Plusieurs options sont disponibles :
- **OpenCV4Lazarus** : binding moderne et maintenu
- **Delphi-OpenCV** : compatible FreePascal avec quelques adaptations

Installation via Online Package Manager de Lazarus :
1. Package → Online Package Manager
2. Rechercher "opencv"
3. Installer le package souhaité

### Installation sur Ubuntu/Linux

**Étape 1 : Installer OpenCV via APT**

```bash
sudo apt update  
sudo apt install libopencv-dev python3-opencv
```

**Étape 2 : Vérifier l'installation**

```bash
pkg-config --modversion opencv4
```

**Étape 3 : Installer les bindings Pascal**

Même procédure que Windows via l'Online Package Manager de Lazarus.

### Configuration alternative : Compilation manuelle

Si vous avez besoin d'une version spécifique ou optimisée :

**Sous Ubuntu :**
```bash
# Dépendances
sudo apt install build-essential cmake git pkg-config libgtk-3-dev

# Télécharger OpenCV
git clone https://github.com/opencv/opencv.git  
cd opencv  
mkdir build && cd build

# Compiler
cmake ..  
make -j4  
sudo make install
```

**Sous Windows :**
Utilisez CMake GUI et Visual Studio pour compiler depuis les sources.

---

## Les Fondamentaux : Charger et Afficher une Image

### Structure de base d'un programme OpenCV

Voici un premier exemple simple qui charge et affiche une image :

```pascal
program FirstOpenCV;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils,
  cv.core, cv.highgui, cv.imgproc; // Modules OpenCV

var
  image: TMat;  // Structure de données pour stocker l'image

begin
  // Charger une image depuis le disque
  image := imread('photo.jpg', IMREAD_COLOR);

  // Vérifier que l'image a bien été chargée
  if image.empty then
  begin
    WriteLn('Erreur : impossible de charger l''image');
    Exit;
  end;

  // Afficher l'image dans une fenêtre
  imshow('Ma première image', image);

  // Attendre une touche avant de fermer
  waitKey(0);

  // Libérer la mémoire
  image.release;
end.
```

### Explications détaillées

**TMat - La structure de base**

`TMat` (Matrix) est le conteneur principal d'OpenCV pour stocker les images. Une image est en réalité une matrice de pixels :
- Pour une image en niveaux de gris : matrice 2D (hauteur × largeur)
- Pour une image couleur : matrice 3D (hauteur × largeur × 3 canaux RGB)

**imread() - Charger une image**

```pascal
image := imread(filename, flags);
```

Flags disponibles :
- `IMREAD_COLOR` : charge en couleur (par défaut)
- `IMREAD_GRAYSCALE` : charge en niveaux de gris
- `IMREAD_UNCHANGED` : charge avec canal alpha si présent

**imshow() - Afficher une image**

```pascal
imshow(windowName, image);
```

Crée une fenêtre et y affiche l'image. La fenêtre s'adapte automatiquement à la taille de l'image.

**waitKey() - Gestion des événements**

```pascal
key := waitKey(delay);
```

- `delay = 0` : attend indéfiniment qu'une touche soit pressée
- `delay > 0` : attend X millisecondes puis continue
- Retourne le code ASCII de la touche pressée

---

## Intégration dans une Application Lazarus

### Créer une application GUI avec OpenCV

Voici comment intégrer OpenCV dans une vraie application Lazarus avec interface graphique :

```pascal
unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  cv.core, cv.highgui, cv.imgproc;

type
  TForm1 = class(TForm)
    ButtonLoad: TButton;
    ButtonGray: TButton;
    ButtonBlur: TButton;
    Image1: TImage;
    OpenDialog1: TOpenDialog;
    procedure ButtonLoadClick(Sender: TObject);
    procedure ButtonGrayClick(Sender: TObject);
    procedure ButtonBlurClick(Sender: TObject);
  private
    FCurrentImage: TMat;
    procedure MatToImage(const AMat: TMat; AImage: TImage);
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.MatToImage(const AMat: TMat; AImage: TImage);  
var
  bmp: TBitmap;
  tempMat: TMat;
  x, y: Integer;
  pixel: PByte;
begin
  if AMat.empty then Exit;

  // Convertir en BGR si nécessaire
  if AMat.channels = 3 then
    cvtColor(AMat, tempMat, COLOR_BGR2RGB)
  else
    tempMat := AMat.clone;

  // Créer le bitmap
  bmp := TBitmap.Create;
  try
    bmp.SetSize(tempMat.cols, tempMat.rows);

    // Copier les pixels
    for y := 0 to tempMat.rows - 1 do
    begin
      pixel := tempMat.ptr(y);
      for x := 0 to tempMat.cols - 1 do
      begin
        if tempMat.channels = 1 then
          bmp.Canvas.Pixels[x, y] := RGBToColor(pixel^, pixel^, pixel^)
        else
          bmp.Canvas.Pixels[x, y] := RGBToColor(pixel[0], pixel[1], pixel[2]);
        Inc(pixel, tempMat.channels);
      end;
    end;

    AImage.Picture.Assign(bmp);
  finally
    bmp.Free;
    if tempMat.data <> AMat.data then
      tempMat.release;
  end;
end;

procedure TForm1.ButtonLoadClick(Sender: TObject);  
begin
  if OpenDialog1.Execute then
  begin
    if not FCurrentImage.empty then
      FCurrentImage.release;

    FCurrentImage := imread(OpenDialog1.FileName, IMREAD_COLOR);

    if FCurrentImage.empty then
      ShowMessage('Erreur lors du chargement')
    else
      MatToImage(FCurrentImage, Image1);
  end;
end;

procedure TForm1.ButtonGrayClick(Sender: TObject);  
var
  grayImage: TMat;
begin
  if FCurrentImage.empty then
  begin
    ShowMessage('Veuillez d''abord charger une image');
    Exit;
  end;

  // Convertir en niveaux de gris
  cvtColor(FCurrentImage, grayImage, COLOR_BGR2GRAY);
  MatToImage(grayImage, Image1);
  grayImage.release;
end;

procedure TForm1.ButtonBlurClick(Sender: TObject);  
var
  blurredImage: TMat;
begin
  if FCurrentImage.empty then
  begin
    ShowMessage('Veuillez d''abord charger une image');
    Exit;
  end;

  // Appliquer un flou gaussien
  GaussianBlur(FCurrentImage, blurredImage, Size(15, 15), 0);
  MatToImage(blurredImage, Image1);
  blurredImage.release;
end;

end.
```

---

## Opérations de Base sur les Images

### Conversion d'espaces colorimétriques

Les images peuvent être représentées dans différents espaces de couleurs :

```pascal
var
  source, destination: TMat;
begin
  source := imread('image.jpg');

  // BGR → Niveaux de gris
  cvtColor(source, destination, COLOR_BGR2GRAY);

  // BGR → HSV (Teinte, Saturation, Valeur)
  cvtColor(source, destination, COLOR_BGR2HSV);

  // BGR → RGB (inversion Rouge/Bleu)
  cvtColor(source, destination, COLOR_BGR2RGB);

  destination.release;
  source.release;
end;
```

**Pourquoi utiliser HSV ?**

L'espace HSV est très utile pour la détection de couleurs car :
- H (Hue) représente la teinte pure
- S (Saturation) représente l'intensité de la couleur
- V (Value) représente la luminosité

Cela permet de détecter une couleur indépendamment de l'éclairage.

### Filtrage et traitement d'image

**Flou gaussien - Réduire le bruit**

```pascal
var
  source, destination: TMat;
begin
  source := imread('image.jpg');

  // Flou gaussien avec kernel 5×5
  GaussianBlur(source, destination, Size(5, 5), 0);

  imshow('Image floue', destination);
  waitKey(0);

  destination.release;
  source.release;
end;
```

**Détection de contours avec Canny**

```pascal
var
  source, gray, edges: TMat;
begin
  source := imread('image.jpg');

  // Convertir en niveaux de gris
  cvtColor(source, gray, COLOR_BGR2GRAY);

  // Appliquer Canny pour détecter les contours
  // Paramètres : seuil bas (50), seuil haut (150)
  Canny(gray, edges, 50, 150);

  imshow('Contours détectés', edges);
  waitKey(0);

  edges.release;
  gray.release;
  source.release;
end;
```

**Seuillage (Thresholding)**

Convertir une image en noir et blanc selon un seuil :

```pascal
var
  source, gray, binary: TMat;
begin
  source := imread('image.jpg');
  cvtColor(source, gray, COLOR_BGR2GRAY);

  // Seuillage simple : pixels > 127 deviennent blancs
  threshold(gray, binary, 127, 255, THRESH_BINARY);

  // Seuillage adaptatif (s'adapte à l'éclairage local)
  adaptiveThreshold(gray, binary, 255, ADAPTIVE_THRESH_GAUSSIAN_C,
                    THRESH_BINARY, 11, 2);

  imshow('Image binaire', binary);
  waitKey(0);
end;
```

---

## Détection d'Objets

### Détection de visages avec Cascade de Haar

OpenCV inclut des classificateurs pré-entraînés pour détecter visages, yeux, sourires, etc.

```pascal
program FaceDetection;

uses
  cv.core, cv.highgui, cv.objdetect, cv.imgproc;

var
  image, gray: TMat;
  faceCascade: TCascadeClassifier;
  faces: TArray<TRect>;
  i: Integer;

begin
  // Charger le classificateur de visages
  faceCascade := TCascadeClassifier.Create;

  {$IFDEF WINDOWS}
  faceCascade.load('C:\opencv\data\haarcascades\haarcascade_frontalface_default.xml');
  {$ENDIF}
  {$IFDEF LINUX}
  faceCascade.load('/usr/share/opencv4/haarcascades/haarcascade_frontalface_default.xml');
  {$ENDIF}

  if faceCascade.empty then
  begin
    WriteLn('Erreur : impossible de charger le classificateur');
    Exit;
  end;

  // Charger l'image
  image := imread('groupe.jpg');
  cvtColor(image, gray, COLOR_BGR2GRAY);

  // Détecter les visages
  faceCascade.detectMultiScale(gray, faces, 1.1, 3, 0, Size(30, 30));

  WriteLn('Nombre de visages détectés : ', Length(faces));

  // Dessiner un rectangle autour de chaque visage
  for i := 0 to High(faces) do
  begin
    rectangle(image, faces[i], Scalar(0, 255, 0), 2);
  end;

  imshow('Détection de visages', image);
  waitKey(0);

  faceCascade.Free;
  gray.release;
  image.release;
end.
```

### Détection de couleurs

Isoler des objets par leur couleur en utilisant l'espace HSV :

```pascal
var
  source, hsv, mask: TMat;
  lowerBlue, upperBlue: TScalar;
begin
  source := imread('image.jpg');

  // Convertir en HSV
  cvtColor(source, hsv, COLOR_BGR2HSV);

  // Définir la plage de bleu en HSV
  lowerBlue := Scalar(100, 50, 50);   // Bleu foncé
  upperBlue := Scalar(130, 255, 255); // Bleu clair

  // Créer un masque pour isoler le bleu
  inRange(hsv, lowerBlue, upperBlue, mask);

  imshow('Masque bleu', mask);
  waitKey(0);
end;
```

### Détection de formes géométriques

Détecter cercles, lignes, rectangles :

```pascal
// Détection de cercles avec Hough Transform
var
  source, gray: TMat;
  circles: TMat;
  i: Integer;
  center: TPoint;
  radius: Integer;
begin
  source := imread('pieces.jpg');
  cvtColor(source, gray, COLOR_BGR2GRAY);

  // Appliquer un flou pour améliorer la détection
  GaussianBlur(gray, gray, Size(9, 9), 2);

  // Détecter les cercles
  HoughCircles(gray, circles, HOUGH_GRADIENT, 1, gray.rows / 8, 200, 100, 0, 0);

  // Dessiner les cercles détectés
  for i := 0 to circles.cols - 1 do
  begin
    center := Point(Round(circles.at<Single>(0, i, 0)),
                    Round(circles.at<Single>(0, i, 1)));
    radius := Round(circles.at<Single>(0, i, 2));

    circle(source, center, radius, Scalar(0, 255, 0), 2);
    circle(source, center, 2, Scalar(0, 0, 255), 3);
  end;

  imshow('Cercles détectés', source);
  waitKey(0);
end;
```

---

## Traitement Vidéo en Temps Réel

### Capture depuis une webcam

```pascal
program WebcamCapture;

uses
  cv.core, cv.highgui, cv.videoio;

var
  capture: TVideoCapture;
  frame: TMat;
  key: Integer;

begin
  // Ouvrir la webcam (0 = première webcam)
  capture := TVideoCapture.Create(0);

  if not capture.isOpened then
  begin
    WriteLn('Erreur : impossible d''ouvrir la webcam');
    Exit;
  end;

  WriteLn('Appuyez sur ESC pour quitter');

  // Boucle de capture
  while True do
  begin
    // Lire une frame
    capture.read(frame);

    if frame.empty then
      Break;

    // Afficher la frame
    imshow('Webcam', frame);

    // Attendre 30ms et vérifier si ESC est pressé
    key := waitKey(30);
    if key = 27 then  // ESC
      Break;
  end;

  capture.release;
  destroyAllWindows;
end.
```

### Détection de mouvement

```pascal
var
  capture: TVideoCapture;
  frame, prevFrame, diff, thresh: TMat;
  gray, prevGray: TMat;
  contours: TArray<TArray<TPoint>>;
  i: Integer;
  area: Double;

begin
  capture := TVideoCapture.Create(0);

  // Lire la première frame
  capture.read(prevFrame);
  cvtColor(prevFrame, prevGray, COLOR_BGR2GRAY);
  GaussianBlur(prevGray, prevGray, Size(21, 21), 0);

  while True do
  begin
    capture.read(frame);
    if frame.empty then Break;

    // Convertir en niveaux de gris
    cvtColor(frame, gray, COLOR_BGR2GRAY);
    GaussianBlur(gray, gray, Size(21, 21), 0);

    // Calculer la différence
    absdiff(prevGray, gray, diff);
    threshold(diff, thresh, 25, 255, THRESH_BINARY);

    // Trouver les contours
    findContours(thresh, contours, RETR_EXTERNAL, CHAIN_APPROX_SIMPLE);

    // Dessiner les zones de mouvement
    for i := 0 to High(contours) do
    begin
      area := contourArea(contours[i]);
      if area > 500 then  // Ignorer les petits mouvements
        drawContours(frame, contours, i, Scalar(0, 255, 0), 2);
    end;

    imshow('Détection de mouvement', frame);

    // La frame actuelle devient la précédente
    prevGray := gray.clone;

    if waitKey(30) = 27 then Break;
  end;

  capture.release;
end;
```

---

## Applications Pratiques Multi-plateformes

### Considérations Windows vs Linux

**Chemins des fichiers de cascade**

```pascal
function GetCascadePath(const filename: string): string;  
begin
  {$IFDEF WINDOWS}
  Result := 'C:\opencv\data\haarcascades\' + filename;
  {$ENDIF}
  {$IFDEF LINUX}
  Result := '/usr/share/opencv4/haarcascades/' + filename;
  {$ENDIF}
end;

// Utilisation
faceCascade.load(GetCascadePath('haarcascade_frontalface_default.xml'));
```

**Accès aux périphériques vidéo**

Sur Linux, vous pouvez spécifier le périphérique exact :

```pascal
{$IFDEF LINUX}
capture := TVideoCapture.Create('/dev/video0');
{$ELSE}
capture := TVideoCapture.Create(0);
{$ENDIF}
```

### Exemple : Application de surveillance simple

Voici une application complète qui enregistre automatiquement quand du mouvement est détecté :

```pascal
program SimpleSurveillance;

uses
  SysUtils, DateUtils,
  cv.core, cv.highgui, cv.videoio, cv.imgproc;

var
  capture: TVideoCapture;
  writer: TVideoWriter;
  frame, prevFrame, diff, thresh: TMat;
  gray, prevGray: TMat;
  motionDetected: Boolean;
  filename: string;
  codec: Integer;
  fps: Double;

begin
  capture := TVideoCapture.Create(0);
  fps := capture.get(CAP_PROP_FPS);
  if fps = 0 then fps := 30.0;

  // Codec pour enregistrement (MJPEG)
  codec := VideoWriter_fourcc('M', 'J', 'P', 'G');

  capture.read(prevFrame);
  cvtColor(prevFrame, prevGray, COLOR_BGR2GRAY);
  GaussianBlur(prevGray, prevGray, Size(21, 21), 0);

  WriteLn('Surveillance démarrée. ESC pour quitter.');

  while True do
  begin
    capture.read(frame);
    if frame.empty then Break;

    cvtColor(frame, gray, COLOR_BGR2GRAY);
    GaussianBlur(gray, gray, Size(21, 21), 0);
    absdiff(prevGray, gray, diff);
    threshold(diff, thresh, 25, 255, THRESH_BINARY);

    // Vérifier si mouvement significatif
    motionDetected := countNonZero(thresh) > 1000;

    if motionDetected then
    begin
      // Commencer l'enregistrement
      if not writer.isOpened then
      begin
        filename := FormatDateTime('yyyy-mm-dd_hh-nn-ss', Now) + '.avi';
        writer := TVideoWriter.Create(filename, codec, fps,
                                       Size(frame.cols, frame.rows));
        WriteLn('Enregistrement : ', filename);
      end;

      writer.write(frame);

      // Afficher indication visuelle
      putText(frame, 'ENREGISTREMENT', Point(10, 30),
              FONT_HERSHEY_SIMPLEX, 1, Scalar(0, 0, 255), 2);
    end
    else if writer.isOpened then
    begin
      // Arrêter l'enregistrement après 2 secondes sans mouvement
      writer.release;
      WriteLn('Enregistrement terminé');
    end;

    imshow('Surveillance', frame);
    prevGray := gray.clone;

    if waitKey(30) = 27 then Break;
  end;

  if writer.isOpened then
    writer.release;
  capture.release;
  destroyAllWindows;
end.
```

---

## Optimisation et Performance

### Conseils pour améliorer les performances

**1. Réduire la résolution des images**

```pascal
var
  source, resized: TMat;
begin
  source := imread('large_image.jpg');

  // Réduire à 50% de la taille
  resize(source, resized, Size(0, 0), 0.5, 0.5, INTER_LINEAR);

  // Traiter l'image réduite (plus rapide)
  // ...
end;
```

**2. Utiliser le ROI (Region of Interest)**

Traiter uniquement une partie de l'image :

```pascal
var
  source, roi: TMat;
  rect: TRect;
begin
  source := imread('image.jpg');

  // Définir une zone d'intérêt
  rect := Rect(100, 100, 300, 300);  // x, y, largeur, hauteur
  roi := source(rect);  // Créer une vue sur cette zone

  // Traiter uniquement le ROI
  GaussianBlur(roi, roi, Size(5, 5), 0);
end;
```

**3. Convertir en niveaux de gris quand possible**

Les algorithmes sont généralement 3× plus rapides sur des images monochromes.

**4. Utiliser plusieurs threads**

OpenCV utilise automatiquement les threads pour certaines opérations. Vous pouvez contrôler ce comportement :

```pascal
// Définir le nombre de threads OpenCV
setNumThreads(4);

// Vérifier combien de threads sont utilisés
WriteLn('Threads actifs : ', getNumThreads);
```

### Mesure des performances

```pascal
var
  startTime, endTime: Int64;
  fps: Double;
begin
  startTime := getTickCount;

  // Votre code de traitement ici
  processImage(frame);

  endTime := getTickCount;
  fps := getTickFrequency / (endTime - startTime);

  WriteLn(Format('FPS: %.2f', [fps]));
end;
```

---

## Ressources et Aller Plus Loin

### Documentation et tutoriels

**Officiel OpenCV :**
- Documentation : https://docs.opencv.org/
- Tutoriels Python (transposables) : https://docs.opencv.org/master/d6/d00/tutorial_py_root.html

**Bindings FreePascal :**
- GitHub OpenCV4Lazarus
- Forum FreePascal (section Graphics)

### Exemples de projets réalisables

**Débutant :**
- Filtre Instagram (sépia, noir&blanc, vintage)
- Détecteur de présence pour domotique
- Lecteur de codes QR/barres
- Application de retouche photo simple

**Intermédiaire :**
- Système de surveillance avec enregistrement
- Reconnaissance de plaques d'immatriculation
- Compteur de personnes
- Application de réalité augmentée simple

**Avancé :**
- Système de reconnaissance faciale avec base de données
- Suivi d'objets multi-caméras
- Reconstruction 3D depuis photos
- Classification d'images avec apprentissage profond

### Combinaison avec d'autres technologies

OpenCV s'intègre parfaitement avec :
- **Bases de données** : stocker les résultats d'analyse
- **Réseaux** : streaming vidéo, surveillance distante
- **IA/ML** : utiliser des modèles TensorFlow/ONNX avec OpenCV
- **IoT** : traitement d'images sur Raspberry Pi

---

## Conclusion

Vous disposez maintenant des bases solides pour exploiter OpenCV avec FreePascal/Lazarus. La vision par ordinateur ouvre un monde de possibilités :

**Points clés à retenir :**
- OpenCV fonctionne de manière identique sur Windows et Linux
- La structure TMat est au cœur de tous les traitements
- Commencez simple (charger/afficher) puis progressez vers la détection
- Les performances sont excellentes grâce au code C++ optimisé d'OpenCV
- La communauté FreePascal propose de bons bindings maintenus

**Prochaines étapes suggérées :**
1. Expérimentez avec vos propres images et webcam
2. Explorez les différents classificateurs Haar disponibles
3. Combinez plusieurs techniques (détection + tracking + enregistrement)
4. Intégrez OpenCV dans vos projets Lazarus existants

La vision par ordinateur évolue rapidement avec l'IA, mais les fondamentaux présentés ici restent essentiels et constituent la base de systèmes plus complexes.

**Bon développement avec OpenCV et FreePascal/Lazarus !**

⏭️ [NLP et traitement de texte](/15-intelligence-artificielle-machine-learning/04-nlp-traitement-texte.md)
