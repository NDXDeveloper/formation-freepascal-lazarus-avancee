🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 12.10 Vision par ordinateur avec OpenCV

## Introduction

La vision par ordinateur est une branche de l'intelligence artificielle qui permet aux machines de "voir" et d'interpréter le monde visuel. OpenCV (Open Source Computer Vision Library) est la bibliothèque de référence mondiale dans ce domaine, utilisée par des millions de développeurs et dans d'innombrables applications industrielles.

### Qu'est-ce qu'OpenCV ?

**OpenCV** est une bibliothèque open source créée en 1999 par Intel, qui contient plus de 2500 algorithmes optimisés pour :

- La détection et la reconnaissance d'objets
- La reconnaissance faciale et de personnes
- Le suivi d'objets en mouvement
- La reconstruction 3D
- L'analyse vidéo
- L'apprentissage automatique
- Et bien plus encore...

### Pourquoi utiliser OpenCV avec FreePascal ?

- **Performance** : Algorithmes optimisés en C++
- **Maturité** : Plus de 20 ans de développement
- **Communauté** : Énorme base de connaissances
- **Portabilité** : Fonctionne sur Windows, Linux, macOS, Android, iOS
- **Gratuit** : Licence BSD permissive

## Installation et configuration

### Installation sur Windows

#### Méthode 1 : Téléchargement officiel

1. Télécharger OpenCV depuis le site officiel : https://opencv.org/releases/
2. Installer dans `C:\opencv`
3. Les DLL se trouvent dans `C:\opencv\build\x64\vc15\bin`

```pascal
// Chemins typiques Windows
const
  OPENCV_PATH_WIN = 'C:\opencv\build\x64\vc15\bin\';
  OPENCV_CORE_DLL = 'opencv_world480.dll'; // Version 4.8.0
```

#### Méthode 2 : Via Chocolatey

```powershell
choco install opencv
```

### Installation sur Ubuntu

#### Via APT (gestionnaire de paquets)

```bash
# Installation des bibliothèques OpenCV
sudo apt update
sudo apt install libopencv-dev python3-opencv

# Vérifier l'installation
pkg-config --modversion opencv4
```

#### Via compilation source (version la plus récente)

```bash
# Dépendances
sudo apt install build-essential cmake git pkg-config
sudo apt install libgtk-3-dev libavcodec-dev libavformat-dev libswscale-dev

# Télécharger OpenCV
git clone https://github.com/opencv/opencv.git
cd opencv
mkdir build && cd build

# Compiler
cmake ..
make -j$(nproc)
sudo make install
```

```pascal
// Chemins typiques Ubuntu
const
  OPENCV_PATH_LINUX = '/usr/lib/x86_64-linux-gnu/';
  OPENCV_CORE_SO = 'libopencv_core.so.4.5';
```

### Bindings Pascal pour OpenCV

Il existe plusieurs projets de bindings Pascal pour OpenCV :

1. **Delphi-OpenCV** : https://github.com/Laex/Delphi-OpenCV
2. **OpenCV-Pascal** : https://github.com/moctes/pascalOpencv
3. **ocv.pas** : Bindings communautaires

#### Installation du binding

```pascal
// Télécharger le binding depuis GitHub
// Copier les fichiers .pas dans votre projet
// Ajouter au chemin de recherche de Lazarus

uses
  ocv.core,      // Types de base et structures
  ocv.imgproc,   // Traitement d'images
  ocv.highgui,   // Interface utilisateur
  ocv.objdetect, // Détection d'objets
  ocv.videoio;   // Capture vidéo
```

## Concepts fondamentaux

### Structure Mat (Matrix)

La structure **Mat** est le conteneur principal d'OpenCV pour stocker les images :

```pascal
type
  TMat = record
    data: Pointer;      // Données de l'image
    rows: Integer;      // Hauteur
    cols: Integer;      // Largeur
    channels: Integer;  // Nombre de canaux (1=gris, 3=RGB, 4=RGBA)
    depth: Integer;     // Profondeur (8 bits, 16 bits, etc.)
  end;
```

### Espaces colorimétriques

OpenCV travaille principalement en **BGR** (Blue-Green-Red), contrairement à RGB habituel :

```pascal
// Conversion d'espaces colorimétriques
procedure ConvertColorSpace(const src: TMat; var dst: TMat; code: Integer);
begin
  cvtColor(src, dst, code);
end;

// Codes de conversion courants
const
  CV_BGR2GRAY = 6;    // BGR vers niveaux de gris
  CV_BGR2RGB = 4;     // BGR vers RGB
  CV_BGR2HSV = 40;    // BGR vers HSV
  CV_GRAY2BGR = 8;    // Gris vers BGR
```

### Région d'intérêt (ROI - Region of Interest)

```pascal
type
  TRect = record
    x, y: Integer;
    width, height: Integer;
  end;

function GetROI(const img: TMat; roi: TRect): TMat;
begin
  Result := Mat(img, roi); // Crée une vue sans copie
end;
```

## Opérations de base

### Chargement et sauvegarde d'images

```pascal
function LoadImage(const filename: string; flags: Integer = 1): TMat;
begin
  // flags: 1 = couleur, 0 = niveaux de gris, -1 = avec alpha
  Result := imread(PAnsiChar(AnsiString(filename)), flags);

  if Result.data = nil then
    raise Exception.Create('Impossible de charger l''image: ' + filename);
end;

procedure SaveImage(const filename: string; const img: TMat);
begin
  if not imwrite(PAnsiChar(AnsiString(filename)), img) then
    raise Exception.Create('Impossible de sauvegarder l''image: ' + filename);
end;

// Exemple d'utilisation
procedure LoadAndSaveExample;
var
  img: TMat;
begin
  img := LoadImage('input.jpg');
  try
    // Traiter l'image ici
    SaveImage('output.png', img);
  finally
    img.release;
  end;
end;
```

### Affichage d'images

```pascal
procedure ShowImage(const windowName: string; const img: TMat);
begin
  namedWindow(PAnsiChar(AnsiString(windowName)), WINDOW_AUTOSIZE);
  imshow(PAnsiChar(AnsiString(windowName)), img);
  waitKey(0); // Attendre une touche
  destroyWindow(PAnsiChar(AnsiString(windowName)));
end;

// Avec temporisation
procedure ShowImageTimed(const windowName: string; const img: TMat; delayMs: Integer);
begin
  imshow(PAnsiChar(AnsiString(windowName)), img);
  waitKey(delayMs);
end;
```

### Accès aux pixels

```pascal
// Obtenir la valeur d'un pixel
function GetPixel(const img: TMat; x, y: Integer): TBGRPixel;
var
  ptr: PByte;
begin
  ptr := PByte(img.data);
  Inc(ptr, (y * img.cols + x) * img.channels);

  Result.blue := ptr^;
  Inc(ptr);
  Result.green := ptr^;
  Inc(ptr);
  Result.red := ptr^;
end;

// Définir la valeur d'un pixel
procedure SetPixel(var img: TMat; x, y: Integer; color: TBGRPixel);
var
  ptr: PByte;
begin
  ptr := PByte(img.data);
  Inc(ptr, (y * img.cols + x) * img.channels);

  ptr^ := color.blue;
  Inc(ptr);
  ptr^ := color.green;
  Inc(ptr);
  ptr^ := color.red;
end;
```

## Traitement d'images avec OpenCV

### Filtrage et lissage

#### Flou gaussien

Le flou gaussien est idéal pour réduire le bruit et les détails :

```pascal
procedure ApplyGaussianBlur(const src: TMat; var dst: TMat;
  kernelSize: Integer; sigma: Double);
begin
  // kernelSize doit être impair (3, 5, 7, 9, etc.)
  GaussianBlur(src, dst, Size(kernelSize, kernelSize), sigma);
end;

// Exemple : réduction du bruit
procedure ReduceNoise;
var
  img, blurred: TMat;
begin
  img := imread('noisy_image.jpg');
  try
    ApplyGaussianBlur(img, blurred, 5, 1.5);
    imwrite('denoised.jpg', blurred);
  finally
    img.release;
    blurred.release;
  end;
end;
```

#### Flou médian

Excellent pour éliminer le "sel et poivre" (bruit impulsionnel) :

```pascal
procedure ApplyMedianBlur(const src: TMat; var dst: TMat; kernelSize: Integer);
begin
  medianBlur(src, dst, kernelSize);
end;
```

#### Flou bilatéral

Conserve les contours tout en lissant les zones uniformes :

```pascal
procedure ApplyBilateralFilter(const src: TMat; var dst: TMat;
  diameter: Integer; sigmaColor, sigmaSpace: Double);
begin
  bilateralFilter(src, dst, diameter, sigmaColor, sigmaSpace);
end;
```

### Détection de contours

#### Filtre de Canny

L'algorithme de Canny est la méthode la plus populaire pour la détection de contours :

```pascal
procedure DetectEdgesCanny(const src: TMat; var edges: TMat;
  threshold1, threshold2: Double);
var
  gray: TMat;
begin
  // Convertir en niveaux de gris
  if src.channels > 1 then
    cvtColor(src, gray, CV_BGR2GRAY)
  else
    gray := src;

  // Appliquer Canny
  Canny(gray, edges, threshold1, threshold2);

  if gray.data <> src.data then
    gray.release;
end;

// Exemple d'utilisation
procedure EdgeDetectionExample;
var
  img, edges: TMat;
begin
  img := imread('photo.jpg');
  try
    // Seuils bas et haut pour Canny
    DetectEdgesCanny(img, edges, 50, 150);
    imwrite('edges.jpg', edges);
  finally
    img.release;
    edges.release;
  end;
end;
```

#### Filtre de Sobel

Détecte les gradients dans différentes directions :

```pascal
procedure DetectEdgesSobel(const src: TMat; var dst: TMat);
var
  gray, grad_x, grad_y, abs_grad_x, abs_grad_y: TMat;
begin
  // Convertir en niveaux de gris
  cvtColor(src, gray, CV_BGR2GRAY);

  // Gradient en X
  Sobel(gray, grad_x, CV_16S, 1, 0, 3);
  convertScaleAbs(grad_x, abs_grad_x);

  // Gradient en Y
  Sobel(gray, grad_y, CV_16S, 0, 1, 3);
  convertScaleAbs(grad_y, abs_grad_y);

  // Combiner les gradients
  addWeighted(abs_grad_x, 0.5, abs_grad_y, 0.5, 0, dst);

  // Libérer les ressources temporaires
  gray.release;
  grad_x.release;
  grad_y.release;
  abs_grad_x.release;
  abs_grad_y.release;
end;
```

### Transformations morphologiques

Les opérations morphologiques modifient la forme des objets dans l'image :

```pascal
type
  TMorphShape = (MORPH_RECT, MORPH_CROSS, MORPH_ELLIPSE);
  TMorphOp = (MORPH_ERODE, MORPH_DILATE, MORPH_OPEN, MORPH_CLOSE,
              MORPH_GRADIENT, MORPH_TOPHAT, MORPH_BLACKHAT);

procedure ApplyMorphology(const src: TMat; var dst: TMat;
  operation: TMorphOp; kernelSize: Integer; shape: TMorphShape);
var
  kernel: TMat;
begin
  // Créer l'élément structurant
  kernel := getStructuringElement(Ord(shape), Size(kernelSize, kernelSize));

  // Appliquer l'opération
  morphologyEx(src, dst, Ord(operation), kernel);

  kernel.release;
end;

// Exemples d'utilisation
procedure MorphologyExamples;
var
  img, result: TMat;
begin
  img := imread('binary_image.jpg', 0); // Charger en niveaux de gris
  try
    // Érosion : réduit les objets blancs
    ApplyMorphology(img, result, MORPH_ERODE, 5, MORPH_RECT);
    imwrite('eroded.jpg', result);
    result.release;

    // Dilatation : agrandit les objets blancs
    ApplyMorphology(img, result, MORPH_DILATE, 5, MORPH_RECT);
    imwrite('dilated.jpg', result);
    result.release;

    // Opening : érosion puis dilatation (élimine petits objets)
    ApplyMorphology(img, result, MORPH_OPEN, 5, MORPH_RECT);
    imwrite('opened.jpg', result);
    result.release;

    // Closing : dilatation puis érosion (remplit petits trous)
    ApplyMorphology(img, result, MORPH_CLOSE, 5, MORPH_RECT);
    imwrite('closed.jpg', result);
    result.release;
  finally
    img.release;
  end;
end;
```

### Seuillage (Thresholding)

Le seuillage convertit une image en niveaux de gris en image binaire :

```pascal
type
  TThresholdType = (THRESH_BINARY, THRESH_BINARY_INV, THRESH_TRUNC,
                    THRESH_TOZERO, THRESH_TOZERO_INV, THRESH_OTSU);

procedure ApplyThreshold(const src: TMat; var dst: TMat;
  threshValue: Double; maxValue: Double; threshType: TThresholdType);
var
  gray: TMat;
begin
  // Convertir en niveaux de gris si nécessaire
  if src.channels > 1 then
    cvtColor(src, gray, CV_BGR2GRAY)
  else
    gray := src;

  // Appliquer le seuillage
  threshold(gray, dst, threshValue, maxValue, Ord(threshType));

  if gray.data <> src.data then
    gray.release;
end;

// Seuillage adaptatif (pour éclairage non uniforme)
procedure ApplyAdaptiveThreshold(const src: TMat; var dst: TMat;
  maxValue: Double; blockSize: Integer; C: Double);
var
  gray: TMat;
begin
  if src.channels > 1 then
    cvtColor(src, gray, CV_BGR2GRAY)
  else
    gray := src;

  adaptiveThreshold(gray, dst, maxValue, ADAPTIVE_THRESH_GAUSSIAN_C,
    THRESH_BINARY, blockSize, C);

  if gray.data <> src.data then
    gray.release;
end;
```

## Détection d'objets

### Détection de formes géométriques

#### Détection de cercles (Transformée de Hough)

```pascal
type
  TCircle = record
    x, y: Integer;    // Centre
    radius: Integer;  // Rayon
  end;

function DetectCircles(const img: TMat; minRadius, maxRadius: Integer): TArray<TCircle>;
var
  gray, circles: TMat;
  i: Integer;
  circleData: PSingle;
  resultList: TList<TCircle>;
  circle: TCircle;
begin
  // Convertir en niveaux de gris
  cvtColor(img, gray, CV_BGR2GRAY);

  // Réduire le bruit
  GaussianBlur(gray, gray, Size(9, 9), 2);

  // Détecter les cercles
  HoughCircles(gray, circles, HOUGH_GRADIENT, 1, gray.rows div 8,
    200, 100, minRadius, maxRadius);

  // Extraire les résultats
  resultList := TList<TCircle>.Create;
  try
    if circles.cols > 0 then
    begin
      circleData := PSingle(circles.data);
      for i := 0 to circles.cols - 1 do
      begin
        circle.x := Round(circleData^);
        Inc(circleData);
        circle.y := Round(circleData^);
        Inc(circleData);
        circle.radius := Round(circleData^);
        Inc(circleData);
        resultList.Add(circle);
      end;
    end;
    Result := resultList.ToArray;
  finally
    resultList.Free;
    gray.release;
    circles.release;
  end;
end;

// Exemple : dessiner les cercles détectés
procedure DrawDetectedCircles;
var
  img, display: TMat;
  circles: TArray<TCircle>;
  i: Integer;
begin
  img := imread('coins.jpg');
  display := img.clone;
  try
    circles := DetectCircles(img, 20, 100);

    // Dessiner les cercles
    for i := 0 to High(circles) do
    begin
      circle(display, Point(circles[i].x, circles[i].y),
        circles[i].radius, Scalar(0, 255, 0), 2); // Cercle vert
      circle(display, Point(circles[i].x, circles[i].y),
        3, Scalar(0, 0, 255), -1); // Centre rouge
    end;

    imwrite('detected_circles.jpg', display);
  finally
    img.release;
    display.release;
  end;
end;
```

#### Détection de lignes

```pascal
type
  TLine = record
    x1, y1, x2, y2: Integer;
  end;

function DetectLines(const img: TMat; threshold: Integer): TArray<TLine>;
var
  gray, edges, lines: TMat;
  i: Integer;
  lineData: PInteger;
  resultList: TList<TLine>;
  line: TLine;
begin
  // Prétraitement
  cvtColor(img, gray, CV_BGR2GRAY);
  Canny(gray, edges, 50, 150);

  // Détection de lignes avec Hough probabiliste
  HoughLinesP(edges, lines, 1, CV_PI / 180, threshold, 50, 10);

  // Extraire les lignes
  resultList := TList<TLine>.Create;
  try
    if lines.rows > 0 then
    begin
      lineData := PInteger(lines.data);
      for i := 0 to lines.rows - 1 do
      begin
        line.x1 := lineData^; Inc(lineData);
        line.y1 := lineData^; Inc(lineData);
        line.x2 := lineData^; Inc(lineData);
        line.y2 := lineData^; Inc(lineData);
        resultList.Add(line);
      end;
    end;
    Result := resultList.ToArray;
  finally
    resultList.Free;
    gray.release;
    edges.release;
    lines.release;
  end;
end;
```

### Détection de contours et d'objets

```pascal
type
  TContour = array of TPoint;
  TContours = array of TContour;

function FindContours(const img: TMat; mode, method: Integer): TContours;
var
  binary: TMat;
  contours: TSeqOfSeq;
  hierarchy: TMat;
  i, j: Integer;
  resultList: TList<TContour>;
  contour: TContour;
  seq: TSeq;
  pt: PPoint;
begin
  // Binariser l'image
  if img.channels > 1 then
    cvtColor(img, binary, CV_BGR2GRAY)
  else
    binary := img.clone;

  threshold(binary, binary, 127, 255, THRESH_BINARY);

  // Trouver les contours
  findContours(binary, contours, hierarchy, mode, method);

  // Convertir en tableau Pascal
  resultList := TList<TContour>.Create;
  try
    for i := 0 to contours.Count - 1 do
    begin
      seq := contours.GetSeq(i);
      SetLength(contour, seq.total);

      pt := PPoint(seq.data);
      for j := 0 to seq.total - 1 do
      begin
        contour[j] := pt^;
        Inc(pt);
      end;

      resultList.Add(contour);
    end;
    Result := resultList.ToArray;
  finally
    resultList.Free;
    binary.release;
    hierarchy.release;
  end;
end;

// Calculer l'aire et le périmètre d'un contour
function ContourArea(const contour: TContour): Double;
begin
  Result := cv.contourArea(contour);
end;

function ContourPerimeter(const contour: TContour; closed: Boolean = True): Double;
begin
  Result := cv.arcLength(contour, closed);
end;

// Approximer un contour (simplification)
function ApproximateContour(const contour: TContour; epsilon: Double): TContour;
var
  approx: TContour;
begin
  cv.approxPolyDP(contour, approx, epsilon, True);
  Result := approx;
end;
```

## Détection faciale

### Utilisation de classificateurs Haar Cascade

OpenCV inclut des classificateurs pré-entraînés pour détecter visages, yeux, sourires, etc.

```pascal
type
  TFaceDetector = class
  private
    FCascade: TCascadeClassifier;
    FInitialized: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function LoadCascade(const filename: string): Boolean;
    function DetectFaces(const img: TMat; scaleFactor: Double = 1.1;
      minNeighbors: Integer = 3): TArray<TRect>;
  end;

constructor TFaceDetector.Create;
begin
  inherited;
  FCascade := TCascadeClassifier.Create;
  FInitialized := False;
end;

destructor TFaceDetector.Destroy;
begin
  FCascade.Free;
  inherited;
end;

function TFaceDetector.LoadCascade(const filename: string): Boolean;
begin
  FInitialized := FCascade.load(PAnsiChar(AnsiString(filename)));
  Result := FInitialized;
end;

function TFaceDetector.DetectFaces(const img: TMat; scaleFactor: Double;
  minNeighbors: Integer): TArray<TRect>;
var
  gray: TMat;
  faces: TVectorOfRect;
  i: Integer;
begin
  if not FInitialized then
    raise Exception.Create('Cascade non chargé');

  // Convertir en niveaux de gris
  if img.channels > 1 then
    cvtColor(img, gray, CV_BGR2GRAY)
  else
    gray := img;

  // Égaliser l'histogramme pour améliorer la détection
  equalizeHist(gray, gray);

  // Détecter les visages
  FCascade.detectMultiScale(gray, faces, scaleFactor, minNeighbors,
    0, Size(30, 30), Size(0, 0));

  // Convertir en tableau
  SetLength(Result, faces.size);
  for i := 0 to faces.size - 1 do
    Result[i] := faces.at(i);

  gray.release;
end;

// Exemple d'utilisation
procedure DetectFacesExample;
var
  detector: TFaceDetector;
  img, display: TMat;
  faces: TArray<TRect>;
  i: Integer;
  cascadePath: string;
begin
  {$IFDEF WINDOWS}
  cascadePath := 'C:\opencv\data\haarcascades\haarcascade_frontalface_default.xml';
  {$ELSE}
  cascadePath := '/usr/share/opencv4/haarcascades/haarcascade_frontalface_default.xml';
  {$ENDIF}

  detector := TFaceDetector.Create;
  try
    if not detector.LoadCascade(cascadePath) then
      raise Exception.Create('Impossible de charger le cascade');

    img := imread('people.jpg');
    display := img.clone;
    try
      // Détecter les visages
      faces := detector.DetectFaces(img);

      WriteLn(Format('Visages détectés : %d', [Length(faces)]));

      // Dessiner des rectangles autour des visages
      for i := 0 to High(faces) do
      begin
        rectangle(display, faces[i], Scalar(0, 255, 0), 2);
      end;

      imwrite('detected_faces.jpg', display);
      imshow('Détection faciale', display);
      waitKey(0);
    finally
      img.release;
      display.release;
    end;
  finally
    detector.Free;
  end;
end;
```

### Détection des yeux

```pascal
procedure DetectFacesAndEyes;
var
  faceCascade, eyeCascade: TCascadeClassifier;
  img, gray, faceROI: TMat;
  faces, eyes: TVectorOfRect;
  i, j: Integer;
  faceRect, eyeRect: TRect;
begin
  faceCascade := TCascadeClassifier.Create;
  eyeCascade := TCascadeClassifier.Create;
  try
    // Charger les cascades
    faceCascade.load('haarcascade_frontalface_default.xml');
    eyeCascade.load('haarcascade_eye.xml');

    img := imread('portrait.jpg');
    cvtColor(img, gray, CV_BGR2GRAY);
    equalizeHist(gray, gray);

    // Détecter les visages
    faceCascade.detectMultiScale(gray, faces, 1.1, 3);

    // Pour chaque visage, détecter les yeux
    for i := 0 to faces.size - 1 do
    begin
      faceRect := faces.at(i);

      // Dessiner le rectangle du visage
      rectangle(img, faceRect, Scalar(255, 0, 0), 2);

      // Extraire la région du visage
      faceROI := Mat(gray, faceRect);

      // Détecter les yeux dans cette région
      eyeCascade.detectMultiScale(faceROI, eyes);

      // Dessiner les yeux
      for j := 0 to eyes.size - 1 do
      begin
        eyeRect := eyes.at(j);
        // Ajuster les coordonnées par rapport à l'image complète
        eyeRect.x := eyeRect.x + faceRect.x;
        eyeRect.y := eyeRect.y + faceRect.y;
        rectangle(img, eyeRect, Scalar(0, 255, 0), 2);
      end;
    end;

    imwrite('faces_and_eyes.jpg', img);
  finally
    faceCascade.Free;
    eyeCascade.Free;
    img.release;
    gray.release;
  end;
end;
```

## Traitement vidéo

### Capture depuis une webcam

```pascal
type
  TVideoCapture = class
  private
    FCapture: TOpenCVCapture;
    FOpened: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function Open(deviceIndex: Integer = 0): Boolean;
    function OpenFile(const filename: string): Boolean;
    function Read(var frame: TMat): Boolean;
    function IsOpened: Boolean;
    procedure Release;
    function GetProperty(propId: Integer): Double;
    procedure SetProperty(propId: Integer; value: Double);
  end;

constructor TVideoCapture.Create;
begin
  inherited;
  FCapture := cv.VideoCapture_Create;
  FOpened := False;
end;

function TVideoCapture.Open(deviceIndex: Integer): Boolean;
begin
  FOpened := cv.VideoCapture_Open(FCapture, deviceIndex);
  Result := FOpened;
end;

function TVideoCapture.Read(var frame: TMat): Boolean;
begin
  Result := cv.VideoCapture_Read(FCapture, frame);
end;

// Exemple : capture et traitement en temps réel
procedure LiveVideoProcessing;
var
  cap: TVideoCapture;
  frame, processed: TMat;
  key: Integer;
begin
  cap := TVideoCapture.Create;
  try
    if not cap.Open(0) then // 0 = webcam par défaut
    begin
      WriteLn('Impossible d''ouvrir la webcam');
      Exit;
    end;

    WriteLn('Appuyez sur ESC pour quitter');

    namedWindow('Webcam', WINDOW_AUTOSIZE);

    while True do
    begin
      // Lire une frame
      if not cap.Read(frame) then
        Break;

      // Traiter la frame (exemple : détection de contours)
      Canny(frame, processed, 50, 150);
      cvtColor(processed, processed, CV_GRAY2BGR);

      // Afficher
      imshow('Webcam', processed);

      // Vérifier si l'utilisateur veut quitter (ESC = 27)
      key := waitKey(1);
      if key = 27 then
        Break;
    end;

    destroyAllWindows;
  finally
    cap.Free;
    frame.release;
    processed.release;
  end;
end;
```

### Enregistrement vidéo

```pascal
type
  TVideoWriter = class
  private
    FWriter: TOpenCVVideoWriter;
  public
    constructor Create;
    destructor Destroy; override;
    function Open(const filename: string; fourcc: Integer;
      fps: Double; frameSize: TSize; isColor: Boolean = True): Boolean;
    procedure Write(const frame: TMat);
    procedure Release;
  end;

constructor TVideoWriter.Create;
begin
  inherited;
  FWriter := cv.VideoWriter_Create;
end;

function TVideoWriter.Open(const filename: string; fourcc: Integer;
  fps: Double; frameSize: TSize; isColor: Boolean): Boolean;
begin
  Result := cv.VideoWriter_Open(FWriter, PAnsiChar(AnsiString(filename)),
    fourcc, fps, frameSize, isColor);
end;

procedure TVideoWriter.Write(const frame: TMat);
begin
  cv.VideoWriter_Write(FWriter, frame);
end;

// Codes FOURCC courants
function MakeFourCC(c1, c2, c3, c4: Char): Integer;
begin
  Result := Ord(c1) or (Ord(c2) shl 8) or (Ord(c3) shl 16) or (Ord(c4) shl 24);
end;

const
  FOURCC_MJPG = $47504A4D;  // Motion JPEG
  FOURCC_XVID = $44495658;  // XVID
  FOURCC_MP4V = $5634504D;  // MPEG-4
  FOURCC_H264 = $34363248;  // H.264

// Exemple : enregistrer une vidéo depuis la webcam
procedure RecordWebcam;
var
  cap: TVideoCapture;
  writer: TVideoWriter;
  frame: TMat;
  key: Integer;
  fps: Double;
  frameSize: TSize;
begin
  cap := TVideoCapture.Create;
  writer := TVideoWriter.Create;
  try
    if not cap.Open(0) then
    begin
      WriteLn('Impossible d''ouvrir la webcam');
      Exit;
    end;

    // Obtenir les propriétés de la caméra
    fps := cap.GetProperty(CAP_PROP_FPS);
    if fps = 0 then fps := 30.0; // Par défaut

    frameSize.width := Round(cap.GetProperty(CAP_PROP_FRAME_WIDTH));
    frameSize.height := Round(cap.GetProperty(CAP_PROP_FRAME_HEIGHT));

    // Ouvrir le fichier de sortie
    {$IFDEF WINDOWS}
    if not writer.Open('output.avi', FOURCC_MJPG, fps, frameSize) then
    {$ELSE}
    if not writer.Open('output.avi', FOURCC_XVID, fps, frameSize) then
    {$ENDIF}
    begin
      WriteLn('Impossible de créer le fichier vidéo');
      Exit;
    end;

    WriteLn('Enregistrement en cours... Appuyez sur ESC pour arrêter');

    namedWindow('Enregistrement', WINDOW_AUTOSIZE);

    while True do
    begin
      if not cap.Read(frame) then
        Break;

      // Écrire la frame
      writer.Write(frame);

      // Afficher
      imshow('Enregistrement', frame);

      key := waitKey(1);
      if key = 27 then
        Break;
    end;

    WriteLn('Enregistrement terminé');
    destroyAllWindows;
  finally
    cap.Free;
    writer.Free;
    frame.release;
  end;
end;
```

### Analyse de vidéo existante

```pascal
procedure AnalyzeVideo(const filename: string);
var
  cap: TVideoCapture;
  frame: TMat;
  frameCount, currentFrame: Integer;
  fps: Double;
  totalTime: Double;
begin
  cap := TVideoCapture.Create;
  try
    if not cap.OpenFile(filename) then
    begin
      WriteLn('Impossible d''ouvrir la vidéo');
      Exit;
    end;

    // Informations sur la vidéo
    frameCount := Round(cap.GetProperty(CAP_PROP_FRAME_COUNT));
    fps := cap.GetProperty(CAP_PROP_FPS);
    totalTime := frameCount / fps;

    WriteLn(Format('Vidéo : %s', [filename]));
    WriteLn(Format('Frames : %d', [frameCount]));
    WriteLn(Format('FPS : %.2f', [fps]));
    WriteLn(Format('Durée : %.2f secondes', [totalTime]));

    currentFrame := 0;
    while cap.Read(frame) do
    begin
      Inc(currentFrame);

      // Traiter chaque frame
      // (exemple : détection d'objets, analyse de mouvement, etc.)

      // Afficher la progression
      if currentFrame mod 30 = 0 then
        Write(Format(#13'Progression : %d/%d', [currentFrame, frameCount]));
    end;

    WriteLn;
    WriteLn('Analyse terminée');
  finally
    cap.Free;
    frame.release;
  end;
end;
```

## Suivi d'objets (Object Tracking)

### Suivi par détection de couleur

```pascal
type
  TColorTracker = class
  private
    FLowerBound: TScalar;
    FUpperBound: TScalar;
  public
    procedure SetColorRange(hMin, hMax, sMin, sMax, vMin, vMax: Integer);
    function Track(const frame: TMat): TRect;
  end;

procedure TColorTracker.SetColorRange(hMin, hMax, sMin, sMax, vMin, vMax: Integer);
begin
  FLowerBound := Scalar(hMin, sMin, vMin);
  FUpperBound := Scalar(hMax, sMax, vMax);
end;

function TColorTracker.Track(const frame: TMat): TRect;
var
  hsv, mask: TMat;
  contours: TContours;
  i, maxIndex: Integer;
  maxArea, area: Double;
begin
  // Convertir en HSV
  cvtColor(frame, hsv, CV_BGR2HSV);

  // Créer un masque avec la plage de couleurs
  inRange(hsv, FLowerBound, FUpperBound, mask);

  // Morphologie pour nettoyer le masque
  erode(mask, mask, Mat, Point(-1, -1), 2);
  dilate(mask, mask, Mat, Point(-1, -1), 2);

  // Trouver les contours
  contours := FindContours(mask, RETR_EXTERNAL, CHAIN_APPROX_SIMPLE);

  // Trouver le plus grand contour
  Result := Rect(0, 0, 0, 0);
  if Length(contours) > 0 then
  begin
    maxArea := 0;
    maxIndex := -1;

    for i := 0 to High(contours) do
    begin
      area := ContourArea(contours[i]);
      if area > maxArea then
      begin
        maxArea := area;
        maxIndex := i;
      end;
    end;

    if maxIndex >= 0 then
      Result := boundingRect(contours[maxIndex]);
  end;

  hsv.release;
  mask.release;
end;

// Exemple : suivre un objet rouge
procedure TrackRedObject;
var
  cap: TVideoCapture;
  tracker: TColorTracker;
  frame: TMat;
  objectRect: TRect;
  key: Integer;
begin
  cap := TVideoCapture.Create;
  tracker := TColorTracker.Create;
  try
    if not cap.Open(0) then
    begin
      WriteLn('Impossible d''ouvrir la webcam');
      Exit;
    end;

    // Plage pour la couleur rouge en HSV
    tracker.SetColorRange(0, 10, 100, 255, 100, 255);

    namedWindow('Suivi', WINDOW_AUTOSIZE);

    while True do
    begin
      if not cap.Read(frame) then
        Break;

      // Suivre l'objet
      objectRect := tracker.Track(frame);

      // Dessiner le rectangle si objet détecté
      if (objectRect.width > 0) and (objectRect.height > 0) then
        rectangle(frame, objectRect, Scalar(0, 255, 0), 2);

      imshow('Suivi', frame);

      key := waitKey(1);
      if key = 27 then
        Break;
    end;

    destroyAllWindows;
  finally
    cap.Free;
    tracker.Free;
    frame.release;
  end;
end;
```

### Algorithmes de suivi avancés

OpenCV propose plusieurs algorithmes de suivi d'objets :

```pascal
type
  TTrackerType = (ttBOOSTING, ttMIL, ttKCF, ttTLD, ttMEDIANFLOW, ttMOSSE, ttCSRT);

function CreateTracker(trackerType: TTrackerType): TTracker;
begin
  case trackerType of
    ttBOOSTING: Result := TrackerBoosting_create;
    ttMIL: Result := TrackerMIL_create;
    ttKCF: Result := TrackerKCF_create;
    ttTLD: Result := TrackerTLD_create;
    ttMEDIANFLOW: Result := TrackerMedianFlow_create;
    ttMOSSE: Result := TrackerMOSSE_create;
    ttCSRT: Result := TrackerCSRT_create;
  else
    Result := nil;
  end;
end;

procedure TrackObjectWithAlgorithm;
var
  cap: TVideoCapture;
  tracker: TTracker;
  frame: TMat;
  bbox: TRect;
  ok: Boolean;
  key: Integer;
begin
  cap := TVideoCapture.Create;
  try
    if not cap.Open(0) then
      Exit;

    // Lire la première frame
    cap.Read(frame);

    // Sélectionner la région à suivre
    bbox := selectROI('Sélectionnez l''objet', frame);

    // Créer le tracker
    tracker := CreateTracker(ttKCF); // KCF est rapide et efficace

    // Initialiser le tracker
    tracker.init(frame, bbox);

    while True do
    begin
      if not cap.Read(frame) then
        Break;

      // Mettre à jour le tracker
      ok := tracker.update(frame, bbox);

      if ok then
        rectangle(frame, bbox, Scalar(0, 255, 0), 2)
      else
        putText(frame, 'Suivi perdu', Point(10, 30),
          FONT_HERSHEY_SIMPLEX, 0.7, Scalar(0, 0, 255), 2);

      imshow('Suivi', frame);

      key := waitKey(1);
      if key = 27 then
        Break;
    end;

    destroyAllWindows;
  finally
    cap.Free;
    tracker.release;
    frame.release;
  end;
end;
```

## Détection de mouvement

### Différence entre frames

```pascal
type
  TMotionDetector = class
  private
    FPreviousFrame: TMat;
    FThreshold: Integer;
  public
    constructor Create(threshold: Integer = 25);
    destructor Destroy; override;
    function DetectMotion(const currentFrame: TMat): TMat;
    function GetMotionRegions(const motionMask: TMat): TArray<TRect>;
  end;

constructor TMotionDetector.Create(threshold: Integer);
begin
  inherited Create;
  FThreshold := threshold;
end;

destructor TMotionDetector.Destroy;
begin
  if FPreviousFrame.data <> nil then
    FPreviousFrame.release;
  inherited;
end;

function TMotionDetector.DetectMotion(const currentFrame: TMat): TMat;
var
  gray, diff: TMat;
begin
  // Convertir en niveaux de gris
  cvtColor(currentFrame, gray, CV_BGR2GRAY);
  GaussianBlur(gray, gray, Size(21, 21), 0);

  // Première frame : initialisation
  if FPreviousFrame.data = nil then
  begin
    FPreviousFrame := gray.clone;
    Result := Mat.zeros(gray.rows, gray.cols, CV_8UC1);
    gray.release;
    Exit;
  end;

  // Calculer la différence absolue
  absdiff(FPreviousFrame, gray, diff);

  // Seuillage pour obtenir les zones de mouvement
  threshold(diff, Result, FThreshold, 255, THRESH_BINARY);

  // Morphologie pour remplir les trous
  dilate(Result, Result, Mat, Point(-1, -1), 2);

  // Mettre à jour la frame précédente
  FPreviousFrame.release;
  FPreviousFrame := gray.clone;

  gray.release;
  diff.release;
end;

function TMotionDetector.GetMotionRegions(const motionMask: TMat): TArray<TRect>;
var
  contours: TContours;
  i: Integer;
  resultList: TList<TRect>;
  area: Double;
  rect: TRect;
begin
  contours := FindContours(motionMask, RETR_EXTERNAL, CHAIN_APPROX_SIMPLE);

  resultList := TList<TRect>.Create;
  try
    for i := 0 to High(contours) do
    begin
      area := ContourArea(contours[i]);

      // Ignorer les petites zones (bruit)
      if area > 500 then
      begin
        rect := boundingRect(contours[i]);
        resultList.Add(rect);
      end;
    end;

    Result := resultList.ToArray;
  finally
    resultList.Free;
  end;
end;

// Exemple : détection de mouvement en temps réel
procedure LiveMotionDetection;
var
  cap: TVideoCapture;
  detector: TMotionDetector;
  frame, motionMask: TMat;
  regions: TArray<TRect>;
  i, key: Integer;
begin
  cap := TVideoCapture.Create;
  detector := TMotionDetector.Create(25);
  try
    if not cap.Open(0) then
      Exit;

    namedWindow('Vidéo', WINDOW_AUTOSIZE);
    namedWindow('Mouvement', WINDOW_AUTOSIZE);

    WriteLn('Détection de mouvement active. ESC pour quitter.');

    while True do
    begin
      if not cap.Read(frame) then
        Break;

      // Détecter le mouvement
      motionMask := detector.DetectMotion(frame);
      regions := detector.GetMotionRegions(motionMask);

      // Dessiner les régions de mouvement
      for i := 0 to High(regions) do
        rectangle(frame, regions[i], Scalar(0, 255, 0), 2);

      // Afficher le nombre de zones détectées
      putText(frame, Format('Zones: %d', [Length(regions)]),
        Point(10, 30), FONT_HERSHEY_SIMPLEX, 1, Scalar(0, 255, 0), 2);

      imshow('Vidéo', frame);
      imshow('Mouvement', motionMask);

      key := waitKey(1);
      if key = 27 then
        Break;
    end;

    destroyAllWindows;
  finally
    cap.Free;
    detector.Free;
    frame.release;
    motionMask.release;
  end;
end;
```

### Soustraction de fond (Background Subtraction)

```pascal
type
  TBackgroundSubtractor = class
  private
    FSubtractor: TBackgroundSubtractorMOG2;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Apply(const frame: TMat; var fgMask: TMat; learningRate: Double = -1);
    function GetBackgroundImage: TMat;
  end;

constructor TBackgroundSubtractor.Create;
begin
  inherited;
  // MOG2 : méthode basée sur des mélanges de gaussiennes
  FSubtractor := createBackgroundSubtractorMOG2(500, 16, True);
end;

destructor TBackgroundSubtractor.Destroy;
begin
  FSubtractor.release;
  inherited;
end;

procedure TBackgroundSubtractor.Apply(const frame: TMat; var fgMask: TMat;
  learningRate: Double);
begin
  FSubtractor.apply(frame, fgMask, learningRate);
end;

function TBackgroundSubtractor.GetBackgroundImage: TMat;
begin
  FSubtractor.getBackgroundImage(Result);
end;

// Exemple : suivi d'objets en mouvement avec soustraction de fond
procedure BackgroundSubtractionDemo;
var
  cap: TVideoCapture;
  subtractor: TBackgroundSubtractor;
  frame, fgMask, background: TMat;
  contours: TContours;
  i, key: Integer;
  rect: TRect;
begin
  cap := TVideoCapture.Create;
  subtractor := TBackgroundSubtractor.Create;
  try
    if not cap.Open(0) then
      Exit;

    namedWindow('Vidéo', WINDOW_AUTOSIZE);
    namedWindow('Premier plan', WINDOW_AUTOSIZE);

    while True do
    begin
      if not cap.Read(frame) then
        Break;

      // Appliquer la soustraction de fond
      subtractor.Apply(frame, fgMask);

      // Nettoyer le masque
      erode(fgMask, fgMask, Mat, Point(-1, -1), 2);
      dilate(fgMask, fgMask, Mat, Point(-1, -1), 2);

      // Trouver les contours des objets en mouvement
      contours := FindContours(fgMask, RETR_EXTERNAL, CHAIN_APPROX_SIMPLE);

      // Dessiner les rectangles
      for i := 0 to High(contours) do
      begin
        if ContourArea(contours[i]) > 500 then
        begin
          rect := boundingRect(contours[i]);
          rectangle(frame, rect, Scalar(0, 255, 0), 2);
        end;
      end;

      imshow('Vidéo', frame);
      imshow('Premier plan', fgMask);

      key := waitKey(30);
      if key = 27 then
        Break;
    end;

    destroyAllWindows;
  finally
    cap.Free;
    subtractor.Free;
    frame.release;
    fgMask.release;
  end;
end;
```

## Reconnaissance optique de caractères (OCR)

### Prétraitement pour OCR

```pascal
procedure PrepareImageForOCR(const src: TMat; var dst: TMat);
var
  gray, thresh: TMat;
begin
  // Convertir en niveaux de gris
  if src.channels > 1 then
    cvtColor(src, gray, CV_BGR2GRAY)
  else
    gray := src.clone;

  // Redimensionner si trop petit
  if (gray.cols < 300) or (gray.rows < 300) then
    resize(gray, gray, Size(0, 0), 2.0, 2.0, INTER_CUBIC);

  // Débruitage
  fastNlMeansDenoising(gray, gray, 10);

  // Seuillage adaptatif
  adaptiveThreshold(gray, thresh, 255, ADAPTIVE_THRESH_GAUSSIAN_C,
    THRESH_BINARY, 11, 2);

  dst := thresh;
  gray.release;
end;
```

### Intégration avec Tesseract OCR

```pascal
// Note: Nécessite tesseract-ocr installé sur le système
{$IFDEF WINDOWS}
const
  TESSERACT_PATH = 'C:\Program Files\Tesseract-OCR\';
{$ELSE}
const
  TESSERACT_PATH = '/usr/bin/';
{$ENDIF}

procedure ExtractTextFromImage(const imagePath: string);
var
  img, processed: TMat;
  outputFile: string;
  textFile: TextFile;
  line: string;
begin
  img := imread(imagePath);
  try
    // Prétraiter l'image
    PrepareImageForOCR(img, processed);

    // Sauvegarder l'image prétraitée
    imwrite('temp_ocr.png', processed);

    // Appeler Tesseract via ligne de commande
    outputFile := 'ocr_output';
    {$IFDEF WINDOWS}
    ExecuteProcess(TESSERACT_PATH + 'tesseract.exe',
      ['temp_ocr.png', outputFile, '-l', 'fra']);
    {$ELSE}
    ExecuteProcess('/usr/bin/tesseract',
      ['temp_ocr.png', outputFile, '-l', 'fra']);
    {$ENDIF}

    // Lire le résultat
    AssignFile(textFile, outputFile + '.txt');
    Reset(textFile);
    try
      while not Eof(textFile) do
      begin
        ReadLn(textFile, line);
        WriteLn(line);
      end;
    finally
      CloseFile(textFile);
    end;
  finally
    img.release;
    processed.release;
    DeleteFile('temp_ocr.png');
    DeleteFile(outputFile + '.txt');
  end;
end;
```

## Stéréovision et profondeur

### Carte de disparité

```pascal
type
  TStereoMatcher = class
  private
    FStereo: TStereoBM;
  public
    constructor Create(numDisparities: Integer = 16; blockSize: Integer = 15);
    destructor Destroy; override;
    procedure ComputeDisparity(const left, right: TMat; var disparity: TMat);
  end;

constructor TStereoMatcher.Create(numDisparities, blockSize: Integer);
begin
  inherited Create;
  FStereo := StereoBM_create(numDisparities, blockSize);
end;

destructor TStereoMatcher.Destroy;
begin
  FStereo.release;
  inherited;
end;

procedure TStereoMatcher.ComputeDisparity(const left, right: TMat;
  var disparity: TMat);
var
  leftGray, rightGray: TMat;
begin
  // Convertir en niveaux de gris
  if left.channels > 1 then
    cvtColor(left, leftGray, CV_BGR2GRAY)
  else
    leftGray := left.clone;

  if right.channels > 1 then
    cvtColor(right, rightGray, CV_BGR2GRAY)
  else
    rightGray := right.clone;

  // Calculer la disparité
  FStereo.compute(leftGray, rightGray, disparity);

  leftGray.release;
  rightGray.release;
end;

// Exemple avec deux caméras
procedure StereoVisionDemo;
var
  capLeft, capRight: TVideoCapture;
  stereo: TStereoMatcher;
  frameLeft, frameRight, disparity, dispNorm: TMat;
  key: Integer;
begin
  capLeft := TVideoCapture.Create;
  capRight := TVideoCapture.Create;
  stereo := TStereoMatcher.Create(64, 15);
  try
    // Ouvrir les deux caméras
    if not capLeft.Open(0) or not capRight.Open(1) then
    begin
      WriteLn('Impossible d''ouvrir les caméras');
      Exit;
    end;

    namedWindow('Gauche', WINDOW_AUTOSIZE);
    namedWindow('Droite', WINDOW_AUTOSIZE);
    namedWindow('Profondeur', WINDOW_AUTOSIZE);

    while True do
    begin
      if not capLeft.Read(frameLeft) or not capRight.Read(frameRight) then
        Break;

      // Calculer la carte de disparité
      stereo.ComputeDisparity(frameLeft, frameRight, disparity);

      // Normaliser pour l'affichage
      normalize(disparity, dispNorm, 0, 255, NORM_MINMAX, CV_8U);

      imshow('Gauche', frameLeft);
      imshow('Droite', frameRight);
      imshow('Profondeur', dispNorm);

      key := waitKey(1);
      if key = 27 then
        Break;
    end;

    destroyAllWindows;
  finally
    capLeft.Free;
    capRight.Free;
    stereo.Free;
    frameLeft.release;
    frameRight.release;
    disparity.release;
    dispNorm.release;
  end;
end;
```

## Optimisation et performance

### Traitement multi-threadé

```pascal
uses
  MTProcs;

procedure ParallelFrameProcessing(frames: TArray<TMat>);
begin
  ProcThreadPool.DoParallelLocalProc(
    procedure(Index: PtrInt; ThreadIndex: Integer; Data: Pointer)
    var
      processed: TMat;
    begin
      // Traiter chaque frame indépendamment
      Canny(frames[Index], processed, 50, 150);
      imwrite(Format('frame_%d.jpg', [Index]), processed);
      processed.release;
    end,
    0, High(frames)
  );
end;
```

### Utilisation du GPU (si disponible)

```pascal
{$IFDEF USE_CUDA}
uses
  cv.cuda;

procedure ProcessWithGPU(const src: TMat; var dst: TMat);
var
  d_src, d_dst: TGpuMat;
begin
  // Uploader vers le GPU
  d_src.upload(src);

  // Traitement sur GPU
  cv.cuda.GaussianBlur(d_src, d_dst, Size(15, 15), 0);

  // Télécharger le résultat
  d_dst.download(dst);

  d_src.release;
  d_dst.release;
end;
{$ENDIF}
```

### Optimisation des performances

```pascal
// Conseils d'optimisation :

// 1. Réutiliser les matrices au lieu de les recréer
procedure OptimizedProcessing;
var
  frame, gray, result: TMat;
  i: Integer;
begin
  // Créer les matrices une seule fois
  gray := Mat.create;
  result := Mat.create;

  for i := 0 to 1000 do
  begin
    frame := imread(Format('frame_%d.jpg', [i]));

    // Réutiliser gray et result
    cvtColor(frame, gray, CV_BGR2GRAY);
    GaussianBlur(gray, result, Size(5, 5), 0);

    imwrite(Format('output_%d.jpg', [i]), result);
    frame.release;
  end;

  gray.release;
  result.release;
end;

// 2. Réduire la résolution quand c'est possible
procedure ProcessLowRes(const src: TMat; var dst: TMat);
var
  small, processed: TMat;
begin
  // Réduire à 50%
  resize(src, small, Size(0, 0), 0.5, 0.5, INTER_LINEAR);

  // Traiter
  GaussianBlur(small, processed, Size(5, 5), 0);

  // Retour à la taille originale
  resize(processed, dst, src.size, 0, 0, INTER_LINEAR);

  small.release;
  processed.release;
end;

// 3. Utiliser des régions d'intérêt (ROI)
procedure ProcessROI(const src: TMat);
var
  roi: TMat;
  roiRect: TRect;
begin
  roiRect := Rect(100, 100, 200, 200);
  roi := Mat(src, roiRect);

  // Traiter seulement la ROI
  GaussianBlur(roi, roi, Size(5, 5), 0);

  // Les modifications sont appliquées directement à src
  // Pas besoin de release car roi est une vue
end;

// 4. Mesurer les performances
procedure BenchmarkOperation;
var
  img: TMat;
  startTime, endTime: TDateTime;
  duration: Double;
begin
  img := imread('test.jpg');
  try
    startTime := Now;

    // Opération à mesurer
    GaussianBlur(img, img, Size(15, 15), 0);

    endTime := Now;
    duration := (endTime - startTime) * 24 * 60 * 60 * 1000; // en ms

    WriteLn(Format('Durée : %.2f ms', [duration]));
  finally
    img.release;
  end;
end;
```

## Apprentissage automatique avec OpenCV

### Classification avec SVM (Support Vector Machine)

```pascal
type
  TSVMClassifier = class
  private
    FSVM: TSVM;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Train(const trainData, labels: TMat);
    function Predict(const sample: TMat): Single;
    procedure SaveModel(const filename: string);
    procedure LoadModel(const filename: string);
  end;

constructor TSVMClassifier.Create;
begin
  inherited;
  FSVM := SVM_create;

  // Configuration du SVM
  FSVM.setType(SVM_C_SVC);
  FSVM.setKernel(SVM_LINEAR);
  FSVM.setTermCriteria(TermCriteria(TermCriteria_MAX_ITER, 100, 1e-6));
end;

destructor TSVMClassifier.Destroy;
begin
  FSVM.release;
  inherited;
end;

procedure TSVMClassifier.Train(const trainData, labels: TMat);
var
  trainDataPtr: TTrainDataPtr;
begin
  trainDataPtr := TrainData_create(trainData, ROW_SAMPLE, labels);
  try
    FSVM.train(trainDataPtr);
    WriteLn('Entraînement terminé');
  finally
    trainDataPtr.release;
  end;
end;

function TSVMClassifier.Predict(const sample: TMat): Single;
begin
  Result := FSVM.predict(sample);
end;

procedure TSVMClassifier.SaveModel(const filename: string);
begin
  FSVM.save(PAnsiChar(AnsiString(filename)));
end;

procedure TSVMClassifier.LoadModel(const filename: string);
begin
  FSVM := Algorithm.load_SVM(PAnsiChar(AnsiString(filename)));
end;

// Exemple : Classification de chiffres manuscrits
procedure TrainDigitClassifier;
var
  classifier: TSVMClassifier;
  trainData, labels: TMat;
  i, j: Integer;
  img: TMat;
  sample: array of Single;
  classLabel: Integer;  { label est un mot réservé en Pascal }
begin
  classifier := TSVMClassifier.Create;
  try
    // Charger les données d'entraînement
    // (Exemple simplifié - en pratique, utiliser un dataset comme MNIST)
    SetLength(sample, 784); // 28x28 pixels

    trainData := Mat.create(100, 784, CV_32F); // 100 échantillons
    labels := Mat.create(100, 1, CV_32S);

    // Remplir avec des données d'exemple
    for i := 0 to 99 do
    begin
      img := imread(Format('digit_%d.png', [i]), 0);

      // Convertir l'image en vecteur
      for j := 0 to 783 do
        sample[j] := img.data[j];

      // Copier dans trainData
      Move(sample[0], trainData.data[i * 784], 784 * SizeOf(Single));

      // Label (0-9)
      classLabel := i mod 10;
      PInteger(labels.data)[i] := classLabel;

      img.release;
    end;

    // Entraîner
    classifier.Train(trainData, labels);

    // Sauvegarder le modèle
    classifier.SaveModel('digit_classifier.xml');

    WriteLn('Modèle entraîné et sauvegardé');
  finally
    classifier.Free;
    trainData.release;
    labels.release;
  end;
end;

// Utiliser le classificateur
procedure ClassifyDigit(const imagePath: string);
var
  classifier: TSVMClassifier;
  img: TMat;
  sample: TMat;
  result: Single;
begin
  classifier := TSVMClassifier.Create;
  try
    classifier.LoadModel('digit_classifier.xml');

    img := imread(imagePath, 0);

    // Préparer l'échantillon
    resize(img, img, Size(28, 28));
    sample := img.reshape(1, 1); // Transformer en ligne
    sample.convertTo(sample, CV_32F);

    // Prédire
    result := classifier.Predict(sample);

    WriteLn(Format('Chiffre prédit : %d', [Round(result)]));

    img.release;
    sample.release;
  finally
    classifier.Free;
  end;
end;
```

### K-Nearest Neighbors (KNN)

```pascal
type
  TKNNClassifier = class
  private
    FKNN: TKNearest;
  public
    constructor Create(k: Integer = 5);
    destructor Destroy; override;
    procedure Train(const trainData, labels: TMat);
    function Predict(const sample: TMat): Single;
  end;

constructor TKNNClassifier.Create(k: Integer);
begin
  inherited Create;
  FKNN := KNearest_create;
  FKNN.setDefaultK(k);
end;

destructor TKNNClassifier.Destroy;
begin
  FKNN.release;
  inherited;
end;

procedure TKNNClassifier.Train(const trainData, labels: TMat);
var
  trainDataPtr: TTrainDataPtr;
begin
  trainDataPtr := TrainData_create(trainData, ROW_SAMPLE, labels);
  try
    FKNN.train(trainDataPtr);
  finally
    trainDataPtr.release;
  end;
end;

function TKNNClassifier.Predict(const sample: TMat): Single;
var
  results: TMat;
begin
  results := Mat.create;
  try
    FKNN.findNearest(sample, FKNN.getDefaultK, results);
    Result := PSingle(results.data)^;
  finally
    results.release;
  end;
end;
```

### Clustering avec K-Means

```pascal
procedure PerformKMeansClustering(const data: TMat; k: Integer);
var
  labels, centers: TMat;
  criteria: TTermCriteria;
  compactness: Double;
begin
  // Critères d'arrêt
  criteria := TermCriteria(TermCriteria_EPS + TermCriteria_MAX_ITER,
    100, 0.2);

  // Effectuer K-Means
  compactness := kmeans(data, k, labels, criteria, 3, KMEANS_PP_CENTERS, centers);

  WriteLn(Format('Compacité : %.2f', [compactness]));
  WriteLn(Format('Nombre de clusters : %d', [k]));

  labels.release;
  centers.release;
end;

// Exemple : Segmentation d'image par couleur
procedure SegmentImageByColor(const imagePath: string; numColors: Integer);
var
  img, reshaped, labels, centers: TMat;
  i, j, classLabel: Integer;  { label est un mot réservé en Pascal }
  color: TVec3b;
  result: TMat;
begin
  img := imread(imagePath);
  try
    // Reshaper en liste de pixels
    reshaped := img.reshape(1, img.rows * img.cols);
    reshaped.convertTo(reshaped, CV_32F);

    // K-Means
    PerformKMeansClustering(reshaped, numColors);

    // Reconstruire l'image avec les couleurs des centres
    result := Mat.create(img.rows, img.cols, img.type_);

    for i := 0 to img.rows - 1 do
    begin
      for j := 0 to img.cols - 1 do
      begin
        classLabel := PInteger(labels.data)[i * img.cols + j];
        { Note : la syntaxe .at<T> est C++; en Pascal, utiliser
          un cast de pointeur ou une méthode du binding }
        color := TVec3b(centers.at(classLabel));
        result.at(i, j, color);
      end;
    end;

    imwrite('segmented.jpg', result);

    reshaped.release;
    labels.release;
    centers.release;
    result.release;
  finally
    img.release;
  end;
end;
```

## Deep Learning avec OpenCV DNN

OpenCV 4.x inclut un module DNN (Deep Neural Network) pour exécuter des modèles pré-entraînés.

### Chargement de modèles

```pascal
type
  TDNNModel = class
  private
    FNet: TNet;
    FInputSize: TSize;
    FMean: TScalar;
    FScale: Double;
  public
    constructor Create;
    destructor Destroy; override;
    function LoadCaffe(const prototxt, caffeModel: string): Boolean;
    function LoadTensorFlow(const model: string): Boolean;
    function LoadONNX(const model: string): Boolean;
    function Predict(const img: TMat): TMat;
    procedure SetInputParams(inputSize: TSize; mean: TScalar; scale: Double);
  end;

constructor TDNNModel.Create;
begin
  inherited;
  FInputSize := Size(224, 224); // Taille par défaut
  FMean := Scalar(0, 0, 0);
  FScale := 1.0;
end;

destructor TDNNModel.Destroy;
begin
  FNet.release;
  inherited;
end;

function TDNNModel.LoadCaffe(const prototxt, caffeModel: string): Boolean;
begin
  try
    FNet := readNetFromCaffe(PAnsiChar(AnsiString(prototxt)),
                              PAnsiChar(AnsiString(caffeModel)));
    Result := not FNet.empty;
  except
    Result := False;
  end;
end;

function TDNNModel.LoadTensorFlow(const model: string): Boolean;
begin
  try
    FNet := readNetFromTensorflow(PAnsiChar(AnsiString(model)));
    Result := not FNet.empty;
  except
    Result := False;
  end;
end;

function TDNNModel.LoadONNX(const model: string): Boolean;
begin
  try
    FNet := readNetFromONNX(PAnsiChar(AnsiString(model)));
    Result := not FNet.empty;
  except
    Result := False;
  end;
end;

procedure TDNNModel.SetInputParams(inputSize: TSize; mean: TScalar; scale: Double);
begin
  FInputSize := inputSize;
  FMean := mean;
  FScale := scale;
end;

function TDNNModel.Predict(const img: TMat): TMat;
var
  blob: TMat;
begin
  // Créer un blob à partir de l'image
  blob := blobFromImage(img, FScale, FInputSize, FMean, True, False);

  // Définir l'entrée
  FNet.setInput(blob);

  // Forward pass
  Result := FNet.forward;

  blob.release;
end;
```

### Classification d'images avec un réseau pré-entraîné

```pascal
procedure ClassifyImageWithDNN(const imagePath: string);
var
  model: TDNNModel;
  img, prediction: TMat;
  classId: Integer;
  confidence: Single;
  classNames: TStringList;
  minVal, maxVal: Double;
  minLoc, maxLoc: TPoint;
begin
  model := TDNNModel.Create;
  classNames := TStringList.Create;
  try
    // Charger le modèle (exemple : MobileNet)
    {$IFDEF WINDOWS}
    if not model.LoadCaffe('mobilenet.prototxt', 'mobilenet.caffemodel') then
    {$ELSE}
    if not model.LoadONNX('mobilenet.onnx') then
    {$ENDIF}
    begin
      WriteLn('Erreur de chargement du modèle');
      Exit;
    end;

    // Paramètres pour MobileNet
    model.SetInputParams(Size(224, 224), Scalar(104, 117, 123), 1.0);

    // Charger les noms de classes
    classNames.LoadFromFile('imagenet_classes.txt');

    // Charger et prédire
    img := imread(imagePath);
    try
      prediction := model.Predict(img);

      // Trouver la classe avec la plus grande probabilité
      minMaxLoc(prediction, @minVal, @maxVal, @minLoc, @maxLoc);
      classId := maxLoc.x;
      confidence := maxVal;

      WriteLn(Format('Classe : %s', [classNames[classId]]));
      WriteLn(Format('Confiance : %.2f%%', [confidence * 100]));

      prediction.release;
    finally
      img.release;
    end;
  finally
    model.Free;
    classNames.Free;
  end;
end;
```

### Détection d'objets avec YOLO

```pascal
type
  TYOLODetector = class
  private
    FNet: TNet;
    FOutputLayers: TStringList;
    FConfThreshold: Single;
    FNMSThreshold: Single;
  public
    constructor Create(confThreshold: Single = 0.5; nmsThreshold: Single = 0.4);
    destructor Destroy; override;
    function LoadModel(const cfg, weights: string): Boolean;
    function Detect(const img: TMat): TArray<TDetection>;
  end;

type
  TDetection = record
    classId: Integer;
    confidence: Single;
    box: TRect;
  end;

constructor TYOLODetector.Create(confThreshold, nmsThreshold: Single);
begin
  inherited Create;
  FConfThreshold := confThreshold;
  FNMSThreshold := nmsThreshold;
  FOutputLayers := TStringList.Create;
end;

destructor TYOLODetector.Destroy;
begin
  FNet.release;
  FOutputLayers.Free;
  inherited;
end;

function TYOLODetector.LoadModel(const cfg, weights: string): Boolean;
var
  outNames: TVector<String>;
  i: Integer;
begin
  try
    FNet := readNetFromDarknet(PAnsiChar(AnsiString(cfg)),
                                PAnsiChar(AnsiString(weights)));

    // Obtenir les noms des couches de sortie
    outNames := FNet.getUnconnectedOutLayersNames;
    FOutputLayers.Clear;
    for i := 0 to outNames.size - 1 do
      FOutputLayers.Add(string(outNames[i]));

    Result := not FNet.empty;
  except
    Result := False;
  end;
end;

function TYOLODetector.Detect(const img: TMat): TArray<TDetection>;
var
  blob: TMat;
  outputs: TVector<TMat>;
  i, j: Integer;
  detection: PSingle;
  confidence: Single;
  classId: Integer;
  centerX, centerY, width, height: Integer;
  left, top: Integer;
  boxes: TVector<TRect>;
  confidences: TVector<Single>;
  classIds: TVector<Integer>;
  indices: TVector<Integer>;
  resultList: TList<TDetection>;
  det: TDetection;
begin
  // Créer le blob
  blob := blobFromImage(img, 1/255.0, Size(416, 416), Scalar(0, 0, 0), True, False);

  FNet.setInput(blob);
  FNet.forward(outputs, FOutputLayers);

  boxes := TVector<TRect>.Create;
  confidences := TVector<Single>.Create;
  classIds := TVector<Integer>.Create;
  resultList := TList<TDetection>.Create;

  try
    // Analyser les détections
    for i := 0 to outputs.size - 1 do
    begin
      detection := PSingle(outputs[i].data);

      for j := 0 to outputs[i].rows - 1 do
      begin
        // Scores à partir du 5ème élément
        Inc(detection, 5);

        // Trouver la classe avec le score maximum
        classId := 0;
        confidence := detection^;
        // ... (parcourir toutes les classes)

        if confidence > FConfThreshold then
        begin
          // Coordonnées du rectangle
          centerX := Round(detection^ * img.cols);
          centerY := Round(detection^ * img.rows);
          width := Round(detection^ * img.cols);
          height := Round(detection^ * img.rows);

          left := centerX - width div 2;
          top := centerY - height div 2;

          boxes.push_back(Rect(left, top, width, height));
          confidences.push_back(confidence);
          classIds.push_back(classId);
        end;
      end;
    end;

    // Non-Maximum Suppression
    NMSBoxes(boxes, confidences, FConfThreshold, FNMSThreshold, indices);

    // Créer les résultats finaux
    for i := 0 to indices.size - 1 do
    begin
      det.classId := classIds[indices[i]];
      det.confidence := confidences[indices[i]];
      det.box := boxes[indices[i]];
      resultList.Add(det);
    end;

    Result := resultList.ToArray;
  finally
    blob.release;
    boxes.Free;
    confidences.Free;
    classIds.Free;
    indices.Free;
    resultList.Free;
  end;
end;

// Exemple d'utilisation
procedure DetectObjectsInImage(const imagePath: string);
var
  detector: TYOLODetector;
  img, display: TMat;
  detections: TArray<TDetection>;
  i: Integer;
  classNames: TStringList;
begin
  detector := TYOLODetector.Create(0.5, 0.4);
  classNames := TStringList.Create;
  try
    // Charger le modèle YOLO
    if not detector.LoadModel('yolov3.cfg', 'yolov3.weights') then
    begin
      WriteLn('Erreur de chargement du modèle YOLO');
      Exit;
    end;

    // Charger les noms de classes
    classNames.LoadFromFile('coco.names');

    img := imread(imagePath);
    display := img.clone;
    try
      // Détecter
      detections := detector.Detect(img);

      WriteLn(Format('Objets détectés : %d', [Length(detections)]));

      // Dessiner les détections
      for i := 0 to High(detections) do
      begin
        rectangle(display, detections[i].box, Scalar(0, 255, 0), 2);

        putText(display,
          Format('%s: %.0f%%', [classNames[detections[i].classId],
                                detections[i].confidence * 100]),
          Point(detections[i].box.x, detections[i].box.y - 5),
          FONT_HERSHEY_SIMPLEX, 0.5, Scalar(0, 255, 0), 2);
      end;

      imwrite('detected_objects.jpg', display);
      imshow('Détections', display);
      waitKey(0);
    finally
      img.release;
      display.release;
    end;
  finally
    detector.Free;
    classNames.Free;
  end;
end;
```

## Calibration de caméra

### Calibration avec échiquier

```pascal
type
  TCameraCalibrator = class
  private
    FBoardSize: TSize;
    FSquareSize: Single;
    FObjectPoints: TVector<TVector<TPoint3f>>;
    FImagePoints: TVector<TVector<TPoint2f>>;
  public
    constructor Create(boardSize: TSize; squareSize: Single);
    destructor Destroy; override;
    function AddCheckerboardImage(const img: TMat): Boolean;
    function Calibrate(imageSize: TSize; var cameraMatrix, distCoeffs: TMat): Double;
    procedure Undistort(const src: TMat; var dst: TMat;
      const cameraMatrix, distCoeffs: TMat);
  end;

constructor TCameraCalibrator.Create(boardSize: TSize; squareSize: Single);
begin
  inherited Create;
  FBoardSize := boardSize;
  FSquareSize := squareSize;
  FObjectPoints := TVector<TVector<TPoint3f>>.Create;
  FImagePoints := TVector<TVector<TPoint2f>>.Create;
end;

destructor TCameraCalibrator.Destroy;
begin
  FObjectPoints.Free;
  FImagePoints.Free;
  inherited;
end;

function TCameraCalibrator.AddCheckerboardImage(const img: TMat): Boolean;
var
  gray: TMat;
  corners: TVector<TPoint2f>;
  objPoints: TVector<TPoint3f>;
  i, j: Integer;
begin
  // Convertir en niveaux de gris
  if img.channels > 1 then
    cvtColor(img, gray, CV_BGR2GRAY)
  else
    gray := img.clone;

  // Trouver les coins de l'échiquier
  Result := findChessboardCorners(gray, FBoardSize, corners,
    CALIB_CB_ADAPTIVE_THRESH + CALIB_CB_NORMALIZE_IMAGE);

  if Result then
  begin
    // Affiner la position des coins
    cornerSubPix(gray, corners, Size(11, 11), Size(-1, -1),
      TermCriteria(TermCriteria_EPS + TermCriteria_MAX_ITER, 30, 0.001));

    // Ajouter aux points image
    FImagePoints.push_back(corners);

    // Créer les points objets 3D
    objPoints := TVector<TPoint3f>.Create;
    for i := 0 to FBoardSize.height - 1 do
      for j := 0 to FBoardSize.width - 1 do
        objPoints.push_back(Point3f(j * FSquareSize, i * FSquareSize, 0));

    FObjectPoints.push_back(objPoints);

    WriteLn(Format('Image ajoutée : %d coins détectés', [corners.size]));
  end else
    WriteLn('Coins non détectés dans cette image');

  gray.release;
end;

function TCameraCalibrator.Calibrate(imageSize: TSize;
  var cameraMatrix, distCoeffs: TMat): Double;
var
  rvecs, tvecs: TVector<TMat>;
begin
  cameraMatrix := Mat.eye(3, 3, CV_64F);
  distCoeffs := Mat.zeros(8, 1, CV_64F);

  Result := calibrateCamera(FObjectPoints, FImagePoints, imageSize,
    cameraMatrix, distCoeffs, rvecs, tvecs);

  WriteLn(Format('Erreur de reprojection : %.2f pixels', [Result]));

  rvecs.Free;
  tvecs.Free;
end;

procedure TCameraCalibrator.Undistort(const src: TMat; var dst: TMat;
  const cameraMatrix, distCoeffs: TMat);
begin
  cv.undistort(src, dst, cameraMatrix, distCoeffs);
end;

// Exemple complet de calibration
procedure CalibrateCamera;
var
  calibrator: TCameraCalibrator;
  img: TMat;
  cameraMatrix, distCoeffs: TMat;
  i: Integer;
  error: Double;
begin
  // Échiquier 9x6, carrés de 25mm
  calibrator := TCameraCalibrator.Create(Size(9, 6), 25.0);
  try
    // Ajouter plusieurs images de l'échiquier sous différents angles
    for i := 1 to 20 do
    begin
      img := imread(Format('calibration_%d.jpg', [i]));
      if not img.empty then
      begin
        calibrator.AddCheckerboardImage(img);
        img.release;
      end;
    end;

    // Calibrer
    error := calibrator.Calibrate(Size(1920, 1080), cameraMatrix, distCoeffs);

    // Sauvegarder les paramètres
    FileStorage_write('camera_calibration.yml', 'cameraMatrix', cameraMatrix);
    FileStorage_write('camera_calibration.yml', 'distCoeffs', distCoeffs);

    WriteLn('Calibration terminée');
    WriteLn(Format('Erreur moyenne : %.3f pixels', [error]));

    cameraMatrix.release;
    distCoeffs.release;
  finally
    calibrator.Free;
  end;
end;
```

## Spécificités Windows/Ubuntu

### Chemins des bibliothèques

```pascal
{$IFDEF WINDOWS}
const
  OPENCV_DLL_PATH = 'C:\opencv\build\x64\vc15\bin\';
  OPENCV_DATA_PATH = 'C:\opencv\data\';
{$ENDIF}

{$IFDEF LINUX}
const
  OPENCV_SO_PATH = '/usr/lib/x86_64-linux-gnu/';
  OPENCV_DATA_PATH = '/usr/share/opencv4/';
{$ENDIF}

function GetHaarCascadePath(const cascadeName: string): string;
begin
  {$IFDEF WINDOWS}
  Result := OPENCV_DATA_PATH + 'haarcascades\' + cascadeName;
  {$ELSE}
  Result := OPENCV_DATA_PATH + 'haarcascades/' + cascadeName;
  {$ENDIF}
end;
```

### Configuration de la capture vidéo

```pascal
procedure ConfigureVideoCapture(cap: TVideoCapture);
begin
  {$IFDEF WINDOWS}
  // Utiliser DirectShow sur Windows
  cap.Open(0 + CAP_DSHOW);
  {$ELSE}
  // Utiliser V4L2 sur Linux
  cap.Open(0 + CAP_V4L2);
  {$ENDIF}

  // Configuration commune
  cap.SetProperty(CAP_PROP_FRAME_WIDTH, 1920);
  cap.SetProperty(CAP_PROP_FRAME_HEIGHT, 1080);
  cap.SetProperty(CAP_PROP_FPS, 30);
end;
```

### Accélération matérielle

```pascal
procedure EnableHardwareAcceleration;
begin
  {$IFDEF WINDOWS}
  // CUDA sur Windows (si disponible)
  if getCudaEnabledDeviceCount > 0 then
  begin
    WriteLn('CUDA disponible');
    setNumThreads(0); // Utiliser tous les cœurs
  end;
  {$ELSE}
  // OpenCL sur Linux
  if cv.ocl.haveOpenCL then
  begin
    WriteLn('OpenCL disponible');
    cv.ocl.setUseOpenCL(True);
  end;
  {$ENDIF}
end;
```

## Application complète : Système de surveillance

```pascal
program SurveillanceSystem;

uses
  Classes, SysUtils, DateUtils,
  ocv.core, ocv.highgui, ocv.imgproc, ocv.objdetect, ocv.videoio;

type
  TSurveillanceSystem = class
  private
    FCapture: TVideoCapture;
    FFaceDetector: TCascadeClassifier;
    FMotionDetector: TMotionDetector;
    FRecording: Boolean;
    FWriter: TVideoWriter;
    FLogFile: TextFile;
    FSnapshotDir: string;
    FVideoDir: string;
    procedure ProcessFrame(const frame: TMat);
    procedure DetectAndAlertMotion(const frame: TMat);
    procedure SaveSnapshot(const frame: TMat; const reason: string);
    procedure LogEvent(const msg: string);
    procedure StartRecording(const frame: TMat);
    procedure StopRecording;
  public
    constructor Create;
    destructor Destroy; override;
    function Initialize: Boolean;
    procedure Run;
    procedure Stop;
  end;

constructor TSurveillanceSystem.Create;
begin
  inherited;
  FCapture := TVideoCapture.Create;
  FFaceDetector := TCascadeClassifier.Create;
  FMotionDetector := TMotionDetector.Create(30);
  FRecording := False;

  // Créer les répertoires de sortie
  FSnapshotDir := 'snapshots';
  FVideoDir := 'recordings';

  if not DirectoryExists(FSnapshotDir) then
    CreateDir(FSnapshotDir);
  if not DirectoryExists(FVideoDir) then
    CreateDir(FVideoDir);

  // Ouvrir le fichier de log
  AssignFile(FLogFile, 'surveillance.log');
  if FileExists('surveillance.log') then
    Append(FLogFile)
  else
    Rewrite(FLogFile);
end;

destructor TSurveillanceSystem.Destroy;
begin
  Stop;
  FCapture.Free;
  FFaceDetector.Free;
  FMotionDetector.Free;

  if FRecording then
    FWriter.Free;

  CloseFile(FLogFile);
  inherited;
end;

function TSurveillanceSystem.Initialize: Boolean;
var
  cascadePath: string;
begin
  Result := False;

  // Charger le classificateur de visages
  {$IFDEF WINDOWS}
  cascadePath := 'C:\opencv\data\haarcascades\haarcascade_frontalface_default.xml';
  {$ELSE}
  cascadePath := '/usr/share/opencv4/haarcascades/haarcascade_frontalface_default.xml';
  {$ENDIF}

  if not FFaceDetector.load(PAnsiChar(AnsiString(cascadePath))) then
  begin
    WriteLn('Erreur : Impossible de charger le cascade de détection faciale');
    Exit;
  end;

  // Ouvrir la caméra
  if not FCapture.Open(0) then
  begin
    WriteLn('Erreur : Impossible d''ouvrir la caméra');
    Exit;
  end;

  // Configuration de la caméra
  FCapture.SetProperty(CAP_PROP_FRAME_WIDTH, 1280);
  FCapture.SetProperty(CAP_PROP_FRAME_HEIGHT, 720);
  FCapture.SetProperty(CAP_PROP_FPS, 30);

  LogEvent('Système de surveillance initialisé');
  Result := True;
end;

procedure TSurveillanceSystem.LogEvent(const msg: string);
var
  timestamp: string;
begin
  timestamp := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);
  WriteLn(FLogFile, Format('[%s] %s', [timestamp, msg]));
  Flush(FLogFile);
  WriteLn(Format('[%s] %s', [timestamp, msg]));
end;

procedure TSurveillanceSystem.SaveSnapshot(const frame: TMat; const reason: string);
var
  filename: string;
  timestamp: string;
begin
  timestamp := FormatDateTime('yyyymmdd_hhnnss', Now);
  filename := Format('%s%ssnapshot_%s_%s.jpg', [FSnapshotDir, PathDelim, timestamp, reason]);

  imwrite(PAnsiChar(AnsiString(filename)), frame);
  LogEvent(Format('Snapshot sauvegardé : %s', [filename]));
end;

procedure TSurveillanceSystem.StartRecording(const frame: TMat);
var
  filename: string;
  timestamp: string;
  fps: Double;
  frameSize: TSize;
begin
  if FRecording then Exit;

  timestamp := FormatDateTime('yyyymmdd_hhnnss', Now);
  filename := Format('%s%svideo_%s.avi', [FVideoDir, PathDelim, timestamp]);

  fps := FCapture.GetProperty(CAP_PROP_FPS);
  if fps = 0 then fps := 30.0;

  frameSize.width := frame.cols;
  frameSize.height := frame.rows;

  FWriter := TVideoWriter.Create;

  {$IFDEF WINDOWS}
  if FWriter.Open(filename, FOURCC_MJPG, fps, frameSize) then
  {$ELSE}
  if FWriter.Open(filename, FOURCC_XVID, fps, frameSize) then
  {$ENDIF}
  begin
    FRecording := True;
    LogEvent(Format('Enregistrement démarré : %s', [filename]));
  end
  else
  begin
    FWriter.Free;
    LogEvent('Erreur : Impossible de démarrer l''enregistrement');
  end;
end;

procedure TSurveillanceSystem.StopRecording;
begin
  if not FRecording then Exit;

  FWriter.Release;
  FWriter.Free;
  FRecording := False;
  LogEvent('Enregistrement arrêté');
end;

procedure TSurveillanceSystem.DetectAndAlertMotion(const frame: TMat);
var
  motionMask: TMat;
  regions: TArray<TRect>;
  i: Integer;
  totalMotionArea: Integer;
begin
  // Détecter le mouvement
  motionMask := FMotionDetector.DetectMotion(frame);
  regions := FMotionDetector.GetMotionRegions(motionMask);

  // Calculer l'aire totale de mouvement
  totalMotionArea := 0;
  for i := 0 to High(regions) do
    totalMotionArea := totalMotionArea + (regions[i].width * regions[i].height);

  // Si mouvement significatif détecté
  if (Length(regions) > 0) and (totalMotionArea > 5000) then
  begin
    // Démarrer l'enregistrement si pas déjà en cours
    if not FRecording then
    begin
      StartRecording(frame);
      SaveSnapshot(frame, 'motion');
      LogEvent(Format('Mouvement détecté : %d zones, aire totale : %d',
        [Length(regions), totalMotionArea]));
    end;
  end
  else
  begin
    // Arrêter l'enregistrement après quelques secondes sans mouvement
    if FRecording then
      StopRecording;
  end;

  motionMask.release;
end;

procedure TSurveillanceSystem.ProcessFrame(const frame: TMat);
var
  gray: TMat;
  faces: TVectorOfRect;
  i: Integer;
  faceRect: TRect;
  display: TMat;
begin
  display := frame.clone;

  // Détection de mouvement
  DetectAndAlertMotion(frame);

  // Détection de visages
  if frame.channels > 1 then
    cvtColor(frame, gray, CV_BGR2GRAY)
  else
    gray := frame.clone;

  equalizeHist(gray, gray);

  FFaceDetector.detectMultiScale(gray, faces, 1.1, 3, 0,
    Size(30, 30), Size(0, 0));

  // Dessiner les rectangles des visages
  if faces.size > 0 then
  begin
    for i := 0 to faces.size - 1 do
    begin
      faceRect := faces.at(i);
      rectangle(display, faceRect, Scalar(0, 255, 0), 2);
      putText(display, 'Visage',
        Point(faceRect.x, faceRect.y - 5),
        FONT_HERSHEY_SIMPLEX, 0.5, Scalar(0, 255, 0), 2);
    end;

    // Sauvegarder une photo si visage détecté
    if not FRecording then
    begin
      SaveSnapshot(frame, 'face');
      LogEvent(Format('%d visage(s) détecté(s)', [faces.size]));
    end;
  end;

  // Afficher le statut
  if FRecording then
    putText(display, 'REC', Point(10, 30),
      FONT_HERSHEY_SIMPLEX, 1, Scalar(0, 0, 255), 3);

  // Afficher le timestamp
  putText(display, FormatDateTime('yyyy-mm-dd hh:nn:ss', Now),
    Point(10, display.rows - 10),
    FONT_HERSHEY_SIMPLEX, 0.5, Scalar(255, 255, 255), 1);

  // Afficher
  imshow('Surveillance', display);

  // Écrire dans la vidéo si enregistrement en cours
  if FRecording then
    FWriter.Write(display);

  gray.release;
  display.release;
end;

procedure TSurveillanceSystem.Run;
var
  frame: TMat;
  key: Integer;
begin
  if not Initialize then
    Exit;

  WriteLn('=== SYSTÈME DE SURVEILLANCE ===');
  WriteLn('Appuyez sur :');
  WriteLn('  ESC : Quitter');
  WriteLn('  R : Démarrer/Arrêter l''enregistrement manuel');
  WriteLn('  S : Prendre un snapshot');
  WriteLn('');

  namedWindow('Surveillance', WINDOW_AUTOSIZE);

  while True do
  begin
    // Lire une frame
    if not FCapture.Read(frame) then
    begin
      LogEvent('Erreur : Impossible de lire la frame');
      Break;
    end;

    // Traiter la frame
    ProcessFrame(frame);

    // Vérifier les touches
    key := waitKey(1);

    case key of
      27: // ESC
        begin
          LogEvent('Arrêt du système demandé par l''utilisateur');
          Break;
        end;

      Ord('r'), Ord('R'): // Enregistrement manuel
        begin
          if FRecording then
            StopRecording
          else
            StartRecording(frame);
        end;

      Ord('s'), Ord('S'): // Snapshot manuel
        begin
          SaveSnapshot(frame, 'manual');
        end;
    end;
  end;

  frame.release;
  destroyAllWindows;
end;

procedure TSurveillanceSystem.Stop;
begin
  if FRecording then
    StopRecording;

  FCapture.Release;
  LogEvent('Système de surveillance arrêté');
end;

// Programme principal
var
  system: TSurveillanceSystem;

begin
  system := TSurveillanceSystem.Create;
  try
    system.Run;
  finally
    system.Free;
  end;
end.
```

## Projet avancé : Reconnaissance de plaques d'immatriculation

```pascal
unit LicensePlateRecognition;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ocv.core, ocv.imgproc, ocv.objdetect;

type
  TPlateRegion = record
    rect: TRect;
    confidence: Single;
  end;

  TLicensePlateDetector = class
  private
    FMinPlateWidth: Integer;
    FMaxPlateWidth: Integer;
    FMinPlateHeight: Integer;
    FMaxPlateHeight: Integer;
    function IsValidPlateSize(const rect: TRect): Boolean;
    function IsValidAspectRatio(const rect: TRect): Boolean;
    function PreprocessImage(const src: TMat): TMat;
    function FindPlateContours(const binary: TMat): TContours;
  public
    constructor Create;
    function DetectPlates(const img: TMat): TArray<TPlateRegion>;
    function ExtractPlateImage(const img: TMat; const region: TRect): TMat;
    property MinPlateWidth: Integer read FMinPlateWidth write FMinPlateWidth;
    property MaxPlateWidth: Integer read FMaxPlateWidth write FMaxPlateWidth;
  end;

implementation

constructor TLicensePlateDetector.Create;
begin
  inherited;
  // Dimensions typiques d'une plaque européenne (en pixels, image 640x480)
  FMinPlateWidth := 80;
  FMaxPlateWidth := 200;
  FMinPlateHeight := 20;
  FMaxPlateHeight := 60;
end;

function TLicensePlateDetector.IsValidPlateSize(const rect: TRect): Boolean;
begin
  Result := (rect.width >= FMinPlateWidth) and
            (rect.width <= FMaxPlateWidth) and
            (rect.height >= FMinPlateHeight) and
            (rect.height <= FMaxPlateHeight);
end;

function TLicensePlateDetector.IsValidAspectRatio(const rect: TRect): Boolean;
var
  aspectRatio: Double;
begin
  aspectRatio := rect.width / rect.height;
  // Plaques européennes : ratio environ 4.5 à 5.5
  // Plaques US : ratio environ 2.0 à 3.0
  Result := (aspectRatio >= 2.0) and (aspectRatio <= 6.0);
end;

function TLicensePlateDetector.PreprocessImage(const src: TMat): TMat;
var
  gray, blur, morph, binary: TMat;
  kernel: TMat;
begin
  // Convertir en niveaux de gris
  if src.channels > 1 then
    cvtColor(src, gray, CV_BGR2GRAY)
  else
    gray := src.clone;

  // Réduire le bruit
  bilateralFilter(gray, blur, 11, 17, 17);

  // Détection de contours avec Canny
  Canny(blur, binary, 30, 200);

  // Morphologie pour connecter les contours
  kernel := getStructuringElement(MORPH_RECT, Size(3, 3));
  morphologyEx(binary, morph, MORPH_CLOSE, kernel);

  Result := morph;

  gray.release;
  blur.release;
  binary.release;
  kernel.release;
end;

function TLicensePlateDetector.FindPlateContours(const binary: TMat): TContours;
var
  allContours: TContours;
  i: Integer;
  rect: TRect;
  area: Double;
  resultList: TList<TContour>;
begin
  // Trouver tous les contours
  allContours := FindContours(binary, RETR_TREE, CHAIN_APPROX_SIMPLE);

  resultList := TList<TContour>.Create;
  try
    // Filtrer les contours selon la taille et le ratio
    for i := 0 to High(allContours) do
    begin
      area := ContourArea(allContours[i]);

      // Ignorer les contours trop petits
      if area < 500 then
        Continue;

      rect := boundingRect(allContours[i]);

      // Vérifier la taille et le ratio
      if IsValidPlateSize(rect) and IsValidAspectRatio(rect) then
        resultList.Add(allContours[i]);
    end;

    Result := resultList.ToArray;
  finally
    resultList.Free;
  end;
end;

function TLicensePlateDetector.DetectPlates(const img: TMat): TArray<TPlateRegion>;
var
  preprocessed: TMat;
  contours: TContours;
  i: Integer;
  resultList: TList<TPlateRegion>;
  region: TPlateRegion;
begin
  // Prétraiter l'image
  preprocessed := PreprocessImage(img);

  try
    // Trouver les contours candidats
    contours := FindPlateContours(preprocessed);

    resultList := TList<TPlateRegion>.Create;
    try
      // Créer les régions
      for i := 0 to High(contours) do
      begin
        region.rect := boundingRect(contours[i]);
        region.confidence := 0.8; // Simplification - devrait être calculé
        resultList.Add(region);
      end;

      Result := resultList.ToArray;
    finally
      resultList.Free;
    end;
  finally
    preprocessed.release;
  end;
end;

function TLicensePlateDetector.ExtractPlateImage(const img: TMat;
  const region: TRect): TMat;
var
  plate, gray, thresh: TMat;
begin
  // Extraire la région
  plate := Mat(img, region);

  // Convertir en niveaux de gris
  if plate.channels > 1 then
    cvtColor(plate, gray, CV_BGR2GRAY)
  else
    gray := plate.clone;

  // Améliorer le contraste
  equalizeHist(gray, gray);

  // Binariser
  adaptiveThreshold(gray, thresh, 255, ADAPTIVE_THRESH_GAUSSIAN_C,
    THRESH_BINARY, 11, 2);

  Result := thresh;
  gray.release;
end;

end.

// Exemple d'utilisation
procedure RecognizeLicensePlate(const imagePath: string);
var
  detector: TLicensePlateDetector;
  img, display, plateImg: TMat;
  plates: TArray<TPlateRegion>;
  i: Integer;
begin
  detector := TLicensePlateDetector.Create;
  try
    img := imread(imagePath);
    display := img.clone;

    try
      // Détecter les plaques
      plates := detector.DetectPlates(img);

      WriteLn(Format('Plaques détectées : %d', [Length(plates)]));

      // Traiter chaque plaque
      for i := 0 to High(plates) do
      begin
        // Dessiner le rectangle
        rectangle(display, plates[i].rect, Scalar(0, 255, 0), 2);

        // Extraire et sauvegarder l'image de la plaque
        plateImg := detector.ExtractPlateImage(img, plates[i].rect);
        try
          imwrite(Format('plate_%d.jpg', [i]), plateImg);

          // Ici, vous pourriez appeler Tesseract OCR pour lire le texte
          // ExtractTextFromImage(Format('plate_%d.jpg', [i]));
        finally
          plateImg.release;
        end;
      end;

      imwrite('detected_plates.jpg', display);
      imshow('Plaques détectées', display);
      waitKey(0);
    finally
      img.release;
      display.release;
    end;
  finally
    detector.Free;
  end;
end;
```

## Ressources et documentation

### Documentation officielle OpenCV

- **Site officiel** : https://opencv.org/
- **Documentation** : https://docs.opencv.org/4.x/
- **Tutoriels** : https://docs.opencv.org/4.x/d9/df8/tutorial_root.html
- **GitHub** : https://github.com/opencv/opencv

### Bindings Pascal

- **Delphi-OpenCV** : https://github.com/Laex/Delphi-OpenCV
- **Forum Lazarus** : https://forum.lazarus.freepascal.org/

### Datasets pour l'apprentissage

- **MNIST** : Chiffres manuscrits (http://yann.lecun.com/exdb/mnist/)
- **CIFAR-10** : Images d'objets (https://www.cs.toronto.edu/~kriz/cifar.html)
- **ImageNet** : Millions d'images classifiées (http://www.image-net.org/)
- **COCO** : Détection d'objets (https://cocodataset.org/)
- **LFW** : Visages (http://vis-www.cs.umass.edu/lfw/)

### Modèles pré-entraînés

OpenCV propose des modèles pré-entraînés disponibles sur le Model Zoo :
- **Classification** : AlexNet, GoogLeNet, ResNet, VGG, MobileNet
- **Détection** : SSD, YOLO, Faster R-CNN
- **Segmentation** : Mask R-CNN, FCN
- **Pose** : OpenPose

Télécharger depuis : https://github.com/opencv/opencv/wiki/Model-Zoo

### Installation des modèles

```bash
# Windows
mkdir C:\opencv\models
cd C:\opencv\models

# Télécharger un modèle (exemple : MobileNet-SSD)
curl -O https://github.com/chuanqi305/MobileNet-SSD/raw/master/mobilenet_iter_73000.caffemodel
curl -O https://github.com/chuanqi305/MobileNet-SSD/raw/master/deploy.prototxt

# Ubuntu
mkdir ~/opencv_models
cd ~/opencv_models

# Télécharger
wget https://github.com/chuanqi305/MobileNet-SSD/raw/master/mobilenet_iter_73000.caffemodel
wget https://github.com/chuanqi305/MobileNet-SSD/raw/master/deploy.prototxt
```

## Conseils et bonnes pratiques

### 1. Gestion de la mémoire

```pascal
// TOUJOURS libérer les ressources
procedure GoodMemoryManagement;
var
  img: TMat;
begin
  img := imread('photo.jpg');
  try
    // Traiter l'image
  finally
    img.release; // Libération garantie
  end;
end;
```

### 2. Performance

```pascal
// Réutiliser les matrices
var
  gGrayBuffer: TMat; // Variable globale

procedure FastProcessing(const img: TMat);
begin
  if gGrayBuffer.empty then
    gGrayBuffer := Mat.create;

  cvtColor(img, gGrayBuffer, CV_BGR2GRAY);
  // Traiter gGrayBuffer
end;
```

### 3. Gestion d'erreurs

```pascal
procedure SafeImageProcessing(const filename: string);
var
  img: TMat;
begin
  img := imread(filename);

  if img.empty then
  begin
    WriteLn(Format('Erreur : Impossible de charger %s', [filename]));
    Exit;
  end;

  try
    // Vérifier les dimensions
    if (img.cols = 0) or (img.rows = 0) then
      raise Exception.Create('Image invalide');

    // Traiter l'image
  finally
    img.release;
  end;
end;
```

### 4. Logging et débogage

```pascal
procedure DebugImage(const img: TMat; const name: string);
begin
  WriteLn(Format('%s - Type: %d, Taille: %dx%d, Canaux: %d',
    [name, img.type_, img.cols, img.rows, img.channels]));

  // Sauvegarder pour inspection visuelle
  imwrite(Format('debug_%s.jpg', [name]), img);
end;
```

### 5. Tests unitaires

```pascal
procedure TestImageProcessing;
var
  img, result: TMat;
  expected: TMat;
begin
  // Créer une image de test
  img := Mat.zeros(100, 100, CV_8UC3);

  // Traiter
  result := ProcessImage(img);

  // Vérifier le résultat
  Assert(result.cols = img.cols);
  Assert(result.rows = img.rows);

  img.release;
  result.release;
end;
```

## Conclusion

OpenCV avec FreePascal/Lazarus offre des possibilités immenses pour la vision par ordinateur :

### Ce que vous avez appris

- **Installation et configuration** multi-plateforme (Windows/Ubuntu)
- **Opérations de base** : chargement, affichage, manipulation d'images
- **Traitement d'images** : filtrage, détection de contours, transformations
- **Détection d'objets** : formes, visages, contours
- **Traitement vidéo** : capture, enregistrement, analyse temps réel
- **Suivi d'objets** : algorithmes de tracking avancés
- **Détection de mouvement** : surveillance, analyse de scène
- **Apprentissage automatique** : SVM, KNN, réseaux de neurones
- **Deep Learning** : modèles pré-entraînés, YOLO, classification
- **Applications complètes** : surveillance, reconnaissance de plaques

### Domaines d'application

- **Sécurité** : Surveillance, détection d'intrusion, contrôle d'accès
- **Automobile** : Véhicules autonomes, assistance à la conduite
- **Médical** : Analyse d'images médicales, diagnostic assisté
- **Industrie** : Contrôle qualité, inspection automatique
- **Retail** : Comptage de personnes, analyse de comportement
- **Agriculture** : Surveillance de cultures, détection de maladies
- **Robotique** : Navigation, manipulation d'objets
- **Réalité augmentée** : Tracking, reconnaissance de marqueurs

### Pour aller plus loin

- **OpenCV Advanced** : Stéréovision, reconstruction 3D, SLAM
- **Performance** : CUDA, OpenCL, optimisations multi-thread
- **Deep Learning** : TensorFlow, PyTorch, création de modèles custom
- **Edge Computing** : Déploiement sur Raspberry Pi, Jetson Nano
- **Cloud** : API Azure Computer Vision, Google Cloud Vision

OpenCV continue d'évoluer avec de nouvelles fonctionnalités. La communauté est active et les ressources abondantes. N'hésitez pas à expérimenter et à contribuer !

---

**Prochaine étape recommandée** : Chapitre 12.11 - Accélération GPU (CUDA/OpenCL)

⏭️ [Accélération GPU (CUDA/OpenCL)](/12-interfaces-graphiques-avancees/11-acceleration-gpu-cuda-opencl.md)
