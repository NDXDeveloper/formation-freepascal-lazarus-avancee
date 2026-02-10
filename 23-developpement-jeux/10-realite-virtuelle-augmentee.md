🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 23.10 Réalité virtuelle et augmentée

## Introduction

La **réalité virtuelle** (VR) et la **réalité augmentée** (AR) représentent les frontières les plus excitantes du développement de jeux et d'applications interactives. Bien que ces technologies soient souvent associées à des moteurs comme Unity ou Unreal Engine, FreePascal et Lazarus peuvent également être utilisés pour créer des expériences immersives.

### Qu'est-ce que la réalité virtuelle (VR) ?

La **VR** plonge l'utilisateur dans un monde entièrement numérique. L'utilisateur porte un casque qui affiche des images stéréoscopiques et suit les mouvements de sa tête pour créer une illusion de présence dans un environnement virtuel.

**Exemples d'applications VR** :
- Jeux immersifs (Half-Life: Alyx, Beat Saber)
- Simulations d'entraînement (pilotage, chirurgie)
- Visites virtuelles (musées, architecture)
- Applications sociales (VRChat, Horizon Worlds)

### Qu'est-ce que la réalité augmentée (AR) ?

L'**AR** superpose des éléments numériques au monde réel. L'utilisateur voit son environnement réel enrichi d'informations ou d'objets virtuels, généralement via un smartphone, une tablette ou des lunettes spécialisées.

**Exemples d'applications AR** :
- Jeux (Pokémon GO)
- Navigation (flèches de direction superposées)
- Visualisation de produits (meubles dans votre salon)
- Éducation (anatomie en 3D superposée)

### Matériel requis

**Pour la VR** :
- Casques PC : Meta Quest (avec Link), HTC Vive, Valve Index, HP Reverb
- Casques autonomes : Meta Quest 2/3, Pico
- Ordinateur puissant (GPU performante, CPU récent)

**Pour l'AR** :
- Smartphones/tablettes (iOS ou Android)
- Lunettes AR : Microsoft HoloLens, Magic Leap
- Webcam (pour AR simple sur PC)

## Concepts fondamentaux de la VR

### 1. Rendu stéréoscopique

La VR nécessite deux images légèrement différentes, une pour chaque œil, créant l'illusion de profondeur.

```pascal
type
  TEye = (eyeLeft, eyeRight);

  TVRCamera = class
  private
    FPosition: TVector3;
    FRotation: TQuaternion;
    FIPD: Single;  // Inter-Pupillary Distance (distance entre les yeux)
  public
    function GetEyePosition(Eye: TEye): TVector3;
    function GetEyeViewMatrix(Eye: TEye): TMatrix4x4;
    function GetProjectionMatrix(Eye: TEye): TMatrix4x4;
  end;

function TVRCamera.GetEyePosition(Eye: TEye): TVector3;
var
  EyeOffset: Single;
begin
  // Distance typique entre les yeux : 63-65mm
  EyeOffset := FIPD / 2.0;

  case Eye of
    eyeLeft:  Result := FPosition + TVector3.Create(-EyeOffset, 0, 0);
    eyeRight: Result := FPosition + TVector3.Create(EyeOffset, 0, 0);
  end;
end;

procedure RenderStereo;
begin
  // Rendu pour l'œil gauche
  SetViewport(0, 0, RenderWidth div 2, RenderHeight);
  SetCamera(VRCamera.GetEyePosition(eyeLeft), VRCamera.GetEyeViewMatrix(eyeLeft));
  RenderScene;

  // Rendu pour l'œil droit
  SetViewport(RenderWidth div 2, 0, RenderWidth div 2, RenderHeight);
  SetCamera(VRCamera.GetEyePosition(eyeRight), VRCamera.GetEyeViewMatrix(eyeRight));
  RenderScene;
end;
```

### 2. Suivi de la position (Tracking)

Le casque VR suit constamment la position et l'orientation de la tête de l'utilisateur.

**Types de tracking** :
- **3DOF** (3 Degrees of Freedom) : Rotation uniquement (yaw, pitch, roll)
- **6DOF** (6 Degrees of Freedom) : Rotation + position (x, y, z)

```pascal
type
  THeadPose = record
    Position: TVector3;      // Position dans l'espace (x, y, z)
    Orientation: TQuaternion; // Rotation (quaternion)
  end;

  TVRTracker = class
  public
    function GetHeadPose: THeadPose; virtual; abstract;
    function GetControllerPose(Hand: TControllerHand): THeadPose; virtual; abstract;
  end;

procedure UpdateVRCamera(Pose: THeadPose);
begin
  Camera.Position := Pose.Position;
  Camera.Rotation := Pose.Orientation;
end;
```

### 3. Framerate et latence

**Critical** : La VR exige des performances exceptionnelles.

- **Framerate minimum** : 90 FPS (certains casques exigent 120 ou 144 FPS)
- **Latence maximale** : <20ms entre mouvement de tête et affichage
- **Conséquence** : Mal des transports (motion sickness) si performances insuffisantes

```pascal
const
  TARGET_VR_FPS = 90;
  FRAME_TIME_MS = 1000 div TARGET_VR_FPS;  // ~11ms

procedure VRGameLoop;
var
  FrameStart, FrameTime: QWord;
begin
  while Running do
  begin
    FrameStart := GetTickCount64;

    // Obtenir la pose actuelle
    CurrentPose := VRTracker.GetHeadPose;

    // Mettre à jour la caméra
    UpdateVRCamera(CurrentPose);

    // Mettre à jour la logique
    UpdateGame(DeltaTime);

    // Rendu stéréo
    RenderStereo;

    // Soumettre au casque
    SubmitFrameToHMD;

    FrameTime := GetTickCount64 - FrameStart;

    if FrameTime < FRAME_TIME_MS then
      Sleep(FRAME_TIME_MS - FrameTime);
  end;
end;
```

### 4. Confort et motion sickness

**Causes du mal des transports en VR** :
- Désaccord entre mouvement visuel et vestibulaire
- Framerate trop bas ou instable
- Latence trop élevée
- Accélérations brutales

**Techniques pour réduire le motion sickness** :
- Point de référence fixe (cockpit, nez virtuel)
- Téléportation au lieu de mouvement continu
- Vignetting lors des déplacements
- Snap rotation (rotation par paliers)

```pascal
procedure TeleportPlayer(TargetPosition: TVector3);
begin
  // Afficher un aperçu de la destination
  ShowTeleportPreview(TargetPosition);

  // Fade out
  FadeScreen(0.2);

  // Téléporter
  Player.Position := TargetPosition;

  // Fade in
  FadeScreen(0.2);
end;

procedure SmoothLocomotion(Direction: TVector3; Speed: Single; DeltaTime: Single);
begin
  // Ajouter un vignetting pendant le mouvement
  VignetteStrength := Speed / MaxSpeed;

  // Déplacer le joueur
  Player.Position := Player.Position + Direction * Speed * DeltaTime;
end;
```

## Intégration OpenVR/SteamVR

**OpenVR** est l'API de Valve pour la VR, compatible avec la plupart des casques PC.

### Installation

1. Installez **SteamVR** via Steam
2. Téléchargez le SDK OpenVR depuis GitHub
3. Créez des bindings Pascal pour OpenVR

### Structure du SDK OpenVR

```
openvr/
├── headers/
│   └── openvr.h
├── lib/
│   ├── win64/
│   │   └── openvr_api.dll
│   └── linux64/
│       └── libopenvr_api.so
└── bin/
```

### Bindings Pascal pour OpenVR

```pascal
unit OpenVR;

{$mode objfpc}{$H+}

interface

const
  {$IFDEF WINDOWS}
  OPENVR_API_LIB = 'openvr_api.dll';
  {$ENDIF}
  {$IFDEF LINUX}
  OPENVR_API_LIB = 'libopenvr_api.so';
  {$ENDIF}

type
  EVRInitError = Integer;

  TrackedDeviceIndex_t = UInt32;

  HmdMatrix34_t = array[0..2, 0..3] of Single;
  HmdMatrix44_t = array[0..3, 0..3] of Single;

  HmdVector3_t = record
    v: array[0..2] of Single;
  end;

  HmdQuaternion_t = record
    w, x, y, z: Single;
  end;

  TrackedDevicePose_t = record
    mDeviceToAbsoluteTracking: HmdMatrix34_t;
    vVelocity: HmdVector3_t;
    vAngularVelocity: HmdVector3_t;
    eTrackingResult: Integer;
    bPoseIsValid: Boolean;
    bDeviceIsConnected: Boolean;
  end;

  IVRSystem = Pointer;
  IVRCompositor = Pointer;

// Initialisation
function VR_InitInternal(peError: PInteger; eType: Integer): IVRSystem; cdecl; external OPENVR_API_LIB name 'VR_InitInternal';
procedure VR_ShutdownInternal; cdecl; external OPENVR_API_LIB name 'VR_ShutdownInternal';
function VR_IsHmdPresent: Boolean; cdecl; external OPENVR_API_LIB name 'VR_IsHmdPresent';

implementation

end.
```

### Wrapper de haut niveau

```pascal
unit VRWrapper;

{$mode objfpc}{$H+}

interface

uses
  OpenVR;

type
  TVRSystem = class
  private
    FSystem: IVRSystem;
    FCompositor: IVRCompositor;
    FInitialized: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    function Initialize: Boolean;
    procedure Shutdown;

    function GetDevicePose(DeviceIndex: TrackedDeviceIndex_t): TrackedDevicePose_t;
    function GetHMDMatrixProjection(Eye: TEye; NearZ, FarZ: Single): TMatrix4x4;

    procedure SubmitFrame(LeftTexture, RightTexture: GLuint);

    property Initialized: Boolean read FInitialized;
  end;

var
  VR: TVRSystem;

implementation

constructor TVRSystem.Create;
begin
  inherited Create;
  FInitialized := False;
end;

destructor TVRSystem.Destroy;
begin
  if FInitialized then
    Shutdown;
  inherited;
end;

function TVRSystem.Initialize: Boolean;
var
  Error: EVRInitError;
begin
  // Vérifier qu'un casque est présent
  if not VR_IsHmdPresent then
  begin
    WriteLn('Aucun casque VR détecté');
    Exit(False);
  end;

  // Initialiser OpenVR
  FSystem := VR_InitInternal(@Error, VRApplication_Scene);

  if Error <> VRInitError_None then
  begin
    WriteLn('Erreur OpenVR : ', Error);
    Exit(False);
  end;

  // Obtenir le compositor
  FCompositor := VR_GetGenericInterface(IVRCompositor_Version, @Error);

  FInitialized := True;
  Result := True;
end;

procedure TVRSystem.Shutdown;
begin
  if FInitialized then
  begin
    VR_ShutdownInternal;
    FInitialized := False;
  end;
end;

function TVRSystem.GetDevicePose(DeviceIndex: TrackedDeviceIndex_t): TrackedDevicePose_t;
var
  Poses: array[0..16] of TrackedDevicePose_t;
begin
  // Obtenir toutes les poses
  VRCompositor_WaitGetPoses(FCompositor, @Poses[0], Length(Poses), nil, 0);

  Result := Poses[DeviceIndex];
end;

procedure TVRSystem.SubmitFrame(LeftTexture, RightTexture: GLuint);
var
  LeftBounds, RightBounds: VRTextureBounds_t;
  LeftEye, RightEye: Texture_t;
begin
  // Définir les limites de texture (toute la texture)
  LeftBounds.uMin := 0.0;
  LeftBounds.uMax := 1.0;
  LeftBounds.vMin := 0.0;
  LeftBounds.vMax := 1.0;
  RightBounds := LeftBounds;

  // Configurer les textures
  LeftEye.handle := Pointer(PtrUInt(LeftTexture));
  LeftEye.eType := TextureType_OpenGL;
  LeftEye.eColorSpace := ColorSpace_Gamma;

  RightEye.handle := Pointer(PtrUInt(RightTexture));
  RightEye.eType := TextureType_OpenGL;
  RightEye.eColorSpace := ColorSpace_Gamma;

  // Soumettre au compositeur
  VRCompositor_Submit(FCompositor, Eye_Left, @LeftEye, @LeftBounds, 0);
  VRCompositor_Submit(FCompositor, Eye_Right, @RightEye, @RightBounds, 0);
end;

end.
```

### Application VR basique

```pascal
program SimpleVR;

{$mode objfpc}{$H+}

uses
  GL, GLext, VRWrapper;

var
  LeftEyeTexture, RightEyeTexture: GLuint;

procedure InitializeVR;
begin
  VR := TVRSystem.Create;

  if not VR.Initialize then
  begin
    WriteLn('Impossible d''initialiser la VR');
    Halt(1);
  end;

  WriteLn('VR initialisée avec succès');
end;

procedure CreateRenderTextures;
begin
  // Créer les textures pour chaque œil
  glGenTextures(1, @LeftEyeTexture);
  glGenTextures(1, @RightEyeTexture);

  // Configuration des textures (1512x1680 pour Quest 2)
  glBindTexture(GL_TEXTURE_2D, LeftEyeTexture);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, 1512, 1680, 0, GL_RGBA, GL_UNSIGNED_BYTE, nil);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

  // Idem pour l'œil droit
  glBindTexture(GL_TEXTURE_2D, RightEyeTexture);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, 1512, 1680, 0, GL_RGBA, GL_UNSIGNED_BYTE, nil);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
end;

procedure RenderEye(Eye: TEye; Texture: GLuint);
var
  Pose: TrackedDevicePose_t;
begin
  // Obtenir la pose du HMD
  Pose := VR.GetDevicePose(k_unTrackedDeviceIndex_Hmd);

  if Pose.bPoseIsValid then
  begin
    // Rendre vers la texture
    glBindFramebuffer(GL_FRAMEBUFFER, FBO);
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, Texture, 0);

    // Configurer la caméra pour cet œil
    SetupEyeCamera(Eye, Pose);

    // Rendre la scène
    RenderScene;
  end;
end;

procedure MainLoop;
begin
  while Running do
  begin
    // Rendre pour chaque œil
    RenderEye(eyeLeft, LeftEyeTexture);
    RenderEye(eyeRight, RightEyeTexture);

    // Soumettre les frames à OpenVR
    VR.SubmitFrame(LeftEyeTexture, RightEyeTexture);

    // Mettre à jour la logique
    UpdateGame;
  end;
end;

begin
  InitializeVR;
  CreateRenderTextures;

  MainLoop;

  VR.Free;
end.
```

## Contrôleurs VR

### Détection et tracking des contrôleurs

```pascal
type
  TControllerHand = (chLeft, chRight);

  TVRController = class
  private
    FDeviceIndex: TrackedDeviceIndex_t;
    FHand: TControllerHand;
    FPose: TrackedDevicePose_t;
  public
    function IsConnected: Boolean;
    function GetPosition: TVector3;
    function GetRotation: TQuaternion;

    function IsButtonPressed(Button: Integer): Boolean;
    function GetTriggerValue: Single;
    function GetJoystickAxis: TVector2;

    procedure Vibrate(Duration: Single; Intensity: Single);
  end;

function TVRController.GetPosition: TVector3;
var
  Matrix: HmdMatrix34_t;
begin
  Matrix := FPose.mDeviceToAbsoluteTracking;

  Result.X := Matrix[0, 3];
  Result.Y := Matrix[1, 3];
  Result.Z := Matrix[2, 3];
end;

function TVRController.IsButtonPressed(Button: Integer): Boolean;
var
  State: VRControllerState_t;
begin
  VRSystem_GetControllerState(VR.System, FDeviceIndex, @State, SizeOf(State));

  Result := (State.ulButtonPressed and (1 shl Button)) <> 0;
end;

function TVRController.GetTriggerValue: Single;
var
  State: VRControllerState_t;
begin
  VRSystem_GetControllerState(VR.System, FDeviceIndex, @State, SizeOf(State));

  // L'axe du trigger est généralement l'axe 1
  Result := State.rAxis[1].x;
end;

procedure TVRController.Vibrate(Duration: Single; Intensity: Single);
var
  MicroSeconds: Word;
begin
  // Convertir en microsecondes
  MicroSeconds := Round(Duration * 1000000 * Intensity);

  VRSystem_TriggerHapticPulse(VR.System, FDeviceIndex, 0, MicroSeconds);
end;
```

### Interactions VR courantes

#### 1. Pointage laser

```pascal
type
  TLaserPointer = class
  private
    FController: TVRController;
    FMaxDistance: Single;
  public
    function Raycast(out HitPoint: TVector3; out HitObject: TGameObject): Boolean;
    procedure Draw;
  end;

function TLaserPointer.Raycast(out HitPoint: TVector3; out HitObject: TGameObject): Boolean;
var
  Origin: TVector3;
  Direction: TVector3;
  EndPoint: TVector3;
begin
  // Origine = position du contrôleur
  Origin := FController.GetPosition;

  // Direction = avant du contrôleur
  Direction := FController.GetRotation.GetForwardVector;

  // Point final du rayon
  EndPoint := Origin + Direction * FMaxDistance;

  // Lancer le raycast
  Result := PhysicsRaycast(Origin, EndPoint, HitPoint, HitObject);
end;

procedure TLaserPointer.Draw;
var
  Origin, EndPoint: TVector3;
  HitPoint: TVector3;
  HitObject: TGameObject;
begin
  Origin := FController.GetPosition;

  if Raycast(HitPoint, HitObject) then
    EndPoint := HitPoint
  else
    EndPoint := Origin + FController.GetRotation.GetForwardVector * FMaxDistance;

  // Dessiner une ligne du contrôleur au point ciblé
  DrawLine3D(Origin, EndPoint, ColorRed);

  // Dessiner un point au point d'impact
  if HitObject <> nil then
    DrawSphere(HitPoint, 0.02, ColorWhite);
end;
```

#### 2. Saisie d'objets (Grab)

```pascal
type
  TGrabbableObject = class
  private
    FGrabbed: Boolean;
    FGrabbingController: TVRController;
    FGrabOffset: TVector3;
  public
    procedure Grab(Controller: TVRController);
    procedure Release;
    procedure Update;
  end;

procedure TGrabbableObject.Grab(Controller: TVRController);
begin
  FGrabbed := True;
  FGrabbingController := Controller;

  // Calculer l'offset entre le contrôleur et l'objet
  FGrabOffset := Self.Position - Controller.GetPosition;

  // Feedback haptique
  Controller.Vibrate(0.1, 0.5);
end;

procedure TGrabbableObject.Release;
begin
  if FGrabbed then
  begin
    // Appliquer la vélocité du contrôleur à l'objet
    Self.Velocity := FGrabbingController.GetVelocity;

    FGrabbed := False;
    FGrabbingController := nil;
  end;
end;

procedure TGrabbableObject.Update;
begin
  if FGrabbed then
  begin
    // Suivre le contrôleur
    Self.Position := FGrabbingController.GetPosition + FGrabOffset;
    Self.Rotation := FGrabbingController.GetRotation;
  end;
end;
```

#### 3. Téléportation

```pascal
type
  TTeleportSystem = class
  private
    FController: TVRController;
    FTeleportPreview: TGameObject;
    FTeleportArc: TList;
    FValidTeleport: Boolean;
  public
    procedure Update;
    procedure ExecuteTeleport;
  private
    procedure CalculateTeleportArc;
    procedure ShowTeleportPreview(Position: TVector3);
  end;

procedure TTeleportSystem.CalculateTeleportArc;
var
  Origin: TVector3;
  Direction: TVector3;
  Velocity: TVector3;
  Position: TVector3;
  Time: Single;
  i: Integer;
const
  ARC_SEGMENTS = 20;
  GRAVITY = 9.81;
  THROW_SPEED = 10.0;
begin
  FTeleportArc.Clear;

  Origin := FController.GetPosition;
  Direction := FController.GetRotation.GetForwardVector;
  Velocity := Direction * THROW_SPEED;

  FValidTeleport := False;

  for i := 0 to ARC_SEGMENTS do
  begin
    Time := i * 0.1;

    // Calcul balistique
    Position.X := Origin.X + Velocity.X * Time;
    Position.Y := Origin.Y + Velocity.Y * Time - 0.5 * GRAVITY * Time * Time;
    Position.Z := Origin.Z + Velocity.Z * Time;

    FTeleportArc.Add(Position);

    // Vérifier la collision avec le sol
    if IsGroundPosition(Position) then
    begin
      FValidTeleport := True;
      ShowTeleportPreview(Position);
      Break;
    end;
  end;
end;

procedure TTeleportSystem.ExecuteTeleport;
var
  TargetPosition: TVector3;
begin
  if FValidTeleport and FController.IsButtonPressed(BUTTON_TRIGGER) then
  begin
    TargetPosition := FTeleportPreview.Position;

    // Fade out
    FadeToBlack(0.2);

    // Téléporter le joueur
    Player.Position := TargetPosition;

    // Fade in
    FadeFromBlack(0.2);

    // Feedback
    FController.Vibrate(0.1, 0.8);
  end;
end;
```

## Réalité Augmentée (AR)

### Concepts fondamentaux AR

La réalité augmentée superpose du contenu numérique sur le monde réel. Les technologies principales sont :

1. **ARCore** (Google) - Android
2. **ARKit** (Apple) - iOS
3. **WebXR** - Navigateurs web
4. **Vuforia** - Multi-plateforme
5. **AR Foundation** - Unity (peut être utilisé avec Pascal via bindings)

### AR simple avec webcam

Pour commencer, créons un système AR basique utilisant une webcam et la détection de marqueurs.

```pascal
unit SimpleAR;

{$mode objfpc}{$H+}

interface

uses
  Classes, Graphics, OpenCV;

type
  TARMarker = record
    ID: Integer;
    Position: TVector3;
    Rotation: TMatrix3x3;
    Detected: Boolean;
  end;

  TARSystem = class
  private
    FCamera: TOpenCVCamera;
    FMarkers: array of TARMarker;
    FCameraMatrix: TMatrix3x3;
    FDistortionCoeffs: TVector;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Initialize;
    procedure Update;
    function DetectMarkers: Integer;

    procedure DrawVirtualObject(MarkerID: Integer; Obj: T3DObject);
  end;

implementation

constructor TARSystem.Create;
begin
  inherited Create;
  FCamera := TOpenCVCamera.Create;
end;

procedure TARSystem.Initialize;
begin
  // Ouvrir la webcam
  FCamera.Open(0);

  // Calibrer la caméra (à faire une fois)
  LoadCameraCalibration('camera_calibration.xml', FCameraMatrix, FDistortionCoeffs);
end;

function TARSystem.DetectMarkers: Integer;
var
  Frame: TOpenCVMat;
  GrayFrame: TOpenCVMat;
  Corners: TPointArray;
  IDs: TIntArray;
  i: Integer;
begin
  // Capturer une image
  Frame := FCamera.Read;

  // Convertir en niveaux de gris
  cvtColor(Frame, GrayFrame, COLOR_BGR2GRAY);

  // Détecter les marqueurs ArUco
  DetectMarkers(GrayFrame, ArucoDict_4x4_50, Corners, IDs);

  Result := Length(IDs);

  SetLength(FMarkers, Result);

  for i := 0 to Result - 1 do
  begin
    FMarkers[i].ID := IDs[i];
    FMarkers[i].Detected := True;

    // Estimer la pose du marqueur
    EstimatePose(Corners[i], FCameraMatrix, FDistortionCoeffs,
                 FMarkers[i].Position, FMarkers[i].Rotation);
  end;
end;

procedure TARSystem.DrawVirtualObject(MarkerID: Integer; Obj: T3DObject);
var
  i: Integer;
  Marker: TARMarker;
begin
  // Trouver le marqueur
  for i := 0 to High(FMarkers) do
  begin
    if (FMarkers[i].ID = MarkerID) and FMarkers[i].Detected then
    begin
      Marker := FMarkers[i];

      // Configurer la matrice de modèle-vue
      glMatrixMode(GL_MODELVIEW);
      glLoadIdentity;

      // Appliquer la transformation du marqueur
      glTranslatef(Marker.Position.X, Marker.Position.Y, Marker.Position.Z);
      ApplyRotationMatrix(Marker.Rotation);

      // Dessiner l'objet 3D
      Obj.Draw;

      Break;
    end;
  end;
end;

end.
```

### Application AR basique

```pascal
program ARDemo;

uses
  SimpleAR, OpenGL;

var
  AR: TARSystem;
  VirtualCube: T3DCube;

procedure Initialize;
begin
  AR := TARSystem.Create;
  AR.Initialize;

  VirtualCube := T3DCube.Create(0.1); // Cube de 10cm
  VirtualCube.LoadTexture('cube_texture.png');
end;

procedure MainLoop;
var
  MarkerCount: Integer;
begin
  while Running do
  begin
    // Détecter les marqueurs dans le flux vidéo
    MarkerCount := AR.DetectMarkers;

    // Afficher le flux vidéo en arrière-plan
    AR.DrawCameraBackground;

    // Dessiner les objets virtuels sur les marqueurs
    if MarkerCount > 0 then
    begin
      AR.DrawVirtualObject(0, VirtualCube);
    end;

    // Afficher
    SwapBuffers;
  end;
end;

begin
  Initialize;
  MainLoop;
  AR.Free;
end.
```

### AR sur mobile

Pour l'AR sur mobile, vous devrez utiliser des frameworks natifs ou créer des bindings.

#### Android avec ARCore

```pascal
unit ARCore_Bindings;

{$mode objfpc}{$H+}

interface

uses
  jni;

type
  TARSession = Pointer;
  TARFrame = Pointer;
  TARCamera = Pointer;
  TARPose = Pointer;

  TARTrackingState = (
    AR_TRACKING_STATE_TRACKING,
    AR_TRACKING_STATE_PAUSED,
    AR_TRACKING_STATE_STOPPED
  );

  TARCoreWrapper = class
  private
    FJNIEnv: PJNIEnv;
    FSession: TARSession;
  public
    constructor Create(JNIEnv: PJNIEnv);
    destructor Destroy; override;

    function Initialize: Boolean;
    function Update: TARFrame;
    function GetTrackingState: TARTrackingState;
    function GetCameraPose: TARPose;

    procedure DetectPlanes;
    function HitTest(ScreenX, ScreenY: Single): TARPose;
  end;

implementation

constructor TARCoreWrapper.Create(JNIEnv: PJNIEnv);
begin
  inherited Create;
  FJNIEnv := JNIEnv;
end;

function TARCoreWrapper.Initialize: Boolean;
begin
  // Créer une session ARCore via JNI
  // Code JNI pour appeler ARCore
  Result := True;
end;

function TARCoreWrapper.Update: TARFrame;
begin
  // Mettre à jour la frame ARCore
  // Retourner la frame courante
end;

function TARCoreWrapper.HitTest(ScreenX, ScreenY: Single): TARPose;
begin
  // Effectuer un hit test pour placer un objet
  // Retourner la pose où placer l'objet
end;

end.
```

#### iOS avec ARKit

```pascal
unit ARKit_Bindings;

{$mode objfpc}{$H+}
{$modeswitch objectivec1}

interface

uses
  CocoaAll, ARKit;

type
  TARKitWrapper = class
  private
    FARSession: ARSession;
    FARView: ARSCNView;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Start;
    procedure Pause;

    function GetCurrentFrame: ARFrame;
    function GetCameraTransform: TMatrix4x4;

    procedure AddAnchor(Position: TVector3);
    procedure DetectPlanes;
  end;

implementation

constructor TARKitWrapper.Create;
begin
  inherited Create;
  FARSession := ARSession.alloc.init;
end;

procedure TARKitWrapper.Start;
var
  Config: ARWorldTrackingConfiguration;
begin
  Config := ARWorldTrackingConfiguration.alloc.init;
  Config.setPlaneDetection(ARPlaneDetectionHorizontal);

  FARSession.runWithConfiguration(Config);
end;

function TARKitWrapper.GetCurrentFrame: ARFrame;
begin
  Result := FARSession.currentFrame;
end;

end.
```

### Suivi de plans (Plane Detection)

Les systèmes AR modernes peuvent détecter les surfaces planes du monde réel.

```pascal
type
  TARPlane = class
  private
    FCenter: TVector3;
    FExtent: TVector2;  // Largeur et hauteur
    FOrientation: TPlaneOrientation;
    FVertices: array of TVector3;
  public
    property Center: TVector3 read FCenter;
    property Extent: TVector2 read FExtent;

    function IsPointOnPlane(Point: TVector3): Boolean;
    procedure Draw;
  end;

  TPlaneOrientation = (poHorizontal, poVertical);

  TPlaneDetector = class
  private
    FDetectedPlanes: TList;
  public
    procedure Update(ARFrame: TARFrame);
    function GetPlaneAt(Position: TVector3): TARPlane;
    procedure DrawAllPlanes;
  end;

procedure TPlaneDetector.Update(ARFrame: TARFrame);
var
  Anchors: TArray;
  i: Integer;
  Anchor: TARAnchor;
  Plane: TARPlane;
begin
  // Obtenir tous les anchors du frame
  Anchors := ARFrame.GetAnchors;

  for i := 0 to High(Anchors) do
  begin
    if Anchors[i] is TARPlaneAnchor then
    begin
      Anchor := TARPlaneAnchor(Anchors[i]);

      // Créer ou mettre à jour le plan
      Plane := FindOrCreatePlane(Anchor.Identifier);
      Plane.FCenter := Anchor.Center;
      Plane.FExtent := Anchor.Extent;

      if Anchor.Alignment = ARPlaneAnchorAlignmentHorizontal then
        Plane.FOrientation := poHorizontal
      else
        Plane.FOrientation := poVertical;
    end;
  end;
end;

procedure TARPlane.Draw;
var
  i: Integer;
begin
  // Dessiner un plan semi-transparent
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  glColor4f(0.0, 1.0, 0.0, 0.3); // Vert transparent

  glBegin(GL_TRIANGLE_FAN);
  for i := 0 to High(FVertices) do
    glVertex3f(FVertices[i].X, FVertices[i].Y, FVertices[i].Z);
  glEnd;

  glDisable(GL_BLEND);
end;
```

### Placement d'objets en AR

```pascal
type
  TARObjectPlacer = class
  private
    FPlanes: TPlaneDetector;
    FPlacedObjects: TList;
  public
    function PlaceObject(ScreenX, ScreenY: Single; Obj: T3DObject): Boolean;
    procedure UpdateObjects(ARFrame: TARFrame);
    procedure DrawObjects;
  end;

function TARObjectPlacer.PlaceObject(ScreenX, ScreenY: Single; Obj: T3DObject): Boolean;
var
  HitResult: TARHitTestResult;
  WorldPosition: TVector3;
  Plane: TARPlane;
begin
  // Effectuer un hit test depuis la position de l'écran
  HitResult := ARSession.HitTest(ScreenX, ScreenY, ARHitTestResultTypeExistingPlane);

  if HitResult <> nil then
  begin
    WorldPosition := HitResult.WorldTransform.GetPosition;

    // Créer un objet à cette position
    Obj.Position := WorldPosition;

    // Aligner avec le plan détecté
    Plane := FPlanes.GetPlaneAt(WorldPosition);
    if Plane <> nil then
      Obj.Rotation := Plane.GetRotation;

    FPlacedObjects.Add(Obj);
    Result := True;
  end
  else
    Result := False;
end;

procedure TARObjectPlacer.UpdateObjects(ARFrame: TARFrame);
var
  i: Integer;
  Obj: T3DObject;
  CameraTransform: TMatrix4x4;
begin
  CameraTransform := ARFrame.Camera.Transform;

  for i := 0 to FPlacedObjects.Count - 1 do
  begin
    Obj := T3DObject(FPlacedObjects[i]);

    // Mettre à jour l'objet (animations, physique, etc.)
    Obj.Update;
  end;
end;

procedure TARObjectPlacer.DrawObjects;
var
  i: Integer;
  Obj: T3DObject;
begin
  for i := 0 to FPlacedObjects.Count - 1 do
  begin
    Obj := T3DObject(FPlacedObjects[i]);
    Obj.Draw;
  end;
end;
```

### Éclairage et ombres en AR

Pour que les objets virtuels semblent réels, ils doivent réagir à l'éclairage du monde réel.

```pascal
type
  TARLightEstimation = class
  private
    FAmbientIntensity: Single;
    FAmbientColor: TColor;
    FLightDirection: TVector3;
  public
    procedure Update(ARFrame: TARFrame);
    procedure ApplyToScene;
  end;

procedure TARLightEstimation.Update(ARFrame: TARFrame);
var
  LightEstimate: TARLightEstimate;
begin
  LightEstimate := ARFrame.LightEstimate;

  if LightEstimate <> nil then
  begin
    // Intensité lumineuse ambiante (0-2000 lumens)
    FAmbientIntensity := LightEstimate.AmbientIntensity / 1000.0;

    // Température de couleur
    FAmbientColor := ConvertTemperatureToColor(LightEstimate.AmbientColorTemperature);

    // Direction de la lumière (si disponible)
    if LightEstimate.PrimaryLightDirection <> nil then
      FLightDirection := LightEstimate.PrimaryLightDirection;
  end;
end;

procedure TARLightEstimation.ApplyToScene;
begin
  // Configurer l'éclairage OpenGL
  glEnable(GL_LIGHTING);
  glEnable(GL_LIGHT0);

  // Lumière ambiante
  glLightfv(GL_LIGHT0, GL_AMBIENT,
    @TVector4.Create(
      FAmbientColor.R * FAmbientIntensity,
      FAmbientColor.G * FAmbientIntensity,
      FAmbientColor.B * FAmbientIntensity,
      1.0
    ));

  // Direction de la lumière
  glLightfv(GL_LIGHT0, GL_POSITION,
    @TVector4.Create(FLightDirection.X, FLightDirection.Y, FLightDirection.Z, 0.0));
end;
```

### Ombres projetées

```pascal
type
  TARShadowRenderer = class
  private
    FShadowTexture: GLuint;
    FShadowMatrix: TMatrix4x4;
  public
    procedure Initialize;
    procedure RenderShadows(Objects: TList; Plane: TARPlane);
  end;

procedure TARShadowRenderer.RenderShadows(Objects: TList; Plane: TARPlane);
var
  i: Integer;
  Obj: T3DObject;
  LightDir: TVector3;
begin
  // Direction de la lumière (du soleil)
  LightDir := TVector3.Create(0.5, -1.0, 0.3).Normalize;

  // Créer une matrice de projection d'ombre sur le plan
  FShadowMatrix := CreateShadowMatrix(Plane.Center, Plane.Normal, LightDir);

  // Désactiver l'écriture dans le depth buffer
  glDepthMask(GL_FALSE);

  // Activer le blending pour des ombres semi-transparentes
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  // Couleur de l'ombre
  glColor4f(0.0, 0.0, 0.0, 0.5); // Noir semi-transparent

  for i := 0 to Objects.Count - 1 do
  begin
    Obj := T3DObject(Objects[i]);

    glPushMatrix;

    // Appliquer la matrice d'ombre
    glMultMatrixf(@FShadowMatrix);

    // Dessiner l'objet (qui sera projeté comme ombre)
    Obj.DrawSimplified; // Version simplifiée pour l'ombre

    glPopMatrix;
  end;

  glDisable(GL_BLEND);
  glDepthMask(GL_TRUE);
end;
```

## Reconnaissance d'images et objets

### Suivi d'images

Détecter et suivre des images du monde réel.

```pascal
type
  TImageTarget = record
    Name: string;
    ReferenceImage: TBitmap;
    PhysicalWidth: Single; // Largeur réelle en mètres
    Tracked: Boolean;
    Pose: TMatrix4x4;
  end;

  TARImageTracker = class
  private
    FTargets: array of TImageTarget;
    FDetector: TFeatureDetector;
  public
    procedure AddTarget(const Name: string; Image: TBitmap; Width: Single);
    procedure Update(ARFrame: TARFrame);
    function GetTrackedImage(const Name: string; out Pose: TMatrix4x4): Boolean;
  end;

procedure TARImageTracker.AddTarget(const Name: string; Image: TBitmap; Width: Single);
var
  Target: TImageTarget;
begin
  Target.Name := Name;
  Target.ReferenceImage := Image;
  Target.PhysicalWidth := Width;
  Target.Tracked := False;

  // Extraire les caractéristiques de l'image
  FDetector.ExtractFeatures(Image, Target.Features);

  SetLength(FTargets, Length(FTargets) + 1);
  FTargets[High(FTargets)] := Target;
end;

procedure TARImageTracker.Update(ARFrame: TARFrame);
var
  i: Integer;
  CameraImage: TBitmap;
  Matches: TFeatureMatches;
begin
  CameraImage := ARFrame.GetCameraImage;

  for i := 0 to High(FTargets) do
  begin
    // Chercher l'image cible dans le frame
    Matches := FDetector.MatchFeatures(CameraImage, FTargets[i].ReferenceImage);

    if Matches.Count > MIN_MATCHES then
    begin
      FTargets[i].Tracked := True;
      FTargets[i].Pose := EstimatePoseFromMatches(Matches, FTargets[i].PhysicalWidth);
    end
    else
      FTargets[i].Tracked := False;
  end;
end;
```

### Application pratique : Catalogue AR

```pascal
program ARCatalog;

uses
  ARCore, ImageTracking, Models3D;

var
  AR: TARSystem;
  ImageTracker: TARImageTracker;
  ProductModels: array of T3DModel;

procedure InitializeAR;
begin
  AR := TARSystem.Create;
  AR.Initialize;

  ImageTracker := TARImageTracker.Create;

  // Ajouter les images de produits à détecter
  ImageTracker.AddTarget('product_001', LoadBitmap('markers/product_001.jpg'), 0.15);
  ImageTracker.AddTarget('product_002', LoadBitmap('markers/product_002.jpg'), 0.15);

  // Charger les modèles 3D correspondants
  ProductModels[0] := Load3DModel('models/product_001.obj');
  ProductModels[1] := Load3DModel('models/product_002.obj');
end;

procedure MainLoop;
var
  Frame: TARFrame;
  Pose: TMatrix4x4;
begin
  while Running do
  begin
    Frame := AR.Update;

    // Mettre à jour le suivi d'images
    ImageTracker.Update(Frame);

    // Afficher le flux de la caméra
    DrawCameraBackground(Frame);

    // Afficher les modèles 3D sur les images détectées
    if ImageTracker.GetTrackedImage('product_001', Pose) then
      DrawModelAtPose(ProductModels[0], Pose);

    if ImageTracker.GetTrackedImage('product_002', Pose) then
      DrawModelAtPose(ProductModels[1], Pose);

    SwapBuffers;
  end;
end;

begin
  InitializeAR;
  MainLoop;
end.
```

## Audio spatial en VR/AR

Le son est crucial pour l'immersion. L'audio spatial donne l'impression que les sons proviennent de positions spécifiques dans l'espace 3D.

```pascal
uses
  OpenAL;

type
  TSpatialAudioSource = class
  private
    FSource: ALuint;
    FPosition: TVector3;
    FVelocity: TVector3;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetPosition(Position: TVector3);
    procedure SetVelocity(Velocity: TVector3);
    procedure Play(Sound: TALBuffer);
    procedure Update;
  end;

constructor TSpatialAudioSource.Create;
begin
  inherited Create;
  alGenSources(1, @FSource);

  // Configurer pour l'audio 3D
  alSourcef(FSource, AL_ROLLOFF_FACTOR, 1.0);
  alSourcef(FSource, AL_REFERENCE_DISTANCE, 1.0);
  alSourcef(FSource, AL_MAX_DISTANCE, 100.0);
end;

procedure TSpatialAudioSource.SetPosition(Position: TVector3);
begin
  FPosition := Position;
  alSource3f(FSource, AL_POSITION, Position.X, Position.Y, Position.Z);
end;

procedure TSpatialAudioSource.Update;
begin
  // Mettre à jour la position et la vélocité
  alSource3f(FSource, AL_POSITION, FPosition.X, FPosition.Y, FPosition.Z);
  alSource3f(FSource, AL_VELOCITY, FVelocity.X, FVelocity.Y, FVelocity.Z);
end;

// Listener (joueur/caméra)
procedure UpdateAudioListener(Camera: TVRCamera);
var
  Position, Forward, Up: TVector3;
  Orientation: array[0..5] of Single;
begin
  Position := Camera.Position;
  Forward := Camera.GetForwardVector;
  Up := Camera.GetUpVector;

  // Position du listener
  alListener3f(AL_POSITION, Position.X, Position.Y, Position.Z);

  // Orientation (forward et up)
  Orientation[0] := Forward.X;
  Orientation[1] := Forward.Y;
  Orientation[2] := Forward.Z;
  Orientation[3] := Up.X;
  Orientation[4] := Up.Y;
  Orientation[5] := Up.Z;

  alListenerfv(AL_ORIENTATION, @Orientation[0]);
end;
```

## Interface utilisateur en VR

Les interfaces 2D traditionnelles ne fonctionnent pas bien en VR. Il faut des interfaces 3D ou "dans l'espace".

### Menu 3D flottant

```pascal
type
  TVRMenu = class
  private
    FPosition: TVector3;
    FButtons: array of TVRButton;
    FCanvas: TTexture;
  public
    procedure Draw;
    function CheckInteraction(Controller: TVRController): Integer;
  end;

  TVRButton = record
    Text: string;
    Rect: TRect;
    Position3D: TVector3;
    Size: TVector2;
    Hovered: Boolean;
    OnClick: TNotifyEvent;
  end;

procedure TVRMenu.Draw;
var
  i: Integer;
  Button: TVRButton;
begin
  // Dessiner un panel 3D
  glPushMatrix;

  // Positionner le menu dans l'espace
  glTranslatef(FPosition.X, FPosition.Y, FPosition.Z);

  // Faire face au joueur
  BillboardToCamera;

  // Dessiner le fond du menu
  DrawQuad(TVector3.Zero, 0.5, 0.7, FCanvas);

  // Dessiner chaque bouton
  for i := 0 to High(FButtons) do
  begin
    Button := FButtons[i];

    if Button.Hovered then
      glColor3f(0.8, 0.8, 1.0) // Surbrillance
    else
      glColor3f(0.6, 0.6, 0.6);

    DrawButton(Button);
  end;

  glPopMatrix;
end;

function TVRMenu.CheckInteraction(Controller: TVRController): Integer;
var
  i: Integer;
  Ray: TRay;
  Intersection: TVector3;
begin
  Result := -1;

  // Créer un rayon depuis le contrôleur
  Ray.Origin := Controller.GetPosition;
  Ray.Direction := Controller.GetForwardVector;

  for i := 0 to High(FButtons) do
  begin
    // Vérifier l'intersection avec le bouton
    if RayIntersectsButton(Ray, FButtons[i], Intersection) then
    begin
      FButtons[i].Hovered := True;

      // Détecter le clic
      if Controller.IsButtonPressed(BUTTON_TRIGGER) then
      begin
        if Assigned(FButtons[i].OnClick) then
          FButtons[i].OnClick(Self);
        Result := i;
      end;
    end
    else
      FButtons[i].Hovered := False;
  end;
end;
```

### HUD attaché au regard

```pascal
type
  TVRHUD = class
  private
    FElements: TList;
    FFollowHead: Boolean;
    FDistance: Single;
  public
    procedure Update(HeadPose: THeadPose);
    procedure Draw;

    procedure AddElement(Element: TVRHUDElement);
  end;

procedure TVRHUD.Update(HeadPose: THeadPose);
var
  TargetPosition: TVector3;
  ForwardVector: TVector3;
begin
  if FFollowHead then
  begin
    // Calculer la position devant le joueur
    ForwardVector := HeadPose.Orientation.GetForwardVector;
    TargetPosition := HeadPose.Position + ForwardVector * FDistance;

    // Interpoler doucement pour éviter les mouvements brusques
    FPosition := Lerp(FPosition, TargetPosition, 0.1);
  end;
end;

procedure TVRHUD.Draw;
var
  i: Integer;
begin
  // Désactiver le depth test pour que le HUD soit toujours visible
  glDisable(GL_DEPTH_TEST);

  glPushMatrix;

  // Positionner le HUD
  glTranslatef(FPosition.X, FPosition.Y, FPosition.Z);
  BillboardToCamera;

  // Dessiner chaque élément
  for i := 0 to FElements.Count - 1 do
    TVRHUDElement(FElements[i]).Draw;

  glPopMatrix;

  glEnable(GL_DEPTH_TEST);
end;
```

## Performance et optimisation VR/AR

### Techniques spécifiques VR

#### 1. Foveated Rendering

Rendre en haute résolution uniquement au centre de la vision, où l'œil peut voir les détails.

```pascal
type
  TFoveatedRenderer = class
  private
    FCenterTexture: GLuint;      // Haute résolution
    FPeripheralTexture: GLuint;  // Basse résolution
  public
    procedure Render(Eye: TEye);
  end;

procedure TFoveatedRenderer.Render(Eye: TEye);
begin
  // Rendre le centre en haute résolution
  glBindFramebuffer(GL_FRAMEBUFFER, FCenterFBO);
  glViewport(0, 0, 1512, 1680); // Résolution complète
  RenderSceneCenter(Eye);

  // Rendre la périphérie en basse résolution
  glBindFramebuffer(GL_FRAMEBUFFER, FPeripheralFBO);
  glViewport(0, 0, 756, 840); // Moitié de la résolution
  RenderScenePeripheral(Eye);

  // Combiner les deux textures
  CompositeTextures(FCenterTexture, FPeripheralTexture);
end;
```

#### 2. Asynchronous Timewarp

Reprojeter la dernière image si le rendu prend trop de temps.

```pascal
procedure AsyncTimewarp(LastFrame: TTexture; CurrentPose, LastPose: THeadPose);
var
  ReprojectionMatrix: TMatrix4x4;
begin
  // Calculer la différence de rotation
  ReprojectionMatrix := CalculateReprojection(CurrentPose, LastPose);

  // Appliquer la reprojection à la texture
  ApplyWarpToTexture(LastFrame, ReprojectionMatrix);

  // Soumettre au HMD
  SubmitToHMD(LastFrame);
end;
```

#### 3. Multi-view rendering

Rendre les deux yeux simultanément avec des extensions OpenGL.

```pascal
procedure MultiViewRender;
begin
  // Activer l'extension multi-view
  glEnable(GL_OVR_multiview);

  // Configurer les deux viewports
  glFramebufferTextureMultiviewOVR(
    GL_FRAMEBUFFER,
    GL_COLOR_ATTACHMENT0,
    StereoTexture,
    0,  // Level
    0,  // BaseViewIndex
    2   // NumViews
  );

  // Un seul rendu pour les deux yeux
  RenderScene;
end;
```

## Tests et débogage VR/AR

### Simulateur VR pour le développement

```pascal
type
  TVRSimulator = class(TVRSystem)
  private
    FMouseLook: Boolean;
    FSimulatedPose: THeadPose;
  public
    procedure Update; override;
    procedure HandleInput(Event: TInputEvent);
  end;

procedure TVRSimulator.Update;
begin
  // Simuler le mouvement de tête avec la souris
  if FMouseLook then
  begin
    FSimulatedPose.Orientation := FSimulatedPose.Orientation *
      TQuaternion.FromEuler(MouseDeltaY * 0.01, MouseDeltaX * 0.01, 0);
  end;

  // Simuler le déplacement avec WASD
  if IsKeyPressed(KEY_W) then
    FSimulatedPose.Position.Z := FSimulatedPose.Position.Z - 0.1;
  if IsKeyPressed(KEY_S) then
    FSimulatedPose.Position.Z := FSimulatedPose.Position.Z + 0.1;
  if IsKeyPressed(KEY_A) then
    FSimulatedPose.Position.X := FSimulatedPose.Position.X - 0.1;
  if IsKeyPressed(KEY_D) then
    FSimulatedPose.Position.X := FSimulatedPose.Position.X + 0.1;
end;
```

### Métriques de performance VR

```pascal
type
  TVRPerformanceMonitor = class
  private
    FFrameTimes: array[0..99] of Single;
    FFrameIndex: Integer;
  public
    procedure RecordFrame(FrameTime: Single);
    function GetAverageFPS: Single;
    function GetPercentile95: Single;
    procedure PrintReport;
  end;

procedure TVRPerformanceMonitor.PrintReport;
begin
  WriteLn('=== VR Performance Report ===');
  WriteLn('Average FPS: ', GetAverageFPS:0:2);
  WriteLn('95th Percentile: ', GetPercentile95:0:2, ' ms');
  WriteLn('Min FPS: ', GetMinFPS:0:2);
  WriteLn('Dropped frames: ', GetDroppedFrames);

  if GetAverageFPS < 90 then
    WriteLn('WARNING: FPS below target! Motion sickness risk!');
end;
```

## Conseils et bonnes pratiques

### VR

1. **Framerate constant** : 90 FPS minimum, jamais en dessous
2. **Latence faible** : <20ms du mouvement à l'affichage
3. **Échelle correcte** : 1 unité = 1 mètre dans le monde virtuel
4. **Éviter les accélérations** : Déplacements à vitesse constante ou téléportation
5. **Point de référence** : Cockpit, corps virtuel ou horizon fixe
6. **Contraste modéré** : Éviter le noir absolu et le blanc pur
7. **Texte lisible** : Grande taille, fort contraste
8. **Audio spatial** : Renforcer l'immersion

### AR

1. **Suivi robuste** : Gérer la perte de tracking gracieusement
2. **Éclairage réaliste** : Adapter les objets virtuels à la lumière réelle
3. **Occlusion** : Objets virtuels cachés par des objets réels
4. **Ombres** : Projeter des ombres sur les surfaces réelles
5. **Échelle appropriée** : Objets virtuels à la bonne taille
6. **Feedback visuel** : Montrer où l'utilisateur peut placer des objets
7. **Performance** : Éviter la surchauffe sur mobile

### Général

1. **Tester sur le matériel réel** : Les simulateurs ne suffisent pas
2. **Accessibilité** : Options pour réduire le motion sickness
3. **Tutoriel intégré** : Expliquer les contrôles en VR/AR
4. **Sauvegarde fréquente** : En cas de crash ou perte de tracking
5. **Options de confort** : Vignetting, snap turn, téléportation

## Ressources et bibliothèques

### Pour FreePascal/Lazarus

- **Castle Game Engine** : Supporte OpenXR et différents casques VR
- **OpenGL** : Base pour le rendu 3D
- **OpenAL** : Audio spatial
- **Bindings personnalisés** : Pour OpenVR, ARCore, ARKit

### Documentation

- OpenVR SDK Documentation
- ARCore Developer Guide
- ARKit Documentation
- OpenXR Specification
- Oculus Developer Center

### Outils

- **SteamVR** : Test et développement VR PC
- **Oculus Developer Hub** : Pour Quest
- **ARCore Cloud Anchors** : Expériences AR partagées
- **Vuforia** : Reconnaissance d'images AR

## Projets pratiques

### Projet 1 : Galerie d'art VR

Un projet simple pour découvrir la VR.

```pascal
program VRGallery;

uses
  VRWrapper, OpenGL, Models3D;

type
  TArtwork = class
    Position: TVector3;
    Texture: GLuint;
    Title: string;
    Artist: string;
  end;

var
  VR: TVRSystem;
  Artworks: array of TArtwork;
  Gallery: T3DModel;

procedure LoadGallery;
var
  i: Integer;
begin
  // Charger le modèle de la galerie
  Gallery := Load3DModel('models/gallery_room.obj');

  // Charger les œuvres d'art
  SetLength(Artworks, 10);
  for i := 0 to 9 do
  begin
    Artworks[i] := TArtwork.Create;
    Artworks[i].Texture := LoadTexture(Format('art/painting_%d.jpg', [i]));
    Artworks[i].Position := GetWallPosition(i);
  end;
end;

procedure RenderGallery(Eye: TEye);
begin
  // Dessiner la salle
  Gallery.Draw;

  // Dessiner chaque œuvre
  for i := 0 to High(Artworks) do
  begin
    glPushMatrix;
    glTranslatef(Artworks[i].Position.X, Artworks[i].Position.Y, Artworks[i].Position.Z);

    // Cadre
    DrawFrame(1.0, 1.5, 0.05);

    // Image
    glBindTexture(GL_TEXTURE_2D, Artworks[i].Texture);
    DrawQuad(0.9, 1.4);

    glPopMatrix;
  end;
end;

procedure CheckInteraction;
var
  Controller: TVRController;
  Ray: TRay;
  HitArtwork: TArtwork;
begin
  Controller := VR.GetController(chRight);
  Ray := Controller.GetLaserRay;

  HitArtwork := RaycastArtworks(Ray);

  if HitArtwork <> nil then
  begin
    // Afficher les informations
    ShowArtInfo(HitArtwork);

    if Controller.IsButtonPressed(BUTTON_TRIGGER) then
    begin
      // Zoom sur l'œuvre
      ZoomOnArtwork(HitArtwork);
    end;
  end;
end;

procedure MainLoop;
begin
  while Running do
  begin
    VR.Update;

    CheckInteraction;

    // Rendu stéréo
    RenderGallery(eyeLeft);
    RenderGallery(eyeRight);

    VR.SubmitFrame;
  end;
end;

begin
  VR := TVRSystem.Create;
  VR.Initialize;

  LoadGallery;
  MainLoop;

  VR.Free;
end.
```

### Projet 2 : Jeu de puzzle AR

Placer des pièces de puzzle dans le monde réel.

```pascal
program ARPuzzle;

uses
  ARCore, PuzzleGame;

type
  TPuzzlePiece = class
    ID: Integer;
    Position: TVector3;
    Rotation: TQuaternion;
    Model: T3DModel;
    CorrectPosition: TVector3;
    Placed: Boolean;
  end;

var
  AR: TARSystem;
  Pieces: array[0..8] of TPuzzlePiece;
  PuzzlePlane: TARPlane;

procedure InitializePuzzle;
var
  i: Integer;
begin
  for i := 0 to 8 do
  begin
    Pieces[i] := TPuzzlePiece.Create;
    Pieces[i].ID := i;
    Pieces[i].Model := Load3DModel(Format('puzzle/piece_%d.obj', [i]));
    Pieces[i].Placed := False;
  end;
end;

function FindPlane: Boolean;
var
  Frame: TARFrame;
begin
  Frame := AR.Update;

  // Détecter un plan horizontal
  PuzzlePlane := AR.DetectHorizontalPlane;
  Result := PuzzlePlane <> nil;
end;

procedure PlacePuzzleBoard;
begin
  // Placer le plateau de puzzle sur le plan détecté
  PuzzleBoard.Position := PuzzlePlane.Center;

  // Calculer les positions correctes des pièces
  for i := 0 to 8 do
  begin
    Row := i div 3;
    Col := i mod 3;

    Pieces[i].CorrectPosition := PuzzleBoard.Position +
      TVector3.Create((Col - 1) * 0.1, 0, (Row - 1) * 0.1);
  end;
end;

procedure HandleTouch(TouchX, TouchY: Single);
var
  HitResult: TARHitTestResult;
  SelectedPiece: TPuzzlePiece;
begin
  HitResult := AR.HitTest(TouchX, TouchY);

  if HitResult <> nil then
  begin
    // Vérifier si on touche une pièce
    SelectedPiece := FindPieceAtPosition(HitResult.WorldPosition);

    if SelectedPiece <> nil then
    begin
      // Déplacer la pièce
      SelectedPiece.Position := HitResult.WorldPosition;

      // Vérifier si elle est à la bonne position
      if IsNearCorrectPosition(SelectedPiece) then
      begin
        SnapToPosition(SelectedPiece);
        PlaySnapSound;

        if AllPiecesPlaced then
          ShowVictory;
      end;
    end;
  end;
end;

procedure RenderPuzzle;
var
  i: Integer;
begin
  // Dessiner le fond vidéo
  AR.DrawCameraBackground;

  // Dessiner le plateau
  if PuzzlePlane <> nil then
    DrawPuzzleBoard;

  // Dessiner les pièces
  for i := 0 to 8 do
  begin
    glPushMatrix;
    ApplyTransform(Pieces[i].Position, Pieces[i].Rotation);

    if Pieces[i].Placed then
      glColor3f(0.5, 1.0, 0.5) // Vert si placée
    else
      glColor3f(1.0, 1.0, 1.0); // Blanc si libre

    Pieces[i].Model.Draw;
    glPopMatrix;
  end;
end;

procedure MainLoop;
begin
  // Phase 1 : Détecter un plan
  ShowMessage('Scannez une surface plane...');
  while not FindPlane do
    AR.Update;

  PlacePuzzleBoard;
  ShowMessage('Placez les pièces !');

  // Phase 2 : Jouer
  while Running and not AllPiecesPlaced do
  begin
    AR.Update;

    if TouchDetected then
      HandleTouch(TouchX, TouchY);

    RenderPuzzle;
    SwapBuffers;
  end;
end;

begin
  AR := TARSystem.Create;
  AR.Initialize;

  InitializePuzzle;
  MainLoop;

  AR.Free;
end.
```

### Projet 3 : Entraîneur de boxe VR

Un jeu d'entraînement sportif en VR.

```pascal
program VRBoxingTrainer;

uses
  VRWrapper, PhysicsEngine;

type
  TPunchingBag = class
    Position: TVector3;
    Velocity: TVector3;
    Rotation: TQuaternion;
    SwingAngle: Single;
    Model: T3DModel;

    procedure Update(DeltaTime: Single);
    procedure OnHit(HitPosition: TVector3; Force: Single);
  end;

  TTarget = class
    Position: TVector3;
    Active: Boolean;
    TimeLeft: Single;
    Color: TColor;
  end;

var
  VR: TVRSystem;
  LeftController, RightController: TVRController;
  PunchingBag: TPunchingBag;
  Targets: array of TTarget;
  Score: Integer;
  Combo: Integer;

procedure TPunchingBag.Update(DeltaTime: Single);
const
  DAMPING = 0.95;
  GRAVITY = 9.81;
begin
  // Physique simple du sac de frappe
  FSwingAngle := FSwingAngle + FVelocity.X * DeltaTime;
  FVelocity.X := FVelocity.X * DAMPING;

  // Force de rappel (comme un pendule)
  FVelocity.X := FVelocity.X - FSwingAngle * 2.0 * DeltaTime;

  // Calculer la position du sac
  FPosition.X := Sin(FSwingAngle) * 0.5;
  FPosition.Y := 1.5 - Cos(FSwingAngle) * 0.5;
end;

procedure TPunchingBag.OnHit(HitPosition: TVector3; Force: Single);
begin
  // Appliquer l'impulsion
  FVelocity := FVelocity + (HitPosition - FPosition).Normalize * Force;

  // Feedback haptique
  VR.GetController(chRight).Vibrate(0.1, Force / 10.0);

  // Score
  Inc(Score, Round(Force * 10));
  Inc(Combo);

  PlayHitSound(Force);
end;

procedure SpawnTarget;
var
  Target: TTarget;
  Angle, Distance: Single;
begin
  Target := TTarget.Create;

  // Position aléatoire devant le joueur
  Angle := Random * Pi * 0.5 - Pi * 0.25; // ±45°
  Distance := 1.5 + Random * 0.5;

  Target.Position := TVector3.Create(
    Sin(Angle) * Distance,
    1.0 + Random * 0.8, // Hauteur entre 1m et 1.8m
    Cos(Angle) * Distance
  );

  Target.Active := True;
  Target.TimeLeft := 2.0; // 2 secondes pour frapper
  Target.Color := RandomColor;

  SetLength(Targets, Length(Targets) + 1);
  Targets[High(Targets)] := Target;
end;

procedure UpdateTargets(DeltaTime: Single);
var
  i: Integer;
  Target: TTarget;
  ControllerPos: TVector3;
  Distance: Single;
begin
  for i := High(Targets) downto 0 do
  begin
    Target := Targets[i];

    if Target.Active then
    begin
      Target.TimeLeft := Target.TimeLeft - DeltaTime;

      // Vérifier collision avec les contrôleurs
      ControllerPos := LeftController.GetPosition;
      Distance := (ControllerPos - Target.Position).Length;

      if Distance < 0.15 then // 15cm de rayon
      begin
        HitTarget(Target, LeftController);
        Target.Active := False;
      end;

      ControllerPos := RightController.GetPosition;
      Distance := (ControllerPos - Target.Position).Length;

      if Distance < 0.15 then
      begin
        HitTarget(Target, RightController);
        Target.Active := False;
      end;

      // Expiration
      if Target.TimeLeft <= 0 then
      begin
        Target.Active := False;
        Combo := 0; // Réinitialiser le combo
      end;
    end
    else
    begin
      // Supprimer la cible inactive
      Target.Free;
      Delete(Targets, i, 1);
    end;
  end;
end;

procedure HitTarget(Target: TTarget; Controller: TVRController);
var
  Speed: Single;
begin
  Speed := Controller.GetVelocity.Length;

  Inc(Score, Round(Speed * 100) * Combo);
  Inc(Combo);

  Controller.Vibrate(0.1, 1.0);
  PlayTargetHitSound;
  CreateParticles(Target.Position, Target.Color);
end;

procedure RenderScene(Eye: TEye);
var
  i: Integer;
begin
  // Dessiner l'environnement (salle de gym)
  DrawGymEnvironment;

  // Dessiner le sac de frappe
  glPushMatrix;
  ApplyTransform(PunchingBag.Position, PunchingBag.Rotation);
  PunchingBag.Model.Draw;
  glPopMatrix;

  // Dessiner les cibles
  for i := 0 to High(Targets) do
  begin
    if Targets[i].Active then
    begin
      DrawTarget(Targets[i]);

      // Barre de temps
      DrawTimerBar(Targets[i].Position, Targets[i].TimeLeft / 2.0);
    end;
  end;

  // Dessiner les contrôleurs (gants de boxe)
  DrawBoxingGlove(LeftController);
  DrawBoxingGlove(RightController);

  // HUD
  DrawScoreHUD(Score, Combo);
end;

procedure MainLoop;
var
  LastTime, CurrentTime: QWord;
  DeltaTime: Single;
  TargetSpawnTimer: Single;
begin
  LastTime := GetTickCount64;
  TargetSpawnTimer := 0;

  while Running do
  begin
    CurrentTime := GetTickCount64;
    DeltaTime := (CurrentTime - LastTime) / 1000.0;
    LastTime := CurrentTime;

    // Obtenir les poses des contrôleurs
    LeftController.Update;
    RightController.Update;

    // Mettre à jour le sac de frappe
    PunchingBag.Update(DeltaTime);

    // Vérifier collision avec le sac
    CheckBagCollision(LeftController);
    CheckBagCollision(RightController);

    // Mettre à jour les cibles
    UpdateTargets(DeltaTime);

    // Faire apparaître de nouvelles cibles
    TargetSpawnTimer := TargetSpawnTimer + DeltaTime;
    if TargetSpawnTimer >= 1.5 then
    begin
      SpawnTarget;
      TargetSpawnTimer := 0;
    end;

    // Rendu stéréo
    RenderScene(eyeLeft);
    RenderScene(eyeRight);

    VR.SubmitFrame;
  end;
end;

begin
  VR := TVRSystem.Create;
  VR.Initialize;

  LeftController := VR.GetController(chLeft);
  RightController := VR.GetController(chRight);

  PunchingBag := TPunchingBag.Create;
  PunchingBag.Model := Load3DModel('models/punching_bag.obj');

  Score := 0;
  Combo := 0;

  MainLoop;

  VR.Free;
end.
```

## Tendances futures

### WebXR

La VR/AR dans le navigateur web devient de plus en plus accessible.

```pascal
// Exemple conceptuel de bindings WebXR pour pas2js
type
  TWebXRSession = class external name 'XRSession'
  public
    procedure requestAnimationFrame(callback: TJSFunction);
    function end_: TJSPromise; external name 'end';
  end;

  TWebXRFrame = class external name 'XRFrame'
  public
    function getViewerPose(refSpace: TXRReferenceSpace): TXRViewerPose;
  end;

procedure InitWebXR;
begin
  if navigator.xr.isSessionSupported('immersive-vr') then
  begin
    navigator.xr.requestSession('immersive-vr').then_(
      procedure(session: TWebXRSession)
      begin
        StartVRSession(session);
      end
    );
  end;
end;
```

### Mixed Reality (MR)

Fusion entre VR et AR, où les objets virtuels interagissent avec les objets réels.

**Exemples** :
- Microsoft HoloLens
- Meta Quest 3 (avec passthrough couleur)
- Apple Vision Pro

```pascal
type
  TMixedRealitySystem = class
  public
    procedure DetectRealObjects;
    function GetRoomMesh: T3DMesh;
    procedure EnablePassthrough;
    procedure PlaceVirtualObject(Position: TVector3; InteractWithReal: Boolean);
  end;
```

### Eye Tracking et Foveated Rendering

Suivre le regard de l'utilisateur pour optimiser le rendu.

```pascal
type
  TEyeTracker = class
  public
    function GetGazeDirection: TVector3;
    function GetGazePoint: TVector3;
    function IsLookingAt(Obj: TGameObject): Boolean;
  end;

// Foveated rendering basé sur le regard
procedure RenderWithEyeTracking(EyeTracker: TEyeTracker);
var
  GazePoint: TVector3;
begin
  GazePoint := EyeTracker.GetGazePoint;

  // Haute résolution au point de regard
  RenderHighRes(GazePoint, Radius := 0.2);

  // Basse résolution en périphérie
  RenderLowRes(OutsideRadius := 0.2);
end;
```

### Hand Tracking

Suivi des mains sans contrôleurs.

```pascal
type
  THandPose = record
    Wrist: TVector3;
    Fingers: array[0..4] of record
      Joints: array[0..3] of TVector3;
    end;
  end;

  THandTracker = class
  public
    function GetHandPose(Hand: THandType): THandPose;
    function IsPinching: Boolean;
    function IsGrabbing: Boolean;
    function IsPointing: Boolean;
  end;
```

### Social VR

Expériences multi-utilisateurs en VR.

```pascal
type
  TVRAvatar = class
    UserID: string;
    HeadPose: THeadPose;
    LeftHandPose: THandPose;
    RightHandPose: THandPose;
    VoiceChannel: TAudioChannel;

    procedure Update;
    procedure Draw;
  end;

  TSocialVRRoom = class
  private
    FAvatars: TList;
  public
    procedure JoinRoom(UserID: string);
    procedure LeaveRoom(UserID: string);
    procedure BroadcastPose(LocalPose: THeadPose);
    procedure UpdateAvatars;
  end;
```

## Ressources complémentaires

### Tutoriels et cours

- **YouTube** : Nombreux tutoriels sur OpenVR et ARCore
- **Udemy/Coursera** : Cours sur le développement VR/AR
- **Documentation officielle** : OpenXR, OpenVR, ARCore, ARKit

### Communautés

- **Reddit** : r/virtualreality, r/augmentedreality, r/vrdev
- **Discord** : Serveurs de développement VR/AR
- **Forums** : Unity, Unreal (principes applicables)

### Matériel de test

**Budget modeste** :
- Meta Quest 2 (~300€) - Autonome et PC VR
- Smartphone avec ARCore/ARKit

**Professionnel** :
- Valve Index (~1000€)
- HTC Vive Pro
- Varjo XR-3 (haute résolution)

### Logiciels utiles

- **Blender** : Modélisation 3D (gratuit)
- **Unity/Unreal** : Pour comparer et apprendre
- **SteamVR** : Test et développement
- **Oculus Developer Hub** : Pour Quest

## Checklist de développement

### Avant de commencer

- [ ] Définir la plateforme cible (VR PC, VR autonome, AR mobile)
- [ ] Acquérir le matériel de test
- [ ] Installer les SDK nécessaires
- [ ] Créer des bindings Pascal si nécessaire
- [ ] Étudier les guidelines de la plateforme

### Pendant le développement

- [ ] Maintenir 90+ FPS en VR
- [ ] Tester régulièrement sur le matériel réel
- [ ] Implémenter des options de confort
- [ ] Optimiser sans relâche
- [ ] Documenter les contrôles

### Avant la release

- [ ] Test avec différents utilisateurs
- [ ] Vérifier l'accessibilité
- [ ] Options pour réduire le motion sickness
- [ ] Tutorial intégré
- [ ] Performance stable sur matériel minimum

## Conclusion

Le développement VR/AR avec FreePascal et Lazarus est tout à fait possible et peut produire des résultats professionnels. Bien que l'écosystème soit moins mature que pour Unity ou Unreal Engine, les avantages de Pascal - performance, contrôle bas niveau, et compilation native - en font un excellent choix pour la VR/AR.

### Points clés à retenir

✅ **Performance critique** : La VR exige 90+ FPS constants  
✅ **Confort utilisateur** : Éviter le motion sickness est prioritaire  
✅ **Bindings nécessaires** : Créez des interfaces Pascal pour les SDK VR/AR  
✅ **Tests réels** : Toujours tester sur du matériel physique  
✅ **Optimisation** : Utilisez toutes les techniques disponibles  
✅ **Audio spatial** : Crucial pour l'immersion  
✅ **UI adaptée** : Interfaces 3D, pas 2D  
✅ **Multi-plateforme** : Windows et Linux pour la VR PC

### L'avenir est immersif

La VR et l'AR ne sont plus de la science-fiction. Avec FreePascal, vous avez tous les outils pour créer des expériences immersives de qualité professionnelle. Le langage offre la performance nécessaire pour la VR et la flexibilité pour s'adapter aux nouvelles plateformes AR.

**N'oubliez pas** : La meilleure application VR/AR n'est pas celle avec les graphismes les plus impressionnants, mais celle qui offre l'expérience la plus confortable et la plus engageante. Commencez simple, testez souvent, et itérez en fonction des retours.

Que vous créiez un jeu, une simulation d'entraînement, une application éducative ou un outil professionnel, FreePascal et Lazarus vous donnent le contrôle et les performances nécessaires pour réussir dans le monde de la VR et de l'AR.

Bonne chance dans vos aventures immersives ! 🥽🎮

⏭️ [Compilateur et Outils Avancés](/24-compilateur-outils-avances/README.md)
