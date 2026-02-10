🔝 Retour au [Sommaire](/SOMMAIRE.md)

# DirectX et Technologies Multimédia Windows avec FreePascal/Lazarus

## Introduction : Le monde multimédia sous Windows

### Qu'est-ce que DirectX ?

DirectX est un ensemble de technologies Microsoft qui permet aux programmes d'accéder directement au matériel multimédia (carte graphique, carte son, etc.) pour obtenir de meilleures performances. C'est comme avoir une ligne directe avec votre matériel au lieu de passer par plusieurs intermédiaires.

Imaginez DirectX comme un traducteur universel entre votre programme et le matériel :
- Sans DirectX : Programme → Windows → Pilote → Matériel (plus lent)
- Avec DirectX : Programme → DirectX → Matériel (plus rapide)

### Les composants principaux de DirectX

| Composant | Rôle | Utilisation typique |
|-----------|------|-------------------|
| **Direct3D** | Graphiques 3D | Jeux, visualisation 3D, CAO |
| **Direct2D** | Graphiques 2D accélérés | Interfaces modernes, dessin vectoriel |
| **DirectSound** | Audio | Effets sonores, musique |
| **DirectInput** | Entrées | Manettes de jeu, joysticks |
| **DirectShow** | Vidéo et médias | Lecture vidéo, capture webcam |
| **DirectWrite** | Texte avancé | Rendu de texte haute qualité |

### Alternatives disponibles dans FreePascal

Avant de plonger dans DirectX, sachez que FreePascal offre plusieurs options pour le multimédia :

1. **OpenGL** : Multi-plateforme, excellent pour la 3D
2. **SDL** : Simple, multi-plateforme, idéal pour débuter
3. **BGRABitmap** : Graphiques 2D avancés, pur Pascal
4. **DirectX** : Performances maximales sur Windows

## Configuration de l'environnement

### Prérequis système

```pascal
// Vérifier la version de DirectX installée
procedure CheckDirectXVersion;  
var
  Registry: TRegistry;
  Version: string;
begin
  Registry := TRegistry.Create;
  try
    Registry.RootKey := HKEY_LOCAL_MACHINE;
    if Registry.OpenKeyReadOnly('SOFTWARE\Microsoft\DirectX') then
    begin
      Version := Registry.ReadString('Version');
      WriteLn('DirectX version: ', Version);
    end
    else
      WriteLn('DirectX non trouvé');
  finally
    Registry.Free;
  end;
end;
```

### Headers DirectX pour FreePascal

FreePascal ne fournit pas nativement tous les headers DirectX. Voici les options :

1. **JEDI-SDL** : Headers pour DirectX 9 et versions antérieures
2. **Clootie Graphics** : Headers DirectX complets
3. **Headers personnalisés** : Pour des besoins spécifiques

Installation des headers Clootie :
```pascal
// Ajouter au uses de votre projet
uses
  DirectX,      // Types de base DirectX
  Direct3D9,    // Pour Direct3D 9
  D3DX9,        // Utilitaires Direct3D
  DirectSound,  // Pour le son
  DirectShow9;  // Pour la vidéo
```

## Direct2D : Graphiques 2D modernes

### Introduction à Direct2D

Direct2D est la technologie moderne de Windows pour le dessin 2D accéléré par le GPU. C'est plus rapide que GDI+ et offre de meilleurs effets.

```pascal
unit Direct2DUnit;

interface

uses
  Windows, SysUtils, Classes, Graphics, Forms, Controls,
  Direct2D, D2D1;  // Headers Direct2D

type
  TDirect2DForm = class(TForm)
  private
    FD2DFactory: ID2D1Factory;
    FRenderTarget: ID2D1HwndRenderTarget;
    procedure InitDirect2D;
    procedure DrawScene;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
  end;

implementation

constructor TDirect2DForm.Create(AOwner: TComponent);  
begin
  inherited;
  InitDirect2D;
end;

procedure TDirect2DForm.InitDirect2D;  
var
  RenderTargetProps: D2D1_RENDER_TARGET_PROPERTIES;
  HwndRenderTargetProps: D2D1_HWND_RENDER_TARGET_PROPERTIES;
begin
  // Créer la factory Direct2D
  D2D1CreateFactory(
    D2D1_FACTORY_TYPE_SINGLE_THREADED,
    ID2D1Factory,
    nil,
    FD2DFactory
  );

  // Configurer le render target
  RenderTargetProps := D2D1RenderTargetProperties();

  HwndRenderTargetProps := D2D1HwndRenderTargetProperties(
    Handle,  // Handle de la fenêtre
    D2D1SizeU(ClientWidth, ClientHeight)
  );

  // Créer le render target
  FD2DFactory.CreateHwndRenderTarget(
    RenderTargetProps,
    HwndRenderTargetProps,
    FRenderTarget
  );
end;

procedure TDirect2DForm.DrawScene;  
var
  Brush: ID2D1SolidColorBrush;
  Color: D2D1_COLOR_F;
  Rect: D2D1_RECT_F;
begin
  if not Assigned(FRenderTarget) then Exit;

  FRenderTarget.BeginDraw;
  try
    // Effacer l'écran
    Color.r := 0.0; Color.g := 0.0; Color.b := 0.3; Color.a := 1.0;
    FRenderTarget.Clear(@Color);

    // Créer un pinceau rouge
    Color.r := 1.0; Color.g := 0.0; Color.b := 0.0; Color.a := 1.0;
    FRenderTarget.CreateSolidColorBrush(Color, nil, Brush);

    // Dessiner un rectangle
    Rect.left := 50; Rect.top := 50;
    Rect.right := 200; Rect.bottom := 150;
    FRenderTarget.FillRectangle(@Rect, Brush);

  finally
    FRenderTarget.EndDraw(nil, nil);
  end;
end;

procedure TDirect2DForm.Paint;  
begin
  DrawScene;
end;

destructor TDirect2DForm.Destroy;  
begin
  FRenderTarget := nil;
  FD2DFactory := nil;
  inherited;
end;

end.
```

## DirectSound : Audio haute performance

### Lecture de sons avec DirectSound

DirectSound permet de jouer plusieurs sons simultanément avec des effets 3D et un contrôle précis.

```pascal
unit DirectSoundUnit;

interface

uses
  Windows, SysUtils, Classes, MMSystem, DirectSound;

type
  TSoundPlayer = class
  private
    FDirectSound: IDirectSound8;
    FPrimaryBuffer: IDirectSoundBuffer;
    FSoundBuffers: TList;
    function LoadWaveFile(const FileName: string): IDirectSoundBuffer;
  public
    constructor Create(WindowHandle: HWND);
    destructor Destroy; override;
    procedure PlaySound(const WaveFile: string);
    procedure SetVolume(Buffer: IDirectSoundBuffer; Volume: Integer);
  end;

implementation

constructor TSoundPlayer.Create(WindowHandle: HWND);  
var
  BufferDesc: DSBUFFERDESC;
begin
  inherited Create;
  FSoundBuffers := TList.Create;

  // Créer l'objet DirectSound
  DirectSoundCreate8(nil, FDirectSound, nil);

  // Définir le niveau de coopération
  FDirectSound.SetCooperativeLevel(WindowHandle, DSSCL_PRIORITY);

  // Créer le buffer primaire
  FillChar(BufferDesc, SizeOf(BufferDesc), 0);
  BufferDesc.dwSize := SizeOf(BufferDesc);
  BufferDesc.dwFlags := DSBCAPS_PRIMARYBUFFER;

  FDirectSound.CreateSoundBuffer(BufferDesc, FPrimaryBuffer, nil);
end;

function TSoundPlayer.LoadWaveFile(const FileName: string): IDirectSoundBuffer;  
var
  WaveFile: TFileStream;
  WaveFormat: TWaveFormatEx;
  BufferDesc: DSBUFFERDESC;
  Buffer: IDirectSoundBuffer;
  AudioData1, AudioData2: Pointer;
  AudioLength1, AudioLength2: DWORD;
  DataSize: Integer;
begin
  Result := nil;

  // Charger le fichier WAV
  WaveFile := TFileStream.Create(FileName, fmOpenRead);
  try
    // Lire le format WAV (simplifié - suppose un WAV standard)
    WaveFile.Seek(20, soFromBeginning);
    WaveFile.Read(WaveFormat, SizeOf(TWaveFormatEx));

    // Obtenir la taille des données audio
    WaveFile.Seek(40, soFromBeginning);
    WaveFile.Read(DataSize, 4);

    // Créer le buffer secondaire
    FillChar(BufferDesc, SizeOf(BufferDesc), 0);
    BufferDesc.dwSize := SizeOf(BufferDesc);
    BufferDesc.dwFlags := DSBCAPS_CTRLVOLUME or DSBCAPS_STATIC;
    BufferDesc.dwBufferBytes := DataSize;
    BufferDesc.lpwfxFormat := @WaveFormat;

    if FDirectSound.CreateSoundBuffer(BufferDesc, Buffer, nil) = DS_OK then
    begin
      // Verrouiller le buffer pour écrire les données
      if Buffer.Lock(0, DataSize, @AudioData1, @AudioLength1,
                     @AudioData2, @AudioLength2, 0) = DS_OK then
      begin
        try
          // Copier les données audio
          WaveFile.Seek(44, soFromBeginning);
          WaveFile.Read(AudioData1^, AudioLength1);
          if AudioLength2 > 0 then
            WaveFile.Read(AudioData2^, AudioLength2);
        finally
          Buffer.Unlock(AudioData1, AudioLength1, AudioData2, AudioLength2);
        end;

        Result := Buffer;
        FSoundBuffers.Add(Pointer(Buffer));
      end;
    end;
  finally
    WaveFile.Free;
  end;
end;

procedure TSoundPlayer.PlaySound(const WaveFile: string);  
var
  Buffer: IDirectSoundBuffer;
begin
  Buffer := LoadWaveFile(WaveFile);
  if Assigned(Buffer) then
  begin
    Buffer.SetCurrentPosition(0);
    Buffer.Play(0, 0, 0);  // Jouer une fois
    // Pour jouer en boucle : Buffer.Play(0, 0, DSBPLAY_LOOPING);
  end;
end;

procedure TSoundPlayer.SetVolume(Buffer: IDirectSoundBuffer; Volume: Integer);  
begin
  // Volume de -10000 (silence) à 0 (volume max)
  if Assigned(Buffer) then
    Buffer.SetVolume(Volume);
end;

destructor TSoundPlayer.Destroy;  
begin
  FSoundBuffers.Free;
  FPrimaryBuffer := nil;
  FDirectSound := nil;
  inherited;
end;

end.
```

## DirectShow : Lecture et capture vidéo

### Lecteur vidéo simple avec DirectShow

```pascal
unit VideoPlayerUnit;

interface

uses
  Windows, SysUtils, Classes, Controls, Forms, ExtCtrls,
  ActiveX, DirectShow9;

type
  TVideoPlayer = class(TPanel)
  private
    FGraphBuilder: IGraphBuilder;
    FMediaControl: IMediaControl;
    FMediaEvent: IMediaEventEx;
    FVideoWindow: IVideoWindow;
    FMediaSeeking: IMediaSeeking;
    FBasicAudio: IBasicAudio;
    FFileName: string;
    procedure InitializeDirectShow;
    procedure CleanupDirectShow;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function LoadVideo(const FileName: string): Boolean;
    procedure Play;
    procedure Pause;
    procedure Stop;
    procedure SetVolume(Volume: Integer); // 0 à 100
    function GetDuration: Int64;
    function GetPosition: Int64;
    procedure SetPosition(Position: Int64);
  end;

implementation

constructor TVideoPlayer.Create(AOwner: TComponent);  
begin
  inherited;
  CoInitialize(nil);
  InitializeDirectShow;
end;

procedure TVideoPlayer.InitializeDirectShow;  
begin
  // Créer le Filter Graph
  CoCreateInstance(
    CLSID_FilterGraph,
    nil,
    CLSCTX_INPROC_SERVER,
    IID_IGraphBuilder,
    FGraphBuilder
  );

  // Obtenir les interfaces de contrôle
  FGraphBuilder.QueryInterface(IID_IMediaControl, FMediaControl);
  FGraphBuilder.QueryInterface(IID_IMediaEventEx, FMediaEvent);
  FGraphBuilder.QueryInterface(IID_IVideoWindow, FVideoWindow);
  FGraphBuilder.QueryInterface(IID_IMediaSeeking, FMediaSeeking);
  FGraphBuilder.QueryInterface(IID_IBasicAudio, FBasicAudio);
end;

function TVideoPlayer.LoadVideo(const FileName: string): Boolean;  
var
  WideFileName: PWideChar;
begin
  Result := False;

  if not FileExists(FileName) then
    Exit;

  Stop; // Arrêter la vidéo précédente si elle existe

  FFileName := FileName;
  WideFileName := PWideChar(WideString(FileName));

  // Construire automatiquement le graph pour lire le fichier
  if FGraphBuilder.RenderFile(WideFileName, nil) = S_OK then
  begin
    // Configurer la fenêtre vidéo
    if Assigned(FVideoWindow) then
    begin
      FVideoWindow.put_Owner(OAHWND(Handle));
      FVideoWindow.put_WindowStyle(WS_CHILD or WS_CLIPSIBLINGS);
      FVideoWindow.SetWindowPosition(0, 0, Width, Height);
      FVideoWindow.put_MessageDrain(OAHWND(Handle));
    end;

    Result := True;
  end;
end;

procedure TVideoPlayer.Play;  
begin
  if Assigned(FMediaControl) then
    FMediaControl.Run;
end;

procedure TVideoPlayer.Pause;  
begin
  if Assigned(FMediaControl) then
    FMediaControl.Pause;
end;

procedure TVideoPlayer.Stop;  
begin
  if Assigned(FMediaControl) then
  begin
    FMediaControl.Stop;
    // Retourner au début
    if Assigned(FMediaSeeking) then
      SetPosition(0);
  end;
end;

procedure TVideoPlayer.SetVolume(Volume: Integer);  
var
  DBVolume: Integer;
begin
  if not Assigned(FBasicAudio) then Exit;

  // Convertir 0-100 en décibels (-10000 à 0)
  if Volume = 0 then
    DBVolume := -10000
  else
    DBVolume := Round(-10000 + (Volume * 100));

  FBasicAudio.put_Volume(DBVolume);
end;

function TVideoPlayer.GetDuration: Int64;  
begin
  Result := 0;
  if Assigned(FMediaSeeking) then
    FMediaSeeking.GetDuration(Result);
end;

function TVideoPlayer.GetPosition: Int64;  
begin
  Result := 0;
  if Assigned(FMediaSeeking) then
    FMediaSeeking.GetCurrentPosition(Result);
end;

procedure TVideoPlayer.SetPosition(Position: Int64);  
begin
  if Assigned(FMediaSeeking) then
    FMediaSeeking.SetPositions(
      @Position,
      AM_SEEKING_AbsolutePositioning,
      nil,
      AM_SEEKING_NoPositioning
    );
end;

procedure TVideoPlayer.CleanupDirectShow;  
begin
  if Assigned(FVideoWindow) then
  begin
    FVideoWindow.put_Visible(False);
    FVideoWindow.put_Owner(0);
  end;

  if Assigned(FMediaControl) then
    FMediaControl.Stop;

  FBasicAudio := nil;
  FMediaSeeking := nil;
  FVideoWindow := nil;
  FMediaEvent := nil;
  FMediaControl := nil;
  FGraphBuilder := nil;
end;

destructor TVideoPlayer.Destroy;  
begin
  CleanupDirectShow;
  CoUninitialize;
  inherited;
end;

end.
```

## Windows Media Foundation : L'alternative moderne

### Introduction à Media Foundation

Windows Media Foundation (WMF) est le successeur de DirectShow, plus moderne et plus simple à utiliser.

```pascal
unit MediaFoundationUnit;

interface

uses
  Windows, SysUtils, Classes, ComObj, ActiveX;

type
  // Interface simplifiée pour Media Foundation
  IMFMediaEngine = interface
    ['{98A7C030-4AEF-4E40-982C-DC4BCA3BA778}']
    function Play: HResult; stdcall;
    function Pause: HResult; stdcall;
    function SetSource(const URL: PWideChar): HResult; stdcall;
    function GetVolume: Double; stdcall;
    function SetVolume(Volume: Double): HResult; stdcall;
  end;

  TMediaFoundationPlayer = class
  private
    FInitialized: Boolean;
    procedure Initialize;
  public
    constructor Create;
    destructor Destroy; override;
    procedure PlayMediaFile(const FileName: string);
  end;

implementation

const
  MF_VERSION = $00070000;  // Version 7.0

// Déclarations des fonctions Media Foundation
function MFStartup(Version: DWORD; dwFlags: DWORD = 0): HResult;
  stdcall; external 'mfplat.dll';
function MFShutdown: HResult;
  stdcall; external 'mfplat.dll';

constructor TMediaFoundationPlayer.Create;  
begin
  inherited;
  Initialize;
end;

procedure TMediaFoundationPlayer.Initialize;  
var
  hr: HResult;
begin
  hr := MFStartup(MF_VERSION);
  FInitialized := (hr = S_OK);

  if not FInitialized then
    raise Exception.Create('Impossible d''initialiser Media Foundation');
end;

procedure TMediaFoundationPlayer.PlayMediaFile(const FileName: string);  
begin
  if not FInitialized then
    Exit;

  // Code simplifié - nécessite l'implémentation complète
  // des interfaces Media Foundation
  WriteLn('Lecture de: ', FileName);
end;

destructor TMediaFoundationPlayer.Destroy;  
begin
  if FInitialized then
    MFShutdown;
  inherited;
end;

end.
```

## XAudio2 : Audio moderne pour les jeux

### Utilisation basique de XAudio2

XAudio2 est le système audio moderne pour les jeux, successeur de DirectSound.

```pascal
unit XAudio2Unit;

interface

uses
  Windows, SysUtils, Classes, ComObj, ActiveX;

type
  // Interface XAudio2 simplifiée
  IXAudio2 = interface(IUnknown)
    ['{8BCFG3B8-432F-40BD-BA06-C0B6CFC9DE45}']
    function CreateMasteringVoice(out ppMasteringVoice: Pointer;
      InputChannels: UINT32 = 2;
      InputSampleRate: UINT32 = 44100;
      Flags: UINT32 = 0;
      DeviceIndex: UINT32 = 0;
      pEffectChain: Pointer = nil): HResult; stdcall;
    function StartEngine: HResult; stdcall;
    procedure StopEngine; stdcall;
  end;

  TXAudio2Player = class
  private
    FXAudio2: IXAudio2;
    FMasterVoice: Pointer;
    FInitialized: Boolean;
    procedure Initialize;
  public
    constructor Create;
    destructor Destroy; override;
    procedure PlayWaveData(Data: Pointer; Size: Integer);
  end;

implementation

const
  CLSID_XAudio2: TGUID = '{5A9CE6A2-5BA4-4C68-A85B-6E3B0F4E5A6F}';
  IID_IXAudio2: TGUID = '{8BCFG3B8-432F-40BD-BA06-C0B6CFC9DE45}';

constructor TXAudio2Player.Create;  
begin
  inherited;
  CoInitialize(nil);
  Initialize;
end;

procedure TXAudio2Player.Initialize;  
var
  hr: HResult;
begin
  // Créer l'instance XAudio2
  hr := CoCreateInstance(CLSID_XAudio2, nil, CLSCTX_INPROC_SERVER,
                         IID_IXAudio2, FXAudio2);

  if hr = S_OK then
  begin
    // Créer la voix de mastering (sortie audio principale)
    hr := FXAudio2.CreateMasteringVoice(FMasterVoice);

    if hr = S_OK then
    begin
      FXAudio2.StartEngine;
      FInitialized := True;
    end;
  end;

  if not FInitialized then
    raise Exception.Create('Impossible d''initialiser XAudio2');
end;

procedure TXAudio2Player.PlayWaveData(Data: Pointer; Size: Integer);  
begin
  if not FInitialized then
    Exit;

  // Implémentation simplifiée
  // Nécessite la création d'une SourceVoice et la soumission du buffer
  WriteLn('Lecture audio - Taille: ', Size, ' octets');
end;

destructor TXAudio2Player.Destroy;  
begin
  if FInitialized then
  begin
    FXAudio2.StopEngine;
    FXAudio2 := nil;
  end;
  CoUninitialize;
  inherited;
end;

end.
```

## Capture vidéo avec la webcam

### Accès simple à la webcam

```pascal
unit WebcamUnit;

interface

uses
  Windows, SysUtils, Classes, Graphics, ExtCtrls,
  ActiveX, DirectShow9;

type
  TWebcamCapture = class
  private
    FGraphBuilder: IGraphBuilder;
    FCaptureGraphBuilder: ICaptureGraphBuilder2;
    FMediaControl: IMediaControl;
    FVideoWindow: IVideoWindow;
    FMoniker: IMoniker;
    FPreviewPanel: TPanel;
    function GetFirstVideoDevice: IMoniker;
    function InitializeCapture: Boolean;
  public
    constructor Create(PreviewPanel: TPanel);
    destructor Destroy; override;
    procedure StartPreview;
    procedure StopPreview;
    procedure TakeSnapshot(Bitmap: TBitmap);
  end;

implementation

constructor TWebcamCapture.Create(PreviewPanel: TPanel);  
begin
  inherited Create;
  FPreviewPanel := PreviewPanel;
  CoInitialize(nil);
  InitializeCapture;
end;

function TWebcamCapture.GetFirstVideoDevice: IMoniker;  
var
  DevEnum: ICreateDevEnum;
  EnumMoniker: IEnumMoniker;
  Fetched: ULONG;
begin
  Result := nil;

  // Créer l'énumérateur de périphériques
  CoCreateInstance(CLSID_SystemDeviceEnum, nil, CLSCTX_INPROC_SERVER,
                   IID_ICreateDevEnum, DevEnum);

  // Énumérer les périphériques vidéo
  if DevEnum.CreateClassEnumerator(CLSID_VideoInputDeviceCategory,
                                    EnumMoniker, 0) = S_OK then
  begin
    // Obtenir le premier périphérique
    EnumMoniker.Next(1, Result, @Fetched);
  end;
end;

function TWebcamCapture.InitializeCapture: Boolean;  
var
  SourceFilter: IBaseFilter;
  hr: HResult;
begin
  Result := False;

  // Créer le Filter Graph
  CoCreateInstance(CLSID_FilterGraph, nil, CLSCTX_INPROC_SERVER,
                   IID_IGraphBuilder, FGraphBuilder);

  // Créer le Capture Graph Builder
  CoCreateInstance(CLSID_CaptureGraphBuilder2, nil, CLSCTX_INPROC_SERVER,
                   IID_ICaptureGraphBuilder2, FCaptureGraphBuilder);

  FCaptureGraphBuilder.SetFiltergraph(FGraphBuilder);

  // Obtenir le premier périphérique vidéo
  FMoniker := GetFirstVideoDevice;
  if FMoniker = nil then
  begin
    ShowMessage('Aucune webcam trouvée');
    Exit;
  end;

  // Créer le filtre source
  FMoniker.BindToObject(nil, nil, IID_IBaseFilter, SourceFilter);

  // Ajouter le filtre au graph
  FGraphBuilder.AddFilter(SourceFilter, 'Video Capture');

  // Configurer l'aperçu
  FCaptureGraphBuilder.RenderStream(
    @PIN_CATEGORY_PREVIEW,
    @MEDIATYPE_Video,
    SourceFilter,
    nil,
    nil
  );

  // Obtenir les interfaces de contrôle
  FGraphBuilder.QueryInterface(IID_IMediaControl, FMediaControl);
  FGraphBuilder.QueryInterface(IID_IVideoWindow, FVideoWindow);

  // Configurer la fenêtre de prévisualisation
  if Assigned(FVideoWindow) then
  begin
    FVideoWindow.put_Owner(OAHWND(FPreviewPanel.Handle));
    FVideoWindow.put_WindowStyle(WS_CHILD or WS_CLIPSIBLINGS);
    FVideoWindow.SetWindowPosition(0, 0,
                                   FPreviewPanel.Width,
                                   FPreviewPanel.Height);
  end;

  Result := True;
end;

procedure TWebcamCapture.StartPreview;  
begin
  if Assigned(FMediaControl) then
    FMediaControl.Run;
end;

procedure TWebcamCapture.StopPreview;  
begin
  if Assigned(FMediaControl) then
    FMediaControl.Stop;
end;

procedure TWebcamCapture.TakeSnapshot(Bitmap: TBitmap);  
begin
  // Implémentation simplifiée
  // Nécessite l'ajout d'un Sample Grabber au graph
  WriteLn('Capture d''image...');
end;

destructor TWebcamCapture.Destroy;  
begin
  StopPreview;

  if Assigned(FVideoWindow) then
  begin
    FVideoWindow.put_Visible(False);
    FVideoWindow.put_Owner(0);
  end;

  FVideoWindow := nil;
  FMediaControl := nil;
  FCaptureGraphBuilder := nil;
  FGraphBuilder := nil;
  FMoniker := nil;

  CoUninitialize;
  inherited;
end;

end.
```

## Optimisation et bonnes pratiques

### 1. Gestion des ressources

```pascal
// TOUJOURS libérer les ressources DirectX
procedure SafeReleaseDirectX;  
begin
  try
    // Arrêter les lectures en cours
    if Assigned(MediaControl) then
      MediaControl.Stop;

    // Libérer dans l'ordre inverse de création
    VideoWindow := nil;
    MediaControl := nil;
    GraphBuilder := nil;

  finally
    // S'assurer que COM est libéré
    CoUninitialize;
  end;
end;
```

### 2. Gestion des erreurs DirectX

```pascal
function DirectXErrorToString(ErrorCode: HResult): string;  
begin
  case ErrorCode of
    E_FAIL: Result := 'Échec général';
    E_INVALIDARG: Result := 'Argument invalide';
    E_OUTOFMEMORY: Result := 'Mémoire insuffisante';
    E_NOTIMPL: Result := 'Non implémenté';
    E_POINTER: Result := 'Pointeur invalide';
    VFW_E_NOT_FOUND: Result := 'Codec non trouvé';
    VFW_E_CANNOT_RENDER: Result := 'Impossible de lire le média';
    VFW_E_INVALID_FILE_FORMAT: Result := 'Format de fichier invalide';
  else
    Result := Format('Erreur DirectX: 0x%X', [ErrorCode]);
  end;
end;

// Utilisation
procedure SafeDirectXCall(hr: HResult; const Operation: string);  
begin
  if Failed(hr) then
    raise Exception.CreateFmt('%s a échoué: %s',
                              [Operation, DirectXErrorToString(hr)]);
end;
```

### 3. Détection des capacités matérielles

```pascal
procedure CheckHardwareCapabilities;  
var
  D3D: IDirect3D9;
  D3DCaps: D3DCAPS9;
  AdapterCount: Integer;
  Mode: D3DDISPLAYMODE;
  AdapterInfo: D3DADAPTER_IDENTIFIER9;
  i: Integer;
begin
  // Créer l'objet Direct3D
  D3D := Direct3DCreate9(D3D_SDK_VERSION);
  if D3D = nil then
  begin
    WriteLn('Direct3D 9 non disponible');
    Exit;
  end;

  try
    // Nombre d'adaptateurs (cartes graphiques)
    AdapterCount := D3D.GetAdapterCount;
    WriteLn('Nombre de cartes graphiques: ', AdapterCount);

    // Parcourir tous les adaptateurs
    for i := 0 to AdapterCount - 1 do
    begin
      WriteLn('--- Carte graphique #', i + 1, ' ---');

      // Obtenir les informations de l'adaptateur
      D3D.GetAdapterIdentifier(i, 0, AdapterInfo);
      WriteLn('Nom: ', AdapterInfo.Description);
      WriteLn('Pilote: ', AdapterInfo.Driver);
      WriteLn('Vendeur ID: ', IntToHex(AdapterInfo.VendorId, 4));

      // Capacités de l'adaptateur
      if D3D.GetDeviceCaps(i, D3DDEVTYPE_HAL, D3DCaps) = D3D_OK then
      begin
        WriteLn('Vertex Shader Version: ',
                Hi(D3DCaps.VertexShaderVersion), '.',
                Lo(D3DCaps.VertexShaderVersion));
        WriteLn('Pixel Shader Version: ',
                Hi(D3DCaps.PixelShaderVersion), '.',
                Lo(D3DCaps.PixelShaderVersion));
        WriteLn('Texture max: ', D3DCaps.MaxTextureWidth, 'x',
                D3DCaps.MaxTextureHeight);
      end;

      // Mode d'affichage actuel
      if D3D.GetAdapterDisplayMode(i, Mode) = D3D_OK then
      begin
        WriteLn('Résolution: ', Mode.Width, 'x', Mode.Height);
        WriteLn('Taux de rafraîchissement: ', Mode.RefreshRate, ' Hz');
        WriteLn('Format: ', GetFormatName(Mode.Format));
      end;

      WriteLn('');
    end;
  finally
    D3D := nil;
  end;
end;

function GetFormatName(Format: TD3DFormat): string;  
begin
  case Format of
    D3DFMT_R8G8B8: Result := 'RGB 24-bit';
    D3DFMT_A8R8G8B8: Result := 'ARGB 32-bit';
    D3DFMT_X8R8G8B8: Result := 'XRGB 32-bit';
    D3DFMT_R5G6B5: Result := 'RGB 16-bit (565)';
    D3DFMT_X1R5G5B5: Result := 'XRGB 16-bit (1555)';
    D3DFMT_A1R5G5B5: Result := 'ARGB 16-bit (1555)';
    D3DFMT_A2R10G10B10: Result := 'ARGB 32-bit (2-10-10-10)';
  else
    Result := 'Format inconnu';
  end;
end;
```

### 4. Performance et optimisation mémoire

```pascal
type
  TResourceManager = class
  private
    FTextureCache: TStringList;
    FSoundCache: TStringList;
    FMaxCacheSize: Int64;
    FCurrentCacheSize: Int64;
    procedure CleanupCache;
  public
    constructor Create(MaxCacheMB: Integer);
    destructor Destroy; override;
    function LoadTexture(const FileName: string): IDirect3DTexture9;
    function LoadSound(const FileName: string): IDirectSoundBuffer;
    procedure PreloadResources(const FileList: TStringList);
  end;

constructor TResourceManager.Create(MaxCacheMB: Integer);  
begin
  inherited Create;
  FTextureCache := TStringList.Create;
  FSoundCache := TStringList.Create;
  FMaxCacheSize := MaxCacheMB * 1024 * 1024;
  FCurrentCacheSize := 0;
end;

procedure TResourceManager.CleanupCache;  
var
  i: Integer;
  OldestTime: TDateTime;
  OldestIndex: Integer;
begin
  // Nettoyer le cache si on dépasse la limite
  while FCurrentCacheSize > FMaxCacheSize do
  begin
    // Trouver et supprimer la ressource la plus ancienne
    OldestTime := Now;
    OldestIndex := -1;

    for i := 0 to FTextureCache.Count - 1 do
    begin
      // Logique pour trouver la plus ancienne texture non utilisée
      // Implémentation simplifiée
      if i = 0 then
        OldestIndex := i;
    end;

    if OldestIndex >= 0 then
    begin
      // Libérer la texture
      FTextureCache.Delete(OldestIndex);
      Dec(FCurrentCacheSize, 1024 * 1024); // Estimation
    end
    else
      Break;
  end;
end;

function TResourceManager.LoadTexture(const FileName: string): IDirect3DTexture9;  
var
  Index: Integer;
begin
  // Vérifier si déjà en cache
  Index := FTextureCache.IndexOf(FileName);
  if Index >= 0 then
  begin
    Result := IDirect3DTexture9(FTextureCache.Objects[Index]);
    Exit;
  end;

  // Charger la nouvelle texture
  // Code de chargement ici...

  // Ajouter au cache
  FTextureCache.AddObject(FileName, TObject(Result));
  Inc(FCurrentCacheSize, GetTextureSize(Result));

  // Nettoyer si nécessaire
  CleanupCache;
end;

procedure TResourceManager.PreloadResources(const FileList: TStringList);  
var
  i: Integer;
begin
  WriteLn('Préchargement de ', FileList.Count, ' ressources...');

  for i := 0 to FileList.Count - 1 do
  begin
    if ExtractFileExt(FileList[i]) = '.png' then
      LoadTexture(FileList[i])
    else if ExtractFileExt(FileList[i]) = '.wav' then
      LoadSound(FileList[i]);

    // Afficher la progression
    if (i mod 10) = 0 then
      WriteLn('  ', Round((i / FileList.Count) * 100), '% complété');
  end;

  WriteLn('Préchargement terminé. Cache: ',
          FCurrentCacheSize div (1024 * 1024), ' MB');
end;
```

## Application complète : Lecteur multimédia

### Architecture du lecteur

Voici un exemple complet d'un lecteur multimédia utilisant DirectShow avec une interface Lazarus.

```pascal
unit MediaPlayerMainForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, Menus, Buttons,
  ActiveX, DirectShow9;

type
  TMediaPlayer = class(TForm)
    // Composants visuels
    VideoPanel: TPanel;
    ControlPanel: TPanel;
    PlayButton: TSpeedButton;
    PauseButton: TSpeedButton;
    StopButton: TSpeedButton;
    OpenButton: TSpeedButton;
    VolumeTrackBar: TTrackBar;
    PositionTrackBar: TTrackBar;
    TimeLabel: TLabel;
    StatusBar: TStatusBar;
    OpenDialog: TOpenDialog;
    Timer1: TTimer;

    // Menu
    MainMenu: TMainMenu;
    FileMenu: TMenuItem;
    OpenMenuItem: TMenuItem;
    ExitMenuItem: TMenuItem;
    ViewMenu: TMenuItem;
    FullScreenMenuItem: TMenuItem;

    // Événements
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure PlayButtonClick(Sender: TObject);
    procedure PauseButtonClick(Sender: TObject);
    procedure StopButtonClick(Sender: TObject);
    procedure OpenButtonClick(Sender: TObject);
    procedure VolumeTrackBarChange(Sender: TObject);
    procedure PositionTrackBarChange(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FullScreenMenuItemClick(Sender: TObject);

  private
    // Interfaces DirectShow
    FGraphBuilder: IGraphBuilder;
    FMediaControl: IMediaControl;
    FMediaEvent: IMediaEventEx;
    FMediaSeeking: IMediaSeeking;
    FBasicAudio: IBasicAudio;
    FVideoWindow: IVideoWindow;
    FMediaFilter: IMediaFilter;

    // État
    FFileName: string;
    FIsPlaying: Boolean;
    FIsPaused: Boolean;
    FIsFullScreen: Boolean;
    FDuration: Int64;
    FUserChangingPosition: Boolean;

    // Méthodes privées
    procedure InitializeDirectShow;
    procedure CleanupDirectShow;
    function LoadMediaFile(const FileName: string): Boolean;
    procedure UpdateControls;
    procedure UpdateTimeDisplay;
    function FormatTime(NanoSeconds: Int64): string;
    procedure SetFullScreen(Value: Boolean);
    procedure ShowError(const Msg: string);
  public
    property IsPlaying: Boolean read FIsPlaying;
    property IsPaused: Boolean read FIsPaused;
  end;

var
  MediaPlayer: TMediaPlayer;

implementation

{$R *.dfm}

procedure TMediaPlayer.FormCreate(Sender: TObject);  
begin
  // Initialiser COM
  CoInitialize(nil);

  // Initialiser DirectShow
  InitializeDirectShow;

  // Configuration initiale
  FIsPlaying := False;
  FIsPaused := False;
  FIsFullScreen := False;
  FUserChangingPosition := False;

  // Configurer l'interface
  VideoPanel.Color := clBlack;
  VideoPanel.Caption := 'Aucun média chargé';

  // Configurer le dialogue d'ouverture
  OpenDialog.Filter := 'Tous les médias|*.avi;*.mp4;*.mkv;*.wmv;*.mp3;*.wav;*.wma|' +
                      'Vidéos|*.avi;*.mp4;*.mkv;*.wmv;*.mpg;*.mpeg|' +
                      'Audio|*.mp3;*.wav;*.wma;*.ogg;*.flac|' +
                      'Tous les fichiers|*.*';

  // Volume par défaut
  VolumeTrackBar.Position := 75;

  UpdateControls;
end;

procedure TMediaPlayer.InitializeDirectShow;  
begin
  try
    // Créer le Filter Graph Manager
    CoCreateInstance(CLSID_FilterGraph, nil, CLSCTX_INPROC_SERVER,
                     IID_IGraphBuilder, FGraphBuilder);

    // Obtenir les interfaces de contrôle
    FGraphBuilder.QueryInterface(IID_IMediaControl, FMediaControl);
    FGraphBuilder.QueryInterface(IID_IMediaEventEx, FMediaEvent);
    FGraphBuilder.QueryInterface(IID_IMediaSeeking, FMediaSeeking);
    FGraphBuilder.QueryInterface(IID_IBasicAudio, FBasicAudio);
    FGraphBuilder.QueryInterface(IID_IVideoWindow, FVideoWindow);
    FGraphBuilder.QueryInterface(IID_IMediaFilter, FMediaFilter);

    // Configurer les formats de temps (100-nanosecond units)
    if Assigned(FMediaSeeking) then
      FMediaSeeking.SetTimeFormat(TIME_FORMAT_MEDIA_TIME);

  except
    on E: Exception do
      ShowError('Erreur d''initialisation DirectShow: ' + E.Message);
  end;
end;

function TMediaPlayer.LoadMediaFile(const FileName: string): Boolean;  
var
  hr: HResult;
  WideFileName: PWideChar;
begin
  Result := False;

  if not FileExists(FileName) then
  begin
    ShowError('Fichier non trouvé: ' + FileName);
    Exit;
  end;

  try
    // Arrêter le média actuel
    if Assigned(FMediaControl) then
      FMediaControl.Stop;

    // Nettoyer le graph existant
    if Assigned(FVideoWindow) then
    begin
      FVideoWindow.put_Visible(False);
      FVideoWindow.put_Owner(0);
    end;

    // Recréer le graph
    CleanupDirectShow;
    InitializeDirectShow;

    // Charger le nouveau fichier
    WideFileName := PWideChar(WideString(FileName));
    hr := FGraphBuilder.RenderFile(WideFileName, nil);

    if Failed(hr) then
    begin
      ShowError('Impossible de charger le fichier. ' +
                'Le codec nécessaire est peut-être manquant.');
      Exit;
    end;

    // Configurer la fenêtre vidéo si c''est une vidéo
    if Assigned(FVideoWindow) then
    begin
      FVideoWindow.put_Owner(OAHWND(VideoPanel.Handle));
      FVideoWindow.put_WindowStyle(WS_CHILD or WS_CLIPSIBLINGS);
      FVideoWindow.put_MessageDrain(OAHWND(Handle));
      FVideoWindow.SetWindowPosition(0, 0, VideoPanel.Width, VideoPanel.Height);
      FVideoWindow.put_Visible(True);

      VideoPanel.Caption := '';
    end
    else
    begin
      // C'est un fichier audio uniquement
      VideoPanel.Caption := ExtractFileName(FileName);
    end;

    // Obtenir la durée
    if Assigned(FMediaSeeking) then
    begin
      FMediaSeeking.GetDuration(FDuration);
      PositionTrackBar.Max := 1000; // Utiliser une échelle de 0-1000
    end;

    FFileName := FileName;
    StatusBar.SimpleText := 'Chargé: ' + ExtractFileName(FileName);

    Result := True;
    UpdateControls;

  except
    on E: Exception do
    begin
      ShowError('Erreur de chargement: ' + E.Message);
      Result := False;
    end;
  end;
end;

procedure TMediaPlayer.PlayButtonClick(Sender: TObject);  
begin
  if not Assigned(FMediaControl) then Exit;

  if FFileName = '' then
  begin
    OpenButtonClick(nil);
    Exit;
  end;

  try
    FMediaControl.Run;
    FIsPlaying := True;
    FIsPaused := False;
    Timer1.Enabled := True;
    UpdateControls;
    StatusBar.SimpleText := 'Lecture: ' + ExtractFileName(FFileName);
  except
    on E: Exception do
      ShowError('Erreur de lecture: ' + E.Message);
  end;
end;

procedure TMediaPlayer.PauseButtonClick(Sender: TObject);  
begin
  if not Assigned(FMediaControl) then Exit;

  try
    FMediaControl.Pause;
    FIsPlaying := False;
    FIsPaused := True;
    Timer1.Enabled := False;
    UpdateControls;
    StatusBar.SimpleText := 'En pause';
  except
    on E: Exception do
      ShowError('Erreur: ' + E.Message);
  end;
end;

procedure TMediaPlayer.StopButtonClick(Sender: TObject);  
var
  Position: Int64;
begin
  if not Assigned(FMediaControl) then Exit;

  try
    FMediaControl.Stop;
    FIsPlaying := False;
    FIsPaused := False;
    Timer1.Enabled := False;

    // Retourner au début
    if Assigned(FMediaSeeking) then
    begin
      Position := 0;
      FMediaSeeking.SetPositions(@Position, AM_SEEKING_AbsolutePositioning,
                                 nil, AM_SEEKING_NoPositioning);
    end;

    PositionTrackBar.Position := 0;
    UpdateControls;
    UpdateTimeDisplay;
    StatusBar.SimpleText := 'Arrêté';
  except
    on E: Exception do
      ShowError('Erreur: ' + E.Message);
  end;
end;

procedure TMediaPlayer.OpenButtonClick(Sender: TObject);  
begin
  if OpenDialog.Execute then
  begin
    if LoadMediaFile(OpenDialog.FileName) then
    begin
      // Démarrer automatiquement la lecture
      PlayButtonClick(nil);
    end;
  end;
end;

procedure TMediaPlayer.VolumeTrackBarChange(Sender: TObject);  
var
  Volume: Integer;
begin
  if not Assigned(FBasicAudio) then Exit;

  // Convertir 0-100 en décibels (-10000 à 0)
  if VolumeTrackBar.Position = 0 then
    Volume := -10000
  else
    Volume := Round((VolumeTrackBar.Position - 100) * 100);

  try
    FBasicAudio.put_Volume(Volume);
  except
    // Ignorer les erreurs (fichier audio uniquement)
  end;
end;

procedure TMediaPlayer.PositionTrackBarChange(Sender: TObject);  
var
  NewPosition: Int64;
begin
  if not Assigned(FMediaSeeking) or FUserChangingPosition then Exit;

  // Calculer la nouvelle position
  NewPosition := Round((PositionTrackBar.Position / 1000) * FDuration);

  try
    FMediaSeeking.SetPositions(@NewPosition, AM_SEEKING_AbsolutePositioning,
                               nil, AM_SEEKING_NoPositioning);
    UpdateTimeDisplay;
  except
    // Ignorer les erreurs
  end;
end;

procedure TMediaPlayer.Timer1Timer(Sender: TObject);  
var
  CurrentPosition: Int64;
  EventCode: Integer;
  Param1, Param2: Integer;
begin
  if not Assigned(FMediaSeeking) then Exit;

  // Mettre à jour la position
  if FMediaSeeking.GetCurrentPosition(CurrentPosition) = S_OK then
  begin
    FUserChangingPosition := True;
    try
      if FDuration > 0 then
        PositionTrackBar.Position := Round((CurrentPosition / FDuration) * 1000);
    finally
      FUserChangingPosition := False;
    end;

    UpdateTimeDisplay;
  end;

  // Vérifier les événements (fin de lecture, etc.)
  if Assigned(FMediaEvent) then
  begin
    while FMediaEvent.GetEvent(EventCode, Param1, Param2, 0) = S_OK do
    begin
      case EventCode of
        EC_COMPLETE:
        begin
          StopButtonClick(nil);
          StatusBar.SimpleText := 'Lecture terminée';
        end;
      end;
      FMediaEvent.FreeEventParams(EventCode, Param1, Param2);
    end;
  end;
end;

procedure TMediaPlayer.UpdateTimeDisplay;  
var
  CurrentPosition: Int64;
begin
  if Assigned(FMediaSeeking) then
  begin
    FMediaSeeking.GetCurrentPosition(CurrentPosition);
    TimeLabel.Caption := FormatTime(CurrentPosition) + ' / ' + FormatTime(FDuration);
  end
  else
    TimeLabel.Caption := '00:00 / 00:00';
end;

function TMediaPlayer.FormatTime(NanoSeconds: Int64): string;  
var
  Seconds: Integer;
  Minutes: Integer;
  Hours: Integer;
begin
  // Convertir de 100-nanosecond units en secondes
  Seconds := NanoSeconds div 10000000;

  Hours := Seconds div 3600;
  Minutes := (Seconds mod 3600) div 60;
  Seconds := Seconds mod 60;

  if Hours > 0 then
    Result := Format('%d:%2.2d:%2.2d', [Hours, Minutes, Seconds])
  else
    Result := Format('%2.2d:%2.2d', [Minutes, Seconds]);
end;

procedure TMediaPlayer.UpdateControls;  
begin
  PlayButton.Enabled := not FIsPlaying and (FFileName <> '');
  PauseButton.Enabled := FIsPlaying;
  StopButton.Enabled := FIsPlaying or FIsPaused;
  PositionTrackBar.Enabled := FFileName <> '';
end;

procedure TMediaPlayer.FormResize(Sender: TObject);  
begin
  // Ajuster la fenêtre vidéo
  if Assigned(FVideoWindow) then
  begin
    FVideoWindow.SetWindowPosition(0, 0, VideoPanel.Width, VideoPanel.Height);
  end;
end;

procedure TMediaPlayer.SetFullScreen(Value: Boolean);  
begin
  if FIsFullScreen = Value then Exit;

  FIsFullScreen := Value;

  if FIsFullScreen then
  begin
    // Passer en plein écran
    BorderStyle := bsNone;
    WindowState := wsMaximized;
    ControlPanel.Visible := False;
    StatusBar.Visible := False;
    MainMenu.Items.Visible := False;
  end
  else
  begin
    // Revenir en mode fenêtré
    BorderStyle := bsSizeable;
    WindowState := wsNormal;
    ControlPanel.Visible := True;
    StatusBar.Visible := True;
    MainMenu.Items.Visible := True;
  end;

  FormResize(nil);
end;

procedure TMediaPlayer.FullScreenMenuItemClick(Sender: TObject);  
begin
  SetFullScreen(not FIsFullScreen);
end;

procedure TMediaPlayer.ShowError(const Msg: string);  
begin
  MessageDlg(Msg, mtError, [mbOK], 0);
  StatusBar.SimpleText := 'Erreur: ' + Msg;
end;

procedure TMediaPlayer.CleanupDirectShow;  
begin
  try
    Timer1.Enabled := False;

    if Assigned(FVideoWindow) then
    begin
      FVideoWindow.put_Visible(False);
      FVideoWindow.put_Owner(0);
    end;

    if Assigned(FMediaControl) then
      FMediaControl.Stop;

    // Libérer les interfaces dans l'ordre inverse
    FMediaFilter := nil;
    FVideoWindow := nil;
    FBasicAudio := nil;
    FMediaSeeking := nil;
    FMediaEvent := nil;
    FMediaControl := nil;
    FGraphBuilder := nil;
  except
    // Ignorer les erreurs lors du nettoyage
  end;
end;

procedure TMediaPlayer.FormDestroy(Sender: TObject);  
begin
  CleanupDirectShow;
  CoUninitialize;
end;

end.
```

## Dépannage et problèmes courants

### Problèmes fréquents et solutions

| Problème | Cause possible | Solution |
|----------|---------------|----------|
| **"Codec manquant"** | Le codec pour le format n'est pas installé | Installer K-Lite Codec Pack ou LAV Filters |
| **Pas de son** | Mauvaise configuration audio | Vérifier FBasicAudio et les paramètres Windows |
| **Vidéo saccadée** | Performances insuffisantes | Réduire la résolution ou utiliser l'accélération matérielle |
| **Écran noir** | Problème de rendu | Vérifier FVideoWindow et les pilotes graphiques |
| **Erreur COM** | COM non initialisé | Toujours appeler CoInitialize au début |

### Vérification de l'installation

```pascal
procedure CheckMultimediaSupport;  
var
  hr: HResult;
  D3D: IDirect3D9;
  DS: IDirectSound8;
begin
  WriteLn('=== Vérification du support multimédia ===');

  // Vérifier Direct3D
  D3D := Direct3DCreate9(D3D_SDK_VERSION);
  if Assigned(D3D) then
  begin
    WriteLn('[OK] Direct3D 9 disponible');
    D3D := nil;
  end
  else
    WriteLn('[ERREUR] Direct3D 9 non disponible');

  // Vérifier DirectSound
  hr := DirectSoundCreate8(nil, DS, nil);
  if Succeeded(hr) then
  begin
    WriteLn('[OK] DirectSound 8 disponible');
    DS := nil;
  end
  else
    WriteLn('[ERREUR] DirectSound 8 non disponible');

  // Vérifier DirectShow
  try
    CoInitialize(nil);
    try
      hr := CoCreateInstance(CLSID_FilterGraph, nil, CLSCTX_INPROC_SERVER,
                            IID_IGraphBuilder, FGraphBuilder);
      if Succeeded(hr) then
      begin
        WriteLn('[OK] DirectShow disponible');
        FGraphBuilder := nil;
      end
      else
        WriteLn('[ERREUR] DirectShow non disponible');
    finally
      CoUninitialize;
    end;
  except
    WriteLn('[ERREUR] COM/DirectShow erreur');
  end;
end;
```

## Ressources et documentation

### Outils de développement

1. **GraphEdit** (ou GraphStudioNext)
   - Visualiser les graphs DirectShow
   - Tester les filtres et codecs
   - Déboguer les problèmes de lecture

2. **DirectX SDK**
   - Documentation complète
   - Exemples de code
   - Outils de diagnostic

3. **PIX pour Windows**
   - Profiler les performances DirectX
   - Analyser les appels GPU

### Headers et bibliothèques pour FreePascal

| Bibliothèque | Description | URL/Package |
|--------------|-------------|-------------|
| **JEDI-SDL** | Headers DirectX et SDL | sourceforge.net/projects/jedi-sdl |
| **Clootie Graphics** | Headers DirectX complets | clootie.ru |
| **PasDirectX** | Alternative moderne | github.com/[rechercher] |

### Exemples de projets

Pour approfondir vos connaissances, étudiez ces types de projets :

1. **Lecteur audio simple** : DirectSound uniquement
2. **Visionneuse d'images** : Direct2D pour les effets
3. **Lecteur vidéo** : DirectShow complet
4. **Enregistreur webcam** : Capture DirectShow
5. **Éditeur vidéo basique** : Filtres et effets DirectShow

## Conclusion

DirectX et les technologies multimédia Windows offrent des possibilités immenses pour vos applications FreePascal/Lazarus. Les points clés à retenir :

### Ce que vous avez appris

- **Bases de DirectX** : Comprendre l'architecture et les composants
- **DirectShow** : Créer des lecteurs multimédia complets
- **DirectSound** : Gérer l'audio avec précision
- **Direct2D** : Graphiques 2D accélérés
- **Capture vidéo** : Utiliser les webcams et périphériques

### Conseils pour progresser

1. **Commencez simple** : Un lecteur WAV avant un lecteur vidéo complet
2. **Gérez les erreurs** : DirectX peut échouer, prévoyez des alternatives
3. **Testez sur différents systèmes** : Les codecs varient selon les machines
4. **Documentez-vous** : La documentation Microsoft est excellente
5. **Réutilisez le code** : Créez vos propres bibliothèques réutilisables

### Prochaines étapes

- Explorer **Windows Media Foundation** pour les applications modernes
- Apprendre **OpenGL** pour le multi-plateforme
- Étudier **FFmpeg** pour un support de formats universel
- Maîtriser la **programmation asynchrone** pour les médias

Avec ces connaissances, vous êtes maintenant capable de créer des applications multimédia professionnelles sous Windows avec FreePascal/Lazarus !

⏭️ [Spécificités Linux/Ubuntu](/07-specificites-linux-ubuntu/README.md)
