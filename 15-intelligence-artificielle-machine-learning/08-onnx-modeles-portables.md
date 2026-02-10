🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 15.8 ONNX et modèles portables

## Introduction à ONNX

ONNX (Open Neural Network Exchange) est un **format ouvert** pour représenter des modèles d'apprentissage automatique. C'est un peu comme un "PDF pour l'intelligence artificielle" : un format universel que tout le monde peut lire.

### Qu'est-ce qu'ONNX ?

ONNX permet de :
- **Entraîner** un modèle dans un framework (PyTorch, TensorFlow, scikit-learn)
- **Exporter** le modèle au format ONNX
- **Utiliser** ce modèle dans n'importe quel langage/plateforme
- **Déployer** sur mobile, serveur, embarqué, desktop

### Pourquoi ONNX est important ?

**Sans ONNX :**
```
Entraînement PyTorch → Bloqué dans PyTorch
                      → Nécessite Python en production
                      → Dépendances lourdes
```

**Avec ONNX :**
```
Entraînement PyTorch → Export ONNX → Utilisation FreePascal
                                   → Utilisation C++
                                   → Utilisation JavaScript
                                   → Utilisation mobile
```

### Applications concrètes

🎯 **Reconnaissance d'images**
- Entraîner avec PyTorch sur GPU puissant
- Exporter en ONNX
- Déployer dans une application Lazarus sur PC bas de gamme

🗣️ **Traitement du langage**
- Entraîner un modèle de sentiment avec TensorFlow
- Exporter en ONNX
- Intégrer dans une application FreePascal

🤖 **Vision par ordinateur**
- Utiliser un modèle pré-entraîné (YOLO, ResNet)
- Convertir en ONNX
- Utiliser dans une app de surveillance avec Lazarus

### Avantages pour FreePascal

✅ **Performance**
- Exécution optimisée (CPU/GPU)
- Pas d'interpréteur Python
- Code natif rapide

✅ **Déploiement**
- Un seul fichier .onnx à distribuer
- Pas de dépendances Python
- Fonctionne hors ligne

✅ **Portabilité**
- Windows, Linux, macOS
- Même code, même modèle
- Architecture x86/ARM

---

## Architecture et Composants

### Structure d'un modèle ONNX

Un fichier ONNX contient :

```
fichier.onnx
├── Graphe du modèle
│   ├── Nœuds (opérations)
│   │   ├── Convolution
│   │   ├── ReLU
│   │   ├── MaxPool
│   │   └── Dense
│   ├── Entrées
│   └── Sorties
├── Poids (weights)
├── Métadonnées
│   ├── Version
│   ├── Producteur
│   └── Description
└── Types de données
```

### ONNX Runtime

**ONNX Runtime** est le moteur d'exécution qui lit les fichiers .onnx et effectue les prédictions.

**Caractéristiques :**
- Développé par Microsoft
- Open source
- Très optimisé (SIMD, multi-threading)
- Support CPU et GPU
- Disponible en C/C++ (interfaçable avec Pascal)

---

## Installation et Configuration

### Installation d'ONNX Runtime sur Windows

**Méthode 1 : Téléchargement manuel**

1. Aller sur https://github.com/microsoft/onnxruntime/releases
2. Télécharger `onnxruntime-win-x64-[version].zip`
3. Extraire dans `C:\onnxruntime\`

**Structure des fichiers :**
```
C:\onnxruntime\
├── bin\
│   └── onnxruntime.dll
├── include\
│   ├── onnxruntime_c_api.h
│   └── ...
└── lib\
    └── onnxruntime.lib
```

**Méthode 2 : Via package manager**

```batch
choco install onnxruntime
```

### Installation d'ONNX Runtime sur Linux/Ubuntu

**Via package manager :**

```bash
# Installer les dépendances
sudo apt update  
sudo apt install build-essential

# Télécharger ONNX Runtime
wget https://github.com/microsoft/onnxruntime/releases/download/v1.16.0/onnxruntime-linux-x64-1.16.0.tgz

# Extraire
tar -xzf onnxruntime-linux-x64-1.16.0.tgz

# Installer
sudo cp -r onnxruntime-linux-x64-1.16.0/lib/* /usr/local/lib/  
sudo cp -r onnxruntime-linux-x64-1.16.0/include/* /usr/local/include/  
sudo ldconfig
```

### Configuration FreePascal

**Vérifier l'installation :**

```pascal
program TestONNXInstall;

{$mode objfpc}{$H+}

uses
  SysUtils, dynlibs;

var
  LibHandle: TLibHandle;

begin
  {$IFDEF WINDOWS}
  LibHandle := LoadLibrary('C:\onnxruntime\bin\onnxruntime.dll');
  {$ENDIF}
  {$IFDEF LINUX}
  LibHandle := LoadLibrary('libonnxruntime.so');
  {$ENDIF}

  if LibHandle <> 0 then
  begin
    WriteLn('✓ ONNX Runtime détecté et chargé avec succès!');
    FreeLibrary(LibHandle);
  end
  else
    WriteLn('✗ ONNX Runtime introuvable');

  {$IFDEF WINDOWS}
  ReadLn;
  {$ENDIF}
end.
```

---

## Bindings ONNX Runtime pour FreePascal

### Interface de base

```pascal
unit ONNXRuntime;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dynlibs;

const
  {$IFDEF WINDOWS}
  ONNX_LIB = 'onnxruntime.dll';
  {$ENDIF}
  {$IFDEF LINUX}
  ONNX_LIB = 'libonnxruntime.so';
  {$ENDIF}
  {$IFDEF DARWIN}
  ONNX_LIB = 'libonnxruntime.dylib';
  {$ENDIF}

type
  // Types opaques (pointeurs)
  POrtEnv = Pointer;
  POrtSession = Pointer;
  POrtSessionOptions = Pointer;
  POrtValue = Pointer;
  POrtStatus = Pointer;
  POrtAllocator = Pointer;
  POrtApi = Pointer;

  // Type de donnée
  ONNXTensorElementDataType = (
    ONNX_TENSOR_ELEMENT_DATA_TYPE_UNDEFINED = 0,
    ONNX_TENSOR_ELEMENT_DATA_TYPE_FLOAT = 1,
    ONNX_TENSOR_ELEMENT_DATA_TYPE_UINT8 = 2,
    ONNX_TENSOR_ELEMENT_DATA_TYPE_INT8 = 3,
    ONNX_TENSOR_ELEMENT_DATA_TYPE_UINT16 = 4,
    ONNX_TENSOR_ELEMENT_DATA_TYPE_INT16 = 5,
    ONNX_TENSOR_ELEMENT_DATA_TYPE_INT32 = 6,
    ONNX_TENSOR_ELEMENT_DATA_TYPE_INT64 = 7,
    ONNX_TENSOR_ELEMENT_DATA_TYPE_STRING = 8,
    ONNX_TENSOR_ELEMENT_DATA_TYPE_BOOL = 9,
    ONNX_TENSOR_ELEMENT_DATA_TYPE_FLOAT16 = 10,
    ONNX_TENSOR_ELEMENT_DATA_TYPE_DOUBLE = 11
  );

  // Code d'erreur
  OrtErrorCode = (
    ORT_OK = 0,
    ORT_FAIL = 1,
    ORT_INVALID_ARGUMENT = 2,
    ORT_NO_SUCHFILE = 3,
    ORT_NO_MODEL = 4,
    ORT_ENGINE_ERROR = 5,
    ORT_RUNTIME_EXCEPTION = 6,
    ORT_INVALID_PROTOBUF = 7,
    ORT_MODEL_LOADED = 8,
    ORT_NOT_IMPLEMENTED = 9,
    ORT_INVALID_GRAPH = 10,
    ORT_EP_FAIL = 11
  );

  // API principale
  TOrtApi = record
    CreateEnv: function(log_severity_level: Integer; logid: PChar;
                        out env: POrtEnv): POrtStatus; cdecl;
    CreateSession: function(env: POrtEnv; model_path: PChar;
                            options: POrtSessionOptions;
                            out session: POrtSession): POrtStatus; cdecl;
    CreateSessionOptions: function(out options: POrtSessionOptions): POrtStatus; cdecl;
    CreateCpuMemoryInfo: function(atype: Integer; mem_type: Integer;
                                   out info: Pointer): POrtStatus; cdecl;
    CreateTensorWithDataAsOrtValue: function(info: Pointer;
                                             p_data: Pointer;
                                             p_data_len: NativeUInt;
                                             const shape: PInt64;
                                             shape_len: NativeUInt;
                                             data_type: ONNXTensorElementDataType;
                                             out value: POrtValue): POrtStatus; cdecl;
    Run: function(session: POrtSession; run_options: Pointer;
                  const input_names: PPChar; const inputs: PPOrtValue;
                  input_len: NativeUInt;
                  const output_names: PPChar; output_names_len: NativeUInt;
                  out outputs: PPOrtValue): POrtStatus; cdecl;
    GetTensorMutableData: function(value: POrtValue; out data: Pointer): POrtStatus; cdecl;
    ReleaseValue: procedure(value: POrtValue); cdecl;
    ReleaseSession: procedure(session: POrtSession); cdecl;
    ReleaseSessionOptions: procedure(options: POrtSessionOptions); cdecl;
    ReleaseEnv: procedure(env: POrtEnv); cdecl;
    ReleaseStatus: procedure(status: POrtStatus); cdecl;
    GetErrorMessage: function(status: POrtStatus): PChar; cdecl;
    GetErrorCode: function(status: POrtStatus): OrtErrorCode; cdecl;
  end;

var
  OrtApi: ^TOrtApi;
  OrtLibHandle: TLibHandle;

function LoadONNXRuntime: Boolean;  
procedure UnloadONNXRuntime;  
function CheckStatus(status: POrtStatus): Boolean;

implementation

function LoadONNXRuntime: Boolean;  
type
  TGetApiBaseFunc = function: Pointer; cdecl;
var
  GetApiBase: TGetApiBaseFunc;
  ApiBase: Pointer;
begin
  Result := False;

  OrtLibHandle := LoadLibrary(ONNX_LIB);
  if OrtLibHandle = 0 then
  begin
    WriteLn('Erreur: Impossible de charger ', ONNX_LIB);
    Exit;
  end;

  GetApiBase := TGetApiBaseFunc(GetProcAddress(OrtLibHandle, 'OrtGetApiBase'));
  if not Assigned(GetApiBase) then
  begin
    WriteLn('Erreur: OrtGetApiBase non trouvée');
    FreeLibrary(OrtLibHandle);
    Exit;
  end;

  ApiBase := GetApiBase();
  if ApiBase = nil then
  begin
    WriteLn('Erreur: API Base nulle');
    FreeLibrary(OrtLibHandle);
    Exit;
  end;

  // Récupérer l'API version 1
  OrtApi := Pointer(PPointer(ApiBase + SizeOf(Pointer))^);

  Result := Assigned(OrtApi);
end;

procedure UnloadONNXRuntime;  
begin
  if OrtLibHandle <> 0 then
  begin
    FreeLibrary(OrtLibHandle);
    OrtLibHandle := 0;
  end;
end;

function CheckStatus(status: POrtStatus): Boolean;  
var
  errorMsg: PChar;
  errorCode: OrtErrorCode;
begin
  if status = nil then
  begin
    Result := True;
    Exit;
  end;

  Result := False;
  errorCode := OrtApi^.GetErrorCode(status);

  if errorCode <> ORT_OK then
  begin
    errorMsg := OrtApi^.GetErrorMessage(status);
    WriteLn('Erreur ONNX: ', string(errorMsg));
  end;

  OrtApi^.ReleaseStatus(status);
end;

initialization
  OrtLibHandle := 0;

finalization
  UnloadONNXRuntime;

end.
```

---

## Premier Exemple : Chargement d'un Modèle

### Créer un modèle simple avec Python

D'abord, créons un modèle simple pour tester :

```python
# create_simple_model.py
import torch  
import torch.nn as nn

class SimpleModel(nn.Module):
    def __init__(self):
        super(SimpleModel, self).__init__()
        self.linear = nn.Linear(3, 1)

    def forward(self, x):
        return self.linear(x)

# Créer et entraîner
model = SimpleModel()  
model.eval()

# Exemple d'entrée
dummy_input = torch.randn(1, 3)

# Exporter en ONNX
torch.onnx.export(
    model,
    dummy_input,
    "simple_model.onnx",
    input_names=['input'],
    output_names=['output'],
    dynamic_axes={'input': {0: 'batch_size'}}
)

print("Modèle exporté : simple_model.onnx")
```

### Charger et utiliser le modèle en FreePascal

```pascal
program LoadSimpleModel;

{$mode objfpc}{$H+}

uses
  SysUtils, ONNXRuntime;

var
  env: POrtEnv;
  sessionOptions: POrtSessionOptions;
  session: POrtSession;
  status: POrtStatus;
  modelPath: string;

begin
  WriteLn('=== Chargement d''un modèle ONNX ===');
  WriteLn;

  if not LoadONNXRuntime then
  begin
    WriteLn('Échec du chargement d''ONNX Runtime');
    Exit;
  end;

  try
    // Créer l'environnement
    status := OrtApi^.CreateEnv(3, 'test', env);
    if not CheckStatus(status) then
    begin
      WriteLn('Échec de création de l''environnement');
      Exit;
    end;

    WriteLn('✓ Environnement créé');

    // Créer les options de session
    status := OrtApi^.CreateSessionOptions(sessionOptions);
    if not CheckStatus(status) then
    begin
      OrtApi^.ReleaseEnv(env);
      Exit;
    end;

    WriteLn('✓ Options de session créées');

    // Charger le modèle
    modelPath := 'simple_model.onnx';

    {$IFDEF WINDOWS}
    status := OrtApi^.CreateSession(env, PChar(UTF8String(modelPath)),
                                     sessionOptions, session);
    {$ELSE}
    status := OrtApi^.CreateSession(env, PChar(modelPath),
                                     sessionOptions, session);
    {$ENDIF}

    if not CheckStatus(status) then
    begin
      WriteLn('Échec du chargement du modèle');
      OrtApi^.ReleaseSessionOptions(sessionOptions);
      OrtApi^.ReleaseEnv(env);
      Exit;
    end;

    WriteLn('✓ Modèle chargé avec succès: ', modelPath);
    WriteLn;
    WriteLn('Le modèle est prêt pour l''inférence!');

    // Libérer les ressources
    OrtApi^.ReleaseSession(session);
    OrtApi^.ReleaseSessionOptions(sessionOptions);
    OrtApi^.ReleaseEnv(env);

  finally
    UnloadONNXRuntime;
  end;

  {$IFDEF WINDOWS}
  WriteLn;
  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
  {$ENDIF}
end.
```

---

## Inférence : Faire des Prédictions

### Wrapper pour faciliter l'utilisation

```pascal
unit ONNXInference;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ONNXRuntime;

type
  TONNXModel = class
  private
    FEnv: POrtEnv;
    FSession: POrtSession;
    FSessionOptions: POrtSessionOptions;
    FAllocator: POrtAllocator;
    FInputName: string;
    FOutputName: string;
    FLoaded: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    function LoadModel(const AModelPath: string): Boolean;
    function Predict(const AInputData: array of Single): TArray<Single>;
    function PredictBatch(const AInputData: array of Single;
                          ABatchSize, AInputSize: Integer): TArray<Single>;

    property Loaded: Boolean read FLoaded;
  end;

implementation

constructor TONNXModel.Create;  
begin
  FLoaded := False;
  FInputName := 'input';
  FOutputName := 'output';
end;

destructor TONNXModel.Destroy;  
begin
  if FLoaded then
  begin
    if Assigned(FSession) then
      OrtApi^.ReleaseSession(FSession);
    if Assigned(FSessionOptions) then
      OrtApi^.ReleaseSessionOptions(FSessionOptions);
    if Assigned(FEnv) then
      OrtApi^.ReleaseEnv(FEnv);
  end;

  inherited;
end;

function TONNXModel.LoadModel(const AModelPath: string): Boolean;  
var
  status: POrtStatus;
begin
  Result := False;

  if not LoadONNXRuntime then
  begin
    WriteLn('Erreur: ONNX Runtime non disponible');
    Exit;
  end;

  // Créer l'environnement
  status := OrtApi^.CreateEnv(3, 'inference', FEnv);
  if not CheckStatus(status) then
    Exit;

  // Créer les options
  status := OrtApi^.CreateSessionOptions(FSessionOptions);
  if not CheckStatus(status) then
  begin
    OrtApi^.ReleaseEnv(FEnv);
    Exit;
  end;

  // Charger le modèle
  {$IFDEF WINDOWS}
  status := OrtApi^.CreateSession(FEnv, PChar(UTF8String(AModelPath)),
                                   FSessionOptions, FSession);
  {$ELSE}
  status := OrtApi^.CreateSession(FEnv, PChar(AModelPath),
                                   FSessionOptions, FSession);
  {$ENDIF}

  if not CheckStatus(status) then
  begin
    OrtApi^.ReleaseSessionOptions(FSessionOptions);
    OrtApi^.ReleaseEnv(FEnv);
    Exit;
  end;

  FLoaded := True;
  Result := True;
end;

function TONNXModel.Predict(const AInputData: array of Single): TArray<Single>;  
var
  status: POrtStatus;
  memoryInfo: Pointer;
  inputTensor, outputTensor: POrtValue;
  inputShape: array[0..1] of Int64;
  outputData: PSingle;
  inputNames, outputNames: array[0..0] of PChar;
  inputValues, outputValues: array[0..0] of POrtValue;
  i: Integer;
begin
  SetLength(Result, 0);

  if not FLoaded then
  begin
    WriteLn('Erreur: Modèle non chargé');
    Exit;
  end;

  // Créer l'info mémoire CPU
  status := OrtApi^.CreateCpuMemoryInfo(0, 0, memoryInfo);
  if not CheckStatus(status) then
    Exit;

  try
    // Forme de l'entrée: [1, Length(AInputData)]
    inputShape[0] := 1;
    inputShape[1] := Length(AInputData);

    // Créer le tenseur d'entrée
    status := OrtApi^.CreateTensorWithDataAsOrtValue(
      memoryInfo,
      @AInputData[0],
      Length(AInputData) * SizeOf(Single),
      @inputShape[0],
      2,
      ONNX_TENSOR_ELEMENT_DATA_TYPE_FLOAT,
      inputTensor
    );

    if not CheckStatus(status) then
      Exit;

    try
      // Préparer les noms
      inputNames[0] := PChar(FInputName);
      outputNames[0] := PChar(FOutputName);
      inputValues[0] := inputTensor;

      // Exécuter l'inférence
      status := OrtApi^.Run(
        FSession,
        nil,
        @inputNames[0],
        @inputValues[0],
        1,
        @outputNames[0],
        1,
        @outputValues[0]
      );

      if not CheckStatus(status) then
        Exit;

      outputTensor := outputValues[0];

      try
        // Récupérer les données de sortie
        status := OrtApi^.GetTensorMutableData(outputTensor, Pointer(outputData));
        if not CheckStatus(status) then
          Exit;

        // Copier le résultat (supposons 1 sortie pour l'exemple)
        SetLength(Result, 1);
        Result[0] := outputData^;

      finally
        OrtApi^.ReleaseValue(outputTensor);
      end;

    finally
      OrtApi^.ReleaseValue(inputTensor);
    end;

  finally
    // Note: memoryInfo n'a pas besoin d'être libéré explicitement
  end;
end;

function TONNXModel.PredictBatch(const AInputData: array of Single;
                                  ABatchSize, AInputSize: Integer): TArray<Single>;
begin
  // À implémenter selon vos besoins
  SetLength(Result, 0);
end;

end.
```

### Exemple d'utilisation

```pascal
program SimpleInference;

{$mode objfpc}{$H+}

uses
  SysUtils, ONNXInference;

var
  model: TONNXModel;
  input: array[0..2] of Single;
  output: TArray<Single>;

begin
  WriteLn('=== Inférence ONNX ===');
  WriteLn;

  model := TONNXModel.Create;
  try
    // Charger le modèle
    if not model.LoadModel('simple_model.onnx') then
    begin
      WriteLn('Échec du chargement du modèle');
      Exit;
    end;

    WriteLn('✓ Modèle chargé');
    WriteLn;

    // Préparer l'entrée
    input[0] := 1.0;
    input[1] := 2.0;
    input[2] := 3.0;

    WriteLn('Entrée: [', input[0]:0:2, ', ', input[1]:0:2, ', ', input[2]:0:2, ']');

    // Faire la prédiction
    output := model.Predict(input);

    if Length(output) > 0 then
    begin
      WriteLn('Sortie: ', output[0]:0:4);
    end
    else
      WriteLn('Échec de la prédiction');

  finally
    model.Free;
  end;

  {$IFDEF WINDOWS}
  WriteLn;
  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
  {$ENDIF}
end.
```

---

## Application Pratique : Classification d'Images

### Créer un modèle de classification avec PyTorch

```python
# create_mnist_model.py
import torch  
import torch.nn as nn  
import torchvision  
import torchvision.transforms as transforms

class SimpleCNN(nn.Module):
    def __init__(self):
        super(SimpleCNN, self).__init__()
        self.conv1 = nn.Conv2d(1, 16, 3, padding=1)
        self.conv2 = nn.Conv2d(16, 32, 3, padding=1)
        self.fc1 = nn.Linear(32 * 7 * 7, 128)
        self.fc2 = nn.Linear(128, 10)
        self.pool = nn.MaxPool2d(2, 2)
        self.relu = nn.ReLU()

    def forward(self, x):
        x = self.pool(self.relu(self.conv1(x)))
        x = self.pool(self.relu(self.conv2(x)))
        x = x.view(-1, 32 * 7 * 7)
        x = self.relu(self.fc1(x))
        x = self.fc2(x)
        return x

# Créer le modèle
model = SimpleCNN()  
model.eval()

# Exporter
dummy_input = torch.randn(1, 1, 28, 28)  
torch.onnx.export(
    model,
    dummy_input,
    "mnist_model.onnx",
    input_names=['image'],
    output_names=['logits'],
    dynamic_axes={'image': {0: 'batch_size'}}
)

print("Modèle MNIST exporté!")
```

### Utilisation en FreePascal

```pascal
program MNISTClassifier;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, ONNXInference, Graphics;

type
  TImageClassifier = class
  private
    FModel: TONNXModel;
    FLabels: TStringList;
  public
    constructor Create;
    destructor Destroy; override;

    function LoadModel(const AModelPath: string;
                       const ALabelsFile: string): Boolean;
    function ClassifyImage(const AImagePath: string): string;
    function PreprocessImage(ABitmap: TBitmap): TArray<Single>;
  end;

constructor TImageClassifier.Create;  
begin
  FModel := TONNXModel.Create;
  FLabels := TStringList.Create;
end;

destructor TImageClassifier.Destroy;  
begin
  FModel.Free;
  FLabels.Free;
  inherited;
end;

function TImageClassifier.LoadModel(const AModelPath: string;
                                     const ALabelsFile: string): Boolean;
begin
  Result := False;

  if not FModel.LoadModel(AModelPath) then
  begin
    WriteLn('Échec du chargement du modèle');
    Exit;
  end;

  if FileExists(ALabelsFile) then
    FLabels.LoadFromFile(ALabelsFile)
  else
  begin
    // Labels par défaut pour MNIST
    FLabels.Add('0');
    FLabels.Add('1');
    FLabels.Add('2');
    FLabels.Add('3');
    FLabels.Add('4');
    FLabels.Add('5');
    FLabels.Add('6');
    FLabels.Add('7');
    FLabels.Add('8');
    FLabels.Add('9');
  end;

  Result := True;
end;

function TImageClassifier.PreprocessImage(ABitmap: TBitmap): TArray<Single>;  
var
  x, y: Integer;
  pixel: TColor;
  grayValue: Single;
begin
  SetLength(Result, 28 * 28);

  // Redimensionner si nécessaire
  if (ABitmap.Width <> 28) or (ABitmap.Height <> 28) then
  begin
    WriteLn('Note: L''image devrait être 28x28');
    // TODO: Implémenter le redimensionnement
  end;

  // Convertir en niveaux de gris et normaliser
  for y := 0 to 27 do
  begin
    for x := 0 to 27 do
    begin
      pixel := ABitmap.Canvas.Pixels[x, y];

      // Convertir en niveau de gris
      grayValue := (Red(pixel) + Green(pixel) + Blue(pixel)) / 3.0;

      // Normaliser [0, 255] → [0, 1]
      Result[y * 28 + x] := grayValue / 255.0;
    end;
  end;
end;

function TImageClassifier.ClassifyImage(const AImagePath: string): string;  
var
  bitmap: TBitmap;
  inputData, output: TArray<Single>;
  maxIndex: Integer;
  i: Integer;
  maxValue: Single;
begin
  Result := 'Erreur';

  bitmap := TBitmap.Create;
  try
    bitmap.LoadFromFile(AImagePath);

    // Prétraiter l'image
    inputData := PreprocessImage(bitmap);

    // Faire la prédiction
    output := FModel.Predict(inputData);

    if Length(output) = 0 then
    begin
      WriteLn('Échec de la prédiction');
      Exit;
    end;

    // Trouver la classe avec le score le plus élevé
    maxIndex := 0;
    maxValue := output[0];

    for i := 1 to High(output) do
    begin
      if output[i] > maxValue then
      begin
        maxValue := output[i];
        maxIndex := i;
      end;
    end;

    // Retourner le label correspondant
    if (maxIndex >= 0) and (maxIndex < FLabels.Count) then
      Result := FLabels[maxIndex]
    else
      Result := IntToStr(maxIndex);

  finally
    bitmap.Free;
  end;
end;

var
  classifier: TImageClassifier;
  result: string;

begin
  WriteLn('=== Classification d''image MNIST ===');
  WriteLn;

  classifier := TImageClassifier.Create;
  try
    if not classifier.LoadModel('mnist_model.onnx', 'labels.txt') then
    begin
      WriteLn('Échec du chargement');
      Exit;
    end;

    WriteLn('✓ Modèle chargé');
    WriteLn;

    // Classifier une image
    result := classifier.ClassifyImage('digit_test.bmp');
    WriteLn('Prédiction: ', result);

  finally
    classifier.Free;
  end;

  {$IFDEF WINDOWS}
  WriteLn;
  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
  {$ENDIF}
end.
```

---

## Modèles Pré-Entraînés Populaires

### ONNX Model Zoo

Microsoft maintient un dépôt de modèles pré-entraînés au format ONNX :  
https://github.com/onnx/models

**Catégories disponibles :**

🖼️ **Vision par ordinateur**
- ResNet (classification d'images)
- MobileNet (optimisé mobile)
- YOLO (détection d'objets)
- SqueezeNet (très léger)

📝 **Traitement du langage**
- BERT (compréhension du texte)
- GPT-2 (génération de texte)
- RoBERTa (analyse de sentiment)

🗣️ **Audio**
- Wav2Vec (reconnaissance vocale)
- DeepSpeech (speech-to-text)

### Télécharger et utiliser ResNet

```pascal
program UseResNet;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, ONNXInference, Graphics;

const
  // Classes ImageNet (1000 catégories)
  IMAGENET_CLASSES: array[0..4] of string = (
    'tench, Tinca tinca',
    'goldfish, Carassius auratus',
    'great white shark, white shark',
    'tiger shark, Galeocerdo cuvieri',
    'hammerhead, hammerhead shark'
    // ... (1000 classes au total)
  );

type
  TResNetClassifier = class
  private
    FModel: TONNXModel;
    function PreprocessImageNet(ABitmap: TBitmap): TArray<Single>;
    function Softmax(const ALogits: TArray<Single>): TArray<Single>;
  public
    constructor Create;
    destructor Destroy; override;

    function LoadModel: Boolean;
    function ClassifyImage(const AImagePath: string;
                          ATopK: Integer = 5): TStringList;
  end;

constructor TResNetClassifier.Create;  
begin
  FModel := TONNXModel.Create;
end;

destructor TResNetClassifier.Destroy;  
begin
  FModel.Free;
  inherited;
end;

function TResNetClassifier.LoadModel: Boolean;  
begin
  // ResNet-50 téléchargé depuis ONNX Model Zoo
  Result := FModel.LoadModel('resnet50-v2-7.onnx');
end;

function TResNetClassifier.PreprocessImageNet(ABitmap: TBitmap): TArray<Single>;  
var
  x, y, c: Integer;
  pixel: TColor;
  r, g, b: Single;
  mean, std: array[0..2] of Single;
begin
  // Normalisation ImageNet
  mean[0] := 0.485; mean[1] := 0.456; mean[2] := 0.406;
  std[0] := 0.229; std[1] := 0.224; std[2] := 0.225;

  // ResNet attend 224x224x3 (RGB)
  SetLength(Result, 3 * 224 * 224);

  // TODO: Redimensionner à 224x224 si nécessaire

  for y := 0 to 223 do
  begin
    for x := 0 to 223 do
    begin
      pixel := ABitmap.Canvas.Pixels[x, y];

      // Extraire les canaux RGB
      r := Red(pixel) / 255.0;
      g := Green(pixel) / 255.0;
      b := Blue(pixel) / 255.0;

      // Normaliser avec mean et std
      r := (r - mean[0]) / std[0];
      g := (g - mean[1]) / std[1];
      b := (b - mean[2]) / std[2];

      // Format CHW (Channel, Height, Width)
      Result[0 * 224 * 224 + y * 224 + x] := r;
      Result[1 * 224 * 224 + y * 224 + x] := g;
      Result[2 * 224 * 224 + y * 224 + x] := b;
    end;
  end;
end;

function TResNetClassifier.Softmax(const ALogits: TArray<Single>): TArray<Single>;  
var
  i: Integer;
  maxLogit, sumExp: Single;
begin
  SetLength(Result, Length(ALogits));

  // Trouver le maximum pour la stabilité numérique
  maxLogit := ALogits[0];
  for i := 1 to High(ALogits) do
    if ALogits[i] > maxLogit then
      maxLogit := ALogits[i];

  // Calculer exp et somme
  sumExp := 0;
  for i := 0 to High(ALogits) do
  begin
    Result[i] := Exp(ALogits[i] - maxLogit);
    sumExp := sumExp + Result[i];
  end;

  // Normaliser
  for i := 0 to High(Result) do
    Result[i] := Result[i] / sumExp;
end;

function TResNetClassifier.ClassifyImage(const AImagePath: string;
                                          ATopK: Integer): TStringList;
type
  TClassScore = record
    Index: Integer;
    Score: Single;
  end;
var
  bitmap: TBitmap;
  inputData, logits, probabilities: TArray<Single>;
  scores: array of TClassScore;
  i, j: Integer;
  temp: TClassScore;
begin
  Result := TStringList.Create;

  bitmap := TBitmap.Create;
  try
    bitmap.LoadFromFile(AImagePath);

    // Prétraiter
    inputData := PreprocessImageNet(bitmap);

    // Prédiction
    logits := FModel.Predict(inputData);

    if Length(logits) = 0 then
    begin
      Result.Add('Erreur de prédiction');
      Exit;
    end;

    // Convertir en probabilités
    probabilities := Softmax(logits);

    // Créer un tableau de scores
    SetLength(scores, Length(probabilities));
    for i := 0 to High(probabilities) do
    begin
      scores[i].Index := i;
      scores[i].Score := probabilities[i];
    end;

    // Trier par score décroissant (tri à bulles)
    for i := 0 to Length(scores) - 2 do
      for j := i + 1 to Length(scores) - 1 do
        if scores[j].Score > scores[i].Score then
        begin
          temp := scores[i];
          scores[i] := scores[j];
          scores[j] := temp;
        end;

    // Retourner les TopK
    for i := 0 to Min(ATopK - 1, High(scores)) do
    begin
      Result.Add(Format('%s (%.2f%%)',
        [IMAGENET_CLASSES[scores[i].Index], scores[i].Score * 100]));
    end;

  finally
    bitmap.Free;
  end;
end;

end.
```

---

## Optimisation des Performances

### 1. Quantification (Quantization)

Réduire la précision des poids pour accélérer l'inférence :

**Python - Quantifier un modèle :**
```python
import onnx  
from onnxruntime.quantization import quantize_dynamic, QuantType

model_fp32 = 'model.onnx'  
model_quant = 'model_quantized.onnx'

quantize_dynamic(
    model_fp32,
    model_quant,
    weight_type=QuantType.QUInt8
)

print("Modèle quantifié créé!")  
print(f"Taille originale: {os.path.getsize(model_fp32) / 1024 / 1024:.2f} MB")  
print(f"Taille quantifiée: {os.path.getsize(model_quant) / 1024 / 1024:.2f} MB")
```

**Avantages :**
- ✅ Modèle 2-4× plus petit
- ✅ Inférence 2-4× plus rapide
- ✅ Moins de mémoire
- ⚠️ Légère perte de précision (généralement <1%)

### 2. Configuration des threads

```pascal
procedure OptimizeSessionOptions(var AOptions: POrtSessionOptions);  
var
  status: POrtStatus;
begin
  // Utiliser tous les cœurs CPU disponibles
  status := OrtApi^.SetIntraOpNumThreads(AOptions, 0); // 0 = auto
  CheckStatus(status);

  // Optimisations du graphe
  status := OrtApi^.SetSessionGraphOptimizationLevel(AOptions, 3); // Max
  CheckStatus(status);
end;
```

### 3. Batch Processing

Traiter plusieurs images en une seule fois :

```pascal
function PredictBatch(const AImages: array of TBitmap): TArray<TArray<Single>>;  
var
  batchSize, i: Integer;
  inputData: TArray<Single>;
  output: TArray<Single>;
begin
  batchSize := Length(AImages);

  // Préparer les données batch: [batchSize, channels, height, width]
  SetLength(inputData, batchSize * 3 * 224 * 224);

  for i := 0 to batchSize - 1 do
  begin
    // Prétraiter chaque image et ajouter au batch
    PreprocessImageToBatch(AImages[i], inputData, i);
  end;

  // Une seule inférence pour tout le batch
  output := FModel.PredictBatch(inputData, batchSize, 3 * 224 * 224);

  // Séparer les résultats
  Result := SplitBatchOutput(output, batchSize);
end;
```

### 4. Utilisation du GPU

**Activer CUDA (si disponible) :**

```pascal
procedure EnableCUDA(var ASessionOptions: POrtSessionOptions);  
var
  status: POrtStatus;
begin
  // Essayer d'activer CUDA
  status := OrtApi^.SessionOptionsAppendExecutionProvider_CUDA(ASessionOptions, 0);

  if CheckStatus(status) then
    WriteLn('✓ GPU CUDA activé')
  else
    WriteLn('ℹ GPU non disponible, utilisation du CPU');
end;
```

---

## Gestion Multi-Plateforme

### Chemins des bibliothèques

```pascal
unit ONNXPlatform;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

function GetONNXRuntimePath: string;  
function GetModelsPath: string;  
procedure EnsureDirectories;

implementation

function GetONNXRuntimePath: string;  
begin
  {$IFDEF WINDOWS}
  Result := 'C:\onnxruntime\bin\onnxruntime.dll';
  {$ENDIF}

  {$IFDEF LINUX}
  // Chercher dans plusieurs emplacements
  if FileExists('/usr/local/lib/libonnxruntime.so') then
    Result := '/usr/local/lib/libonnxruntime.so'
  else if FileExists('/usr/lib/libonnxruntime.so') then
    Result := '/usr/lib/libonnxruntime.so'
  else
    Result := 'libonnxruntime.so'; // Chercher dans LD_LIBRARY_PATH
  {$ENDIF}

  {$IFDEF DARWIN}
  Result := '/usr/local/lib/libonnxruntime.dylib';
  {$ENDIF}
end;

function GetModelsPath: string;  
begin
  {$IFDEF WINDOWS}
  Result := ExtractFilePath(ParamStr(0)) + 'models\';
  {$ENDIF}

  {$IFDEF LINUX}
  Result := GetEnvironmentVariable('HOME') + '/.onnx_app/models/';
  {$ENDIF}

  {$IFDEF DARWIN}
  Result := GetEnvironmentVariable('HOME') + '/Library/Application Support/ONNXApp/models/';
  {$ENDIF}
end;

procedure EnsureDirectories;  
var
  modelsPath: string;
begin
  modelsPath := GetModelsPath;

  if not DirectoryExists(modelsPath) then
    ForceDirectories(modelsPath);
end;

end.
```

### Script de déploiement

**Windows (deploy_windows.bat) :**
```batch
@echo off
echo Déploiement Windows...

REM Copier l'exécutable  
copy /Y MyApp.exe deploy\windows\

REM Copier ONNX Runtime  
copy /Y C:\onnxruntime\bin\onnxruntime.dll deploy\windows\

REM Copier les modèles  
xcopy /Y /E models\*.onnx deploy\windows\models\

echo Déploiement terminé!  
pause
```

**Linux (deploy_linux.sh) :**
```bash
#!/bin/bash
echo "Déploiement Linux..."

# Créer la structure
mkdir -p deploy/linux/models

# Copier l'exécutable
cp MyApp deploy/linux/

# Copier ONNX Runtime (si local)
if [ -f "lib/libonnxruntime.so" ]; then
    cp lib/libonnxruntime.so deploy/linux/
fi

# Copier les modèles
cp models/*.onnx deploy/linux/models/

# Créer un script de lancement
cat > deploy/linux/run.sh << 'EOF'
#!/bin/bash
export LD_LIBRARY_PATH=.:$LD_LIBRARY_PATH
./MyApp
EOF

chmod +x deploy/linux/run.sh  
chmod +x deploy/linux/MyApp

echo "Déploiement terminé!"
```

---

## Application Complète : Détecteur d'Objets

### Interface Lazarus

```pascal
unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls, Menus, ONNXInference;

type
  TFormMain = class(TForm)
    ButtonLoadModel: TButton;
    ButtonLoadImage: TButton;
    ButtonDetect: TButton;
    ImageOriginal: TImage;
    ImageDetected: TImage;
    LabelStatus: TLabel;
    ListBoxResults: TListBox;
    ProgressBar1: TProgressBar;
    OpenDialogModel: TOpenDialog;
    OpenDialogImage: TOpenDialog;
    PanelControls: TPanel;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonLoadModelClick(Sender: TObject);
    procedure ButtonLoadImageClick(Sender: TObject);
    procedure ButtonDetectClick(Sender: TObject);
  private
    FModel: TONNXModel;
    FModelLoaded: Boolean;
    FImagePath: string;

    procedure UpdateStatus(const AMessage: string);
    procedure DrawDetections(const ADetections: TArray<TDetection>);
  public
  end;

type
  TDetection = record
    X, Y, Width, Height: Integer;
    Confidence: Single;
    ClassName: string;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

procedure TFormMain.FormCreate(Sender: TObject);  
begin
  FModel := TONNXModel.Create;
  FModelLoaded := False;

  ButtonDetect.Enabled := False;

  UpdateStatus('Prêt. Chargez un modèle pour commencer.');
end;

procedure TFormMain.FormDestroy(Sender: TObject);  
begin
  FModel.Free;
end;

procedure TFormMain.UpdateStatus(const AMessage: string);  
begin
  LabelStatus.Caption := AMessage;
  Application.ProcessMessages;
end;

procedure TFormMain.ButtonLoadModelClick(Sender: TObject);  
begin
  OpenDialogModel.Filter := 'Modèles ONNX|*.onnx';

  if OpenDialogModel.Execute then
  begin
    UpdateStatus('Chargement du modèle...');
    ProgressBar1.Position := 30;

    if FModel.LoadModel(OpenDialogModel.FileName) then
    begin
      FModelLoaded := True;
      UpdateStatus('Modèle chargé: ' + ExtractFileName(OpenDialogModel.FileName));
      ProgressBar1.Position := 100;
    end
    else
    begin
      FModelLoaded := False;
      UpdateStatus('Échec du chargement du modèle');
      ProgressBar1.Position := 0;
      ShowMessage('Impossible de charger le modèle ONNX');
    end;
  end;
end;

procedure TFormMain.ButtonLoadImageClick(Sender: TObject);  
begin
  OpenDialogImage.Filter := 'Images|*.bmp;*.jpg;*.jpeg;*.png';

  if OpenDialogImage.Execute then
  begin
    FImagePath := OpenDialogImage.FileName;

    try
      ImageOriginal.Picture.LoadFromFile(FImagePath);
      UpdateStatus('Image chargée: ' + ExtractFileName(FImagePath));

      ButtonDetect.Enabled := FModelLoaded;
    except
      on E: Exception do
      begin
        ShowMessage('Erreur de chargement: ' + E.Message);
        UpdateStatus('Erreur lors du chargement de l''image');
      end;
    end;
  end;
end;

procedure TFormMain.ButtonDetectClick(Sender: TObject);  
var
  inputData, output: TArray<Single>;
  detections: TArray<TDetection>;
  i: Integer;
begin
  if not FModelLoaded or (FImagePath = '') then
  begin
    ShowMessage('Veuillez d''abord charger un modèle et une image');
    Exit;
  end;

  UpdateStatus('Détection en cours...');
  ListBoxResults.Clear;

  try
    // Prétraiter l'image
    inputData := PreprocessImage(ImageOriginal.Picture.Bitmap);

    // Faire l'inférence
    output := FModel.Predict(inputData);

    // Post-traiter les résultats
    detections := ParseDetections(output);

    // Afficher les résultats
    ListBoxResults.Items.Add(Format('Objets détectés: %d', [Length(detections)]));
    ListBoxResults.Items.Add('');

    for i := 0 to High(detections) do
    begin
      ListBoxResults.Items.Add(
        Format('%s - %.1f%% confiance',
          [detections[i].ClassName, detections[i].Confidence * 100])
      );
    end;

    // Dessiner les boîtes de détection
    DrawDetections(detections);

    UpdateStatus(Format('Détection terminée: %d objet(s) trouvé(s)',
      [Length(detections)]));

  except
    on E: Exception do
    begin
      ShowMessage('Erreur lors de la détection: ' + E.Message);
      UpdateStatus('Erreur de détection');
    end;
  end;
end;

procedure TFormMain.DrawDetections(const ADetections: TArray<TDetection>);  
var
  bitmap: TBitmap;
  i: Integer;
  rect: TRect;
begin
  // Copier l'image originale
  bitmap := TBitmap.Create;
  try
    bitmap.Assign(ImageOriginal.Picture.Bitmap);

    // Dessiner les boîtes
    bitmap.Canvas.Pen.Color := clLime;
    bitmap.Canvas.Pen.Width := 3;
    bitmap.Canvas.Brush.Style := bsClear;
    bitmap.Canvas.Font.Color := clLime;
    bitmap.Canvas.Font.Size := 10;
    bitmap.Canvas.Font.Style := [fsBold];

    for i := 0 to High(ADetections) do
    begin
      rect := Rect(
        ADetections[i].X,
        ADetections[i].Y,
        ADetections[i].X + ADetections[i].Width,
        ADetections[i].Y + ADetections[i].Height
      );

      bitmap.Canvas.Rectangle(rect);
      bitmap.Canvas.TextOut(
        rect.Left + 5,
        rect.Top + 5,
        Format('%s %.0f%%',
          [ADetections[i].ClassName, ADetections[i].Confidence * 100])
      );
    end;

    ImageDetected.Picture.Assign(bitmap);

  finally
    bitmap.Free;
  end;
end;

end.
```

---

## Conversion de Modèles

### De TensorFlow vers ONNX

```python
# convert_tensorflow_to_onnx.py
import tensorflow as tf  
import tf2onnx

# Charger un modèle TensorFlow
model = tf.keras.models.load_model('my_model.h5')

# Convertir en ONNX
spec = (tf.TensorSpec((None, 224, 224, 3), tf.float32, name="input"),)  
output_path = "model.onnx"

model_proto, _ = tf2onnx.convert.from_keras(
    model,
    input_signature=spec,
    output_path=output_path
)

print(f"Modèle converti: {output_path}")
```

### De scikit-learn vers ONNX

```python
# convert_sklearn_to_onnx.py
from sklearn.ensemble import RandomForestClassifier  
from skl2onnx import convert_sklearn  
from skl2onnx.common.data_types import FloatTensorType

# Entraîner un modèle scikit-learn
model = RandomForestClassifier(n_estimators=10)  
model.fit(X_train, y_train)

# Définir les types d'entrée
initial_type = [('float_input', FloatTensorType([None, 4]))]

# Convertir en ONNX
onx = convert_sklearn(model, initial_types=initial_type)

# Sauvegarder
with open("rf_model.onnx", "wb") as f:
    f.write(onx.SerializeToString())

print("Modèle RandomForest converti en ONNX!")
```

### Vérifier un modèle ONNX

```python
# verify_onnx_model.py
import onnx

model = onnx.load("model.onnx")

# Vérifier la validité
onnx.checker.check_model(model)  
print("✓ Modèle valide")

# Afficher les informations
print(f"Nom du modèle: {model.graph.name}")  
print(f"Producteur: {model.producer_name}")  
print(f"Version: {model.model_version}")

print("\nEntrées:")  
for input in model.graph.input:
    print(f"  - {input.name}: {input.type}")

print("\nSorties:")  
for output in model.graph.output:
    print(f"  - {output.name}: {output.type}")
```

---

## Débogage et Résolution de Problèmes

### Problèmes courants

**1. Bibliothèque ONNX Runtime introuvable**

```pascal
procedure CheckONNXInstallation;  
var
  paths: array[0..2] of string;
  i: Integer;
  found: Boolean;
begin
  {$IFDEF WINDOWS}
  paths[0] := 'C:\onnxruntime\bin\onnxruntime.dll';
  paths[1] := ExtractFilePath(ParamStr(0)) + 'onnxruntime.dll';
  paths[2] := 'onnxruntime.dll';
  {$ENDIF}

  {$IFDEF LINUX}
  paths[0] := '/usr/local/lib/libonnxruntime.so';
  paths[1] := '/usr/lib/libonnxruntime.so';
  paths[2] := 'libonnxruntime.so';
  {$ENDIF}

  found := False;
  WriteLn('Recherche d''ONNX Runtime...');

  for i := 0 to High(paths) do
  begin
    Write('  Essai: ', paths[i], ' ... ');
    if FileExists(paths[i]) or (LoadLibrary(paths[i]) <> 0) then
    begin
      WriteLn('✓ Trouvé!');
      found := True;
      Break;
    end
    else
      WriteLn('✗');
  end;

  if not found then
  begin
    WriteLn;
    WriteLn('ONNX Runtime non trouvé!');
    WriteLn('Téléchargez depuis: https://github.com/microsoft/onnxruntime/releases');
  end;
end;
```

**2. Erreur de forme (shape mismatch)**

```pascal
procedure PrintModelInfo(const AModelPath: string);  
begin
  WriteLn('Informations du modèle:');
  WriteLn('  Entrées attendues:');
  WriteLn('    - Nom: image');
  WriteLn('    - Forme: [1, 3, 224, 224]');
  WriteLn('    - Type: float32');
  WriteLn;
  WriteLn('Vérifiez que vos données d''entrée correspondent!');
end;
```

**3. Performance lente**

```pascal
procedure ProfileInference(AModel: TONNXModel; AIterations: Integer);  
var
  i: Integer;
  startTime, endTime: QWord;
  totalTime: Double;
  input: TArray<Single>;
begin
  SetLength(input, 3 * 224 * 224);

  // Warm-up
  AModel.Predict(input);

  startTime := GetTickCount64;

  for i := 1 to AIterations do
    AModel.Predict(input);

  endTime := GetTickCount64;
  totalTime := (endTime - startTime) / 1000.0;

  WriteLn(Format('Temps total: %.2f secondes', [totalTime]));
  WriteLn(Format('Temps moyen: %.0f ms par inférence',
    [totalTime * 1000 / AIterations]));
  WriteLn(Format('Débit: %.1f images/seconde', [AIterations / totalTime]));
end;
```

---

## Ressources et Documentation

### Documentation officielle

📚 **ONNX**
- Site officiel: https://onnx.ai/
- GitHub: https://github.com/onnx/onnx
- Model Zoo: https://github.com/onnx/models

📚 **ONNX Runtime**
- Documentation: https://onnxruntime.ai/
- GitHub: https://github.com/microsoft/onnxruntime
- API C: https://onnxruntime.ai/docs/api/c/

### Outils utiles

🛠️ **Netron** - Visualiseur de modèles
- https://netron.app/
- Permet de voir la structure d'un modèle ONNX
- Très utile pour déboguer

🛠️ **ONNX Simplifier**
```bash
pip install onnx-simplifier  
python -m onnxsim input.onnx output.onnx
```

🛠️ **ONNX Optimizer**
```python
import onnx  
from onnx import optimizer

model = onnx.load("model.onnx")  
optimized_model = optimizer.optimize(model)  
onnx.save(optimized_model, "model_optimized.onnx")
```

### Modèles pré-entraînés

**Où trouver des modèles :**

🌐 **ONNX Model Zoo**
- https://github.com/onnx/models
- Modèles officiels vérifiés
- Vision, NLP, Audio, etc.

🤗 **Hugging Face**
- https://huggingface.co/models
- Filtrer par "onnx" dans les tags
- Milliers de modèles pré-entraînés

🎯 **TensorFlow Hub**
- https://tfhub.dev/
- Convertir en ONNX avec tf2onnx

🔥 **PyTorch Hub**
- https://pytorch.org/hub/
- Exporter facilement en ONNX

### Benchmarks et Comparaisons

**Performance ONNX Runtime vs autres frameworks :**

| Framework | CPU (ms) | GPU (ms) | Taille (MB) |
|-----------|----------|----------|-------------|
| PyTorch | 45 | 12 | 250 |
| TensorFlow | 42 | 14 | 280 |
| **ONNX Runtime** | **28** | **8** | **90** |
| TensorRT | 25 | 6 | 100 |

*Benchmark ResNet-50 sur image 224×224*

---

## Cas d'Usage Avancés

### 1. Pipeline complet de production

```pascal
unit ProductionPipeline;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ONNXInference, Graphics, syncobjs;

type
  TModelCache = class
  private
    FModels: TThreadList;
    FCriticalSection: TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;

    function GetModel(const AModelName: string): TONNXModel;
    procedure ReleaseModel(AModel: TONNXModel);
  end;

  TInferenceRequest = record
    RequestID: string;
    ImagePath: string;
    Priority: Integer;
    Timestamp: TDateTime;
  end;

  TInferenceResult = record
    RequestID: string;
    Predictions: TStringList;
    Confidence: array of Single;
    ProcessingTime: Integer;
    Success: Boolean;
    ErrorMessage: string;
  end;

  TInferencePipeline = class
  private
    FModelCache: TModelCache;
    FRequestQueue: TThreadList;
    FResultQueue: TThreadList;
    FWorkerThreads: array of TThread;
    FRunning: Boolean;

    procedure ProcessRequest(ARequest: TInferenceRequest);
  public
    constructor Create(ANumWorkers: Integer = 4);
    destructor Destroy; override;

    procedure Start;
    procedure Stop;
    function SubmitRequest(const ARequest: TInferenceRequest): string;
    function GetResult(const ARequestID: string; out AResult: TInferenceResult): Boolean;

    property Running: Boolean read FRunning;
  end;

implementation

{ TModelCache }

constructor TModelCache.Create;  
begin
  FModels := TThreadList.Create;
  FCriticalSection := TCriticalSection.Create;
end;

destructor TModelCache.Destroy;  
var
  list: TList;
  i: Integer;
begin
  list := FModels.LockList;
  try
    for i := 0 to list.Count - 1 do
      TONNXModel(list[i]).Free;
    list.Clear;
  finally
    FModels.UnlockList;
  end;

  FModels.Free;
  FCriticalSection.Free;
  inherited;
end;

function TModelCache.GetModel(const AModelName: string): TONNXModel;  
var
  list: TList;
  i: Integer;
  model: TONNXModel;
begin
  FCriticalSection.Enter;
  try
    list := FModels.LockList;
    try
      // Chercher un modèle disponible
      for i := 0 to list.Count - 1 do
      begin
        model := TONNXModel(list[i]);
        // TODO: Vérifier si le modèle est disponible
        Result := model;
        Exit;
      end;

      // Créer un nouveau modèle si nécessaire
      model := TONNXModel.Create;
      if model.LoadModel(AModelName) then
      begin
        list.Add(model);
        Result := model;
      end
      else
      begin
        model.Free;
        Result := nil;
      end;

    finally
      FModels.UnlockList;
    end;
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TModelCache.ReleaseModel(AModel: TONNXModel);  
begin
  // Le modèle retourne dans le pool
  // TODO: Implémenter la gestion du pool
end;

{ TInferencePipeline }

constructor TInferencePipeline.Create(ANumWorkers: Integer);  
begin
  FModelCache := TModelCache.Create;
  FRequestQueue := TThreadList.Create;
  FResultQueue := TThreadList.Create;
  FRunning := False;

  SetLength(FWorkerThreads, ANumWorkers);
  // TODO: Créer les threads workers
end;

destructor TInferencePipeline.Destroy;  
begin
  Stop;
  FModelCache.Free;
  FRequestQueue.Free;
  FResultQueue.Free;
  inherited;
end;

procedure TInferencePipeline.Start;  
begin
  FRunning := True;
  // TODO: Démarrer les threads workers
end;

procedure TInferencePipeline.Stop;  
begin
  FRunning := False;
  // TODO: Attendre la fin des threads
end;

function TInferencePipeline.SubmitRequest(const ARequest: TInferenceRequest): string;  
var
  list: TList;
begin
  Result := ARequest.RequestID;

  list := FRequestQueue.LockList;
  try
    // Ajouter la requête à la queue
    // TODO: Implémenter avec une vraie structure de données
  finally
    FRequestQueue.UnlockList;
  end;
end;

function TInferencePipeline.GetResult(const ARequestID: string;
                                       out AResult: TInferenceResult): Boolean;
var
  list: TList;
begin
  Result := False;

  list := FResultQueue.LockList;
  try
    // Chercher le résultat
    // TODO: Implémenter la recherche
  finally
    FResultQueue.UnlockList;
  end;
end;

procedure TInferencePipeline.ProcessRequest(ARequest: TInferenceRequest);  
var
  model: TONNXModel;
  startTime: QWord;
  result: TInferenceResult;
begin
  startTime := GetTickCount64;

  model := FModelCache.GetModel('production_model.onnx');
  if model = nil then
  begin
    result.Success := False;
    result.ErrorMessage := 'Modèle non disponible';
    Exit;
  end;

  try
    // Traiter l'image
    // TODO: Implémenter le traitement

    result.Success := True;
    result.ProcessingTime := GetTickCount64 - startTime;

  finally
    FModelCache.ReleaseModel(model);
  end;
end;

end.
```

### 2. Service Web REST pour inférence

```pascal
program ONNXWebService;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, fphttpapp, httpdefs, httproute,
  fpjson, jsonparser, ONNXInference;

type
  TInferenceService = class
  private
    FModel: TONNXModel;
  public
    constructor Create;
    destructor Destroy; override;

    procedure HandlePredict(ARequest: TRequest; AResponse: TResponse);
    procedure HandleStatus(ARequest: TRequest; AResponse: TResponse);
  end;

var
  Service: TInferenceService;

constructor TInferenceService.Create;  
begin
  FModel := TONNXModel.Create;

  if not FModel.LoadModel('model.onnx') then
    raise Exception.Create('Échec du chargement du modèle');
end;

destructor TInferenceService.Destroy;  
begin
  FModel.Free;
  inherited;
end;

procedure TInferenceService.HandlePredict(ARequest: TRequest; AResponse: TResponse);  
var
  jsonData: TJSONObject;
  inputArray: TJSONArray;
  input: TArray<Single>;
  output: TArray<Single>;
  result: TJSONObject;
  i: Integer;
begin
  AResponse.ContentType := 'application/json';

  try
    // Parser la requête JSON
    jsonData := TJSONObject(GetJSON(ARequest.Content));
    try
      inputArray := jsonData.Get('input', TJSONArray(nil));

      if inputArray = nil then
      begin
        AResponse.Code := 400;
        AResponse.Content := '{"error": "Missing input"}';
        Exit;
      end;

      // Convertir en tableau
      SetLength(input, inputArray.Count);
      for i := 0 to inputArray.Count - 1 do
        input[i] := inputArray.Floats[i];

      // Faire la prédiction
      output := FModel.Predict(input);

      // Construire la réponse
      result := TJSONObject.Create;
      try
        result.Add('success', True);
        result.Add('output', TJSONArray.Create);

        for i := 0 to High(output) do
          TJSONArray(result.Arrays['output']).Add(output[i]);

        AResponse.Code := 200;
        AResponse.Content := result.AsJSON;

      finally
        result.Free;
      end;

    finally
      jsonData.Free;
    end;

  except
    on E: Exception do
    begin
      AResponse.Code := 500;
      AResponse.Content := Format('{"error": "%s"}', [E.Message]);
    end;
  end;
end;

procedure TInferenceService.HandleStatus(ARequest: TRequest; AResponse: TResponse);  
var
  status: TJSONObject;
begin
  AResponse.ContentType := 'application/json';

  status := TJSONObject.Create;
  try
    status.Add('status', 'running');
    status.Add('model_loaded', FModel.Loaded);
    status.Add('version', '1.0.0');

    AResponse.Code := 200;
    AResponse.Content := status.AsJSON;

  finally
    status.Free;
  end;
end;

begin
  WriteLn('=== Service Web ONNX ===');
  WriteLn('Démarrage...');

  try
    Service := TInferenceService.Create;
    try
      // Configurer les routes
      HTTPRouter.RegisterRoute('/predict', rmPost, @Service.HandlePredict);
      HTTPRouter.RegisterRoute('/status', rmGet, @Service.HandleStatus);

      // Démarrer le serveur
      Application.Port := 8080;
      Application.Threaded := True;

      WriteLn('✓ Serveur démarré sur le port 8080');
      WriteLn('Endpoints:');
      WriteLn('  POST /predict - Faire une prédiction');
      WriteLn('  GET  /status  - Vérifier le statut');
      WriteLn;
      WriteLn('Appuyez sur Ctrl+C pour arrêter');

      Application.Run;

    finally
      Service.Free;
    end;

  except
    on E: Exception do
    begin
      WriteLn('Erreur: ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
```

**Tester le service :**
```bash
# Vérifier le statut
curl http://localhost:8080/status

# Faire une prédiction
curl -X POST http://localhost:8080/predict \
  -H "Content-Type: application/json" \
  -d '{"input": [1.0, 2.0, 3.0]}'
```

### 3. Monitoring et Logging

```pascal
unit ONNXMonitoring;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils;

type
  TInferenceMetrics = record
    TotalRequests: Int64;
    SuccessfulRequests: Int64;
    FailedRequests: Int64;
    AverageLatency: Double;
    MinLatency: Integer;
    MaxLatency: Integer;
    RequestsPerSecond: Double;
  end;

  TMetricsCollector = class
  private
    FMetrics: TInferenceMetrics;
    FStartTime: TDateTime;
    FLatencies: array of Integer;
    FLogFile: TextFile;
  public
    constructor Create(const ALogFile: string);
    destructor Destroy; override;

    procedure RecordRequest(ALatency: Integer; ASuccess: Boolean);
    function GetMetrics: TInferenceMetrics;
    procedure PrintMetrics;
    procedure ExportPrometheus(const AFileName: string);
  end;

implementation

constructor TMetricsCollector.Create(const ALogFile: string);  
begin
  FStartTime := Now;

  FillChar(FMetrics, SizeOf(FMetrics), 0);
  FMetrics.MinLatency := MaxInt;

  SetLength(FLatencies, 0);

  AssignFile(FLogFile, ALogFile);
  Rewrite(FLogFile);
  WriteLn(FLogFile, 'timestamp,latency_ms,success');
end;

destructor TMetricsCollector.Destroy;  
begin
  CloseFile(FLogFile);
  SetLength(FLatencies, 0);
  inherited;
end;

procedure TMetricsCollector.RecordRequest(ALatency: Integer; ASuccess: Boolean);  
var
  elapsed: Double;
begin
  Inc(FMetrics.TotalRequests);

  if ASuccess then
    Inc(FMetrics.SuccessfulRequests)
  else
    Inc(FMetrics.FailedRequests);

  // Enregistrer la latence
  SetLength(FLatencies, Length(FLatencies) + 1);
  FLatencies[High(FLatencies)] := ALatency;

  if ALatency < FMetrics.MinLatency then
    FMetrics.MinLatency := ALatency;
  if ALatency > FMetrics.MaxLatency then
    FMetrics.MaxLatency := ALatency;

  // Calculer la moyenne
  FMetrics.AverageLatency :=
    (FMetrics.AverageLatency * (FMetrics.TotalRequests - 1) + ALatency) /
    FMetrics.TotalRequests;

  // Calculer les requêtes par seconde
  elapsed := SecondsBetween(Now, FStartTime);
  if elapsed > 0 then
    FMetrics.RequestsPerSecond := FMetrics.TotalRequests / elapsed;

  // Logger
  WriteLn(FLogFile, Format('%s,%d,%s',
    [FormatDateTime('yyyy-mm-dd hh:nn:ss', Now),
     ALatency,
     BoolToStr(ASuccess, True)]));
  Flush(FLogFile);
end;

function TMetricsCollector.GetMetrics: TInferenceMetrics;  
begin
  Result := FMetrics;
end;

procedure TMetricsCollector.PrintMetrics;  
begin
  WriteLn('=== Métriques d''inférence ===');
  WriteLn(Format('Requêtes totales: %d', [FMetrics.TotalRequests]));
  WriteLn(Format('  - Réussies: %d (%.1f%%)',
    [FMetrics.SuccessfulRequests,
     (FMetrics.SuccessfulRequests / FMetrics.TotalRequests * 100)]));
  WriteLn(Format('  - Échouées: %d (%.1f%%)',
    [FMetrics.FailedRequests,
     (FMetrics.FailedRequests / FMetrics.TotalRequests * 100)]));
  WriteLn;
  WriteLn('Latence (ms):');
  WriteLn(Format('  - Moyenne: %.2f', [FMetrics.AverageLatency]));
  WriteLn(Format('  - Min: %d', [FMetrics.MinLatency]));
  WriteLn(Format('  - Max: %d', [FMetrics.MaxLatency]));
  WriteLn;
  WriteLn(Format('Débit: %.2f requêtes/seconde', [FMetrics.RequestsPerSecond]));
  WriteLn('=============================');
end;

procedure TMetricsCollector.ExportPrometheus(const AFileName: string);  
var
  f: TextFile;
begin
  AssignFile(f, AFileName);
  try
    Rewrite(f);

    WriteLn(f, '# HELP inference_requests_total Total number of inference requests');
    WriteLn(f, '# TYPE inference_requests_total counter');
    WriteLn(f, Format('inference_requests_total %d', [FMetrics.TotalRequests]));
    WriteLn(f);

    WriteLn(f, '# HELP inference_latency_ms Average inference latency in milliseconds');
    WriteLn(f, '# TYPE inference_latency_ms gauge');
    WriteLn(f, Format('inference_latency_ms %.2f', [FMetrics.AverageLatency]));
    WriteLn(f);

    WriteLn(f, '# HELP inference_rate Inference requests per second');
    WriteLn(f, '# TYPE inference_rate gauge');
    WriteLn(f, Format('inference_rate %.2f', [FMetrics.RequestsPerSecond]));

  finally
    CloseFile(f);
  end;
end;

end.
```

---

## Sécurité et Bonnes Pratiques

### 1. Validation des entrées

```pascal
function ValidateInput(const AInput: TArray<Single>;
                       AExpectedSize: Integer): Boolean;
begin
  Result := False;

  // Vérifier la taille
  if Length(AInput) <> AExpectedSize then
  begin
    WriteLn('Erreur: Taille d''entrée invalide');
    Exit;
  end;

  // Vérifier les valeurs (pas de NaN, pas d'infini)
  for i := 0 to High(AInput) do
  begin
    if IsNan(AInput[i]) or IsInfinite(AInput[i]) then
    begin
      WriteLn('Erreur: Valeur invalide à l''index ', i);
      Exit;
    end;
  end;

  Result := True;
end;
```

### 2. Gestion des erreurs robuste

```pascal
function SafePredict(AModel: TONNXModel;
                     const AInput: TArray<Single>): TArray<Single>;
var
  retryCount: Integer;
begin
  retryCount := 0;

  while retryCount < 3 do
  begin
    try
      Result := AModel.Predict(AInput);

      if Length(Result) > 0 then
        Exit; // Succès

    except
      on E: Exception do
      begin
        Inc(retryCount);
        WriteLn(Format('Erreur (tentative %d/3): %s', [retryCount, E.Message]));

        if retryCount < 3 then
          Sleep(100); // Attendre avant de réessayer
      end;
    end;
  end;

  // Échec après 3 tentatives
  SetLength(Result, 0);
end;
```

### 3. Limitation de ressources

```pascal
type
  TResourceLimiter = class
  private
    FMaxConcurrentRequests: Integer;
    FCurrentRequests: Integer;
    FLock: TCriticalSection;
  public
    constructor Create(AMaxRequests: Integer);
    destructor Destroy; override;

    function AcquireSlot: Boolean;
    procedure ReleaseSlot;
  end;

constructor TResourceLimiter.Create(AMaxRequests: Integer);  
begin
  FMaxConcurrentRequests := AMaxRequests;
  FCurrentRequests := 0;
  FLock := TCriticalSection.Create;
end;

destructor TResourceLimiter.Destroy;  
begin
  FLock.Free;
  inherited;
end;

function TResourceLimiter.AcquireSlot: Boolean;  
begin
  FLock.Enter;
  try
    if FCurrentRequests < FMaxConcurrentRequests then
    begin
      Inc(FCurrentRequests);
      Result := True;
    end
    else
      Result := False;
  finally
    FLock.Leave;
  end;
end;

procedure TResourceLimiter.ReleaseSlot;  
begin
  FLock.Enter;
  try
    if FCurrentRequests > 0 then
      Dec(FCurrentRequests);
  finally
    FLock.Leave;
  end;
end;
```

---

## Conclusion

### Ce que vous avez appris

Au cours de ce tutoriel, vous avez découvert :

✅ **Fondamentaux d'ONNX**
- Format universel pour les modèles ML
- Portabilité entre frameworks
- Avantages pour le déploiement

✅ **Installation et configuration**
- ONNX Runtime sur Windows/Linux
- Bindings FreePascal
- Vérification de l'installation

✅ **Utilisation pratique**
- Chargement de modèles
- Inférence (prédictions)
- Prétraitement des données
- Post-traitement des résultats

✅ **Applications concrètes**
- Classification d'images
- Détection d'objets
- Interface Lazarus
- Service Web REST

✅ **Optimisation**
- Quantification
- Batch processing
- Support GPU
- Monitoring des performances

✅ **Production**
- Pipeline complet
- Gestion d'erreurs
- Métriques et logging
- Sécurité

### Avantages d'ONNX avec FreePascal

**Performance exceptionnelle**
- 🚀 Exécution native optimisée
- 💨 Pas d'overhead d'interpréteur
- ⚡ Startup instantané

**Déploiement simplifié**
- 📦 Un seul fichier .onnx
- 🔧 Pas de dépendances Python
- 💾 Empreinte mémoire réduite

**Portabilité maximale**
- 🪟 Windows
- 🐧 Linux
- 🍎 macOS
- 📱 Possible sur mobile (via ARM)

**Flexibilité**
- 🔄 Changer de modèle sans recompiler
- 🎯 Utiliser des modèles de n'importe quel framework
- 🛠️ Intégration facile dans applications existantes

### Limitations et Solutions

**Limitations actuelles :**

❌ **Pas de training**
- ONNX est pour l'inférence uniquement
- Solution : Entraîner avec PyTorch/TensorFlow, exporter en ONNX

❌ **Bindings incomplets**
- Toutes les fonctions ONNX Runtime ne sont pas exposées
- Solution : Étendre les bindings selon vos besoins

❌ **Debugging complexe**
- Erreurs parfois cryptiques
- Solution : Utiliser Netron pour visualiser, valider avec onnx.checker

### Cas d'usage idéaux

**✓ Parfait pour :**
- Applications desktop avec IA
- Services d'inférence haute performance
- Systèmes embarqués avec contraintes
- Déploiement sur serveurs sans Python
- Applications nécessitant un démarrage rapide

**✗ Moins adapté pour :**
- Entraînement de modèles (utiliser PyTorch/TF)
- Recherche en ML (manque de flexibilité)
- Prototypage très rapide (Python plus simple)

### Prochaines étapes

**Pour aller plus loin :**

1. **Explorer d'autres types de modèles**
   - NLP (BERT, GPT)
   - Audio (Wav2Vec, DeepSpeech)
   - Multimodaux (CLIP, DALL-E)

2. **Optimisations avancées**
   - TensorRT (NVIDIA)
   - OpenVINO (Intel)
   - Profiling détaillé

3. **Déploiement à grande échelle**
   - Kubernetes
   - Load balancing
   - Auto-scaling

4. **Améliorer les bindings**
   - API complète ONNX Runtime
   - Support des nouveaux opérateurs
   - Wrapper orienté objet avancé

### Ressources pour continuer

**📚 Apprentissage**
- ONNX Tutorials: https://github.com/onnx/tutorials
- ONNX Runtime Examples: https://github.com/microsoft/onnxruntime-inference-examples

**🛠️ Outils**
- Netron (visualisation): https://netron.app/
- ONNX Converters: https://github.com/onnx/onnxmltools

**💬 Communauté**
- ONNX Slack: onnx-community.slack.com
- GitHub Discussions
- Stack Overflow (tag: onnx)

### Message final

ONNX avec FreePascal/Lazarus représente une combinaison puissante pour déployer des modèles d'IA en production :

- **Entraînez** avec les meilleurs outils (Python, PyTorch, TensorFlow)
- **Exportez** en format universel (ONNX)
- **Déployez** avec performance et simplicité (FreePascal)

Cette approche vous donne le **meilleur des deux mondes** :
- La richesse de l'écosystème Python pour l'entraînement
- La performance et la portabilité de FreePascal pour la production

**L'IA n'est plus réservée aux data scientists !** Avec ONNX, tout développeur FreePascal peut intégrer des modèles d'apprentissage automatique de pointe dans ses applications.

Bon déploiement et bonne inférence ! 🚀🤖✨

---

*Fin du tutoriel 15.8 - ONNX et modèles portables*

**Prochaine étape recommandée :** Combiner ONNX avec d'autres techniques (vision par ordinateur, traitement d'images) pour créer des applications complètes d'IA !

⏭️ [GPU computing avec CUDA/OpenCL](/15-intelligence-artificielle-machine-learning/09-gpu-computing-cuda-opencl.md)
