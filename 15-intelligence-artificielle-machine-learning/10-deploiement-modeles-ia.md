🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 15.10 Déploiement de modèles IA

## Introduction

Le déploiement de modèles d'intelligence artificielle consiste à mettre en production des modèles entraînés pour qu'ils puissent être utilisés dans des applications réelles. Avec FreePascal/Lazarus, vous pouvez intégrer et déployer des modèles IA de différentes manières, que ce soit sur Windows ou Ubuntu.

## Concepts fondamentaux

### Qu'est-ce qu'un modèle IA ?

Un modèle IA est essentiellement un ensemble de paramètres mathématiques (poids, biais) obtenus après un processus d'apprentissage. Ce modèle peut effectuer des prédictions, classifications ou autres tâches intelligentes.

### Formats de modèles courants

- **ONNX** (Open Neural Network Exchange) : Format universel compatible avec de nombreux frameworks
- **TensorFlow SavedModel** : Format natif de TensorFlow
- **PyTorch (.pt, .pth)** : Format natif de PyTorch
- **Pickle (.pkl)** : Format Python pour la sérialisation
- **Modèles propriétaires** : Formats spécifiques à certains frameworks

## Stratégies de déploiement avec FreePascal

### 1. Approche par bibliothèque native

Utiliser des bibliothèques Pascal natives qui implémentent les algorithmes directement.

**Avantages :**
- Pas de dépendances externes complexes
- Performance optimale
- Portabilité garantie Windows/Ubuntu
- Contrôle total sur le code

**Inconvénients :**
- Nécessite de réimplémenter les algorithmes
- Limité aux modèles simples

```pascal
unit SimpleNeuralNet;

interface

type
  TMatrix = array of array of Double;

  TNeuralNetwork = class
  private
    FWeights: array of TMatrix;
    FBiases: array of TMatrix;
  public
    constructor Create;
    procedure LoadModel(const FileName: string);
    function Predict(const Input: TMatrix): TMatrix;
    function Sigmoid(x: Double): Double;
  end;

implementation

uses
  SysUtils, Classes;

constructor TNeuralNetwork.Create;  
begin
  inherited Create;
end;

procedure TNeuralNetwork.LoadModel(const FileName: string);  
var
  F: TextFile;
  Line: string;
begin
  // Charger les poids et biais depuis un fichier
  AssignFile(F, FileName);
  Reset(F);
  try
    // Parser le fichier de modèle
    while not Eof(F) do
    begin
      ReadLn(F, Line);
      // Traitement des données...
    end;
  finally
    CloseFile(F);
  end;
end;

function TNeuralNetwork.Sigmoid(x: Double): Double;  
begin
  Result := 1.0 / (1.0 + Exp(-x));
end;

function TNeuralNetwork.Predict(const Input: TMatrix): TMatrix;  
var
  i, j, k: Integer;
  Sum: Double;
begin
  // Implémentation simplifiée d'une prédiction
  // Forward pass à travers les couches
  SetLength(Result, 1, Length(FBiases[High(FBiases)]));

  for i := 0 to High(FWeights) do
  begin
    // Multiplication matricielle et activation
    for j := 0 to High(FWeights[i]) do
    begin
      Sum := FBiases[i][j][0];
      for k := 0 to High(FWeights[i][j]) do
        Sum := Sum + Input[0][k] * FWeights[i][j][k];
      Result[0][j] := Sigmoid(Sum);
    end;
  end;
end;

end.
```

### 2. Interopérabilité avec Python

L'approche la plus pratique pour utiliser des modèles complexes consiste à interfacer FreePascal avec Python.

#### Installation de Python4Lazarus

**Sur Windows :**
```bash
# Installer Python
choco install python

# Installer les dépendances IA
pip install tensorflow numpy onnxruntime
```

**Sur Ubuntu :**
```bash
# Installer Python et pip
sudo apt install python3 python3-pip

# Installer les dépendances IA
pip3 install tensorflow numpy onnxruntime
```

#### Utilisation de Python4Lazarus

```pascal
unit AIModelDeployment;

interface

uses
  Classes, SysUtils, PythonEngine, PythonGUIInputOutput;

type
  TAIModel = class
  private
    FPythonEngine: TPythonEngine;
    FPythonModule: TPythonModule;
    FModelLoaded: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function LoadModel(const ModelPath: string): Boolean;
    function Predict(const InputData: array of Double): TArray<Double>;
  end;

implementation

constructor TAIModel.Create;  
begin
  inherited Create;
  FPythonEngine := TPythonEngine.Create(nil);

  {$IFDEF WINDOWS}
  FPythonEngine.DllPath := 'C:\Python310';
  FPythonEngine.DllName := 'python310.dll';
  {$ENDIF}

  {$IFDEF UNIX}
  FPythonEngine.DllPath := '/usr/lib';
  FPythonEngine.DllName := 'libpython3.10.so';
  {$ENDIF}

  FPythonEngine.LoadDll;
  FModelLoaded := False;
end;

destructor TAIModel.Destroy;  
begin
  FPythonEngine.Free;
  inherited Destroy;
end;

function TAIModel.LoadModel(const ModelPath: string): Boolean;  
var
  Script: TStringList;
begin
  Result := False;
  Script := TStringList.Create;
  try
    Script.Add('import tensorflow as tf');
    Script.Add('import numpy as np');
    Script.Add('');
    Script.Add('def load_model(path):');
    Script.Add('    global model');
    Script.Add('    model = tf.keras.models.load_model(path)');
    Script.Add('    return True');
    Script.Add('');
    Script.Add('def predict(input_data):');
    Script.Add('    input_array = np.array(input_data).reshape(1, -1)');
    Script.Add('    prediction = model.predict(input_array)');
    Script.Add('    return prediction.tolist()[0]');

    FPythonEngine.ExecStrings(Script);

    // Charger le modèle
    FPythonEngine.ExecString(
      Format('load_model("%s")', [StringReplace(ModelPath, '\', '/', [rfReplaceAll])])
    );

    FModelLoaded := True;
    Result := True;
  finally
    Script.Free;
  end;
end;

function TAIModel.Predict(const InputData: array of Double): TArray<Double>;  
var
  PyInput, PyResult: PPyObject;
  i, ResultSize: Integer;
  InputStr: string;
begin
  if not FModelLoaded then
    raise Exception.Create('Modèle non chargé');

  // Construire la chaîne d'entrée
  InputStr := '[';
  for i := Low(InputData) to High(InputData) do
  begin
    InputStr := InputStr + FloatToStr(InputData[i]);
    if i < High(InputData) then
      InputStr := InputStr + ', ';
  end;
  InputStr := InputStr + ']';

  // Exécuter la prédiction
  PyResult := FPythonEngine.EvalString(Format('predict(%s)', [InputStr]));

  // Extraire les résultats
  if PyResult <> nil then
  begin
    ResultSize := FPythonEngine.PyList_Size(PyResult);
    SetLength(Result, ResultSize);

    for i := 0 to ResultSize - 1 do
      Result[i] := FPythonEngine.PyFloat_AsDouble(
        FPythonEngine.PyList_GetItem(PyResult, i)
      );
  end;
end;

end.
```

### 3. Utilisation d'ONNX Runtime

ONNX Runtime est une solution performante et portable pour déployer des modèles.

#### Installation ONNX Runtime

**Windows :**
- Télécharger les DLL depuis https://github.com/microsoft/onnxruntime/releases
- Placer `onnxruntime.dll` dans le dossier de l'application

**Ubuntu :**
```bash
sudo apt install libonnxruntime-dev
```

#### Interface Pascal pour ONNX

```pascal
unit ONNXModel;

interface

uses
  Classes, SysUtils, DynLibs;

type
  POrtEnv = Pointer;
  POrtSession = Pointer;
  POrtSessionOptions = Pointer;
  POrtValue = Pointer;

  TONNXModel = class
  private
    FLibHandle: TLibHandle;
    FEnv: POrtEnv;
    FSession: POrtSession;
    FSessionOptions: POrtSessionOptions;

    // Pointeurs de fonctions
    FCreateEnv: function(log_level: Integer; logid: PChar; out env: POrtEnv): Integer; cdecl;
    FCreateSessionOptions: function(out options: POrtSessionOptions): Integer; cdecl;
    FCreateSession: function(env: POrtEnv; model_path: PChar;
      options: POrtSessionOptions; out session: POrtSession): Integer; cdecl;
    FRun: function(session: POrtSession; run_options: Pointer;
      input_names: PPChar; inputs: PPOrtValue; input_count: NativeInt;
      output_names: PPChar; output_count: NativeInt;
      outputs: PPOrtValue): Integer; cdecl;

    function LoadONNXLibrary: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function LoadModel(const ModelPath: string): Boolean;
    function Predict(const Input: array of Single): TArray<Single>;
  end;

implementation

constructor TONNXModel.Create;  
begin
  inherited Create;
  LoadONNXLibrary;
end;

destructor TONNXModel.Destroy;  
begin
  if FSession <> nil then
    // Libérer la session
  if FEnv <> nil then
    // Libérer l'environnement
  if FLibHandle <> 0 then
    UnloadLibrary(FLibHandle);
  inherited Destroy;
end;

function TONNXModel.LoadONNXLibrary: Boolean;  
begin
  {$IFDEF WINDOWS}
  FLibHandle := LoadLibrary('onnxruntime.dll');
  {$ENDIF}

  {$IFDEF UNIX}
  FLibHandle := LoadLibrary('libonnxruntime.so');
  {$ENDIF}

  Result := FLibHandle <> 0;

  if Result then
  begin
    // Charger les fonctions
    FCreateEnv := GetProcAddress(FLibHandle, 'OrtCreateEnv');
    FCreateSessionOptions := GetProcAddress(FLibHandle, 'OrtCreateSessionOptions');
    FCreateSession := GetProcAddress(FLibHandle, 'OrtCreateSession');
    FRun := GetProcAddress(FLibHandle, 'OrtRun');
  end;
end;

function TONNXModel.LoadModel(const ModelPath: string): Boolean;  
begin
  Result := False;

  // Créer l'environnement ONNX
  if FCreateEnv(3, 'FreePascalONNX', FEnv) <> 0 then
    Exit;

  // Créer les options de session
  if FCreateSessionOptions(FSessionOptions) <> 0 then
    Exit;

  // Charger le modèle
  if FCreateSession(FEnv, PChar(ModelPath), FSessionOptions, FSession) = 0 then
    Result := True;
end;

function TONNXModel.Predict(const Input: array of Single): TArray<Single>;  
begin
  // Implémentation de la prédiction avec ONNX Runtime
  // Cette partie nécessite la gestion des tenseurs ONNX
  // et l'appel à FRun avec les paramètres appropriés

  // Code simplifié pour l'exemple
  SetLength(Result, 1);
  Result[0] := 0.0;
end;

end.
```

### 4. API REST pour modèles IA

Déployer le modèle comme service web est une approche moderne et flexible.

```pascal
unit AIWebService;

interface

uses
  fphttpapp, httpdefs, httproute, fpjson, jsonparser, Classes, SysUtils;

type
  TAIService = class
  private
    FModel: TAIModel; // Votre classe de modèle
  public
    constructor Create;
    destructor Destroy; override;
    procedure HandlePredict(ARequest: TRequest; AResponse: TResponse);
    procedure HandleHealth(ARequest: TRequest; AResponse: TResponse);
  end;

implementation

constructor TAIService.Create;  
begin
  inherited Create;
  FModel := TAIModel.Create;
  FModel.LoadModel('model.h5');

  // Enregistrer les routes
  HTTPRouter.RegisterRoute('/predict', rmPost, @HandlePredict);
  HTTPRouter.RegisterRoute('/health', rmGet, @HandleHealth);
end;

destructor TAIService.Destroy;  
begin
  FModel.Free;
  inherited Destroy;
end;

procedure TAIService.HandlePredict(ARequest: TRequest; AResponse: TResponse);  
var
  JSONData, JSONResult: TJSONObject;
  JSONArray: TJSONArray;
  Input: array of Double;
  Output: TArray<Double>;
  i: Integer;
begin
  try
    // Parser la requête JSON
    JSONData := GetJSON(ARequest.Content) as TJSONObject;
    JSONArray := JSONData.Get('input', TJSONArray(nil));

    // Convertir en tableau
    SetLength(Input, JSONArray.Count);
    for i := 0 to JSONArray.Count - 1 do
      Input[i] := JSONArray.Floats[i];

    // Faire la prédiction
    Output := FModel.Predict(Input);

    // Construire la réponse
    JSONResult := TJSONObject.Create;
    JSONArray := TJSONArray.Create;

    for i := 0 to High(Output) do
      JSONArray.Add(Output[i]);

    JSONResult.Add('prediction', JSONArray);

    AResponse.Content := JSONResult.AsJSON;
    AResponse.ContentType := 'application/json';
    AResponse.Code := 200;

    JSONResult.Free;
    JSONData.Free;
  except
    on E: Exception do
    begin
      AResponse.Content := Format('{"error": "%s"}', [E.Message]);
      AResponse.ContentType := 'application/json';
      AResponse.Code := 500;
    end;
  end;
end;

procedure TAIService.HandleHealth(ARequest: TRequest; AResponse: TResponse);  
begin
  AResponse.Content := '{"status": "healthy"}';
  AResponse.ContentType := 'application/json';
  AResponse.Code := 200;
end;

end.
```

## Optimisation et bonnes pratiques

### 1. Gestion de la mémoire

```pascal
procedure OptimizeMemory;  
begin
  // Libérer la mémoire régulièrement
  if GetHeapStatus.TotalAllocated > MaxMemoryThreshold then
  begin
    // Déclencher le garbage collection si disponible
    // ou libérer les ressources non utilisées
  end;
end;
```

### 2. Mise en cache des prédictions

```pascal
type
  TPredictionCache = class
  private
    FCache: TFPHashList;
  public
    function GetPrediction(const Input: string): TArray<Double>;
    procedure AddPrediction(const Input: string; const Output: TArray<Double>);
  end;
```

### 3. Threading pour performance

```pascal
type
  TPredictionThread = class(TThread)
  private
    FInput: TArray<Double>;
    FOutput: TArray<Double>;
    FModel: TAIModel;
  protected
    procedure Execute; override;
  public
    constructor Create(AModel: TAIModel; const AInput: TArray<Double>);
    property Output: TArray<Double> read FOutput;
  end;
```

## Déploiement en production

### Configuration Windows

**Service Windows :**
```pascal
program AIModelService;

{$mode objfpc}{$H+}

uses
  Windows, SysUtils, ServiceManager;

type
  TAIModelService = class(TService)
  protected
    function DoStart: Boolean; override;
    function DoStop: Boolean; override;
  end;

function TAIModelService.DoStart: Boolean;  
begin
  // Initialiser le modèle et démarrer le serveur
  Result := True;
end;

function TAIModelService.DoStop: Boolean;  
begin
  // Arrêter proprement le service
  Result := True;
end;

var
  Service: TAIModelService;
begin
  Service := TAIModelService.Create(nil);
  try
    Service.Run;
  finally
    Service.Free;
  end;
end.
```

### Configuration Ubuntu (systemd)

Créer un fichier `/etc/systemd/system/aimodel.service` :

```ini
[Unit]
Description=AI Model Service  
After=network.target

[Service]
Type=simple  
User=aiservice  
WorkingDirectory=/opt/aimodel  
ExecStart=/opt/aimodel/aiservice  
Restart=always  
RestartSec=10

[Install]
WantedBy=multi-user.target
```

Activer et démarrer le service :
```bash
sudo systemctl enable aimodel.service  
sudo systemctl start aimodel.service  
sudo systemctl status aimodel.service
```

### Monitoring et logs

```pascal
unit AILogger;

interface

uses
  SysUtils, Classes;

type
  TLogLevel = (llDebug, llInfo, llWarning, llError);

  TAILogger = class
  private
    FLogFile: TextFile;
    FLogPath: string;
  public
    constructor Create(const LogPath: string);
    destructor Destroy; override;
    procedure Log(Level: TLogLevel; const Msg: string);
  end;

implementation

constructor TAILogger.Create(const LogPath: string);  
begin
  inherited Create;
  FLogPath := LogPath;
  AssignFile(FLogFile, FLogPath);
  {$I-}
  Append(FLogFile);
  {$I+}
  if IOResult <> 0 then
    Rewrite(FLogFile);
end;

destructor TAILogger.Destroy;  
begin
  CloseFile(FLogFile);
  inherited Destroy;
end;

procedure TAILogger.Log(Level: TLogLevel; const Msg: string);  
var
  LevelStr: string;
begin
  case Level of
    llDebug: LevelStr := 'DEBUG';
    llInfo: LevelStr := 'INFO';
    llWarning: LevelStr := 'WARNING';
    llError: LevelStr := 'ERROR';
  end;

  WriteLn(FLogFile, Format('[%s] %s: %s',
    [FormatDateTime('yyyy-mm-dd hh:nn:ss', Now), LevelStr, Msg]));
  Flush(FLogFile);
end;

end.
```

## Docker et conteneurisation

### Dockerfile pour Windows

```dockerfile
FROM mcr.microsoft.com/windows/servercore:ltsc2022

# Installer FreePascal
ADD https://sourceforge.net/projects/freepascal/files/Win32/3.2.2/fpc-3.2.2.i386-win32.exe /fpc-installer.exe  
RUN fpc-installer.exe /SILENT

# Copier l'application
COPY aiservice.exe /app/  
COPY model.onnx /app/  
COPY onnxruntime.dll /app/

WORKDIR /app  
EXPOSE 8080

CMD ["aiservice.exe"]
```

### Dockerfile pour Ubuntu

```dockerfile
FROM ubuntu:22.04

# Installer les dépendances
RUN apt-get update && apt-get install -y \
    fpc \
    libonnxruntime-dev \
    && rm -rf /var/lib/apt/lists/*

# Copier l'application
COPY aiservice /app/  
COPY model.onnx /app/

WORKDIR /app  
RUN chmod +x aiservice

EXPOSE 8080

CMD ["./aiservice"]
```

## Conclusion

Le déploiement de modèles IA avec FreePascal/Lazarus offre plusieurs approches, chacune adaptée à des besoins spécifiques. L'interopérabilité avec Python reste la solution la plus flexible pour les modèles complexes, tandis que l'utilisation d'ONNX Runtime offre d'excellentes performances. Le déploiement via API REST permet une intégration facile dans des architectures modernes, et la conteneurisation facilite le déploiement multi-plateforme.

La clé du succès réside dans le choix de l'approche adaptée à vos contraintes de performance, de maintenance et de portabilité.

⏭️ [Traitement de Données et Calcul Scientifique](/16-traitement-donnees-calcul-scientifique/README.md)
