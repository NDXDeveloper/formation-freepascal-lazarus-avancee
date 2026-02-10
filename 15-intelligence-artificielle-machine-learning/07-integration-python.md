🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 15.7 Intégration avec Python

## Introduction

L'intégration de Python avec FreePascal représente une approche pragmatique et puissante pour le développement d'applications d'Intelligence Artificielle. Cette combinaison vous permet de bénéficier simultanément des forces de chaque langage : l'écosystème IA riche de Python et les performances natives de FreePascal.

### Pourquoi intégrer Python et FreePascal ?

Au lieu de choisir entre Python et FreePascal, pourquoi ne pas utiliser les deux ensemble ?

```
┌─────────────────────────────────────────┐
│   Application FreePascal/Lazarus        │
│   ┌─────────────────────────────────┐   │
│   │  Interface utilisateur (LCL)    │   │
│   │  Logique métier (Pascal)        │   │
│   │  Performance critique           │   │
│   └──────────────┬──────────────────┘   │
│                  │                      │
│                  ↓                      │
│   ┌─────────────────────────────────┐   │
│   │  Moteur Python intégré          │   │
│   │  • TensorFlow / PyTorch         │   │
│   │  • scikit-learn                 │   │
│   │  • OpenCV, NLTK, etc.           │   │
│   └─────────────────────────────────┘   │
└─────────────────────────────────────────┘
```

#### Avantages de cette approche hybride

**1. Le meilleur des deux mondes**

| Aspect | FreePascal | Python |
|--------|------------|--------|
| **Interface graphique** | ✅ Lazarus (natif, rapide) | ❌ Tkinter/Qt (plus lent) |
| **Performance** | ✅ Code natif compilé | ❌ Interprété |
| **Écosystème IA** | ❌ Limité | ✅ Très riche |
| **Déploiement** | ✅ Exécutable autonome | ❌ Dépendances complexes |
| **Développement IA** | ❌ Moins d'outils | ✅ Excellents outils |

**2. Réutilisation de code existant**

Si vous avez déjà :
- Des applications Lazarus fonctionnelles
- Des modèles IA entraînés en Python
- Des scripts Python de traitement de données

Vous pouvez les combiner sans tout réécrire !

**3. Flexibilité architecturale**

```pascal
// Approche flexible selon les besoins
case TaskType of
  ttPerformanceCritical:
    Result := PascalOptimizedFunction(Data);
  ttMachineLearning:
    Result := PythonMLModel.Predict(Data);
  ttDataProcessing:
    Result := PythonPandasScript.Process(Data);
end;
```

**4. Évolutivité**

Commencez avec du Python pur, puis optimisez progressivement :

```
Phase 1 : Prototype Python pur (rapide à développer)
    ↓
Phase 2 : Interface Lazarus + Backend Python
    ↓
Phase 3 : Parties critiques réécrites en Pascal
    ↓
Phase 4 : Application optimale mixte
```

## Scénarios d'utilisation

### Scénario 1 : Application desktop avec IA

**Objectif** : Créer une application de reconnaissance d'images avec interface graphique native.

```pascal
program ImageRecognitionApp;

uses
  Forms, PythonEngine, PythonGUIInputOutput;

type
  TMainForm = class(TForm)
  private
    PythonEngine: TPythonEngine;
    MLModel: Variant;
  public
    procedure LoadAIModel;
    procedure ClassifyImage(ImagePath: string);
  end;

procedure TMainForm.LoadAIModel;  
begin
  // Charger un modèle TensorFlow pré-entraîné
  PythonEngine.ExecString('import tensorflow as tf');
  PythonEngine.ExecString('model = tf.keras.models.load_model("model.h5")');
  MLModel := PythonEngine.EvalString('model');
end;

procedure TMainForm.ClassifyImage(ImagePath: string);  
var
  Prediction: string;
begin
  // Interface Pascal rapide
  // + Puissance de TensorFlow
  PythonEngine.ExecString(
    Format('result = predict_image("%s")', [ImagePath])
  );
  Prediction := PythonEngine.EvalString('result');

  LabelResult.Caption := 'Classe détectée : ' + Prediction;
end;
```

**Avantages** :
- Interface native Windows/Ubuntu (LCL)
- Utilisation directe des modèles TensorFlow
- Déploiement d'un seul exécutable

### Scénario 2 : Analyse de données avec interface visuelle

**Objectif** : Créer un outil d'analyse de données avec visualisations.

```pascal
// L'utilisateur charge un CSV via l'interface Lazarus
procedure TMainForm.ButtonAnalyzeClick(Sender: TObject);  
var
  CSVPath: string;
  AnalysisResults: string;
begin
  CSVPath := OpenDialog.FileName;

  // Python fait l'analyse avec pandas
  PythonEngine.ExecString('import pandas as pd');
  PythonEngine.ExecString('import matplotlib.pyplot as plt');
  PythonEngine.ExecString(
    Format('df = pd.read_csv("%s")', [CSVPath])
  );
  PythonEngine.ExecString('summary = df.describe().to_json()');

  AnalysisResults := PythonEngine.EvalString('summary');

  // Affichage dans l'interface Pascal
  MemoResults.Text := FormatJSON(AnalysisResults);
end;
```

### Scénario 3 : Application métier avec prédictions IA

**Objectif** : Système de prédiction de maintenance dans une application de gestion.

```pascal
type
  TMaintenancePredictor = class
  private
    FPythonEngine: TPythonEngine;
    FModel: Variant;
  public
    constructor Create;
    function PredictFailure(SensorData: TSensorReadings): TFailurePrediction;
  end;

function TMaintenancePredictor.PredictFailure(
  SensorData: TSensorReadings): TFailurePrediction;
var
  InputArray: string;
  ProbabilityStr: string;
  Probability: Double;
begin
  // Convertir les données Pascal en format Python
  InputArray := FormatSensorDataAsArray(SensorData);

  // Prédiction via modèle scikit-learn
  FPythonEngine.ExecString(
    Format('prediction_proba = model.predict_proba([%s])[0][1]',
    [InputArray])
  );

  ProbabilityStr := FPythonEngine.EvalString('prediction_proba');
  Probability := StrToFloat(ProbabilityStr);

  Result.FailureProbability := Probability;
  Result.RecommendedAction := DetermineAction(Probability);
  Result.PredictionDate := Now;
end;
```

### Scénario 4 : Traitement batch avec progression

**Objectif** : Traiter de nombreux fichiers avec feedback visuel.

```pascal
procedure TMainForm.ProcessImagesWithProgress;  
var
  i: Integer;
  ImageFiles: TStringList;
  Result: string;
begin
  ImageFiles := GetImageFilesList;
  ProgressBar.Max := ImageFiles.Count;

  for i := 0 to ImageFiles.Count - 1 do
  begin
    // Traitement Python pour chaque image
    Result := ProcessImageWithPython(ImageFiles[i]);

    // Mise à jour de l'interface Pascal
    ProgressBar.Position := i + 1;
    LabelStatus.Caption := Format('Traité %d/%d', [i + 1, ImageFiles.Count]);
    Application.ProcessMessages; // Garder l'interface réactive
  end;

  ShowMessage('Traitement terminé !');
end;
```

## Concepts fondamentaux de l'intégration

### 1. Qu'est-ce que Python4Lazarus ?

**Python4Lazarus** (aussi appelé **Python4Delphi** dans sa version originale) est un ensemble de composants qui permettent d'intégrer l'interpréteur Python directement dans votre application FreePascal/Lazarus.

```
Application Lazarus
    ↓
┌──────────────────────────────┐
│  TPythonEngine               │ ← Charge l'interpréteur Python
├──────────────────────────────┤
│  TPythonModule               │ ← Expose des fonctions Pascal à Python
├──────────────────────────────┤
│  TPythonGUIInputOutput       │ ← Capture les sorties Python
└──────────────────────────────┘
    ↓
Interpréteur Python embarqué
```

### 2. Les composants principaux

#### TPythonEngine

Le moteur Python - le cœur de l'intégration :

```pascal
var
  PythonEngine: TPythonEngine;

procedure TForm1.FormCreate(Sender: TObject);  
begin
  PythonEngine := TPythonEngine.Create(Self);
  PythonEngine.DllPath := 'C:\Python39';  // Windows
  // ou '/usr/lib/python3.9' sur Ubuntu
  PythonEngine.LoadDll;

  if PythonEngine.Initialized then
    ShowMessage('Python initialisé !')
  else
    ShowMessage('Erreur d''initialisation Python');
end;
```

#### TPythonModule

Permet d'exposer des fonctions Pascal à Python :

```pascal
// Python peut maintenant appeler des fonctions Pascal !
procedure TForm1.PythonModuleInitialization(Sender: TObject);  
begin
  with Sender as TPythonModule do
  begin
    AddMethod('pascal_hello', @HelloFromPascal,
              'pascal_hello() -> str');
    AddMethod('pascal_multiply', @MultiplyNumbers,
              'pascal_multiply(a, b) -> float');
  end;
end;

function HelloFromPascal(Self, Args: PPyObject): PPyObject; cdecl;  
begin
  Result := PythonEngine.PyString_FromString('Hello from Pascal!');
end;
```

#### TPythonGUIInputOutput

Capture les sorties Python (print, erreurs) :

```pascal
var
  PythonIO: TPythonGUIInputOutput;

procedure SetupPythonOutput;  
begin
  PythonIO := TPythonGUIInputOutput.Create(Self);
  PythonIO.OnSendData := @HandlePythonOutput;
end;

procedure HandlePythonOutput(Sender: TObject; const Data: string);  
begin
  // Afficher les print() Python dans un Memo
  MemoConsole.Lines.Add(Data);
end;
```

### 3. Modes d'interaction

Il existe plusieurs façons d'interagir avec Python depuis Pascal :

#### Mode A : Exécution de scripts

```pascal
// Exécuter du code Python directement
PythonEngine.ExecString('print("Hello from Python!")');  
PythonEngine.ExecString('x = 42');  
PythonEngine.ExecString('y = x * 2');
```

#### Mode B : Évaluation d'expressions

```pascal
// Obtenir une valeur de retour
var
  Result: string;
begin
  PythonEngine.ExecString('result = 10 + 20');
  Result := PythonEngine.EvalString('result');
  ShowMessage('Résultat : ' + Result); // "30"
end;
```

#### Mode C : Exécution de fichiers

```pascal
// Exécuter un script Python complet
PythonEngine.ExecFile('my_script.py');

// ou
var
  Script: TStringList;
begin
  Script := TStringList.Create;
  try
    Script.LoadFromFile('analysis.py');
    PythonEngine.ExecStrings(Script);
  finally
    Script.Free;
  end;
end;
```

#### Mode D : Appel de fonctions Python

```pascal
// Définir une fonction en Python
PythonEngine.ExecString(
  'def analyze_data(data):' + sLineBreak +
  '    return sum(data) / len(data)'
);

// L'appeler depuis Pascal
var
  PyFunction: Variant;
  Result: Double;
begin
  PyFunction := PythonEngine.EvalString('analyze_data');
  Result := PyFunction([1, 2, 3, 4, 5]); // Moyenne = 3.0
end;
```

### 4. Échange de données entre Pascal et Python

#### Types de données simples

```pascal
// Pascal → Python
PythonEngine.ExecString(Format('number = %d', [42]));  
PythonEngine.ExecString(Format('text = "%s"', ['Hello']));  
PythonEngine.ExecString(Format('price = %.2f', [19.99]));

// Python → Pascal
var
  IntResult: Integer;
  StrResult: string;
  FloatResult: Double;
begin
  PythonEngine.ExecString('py_int = 100');
  IntResult := StrToInt(PythonEngine.EvalString('py_int'));

  PythonEngine.ExecString('py_str = "Python Text"');
  StrResult := PythonEngine.EvalString('py_str');

  PythonEngine.ExecString('py_float = 3.14159');
  FloatResult := StrToFloat(PythonEngine.EvalString('py_float'));
end;
```

#### Listes et tableaux

```pascal
// Envoyer un tableau Pascal vers Python
var
  Data: array of Integer;
  DataStr: string;
  i: Integer;
begin
  SetLength(Data, 5);
  for i := 0 to 4 do
    Data[i] := i * 10;

  // Convertir en chaîne Python
  DataStr := '[';
  for i := 0 to High(Data) do
  begin
    DataStr := DataStr + IntToStr(Data[i]);
    if i < High(Data) then
      DataStr := DataStr + ', ';
  end;
  DataStr := DataStr + ']';

  // Utiliser en Python
  PythonEngine.ExecString(Format('py_list = %s', [DataStr]));
  PythonEngine.ExecString('py_sum = sum(py_list)');
end;
```

#### Structures complexes (JSON)

```pascal
uses
  fpjson, jsonparser;

// Pascal → Python via JSON
procedure SendComplexData;  
var
  JSONObj: TJSONObject;
  JSONStr: string;
begin
  JSONObj := TJSONObject.Create;
  try
    JSONObj.Add('name', 'John Doe');
    JSONObj.Add('age', 30);
    JSONObj.Add('city', 'Paris');

    JSONStr := JSONObj.AsJSON;

    PythonEngine.ExecString('import json');
    PythonEngine.ExecString(
      Format('data = json.loads(''%s'')', [JSONStr])
    );
    PythonEngine.ExecString('print(data["name"])'); // John Doe
  finally
    JSONObj.Free;
  end;
end;

// Python → Pascal via JSON
procedure ReceiveComplexData;  
var
  JSONStr: string;
  JSONData: TJSONData;
begin
  PythonEngine.ExecString('import json');
  PythonEngine.ExecString(
    'result = json.dumps({"status": "ok", "value": 42})'
  );

  JSONStr := PythonEngine.EvalString('result');
  JSONData := GetJSON(JSONStr);
  try
    ShowMessage('Status: ' + JSONData.FindPath('status').AsString);
    ShowMessage('Value: ' + IntToStr(JSONData.FindPath('value').AsInteger));
  finally
    JSONData.Free;
  end;
end;
```

### 5. Gestion des erreurs

La gestion des erreurs est cruciale pour une intégration robuste :

```pascal
procedure SafePythonExecution;  
begin
  try
    PythonEngine.ExecString('result = 10 / 0');  // Division par zéro
  except
    on E: Exception do
    begin
      // Capturer les erreurs Python
      ShowMessage('Erreur Python : ' + E.Message);

      // Obtenir le traceback complet
      if PythonEngine.PyErr_Occurred <> nil then
      begin
        PythonEngine.PyErr_Print;
        MemoErrors.Lines.Add('Erreur détectée en Python');
      end;
    end;
  end;
end;

// Vérification explicite
procedure CheckPythonError;  
begin
  PythonEngine.ExecString('import sys');
  PythonEngine.ExecString('result = undefined_variable');

  if PythonEngine.PyErr_Occurred <> nil then
  begin
    ShowMessage('Une erreur s''est produite !');
    PythonEngine.PyErr_Clear; // Effacer l'erreur
  end;
end;
```

## Architecture d'une application hybride

### Modèle de séparation des responsabilités

```
┌─────────────────────────────────────────────────────┐
│                 Couche Présentation                 │
│              (FreePascal/Lazarus LCL)               │
│  • Formulaires, contrôles                           │
│  • Événements utilisateur                           │
│  • Affichage des résultats                          │
└───────────────────┬─────────────────────────────────┘
                    │
┌───────────────────▼─────────────────────────────────┐
│              Couche Logique Métier                  │
│                  (FreePascal)                       │
│  • Validation des données                           │
│  • Règles métier                                    │
│  • Orchestration                                    │
└───────────┬───────────────────────┬─────────────────┘
            │                       │
            │                       │
┌───────────▼─────────────┐ ┌───────▼─────────────────┐
│   Traitement Pascal     │ │   Traitement Python     │
│   • Performance         │ │   • Machine Learning    │
│   • Algorithmes natifs  │ │   • Data Science        │
│   • Accès fichiers      │ │   • Bibliothèques IA    │
└─────────────────────────┘ └─────────────────────────┘
```

### Exemple d'architecture complète

```pascal
unit AIService;

interface

type
  // Service IA avec Python intégré
  TAIService = class
  private
    FPythonEngine: TPythonEngine;
    FModelLoaded: Boolean;
    procedure InitializePython;
    procedure LoadMLModel;
  public
    constructor Create;
    destructor Destroy; override;

    // Interface publique (utilisée par l'UI)
    function PredictCategory(const Text: string): string;
    function AnalyzeSentiment(const Text: string): Double;
    function GenerateRecommendations(UserID: Integer): TStringList;
  end;

implementation

constructor TAIService.Create;  
begin
  inherited Create;
  InitializePython;
  LoadMLModel;
end;

procedure TAIService.InitializePython;  
begin
  FPythonEngine := TPythonEngine.Create(nil);
  FPythonEngine.LoadDll;

  // Importer les bibliothèques nécessaires
  FPythonEngine.ExecString('import numpy as np');
  FPythonEngine.ExecString('import tensorflow as tf');
  FPythonEngine.ExecString('from transformers import pipeline');
end;

procedure TAIService.LoadMLModel;  
begin
  FPythonEngine.ExecString(
    'sentiment_analyzer = pipeline("sentiment-analysis")'
  );
  FModelLoaded := True;
end;

function TAIService.AnalyzeSentiment(const Text: string): Double;  
var
  ScoreStr: string;
begin
  if not FModelLoaded then
    raise Exception.Create('Modèle non chargé');

  FPythonEngine.ExecString(
    Format('result = sentiment_analyzer("%s")[0]', [Text])
  );
  FPythonEngine.ExecString('score = result["score"]');

  ScoreStr := FPythonEngine.EvalString('score');
  Result := StrToFloat(ScoreStr);
end;

end.
```

## Cas d'usage pratiques

### Cas 1 : Application de traduction

```pascal
type
  TTranslationApp = class(TForm)
  private
    FPython: TPythonEngine;
    procedure InitializeTranslator;
  public
    function Translate(const Text, FromLang, ToLang: string): string;
  end;

procedure TTranslationApp.InitializeTranslator;  
begin
  FPython.ExecString('from googletrans import Translator');
  FPython.ExecString('translator = Translator()');
end;

function TTranslationApp.Translate(
  const Text, FromLang, ToLang: string): string;
begin
  FPython.ExecString(
    Format('translation = translator.translate("%s", src="%s", dest="%s")',
    [Text, FromLang, ToLang])
  );
  FPython.ExecString('result_text = translation.text');
  Result := FPython.EvalString('result_text');
end;
```

### Cas 2 : Analyse de données CSV avec Pandas

```pascal
procedure TDataAnalyzer.AnalyzeCSV(const FilePath: string);  
begin
  PythonEngine.ExecString('import pandas as pd');
  PythonEngine.ExecString(
    Format('df = pd.read_csv("%s")', [FilePath])
  );

  // Statistiques descriptives
  PythonEngine.ExecString('stats = df.describe()');
  PythonEngine.ExecString('stats_json = stats.to_json()');

  StatsJSON := PythonEngine.EvalString('stats_json');
  DisplayStats(StatsJSON);

  // Créer un graphique
  PythonEngine.ExecString('import matplotlib.pyplot as plt');
  PythonEngine.ExecString('df.plot()');
  PythonEngine.ExecString('plt.savefig("chart.png")');

  ImageChart.Picture.LoadFromFile('chart.png');
end;
```

### Cas 3 : Détection d'objets en temps réel

```pascal
procedure TCameraApp.ProcessVideoFrame;  
var
  Frame: TBitmap;
  DetectedObjects: string;
begin
  Frame := CaptureFrame;
  SaveFrameToTempFile(Frame, 'temp_frame.jpg');

  // YOLO ou autre modèle de détection
  PythonEngine.ExecString(
    'objects = detector.detect("temp_frame.jpg")'
  );
  PythonEngine.ExecString('objects_json = json.dumps(objects)');

  DetectedObjects := PythonEngine.EvalString('objects_json');
  DrawBoundingBoxes(Frame, DetectedObjects);

  ImageDisplay.Picture.Assign(Frame);
end;
```

## Avantages et limitations

### Avantages

✅ **Accès immédiat à l'écosystème Python**
- TensorFlow, PyTorch, scikit-learn
- Pandas, NumPy, Matplotlib
- Transformers, OpenCV, NLTK

✅ **Développement rapide**
- Prototypage en Python (rapide)
- Interface en Lazarus (native)
- Meilleur des deux mondes

✅ **Réutilisation du code**
- Scripts Python existants
- Modèles ML pré-entraînés
- Applications Lazarus existantes

✅ **Flexibilité**
- Choisir le bon outil pour chaque tâche
- Migration progressive
- Tests facilités

### Limitations

⚠️ **Dépendance à Python**
- Python doit être installé sur le système cible
- Gestion des versions Python
- Taille de déploiement augmentée

⚠️ **Performance de l'interopérabilité**
- Coût de communication Pascal ↔ Python
- Sérialisation/désérialisation
- Pas idéal pour boucles serrées

⚠️ **Complexité**
- Deux environnements à gérer
- Debugging plus complexe
- Gestion d'erreurs dans deux langages

⚠️ **Portabilité**
- Chemins Python différents (Windows/Ubuntu)
- Versions de bibliothèques
- Dépendances système

### Quand utiliser l'intégration Python ?

**OUI, si :**
- Vous avez besoin de bibliothèques IA Python spécifiques
- Vous voulez prototyper rapidement
- Vous avez déjà du code Python à réutiliser
- L'interface utilisateur native est importante

**NON, si :**
- Performance maximale requise partout
- Déploiement ultra-léger nécessaire
- Pas de dépendances externes acceptables
- Application critique temps-réel

### Alternatives à considérer

1. **ONNX Runtime** : Modèles portables sans Python
2. **TensorFlow C API** : Bindings directs (chapitre 15.1)
3. **Implémentation native** : Réseaux de neurones en Pascal (chapitre 15.2)
4. **Services externes** : API REST avec Python backend séparé

## Structure des prochaines sections

Ce chapitre 15.7 couvrira en détail :

- **15.7.1** Python4Lazarus sur Windows
  - Installation et configuration
  - Gestion des versions Python
  - Cas particuliers Windows

- **15.7.2** Python4Lazarus sur Ubuntu
  - Installation via apt/pip
  - Configuration des chemins
  - Spécificités Linux

Ces sections fourniront des guides pas-à-pas complets pour mettre en place Python4Lazarus sur chaque plateforme, avec des exemples concrets et des solutions aux problèmes courants.

## Conclusion

L'intégration de Python avec FreePascal via Python4Lazarus ouvre des possibilités extraordinaires pour le développement d'applications d'Intelligence Artificielle. Cette approche hybride permet de :

🎯 Créer des **interfaces natives performantes** avec Lazarus  
🎯 Exploiter le **riche écosystème IA** de Python  
🎯 **Déployer facilement** sur Windows et Ubuntu  
🎯 **Réutiliser** code et modèles existants

Bien que cette approche introduise une certaine complexité, elle offre une flexibilité inégalée et permet d'accélérer considérablement le développement d'applications IA professionnelles avec FreePascal.

Dans les prochaines sections, nous allons explorer l'installation et la configuration détaillée de Python4Lazarus sur Windows et Ubuntu, avec de nombreux exemples pratiques et des solutions aux problèmes courants.

---

**Prérequis pour continuer :**
- FreePascal et Lazarus installés
- Connaissances de base en Python
- Compréhension des concepts IA (chapitre 15 introduction)

**Prêt à installer Python4Lazarus ?** Direction les sections 15.7.1 et 15.7.2 !

⏭️ [Python4Lazarus sur Windows](/15-intelligence-artificielle-machine-learning/07.1-python4lazarus-windows.md)
