🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 16.9 Intégration avec R et Python

## Introduction

FreePascal est excellent pour créer des applications performantes et des interfaces utilisateur, mais parfois vous avez besoin des bibliothèques spécialisées de **Python** (machine learning, data science) ou **R** (statistiques avancées). La bonne nouvelle ? Vous pouvez combiner le meilleur des trois mondes !

### Pourquoi intégrer Python ou R ?

**Python** excelle dans :
- Machine Learning (TensorFlow, PyTorch, scikit-learn)
- Traitement de données (Pandas, NumPy)
- Vision par ordinateur (OpenCV)
- Web scraping (BeautifulSoup)

**R** excelle dans :
- Statistiques avancées
- Analyses biostatistiques
- Visualisations scientifiques (ggplot2)
- Modélisation prédictive

**FreePascal** excelle dans :
- Applications natives rapides
- Interfaces utilisateur professionnelles
- Performance et contrôle bas niveau
- Déploiement sans dépendances

**L'intégration = le meilleur de chaque langage !**

---

## Partie 1 : Intégration avec Python

### 1.1 Les différentes approches

Il existe plusieurs façons d'intégrer Python dans vos applications FreePascal :

| Méthode | Avantages | Inconvénients |
|---------|-----------|---------------|
| **Python4Lazarus** | Intégration native, appels directs | Nécessite Python installé |
| **Ligne de commande** | Simple, portable | Moins performant |
| **Serveur REST** | Découplé, scalable | Plus complexe à mettre en place |
| **Fichiers intermédiaires** | Très simple | Lent pour grands volumes |

### 1.2 Méthode 1 : Python4Lazarus (Recommandé)

**Python4Lazarus** (P4L) est une bibliothèque qui permet d'appeler Python directement depuis FreePascal.

#### Installation sur Windows

1. **Installer Python** (si pas déjà fait)
   - Télécharger depuis python.org
   - Cocher "Add Python to PATH" lors de l'installation
   - Version recommandée : Python 3.9 ou 3.10

2. **Installer Python4Lazarus dans Lazarus**
   - Ouvrir Lazarus
   - Menu : Paquets → Installer/Désinstaller des paquets
   - Chercher "python4lazarus" dans la liste
   - Ajouter à droite et reconstruire l'IDE

#### Installation sur Ubuntu

```bash
# Installer Python et les dépendances
sudo apt update  
sudo apt install python3 python3-dev python3-pip

# Installer Python4Lazarus via OPM (Online Package Manager) dans Lazarus
# Ou télécharger depuis GitHub
```

#### Premier programme avec Python4Lazarus

```pascal
program FirstPython4Lazarus;

uses
  SysUtils, PythonEngine;

var
  PythonEngine: TPythonEngine;
  Result: PPyObject;

begin
  // Création du moteur Python
  PythonEngine := TPythonEngine.Create(nil);
  try
    // Initialisation
    PythonEngine.LoadDll;

    // Exécution de code Python simple
    PythonEngine.ExecString('print("Hello from Python!")');

    // Calcul et récupération du résultat
    Result := PythonEngine.EvalString('2 + 2');
    WriteLn('Résultat Python : ', PythonEngine.PyObjectAsString(Result));

  finally
    PythonEngine.Free;
  end;

  ReadLn;
end.
```

#### Appeler une fonction Python

```pascal
program CallPythonFunction;

uses
  SysUtils, PythonEngine;

var
  PythonEngine: TPythonEngine;
  Result: PPyObject;
  Value: Double;

begin
  PythonEngine := TPythonEngine.Create(nil);
  try
    PythonEngine.LoadDll;

    // Définir une fonction Python
    PythonEngine.ExecString(
      'import math' + #13#10 +
      'def calculate_stats(numbers):' + #13#10 +
      '    return {' + #13#10 +
      '        "mean": sum(numbers) / len(numbers),' + #13#10 +
      '        "max": max(numbers),' + #13#10 +
      '        "min": min(numbers)' + #13#10 +
      '    }'
    );

    // Appeler la fonction avec des données
    Result := PythonEngine.EvalString('calculate_stats([10, 20, 30, 40, 50])');

    // Récupérer les résultats
    WriteLn('Résultats statistiques :');
    WriteLn(PythonEngine.PyObjectAsString(Result));

  finally
    PythonEngine.Free;
  end;

  ReadLn;
end.
```

### 1.3 Utiliser des bibliothèques Python (NumPy, Pandas)

Exemple complet avec NumPy pour le calcul scientifique :

```pascal
program UsePythonLibraries;

uses
  SysUtils, Classes, PythonEngine, PythonGUIInputOutput;

type
  TDataAnalyzer = class
  private
    FPythonEngine: TPythonEngine;
    FPythonIO: TPythonGUIInputOutput;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AnalyzeData(const Data: array of Double);
    function PredictWithML(const Features: array of Double): Double;
  end;

constructor TDataAnalyzer.Create;  
begin
  FPythonEngine := TPythonEngine.Create(nil);
  FPythonEngine.LoadDll;

  FPythonIO := TPythonGUIInputOutput.Create(nil);
  FPythonIO.PythonEngine := FPythonEngine;
end;

destructor TDataAnalyzer.Destroy;  
begin
  FPythonIO.Free;
  FPythonEngine.Free;
  inherited;
end;

procedure TDataAnalyzer.AnalyzeData(const Data: array of Double);  
var
  DataStr: string;
  i: Integer;
  PythonCode: TStringList;
begin
  // Convertir les données en format Python
  DataStr := '[';
  for i := Low(Data) to High(Data) do
  begin
    DataStr := DataStr + FloatToStr(Data[i]);
    if i < High(Data) then
      DataStr := DataStr + ', ';
  end;
  DataStr := DataStr + ']';

  // Créer le script Python
  PythonCode := TStringList.Create;
  try
    PythonCode.Add('import numpy as np');
    PythonCode.Add('import pandas as pd');
    PythonCode.Add('');
    PythonCode.Add('# Données reçues de FreePascal');
    PythonCode.Add('data = np.' + DataStr);
    PythonCode.Add('');
    PythonCode.Add('# Calculs statistiques');
    PythonCode.Add('print("Moyenne :", np.mean(data))');
    PythonCode.Add('print("Écart-type :", np.std(data))');
    PythonCode.Add('print("Médiane :", np.median(data))');
    PythonCode.Add('print("Percentile 25% :", np.percentile(data, 25))');
    PythonCode.Add('print("Percentile 75% :", np.percentile(data, 75))');

    // Exécuter
    FPythonEngine.ExecStrings(PythonCode);

  finally
    PythonCode.Free;
  end;
end;

function TDataAnalyzer.PredictWithML(const Features: array of Double): Double;  
var
  PythonCode: TStringList;
  ResultObj: PPyObject;
  i: Integer;
  FeaturesStr: string;
begin
  Result := 0;

  // Convertir les features en string Python
  FeaturesStr := '[';
  for i := Low(Features) to High(Features) do
  begin
    FeaturesStr := FeaturesStr + FloatToStr(Features[i]);
    if i < High(Features) then
      FeaturesStr := FeaturesStr + ', ';
  end;
  FeaturesStr := FeaturesStr + ']';

  PythonCode := TStringList.Create;
  try
    PythonCode.Add('from sklearn.linear_model import LinearRegression');
    PythonCode.Add('import numpy as np');
    PythonCode.Add('');
    PythonCode.Add('# Données d''entraînement (exemple simplifié)');
    PythonCode.Add('X_train = np.array([[1], [2], [3], [4], [5]])');
    PythonCode.Add('y_train = np.array([2, 4, 6, 8, 10])');
    PythonCode.Add('');
    PythonCode.Add('# Entraîner le modèle');
    PythonCode.Add('model = LinearRegression()');
    PythonCode.Add('model.fit(X_train, y_train)');
    PythonCode.Add('');
    PythonCode.Add('# Prédiction avec les données de FreePascal');
    PythonCode.Add('features = np.array(' + FeaturesStr + ').reshape(-1, 1)');
    PythonCode.Add('prediction = model.predict(features)[0]');

    FPythonEngine.ExecStrings(PythonCode);

    // Récupérer la prédiction
    ResultObj := FPythonEngine.EvalString('prediction');
    Result := FPythonEngine.PyObjectAsFloat(ResultObj);

  finally
    PythonCode.Free;
  end;
end;

// Programme principal
var
  Analyzer: TDataAnalyzer;
  TestData: array[0..9] of Double = (1.5, 2.3, 3.1, 4.7, 5.2, 6.8, 7.1, 8.9, 9.3, 10.5);
  Prediction: Double;

begin
  Analyzer := TDataAnalyzer.Create;
  try
    WriteLn('=== Analyse de données avec NumPy ===');
    WriteLn;

    Analyzer.AnalyzeData(TestData);

    WriteLn;
    WriteLn('=== Prédiction avec Machine Learning ===');
    Prediction := Analyzer.PredictWithML([6.5]);
    WriteLn('Prédiction pour x=6.5 : ', Prediction:0:2);

  finally
    Analyzer.Free;
  end;

  ReadLn;
end.
```

### 1.4 Méthode 2 : Ligne de commande (Simple et portable)

Cette méthode est plus simple mais moins performante pour de gros volumes :

```pascal
program PythonCommandLine;

uses
  SysUtils, Process, Classes;

function RunPythonScript(const ScriptPath: string; const Args: array of string): string;  
var
  Process: TProcess;
  OutputLines: TStringList;
  i: Integer;
begin
  Result := '';
  Process := TProcess.Create(nil);
  OutputLines := TStringList.Create;
  try
    // Configuration du processus
    {$IFDEF WINDOWS}
    Process.Executable := 'python';  // ou 'python.exe'
    {$ENDIF}
    {$IFDEF LINUX}
    Process.Executable := 'python3';
    {$ENDIF}

    Process.Parameters.Add(ScriptPath);

    // Ajouter les arguments
    for i := Low(Args) to High(Args) do
      Process.Parameters.Add(Args[i]);

    // Options pour capturer la sortie
    Process.Options := [poWaitOnExit, poUsePipes];

    // Exécuter
    Process.Execute;

    // Lire la sortie
    OutputLines.LoadFromStream(Process.Output);
    Result := OutputLines.Text;

  finally
    OutputLines.Free;
    Process.Free;
  end;
end;

// Créer un script Python temporaire
procedure CreatePythonScript(const FileName: string);  
var
  Script: TStringList;
begin
  Script := TStringList.Create;
  try
    Script.Add('import sys');
    Script.Add('import json');
    Script.Add('');
    Script.Add('# Récupérer les arguments');
    Script.Add('data = json.loads(sys.argv[1])');
    Script.Add('');
    Script.Add('# Traitement');
    Script.Add('result = {');
    Script.Add('    "sum": sum(data),');
    Script.Add('    "mean": sum(data) / len(data),');
    Script.Add('    "count": len(data)');
    Script.Add('}');
    Script.Add('');
    Script.Add('# Retourner en JSON');
    Script.Add('print(json.dumps(result))');

    Script.SaveToFile(FileName);
  finally
    Script.Free;
  end;
end;

var
  ScriptPath: string;
  JSONData: string;
  Output: string;

begin
  // Créer le script
  ScriptPath := 'temp_script.py';
  CreatePythonScript(ScriptPath);

  // Données à envoyer (format JSON)
  JSONData := '[10, 20, 30, 40, 50]';

  // Exécuter le script Python
  WriteLn('Exécution du script Python...');
  Output := RunPythonScript(ScriptPath, [JSONData]);

  WriteLn('Résultat :');
  WriteLn(Output);

  // Nettoyer
  DeleteFile(ScriptPath);

  ReadLn;
end.
```

### 1.5 Gestion des environnements virtuels Python

Pour éviter les conflits de bibliothèques, utilisez des environnements virtuels :

```pascal
procedure SetupPythonEnvironment;  
var
  Process: TProcess;
begin
  Process := TProcess.Create(nil);
  try
    {$IFDEF WINDOWS}
    // Créer un environnement virtuel sur Windows
    Process.Executable := 'python';
    Process.Parameters.Add('-m');
    Process.Parameters.Add('venv');
    Process.Parameters.Add('venv_pascal');
    Process.Execute;

    // Installer les dépendances
    Process.Executable := 'venv_pascal\Scripts\pip.exe';
    {$ENDIF}

    {$IFDEF LINUX}
    // Créer un environnement virtuel sur Ubuntu
    Process.Executable := 'python3';
    Process.Parameters.Add('-m');
    Process.Parameters.Add('venv');
    Process.Parameters.Add('venv_pascal');
    Process.Execute;

    // Installer les dépendances
    Process.Executable := 'venv_pascal/bin/pip';
    {$ENDIF}

    Process.Parameters.Clear;
    Process.Parameters.Add('install');
    Process.Parameters.Add('numpy');
    Process.Parameters.Add('pandas');
    Process.Parameters.Add('scikit-learn');
    Process.Execute;

  finally
    Process.Free;
  end;
end;
```

---

## Partie 2 : Intégration avec R

### 2.1 Pourquoi utiliser R ?

R est LE langage de référence pour :
- Analyses statistiques avancées
- Bioinformatique et biostatistiques
- Visualisations de données scientifiques
- Modèles prédictifs et séries temporelles

### 2.2 Méthode 1 : Ligne de commande (Rscript)

C'est la méthode la plus simple et la plus portable :

```pascal
program RIntegration;

uses
  SysUtils, Process, Classes;

function RunRScript(const ScriptPath: string): string;  
var
  Process: TProcess;
  OutputLines: TStringList;
begin
  Result := '';
  Process := TProcess.Create(nil);
  OutputLines := TStringList.Create;
  try
    {$IFDEF WINDOWS}
    // Windows : Rscript.exe doit être dans le PATH
    Process.Executable := 'Rscript';
    {$ENDIF}
    {$IFDEF LINUX}
    // Ubuntu : généralement /usr/bin/Rscript
    Process.Executable := 'Rscript';
    {$ENDIF}

    Process.Parameters.Add(ScriptPath);
    Process.Options := [poWaitOnExit, poUsePipes];

    Process.Execute;

    OutputLines.LoadFromStream(Process.Output);
    Result := OutputLines.Text;

  finally
    OutputLines.Free;
    Process.Free;
  end;
end;

procedure CreateStatisticalAnalysisScript(const FileName: string);  
var
  Script: TStringList;
begin
  Script := TStringList.Create;
  try
    Script.Add('# Script R appelé depuis FreePascal');
    Script.Add('');
    Script.Add('# Lire les données depuis un fichier CSV');
    Script.Add('data <- read.csv("data.csv")');
    Script.Add('');
    Script.Add('# Analyses statistiques');
    Script.Add('cat("=== STATISTIQUES DESCRIPTIVES ===\n")');
    Script.Add('print(summary(data))');
    Script.Add('');
    Script.Add('cat("\n=== CORRÉLATIONS ===\n")');
    Script.Add('print(cor(data))');
    Script.Add('');
    Script.Add('# Test statistique (t-test)');
    Script.Add('if(ncol(data) >= 2) {');
    Script.Add('  cat("\n=== T-TEST ===\n")');
    Script.Add('  result <- t.test(data[,1], data[,2])');
    Script.Add('  print(result)');
    Script.Add('}');
    Script.Add('');
    Script.Add('# Régression linéaire');
    Script.Add('if(ncol(data) >= 2) {');
    Script.Add('  cat("\n=== RÉGRESSION LINÉAIRE ===\n")');
    Script.Add('  model <- lm(data[,1] ~ data[,2])');
    Script.Add('  print(summary(model))');
    Script.Add('}');

    Script.SaveToFile(FileName);
  finally
    Script.Free;
  end;
end;

procedure CreateSampleData;  
var
  Data: TStringList;
begin
  Data := TStringList.Create;
  try
    Data.Add('X,Y');
    Data.Add('1.2,2.4');
    Data.Add('2.3,4.5');
    Data.Add('3.1,6.2');
    Data.Add('4.5,9.0');
    Data.Add('5.4,10.8');
    Data.Add('6.2,12.3');
    Data.Add('7.1,14.2');
    Data.Add('8.3,16.5');
    Data.Add('9.2,18.4');
    Data.Add('10.1,20.2');

    Data.SaveToFile('data.csv');
  finally
    Data.Free;
  end;
end;

var
  ScriptPath: string;
  Output: string;

begin
  WriteLn('=== Intégration R avec FreePascal ===');
  WriteLn;

  // Créer les données de test
  WriteLn('Création des données de test...');
  CreateSampleData;

  // Créer le script R
  ScriptPath := 'analysis.R';
  WriteLn('Création du script R...');
  CreateStatisticalAnalysisScript(ScriptPath);

  // Exécuter l'analyse R
  WriteLn('Exécution de l''analyse statistique avec R...');
  WriteLn;

  Output := RunRScript(ScriptPath);
  WriteLn(Output);

  // Nettoyer
  DeleteFile(ScriptPath);
  DeleteFile('data.csv');

  WriteLn;
  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
```

### 2.3 Créer des graphiques avec R

R excelle dans les visualisations. Voici comment générer des graphiques :

```pascal
program RGraphics;

uses
  SysUtils, Process, Classes;

procedure GeneratePlotScript(const DataFile, OutputFile: string);  
var
  Script: TStringList;
begin
  Script := TStringList.Create;
  try
    Script.Add('# Charger les données');
    Script.Add('data <- read.csv("' + DataFile + '")');
    Script.Add('');
    Script.Add('# Ouvrir le périphérique graphique');
    Script.Add('png("' + OutputFile + '", width=800, height=600)');
    Script.Add('');
    Script.Add('# Créer un layout avec plusieurs graphiques');
    Script.Add('par(mfrow=c(2,2))');
    Script.Add('');
    Script.Add('# Graphique 1 : Nuage de points');
    Script.Add('plot(data$X, data$Y, main="Nuage de points", ');
    Script.Add('     xlab="X", ylab="Y", pch=19, col="blue")');
    Script.Add('abline(lm(data$Y ~ data$X), col="red", lwd=2)');
    Script.Add('');
    Script.Add('# Graphique 2 : Histogramme');
    Script.Add('hist(data$Y, main="Distribution de Y", ');
    Script.Add('     xlab="Valeurs", col="lightblue", breaks=10)');
    Script.Add('');
    Script.Add('# Graphique 3 : Boxplot');
    Script.Add('boxplot(data$Y, main="Boxplot de Y", ');
    Script.Add('        ylab="Valeurs", col="lightgreen")');
    Script.Add('');
    Script.Add('# Graphique 4 : Densité');
    Script.Add('plot(density(data$Y), main="Courbe de densité", ');
    Script.Add('     xlab="Valeurs", col="darkgreen", lwd=2)');
    Script.Add('polygon(density(data$Y), col=rgb(0,1,0,0.3))');
    Script.Add('');
    Script.Add('# Fermer le périphérique');
    Script.Add('dev.off()');
    Script.Add('');
    Script.Add('cat("Graphique sauvegardé dans : ' + OutputFile + '\n")');

    Script.SaveToFile('plot_script.R');
  finally
    Script.Free;
  end;
end;

var
  Process: TProcess;

begin
  WriteLn('=== Génération de graphiques avec R ===');
  WriteLn;

  // Créer des données (réutiliser la fonction précédente)
  CreateSampleData;

  // Créer le script de visualisation
  GeneratePlotScript('data.csv', 'output_plot.png');

  // Exécuter R
  WriteLn('Génération des graphiques...');
  Process := TProcess.Create(nil);
  try
    Process.Executable := 'Rscript';
    Process.Parameters.Add('plot_script.R');
    Process.Options := [poWaitOnExit];
    Process.Execute;

    WriteLn('Graphiques générés avec succès !');
    WriteLn('Fichier : output_plot.png');

  finally
    Process.Free;
  end;

  ReadLn;
end.
```

### 2.4 Méthode 2 : Fichiers intermédiaires (JSON/CSV)

Communication via fichiers pour éviter les dépendances :

```pascal
program RDataExchange;

uses
  SysUtils, Classes, fpjson, jsonparser;

type
  TRIntegration = class
  private
    FDataPath: string;
    FResultPath: string;
    FScriptPath: string;
  public
    constructor Create;

    procedure SendData(const Data: array of Double);
    function ReceiveResults: TJSONObject;
    procedure ExecuteRAnalysis;
  end;

constructor TRIntegration.Create;  
begin
  FDataPath := 'r_input.json';
  FResultPath := 'r_output.json';
  FScriptPath := 'analysis.R';
end;

procedure TRIntegration.SendData(const Data: array of Double);  
var
  JSON: TJSONObject;
  JSONArray: TJSONArray;
  i: Integer;
  FileStream: TFileStream;
  JSONString: string;
begin
  JSON := TJSONObject.Create;
  JSONArray := TJSONArray.Create;
  try
    // Convertir les données en JSON
    for i := Low(Data) to High(Data) do
      JSONArray.Add(Data[i]);

    JSON.Add('data', JSONArray);
    JSON.Add('timestamp', FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));

    // Sauvegarder
    JSONString := JSON.FormatJSON;
    FileStream := TFileStream.Create(FDataPath, fmCreate);
    try
      FileStream.WriteBuffer(JSONString[1], Length(JSONString));
    finally
      FileStream.Free;
    end;

  finally
    JSON.Free;
  end;
end;

function TRIntegration.ReceiveResults: TJSONObject;  
var
  FileStream: TFileStream;
  Parser: TJSONParser;
begin
  Result := nil;
  if not FileExists(FResultPath) then
    Exit;

  FileStream := TFileStream.Create(FResultPath, fmOpenRead);
  try
    Parser := TJSONParser.Create(FileStream, []);
    try
      Result := TJSONObject(Parser.Parse);
    finally
      Parser.Free;
    end;
  finally
    FileStream.Free;
  end;
end;

procedure TRIntegration.ExecuteRAnalysis;  
var
  Script: TStringList;
  Process: TProcess;
begin
  // Créer le script R
  Script := TStringList.Create;
  try
    Script.Add('library(jsonlite)');
    Script.Add('');
    Script.Add('# Lire les données JSON');
    Script.Add('input_data <- fromJSON("' + FDataPath + '")');
    Script.Add('data <- input_data$data');
    Script.Add('');
    Script.Add('# Analyses statistiques');
    Script.Add('results <- list(');
    Script.Add('  mean = mean(data),');
    Script.Add('  median = median(data),');
    Script.Add('  sd = sd(data),');
    Script.Add('  min = min(data),');
    Script.Add('  max = max(data),');
    Script.Add('  q25 = quantile(data, 0.25),');
    Script.Add('  q75 = quantile(data, 0.75)');
    Script.Add(')');
    Script.Add('');
    Script.Add('# Sauvegarder les résultats en JSON');
    Script.Add('write_json(results, "' + FResultPath + '", auto_unbox=TRUE)');

    Script.SaveToFile(FScriptPath);
  finally
    Script.Free;
  end;

  // Exécuter R
  Process := TProcess.Create(nil);
  try
    Process.Executable := 'Rscript';
    Process.Parameters.Add(FScriptPath);
    Process.Options := [poWaitOnExit];
    Process.Execute;
  finally
    Process.Free;
  end;
end;

// Utilisation
var
  RInt: TRIntegration;
  TestData: array[0..9] of Double = (1.5, 2.3, 3.1, 4.7, 5.2, 6.8, 7.1, 8.9, 9.3, 10.5);
  Results: TJSONObject;

begin
  RInt := TRIntegration.Create;
  try
    WriteLn('=== Communication avec R via JSON ===');
    WriteLn;

    // Envoyer les données à R
    WriteLn('Envoi des données à R...');
    RInt.SendData(TestData);

    // Exécuter l''analyse
    WriteLn('Exécution de l''analyse R...');
    RInt.ExecuteRAnalysis;

    // Récupérer les résultats
    WriteLn('Récupération des résultats...');
    Results := RInt.ReceiveResults;
    if Assigned(Results) then
    try
      WriteLn;
      WriteLn('Résultats :');
      WriteLn('  Moyenne : ', Results.Get('mean').AsFloat:0:2);
      WriteLn('  Médiane : ', Results.Get('median').AsFloat:0:2);
      WriteLn('  Écart-type : ', Results.Get('sd').AsFloat:0:2);
      WriteLn('  Min : ', Results.Get('min').AsFloat:0:2);
      WriteLn('  Max : ', Results.Get('max').AsFloat:0:2);
    finally
      Results.Free;
    end;

  finally
    RInt.Free;
  end;

  ReadLn;
end.
```

---

## Partie 3 : Comparaison et choix de la méthode

### 3.1 Tableau comparatif

| Critère | Python4Lazarus | Ligne de commande | Serveur REST | Fichiers |
|---------|----------------|-------------------|--------------|----------|
| **Performance** | ⭐⭐⭐⭐⭐ | ⭐⭐ | ⭐⭐⭐⭐ | ⭐ |
| **Simplicité** | ⭐⭐⭐ | ⭐⭐⭐⭐⭐ | ⭐⭐ | ⭐⭐⭐⭐ |
| **Portabilité** | ⭐⭐⭐ | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ |
| **Maintenance** | ⭐⭐⭐ | ⭐⭐⭐⭐ | ⭐⭐⭐ | ⭐⭐⭐⭐⭐ |
| **Gros volumes** | ⭐⭐⭐⭐⭐ | ⭐⭐ | ⭐⭐⭐⭐ | ⭐⭐ |
| **Debugging** | ⭐⭐⭐⭐ | ⭐⭐ | ⭐⭐⭐ | ⭐⭐⭐ |
| **Sécurité** | ⭐⭐⭐⭐ | ⭐⭐⭐ | ⭐⭐⭐⭐⭐ | ⭐⭐⭐ |

### 3.2 Recommandations

**Utilisez Python4Lazarus quand :**
- Vous avez besoin de performances maximales
- Vous faites beaucoup d'appels Python
- Vous voulez un couplage fort entre Pascal et Python
- Vous développez sur une seule plateforme

**Utilisez la ligne de commande quand :**
- Vous débutez avec l'intégration
- Vous avez des scripts R/Python existants
- La portabilité est essentielle
- Les performances ne sont pas critiques

**Utilisez des fichiers intermédiaires quand :**
- Les calculs sont longs (>1 seconde)
- Vous voulez découpler complètement les composants
- Vous devez archiver les résultats
- Plusieurs programmes doivent partager les données

**Utilisez un serveur REST quand :**
- Vous développez une application distribuée
- Python/R tourne sur une autre machine
- Vous voulez scaler horizontalement
- Plusieurs clients utilisent le même service

---

## Partie 4 : Exemples d'applications pratiques

### 4.1 Application de Machine Learning complète

Voici une application complète qui utilise Python pour le ML et FreePascal pour l'interface :

```pascal
program MLPredictionApp;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, Process, fpjson, jsonparser;

type
  TMLModel = class
  private
    FModelPath: string;
    FPythonScript: string;

    procedure CreateTrainingScript;
    procedure CreatePredictionScript;
    function ExecutePython(const ScriptName: string; const Args: array of string): string;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Train(const XData, YData: array of array of Double);
    function Predict(const Features: array of Double): Double;
    function GetModelAccuracy: Double;
  end;

constructor TMLModel.Create;  
begin
  FModelPath := 'model.pkl';
  FPythonScript := 'ml_script.py';
end;

destructor TMLModel.Destroy;  
begin
  // Nettoyer les fichiers temporaires
  if FileExists(FPythonScript) then
    DeleteFile(FPythonScript);
  inherited;
end;

procedure TMLModel.CreateTrainingScript;  
var
  Script: TStringList;
begin
  Script := TStringList.Create;
  try
    Script.Add('import sys');
    Script.Add('import json');
    Script.Add('import pickle');
    Script.Add('from sklearn.ensemble import RandomForestRegressor');
    Script.Add('from sklearn.model_selection import train_test_split');
    Script.Add('from sklearn.metrics import r2_score, mean_squared_error');
    Script.Add('import numpy as np');
    Script.Add('');
    Script.Add('# Charger les données depuis JSON');
    Script.Add('with open("training_data.json", "r") as f:');
    Script.Add('    data = json.load(f)');
    Script.Add('');
    Script.Add('X = np.array(data["X"])');
    Script.Add('y = np.array(data["y"])');
    Script.Add('');
    Script.Add('# Diviser en train/test');
    Script.Add('X_train, X_test, y_train, y_test = train_test_split(');
    Script.Add('    X, y, test_size=0.2, random_state=42');
    Script.Add(')');
    Script.Add('');
    Script.Add('# Entraîner le modèle');
    Script.Add('model = RandomForestRegressor(n_estimators=100, random_state=42)');
    Script.Add('model.fit(X_train, y_train)');
    Script.Add('');
    Script.Add('# Évaluer');
    Script.Add('y_pred = model.predict(X_test)');
    Script.Add('r2 = r2_score(y_test, y_pred)');
    Script.Add('mse = mean_squared_error(y_test, y_pred)');
    Script.Add('');
    Script.Add('# Sauvegarder le modèle');
    Script.Add('with open("' + FModelPath + '", "wb") as f:');
    Script.Add('    pickle.dump(model, f)');
    Script.Add('');
    Script.Add('# Sauvegarder les métriques');
    Script.Add('metrics = {"r2": r2, "mse": mse}');
    Script.Add('with open("metrics.json", "w") as f:');
    Script.Add('    json.dump(metrics, f)');
    Script.Add('');
    Script.Add('print(f"Modèle entraîné - R²: {r2:.4f}, MSE: {mse:.4f}")');

    Script.SaveToFile('train_model.py');
  finally
    Script.Free;
  end;
end;

procedure TMLModel.CreatePredictionScript;  
var
  Script: TStringList;
begin
  Script := TStringList.Create;
  try
    Script.Add('import sys');
    Script.Add('import json');
    Script.Add('import pickle');
    Script.Add('import numpy as np');
    Script.Add('');
    Script.Add('# Charger le modèle');
    Script.Add('with open("' + FModelPath + '", "rb") as f:');
    Script.Add('    model = pickle.load(f)');
    Script.Add('');
    Script.Add('# Charger les features depuis JSON');
    Script.Add('with open("predict_input.json", "r") as f:');
    Script.Add('    features = json.load(f)');
    Script.Add('');
    Script.Add('# Prédiction');
    Script.Add('X = np.array(features).reshape(1, -1)');
    Script.Add('prediction = model.predict(X)[0]');
    Script.Add('');
    Script.Add('# Sauvegarder le résultat');
    Script.Add('result = {"prediction": float(prediction)}');
    Script.Add('with open("predict_output.json", "w") as f:');
    Script.Add('    json.dump(result, f)');
    Script.Add('');
    Script.Add('print(prediction)');

    Script.SaveToFile('predict_model.py');
  finally
    Script.Free;
  end;
end;

function TMLModel.ExecutePython(const ScriptName: string; const Args: array of string): string;  
var
  Process: TProcess;
  Output: TStringList;
  i: Integer;
begin
  Result := '';
  Process := TProcess.Create(nil);
  Output := TStringList.Create;
  try
    {$IFDEF WINDOWS}
    Process.Executable := 'python';
    {$ELSE}
    Process.Executable := 'python3';
    {$ENDIF}

    Process.Parameters.Add(ScriptName);
    for i := Low(Args) to High(Args) do
      Process.Parameters.Add(Args[i]);

    Process.Options := [poWaitOnExit, poUsePipes];
    Process.Execute;

    Output.LoadFromStream(Process.Output);
    Result := Output.Text;
  finally
    Output.Free;
    Process.Free;
  end;
end;

procedure TMLModel.Train(const XData, YData: array of array of Double);  
var
  JSON: TJSONObject;
  XArray, YArray: TJSONArray;
  RowArray: TJSONArray;
  i, j: Integer;
  DataFile: TFileStream;
  JSONString: string;
begin
  // Créer le script d'entraînement
  CreateTrainingScript;

  // Préparer les données en JSON
  JSON := TJSONObject.Create;
  XArray := TJSONArray.Create;
  YArray := TJSONArray.Create;
  try
    // Ajouter X (matrice)
    for i := Low(XData) to High(XData) do
    begin
      RowArray := TJSONArray.Create;
      for j := Low(XData[i]) to High(XData[i]) do
        RowArray.Add(XData[i][j]);
      XArray.Add(RowArray);
    end;

    // Ajouter y (vecteur)
    for i := Low(YData) to High(YData) do
      YArray.Add(YData[i][0]);

    JSON.Add('X', XArray);
    JSON.Add('y', YArray);

    // Sauvegarder
    JSONString := JSON.FormatJSON;
    DataFile := TFileStream.Create('training_data.json', fmCreate);
    try
      DataFile.WriteBuffer(JSONString[1], Length(JSONString));
    finally
      DataFile.Free;
    end;
  finally
    JSON.Free;
  end;

  // Lancer l'entraînement
  WriteLn('Entraînement du modèle...');
  WriteLn(ExecutePython('train_model.py', []));
end;

function TMLModel.Predict(const Features: array of Double): Double;  
var
  JSON: TJSONArray;
  i: Integer;
  DataFile: TFileStream;
  JSONString: string;
  ResultJSON: TJSONObject;
  Parser: TJSONParser;
  FileStream: TFileStream;
begin
  Result := 0;

  // Créer le script de prédiction
  CreatePredictionScript;

  // Préparer les features en JSON
  JSON := TJSONArray.Create;
  try
    for i := Low(Features) to High(Features) do
      JSON.Add(Features[i]);

    JSONString := JSON.FormatJSON;
    DataFile := TFileStream.Create('predict_input.json', fmCreate);
    try
      DataFile.WriteBuffer(JSONString[1], Length(JSONString));
    finally
      DataFile.Free;
    end;
  finally
    JSON.Free;
  end;

  // Lancer la prédiction
  ExecutePython('predict_model.py', []);

  // Lire le résultat
  if FileExists('predict_output.json') then
  begin
    FileStream := TFileStream.Create('predict_output.json', fmOpenRead);
    try
      Parser := TJSONParser.Create(FileStream, []);
      try
        ResultJSON := TJSONObject(Parser.Parse);
        try
          Result := ResultJSON.Get('prediction').AsFloat;
        finally
          ResultJSON.Free;
        end;
      finally
        Parser.Free;
      end;
    finally
      FileStream.Free;
    end;
  end;
end;

function TMLModel.GetModelAccuracy: Double;  
var
  Parser: TJSONParser;
  FileStream: TFileStream;
  JSON: TJSONObject;
begin
  Result := 0;

  if not FileExists('metrics.json') then
    Exit;

  FileStream := TFileStream.Create('metrics.json', fmOpenRead);
  try
    Parser := TJSONParser.Create(FileStream, []);
    try
      JSON := TJSONObject(Parser.Parse);
      try
        Result := JSON.Get('r2').AsFloat;
      finally
        JSON.Free;
      end;
    finally
      Parser.Free;
    end;
  finally
    FileStream.Free;
  end;
end;

// Programme principal
var
  Model: TMLModel;

  // Données d'entraînement (exemple simple : prédire Y = 2*X1 + 3*X2)
  XTrain: array[0..99, 0..1] of Double;
  YTrain: array[0..99, 0..0] of Double;

  i: Integer;
  TestFeatures: array[0..1] of Double;
  Prediction: Double;
  Accuracy: Double;

begin
  WriteLn('=== Application de Machine Learning ===');
  WriteLn;

  // Générer des données d'entraînement
  WriteLn('Génération des données d''entraînement...');
  for i := 0 to 99 do
  begin
    XTrain[i][0] := Random * 10;
    XTrain[i][1] := Random * 10;
    YTrain[i][0] := 2 * XTrain[i][0] + 3 * XTrain[i][1] + (Random - 0.5); // Avec un peu de bruit
  end;

  Model := TMLModel.Create;
  try
    // Entraîner le modèle
    Model.Train(XTrain, YTrain);

    // Obtenir la précision
    Accuracy := Model.GetModelAccuracy;
    WriteLn;
    WriteLn('Précision du modèle (R²) : ', Accuracy:0:4);

    // Faire des prédictions
    WriteLn;
    WriteLn('=== Prédictions ===');

    TestFeatures[0] := 5.0;
    TestFeatures[1] := 3.0;
    Prediction := Model.Predict(TestFeatures);
    WriteLn('Pour X1=5.0, X2=3.0 : ', Prediction:0:2, ' (attendu : ', 2*5.0 + 3*3.0:0:2, ')');

    TestFeatures[0] := 2.5;
    TestFeatures[1] := 7.5;
    Prediction := Model.Predict(TestFeatures);
    WriteLn('Pour X1=2.5, X2=7.5 : ', Prediction:0:2, ' (attendu : ', 2*2.5 + 3*7.5:0:2, ')');

  finally
    Model.Free;
  end;

  WriteLn;
  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
```

### 4.2 Visualisation scientifique avec R

Application qui génère des rapports statistiques avec graphiques :

```pascal
program ScientificReport;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, Process, DateUtils;

type
  TReportGenerator = class
  private
    FDataFile: string;
    FReportPath: string;

    procedure GenerateRMarkdownScript;
    function ExecuteR(const ScriptName: string): Boolean;
  public
    constructor Create(const ReportName: string);

    procedure AddData(const XValues, YValues: array of Double; const SeriesName: string);
    function GenerateReport: Boolean;
    function GetReportPath: string;
  end;

constructor TReportGenerator.Create(const ReportName: string);  
begin
  FDataFile := 'report_data.csv';
  FReportPath := ReportName + '_' + FormatDateTime('yyyymmdd_hhnnss', Now) + '.html';
end;

procedure TReportGenerator.AddData(const XValues, YValues: array of Double;
  const SeriesName: string);
var
  Data: TStringList;
  i: Integer;
begin
  Data := TStringList.Create;
  try
    // Si le fichier existe déjà, charger les données existantes
    if FileExists(FDataFile) then
      Data.LoadFromFile(FDataFile)
    else
    begin
      // Créer l'en-tête
      Data.Add('Series,X,Y');
    end;

    // Ajouter les nouvelles données
    for i := Low(XValues) to High(XValues) do
      Data.Add(Format('%s,%.6f,%.6f', [SeriesName, XValues[i], YValues[i]]));

    Data.SaveToFile(FDataFile);
  finally
    Data.Free;
  end;
end;

procedure TReportGenerator.GenerateRMarkdownScript;  
var
  Script: TStringList;
begin
  Script := TStringList.Create;
  try
    Script.Add('---');
    Script.Add('title: "Rapport d''Analyse Scientifique"');
    Script.Add('author: "Généré par FreePascal"');
    Script.Add('date: "`r Sys.Date()`"');
    Script.Add('output:');
    Script.Add('  html_document:');
    Script.Add('    toc: true');
    Script.Add('    theme: united');
    Script.Add('---');
    Script.Add('');
    Script.Add('```{r setup, include=FALSE}');
    Script.Add('knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)');
    Script.Add('library(ggplot2)');
    Script.Add('library(dplyr)');
    Script.Add('```');
    Script.Add('');
    Script.Add('## Chargement des données');
    Script.Add('');
    Script.Add('```{r}');
    Script.Add('data <- read.csv("' + FDataFile + '")');
    Script.Add('head(data)');
    Script.Add('```');
    Script.Add('');
    Script.Add('## Statistiques descriptives');
    Script.Add('');
    Script.Add('```{r}');
    Script.Add('summary(data)');
    Script.Add('```');
    Script.Add('');
    Script.Add('## Statistiques par série');
    Script.Add('');
    Script.Add('```{r}');
    Script.Add('data %>%');
    Script.Add('  group_by(Series) %>%');
    Script.Add('  summarise(');
    Script.Add('    Count = n(),');
    Script.Add('    Mean_X = mean(X),');
    Script.Add('    Mean_Y = mean(Y),');
    Script.Add('    SD_X = sd(X),');
    Script.Add('    SD_Y = sd(Y),');
    Script.Add('    Cor = cor(X, Y)');
    Script.Add('  )');
    Script.Add('```');
    Script.Add('');
    Script.Add('## Visualisations');
    Script.Add('');
    Script.Add('### Nuage de points');
    Script.Add('');
    Script.Add('```{r fig.width=10, fig.height=6}');
    Script.Add('ggplot(data, aes(x = X, y = Y, color = Series)) +');
    Script.Add('  geom_point(size = 3, alpha = 0.6) +');
    Script.Add('  geom_smooth(method = "lm", se = TRUE) +');
    Script.Add('  theme_minimal() +');
    Script.Add('  labs(title = "Relation X-Y par série",');
    Script.Add('       x = "Variable X",');
    Script.Add('       y = "Variable Y") +');
    Script.Add('  theme(legend.position = "bottom")');
    Script.Add('```');
    Script.Add('');
    Script.Add('### Distribution de Y');
    Script.Add('');
    Script.Add('```{r fig.width=10, fig.height=6}');
    Script.Add('ggplot(data, aes(x = Y, fill = Series)) +');
    Script.Add('  geom_histogram(bins = 30, alpha = 0.7, position = "identity") +');
    Script.Add('  theme_minimal() +');
    Script.Add('  labs(title = "Distribution de Y",');
    Script.Add('       x = "Valeur Y",');
    Script.Add('       y = "Fréquence")');
    Script.Add('```');
    Script.Add('');
    Script.Add('### Boxplots');
    Script.Add('');
    Script.Add('```{r fig.width=10, fig.height=6}');
    Script.Add('ggplot(data, aes(x = Series, y = Y, fill = Series)) +');
    Script.Add('  geom_boxplot() +');
    Script.Add('  theme_minimal() +');
    Script.Add('  labs(title = "Distribution de Y par série",');
    Script.Add('       x = "Série",');
    Script.Add('       y = "Valeur Y")');
    Script.Add('```');
    Script.Add('');
    Script.Add('## Analyses de régression');
    Script.Add('');
    Script.Add('```{r}');
    Script.Add('# Régression pour chaque série');
    Script.Add('series <- unique(data$Series)');
    Script.Add('');
    Script.Add('for(s in series) {');
    Script.Add('  cat("\n### Série:", s, "\n")');
    Script.Add('  subset_data <- data[data$Series == s, ]');
    Script.Add('  model <- lm(Y ~ X, data = subset_data)');
    Script.Add('  print(summary(model))');
    Script.Add('  cat("\n")');
    Script.Add('}');
    Script.Add('```');
    Script.Add('');
    Script.Add('## Conclusion');
    Script.Add('');
    Script.Add('Ce rapport a été généré automatiquement par FreePascal le `r Sys.time()`.');

    Script.SaveToFile('report.Rmd');
  finally
    Script.Free;
  end;
end;

function TReportGenerator.ExecuteR(const ScriptName: string): Boolean;  
var
  Process: TProcess;
  Output: TStringList;
begin
  Result := False;
  Process := TProcess.Create(nil);
  Output := TStringList.Create;
  try
    {$IFDEF WINDOWS}
    Process.Executable := 'Rscript';
    {$ELSE}
    Process.Executable := 'Rscript';
    {$ENDIF}

    Process.Parameters.Add('-e');
    Process.Parameters.Add('rmarkdown::render("' + ScriptName + '", output_file = "' + FReportPath + '")');

    Process.Options := [poWaitOnExit, poUsePipes];
    Process.Execute;

    Output.LoadFromStream(Process.Output);
    WriteLn(Output.Text);

    Result := FileExists(FReportPath);
  finally
    Output.Free;
    Process.Free;
  end;
end;

function TReportGenerator.GenerateReport: Boolean;  
begin
  WriteLn('Génération du script R Markdown...');
  GenerateRMarkdownScript;

  WriteLn('Compilation du rapport avec R...');
  Result := ExecuteR('report.Rmd');

  if Result then
    WriteLn('Rapport généré : ', FReportPath)
  else
    WriteLn('Erreur lors de la génération du rapport');
end;

function TReportGenerator.GetReportPath: string;  
begin
  Result := FReportPath;
end;

// Programme principal
var
  Report: TReportGenerator;
  i: Integer;
  X, Y: array[0..49] of Double;

begin
  WriteLn('=== Générateur de Rapports Scientifiques ===');
  WriteLn;

  Report := TReportGenerator.Create('scientific_report');
  try
    // Générer des données de test - Série 1
    WriteLn('Génération des données...');
    for i := 0 to 49 do
    begin
      X[i] := i * 0.5;
      Y[i] := 2 * X[i] + 5 + (Random - 0.5) * 3;
    end;
    Report.AddData(X, Y, 'Série_Linéaire');

    // Série 2 - Relation quadratique
    for i := 0 to 49 do
    begin
      X[i] := i * 0.5;
      Y[i] := 0.1 * X[i] * X[i] + (Random - 0.5) * 2;
    end;
    Report.AddData(X, Y, 'Série_Quadratique');

    // Série 3 - Relation logarithmique
    for i := 0 to 49 do
    begin
      X[i] := (i + 1) * 0.5;
      Y[i] := 5 * Ln(X[i]) + (Random - 0.5) * 1.5;
    end;
    Report.AddData(X, Y, 'Série_Logarithmique');

    // Générer le rapport
    WriteLn;
    if Report.GenerateReport then
    begin
      WriteLn;
      WriteLn('✓ Rapport généré avec succès !');
      WriteLn('  Fichier : ', Report.GetReportPath);
      WriteLn;
      WriteLn('Ouvrez le fichier HTML pour voir le rapport complet avec graphiques.');
    end;

  finally
    Report.Free;
  end;

  WriteLn;
  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
```

### 4.3 Traitement d'images avec OpenCV (Python)

```pascal
program ImageProcessingApp;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, Process;

type
  TImageProcessor = class
  private
    procedure CreateOpenCVScript;
    function ExecutePython(const ScriptName: string): string;
  public
    function DetectFaces(const ImagePath: string): Integer;
    function ApplyFilters(const InputPath, OutputPath: string; const FilterType: string): Boolean;
    function ExtractFeatures(const ImagePath: string): string;
  end;

procedure TImageProcessor.CreateOpenCVScript;  
var
  Script: TStringList;
begin
  Script := TStringList.Create;
  try
    Script.Add('import cv2');
    Script.Add('import sys');
    Script.Add('import json');
    Script.Add('import numpy as np');
    Script.Add('');
    Script.Add('def detect_faces(image_path):');
    Script.Add('    """Détecte les visages dans une image"""');
    Script.Add('    face_cascade = cv2.CascadeClassifier(');
    Script.Add('        cv2.data.haarcascades + "haarcascade_frontalface_default.xml"');
    Script.Add('    )');
    Script.Add('    ');
    Script.Add('    img = cv2.imread(image_path)');
    Script.Add('    gray = cv2.cvtColor(img, cv2.COLOR_BGR2GRAY)');
    Script.Add('    ');
    Script.Add('    faces = face_cascade.detectMultiScale(gray, 1.1, 4)');
    Script.Add('    ');
    Script.Add('    # Dessiner des rectangles autour des visages');
    Script.Add('    for (x, y, w, h) in faces:');
    Script.Add('        cv2.rectangle(img, (x, y), (x+w, y+h), (255, 0, 0), 2)');
    Script.Add('    ');
    Script.Add('    cv2.imwrite("output_faces.jpg", img)');
    Script.Add('    return len(faces)');
    Script.Add('');
    Script.Add('def apply_filter(input_path, output_path, filter_type):');
    Script.Add('    """Applique différents filtres"""');
    Script.Add('    img = cv2.imread(input_path)');
    Script.Add('    ');
    Script.Add('    if filter_type == "blur":');
    Script.Add('        result = cv2.GaussianBlur(img, (15, 15), 0)');
    Script.Add('    elif filter_type == "sharpen":');
    Script.Add('        kernel = np.array([[-1,-1,-1], [-1,9,-1], [-1,-1,-1]])');
    Script.Add('        result = cv2.filter2D(img, -1, kernel)');
    Script.Add('    elif filter_type == "edge":');
    Script.Add('        result = cv2.Canny(img, 100, 200)');
    Script.Add('    elif filter_type == "grayscale":');
    Script.Add('        result = cv2.cvtColor(img, cv2.COLOR_BGR2GRAY)');
    Script.Add('    else:');
    Script.Add('        result = img');
    Script.Add('    ');
    Script.Add('    cv2.imwrite(output_path, result)');
    Script.Add('    return True');
    Script.Add('');
    Script.Add('def extract_features(image_path):');
    Script.Add('    """Extrait des caractéristiques de l''image"""');
    Script.Add('    img = cv2.imread(image_path)');
    Script.Add('    ');
    Script.Add('    features = {');
    Script.Add('        "width": img.shape[1],');
    Script.Add('        "height": img.shape[0],');
    Script.Add('        "channels": img.shape[2] if len(img.shape) > 2 else 1,');
    Script.Add('        "mean_brightness": int(cv2.cvtColor(img, cv2.COLOR_BGR2GRAY).mean()),');
    Script.Add('        "dominant_color": img.mean(axis=(0,1)).tolist()');
    Script.Add('    }');
    Script.Add('    ');
    Script.Add('    return json.dumps(features)');
    Script.Add('');
    Script.Add('if __name__ == "__main__":');
    Script.Add('    command = sys.argv[1]');
    Script.Add('    ');
    Script.Add('    if command == "detect_faces":');
    Script.Add('        count = detect_faces(sys.argv[2])');
    Script.Add('        print(count)');
    Script.Add('    elif command == "apply_filter":');
    Script.Add('        apply_filter(sys.argv[2], sys.argv[3], sys.argv[4])');
    Script.Add('        print("OK")');
    Script.Add('    elif command == "extract_features":');
    Script.Add('        print(extract_features(sys.argv[2]))');

    Script.SaveToFile('opencv_processor.py');
  finally
    Script.Free;
  end;
end;

function TImageProcessor.ExecutePython(const ScriptName: string): string;  
var
  Process: TProcess;
  Output: TStringList;
  i: Integer;
begin
  Result := '';
  Process := TProcess.Create(nil);
  Output := TStringList.Create;
  try
    {$IFDEF WINDOWS}
    Process.Executable := 'python';
    {$ELSE}
    Process.Executable := 'python3';
    {$ENDIF}

    Process.Parameters.Add(ScriptName);
    for i := 2 to ParamCount do
      Process.Parameters.Add(ParamStr(i));

    Process.Options := [poWaitOnExit, poUsePipes];
    Process.Execute;

    Output.LoadFromStream(Process.Output);
    Result := Trim(Output.Text);
  finally
    Output.Free;
    Process.Free;
  end;
end;

function TImageProcessor.DetectFaces(const ImagePath: string): Integer;  
var
  Output: string;
begin
  CreateOpenCVScript;

  // Exécuter la détection de visages
  Process.Parameters.Clear;
  Process.Parameters.Add('opencv_processor.py');
  Process.Parameters.Add('detect_faces');
  Process.Parameters.Add(ImagePath);

  Output := ExecutePython('opencv_processor.py');
  Result := StrToIntDef(Output, 0);
end;

function TImageProcessor.ApplyFilters(const InputPath, OutputPath: string;
  const FilterType: string): Boolean;
var
  Process: TProcess;
begin
  Result := False;
  CreateOpenCVScript;

  Process := TProcess.Create(nil);
  try
    {$IFDEF WINDOWS}
    Process.Executable := 'python';
    {$ELSE}
    Process.Executable := 'python3';
    {$ENDIF}

    Process.Parameters.Add('opencv_processor.py');
    Process.Parameters.Add('apply_filter');
    Process.Parameters.Add(InputPath);
    Process.Parameters.Add(OutputPath);
    Process.Parameters.Add(FilterType);

    Process.Options := [poWaitOnExit];
    Process.Execute;

    Result := FileExists(OutputPath);
  finally
    Process.Free;
  end;
end;

function TImageProcessor.ExtractFeatures(const ImagePath: string): string;  
var
  Process: TProcess;
  Output: TStringList;
begin
  CreateOpenCVScript;

  Process := TProcess.Create(nil);
  Output := TStringList.Create;
  try
    {$IFDEF WINDOWS}
    Process.Executable := 'python';
    {$ELSE}
    Process.Executable := 'python3';
    {$ENDIF}

    Process.Parameters.Add('opencv_processor.py');
    Process.Parameters.Add('extract_features');
    Process.Parameters.Add(ImagePath);

    Process.Options := [poWaitOnExit, poUsePipes];
    Process.Execute;

    Output.LoadFromStream(Process.Output);
    Result := Output.Text;
  finally
    Output.Free;
    Process.Free;
  end;
end;

// Programme principal
var
  Processor: TImageProcessor;
  ImagePath: string;
  FaceCount: Integer;
  Features: string;

begin
  WriteLn('=== Traitement d''Images avec OpenCV ===');
  WriteLn;

  // Vérifier si une image est fournie en paramètre
  if ParamCount < 1 then
  begin
    WriteLn('Usage: ', ExtractFileName(ParamStr(0)), ' <chemin_image>');
    WriteLn;
    WriteLn('Exemple: ', ExtractFileName(ParamStr(0)), ' photo.jpg');
    Exit;
  end;

  ImagePath := ParamStr(1);

  if not FileExists(ImagePath) then
  begin
    WriteLn('Erreur: Le fichier ', ImagePath, ' n''existe pas.');
    Exit;
  end;

  Processor := TImageProcessor.Create;
  try
    // Détecter les visages
    WriteLn('Détection de visages...');
    FaceCount := Processor.DetectFaces(ImagePath);
    WriteLn('  → ', FaceCount, ' visage(s) détecté(s)');
    WriteLn('  → Résultat sauvegardé dans : output_faces.jpg');
    WriteLn;

    // Appliquer différents filtres
    WriteLn('Application des filtres...');

    if Processor.ApplyFilters(ImagePath, 'output_blur.jpg', 'blur') then
      WriteLn('  ✓ Flou : output_blur.jpg');

    if Processor.ApplyFilters(ImagePath, 'output_sharpen.jpg', 'sharpen') then
      WriteLn('  ✓ Netteté : output_sharpen.jpg');

    if Processor.ApplyFilters(ImagePath, 'output_edge.jpg', 'edge') then
      WriteLn('  ✓ Contours : output_edge.jpg');

    if Processor.ApplyFilters(ImagePath, 'output_gray.jpg', 'grayscale') then
      WriteLn('  ✓ Niveaux de gris : output_gray.jpg');

    WriteLn;

    // Extraire les caractéristiques
    WriteLn('Extraction des caractéristiques...');
    Features := Processor.ExtractFeatures(ImagePath);
    WriteLn(Features);

  finally
    Processor.Free;
  end;

  WriteLn;
  WriteLn('Traitement terminé !');
  ReadLn;
end.
```

---

## Partie 5 : Bonnes pratiques et gestion des erreurs

### 5.1 Gestion robuste des erreurs

Toujours gérer les cas où Python/R n'est pas installé ou rencontre une erreur :

```pascal
type
  TIntegrationError = (ieNone, ieNotInstalled, ieScriptError, ieTimeout, ieInvalidData);

function CheckPythonInstallation: Boolean;  
var
  Process: TProcess;
  Output: TStringList;
begin
  Result := False;
  Process := TProcess.Create(nil);
  Output := TStringList.Create;
  try
    {$IFDEF WINDOWS}
    Process.Executable := 'python';
    {$ELSE}
    Process.Executable := 'python3';
    {$ENDIF}

    Process.Parameters.Add('--version');
    Process.Options := [poWaitOnExit, poUsePipes, poNoConsole];

    try
      Process.Execute;
      Output.LoadFromStream(Process.Output);

      // Vérifier que la sortie contient "Python"
      Result := Pos('Python', Output.Text) > 0;

      if Result then
        WriteLn('✓ Python détecté : ', Trim(Output.Text))
      else
        WriteLn('✗ Python non trouvé');

    except
      on E: Exception do
      begin
        WriteLn('✗ Erreur lors de la vérification de Python : ', E.Message);
        Result := False;
      end;
    end;

  finally
    Output.Free;
    Process.Free;
  end;
end;

function CheckRInstallation: Boolean;  
var
  Process: TProcess;
  Output: TStringList;
begin
  Result := False;
  Process := TProcess.Create(nil);
  Output := TStringList.Create;
  try
    Process.Executable := 'Rscript';
    Process.Parameters.Add('--version');
    Process.Options := [poWaitOnExit, poUsePipes, poNoConsole];

    try
      Process.Execute;
      Output.LoadFromStream(Process.Output);

      Result := (Pos('R scripting', Output.Text) > 0) or
                (Pos('version', Output.Text) > 0);

      if Result then
        WriteLn('✓ R détecté')
      else
        WriteLn('✗ R non trouvé');

    except
      on E: Exception do
      begin
        WriteLn('✗ Erreur lors de la vérification de R : ', E.Message);
        Result := False;
      end;
    end;

  finally
    Output.Free;
    Process.Free;
  end;
end;

procedure CheckEnvironment;  
begin
  WriteLn('=== Vérification de l''environnement ===');
  WriteLn;

  CheckPythonInstallation;
  CheckRInstallation;

  WriteLn;
end;
```

### 5.2 Timeout et processus bloqués

Protégez-vous contre les scripts qui ne se terminent pas :

```pascal
uses
  Classes, SysUtils, Process, DateUtils;

function ExecutePythonWithTimeout(const Script: string;
  const Args: array of string; TimeoutSeconds: Integer): string;
var
  Process: TProcess;
  Output: TStringList;
  StartTime: TDateTime;
  i: Integer;
begin
  Result := '';
  Process := TProcess.Create(nil);
  Output := TStringList.Create;
  try
    {$IFDEF WINDOWS}
    Process.Executable := 'python';
    {$ELSE}
    Process.Executable := 'python3';
    {$ENDIF}

    Process.Parameters.Add(Script);
    for i := Low(Args) to High(Args) do
      Process.Parameters.Add(Args[i]);

    Process.Options := [poUsePipes, poNoConsole];

    StartTime := Now;
    Process.Execute;

    // Attendre avec timeout
    while Process.Running do
    begin
      Sleep(100);

      // Vérifier le timeout
      if SecondsBetween(Now, StartTime) > TimeoutSeconds then
      begin
        Process.Terminate(1);
        raise Exception.Create('Timeout : le script Python a dépassé ' +
                             IntToStr(TimeoutSeconds) + ' secondes');
      end;
    end;

    // Lire la sortie
    Output.LoadFromStream(Process.Output);
    Result := Output.Text;

    // Vérifier le code de sortie
    if Process.ExitStatus <> 0 then
    begin
      raise Exception.CreateFmt('Le script Python a échoué (code: %d)',
                               [Process.ExitStatus]);
    end;

  finally
    Output.Free;
    Process.Free;
  end;
end;
```

### 5.3 Validation des données

Toujours valider les données avant de les envoyer à Python/R :

```pascal
function ValidateNumericArray(const Data: array of Double): Boolean;  
var
  i: Integer;
begin
  Result := True;

  // Vérifier qu'il y a des données
  if Length(Data) = 0 then
  begin
    WriteLn('Erreur : tableau vide');
    Exit(False);
  end;

  // Vérifier les valeurs
  for i := Low(Data) to High(Data) do
  begin
    // Vérifier NaN et Infinity
    if IsNaN(Data[i]) or IsInfinite(Data[i]) then
    begin
      WriteLn('Erreur : valeur invalide à l''index ', i);
      Exit(False);
    end;
  end;
end;

function SanitizeStringForJSON(const S: string): string;  
begin
  // Échapper les caractères spéciaux pour JSON
  Result := StringReplace(S, '\', '\\', [rfReplaceAll]);
  Result := StringReplace(Result, '"', '\"', [rfReplaceAll]);
  Result := StringReplace(Result, #10, '\n', [rfReplaceAll]);
  Result := StringReplace(Result, #13, '\r', [rfReplaceAll]);
  Result := StringReplace(Result, #9, '\t', [rfReplaceAll]);
end;
```

### 5.4 Gestion des chemins multi-plateformes

Les chemins de fichiers diffèrent entre Windows et Linux :

```pascal
function GetPythonExecutable: string;  
begin
  {$IFDEF WINDOWS}
  Result := 'python.exe';
  {$ELSE}
  Result := 'python3';
  {$ENDIF}
end;

function GetRExecutable: string;  
begin
  {$IFDEF WINDOWS}
  Result := 'Rscript.exe';
  {$ELSE}
  Result := 'Rscript';
  {$ENDIF}
end;

function NormalizePath(const Path: string): string;  
begin
  Result := Path;
  {$IFDEF WINDOWS}
  // Remplacer / par \
  Result := StringReplace(Result, '/', '\', [rfReplaceAll]);
  {$ELSE}
  // Remplacer \ par /
  Result := StringReplace(Result, '\', '/', [rfReplaceAll]);
  {$ENDIF}
end;

function GetTempDirectory: string;  
begin
  {$IFDEF WINDOWS}
  Result := GetEnvironmentVariable('TEMP');
  if Result = '' then
    Result := 'C:\Temp';
  {$ELSE}
  Result := '/tmp';
  {$ENDIF}

  // S'assurer que le répertoire se termine par le séparateur
  Result := IncludeTrailingPathDelimiter(Result);
end;
```

### 5.5 Logging et débogage

Implémentez un système de logging pour faciliter le débogage :

```pascal
type
  TLogLevel = (llDebug, llInfo, llWarning, llError);

procedure Log(const Message: string; Level: TLogLevel = llInfo);  
var
  LogFile: TextFile;
  TimeStamp: string;
  LevelStr: string;
begin
  TimeStamp := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);

  case Level of
    llDebug: LevelStr := 'DEBUG';
    llInfo: LevelStr := 'INFO';
    llWarning: LevelStr := 'WARNING';
    llError: LevelStr := 'ERROR';
  end;

  // Afficher dans la console
  WriteLn(Format('[%s] %s: %s', [TimeStamp, LevelStr, Message]));

  // Écrire dans un fichier
  try
    AssignFile(LogFile, 'integration.log');
    if FileExists('integration.log') then
      Append(LogFile)
    else
      Rewrite(LogFile);

    WriteLn(LogFile, Format('[%s] %s: %s', [TimeStamp, LevelStr, Message]));
    CloseFile(LogFile);
  except
    // Ignorer les erreurs de logging
  end;
end;

// Utilisation
procedure ExecuteWithLogging;  
begin
  Log('Démarrage de l''intégration Python', llInfo);

  try
    Log('Vérification de Python...', llDebug);
    if not CheckPythonInstallation then
    begin
      Log('Python n''est pas installé', llError);
      Exit;
    end;

    Log('Exécution du script...', llDebug);
    // ... votre code ...

    Log('Intégration terminée avec succès', llInfo);

  except
    on E: Exception do
      Log('Erreur : ' + E.Message, llError);
  end;
end;
```

---

## Partie 6 : Optimisation des performances

### 6.1 Réutilisation des processus Python

Au lieu de lancer Python à chaque appel, gardez un processus actif :

```pascal
type
  TPersistentPython = class
  private
    FProcess: TProcess;
    FActive: Boolean;

    procedure StartPython;
    procedure StopPython;
  public
    constructor Create;
    destructor Destroy; override;

    function Execute(const Code: string): string;
    property Active: Boolean read FActive;
  end;

constructor TPersistentPython.Create;  
begin
  FProcess := TProcess.Create(nil);
  FActive := False;
  StartPython;
end;

destructor TPersistentPython.Destroy;  
begin
  StopPython;
  FProcess.Free;
  inherited;
end;

procedure TPersistentPython.StartPython;  
begin
  if FActive then Exit;

  {$IFDEF WINDOWS}
  FProcess.Executable := 'python';
  {$ELSE}
  FProcess.Executable := 'python3';
  {$ENDIF}

  FProcess.Parameters.Add('-i'); // Mode interactif
  FProcess.Options := [poUsePipes];

  try
    FProcess.Execute;
    FActive := True;
    Log('Python démarré en mode persistant', llInfo);
  except
    on E: Exception do
    begin
      Log('Erreur au démarrage de Python : ' + E.Message, llError);
      FActive := False;
    end;
  end;
end;

procedure TPersistentPython.StopPython;  
begin
  if not FActive then Exit;

  if FProcess.Running then
    FProcess.Terminate(0);

  FActive := False;
  Log('Python arrêté', llInfo);
end;

function TPersistentPython.Execute(const Code: string): string;  
var
  Output: TStringList;
begin
  Result := '';

  if not FActive then
    raise Exception.Create('Python n''est pas actif');

  Output := TStringList.Create;
  try
    // Envoyer le code à Python
    FProcess.Input.Write(Code[1], Length(Code));
    FProcess.Input.WriteLn('');

    // Attendre et lire la réponse
    Sleep(100);

    Output.LoadFromStream(FProcess.Output);
    Result := Output.Text;

  finally
    Output.Free;
  end;
end;

// Utilisation
var
  Python: TPersistentPython;

begin
  Python := TPersistentPython.Create;
  try
    // Plusieurs appels sans relancer Python à chaque fois
    WriteLn(Python.Execute('print(2 + 2)'));
    WriteLn(Python.Execute('print("Hello")'));
    WriteLn(Python.Execute('import math; print(math.pi)'));
  finally
    Python.Free;
  end;
end;
```

### 6.2 Mise en cache des résultats

Évitez de recalculer les mêmes choses :

```pascal
uses
  Generics.Collections;

type
  TResultCache = class
  private
    FCache: TDictionary<string, string>;
    FMaxSize: Integer;

    function GenerateKey(const FunctionName: string; const Params: array of string): string;
  public
    constructor Create(MaxSize: Integer = 100);
    destructor Destroy; override;

    function Get(const FunctionName: string; const Params: array of string;
                 out Result: string): Boolean;
    procedure Put(const FunctionName: string; const Params: array of string;
                  const Result: string);
    procedure Clear;
  end;

constructor TResultCache.Create(MaxSize: Integer);  
begin
  FCache := TDictionary<string, string>.Create;
  FMaxSize := MaxSize;
end;

destructor TResultCache.Destroy;  
begin
  FCache.Free;
  inherited;
end;

function TResultCache.GenerateKey(const FunctionName: string;
  const Params: array of string): string;
var
  i: Integer;
begin
  Result := FunctionName;
  for i := Low(Params) to High(Params) do
    Result := Result + '|' + Params[i];
end;

function TResultCache.Get(const FunctionName: string;
  const Params: array of string; out Result: string): Boolean;
var
  Key: string;
begin
  Key := GenerateKey(FunctionName, Params);
  Result := FCache.TryGetValue(Key, Result);

  if Result then
    Log('Cache hit pour : ' + Key, llDebug)
  else
    Log('Cache miss pour : ' + Key, llDebug);
end;

procedure TResultCache.Put(const FunctionName: string;
  const Params: array of string; const Result: string);
var
  Key: string;
begin
  Key := GenerateKey(FunctionName, Params);

  // Si le cache est plein, le vider
  if FCache.Count >= FMaxSize then
    Clear;

  FCache.AddOrSetValue(Key, Result);
  Log('Résultat mis en cache : ' + Key, llDebug);
end;

procedure TResultCache.Clear;  
begin
  FCache.Clear;
  Log('Cache vidé', llDebug);
end;

// Utilisation avec cache
var
  Cache: TResultCache;
  Result: string;

function CalculateWithPython(const Expression: string): string;  
begin
  // Vérifier le cache
  if Cache.Get('calculate', [Expression], Result) then
    Exit(Result);

  // Calculer avec Python
  Result := ExecutePython('calc.py', [Expression]);

  // Mettre en cache
  Cache.Put('calculate', [Expression], Result);
end;

begin
  Cache := TResultCache.Create(50);
  try
    // Premier appel : calcule avec Python
    WriteLn(CalculateWithPython('2 + 2'));  // Lent

    // Deuxième appel : récupère du cache
    WriteLn(CalculateWithPython('2 + 2'));  // Rapide !

  finally
    Cache.Free;
  end;
end;
```

### 6.3 Traitement parallèle

Pour traiter plusieurs requêtes Python/R en parallèle :

```pascal
uses
  Classes, MTProcs;

procedure ProcessDataInParallel(const DataSets: array of TDoubleArray);  
var
  Results: array of string;
  i: Integer;
begin
  SetLength(Results, Length(DataSets));

  // Traiter chaque dataset en parallèle
  ProcThreadPool.DoParallelLocalProc(
    procedure(Index: PtrInt; ThreadIndex: PtrInt; Data: Pointer)
    begin
      // Chaque thread lance son propre processus Python
      Results[Index] := AnalyzeDataWithPython(DataSets[Index]);

      Log(Format('Dataset %d traité par thread %d', [Index, ThreadIndex]), llDebug);
    end,
    0, High(DataSets), nil
  );

  // Tous les résultats sont maintenant disponibles
  for i := 0 to High(Results) do
    WriteLn('Résultat ', i, ' : ', Results[i]);
end;
```

---

## Conclusion

### Points clés à retenir

1. **Choix de la méthode d'intégration**
   - Python4Lazarus pour performances maximales
   - Ligne de commande pour simplicité et portabilité
   - Fichiers intermédiaires pour découplage total

2. **Gestion robuste**
   - Toujours vérifier que Python/R est installé
   - Gérer les timeouts et les erreurs
   - Valider les données échangées
   - Logger pour faciliter le débogage

3. **Optimisation**
   - Réutiliser les processus quand possible
   - Mettre en cache les résultats
   - Traiter en parallèle pour les gros volumes

4. **Portabilité Windows/Ubuntu**
   - Utiliser les chemins appropriés
   - Tester sur les deux plateformes
   - Gérer les différences d'encodage

### Ressources supplémentaires

**Pour Python4Lazarus :**
- GitHub : github.com/Alexey-T/Python-for-Lazarus
- Documentation : wiki.freepascal.org

**Pour R :**
- Documentation R Markdown : rmarkdown.rstudio.com
- CRAN : cran.r-project.org

**Bibliothèques utiles :**
- Python : NumPy, Pandas, scikit-learn, OpenCV, TensorFlow
- R : ggplot2, dplyr, caret, shiny

L'intégration de Python et R dans vos applications FreePascal vous ouvre un monde de possibilités pour le traitement de données, l'analyse statistique et le machine learning, tout en conservant les avantages de FreePascal pour l'interface utilisateur et les performances !

⏭️ [Bibliothèques scientifiques par OS](/16-traitement-donnees-calcul-scientifique/10-bibliotheques-scientifiques-par-os.md)
