🔝 Retour au [Sommaire](/SOMMAIRE.md)

# Tests cross-platform automatisés dans FreePascal/Lazarus

## Introduction

Lorsque vous développez une application qui doit fonctionner sur Windows et Linux/Ubuntu, il est essentiel de s'assurer qu'elle se comporte correctement sur chaque plateforme. Les tests automatisés vous permettent de vérifier rapidement que votre code fonctionne partout, sans avoir à tester manuellement sur chaque système. Ce tutoriel vous guidera dans la mise en place d'un système de tests efficace et portable.

## Comprendre les défis des tests multi-plateformes

### Pourquoi les tests multi-plateformes sont différents

Votre application peut se comporter différemment selon le système d'exploitation à cause de :

- **Chemins de fichiers** : `C:\Users\Jean\Documents` (Windows) vs `/home/jean/Documents` (Linux)
- **Fins de ligne** : CRLF sous Windows, LF sous Linux
- **Sensibilité à la casse** : Windows ignore la casse, Linux la respecte
- **Permissions** : Système très différent entre Windows et Linux
- **Comportement des API** : Les fonctions système peuvent réagir différemment
- **Interface graphique** : Les widgets peuvent avoir des comportements variés

### Types de tests nécessaires

1. **Tests unitaires** : Vérifient des fonctions individuelles
2. **Tests d'intégration** : Vérifient l'interaction entre composants
3. **Tests d'interface** : Vérifient le comportement de l'interface graphique
4. **Tests de compatibilité** : Vérifient les spécificités de chaque OS

## Configuration de FPCUnit

FPCUnit est le framework de tests intégré à FreePascal, similaire à JUnit pour Java ou NUnit pour .NET.

### Installation et configuration

FPCUnit est inclus avec FreePascal/Lazarus. Pour créer un projet de tests :

1. Dans Lazarus : **Fichier** → **Nouveau** → **Projet** → **Application de tests FPCUnit**
2. Choisissez l'interface (console ou graphique)
3. Lazarus crée automatiquement la structure de base

### Structure d'un projet de tests

```
MonProjet/
├── src/                      # Code source de l'application
│   ├── monunite.pas
│   └── utils.pas
├── tests/                    # Tests automatisés
│   ├── testmonunite.pas
│   ├── testutils.pas
│   └── testsuite.pas        # Suite de tests principale
└── MonProjet.lpi            # Projet principal
```

## Création de tests unitaires de base

### Structure d'une classe de test

```pascal
unit TestMonUnite;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FPCUnit, TestRegistry,
  MonUnite; // L'unité à tester

type
  TTestMonUnite = class(TTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    // Les méthodes de test doivent être dans la section published
    procedure TestAddition;
    procedure TestDivision;
    procedure TestCheminFichier;
  end;

implementation

procedure TTestMonUnite.SetUp;
begin
  // Code exécuté avant chaque test
  // Initialiser les variables, créer des objets, etc.
end;

procedure TTestMonUnite.TearDown;
begin
  // Code exécuté après chaque test
  // Nettoyer, libérer la mémoire, supprimer les fichiers temporaires
end;

procedure TTestMonUnite.TestAddition;
begin
  // Test simple
  AssertEquals('2 + 2 devrait égaler 4', 4, Addition(2, 2));
  AssertEquals('Nombres négatifs', -5, Addition(-2, -3));
end;

procedure TTestMonUnite.TestDivision;
begin
  // Test avec vérification d'exception
  AssertEquals('Division normale', 2.5, Division(5, 2), 0.001);

  // Vérifier qu'une exception est levée
  AssertException('Division par zéro', EDivByZero,
    @TestDivisionParZero);
end;

procedure TTestMonUnite.TestCheminFichier;
var
  Chemin: string;
begin
  // Test avec comportement différent selon l'OS
  {$IFDEF WINDOWS}
    Chemin := 'C:\Temp\test.txt';
    AssertEquals('Chemin Windows', 'C:\Temp\test.txt',
                 NormaliserChemin(Chemin));
  {$ENDIF}

  {$IFDEF UNIX}
    Chemin := '/tmp/test.txt';
    AssertEquals('Chemin Unix', '/tmp/test.txt',
                 NormaliserChemin(Chemin));
  {$ENDIF}
end;

initialization
  RegisterTest(TTestMonUnite);

end.
```

### Méthodes d'assertion disponibles

FPCUnit fournit de nombreuses méthodes pour vérifier vos résultats :

```pascal
// Assertions de base
AssertTrue('Message', Condition);
AssertFalse('Message', Condition);
AssertEquals('Message', Attendu, Obtenu);
AssertNull('Message', Objet);
AssertNotNull('Message', Objet);
AssertSame('Message', Objet1, Objet2);

// Assertions pour les nombres flottants (avec tolérance)
AssertEquals('Message', 3.14, Pi, 0.01);

// Assertions pour les chaînes
AssertEquals('Chaînes identiques', 'Hello', MaFonction());

// Vérification d'exceptions
AssertException('Message', ExceptionClass, @MethodeQuiLeve);
```

## Tests spécifiques à chaque plateforme

### Utilisation de la compilation conditionnelle

```pascal
unit TestPlateforme;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FPCUnit, TestRegistry;

type
  TTestPlateforme = class(TTestCase)
  published
    procedure TestCheminsSysteme;
    procedure TestPermissions;
    procedure TestProcessus;
  end;

implementation

procedure TTestPlateforme.TestCheminsSysteme;
var
  CheminTemp: string;
  CheminHome: string;
begin
  {$IFDEF WINDOWS}
    CheminTemp := GetEnvironmentVariable('TEMP');
    AssertTrue('TEMP existe sous Windows', CheminTemp <> '');
    AssertTrue('Format chemin Windows', Pos(':\', CheminTemp) > 0);

    CheminHome := GetEnvironmentVariable('USERPROFILE');
    AssertTrue('USERPROFILE existe', CheminHome <> '');
  {$ENDIF}

  {$IFDEF UNIX}
    CheminTemp := '/tmp';
    AssertTrue('Dossier /tmp existe', DirectoryExists(CheminTemp));

    CheminHome := GetEnvironmentVariable('HOME');
    AssertTrue('HOME existe sous Unix', CheminHome <> '');
    AssertTrue('Format chemin Unix', CheminHome[1] = '/');
  {$ENDIF}
end;

procedure TTestPlateforme.TestPermissions;
var
  NomFichier: string;
  Fichier: TextFile;
begin
  // Créer un fichier temporaire
  NomFichier := GetTempFileName;
  AssignFile(Fichier, NomFichier);
  Rewrite(Fichier);
  WriteLn(Fichier, 'Test');
  CloseFile(Fichier);

  try
    {$IFDEF WINDOWS}
      // Sous Windows, tester les attributs
      AssertTrue('Fichier lisible', FileIsReadable(NomFichier));
      FileSetAttr(NomFichier, faReadOnly);
      AssertFalse('Fichier non modifiable', FileIsWritable(NomFichier));
      FileSetAttr(NomFichier, 0); // Retirer readonly
    {$ENDIF}

    {$IFDEF UNIX}
      // Sous Unix, tester les permissions chmod
      AssertTrue('Fichier lisible', FileIsReadable(NomFichier));
      FpChmod(NomFichier, &444); // Lecture seule
      AssertFalse('Fichier non modifiable', FileIsWritable(NomFichier));
      FpChmod(NomFichier, &644); // Permissions normales
    {$ENDIF}
  finally
    DeleteFile(NomFichier);
  end;
end;

procedure TTestPlateforme.TestProcessus;
var
  Output: string;
begin
  {$IFDEF WINDOWS}
    // Tester une commande Windows
    AssertTrue('Commande dir fonctionne',
               ExecuterCommande('cmd /c dir', Output));
    AssertTrue('Output non vide', Length(Output) > 0);
  {$ENDIF}

  {$IFDEF UNIX}
    // Tester une commande Unix
    AssertTrue('Commande ls fonctionne',
               ExecuterCommande('ls', Output));
    AssertTrue('Output non vide', Length(Output) > 0);
  {$ENDIF}
end;

initialization
  RegisterTest(TTestPlateforme);

end.
```

## Tests d'interface graphique

### Tests de formulaires et composants

```pascal
unit TestInterface;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FPCUnit, TestRegistry, Forms, Controls,
  FormPrincipal; // Le formulaire à tester

type
  TTestInterface = class(TTestCase)
  private
    FForm: TFormPrincipal;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCreationFormulaire;
    procedure TestBoutons;
    procedure TestMenus;
    procedure TestRedimensionnement;
  end;

implementation

procedure TTestInterface.SetUp;
begin
  // Créer le formulaire pour les tests
  Application.Initialize;
  FForm := TFormPrincipal.Create(nil);
end;

procedure TTestInterface.TearDown;
begin
  // Libérer le formulaire
  FForm.Free;
  FForm := nil;
end;

procedure TTestInterface.TestCreationFormulaire;
begin
  AssertNotNull('Formulaire créé', FForm);
  AssertEquals('Titre correct', 'Mon Application', FForm.Caption);

  // Vérifier la taille selon la plateforme
  {$IFDEF WINDOWS}
    AssertTrue('Largeur Windows', FForm.Width >= 800);
  {$ENDIF}

  {$IFDEF UNIX}
    // Linux peut avoir des contraintes différentes
    AssertTrue('Largeur Linux', FForm.Width >= 640);
  {$ENDIF}
end;

procedure TTestInterface.TestBoutons;
begin
  AssertNotNull('Bouton OK existe', FForm.BtnOK);
  AssertTrue('Bouton OK visible', FForm.BtnOK.Visible);
  AssertTrue('Bouton OK activé', FForm.BtnOK.Enabled);

  // Simuler un clic
  FForm.BtnOK.Click;

  // Vérifier le résultat
  AssertEquals('Action effectuée', 'OK', FForm.DernierAction);
end;

procedure TTestInterface.TestMenus;
begin
  AssertNotNull('Menu principal existe', FForm.MainMenu);
  AssertTrue('Menu Fichier existe', FForm.MenuFichier.Count > 0);

  // Tester un élément de menu
  AssertNotNull('Menu Ouvrir existe', FForm.MenuOuvrir);
  AssertTrue('Raccourci correct', FForm.MenuOuvrir.ShortCut <> 0);

  // Vérifier les raccourcis selon l'OS
  {$IFDEF WINDOWS}
    // Ctrl+O sous Windows
    AssertEquals('Raccourci Windows', 'Ctrl+O',
                 ShortCutToText(FForm.MenuOuvrir.ShortCut));
  {$ENDIF}
end;

procedure TTestInterface.TestRedimensionnement;
var
  AncienneLargeur: Integer;
begin
  AncienneLargeur := FForm.Width;

  // Redimensionner
  FForm.Width := 1024;
  Application.ProcessMessages; // Traiter les événements

  AssertEquals('Nouvelle largeur', 1024, FForm.Width);

  // Vérifier que les composants s'adaptent
  if FForm.Panel1.Align = alClient then
    AssertEquals('Panel redimensionné', FForm.Width,
                 FForm.Panel1.Width);
end;

initialization
  RegisterTest(TTestInterface);

end.
```

## Organisation des suites de tests

### Création d'une suite de tests principale

```pascal
unit TestSuite;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FPCUnit, TestRegistry,
  // Importer tous vos modules de tests
  TestMonUnite,
  TestPlateforme,
  TestInterface,
  TestBaseDonnees,
  TestReseau;

type
  TTestSuitePrincipale = class(TTestSuite)
  public
    class function Suite: TTestSuite;
  end;

implementation

class function TTestSuitePrincipale.Suite: TTestSuite;
var
  TestSuite: TTestSuite;
begin
  TestSuite := TTestSuite.Create('Tests Complets Application');

  // Ajouter les catégories de tests
  TestSuite.AddTest(TTestSuite.Create('Tests Unitaires'));
  TestSuite.AddTest(TTestMonUnite);

  TestSuite.AddTest(TTestSuite.Create('Tests Plateforme'));
  TestSuite.AddTest(TTestPlateforme);

  // Tests conditionnels selon l'OS
  {$IFDEF WINDOWS}
    TestSuite.AddTest(TTestSuite.Create('Tests Windows'));
    TestSuite.AddTest(TTestWindows);
  {$ENDIF}

  {$IFDEF UNIX}
    TestSuite.AddTest(TTestSuite.Create('Tests Unix'));
    TestSuite.AddTest(TTestUnix);
  {$ENDIF}

  // Tests d'interface seulement si pas en mode console
  if not IsConsole then
  begin
    TestSuite.AddTest(TTestSuite.Create('Tests Interface'));
    TestSuite.AddTest(TTestInterface);
  end;

  Result := TestSuite;
end;

initialization
  RegisterTest(TTestSuitePrincipale.Suite);

end.
```

## Exécution des tests

### Application console de tests

```pascal
program TestsConsole;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, FPCUnit, TestRegistry,
  ConsoleTestRunner,
  TestSuite; // Votre suite de tests

var
  Application: TTestRunner;

begin
  // Configurer les options
  DefaultFormat := fPlain; // ou fXML pour sortie XML
  DefaultRunAllTests := True;

  Application := TTestRunner.Create(nil);
  try
    Application.Initialize;
    Application.Title := 'Tests Multi-plateformes';
    Application.Run;
  finally
    Application.Free;
  end;
end.
```

### Application graphique de tests

```pascal
program TestsGUI;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner,
  TestSuite; // Votre suite de tests

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.
```

## Automatisation avec scripts

### Script de tests pour Windows (test.bat)

```batch
@echo off
echo ========================================
echo Tests automatises Windows
echo ========================================

REM Compiler les tests
fpc -Mobjfpc -Sh TestsConsole.pas

if %errorlevel% neq 0 (
    echo Erreur de compilation!
    exit /b 1
)

REM Executer les tests
TestsConsole.exe --format=xml --all > resultats_windows.xml

if %errorlevel% neq 0 (
    echo Des tests ont echoue!
    type resultats_windows.xml
    exit /b 1
)

echo Tous les tests sont passes!
exit /b 0
```

### Script de tests pour Linux (test.sh)

```bash
#!/bin/bash

echo "========================================"
echo "Tests automatisés Linux/Ubuntu"
echo "========================================"

# Compiler les tests
fpc -Mobjfpc -Sh TestsConsole.pas

if [ $? -ne 0 ]; then
    echo "Erreur de compilation!"
    exit 1
fi

# Exécuter les tests
./TestsConsole --format=xml --all > resultats_linux.xml

if [ $? -ne 0 ]; then
    echo "Des tests ont échoué!"
    cat resultats_linux.xml
    exit 1
fi

echo "Tous les tests sont passés!"
exit 0
```

## Intégration continue (CI)

### Configuration GitHub Actions

Créez `.github/workflows/tests.yml` :

```yaml
name: Tests Multi-plateformes

on:
  push:
    branches: [ main, develop ]
  pull_request:
    branches: [ main ]

jobs:
  test-windows:
    runs-on: windows-latest
    steps:
    - uses: actions/checkout@v2

    - name: Installer FreePascal
      run: |
        choco install freepascal

    - name: Compiler les tests
      run: |
        fpc -Mobjfpc -Sh tests/TestsConsole.pas

    - name: Exécuter les tests
      run: |
        cd tests
        TestsConsole.exe --format=plain --all

    - name: Upload résultats
      uses: actions/upload-artifact@v2
      with:
        name: resultats-windows
        path: tests/*.xml

  test-ubuntu:
    runs-on: ubuntu-latest
    steps:
    - uses: actions/checkout@v2

    - name: Installer FreePascal
      run: |
        sudo apt-get update
        sudo apt-get install -y fpc

    - name: Compiler les tests
      run: |
        fpc -Mobjfpc -Sh tests/TestsConsole.pas

    - name: Exécuter les tests
      run: |
        cd tests
        ./TestsConsole --format=plain --all

    - name: Upload résultats
      uses: actions/upload-artifact@v2
      with:
        name: resultats-ubuntu
        path: tests/*.xml
```

## Gestion des données de test

### Fichiers de test portables

```pascal
unit TestData;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TTestDataManager = class
  private
    FDataPath: string;
    function GetTestDataPath: string;
  public
    constructor Create;
    function GetTestFile(const AFileName: string): string;
    procedure CreateTestFile(const AFileName, AContent: string);
    procedure CleanupTestFiles;
  end;

implementation

constructor TTestDataManager.Create;
begin
  FDataPath := GetTestDataPath;
  ForceDirectories(FDataPath);
end;

function TTestDataManager.GetTestDataPath: string;
begin
  // Utiliser un dossier temporaire selon l'OS
  {$IFDEF WINDOWS}
    Result := GetEnvironmentVariable('TEMP') + '\TestData\';
  {$ENDIF}

  {$IFDEF UNIX}
    Result := '/tmp/TestData/';
  {$ENDIF}

  // S'assurer que le dossier existe
  if not DirectoryExists(Result) then
    CreateDir(Result);
end;

function TTestDataManager.GetTestFile(const AFileName: string): string;
begin
  Result := FDataPath + AFileName;
end;

procedure TTestDataManager.CreateTestFile(const AFileName, AContent: string);
var
  F: TextFile;
  FullPath: string;
begin
  FullPath := GetTestFile(AFileName);
  AssignFile(F, FullPath);
  Rewrite(F);
  try
    Write(F, AContent);
  finally
    CloseFile(F);
  end;
end;

procedure TTestDataManager.CleanupTestFiles;
var
  SearchRec: TSearchRec;
begin
  // Supprimer tous les fichiers de test
  if FindFirst(FDataPath + '*.*', faAnyFile, SearchRec) = 0 then
  begin
    repeat
      if (SearchRec.Attr and faDirectory) = 0 then
        DeleteFile(FDataPath + SearchRec.Name);
    until FindNext(SearchRec) <> 0;
    FindClose(SearchRec);
  end;

  // Supprimer le dossier
  RemoveDir(FDataPath);
end;

end.
```

## Tests de performance cross-platform

### Mesure des performances

```pascal
unit TestPerformance;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FPCUnit, TestRegistry, DateUtils;

type
  TTestPerformance = class(TTestCase)
  private
    FStartTime: TDateTime;
    procedure StartTimer;
    function StopTimer: Integer; // Retourne les millisecondes
  published
    procedure TestVitesseCalcul;
    procedure TestVitesseIO;
    procedure TestVitesseMemoire;
  end;

implementation

procedure TTestPerformance.StartTimer;
begin
  FStartTime := Now;
end;

function TTestPerformance.StopTimer: Integer;
begin
  Result := MilliSecondsBetween(Now, FStartTime);
end;

procedure TTestPerformance.TestVitesseCalcul;
var
  i, j: Integer;
  Temps: Integer;
  MaxTemps: Integer;
begin
  // Définir les limites selon l'OS
  {$IFDEF WINDOWS}
    MaxTemps := 1000; // 1 seconde max sur Windows
  {$ENDIF}

  {$IFDEF UNIX}
    MaxTemps := 800; // Linux généralement plus rapide
  {$ENDIF}

  StartTimer;

  // Calcul intensif
  for i := 1 to 1000000 do
    j := i * 2 + i div 3;

  Temps := StopTimer;

  AssertTrue(Format('Calcul en moins de %d ms (actuel: %d ms)',
             [MaxTemps, Temps]), Temps < MaxTemps);
end;

procedure TTestPerformance.TestVitesseIO;
var
  F: TextFile;
  i: Integer;
  NomFichier: string;
  Temps: Integer;
begin
  NomFichier := GetTempFileName;

  StartTimer;

  // Écriture
  AssignFile(F, NomFichier);
  Rewrite(F);
  try
    for i := 1 to 10000 do
      WriteLn(F, 'Ligne de test numéro ', i);
  finally
    CloseFile(F);
  end;

  // Lecture
  AssignFile(F, NomFichier);
  Reset(F);
  try
    while not Eof(F) do
      ReadLn(F);
  finally
    CloseFile(F);
  end;

  Temps := StopTimer;
  DeleteFile(NomFichier);

  // Les performances IO varient beaucoup selon l'OS
  {$IFDEF WINDOWS}
    AssertTrue('IO Windows < 2000ms', Temps < 2000);
  {$ENDIF}

  {$IFDEF UNIX}
    AssertTrue('IO Unix < 1500ms', Temps < 1500);
  {$ENDIF}
end;

procedure TTestPerformance.TestVitesseMemoire;
var
  Liste: TStringList;
  i: Integer;
  Temps: Integer;
begin
  StartTimer;

  Liste := TStringList.Create;
  try
    // Allocation et manipulation mémoire
    for i := 1 to 100000 do
      Liste.Add('Élément ' + IntToStr(i));

    Liste.Sort;
    Liste.Clear;
  finally
    Liste.Free;
  end;

  Temps := StopTimer;

  AssertTrue('Gestion mémoire < 500ms', Temps < 500);
end;

initialization
  RegisterTest(TTestPerformance);

end.
```

## Rapports de tests

### Génération de rapports HTML

```pascal
unit RapportTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FPCUnit, TestReport;

type
  TRapportHTML = class
  private
    FResultats: TTestResult;
    FNomFichier: string;
  public
    constructor Create(AResultats: TTestResult;
                      const ANomFichier: string);
    procedure Generer;
  end;

implementation

constructor TRapportHTML.Create(AResultats: TTestResult;
                                const ANomFichier: string);
begin
  FResultats := AResultats;
  FNomFichier := ANomFichier;
end;

procedure TRapportHTML.Generer;
var
  F: TextFile;
  i: Integer;
begin
  AssignFile(F, FNomFichier);
  Rewrite(F);
  try
    WriteLn(F, '<html>');
    WriteLn(F, '<head>');
    WriteLn(F, '<title>Rapport de Tests Multi-plateformes</title>');
    WriteLn(F, '<style>');
    WriteLn(F, 'body { font-family: Arial, sans-serif; }');
    WriteLn(F, '.success { color: green; }');
    WriteLn(F, '.failure { color: red; }');
    WriteLn(F, '.info { background: #f0f0f0; padding: 10px; }');
    WriteLn(F, '</style>');
    WriteLn(F, '</head>');
    WriteLn(F, '<body>');

    WriteLn(F, '<h1>Rapport de Tests</h1>');

    // Informations système
    WriteLn(F, '<div class="info">');
    WriteLn(F, '<h2>Environnement de test</h2>');
    {$IFDEF WINDOWS}
      WriteLn(F, '<p>Système : Windows</p>');
    {$ENDIF}
    {$IFDEF UNIX}
      WriteLn(F, '<p>Système : Linux/Unix</p>');
    {$ENDIF}
    WriteLn(F, '<p>Date : ', DateTimeToStr(Now), '</p>');
    WriteLn(F, '</div>');

    // Résumé
    WriteLn(F, '<h2>Résumé</h2>');
    WriteLn(F, '<ul>');
    WriteLn(F, '<li>Tests exécutés : ', FResultats.RunTests, '</li>');

    if FResultats.NumberOfFailures = 0 then
      WriteLn(F, '<li class="success">Échecs : 0</li>')
    else
      WriteLn(F, '<li class="failure">Échecs : ',
              FResultats.NumberOfFailures, '</li>');

    WriteLn(F, '<li>Erreurs : ', FResultats.NumberOfErrors, '</li>');
    WriteLn(F, '</ul>');

    // Détails des échecs
    if FResultats.NumberOfFailures > 0 then
    begin
      WriteLn(F, '<h2>Détails des échecs</h2>');
      WriteLn(F, '<ul>');
      for i := 0 to FResultats.Failures.Count - 1 do
      begin
        WriteLn(F, '<li class="failure">');
        WriteLn(F, FResultats.Failures[i].AsString);
        WriteLn(F, '</li>');
      end;
      WriteLn(F, '</ul>');
    end;

    WriteLn(F, '</body>');
    WriteLn(F, '</html>');
  finally
    CloseFile(F);
  end;
end;

end.
```

## Conseils et bonnes pratiques

### Organisation des tests

1. **Un fichier de test par unité** : Gardez une correspondance claire
2. **Noms explicites** : `TestCalculateurTaxe` plutôt que `Test1`
3. **Tests indépendants** : Chaque test doit pouvoir s'exécuter seul
4. **Nettoyage systématique** : Utilisez `TearDown` pour nettoyer

### Tests efficaces

```pascal
procedure TMonTest.TestExempleComplet;
var
  Resultat: Double;
  FichierTest: string;
begin
  // Arrange (Préparer)
  FichierTest := CreateTestFile('donnees.txt');

  try
    // Act (Agir)
    Resultat := CalculerDepuisFichier(FichierTest);

    // Assert (Vérifier)
    AssertEquals('Résultat attendu', 42.0, Resultat, 0.01);
  finally
    // Cleanup (Nettoyer)
    DeleteFile(FichierTest);
  end;
end;
```

### Gestion des différences OS

```pascal
procedure TTestOS.TestComportementSpecifique;
begin
  {$IFDEF WINDOWS}
    RunTestWindows;
  {$ELSE}
    {$IFDEF UNIX}
      RunTestUnix;
    {$ELSE}
      Ignore('Plateforme non supportée');
    {$ENDIF}
  {$ENDIF}
end;

procedure TTestOS.RunTestWindows;
begin
  // Tests spécifiques Windows
  AssertTrue('Service Windows', WindowsServiceExists('Spooler'));
end;

procedure TTestOS.RunTestUnix;
begin
  // Tests spécifiques Unix
  AssertTrue('Daemon Unix', UnixDaemonExists('cron'));
end;
```

## Débogage des tests qui échouent

### Techniques de diagnostic

```pascal
procedure TTestDebug.TestAvecDiagnostic;
var
  Valeur: Integer;
  Message: string;
begin
  // Ajouter des informations de contexte
  Valeur := CalculerQuelqueChose();

  // Message détaillé en cas d'échec
  Message := Format('Échec du calcul. Valeur obtenue: %d, ' +
                    'Valeur attendue: 100, OS: %s, ' +
                    'Version FPC: %s',
                    [Valeur,
                     {$IFDEF WINDOWS}'Windows'{$ELSE}'Unix'{$ENDIF},
                     {$I %FPCVERSION%}]);

  AssertEquals(Message, 100, Valeur);

  // Logging pour debug
  if IsDebugMode then
  begin
    WriteLn('DEBUG: Valeur calculée = ', Valeur);
    WriteLn('DEBUG: Mémoire utilisée = ', GetHeapStatus.TotalAllocated);
  end;
end;
```

### Création d'un système de logging pour les tests

```pascal
unit TestLogger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TLogLevel = (llDebug, llInfo, llWarning, llError);

  TTestLogger = class
  private
    FLogFile: TextFile;
    FLogFileName: string;
    FEnabled: Boolean;
    FLogLevel: TLogLevel;
  public
    constructor Create(const AFileName: string);
    destructor Destroy; override;
    procedure Log(ALevel: TLogLevel; const AMessage: string);
    procedure LogPlatform(const AMessage: string);
    procedure LogMemory;
    procedure LogException(E: Exception);
    property Enabled: Boolean read FEnabled write FEnabled;
    property LogLevel: TLogLevel read FLogLevel write FLogLevel;
  end;

var
  TestLog: TTestLogger;

implementation

constructor TTestLogger.Create(const AFileName: string);
begin
  FLogFileName := AFileName;
  FEnabled := True;
  FLogLevel := llDebug;

  // Créer le fichier de log
  AssignFile(FLogFile, FLogFileName);
  Rewrite(FLogFile);

  // En-tête du log
  WriteLn(FLogFile, '===========================================');
  WriteLn(FLogFile, 'Log de tests - ', DateTimeToStr(Now));
  WriteLn(FLogFile, 'Plateforme: ',
          {$IFDEF WINDOWS}'Windows'{$ENDIF}
          {$IFDEF UNIX}'Unix/Linux'{$ENDIF});
  WriteLn(FLogFile, 'Version FPC: ', {$I %FPCVERSION%});
  WriteLn(FLogFile, '===========================================');
  WriteLn(FLogFile);

  Flush(FLogFile);
end;

destructor TTestLogger.Destroy;
begin
  if FEnabled then
  begin
    WriteLn(FLogFile);
    WriteLn(FLogFile, 'Fin du log - ', DateTimeToStr(Now));
    CloseFile(FLogFile);
  end;
  inherited;
end;

procedure TTestLogger.Log(ALevel: TLogLevel; const AMessage: string);
const
  LevelStr: array[TLogLevel] of string =
    ('DEBUG', 'INFO', 'WARNING', 'ERROR');
begin
  if not FEnabled then Exit;
  if ALevel < FLogLevel then Exit;

  WriteLn(FLogFile, Format('[%s] %s - %s',
          [LevelStr[ALevel], TimeToStr(Now), AMessage]));
  Flush(FLogFile);
end;

procedure TTestLogger.LogPlatform(const AMessage: string);
begin
  {$IFDEF WINDOWS}
    Log(llInfo, '[Windows] ' + AMessage);
  {$ENDIF}

  {$IFDEF UNIX}
    Log(llInfo, '[Unix] ' + AMessage);
  {$ENDIF}
end;

procedure TTestLogger.LogMemory;
var
  HeapStatus: THeapStatus;
begin
  HeapStatus := GetHeapStatus;
  Log(llDebug, Format('Mémoire - Allouée: %d, Libre: %d, Total: %d',
      [HeapStatus.TotalAllocated,
       HeapStatus.TotalFree,
       HeapStatus.CurrHeapSize]));
end;

procedure TTestLogger.LogException(E: Exception);
begin
  Log(llError, 'Exception: ' + E.ClassName + ' - ' + E.Message);
  {$IFDEF WINDOWS}
    if E is EOSError then
      Log(llError, 'Code erreur Windows: ' + IntToStr(GetLastError));
  {$ENDIF}
end;

initialization
  TestLog := TTestLogger.Create('test_debug.log');

finalization
  TestLog.Free;

end.
```

### Utilisation du logger dans les tests

```pascal
unit TestAvecLog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FPCUnit, TestRegistry, TestLogger;

type
  TTestAvecLog = class(TTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestOperationComplexe;
  end;

implementation

procedure TTestAvecLog.SetUp;
begin
  TestLog.Log(llInfo, 'Début du test: ' + Self.TestName);
  TestLog.LogMemory;
end;

procedure TTestAvecLog.TearDown;
begin
  TestLog.LogMemory;
  TestLog.Log(llInfo, 'Fin du test: ' + Self.TestName);
end;

procedure TTestAvecLog.TestOperationComplexe;
var
  i: Integer;
  Liste: TStringList;
begin
  TestLog.Log(llDebug, 'Création de la liste');
  Liste := TStringList.Create;
  try
    // Opération qui pourrait varier selon l'OS
    for i := 1 to 1000 do
    begin
      Liste.Add('Item ' + IntToStr(i));
      if i mod 100 = 0 then
        TestLog.Log(llDebug, Format('%d items ajoutés', [i]));
    end;

    TestLog.LogPlatform('Tri de la liste');
    Liste.Sort;

    AssertEquals('Nombre d''éléments', 1000, Liste.Count);
    TestLog.Log(llInfo, 'Test réussi');

  except
    on E: Exception do
    begin
      TestLog.LogException(E);
      raise; // Relancer l'exception pour que le test échoue
    end;
  end;

  Liste.Free;
end;

initialization
  RegisterTest(TTestAvecLog);

end.
```

## Mock Objects et isolation des tests

### Création de mocks simples

```pascal
unit MockObjects;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  // Interface pour le service réel
  IFileService = interface
    ['{A1B2C3D4-E5F6-7890-ABCD-EF1234567890}']
    function ReadFile(const AFileName: string): string;
    procedure WriteFile(const AFileName: string; const AContent: string);
    function FileExists(const AFileName: string): Boolean;
  end;

  // Mock pour les tests
  TMockFileService = class(TInterfacedObject, IFileService)
  private
    FFiles: TStringList;
    FReadCount: Integer;
    FWriteCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    // Implémentation de l'interface
    function ReadFile(const AFileName: string): string;
    procedure WriteFile(const AFileName: string; const AContent: string);
    function FileExists(const AFileName: string): Boolean;

    // Méthodes de vérification pour les tests
    property ReadCount: Integer read FReadCount;
    property WriteCount: Integer read FWriteCount;
    procedure Reset;
    procedure SetFileContent(const AFileName, AContent: string);
  end;

  // Service réel (pour production)
  TRealFileService = class(TInterfacedObject, IFileService)
  public
    function ReadFile(const AFileName: string): string;
    procedure WriteFile(const AFileName: string; const AContent: string);
    function FileExists(const AFileName: string): Boolean;
  end;

implementation

{ TMockFileService }

constructor TMockFileService.Create;
begin
  FFiles := TStringList.Create;
  FReadCount := 0;
  FWriteCount := 0;
end;

destructor TMockFileService.Destroy;
begin
  FFiles.Free;
  inherited;
end;

function TMockFileService.ReadFile(const AFileName: string): string;
var
  Index: Integer;
begin
  Inc(FReadCount);
  Index := FFiles.IndexOfName(AFileName);
  if Index >= 0 then
    Result := FFiles.ValueFromIndex[Index]
  else
    raise Exception.Create('Fichier non trouvé: ' + AFileName);
end;

procedure TMockFileService.WriteFile(const AFileName: string;
                                     const AContent: string);
begin
  Inc(FWriteCount);
  FFiles.Values[AFileName] := AContent;
end;

function TMockFileService.FileExists(const AFileName: string): Boolean;
begin
  Result := FFiles.IndexOfName(AFileName) >= 0;
end;

procedure TMockFileService.Reset;
begin
  FFiles.Clear;
  FReadCount := 0;
  FWriteCount := 0;
end;

procedure TMockFileService.SetFileContent(const AFileName, AContent: string);
begin
  FFiles.Values[AFileName] := AContent;
end;

{ TRealFileService }

function TRealFileService.ReadFile(const AFileName: string): string;
var
  F: TextFile;
  Line: string;
begin
  Result := '';
  if not FileExists(AFileName) then
    raise Exception.Create('Fichier non trouvé: ' + AFileName);

  AssignFile(F, AFileName);
  Reset(F);
  try
    while not Eof(F) do
    begin
      ReadLn(F, Line);
      Result := Result + Line + LineEnding;
    end;
  finally
    CloseFile(F);
  end;
end;

procedure TRealFileService.WriteFile(const AFileName: string;
                                    const AContent: string);
var
  F: TextFile;
begin
  AssignFile(F, AFileName);
  Rewrite(F);
  try
    Write(F, AContent);
  finally
    CloseFile(F);
  end;
end;

function TRealFileService.FileExists(const AFileName: string): Boolean;
begin
  Result := SysUtils.FileExists(AFileName);
end;

end.
```

### Utilisation des mocks dans les tests

```pascal
unit TestAvecMocks;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FPCUnit, TestRegistry, MockObjects;

type
  TProcesseurFichier = class
  private
    FFileService: IFileService;
  public
    constructor Create(AFileService: IFileService);
    function ProcesserFichier(const ANomFichier: string): string;
  end;

  TTestProcesseurFichier = class(TTestCase)
  private
    FMockService: TMockFileService;
    FProcesseur: TProcesseurFichier;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcesserFichierSimple;
    procedure TestProcesserFichierInexistant;
    procedure TestVerifierAppels;
  end;

implementation

{ TProcesseurFichier }

constructor TProcesseurFichier.Create(AFileService: IFileService);
begin
  FFileService := AFileService;
end;

function TProcesseurFichier.ProcesserFichier(const ANomFichier: string): string;
var
  Contenu: string;
begin
  if not FFileService.FileExists(ANomFichier) then
    Exit('Fichier introuvable');

  Contenu := FFileService.ReadFile(ANomFichier);

  // Traitement du contenu
  Result := UpperCase(Contenu);

  // Sauvegarder le résultat
  FFileService.WriteFile(ANomFichier + '.processed', Result);
end;

{ TTestProcesseurFichier }

procedure TTestProcesseurFichier.SetUp;
begin
  FMockService := TMockFileService.Create;
  FProcesseur := TProcesseurFichier.Create(FMockService);
end;

procedure TTestProcesseurFichier.TearDown;
begin
  FProcesseur.Free;
  // FMockService est libéré automatiquement (interface)
end;

procedure TTestProcesseurFichier.TestProcesserFichierSimple;
var
  Resultat: string;
begin
  // Préparer le mock
  FMockService.SetFileContent('test.txt', 'hello world');

  // Exécuter
  Resultat := FProcesseur.ProcesserFichier('test.txt');

  // Vérifier
  AssertEquals('Contenu transformé', 'HELLO WORLD', Resultat);
  AssertTrue('Fichier de sortie créé',
             FMockService.FileExists('test.txt.processed'));
end;

procedure TTestProcesseurFichier.TestProcesserFichierInexistant;
var
  Resultat: string;
begin
  // Le fichier n'existe pas dans le mock
  Resultat := FProcesseur.ProcesserFichier('inexistant.txt');

  AssertEquals('Message d''erreur', 'Fichier introuvable', Resultat);
end;

procedure TTestProcesseurFichier.TestVerifierAppels;
begin
  FMockService.SetFileContent('data.txt', 'test');

  FProcesseur.ProcesserFichier('data.txt');

  // Vérifier que les méthodes ont été appelées
  AssertEquals('Nombre de lectures', 1, FMockService.ReadCount);
  AssertEquals('Nombre d''écritures', 1, FMockService.WriteCount);
end;

initialization
  RegisterTest(TTestProcesseurFichier);

end.
```

## Tests de régression

### Système de snapshots

```pascal
unit TestSnapshots;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FPCUnit, TestRegistry, fpjson, jsonparser;

type
  TSnapshotManager = class
  private
    FSnapshotDir: string;
    function GetSnapshotPath(const AName: string): string;
  public
    constructor Create;
    procedure SaveSnapshot(const AName: string; const AData: string);
    function LoadSnapshot(const AName: string): string;
    function CompareWithSnapshot(const AName, AActual: string): Boolean;
    procedure UpdateSnapshot(const AName: string; const ANewData: string);
  end;

  TTestRegression = class(TTestCase)
  private
    FSnapshots: TSnapshotManager;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestFormatageJSON;
    procedure TestCalculComplexe;
    procedure TestSortieHTML;
  end;

implementation

{ TSnapshotManager }

constructor TSnapshotManager.Create;
begin
  {$IFDEF WINDOWS}
    FSnapshotDir := ExtractFilePath(ParamStr(0)) + 'snapshots\';
  {$ELSE}
    FSnapshotDir := ExtractFilePath(ParamStr(0)) + 'snapshots/';
  {$ENDIF}

  ForceDirectories(FSnapshotDir);
end;

function TSnapshotManager.GetSnapshotPath(const AName: string): string;
begin
  Result := FSnapshotDir + AName + '.snapshot';
end;

procedure TSnapshotManager.SaveSnapshot(const AName: string;
                                        const AData: string);
var
  F: TextFile;
begin
  AssignFile(F, GetSnapshotPath(AName));
  Rewrite(F);
  try
    Write(F, AData);
  finally
    CloseFile(F);
  end;
end;

function TSnapshotManager.LoadSnapshot(const AName: string): string;
var
  F: TextFile;
  Line: string;
begin
  Result := '';
  if not FileExists(GetSnapshotPath(AName)) then
    Exit;

  AssignFile(F, GetSnapshotPath(AName));
  Reset(F);
  try
    while not Eof(F) do
    begin
      ReadLn(F, Line);
      if Result <> '' then
        Result := Result + LineEnding;
      Result := Result + Line;
    end;
  finally
    CloseFile(F);
  end;
end;

function TSnapshotManager.CompareWithSnapshot(const AName,
                                              AActual: string): Boolean;
var
  Expected: string;
begin
  Expected := LoadSnapshot(AName);

  // Si pas de snapshot, le créer
  if Expected = '' then
  begin
    SaveSnapshot(AName, AActual);
    Result := True;
    WriteLn('Nouveau snapshot créé: ', AName);
  end
  else
    Result := Expected = AActual;
end;

procedure TSnapshotManager.UpdateSnapshot(const AName: string;
                                         const ANewData: string);
begin
  SaveSnapshot(AName, ANewData);
  WriteLn('Snapshot mis à jour: ', AName);
end;

{ TTestRegression }

procedure TTestRegression.SetUp;
begin
  FSnapshots := TSnapshotManager.Create;
end;

procedure TTestRegression.TearDown;
begin
  FSnapshots.Free;
end;

procedure TTestRegression.TestFormatageJSON;
var
  JSON: TJSONObject;
  Output: string;
begin
  // Créer un objet JSON
  JSON := TJSONObject.Create;
  try
    JSON.Add('nom', 'Test');
    JSON.Add('version', '1.0');
    JSON.Add('plateforme', {$IFDEF WINDOWS}'Windows'{$ELSE}'Unix'{$ENDIF});

    Output := JSON.FormatJSON;

    // Comparer avec le snapshot
    AssertTrue('JSON correspond au snapshot',
               FSnapshots.CompareWithSnapshot('json_output', Output));
  finally
    JSON.Free;
  end;
end;

procedure TTestRegression.TestCalculComplexe;
var
  Resultat: string;
  i: Integer;
begin
  // Calcul qui doit toujours donner le même résultat
  Resultat := '';
  for i := 1 to 10 do
    Resultat := Resultat + Format('Ligne %d: %d' + LineEnding,
                                  [i, i * i]);

  AssertTrue('Calcul correspond au snapshot',
             FSnapshots.CompareWithSnapshot('calcul_complexe', Resultat));
end;

procedure TTestRegression.TestSortieHTML;
var
  HTML: TStringList;
begin
  HTML := TStringList.Create;
  try
    HTML.Add('<html>');
    HTML.Add('<head><title>Test</title></head>');
    HTML.Add('<body>');
    HTML.Add('<h1>Plateforme: ' +
             {$IFDEF WINDOWS}'Windows'{$ELSE}'Unix'{$ENDIF} +
             '</h1>');
    HTML.Add('<p>Version: 1.0</p>');
    HTML.Add('</body>');
    HTML.Add('</html>');

    AssertTrue('HTML correspond au snapshot',
               FSnapshots.CompareWithSnapshot('sortie_html', HTML.Text));
  finally
    HTML.Free;
  end;
end;

initialization
  RegisterTest(TTestRegression);

end.
```

## Tests de compatibilité multi-versions

### Tests pour différentes versions d'OS

```pascal
unit TestCompatibilite;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FPCUnit, TestRegistry
  {$IFDEF WINDOWS}, Windows{$ENDIF}
  {$IFDEF UNIX}, BaseUnix{$ENDIF};

type
  TTestCompatibilite = class(TTestCase)
  private
    function GetOSVersion: string;
    function IsWindows10OrNewer: Boolean;
    function IsUbuntu20OrNewer: Boolean;
  published
    procedure TestVersionOS;
    procedure TestFonctionnalitesModernes;
    procedure TestRetrocompatibilite;
  end;

implementation

function TTestCompatibilite.GetOSVersion: string;
{$IFDEF WINDOWS}
var
  OSVersionInfo: TOSVersionInfo;
begin
  OSVersionInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  if GetVersionEx(OSVersionInfo) then
    Result := Format('Windows %d.%d Build %d',
              [OSVersionInfo.dwMajorVersion,
               OSVersionInfo.dwMinorVersion,
               OSVersionInfo.dwBuildNumber])
  else
    Result := 'Windows (version inconnue)';
end;
{$ELSE}
var
  F: TextFile;
  Line: string;
begin
  Result := 'Unix/Linux';

  // Essayer de lire la version depuis /etc/os-release
  if FileExists('/etc/os-release') then
  begin
    AssignFile(F, '/etc/os-release');
    Reset(F);
    try
      while not Eof(F) do
      begin
        ReadLn(F, Line);
        if Pos('PRETTY_NAME=', Line) = 1 then
        begin
          Result := Copy(Line, 13, Length(Line) - 13);
          Result := StringReplace(Result, '"', '', [rfReplaceAll]);
          Break;
        end;
      end;
    finally
      CloseFile(F);
    end;
  end;
end;
{$ENDIF}

function TTestCompatibilite.IsWindows10OrNewer: Boolean;
{$IFDEF WINDOWS}
var
  OSVersionInfo: TOSVersionInfo;
begin
  OSVersionInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  if GetVersionEx(OSVersionInfo) then
    Result := OSVersionInfo.dwMajorVersion >= 10
  else
    Result := False;
end;
{$ELSE}
begin
  Result := False;
end;
{$ENDIF}

function TTestCompatibilite.IsUbuntu20OrNewer: Boolean;
{$IFDEF UNIX}
var
  F: TextFile;
  Line: string;
  Version: string;
begin
  Result := False;

  if FileExists('/etc/os-release') then
  begin
    AssignFile(F, '/etc/os-release');
    Reset(F);
    try
      while not Eof(F) do
      begin
        ReadLn(F, Line);
        if Pos('VERSION_ID=', Line) = 1 then
        begin
          Version := Copy(Line, 12, Length(Line) - 12);
          Version := StringReplace(Version, '"', '', [rfReplaceAll]);

          // Vérifier si c'est Ubuntu 20.04 ou plus récent
          if Pos('20', Version) = 1 then
            Result := True
          else if Pos('21', Version) = 1 then
            Result := True
          else if Pos('22', Version) = 1 then
            Result := True;

          Break;
        end;
      end;
    finally
      CloseFile(F);
    end;
  end;
end;
{$ELSE}
begin
  Result := False;
end;
{$ENDIF}

procedure TTestCompatibilite.TestVersionOS;
var
  Version: string;
begin
  Version := GetOSVersion;

  WriteLn('OS détecté: ', Version);

  {$IFDEF WINDOWS}
    AssertTrue('Windows détecté', Pos('Windows', Version) > 0);
  {$ENDIF}

  {$IFDEF UNIX}
    AssertTrue('Unix/Linux détecté',
               (Pos('Linux', Version) > 0) or (Pos('Ubuntu', Version) > 0));
  {$ENDIF}
end;

procedure TTestCompatibilite.TestFonctionnalitesModernes;
begin
  {$IFDEF WINDOWS}
    if IsWindows10OrNewer then
    begin
      // Tester les fonctionnalités Windows 10+
      WriteLn('Windows 10+ détecté - Tests modernes activés');
      // Par exemple, vérifier le support des notifications modernes
      AssertTrue('Support notifications Windows 10', True);
    end
    else
    begin
      WriteLn('Windows ancien - Tests de compatibilité');
      Ignore('Fonctionnalités Windows 10+ non disponibles');
    end;
  {$ENDIF}

  {$IFDEF UNIX}
    if IsUbuntu20OrNewer then
    begin
      WriteLn('Ubuntu 20+ détecté - Tests modernes activés');
      // Tester les fonctionnalités Ubuntu 20+
      AssertTrue('Systemd disponible',
                 FileExists('/bin/systemctl'));
    end
    else
    begin
      WriteLn('Version Ubuntu ancienne');
    end;
  {$ENDIF}
end;

procedure TTestCompatibilite.TestRetrocompatibilite;
begin
  // Tester que les fonctionnalités de base fonctionnent partout

  // Test de création de fichier (doit marcher partout)
  AssertTrue('Création fichier temporaire',
             Length(GetTempFileName) > 0);

  // Test des variables d'environnement basiques
  {$IFDEF WINDOWS}
    AssertTrue('Variable PATH existe',
               GetEnvironmentVariable('PATH') <> '');
  {$ENDIF}

  {$IFDEF UNIX}
    AssertTrue('Variable HOME existe',
               GetEnvironmentVariable('HOME') <> '');
  {$ENDIF}
end;

initialization
  RegisterTest(TTestCompatibilite);

end.
```

## Benchmarking et tests de charge

### Framework de benchmark portable

```pascal
unit TestBenchmark;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FPCUnit, TestRegistry, DateUtils;

type
  TBenchmarkResult = record
    NomTest: string;
    Iterations: Integer;
    TempsMoyen: Double;  // en millisecondes
    TempsMin: Double;
    TempsMax: Double;
    EcartType: Double;
  end;

  TBenchmarkRunner = class
  private
    FResultats: array of TBenchmarkResult;
    function CalculerEcartType(const Temps: array of Double): Double;
  public
    function RunBenchmark(const ANom: string;
                         AProc: TProcedure;
                         AIterations: Integer = 1000): TBenchmarkResult;
    procedure SaveResults(const AFileName: string);
    procedure CompareWithBaseline(const ABaseline: string);
  end;

  TTestPerformanceCrossPlatform = class(TTestCase)
  private
    FBenchmark: TBenchmarkRunner;
    procedure BenchStringConcat;
    procedure BenchListSort;
    procedure BenchFileIO;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestPerformanceStrings;
    procedure TestPerformanceListes;
    procedure TestPerformanceIO;
  end;

implementation

{ TBenchmarkRunner }

function TBenchmarkRunner.CalculerEcartType(const Temps: array of Double): Double;
var
  Moyenne, Somme: Double;
  i: Integer;
begin
  if Length(Temps) = 0 then
    Exit(0);

  // Calculer la moyenne
  Somme := 0;
  for i := 0 to High(Temps) do
    Somme := Somme + Temps[i];
  Moyenne := Somme / Length(Temps);

  // Calculer l'écart-type
  Somme := 0;
  for i := 0 to High(Temps) do
    Somme := Somme + Sqr(Temps[i] - Moyenne);

  Result := Sqrt(Somme / Length(Temps));
end;

function TBenchmarkRunner.RunBenchmark(const ANom: string;
                                       AProc: TProcedure;
                                       AIterations: Integer): TBenchmarkResult;
var
  i, WarmUp: Integer;
  StartTime, EndTime: TDateTime;
  Temps: array of Double;
  TempTotal: Double;
begin
  Result.NomTest := ANom;
  Result.Iterations := AIterations;

  // Warm-up (10% des itérations)
  WarmUp := AIterations div 10;
  for i := 1 to WarmUp do
    AProc();

  // Mesures réelles
  SetLength(Temps, AIterations);
  for i := 0 to AIterations - 1 do
  begin
    StartTime := Now;
    AProc();
    EndTime := Now;
    Temps[i] := MilliSecondsBetween(EndTime, StartTime);
  end;

  // Calcul des statistiques
  Result.TempsMin := Temps[0];
  Result.TempsMax := Temps[0];
  TempTotal := 0;

  for i := 0 to High(Temps) do
  begin
    TempTotal := TempTotal + Temps[i];
    if Temps[i] < Result.TempsMin then
      Result.TempsMin := Temps[i];
    if Temps[i] > Result.TempsMax then
      Result.TempsMax := Temps[i];
  end;

  Result.TempsMoyen := TempTotal / AIterations;
  Result.EcartType := CalculerEcartType(Temps);

  // Ajouter aux résultats
  SetLength(FResultats, Length(FResultats) + 1);
  FResultats[High(FResultats)] := Result;
end;

procedure TBenchmarkRunner.SaveResults(const AFileName: string);
var
  F: TextFile;
  i: Integer;
begin
  AssignFile(F, AFileName);
  Rewrite(F);
  try
    WriteLn(F, 'Résultats de Benchmark - ', DateTimeToStr(Now));
    WriteLn(F, 'Plateforme: ',
            {$IFDEF WINDOWS}'Windows'{$ELSE}'Linux/Unix'{$ENDIF});
    WriteLn(F, '================================================');
    WriteLn(F);

    for i := 0 to High(FResultats) do
    begin
      with FResultats[i] do
      begin
        WriteLn(F, 'Test: ', NomTest);
        WriteLn(F, '  Itérations: ', Iterations);
        WriteLn(F, '  Temps moyen: ', TempsMoyen:0:3, ' ms');
        WriteLn(F, '  Temps min: ', TempsMin:0:3, ' ms');
        WriteLn(F, '  Temps max: ', TempsMax:0:3, ' ms');
        WriteLn(F, '  Écart-type: ', EcartType:0:3, ' ms');
        WriteLn(F);
      end;
    end;

    // Format CSV pour analyse
    WriteLn(F);
    WriteLn(F, 'Format CSV:');
    WriteLn(F, 'Test;Iterations;Moyenne;Min;Max;EcartType');
    for i := 0 to High(FResultats) do
    begin
      with FResultats[i] do
        WriteLn(F, Format('%s;%d;%.3f;%.3f;%.3f;%.3f',
                [NomTest, Iterations, TempsMoyen,
                 TempsMin, TempsMax, EcartType]));
    end;
  finally
    CloseFile(F);
  end;
end;

procedure TBenchmarkRunner.CompareWithBaseline(const ABaseline: string);
var
  F: TextFile;
  Line: string;
  BaselineResults: array of TBenchmarkResult;
  i, j: Integer;
  Difference: Double;
begin
  if not FileExists(ABaseline) then
  begin
    WriteLn('Pas de baseline trouvée, sauvegarde actuelle comme baseline');
    SaveResults(ABaseline);
    Exit;
  end;

  // Charger les résultats de référence
  // (Code simplifié - en réalité, il faudrait parser le CSV)

  WriteLn('Comparaison avec baseline:');
  WriteLn('==========================');

  for i := 0 to High(FResultats) do
  begin
    // Rechercher le test correspondant dans la baseline
    for j := 0 to High(BaselineResults) do
    begin
      if BaselineResults[j].NomTest = FResultats[i].NomTest then
      begin
        Difference := ((FResultats[i].TempsMoyen -
                       BaselineResults[j].TempsMoyen) /
                       BaselineResults[j].TempsMoyen) * 100;

        Write(FResultats[i].NomTest, ': ');
        if Difference > 10 then
          WriteLn('RÉGRESSION - ', Difference:0:1, '% plus lent')
        else if Difference < -10 then
          WriteLn('AMÉLIORATION - ', Abs(Difference):0:1, '% plus rapide')
        else
          WriteLn('Stable (', Difference:0:1, '%)');

        Break;
      end;
    end;
  end;
end;

{ TTestPerformanceCrossPlatform }

procedure TTestPerformanceCrossPlatform.SetUp;
begin
  FBenchmark := TBenchmarkRunner.Create;
end;

procedure TTestPerformanceCrossPlatform.TearDown;
var
  NomFichier: string;
begin
  // Sauvegarder les résultats
  {$IFDEF WINDOWS}
    NomFichier := 'benchmark_windows.txt';
  {$ELSE}
    NomFichier := 'benchmark_linux.txt';
  {$ENDIF}

  FBenchmark.SaveResults(NomFichier);
  FBenchmark.CompareWithBaseline('benchmark_baseline.txt');

  FBenchmark.Free;
end;

procedure TTestPerformanceCrossPlatform.BenchStringConcat;
var
  S: string;
  i: Integer;
begin
  S := '';
  for i := 1 to 100 do
    S := S + IntToStr(i);
end;

procedure TTestPerformanceCrossPlatform.BenchListSort;
var
  Liste: TStringList;
  i: Integer;
begin
  Liste := TStringList.Create;
  try
    for i := 1 to 100 do
      Liste.Add(IntToStr(Random(1000)));
    Liste.Sort;
  finally
    Liste.Free;
  end;
end;

procedure TTestPerformanceCrossPlatform.BenchFileIO;
var
  NomFichier: string;
  F: TextFile;
  i: Integer;
begin
  NomFichier := GetTempFileName;

  AssignFile(F, NomFichier);
  Rewrite(F);
  try
    for i := 1 to 100 do
      WriteLn(F, 'Ligne ', i);
  finally
    CloseFile(F);
  end;

  DeleteFile(NomFichier);
end;

procedure TTestPerformanceCrossPlatform.TestPerformanceStrings;
var
  Result: TBenchmarkResult;
begin
  Result := FBenchmark.RunBenchmark('Concaténation strings',
                                    @BenchStringConcat, 100);

  // Vérifier que les performances sont acceptables
  {$IFDEF WINDOWS}
    AssertTrue('Performance strings Windows', Result.TempsMoyen < 10);
  {$ELSE}
    AssertTrue('Performance strings Linux', Result.TempsMoyen < 8);
  {$ENDIF}
end;

procedure TTestPerformanceCrossPlatform.TestPerformanceListes;
var
  Result: TBenchmarkResult;
begin
  Result := FBenchmark.RunBenchmark('Tri de listes',
                                    @BenchListSort, 100);

  AssertTrue('Performance tri acceptable', Result.TempsMoyen < 20);
end;

procedure TTestPerformanceCrossPlatform.TestPerformanceIO;
var
  Result: TBenchmarkResult;
begin
  Result := FBenchmark.RunBenchmark('I/O fichiers',
                                    @BenchFileIO, 50);

  // L'I/O est généralement plus lent sur Windows
  {$IFDEF WINDOWS}
    AssertTrue('Performance I/O Windows', Result.TempsMoyen < 50);
  {$ELSE}
    AssertTrue('Performance I/O Linux', Result.TempsMoyen < 30);
  {$ENDIF}
end;

initialization
  RegisterTest(TTestPerformanceCrossPlatform);

end.
```

## Intégration avec Jenkins

### Configuration Jenkins pour tests multi-plateformes

Créez un fichier `Jenkinsfile` à la racine de votre projet :

```groovy
pipeline {
    agent none

    stages {
        stage('Tests Multi-Plateformes') {
            parallel {
                stage('Tests Windows') {
                    agent { label 'windows' }
                    steps {
                        checkout scm

                        script {
                            echo 'Compilation sur Windows'
                            bat '''
                                fpc -Mobjfpc -Sh -B tests/TestsConsole.pas
                                if %errorlevel% neq 0 exit /b %errorlevel%
                            '''

                            echo 'Exécution des tests Windows'
                            bat '''
                                cd tests
                                TestsConsole.exe --format=xml --all > results_windows.xml
                            '''
                        }
                    }
                    post {
                        always {
                            junit 'tests/results_windows.xml'
                            archiveArtifacts artifacts: 'tests/*.log',
                                           allowEmptyArchive: true
                        }
                    }
                }

                stage('Tests Linux') {
                    agent { label 'linux' }
                    steps {
                        checkout scm

                        script {
                            echo 'Compilation sur Linux'
                            sh '''
                                fpc -Mobjfpc -Sh -B tests/TestsConsole.pas
                                if [ $? -ne 0 ]; then exit 1; fi
                            '''

                            echo 'Exécution des tests Linux'
                            sh '''
                                cd tests
                                ./TestsConsole --format=xml --all > results_linux.xml
                            '''
                        }
                    }
                    post {
                        always {
                            junit 'tests/results_linux.xml'
                            archiveArtifacts artifacts: 'tests/*.log',
                                           allowEmptyArchive: true
                        }
                    }
                }
            }
        }

        stage('Analyse des résultats') {
            agent any
            steps {
                script {
                    echo 'Génération du rapport consolidé'
                    // Script pour merger les résultats
                }
            }
        }
    }

    post {
        failure {
            emailext (
                subject: "Tests échoués: ${env.JOB_NAME} - ${env.BUILD_NUMBER}",
                body: "Les tests ont échoué. Voir ${env.BUILD_URL}",
                to: 'equipe-dev@exemple.com'
            )
        }
        success {
            echo 'Tous les tests sont passés!'
        }
    }
}
```

## Générateur de rapports HTML complet

### Classe de génération de rapports avancée

```pascal
unit ReportGenerator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FPCUnit, TestReport, fpjson, jsonparser;

type
  TTestReportGenerator = class
  private
    FResults: TTestResult;
    FPlatform: string;
    FOutputDir: string;

    function GetPlatformInfo: TJSONObject;
    function GetTestResultsJSON: TJSONArray;
    procedure GenerateCSS(const AFileName: string);
    procedure GenerateJS(const AFileName: string);
    procedure GenerateCharts(const AFileName: string);
  public
    constructor Create(AResults: TTestResult; const AOutputDir: string);
    procedure GenerateFullReport;
    procedure GenerateJSONReport(const AFileName: string);
    procedure GenerateSummary(const AFileName: string);
    procedure MergeReports(const AReportFiles: array of string;
                          const AOutputFile: string);
  end;

implementation

constructor TTestReportGenerator.Create(AResults: TTestResult;
                                       const AOutputDir: string);
begin
  FResults := AResults;
  FOutputDir := AOutputDir;

  {$IFDEF WINDOWS}
    FPlatform := 'Windows';
  {$ELSE}
    FPlatform := 'Linux/Unix';
  {$ENDIF}

  ForceDirectories(FOutputDir);
end;

function TTestReportGenerator.GetPlatformInfo: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.Add('platform', FPlatform);
  Result.Add('fpc_version', {$I %FPCVERSION%});
  Result.Add('date', DateTimeToStr(Now));
  Result.Add('cpu', {$I %FPCTARGETCPU%});
  Result.Add('os', {$I %FPCTARGETOS%});
end;

function TTestReportGenerator.GetTestResultsJSON: TJSONArray;
var
  i: Integer;
  TestObj: TJSONObject;
begin
  Result := TJSONArray.Create;

  // Ajouter chaque résultat de test
  for i := 0 to FResults.Testsuite.Tests.Count - 1 do
  begin
    TestObj := TJSONObject.Create;
    TestObj.Add('name', FResults.Testsuite.Tests[i].TestName);
    TestObj.Add('passed', FResults.Testsuite.Tests[i].Passed);
    TestObj.Add('time', FResults.Testsuite.Tests[i].ElapsedTime);

    if not FResults.Testsuite.Tests[i].Passed then
      TestObj.Add('error', FResults.Testsuite.Tests[i].ErrorMessage);

    Result.Add(TestObj);
  end;
end;

procedure TTestReportGenerator.GenerateCSS(const AFileName: string);
var
  F: TextFile;
begin
  AssignFile(F, AFileName);
  Rewrite(F);
  try
    WriteLn(F, '/* Styles pour le rapport de tests */');
    WriteLn(F, 'body {');
    WriteLn(F, '    font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;');
    WriteLn(F, '    margin: 0;');
    WriteLn(F, '    padding: 20px;');
    WriteLn(F, '    background: #f5f5f5;');
    WriteLn(F, '}');
    WriteLn(F, '.container {');
    WriteLn(F, '    max-width: 1200px;');
    WriteLn(F, '    margin: 0 auto;');
    WriteLn(F, '    background: white;');
    WriteLn(F, '    border-radius: 8px;');
    WriteLn(F, '    box-shadow: 0 2px 4px rgba(0,0,0,0.1);');
    WriteLn(F, '    padding: 30px;');
    WriteLn(F, '}');
    WriteLn(F, '.header {');
    WriteLn(F, '    border-bottom: 2px solid #e0e0e0;');
    WriteLn(F, '    padding-bottom: 20px;');
    WriteLn(F, '    margin-bottom: 30px;');
    WriteLn(F, '}');
    WriteLn(F, '.platform-badge {');
    WriteLn(F, '    display: inline-block;');
    WriteLn(F, '    padding: 5px 15px;');
    WriteLn(F, '    border-radius: 20px;');
    WriteLn(F, '    font-size: 14px;');
    WriteLn(F, '    font-weight: bold;');
    WriteLn(F, '}');
    WriteLn(F, '.platform-windows { background: #0078d4; color: white; }');
    WriteLn(F, '.platform-linux { background: #dd4814; color: white; }');
    WriteLn(F, '.stats {');
    WriteLn(F, '    display: grid;');
    WriteLn(F, '    grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));');
    WriteLn(F, '    gap: 20px;');
    WriteLn(F, '    margin: 30px 0;');
    WriteLn(F, '}');
    WriteLn(F, '.stat-card {');
    WriteLn(F, '    padding: 20px;');
    WriteLn(F, '    border-radius: 8px;');
    WriteLn(F, '    text-align: center;');
    WriteLn(F, '}');
    WriteLn(F, '.stat-value {');
    WriteLn(F, '    font-size: 36px;');
    WriteLn(F, '    font-weight: bold;');
    WriteLn(F, '    margin: 10px 0;');
    WriteLn(F, '}');
    WriteLn(F, '.stat-label {');
    WriteLn(F, '    color: #666;');
    WriteLn(F, '    font-size: 14px;');
    WriteLn(F, '}');
    WriteLn(F, '.success { background: #d4edda; color: #155724; }');
    WriteLn(F, '.failure { background: #f8d7da; color: #721c24; }');
    WriteLn(F, '.warning { background: #fff3cd; color: #856404; }');
    WriteLn(F, '.test-list {');
    WriteLn(F, '    margin-top: 30px;');
    WriteLn(F, '}');
    WriteLn(F, '.test-item {');
    WriteLn(F, '    padding: 15px;');
    WriteLn(F, '    margin: 10px 0;');
    WriteLn(F, '    border-left: 4px solid #ddd;');
    WriteLn(F, '    background: #fafafa;');
    WriteLn(F, '}');
    WriteLn(F, '.test-passed { border-color: #28a745; }');
    WriteLn(F, '.test-failed { border-color: #dc3545; }');
    WriteLn(F, '.test-name { font-weight: bold; }');
    WriteLn(F, '.test-time { color: #666; font-size: 12px; }');
    WriteLn(F, '.error-message {');
    WriteLn(F, '    margin-top: 10px;');
    WriteLn(F, '    padding: 10px;');
    WriteLn(F, '    background: #fff;');
    WriteLn(F, '    border-radius: 4px;');
    WriteLn(F, '    font-family: monospace;');
    WriteLn(F, '    font-size: 12px;');
    WriteLn(F, '}');
  finally
    CloseFile(F);
  end;
end;

procedure TTestReportGenerator.GenerateJS(const AFileName: string);
var
  F: TextFile;
begin
  AssignFile(F, AFileName);
  Rewrite(F);
  try
    WriteLn(F, '// JavaScript pour le rapport de tests');
    WriteLn(F, 'document.addEventListener("DOMContentLoaded", function() {');
    WriteLn(F, '    // Filtrage des tests');
    WriteLn(F, '    const filterButtons = document.querySelectorAll(".filter-btn");');
    WriteLn(F, '    const testItems = document.querySelectorAll(".test-item");');
    WriteLn(F, '    ');
    WriteLn(F, '    filterButtons.forEach(btn => {');
    WriteLn(F, '        btn.addEventListener("click", function() {');
    WriteLn(F, '            const filter = this.dataset.filter;');
    WriteLn(F, '            ');
    WriteLn(F, '            filterButtons.forEach(b => b.classList.remove("active"));');
    WriteLn(F, '            this.classList.add("active");');
    WriteLn(F, '            ');
    WriteLn(F, '            testItems.forEach(item => {');
    WriteLn(F, '                if (filter === "all") {');
    WriteLn(F, '                    item.style.display = "block";');
    WriteLn(F, '                } else if (filter === "passed" && item.classList.contains("test-passed")) {');
    WriteLn(F, '                    item.style.display = "block";');
    WriteLn(F, '                } else if (filter === "failed" && item.classList.contains("test-failed")) {');
    WriteLn(F, '                    item.style.display = "block";');
    WriteLn(F, '                } else {');
    WriteLn(F, '                    item.style.display = "none";');
    WriteLn(F, '                }');
    WriteLn(F, '            });');
    WriteLn(F, '        });');
    WriteLn(F, '    });');
    WriteLn(F, '    ');
    WriteLn(F, '    // Temps d''exécution total');
    WriteLn(F, '    const times = document.querySelectorAll(".test-time");');
    WriteLn(F, '    let totalTime = 0;');
    WriteLn(F, '    times.forEach(t => {');
    WriteLn(F, '        totalTime += parseFloat(t.dataset.time || 0);');
    WriteLn(F, '    });');
    WriteLn(F, '    document.getElementById("total-time").textContent = totalTime.toFixed(2) + " ms";');
    WriteLn(F, '});');
  finally
    CloseFile(F);
  end;
end;

procedure TTestReportGenerator.GenerateFullReport;
var
  F: TextFile;
  i: Integer;
  PassedCount, FailedCount: Integer;
  HTMLFile, CSSFile, JSFile: string;
begin
  HTMLFile := FOutputDir + PathDelim + 'report.html';
  CSSFile := FOutputDir + PathDelim + 'report.css';
  JSFile := FOutputDir + PathDelim + 'report.js';

  // Générer les fichiers CSS et JS
  GenerateCSS(CSSFile);
  GenerateJS(JSFile);

  // Calculer les statistiques
  PassedCount := 0;
  FailedCount := 0;
  for i := 0 to FResults.Count - 1 do
  begin
    if FResults.Test[i].TestPassed then
      Inc(PassedCount)
    else
      Inc(FailedCount);
  end;

  // Générer le HTML principal
  AssignFile(F, HTMLFile);
  Rewrite(F);
  try
    WriteLn(F, '<!DOCTYPE html>');
    WriteLn(F, '<html lang="fr">');
    WriteLn(F, '<head>');
    WriteLn(F, '    <meta charset="UTF-8">');
    WriteLn(F, '    <meta name="viewport" content="width=device-width, initial-scale=1.0">');
    WriteLn(F, '    <title>Rapport de Tests - ', FPlatform, '</title>');
    WriteLn(F, '    <link rel="stylesheet" href="report.css">');
    WriteLn(F, '</head>');
    WriteLn(F, '<body>');
    WriteLn(F, '    <div class="container">');

    // En-tête
    WriteLn(F, '        <div class="header">');
    WriteLn(F, '            <h1>Rapport de Tests Multi-plateformes</h1>');
    WriteLn(F, '            <span class="platform-badge platform-',
            LowerCase(StringReplace(FPlatform, '/', '-', [rfReplaceAll])), '">',
            FPlatform, '</span>');
    WriteLn(F, '            <p>Généré le ', DateTimeToStr(Now), '</p>');
    WriteLn(F, '        </div>');

    // Statistiques
    WriteLn(F, '        <div class="stats">');
    WriteLn(F, '            <div class="stat-card">');
    WriteLn(F, '                <div class="stat-label">Tests totaux</div>');
    WriteLn(F, '                <div class="stat-value">', FResults.Count, '</div>');
    WriteLn(F, '            </div>');
    WriteLn(F, '            <div class="stat-card success">');
    WriteLn(F, '                <div class="stat-label">Réussis</div>');
    WriteLn(F, '                <div class="stat-value">', PassedCount, '</div>');
    WriteLn(F, '            </div>');
    WriteLn(F, '            <div class="stat-card failure">');
    WriteLn(F, '                <div class="stat-label">Échecs</div>');
    WriteLn(F, '                <div class="stat-value">', FailedCount, '</div>');
    WriteLn(F, '            </div>');
    WriteLn(F, '            <div class="stat-card">');
    WriteLn(F, '                <div class="stat-label">Temps total</div>');
    WriteLn(F, '                <div class="stat-value" id="total-time">-</div>');
    WriteLn(F, '            </div>');
    WriteLn(F, '        </div>');

    // Filtres
    WriteLn(F, '        <div class="filters">');
    WriteLn(F, '            <button class="filter-btn active" data-filter="all">Tous</button>');
    WriteLn(F, '            <button class="filter-btn" data-filter="passed">Réussis</button>');
    WriteLn(F, '            <button class="filter-btn" data-filter="failed">Échecs</button>');
    WriteLn(F, '        </div>');

    // Liste des tests
    WriteLn(F, '        <div class="test-list">');
    WriteLn(F, '            <h2>Détails des tests</h2>');

    for i := 0 to FResults.Count - 1 do
    begin
      if FResults.Test[i].TestPassed then
        WriteLn(F, '            <div class="test-item test-passed">')
      else
        WriteLn(F, '            <div class="test-item test-failed">');

      WriteLn(F, '                <div class="test-name">',
              FResults.Test[i].TestSuiteName, '::',
              FResults.Test[i].TestName, '</div>');
      WriteLn(F, '                <div class="test-time" data-time="',
              FResults.Test[i].ElapsedTime, '">',
              'Temps: ', FResults.Test[i].ElapsedTime, ' ms</div>');

      if not FResults.Test[i].TestPassed then
      begin
        WriteLn(F, '                <div class="error-message">');
        WriteLn(F, FResults.Test[i].Message);
        WriteLn(F, '                </div>');
      end;

      WriteLn(F, '            </div>');
    end;

    WriteLn(F, '        </div>');
    WriteLn(F, '    </div>');
    WriteLn(F, '    <script src="report.js"></script>');
    WriteLn(F, '</body>');
    WriteLn(F, '</html>');
  finally
    CloseFile(F);
  end;

  WriteLn('Rapport généré: ', HTMLFile);
end;

procedure TTestReportGenerator.GenerateJSONReport(const AFileName: string);
var
  RootObj: TJSONObject;
  ResultsArray: TJSONArray;
begin
  RootObj := TJSONObject.Create;
  try
    RootObj.Add('platform_info', GetPlatformInfo);
    RootObj.Add('summary', TJSONObject.Create);
    RootObj.Objects['summary'].Add('total', FResults.Count);
    RootObj.Objects['summary'].Add('passed', FResults.NumberOfRuns -
                                             FResults.NumberOfFailures);
    RootObj.Objects['summary'].Add('failed', FResults.NumberOfFailures);
    RootObj.Objects['summary'].Add('errors', FResults.NumberOfErrors);

    RootObj.Add('tests', GetTestResultsJSON);

    // Sauvegarder le JSON
    with TStringList.Create do
    try
      Text := RootObj.FormatJSON;
      SaveToFile(AFileName);
    finally
      Free;
    end;
  finally
    RootObj.Free;
  end;
end;

procedure TTestReportGenerator.GenerateSummary(const AFileName: string);
var
  F: TextFile;
begin
  AssignFile(F, AFileName);
  Rewrite(F);
  try
    WriteLn(F, 'RÉSUMÉ DES TESTS');
    WriteLn(F, '================');
    WriteLn(F);
    WriteLn(F, 'Plateforme: ', FPlatform);
    WriteLn(F, 'Date: ', DateTimeToStr(Now));
    WriteLn(F);
    WriteLn(F, 'Résultats:');
    WriteLn(F, '  Total: ', FResults.Count);
    WriteLn(F, '  Réussis: ', FResults.Count - FResults.NumberOfFailures);
    WriteLn(F, '  Échecs: ', FResults.NumberOfFailures);
    WriteLn(F, '  Erreurs: ', FResults.NumberOfErrors);
    WriteLn(F);

    if FResults.NumberOfFailures > 0 then
    begin
      WriteLn(F, 'Tests échoués:');
      // Lister les tests échoués
    end;
  finally
    CloseFile(F);
  end;
end;

procedure TTestReportGenerator.MergeReports(const AReportFiles: array of string;
                                           const AOutputFile: string);
begin
  // Fusionner plusieurs rapports JSON en un seul
  // Utile pour consolider les résultats Windows et Linux
end;

end.
```

## Conclusion et recommandations finales

### Checklist pour tests cross-platform réussis

1. **Organisation du code**
   - Séparer les tests unitaires, d'intégration et de performance
   - Un fichier de test par module de code
   - Utiliser des noms de tests descriptifs

2. **Gestion des différences OS**
   - Utiliser la compilation conditionnelle intelligemment
   - Abstraire les opérations système dans des interfaces
   - Tester les chemins, permissions et comportements spécifiques

3. **Automatisation**
   - Scripts de build pour chaque plateforme
   - Intégration CI/CD (GitHub Actions, Jenkins, GitLab CI)
   - Rapports automatiques après chaque commit

4. **Performance**
   - Benchmarks réguliers
   - Comparaison avec des baselines
   - Surveillance des régressions de performance
   - Tests de charge adaptés à chaque OS
   - Optimisation basée sur les métriques réelles

5. **Qualité des tests**
   - Viser une couverture de code > 80%
   - Tests indépendants et répétables
   - Nettoyage systématique après chaque test
   - Documentation des cas de test complexes

6. **Maintenance**
   - Mise à jour régulière des snapshots
   - Révision périodique des tests obsolètes
   - Adaptation aux nouvelles versions d'OS
   - Archivage des rapports pour suivi historique

## Structure recommandée d'un projet avec tests

### Organisation complète des dossiers

```
MonProjet/
├── src/                          # Code source principal
│   ├── core/                     # Logique métier
│   │   ├── models/
│   │   ├── services/
│   │   └── utils/
│   ├── ui/                       # Interface utilisateur
│   │   ├── forms/
│   │   └── components/
│   └── platform/                 # Code spécifique OS
│       ├── windows/
│       └── linux/
├── tests/                        # Tous les tests
│   ├── unit/                     # Tests unitaires
│   │   ├── core/
│   │   └── utils/
│   ├── integration/              # Tests d'intégration
│   ├── performance/              # Tests de performance
│   ├── fixtures/                 # Données de test
│   ├── mocks/                    # Mock objects
│   ├── snapshots/                # Snapshots pour régression
│   └── reports/                  # Rapports générés
├── scripts/                      # Scripts d'automatisation
│   ├── build_windows.bat
│   ├── build_linux.sh
│   ├── test_windows.bat
│   ├── test_linux.sh
│   └── deploy.sh
├── ci/                          # Configuration CI/CD
│   ├── Jenkinsfile
│   ├── .github/workflows/
│   └── .gitlab-ci.yml
├── docs/                        # Documentation
│   └── tests/                   # Documentation des tests
└── README.md
```

## Script maître pour l'exécution complète des tests

### Script Python pour orchestration multi-plateforme

```python
#!/usr/bin/env python3
"""
test_runner.py - Orchestrateur de tests multi-plateformes
"""

import os
import sys
import platform
import subprocess
import json
import datetime
from pathlib import Path

class TestOrchestrator:
    def __init__(self):
        self.platform = platform.system()
        self.project_root = Path(__file__).parent.parent
        self.test_dir = self.project_root / 'tests'
        self.report_dir = self.test_dir / 'reports'
        self.results = []

        # Créer le dossier de rapports
        self.report_dir.mkdir(exist_ok=True)

    def compile_tests(self):
        """Compile les tests pour la plateforme actuelle"""
        print(f"🔨 Compilation des tests pour {self.platform}...")

        compile_cmd = [
            'fpc',
            '-Mobjfpc',
            '-Sh',
            '-B',
            str(self.test_dir / 'TestsConsole.pas')
        ]

        try:
            result = subprocess.run(compile_cmd,
                                  capture_output=True,
                                  text=True)
            if result.returncode != 0:
                print(f"❌ Erreur de compilation:\n{result.stderr}")
                return False
            print("✅ Compilation réussie")
            return True
        except Exception as e:
            print(f"❌ Erreur: {e}")
            return False

    def run_unit_tests(self):
        """Exécute les tests unitaires"""
        print("\n📋 Exécution des tests unitaires...")
        return self._run_test_suite('unit')

    def run_integration_tests(self):
        """Exécute les tests d'intégration"""
        print("\n🔗 Exécution des tests d'intégration...")
        return self._run_test_suite('integration')

    def run_performance_tests(self):
        """Exécute les tests de performance"""
        print("\n⚡ Exécution des tests de performance...")
        return self._run_test_suite('performance')

    def _run_test_suite(self, suite_type):
        """Exécute une suite de tests spécifique"""
        if self.platform == 'Windows':
            exe_name = 'TestsConsole.exe'
        else:
            exe_name = './TestsConsole'

        test_cmd = [
            str(self.test_dir / exe_name),
            f'--suite={suite_type}',
            '--format=json',
            '--verbose'
        ]

        try:
            result = subprocess.run(test_cmd,
                                  capture_output=True,
                                  text=True,
                                  cwd=str(self.test_dir))

            # Parser les résultats JSON
            if result.stdout:
                try:
                    test_results = json.loads(result.stdout)
                    self.results.append({
                        'suite': suite_type,
                        'results': test_results,
                        'timestamp': datetime.datetime.now().isoformat()
                    })

                    # Afficher le résumé
                    passed = test_results.get('passed', 0)
                    failed = test_results.get('failed', 0)
                    total = passed + failed

                    if failed == 0:
                        print(f"  ✅ {passed}/{total} tests réussis")
                    else:
                        print(f"  ❌ {failed}/{total} tests échoués")

                    return failed == 0
                except json.JSONDecodeError:
                    print(f"  ⚠️ Impossible de parser les résultats")
                    return False

            return result.returncode == 0

        except Exception as e:
            print(f"  ❌ Erreur: {e}")
            return False

    def generate_report(self):
        """Génère un rapport HTML consolidé"""
        print("\n📊 Génération du rapport...")

        report_file = self.report_dir / f"report_{self.platform.lower()}_{datetime.datetime.now().strftime('%Y%m%d_%H%M%S')}.html"

        html_content = self._generate_html_report()

        with open(report_file, 'w', encoding='utf-8') as f:
            f.write(html_content)

        print(f"✅ Rapport généré: {report_file}")
        return report_file

    def _generate_html_report(self):
        """Génère le contenu HTML du rapport"""
        total_passed = sum(r['results'].get('passed', 0) for r in self.results)
        total_failed = sum(r['results'].get('failed', 0) for r in self.results)

        html = f"""<!DOCTYPE html>
<html>
<head>
    <title>Rapport de Tests - {self.platform}</title>
    <meta charset="UTF-8">
    <style>
        body {{
            font-family: Arial, sans-serif;
            margin: 40px;
            background: #f5f5f5;
        }}
        .header {{
            background: white;
            padding: 20px;
            border-radius: 8px;
            margin-bottom: 20px;
            box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }}
        h1 {{
            color: #333;
            margin: 0;
        }}
        .summary {{
            display: flex;
            gap: 20px;
            margin-top: 20px;
        }}
        .summary-card {{
            flex: 1;
            padding: 15px;
            border-radius: 8px;
            text-align: center;
        }}
        .passed {{ background: #d4edda; color: #155724; }}
        .failed {{ background: #f8d7da; color: #721c24; }}
        .total {{ background: #cce5ff; color: #004085; }}
        .suite-section {{
            background: white;
            padding: 20px;
            margin: 20px 0;
            border-radius: 8px;
            box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }}
        table {{
            width: 100%;
            border-collapse: collapse;
            margin-top: 15px;
        }}
        th, td {{
            padding: 10px;
            text-align: left;
            border-bottom: 1px solid #ddd;
        }}
        th {{
            background: #f8f9fa;
            font-weight: bold;
        }}
        .test-passed {{ color: green; }}
        .test-failed {{ color: red; }}
    </style>
</head>
<body>
    <div class="header">
        <h1>Rapport de Tests Multi-plateformes</h1>
        <p>Plateforme: <strong>{self.platform}</strong></p>
        <p>Date: {datetime.datetime.now().strftime('%Y-%m-%d %H:%M:%S')}</p>

        <div class="summary">
            <div class="summary-card total">
                <h3>Total</h3>
                <p style="font-size: 24px; font-weight: bold;">{total_passed + total_failed}</p>
            </div>
            <div class="summary-card passed">
                <h3>Réussis</h3>
                <p style="font-size: 24px; font-weight: bold;">{total_passed}</p>
            </div>
            <div class="summary-card failed">
                <h3>Échecs</h3>
                <p style="font-size: 24px; font-weight: bold;">{total_failed}</p>
            </div>
        </div>
    </div>
"""

        for result in self.results:
            suite_name = result['suite'].capitalize()
            suite_results = result['results']

            html += f"""
    <div class="suite-section">
        <h2>Tests {suite_name}</h2>
        <table>
            <thead>
                <tr>
                    <th>Test</th>
                    <th>Statut</th>
                    <th>Temps (ms)</th>
                    <th>Message</th>
                </tr>
            </thead>
            <tbody>
"""

            # Ajouter les détails des tests (si disponibles)
            if 'tests' in suite_results:
                for test in suite_results['tests']:
                    status_class = 'test-passed' if test.get('passed') else 'test-failed'
                    status_text = '✅ Réussi' if test.get('passed') else '❌ Échoué'

                    html += f"""
                <tr>
                    <td>{test.get('name', 'N/A')}</td>
                    <td class="{status_class}">{status_text}</td>
                    <td>{test.get('time', 'N/A')}</td>
                    <td>{test.get('message', '-')}</td>
                </tr>
"""

            html += """
            </tbody>
        </table>
    </div>
"""

        html += """
</body>
</html>
"""
        return html

    def run_all(self):
        """Exécute tous les tests et génère le rapport"""
        print(f"🚀 Démarrage des tests sur {self.platform}")
        print("=" * 50)

        # Compilation
        if not self.compile_tests():
            print("\n❌ Échec de la compilation. Arrêt.")
            return False

        # Exécution des différentes suites
        all_passed = True
        all_passed &= self.run_unit_tests()
        all_passed &= self.run_integration_tests()
        all_passed &= self.run_performance_tests()

        # Génération du rapport
        report_file = self.generate_report()

        # Résumé final
        print("\n" + "=" * 50)
        if all_passed:
            print("✅ TOUS LES TESTS SONT PASSÉS!")
        else:
            print("❌ DES TESTS ONT ÉCHOUÉ")

        print(f"\n📄 Rapport disponible: {report_file}")

        # Ouvrir le rapport dans le navigateur (optionnel)
        if platform.system() == 'Windows':
            os.startfile(report_file)
        elif platform.system() == 'Darwin':  # macOS
            subprocess.run(['open', report_file])
        elif platform.system() == 'Linux':
            subprocess.run(['xdg-open', report_file])

        return all_passed


def main():
    orchestrator = TestOrchestrator()
    success = orchestrator.run_all()

    # Code de sortie pour CI/CD
    sys.exit(0 if success else 1)


if __name__ == '__main__':
    main()
```

## Makefile pour automatisation complète

### Makefile multi-plateforme

```makefile
# Makefile pour tests cross-platform FreePascal/Lazarus

# Détection de l'OS
ifeq ($(OS),Windows_NT)
    PLATFORM := Windows
    EXE_EXT := .exe
    RM := del /Q
    MKDIR := mkdir
    PATH_SEP := \\
else
    PLATFORM := $(shell uname -s)
    EXE_EXT :=
    RM := rm -f
    MKDIR := mkdir -p
    PATH_SEP := /
endif

# Configuration
FPC := fpc
FPC_FLAGS := -Mobjfpc -Sh -B -FE./bin -FU./units
TEST_DIR := tests
BIN_DIR := bin
REPORT_DIR := $(TEST_DIR)$(PATH_SEP)reports

# Fichiers
TEST_CONSOLE := $(BIN_DIR)$(PATH_SEP)TestsConsole$(EXE_EXT)
TEST_GUI := $(BIN_DIR)$(PATH_SEP)TestsGUI$(EXE_EXT)

# Cibles principales
.PHONY: all clean test test-console test-gui test-unit test-integration \
        test-performance report help

all: test-console test-gui

help:
	@echo "Commandes disponibles:"
	@echo "  make all              - Compile tous les tests"
	@echo "  make test             - Exécute tous les tests"
	@echo "  make test-unit        - Exécute les tests unitaires"
	@echo "  make test-integration - Exécute les tests d'intégration"
	@echo "  make test-performance - Exécute les tests de performance"
	@echo "  make report           - Génère le rapport HTML"
	@echo "  make clean            - Nettoie les fichiers générés"
	@echo "  make ci               - Mode CI/CD (tous les tests + rapport)"

# Création des dossiers
$(BIN_DIR):
	$(MKDIR) $(BIN_DIR)

$(REPORT_DIR):
	$(MKDIR) $(REPORT_DIR)

# Compilation
test-console: $(BIN_DIR)
	$(FPC) $(FPC_FLAGS) $(TEST_DIR)$(PATH_SEP)TestsConsole.pas

test-gui: $(BIN_DIR)
	$(FPC) $(FPC_FLAGS) $(TEST_DIR)$(PATH_SEP)TestsGUI.pas

# Exécution des tests
test: test-console
	@echo "========================================="
	@echo "Exécution des tests sur $(PLATFORM)"
	@echo "========================================="
	$(TEST_CONSOLE) --all --format=plain

test-unit: test-console
	@echo "Tests unitaires..."
	$(TEST_CONSOLE) --suite=unit --format=xml > $(REPORT_DIR)$(PATH_SEP)unit.xml

test-integration: test-console
	@echo "Tests d'intégration..."
	$(TEST_CONSOLE) --suite=integration --format=xml > $(REPORT_DIR)$(PATH_SEP)integration.xml

test-performance: test-console
	@echo "Tests de performance..."
	$(TEST_CONSOLE) --suite=performance --format=xml > $(REPORT_DIR)$(PATH_SEP)performance.xml

# Génération du rapport
report: $(REPORT_DIR) test-unit test-integration test-performance
	@echo "Génération du rapport..."
	python3 scripts/generate_report.py

# Mode CI/CD
ci: clean all test report
	@echo "========================================="
	@echo "Build CI/CD terminé"
	@echo "Plateforme: $(PLATFORM)"
	@echo "========================================="

# Nettoyage
clean:
	$(RM) $(BIN_DIR)$(PATH_SEP)*$(EXE_EXT)
	$(RM) $(TEST_DIR)$(PATH_SEP)*.ppu
	$(RM) $(TEST_DIR)$(PATH_SEP)*.o
	$(RM) $(REPORT_DIR)$(PATH_SEP)*.*

# Installation des dépendances (pour CI/CD)
install-deps:
ifeq ($(PLATFORM),Windows)
	@echo "Installation des dépendances Windows..."
	# Commandes spécifiques Windows
else
	@echo "Installation des dépendances Linux/Unix..."
	sudo apt-get update
	sudo apt-get install -y fpc lazarus
endif
```

## Modèle de documentation des tests

### Template pour documenter les tests

```markdown
# Documentation des Tests - [Nom du Module]

## Vue d'ensemble
Description du module testé et de la stratégie de test adoptée.

## Tests unitaires

### Test: `TestCalculateurTaxe`
**Objectif:** Vérifier le calcul correct des taxes selon les régions

**Cas de test:**
- Calcul avec taux standard (20%)
- Calcul avec taux réduit (5.5%)
- Gestion des arrondis
- Cas limites (montant négatif, zéro)

**Spécificités cross-platform:**
- Windows: Utilise les paramètres régionaux système
- Linux: Utilise la locale configurée

**Données de test:**
```
Montant: 100.00 EUR
Taux: 20%
Résultat attendu: 120.00 EUR
```

### Test: `TestGestionFichiers`
**Objectif:** Vérifier les opérations sur les fichiers

**Différences OS:**
| Opération | Windows | Linux |
|-----------|---------|-------|
| Chemin temp | %TEMP% | /tmp |
| Séparateur | \ | / |
| Permissions | Attributs | chmod |

## Tests d'intégration

### Test: `TestConnexionBDD`
**Objectif:** Vérifier la connexion à différentes bases de données

**Configuration:**
- Windows: SQL Server, PostgreSQL via ODBC
- Linux: PostgreSQL natif, MySQL

**Timeout:** 30 secondes

## Tests de performance

### Benchmark: `BenchmarkTriListe`
**Métriques cibles:**
| Plateforme | Temps moyen | Temps max |
|------------|-------------|-----------|
| Windows | < 50ms | < 100ms |
| Linux | < 40ms | < 80ms |

**Volume de données:** 10,000 éléments

## Problèmes connus

### Windows
- Les tests de permissions peuvent échouer sans droits admin
- Antivirus peut ralentir les tests I/O

### Linux
- Tests GUI nécessitent un serveur X
- SELinux peut bloquer certaines opérations

## Maintenance

**Dernière mise à jour:** [Date]  
**Responsable:** [Nom]  
**Fréquence de révision:** Mensuelle  
```

## Ressources et références utiles

### Documentation officielle
- [FPCUnit Documentation](https://wiki.freepascal.org/FPCUnit)
- [Lazarus Testing](https://wiki.lazarus.freepascal.org/Testing)
- [FreePascal Cross-compilation](https://wiki.freepascal.org/Cross_compiling)

### Outils recommandés
1. **Couverture de code**: [FPCover](https://wiki.freepascal.org/FPCover)
2. **Analyse statique**: [Pascal Analyzer](http://www.peganza.com)
3. **Profiling Windows**: DProf, Intel VTune
4. **Profiling Linux**: gprof, Valgrind, perf

### Bonnes pratiques à retenir

1. **Isolement des tests**
   - Chaque test doit être indépendant
   - Utiliser SetUp et TearDown systématiquement
   - Nettoyer toutes les ressources

2. **Nommage cohérent**
   - `TestNomClasse` pour les classes de test
   - `TestNomMethode` pour les méthodes de test
   - Préfixes clairs: `Bench`, `Mock`, `Stub`

3. **Gestion des erreurs**
   - Toujours tester les cas d'erreur
   - Vérifier les exceptions attendues
   - Logger les erreurs pour diagnostic

4. **Performance**
   - Benchmarks reproductibles
   - Warm-up avant mesures
   - Plusieurs itérations pour moyenne

5. **Documentation**
   - Commenter les tests complexes
   - Documenter les dépendances OS
   - Maintenir un changelog des tests

## Exemple d'intégration complète

### Programme principal avec tests intégrés

```pascal
program MonApplicationAvecTests;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
    cthreads,
  {$ENDIF}
  Classes, SysUtils, Forms, Interfaces,
  // Unités de l'application
  FormPrincipal, MonModule,
  // Tests (seulement en mode debug)
  {$IFDEF DEBUG}
    FPCUnit, TestRegistry, ConsoleTestRunner,
    TestMonModule, TestInterface, TestPlateforme,
  {$ENDIF}
  ;

{$R *.res}

procedure RunTests;
{$IFDEF DEBUG}
var
  TestRunner: TTestRunner;
begin
  WriteLn('Mode DEBUG - Exécution des tests...');

  TestRunner := TTestRunner.Create(nil);
  try
    TestRunner.Initialize;
    TestRunner.Run;

    if TestRunner.HasErrors then
    begin
      WriteLn('❌ Des tests ont échoué!');
      Halt(1);
    end
    else
      WriteLn('✅ Tous les tests sont passés');
  finally
    TestRunner.Free;
  end;
end;
{$ELSE}
begin
  // Rien en mode release
end;
{$ENDIF}

begin
  // Exécuter les tests si demandé
  if (ParamCount > 0) and (ParamStr(1) = '--test') then
  begin
    RunTests;
    Exit;
  end;

  // Démarrage normal de l'application
  Application.Initialize;
  Application.Title := 'Mon Application';
  Application.CreateForm(TFormPrincipal, FormPrincipal);
  Application.Run;
end.
```

## Conclusion

Les tests cross-platform automatisés sont essentiels pour garantir la qualité et la fiabilité de vos applications FreePascal/Lazarus sur Windows et Linux/Ubuntu. En suivant les pratiques présentées dans ce tutoriel, vous pourrez :

- **Détecter rapidement** les problèmes spécifiques à chaque plateforme
- **Automatiser** la validation de votre code
- **Maintenir** une qualité constante
- **Documenter** le comportement attendu
- **Mesurer** les performances sur chaque OS

L'investissement initial dans la mise en place d'une infrastructure de tests solide sera rapidement rentabilisé par la réduction des bugs en production et la confiance accrue dans vos déploiements multi-plateformes.

N'oubliez pas que les tests ne sont pas une destination mais un voyage continu. Commencez petit, avec quelques tests critiques, puis étendez progressivement votre couverture. L'important est d'avoir une base solide sur laquelle construire et d'intégrer les tests dans votre flux de développement quotidien.

Bon développement et bons tests ! 🚀

⏭️ [Intégration native par plateforme](/05-developpement-multiplateforme-approfondi/07-integration-native-par-plateforme.md)
