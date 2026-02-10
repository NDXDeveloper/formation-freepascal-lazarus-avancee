🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 24.7 Outils de build personnalisés

## Introduction

Un **outil de build** (ou outil de construction) est un programme qui automatise le processus de compilation, d'assemblage et de déploiement de votre application. Au lieu de compiler manuellement chaque fichier, un outil de build orchestre toutes les étapes nécessaires pour créer votre logiciel final.

**Analogie simple :** Imaginez que vous construisez une maison. Au lieu de coordonner manuellement tous les artisans (maçons, plombiers, électriciens), vous engagez un chef de chantier qui gère tout le processus. Un outil de build est ce "chef de chantier" pour votre code.

---

## Pourquoi créer un outil de build personnalisé ?

### Avantages principaux

**1. Automatisation complète**
- Compiler pour plusieurs plateformes en une commande
- Exécuter les tests automatiquement
- Générer la documentation
- Créer les installateurs

**2. Reproductibilité**
- Garantir que chaque build est identique
- Éviter les erreurs "ça marche sur ma machine"
- Faciliter l'intégration continue (CI/CD)

**3. Gains de productivité**
- Réduire les tâches répétitives
- Standardiser le processus de build
- Faciliter l'onboarding des nouveaux développeurs

**4. Flexibilité**
- Adapter le build à vos besoins spécifiques
- Intégrer des outils externes
- Créer des builds conditionnels

---

## Les outils natifs de FreePascal/Lazarus

### 1. FPC (FreePascal Compiler)

Le compilateur lui-même, utilisé en ligne de commande.

```bash
# Compilation simple
fpc monprogramme.pas

# Avec options
fpc -O3 -Xs -XX monprogramme.pas
```

**Options courantes :**
```bash
-O3              # Optimisation niveau 3
-Xs              # Strip symbols (enlever les symboles de debug)
-XX              # Smart linking (lien intelligent)
-Fu<path>        # Répertoire des unités
-Fi<path>        # Répertoire des includes
-FE<path>        # Répertoire de sortie
-o<name>         # Nom du fichier exécutable
```

### 2. lazbuild

Outil en ligne de commande de Lazarus pour compiler des projets.

```bash
# Compiler un projet
lazbuild monproject.lpi

# Avec configuration spécifique
lazbuild --build-mode=Release monproject.lpi

# Pour plusieurs OS
lazbuild --os=linux --cpu=x86_64 monproject.lpi
lazbuild --os=win64 --cpu=x86_64 monproject.lpi
```

**Options utiles :**
```bash
--build-mode=<mode>      # Debug, Release, etc.
--os=<os>                # OS cible (linux, win64, darwin)
--cpu=<cpu>              # CPU cible (x86_64, i386, arm)
--build-all              # Tout recompiler
--skip-dependencies      # Ne pas compiler les dépendances
--no-write-project       # Ne pas modifier le .lpi
--quiet                  # Mode silencieux
--verbose                # Mode verbeux
```

### 3. fpmake

Système de build natif de FreePascal (similaire à Make).

**Fichier : `fpmake.pp`**
```pascal
program fpmake;

{$mode objfpc}{$H+}

uses
  fpmkunit;

var
  P: TPackage;
  T: TTarget;

begin
  With Installer do
  begin
    P := AddPackage('monprojet');
    P.Version := '1.0.0';

    P.SourcePath.Add('src');
    P.IncludePath.Add('inc');

    T := P.Targets.AddProgram('monprogramme.pas');
    T.Dependencies.AddUnit('malib');

    Run;
  end;
end.
```

**Utilisation :**
```bash
fpc fpmake.pp      # Compiler le script de build
./fpmake compile   # Compiler le projet
./fpmake install   # Installer
./fpmake clean     # Nettoyer
```

---

## Créer un outil de build simple

### Script de build de base (Windows)

**Fichier : `build.bat`**
```batch
@echo off
setlocal

echo =======================================
echo  Build Script - MonProjet
echo =======================================
echo.

:: Configuration
set PROJECT_NAME=MonProjet
set PROJECT_FILE=src\%PROJECT_NAME%.lpi
set OUTPUT_DIR=bin
set BUILD_MODE=Release

:: Créer le répertoire de sortie
if not exist "%OUTPUT_DIR%" mkdir "%OUTPUT_DIR%"

:: Compilation
echo [1/3] Compilation en cours...
lazbuild --build-mode=%BUILD_MODE% "%PROJECT_FILE%"

if %ERRORLEVEL% NEQ 0 (
    echo.
    echo ERREUR : La compilation a echoue
    pause
    exit /b 1
)

:: Copier l'exécutable
echo [2/3] Copie de l'executable...
copy /Y "%PROJECT_NAME%.exe" "%OUTPUT_DIR%\"

:: Copier les ressources
echo [3/3] Copie des ressources...
xcopy /Y /E /I resources "%OUTPUT_DIR%\resources"

echo.
echo =======================================
echo  Build termine avec succes!
echo  Executable: %OUTPUT_DIR%\%PROJECT_NAME%.exe
echo =======================================
pause
```

### Script de build de base (Linux/Ubuntu)

**Fichier : `build.sh`**
```bash
#!/bin/bash

echo "======================================="
echo " Build Script - MonProjet"
echo "======================================="
echo

# Configuration
PROJECT_NAME="MonProjet"
PROJECT_FILE="src/${PROJECT_NAME}.lpi"
OUTPUT_DIR="bin"
BUILD_MODE="Release"

# Créer le répertoire de sortie
mkdir -p "$OUTPUT_DIR"

# Compilation
echo "[1/3] Compilation en cours..."
lazbuild --build-mode=$BUILD_MODE "$PROJECT_FILE"

if [ $? -ne 0 ]; then
    echo
    echo "ERREUR : La compilation a échoué"
    exit 1
fi

# Copier l'exécutable
echo "[2/3] Copie de l'exécutable..."
cp -f "$PROJECT_NAME" "$OUTPUT_DIR/"
chmod +x "$OUTPUT_DIR/$PROJECT_NAME"

# Copier les ressources
echo "[3/3] Copie des ressources..."
cp -r resources "$OUTPUT_DIR/"

echo
echo "======================================="
echo " Build terminé avec succès!"
echo " Exécutable: $OUTPUT_DIR/$PROJECT_NAME"
echo "======================================="
```

---

## Outil de build avancé en Pascal

### Structure du projet

```
project/
  ├── src/             (code source)
  ├── tests/           (tests unitaires)
  ├── docs/            (documentation)
  ├── resources/       (ressources)
  ├── build/           (fichiers temporaires)
  ├── bin/             (exécutables)
  └── buildtool.lpr    (notre outil de build)
```

### Outil de build complet

**Fichier : `buildtool.lpr`**
```pascal
program BuildTool;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, Process, StrUtils;

const
  VERSION = '1.0.0';

type
  TBuildConfig = record
    ProjectName: string;
    ProjectFile: string;
    BuildMode: string;
    OutputDir: string;
    TargetOS: string;
    TargetCPU: string;
    RunTests: Boolean;
    GenerateDocs: Boolean;
    CreateInstaller: Boolean;
  end;

var
  Config: TBuildConfig;
  StartTime: TDateTime;

{ Utilitaires }

procedure PrintHeader;
begin
  WriteLn('===========================================');
  WriteLn(' BuildTool v', VERSION);
  WriteLn(' Outil de build personnalisé');
  WriteLn('===========================================');
  WriteLn;
end;

procedure PrintStep(const Step: string);
begin
  WriteLn;
  WriteLn('>>> ', Step);
end;

procedure PrintSuccess(const Msg: string);
begin
  WriteLn('[OK] ', Msg);
end;

procedure PrintError(const Msg: string);
begin
  WriteLn('[ERREUR] ', Msg);
end;

function ExecuteCommand(const Command: string; ShowOutput: Boolean = True): Boolean;
var
  Process: TProcess;
  OutputLines: TStringList;
  Line: string;
begin
  Result := False;
  Process := TProcess.Create(nil);
  OutputLines := TStringList.Create;
  try
    Process.Executable := {$IFDEF WINDOWS}'cmd.exe'{$ELSE}'/bin/bash'{$ENDIF};
    Process.Parameters.Add({$IFDEF WINDOWS}'/c'{$ELSE}'-c'{$ENDIF});
    Process.Parameters.Add(Command);
    Process.Options := [poWaitOnExit, poUsePipes];

    try
      Process.Execute;

      OutputLines.LoadFromStream(Process.Output);

      if ShowOutput then
      begin
        for Line in OutputLines do
          WriteLn('  ', Line);
      end;

      Result := Process.ExitStatus = 0;

    except
      on E: Exception do
      begin
        PrintError('Erreur lors de l''exécution : ' + E.Message);
        Result := False;
      end;
    end;

  finally
    OutputLines.Free;
    Process.Free;
  end;
end;

{ Étapes de build }

function CleanBuild: Boolean;
begin
  PrintStep('Nettoyage...');

  // Supprimer les fichiers temporaires
  if DirectoryExists(Config.OutputDir) then
  begin
    if ExecuteCommand({$IFDEF WINDOWS}'rmdir /S /Q '{$ELSE}'rm -rf '{$ENDIF} +
                      Config.OutputDir, False) then
      PrintSuccess('Répertoire de sortie nettoyé')
    else
      PrintError('Impossible de nettoyer le répertoire de sortie');
  end;

  // Recréer le répertoire
  if not DirectoryExists(Config.OutputDir) then
    ForceDirectories(Config.OutputDir);

  Result := True;
end;

function CompileProject: Boolean;
var
  Command: string;
begin
  PrintStep('Compilation du projet...');

  Command := 'lazbuild --build-mode=' + Config.BuildMode;

  if Config.TargetOS <> '' then
    Command := Command + ' --os=' + Config.TargetOS;

  if Config.TargetCPU <> '' then
    Command := Command + ' --cpu=' + Config.TargetCPU;

  Command := Command + ' ' + Config.ProjectFile;

  WriteLn('Commande : ', Command);

  Result := ExecuteCommand(Command);

  if Result then
    PrintSuccess('Compilation réussie')
  else
    PrintError('Échec de la compilation');
end;

function RunTests: Boolean;
begin
  if not Config.RunTests then
  begin
    WriteLn('Tests désactivés');
    Exit(True);
  end;

  PrintStep('Exécution des tests...');

  if not FileExists('tests/TestRunner.exe') then
  begin
    PrintError('Programme de test non trouvé');
    Exit(False);
  end;

  Result := ExecuteCommand('tests/TestRunner.exe');

  if Result then
    PrintSuccess('Tous les tests sont passés')
  else
    PrintError('Certains tests ont échoué');
end;

function GenerateDocumentation: Boolean;
begin
  if not Config.GenerateDocs then
  begin
    WriteLn('Génération de documentation désactivée');
    Exit(True);
  end;

  PrintStep('Génération de la documentation...');

  Result := ExecuteCommand('pasdoc --format html --output docs/html src/*.pas');

  if Result then
    PrintSuccess('Documentation générée dans docs/html')
  else
    PrintError('Échec de la génération de documentation');
end;

function CopyResources: Boolean;
var
  Command: string;
begin
  PrintStep('Copie des ressources...');

  if not DirectoryExists('resources') then
  begin
    WriteLn('Aucune ressource à copier');
    Exit(True);
  end;

  {$IFDEF WINDOWS}
  Command := 'xcopy /Y /E /I resources ' + Config.OutputDir + '\resources';
  {$ELSE}
  Command := 'cp -r resources ' + Config.OutputDir + '/';
  {$ENDIF}

  Result := ExecuteCommand(Command, False);

  if Result then
    PrintSuccess('Ressources copiées')
  else
    PrintError('Échec de la copie des ressources');
end;

function CreateInstaller: Boolean;
begin
  if not Config.CreateInstaller then
  begin
    WriteLn('Création d''installateur désactivée');
    Exit(True);
  end;

  PrintStep('Création de l''installateur...');

  {$IFDEF WINDOWS}
  // Utiliser Inno Setup
  if not FileExists('installer\setup.iss') then
  begin
    PrintError('Script Inno Setup non trouvé');
    Exit(False);
  end;

  Result := ExecuteCommand('iscc installer\setup.iss');
  {$ELSE}
  // Créer un .deb ou AppImage
  PrintError('Création d''installateur non implémentée pour Linux');
  Result := False;
  {$ENDIF}

  if Result then
    PrintSuccess('Installateur créé')
  else
    PrintError('Échec de la création de l''installateur');
end;

{ Configuration et menu }

procedure LoadDefaultConfig;
begin
  Config.ProjectName := 'MonProjet';
  Config.ProjectFile := 'src/MonProjet.lpi';
  Config.BuildMode := 'Release';
  Config.OutputDir := 'bin';
  Config.TargetOS := '';
  Config.TargetCPU := '';
  Config.RunTests := False;
  Config.GenerateDocs := False;
  Config.CreateInstaller := False;
end;

procedure ParseCommandLine;
var
  i: Integer;
  Param: string;
begin
  for i := 1 to ParamCount do
  begin
    Param := LowerCase(ParamStr(i));

    case Param of
      '--debug', '-d':
        Config.BuildMode := 'Debug';
      '--release', '-r':
        Config.BuildMode := 'Release';
      '--tests', '-t':
        Config.RunTests := True;
      '--docs':
        Config.GenerateDocs := True;
      '--installer', '-i':
        Config.CreateInstaller := True;
      '--clean', '-c':
        CleanBuild;
      '--help', '-h':
      begin
        WriteLn('Usage: buildtool [options]');
        WriteLn;
        WriteLn('Options:');
        WriteLn('  -d, --debug        Compiler en mode Debug');
        WriteLn('  -r, --release      Compiler en mode Release');
        WriteLn('  -t, --tests        Exécuter les tests');
        WriteLn('  --docs             Générer la documentation');
        WriteLn('  -i, --installer    Créer l''installateur');
        WriteLn('  -c, --clean        Nettoyer avant de compiler');
        WriteLn('  -h, --help         Afficher cette aide');
        WriteLn;
        Halt(0);
      end;
    end;
  end;
end;

procedure PrintSummary;
var
  Duration: Double;
begin
  Duration := (Now - StartTime) * 24 * 60 * 60; // Convertir en secondes

  WriteLn;
  WriteLn('===========================================');
  WriteLn(' Build terminé avec succès!');
  WriteLn(' Durée : ', Duration:0:2, ' secondes');
  WriteLn(' Configuration : ', Config.BuildMode);
  WriteLn(' Sortie : ', Config.OutputDir);
  WriteLn('===========================================');
end;

{ Programme principal }

var
  Success: Boolean;

begin
  StartTime := Now;

  PrintHeader;

  LoadDefaultConfig;
  ParseCommandLine;

  WriteLn('Configuration du build :');
  WriteLn('  Projet : ', Config.ProjectName);
  WriteLn('  Mode : ', Config.BuildMode);
  WriteLn('  Sortie : ', Config.OutputDir);
  WriteLn('  Tests : ', BoolToStr(Config.RunTests, True));
  WriteLn('  Documentation : ', BoolToStr(Config.GenerateDocs, True));
  WriteLn('  Installateur : ', BoolToStr(Config.CreateInstaller, True));

  Success := True;

  // Exécuter les étapes
  Success := Success and CleanBuild;
  Success := Success and CompileProject;
  Success := Success and RunTests;
  Success := Success and CopyResources;
  Success := Success and GenerateDocumentation;
  Success := Success and CreateInstaller;

  if Success then
  begin
    PrintSummary;
    ExitCode := 0;
  end
  else
  begin
    WriteLn;
    PrintError('Le build a échoué');
    ExitCode := 1;
  end;
end.
```

**Utilisation :**
```bash
# Build de base
./buildtool

# Build en mode Debug avec tests
./buildtool --debug --tests

# Build complet (Release + docs + installateur)
./buildtool --release --docs --installer

# Nettoyer et recompiler
./buildtool --clean --release
```

---

## Intégration avec Make/Makefile

### Makefile de base

**Fichier : `Makefile`**
```makefile
# Configuration
PROJECT_NAME = MonProjet
PROJECT_FILE = src/$(PROJECT_NAME).lpi
BUILD_MODE = Release
OUTPUT_DIR = bin
LAZBUILD = lazbuild

# Cibles
.PHONY: all clean build test install help

all: clean build

help:
	@echo "Cibles disponibles :"
	@echo "  make build          - Compiler le projet"
	@echo "  make clean          - Nettoyer"
	@echo "  make test           - Exécuter les tests"
	@echo "  make install        - Installer"
	@echo "  make debug          - Compiler en mode Debug"
	@echo "  make release        - Compiler en mode Release"

clean:
	@echo "Nettoyage..."
	@rm -rf $(OUTPUT_DIR)
	@rm -rf lib
	@rm -f *.o *.ppu *.rst

build:
	@echo "Compilation..."
	@mkdir -p $(OUTPUT_DIR)
	$(LAZBUILD) --build-mode=$(BUILD_MODE) $(PROJECT_FILE)
	@echo "Build terminé!"

debug:
	@$(MAKE) build BUILD_MODE=Debug

release:
	@$(MAKE) build BUILD_MODE=Release

test: build
	@echo "Exécution des tests..."
	@./tests/TestRunner

install: build
	@echo "Installation..."
	@install -m 755 $(OUTPUT_DIR)/$(PROJECT_NAME) /usr/local/bin/

# Builds multi-plateformes
build-linux:
	$(LAZBUILD) --os=linux --cpu=x86_64 $(PROJECT_FILE)

build-windows:
	$(LAZBUILD) --os=win64 --cpu=x86_64 $(PROJECT_FILE)

build-all: build-linux build-windows
	@echo "Tous les builds terminés!"
```

**Utilisation :**
```bash
make              # Build complet (clean + build)
make clean        # Nettoyer
make build        # Compiler
make debug        # Compiler en Debug
make release      # Compiler en Release
make test         # Exécuter tests
make build-all    # Compiler pour toutes les plateformes
```

---

## Build multi-plateforme avancé

### Script de cross-compilation

**Fichier : `crossbuild.sh`**
```bash
#!/bin/bash

PROJECT_NAME="MonProjet"
PROJECT_FILE="src/${PROJECT_NAME}.lpi"
OUTPUT_BASE="releases"

echo "========================================="
echo " Cross-Compilation - $PROJECT_NAME"
echo "========================================="
echo

# Fonction de build
build_for_platform() {
    local os=$1
    local cpu=$2
    local output_dir="${OUTPUT_BASE}/${os}-${cpu}"

    echo "Building for ${os}-${cpu}..."

    mkdir -p "$output_dir"

    lazbuild --os=$os --cpu=$cpu --build-mode=Release "$PROJECT_FILE"

    if [ $? -eq 0 ]; then
        # Copier l'exécutable
        if [ "$os" = "win64" ] || [ "$os" = "win32" ]; then
            cp "${PROJECT_NAME}.exe" "$output_dir/"
        else
            cp "$PROJECT_NAME" "$output_dir/"
            chmod +x "$output_dir/$PROJECT_NAME"
        fi

        # Copier les ressources
        cp -r resources "$output_dir/" 2>/dev/null

        # Créer une archive
        cd "$OUTPUT_BASE"
        tar czf "${PROJECT_NAME}-${os}-${cpu}.tar.gz" "${os}-${cpu}"
        cd ..

        echo "[OK] Build for ${os}-${cpu} completed"
    else
        echo "[ERREUR] Build for ${os}-${cpu} failed"
        return 1
    fi

    echo
}

# Nettoyer
rm -rf "$OUTPUT_BASE"
mkdir -p "$OUTPUT_BASE"

# Builds pour différentes plateformes
build_for_platform "linux" "x86_64"
build_for_platform "linux" "i386"
build_for_platform "win64" "x86_64"
build_for_platform "win32" "i386"
build_for_platform "darwin" "x86_64"

echo "========================================="
echo " Cross-compilation terminée!"
echo " Archives disponibles dans: $OUTPUT_BASE"
echo "========================================="

ls -lh "$OUTPUT_BASE"/*.tar.gz
```

---

## Intégration CI/CD

### GitHub Actions

**Fichier : `.github/workflows/build.yml`**
```yaml
name: Build

on:
  push:
    branches: [ main, develop ]
  pull_request:
    branches: [ main ]

jobs:
  build-linux:
    runs-on: ubuntu-latest

    steps:
    - uses: actions/checkout@v3

    - name: Install FreePascal/Lazarus
      run: |
        sudo apt-get update
        sudo apt-get install -y lazarus fpc

    - name: Build Project
      run: |
        lazbuild --build-mode=Release src/MonProjet.lpi

    - name: Run Tests
      run: |
        ./tests/TestRunner

    - name: Upload Artifact
      uses: actions/upload-artifact@v3
      with:
        name: MonProjet-Linux
        path: bin/MonProjet

  build-windows:
    runs-on: windows-latest

    steps:
    - uses: actions/checkout@v3

    - name: Install FreePascal/Lazarus
      run: |
        choco install lazarus

    - name: Build Project
      run: |
        lazbuild --build-mode=Release src/MonProjet.lpi

    - name: Upload Artifact
      uses: actions/upload-artifact@v3
      with:
        name: MonProjet-Windows
        path: bin/MonProjet.exe
```

### GitLab CI

**Fichier : `.gitlab-ci.yml`**
```yaml
stages:
  - build
  - test
  - deploy

variables:
  PROJECT_NAME: "MonProjet"
  PROJECT_FILE: "src/${PROJECT_NAME}.lpi"

before_script:
  - apt-get update -qq
  - apt-get install -y lazarus fpc

build:linux:
  stage: build
  script:
    - lazbuild --build-mode=Release $PROJECT_FILE
  artifacts:
    paths:
      - bin/
    expire_in: 1 week
  tags:
    - linux

build:windows:
  stage: build
  script:
    - lazbuild --os=win64 --cpu=x86_64 --build-mode=Release $PROJECT_FILE
  artifacts:
    paths:
      - bin/
    expire_in: 1 week
  tags:
    - windows

test:
  stage: test
  script:
    - ./tests/TestRunner
  dependencies:
    - build:linux

deploy:release:
  stage: deploy
  script:
    - echo "Déploiement en production"
    # Copier vers serveur, créer release GitHub, etc.
  only:
    - main
    - tags
```

---

## Build avec versioning automatique

### Générer un numéro de version automatique

**Fichier : `version_gen.pas`**
```pascal
program VersionGen;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, Process;

function GetGitCommitCount: Integer;
var
  Process: TProcess;
  Output: TStringList;
begin
  Result := 0;
  Process := TProcess.Create(nil);
  Output := TStringList.Create;
  try
    Process.Executable := 'git';
    Process.Parameters.Add('rev-list');
    Process.Parameters.Add('--count');
    Process.Parameters.Add('HEAD');
    Process.Options := [poWaitOnExit, poUsePipes];

    try
      Process.Execute;
      Output.LoadFromStream(Process.Output);
      if Output.Count > 0 then
        Result := StrToIntDef(Trim(Output[0]), 0);
    except
      Result := 0;
    end;
  finally
    Output.Free;
    Process.Free;
  end;
end;

function GetGitCommitHash: string;
var
  Process: TProcess;
  Output: TStringList;
begin
  Result := 'unknown';
  Process := TProcess.Create(nil);
  Output := TStringList.Create;
  try
    Process.Executable := 'git';
    Process.Parameters.Add('rev-parse');
    Process.Parameters.Add('--short');
    Process.Parameters.Add('HEAD');
    Process.Options := [poWaitOnExit, poUsePipes];

    try
      Process.Execute;
      Output.LoadFromStream(Process.Output);
      if Output.Count > 0 then
        Result := Trim(Output[0]);
    except
      Result := 'unknown';
    end;
  finally
    Output.Free;
    Process.Free;
  end;
end;

procedure GenerateVersionFile;
var
  VersionFile: TStringList;
  Major, Minor: Integer;
  Build: Integer;
  Hash: string;
begin
  Major := 1;
  Minor := 0;
  Build := GetGitCommitCount;
  Hash := GetGitCommitHash;

  VersionFile := TStringList.Create;
  try
    VersionFile.Add('unit Version;');
    VersionFile.Add('');
    VersionFile.Add('interface');
    VersionFile.Add('');
    VersionFile.Add('const');
    VersionFile.Add(Format('  VERSION_MAJOR = %d;', [Major]));
    VersionFile.Add(Format('  VERSION_MINOR = %d;', [Minor]));
    VersionFile.Add(Format('  VERSION_BUILD = %d;', [Build]));
    VersionFile.Add(Format('  VERSION_HASH = ''%s'';', [Hash]));
    VersionFile.Add(Format('  VERSION_STRING = ''%d.%d.%d-%s'';',
      [Major, Minor, Build, Hash]));
    VersionFile.Add('');
    VersionFile.Add('implementation');
    VersionFile.Add('');
    VersionFile.Add('end.');

    VersionFile.SaveToFile('src/Version.pas');

    WriteLn('Version générée : ', Major, '.', Minor, '.', Build, '-', Hash);
  finally
    VersionFile.Free;
  end;
end;

begin
  try
    GenerateVersionFile;
  except
    on E: Exception do
    begin
      WriteLn('Erreur : ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
```

**Intégration dans le build :**
```bash
# Générer le fichier de version
fpc version_gen.pas
./version_gen

# Puis compiler le projet
lazbuild projet.lpi
```

---

## Optimisations et bonnes pratiques

### 1. Build incrémental

```pascal
function NeedRebuild(const SourceFile, OutputFile: string): Boolean;
begin
  Result := not FileExists(OutputFile) or
            (FileAge(SourceFile) > FileAge(OutputFile));
end;

procedure SmartBuild;
begin
  if NeedRebuild('src/main.pas', 'bin/main.o') then
  begin
    WriteLn('Compilation nécessaire...');
    CompileFile('src/main.pas');
  end
  else
    WriteLn('Fichier à jour, compilation ignorée');
end;
```

### 2. Parallélisation

```pascal
uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, Process;

type
  TBuildThread = class(TThread)
  private
    FCommand: string;
    FSuccess: Boolean;
  protected
    procedure Execute; override;
  public
    constructor Create(const Command: string);
    property Success: Boolean read FSuccess;
  end;

constructor TBuildThread.Create(const Command: string);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FCommand := Command;
end;

procedure TBuildThread.Execute;
begin
  FSuccess := ExecuteCommand(FCommand, False);
end;

procedure ParallelBuild;
var
  Threads: array[1..3] of TBuildThread;
  i: Integer;
  AllSuccess: Boolean;
begin
  // Lancer plusieurs builds en parallèle
  Threads[1] := TBuildThread.Create('lazbuild --os=linux projet.lpi');
  Threads[2] := TBuildThread.Create('lazbuild --os=win64 projet.lpi');
  Threads[3] := TBuildThread.Create('lazbuild --os=darwin projet.lpi');

  WriteLn('Lancement des builds en parallèle...');

  for i := 1 to 3 do
    Threads[i].Start;

  // Attendre la fin de tous les threads
  for i := 1 to 3 do
  begin
    Threads[i].WaitFor;
    WriteLn('Build ', i, ' terminé : ', BoolToStr(Threads[i].Success, True));
  end;

  // Vérifier le succès global
  AllSuccess := True;
  for i := 1 to 3 do
    AllSuccess := AllSuccess and Threads[i].Success;

  // Libérer les threads
  for i := 1 to 3 do
    Threads[i].Free;

  if AllSuccess then
    WriteLn('Tous les builds ont réussi!')
  else
    WriteLn('Certains builds ont échoué');
end;
```

### 3. Cache de build

```pascal
type
  TBuildCache = class
  private
    FCacheDir: string;
    FCacheIndex: TStringList;
    function GetCacheKey(const FileName: string): string;
    function GetCachePath(const Key: string): string;
  public
    constructor Create(const CacheDir: string);
    destructor Destroy; override;
    function IsCached(const FileName: string): Boolean;
    procedure AddToCache(const FileName: string);
    procedure ClearCache;
  end;

constructor TBuildCache.Create(const CacheDir: string);
begin
  inherited Create;
  FCacheDir := CacheDir;
  FCacheIndex := TStringList.Create;

  ForceDirectories(FCacheDir);

  if FileExists(FCacheDir + 'index.txt') then
    FCacheIndex.LoadFromFile(FCacheDir + 'index.txt');
end;

destructor TBuildCache.Destroy;
begin
  FCacheIndex.SaveToFile(FCacheDir + 'index.txt');
  FCacheIndex.Free;
  inherited Destroy;
end;

function TBuildCache.GetCacheKey(const FileName: string): string;
var
  Hash: string;
  Content: TStringList;
begin
  // Calculer un hash simple basé sur le contenu du fichier
  Content := TStringList.Create;
  try
    Content.LoadFromFile(FileName);
    // Hash simplifié (dans un vrai projet, utiliser MD5/SHA)
    Hash := IntToHex(Length(Content.Text) xor $5A5A5A5A, 8);
    Result := ExtractFileName(FileName) + '_' + Hash;
  finally
    Content.Free;
  end;
end;

function TBuildCache.IsCached(const FileName: string): Boolean;
var
  Key: string;
begin
  Key := GetCacheKey(FileName);
  Result := FCacheIndex.IndexOf(Key) >= 0;
end;

procedure TBuildCache.AddToCache(const FileName: string);
var
  Key: string;
begin
  Key := GetCacheKey(FileName);
  if FCacheIndex.IndexOf(Key) < 0 then
    FCacheIndex.Add(Key);
end;

procedure TBuildCache.ClearCache;
begin
  FCacheIndex.Clear;
  DeleteFile(FCacheDir + 'index.txt');
end;
```

### 4. Logging structuré

```pascal
type
  TLogLevel = (llDebug, llInfo, llWarning, llError);

  TBuildLogger = class
  private
    FLogFile: TextFile;
    FLogToFile: Boolean;
    FLogToConsole: Boolean;
    procedure WriteLog(Level: TLogLevel; const Msg: string);
  public
    constructor Create(const LogFileName: string = '');
    destructor Destroy; override;
    procedure Debug(const Msg: string);
    procedure Info(const Msg: string);
    procedure Warning(const Msg: string);
    procedure Error(const Msg: string);
    property LogToFile: Boolean read FLogToFile write FLogToFile;
    property LogToConsole: Boolean read FLogToConsole write FLogToConsole;
  end;

constructor TBuildLogger.Create(const LogFileName: string = '');
begin
  inherited Create;
  FLogToConsole := True;
  FLogToFile := LogFileName <> '';

  if FLogToFile then
  begin
    AssignFile(FLogFile, LogFileName);
    Rewrite(FLogFile);
  end;
end;

destructor TBuildLogger.Destroy;
begin
  if FLogToFile then
    CloseFile(FLogFile);
  inherited Destroy;
end;

procedure TBuildLogger.WriteLog(Level: TLogLevel; const Msg: string);
var
  LevelStr: string;
  LogLine: string;
begin
  case Level of
    llDebug:   LevelStr := '[DEBUG]';
    llInfo:    LevelStr := '[INFO]';
    llWarning: LevelStr := '[WARN]';
    llError:   LevelStr := '[ERROR]';
  end;

  LogLine := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) + ' ' +
             LevelStr + ' ' + Msg;

  if FLogToConsole then
    WriteLn(LogLine);

  if FLogToFile then
    WriteLn(FLogFile, LogLine);
end;

procedure TBuildLogger.Debug(const Msg: string);
begin
  WriteLog(llDebug, Msg);
end;

procedure TBuildLogger.Info(const Msg: string);
begin
  WriteLog(llInfo, Msg);
end;

procedure TBuildLogger.Warning(const Msg: string);
begin
  WriteLog(llWarning, Msg);
end;

procedure TBuildLogger.Error(const Msg: string);
begin
  WriteLog(llError, Msg);
end;

// Utilisation
var
  Logger: TBuildLogger;
begin
  Logger := TBuildLogger.Create('build.log');
  try
    Logger.Info('Début du build');
    Logger.Debug('Configuration chargée');
    Logger.Warning('Dépendance manquante');
    Logger.Error('Compilation échouée');
  finally
    Logger.Free;
  end;
end;
```

---

## Intégration avec des outils externes

### 1. Notification Slack/Discord

```pascal
uses
  fphttpclient, fpjson, jsonparser;

procedure SendSlackNotification(const WebhookURL, Message: string);
var
  HTTPClient: TFPHTTPClient;
  Response: string;
  JSONData: TJSONObject;
begin
  HTTPClient := TFPHTTPClient.Create(nil);
  try
    JSONData := TJSONObject.Create;
    try
      JSONData.Strings['text'] := Message;

      HTTPClient.RequestBody := TStringStream.Create(JSONData.AsJSON);
      HTTPClient.AddHeader('Content-Type', 'application/json');

      Response := HTTPClient.Post(WebhookURL);

      WriteLn('Notification envoyée à Slack');
    finally
      JSONData.Free;
    end;
  finally
    HTTPClient.Free;
  end;
end;

// Utilisation
procedure NotifyBuildComplete(Success: Boolean);
var
  Msg: string;
begin
  if Success then
    Msg := '✅ Build réussi!'
  else
    Msg := '❌ Build échoué!';

  SendSlackNotification('https://hooks.slack.com/services/YOUR/WEBHOOK/URL', Msg);
end;
```

### 2. Upload vers serveur FTP

```pascal
uses
  fpsock, sockets;

procedure UploadToFTP(const FileName, Host, User, Pass, RemotePath: string);
var
  // Implémentation simplifiée - utiliser un client FTP complet en production
  Command: string;
begin
  {$IFDEF WINDOWS}
  Command := Format('ftp -s:ftpcmds.txt', []);
  {$ELSE}
  Command := Format('lftp -u %s,%s -e "put %s; bye" %s',
    [User, Pass, FileName, Host]);
  {$ENDIF}

  ExecuteCommand(Command);
  WriteLn('Fichier uploadé : ', FileName);
end;
```

### 3. Génération de release notes

```pascal
procedure GenerateReleaseNotes(const OutputFile: string);
var
  Process: TProcess;
  Output: TStringList;
  ReleaseNotes: TStringList;
  Line: string;
begin
  Process := TProcess.Create(nil);
  Output := TStringList.Create;
  ReleaseNotes := TStringList.Create;
  try
    // Obtenir les commits depuis le dernier tag
    Process.Executable := 'git';
    Process.Parameters.Add('log');
    Process.Parameters.Add('--pretty=format:- %s');
    Process.Parameters.Add('HEAD...$(git describe --tags --abbrev=0)');
    Process.Options := [poWaitOnExit, poUsePipes];

    Process.Execute;
    Output.LoadFromStream(Process.Output);

    ReleaseNotes.Add('# Release Notes');
    ReleaseNotes.Add('');
    ReleaseNotes.Add('## Version ' + GetVersion);
    ReleaseNotes.Add('');
    ReleaseNotes.Add('### Changements :');
    ReleaseNotes.Add('');
    ReleaseNotes.AddStrings(Output);

    ReleaseNotes.SaveToFile(OutputFile);
    WriteLn('Release notes générées : ', OutputFile);
  finally
    ReleaseNotes.Free;
    Output.Free;
    Process.Free;
  end;
end;
```

---

## Gestion des dépendances

### Système simple de gestion de dépendances

**Fichier : `dependencies.json`**
```json
{
  "dependencies": [
    {
      "name": "synapse",
      "url": "https://github.com/synopse/synapse.git",
      "version": "master"
    },
    {
      "name": "indy",
      "url": "https://github.com/IndySockets/Indy.git",
      "version": "10.6.2"
    }
  ]
}
```

**Gestionnaire de dépendances :**
```pascal
program DependencyManager;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, fpjson, jsonparser, Process;

type
  TDependency = record
    Name: string;
    URL: string;
    Version: string;
  end;

procedure LoadDependencies(const FileName: string; var Deps: array of TDependency);
var
  JSONData: TJSONData;
  JSONArray: TJSONArray;
  JSONObj: TJSONObject;
  FileContent: TStringList;
  i: Integer;
begin
  FileContent := TStringList.Create;
  try
    FileContent.LoadFromFile(FileName);
    JSONData := GetJSON(FileContent.Text);
  finally
    FileContent.Free;
  end;
  try
    JSONArray := TJSONObject(JSONData).Arrays['dependencies'];

    SetLength(Deps, JSONArray.Count);

    for i := 0 to JSONArray.Count - 1 do
    begin
      JSONObj := TJSONObject(JSONArray[i]);
      Deps[i].Name := JSONObj.Strings['name'];
      Deps[i].URL := JSONObj.Strings['url'];
      Deps[i].Version := JSONObj.Strings['version'];
    end;
  finally
    JSONData.Free;
  end;
end;

function CloneRepository(const URL, Name, Version: string): Boolean;
var
  Command: string;
  TargetDir: string;
begin
  TargetDir := 'lib/' + Name;

  WriteLn('Installation de ', Name, ' (', Version, ')...');

  if DirectoryExists(TargetDir) then
  begin
    WriteLn('  Déjà installé, mise à jour...');
    Command := 'git -C ' + TargetDir + ' pull';
  end
  else
  begin
    ForceDirectories('lib');
    Command := 'git clone ' + URL + ' ' + TargetDir;
  end;

  Result := ExecuteCommand(Command, False);

  if Result then
  begin
    // Checkout de la version spécifique
    if Version <> 'master' then
    begin
      Command := 'git -C ' + TargetDir + ' checkout ' + Version;
      Result := ExecuteCommand(Command, False);
    end;

    if Result then
      WriteLn('  [OK] ', Name, ' installé')
    else
      WriteLn('  [ERREUR] Impossible de basculer vers la version ', Version);
  end
  else
    WriteLn('  [ERREUR] Installation échouée');
end;

procedure InstallDependencies;
var
  Deps: array of TDependency;
  i: Integer;
begin
  WriteLn('========================================');
  WriteLn(' Gestionnaire de dépendances');
  WriteLn('========================================');
  WriteLn;

  LoadDependencies('dependencies.json', Deps);

  WriteLn('Dépendances trouvées : ', Length(Deps));
  WriteLn;

  for i := 0 to High(Deps) do
    CloneRepository(Deps[i].URL, Deps[i].Name, Deps[i].Version);

  WriteLn;
  WriteLn('Installation des dépendances terminée!');
end;

begin
  InstallDependencies;
end.
```

---

## Configuration avancée

### Fichier de configuration du build

**Fichier : `build.json`**
```json
{
  "project": {
    "name": "MonProjet",
    "version": "1.0.0",
    "author": "Votre Nom"
  },
  "paths": {
    "source": "src",
    "output": "bin",
    "resources": "resources",
    "tests": "tests"
  },
  "build": {
    "default_mode": "Release",
    "compiler_options": [
      "-O3",
      "-Xs",
      "-XX"
    ]
  },
  "targets": [
    {
      "os": "linux",
      "cpu": "x86_64",
      "enabled": true
    },
    {
      "os": "win64",
      "cpu": "x86_64",
      "enabled": true
    },
    {
      "os": "darwin",
      "cpu": "x86_64",
      "enabled": false
    }
  ],
  "steps": {
    "pre_build": [
      "version_gen"
    ],
    "post_build": [
      "copy_resources",
      "run_tests"
    ]
  }
}
```

**Lecteur de configuration :**
```pascal
type
  TBuildConfiguration = class
  private
    FProjectName: string;
    FVersion: string;
    FSourcePath: string;
    FOutputPath: string;
    FCompilerOptions: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromFile(const FileName: string);
    property ProjectName: string read FProjectName;
    property Version: string read FVersion;
    property SourcePath: string read FSourcePath;
    property OutputPath: string read FOutputPath;
    property CompilerOptions: TStringList read FCompilerOptions;
  end;

constructor TBuildConfiguration.Create;
begin
  inherited Create;
  FCompilerOptions := TStringList.Create;
end;

destructor TBuildConfiguration.Destroy;
begin
  FCompilerOptions.Free;
  inherited Destroy;
end;

procedure TBuildConfiguration.LoadFromFile(const FileName: string);
var
  JSONData: TJSONData;
  JSONObj, ProjectObj, PathsObj, BuildObj: TJSONObject;
  OptionsArray: TJSONArray;
  FileContent: TStringList;
  i: Integer;
begin
  FileContent := TStringList.Create;
  try
    FileContent.LoadFromFile(FileName);
    JSONData := GetJSON(FileContent.Text);
  finally
    FileContent.Free;
  end;
  try
    JSONObj := TJSONObject(JSONData);

    // Projet
    ProjectObj := JSONObj.Objects['project'];
    FProjectName := ProjectObj.Strings['name'];
    FVersion := ProjectObj.Strings['version'];

    // Chemins
    PathsObj := JSONObj.Objects['paths'];
    FSourcePath := PathsObj.Strings['source'];
    FOutputPath := PathsObj.Strings['output'];

    // Build
    BuildObj := JSONObj.Objects['build'];
    OptionsArray := BuildObj.Arrays['compiler_options'];

    for i := 0 to OptionsArray.Count - 1 do
      FCompilerOptions.Add(OptionsArray.Strings[i]);
  finally
    JSONData.Free;
  end;
end;
```

---

## Documentation du système de build

### Générer la documentation du build

```pascal
procedure GenerateBuildDocumentation;
var
  Doc: TStringList;
begin
  Doc := TStringList.Create;
  try
    Doc.Add('# Documentation du Build');
    Doc.Add('');
    Doc.Add('## Commandes disponibles');
    Doc.Add('');
    Doc.Add('### Build standard');
    Doc.Add('```bash');
    Doc.Add('./buildtool');
    Doc.Add('```');
    Doc.Add('');
    Doc.Add('### Build avec options');
    Doc.Add('```bash');
    Doc.Add('./buildtool --release --tests --docs');
    Doc.Add('```');
    Doc.Add('');
    Doc.Add('## Options');
    Doc.Add('');
    Doc.Add('- `--debug` : Compiler en mode Debug');
    Doc.Add('- `--release` : Compiler en mode Release');
    Doc.Add('- `--tests` : Exécuter les tests unitaires');
    Doc.Add('- `--docs` : Générer la documentation');
    Doc.Add('- `--installer` : Créer l''installateur');
    Doc.Add('- `--clean` : Nettoyer avant compilation');
    Doc.Add('');
    Doc.Add('## Structure du projet');
    Doc.Add('');
    Doc.Add('```');
    Doc.Add('project/');
    Doc.Add('  ├── src/           (code source)');
    Doc.Add('  ├── tests/         (tests unitaires)');
    Doc.Add('  ├── resources/     (ressources)');
    Doc.Add('  ├── bin/           (exécutables)');
    Doc.Add('  └── docs/          (documentation)');
    Doc.Add('```');

    Doc.SaveToFile('BUILD.md');
    WriteLn('Documentation générée : BUILD.md');
  finally
    Doc.Free;
  end;
end;
```

---

## Debugging du build

### Mode verbose détaillé

```pascal
var
  VerboseMode: Boolean = False;

procedure VerboseLog(const Msg: string);
begin
  if VerboseMode then
    WriteLn('[VERBOSE] ', Msg);
end;

function ExecuteCommandVerbose(const Command: string): Boolean;
begin
  VerboseLog('Exécution : ' + Command);
  Result := ExecuteCommand(Command);
  VerboseLog('Code de sortie : ' + IntToStr(ExitCode));
end;
```

### Profiling du build

```pascal
type
  TBuildProfiler = class
  private
    FSteps: TStringList;
    FStartTime: TDateTime;
  public
    constructor Create;
    destructor Destroy; override;
    procedure StartStep(const StepName: string);
    procedure EndStep(const StepName: string);
    procedure PrintReport;
  end;

constructor TBuildProfiler.Create;
begin
  inherited Create;
  FSteps := TStringList.Create;
  FStartTime := Now;
end;

destructor TBuildProfiler.Destroy;
begin
  FSteps.Free;
  inherited Destroy;
end;

procedure TBuildProfiler.StartStep(const StepName: string);
begin
  FSteps.AddObject(StepName, TObject(PtrInt(GetTickCount64)));
end;

procedure TBuildProfiler.EndStep(const StepName: string);
var
  Index: Integer;
  StartTick, Duration: QWord;
begin
  Index := FSteps.IndexOf(StepName);
  if Index >= 0 then
  begin
    StartTick := QWord(PtrInt(FSteps.Objects[Index]));
    Duration := GetTickCount64 - StartTick;
    FSteps[Index] := FSteps[Index] + ' (' + IntToStr(Duration) + ' ms)';
  end;
end;

procedure TBuildProfiler.PrintReport;
var
  i: Integer;
  TotalDuration: Double;
begin
  TotalDuration := (Now - FStartTime) * 24 * 60 * 60;

  WriteLn;
  WriteLn('=== Rapport de profiling ===');
  WriteLn;

  for i := 0 to FSteps.Count - 1 do
    WriteLn('  ', FSteps[i]);

  WriteLn;
  WriteLn('Durée totale : ', TotalDuration:0:2, ' secondes');
end;

// Utilisation
var
  Profiler: TBuildProfiler;
begin
  Profiler := TBuildProfiler.Create;
  try
    Profiler.StartStep('Compilation');
    CompileProject;
    Profiler.EndStep('Compilation');

    Profiler.StartStep('Tests');
    RunTests;
    Profiler.EndStep('Tests');

    Profiler.PrintReport;
  finally
    Profiler.Free;
  end;
end;
```

---

## Conclusion

Les outils de build personnalisés sont essentiels pour :

✅ **Automatiser** le processus de compilation  
✅ **Standardiser** les builds dans l'équipe  
✅ **Optimiser** le temps de développement  
✅ **Faciliter** le déploiement multi-plateforme  
✅ **Intégrer** CI/CD facilement

**Points clés à retenir :**
- Commencer simple puis enrichir progressivement
- Utiliser `lazbuild` pour les projets Lazarus
- Documenter votre système de build
- Intégrer des logs et du profiling
- Tester sur toutes les plateformes cibles

**Prochaines étapes :**
1. Créer un script de build basique pour votre projet
2. Ajouter la compilation multi-plateforme
3. Intégrer les tests automatiques
4. Configurer CI/CD (GitHub Actions ou GitLab CI)
5. Automatiser le versioning et les releases

**Ressources utiles :**
- Documentation lazbuild : https://wiki.lazarus.freepascal.org/lazbuild
- Documentation fpmake : https://www.freepascal.org/docs-html/prog/progap7.html
- Exemples de Makefiles : https://makefiletutorial.com/

Un bon système de build peut faire gagner des heures de travail et éviter de nombreuses erreurs. Investissez du temps dans sa création, cela sera largement rentabilisé!

⏭️ [Cross-compilation avancée](/24-compilateur-outils-avances/08-cross-compilation-avancee.md)
