🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 2.10 Automatisation avec lazbuild

## Introduction : Qu'est-ce que lazbuild ?

Imaginez que vous devez cuire 100 gâteaux. Vous pourriez ouvrir le four et surveiller chaque gâteau manuellement, ou vous pourriez programmer le four pour qu'il fasse tout automatiquement. **Lazbuild** est le "four programmable" de Lazarus : il compile vos projets automatiquement, sans interface graphique, parfait pour l'automatisation !

**Lazbuild, c'est quoi exactement ?**

Lazbuild est l'outil en ligne de commande de Lazarus qui permet de :
- 🏗️ **Compiler** des projets sans ouvrir l'IDE
- 🔄 **Automatiser** les builds dans des scripts
- 🤖 **Intégrer** dans les systèmes CI/CD
- 📦 **Créer** des builds en masse
- 🌙 **Compiler** la nuit (builds nocturnes)
- ☁️ **Builder** sur des serveurs sans interface graphique

**Pourquoi utiliser lazbuild ?**
- **Rapidité** : Pas d'interface graphique = plus rapide
- **Automatisation** : Scriptable et programmable
- **CI/CD** : Intégration parfaite avec Jenkins, GitHub Actions, etc.
- **Serveurs** : Fonctionne sur des serveurs sans affichage
- **Batch** : Compiler plusieurs projets d'un coup

## Installation et configuration

### Où trouver lazbuild

#### Sur Windows

```
C:\lazarus\lazbuild.exe
```

Si pas dans le PATH, ajouter :
```batch
REM Ajouter au PATH temporairement
set PATH=%PATH%;C:\lazarus

REM Ou utiliser le chemin complet
C:\lazarus\lazbuild.exe mon_projet.lpi
```

#### Sur Linux/Ubuntu

```bash
# Généralement installé avec Lazarus
/usr/bin/lazbuild

# Vérifier l'installation
which lazbuild
lazbuild --version

# Si non installé
sudo apt-get install lazarus
```

#### Sur macOS

```bash
# Dans le bundle Lazarus
/Applications/Lazarus/lazbuild

# Ou si installé via Homebrew
brew install lazarus
lazbuild --version
```

### Première utilisation

#### Test simple

```bash
# Afficher l'aide
lazbuild --help

# Version et informations
lazbuild --version

# Compiler un projet simple
lazbuild MonProjet.lpi
```

**Résultat attendu :**
```
Lazarus build tool v2.2.6
FPC version: 3.2.2
Building project: MonProjet.lpi
Target: MonProjet.exe
Success: Project compiled successfully
```

## Syntaxe et options de base

### Syntaxe générale

```
lazbuild [options] project.lpi|package.lpk
```

### Options principales

```
Options essentielles :
├── Compilation
│   ├── -B, --build-all : Tout recompiler
│   ├── -r, --recursive : Compiler les dépendances
│   └── --skip-dependencies : Ignorer les dépendances
├── Configuration
│   ├── --build-mode=<mode> : Mode de compilation
│   ├── --os=<os> : OS cible
│   ├── --cpu=<cpu> : CPU cible
│   └── --ws=<widgetset> : Widget set
├── Chemins
│   ├── --pcp=<path> : Config path
│   ├── --scp=<path> : Secondary config
│   └── --compiler=<path> : Chemin FPC
└── Affichage
    ├── -q, --quiet : Mode silencieux
    ├── -v, --verbose : Mode verbeux
    └── --no-write-project : Ne pas modifier .lpi
```

### Exemples d'utilisation basique

#### Compilation simple

```bash
# Compiler avec les paramètres par défaut
lazbuild MonProjet.lpi

# Forcer la recompilation complète
lazbuild -B MonProjet.lpi

# Mode silencieux (pour scripts)
lazbuild -q MonProjet.lpi

# Mode verbeux (pour debug)
lazbuild -v MonProjet.lpi
```

#### Compilation avec mode

```bash
# Compiler en mode Release
lazbuild --build-mode=Release MonProjet.lpi

# Compiler en mode Debug
lazbuild --build-mode=Debug MonProjet.lpi

# Lister les modes disponibles
lazbuild --list-modes MonProjet.lpi
```

## Compilation de projets

### Projet simple

Structure basique :
```
MonProjet/
├── MonProjet.lpi     # Fichier projet Lazarus
├── MonProjet.lpr     # Programme principal
├── unit1.pas         # Unité
└── unit1.lfm         # Formulaire
```

Commande :
```bash
cd MonProjet
lazbuild MonProjet.lpi

# Résultat : MonProjet.exe (Windows) ou MonProjet (Linux)
```

### Projet avec dépendances

```bash
# Compiler avec toutes les dépendances
lazbuild -r MonProjet.lpi

# Ignorer les dépendances (plus rapide si déjà compilées)
lazbuild --skip-dependencies MonProjet.lpi
```

### Projets multiples

#### Script Windows - compile_all.bat

```batch
@echo off
echo === Compilation de tous les projets ===
echo.

set PROJECTS=Projet1.lpi Projet2.lpi Projet3.lpi
set FAILED=

for %%P in (%PROJECTS%) do (
    echo Compilation de %%P...
    lazbuild -q %%P

    if errorlevel 1 (
        echo [ERREUR] %%P a échoué
        set FAILED=%FAILED% %%P
    ) else (
        echo [OK] %%P compilé avec succès
    )
    echo.
)

if defined FAILED (
    echo === Projets en erreur : %FAILED% ===
    exit /b 1
) else (
    echo === Tous les projets compilés avec succès ===
)
```

#### Script Linux - compile_all.sh

```bash
#!/bin/bash

echo "=== Compilation de tous les projets ==="
echo

PROJECTS="Projet1.lpi Projet2.lpi Projet3.lpi"
FAILED=""

for PROJECT in $PROJECTS; do
    echo "Compilation de $PROJECT..."

    if lazbuild -q "$PROJECT"; then
        echo "[OK] $PROJECT compilé avec succès"
    else
        echo "[ERREUR] $PROJECT a échoué"
        FAILED="$FAILED $PROJECT"
    fi
    echo
done

if [ -n "$FAILED" ]; then
    echo "=== Projets en erreur : $FAILED ==="
    exit 1
else
    echo "=== Tous les projets compilés avec succès ==="
fi
```

## Compilation de packages

### Package simple

```bash
# Compiler un package
lazbuild MonPackage.lpk

# Installer un package (recompile l'IDE)
lazbuild --add-package MonPackage.lpk

# Installer sans confirmation
lazbuild --add-package-link MonPackage.lpk
```

### Gestion des packages

```bash
# Lister les packages requis
lazbuild --list-packages MonProjet.lpi

# Compiler tous les packages requis
lazbuild -r --build-required-packages MonProjet.lpi

# Nettoyer et recompiler les packages
lazbuild --clean-all --build-all-packages
```

## Cross-compilation avec lazbuild

### Compiler pour une autre plateforme

#### Windows vers Linux

```bash
# Compiler pour Linux 64-bit depuis Windows
lazbuild --os=linux --cpu=x86_64 --ws=gtk2 MonProjet.lpi

# Avec un mode spécifique
lazbuild --os=linux --cpu=x86_64 --build-mode=Release-Linux MonProjet.lpi
```

#### Linux vers Windows

```bash
# Compiler pour Windows 64-bit depuis Linux
lazbuild --os=win64 --cpu=x86_64 --ws=win32 MonProjet.lpi

# Pour Windows 32-bit
lazbuild --os=win32 --cpu=i386 --ws=win32 MonProjet.lpi
```

### Script de cross-compilation

`cross_compile.sh` :
```bash
#!/bin/bash

PROJECT="MonProjet.lpi"
VERSION="1.0.0"

echo "=== Cross-compilation v$VERSION ==="

# Créer les répertoires de sortie
mkdir -p bin/windows/x64
mkdir -p bin/windows/x86
mkdir -p bin/linux/x64
mkdir -p bin/macos/x64

# Windows 64-bit
echo "Compilation Windows 64-bit..."
lazbuild --os=win64 --cpu=x86_64 --ws=win32 \
         --build-mode=Release $PROJECT
mv MonProjet.exe bin/windows/x64/

# Windows 32-bit
echo "Compilation Windows 32-bit..."
lazbuild --os=win32 --cpu=i386 --ws=win32 \
         --build-mode=Release $PROJECT
mv MonProjet.exe bin/windows/x86/

# Linux 64-bit
echo "Compilation Linux 64-bit..."
lazbuild --os=linux --cpu=x86_64 --ws=gtk2 \
         --build-mode=Release $PROJECT
mv MonProjet bin/linux/x64/

# macOS 64-bit (si cross-compiler disponible)
echo "Compilation macOS 64-bit..."
lazbuild --os=darwin --cpu=x86_64 --ws=cocoa \
         --build-mode=Release $PROJECT
mv MonProjet bin/macos/x64/

echo "=== Cross-compilation terminée ==="
```

## Scripts d'automatisation avancés

### Build avec versioning automatique

#### Windows - build_with_version.bat

```batch
@echo off
setlocal enabledelayedexpansion

REM Récupérer la version depuis Git
for /f "tokens=*" %%i in ('git describe --tags --always') do set VERSION=%%i
if "%VERSION%"=="" set VERSION=dev

REM Récupérer la date
for /f "tokens=2 delims==" %%i in ('wmic os get localdatetime /value') do set datetime=%%i
set BUILD_DATE=%datetime:~0,8%-%datetime:~8,6%

echo === Build version %VERSION% (%BUILD_DATE%) ===

REM Créer le fichier de version
echo const > src\version.inc
echo   APP_VERSION = '%VERSION%'; >> src\version.inc
echo   BUILD_DATE = '%BUILD_DATE%'; >> src\version.inc
echo   BUILD_NUMBER = '%datetime:~8,6%'; >> src\version.inc

REM Compiler
lazbuild --build-mode=Release MonProjet.lpi

if %errorlevel% == 0 (
    REM Renommer avec version
    move bin\MonProjet.exe bin\MonProjet-%VERSION%.exe
    echo Build réussi : MonProjet-%VERSION%.exe
) else (
    echo Erreur de compilation !
    exit /b 1
)
```

#### Linux - build_with_version.sh

```bash
#!/bin/bash

# Récupérer la version depuis Git
VERSION=$(git describe --tags --always 2>/dev/null || echo "dev")
BUILD_DATE=$(date +%Y%m%d-%H%M%S)
BUILD_NUMBER=$(date +%s)

echo "=== Build version $VERSION ($BUILD_DATE) ==="

# Créer le fichier de version
cat > src/version.inc << EOF
const
  APP_VERSION = '$VERSION';
  BUILD_DATE = '$BUILD_DATE';
  BUILD_NUMBER = '$BUILD_NUMBER';
EOF

# Compiler
if lazbuild --build-mode=Release MonProjet.lpi; then
    # Renommer avec version
    mv bin/MonProjet "bin/MonProjet-$VERSION"
    echo "Build réussi : MonProjet-$VERSION"

    # Créer un lien symbolique vers la dernière version
    ln -sf "MonProjet-$VERSION" bin/MonProjet-latest
else
    echo "Erreur de compilation !"
    exit 1
fi
```

### Build nocturne avec rapport

`nightly_build.sh` :
```bash
#!/bin/bash

# Configuration
PROJECT_DIR="/home/build/projects"
LOG_DIR="/var/log/builds"
EMAIL="dev-team@company.com"
DATE=$(date +%Y%m%d)
LOG_FILE="$LOG_DIR/build-$DATE.log"

# Fonction de log
log() {
    echo "[$(date +%H:%M:%S)] $1" | tee -a "$LOG_FILE"
}

# Début du build
log "=== Build nocturne démarré ==="

cd "$PROJECT_DIR"

# Mise à jour du code
log "Mise à jour depuis Git..."
git pull origin main >> "$LOG_FILE" 2>&1

# Nettoyer
log "Nettoyage..."
rm -rf lib/* bin/*

# Compiler tous les modes
MODES="Debug Release Test"
SUCCESS=true

for MODE in $MODES; do
    log "Compilation mode $MODE..."

    if lazbuild --build-mode="$MODE" MonProjet.lpi >> "$LOG_FILE" 2>&1; then
        log "✓ $MODE compilé avec succès"
    else
        log "✗ $MODE a échoué"
        SUCCESS=false
    fi
done

# Tests unitaires
if [ "$SUCCESS" = true ]; then
    log "Exécution des tests..."
    if ./bin/test/run_tests.sh >> "$LOG_FILE" 2>&1; then
        log "✓ Tests passés"
    else
        log "✗ Tests échoués"
        SUCCESS=false
    fi
fi

# Rapport
if [ "$SUCCESS" = true ]; then
    SUBJECT="✓ Build nocturne réussi - $DATE"
    STATUS="SUCCESS"
else
    SUBJECT="✗ Build nocturne échoué - $DATE"
    STATUS="FAILED"
fi

# Envoyer email
cat "$LOG_FILE" | mail -s "$SUBJECT" "$EMAIL"

# Archive
gzip "$LOG_FILE"

log "=== Build nocturne terminé : $STATUS ==="
```

## Intégration CI/CD

### GitHub Actions

`.github/workflows/build.yml` :
```yaml
name: Build with lazbuild

on:
  push:
    branches: [ main, develop ]
  pull_request:
    branches: [ main ]

jobs:
  build:
    runs-on: ${{ matrix.os }}

    strategy:
      matrix:
        os: [ubuntu-latest, windows-latest, macos-latest]
        mode: [Debug, Release]

    steps:
    - uses: actions/checkout@v3

    - name: Install Lazarus (Ubuntu)
      if: runner.os == 'Linux'
      run: |
        sudo apt-get update
        sudo apt-get install -y lazarus

    - name: Install Lazarus (Windows)
      if: runner.os == 'Windows'
      run: |
        choco install lazarus

    - name: Install Lazarus (macOS)
      if: runner.os == 'macOS'
      run: |
        brew install --cask lazarus

    - name: Build with lazbuild
      run: |
        lazbuild --build-mode=${{ matrix.mode }} MonProjet.lpi

    - name: Run tests
      if: matrix.mode == 'Debug'
      run: |
        if [ "$RUNNER_OS" == "Windows" ]; then
          ./bin/debug/MonProjet.exe --test
        else
          ./bin/debug/MonProjet --test
        fi
      shell: bash

    - name: Upload artifacts
      uses: actions/upload-artifact@v3
      with:
        name: ${{ runner.os }}-${{ matrix.mode }}
        path: bin/
```

### GitLab CI

`.gitlab-ci.yml` :
```yaml
stages:
  - build
  - test
  - deploy

variables:
  LAZARUS_VERSION: "2.2.6"

before_script:
  - apt-get update -qq
  - apt-get install -y lazarus

build:debug:
  stage: build
  script:
    - lazbuild --build-mode=Debug MonProjet.lpi
  artifacts:
    paths:
      - bin/debug/
    expire_in: 1 day

build:release:
  stage: build
  script:
    - lazbuild --build-mode=Release MonProjet.lpi
  artifacts:
    paths:
      - bin/release/
    expire_in: 1 week

test:unit:
  stage: test
  dependencies:
    - build:debug
  script:
    - ./bin/debug/MonProjet --run-tests
    - ./bin/debug/MonProjet --coverage-report
  coverage: '/Coverage: \d+\.\d+%/'
  artifacts:
    reports:
      coverage_report:
        coverage_format: cobertura
        path: coverage.xml

deploy:staging:
  stage: deploy
  dependencies:
    - build:release
  script:
    - scp bin/release/MonProjet user@staging:/opt/app/
  only:
    - develop

deploy:production:
  stage: deploy
  dependencies:
    - build:release
  script:
    - scp bin/release/MonProjet user@production:/opt/app/
  only:
    - main
  when: manual
```

### Jenkins

`Jenkinsfile` :
```groovy
pipeline {
    agent any

    environment {
        LAZARUS_HOME = '/usr/lib/lazarus'
        PROJECT = 'MonProjet.lpi'
    }

    stages {
        stage('Checkout') {
            steps {
                checkout scm
            }
        }

        stage('Build Matrix') {
            matrix {
                axes {
                    axis {
                        name 'MODE'
                        values 'Debug', 'Release', 'Test'
                    }
                    axis {
                        name 'PLATFORM'
                        values 'linux', 'windows'
                    }
                }
                stages {
                    stage('Compile') {
                        steps {
                            script {
                                def os = PLATFORM == 'windows' ? 'win64' : 'linux'
                                sh """
                                    lazbuild \
                                        --build-mode=${MODE} \
                                        --os=${os} \
                                        --cpu=x86_64 \
                                        ${PROJECT}
                                """
                            }
                        }
                    }
                }
            }
        }

        stage('Test') {
            steps {
                sh './bin/test/MonProjet --junit-xml=results.xml'
                junit 'results.xml'
            }
        }

        stage('Archive') {
            steps {
                archiveArtifacts artifacts: 'bin/**/*',
                                 fingerprint: true
            }
        }
    }

    post {
        always {
            cleanWs()
        }
        success {
            mail to: 'team@company.com',
                 subject: "Build Success: ${env.JOB_NAME} - ${env.BUILD_NUMBER}",
                 body: "Build completed successfully."
        }
        failure {
            mail to: 'team@company.com',
                 subject: "Build Failed: ${env.JOB_NAME} - ${env.BUILD_NUMBER}",
                 body: "Build failed. Check logs: ${env.BUILD_URL}"
        }
    }
}
```

## Configuration avancée

### Fichier de configuration lazbuild

Créer `lazbuild.cfg` :
```ini
# Configuration globale lazbuild

# Chemins
[Paths]
LazarusDir=/usr/lib/lazarus
CompilerPath=/usr/bin/fpc
ConfigPath=~/.lazarus

# Options par défaut
[Options]
BuildMode=Release
Verbose=false
BuildAll=true

# Environnement
[Environment]
FPCDIR=/usr/lib/fpc/3.2.2
PP=/usr/bin/fpc
FPCTARGET=x86_64-linux
```

### Variables d'environnement

```bash
# Linux/macOS
export LAZARUS_DIR=/usr/lib/lazarus
export FPC_DIR=/usr/lib/fpc/3.2.2
export LAZBUILD_OPTIONS="--quiet --build-all"

# Windows
set LAZARUS_DIR=C:\lazarus
set FPC_DIR=C:\lazarus\fpc\3.2.2
set LAZBUILD_OPTIONS=--quiet --build-all
```

### Profils de compilation

`build_profiles.json` :
```json
{
  "profiles": {
    "development": {
      "mode": "Debug",
      "options": "--verbose",
      "output": "bin/debug"
    },
    "staging": {
      "mode": "Release",
      "options": "--build-all --optimize=3",
      "output": "bin/staging"
    },
    "production": {
      "mode": "Release",
      "options": "--build-all --optimize=3 --strip-symbols",
      "output": "bin/production",
      "post_build": [
        "strip ${output}/${binary}",
        "upx --best ${output}/${binary}",
        "sign.sh ${output}/${binary}"
      ]
    }
  }
}
```

Script utilisant les profils :
```python
#!/usr/bin/env python3
import json
import subprocess
import sys

def build_with_profile(profile_name):
    with open('build_profiles.json') as f:
        profiles = json.load(f)['profiles']

    if profile_name not in profiles:
        print(f"Profile '{profile_name}' not found")
        return False

    profile = profiles[profile_name]

    # Construire la commande
    cmd = [
        'lazbuild',
        f'--build-mode={profile["mode"]}',
        profile['options'],
        'MonProjet.lpi'
    ]

    print(f"Building with profile '{profile_name}'...")
    result = subprocess.run(' '.join(cmd), shell=True)

    if result.returncode == 0:
        # Exécuter les commandes post-build
        if 'post_build' in profile:
            for post_cmd in profile['post_build']:
                subprocess.run(post_cmd, shell=True)
        print(f"Build successful: {profile['output']}")
        return True
    else:
        print("Build failed")
        return False

if __name__ == '__main__':
    profile = sys.argv[1] if len(sys.argv) > 1 else 'development'
    build_with_profile(profile)
```

## Gestion des erreurs

### Codes de retour

```bash
# Codes de retour lazbuild
# 0 : Succès
# 1 : Erreur de compilation
# 2 : Projet non trouvé
# 3 : Erreur de configuration

# Vérifier dans un script
lazbuild MonProjet.lpi
RESULT=$?

case $RESULT in
    0) echo "Compilation réussie" ;;
    1) echo "Erreur de compilation" ;;
    2) echo "Projet non trouvé" ;;
    3) echo "Erreur de configuration" ;;
    *) echo "Erreur inconnue: $RESULT" ;;
esac
```

### Capture et analyse des erreurs

`build_with_error_handling.sh` :
```bash
#!/bin/bash

PROJECT="MonProjet.lpi"
LOG_FILE="build.log"
ERROR_FILE="errors.log"

# Compiler et capturer la sortie
lazbuild "$PROJECT" 2>&1 | tee "$LOG_FILE"
RESULT=${PIPESTATUS[0]}

if [ $RESULT -ne 0 ]; then
    echo "=== Erreur de compilation ==="

    # Extraire les erreurs
    grep -E "Error:|Fatal:" "$LOG_FILE" > "$ERROR_FILE"

    # Compter les erreurs
    ERROR_COUNT=$(wc -l < "$ERROR_FILE")
    echo "Nombre d'erreurs : $ERROR_COUNT"

    # Afficher les premières erreurs
    echo "Premières erreurs :"
    head -5 "$ERROR_FILE"

    # Notifier
    if command -v notify-send &> /dev/null; then
        notify-send "Build Failed" "$ERROR_COUNT errors found"
    fi

    exit 1
else
    echo "=== Compilation réussie ==="

    # Statistiques
    WARNINGS=$(grep -c "Warning:" "$LOG_FILE")
    HINTS=$(grep -c "Hint:" "$LOG_FILE")

    echo "Warnings : $WARNINGS"
    echo "Hints : $HINTS"
fi
```

## Optimisations et performances

### Compilation parallèle

> **Attention** : `lazbuild` ne supporte pas l'option `-j` pour la compilation
> parallèle (contrairement à `make`). FPC compile séquentiellement.
> Pour accélérer les builds, vous pouvez compiler **plusieurs projets**
> en parallèle dans des processus séparés :

```bash
# Linux : compiler plusieurs projets en parallèle
lazbuild Projet1.lpi &
lazbuild Projet2.lpi &
lazbuild Projet3.lpi &
wait  # Attendre que tous les builds se terminent

# Ou utiliser GNU parallel si disponible
echo -e "Projet1.lpi\nProjet2.lpi\nProjet3.lpi" | \
  parallel lazbuild --build-mode=Release {}
```

### Cache de compilation

```bash
# Utiliser un cache partagé
export LAZBUILD_CACHE=/tmp/lazbuild-cache
mkdir -p $LAZBUILD_CACHE

lazbuild --primary-config-path=$LAZBUILD_CACHE MonProjet.lpi
```

### Build incrémental

```bash
# Ne recompiler que ce qui a changé
lazbuild --no-build-all MonProjet.lpi

# Forcer la recompilation complète si nécessaire
lazbuild -B MonProjet.lpi
```

## Monitoring et statistiques

### Script de monitoring

`monitor_builds.py` :
```python
#!/usr/bin/env python3
import subprocess
import time
import datetime
import sqlite3

def init_db():
    conn = sqlite3.connect('builds.db')
    c = conn.cursor()
    c.execute('''CREATE TABLE IF NOT EXISTS builds
                 (id INTEGER PRIMARY KEY,
                  timestamp TEXT,
                  project TEXT,
                  mode TEXT,
                  duration REAL,
                  success INTEGER,
                  size INTEGER)''')
    conn.commit()
    return conn

def build_and_monitor(project, mode):
    start_time = time.time()

    cmd = ['lazbuild', f'--build-mode={mode}', project]
    result = subprocess.run(cmd, capture_output=True, text=True)

    duration = time.time() - start_time
    success = result.returncode == 0

    # Obtenir la taille du binaire
    size = 0
    if success:
        import os
        binary = project.replace('.lpi', '')
        if os.path.exists(binary):
            size = os.path.getsize(binary)

    # Enregistrer dans la base
    conn = init_db()
    c = conn.cursor()
    c.execute('''INSERT INTO builds
                 (timestamp, project, mode, duration, success, size)
                 VALUES (?, ?, ?, ?, ?, ?)''',
              (datetime.datetime.now().isoformat(),
               project, mode, duration, success, size))
    conn.commit()
    conn.close()

    return success, duration

def generate_report():
    conn = init_db()
    c = conn.cursor()

    # Statistiques globales
    c.execute('''SELECT
                 COUNT(*) as total,
                 SUM(success) as successful,
                 AVG(duration) as avg_duration
                 FROM builds
                 WHERE timestamp > datetime('now', '-7 days')''')

    stats = c.fetchone()
    print(f"=== Rapport des 7 derniers jours ===")
    print(f"Builds totaux : {stats[0]}")
    print(f"Réussis : {stats[1]}")
    print(f"Taux de succès : {stats[1]/stats[0]*100:.1f}%")
    print(f"Durée moyenne : {stats[2]:.2f}s")

    conn.close()

if __name__ == '__main__':
    # Build et monitorer
    success, duration = build_and_monitor('MonProjet.lpi', 'Release')

    if success:
        print(f"✓ Build réussi en {duration:.2f}s")
    else:
        print(f"✗ Build échoué après {duration:.2f}s")

    # Générer rapport
    generate_report()
```

## Dépannage

### Problèmes courants

#### "lazbuild: command not found"

**Linux :**
```bash
# Vérifier l'installation
which lazarus
find / -name lazbuild 2>/dev/null

# Ajouter au PATH
export PATH=$PATH:/usr/lib/lazarus
```

**Windows :**
```batch
REM Ajouter au PATH système
setx PATH "%PATH%;C:\lazarus"
```

#### "Can't find project"

```bash
# Vérifier le chemin
ls -la *.lpi
pwd

# Utiliser le chemin absolu
lazbuild /full/path/to/project.lpi
```

#### "Missing dependencies"

```bash
# Compiler avec les dépendances
lazbuild -r MonProjet.lpi

# Installer les packages manquants
lazbuild --add-package-link RequiredPackage.lpk
```

### Logs de débogage

```bash
# Mode très verbeux
lazbuild -vvv MonProjet.lpi > debug.log 2>&1

# Analyser les erreurs
grep -i error debug.log
grep -i "not found" debug.log
```

## Bonnes pratiques

### Structure de projet pour automatisation

```
MonProjet/
├── src/                # Sources
├── bin/               # Binaires (ignoré par Git)
├── lib/               # Unités compilées (ignoré)
├── tests/             # Tests unitaires
├── scripts/           # Scripts de build
│   ├── build.sh
│   ├── build.bat
│   └── deploy.py
├── .gitlab-ci.yml     # CI/CD GitLab
├── .github/           # CI/CD GitHub
├── Makefile          # Build avec Make
└── build.config      # Configuration lazbuild
```

### Makefile universel

```makefile
# Makefile pour lazbuild

PROJECT = MonProjet.lpi
LAZBUILD = lazbuild
MODE ?= Release

.PHONY: all clean build test deploy

all: build

build:
	$(LAZBUILD) --build-mode=$(MODE) $(PROJECT)

clean:
	rm -rf lib/ bin/
	$(LAZBUILD) --clean $(PROJECT)

test: MODE=Test
test: build
	./bin/test/MonProjet --run-tests

deploy: MODE=Release
deploy: build
	@echo "Deploying to server..."
	scp bin/release/MonProjet user@server:/opt/app/

install:
	$(LAZBUILD) --add-package-link packages/*.lpk

help:
	@echo "Usage: make [target]"
	@echo "Targets:"
	@echo "  build    - Compile le projet (MODE=Debug|Release|Test)"
	@echo "  clean    - Nettoyer les fichiers compilés"
	@echo "  test     - Compiler et exécuter les tests"
	@echo "  deploy   - Compiler et déployer"
	@echo "  install  - Installer les packages requis"

# Détection de l'OS pour commandes spécifiques
ifeq ($(OS),Windows_NT)
    RM = del /Q
    MKDIR = mkdir
else
    RM = rm -f
    MKDIR = mkdir -p
endif
```

### Convention de nommage des builds

```
Nommage recommandé :
├── Développement
│   └── MonProjet-dev-{commit-hash}
├── Version taguée
│   └── MonProjet-v1.2.3-{platform}
├── Build nocturne
│   └── MonProjet-nightly-{date}
└── Release
    └── MonProjet-{version}-{platform}-{arch}

Exemples :
- MonProjet-v1.2.3-win64.exe
- MonProjet-v1.2.3-linux-x86_64
- MonProjet-nightly-20240115
- MonProjet-dev-a3f5b2c
```

### Script de release complet

`release.sh` :
```bash
#!/bin/bash

# Script de release complet avec lazbuild

VERSION=$1
if [ -z "$VERSION" ]; then
    echo "Usage: $0 <version>"
    echo "Example: $0 1.2.3"
    exit 1
fi

echo "=== Release v$VERSION ==="

# Vérifier que tout est commité
if [ -n "$(git status --porcelain)" ]; then
    echo "Erreur : Des modifications non commitées existent"
    exit 1
fi

# Créer le tag
git tag -a "v$VERSION" -m "Release version $VERSION"

# Créer le répertoire de release
RELEASE_DIR="releases/v$VERSION"
mkdir -p "$RELEASE_DIR"

# Compiler pour toutes les plateformes
PLATFORMS=(
    "win32:i386:win32"
    "win64:x86_64:win32"
    "linux:x86_64:gtk2"
    "darwin:x86_64:cocoa"
)

for PLATFORM in "${PLATFORMS[@]}"; do
    IFS=':' read -r OS CPU WS <<< "$PLATFORM"

    echo "Building for $OS-$CPU..."

    if lazbuild --os="$OS" --cpu="$CPU" --ws="$WS" \
                --build-mode=Release MonProjet.lpi; then

        # Déterminer le nom du binaire
        if [[ "$OS" == "win"* ]]; then
            BINARY="MonProjet.exe"
            OUTPUT="MonProjet-v$VERSION-$OS.exe"
        else
            BINARY="MonProjet"
            OUTPUT="MonProjet-v$VERSION-$OS-$CPU"
        fi

        # Copier et compresser
        cp "bin/$BINARY" "$RELEASE_DIR/$OUTPUT"

        # Créer archive
        cd "$RELEASE_DIR"
        if [[ "$OS" == "win"* ]]; then
            zip "$OUTPUT.zip" "$OUTPUT"
        else
            tar czf "$OUTPUT.tar.gz" "$OUTPUT"
        fi
        cd - > /dev/null

        echo "✓ $OS-$CPU built successfully"
    else
        echo "✗ Failed to build for $OS-$CPU"
    fi
done

# Générer les checksums
cd "$RELEASE_DIR"
sha256sum * > SHA256SUMS
cd - > /dev/null

# Créer les notes de release
cat > "$RELEASE_DIR/RELEASE_NOTES.md" << EOF
# Release v$VERSION

Date: $(date +%Y-%m-%d)

## Changements
- [À compléter]

## Fichiers

### Windows
- MonProjet-v$VERSION-win32.exe (32-bit)
- MonProjet-v$VERSION-win64.exe (64-bit)

### Linux
- MonProjet-v$VERSION-linux-x86_64.tar.gz

### macOS
- MonProjet-v$VERSION-darwin-x86_64.tar.gz

## Installation

### Windows
\`\`\`
MonProjet-v$VERSION-win64.exe
\`\`\`

### Linux
\`\`\`bash
tar xzf MonProjet-v$VERSION-linux-x86_64.tar.gz
chmod +x MonProjet-v$VERSION-linux-x86_64
./MonProjet-v$VERSION-linux-x86_64
\`\`\`

## Vérification
\`\`\`bash
sha256sum -c SHA256SUMS
\`\`\`
EOF

echo ""
echo "=== Release v$VERSION créée avec succès ==="
echo "Fichiers dans : $RELEASE_DIR"
echo ""
echo "Prochaines étapes :"
echo "1. Compléter RELEASE_NOTES.md"
echo "2. git push origin v$VERSION"
echo "3. Uploader sur GitHub/GitLab releases"
```

### Documentation d'automatisation

Créer `AUTOMATION.md` :
````markdown
# Guide d'automatisation avec lazbuild

## Installation rapide

### Ubuntu/Debian
```bash
sudo apt-get install lazarus
```

### Windows
```powershell
choco install lazarus
# ou
winget install Lazarus
```

### macOS
```bash
brew install --cask lazarus
```

## Commandes essentielles

### Build simple
```bash
lazbuild MonProjet.lpi
```

### Build avec mode
```bash
lazbuild --build-mode=Release MonProjet.lpi
```

### Cross-compilation
```bash
# Windows → Linux
lazbuild --os=linux --cpu=x86_64 MonProjet.lpi

# Linux → Windows
lazbuild --os=win64 --cpu=x86_64 MonProjet.lpi
```

## Scripts disponibles

| Script | Description | Usage |
|--------|-------------|--------|
| `build.sh` | Build standard | `./build.sh [mode]` |
| `build_all.sh` | Build tous les modes | `./build_all.sh` |
| `release.sh` | Créer une release | `./release.sh 1.2.3` |
| `test.sh` | Lancer les tests | `./test.sh` |
| `deploy.sh` | Déployer | `./deploy.sh staging|production` |

## CI/CD

### GitHub Actions
- Déclenché sur : push, pull_request
- Plateformes : Windows, Linux, macOS
- Modes : Debug, Release

### GitLab CI
- Pipeline : build → test → deploy
- Environnements : staging, production
- Déploiement manuel en production

## Variables d'environnement

| Variable | Description | Défaut |
|----------|-------------|---------|
| `LAZBUILD_MODE` | Mode de compilation | Release |
| `LAZBUILD_OPTIONS` | Options supplémentaires | -q |
| `LAZARUS_DIR` | Répertoire Lazarus | Auto-détecté |

## Troubleshooting

### lazbuild introuvable
```bash
export PATH=$PATH:/usr/lib/lazarus
```

### Erreur de dépendances
```bash
lazbuild -r MonProjet.lpi
```

### Build lent
```bash
# Utiliser le cache
export LAZBUILD_CACHE=/tmp/lazbuild-cache

# Compilation incrémentale (par défaut, ne recompile que les unités modifiées)
lazbuild MonProjet.lpi

# Si vous avez plusieurs projets, lancez-les en parallèle
lazbuild Projet1.lpi & lazbuild Projet2.lpi & wait
```
````

## Exemples d'utilisation réels

### Projet open source avec GitHub Actions

Exemple réel d'un projet utilisant lazbuild :

```yaml
# .github/workflows/release.yml
name: Create Release

on:
  push:
    tags:
      - 'v*'

jobs:
  create-release:
    runs-on: ubuntu-latest
    outputs:
      upload_url: ${{ steps.create_release.outputs.upload_url }}
    steps:
      - name: Create Release
        id: create_release
        uses: actions/create-release@v1
        env:
          GITHUB_TOKEN: ${{ secrets.GITHUB_TOKEN }}
        with:
          tag_name: ${{ github.ref }}
          release_name: Release ${{ github.ref }}
          draft: false
          prerelease: false

  build-and-upload:
    needs: create-release
    strategy:
      matrix:
        include:
          - os: ubuntu-latest
            target: linux-x86_64
            ext: ''
          - os: windows-latest
            target: win64
            ext: '.exe'
          - os: macos-latest
            target: darwin-x86_64
            ext: ''

    runs-on: ${{ matrix.os }}

    steps:
      - uses: actions/checkout@v3

      - name: Install Lazarus
        uses: gcarreno/setup-lazarus@v3
        with:
          lazarus-version: stable

      - name: Build with lazbuild
        run: lazbuild --build-mode=Release MonProjet.lpi

      - name: Package
        run: |
          if [ "${{ matrix.os }}" = "windows-latest" ]; then
            7z a MonProjet-${{ matrix.target }}.zip MonProjet.exe
          else
            tar czf MonProjet-${{ matrix.target }}.tar.gz MonProjet
          fi
        shell: bash

      - name: Upload Release Asset
        uses: actions/upload-release-asset@v1
        env:
          GITHUB_TOKEN: ${{ secrets.GITHUB_TOKEN }}
        with:
          upload_url: ${{ needs.create-release.outputs.upload_url }}
          asset_path: ./MonProjet-${{ matrix.target }}.${{ matrix.ext == '' && 'tar.gz' || 'zip' }}
          asset_name: MonProjet-${{ matrix.target }}.${{ matrix.ext == '' && 'tar.gz' || 'zip' }}
          asset_content_type: application/octet-stream
```

### Entreprise avec Jenkins et Docker

`Dockerfile.build` :
```dockerfile
FROM ubuntu:22.04

# Installer les dépendances
RUN apt-get update && apt-get install -y \
    wget \
    git \
    make \
    binutils \
    lazarus \
    && rm -rf /var/lib/apt/lists/*

# Créer un utilisateur build
RUN useradd -m -s /bin/bash builder
USER builder
WORKDIR /home/builder

# Script de build
COPY build.sh /usr/local/bin/
RUN chmod +x /usr/local/bin/build.sh

ENTRYPOINT ["/usr/local/bin/build.sh"]
```

`docker-compose.yml` :
```yaml
version: '3.8'

services:
  builder:
    build:
      context: .
      dockerfile: Dockerfile.build
    volumes:
      - ./src:/home/builder/src
      - ./bin:/home/builder/bin
    environment:
      - BUILD_MODE=${BUILD_MODE:-Release}
      - TARGET_OS=${TARGET_OS:-linux}
    command: lazbuild --build-mode=${BUILD_MODE} /home/builder/src/MonProjet.lpi
```

### Script de déploiement continu

`continuous_deployment.sh` :
```bash
#!/bin/bash

# Déploiement continu avec lazbuild

BRANCH=$(git rev-parse --abbrev-ref HEAD)
COMMIT=$(git rev-parse --short HEAD)
TIMESTAMP=$(date +%Y%m%d-%H%M%S)

echo "=== Déploiement continu ==="
echo "Branche : $BRANCH"
echo "Commit : $COMMIT"
echo "Timestamp : $TIMESTAMP"

# Déterminer l'environnement selon la branche
case $BRANCH in
    main|master)
        ENV="production"
        MODE="Release"
        SERVER="prod.company.com"
        ;;
    staging)
        ENV="staging"
        MODE="Release"
        SERVER="staging.company.com"
        ;;
    develop)
        ENV="development"
        MODE="Debug"
        SERVER="dev.company.com"
        ;;
    *)
        echo "Branche non déployable : $BRANCH"
        exit 0
        ;;
esac

echo "Environnement : $ENV"
echo "Mode : $MODE"
echo "Serveur : $SERVER"

# Compiler
echo "Compilation..."
if ! lazbuild --build-mode="$MODE" MonProjet.lpi; then
    echo "Erreur de compilation"
    exit 1
fi

# Tests smoke
echo "Tests smoke..."
if ! ./bin/MonProjet --smoke-test; then
    echo "Tests smoke échoués"
    exit 1
fi

# Créer le package
PACKAGE="MonProjet-$ENV-$COMMIT-$TIMESTAMP.tar.gz"
tar czf "$PACKAGE" -C bin MonProjet

# Déployer
echo "Déploiement sur $SERVER..."
scp "$PACKAGE" "deploy@$SERVER:/opt/deployments/"

# Exécuter le script de déploiement distant
ssh "deploy@$SERVER" "/opt/scripts/deploy.sh $PACKAGE $ENV"

# Notification
if [ $? -eq 0 ]; then
    echo "✓ Déploiement réussi sur $ENV"
    # Envoyer notification Slack/Discord/Email
    curl -X POST -H 'Content-type: application/json' \
         --data "{\"text\":\"✓ Déploiement réussi sur $ENV (commit: $COMMIT)\"}" \
         "$SLACK_WEBHOOK_URL"
else
    echo "✗ Déploiement échoué"
    exit 1
fi
```

## Métriques et monitoring

### Dashboard de build

`generate_dashboard.py` :
```python
#!/usr/bin/env python3
import sqlite3
import datetime
import json

def generate_html_dashboard():
    conn = sqlite3.connect('builds.db')
    c = conn.cursor()

    # Récupérer les derniers builds
    c.execute('''SELECT timestamp, project, mode, duration, success
                 FROM builds
                 ORDER BY timestamp DESC
                 LIMIT 20''')

    recent_builds = c.fetchall()

    # Calculer les statistiques
    c.execute('''SELECT
                 COUNT(*) as total,
                 SUM(success) as successful,
                 AVG(duration) as avg_duration
                 FROM builds
                 WHERE timestamp > datetime('now', '-1 day')''')

    stats = c.fetchone()

    # Générer le HTML
    html = '''<!DOCTYPE html>
<html>
<head>
    <title>LazbBuild Dashboard</title>
    <meta charset="UTF-8">
    <style>
        body { font-family: Arial, sans-serif; margin: 20px; }
        h1 { color: #333; }
        table { border-collapse: collapse; width: 100%; margin-top: 20px; }
        th, td { border: 1px solid #ddd; padding: 12px; text-align: left; }
        th { background-color: #4CAF50; color: white; }
        tr:nth-child(even) { background-color: #f2f2f2; }
        .success { color: green; font-weight: bold; }
        .failure { color: red; font-weight: bold; }
        .stats { background-color: #f0f0f0; padding: 15px; border-radius: 5px; margin: 20px 0; }
    </style>
    <meta http-equiv="refresh" content="60">
</head>
<body>
    <h1>🚀 LazbBuild Dashboard</h1>

    <div class="stats">
        <h2>📊 Statistiques des dernières 24h</h2>
        <ul>
            <li>Builds totaux : ''' + str(stats[0]) + '''</li>
            <li>Réussis : ''' + str(stats[1]) + '''</li>
            <li>Taux de succès : ''' + f"{(stats[1]/stats[0]*100):.1f}" + '''%</li>
            <li>Temps moyen : ''' + f"{stats[2]:.2f}" + ''' secondes</li>
        </ul>
    </div>

    <h2>📋 Derniers builds</h2>
    <table>
        <tr>
            <th>Date/Heure</th>
            <th>Projet</th>
            <th>Mode</th>
            <th>Durée</th>
            <th>Statut</th>
        </tr>'''

    for build in recent_builds:
        timestamp, project, mode, duration, success = build
        status_class = 'success' if success else 'failure'
        status_text = '✅ Succès' if success else '❌ Échec'

        html += f'''
        <tr>
            <td>{timestamp}</td>
            <td>{project}</td>
            <td>{mode}</td>
            <td>{duration:.2f}s</td>
            <td class="{status_class}">{status_text}</td>
        </tr>'''

    html += '''
    </table>

    <p style="margin-top: 30px; color: #666;">
        Dernière mise à jour : ''' + datetime.datetime.now().strftime('%Y-%m-%d %H:%M:%S') + '''
        <br>Rafraîchissement automatique toutes les 60 secondes
    </p>
</body>
</html>'''

    # Sauvegarder le dashboard
    with open('dashboard.html', 'w', encoding='utf-8') as f:
        f.write(html)

    conn.close()
    print("Dashboard généré : dashboard.html")

if __name__ == '__main__':
    generate_html_dashboard()
```

### Intégration avec Grafana

`prometheus_exporter.py` :
```python
#!/usr/bin/env python3
from prometheus_client import start_http_server, Gauge, Counter
import sqlite3
import time

# Métriques Prometheus
build_duration = Gauge('lazbuild_duration_seconds', 'Build duration in seconds', ['project', 'mode'])
build_success = Counter('lazbuild_success_total', 'Successful builds', ['project'])
build_failure = Counter('lazbuild_failure_total', 'Failed builds', ['project'])
build_size = Gauge('lazbuild_binary_size_bytes', 'Binary size in bytes', ['project'])

def update_metrics():
    conn = sqlite3.connect('builds.db')
    c = conn.cursor()

    # Récupérer les métriques récentes
    c.execute('''SELECT project, mode, duration, success, size
                 FROM builds
                 WHERE timestamp > datetime('now', '-1 hour')''')

    for row in c.fetchall():
        project, mode, duration, success, size = row

        build_duration.labels(project=project, mode=mode).set(duration)

        if success:
            build_success.labels(project=project).inc()
        else:
            build_failure.labels(project=project).inc()

        if size:
            build_size.labels(project=project).set(size)

    conn.close()

if __name__ == '__main__':
    # Démarrer le serveur HTTP pour Prometheus
    start_http_server(8000)
    print("Prometheus exporter running on port 8000")

    # Mettre à jour les métriques toutes les 30 secondes
    while True:
        update_metrics()
        time.sleep(30)
```

## Cas d'usage avancés

### Build Matrix complexe

`build_matrix.sh` :
```bash
#!/bin/bash

# Build matrix pour tests complets

MODES=("Debug" "Release" "Test")
PLATFORMS=("win32:i386" "win64:x86_64" "linux:x86_64" "linux:i386")
WIDGETSETS=("win32" "gtk2" "gtk3" "qt5")

echo "=== Build Matrix Execution ==="
echo "Total combinations: $((${#MODES[@]} * ${#PLATFORMS[@]}))"

RESULTS_FILE="build_matrix_results.csv"
echo "Mode,Platform,CPU,WidgetSet,Result,Duration" > "$RESULTS_FILE"

for MODE in "${MODES[@]}"; do
    for PLATFORM in "${PLATFORMS[@]}"; do
        IFS=':' read -r OS CPU <<< "$PLATFORM"

        # Déterminer le widgetset approprié
        if [[ "$OS" == "win"* ]]; then
            WS="win32"
        else
            WS="gtk2"  # Par défaut pour Linux
        fi

        echo "Building: $MODE | $OS | $CPU | $WS"

        START=$(date +%s)

        if lazbuild --build-mode="$MODE" \
                   --os="$OS" \
                   --cpu="$CPU" \
                   --ws="$WS" \
                   MonProjet.lpi > /dev/null 2>&1; then
            RESULT="SUCCESS"
            echo "  ✅ Success"
        else
            RESULT="FAILED"
            echo "  ❌ Failed"
        fi

        END=$(date +%s)
        DURATION=$((END - START))

        echo "$MODE,$OS,$CPU,$WS,$RESULT,$DURATION" >> "$RESULTS_FILE"
    done
done

echo ""
echo "=== Matrix Build Complete ==="
echo "Results saved to: $RESULTS_FILE"

# Générer un rapport
SUCCESS_COUNT=$(grep -c "SUCCESS" "$RESULTS_FILE")
TOTAL_COUNT=$(($(wc -l < "$RESULTS_FILE") - 1))  # -1 pour l'en-tête

echo ""
echo "Summary:"
echo "  Total builds: $TOTAL_COUNT"
echo "  Successful: $SUCCESS_COUNT"
echo "  Failed: $((TOTAL_COUNT - SUCCESS_COUNT))"
echo "  Success rate: $((SUCCESS_COUNT * 100 / TOTAL_COUNT))%"
```

### Intégration avec Ansible

`playbook_deploy.yml` :
```yaml
---
- name: Deploy Lazarus Application
  hosts: production
  vars:
    app_name: MonProjet
    app_version: "{{ version | default('latest') }}"
    app_path: /opt/applications/{{ app_name }}

  tasks:
    - name: Ensure application directory exists
      file:
        path: "{{ app_path }}"
        state: directory
        owner: app
        group: app
        mode: '0755'

    - name: Build application on build server
      delegate_to: build-server
      command: lazbuild --build-mode=Release {{ app_name }}.lpi
      args:
        chdir: /home/builder/projects/{{ app_name }}
      register: build_result

    - name: Copy binary to production
      copy:
        src: "/home/builder/projects/{{ app_name }}/bin/{{ app_name }}"
        dest: "{{ app_path }}/{{ app_name }}-{{ app_version }}"
        owner: app
        group: app
        mode: '0755'
      when: build_result.rc == 0

    - name: Create symlink to current version
      file:
        src: "{{ app_path }}/{{ app_name }}-{{ app_version }}"
        dest: "{{ app_path }}/{{ app_name }}-current"
        state: link

    - name: Create systemd service
      template:
        src: app.service.j2
        dest: /etc/systemd/system/{{ app_name }}.service
      notify: restart application

    - name: Start and enable service
      systemd:
        name: "{{ app_name }}"
        state: started
        enabled: yes
        daemon_reload: yes

  handlers:
    - name: restart application
      systemd:
        name: "{{ app_name }}"
        state: restarted
```

## Conclusion

Lazbuild est l'outil indispensable pour automatiser vos compilations Lazarus. Il transforme un processus manuel répétitif en une opération automatisée, reproductible et fiable.

**Bénéfices de l'automatisation avec lazbuild :**
- 🚀 **Rapidité** : Compilation sans interface graphique
- 🤖 **Automatisation** : Intégration CI/CD parfaite
- 📦 **Scalabilité** : Gérer des dizaines de projets
- 🔄 **Reproductibilité** : Builds identiques à chaque fois
- 🌍 **Cross-platform** : Un outil pour toutes les plateformes
- 📊 **Monitoring** : Métriques et dashboards de suivi
- 🔧 **Flexibilité** : Scripts et configurations personnalisables

**Points clés à retenir :**
- Lazbuild est installé avec Lazarus
- Syntaxe simple : `lazbuild [options] projet.lpi`
- Parfait pour CI/CD (GitHub Actions, GitLab CI, Jenkins)
- Scriptable pour automatisation complexe
- Support complet de la cross-compilation
- Intégrable avec tous les outils DevOps modernes

**Recommandations finales :**
1. **Commencez simple** : Un Makefile basique ou script batch/bash
2. **Versionnez vos scripts** : Gardez-les dans Git avec votre code
3. **Automatisez progressivement** : CI/CD, tests, déploiement
4. **Mesurez et optimisez** : Utilisez les métriques pour améliorer
5. **Documentez** : Vos collègues vous remercieront

L'automatisation avec lazbuild est un investissement qui paie rapidement. Une fois vos scripts en place, vous gagnerez un temps précieux et éviterez les erreurs de compilation manuelle. C'est un outil essentiel pour tout développeur Lazarus sérieux qui veut passer du développement amateur au niveau professionnel !

⏭️ [Synchronisation de projets entre OS](/02-maitrise-ide-lazarus/11-synchronisation-projets-entre-os.md)
