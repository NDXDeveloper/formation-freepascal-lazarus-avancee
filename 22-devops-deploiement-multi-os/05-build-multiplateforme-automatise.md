🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 22.5 Build multi-plateforme automatisé

## Introduction

Le build multi-plateforme automatisé est une pratique essentielle pour développer des applications FreePascal/Lazarus fonctionnant sur Windows et Ubuntu (Linux). Au lieu de compiler manuellement votre projet sur chaque système d'exploitation, vous pouvez automatiser ce processus pour gagner du temps et garantir la cohérence de vos builds.

Dans ce chapitre, nous allons découvrir les outils et techniques permettant d'automatiser la compilation de vos projets FreePascal/Lazarus pour plusieurs plateformes simultanément.

## Pourquoi automatiser les builds multi-plateformes ?

### Avantages principaux

1. **Gain de temps** : Plus besoin de compiler manuellement sur chaque machine
2. **Cohérence** : Tous les builds utilisent les mêmes paramètres et versions
3. **Détection précoce des erreurs** : Les problèmes de compatibilité sont identifiés rapidement
4. **Traçabilité** : Chaque build est documenté et reproductible
5. **Qualité** : Les tests peuvent être exécutés automatiquement après chaque build

### Cas d'usage typiques

- Compiler une application de gestion pour Windows et Ubuntu à partir d'un seul code source
- Créer automatiquement des versions de test quotidiennes
- Générer des builds de production lors de la publication d'une nouvelle version
- Valider que les modifications de code fonctionnent sur toutes les plateformes cibles

## Outils essentiels

### lazbuild : L'outil en ligne de commande de Lazarus

`lazbuild` est l'outil officiel fourni avec Lazarus pour compiler des projets en ligne de commande, sans interface graphique. C'est la pierre angulaire de l'automatisation.

#### Emplacement de lazbuild

**Sur Windows :**
```
C:\lazarus\lazbuild.exe
```

**Sur Ubuntu/Linux :**
```
/usr/bin/lazbuild
ou
/usr/local/bin/lazbuild
```

#### Utilisation basique

Pour compiler un projet :

```bash
lazbuild monprojet.lpi
```

Pour compiler avec une configuration spécifique :

```bash
lazbuild --build-mode=Release monprojet.lpi
```

Pour nettoyer puis recompiler :

```bash
lazbuild --build-all monprojet.lpi
```

#### Options utiles de lazbuild

| Option | Description |
|--------|-------------|
| `--build-mode=<mode>` | Sélectionne le mode de build (Debug, Release, etc.) |
| `--build-all` | Reconstruit tout depuis zéro |
| `--os=<système>` | Spécifie le système d'exploitation cible |
| `--cpu=<architecture>` | Spécifie l'architecture processeur |
| `--compiler=<chemin>` | Utilise un compilateur spécifique |
| `--verbose` | Affiche des informations détaillées |
| `--quiet` | Mode silencieux (erreurs uniquement) |

### fpc : Le compilateur FreePascal

Le compilateur FreePascal (`fpc`) peut aussi être utilisé directement pour des scripts plus simples ou des bibliothèques sans interface graphique.

```bash
fpc -Mobjfpc -Scgi -O2 -g -gl monprogramme.pas
```

Options communes :
- `-M` : Mode de compilation (objfpc, delphi, tp)
- `-O2` : Optimisation niveau 2
- `-g` : Génération d'informations de débogage
- `-gl` : Numéros de ligne dans les messages d'erreur

## Scripts de build simples

### Script batch pour Windows

Créez un fichier `build-windows.bat` :

```batch
@echo off
echo ======================================  
echo Build automatique pour Windows  
echo ======================================

SET LAZARUS_DIR=C:\lazarus  
SET PROJECT_DIR=%~dp0  
SET PROJECT_NAME=MonApplication

echo.  
echo Nettoyage des anciens builds...  
if exist "%PROJECT_DIR%lib" rd /s /q "%PROJECT_DIR%lib"  
if exist "%PROJECT_DIR%bin" rd /s /q "%PROJECT_DIR%bin"

echo.  
echo Compilation en mode Debug...
"%LAZARUS_DIR%\lazbuild.exe" --build-mode=Debug "%PROJECT_DIR%\%PROJECT_NAME%.lpi"

if %ERRORLEVEL% NEQ 0 (
    echo ERREUR: La compilation en mode Debug a echoue!
    pause
    exit /b 1
)

echo.  
echo Compilation en mode Release...
"%LAZARUS_DIR%\lazbuild.exe" --build-mode=Release "%PROJECT_DIR%\%PROJECT_NAME%.lpi"

if %ERRORLEVEL% NEQ 0 (
    echo ERREUR: La compilation en mode Release a echoue!
    pause
    exit /b 1
)

echo.  
echo ======================================  
echo Build termine avec succes!  
echo ======================================  
pause
```

### Script bash pour Ubuntu/Linux

Créez un fichier `build-linux.sh` :

```bash
#!/bin/bash

echo "======================================"  
echo "Build automatique pour Ubuntu/Linux"  
echo "======================================"

PROJECT_DIR="$(cd "$(dirname "$0")" && pwd)"  
PROJECT_NAME="MonApplication"

echo ""  
echo "Nettoyage des anciens builds..."  
rm -rf "$PROJECT_DIR/lib"  
rm -rf "$PROJECT_DIR/bin"

echo ""  
echo "Compilation en mode Debug..."  
lazbuild --build-mode=Debug "$PROJECT_DIR/$PROJECT_NAME.lpi"

if [ $? -ne 0 ]; then
    echo "ERREUR: La compilation en mode Debug a échoué!"
    exit 1
fi

echo ""  
echo "Compilation en mode Release..."  
lazbuild --build-mode=Release "$PROJECT_DIR/$PROJECT_NAME.lpi"

if [ $? -ne 0 ]; then
    echo "ERREUR: La compilation en mode Release a échoué!"
    exit 1
fi

echo ""  
echo "======================================"  
echo "Build terminé avec succès!"  
echo "======================================"
```

N'oubliez pas de rendre le script exécutable :

```bash
chmod +x build-linux.sh
```

## Configuration des modes de build dans Lazarus

Pour que vos scripts fonctionnent correctement, vous devez configurer les modes de build dans votre projet Lazarus.

### Création d'un mode de build

1. Ouvrez votre projet dans Lazarus
2. Menu **Projet → Options du projet**
3. Section **Options du compilateur**
4. Cliquez sur **Build modes** en haut
5. Cliquez sur le bouton **+** pour ajouter un nouveau mode

### Configuration typique

#### Mode Debug
- **Optimisation** : Niveau 0 ou 1
- **Informations de débogage** : Activées
- **Assertions** : Activées
- **Vérifications** : Range checking, Overflow checking, I/O checking activés
- **Répertoire de sortie** : `bin/debug/`

#### Mode Release
- **Optimisation** : Niveau 2 ou 3
- **Informations de débogage** : Désactivées (ou minimales)
- **Assertions** : Désactivées
- **Vérifications** : Désactivées
- **Strip symbols** : Activé (pour réduire la taille de l'exécutable)
- **Répertoire de sortie** : `bin/release/`

### Compilation conditionnelle multi-plateforme

Dans votre code, utilisez des directives pour gérer les différences entre plateformes :

```pascal
{$IFDEF WINDOWS}
  // Code spécifique Windows
  ConfigPath := GetEnvironmentVariable('APPDATA') + '\MonApp\';
{$ENDIF}

{$IFDEF LINUX}
  // Code spécifique Linux
  ConfigPath := GetEnvironmentVariable('HOME') + '/.monapp/';
{$ENDIF}

{$IFDEF UNIX}
  // Code commun à tous les Unix (Linux, macOS, BSD)
  PathSeparator := '/';
{$ENDIF}
```

## Scripts de build avancés

### Script multi-plateforme avec Makefile

Un `Makefile` peut être utilisé sur les deux plateformes si vous avez `make` installé (natif sur Linux, disponible via MinGW ou Cygwin sur Windows).

Créez un fichier `Makefile` :

```makefile
# Configuration
PROJECT_NAME = MonApplication  
PROJECT_FILE = $(PROJECT_NAME).lpi  
LAZBUILD = lazbuild  
BUILD_DIR = build  
BIN_DIR = bin

# Détection automatique de la plateforme
ifeq ($(OS),Windows_NT)
    PLATFORM = windows
    EXE_EXT = .exe
else
    PLATFORM = linux
    EXE_EXT =
endif

# Cibles principales
.PHONY: all clean debug release install

all: release

debug:
	@echo "=== Build Debug pour $(PLATFORM) ==="
	$(LAZBUILD) --build-mode=Debug $(PROJECT_FILE)
	@echo "Build Debug terminé"

release:
	@echo "=== Build Release pour $(PLATFORM) ==="
	$(LAZBUILD) --build-mode=Release $(PROJECT_FILE)
	@echo "Build Release terminé"

clean:
	@echo "=== Nettoyage ==="
	@rm -rf lib backup $(BIN_DIR)/*$(EXE_EXT)
	@echo "Nettoyage terminé"

rebuild: clean release

install: release
	@echo "=== Installation ==="
ifeq ($(PLATFORM),linux)
	@sudo cp $(BIN_DIR)/$(PROJECT_NAME) /usr/local/bin/
	@echo "Installation dans /usr/local/bin/ terminée"
else
	@echo "Installation non implémentée pour Windows"
endif
```

Utilisation :

```bash
# Compiler en mode release
make release

# Compiler en mode debug
make debug

# Nettoyer et recompiler
make rebuild

# Nettoyer
make clean
```

### Script avec tests automatisés

Créez un fichier `build-and-test.sh` :

```bash
#!/bin/bash

set -e  # Arrête le script en cas d'erreur

PROJECT_NAME="MonApplication"  
PROJECT_FILE="$PROJECT_NAME.lpi"  
TEST_PROJECT="tests/TestsUnitaires.lpi"

echo "======================================"  
echo "Build et tests automatiques"  
echo "======================================"

# Build principal
echo ""  
echo "1. Compilation du projet principal..."  
lazbuild --build-mode=Release "$PROJECT_FILE"

# Build des tests
if [ -f "$TEST_PROJECT" ]; then
    echo ""
    echo "2. Compilation des tests unitaires..."
    lazbuild --build-mode=Debug "$TEST_PROJECT"

    # Exécution des tests
    echo ""
    echo "3. Exécution des tests..."
    ./bin/TestsUnitaires

    if [ $? -eq 0 ]; then
        echo "✓ Tous les tests sont passés avec succès"
    else
        echo "✗ Des tests ont échoué"
        exit 1
    fi
else
    echo ""
    echo "2. Aucun projet de test trouvé, passage ignoré"
fi

# Vérification de l'exécutable
echo ""  
echo "4. Vérification de l'exécutable..."  
if [ -f "bin/$PROJECT_NAME" ]; then
    echo "✓ Exécutable créé : bin/$PROJECT_NAME"
    ls -lh "bin/$PROJECT_NAME"
else
    echo "✗ Exécutable non trouvé!"
    exit 1
fi

echo ""  
echo "======================================"  
echo "Build et tests terminés avec succès!"  
echo "======================================"
```

## Cross-compilation : compiler pour une autre plateforme

La cross-compilation permet de compiler pour Windows depuis Linux, ou vice-versa.

### Compiler pour Windows depuis Ubuntu

1. **Installer les outils de cross-compilation** :

```bash
sudo apt-get install fpc-source  
cd /usr/lib/fpc/$(fpc -iV)/  
sudo make crossinstall OS_TARGET=win64 CPU_TARGET=x86_64
```

2. **Compiler votre projet** :

```bash
lazbuild --os=win64 --cpu=x86_64 MonApplication.lpi
```

### Compiler pour Linux depuis Windows

C'est plus complexe et nécessite une chaîne de compilation croisée. Une alternative plus simple est d'utiliser WSL (Windows Subsystem for Linux) ou une machine virtuelle.

**Avec WSL :**

```bash
# Dans WSL Ubuntu
sudo apt-get install lazarus fpc  
cd /mnt/c/mes_projets/MonApplication  
lazbuild MonApplication.lpi
```

## Intégration avec les systèmes de versioning

### Script de build avec Git

Créez un fichier `build-versioned.sh` :

```bash
#!/bin/bash

# Récupérer les informations Git
GIT_COMMIT=$(git rev-parse --short HEAD)  
GIT_BRANCH=$(git rev-parse --abbrev-ref HEAD)  
BUILD_DATE=$(date +"%Y-%m-%d %H:%M:%S")

echo "======================================"  
echo "Build versionné"  
echo "Commit: $GIT_COMMIT"  
echo "Branche: $GIT_BRANCH"  
echo "Date: $BUILD_DATE"  
echo "======================================"

# Créer un fichier de version
cat > version.inc <<EOF  
const
  APP_VERSION = '1.0.0';
  GIT_COMMIT = '$GIT_COMMIT';
  GIT_BRANCH = '$GIT_BRANCH';
  BUILD_DATE = '$BUILD_DATE';
EOF

# Compiler
lazbuild --build-mode=Release MonApplication.lpi

# Renommer l'exécutable avec la version
mv bin/MonApplication bin/MonApplication-$GIT_COMMIT
```

Dans votre code Pascal, incluez le fichier de version :

```pascal
{$I version.inc}

procedure TFormPrincipal.AfficherVersion;  
begin
  ShowMessage(Format('Version %s'#13#10'Commit: %s'#13#10'Date: %s',
    [APP_VERSION, GIT_COMMIT, BUILD_DATE]));
end;
```

## Organisation des fichiers de build

### Structure de projet recommandée

```
MonProjet/
├── src/                    # Code source
│   ├── main.pas
│   └── units/
├── tests/                  # Tests unitaires
│   └── TestsUnitaires.lpi
├── bin/                    # Exécutables compilés
│   ├── debug/
│   └── release/
├── lib/                    # Fichiers objets temporaires
├── resources/              # Ressources (icônes, images)
├── docs/                   # Documentation
├── scripts/                # Scripts de build
│   ├── build-windows.bat
│   ├── build-linux.sh
│   └── build-all.sh
├── MonProjet.lpi           # Projet Lazarus principal
├── MonProjet.lpr           # Code source principal
├── Makefile                # Makefile optionnel
└── README.md               # Documentation
```

### Fichier .gitignore

Pour éviter de versionner les fichiers compilés :

```gitignore
# Fichiers compilés
*.exe
*.dll
*.so
*.o
*.ppu
*.compiled
*.rst

# Répertoires de build
lib/  
bin/  
backup/

# Fichiers Lazarus temporaires
*.lps
*.bak
*.~*
```

## Gestion des dépendances

### Bibliothèques externes

Si votre projet utilise des bibliothèques externes (DLL sur Windows, .so sur Linux), créez un script de copie automatique :

```bash
#!/bin/bash

PROJECT_NAME="MonApplication"  
BIN_DIR="bin"  
LIB_DIR="libs"

echo "Copie des dépendances..."

if [[ "$OSTYPE" == "linux-gnu"* ]]; then
    # Linux
    cp "$LIB_DIR/linux/"*.so "$BIN_DIR/"
    echo "Bibliothèques Linux copiées"
elif [[ "$OSTYPE" == "msys" ]] || [[ "$OSTYPE" == "win32" ]]; then
    # Windows
    cp "$LIB_DIR/windows/"*.dll "$BIN_DIR/"
    echo "Bibliothèques Windows copiées"
fi

echo "Dépendances copiées avec succès"
```

### Packages Lazarus

Si votre projet nécessite des packages spécifiques, documentez-les dans un fichier `DEPENDENCIES.md` :

```markdown
# Dépendances du projet

## Packages Lazarus requis

- LazControls (inclus dans Lazarus)
- BGRABitmap (à installer via Online Package Manager)
- Synapse (à télécharger et compiler)

## Installation

### Ubuntu
sudo apt-get install lazarus-ide-gtk2  
Dans Lazarus: Package → Online Package Manager → BGRABitmap → Install

### Windows
Télécharger l'installeur Lazarus depuis https://www.lazarus-ide.org  
Dans Lazarus: Package → Online Package Manager → BGRABitmap → Install
```

## Logs et rapports de build

### Script avec génération de logs

```bash
#!/bin/bash

LOG_DIR="build-logs"  
LOG_FILE="$LOG_DIR/build-$(date +%Y%m%d-%H%M%S).log"

mkdir -p "$LOG_DIR"

echo "Démarrage du build..." | tee "$LOG_FILE"  
echo "Date: $(date)" | tee -a "$LOG_FILE"  
echo "======================================" | tee -a "$LOG_FILE"

# Compiler avec redirection des logs
lazbuild --build-mode=Release MonApplication.lpi 2>&1 | tee -a "$LOG_FILE"

BUILD_RESULT=$?

if [ $BUILD_RESULT -eq 0 ]; then
    echo "" | tee -a "$LOG_FILE"
    echo "✓ Build réussi!" | tee -a "$LOG_FILE"
else
    echo "" | tee -a "$LOG_FILE"
    echo "✗ Build échoué!" | tee -a "$LOG_FILE"
    exit 1
fi

echo "Log sauvegardé: $LOG_FILE"
```

## Bonnes pratiques

### 1. Séparer les modes de build

Utilisez toujours au minimum deux modes :
- **Debug** : pour le développement, avec tous les contrôles activés
- **Release** : pour la production, optimisé et sans informations de débogage

### 2. Automatiser tôt dans le projet

Mettez en place l'automatisation dès le début du projet, pas à la fin.

### 3. Tester sur les plateformes cibles

L'automatisation ne remplace pas les tests réels. Testez régulièrement vos builds sur Windows et Ubuntu.

### 4. Documenter le processus

Créez un fichier `BUILD.md` expliquant comment compiler le projet :

```markdown
# Instructions de build

## Prérequis
- Lazarus 2.2.0 ou supérieur
- FreePascal 3.2.2 ou supérieur

## Compilation manuelle
1. Ouvrir MonApplication.lpi dans Lazarus
2. Menu Exécuter → Compiler

## Compilation automatique

### Windows
Exécuter: scripts\build-windows.bat

### Linux
Exécuter: ./scripts/build-linux.sh

## Modes de build disponibles
- Debug: Pour le développement
- Release: Pour la distribution
```

### 5. Utiliser des chemins relatifs

Évitez les chemins absolus dans vos scripts. Utilisez des chemins relatifs au répertoire du projet.

### 6. Gérer les erreurs

Vos scripts doivent toujours vérifier le succès de chaque étape et arrêter le processus en cas d'erreur.

## Résolution des problèmes courants

### Erreur "lazbuild not found"

**Sur Linux :**
```bash
# Vérifier l'emplacement
which lazbuild

# Si non trouvé, créer un lien symbolique
sudo ln -s /usr/lib/lazarus/lazbuild /usr/local/bin/lazbuild
```

**Sur Windows :**
Ajoutez le répertoire Lazarus au PATH système ou utilisez le chemin complet dans vos scripts.

### Erreur de permissions (Linux)

```bash
# Rendre le script exécutable
chmod +x build-linux.sh

# Problème de permissions sur les répertoires
sudo chown -R $USER:$USER .
```

### Différences d'encodage entre plateformes

Assurez-vous que vos fichiers sources utilisent UTF-8 sans BOM (Byte Order Mark) pour une compatibilité maximale.

### Chemins de fichiers

Utilisez toujours les fonctions de FreePascal pour gérer les chemins :

```pascal
uses
  SysUtils;

var
  CheminConfig: string;
begin
  // Portable : utilise le bon séparateur selon l'OS
  CheminConfig := IncludeTrailingPathDelimiter(GetAppConfigDir(False)) + 'config.ini';
end;
```

## Conclusion

L'automatisation des builds multi-plateformes est un investissement qui se rentabilise rapidement. Elle garantit que votre application FreePascal/Lazarus fonctionne de manière cohérente sur Windows et Ubuntu, tout en simplifiant votre flux de travail de développement.

Les outils présentés dans ce chapitre constituent une base solide pour créer un système de build adapté à vos besoins spécifiques. N'hésitez pas à les personnaliser et à les enrichir au fur et à mesure de l'évolution de votre projet.

Dans le prochain chapitre, nous verrons comment créer des packages d'installation professionnels pour distribuer votre application sur les deux plateformes.

⏭️ [Packaging et distribution](/22-devops-deploiement-multi-os/06-packaging-distribution.md)
