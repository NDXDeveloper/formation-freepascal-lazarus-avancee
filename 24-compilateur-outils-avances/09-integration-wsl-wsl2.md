🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 24.9 Intégration WSL/WSL2

## Introduction

**WSL** (Windows Subsystem for Linux) est une fonctionnalité de Windows 10/11 qui permet d'exécuter un environnement Linux directement sous Windows, sans machine virtuelle ni dual-boot. **WSL2** est la version améliorée qui utilise un véritable noyau Linux.

**Analogie simple :** Imaginez avoir deux maisons côte à côte avec une porte qui les relie. WSL vous permet d'avoir Windows (maison 1) et Linux (maison 2) qui communiquent facilement, partageant fichiers et outils sans avoir besoin de déménager.

**Pourquoi WSL pour le développement FreePascal/Lazarus ?**
- Développer et tester sous Linux sans quitter Windows
- Compiler pour Linux depuis Windows
- Tester le comportement multi-plateforme facilement
- Utiliser les outils Linux natifs
- Partager le code entre environnements

---

## Installation de WSL2

### Vérifier la compatibilité

**Prérequis :**
- Windows 10 version 2004 ou supérieure (Build 19041+)
- Windows 11 (toutes versions)
- Virtualisation activée dans le BIOS

**Vérifier votre version de Windows :**
```cmd
winver
```

### Installation rapide (Windows 11 / Windows 10 récent)

**Méthode la plus simple :**

```powershell
# Ouvrir PowerShell en tant qu'administrateur
wsl --install
```

Cette commande :
- Active les fonctionnalités WSL
- Installe WSL2
- Télécharge Ubuntu (distribution par défaut)
- Redémarre automatiquement si nécessaire

**Après redémarrage :**
- Ubuntu se lance automatiquement
- Créer un nom d'utilisateur Linux
- Créer un mot de passe

### Installation manuelle (Windows 10 ancien)

Si la commande `wsl --install` ne fonctionne pas :

**Étape 1 : Activer WSL**
```powershell
dism.exe /online /enable-feature /featurename:Microsoft-Windows-Subsystem-Linux /all /norestart
```

**Étape 2 : Activer la plateforme de machine virtuelle**
```powershell
dism.exe /online /enable-feature /featurename:VirtualMachinePlatform /all /norestart
```

**Étape 3 : Redémarrer Windows**

**Étape 4 : Télécharger le package de mise à jour WSL2**
- Aller sur : https://aka.ms/wsl2kernel
- Télécharger et installer le package

**Étape 5 : Définir WSL2 par défaut**
```powershell
wsl --set-default-version 2
```

**Étape 6 : Installer une distribution Linux**
- Ouvrir le Microsoft Store
- Rechercher "Ubuntu" (ou "Ubuntu 22.04 LTS")
- Cliquer sur "Obtenir" puis "Installer"
- Lancer Ubuntu depuis le menu Démarrer

### Vérifier l'installation

```powershell
# Lister les distributions installées
wsl --list --verbose

# Exemple de sortie :
# NAME      STATE           VERSION
# Ubuntu    Running         2
```

---

## Configuration de l'environnement Ubuntu sous WSL2

### Premier lancement

Lors du premier lancement d'Ubuntu :
```bash
# Créer votre utilisateur
Enter new UNIX username: votrenom
New password: ********
Retype new password: ********
```

### Mise à jour du système

```bash
# Mettre à jour la liste des paquets
sudo apt update

# Mettre à jour les paquets installés
sudo apt upgrade -y
```

### Installation de FreePascal et Lazarus

**Option 1 : Installation depuis les dépôts officiels**
```bash
# Installer FreePascal
sudo apt install fpc

# Installer Lazarus (avec interface graphique)
sudo apt install lazarus lazarus-ide

# Vérifier les versions
fpc -iV
lazbuild --version
```

**Option 2 : Installation depuis les PPA (versions plus récentes)**
```bash
# Ajouter le PPA
sudo add-apt-repository ppa:lazarus-team/lazarus
sudo apt update

# Installer
sudo apt install lazarus
```

**Option 3 : Installation manuelle (dernière version)**
```bash
# Télécharger depuis le site officiel
cd ~/Downloads
wget https://sourceforge.net/projects/lazarus/files/Lazarus%20Linux%20amd64%20DEB/Lazarus%203.4/lazarus-project_3.4.0-0_amd64.deb

# Installer
sudo dpkg -i lazarus-project_3.4.0-0_amd64.deb
sudo apt-get install -f  # Résoudre les dépendances
```

### Vérification de l'installation

```bash
# Tester FreePascal
echo 'program test; begin writeln("Hello from WSL!"); end.' > test.pas
fpc test.pas
./test

# Résultat attendu : Hello from WSL!
```

---

## Système de fichiers : Windows ↔ Linux

### Accéder aux fichiers Windows depuis WSL

Les disques Windows sont montés automatiquement dans `/mnt/` :

```bash
# Accéder au disque C:
cd /mnt/c

# Accéder au dossier Documents Windows
cd /mnt/c/Users/VotreNom/Documents

# Lister les fichiers
ls -la

# Exemple : naviguer vers un projet
cd /mnt/c/Users/VotreNom/Projects/MonProjet
```

**Structure des montages :**
```
/mnt/c/     → C:\
/mnt/d/     → D:\
/mnt/e/     → E:\
etc.
```

### Accéder aux fichiers Linux depuis Windows

**Méthode 1 : Explorateur de fichiers Windows**
```
\\wsl$\Ubuntu\home\votrenom\
```

Dans l'Explorateur Windows, taper dans la barre d'adresse :
```
\\wsl$\Ubuntu
```

**Méthode 2 : Créer un raccourci réseau**
- Ouvrir "Ce PC"
- Clic droit → "Ajouter un emplacement réseau"
- Entrer : `\\wsl$\Ubuntu`

**Méthode 3 : Ligne de commande Windows**
```cmd
explorer.exe \\wsl$\Ubuntu\home\votrenom
```

### Bonnes pratiques pour les chemins

**❌ À éviter : Travailler sur /mnt/c depuis WSL**
```bash
# LENT - Les performances sont dégradées
cd /mnt/c/Users/VotreNom/Projects
fpc monprojet.pas  # Sera plus lent
```

**✅ Recommandé : Travailler dans le système de fichiers Linux**
```bash
# RAPIDE - Performances natives Linux
cd ~/projects
fpc monprojet.pas  # Sera rapide
```

**Pourquoi ?**
- WSL2 accède aux fichiers Linux de manière native (rapide)
- L'accès à /mnt/c passe par une couche de traduction (lent)

**Solution : Dupliquer ou lier les projets**
```bash
# Option 1 : Copier le projet
cp -r /mnt/c/Users/VotreNom/Projects/MonProjet ~/projects/

# Option 2 : Créer un lien symbolique
ln -s /mnt/c/Users/VotreNom/Projects ~/windows-projects
```

---

## Développement multi-plateforme avec WSL2

### Scénario 1 : Développer sous Windows, compiler sous Linux

**Structure recommandée :**
```
C:\Projects\MonProjet\     (code source Windows)
~/projects/MonProjet/      (lien vers le code ou copie)
```

**Script de synchronisation Windows → WSL**

**Fichier : `sync_to_wsl.bat`**
```batch
@echo off
echo Synchronisation vers WSL...

:: Définir les chemins
set WINDOWS_PROJECT=C:\Projects\MonProjet
set WSL_PROJECT=/home/votrenom/projects/MonProjet

:: Synchroniser avec rsync (plus rapide)
wsl rsync -av --delete "%WINDOWS_PROJECT%/" "%WSL_PROJECT%/"

echo Synchronisation terminée!
pause
```

**Script de compilation dans WSL**

**Fichier : `build_linux.bat`**
```batch
@echo off
echo Compilation Linux via WSL...

wsl bash -c "cd ~/projects/MonProjet && ./build.sh"

if %ERRORLEVEL% EQU 0 (
    echo Build Linux réussi!
) else (
    echo Erreur lors du build Linux
)

pause
```

### Scénario 2 : Projet partagé entre Windows et WSL

**Configuration du projet**

**Fichier : `build_multi.sh`** (dans le projet)
```bash
#!/bin/bash

# Détecter l'environnement
if grep -qi microsoft /proc/version; then
    echo "Environnement : WSL"
    PLATFORM="linux-wsl"
else
    echo "Environnement : Linux natif"
    PLATFORM="linux"
fi

# Compiler
echo "Compilation pour $PLATFORM..."
lazbuild --build-mode=Release projet.lpi

# Copier l'exécutable dans le bon dossier
if [ "$PLATFORM" = "linux-wsl" ]; then
    # Copier vers Windows pour faciliter les tests
    cp projet /mnt/c/Users/VotreNom/Desktop/projet-linux
fi

echo "Build terminé!"
```

### Scénario 3 : Développement hybride avec VS Code

**Installation de Visual Studio Code**
- VS Code sous Windows peut éditer des fichiers dans WSL
- Extension WSL pour VS Code

**Configuration :**
```bash
# Dans WSL, installer code
# (se fait automatiquement si VS Code Windows est installé)
code .
```

Cela ouvre VS Code Windows avec l'espace de travail WSL.

**Avantages :**
- Édition Windows rapide
- Compilation Linux native
- Terminal intégré WSL
- Débogage dans les deux environnements

---

## Compilation croisée avec WSL2

### Compiler pour Windows depuis WSL

**Installation du cross-compilateur**
```bash
# Installer les outils de cross-compilation
sudo apt install mingw-w64

# Installer FPC avec support Windows
sudo apt install fpc-source

# Configurer fpcupdeluxe pour cross-compilation
# (permet de compiler vers Windows)
```

**Compilation pour Windows depuis WSL**
```bash
# Compiler un programme pour Windows 64-bit
fpc -Twin64 -Px86_64 monprogramme.pas

# Le .exe généré peut être exécuté sous Windows
```

**Script de build universel**

**Fichier : `crossbuild.sh`**
```bash
#!/bin/bash

PROJECT="MonProjet"
PROJECT_FILE="src/${PROJECT}.lpi"

echo "================================"
echo " Cross-Compilation Multi-OS"
echo "================================"
echo

# Build Linux
echo "[1/2] Compilation Linux..."
lazbuild --os=linux --cpu=x86_64 "$PROJECT_FILE"
mv "$PROJECT" "bin/${PROJECT}-linux"

# Build Windows
echo "[2/2] Compilation Windows..."
lazbuild --os=win64 --cpu=x86_64 "$PROJECT_FILE"
mv "${PROJECT}.exe" "bin/${PROJECT}.exe"

echo
echo "Build terminé!"
echo "  Linux:   bin/${PROJECT}-linux"
echo "  Windows: bin/${PROJECT}.exe"
```

### Tester les exécutables Windows depuis WSL

WSL2 permet d'exécuter des .exe Windows directement :

```bash
# Compiler pour Windows
lazbuild --os=win64 projet.lpi

# Exécuter le .exe depuis WSL
./projet.exe

# Résultat : Le programme Windows s'exécute!
```

**Limites :**
- Les programmes GUI Windows ne s'affichent pas correctement
- Préférer tester les GUI sous Windows natif

---

## Interface graphique dans WSL2

### Option 1 : WSLg (Windows 11 et Windows 10 récent)

**WSLg** (WSL Graphics) est intégré à WSL2 et permet d'exécuter des applications Linux avec interface graphique.

**Vérifier si WSLg est disponible :**
```bash
# Vérifier la variable DISPLAY
echo $DISPLAY

# Si elle affiche quelque chose comme ":0", WSLg est actif
```

**Lancer Lazarus IDE dans WSL2 :**
```bash
# Installer Lazarus
sudo apt install lazarus

# Lancer l'IDE
lazarus-ide &

# L'interface graphique s'ouvre dans Windows!
```

**Tester une application GUI :**
```bash
# Installer un éditeur graphique
sudo apt install gedit

# Lancer
gedit &

# La fenêtre s'ouvre dans Windows
```

### Option 2 : Serveur X11 (Windows 10 ancien)

Si WSLg n'est pas disponible, utiliser un serveur X11.

**Installation de VcXsrv (Windows) :**
- Télécharger : https://sourceforge.net/projects/vcxsrv/
- Installer VcXsrv
- Lancer "XLaunch"
- Choisir "Multiple windows"
- Choisir "Start no client"
- **Important** : Cocher "Disable access control"

**Configuration WSL :**
```bash
# Éditer ~/.bashrc
nano ~/.bashrc

# Ajouter à la fin :
export DISPLAY=$(cat /etc/resolv.conf | grep nameserver | awk '{print $2}'):0
export LIBGL_ALWAYS_INDIRECT=1

# Recharger
source ~/.bashrc
```

**Tester :**
```bash
# Installer xeyes (programme de test)
sudo apt install x11-apps

# Lancer
xeyes

# Si une fenêtre avec des yeux apparaît, X11 fonctionne!
```

### Option 3 : Mode console uniquement

Pour le développement sans GUI :

```bash
# Utiliser lazbuild au lieu de l'IDE
lazbuild projet.lpi

# Éditer avec vim ou nano
vim src/main.pas

# Compiler en ligne de commande
fpc src/main.pas
```

---

## Intégration des outils de build

### Makefile multi-environnement

**Fichier : `Makefile`**
```makefile
# Détecter l'environnement
ifeq ($(OS),Windows_NT)
    PLATFORM := windows
    EXE := .exe
    RM := del /Q
    MKDIR := mkdir
else
    UNAME := $(shell uname -s)
    ifeq ($(UNAME),Linux)
        # Détecter si c'est WSL
        ifneq (,$(findstring microsoft,$(shell cat /proc/version 2>/dev/null)))
            PLATFORM := wsl
        else
            PLATFORM := linux
        endif
    endif
    EXE :=
    RM := rm -f
    MKDIR := mkdir -p
endif

# Configuration
PROJECT := MonProjet
BUILD_DIR := bin

.PHONY: all clean

all:
	@echo "Building on $(PLATFORM)..."
	$(MKDIR) $(BUILD_DIR)
	lazbuild --build-mode=Release $(PROJECT).lpi
	@echo "Build completed for $(PLATFORM)"

clean:
	$(RM) $(BUILD_DIR)/*$(EXE)
	@echo "Cleaned"
```

### Script de build intelligent

**Fichier : `smart_build.sh`**
```bash
#!/bin/bash

# Couleurs pour l'affichage
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Détecter l'environnement
detect_environment() {
    if grep -qi microsoft /proc/version 2>/dev/null; then
        echo "wsl"
    elif [[ "$OSTYPE" == "linux-gnu"* ]]; then
        echo "linux"
    elif [[ "$OSTYPE" == "darwin"* ]]; then
        echo "macos"
    else
        echo "unknown"
    fi
}

ENV=$(detect_environment)

echo -e "${YELLOW}Environnement détecté : $ENV${NC}"

# Configuration selon l'environnement
case $ENV in
    wsl)
        echo "Configuration WSL..."
        OUTPUT_DIR="bin/wsl"
        # Copier aussi vers Windows pour faciliter les tests
        WINDOWS_OUTPUT="/mnt/c/Users/$USER/Desktop/builds"
        mkdir -p "$WINDOWS_OUTPUT"
        ;;
    linux)
        echo "Configuration Linux natif..."
        OUTPUT_DIR="bin/linux"
        ;;
    *)
        echo -e "${RED}Environnement non supporté${NC}"
        exit 1
        ;;
esac

# Créer les dossiers
mkdir -p "$OUTPUT_DIR"

# Compiler
echo -e "${GREEN}Compilation...${NC}"
lazbuild --build-mode=Release projet.lpi

if [ $? -eq 0 ]; then
    echo -e "${GREEN}✓ Compilation réussie${NC}"

    # Copier l'exécutable
    cp projet "$OUTPUT_DIR/"

    # Si WSL, copier aussi vers Windows
    if [ "$ENV" = "wsl" ]; then
        cp projet "$WINDOWS_OUTPUT/projet-linux"
        echo -e "${GREEN}✓ Copié vers Windows: $WINDOWS_OUTPUT${NC}"
    fi
else
    echo -e "${RED}✗ Compilation échouée${NC}"
    exit 1
fi
```

---

## Débogage avec WSL2

### Utiliser GDB dans WSL

```bash
# Installer GDB
sudo apt install gdb

# Compiler avec symboles de debug
fpc -g monprogramme.pas

# Déboguer
gdb ./monprogramme

# Commandes GDB basiques :
# run           - Exécuter
# break main    - Point d'arrêt
# next          - Ligne suivante
# print var     - Afficher variable
# quit          - Quitter
```

### Debug à distance depuis Windows

**Configuration pour VS Code :**

**Fichier : `.vscode/launch.json`**
```json
{
    "version": "0.2.0",
    "configurations": [
        {
            "name": "Debug in WSL",
            "type": "cppdbg",
            "request": "launch",
            "program": "${workspaceFolder}/monprogramme",
            "args": [],
            "stopAtEntry": false,
            "cwd": "${workspaceFolder}",
            "environment": [],
            "externalConsole": false,
            "MIMode": "gdb",
            "miDebuggerPath": "/usr/bin/gdb",
            "setupCommands": [
                {
                    "description": "Enable pretty-printing",
                    "text": "-enable-pretty-printing",
                    "ignoreFailures": true
                }
            ],
            "pipeTransport": {
                "debuggerPath": "/usr/bin/gdb",
                "pipeProgram": "wsl",
                "pipeArgs": [],
                "pipeCwd": ""
            }
        }
    ]
}
```

---

## Optimisations et astuces

### Améliorer les performances

**1. Désactiver Windows Defender pour les fichiers WSL**
```powershell
# Exécuter en PowerShell Administrateur
Add-MpPreference -ExclusionPath "\\wsl$"
```

**2. Limiter la RAM de WSL2**

**Fichier : `C:\Users\VotreNom\.wslconfig`**
```ini
[wsl2]
memory=4GB          # Limiter à 4GB
processors=4        # Utiliser 4 cœurs
swap=2GB           # 2GB de swap
localhostForwarding=true
```

**Après modification, redémarrer WSL :**
```powershell
wsl --shutdown
```

### Commandes WSL utiles

```powershell
# Lister les distributions
wsl --list --verbose

# Définir la distribution par défaut
wsl --set-default Ubuntu

# Arrêter WSL
wsl --shutdown

# Arrêter une distribution spécifique
wsl --terminate Ubuntu

# Exporter une distribution (backup)
wsl --export Ubuntu D:\Backup\ubuntu.tar

# Importer une distribution
wsl --import Ubuntu D:\WSL\Ubuntu D:\Backup\ubuntu.tar

# Désinstaller une distribution
wsl --unregister Ubuntu
```

### Accès réseau

**Depuis WSL vers Windows :**
```bash
# Windows est accessible via l'IP du host
ping $(cat /etc/resolv.conf | grep nameserver | awk '{print $2}')

# Exemple : accéder à un serveur web Windows
curl http://$(cat /etc/resolv.conf | grep nameserver | awk '{print $2}'):8080
```

**Depuis Windows vers WSL :**
```cmd
# WSL est accessible via localhost
ping localhost

# Accéder à un serveur dans WSL
curl http://localhost:3000
```

---

## Cas d'usage pratiques

### 1. Projet partagé Git

```bash
# Cloner le projet dans WSL
cd ~
git clone https://github.com/user/projet.git
cd projet

# Développer sous Windows via \\wsl$
# Compiler sous WSL
./build.sh

# Tester sous Windows
/mnt/c/chemin/vers/executable.exe
```

### 2. Tests automatisés multi-OS

**Fichier : `test_multi_os.sh`**
```bash
#!/bin/bash

echo "=== Tests Multi-OS ==="

# Test Linux
echo "[1/2] Tests Linux..."
lazbuild --os=linux tests/TestRunner.lpi
./tests/TestRunner

# Test Windows (via WSL)
echo "[2/2] Tests Windows..."
lazbuild --os=win64 tests/TestRunner.lpi
./tests/TestRunner.exe  # Exécuté via WSL

echo "Tests terminés!"
```

### 3. Pipeline de build complet

**Script : `pipeline.sh`**
```bash
#!/bin/bash

set -e  # Arrêter en cas d'erreur

echo "=== Pipeline de Build ==="

# Étape 1 : Nettoyage
echo "[1/5] Nettoyage..."
rm -rf bin/*

# Étape 2 : Build Linux
echo "[2/5] Build Linux..."
lazbuild --os=linux --build-mode=Release projet.lpi
mv projet bin/projet-linux

# Étape 3 : Build Windows
echo "[3/5] Build Windows..."
lazbuild --os=win64 --build-mode=Release projet.lpi
mv projet.exe bin/projet.exe

# Étape 4 : Tests
echo "[4/5] Exécution des tests..."
./bin/projet-linux --run-tests

# Étape 5 : Copie vers Windows
echo "[5/5] Copie vers Windows..."
cp bin/* /mnt/c/Users/$USER/Desktop/releases/

echo "Pipeline terminé!"
```

---

## Résolution des problèmes courants

### Problème : WSL2 ne démarre pas

**Solution :**
```powershell
# Vérifier le service WSL
Get-Service -Name "*WSL*"

# Redémarrer le service
Restart-Service LxssManager
```

### Problème : Performances lentes

**Causes et solutions :**
1. Travail sur /mnt/c → Déplacer vers ~/
2. Antivirus → Exclure \\wsl$
3. RAM insuffisante → Ajuster .wslconfig

### Problème : Interface graphique ne fonctionne pas

**WSL avec interface graphique disponible depuis le Windows store !**

---

## Conclusion

WSL2 est un outil puissant pour le développement FreePascal/Lazarus multi-plateforme :

✅ **Développement hybride** Windows + Linux  
✅ **Tests rapides** sur les deux OS  
✅ **Compilation croisée** facilitée  
✅ **Partage de code** transparent  
✅ **Performances** proches du natif

**Points clés à retenir :**
- Travailler dans le système de fichiers Linux pour les meilleures performances
- Utiliser \\wsl$ pour accéder aux fichiers depuis Windows
- Automatiser avec des scripts de build intelligents
- Profiter de WSLg pour les interfaces graphiques
- Optimiser la configuration avec .wslconfig

**Prochaines étapes :**
1. Installer WSL2 et Ubuntu
2. Configurer FreePascal/Lazarus
3. Créer un projet test multi-plateforme
4. Automatiser le workflow de build
5. Explorer l'intégration avec votre IDE préféré

WSL2 transforme Windows en une véritable station de développement multi-plateforme!

⏭️ [Remote debugging cross-platform](/24-compilateur-outils-avances/10-remote-debugging-cross-platform.md)
