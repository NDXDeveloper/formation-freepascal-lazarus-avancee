🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 26.11 Ressources spécifiques Windows/Ubuntu

## Introduction

Le développement multi-plateforme avec FreePascal et Lazarus nécessite une bonne connaissance des ressources spécifiques à chaque système d'exploitation. Cette section recense et organise les ressources essentielles pour développer efficacement sur Windows et Ubuntu/Linux, qu'il s'agisse de documentation, d'outils, de bibliothèques ou de communautés. Que vous développiez sur un seul OS ou sur les deux, ces ressources vous aideront à maîtriser les spécificités de chaque plateforme.

## Documentation officielle par plateforme

### Documentation Windows

**Microsoft Documentation :**

**API Windows (Win32) :**
- https://learn.microsoft.com/en-us/windows/win32/
  - Documentation complète de l'API Windows
  - Exemples en C/C++ (adaptables en Pascal)
  - Guide de migration entre versions Windows
  - Bonnes pratiques de développement Windows

**Sujets essentiels pour FreePascal :**
- **Gestion des fenêtres** : CreateWindow, WindowProc
- **GDI/GDI+** : Graphiques et dessin
- **Services Windows** : Création et gestion
- **Registry** : Lecture/écriture registre
- **COM/ActiveX** : Interopérabilité composants
- **Shell Extensions** : Intégration explorateur Windows

**PowerShell et scripting :**
- https://learn.microsoft.com/en-us/powershell/
  - Automatisation tâches Windows
  - Interaction avec applications FreePascal
  - Scripting de builds et déploiements

**Visual Studio Build Tools :**
- https://visualstudio.microsoft.com/downloads/
  - Compilateurs C/C++ pour bindings
  - Outils de debugging (dumpbin, depends)
  - Profiling et analyse

**Ressources FreePascal spécifiques Windows :**

- **Wiki FreePascal Windows** :
  - https://wiki.freepascal.org/Windows_Programming
  - https://wiki.freepascal.org/Windows_Services
  - https://wiki.freepascal.org/Winapi

- **Exemples de code** :
  - https://github.com/search?q=freepascal+windows+api
  - Packages OPM avec support Windows spécifique

### Documentation Linux/Ubuntu

**Documentation Ubuntu officielle :**

- **Ubuntu Documentation** :
  - https://help.ubuntu.com/
  - Guides d'administration système
  - Configuration réseau et services
  - Gestion des paquets

- **Ubuntu Server Guide** :
  - https://ubuntu.com/server/docs
  - Déploiement d'applications serveur
  - Services systemd
  - Configuration sécurité

**Documentation Linux générale :**

**Man Pages (manuels système) :**
```bash
# Dans le terminal Ubuntu
man 2 syscall_name    # Appels système (ex: man 2 open)  
man 3 function_name   # Fonctions bibliothèque C (ex: man 3 printf)  
man 7 topic          # Conventions et protocoles
```

Également disponible en ligne :
- https://man7.org/linux/man-pages/
- https://www.kernel.org/doc/man-pages/

**Freedesktop.org standards :**
- https://www.freedesktop.org/wiki/Specifications/
  - Desktop Entry Specification (.desktop files)
  - Icon Theme Specification
  - XDG Base Directory Specification
  - D-Bus specification

**Systemd :**
- https://www.freedesktop.org/wiki/Software/systemd/
- https://systemd.io/
  - Création d'unités de service
  - Journalctl et logging
  - Timers et automatisation

**Ressources FreePascal spécifiques Linux :**

- **Wiki FreePascal Linux** :
  - https://wiki.freepascal.org/Linux
  - https://wiki.freepascal.org/Unix
  - https://wiki.freepascal.org/systemd

- **GTK/Qt Documentation** :
  - https://docs.gtk.org/ (pour GTK2/GTK3)
  - https://doc.qt.io/ (pour Qt5/Qt6)

## Outils de développement par plateforme

### Outils Windows

**IDE et éditeurs :**

**1. Lazarus pour Windows**
- **Téléchargement** : https://www.lazarus-ide.org/index.php?page=downloads
- **Installeur officiel** : lazarus-[version]-fpc-[version]-win64.exe
- **Version portable** : Disponible pour clé USB
- **Chocolatey** : `choco install lazarus`

**2. Visual Studio Code avec extensions Pascal**
- **VS Code** : https://code.visualstudio.com/
- **Extension Pascal** : OmniPascal, Pascal Language Server
- **Configuration pour FPC** :
```json
{
  "pascal.compiler": "fpc",
  "pascal.compilerPath": "C:\\lazarus\\fpc\\3.2.2\\bin\\x86_64-win64\\fpc.exe"
}
```

**3. Notepad++ pour édition rapide**
- https://notepad-plus-plus.org/
- Support syntaxe Pascal natif
- Léger et rapide

**Outils de compilation et build :**

**1. MSBuild et scripts**
- Intégré à Windows SDK
- Automatisation builds complexes
- Intégration CI/CD

**2. Make pour Windows**
- **GNU Make** : Via MSYS2 ou MinGW
- **Installation MSYS2** : https://www.msys2.org/
```bash
# Dans MSYS2
pacman -S make
```

**3. CMake**
- https://cmake.org/download/
- Génération Makefiles cross-platform
- Intégration avec FreePascal

**Débogage et profiling :**

**1. GDB pour Windows**
- **Fourni avec Lazarus** : lazarus\mingw\[arch]\bin\gdb.exe
- **Standalone** : Via MSYS2 ou TDM-GCC

**2. Dr. Memory**
- https://drmemory.org/
- Détection fuites mémoire
- Alternative à Valgrind pour Windows

**3. Process Explorer**
- https://learn.microsoft.com/en-us/sysinternals/downloads/process-explorer
- Monitoring applications en temps réel
- Analyse ressources système

**4. Dependency Walker**
- https://www.dependencywalker.com/
- Analyse dépendances DLL
- Détection de DLL manquantes

**Outils réseau et communication :**

**1. Wireshark**
- https://www.wireshark.org/
- Analyse trafic réseau
- Debugging protocoles

**2. Postman**
- https://www.postman.com/
- Test API REST
- Documentation API

**Virtualisation et conteneurs :**

**1. Docker Desktop pour Windows**
- https://www.docker.com/products/docker-desktop
- Conteneurisation applications
- Support WSL2

**2. VirtualBox**
- https://www.virtualbox.org/
- Machines virtuelles Linux pour tests
- Snapshots et clonage

**3. WSL2 (Windows Subsystem for Linux)**
- Intégré à Windows 10/11
```powershell
# Installation
wsl --install -d Ubuntu
```
- Accès terminal Linux natif
- Développement cross-platform simplifié

**Gestionnaires de paquets :**

**1. Chocolatey**
- https://chocolatey.org/
- Installation automatisée logiciels
```powershell
# Exemples
choco install lazarus  
choco install git  
choco install vscode
```

**2. Scoop**
- https://scoop.sh/
- Alternative à Chocolatey
- Installations portables

### Outils Ubuntu/Linux

**IDE et éditeurs :**

**1. Lazarus pour Ubuntu**
- **APT (Ubuntu officiel - souvent ancien)** :
```bash
sudo apt update  
sudo apt install lazarus
```

- **PPA (version plus récente)** :
```bash
sudo add-apt-repository ppa:lazarus-team/lazarus  
sudo apt update  
sudo apt install lazarus
```

- **FPCUpDeluxe (recommandé)** :
  - https://github.com/LongDirtyAnimAlf/fpcupdeluxe
  - Installation dernières versions FPC et Lazarus
  - Gestion cross-compilation
  - Interface graphique simple

**2. Visual Studio Code**
```bash
# Installation via Snap
sudo snap install code --classic

# Ou via .deb
wget https://code.visualstudio.com/sha/download?build=stable&os=linux-deb-x64  
sudo apt install ./code_*.deb
```

**3. Vim/Neovim avec support Pascal**
```bash
sudo apt install vim
# Plugin Pascal : https://github.com/vim-pascal/vim-pascal
```

**4. Geany**
```bash
sudo apt install geany
```
- IDE léger avec support Pascal
- Bon pour éditions rapides

**Outils de compilation et build :**

**1. Make et build tools**
```bash
sudo apt install build-essential  
sudo apt install make cmake
```

**2. FPC depuis source**
```bash
# Dépendances
sudo apt install subversion git  
sudo apt install binutils  
sudo apt install libgtk-3-dev

# Compilation FPC
svn checkout https://svn.freepascal.org/svn/fpc/trunk fpc  
cd fpc  
make clean all  
sudo make install
```

**Débogage et profiling :**

**1. GDB (GNU Debugger)**
```bash
sudo apt install gdb
# Utilisation
gdb ./mon_programme
```

**2. Valgrind**
```bash
sudo apt install valgrind
# Détection fuites mémoire
valgrind --leak-check=full ./mon_programme
```

**3. Perf**
```bash
sudo apt install linux-tools-common linux-tools-generic
# Profiling performance
perf record ./mon_programme  
perf report
```

**4. Heaptrack**
```bash
sudo apt install heaptrack
# Analyse allocation mémoire
heaptrack ./mon_programme  
heaptrack_gui heaptrack.mon_programme.*.gz
```

**Outils système et monitoring :**

**1. htop/btop**
```bash
sudo apt install htop
# Ou version moderne
sudo snap install btop
```

**2. strace**
```bash
sudo apt install strace
# Trace appels système
strace ./mon_programme
```

**3. lsof**
```bash
sudo apt install lsof
# Fichiers ouverts par processus
lsof -p [PID]
```

**Outils réseau :**

**1. tcpdump/Wireshark**
```bash
sudo apt install tcpdump wireshark
```

**2. curl/wget**
```bash
sudo apt install curl wget
# Test API REST
curl -X GET https://api.example.com/data
```

**Conteneurisation :**

**1. Docker**
```bash
sudo apt update  
sudo apt install docker.io  
sudo systemctl start docker  
sudo systemctl enable docker  
sudo usermod -aG docker $USER
```

**2. Podman (alternative sans daemon)**
```bash
sudo apt install podman
```

**Gestionnaires de paquets :**

**1. APT (natif)**
```bash
sudo apt install [package]  
sudo apt search [terme]  
sudo apt update && sudo apt upgrade
```

**2. Snap**
```bash
sudo snap install [package]  
sudo snap find [terme]
```

**3. Flatpak**
```bash
sudo apt install flatpak  
flatpak install flathub [app-id]
```

## Bibliothèques et composants spécifiques

### Bibliothèques Windows

**API Windows et système :**

**1. jwa/jwapi (JEDI Windows API)**
- https://github.com/jwa-dev/jwa
- Headers Pascal complets pour Win32 API
- Traductions de windows.h
- Installation via OPM

**2. Windows SDK wrappers**
- Inclus avec FreePascal (units Windows, JwaWindows)
- Accès direct à toutes fonctions Win32

**Composants UI Windows :**

**1. KControls**
- https://github.com/kryslt/KControls
- Composants natifs Windows avancés
- Grilles, éditeurs, graphiques
- Compatible Lazarus

**2. Virtual TreeView**
- https://github.com/Virtual-TreeView/Virtual-TreeView
- Arbre hiérarchique haute performance
- Alternative supérieure à TTreeView

**Bases de données Windows :**

**1. ADO (ActiveX Data Objects)**
- Accès bases Microsoft (SQL Server, Access)
- Via COM/ActiveX
- Unit TADOConnection dans Lazarus

**2. ODBC**
- Drivers natifs Windows
- SQLdb avec ODBC connector

**Graphiques et multimédia :**

**1. DirectX headers**
- https://github.com/alexandreapj/dx-pas
- DirectX pour Pascal
- Gaming et graphiques 3D

**2. Windows Imaging Component (WIC)**
- Gestion images Windows native
- Formats variés (PNG, JPEG, TIFF, etc.)

**Communication :**

**1. Windows Sockets 2 (Winsock)**
- Inclus dans FreePascal (unit WinSock2)
- Programmation réseau Windows

**2. Named Pipes**
- IPC (Inter-Process Communication) Windows
- Unit Pipes dans FreePascal

### Bibliothèques Linux/Ubuntu

**Système et POSIX :**

**1. BaseUnix, Unix, Linux**
- Units FreePascal standard
- Appels système POSIX
- Gestion processus, fichiers, signaux

**2. libc wrappers**
- Accès fonctions C standard
- Unit CTypes pour compatibilité

**Interface graphique GTK :**

**1. GTK2/GTK3**
- Widgets natifs GNOME
- Installation :
```bash
sudo apt install libgtk-3-dev
```
- Lazarus utilise automatiquement

**2. libadwaita (GTK4)**
- Style moderne GNOME
- Composants Material Design

**Interface graphique Qt :**

**1. Qt5/Qt6 Pas**
- Bindings Qt pour Pascal
- Installation :
```bash
sudo apt install qt5-default libqt5pas-dev
```

**Bases de données Linux :**

**1. PostgreSQL libpq**
```bash
sudo apt install libpq-dev
```
- Accès direct PostgreSQL
- Unit PQConnection dans SQLdb

**2. MySQL/MariaDB**
```bash
sudo apt install libmysqlclient-dev
```
- Unit MySQLConnection

**3. SQLite**
```bash
sudo apt install libsqlite3-dev
```
- Base de données embarquée
- Parfait pour applications locales

**Graphiques et multimédia :**

**1. Cairo**
```bash
sudo apt install libcairo2-dev
```
- Dessin vectoriel 2D
- Utilisé par GTK

**2. SDL2**
```bash
sudo apt install libsdl2-dev
```
- Gaming et multimédia
- Bindings Pascal disponibles

**3. OpenGL/Mesa**
```bash
sudo apt install libgl-dev mesa-utils
```
- Graphiques 3D
- Unit GL dans FreePascal

**Audio :**

**1. ALSA**
```bash
sudo apt install libasound2-dev
```
- Audio bas niveau Linux

**2. PulseAudio**
```bash
sudo apt install libpulse-dev
```
- Serveur audio moderne

**3. OpenAL**
```bash
sudo apt install libopenal-dev
```
- Audio 3D cross-platform

**Communication et réseau :**

**1. D-Bus**
```bash
sudo apt install libdbus-1-dev
```
- IPC Linux standard
- Communication entre applications

**2. systemd**
- Gestion services
- Socket activation
- Journalisation

**Cryptographie et sécurité :**

**1. OpenSSL**
```bash
sudo apt install libssl-dev
```
- Cryptographie, TLS/SSL
- Unit OpenSSL dans FreePascal

**2. GnuTLS**
```bash
sudo apt install libgnutls28-dev
```
- Alternative OpenSSL

## Packages et composants cross-platform recommandés

### Packages essentiels (Windows + Linux)

**1. Synapse**
- **URL** : http://www.ararat.cz/synapse/
- **Installation** : Via OPM ou manuel
- **Fonctionnalités** :
  - TCP/IP, UDP, HTTP, FTP, SMTP
  - SSL/TLS avec OpenSSL
  - Léger et stable
  - Parfait pour applications réseau

**2. Indy (Internet Direct)**
- **URL** : https://github.com/IndySockets/Indy
- **Installation** : Via OPM
- **Fonctionnalités** :
  - Protocoles internet variés
  - Clients et serveurs
  - Support SSL/TLS

**3. mORMot**
- **URL** : https://github.com/synopse/mORMot2
- **Fonctionnalités** :
  - ORM haute performance
  - REST server/client
  - SOA (Service-Oriented Architecture)
  - Cross-platform complet

**4. ZEOS DBO**
- **URL** : https://sourceforge.net/projects/zeoslib/
- **Installation** : Via OPM
- **Fonctionnalités** :
  - Accès bases de données multiples
  - PostgreSQL, MySQL, SQLite, Oracle, etc.
  - Alternative à SQLdb

**5. BGRABitmap**
- **URL** : https://github.com/bgrabitmap/bgrabitmap
- **Installation** : Via OPM
- **Fonctionnalités** :
  - Graphiques 2D avancés
  - Anti-aliasing, effets
  - Très performant

**6. Brook Framework**
- **URL** : https://github.com/risoflora/brookframework
- **Fonctionnalités** :
  - Web framework moderne
  - REST API
  - MVC pattern

**7. fpJSON / JSONTools**
- **Inclus** : RTL FreePascal
- **Fonctionnalités** :
  - Parsing JSON
  - Génération JSON
  - API moderne

**8. LazUtils**
- **Inclus** : Lazarus
- **Fonctionnalités** :
  - Utilitaires cross-platform
  - FileUtil, LazFileUtils
  - Gestion chemins multi-OS

### Online Package Manager (OPM)

**Installation et utilisation :**

Dans Lazarus :
1. Menu **Package → Online Package Manager**
2. Rechercher package souhaité
3. Cliquer "Install"
4. Rebuild IDE si nécessaire

**Packages populaires disponibles :**
- Graphics et UI : BGRAControls, ATTabs, ATButtons
- Bases de données : ZeosDBO, Brook, mORMot
- Réseau : Synapse, Indy, fpWeb
- Utilitaires : DCPcrypt, LazSerial, JsonTools
- Gaming : Castle Game Engine, ZenGL

**Repository officiel :**
- https://packages.lazarus-ide.org/

## Tutoriels et guides pratiques

### Tutoriels Windows spécifiques

**1. Wiki FreePascal Windows**
- https://wiki.freepascal.org/Windows_Programming_Tips
- **Sujets** :
  - Créer une application Windows sans LCL
  - Utiliser API Win32 directement
  - Créer services Windows
  - Accéder au registre
  - COM/ActiveX

**2. Creating Windows Services**
- https://wiki.freepascal.org/Windows_Services
- Guide complet création de services
- Exemples de code

**3. Windows API Tutorial (English)**
- http://www.functionx.com/win32/
- Référence Win32 avec exemples
- Adaptable en Pascal

**4. DirectX with FreePascal**
- https://github.com/alexandreapj/dx-pas/wiki
- Tutoriels DirectX en Pascal
- Gaming sous Windows

### Tutoriels Linux/Ubuntu spécifiques

**1. Wiki FreePascal Linux**
- https://wiki.freepascal.org/Linux_Programming_Tips
- **Sujets** :
  - Programmation système Linux
  - GTK applications
  - D-Bus communication
  - Systemd services

**2. Creating systemd Services**
- https://wiki.freepascal.org/systemd
- Services Linux modernes
- Socket activation

**3. Linux System Programming**
- https://man7.org/training/
- Cours complet (C, mais concepts applicables)
- Processus, threads, IPC

**4. GTK Programming Guide**
- https://docs.gtk.org/gtk3/getting_started.html
- Guide officiel GTK
- Exemples adaptables en Pascal

### Tutoriels cross-platform

**1. Wiki Lazarus général**
- https://wiki.lazarus.freepascal.org/
- Tutoriels tous niveaux
- Exemples multi-plateforme

**2. FreePascal Complete Guide**
- https://www.freepascal.org/docs.html
- Documentation complète langage
- Guide de référence

**3. YouTube - Pascal Programming**
- **Chaîne "Pascal Programming TV"**
- Tutoriels vidéo gratuits
- Projets pratiques

**4. Udemy/Coursera**
- Chercher "Pascal Programming"
- Cours payants mais structurés

## Sites web et blogs communautaires

### Sites généralistes

**1. FreePascal Forum**
- https://forum.lazarus.freepascal.org/
- **Sections** :
  - General (discussions générales)
  - Beginners (débutants)
  - Windows (spécifique Windows)
  - Linux (spécifique Linux)
  - Databases, Networking, Graphics, etc.
- Communauté active et aidante

**2. Stack Overflow**
- https://stackoverflow.com/questions/tagged/freepascal
- https://stackoverflow.com/questions/tagged/lazarus
- Q&A techniques
- Réponses rapides

**3. Reddit**
- r/fpc : https://www.reddit.com/r/fpc/
- r/lazarus : https://www.reddit.com/r/lazarus/
- r/pascal : https://www.reddit.com/r/pascal/

### Blogs techniques spécialisés

**1. Blog Synopse (mORMot)**
- https://blog.synopse.info/
- Auteur de mORMot
- Articles techniques avancés
- Performance et optimisation

**2. Pascal Game Development**
- https://www.pascalgamedevelopment.com/
- Gaming avec Pascal
- Tutoriels et ressources

**3. Delphi Developer (applicable à FPC)**
- https://delphideveloper.com/
- Beaucoup applicable à FreePascal
- Composants et techniques

**4. Blaise Pascal Magazine (archive)**
- Ancien magazine, archives disponibles
- Articles de fond

### Blogs personnels notables

- **Marco Cantù** : Expert Delphi/Pascal (archives)
- **Michael Van Canneyt** : Core developer FPC
- **Mattias Gaertner** : Core developer Lazarus

## Communautés et support

### Forums et discussion

**Forums officiels :**

**1. Lazarus Forum**
- https://forum.lazarus.freepascal.org/
- ~50k membres
- Très actif (posts quotidiens)
- Support multilingue

**2. FreePascal Mailing Lists**
- fpc-pascal@lists.freepascal.org (général)
- fpc-devel@lists.freepascal.org (développement)
- lazarus@lists.lazarus.freepascal.org
- Archives : https://lists.freepascal.org/

**Plateformes de chat :**

**1. Discord**
- Serveurs FreePascal/Lazarus communautaires
- Chat temps réel
- Canaux thématiques

**2. IRC (historique, moins actif)**
- #lazarus-ide sur Libera.Chat
- #fpc sur Libera.Chat

**3. Telegram**
- Groupes FreePascal/Lazarus
- Communautés locales (FR, DE, RU, etc.)

### Support professionnel et consulting

**1. Support commercial**
- Lazarus Team propose support payant
- Développeurs freelance sur forum
- Sociétés spécialisées Pascal

**2. Formation**
- Formations en ligne
- Workshops occasionnels
- Support d'entreprise sur demande

## Livres et publications

### Livres en anglais

**1. "Mastering Pascal and Delphi Programming"**
- Marco Cantù
- Principes applicables à FreePascal

**2. "Essential Pascal"**
- Marco Cantù (gratuit en ligne)
- http://www.marcocantu.com/epascal/
- Excellent pour débuter

**3. "Modern Object Pascal Introduction for Programmers"**
- Michalis Kamburelis (Castle Game Engine)
- https://castle-engine.io/modern_pascal_introduction.html
- Gratuit, moderne, excellente ressource

### Livres en français

**1. "Programmer en Turbo Pascal"**
- Anciens livres Turbo Pascal restent pertinents
- Fondamentaux du langage

**2. "Lazarus Free Pascal"**
- Livres spécifiques Lazarus
- Chercher sur Amazon/FNAC

### Documentation officielle

**1. FreePascal Documentation**
- https://www.freepascal.org/docs.html
- Reference Guide (PDF)
- Programmer's Guide (PDF)
- RTL Reference

**2. Lazarus Documentation**
- https://www.lazarus-ide.org/documentation.php
- LCL Reference
- Component Writers Guide

## Chaînes YouTube et contenu vidéo

### Chaînes recommandées

**1. Pascal Programming TV**
- Tutoriels FreePascal/Lazarus
- Projets pratiques
- En anglais

**2. Delphi Praxis (applicable FPC)**
- Techniques avancées
- Beaucoup applicable à FreePascal

**3. Alcinoe Pascal**
- Composants et bibliothèques
- Tutoriels spécifiques

**4. The Coding Train (Programming concepts)**
- Pas spécifique Pascal
- Concepts programmation généraux

### Playlists utiles

- "FreePascal Tutorial Series"
- "Lazarus IDE Tutorials"
- "Pascal Game Development"
- "Database with Lazarus"

## Ressources pour débutants absolus

### Parcours d'apprentissage recommandé

**Étape 1 : Bases du langage (2-4 semaines)**

1. **Installation**
   - Windows : https://wiki.freepascal.org/Installing_Lazarus#Windows
   - Ubuntu : https://wiki.freepascal.org/Installing_Lazarus#Ubuntu_Linux

2. **Premiers programmes**
   - https://wiki.lazarus.freepascal.org/Lazarus_Tutorial
   - Programme "Hello World"
   - Variables et types
   - Structures de contrôle

3. **Livre recommandé**
   - "Modern Object Pascal Introduction for Programmers"
   - Gratuit et moderne

**Étape 2 : Interface graphique (2-3 semaines)**

1. **LCL Basics**
   - https://wiki.lazarus.freepascal.org/LCL_Components
   - Boutons, labels, edits
   - Événements

2. **Tutoriels pratiques**
   - Créer calculatrice simple
   - Gestionnaire de tâches
   - Lecteur de fichiers texte

**Étape 3 : Bases de données (2-3 semaines)**

1. **SQLite avec Lazarus**
   - https://wiki.freepascal.org/SQLdb_Tutorial1
   - Base de données locale
   - CRUD operations

2. **Projet pratique**
   - Carnet d'adresses
   - Gestion de stock simple

**Étape 4 : Multi-plateforme (2-3 semaines)**

1. **Compilation cross-platform**
   - Tester même application Windows/Linux
   - Gérer chemins de fichiers
   - Gestion des différences UI

2. **Projet final débutant**
   - Application complète fonctionnant sur les deux OS

### Ressources en français

**1. developpez.com**
- https://lazarus.developpez.com/
- Tutoriels en français
- Forum actif français

**2. YouTube français**
- "Programmation Pascal"
- "Tutoriel Lazarus"
- Contenus variés

**3. Communauté francophone**
- Section française du forum Lazarus
- Discord francophones

## Environnements de test et CI/CD

### Windows

**1. GitHub Actions**
```yaml
# Exemple workflow Windows
name: Windows Build  
on: [push]  
jobs:
  build:
    runs-on: windows-latest
    steps:
      - uses: actions/checkout@v2
      - name: Install Lazarus
        run: choco install lazarus
      - name: Build
        run: lazbuild projet.lpi
```

**2. AppVeyor**
- https://www.appveyor.com/
- CI/CD gratuit pour open source
- Support Windows natif

**3. Azure Pipelines**
- Support Windows, Linux, macOS
- Gratuit pour projets open source

### Ubuntu/Linux

**1. GitHub Actions**
```yaml
# Exemple workflow Ubuntu
name: Ubuntu Build  
on: [push]  
jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - name: Install Lazarus
        run: |
          sudo add-apt-repository ppa:lazarus-team/lazarus
          sudo apt update
          sudo apt install lazarus
      - name: Build
        run: lazbuild projet.lpi
```

**2. GitLab CI/CD**
- https://gitlab.com/
- Runners Linux gratuits
- Excellente intégration Docker

**3. Travis CI**
- Support Linux et macOS
- Configuration .travis.yml

### Docker pour tests multi-OS

**Image FreePascal officielle**
```dockerfile
FROM ubuntu:22.04

RUN apt-get update && apt-get install -y \
    fpc \
    lazarus \
    lcl

WORKDIR /app  
COPY . /app

RUN lazbuild projet.lpi  
CMD ["./projet"]
```

**Images communautaires**
- https://hub.docker.com/search?q=freepascal
- https://hub.docker.com/search?q=lazarus

## Outils de packaging et distribution

### Windows

**1. Inno Setup**
- https://jrsoftware.org/isinfo.php
- Créateur d'installateurs Windows
- Script simple
- Gratuit

**2. NSIS**
- https://nsis.sourceforge.io/
- Alternative à Inno Setup
- Plus de contrôle

**3. MSI (Windows Installer)**
- WiX Toolset : https://wixtoolset.org/
- Installateurs professionnels
- Intégration Windows complète

**4. PortableApps.com Format**
- https://portableapps.com/development
- Applications portables
- Grande distribution

### Ubuntu/Linux

**1. DEB (Debian/Ubuntu)**
```bash
# Structure basique
mkdir -p monapp-1.0/DEBIAN  
mkdir -p monapp-1.0/usr/bin  
mkdir -p monapp-1.0/usr/share/applications

# Fichier control
cat > monapp-1.0/DEBIAN/control << EOF  
Package: monapp  
Version: 1.0  
Architecture: amd64  
Maintainer: Votre Nom  
Description: Description de l'application  
EOF

# Construire le paquet
dpkg-deb --build monapp-1.0
```

**2. AppImage**
- https://appimage.org/
- Application auto-contenue
- Fonctionne sur toutes distributions
- Outil : appimagetool

**3. Snap**
```yaml
# snapcraft.yaml
name: monapp  
version: '1.0'  
summary: Résumé  
description: Description complète  
base: core22  
confinement: strict  
apps:
  monapp:
    command: bin/monapp
parts:
  monapp:
    plugin: dump
    source: .
```

**4. Flatpak**
- https://flatpak.org/
- Distribution universelle
- Sandboxing

## Ressources de veille technologique

### Newsletters et agrégateurs

**1. FreePascal News**
- Newsletter officielle (sporadique)
- Annonces versions

**2. Developpez.com**
- Agrégateur actualités Pascal
- https://pascal.developpez.com/

**3. Reddit r/fpc et r/lazarus**
- Posts réguliers
- Nouvelles bibliothèques
- Discussions techniques

### Réseaux sociaux

**1. Twitter/X**
- @FreePascal_org
- @LazarusIDE
- #FreePascal #Lazarus

**2. LinkedIn**
- Groupe "Pascal Programming"
- Réseau professionnel

### Conférences et événements

**1. Delphi & Pascal Developer Days**
- Conférence annuelle (Europe)
- Présentations techniques
- Networking

**2. EKON**
- Conférence allemande Delphi/Pascal
- Applicable à FreePascal

**3. Meetups locaux**
- Chercher sur Meetup.com
- Groupes utilisateurs locaux

## Conclusion : Centraliser vos ressources

Le développement multi-plateforme avec FreePascal/Lazarus nécessite d'avoir accès rapidement aux bonnes ressources. Voici une méthode pour s'organiser :

**1. Bookmarks organisés**
Créez des dossiers de favoris :
- Documentation officielle
- Forums et communautés
- Outils Windows
- Outils Linux
- Packages favoris
- Tutoriels

**2. Environnement de développement standard**
Documentez votre setup :
- Versions FPC/Lazarus utilisées
- Packages installés
- Configuration IDE
- Scripts de build

**3. Veille active**
- Suivez forums (notifications email)
- Abonnez-vous aux newsletters
- Lisez blogs régulièrement
- Testez nouvelles bibliothèques

**4. Contribution**
- Partagez vos découvertes
- Aidez sur les forums
- Créez vos propres ressources
- Open source vos outils

**Les ressources ne manquent pas, mais sont parfois dispersées. Cette liste vous fournit les points d'entrée essentiels pour devenir autonome dans votre développement FreePascal/Lazarus sur Windows et Ubuntu.**

🔝 Retour au [Sommaire](/SOMMAIRE.md)
