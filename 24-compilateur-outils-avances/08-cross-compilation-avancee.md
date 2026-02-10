🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 24.8 Cross-compilation avancée

## Introduction

La **cross-compilation** (compilation croisée) est une technique puissante qui consiste à compiler un programme sur une plateforme (appelée **plateforme hôte**) pour qu'il s'exécute sur une autre plateforme différente (appelée **plateforme cible**). Cette approche est fondamentale dans le développement logiciel moderne et particulièrement bien supportée par FreePascal.

### Qu'est-ce que la cross-compilation ?

Imaginons que vous développiez sur un ordinateur Linux, mais que vous souhaitiez créer une application pour Windows. Plutôt que de :
1. Transférer votre code sur une machine Windows
2. Installer les outils de développement sur Windows
3. Compiler là-bas
4. Retransférer le binaire pour le distribuer

Vous pouvez simplement **compiler directement depuis Linux vers Windows** en une seule étape. C'est la cross-compilation.

### Pourquoi utiliser la cross-compilation ?

#### Avantages pratiques

**1. Gain de temps et productivité**
- Pas besoin de changer de machine ou de système d'exploitation
- Workflow de développement unifié
- Compilation de toutes les versions depuis un seul poste

**2. Économies de ressources**
- Un seul environnement de développement à maintenir
- Pas besoin de licences multiples pour différents OS
- Réduction des coûts matériels (une seule machine puissante)

**3. Automatisation facilitée**
- Builds automatisés pour toutes les plateformes
- Intégration CI/CD simplifiée
- Pipelines de déploiement multi-plateformes

**4. Développement pour plateformes spéciales**
- Systèmes embarqués (Raspberry Pi, Arduino)
- Plateformes mobiles (Android)
- Architectures spécifiques (ARM, MIPS)
- Systèmes sans environnement de développement complet

**5. Cohérence des versions**
- Même version du compilateur pour toutes les plateformes
- Reproductibilité des builds
- Débogage facilité avec un environnement unique

## Concepts fondamentaux

### Plateforme hôte vs plateforme cible

**Plateforme hôte (host)** : Le système sur lequel vous compilez
- Votre ordinateur de développement
- Votre serveur de build CI/CD
- Exemple : Ubuntu Linux 64 bits

**Plateforme cible (target)** : Le système sur lequel le programme s'exécutera
- Le système de déploiement final
- Peut être différent de l'hôte
- Exemple : Windows 64 bits, Raspberry Pi, Android

### Triplet d'architecture

Chaque plateforme est identifiée par un **triplet** (parfois quadruplet) :

```
<architecture>-<fabricant>-<système>-<abi>
```

**Exemples courants :**
- `x86_64-linux-gnu` : Linux 64 bits
- `x86_64-w64-mingw32` : Windows 64 bits
- `i686-w64-mingw32` : Windows 32 bits
- `arm-linux-gnueabihf` : ARM Linux avec virgule flottante matérielle
- `aarch64-linux-gnu` : ARM 64 bits Linux

**Décomposition :**
- **architecture** : Type de processeur (x86_64, i686, arm, aarch64, mips, etc.)
- **fabricant** : Généralement w64, pc, unknown, none
- **système** : OS cible (linux, mingw32, darwin, android, none pour bare-metal)
- **abi** : Interface binaire (gnu, gnueabihf, eabi, etc.)

### Composants d'une toolchain de cross-compilation

Pour cross-compiler, vous avez besoin d'une **toolchain complète** :

#### 1. Le compilateur croisé
- Version de FPC capable de générer du code pour la plateforme cible
- Comprend les phases de compilation (analyse, génération de code)

#### 2. L'assembleur cible
- Convertit le code assembleur en code machine pour la plateforme cible
- Exemple : `x86_64-w64-mingw32-as` pour Windows 64 bits

#### 3. L'éditeur de liens (linker)
- Combine les fichiers objets et bibliothèques
- Crée l'exécutable final pour la plateforme cible
- Exemple : `x86_64-w64-mingw32-ld` pour Windows

#### 4. Les bibliothèques système
- Bibliothèques standard de la plateforme cible
- Runtime C (libc sur Linux, msvcrt sur Windows)
- Bibliothèques système (Win32 API, POSIX, etc.)

#### 5. Les unités FreePascal cibles
- Unités RTL (Runtime Library) compilées pour la cible
- Unités FCL (Free Component Library) pour la cible
- Packages additionnels (LCL pour interfaces graphiques)

## Scénarios de cross-compilation avec FreePascal

### 1. Desktop vers Desktop

Le cas le plus courant pour les développeurs d'applications :

**Scénarios typiques :**
- **Linux → Windows** : Développer sur Linux, cibler Windows
- **Windows → Linux** : Développer sur Windows, cibler Linux
- **Linux → macOS** : Développer sur Linux, cibler macOS
- **macOS → Windows/Linux** : Développer sur macOS, cibler Windows ou Linux

**Cas d'usage :**
- Applications de bureau multi-plateformes
- Outils en ligne de commande portables
- Services et démons système
- Applications d'entreprise

### 2. Desktop vers Embarqué

Développement pour systèmes embarqués et IoT :

**Scénarios typiques :**
- **Linux/Windows → Raspberry Pi** : Applications pour Pi
- **Linux/Windows → ARM générique** : Systèmes embarqués Linux
- **Linux/Windows → Android** : Applications mobiles
- **Desktop → Microcontrôleurs** : Arduino, STM32, etc.

**Cas d'usage :**
- Domotique et IoT
- Systèmes industriels embarqués
- Robotique
- Appareils médicaux
- Instrumentation scientifique

### 3. Serveur vers Multiple

Automatisation dans les environnements CI/CD :

**Scénarios typiques :**
- **Serveur Linux → Tous les OS** : Build server unique
- **Container Docker → Multi-arch** : Builds dans conteneurs

**Cas d'usage :**
- Pipelines CI/CD automatisés
- Releases multi-plateformes
- Distribution logicielle
- Tests automatisés

### 4. Architectures spéciales

Ciblage d'architectures non-x86 :

**Scénarios typiques :**
- **x86_64 → ARM** : Serveurs ARM, edge computing
- **x86_64 → MIPS** : Routeurs, équipements réseau
- **x86_64 → RISC-V** : Nouvelle génération de processeurs

## Architecture de FreePascal pour la cross-compilation

### Design modulaire du compilateur

FreePascal est conçu dès l'origine pour supporter la cross-compilation :

```
Code Source Pascal
       ↓
   [Frontend]
   - Analyse lexicale
   - Analyse syntaxique
   - Analyse sémantique
       ↓
   [Middle-end]
   - Représentation intermédiaire
   - Optimisations indépendantes de la plateforme
       ↓
   [Backend spécifique à la cible]
   - Génération de code pour l'architecture cible
   - Optimisations spécifiques à l'architecture
       ↓
   Code Assembleur / Objet
       ↓
   [Linker de la plateforme cible]
       ↓
   Exécutable pour la plateforme cible
```

### Organisation des fichiers du compilateur

FreePascal organise ses fichiers de manière claire :

```
/usr/lib/fpc/3.2.2/
│
├── bin/                          # Binaires du compilateur
│   └── x86_64-linux/            # Compilateur natif
│       ├── fpc                  # Exécutable principal
│       ├── ppcx64               # Compilateur x64
│       └── ...
│
├── units/                        # Unités compilées
│   ├── x86_64-linux/            # Unités pour Linux 64 bits
│   │   ├── rtl/                 # Runtime library
│   │   ├── fcl-base/            # FCL base
│   │   ├── fcl-db/              # FCL database
│   │   └── ...
│   │
│   ├── x86_64-win64/            # Unités pour Windows 64 bits
│   │   ├── rtl/
│   │   └── ...
│   │
│   ├── i386-win32/              # Unités pour Windows 32 bits
│   │   └── ...
│   │
│   └── arm-linux/               # Unités pour ARM Linux
│       └── ...
│
└── source/                       # Code source du compilateur
    ├── compiler/                # Source du compilateur
    ├── rtl/                     # Source de la RTL
    └── packages/                # Source des packages
```

### Processus de compilation croisée

Voici ce qui se passe lors d'une cross-compilation :

**Étape 1 : Préparation**
```
Paramètres de compilation
  ├── -T<os>      : Système cible (linux, win64, darwin, etc.)
  ├── -P<cpu>     : Architecture cible (x86_64, i386, arm, etc.)
  └── -O<niveau>  : Niveau d'optimisation
```

**Étape 2 : Analyse du code**
```pascal
program Hello;  
begin
  WriteLn('Hello Cross-Compilation!');
end.
```
↓ Analyse indépendante de la plateforme

**Étape 3 : Génération de code**
```
Code Pascal → Représentation intermédiaire →  
Assembleur pour la cible → Fichiers objets
```

**Étape 4 : Liaison**
```
Fichiers objets (.o) + RTL cible + Bibliothèques système cible →  
Linker de la plateforme cible →  
Exécutable final (.exe, .elf, etc.)
```

## Différences entre compilation native et cross-compilation

### Compilation native

Sur votre plateforme actuelle, pour votre plateforme :

```bash
# Sur Linux, pour Linux
fpc monprogramme.pas
# Produit : monprogramme (ELF Linux)

# Sur Windows, pour Windows
fpc monprogramme.pas
# Produit : monprogramme.exe (PE Windows)
```

**Avantages :**
- Configuration simple
- Tout fonctionne "out of the box"
- Facile à tester immédiatement

**Inconvénients :**
- Nécessite l'OS cible pour compiler
- Workflow fragmenté pour multi-plateforme
- Duplication des environnements

### Cross-compilation

Sur une plateforme, pour une autre plateforme :

```bash
# Sur Linux, pour Windows
fpc -Twin64 -Px86_64 monprogramme.pas
# Produit : monprogramme.exe (PE Windows)

# Sur Windows, pour Linux
fpc -Tlinux -Px86_64 monprogramme.pas
# Produit : monprogramme (ELF Linux)
```

**Avantages :**
- Un seul environnement de développement
- Build multi-plateforme automatisable
- Productivité accrue

**Inconvénients :**
- Configuration initiale plus complexe
- Test sur plateforme cible nécessaire
- Gestion des dépendances spécifiques

## Prérequis généraux

### Connaissances requises

Avant de vous lancer dans la cross-compilation, il est recommandé de maîtriser :

1. **FreePascal/Lazarus de base**
   - Compilation simple en ligne de commande
   - Utilisation de l'IDE Lazarus
   - Structure d'un projet Pascal

2. **Concepts système**
   - Différences entre Windows et Linux
   - Chemins de fichiers (/ vs \)
   - Variables d'environnement
   - Permissions de fichiers

3. **Ligne de commande**
   - Navigation dans les répertoires
   - Exécution de scripts
   - Variables et paramètres

4. **Notions de compilation**
   - Fichiers sources, objets, exécutables
   - Bibliothèques statiques vs dynamiques
   - Éditeur de liens (linker)

### Outils nécessaires

**Sur tous les systèmes :**
- FreePascal 3.2.0 ou supérieur
- Git (pour récupérer des sources si nécessaire)
- Éditeur de texte ou IDE Lazarus

**Spécifique à la cross-compilation :**
- Cross-compilateurs pour vos plateformes cibles
- Bibliothèques de la plateforme cible
- Outils de test (Wine, QEMU, etc.)

### Espace disque requis

Pour une installation complète multi-plateformes :

```
FreePascal de base              : ~200 MB  
Lazarus IDE                     : ~150 MB  
Sources FPC                     : ~100 MB

Cross-compiler Windows          : ~300 MB  
Cross-compiler ARM              : ~200 MB  
Cross-compiler Android          : ~500 MB

Total recommandé                : ~2 GB
```

## Compilation conditionnelle

Un aspect crucial de la cross-compilation est la **compilation conditionnelle**, qui permet d'adapter le code selon la plateforme cible.

### Directives de base

FreePascal fournit des directives pour détecter la plateforme cible :

```pascal
program CrossPlatformApp;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,    // Nécessaire sur Unix
  {$ENDIF}
  SysUtils;

begin
  {$IFDEF WINDOWS}
  WriteLn('Compilé pour Windows');
  // Code spécifique Windows
  {$ENDIF}

  {$IFDEF LINUX}
  WriteLn('Compilé pour Linux');
  // Code spécifique Linux
  {$ENDIF}

  {$IFDEF DARWIN}
  WriteLn('Compilé pour macOS');
  // Code spécifique macOS
  {$ENDIF}

  {$IFDEF ANDROID}
  WriteLn('Compilé pour Android');
  // Code spécifique Android
  {$ENDIF}
end.
```

### Détection de l'architecture

```pascal
program ArchDetection;

begin
  {$IFDEF CPUX86_64}
  WriteLn('Architecture : x86_64 (64 bits)');
  {$ENDIF}

  {$IFDEF CPUI386}
  WriteLn('Architecture : i386 (32 bits)');
  {$ENDIF}

  {$IFDEF CPUARM}
  WriteLn('Architecture : ARM');
  {$ENDIF}

  {$IFDEF CPUAARCH64}
  WriteLn('Architecture : ARM64');
  {$ENDIF}
end.
```

### Macros prédéfinies

FreePascal définit automatiquement des macros utiles :

```pascal
program BuildInfo;

begin
  WriteLn('Système cible    : {$I %FPCTARGETOS%}');
  WriteLn('CPU cible        : {$I %FPCTARGETCPU%}');
  WriteLn('Version FPC      : {$I %FPCVERSION%}');
  WriteLn('Date compilation : {$I %DATE%}');
  WriteLn('Heure compilation: {$I %TIME%}');
end.
```

### Code portable

Pour écrire du code vraiment portable :

```pascal
program PortableApp;

uses
  SysUtils;

const
  // Séparateur de chemin portable
  CONFIG_DIR = 'config' + PathDelim + 'app.conf';

  // Fin de ligne portable
  {$IFDEF WINDOWS}
  LINE_END = #13#10;  // CRLF pour Windows
  {$ELSE}
  LINE_END = #10;     // LF pour Unix
  {$ENDIF}

var
  ConfigPath: string;

begin
  // Chemin de configuration adaptatif
  ConfigPath := GetAppConfigDir(False);
  WriteLn('Configuration : ', ConfigPath);

  // Écriture avec fin de ligne correcte
  WriteLn('Ligne 1' + LINE_END + 'Ligne 2');
end.
```

## Défis de la cross-compilation

### Différences de plateforme

**1. Format des exécutables**
- Windows : PE (Portable Executable) - .exe
- Linux : ELF (Executable and Linkable Format)
- macOS : Mach-O (Mach Object)

**2. Bibliothèques système**
- Windows : DLL (Dynamic Link Library)
- Linux : SO (Shared Object)
- macOS : DYLIB (Dynamic Library)

**3. Chemins de fichiers**
- Windows : `C:\Program Files\MonApp\`
- Linux : `/usr/local/bin/monapp`
- Séparateur : `\` (Windows) vs `/` (Unix)

**4. Conventions de nommage**
- Windows : Insensible à la casse
- Linux : Sensible à la casse
- Exemple : `fichier.txt` ≠ `Fichier.txt` sur Linux

**5. Fins de ligne**
- Windows : CRLF (`\r\n`)
- Unix/Linux : LF (`\n`)
- macOS ancien : CR (`\r`)

### Gestion des dépendances

Les bibliothèques externes posent des défis :

```pascal
// Chargement dynamique adaptatif
const
  {$IFDEF WINDOWS}
  SQLITE_LIB = 'sqlite3.dll';
  {$ENDIF}
  {$IFDEF LINUX}
  SQLITE_LIB = 'libsqlite3.so';
  {$ENDIF}
  {$IFDEF DARWIN}
  SQLITE_LIB = 'libsqlite3.dylib';
  {$ENDIF}
```

### Tests sur la plateforme cible

Vous devez toujours tester sur la vraie plateforme cible :

**Options de test :**
1. **Machine physique** : Le plus fiable
2. **Machine virtuelle** : VirtualBox, VMware
3. **Émulation** : Wine (Windows sur Linux), QEMU (ARM, etc.)
4. **WSL/WSL2** : Linux sur Windows
5. **Docker** : Conteneurs pour différentes plateformes
6. **Cloud** : Services de test en ligne

## Outils de facilitation

### fpcupdeluxe

**fpcupdeluxe** est un outil graphique qui simplifie énormément l'installation de cross-compilers :

- Interface graphique intuitive
- Installation automatique des toolchains
- Gestion des bibliothèques
- Configuration automatique
- Support de nombreuses plateformes

### lazbuild

**lazbuild** est l'outil en ligne de commande de Lazarus pour compiler sans l'IDE :

```bash
# Compiler un projet Lazarus en ligne de commande
lazbuild --os=win64 --cpu=x86_64 monprojet.lpi

# Avantages :
# - Automatisation facile
# - Intégration CI/CD
# - Scripts de build
```

### Make et scripts

Utilisation de Makefiles pour automatiser :

```makefile
# Makefile simple pour cross-compilation
all: linux windows

linux:
	fpc -Tlinux -Px86_64 monprojet.pas

windows:
	fpc -Twin64 -Px86_64 monprojet.pas

clean:
	rm -f *.o *.ppu monprojet monprojet.exe
```

## Organisation de ce chapitre

Ce chapitre est divisé en trois sections détaillées :

### 24.8.1 Windows → Linux
- Installation du cross-compiler sur Windows
- Configuration de l'environnement MinGW
- Compilation d'applications console et GUI
- Gestion des bibliothèques Linux depuis Windows
- Tests avec WSL et machines virtuelles

### 24.8.2 Linux → Windows
- Installation du cross-compiler sur Linux
- Utilisation de MinGW-w64
- Compilation d'applications Win32/Win64
- Gestion des ressources Windows
- Tests avec Wine

### 24.8.3 Toolchains croisés
- Installation de toolchains multiples
- Compilation pour ARM, Android, embarqué
- Gestion avancée des configurations
- Automatisation avec CI/CD
- Docker et conteneurisation

## Conseils avant de commencer

### 1. Commencez simplement

Ne tentez pas tout d'un coup. Progression recommandée :

1. **Compilez nativement** : Maîtrisez la compilation simple
2. **Premier cross-compile** : Choisissez une cible (Linux→Windows ou inverse)
3. **Automatisation** : Créez des scripts
4. **Multiple cibles** : Ajoutez d'autres plateformes
5. **CI/CD** : Intégrez dans vos pipelines

### 2. Documentez votre configuration

Gardez une trace de :
- Versions des outils installés
- Chemins de bibliothèques
- Options de compilation utilisées
- Problèmes rencontrés et solutions

### 3. Testez systématiquement

**Règle d'or** : Ne faites jamais confiance à un binaire cross-compilé sans l'avoir testé sur la plateforme cible réelle.

### 4. Utilisez le contrôle de version

Versionnez :
- Vos fichiers de configuration (.cfg)
- Vos scripts de build
- Vos Makefiles
- Votre documentation

### 5. Rejoignez la communauté

Ressources utiles :
- **Forum FreePascal** : https://forum.lazarus.freepascal.org/
- **Wiki Lazarus** : https://wiki.lazarus.freepascal.org/
- **Documentation FPC** : https://www.freepascal.org/docs.html
- **Stack Overflow** : Tag [freepascal] et [lazarus]

## Résumé

La cross-compilation avec FreePascal est :

✅ **Puissante** : Support natif de nombreuses plateformes  
✅ **Mature** : Technologie éprouvée depuis des années  
✅ **Bien documentée** : Communauté active et documentation riche  
✅ **Gratuite** : Tous les outils sont open source  
✅ **Productive** : Workflow unifié pour toutes les plateformes

Dans les sections suivantes, nous allons explorer en détail comment mettre en place et utiliser la cross-compilation dans vos projets, que vous développiez sur Windows, Linux, ou que vous cibliez des plateformes embarquées.

Prêt à compiler une application Windows depuis Linux, ou vice-versa ? Passons à la pratique !

⏭️ [Windows → Linux](/24-compilateur-outils-avances/08.1-windows-vers-linux.md)
