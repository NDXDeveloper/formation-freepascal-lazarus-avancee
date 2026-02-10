🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 1.5 Configuration avancée du compilateur FPC

## Introduction : Pourquoi configurer FPC ?

### Le compilateur, cœur de votre développement

FreePascal Compiler (FPC) est bien plus qu'un simple traducteur de code Pascal en binaire. C'est un outil sophistiqué qui peut être finement configuré pour :

- **Optimiser** les performances de vos applications
- **Cibler** différentes plateformes et architectures
- **Déboguer** efficacement vos programmes
- **Personnaliser** le comportement de compilation
- **Automatiser** votre workflow de développement

### Les trois niveaux de configuration

FPC peut être configuré à trois niveaux différents :

```
┌─────────────────────────────────────┐
│     1. Configuration globale        │
│         (/etc/fpc.cfg)              │
├─────────────────────────────────────┤
│     2. Configuration utilisateur    │
│       (~/.fpc.cfg ou %HOME%)        │
├─────────────────────────────────────┤
│     3. Configuration projet         │
│    (ligne de commande ou IDE)       │
└─────────────────────────────────────┘
```

Chaque niveau peut surcharger le précédent, offrant une flexibilité maximale.

## Le fichier de configuration fpc.cfg

### Localisation du fichier

Le fichier `fpc.cfg` est le centre névralgique de la configuration :

**Windows** :
```
C:\FPC\3.2.2\bin\x86_64-win64\fpc.cfg
C:\Users\%USERNAME%\fpc.cfg
%LOCALAPPDATA%\FreePascal\fpc.cfg
```

**Linux/Unix** :
```
/etc/fpc.cfg                 # Configuration système
~/.fpc.cfg                   # Configuration utilisateur
$XDG_CONFIG_HOME/fpc/fpc.cfg # Standard XDG
```

**macOS** :
```
/usr/local/etc/fpc.cfg       # Configuration système
~/.fpc.cfg                   # Configuration utilisateur
~/Library/Application Support/FreePascal/fpc.cfg
```

### Structure du fichier fpc.cfg

Le fichier utilise une syntaxe simple mais puissante :

```ini
# Commentaires commencent par #
# Variables avec $
# Conditions avec #IFDEF

# Chemins de recherche des unités
-Fu/usr/lib/fpc/$fpcversion/units/$fpctarget
-Fu/usr/lib/fpc/$fpcversion/units/$fpctarget/*
-Fu~/fpc/units

# Options de compilation
-O2        # Optimisation niveau 2
-Xs        # Strip les symboles
-CX        # Smartlinking
-XX        # Link intelligent

# Conditions selon la plateforme
#IFDEF WINDOWS
  -WG      # Application GUI Windows
#ENDIF

#IFDEF LINUX
  -Cg      # Génération de code PIC
#ENDIF

#IFDEF DEBUG
  -g       # Informations de débogage
  -gl      # Numéros de ligne
  -O-      # Pas d'optimisation
#ENDIF
```

### Variables prédéfinies

FPC définit automatiquement des variables utiles :

| Variable | Description | Exemple |
|----------|-------------|---------|
| `$fpcversion` | Version de FPC | 3.2.2 |
| `$fpctarget` | Cible CPU-OS | x86_64-linux |
| `$fpccpu` | Architecture CPU | x86_64 |
| `$fpcos` | Système d'exploitation | linux |
| `$fpcdate` | Date de compilation | 2024/01/15 |
| `$fpctime` | Heure de compilation | 14:30:00 |

### Création d'un fpc.cfg personnalisé

Créons un fichier de configuration optimisé :

```ini
# === Configuration FPC Personnalisée ===
# Auteur : Votre Nom
# Date : 2024

# === Chemins de base ===
# Adapter selon votre installation
#IFDEF WINDOWS
  -Fu$FPCDIR\units\$fpctarget
  -Fu$FPCDIR\units\$fpctarget\*
  -FE.\bin
  -FU.\lib
#ENDIF

#IFDEF UNIX
  -Fu/usr/lib/fpc/$fpcversion/units/$fpctarget
  -Fu/usr/lib/fpc/$fpcversion/units/$fpctarget/*
  -FE./bin
  -FU./lib
#ENDIF

# === Chemins personnels ===
# Mes bibliothèques personnelles
-Fu~/MyLibs/pascal
-Fi~/MyLibs/includes

# === Options par défaut ===
# Optimisations standards
-O2        # Optimisation niveau 2
-CX        # Smartlinking activé
-XX        # Link intelligent
-Xs        # Strip symboles en release

# Sécurité
-Cr        # Vérification des ranges
-Ct        # Vérification stack
-Ci        # Vérification I/O

# === Modes de compilation ===
#IFDEF DEBUG
  # Mode débogage
  -g         # Infos débogage
  -gl        # Numéros de ligne
  -gw3       # Format DWARF3
  -O-        # Pas d'optimisation
  -Sa        # Assertions activées
  -Cr        # Range checking
  -Ct        # Stack checking
  -Ci        # I/O checking
  -Co        # Overflow checking

  # Messages
  -vewnhi    # Warnings, errors, notes, hints, info
  -l         # Afficher les logos

  # Définir DEBUG
  -dDEBUG
#ENDIF

#IFDEF RELEASE
  # Mode release
  -O3        # Optimisation maximale
  -CX        # Smart linking
  -XX        # Smart linking
  -Xs        # Strip symbols

  # Désactiver les vérifications
  -Cr-       # Pas de range checking
  -Ct-       # Pas de stack checking
  -Ci-       # Pas de I/O checking
  -Co-       # Pas de overflow checking
  -Sa-       # Pas d'assertions

  # Messages minimaux
  -vew       # Seulement erreurs et warnings
  -l-        # Pas de logos

  # Définir RELEASE
  -dRELEASE
#ENDIF

# === Spécifique plateforme ===
#IFDEF WINDOWS
  -WG        # GUI application
  #IFDEF WIN64
    -Px86_64
  #ENDIF
#ENDIF

#IFDEF LINUX
  -Cg        # Code PIC pour bibliothèques partagées
#ENDIF

#IFDEF DARWIN
  -k-macosx_version_min -k10.10  # Version min macOS
#ENDIF

# === Encoding et locale ===
-FcUTF8      # Fichiers source en UTF-8

# === Fin de configuration ===
```

## Options de compilation essentielles

### Options d'optimisation

#### Niveaux d'optimisation (-O)

```bash
# Pas d'optimisation (débogage)
fpc -O- program.pas

# Optimisations rapides et sûres
fpc -O1 program.pas

# Optimisations standards (recommandé)
fpc -O2 program.pas

# Optimisations maximales
fpc -O3 program.pas

# Optimisations agressives (expérimental)
fpc -O4 program.pas
```

#### Optimisations spécifiques

```bash
# Optimisation pour la taille
fpc -Os program.pas

# Optimisation pour un CPU spécifique
fpc -CpCOREAVX2 program.pas    # Intel Core avec AVX2
fpc -CpARMV7 program.pas       # ARM v7

# Inline des fonctions
fpc -Si program.pas

# Utiliser les registres
fpc -Or program.pas

# Optimisations de boucles
fpc -OoLOOPUNROLL program.pas
```

### Options de débogage

#### Informations de débogage

```bash
# Format de débogage
fpc -g program.pas          # Format par défaut
fpc -gw2 program.pas        # DWARF 2
fpc -gw3 program.pas        # DWARF 3 (recommandé Linux)
fpc -gs program.pas         # Stabs

# Niveau de détail
fpc -gl program.pas         # Numéros de ligne
fpc -gc program.pas         # Génération de code pour gdb
fpc -gh program.pas         # Heap trace (détection fuites)
fpc -gv program.pas         # Valgrind compatible

# Combinaison typique débogage
fpc -g -gl -gh -Cr -Ct -Ci program.pas
```

### Options de vérification (checking)

```bash
# Range checking (indices tableaux)
fpc -Cr program.pas

# Stack checking (débordement pile)
fpc -Ct program.pas

# I/O checking (erreurs fichiers)
fpc -Ci program.pas

# Overflow checking (débordements entiers)
fpc -Co program.pas

# Assertions
fpc -Sa program.pas

# Tout activer (développement)
fpc -Cr -Ct -Ci -Co -Sa program.pas

# Tout désactiver (production)
fpc -Cr- -Ct- -Ci- -Co- -Sa- program.pas
```

### Options de liaison (linking)

```bash
# Smart linking (réduit la taille)
fpc -CX -XX program.pas

# Strip symbols (encore plus petit)
fpc -Xs program.pas

# Bibliothèque statique
fpc -static program.pas

# Bibliothèque dynamique
fpc -shared library.pas

# Définir le nom de sortie
fpc -o"MyApp.exe" program.pas

# Définir les chemins
fpc -FE./bin -FU./lib program.pas
# -FE : Répertoire des exécutables
# -FU : Répertoire des unités compilées
```

## Chemins et recherche d'unités

### Configuration des chemins

#### Chemins de recherche d'unités (-Fu)

```bash
# Ajouter un chemin
fpc -Fu/home/user/myunits program.pas

# Plusieurs chemins
fpc -Fu/path1 -Fu/path2 -Fu/path3 program.pas

# Récursif avec wildcard
fpc -Fu/home/user/units/* program.pas

# Dans fpc.cfg
-Fu$LAZARUS_DIR/components/*/lib/$fpctarget
```

#### Chemins d'include (-Fi)

```bash
# Fichiers include
fpc -Fi/home/user/includes program.pas

# Pour les directives {$I filename}
-Fi../includes
-Fi../../common
```

#### Chemins de bibliothèques (-Fl)

```bash
# Bibliothèques système
fpc -Fl/usr/local/lib program.pas

# Bibliothèques Windows
fpc -FlC:\mylibs program.pas

# Dans fpc.cfg
#IFDEF WINDOWS
  -FlC:\Windows\System32
#ENDIF
```

### Organisation recommandée des projets

```
MonProjet/
├── src/              # Sources
│   ├── main.pas
│   └── units/
├── bin/              # Exécutables (-FE)
│   ├── debug/
│   └── release/
├── lib/              # Unités compilées (-FU)
│   ├── debug/
│   └── release/
├── include/          # Fichiers include (-Fi)
├── docs/             # Documentation
└── project.cfg       # Config projet
```

Configuration associée :
```ini
# project.cfg
-Fusrc/units
-Fiinclude
-FEbin/$BUILDMODE
-FUlib/$BUILDMODE

#IFDEF DEBUG
  -dBUILDMODE=debug
#ELSE
  -dBUILDMODE=release
#ENDIF
```

## Directives de compilation

### Directives dans le code source

Les directives permettent de configurer la compilation directement dans le code :

```pascal
{$MODE OBJFPC}        // Mode Object Pascal
{$H+}                 // Strings longs (AnsiString)
{$INLINE ON}          // Autoriser inline
{$OPTIMIZATION ON}    // Optimisations activées

{$IFDEF WINDOWS}
  {$APPTYPE GUI}      // Application Windows GUI
{$ENDIF}

{$R+}                 // Range checking ON
{$Q+}                 // Overflow checking ON
{$S+}                 // Stack checking ON

{$WARNINGS ON}
{$HINTS ON}
{$NOTES ON}

// Directives locales
{$PUSH}               // Sauvegarder état
{$R-}                 // Désactiver range checking
// Code critique en performance
{$POP}                // Restaurer état
```

### Directives conditionnelles

```pascal
// Définir des symboles (booléens, pas de valeur)
{$DEFINE DEBUG}
{$DEFINE VERSION2}

// Pour définir des macros avec une valeur, activer {$MACRO ON}
{$MACRO ON}
{$DEFINE AppVersion := '2.0'}
// Sans {$MACRO ON}, la syntaxe {$DEFINE Name := value} provoque une erreur

// Conditions simples
{$IFDEF DEBUG}
  WriteLn('Mode debug activé');
{$ENDIF}

// Conditions complexes
{$IF DEFINED(DEBUG) AND NOT DEFINED(RELEASE)}
  {$MESSAGE HINT 'Compilation en mode debug'}
{$ENDIF}

// Selon la version FPC
{$IF FPC_VERSION >= 3}
  {$IF FPC_VERSION = 3}
    {$IF FPC_RELEASE >= 2}
      // Code pour FPC 3.2+
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

// Selon la plateforme
{$IFDEF MSWINDOWS}
  // Code Windows
{$ELSE}
  {$IFDEF UNIX}
    {$IFDEF LINUX}
      // Code Linux
    {$ENDIF}
    {$IFDEF DARWIN}
      // Code macOS
    {$ENDIF}
  {$ENDIF}
{$ENDIF}
```

### Directives de ressources

```pascal
// Inclure des ressources
{$R *.res}            // Fichier ressource automatique
{$R myicon.res}       // Icône spécifique

// Windows : informations de version
{$IFDEF WINDOWS}
  {$R version.res}
{$ENDIF}

// Inclure des fichiers
{$I configuration.inc}
{$INCLUDE types.inc}

// Lier des bibliothèques
{$LINKLIB m}          // Bibliothèque math
{$LINKLIB pthread}    // Threads POSIX

// macOS : frameworks
{$IFDEF DARWIN}
  {$LINKFRAMEWORK Cocoa}
  {$LINKFRAMEWORK Carbon}
{$ENDIF}
```

## Modes de compilation

### Les différents modes disponibles

FPC supporte plusieurs modes de syntaxe :

```pascal
{$MODE FPC}           // Mode FPC par défaut
{$MODE OBJFPC}        // Mode Object Pascal FPC (recommandé)
{$MODE DELPHI}        // Compatibilité Delphi maximale
{$MODE TP}            // Turbo Pascal 7.0
{$MODE MACPAS}        // Mac Pascal
{$MODE ISO}           // ISO Pascal standard
```

### Comparaison des modes principaux

| Fonctionnalité | FPC | OBJFPC | DELPHI | TP |
|---------------|-----|---------|--------|-----|
| Classes | ✅ | ✅ | ✅ | ❌ |
| Exceptions | ✅ | ✅ | ✅ | ❌ |
| AnsiStrings | ✅ | ✅ | ✅ | ❌ |
| Operator overload | ✅ | ✅ | ⚠️ | ❌ |
| Generics | ✅ | ✅ | ✅ | ❌ |
| Properties | ✅ | ✅ | ✅ | ❌ |
| @ = pointer | ❌ | ❌ | ✅ | ✅ |

### Configuration par mode

```ini
# Dans fpc.cfg - Configuration selon le mode

#IFDEF MODE_DELPHI
  -Mdelphi
  -Sh       # AnsiStrings par défaut (équivalent {$H+})
#ENDIF

#IFDEF MODE_OBJFPC
  -Mobjfpc
  -Sh       # AnsiStrings par défaut (équivalent {$H+})
  -Sc       # Opérateurs C (*=, +=, etc.)
  -Sg       # Goto autorisé
  -Si       # Inline supporté
#ENDIF

#IFDEF MODE_TP
  -Mtp
  -So       # Compatibilité TP7
#ENDIF
```

## Cross-compilation

### Configuration pour compiler vers d'autres plateformes

#### Principes de base

```bash
# Syntaxe générale
fpc -T<OS> -P<CPU> program.pas

# Exemples
fpc -Tlinux -Px86_64 program.pas    # Linux 64-bit
fpc -Twin64 -Px86_64 program.pas    # Windows 64-bit
fpc -Tdarwin -Px86_64 program.pas   # macOS Intel
fpc -Tdarwin -Paarch64 program.pas  # macOS ARM (M1/M2)
```

#### Configuration dans fpc.cfg

```ini
# === Configuration Cross-Compilation ===

# Définir les chemins des cross-compilateurs
#IFDEF CROSSWIN64
  -Twin64
  -Px86_64
  -XPx86_64-win64-
  -Fu$FPCDIR/units/x86_64-win64
  -Fl$FPCDIR/lib/x86_64-win64
#ENDIF

#IFDEF CROSSLINUX64
  -Tlinux
  -Px86_64
  -XPx86_64-linux-
  -Fu$FPCDIR/units/x86_64-linux
  -Fl/usr/x86_64-linux-gnu/lib
#ENDIF

#IFDEF CROSSARM
  -Tlinux
  -Parm
  -XParm-linux-gnueabihf-
  -Fu$FPCDIR/units/arm-linux
  -CpARMV7A
  -CfVFPV3
#ENDIF
```

### Scripts de cross-compilation

Créez `cross-compile.sh` :

```bash
#!/bin/bash

# Script de cross-compilation multi-cibles

PROJECT="myproject.pas"
BASE_DIR=$(pwd)

# Fonction de compilation
compile_for() {
    local OS=$1
    local CPU=$2
    local OUTPUT_DIR="bin/$OS-$CPU"

    echo "Compilation pour $OS-$CPU..."
    mkdir -p "$OUTPUT_DIR"

    fpc -T$OS -P$CPU \
        -FE"$OUTPUT_DIR" \
        -FU"lib/$OS-$CPU" \
        -O3 -CX -XX -Xs \
        "$PROJECT"

    if [ $? -eq 0 ]; then
        echo "✓ Succès : $OS-$CPU"
    else
        echo "✗ Échec : $OS-$CPU"
    fi
}

# Compiler pour différentes cibles
compile_for linux x86_64
compile_for linux i386
compile_for win64 x86_64
compile_for win32 i386
compile_for darwin x86_64
compile_for darwin aarch64

echo "Compilation terminée!"
```

## Optimisations avancées

### Optimisations CPU spécifiques

```ini
# Intel/AMD modernes
-CpCOREAVX2      # AVX2 instructions
-CpCOREI         # Core i3/i5/i7
-CpATHLON64      # AMD Athlon 64

# ARM
-CpARMV7A        # ARM v7 avec NEON
-CpARMV8         # ARM 64-bit

# Options d'optimisation CPU
-Cf64            # FPU 64-bit
-CfSSE3          # SSE3 pour calculs flottants
-CfAVX           # AVX instructions

# Alignement mémoire
-Oa              # Alignement optimal
-Oa=16           # Alignement 16 octets (SSE)
```

### Profil d'optimisation par usage

#### Application Desktop

```ini
# desktop.cfg
-O2              # Optimisation équilibrée
-CX -XX          # Smart linking
-Xs              # Strip symbols
-WG              # Windows GUI
```

#### Serveur/Service

```ini
# server.cfg
-O3              # Performance maximale
-CpCOREAVX2      # CPU moderne
-Ur              # Réentrant (thread-safe)
-St              # Stack statique
```

#### Bibliothèque

```ini
# library.cfg
-O2              # Optimisation standard
-Cg              # Code PIC (Linux)
-CD              # Créer bibliothèque dynamique
-Ur              # Thread-safe
```

#### Embarqué/IoT

```ini
# embedded.cfg
-Os              # Optimiser pour la taille
-CX -XX -Xs      # Taille minimale
-Xt              # Stack statique
-k--gc-sections  # Linker : supprimer code mort
```

## Messages et diagnostics

### Configuration des messages

```bash
# Niveaux de messages
fpc -v0 program.pas    # Silencieux
fpc -ve program.pas    # Erreurs seulement
fpc -vew program.pas   # Erreurs + Warnings
fpc -vewn program.pas  # + Notes
fpc -vewnh program.pas # + Hints
fpc -vewnhi program.pas # + Info (verbose)
fpc -va program.pas    # Tout (debug compilateur)

# Messages spécifiques
fpc -vm5024 program.pas  # Afficher message 5024
fpc -vq5024 program.pas  # Masquer message 5024
```

### Directives de messages dans le code

```pascal
// Contrôle des messages
{$WARNINGS OFF}
// Code sans warnings
{$WARNINGS ON}

{$WARN 5024 OFF}  // Désactiver warning spécifique
{$WARN SYMBOL_DEPRECATED OFF}

// Messages personnalisés
{$MESSAGE HINT 'Optimisation nécessaire ici'}
{$MESSAGE WARN 'Code temporaire - à réviser'}
{$MESSAGE ERROR 'Ne pas compiler en production'}
{$MESSAGE FATAL 'Configuration invalide'}

// Conditional messages
{$IFDEF DEBUG}
  {$MESSAGE 'Compilation DEBUG'}
{$ELSE}
  {$MESSAGE 'Compilation RELEASE'}
{$ENDIF}
```

## Outils de configuration

### fpcmkcfg : Générateur de configuration

```bash
# Générer une nouvelle configuration
fpcmkcfg -d basepath=/usr/lib/fpc/3.2.2 -o fpc.cfg

# Avec des variables
fpcmkcfg -d basepath=$FPCDIR \
         -d sharepath=$FPCDIR/share \
         -o ~/.fpc.cfg

# Template personnalisé
fpcmkcfg -t mytemplate.cfg -o fpc.cfg
```

### fpcsubst : Variables de substitution

```bash
# Voir les substitutions actuelles
fpcsubst -l

# Ajouter une substitution
fpcsubst -a MYLIB=/home/user/mylib

# Dans fpc.cfg
-Fu$(MYLIB)/units
```

### fp IDE : Configuration graphique

L'IDE texte `fp` offre une interface pour configurer :
1. Options → Directories : Chemins
2. Options → Compiler : Options de compilation
3. Options → Memory : Tailles mémoire
4. Options → Linker : Options de liaison

## Scripts et automatisation

### Makefile pour FPC

Créez un `Makefile` :

```makefile
# Variables
FPC = fpc
FPCFLAGS = -O3 -CX -XX -Xs
DEBUGFLAGS = -g -gl -gh -Cr -Ct -Ci
TARGET = myapp
SOURCES = $(wildcard src/*.pas)
UNITS = $(wildcard src/units/*.pas)

# Plateformes
PLATFORMS = linux-x86_64 win64-x86_64 darwin-x86_64

# Règles
.PHONY: all clean debug release cross-compile

all: release

debug:
	@echo "Building DEBUG version..."
	@mkdir -p bin/debug lib/debug
	$(FPC) $(DEBUGFLAGS) -dDEBUG \
	       -FEbin/debug -FUlib/debug \
	       src/$(TARGET).pas

release:
	@echo "Building RELEASE version..."
	@mkdir -p bin/release lib/release
	$(FPC) $(FPCFLAGS) -dRELEASE \
	       -FEbin/release -FUlib/release \
	       src/$(TARGET).pas

cross-compile:
	@for platform in $(PLATFORMS); do \
	    echo "Building for $$platform..."; \
	    OS=$${platform%-*}; \
	    ARCH=$${platform#*-}; \
	    mkdir -p bin/$$platform; \
	    $(FPC) -T$$OS -P$$ARCH $(FPCFLAGS) \
	           -FEbin/$$platform \
	           src/$(TARGET).pas; \
	done

clean:
	@echo "Cleaning..."
	@rm -rf bin lib
	@find . -name "*.ppu" -delete
	@find . -name "*.o" -delete
	@find . -name "*.compiled" -delete
```

### Script de build avec profils

Créez `build.sh` :

```bash
#!/bin/bash

# Profils de compilation
declare -A PROFILES
PROFILES[debug]="-g -gl -gh -Cr -Ct -Ci -Sa -O- -dDEBUG"
PROFILES[release]="-O3 -CX -XX -Xs -dRELEASE"
PROFILES[profile]="-g -gl -pg -O2 -dPROFILE"
PROFILES[small]="-Os -CX -XX -Xs -dSMALL"

# Fonction de compilation
build() {
    local PROFILE=$1
    local FLAGS=${PROFILES[$PROFILE]}

    if [ -z "$FLAGS" ]; then
        echo "Profil inconnu : $PROFILE"
        echo "Profils disponibles : ${!PROFILES[@]}"
        exit 1
    fi

    echo "Compilation avec profil : $PROFILE"
    echo "Options : $FLAGS"

    mkdir -p "bin/$PROFILE" "lib/$PROFILE"

    fpc $FLAGS \
        -FE"bin/$PROFILE" \
        -FU"lib/$PROFILE" \
        src/main.pas

    if [ $? -eq 0 ]; then
        echo "✓ Compilation réussie"
        echo "Exécutable : bin/$PROFILE/main"
    else
        echo "✗ Compilation échouée"
        exit 1
    fi
}

# Traitement des arguments
if [ $# -eq 0 ]; then
    echo "Usage: $0 <profile>"
    echo "Profils : ${!PROFILES[@]}"
    exit 1
fi

build $1
```

## Diagnostic et résolution de problèmes

### Problèmes courants de configuration

#### "Can't find unit"

```bash
# Vérifier les chemins
fpc -vut program.pas  # Affiche la recherche d'unités

# Solution : Ajouter les chemins
fpc -Fu/path/to/units program.pas
```

#### "Error while linking"

```bash
# Vérifier les bibliothèques
fpc -vd program.pas   # Mode debug du linker

# Solutions possibles
fpc -Fl/usr/local/lib program.pas  # Ajouter chemin lib
fpc -k-L/usr/local/lib program.pas # Option linker
```

#### Performance de compilation lente

```ini
# Optimisations dans fpc.cfg
-O2              # Pas O3/O4 pour la compilation
-Ur              # Unités en RAM
-FU/tmp/fpc      # Unités dans tmpfs/ramdisk
```

### Outils de diagnostic

```bash
# Voir la configuration active
fpc -i            # Informations compilateur
fpc -ic           # CPU supportés
fpc -it           # OS supportés
fpc -io           # Optimisations disponibles

# Tracer la compilation
fpc -va program.pas > compile.log 2>&1

# Analyser les dépendances
fpc -vut program.pas | grep "Searching"

# Temps de compilation
time fpc -O2 program.pas
```

## Meilleures pratiques

### Organisation de configurations multiples

```
configs/
├── development.cfg    # Debug, tous checks
├── testing.cfg       # Tests, profiling
├── staging.cfg       # Pré-production
├── production.cfg    # Release, optimisé
└── cross-compile/
    ├── linux.cfg
    ├── windows.cfg
    └── macos.cfg
```

Utilisation :
```bash
# Développement
fpc @configs/development.cfg program.pas

# Production
fpc @configs/production.cfg program.pas
```

### Configuration d'équipe

```ini
# team-base.cfg - Configuration partagée
-Fu$(TEAM_LIBS)/units
-Fi$(TEAM_LIBS)/includes
-Mobjfpc
-Sh
-Sc

# developer.cfg - Configuration personnelle
# Include la config d'équipe
#INCLUDE team-base.cfg

# Personnalisations locales
-Fu~/my-units
-dDEVELOPER_MODE
```

### Documentation de configuration

Créez un `CONFIG.md` :

````markdown
# Configuration FPC du Projet

## Profils disponibles
- **debug** : Développement avec tous les checks
- **release** : Production optimisée
- **test** : Tests unitaires
- **profile** : Analyse de performance

## Variables d'environnement
- `FPC_CONFIG` : Fichier de config à utiliser
- `FPC_PROFILE` : Profil de compilation
- `FPC_TARGET` : Cible de compilation

## Usage
```bash
FPC_PROFILE=release make build
```
````

## Conclusion

### Points clés à retenir

1. **fpc.cfg est central** : C'est le cœur de la configuration
2. **Hiérarchie de configuration** : Global → Utilisateur → Projet
3. **Modes adaptés** : Choisir le bon mode selon le contexte (OBJFPC recommandé)
4. **Optimisations ciblées** : Adapter selon l'usage (desktop, serveur, embarqué)
5. **Cross-compilation** : Une configuration, plusieurs plateformes
6. **Automatisation** : Scripts et makefiles pour reproductibilité

### Configuration de référence rapide

#### Pour débuter (simple et sûr)

```ini
# minimal.cfg
-Mobjfpc         # Mode Object Pascal
-O2              # Optimisation standard
-Cr -Ct -Ci      # Vérifications actives
-g -gl           # Débogage
-Fu./units       # Unités locales
```

#### Pour production (optimisé)

```ini
# production.cfg
-Mobjfpc         # Mode Object Pascal
-O3              # Optimisation maximale
-CX -XX -Xs      # Taille minimale
-dRELEASE        # Define RELEASE
-Fu./units       # Unités
-FE./bin         # Sortie
```

#### Pour développement (debug complet)

```ini
# development.cfg
-Mobjfpc         # Mode Object Pascal
-O-              # Pas d'optimisation
-g -gl -gh       # Debug + heap trace
-Cr -Ct -Ci -Co  # Tous les checks
-Sa              # Assertions
-dDEBUG          # Define DEBUG
-vewnhi          # Messages verbeux
```

### Commandes FPC essentielles

```bash
# Informations système
fpc -i           # Info compilateur
fpc -ic          # CPUs supportés
fpc -it          # Targets supportées

# Compilation basique
fpc program.pas                    # Simple
fpc -O2 -CX -XX program.pas       # Optimisé
fpc -g -gl -Cr program.pas        # Debug

# Cross-compilation
fpc -Tlinux -Px86_64 program.pas  # Linux 64-bit
fpc -Twin64 program.pas           # Windows 64-bit

# Avec configuration
fpc @myconfig.cfg program.pas     # Fichier config
fpc -dDEBUG program.pas           # Define symbol
```

### Architecture de projet recommandée

```
project/
├── src/                 # Sources
│   ├── project.pas    # Programme principal
│   └── units/          # Unités
├── config/             # Configurations
│   ├── fpc.cfg        # Config principale
│   ├── debug.cfg      # Profil debug
│   └── release.cfg    # Profil release
├── bin/                # Exécutables
│   ├── debug/
│   └── release/
├── lib/                # Unités compilées
│   ├── debug/
│   └── release/
├── scripts/            # Automatisation
│   ├── build.sh
│   └── clean.sh
└── Makefile           # Build automation
```

### Workflow de développement type

1. **Développement initial**
   ```bash
   fpc @config/debug.cfg src/project.pas
   ```

2. **Tests et validation**
   ```bash
   fpc @config/test.cfg -FUtests/lib src/project.pas
   ```

3. **Optimisation**
   ```bash
   fpc @config/profile.cfg src/project.pas
   gprof project gmon.out > analysis.txt
   ```

4. **Production**
   ```bash
   fpc @config/release.cfg src/project.pas
   strip bin/release/project
   ```

5. **Cross-compilation**
   ```bash
   make cross-compile
   ```

### Checklist de configuration

Avant de finaliser votre configuration :

- [ ] **Mode de compilation** approprié choisi (OBJFPC/DELPHI)
- [ ] **Chemins** correctement configurés (-Fu, -Fi, -Fl)
- [ ] **Optimisations** adaptées à l'usage (-O2/-O3/-Os)
- [ ] **Vérifications** activées en debug (-Cr -Ct -Ci)
- [ ] **Symboles de debug** en développement (-g -gl)
- [ ] **Cross-compilation** testée si nécessaire
- [ ] **Scripts de build** automatisés créés
- [ ] **Documentation** de la configuration écrite
- [ ] **Profils** multiples définis (debug/release/test)
- [ ] **Variables d'environnement** documentées

### Évolution et maintenance

#### Versioning de la configuration

```bash
# Versionner les configs avec Git
git add config/*.cfg
git commit -m "Configuration FPC v1.0"
git tag -a config-v1.0 -m "Configuration stable"
```

#### Mise à jour lors des changements FPC

```bash
# Tester avec nouvelle version
fpc-3.2.2 @config/test.cfg src/project.pas
fpc-3.4.0 @config/test.cfg src/project.pas

# Adapter si nécessaire
#IF FPC_VERSION >= 3.4
  -O4  # Nouvelle optimisation
#ENDIF
```

#### Partage en équipe

```ini
# .gitignore
lib/
bin/
*.ppu
*.o
*.exe

# À versionner
config/*.cfg
scripts/*.sh
Makefile
```

### Ressources pour approfondir

**Documentation officielle**
- [FPC User Manual](https://www.freepascal.org/docs.html)
- [Compiler Options Reference](https://www.freepascal.org/docs-html/user/userch5.html)
- [FPC Wiki Configuration](https://wiki.freepascal.org/fpc.cfg)

**Outils complémentaires**
- `fpcmkcfg` : Générateur de configuration
- `fpcsubst` : Gestionnaire de variables
- `fp` IDE : Configuration graphique
- `lazbuild` : Build tool Lazarus

**Communauté**
- Forum FreePascal : Section Compiler
- Mailing list FPC-devel
- Stack Overflow tag [freepascal]

### Le mot de la fin sur la configuration

La configuration de FPC peut sembler complexe au premier abord, mais elle suit une logique simple :

1. **Commencez simple** : Configuration minimale qui fonctionne
2. **Évoluez progressivement** : Ajoutez des options selon les besoins
3. **Documentez** : Notez pourquoi chaque option est là
4. **Automatisez** : Scripts pour ne pas retaper les commandes
5. **Partagez** : Configurations d'équipe versionnées

Une bonne configuration FPC est celle qui :
- **Compile rapidement** en développement
- **Optimise bien** en production
- **Détecte les erreurs** tôt
- **S'adapte** aux différentes plateformes
- **Se maintient** facilement

Avec ces bases solides, vous êtes prêt à exploiter pleinement la puissance de FPC dans vos projets. La configuration n'est pas une contrainte mais un outil puissant pour adapter le compilateur à vos besoins exacts.

---

**Rappel important** : La meilleure configuration est celle que vous comprenez. N'ajoutez pas d'options "au cas où" - chaque paramètre doit avoir une raison d'être dans votre workflow.

⏭️ [Modes de compatibilité (Turbo Pascal, Delphi, ObjFPC)](/01-introduction-freepascal-lazarus/06-modes-compatibilite.md)
