🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 2.8 Cross-compilation Windows↔Linux

## Introduction : Compiler pour une autre plateforme

La cross-compilation est une fonctionnalité magique : vous êtes sur Windows et vous créez un exécutable Linux, ou vice-versa. C'est comme avoir une usine qui produit à la fois des voitures pour conduite à gauche et à droite, sans changer d'usine !

**Qu'est-ce que la cross-compilation ?**

La cross-compilation consiste à compiler un programme sur une plateforme (l'hôte) pour qu'il s'exécute sur une autre plateforme (la cible). Par exemple :
- 💻 **Hôte** : Windows 10 64-bit (votre PC)
- 🎯 **Cible** : Ubuntu Linux 64-bit (le serveur de production)
- 📦 **Résultat** : Un exécutable Linux créé sur Windows

**Pourquoi cross-compiler ?**
- 🚀 **Productivité** : Un seul environnement de développement
- 💰 **Économie** : Pas besoin de plusieurs machines
- ⏱️ **Rapidité** : Compilation sur votre machine puissante
- 🔄 **CI/CD** : Builds automatisés multi-plateformes
- 🎯 **Déploiement** : Créer des binaires pour tous depuis un seul endroit

## Concepts fondamentaux

### Architecture de la cross-compilation

```
Processus de cross-compilation :
├── Code source (.pas)
│   └── Identique pour toutes les plateformes
├── Compilateur (FPC)
│   ├── Version native (compile pour l'hôte)
│   └── Version cross (compile pour la cible)
├── Bibliothèques
│   ├── RTL (Runtime Library) de la cible
│   ├── Packages de la cible
│   └── Binutils de la cible
└── Exécutable final
    └── Format spécifique à la cible
```

### Ce dont vous avez besoin

Pour cross-compiler, il vous faut :

1. **FPC cross-compiler** : Le compilateur capable de générer du code pour la cible
2. **Binutils** : Les outils pour assembler et lier (as, ld) pour la cible
3. **RTL de la cible** : La Runtime Library FreePascal pour la plateforme cible
4. **Bibliothèques système** : Les librairies de base de l'OS cible (libc, etc.)

### Terminologie importante

```
Termes à connaître :
├── Host (Hôte)
│   └── La machine où vous compilez
├── Target (Cible)
│   └── La machine où le programme s'exécutera
├── CPU
│   ├── i386 (32-bit x86)
│   ├── x86_64 (64-bit x86)
│   └── arm, aarch64, etc.
├── OS
│   ├── win32/win64 (Windows)
│   ├── linux (Linux)
│   └── darwin (macOS)
└── Widget Set
    ├── win32/win64 (Windows natif)
    ├── gtk2/gtk3 (Linux GTK)
    └── qt5 (Linux Qt)
```

## Windows → Linux

### Installation du cross-compiler

#### Méthode 1 : FpcUpDeluxe (Recommandé pour débutants)

FpcUpDeluxe est un outil qui automatise l'installation :

1. **Télécharger FpcUpDeluxe**
   - Site : https://github.com/LongDirtyAnimAlf/fpcupdeluxe/releases
   - Choisir : `fpcupdeluxe-x86_64-win64.exe`

2. **Configurer pour Linux**
   ```
   Dans FpcUpDeluxe :
   ├── Installation FPC/Lazarus de base
   ├── Cross → CPU : x86_64
   ├── Cross → OS : linux
   └── Install crosscompiler
   ```

3. **Vérification**
   ```cmd
   fpc -Tlinux -Px86_64 -iTO
   # Doit afficher : linux
   ```

#### Méthode 2 : Installation manuelle

1. **Télécharger les binutils Linux pour Windows**
   ```
   Fichiers nécessaires :
   ├── x86_64-linux-as.exe    # Assembleur
   ├── x86_64-linux-ld.exe    # Linker
   ├── x86_64-linux-strip.exe # Strip symbols
   └── x86_64-linux-objdump.exe
   ```

2. **Installer dans FPC**
   ```
   C:\FPC\3.2.2\bin\x86_64-win64\
   └── Copier les binutils ici
   ```

3. **Compiler les unités RTL**
   ```cmd
   cd C:\FPC\3.2.2\source
   make clean all OS_TARGET=linux CPU_TARGET=x86_64
   make crossinstall OS_TARGET=linux CPU_TARGET=x86_64 INSTALL_PREFIX=C:\FPC\3.2.2
   ```

### Configuration dans Lazarus

#### Configurer le projet

**Projet → Options du projet → Config and Target**

```
┌─ Configuration Cross-Compilation ────────────────┐
│                                                  │
│ Target OS : [Linux               ▼]              │
│             ├── Default (Win64)                  │
│             ├── Linux                            │
│             ├── Win32                            │
│             └── Darwin                           │
│                                                  │
│ Target CPU : [x86_64             ▼]              │
│              ├── Default                         │
│              ├── i386                            │
│              └── x86_64                          │
│                                                  │
│ Target Processor : [Default      ▼]              │
│                                                  │
│ Widget Set : [gtk2               ▼]              │
│              ├── Default (win32)                 │
│              ├── gtk2                            │
│              ├── gtk3                            │
│              └── qt5                             │
│                                                  │
│ [OK] [Test] [Annuler]                            │
└──────────────────────────────────────────────────┘
```

#### Créer des modes de compilation

**Projet → Options du projet → Modes de compilation**

```
Modes recommandés :
├── Debug-Windows
│   ├── OS : win64
│   ├── CPU : x86_64
│   └── Widget : win32
├── Debug-Linux
│   ├── OS : linux
│   ├── CPU : x86_64
│   └── Widget : gtk2
├── Release-Windows
│   ├── Optimisations : -O3
│   └── Strip symbols : -Xs
└── Release-Linux
    ├── Optimisations : -O3
    └── Strip symbols : -Xs
```

### Gérer les bibliothèques Linux

#### Problème des dépendances

Linux nécessite des bibliothèques système (.so) que Windows n'a pas :

```
Dépendances typiques Linux :
├── libc.so.6        # Bibliothèque C standard
├── libdl.so.2       # Dynamic linking
├── libpthread.so.0  # Threads POSIX
├── libX11.so.6      # Système X Window
├── libgtk-x11-2.0.so.0  # GTK2
└── libgdk-x11-2.0.so.0  # GDK
```

#### Solution 1 : Copier depuis Linux

```bash
# Sur une machine Linux
mkdir linux_libs
cp /lib/x86_64-linux-gnu/libc.so.6 linux_libs/
cp /lib/x86_64-linux-gnu/libdl.so.2 linux_libs/
# ... autres bibliothèques

# Transférer vers Windows
scp -r linux_libs/ user@windows:/path/to/project/
```

#### Solution 2 : Utiliser une VM/Docker

```dockerfile
# Dockerfile pour extraire les libs
FROM ubuntu:20.04
RUN apt-get update && apt-get install -y \
    libc6-dev \
    libgtk2.0-dev \
    libx11-dev
CMD ["/bin/bash"]
```

### Code conditionnel multi-plateforme

#### Directives de compilation

```pascal
program CrossPlatformApp;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,  // Threads POSIX pour Linux
  unix,      // fpSetEnv et autres appels POSIX
  {$ENDIF}
  {$IFDEF WINDOWS}
  Windows,   // API Windows
  {$ENDIF}
  Classes, SysUtils, Forms;

begin
  {$IFDEF WINDOWS}
  SetConsoleOutputCP(CP_UTF8);  // Windows seulement
  {$ENDIF}

  {$IFDEF LINUX}
  // Configuration spécifique Linux
  // Note : SetEnvironmentVariable n'existe pas en FPC,
  // utiliser fpSetEnv de l'unité unix
  fpSetEnv('GTK_THEME', 'Adwaita', 1);
  {$ENDIF}

  Application.Initialize;
  Application.Run;
end.
```

#### Gestion des chemins

```pascal
function GetConfigPath: string;
begin
  {$IFDEF WINDOWS}
  Result := GetEnvironmentVariable('APPDATA') + '\MonApp\';
  {$ENDIF}

  {$IFDEF UNIX}
  Result := GetEnvironmentVariable('HOME') + '/.config/monapp/';
  {$ENDIF}

  // Créer le répertoire s'il n'existe pas
  if not DirectoryExists(Result) then
    CreateDir(Result);
end;

function GetExecutablePath: string;
begin
  Result := ExtractFilePath(ParamStr(0));
  {$IFDEF WINDOWS}
  // Windows : C:\Program Files\MonApp\
  {$ENDIF}
  {$IFDEF UNIX}
  // Linux : /usr/local/bin/
  {$ENDIF}
end;
```

#### Séparateurs de chemins

```pascal
uses
  LazFileUtils;  // Utilitaires multi-plateformes

procedure WorkWithPaths;
var
  FullPath: string;
begin
  // Utiliser PathDelim au lieu de '\' ou '/'
  FullPath := ExtractFilePath(ParamStr(0)) + PathDelim + 'data' + PathDelim + 'config.ini';

  // Ou utiliser ConcatPaths
  FullPath := ConcatPaths([ExtractFilePath(ParamStr(0)), 'data', 'config.ini']);

  // Convertir les chemins
  {$IFDEF WINDOWS}
  FullPath := StringReplace(FullPath, '/', '\', [rfReplaceAll]);
  {$ENDIF}
  {$IFDEF UNIX}
  FullPath := StringReplace(FullPath, '\', '/', [rfReplaceAll]);
  {$ENDIF}
end;
```

## Linux → Windows

### Installation sur Linux

#### Installer le cross-compiler Windows

```bash
# Ubuntu/Debian
sudo apt-get update
sudo apt-get install mingw-w64

# Vérifier l'installation
x86_64-w64-mingw32-gcc --version

# Installer les packages FPC pour cross-compilation
sudo apt-get install fp-units-win32 fp-units-win64
```

#### Compiler FPC pour Windows

```bash
# Télécharger les sources FPC
cd ~
svn co https://svn.freepascal.org/svn/fpc/trunk fpc-source

# Compiler le cross-compiler
cd fpc-source
make clean all OS_TARGET=win64 CPU_TARGET=x86_64

# Installer
sudo make crossinstall OS_TARGET=win64 CPU_TARGET=x86_64 INSTALL_PREFIX=/usr
```

### Configuration Lazarus Linux

#### Configurer les chemins

**Outils → Options → Environnement → Fichiers**

```
Chemins cross-compilation :
├── Compilateur Windows
│   └── /usr/bin/ppcrossx64
├── Binutils Windows
│   └── /usr/bin/x86_64-w64-mingw32-*
└── Unités Windows
    └── /usr/lib/fpc/3.2.2/units/x86_64-win64/
```

#### Configuration du projet

```
Projet → Options → Config and Target
├── Target OS : win64
├── Target CPU : x86_64
├── Widget Set : win32
└── Options supplémentaires : -Twin64 -Px86_64
```

### Ressources Windows

#### Icônes et manifestes

```pascal
// Fichier projet.lpr
{$IFDEF WINDOWS}
  {$R *.res}  // Inclure les ressources Windows
  {$R manifest.res}  // Manifeste pour UAC
{$ENDIF}
```

Créer `manifest.res` :
```xml
<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<assembly xmlns="urn:schemas-microsoft-com:asm.v1" manifestVersion="1.0">
  <assemblyIdentity
    version="1.0.0.0"
    processorArchitecture="*"
    name="MonApp"
    type="win32"/>
  <trustInfo xmlns="urn:schemas-microsoft-com:asm.v2">
    <security>
      <requestedPrivileges>
        <requestedExecutionLevel level="asInvoker" uiAccess="false"/>
      </requestedPrivileges>
    </security>
  </trustInfo>
</assembly>
```

## Problèmes courants et solutions

### Erreurs de compilation

#### "Can't find unit X used by Y"

**Problème** : Unité manquante pour la plateforme cible

**Solution** :
```
Projet → Options → Chemins
├── Autres fichiers unités (-Fu)
│   └── Ajouter : $(LazarusDir)/lcl/units/$(TargetCPU)-$(TargetOS)/$(LCLWidgetType)
└── Bibliothèques (-Fl)
    └── Ajouter : /usr/x86_64-linux-gnu/lib (pour Linux)
```

#### "Error while linking"

**Problème** : Bibliothèques manquantes

**Solutions** :

1. **Vérifier les dépendances**
   ```bash
   # Sur Linux, vérifier les dépendances
   ldd mon_executable

   # Sur Windows pour exe Linux (avec WSL)
   wsl ldd mon_executable
   ```

2. **Liaison statique**
   ```
   Options du projet → Compilation et édition de liens
   └── Options de liaison : -static
   ```

#### "Incompatible target"

**Problème** : Mauvaise configuration CPU/OS

**Solution** :
```pascal
// Vérifier dans le code
{$IF DEFINED(CPU64) and DEFINED(LINUX)}
  // Code 64-bit Linux
{$ELSEIF DEFINED(CPU32) and DEFINED(WINDOWS)}
  // Code 32-bit Windows
{$ELSE}
  {$ERROR Configuration non supportée}
{$ENDIF}
```

### Problèmes d'exécution

#### Executable Linux ne démarre pas

**Causes possibles** :
1. **Permissions** : `chmod +x mon_executable`
2. **Architecture** : Vérifier avec `file mon_executable`
3. **Dépendances** : `ldd mon_executable`

#### Executable Windows ne démarre pas

**Causes possibles** :
1. **DLL manquantes** : Utiliser Dependency Walker
2. **Architecture** : 32-bit vs 64-bit
3. **Antivirus** : Ajouter une exception

## Automatisation avec scripts

### Script Windows pour compiler Linux

`build_linux.bat` :
```batch
@echo off
echo === Cross-compilation Windows vers Linux ===

set PROJECT=MonProjet
set LAZARUS=C:\lazarus
set FPC=C:\lazarus\fpc\3.2.2\bin\x86_64-win64

REM Nettoyer
del /Q lib\x86_64-linux\*.*

REM Compiler pour Linux
%LAZARUS%\lazbuild.exe ^
  --os=linux ^
  --cpu=x86_64 ^
  --ws=gtk2 ^
  --build-mode=Release-Linux ^
  %PROJECT%.lpi

if %ERRORLEVEL% == 0 (
    echo.
    echo === Compilation réussie ===
    echo Exécutable : bin\linux\%PROJECT%

    REM Créer archive pour déploiement
    7z a -ttar %PROJECT%-linux.tar bin\linux\%PROJECT%
    7z a -tgzip %PROJECT%-linux.tar.gz %PROJECT%-linux.tar
    del %PROJECT%-linux.tar

    echo Archive créée : %PROJECT%-linux.tar.gz
) else (
    echo.
    echo === Erreur de compilation ===
)

pause
```

### Script Linux pour compiler Windows

`build_windows.sh` :
```bash
#!/bin/bash

echo "=== Cross-compilation Linux vers Windows ==="

PROJECT="MonProjet"
LAZARUS="/usr/lib/lazarus"
TARGET_OS="win64"
TARGET_CPU="x86_64"

# Nettoyer
rm -rf lib/x86_64-win64/*

# Compiler pour Windows
lazbuild \
  --os=$TARGET_OS \
  --cpu=$TARGET_CPU \
  --ws=win32 \
  --build-mode=Release-Windows \
  $PROJECT.lpi

if [ $? -eq 0 ]; then
    echo ""
    echo "=== Compilation réussie ==="
    echo "Exécutable : bin/windows/$PROJECT.exe"

    # Créer archive ZIP pour déploiement
    cd bin/windows
    zip -r ../../$PROJECT-windows.zip $PROJECT.exe *.dll
    cd ../..

    echo "Archive créée : $PROJECT-windows.zip"
else
    echo ""
    echo "=== Erreur de compilation ==="
fi
```

### Makefile multi-plateforme

`Makefile` :
```makefile
PROJECT = MonProjet
LAZBUILD = lazbuild

# Détection de l'OS
ifeq ($(OS),Windows_NT)
    HOST_OS = windows
    EXE = .exe
else
    HOST_OS = linux
    EXE =
endif

all: native

native:
	$(LAZBUILD) --build-mode=Release-$(HOST_OS) $(PROJECT).lpi

windows:
	$(LAZBUILD) --os=win64 --cpu=x86_64 --ws=win32 \
	  --build-mode=Release-Windows $(PROJECT).lpi

linux:
	$(LAZBUILD) --os=linux --cpu=x86_64 --ws=gtk2 \
	  --build-mode=Release-Linux $(PROJECT).lpi

cross: windows linux

clean:
	rm -rf lib/ bin/

install:
ifeq ($(HOST_OS),linux)
	cp bin/linux/$(PROJECT) /usr/local/bin/
else
	copy bin\windows\$(PROJECT).exe C:\Program Files\$(PROJECT)\
endif

.PHONY: all native windows linux cross clean install
```

## Tests multi-plateformes

### Tests unitaires cross-platform

```pascal
unit TestCrossPlatform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry;

type
  TTestCrossPlatform = class(TTestCase)
  published
    procedure TestPathSeparator;
    procedure TestFileOperations;
    procedure TestSystemCalls;
  end;

implementation

procedure TTestCrossPlatform.TestPathSeparator;
var
  Path: string;
begin
  Path := 'folder' + PathDelim + 'file.txt';

  {$IFDEF WINDOWS}
  AssertEquals('folder\file.txt', Path);
  {$ENDIF}

  {$IFDEF UNIX}
  AssertEquals('folder/file.txt', Path);
  {$ENDIF}
end;

procedure TTestCrossPlatform.TestFileOperations;
var
  TestFile: string;
begin
  {$IFDEF WINDOWS}
  TestFile := GetEnvironmentVariable('TEMP') + '\test.txt';
  {$ENDIF}

  {$IFDEF UNIX}
  TestFile := '/tmp/test.txt';
  {$ENDIF}

  // Créer et vérifier
  with TStringList.Create do
  try
    Add('Test');
    SaveToFile(TestFile);
    AssertTrue(FileExists(TestFile));
  finally
    Free;
  end;

  // Nettoyer
  DeleteFile(TestFile);
end;

procedure TTestCrossPlatform.TestSystemCalls;
begin
  {$IFDEF WINDOWS}
  AssertTrue(Length(GetEnvironmentVariable('WINDIR')) > 0);
  {$ENDIF}

  {$IFDEF UNIX}
  AssertTrue(Length(GetEnvironmentVariable('HOME')) > 0);
  {$ENDIF}
end;

initialization
  RegisterTest(TTestCrossPlatform);
end.
```

### Environnement de test

#### Avec Docker

`Dockerfile` pour tester l'exe Linux :
```dockerfile
FROM ubuntu:20.04

RUN apt-get update && apt-get install -y \
    libgtk2.0-0 \
    libx11-6 \
    xvfb

COPY bin/linux/monapp /usr/local/bin/
RUN chmod +x /usr/local/bin/monapp

# Lancer avec display virtuel
CMD xvfb-run -a /usr/local/bin/monapp
```

#### Avec Wine (tester exe Windows sur Linux)

```bash
# Installer Wine
sudo apt-get install wine64

# Tester l'exe Windows
wine bin/windows/MonProjet.exe

# Déboguer si problèmes
WINEDEBUG=+all wine bin/windows/MonProjet.exe 2> wine_debug.log
```

## CI/CD Cross-compilation

### GitHub Actions

`.github/workflows/build.yml` :
```yaml
name: Build Multi-Platform

on: [push, pull_request]

jobs:
  build-windows:
    runs-on: windows-latest
    steps:
    - uses: actions/checkout@v2

    - name: Install Lazarus
      run: |
        choco install lazarus

    - name: Build for Windows
      run: |
        lazbuild --build-mode=Release-Windows MonProjet.lpi

    - name: Build for Linux
      run: |
        lazbuild --os=linux --cpu=x86_64 --build-mode=Release-Linux MonProjet.lpi

    - name: Upload artifacts
      uses: actions/upload-artifact@v2
      with:
        name: binaries-windows
        path: |
          bin/windows/*.exe
          bin/linux/*

  build-linux:
    runs-on: ubuntu-latest
    steps:
    - uses: actions/checkout@v2

    - name: Install Lazarus
      run: |
        sudo apt-get update
        sudo apt-get install -y lazarus

    - name: Build for Linux
      run: |
        lazbuild --build-mode=Release-Linux MonProjet.lpi

    - name: Install MinGW for Windows cross-compile
      run: |
        sudo apt-get install -y mingw-w64

    - name: Build for Windows
      run: |
        lazbuild --os=win64 --cpu=x86_64 --build-mode=Release-Windows MonProjet.lpi

    - name: Upload artifacts
      uses: actions/upload-artifact@v2
      with:
        name: binaries-linux
        path: |
          bin/linux/*
          bin/windows/*.exe
```

## Optimisations et bonnes pratiques

### Réduire la taille des exécutables

```
Options du projet → Compilation et édition de liens
├── Optimisations
│   ├── Niveau : 3 (-O3)
│   └── ☑ Optimiser pour la taille (-Os)
├── Débogage
│   └── ☐ Générer infos de débogage
├── Liaison
│   ├── ☑ Strip symbols (-Xs)
│   ├── ☑ Smart linking (-XX)
│   └── ☑ Link smart (-CX)
└── Résultat
    ├── Windows : ~2 MB au lieu de 15 MB
    └── Linux : ~1.5 MB au lieu de 12 MB
```

### Gestion des icônes multi-plateformes

```pascal
// Structure des ressources
project/
├── icons/
│   ├── windows/
│   │   ├── app.ico      # Icône Windows
│   │   └── app.res      # Ressource compilée
│   └── linux/
│       ├── app.png      # Icône Linux
│       └── app.desktop  # Fichier desktop
```

Fichier `.desktop` pour Linux :
```ini
[Desktop Entry]
Version=1.0
Type=Application
Name=Mon Application
Comment=Description de mon application
Exec=/usr/local/bin/monapp
Icon=monapp
Categories=Utility;Development;
```

### Vérification de la cross-compilation

```pascal
program VerifyCrossCompile;
{$mode objfpc}{$H+}

begin
  WriteLn('=== Informations de compilation ===');
  WriteLn('OS cible : ', {$I %FPCTARGETOS%});
  WriteLn('CPU cible : ', {$I %FPCTARGETCPU%});
  WriteLn('Version FPC : ', {$I %FPCVERSION%});
  WriteLn('Date compilation : ', {$I %DATE%} , ' ', {$I %TIME%});

  {$IFDEF WINDOWS}
  WriteLn('Compilé pour Windows');
  {$ENDIF}

  {$IFDEF LINUX}
  WriteLn('Compilé pour Linux');
  {$ENDIF}

  {$IFDEF CPU64}
  WriteLn('Architecture 64-bit');
  {$ELSE}
  WriteLn('Architecture 32-bit');
  {$ENDIF}

  ReadLn;
end.
```

## Checklist de cross-compilation

Avant de cross-compiler, vérifiez :

```
Windows → Linux :
☐ Cross-compiler FPC installé
☐ Binutils Linux (as, ld) disponibles
☐ RTL Linux compilée
☐ Mode de build Linux configuré
☐ Widget set (gtk2/gtk3/qt5) choisi
☐ Code avec directives {$IFDEF}
☐ Chemins avec PathDelim
☐ Tests sur Linux (VM/Docker/WSL)

Linux → Windows :
☐ MinGW installé
☐ Cross-compiler FPC Windows
☐ Mode de build Windows configuré
☐ Ressources Windows (.res, .ico)
☐ Manifeste pour UAC
☐ DLL nécessaires identifiées
☐ Tests avec Wine
```

## Conclusion

La cross-compilation avec Lazarus/FPC est un outil puissant qui permet de créer des applications véritablement multi-plateformes depuis un seul environnement de développement. Bien que la configuration initiale demande un peu de travail, les bénéfices sont énormes :

**Avantages acquis :**
- 🚀 Développement plus rapide
- 💻 Un seul environnement à maîtriser
- 🔄 Builds automatisés multi-plateformes
- 📦 Distribution simplifiée
- 🧪 Tests cross-platform facilités

**Points clés à retenir :**
- Utilisez FpcUpDeluxe pour simplifier l'installation
- Créez des modes de build séparés par plateforme
- Utilisez les directives {$IFDEF} pour le code spécifique
- Testez toujours sur la plateforme cible
- Automatisez avec des scripts et CI/CD

La cross-compilation transforme Lazarus en véritable forge logicielle multi-plateforme, vous permettant de toucher un maximum d'utilisateurs avec un minimum d'effort !

⏭️ [Profils de projet et configurations multiples](/02-maitrise-ide-lazarus/09-profils-projet-configurations-multiples.md)
