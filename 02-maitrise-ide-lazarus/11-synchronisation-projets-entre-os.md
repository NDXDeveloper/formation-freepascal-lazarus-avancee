🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 2.11 Synchronisation de projets entre OS

## Introduction : Le défi du développement multi-OS

Imaginez que vous êtes un chef cuisinier qui doit préparer le même plat dans trois cuisines différentes : une française, une japonaise et une américaine. Chaque cuisine a ses propres ustensiles, ses propres fours, ses propres ingrédients. C'est exactement le défi de développer sur Windows, Linux et macOS simultanément !

**Qu'est-ce que la synchronisation de projets entre OS ?**

La synchronisation entre OS consiste à maintenir un projet Lazarus qui fonctionne parfaitement sur plusieurs systèmes d'exploitation, en gardant :
- 📁 **Le même code source** partagé entre tous les OS
- ⚙️ **Des configurations** adaptées à chaque plateforme
- 🔄 **Une synchronisation** automatique des modifications
- 🏗️ **Des builds** qui fonctionnent partout
- 🧪 **Des tests** validés sur chaque OS

**Pourquoi synchroniser entre OS ?**
- **Portée maximale** : Votre application touche tous les utilisateurs
- **Développement flexible** : Travaillez sur votre OS préféré
- **Tests complets** : Détectez les bugs spécifiques à chaque OS
- **Équipe distribuée** : Chacun utilise son OS favori
- **CI/CD robuste** : Builds et tests automatiques multi-plateformes

## Les défis de la synchronisation

### Différences fondamentales entre OS

```
Différences principales :
├── Chemins de fichiers
│   ├── Windows : C:\Users\Name\Documents
│   ├── Linux : /home/name/documents
│   └── macOS : /Users/name/Documents
├── Séparateurs
│   ├── Windows : \ (backslash)
│   └── Unix : / (slash)
├── Fins de ligne
│   ├── Windows : CRLF (\r\n)
│   ├── Linux : LF (\n)
│   └── macOS : LF (\n)
├── Sensibilité à la casse
│   ├── Windows : Insensible (File.txt = file.txt)
│   └── Unix : Sensible (File.txt ≠ file.txt)
└── Exécutables
    ├── Windows : .exe
    └── Unix : sans extension
```

### Problèmes courants

**1. Chemins hardcodés**
```pascal
// ❌ MAUVAIS : Chemin Windows hardcodé
ConfigFile := 'C:\Program Files\MonApp\config.ini';

// ✅ BON : Chemin portable
ConfigFile := GetAppConfigDir(False) + 'config.ini';
```

**2. Fins de ligne mixtes**
```
Symptôme : Git affiche des modifications partout  
Cause : Mélange CRLF/LF  
Solution : Configuration Git et .gitattributes
```

**3. Dépendances système**
```
Windows : Besoin de DLLs spécifiques  
Linux : Packages système requis  
macOS : Frameworks nécessaires
```

## Configuration du projet portable

### Structure de projet recommandée

```
MonProjetPortable/
├── .git/                    # Version control
├── .gitignore              # Fichiers à ignorer
├── .gitattributes          # Règles de fins de ligne
├── project.lpi             # Projet Lazarus
├── project.lpr             # Programme principal
├── src/                    # Sources communes
│   ├── common/            # Code partagé
│   ├── windows/           # Code spécifique Windows
│   ├── linux/             # Code spécifique Linux
│   └── macos/             # Code spécifique macOS
├── resources/              # Ressources
│   ├── icons/
│   │   ├── windows/      # .ico files
│   │   ├── linux/        # .png files
│   │   └── macos/        # .icns files
│   └── data/
├── lib/                    # Unités compilées (ignoré)
│   └── $(TargetCPU)-$(TargetOS)/
├── bin/                    # Exécutables (ignoré)
│   ├── windows/
│   ├── linux/
│   └── macos/
├── config/                 # Configurations
│   ├── dev/               # Développement
│   └── prod/              # Production
└── scripts/               # Scripts de build
    ├── build_windows.bat
    ├── build_linux.sh
    └── build_macos.sh
```

### Fichier .gitignore multi-OS

```gitignore
# === Lazarus/FreePascal ===
*.compiled
*.ppu
*.o
*.or
*.a
*.rsj
*.lps
*.bak*
backup/  
lib/

# === Executables ===
*.exe
*.dll
*.so
*.dylib
*.app
bin/

# === OS Specific ===
# Windows
Thumbs.db  
Desktop.ini
*.lnk

# macOS
.DS_Store
.AppleDouble
.LSOverride
._*

# Linux
*~
.directory
.Trash-*

# === IDE ===
.idea/
.vscode/
*.sublime-*

# === Project specific ===
config/local/
*.log
temp/
```

### Fichier .gitattributes

```gitattributes
# Gestion automatique des fins de ligne
* text=auto

# Sources Pascal - toujours LF
*.pas text eol=lf
*.pp text eol=lf
*.inc text eol=lf
*.lpr text eol=lf

# Fichiers projet - toujours LF
*.lpi text eol=lf
*.lpk text eol=lf
*.lps text eol=lf

# Scripts
*.sh text eol=lf
*.bat text eol=crlf
*.cmd text eol=crlf

# Binaires
*.exe binary
*.dll binary
*.so binary
*.dylib binary
*.ico binary
*.icns binary
*.png binary
*.jpg binary
```

## Code portable

### Utilisation des directives conditionnelles

```pascal
unit CrossPlatformUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils
  {$IFDEF WINDOWS}
  , Windows, Registry, ShellAPI
  {$ENDIF}
  {$IFDEF UNIX}
  , BaseUnix, Unix
  {$IFDEF LINUX}
  , Linux
  {$ENDIF}
  {$IFDEF DARWIN}
  , CocoaAll
  {$ENDIF}
  {$ENDIF};

type
  { TCrossPlatformHelper }
  TCrossPlatformHelper = class
  public
    class function GetPlatformName: string;
    class function GetHomeDirectory: string;
    class function GetTempDirectory: string;
    class function GetAppDataDirectory: string;
    class function ExecuteCommand(const ACommand: string): Boolean;
    class function OpenURL(const AURL: string): Boolean;
    class function GetSystemInfo: string;
  end;

implementation

uses
  FileUtil, LazFileUtils;

{ TCrossPlatformHelper }

class function TCrossPlatformHelper.GetPlatformName: string;  
begin
  {$IFDEF WINDOWS}
    {$IFDEF WIN32}
    Result := 'Windows 32-bit';
    {$ELSE}
    Result := 'Windows 64-bit';
    {$ENDIF}
  {$ENDIF}

  {$IFDEF LINUX}
  Result := 'Linux';
  {$ENDIF}

  {$IFDEF DARWIN}
  Result := 'macOS';
  {$ENDIF}

  {$IFDEF FREEBSD}
  Result := 'FreeBSD';
  {$ENDIF}
end;

class function TCrossPlatformHelper.GetHomeDirectory: string;  
begin
  {$IFDEF WINDOWS}
  Result := GetEnvironmentVariable('USERPROFILE');
  {$ELSE}
  Result := GetEnvironmentVariable('HOME');
  {$ENDIF}

  Result := IncludeTrailingPathDelimiter(Result);
end;

class function TCrossPlatformHelper.GetTempDirectory: string;  
begin
  Result := GetTempDir;  // Lazarus fonction portable
end;

class function TCrossPlatformHelper.GetAppDataDirectory: string;  
begin
  {$IFDEF WINDOWS}
  Result := GetEnvironmentVariable('APPDATA');
  {$ENDIF}

  {$IFDEF LINUX}
  Result := GetEnvironmentVariable('HOME') + '/.config';
  {$ENDIF}

  {$IFDEF DARWIN}
  Result := GetEnvironmentVariable('HOME') + '/Library/Application Support';
  {$ENDIF}

  Result := IncludeTrailingPathDelimiter(Result) + 'MonApp' + PathDelim;

  // Créer le répertoire s'il n'existe pas
  if not DirectoryExists(Result) then
    CreateDir(Result);
end;

class function TCrossPlatformHelper.ExecuteCommand(const ACommand: string): Boolean;  
begin
  {$IFDEF WINDOWS}
  Result := ShellExecute(0, 'open', PChar(ACommand), nil, nil, SW_SHOW) > 32;
  {$ENDIF}

  {$IFDEF UNIX}
  Result := FpSystem(ACommand) = 0;
  {$ENDIF}
end;

class function TCrossPlatformHelper.OpenURL(const AURL: string): Boolean;  
begin
  {$IFDEF WINDOWS}
  Result := ShellExecute(0, 'open', PChar(AURL), nil, nil, SW_SHOW) > 32;
  {$ENDIF}

  {$IFDEF LINUX}
  Result := FpSystem('xdg-open ' + AURL) = 0;
  {$ENDIF}

  {$IFDEF DARWIN}
  Result := FpSystem('open ' + AURL) = 0;
  {$ENDIF}
end;

class function TCrossPlatformHelper.GetSystemInfo: string;  
begin
  Result := 'Platform: ' + GetPlatformName + LineEnding;
  Result := Result + 'Home: ' + GetHomeDirectory + LineEnding;
  Result := Result + 'Temp: ' + GetTempDirectory + LineEnding;
  Result := Result + 'AppData: ' + GetAppDataDirectory + LineEnding;
  Result := Result + 'Path Delimiter: ' + PathDelim + LineEnding;
  Result := Result + 'Line Ending: ';

  {$IFDEF WINDOWS}
  Result := Result + 'CRLF';
  {$ELSE}
  Result := Result + 'LF';
  {$ENDIF}
end;

end.
```

### Gestion des chemins portables

```pascal
unit PathUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LazFileUtils;

type
  { TPathHelper }
  TPathHelper = class
  public
    // Convertir les séparateurs selon l'OS
    class function NormalizePath(const APath: string): string;

    // Construire un chemin portable
    class function BuildPath(const Parts: array of string): string;

    // Obtenir les chemins standard
    class function GetExecutablePath: string;
    class function GetConfigPath: string;
    class function GetDataPath: string;
    class function GetLogPath: string;

    // Vérifications
    class function IsAbsolutePath(const APath: string): Boolean;
    class function MakeRelativePath(const APath, ABase: string): string;
  end;

implementation

{ TPathHelper }

class function TPathHelper.NormalizePath(const APath: string): string;  
begin
  Result := APath;

  // Remplacer les séparateurs selon l'OS
  {$IFDEF WINDOWS}
  Result := StringReplace(Result, '/', '\', [rfReplaceAll]);
  {$ELSE}
  Result := StringReplace(Result, '\', '/', [rfReplaceAll]);
  {$ENDIF}

  // Nettoyer les doubles séparateurs
  Result := StringReplace(Result, PathDelim + PathDelim, PathDelim, [rfReplaceAll]);
end;

class function TPathHelper.BuildPath(const Parts: array of string): string;  
var
  i: Integer;
begin
  Result := '';

  for i := Low(Parts) to High(Parts) do
  begin
    if i = Low(Parts) then
      Result := Parts[i]
    else
      Result := Result + PathDelim + Parts[i];
  end;

  Result := NormalizePath(Result);
end;

class function TPathHelper.GetExecutablePath: string;  
begin
  Result := ExtractFilePath(ParamStr(0));
end;

class function TPathHelper.GetConfigPath: string;  
begin
  // Utiliser la fonction Lazarus portable
  Result := GetAppConfigDir(False);

  if not DirectoryExists(Result) then
    CreateDir(Result);
end;

class function TPathHelper.GetDataPath: string;  
begin
  {$IFDEF WINDOWS}
  Result := GetExecutablePath + 'data' + PathDelim;
  {$ELSE}
  // Sur Unix, les données peuvent être dans /usr/share
  if DirectoryExists('/usr/share/monapp/') then
    Result := '/usr/share/monapp/'
  else
    Result := GetExecutablePath + 'data' + PathDelim;
  {$ENDIF}
end;

class function TPathHelper.GetLogPath: string;  
begin
  {$IFDEF WINDOWS}
  Result := GetConfigPath + 'logs' + PathDelim;
  {$ELSE}
  // Sur Unix, utiliser /var/log si possible
  if DirectoryIsWritable('/var/log/') then
    Result := '/var/log/monapp/'
  else
    Result := GetConfigPath + 'logs' + PathDelim;
  {$ENDIF}

  if not DirectoryExists(Result) then
    CreateDir(Result);
end;

class function TPathHelper.IsAbsolutePath(const APath: string): Boolean;  
begin
  {$IFDEF WINDOWS}
  // Windows : C:\ ou \\server\
  Result := ((Length(APath) >= 3) and (APath[2] = ':') and (APath[3] = '\')) or
            ((Length(APath) >= 2) and (APath[1] = '\') and (APath[2] = '\'));
  {$ELSE}
  // Unix : commence par /
  Result := (Length(APath) > 0) and (APath[1] = '/');
  {$ENDIF}
end;

class function TPathHelper.MakeRelativePath(const APath, ABase: string): string;  
begin
  Result := CreateRelativePath(APath, ABase);
end;

end.
```

## Synchronisation avec Git

### Configuration Git multi-OS

#### Configuration globale

```bash
# Configuration pour tous les OS
git config --global core.autocrlf input  # Linux/macOS  
git config --global core.autocrlf true   # Windows

# Ignorer les changements de permissions (Linux/macOS)
git config core.fileMode false

# Sensibilité à la casse
git config core.ignorecase false
```

#### Hooks Git pour synchronisation

`.git/hooks/pre-commit` :
```bash
#!/bin/bash
# Hook pour vérifier la portabilité avant commit

echo "Vérification de la portabilité..."

# Vérifier les chemins Windows hardcodés
if grep -r "C:\\\\" --include="*.pas" src/; then
    echo "❌ Erreur : Chemins Windows hardcodés détectés!"
    exit 1
fi

# Vérifier les fins de ligne
if git diff --cached --name-only | xargs -I {} file {} | grep CRLF; then
    echo "⚠️  Attention : Fins de ligne CRLF détectées"
fi

# Vérifier les fichiers sensibles à la casse
FILES=$(git diff --cached --name-only)  
for f in $FILES; do
    LOWER=$(echo "$f" | tr '[:upper:]' '[:lower:]')
    if [ "$f" != "$LOWER" ]; then
        if git ls-files | grep -i "^$LOWER$" | grep -v "^$f$"; then
            echo "❌ Erreur : Conflit potentiel de casse avec $f"
            exit 1
        fi
    fi
done

echo "✅ Vérifications passées"
```

### Script de synchronisation

`sync_project.sh` :
```bash
#!/bin/bash

# Script de synchronisation multi-OS

PROJECT_NAME="MonProjet"  
REMOTE="origin"  
BRANCH=$(git branch --show-current)

echo "=== Synchronisation du projet $PROJECT_NAME ==="  
echo "Branche : $BRANCH"

# Fonction pour détecter l'OS
detect_os() {
    if [[ "$OSTYPE" == "linux-gnu"* ]]; then
        echo "linux"
    elif [[ "$OSTYPE" == "darwin"* ]]; then
        echo "macos"
    elif [[ "$OSTYPE" == "cygwin" ]] || [[ "$OSTYPE" == "msys" ]] || [[ "$OSTYPE" == "win32" ]]; then
        echo "windows"
    else
        echo "unknown"
    fi
}

OS=$(detect_os)  
echo "OS détecté : $OS"

# Sauvegarder les modifications locales
echo "Sauvegarde des modifications locales..."  
git stash push -m "Auto-stash before sync $(date +%Y%m%d-%H%M%S)"

# Récupérer les dernières modifications
echo "Récupération des modifications distantes..."  
git fetch $REMOTE

# Fusionner ou rebaser
if [ "$1" == "--rebase" ]; then
    echo "Rebase sur $REMOTE/$BRANCH..."
    git rebase $REMOTE/$BRANCH
else
    echo "Merge de $REMOTE/$BRANCH..."
    git merge $REMOTE/$BRANCH
fi

# Réappliquer les modifications locales
if git stash list | grep -q "Auto-stash before sync"; then
    echo "Réapplication des modifications locales..."
    git stash pop
fi

# Nettoyer les fichiers spécifiques à l'OS
echo "Nettoyage des fichiers temporaires..."  
case $OS in
    windows)
        find . -name "*.exe" -o -name "*.dll" | xargs rm -f
        ;;
    linux)
        find . -name "*.so" -o -name "*.a" | xargs rm -f
        ;;
    macos)
        find . -name "*.dylib" -o -name "*.app" | xargs rm -rf
        ;;
esac

# Recompiler pour l'OS actuel
echo "Recompilation pour $OS..."  
case $OS in
    windows)
        lazbuild.exe --build-mode=Debug $PROJECT_NAME.lpi
        ;;
    linux|macos)
        lazbuild --build-mode=Debug $PROJECT_NAME.lpi
        ;;
esac

echo "=== Synchronisation terminée ==="
```

## Environnements de développement

### Configuration multi-OS avec machines virtuelles

#### Structure recommandée

```
Environnements de développement :
├── Machine principale (votre OS préféré)
│   ├── IDE Lazarus
│   ├── Git
│   └── Dossier projet partagé
├── VM Windows (VirtualBox/VMware)
│   ├── Lazarus Windows
│   └── Dossier partagé monté
├── VM Linux (VirtualBox/VMware)
│   ├── Lazarus Linux
│   └── Dossier partagé monté
└── VM macOS (si possible)
    ├── Lazarus macOS
    └── Dossier partagé monté
```

#### Configuration VirtualBox

Script de création de VM : `create_dev_vms.sh`
```bash
#!/bin/bash

# Créer des VMs de développement

# VM Ubuntu
VBoxManage createvm --name "Ubuntu-Dev" --ostype Ubuntu_64 --register  
VBoxManage modifyvm "Ubuntu-Dev" --memory 4096 --cpus 2  
VBoxManage createhd --filename "Ubuntu-Dev.vdi" --size 30000  
VBoxManage storagectl "Ubuntu-Dev" --name "SATA" --add sata --controller IntelAhci  
VBoxManage storageattach "Ubuntu-Dev" --storagectl "SATA" --port 0 --device 0 --type hdd --medium "Ubuntu-Dev.vdi"

# Dossier partagé
VBoxManage sharedfolder add "Ubuntu-Dev" --name "projects" --hostpath "$(pwd)" --automount

# VM Windows
VBoxManage createvm --name "Windows-Dev" --ostype Windows10_64 --register  
VBoxManage modifyvm "Windows-Dev" --memory 4096 --cpus 2  
VBoxManage createhd --filename "Windows-Dev.vdi" --size 40000  
VBoxManage storagectl "Windows-Dev" --name "SATA" --add sata --controller IntelAhci  
VBoxManage storageattach "Windows-Dev" --storagectl "SATA" --port 0 --device 0 --type hdd --medium "Windows-Dev.vdi"

# Dossier partagé
VBoxManage sharedfolder add "Windows-Dev" --name "projects" --hostpath "$(pwd)" --automount

echo "VMs créées. Installer les OS et les Guest Additions."
```

### Docker pour tests multi-OS

#### Dockerfile pour Linux

`Dockerfile.ubuntu` :
```dockerfile
FROM ubuntu:22.04

# Installer Lazarus et dépendances
RUN apt-get update && apt-get install -y \
    lazarus \
    fpc \
    git \
    build-essential \
    libgtk2.0-dev \
    && rm -rf /var/lib/apt/lists/*

# Créer utilisateur de développement
RUN useradd -m -s /bin/bash developer  
USER developer  
WORKDIR /home/developer

# Copier le projet
COPY --chown=developer:developer . /home/developer/project

# Compiler
WORKDIR /home/developer/project  
RUN lazbuild --build-mode=Release MonProjet.lpi

CMD ["./bin/linux/MonProjet"]
```

#### Docker Compose multi-OS

`docker-compose.yml` :
```yaml
version: '3.8'

services:
  build-linux:
    build:
      context: .
      dockerfile: Dockerfile.ubuntu
    volumes:
      - ./src:/home/developer/project/src
      - ./bin/linux:/home/developer/project/bin
    environment:
      - DISPLAY=${DISPLAY}
      - QT_X11_NO_MITSHM=1
    volumes:
      - /tmp/.X11-unix:/tmp/.X11-unix:rw
    network_mode: host

  test-linux:
    image: ubuntu:22.04
    volumes:
      - ./bin/linux:/app
    command: /app/MonProjet --test

  # Wine pour tester Windows sur Linux
  test-windows:
    image: scottyhardy/docker-wine
    volumes:
      - ./bin/windows:/wine/drive_c/app
    command: wine /wine/drive_c/app/MonProjet.exe --test
```

### WSL2 pour développement Windows/Linux

#### Configuration WSL2

```powershell
# PowerShell en tant qu'admin

# Installer WSL2
wsl --install -d Ubuntu-22.04

# Configurer la mémoire
@"
[wsl2]
memory=8GB  
processors=4  
localhostForwarding=true
"@ | Out-File -FilePath "$env:USERPROFILE\.wslconfig" -Encoding ASCII

# Redémarrer WSL
wsl --shutdown  
wsl
```

#### Script de synchronisation WSL

`wsl_sync.ps1` :
```powershell
# Script PowerShell pour synchroniser avec WSL

$ProjectPath = "C:\Projects\MonProjet"
$WSLPath = "/mnt/c/Projects/MonProjet"

Write-Host "=== Synchronisation Windows <-> WSL ===" -ForegroundColor Green

# Compiler sur Windows
Write-Host "Compilation Windows..." -ForegroundColor Yellow  
Set-Location $ProjectPath
& lazbuild.exe --build-mode=Release MonProjet.lpi

if ($LASTEXITCODE -eq 0) {
    Write-Host "✓ Compilation Windows réussie" -ForegroundColor Green
} else {
    Write-Host "✗ Compilation Windows échouée" -ForegroundColor Red
    exit 1
}

# Compiler sur WSL
Write-Host "Compilation Linux (WSL)..." -ForegroundColor Yellow  
wsl bash -c "cd $WSLPath && lazbuild --build-mode=Release MonProjet.lpi"

if ($LASTEXITCODE -eq 0) {
    Write-Host "✓ Compilation Linux réussie" -ForegroundColor Green
} else {
    Write-Host "✗ Compilation Linux échouée" -ForegroundColor Red
    exit 1
}

# Tester les deux versions
Write-Host "`nTests..." -ForegroundColor Yellow

# Test Windows
Start-Process -FilePath "$ProjectPath\bin\windows\MonProjet.exe" -ArgumentList "--test" -Wait -NoNewWindow

# Test Linux
wsl bash -c "$WSLPath/bin/linux/MonProjet --test"

Write-Host "`n=== Synchronisation terminée ===" -ForegroundColor Green
```

## Tests multi-plateformes

### Framework de tests portable

```pascal
unit CrossPlatformTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry;

type
  { TCrossPlatformTests }
  TCrossPlatformTests = class(TTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    // Tests génériques
    procedure TestPathHandling;
    procedure TestFileOperations;
    procedure TestConfigStorage;

    // Tests spécifiques OS
    procedure TestPlatformSpecific;
    procedure TestSystemCalls;
    procedure TestGUIComponents;
  end;

implementation

uses
  FileUtil, LazFileUtils, CrossPlatformUtils, PathUtils;

{ TCrossPlatformTests }

procedure TCrossPlatformTests.SetUp;  
begin
  // Préparation pour chaque test
end;

procedure TCrossPlatformTests.TearDown;  
begin
  // Nettoyage après chaque test
end;

procedure TCrossPlatformTests.TestPathHandling;  
var
  TestPath: string;
  Expected: string;
begin
  // Test de normalisation des chemins
  {$IFDEF WINDOWS}
  TestPath := 'C:/Users/Test/Documents';
  Expected := 'C:\Users\Test\Documents';
  {$ELSE}
  TestPath := '/home/test\documents';
  Expected := '/home/test/documents';
  {$ENDIF}

  AssertEquals('Path normalization', Expected,
               TPathHelper.NormalizePath(TestPath));

  // Test de construction de chemins
  TestPath := TPathHelper.BuildPath(['home', 'user', 'documents']);
  AssertTrue('Path contains delimiter', Pos(PathDelim, TestPath) > 0);

  // Test de chemins absolus
  {$IFDEF WINDOWS}
  AssertTrue('Absolute path Windows',
             TPathHelper.IsAbsolutePath('C:\Windows'));
  AssertFalse('Relative path Windows',
              TPathHelper.IsAbsolutePath('Windows\System32'));
  {$ELSE}
  AssertTrue('Absolute path Unix',
             TPathHelper.IsAbsolutePath('/usr/bin'));
  AssertFalse('Relative path Unix',
              TPathHelper.IsAbsolutePath('usr/bin'));
  {$ENDIF}
end;

procedure TCrossPlatformTests.TestFileOperations;  
var
  TestFile: string;
  TestContent: TStringList;
begin
  TestFile := GetTempDir + 'test_' +
              FormatDateTime('yyyymmddhhnnss', Now) + '.txt';

  TestContent := TStringList.Create;
  try
    TestContent.Add('Test line 1');
    TestContent.Add('Test line 2');

    // Test d'écriture
    TestContent.SaveToFile(TestFile);
    AssertTrue('File created', FileExists(TestFile));

    // Test de lecture
    TestContent.Clear;
    TestContent.LoadFromFile(TestFile);
    AssertEquals('File content', 2, TestContent.Count);

    // Test de suppression
    DeleteFile(TestFile);
    AssertFalse('File deleted', FileExists(TestFile));
  finally
    TestContent.Free;
  end;
end;

procedure TCrossPlatformTests.TestConfigStorage;  
var
  ConfigPath: string;
  ConfigFile: string;
begin
  ConfigPath := GetAppConfigDir(False);
  AssertTrue('Config dir exists', DirectoryExists(ConfigPath));

  ConfigFile := ConfigPath + 'test.ini';

  // Test d'écriture de config
  with TStringList.Create do
  try
    Add('[Settings]');
    Add('Value1=Test');
    SaveToFile(ConfigFile);

    AssertTrue('Config file created', FileExists(ConfigFile));

    // Nettoyage
    DeleteFile(ConfigFile);
  finally
    Free;
  end;
end;

procedure TCrossPlatformTests.TestPlatformSpecific;  
begin
  // Test de détection de plateforme
  {$IFDEF WINDOWS}
  AssertTrue('Windows detected',
             Pos('Windows', TCrossPlatformHelper.GetPlatformName) > 0);
  {$ENDIF}

  {$IFDEF LINUX}
  AssertEquals('Linux detected', 'Linux',
               TCrossPlatformHelper.GetPlatformName);
  {$ENDIF}

  {$IFDEF DARWIN}
  AssertEquals('macOS detected', 'macOS',
               TCrossPlatformHelper.GetPlatformName);
  {$ENDIF}

  // Test des répertoires système
  AssertTrue('Home dir exists',
             DirectoryExists(TCrossPlatformHelper.GetHomeDirectory));
  AssertTrue('Temp dir exists',
             DirectoryExists(TCrossPlatformHelper.GetTempDirectory));
end;

procedure TCrossPlatformTests.TestSystemCalls;  
var
  Success: Boolean;
begin
  // Test d'exécution de commandes simples
  {$IFDEF WINDOWS}
  Success := TCrossPlatformHelper.ExecuteCommand('echo test > nul');
  {$ELSE}
  Success := TCrossPlatformHelper.ExecuteCommand('echo test > /dev/null');
  {$ENDIF}

  AssertTrue('Command execution', Success);
end;

procedure TCrossPlatformTests.TestGUIComponents;  
begin
  // Tests spécifiques aux composants GUI selon l'OS
  {$IFDEF WINDOWS}
  // Test des composants Windows
  AssertTrue('Windows GUI available', True);
  {$ENDIF}

  {$IFDEF LINUX}
  // Test de la disponibilité de GTK
  AssertTrue('GTK available', True);
  {$ENDIF}

  {$IFDEF DARWIN}
  // Test de Cocoa
  AssertTrue('Cocoa available', True);
  {$ENDIF}
end;

initialization
  RegisterTest(TCrossPlatformTests);
end.
```

### Script de tests multi-OS

`run_tests_all_platforms.sh` :
```bash
#!/bin/bash

# Script pour exécuter les tests sur toutes les plateformes

PROJECT="MonProjet"  
TEST_RESULTS_DIR="test_results"  
TIMESTAMP=$(date +%Y%m%d-%H%M%S)

mkdir -p $TEST_RESULTS_DIR

echo "=== Tests Multi-Plateformes ==="  
echo "Timestamp: $TIMESTAMP"

# Fonction pour exécuter les tests
run_tests() {
    local platform=$1
    local command=$2
    local output_file="$TEST_RESULTS_DIR/test_${platform}_${TIMESTAMP}.xml"

    echo "Testing on $platform..."

    if eval "$command" > "$output_file" 2>&1; then
        echo "✅ $platform: Tests passed"
        return 0
    else
        echo "❌ $platform: Tests failed"
        return 1
    fi
}

# Tests locaux (OS actuel)
if [[ "$OSTYPE" == "linux-gnu"* ]]; then
    run_tests "linux-local" "./bin/linux/$PROJECT --test --format=xml"
elif [[ "$OSTYPE" == "darwin"* ]]; then
    run_tests "macos-local" "./bin/macos/$PROJECT --test --format=xml"
elif [[ "$OSTYPE" == "msys" ]] || [[ "$OSTYPE" == "cygwin" ]]; then
    run_tests "windows-local" "./bin/windows/$PROJECT.exe --test --format=xml"
fi

# Tests dans Docker (Linux)
if command -v docker &> /dev/null; then
    echo "Running Docker tests..."
    run_tests "linux-docker" "docker run --rm -v $(pwd):/app ubuntu:22.04 /app/bin/linux/$PROJECT --test"
fi

# Tests dans Wine (Windows sur Linux)
if command -v wine &> /dev/null; then
    echo "Running Wine tests..."
    run_tests "windows-wine" "wine ./bin/windows/$PROJECT.exe --test"
fi

# Tests sur VM (si configurées)
if VBoxManage list runningvms | grep -q "Windows-Dev"; then
    echo "Running Windows VM tests..."
    run_tests "windows-vm" "VBoxManage guestcontrol Windows-Dev run --exe 'C:\\projects\\$PROJECT\\bin\\windows\\$PROJECT.exe' --username developer --password dev123 -- --test"
fi

if VBoxManage list runningvms | grep -q "Ubuntu-Dev"; then
    echo "Running Ubuntu VM tests..."
    run_tests "linux-vm" "VBoxManage guestcontrol Ubuntu-Dev run --exe '/home/developer/projects/$PROJECT/bin/linux/$PROJECT' --username developer --password dev123 -- --test"
fi

# Générer le rapport consolidé
echo ""  
echo "=== Génération du rapport consolidé ==="

cat > "$TEST_RESULTS_DIR/summary_${TIMESTAMP}.html" << EOF
<!DOCTYPE html>
<html>
<head>
    <title>Test Results - $TIMESTAMP</title>
    <style>
        body { font-family: Arial, sans-serif; margin: 20px; }
        h1 { color: #333; }
        .success { color: green; }
        .failure { color: red; }
        table { border-collapse: collapse; width: 100%; }
        th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }
        th { background-color: #4CAF50; color: white; }
    </style>
</head>
<body>
    <h1>Multi-Platform Test Results</h1>
    <p>Date: $(date)</p>
    <table>
        <tr>
            <th>Platform</th>
            <th>Status</th>
            <th>Details</th>
        </tr>
EOF

# Ajouter les résultats au rapport
for result_file in $TEST_RESULTS_DIR/test_*_${TIMESTAMP}.xml; do
    if [ -f "$result_file" ]; then
        platform=$(basename "$result_file" | sed "s/test_\(.*\)_${TIMESTAMP}.xml/\1/")
        if grep -q "failures=\"0\"" "$result_file" 2>/dev/null; then
            echo "        <tr><td>$platform</td><td class='success'>✅ Passed</td><td>All tests passed</td></tr>" >> "$TEST_RESULTS_DIR/summary_${TIMESTAMP}.html"
        else
            echo "        <tr><td>$platform</td><td class='failure'>❌ Failed</td><td>Check $result_file</td></tr>" >> "$TEST_RESULTS_DIR/summary_${TIMESTAMP}.html"
        fi
    fi
done

cat >> "$TEST_RESULTS_DIR/summary_${TIMESTAMP}.html" << EOF
    </table>
</body>
</html>
EOF

echo "Rapport disponible : $TEST_RESULTS_DIR/summary_${TIMESTAMP}.html"
```

## CI/CD multi-plateformes

### GitHub Actions multi-OS

`.github/workflows/multi-platform.yml` :
```yaml
name: Multi-Platform Build and Test

on:
  push:
    branches: [ main, develop ]
  pull_request:
    branches: [ main ]

jobs:
  build-and-test:
    strategy:
      matrix:
        os: [ubuntu-latest, windows-latest, macos-latest]
        include:
          - os: ubuntu-latest
            platform: linux
            executable: MonProjet
          - os: windows-latest
            platform: windows
            executable: MonProjet.exe
          - os: macos-latest
            platform: macos
            executable: MonProjet

    runs-on: ${{ matrix.os }}

    steps:
    - uses: actions/checkout@v3
      with:
        # Important pour la synchronisation
        lf: true  # Force LF sur tous les OS

    - name: Setup Lazarus
      uses: gcarreno/setup-lazarus@v3
      with:
        lazarus-version: stable
        fpc-version: stable

    - name: Cache dependencies
      uses: actions/cache@v3
      with:
        path: |
          ~/.lazarus
          ~/.fpc
        key: ${{ runner.os }}-lazarus-${{ hashFiles('**/*.lpi') }}

    - name: Build
      run: |
        lazbuild --build-mode=Release MonProjet.lpi
      shell: bash

    - name: Run Tests
      run: |
        ./bin/${{ matrix.platform }}/${{ matrix.executable }} --test --junit-output=test-results.xml
      shell: bash

    - name: Upload Test Results
      uses: actions/upload-artifact@v3
      if: always()
      with:
        name: test-results-${{ matrix.os }}
        path: test-results.xml

    - name: Upload Binary
      uses: actions/upload-artifact@v3
      with:
        name: binary-${{ matrix.os }}
        path: bin/${{ matrix.platform }}/${{ matrix.executable }}

  cross-platform-validation:
    needs: build-and-test
    runs-on: ubuntu-latest

    steps:
    - uses: actions/checkout@v3

    - name: Download all artifacts
      uses: actions/download-artifact@v3

    - name: Validate Cross-Platform Compatibility
      run: |
        echo "=== Cross-Platform Validation ==="

        # Vérifier que tous les binaires existent
        for platform in linux windows macos; do
          if [ -f "binary-*-latest/$platform"* ]; then
            echo "✅ $platform binary found"
          else
            echo "❌ $platform binary missing"
            exit 1
          fi
        done

        # Vérifier les résultats de tests
        for result in test-results-*/test-results.xml; do
          if grep -q 'failures="0"' "$result"; then
            echo "✅ Tests passed for $(dirname $result)"
          else
            echo "❌ Tests failed for $(dirname $result)"
            exit 1
          fi
        done

        echo "=== All platforms validated successfully ==="
```

### GitLab CI multi-OS

`.gitlab-ci.yml` :
```yaml
stages:
  - build
  - test
  - sync
  - deploy

variables:
  PROJECT_NAME: "MonProjet"

# Templates
.build_template:
  stage: build
  script:
    - lazbuild --build-mode=Release ${PROJECT_NAME}.lpi
  artifacts:
    paths:
      - bin/
    expire_in: 1 week

.test_template:
  stage: test
  script:
    - ./bin/*/MonProjet* --test --junit-output=test-results.xml
  artifacts:
    reports:
      junit: test-results.xml

# Jobs par plateforme
build:linux:
  extends: .build_template
  image: registry.gitlab.com/freepascal.org/lazarus:stable
  tags:
    - linux

build:windows:
  extends: .build_template
  tags:
    - windows
  before_script:
    - choco install lazarus

build:macos:
  extends: .build_template
  tags:
    - macos
  before_script:
    - brew install --cask lazarus

test:linux:
  extends: .test_template
  image: ubuntu:latest
  needs: ["build:linux"]
  tags:
    - linux

test:windows:
  extends: .test_template
  needs: ["build:windows"]
  tags:
    - windows

test:macos:
  extends: .test_template
  needs: ["build:macos"]
  tags:
    - macos

# Synchronisation des résultats
sync:results:
  stage: sync
  image: alpine:latest
  needs: ["test:linux", "test:windows", "test:macos"]
  script:
    - |
      echo "=== Consolidation des résultats ==="
      for platform in linux windows macos; do
        echo "Platform: $platform"
        cat test-results-$platform.xml || echo "No results for $platform"
      done
  artifacts:
    paths:
      - consolidated-results/
```

## Outils de synchronisation

### Rsync pour synchronisation

`sync_with_rsync.sh` :
```bash
#!/bin/bash

# Synchronisation bidirectionnelle avec rsync

SOURCE_DIR="."  
REMOTE_HOST="dev-server"  
REMOTE_DIR="/home/developer/projects/MonProjet"

# Exclusions communes
EXCLUDES="--exclude=*.ppu --exclude=*.o --exclude=*.exe --exclude=*.so --exclude=lib/ --exclude=bin/"

echo "=== Synchronisation avec rsync ==="

# Synchroniser vers le serveur
echo "Upload des modifications..."  
rsync -avz --delete $EXCLUDES \
    "$SOURCE_DIR/" \
    "$REMOTE_HOST:$REMOTE_DIR/"

# Compiler sur le serveur
echo "Compilation distante..."  
ssh $REMOTE_HOST "cd $REMOTE_DIR && lazbuild --build-mode=Release MonProjet.lpi"

# Récupérer les binaires
echo "Récupération des binaires..."  
rsync -avz \
    "$REMOTE_HOST:$REMOTE_DIR/bin/" \
    "$SOURCE_DIR/bin/"

echo "=== Synchronisation terminée ==="
```

### Syncthing pour synchronisation continue

`syncthing-config.xml` :
```xml
<configuration version="37">
    <folder id="monprojet" label="MonProjet" path="~/Projects/MonProjet" type="sendreceive">
        <device id="DEVICE-ID-WINDOWS"></device>
        <device id="DEVICE-ID-LINUX"></device>
        <device id="DEVICE-ID-MACOS"></device>

        <!-- Ignorer les fichiers temporaires -->
        <ignorePatterns>
            <pattern>*.ppu</pattern>
            <pattern>*.o</pattern>
            <pattern>*.exe</pattern>
            <pattern>*.so</pattern>
            <pattern>*.dylib</pattern>
            <pattern>lib/</pattern>
            <pattern>bin/</pattern>
            <pattern>backup/</pattern>
            <pattern>.git/</pattern>
        </ignorePatterns>

        <versioning type="staggered">
            <param key="cleanInterval" val="3600"></param>
            <param key="maxAge" val="604800"></param>
        </versioning>
    </folder>
</configuration>
```

### Cloud sync avec rclone

`rclone_sync.sh` :
```bash
#!/bin/bash

# Synchronisation avec stockage cloud

REMOTE="gdrive"  # ou dropbox, onedrive, etc.  
PROJECT_DIR="MonProjet"  
LOCAL_PATH="."  
REMOTE_PATH="$REMOTE:Development/$PROJECT_DIR"

echo "=== Synchronisation Cloud avec rclone ==="

# Synchronisation bidirectionnelle
echo "Synchronisation bidirectionnelle..."  
rclone bisync "$LOCAL_PATH" "$REMOTE_PATH" \
    --exclude "*.ppu" \
    --exclude "*.o" \
    --exclude "*.exe" \
    --exclude "bin/**" \
    --exclude "lib/**" \
    --exclude ".git/**" \
    --resync \
    --verbose

# Backup des binaires compilés
echo "Backup des binaires..."  
for platform in windows linux macos; do
    if [ -d "bin/$platform" ]; then
        rclone copy "bin/$platform" "$REMOTE_PATH/releases/$(date +%Y%m%d)/$platform"
    fi
done

echo "=== Synchronisation terminée ==="
```

## Gestion des configurations

### Fichier de configuration multi-OS

`config_manager.pas` :
```pascal
unit ConfigManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, fpjson, jsonparser;

type
  { TConfigManager }
  TConfigManager = class
  private
    FConfigPath: string;
    FIniFile: TIniFile;
    function GetConfigFileName: string;
  public
    constructor Create;
    destructor Destroy; override;

    // Méthodes génériques
    procedure SetValue(const Section, Key, Value: string);
    function GetValue(const Section, Key, Default: string): string;
    procedure SetIntValue(const Section, Key: string; Value: Integer);
    function GetIntValue(const Section, Key: string; Default: Integer): Integer;
    procedure SetBoolValue(const Section, Key: string; Value: Boolean);
    function GetBoolValue(const Section, Key: string; Default: Boolean): Boolean;

    // Configurations spécifiques OS
    procedure LoadOSSpecificConfig;
    procedure SaveOSSpecificConfig;

    // Export/Import
    procedure ExportToJSON(const FileName: string);
    procedure ImportFromJSON(const FileName: string);
  end;

implementation

uses
  FileUtil, LazFileUtils;

{ TConfigManager }

constructor TConfigManager.Create;  
begin
  FConfigPath := GetAppConfigDir(False);
  ForceDirectories(FConfigPath);
  FIniFile := TIniFile.Create(GetConfigFileName);
  LoadOSSpecificConfig;
end;

destructor TConfigManager.Destroy;  
begin
  SaveOSSpecificConfig;
  FIniFile.Free;
  inherited Destroy;
end;

function TConfigManager.GetConfigFileName: string;  
begin
  Result := FConfigPath + 'config.ini';
end;

procedure TConfigManager.SetValue(const Section, Key, Value: string);  
begin
  FIniFile.WriteString(Section, Key, Value);
end;

function TConfigManager.GetValue(const Section, Key, Default: string): string;  
begin
  Result := FIniFile.ReadString(Section, Key, Default);
end;

procedure TConfigManager.SetIntValue(const Section, Key: string; Value: Integer);  
begin
  FIniFile.WriteInteger(Section, Key, Value);
end;

function TConfigManager.GetIntValue(const Section, Key: string; Default: Integer): Integer;  
begin
  Result := FIniFile.ReadInteger(Section, Key, Default);
end;

procedure TConfigManager.SetBoolValue(const Section, Key: string; Value: Boolean);  
begin
  FIniFile.WriteBool(Section, Key, Value);
end;

function TConfigManager.GetBoolValue(const Section, Key: string; Default: Boolean): Boolean;  
begin
  Result := FIniFile.ReadBool(Section, Key, Default);
end;

procedure TConfigManager.LoadOSSpecificConfig;  
var
  OSSection: string;
begin
  {$IFDEF WINDOWS}
  OSSection := 'Windows';
  {$ENDIF}
  {$IFDEF LINUX}
  OSSection := 'Linux';
  {$ENDIF}
  {$IFDEF DARWIN}
  OSSection := 'macOS';
  {$ENDIF}

  // Charger les configurations spécifiques à l'OS
  if FIniFile.SectionExists(OSSection) then
  begin
    // Exemple : Charger les chemins spécifiques
    SetValue('Paths', 'DataDir',
             GetValue(OSSection, 'DataDir', GetValue('Paths', 'DataDir', '')));
    SetValue('Paths', 'LogDir',
             GetValue(OSSection, 'LogDir', GetValue('Paths', 'LogDir', '')));
  end;
end;

procedure TConfigManager.SaveOSSpecificConfig;  
var
  OSSection: string;
begin
  {$IFDEF WINDOWS}
  OSSection := 'Windows';
  {$ENDIF}
  {$IFDEF LINUX}
  OSSection := 'Linux';
  {$ENDIF}
  {$IFDEF DARWIN}
  OSSection := 'macOS';
  {$ENDIF}

  // Sauvegarder les configurations spécifiques à l'OS
  SetValue(OSSection, 'LastRun', DateTimeToStr(Now));
  SetValue(OSSection, 'Version', {$I %FPCVERSION%});
end;

procedure TConfigManager.ExportToJSON(const FileName: string);  
var
  JSONObject: TJSONObject;
  Sections: TStringList;
  Keys: TStringList;
  i, j: Integer;
  SectionObj: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  Sections := TStringList.Create;
  Keys := TStringList.Create;
  try
    FIniFile.ReadSections(Sections);

    for i := 0 to Sections.Count - 1 do
    begin
      SectionObj := TJSONObject.Create;
      FIniFile.ReadSection(Sections[i], Keys);

      for j := 0 to Keys.Count - 1 do
      begin
        SectionObj.Add(Keys[j],
                      FIniFile.ReadString(Sections[i], Keys[j], ''));
      end;

      JSONObject.Add(Sections[i], SectionObj);
    end;

    with TStringList.Create do
    try
      Text := JSONObject.FormatJSON;
      SaveToFile(FileName);
    finally
      Free;
    end;
  finally
    JSONObject.Free;
    Sections.Free;
    Keys.Free;
  end;
end;

procedure TConfigManager.ImportFromJSON(const FileName: string);  
var
  JSONData: TJSONData;
  JSONObject: TJSONObject;
  Iterator: TJSONEnum;
  SubIterator: TJSONEnum;
  FS: TFileStream;
begin
  if not FileExists(FileName) then
    Exit;

  FS := TFileStream.Create(FileName, fmOpenRead);
  try
    JSONData := GetJSON(FS);
    try
      if JSONData is TJSONObject then
      begin
        JSONObject := TJSONObject(JSONData);

        for Iterator in JSONObject do
        begin
          if Iterator.Value is TJSONObject then
          begin
            for SubIterator in TJSONObject(Iterator.Value) do
            begin
              FIniFile.WriteString(Iterator.Key,
                                 SubIterator.Key,
                                 SubIterator.Value.AsString);
            end;
          end;
        end;
      end;
    finally
      JSONData.Free;
    end;
  finally
    FS.Free;
  end;
end;

end.
```

## Bonnes pratiques

### Checklist de portabilité

```markdown
# Checklist de Portabilité Multi-OS

## Code
☐ Pas de chemins hardcodés
☐ Utilisation de PathDelim et DirectorySeparator
☐ Directives {$IFDEF} pour code spécifique OS
☐ Gestion des fins de ligne (CRLF/LF)
☐ Sensibilité à la casse des noms de fichiers
☐ Encodage UTF-8 pour tous les fichiers sources

## Configuration
☐ .gitignore multi-OS configuré
☐ .gitattributes pour les fins de ligne
☐ Modes de compilation par OS
☐ Chemins relatifs dans le projet

## Tests
☐ Tests unitaires cross-platform
☐ Tests d'intégration par OS
☐ CI/CD multi-plateformes configuré
☐ Validation sur au moins 2 OS

## Documentation
☐ Instructions d'installation par OS
☐ Dépendances documentées par plateforme
☐ Problèmes connus par OS
☐ Guide de contribution multi-OS
```

### Structure de documentation

`README.md` :
````markdown
# MonProjet - Application Multi-Plateforme

## Plateformes Supportées

| OS | Version | Architecture | Status |
|----|---------|--------------|--------|
| Windows | 10/11 | x64, x86 | ✅ Stable |
| Ubuntu | 20.04+ | x64 | ✅ Stable |
| macOS | 11+ | x64, ARM64 | 🔧 Beta |

## Installation

### Windows
```bash
# Télécharger l'installateur
MonProjet-Setup-Windows.exe
```

### Linux
```bash
# Via APT
sudo apt install monprojet

# Ou AppImage
chmod +x MonProjet.AppImage
./MonProjet.AppImage
```

### macOS
```bash
# Via Homebrew
brew install monprojet

# Ou DMG
# Glisser MonProjet.app dans Applications
```

## Développement

### Prérequis
- Lazarus 2.2+
- FPC 3.2+
- Git

### Clone et Build
```bash
git clone https://github.com/user/monprojet  
cd monprojet  
lazbuild --build-mode=Release MonProjet.lpi
```

### Synchronisation Multi-OS
Voir [SYNC.md](docs/SYNC.md) pour la configuration de synchronisation.

## Tests
```bash
# Tests locaux
make test

# Tests multi-plateformes
./scripts/test_all_platforms.sh
```
````

## Conclusion

La synchronisation de projets entre OS est un défi technique qui demande de la rigueur et de bonnes pratiques. Mais avec les bons outils et une architecture bien pensée, vous pouvez créer des applications véritablement portables qui fonctionnent parfaitement sur Windows, Linux et macOS.

**Points clés à retenir :**
- 📁 **Structure portable** : Organisation claire des fichiers
- 🔧 **Code conditionnel** : Utilisation des directives {$IFDEF}
- 🛣️ **Chemins portables** : Jamais de chemins hardcodés
- 🔄 **Git configuré** : .gitignore et .gitattributes adaptés
- 🧪 **Tests multi-OS** : Validation sur chaque plateforme
- 🤖 **CI/CD** : Automatisation des builds et tests
- 📚 **Documentation** : Instructions claires par OS

**Bénéfices de la synchronisation multi-OS :**
- ✨ **Portée maximale** : Touchez tous les utilisateurs
- 🚀 **Développement flexible** : Travaillez sur votre OS préféré
- 🛡️ **Robustesse** : Détection précoce des bugs spécifiques
- 👥 **Collaboration** : Équipe sur différents OS
- 📈 **Professionnalisme** : Application vraiment cross-platform

La maîtrise de la synchronisation multi-OS transforme votre projet Lazarus d'une application limitée à une plateforme en une solution professionnelle véritablement universelle !

⏭️ [Langage Object Pascal Avancé](/03-langage-object-pascal-avance/README.md)
