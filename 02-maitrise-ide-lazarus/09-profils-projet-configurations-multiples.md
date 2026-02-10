🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 2.9 Profils de projet et configurations multiples

## Introduction : Pourquoi plusieurs configurations ?

Imaginez que vous construisez une voiture. Parfois vous voulez une version course (rapide mais sans confort), parfois une version luxe (confortable mais plus lourde), et parfois une version test (avec tous les capteurs de diagnostic). C'est exactement ce que permettent les profils et configurations dans Lazarus : adapter votre projet à différents besoins sans dupliquer le code.

**Qu'est-ce qu'un profil de projet ?**

Un profil (ou mode de compilation) est un ensemble de paramètres de compilation sauvegardés sous un nom. Vous pouvez basculer entre les profils en un clic pour :
- 🐛 **Debug** : Avec symboles de débogage, sans optimisation
- 🚀 **Release** : Optimisé, sans debug, plus petit
- 🖥️ **Windows/Linux/Mac** : Différentes plateformes
- 🧪 **Test** : Avec couverture de code et assertions
- 📱 **Demo** : Version limitée pour démonstration

**Avantages des configurations multiples :**
- ⚡ **Productivité** : Basculer rapidement entre modes
- 🎯 **Précision** : Paramètres adaptés à chaque usage
- 🔒 **Sécurité** : Éviter de publier du code debug
- 👥 **Collaboration** : Configurations partagées en équipe
- 🔄 **CI/CD** : Builds automatisés variés

## Comprendre les modes de compilation

### Architecture des modes

```
Structure des modes de compilation :
├── Mode de base (Default)
│   └── Configuration par défaut du projet
├── Modes prédéfinis
│   ├── Debug
│   └── Release
└── Modes personnalisés
    ├── Debug-Windows-x64
    ├── Release-Linux-GTK
    ├── Test-Coverage
    └── Demo-Limited
```

### Hiérarchie et héritage

Les modes peuvent hériter les uns des autres :

```
Hiérarchie d'héritage :  
Default (base)
├── Debug (hérite de Default)
│   ├── Debug-Windows (hérite de Debug)
│   └── Debug-Linux (hérite de Debug)
└── Release (hérite de Default)
    ├── Release-Windows (hérite de Release)
    └── Release-Linux (hérite de Release)
```

**Principe d'héritage :**
- Un mode enfant hérite tous les paramètres du parent
- Vous pouvez surcharger des paramètres spécifiques
- Les modifications du parent se propagent aux enfants

## Créer et gérer les modes

### Accéder aux modes de compilation

**Projet → Options du projet → Modes de compilation**

```
┌─ Modes de compilation ───────────────────────────┐
│ Modes actuels :                                  │
│ ├── 📁 Default                                   │
│ ├── 🐛 Debug                                     │
│ │   ├── Debug-Win32                              │
│ │   ├── Debug-Win64                              │
│ │   └── Debug-Linux                              │
│ └── 🚀 Release                                   │
│     ├── Release-Win32                            │
│     ├── Release-Win64                            │
│     └── Release-Linux                            │
│                                                  │
│ Mode actif : [Debug-Win64        ▼]              │
│                                                  │
│ [Créer] [Dupliquer] [Supprimer] [Renommer]       │
│                                                  │
│ ☑ Afficher les options héritées                 │
│ ☑ Stocker la session avec le mode               │
└──────────────────────────────────────────────────┘
```

### Créer un nouveau mode

#### Méthode 1 : Via l'interface

1. **Cliquer sur "Créer"**
2. **Configurer le nouveau mode :**

```
┌─ Nouveau mode de compilation ────────────────────┐
│                                                  │
│ Nom : [Production-AWS_____________]              │
│                                                  │
│ Créer comme :                                    │
│ ○ Mode vide                                      │
│ ● Copie de : [Release           ▼]               │
│ ○ Hériter de : [Default         ▼]               │
│                                                  │
│ Options initiales :                              │
│ ☑ Optimisations niveau 3                        │
│ ☑ Strip symbols                                 │
│ ☐ Symboles de debug                             │
│                                                  │
│ [OK] [Annuler]                                   │
└──────────────────────────────────────────────────┘
```

#### Méthode 2 : Édition du fichier .lpi

```xml
<!-- Dans MonProjet.lpi -->
<BuildModes Count="3">
  <Item1 Name="Default" Default="True"/>
  <Item2 Name="Debug">
    <CompilerOptions>
      <Debugging>
        <GenerateDebugInfo Value="True"/>
        <DebugInfoType Value="dsDwarf3"/>
      </Debugging>
      <Optimizations>
        <OptimizationLevel Value="0"/>
      </Optimizations>
    </CompilerOptions>
  </Item2>
  <Item3 Name="Release">
    <CompilerOptions>
      <Debugging>
        <GenerateDebugInfo Value="False"/>
        <StripSymbols Value="True"/>
      </Debugging>
      <Optimizations>
        <OptimizationLevel Value="3"/>
      </Optimizations>
    </CompilerOptions>
  </Item3>
</BuildModes>
```

### Basculer entre les modes

#### Dans l'IDE

**Méthode rapide :**
- Barre d'outils : Liste déroulante des modes
- Raccourci : **Ctrl+Shift+B** pour ouvrir le sélecteur

```
┌─ Sélection rapide du mode ───────────────────────┐
│                                                  │
│ Sélectionnez le mode :                           │
│                                                  │
│ ○ Default                                        │
│ ● Debug                                          │
│ ○ Release                                        │
│ ○ Test                                           │
│                                                  │
│ [OK] [Annuler]                                   │
└──────────────────────────────────────────────────┘
```

#### En ligne de commande

```bash
# Compiler avec un mode spécifique
lazbuild --build-mode=Release MonProjet.lpi

# Lister les modes disponibles
lazbuild --list-modes MonProjet.lpi
```

## Configuration par mode

### Options de compilation

Pour chaque mode, vous pouvez configurer :

```
Options configurables par mode :
├── Compilation
│   ├── Optimisations (-O0 à -O3)
│   ├── Processeur cible
│   ├── Syntaxe (Delphi, ObjFPC, etc.)
│   └── Vérifications (range, overflow, I/O)
├── Débogage
│   ├── Symboles de debug (-g)
│   ├── Format (Dwarf, Stabs)
│   ├── Assertions (-Sa)
│   └── Heaptrc (détection fuites)
├── Liaison
│   ├── Strip symbols (-Xs)
│   ├── Smart linking (-XX)
│   └── Bibliothèques statiques/dynamiques
├── Chemins
│   ├── Unités (-Fu)
│   ├── Includes (-Fi)
│   ├── Bibliothèques (-Fl)
│   └── Sortie (-FU, -FE)
└── Messages
    ├── Verbosité
    ├── Warnings
    └── Hints
```

### Exemple : Mode Debug complet

```
Configuration Mode Debug :
├── Compilation
│   ├── Optimisation : Aucune (-O-)
│   ├── ☑ Range checking (-Cr)
│   ├── ☑ Overflow checking (-Co)
│   ├── ☑ I/O checking (-Ci)
│   └── ☑ Stack checking (-Ct)
├── Débogage
│   ├── ☑ Generate debug info (-g)
│   ├── Type : Dwarf 3 (-gw3)
│   ├── ☑ Use Heaptrc (-gh)
│   ├── ☑ Line numbers (-gl)
│   └── ☑ Assertions (-Sa)
├── Liaison
│   ├── ☐ Strip symbols
│   └── ☐ Smart linking
└── Sortie
    └── Répertoire : bin/debug
```

### Exemple : Mode Release optimisé

```
Configuration Mode Release :
├── Compilation
│   ├── Optimisation : Niveau 3 (-O3)
│   ├── ☑ Optimiser pour la taille (-Os)
│   ├── ☐ Range checking
│   ├── ☐ Overflow checking
│   └── Target CPU : COREAVX2
├── Débogage
│   ├── ☐ Generate debug info
│   ├── ☐ Heaptrc
│   └── ☐ Assertions
├── Liaison
│   ├── ☑ Strip symbols (-Xs)
│   ├── ☑ Smart linking (-XX)
│   └── ☑ Link smart (-CX)
└── Sortie
    └── Répertoire : bin/release
```

## Modes spécialisés

### Mode Test avec couverture

```pascal
// Configuration pour tests unitaires
Mode : Test-Coverage
├── Defines : TEST_MODE;COVERAGE
├── Compilation
│   ├── ☑ Assertions activées
│   └── ☑ Include test units
├── Chemins
│   └── Ajouter : tests/
└── Post-compilation
    └── Commande : run_tests.sh
```

Code conditionnel :
```pascal
{$IFDEF TEST_MODE}
uses consoletestrunner;

procedure RunTests;  
var
  App: TTestRunner;
begin
  WriteLn('Mode Test activé');
  {$IFDEF COVERAGE}
  InitializeCoverage;
  {$ENDIF}

  // Exécuter les tests avec FPCUnit
  App := TTestRunner.Create(nil);
  try
    App.Initialize;
    App.Run;
  finally
    App.Free;
  end;

  {$IFDEF COVERAGE}
  GenerateCoverageReport;
  {$ENDIF}
end;
{$ENDIF}
```

> **Note** : FPCUnit utilise `consoletestrunner` (pas `TestFramework` qui est Delphi/DUnit).

### Mode Demo limité

```pascal
// Configuration pour version démo
Mode : Demo-Limited
├── Defines : DEMO_VERSION;LIMITED_FEATURES
├── Compilation
│   └── Optimisation : Normal
└── Options personnalisées
    └── -dMAX_RECORDS=100 -dTRIAL_DAYS=30
```

Code avec limitations :
```pascal
unit Features;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Dialogs;

const
  {$IFDEF DEMO_VERSION}
  MAX_RECORDS = 100;
  TRIAL_DAYS = 30;
  WATERMARK = 'DEMO VERSION';
  {$ELSE}
  MAX_RECORDS = MaxInt;
  TRIAL_DAYS = 0;
  WATERMARK = '';
  {$ENDIF}

procedure SaveData;

implementation

procedure SaveData;  
begin
  {$IFDEF DEMO_VERSION}
  if RecordCount >= MAX_RECORDS then
  begin
    ShowMessage('Limite démo atteinte : ' + IntToStr(MAX_RECORDS) + ' enregistrements');
    Exit;
  end;
  {$ENDIF}

  // Sauvegarde normale
  DoSave;

  {$IFDEF DEMO_VERSION}
  AddWatermark(WATERMARK);
  {$ENDIF}
end;
```

### Mode Cross-platform

```
Modes pour multi-plateforme :
├── Windows-32bit
│   ├── OS : win32
│   ├── CPU : i386
│   └── Widget : win32
├── Windows-64bit
│   ├── OS : win64
│   ├── CPU : x86_64
│   └── Widget : win32
├── Linux-64bit-GTK
│   ├── OS : linux
│   ├── CPU : x86_64
│   └── Widget : gtk2
└── macOS-64bit
    ├── OS : darwin
    ├── CPU : x86_64
    └── Widget : cocoa
```

## Variables et macros

### Variables prédéfinies

Lazarus fournit des variables utilisables dans les configurations :

```
Variables système :
├── $(LazarusDir) : Répertoire Lazarus
├── $(FPCVer) : Version FPC (ex: 3.2.2)
├── $(TargetOS) : OS cible (win32, linux, etc.)
├── $(TargetCPU) : CPU cible (i386, x86_64, etc.)
├── $(ProjPath) : Chemin du projet
├── $(ProjName) : Nom du projet
├── $(ProjOutDir) : Répertoire de sortie
└── $(ConfigPath) : Chemin de config
```

### Macros personnalisées

Créer vos propres variables :

```
Projet → Options → Macros personnalisées
├── APP_VERSION = 1.2.3
├── COMPANY = MaSociete
├── BUILD_DATE = {$I %DATE%}
└── DEPLOY_PATH = \\serveur\deploy\$(TargetOS)
```

Utilisation :
```pascal
const
  APP_VERSION = '$(APP_VERSION)';
  COMPANY_NAME = '$(COMPANY)';

initialization
  WriteLn('Version : ' + APP_VERSION);
  WriteLn('© ' + COMPANY_NAME);
```

### Macros conditionnelles

```xml
<!-- Dans le .lpi -->
<ConditionalMacros>
  <If Condition="'$(TargetOS)'='win32'">
    <Then>
      <Macro Name="ICON_PATH" Value="icons\windows\"/>
      <Macro Name="INSTALLER" Value="inno"/>
    </Then>
  </If>
  <If Condition="'$(TargetOS)'='linux'">
    <Then>
      <Macro Name="ICON_PATH" Value="icons/linux/"/>
      <Macro Name="INSTALLER" Value="deb"/>
    </Then>
  </If>
</ConditionalMacros>
```

## Gestion des chemins par mode

### Structure de répertoires recommandée

```
MonProjet/
├── src/                 # Sources
├── bin/                 # Exécutables
│   ├── debug/
│   │   ├── win32/
│   │   ├── win64/
│   │   └── linux/
│   └── release/
│       ├── win32/
│       ├── win64/
│       └── linux/
├── lib/                 # Unités compilées
│   ├── debug/
│   │   └── $(TargetCPU)-$(TargetOS)/
│   └── release/
│       └── $(TargetCPU)-$(TargetOS)/
└── config/             # Configurations
    ├── debug.cfg
    └── release.cfg
```

### Configuration des chemins

```
Mode Debug - Chemins :
├── Unit output : lib/debug/$(TargetCPU)-$(TargetOS)
├── Target : bin/debug/$(TargetOS)/$(ProjName)
└── Debug output : temp/debug

Mode Release - Chemins :
├── Unit output : lib/release/$(TargetCPU)-$(TargetOS)
├── Target : bin/release/$(TargetOS)/$(ProjName)
└── Debug output : (vide)
```

## Scripts et automatisation

### Scripts de build par mode

#### Windows - build_all.bat

```batch
@echo off
echo === Compilation de tous les modes ===

set MODES=Debug Release Test Demo

for %%M in (%MODES%) do (
    echo.
    echo Compilation mode : %%M
    echo ========================

    lazbuild --build-mode=%%M MonProjet.lpi

    if errorlevel 1 (
        echo ERREUR : Mode %%M a échoué
        pause
        exit /b 1
    )
)

echo.  
echo === Tous les modes compilés avec succès ===

REM Créer les archives  
echo.  
echo Création des archives...

cd bin\release\win64
7z a ..\..\..\MonProjet-Win64-Release.zip *.*
cd ..\..\..

echo.  
echo Terminé !  
pause
```

#### Linux - build_all.sh

```bash
#!/bin/bash

echo "=== Compilation de tous les modes ==="

MODES="Debug Release Test Demo"

for MODE in $MODES; do
    echo ""
    echo "Compilation mode : $MODE"
    echo "========================"

    lazbuild --build-mode=$MODE MonProjet.lpi

    if [ $? -ne 0 ]; then
        echo "ERREUR : Mode $MODE a échoué"
        exit 1
    fi
done

echo ""  
echo "=== Tous les modes compilés avec succès ==="

# Créer les archives
echo ""  
echo "Création des archives..."

cd bin/release/linux  
tar czf ../../../MonProjet-Linux-Release.tar.gz *  
cd ../../..

echo ""  
echo "Terminé !"
```

### Commandes pre/post compilation

Chaque mode peut avoir ses commandes :

```
Mode Release - Commandes :
├── Avant compilation
│   ├── Commande : update_version.sh
│   └── Paramètres : $(ProjPath) $(APP_VERSION)
├── Après compilation
│   ├── Commande 1 : strip $(TargetFile)
│   ├── Commande 2 : upx --best $(TargetFile)
│   └── Commande 3 : sign.bat $(TargetFile)
└── Avant exécution
    └── Commande : check_env.sh
```

Script `update_version.sh` :
```bash
#!/bin/bash
PROJECT_PATH=$1  
VERSION=$2

# Mettre à jour le numéro de version
sed -i "s/VERSION = '.*'/VERSION = '$VERSION'/g" $PROJECT_PATH/version.inc

# Mettre à jour la date de build
echo "const BUILD_DATE = '$(date +%Y-%m-%d %H:%M:%S)';" > $PROJECT_PATH/builddate.inc
```

## Modes et travail en équipe

### Partage des configurations

#### Via Git

`.gitignore` adapté :
```gitignore
# Garder les modes de compilation
!*.lpi

# Ignorer les sessions personnelles
*.lps

# Ignorer les répertoires de build
bin/  
lib/

# Garder les configs de mode
!config/*.cfg
```

#### Export/Import de modes

```
Projet → Options → Modes de compilation
└── [Exporter] → Sauvegarder modes.xml

Autre développeur :
└── [Importer] → Charger modes.xml
```

### Conventions d'équipe

Document `BUILDING.md` :
```markdown
# Guide de compilation

## Modes standards

### Debug
- Utilisation : Développement quotidien
- Commande : `lazbuild --build-mode=Debug`
- Sortie : `bin/debug/`

### Release
- Utilisation : Version de production
- Commande : `lazbuild --build-mode=Release`
- Sortie : `bin/release/`
- Post-traitement : Signature requise

### Test
- Utilisation : Tests unitaires
- Commande : `lazbuild --build-mode=Test && ./run_tests.sh`
- Couverture : `coverage/report.html`

## Variables d'environnement

- `APP_VERSION` : Version de l'application
- `DEPLOY_SERVER` : Serveur de déploiement
- `LICENSE_KEY` : Clé de licence (Release uniquement)
```

## Intégration CI/CD

### GitHub Actions avec modes

`.github/workflows/build.yml` :
```yaml
name: Build All Modes

on:
  push:
    branches: [ main, develop ]
  pull_request:
    branches: [ main ]

jobs:
  build:
    strategy:
      matrix:
        mode: [Debug, Release, Test]
        os: [ubuntu-latest, windows-latest, macos-latest]

    runs-on: ${{ matrix.os }}

    steps:
    - uses: actions/checkout@v2

    - name: Setup Lazarus
      uses: gcarreno/setup-lazarus@v3
      with:
        lazarus-version: stable

    - name: Build ${{ matrix.mode }}
      run: lazbuild --build-mode=${{ matrix.mode }} MonProjet.lpi

    - name: Run Tests
      if: matrix.mode == 'Test'
      run: |
        if [ "$RUNNER_OS" == "Windows" ]; then
          ./bin/test/win64/MonProjet.exe --run-tests
        else
          ./bin/test/linux/MonProjet --run-tests
        fi

    - name: Upload Artifacts
      if: matrix.mode == 'Release'
      uses: actions/upload-artifact@v2
      with:
        name: ${{ matrix.os }}-${{ matrix.mode }}
        path: bin/release/
```

### Jenkins Pipeline

`Jenkinsfile` :
```groovy
pipeline {
    agent any

    parameters {
        choice(name: 'BUILD_MODE',
               choices: ['Debug', 'Release', 'Test', 'All'],
               description: 'Mode de compilation')
    }

    stages {
        stage('Checkout') {
            steps {
                checkout scm
            }
        }

        stage('Build') {
            steps {
                script {
                    if (params.BUILD_MODE == 'All') {
                        def modes = ['Debug', 'Release', 'Test']
                        modes.each { mode ->
                            sh "lazbuild --build-mode=${mode} MonProjet.lpi"
                        }
                    } else {
                        sh "lazbuild --build-mode=${params.BUILD_MODE} MonProjet.lpi"
                    }
                }
            }
        }

        stage('Test') {
            when {
                expression {
                    params.BUILD_MODE == 'Test' || params.BUILD_MODE == 'All'
                }
            }
            steps {
                sh './bin/test/linux/MonProjet --run-tests --junit-output=test-results.xml'
                junit 'test-results.xml'
            }
        }

        stage('Archive') {
            when {
                expression { params.BUILD_MODE == 'Release' }
            }
            steps {
                archiveArtifacts artifacts: 'bin/release/**/*',
                                 fingerprint: true
            }
        }
    }
}
```

## Débogage et diagnostic

### Vérifier le mode actif

```pascal
program ShowBuildMode;
{$mode objfpc}{$H+}

begin
  WriteLn('=== Configuration de compilation ===');

  {$IFDEF DEBUG}
  WriteLn('Mode : DEBUG');
  {$ENDIF}

  {$IFDEF RELEASE}
  WriteLn('Mode : RELEASE');
  {$ENDIF}

  {$IFDEF TEST_MODE}
  WriteLn('Mode : TEST');
  {$ENDIF}

  WriteLn('OS : ', {$I %FPCTARGETOS%});
  WriteLn('CPU : ', {$I %FPCTARGETCPU%});
  WriteLn('FPC : ', {$I %FPCVERSION%});
  WriteLn('Date : ', {$I %DATE%}, ' ', {$I %TIME%});

  // Vérifier les assertions
  {$IFOPT C+}
  WriteLn('Assertions : Activées');
  {$ELSE}
  WriteLn('Assertions : Désactivées');
  {$ENDIF}

  ReadLn;
end.
```

### Journal de compilation par mode

```pascal
unit BuildLog;

{$mode objfpc}{$H+}

interface

procedure LogBuildInfo;

implementation

uses
  SysUtils;

procedure LogBuildInfo;  
var
  LogFile: TextFile;
  LogPath: string;
begin
  {$IFDEF DEBUG}
  LogPath := 'build_debug.log';
  {$ELSE}
  LogPath := 'build_release.log';
  {$ENDIF}

  AssignFile(LogFile, LogPath);
  if FileExists(LogPath) then
    Append(LogFile)
  else
    Rewrite(LogFile);

  WriteLn(LogFile, '=== Build Info ===');
  WriteLn(LogFile, 'Date : ', DateTimeToStr(Now));
  // Note : {$I %BUILDMODE%} n'existe pas en FPC.
  // Utiliser des defines pour identifier le mode :
  {$IFDEF DEBUG}
  WriteLn(LogFile, 'Mode : Debug');
  {$ELSE}
  WriteLn(LogFile, 'Mode : Release');
  {$ENDIF}
  WriteLn(LogFile, 'Version FPC : ', {$I %FPCVERSION%});
  WriteLn(LogFile, 'Target : ', {$I %FPCTARGETOS%}, '-', {$I %FPCTARGETCPU%});
  WriteLn(LogFile, '');

  CloseFile(LogFile);
end;

initialization
  LogBuildInfo;
end.
```

## Bonnes pratiques

### Organisation des modes

```
Stratégie recommandée :
├── Modes de base
│   ├── Debug (développement)
│   └── Release (production)
├── Modes plateforme
│   ├── Par OS (Windows, Linux, Mac)
│   └── Par architecture (32/64 bits)
├── Modes spéciaux
│   ├── Test (avec framework de test)
│   ├── Profile (avec profiling)
│   └── Demo (version limitée)
└── Modes déploiement
    ├── Staging (pré-production)
    └── Production (avec signature)
```

### Nommage des modes

```
Convention de nommage :
[Objectif]-[Plateforme]-[Architecture]-[Variante]

Exemples :
├── Debug-Win64
├── Release-Linux-x64
├── Test-Win32-Coverage
├── Demo-CrossPlatform
└── Production-AWS-Lambda
```

### Documentation des modes

Créer un fichier `MODES.md` :

```markdown
# Modes de compilation

## Debug
- **Usage** : Développement
- **Optimisation** : Désactivée
- **Debug** : Complet avec Heaptrc
- **Assertions** : Activées
- **Sortie** : `bin/debug/`

## Release
- **Usage** : Production
- **Optimisation** : Niveau 3 + taille
- **Debug** : Aucun
- **Assertions** : Désactivées
- **Sortie** : `bin/release/`
- **Post-build** : Strip + UPX + Signature

## Test
- **Usage** : Tests unitaires
- **Defines** : TEST_MODE, MOCK_DATA
- **Includes** : `tests/`
- **Framework** : FPCUnit
```

## Conclusion

Les profils de projet et configurations multiples sont essentiels pour un développement professionnel. Ils permettent d'adapter votre projet à chaque situation sans compromis ni duplication de code.

**Bénéfices acquis :**
- 🎯 **Précision** : Paramètres optimaux pour chaque usage
- ⚡ **Rapidité** : Basculement instantané entre configurations
- 🔒 **Sécurité** : Éviter les erreurs de déploiement
- 👥 **Collaboration** : Configurations partagées et documentées
- 🤖 **Automatisation** : CI/CD avec builds multiples

**Points clés à retenir :**
- Créez au minimum Debug et Release
- Utilisez l'héritage pour éviter la duplication
- Documentez vos modes pour l'équipe
- Automatisez avec des scripts
- Testez chaque mode régulièrement

La maîtrise des modes de compilation transforme un projet amateur en solution professionnelle, prête pour le développement, les tests et la production !

⏭️ [Automatisation avec lazbuild](/02-maitrise-ide-lazarus/10-automatisation-lazbuild.md)
