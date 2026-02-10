🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 26.1 Contribution au projet FreePascal/Lazarus

## Introduction

Contribuer à FreePascal et Lazarus est une excellente façon de perfectionner vos compétences de développeur avancé tout en participant à un écosystème open source utilisé dans le monde entier. Ce chapitre vous guidera à travers toutes les étapes pour devenir un contributeur efficace, que vous souhaitiez corriger un bug, ajouter une fonctionnalité, améliorer la documentation ou aider la communauté.

### Pourquoi contribuer ?

**Avantages personnels :**
- Amélioration de vos compétences en programmation Pascal
- Compréhension approfondie du fonctionnement interne d'un compilateur et d'un IDE
- Expérience du travail collaboratif sur un grand projet
- Reconnaissance au sein de la communauté des développeurs
- Portfolio enrichi avec des contributions concrètes
- Apprentissage des meilleures pratiques de développement

**Avantages pour la communauté :**
- Amélioration de la qualité du compilateur et de l'IDE
- Correction de bugs qui affectent les utilisateurs
- Ajout de fonctionnalités demandées
- Documentation plus complète et accessible
- Traductions dans différentes langues
- Support aux nouveaux utilisateurs

### Types de contributions possibles

Vous n'avez pas besoin d'être un expert pour contribuer ! Voici différents niveaux de contribution :

**Contributions pour débutants :**
- Signaler des bugs avec des rapports détaillés
- Améliorer la documentation (corriger des fautes, clarifier des explications)
- Traduire l'IDE et la documentation
- Répondre aux questions sur les forums
- Créer des tutoriels et exemples

**Contributions intermédiaires :**
- Corriger des bugs mineurs dans les packages
- Ajouter des tests unitaires
- Améliorer des composants LCL existants
- Créer des exemples d'utilisation
- Optimiser du code existant

**Contributions avancées :**
- Corriger des bugs dans le compilateur FPC
- Implémenter de nouvelles fonctionnalités du langage
- Développer de nouveaux backends du compilateur
- Améliorer l'architecture de l'IDE
- Optimisations du compilateur
- Support de nouvelles plateformes

## Comprendre l'organisation des projets

### FreePascal - Le compilateur

**Structure du projet FreePascal :**

FreePascal est organisé en plusieurs composants principaux :

```
freepascal/
├── compiler/        # Code source du compilateur FPC
│   ├── systems/     # Support des différents OS
│   ├── x86/         # Backend x86
│   ├── arm/         # Backend ARM
│   └── ...
├── rtl/             # Run-Time Library (bibliothèque d'exécution)
│   ├── objpas/      # Unités Object Pascal
│   ├── inc/         # Fichiers include multi-plateformes
│   ├── win/         # Spécifique Windows
│   ├── unix/        # Spécifique Unix/Linux
│   └── ...
├── packages/        # Packages additionnels
│   ├── fcl-base/    # Free Component Library - Base
│   ├── fcl-web/     # FCL Web
│   ├── fcl-db/      # FCL Database
│   ├── sqlite/      # Support SQLite
│   └── ...
├── utils/           # Utilitaires (fpcres, fpdoc, etc.)
├── tests/           # Suite de tests
└── docs/            # Documentation
```

**Rôles des différents répertoires :**

- **compiler/** : Cœur du compilateur écrit en Pascal. Contient l'analyseur syntaxique, le générateur de code, l'optimiseur.
- **rtl/** : Bibliothèque d'exécution (Run-Time Library) qui fournit les unités de base (System, SysUtils, Classes, etc.)
- **packages/** : Bibliothèques supplémentaires organisées en packages
- **utils/** : Outils en ligne de commande (fpcmake, fpdoc pour documentation, etc.)

### Lazarus - L'IDE

**Structure du projet Lazarus :**

```
lazarus/
├── ide/             # Code source de l'IDE lui-même
│   ├── frames/      # Cadres réutilisables de l'interface
│   ├── packages/    # Packages internes de l'IDE
│   └── ...
├── lcl/             # Lazarus Component Library
│   ├── interfaces/  # Widgetsets (Win32, GTK2, Qt, etc.)
│   │   ├── win32/
│   │   ├── gtk2/
│   │   ├── qt5/
│   │   └── ...
│   ├── forms/       # Gestion des formulaires
│   ├── controls/    # Contrôles de base
│   └── ...
├── components/      # Composants Lazarus
│   ├── codetools/   # Outils d'analyse de code
│   ├── synedit/     # Éditeur de texte avec coloration syntaxique
│   ├── lazcontrols/ # Contrôles supplémentaires
│   ├── tachart/     # Graphiques et diagrammes
│   └── ...
├── designer/        # Designer de formulaires visuel
├── debugger/        # Interface de débogage
├── packager/        # Gestionnaire de packages
├── converter/       # Convertisseur Delphi vers Lazarus
├── docs/            # Documentation
├── examples/        # Exemples d'applications
└── tools/           # Outils divers
```

**Composants clés :**

- **ide/** : L'application IDE elle-même avec toute sa logique
- **lcl/** : La bibliothèque de composants visuels multi-plateformes
- **components/** : Extensions et composants additionnels
- **designer/** : Le concepteur visuel de formulaires (drag & drop)
- **codetools/** : Outils d'analyse et de refactoring du code

## Préparation de l'environnement de développement

### Installer les outils nécessaires

**Outils indispensables :**

1. **Git** : Système de contrôle de version
   ```bash
   # Windows (avec chocolatey)
   choco install git

   # Ubuntu
   sudo apt install git
   ```

2. **FreePascal et Lazarus** : Versions stables installées
   - Télécharger depuis https://www.lazarus-ide.org/
   - Ou compiler depuis les sources (voir plus loin)

3. **Éditeur de texte** (optionnel mais utile pour Git commits)
   ```bash
   # Ubuntu
   sudo apt install vim nano gedit

   # Windows
   # Notepad++, VS Code, etc.
   ```

4. **Outils de compilation** (pour compiler FPC depuis sources)
   ```bash
   # Ubuntu - outils nécessaires
   sudo apt install build-essential binutils-dev
   sudo apt install libncurses5-dev libgpm-dev

   # Windows
   # Installer un FPC existant d'abord (bootstrap)
   ```

### Configuration de Git

Avant de commencer, configurez votre identité Git :

```bash
git config --global user.name "Votre Nom"  
git config --global user.email "votre.email@example.com"

# Configuration optionnelle mais recommandée
git config --global core.autocrlf input  # Linux/Mac  
git config --global core.autocrlf true   # Windows

git config --global pull.rebase false  
git config --global init.defaultBranch main
```

### Créer un compte GitLab

Les projets FreePascal et Lazarus utilisent GitLab :

1. **Créer un compte** sur https://gitlab.com
2. **Configurer une clé SSH** pour faciliter les push/pull :

```bash
# Générer une clé SSH (si vous n'en avez pas)
ssh-keygen -t ed25519 -C "votre.email@example.com"

# Afficher la clé publique
cat ~/.ssh/id_ed25519.pub

# Copier cette clé et l'ajouter dans GitLab :
# GitLab > Settings > SSH Keys
```

3. **Tester la connexion** :
```bash
ssh -T git@gitlab.com
# Devrait afficher : "Welcome to GitLab, @votrenom!"
```

## Cloner les dépôts sources

### Cloner FreePascal

Le dépôt officiel de FreePascal est hébergé sur GitLab :

```bash
# Cloner le dépôt principal
git clone https://gitlab.com/freepascal.org/fpc/source.git fpc-source

cd fpc-source

# Voir les branches disponibles
git branch -a

# Par défaut, vous êtes sur 'main' (développement actif)
# Pour une version stable, basculer sur une branche :
git checkout fixes_3_2
```

**Branches importantes :**
- `main` : Branche de développement (futur 3.4.0)
- `fixes_3_2` : Corrections pour la version stable 3.2.x
- `release_3_2_2` : Version release spécifique

### Cloner Lazarus

```bash
# Cloner le dépôt Lazarus
git clone https://gitlab.com/freepascal.org/lazarus/lazarus.git lazarus-source

cd lazarus-source

# Branches principales
git branch -a

# main = développement
# fixes_3_0 = corrections pour Lazarus 3.0.x
```

### Garder votre copie à jour

Il est important de synchroniser régulièrement avec le dépôt officiel :

```bash
# Dans le répertoire du projet
cd fpc-source  # ou lazarus-source

# Récupérer les dernières modifications
git fetch origin

# Mettre à jour votre branche locale
git pull origin main

# Voir l'historique
git log --oneline --graph --decorate --all -10
```

## Compiler FreePascal depuis les sources

### Compilation sous Linux/Ubuntu

**Prérequis :**
- Un compilateur FPC déjà installé (version "bootstrap")
- Outils de build (make, binutils)

**Étapes de compilation :**

```bash
cd fpc-source

# Compiler le compilateur et la RTL
make clean all

# Installer dans le système (optionnel)
sudo make install PREFIX=/usr/local

# Ou installer localement pour tests
make install PREFIX=$HOME/fpc-dev

# Créer un lien symbolique pour faciliter l'utilisation
sudo ln -sf /usr/local/lib/fpc/3.3.1/ppcx64 /usr/local/bin/
```

**Compilation ciblée :**

```bash
# Compiler uniquement le compilateur
make compiler

# Compiler uniquement la RTL
make rtl

# Compiler un package spécifique
cd packages/fcl-base  
make clean all
```

### Compilation sous Windows

**Prérequis :**
- FPC installé (bootstrap compiler)
- Make pour Windows (MinGW make ou GNU make)

**Étapes :**

```cmd
cd fpc-source

REM Nettoyer et compiler tout  
make clean all

REM Installer localement  
make install PREFIX=C:\fpc-dev

REM Compiler juste le compilateur  
make compiler
```

**Astuce Windows :** Utilisez un environnement type MSYS2 pour faciliter la compilation :

```bash
# Dans MSYS2
pacman -S make mingw-w64-x86_64-gcc

cd /c/dev/fpc-source  
make clean all
```

### Tests de base après compilation

Vérifiez que votre compilation fonctionne :

```bash
# Tester le compilateur
./compiler/ppcx64 --version

# Compiler un programme simple
echo 'begin writeln("Hello FPC!"); end.' > test.pas
./compiler/ppcx64 test.pas
./test

# Exécuter la suite de tests
cd tests  
make clean all
```

## Compiler Lazarus depuis les sources

### Compilation de Lazarus sous Linux

```bash
cd lazarus-source

# Définir le compilateur FPC à utiliser
export FPC=/usr/local/bin/fpc  # ou chemin de votre FPC compilé

# Compiler l'IDE complet
make clean bigide

# Ou compiler seulement l'IDE de base
make clean all

# Lancer Lazarus compilé
./lazarus
```

**Options de compilation utiles :**

```bash
# Compiler avec des informations de débogage
make clean all OPT="-g -gl -O1"

# Compiler en mode release optimisé
make clean all OPT="-O3 -Xs"

# Compiler un widgetset spécifique
make clean all LCL_PLATFORM=gtk2  
make clean all LCL_PLATFORM=qt5
```

### Compilation sous Windows

```cmd
cd lazarus-source

REM Définir le FPC à utiliser  
set PATH=C:\fpc\bin\i386-win32;%PATH%

REM Compiler  
make clean bigide

REM Lancer  
lazarus.exe
```

### Structure de compilation modulaire

Vous pouvez compiler des parties spécifiques :

```bash
# Compiler uniquement les outils
make cleanide ideintf

# Compiler la LCL uniquement
cd lcl  
make clean all

# Compiler un package spécifique
cd components/synedit  
lazbuild synedit.lpk
```

## Comprendre le processus de contribution

### Le workflow Git/GitLab standard

Le processus de contribution suit ce schéma classique :

```
1. Fork du projet → Votre copie sur GitLab
2. Clone local → Copie sur votre machine
3. Création branche → Branche pour votre modification
4. Modifications → Écriture du code
5. Commit → Enregistrement local
6. Push → Envoi vers votre fork
7. Merge Request → Demande d'intégration
8. Code Review → Relecture par les mainteneurs
9. Merge → Intégration dans le projet principal
```

### Fork du projet sur GitLab

**Étapes pour forker :**

1. Connectez-vous sur GitLab
2. Accédez au projet : https://gitlab.com/freepascal.org/fpc/source (FPC) ou https://gitlab.com/freepascal.org/lazarus/lazarus
3. Cliquez sur le bouton **"Fork"** en haut à droite
4. Sélectionnez votre namespace (votre compte utilisateur)
5. Le fork est créé dans votre espace : `https://gitlab.com/votrecompte/fpc-source`

**Cloner votre fork :**

```bash
# Cloner VOTRE fork (remplacer 'votrecompte')
git clone git@gitlab.com:votrecompte/fpc-source.git  
cd fpc-source

# Ajouter le dépôt officiel comme "upstream"
git remote add upstream https://gitlab.com/freepascal.org/fpc/source.git

# Vérifier les remotes configurés
git remote -v
# origin    git@gitlab.com:votrecompte/fpc-source.git (fetch)
# origin    git@gitlab.com:votrecompte/fpc-source.git (push)
# upstream  https://gitlab.com/freepascal.org/fpc/source.git (fetch)
```

### Workflow de contribution typique

**Exemple complet : corriger un bug dans la RTL**

```bash
# 1. Synchroniser avec upstream
git fetch upstream  
git checkout main  
git merge upstream/main

# 2. Créer une branche pour votre correction
git checkout -b fix-sysutils-format-bug

# 3. Faire vos modifications
# Éditer rtl/objpas/sysutils/sysstr.inc (par exemple)
nano rtl/objpas/sysutils/sysstr.inc

# 4. Compiler et tester
make rtl
./tests/test-format

# 5. Vérifier les modifications
git status  
git diff

# 6. Ajouter les fichiers modifiés
git add rtl/objpas/sysutils/sysstr.inc

# 7. Créer un commit avec message clair
git commit -m "RTL: Fix format string handling with empty string

- Fixed crash when format string is empty
- Added test case for empty format
- Resolves issue #12345"

# 8. Pousser vers VOTRE fork
git push origin fix-sysutils-format-bug

# 9. Créer une Merge Request sur GitLab
# Se rendre sur GitLab, bouton "Create merge request"
```

### Créer une bonne Merge Request (MR)

**Éléments d'une MR de qualité :**

1. **Titre explicite** :
   - ✅ Bon : "RTL: Fix memory leak in TStringList.Delete"
   - ❌ Mauvais : "Fix bug"

2. **Description détaillée** :
```markdown
## Description
Fixes a memory leak in TStringList.Delete when the list contains objects.

## Problem
When calling TStringList.Delete with OwnsObjects=True, the object  
was not freed properly if an exception occurred during deletion.

## Solution
Added try-finally block to ensure proper cleanup.

## Testing
- Added unit test test_stringlist_delete_with_exception
- Verified no regression with existing tests
- Tested on Windows 10 and Ubuntu 22.04

## Related Issues
Closes #12345
```

3. **Tests** : Toujours ajouter ou exécuter des tests
4. **Documentation** : Mettre à jour si nécessaire
5. **Changements minimaux** : Ne modifier que ce qui est nécessaire

### Répondre aux commentaires de révision

Les mainteneurs vont probablement demander des modifications :

```bash
# 1. Effectuer les corrections demandées
nano rtl/objpas/sysutils/sysstr.inc

# 2. Commiter les changements
git add rtl/objpas/sysutils/sysstr.inc  
git commit -m "Address review comments: improve error message"

# 3. Pousser les modifications
git push origin fix-sysutils-format-bug

# La MR se met à jour automatiquement
```

**Bonnes pratiques :**
- Répondre poliment à tous les commentaires
- Expliquer vos choix de design si nécessaire
- Accepter les critiques constructives
- Demander des clarifications si besoin

## Standards de code et conventions

### Conventions de nommage FreePascal

**Variables et paramètres :**
```pascal
// PascalCase pour les types
type
  TMyClass = class
    // Premier caractère en majuscule pour les membres publics
    MyPublicField: Integer;
  private
    // F + PascalCase pour les champs privés
    FPrivateField: Integer;
    // Getters/Setters avec Get/Set préfixe
    function GetValue: Integer;
    procedure SetValue(AValue: Integer);
  end;

// Paramètres avec préfixe A
procedure DoSomething(AParam1: Integer; AParam2: string);

// Variables locales en minuscule ou camelCase
var
  i, count: Integer;
  myLocalVariable: string;
```

**Unités et fichiers :**
```pascal
// Nom d'unité en minuscules
unit sysutils;

// Ou avec préfixe pour organisation
unit fcl.db.sqldb;
```

### Formatage du code

**Indentation et espacement :**

```pascal
// Utiliser 2 espaces d'indentation
procedure Example;  
var
  i: Integer;
begin
  for i := 0 to 10 do
  begin
    WriteLn(i);
    if i mod 2 = 0 then
      WriteLn('Even')
    else
      WriteLn('Odd');
  end;
end;

// Espaces autour des opérateurs
x := y + z;  
Result := (a * b) + c;

// Pas d'espace avant les parenthèses de fonction
WriteLn('Hello');  // ✅  
WriteLn ('Hello'); // ❌

// Mais espace après les mots-clés
if x > 0 then     // ✅  
if(x > 0)then     // ❌
```

**Blocs begin/end :**

```pascal
// Préféré dans FPC/Lazarus : begin sur la même ligne
if Condition then begin
  DoSomething;
  DoSomethingElse;
end;

// Pour les fonctions/procédures
function GetValue: Integer;  
begin
  Result := FValue;
end;
```

### Commentaires et documentation

**Commentaires de code :**

```pascal
// Commentaires simples pour explications courtes
x := x + 1; // Incrémenter le compteur

{ Commentaires multi-lignes
  pour explications longues ou
  descriptions détaillées }

(* Style alternatif de commentaires
   multi-lignes, moins utilisé *)
```

**Documentation PasDoc :**

```pascal
{**
  Calcule la somme de deux nombres.

  @param(AValue1 Premier nombre à additionner)
  @param(AValue2 Second nombre à additionner)
  @returns(La somme des deux nombres)
  @raises(EOverflow Si le résultat dépasse les limites d'un Integer)

  @bold(Exemple:)
  @longcode(#
  var
    Sum: Integer;
  begin
    Sum := Add(10, 20);
    WriteLn(Sum); // Affiche 30
  end;
  #)
}
function Add(AValue1, AValue2: Integer): Integer;  
begin
  Result := AValue1 + AValue2;
end;
```

### Directives de compilation

**Organisation des directives :**

```pascal
{$mode objfpc}        // Mode Object Pascal
{$H+}                 // Chaînes longues (AnsiString)
{$J-}                 // Constantes typées non modifiables

// Directives conditionnelles multi-plateforme
{$IFDEF WINDOWS}
  // Code spécifique Windows
  uses Windows, Registry;
{$ENDIF}

{$IFDEF UNIX}
  {$IFDEF LINUX}
    // Code spécifique Linux
    uses BaseUnix, Linux;
  {$ENDIF}
{$ENDIF}

// Directives pour compatibilité
{$IFDEF FPC}
  {$mode delphi}
{$ENDIF}
```

## Types de contributions détaillés

### 1. Rapporter des bugs

**Où rapporter :**
- FreePascal : https://gitlab.com/freepascal.org/fpc/source/-/issues
- Lazarus : https://gitlab.com/freepascal.org/lazarus/lazarus/-/issues

**Structure d'un bon rapport de bug :**

````markdown
## Description
Brief description of the bug

## Steps to Reproduce
1. Create a new project
2. Add a TButton on Form1
3. Add this code in button click event: ...
4. Run the application
5. Click the button

## Expected Behavior
The application should display "Hello World"

## Actual Behavior
The application crashes with Access Violation

## Environment
- FreePascal version: 3.2.2
- Lazarus version: 2.2.6
- Operating System: Ubuntu 22.04 LTS
- Widgetset: GTK2

## Code Sample
```pascal
procedure TForm1.Button1Click(Sender: TObject);  
var
  List: TStringList;
begin
  List := TStringList.Create;
  // Bug: List not freed -> memory leak
  ShowMessage(List.Text);
end;
```

## Additional Information
- The bug only occurs when compiled with -O3 optimization
- Works correctly with FPC 3.0.4
- Stack trace attached
````

**Informations essentielles à inclure :**
- Version exacte de FPC/Lazarus
- Système d'exploitation et architecture (32/64 bits)
- Code minimal reproduisant le bug
- Messages d'erreur complets
- Options de compilation utilisées

### 2. Corriger des bugs

**Processus de correction :**

```bash
# 1. Trouver un bug à corriger
# Chercher les issues avec label "good first issue" ou "help wanted"

# 2. Comprendre le problème
# Lire l'issue, reproduire le bug localement

# 3. Localiser le code problématique
# Utiliser grep, ide find, debugger

grep -r "TStringList.Delete" rtl/

# 4. Écrire un test qui échoue
cd tests/test/units/sysutils  
nano tstringlist_test.pas

# 5. Corriger le bug
nano rtl/objpas/classes.inc

# 6. Vérifier que le test passe maintenant
make clean all
./test_sysutils

# 7. Soumettre la correction
git commit -m "Classes: Fix TStringList.Delete memory leak"
```

**Exemple de correction :**

```pascal
// AVANT (buggy)
procedure TStringList.Delete(Index: Integer);  
begin
  if (Index < 0) or (Index >= FCount) then
    Error(SListIndexError, Index);
  FCount := FCount - 1;
  if Index < FCount then
    System.Move(FList[Index + 1], FList[Index],
                (FCount - Index) * SizeOf(Pointer));
  // Bug: FList[FCount] not cleared -> dangling pointer
end;

// APRÈS (corrigé)
procedure TStringList.Delete(Index: Integer);  
begin
  if (Index < 0) or (Index >= FCount) then
    Error(SListIndexError, Index);
  FCount := FCount - 1;
  if Index < FCount then
    System.Move(FList[Index + 1], FList[Index],
                (FCount - Index) * SizeOf(Pointer));
  FList[FCount] := nil; // Fix: Clear dangling pointer
end;
```

### 3. Améliorer la documentation

**Documentation du code source :**

La documentation utilise le format **PasDoc** (similaire à JavaDoc) :

```pascal
{**
  TCustomList est une classe de base pour les listes.

  Cette classe fournit les mécanismes de base pour gérer
  une collection d'éléments avec allocation dynamique.

  @seealso(TList)
  @seealso(TObjectList)
  @author(Équipe FreePascal)
  @created(15 Mars 2000)
  @lastmod(10 Janvier 2024)
}
TCustomList = class(TPersistent)  
private
  FList: Pointer;      //<! Pointeur vers le tableau d'éléments
  FCount: Integer;     //<! Nombre d'éléments dans la liste
  FCapacity: Integer;  //<! Capacité allouée

  {** Agrandit la capacité de la liste automatiquement }
  procedure Grow;

public
  {**
    Crée une nouvelle instance de liste.
    La capacité initiale est de 0.
  }
  constructor Create; virtual;

  {**
    Libère la liste et tous ses éléments.
    @bold(Note:) Les objets contenus ne sont pas libérés automatiquement.
  }
  destructor Destroy; override;

  {**
    Ajoute un élément à la fin de la liste.
    @param(Item Élément à ajouter)
    @returns(Index de l'élément ajouté)
  }
  function Add(Item: Pointer): Integer;
end;
```

**Documentation dans les fichiers .txt/.md :**

Chaque package devrait avoir un fichier README :

````markdown
# FCL-DB - Free Component Library Database

## Description
FCL-DB provides database connectivity components for FreePascal,  
similar to Delphi's DBX and ADO components.

## Supported Databases
- PostgreSQL
- MySQL/MariaDB
- SQLite
- Firebird/InterBase
- ODBC
- Oracle (via OCI)

## Basic Usage

```pascal
uses
  sqldb, pqconnection;

var
  Connection: TPQConnection;
  Query: TSQLQuery;
begin
  Connection := TPQConnection.Create(nil);
  Connection.DatabaseName := 'mydb';
  Connection.HostName := 'localhost';
  Connection.Open;

  Query := TSQLQuery.Create(nil);
  Query.Database := Connection;
  Query.SQL.Text := 'SELECT * FROM users';
  Query.Open;

  while not Query.EOF do
  begin
    WriteLn(Query.FieldByName('username').AsString);
    Query.Next;
  end;
end;
```

## Documentation
Full documentation: https://wiki.freepascal.org/fcl-db
````

### 4. Créer des tests

**Structure des tests FreePascal :**

```pascal
unit TestSysUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry;

type
  TTestSysUtils = class(TTestCase)
  private
    procedure DoFormatInvalid;
  published
    procedure TestFormatSimple;
    procedure TestFormatWithArgs;
    procedure TestFormatInvalid;
  end;

implementation

procedure TTestSysUtils.TestFormatSimple;  
begin
  AssertEquals('Hello World', Format('Hello %s', ['World']));
end;

procedure TTestSysUtils.TestFormatWithArgs;  
var
  S: string;
begin
  S := Format('%s has %d apples', ['John', 5]);
  AssertEquals('John has 5 apples', S);
end;

procedure TTestSysUtils.DoFormatInvalid;  
begin
  Format('%z', []); // Format spécifier invalide
end;

procedure TTestSysUtils.TestFormatInvalid;  
begin
  // Doit lever une exception
  AssertException(EConvertError, @DoFormatInvalid);
end;

initialization
  RegisterTest(TTestSysUtils);

end.
```

**Tests pour Lazarus (LCL) :**

```pascal
unit TestLCLButton;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls,
  fpcunit, testregistry;

type
  TTestButton = class(TTestCase)
  private
    FForm: TForm;
    FButton: TButton;
    FClickCount: Integer;
    procedure ButtonClickHandler(Sender: TObject);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestButtonCreate;
    procedure TestButtonClick;
    procedure TestButtonEnabled;
  end;

implementation

procedure TTestButton.SetUp;  
begin
  Application.CreateForm(TForm, FForm);
  FButton := TButton.Create(FForm);
  FButton.Parent := FForm;
  FClickCount := 0;
end;

procedure TTestButton.TearDown;  
begin
  FForm.Free;
end;

procedure TTestButton.ButtonClickHandler(Sender: TObject);  
begin
  Inc(FClickCount);
end;

procedure TTestButton.TestButtonClick;  
begin
  FButton.OnClick := @ButtonClickHandler;
  FButton.Click;
  AssertEquals('Click should increment counter', 1, FClickCount);
end;

procedure TTestButton.TestButtonEnabled;  
begin
  FButton.Enabled := False;
  AssertFalse('Button should be disabled', FButton.Enabled);

  FButton.Enabled := True;
  AssertTrue('Button should be enabled', FButton.Enabled);
end;

initialization
  RegisterTest(TTestButton);

end.
```

### 5. Ajouter des fonctionnalités

**Processus d'ajout de fonctionnalité :**

1. **Discuter d'abord** : Ouvrir une issue pour discuter de la fonctionnalité
2. **Obtenir l'accord** : Attendre validation des mainteneurs
3. **Designer l'API** : Proposer l'interface publique
4. **Implémenter** : Écrire le code avec tests
5. **Documenter** : Ajouter documentation complète
6. **Soumettre** : Créer la Merge Request

**Exemple : Ajouter une méthode à TStringList**

```pascal
// Dans rtl/objpas/classes.inc

type
  TStringList = class(TStrings)
  public
    // ... méthodes existantes ...

    {**
      Joins all strings into a single string with a separator.

      @param(Separator String to insert between elements)
      @returns(Joined string)

      Example:
      @longcode(#
      var
        List: TStringList;
      begin
        List := TStringList.Create;
        List.Add('apple');
        List.Add('banana');
        List.Add('cherry');
        WriteLn(List.Join(', ')); // apple, banana, cherry
      end;
      #)
    }
    function Join(const Separator: string): string;
  end;

// Implémentation
function TStringList.Join(const Separator: string): string;  
var
  i: Integer;
begin
  Result := '';
  if Count = 0 then
    Exit;

  Result := Strings[0];
  for i := 1 to Count - 1 do
    Result := Result + Separator + Strings[i];
end;

// Test associé (dans tests/)
procedure TTestStringList.TestJoin;  
var
  List: TStringList;
begin
  List := TStringList.Create;
  try
    List.Add('A');
    List.Add('B');
    List.Add('C');
    AssertEquals('A,B,C', List.Join(','));
    AssertEquals('A-B-C', List.Join('-'));
  finally
    List.Free;
  end;
end;
```

## Travailler sur le compilateur FPC

### Comprendre l'architecture du compilateur

Le compilateur FPC est lui-même écrit en Pascal et suit une architecture classique :

```
Code Source Pascal
      ↓
  [Scanner] ─→ Tokens
      ↓
  [Parser] ─→ Abstract Syntax Tree (AST)
      ↓
  [Type Checker] ─→ AST typé
      ↓
  [Code Generator] ─→ Code intermédiaire
      ↓
  [Optimizer] ─→ Code optimisé
      ↓
  [Backend] ─→ Code assembleur
      ↓
  [Assembler] ─→ Code objet
      ↓
  [Linker] ─→ Exécutable
```

**Fichiers importants :**

```
compiler/
├── scanner.pas       # Analyse lexicale (tokens)
├── pparser.pas       # Analyse syntaxique (parser)
├── psystem.pas       # Types système
├── symtable.pas      # Table des symboles
├── node.pas          # Nœuds de l'AST
├── ncgutil.pas       # Utilitaires génération de code
├── x86/              # Backend x86
│   ├── cgcpu.pas     # Générateur de code x86
│   └── ...
└── optimizer/        # Optimiseur
    ├── optbase.pas
    └── ...
```

### Déboguer le compilateur

**Compiler le compilateur en mode debug :**

```bash
cd compiler

# Compiler avec informations de débogage
make DEBUG=1

# Ou avec plus d'options
make OPT="-g -gl -godwarfsets -O-"
```

**Utiliser le compilateur en mode verbose :**

```bash
# Afficher toutes les étapes
./ppcx64 -va test.pas

# Afficher l'arbre syntaxique
./ppcx64 -vt test.pas

# Afficher les informations de génération de code
./ppcx64 -vn test.pas

# Tout afficher
./ppcx64 -vall test.pas > compiler.log 2>&1
```

**Déboguer avec GDB :**

```bash
# Lancer le compilateur sous GDB
gdb --args ./ppcx64 -g test.pas

# Dans GDB
(gdb) break pparser.pas:1234
(gdb) run
(gdb) print variable_name
(gdb) continue
```

### Ajouter un avertissement (warning)

Exemple : ajouter un warning pour variables non utilisées :

```pascal
// Dans compiler/symtable.pas

procedure TWarnUnusedVariables;  
var
  sym: TSymbol;
begin
  for sym in current_module.localsymtable do
  begin
    if (sym.typ = varsym) and not sym.used then
      MessagePos(sym.fileinfo,
                 parser_w_unused_variable,
                 sym.realname);
  end;
end;
```

### Ajouter une nouvelle fonctionnalité langage

**Exemple simplifié : Ajouter l'opérateur `**` pour puissance**

1. **Modifier le scanner** (scanner.pas) :
```pascal
// Ajouter le token
type
  TToken = (..., _POWER, ...);

// Dans le scanner
if c = '*' then  
begin
  c := current_scanner.readnextchar;
  if c = '*' then
    token := _POWER  // **
  else
  begin
    current_scanner.ungetchar(c);
    token := _STAR;  // *
  end;
end;
```

2. **Modifier le parser** (pparser.pas) :
```pascal
// Dans l'analyse des expressions
case token of
  _POWER:
    begin
      consume(_POWER);
      right := parse_factor;
      left := cpowernode.create(left, right);
    end;
end;
```

3. **Ajouter le nœud** (node.pas) :
```pascal
type
  TPowerNode = class(TBinaryNode)
    function pass_1: TNode; override;
    function pass_typecheck: TNode; override;
  end;

// Génération de code pour a**b
// Transformer en appel à Power(a, b)
```

C'est complexe ! Les vrais changements au langage nécessitent discussion approfondie avec l'équipe.

## Travailler sur l'IDE Lazarus

### Structure de l'IDE

**Points d'entrée importants :**

```
ide/
├── lazarus.lpr           # Programme principal
├── main.pas              # Fenêtre principale
├── sourceeditor.pas      # Éditeur de code
├── designer.pas          # Designer de formulaires
├── project.pas           # Gestion de projet
├── environmentopts.pas   # Options d'environnement
└── ...
```

### Ajouter un composant à la palette

Créer un nouveau composant dans la palette :

```pascal
// 1. Créer le composant
unit MyNewComponent;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, StdCtrls;

type
  TMyButton = class(TButton)
  private
    FSpecialProperty: string;
  published
    property SpecialProperty: string read FSpecialProperty write FSpecialProperty;
  end;

procedure Register;

implementation

procedure Register;  
begin
  RegisterComponents('MyTab', [TMyButton]);
end;

end.

// 2. Créer un package pour le composant
// File → New → Package
// Add unit MyNewComponent
// Compile and Install
```

### Ajouter un menu à l'IDE

```pascal
// Dans un package IDE
unit IDEMenuExtension;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MenuIntf, IDECommands, LazIDEIntf;

procedure Register;

implementation

procedure MyMenuItemClick(Sender: TObject);  
begin
  ShowMessage('Hello from my menu extension!');
end;

procedure Register;  
var
  Key: TIDEShortCut;
  Cmd: TIDECommand;
  MenuItem: TIDEMenuCommand;
begin
  // Créer une commande
  Key := IDEShortCut(VK_UNKNOWN, [], VK_UNKNOWN, []);
  Cmd := RegisterIDECommand(
    RegisterIDEMenuRoot('MyMenuRoot'),
    'MyCommand',
    'My Custom Action',
    Key,
    @MyMenuItemClick
  );

  // Ajouter au menu Tools
  MenuItem := RegisterIDEMenuCommand(itmTools, 'MyMenuItem', 'My Tool');
  MenuItem.OnClick := @MyMenuItemClick;
end;

end.
```

### Personnaliser l'éditeur SynEdit

SynEdit est l'éditeur de texte utilisé dans Lazarus :

```pascal
// Dans components/synedit/

// Ajouter un nouveau highlighter (coloration syntaxique)
type
  TSynCustomLang = class(TSynCustomHighlighter)
  private
    FKeywords: TStringList;
  protected
    procedure DoKeywordFound(const AKeyword: string);
  public
    constructor Create(AOwner: TComponent); override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
  end;
```

## Interagir avec la communauté

### Forums et listes de diffusion

**Ressources communautaires :**

1. **Forum officiel Lazarus** : https://forum.lazarus.freepascal.org/
   - Section "FPC development"
   - Section "Lazarus development"

2. **Listes de diffusion** :
   ```
   fpc-devel@lists.freepascal.org    # Développement FPC
   fpc-pascal@lists.freepascal.org   # Utilisation générale
   lazarus@lists.lazarus.freepascal.org  # Lazarus
   ```

   S'inscrire : https://lists.freepascal.org/

3. **IRC/Discord** :
   - IRC : #fpc sur irc.freenode.net
   - Discord : Communautés Pascal

4. **GitLab** :
   - Issues : https://gitlab.com/freepascal.org/fpc/source/-/issues
   - Merge Requests : https://gitlab.com/freepascal.org/fpc/source/-/merge_requests

### Bonnes pratiques de communication

**Sur les forums :**

```markdown
✅ BON :
"Hello,

I would like to contribute a fix for issue #12345
(memory leak in TStringList).

I have prepared a patch that:
- Fixes the memory leak
- Adds a unit test
- Updates the documentation

Could someone review my approach before I submit a MR?

Thanks!"

❌ MAUVAIS :
"FPC IS BUGGY!!! FIX TStringList NOW!!"
```

**Dans les Merge Requests :**

- Soyez patient : les mainteneurs sont des bénévoles
- Répondez rapidement aux commentaires
- Acceptez les critiques constructives
- N'ayez pas peur de demander de l'aide

### Participer aux discussions

**Types de discussions :**

1. **Proposals** : Nouvelles fonctionnalités du langage
   - Nécessitent discussion approfondie
   - Doivent être justifiées (cas d'usage)
   - Considérer la rétrocompatibilité

2. **Bugs** : Corrections de problèmes
   - Fournir cas de reproduction minimal
   - Proposer une solution si possible

3. **Documentation** : Améliorations
   - Clarifier les zones confuses
   - Ajouter des exemples

**Exemple de proposition :**

````markdown
# Proposal: Add inline variables in for loops

## Motivation
Currently, we must declare loop variables separately:

```pascal
var
  i: Integer;
begin
  for i := 0 to 10 do
    WriteLn(i);
end;
```

## Proposed Syntax
Allow inline declaration:

```pascal
begin
  for var i: Integer := 0 to 10 do
    WriteLn(i);
end;
```

## Benefits
- Cleaner code
- Reduced scope of loop variables
- Compatible with modern Pascal (Delphi 10.3+)

## Compatibility
- Optional feature
- Can be enabled with directive {$INLINE VARS ON}
- No impact on existing code

## Implementation Complexity
Medium - requires parser changes but no deep semantic changes
````

## Conseils pour contribuer efficacement

### Commencer petit

**Contributions faciles pour débuter :**

1. **Documentation** :
   - Corriger des fautes de frappe
   - Améliorer des explications confuses
   - Ajouter des exemples manquants

2. **Traductions** :
   - Traduire l'IDE dans votre langue
   - Traduire la documentation

3. **Exemples** :
   - Créer des exemples d'utilisation
   - Améliorer les exemples existants

4. **Tests** :
   - Ajouter des cas de test manquants
   - Améliorer la couverture des tests

5. **Petits bugs** :
   - Chercher issues avec label "good first issue"
   - Commencer par des corrections simples

### Gérer son temps

**Stratégies efficaces :**

- **Régularité** : Contribuer régulièrement (1-2h/semaine) plutôt que gros efforts sporadiques
- **Scope limité** : Prendre des tâches bien définies et limitées
- **Communication** : Prévenir si vous ne pouvez pas terminer une tâche
- **Patience** : Les révisions peuvent prendre du temps

### Devenir mainteneur

**Parcours typique :**

1. **Contributeur régulier** : Plusieurs contributions acceptées
2. **Expert domaine** : Spécialisation dans une zone (RTL, LCL, etc.)
3. **Reviewer** : Aider à réviser d'autres contributions
4. **Mainteneur** : Droits d'écriture sur le dépôt

**Responsabilités d'un mainteneur :**
- Réviser les Merge Requests
- Maintenir une zone du code
- Aider les nouveaux contributeurs
- Gérer les issues et bugs

## Outils et ressources

### Outils de développement

**IDEs et éditeurs :**
- Lazarus (évidemment !)
- VS Code avec extension Pascal
- Vim/Emacs avec support Pascal

**Outils d'analyse :**

```bash
# PasDoc - Générateur de documentation
pasdoc --format html \
       --output docs/ \
       --source rtl/objpas/*.pp

# fpcunit - Tests unitaires
./runtests

# Valgrind - Détection fuites mémoire (Linux)
valgrind --leak-check=full ./monprogramme

# gprof - Profiling
fpc -pg monprogramme.pas
./monprogramme
gprof monprogramme gmon.out > profile.txt
```

**Scripts utiles :**

```bash
# Trouver tous les TODOs dans le code
grep -r "TODO" compiler/ rtl/ lcl/

# Trouver fichiers modifiés récemment
find . -name "*.pas" -mtime -7

# Compter lignes de code
cloc compiler/ rtl/
```

### Documentation et ressources

**Documentation officielle :**
- Wiki FPC : https://wiki.freepascal.org/
- Wiki Lazarus : https://wiki.lazarus.freepascal.org/
- Documentation RTL : https://www.freepascal.org/docs-html/rtl/
- Documentation FCL : https://www.freepascal.org/docs-html/fcl/

**Guides du contributeur :**
- FPC Contributor Guide : https://wiki.freepascal.org/FPC_contributor_guide
- Lazarus Development : https://wiki.lazarus.freepascal.org/Development

**Blogs et tutoriels :**
- Planet Pascal : https://planet-pascal.io/
- Blogs de développeurs FreePascal

### Suivi des bugs et roadmap

**GitLab Issues :**

Filtres utiles :
```
# Bugs ouverts faciles
label:~"good first issue" state:opened

# Fonctionnalités demandées
label:~"enhancement" state:opened

# Bugs haute priorité
label:~"priority::high" state:opened

# Documentation
label:~"documentation" state:opened
```

**Roadmap :**
- FPC 3.4.0 : Nouvelles fonctionnalités langage
- Lazarus 3.2 : Améliorations IDE et LCL
- Voir : https://wiki.freepascal.org/Roadmap

## Résumé et checklist

### Checklist du contributeur

**Avant de contribuer :**
- [ ] Compte GitLab créé
- [ ] Git configuré localement
- [ ] Clé SSH ajoutée à GitLab
- [ ] Dépôt forké
- [ ] Sources compilées localement
- [ ] Lu les standards de code

**Pour chaque contribution :**
- [ ] Issue ouverte (ou référencée si existante)
- [ ] Branche créée depuis main
- [ ] Code écrit et testé
- [ ] Tests ajoutés/mis à jour
- [ ] Documentation mise à jour
- [ ] Commit avec message clair
- [ ] Push vers fork
- [ ] Merge Request créée
- [ ] Répondu aux commentaires de révision

**Après acceptation :**
- [ ] Branche locale supprimée
- [ ] Mis à jour depuis upstream
- [ ] Partagé l'information sur les forums

## Conclusion

Contribuer à FreePascal et Lazarus est une expérience enrichissante qui vous permet de :

- Améliorer vos compétences techniques
- Comprendre le fonctionnement interne d'un compilateur et d'un IDE
- Collaborer avec une communauté mondiale
- Avoir un impact réel sur des outils utilisés par des milliers de développeurs

**Commencez petit**, soyez patient, et n'hésitez pas à demander de l'aide. La communauté FreePascal/Lazarus est accueillante et toujours prête à aider les nouveaux contributeurs.

**N'oubliez pas** : chaque contribution compte, même la plus petite correction de documentation aide l'ensemble de l'écosystème !

### Prochaines étapes

1. **Créez votre compte GitLab** si ce n'est pas déjà fait
2. **Clonez les sources** de FreePascal ou Lazarus
3. **Compilez-les** pour vérifier que tout fonctionne
4. **Choisissez une première contribution** :
   - Corriger une faute de frappe dans la doc
   - Traduire quelques chaînes de l'IDE
   - Ajouter un test manquant
5. **Soumettez votre première MR** !

Bienvenue dans la communauté des contributeurs FreePascal/Lazarus ! 🎉

⏭️ [Création et maintenance de packages](/26-communaute-ecosysteme/02-creation-maintenance-packages.md)
