🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 18.10 Documentation automatique

## Introduction

La **documentation automatique** consiste à générer de la documentation (pages HTML, PDF, etc.) directement à partir du code source et de ses commentaires, sans avoir à écrire manuellement des documents séparés.

### Analogie simple

Imaginez que vous écrivez une recette de cuisine :
- **Sans documentation automatique** : Vous cuisinez, puis vous devez écrire la recette dans un cahier séparé
- **Avec documentation automatique** : Vous annotez vos actions pendant que vous cuisinez, et un système génère automatiquement le livre de recettes

### Pourquoi utiliser la documentation automatique ?

1. **Gain de temps** : Pas besoin d'écrire la documentation séparément
2. **Cohérence** : La documentation reste synchronisée avec le code
3. **Exhaustivité** : Couvre toutes les fonctions automatiquement
4. **Maintenance facilitée** : Mise à jour automatique lors des modifications
5. **Format professionnel** : Génère des documents HTML/PDF de qualité
6. **Navigation facile** : Liens hypertextes entre les sections

## Format des commentaires de documentation

### Commentaires standard Pascal

FreePascal supporte plusieurs formats de commentaires pour la documentation.

#### 1. Commentaires avec accolades

```pascal
{
  Cette fonction calcule la somme de deux nombres.
  @param a Le premier nombre
  @param b Le second nombre
  @return La somme de a et b
}
function Additionner(a, b: Integer): Integer;
begin
  Result := a + b;
end;
```

#### 2. Commentaires avec parenthèses-étoile

```pascal
(*
  Classe représentant un point dans un espace 2D.

  Cette classe encapsule les coordonnées X et Y d'un point
  et fournit des méthodes pour manipuler ces coordonnées.
*)
type
  TPoint2D = class
  private
    FX, FY: Double;
  public
    {* Constructeur du point *}
    constructor Create(AX, AY: Double);

    {* Propriété X - Coordonnée horizontale *}
    property X: Double read FX write FX;

    {* Propriété Y - Coordonnée verticale *}
    property Y: Double read FY write FY;
  end;
```

#### 3. Commentaires doubles-slash (Delphi style)

```pascal
/// <summary>
/// Vérifie si une chaîne est un email valide
/// </summary>
/// <param name="email">L'adresse email à vérifier</param>
/// <returns>True si l'email est valide, False sinon</returns>
function EstEmailValide(const email: string): Boolean;
begin
  Result := Pos('@', email) > 0;
end;
```

### Tags de documentation courants

**Tags généraux :**
- `@param` ou `@parameter` : Décrit un paramètre
- `@return` ou `@returns` : Décrit la valeur de retour
- `@see` : Référence à d'autres éléments
- `@author` : Auteur du code
- `@version` : Version du code
- `@since` : Depuis quelle version
- `@deprecated` : Élément obsolète
- `@example` : Exemple d'utilisation

**Tags spéciaux :**
- `@raises` ou `@exception` : Exceptions levées
- `@note` : Note importante
- `@warning` : Avertissement
- `@todo` : Tâche à faire

### Exemple complet de documentation

```pascal
unit MathUtils;

{$mode objfpc}{$H+}

interface

{
  @abstract(Unité contenant des utilitaires mathématiques)

  Cette unité fournit des fonctions et procédures pour effectuer
  des opérations mathématiques courantes.

  @author Jean Dupont
  @version 1.2.0
  @since 2024-01-15
}

type
  {
    @abstract(Exception levée lors d'erreurs mathématiques)

    Cette exception est levée quand une opération mathématique
    invalide est tentée, comme une division par zéro.
  }
  EMathError = class(Exception);

  {
    Classe utilitaire pour opérations mathématiques avancées.

    @note Cette classe ne doit pas être instanciée directement.
          Utilisez les méthodes de classe.
  }
  TMathHelper = class
  public
    {
      Calcule la factorielle d'un nombre.

      @param n Le nombre dont on veut la factorielle (doit être >= 0)
      @return La factorielle de n
      @raises EMathError si n est négatif

      @example
      <code>
      var
        result: Int64;
      begin
        result := TMathHelper.Factorielle(5); // Retourne 120
      end;
      </code>
    }
    class function Factorielle(n: Integer): Int64;

    {
      Vérifie si un nombre est premier.

      Un nombre premier est un entier naturel supérieur à 1
      qui n'a que deux diviseurs : 1 et lui-même.

      @param n Le nombre à tester
      @return True si n est premier, False sinon
      @see EstPairOuImpair
    }
    class function EstPremier(n: Integer): Boolean;
  end;

implementation

class function TMathHelper.Factorielle(n: Integer): Int64;
var
  i: Integer;
begin
  if n < 0 then
    raise EMathError.Create('La factorielle n''est pas définie pour les nombres négatifs');

  Result := 1;
  for i := 2 to n do
    Result := Result * i;
end;

class function TMathHelper.EstPremier(n: Integer): Boolean;
var
  i: Integer;
begin
  if n <= 1 then
    Exit(False);
  if n = 2 then
    Exit(True);
  if n mod 2 = 0 then
    Exit(False);

  i := 3;
  while i * i <= n do
  begin
    if n mod i = 0 then
      Exit(False);
    Inc(i, 2);
  end;

  Result := True;
end;

end.
```

## PasDoc - Générateur de documentation principal

**PasDoc** est l'outil principal pour générer la documentation de code FreePascal/Lazarus.

### Installation de PasDoc

#### Sur Ubuntu

```bash
# Installation via apt
sudo apt-get update
sudo apt-get install pasdoc

# Vérifier l'installation
pasdoc --version
```

#### Sur Windows

**Méthode 1 : Téléchargement direct**

1. Aller sur https://github.com/pasdoc/pasdoc/releases
2. Télécharger `pasdoc-windows-x86_64.zip`
3. Extraire dans `C:\pasdoc`
4. Ajouter `C:\pasdoc` au PATH système

**Méthode 2 : Compilation depuis les sources**

```bash
# Prérequis : Lazarus installé
cd C:\temp
git clone https://github.com/pasdoc/pasdoc.git
cd pasdoc\source
lazbuild pasdoc.lpi
```

### Utilisation basique de PasDoc

#### Commande simple

```bash
# Générer la documentation HTML
pasdoc --format=html --output=docs *.pas

# Générer la documentation en LaTeX
pasdoc --format=latex --output=docs *.pas

# Avec titre personnalisé
pasdoc --format=html --title="Ma Bibliothèque" --output=docs *.pas
```

#### Options utiles

```bash
# Spécifier plusieurs répertoires sources
pasdoc --format=html --output=docs \
  --source=src/*.pas \
  --source=lib/*.pas

# Inclure les unités privées
pasdoc --format=html --output=docs \
  --visible-members=private,protected,public *.pas

# Exclure certaines unités
pasdoc --format=html --output=docs \
  --exclude=*test*.pas *.pas

# Utiliser un fichier de configuration
pasdoc @config.txt
```

### Fichier de configuration PasDoc

**`pasdoc.cfg` :**

```ini
# Configuration PasDoc

# Format de sortie
--format=html

# Répertoire de sortie
--output=docs

# Titre du projet
--title=Mon Projet FreePascal

# Description
--introduction=intro.txt

# Sources
--source=src/*.pas
--source=lib/*.pas

# Exclure les tests
--exclude=*test*.pas

# Visibilité
--visible-members=public,published

# Options HTML
--css=custom.css
--header=header.html
--footer=footer.html

# Langue
--language=fr_FR

# Graphiques de hiérarchie de classes
--graphviz-uses
--graphviz-classes

# Fichiers additionnels
--additional-file=README.md
--additional-file=LICENSE.txt

# Verbosité
--verbosity=2
```

**Utilisation :**

```bash
pasdoc @pasdoc.cfg
```

### Personnalisation du style HTML

**`custom.css` :**

```css
/* Style personnalisé pour la documentation PasDoc */

body {
    font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
    line-height: 1.6;
    color: #333;
    background-color: #f8f9fa;
    margin: 0;
    padding: 0;
}

.container {
    max-width: 1200px;
    margin: 0 auto;
    padding: 20px;
    background-color: white;
    box-shadow: 0 0 10px rgba(0,0,0,0.1);
}

h1, h2, h3 {
    color: #2c3e50;
    border-bottom: 2px solid #3498db;
    padding-bottom: 10px;
}

code {
    background-color: #f4f4f4;
    padding: 2px 6px;
    border-radius: 3px;
    font-family: 'Courier New', monospace;
}

pre {
    background-color: #2d2d2d;
    color: #f8f8f2;
    padding: 15px;
    border-radius: 5px;
    overflow-x: auto;
}

.function-description {
    background-color: #e8f4f8;
    padding: 15px;
    border-left: 4px solid #3498db;
    margin: 10px 0;
}

.parameter {
    font-weight: bold;
    color: #e74c3c;
}

.return-value {
    font-weight: bold;
    color: #27ae60;
}

.deprecated {
    background-color: #fff3cd;
    border-left: 4px solid #ffc107;
    padding: 10px;
    margin: 10px 0;
}

.note {
    background-color: #d1ecf1;
    border-left: 4px solid #17a2b8;
    padding: 10px;
    margin: 10px 0;
}

.warning {
    background-color: #f8d7da;
    border-left: 4px solid #dc3545;
    padding: 10px;
    margin: 10px 0;
}
```

### En-tête et pied de page personnalisés

**`header.html` :**

```html
<!DOCTYPE html>
<html lang="fr">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>@title - Documentation</title>
    <link rel="stylesheet" href="custom.css">
</head>
<body>
    <header>
        <div class="header-container">
            <img src="logo.png" alt="Logo" class="logo">
            <h1>@title</h1>
            <nav>
                <a href="index.html">Accueil</a>
                <a href="classes.html">Classes</a>
                <a href="functions.html">Fonctions</a>
                <a href="units.html">Unités</a>
            </nav>
        </div>
    </header>
    <div class="container">
```

**`footer.html` :**

```html
    </div>
    <footer>
        <div class="footer-container">
            <p>&copy; 2025 Mon Projet. Tous droits réservés.</p>
            <p>Documentation générée avec <a href="https://pasdoc.github.io/">PasDoc</a></p>
            <p>Version: @version | Date: @date</p>
        </div>
    </footer>
</body>
</html>
```

## Génération de diagrammes

### Diagrammes de classes avec Graphviz

PasDoc peut générer des diagrammes de classes si Graphviz est installé.

#### Installation de Graphviz

**Ubuntu :**
```bash
sudo apt-get install graphviz
```

**Windows :**
1. Télécharger depuis https://graphviz.org/download/
2. Installer et ajouter au PATH

#### Génération des diagrammes

```bash
# Diagramme de hiérarchie des classes
pasdoc --format=html \
       --output=docs \
       --graphviz-classes \
       --graphviz-uses \
       --link-gv-classes=svg \
       --link-gv-uses=svg \
       *.pas
```

**Résultat :** PasDoc génère des fichiers SVG montrant :
- La hiérarchie des classes (héritage)
- Les dépendances entre unités (uses)

## Scripts d'automatisation

### Script bash pour Linux

**`generate_docs.sh` :**

```bash
#!/bin/bash
# Script de génération de documentation

PROJECT_NAME="Mon Projet FreePascal"
SOURCE_DIR="src"
OUTPUT_DIR="docs"
VERSION=$(cat VERSION.txt)

echo "=== Génération de la documentation ==="
echo "Projet: $PROJECT_NAME"
echo "Version: $VERSION"
echo

# Nettoyer l'ancien répertoire de documentation
if [ -d "$OUTPUT_DIR" ]; then
    echo "Nettoyage de l'ancienne documentation..."
    rm -rf "$OUTPUT_DIR"
fi

# Créer le répertoire de sortie
mkdir -p "$OUTPUT_DIR"

# Générer la documentation
echo "Génération de la documentation HTML..."
pasdoc --format=html \
       --output="$OUTPUT_DIR" \
       --title="$PROJECT_NAME" \
       --source="$SOURCE_DIR/*.pas" \
       --exclude="*test*.pas" \
       --visible-members=public,published \
       --css=custom.css \
       --header=header.html \
       --footer=footer.html \
       --graphviz-classes \
       --graphviz-uses \
       --language=fr_FR \
       --verbosity=2

if [ $? -eq 0 ]; then
    echo
    echo "✓ Documentation générée avec succès dans $OUTPUT_DIR"
    echo

    # Ouvrir dans le navigateur (optionnel)
    if command -v xdg-open &> /dev/null; then
        read -p "Ouvrir la documentation dans le navigateur ? (o/n) " -n 1 -r
        echo
        if [[ $REPLY =~ ^[Oo]$ ]]; then
            xdg-open "$OUTPUT_DIR/index.html"
        fi
    fi
else
    echo
    echo "❌ Erreur lors de la génération de la documentation"
    exit 1
fi
```

**Utilisation :**

```bash
chmod +x generate_docs.sh
./generate_docs.sh
```

### Script PowerShell pour Windows

**`generate_docs.ps1` :**

```powershell
# Script de génération de documentation

$ProjectName = "Mon Projet FreePascal"
$SourceDir = "src"
$OutputDir = "docs"
$Version = Get-Content "VERSION.txt"

Write-Host "=== Génération de la documentation ===" -ForegroundColor Green
Write-Host "Projet: $ProjectName"
Write-Host "Version: $Version"
Write-Host ""

# Nettoyer l'ancien répertoire
if (Test-Path $OutputDir) {
    Write-Host "Nettoyage de l'ancienne documentation..." -ForegroundColor Yellow
    Remove-Item -Recurse -Force $OutputDir
}

# Créer le répertoire de sortie
New-Item -ItemType Directory -Force -Path $OutputDir | Out-Null

# Générer la documentation
Write-Host "Génération de la documentation HTML..." -ForegroundColor Yellow
& pasdoc --format=html `
         --output=$OutputDir `
         --title=$ProjectName `
         --source="$SourceDir\*.pas" `
         --exclude="*test*.pas" `
         --visible-members=public,published `
         --css=custom.css `
         --header=header.html `
         --footer=footer.html `
         --graphviz-classes `
         --graphviz-uses `
         --language=fr_FR `
         --verbosity=2

if ($LASTEXITCODE -eq 0) {
    Write-Host ""
    Write-Host "✓ Documentation générée avec succès dans $OutputDir" -ForegroundColor Green
    Write-Host ""

    # Ouvrir dans le navigateur
    $response = Read-Host "Ouvrir la documentation dans le navigateur ? (o/n)"
    if ($response -eq 'o') {
        Start-Process "$OutputDir\index.html"
    }
}
else {
    Write-Host ""
    Write-Host "❌ Erreur lors de la génération de la documentation" -ForegroundColor Red
    exit 1
}
```

**Utilisation :**

```powershell
.\generate_docs.ps1
```

## Intégration avec CI/CD

### GitLab CI

**`.gitlab-ci.yml` :**

```yaml
stages:
  - build
  - test
  - documentation
  - deploy

documentation:
  stage: documentation
  image: ubuntu:latest
  before_script:
    - apt-get update
    - apt-get install -y pasdoc graphviz
  script:
    - chmod +x generate_docs.sh
    - ./generate_docs.sh
  artifacts:
    paths:
      - docs/
    expire_in: 1 week
  only:
    - main
    - tags

pages:
  stage: deploy
  dependencies:
    - documentation
  script:
    - mkdir -p public
    - cp -r docs/* public/
  artifacts:
    paths:
      - public
  only:
    - main
```

### GitHub Actions

**`.github/workflows/documentation.yml` :**

```yaml
name: Generate Documentation

on:
  push:
    branches: [main]
  pull_request:
    branches: [main]

jobs:
  generate-docs:
    runs-on: ubuntu-latest

    steps:
    - uses: actions/checkout@v3

    - name: Install PasDoc and Graphviz
      run: |
        sudo apt-get update
        sudo apt-get install -y pasdoc graphviz

    - name: Generate documentation
      run: |
        chmod +x generate_docs.sh
        ./generate_docs.sh

    - name: Upload documentation
      uses: actions/upload-artifact@v3
      with:
        name: documentation
        path: docs/

    - name: Deploy to GitHub Pages
      if: github.ref == 'refs/heads/main'
      uses: peaceiris/actions-gh-pages@v3
      with:
        github_token: ${{ secrets.GITHUB_TOKEN }}
        publish_dir: ./docs
```

## Documentation XML (format Delphi)

### Format XML Documentation

FreePascal supporte également le format XML de documentation de Delphi.

```pascal
unit MyUnit;

interface

type
  /// <summary>
  /// Classe représentant un rectangle
  /// </summary>
  /// <remarks>
  /// Cette classe fournit des méthodes pour manipuler des rectangles
  /// dans un espace 2D.
  /// </remarks>
  TRectangle = class
  private
    FWidth, FHeight: Double;
  public
    /// <summary>
    /// Crée un nouveau rectangle
    /// </summary>
    /// <param name="AWidth">Largeur du rectangle</param>
    /// <param name="AHeight">Hauteur du rectangle</param>
    constructor Create(AWidth, AHeight: Double);

    /// <summary>
    /// Calcule l'aire du rectangle
    /// </summary>
    /// <returns>L'aire en unités carrées</returns>
    function CalculerAire: Double;

    /// <summary>
    /// Calcule le périmètre du rectangle
    /// </summary>
    /// <returns>Le périmètre en unités linéaires</returns>
    function CalculerPerimetre: Double;

    /// <summary>
    /// Largeur du rectangle
    /// </summary>
    property Width: Double read FWidth write FWidth;

    /// <summary>
    /// Hauteur du rectangle
    /// </summary>
    property Height: Double read FHeight write FHeight;
  end;

implementation

constructor TRectangle.Create(AWidth, AHeight: Double);
begin
  inherited Create;
  FWidth := AWidth;
  FHeight := AHeight;
end;

function TRectangle.CalculerAire: Double;
begin
  Result := FWidth * FHeight;
end;

function TRectangle.CalculerPerimetre: Double;
begin
  Result := 2 * (FWidth + FHeight);
end;

end.
```

## Génération de documentation API REST

### Documenter une API web

Pour documenter une API REST créée avec FreePascal, vous pouvez utiliser une approche hybride.

**`APIDocumentation.pas` :**

```pascal
unit APIDocumentation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser;

type
  {
    @abstract(Classe pour générer la documentation API au format OpenAPI/Swagger)

    Cette classe permet de créer automatiquement une spécification
    OpenAPI 3.0 pour documenter votre API REST.
  }
  TAPIDocGenerator = class
  private
    FSpec: TJSONObject;
  public
    constructor Create(const Title, Version, Description: string);
    destructor Destroy; override;

    {
      Ajoute un endpoint à la documentation

      @param Path Le chemin de l'endpoint (ex: /api/users)
      @param Method La méthode HTTP (GET, POST, etc.)
      @param Summary Description courte
      @param Description Description détaillée
    }
    procedure AddEndpoint(const Path, Method, Summary, Description: string);

    {
      Génère le fichier JSON de spécification OpenAPI

      @param FileName Nom du fichier de sortie
    }
    procedure SaveToFile(const FileName: string);
  end;

implementation

constructor TAPIDocGenerator.Create(const Title, Version, Description: string);
var
  info: TJSONObject;
begin
  inherited Create;
  FSpec := TJSONObject.Create;

  FSpec.Add('openapi', '3.0.0');

  info := TJSONObject.Create;
  info.Add('title', Title);
  info.Add('version', Version);
  info.Add('description', Description);

  FSpec.Add('info', info);
  FSpec.Add('paths', TJSONObject.Create);
end;

destructor TAPIDocGenerator.Destroy;
begin
  FSpec.Free;
  inherited Destroy;
end;

procedure TAPIDocGenerator.AddEndpoint(const Path, Method, Summary, Description: string);
var
  paths, pathItem, methodItem: TJSONObject;
begin
  paths := FSpec.Objects['paths'];

  if not paths.Find(Path, pathItem) then
  begin
    pathItem := TJSONObject.Create;
    paths.Add(Path, pathItem);
  end;

  methodItem := TJSONObject.Create;
  methodItem.Add('summary', Summary);
  methodItem.Add('description', Description);

  pathItem.Add(LowerCase(Method), methodItem);
end;

procedure TAPIDocGenerator.SaveToFile(const FileName: string);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.Text := FSpec.FormatJSON;
    sl.SaveToFile(FileName);
  finally
    sl.Free;
  end;
end;

end.
```

**Utilisation :**

```pascal
program GenerateAPIDoc;

uses
  APIDocumentation;

var
  doc: TAPIDocGenerator;

begin
  doc := TAPIDocGenerator.Create(
    'Mon API',
    '1.0.0',
    'API REST pour la gestion des utilisateurs'
  );

  try
    doc.AddEndpoint('/api/users', 'GET',
      'Liste les utilisateurs',
      'Retourne la liste complète des utilisateurs enregistrés');

    doc.AddEndpoint('/api/users/{id}', 'GET',
      'Récupère un utilisateur',
      'Retourne les détails d''un utilisateur spécifique');

    doc.AddEndpoint('/api/users', 'POST',
      'Crée un utilisateur',
      'Crée un nouvel utilisateur dans le système');

    doc.SaveToFile('api-spec.json');
    WriteLn('Documentation API générée: api-spec.json');

  finally
    doc.Free;
  end;
end.
```

## Bonnes pratiques de documentation

### 1. Commentaires clairs et concis

**❌ Mauvais :**
```pascal
// Cette fonction fait des trucs
function Calcul(x: Integer): Integer;
```

**✓ Bon :**
```pascal
{
  Calcule le carré d'un nombre entier.

  @param x Le nombre à élever au carré
  @return Le carré de x (x * x)
}
function Carre(x: Integer): Integer;
```

### 2. Documenter l'intention, pas l'évidence

**❌ Mauvais :**
```pascal
{ Incrémente i de 1 }
Inc(i);
```

**✓ Bon :**
```pascal
{ Passe à l'utilisateur suivant dans la liste }
Inc(i);
```

### 3. Inclure des exemples

```pascal
{
  Formatte un nombre avec des séparateurs de milliers.

  @param value Le nombre à formater
  @return La chaîne formatée

  @example
  <code>
  WriteLn(FormatterNombre(1234567)); // Affiche "1 234 567"
  WriteLn(FormatterNombre(42));       // Affiche "42"
  </code>
}
function FormatterNombre(value: Int64): string;
```

### 4. Documenter les cas limites

```pascal
{
  Divise deux nombres entiers.

  @param a Le dividende
  @param b Le diviseur
  @return Le quotient de a divisé par b
  @raises EDivByZero Si b est égal à zéro

  @note Cette fonction effectue une division entière.
        Le reste est ignoré.
}
function Diviser(a, b: Integer): Integer;
```

### 5. Maintenir la documentation à jour

```pascal
{
  Charge un fichier de configuration.

  @param FileName Chemin du fichier à charger
  @return True si le chargement a réussi

  @deprecated Utilisez LoadConfigFromJSON à la place.
              Cette méthode sera supprimée dans la version 3.0.

  @see LoadConfigFromJSON
}
function LoadConfig(const FileName: string): Boolean; deprecated;
```

## Structure d'une bonne documentation

### Page d'introduction

**`intro.txt` :**

````markdown
# Mon Projet FreePascal

## Description

Ce projet fournit une bibliothèque complète pour la manipulation
de données géométriques en 2D et 3D.

## Installation

```bash
git clone https://github.com/user/mon-projet.git
cd mon-projet
lazbuild mon-projet.lpi
```

## Démarrage rapide

```pascal
uses
  GeometryLib;

var
  point: TPoint2D;
  distance: Double;
begin
  point := TPoint2D.Create(10, 20);
  distance := point.DistanceToOrigin;
  WriteLn('Distance: ', distance:0:2);
end.
```

## Fonctionnalités principales

- Gestion de points 2D et 3D
- Calculs de distances et d'angles
- Transformations géométriques
- Support des polygones et courbes

## Licence

Ce projet est sous licence MIT. Voir LICENSE.txt pour plus de détails.
````

### Index des modules

Organiser la documentation par thèmes :

```
docs/
├── index.html              # Page d'accueil
├── units/                  # Documentation des unités
│   ├── GeometryLib.html
│   ├── MathUtils.html
│   └── FileUtils.html
├── classes/                # Documentation des classes
│   ├── TPoint2D.html
│   ├── TRectangle.html
│   └── TCircle.html
├── functions/              # Index des fonctions
│   └── index.html
├── tutorials/              # Tutoriels
│   ├── getting-started.html
│   └── advanced-usage.html
└── api/                    # Référence API complète
    └── index.html
```

## Outils complémentaires

### 1. Doxygen

Bien que principalement pour C/C++, Doxygen peut parser du Pascal.

**Installation :**
```bash
# Ubuntu
sudo apt-get install doxygen graphviz

# Windows
# Télécharger depuis doxygen.org
```

**Configuration (`Doxyfile`) :**
```
PROJECT_NAME = "Mon Projet"
OUTPUT_DIRECTORY = docs
INPUT = src
FILE_PATTERNS = *.pas *.pp
RECURSIVE = YES
GENERATE_HTML = YES
GENERATE_LATEX = NO
OPTIMIZE_OUTPUT_FOR_C = NO
EXTRACT_ALL = YES
```

**Génération :**
```bash
doxygen Doxyfile
```

### 2. Sphinx avec domain Pascal

Pour une documentation plus élaborée (comme celle de Python).

```bash
# Installation
pip install sphinx

# Créer un projet Sphinx
sphinx-quickstart

# Structure du projet
docs/
├── source/
│   ├── conf.py
│   ├── index.rst
│   └── modules/
│       ├── geometry.rst
│       └── utils.rst
└── build/
```

**Configuration (`conf.py`) :**

```python
# Configuration Sphinx pour FreePascal

project = 'Mon Projet FreePascal'
copyright = '2025, Mon Nom'
author = 'Mon Nom'
version = '1.0'
release = '1.0.0'

# Extensions
extensions = [
    'sphinx.ext.autodoc',
    'sphinx.ext.napoleon',
    'sphinx.ext.viewcode',
]

# Thème
html_theme = 'sphinx_rtd_theme'

# Langue
language = 'fr'
```

**Fichier reStructuredText (`index.rst`) :**

```rst
Documentation Mon Projet
=========================

Bienvenue dans la documentation de Mon Projet FreePascal.

.. toctree::
   :maxdepth: 2
   :caption: Contenu:

   introduction
   installation
   modules/geometry
   modules/utils
   api/index
   exemples

Introduction
============

Mon Projet est une bibliothèque FreePascal pour...

Installation
============

Ubuntu
------

.. code-block:: bash

   sudo apt-get install fpc lazarus
   git clone https://github.com/user/mon-projet.git

Windows
-------

1. Télécharger Lazarus depuis https://www.lazarus-ide.org/
2. Cloner le dépôt
3. Ouvrir le projet dans Lazarus

Démarrage Rapide
================

Voici un exemple simple :

.. code-block:: pascal

   program HelloWorld;
   uses
     GeometryLib;

   var
     point: TPoint2D;
   begin
     point := TPoint2D.Create(10, 20);
     WriteLn('X: ', point.X, ' Y: ', point.Y);
   end.

API Reference
=============

.. toctree::
   :maxdepth: 1

   api/classes
   api/functions
   api/types
```

**Génération :**

```bash
# Construire la documentation
cd docs
make html

# Ouvrir dans le navigateur
# Linux
xdg-open build/html/index.html

# Windows
start build\html\index.html
```

### 3. Natural Docs

Alternative multi-langage simple d'utilisation.

**Installation Windows :**

1. Télécharger depuis https://www.naturaldocs.org/
2. Extraire dans `C:\NaturalDocs`
3. Ajouter au PATH

**Installation Ubuntu :**

```bash
# Télécharger la dernière version
wget https://www.naturaldocs.org/download/natural_docs_2.2.zip
unzip natural_docs_2.2.zip
sudo mv NaturalDocs /opt/

# Créer un lien symbolique
sudo ln -s /opt/NaturalDocs/NaturalDocs /usr/local/bin/naturaldocs
```

**Configuration (`Natural Docs.cfg`) :**

```
Format: 2.2

# Sources
Source Folder: src
   Name: Sources

# Sortie
Output Folder: docs
   Name: Documentation

# Exclusions
Ignore Source Folder: test
Ignore Source Folder: temp
```

**Génération :**

```bash
naturaldocs --project=. --input=src --output=HTML:docs
```

## Documentation embarquée dans l'application

### Générer un fichier d'aide

**Créer un système d'aide intégré :**

```pascal
unit HelpSystem;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser;

type
  THelpTopic = record
    Title: string;
    Content: string;
    Keywords: array of string;
  end;

  THelpSystem = class
  private
    FTopics: array of THelpTopic;
  public
    constructor Create;
    destructor Destroy; override;

    {
      Charge les sujets d'aide depuis un fichier JSON
      @param FileName Chemin du fichier d'aide
    }
    procedure LoadFromFile(const FileName: string);

    {
      Recherche un sujet d'aide
      @param Query Terme à rechercher
      @return Index du sujet trouvé, -1 si non trouvé
    }
    function Search(const Query: string): Integer;

    {
      Affiche un sujet d'aide
      @param TopicIndex Index du sujet à afficher
    }
    procedure ShowTopic(TopicIndex: Integer);

    {
      Génère un fichier d'aide HTML
      @param OutputFile Chemin du fichier HTML de sortie
    }
    procedure GenerateHTML(const OutputFile: string);
  end;

implementation

constructor THelpSystem.Create;
begin
  inherited Create;
  SetLength(FTopics, 0);
end;

destructor THelpSystem.Destroy;
begin
  SetLength(FTopics, 0);
  inherited Destroy;
end;

procedure THelpSystem.LoadFromFile(const FileName: string);
var
  json: TJSONData;
  topics: TJSONArray;
  topic: TJSONObject;
  i, j: Integer;
  keywords: TJSONArray;
begin
  json := GetJSON(TFileStream.Create(FileName, fmOpenRead));
  try
    if json is TJSONObject then
    begin
      topics := TJSONObject(json).Arrays['topics'];
      SetLength(FTopics, topics.Count);

      for i := 0 to topics.Count - 1 do
      begin
        topic := topics.Objects[i];
        FTopics[i].Title := topic.Get('title', '');
        FTopics[i].Content := topic.Get('content', '');

        if topic.Find('keywords', keywords) then
        begin
          SetLength(FTopics[i].Keywords, keywords.Count);
          for j := 0 to keywords.Count - 1 do
            FTopics[i].Keywords[j] := keywords.Strings[j];
        end;
      end;
    end;
  finally
    json.Free;
  end;
end;

function THelpSystem.Search(const Query: string): Integer;
var
  i, j: Integer;
  lowerQuery: string;
begin
  Result := -1;
  lowerQuery := LowerCase(Query);

  for i := 0 to High(FTopics) do
  begin
    // Recherche dans le titre
    if Pos(lowerQuery, LowerCase(FTopics[i].Title)) > 0 then
      Exit(i);

    // Recherche dans les mots-clés
    for j := 0 to High(FTopics[i].Keywords) do
      if Pos(lowerQuery, LowerCase(FTopics[i].Keywords[j])) > 0 then
        Exit(i);
  end;
end;

procedure THelpSystem.ShowTopic(TopicIndex: Integer);
begin
  if (TopicIndex >= 0) and (TopicIndex <= High(FTopics)) then
  begin
    WriteLn('=== ', FTopics[TopicIndex].Title, ' ===');
    WriteLn;
    WriteLn(FTopics[TopicIndex].Content);
    WriteLn;
  end
  else
    WriteLn('Sujet non trouvé.');
end;

procedure THelpSystem.GenerateHTML(const OutputFile: string);
var
  html: TStringList;
  i: Integer;
begin
  html := TStringList.Create;
  try
    html.Add('<!DOCTYPE html>');
    html.Add('<html lang="fr">');
    html.Add('<head>');
    html.Add('  <meta charset="UTF-8">');
    html.Add('  <title>Aide de l''application</title>');
    html.Add('  <style>');
    html.Add('    body { font-family: Arial, sans-serif; max-width: 800px; margin: 0 auto; padding: 20px; }');
    html.Add('    .topic { margin-bottom: 30px; padding: 15px; border: 1px solid #ddd; border-radius: 5px; }');
    html.Add('    .topic h2 { color: #333; margin-top: 0; }');
    html.Add('    .content { line-height: 1.6; }');
    html.Add('  </style>');
    html.Add('</head>');
    html.Add('<body>');
    html.Add('  <h1>Documentation de l''application</h1>');

    for i := 0 to High(FTopics) do
    begin
      html.Add('  <div class="topic">');
      html.Add('    <h2>' + FTopics[i].Title + '</h2>');
      html.Add('    <div class="content">');
      html.Add('      <p>' + StringReplace(FTopics[i].Content, #10, '</p><p>', [rfReplaceAll]) + '</p>');
      html.Add('    </div>');
      html.Add('  </div>');
    end;

    html.Add('</body>');
    html.Add('</html>');

    html.SaveToFile(OutputFile);
  finally
    html.Free;
  end;
end;

end.
```

**Fichier d'aide JSON (`help.json`) :**

```json
{
  "topics": [
    {
      "title": "Démarrage",
      "content": "Pour démarrer l'application, cliquez sur le bouton 'Nouveau' dans le menu Fichier.",
      "keywords": ["démarrage", "nouveau", "commencer"]
    },
    {
      "title": "Enregistrer un fichier",
      "content": "Pour enregistrer votre travail, utilisez Fichier > Enregistrer ou appuyez sur Ctrl+S.",
      "keywords": ["enregistrer", "sauvegarder", "save"]
    },
    {
      "title": "Raccourcis clavier",
      "content": "Ctrl+N: Nouveau\nCtrl+O: Ouvrir\nCtrl+S: Enregistrer\nCtrl+Q: Quitter",
      "keywords": ["raccourcis", "touches", "clavier"]
    }
  ]
}
```

**Programme de démonstration :**

```pascal
program HelpDemo;

uses
  HelpSystem;

var
  help: THelpSystem;
  query: string;
  topicIndex: Integer;

begin
  help := THelpSystem.Create;
  try
    // Charger l'aide
    help.LoadFromFile('help.json');

    // Générer la version HTML
    help.GenerateHTML('help.html');
    WriteLn('Fichier d''aide HTML généré: help.html');
    WriteLn;

    // Exemple d'utilisation interactive
    WriteLn('=== Système d''aide ===');
    WriteLn('Entrez un terme de recherche (ou "quit" pour quitter):');
    WriteLn;

    repeat
      Write('Recherche: ');
      ReadLn(query);

      if LowerCase(query) = 'quit' then
        Break;

      topicIndex := help.Search(query);
      if topicIndex >= 0 then
        help.ShowTopic(topicIndex)
      else
        WriteLn('Aucun sujet trouvé pour "', query, '"');

      WriteLn;
    until False;

  finally
    help.Free;
  end;
end.
```

## Documentation pour utilisateurs finaux

### Manuel utilisateur automatisé

**Créer un générateur de manuel utilisateur :**

```pascal
unit UserManualGenerator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TManualSection = record
    Title: string;
    Level: Integer;  // 1=Chapitre, 2=Section, 3=Sous-section
    Content: string;
    ScreenshotPath: string;
  end;

  TUserManualGenerator = class
  private
    FSections: array of TManualSection;
    FTitle: string;
    FVersion: string;
  public
    constructor Create(const ATitle, AVersion: string);

    procedure AddSection(const Title, Content: string; Level: Integer = 1);
    procedure AddScreenshot(const Path: string);

    procedure GenerateHTML(const OutputFile: string);
    procedure GeneratePDF(const OutputFile: string);
    procedure GenerateMarkdown(const OutputFile: string);
  end;

implementation

constructor TUserManualGenerator.Create(const ATitle, AVersion: string);
begin
  inherited Create;
  FTitle := ATitle;
  FVersion := AVersion;
  SetLength(FSections, 0);
end;

procedure TUserManualGenerator.AddSection(const Title, Content: string; Level: Integer);
var
  idx: Integer;
begin
  idx := Length(FSections);
  SetLength(FSections, idx + 1);
  FSections[idx].Title := Title;
  FSections[idx].Content := Content;
  FSections[idx].Level := Level;
  FSections[idx].ScreenshotPath := '';
end;

procedure TUserManualGenerator.AddScreenshot(const Path: string);
begin
  if Length(FSections) > 0 then
    FSections[High(FSections)].ScreenshotPath := Path;
end;

procedure TUserManualGenerator.GenerateHTML(const OutputFile: string);
var
  html: TStringList;
  i: Integer;
  headerTag: string;
begin
  html := TStringList.Create;
  try
    html.Add('<!DOCTYPE html>');
    html.Add('<html lang="fr">');
    html.Add('<head>');
    html.Add('  <meta charset="UTF-8">');
    html.Add('  <title>' + FTitle + ' - Manuel Utilisateur</title>');
    html.Add('  <style>');
    html.Add('    body { font-family: "Segoe UI", Arial, sans-serif; max-width: 900px; margin: 0 auto; padding: 20px; line-height: 1.6; }');
    html.Add('    h1 { color: #2c3e50; border-bottom: 3px solid #3498db; padding-bottom: 10px; }');
    html.Add('    h2 { color: #34495e; margin-top: 30px; }');
    html.Add('    h3 { color: #7f8c8d; }');
    html.Add('    .version { color: #95a5a6; font-size: 0.9em; }');
    html.Add('    .screenshot { max-width: 100%; border: 1px solid #ddd; margin: 15px 0; }');
    html.Add('    .toc { background: #ecf0f1; padding: 15px; border-radius: 5px; margin: 20px 0; }');
    html.Add('    .toc ul { list-style-type: none; }');
    html.Add('  </style>');
    html.Add('</head>');
    html.Add('<body>');
    html.Add('  <h1>' + FTitle + '</h1>');
    html.Add('  <p class="version">Version ' + FVersion + '</p>');

    // Table des matières
    html.Add('  <div class="toc">');
    html.Add('    <h2>Table des matières</h2>');
    html.Add('    <ul>');
    for i := 0 to High(FSections) do
    begin
      if FSections[i].Level = 1 then
        html.Add('      <li><a href="#section' + IntToStr(i) + '">' + FSections[i].Title + '</a></li>');
    end;
    html.Add('    </ul>');
    html.Add('  </div>');

    // Contenu
    for i := 0 to High(FSections) do
    begin
      case FSections[i].Level of
        1: headerTag := 'h2';
        2: headerTag := 'h3';
        3: headerTag := 'h4';
      else
        headerTag := 'h5';
      end;

      html.Add('  <' + headerTag + ' id="section' + IntToStr(i) + '">' +
               FSections[i].Title + '</' + headerTag + '>');
      html.Add('  <p>' + StringReplace(FSections[i].Content, #10, '</p><p>', [rfReplaceAll]) + '</p>');

      if FSections[i].ScreenshotPath <> '' then
        html.Add('  <img src="' + FSections[i].ScreenshotPath + '" class="screenshot" alt="Screenshot">');
    end;

    html.Add('</body>');
    html.Add('</html>');

    html.SaveToFile(OutputFile);
  finally
    html.Free;
  end;
end;

procedure TUserManualGenerator.GenerateMarkdown(const OutputFile: string);
var
  md: TStringList;
  i: Integer;
  prefix: string;
begin
  md := TStringList.Create;
  try
    md.Add('# ' + FTitle);
    md.Add('');
    md.Add('**Version ' + FVersion + '**');
    md.Add('');
    md.Add('---');
    md.Add('');

    // Table des matières
    md.Add('## Table des matières');
    md.Add('');
    for i := 0 to High(FSections) do
    begin
      if FSections[i].Level = 1 then
        md.Add('- [' + FSections[i].Title + '](#' +
               StringReplace(LowerCase(FSections[i].Title), ' ', '-', [rfReplaceAll]) + ')');
    end;
    md.Add('');
    md.Add('---');
    md.Add('');

    // Contenu
    for i := 0 to High(FSections) do
    begin
      prefix := StringOfChar('#', FSections[i].Level + 1);
      md.Add(prefix + ' ' + FSections[i].Title);
      md.Add('');
      md.Add(FSections[i].Content);
      md.Add('');

      if FSections[i].ScreenshotPath <> '' then
      begin
        md.Add('![Screenshot](' + FSections[i].ScreenshotPath + ')');
        md.Add('');
      end;
    end;

    md.SaveToFile(OutputFile);
  finally
    md.Free;
  end;
end;

procedure TUserManualGenerator.GeneratePDF(const OutputFile: string);
begin
  // Pour générer un PDF, on peut:
  // 1. Utiliser une bibliothèque comme fpReport
  // 2. Générer du HTML puis utiliser wkhtmltopdf
  // 3. Générer du LaTeX puis compiler avec pdflatex

  WriteLn('Génération PDF: Non implémentée dans cet exemple');
  WriteLn('Suggestion: Générer HTML puis utiliser wkhtmltopdf:');
  WriteLn('  wkhtmltopdf manual.html manual.pdf');
end;

end.
```

**Utilisation :**

```pascal
program GenerateUserManual;

uses
  UserManualGenerator;

var
  manual: TUserManualGenerator;

begin
  manual := TUserManualGenerator.Create('Mon Application', '2.5.0');
  try
    // Introduction
    manual.AddSection('Introduction',
      'Bienvenue dans Mon Application. Ce logiciel vous permet de gérer vos projets efficacement.', 1);

    // Installation
    manual.AddSection('Installation', '', 1);
    manual.AddSection('Configuration requise',
      'Système d''exploitation: Windows 10/11 ou Ubuntu 20.04+' + #10 +
      'Mémoire: 4 GB RAM minimum' + #10 +
      'Espace disque: 500 MB', 2);

    manual.AddSection('Procédure d''installation',
      '1. Téléchargez l''installateur depuis notre site' + #10 +
      '2. Double-cliquez sur le fichier téléchargé' + #10 +
      '3. Suivez les instructions à l''écran', 2);

    // Démarrage rapide
    manual.AddSection('Démarrage rapide', '', 1);
    manual.AddSection('Premier lancement',
      'Au premier lancement, l''assistant de configuration vous guidera pour paramétrer l''application.', 2);
    manual.AddScreenshot('screenshots/first_launch.png');

    manual.AddSection('Créer un nouveau projet',
      'Cliquez sur "Fichier > Nouveau projet" ou utilisez le raccourci Ctrl+N.', 2);
    manual.AddScreenshot('screenshots/new_project.png');

    // Fonctionnalités
    manual.AddSection('Fonctionnalités principales', '', 1);
    manual.AddSection('Gestion de projets',
      'L''application vous permet de créer, modifier et supprimer des projets.', 2);

    manual.AddSection('Collaboration',
      'Partagez vos projets avec d''autres utilisateurs en temps réel.', 2);

    // FAQ
    manual.AddSection('Foire aux questions', '', 1);
    manual.AddSection('Comment réinitialiser mon mot de passe ?',
      'Cliquez sur "Mot de passe oublié" sur l''écran de connexion.', 2);

    // Génération
    manual.GenerateHTML('manual.html');
    WriteLn('Manuel HTML généré: manual.html');

    manual.GenerateMarkdown('manual.md');
    WriteLn('Manuel Markdown généré: manual.md');

  finally
    manual.Free;
  end;
end.
```

## Versionning de la documentation

### Gérer plusieurs versions

**Structure recommandée :**

```
docs/
├── latest/           # Documentation de la dernière version
├── v2.0/            # Documentation version 2.0
├── v1.5/            # Documentation version 1.5
└── versions.json    # Métadonnées des versions
```

**`versions.json` :**

```json
{
  "current": "2.0.0",
  "versions": [
    {
      "number": "2.0.0",
      "path": "latest",
      "release_date": "2025-01-15",
      "status": "stable"
    },
    {
      "number": "1.5.2",
      "path": "v1.5",
      "release_date": "2024-08-10",
      "status": "maintenance"
    },
    {
      "number": "1.0.0",
      "path": "v1.0",
      "release_date": "2023-05-01",
      "status": "deprecated"
    }
  ]
}
```

### Script de versioning

**`version_docs.sh` :**

```bash
#!/bin/bash
# Script pour créer une nouvelle version de documentation

VERSION=$1

if [ -z "$VERSION" ]; then
    echo "Usage: ./version_docs.sh <version>"
    echo "Exemple: ./version_docs.sh 2.1.0"
    exit 1
fi

echo "=== Création de la documentation version $VERSION ==="

# Créer le répertoire de version
mkdir -p "docs/v$VERSION"

# Générer la documentation
./generate_docs.sh

# Copier dans le répertoire versionné
cp -r docs/latest/* "docs/v$VERSION/"

# Mettre à jour versions.json
# (nécessite jq pour manipuler le JSON)
jq ".versions += [{
  \"number\": \"$VERSION\",
  \"path\": \"v$VERSION\",
  \"release_date\": \"$(date +%Y-%m-%d)\",
  \"status\": \"stable\"
}]" docs/versions.json > docs/versions.json.tmp

mv docs/versions.json.tmp docs/versions.json

echo "✓ Documentation version $VERSION créée dans docs/v$VERSION"
```

## Automatisation complète

### Makefile pour la documentation

**`Makefile` :**

```makefile
# Makefile pour générer la documentation

PROJECT_NAME = Mon Projet
VERSION = $(shell cat VERSION.txt)
SOURCE_DIR = src
DOCS_DIR = docs
BUILD_DIR = $(DOCS_DIR)/build

.PHONY: all clean docs html pdf help

all: docs

docs: html

html:
	@echo "Génération de la documentation HTML..."
	@mkdir -p $(BUILD_DIR)/html
	pasdoc --format=html \
	       --output=$(BUILD_DIR)/html \
	       --title="$(PROJECT_NAME) v$(VERSION)" \
	       --source=$(SOURCE_DIR)/*.pas \
	       --css=custom.css \
	       --graphviz-classes \
	       --graphviz-uses
	@echo "✓ Documentation générée dans $(BUILD_DIR)/html"

pdf: html
	@echo "Génération du PDF..."
	@wkhtmltopdf $(BUILD_DIR)/html/index.html $(BUILD_DIR)/manual.pdf
	@echo "✓ PDF généré: $(BUILD_DIR)/manual.pdf"

serve:
	@echo "Démarrage du serveur de documentation..."
	@cd $(BUILD_DIR)/html && python3 -m http.server 8000

clean:
	@echo "Nettoyage des fichiers générés..."
	@rm -rf $(BUILD_DIR)
	@echo "✓ Nettoyage terminé"

help:
	@echo "Commandes disponibles:"
	@echo "  make html    - Génère la documentation HTML"
	@echo "  make pdf     - Génère la documentation PDF"
	@echo "  make serve   - Lance un serveur local"
	@echo "  make clean   - Nettoie les fichiers générés"
	@echo "  make help    - Affiche cette aide"
```

**Utilisation :**

```bash
# Générer la documentation HTML
make html

# Générer le PDF
make pdf

# Lancer un serveur local
make serve
# Puis ouvrir http://localhost:8000

# Nettoyer
make clean
```

## Métriques de documentation

### Analyser la qualité de la documentation

```pascal
program DocumentationAnalyzer;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils;

type
  TDocStats = record
    TotalFunctions: Integer;
    DocumentedFunctions: Integer;
    TotalClasses: Integer;
    DocumentedClasses: Integer;
    TotalLines: Integer;
    CommentLines: Integer;
  end;

function AnalyzeSourceFile(const FileName: string): TDocStats;
var
  lines: TStringList;
  i: Integer;
  line, trimmedLine: string;
  inComment, inFunction, hasDocComment: Boolean;
begin
  FillChar(Result, SizeOf(Result), 0);

  lines := TStringList.Create;
  try
    lines.LoadFromFile(FileName);
    Result.TotalLines := lines.Count;

    inComment := False;
    inFunction := False;
    hasDocComment := False;

    for i := 0 to lines.Count - 1 do
    begin
      line := lines[i];
      trimmedLine := Trim(line);

      // Détection de commentaires
      if Pos('{', trimmedLine) > 0 then
        inComment := True;
      if Pos('(*', trimmedLine) > 0 then
        inComment := True;
      if (Pos('//', trimmedLine) = 1) or inComment then
        Inc(Result.CommentLines);
      if Pos('}', trimmedLine) > 0 then
        inComment := False;
      if Pos('*)', trimmedLine) > 0 then
        inComment := False;

      // Détection de fonctions/procédures
      if (Pos('function ', LowerCase(trimmedLine)) = 1) or
         (Pos('procedure ', LowerCase(trimmedLine)) = 1) then
      begin
        Inc(Result.TotalFunctions);
        if hasDocComment then
          Inc(Result.DocumentedFunctions);
        hasDocComment := False;
      end;

      // Détection de classes
      if Pos('= class', LowerCase(trimmedLine)) > 0 then
      begin
        Inc(Result.TotalClasses);
        if hasDocComment then
          Inc(Result.DocumentedClasses);
        hasDocComment := False;
      end;

      // Marquer si ligne précédente était un commentaire
      if inComment or (Pos('//', trimmedLine) = 1) then
        hasDocComment := True;
    end;

  finally
    lines.Free;
  end;
end;

procedure PrintStats(const Stats: TDocStats);
var
  funcCoverage, classCoverage, commentRatio: Double;
begin
  WriteLn('=== Statistiques de documentation ===');
  WriteLn;
  WriteLn('Lignes de code:         ', Stats.TotalLines);
  WriteLn('Lignes de commentaires: ', Stats.CommentLines);

  if Stats.TotalLines > 0 then
  begin
    commentRatio := (Stats.CommentLines * 100.0) / Stats.TotalLines;
    WriteLn('Ratio commentaires:     ', commentRatio:0:1, '%');
  end;

  WriteLn;
  WriteLn('Fonctions totales:      ', Stats.TotalFunctions);
  WriteLn('Fonctions documentées:  ', Stats.DocumentedFunctions);

  if Stats.TotalFunctions > 0 then
  begin
    funcCoverage := (Stats.DocumentedFunctions * 100.0) / Stats.TotalFunctions;
    WriteLn('Couverture fonctions:   ', funcCoverage:0:1, '%');

    if funcCoverage < 50 then
      WriteLn('⚠️  Couverture faible!')
    else if funcCoverage < 80 then
      WriteLn('⚠️  Couverture moyenne')
    else
      WriteLn('✓ Bonne couverture');
  end;

  WriteLn;
  WriteLn('Classes totales:        ', Stats.TotalClasses);
  WriteLn('Classes documentées:    ', Stats.DocumentedClasses);

  if Stats.TotalClasses > 0 then
  begin
    classCoverage := (Stats.DocumentedClasses * 100.0) / Stats.TotalClasses;
    WriteLn('Couverture classes:     ', classCoverage:0:1, '%');

    if classCoverage < 50 then
      WriteLn('⚠️  Couverture faible!')
    else if classCoverage < 80 then
      WriteLn('⚠️  Couverture moyenne')
    else
      WriteLn('✓ Bonne couverture');
  end;
end;

var
  stats: TDocStats;
  searchRec: TSearchRec;
  totalStats: TDocStats;

begin
  WriteLn('=== Analyseur de documentation ===');
  WriteLn;

  FillChar(totalStats, SizeOf(totalStats), 0);

  // Analyser tous les fichiers .pas
  if FindFirst('src/*.pas', faAnyFile, searchRec) = 0 then
  begin
    repeat
      WriteLn('Analyse de ', searchRec.Name, '...');
      stats := AnalyzeSourceFile('src/' + searchRec.Name);

      // Accumuler les statistiques
      Inc(totalStats.TotalLines, stats.TotalLines);
      Inc(totalStats.CommentLines, stats.CommentLines);
      Inc(totalStats.TotalFunctions, stats.TotalFunctions);
      Inc(totalStats.DocumentedFunctions, stats.DocumentedFunctions);
      Inc(totalStats.TotalClasses, stats.TotalClasses);
      Inc(totalStats.DocumentedClasses, stats.DocumentedClasses);

    until FindNext(searchRec) <> 0;
    FindClose(searchRec);
  end;

  WriteLn;
  WriteLn('=== TOTAUX ===');
  PrintStats(totalStats);
end.
```

### Rapport de couverture de documentation

**Générer un rapport HTML :**

```pascal
procedure GenerateDocCoverageReport(const Stats: TDocStats; const OutputFile: string);
var
  html: TStringList;
  funcCoverage, classCoverage, commentRatio: Double;
begin
  html := TStringList.Create;
  try
    if Stats.TotalFunctions > 0 then
      funcCoverage := (Stats.DocumentedFunctions * 100.0) / Stats.TotalFunctions
    else
      funcCoverage := 0;

    if Stats.TotalClasses > 0 then
      classCoverage := (Stats.DocumentedClasses * 100.0) / Stats.TotalClasses
    else
      classCoverage := 0;

    if Stats.TotalLines > 0 then
      commentRatio := (Stats.CommentLines * 100.0) / Stats.TotalLines
    else
      commentRatio := 0;

    html.Add('<!DOCTYPE html>');
    html.Add('<html lang="fr">');
    html.Add('<head>');
    html.Add('  <meta charset="UTF-8">');
    html.Add('  <title>Rapport de Couverture Documentation</title>');
    html.Add('  <style>');
    html.Add('    body { font-family: Arial, sans-serif; max-width: 800px; margin: 20px auto; }');
    html.Add('    .metric { margin: 20px 0; padding: 15px; border-radius: 5px; }');
    html.Add('    .good { background: #d4edda; border-left: 4px solid #28a745; }');
    html.Add('    .medium { background: #fff3cd; border-left: 4px solid #ffc107; }');
    html.Add('    .bad { background: #f8d7da; border-left: 4px solid #dc3545; }');
    html.Add('    .progress-bar { width: 100%; height: 30px; background: #e9ecef; border-radius: 5px; overflow: hidden; }');
    html.Add('    .progress-fill { height: 100%; background: #007bff; text-align: center; line-height: 30px; color: white; }');
    html.Add('  </style>');
    html.Add('</head>');
    html.Add('<body>');
    html.Add('  <h1>Rapport de Couverture Documentation</h1>');
    html.Add('  <p>Généré le ' + FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) + '</p>');

    // Fonctions
    html.Add('  <div class="metric ' +
             IfThen(funcCoverage >= 80, 'good', IfThen(funcCoverage >= 50, 'medium', 'bad')) + '">');
    html.Add('    <h2>Couverture des fonctions</h2>');
    html.Add('    <p>' + IntToStr(Stats.DocumentedFunctions) + ' / ' +
             IntToStr(Stats.TotalFunctions) + ' fonctions documentées</p>');
    html.Add('    <div class="progress-bar">');
    html.Add('      <div class="progress-fill" style="width: ' + FloatToStr(funcCoverage) + '%">');
    html.Add('        ' + FormatFloat('0.0', funcCoverage) + '%');
    html.Add('      </div>');
    html.Add('    </div>');
    html.Add('  </div>');

    // Classes
    html.Add('  <div class="metric ' +
             IfThen(classCoverage >= 80, 'good', IfThen(classCoverage >= 50, 'medium', 'bad')) + '">');
    html.Add('    <h2>Couverture des classes</h2>');
    html.Add('    <p>' + IntToStr(Stats.DocumentedClasses) + ' / ' +
             IntToStr(Stats.TotalClasses) + ' classes documentées</p>');
    html.Add('    <div class="progress-bar">');
    html.Add('      <div class="progress-fill" style="width: ' + FloatToStr(classCoverage) + '%">');
    html.Add('        ' + FormatFloat('0.0', classCoverage) + '%');
    html.Add('      </div>');
    html.Add('    </div>');
    html.Add('  </div>');

    // Commentaires
    html.Add('  <div class="metric">');
    html.Add('    <h2>Ratio de commentaires</h2>');
    html.Add('    <p>' + IntToStr(Stats.CommentLines) + ' / ' +
             IntToStr(Stats.TotalLines) + ' lignes de commentaires</p>');
    html.Add('    <div class="progress-bar">');
    html.Add('      <div class="progress-fill" style="width: ' + FloatToStr(commentRatio) + '%">');
    html.Add('        ' + FormatFloat('0.0', commentRatio) + '%');
    html.Add('      </div>');
    html.Add('    </div>');
    html.Add('  </div>');

    html.Add('</body>');
    html.Add('</html>');

    html.SaveToFile(OutputFile);
  finally
    html.Free;
  end;
end;
```

## Documentation multilingue

### Support de plusieurs langues

**Structure pour documentation multilingue :**

```
docs/
├── en/              # Documentation en anglais
│   ├── index.html
│   └── api/
├── fr/              # Documentation en français
│   ├── index.html
│   └── api/
└── de/              # Documentation en allemand
    ├── index.html
    └── api/
```

**Script de génération multilingue :**

```bash
#!/bin/bash
# generate_multilang_docs.sh

LANGUAGES=("en" "fr" "de")
SOURCE_DIR="src"

for lang in "${LANGUAGES[@]}"; do
    echo "Génération de la documentation en $lang..."

    pasdoc --format=html \
           --output="docs/$lang" \
           --language="$lang" \
           --title="My Project" \
           --source="$SOURCE_DIR/*.pas" \
           --introduction="intro_$lang.txt"

    echo "✓ Documentation $lang générée"
done

# Créer une page d'accueil pour la sélection de langue
cat > docs/index.html << 'EOF'
<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8">
    <title>Documentation - Language Selection</title>
    <style>
        body { font-family: Arial; text-align: center; padding: 50px; }
        .language-btn { display: inline-block; margin: 10px; padding: 20px 40px;
                       background: #007bff; color: white; text-decoration: none;
                       border-radius: 5px; font-size: 18px; }
        .language-btn:hover { background: #0056b3; }
    </style>
</head>
<body>
    <h1>Choose Your Language / Choisissez votre langue</h1>
    <div>
        <a href="en/index.html" class="language-btn">English</a>
        <a href="fr/index.html" class="language-btn">Français</a>
        <a href="de/index.html" class="language-btn">Deutsch</a>
    </div>
</body>
</html>
EOF

echo "✓ Page de sélection de langue créée"
```

## Intégration avec l'IDE Lazarus

### Générer la documentation depuis Lazarus

**Menu personnalisé dans Lazarus :**

1. Ouvrir **Outils → Configurer les outils externes...**
2. Ajouter un nouvel outil :
   - **Titre** : Générer Documentation
   - **Nom du programme** : `pasdoc` (ou chemin complet)
   - **Paramètres** : `--format=html --output=$ProjPath()/docs $EdFile()`
   - **Répertoire de travail** : `$ProjPath()`

**Macro Lazarus pour automatisation :**

```pascal
unit DocGeneratorMacro;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process;

procedure GenerateProjectDocumentation;

implementation

procedure GenerateProjectDocumentation;
var
  process: TProcess;
  outputLines: TStringList;
begin
  process := TProcess.Create(nil);
  outputLines := TStringList.Create;
  try
    process.Executable := 'pasdoc';
    process.Parameters.Add('--format=html');
    process.Parameters.Add('--output=docs');
    process.Parameters.Add('src/*.pas');
    process.Options := process.Options + [poWaitOnExit, poUsePipes];

    try
      process.Execute;
      outputLines.LoadFromStream(process.Output);

      if process.ExitStatus = 0 then
        ShowMessage('Documentation générée avec succès dans le dossier docs/')
      else
        ShowMessage('Erreur lors de la génération de la documentation:'#13#10 +
                   outputLines.Text);
    except
      on E: Exception do
        ShowMessage('Erreur: ' + E.Message);
    end;

  finally
    outputLines.Free;
    process.Free;
  end;
end;

end.
```

## Checklist de documentation complète

### Avant la release

```
Documentation du code:
□ Toutes les fonctions publiques documentées
□ Toutes les classes documentées
□ Tous les paramètres expliqués
□ Valeurs de retour décrites
□ Exceptions possibles listées
□ Exemples fournis pour fonctions complexes

Documentation utilisateur:
□ Guide d'installation complet
□ Tutoriel de démarrage rapide
□ Exemples d'utilisation courants
□ FAQ à jour
□ Guide de dépannage
□ Notes de version (changelog)

Documentation technique:
□ Architecture du projet expliquée
□ Diagrammes de classes générés
□ Documentation API générée
□ Guide de contribution (pour open source)
□ Spécifications techniques
□ Processus de build documenté

Formats de documentation:
□ HTML généré et testé
□ PDF créé (optionnel)
□ README.md à jour
□ Documentation en ligne déployée
□ Version imprimable disponible

Accessibilité:
□ Liens hypertextes fonctionnels
□ Navigation claire
□ Recherche disponible
□ Support multi-navigateurs testé
□ Responsive (mobile-friendly)
```

## Maintenance de la documentation

### Processus de mise à jour

**1. Documentation synchronisée avec le code**

```bash
#!/bin/bash
# pre-commit hook pour vérifier la documentation

echo "Vérification de la documentation..."

# Chercher les fonctions non documentées
UNDOC=$(grep -r "^function\|^procedure" src/*.pas | \
        grep -v "{" | \
        wc -l)

if [ $UNDOC -gt 0 ]; then
    echo "⚠️  Attention: $UNDOC fonction(s) sans documentation détectée(s)"
    read -p "Continuer le commit ? (o/n) " -n 1 -r
    echo
    if [[ ! $REPLY =~ ^[Oo]$ ]]; then
        exit 1
    fi
fi

echo "✓ Vérification terminée"
```

**2. Revue de documentation**

```
□ Un réviseur vérifie la clarté
□ Les exemples sont testés
□ Les liens sont validés
□ Les captures d'écran sont à jour
□ L'orthographe est vérifiée
```

**3. Notification des changements**

**`CHANGELOG_DOCS.md` :**

```markdown
# Changelog Documentation

## [2.0.0] - 2025-10-06

### Ajouté
- Documentation de l'API REST
- Tutoriel sur les plugins
- Guide de migration depuis v1.x

### Modifié
- Mise à jour des captures d'écran
- Amélioration du guide d'installation
- Refonte de la section architecture

### Corrigé
- Liens cassés dans la section API
- Exemples de code obsolètes
- Fautes de frappe dans le manuel utilisateur

### Supprimé
- Documentation des fonctionnalités deprecated
```

## Outils de validation

### Vérifier les liens

**Script de validation des liens :**

```pascal
program LinkChecker;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, RegExpr, fphttpclient;

function ExtractLinks(const HTMLContent: string): TStringList;
var
  regex: TRegExpr;
begin
  Result := TStringList.Create;
  Result.Duplicates := dupIgnore;
  Result.Sorted := True;

  regex := TRegExpr.Create;
  try
    regex.Expression := 'href="([^"]+)"';
    if regex.Exec(HTMLContent) then
    begin
      repeat
        Result.Add(regex.Match[1]);
      until not regex.ExecNext;
    end;
  finally
    regex.Free;
  end;
end;

function CheckLink(const URL: string): Boolean;
var
  client: TFPHTTPClient;
begin
  Result := False;

  // Ignorer les liens internes et ancres
  if (Pos('#', URL) = 1) or (Pos('mailto:', URL) = 1) then
    Exit(True);

  client := TFPHTTPClient.Create(nil);
  try
    try
      client.Get(URL);
      Result := (client.ResponseStatusCode >= 200) and
                (client.ResponseStatusCode < 400);
    except
      Result := False;
    end;
  finally
    client.Free;
  end;
end;

var
  htmlContent: string;
  links: TStringList;
  i: Integer;
  brokenLinks: Integer;

begin
  WriteLn('=== Vérificateur de liens ===');
  WriteLn;

  // Charger le fichier HTML
  with TStringList.Create do
  try
    LoadFromFile('docs/index.html');
    htmlContent := Text;
  finally
    Free;
  end;

  // Extraire les liens
  links := ExtractLinks(htmlContent);
  try
    WriteLn('Liens trouvés: ', links.Count);
    WriteLn('Vérification en cours...');
    WriteLn;

    brokenLinks := 0;

    for i := 0 to links.Count - 1 do
    begin
      Write('Test ', i + 1:3, '/', links.Count, ': ', links[i]:50);

      if CheckLink(links[i]) then
        WriteLn(' ✓')
      else
      begin
        WriteLn(' ❌ CASSÉ');
        Inc(brokenLinks);
      end;

      Sleep(100); // Pause pour ne pas surcharger le serveur
    end;

    WriteLn;
    WriteLn('=== Résultats ===');
    WriteLn('Liens testés: ', links.Count);
    WriteLn('Liens cassés: ', brokenLinks);

    if brokenLinks > 0 then
      WriteLn('⚠️  Des liens cassés ont été détectés!')
    else
      WriteLn('✓ Tous les liens sont valides');

  finally
    links.Free;
  end;
end.
```

## Exemples de documentation professionnelle

### Template de documentation complète

**Structure recommandée :**

```
MonProjet/
├── docs/
│   ├── index.html                    # Page d'accueil
│   ├── getting-started.html          # Démarrage rapide
│   ├── installation.html             # Guide d'installation
│   ├── user-guide/
│   │   ├── basics.html               # Bases
│   │   ├── advanced.html             # Avancé
│   │   └── troubleshooting.html      # Dépannage
│   ├── api/
│   │   ├── index.html                # Index API
│   │   ├── classes/                  # Documentation classes
│   │   └── functions/                # Documentation fonctions
│   ├── tutorials/
│   │   ├── tutorial-01.html          # Tutoriel 1
│   │   └── tutorial-02.html          # Tutoriel 2
│   ├── examples/
│   │   ├── example-01.pas            # Exemple 1
│   │   └── example-02.pas            # Exemple 2
│   ├── changelog.html                # Historique des versions
│   ├── faq.html                      # FAQ
│   └── contributing.html             # Guide de contribution
├── README.md                         # README principal
├── CHANGELOG.md                      # Changelog
├── LICENSE.txt                       # Licence
└── CONTRIBUTING.md                   # Guide contribution
```

## Conclusion

La documentation automatique est un élément crucial pour la réussite de tout projet FreePascal/Lazarus.

**Points clés à retenir :**

✅ **Documenter en écrivant le code** : Ne pas reporter la documentation à plus tard  
✅ **Utiliser PasDoc** : Outil standard pour FreePascal, simple et efficace  
✅ **Automatiser** : Scripts, CI/CD, hooks Git pour maintenir à jour  
✅ **Plusieurs formats** : HTML pour le web, Markdown pour GitHub, PDF pour impression  
✅ **Multi-audience** : Documentation développeur ET utilisateur final  
✅ **Maintenir à jour** : La documentation obsolète est pire que pas de documentation

**Stratégie de documentation recommandée :**

1. **Phase 1 : Basique (Jour 1)**
   - Commenter les fonctions publiques
   - Créer un README.md minimal
   - Documenter l'installation

2. **Phase 2 : Standard (Semaine 1)**
   - Installer PasDoc
   - Générer la documentation HTML
   - Créer un guide de démarrage rapide

3. **Phase 3 : Avancée (Mois 1)**
   - Automatiser la génération (scripts)
   - Créer des tutoriels
   - Ajouter des exemples de code

4. **Phase 4 : Professionnelle (Continu)**
   - Intégration CI/CD
   - Documentation multilingue
   - Métriques de qualité
   - Versioning de la documentation

**Différences Windows/Ubuntu :**

| Aspect | Windows | Ubuntu |
|--------|---------|--------|
| Installation PasDoc | Téléchargement manuel | apt-get install pasdoc |
| Scripts | PowerShell (.ps1) | Bash (.sh) |
| Outils graphiques | Nombreux | Sphinx, doxygen |
| Serveur local | Python + Windows Firewall | Python simple |

**Ressources :**

- PasDoc : https://pasdoc.github.io/
- Doxygen : https://www.doxygen.nl/
- Sphinx : https://www.sphinx-doc.org/
- Natural Docs : https://www.naturaldocs.org/

**Rappel important :**

> "Code tells you HOW, Documentation tells you WHY"

La documentation n'est pas un luxe, c'est une nécessité. Un code non documenté est un code qui sera difficilement maintenable et qui découragera les contributions.

Investissez dans la documentation dès le début du projet, et elle vous le rendra au centuple ! 📚✨

⏭️ [Interopérabilité et Bindings](/19-interoperabilite-bindings/README.md)
