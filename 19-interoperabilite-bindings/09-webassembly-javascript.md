🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 19.9 WebAssembly et JavaScript - Tutoriel pour Développeurs Avancés

## Table des matières

1. [Introduction à WebAssembly](#introduction-à-webassembly)
2. [Compiler FreePascal vers WebAssembly](#compiler-freepascal-vers-webassembly)
3. [Pas2JS : Transpilation Pascal vers JavaScript](#pas2js--transpilation-pascal-vers-javascript)
4. [Interopérabilité Pascal/JavaScript](#interopérabilité-pascaljavascript)
5. [Manipulation du DOM depuis Pascal](#manipulation-du-dom-depuis-pascal)
6. [Cas pratiques](#cas-pratiques)
7. [Performance et optimisation](#performance-et-optimisation)
8. [Déploiement d'applications web](#déploiement-dapplications-web)
9. [Conclusion](#conclusion)

---

## Introduction à WebAssembly

### Qu'est-ce que WebAssembly ?

**WebAssembly** (abrégé **Wasm**) est un format binaire portable qui permet d'exécuter du code compilé dans les navigateurs web à des vitesses proches du natif. C'est un complément à JavaScript, pas un remplacement.

**Analogie simple** : Si JavaScript est comme écrire des instructions en anglais, WebAssembly est comme envoyer des instructions en code machine - beaucoup plus rapide à exécuter, mais moins facile à écrire directement.

### Architecture WebAssembly

```
┌─────────────────────────────────────────────────┐
│              Navigateur Web                     │
├─────────────────────────────────────────────────┤
│  ┌──────────────┐        ┌──────────────┐       │
│  │  JavaScript  │◀──────▶│  WebAssembly │       │
│  │   Engine     │  API   │    Module    │       │
│  │   (V8, SM)   │        │   (.wasm)    │       │
│  └──────────────┘        └──────────────┘       │
├─────────────────────────────────────────────────┤
│           Navigateur (Chrome, Firefox, etc.)    │
└─────────────────────────────────────────────────┘
```

### Pourquoi WebAssembly avec FreePascal ?

#### 1. Performance

WebAssembly offre des performances bien supérieures à JavaScript pour :
- Calculs mathématiques intensifs
- Traitement d'images et vidéos
- Compression/décompression
- Cryptographie
- Jeux et simulations physiques

**Exemple de gain** :
```
Opération : Calcul de 1 million de nombres premiers

JavaScript pur    : ~2500 ms  
WebAssembly       : ~400 ms  
Gain              : 6x plus rapide
```

#### 2. Réutilisation de code

Vous pouvez réutiliser du code Pascal existant dans le navigateur :

```pascal
// Code Pascal existant
function CalculateComplexFormula(x, y: Double): Double;  
begin
  Result := // ... calcul complexe
end;

// Utilisable dans le navigateur via WebAssembly!
```

#### 3. Type-safety et robustesse

Pascal est un langage fortement typé, ce qui réduit les erreurs :

```pascal
// Pascal - Erreur détectée à la compilation
var
  x: Integer;
begin
  x := 'Hello';  // ERREUR : Type incompatible
end;
```

```javascript
// JavaScript - Erreur à l'exécution
let x = 42;  
x = "Hello";  // Autorisé, mais peut causer des bugs
```

### WebAssembly vs JavaScript : Quand utiliser quoi ?

| Aspect | JavaScript | WebAssembly |
|--------|------------|-------------|
| **Manipulation DOM** | ✓ Excellent | ✗ Indirect seulement |
| **Calculs intensifs** | △ Correct | ✓ Excellent |
| **Taille du code** | △ Moyenne | ✓ Compacte (binaire) |
| **Chargement initial** | ✓ Rapide | △ Nécessite compilation |
| **Débogage** | ✓ Facile | △ Plus complexe |
| **Interactivité UI** | ✓ Excellent | ✗ Via JavaScript |

**Règle d'or** : Utilisez WebAssembly pour les calculs, JavaScript pour l'interface.

---

## Compiler FreePascal vers WebAssembly

### État actuel du support

**Important** : Le support WebAssembly dans FreePascal est **expérimental** (fin 2024/début 2025). Il existe deux approches principales :

1. **WebAssembly natif** : Compilation directe vers Wasm (limité)
2. **Pas2JS** : Transpilation vers JavaScript (mature et recommandé)

### Installation des outils

#### Installer Pas2JS

**Sur Windows** :

```bash
# Via Lazarus
# Ouvrir Lazarus → Package → Installer les packages
# Chercher "pas2js" et installer

# Ou télécharger depuis
# https://wiki.freepascal.org/pas2js
```

**Sur Linux (Ubuntu)** :

```bash
# Installer depuis les sources FPC
sudo apt install fpc-source

# Compiler Pas2JS
cd /usr/share/fpcsrc/packages/pas2js  
make  
sudo make install

# Vérifier l'installation
pas2js -h
```

**Vérification** :

```bash
pas2js -version
# Devrait afficher : Free Pascal Pas2JS Compiler version x.x.x
```

### Approche 1 : WebAssembly expérimental

**Note** : Cette approche est encore en développement. Voici un aperçu :

**hello_wasm.pas** :

```pascal
program HelloWasm;

{$mode objfpc}

function Add(a, b: Integer): Integer; export;  
begin
  Result := a + b;
end;

function Multiply(a, b: Integer): Integer; export;  
begin
  Result := a * b;
end;

exports
  Add,
  Multiply;

begin  
end.
```

**Compilation vers WebAssembly** (expérimental) :

```bash
# Avec le backend Wasm (si disponible dans votre version FPC)
fpc -Twasm -O3 hello_wasm.pas

# Génère : hello_wasm.wasm
```

**Utilisation en JavaScript** :

```html
<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8">
    <title>WebAssembly Demo</title>
</head>
<body>
    <h1>FreePascal WebAssembly Demo</h1>
    <div id="result"></div>

    <script>
        // Charger le module WebAssembly
        fetch('hello_wasm.wasm')
            .then(response => response.arrayBuffer())
            .then(bytes => WebAssembly.instantiate(bytes))
            .then(results => {
                const wasmModule = results.instance.exports;

                // Appeler les fonctions Pascal
                const sum = wasmModule.Add(10, 32);
                const product = wasmModule.Multiply(12, 8);

                document.getElementById('result').innerHTML = `
                    <p>10 + 32 = ${sum}</p>
                    <p>12 × 8 = ${product}</p>
                `;
            })
            .catch(error => console.error('Erreur:', error));
    </script>
</body>
</html>
```

---

## Pas2JS : Transpilation Pascal vers JavaScript

**Pas2JS** est l'approche **mature et recommandée** pour utiliser Pascal dans le navigateur. Il transpile du code Pascal en JavaScript lisible.

### Principe de fonctionnement

```
┌──────────────┐    Pas2JS     ┌──────────────┐
│ Code Pascal  │──────────────▶│  JavaScript  │
│   (.pas)     │  Transpile    │    (.js)     │
└──────────────┘               └──────────────┘
```

Le code Pascal est converti en JavaScript équivalent :

```pascal
// Pascal
function Add(a, b: Integer): Integer;  
begin
  Result := a + b;
end;
```

```javascript
// JavaScript généré
function Add(a, b) {
    return a + b;
}
```

### Premier programme Pas2JS

**hello_web.pas** :

```pascal
program HelloWeb;

{$mode objfpc}

uses
  JS, Web;

procedure ShowMessage;  
begin
  window.alert('Bonjour depuis Pascal!');
end;

begin
  // Point d'entrée de l'application
  ShowMessage;
end.
```

**Compilation** :

```bash
pas2js -Jirtl.js -Jc hello_web.pas
```

**Options importantes** :
- `-Jirtl.js` : Inclure la bibliothèque runtime
- `-Jc` : Créer un fichier JavaScript complet

**Résultat** : Fichier `hello_web.js` généré.

**Page HTML** :

```html
<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8">
    <title>Pas2JS Demo</title>
</head>
<body>
    <h1>Premier programme Pas2JS</h1>

    <!-- Inclure le runtime Pas2JS -->
    <script src="rtl.js"></script>

    <!-- Inclure votre programme -->
    <script src="hello_web.js"></script>
</body>
</html>
```

### Structure d'un projet Pas2JS

**Projet complet** :

```
my_web_app/
├── src/
│   ├── main.pas          # Programme principal
│   ├── utils.pas         # Unités Pascal
│   └── types.pas
├── web/
│   ├── index.html        # Page HTML
│   ├── styles.css        # CSS
│   └── assets/
│       └── images/
├── build/
│   ├── rtl.js           # Runtime généré
│   └── main.js          # JavaScript généré
└── compile.sh           # Script de compilation
```

**Script de compilation (compile.sh)** :

```bash
#!/bin/bash

# Répertoires
SRC_DIR="src"  
BUILD_DIR="build"  
WEB_DIR="web"

# Nettoyer
rm -rf $BUILD_DIR  
mkdir -p $BUILD_DIR

# Compiler avec Pas2JS
echo "Compilation de main.pas..."  
pas2js -Jirtl.js -Jc -O1 \
    -Fu$SRC_DIR \
    -FE$BUILD_DIR \
    $SRC_DIR/main.pas

if [ $? -eq 0 ]; then
    echo "✓ Compilation réussie"

    # Copier les fichiers web
    cp $WEB_DIR/*.html $BUILD_DIR/
    cp $WEB_DIR/*.css $BUILD_DIR/

    echo "✓ Fichiers copiés dans $BUILD_DIR"
    echo ""
    echo "Pour tester : ouvrez $BUILD_DIR/index.html dans un navigateur"
else
    echo "✗ Erreur de compilation"
    exit 1
fi
```

---

## Interopérabilité Pascal/JavaScript

### Appeler JavaScript depuis Pascal

Pas2JS fournit l'unité `JS` pour interagir avec JavaScript.

**Fonctions JavaScript basiques** :

```pascal
program JSInterop;

{$mode objfpc}

uses
  JS, Web;

procedure DemoConsole;  
begin
  // console.log()
  console.log('Message dans la console');
  console.warn('Avertissement');
  console.error('Erreur');
end;

procedure DemoAlert;  
begin
  // window.alert()
  window.alert('Bonjour!');

  // window.confirm()
  if window.confirm('Continuer?') then
    console.log('Utilisateur a confirmé')
  else
    console.log('Utilisateur a annulé');
end;

procedure DemoPrompt;  
var
  name: String;
begin
  // window.prompt()
  name := window.prompt('Votre nom?', 'Anonyme');
  console.log('Bonjour ' + name);
end;

begin
  DemoConsole;
  DemoAlert;
  DemoPrompt;
end.
```

### Utiliser des objets JavaScript

**Accéder aux propriétés** :

```pascal
uses
  JS, Web;

procedure ShowBrowserInfo;  
var
  userAgent: String;
  language: String;
begin
  // Accéder à navigator
  userAgent := window.navigator.userAgent;
  language := window.navigator.language;

  console.log('User Agent: ' + userAgent);
  console.log('Langue: ' + language);

  // Afficher dans la page
  document.body.innerHTML :=
    '<h2>Informations du navigateur</h2>' +
    '<p>User Agent: ' + userAgent + '</p>' +
    '<p>Langue: ' + language + '</p>';
end;

begin
  ShowBrowserInfo;
end.
```

### Appeler des fonctions JavaScript personnalisées

**JavaScript externe (custom.js)** :

```javascript
// Fonction JavaScript que nous voulons appeler depuis Pascal
function calculateArea(radius) {
    return Math.PI * radius * radius;
}

function formatCurrency(amount) {
    return new Intl.NumberFormat('fr-FR', {
        style: 'currency',
        currency: 'EUR'
    }).format(amount);
}

// Objet JavaScript avec méthodes
const Calculator = {
    add: function(a, b) {
        return a + b;
    },
    multiply: function(a, b) {
        return a * b;
    }
};
```

**Pascal - Déclarations externes** :

```pascal
program UseCustomJS;

{$mode objfpc}

uses
  JS, Web;

// Déclarer les fonctions JavaScript externes
function calculateArea(radius: Double): Double; external name 'calculateArea';  
function formatCurrency(amount: Double): String; external name 'formatCurrency';

// Déclarer l'objet Calculator
type
  TCalculator = class external name 'Calculator'
    function add(a, b: Integer): Integer;
    function multiply(a, b: Integer): Integer;
  end;

var
  calc: TCalculator; external name 'Calculator';

procedure TestJSFunctions;  
var
  area: Double;
  price: String;
  sum: Integer;
begin
  // Appeler calculateArea
  area := calculateArea(5.0);
  console.log('Aire du cercle (r=5): ' + FloatToStr(area));

  // Appeler formatCurrency
  price := formatCurrency(1234.56);
  console.log('Prix formaté: ' + price);

  // Utiliser l'objet Calculator
  sum := calc.add(10, 20);
  console.log('10 + 20 = ' + IntToStr(sum));
end;

begin
  TestJSFunctions;
end.
```

**Page HTML** :

```html
<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8">
    <title>Interop JavaScript</title>
</head>
<body>
    <h1>Interopérabilité Pascal/JavaScript</h1>

    <!-- JavaScript personnalisé d'abord -->
    <script src="custom.js"></script>

    <!-- Runtime Pas2JS -->
    <script src="rtl.js"></script>

    <!-- Programme Pascal compilé -->
    <script src="main.js"></script>
</body>
</html>
```

### Passer des callbacks Pascal à JavaScript

**JavaScript avec callback** :

```javascript
function processArray(arr, callback) {
    const results = [];
    for (let i = 0; i < arr.length; i++) {
        results.push(callback(arr[i]));
    }
    return results;
}
```

**Pascal** :

```pascal
program Callbacks;

{$mode objfpc}

uses
  JS, Web;

// Déclarer la fonction JavaScript
function processArray(arr: TJSArray; callback: TJSFunction): TJSArray;
  external name 'processArray';

// Fonction Pascal qui sera utilisée comme callback
function Double(value: JSValue): JSValue;  
var
  n: Integer;
begin
  n := Integer(value);
  Result := n * 2;
end;

procedure TestCallback;  
var
  numbers: TJSArray;
  results: TJSArray;
  i: Integer;
begin
  // Créer un tableau JavaScript
  numbers := TJSArray.new(1, 2, 3, 4, 5);

  // Passer la fonction Pascal comme callback
  results := processArray(numbers, @Double);

  // Afficher les résultats
  console.log('Résultats:');
  for i := 0 to results.length - 1 do
    console.log(results[i]);
end;

begin
  TestCallback;
end.
```

---

## Manipulation du DOM depuis Pascal

Le DOM (Document Object Model) est la structure de la page HTML que vous pouvez manipuler dynamiquement.

### Accéder aux éléments

**HTML de départ** :

```html
<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8">
    <title>DOM Manipulation</title>
</head>
<body>
    <h1 id="title">Titre Original</h1>
    <div id="content"></div>
    <button id="btnClick">Cliquez-moi</button>
    <input id="inputName" type="text" placeholder="Votre nom">

    <script src="rtl.js"></script>
    <script src="main.js"></script>
</body>
</html>
```

**Pascal - Manipulation du DOM** :

```pascal
program DOMManipulation;

{$mode objfpc}

uses
  JS, Web;

procedure ModifyElements;  
var
  titleElement: TJSHTMLElement;
  contentDiv: TJSHTMLElement;
  inputElement: TJSHTMLInputElement;
begin
  // Accéder à un élément par ID
  titleElement := TJSHTMLElement(document.getElementById('title'));
  titleElement.innerHTML := 'Titre Modifié depuis Pascal!';

  // Modifier le style
  titleElement.style.setProperty('color', 'blue');
  titleElement.style.setProperty('font-size', '2em');

  // Ajouter du contenu
  contentDiv := TJSHTMLElement(document.getElementById('content'));
  contentDiv.innerHTML :=
    '<p>Ce contenu a été généré par Pascal!</p>' +
    '<ul>' +
    '  <li>Item 1</li>' +
    '  <li>Item 2</li>' +
    '  <li>Item 3</li>' +
    '</ul>';

  // Lire la valeur d'un input
  inputElement := TJSHTMLInputElement(document.getElementById('inputName'));
  console.log('Valeur du champ: ' + inputElement.value);
end;

begin
  // Attendre que le DOM soit chargé
  window.addEventListener('DOMContentLoaded', @ModifyElements);
end.
```

### Créer des éléments dynamiquement

```pascal
procedure CreateElements;  
var
  newDiv: TJSHTMLElement;
  newParagraph: TJSHTMLElement;
  newButton: TJSHTMLButtonElement;
  body: TJSHTMLElement;
begin
  body := TJSHTMLElement(document.body);

  // Créer une div
  newDiv := TJSHTMLElement(document.createElement('div'));
  newDiv.id := 'dynamicContent';
  newDiv.className := 'container';

  // Créer un paragraphe
  newParagraph := TJSHTMLElement(document.createElement('p'));
  newParagraph.textContent := 'Paragraphe créé dynamiquement';
  newParagraph.style.setProperty('color', 'green');

  // Créer un bouton
  newButton := TJSHTMLButtonElement(document.createElement('button'));
  newButton.textContent := 'Nouveau Bouton';
  newButton.onclick := @HandleButtonClick;

  // Ajouter les éléments au DOM
  newDiv.appendChild(newParagraph);
  newDiv.appendChild(newButton);
  body.appendChild(newDiv);
end;

procedure HandleButtonClick(event: TJSMouseEvent);  
begin
  window.alert('Bouton dynamique cliqué!');
end;
```

### Gérer les événements

**Événements courants** :

```pascal
program EventHandling;

{$mode objfpc}

uses
  JS, Web;

var
  clickCount: Integer = 0;

// Gestionnaire de clic
procedure OnButtonClick(event: TJSMouseEvent);  
var
  button: TJSHTMLElement;
begin
  Inc(clickCount);
  button := TJSHTMLElement(event.target);
  button.textContent := 'Cliqué ' + IntToStr(clickCount) + ' fois';
  console.log('Clic numéro ' + IntToStr(clickCount));
end;

// Gestionnaire de saisie
procedure OnInputChange(event: TJSEvent);  
var
  input: TJSHTMLInputElement;
  display: TJSHTMLElement;
begin
  input := TJSHTMLInputElement(event.target);
  display := TJSHTMLElement(document.getElementById('nameDisplay'));
  display.textContent := 'Bonjour, ' + input.value + '!';
end;

// Gestionnaire de souris over
procedure OnMouseOver(event: TJSMouseEvent);  
var
  element: TJSHTMLElement;
begin
  element := TJSHTMLElement(event.target);
  element.style.setProperty('background-color', 'yellow');
end;

// Gestionnaire de souris out
procedure OnMouseOut(event: TJSMouseEvent);  
var
  element: TJSHTMLElement;
begin
  element := TJSHTMLElement(event.target);
  element.style.setProperty('background-color', '');
end;

procedure SetupEventListeners;  
var
  button: TJSHTMLElement;
  input: TJSHTMLInputElement;
  hoverDiv: TJSHTMLElement;
begin
  // Bouton click
  button := TJSHTMLElement(document.getElementById('btnClick'));
  button.addEventListener('click', @OnButtonClick);

  // Input change
  input := TJSHTMLInputElement(document.getElementById('inputName'));
  input.addEventListener('input', @OnInputChange);

  // Mouse over/out
  hoverDiv := TJSHTMLElement(document.getElementById('hoverDiv'));
  hoverDiv.addEventListener('mouseover', @OnMouseOver);
  hoverDiv.addEventListener('mouseout', @OnMouseOut);
end;

begin
  window.addEventListener('DOMContentLoaded', @SetupEventListeners);
end.
```

**HTML correspondant** :

```html
<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8">
    <title>Gestion d'événements</title>
    <style>
        #hoverDiv {
            width: 200px;
            height: 100px;
            border: 2px solid black;
            padding: 20px;
            margin: 20px 0;
        }
    </style>
</head>
<body>
    <h1>Gestion d'événements avec Pascal</h1>

    <button id="btnClick">Cliquez-moi</button>

    <br><br>
    <input id="inputName" type="text" placeholder="Votre nom">
    <div id="nameDisplay"></div>

    <div id="hoverDiv">Passez la souris ici</div>

    <script src="rtl.js"></script>
    <script src="main.js"></script>
</body>
</html>
```

### Formulaires et validation

```pascal
program FormValidation;

{$mode objfpc}

uses
  JS, Web, SysUtils;

procedure ValidateAndSubmit(event: TJSEvent);  
var
  emailInput: TJSHTMLInputElement;
  passwordInput: TJSHTMLInputElement;
  errorDiv: TJSHTMLElement;
  email, password: String;
  errors: String;
begin
  // Empêcher l'envoi par défaut
  event.preventDefault();

  errors := '';

  // Récupérer les valeurs
  emailInput := TJSHTMLInputElement(document.getElementById('email'));
  passwordInput := TJSHTMLInputElement(document.getElementById('password'));
  errorDiv := TJSHTMLElement(document.getElementById('errors'));

  email := emailInput.value;
  password := passwordInput.value;

  // Validation
  if email = '' then
    errors := errors + 'L''email est requis.<br>';

  if Pos('@', email) = 0 then
    errors := errors + 'L''email doit contenir un @.<br>';

  if password = '' then
    errors := errors + 'Le mot de passe est requis.<br>';

  if Length(password) < 8 then
    errors := errors + 'Le mot de passe doit faire au moins 8 caractères.<br>';

  // Afficher les erreurs ou valider
  if errors <> '' then
  begin
    errorDiv.innerHTML := '<div style="color: red;">' + errors + '</div>';
  end
  else
  begin
    errorDiv.innerHTML := '<div style="color: green;">✓ Formulaire valide!</div>';
    console.log('Email: ' + email);
    console.log('Password: ' + password);
    // Ici, vous pourriez envoyer les données à un serveur
  end;
end;

procedure SetupForm;  
var
  form: TJSHTMLFormElement;
begin
  form := TJSHTMLFormElement(document.getElementById('loginForm'));
  form.addEventListener('submit', @ValidateAndSubmit);
end;

begin
  window.addEventListener('DOMContentLoaded', @SetupForm);
end.
```

**HTML du formulaire** :

```html
<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8">
    <title>Formulaire avec validation</title>
    <style>
        form {
            max-width: 400px;
            margin: 50px auto;
            padding: 20px;
            border: 1px solid #ccc;
            border-radius: 5px;
        }
        input {
            width: 100%;
            padding: 10px;
            margin: 10px 0;
            box-sizing: border-box;
        }
        button {
            width: 100%;
            padding: 10px;
            background-color: #4CAF50;
            color: white;
            border: none;
            cursor: pointer;
        }
        button:hover {
            background-color: #45a049;
        }
    </style>
</head>
<body>
    <form id="loginForm">
        <h2>Connexion</h2>

        <label for="email">Email:</label>
        <input id="email" type="text" placeholder="email@example.com">

        <label for="password">Mot de passe:</label>
        <input id="password" type="password" placeholder="••••••••">

        <button type="submit">Se connecter</button>

        <div id="errors"></div>
    </form>

    <script src="rtl.js"></script>
    <script src="main.js"></script>
</body>
</html>
```

---

## Cas pratiques

### Cas 1 : Calculatrice interactive

**calculatrice.pas** :

```pascal
program Calculatrice;

{$mode objfpc}

uses
  JS, Web, SysUtils;

var
  display: TJSHTMLInputElement;
  currentValue: String = '0';
  previousValue: String = '';
  operation: String = '';

procedure UpdateDisplay;  
begin
  display.value := currentValue;
end;

procedure OnNumberClick(event: TJSMouseEvent);  
var
  button: TJSHTMLButtonElement;
  digit: String;
begin
  button := TJSHTMLButtonElement(event.target);
  digit := button.textContent;

  if currentValue = '0' then
    currentValue := digit
  else
    currentValue := currentValue + digit;

  UpdateDisplay;
end;

procedure OnOperationClick(event: TJSMouseEvent);  
var
  button: TJSHTMLButtonElement;
begin
  button := TJSHTMLButtonElement(event.target);
  operation := button.textContent;
  previousValue := currentValue;
  currentValue := '0';
end;

procedure OnEqualsClick(event: TJSMouseEvent);  
var
  a, b, result: Double;
begin
  a := StrToFloatDef(previousValue, 0);
  b := StrToFloatDef(currentValue, 0);

  case operation of
    '+': result := a + b;
    '-': result := a - b;
    '×': result := a * b;
    '÷': if b <> 0 then result := a / b else result := 0;
  else
    result := b;
  end;

  currentValue := FloatToStr(result);
  previousValue := '';
  operation := '';
  UpdateDisplay;
end;

procedure OnClearClick(event: TJSMouseEvent);  
begin
  currentValue := '0';
  previousValue := '';
  operation := '';
  UpdateDisplay;
end;

procedure SetupCalculator;  
var
  buttons: TJSNodeList;
  i: Integer;
  button: TJSHTMLButtonElement;
begin
  display := TJSHTMLInputElement(document.getElementById('display'));

  // Boutons numériques
  buttons := document.querySelectorAll('.number');
  for i := 0 to buttons.length - 1 do
  begin
    button := TJSHTMLButtonElement(buttons[i]);
    button.addEventListener('click', @OnNumberClick);
  end;

  // Boutons d'opération
  buttons := document.querySelectorAll('.operation');
  for i := 0 to buttons.length - 1 do
  begin
    button := TJSHTMLButtonElement(buttons[i]);
    button.addEventListener('click', @OnOperationClick);
  end;

  // Bouton égal
  TJSHTMLElement(document.getElementById('equals')).addEventListener('click', @OnEqualsClick);

  // Bouton clear
  TJSHTMLElement(document.getElementById('clear')).addEventListener('click', @OnClearClick);

  UpdateDisplay;
end;

begin
  window.addEventListener('DOMContentLoaded', @SetupCalculator);
end.
```

**HTML de la calculatrice** :

```html
<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8">
    <title>Calculatrice Pascal</title>
    <style>
        body {
            font-family: Arial, sans-serif;
            display: flex;
            justify-content: center;
            align-items: center;
            min-height: 100vh;
            margin: 0;
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        }

        .calculator {
            background: white;
            padding: 20px;
            border-radius: 10px;
            box-shadow: 0 10px 30px rgba(0,0,0,0.3);
        }

        #display {
            width: 100%;
            height: 60px;
            font-size: 2em;
            text-align: right;
            border: 2px solid #ddd;
            border-radius: 5px;
            padding: 10px;
            box-sizing: border-box;
            margin-bottom: 10px;
        }

        .buttons {
            display: grid;
            grid-template-columns: repeat(4, 1fr);
            gap: 10px;
        }

        button {
            height: 60px;
            font-size: 1.5em;
            border: none;
            border-radius: 5px;
            cursor: pointer;
            transition: all 0.3s;
        }

        button:hover {
            transform: scale(1.05);
        }

        .number {
            background: #f0f0f0;
        }

        .operation {
            background: #ff9500;
            color: white;
        }

        #equals {
            background: #4CAF50;
            color: white;
            grid-column: span 2;
        }

        #clear {
            background: #f44336;
            color: white;
            grid-column: span 2;
        }
    </style>
</head>
<body>
    <div class="calculator">
        <input id="display" type="text" readonly>

        <div class="buttons">
            <button class="number">7</button>
            <button class="number">8</button>
            <button class="number">9</button>
            <button class="operation">÷</button>

            <button class="number">4</button>
            <button class="number">5</button>
            <button class="number">6</button>
            <button class="operation">×</button>

            <button class="number">1</button>
            <button class="number">2</button>
            <button class="number">3</button>
            <button class="operation">-</button>

            <button class="number">0</button>
            <button class="number">.</button>
            <button class="operation">+</button>
            <button id="equals">=</button>

            <button id="clear">C</button>
        </div>
    </div>

    <script src="rtl.js"></script>
    <script src="calculatrice.js"></script>
</body>
</html>
```

**Compilation** :

```bash
pas2js -Jirtl.js -Jc calculatrice.pas
```

---

### Cas 2 : Application de dessin sur Canvas

**dessin.pas** :

```pascal
program ApplicationDessin;

{$mode objfpc}

uses
  JS, Web;

var
  canvas: TJSHTMLCanvasElement;
  ctx: TJSCanvasRenderingContext2D;
  isDrawing: Boolean = False;
  lastX, lastY: Integer;
  currentColor: String = '#000000';
  currentSize: Integer = 5;

procedure StartDrawing(event: TJSMouseEvent);  
var
  rect: TJSClientRect;
begin
  isDrawing := True;
  rect := canvas.getBoundingClientRect();
  lastX := Trunc(event.clientX - rect.left);
  lastY := Trunc(event.clientY - rect.top);
end;

procedure Draw(event: TJSMouseEvent);  
var
  rect: TJSClientRect;
  currentX, currentY: Integer;
begin
  if not isDrawing then Exit;

  rect := canvas.getBoundingClientRect();
  currentX := Trunc(event.clientX - rect.left);
  currentY := Trunc(event.clientY - rect.top);

  // Dessiner une ligne
  ctx.beginPath();
  ctx.moveTo(lastX, lastY);
  ctx.lineTo(currentX, currentY);
  ctx.strokeStyle := currentColor;
  ctx.lineWidth := currentSize;
  ctx.lineCap := 'round';
  ctx.stroke();

  lastX := currentX;
  lastY := currentY;
end;

procedure StopDrawing(event: TJSMouseEvent);  
begin
  isDrawing := False;
end;

procedure OnColorChange(event: TJSEvent);  
var
  input: TJSHTMLInputElement;
begin
  input := TJSHTMLInputElement(event.target);
  currentColor := input.value;
end;

procedure OnSizeChange(event: TJSEvent);  
var
  input: TJSHTMLInputElement;
  sizeDisplay: TJSHTMLElement;
begin
  input := TJSHTMLInputElement(event.target);
  currentSize := StrToInt(input.value);

  sizeDisplay := TJSHTMLElement(document.getElementById('sizeValue'));
  sizeDisplay.textContent := IntToStr(currentSize) + 'px';
end;

procedure ClearCanvas(event: TJSMouseEvent);  
begin
  ctx.clearRect(0, 0, canvas.width, canvas.height);

  // Remplir avec du blanc
  ctx.fillStyle := '#ffffff';
  ctx.fillRect(0, 0, canvas.width, canvas.height);
end;

procedure SaveImage(event: TJSMouseEvent);  
var
  dataURL: String;
  link: TJSHTMLAnchorElement;
begin
  // Convertir le canvas en image
  dataURL := canvas.toDataURL('image/png');

  // Créer un lien de téléchargement
  link := TJSHTMLAnchorElement(document.createElement('a'));
  link.download := 'dessin.png';
  link.href := dataURL;
  link.click();
end;

procedure SetupDrawingApp;  
var
  colorInput: TJSHTMLInputElement;
  sizeInput: TJSHTMLInputElement;
  clearBtn: TJSHTMLButtonElement;
  saveBtn: TJSHTMLButtonElement;
begin
  // Récupérer le canvas
  canvas := TJSHTMLCanvasElement(document.getElementById('drawingCanvas'));
  ctx := TJSCanvasRenderingContext2D(canvas.getContext('2d'));

  // Initialiser avec fond blanc
  ctx.fillStyle := '#ffffff';
  ctx.fillRect(0, 0, canvas.width, canvas.height);

  // Événements de dessin
  canvas.addEventListener('mousedown', @StartDrawing);
  canvas.addEventListener('mousemove', @Draw);
  canvas.addEventListener('mouseup', @StopDrawing);
  canvas.addEventListener('mouseout', @StopDrawing);

  // Contrôles
  colorInput := TJSHTMLInputElement(document.getElementById('colorPicker'));
  colorInput.addEventListener('change', @OnColorChange);

  sizeInput := TJSHTMLInputElement(document.getElementById('sizePicker'));
  sizeInput.addEventListener('input', @OnSizeChange);

  clearBtn := TJSHTMLButtonElement(document.getElementById('clearBtn'));
  clearBtn.addEventListener('click', @ClearCanvas);

  saveBtn := TJSHTMLButtonElement(document.getElementById('saveBtn'));
  saveBtn.addEventListener('click', @SaveImage);
end;

begin
  window.addEventListener('DOMContentLoaded', @SetupDrawingApp);
end.
```

**HTML de l'application de dessin** :

```html
<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8">
    <title>Application de Dessin Pascal</title>
    <style>
        body {
            font-family: Arial, sans-serif;
            margin: 0;
            padding: 20px;
            background: #f5f5f5;
        }

        .container {
            max-width: 900px;
            margin: 0 auto;
            background: white;
            padding: 20px;
            border-radius: 10px;
            box-shadow: 0 2px 10px rgba(0,0,0,0.1);
        }

        h1 {
            text-align: center;
            color: #333;
        }

        .controls {
            display: flex;
            gap: 20px;
            align-items: center;
            justify-content: center;
            margin-bottom: 20px;
            padding: 15px;
            background: #f9f9f9;
            border-radius: 5px;
        }

        .control-group {
            display: flex;
            align-items: center;
            gap: 10px;
        }

        #drawingCanvas {
            border: 2px solid #ddd;
            cursor: crosshair;
            display: block;
            margin: 0 auto;
            background: white;
        }

        button {
            padding: 10px 20px;
            border: none;
            border-radius: 5px;
            cursor: pointer;
            font-size: 1em;
            transition: all 0.3s;
        }

        #clearBtn {
            background: #f44336;
            color: white;
        }

        #clearBtn:hover {
            background: #da190b;
        }

        #saveBtn {
            background: #4CAF50;
            color: white;
        }

        #saveBtn:hover {
            background: #45a049;
        }

        input[type="color"] {
            width: 50px;
            height: 40px;
            border: none;
            cursor: pointer;
        }

        input[type="range"] {
            width: 150px;
        }
    </style>
</head>
<body>
    <div class="container">
        <h1>🎨 Application de Dessin</h1>

        <div class="controls">
            <div class="control-group">
                <label>Couleur:</label>
                <input id="colorPicker" type="color" value="#000000">
            </div>

            <div class="control-group">
                <label>Taille:</label>
                <input id="sizePicker" type="range" min="1" max="50" value="5">
                <span id="sizeValue">5px</span>
            </div>

            <button id="clearBtn">🗑️ Effacer</button>
            <button id="saveBtn">💾 Sauvegarder</button>
        </div>

        <canvas id="drawingCanvas" width="800" height="600"></canvas>
    </div>

    <script src="rtl.js"></script>
    <script src="dessin.js"></script>
</body>
</html>
```

---

### Cas 3 : Jeu simple - Snake

**snake.pas** :

```pascal
program JeuSnake;

{$mode objfpc}

uses
  JS, Web, SysUtils;

const
  GRID_SIZE = 20;
  CELL_SIZE = 20;
  CANVAS_WIDTH = GRID_SIZE * CELL_SIZE;
  CANVAS_HEIGHT = GRID_SIZE * CELL_SIZE;

type
  TPoint = record
    X, Y: Integer;
  end;

  TDirection = (dUp, dDown, dLeft, dRight);

var
  canvas: TJSHTMLCanvasElement;
  ctx: TJSCanvasRenderingContext2D;
  snake: array of TPoint;
  food: TPoint;
  direction: TDirection;
  gameOver: Boolean;
  score: Integer;
  gameTimer: Integer;

procedure InitGame;  
begin
  // Initialiser le serpent au centre
  SetLength(snake, 3);
  snake[0].X := 10; snake[0].Y := 10;
  snake[1].X := 9;  snake[1].Y := 10;
  snake[2].X := 8;  snake[2].Y := 10;

  // Direction initiale
  direction := dRight;

  // Placer la nourriture
  food.X := Random(GRID_SIZE);
  food.Y := Random(GRID_SIZE);

  gameOver := False;
  score := 0;

  UpdateScore;
end;

procedure UpdateScore;  
var
  scoreElement: TJSHTMLElement;
begin
  scoreElement := TJSHTMLElement(document.getElementById('score'));
  scoreElement.textContent := 'Score: ' + IntToStr(score);
end;

procedure DrawCell(x, y: Integer; color: String);  
begin
  ctx.fillStyle := color;
  ctx.fillRect(x * CELL_SIZE, y * CELL_SIZE, CELL_SIZE - 2, CELL_SIZE - 2);
end;

procedure Draw;  
var
  i: Integer;
begin
  // Effacer le canvas
  ctx.fillStyle := '#f0f0f0';
  ctx.fillRect(0, 0, CANVAS_WIDTH, CANVAS_HEIGHT);

  // Dessiner la grille
  ctx.strokeStyle := '#ddd';
  ctx.lineWidth := 1;
  for i := 0 to GRID_SIZE do
  begin
    ctx.beginPath();
    ctx.moveTo(i * CELL_SIZE, 0);
    ctx.lineTo(i * CELL_SIZE, CANVAS_HEIGHT);
    ctx.stroke();

    ctx.beginPath();
    ctx.moveTo(0, i * CELL_SIZE);
    ctx.lineTo(CANVAS_WIDTH, i * CELL_SIZE);
    ctx.stroke();
  end;

  // Dessiner le serpent
  for i := 0 to High(snake) do
  begin
    if i = 0 then
      DrawCell(snake[i].X, snake[i].Y, '#4CAF50')  // Tête
    else
      DrawCell(snake[i].X, snake[i].Y, '#8BC34A');  // Corps
  end;

  // Dessiner la nourriture
  DrawCell(food.X, food.Y, '#f44336');
end;

procedure CheckCollision;  
var
  i: Integer;
  head: TPoint;
begin
  head := snake[0];

  // Collision avec les murs
  if (head.X < 0) or (head.X >= GRID_SIZE) or
     (head.Y < 0) or (head.Y >= GRID_SIZE) then
  begin
    gameOver := True;
    Exit;
  end;

  // Collision avec soi-même
  for i := 1 to High(snake) do
  begin
    if (head.X = snake[i].X) and (head.Y = snake[i].Y) then
    begin
      gameOver := True;
      Exit;
    end;
  end;
end;

procedure Update;  
var
  i: Integer;
  newHead: TPoint;
  ateFood: Boolean;
begin
  if gameOver then Exit;

  // Calculer la nouvelle position de la tête
  newHead := snake[0];

  case direction of
    dUp:    Dec(newHead.Y);
    dDown:  Inc(newHead.Y);
    dLeft:  Dec(newHead.X);
    dRight: Inc(newHead.X);
  end;

  // Vérifier si on mange la nourriture
  ateFood := (newHead.X = food.X) and (newHead.Y = food.Y);

  // Déplacer le serpent
  for i := High(snake) downto 1 do
    snake[i] := snake[i - 1];

  snake[0] := newHead;

  // Si on a mangé, agrandir le serpent
  if ateFood then
  begin
    SetLength(snake, Length(snake) + 1);
    snake[High(snake)] := snake[High(snake) - 1];

    // Nouvelle nourriture
    food.X := Random(GRID_SIZE);
    food.Y := Random(GRID_SIZE);

    Inc(score, 10);
    UpdateScore;
  end;

  CheckCollision;

  if gameOver then
  begin
    window.clearInterval(gameTimer);
    window.alert('Game Over! Score: ' + IntToStr(score));
  end;
end;

procedure GameLoop;  
begin
  Update;
  Draw;
end;

procedure OnKeyPress(event: TJSKeyboardEvent);  
begin
  case event.key of
    'ArrowUp':    if direction <> dDown then direction := dUp;
    'ArrowDown':  if direction <> dUp then direction := dDown;
    'ArrowLeft':  if direction <> dRight then direction := dLeft;
    'ArrowRight': if direction <> dLeft then direction := dRight;
  end;

  event.preventDefault();
end;

procedure StartNewGame(event: TJSMouseEvent);  
begin
  if gameTimer <> 0 then
    window.clearInterval(gameTimer);

  InitGame;
  Draw;
  gameTimer := window.setInterval(@GameLoop, 150);
end;

procedure SetupGame;  
var
  startBtn: TJSHTMLButtonElement;
begin
  canvas := TJSHTMLCanvasElement(document.getElementById('gameCanvas'));
  ctx := TJSCanvasRenderingContext2D(canvas.getContext('2d'));

  // Événements clavier
  document.addEventListener('keydown', @OnKeyPress);

  // Bouton start
  startBtn := TJSHTMLButtonElement(document.getElementById('startBtn'));
  startBtn.addEventListener('click', @StartNewGame);

  // Dessiner l'état initial
  InitGame;
  Draw;
end;

begin
  window.addEventListener('DOMContentLoaded', @SetupGame);
end.
```

**HTML du jeu Snake** :

```html
<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8">
    <title>Snake Game - Pascal</title>
    <style>
        body {
            font-family: Arial, sans-serif;
            display: flex;
            justify-content: center;
            align-items: center;
            min-height: 100vh;
            margin: 0;
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        }

        .game-container {
            background: white;
            padding: 20px;
            border-radius: 10px;
            box-shadow: 0 10px 30px rgba(0,0,0,0.3);
            text-align: center;
        }

        h1 {
            margin: 0 0 20px 0;
            color: #333;
        }

        #gameCanvas {
            border: 2px solid #333;
            display: block;
            margin: 0 auto;
        }

        .controls {
            margin-top: 20px;
            display: flex;
            justify-content: space-between;
            align-items: center;
        }

        #score {
            font-size: 1.5em;
            font-weight: bold;
            color: #4CAF50;
        }

        #startBtn {
            padding: 10px 30px;
            font-size: 1.2em;
            background: #4CAF50;
            color: white;
            border: none;
            border-radius: 5px;
            cursor: pointer;
            transition: all 0.3s;
        }

        #startBtn:hover {
            background: #45a049;
            transform: scale(1.05);
        }

        .instructions {
            margin-top: 20px;
            padding: 15px;
            background: #f9f9f9;
            border-radius: 5px;
            text-align: left;
        }

        .instructions h3 {
            margin-top: 0;
        }
    </style>
</head>
<body>
    <div class="game-container">
        <h1>🐍 Snake Game</h1>

        <canvas id="gameCanvas" width="400" height="400"></canvas>

        <div class="controls">
            <div id="score">Score: 0</div>
            <button id="startBtn">Nouvelle Partie</button>
        </div>

        <div class="instructions">
            <h3>Instructions:</h3>
            <ul>
                <li>Utilisez les <strong>flèches du clavier</strong> pour diriger le serpent</li>
                <li>Mangez les <span style="color: #f44336;">●</span> rouges pour grandir</li>
                <li>Évitez les murs et votre propre corps</li>
            </ul>
        </div>
    </div>

    <script src="rtl.js"></script>
    <script src="snake.js"></script>
</body>
</html>
```

---

## Performance et optimisation

### Comparaison de performance

**Test : Calcul de nombres premiers**

**JavaScript pur** :

```javascript
function isPrime(n) {
    if (n <= 1) return false;
    if (n <= 3) return true;
    if (n % 2 === 0 || n % 3 === 0) return false;

    for (let i = 5; i * i <= n; i += 6) {
        if (n % i === 0 || n % (i + 2) === 0) return false;
    }
    return true;
}

function countPrimes(max) {
    let count = 0;
    for (let i = 2; i <= max; i++) {
        if (isPrime(i)) count++;
    }
    return count;
}

// Test
console.time('JavaScript');  
const result = countPrimes(100000);  
console.timeEnd('JavaScript');  
console.log('Nombres premiers trouvés:', result);
```

**Pascal (Pas2JS)** :

```pascal
program PrimesTest;

{$mode objfpc}

uses
  JS, Web;

function IsPrime(n: Integer): Boolean;  
var
  i: Integer;
begin
  if n <= 1 then Exit(False);
  if n <= 3 then Exit(True);
  if (n mod 2 = 0) or (n mod 3 = 0) then Exit(False);

  i := 5;
  while i * i <= n do
  begin
    if (n mod i = 0) or (n mod (i + 2) = 0) then
      Exit(False);
    Inc(i, 6);
  end;

  Result := True;
end;

function CountPrimes(max: Integer): Integer;  
var
  i, count: Integer;
begin
  count := 0;
  for i := 2 to max do
  begin
    if IsPrime(i) then
      Inc(count);
  end;
  Result := count;
end;

procedure RunTest;  
var
  startTime, endTime: Double;
  result: Integer;
begin
  console.log('Test de performance Pascal');

  startTime := window.performance.now();
  result := CountPrimes(100000);
  endTime := window.performance.now();

  console.log('Temps Pascal: ' + FloatToStr(endTime - startTime) + ' ms');
  console.log('Nombres premiers trouvés: ' + IntToStr(result));
end;

begin
  window.addEventListener('DOMContentLoaded', @RunTest);
end.
```

**Résultats typiques** :

```
JavaScript pur : ~2500 ms  
Pas2JS        : ~2800 ms  
Différence    : Similaire (le code est transpilé en JS)
```

**Note importante** : Pas2JS génère du JavaScript, donc les performances sont similaires. Pour de vraies gains de performance, il faudrait WebAssembly natif.

### Optimisations Pas2JS

#### 1. Éviter les allocations inutiles

```pascal
// ✗ LENT - Crée beaucoup de chaînes temporaires
var
  result: String;
  i: Integer;
begin
  result := '';
  for i := 1 to 1000 do
    result := result + IntToStr(i) + ', ';
end;

// ✓ RAPIDE - Utilise un tableau
var
  parts: array of String;
  i: Integer;
begin
  SetLength(parts, 1000);
  for i := 1 to 1000 do
    parts[i-1] := IntToStr(i);
  result := parts.join(', ');
end;
```

#### 2. Minimiser les accès DOM

```pascal
// ✗ LENT - Accès DOM dans la boucle
for i := 1 to 100 do  
begin
  element := TJSHTMLElement(document.getElementById('item' + IntToStr(i)));
  element.textContent := 'Item ' + IntToStr(i);
end;

// ✓ RAPIDE - Construire le HTML en mémoire
var
  html: String;
begin
  html := '';
  for i := 1 to 100 do
    html := html + '<div id="item' + IntToStr(i) + '">Item ' + IntToStr(i) + '</div>';

  container.innerHTML := html;
end;
```

#### 3. Utiliser requestAnimationFrame pour les animations

```pascal
var
  animationId: Integer;
  position: Double = 0;

procedure Animate(timestamp: Double);  
var
  element: TJSHTMLElement;
begin
  position := position + 2;

  if position > 500 then
    position := 0;

  element := TJSHTMLElement(document.getElementById('box'));
  element.style.setProperty('left', FloatToStr(position) + 'px');

  // Continuer l'animation
  animationId := window.requestAnimationFrame(@Animate);
end;

procedure StartAnimation(event: TJSMouseEvent);  
begin
  animationId := window.requestAnimationFrame(@Animate);
end;
```

### Techniques avancées

#### Utiliser les Web Workers

Les Web Workers permettent d'exécuter du code en arrière-plan sans bloquer l'interface.

**worker.pas** :

```pascal
program Worker;

{$mode objfpc}

uses
  JS, Web;

// Fonction de calcul intensif
function CalculateFactorial(n: Integer): Double;  
var
  i: Integer;
  result: Double;
begin
  result := 1;
  for i := 2 to n do
    result := result * i;
  Result := result;
end;

procedure OnMessage(event: TJSMessageEvent);  
var
  n: Integer;
  result: Double;
  response: TJSObject;
begin
  n := Integer(event.data);
  result := CalculateFactorial(n);

  response := TJSObject.new;
  response['result'] := result;

  postMessage(response);
end;

begin
  self.addEventListener('message', @OnMessage);
end.
```

**main.pas (utilise le worker)** :

```pascal
var
  worker: TJSWorker;

procedure OnWorkerMessage(event: TJSMessageEvent);  
var
  result: Double;
begin
  result := Double(TJSObject(event.data)['result']);
  console.log('Résultat du worker: ' + FloatToStr(result));

  TJSHTMLElement(document.getElementById('result')).textContent :=
    'Résultat: ' + FloatToStr(result);
end;

procedure StartCalculation(event: TJSMouseEvent);  
var
  n: Integer;
begin
  n := 1000;

  // Envoyer le travail au worker
  worker.postMessage(n);

  console.log('Calcul démarré en arrière-plan...');
end;

procedure SetupWorker;  
begin
  worker := TJSWorker.new('worker.js');
  worker.addEventListener('message', @OnWorkerMessage);

  TJSHTMLElement(document.getElementById('calcBtn'))
    .addEventListener('click', @StartCalculation);
end;

begin
  window.addEventListener('DOMContentLoaded', @SetupWorker);
end;
```

---

## Déploiement d'applications web

### Structure d'un projet prêt pour la production

```
my_web_app/
├── src/
│   ├── main.pas
│   ├── utils.pas
│   └── types.pas
├── public/
│   ├── index.html
│   ├── styles.css
│   ├── images/
│   │   ├── logo.png
│   │   └── favicon.ico
│   └── fonts/
├── dist/                  # Fichiers de production
│   ├── index.html
│   ├── app.js            # JavaScript compilé
│   ├── rtl.js            # Runtime
│   ├── styles.min.css    # CSS minifié
│   └── assets/
│       ├── images/
│       └── fonts/
├── scripts/
│   ├── build.sh          # Script de build
│   └── deploy.sh         # Script de déploiement
├── package.json          # Configuration npm (optionnel)
└── README.md
```

### Script de build automatisé

**build.sh** :

```bash
#!/bin/bash
set -e

echo "=== Build de l'application Pas2JS ==="

# Variables
SRC_DIR="src"  
PUBLIC_DIR="public"  
DIST_DIR="dist"

# Nettoyer le répertoire de distribution
echo "Nettoyage..."  
rm -rf $DIST_DIR  
mkdir -p $DIST_DIR/assets/{images,fonts}

# Compiler le code Pascal
echo "Compilation du code Pascal..."  
pas2js -Jirtl.js -Jc -O1 \
    -Fu$SRC_DIR \
    -FE$DIST_DIR \
    $SRC_DIR/main.pas

if [ $? -ne 0 ]; then
    echo "✗ Erreur de compilation"
    exit 1
fi

echo "✓ Compilation réussie"

# Copier les fichiers HTML
echo "Copie des fichiers HTML..."  
cp $PUBLIC_DIR/*.html $DIST_DIR/

# Minifier le CSS (nécessite csso ou similaire)
echo "Traitement du CSS..."  
if command -v csso &> /dev/null; then
    csso $PUBLIC_DIR/styles.css -o $DIST_DIR/styles.min.css
    echo "✓ CSS minifié"
else
    cp $PUBLIC_DIR/styles.css $DIST_DIR/
    echo "⚠ csso non trouvé, CSS copié sans minification"
fi

# Copier les assets
echo "Copie des assets..."  
cp -r $PUBLIC_DIR/images/* $DIST_DIR/assets/images/ 2>/dev/null || :  
cp -r $PUBLIC_DIR/fonts/* $DIST_DIR/assets/fonts/ 2>/dev/null || :

# Optimiser les images (optionnel, nécessite imagemagick)
if command -v mogrify &> /dev/null; then
    echo "Optimisation des images..."
    mogrify -strip -quality 85 $DIST_DIR/assets/images/*.{jpg,jpeg,png} 2>/dev/null || :
    echo "✓ Images optimisées"
fi

# Créer un fichier de version
echo "Création du fichier de version..."  
date "+%Y-%m-%d %H:%M:%S" > $DIST_DIR/version.txt  
git rev-parse --short HEAD >> $DIST_DIR/version.txt 2>/dev/null || echo "no-git" >> $DIST_DIR/version.txt

echo ""  
echo "=== Build terminé avec succès ==="  
echo "Les fichiers sont dans : $DIST_DIR/"  
echo ""  
echo "Pour tester localement :"  
echo "  cd $DIST_DIR && python3 -m http.server 8000"  
echo "  Puis ouvrez : http://localhost:8000"
```

**Rendre le script exécutable** :

```bash
chmod +x scripts/build.sh
./scripts/build.sh
```

### Optimisation pour la production

#### 1. Minification du JavaScript

**Installation de terser (minifieur JavaScript)** :

```bash
npm install -g terser
```

**Ajout au script de build** :

```bash
# Minifier le JavaScript généré
echo "Minification du JavaScript..."  
if command -v terser &> /dev/null; then
    terser $DIST_DIR/main.js \
        --compress \
        --mangle \
        --output $DIST_DIR/app.min.js

    # Remplacer dans le HTML
    sed -i 's/main.js/app.min.js/g' $DIST_DIR/index.html

    echo "✓ JavaScript minifié"
else
    echo "⚠ terser non trouvé, JavaScript non minifié"
fi
```

#### 2. Compression Gzip

**Créer des versions .gz pré-compressées** :

```bash
# Compresser les fichiers pour les serveurs qui supportent gzip
echo "Compression Gzip..."  
gzip -9 -k $DIST_DIR/*.js  
gzip -9 -k $DIST_DIR/*.css  
gzip -9 -k $DIST_DIR/*.html

echo "✓ Fichiers compressés créés"
```

#### 3. Cache-busting avec hash

**Script pour ajouter des hash aux fichiers** :

```bash
#!/bin/bash

# Générer un hash basé sur le contenu
HASH=$(md5sum $DIST_DIR/app.min.js | cut -d' ' -f1 | cut -c1-8)

# Renommer avec le hash
mv $DIST_DIR/app.min.js $DIST_DIR/app.$HASH.min.js

# Mettre à jour les références dans le HTML
sed -i "s/app.min.js/app.$HASH.min.js/g" $DIST_DIR/index.html

echo "✓ Cache-busting appliqué: app.$HASH.min.js"
```

### Configuration de serveur web

#### Nginx

**nginx.conf** :

```nginx
server {
    listen 80;
    server_name myapp.example.com;

    root /var/www/myapp/dist;
    index index.html;

    # Gzip compression
    gzip on;
    gzip_types text/plain text/css application/json application/javascript text/xml application/xml application/xml+rss text/javascript;
    gzip_min_length 1000;

    # Cache des assets
    location ~* \.(js|css|png|jpg|jpeg|gif|ico|svg|woff|woff2|ttf|eot)$ {
        expires 1y;
        add_header Cache-Control "public, immutable";
    }

    # Pas de cache pour le HTML
    location ~* \.html$ {
        expires -1;
        add_header Cache-Control "no-cache, no-store, must-revalidate";
    }

    # Servir le fichier gzip si disponible
    location ~ ^(.+)$ {
        gzip_static on;
        try_files $1.gz $1 =404;
    }

    # Fallback pour les Single Page Applications
    location / {
        try_files $uri $uri/ /index.html;
    }
}
```

#### Apache

**.htaccess** :

```apache
# Activer la compression
<IfModule mod_deflate.c>
    AddOutputFilterByType DEFLATE text/html text/plain text/css application/javascript application/json
</IfModule>

# Cache des assets
<IfModule mod_expires.c>
    ExpiresActive On

    # Images
    ExpiresByType image/jpg "access plus 1 year"
    ExpiresByType image/jpeg "access plus 1 year"
    ExpiresByType image/png "access plus 1 year"
    ExpiresByType image/gif "access plus 1 year"
    ExpiresByType image/svg+xml "access plus 1 year"

    # CSS et JavaScript
    ExpiresByType text/css "access plus 1 year"
    ExpiresByType application/javascript "access plus 1 year"

    # Fonts
    ExpiresByType font/woff "access plus 1 year"
    ExpiresByType font/woff2 "access plus 1 year"

    # HTML - pas de cache
    ExpiresByType text/html "access plus 0 seconds"
</IfModule>

# Fallback pour SPA
<IfModule mod_rewrite.c>
    RewriteEngine On
    RewriteBase /
    RewriteRule ^index\.html$ - [L]
    RewriteCond %{REQUEST_FILENAME} !-f
    RewriteCond %{REQUEST_FILENAME} !-d
    RewriteRule . /index.html [L]
</IfModule>
```

### Déploiement automatisé

#### Script de déploiement

**deploy.sh** :

```bash
#!/bin/bash
set -e

echo "=== Déploiement de l'application ==="

# Variables
SERVER="user@myapp.example.com"  
REMOTE_DIR="/var/www/myapp"  
LOCAL_DIST="dist"

# 1. Build
echo "1. Build de l'application..."
./scripts/build.sh

# 2. Backup de l'ancienne version
echo "2. Backup de l'ancienne version..."  
ssh $SERVER "cd $REMOTE_DIR && tar -czf backup-$(date +%Y%m%d-%H%M%S).tar.gz dist/"

# 3. Transfert des fichiers
echo "3. Transfert des fichiers..."  
rsync -avz --delete \
    $LOCAL_DIST/ \
    $SERVER:$REMOTE_DIR/dist/

# 4. Redémarrage du serveur (si nécessaire)
echo "4. Redémarrage du serveur..."  
ssh $SERVER "sudo systemctl reload nginx"

# 5. Test de santé
echo "5. Test de santé..."  
HTTP_CODE=$(curl -s -o /dev/null -w "%{http_code}" https://myapp.example.com)

if [ $HTTP_CODE -eq 200 ]; then
    echo "✓ Déploiement réussi! (HTTP $HTTP_CODE)"
else
    echo "✗ Erreur: HTTP $HTTP_CODE"
    exit 1
fi

echo ""  
echo "=== Déploiement terminé ==="
```

#### GitHub Actions pour CI/CD

**.github/workflows/deploy.yml** :

```yaml
name: Build and Deploy

on:
  push:
    branches:
      - main

jobs:
  build-and-deploy:
    runs-on: ubuntu-latest

    steps:
    - name: Checkout code
      uses: actions/checkout@v3

    - name: Install FreePascal
      run: |
        sudo apt-get update
        sudo apt-get install -y fpc

    - name: Install Pas2JS
      run: |
        cd /tmp
        git clone https://github.com/fpc/pas2js.git
        cd pas2js
        make
        sudo make install

    - name: Install dependencies
      run: |
        npm install -g terser csso

    - name: Build
      run: |
        chmod +x scripts/build.sh
        ./scripts/build.sh

    - name: Deploy to server
      uses: easingthemes/ssh-deploy@v2
      with:
        SSH_PRIVATE_KEY: ${{ secrets.SSH_PRIVATE_KEY }}
        REMOTE_HOST: ${{ secrets.REMOTE_HOST }}
        REMOTE_USER: ${{ secrets.REMOTE_USER }}
        SOURCE: "dist/"
        TARGET: "/var/www/myapp/dist/"

    - name: Health check
      run: |
        curl -f https://myapp.example.com || exit 1
```

### Hébergement gratuit

#### GitHub Pages

**Configuration pour GitHub Pages** :

1. Créer une branche `gh-pages`
2. Copier les fichiers de `dist/` dans cette branche
3. Activer GitHub Pages dans les paramètres du repo

**Script automatique** :

```bash
#!/bin/bash

# Build
./scripts/build.sh

# Pousser vers gh-pages
cd dist  
git init  
git add .  
git commit -m "Deploy to GitHub Pages"  
git branch -M gh-pages  
git remote add origin https://github.com/username/repo.git  
git push -f origin gh-pages

echo "✓ Déployé sur: https://username.github.io/repo/"
```

#### Netlify

**netlify.toml** :

```toml
[build]
  command = "./scripts/build.sh"
  publish = "dist"

[[redirects]]
  from = "/*"
  to = "/index.html"
  status = 200

[build.environment]
  NODE_VERSION = "18"
```

**Déploiement** :

```bash
# Installer Netlify CLI
npm install -g netlify-cli

# Déployer
netlify deploy --prod --dir=dist
```

#### Vercel

**vercel.json** :

```json
{
  "buildCommand": "./scripts/build.sh",
  "outputDirectory": "dist",
  "routes": [
    {
      "src": "/(.*)",
      "dest": "/$1"
    }
  ]
}
```

---

## Bonnes pratiques et astuces

### 1. Organisation du code

**Séparer en modules** :

```pascal
// types.pas - Types partagés
unit Types;

interface

type
  TPoint = record
    X, Y: Integer;
  end;

  TCallback = reference to procedure(data: JSValue);

implementation

end.
```

```pascal
// utils.pas - Fonctions utilitaires
unit Utils;

interface

uses
  JS, Web;

function GetElement(id: String): TJSHTMLElement;  
procedure ShowNotification(message: String);

implementation

function GetElement(id: String): TJSHTMLElement;  
begin
  Result := TJSHTMLElement(document.getElementById(id));
end;

procedure ShowNotification(message: String);  
var
  notification: TJSHTMLElement;
begin
  notification := TJSHTMLElement(document.createElement('div'));
  notification.className := 'notification';
  notification.textContent := message;
  document.body.appendChild(notification);

  // Auto-destruction après 3 secondes
  window.setTimeout(procedure
  begin
    notification.remove();
  end, 3000);
end;

end.
```

```pascal
// main.pas - Programme principal
program Main;

{$mode objfpc}

uses
  JS, Web, Types, Utils;

procedure OnButtonClick(event: TJSMouseEvent);  
begin
  ShowNotification('Bouton cliqué!');
end;

procedure Init;  
begin
  GetElement('myButton').addEventListener('click', @OnButtonClick);
end;

begin
  window.addEventListener('DOMContentLoaded', @Init);
end.
```

### 2. Gestion d'état simple

```pascal
unit AppState;

interface

uses
  JS, Classes;

type
  TAppState = class
  private
    FData: TJSObject;
    FListeners: array of TNotifyEvent;
  public
    constructor Create;
    procedure SetValue(key: String; value: JSValue);
    function GetValue(key: String): JSValue;
    procedure Subscribe(listener: TNotifyEvent);
    procedure Notify;
  end;

var
  AppState: TAppState;

implementation

constructor TAppState.Create;  
begin
  FData := TJSObject.new;
  SetLength(FListeners, 0);
end;

procedure TAppState.SetValue(key: String; value: JSValue);  
begin
  FData[key] := value;
  Notify;
end;

function TAppState.GetValue(key: String): JSValue;  
begin
  Result := FData[key];
end;

procedure TAppState.Subscribe(listener: TNotifyEvent);  
begin
  SetLength(FListeners, Length(FListeners) + 1);
  FListeners[High(FListeners)] := listener;
end;

procedure TAppState.Notify;  
var
  listener: TNotifyEvent;
begin
  for listener in FListeners do
    listener(Self);
end;

initialization
  AppState := TAppState.Create;

end.
```

**Utilisation** :

```pascal
uses
  AppState;

procedure OnStateChange(Sender: TObject);  
var
  count: Integer;
begin
  count := Integer(AppState.GetValue('count'));
  console.log('Count changed: ' + IntToStr(count));
  UpdateUI;
end;

procedure IncrementCount(event: TJSMouseEvent);  
var
  count: Integer;
begin
  count := Integer(AppState.GetValue('count'));
  AppState.SetValue('count', count + 1);
end;

procedure Init;  
begin
  AppState.SetValue('count', 0);
  AppState.Subscribe(@OnStateChange);
end;
```

### 3. Gestion d'erreurs

```pascal
procedure SafeExecute(proc: TProcedure; errorMsg: String);  
begin
  try
    proc();
  except
    on E: Exception do
    begin
      console.error('Erreur: ' + errorMsg);
      console.error(E.Message);
      ShowNotification('Erreur: ' + errorMsg);
    end;
  end;
end;

// Utilisation
SafeExecute(
  procedure
  begin
    // Code qui peut échouer
    LoadData();
    ProcessData();
  end,
  'Impossible de charger les données'
);
```

### 4. Lazy Loading

**Charger des modules à la demande** :

```pascal
var
  heavyModuleLoaded: Boolean = False;

procedure LoadHeavyModule;  
var
  script: TJSHTMLScriptElement;
begin
  if heavyModuleLoaded then Exit;

  script := TJSHTMLScriptElement(document.createElement('script'));
  script.src := 'heavy-module.js';
  script.onload := procedure
  begin
    heavyModuleLoaded := True;
    console.log('Module lourd chargé');
    InitHeavyModule();
  end;

  document.head.appendChild(script);
end;

procedure OnFeatureButtonClick(event: TJSMouseEvent);  
begin
  if not heavyModuleLoaded then
    LoadHeavyModule()
  else
    UseHeavyFeature();
end;
```

### 5. Progressive Web App (PWA)

**manifest.json** :

```json
{
  "name": "Mon Application Pascal",
  "short_name": "PascalApp",
  "description": "Application web développée en FreePascal",
  "start_url": "/",
  "display": "standalone",
  "background_color": "#ffffff",
  "theme_color": "#667eea",
  "icons": [
    {
      "src": "/assets/images/icon-192.png",
      "sizes": "192x192",
      "type": "image/png"
    },
    {
      "src": "/assets/images/icon-512.png",
      "sizes": "512x512",
      "type": "image/png"
    }
  ]
}
```

**Service Worker (JavaScript nécessaire)** :

```javascript
// sw.js
const CACHE_NAME = 'pascal-app-v1';  
const urlsToCache = [
  '/',
  '/index.html',
  '/app.js',
  '/rtl.js',
  '/styles.css'
];

self.addEventListener('install', event => {
  event.waitUntil(
    caches.open(CACHE_NAME)
      .then(cache => cache.addAll(urlsToCache))
  );
});

self.addEventListener('fetch', event => {
  event.respondWith(
    caches.match(event.request)
      .then(response => response || fetch(event.request))
  );
});
```

**Enregistrement depuis Pascal** :

```pascal
procedure RegisterServiceWorker;  
begin
  if window.navigator.serviceWorker <> nil then
  begin
    window.navigator.serviceWorker.register('/sw.js')
      .then_(procedure(reg)
      begin
        console.log('Service Worker enregistré');
      end)
      .catch_(procedure(err)
      begin
        console.error('Erreur Service Worker:', err);
      end);
  end;
end;

begin
  window.addEventListener('load', @RegisterServiceWorker);
end.
```

---

## Limitations et considérations

### Limitations de Pas2JS

1. **Pas de threading** : JavaScript est mono-thread (utilisez Web Workers)
2. **Pas d'accès fichier direct** : Utilisez les API du navigateur (File API)
3. **Différences de comportement** : Certaines fonctions Pascal ne sont pas disponibles
4. **Taille du runtime** : rtl.js ajoute environ 200-300 Ko
5. **Débogage** : Plus complexe que JavaScript natif

### Quand ne pas utiliser Pas2JS

**Évitez Pas2JS quand** :
- Le projet est simple et JavaScript suffit
- L'équipe ne connaît pas Pascal
- Vous avez besoin de bibliothèques JavaScript spécifiques
- Le SEO est critique (pas de SSR natif)
- Les performances sont critiques (utilisez WebAssembly natif)

**Utilisez Pas2JS quand** :
- Vous avez du code Pascal à réutiliser
- L'équipe maîtrise Pascal
- Vous voulez la sûreté des types
- Vous développez des applications de calcul
- Vous créez des outils internes

---

## Ressources et références

### Documentation officielle

**Pas2JS** :
- Wiki FreePascal : https://wiki.freepascal.org/pas2js
- Documentation : https://wiki.freepascal.org/pas2js_documentation
- Exemples : https://github.com/pas2js/pas2js_examples

**WebAssembly** :
- Site officiel : https://webassembly.org/
- MDN WebAssembly : https://developer.mozilla.org/en-US/docs/WebAssembly

### Bibliothèques et frameworks

**UI Frameworks compatibles** :
- **Bootstrap** : Via manipulation DOM
- **Bulma** : CSS pur, facile à utiliser
- **Tailwind CSS** : Classes utilitaires

**Bibliothèques JavaScript utilisables** :
- **Chart.js** : Graphiques
- **Three.js** : 3D
- **Leaflet** : Cartes interactives
- **Axios** : Requêtes HTTP

### Exemples de projets

**Projets open source en Pas2JS** :
- **Castle Game Engine** : Moteur de jeu
- **Pas2JS Widgets** : Composants UI
- **Pas2JS Router** : Routing SPA

### Communauté

**Forums et support** :
- Forum FreePascal : https://forum.lazarus.freepascal.org/
- Reddit : r/fpc, r/webdev
- Stack Overflow : Tag [pas2js]

---

## Conclusion

### Ce que nous avons appris

1. **WebAssembly et Pas2JS**
   - Différences entre WebAssembly natif et transpilation
   - Installation et configuration de Pas2JS
   - Compilation de code Pascal vers JavaScript

2. **Interopérabilité**
   - Appeler JavaScript depuis Pascal
   - Utiliser des objets JavaScript
   - Passer des callbacks
   - Déclarer des fonctions externes

3. **Manipulation du DOM**
   - Accéder et modifier les éléments HTML
   - Créer des éléments dynamiquement
   - Gérer les événements
   - Valider des formulaires

4. **Applications pratiques**
   - Calculatrice interactive
   - Application de dessin sur Canvas
   - Jeu Snake complet
   - Web Workers pour calculs lourds

5. **Production et déploiement**
   - Scripts de build automatisés
   - Optimisation et minification
   - Configuration serveur web
   - CI/CD avec GitHub Actions
   - Hébergement gratuit

### Avantages de Pascal pour le web

**✓ Points forts** :
- **Type-safety** : Moins d'erreurs runtime
- **Syntaxe claire** : Code lisible et maintenable
- **Réutilisation** : Utiliser du code Pascal existant
- **Performance** : Comparable à JavaScript (transpilé)
- **Outils** : IDE complet avec Lazarus

**✗ Limitations** :
- **Écosystème** : Moins de bibliothèques que JavaScript
- **Communauté** : Plus petite que JS
- **Taille** : Runtime Pas2JS ajoute du poids
- **Adoption** : Moins courant dans le web
- **SEO** : Pas de Server-Side Rendering natif

### L'avenir de Pascal pour le web

**Tendances** :
- **WebAssembly natif** : Support en amélioration dans FPC
- **TypeScript** : Inspire des bonnes pratiques pour Pas2JS
- **Progressive Web Apps** : Pascal peut créer des PWA
- **Edge Computing** : Wasm pour le serverless

### Recommandations finales

**Pour débuter** :
1. Commencez par des projets simples (calculatrice, todo list)
2. Maîtrisez la manipulation du DOM
3. Apprenez l'interopérabilité JavaScript
4. Créez des composants réutilisables
5. Automatisez votre workflow de build

**Pour aller plus loin** :
1. Explorez les Web Workers pour la performance
2. Créez une bibliothèque de composants
3. Implémentez un système de routing SPA
4. Intégrez avec des API REST
5. Optimisez pour la production

**Bonnes pratiques** :
- ✓ Séparez le code en modules logiques
- ✓ Documentez votre code
- ✓ Testez sur différents navigateurs
- ✓ Optimisez les performances critiques
- ✓ Utilisez le contrôle de version (Git)
- ✓ Automatisez le build et le déploiement

### Mot de la fin

Pascal avec Pas2JS offre une alternative intéressante à JavaScript pour le développement web, particulièrement pour les développeurs qui maîtrisent déjà Pascal ou qui ont du code Pascal à réutiliser.

Bien que WebAssembly natif soit encore expérimental dans FreePascal, Pas2JS est **mature et utilisable en production** dès aujourd'hui pour créer des applications web complètes et performantes.

**Le meilleur choix dépend de votre contexte** :
- Équipe Pascal → Pas2JS est excellent
- Code Pascal existant → Réutilisez-le sur le web
- Projet nouveau → Évaluez JS vs Pas2JS
- Performance critique → Attendez Wasm natif ou utilisez Rust/C++

L'important est de choisir la technologie qui correspond le mieux à votre projet, votre équipe et vos contraintes. Pascal pour le web n'est pas un choix évident pour tout le monde, mais c'est un **outil puissant** quand il est utilisé dans le bon contexte.

Bonne chance dans vos développements web avec FreePascal ! 🚀🌐

---

**Fin du tutoriel 19.9 WebAssembly et JavaScript**

Pour toute question ou suggestion, consultez la documentation Pas2JS ou les forums de la communauté FreePascal.

⏭️ [Génération automatique de bindings](/19-interoperabilite-bindings/10-generation-automatique-bindings.md)
