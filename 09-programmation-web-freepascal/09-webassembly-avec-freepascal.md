🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 9.9 WebAssembly avec FreePascal

## Introduction

WebAssembly (souvent abrégé WASM) est un format d'instructions binaires qui permet d'exécuter du code compilé dans un navigateur web à une vitesse proche de celle du code natif. C'est une alternative performante à JavaScript pour les applications web nécessitant des calculs intensifs.

**Qu'est-ce que WebAssembly ?**

WebAssembly est un standard web qui permet d'exécuter du code compilé depuis des langages comme C, C++, Rust, et FreePascal directement dans le navigateur. Il offre des performances proches du code natif tout en restant sécurisé dans le sandbox du navigateur.

```
┌─────────────────┐
│  Code Pascal    │
│   (.pas)        │
└────────┬────────┘
         │
         ▼
   Compilation FPC
         │
         ▼
┌─────────────────┐
│  WebAssembly    │
│   (.wasm)       │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│   Navigateur    │
│   Exécution     │
└─────────────────┘
```

**Avantages de WebAssembly :**
- **Performance** : Exécution 10 à 20 fois plus rapide que JavaScript
- **Portabilité** : Un seul binaire fonctionne sur tous les navigateurs modernes
- **Sécurité** : Exécution dans un environnement sandbox isolé
- **Taille réduite** : Format binaire compact
- **Interopérabilité** : Peut appeler et être appelé par JavaScript

## 9.9.1 Configuration et installation

### Prérequis

Pour compiler du FreePascal vers WebAssembly, vous avez besoin de :

**FreePascal 3.2.2 ou supérieur** avec support WebAssembly

**Windows :**

```batch
REM Télécharger la version avec support WASM depuis
REM https://www.freepascal.org/download.html

REM Ou compiler FPC avec support WASM
git clone https://gitlab.com/freepascal.org/fpc/source.git fpc
cd fpc
make clean
make all OPT="-dWASM"
make install
```

**Ubuntu/Linux :**

```bash
# Installer les dépendances
sudo apt-get update
sudo apt-get install build-essential binutils

# Cloner et compiler FPC avec support WASM
git clone https://gitlab.com/freepascal.org/fpc/source.git fpc
cd fpc
make clean
make all OPT="-dWASM"
sudo make install

# Vérifier l'installation
fpc -i | grep wasm
```

### Vérification de l'installation

```bash
# Compiler un programme de test
fpc -Twasm -Wmtestwasm hello.pas

# Si la compilation réussit, WASM est opérationnel
```

## 9.9.2 Premier programme WebAssembly

### Hello World en WebAssembly

**hello.pas :**

```pascal
program HelloWasm;

{$mode objfpc}
{$H+}

// Fonction exportée vers JavaScript
function Add(A, B: Integer): Integer; cdecl; export; public name 'add';
begin
  Result := A + B;
end;

function Multiply(A, B: Integer): Integer; cdecl; export; public name 'multiply';
begin
  Result := A * B;
end;

// Point d'entrée (optionnel pour WASM)
begin
  WriteLn('Module WebAssembly chargé');
end.
```

### Compilation vers WebAssembly

```bash
# Compiler le programme
fpc -Twasm -WmHelloWasm hello.pas

# Génère hello.wasm
```

### Chargement dans le navigateur

**index.html :**

```html
<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8">
    <title>WebAssembly avec FreePascal</title>
</head>
<body>
    <h1>WebAssembly - FreePascal</h1>

    <div>
        <input type="number" id="num1" value="5"> +
        <input type="number" id="num2" value="3"> =
        <span id="result">?</span>
    </div>

    <button onclick="calculate()">Calculer</button>

    <script>
        let wasmInstance = null;

        // Charger le module WASM
        async function loadWasm() {
            try {
                const response = await fetch('hello.wasm');
                const buffer = await response.arrayBuffer();
                const module = await WebAssembly.instantiate(buffer, {});

                wasmInstance = module.instance;
                console.log('Module WASM chargé avec succès');
                console.log('Fonctions exportées:', Object.keys(wasmInstance.exports));
            } catch (error) {
                console.error('Erreur de chargement WASM:', error);
            }
        }

        // Utiliser les fonctions WASM
        function calculate() {
            if (!wasmInstance) {
                alert('Module WASM non chargé');
                return;
            }

            const num1 = parseInt(document.getElementById('num1').value);
            const num2 = parseInt(document.getElementById('num2').value);

            // Appeler la fonction Pascal depuis JavaScript
            const sum = wasmInstance.exports.add(num1, num2);
            const product = wasmInstance.exports.multiply(num1, num2);

            document.getElementById('result').textContent = sum;
            console.log(`${num1} + ${num2} = ${sum}`);
            console.log(`${num1} × ${num2} = ${product}`);
        }

        // Charger au démarrage
        loadWasm();
    </script>
</body>
</html>
```

### Serveur HTTP local

Pour tester, vous devez servir les fichiers via HTTP (pas en local file://) :

**Python 3 (Windows/Ubuntu) :**

```bash
# Dans le répertoire contenant index.html et hello.wasm
python -m http.server 8000

# Ouvrir http://localhost:8000 dans le navigateur
```

**Node.js :**

```bash
# Installer http-server
npm install -g http-server

# Démarrer le serveur
http-server -p 8000

# Ouvrir http://localhost:8000
```

## 9.9.3 Passage de paramètres et types de données

### Types supportés

WebAssembly supporte nativement uniquement 4 types de données :
- `i32` : Entier 32 bits
- `i64` : Entier 64 bits
- `f32` : Flottant 32 bits (single)
- `f64` : Flottant 64 bits (double)

**Correspondance avec Pascal :**

```pascal
program TypesWasm;

{$mode objfpc}

// Entiers
function AddInt32(A, B: LongInt): LongInt; cdecl; export; public name 'addInt32';
begin
  Result := A + B;
end;

function AddInt64(A, B: Int64): Int64; cdecl; export; public name 'addInt64';
begin
  Result := A + B;
end;

// Flottants
function AddFloat32(A, B: Single): Single; cdecl; export; public name 'addFloat32';
begin
  Result := A + B;
end;

function AddFloat64(A, B: Double): Double; cdecl; export; public name 'addFloat64';
begin
  Result := A + B;
end;

// Booléens (représentés comme i32)
function LogicalAnd(A, B: Boolean): Boolean; cdecl; export; public name 'logicalAnd';
begin
  Result := A and B;
end;

begin
end.
```

**Utilisation en JavaScript :**

```javascript
async function testTypes() {
    const response = await fetch('types.wasm');
    const buffer = await response.arrayBuffer();
    const wasm = await WebAssembly.instantiate(buffer, {});

    // Entiers
    console.log('10 + 20 =', wasm.instance.exports.addInt32(10, 20));
    console.log('1000000000 + 2000000000 =',
                wasm.instance.exports.addInt64(1000000000n, 2000000000n));

    // Flottants
    console.log('3.14 + 2.86 =', wasm.instance.exports.addFloat32(3.14, 2.86));
    console.log('Math.PI + Math.E =',
                wasm.instance.exports.addFloat64(Math.PI, Math.E));

    // Booléens (0 = false, 1 = true)
    console.log('true AND false =', wasm.instance.exports.logicalAnd(1, 0));
}
```

### Passage de chaînes de caractères

Les chaînes nécessitent de passer par la mémoire partagée :

```pascal
program StringsWasm;

{$mode objfpc}

uses
  SysUtils;

var
  SharedMemory: array[0..4095] of Byte;

// Fonction pour écrire une chaîne dans la mémoire
function GetMemoryPointer: PByte; cdecl; export; public name 'getMemoryPointer';
begin
  Result := @SharedMemory[0];
end;

function GetMemorySize: Integer; cdecl; export; public name 'getMemorySize';
begin
  Result := SizeOf(SharedMemory);
end;

// Convertir une chaîne en majuscules
function ToUpperCase(Offset: Integer; Length: Integer): Integer;
  cdecl; export; public name 'toUpperCase';
var
  InputStr: string;
  OutputStr: string;
  i: Integer;
begin
  // Lire depuis la mémoire partagée
  SetLength(InputStr, Length);
  for i := 0 to Length - 1 do
    InputStr[i + 1] := Chr(SharedMemory[Offset + i]);

  // Convertir
  OutputStr := UpperCase(InputStr);

  // Écrire dans la mémoire
  for i := 1 to System.Length(OutputStr) do
    SharedMemory[Offset + i - 1] := Ord(OutputStr[i]);

  Result := System.Length(OutputStr);
end;

begin
end.
```

**JavaScript pour manipuler les chaînes :**

```javascript
class WasmStringHelper {
    constructor(wasmInstance) {
        this.wasm = wasmInstance;
        this.memory = new Uint8Array(
            this.wasm.exports.memory.buffer,
            this.wasm.exports.getMemoryPointer(),
            this.wasm.exports.getMemorySize()
        );
    }

    // Écrire une chaîne JavaScript dans la mémoire WASM
    writeString(str, offset) {
        const encoder = new TextEncoder();
        const bytes = encoder.encode(str);
        this.memory.set(bytes, offset);
        return bytes.length;
    }

    // Lire une chaîne depuis la mémoire WASM
    readString(offset, length) {
        const bytes = this.memory.slice(offset, offset + length);
        const decoder = new TextDecoder();
        return decoder.decode(bytes);
    }
}

// Utilisation
async function testStrings() {
    const response = await fetch('strings.wasm');
    const buffer = await response.arrayBuffer();
    const wasm = await WebAssembly.instantiate(buffer, {});

    const helper = new WasmStringHelper(wasm.instance);

    // Écrire une chaîne
    const input = "hello world";
    const length = helper.writeString(input, 0);

    // Appeler la fonction WASM
    const resultLength = wasm.instance.exports.toUpperCase(0, length);

    // Lire le résultat
    const output = helper.readString(0, resultLength);
    console.log(`"${input}" -> "${output}"`);
}
```

## 9.9.4 Calculs mathématiques intensifs

### Exemple : Calcul de nombres premiers

```pascal
program PrimesWasm;

{$mode objfpc}

// Vérifier si un nombre est premier
function IsPrime(N: Integer): Boolean; cdecl; export; public name 'isPrime';
var
  i: Integer;
begin
  if N <= 1 then
    Exit(False);

  if N <= 3 then
    Exit(True);

  if (N mod 2 = 0) or (N mod 3 = 0) then
    Exit(False);

  i := 5;
  while i * i <= N do
  begin
    if (N mod i = 0) or (N mod (i + 2) = 0) then
      Exit(False);
    Inc(i, 6);
  end;

  Result := True;
end;

// Compter les nombres premiers jusqu'à N
function CountPrimes(N: Integer): Integer; cdecl; export; public name 'countPrimes';
var
  i, Count: Integer;
begin
  Count := 0;
  for i := 2 to N do
  begin
    if IsPrime(i) then
      Inc(Count);
  end;
  Result := Count;
end;

// Calculer la somme des N premiers nombres premiers
function SumFirstNPrimes(N: Integer): Int64; cdecl; export; public name 'sumFirstNPrimes';
var
  Sum: Int64;
  Count, Current: Integer;
begin
  Sum := 0;
  Count := 0;
  Current := 2;

  while Count < N do
  begin
    if IsPrime(Current) then
    begin
      Sum := Sum + Current;
      Inc(Count);
    end;
    Inc(Current);
  end;

  Result := Sum;
end;

begin
end.
```

**Comparaison de performance WASM vs JavaScript :**

```html
<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8">
    <title>Performance WASM vs JavaScript</title>
    <style>
        .result {
            margin: 20px 0;
            padding: 10px;
            background: #f0f0f0;
        }
        .faster {
            color: green;
            font-weight: bold;
        }
    </style>
</head>
<body>
    <h1>Comparaison de Performance</h1>

    <button onclick="runBenchmark()">Lancer le benchmark</button>

    <div id="results"></div>

    <script>
        let wasm = null;

        // Version JavaScript pure
        function isPrimeJS(n) {
            if (n <= 1) return false;
            if (n <= 3) return true;
            if (n % 2 === 0 || n % 3 === 0) return false;

            for (let i = 5; i * i <= n; i += 6) {
                if (n % i === 0 || n % (i + 2) === 0) return false;
            }
            return true;
        }

        function countPrimesJS(n) {
            let count = 0;
            for (let i = 2; i <= n; i++) {
                if (isPrimeJS(i)) count++;
            }
            return count;
        }

        // Benchmark
        async function runBenchmark() {
            const resultsDiv = document.getElementById('results');
            resultsDiv.innerHTML = '<p>Calcul en cours...</p>';

            const testSize = 100000;

            // Test JavaScript
            const jsStart = performance.now();
            const jsResult = countPrimesJS(testSize);
            const jsTime = performance.now() - jsStart;

            // Test WebAssembly
            const wasmStart = performance.now();
            const wasmResult = wasm.exports.countPrimes(testSize);
            const wasmTime = performance.now() - wasmStart;

            // Calculer le rapport
            const speedup = (jsTime / wasmTime).toFixed(2);

            // Afficher les résultats
            resultsDiv.innerHTML = `
                <div class="result">
                    <h3>Résultats pour ${testSize.toLocaleString()} nombres :</h3>
                    <p>Nombres premiers trouvés : ${jsResult}</p>
                    <p><strong>JavaScript :</strong> ${jsTime.toFixed(2)} ms</p>
                    <p><strong>WebAssembly :</strong> ${wasmTime.toFixed(2)} ms</p>
                    <p class="faster">WebAssembly est ${speedup}× plus rapide !</p>
                </div>
            `;
        }

        // Charger WASM au démarrage
        async function init() {
            const response = await fetch('primes.wasm');
            const buffer = await response.arrayBuffer();
            const module = await WebAssembly.instantiate(buffer, {});
            wasm = module.instance;
            console.log('WASM chargé et prêt');
        }

        init();
    </script>
</body>
</html>
```

## 9.9.5 Traitement d'images

### Filtres d'images en WebAssembly

```pascal
program ImageFilters;

{$mode objfpc}

type
  PRGBA = ^TRGBA;
  TRGBA = packed record
    R, G, B, A: Byte;
  end;

var
  ImageBuffer: array of TRGBA;

// Obtenir un pointeur vers le buffer
function GetImageBuffer: Pointer; cdecl; export; public name 'getImageBuffer';
begin
  Result := @ImageBuffer[0];
end;

// Allouer le buffer
procedure AllocateBuffer(PixelCount: Integer); cdecl; export; public name 'allocateBuffer';
begin
  SetLength(ImageBuffer, PixelCount);
end;

// Convertir en niveaux de gris
procedure ConvertToGrayscale(Width, Height: Integer);
  cdecl; export; public name 'convertToGrayscale';
var
  i, Total: Integer;
  Gray: Byte;
begin
  Total := Width * Height;

  for i := 0 to Total - 1 do
  begin
    // Formule de luminance
    Gray := Round(0.299 * ImageBuffer[i].R +
                  0.587 * ImageBuffer[i].G +
                  0.114 * ImageBuffer[i].B);

    ImageBuffer[i].R := Gray;
    ImageBuffer[i].G := Gray;
    ImageBuffer[i].B := Gray;
  end;
end;

// Inverser les couleurs
procedure InvertColors(Width, Height: Integer);
  cdecl; export; public name 'invertColors';
var
  i, Total: Integer;
begin
  Total := Width * Height;

  for i := 0 to Total - 1 do
  begin
    ImageBuffer[i].R := 255 - ImageBuffer[i].R;
    ImageBuffer[i].G := 255 - ImageBuffer[i].G;
    ImageBuffer[i].B := 255 - ImageBuffer[i].B;
  end;
end;

// Ajuster la luminosité
procedure AdjustBrightness(Width, Height: Integer; Amount: Integer);
  cdecl; export; public name 'adjustBrightness';
var
  i, Total: Integer;

  function Clamp(Value: Integer): Byte;
  begin
    if Value < 0 then
      Result := 0
    else if Value > 255 then
      Result := 255
    else
      Result := Value;
  end;

begin
  Total := Width * Height;

  for i := 0 to Total - 1 do
  begin
    ImageBuffer[i].R := Clamp(ImageBuffer[i].R + Amount);
    ImageBuffer[i].G := Clamp(ImageBuffer[i].G + Amount);
    ImageBuffer[i].B := Clamp(ImageBuffer[i].B + Amount);
  end;
end;

// Flou simple (box blur)
procedure BlurImage(Width, Height: Integer);
  cdecl; export; public name 'blurImage';
var
  TempBuffer: array of TRGBA;
  x, y, dx, dy: Integer;
  SumR, SumG, SumB, Count: Integer;
  KernelSize: Integer;
  Pixel: TRGBA;

  function GetPixel(X, Y: Integer): TRGBA;
  begin
    if (X < 0) or (X >= Width) or (Y < 0) or (Y >= Height) then
    begin
      Result.R := 0;
      Result.G := 0;
      Result.B := 0;
      Result.A := 255;
    end
    else
      Result := ImageBuffer[Y * Width + X];
  end;

begin
  SetLength(TempBuffer, Width * Height);
  KernelSize := 1; // Rayon du flou

  for y := 0 to Height - 1 do
  begin
    for x := 0 to Width - 1 do
    begin
      SumR := 0;
      SumG := 0;
      SumB := 0;
      Count := 0;

      // Parcourir le kernel
      for dy := -KernelSize to KernelSize do
      begin
        for dx := -KernelSize to KernelSize do
        begin
          Pixel := GetPixel(x + dx, y + dy);
          SumR := SumR + Pixel.R;
          SumG := SumG + Pixel.G;
          SumB := SumB + Pixel.B;
          Inc(Count);
        end;
      end;

      // Moyenne
      TempBuffer[y * Width + x].R := SumR div Count;
      TempBuffer[y * Width + x].G := SumG div Count;
      TempBuffer[y * Width + x].B := SumB div Count;
      TempBuffer[y * Width + x].A := 255;
    end;
  end;

  // Copier le résultat
  ImageBuffer := TempBuffer;
end;

begin
end.
```

**Application HTML5 Canvas :**

```html
<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8">
    <title>Filtres d'images WASM</title>
    <style>
        canvas {
            border: 1px solid #ccc;
            margin: 10px;
        }
        .controls {
            margin: 20px 0;
        }
        button {
            margin: 5px;
            padding: 10px 20px;
        }
    </style>
</head>
<body>
    <h1>Filtres d'images avec WebAssembly</h1>

    <div>
        <input type="file" id="imageInput" accept="image/*">
    </div>

    <div class="controls">
        <button onclick="applyFilter('grayscale')">Niveaux de gris</button>
        <button onclick="applyFilter('invert')">Inverser</button>
        <button onclick="applyFilter('brighten')">+ Luminosité</button>
        <button onclick="applyFilter('darken')">- Luminosité</button>
        <button onclick="applyFilter('blur')">Flou</button>
        <button onclick="resetImage()">Réinitialiser</button>
    </div>

    <div>
        <canvas id="canvas"></canvas>
    </div>

    <div id="performance"></div>

    <script>
        let wasm = null;
        let canvas = null;
        let ctx = null;
        let originalImageData = null;

        // Charger une image
        document.getElementById('imageInput').addEventListener('change', function(e) {
            const file = e.target.files[0];
            if (!file) return;

            const reader = new FileReader();
            reader.onload = function(event) {
                const img = new Image();
                img.onload = function() {
                    canvas.width = img.width;
                    canvas.height = img.height;
                    ctx.drawImage(img, 0, 0);

                    // Sauvegarder l'image originale
                    originalImageData = ctx.getImageData(0, 0, canvas.width, canvas.height);
                };
                img.src = event.target.result;
            };
            reader.readAsDataURL(file);
        });

        // Appliquer un filtre
        async function applyFilter(filterName) {
            if (!originalImageData || !wasm) {
                alert('Veuillez charger une image d\'abord');
                return;
            }

            const width = canvas.width;
            const height = canvas.height;
            const pixelCount = width * height;

            // Allouer le buffer dans WASM
            wasm.exports.allocateBuffer(pixelCount);

            // Obtenir un pointeur vers la mémoire WASM
            const bufferPtr = wasm.exports.getImageBuffer();
            const wasmMemory = new Uint8Array(
                wasm.exports.memory.buffer,
                bufferPtr,
                pixelCount * 4
            );

            // Copier les données de l'image vers WASM
            wasmMemory.set(originalImageData.data);

            // Appliquer le filtre
            const start = performance.now();

            switch(filterName) {
                case 'grayscale':
                    wasm.exports.convertToGrayscale(width, height);
                    break;
                case 'invert':
                    wasm.exports.invertColors(width, height);
                    break;
                case 'brighten':
                    wasm.exports.adjustBrightness(width, height, 30);
                    break;
                case 'darken':
                    wasm.exports.adjustBrightness(width, height, -30);
                    break;
                case 'blur':
                    wasm.exports.blurImage(width, height);
                    break;
            }

            const elapsed = performance.now() - start;

            // Récupérer les données modifiées
            const imageData = ctx.createImageData(width, height);
            imageData.data.set(wasmMemory);

            // Afficher
            ctx.putImageData(imageData, 0, 0);

            document.getElementById('performance').innerHTML =
                `Filtre appliqué en ${elapsed.toFixed(2)} ms`;
        }

        // Réinitialiser
        function resetImage() {
            if (originalImageData) {
                ctx.putImageData(originalImageData, 0, 0);
            }
        }

        // Initialisation
        async function init() {
            canvas = document.getElementById('canvas');
            ctx = canvas.getContext('2d');

            // Charger WASM
            const response = await fetch('imagefilters.wasm');
            const buffer = await response.arrayBuffer();
            const module = await WebAssembly.instantiate(buffer, {
                env: {
                    memory: new WebAssembly.Memory({ initial: 256 })
                }
            });
            wasm = module.instance;

            console.log('WASM chargé');
        }

        init();
    </script>
</body>
</html>
```

## 9.9.6 Interaction avec JavaScript

### Appeler JavaScript depuis WASM

```pascal
program WasmJSInterop;

{$mode objfpc}

uses
  SysUtils;

// Déclarer les fonctions JavaScript que WASM peut appeler
procedure JSConsoleLog(Message: PChar); external 'env' name 'consoleLog';
procedure JSAlert(Message: PChar); external 'env' name 'showAlert';
function JSGetTimestamp: Double; external 'env' name 'getTimestamp';

// Fonction qui utilise JavaScript
function ProcessWithLogging(Value: Integer): Integer;
  cdecl; export; public name 'processWithLogging';
var
  StartTime, EndTime: Double;
begin
  StartTime := JSGetTimestamp;
  JSConsoleLog('Début du traitement');

  // Traitement intensif
  Result := Value * Value;
  Sleep(100); // Simulation

  EndTime := JSGetTimestamp;
  JSConsoleLog('Fin du traitement');

  if Result > 1000 then
    JSAlert('Résultat important!');

  Exit(Result);
end;

begin
end.
```

**Configuration des imports JavaScript :**

```javascript
async function loadWasmWithImports() {
    // Définir les fonctions que WASM peut appeler
    const imports = {
        env: {
            consoleLog: function(ptr) {
                const message = readStringFromMemory(ptr);
                console.log('[WASM]', message);
            },

            showAlert: function(ptr) {
                const message = readStringFromMemory(ptr);
                alert(message);
            },

            getTimestamp: function() {
                return Date.now();
            },

            memory: new WebAssembly.Memory({ initial: 256 })
        }
    };

    const response = await fetch('wasmjsinterop.wasm');
    const buffer = await response.arrayBuffer();
    const wasm = await WebAssembly.instantiate(buffer, imports);

    return wasm.instance;
}

// Fonction helper pour lire des chaînes
function readStringFromMemory(ptr) {
    const memory = new Uint8Array(wasmInstance.exports.memory.buffer);
    let end = ptr;
    while (memory[end] !== 0) end++;
    const bytes = memory.slice(ptr, end);
    return new TextDecoder().decode(bytes);
}
```

## 9.9.7 Gestion de la mémoire

### Mémoire linéaire

WebAssembly utilise une mémoire linéaire (un grand tableau d'octets) partagée entre WASM et JavaScript.

```pascal
program MemoryManagement;

{$mode objfpc}

const
  BUFFER_SIZE = 1024 * 1024; // 1 MB

type
  PDataBuffer = ^TDataBuffer;
  TDataBuffer = array[0..BUFFER_SIZE-1] of Byte;

var
  GlobalBuffer: TDataBuffer;
  UsedMemory: Integer;

// Obtenir la taille de la mémoire disponible
function GetMemorySize: Integer; cdecl; export; public name 'getMemorySize';
begin
  Result := BUFFER_SIZE;
end;

// Obtenir la mémoire utilisée
function GetUsedMemory: Integer; cdecl; export; public name 'getUsedMemory';
begin
  Result := UsedMemory;
end;

// Obtenir un pointeur vers le buffer
function GetBufferPointer: Pointer; cdecl; export; public name 'getBufferPointer';
begin
  Result := @GlobalBuffer[0];
end;

// Allouer de l'espace dans le buffer
function AllocateMemory(Size: Integer): Integer; cdecl; export; public name 'allocateMemory';
var
  Offset: Integer;
begin
  if UsedMemory + Size > BUFFER_SIZE then
    Exit(-1); // Pas assez de mémoire

  Offset := UsedMemory;
  UsedMemory := UsedMemory + Size;
  Result := Offset;
end;

// Libérer toute la mémoire (réinitialisation simple)
procedure FreeAllMemory; cdecl; export; public name 'freeAllMemory';
begin
  UsedMemory := 0;
  FillByte(GlobalBuffer, BUFFER_SIZE, 0);
end;

// Copier des données dans le buffer
procedure CopyToBuffer(Offset: Integer; Data: Pointer; Size: Integer);
  cdecl; export; public name 'copyToBuffer';
var
  i: Integer;
  SrcPtr: PByte;
begin
  if (Offset < 0) or (Offset + Size > BUFFER_SIZE) then
    Exit;

  SrcPtr := PByte(Data);
  for i := 0 to Size - 1 do
    GlobalBuffer[Offset + i] := SrcPtr[i];
end;

begin
  UsedMemory := 0;
  FillByte(GlobalBuffer, BUFFER_SIZE, 0);
end.
```

**Gestion côté JavaScript :**

```javascript
class WasmMemoryManager {
    constructor(wasmInstance) {
        this.wasm = wasmInstance;
        this.memory = new Uint8Array(this.wasm.exports.memory.buffer);
        this.bufferPointer = this.wasm.exports.getBufferPointer();
    }

    // Obtenir la vue sur le buffer
    getBuffer(offset, length) {
        return new Uint8Array(
            this.wasm.exports.memory.buffer,
            this.bufferPointer + offset,
            length
        );
    }

    // Écrire des données
    writeData(data, offset = null) {
        let actualOffset = offset;

        if (actualOffset === null) {
            // Allouer automatiquement
            actualOffset = this.wasm.exports.allocateMemory(data.length);
            if (actualOffset === -1) {
                throw new Error('Mémoire insuffisante');
            }
        }

        const buffer = this.getBuffer(actualOffset, data.length);
        buffer.set(data);

        return actualOffset;
    }

    // Lire des données
    readData(offset, length) {
        const buffer = this.getBuffer(offset, length);
        return new Uint8Array(buffer);
    }

    // Obtenir les statistiques mémoire
    getStats() {
        return {
            total: this.wasm.exports.getMemorySize(),
            used: this.wasm.exports.getUsedMemory(),
            free: this.wasm.exports.getMemorySize() -
                  this.wasm.exports.getUsedMemory()
        };
    }

    // Réinitialiser
    reset() {
        this.wasm.exports.freeAllMemory();
    }
}

// Utilisation
async function testMemory() {
    const wasm = await loadWasm('memory.wasm');
    const memMgr = new WasmMemoryManager(wasm);

    // Écrire des données
    const data = new Uint8Array([1, 2, 3, 4, 5]);
    const offset = memMgr.writeData(data);

    // Lire les données
    const readData = memMgr.readData(offset, 5);
    console.log('Données lues:', readData);

    // Statistiques
    console.log('Mémoire:', memMgr.getStats());
}
```

### Croissance de la mémoire

```pascal
program GrowableMemory;

{$mode objfpc}

var
  CurrentPages: Integer;

const
  PAGE_SIZE = 65536; // 64 KB

// Obtenir le nombre de pages actuelles
function GetCurrentPages: Integer; cdecl; export; public name 'getCurrentPages';
begin
  Result := CurrentPages;
end;

// Demander plus de mémoire
function GrowMemory(AdditionalPages: Integer): Integer;
  cdecl; export; public name 'growMemory';
begin
  // Note: En WASM, la croissance est gérée par l'environnement
  // Cette fonction sert d'interface
  CurrentPages := CurrentPages + AdditionalPages;
  Result := CurrentPages;
end;

begin
  CurrentPages := 1; // Commence avec 1 page (64 KB)
end.
```

**JavaScript pour gérer la croissance :**

```javascript
async function initWithGrowableMemory() {
    // Créer une mémoire qui peut croître
    const memory = new WebAssembly.Memory({
        initial: 1,  // 1 page = 64 KB
        maximum: 100 // Maximum 100 pages = 6.4 MB
    });

    const imports = {
        env: { memory: memory }
    };

    const response = await fetch('growable.wasm');
    const buffer = await response.arrayBuffer();
    const wasm = await WebAssembly.instantiate(buffer, imports);

    console.log('Mémoire initiale:', memory.buffer.byteLength, 'bytes');

    // Faire croître la mémoire
    const previousPages = memory.grow(10); // Ajouter 10 pages
    console.log('Pages précédentes:', previousPages);
    console.log('Nouvelle taille:', memory.buffer.byteLength, 'bytes');

    return wasm.instance;
}
```

## 9.9.8 Compilation et optimisation

### Options de compilation

```bash
# Compilation de base
fpc -Twasm -WmMyModule myprogram.pas

# Avec optimisation maximale
fpc -Twasm -O3 -WmMyModule myprogram.pas

# Avec informations de débogage
fpc -Twasm -g -WmMyModule myprogram.pas

# Compilation avec taille minimale
fpc -Twasm -Os -WmMyModule myprogram.pas

# Options avancées
fpc -Twasm \
    -O3 \                    # Optimisation niveau 3
    -Xs \                    # Strip symbols
    -XX \                    # Smartlinking
    -CX \                    # Smartlinking avancé
    -WmMyModule \           # Nom du module
    myprogram.pas
```

### Optimisation du code

**Code non optimisé :**

```pascal
program UnoptimizedCode;

{$mode objfpc}

// Calcul inefficace
function CalculateSumSlow(N: Integer): Int64;
  cdecl; export; public name 'calculateSumSlow';
var
  i: Integer;
  Sum: Int64;
begin
  Sum := 0;
  for i := 1 to N do
  begin
    Sum := Sum + i;
    // Opérations inutiles
    if Sum > 0 then
      Sum := Sum + 0;
  end;
  Result := Sum;
end;

begin
end.
```

**Code optimisé :**

```pascal
program OptimizedCode;

{$mode objfpc}

// Calcul optimisé avec formule mathématique
function CalculateSumFast(N: Integer): Int64;
  cdecl; export; public name 'calculateSumFast';
begin
  // Formule de Gauss : n * (n + 1) / 2
  Result := Int64(N) * (N + 1) div 2;
end;

// Utiliser des types appropriés
function ProcessArray(Data: PInteger; Count: Integer): Int64;
  cdecl; export; public name 'processArray';
var
  i: Integer;
  Sum: Int64;
begin
  Sum := 0;

  // Déroulement de boucle manuel
  i := 0;
  while i < Count - 3 do
  begin
    Sum := Sum + Data[i] + Data[i+1] + Data[i+2] + Data[i+3];
    Inc(i, 4);
  end;

  // Traiter le reste
  while i < Count do
  begin
    Sum := Sum + Data[i];
    Inc(i);
  end;

  Result := Sum;
end;

begin
end.
```

### Mesure des performances

```html
<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8">
    <title>Benchmark WASM</title>
</head>
<body>
    <h1>Benchmark des optimisations</h1>
    <button onclick="runBenchmark()">Lancer</button>
    <div id="results"></div>

    <script>
        let wasm = null;

        async function runBenchmark() {
            const iterations = 1000;
            const testValue = 1000000;

            // Test version lente
            let start = performance.now();
            for (let i = 0; i < iterations; i++) {
                wasm.exports.calculateSumSlow(testValue);
            }
            const slowTime = performance.now() - start;

            // Test version rapide
            start = performance.now();
            for (let i = 0; i < iterations; i++) {
                wasm.exports.calculateSumFast(testValue);
            }
            const fastTime = performance.now() - start;

            // Afficher les résultats
            const speedup = (slowTime / fastTime).toFixed(2);
            document.getElementById('results').innerHTML = `
                <h3>Résultats (${iterations} itérations):</h3>
                <p>Version non optimisée : ${slowTime.toFixed(2)} ms</p>
                <p>Version optimisée : ${fastTime.toFixed(2)} ms</p>
                <p><strong>Gain de performance : ${speedup}×</strong></p>
            `;
        }

        async function init() {
            const response = await fetch('optimized.wasm');
            const buffer = await response.arrayBuffer();
            const module = await WebAssembly.instantiate(buffer, {});
            wasm = module.instance;
        }

        init();
    </script>
</body>
</html>
```

## 9.9.9 Debugging et outils

### Source Maps

Pour faciliter le débogage, générez des source maps :

```bash
# Compiler avec informations de débogage
fpc -Twasm -g -gl myprogram.pas

# Générer un fichier source map (si supporté)
wasm-sourcemap myprogram.wasm -o myprogram.wasm.map
```

### Inspection du module WASM

**Avec wasm-objdump (wabt tools) :**

```bash
# Installer wabt (WebAssembly Binary Toolkit)
# Ubuntu
sudo apt-get install wabt

# Windows - télécharger depuis GitHub
# https://github.com/WebAssembly/wabt/releases

# Afficher les sections
wasm-objdump -h myprogram.wasm

# Afficher les exports
wasm-objdump -x myprogram.wasm

# Désassembler en texte
wasm2wat myprogram.wasm -o myprogram.wat

# Réassembler depuis texte
wat2wasm myprogram.wat -o myprogram.wasm
```

### Console de débogage

```javascript
class WasmDebugger {
    constructor(wasmInstance) {
        this.wasm = wasmInstance;
        this.callStack = [];
        this.breakpoints = new Set();
    }

    // Wrapper pour tracer les appels
    wrapFunction(name, func) {
        const self = this;
        return function(...args) {
            self.callStack.push(name);
            console.log(`[WASM] Calling ${name}(${args.join(', ')})`);

            // Vérifier les breakpoints
            if (self.breakpoints.has(name)) {
                debugger; // Point d'arrêt JavaScript
            }

            try {
                const result = func.apply(this, args);
                console.log(`[WASM] ${name} returned:`, result);
                self.callStack.pop();
                return result;
            } catch (error) {
                console.error(`[WASM] Error in ${name}:`, error);
                console.error('Call stack:', self.callStack);
                self.callStack.pop();
                throw error;
            }
        };
    }

    // Wrapper pour toutes les fonctions exportées
    wrapAllExports() {
        const wrapped = {};
        for (const [name, func] of Object.entries(this.wasm.exports)) {
            if (typeof func === 'function') {
                wrapped[name] = this.wrapFunction(name, func);
            } else {
                wrapped[name] = func;
            }
        }
        return wrapped;
    }

    // Ajouter un breakpoint
    addBreakpoint(functionName) {
        this.breakpoints.add(functionName);
    }

    // Supprimer un breakpoint
    removeBreakpoint(functionName) {
        this.breakpoints.delete(functionName);
    }

    // Afficher la pile d'appels
    printCallStack() {
        console.log('Call stack:', this.callStack);
    }
}

// Utilisation
async function loadWithDebugger() {
    const response = await fetch('myprogram.wasm');
    const buffer = await response.arrayBuffer();
    const module = await WebAssembly.instantiate(buffer, {});

    const debugger = new WasmDebugger(module.instance);
    const wrappedExports = debugger.wrapAllExports();

    // Ajouter un breakpoint
    debugger.addBreakpoint('calculateSum');

    // Utiliser les fonctions wrappées
    const result = wrappedExports.calculateSum(100);

    return { wasm: wrappedExports, debugger: debugger };
}
```

## 9.9.10 Threading avec Web Workers

### Module WASM dans un Worker

**worker.js :**

```javascript
// Worker JavaScript qui charge et utilise WASM
let wasm = null;

// Charger WASM dans le worker
async function loadWasm() {
    try {
        const response = await fetch('compute.wasm');
        const buffer = await response.arrayBuffer();
        const module = await WebAssembly.instantiate(buffer, {});
        wasm = module.instance;

        postMessage({ type: 'ready' });
    } catch (error) {
        postMessage({ type: 'error', message: error.message });
    }
}

// Traiter les messages
self.onmessage = function(e) {
    const { type, data } = e.data;

    switch (type) {
        case 'init':
            loadWasm();
            break;

        case 'compute':
            if (!wasm) {
                postMessage({ type: 'error', message: 'WASM not loaded' });
                return;
            }

            // Effectuer un calcul intensif
            const result = wasm.exports.heavyComputation(data.value);
            postMessage({ type: 'result', result: result });
            break;
    }
};
```

**compute.pas :**

```pascal
program ComputeWorker;

{$mode objfpc}

uses
  SysUtils;

// Calcul intensif pour le worker
function HeavyComputation(N: Integer): Int64;
  cdecl; export; public name 'heavyComputation';
var
  i: Integer;
  Sum: Int64;
begin
  Sum := 0;

  // Simulation de calcul intensif
  for i := 1 to N do
  begin
    Sum := Sum + (i * i) mod 1000000007;

    // Opérations mathématiques complexes
    if i mod 2 = 0 then
      Sum := Sum + (i div 2)
    else
      Sum := Sum - (i div 3);
  end;

  Result := Sum;
end;

// Traiter un tableau de données
function ProcessDataArray(Data: PInteger; Count: Integer): Int64;
  cdecl; export; public name 'processDataArray';
var
  i: Integer;
begin
  Result := 0;

  for i := 0 to Count - 1 do
  begin
    Result := Result + HeavyComputation(Data[i]);
  end;
end;

begin
end.
```

**Application principale :**

```html
<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8">
    <title>WASM Multi-threading</title>
</head>
<body>
    <h1>Calculs parallèles avec WASM</h1>

    <div>
        <button onclick="startComputation()">Lancer calcul</button>
        <button onclick="cancelComputation()">Annuler</button>
    </div>

    <div id="status">Prêt</div>
    <div id="progress"></div>
    <div id="results"></div>

    <script>
        const workers = [];
        const workerCount = 4; // Utiliser 4 workers
        let tasksCompleted = 0;
        let totalTasks = 0;

        // Initialiser les workers
        function initWorkers() {
            for (let i = 0; i < workerCount; i++) {
                const worker = new Worker('worker.js');

                worker.onmessage = function(e) {
                    handleWorkerMessage(i, e.data);
                };

                worker.onerror = function(error) {
                    console.error(`Worker ${i} error:`, error);
                };

                // Initialiser le worker
                worker.postMessage({ type: 'init' });
                workers.push(worker);
            }
        }

        // Gérer les messages des workers
        function handleWorkerMessage(workerId, message) {
            switch (message.type) {
                case 'ready':
                    console.log(`Worker ${workerId} ready`);
                    break;

                case 'result':
                    tasksCompleted++;
                    updateProgress();

                    const resultsDiv = document.getElementById('results');
                    resultsDiv.innerHTML += `
                        <p>Worker ${workerId}: ${message.result}</p>
                    `;

                    if (tasksCompleted === totalTasks) {
                        document.getElementById('status').textContent =
                            'Tous les calculs terminés!';
                    }
                    break;

                case 'error':
                    console.error(`Worker ${workerId} error:`, message.message);
                    break;
            }
        }

        // Lancer les calculs
        function startComputation() {
            tasksCompleted = 0;
            totalTasks = workerCount * 10; // 10 tâches par worker

            document.getElementById('results').innerHTML = '';
            document.getElementById('status').textContent = 'Calcul en cours...';

            const startTime = performance.now();

            // Distribuer les tâches aux workers
            for (let i = 0; i < workerCount; i++) {
                for (let j = 0; j < 10; j++) {
                    workers[i].postMessage({
                        type: 'compute',
                        data: { value: 1000000 + (i * 10000) + (j * 1000) }
                    });
                }
            }
        }

        // Annuler les calculs
        function cancelComputation() {
            workers.forEach(w => w.terminate());
            workers.length = 0;
            initWorkers();
            document.getElementById('status').textContent = 'Annulé';
        }

        // Mettre à jour la progression
        function updateProgress() {
            const percent = (tasksCompleted / totalTasks * 100).toFixed(1);
            document.getElementById('progress').textContent =
                `Progression: ${tasksCompleted}/${totalTasks} (${percent}%)`;
        }

        // Initialiser au chargement
        initWorkers();
    </script>
</body>
</html>
```

## 9.9.11 Partage de mémoire avec SharedArrayBuffer

### Calculs parallèles avec mémoire partagée

```pascal
program SharedMemoryCompute;

{$mode objfpc}

type
  PSharedData = ^TSharedData;
  TSharedData = packed record
    Input: array[0..999] of Integer;
    Output: array[0..999] of Integer;
    Status: Integer; // 0 = idle, 1 = processing, 2 = done
  end;

var
  SharedData: PSharedData;

// Initialiser le pointeur vers la mémoire partagée
procedure InitSharedMemory(Ptr: Pointer);
  cdecl; export; public name 'initSharedMemory';
begin
  SharedData := PSharedData(Ptr);
  SharedData^.Status := 0;
end;

// Traiter une portion des données
procedure ProcessChunk(StartIdx, EndIdx: Integer);
  cdecl; export; public name 'processChunk';
var
  i: Integer;
begin
  if not Assigned(SharedData) then
    Exit;

  SharedData^.Status := 1; // Processing

  for i := StartIdx to EndIdx do
  begin
    if i < 1000 then
      SharedData^.Output[i] := SharedData^.Input[i] * 2 + 1;
  end;

  SharedData^.Status := 2; // Done
end;

begin
end.
```

**JavaScript avec SharedArrayBuffer :**

```javascript
// Note: SharedArrayBuffer nécessite des headers HTTP spécifiques
// Cross-Origin-Embedder-Policy: require-corp
// Cross-Origin-Opener-Policy: same-origin

class SharedMemoryCompute {
    constructor() {
        this.sharedBuffer = null;
        this.workers = [];
        this.wasm = null;
    }

    async init(workerCount) {
        // Créer un buffer partagé
        const bufferSize = 1024 * 8; // 8 KB
        this.sharedBuffer = new SharedArrayBuffer(bufferSize);

        // Charger WASM
        const response = await fetch('sharedmemory.wasm');
        const buffer = await response.arrayBuffer();
        const module = await WebAssembly.instantiate(buffer, {});
        this.wasm = module.instance;

        // Initialiser WASM avec la mémoire partagée
        const sharedView = new Uint8Array(this.sharedBuffer);
        const wasmMemory = new Uint8Array(this.wasm.exports.memory.buffer);

        // Créer les workers
        for (let i = 0; i < workerCount; i++) {
            const worker = new Worker('shared-worker.js');
            worker.postMessage({
                type: 'init',
                sharedBuffer: this.sharedBuffer,
                wasmBuffer: buffer
            });
            this.workers.push(worker);
        }
    }

    async compute(inputData) {
        // Écrire les données d'entrée
        const view = new Int32Array(this.sharedBuffer);
        for (let i = 0; i < inputData.length; i++) {
            view[i] = inputData[i];
        }

        // Distribuer le travail aux workers
        const chunkSize = Math.ceil(inputData.length / this.workers.length);

        const promises = this.workers.map((worker, idx) => {
            return new Promise((resolve) => {
                worker.onmessage = (e) => {
                    if (e.data.type === 'done') {
                        resolve();
                    }
                };

                const start = idx * chunkSize;
                const end = Math.min(start + chunkSize, inputData.length);

                worker.postMessage({
                    type: 'compute',
                    start: start,
                    end: end
                });
            });
        });

        await Promise.all(promises);

        // Lire les résultats
        const results = [];
        const outputOffset = 1000; // Offset pour les données de sortie
        for (let i = 0; i < inputData.length; i++) {
            results.push(view[outputOffset + i]);
        }

        return results;
    }
}

// Utilisation
async function testSharedMemory() {
    const compute = new SharedMemoryCompute();
    await compute.init(4); // 4 workers

    const inputData = Array.from({ length: 1000 }, (_, i) => i);

    const start = performance.now();
    const results = await compute.compute(inputData);
    const elapsed = performance.now() - start;

    console.log('Calcul terminé en', elapsed, 'ms');
    console.log('Premiers résultats:', results.slice(0, 10));
}
```

## 9.9.12 Intégration avec des frameworks web

### React avec WebAssembly

```jsx
import React, { useState, useEffect } from 'react';

function WasmComponent() {
    const [wasm, setWasm] = useState(null);
    const [result, setResult] = useState(null);
    const [loading, setLoading] = useState(true);

    // Charger WASM au montage
    useEffect(() => {
        async function loadWasm() {
            try {
                const response = await fetch('/compute.wasm');
                const buffer = await response.arrayBuffer();
                const module = await WebAssembly.instantiate(buffer, {});
                setWasm(module.instance);
                setLoading(false);
            } catch (error) {
                console.error('Erreur WASM:', error);
                setLoading(false);
            }
        }

        loadWasm();
    }, []);

    // Fonction de calcul
    const handleCompute = (value) => {
        if (!wasm) return;

        const result = wasm.exports.compute(value);
        setResult(result);
    };

    if (loading) {
        return <div>Chargement du module WASM...</div>;
    }

    if (!wasm) {
        return <div>Erreur de chargement WASM</div>;
    }

    return (
        <div>
            <h2>Calculs WebAssembly</h2>
            <button onClick={() => handleCompute(1000)}>
                Calculer
            </button>
            {result !== null && (
                <div>Résultat: {result}</div>
            )}
        </div>
    );
}

export default WasmComponent;
```

### Vue.js avec WebAssembly

```vue
<template>
    <div class="wasm-component">
        <h2>Calculs WebAssembly</h2>

        <div v-if="loading">
            Chargement du module WASM...
        </div>

        <div v-else-if="error">
            Erreur: {{ error }}
        </div>

        <div v-else>
            <input v-model.number="inputValue" type="number">
            <button @click="compute">Calculer</button>

            <div v-if="result !== null">
                Résultat: {{ result }}
            </div>
        </div>
    </div>
</template>

<script>
export default {
    name: 'WasmComponent',

    data() {
        return {
            wasm: null,
            loading: true,
            error: null,
            inputValue: 100,
            result: null
        };
    },

    async mounted() {
        try {
            const response = await fetch('/compute.wasm');
            const buffer = await response.arrayBuffer();
            const module = await WebAssembly.instantiate(buffer, {});
            this.wasm = module.instance;
            this.loading = false;
        } catch (error) {
            this.error = error.message;
            this.loading = false;
        }
    },

    methods: {
        compute() {
            if (!this.wasm) return;

            this.result = this.wasm.exports.compute(this.inputValue);
        }
    }
};
</script>

<style scoped>
.wasm-component {
    padding: 20px;
    border: 1px solid #ccc;
    border-radius: 5px;
}

button {
    margin-left: 10px;
    padding: 5px 15px;
}

input {
    padding: 5px;
    width: 100px;
}
</style>
```

## 9.9.13 Différences multi-plateformes

### Compilation spécifique par plateforme

**Windows (build.bat) :**

```batch
@echo off
echo Compilation WebAssembly pour Windows...

REM Compiler le module WASM
fpc -Twasm -O3 -WmMyModule myprogram.pas

echo Compilation terminée.
```

**Ubuntu/Linux (build.sh) :**

```bash
#!/bin/bash
echo "Compilation WebAssembly pour Linux..."

# Compiler le module WASM
fpc -Twasm -O3 -WmMyModule myprogram.pas

echo "Compilation terminée."
```

Les modules WebAssembly compilés (`.wasm`) sont identiques quelle que soit la plateforme de compilation. Le fichier `.wasm` produit sous Windows fonctionnera exactement de la même manière sous Linux et inversement, car WebAssembly est un format portable par définition.

## Conclusion

WebAssembly avec FreePascal ouvre de nouvelles possibilités pour les développeurs Pascal :

1. **Performance proche du natif** dans le navigateur web
2. **Réutilisation de code** Pascal existant côté client
3. **Calculs intensifs** (images, mathématiques, cryptographie) déportés dans le navigateur
4. **Interopérabilité** avec JavaScript et les frameworks web modernes
5. **Portabilité** totale entre navigateurs et systèmes d'exploitation

La compilation vers WASM reste un domaine en évolution dans l'écosystème FreePascal, mais les fondations sont solides et les cas d'utilisation nombreux.

⏭️ [Intégration avec frameworks JavaScript](/09-programmation-web-freepascal/10-integration-frameworks-javascript.md)
