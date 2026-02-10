🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 19.8 Mono Interop (Linux) - Tutoriel pour Développeurs Avancés

## Table des matières

1. [Introduction à Mono](#introduction-à-mono)
2. [Installation et configuration](#installation-et-configuration)
3. [Différences avec .NET Windows](#différences-avec-net-windows)
4. [P/Invoke sous Linux](#pinvoke-sous-linux)
5. [Créer des bibliothèques partagées (.so)](#créer-des-bibliothèques-partagées-so)
6. [Marshalling Linux-spécifique](#marshalling-linux-spécifique)
7. [Gestion des chemins et conventions](#gestion-des-chemins-et-conventions)
8. [Interopérabilité avec les bibliothèques système](#interopérabilité-avec-les-bibliothèques-système)
9. [D-Bus et communication IPC](#d-bus-et-communication-ipc)
10. [Cas pratiques](#cas-pratiques)
11. [Débogage et diagnostics](#débogage-et-diagnostics)
12. [Conclusion](#conclusion)

---

## Introduction à Mono

### Qu'est-ce que Mono ?

**Mono** est une implémentation open source du framework .NET qui permet d'exécuter des applications C# et .NET sur Linux, macOS et d'autres systèmes Unix. C'est l'équivalent multiplateforme de .NET Framework.

**Analogie** : Si vous imaginez .NET Framework comme une application Windows exclusive, Mono est comme Wine pour .NET - il permet d'exécuter du code .NET sur d'autres systèmes d'exploitation.

### Évolution de l'écosystème .NET sur Linux

```
┌─────────────────────────────────────────────────┐
│  Historique de .NET sur Linux                   │
├─────────────────────────────────────────────────┤
│  2004-2016  │  Mono (Xamarin/Microsoft)         │
│  2016-2020  │  .NET Core 1.0 → 3.1              │
│  2020-...   │  .NET 5+ (unification)            │
└─────────────────────────────────────────────────┘
```

**Aujourd'hui en 2025** :
- **Mono** : Toujours utilisé, notamment pour Xamarin et Unity
- **.NET 6/7/8** : Solution moderne, officielle et recommandée par Microsoft
- Les deux supportent P/Invoke et l'interop native

### Architecture sous Linux

```
┌───────────────────────────────────────────────────┐
│         Application C# / Mono                     │
└────────────────┬──────────────────────────────────┘
                 │
         ┌───────▼────────┐
         │  Runtime Mono  │
         │  ou .NET CLR   │
         └───────┬────────┘
                 │
                 │ P/Invoke
                 │ ┌─────────────────┐
                 ├─┤ Bibliothèque .so│
                 │ │  (FreePascal)   │
                 │ └─────────────────┘
                 │
         ┌───────▼────────┐
         │  Kernel Linux  │
         └────────────────┘
```

### Pourquoi utiliser Mono/FreePascal sur Linux ?

#### 1. Performance native

Combiner la productivité de C# avec la performance du code natif Pascal pour :
- Calculs intensifs
- Traitement de données en temps réel
- Accès matériel bas niveau
- Algorithmes optimisés

#### 2. Réutilisation de code

**Scénario** : Vous avez une bibliothèque Pascal qui fonctionne sur Windows et Linux, et vous voulez l'utiliser depuis Mono/C#.

```pascal
// Même code Pascal, fonctionne partout
function CalculateComplex(x, y: Double): Double; cdecl; export;  
begin
  Result := // ... algorithme complexe
end;
```

```csharp
// Code C# multiplateforme
double result = NativeLib.CalculateComplex(10.5, 20.3);
```

#### 3. Intégration système Linux

- Services systemd
- D-Bus (communication inter-processus)
- Bibliothèques système (libc, libpthread, etc.)
- Périphériques et GPIO (Raspberry Pi)

---

## Installation et configuration

### Installer Mono sur Ubuntu

#### Méthode 1 : Depuis les dépôts Ubuntu

```bash
sudo apt update  
sudo apt install mono-complete
```

**Vérification** :
```bash
mono --version
# Devrait afficher : Mono JIT compiler version 6.x.x
```

#### Méthode 2 : Depuis les dépôts officiels Mono (version plus récente)

```bash
# Ajouter la clé GPG
sudo apt install gnupg ca-certificates  
sudo gpg --homedir /tmp --no-default-keyring --keyring /usr/share/keyrings/mono-official-archive-keyring.gpg --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 3FA7E0328081BFF6A14DA29AA6A19B38D3D831EF

# Ajouter le dépôt
echo "deb [signed-by=/usr/share/keyrings/mono-official-archive-keyring.gpg] https://download.mono-project.com/repo/ubuntu stable-focal main" | sudo tee /etc/apt/sources.list.d/mono-official-stable.list

# Installer
sudo apt update  
sudo apt install mono-complete
```

#### Méthode 3 : .NET SDK moderne (recommandé pour nouveaux projets)

```bash
# Ajouter le dépôt Microsoft
wget https://packages.microsoft.com/config/ubuntu/22.04/packages-microsoft-prod.deb -O packages-microsoft-prod.deb  
sudo dpkg -i packages-microsoft-prod.deb  
rm packages-microsoft-prod.deb

# Installer .NET SDK
sudo apt update  
sudo apt install -y dotnet-sdk-8.0

# Vérifier
dotnet --version
# Devrait afficher : 8.0.xxx
```

### Installer FreePascal/Lazarus sur Ubuntu

```bash
# FreePascal
sudo apt update  
sudo apt install fpc

# Lazarus (IDE complet)
sudo apt install lazarus

# Vérifier
fpc -version
# Free Pascal Compiler version 3.2.x
```

### Outils de développement

**Éditeur de code** :
```bash
# Visual Studio Code avec extensions
sudo snap install code --classic  
code --install-extension ms-dotnettools.csharp

# Ou MonoDevelop
sudo apt install monodevelop
```

**Outils de build** :
```bash
# Build essentials
sudo apt install build-essential

# Outils de débogage
sudo apt install gdb valgrind
```

---

## Différences avec .NET Windows

### Principales différences techniques

| Aspect | Windows (.NET Framework) | Linux (Mono/.NET) |
|--------|--------------------------|-------------------|
| **Bibliothèques natives** | `.dll` | `.so` (Shared Object) |
| **Convention d'appel** | Cdecl ou StdCall | Cdecl (standard) |
| **Séparateur de chemin** | `\` (backslash) | `/` (forward slash) |
| **Fin de ligne** | CRLF (`\r\n`) | LF (`\n`) |
| **Sensibilité à la casse** | Non (fichiers) | Oui (tout) |
| **Exécutables** | `.exe` | Pas d'extension ou `.out` |
| **Permissions** | ACL Windows | POSIX (rwxrwxrwx) |

### Chemins de bibliothèques

**Windows** :
```
C:\MyApp\native\mathlib.dll
```

**Linux** :
```
/usr/local/lib/libmathlib.so
/opt/myapp/lib/libmathlib.so
./libmathlib.so
```

### Convention de nommage

**Windows** : `mathlib.dll`

**Linux** :
- Format standard : `libmathlib.so` (préfixe `lib`)
- Versioning : `libmathlib.so.1.0.0`
- Lien symbolique : `libmathlib.so` → `libmathlib.so.1`

### Loader de bibliothèques

**Windows** : `LoadLibrary()` cherche dans :
1. Répertoire de l'exécutable
2. Répertoire système (System32)
3. PATH

**Linux** : `dlopen()` cherche dans :
1. Répertoire courant (si `./` spécifié)
2. `LD_LIBRARY_PATH`
3. `/etc/ld.so.cache` (configuré via `/etc/ld.so.conf`)
4. `/lib`, `/usr/lib`, `/usr/local/lib`

---

## P/Invoke sous Linux

### Structure de base

Le principe reste le même que sur Windows, avec quelques adaptations.

### Créer une bibliothèque Pascal pour Linux

**mathlib.pas** :

```pascal
library mathlib;

{$mode objfpc}{$H+}

uses
  SysUtils, Math;

// Addition simple
function Add(a, b: Integer): Integer; cdecl; export;  
begin
  Result := a + b;
end;

// Multiplication
function Multiply(a, b: Integer): Integer; cdecl; export;  
begin
  Result := a * b;
end;

// Puissance
function Power(base, exponent: Double): Double; cdecl; export;  
begin
  Result := Math.Power(base, exponent);
end;

// Somme d'un tableau
function SumArray(arr: PInteger; length: Integer): Integer; cdecl; export;  
var
  i: Integer;
  sum: Integer;
begin
  sum := 0;
  for i := 0 to length - 1 do
  begin
    sum := sum + arr^;
    Inc(arr);
  end;
  Result := sum;
end;

exports
  Add,
  Multiply,
  Power,
  SumArray;

begin  
end.
```

**Compilation sous Linux** :

```bash
# Compiler en bibliothèque partagée
fpc -olibmathlib.so mathlib.pas

# Vérifier les exports
nm -D libmathlib.so | grep -E "Add|Multiply|Power|SumArray"

# Vérifier les dépendances
ldd libmathlib.so
```

**Options de compilation importantes** :

```bash
# Optimisation maximale
fpc -O3 -olibmathlib.so mathlib.pas

# Position Independent Code (requis pour .so)
fpc -fPIC -olibmathlib.so mathlib.pas

# Stripping des symboles (production)
fpc -Xs -olibmathlib.so mathlib.pas
```

### Code C# pour Mono/Linux

**Program.cs** :

```csharp
using System;  
using System.Runtime.InteropServices;

namespace MathLibDemo
{
    /// <summary>
    /// Imports P/Invoke pour la bibliothèque native
    /// </summary>
    internal static class NativeMethods
    {
        // IMPORTANT : Nom de la bibliothèque sans "lib" et sans ".so"
        private const string LIB_NAME = "mathlib";

        [DllImport(LIB_NAME, CallingConvention = CallingConvention.Cdecl)]
        public static extern int Add(int a, int b);

        [DllImport(LIB_NAME, CallingConvention = CallingConvention.Cdecl)]
        public static extern int Multiply(int a, int b);

        [DllImport(LIB_NAME, CallingConvention = CallingConvention.Cdecl)]
        public static extern double Power(double baseValue, double exponent);

        [DllImport(LIB_NAME, CallingConvention = CallingConvention.Cdecl)]
        public static extern int SumArray(int[] arr, int length);
    }

    class Program
    {
        static void Main()
        {
            Console.WriteLine("=== Test P/Invoke sous Linux avec Mono ===\n");

            try
            {
                // Test addition
                int sum = NativeMethods.Add(15, 27);
                Console.WriteLine($"Addition : 15 + 27 = {sum}");

                // Test multiplication
                int product = NativeMethods.Multiply(12, 8);
                Console.WriteLine($"Multiplication : 12 × 8 = {product}");

                // Test puissance
                double power = NativeMethods.Power(2, 16);
                Console.WriteLine($"Puissance : 2^16 = {power}");

                // Test tableau
                int[] numbers = { 10, 20, 30, 40, 50 };
                int total = NativeMethods.SumArray(numbers, numbers.Length);
                Console.WriteLine($"Somme du tableau : {total}");

                Console.WriteLine("\n✓ Tous les tests réussis!");
            }
            catch (DllNotFoundException ex)
            {
                Console.WriteLine($"\n✗ Erreur : Bibliothèque non trouvée");
                Console.WriteLine($"Message : {ex.Message}");
                Console.WriteLine("\nAssurez-vous que libmathlib.so est dans:");
                Console.WriteLine("  - Le répertoire courant");
                Console.WriteLine("  - /usr/local/lib");
                Console.WriteLine("  - Un répertoire dans LD_LIBRARY_PATH");
            }
            catch (Exception ex)
            {
                Console.WriteLine($"\n✗ Erreur : {ex.Message}");
            }
        }
    }
}
```

**Compilation avec Mono** :

```bash
# Compiler avec mcs (Mono C# compiler)
mcs Program.cs -out:MathDemo.exe

# Ou avec dotnet (recommandé)
dotnet new console -n MathDemo  
cd MathDemo
# Copier Program.cs
dotnet build
```

**Exécution** :

```bash
# Avec Mono
mono MathDemo.exe

# Avec dotnet
dotnet run

# Ou rendre exécutable (avec dotnet publish)
chmod +x MathDemo
./MathDemo
```

### Configuration du chemin de bibliothèque

#### Méthode 1 : Copier dans le répertoire de l'application

```bash
cp libmathlib.so /chemin/vers/application/  
cd /chemin/vers/application  
mono MathDemo.exe
```

#### Méthode 2 : Installer dans le système

```bash
# Copier dans /usr/local/lib
sudo cp libmathlib.so /usr/local/lib/

# Mettre à jour le cache du loader
sudo ldconfig

# Vérifier
ldconfig -p | grep mathlib
```

#### Méthode 3 : Variable d'environnement LD_LIBRARY_PATH

```bash
# Temporaire (session courante)
export LD_LIBRARY_PATH=/chemin/vers/lib:$LD_LIBRARY_PATH  
mono MathDemo.exe

# Permanent (ajouter à ~/.bashrc)
echo 'export LD_LIBRARY_PATH=/opt/myapp/lib:$LD_LIBRARY_PATH' >> ~/.bashrc  
source ~/.bashrc
```

#### Méthode 4 : Configuration rpath (recommandé pour distribution)

```bash
# Compiler avec rpath
fpc -k'-rpath=$ORIGIN' -olibmathlib.so mathlib.pas

# $ORIGIN = répertoire de l'exécutable
# La bibliothèque sera cherchée dans le même dossier
```

---

## Créer des bibliothèques partagées (.so)

### Anatomie d'une bibliothèque .so

Une bibliothèque partagée Linux (Shared Object) est similaire à une DLL Windows mais avec des spécificités.

### Structure complète d'un projet

```
myproject/
├── src/
│   ├── mathlib.pas          # Code source Pascal
│   └── mathlib.lpr          # Projet Lazarus (optionnel)
├── build/
│   └── libmathlib.so        # Bibliothèque compilée
├── csharp/
│   ├── Program.cs           # Code C#
│   └── MathDemo.csproj      # Projet .NET
└── Makefile                 # Automatisation
```

### Makefile pour automatisation

**Makefile** :

```makefile
# Variables
FPC = fpc  
FPC_FLAGS = -O3 -fPIC -CX  
LIB_NAME = libmathlib.so  
SRC = src/mathlib.pas  
BUILD_DIR = build

# Cibles
all: $(BUILD_DIR)/$(LIB_NAME)

$(BUILD_DIR)/$(LIB_NAME): $(SRC)
	@mkdir -p $(BUILD_DIR)
	$(FPC) $(FPC_FLAGS) -o$(BUILD_DIR)/$(LIB_NAME) $(SRC)
	@echo "✓ Bibliothèque compilée : $(BUILD_DIR)/$(LIB_NAME)"

install: $(BUILD_DIR)/$(LIB_NAME)
	sudo cp $(BUILD_DIR)/$(LIB_NAME) /usr/local/lib/
	sudo ldconfig
	@echo "✓ Bibliothèque installée dans /usr/local/lib"

clean:
	rm -rf $(BUILD_DIR)
	rm -f *.o *.ppu
	@echo "✓ Nettoyage effectué"

test: $(BUILD_DIR)/$(LIB_NAME)
	cd csharp && dotnet run
	@echo "✓ Tests exécutés"

.PHONY: all install clean test
```

**Utilisation** :

```bash
# Compiler
make

# Installer dans le système
make install

# Nettoyer
make clean

# Compiler et tester
make test
```

### Versioning des bibliothèques

Linux utilise un système de versioning sophistiqué pour les bibliothèques.

**Schéma de versioning** :

```
libmathlib.so.MAJOR.MINOR.PATCH
```

- **MAJOR** : Changements incompatibles de l'API
- **MINOR** : Ajout de fonctionnalités compatibles
- **PATCH** : Corrections de bugs

**Exemple de structure** :

```bash
# Fichier réel avec version complète
libmathlib.so.1.2.3

# Lien symbolique pour la compatibilité MAJOR
libmathlib.so.1 -> libmathlib.so.1.2.3

# Lien symbolique pour le développement
libmathlib.so -> libmathlib.so.1
```

**Script de déploiement** :

```bash
#!/bin/bash
# deploy.sh

VERSION="1.2.3"  
LIB_BASE="libmathlib.so"  
LIB_FULL="${LIB_BASE}.${VERSION}"  
INSTALL_DIR="/usr/local/lib"

# Compiler avec version
fpc -O3 -fPIC -o${LIB_FULL} mathlib.pas

# Installer
sudo cp ${LIB_FULL} ${INSTALL_DIR}/  
cd ${INSTALL_DIR}

# Créer les liens symboliques
sudo ln -sf ${LIB_FULL} ${LIB_BASE}.1  
sudo ln -sf ${LIB_BASE}.1 ${LIB_BASE}

# Mettre à jour le cache
sudo ldconfig

echo "✓ ${LIB_FULL} installé et configuré"
```

---

## Marshalling Linux-spécifique

### Types de base

Les types de base sont identiques à Windows, mais avec quelques particularités.

**Correspondance des types** :

```pascal
// Pascal (FreePascal)
Integer   // 4 octets sur x86/x64 Linux  
Int64     // 8 octets  
PtrInt    // 4 octets (32-bit) ou 8 octets (64-bit)  
PtrUInt   // Non signé, taille dépend de l'architecture
```

```csharp
// C# / Mono
int       // 4 octets (toujours)  
long      // 8 octets (toujours)  
IntPtr    // 4 ou 8 octets selon l'architecture
```

### Structures et alignement

**Pascal** :

```pascal
type
  // Structure packed (pas d'alignement)
  TPoint = packed record
    X: Integer;
    Y: Integer;
  end;

  // Structure avec alignement par défaut
  TData = record
    Flag: Boolean;      // 1 octet + 3 octets de padding
    Value: Integer;     // 4 octets
    Name: array[0..31] of AnsiChar;  // 32 octets
  end;
```

**C#** :

```csharp
// Structure packed (correspondance exacte)
[StructLayout(LayoutKind.Sequential, Pack = 1)]
public struct Point
{
    public int X;
    public int Y;
}

// Structure avec alignement naturel
[StructLayout(LayoutKind.Sequential)]
public struct Data
{
    [MarshalAs(UnmanagedType.I1)]
    public bool Flag;

    public int Value;

    [MarshalAs(UnmanagedType.ByValArray, SizeConst = 32)]
    public byte[] Name;
}
```

### Chaînes de caractères sous Linux

**Encodages courants** :
- **UTF-8** : Standard sur Linux moderne
- **ASCII** : Pour compatibilité
- **Latin-1** : Pour certains systèmes européens

**Pascal - Chaînes UTF-8** :

```pascal
{$codepage UTF8}

function GetUTF8Message: PAnsiChar; cdecl; export;  
const
  MSG: AnsiString = 'Bonjour Linux! 🐧';
begin
  Result := PAnsiChar(MSG);
end;

procedure ProcessUTF8String(str: PAnsiChar); cdecl; export;  
var
  s: AnsiString;
begin
  s := str;
  WriteLn('Reçu depuis C# : ', s);
end;
```

**C# - Chaînes UTF-8** :

```csharp
[DllImport("libmathlib.so", CallingConvention = CallingConvention.Cdecl)]
[return: MarshalAs(UnmanagedType.CustomMarshaler,
         MarshalerType = "System.Runtime.InteropServices.UTF8Marshaler")]
public static extern string GetUTF8Message();

[DllImport("libmathlib.so", CallingConvention = CallingConvention.Cdecl)]
public static extern void ProcessUTF8String(
    [MarshalAs(UnmanagedType.LPUTF8Str)] string str);

// Utilisation
string message = GetUTF8Message();  
Console.WriteLine(message);  // Affiche correctement les émojis

ProcessUTF8String("Message C# avec émojis: 🚀🌟");
```

### Pointeurs et mémoire partagée

**Pascal** :

```pascal
// Allouer de la mémoire partagée
function AllocateBuffer(size: PtrInt): Pointer; cdecl; export;  
begin
  GetMem(Result, size);
  FillChar(Result^, size, 0);
end;

// Libérer la mémoire
procedure FreeBuffer(buffer: Pointer); cdecl; export;  
begin
  if buffer <> nil then
    FreeMem(buffer);
end;

// Écrire dans le buffer
procedure WriteToBuffer(buffer: Pointer; offset: PtrInt;
  value: Integer); cdecl; export;
var
  p: PInteger;
begin
  p := buffer;
  Inc(p, offset);
  p^ := value;
end;
```

**C#** :

```csharp
[DllImport("libmathlib.so")]
public static extern IntPtr AllocateBuffer(IntPtr size);

[DllImport("libmathlib.so")]
public static extern void FreeBuffer(IntPtr buffer);

[DllImport("libmathlib.so")]
public static extern void WriteToBuffer(IntPtr buffer, IntPtr offset, int value);

// Utilisation sécurisée
public class NativeBuffer : IDisposable
{
    private IntPtr _buffer;
    private int _size;

    public NativeBuffer(int size)
    {
        _size = size;
        _buffer = NativeMethods.AllocateBuffer(new IntPtr(size));
        if (_buffer == IntPtr.Zero)
            throw new OutOfMemoryException("Impossible d'allouer le buffer");
    }

    public void WriteInt(int offset, int value)
    {
        if (offset < 0 || offset >= _size / sizeof(int))
            throw new ArgumentOutOfRangeException(nameof(offset));

        NativeMethods.WriteToBuffer(_buffer, new IntPtr(offset), value);
    }

    public void Dispose()
    {
        if (_buffer != IntPtr.Zero)
        {
            NativeMethods.FreeBuffer(_buffer);
            _buffer = IntPtr.Zero;
        }
        GC.SuppressFinalize(this);
    }

    ~NativeBuffer()
    {
        Dispose();
    }
}

// Utilisation
using (var buffer = new NativeBuffer(1024))
{
    buffer.WriteInt(0, 42);
    buffer.WriteInt(1, 100);
    // Libération automatique
}
```

---

## Gestion des chemins et conventions

### Chemins de fichiers portables

**Code qui fonctionne sur Windows et Linux** :

```csharp
using System;  
using System.IO;  
using System.Runtime.InteropServices;

public class PathHelper
{
    // Détecter le système d'exploitation
    public static bool IsLinux => RuntimeInformation.IsOSPlatform(OSPlatform.Linux);
    public static bool IsWindows => RuntimeInformation.IsOSPlatform(OSPlatform.Windows);

    // Obtenir le nom de bibliothèque correct
    public static string GetLibraryName(string baseName)
    {
        if (IsWindows)
            return $"{baseName}.dll";
        else if (IsLinux)
            return $"lib{baseName}.so";
        else
            return $"lib{baseName}.dylib";  // macOS
    }

    // Construire un chemin portable
    public static string BuildPath(params string[] parts)
    {
        return Path.Combine(parts);
    }

    // Obtenir le répertoire de l'application
    public static string GetAppDirectory()
    {
        return AppContext.BaseDirectory;
    }
}

// Utilisation
string libPath = Path.Combine(
    PathHelper.GetAppDirectory(),
    "native",
    PathHelper.GetLibraryName("mathlib")
);
Console.WriteLine($"Chemin de la bibliothèque : {libPath}");
// Windows : C:\MyApp\native\mathlib.dll
// Linux   : /home/user/MyApp/native/libmathlib.so
```

### DllImport multiplateforme

**Approche 1 : Compilation conditionnelle** :

```csharp
internal static class NativeMethods
{
#if WINDOWS
    private const string LIB_NAME = "mathlib.dll";
#elif LINUX
    private const string LIB_NAME = "libmathlib.so";
#else
    private const string LIB_NAME = "libmathlib.dylib";
#endif

    [DllImport(LIB_NAME, CallingConvention = CallingConvention.Cdecl)]
    public static extern int Add(int a, int b);
}
```

**Approche 2 : Détection à l'exécution** :

```csharp
public static class NativeLibrary
{
    private static IntPtr _handle;

    static NativeLibrary()
    {
        string libName = GetPlatformLibraryName("mathlib");
        _handle = LoadLibrary(libName);

        if (_handle == IntPtr.Zero)
            throw new DllNotFoundException($"Impossible de charger {libName}");
    }

    private static string GetPlatformLibraryName(string baseName)
    {
        if (RuntimeInformation.IsOSPlatform(OSPlatform.Windows))
            return $"{baseName}.dll";
        else if (RuntimeInformation.IsOSPlatform(OSPlatform.Linux))
            return $"lib{baseName}.so";
        else
            return $"lib{baseName}.dylib";
    }

    [DllImport("libdl.so.2")]  // Linux
    private static extern IntPtr dlopen(string filename, int flags);

    [DllImport("kernel32.dll", SetLastError = true)]  // Windows
    private static extern IntPtr LoadLibraryEx(string lpFileName, IntPtr hFile, uint dwFlags);

    private static IntPtr LoadLibrary(string libName)
    {
        if (RuntimeInformation.IsOSPlatform(OSPlatform.Linux))
            return dlopen(libName, 2);  // RTLD_NOW
        else
            return LoadLibraryEx(libName, IntPtr.Zero, 0);
    }
}
```

### Configuration selon l'environnement

**appsettings.json** :

```json
{
  "NativeLibraries": {
    "SearchPaths": [
      "./native",
      "/usr/local/lib",
      "/opt/myapp/lib"
    ],
    "Libraries": {
      "Math": {
        "Windows": "mathlib.dll",
        "Linux": "libmathlib.so",
        "macOS": "libmathlib.dylib"
      }
    }
  }
}
```

**Chargement de configuration** :

```csharp
using System.Text.Json;  
using System.IO;

public class LibraryConfig
{
    public static string GetLibraryPath(string libraryName)
    {
        string configPath = Path.Combine(
            AppContext.BaseDirectory,
            "appsettings.json"
        );

        string json = File.ReadAllText(configPath);
        using JsonDocument doc = JsonDocument.Parse(json);

        string platform = GetCurrentPlatform();
        string libFileName = doc.RootElement
            .GetProperty("NativeLibraries")
            .GetProperty("Libraries")
            .GetProperty(libraryName)
            .GetProperty(platform)
            .GetString();

        // Chercher dans les chemins configurés
        var searchPaths = doc.RootElement
            .GetProperty("NativeLibraries")
            .GetProperty("SearchPaths");

        foreach (JsonElement pathElement in searchPaths.EnumerateArray())
        {
            string searchPath = pathElement.GetString();
            string fullPath = Path.Combine(
                AppContext.BaseDirectory,
                searchPath,
                libFileName
            );

            if (File.Exists(fullPath))
                return fullPath;
        }

        throw new FileNotFoundException(
            $"Bibliothèque {libFileName} introuvable dans les chemins de recherche");
    }

    private static string GetCurrentPlatform()
    {
        if (RuntimeInformation.IsOSPlatform(OSPlatform.Windows))
            return "Windows";
        else if (RuntimeInformation.IsOSPlatform(OSPlatform.Linux))
            return "Linux";
        else if (RuntimeInformation.IsOSPlatform(OSPlatform.OSX))
            return "macOS";
        else
            throw new PlatformNotSupportedException("Plateforme non supportée");
    }
}

// Utilisation
string libPath = LibraryConfig.GetLibraryPath("Math");  
Console.WriteLine($"Bibliothèque trouvée : {libPath}");
```

---

## Interopérabilité avec les bibliothèques système

Linux fournit de nombreuses bibliothèques système natives que vous pouvez utiliser depuis Mono/C#.

### Utiliser libc (bibliothèque C standard)

**Exemples de fonctions libc** :

```csharp
using System;  
using System.Runtime.InteropServices;

public static class LibC
{
    // Obtenir l'ID du processus
    [DllImport("libc.so.6")]
    public static extern int getpid();

    // Obtenir l'ID de l'utilisateur
    [DllImport("libc.so.6")]
    public static extern int getuid();

    // Obtenir le nom d'hôte
    [DllImport("libc.so.6")]
    public static extern int gethostname(
        [MarshalAs(UnmanagedType.LPArray)] byte[] name,
        int len
    );

    // Dormir (sleep)
    [DllImport("libc.so.6")]
    public static extern uint sleep(uint seconds);
}

// Utilisation
class Program
{
    static void Main()
    {
        // PID
        int pid = LibC.getpid();
        Console.WriteLine($"Process ID : {pid}");

        // UID
        int uid = LibC.getuid();
        Console.WriteLine($"User ID : {uid}");

        // Hostname
        byte[] hostname = new byte[256];
        LibC.gethostname(hostname, hostname.Length);
        string name = System.Text.Encoding.UTF8.GetString(hostname)
            .TrimEnd('\0');
        Console.WriteLine($"Hostname : {name}");

        // Sleep
        Console.WriteLine("Attente de 2 secondes...");
        LibC.sleep(2);
        Console.WriteLine("Terminé!");
    }
}
```

### Interagir avec le système de fichiers

**Pascal - Opérations fichiers** :

```pascal
library fileops;

{$mode objfpc}{$H+}

uses
  SysUtils, BaseUnix;

// Vérifier si un fichier existe
function FileExists(path: PAnsiChar): Boolean; cdecl; export;  
begin
  Result := SysUtils.FileExists(string(path));
end;

// Obtenir la taille d'un fichier
function GetFileSize(path: PAnsiChar): Int64; cdecl; export;  
var
  info: stat;
begin
  if FpStat(string(path), info) = 0 then
    Result := info.st_size
  else
    Result := -1;
end;

// Obtenir les permissions d'un fichier (mode Unix)
function GetFileMode(path: PAnsiChar): Integer; cdecl; export;  
var
  info: stat;
begin
  if FpStat(string(path), info) = 0 then
    Result := info.st_mode
  else
    Result := -1;
end;

// Changer les permissions
function SetFileMode(path: PAnsiChar; mode: Integer): Boolean; cdecl; export;  
begin
  Result := FpChmod(string(path), mode) = 0;
end;

exports
  FileExists,
  GetFileSize,
  GetFileMode,
  SetFileMode;

begin  
end.
```

**C# - Wrapper** :

```csharp
using System;  
using System.Runtime.InteropServices;

public static class FileOps
{
    [DllImport("libfileops.so")]
    [return: MarshalAs(UnmanagedType.I1)]
    public static extern bool FileExists(string path);

    [DllImport("libfileops.so")]
    public static extern long GetFileSize(string path);

    [DllImport("libfileops.so")]
    public static extern int GetFileMode(string path);

    [DllImport("libfileops.so")]
    [return: MarshalAs(UnmanagedType.I1)]
    public static extern bool SetFileMode(string path, int mode);
}

// Classe helper pour les permissions Unix
public static class UnixPermissions
{
    // Constantes de permissions
    public const int S_IRUSR = 0x100;  // Lecture propriétaire
    public const int S_IWUSR = 0x080;  // Écriture propriétaire
    public const int S_IXUSR = 0x040;  // Exécution propriétaire

    public const int S_IRGRP = 0x020;  // Lecture groupe
    public const int S_IWGRP = 0x010;  // Écriture groupe
    public const int S_IXGRP = 0x008;  // Exécution groupe

    public const int S_IROTH = 0x004;  // Lecture autres
    public const int S_IWOTH = 0x002;  // Écriture autres
    public const int S_IXOTH = 0x001;  // Exécution autres

    // Modes courants
    public const int MODE_0644 = S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH;  // rw-r--r--
    public const int MODE_0755 = S_IRUSR | S_IWUSR | S_IXUSR | S_IRGRP | S_IXGRP | S_IROTH | S_IXOTH;  // rwxr-xr-x

    public static string FormatMode(int mode)
    {
        char[] perms = new char[9];
        perms[0] = (mode & S_IRUSR) != 0 ? 'r' : '-';
        perms[1] = (mode & S_IWUSR) != 0 ? 'w' : '-';
        perms[2] = (mode & S_IXUSR) != 0 ? 'x' : '-';
        perms[3] = (mode & S_IRGRP) != 0 ? 'r' : '-';
        perms[4] = (mode & S_IWGRP) != 0 ? 'w' : '-';
        perms[5] = (mode & S_IXGRP) != 0 ? 'x' : '-';
        perms[6] = (mode & S_IROTH) != 0 ? 'r' : '-';
        perms[7] = (mode & S_IWOTH) != 0 ? 'w' : '-';
        perms[8] = (mode & S_IXOTH) != 0 ? 'x' : '-';
        return new string(perms);
    }
}

// Utilisation
class Program
{
    static void Main()
    {
        string testFile = "/tmp/test.txt";

        if (FileOps.FileExists(testFile))
        {
            long size = FileOps.GetFileSize(testFile);
            Console.WriteLine($"Taille : {size} octets");

            int mode = FileOps.GetFileMode(testFile);
            string perms = UnixPermissions.FormatMode(mode);
            Console.WriteLine($"Permissions : {perms}");

            // Changer en rw-r--r-- (644)
            if (FileOps.SetFileMode(testFile, UnixPermissions.MODE_0644))
                Console.WriteLine("Permissions modifiées avec succès");
        }
    }
}
```

### Accès aux informations système

**Pascal** :

```pascal
library sysinfo;

{$mode objfpc}{$H+}

uses
  SysUtils, Unix, BaseUnix;

type
  TSystemInfo = packed record
    Hostname: array[0..255] of AnsiChar;
    Username: array[0..31] of AnsiChar;
    HomeDir: array[0..255] of AnsiChar;
    ProcessID: Integer;
    ParentProcessID: Integer;
    UserID: Integer;
    GroupID: Integer;
  end;

function GetSystemInfo: TSystemInfo; cdecl; export;  
var
  info: TSystemInfo;
  buf: array[0..255] of AnsiChar;
begin
  FillChar(info, SizeOf(info), 0);

  // Hostname
  if FpGetHostName(@buf[0], 256) = 0 then
    StrPCopy(info.Hostname, buf);

  // Username
  StrPCopy(info.Username, GetEnvironmentVariable('USER'));

  // Home directory
  StrPCopy(info.HomeDir, GetEnvironmentVariable('HOME'));

  // Process IDs
  info.ProcessID := FpGetPID;
  info.ParentProcessID := FpGetPPID;

  // User/Group IDs
  info.UserID := FpGetUID;
  info.GroupID := FpGetGID;

  Result := info;
end;

// Obtenir le nombre de CPUs
function GetCPUCount: Integer; cdecl; export;  
begin
  Result := GetCPUCount;  // Fonction FPC intégrée
end;

// Obtenir le uptime du système (en secondes)
function GetUptime: Int64; cdecl; export;  
var
  f: TextFile;
  line: string;
  uptime: Double;
begin
  Result := -1;
  try
    AssignFile(f, '/proc/uptime');
    Reset(f);
    ReadLn(f, line);
    CloseFile(f);

    // Format: "uptime_seconds idle_seconds"
    uptime := StrToFloatDef(Copy(line, 1, Pos(' ', line) - 1), -1);
    Result := Trunc(uptime);
  except
    Result := -1;
  end;
end;

exports
  GetSystemInfo,
  GetCPUCount,
  GetUptime;

begin  
end.
```

**C#** :

```csharp
using System;  
using System.Runtime.InteropServices;

[StructLayout(LayoutKind.Sequential, Pack = 1, CharSet = CharSet.Ansi)]
public struct SystemInfo
{
    [MarshalAs(UnmanagedType.ByValTStr, SizeConst = 256)]
    public string Hostname;

    [MarshalAs(UnmanagedType.ByValTStr, SizeConst = 32)]
    public string Username;

    [MarshalAs(UnmanagedType.ByValTStr, SizeConst = 256)]
    public string HomeDir;

    public int ProcessID;
    public int ParentProcessID;
    public int UserID;
    public int GroupID;
}

public static class SysInfo
{
    [DllImport("libsysinfo.so")]
    public static extern SystemInfo GetSystemInfo();

    [DllImport("libsysinfo.so")]
    public static extern int GetCPUCount();

    [DllImport("libsysinfo.so")]
    public static extern long GetUptime();
}

// Utilisation
class Program
{
    static void Main()
    {
        Console.WriteLine("=== Informations système Linux ===\n");

        SystemInfo info = SysInfo.GetSystemInfo();

        Console.WriteLine($"Hostname     : {info.Hostname}");
        Console.WriteLine($"Username     : {info.Username}");
        Console.WriteLine($"Home Dir     : {info.HomeDir}");
        Console.WriteLine($"Process ID   : {info.ProcessID}");
        Console.WriteLine($"Parent PID   : {info.ParentProcessID}");
        Console.WriteLine($"User ID      : {info.UserID}");
        Console.WriteLine($"Group ID     : {info.GroupID}");
        Console.WriteLine($"CPU Count    : {SysInfo.GetCPUCount()}");

        long uptime = SysInfo.GetUptime();
        TimeSpan uptimeSpan = TimeSpan.FromSeconds(uptime);
        Console.WriteLine($"Uptime       : {uptimeSpan.Days}j {uptimeSpan.Hours}h {uptimeSpan.Minutes}m");
    }
}
```

---

## D-Bus et communication IPC

**D-Bus** est le système de communication inter-processus standard sur Linux. Il permet aux applications de communiquer entre elles.

### Introduction à D-Bus

**D-Bus** fournit :
- **Bus système** : Communication avec les services système (NetworkManager, systemd, etc.)
- **Bus session** : Communication entre applications de l'utilisateur
- **Notifications** : Système de notifications desktop
- **Méthodes RPC** : Appels de méthodes distantes

### Utiliser D-Bus depuis Pascal

**Installation de la bibliothèque** :

```bash
# Installer les headers D-Bus
sudo apt install libdbus-1-dev

# Ou utiliser le binding FreePascal
lazbuild --add-package dbus_binding
```

**Pascal - Client D-Bus** :

```pascal
library dbusclient;

{$mode objfpc}{$H+}

uses
  SysUtils, dbus;

// Envoyer une notification desktop
function SendNotification(title, message: PAnsiChar;
  timeout: Integer): Boolean; cdecl; export;
var
  conn: PDBusConnection;
  msg: PDBusMessage;
  err: DBusError;
  serial: dbus_uint32_t;
begin
  Result := False;

  dbus_error_init(@err);

  // Se connecter au bus session
  conn := dbus_bus_get(DBUS_BUS_SESSION, @err);
  if dbus_error_is_set(@err) <> 0 then
  begin
    dbus_error_free(@err);
    Exit;
  end;

  // Créer un message de notification
  msg := dbus_message_new_method_call(
    'org.freedesktop.Notifications',
    '/org/freedesktop/Notifications',
    'org.freedesktop.Notifications',
    'Notify'
  );

  if msg = nil then Exit;

  try
    // Ajouter les arguments
    dbus_message_append_args(msg,
      DBUS_TYPE_STRING, @'MyApp',           // app_name
      DBUS_TYPE_UINT32, @0,                 // replaces_id
      DBUS_TYPE_STRING, @'',                // app_icon
      DBUS_TYPE_STRING, @title,             // summary
      DBUS_TYPE_STRING, @message,           // body
      DBUS_TYPE_ARRAY, DBUS_TYPE_STRING, @nil, 0,  // actions
      DBUS_TYPE_ARRAY, DBUS_TYPE_DICT_ENTRY, @nil, 0,  // hints
      DBUS_TYPE_INT32, @timeout,            // expire_timeout
      DBUS_TYPE_INVALID
    );

    // Envoyer le message
    if dbus_connection_send(conn, msg, @serial) <> 0 then
    begin
      dbus_connection_flush(conn);
      Result := True;
    end;

  finally
    dbus_message_unref(msg);
  end;
end;

exports
  SendNotification;

begin  
end.
```

**C# - Wrapper D-Bus** :

```csharp
using System;  
using System.Runtime.InteropServices;

public static class DBusNotifications
{
    [DllImport("libdbusclient.so")]
    [return: MarshalAs(UnmanagedType.I1)]
    public static extern bool SendNotification(
        string title,
        string message,
        int timeout
    );

    public static void Notify(string title, string message,
        int timeoutMs = 5000)
    {
        if (!SendNotification(title, message, timeoutMs))
        {
            throw new InvalidOperationException(
                "Impossible d'envoyer la notification D-Bus");
        }
    }
}

// Utilisation
class Program
{
    static void Main()
    {
        try
        {
            DBusNotifications.Notify(
                "Application C#",
                "Notification envoyée via D-Bus depuis Mono!",
                5000  // 5 secondes
            );

            Console.WriteLine("✓ Notification envoyée");
        }
        catch (Exception ex)
        {
            Console.WriteLine($"✗ Erreur : {ex.Message}");
        }
    }
}
```

### Alternative : Utiliser une bibliothèque C# pour D-Bus

**Tmds.DBus** est une bibliothèque .NET moderne pour D-Bus :

```bash
dotnet add package Tmds.DBus
```

**Code C# direct** :

```csharp
using System;  
using System.Threading.Tasks;  
using Tmds.DBus;

// Interface pour les notifications
[DBusInterface("org.freedesktop.Notifications")]
interface INotifications : IDBusObject
{
    Task<uint> NotifyAsync(
        string appName,
        uint replacesId,
        string appIcon,
        string summary,
        string body,
        string[] actions,
        IDictionary<string, object> hints,
        int expireTimeout
    );
}

class Program
{
    static async Task Main()
    {
        // Se connecter au bus session
        using var connection = Connection.Session;

        // Obtenir le proxy pour les notifications
        var notifications = connection.CreateProxy<INotifications>(
            "org.freedesktop.Notifications",
            "/org/freedesktop/Notifications"
        );

        // Envoyer une notification
        await notifications.NotifyAsync(
            "MonApp C#",              // app_name
            0,                        // replaces_id
            "",                       // app_icon
            "Titre de notification",  // summary
            "Message de la notification 🚀",  // body
            Array.Empty<string>(),    // actions
            new Dictionary<string, object>(),  // hints
            5000                      // timeout (ms)
        );

        Console.WriteLine("✓ Notification envoyée via Tmds.DBus");
    }
}
```

---

## Cas pratiques

### Cas 1 : Monitoring système en temps réel

**Objectif** : Créer un moniteur système qui utilise du code Pascal optimisé pour collecter les métriques, et C# pour l'affichage.

**Pascal - system_monitor.pas** :

```pascal
library system_monitor;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, Unix;

type
  TCPUStats = packed record
    User: Int64;
    Nice: Int64;
    System: Int64;
    Idle: Int64;
    Total: Int64;
  end;

  TMemoryStats = packed record
    Total: Int64;      // Ko
    Free: Int64;       // Ko
    Available: Int64;  // Ko
    Used: Int64;       // Ko
    Cached: Int64;     // Ko
  end;

// Lire les statistiques CPU depuis /proc/stat
function GetCPUStats: TCPUStats; cdecl; export;  
var
  f: TextFile;
  line: string;
  parts: TStringList;
begin
  FillChar(Result, SizeOf(Result), 0);
  parts := TStringList.Create;
  try
    AssignFile(f, '/proc/stat');
    Reset(f);
    ReadLn(f, line);  // Première ligne = CPU total
    CloseFile(f);

    parts.Delimiter := ' ';
    parts.StrictDelimiter := True;
    parts.DelimitedText := line;

    if parts.Count >= 5 then
    begin
      Result.User := StrToInt64Def(parts[1], 0);
      Result.Nice := StrToInt64Def(parts[2], 0);
      Result.System := StrToInt64Def(parts[3], 0);
      Result.Idle := StrToInt64Def(parts[4], 0);
      Result.Total := Result.User + Result.Nice + Result.System + Result.Idle;
    end;
  finally
    parts.Free;
  end;
end;

// Calculer le pourcentage d'utilisation CPU
function CalculateCPUUsage(prev, current: TCPUStats): Double; cdecl; export;  
var
  totalDelta, idleDelta: Int64;
begin
  totalDelta := current.Total - prev.Total;
  idleDelta := current.Idle - prev.Idle;

  if totalDelta = 0 then
    Result := 0
  else
    Result := 100.0 * (1.0 - idleDelta / totalDelta);
end;

// Lire les statistiques mémoire depuis /proc/meminfo
function GetMemoryStats: TMemoryStats; cdecl; export;  
var
  f: TextFile;
  line: string;
  key, value: string;
  pos: Integer;
begin
  FillChar(Result, SizeOf(Result), 0);

  try
    AssignFile(f, '/proc/meminfo');
    Reset(f);

    while not EOF(f) do
    begin
      ReadLn(f, line);
      pos := System.Pos(':', line);
      if pos > 0 then
      begin
        key := Trim(Copy(line, 1, pos - 1));
        value := Trim(Copy(line, pos + 1, Length(line)));
        value := Copy(value, 1, System.Pos(' ', value) - 1);

        if key = 'MemTotal' then
          Result.Total := StrToInt64Def(value, 0)
        else if key = 'MemFree' then
          Result.Free := StrToInt64Def(value, 0)
        else if key = 'MemAvailable' then
          Result.Available := StrToInt64Def(value, 0)
        else if key = 'Cached' then
          Result.Cached := StrToInt64Def(value, 0);
      end;
    end;

    CloseFile(f);
    Result.Used := Result.Total - Result.Available;
  except
    // Erreur de lecture
  end;
end;

exports
  GetCPUStats,
  CalculateCPUUsage,
  GetMemoryStats;

begin  
end.
```

**C# - Application de monitoring** :

```csharp
using System;  
using System.Runtime.InteropServices;  
using System.Threading;

[StructLayout(LayoutKind.Sequential, Pack = 1)]
public struct CPUStats
{
    public long User;
    public long Nice;
    public long System;
    public long Idle;
    public long Total;
}

[StructLayout(LayoutKind.Sequential, Pack = 1)]
public struct MemoryStats
{
    public long Total;
    public long Free;
    public long Available;
    public long Used;
    public long Cached;
}

public static class SystemMonitor
{
    [DllImport("libsystem_monitor.so")]
    public static extern CPUStats GetCPUStats();

    [DllImport("libsystem_monitor.so")]
    public static extern double CalculateCPUUsage(CPUStats prev, CPUStats current);

    [DllImport("libsystem_monitor.so")]
    public static extern MemoryStats GetMemoryStats();
}

class Program
{
    static void Main()
    {
        Console.Clear();
        Console.WriteLine("=== Moniteur Système Linux ===");
        Console.WriteLine("Appuyez sur Ctrl+C pour quitter\n");

        CPUStats prevCPU = SystemMonitor.GetCPUStats();

        while (true)
        {
            Thread.Sleep(1000);

            // CPU
            CPUStats currentCPU = SystemMonitor.GetCPUStats();
            double cpuUsage = SystemMonitor.CalculateCPUUsage(prevCPU, currentCPU);
            prevCPU = currentCPU;

            // Mémoire
            MemoryStats mem = SystemMonitor.GetMemoryStats();
            double memUsagePercent = 100.0 * mem.Used / mem.Total;

            // Affichage
            Console.SetCursorPosition(0, 3);
            Console.WriteLine($"CPU Usage    : {cpuUsage,6:F2}% {GetBar(cpuUsage)}");
            Console.WriteLine($"Memory Usage : {memUsagePercent,6:F2}% {GetBar(memUsagePercent)}");
            Console.WriteLine($"Memory Used  : {mem.Used / 1024,8:F0} MB / {mem.Total / 1024,8:F0} MB");
            Console.WriteLine($"Memory Free  : {mem.Available / 1024,8:F0} MB");
            Console.WriteLine($"Memory Cached: {mem.Cached / 1024,8:F0} MB");
        }
    }

    static string GetBar(double percent)
    {
        int width = 40;
        int filled = (int)(width * percent / 100);
        return "[" + new string('█', filled) + new string('░', width - filled) + "]";
    }
}
```

### Cas 2 : Traitement d'images avec accélération native

**Pascal - image_processing_linux.pas** :

```pascal
library image_processing_linux;

{$mode objfpc}{$H+}

uses
  SysUtils, Math;

type
  TRGB = packed record
    R, G, B: Byte;
  end;
  PRGB = ^TRGB;

// Appliquer un filtre Sobel (détection de contours)
procedure ApplySobelFilter(pixels: PRGB; width, height: Integer);
  cdecl; export;
const
  GX: array[0..2, 0..2] of Integer = (
    (-1, 0, 1),
    (-2, 0, 2),
    (-1, 0, 1)
  );
  GY: array[0..2, 0..2] of Integer = (
    (-1, -2, -1),
    ( 0,  0,  0),
    ( 1,  2,  1)
  );
var
  x, y, i, j: Integer;
  gxSum, gySum, magnitude: Integer;
  temp: array of TRGB;
  src, dst: PRGB;
begin
  SetLength(temp, width * height);
  Move(pixels^, temp[0], width * height * SizeOf(TRGB));

  // Appliquer le filtre Sobel
  for y := 1 to height - 2 do
  begin
    for x := 1 to width - 2 do
    begin
      gxSum := 0;
      gySum := 0;

      // Convoluer avec les kernels Gx et Gy
      for j := 0 to 2 do
      begin
        for i := 0 to 2 do
        begin
          src := @temp[(y + j - 1) * width + (x + i - 1)];
          // Utiliser seulement le canal rouge pour simplifier
          gxSum := gxSum + GX[j, i] * src^.R;
          gySum := gySum + GY[j, i] * src^.R;
        end;
      end;

      // Magnitude du gradient
      magnitude := Round(Sqrt(gxSum * gxSum + gySum * gySum));
      magnitude := EnsureRange(magnitude, 0, 255);

      // Écrire le résultat
      dst := pixels;
      Inc(dst, y * width + x);
      dst^.R := magnitude;
      dst^.G := magnitude;
      dst^.B := magnitude;
    end;
  end;
end;

// Redimensionner une image (bilinear interpolation)
procedure ResizeImage(srcPixels: PRGB; srcWidth, srcHeight: Integer;
  dstPixels: PRGB; dstWidth, dstHeight: Integer); cdecl; export;
var
  x, y: Integer;
  srcX, srcY: Double;
  x1, y1, x2, y2: Integer;
  dx, dy: Double;
  p1, p2, p3, p4: PRGB;
  r, g, b: Double;
  dst: PRGB;
begin
  for y := 0 to dstHeight - 1 do
  begin
    for x := 0 to dstWidth - 1 do
    begin
      // Coordonnées dans l'image source
      srcX := x * (srcWidth - 1) / (dstWidth - 1);
      srcY := y * (srcHeight - 1) / (dstHeight - 1);

      // Points voisins
      x1 := Floor(srcX);
      y1 := Floor(srcY);
      x2 := Min(x1 + 1, srcWidth - 1);
      y2 := Min(y1 + 1, srcHeight - 1);

      // Distances pour interpolation
      dx := srcX - x1;
      dy := srcY - y1;

      // Les 4 pixels voisins
      p1 := srcPixels;
      Inc(p1, y1 * srcWidth + x1);

      p2 := srcPixels;
      Inc(p2, y1 * srcWidth + x2);

      p3 := srcPixels;
      Inc(p3, y2 * srcWidth + x1);

      p4 := srcPixels;
      Inc(p4, y2 * srcWidth + x2);

      // Interpolation bilinéaire
      r := (1 - dx) * (1 - dy) * p1^.R +
           dx * (1 - dy) * p2^.R +
           (1 - dx) * dy * p3^.R +
           dx * dy * p4^.R;

      g := (1 - dx) * (1 - dy) * p1^.G +
           dx * (1 - dy) * p2^.G +
           (1 - dx) * dy * p3^.G +
           dx * dy * p4^.G;

      b := (1 - dx) * (1 - dy) * p1^.B +
           dx * (1 - dy) * p2^.B +
           (1 - dx) * dy * p3^.B +
           dx * dy * p4^.B;

      // Écrire le pixel de destination
      dst := dstPixels;
      Inc(dst, y * dstWidth + x);
      dst^.R := Round(r);
      dst^.G := Round(g);
      dst^.B := Round(b);
    end;
  end;
end;

exports
  ApplySobelFilter,
  ResizeImage;

begin  
end.
```

**C# - Application de traitement d'images** :

```csharp
using System;  
using System.Drawing;  
using System.Drawing.Imaging;  
using System.Runtime.InteropServices;  
using System.Diagnostics;

namespace ImageProcessing
{
    [StructLayout(LayoutKind.Sequential, Pack = 1)]
    public struct RGB
    {
        public byte R;
        public byte G;
        public byte B;
    }

    internal static class NativeImageOps
    {
        [DllImport("libimage_processing_linux.so")]
        public static extern void ApplySobelFilter(
            IntPtr pixels, int width, int height);

        [DllImport("libimage_processing_linux.so")]
        public static extern void ResizeImage(
            IntPtr srcPixels, int srcWidth, int srcHeight,
            IntPtr dstPixels, int dstWidth, int dstHeight);
    }

    public class ImageProcessor
    {
        /// <summary>
        /// Applique un filtre de détection de contours Sobel
        /// </summary>
        public static Bitmap ApplySobel(Bitmap source)
        {
            Bitmap result = new Bitmap(source.Width, source.Height,
                PixelFormat.Format24bppRgb);

            // Copier l'image source vers résultat
            using (Graphics g = Graphics.FromImage(result))
            {
                g.DrawImage(source, 0, 0);
            }

            BitmapData data = result.LockBits(
                new Rectangle(0, 0, result.Width, result.Height),
                ImageLockMode.ReadWrite,
                PixelFormat.Format24bppRgb);

            try
            {
                NativeImageOps.ApplySobelFilter(
                    data.Scan0,
                    result.Width,
                    result.Height);
            }
            finally
            {
                result.UnlockBits(data);
            }

            return result;
        }

        /// <summary>
        /// Redimensionne une image avec interpolation bilinéaire
        /// </summary>
        public static Bitmap Resize(Bitmap source, int newWidth, int newHeight)
        {
            Bitmap result = new Bitmap(newWidth, newHeight,
                PixelFormat.Format24bppRgb);

            BitmapData srcData = source.LockBits(
                new Rectangle(0, 0, source.Width, source.Height),
                ImageLockMode.ReadOnly,
                PixelFormat.Format24bppRgb);

            BitmapData dstData = result.LockBits(
                new Rectangle(0, 0, result.Width, result.Height),
                ImageLockMode.WriteOnly,
                PixelFormat.Format24bppRgb);

            try
            {
                NativeImageOps.ResizeImage(
                    srcData.Scan0, source.Width, source.Height,
                    dstData.Scan0, result.Width, result.Height);
            }
            finally
            {
                source.UnlockBits(srcData);
                result.UnlockBits(dstData);
            }

            return result;
        }
    }
}

class Program
{
    static void Main(string[] args)
    {
        if (args.Length < 1)
        {
            Console.WriteLine("Usage: mono ImageApp.exe <image.jpg>");
            return;
        }

        string inputPath = args[0];

        try
        {
            Console.WriteLine($"Chargement de {inputPath}...");
            using (Bitmap original = new Bitmap(inputPath))
            {
                Console.WriteLine($"Dimensions: {original.Width}x{original.Height}");

                // Test 1: Détection de contours
                Console.WriteLine("\n1. Application du filtre Sobel...");
                Stopwatch sw = Stopwatch.StartNew();
                using (Bitmap edges = ImageProcessor.ApplySobel(original))
                {
                    sw.Stop();
                    Console.WriteLine($"   Temps: {sw.ElapsedMilliseconds} ms");
                    edges.Save("output_edges.jpg");
                    Console.WriteLine("   Sauvegardé: output_edges.jpg");
                }

                // Test 2: Redimensionnement
                Console.WriteLine("\n2. Redimensionnement (50%)...");
                sw.Restart();
                using (Bitmap resized = ImageProcessor.Resize(
                    original,
                    original.Width / 2,
                    original.Height / 2))
                {
                    sw.Stop();
                    Console.WriteLine($"   Temps: {sw.ElapsedMilliseconds} ms");
                    Console.WriteLine($"   Nouvelles dimensions: {resized.Width}x{resized.Height}");
                    resized.Save("output_resized.jpg");
                    Console.WriteLine("   Sauvegardé: output_resized.jpg");
                }

                Console.WriteLine("\n✓ Traitement terminé!");
            }
        }
        catch (Exception ex)
        {
            Console.WriteLine($"\n✗ Erreur: {ex.Message}");
        }
    }
}
```

**Compilation et exécution** :

```bash
# Compiler la bibliothèque Pascal
fpc -O3 -fPIC -olibimage_processing_linux.so image_processing_linux.pas

# Copier dans /usr/local/lib
sudo cp libimage_processing_linux.so /usr/local/lib/  
sudo ldconfig

# Compiler le programme C#
mcs Program.cs -r:System.Drawing.dll -out:ImageApp.exe

# Exécuter
mono ImageApp.exe input.jpg
```

### Cas 3 : Service systemd avec interop

**Objectif** : Créer un service systemd qui utilise du code Pascal pour les opérations critiques.

**Pascal - service_worker.pas** :

```pascal
library service_worker;

{$mode objfpc}{$H+}

uses
  SysUtils, Unix, BaseUnix;

type
  TLogCallback = procedure(level: Integer; message: PAnsiChar); cdecl;

var
  LogCallback: TLogCallback = nil;

// Enregistrer un callback pour les logs
procedure RegisterLogCallback(callback: TLogCallback); cdecl; export;  
begin
  LogCallback := callback;
end;

// Helper pour envoyer des logs
procedure Log(level: Integer; const msg: string);  
begin
  if Assigned(LogCallback) then
    LogCallback(level, PAnsiChar(msg));
end;

// Fonction de travail principale
procedure DoWork(param: PAnsiChar): Integer; cdecl; export;  
var
  i: Integer;
  config: string;
begin
  config := string(param);
  Log(1, 'Démarrage du travail avec config: ' + config);

  try
    // Simuler un traitement
    for i := 1 to 10 do
    begin
      Sleep(1000);
      Log(1, Format('Progression: %d/10', [i]));
    end;

    Log(1, 'Travail terminé avec succès');
    Result := 0;
  except
    on E: Exception do
    begin
      Log(3, 'Erreur: ' + E.Message);
      Result := 1;
    end;
  end;
end;

// Vérifier l'état du système
function CheckSystemHealth: Boolean; cdecl; export;  
var
  loadAvg: array[0..2] of Double;
begin
  // Obtenir la charge système
  if getloadavg(@loadAvg[0], 3) = 3 then
  begin
    Log(1, Format('Load average: %.2f, %.2f, %.2f',
      [loadAvg[0], loadAvg[1], loadAvg[2]]));

    // Système OK si charge < 2.0
    Result := loadAvg[0] < 2.0;
  end
  else
    Result := False;
end;

exports
  RegisterLogCallback,
  DoWork,
  CheckSystemHealth;

begin  
end.
```

**C# - Service systemd** :

```csharp
using System;  
using System.Runtime.InteropServices;  
using System.Threading;  
using System.Threading.Tasks;

namespace SystemdService
{
    internal static class NativeWorker
    {
        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate void LogCallback(int level, string message);

        [DllImport("libservice_worker.so")]
        public static extern void RegisterLogCallback(LogCallback callback);

        [DllImport("libservice_worker.so")]
        public static extern int DoWork(string param);

        [DllImport("libservice_worker.so")]
        [return: MarshalAs(UnmanagedType.I1)]
        public static extern bool CheckSystemHealth();
    }

    class ServiceWorker
    {
        private static NativeWorker.LogCallback _logCallback;
        private static bool _running = true;

        static void Main(string[] args)
        {
            // Installer le callback de logs
            _logCallback = OnNativeLog;
            NativeWorker.RegisterLogCallback(_logCallback);

            // Gérer Ctrl+C
            Console.CancelKeyPress += (sender, e) =>
            {
                e.Cancel = true;
                _running = false;
                Console.WriteLine("\nArrêt en cours...");
            };

            Console.WriteLine("=== Service systemd démarré ===");
            LogInfo("Service initialisé");

            // Boucle principale
            while (_running)
            {
                try
                {
                    // Vérifier la santé du système
                    if (!NativeWorker.CheckSystemHealth())
                    {
                        LogWarning("Charge système élevée, attente...");
                        Thread.Sleep(5000);
                        continue;
                    }

                    // Effectuer le travail
                    LogInfo("Démarrage d'une tâche de travail");
                    int result = NativeWorker.DoWork("/etc/myservice/config.conf");

                    if (result == 0)
                        LogInfo("Tâche terminée avec succès");
                    else
                        LogError("Tâche terminée avec erreur");

                    // Attendre avant la prochaine itération
                    Thread.Sleep(30000);  // 30 secondes
                }
                catch (Exception ex)
                {
                    LogError($"Exception: {ex.Message}");
                    Thread.Sleep(5000);
                }
            }

            LogInfo("Service arrêté proprement");
            Console.WriteLine("Service terminé.");
        }

        static void OnNativeLog(int level, string message)
        {
            string timestamp = DateTime.Now.ToString("yyyy-MM-dd HH:mm:ss");
            string levelStr = level switch
            {
                0 => "DEBUG",
                1 => "INFO",
                2 => "WARNING",
                3 => "ERROR",
                _ => "UNKNOWN"
            };

            Console.WriteLine($"[{timestamp}] [{levelStr}] {message}");
        }

        static void LogInfo(string message) =>
            OnNativeLog(1, message);

        static void LogWarning(string message) =>
            OnNativeLog(2, message);

        static void LogError(string message) =>
            OnNativeLog(3, message);
    }
}
```

**Fichier unit systemd** :

**/etc/systemd/system/myservice.service** :

```ini
[Unit]
Description=Mon Service Mono/Pascal  
After=network.target

[Service]
Type=simple  
User=myuser  
WorkingDirectory=/opt/myservice  
Environment="LD_LIBRARY_PATH=/usr/local/lib"  
ExecStart=/usr/bin/mono /opt/myservice/ServiceWorker.exe  
Restart=on-failure  
RestartSec=10

# Logs
StandardOutput=journal  
StandardError=journal  
SyslogIdentifier=myservice

[Install]
WantedBy=multi-user.target
```

**Installation et gestion du service** :

```bash
# Compiler
fpc -O3 -fPIC -olibservice_worker.so service_worker.pas  
sudo cp libservice_worker.so /usr/local/lib/  
sudo ldconfig

mcs ServiceWorker.cs -out:ServiceWorker.exe

# Installer
sudo mkdir -p /opt/myservice  
sudo cp ServiceWorker.exe /opt/myservice/  
sudo cp libservice_worker.so /opt/myservice/

# Copier le fichier unit
sudo cp myservice.service /etc/systemd/system/

# Activer et démarrer
sudo systemctl daemon-reload  
sudo systemctl enable myservice  
sudo systemctl start myservice

# Vérifier le statut
sudo systemctl status myservice

# Voir les logs
sudo journalctl -u myservice -f
```

---

## Débogage et diagnostics

### Outils de débogage sous Linux

#### 1. GDB (GNU Debugger)

**Déboguer le code natif Pascal** :

```bash
# Compiler avec symboles de débogage
fpc -g -olibmathlib.so mathlib.pas

# Démarrer GDB avec Mono
gdb --args mono MathDemo.exe

# Commandes GDB utiles
(gdb) run                    # Exécuter
(gdb) break Add              # Point d'arrêt sur fonction Add
(gdb) continue               # Continuer
(gdb) backtrace              # Stack trace
(gdb) info locals            # Variables locales
(gdb) print variable         # Afficher variable
```

**Attacher GDB à un processus en cours** :

```bash
# Trouver le PID
ps aux | grep mono

# Attacher
sudo gdb -p <PID>

# Dans GDB
(gdb) continue
# Ctrl+C pour interrompre
(gdb) backtrace
```

#### 2. Valgrind

**Détecter les fuites mémoire** :

```bash
# Exécuter avec Valgrind
valgrind --leak-check=full --show-leak-kinds=all mono MathDemo.exe

# Résultat typique:
# ==12345== HEAP SUMMARY:
# ==12345==     in use at exit: 0 bytes in 0 blocks
# ==12345==   total heap usage: 100 allocs, 100 frees
```

**Options utiles** :

```bash
# Vérification complète
valgrind --tool=memcheck --leak-check=full \
         --track-origins=yes --verbose mono MathDemo.exe

# Profiling
valgrind --tool=callgrind mono MathDemo.exe
# Visualiser avec kcachegrind
kcachegrind callgrind.out.12345
```

#### 3. strace

**Tracer les appels système** :

```bash
# Tracer tous les appels
strace mono MathDemo.exe

# Tracer uniquement les appels de fichiers
strace -e trace=open,openat,read,write mono MathDemo.exe

# Tracer les appels de bibliothèques
strace -e trace=dlopen mono MathDemo.exe

# Exemple de sortie:
# openat(AT_FDCWD, "libmathlib.so", O_RDONLY|O_CLOEXEC) = 3
```

#### 4. ldd - Vérifier les dépendances

```bash
# Lister les dépendances d'une bibliothèque
ldd libmathlib.so

# Résultat typique:
#     linux-vdso.so.1 (0x00007fff...)
#     libc.so.6 => /lib/x86_64-linux-gnu/libc.so.6
#     /lib64/ld-linux-x86-64.so.2

# Dépendances manquantes apparaissent comme:
#     libmissing.so => not found
```

#### 5. nm - Examiner les symboles

```bash
# Lister tous les symboles exportés
nm -D libmathlib.so

# Filtrer les fonctions exportées
nm -D libmathlib.so | grep ' T '

# Résultat typique:
# 0000000000001234 T Add
# 0000000000001567 T Multiply
# 000000000000189a T Power
```

### Logs et monitoring

**Configuration du logging en C#** :

```csharp
using System;  
using System.IO;

public class Logger
{
    private static readonly string LogPath =
        Path.Combine("/var/log/myapp", "application.log");

    private static readonly object _lock = new object();

    public static void Log(string level, string message)
    {
        lock (_lock)
        {
            string timestamp = DateTime.Now.ToString("yyyy-MM-dd HH:mm:ss.fff");
            string logLine = $"[{timestamp}] [{level}] {message}";

            Console.WriteLine(logLine);

            try
            {
                File.AppendAllText(LogPath, logLine + Environment.NewLine);
            }
            catch
            {
                // Ignorer les erreurs d'écriture de log
            }
        }
    }

    public static void Info(string message) => Log("INFO", message);
    public static void Warning(string message) => Log("WARN", message);
    public static void Error(string message) => Log("ERROR", message);
}

// Utilisation
Logger.Info("Application démarrée");  
Logger.Error("Erreur lors du chargement de libmathlib.so");
```

**Intégration avec syslog** :

```csharp
using System.Runtime.InteropServices;

public static class Syslog
{
    // Priorités syslog
    public const int LOG_INFO = 6;
    public const int LOG_WARNING = 4;
    public const int LOG_ERR = 3;

    [DllImport("libc.so.6")]
    private static extern void openlog(string ident, int option, int facility);

    [DllImport("libc.so.6")]
    private static extern void syslog(int priority, string format, string message);

    [DllImport("libc.so.6")]
    private static extern void closelog();

    public static void Initialize(string appName)
    {
        openlog(appName, 0, 0);
    }

    public static void Log(int priority, string message)
    {
        syslog(priority, "%s", message);
    }

    public static void Close()
    {
        closelog();
    }
}

// Utilisation
Syslog.Initialize("myapp");  
Syslog.Log(Syslog.LOG_INFO, "Application started");  
Syslog.Log(Syslog.LOG_ERR, "Critical error occurred");  
Syslog.Close();
```

### Problèmes courants et solutions

#### Problème 1 : Library not found

**Symptôme** :
```
DllNotFoundException: libmathlib.so
```

**Solutions** :

1. **Vérifier la présence** :
```bash
find / -name "libmathlib.so" 2>/dev/null
```

2. **Vérifier LD_LIBRARY_PATH** :
```bash
echo $LD_LIBRARY_PATH  
export LD_LIBRARY_PATH=/usr/local/lib:$LD_LIBRARY_PATH
```

3. **Mettre à jour le cache** :
```bash
sudo ldconfig -v | grep mathlib
```

4. **Vérifier avec ldd** :
```bash
ldd libmathlib.so
# Si dépendances manquantes, installer les packages nécessaires
```

#### Problème 2 : Symbol not found

**Symptôme** :
```
EntryPointNotFoundException: Unable to find an entry point named 'Add'
```

**Solution** :

```bash
# Vérifier les exports
nm -D libmathlib.so | grep Add

# Si manquant, vérifier le code Pascal
# Assurez-vous d'avoir: function Add(...); cdecl; export;
# Et dans la section exports: exports Add;
```

#### Problème 3 : Segmentation fault

**Symptôme** : Le programme crash avec "Segmentation fault (core dumped)"

**Solutions** :

1. **Activer les core dumps** :
```bash
ulimit -c unlimited  
mono MathDemo.exe
# Si crash, un fichier core sera créé

# Analyser avec GDB
gdb mono core
(gdb) backtrace
```

2. **Vérifier les pointeurs** :
```csharp
// ✗ MAUVAIS
int value = 42;  
NativeMethods.ProcessPointer(ref value);
// Si la fonction native garde le pointeur

// ✓ BON
IntPtr ptr = Marshal.AllocHGlobal(sizeof(int));  
try
{
    Marshal.WriteInt32(ptr, 42);
    NativeMethods.ProcessPointer(ptr);
}
finally
{
    Marshal.FreeHGlobal(ptr);
}
```

3. **Vérifier les tailles de structures** :
```csharp
Console.WriteLine($"Taille C#: {Marshal.SizeOf<MyStruct>()}");
// Doit correspondre à SizeOf(TMyStruct) en Pascal
```

#### Problème 4 : Permissions insuffisantes

**Symptôme** :
```
Permission denied
```

**Solutions** :

```bash
# Vérifier les permissions de la bibliothèque
ls -l libmathlib.so
# Devrait être: -rwxr-xr-x

# Corriger si nécessaire
chmod 755 libmathlib.so

# Pour les services systemd
sudo chown root:root /opt/myservice/*  
sudo chmod 755 /opt/myservice/*.exe
```

---

## Bonnes pratiques spécifiques à Linux

### 1. Chemins et fichiers

**Toujours utiliser Path.Combine** :

```csharp
// ✓ BON - portable
string configPath = Path.Combine(
    Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData),
    "myapp",
    "config.json"
);

// ✗ MAUVAIS - spécifique Windows
string configPath = "C:\\Users\\John\\AppData\\myapp\\config.json";
```

**Respecter la hiérarchie FHS (Filesystem Hierarchy Standard)** :

```csharp
public static class LinuxPaths
{
    // Binaires
    public static string Bin => "/usr/local/bin";

    // Bibliothèques
    public static string Lib => "/usr/local/lib";

    // Configuration système
    public static string EtcSystem => "/etc";

    // Configuration utilisateur
    public static string EtcUser => Path.Combine(
        Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData));

    // Données variables
    public static string VarData => "/var/lib/myapp";

    // Logs
    public static string VarLog => "/var/log/myapp";

    // Fichiers temporaires
    public static string Tmp => "/tmp";
}
```

### 2. Gestion des signaux Unix

**Gérer proprement SIGTERM et SIGINT** :

```csharp
using System;  
using System.Runtime.InteropServices;  
using Mono.Unix;  
using Mono.Unix.Native;

class GracefulShutdown
{
    private static bool _shutdown = false;

    static void Main()
    {
        // Installer les gestionnaires de signaux
        UnixSignal[] signals = new UnixSignal[]
        {
            new UnixSignal(Signum.SIGINT),   // Ctrl+C
            new UnixSignal(Signum.SIGTERM),  // systemctl stop
            new UnixSignal(Signum.SIGHUP)    // Rechargement config
        };

        // Thread de surveillance des signaux
        Task.Run(() =>
        {
            int index = UnixSignal.WaitAny(signals, -1);

            if (index >= 0 && index < 2)  // SIGINT ou SIGTERM
            {
                Console.WriteLine($"\nSignal {signals[index].Signum} reçu, arrêt...");
                _shutdown = true;
            }
            else if (index == 2)  // SIGHUP
            {
                Console.WriteLine("SIGHUP reçu, rechargement de la configuration...");
                ReloadConfiguration();
            }
        });

        // Boucle principale
        while (!_shutdown)
        {
            // Travail...
            Thread.Sleep(1000);
        }

        Console.WriteLine("Arrêt propre terminé");
    }

    static void ReloadConfiguration()
    {
        // Recharger la configuration sans arrêter le service
    }
}
```

### 3. Permissions et sécurité

**Ne jamais exécuter en root si possible** :

```bash
# Créer un utilisateur dédié
sudo useradd -r -s /bin/false myappuser

# Créer les répertoires
sudo mkdir -p /opt/myapp /var/log/myapp  
sudo chown myappuser:myappuser /opt/myapp /var/log/myapp

# Fichier unit systemd
[Service]
User=myappuser  
Group=myappuser
```

**Vérifier les permissions avant d'écrire** :

```csharp
public static bool CanWriteToDirectory(string path)
{
    try
    {
        string testFile = Path.Combine(path, ".write_test");
        File.WriteAllText(testFile, "test");
        File.Delete(testFile);
        return true;
    }
    catch
    {
        return false;
    }
}

// Utilisation
if (!CanWriteToDirectory("/var/log/myapp"))
{
    Console.WriteLine("Permissions insuffisantes pour écrire dans /var/log/myapp");
    // Utiliser un chemin alternatif
}
```

### 4. Packaging et distribution

**Structure d'un package Debian** :

```
myapp_1.0.0-1/
├── DEBIAN/
│   ├── control
│   ├── postinst
│   ├── prerm
│   └── postrm
├── usr/
│   ├── local/
│   │   ├── bin/
│   │   │   └── myapp
│   │   └── lib/
│   │       ├── myapp/
│   │       │   ├── MyApp.exe
│   │       │   └── MyApp.dll
│   │       └── libmathlib.so
│   └── share/
│       └── doc/
│           └── myapp/
│               └── README
└── etc/
    └── systemd/
        └── system/
            └── myapp.service
```

**Fichier control** :

```
Package: myapp  
Version: 1.0.0-1  
Architecture: amd64  
Maintainer: Votre Nom <email@example.com>  
Depends: mono-runtime (>= 6.0), libc6  
Description: Mon application Mono/Pascal
 Description détaillée de l'application
```

**Script postinst** :

```bash
#!/bin/bash
set -e

# Recharger le cache des bibliothèques
ldconfig

# Créer l'utilisateur si nécessaire
if ! id -u myappuser > /dev/null 2>&1; then
    useradd -r -s /bin/false myappuser
fi

# Créer les répertoires de logs
mkdir -p /var/log/myapp  
chown myappuser:myappuser /var/log/myapp

# Activer et démarrer le service
systemctl daemon-reload  
systemctl enable myapp.service  
systemctl start myapp.service

echo "myapp installé avec succès"  
exit 0
```

**Script prerm** (avant désinstallation) :

```bash
#!/bin/bash
set -e

# Arrêter le service
if systemctl is-active --quiet myapp.service; then
    systemctl stop myapp.service
fi

# Désactiver le service
systemctl disable myapp.service

exit 0
```

**Script postrm** (après désinstallation) :

```bash
#!/bin/bash
set -e

# Supprimer l'utilisateur
if id -u myappuser > /dev/null 2>&1; then
    userdel myappuser
fi

# Supprimer les logs si purge complet
if [ "$1" = "purge" ]; then
    rm -rf /var/log/myapp
fi

# Recharger systemd
systemctl daemon-reload

exit 0
```

**Créer le package** :

```bash
# Construire le package .deb
dpkg-deb --build myapp_1.0.0-1

# Installer
sudo dpkg -i myapp_1.0.0-1.deb

# Désinstaller
sudo apt remove myapp

# Purge complète
sudo apt purge myapp
```

### 5. AppImage pour distribution simple

**Structure d'un AppImage** :

```
MyApp.AppDir/
├── AppRun                    # Script de lancement
├── myapp.desktop            # Fichier desktop
├── myapp.png                # Icône
└── usr/
    ├── bin/
    │   └── myapp
    └── lib/
        ├── mono/
        │   └── (runtime Mono embarqué)
        └── (bibliothèques natives)
```

**Script AppRun** :

```bash
#!/bin/bash

# Obtenir le répertoire de l'AppImage
APPDIR="$(dirname "$(readlink -f "${0}")")"

# Configurer les chemins
export LD_LIBRARY_PATH="${APPDIR}/usr/lib:${LD_LIBRARY_PATH}"  
export MONO_PATH="${APPDIR}/usr/lib/mono/4.5"  
export PATH="${APPDIR}/usr/bin:${PATH}"

# Lancer l'application
exec "${APPDIR}/usr/bin/mono" "${APPDIR}/usr/lib/myapp/MyApp.exe" "$@"
```

**Créer l'AppImage** :

```bash
# Télécharger appimagetool
wget https://github.com/AppImage/AppImageKit/releases/download/continuous/appimagetool-x86_64.AppImage  
chmod +x appimagetool-x86_64.AppImage

# Créer l'AppImage
./appimagetool-x86_64.AppImage MyApp.AppDir MyApp-x86_64.AppImage

# Utilisation
chmod +x MyApp-x86_64.AppImage
./MyApp-x86_64.AppImage
```

---

## Comparaison avec .NET moderne

### Mono vs .NET 6/7/8

| Aspect | Mono | .NET 6+ |
|--------|------|---------|
| **Support officiel** | Communauté | Microsoft |
| **Performance** | Bonne | Excellente |
| **Compatibilité API** | .NET Framework 4.x | .NET Standard 2.1+ |
| **AOT Compilation** | Limitée | Native AOT |
| **Taille binaires** | Grande (runtime) | Petite (single file) |
| **Xamarin/Unity** | Oui | Migration en cours |
| **Recommandation** | Projets existants | Nouveaux projets |

### Migration vers .NET moderne

**Différences dans P/Invoke** :

```csharp
// Mono et .NET moderne : identique pour la plupart
[DllImport("libmathlib.so")]
public static extern int Add(int a, int b);

// .NET 7+ : Source Generator (performance améliorée)
[LibraryImport("libmathlib.so")]
public static partial int Add(int a, int b);
```

**NativeLibrary (API moderne)** :

```csharp
using System.Runtime.InteropServices;

public static class ModernNativeLoader
{
    private static IntPtr _handle;

    static ModernNativeLoader()
    {
        // Charger la bibliothèque avec l'API moderne
        _handle = NativeLibrary.Load("libmathlib.so");
    }

    public static int Add(int a, int b)
    {
        // Obtenir le pointeur de fonction
        IntPtr funcPtr = NativeLibrary.GetExport(_handle, "Add");

        // Créer un délégué
        var addFunc = Marshal.GetDelegateForFunctionPointer<AddDelegate>(funcPtr);

        return addFunc(a, b);
    }

    [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
    private delegate int AddDelegate(int a, int b);
}
```

---

## Checklist complète pour déploiement Linux

### Avant le déploiement

- [ ] **Tester sur un système propre** (machine virtuelle Ubuntu fraîche)
- [ ] **Vérifier les dépendances** avec `ldd`
- [ ] **Tester avec différentes versions** d'Ubuntu (20.04, 22.04, 24.04)
- [ ] **Documenter les prérequis** (Mono/dotnet version, libc version)
- [ ] **Vérifier les permissions** (fichiers, répertoires)
- [ ] **Tester les chemins relatifs** et absolus
- [ ] **Valider avec Valgrind** (fuites mémoire)
- [ ] **Profiler les performances** critiques
- [ ] **Créer des tests unitaires** pour l'interop
- [ ] **Documenter l'API** P/Invoke

### Structure de déploiement recommandée

```
/opt/myapp/                   # Application
├── bin/
│   └── MyApp.exe
├── lib/
│   ├── libmathlib.so.1.0.0
│   ├── libmathlib.so.1 -> libmathlib.so.1.0.0
│   └── libmathlib.so -> libmathlib.so.1
└── etc/
    └── config.json

/var/log/myapp/               # Logs
└── application.log

/etc/systemd/system/          # Service
└── myapp.service

/usr/share/doc/myapp/         # Documentation
├── README.md
├── LICENSE
└── CHANGELOG.md
```

### Scripts d'installation

**install.sh** :

```bash
#!/bin/bash
set -e

APP_NAME="myapp"  
APP_VERSION="1.0.0"  
INSTALL_DIR="/opt/${APP_NAME}"  
LOG_DIR="/var/log/${APP_NAME}"

echo "=== Installation de ${APP_NAME} ${APP_VERSION} ==="

# Vérifier les prérequis
echo "Vérification des prérequis..."  
if ! command -v mono &> /dev/null; then
    echo "✗ Mono n'est pas installé"
    echo "  Installez avec: sudo apt install mono-runtime"
    exit 1
fi

echo "✓ Mono installé: $(mono --version | head -n1)"

# Créer l'utilisateur
echo "Création de l'utilisateur..."  
if ! id -u ${APP_NAME} &> /dev/null; then
    sudo useradd -r -s /bin/false ${APP_NAME}
    echo "✓ Utilisateur ${APP_NAME} créé"
else
    echo "✓ Utilisateur ${APP_NAME} existe déjà"
fi

# Créer les répertoires
echo "Création des répertoires..."  
sudo mkdir -p ${INSTALL_DIR}/{bin,lib,etc}  
sudo mkdir -p ${LOG_DIR}

# Copier les fichiers
echo "Installation des fichiers..."  
sudo cp bin/MyApp.exe ${INSTALL_DIR}/bin/  
sudo cp lib/libmathlib.so.${APP_VERSION} ${INSTALL_DIR}/lib/  
sudo cp etc/config.json ${INSTALL_DIR}/etc/

# Créer les liens symboliques
cd ${INSTALL_DIR}/lib  
sudo ln -sf libmathlib.so.${APP_VERSION} libmathlib.so.1  
sudo ln -sf libmathlib.so.1 libmathlib.so  
cd -

# Définir les permissions
echo "Configuration des permissions..."  
sudo chown -R ${APP_NAME}:${APP_NAME} ${INSTALL_DIR}  
sudo chown -R ${APP_NAME}:${APP_NAME} ${LOG_DIR}  
sudo chmod 755 ${INSTALL_DIR}/bin/MyApp.exe  
sudo chmod 755 ${INSTALL_DIR}/lib/*.so*

# Mettre à jour ldconfig
echo "Mise à jour du cache des bibliothèques..."  
echo "${INSTALL_DIR}/lib" | sudo tee /etc/ld.so.conf.d/${APP_NAME}.conf  
sudo ldconfig

# Installer le service systemd
if [ -f "systemd/myapp.service" ]; then
    echo "Installation du service systemd..."
    sudo cp systemd/myapp.service /etc/systemd/system/
    sudo systemctl daemon-reload
    sudo systemctl enable ${APP_NAME}.service
    echo "✓ Service installé et activé"
fi

echo ""  
echo "=== Installation terminée ==="  
echo ""  
echo "Pour démarrer le service:"  
echo "  sudo systemctl start ${APP_NAME}"  
echo ""  
echo "Pour voir les logs:"  
echo "  sudo journalctl -u ${APP_NAME} -f"  
echo ""
```

**uninstall.sh** :

```bash
#!/bin/bash
set -e

APP_NAME="myapp"  
INSTALL_DIR="/opt/${APP_NAME}"  
LOG_DIR="/var/log/${APP_NAME}"

echo "=== Désinstallation de ${APP_NAME} ==="

# Arrêter le service
if systemctl is-active --quiet ${APP_NAME}.service; then
    echo "Arrêt du service..."
    sudo systemctl stop ${APP_NAME}.service
fi

# Désactiver le service
if systemctl is-enabled --quiet ${APP_NAME}.service; then
    echo "Désactivation du service..."
    sudo systemctl disable ${APP_NAME}.service
fi

# Supprimer le fichier service
if [ -f "/etc/systemd/system/${APP_NAME}.service" ]; then
    sudo rm /etc/systemd/system/${APP_NAME}.service
    sudo systemctl daemon-reload
fi

# Supprimer les fichiers
echo "Suppression des fichiers..."  
sudo rm -rf ${INSTALL_DIR}

# Supprimer la configuration ldconfig
if [ -f "/etc/ld.so.conf.d/${APP_NAME}.conf" ]; then
    sudo rm /etc/ld.so.conf.d/${APP_NAME}.conf
    sudo ldconfig
fi

# Demander si on supprime les logs
read -p "Supprimer les logs dans ${LOG_DIR}? (o/N) " -n 1 -r  
echo  
if [[ $REPLY =~ ^[Oo]$ ]]; then
    sudo rm -rf ${LOG_DIR}
    echo "✓ Logs supprimés"
fi

# Demander si on supprime l'utilisateur
read -p "Supprimer l'utilisateur ${APP_NAME}? (o/N) " -n 1 -r  
echo  
if [[ $REPLY =~ ^[Oo]$ ]]; then
    sudo userdel ${APP_NAME}
    echo "✓ Utilisateur supprimé"
fi

echo ""  
echo "=== Désinstallation terminée ==="
```

---

## Annexes

### Tableau récapitulatif : Différences Windows/Linux

| Aspect | Windows | Linux |
|--------|---------|-------|
| **Extension bibliothèque** | `.dll` | `.so` |
| **Préfixe bibliothèque** | Aucun | `lib` (convention) |
| **Séparateur chemin** | `\` | `/` |
| **Fin de ligne** | CRLF (`\r\n`) | LF (`\n`) |
| **Variables d'environnement** | `%VAR%` | `$VAR` |
| **Exécutables** | `.exe` | Pas d'extension |
| **Services** | Services Windows | systemd/init.d |
| **Permissions** | ACL | chmod/chown |
| **Registre** | Oui | Non (fichiers conf) |
| **Case sensitivity** | Non | Oui |
| **Loader de DLL** | LoadLibrary | dlopen |
| **Variable PATH lib** | PATH | LD_LIBRARY_PATH |

### Commandes essentielles

**Compilation** :

```bash
# Compiler bibliothèque Pascal
fpc -O3 -fPIC -olibmylib.so mylib.pas

# Compiler avec debug
fpc -g -olibmylib.so mylib.pas

# Compiler programme C#
mcs Program.cs -r:System.Drawing.dll -out:MyApp.exe

# Avec dotnet
dotnet build  
dotnet publish -c Release
```

**Gestion des bibliothèques** :

```bash
# Lister les dépendances
ldd libmylib.so

# Mettre à jour le cache
sudo ldconfig

# Vérifier le cache
ldconfig -p | grep mylib

# Voir les exports
nm -D libmylib.so

# Chercher des symboles
objdump -T libmylib.so | grep Add
```

**Débogage** :

```bash
# Exécuter avec GDB
gdb --args mono MyApp.exe

# Valgrind pour mémoire
valgrind --leak-check=full mono MyApp.exe

# Tracer les appels système
strace -e trace=open,openat mono MyApp.exe

# Profiling
perf record mono MyApp.exe  
perf report
```

**Services systemd** :

```bash
# Recharger la configuration
sudo systemctl daemon-reload

# Démarrer/Arrêter
sudo systemctl start myapp  
sudo systemctl stop myapp

# Activer au démarrage
sudo systemctl enable myapp

# Voir le statut
sudo systemctl status myapp

# Voir les logs
sudo journalctl -u myapp -f
```

### Ressources supplémentaires

**Documentation officielle** :

- **Mono Project** : https://www.mono-project.com/
- **.NET sur Linux** : https://docs.microsoft.com/dotnet/core/install/linux
- **FreePascal** : https://www.freepascal.org/docs.html
- **systemd** : https://www.freedesktop.org/wiki/Software/systemd/

**Bibliothèques utiles** :

- **Tmds.DBus** : Communication D-Bus pour .NET
  ```bash
  dotnet add package Tmds.DBus
  ```

- **Mono.Posix** : API POSIX pour Mono
  ```bash
  dotnet add package Mono.Posix.NETStandard
  ```

- **System.Drawing.Common** : Graphics sur Linux
  ```bash
  dotnet add package System.Drawing.Common
  ```

**Outils de développement** :

- **MonoDevelop** : IDE pour Mono
- **Visual Studio Code** : Éditeur moderne avec extensions C#/Pascal
- **Lazarus** : IDE FreePascal complet
- **Rider** : IDE JetBrains (commercial)

### Exemple de projet complet

**Structure finale** :

```
MonoPascalProject/
├── src/
│   ├── pascal/
│   │   ├── mathlib.pas
│   │   ├── fileops.pas
│   │   └── Makefile
│   └── csharp/
│       ├── Program.cs
│       ├── NativeMethods.cs
│       └── MyApp.csproj
├── tests/
│   ├── test_mathlib.sh
│   └── UnitTests.cs
├── scripts/
│   ├── build.sh
│   ├── install.sh
│   └── uninstall.sh
├── systemd/
│   └── myapp.service
├── docs/
│   ├── README.md
│   ├── INSTALL.md
│   └── API.md
└── packaging/
    ├── debian/
    │   └── DEBIAN/
    │       ├── control
    │       ├── postinst
    │       └── prerm
    └── appimage/
        └── MyApp.AppDir/
```

**Script build.sh complet** :

```bash
#!/bin/bash
set -e

echo "=== Build du projet Mono/Pascal ==="

# Nettoyer
echo "Nettoyage..."  
rm -rf build/  
mkdir -p build/{lib,bin}

# Compiler les bibliothèques Pascal
echo "Compilation des bibliothèques Pascal..."  
cd src/pascal  
make clean  
make  
cp *.so ../../build/lib/  
cd ../..

# Compiler l'application C#
echo "Compilation de l'application C#..."  
cd src/csharp  
dotnet build -c Release -o ../../build/bin/  
cd ../..

# Copier les fichiers additionnels
echo "Copie des fichiers de configuration..."  
cp -r etc build/

# Tests
echo "Exécution des tests..."  
cd tests
./test_mathlib.sh
cd ..

echo "✓ Build terminé avec succès"  
echo "Les fichiers sont dans: build/"
```

---

## Conclusion

### Ce que nous avons appris

1. **Installation et configuration**
   - Installation de Mono et .NET sur Linux
   - Configuration de FreePascal
   - Gestion des chemins et bibliothèques

2. **Interopérabilité fondamentale**
   - Création de bibliothèques partagées (.so)
   - P/Invoke sous Linux
   - Marshalling des données
   - Gestion des chaînes UTF-8

3. **Intégration système**
   - Utilisation des bibliothèques système (libc)
   - D-Bus pour la communication IPC
   - Services systemd
   - Permissions et sécurité Unix

4. **Développement avancé**
   - Monitoring système en temps réel
   - Traitement d'images optimisé
   - Services en arrière-plan
   - Logging et diagnostics

5. **Déploiement professionnel**
   - Packaging Debian (.deb)
   - AppImage pour distribution
   - Scripts d'installation/désinstallation
   - Services systemd

### Différences clés avec Windows

**Avantages de Linux** :
- Écosystème open source complet
- Performance système souvent meilleure
- Coût nul (pas de licences)
- Stabilité pour les services
- Outils de débogage puissants (Valgrind, GDB)
- Déploiement conteneurisé (Docker) simple

**Défis spécifiques** :
- Diversité des distributions
- Gestion des dépendances plus complexe
- Permissions Unix à maîtriser
- Moins d'outils graphiques
- Documentation parfois fragmentée

### Quand utiliser Mono/Pascal sur Linux ?

**✓ Utilisez cette approche quand** :
- Vous avez du code Pascal existant à réutiliser
- Vous développez des services backend haute performance
- Vous créez des outils système ou serveurs
- Vous avez besoin de calculs intensifs optimisés
- Vous développez pour Raspberry Pi ou embarqué
- Vous créez des démons système

**✗ Évitez cette approche quand** :
- .NET pur suffit largement
- Le projet est purement managé
- Vous visez le desktop avec GUI complexe
- L'équipe n'a pas d'expertise Pascal
- La maintenance sera difficile

### Aller plus loin

**Prochaines étapes recommandées** :

1. **Maîtriser .NET moderne sur Linux**
   - Migrer vers .NET 8+ si possible
   - Explorer NativeAOT pour performance
   - Utiliser les nouvelles API d'interop

2. **Approfondir systemd**
   - Socket activation
   - Timers systemd
   - Security hardening (sandboxing)

3. **Conteneurisation**
   - Créer des images Docker
   - Orchestration Kubernetes
   - Multi-stage builds optimisés

4. **Performance avancée**
   - Profiling avec perf
   - SIMD et vectorisation
   - Lock-free programming

5. **Sécurité**
   - SELinux/AppArmor policies
   - Capabilities Linux
   - Secure coding practices

### Ressources finales

**Communautés** :
- Forum Mono : https://github.com/mono/mono/discussions
- FreePascal Forum : https://forum.lazarus.freepascal.org
- Reddit : r/linux, r/dotnet, r/fpc
- Stack Overflow : Tags [mono], [freepascal], [pinvoke]

**Projets open source exemplaires** :
- **mORMot** : Framework SOA/ORM
- **Synapse** : Bibliothèque réseau FreePascal
- **Castle Game Engine** : Moteur de jeu Pascal
- **ASP.NET Core** : Framework web .NET

**Livres recommandés** :
- "The Linux Programming Interface" - Michael Kerrisk
- "Pro .NET Memory Management" - Konrad Kokosa
- "Systems Performance" - Brendan Gregg

### Mot de la fin

L'interopérabilité entre Mono/.NET et FreePascal sur Linux ouvre de nombreuses possibilités pour créer des applications performantes et professionnelles. En combinant la productivité de C# avec la performance du code natif Pascal, vous pouvez créer des solutions optimales pour les environnements Linux.

**Points clés à retenir** :

1. **Simplicité** : Commencez simple avec P/Invoke basique
2. **Portabilité** : Écrivez du code multiplateforme dès le début
3. **Robustesse** : Gérez les erreurs et testez sur systèmes propres
4. **Performance** : Profilez avant d'optimiser
5. **Sécurité** : Respectez les principes de moindre privilège
6. **Documentation** : Documentez vos choix d'interop

L'écosystème Linux offre des outils puissants (systemd, D-Bus, perf, Valgrind) qui, combinés avec l'interop .NET/Pascal, permettent de créer des solutions professionnelles robustes et performantes.

**Le meilleur code est celui qui fonctionne, est maintenable, et répond aux besoins.** L'interop Mono/Pascal sur Linux n'est qu'un outil dans votre boîte à outils - utilisez-le judicieusement !

---

**Fin du tutoriel 19.8 Mono Interop (Linux)**

Bonne chance dans vos développements Linux avec Mono et FreePascal ! 🐧🚀

⏭️ [WebAssembly et JavaScript](/19-interoperabilite-bindings/09-webassembly-javascript.md)
