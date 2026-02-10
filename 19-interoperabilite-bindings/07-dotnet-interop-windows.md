🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 19.7 .NET Interop (Windows) - Tutoriel pour Développeurs Avancés

## Table des matières

1. [Introduction à .NET](#introduction-à-net)
2. [Installation et prérequis](#installation-et-prérequis)
3. [Les trois approches d'interopérabilité](#les-trois-approches-dinteropérabilité)
4. [Approche 1 : P/Invoke](#approche-1-pinvoke---appeler-pascal-depuis-net)
5. [Approche 2 : COM Interop](#approche-2-com-interop)
6. [Approche 3 : Processus séparés (IPC)](#approche-3-processus-séparés-avec-ipc)
7. [Marshalling avancé](#marshalling-avancé)
8. [Wrapper C# professionnel](#wrapper-c-professionnel)
9. [Bonnes pratiques](#bonnes-pratiques)
10. [Conclusion](#conclusion)

---

## Introduction à .NET

### Qu'est-ce que .NET ?

**.NET** (prononcé "dot net") est une plateforme de développement créée par Microsoft qui permet d'écrire des applications dans différents langages (C#, VB.NET, F#) qui compilent vers un bytecode commun appelé **IL (Intermediate Language)**, exécuté par le **CLR (Common Language Runtime)**.

**Analogie simple** : Si vous imaginez que Java et la JVM sont comme un système d'exploitation universel pour le code, .NET et le CLR sont la version Microsoft de ce concept, mais profondément intégrée à Windows.

### Les composants de .NET

**.NET Framework** (Windows uniquement) et **.NET** (multiplateforme, anciennement .NET Core) fournissent :

- **CLR** : Machine virtuelle qui exécute le code managé
- **BCL** : Bibliothèque de classes de base (Base Class Library)
- **Outils** : Visual Studio, VS Code, compilateurs
- **Écosystème** : Des milliers de packages via NuGet

### Architecture conceptuelle

```
┌───────────────────────────────────────────────────┐
│           Applications .NET                       │
│   (C#, VB.NET, F#, autres langages)               │
└────────────────┬──────────────────────────────────┘
                 │
         ┌───────▼────────┐
         │  CLR Runtime   │
         │  (Managed)     │
         └───────┬────────┘
                 │
         ┌───────▼────────┐
         │  Windows OS    │
         └────────────────┘

┌───────────────────────────────────────────────────┐
│        Application FreePascal (Native)            │
└────────────────┬──────────────────────────────────┘
                 │
                 │ Interopérabilité
                 │ ┌─────────────┐
                 ├─┤ P/Invoke    │
                 │ └─────────────┘
                 │ ┌─────────────┐
                 ├─┤ COM Interop │
                 │ └─────────────┘
                 │ ┌─────────────┐
                 └─┤ IPC/Sockets │
                   └─────────────┘
```

### Pourquoi interfacer FreePascal avec .NET ?

#### 1. Accès à l'écosystème .NET

Des milliers de bibliothèques sont disponibles gratuitement via NuGet :

- **Entity Framework** : ORM puissant pour bases de données
- **ASP.NET** : Framework web moderne
- **ML.NET** : Machine learning
- **WPF/WinForms** : Interfaces graphiques riches
- **Azure SDK** : Services cloud Microsoft

#### 2. Réutilisation de code existant

**Scénario 1** : Vous avez une bibliothèque Pascal performante que vous voulez utiliser depuis C#

```pascal
// Bibliothèque Pascal existante (calculs complexes)
function CalculerComplexe(x, y: Double): Double; cdecl;
begin
  Result := // ... algorithme optimisé en Pascal
end;
```

```csharp
// Appelée facilement depuis C#
double result = NativeLib.CalculerComplexe(10.5, 20.3);
```

**Scénario 2** : Vous voulez utiliser une bibliothèque .NET depuis votre application Pascal

```csharp
// Bibliothèque .NET existante
public class MathHelper {
    public static double CalculateArea(double radius) {
        return Math.PI * radius * radius;
    }
}
```

#### 3. Intégration Windows profonde

.NET est profondément intégré à l'écosystème Windows :

- Windows Forms et WPF pour les interfaces
- Active Directory pour l'authentification
- Windows Services pour les démons
- PowerShell pour l'automatisation

---

## Installation et prérequis

### Installer .NET SDK sur Windows

#### Méthode 1 : Installateur officiel

1. Téléchargez le SDK depuis [https://dotnet.microsoft.com/download](https://dotnet.microsoft.com/download)
2. Exécutez l'installateur (version 8.0 ou supérieure recommandée)
3. Redémarrez votre terminal/invite de commandes

#### Méthode 2 : Via winget (Windows 10/11)

```powershell
winget install Microsoft.DotNet.SDK.8
```

#### Méthode 3 : Via Chocolatey

```powershell
choco install dotnet-sdk
```

### Vérification de l'installation

Ouvrez un terminal et tapez :

```bash
dotnet --version
```

Vous devriez voir quelque chose comme : `8.0.xxx`

### Outils de développement recommandés

**Visual Studio Community** (gratuit, complet) :
- IDE complet avec débogueur intégré
- IntelliSense avancé
- Designer visuel pour WinForms/WPF

**Visual Studio Code** (léger, extensible) :
- Éditeur moderne et rapide
- Extension C# Dev Kit pour le support complet

Installation de l'extension C# pour VS Code :

```bash
code --install-extension ms-dotnettools.csdevkit
```

---

## Les trois approches d'interopérabilité

Il existe trois méthodes principales pour faire communiquer FreePascal et .NET :

### Comparaison rapide

| Approche | Direction | Complexité | Performance | Usage typique |
|----------|-----------|------------|-------------|---------------|
| **P/Invoke** | .NET → Pascal | Faible | Excellente | Appeler des DLL natives |
| **COM Interop** | Bidirectionnelle | Moyenne | Bonne | Objets et interfaces |
| **IPC (Sockets)** | Bidirectionnelle | Élevée | Variable | Processus séparés |

### Approche 1 : P/Invoke (Platform Invocation Services)

**Direction** : .NET appelle du code Pascal natif

```
┌──────────────┐         ┌──────────────┐
│  C# (.NET)   │ P/Invoke│ Pascal (DLL) │
│  Managed     │────────▶│   Native     │
└──────────────┘         └──────────────┘
```

**Avantages** :
- Simple à mettre en place
- Performance maximale (appel direct)
- Standard .NET bien documenté

**Inconvénients** :
- Unidirectionnel (seulement .NET → Pascal)
- Pas d'accès aux objets .NET depuis Pascal
- Gestion manuelle de la mémoire

**Quand l'utiliser** : Pour appeler des fonctions Pascal depuis C#, calculs intensifs, bibliothèques natives.

### Approche 2 : COM Interop

**Direction** : Bidirectionnelle via COM

```
┌──────────────┐    COM    ┌──────────────┐
│  C# (.NET)   │◀────────▶ │ Pascal (COM) │
│              │           │              │
└──────────────┘           └──────────────┘
```

**Avantages** :
- Communication bidirectionnelle
- Support des objets et interfaces
- Callbacks possibles
- Standard Windows mature

**Inconvénients** :
- Configuration plus complexe (enregistrement COM)
- Overhead léger de COM
- Nécessite des GUIDs

**Quand l'utiliser** : Pour des objets complexes, interfaces riches, communication bidirectionnelle.

### Approche 3 : Processus séparés avec IPC

**Direction** : Bidirectionnelle via réseau/pipes

```
┌──────────────┐   Socket   ┌──────────────┐
│  C# (.NET)   │◀─────────▶│ Pascal (EXE)  │
│  Process 1   │    JSON    │  Process 2   │
└──────────────┘            └──────────────┘
```

**Avantages** :
- Isolation complète des processus
- Flexibilité maximale (protocole personnalisé)
- Peut fonctionner sur des machines différentes
- Pas de problèmes de compatibilité binaire

**Inconvénients** :
- Latence réseau
- Sérialisation/désérialisation
- Complexité de gestion des processus

**Quand l'utiliser** : Microservices, applications distribuées, isolation de sécurité.

---

## Approche 1 : P/Invoke - Appeler Pascal depuis .NET

P/Invoke est la méthode la plus directe pour appeler du code natif Pascal depuis C#.

### Étape 1 : Créer une DLL Pascal

Créez un fichier `mathlib.pas` :

```pascal
library mathlib;

{$mode objfpc}{$H+}

uses
  SysUtils, Math;

// Fonction simple d'addition
function Add(a, b: Integer): Integer; cdecl; export;
begin
  Result := a + b;
end;

// Fonction avec nombres décimaux
function Power(base, exponent: Double): Double; cdecl; export;
begin
  Result := Math.Power(base, exponent);
end;

// Fonction retournant une chaîne (attention à la gestion mémoire)
function GetVersion: PAnsiChar; cdecl; export;
begin
  Result := '1.0.0';  // Chaîne constante, pas de désallocation nécessaire
end;

// Procédure modifiant des paramètres par référence
procedure Swap(var a, b: Integer); cdecl; export;
var
  temp: Integer;
begin
  temp := a;
  a := b;
  b := temp;
end;

// Fonction travaillant avec un tableau
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

// Section d'export : liste les fonctions accessibles
exports
  Add,
  Power,
  GetVersion,
  Swap,
  SumArray;

begin
  // Point d'entrée de la DLL (vide ici)
end.
```

**Points importants** :

1. **`cdecl`** : Convention d'appel compatible avec C (obligatoire pour P/Invoke)
2. **`export`** : Rend la fonction visible depuis l'extérieur de la DLL
3. **`exports`** : Section qui liste toutes les fonctions exportées
4. **`library`** : Mot-clé pour créer une DLL au lieu d'un programme

### Compilation de la DLL

```bash
fpc -omathlib.dll mathlib.pas
```

Options utiles :
- `-O3` : Optimisation maximale
- `-CX` : Création d'une DLL Windows

Après compilation, vous obtenez :
- `mathlib.dll` : La bibliothèque compilée
- `mathlib.o` : Fichier objet (peut être supprimé)

### Étape 2 : Utiliser la DLL depuis C#

Créez un fichier `Program.cs` :

```csharp
using System;
using System.Runtime.InteropServices;

/// <summary>
/// Classe statique contenant les déclarations P/Invoke
/// </summary>
class NativeMath
{
    // Déclaration P/Invoke : Add
    [DllImport("mathlib.dll", CallingConvention = CallingConvention.Cdecl)]
    public static extern int Add(int a, int b);

    // Déclaration P/Invoke : Power
    [DllImport("mathlib.dll", CallingConvention = CallingConvention.Cdecl)]
    public static extern double Power(double baseValue, double exponent);

    // Déclaration P/Invoke : GetVersion (retourne un pointeur)
    [DllImport("mathlib.dll", CallingConvention = CallingConvention.Cdecl)]
    public static extern IntPtr GetVersion();

    // Déclaration P/Invoke : Swap (paramètres par référence)
    [DllImport("mathlib.dll", CallingConvention = CallingConvention.Cdecl)]
    public static extern void Swap(ref int a, ref int b);

    // Déclaration P/Invoke : SumArray (tableau)
    [DllImport("mathlib.dll", CallingConvention = CallingConvention.Cdecl)]
    public static extern int SumArray(int[] arr, int length);

    // Méthode helper pour convertir IntPtr en string
    public static string GetVersionString()
    {
        IntPtr ptr = GetVersion();
        return Marshal.PtrToStringAnsi(ptr);
    }
}

class Program
{
    static void Main()
    {
        Console.WriteLine("=== Démonstration P/Invoke avec FreePascal ===\n");

        // Test 1 : Addition simple
        int sum = NativeMath.Add(10, 32);
        Console.WriteLine($"Addition : 10 + 32 = {sum}");

        // Test 2 : Puissance
        double power = NativeMath.Power(2, 10);
        Console.WriteLine($"Puissance : 2^10 = {power}");

        // Test 3 : Récupération de version
        string version = NativeMath.GetVersionString();
        Console.WriteLine($"Version de la bibliothèque : {version}");

        // Test 4 : Échange de valeurs
        int a = 5, b = 10;
        Console.WriteLine($"\nAvant swap : a={a}, b={b}");
        NativeMath.Swap(ref a, ref b);
        Console.WriteLine($"Après swap : a={a}, b={b}");

        // Test 5 : Somme d'un tableau
        int[] numbers = { 1, 2, 3, 4, 5 };
        int total = NativeMath.SumArray(numbers, numbers.Length);
        Console.WriteLine($"\nSomme du tableau [1,2,3,4,5] : {total}");

        Console.WriteLine("\nAppuyez sur une touche pour quitter...");
        Console.ReadKey();
    }
}
```

**Explication des attributs** :

- **`[DllImport(...)]`** : Indique que la méthode est implémentée dans une DLL native
- **`CallingConvention.Cdecl`** : Correspond au `cdecl` de Pascal
- **`extern`** : La méthode n'a pas de corps, elle est externe
- **`ref`** : Passage par référence (correspond à `var` en Pascal)

### Compilation et exécution C#

**Avec le compilateur csc** :

```bash
csc Program.cs
Program.exe
```

**Avec dotnet** (recommandé) :

```bash
# Créer un nouveau projet console
dotnet new console -n MathDemo
cd MathDemo

# Remplacer le contenu de Program.cs
# Copier mathlib.dll dans le dossier du projet

# Compiler et exécuter
dotnet run
```

**Important** : `mathlib.dll` doit être dans le même dossier que l'exécutable C# ou dans un dossier du PATH système.

### Gestion des chaînes de caractères

Les chaînes sont délicates en P/Invoke car elles doivent traverser la frontière managé/natif. Voici les approches recommandées :

#### Approche 1 : Retourner une chaîne constante

**Pascal** :

```pascal
const
  VERSION_STRING: PAnsiChar = 'Version 1.0.0';

function GetVersion: PAnsiChar; cdecl; export;
begin
  Result := VERSION_STRING;  // Pas de désallocation nécessaire
end;
```

**C#** :

```csharp
[DllImport("mylib.dll", CallingConvention = CallingConvention.Cdecl)]
[return: MarshalAs(UnmanagedType.LPStr)]
public static extern string GetVersion();

// Utilisation directe
string version = GetVersion();
```

#### Approche 2 : Buffer fourni par C#

Cette approche est plus sûre pour les chaînes dynamiques.

**Pascal** :

```pascal
procedure GetMessage(buffer: PAnsiChar; bufferSize: Integer); cdecl; export;
var
  msg: string;
begin
  msg := 'Bonjour depuis Pascal!';

  // Vérifier que le buffer est assez grand
  if Length(msg) < bufferSize then
    StrPCopy(buffer, msg)
  else
    buffer[0] := #0;  // Chaîne vide si buffer trop petit
end;
```

**C#** :

```csharp
[DllImport("mylib.dll", CallingConvention = CallingConvention.Cdecl)]
public static extern void GetMessage(
    [MarshalAs(UnmanagedType.LPStr)] StringBuilder buffer,
    int bufferSize);

// Utilisation
var sb = new StringBuilder(256);
GetMessage(sb, sb.Capacity);
string message = sb.ToString();
Console.WriteLine(message);
```

#### Approche 3 : Allocation native + libération

**Pascal** :

```pascal
function CreateString(const text: PAnsiChar): PAnsiChar; cdecl; export;
var
  len: Integer;
begin
  len := StrLen(text);
  GetMem(Result, len + 1);
  StrCopy(Result, text);
end;

procedure FreeString(str: PAnsiChar); cdecl; export;
begin
  if str <> nil then
    FreeMem(str);
end;
```

**C#** :

```csharp
[DllImport("mylib.dll", CallingConvention = CallingConvention.Cdecl)]
public static extern IntPtr CreateString(string text);

[DllImport("mylib.dll", CallingConvention = CallingConvention.Cdecl)]
public static extern void FreeString(IntPtr str);

// Utilisation avec gestion de mémoire
IntPtr ptr = CreateString("Hello");
try
{
    string result = Marshal.PtrToStringAnsi(ptr);
    Console.WriteLine(result);
}
finally
{
    FreeString(ptr);  // IMPORTANT : libérer la mémoire
}
```

### Gestion des structures

Les structures (records en Pascal) peuvent être passées entre Pascal et C#.

**Pascal** :

```pascal
type
  TPoint = packed record
    X: Integer;
    Y: Integer;
  end;
  PPoint = ^TPoint;

function CreatePoint(x, y: Integer): TPoint; cdecl; export;
begin
  Result.X := x;
  Result.Y := y;
end;

procedure MovePoint(p: PPoint; dx, dy: Integer); cdecl; export;
begin
  Inc(p^.X, dx);
  Inc(p^.Y, dy);
end;

function DistanceToOrigin(p: TPoint): Double; cdecl; export;
begin
  Result := Sqrt(p.X * p.X + p.Y * p.Y);
end;
```

**C#** :

```csharp
// Structure correspondante avec attributs de marshalling
[StructLayout(LayoutKind.Sequential, Pack = 1)]
public struct Point
{
    public int X;
    public int Y;

    public override string ToString() => $"({X}, {Y})";
}

class NativeGeometry
{
    [DllImport("geometry.dll", CallingConvention = CallingConvention.Cdecl)]
    public static extern Point CreatePoint(int x, int y);

    [DllImport("geometry.dll", CallingConvention = CallingConvention.Cdecl)]
    public static extern void MovePoint(ref Point p, int dx, int dy);

    [DllImport("geometry.dll", CallingConvention = CallingConvention.Cdecl)]
    public static extern double DistanceToOrigin(Point p);
}

// Utilisation
class Program
{
    static void Main()
    {
        // Créer un point
        var point = NativeGeometry.CreatePoint(3, 4);
        Console.WriteLine($"Point créé : {point}");

        // Déplacer le point
        NativeGeometry.MovePoint(ref point, 1, 1);
        Console.WriteLine($"Après déplacement : {point}");

        // Calculer la distance
        double distance = NativeGeometry.DistanceToOrigin(point);
        Console.WriteLine($"Distance à l'origine : {distance:F2}");
    }
}
```

**Points critiques** :

- **`packed record`** en Pascal = **`Pack = 1`** en C#
- **`LayoutKind.Sequential`** : Les champs sont disposés dans l'ordre de déclaration
- Les types doivent correspondre exactement (Integer = int, Double = double)

### Callbacks - Appeler C# depuis Pascal

Les callbacks permettent au code Pascal d'appeler des fonctions C# (délégués).

**Pascal** :

```pascal
type
  // Type de callback : pointeur vers une procédure
  TProgressCallback = procedure(percent: Integer); cdecl;

procedure ProcessData(callback: TProgressCallback; data: PInteger;
  count: Integer); cdecl; export;
var
  i: Integer;
begin
  for i := 0 to count - 1 do
  begin
    // Simuler un traitement long
    Sleep(100);

    // Appeler le callback C# pour notifier la progression
    if Assigned(callback) then
      callback((i + 1) * 100 div count);
  end;
end;
```

**C#** :

```csharp
class NativeProcessor
{
    // Définir le type de délégué correspondant
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
    public delegate void ProgressCallback(int percent);

    [DllImport("processor.dll", CallingConvention = CallingConvention.Cdecl)]
    public static extern void ProcessData(
        ProgressCallback callback,
        int[] data,
        int count);
}

class Program
{
    // Méthode qui sera appelée depuis Pascal
    static void OnProgress(int percent)
    {
        Console.Write($"\rProgression : {percent}%   ");
        if (percent == 100)
            Console.WriteLine();
    }

    static void Main()
    {
        Console.WriteLine("Démarrage du traitement...\n");

        int[] data = new int[10];

        // CRITIQUE : Garder une référence au délégué !
        var callback = new NativeProcessor.ProgressCallback(OnProgress);

        NativeProcessor.ProcessData(callback, data, data.Length);

        // Empêcher le GC de collecter le délégué pendant l'appel
        GC.KeepAlive(callback);

        Console.WriteLine("\nTraitement terminé !");
    }
}
```

**Attention** : Le Garbage Collector .NET peut collecter le délégué si vous ne gardez pas de référence forte. Utilisez toujours `GC.KeepAlive()` ou stockez le délégué dans une variable de classe.

---

## Approche 2 : COM Interop

COM (Component Object Model) est une technologie Microsoft qui permet la communication bidirectionnelle entre applications.

### Vue d'ensemble

COM fournit :
- **Objets** : Pas seulement des fonctions, mais des objets avec méthodes et propriétés
- **Interfaces** : Contrats définissant les méthodes disponibles
- **Bidirectionnel** : .NET peut appeler Pascal et vice-versa
- **GUIDs** : Identification unique des objets et interfaces

### Créer un serveur COM en Pascal

**comserver.pas** :

```pascal
library comserver;

{$mode objfpc}{$H+}

uses
  ComObj, ComServ, SysUtils, Windows, ActiveX;

type
  // Interface COM avec GUID unique
  ICalculator = interface(IDispatch)
    ['{12345678-1234-1234-1234-123456789ABC}']
    function Add(a, b: Integer): Integer; safecall;
    function Multiply(a, b: Integer): Integer; safecall;
    function GetDescription: WideString; safecall;
  end;

  // Implémentation de l'interface
  TCalculator = class(TAutoObject, ICalculator)
  public
    function Add(a, b: Integer): Integer; safecall;
    function Multiply(a, b: Integer): Integer; safecall;
    function GetDescription: WideString; safecall;
  end;

const
  // GUID de classe (CLSID) - doit être unique
  CLASS_Calculator: TGUID = '{23456789-2345-2345-2345-23456789ABCD}';

{ TCalculator - Implémentation }

function TCalculator.Add(a, b: Integer): Integer;
begin
  Result := a + b;
end;

function TCalculator.Multiply(a, b: Integer): Integer;
begin
  Result := a * b;
end;

function TCalculator.GetDescription: WideString;
begin
  Result := 'Pascal COM Calculator v1.0';
end;

initialization
  // Enregistrer la classe COM
  TAutoObjectFactory.Create(
    ComServer,
    TCalculator,
    CLASS_Calculator,
    'Calculator',
    'Pascal Calculator COM Object',
    ciMultiInstance,  // Chaque client obtient sa propre instance
    tmApartment       // Modèle de threading
  );

end.
```

**Compilation** :

```bash
fpc -ocomserver.dll comserver.pas
```

**Enregistrement COM** :

```bash
# Enregistrer le serveur COM dans le registre Windows
regsvr32 comserver.dll

# Désenregistrer (si nécessaire)
regsvr32 /u comserver.dll
```

### Utiliser le serveur COM depuis C#

#### Méthode 1 : Type Library Importer (recommandé)

Générer un assembly d'interop :

```bash
tlbimp comserver.dll /out:ComServerInterop.dll
```

Utilisation en C# :

```csharp
using System;
using ComServerLib;  // Référence à ComServerInterop.dll

class Program
{
    static void Main()
    {
        Console.WriteLine("=== Test COM Interop ===\n");

        // Créer une instance de l'objet COM
        var calc = new Calculator();

        try
        {
            // Appeler les méthodes COM
            int sum = calc.Add(10, 5);
            int product = calc.Multiply(10, 5);
            string desc = calc.GetDescription();

            Console.WriteLine($"Description : {desc}");
            Console.WriteLine($"10 + 5 = {sum}");
            Console.WriteLine($"10 * 5 = {product}");
        }
        finally
        {
            // IMPORTANT : Libérer l'objet COM
            System.Runtime.InteropServices.Marshal.ReleaseComObject(calc);
        }
    }
}
```

#### Méthode 2 : Late Binding (dynamic)

Sans génération d'assembly :

```csharp
using System;

class Program
{
    static void Main()
    {
        // Créer l'objet COM via son ProgID
        Type calcType = Type.GetTypeFromProgID("Calculator");

        if (calcType == null)
        {
            Console.WriteLine("Objet COM non trouvé. Vérifiez l'enregistrement.");
            return;
        }

        dynamic calc = Activator.CreateInstance(calcType);

        try
        {
            // Appel dynamique (pas de vérification à la compilation)
            int sum = calc.Add(10, 5);
            string desc = calc.GetDescription();

            Console.WriteLine($"Description : {desc}");
            Console.WriteLine($"10 + 5 = {sum}");
        }
        finally
        {
            System.Runtime.InteropServices.Marshal.ReleaseComObject(calc);
        }
    }
}
```

### Avantages et inconvénients de COM

**Avantages** :
- Communication bidirectionnelle complète
- Objets avec état et méthodes multiples
- Standard Windows mature et bien supporté
- Callbacks et événements possibles

**Inconvénients** :
- Configuration complexe (enregistrement, GUIDs)
- Overhead léger de COM
- Nécessite des privilèges administrateur pour l'enregistrement
- Limité à Windows

---

## Approche 3 : Processus séparés avec IPC

Cette approche utilise la communication inter-processus (IPC) via sockets TCP/IP.

### Architecture

```
┌─────────────────┐       TCP/IP        ┌─────────────────┐
│   Client C#     │      (JSON)         │  Serveur Pascal │
│   (.NET)        │◀──────────────────▶│   (Native)       │
│   Process 1     │   Port 9090         │   Process 2     │
└─────────────────┘                     └─────────────────┘
```

### Serveur Pascal avec sockets

**server.pas** :

```pascal
program PascalServer;

{$mode objfpc}{$H+}

uses
  SysUtils, ssockets, fpjson, jsonparser;

type
  TRequest = record
    Action: string;
    A, B: Integer;
  end;

function ProcessRequest(const Req: TRequest): TJSONObject;
begin
  Result := TJSONObject.Create;

  if Req.Action = 'add' then
  begin
    Result.Add('result', Req.A + Req.B);
    Result.Add('success', True);
  end
  else if Req.Action = 'multiply' then
  begin
    Result.Add('result', Req.A * Req.B);
    Result.Add('success', True);
  end
  else if Req.Action = 'subtract' then
  begin
    Result.Add('result', Req.A - Req.B);
    Result.Add('success', True);
  end
  else if Req.Action = 'divide' then
  begin
    if Req.B <> 0 then
    begin
      Result.Add('result', Req.A / Req.B);
      Result.Add('success', True);
    end
    else
    begin
      Result.Add('success', False);
      Result.Add('error', 'Division par zéro');
    end;
  end
  else
  begin
    Result.Add('success', False);
    Result.Add('error', 'Action inconnue: ' + Req.Action);
  end;
end;

var
  Server: TInetServer;
  Client: TSocketStream;
  Line: string;
  JSON: TJSONData;
  Request: TRequest;
  Response: TJSONObject;

begin
  Server := TInetServer.Create('127.0.0.1', 9090);
  try
    WriteLn('====================================');
    WriteLn('Serveur Pascal démarré');
    WriteLn('Écoute sur 127.0.0.1:9090');
    WriteLn('====================================');
    WriteLn;

    while True do
    begin
      // Accepter une connexion cliente
      Client := Server.Accept;
      if Client <> nil then
      try
        WriteLn('[', FormatDateTime('hh:nn:ss', Now), '] Client connecté');

        // Lire la requête JSON
        SetLength(Line, 1024);
        Client.Read(Line[1], 1024);
        Line := Trim(Line);

        WriteLn('  Requête reçue: ', Line);

        // Parser le JSON
        JSON := GetJSON(Line);
        try
          if JSON is TJSONObject then
          begin
            // Extraire les données de la requête
            Request.Action := TJSONObject(JSON).Get('action', '');
            Request.A := TJSONObject(JSON).Get('a', 0);
            Request.B := TJSONObject(JSON).Get('b', 0);

            // Traiter la requête
            Response := ProcessRequest(Request);
            try
              // Envoyer la réponse
              Line := Response.AsJSON + #13#10;
              Client.Write(Line[1], Length(Line));
              WriteLn('  Réponse envoyée: ', Response.AsJSON);
            finally
              Response.Free;
            end;
          end;
        finally
          JSON.Free;
        end;

        WriteLn;

      finally
        Client.Free;
      end;
    end;

  finally
    Server.Free;
  end;
end.
```

**Compilation** :

```bash
fpc -oserver.exe server.pas
```

**Exécution** :

```bash
./server.exe
```

### Client C# avec sockets

**Program.cs** :

```csharp
using System;
using System.Net.Sockets;
using System.Text;
using System.Text.Json;

namespace PascalClient
{
    /// <summary>
    /// Client pour communiquer avec le serveur Pascal via TCP/IP
    /// </summary>
    public class PascalMathClient : IDisposable
    {
        private readonly string host;
        private readonly int port;

        public PascalMathClient(string host = "127.0.0.1", int port = 9090)
        {
            this.host = host;
            this.port = port;
        }

        /// <summary>
        /// Addition
        /// </summary>
        public int Add(int a, int b)
        {
            var request = new { action = "add", a, b };
            var response = SendRequest(request);
            return response.GetProperty("result").GetInt32();
        }

        /// <summary>
        /// Multiplication
        /// </summary>
        public int Multiply(int a, int b)
        {
            var request = new { action = "multiply", a, b };
            var response = SendRequest(request);
            return response.GetProperty("result").GetInt32();
        }

        /// <summary>
        /// Soustraction
        /// </summary>
        public int Subtract(int a, int b)
        {
            var request = new { action = "subtract", a, b };
            var response = SendRequest(request);
            return response.GetProperty("result").GetInt32();
        }

        /// <summary>
        /// Division
        /// </summary>
        public double Divide(int a, int b)
        {
            var request = new { action = "divide", a, b };
            var response = SendRequest(request);

            if (!response.GetProperty("success").GetBoolean())
            {
                string error = response.GetProperty("error").GetString();
                throw new InvalidOperationException(error);
            }

            return response.GetProperty("result").GetDouble();
        }

        /// <summary>
        /// Envoie une requête au serveur Pascal et retourne la réponse
        /// </summary>
        private JsonElement SendRequest(object request)
        {
            try
            {
                using var client = new TcpClient(host, port);
                using var stream = client.GetStream();

                // Sérialiser et envoyer la requête
                string json = JsonSerializer.Serialize(request) + "\n";
                byte[] data = Encoding.UTF8.GetBytes(json);
                stream.Write(data, 0, data.Length);

                // Recevoir la réponse
                byte[] buffer = new byte[1024];
                int bytesRead = stream.Read(buffer, 0, buffer.Length);
                string responseJson = Encoding.UTF8.GetString(buffer, 0, bytesRead);

                return JsonSerializer.Deserialize<JsonElement>(responseJson);
            }
            catch (SocketException ex)
            {
                throw new InvalidOperationException(
                    $"Impossible de se connecter au serveur Pascal sur {host}:{port}. " +
                    $"Vérifiez que le serveur est démarré.", ex);
            }
        }

        public void Dispose()
        {
            // Rien à nettoyer pour l'instant
        }
    }

    class Program
    {
        static void Main()
        {
            Console.WriteLine("=== Client .NET pour serveur Pascal ===\n");

            try
            {
                using var client = new PascalMathClient();

                // Tests des opérations
                Console.WriteLine("Tests des opérations mathématiques:\n");

                int sum = client.Add(10, 5);
                Console.WriteLine($"  10 + 5 = {sum}");

                int product = client.Multiply(10, 5);
                Console.WriteLine($"  10 × 5 = {product}");

                int difference = client.Subtract(10, 5);
                Console.WriteLine($"  10 - 5 = {difference}");

                double quotient = client.Divide(10, 5);
                Console.WriteLine($"  10 ÷ 5 = {quotient}");

                // Test de gestion d'erreur
                Console.WriteLine("\nTest de division par zéro:");
                try
                {
                    client.Divide(10, 0);
                }
                catch (InvalidOperationException ex)
                {
                    Console.WriteLine($"  Erreur capturée: {ex.Message}");
                }

                Console.WriteLine("\n✓ Tous les tests réussis!");
            }
            catch (Exception ex)
            {
                Console.WriteLine($"\n✗ Erreur: {ex.Message}");
            }

            Console.WriteLine("\nAppuyez sur une touche pour quitter...");
            Console.ReadKey();
        }
    }
}
```

**Compilation et exécution** :

```bash
# Compiler
dotnet new console -n PascalClient
cd PascalClient
# Copier Program.cs
dotnet run

# Ou avec csc
csc Program.cs
Program.exe
```

### Avantages de l'approche IPC

**Avantages** :
- **Isolation complète** : Crash d'un processus n'affecte pas l'autre
- **Flexibilité** : Protocole personnalisé (JSON, XML, Protocol Buffers, etc.)
- **Distribution** : Peut fonctionner sur des machines différentes
- **Indépendance** : Pas de dépendances binaires
- **Scalabilité** : Facile d'ajouter plusieurs clients ou serveurs

**Inconvénients** :
- **Latence** : Communication réseau plus lente que les appels directs
- **Sérialisation** : Overhead de conversion JSON
- **Complexité** : Gestion des connexions, timeouts, reconnexions
- **Déploiement** : Deux processus à gérer

---

## Marshalling avancé

Le marshalling est le processus de conversion des données entre le monde managé (.NET) et le monde natif (Pascal).

### Tableaux et allocation mémoire

#### Retourner un tableau depuis Pascal

**Pascal** :

```pascal
// Retourner un tableau de nombres premiers
function GetPrimes(count: Integer; out outArray: PInteger): Integer; cdecl; export;
const
  primes: array[0..24] of Integer =
    (2, 3, 5, 7, 11, 13, 17, 19, 23, 29,
     31, 37, 41, 43, 47, 53, 59, 61, 67, 71,
     73, 79, 83, 89, 97);
begin
  if count > 25 then count := 25;

  // Allouer la mémoire
  GetMem(outArray, count * SizeOf(Integer));

  // Copier les données
  Move(primes[0], outArray^, count * SizeOf(Integer));

  Result := count;
end;

// Libérer le tableau alloué
procedure FreeArray(arr: PInteger); cdecl; export;
begin
  if arr <> nil then
    FreeMem(arr);
end;
```

**C#** :

```csharp
class NativeArrays
{
    [DllImport("arrays.dll", CallingConvention = CallingConvention.Cdecl)]
    public static extern int GetPrimes(int count, out IntPtr outArray);

    [DllImport("arrays.dll", CallingConvention = CallingConvention.Cdecl)]
    public static extern void FreeArray(IntPtr arr);

    /// <summary>
    /// Wrapper managé qui gère automatiquement la mémoire
    /// </summary>
    public static int[] GetPrimesManaged(int count)
    {
        IntPtr ptr;
        int actualCount = GetPrimes(count, out ptr);

        try
        {
            // Copier les données natives vers un tableau managé
            int[] result = new int[actualCount];
            Marshal.Copy(ptr, result, 0, actualCount);
            return result;
        }
        finally
        {
            // CRITIQUE : Toujours libérer la mémoire native
            FreeArray(ptr);
        }
    }
}

// Utilisation simple
int[] primes = NativeArrays.GetPrimesManaged(10);
Console.WriteLine("Les 10 premiers nombres premiers:");
Console.WriteLine(string.Join(", ", primes));
```

### Structures complexes et imbriquées

**Pascal** :

```pascal
type
  TPerson = packed record
    Name: array[0..49] of AnsiChar;
    Age: Integer;
    Salary: Double;
  end;
  PPerson = ^TPerson;

// Créer et initialiser une personne
procedure FillPerson(p: PPerson); cdecl; export;
begin
  StrPCopy(p^.Name, 'John Doe');
  p^.Age := 30;
  p^.Salary := 50000.00;
end;

// Calculer le salaire moyen d'une équipe
function GetAverageSalary(persons: PPerson; count: Integer): Double; cdecl; export;
var
  i: Integer;
  total: Double;
begin
  total := 0;
  for i := 0 to count - 1 do
  begin
    total := total + persons^.Salary;
    Inc(persons);  // Avancer au prochain élément
  end;
  Result := total / count;
end;

// Trouver la personne la plus âgée
function FindOldest(persons: PPerson; count: Integer): TPerson; cdecl; export;
var
  i: Integer;
  oldest: PPerson;
begin
  oldest := persons;
  for i := 1 to count - 1 do
  begin
    Inc(persons);
    if persons^.Age > oldest^.Age then
      oldest := persons;
  end;
  Result := oldest^;
end;
```

**C#** :

```csharp
// Structure avec attributs de marshalling précis
[StructLayout(LayoutKind.Sequential, Pack = 1, CharSet = CharSet.Ansi)]
public struct Person
{
    [MarshalAs(UnmanagedType.ByValTStr, SizeConst = 50)]
    public string Name;

    public int Age;
    public double Salary;

    public override string ToString() =>
        $"{Name}, {Age} ans, {Salary:C}";
}

class NativePersons
{
    [DllImport("persons.dll", CallingConvention = CallingConvention.Cdecl)]
    public static extern void FillPerson(ref Person p);

    [DllImport("persons.dll", CallingConvention = CallingConvention.Cdecl)]
    public static extern double GetAverageSalary(Person[] persons, int count);

    [DllImport("persons.dll", CallingConvention = CallingConvention.Cdecl)]
    public static extern Person FindOldest(Person[] persons, int count);
}

// Utilisation
class Program
{
    static void Main()
    {
        // Test 1 : Remplir une personne
        var person = new Person();
        NativePersons.FillPerson(ref person);
        Console.WriteLine($"Personne créée: {person}");

        // Test 2 : Équipe et salaire moyen
        var team = new Person[]
        {
            new Person { Name = "Alice", Age = 25, Salary = 45000 },
            new Person { Name = "Bob", Age = 30, Salary = 55000 },
            new Person { Name = "Charlie", Age = 35, Salary = 65000 },
            new Person { Name = "Diana", Age = 28, Salary = 50000 }
        };

        double avgSalary = NativePersons.GetAverageSalary(team, team.Length);
        Console.WriteLine($"\nSalaire moyen de l'équipe: {avgSalary:C}");

        // Test 3 : Trouver le plus âgé
        Person oldest = NativePersons.FindOldest(team, team.Length);
        Console.WriteLine($"Personne la plus âgée: {oldest}");
    }
}
```

**Points critiques du marshalling** :

1. **`Pack = 1`** : Alignement des octets (correspond à `packed record`)
2. **`LayoutKind.Sequential`** : Ordre des champs préservé
3. **`ByValTStr`** : Chaîne de taille fixe dans la structure
4. **Tailles identiques** : Les structures doivent avoir la même taille en octets

### Structures avec pointeurs

**Pascal** :

```pascal
type
  TNode = packed record
    Value: Integer;
    Next: PNode;  // Pointeur vers le prochain nœud
  end;
  PNode = ^TNode;

// Créer une liste chaînée
function CreateList(values: PInteger; count: Integer): PNode; cdecl; export;
var
  i: Integer;
  current, previous: PNode;
begin
  Result := nil;
  previous := nil;

  for i := 0 to count - 1 do
  begin
    New(current);
    current^.Value := values^;
    current^.Next := nil;

    if Result = nil then
      Result := current
    else
      previous^.Next := current;

    previous := current;
    Inc(values);
  end;
end;

// Libérer une liste chaînée
procedure FreeList(node: PNode); cdecl; export;
var
  next: PNode;
begin
  while node <> nil do
  begin
    next := node^.Next;
    Dispose(node);
    node := next;
  end;
end;

// Compter les éléments
function CountList(node: PNode): Integer; cdecl; export;
var
  count: Integer;
begin
  count := 0;
  while node <> nil do
  begin
    Inc(count);
    node := node^.Next;
  end;
  Result := count;
end;
```

**C#** :

```csharp
[StructLayout(LayoutKind.Sequential, Pack = 1)]
public struct Node
{
    public int Value;
    public IntPtr Next;  // Pointeur vers le prochain nœud
}

class NativeLinkedList
{
    [DllImport("list.dll", CallingConvention = CallingConvention.Cdecl)]
    public static extern IntPtr CreateList(int[] values, int count);

    [DllImport("list.dll", CallingConvention = CallingConvention.Cdecl)]
    public static extern void FreeList(IntPtr node);

    [DllImport("list.dll", CallingConvention = CallingConvention.Cdecl)]
    public static extern int CountList(IntPtr node);

    /// <summary>
    /// Convertir une liste native en List<int> managée
    /// </summary>
    public static List<int> ToManagedList(IntPtr head)
    {
        var result = new List<int>();
        IntPtr current = head;

        while (current != IntPtr.Zero)
        {
            // Lire la structure depuis la mémoire native
            Node node = Marshal.PtrToStructure<Node>(current);
            result.Add(node.Value);
            current = node.Next;
        }

        return result;
    }
}

// Utilisation
int[] values = { 1, 2, 3, 4, 5 };
IntPtr listHead = NativeLinkedList.CreateList(values, values.Length);

try
{
    int count = NativeLinkedList.CountList(listHead);
    Console.WriteLine($"Nombre d'éléments: {count}");

    List<int> managedList = NativeLinkedList.ToManagedList(listHead);
    Console.WriteLine($"Éléments: {string.Join(", ", managedList)}");
}
finally
{
    NativeLinkedList.FreeList(listHead);
}
```

---

## Wrapper C# professionnel

Créer une bibliothèque C# conviviale qui encapsule les appels natifs.

### Structure du projet

```
MathLibrary/
├── src/
│   ├── MathLib.cs              // Classe principale
│   ├── NativeMethods.cs        // Imports P/Invoke
│   └── MathException.cs        // Exceptions personnalisées
├── native/
│   ├── mathlib.pas             // Code Pascal
│   └── mathlib.dll             // DLL compilée
└── MathLibrary.csproj          // Fichier projet
```

### NativeMethods.cs - Imports P/Invoke

```csharp
using System;
using System.Runtime.InteropServices;

namespace MathLibrary.Native
{
    /// <summary>
    /// Méthodes natives importées depuis mathlib.dll
    /// </summary>
    internal static class NativeMethods
    {
        private const string DLL_NAME = "mathlib.dll";

        [DllImport(DLL_NAME, CallingConvention = CallingConvention.Cdecl)]
        internal static extern int Native_Add(int a, int b);

        [DllImport(DLL_NAME, CallingConvention = CallingConvention.Cdecl,
                   EntryPoint = "Power")]
        internal static extern double Native_Power(double baseValue, double exponent);

        [DllImport(DLL_NAME, CallingConvention = CallingConvention.Cdecl,
                   EntryPoint = "SumArray")]
        internal static extern int Native_SumArray(int[] arr, int length);

        [DllImport(DLL_NAME, CallingConvention = CallingConvention.Cdecl,
                   EntryPoint = "GetVersion")]
        internal static extern IntPtr Native_GetVersion();
    }
}
```

### MathException.cs - Exceptions personnalisées

```csharp
using System;

namespace MathLibrary
{
    /// <summary>
    /// Exception levée lors d'erreurs dans la bibliothèque mathématique
    /// </summary>
    public class MathLibraryException : Exception
    {
        public MathLibraryException(string message) : base(message)
        {
        }

        public MathLibraryException(string message, Exception innerException)
            : base(message, innerException)
        {
        }
    }
}
```

### MathLib.cs - API publique

```csharp
using System;
using System.Runtime.InteropServices;
using MathLibrary.Native;

namespace MathLibrary
{
    /// <summary>
    /// Bibliothèque mathématique haute performance utilisant du code natif Pascal
    /// </summary>
    public class MathLib : IDisposable
    {
        private bool _disposed = false;
        private static readonly object _lock = new object();
        private static bool _isInitialized = false;

        /// <summary>
        /// Version de la bibliothèque native
        /// </summary>
        public static string Version
        {
            get
            {
                try
                {
                    IntPtr ptr = NativeMethods.Native_GetVersion();
                    return Marshal.PtrToStringAnsi(ptr) ?? "Unknown";
                }
                catch (DllNotFoundException)
                {
                    throw new MathLibraryException(
                        "La bibliothèque native mathlib.dll n'a pas été trouvée. " +
                        "Assurez-vous qu'elle est dans le même dossier que l'exécutable.");
                }
            }
        }

        /// <summary>
        /// Initialise la bibliothèque (optionnel, appelé automatiquement)
        /// </summary>
        public static void Initialize()
        {
            lock (_lock)
            {
                if (!_isInitialized)
                {
                    // Vérifier que la DLL est accessible
                    _ = Version;
                    _isInitialized = true;
                }
            }
        }

        /// <summary>
        /// Additionne deux nombres entiers
        /// </summary>
        /// <param name="a">Premier nombre</param>
        /// <param name="b">Deuxième nombre</param>
        /// <returns>La somme de a et b</returns>
        /// <exception cref="MathLibraryException">Si la bibliothèque native n'est pas disponible</exception>
        public int Add(int a, int b)
        {
            ThrowIfDisposed();

            try
            {
                return NativeMethods.Native_Add(a, b);
            }
            catch (DllNotFoundException ex)
            {
                throw new MathLibraryException(
                    "Impossible d'appeler la fonction Add: DLL non trouvée", ex);
            }
        }

        /// <summary>
        /// Calcule base^exposant
        /// </summary>
        /// <param name="baseValue">La base</param>
        /// <param name="exponent">L'exposant</param>
        /// <returns>Le résultat de base^exposant</returns>
        /// <exception cref="ArgumentException">Si les paramètres sont invalides</exception>
        public double Power(double baseValue, double exponent)
        {
            ThrowIfDisposed();

            // Validation des paramètres
            if (double.IsNaN(baseValue) || double.IsInfinity(baseValue))
                throw new ArgumentException("La base ne peut pas être NaN ou Infinity",
                    nameof(baseValue));

            if (double.IsNaN(exponent) || double.IsInfinity(exponent))
                throw new ArgumentException("L'exposant ne peut pas être NaN ou Infinity",
                    nameof(exponent));

            if (baseValue < 0 && Math.Floor(exponent) != exponent)
                throw new ArgumentException(
                    "Impossible de calculer la puissance d'un nombre négatif " +
                    "avec un exposant non entier");

            try
            {
                return NativeMethods.Native_Power(baseValue, exponent);
            }
            catch (DllNotFoundException ex)
            {
                throw new MathLibraryException(
                    "Impossible d'appeler la fonction Power: DLL non trouvée", ex);
            }
        }

        /// <summary>
        /// Calcule la somme des éléments d'un tableau
        /// </summary>
        /// <param name="values">Le tableau de valeurs</param>
        /// <returns>La somme de tous les éléments</returns>
        /// <exception cref="ArgumentNullException">Si le tableau est null</exception>
        public int Sum(params int[] values)
        {
            ThrowIfDisposed();

            if (values == null)
                throw new ArgumentNullException(nameof(values));

            if (values.Length == 0)
                return 0;

            try
            {
                return NativeMethods.Native_SumArray(values, values.Length);
            }
            catch (DllNotFoundException ex)
            {
                throw new MathLibraryException(
                    "Impossible d'appeler la fonction SumArray: DLL non trouvée", ex);
            }
        }

        /// <summary>
        /// Calcule la moyenne des éléments d'un tableau
        /// </summary>
        public double Average(params int[] values)
        {
            if (values == null || values.Length == 0)
                throw new ArgumentException("Le tableau ne peut pas être vide");

            int sum = Sum(values);
            return (double)sum / values.Length;
        }

        private void ThrowIfDisposed()
        {
            if (_disposed)
                throw new ObjectDisposedException(nameof(MathLib));
        }

        public void Dispose()
        {
            if (!_disposed)
            {
                // Nettoyer les ressources si nécessaire
                _disposed = true;
                GC.SuppressFinalize(this);
            }
        }

        ~MathLib()
        {
            Dispose();
        }
    }
}
```

### Utilisation du wrapper

```csharp
using System;
using MathLibrary;

class Program
{
    static void Main()
    {
        Console.WriteLine("=== Démonstration MathLibrary ===\n");

        try
        {
            // Afficher la version
            Console.WriteLine($"Version de la bibliothèque: {MathLib.Version}\n");

            using (var math = new MathLib())
            {
                // Addition
                int sum = math.Add(42, 58);
                Console.WriteLine($"42 + 58 = {sum}");

                // Puissance
                double power = math.Power(2, 10);
                Console.WriteLine($"2^10 = {power}");

                // Somme de tableau
                int total = math.Sum(1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
                Console.WriteLine($"Somme de 1 à 10 = {total}");

                // Moyenne
                double avg = math.Average(10, 20, 30, 40, 50);
                Console.WriteLine($"Moyenne de [10,20,30,40,50] = {avg}");
            }

            Console.WriteLine("\n✓ Tous les tests réussis!");
        }
        catch (MathLibraryException ex)
        {
            Console.WriteLine($"\n✗ Erreur bibliothèque: {ex.Message}");
        }
        catch (Exception ex)
        {
            Console.WriteLine($"\n✗ Erreur: {ex.Message}");
        }

        Console.WriteLine("\nAppuyez sur une touche pour quitter...");
        Console.ReadKey();
    }
}
```

---

## Bonnes pratiques

### 1. Gestion de la mémoire

**Règle d'or** : Qui alloue, libère.

```csharp
// ✓ BON : Libération systématique
IntPtr ptr = NativeMethods.AllocateBuffer(1024);
try
{
    // Utiliser le buffer
}
finally
{
    NativeMethods.FreeBuffer(ptr);  // Toujours exécuté
}

// ✗ MAUVAIS : Oubli de libération
IntPtr ptr = NativeMethods.AllocateBuffer(1024);
// Utiliser le buffer... mais pas de Free() = fuite mémoire!
```

### 2. Conventions d'appel

Toujours utiliser `cdecl` pour la compatibilité maximale :

```pascal
// ✓ BON
function Add(a, b: Integer): Integer; cdecl; export;

// ✗ MAUVAIS (défaut = register sur FreePascal)
function Add(a, b: Integer): Integer; export;
```

```csharp
// ✓ BON
[DllImport("lib.dll", CallingConvention = CallingConvention.Cdecl)]
public static extern int MyFunction(int x);

// ✗ MAUVAIS (défaut = WinAPI/StdCall)
[DllImport("lib.dll")]
public static extern int MyFunction(int x);
```

### 3. Gestion des erreurs

**Ne jamais ignorer les erreurs** :

```pascal
// ✓ BON : Retourner un code d'erreur
function ProcessData(data: PByte; size: Integer): Integer; cdecl; export;
begin
  try
    // Traitement...
    Result := 0;  // Succès
  except
    on E: Exception do
      Result := -1;  // Erreur
  end;
end;
```

```csharp
// ✓ BON : Vérifier les codes d'erreur
int result = NativeMethods.ProcessData(buffer, size);
if (result != 0)
{
    throw new InvalidOperationException("Échec du traitement des données");
}
```

**Utiliser des structures pour les erreurs détaillées** :

```pascal
type
  TResult = packed record
    Success: Boolean;
    ErrorCode: Integer;
    ErrorMessage: array[0..255] of AnsiChar;
  end;

function ComplexOperation(param: Integer): TResult; cdecl; export;
begin
  Result.Success := False;
  Result.ErrorCode := 0;
  Result.ErrorMessage[0] := #0;

  try
    // Opération complexe...
    Result.Success := True;
  except
    on E: Exception do
    begin
      Result.ErrorCode := 1;
      StrPCopy(Result.ErrorMessage, E.Message);
    end;
  end;
end;
```

```csharp
[StructLayout(LayoutKind.Sequential, Pack = 1, CharSet = CharSet.Ansi)]
public struct Result
{
    [MarshalAs(UnmanagedType.I1)]
    public bool Success;

    public int ErrorCode;

    [MarshalAs(UnmanagedType.ByValTStr, SizeConst = 256)]
    public string ErrorMessage;
}

// Utilisation
Result result = NativeMethods.ComplexOperation(42);
if (!result.Success)
{
    throw new Exception($"Erreur {result.ErrorCode}: {result.ErrorMessage}");
}
```

### 4. Thread Safety

**Protéger les ressources partagées** :

```pascal
var
  CriticalSection: TRTLCriticalSection;

procedure InitLibrary; cdecl; export;
begin
  InitCriticalSection(CriticalSection);
end;

function ThreadSafeOperation(value: Integer): Integer; cdecl; export;
begin
  EnterCriticalSection(CriticalSection);
  try
    // Opération critique...
    Result := value * 2;
  finally
    LeaveCriticalSection(CriticalSection);
  end;
end;

procedure CleanupLibrary; cdecl; export;
begin
  DoneCriticalSection(CriticalSection);
end;
```

```csharp
// C# - Thread safe par design avec lock
private static readonly object _lock = new object();

public int ThreadSafeCall(int value)
{
    lock (_lock)
    {
        return NativeMethods.ThreadSafeOperation(value);
    }
}
```

### 5. Documentation et IntelliSense

**Documenter les fonctions avec XML comments** :

```csharp
/// <summary>
/// Calcule la distance euclidienne entre deux points 3D.
/// </summary>
/// <param name="x1">Coordonnée X du premier point</param>
/// <param name="y1">Coordonnée Y du premier point</param>
/// <param name="z1">Coordonnée Z du premier point</param>
/// <param name="x2">Coordonnée X du deuxième point</param>
/// <param name="y2">Coordonnée Y du deuxième point</param>
/// <param name="z2">Coordonnée Z du deuxième point</param>
/// <returns>La distance entre les deux points</returns>
/// <exception cref="MathLibraryException">
/// Si la bibliothèque native n'est pas disponible
/// </exception>
/// <example>
/// <code>
/// var math = new MathLib();
/// double distance = math.Distance3D(0, 0, 0, 1, 1, 1);
/// Console.WriteLine($"Distance: {distance}"); // Affiche: 1.732...
/// </code>
/// </example>
[DllImport("mathlib.dll", CallingConvention = CallingConvention.Cdecl)]
public static extern double Distance3D(
    double x1, double y1, double z1,
    double x2, double y2, double z2);
```

### 6. Validation des paramètres

**Toujours valider avant d'appeler le code natif** :

```csharp
public double Divide(int a, int b)
{
    // ✓ BON : Validation avant l'appel natif
    if (b == 0)
        throw new ArgumentException("Le diviseur ne peut pas être zéro", nameof(b));

    return NativeMethods.Divide(a, b);
}

public void ProcessArray(int[] data)
{
    // ✓ BON : Vérifier null et taille
    if (data == null)
        throw new ArgumentNullException(nameof(data));

    if (data.Length == 0)
        throw new ArgumentException("Le tableau ne peut pas être vide", nameof(data));

    if (data.Length > 10000)
        throw new ArgumentException("Le tableau est trop grand (max 10000)", nameof(data));

    NativeMethods.ProcessArray(data, data.Length);
}
```

### 7. Logging et débogage

**Ajouter des logs pour faciliter le débogage** :

```csharp
using System;
using System.Diagnostics;

public class MathLib
{
    private static readonly TraceSource _trace =
        new TraceSource("MathLibrary", SourceLevels.All);

    public int Add(int a, int b)
    {
        _trace.TraceInformation($"Add appelé avec a={a}, b={b}");

        try
        {
            int result = NativeMethods.Native_Add(a, b);
            _trace.TraceInformation($"Add retourne {result}");
            return result;
        }
        catch (Exception ex)
        {
            _trace.TraceError($"Erreur dans Add: {ex.Message}");
            throw;
        }
    }
}
```

**Configuration du logging (App.config)** :

```xml
<?xml version="1.0" encoding="utf-8" ?>
<configuration>
  <system.diagnostics>
    <sources>
      <source name="MathLibrary" switchValue="All">
        <listeners>
          <add name="console" />
          <add name="file" />
        </listeners>
      </source>
    </sources>
    <sharedListeners>
      <add name="console"
           type="System.Diagnostics.ConsoleTraceListener" />
      <add name="file"
           type="System.Diagnostics.TextWriterTraceListener"
           initializeData="mathlib.log" />
    </sharedListeners>
  </system.diagnostics>
</configuration>
```

### 8. Tests unitaires

**Tester l'interop avec des tests automatisés** :

```csharp
using Xunit;
using MathLibrary;

public class MathLibTests
{
    [Fact]
    public void Add_TwoPositiveNumbers_ReturnsSum()
    {
        // Arrange
        using var math = new MathLib();

        // Act
        int result = math.Add(5, 7);

        // Assert
        Assert.Equal(12, result);
    }

    [Fact]
    public void Power_TwoToTen_Returns1024()
    {
        // Arrange
        using var math = new MathLib();

        // Act
        double result = math.Power(2, 10);

        // Assert
        Assert.Equal(1024.0, result, precision: 5);
    }

    [Fact]
    public void Power_NegativeBaseWithFractionalExponent_ThrowsArgumentException()
    {
        // Arrange
        using var math = new MathLib();

        // Act & Assert
        Assert.Throws<ArgumentException>(() => math.Power(-5, 0.5));
    }

    [Fact]
    public void Sum_EmptyArray_ReturnsZero()
    {
        // Arrange
        using var math = new MathLib();

        // Act
        int result = math.Sum();

        // Assert
        Assert.Equal(0, result);
    }

    [Fact]
    public void Sum_NullArray_ThrowsArgumentNullException()
    {
        // Arrange
        using var math = new MathLib();

        // Act & Assert
        Assert.Throws<ArgumentNullException>(() => math.Sum(null));
    }
}
```

### 9. Déploiement de la DLL

**Plusieurs stratégies pour distribuer la DLL native** :

#### Stratégie 1 : Copier la DLL avec l'exécutable

```xml
<!-- Dans le .csproj -->
<ItemGroup>
  <None Include="native\mathlib.dll">
    <CopyToOutputDirectory>PreserveNewest</CopyToOutputDirectory>
  </None>
</ItemGroup>
```

#### Stratégie 2 : Ressources embarquées

```csharp
public static class NativeLibraryLoader
{
    private static bool _loaded = false;

    public static void EnsureLoaded()
    {
        if (_loaded) return;

        // Extraire la DLL depuis les ressources
        string tempPath = Path.Combine(Path.GetTempPath(), "mathlib.dll");

        using (var stream = Assembly.GetExecutingAssembly()
            .GetManifestResourceStream("MathLibrary.native.mathlib.dll"))
        using (var file = File.Create(tempPath))
        {
            stream.CopyTo(file);
        }

        // Charger explicitement la DLL
        LoadLibrary(tempPath);
        _loaded = true;
    }

    [DllImport("kernel32", SetLastError = true, CharSet = CharSet.Unicode)]
    private static extern IntPtr LoadLibrary(string lpFileName);
}
```

#### Stratégie 3 : NuGet Package

**Structure du package NuGet** :

```
MathLibrary.1.0.0.nupkg
├── lib/
│   └── net6.0/
│       └── MathLibrary.dll          (Assembly managé)
└── runtimes/
    ├── win-x64/
    │   └── native/
    │       └── mathlib.dll          (DLL native 64-bit)
    └── win-x86/
        └── native/
            └── mathlib.dll          (DLL native 32-bit)
```

**Fichier .nuspec** :

```xml
<?xml version="1.0"?>
<package>
  <metadata>
    <id>MathLibrary</id>
    <version>1.0.0</version>
    <authors>Votre Nom</authors>
    <description>Bibliothèque mathématique haute performance</description>
    <dependencies>
      <group targetFramework="net6.0" />
    </dependencies>
  </metadata>
  <files>
    <file src="bin\Release\net6.0\MathLibrary.dll" target="lib\net6.0" />
    <file src="native\win-x64\mathlib.dll" target="runtimes\win-x64\native" />
    <file src="native\win-x86\mathlib.dll" target="runtimes\win-x86\native" />
  </files>
</package>
```

---

## Scénarios avancés

### 1. Callbacks complexes avec contexte

**Pascal** :

```pascal
type
  TProgressCallback = procedure(context: Pointer; percent: Integer); cdecl;

procedure ProcessWithContext(callback: TProgressCallback; context: Pointer;
  data: PInteger; count: Integer); cdecl; export;
var
  i: Integer;
begin
  for i := 0 to count - 1 do
  begin
    // Traitement...
    Sleep(50);

    // Appeler le callback avec le contexte
    if Assigned(callback) then
      callback(context, (i + 1) * 100 div count);
  end;
end;
```

**C#** :

```csharp
public class ProgressReporter
{
    private int _totalItems;
    private int _processedItems;

    [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
    private delegate void ProgressCallbackDelegate(IntPtr context, int percent);

    public void ProcessData(int[] data)
    {
        _totalItems = data.Length;
        _processedItems = 0;

        // Créer un GCHandle pour passer 'this' au code natif
        GCHandle handle = GCHandle.Alloc(this);
        try
        {
            var callback = new ProgressCallbackDelegate(OnProgressCallback);
            NativeMethods.ProcessWithContext(
                callback,
                GCHandle.ToIntPtr(handle),
                data,
                data.Length
            );

            GC.KeepAlive(callback);
        }
        finally
        {
            handle.Free();
        }
    }

    private static void OnProgressCallback(IntPtr context, int percent)
    {
        // Récupérer l'instance depuis le contexte
        GCHandle handle = GCHandle.FromIntPtr(context);
        var reporter = (ProgressReporter)handle.Target;

        reporter._processedItems++;
        Console.WriteLine($"Progression: {percent}% ({reporter._processedItems}/{reporter._totalItems})");
    }
}
```

### 2. Gestion des chaînes Unicode

**Pascal** :

```pascal
function GetUnicodeMessage: PWideChar; cdecl; export;
const
  MSG: WideString = 'Bonjour 世界 🌍';
begin
  Result := PWideChar(MSG);
end;

procedure ProcessUnicodeString(str: PWideChar); cdecl; export;
var
  s: WideString;
begin
  s := str;
  WriteLn('Reçu depuis C#: ', s);
end;
```

**C#** :

```csharp
[DllImport("lib.dll", CallingConvention = CallingConvention.Cdecl,
           CharSet = CharSet.Unicode)]
[return: MarshalAs(UnmanagedType.LPWStr)]
public static extern string GetUnicodeMessage();

[DllImport("lib.dll", CallingConvention = CallingConvention.Cdecl,
           CharSet = CharSet.Unicode)]
public static extern void ProcessUnicodeString(
    [MarshalAs(UnmanagedType.LPWStr)] string str);

// Utilisation
string message = GetUnicodeMessage();
Console.WriteLine(message);  // Affiche: Bonjour 世界 🌍

ProcessUnicodeString("Message depuis C# avec émojis: 😀🎉");
```

### 3. Objets opaques (Handles)

Parfois, il est nécessaire de manipuler des objets Pascal complexes depuis C# sans en connaître la structure interne.

**Pascal** :

```pascal
type
  TDatabase = class
  private
    FConnectionString: string;
    FConnected: Boolean;
  public
    constructor Create(const ConnectionString: string);
    function Connect: Boolean;
    procedure Disconnect;
    function ExecuteQuery(const SQL: string): Integer;
  end;

// Wrapper C pour exposer la classe
function Database_Create(connStr: PAnsiChar): Pointer; cdecl; export;
begin
  Result := TDatabase.Create(string(connStr));
end;

procedure Database_Free(db: Pointer); cdecl; export;
begin
  TDatabase(db).Free;
end;

function Database_Connect(db: Pointer): Boolean; cdecl; export;
begin
  Result := TDatabase(db).Connect;
end;

procedure Database_Disconnect(db: Pointer); cdecl; export;
begin
  TDatabase(db).Disconnect;
end;

function Database_ExecuteQuery(db: Pointer; sql: PAnsiChar): Integer; cdecl; export;
begin
  Result := TDatabase(db).ExecuteQuery(string(sql));
end;

{ TDatabase implementation }

constructor TDatabase.Create(const ConnectionString: string);
begin
  FConnectionString := ConnectionString;
  FConnected := False;
end;

function TDatabase.Connect: Boolean;
begin
  // Logique de connexion...
  FConnected := True;
  Result := True;
end;

procedure TDatabase.Disconnect;
begin
  FConnected := False;
end;

function TDatabase.ExecuteQuery(const SQL: string): Integer;
begin
  if not FConnected then
    raise Exception.Create('Non connecté');

  // Exécuter la requête...
  Result := 42;  // Nombre de lignes affectées
end;
```

**C#** :

```csharp
/// <summary>
/// Wrapper managé pour la classe Pascal TDatabase
/// </summary>
public class Database : IDisposable
{
    private IntPtr _handle;
    private bool _disposed = false;

    [DllImport("database.dll", CallingConvention = CallingConvention.Cdecl)]
    private static extern IntPtr Database_Create(
        [MarshalAs(UnmanagedType.LPStr)] string connStr);

    [DllImport("database.dll", CallingConvention = CallingConvention.Cdecl)]
    private static extern void Database_Free(IntPtr db);

    [DllImport("database.dll", CallingConvention = CallingConvention.Cdecl)]
    [return: MarshalAs(UnmanagedType.I1)]
    private static extern bool Database_Connect(IntPtr db);

    [DllImport("database.dll", CallingConvention = CallingConvention.Cdecl)]
    private static extern void Database_Disconnect(IntPtr db);

    [DllImport("database.dll", CallingConvention = CallingConvention.Cdecl)]
    private static extern int Database_ExecuteQuery(
        IntPtr db,
        [MarshalAs(UnmanagedType.LPStr)] string sql);

    public Database(string connectionString)
    {
        _handle = Database_Create(connectionString);
        if (_handle == IntPtr.Zero)
            throw new InvalidOperationException("Échec de création de la base de données");
    }

    public bool Connect()
    {
        ThrowIfDisposed();
        return Database_Connect(_handle);
    }

    public void Disconnect()
    {
        ThrowIfDisposed();
        Database_Disconnect(_handle);
    }

    public int ExecuteQuery(string sql)
    {
        ThrowIfDisposed();

        if (string.IsNullOrWhiteSpace(sql))
            throw new ArgumentException("La requête SQL ne peut pas être vide", nameof(sql));

        return Database_ExecuteQuery(_handle, sql);
    }

    private void ThrowIfDisposed()
    {
        if (_disposed)
            throw new ObjectDisposedException(nameof(Database));
    }

    public void Dispose()
    {
        if (!_disposed)
        {
            if (_handle != IntPtr.Zero)
            {
                Database_Free(_handle);
                _handle = IntPtr.Zero;
            }
            _disposed = true;
            GC.SuppressFinalize(this);
        }
    }

    ~Database()
    {
        Dispose();
    }
}

// Utilisation
using (var db = new Database("Server=localhost;Database=test"))
{
    if (db.Connect())
    {
        Console.WriteLine("Connecté!");

        int affected = db.ExecuteQuery("UPDATE users SET active = 1");
        Console.WriteLine($"{affected} lignes affectées");

        db.Disconnect();
    }
}
```

### 4. Gestion des événements

**Pascal avec événements** :

```pascal
type
  TEventCallback = procedure(eventType: Integer; data: Pointer); cdecl;

var
  GlobalEventCallback: TEventCallback = nil;

procedure RegisterEventCallback(callback: TEventCallback); cdecl; export;
begin
  GlobalEventCallback := callback;
end;

procedure UnregisterEventCallback; cdecl; export;
begin
  GlobalEventCallback := nil;
end;

procedure TriggerEvent(eventType: Integer; data: Pointer); cdecl; export;
begin
  if Assigned(GlobalEventCallback) then
    GlobalEventCallback(eventType, data);
end;

// Simuler des événements depuis un thread
procedure StartEventGenerator; cdecl; export;
var
  Thread: TThread;
begin
  Thread := TThread.CreateAnonymousThread(
    procedure
    var
      i: Integer;
    begin
      for i := 1 to 10 do
      begin
        Sleep(1000);
        if Assigned(GlobalEventCallback) then
          GlobalEventCallback(1, Pointer(i));
      end;
    end
  );
  Thread.FreeOnTerminate := True;
  Thread.Start;
end;
```

**C# avec gestion d'événements** :

```csharp
public class EventManager : IDisposable
{
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
    private delegate void EventCallbackDelegate(int eventType, IntPtr data);

    [DllImport("events.dll", CallingConvention = CallingConvention.Cdecl)]
    private static extern void RegisterEventCallback(EventCallbackDelegate callback);

    [DllImport("events.dll", CallingConvention = CallingConvention.Cdecl)]
    private static extern void UnregisterEventCallback();

    [DllImport("events.dll", CallingConvention = CallingConvention.Cdecl)]
    private static extern void StartEventGenerator();

    // Événement .NET
    public event EventHandler<EventArgs> NativeEventReceived;

    private EventCallbackDelegate _callback;
    private bool _disposed = false;

    public EventManager()
    {
        // IMPORTANT : Garder une référence au délégué
        _callback = OnNativeEvent;
        RegisterEventCallback(_callback);
    }

    public void StartGenerator()
    {
        StartEventGenerator();
    }

    private void OnNativeEvent(int eventType, IntPtr data)
    {
        // Marshaller les données si nécessaire
        int value = data.ToInt32();

        Console.WriteLine($"Événement natif reçu: Type={eventType}, Data={value}");

        // Déclencher l'événement .NET
        NativeEventReceived?.Invoke(this, new EventArgs());
    }

    public void Dispose()
    {
        if (!_disposed)
        {
            UnregisterEventCallback();
            _callback = null;
            _disposed = true;
            GC.SuppressFinalize(this);
        }
    }

    ~EventManager()
    {
        Dispose();
    }
}

// Utilisation
class Program
{
    static void Main()
    {
        using (var manager = new EventManager())
        {
            manager.NativeEventReceived += (sender, e) =>
            {
                Console.WriteLine("Événement .NET déclenché!");
            };

            Console.WriteLine("Démarrage du générateur d'événements...");
            manager.StartGenerator();

            Console.WriteLine("Appuyez sur une touche pour quitter...");
            Console.ReadKey();
        }
    }
}
```

---

## Débogage et diagnostics

### 1. Débogage mixte (Pascal + C#)

**Visual Studio** permet de déboguer simultanément le code managé et natif.

**Configuration dans Visual Studio** :

1. Projet C# → Propriétés → Déboguer
2. Cocher "Activer le débogage du code natif"
3. Placer des points d'arrêt dans le code C# et Pascal
4. Le débogueur s'arrêtera dans les deux

**Lazarus** :

1. Compiler la DLL Pascal avec `-g` (informations de débogage)
2. Utiliser GDB pour déboguer

```bash
fpc -g -omathlib.dll mathlib.pas
```

### 2. Diagnostics avec Dependency Walker

**Dependency Walker** affiche les dépendances DLL et les exports.

```bash
# Télécharger depuis dependencywalker.com
depends.exe mathlib.dll
```

Vérifier :
- Les fonctions exportées sont visibles
- Les dépendances (msvcrt.dll, etc.) sont présentes
- Les conventions d'appel sont correctes

### 3. Outils de profilage

**dotTrace** (JetBrains) :
- Profile les appels P/Invoke
- Mesure le temps passé dans le code natif
- Identifie les goulots d'étranglement

**Visual Studio Profiler** :
- Performance Profiler → CPU Usage
- Affiche les appels natifs et managés

### 4. Logs détaillés avec ETW

**Event Tracing for Windows** permet des logs haute performance.

```csharp
using System.Diagnostics.Tracing;

[EventSource(Name = "MathLibrary-Events")]
public class MathLibraryEventSource : EventSource
{
    public static readonly MathLibraryEventSource Log = new MathLibraryEventSource();

    [Event(1, Level = EventLevel.Informational)]
    public void FunctionCalled(string functionName, string parameters)
    {
        WriteEvent(1, functionName, parameters);
    }

    [Event(2, Level = EventLevel.Warning)]
    public void FunctionFailed(string functionName, string error)
    {
        WriteEvent(2, functionName, error);
    }
}

// Utilisation
public int Add(int a, int b)
{
    MathLibraryEventSource.Log.FunctionCalled("Add", $"a={a}, b={b}");

    try
    {
        return NativeMethods.Native_Add(a, b);
    }
    catch (Exception ex)
    {
        MathLibraryEventSource.Log.FunctionFailed("Add", ex.Message);
        throw;
    }
}
```

**Visualiser avec PerfView** :

```bash
# Télécharger PerfView de Microsoft
PerfView.exe collect
# Exécuter votre application
# Arrêter la capture
PerfView.exe analyze
```

---

## Performances et optimisations

### 1. Réduire le overhead P/Invoke

**Batch calls** : Regrouper plusieurs appels en un seul.

**❌ Lent (N appels P/Invoke)** :

```csharp
for (int i = 0; i < 1000; i++)
{
    result[i] = NativeMethods.Process(data[i]);
}
```

**✅ Rapide (1 appel P/Invoke)** :

```csharp
NativeMethods.ProcessBatch(data, result, data.Length);
```

### 2. Pinning et GC

Éviter les copies en "épinglant" les tableaux managés.

```csharp
// ❌ Copie des données
public int Sum(int[] values)
{
    return NativeMethods.SumArray(values, values.Length);
}

// ✅ Données épinglées (pas de copie)
public unsafe int Sum(int[] values)
{
    fixed (int* ptr = values)
    {
        return NativeMethods.SumArrayPointer(ptr, values.Length);
    }
}
```

**Pascal correspondant** :

```pascal
function SumArrayPointer(arr: PInteger; length: Integer): Integer; cdecl; export;
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
```

### 3. Buffers réutilisables

**Éviter les allocations répétées** :

```csharp
public class BufferPool
{
    private readonly int _bufferSize;
    private readonly Stack<byte[]> _buffers = new Stack<byte[]>();

    public BufferPool(int bufferSize)
    {
        _bufferSize = bufferSize;
    }

    public byte[] Rent()
    {
        lock (_buffers)
        {
            return _buffers.Count > 0 ? _buffers.Pop() : new byte[_bufferSize];
        }
    }

    public void Return(byte[] buffer)
    {
        lock (_buffers)
        {
            Array.Clear(buffer, 0, buffer.Length);
            _buffers.Push(buffer);
        }
    }
}

// Utilisation
private static readonly BufferPool _pool = new BufferPool(4096);

public void ProcessData()
{
    byte[] buffer = _pool.Rent();
    try
    {
        NativeMethods.FillBuffer(buffer, buffer.Length);
        // Traiter...
    }
    finally
    {
        _pool.Return(buffer);
    }
}
```

### 4. Benchmarking

**BenchmarkDotNet** pour mesurer les performances :

```csharp
using BenchmarkDotNet.Attributes;
using BenchmarkDotNet.Running;

[MemoryDiagnoser]
public class MathBenchmarks
{
    private readonly MathLib _mathLib = new MathLib();
    private readonly int[] _data = Enumerable.Range(1, 1000).ToArray();

    [Benchmark]
    public int NativeSum()
    {
        return _mathLib.Sum(_data);
    }

    [Benchmark]
    public int ManagedSum()
    {
        int sum = 0;
        for (int i = 0; i < _data.Length; i++)
            sum += _data[i];
        return sum;
    }

    [Benchmark]
    public int LinqSum()
    {
        return _data.Sum();
    }
}

class Program
{
    static void Main()
    {
        var summary = BenchmarkRunner.Run<MathBenchmarks>();
    }
}
```

**Résultats typiques** :

```
|      Method |      Mean |     Error |    StdDev | Allocated |
|------------ |----------:|----------:|----------:|----------:|
|   NativeSum | 2.547 μs  | 0.0312 μs | 0.0292 μs |         - |
|  ManagedSum | 1.203 μs  | 0.0087 μs | 0.0081 μs |         - |
|     LinqSum | 2.891 μs  | 0.0421 μs | 0.0394 μs |      40 B |
```

**Interprétation** :
- Le code managé est souvent plus rapide pour des opérations simples (overhead P/Invoke)
- Le code natif devient avantageux pour des calculs complexes ou intensifs
- Toujours mesurer avant d'optimiser !

### 5. Traitement par blocs (Chunking)

Pour de grandes quantités de données, traiter par blocs :

```csharp
public class ChunkedProcessor
{
    private const int CHUNK_SIZE = 10000;

    public long ProcessLargeDataset(int[] data)
    {
        long total = 0;
        int processed = 0;

        while (processed < data.Length)
        {
            int chunkSize = Math.Min(CHUNK_SIZE, data.Length - processed);

            // Créer un segment sans copie
            ArraySegment<int> chunk = new ArraySegment<int>(
                data, processed, chunkSize);

            // Traiter le chunk
            total += ProcessChunk(chunk.Array, chunk.Offset, chunk.Count);

            processed += chunkSize;
        }

        return total;
    }

    private int ProcessChunk(int[] data, int offset, int count)
    {
        // Traitement natif du chunk
        return NativeMethods.ProcessSegment(data, offset, count);
    }
}
```

**Pascal** :

```pascal
function ProcessSegment(arr: PInteger; offset, count: Integer): Integer;
  cdecl; export;
var
  i: Integer;
  sum: Integer;
  ptr: PInteger;
begin
  sum := 0;
  ptr := arr;
  Inc(ptr, offset);  // Avancer au début du segment

  for i := 0 to count - 1 do
  begin
    sum := sum + ptr^;
    Inc(ptr);
  end;

  Result := sum;
end;
```

---

## Cas pratiques complets

### Cas 1 : Traitement d'images haute performance

**Scénario** : Appliquer des filtres d'images complexes en Pascal, contrôlés depuis C#.

**Pascal - image_processing.pas** :

```pascal
library image_processing;

{$mode objfpc}{$H+}

uses
  SysUtils, Math;

type
  TRGB = packed record
    R, G, B: Byte;
  end;
  PRGB = ^TRGB;

// Appliquer un filtre de flou gaussien
procedure ApplyGaussianBlur(pixels: PRGB; width, height, radius: Integer);
  cdecl; export;
var
  x, y, i, j: Integer;
  sumR, sumG, sumB, count: Integer;
  temp: array of TRGB;
  src, dst: PRGB;
begin
  SetLength(temp, width * height);

  // Copier les pixels originaux
  Move(pixels^, temp[0], width * height * SizeOf(TRGB));

  // Appliquer le flou
  for y := 0 to height - 1 do
  begin
    for x := 0 to width - 1 do
    begin
      sumR := 0; sumG := 0; sumB := 0; count := 0;

      // Moyenner les pixels dans le rayon
      for j := Max(0, y - radius) to Min(height - 1, y + radius) do
      begin
        for i := Max(0, x - radius) to Min(width - 1, x + radius) do
        begin
          src := @temp[j * width + i];
          Inc(sumR, src^.R);
          Inc(sumG, src^.G);
          Inc(sumB, src^.B);
          Inc(count);
        end;
      end;

      // Écrire le pixel moyenné
      dst := pixels;
      Inc(dst, y * width + x);
      dst^.R := sumR div count;
      dst^.G := sumG div count;
      dst^.B := sumB div count;
    end;
  end;
end;

// Convertir en niveaux de gris
procedure ConvertToGrayscale(pixels: PRGB; width, height: Integer);
  cdecl; export;
var
  i, gray: Integer;
  p: PRGB;
begin
  p := pixels;
  for i := 0 to (width * height) - 1 do
  begin
    // Formule standard de luminosité
    gray := Round(0.299 * p^.R + 0.587 * p^.G + 0.114 * p^.B);
    p^.R := gray;
    p^.G := gray;
    p^.B := gray;
    Inc(p);
  end;
end;

// Ajuster la luminosité
procedure AdjustBrightness(pixels: PRGB; width, height: Integer;
  factor: Double); cdecl; export;
var
  i: Integer;
  p: PRGB;
begin
  p := pixels;
  for i := 0 to (width * height) - 1 do
  begin
    p^.R := EnsureRange(Round(p^.R * factor), 0, 255);
    p^.G := EnsureRange(Round(p^.G * factor), 0, 255);
    p^.B := EnsureRange(Round(p^.B * factor), 0, 255);
    Inc(p);
  end;
end;

exports
  ApplyGaussianBlur,
  ConvertToGrayscale,
  AdjustBrightness;

begin
end.
```

**C# - ImageProcessor.cs** :

```csharp
using System;
using System.Drawing;
using System.Drawing.Imaging;
using System.Runtime.InteropServices;

namespace ImageProcessing
{
    [StructLayout(LayoutKind.Sequential, Pack = 1)]
    public struct RGB
    {
        public byte R;
        public byte G;
        public byte B;
    }

    internal static class NativeMethods
    {
        [DllImport("image_processing.dll", CallingConvention = CallingConvention.Cdecl)]
        public static extern void ApplyGaussianBlur(
            IntPtr pixels, int width, int height, int radius);

        [DllImport("image_processing.dll", CallingConvention = CallingConvention.Cdecl)]
        public static extern void ConvertToGrayscale(
            IntPtr pixels, int width, int height);

        [DllImport("image_processing.dll", CallingConvention = CallingConvention.Cdecl)]
        public static extern void AdjustBrightness(
            IntPtr pixels, int width, int height, double factor);
    }

    public class ImageProcessor
    {
        /// <summary>
        /// Applique un filtre de flou gaussien à une image
        /// </summary>
        public static Bitmap ApplyGaussianBlur(Bitmap source, int radius)
        {
            Bitmap result = new Bitmap(source.Width, source.Height,
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
                // Copier les données source vers destination
                int bytes = Math.Abs(srcData.Stride) * source.Height;
                byte[] buffer = new byte[bytes];
                Marshal.Copy(srcData.Scan0, buffer, 0, bytes);
                Marshal.Copy(buffer, 0, dstData.Scan0, bytes);

                // Appliquer le filtre natif
                NativeMethods.ApplyGaussianBlur(
                    dstData.Scan0,
                    result.Width,
                    result.Height,
                    radius);
            }
            finally
            {
                source.UnlockBits(srcData);
                result.UnlockBits(dstData);
            }

            return result;
        }

        /// <summary>
        /// Convertit une image en niveaux de gris
        /// </summary>
        public static Bitmap ConvertToGrayscale(Bitmap source)
        {
            Bitmap result = new Bitmap(source.Width, source.Height,
                PixelFormat.Format24bppRgb);

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
                NativeMethods.ConvertToGrayscale(
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
        /// Ajuste la luminosité d'une image
        /// </summary>
        public static Bitmap AdjustBrightness(Bitmap source, double factor)
        {
            if (factor < 0 || factor > 3.0)
                throw new ArgumentException(
                    "Le facteur doit être entre 0 et 3", nameof(factor));

            Bitmap result = new Bitmap(source.Width, source.Height,
                PixelFormat.Format24bppRgb);

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
                NativeMethods.AdjustBrightness(
                    data.Scan0,
                    result.Width,
                    result.Height,
                    factor);
            }
            finally
            {
                result.UnlockBits(data);
            }

            return result;
        }
    }
}
```

**Programme de démonstration** :

```csharp
using System;
using System.Drawing;
using System.Diagnostics;
using ImageProcessing;

class Program
{
    static void Main()
    {
        Console.WriteLine("=== Traitement d'images avec interop Pascal ===\n");

        // Charger l'image
        using (Bitmap original = new Bitmap("input.jpg"))
        {
            Console.WriteLine($"Image chargée: {original.Width}x{original.Height}");

            // Test 1 : Flou gaussien
            Console.WriteLine("\n1. Application du flou gaussien...");
            Stopwatch sw = Stopwatch.StartNew();
            using (Bitmap blurred = ImageProcessor.ApplyGaussianBlur(original, 5))
            {
                sw.Stop();
                Console.WriteLine($"   Temps: {sw.ElapsedMilliseconds} ms");
                blurred.Save("output_blur.jpg");
                Console.WriteLine("   Sauvegardé: output_blur.jpg");
            }

            // Test 2 : Niveaux de gris
            Console.WriteLine("\n2. Conversion en niveaux de gris...");
            sw.Restart();
            using (Bitmap grayscale = ImageProcessor.ConvertToGrayscale(original))
            {
                sw.Stop();
                Console.WriteLine($"   Temps: {sw.ElapsedMilliseconds} ms");
                grayscale.Save("output_grayscale.jpg");
                Console.WriteLine("   Sauvegardé: output_grayscale.jpg");
            }

            // Test 3 : Ajustement luminosité
            Console.WriteLine("\n3. Augmentation de la luminosité (150%)...");
            sw.Restart();
            using (Bitmap brighter = ImageProcessor.AdjustBrightness(original, 1.5))
            {
                sw.Stop();
                Console.WriteLine($"   Temps: {sw.ElapsedMilliseconds} ms");
                brighter.Save("output_bright.jpg");
                Console.WriteLine("   Sauvegardé: output_bright.jpg");
            }

            // Test 4 : Combinaison de filtres
            Console.WriteLine("\n4. Application de filtres combinés...");
            sw.Restart();
            using (Bitmap temp1 = ImageProcessor.ApplyGaussianBlur(original, 3))
            using (Bitmap temp2 = ImageProcessor.AdjustBrightness(temp1, 1.2))
            using (Bitmap final = ImageProcessor.ConvertToGrayscale(temp2))
            {
                sw.Stop();
                Console.WriteLine($"   Temps total: {sw.ElapsedMilliseconds} ms");
                final.Save("output_combined.jpg");
                Console.WriteLine("   Sauvegardé: output_combined.jpg");
            }
        }

        Console.WriteLine("\n✓ Traitement terminé!");
        Console.WriteLine("\nAppuyez sur une touche pour quitter...");
        Console.ReadKey();
    }
}
```

---

### Cas 2 : Serveur de calcul haute performance

**Scénario** : Serveur de calcul scientifique en Pascal, API REST en C#.

**Pascal - compute_server.pas** :

```pascal
library compute_server;

{$mode objfpc}{$H+}

uses
  SysUtils, Math;

type
  TMatrix = array of array of Double;

// Multiplication matricielle
function MultiplyMatrices(a: PDouble; aRows, aCols: Integer;
                         b: PDouble; bRows, bCols: Integer;
                         result: PDouble): Boolean; cdecl; export;
var
  i, j, k: Integer;
  sum: Double;
  pA, pB, pResult: PDouble;
begin
  Result := False;

  // Vérifier les dimensions
  if aCols <> bRows then Exit;

  // Multiplication
  for i := 0 to aRows - 1 do
  begin
    for j := 0 to bCols - 1 do
    begin
      sum := 0;
      for k := 0 to aCols - 1 do
      begin
        pA := a;
        Inc(pA, i * aCols + k);

        pB := b;
        Inc(pB, k * bCols + j);

        sum := sum + pA^ * pB^;
      end;

      pResult := result;
      Inc(pResult, i * bCols + j);
      pResult^ := sum;
    end;
  end;

  Result := True;
end;

// Calcul FFT (Fast Fourier Transform) simplifié
procedure ComputeFFT(realPart, imagPart: PDouble; size: Integer);
  cdecl; export;
var
  i, j, k, m, n: Integer;
  angle, wReal, wImag, tReal, tImag: Double;
  pReal1, pReal2, pImag1, pImag2: PDouble;
begin
  // Implémentation simplifiée de la FFT Cooley-Tukey
  n := size;

  // Bit reversal
  j := 0;
  for i := 0 to n - 2 do
  begin
    if i < j then
    begin
      // Échanger réel[i] et réel[j]
      pReal1 := realPart; Inc(pReal1, i);
      pReal2 := realPart; Inc(pReal2, j);
      tReal := pReal1^;
      pReal1^ := pReal2^;
      pReal2^ := tReal;

      // Échanger imag[i] et imag[j]
      pImag1 := imagPart; Inc(pImag1, i);
      pImag2 := imagPart; Inc(pImag2, j);
      tImag := pImag1^;
      pImag1^ := pImag2^;
      pImag2^ := tImag;
    end;

    k := n div 2;
    while k <= j do
    begin
      j := j - k;
      k := k div 2;
    end;
    j := j + k;
  end;

  // FFT
  m := 1;
  while m < n do
  begin
    m := m * 2;
    angle := -2.0 * Pi / m;

    for k := 0 to m div 2 - 1 do
    begin
      wReal := Cos(k * angle);
      wImag := Sin(k * angle);

      i := k;
      while i < n do
      begin
        j := i + m div 2;

        pReal1 := realPart; Inc(pReal1, j);
        pImag1 := imagPart; Inc(pImag1, j);

        tReal := wReal * pReal1^ - wImag * pImag1^;
        tImag := wReal * pImag1^ + wImag * pReal1^;

        pReal2 := realPart; Inc(pReal2, i);
        pImag2 := imagPart; Inc(pImag2, i);

        pReal1^ := pReal2^ - tReal;
        pImag1^ := pImag2^ - tImag;
        pReal2^ := pReal2^ + tReal;
        pImag2^ := pImag2^ + tImag;

        i := i + m;
      end;
    end;
  end;
end;

// Régression linéaire
function LinearRegression(x, y: PDouble; count: Integer;
                         out slope, intercept: Double): Boolean;
  cdecl; export;
var
  i: Integer;
  sumX, sumY, sumXY, sumXX: Double;
  pX, pY: PDouble;
begin
  Result := False;
  if count < 2 then Exit;

  sumX := 0; sumY := 0; sumXY := 0; sumXX := 0;

  for i := 0 to count - 1 do
  begin
    pX := x; Inc(pX, i);
    pY := y; Inc(pY, i);

    sumX := sumX + pX^;
    sumY := sumY + pY^;
    sumXY := sumXY + (pX^ * pY^);
    sumXX := sumXX + (pX^ * pX^);
  end;

  slope := (count * sumXY - sumX * sumY) / (count * sumXX - sumX * sumX);
  intercept := (sumY - slope * sumX) / count;

  Result := True;
end;

exports
  MultiplyMatrices,
  ComputeFFT,
  LinearRegression;

begin
end.
```

**C# - API REST avec ASP.NET Core** :

```csharp
using Microsoft.AspNetCore.Mvc;
using System.Runtime.InteropServices;

namespace ComputeAPI.Controllers
{
    internal static class NativeCompute
    {
        [DllImport("compute_server.dll", CallingConvention = CallingConvention.Cdecl)]
        [return: MarshalAs(UnmanagedType.I1)]
        public static extern bool MultiplyMatrices(
            double[] a, int aRows, int aCols,
            double[] b, int bRows, int bCols,
            double[] result);

        [DllImport("compute_server.dll", CallingConvention = CallingConvention.Cdecl)]
        public static extern void ComputeFFT(
            double[] realPart, double[] imagPart, int size);

        [DllImport("compute_server.dll", CallingConvention = CallingConvention.Cdecl)]
        [return: MarshalAs(UnmanagedType.I1)]
        public static extern bool LinearRegression(
            double[] x, double[] y, int count,
            out double slope, out double intercept);
    }

    [ApiController]
    [Route("api/[controller]")]
    public class ComputeController : ControllerBase
    {
        /// <summary>
        /// Multiplie deux matrices
        /// </summary>
        [HttpPost("matrix/multiply")]
        public ActionResult<MatrixResult> MultiplyMatrices(
            [FromBody] MatrixMultiplyRequest request)
        {
            if (!ValidateMatrix(request.MatrixA) || !ValidateMatrix(request.MatrixB))
                return BadRequest("Matrices invalides");

            int aRows = request.MatrixA.Length;
            int aCols = request.MatrixA[0].Length;
            int bRows = request.MatrixB.Length;
            int bCols = request.MatrixB[0].Length;

            if (aCols != bRows)
                return BadRequest("Dimensions incompatibles pour la multiplication");

            // Aplatir les matrices
            double[] a = FlattenMatrix(request.MatrixA);
            double[] b = FlattenMatrix(request.MatrixB);
            double[] result = new double[aRows * bCols];

            // Appel natif
            bool success = NativeCompute.MultiplyMatrices(
                a, aRows, aCols, b, bRows, bCols, result);

            if (!success)
                return StatusCode(500, "Échec de la multiplication");

            return Ok(new MatrixResult
            {
                Matrix = UnflattenMatrix(result, aRows, bCols),
                Rows = aRows,
                Cols = bCols
            });
        }

        /// <summary>
        /// Calcule la FFT d'un signal
        /// </summary>
        [HttpPost("signal/fft")]
        public ActionResult<FFTResult> ComputeFFT([FromBody] FFTRequest request)
        {
            if (request.Signal == null || request.Signal.Length == 0)
                return BadRequest("Signal vide");

            // Vérifier que la taille est une puissance de 2
            if ((request.Signal.Length & (request.Signal.Length - 1)) != 0)
                return BadRequest("La taille du signal doit être une puissance de 2");

            double[] real = request.Signal.ToArray();
            double[] imag = new double[request.Signal.Length];

            // Appel natif
            NativeCompute.ComputeFFT(real, imag, request.Signal.Length);

            return Ok(new FFTResult
            {
                RealPart = real,
                ImaginaryPart = imag,
                Size = request.Signal.Length
            });
        }

        /// <summary>
        /// Calcule une régression linéaire
        /// </summary>
        [HttpPost("stats/regression")]
        public ActionResult<RegressionResult> LinearRegression(
            [FromBody] RegressionRequest request)
        {
            if (request.X == null || request.Y == null)
                return BadRequest("Données manquantes");

            if (request.X.Length != request.Y.Length)
                return BadRequest("X et Y doivent avoir la même taille");

            if (request.X.Length < 2)
                return BadRequest("Au moins 2 points sont nécessaires");

            // Appel natif
            bool success = NativeCompute.LinearRegression(
                request.X, request.Y, request.X.Length,
                out double slope, out double intercept);

            if (!success)
                return StatusCode(500, "Échec du calcul de régression");

            return Ok(new RegressionResult
            {
                Slope = slope,
                Intercept = intercept,
                Equation = $"y = {slope:F4}x + {intercept:F4}"
            });
        }

        private bool ValidateMatrix(double[][] matrix)
        {
            if (matrix == null || matrix.Length == 0) return false;
            int cols = matrix[0].Length;
            return matrix.All(row => row != null && row.Length == cols);
        }

        private double[] FlattenMatrix(double[][] matrix)
        {
            return matrix.SelectMany(row => row).ToArray();
        }

        private double[][] UnflattenMatrix(double[] flat, int rows, int cols)
        {
            double[][] result = new double[rows][];
            for (int i = 0; i < rows; i++)
            {
                result[i] = new double[cols];
                Array.Copy(flat, i * cols, result[i], 0, cols);
            }
            return result;
        }
    }

    // Modèles de données
    public class MatrixMultiplyRequest
    {
        public double[][] MatrixA { get; set; }
        public double[][] MatrixB { get; set; }
    }

    public class MatrixResult
    {
        public double[][] Matrix { get; set; }
        public int Rows { get; set; }
        public int Cols { get; set; }
    }

    public class FFTRequest
    {
        public double[] Signal { get; set; }
    }

    public class FFTResult
    {
        public double[] RealPart { get; set; }
        public double[] ImaginaryPart { get; set; }
        public int Size { get; set; }
    }

    public class RegressionRequest
    {
        public double[] X { get; set; }
        public double[] Y { get; set; }
    }

    public class RegressionResult
    {
        public double Slope { get; set; }
        public double Intercept { get; set; }
        public string Equation { get; set; }
    }
}
```

**Client de test** :

```csharp
using System;
using System.Net.Http;
using System.Net.Http.Json;
using System.Threading.Tasks;

class ComputeClient
{
    private readonly HttpClient _client;

    public ComputeClient(string baseUrl = "http://localhost:5000")
    {
        _client = new HttpClient { BaseAddress = new Uri(baseUrl) };
    }

    public async Task TestMatrixMultiplication()
    {
        Console.WriteLine("Test: Multiplication de matrices\n");

        var request = new
        {
            MatrixA = new double[][] {
                new double[] { 1, 2, 3 },
                new double[] { 4, 5, 6 }
            },
            MatrixB = new double[][] {
                new double[] { 7, 8 },
                new double[] { 9, 10 },
                new double[] { 11, 12 }
            }
        };

        var response = await _client.PostAsJsonAsync(
            "/api/compute/matrix/multiply", request);

        if (response.IsSuccessStatusCode)
        {
            var result = await response.Content.ReadFromJsonAsync<dynamic>();
            Console.WriteLine($"Résultat: {result}");
        }
    }

    public async Task TestFFT()
    {
        Console.WriteLine("\nTest: FFT\n");

        // Signal simple: somme de sinusoïdes
        double[] signal = new double[128];
        for (int i = 0; i < signal.Length; i++)
        {
            signal[i] = Math.Sin(2 * Math.PI * i / 16) +
                       0.5 * Math.Sin(2 * Math.PI * i / 8);
        }

        var request = new { Signal = signal };

        var response = await _client.PostAsJsonAsync(
            "/api/compute/signal/fft", request);

        if (response.IsSuccessStatusCode)
        {
            Console.WriteLine("FFT calculée avec succès");
            var result = await response.Content.ReadFromJsonAsync<dynamic>();
            Console.WriteLine($"Taille: {result.Size}");
        }
    }

    public async Task TestRegression()
    {
        Console.WriteLine("\nTest: Régression linéaire\n");

        var request = new
        {
            X = new double[] { 1, 2, 3, 4, 5 },
            Y = new double[] { 2, 4, 5, 4, 5 }
        };

        var response = await _client.PostAsJsonAsync(
            "/api/compute/stats/regression", request);

        if (response.IsSuccessStatusCode)
        {
            var result = await response.Content.ReadFromJsonAsync<dynamic>();
            Console.WriteLine($"Équation: {result.Equation}");
            Console.WriteLine($"Pente: {result.Slope:F4}");
            Console.WriteLine($"Ordonnée à l'origine: {result.Intercept:F4}");
        }
    }
}

class Program
{
    static async Task Main()
    {
        Console.WriteLine("=== Client API de calcul scientifique ===\n");

        var client = new ComputeClient();

        try
        {
            await client.TestMatrixMultiplication();
            await client.TestFFT();
            await client.TestRegression();

            Console.WriteLine("\n✓ Tous les tests réussis!");
        }
        catch (Exception ex)
        {
            Console.WriteLine($"\n✗ Erreur: {ex.Message}");
        }

        Console.WriteLine("\nAppuyez sur une touche pour quitter...");
        Console.ReadKey();
    }
}
```

---

## Problèmes courants et solutions

### Problème 1 : DLL Not Found

**Symptôme** :
```
System.DllNotFoundException: Unable to load DLL 'mathlib.dll'
```

**Solutions** :

1. **Vérifier l'emplacement de la DLL** :
```csharp
// Afficher le répertoire de recherche
Console.WriteLine($"Répertoire courant: {Environment.CurrentDirectory}");
Console.WriteLine($"Répertoire de l'exe: {AppDomain.CurrentDomain.BaseDirectory}");
```

2. **Spécifier le chemin complet** :
```csharp
[DllImport(@"C:\MyApp\native\mathlib.dll")]
public static extern int Add(int a, int b);
```

3. **Charger la DLL manuellement** :
```csharp
[DllImport("kernel32", SetLastError = true, CharSet = CharSet.Unicode)]
private static extern IntPtr LoadLibrary(string lpFileName);

static Program()
{
    string dllPath = Path.Combine(AppContext.BaseDirectory, "mathlib.dll");
    IntPtr handle = LoadLibrary(dllPath);

    if (handle == IntPtr.Zero)
    {
        int errorCode = Marshal.GetLastWin32Error();
        throw new DllNotFoundException(
            $"Impossible de charger {dllPath}. Code erreur: {errorCode}");
    }
}
```

4. **Ajouter au PATH** :
```csharp
string nativePath = Path.Combine(AppContext.BaseDirectory, "native");
string path = Environment.GetEnvironmentVariable("PATH");
Environment.SetEnvironmentVariable("PATH", $"{nativePath};{path}");
```

### Problème 2 : EntryPointNotFoundException

**Symptôme** :
```
System.EntryPointNotFoundException: Unable to find an entry point named 'MyFunction'
```

**Causes et solutions** :

1. **Nom de fonction incorrect** :
```bash
# Vérifier les exports avec dumpbin (Visual Studio)
dumpbin /EXPORTS mathlib.dll

# Ou avec objdump (MinGW)
objdump -p mathlib.dll | grep "Export"
```

2. **Convention d'appel incorrecte** :
```pascal
// ✓ BON
function Add(a, b: Integer): Integer; cdecl; export;

// ✗ MAUVAIS - nom décoré avec stdcall
function Add(a, b: Integer): Integer; stdcall; export;
```

3. **Spécifier le nom exact dans C#** :
```csharp
// Si la fonction s'exporte comme "MyFunc" mais vous voulez l'appeler "MyFunction"
[DllImport("lib.dll", EntryPoint = "MyFunc",
           CallingConvention = CallingConvention.Cdecl)]
public static extern int MyFunction(int x);
```

### Problème 3 : Memory Access Violation

**Symptôme** :
```
System.AccessViolationException: Attempted to read or write protected memory
```

**Causes courantes** :

1. **Tailles de structure incorrectes** :
```csharp
// Toujours vérifier avec Marshal.SizeOf
[StructLayout(LayoutKind.Sequential, Pack = 1)]
public struct MyStruct
{
    public int X;
    public double Y;
}

// Vérifier
Console.WriteLine($"Taille C#: {Marshal.SizeOf<MyStruct>()}");
// Doit correspondre à SizeOf(TMyStruct) en Pascal
```

2. **Pointeurs invalides** :
```csharp
// ✗ MAUVAIS - pointeur vers variable locale
int value = 42;
NativeMethods.ProcessPointer(ref value);
// La fonction native garde le pointeur qui devient invalide

// ✓ BON - allouer sur le tas managé
int[] value = new int[1] { 42 };
NativeMethods.ProcessArray(value, 1);
```

3. **Libération prématurée** :
```csharp
// ✗ MAUVAIS
IntPtr ptr = NativeMethods.CreateObject();
NativeMethods.FreeObject(ptr);
NativeMethods.UseObject(ptr);  // ERREUR : objet déjà libéré

// ✓ BON
IntPtr ptr = NativeMethods.CreateObject();
try
{
    NativeMethods.UseObject(ptr);
}
finally
{
    NativeMethods.FreeObject(ptr);
}
```

### Problème 4 : Chaînes corrompues

**Symptôme** : Caractères bizarres, chaînes tronquées.

**Solutions** :

1. **Spécifier l'encodage correct** :
```csharp
// ANSI (par défaut en Pascal)
[DllImport("lib.dll", CharSet = CharSet.Ansi)]
public static extern void ProcessString(string str);

// Unicode
[DllImport("lib.dll", CharSet = CharSet.Unicode)]
public static extern void ProcessWideString(string str);
```

2. **Utiliser StringBuilder pour les buffers** :
```csharp
[DllImport("lib.dll")]
public static extern void GetString(StringBuilder buffer, int size);

// Utilisation
var sb = new StringBuilder(256);
GetString(sb, sb.Capacity);
string result = sb.ToString();
```

3. **Gestion correcte en Pascal** :
```pascal
// Pour ANSI
procedure GetString(buffer: PAnsiChar; size: Integer); cdecl; export;
var
  msg: AnsiString;
begin
  msg := 'Hello World';
  if Length(msg) < size then
    StrPCopy(buffer, msg);
end;

// Pour Unicode
procedure GetWideString(buffer: PWideChar; size: Integer); cdecl; export;
var
  msg: WideString;
begin
  msg := 'Hello 世界';
  if Length(msg) < size then
    Move(msg[1], buffer^, (Length(msg) + 1) * SizeOf(WideChar));
end;
```

### Problème 5 : Fuite mémoire

**Symptôme** : La mémoire augmente continuellement.

**Solutions** :

1. **Libérer systématiquement** :
```csharp
public class ManagedWrapper : IDisposable
{
    private IntPtr _handle;

    public ManagedWrapper()
    {
        _handle = NativeMethods.Create();
    }

    public void Dispose()
    {
        if (_handle != IntPtr.Zero)
        {
            NativeMethods.Destroy(_handle);
            _handle = IntPtr.Zero;
        }
        GC.SuppressFinalize(this);
    }

    ~ManagedWrapper()
    {
        Dispose();
    }
}
```

2. **Utiliser using** :
```csharp
// ✓ BON
using (var wrapper = new ManagedWrapper())
{
    wrapper.DoWork();
} // Dispose() appelé automatiquement

// ✗ MAUVAIS
var wrapper = new ManagedWrapper();
wrapper.DoWork();
// Dispose() jamais appelé = fuite mémoire
```

3. **Profiler avec dotMemory** :
```csharp
// Identifier les fuites
for (int i = 0; i < 10000; i++)
{
    var obj = new ManagedWrapper();
    obj.DoWork();
    // Si pas de Dispose(), 10000 objets en mémoire
}
```

### Problème 6 : Threading et synchronisation

**Symptôme** : Crashes aléatoires, deadlocks, données corrompues.

**Solutions** :

1. **Protéger les ressources partagées en Pascal** :
```pascal
var
  GlobalLock: TRTLCriticalSection;
  Counter: Integer;

procedure InitLibrary; cdecl; export;
begin
  InitCriticalSection(GlobalLock);
  Counter := 0;
end;

procedure IncrementCounter; cdecl; export;
begin
  EnterCriticalSection(GlobalLock);
  try
    Inc(Counter);
  finally
    LeaveCriticalSection(GlobalLock);
  end;
end;

procedure CleanupLibrary; cdecl; export;
begin
  DoneCriticalSection(GlobalLock);
end;
```

2. **Synchroniser en C#** :
```csharp
public class ThreadSafeWrapper
{
    private static readonly object _lock = new object();

    public void ThreadSafeCall()
    {
        lock (_lock)
        {
            NativeMethods.IncrementCounter();
        }
    }

    // Ou avec SemaphoreSlim pour async
    private static readonly SemaphoreSlim _semaphore = new SemaphoreSlim(1, 1);

    public async Task ThreadSafeCallAsync()
    {
        await _semaphore.WaitAsync();
        try
        {
            NativeMethods.IncrementCounter();
        }
        finally
        {
            _semaphore.Release();
        }
    }
}
```

---

## Outils utiles

### 1. Dependency Walker

Analyse les dépendances DLL et les exports.

**Utilisation** :
```
depends.exe mathlib.dll
```

**Vérifier** :
- Les fonctions exportées sont visibles
- Les dépendances (msvcrt.dll, etc.) sont présentes
- L'architecture (x86/x64) correspond

### 2. Process Monitor (Sysinternals)

Surveille les accès fichiers, registre, réseau.

**Filtrer** :
- Process Name: votre_app.exe
- Operation: Load Image
- Path: contains .dll

**Identifier** :
- Quelles DLL sont chargées
- Où l'application cherche les DLL
- Les échecs de chargement

### 3. WinDbg

Débogueur bas niveau pour diagnostiquer les crashes.

**Commandes utiles** :
```
!analyze -v          # Analyser le crash
lm                   # Lister les modules chargés
kb                   # Stack trace
!heap -stat          # Statistiques mémoire
```

### 4. API Monitor

Capture et affiche les appels API Windows.

**Utilité** :
- Voir tous les appels à LoadLibrary
- Inspecter les paramètres des fonctions
- Tracer les allocations mémoire

### 5. dotPeek / ILSpy

Décompilateurs .NET pour inspecter les assemblies.

**Vérifier** :
- Les attributs DllImport sont corrects
- Les structures sont bien définies
- Le code généré est optimal

---

## Checklist de déploiement

### Avant de publier votre application

- [ ] **Tester sur un système propre** (sans SDK installé)
- [ ] **Inclure toutes les DLL natives** dans le package
- [ ] **Vérifier l'architecture** (x86/x64/AnyCPU)
- [ ] **Tester les droits utilisateur** (admin vs standard)
- [ ] **Vérifier les dépendances natives** (Visual C++ Runtime, etc.)
- [ ] **Ajouter un installateur** (ou xcopy deployment)
- [ ] **Documenter les prérequis système**
- [ ] **Gérer les chemins relatifs** correctement
- [ ] **Tester les mises à jour** (remplacement de DLL)
- [ ] **Vérifier la compatibilité Windows** (7/8/10/11)

### Package de distribution typique

```
MyApp/
├── MyApp.exe                    # Application .NET
├── MyApp.dll                    # Bibliothèques managées
├── mathlib.dll                  # DLL native Pascal
├── vcruntime140.dll             # Runtime C++ (si nécessaire)
├── config.json                  # Configuration
├── README.txt                   # Documentation
└── logs/                        # Dossier de logs
```

### Installer avec Inno Setup

**script.iss** :

```inno
[Setup]
AppName=My Application
AppVersion=1.0
DefaultDirName={pf}\MyApp
OutputDir=Output
OutputBaseFilename=MyAppSetup

[Files]
Source: "MyApp.exe"; DestDir: "{app}"
Source: "MyApp.dll"; DestDir: "{app}"
Source: "mathlib.dll"; DestDir: "{app}"
Source: "config.json"; DestDir: "{app}"; Flags: onlyifdoesntexist

[Icons]
Name: "{commonprograms}\My Application"; Filename: "{app}\MyApp.exe"
Name: "{commondesktop}\My Application"; Filename: "{app}\MyApp.exe"

[Run]
Filename: "{app}\MyApp.exe"; Description: "Lancer My Application"; Flags: nowait postinstall skipifsilent
```

---

## Ressources et références

### Documentation officielle

**Microsoft** :
- [Platform Invocation Services (P/Invoke)](https://docs.microsoft.com/dotnet/standard/native-interop/pinvoke)
- [Marshaling Data with Platform Invoke](https://docs.microsoft.com/dotnet/framework/interop/marshaling-data-with-platform-invoke)
- [Type Library Importer (Tlbimp.exe)](https://docs.microsoft.com/dotnet/framework/tools/tlbimp-exe-type-library-importer)

**FreePascal** :
- [FreePascal Reference Guide](https://www.freepascal.org/docs.html)
- [Creating Libraries](https://wiki.freepascal.org/Creating_dynamic_libraries)

### Livres recommandés

1. **"Essential .NET" par Don Box** - Comprendre le CLR en profondeur
2. **"Advanced .NET Debugging" par Mario Hewardt** - Techniques de débogage avancées
3. **"COM and .NET Interoperability" par Andrew Troelsen** - Interop COM détaillé

### Projets open source exemplaires

**mORMot** :
- Framework SOA/ORM pour FreePascal
- Excellent exemple d'interop .NET/Pascal
- https://github.com/synopse/mORMot2

**Lazarus IDE** :
- Code source complet d'un IDE en Pascal
- Intégration avec de nombreuses bibliothèques C
- https://gitlab.com/freepascal.org/lazarus/lazarus

### Communautés et forums

**FreePascal/Lazarus** :
- Forum officiel : https://forum.lazarus.freepascal.org
- Reddit : r/fpc, r/lazarus
- Discord : FreePascal Community

**.NET** :
- Stack Overflow : Tags [pinvoke], [interop], [marshalling]
- .NET Foundation : https://dotnetfoundation.org
- Reddit : r/dotnet, r/csharp

---

## Conclusion

### Ce que nous avons appris

1. **Trois approches d'interopérabilité** :
   - P/Invoke pour les appels simples et performants
   - COM Interop pour les objets et la bidirectionnalité
   - IPC pour l'isolation et la distribution

2. **Marshalling des données** :
   - Types primitifs, structures, tableaux
   - Chaînes ANSI et Unicode
   - Pointeurs et gestion mémoire

3. **Bonnes pratiques** :
   - Validation systématique
   - Gestion d'erreurs robuste
   - Tests unitaires complets
   - Documentation claire

4. **Optimisations** :
   - Batch calls pour réduire l'overhead
   - Pinning pour éviter les copies
   - Pooling de ressources
   - Benchmarking systématique

5. **Débogage et diagnostics** :
   - Outils système (Dependency Walker, Process Monitor)
   - Profilers (.NET et natifs)
   - Logging structuré
   - Tests sur environnements propres

### Quand utiliser l'interop .NET/Pascal ?

**✓ Utilisez l'interop quand** :
- Vous avez du code Pascal haute performance à réutiliser
- Vous développez des algorithmes intensifs nécessitant le contrôle bas niveau
- Vous intégrez des bibliothèques C/C++ existantes via Pascal
- Vous créez des plugins pour des applications .NET
- Performance critique (calcul scientifique, traitement d'images, etc.)

**✗ Évitez l'interop quand** :
- Tout peut être fait en .NET pur avec de bonnes performances
- L'overhead P/Invoke annule les gains
- La complexité n'apporte pas de valeur
- Le code doit être multiplateforme (sauf via .NET Core)

### Aller plus loin

**Prochaines étapes** :

1. **Approfondir COM Interop** :
   - Créer des serveurs COM out-of-process
   - Implémenter IDispatch pour le late binding
   - Gérer les événements COM

2. **Explorer .NET Core** :
   - Interop multiplateforme avec .NET 6+
   - Hosting du CLR dans des applications natives
   - NativeAOT pour la compilation ahead-of-time

3. **Maîtriser les performances** :
   - Profiling avancé avec ETW
   - Optimisations SIMD
   - Memory-mapped files pour les gros volumes

4. **Sécurité** :
   - Code signing des assemblies et DLL
   - Sandboxing et isolation de processus
   - Validation rigoureuse des entrées

### Mot de la fin

L'interopérabilité entre .NET et FreePascal ouvre des possibilités immenses :
- Combiner la productivité de C# avec la performance de Pascal
- Réutiliser des décennies de code existant
- Créer des applications hybrides optimales

**Rappelez-vous** :
- **Simplicité** : Commencez par P/Invoke, complexifiez seulement si nécessaire
- **Robustesse** : Gérez toujours les erreurs et la mémoire
- **Performance** : Mesurez avant d'optimiser
- **Documentation** : Documentez vos choix d'interop pour l'équipe

**Le meilleur code est celui qui fonctionne, est maintenable, et répond aux besoins.** L'interop n'est qu'un outil parmi d'autres - utilisez-le judicieusement.

---

## Annexe : Tableau récapitulatif des types

### Correspondance des types de base

| Pascal (FPC) | C# (.NET) | Taille | Notes |
|--------------|-----------|--------|-------|
| `Byte` | `byte` | 1 octet | Non signé |
| `ShortInt` | `sbyte` | 1 octet | Signé |
| `Word` | `ushort` | 2 octets | Non signé |
| `SmallInt` | `short` | 2 octets | Signé |
| `LongWord` | `uint` | 4 octets | Non signé |
| `Integer` / `LongInt` | `int` | 4 octets | Signé |
| `Int64` | `long` | 8 octets | Signé |
| `QWord` / `UInt64` | `ulong` | 8 octets | Non signé |
| `Single` | `float` | 4 octets | Flottant simple |
| `Double` | `double` | 8 octets | Flottant double |
| `Extended` | - | 10 octets | Pas d'équivalent direct |
| `Boolean` | `bool` | 1 octet | Avec `MarshalAs` |
| `Char` | `char` | 1 octet | ANSI |
| `WideChar` | `char` | 2 octets | Unicode |
| `PAnsiChar` | `string` | Pointeur | CharSet.Ansi |
| `PWideChar` | `string` | Pointeur | CharSet.Unicode |
| `Pointer` | `IntPtr` | 4/8 octets | Dépend de l'architecture |

### Attributs de marshalling courants

| Attribut | Usage | Exemple |
|----------|-------|---------|
| `[DllImport]` | Importer une fonction | `[DllImport("lib.dll")]` |
| `CallingConvention` | Convention d'appel | `CallingConvention.Cdecl` |
| `CharSet` | Encodage des chaînes | `CharSet.Ansi` |
| `EntryPoint` | Nom de la fonction | `EntryPoint = "RealName"` |
| `[StructLayout]` | Disposition de structure | `LayoutKind.Sequential` |
| `Pack` | Alignement | `Pack = 1` |
| `[MarshalAs]` | Type de marshalling | `UnmanagedType.LPStr` |
| `[return: MarshalAs]` | Type de retour | `[return: MarshalAs(...)]` |

---

**Fin du tutoriel 19.7 .NET Interop (Windows)**

Pour toute question ou suggestion d'amélioration, consultez la documentation officielle ou les forums communautaires FreePascal et .NET.

Bon développement ! 🚀

⏭️ [Mono interop (Linux)](/19-interoperabilite-bindings/08-mono-interop-linux.md)
