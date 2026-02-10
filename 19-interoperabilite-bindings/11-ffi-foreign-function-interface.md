🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 19.11 FFI (Foreign Function Interface)

## Table des matières

1. [Introduction au FFI](#1-introduction-au-ffi)
2. [Concepts fondamentaux](#2-concepts-fondamentaux)
3. [FFI dans FreePascal](#3-ffi-dans-freepascal)
4. [Appeler des fonctions C](#4-appeler-des-fonctions-c)
5. [Gestion des types](#5-gestion-des-types)
6. [Structures et pointeurs](#6-structures-et-pointeurs)
7. [Callbacks et fonction pointers](#7-callbacks-et-fonction-pointers)
8. [Chargement dynamique](#8-chargement-dynamique)
9. [FFI avancé](#9-ffi-avancé)
10. [Cas pratiques](#10-cas-pratiques)
11. [Bonnes pratiques](#11-bonnes-pratiques)
12. [Conclusion](#12-conclusion)

---

## 1. Introduction au FFI

### 1.1 Qu'est-ce que le FFI ?

**FFI** (Foreign Function Interface) est un mécanisme qui permet à un langage de programmation d'appeler des fonctions écrites dans un autre langage. C'est comme un pont entre deux mondes.

**Analogie simple** : Imaginez que vous parlez français et que vous voulez commander dans un restaurant chinois. Le FFI est comme un traducteur qui vous permet de communiquer, même si vous ne parlez pas la même langue.

### 1.2 Pourquoi utiliser le FFI ?

**Raisons principales** :

1. **Réutiliser du code existant**
   ```
   Bibliothèque C (mature, testée, performante)
                    ↓
            FFI (pont)
                    ↓
   Application FreePascal (utilise la bibliothèque)
   ```

2. **Accéder à des bibliothèques système**
   - Windows : API Win32
   - Linux : libc, GTK, Qt
   - macOS : Cocoa, Core Foundation

3. **Performance**
   - Utiliser du code optimisé en C/C++
   - Accès direct au matériel

4. **Écosystème**
   - Des milliers de bibliothèques disponibles
   - Ne pas réinventer la roue

### 1.3 FFI vs Bindings

**Quelle est la différence ?**

```
┌─────────────────────────────────────┐
│ FFI (Foreign Function Interface)    │
│                                     │
│ Le MÉCANISME général qui permet     │
│ d'appeler du code externe           │
└─────────────────────────────────────┘
                 ↓
┌─────────────────────────────────────┐
│ Bindings                            │
│                                     │
│ L'IMPLÉMENTATION concrète du FFI    │
│ pour une bibliothèque spécifique    │
└─────────────────────────────────────┘
```

**Exemple** :
- **FFI** : La capacité de FreePascal à appeler des fonctions C
- **Binding** : Le code Pascal qui déclare les fonctions de la bibliothèque SQLite

### 1.4 Architecture du FFI

```
┌───────────────────────────────────────────┐
│     Application FreePascal                │
│                                           │
│   procedure MaFonction;                   │
│   begin                                   │
│     result := fonction_c(10, 20);        │
│   end;                                    │
└─────────────────┬─────────────────────────┘
                  │
          ┌───────▼────────┐
          │  FFI Layer     │  ← Conversion des types
          │  (Système)     │     Gestion des appels
          └───────┬────────┘
                  │
          ┌───────▼────────┐
          │ Bibliothèque C │
          │                │
          │ int fonction_c │
          │ (int a, int b) │
          └────────────────┘
```

---

## 2. Concepts fondamentaux

### 2.1 Conventions d'appel

Les **conventions d'appel** définissent comment les fonctions reçoivent leurs paramètres et retournent leurs résultats.

#### cdecl (C Declaration)

**La plus courante pour C** :
- Paramètres passés sur la pile (de droite à gauche)
- L'appelant nettoie la pile
- Compatible avec la plupart des bibliothèques C

```pascal
function add(a, b: Integer): Integer; cdecl; external;
```

#### stdcall (Standard Call)

**Utilisé par l'API Windows** :
- Paramètres sur la pile (de droite à gauche)
- L'appelé nettoie la pile
- Plus efficace pour Windows

```pascal
function MessageBoxA(hWnd: HWND; lpText, lpCaption: PAnsiChar;
  uType: UINT): Integer; stdcall; external 'user32.dll';
```

#### Comparaison

| Convention | Usage principal | Nettoyage pile | Performance |
|------------|-----------------|----------------|-------------|
| **cdecl** | Bibliothèques C | Appelant | Standard |
| **stdcall** | API Windows | Appelé | Meilleure |
| **register** | Code Pascal | Registres | Optimale |

### 2.2 Linkage externe

Pour utiliser une fonction externe, on utilise le mot-clé **`external`**.

**Syntaxe générale** :
```pascal
function nom_fonction(params): TypeRetour; [convention]; external ['bibliothèque'];
```

**Exemples** :

```pascal
// Fonction dans une bibliothèque partagée
function sqrt(x: Double): Double; cdecl; external 'libm.so.6';

// Fonction Windows
function GetTickCount: DWORD; stdcall; external 'kernel32.dll';

// Fonction sans nom de bibliothèque (sera liée à la compilation)
function my_func(x: Integer): Integer; cdecl; external;
```

### 2.3 Types de données compatibles

**Types de base compatibles entre C et Pascal** :

| Type C | Type Pascal | Taille | Notes |
|--------|-------------|--------|-------|
| `char` | `AnsiChar` | 1 octet | Caractère |
| `int` | `Integer` / `LongInt` | 4 octets | Entier signé |
| `unsigned int` | `Cardinal` / `LongWord` | 4 octets | Entier non signé |
| `long` | `LongInt` | 4/8 octets | Dépend de la plateforme |
| `long long` | `Int64` | 8 octets | Grand entier |
| `float` | `Single` | 4 octets | Flottant simple |
| `double` | `Double` | 8 octets | Flottant double |
| `void*` | `Pointer` | 4/8 octets | Pointeur générique |
| `char*` | `PAnsiChar` | 4/8 octets | Chaîne C |

### 2.4 Gestion de la mémoire

**Règle d'or** : Qui alloue doit libérer !

```pascal
var
  ptr: PAnsiChar;
begin
  // ✓ BON : Allocation et libération du même côté
  ptr := c_function_that_allocates();
  try
    // Utiliser ptr
  finally
    c_function_that_frees(ptr);  // Libération côté C
  end;

  // ❌ MAUVAIS : Mélanger allocations Pascal et C
  ptr := StrAlloc(100);          // Allocation Pascal
  c_function_that_frees(ptr);    // ❌ Erreur ! Libération C
end;
```

---

## 3. FFI dans FreePascal

### 3.1 Mécanismes FFI disponibles

FreePascal offre plusieurs mécanismes pour utiliser du code externe :

#### 1. Linkage statique

Lier une bibliothèque au moment de la compilation.

```pascal
{$LINKLIB m}  // Lier libm (bibliothèque mathématique)

function sin(x: Double): Double; cdecl; external;  
function cos(x: Double): Double; cdecl; external;
```

#### 2. Linkage dynamique

Charger une bibliothèque au moment de l'exécution.

```pascal
function sqrt(x: Double): Double; cdecl; external 'libm.so.6';
```

#### 3. Chargement manuel

Charger explicitement avec `LoadLibrary`.

```pascal
uses
  DynLibs;

var
  LibHandle: TLibHandle;
  MySqrt: function(x: Double): Double; cdecl;
begin
  LibHandle := LoadLibrary('libm.so.6');
  Pointer(MySqrt) := GetProcAddress(LibHandle, 'sqrt');

  if @MySqrt <> nil then
    WriteLn(MySqrt(16.0));  // 4.0

  UnloadLibrary(LibHandle);
end;
```

### 3.2 Directives de compilation

**Directives utiles pour FFI** :

```pascal
{$LINKLIB nom}        // Lier une bibliothèque
{$L fichier.o}        // Lier un fichier objet
{$CALLING cdecl}      // Convention d'appel par défaut
{$PACKRECORDS C}      // Alignement comme en C
```

### 3.3 Premier exemple complet

**Utiliser la fonction C `strlen`** :

```pascal
program TestStrLen;

{$mode objfpc}{$H+}

// Déclarer la fonction C strlen
function strlen(s: PAnsiChar): size_t; cdecl; external 'libc.so.6';

var
  texte: AnsiString;
  longueur: size_t;
begin
  texte := 'Bonjour FreePascal !';

  // Appeler la fonction C
  longueur := strlen(PAnsiChar(texte));

  WriteLn('Texte : ', texte);
  WriteLn('Longueur : ', longueur);
end.
```

**Sortie** :
```
Texte : Bonjour FreePascal !  
Longueur : 20
```

---

## 4. Appeler des fonctions C

### 4.1 Fonctions simples

**Bibliothèque C (libmath.c)** :
```c
// Compilation : gcc -shared -fPIC -o libmath.so libmath.c

int add(int a, int b) {
    return a + b;
}

int multiply(int a, int b) {
    return a * b;
}

double divide(double a, double b) {
    if (b == 0.0) return 0.0;
    return a / b;
}
```

**Utilisation en Pascal** :

```pascal
program TestMath;

{$mode objfpc}{$H+}

// Déclarer les fonctions C
function add(a, b: Integer): Integer; cdecl; external 'libmath.so';  
function multiply(a, b: Integer): Integer; cdecl; external 'libmath.so';  
function divide(a, b: Double): Double; cdecl; external 'libmath.so';

begin
  WriteLn('10 + 5 = ', add(10, 5));
  WriteLn('10 × 5 = ', multiply(10, 5));
  WriteLn('10 ÷ 2 = ', divide(10.0, 2.0):0:2);
end.
```

### 4.2 Fonctions avec chaînes

**Bibliothèque C** :
```c
#include <string.h>
#include <ctype.h>

// Convertir en majuscules
void to_upper(char* str) {
    while (*str) {
        *str = toupper(*str);
        str++;
    }
}

// Copier une chaîne
void my_strcpy(char* dest, const char* src) {
    strcpy(dest, src);
}

// Retourner la longueur
int my_strlen(const char* str) {
    return strlen(str);
}
```

**Pascal** :

```pascal
program TestStrings;

{$mode objfpc}{$H+}

uses
  SysUtils;

procedure to_upper(s: PAnsiChar); cdecl; external 'libstring.so';  
procedure my_strcpy(dest, src: PAnsiChar); cdecl; external 'libstring.so';  
function my_strlen(s: PAnsiChar): Integer; cdecl; external 'libstring.so';

var
  buffer: array[0..255] of AnsiChar;
  texte: AnsiString;
begin
  // Test 1 : Convertir en majuscules
  texte := 'hello world';
  StrPCopy(buffer, texte);
  to_upper(buffer);
  WriteLn('Majuscules : ', buffer);

  // Test 2 : Copier une chaîne
  my_strcpy(buffer, 'FreePascal');
  WriteLn('Copie : ', buffer);

  // Test 3 : Longueur
  WriteLn('Longueur : ', my_strlen('Testing'));
end.
```

### 4.3 Fonctions avec valeurs de retour multiples

**C - Utiliser des pointeurs pour retourner plusieurs valeurs** :

```c
// Diviser avec reste
void div_mod(int dividend, int divisor, int* quotient, int* remainder) {
    *quotient = dividend / divisor;
    *remainder = dividend % divisor;
}
```

**Pascal** :

```pascal
procedure div_mod(dividend, divisor: Integer;
  quotient, remainder: PInteger); cdecl; external 'libmath.so';

var
  q, r: Integer;
begin
  div_mod(17, 5, @q, @r);
  WriteLn('17 ÷ 5 = ', q, ' reste ', r);
  // Affiche : 17 ÷ 5 = 3 reste 2
end;
```

**Alternative Pascal avec `var`** :

```pascal
// Déclarer avec var pour plus de clarté
procedure div_mod(dividend, divisor: Integer;
  var quotient, remainder: Integer); cdecl; external 'libmath.so';

var
  q, r: Integer;
begin
  div_mod(17, 5, q, r);  // Pas besoin de @
  WriteLn('17 ÷ 5 = ', q, ' reste ', r);
end;
```

---

## 5. Gestion des types

### 5.1 Correspondance des types de base

**Guide de conversion C → Pascal** :

```pascal
// Types entiers
type
  c_char = AnsiChar;       // char (1 octet)
  c_schar = ShortInt;      // signed char
  c_uchar = Byte;          // unsigned char
  c_short = SmallInt;      // short (2 octets)
  c_ushort = Word;         // unsigned short
  c_int = LongInt;         // int (4 octets)
  c_uint = LongWord;       // unsigned int
  c_long = PtrInt;         // long (4 ou 8 octets selon plateforme)
  c_ulong = PtrUInt;       // unsigned long
  c_longlong = Int64;      // long long (8 octets)
  c_ulonglong = QWord;     // unsigned long long

// Types flottants
type
  c_float = Single;        // float (4 octets)
  c_double = Double;       // double (8 octets)
  c_longdouble = Extended; // long double (10 octets)

// Pointeurs
type
  c_pointer = Pointer;     // void*
  c_string = PAnsiChar;    // char*
```

### 5.2 Types dépendants de la plateforme

**Gérer les différences 32-bit vs 64-bit** :

```pascal
type
  {$IFDEF CPU64}
  size_t = QWord;          // 8 octets en 64-bit
  ssize_t = Int64;
  {$ELSE}
  size_t = LongWord;       // 4 octets en 32-bit
  ssize_t = LongInt;
  {$ENDIF}

  // Pointeurs
  {$IFDEF CPU64}
  ptr_type = Int64;
  {$ELSE}
  ptr_type = LongInt;
  {$ENDIF}
```

### 5.3 Énumérations

**C** :
```c
enum Status {
    STATUS_OK = 0,
    STATUS_ERROR = 1,
    STATUS_PENDING = 2
};
```

**Pascal** :
```pascal
type
  TStatus = (
    STATUS_OK = 0,
    STATUS_ERROR = 1,
    STATUS_PENDING = 2
  );

// Ou plus simplement
type
  TStatus = Integer;
const
  STATUS_OK = 0;
  STATUS_ERROR = 1;
  STATUS_PENDING = 2;
```

### 5.4 Types opaques

Pour des structures C dont on ne connaît pas l'implémentation :

```c
// En C - structure opaque
typedef struct SDL_Window SDL_Window;
```

```pascal
// En Pascal - utiliser un pointeur
type
  PSDL_Window = Pointer;  // Type opaque
```

---

## 6. Structures et pointeurs

### 6.1 Structures simples

**C** :
```c
typedef struct {
    int x;
    int y;
} Point;

Point create_point(int x, int y) {
    Point p;
    p.x = x;
    p.y = y;
    return p;
}
```

**Pascal** :

```pascal
type
  Point = packed record  // packed = même alignement qu'en C
    x: Integer;
    y: Integer;
  end;

function create_point(x, y: Integer): Point; cdecl; external 'libgeom.so';

var
  p: Point;
begin
  p := create_point(10, 20);
  WriteLn('Point : (', p.x, ', ', p.y, ')');
end;
```

### 6.2 Structures avec pointeurs

**C** :
```c
typedef struct {
    char* name;
    int age;
    double salary;
} Person;

Person* create_person(const char* name, int age, double salary) {
    Person* p = malloc(sizeof(Person));
    p->name = strdup(name);
    p->age = age;
    p->salary = salary;
    return p;
}

void free_person(Person* p) {
    if (p) {
        free(p->name);
        free(p);
    }
}
```

**Pascal** :

```pascal
type
  PPerson = ^TPerson;
  TPerson = packed record
    name: PAnsiChar;
    age: Integer;
    salary: Double;
  end;

function create_person(name: PAnsiChar; age: Integer;
  salary: Double): PPerson; cdecl; external 'libperson.so';

procedure free_person(p: PPerson); cdecl; external 'libperson.so';

var
  person: PPerson;
begin
  person := create_person('Alice', 30, 50000.0);
  try
    WriteLn('Nom : ', person^.name);
    WriteLn('Âge : ', person^.age);
    WriteLn('Salaire : ', person^.salary:0:2);
  finally
    free_person(person);
  end;
end;
```

### 6.3 Alignement des structures

**Important** : L'alignement doit correspondre !

```pascal
// ❌ MAUVAIS - alignement par défaut de Pascal
type
  BadStruct = record
    a: Byte;
    b: Integer;
  end;

// ✓ BON - alignement comme en C
type
  GoodStruct = packed record
    a: Byte;
    b: Integer;
  end;

// Ou utiliser la directive
{$PACKRECORDS C}
type
  GoodStruct2 = record
    a: Byte;
    b: Integer;
  end;
{$PACKRECORDS DEFAULT}
```

### 6.4 Tableaux

**C** :
```c
void sum_array(int* arr, int length, int* result) {
    int sum = 0;
    for (int i = 0; i < length; i++) {
        sum += arr[i];
    }
    *result = sum;
}
```

**Pascal** :

```pascal
procedure sum_array(arr: PInteger; length: Integer;
  result: PInteger); cdecl; external 'libarray.so';

var
  numbers: array[0..4] of Integer = (1, 2, 3, 4, 5);
  total: Integer;
begin
  sum_array(@numbers[0], Length(numbers), @total);
  WriteLn('Somme : ', total);  // 15
end;
```

---

## 7. Callbacks et fonction pointers

### 7.1 Callbacks simples

**C** :
```c
typedef void (*callback_t)(int value);

void process_numbers(int* arr, int length, callback_t callback) {
    for (int i = 0; i < length; i++) {
        callback(arr[i]);
    }
}
```

**Pascal** :

```pascal
type
  TCallback = procedure(value: Integer); cdecl;

procedure process_numbers(arr: PInteger; length: Integer;
  callback: TCallback); cdecl; external 'libcallback.so';

// Fonction callback Pascal
procedure my_callback(value: Integer); cdecl;  
begin
  WriteLn('Valeur : ', value);
end;

var
  numbers: array[0..2] of Integer = (10, 20, 30);
begin
  process_numbers(@numbers[0], Length(numbers), @my_callback);
end;
```

### 7.2 Callbacks avec contexte

**C** :
```c
typedef void (*callback_with_context_t)(int value, void* user_data);

void process_with_context(int* arr, int length,
                         callback_with_context_t callback,
                         void* user_data) {
    for (int i = 0; i < length; i++) {
        callback(arr[i], user_data);
    }
}
```

**Pascal** :

```pascal
type
  TCallbackWithContext = procedure(value: Integer;
    user_data: Pointer); cdecl;

procedure process_with_context(arr: PInteger; length: Integer;
  callback: TCallbackWithContext; user_data: Pointer);
  cdecl; external 'libcallback.so';

type
  TMyData = record
    total: Integer;
    count: Integer;
  end;

procedure accumulate_callback(value: Integer; user_data: Pointer); cdecl;  
var
  data: ^TMyData;
begin
  data := user_data;
  Inc(data^.total, value);
  Inc(data^.count);
end;

var
  numbers: array[0..2] of Integer = (10, 20, 30);
  mydata: TMyData;
begin
  mydata.total := 0;
  mydata.count := 0;

  process_with_context(@numbers[0], Length(numbers),
    @accumulate_callback, @mydata);

  WriteLn('Total : ', mydata.total);        // 60
  WriteLn('Moyenne : ', mydata.total / mydata.count:0:2);  // 20.00
end;
```

### 7.3 Attention au Garbage Collection

**⚠️ IMPORTANT** : Garder une référence au callback !

```pascal
procedure SafeCallback;  
var
  callback: TCallback;
begin
  // ✓ BON : Garder une référence
  callback := @my_callback;
  process_numbers(@numbers[0], 3, callback);
  // callback reste valide pendant l'appel

  // ❌ MAUVAIS : Référence temporaire
  process_numbers(@numbers[0], 3, @my_callback);
  // Peut être collecté par le GC !
end;
```

---

## 8. Chargement dynamique

### 8.1 Utiliser DynLibs

**Unit DynLibs** fournit des fonctions pour charger des bibliothèques dynamiquement.

```pascal
program DynamicLoading;

{$mode objfpc}{$H+}

uses
  DynLibs, SysUtils;

type
  TSqrtFunc = function(x: Double): Double; cdecl;

var
  LibHandle: TLibHandle;
  MySqrt: TSqrtFunc;
  result: Double;
begin
  // Charger la bibliothèque
  LibHandle := LoadLibrary('libm.so.6');

  if LibHandle = NilHandle then
  begin
    WriteLn('Erreur : Impossible de charger la bibliothèque');
    Halt(1);
  end;

  try
    // Obtenir l'adresse de la fonction
    Pointer(MySqrt) := GetProcAddress(LibHandle, 'sqrt');

    if @MySqrt = nil then
    begin
      WriteLn('Erreur : Fonction sqrt non trouvée');
      Halt(1);
    end;

    // Utiliser la fonction
    result := MySqrt(16.0);
    WriteLn('sqrt(16) = ', result:0:2);

  finally
    // Décharger la bibliothèque
    UnloadLibrary(LibHandle);
  end;
end.
```

### 8.2 Wrapper pour chargement dynamique

**Créer une classe wrapper** :

```pascal
unit MathLibWrapper;

{$mode objfpc}{$H+}

interface

uses
  DynLibs, SysUtils;

type
  EMathLibException = class(Exception);

  TMathLib = class
  private
    FLibHandle: TLibHandle;
    FSqrt: function(x: Double): Double; cdecl;
    FSin: function(x: Double): Double; cdecl;
    FCos: function(x: Double): Double; cdecl;
  public
    constructor Create;
    destructor Destroy; override;

    function Sqrt(x: Double): Double;
    function Sin(x: Double): Double;
    function Cos(x: Double): Double;
  end;

implementation

constructor TMathLib.Create;  
begin
  inherited Create;

  // Charger la bibliothèque
  FLibHandle := LoadLibrary('libm.so.6');
  if FLibHandle = NilHandle then
    raise EMathLibException.Create('Impossible de charger libm.so.6');

  // Charger les fonctions
  Pointer(FSqrt) := GetProcAddress(FLibHandle, 'sqrt');
  Pointer(FSin) := GetProcAddress(FLibHandle, 'sin');
  Pointer(FCos) := GetProcAddress(FLibHandle, 'cos');

  if (@FSqrt = nil) or (@FSin = nil) or (@FCos = nil) then
    raise EMathLibException.Create('Fonctions non trouvées');
end;

destructor TMathLib.Destroy;  
begin
  if FLibHandle <> NilHandle then
    UnloadLibrary(FLibHandle);
  inherited;
end;

function TMathLib.Sqrt(x: Double): Double;  
begin
  Result := FSqrt(x);
end;

function TMathLib.Sin(x: Double): Double;  
begin
  Result := FSin(x);
end;

function TMathLib.Cos(x: Double): Double;  
begin
  Result := FCos(x);
end;

end.
```

**Utilisation** :

```pascal
program TestWrapper;

uses
  MathLibWrapper;

var
  math: TMathLib;
begin
  math := TMathLib.Create;
  try
    WriteLn('sqrt(16) = ', math.Sqrt(16.0):0:2);
    WriteLn('sin(0) = ', math.Sin(0.0):0:2);
    WriteLn('cos(0) = ', math.Cos(0.0):0:2);
  finally
    math.Free;
  end;
end.
```

### 8.3 Lazy Loading

**Charger les fonctions à la demande** :

```pascal
type
  TMathLibLazy = class
  private
    FLibHandle: TLibHandle;
    FSqrt: function(x: Double): Double; cdecl;

    function GetSqrt: function(x: Double): Double; cdecl;
  public
    constructor Create;
    destructor Destroy; override;

    property Sqrt: function(x: Double): Double cdecl read GetSqrt;
  end;

function TMathLibLazy.GetSqrt: function(x: Double): Double; cdecl;  
begin
  if @FSqrt = nil then
  begin
    if FLibHandle = NilHandle then
      FLibHandle := LoadLibrary('libm.so.6');

    if FLibHandle <> NilHandle then
      Pointer(FSqrt) := GetProcAddress(FLibHandle, 'sqrt');
  end;

  Result := FSqrt;
end;
```

---

## 9. FFI avancé

### 9.1 Gestion des erreurs C

**Bibliothèque C avec gestion d'erreurs** :

```c
#include <errno.h>
#include <string.h>

int my_operation(int value) {
    if (value < 0) {
        errno = EINVAL;  // Invalid argument
        return -1;
    }
    return value * 2;
}

const char* get_last_error() {
    return strerror(errno);
}
```

**Pascal** :

```pascal
function my_operation(value: Integer): Integer; cdecl; external 'libmylib.so';  
function get_last_error: PAnsiChar; cdecl; external 'libmylib.so';

var
  result: Integer;
begin
  result := my_operation(-5);

  if result = -1 then
  begin
    WriteLn('Erreur : ', get_last_error);
    // Affiche : Erreur : Invalid argument
  end;
end;
```

### 9.2 Structures complexes

**Structures imbriquées** :

```c
typedef struct {
    int x, y;
} Point;

typedef struct {
    Point top_left;
    Point bottom_right;
} Rectangle;

Rectangle create_rect(int x1, int y1, int x2, int y2) {
    Rectangle rect;
    rect.top_left.x = x1;
    rect.top_left.y = y1;
    rect.bottom_right.x = x2;
    rect.bottom_right.y = y2;
    return rect;
}

int rect_area(Rectangle* rect) {
    int width = rect->bottom_right.x - rect->top_left.x;
    int height = rect->bottom_right.y - rect->top_left.y;
    return width * height;
}
```

**Pascal** :

```pascal
{$PACKRECORDS C}
type
  TPoint = record
    x, y: Integer;
  end;

  TRectangle = record
    top_left: TPoint;
    bottom_right: TPoint;
  end;
  PRectangle = ^TRectangle;
{$PACKRECORDS DEFAULT}

function create_rect(x1, y1, x2, y2: Integer): TRectangle;
  cdecl; external 'libgeom.so';

function rect_area(rect: PRectangle): Integer;
  cdecl; external 'libgeom.so';

var
  rect: TRectangle;
  area: Integer;
begin
  rect := create_rect(0, 0, 10, 20);
  area := rect_area(@rect);

  WriteLn('Rectangle : (', rect.top_left.x, ',', rect.top_left.y,
          ') -> (', rect.bottom_right.x, ',', rect.bottom_right.y, ')');
  WriteLn('Aire : ', area);
  // Affiche : Rectangle : (0,0) -> (10,20)
  //           Aire : 200
end;
```

### 9.3 Unions

**C** :
```c
typedef union {
    int i;
    float f;
    char c[4];
} Value;

int get_as_int(Value v) {
    return v.i;
}

float get_as_float(Value v) {
    return v.f;
}
```

**Pascal** :

```pascal
{$PACKRECORDS C}
type
  TValue = record
    case Integer of
      0: (i: Integer);
      1: (f: Single);
      2: (c: array[0..3] of AnsiChar);
  end;
{$PACKRECORDS DEFAULT}

function get_as_int(v: TValue): Integer; cdecl; external 'libunion.so';  
function get_as_float(v: TValue): Single; cdecl; external 'libunion.so';

var
  val: TValue;
begin
  val.i := 42;
  WriteLn('Comme entier : ', get_as_int(val));

  val.f := 3.14;
  WriteLn('Comme flottant : ', get_as_float(val):0:2);
end;
```

### 9.4 Tableaux de structures

**C** :
```c
typedef struct {
    char name[50];
    int score;
} Player;

void sort_players(Player* players, int count) {
    // Tri par score décroissant
    for (int i = 0; i < count - 1; i++) {
        for (int j = i + 1; j < count; j++) {
            if (players[j].score > players[i].score) {
                Player temp = players[i];
                players[i] = players[j];
                players[j] = temp;
            }
        }
    }
}
```

**Pascal** :

```pascal
{$PACKRECORDS C}
type
  TPlayer = record
    name: array[0..49] of AnsiChar;
    score: Integer;
  end;
  PPlayer = ^TPlayer;
{$PACKRECORDS DEFAULT}

procedure sort_players(players: PPlayer; count: Integer);
  cdecl; external 'libgame.so';

var
  players: array[0..2] of TPlayer;
  i: Integer;
begin
  // Initialiser les joueurs
  StrPCopy(players[0].name, 'Alice');
  players[0].score := 100;

  StrPCopy(players[1].name, 'Bob');
  players[1].score := 150;

  StrPCopy(players[2].name, 'Charlie');
  players[2].score := 120;

  // Trier
  sort_players(@players[0], Length(players));

  // Afficher
  WriteLn('Classement :');
  for i := 0 to High(players) do
    WriteLn(i + 1, '. ', players[i].name, ' : ', players[i].score);

  // Affiche :
  // Classement :
  // 1. Bob : 150
  // 2. Charlie : 120
  // 3. Alice : 100
end;
```

### 9.5 Pointeurs de fonctions multiples

**C - Table de fonctions** :
```c
typedef int (*operation_t)(int a, int b);

typedef struct {
    operation_t add;
    operation_t subtract;
    operation_t multiply;
} Calculator;

int add(int a, int b) { return a + b; }  
int subtract(int a, int b) { return a - b; }  
int multiply(int a, int b) { return a * b; }

Calculator* create_calculator() {
    Calculator* calc = malloc(sizeof(Calculator));
    calc->add = add;
    calc->subtract = subtract;
    calc->multiply = multiply;
    return calc;
}
```

**Pascal** :

```pascal
type
  TOperation = function(a, b: Integer): Integer; cdecl;

  PCalculator = ^TCalculator;
  TCalculator = record
    add: TOperation;
    subtract: TOperation;
    multiply: TOperation;
  end;

function create_calculator: PCalculator; cdecl; external 'libcalc.so';  
procedure free_calculator(calc: PCalculator); cdecl; external 'libcalc.so';

var
  calc: PCalculator;
  result: Integer;
begin
  calc := create_calculator;
  try
    result := calc^.add(10, 5);
    WriteLn('10 + 5 = ', result);

    result := calc^.subtract(10, 5);
    WriteLn('10 - 5 = ', result);

    result := calc^.multiply(10, 5);
    WriteLn('10 × 5 = ', result);
  finally
    free_calculator(calc);
  end;
end;
```

### 9.6 Variadic Functions

**⚠️ Limitation** : FreePascal ne supporte pas directement les fonctions variadiques C (comme `printf`).

**Solution 1 : Wrapper C** :

```c
// wrapper.c
#include <stdio.h>
#include <stdarg.h>

// Wrapper non-variadique
int my_printf_int(const char* format, int value) {
    return printf(format, value);
}

int my_printf_string(const char* format, const char* value) {
    return printf(format, value);
}

int my_printf_double(const char* format, double value) {
    return printf(format, value);
}
```

**Pascal** :
```pascal
function my_printf_int(format: PAnsiChar; value: Integer): Integer;
  cdecl; external 'libwrapper.so';

function my_printf_string(format, value: PAnsiChar): Integer;
  cdecl; external 'libwrapper.so';

function my_printf_double(format: PAnsiChar; value: Double): Integer;
  cdecl; external 'libwrapper.so';

begin
  my_printf_int('Entier : %d'#10, 42);
  my_printf_string('Texte : %s'#10, 'Hello');
  my_printf_double('Flottant : %.2f'#10, 3.14159);
end;
```

**Solution 2 : Utiliser les wrappers existants** :

```pascal
uses
  CTypes;

// Wrappers typés pour printf
function printf_s(format: PAnsiChar; s: PAnsiChar): cint;
  cdecl; varargs; external 'c' name 'printf';

function printf_d(format: PAnsiChar; d: cint): cint;
  cdecl; varargs; external 'c' name 'printf';

begin
  printf_s('Nom : %s'#10, 'FreePascal');
  printf_d('Nombre : %d'#10, 42);
end;
```

---

## 10. Cas pratiques

### 10.1 Wrapper pour SQLite

**Wrapper complet pour SQLite3** :

```pascal
unit SQLiteWrapper;

{$mode objfpc}{$H+}

interface

uses
  DynLibs, SysUtils;

type
  PSQLite3 = Pointer;
  PSQLite3_Stmt = Pointer;

  ESQLiteException = class(Exception);

  TSQLite = class
  private
    FLibHandle: TLibHandle;
    FDB: PSQLite3;

    // Pointeurs de fonctions
    Fsqlite3_open: function(filename: PAnsiChar;
      var db: PSQLite3): Integer; cdecl;
    Fsqlite3_close: function(db: PSQLite3): Integer; cdecl;
    Fsqlite3_exec: function(db: PSQLite3; sql: PAnsiChar;
      callback: Pointer; arg: Pointer;
      errmsg: PPAnsiChar): Integer; cdecl;
    Fsqlite3_errmsg: function(db: PSQLite3): PAnsiChar; cdecl;
    Fsqlite3_prepare_v2: function(db: PSQLite3; sql: PAnsiChar;
      nByte: Integer; var stmt: PSQLite3_Stmt;
      var tail: PAnsiChar): Integer; cdecl;
    Fsqlite3_step: function(stmt: PSQLite3_Stmt): Integer; cdecl;
    Fsqlite3_finalize: function(stmt: PSQLite3_Stmt): Integer; cdecl;
    Fsqlite3_column_int: function(stmt: PSQLite3_Stmt;
      col: Integer): Integer; cdecl;
    Fsqlite3_column_text: function(stmt: PSQLite3_Stmt;
      col: Integer): PAnsiChar; cdecl;

    procedure LoadFunctions;
    procedure CheckError(ErrorCode: Integer; const Msg: string);
  public
    constructor Create(const DatabaseFile: string);
    destructor Destroy; override;

    procedure Execute(const SQL: string);
    function Query(const SQL: string): TStringList;
  end;

implementation

const
  SQLITE_OK = 0;
  SQLITE_ROW = 100;
  SQLITE_DONE = 101;

constructor TSQLite.Create(const DatabaseFile: string);  
var
  ret: Integer;
begin
  inherited Create;

  // Charger la bibliothèque
  {$IFDEF WINDOWS}
  FLibHandle := LoadLibrary('sqlite3.dll');
  {$ELSE}
  FLibHandle := LoadLibrary('libsqlite3.so.0');
  {$ENDIF}

  if FLibHandle = NilHandle then
    raise ESQLiteException.Create('Impossible de charger SQLite3');

  try
    LoadFunctions;

    // Ouvrir la base de données
    ret := Fsqlite3_open(PAnsiChar(AnsiString(DatabaseFile)), FDB);
    CheckError(ret, 'Erreur lors de l''ouverture de la base');
  except
    UnloadLibrary(FLibHandle);
    raise;
  end;
end;

destructor TSQLite.Destroy;  
begin
  if FDB <> nil then
    Fsqlite3_close(FDB);

  if FLibHandle <> NilHandle then
    UnloadLibrary(FLibHandle);

  inherited;
end;

procedure TSQLite.LoadFunctions;  
begin
  Pointer(Fsqlite3_open) := GetProcAddress(FLibHandle, 'sqlite3_open');
  Pointer(Fsqlite3_close) := GetProcAddress(FLibHandle, 'sqlite3_close');
  Pointer(Fsqlite3_exec) := GetProcAddress(FLibHandle, 'sqlite3_exec');
  Pointer(Fsqlite3_errmsg) := GetProcAddress(FLibHandle, 'sqlite3_errmsg');
  Pointer(Fsqlite3_prepare_v2) := GetProcAddress(FLibHandle, 'sqlite3_prepare_v2');
  Pointer(Fsqlite3_step) := GetProcAddress(FLibHandle, 'sqlite3_step');
  Pointer(Fsqlite3_finalize) := GetProcAddress(FLibHandle, 'sqlite3_finalize');
  Pointer(Fsqlite3_column_int) := GetProcAddress(FLibHandle, 'sqlite3_column_int');
  Pointer(Fsqlite3_column_text) := GetProcAddress(FLibHandle, 'sqlite3_column_text');

  if (@Fsqlite3_open = nil) or (@Fsqlite3_close = nil) then
    raise ESQLiteException.Create('Fonctions SQLite non trouvées');
end;

procedure TSQLite.CheckError(ErrorCode: Integer; const Msg: string);  
begin
  if ErrorCode <> SQLITE_OK then
    raise ESQLiteException.CreateFmt('%s: %s',
      [Msg, string(Fsqlite3_errmsg(FDB))]);
end;

procedure TSQLite.Execute(const SQL: string);  
var
  ret: Integer;
begin
  ret := Fsqlite3_exec(FDB, PAnsiChar(AnsiString(SQL)), nil, nil, nil);
  CheckError(ret, 'Erreur lors de l''exécution');
end;

function TSQLite.Query(const SQL: string): TStringList;  
var
  stmt: PSQLite3_Stmt;
  ret: Integer;
  tail: PAnsiChar;
  id: Integer;
  name: string;
begin
  Result := TStringList.Create;

  ret := Fsqlite3_prepare_v2(FDB, PAnsiChar(AnsiString(SQL)),
    -1, stmt, tail);
  CheckError(ret, 'Erreur lors de la préparation');

  try
    while Fsqlite3_step(stmt) = SQLITE_ROW do
    begin
      id := Fsqlite3_column_int(stmt, 0);
      name := string(Fsqlite3_column_text(stmt, 1));
      Result.Add(Format('%d: %s', [id, name]));
    end;
  finally
    Fsqlite3_finalize(stmt);
  end;
end;

end.
```

**Utilisation** :

```pascal
program TestSQLite;

uses
  SQLiteWrapper, SysUtils;

var
  db: TSQLite;
  results: TStringList;
  i: Integer;
begin
  db := TSQLite.Create('test.db');
  try
    // Créer une table
    db.Execute('CREATE TABLE IF NOT EXISTS users (id INTEGER PRIMARY KEY, name TEXT)');

    // Insérer des données
    db.Execute('INSERT INTO users (name) VALUES (''Alice'')');
    db.Execute('INSERT INTO users (name) VALUES (''Bob'')');

    // Requête
    results := db.Query('SELECT id, name FROM users');
    try
      WriteLn('Utilisateurs :');
      for i := 0 to results.Count - 1 do
        WriteLn('  ', results[i]);
    finally
      results.Free;
    end;
  finally
    db.Free;
  end;
end.
```

### 10.2 Wrapper pour libcurl

**HTTP client avec libcurl** :

```pascal
unit CurlWrapper;

{$mode objfpc}{$H+}

interface

uses
  DynLibs, SysUtils, Classes;

type
  PCURL = Pointer;

  TCurlWriteCallback = function(buffer: PAnsiChar; size, nitems: size_t;
    userdata: Pointer): size_t; cdecl;

  TCurl = class
  private
    FLibHandle: TLibHandle;
    FCurl: PCURL;

    Fcurl_easy_init: function: PCURL; cdecl;
    Fcurl_easy_cleanup: procedure(curl: PCURL); cdecl;
    Fcurl_easy_setopt: function(curl: PCURL; option: Integer;
      param: Pointer): Integer; cdecl;
    Fcurl_easy_perform: function(curl: PCURL): Integer; cdecl;

    procedure LoadFunctions;
  public
    constructor Create;
    destructor Destroy; override;

    function Get(const URL: string): string;
  end;

implementation

const
  CURLOPT_URL = 10002;
  CURLOPT_WRITEFUNCTION = 20011;
  CURLOPT_WRITEDATA = 10001;

function WriteCallback(buffer: PAnsiChar; size, nitems: size_t;
  userdata: Pointer): size_t; cdecl;
var
  stream: TMemoryStream;
begin
  stream := TMemoryStream(userdata);
  Result := size * nitems;
  stream.Write(buffer^, Result);
end;

constructor TCurl.Create;  
begin
  inherited Create;

  {$IFDEF WINDOWS}
  FLibHandle := LoadLibrary('libcurl.dll');
  {$ELSE}
  FLibHandle := LoadLibrary('libcurl.so.4');
  {$ENDIF}

  if FLibHandle = NilHandle then
    raise Exception.Create('Impossible de charger libcurl');

  LoadFunctions;

  FCurl := Fcurl_easy_init;
  if FCurl = nil then
    raise Exception.Create('Impossible d''initialiser CURL');
end;

destructor TCurl.Destroy;  
begin
  if FCurl <> nil then
    Fcurl_easy_cleanup(FCurl);

  if FLibHandle <> NilHandle then
    UnloadLibrary(FLibHandle);

  inherited;
end;

procedure TCurl.LoadFunctions;  
begin
  Pointer(Fcurl_easy_init) := GetProcAddress(FLibHandle, 'curl_easy_init');
  Pointer(Fcurl_easy_cleanup) := GetProcAddress(FLibHandle, 'curl_easy_cleanup');
  Pointer(Fcurl_easy_setopt) := GetProcAddress(FLibHandle, 'curl_easy_setopt');
  Pointer(Fcurl_easy_perform) := GetProcAddress(FLibHandle, 'curl_easy_perform');
end;

function TCurl.Get(const URL: string): string;  
var
  stream: TMemoryStream;
  urlStr: AnsiString;
begin
  stream := TMemoryStream.Create;
  try
    urlStr := AnsiString(URL);

    Fcurl_easy_setopt(FCurl, CURLOPT_URL, PAnsiChar(urlStr));
    Fcurl_easy_setopt(FCurl, CURLOPT_WRITEFUNCTION, @WriteCallback);
    Fcurl_easy_setopt(FCurl, CURLOPT_WRITEDATA, stream);

    if Fcurl_easy_perform(FCurl) = 0 then
    begin
      stream.Position := 0;
      SetLength(Result, stream.Size);
      stream.Read(Result[1], stream.Size);
    end
    else
      Result := '';
  finally
    stream.Free;
  end;
end;

end.
```

### 10.3 Intégration avec libgit2

**Wrapper simplifié pour Git** :

```pascal
unit GitWrapper;

{$mode objfpc}{$H+}

interface

uses
  DynLibs, SysUtils;

type
  Pgit_repository = Pointer;
  Pgit_reference = Pointer;

  TGit = class
  private
    FLibHandle: TLibHandle;

    Fgit_libgit2_init: function: Integer; cdecl;
    Fgit_libgit2_shutdown: function: Integer; cdecl;
    Fgit_repository_open: function(out repo: Pgit_repository;
      path: PAnsiChar): Integer; cdecl;
    Fgit_repository_free: procedure(repo: Pgit_repository); cdecl;
    Fgit_repository_head: function(out ref: Pgit_reference;
      repo: Pgit_repository): Integer; cdecl;
    Fgit_reference_name: function(ref: Pgit_reference): PAnsiChar; cdecl;
    Fgit_reference_free: procedure(ref: Pgit_reference); cdecl;

    procedure LoadFunctions;
  public
    constructor Create;
    destructor Destroy; override;

    function GetCurrentBranch(const RepoPath: string): string;
  end;

implementation

constructor TGit.Create;  
begin
  inherited Create;

  {$IFDEF WINDOWS}
  FLibHandle := LoadLibrary('git2.dll');
  {$ELSE}
  FLibHandle := LoadLibrary('libgit2.so.1.1');
  {$ENDIF}

  if FLibHandle = NilHandle then
    raise Exception.Create('Impossible de charger libgit2');

  LoadFunctions;
  Fgit_libgit2_init;
end;

destructor TGit.Destroy;  
begin
  if FLibHandle <> NilHandle then
  begin
    Fgit_libgit2_shutdown;
    UnloadLibrary(FLibHandle);
  end;

  inherited;
end;

procedure TGit.LoadFunctions;  
begin
  Pointer(Fgit_libgit2_init) := GetProcAddress(FLibHandle, 'git_libgit2_init');
  Pointer(Fgit_libgit2_shutdown) := GetProcAddress(FLibHandle, 'git_libgit2_shutdown');
  Pointer(Fgit_repository_open) := GetProcAddress(FLibHandle, 'git_repository_open');
  Pointer(Fgit_repository_free) := GetProcAddress(FLibHandle, 'git_repository_free');
  Pointer(Fgit_repository_head) := GetProcAddress(FLibHandle, 'git_repository_head');
  Pointer(Fgit_reference_name) := GetProcAddress(FLibHandle, 'git_reference_name');
  Pointer(Fgit_reference_free) := GetProcAddress(FLibHandle, 'git_reference_free');
end;

function TGit.GetCurrentBranch(const RepoPath: string): string;  
var
  repo: Pgit_repository;
  ref: Pgit_reference;
  ret: Integer;
begin
  Result := '';

  ret := Fgit_repository_open(repo, PAnsiChar(AnsiString(RepoPath)));
  if ret <> 0 then
    Exit;

  try
    ret := Fgit_repository_head(ref, repo);
    if ret = 0 then
    begin
      try
        Result := string(Fgit_reference_name(ref));
      finally
        Fgit_reference_free(ref);
      end;
    end;
  finally
    Fgit_repository_free(repo);
  end;
end;

end.
```

---

## 11. Bonnes pratiques

### 11.1 Gestion des erreurs

**✓ Toujours vérifier les codes de retour** :

```pascal
function SafeOperation: Boolean;  
var
  ret: Integer;
begin
  Result := False;

  ret := c_function();

  if ret < 0 then
  begin
    WriteLn('Erreur : code ', ret);
    Exit;
  end;

  Result := True;
end;
```

**✓ Utiliser try-finally** :

```pascal
var
  handle: Pointer;
begin
  handle := c_create();
  if handle = nil then
    raise Exception.Create('Création échouée');

  try
    // Utiliser handle
  finally
    c_destroy(handle);
  end;
end;
```

### 11.2 Gestion de la mémoire

**✓ Respecter les responsabilités** :

```pascal
// Règle 1 : Qui alloue doit libérer
var
  ptr: PAnsiChar;
begin
  // Pascal alloue → Pascal libère
  ptr := StrAlloc(100);
  try
    // utiliser ptr
  finally
    StrDispose(ptr);
  end;

  // C alloue → C libère
  ptr := c_allocate_string();
  try
    // utiliser ptr
  finally
    c_free_string(ptr);
  end;
end;
```

**✓ Éviter les fuites** :

```pascal
type
  TResourceWrapper = class
  private
    FHandle: Pointer;
  public
    constructor Create;
    destructor Destroy; override;
  end;

constructor TResourceWrapper.Create;  
begin
  inherited;
  FHandle := c_create_resource();
  if FHandle = nil then
    raise Exception.Create('Création échouée');
end;

destructor TResourceWrapper.Destroy;  
begin
  if FHandle <> nil then
    c_destroy_resource(FHandle);
  inherited;
end;
```

### 11.3 Thread-safety

**✓ Vérifier la documentation de la bibliothèque** :

```pascal
// Exemple avec une bibliothèque thread-safe
type
  TThreadSafeWrapper = class
  private
    FHandle: Pointer;
    FLock: TRTLCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SafeOperation;
  end;

procedure TThreadSafeWrapper.SafeOperation;  
begin
  EnterCriticalSection(FLock);
  try
    c_operation(FHandle);
  finally
    LeaveCriticalSection(FLock);
  end;
end;
```

### 11.4 Documentation

**✓ Documenter les déclarations externes** :

```pascal
/// <summary>
/// Calcule la racine carrée d'un nombre
/// </summary>
/// <param name="x">Nombre dont on veut la racine carrée (doit être >= 0)</param>
/// <returns>La racine carrée de x</returns>
/// <remarks>
/// Bibliothèque : libm.so.6 (Linux) / msvcrt.dll (Windows)
/// Thread-safe : Oui
/// </remarks>
function sqrt(x: Double): Double; cdecl; external 'libm.so.6';
```

### 11.5 Tests

**✓ Tester les cas limites** :

```pascal
procedure TestFFI;  
begin
  // Test normal
  Assert(Abs(MySqrt(16.0) - 4.0) < 0.001);

  // Test zéro
  Assert(Abs(MySqrt(0.0)) < 0.001);

  // Test valeurs négatives (doit gérer l'erreur)
  try
    MySqrt(-1.0);
    Fail('Devrait lever une exception');
  except
    on E: Exception do
      WriteLn('OK : erreur détectée');
  end;
end;
```

### 11.6 Portabilité Windows/Linux

**✓ Utiliser des directives conditionnelles** :

```pascal
{$IFDEF WINDOWS}
const
  MATH_LIB = 'msvcrt.dll';
{$ELSE}
const
  MATH_LIB = 'libm.so.6';
{$ENDIF}

function sqrt(x: Double): Double; cdecl; external MATH_LIB;
```

**✓ Wrapper multi-plateforme** :

```pascal
unit MathLibMultiplatform;

interface

function MySqrt(x: Double): Double;

implementation

{$IFDEF WINDOWS}
function sqrt(x: Double): Double; cdecl; external 'msvcrt.dll';
{$ELSE}
function sqrt(x: Double): Double; cdecl; external 'libm.so.6';
{$ENDIF}

function MySqrt(x: Double): Double;  
begin
  Result := sqrt(x);
end;

end.
```

---

## 12. Conclusion

### 12.1 Récapitulatif

Le **FFI** (Foreign Function Interface) permet à FreePascal de :

1. ✅ **Réutiliser des bibliothèques existantes** (C/C++, système)
2. ✅ **Accéder aux API natives** (Windows API, libc, GTK, Qt)
3. ✅ **Optimiser les performances** (code C optimisé)
4. ✅ **Étendre les capacités** (matériel, protocoles, formats)

### 12.2 Points clés à retenir

| Aspect | Conseil |
|--------|---------|
| **Conventions d'appel** | Utiliser `cdecl` pour C, `stdcall` pour Windows API |
| **Types** | Vérifier la correspondance exacte (taille, alignement) |
| **Structures** | Utiliser `{$PACKRECORDS C}` ou `packed record` |
| **Mémoire** | Qui alloue doit libérer (côté Pascal ou côté C) |
| **Erreurs** | Toujours vérifier les codes de retour |
| **Portabilité** | Utiliser des directives `{$IFDEF}` pour Windows/Linux |

### 12.3 Ressources utiles

**Documentation** :
- FreePascal Wiki : https://wiki.freepascal.org/
- FPC Reference Guide : Section "External Libraries"
- Lazarus CCR : https://lazarus-ccr.sourceforge.io/

**Exemples de bindings** :
- SDL2 : https://github.com/PascalGameDevelopment/SDL2-for-Pascal
- OpenGL : https://github.com/daar/pasgltf
- cURL : https://github.com/curl/curl
- SQLite : Inclus dans Lazarus (sqldb)

**Outils** :
- h2pas : Convertisseur automatique de headers C vers Pascal
- c2pas : Alternative plus moderne
- SWIG : Générateur de bindings multi-langages

### 12.4 Exercices pratiques

#### Exercice 1 : Wrapper basique

**Objectif** : Créer un wrapper pour une bibliothèque mathématique simple.

**Bibliothèque C (libexercice.c)** :
```c
// Compiler : gcc -shared -fPIC -o libexercice.so libexercice.c

double power(double base, double exponent) {
    double result = 1.0;
    for (int i = 0; i < (int)exponent; i++) {
        result *= base;
    }
    return result;
}

int factorial(int n) {
    if (n <= 1) return 1;
    return n * factorial(n - 1);
}

int is_prime(int n) {
    if (n < 2) return 0;
    for (int i = 2; i * i <= n; i++) {
        if (n % i == 0) return 0;
    }
    return 1;
}
```

**À faire** :
1. Déclarer les fonctions externes en Pascal
2. Créer une unit wrapper avec gestion d'erreurs
3. Écrire un programme de test

#### Exercice 2 : Structures et callbacks

**Objectif** : Manipuler des structures complexes et utiliser des callbacks.

**Bibliothèque C** :
```c
typedef struct {
    char name[50];
    int age;
    double salary;
} Employee;

typedef void (*employee_callback)(Employee* emp, void* userdata);

void foreach_employee(Employee* employees, int count,
                     employee_callback callback, void* userdata) {
    for (int i = 0; i < count; i++) {
        callback(&employees[i], userdata);
    }
}

double average_salary(Employee* employees, int count) {
    double total = 0.0;
    for (int i = 0; i < count; i++) {
        total += employees[i].salary;
    }
    return total / count;
}
```

**À faire** :
1. Définir la structure `TEmployee` en Pascal
2. Implémenter un callback pour afficher les employés
3. Calculer le salaire moyen avec un callback

#### Exercice 3 : Chargement dynamique

**Objectif** : Créer un système de plugins dynamiques.

**Plugin C (plugin.c)** :
```c
typedef struct {
    const char* name;
    const char* version;
    int (*execute)(const char* input);
} Plugin;

int my_plugin_execute(const char* input) {
    // Traitement
    return strlen(input);
}

Plugin* get_plugin() {
    static Plugin p = {
        .name = "MyPlugin",
        .version = "1.0",
        .execute = my_plugin_execute
    };
    return &p;
}
```

**À faire** :
1. Charger dynamiquement le plugin
2. Créer une interface commune pour tous les plugins
3. Gérer plusieurs plugins simultanément

### 12.5 Projet complet : Wrapper JSON (simdjson)

**Créer un wrapper complet pour une bibliothèque JSON haute performance.**

```pascal
unit SimdJsonWrapper;

{$mode objfpc}{$H+}

interface

uses
  DynLibs, SysUtils, Classes;

type
  PJsonDocument = Pointer;
  PJsonElement = Pointer;

  EJsonException = class(Exception);

  TJsonWrapper = class
  private
    FLibHandle: TLibHandle;

    // Pointeurs de fonctions
    Fcreate_parser: function: Pointer; cdecl;
    Fdestroy_parser: procedure(parser: Pointer); cdecl;
    Fparse_string: function(parser: Pointer; json: PAnsiChar;
      len: size_t; out doc: PJsonDocument): Integer; cdecl;
    Fget_type: function(elem: PJsonElement): Integer; cdecl;
    Fget_string: function(elem: PJsonElement;
      out str: PAnsiChar): Integer; cdecl;
    Fget_int: function(elem: PJsonElement;
      out value: Int64): Integer; cdecl;
    Fget_double: function(elem: PJsonElement;
      out value: Double): Integer; cdecl;
    Fget_bool: function(elem: PJsonElement;
      out value: Boolean): Integer; cdecl;
    Fget_object_field: function(elem: PJsonElement; key: PAnsiChar;
      out field: PJsonElement): Integer; cdecl;
    Fget_array_element: function(elem: PJsonElement; index: size_t;
      out element: PJsonElement): Integer; cdecl;

    FParser: Pointer;

    procedure LoadFunctions;
  public
    constructor Create;
    destructor Destroy; override;

    function Parse(const JsonString: string): PJsonDocument;
    function GetString(elem: PJsonElement): string;
    function GetInt(elem: PJsonElement): Int64;
    function GetDouble(elem: PJsonElement): Double;
    function GetBool(elem: PJsonElement): Boolean;
    function GetField(elem: PJsonElement; const Key: string): PJsonElement;
    function GetArrayElement(elem: PJsonElement; Index: Integer): PJsonElement;
  end;

implementation

const
  JSON_TYPE_OBJECT = 0;
  JSON_TYPE_ARRAY = 1;
  JSON_TYPE_STRING = 2;
  JSON_TYPE_NUMBER = 3;
  JSON_TYPE_BOOL = 4;
  JSON_TYPE_NULL = 5;

constructor TJsonWrapper.Create;  
begin
  inherited Create;

  {$IFDEF WINDOWS}
  FLibHandle := LoadLibrary('simdjson.dll');
  {$ELSE}
  FLibHandle := LoadLibrary('libsimdjson.so');
  {$ENDIF}

  if FLibHandle = NilHandle then
    raise EJsonException.Create('Impossible de charger simdjson');

  try
    LoadFunctions;
    FParser := Fcreate_parser;
    if FParser = nil then
      raise EJsonException.Create('Impossible de créer le parser');
  except
    UnloadLibrary(FLibHandle);
    raise;
  end;
end;

destructor TJsonWrapper.Destroy;  
begin
  if FParser <> nil then
    Fdestroy_parser(FParser);

  if FLibHandle <> NilHandle then
    UnloadLibrary(FLibHandle);

  inherited;
end;

procedure TJsonWrapper.LoadFunctions;  
begin
  Pointer(Fcreate_parser) := GetProcAddress(FLibHandle, 'simdjson_create_parser');
  Pointer(Fdestroy_parser) := GetProcAddress(FLibHandle, 'simdjson_destroy_parser');
  Pointer(Fparse_string) := GetProcAddress(FLibHandle, 'simdjson_parse_string');
  Pointer(Fget_type) := GetProcAddress(FLibHandle, 'simdjson_get_type');
  Pointer(Fget_string) := GetProcAddress(FLibHandle, 'simdjson_get_string');
  Pointer(Fget_int) := GetProcAddress(FLibHandle, 'simdjson_get_int');
  Pointer(Fget_double) := GetProcAddress(FLibHandle, 'simdjson_get_double');
  Pointer(Fget_bool) := GetProcAddress(FLibHandle, 'simdjson_get_bool');
  Pointer(Fget_object_field) := GetProcAddress(FLibHandle, 'simdjson_get_field');
  Pointer(Fget_array_element) := GetProcAddress(FLibHandle, 'simdjson_get_array_element');

  if @Fcreate_parser = nil then
    raise EJsonException.Create('Fonctions simdjson non trouvées');
end;

function TJsonWrapper.Parse(const JsonString: string): PJsonDocument;  
var
  doc: PJsonDocument;
  ret: Integer;
  jsonStr: AnsiString;
begin
  jsonStr := AnsiString(JsonString);
  ret := Fparse_string(FParser, PAnsiChar(jsonStr), Length(jsonStr), doc);

  if ret <> 0 then
    raise EJsonException.Create('Erreur de parsing JSON');

  Result := doc;
end;

function TJsonWrapper.GetString(elem: PJsonElement): string;  
var
  str: PAnsiChar;
  ret: Integer;
begin
  ret := Fget_string(elem, str);
  if ret = 0 then
    Result := string(str)
  else
    raise EJsonException.Create('Pas une chaîne');
end;

function TJsonWrapper.GetInt(elem: PJsonElement): Int64;  
var
  value: Int64;
  ret: Integer;
begin
  ret := Fget_int(elem, value);
  if ret = 0 then
    Result := value
  else
    raise EJsonException.Create('Pas un entier');
end;

function TJsonWrapper.GetDouble(elem: PJsonElement): Double;  
var
  value: Double;
  ret: Integer;
begin
  ret := Fget_double(elem, value);
  if ret = 0 then
    Result := value
  else
    raise EJsonException.Create('Pas un nombre');
end;

function TJsonWrapper.GetBool(elem: PJsonElement): Boolean;  
var
  value: Boolean;
  ret: Integer;
begin
  ret := Fget_bool(elem, value);
  if ret = 0 then
    Result := value
  else
    raise EJsonException.Create('Pas un booléen');
end;

function TJsonWrapper.GetField(elem: PJsonElement; const Key: string): PJsonElement;  
var
  field: PJsonElement;
  ret: Integer;
  keyStr: AnsiString;
begin
  keyStr := AnsiString(Key);
  ret := Fget_object_field(elem, PAnsiChar(keyStr), field);

  if ret = 0 then
    Result := field
  else
    raise EJsonException.CreateFmt('Champ "%s" non trouvé', [Key]);
end;

function TJsonWrapper.GetArrayElement(elem: PJsonElement; Index: Integer): PJsonElement;  
var
  element: PJsonElement;
  ret: Integer;
begin
  ret := Fget_array_element(elem, Index, element);

  if ret = 0 then
    Result := element
  else
    raise EJsonException.CreateFmt('Index %d hors limites', [Index]);
end;

end.
```

**Programme de test** :

```pascal
program TestJson;

{$mode objfpc}{$H+}

uses
  SimdJsonWrapper, SysUtils;

var
  json: TJsonWrapper;
  doc: PJsonDocument;
  nameField, ageField: PJsonElement;
begin
  json := TJsonWrapper.Create;
  try
    // Parser JSON
    doc := json.Parse('{"name": "Alice", "age": 30, "active": true}');

    // Extraire les champs
    nameField := json.GetField(doc, 'name');
    WriteLn('Nom : ', json.GetString(nameField));

    ageField := json.GetField(doc, 'age');
    WriteLn('Âge : ', json.GetInt(ageField));

  finally
    json.Free;
  end;
end.
```

### 12.6 Débogage FFI

#### Techniques de débogage

**1. Vérifier que la bibliothèque est chargée** :

```pascal
uses
  DynLibs;

procedure CheckLibrary;  
var
  handle: TLibHandle;
begin
  handle := LoadLibrary('libtest.so');

  if handle = NilHandle then
    WriteLn('❌ Bibliothèque non trouvée')
  else
  begin
    WriteLn('✓ Bibliothèque chargée');
    UnloadLibrary(handle);
  end;
end;
```

**2. Tracer les appels FFI** :

```pascal
function TracedCall(const FuncName: string; x: Double): Double;  
begin
  WriteLn('[FFI] Appel de ', FuncName, ' avec x=', x:0:2);
  Result := c_function(x);
  WriteLn('[FFI] Retour : ', Result:0:2);
end;
```

**3. Utiliser Valgrind (Linux)** :

```bash
# Détecter les fuites mémoire
valgrind --leak-check=full ./mon_programme

# Détecter les erreurs d'accès mémoire
valgrind --tool=memcheck ./mon_programme
```

**4. Utiliser strace/ltrace (Linux)** :

```bash
# Tracer les appels système
strace ./mon_programme

# Tracer les appels aux bibliothèques
ltrace ./mon_programme
```

**5. Logs détaillés** :

```pascal
unit FFILogger;

interface

procedure LogFFICall(const Func: string; const Params: array of string);  
procedure LogFFIReturn(const Func: string; const ReturnValue: string);  
procedure LogFFIError(const Func, Error: string);

implementation

uses
  SysUtils;

var
  LogFile: TextFile;

procedure LogFFICall(const Func: string; const Params: array of string);  
var
  i: Integer;
  paramStr: string;
begin
  paramStr := '';
  for i := 0 to High(Params) do
  begin
    if i > 0 then paramStr := paramStr + ', ';
    paramStr := paramStr + Params[i];
  end;

  WriteLn(LogFile, Format('[%s] CALL %s(%s)',
    [FormatDateTime('hh:nn:ss.zzz', Now), Func, paramStr]));
  Flush(LogFile);
end;

procedure LogFFIReturn(const Func: string; const ReturnValue: string);  
begin
  WriteLn(LogFile, Format('[%s] RETURN %s = %s',
    [FormatDateTime('hh:nn:ss.zzz', Now), Func, ReturnValue]));
  Flush(LogFile);
end;

procedure LogFFIError(const Func, Error: string);  
begin
  WriteLn(LogFile, Format('[%s] ERROR %s: %s',
    [FormatDateTime('hh:nn:ss.zzz', Now), Func, Error]));
  Flush(LogFile);
end;

initialization
  AssignFile(LogFile, 'ffi_debug.log');
  Rewrite(LogFile);

finalization
  CloseFile(LogFile);

end.
```

### 12.7 Optimisation FFI

#### Réduire les appels

**❌ Mauvais** : Appels répétés

```pascal
for i := 0 to 1000000 do
  result := c_simple_function(i);  // 1 million d'appels FFI !
```

**✓ Bon** : Traitement par batch

```pascal
var
  data: array[0..1000000] of Integer;
  results: array[0..1000000] of Integer;
begin
  // Remplir data
  for i := 0 to High(data) do
    data[i] := i;

  // 1 seul appel FFI
  c_batch_function(@data[0], @results[0], Length(data));
end;
```

#### Inline des appels fréquents

```pascal
// Pour les fonctions très simples, éviter FFI
function FastMod(a, b: Integer): Integer; inline;  
begin
  Result := a mod b;  // Pas besoin d'appeler C pour ça
end;
```

#### Cache des résultats

```pascal
type
  TCachedFFI = class
  private
    FCache: TDictionary<Double, Double>;
  public
    function Sqrt(x: Double): Double;
  end;

function TCachedFFI.Sqrt(x: Double): Double;  
begin
  if not FCache.TryGetValue(x, Result) then
  begin
    Result := c_sqrt(x);  // Appel FFI uniquement si pas en cache
    FCache.Add(x, Result);
  end;
end;
```

### 12.8 Pièges courants

#### 1. Alignement des structures

**❌ Problème** :
```pascal
type
  TBadAlign = record  // Alignement Pascal par défaut
    a: Byte;          // 1 octet
    b: Integer;       // 4 octets
  end;  // Taille : 5 ou 8 octets selon l'alignement
```

**✓ Solution** :
```pascal
{$PACKRECORDS C}
type
  TGoodAlign = record
    a: Byte;
    b: Integer;
  end;
{$PACKRECORDS DEFAULT}
```

#### 2. Chaînes Pascal vs C

**❌ Problème** :
```pascal
var
  s: string;
begin
  s := 'Hello';
  c_function(s);  // ❌ Passe un pointeur vers la structure string Pascal !
end;
```

**✓ Solution** :
```pascal
var
  s: AnsiString;
begin
  s := 'Hello';
  c_function(PAnsiChar(s));  // ✓ Conversion explicite
end;
```

#### 3. Durée de vie des pointeurs

**❌ Problème** :
```pascal
function GetName: PAnsiChar;  
var
  s: AnsiString;
begin
  s := 'Temporaire';
  Result := PAnsiChar(s);  // ❌ s est libéré en sortant de fonction !
end;
```

**✓ Solution** :
```pascal
function GetName: PAnsiChar;  
const
  NAME: AnsiString = 'Permanent';  // Constante globale
begin
  Result := PAnsiChar(NAME);
end;
```

#### 4. Callbacks et contexte

**❌ Problème** :
```pascal
procedure ProcessData;  
var
  localVar: Integer;

  procedure MyCallback(value: Integer); cdecl;
  begin
    Inc(localVar, value);  // ❌ Accès à une variable locale !
  end;

begin
  c_register_callback(@MyCallback);
end;
```

**✓ Solution** :
```pascal
type
  TContext = record
    total: Integer;
  end;

procedure MyCallback(value: Integer; context: Pointer); cdecl;  
var
  ctx: ^TContext;
begin
  ctx := context;
  Inc(ctx^.total, value);
end;

procedure ProcessData;  
var
  context: TContext;
begin
  context.total := 0;
  c_register_callback_with_context(@MyCallback, @context);
end;
```

#### 5. Convention d'appel incorrecte

**❌ Problème** :
```pascal
// Déclaration sans convention (par défaut : register)
function c_function(x: Integer): Integer; external 'lib.so';
```

**✓ Solution** :
```pascal
// Toujours spécifier cdecl pour les fonctions C
function c_function(x: Integer): Integer; cdecl; external 'lib.so';
```

### 12.9 Checklist avant production

**Avant de déployer du code FFI en production** :

- [ ] Toutes les conventions d'appel sont spécifiées (`cdecl`, `stdcall`)
- [ ] L'alignement des structures est correct (`{$PACKRECORDS C}`)
- [ ] La gestion de la mémoire est cohérente (qui alloue doit libérer)
- [ ] Les codes d'erreur sont vérifiés après chaque appel
- [ ] Les ressources sont libérées avec `try-finally`
- [ ] Le code est testé sur toutes les plateformes cibles
- [ ] Les chemins de bibliothèques sont corrects (Windows/Linux)
- [ ] La documentation indique les dépendances externes
- [ ] Les callbacks ne capturent pas de variables locales
- [ ] Les tests couvrent les cas limites et les erreurs

### 12.10 Pour aller plus loin

**Sujets avancés** :

1. **Génération automatique de bindings**
   - Utiliser h2pas pour convertir les headers C
   - SWIG pour générer des bindings complexes
   - Créer ses propres générateurs

2. **FFI avec C++**
   - Utiliser `extern "C"` côté C++
   - Gérer les classes C++ avec des wrappers C
   - Name mangling et démangling

3. **FFI avec Rust**
   - Rust expose des fonctions C-compatible
   - Sécurité mémoire garantie côté Rust
   - Performance native

4. **FFI avec Python**
   - Python4Lazarus
   - Embedding Python dans FreePascal
   - Appeler du code Pascal depuis Python

5. **WebAssembly**
   - Compiler FreePascal en WASM
   - FFI JavaScript ↔ Pascal
   - Performance proche du natif

**Lectures recommandées** :

📖 "Expert Pascal Programming" - Marco Cantù  
📖 "Mastering FreePascal" - Community  
📖 FreePascal Documentation officielle  
📖 "The Art of Assembly Language" - Randall Hyde

---

## Annexes

### Annexe A : Tableaux de correspondance des types

#### Types de base C ↔ Pascal

| C | FreePascal | Bits | Signé | Notes |
|---|------------|------|-------|-------|
| `char` | `AnsiChar` | 8 | Oui/Non* | Dépend du compilateur |
| `signed char` | `ShortInt` | 8 | Oui | -128 à 127 |
| `unsigned char` | `Byte` | 8 | Non | 0 à 255 |
| `short` | `SmallInt` | 16 | Oui | -32768 à 32767 |
| `unsigned short` | `Word` | 16 | Non | 0 à 65535 |
| `int` | `LongInt` / `Integer` | 32 | Oui | -2³¹ à 2³¹-1 |
| `unsigned int` | `LongWord` / `Cardinal` | 32 | Non | 0 à 2³²-1 |
| `long` | `PtrInt`** | 32/64 | Oui | Dépend de la plateforme |
| `unsigned long` | `PtrUInt`** | 32/64 | Non | Dépend de la plateforme |
| `long long` | `Int64` | 64 | Oui | -2⁶³ à 2⁶³-1 |
| `unsigned long long` | `QWord` | 64 | Non | 0 à 2⁶⁴-1 |
| `float` | `Single` | 32 | Oui | IEEE 754 |
| `double` | `Double` | 64 | Oui | IEEE 754 |
| `long double` | `Extended`*** | 80 | Oui | x86 uniquement |
| `void*` | `Pointer` | 32/64 | - | Pointeur générique |
| `char*` | `PAnsiChar` | 32/64 | - | Chaîne C |
| `size_t` | `size_t` / `PtrUInt` | 32/64 | Non | Taille mémoire |
| `ssize_t` | `ssize_t` / `PtrInt` | 32/64 | Oui | Taille signée |

\* Sur la plupart des systèmes, `char` est signé
\*\* `PtrInt` et `PtrUInt` sont 32-bit sur x86, 64-bit sur x64
\*\*\* `Extended` n'est plus supporté sur ARM et x64

#### Types Windows spécifiques

| Windows | FreePascal | Description |
|---------|------------|-------------|
| `BYTE` | `Byte` | 8-bit non signé |
| `WORD` | `Word` | 16-bit non signé |
| `DWORD` | `LongWord` | 32-bit non signé |
| `QWORD` | `QWord` | 64-bit non signé |
| `BOOL` | `LongBool` | Booléen 32-bit |
| `HANDLE` | `THandle` | Handle générique |
| `HWND` | `HWND` | Handle de fenêtre |
| `HINSTANCE` | `HINST` | Handle d'instance |
| `LPSTR` | `PAnsiChar` | Chaîne ANSI |
| `LPWSTR` | `PWideChar` | Chaîne Unicode |
| `LPCSTR` | `PAnsiChar` | Chaîne ANSI const |
| `LPCWSTR` | `PWideChar` | Chaîne Unicode const |

### Annexe B : Outils de diagnostic

#### Sous Linux

```bash
# Lister les symboles d'une bibliothèque
nm -D libtest.so

# Voir les dépendances
ldd libtest.so

# Voir les symboles exportés (plus lisible)
objdump -T libtest.so

# Vérifier qu'un symbole existe
nm -D libtest.so | grep sqrt

# Tracer les appels de fonctions
ltrace -C ./mon_programme

# Tracer les appels système
strace ./mon_programme
```

#### Sous Windows

```powershell
# Voir les dépendances avec Dependency Walker
depends.exe mon_programme.exe

# Voir les exports d'une DLL
dumpbin /EXPORTS ma_lib.dll

# Utiliser Process Monitor
procmon.exe
```

### Annexe C : Template de wrapper

```pascal
unit MyLibWrapper;

{$mode objfpc}{$H+}
{$PACKRECORDS C}

interface

uses
  DynLibs, SysUtils;

type
  EMyLibException = class(Exception);

  TMyLib = class
  private
    FLibHandle: TLibHandle;
    FInitialized: Boolean;

    // Pointeurs de fonctions
    Finit: function: Integer; cdecl;
    Fcleanup: procedure; cdecl;
    // ... autres fonctions

    procedure LoadFunctions;
    procedure CheckError(ErrorCode: Integer; const Msg: string);
  public
    constructor Create;
    destructor Destroy; override;

    // Méthodes publiques
    procedure Initialize;
    // ... autres méthodes
  end;

implementation

const
  {$IFDEF WINDOWS}
  LIB_NAME = 'mylib.dll';
  {$ELSE}
  LIB_NAME = 'libmylib.so';
  {$ENDIF}

  // Codes d'erreur
  ERR_OK = 0;
  ERR_INVALID = -1;

constructor TMyLib.Create;  
begin
  inherited Create;
  FInitialized := False;

  FLibHandle := LoadLibrary(LIB_NAME);
  if FLibHandle = NilHandle then
    raise EMyLibException.CreateFmt('Impossible de charger %s', [LIB_NAME]);

  try
    LoadFunctions;
  except
    UnloadLibrary(FLibHandle);
    raise;
  end;
end;

destructor TMyLib.Destroy;  
begin
  if FInitialized then
    Fcleanup;

  if FLibHandle <> NilHandle then
    UnloadLibrary(FLibHandle);

  inherited;
end;

procedure TMyLib.LoadFunctions;  
begin
  Pointer(Finit) := GetProcAddress(FLibHandle, 'mylib_init');
  Pointer(Fcleanup) := GetProcAddress(FLibHandle, 'mylib_cleanup');

  if (@Finit = nil) or (@Fcleanup = nil) then
    raise EMyLibException.Create('Fonctions non trouvées');
end;

procedure TMyLib.CheckError(ErrorCode: Integer; const Msg: string);  
begin
  if ErrorCode <> ERR_OK then
    raise EMyLibException.CreateFmt('%s (code: %d)', [Msg, ErrorCode]);
end;

procedure TMyLib.Initialize;  
var
  ret: Integer;
begin
  ret := Finit;
  CheckError(ret, 'Erreur d''initialisation');
  FInitialized := True;
end;

end.
```

---

**FIN DU CHAPITRE 19.11 - FFI (Foreign Function Interface)**

Ce chapitre vous a fourni tous les outils nécessaires pour maîtriser le FFI dans FreePascal/Lazarus, de l'utilisation basique aux techniques avancées, avec des exemples pratiques et des wrappers complets pour des bibliothèques réelles.

⏭️ [Optimisation et Performance](/20-optimisation-performance/README.md)
