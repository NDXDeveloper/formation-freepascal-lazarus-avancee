🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 19.2 Bindings C/C++ avancés

## Introduction

Les bindings (liaisons) permettent d'utiliser des bibliothèques écrites en C ou C++ directement depuis FreePascal. C'est une compétence essentielle pour accéder à l'écosystème riche des bibliothèques existantes et pour interagir avec le système d'exploitation.

### Pourquoi créer des bindings ?

- Réutiliser des bibliothèques C/C++ éprouvées sans les réécrire
- Accéder aux API système (Windows API, POSIX, etc.)
- Intégrer des bibliothèques tierces (SDL, OpenGL, OpenCV, etc.)
- Optimiser certaines parties critiques en C/C++ tout en gardant l'application en Pascal

## Concepts fondamentaux

### La convention d'appel

Lorsque vous appelez une fonction externe, le compilateur doit savoir comment organiser les paramètres en mémoire. C'est ce qu'on appelle la **convention d'appel**.

#### Principales conventions d'appel

**cdecl** (C declaration)
- Convention standard du langage C
- L'appelant nettoie la pile
- Compatible avec les fonctions variadiques (nombre variable de paramètres)
- Utilisée sur Linux et pour la plupart des bibliothèques C

```pascal
function ma_fonction(x: Integer): Integer; cdecl; external 'malib';
```

**stdcall** (Standard call)
- Convention Windows standard
- La fonction appelée nettoie la pile
- Utilisée par l'API Windows (Win32 API)
- Plus efficace que cdecl mais incompatible avec les fonctions variadiques

```pascal
function MessageBoxA(hWnd: HWND; lpText: PChar; lpCaption: PChar;
  uType: UINT): Integer; stdcall; external 'user32.dll';
```

**Choisir la bonne convention**
- Sur **Windows** : `stdcall` pour l'API Windows, `cdecl` pour les bibliothèques C standard
- Sur **Linux/Ubuntu** : `cdecl` dans la grande majorité des cas

### Liaison externe : external vs external name

#### Syntaxe de base

```pascal
// Liaison simple - le nom Pascal et C sont identiques
function strlen(s: PChar): NativeInt; cdecl; external 'c';

// Liaison avec nom différent
function longueur_chaine(s: PChar): NativeInt; cdecl;
  external 'c' name 'strlen';
```

#### Spécifier la bibliothèque

**Sous Windows (.dll)**
```pascal
function sqlite3_open(filename: PChar; var db: Pointer): Integer; cdecl;
  external 'sqlite3.dll';
```

**Sous Linux (.so)**
```pascal
function sqlite3_open(filename: PChar; var db: Pointer): Integer; cdecl;
  external 'libsqlite3.so';
```

**Multi-plateforme**
```pascal
{$IFDEF WINDOWS}
const SQLITE_LIB = 'sqlite3.dll';
{$ELSE}
const SQLITE_LIB = 'libsqlite3.so';
{$ENDIF}

function sqlite3_open(filename: PChar; var db: Pointer): Integer; cdecl;
  external SQLITE_LIB;
```

## Correspondance des types

### Types simples

La correspondance entre les types C et Pascal est cruciale pour éviter les erreurs.

| Type C | Type Pascal (32-bit) | Type Pascal (64-bit) | Notes |
|--------|---------------------|---------------------|-------|
| char | Byte ou AnsiChar | Byte ou AnsiChar | Signé ou non selon le compilateur C |
| unsigned char | Byte | Byte | |
| short | SmallInt | SmallInt | 16 bits signé |
| unsigned short | Word | Word | 16 bits non signé |
| int | Integer ou LongInt | Integer ou LongInt | 32 bits signé |
| unsigned int | LongWord ou Cardinal | LongWord ou Cardinal | 32 bits non signé |
| long | LongInt | Int64 | Dépend de la plateforme ! |
| unsigned long | LongWord | QWord | Dépend de la plateforme ! |
| long long | Int64 | Int64 | 64 bits signé |
| size_t | NativeUInt | NativeUInt | Taille dépendant de l'architecture |
| ptrdiff_t | NativeInt | NativeInt | |
| float | Single | Single | 32 bits |
| double | Double | Double | 64 bits |
| void* | Pointer | Pointer | Pointeur générique |

### Pointeurs et chaînes

#### Chaînes de caractères

En C, les chaînes sont des tableaux de caractères terminés par zéro (`\0`).

```pascal
// En C : char* ou const char*
// En Pascal : PChar ou PAnsiChar

function get_name(): PChar; cdecl; external 'mylib';

var
  nom: PChar;
  nomPascal: string;
begin
  nom := get_name();
  nomPascal := StrPas(nom);  // Conversion en string Pascal
end;
```

**Attention aux encodages**
- `PChar` = pointeur vers caractère dans l'encodage par défaut
- `PAnsiChar` = pointeur vers caractère ANSI (1 octet)
- `PWideChar` = pointeur vers caractère Unicode (UTF-16, 2 octets)
- `PUTF8String` = pointeur vers chaîne UTF-8

#### Passage de chaînes à une fonction C

```pascal
// Fonction C : void process_string(const char* str)
procedure process_string(str: PChar); cdecl; external 'mylib';

var
  texte: string;
begin
  texte := 'Bonjour le monde';
  process_string(PChar(texte));  // Conversion temporaire
end;
```

**Important** : Si la fonction C modifie la chaîne, utilisez un buffer fixe :

```pascal
var
  buffer: array[0..255] of AnsiChar;
begin
  StrPCopy(buffer, 'Texte initial');
  process_string(buffer);
end;
```

### Tableaux

En C, les tableaux et pointeurs sont très liés.

```c
// En C
void process_array(int* arr, int size);
```

```pascal
// En Pascal
procedure process_array(arr: PInteger; size: Integer); cdecl;
  external 'mylib';

var
  tableau: array[0..9] of Integer;
begin
  // @ donne l'adresse du premier élément
  process_array(@tableau[0], Length(tableau));
end;
```

**Tableau dynamique**
```pascal
var
  tableau: array of Integer;
begin
  SetLength(tableau, 100);
  // Les tableaux dynamiques sont déjà des pointeurs
  process_array(PInteger(tableau), Length(tableau));
end;
```

## Structures et records

### Correspondance structure C / record Pascal

```c
// Structure C
typedef struct {
    int x;
    int y;
    char name[50];
} Point;
```

```pascal
// Record Pascal équivalent
type
  TPoint = record
    x: Integer;
    y: Integer;
    name: array[0..49] of AnsiChar;
  end;
  PPoint = ^TPoint;
```

### Alignement mémoire : packed

**Problème** : Le compilateur peut ajouter du padding (remplissage) pour aligner les champs en mémoire.

```c
// Structure C
struct MyStruct {
    char a;      // 1 octet
    int b;       // 4 octets (peut être aligné sur 4 octets)
    char c;      // 1 octet
};
// Taille réelle : peut être 12 octets au lieu de 6 !
```

**Solution** : Utiliser `packed` pour désactiver l'alignement

```pascal
type
  TMyStruct = packed record
    a: AnsiChar;  // 1 octet
    b: Integer;   // 4 octets
    c: AnsiChar;  // 1 octet
  end;
  // Taille garantie : 6 octets
```

**Attention** : Vérifiez toujours la taille avec `SizeOf` et comparez avec la structure C.

### Directives d'alignement

Si la bibliothèque C utilise un alignement spécifique :

```pascal
{$PACKRECORDS C}  // Utilise l'alignement C standard
type
  TMyStruct = record
    a: AnsiChar;
    b: Integer;
    c: AnsiChar;
  end;
{$PACKRECORDS DEFAULT}  // Retour à l'alignement par défaut
```

Autres options :
- `{$PACKRECORDS 1}` : Aucun alignement (comme packed)
- `{$PACKRECORDS 2}` : Alignement sur 2 octets
- `{$PACKRECORDS 4}` : Alignement sur 4 octets
- `{$PACKRECORDS 8}` : Alignement sur 8 octets

## Fonctions callback

Les callbacks sont des fonctions Pascal que vous passez à une bibliothèque C pour qu'elle les appelle.

### Définir un callback

```c
// En C
typedef void (*callback_t)(int value);  
void register_callback(callback_t cb);
```

```pascal
// En Pascal
type
  TCallback = procedure(value: Integer); cdecl;

procedure register_callback(cb: TCallback); cdecl; external 'mylib';

// Implémentation du callback
procedure mon_callback(value: Integer); cdecl;  
begin
  WriteLn('Valeur reçue : ', value);
end;

begin
  register_callback(@mon_callback);
end;
```

**Important** : Le callback doit utiliser la même convention d'appel que ce qu'attend la bibliothèque C (généralement `cdecl`).

### Callbacks avec contexte (closure)

Certaines bibliothèques permettent de passer un pointeur de contexte :

```c
// En C
typedef void (*callback_with_context_t)(int value, void* user_data);  
void register_callback_ex(callback_with_context_t cb, void* user_data);
```

```pascal
type
  TCallbackWithContext = procedure(value: Integer; user_data: Pointer); cdecl;

procedure register_callback_ex(cb: TCallbackWithContext;
  user_data: Pointer); cdecl; external 'mylib';

type
  TMonContexte = record
    compteur: Integer;
    nom: string;
  end;
  PMonContexte = ^TMonContexte;

procedure callback_avec_contexte(value: Integer; user_data: Pointer); cdecl;  
var
  ctx: PMonContexte;
begin
  ctx := PMonContexte(user_data);
  Inc(ctx^.compteur);
  WriteLn(ctx^.nom, ' a reçu : ', value);
end;

var
  contexte: TMonContexte;
begin
  contexte.compteur := 0;
  contexte.nom := 'MonHandler';
  register_callback_ex(@callback_avec_contexte, @contexte);
end;
```

## Énumérations et constantes

### Énumérations C

```c
// En C
enum Color {
    COLOR_RED = 0,
    COLOR_GREEN = 1,
    COLOR_BLUE = 2
};
```

```pascal
// En Pascal - Option 1 : constantes
const
  COLOR_RED = 0;
  COLOR_GREEN = 1;
  COLOR_BLUE = 2;

// Option 2 : énumération typée (plus sûr)
type
  TColor = (COLOR_RED = 0, COLOR_GREEN = 1, COLOR_BLUE = 2);
```

### Flags et masques de bits

```c
// En C
#define FLAG_READ    0x01
#define FLAG_WRITE   0x02
#define FLAG_EXECUTE 0x04
```

```pascal
// En Pascal
const
  FLAG_READ    = $01;
  FLAG_WRITE   = $02;
  FLAG_EXECUTE = $04;

// Utilisation
var
  flags: Integer;
begin
  flags := FLAG_READ or FLAG_WRITE;  // Combinaison
  if (flags and FLAG_READ) <> 0 then  // Test
    WriteLn('Lecture autorisée');
end;
```

## Gestion d'erreurs

### Codes de retour

```pascal
function open_file(filename: PChar): Integer; cdecl; external 'mylib';

var
  result: Integer;
begin
  result := open_file('data.txt');
  if result < 0 then
    WriteLn('Erreur lors de l''ouverture du fichier : ', result)
  else
    WriteLn('Fichier ouvert avec succès');
end;
```

### errno sur Linux

```pascal
{$IFDEF UNIX}
function __errno_location(): PInteger; cdecl; external 'c' name '__errno_location';

function GetErrno: Integer; inline;  
begin
  Result := __errno_location()^;
end;
{$ENDIF}

procedure test_erreur;  
var
  f: Integer;
begin
  f := FpOpen('fichier_inexistant.txt', O_RDONLY);
  if f < 0 then
  begin
    WriteLn('Erreur : ', GetErrno);
    WriteLn('Message : ', StrError(GetErrno));
  end;
end;
```

## Exemple complet : binding de la bibliothèque cURL

cURL est une bibliothèque populaire pour effectuer des requêtes HTTP.

### Structures et types de base

```pascal
unit curl_binding;

{$mode objfpc}{$H+}

interface

const
  {$IFDEF WINDOWS}
  CURL_LIB = 'libcurl.dll';
  {$ELSE}
  CURL_LIB = 'libcurl.so';
  {$ENDIF}

type
  TCURL = Pointer;
  TCURLcode = Integer;
  TCURLoption = Integer;

const
  // Codes de retour
  CURLE_OK = 0;
  CURLE_FAILED_INIT = 2;

  // Options
  CURLOPT_URL = 10002;
  CURLOPT_WRITEFUNCTION = 20011;
  CURLOPT_WRITEDATA = 10001;

// Fonctions principales
function curl_easy_init(): TCURL; cdecl; external CURL_LIB;  
function curl_easy_setopt(curl: TCURL; option: TCURLoption): TCURLcode;
  cdecl; varargs; external CURL_LIB;
function curl_easy_perform(curl: TCURL): TCURLcode; cdecl; external CURL_LIB;  
procedure curl_easy_cleanup(curl: TCURL); cdecl; external CURL_LIB;  
function curl_easy_strerror(code: TCURLcode): PChar; cdecl; external CURL_LIB;

// Type callback pour recevoir les données
type
  TWriteCallback = function(buffer: PChar; size: NativeInt;
    nitems: NativeInt; userdata: Pointer): NativeInt; cdecl;

implementation

end.
```

### Utilisation du binding

```pascal
program test_curl;

uses
  curl_binding, SysUtils;

// Callback pour recevoir les données
function write_callback(buffer: PChar; size: NativeInt;
  nitems: NativeInt; userdata: Pointer): NativeInt; cdecl;
var
  realsize: NativeInt;
  output: ^string;
begin
  realsize := size * nitems;
  output := userdata;
  SetLength(output^, Length(output^) + realsize);
  Move(buffer^, output^[Length(output^) - realsize + 1], realsize);
  Result := realsize;
end;

var
  curl: TCURL;
  res: TCURLcode;
  response: string;
begin
  response := '';

  curl := curl_easy_init();
  if curl <> nil then
  begin
    try
      // Configuration
      curl_easy_setopt(curl, CURLOPT_URL, PChar('https://api.github.com'));
      curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, @write_callback);
      curl_easy_setopt(curl, CURLOPT_WRITEDATA, @response);

      // Exécution
      res := curl_easy_perform(curl);

      if res = CURLE_OK then
        WriteLn('Réponse reçue : ', Length(response), ' octets')
      else
        WriteLn('Erreur : ', StrPas(curl_easy_strerror(res)));

    finally
      curl_easy_cleanup(curl);
    end;
  end
  else
    WriteLn('Impossible d''initialiser cURL');
end.
```

## Techniques avancées

### Fonctions variadiques

Certaines fonctions C acceptent un nombre variable d'arguments (comme `printf`).

```pascal
// Déclaration avec varargs
function printf(format: PChar): Integer; cdecl; varargs; external 'c';

// Utilisation
begin
  printf('Nombre : %d, Texte : %s', 42, PChar('Hello'));
end;
```

**Attention** : Les types doivent correspondre exactement (pas de conversion automatique).

### Macros C et fonctions inline

Les macros C ne peuvent pas être importées directement. Il faut les réécrire en Pascal.

```c
// En C - Macro
#define MAX(a, b) ((a) > (b) ? (a) : (b))
```

```pascal
// En Pascal - Fonction inline
function MAX(a, b: Integer): Integer; inline;  
begin
  if a > b then
    Result := a
  else
    Result := b;
end;
```

### Gestion de la mémoire

#### Allocation par la bibliothèque C

Si une fonction C alloue de la mémoire, vous devez la libérer avec la fonction appropriée.

```pascal
// La bibliothèque alloue la mémoire
function create_object(): Pointer; cdecl; external 'mylib';
// Vous devez la libérer avec cette fonction
procedure destroy_object(obj: Pointer); cdecl; external 'mylib';

var
  obj: Pointer;
begin
  obj := create_object();
  try
    // Utilisation de obj
  finally
    destroy_object(obj);  // Important !
  end;
end;
```

**Ne jamais utiliser** `FreeMem` ou `Dispose` sur de la mémoire allouée par du code C !

#### Allocation côté Pascal pour le C

```pascal
procedure fill_buffer(buffer: PByte; size: Integer); cdecl; external 'mylib';

var
  buffer: PByte;
begin
  GetMem(buffer, 1024);
  try
    fill_buffer(buffer, 1024);
    // Traitement du buffer
  finally
    FreeMem(buffer);
  end;
end;
```

## Débogage des bindings

### Problèmes courants

1. **Crash au lancement**
   - Vérifier que la bibliothèque est présente et accessible
   - Vérifier l'architecture (32/64 bits)

2. **Crash lors de l'appel**
   - Convention d'appel incorrecte (cdecl vs stdcall)
   - Types de paramètres incorrects
   - Problème d'alignement des structures

3. **Résultats incorrects**
   - Erreur dans la correspondance des types
   - Problème d'endianness (rare)
   - Buffer trop petit

### Outils de diagnostic

**Vérifier le chargement de la bibliothèque**
```pascal
function LoadLibrary(libname: PChar): TLibHandle;  
var
  lib: TLibHandle;
begin
  lib := LoadLibrary('mylib.so');
  if lib = 0 then
    WriteLn('Impossible de charger la bibliothèque')
  else
  begin
    WriteLn('Bibliothèque chargée avec succès');
    FreeLibrary(lib);
  end;
end;
```

**Charger dynamiquement les fonctions**
```pascal
uses
  dynlibs;

type
  TMyFunction = function(x: Integer): Integer; cdecl;

var
  lib: TLibHandle;
  my_func: TMyFunction;
begin
  lib := LoadLibrary('mylib.so');
  if lib <> 0 then
  begin
    my_func := TMyFunction(GetProcAddress(lib, 'my_function'));
    if Assigned(my_func) then
      WriteLn('Résultat : ', my_func(42))
    else
      WriteLn('Fonction non trouvée');
    FreeLibrary(lib);
  end;
end;
```

## Bonnes pratiques

### Organisation du code

1. **Un fichier par bibliothèque** : Créez une unit dédiée pour chaque bibliothèque C
2. **Séparation interface/implémentation** : Définissez les types et fonctions dans l'interface
3. **Documentation** : Commentez les particularités et les gotchas

### Exemple de structure

```pascal
unit mylib_binding;

{$mode objfpc}{$H+}

interface

// Définition de la bibliothèque selon l'OS
const
  {$IFDEF WINDOWS}
  MYLIB = 'mylib.dll';
  {$ELSE}
  {$IFDEF DARWIN}
  MYLIB = 'libmylib.dylib';
  {$ELSE}
  MYLIB = 'libmylib.so';
  {$ENDIF}
  {$ENDIF}

// Types
type
  TMyHandle = Pointer;

// Constantes
const
  MY_SUCCESS = 0;
  MY_ERROR = -1;

// Fonctions
function my_init(): TMyHandle; cdecl; external MYLIB;  
procedure my_cleanup(handle: TMyHandle); cdecl; external MYLIB;

implementation

// Code d'initialisation si nécessaire

end.
```

### Tests et validation

1. Testez chaque fonction binding individuellement
2. Comparez les résultats avec un programme C équivalent
3. Testez sur toutes les plateformes cibles
4. Vérifiez les cas limites (valeurs nulles, buffers vides, etc.)

## Conclusion

Les bindings C/C++ sont un outil puissant pour étendre les capacités de FreePascal. En maîtrisant :
- La correspondance des types
- Les conventions d'appel
- L'alignement mémoire
- La gestion des callbacks

Vous pouvez intégrer pratiquement n'importe quelle bibliothèque C dans vos projets FreePascal, tout en conservant la sécurité et l'élégance du langage Pascal.

N'oubliez pas de toujours :
- Vérifier la documentation de la bibliothèque C
- Tester exhaustivement vos bindings
- Gérer correctement la mémoire
- Prévoir la portabilité multi-plateforme dès le début

⏭️ [Interfaçage avec Python](/19-interoperabilite-bindings/03-interfacage-python.md)
