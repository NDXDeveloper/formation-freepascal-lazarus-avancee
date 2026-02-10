🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 19.10 Génération automatique de bindings

## Table des matières

1. [Introduction aux bindings](#1-introduction-aux-bindings)
2. [Pourquoi automatiser ?](#2-pourquoi-automatiser-la-génération-de-bindings)
3. [Outils disponibles](#3-outils-de-génération-automatique)
4. [h2pas - L'outil principal](#4-h2pas--convertir-les-headers-c-en-pascal)
5. [SWIG - Bindings multilangage](#5-swig--bindings-multilangage)
6. [Créer son générateur](#6-créer-son-propre-générateur)
7. [Bindings pour C](#7-bindings-pour-bibliothèques-c)
8. [Bindings pour C++](#8-bindings-pour-bibliothèques-c)
9. [Cas pratiques](#9-cas-pratiques)
10. [Bonnes pratiques](#10-bonnes-pratiques)
11. [Conclusion](#11-conclusion)

---

## 1. Introduction aux bindings

### 1.1 Qu'est-ce qu'un binding ?

Un **binding** (liaison en français) est une couche de code qui permet à FreePascal d'utiliser des bibliothèques écrites dans d'autres langages, principalement C et C++.

**Analogie simple** : Imaginez une bibliothèque C comme un livre écrit en anglais. Le binding est la traduction en français qui vous permet de comprendre et d'utiliser ce livre.

### 1.2 Architecture d'un binding

```
┌─────────────────────────────────────────────┐
│       Application FreePascal                │
│                                             │
│   var result: Integer;                      │
│   result := c_function(10, 20);             │
└────────────────┬────────────────────────────┘
                 │
         ┌───────▼────────┐
         │    Binding     │  ← Déclarations Pascal
         │   (Pascal)     │
         └───────┬────────┘
                 │
         ┌───────▼────────┐
         │ Bibliothèque C │  ← Code compilé (.dll/.so)
         └────────────────┘
```

### 1.3 Exemple de binding

**Code C (bibliothèque)** :
```c
// math_lib.h
int add(int a, int b) {
    return a + b;
}
```

**Binding Pascal** :
```pascal
// math_lib_bindings.pas
function add(a, b: Integer): Integer; cdecl; external 'libmath.so';
```

**Utilisation en Pascal** :
```pascal
program TestBinding;

var
  result: Integer;
begin
  result := add(10, 20);  // Appelle la fonction C !
  WriteLn('10 + 20 = ', result);
end.
```

### 1.4 Types de bindings

#### Bindings manuels
Écrits à la main, fonction par fonction.

**Avantages** : Contrôle total, optimisé  
**Inconvénients** : Très long, risque d'erreurs, difficile à maintenir  

#### Bindings semi-automatiques
Générés automatiquement puis modifiés manuellement.

**Avantages** : Base solide, gain de temps  
**Inconvénients** : Nécessite ajustements manuels  

#### Bindings automatiques
Entièrement générés par un outil.

**Avantages** : Très rapide, facile à mettre à jour  
**Inconvénients** : Moins de contrôle, peut nécessiter corrections  

---

## 2. Pourquoi automatiser la génération de bindings ?

### 2.1 Le problème du volume

**Bibliothèques réelles** :
- **SQLite** : ~200 fonctions publiques
- **OpenSSL** : ~3000 fonctions
- **Qt** : ~10000+ fonctions

Écrire manuellement les bindings pour OpenSSL prendrait **plusieurs mois** !

### 2.2 Le problème des erreurs

**Exemple d'erreur courante** :

```c
// Header C
int process(const char* name, struct options* opts, callback_t cb);
```

```pascal
// ❌ Binding manuel incorrect
function process(name: PChar; opts: Pointer; cb: TCallback): Integer;
// Erreurs :
// - PChar au lieu de PAnsiChar
// - Pointer au lieu du type struct correct
// - Type callback peut-être incorrect
```

### 2.3 Le problème de la maintenance

```c
// Version 1.0 de la bibliothèque
int process(int value);

// Version 2.0 - Nouveau paramètre ajouté
int process(int value, int flags);
```

Avec des bindings manuels, il faut :
1. Trouver tous les changements dans la documentation
2. Modifier chaque fonction concernée
3. Vérifier que rien n'est cassé
4. Recompiler et tester

**Avec génération automatique** :
```bash
h2pas library_v2.h  # C'est fait !
```

### 2.4 Gains de l'automatisation

| Aspect | Manuel | Automatique | Gain |
|--------|--------|-------------|------|
| **Temps** | 2-3 semaines | 1-2 jours | 90% |
| **Erreurs** | Nombreuses | Rares | 95% |
| **Maintenance** | Difficile | Facile | 85% |
| **Cohérence** | Variable | Uniforme | 100% |

---

## 3. Outils de génération automatique

### 3.1 Vue d'ensemble

| Outil | Source | Cible | Difficulté | Usage recommandé |
|-------|--------|-------|------------|------------------|
| **h2pas** | C | Pascal | ⭐ Facile | Headers C simples |
| **SWIG** | C/C++ | Multiple | ⭐⭐ Moyen | Projets multilangage |
| **Script custom** | Tout | Pascal | ⭐⭐⭐ Variable | Besoins spécifiques |
| **libclang** | C/C++ | Tout | ⭐⭐⭐ Difficile | Projets complexes |

### 3.2 h2pas - L'outil principal

**Points forts** :
- ✓ Inclus avec FreePascal (aucune installation)
- ✓ Simple à utiliser
- ✓ Génère du Pascal natif
- ✓ Parfait pour débuter

**Points faibles** :
- ✗ Limité au C (pas C++)
- ✗ Corrections manuelles souvent nécessaires
- ✗ Macros complexes non supportées

**Verdict** : **Excellent pour 80% des cas d'usage**

### 3.3 SWIG - Pour projets avancés

**Points forts** :
- ✓ Support C et C++
- ✓ Très flexible
- ✓ Génère pour de nombreux langages

**Points faibles** :
- ✗ Courbe d'apprentissage
- ✗ Configuration complexe

**Verdict** : **Pour projets multilangage**

### 3.4 Scripts personnalisés

**Points forts** :
- ✓ Contrôle total
- ✓ Adapté à vos besoins précis

**Points faibles** :
- ✗ Demande du développement
- ✗ Maintenance à prévoir

**Verdict** : **Pour besoins très spécifiques**

---

## 4. h2pas : Convertir les headers C en Pascal

### 4.1 Installation

**Bonne nouvelle : h2pas est déjà installé avec FreePascal !**

**Vérification** :
```bash
h2pas -h
# Devrait afficher l'aide
```

**Version** :
```bash
h2pas -v
# Free Pascal H2Pas version x.x.x
```

### 4.2 Premier exemple simple

**Fichier C (math_simple.h)** :
```c
#ifndef MATH_SIMPLE_H
#define MATH_SIMPLE_H

// Fonction d'addition
int add(int a, int b);

// Fonction de multiplication
double multiply(double a, double b);

#endif
```

**Génération** :
```bash
h2pas math_simple.h
```

**Résultat (math_simple.pas)** :
```pascal
unit math_simple;

interface

function add(a, b: longint): longint; cdecl; external;
function multiply(a, b: double): double; cdecl; external;

implementation

end.
```

### 4.3 Exemple avec types complexes

**Fichier C (geometry.h)** :
```c
#ifndef GEOMETRY_H
#define GEOMETRY_H

#define PI 3.14159265359

typedef struct {
    double x;
    double y;
} Point;

typedef enum {
    SHAPE_CIRCLE,
    SHAPE_SQUARE,
    SHAPE_TRIANGLE
} ShapeType;

Point create_point(double x, double y);
double distance(Point p1, Point p2);

#endif
```

**Génération** :
```bash
h2pas geometry.h -o geometry.pas
```

**Résultat (geometry.pas)** :
```pascal
unit geometry;

interface

const
  PI = 3.14159265359;

type
  Point = record
    x: double;
    y: double;
  end;
  PPoint = ^Point;

  ShapeType = (
    SHAPE_CIRCLE,
    SHAPE_SQUARE,
    SHAPE_TRIANGLE
  );

function create_point(x, y: double): Point; cdecl; external;
function distance(p1, p2: Point): double; cdecl; external;

implementation

end.
```

### 4.4 Options importantes de h2pas

#### Option -l : Spécifier la bibliothèque

```bash
h2pas -l libmath.so.1 geometry.h
```

**Résultat** :
```pascal
function create_point(x, y: double): Point; cdecl; external 'libmath.so.1';
```

#### Option -u : Créer une unit complète

```bash
h2pas -u geometry.h
```

Génère automatiquement les sections `interface` et `implementation`.

#### Option -p : Ajouter un préfixe

```bash
h2pas -p Geo_ geometry.h
```

**Résultat** :
```pascal
function Geo_create_point(x, y: double): Point; cdecl; external;
```

#### Option -t : Convertir les types

```bash
h2pas -t geometry.h
```

Convertit automatiquement les types C en types Pascal natifs.

### 4.5 Commande complète pour usage professionnel

```bash
h2pas \
  -u \                      # Créer une unit
  -l libgeometry.so.1 \    # Spécifier la bibliothèque
  -p Geo_ \                # Préfixer les symboles
  -t \                     # Convertir les types
  -o GeometryBindings.pas \ # Nom de fichier de sortie
  geometry.h
```

### 4.6 Erreurs courantes et solutions

#### Problème 1 : Macros complexes

**Header C** :
```c
#define COMPLEX_MACRO(x, y) ((x) + (y) * 2)
```

**h2pas ne peut pas convertir** cette macro.

**Solution** : Ajouter manuellement après génération
```pascal
// Ajouter dans le fichier .pas
function COMPLEX_MACRO(x, y: Integer): Integer; inline;
begin
  Result := x + y * 2;
end;
```

#### Problème 2 : Types opaques

**Header C** :
```c
typedef struct SDL_Window SDL_Window;
```

**h2pas génère** :
```pascal
type
  SDL_Window = record end;  // ❌ Incorrect
```

**Correction** :
```pascal
type
  PSDL_Window = Pointer;  // ✓ Correct pour type opaque
```

#### Problème 3 : Callbacks

**Header C** :
```c
typedef void (*callback_t)(int value);
```

**Correction dans Pascal** :
```pascal
type
  callback_t = procedure(value: Integer); cdecl;
```

### 4.7 Script de post-traitement

**Automatiser les corrections courantes** :

```bash
#!/bin/bash
# correct_bindings.sh

INPUT="$1"
OUTPUT="$2"

# Générer avec h2pas
h2pas -u -l libmath.so "$INPUT" -o "$OUTPUT"

# Corrections automatiques
sed -i 's/longint/Integer/g' "$OUTPUT"
sed -i 's/\bdouble\b/Double/g' "$OUTPUT"
sed -i 's/external;$/external libmath;/g' "$OUTPUT"

echo "✓ Bindings générés et corrigés : $OUTPUT"
```

**Utilisation** :
```bash
chmod +x correct_bindings.sh
./correct_bindings.sh math_lib.h MathBindings.pas
```

---

## 5. SWIG : Bindings multilangage

### 5.1 Introduction à SWIG

**SWIG** (Simplified Wrapper and Interface Generator) est un outil puissant qui génère des bindings pour de nombreux langages.

**Langages supportés** :
- Python, Java, C#, Ruby, Perl, Lua, PHP, JavaScript
- Et bien d'autres...

### 5.2 Installation

**Linux (Ubuntu/Debian)** :
```bash
sudo apt install swig
```

**Windows** :
```bash
# Via Chocolatey
choco install swig

# Ou télécharger : http://www.swig.org/download.html
```

**Vérification** :
```bash
swig -version
# SWIG Version 4.x.x
```

### 5.3 Fichier d'interface SWIG

SWIG utilise des fichiers `.i` pour décrire l'interface.

**math_lib.i** :
```swig
%module math_lib

%{
#include "math_lib.h"
%}

// Inclure le header C
%include "math_lib.h"
```

### 5.4 Génération pour Python (exemple)

```bash
# Générer le wrapper
swig -python math_lib.i

# Compiler
gcc -shared -fPIC math_lib.c math_lib_wrap.c \
    -I/usr/include/python3.10 \
    -o _math_lib.so
```

**Utilisation en Python** :
```python
import math_lib

result = math_lib.add(10, 20)
print(f"10 + 20 = {result}")
```

### 5.5 SWIG et Pascal

SWIG n'a pas de support officiel pour Pascal, mais vous pouvez :

**Approche 1** : Générer un wrapper C simplifié puis utiliser h2pas

```swig
%module math_lib_simple

%{
#include "math_lib.h"
%}

// Wrapper C simplifié
%inline %{
int ml_add(int a, int b) {
    return add(a, b);
}
%}
```

Puis :
```bash
swig -c math_lib_simple.i
h2pas math_lib_simple.h
```

**Approche 2** : Utiliser les bindings C générés directement

---

## 6. Créer son propre générateur

### 6.1 Pourquoi un générateur personnalisé ?

**Cas d'usage** :
- Format spécifique (XML, JSON de documentation)
- API propriétaire non-standard
- Transformations complexes requises
- Intégration dans pipeline de build

### 6.2 Architecture d'un générateur

```
┌──────────────┐
│ Fichier      │  Header C, XML, JSON, etc.
│ d'entrée     │
└──────┬───────┘
       │
   ┌───▼────┐
   │ Parser │  Analyser et extraire
   └───┬────┘
       │
   ┌───▼────┐
   │  AST   │  Représentation intermédiaire
   └───┬────┘
       │
   ┌───▼─────┐
   │Generator│  Générer le code Pascal
   └───┬─────┘
       │
┌──────▼──────┐
│Bindings.pas │
└─────────────┘
```

### 6.3 Générateur simple en Python

**gen_bindings.py** :
```python
#!/usr/bin/env python3
"""
Générateur de bindings Pascal
Lit un fichier JSON décrivant une API C
"""

import json
import sys

def convert_type(c_type):
    """Convertir un type C en type Pascal"""
    type_map = {
        'int': 'Integer',
        'long': 'LongInt',
        'float': 'Single',
        'double': 'Double',
        'char*': 'PAnsiChar',
        'void*': 'Pointer',
        'void': ''
    }
    return type_map.get(c_type, c_type)

def generate_function(func, library):
    """Générer le code d'une fonction"""
    # Construire la liste des paramètres
    params = []
    for p in func.get('params', []):
        param_str = f"{p['name']}: {convert_type(p['type'])}"
        params.append(param_str)

    params_str = '; '.join(params)
    return_type = convert_type(func.get('return', 'void'))

    # Fonction ou procédure ?
    if return_type:
        return f"function {func['name']}({params_str}): {return_type}; cdecl; external '{library}';"
    else:
        return f"procedure {func['name']}({params_str}); cdecl; external '{library}';"

def generate_struct(struct):
    """Générer le code d'une structure"""
    lines = [f"  {struct['name']} = record"]

    for field in struct['fields']:
        field_type = convert_type(field['type'])
        lines.append(f"    {field['name']}: {field_type};")

    lines.append("  end;")
    return '\n'.join(lines)

def generate_bindings(spec, output_file):
    """Générer le fichier complet"""
    with open(output_file, 'w') as f:
        # En-tête de l'unit
        f.write(f"unit {spec['name']};\n\n")
        f.write("interface\n\n")

        # Constantes
        if 'constants' in spec:
            f.write("const\n")
            for const in spec['constants']:
                f.write(f"  {const['name']} = {const['value']};\n")
            f.write("\n")

        # Types
        if 'structs' in spec:
            f.write("type\n")
            for struct in spec['structs']:
                f.write(generate_struct(struct) + "\n")
            f.write("\n")

        # Fonctions
        if 'functions' in spec:
            f.write("// Fonctions de la bibliothèque\n")
            for func in spec['functions']:
                f.write(generate_function(func, spec['library']) + "\n")

        # Pied de l'unit
        f.write("\nimplementation\n\n")
        f.write("end.\n")

def main():
    if len(sys.argv) < 3:
        print("Usage: gen_bindings.py input.json output.pas")
        print("Exemple: gen_bindings.py api.json MathBindings.pas")
        sys.exit(1)

    input_file = sys.argv[1]
    output_file = sys.argv[2]

    # Charger la spécification
    with open(input_file, 'r') as f:
        spec = json.load(f)

    # Générer les bindings
    generate_bindings(spec, output_file)
    print(f"✓ Bindings générés: {output_file}")

if __name__ == '__main__':
    main()
```

### 6.4 Fichier de spécification JSON

**api_spec.json** :
```json
{
  "name": "MathLib",
  "library": "libmath.so",
  "constants": [
    {"name": "PI", "value": "3.14159265359"},
    {"name": "E", "value": "2.71828182846"}
  ],
  "structs": [
    {
      "name": "TPoint",
      "fields": [
        {"name": "X", "type": "double"},
        {"name": "Y", "type": "double"}
      ]
    },
    {
      "name": "TRectangle",
      "fields": [
        {"name": "TopLeft", "type": "TPoint"},
        {"name": "BottomRight", "type": "TPoint"}
      ]
    }
  ],
  "functions": [
    {
      "name": "add",
      "params": [
        {"name": "a", "type": "int"},
        {"name": "b", "type": "int"}
      ],
      "return": "int"
    },
    {
      "name": "create_point",
      "params": [
        {"name": "x", "type": "double"},
        {"name": "y", "type": "double"}
      ],
      "return": "TPoint"
    },
    {
      "name": "distance",
      "params": [
        {"name": "p1", "type": "TPoint"},
        {"name": "p2", "type": "TPoint"}
      ],
      "return": "double"
    }
  ]
}
```

### 6.5 Utilisation du générateur

```bash
# Rendre le script exécutable
chmod +x gen_bindings.py

# Générer les bindings
./gen_bindings.py api_spec.json MathLib.pas
```

**Résultat (MathLib.pas)** :
```pascal
unit MathLib;

interface

const
  PI = 3.14159265359;
  E = 2.71828182846;

type
  TPoint = record
    X: Double;
    Y: Double;
  end;

  TRectangle = record
    TopLeft: TPoint;
    BottomRight: TPoint;
  end;

// Fonctions de la bibliothèque
function add(a: Integer; b: Integer): Integer; cdecl; external 'libmath.so';
function create_point(x: Double; y: Double): TPoint; cdecl; external 'libmath.so';
function distance(p1: TPoint; p2: TPoint): Double; cdecl; external 'libmath.so';

implementation

end.
```

---

## 7. Bindings pour bibliothèques C

### 7.1 Exemple complet : SQLite

SQLite est une bibliothèque C très populaire pour les bases de données.

#### Étape 1 : Télécharger le header

```bash
wget https://www.sqlite.org/2024/sqlite-amalgamation-3450000.zip
unzip sqlite-amalgamation-3450000.zip
cd sqlite-amalgamation-3450000
```

#### Étape 2 : Générer les bindings

```bash
h2pas -l libsqlite3.so.0 -u sqlite3.h -o SQLite3.pas
```

#### Étape 3 : Corrections manuelles

```pascal
unit SQLite3;

interface

const
  {$IFDEF WINDOWS}
  SQLite3Lib = 'sqlite3.dll';
  {$ELSE}
  SQLite3Lib = 'libsqlite3.so.0';
  {$ENDIF}

type
  // Types opaques (pointeurs)
  Psqlite3 = Pointer;
  Psqlite3_stmt = Pointer;

  // Type pour callback
  sqlite3_callback = function(
    user_data: Pointer;
    num_cols: Integer;
    values: PPAnsiChar;
    col_names: PPAnsiChar
  ): Integer; cdecl;

// Fonctions principales
function sqlite3_open(
  filename: PAnsiChar;
  out db: Psqlite3
): Integer; cdecl; external SQLite3Lib;

function sqlite3_close(
  db: Psqlite3
): Integer; cdecl; external SQLite3Lib;

function sqlite3_exec(
  db: Psqlite3;
  sql: PAnsiChar;
  callback: sqlite3_callback;
  user_data: Pointer;
  errmsg: PPAnsiChar
): Integer; cdecl; external SQLite3Lib;

function sqlite3_errmsg(
  db: Psqlite3
): PAnsiChar; cdecl; external SQLite3Lib;

// Constantes de codes de retour
const
  SQLITE_OK = 0;
  SQLITE_ERROR = 1;
  SQLITE_ROW = 100;
  SQLITE_DONE = 101;

implementation

end.
```

#### Étape 4 : Wrapper orienté objet

```pascal
unit SQLiteWrapper;

interface

uses
  SQLite3, SysUtils;

type
  ESQLiteException = class(Exception);

  TSQLiteDatabase = class
  private
    FHandle: Psqlite3;
    FFilename: string;
  public
    constructor Create(const Filename: string);
    destructor Destroy; override;

    procedure Execute(const SQL: string);

    property Filename: string read FFilename;
  end;

implementation

constructor TSQLiteDatabase.Create(const Filename: string);
var
  res: Integer;
begin
  inherited Create;
  FFilename := Filename;

  res := sqlite3_open(PAnsiChar(AnsiString(Filename)), FHandle);
  if res <> SQLITE_OK then
    raise ESQLiteException.CreateFmt(
      'Impossible d''ouvrir la base : %s',
      [string(sqlite3_errmsg(FHandle))]
    );
end;

destructor TSQLiteDatabase.Destroy;
begin
  if FHandle <> nil then
    sqlite3_close(FHandle);
  inherited;
end;

procedure TSQLiteDatabase.Execute(const SQL: string);
var
  res: Integer;
  errmsg: PAnsiChar;
begin
  res := sqlite3_exec(
    FHandle,
    PAnsiChar(AnsiString(SQL)),
    nil,
    nil,
    @errmsg
  );

  if res <> SQLITE_OK then
  begin
    try
      raise ESQLiteException.CreateFmt(
        'Erreur SQL : %s',
        [string(errmsg)]
      );
    finally
      sqlite3_free(errmsg);
    end;
  end;
end;

end.
```

#### Étape 5 : Utilisation

```pascal
program TestSQLite;

uses
  SQLiteWrapper;

var
  db: TSQLiteDatabase;
begin
  db := TSQLiteDatabase.Create('test.db');
  try
    // Créer une table
    db.Execute('CREATE TABLE IF NOT EXISTS users (id INTEGER PRIMARY KEY, name TEXT)');

    // Insérer des données
    db.Execute('INSERT INTO users (name) VALUES ("Alice")');
    db.Execute('INSERT INTO users (name) VALUES ("Bob")');

    WriteLn('✓ Base de données créée avec succès');
  finally
    db.Free;
  end;
end.
```

---

## 8. Bindings pour bibliothèques C++

### 8.1 Le défi du C++

Le C++ est plus complexe que le C :
- **Classes** et méthodes
- **Namespaces**
- **Templates**
- **Surcharge** de fonctions
- **Exceptions**

### 8.2 Solution : Wrapper C intermédiaire

L'idée est de créer une couche C qui expose la bibliothèque C++.

#### Étape 1 : Bibliothèque C++ originale

**Calculator.hpp** :
```cpp
#ifndef CALCULATOR_HPP
#define CALCULATOR_HPP

namespace Math {
    class Calculator {
    private:
        int precision;

    public:
        Calculator(int prec = 2);
        ~Calculator();

        double add(double a, double b);
        double multiply(double a, double b);
        int getPrecision() const;
    };
}

#endif
```

**Calculator.cpp** :
```cpp
#include "Calculator.hpp"
#include <cmath>

namespace Math {
    Calculator::Calculator(int prec) : precision(prec) {}

    Calculator::~Calculator() {}

    double Calculator::add(double a, double b) {
        double result = a + b;
        double factor = pow(10, precision);
        return round(result * factor) / factor;
    }

    double Calculator::multiply(double a, double b) {
        double result = a * b;
        double factor = pow(10, precision);
        return round(result * factor) / factor;
    }

    int Calculator::getPrecision() const {
        return precision;
    }
}
```

#### Étape 2 : Wrapper C

**Calculator_c.h** :
```c
#ifndef CALCULATOR_C_H
#define CALCULATOR_C_H

#ifdef __cplusplus
extern "C" {
#endif

// Type opaque pour la classe C++
typedef void* CalculatorHandle;

// Fonctions C wrappant les méthodes C++
CalculatorHandle calculator_create(int precision);
void calculator_destroy(CalculatorHandle handle);
double calculator_add(CalculatorHandle handle, double a, double b);
double calculator_multiply(CalculatorHandle handle, double a, double b);
int calculator_get_precision(CalculatorHandle handle);

#ifdef __cplusplus
}
#endif

#endif
```

**Calculator_c.cpp** :
```cpp
#include "Calculator.hpp"
#include "Calculator_c.h"

extern "C" {

CalculatorHandle calculator_create(int precision) {
    return new Math::Calculator(precision);
}

void calculator_destroy(CalculatorHandle handle) {
    delete static_cast<Math::Calculator*>(handle);
}

double calculator_add(CalculatorHandle handle, double a, double b) {
    Math::Calculator* calc = static_cast<Math::Calculator*>(handle);
    return calc->add(a, b);
}

double calculator_multiply(CalculatorHandle handle, double a, double b) {
    Math::Calculator* calc = static_cast<Math::Calculator*>(handle);
    return calc->multiply(a, b);
}

int calculator_get_precision(CalculatorHandle handle) {
    Math::Calculator* calc = static_cast<Math::Calculator*>(handle);
    return calc->getPrecision();
}

} // extern "C"
```

#### Étape 3 : Compilation du wrapper

**Makefile** :
```makefile
# Variables
CXX = g++
CXXFLAGS = -fPIC -O2 -Wall
LDFLAGS = -shared

# Nom de la bibliothèque
TARGET = libcalculator.so

# Fichiers sources
SOURCES = Calculator.cpp Calculator_c.cpp
OBJECTS = $(SOURCES:.cpp=.o)

# Règle principale
all: $(TARGET)

$(TARGET): $(OBJECTS)
	$(CXX) $(LDFLAGS) -o $@ $^

%.o: %.cpp
	$(CXX) $(CXXFLAGS) -c $<

clean:
	rm -f $(OBJECTS) $(TARGET)

.PHONY: all clean
```

**Compilation** :
```bash
make
# Génère : libcalculator.so
```

#### Étape 4 : Bindings Pascal

Générer avec h2pas :
```bash
h2pas -l libcalculator.so Calculator_c.h -o CalculatorBindings.pas
```

Ou écrire manuellement :

**CalculatorBindings.pas** :
```pascal
unit CalculatorBindings;

interface

const
  {$IFDEF WINDOWS}
  CalcLib = 'calculator.dll';
  {$ELSE}
  CalcLib = 'libcalculator.so';
  {$ENDIF}

type
  TCalculatorHandle = Pointer;

// Fonctions de la bibliothèque C
function calculator_create(precision: Integer): TCalculatorHandle;
  cdecl; external CalcLib;

procedure calculator_destroy(handle: TCalculatorHandle);
  cdecl; external CalcLib;

function calculator_add(handle: TCalculatorHandle; a, b: Double): Double;
  cdecl; external CalcLib;

function calculator_multiply(handle: TCalculatorHandle; a, b: Double): Double;
  cdecl; external CalcLib;

function calculator_get_precision(handle: TCalculatorHandle): Integer;
  cdecl; external CalcLib;

implementation

end.
```

#### Étape 5 : Wrapper Pascal orienté objet

**Calculator.pas** :
```pascal
unit Calculator;

interface

uses
  CalculatorBindings, SysUtils;

type
  ECalculatorException = class(Exception);

  TCalculator = class
  private
    FHandle: TCalculatorHandle;
    function GetPrecision: Integer;
  public
    constructor Create(Precision: Integer = 2);
    destructor Destroy; override;

    function Add(A, B: Double): Double;
    function Multiply(A, B: Double): Double;

    property Precision: Integer read GetPrecision;
  end;

implementation

constructor TCalculator.Create(Precision: Integer);
begin
  inherited Create;
  FHandle := calculator_create(Precision);

  if FHandle = nil then
    raise ECalculatorException.Create('Impossible de créer le calculateur');
end;

destructor TCalculator.Destroy;
begin
  if FHandle <> nil then
    calculator_destroy(FHandle);
  inherited;
end;

function TCalculator.GetPrecision: Integer;
begin
  if FHandle = nil then
    raise ECalculatorException.Create('Calculateur non initialisé');

  Result := calculator_get_precision(FHandle);
end;

function TCalculator.Add(A, B: Double): Double;
begin
  if FHandle = nil then
    raise ECalculatorException.Create('Calculateur non initialisé');

  Result := calculator_add(FHandle, A, B);
end;

function TCalculator.Multiply(A, B: Double): Double;
begin
  if FHandle = nil then
    raise ECalculatorException.Create('Calculateur non initialisé');

  Result := calculator_multiply(FHandle, A, B);
end;

end.
```

#### Étape 6 : Utilisation finale

**TestCalculator.pas** :
```pascal
program TestCalculator;

{$mode objfpc}{$H+}

uses
  Calculator, SysUtils;

var
  calc: TCalculator;
begin
  WriteLn('=== Test du calculateur C++ depuis Pascal ===');
  WriteLn;

  // Créer un calculateur avec précision de 3 décimales
  calc := TCalculator.Create(3);
  try
    WriteLn('Précision : ', calc.Precision, ' décimales');
    WriteLn;

    WriteLn('Tests des opérations :');
    WriteLn('  10.12345 + 5.67890 = ', calc.Add(10.12345, 5.67890):0:3);
    WriteLn('  3.14159 × 2.71828 = ', calc.Multiply(3.14159, 2.71828):0:3);
    WriteLn;

    WriteLn('✓ Tests réussis');
  finally
    calc.Free;
  end;

  WriteLn;
  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
```

**Compilation et exécution** :
```bash
# Compiler
fpc TestCalculator.pas

# Exécuter
./TestCalculator
```

---

## 9. Cas pratiques

### 9.1 Cas 1 : Pipeline automatisé complet

**Objectif** : Créer un système qui génère automatiquement les bindings à chaque nouvelle version de la bibliothèque.

#### Structure du projet

```
bindings-project/
├── upstream/
│   └── library-v1.2.3/
│       ├── include/
│       │   └── library.h
│       └── lib/
│           └── liblibrary.so
├── generator/
│   ├── generate.py
│   └── postprocess.py
├── bindings/
│   └── LibraryBindings.pas
├── tests/
│   └── test_bindings.pas
├── scripts/
│   ├── build.sh
│   └── update.sh
└── README.md
```

#### Script de build automatisé

**scripts/build.sh** :
```bash
#!/bin/bash
set -e

echo "=== Génération automatique des bindings ==="

# Configuration
VERSION="1.2.3"
HEADER="upstream/library-v${VERSION}/include/library.h"
OUTPUT="bindings/LibraryBindings.pas"
LIBRARY="liblibrary.so.${VERSION}"

# Vérifier que le header existe
if [ ! -f "$HEADER" ]; then
    echo "❌ Erreur : Header non trouvé : $HEADER"
    exit 1
fi

# Étape 1 : Générer avec h2pas
echo "1. Génération avec h2pas..."
h2pas -l "$LIBRARY" -u "$HEADER" -o "${OUTPUT}.tmp"

# Étape 2 : Post-traitement
echo "2. Post-traitement..."
python3 generator/postprocess.py "${OUTPUT}.tmp" "$OUTPUT"

# Étape 3 : Vérifier la compilation
echo "3. Vérification de la compilation..."
fpc -Sc "$OUTPUT"

# Étape 4 : Exécuter les tests
echo "4. Exécution des tests..."
fpc tests/test_bindings.pas -Fu"bindings"
./tests/test_bindings

# Nettoyage
rm -f "${OUTPUT}.tmp"

echo ""
echo "✓ Bindings générés et testés avec succès"
echo "   Fichier : $OUTPUT"
```

#### Script de post-traitement

**generator/postprocess.py** :
```python
#!/usr/bin/env python3
"""
Post-traitement des bindings générés par h2pas
"""

import sys
import re

def postprocess(input_file, output_file):
    """Nettoyer et améliorer les bindings"""

    with open(input_file, 'r') as f:
        content = f.read()

    # Liste des transformations
    transformations = [
        # Types
        (r'\blongint\b', 'Integer'),
        (r'\bdword\b', 'Cardinal'),
        (r'\bword\b', 'Word'),

        # Pointeurs
        (r'Plongint', 'PInteger'),
        (r'Pdouble', 'PDouble'),

        # Constantes
        (r'\bnil\b', 'nil'),
    ]

    # Appliquer les transformations
    for pattern, replacement in transformations:
        content = re.sub(pattern, replacement, content)

    # Ajouter les directives de compilation conditionnelle
    multiplatform_header = """
{$IFDEF WINDOWS}
const
  LibraryName = 'library.dll';
{$ELSE}
  {$IFDEF DARWIN}
  const
    LibraryName = 'liblibrary.dylib';
  {$ELSE}
  const
    LibraryName = 'liblibrary.so';
  {$ENDIF}
{$ENDIF}

"""

    # Insérer après "interface"
    content = content.replace(
        'interface\n',
        'interface\n' + multiplatform_header
    )

    # Remplacer les external par external LibraryName
    content = re.sub(
        r"external '[^']*'",
        'external LibraryName',
        content
    )

    # Écrire le résultat
    with open(output_file, 'w') as f:
        f.write(content)

    print(f"✓ Post-traitement terminé : {output_file}")

if __name__ == '__main__':
    if len(sys.argv) < 3:
        print("Usage: postprocess.py input.pas output.pas")
        sys.exit(1)

    postprocess(sys.argv[1], sys.argv[2])
```

#### Tests automatiques

**tests/test_bindings.pas** :
```pascal
program TestBindings;

{$mode objfpc}{$H+}

uses
  SysUtils, LibraryBindings;

var
  TestsPassed: Integer = 0;
  TestsFailed: Integer = 0;

procedure Test(const Name: string; Condition: Boolean);
begin
  Write('  ', Name, ' ... ');
  if Condition then
  begin
    WriteLn('✓ OK');
    Inc(TestsPassed);
  end
  else
  begin
    WriteLn('✗ ÉCHEC');
    Inc(TestsFailed);
  end;
end;

procedure TestBasicFunctions;
var
  result: Integer;
begin
  WriteLn('Test : Fonctions de base');

  result := library_add(10, 20);
  Test('Addition', result = 30);

  result := library_multiply(5, 6);
  Test('Multiplication', result = 30);
end;

procedure TestMemoryManagement;
var
  ptr: Pointer;
begin
  WriteLn;
  WriteLn('Test : Gestion mémoire');

  ptr := library_allocate(1024);
  Test('Allocation', ptr <> nil);

  if ptr <> nil then
  begin
    library_free(ptr);
    Test('Libération', True);
  end;
end;

begin
  WriteLn('=== Suite de tests des bindings ===');
  WriteLn;

  try
    TestBasicFunctions;
    TestMemoryManagement;

    WriteLn;
    WriteLn('=== Résultats ===');
    WriteLn('Tests réussis : ', TestsPassed);
    WriteLn('Tests échoués : ', TestsFailed);

    if TestsFailed = 0 then
    begin
      WriteLn;
      WriteLn('✓ Tous les tests ont réussi');
      ExitCode := 0;
    end
    else
    begin
      WriteLn;
      WriteLn('✗ Certains tests ont échoué');
      ExitCode := 1;
    end;
  except
    on E: Exception do
    begin
      WriteLn;
      WriteLn('✗ ERREUR : ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
```

#### Script de mise à jour

**scripts/update.sh** :
```bash
#!/bin/bash
set -e

echo "=== Vérification des mises à jour ==="

# URL de la dernière version
LATEST_URL="https://example.com/library/latest"
CURRENT_VERSION=$(cat version.txt)

# Télécharger les informations de la dernière version
LATEST_VERSION=$(curl -s "$LATEST_URL/version.txt")

if [ "$LATEST_VERSION" = "$CURRENT_VERSION" ]; then
    echo "✓ Déjà à jour (version $CURRENT_VERSION)"
    exit 0
fi

echo "Nouvelle version disponible : $LATEST_VERSION"
echo "Version actuelle : $CURRENT_VERSION"
echo ""

# Télécharger la nouvelle version
echo "Téléchargement de la version $LATEST_VERSION..."
wget "$LATEST_URL/library-v${LATEST_VERSION}.tar.gz"
tar -xzf "library-v${LATEST_VERSION}.tar.gz" -C upstream/
rm "library-v${LATEST_VERSION}.tar.gz"

# Mettre à jour le numéro de version
echo "$LATEST_VERSION" > version.txt

# Régénérer les bindings
echo ""
./scripts/build.sh

echo ""
echo "✓ Mise à jour terminée vers la version $LATEST_VERSION"
```

### 9.2 Cas 2 : CI/CD avec GitHub Actions

**Automatiser avec GitHub Actions** :

**.github/workflows/generate-bindings.yml** :
```yaml
name: Generate Bindings

on:
  schedule:
    # Vérifier les mises à jour tous les jours à minuit
    - cron: '0 0 * * *'
  workflow_dispatch:
  push:
    branches:
      - main
    paths:
      - 'upstream/**'

jobs:
  generate:
    runs-on: ubuntu-latest

    steps:
    - name: Checkout code
      uses: actions/checkout@v3

    - name: Install FreePascal
      run: |
        sudo apt-get update
        sudo apt-get install -y fpc

    - name: Install dependencies
      run: |
        sudo apt-get install -y python3 wget

    - name: Check for updates
      id: check
      run: |
        ./scripts/update.sh || echo "no_update=true" >> $GITHUB_OUTPUT

    - name: Generate bindings
      if: steps.check.outputs.no_update != 'true'
      run: |
        chmod +x scripts/build.sh
        ./scripts/build.sh

    - name: Run tests
      if: steps.check.outputs.no_update != 'true'
      run: |
        cd tests
        fpc test_bindings.pas
        ./test_bindings

    - name: Create Pull Request
      if: steps.check.outputs.no_update != 'true'
      uses: peter-evans/create-pull-request@v5
      with:
        title: "Update bindings"
        body: |
          Bindings automatiquement mis à jour

          - Nouvelle version de la bibliothèque détectée
          - Bindings régénérés et testés
          - Prêt pour review
        branch: auto-update-bindings
        commit-message: "chore: update bindings"
```

---

## 10. Bonnes pratiques

### 10.1 Documentation des bindings

**Toujours documenter** :

```pascal
unit MyLibBindings;

{**
 * Bindings FreePascal pour MyLib v1.2.3
 *
 * Bibliothèque source : https://github.com/example/mylib
 * Documentation : https://mylib.readthedocs.io
 *
 * Généré automatiquement le : 2024-01-15
 * Outil utilisé : h2pas v3.2.2 + post-processing
 *
 * @author Votre Nom <email@example.com>
 * @license MIT
 * @version 1.2.3
 *}

interface

{**
 * Crée une nouvelle instance
 *
 * @param precision Nombre de décimales (1-10)
 * @return Handle vers l'objet créé, ou nil en cas d'erreur
 * @note L'objet doit être libéré avec destroy()
 *}
function create(precision: Integer): Pointer; cdecl; external;
```

### 10.2 Versioning et compatibilité

```pascal
const
  // Version des bindings
  MYLIB_BINDING_VERSION = '1.2.3';
  MYLIB_BINDING_DATE = '2024-01-15';

  // Version minimale de la bibliothèque requise
  MYLIB_MIN_VERSION = '1.2.0';
  MYLIB_MAX_VERSION = '1.9.9';

// Vérification à l'initialisation
initialization
  CheckLibraryVersion;
```

### 10.3 Gestion des erreurs

**Wrapper sécurisé** :

```pascal
unit SafeBindings;

interface

uses
  MyLibBindings, SysUtils;

type
  EMyLibException = class(Exception);

  TSafeWrapper = class
  private
    FHandle: Pointer;
  public
    constructor Create;
    destructor Destroy; override;

    function SafeOperation(Value: Integer): Integer;
  end;

implementation

constructor TSafeWrapper.Create;
begin
  inherited Create;
  FHandle := mylib_create();

  if FHandle = nil then
    raise EMyLibException.Create(
      'Échec de l''initialisation. Vérifiez que la bibliothèque est installée.'
    );
end;

destructor TSafeWrapper.Destroy;
begin
  if FHandle <> nil then
  begin
    try
      mylib_destroy(FHandle);
    except
      // Logger mais ne pas propager depuis le destructeur
    end;
  end;
  inherited;
end;

function TSafeWrapper.SafeOperation(Value: Integer): Integer;
begin
  if FHandle = nil then
    raise EMyLibException.Create('Objet non initialisé');

  try
    Result := mylib_operation(FHandle, Value);
  except
    on E: Exception do
      raise EMyLibException.CreateFmt(
        'Erreur lors de l''opération : %s',
        [E.Message]
      );
  end;
end;

end.
```

### 10.4 Tests exhaustifs

```pascal
program ComprehensiveTests;

{$mode objfpc}{$H+}

uses
  SysUtils, MyLibBindings;

type
  TTestResult = record
    Name: string;
    Passed: Boolean;
    Message: string;
  end;

var
  Results: array of TTestResult;

procedure AddTest(const Name: string; Passed: Boolean; const Msg: string = '');
var
  idx: Integer;
begin
  idx := Length(Results);
  SetLength(Results, idx + 1);
  Results[idx].Name := Name;
  Results[idx].Passed := Passed;
  Results[idx].Message := Msg;
end;

procedure TestBasics;
begin
  AddTest('Addition', mylib_add(2, 3) = 5);
  AddTest('Soustraction', mylib_sub(10, 3) = 7);
end;

procedure TestEdgeCases;
begin
  AddTest('Division par zéro', mylib_div(10, 0) = 0, 'Devrait retourner 0');
  AddTest('Valeur maximale', mylib_add(MaxInt, 1) = MaxInt, 'Devrait saturer');
end;

procedure TestMemoryLeaks;
var
  i: Integer;
  ptr: Pointer;
begin
  for i := 1 to 10000 do
  begin
    ptr := mylib_allocate(1024);
    mylib_free(ptr);
  end;
  AddTest('Pas de fuite mémoire', True, '10000 allocations/libérations');
end;

procedure ShowResults;
var
  i, passed, failed: Integer;
begin
  passed := 0;
  failed := 0;

  WriteLn('=== Résultats des tests ===');
  WriteLn;

  for i := 0 to High(Results) do
  begin
    Write('  ', Results[i].Name, ' ... ');
    if Results[i].Passed then
    begin
      WriteLn('✓ OK');
      Inc(passed);
    end
    else
    begin
      WriteLn('✗ ÉCHEC');
      if Results[i].Message <> '' then
        WriteLn('    ', Results[i].Message);
      Inc(failed);
    end;
  end;

  WriteLn;
  WriteLn('Total : ', passed + failed, ' tests');
  WriteLn('Réussis : ', passed);
  WriteLn('Échoués : ', failed);

  if failed = 0 then
  begin
    WriteLn;
    WriteLn('✓ Tous les tests ont réussi');
    ExitCode := 0;
  end
  else
  begin
    WriteLn;
    WriteLn('✗ ', failed, ' test(s) ont échoué');
    ExitCode := 1;
  end;
end;

begin
  WriteLn('=== Tests exhaustifs des bindings ===');
  WriteLn;

  TestBasics;
  TestEdgeCases;
  TestMemoryLeaks;

  WriteLn;
  ShowResults;
end.
```

### 10.5 Multiplateforme

```pascal
unit CrossPlatformBindings;

interface

{$IFDEF WINDOWS}
  {$IFDEF CPU64}
    const LibName = 'mylib64.dll';
  {$ELSE}
    const LibName = 'mylib32.dll';
  {$ENDIF}
{$ELSE}
  {$IFDEF DARWIN}
    const LibName = 'libmylib.dylib';
  {$ELSE}
    {$IFDEF CPU64}
      const LibName = 'libmylib.so.1';
    {$ELSE}
      const LibName = 'libmylib.so.1';
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

function mylib_init: Integer; cdecl; external LibName;

implementation

end.
```

### 10.6 Checklist de qualité

**Avant de publier des bindings** :

- [ ] **Documentation complète**
  - [ ] En-tête avec version et licence
  - [ ] Commentaires pour chaque fonction
  - [ ] Exemples d'utilisation

- [ ] **Tests**
  - [ ] Tests unitaires pour fonctions principales
  - [ ] Tests de cas limites
  - [ ] Tests de gestion mémoire
  - [ ] Tests sur toutes les plateformes cibles

- [ ] **Compatibilité**
  - [ ] Windows 32/64-bit
  - [ ] Linux 32/64-bit
  - [ ] macOS (si applicable)

- [ ] **Gestion d'erreurs**
  - [ ] Vérification des handles nil
  - [ ] Exceptions documentées
  - [ ] Messages d'erreur clairs

- [ ] **Performance**
  - [ ] Pas d'allocations inutiles
  - [ ] Appels directs quand possible

- [ ] **Distribution**
  - [ ] README avec instructions d'installation
  - [ ] Exemples de code
  - [ ] Liste des dépendances
  - [ ] Changelog

---

## 11. Conclusion

### 11.1 Résumé

Nous avons exploré la génération automatique de bindings :

1. **Concepts** : Comprendre les bindings et leur importance
2. **h2pas** : Outil principal pour générer des bindings C
3. **SWIG** : Alternative pour projets multilangage
4. **Générateurs personnalisés** : Scripts Python pour besoins spécifiques
5. **Bindings C** : Exemple complet avec SQLite
6. **Bindings C++** : Technique du wrapper C intermédiaire
7. **Cas pratiques** : Pipelines automatisés et CI/CD
8. **Bonnes pratiques** : Documentation, tests, multiplateforme

### 11.2 Gains mesurables

**Comparaison Manuel vs Automatique** :

| Métrique | Manuel | Automatique | Gain |
|----------|--------|-------------|------|
| **Temps initial** | 2-3 semaines | 1-2 jours | **90%** |
| **Maintenance** | Difficile | Facile | **85%** |
| **Erreurs** | Fréquentes | Rares | **95%** |
| **Cohérence** | Variable | Uniforme | **100%** |

**Exemple concret** :
```
Bibliothèque avec 500 fonctions :
├─ Écriture manuelle : 120 heures
├─ h2pas + corrections : 12 heures
└─ Générateur custom : 8 heures (après setup initial)

Gain de temps : 90%+
```

### 11.3 Quand automatiser ?

**✓ Automatisez dans ces cas** :
- Bibliothèque avec plus de 50 fonctions
- Mises à jour fréquentes de la bibliothèque
- Headers C bien structurés
- Projet à long terme nécessitant maintenance
- Besoin de bindings pour plusieurs langages

**✗ Préférez le manuel dans ces cas** :
- Moins de 20 fonctions
- API très simple et stable
- Besoin de contrôle total et optimisations fines
- Prototype rapide ou proof-of-concept
- Headers C très mal structurés

### 11.4 Recommandations finales

**Pour débuter** :
1. Commencez avec **h2pas** pour des headers C simples
2. Apprenez le **post-traitement** avec des scripts bash/Python
3. Créez des **wrappers orientés objet** pour faciliter l'utilisation
4. Écrivez des **tests unitaires** pour valider

**Pour aller plus loin** :
1. Explorez **libclang** pour parser C/C++ complexe
2. Créez un **générateur personnalisé** adapté à vos besoins
3. Intégrez dans **pipeline CI/CD** pour automatisation complète
4. **Contribuez** des bindings à la communauté FreePascal

**Outils et ressources** :
- Documentation h2pas : `man h2pas`
- Wiki FreePascal : https://wiki.freepascal.org/
- Forums : https://forum.lazarus.freepascal.org/
- SWIG : http://www.swig.org/
- Packages existants : cherchez avant de réinventer !

### 11.5 L'avenir des bindings

**Tendances émergentes** :
- **IA et génération** : ChatGPT/Claude pour aide au binding
- **Meilleurs outils** : h2pas en constante amélioration
- **Standards FFI** : Interfaces de fonctions étrangères universelles
- **WebAssembly** : Nouvelle cible pour bindings cross-platform

**Dans FreePascal** :
- Support C++ amélioré
- Meilleure intégration LLVM
- Outils natifs plus puissants

### 11.6 Mot de la fin

La génération automatique de bindings est un **multiplicateur de productivité** essentiel. Elle permet d'accéder à l'immense écosystème des bibliothèques C/C++ sans le coût prohibitif de l'écriture manuelle.

**Points clés à retenir** :

🚀 **Automatisez** quand c'est possible - vous gagnerez des semaines

🔧 **Post-traitez** pour améliorer la qualité - les outils ne sont pas parfaits

📝 **Documentez** bien - les autres développeurs vous remercieront

🧪 **Testez** exhaustivement - les bindings sont critiques

🔄 **Maintenez** avec des pipelines automatisés - la bibliothèque C évoluera

💡 **Partagez** vos bindings - contribuez à la communauté

**Le meilleur binding est celui qui fonctionne parfaitement et que vous n'avez pas eu à écrire manuellement !**

Bonne génération de bindings avec FreePascal ! 🎯🚀

---

**Fin du tutoriel 19.10 - Génération automatique de bindings**

⏭️ [FFI (Foreign Function Interface)](/19-interoperabilite-bindings/11-ffi-foreign-function-interface.md)
