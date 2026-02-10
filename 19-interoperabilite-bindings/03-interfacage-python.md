🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 19.3 Interfaçage avec Python

## Introduction

Python est l'un des langages les plus populaires au monde, particulièrement dans les domaines de la data science, du machine learning, de l'automatisation et du scripting. FreePascal peut interagir avec Python de deux façons principales :

1. **Appeler Python depuis FreePascal** : Intégrer un interpréteur Python dans votre application Pascal
2. **Appeler FreePascal depuis Python** : Créer des bibliothèques Pascal utilisables depuis Python

Cette bidirectionnalité ouvre des possibilités immenses pour combiner la performance de Pascal avec l'écosystème riche de Python.

### Pourquoi interfacer FreePascal et Python ?

#### Avantages pour FreePascal

- **Accès à l'écosystème Python** : NumPy, Pandas, TensorFlow, scikit-learn, etc.
- **Scripting dynamique** : Permettre aux utilisateurs d'étendre votre application
- **Prototypage rapide** : Tester des algorithmes en Python avant de les implémenter en Pascal
- **Intégration avec des outils existants** : Utiliser des scripts Python déjà développés

#### Avantages pour Python

- **Performance** : Code critique optimisé en Pascal compilé
- **Accès bas niveau** : Gestion fine de la mémoire et des ressources
- **Réutilisation** : Utiliser du code Pascal legacy
- **Déploiement** : Créer des extensions sans dépendance C/C++

### Les différentes approches

```
┌─────────────────────────────────────────────────────────┐
│           Approches d'interfaçage                       │
├─────────────────────────────────────────────────────────┤
│                                                         │
│  1. Python depuis Pascal (Embedding)                   │
│     ┌──────────────┐                                   │
│     │ Application  │                                   │
│     │   Pascal     │                                   │
│     │              │                                   │
│     │  ┌────────┐  │                                   │
│     │  │ Python │  │  ← Interpréteur embarqué         │
│     │  │Engine  │  │                                   │
│     │  └────────┘  │                                   │
│     └──────────────┘                                   │
│                                                         │
│  2. Pascal depuis Python (Extension)                   │
│     ┌──────────────┐                                   │
│     │   Python     │                                   │
│     │  Programme   │                                   │
│     │              │                                   │
│     │ import mylib │  → Bibliothèque Pascal           │
│     └──────────────┘                                   │
│                                                         │
│  3. Communication inter-processus                      │
│     ┌─────────┐         ┌─────────┐                   │
│     │ Pascal  │ ←────→  │ Python  │                   │
│     │ Process │  JSON   │ Process │                   │
│     └─────────┘  Socket └─────────┘                   │
│                                                         │
└─────────────────────────────────────────────────────────┘
```

## Approche 1 : Appeler Python depuis FreePascal (Embedding)

### Comprendre l'embedding Python

L'**embedding** consiste à intégrer un interpréteur Python complet dans votre application FreePascal. Votre application devient l'hôte, et Python est l'invité.

**Cas d'usage** :
- Système de plugins Python dans une application Pascal
- Console Python intégrée pour le débogage
- Automatisation scriptable par l'utilisateur
- Utilisation ponctuelle de bibliothèques Python

### Installation des prérequis

#### Sous Windows

1. **Installer Python** :
```bash
# Télécharger depuis python.org ou via chocolatey
choco install python
```

2. **Localiser les fichiers** :
```
C:\Python311\
├── python311.dll
├── include\
│   └── Python.h
└── libs\
    └── python311.lib
```

3. **Variables d'environnement** (optionnel) :
```
PYTHONHOME=C:\Python311
PATH=%PATH%;C:\Python311
```

#### Sous Ubuntu/Linux

1. **Installer Python et les fichiers de développement** :
```bash
sudo apt update
sudo apt install python3 python3-dev
```

2. **Vérifier l'installation** :
```bash
python3 --version
python3-config --includes
python3-config --ldflags
```

3. **Localiser les fichiers** :
```
/usr/include/python3.11/
├── Python.h
└── ...

/usr/lib/x86_64-linux-gnu/
├── libpython3.11.so
└── ...
```

### Package Python4Delphi/Python4Lazarus

Le moyen le plus simple d'interfacer Python est d'utiliser **Python4Delphi** (P4D), compatible avec FreePascal/Lazarus.

#### Installation via Lazarus

1. **Télécharger Python4Delphi** :
```bash
git clone https://github.com/pyscripter/python4delphi.git
```

2. **Dans Lazarus** :
   - Package → Open Package File (.lpk)
   - Naviguer vers `python4delphi/Source/PythonForDelphi.lpk`
   - Compile → Use → Install
   - Redémarrer Lazarus

3. **Composants disponibles** :
   - `TPythonEngine` : Moteur Python principal
   - `TPythonInputOutput` : Gestion des entrées/sorties
   - `TPythonModule` : Créer des modules Python
   - `TPythonType` : Définir des types Python personnalisés

### Premier exemple : Exécuter du code Python

```pascal
program HelloPython;

{$mode objfpc}{$H+}

uses
  SysUtils, PythonEngine;

var
  PythonEngine: TPythonEngine;
begin
  // Créer le moteur Python
  PythonEngine := TPythonEngine.Create(nil);
  try
    // Configuration
    PythonEngine.AutoLoad := True;
    PythonEngine.AutoFinalize := True;

    // Charger Python
    PythonEngine.LoadDll;

    // Exécuter du code Python simple
    PythonEngine.ExecString('print("Hello from Python!")');
    PythonEngine.ExecString('x = 2 + 2');
    PythonEngine.ExecString('print(f"2 + 2 = {x}")');

    // Exécuter du code multi-lignes
    PythonEngine.ExecStrings([
      'def saluer(nom):',
      '    return f"Bonjour, {nom}!"',
      '',
      'print(saluer("FreePascal"))'
    ]);

  finally
    PythonEngine.Free;
  end;

  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
```

**Sortie** :
```
Hello from Python!
2 + 2 = 4
Bonjour, FreePascal!
Appuyez sur Entrée pour quitter...
```

### Échange de données entre Pascal et Python

#### Passer des variables à Python

```pascal
program PasserVariables;

{$mode objfpc}{$H+}

uses
  SysUtils, PythonEngine, Variants;

var
  Engine: TPythonEngine;
  MainModule: Variant;
  nombre: Integer;
  texte: string;
  resultat: Variant;
begin
  Engine := TPythonEngine.Create(nil);
  try
    Engine.LoadDll;

    // Obtenir le module __main__
    MainModule := Engine.GetMainModule;

    // Définir des variables Python depuis Pascal
    nombre := 42;
    texte := 'FreePascal';

    MainModule.nombre := nombre;
    MainModule.texte := texte;

    // Utiliser ces variables en Python
    Engine.ExecString('print(f"{texte} a le nombre {nombre}")');

    // Faire des calculs en Python
    Engine.ExecString('resultat = nombre * 2');

    // Récupérer le résultat
    resultat := MainModule.resultat;
    WriteLn('Résultat récupéré : ', Integer(resultat));

  finally
    Engine.Free;
  end;

  ReadLn;
end.
```

#### Appeler des fonctions Python

```pascal
program AppelerFonctions;

{$mode objfpc}{$H+}

uses
  SysUtils, PythonEngine, Variants;

var
  Engine: TPythonEngine;
  MainModule: Variant;
  MaFonction: Variant;
  Resultat: Variant;
begin
  Engine := TPythonEngine.Create(nil);
  try
    Engine.LoadDll;
    MainModule := Engine.GetMainModule;

    // Définir une fonction Python
    Engine.ExecStrings([
      'def calculer_surface(longueur, largeur):',
      '    """Calcule la surface d''un rectangle"""',
      '    return longueur * largeur'
    ]);

    // Obtenir la fonction
    MaFonction := MainModule.calculer_surface;

    // Appeler la fonction avec des paramètres
    Resultat := MaFonction(10, 5);
    WriteLn('Surface : ', Double(Resultat));

    // Autre exemple : fonction mathématique
    Engine.ExecStrings([
      'import math',
      'def calculer_cercle(rayon):',
      '    aire = math.pi * rayon ** 2',
      '    circonference = 2 * math.pi * rayon',
      '    return {"aire": aire, "circonference": circonference}'
    ]);

    Resultat := MainModule.calculer_cercle(5.0);
    WriteLn('Aire du cercle : ', Double(Resultat.aire):0:2);
    WriteLn('Circonférence : ', Double(Resultat.circonference):0:2);

  finally
    Engine.Free;
  end;

  ReadLn;
end.
```

### Utiliser des bibliothèques Python (NumPy, Pandas, etc.)

```pascal
program UtiliserNumPy;

{$mode objfpc}{$H+}

uses
  SysUtils, PythonEngine, Variants;

var
  Engine: TPythonEngine;
  MainModule: Variant;
  Tableau: Variant;
  Moyenne, EcartType: Double;
begin
  Engine := TPythonEngine.Create(nil);
  try
    Engine.LoadDll;
    MainModule := Engine.GetMainModule;

    // Vérifier si NumPy est disponible
    try
      Engine.ExecString('import numpy as np');
      WriteLn('NumPy est disponible');
    except
      on E: Exception do
      begin
        WriteLn('Erreur : NumPy n''est pas installé');
        WriteLn('Installez-le avec : pip install numpy');
        Exit;
      end;
    end;

    // Créer un tableau NumPy
    Engine.ExecStrings([
      'donnees = np.array([1, 2, 3, 4, 5, 6, 7, 8, 9, 10])',
      'moyenne = float(np.mean(donnees))',
      'ecart_type = float(np.std(donnees))'
    ]);

    // Récupérer les résultats
    Moyenne := MainModule.moyenne;
    EcartType := MainModule.ecart_type;

    WriteLn('Moyenne : ', Moyenne:0:2);
    WriteLn('Écart-type : ', EcartType:0:2);

    // Opérations matricielles
    Engine.ExecStrings([
      'matrice_a = np.array([[1, 2], [3, 4]])',
      'matrice_b = np.array([[5, 6], [7, 8]])',
      'produit = np.dot(matrice_a, matrice_b)',
      'print("Produit matriciel :")',
      'print(produit)'
    ]);

  finally
    Engine.Free;
  end;

  ReadLn;
end.
```

### Gérer les erreurs Python

```pascal
program GestionErreurs;

{$mode objfpc}{$H+}

uses
  SysUtils, PythonEngine, Variants;

var
  Engine: TPythonEngine;

procedure ExecuterCodeAvecGestion(const Code: string);
begin
  try
    Engine.ExecString(Code);
  except
    on E: EPythonError do
    begin
      WriteLn('Erreur Python détectée :');
      WriteLn('  Message : ', E.Message);

      // Afficher la traceback complète
      if Engine.Traceback.Text <> '' then
      begin
        WriteLn('  Traceback :');
        WriteLn(Engine.Traceback.Text);
      end;
    end;
    on E: Exception do
      WriteLn('Autre erreur : ', E.Message);
  end;
end;

begin
  Engine := TPythonEngine.Create(nil);
  try
    Engine.LoadDll;

    // Cas 1 : Erreur de syntaxe
    WriteLn('Test 1 : Erreur de syntaxe');
    ExecuterCodeAvecGestion('print("test"');
    WriteLn;

    // Cas 2 : Division par zéro
    WriteLn('Test 2 : Division par zéro');
    ExecuterCodeAvecGestion('x = 10 / 0');
    WriteLn;

    // Cas 3 : Variable non définie
    WriteLn('Test 3 : Variable non définie');
    ExecuterCodeAvecGestion('print(variable_inexistante)');
    WriteLn;

    // Cas 4 : Import échoué
    WriteLn('Test 4 : Module inexistant');
    ExecuterCodeAvecGestion('import module_qui_nexiste_pas');
    WriteLn;

    // Code valide
    WriteLn('Test 5 : Code valide');
    ExecuterCodeAvecGestion('print("Tout va bien !")');

  finally
    Engine.Free;
  end;

  ReadLn;
end.
```

### Rediriger la sortie Python

```pascal
program RedirigerSortie;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, PythonEngine;

var
  Engine: TPythonEngine;
  Output: TPythonInputOutput;
  Sortie: TStringList;

procedure SurSortie(Sender: TObject; const S: string);
begin
  WriteLn('[PYTHON] ', S);
  Sortie.Add(S);
end;

begin
  Engine := TPythonEngine.Create(nil);
  Output := TPythonInputOutput.Create(nil);
  Sortie := TStringList.Create;
  try
    // Configuration
    Engine.LoadDll;
    Output.OnSendData := @SurSortie;

    // Lier la sortie
    Engine.IO := Output;

    // Exécuter du code Python
    Engine.ExecStrings([
      'print("Ligne 1")',
      'print("Ligne 2")',
      'for i in range(5):',
      '    print(f"Compteur : {i}")'
    ]);

    WriteLn;
    WriteLn('Contenu capturé :');
    WriteLn(Sortie.Text);

  finally
    Sortie.Free;
    Output.Free;
    Engine.Free;
  end;

  ReadLn;
end.
```

### Exemple avancé : Application avec console Python intégrée

```pascal
program ConsolePython;

{$mode objfpc}{$H+}

uses
  SysUtils, PythonEngine, Variants;

var
  Engine: TPythonEngine;
  MainModule: Variant;
  Commande: string;
  Continuer: Boolean;

procedure InitialiserEnvironnement;
begin
  Engine.ExecStrings([
    'import sys',
    'import math',
    'import random',
    '',
    'def aide():',
    '    print("Commandes disponibles :")',
    '    print("  aide() - Affiche cette aide")',
    '    print("  quit() ou exit() - Quitter")',
    '    print("  Toute expression Python valide")',
    '',
    'print("Console Python intégrée dans FreePascal")',
    'print("Tapez aide() pour obtenir de l''aide")',
    'print(f"Python {sys.version}")',
    'print()'
  ]);
end;

begin
  Engine := TPythonEngine.Create(nil);
  try
    Engine.LoadDll;
    MainModule := Engine.GetMainModule;

    InitialiserEnvironnement;

    Continuer := True;
    while Continuer do
    begin
      Write('>>> ');
      ReadLn(Commande);

      if (Commande = 'quit()') or (Commande = 'exit()') then
      begin
        Continuer := False;
        Continue;
      end;

      if Trim(Commande) = '' then
        Continue;

      try
        Engine.ExecString(Commande);
      except
        on E: EPythonError do
          WriteLn('Erreur : ', E.Message);
      end;
    end;

    WriteLn('Au revoir !');

  finally
    Engine.Free;
  end;
end.
```

## Approche 2 : Appeler FreePascal depuis Python (Extension)

### Créer une bibliothèque utilisable depuis Python

La méthode la plus simple est de créer une bibliothèque partagée (.dll/.so) avec une interface C, puis de l'utiliser via **ctypes**.

#### Bibliothèque Pascal

```pascal
library mathpas;

{$mode objfpc}{$H+}

uses
  SysUtils, Math;

// Types pour gérer les erreurs
var
  LastError: Integer = 0;
  LastErrorMsg: string = '';

const
  SUCCESS = 0;
  ERROR_INVALID_PARAMETER = 1;
  ERROR_DIVISION_BY_ZERO = 2;

// Fonction simple
function Additionner(a, b: Double): Double; cdecl;
begin
  LastError := SUCCESS;
  Result := a + b;
end;

// Fonction avec gestion d'erreur
function Diviser(a, b: Double; out resultat: Double): Integer; cdecl;
begin
  LastError := SUCCESS;
  LastErrorMsg := '';

  if b = 0 then
  begin
    LastError := ERROR_DIVISION_BY_ZERO;
    LastErrorMsg := 'Division par zéro';
    resultat := 0;
    Result := LastError;
    Exit;
  end;

  resultat := a / b;
  Result := SUCCESS;
end;

// Fonction mathématique avancée
function CalculerFactorielle(n: Integer): Int64; cdecl;
var
  i: Integer;
begin
  LastError := SUCCESS;

  if n < 0 then
  begin
    LastError := ERROR_INVALID_PARAMETER;
    LastErrorMsg := 'n doit être positif';
    Result := 0;
    Exit;
  end;

  Result := 1;
  for i := 2 to n do
    Result := Result * i;
end;

// Fonction avec tableau
procedure TrierTableau(tableau: PDouble; taille: Integer); cdecl;
var
  i, j: Integer;
  temp: Double;
begin
  LastError := SUCCESS;

  if tableau = nil then
  begin
    LastError := ERROR_INVALID_PARAMETER;
    Exit;
  end;

  // Tri à bulles (simple pour l'exemple)
  for i := 0 to taille - 2 do
    for j := 0 to taille - i - 2 do
      if PDouble(tableau + j * SizeOf(Double))^ >
         PDouble(tableau + (j + 1) * SizeOf(Double))^ then
      begin
        temp := PDouble(tableau + j * SizeOf(Double))^;
        PDouble(tableau + j * SizeOf(Double))^ :=
          PDouble(tableau + (j + 1) * SizeOf(Double))^;
        PDouble(tableau + (j + 1) * SizeOf(Double))^ := temp;
      end;
end;

// Fonction de statistiques
procedure CalculerStatistiques(tableau: PDouble; taille: Integer;
  out moyenne, ecartType: Double); cdecl;
var
  i: Integer;
  somme, variance: Double;
begin
  LastError := SUCCESS;
  moyenne := 0;
  ecartType := 0;

  if (tableau = nil) or (taille <= 0) then
  begin
    LastError := ERROR_INVALID_PARAMETER;
    Exit;
  end;

  // Calculer la moyenne
  somme := 0;
  for i := 0 to taille - 1 do
    somme := somme + PDouble(tableau + i * SizeOf(Double))^;
  moyenne := somme / taille;

  // Calculer l'écart-type
  variance := 0;
  for i := 0 to taille - 1 do
    variance := variance + Sqr(PDouble(tableau + i * SizeOf(Double))^ - moyenne);
  variance := variance / taille;
  ecartType := Sqrt(variance);
end;

// Récupération des erreurs
function ObtenirDerniereErreur: Integer; cdecl;
begin
  Result := LastError;
end;

procedure ObtenirMessageErreur(buffer: PChar; taille: Integer); cdecl;
begin
  if buffer <> nil then
    StrLCopy(buffer, PChar(LastErrorMsg), taille - 1);
end;

// Version
function ObtenirVersion: PChar; cdecl;
begin
  Result := '1.0.0';
end;

exports
  Additionner,
  Diviser,
  CalculerFactorielle,
  TrierTableau,
  CalculerStatistiques,
  ObtenirDerniereErreur,
  ObtenirMessageErreur,
  ObtenirVersion;

begin
end.
```

#### Compilation

**Windows** :
```batch
fpc -omathpas.dll mathpas.pas
```

**Linux** :
```bash
fpc -olibmathpas.so mathpas.pas
```

#### Utilisation depuis Python avec ctypes

```python
#!/usr/bin/env python3
"""
Utilisation de la bibliothèque mathpas depuis Python
"""

import ctypes
import sys
import os
from typing import List

# Déterminer le nom de la bibliothèque selon la plateforme
if sys.platform == 'win32':
    lib_name = 'mathpas.dll'
elif sys.platform == 'darwin':
    lib_name = 'libmathpas.dylib'
else:
    lib_name = 'libmathpas.so'

# Charger la bibliothèque
try:
    mathpas = ctypes.CDLL(os.path.join('.', lib_name))
except OSError as e:
    print(f"Erreur lors du chargement de {lib_name}: {e}")
    sys.exit(1)

# Définir les prototypes des fonctions
mathpas.Additionner.argtypes = [ctypes.c_double, ctypes.c_double]
mathpas.Additionner.restype = ctypes.c_double

mathpas.Diviser.argtypes = [ctypes.c_double, ctypes.c_double,
                            ctypes.POINTER(ctypes.c_double)]
mathpas.Diviser.restype = ctypes.c_int

mathpas.CalculerFactorielle.argtypes = [ctypes.c_int]
mathpas.CalculerFactorielle.restype = ctypes.c_int64

mathpas.TrierTableau.argtypes = [ctypes.POINTER(ctypes.c_double), ctypes.c_int]
mathpas.TrierTableau.restype = None

mathpas.CalculerStatistiques.argtypes = [
    ctypes.POINTER(ctypes.c_double),
    ctypes.c_int,
    ctypes.POINTER(ctypes.c_double),
    ctypes.POINTER(ctypes.c_double)
]
mathpas.CalculerStatistiques.restype = None

mathpas.ObtenirVersion.argtypes = []
mathpas.ObtenirVersion.restype = ctypes.c_char_p

mathpas.ObtenirDerniereErreur.argtypes = []
mathpas.ObtenirDerniereErreur.restype = ctypes.c_int

mathpas.ObtenirMessageErreur.argtypes = [ctypes.c_char_p, ctypes.c_int]
mathpas.ObtenirMessageErreur.restype = None


def additionner(a: float, b: float) -> float:
    """Addition de deux nombres"""
    return mathpas.Additionner(a, b)


def diviser(a: float, b: float) -> tuple[bool, float, str]:
    """
    Division de deux nombres
    Retourne: (succès, résultat, message_erreur)
    """
    resultat = ctypes.c_double()
    code = mathpas.Diviser(a, b, ctypes.byref(resultat))

    if code == 0:
        return (True, resultat.value, "")
    else:
        buffer = ctypes.create_string_buffer(256)
        mathpas.ObtenirMessageErreur(buffer, 256)
        return (False, 0.0, buffer.value.decode())


def calculer_factorielle(n: int) -> int:
    """Calcule la factorielle de n"""
    return mathpas.CalculerFactorielle(n)


def trier_tableau(tableau: List[float]) -> List[float]:
    """Trie un tableau de nombres"""
    taille = len(tableau)
    tableau_c = (ctypes.c_double * taille)(*tableau)
    mathpas.TrierTableau(tableau_c, taille)
    return list(tableau_c)


def calculer_statistiques(tableau: List[float]) -> dict:
    """Calcule moyenne et écart-type"""
    taille = len(tableau)
    tableau_c = (ctypes.c_double * taille)(*tableau)
    moyenne = ctypes.c_double()
    ecart_type = ctypes.c_double()

    mathpas.CalculerStatistiques(
        tableau_c,
        taille,
        ctypes.byref(moyenne),
        ctypes.byref(ecart_type)
    )

    return {
        'moyenne': moyenne.value,
        'ecart_type': ecart_type.value
    }


def obtenir_version() -> str:
    """Retourne la version de la bibliothèque"""
    return mathpas.ObtenirVersion().decode()


# Tests
if __name__ == "__main__":
    print(f"mathpas version {obtenir_version()}\n")

    # Test addition
    print(f"5 + 3 = {additionner(5, 3)}")

    # Test division
    succes, resultat, erreur = diviser(10, 2)
    if succes:
        print(f"10 / 2 = {resultat}")
    else:
        print(f"Erreur : {erreur}")

    # Test division par zéro
    succes, resultat, erreur = diviser(10, 0)
    if not succes:
        print(f"10 / 0 → Erreur attendue : {erreur}")

    # Test factorielle
    print(f"\n5! = {calculer_factorielle(5)}")
    print(f"10! = {calculer_factorielle(10)}")

    # Test tri
    donnees = [3.14, 1.41, 2.71, 0.57, 1.61]
    print(f"\nAvant tri : {donnees}")
    donnees_triees = trier_tableau(donnees)
    print(f"Après tri : {donnees_triees}")

    # Test statistiques
    valeurs = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    stats = calculer_statistiques(valeurs)
    print(f"\nStatistiques de {valeurs}:")
    print(f"  Moyenne : {stats['moyenne']:.2f}")
    print(f"  Écart-type : {stats['ecart_type']:.2f}")
```

**Exécution** :
```bash
python test_mathpas.py
```

**Sortie attendue** :
```
mathpas version 1.0.0

5 + 3 = 8.0
10 / 2 = 5.0
10 / 0 → Erreur attendue : Division par zéro

5! = 120
10! = 3628800

Avant tri : [3.14, 1.41, 2.71, 0.57, 1.61]
Après tri : [0.57, 1.41, 1.61, 2.71, 3.14]

Statistiques de [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]:
  Moyenne : 5.50
  Écart-type : 2.87
```

### Créer un wrapper Python plus pythonique

Pour une meilleure intégration, créez une classe Python wrapper :

```python
#!/usr/bin/env python3
"""
Wrapper orienté objet pour mathpas
"""

import ctypes
import sys
import os
from typing import List, Tuple, Optional


class MathPasError(Exception):
    """Exception pour les erreurs de mathpas"""
    pass


class MathPas:
    """Interface Python pour la bibliothèque mathpas"""

    def __init__(self, lib_path: Optional[str] = None):
        """
        Initialise la bibliothèque mathpas

        Args:
            lib_path: Chemin vers la bibliothèque (optionnel)
        """
        if lib_path is None:
            # Déterminer automatiquement le nom
            if sys.platform == 'win32':
                lib_name = 'mathpas.dll'
            elif sys.platform == 'darwin':
                lib_name = 'libmathpas.dylib'
            else:
                lib_name = 'libmathpas.so'
            lib_path = os.path.join('.', lib_name)

        try:
            self._lib = ctypes.CDLL(lib_path)
        except OSError as e:
            raise MathPasError(f"Impossible de charger {lib_path}: {e}")

        self._configure_functions()

    def _configure_functions(self):
        """Configure les prototypes des fonctions"""
        # Additionner
        self._lib.Additionner.argtypes = [ctypes.c_double, ctypes.c_double]
        self._lib.Additionner.restype = ctypes.c_double

        # Diviser
        self._lib.Diviser.argtypes = [
            ctypes.c_double,
            ctypes.c_double,
            ctypes.POINTER(ctypes.c_double)
        ]
        self._lib.Diviser.restype = ctypes.c_int

        # CalculerFactorielle
        self._lib.CalculerFactorielle.argtypes = [ctypes.c_int]
        self._lib.CalculerFactorielle.restype = ctypes.c_int64

        # TrierTableau
        self._lib.TrierTableau.argtypes = [
            ctypes.POINTER(ctypes.c_double),
            ctypes.c_int
        ]
        self._lib.TrierTableau.restype = None

        # CalculerStatistiques
        self._lib.CalculerStatistiques.argtypes = [
            ctypes.POINTER(ctypes.c_double),
            ctypes.c_int,
            ctypes.POINTER(ctypes.c_double),
            ctypes.POINTER(ctypes.c_double)
        ]
        self._lib.CalculerStatistiques.restype = None

        # Gestion des erreurs
        self._lib.ObtenirDerniereErreur.argtypes = []
        self._lib.ObtenirDerniereErreur.restype = ctypes.c_int

        self._lib.ObtenirMessageErreur.argtypes = [ctypes.c_char_p, ctypes.c_int]
        self._lib.ObtenirMessageErreur.restype = None

        # Version
        self._lib.ObtenirVersion.argtypes = []
        self._lib.ObtenirVersion.restype = ctypes.c_char_p

    def _check_error(self):
        """Vérifie s'il y a eu une erreur et lève une exception si nécessaire"""
        code = self._lib.ObtenirDerniereErreur()
        if code != 0:
            buffer = ctypes.create_string_buffer(256)
            self._lib.ObtenirMessageErreur(buffer, 256)
            raise MathPasError(buffer.value.decode())

    @property
    def version(self) -> str:
        """Retourne la version de la bibliothèque"""
        return self._lib.ObtenirVersion().decode()

    def add(self, a: float, b: float) -> float:
        """
        Additionne deux nombres

        Args:
            a: Premier nombre
            b: Deuxième nombre

        Returns:
            La somme de a et b
        """
        return self._lib.Additionner(a, b)

    def divide(self, a: float, b: float) -> float:
        """
        Divise deux nombres

        Args:
            a: Dividende
            b: Diviseur

        Returns:
            Le résultat de a / b

        Raises:
            MathPasError: Si b est zéro
        """
        result = ctypes.c_double()
        code = self._lib.Diviser(a, b, ctypes.byref(result))

        if code != 0:
            self._check_error()

        return result.value

    def factorial(self, n: int) -> int:
        """
        Calcule la factorielle de n

        Args:
            n: Nombre entier positif

        Returns:
            n!

        Raises:
            MathPasError: Si n est négatif
        """
        result = self._lib.CalculerFactorielle(n)
        self._check_error()
        return result

    def sort(self, values: List[float]) -> List[float]:
        """
        Trie une liste de nombres

        Args:
            values: Liste de nombres à trier

        Returns:
            Liste triée par ordre croissant
        """
        if not values:
            return []

        size = len(values)
        array = (ctypes.c_double * size)(*values)
        self._lib.TrierTableau(array, size)

        return list(array)

    def statistics(self, values: List[float]) -> Tuple[float, float]:
        """
        Calcule la moyenne et l'écart-type

        Args:
            values: Liste de nombres

        Returns:
            Tuple (moyenne, écart-type)

        Raises:
            MathPasError: Si la liste est vide
        """
        if not values:
            raise MathPasError("La liste ne peut pas être vide")

        size = len(values)
        array = (ctypes.c_double * size)(*values)
        mean = ctypes.c_double()
        std = ctypes.c_double()

        self._lib.CalculerStatistiques(
            array,
            size,
            ctypes.byref(mean),
            ctypes.byref(std)
        )

        self._check_error()
        return (mean.value, std.value)


# Exemple d'utilisation
if __name__ == "__main__":
    # Créer l'instance
    mp = MathPas()

    print(f"MathPas version {mp.version}\n")

    # Utilisation style Python
    print("Tests de base:")
    print(f"  5 + 3 = {mp.add(5, 3)}")
    print(f"  10 / 2 = {mp.divide(10, 2)}")
    print(f"  5! = {mp.factorial(5)}")

    # Gestion des erreurs
    print("\nGestion des erreurs:")
    try:
        mp.divide(10, 0)
    except MathPasError as e:
        print(f"  Division par zéro: {e}")

    try:
        mp.factorial(-5)
    except MathPasError as e:
        print(f"  Factorielle négative: {e}")

    # Tri
    print("\nTri:")
    data = [3.14, 1.41, 2.71, 0.57, 1.61]
    print(f"  Avant: {data}")
    sorted_data = mp.sort(data)
    print(f"  Après: {sorted_data}")

    # Statistiques
    print("\nStatistiques:")
    values = list(range(1, 11))
    mean, std = mp.statistics(values)
    print(f"  Données: {values}")
    print(f"  Moyenne: {mean:.2f}")
    print(f"  Écart-type: {std:.2f}")
```

## Approche 3 : Communication inter-processus

Lorsque l'embedding ou les extensions natives sont trop complexes, la communication inter-processus (IPC) offre une alternative flexible.

### Avantages de l'IPC

- **Isolation** : Les processus sont indépendants (crash de l'un n'affecte pas l'autre)
- **Langages hétérogènes** : Aucune dépendance directe
- **Déploiement flexible** : Les processus peuvent être sur des machines différentes
- **Mise à jour indépendante** : Chaque composant peut évoluer séparément

### Communication via sockets et JSON

C'est l'approche la plus universelle et simple.

#### Serveur Pascal

```pascal
program ServeurPascal;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, ssockets, fpjson, jsonparser;

type
  TRequete = record
    Action: string;
    Parametres: TJSONObject;
  end;

function TraiterRequete(const Requete: TRequete): TJSONObject;
var
  a, b: Double;
  n, i: Integer;
  fact: Int64;
begin
  Result := TJSONObject.Create;

  try
    if Requete.Action = 'additionner' then
    begin
      a := Requete.Parametres.Get('a', 0.0);
      b := Requete.Parametres.Get('b', 0.0);
      Result.Add('resultat', a + b);
      Result.Add('succes', True);
    end
    else if Requete.Action = 'multiplier' then
    begin
      a := Requete.Parametres.Get('a', 0.0);
      b := Requete.Parametres.Get('b', 0.0);
      Result.Add('resultat', a * b);
      Result.Add('succes', True);
    end
    else if Requete.Action = 'factorielle' then
    begin
      n := Requete.Parametres.Get('n', 0);
      fact := 1;
      for i := 2 to n do
        fact := fact * i;
      Result.Add('resultat', fact);
      Result.Add('succes', True);
    end
    else
    begin
      Result.Add('succes', False);
      Result.Add('erreur', 'Action inconnue: ' + Requete.Action);
    end;
  except
    on E: Exception do
    begin
      Result.Add('succes', False);
      Result.Add('erreur', E.Message);
    end;
  end;
end;

var
  Server: TInetServer;
  Client: TSocketStream;
  Ligne: string;
  JSON: TJSONData;
  Requete: TRequete;
  Reponse: TJSONObject;
  c: Char;

begin
  Server := TInetServer.Create('127.0.0.1', 9090);
  try
    WriteLn('Serveur Pascal démarré sur le port 9090');
    WriteLn('En attente de connexions...');

    while True do
    begin
      Client := Server.Accept;
      if Client <> nil then
      try
        WriteLn('Client connecté');

        // Lire la requête
        Ligne := '';
        repeat
          if Client.Read(c, 1) = 1 then
            Ligne := Ligne + c;
        until (c = #10) or (Client.Position >= Client.Size);

        WriteLn('Requête reçue: ', Ligne);

        // Parser le JSON
        JSON := GetJSON(Ligne);
        try
          if JSON is TJSONObject then
          begin
            Requete.Action := TJSONObject(JSON).Get('action', '');
            Requete.Parametres := TJSONObject(JSON).Get('params',
              TJSONObject.Create) as TJSONObject;

            // Traiter
            Reponse := TraiterRequete(Requete);
            try
              // Envoyer la réponse
              Ligne := Reponse.AsJSON + #10;
              Client.Write(Ligne[1], Length(Ligne));
              WriteLn('Réponse envoyée: ', Reponse.AsJSON);
            finally
              Reponse.Free;
            end;
          end;
        finally
          JSON.Free;
        end;

      finally
        Client.Free;
      end;
    end;

  finally
    Server.Free;
  end;
end.
```

#### Client Python

```python
#!/usr/bin/env python3
"""
Client Python pour communiquer avec le serveur Pascal
"""

import socket
import json
from typing import Any, Dict


class PascalClient:
    """Client pour communiquer avec le serveur Pascal"""

    def __init__(self, host: str = '127.0.0.1', port: int = 9090):
        """
        Initialise le client

        Args:
            host: Adresse du serveur
            port: Port du serveur
        """
        self.host = host
        self.port = port

    def _send_request(self, action: str, params: Dict[str, Any]) -> Dict[str, Any]:
        """
        Envoie une requête au serveur

        Args:
            action: Action à effectuer
            params: Paramètres de l'action

        Returns:
            Réponse du serveur
        """
        # Préparer la requête
        request = {
            'action': action,
            'params': params
        }

        # Connexion au serveur
        with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
            s.connect((self.host, self.port))

            # Envoyer la requête
            request_json = json.dumps(request) + '\n'
            s.sendall(request_json.encode())

            # Recevoir la réponse
            response_data = s.recv(4096)
            response = json.loads(response_data.decode())

            return response

    def add(self, a: float, b: float) -> float:
        """Additionne deux nombres"""
        response = self._send_request('additionner', {'a': a, 'b': b})

        if response['succes']:
            return response['resultat']
        else:
            raise Exception(response['erreur'])

    def multiply(self, a: float, b: float) -> float:
        """Multiplie deux nombres"""
        response = self._send_request('multiplier', {'a': a, 'b': b})

        if response['succes']:
            return response['resultat']
        else:
            raise Exception(response['erreur'])

    def factorial(self, n: int) -> int:
        """Calcule la factorielle de n"""
        response = self._send_request('factorielle', {'n': n})

        if response['succes']:
            return response['resultat']
        else:
            raise Exception(response['erreur'])


# Exemple d'utilisation
if __name__ == "__main__":
    client = PascalClient()

    print("Tests du client Pascal:")
    print(f"5 + 3 = {client.add(5, 3)}")
    print(f"5 * 3 = {client.multiply(5, 3)}")
    print(f"5! = {client.factorial(5)}")
    print(f"10! = {client.factorial(10)}")
```

### Communication via pipes nommés (Windows)

#### Serveur Pascal (Windows)

```pascal
program ServeurPipeWindows;

{$mode objfpc}{$H+}

uses
  Windows, SysUtils;

const
  PIPE_NAME = '\\.\pipe\mathpas_pipe';
  BUFFER_SIZE = 512;

var
  Pipe: THandle;
  Buffer: array[0..BUFFER_SIZE-1] of Char;
  BytesRead, BytesWritten: DWORD;
  Message, Response: string;

begin
  WriteLn('Démarrage du serveur pipe...');

  while True do
  begin
    // Créer le pipe nommé
    Pipe := CreateNamedPipe(
      PIPE_NAME,
      PIPE_ACCESS_DUPLEX,
      PIPE_TYPE_MESSAGE or PIPE_READMODE_MESSAGE or PIPE_WAIT,
      1,
      BUFFER_SIZE,
      BUFFER_SIZE,
      0,
      nil
    );

    if Pipe = INVALID_HANDLE_VALUE then
    begin
      WriteLn('Erreur lors de la création du pipe');
      Break;
    end;

    WriteLn('En attente de connexion...');

    // Attendre une connexion
    if ConnectNamedPipe(Pipe, nil) then
    begin
      WriteLn('Client connecté');

      // Lire le message
      if ReadFile(Pipe, Buffer, BUFFER_SIZE, BytesRead, nil) then
      begin
        SetString(Message, Buffer, BytesRead);
        WriteLn('Message reçu: ', Message);

        // Traiter le message (exemple simple)
        Response := 'Echo: ' + Message;

        // Envoyer la réponse
        WriteFile(Pipe, Response[1], Length(Response), BytesWritten, nil);
        WriteLn('Réponse envoyée: ', Response);
      end;

      // Déconnecter
      DisconnectNamedPipe(Pipe);
    end;

    CloseHandle(Pipe);
  end;
end.
```

#### Client Python (Windows)

```python
#!/usr/bin/env python3
"""
Client Python pour communiquer via pipe nommé (Windows)
"""

import win32pipe
import win32file


class PipeClient:
    """Client pour communiquer via pipes nommés"""

    def __init__(self, pipe_name: str = r'\\.\pipe\mathpas_pipe'):
        self.pipe_name = pipe_name

    def send_message(self, message: str) -> str:
        """
        Envoie un message et attend la réponse

        Args:
            message: Message à envoyer

        Returns:
            Réponse du serveur
        """
        # Ouvrir le pipe
        handle = win32file.CreateFile(
            self.pipe_name,
            win32file.GENERIC_READ | win32file.GENERIC_WRITE,
            0,
            None,
            win32file.OPEN_EXISTING,
            0,
            None
        )

        try:
            # Envoyer le message
            win32file.WriteFile(handle, message.encode())

            # Lire la réponse
            result, data = win32file.ReadFile(handle, 512)
            return data.decode()

        finally:
            win32file.CloseHandle(handle)


# Exemple d'utilisation
if __name__ == "__main__":
    client = PipeClient()

    response = client.send_message("Hello from Python!")
    print(f"Réponse: {response}")
```

### Communication via Unix Domain Sockets (Linux)

#### Serveur Pascal (Linux)

```pascal
program ServeurUnixSocket;

{$mode objfpc}{$H+}

uses
  BaseUnix, Unix, Sockets, SysUtils;

const
  SOCKET_PATH = '/tmp/mathpas.sock';

var
  ServerSocket, ClientSocket: LongInt;
  ServerAddr, ClientAddr: TSockAddr;
  AddrLen: TSockLen;
  Buffer: array[0..255] of Char;
  BytesRead: LongInt;
  Message: string;

begin
  // Supprimer le socket s'il existe déjà
  DeleteFile(SOCKET_PATH);

  // Créer le socket
  ServerSocket := fpSocket(AF_UNIX, SOCK_STREAM, 0);
  if ServerSocket < 0 then
  begin
    WriteLn('Erreur lors de la création du socket');
    Halt(1);
  end;

  // Configurer l'adresse
  FillChar(ServerAddr, SizeOf(ServerAddr), 0);
  ServerAddr.sa_family := AF_UNIX;
  Move(SOCKET_PATH[1], ServerAddr.sa_data, Length(SOCKET_PATH));

  // Lier le socket
  if fpBind(ServerSocket, @ServerAddr, SizeOf(ServerAddr)) < 0 then
  begin
    WriteLn('Erreur lors du bind');
    Halt(1);
  end;

  // Écouter
  if fpListen(ServerSocket, 5) < 0 then
  begin
    WriteLn('Erreur lors du listen');
    Halt(1);
  end;

  WriteLn('Serveur Unix socket démarré sur ', SOCKET_PATH);

  while True do
  begin
    WriteLn('En attente de connexion...');

    AddrLen := SizeOf(ClientAddr);
    ClientSocket := fpAccept(ServerSocket, @ClientAddr, @AddrLen);

    if ClientSocket >= 0 then
    begin
      WriteLn('Client connecté');

      // Lire les données
      BytesRead := fpRecv(ClientSocket, @Buffer, SizeOf(Buffer), 0);

      if BytesRead > 0 then
      begin
        SetString(Message, Buffer, BytesRead);
        WriteLn('Message reçu: ', Message);

        // Envoyer une réponse
        Message := 'Echo: ' + Message;
        fpSend(ClientSocket, @Message[1], Length(Message), 0);
      end;

      fpClose(ClientSocket);
    end;
  end;

  fpClose(ServerSocket);
  DeleteFile(SOCKET_PATH);
end.
```

#### Client Python (Linux)

```python
#!/usr/bin/env python3
"""
Client Python pour Unix Domain Sockets
"""

import socket


class UnixSocketClient:
    """Client pour Unix Domain Sockets"""

    def __init__(self, socket_path: str = '/tmp/mathpas.sock'):
        self.socket_path = socket_path

    def send_message(self, message: str) -> str:
        """
        Envoie un message et attend la réponse

        Args:
            message: Message à envoyer

        Returns:
            Réponse du serveur
        """
        with socket.socket(socket.AF_UNIX, socket.SOCK_STREAM) as s:
            s.connect(self.socket_path)
            s.sendall(message.encode())
            response = s.recv(1024)
            return response.decode()


# Exemple d'utilisation
if __name__ == "__main__":
    client = UnixSocketClient()

    response = client.send_message("Hello from Python!")
    print(f"Réponse: {response}")
```

## Utilisation de bibliothèques Python avancées depuis Pascal

### Exemple : Utiliser scikit-learn depuis Pascal

```pascal
program MachineLearningPascal;

{$mode objfpc}{$H+}

uses
  SysUtils, PythonEngine, Variants;

var
  Engine: TPythonEngine;
  MainModule: Variant;
  Model: Variant;
  Predictions: Variant;
  i: Integer;

begin
  Engine := TPythonEngine.Create(nil);
  try
    Engine.LoadDll;
    MainModule := Engine.GetMainModule;

    WriteLn('Chargement de scikit-learn...');

    // Importer les bibliothèques nécessaires
    Engine.ExecStrings([
      'import numpy as np',
      'from sklearn.linear_model import LinearRegression',
      'from sklearn.preprocessing import StandardScaler'
    ]);

    WriteLn('Création d''un modèle de régression linéaire...');

    // Créer des données d'entraînement
    Engine.ExecStrings([
      '# Données d''entraînement',
      'X_train = np.array([[1], [2], [3], [4], [5]])',
      'y_train = np.array([2, 4, 6, 8, 10])',
      '',
      '# Créer et entraîner le modèle',
      'model = LinearRegression()',
      'model.fit(X_train, y_train)',
      '',
      '# Afficher les paramètres',
      'print(f"Coefficient: {model.coef_[0]:.2f}")',
      'print(f"Intercept: {model.intercept_:.2f}")'
    ]);

    WriteLn;
    WriteLn('Prédictions:');

    // Faire des prédictions
    Engine.ExecStrings([
      'X_test = np.array([[6], [7], [8]])',
      'predictions = model.predict(X_test)',
      'print("Prédictions:", predictions)'
    ]);

    // Récupérer les prédictions dans Pascal
    Predictions := MainModule.predictions;
    WriteLn;
    WriteLn('Prédictions récupérées en Pascal:');
    for i := 0 to 2 do
      WriteLn(Format('  X=%d → Y=%.2f', [i+6, Double(Predictions.__getitem__(i))]));

  finally
    Engine.Free;
  end;

  ReadLn;
end.
```

### Exemple : Traitement d'images avec PIL/Pillow

```pascal
program TraitementImagesPython;

{$mode objfpc}{$H+}

uses
  SysUtils, PythonEngine, Variants;

var
  Engine: TPythonEngine;
  MainModule: Variant;

begin
  Engine := TPythonEngine.Create(nil);
  try
    Engine.LoadDll;
    MainModule := Engine.GetMainModule;

    WriteLn('Traitement d''images avec Pillow...');

    // Charger et manipuler une image
    Engine.ExecStrings([
      'from PIL import Image, ImageFilter, ImageEnhance',
      '',
      'try:',
      '    # Charger l''image',
      '    img = Image.open("test.jpg")',
      '    print(f"Image chargée: {img.size[0]}x{img.size[1]} pixels")',
      '    ',
      '    # Redimensionner',
      '    img_resized = img.resize((200, 200))',
      '    img_resized.save("test_resized.jpg")',
      '    print("Image redimensionnée sauvegardée")',
      '    ',
      '    # Appliquer un filtre',
      '    img_blur = img.filter(ImageFilter.BLUR)',
      '    img_blur.save("test_blur.jpg")',
      '    print("Filtre flou appliqué")',
      '    ',
      '    # Ajuster la luminosité',
      '    enhancer = ImageEnhance.Brightness(img)',
      '    img_bright = enhancer.enhance(1.5)',
      '    img_bright.save("test_bright.jpg")',
      '    print("Luminosité ajustée")',
      '    ',
      '    # Convertir en niveaux de gris',
      '    img_gray = img.convert("L")',
      '    img_gray.save("test_gray.jpg")',
      '    print("Converti en niveaux de gris")',
      '    ',
      'except FileNotFoundError:',
      '    print("Erreur: test.jpg non trouvé")',
      'except Exception as e:',
      '    print(f"Erreur: {e}")'
    ]);

  finally
    Engine.Free;
  end;

  ReadLn;
end.
```

## Bonnes pratiques et recommandations

### Pour l'embedding Python

✅ **À faire** :
1. **Initialiser une seule fois** : Créer un TPythonEngine global
2. **Gérer les exceptions** : Utiliser try..except pour capturer les erreurs Python
3. **Libérer les ressources** : Toujours Free le TPythonEngine
4. **Vérifier la disponibilité** : Tester si Python est installé
5. **Documenter les dépendances** : Lister les packages Python requis
6. **Isoler le code Python** : Utiliser des fichiers .py séparés quand c'est complexe
7. **Versionner Python** : Spécifier la version minimale requise
8. **Tester les imports** : Vérifier que les bibliothèques Python sont disponibles

❌ **À éviter** :
1. **Créer/détruire TPythonEngine en boucle** : Très coûteux
2. **Ignorer les erreurs** : Toujours gérer les exceptions Python
3. **Mélanger les versions Python** : Utiliser une seule version
4. **Oublier AutoFinalize** : Risque de fuites mémoire
5. **Code Python non testé** : Valider d'abord en Python pur
6. **Dépendances implicites** : Documenter tous les packages nécessaires

### Pour les extensions Pascal

✅ **À faire** :
1. **Convention cdecl** : Obligatoire pour l'interopérabilité
2. **Types simples** : Utiliser int, double, char*, pas de types Pascal complexes
3. **Gestion d'erreurs claire** : Codes de retour + messages
4. **Documentation** : Fournir des exemples Python
5. **Tests** : Tester depuis Python avant de distribuer
6. **Wrapper Python** : Créer une classe Python pour l'ergonomie
7. **Versioning** : Respecter la compatibilité ABI

❌ **À éviter** :
1. **Types Pascal natifs** : string, classes, etc.
2. **Exceptions non gérées** : Doivent rester dans la DLL
3. **Mémoire partagée** : Toujours allouer/libérer du même côté
4. **Pas de documentation** : Les utilisateurs Python ne connaissent pas Pascal

### Pour l'IPC

✅ **À faire** :
1. **Format standard** : JSON, MessagePack, Protocol Buffers
2. **Gestion des timeouts** : Ne pas bloquer indéfiniment
3. **Reconnexion automatique** : Gérer les déconnexions
4. **Validation des données** : Vérifier les entrées/sorties
5. **Logging des échanges** : Pour le débogage
6. **Versioning du protocole** : Gérer l'évolution
7. **Authentification** : Si nécessaire (tokens, certificats)
8. **Tests de charge** : Vérifier la performance

❌ **À éviter** :
1. **Formats binaires propriétaires** : Difficiles à déboguer
2. **Pas de gestion d'erreur** : Le réseau peut échouer
3. **Blocage indéfini** : Toujours avoir un timeout
4. **Pas de validation** : Données malformées = crash
5. **Sécurité faible** : Valider et assainir les entrées
6. **Un seul client** : Prévoir le multi-client si nécessaire

## Cas d'usage avancés

### Cas 1 : Application Pascal avec plugins Python

Une application Pascal qui permet aux utilisateurs d'écrire des plugins en Python.

#### Architecture de plugins

```pascal
program PluginSystem;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, PythonEngine, Variants;

type
  TPlugin = class
  private
    FName: string;
    FVersion: string;
    FModule: Variant;
    FEngine: TPythonEngine;
  public
    constructor Create(AEngine: TPythonEngine; const FileName: string);
    function Execute(const Params: array of Variant): Variant;
    property Name: string read FName;
    property Version: string read FVersion;
  end;

  TPluginManager = class
  private
    FEngine: TPythonEngine;
    FPlugins: TList;
    function GetPlugin(Index: Integer): TPlugin;
    function GetPluginCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadPlugin(const FileName: string);
    procedure UnloadAll;
    property Plugins[Index: Integer]: TPlugin read GetPlugin;
    property PluginCount: Integer read GetPluginCount;
  end;

{ TPlugin }

constructor TPlugin.Create(AEngine: TPythonEngine; const FileName: string);
var
  PluginName: string;
begin
  FEngine := AEngine;

  // Extraire le nom du fichier sans extension
  PluginName := ChangeFileExt(ExtractFileName(FileName), '');

  // Charger le module Python
  FEngine.ExecStrings([
    Format('import sys', []),
    Format('sys.path.append("%s")', [ExtractFilePath(FileName)]),
    Format('import %s', [PluginName])
  ]);

  // Obtenir le module
  FModule := FEngine.GetMainModule;
  FModule := FModule.__dict__.__getitem__(PluginName);

  // Lire les métadonnées
  try
    FName := string(FModule.PLUGIN_NAME);
    FVersion := string(FModule.PLUGIN_VERSION);
  except
    FName := PluginName;
    FVersion := '1.0.0';
  end;

  WriteLn(Format('Plugin chargé: %s v%s', [FName, FVersion]));
end;

function TPlugin.Execute(const Params: array of Variant): Variant;
var
  ExecuteFunc: Variant;
  ParamsArray: Variant;
  i: Integer;
begin
  // Obtenir la fonction execute du plugin
  ExecuteFunc := FModule.execute;

  // Convertir les paramètres en liste Python
  FEngine.ExecString('params_list = []');
  ParamsArray := FEngine.GetMainModule.params_list;

  for i := Low(Params) to High(Params) do
    ParamsArray.append(Params[i]);

  // Appeler la fonction
  Result := ExecuteFunc(ParamsArray);
end;

{ TPluginManager }

constructor TPluginManager.Create;
begin
  inherited Create;
  FEngine := TPythonEngine.Create(nil);
  FEngine.LoadDll;
  FPlugins := TList.Create;
end;

destructor TPluginManager.Destroy;
begin
  UnloadAll;
  FPlugins.Free;
  FEngine.Free;
  inherited Destroy;
end;

procedure TPluginManager.LoadPlugin(const FileName: string);
var
  Plugin: TPlugin;
begin
  if not FileExists(FileName) then
  begin
    WriteLn('Erreur: Plugin non trouvé: ', FileName);
    Exit;
  end;

  try
    Plugin := TPlugin.Create(FEngine, FileName);
    FPlugins.Add(Plugin);
  except
    on E: Exception do
      WriteLn('Erreur lors du chargement du plugin: ', E.Message);
  end;
end;

procedure TPluginManager.UnloadAll;
var
  i: Integer;
begin
  for i := 0 to FPlugins.Count - 1 do
    TPlugin(FPlugins[i]).Free;
  FPlugins.Clear;
end;

function TPluginManager.GetPlugin(Index: Integer): TPlugin;
begin
  Result := TPlugin(FPlugins[Index]);
end;

function TPluginManager.GetPluginCount: Integer;
begin
  Result := FPlugins.Count;
end;

var
  Manager: TPluginManager;
  Result: Variant;
  i: Integer;

begin
  Manager := TPluginManager.Create;
  try
    WriteLn('=== Système de plugins Python ===');
    WriteLn;

    // Charger les plugins
    Manager.LoadPlugin('plugins/calculatrice.py');
    Manager.LoadPlugin('plugins/texte.py');

    WriteLn;
    WriteLn('Plugins chargés: ', Manager.PluginCount);

    // Utiliser les plugins
    if Manager.PluginCount > 0 then
    begin
      WriteLn;
      WriteLn('Exécution des plugins:');

      for i := 0 to Manager.PluginCount - 1 do
      begin
        WriteLn('  Plugin: ', Manager.Plugins[i].Name);

        try
          Result := Manager.Plugins[i].Execute([10, 5]);
          WriteLn('  Résultat: ', string(Result));
        except
          on E: Exception do
            WriteLn('  Erreur: ', E.Message);
        end;
      end;
    end;

  finally
    Manager.Free;
  end;

  WriteLn;
  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
```

#### Exemple de plugin Python

**plugins/calculatrice.py** :
```python
"""
Plugin calculatrice pour l'application Pascal
"""

PLUGIN_NAME = "Calculatrice"
PLUGIN_VERSION = "1.0.0"
PLUGIN_AUTHOR = "Votre Nom"
PLUGIN_DESCRIPTION = "Effectue des calculs mathématiques"

def execute(params):
    """
    Fonction principale du plugin

    Args:
        params: Liste de paramètres

    Returns:
        Résultat du calcul
    """
    if len(params) < 2:
        return "Erreur: au moins 2 paramètres requis"

    a = params[0]
    b = params[1]

    return {
        'addition': a + b,
        'soustraction': a - b,
        'multiplication': a * b,
        'division': a / b if b != 0 else 'Division par zéro'
    }

def aide():
    """Retourne l'aide du plugin"""
    return f"{PLUGIN_NAME} v{PLUGIN_VERSION}\n{PLUGIN_DESCRIPTION}"
```

**plugins/texte.py** :
```python
"""
Plugin de manipulation de texte
"""

PLUGIN_NAME = "Texte Utils"
PLUGIN_VERSION = "1.0.0"
PLUGIN_AUTHOR = "Votre Nom"
PLUGIN_DESCRIPTION = "Utilitaires de manipulation de texte"

def execute(params):
    """
    Fonction principale du plugin

    Args:
        params: Liste de paramètres (premier = texte)

    Returns:
        Statistiques sur le texte
    """
    if len(params) < 1:
        return "Erreur: texte requis"

    texte = str(params[0])

    return {
        'longueur': len(texte),
        'mots': len(texte.split()),
        'majuscules': sum(1 for c in texte if c.isupper()),
        'minuscules': sum(1 for c in texte if c.islower()),
        'chiffres': sum(1 for c in texte if c.isdigit())
    }
```

### Cas 2 : Service web Pascal avec backend Python

Un service web en Pascal qui utilise Python pour le machine learning.

#### Serveur Pascal avec API REST

```pascal
program ServeurWebML;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, fphttpserver, httpdefs, httproute,
  fpjson, jsonparser, PythonEngine, Variants;

type
  TMLServer = class
  private
    FPython: TPythonEngine;
    FModel: Variant;
    procedure InitModel;
  public
    constructor Create;
    destructor Destroy; override;
    function Predict(const InputData: TJSONArray): TJSONObject;
  end;

var
  Server: THTTPServer;
  MLServer: TMLServer;

{ TMLServer }

constructor TMLServer.Create;
begin
  FPython := TPythonEngine.Create(nil);
  FPython.LoadDll;
  InitModel;
end;

destructor TMLServer.Destroy;
begin
  FPython.Free;
  inherited Destroy;
end;

procedure TMLServer.InitModel;
begin
  WriteLn('Initialisation du modèle ML...');

  FPython.ExecStrings([
    'import numpy as np',
    'from sklearn.linear_model import LogisticRegression',
    '',
    '# Données d''entraînement (exemple simple)',
    'X_train = np.array([[1, 2], [2, 3], [3, 4], [4, 5]])',
    'y_train = np.array([0, 0, 1, 1])',
    '',
    '# Créer et entraîner le modèle',
    'model = LogisticRegression()',
    'model.fit(X_train, y_train)',
    '',
    'print("Modèle entraîné avec succès")'
  ]);

  FModel := FPython.GetMainModule.model;
  WriteLn('Modèle prêt');
end;

function TMLServer.Predict(const InputData: TJSONArray): TJSONObject;
var
  i: Integer;
  DataStr: string;
  Prediction: Variant;
  Proba: Variant;
begin
  Result := TJSONObject.Create;

  try
    // Convertir les données JSON en format NumPy
    DataStr := '[';
    for i := 0 to InputData.Count - 1 do
    begin
      if i > 0 then DataStr := DataStr + ', ';
      DataStr := DataStr + FloatToStr(InputData.Items[i].AsFloat);
    end;
    DataStr := DataStr + ']';

    // Préparer les données pour la prédiction
    FPython.ExecStrings([
      Format('X_test = np.array([%s])', [DataStr]),
      'prediction = model.predict(X_test)[0]',
      'proba = model.predict_proba(X_test)[0]'
    ]);

    // Récupérer les résultats
    Prediction := FPython.GetMainModule.prediction;
    Proba := FPython.GetMainModule.proba;

    Result.Add('prediction', Integer(Prediction));
    Result.Add('probabilite_classe_0', Double(Proba.__getitem__(0)));
    Result.Add('probabilite_classe_1', Double(Proba.__getitem__(1)));
    Result.Add('succes', True);

  except
    on E: Exception do
    begin
      Result.Add('succes', False);
      Result.Add('erreur', E.Message);
    end;
  end;
end;

// Routes HTTP

procedure RoutePredict(ARequest: TRequest; AResponse: TResponse);
var
  RequestData: TJSONObject;
  InputData: TJSONArray;
  Result: TJSONObject;
begin
  try
    // Parser la requête JSON
    RequestData := TJSONObject(GetJSON(ARequest.Content));
    try
      InputData := RequestData.Get('data', TJSONArray.Create) as TJSONArray;

      // Faire la prédiction
      Result := MLServer.Predict(InputData);

      // Envoyer la réponse
      AResponse.Content := Result.AsJSON;
      AResponse.ContentType := 'application/json';
      AResponse.Code := 200;

    finally
      RequestData.Free;
      Result.Free;
    end;

  except
    on E: Exception do
    begin
      AResponse.Content := Format('{"succes": false, "erreur": "%s"}',
        [E.Message]);
      AResponse.ContentType := 'application/json';
      AResponse.Code := 500;
    end;
  end;
end;

procedure RouteStatus(ARequest: TRequest; AResponse: TResponse);
var
  Status: TJSONObject;
begin
  Status := TJSONObject.Create;
  try
    Status.Add('status', 'ok');
    Status.Add('service', 'ML Server');
    Status.Add('version', '1.0.0');

    AResponse.Content := Status.AsJSON;
    AResponse.ContentType := 'application/json';
    AResponse.Code := 200;
  finally
    Status.Free;
  end;
end;

begin
  try
    WriteLn('=== Serveur Web ML Pascal/Python ===');
    WriteLn;

    // Initialiser le serveur ML
    MLServer := TMLServer.Create;

    // Configurer les routes
    HTTPRouter.RegisterRoute('/predict', @RoutePredict);
    HTTPRouter.RegisterRoute('/status', @RouteStatus);

    // Démarrer le serveur HTTP
    Server := THTTPServer.Create(nil);
    Server.Port := 8080;
    Server.Active := True;

    WriteLn('Serveur démarré sur http://localhost:8080');
    WriteLn('Routes disponibles:');
    WriteLn('  POST /predict - Effectuer une prédiction');
    WriteLn('  GET  /status  - Statut du serveur');
    WriteLn;
    WriteLn('Appuyez sur Entrée pour arrêter...');
    ReadLn;

  finally
    Server.Free;
    MLServer.Free;
  end;
end.
```

#### Client Python pour tester le service

```python
#!/usr/bin/env python3
"""
Client Python pour tester le serveur ML Pascal
"""

import requests
import json


class MLClient:
    """Client pour le serveur ML"""

    def __init__(self, base_url: str = 'http://localhost:8080'):
        self.base_url = base_url

    def predict(self, data: list) -> dict:
        """
        Effectue une prédiction

        Args:
            data: Données d'entrée

        Returns:
            Résultat de la prédiction
        """
        url = f"{self.base_url}/predict"
        payload = {'data': data}

        response = requests.post(url, json=payload)
        return response.json()

    def status(self) -> dict:
        """
        Récupère le statut du serveur

        Returns:
            Statut du serveur
        """
        url = f"{self.base_url}/status"
        response = requests.get(url)
        return response.json()


if __name__ == "__main__":
    client = MLClient()

    # Vérifier le statut
    print("Statut du serveur:")
    status = client.status()
    print(json.dumps(status, indent=2))
    print()

    # Faire des prédictions
    print("Prédictions:")

    test_data = [
        [1.5, 2.5],
        [3.5, 4.5],
        [2.0, 3.0]
    ]

    for data in test_data:
        result = client.predict(data)
        print(f"\nDonnées: {data}")

        if result.get('succes'):
            print(f"  Prédiction: classe {result['prediction']}")
            print(f"  Probabilité classe 0: {result['probabilite_classe_0']:.2%}")
            print(f"  Probabilité classe 1: {result['probabilite_classe_1']:.2%}")
        else:
            print(f"  Erreur: {result.get('erreur')}")
```

### Cas 3 : Interface graphique Pascal avec graphiques Python (Matplotlib)

```pascal
program GraphiquesPython;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, PythonEngine, Forms, StdCtrls, ExtCtrls;

type
  TMainForm = class(TForm)
  private
    FPython: TPythonEngine;
    FBtnGenerer: TButton;
    FImage: TImage;
    FMemo: TMemo;
    procedure BtnGenererClick(Sender: TObject);
    procedure GenererGraphique;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ TMainForm }

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Caption := 'Graphiques Python depuis Pascal';
  Width := 800;
  Height := 600;

  // Bouton
  FBtnGenerer := TButton.Create(Self);
  FBtnGenerer.Parent := Self;
  FBtnGenerer.Caption := 'Générer graphique';
  FBtnGenerer.Left := 10;
  FBtnGenerer.Top := 10;
  FBtnGenerer.OnClick := @BtnGenererClick;

  // Image pour afficher le graphique
  FImage := TImage.Create(Self);
  FImage.Parent := Self;
  FImage.Left := 10;
  FImage.Top := 50;
  FImage.Width := 760;
  FImage.Height := 400;

  // Memo pour les logs
  FMemo := TMemo.Create(Self);
  FMemo.Parent := Self;
  FMemo.Left := 10;
  FMemo.Top := 460;
  FMemo.Width := 760;
  FMemo.Height := 100;
  FMemo.ScrollBars := ssVertical;

  // Initialiser Python
  FPython := TPythonEngine.Create(nil);
  FPython.LoadDll;

  FMemo.Lines.Add('Python initialisé');
  FMemo.Lines.Add('Cliquez sur le bouton pour générer un graphique');
end;

destructor TMainForm.Destroy;
begin
  FPython.Free;
  inherited Destroy;
end;

procedure TMainForm.BtnGenererClick(Sender: TObject);
begin
  GenererGraphique;
end;

procedure TMainForm.GenererGraphique;
var
  TempFile: string;
begin
  TempFile := GetTempDir + 'graphique.png';
  FMemo.Lines.Add('Génération du graphique...');

  try
    FPython.ExecStrings([
      'import matplotlib',
      'matplotlib.use("Agg")  # Backend sans affichage',
      'import matplotlib.pyplot as plt',
      'import numpy as np',
      '',
      '# Créer des données',
      'x = np.linspace(0, 10, 100)',
      'y1 = np.sin(x)',
      'y2 = np.cos(x)',
      '',
      '# Créer le graphique',
      'plt.figure(figsize=(10, 6))',
      'plt.plot(x, y1, label="sin(x)", linewidth=2)',
      'plt.plot(x, y2, label="cos(x)", linewidth=2)',
      'plt.xlabel("x")',
      'plt.ylabel("y")',
      'plt.title("Fonctions trigonométriques")',
      'plt.legend()',
      'plt.grid(True, alpha=0.3)',
      '',
      '# Sauvegarder',
      Format('plt.savefig("%s", dpi=100, bbox_inches="tight")',
        [StringReplace(TempFile, '\', '/', [rfReplaceAll])]),
      'plt.close()',
      '',
      'print("Graphique généré")'
    ]);

    // Charger l'image
    if FileExists(TempFile) then
    begin
      FImage.Picture.LoadFromFile(TempFile);
      FMemo.Lines.Add('Graphique affiché avec succès');
      DeleteFile(TempFile);
    end
    else
      FMemo.Lines.Add('Erreur: fichier non créé');

  except
    on E: Exception do
      FMemo.Lines.Add('Erreur: ' + E.Message);
  end;
end;

var
  MainForm: TMainForm;

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
```

## Performance et optimisation

### Mesurer les performances

```pascal
program BenchmarkPythonPascal;

{$mode objfpc}{$H+}

uses
  SysUtils, DateUtils, PythonEngine, Variants, Math;

var
  Engine: TPythonEngine;
  StartTime, EndTime: TDateTime;
  i, n: Integer;
  ResultPython, ResultPascal: Double;

function CalculPascal(x: Double): Double;
begin
  Result := Sin(x) * Cos(x) + Sqrt(Abs(x));
end;

begin
  Engine := TPythonEngine.Create(nil);
  try
    Engine.LoadDll;

    n := 1000000;
    WriteLn(Format('Benchmark avec %d itérations', [n]));
    WriteLn;

    // Benchmark Pascal
    WriteLn('Test en Pascal pur:');
    StartTime := Now;
    ResultPascal := 0;
    for i := 1 to n do
      ResultPascal := ResultPascal + CalculPascal(i * 0.001);
    EndTime := Now;
    WriteLn(Format('  Temps: %.3f secondes',
      [MilliSecondsBetween(EndTime, StartTime) / 1000]));
    WriteLn(Format('  Résultat: %.6f', [ResultPascal]));
    WriteLn;

    // Benchmark Python (via embedding)
    WriteLn('Test en Python (via embedding):');
    Engine.ExecStrings([
      'import math',
      'def calcul(x):',
      '    return math.sin(x) * math.cos(x) + math.sqrt(abs(x))'
    ]);

    StartTime := Now;
    ResultPython := 0;
    Engine.ExecString(Format('result = sum(calcul(i * 0.001) for i in range(1, %d))', [n]));
    ResultPython := Engine.GetMainModule.result;
    EndTime := Now;
    WriteLn(Format('  Temps: %.3f secondes',
      [MilliSecondsBetween(EndTime, StartTime) / 1000]));
    WriteLn(Format('  Résultat: %.6f', [ResultPython]));
    WriteLn;

    // Benchmark Python (NumPy vectorisé)
    WriteLn('Test en Python avec NumPy (vectorisé):');
    StartTime := Now;
    Engine.ExecStrings([
      'import numpy as np',
      Format('x = np.arange(1, %d) * 0.001', [n]),
      'result_np = np.sum(np.sin(x) * np.cos(x) + np.sqrt(np.abs(x)))'
    ]);
    ResultPython := Engine.GetMainModule.result_np;
    EndTime := Now;
    WriteLn(Format('  Temps: %.3f secondes',
      [MilliSecondsBetween(EndTime, StartTime) / 1000]));
    WriteLn(Format('  Résultat: %.6f', [ResultPython]));

  finally
    Engine.Free;
  end;

  WriteLn;
  WriteLn('Conclusion:');
  WriteLn('  Pascal pur: Le plus rapide pour ce type de calcul');
  WriteLn('  Python via embedding: Plus lent à cause de l''overhead');
  WriteLn('  NumPy vectorisé: Très rapide grâce à l''optimisation C');
  WriteLn;
  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
```

### Stratégies d'optimisation

**1. Minimiser les appels Python**
```pascal
// ❌ Mauvais: appel Python dans une boucle
for i := 1 to 10000 do
  Result := Result + PythonFunction(i);

// ✅ Bon: traiter le tableau en une fois en Python
Engine.ExecString('result = sum(process_value(i) for i in range(10000))');
Result := Engine.GetMainModule.result;
```

**2. Utiliser NumPy pour les calculs vectoriels**
```python
# ❌ Lent: boucle Python
result = [x**2 for x in range(1000000)]

# ✅ Rapide: NumPy vectorisé
import numpy as np
result = np.arange(1000000) ** 2
```

**3. Cache des résultats Python**
```pascal
// Charger une fois, utiliser plusieurs fois
Engine.ExecStrings([
  'import heavy_module',
  'data = heavy_module.load_data()'
]);

// Réutiliser 'data' sans recharger
for i := 1 to 100 do
  Engine.ExecString(Format('result_%d = process(data, %d)', [i, i]));
```

**4. Parallélisation**
```python
# Utiliser multiprocessing en Python
from multiprocessing import Pool

def process_data(chunk):
    return [x**2 for x in chunk]

# Traiter en parallèle
with Pool(4) as p:
    results = p.map(process_data, data_chunks)
```

## Débogage et dépannage

### Problèmes courants

#### Problème 1 : Python non trouvé

**Symptôme** :
```
Error: Unable to load Python DLL
```

**Solutions** :
```pascal
// Spécifier le chemin explicitement
Engine.DllPath := 'C:\Python311\';
Engine.DllName := 'python311.dll';
Engine.LoadDll;

// Ou sous Linux
Engine.DllName := 'libpython3.11.so';
```

#### Problème 2 : Module Python non trouvé

**Symptôme** :
```
ModuleNotFoundError: No module named 'numpy'
```

**Solutions** :
```python
# Installer le module
pip install numpy

# Ou spécifier le chemin Python
import sys
sys.path.append('/chemin/vers/modules')
```

#### Problème 3 : Encodage des chaînes

**Symptôme** :
Caractères accentués mal affichés

**Solution** :
```pascal
// S'assurer de l'encodage UTF-8
Engine.ExecStrings([
  '# -*- coding: utf-8 -*-',
  'texte = "Texte avec des accents: éàç"',
  'print(texte)'
]);
```

#### Problème 4 : Fuites mémoire

**Diagnostic** :
```pascal
// Activer le débogage Python
Engine.ExecStrings([
  'import sys',
  'import gc',
  'gc.set_debug(gc.DEBUG_LEAK)'
]);
```

**Solution** :
```pascal
// Toujours libérer les ressources
try
  Engine.ExecString('...');
finally
  Engine.Free;
end;
```

### Outils de débogage

**1. Activer les traces Python**
```pascal
Engine.ExecStrings([
  'import sys',
  'sys.settrace(lambda *args: print(args))'
]);
```

**2. Capturer les exceptions Python**
```pascal
try
  Engine.ExecString(Code);
except
  on E: EPythonError do
  begin
    WriteLn('Exception Python:');
    WriteLn('  Message: ', E.Message);
    WriteLn('  Traceback:');
    WriteLn(Engine.Traceback.Text);
  end;
end;
```

**3. Inspecter les variables Python**
```pascal
Engine.ExecStrings([
  'import pprint',
  'pprint.pprint(locals())'
]);
```

## Checklist de déploiement

Avant de distribuer votre application Pascal/Python :

- [ ] Version Python spécifiée (ex: >= 3.9)
- [ ] Liste complète des dépendances Python (requirements.txt)
- [ ] Instructions d'installation Python claires
- [ ] Gestion des erreurs si Python absent
- [ ] Tests sur Windows et Linux
- [ ] Documentation des fonctions Python utilisées
- [ ] Exemples de code fournis
- [ ] Performance acceptable mesurée
- [ ] Pas de fuites mémoire (testé)
- [ ] Logs pour le débogage
- [ ] Gestion propre des erreurs Python
- [ ] Script d'installation automatique (optionnel)

## Conclusion

L'interfaçage entre FreePascal et Python offre le meilleur des deux mondes : **la performance et la robustesse de Pascal** combinées à **la flexibilité et l'écosystème riche de Python**.

### Récapitulatif des approches

| Approche | Avantages | Inconvénients | Cas d'usage |
|----------|-----------|---------------|-------------|
| **Embedding Python** | - Accès direct à l'écosystème Python<br>- Code Python modifiable sans recompilation<br>- Parfait pour les plugins | - Overhead de performance<br>- Dépendance Python runtime<br>- Gestion mémoire complexe | Applications avec système de plugins, scripting utilisateur, utilisation de bibliothèques ML/data science |
| **Extension Pascal** | - Performance native<br>- Pas de runtime Python nécessaire côté Pascal<br>- Intégration transparente | - Création de bindings nécessaire<br>- Moins flexible<br>- Débogage plus complexe | Optimisation de code critique en Pascal, bibliothèques réutilisables, modules de calcul intensif |
| **IPC (Sockets/Pipes)** | - Isolation complète<br>- Langages totalement indépendants<br>- Scalabilité (processus distribués) | - Latence réseau<br>- Sérialisation/désérialisation<br>- Plus complexe à mettre en place | Microservices, applications distribuées, séparation stricte des responsabilités |

### Quand choisir quelle approche ?

#### Utilisez l'embedding Python si :
- Vous avez besoin d'utiliser des bibliothèques Python avancées (NumPy, TensorFlow, etc.)
- Vous voulez offrir un système de plugins/scripts aux utilisateurs
- Vous prototypez rapidement et voulez tester des algorithmes Python
- La performance n'est pas absolument critique
- Vous êtes prêt à distribuer Python avec votre application

**Exemple typique** : Application de traitement de données avec visualisations Python (Matplotlib), analyse via Pandas, ou modèles ML

#### Utilisez les extensions Pascal si :
- Vous devez optimiser du code critique en performance
- Python est votre langage principal mais certaines parties nécessitent des optimisations
- Vous voulez créer une bibliothèque réutilisable depuis Python
- Vous avez du code Pascal existant à réutiliser
- La portabilité binaire est importante

**Exemple typique** : Module de calcul scientifique intensif, traitement d'images en temps réel, algorithmes de cryptographie

#### Utilisez l'IPC si :
- Vous voulez une séparation complète entre les composants
- Les processus peuvent être sur des machines différentes
- Vous avez besoin de haute disponibilité (redémarrage indépendant)
- Vous développez une architecture microservices
- Les langages doivent évoluer indépendamment

**Exemple typique** : Architecture distribuée, API REST, services web, systèmes scalables

### Exemple complet : Application de synthèse

Voici un exemple qui combine les trois approches pour créer une application complète.

#### Architecture du système

```
┌────────────────────────────────────────────────────┐
│           Application FreePascal (GUI)             │
│                                                    │
│  ┌──────────────┐  ┌──────────────┐              │
│  │  Interface   │  │   Logique    │              │
│  │  Utilisateur │  │  Métier      │              │
│  └──────┬───────┘  └───────┬──────┘              │
│         │                   │                      │
│         │    ┌──────────────┴──────┐              │
│         │    │  Module Python      │              │
│         │    │  (Embedding)        │              │
│         │    │  - Visualisation    │              │
│         │    │  - Machine Learning │              │
│         │    └─────────────────────┘              │
│         │                                          │
│  ┌──────┴─────────────────────────────────────┐  │
│  │  Bibliothèque Pascal (.so/.dll)            │  │
│  │  - Calculs intensifs                       │  │
│  │  - Traitement d'images                     │  │
│  │  - Algorithmes optimisés                   │  │
│  └────────────────────────────────────────────┘  │
│                                                    │
└───────────────────────┬────────────────────────────┘
                        │
                        │ IPC (REST/JSON)
                        │
         ┌──────────────┴──────────────┐
         │  Service Python distant     │
         │  - Base de données          │
         │  - API externes             │
         │  - Traitement asynchrone    │
         └─────────────────────────────┘
```

#### Fichier requirements.txt

```
# Dépendances Python pour l'application
numpy>=1.21.0
pandas>=1.3.0
matplotlib>=3.4.0
scikit-learn>=1.0.0
requests>=2.26.0
flask>=2.0.0
```

#### Script d'installation automatique

**install_dependencies.pas** :
```pascal
program InstallDependencies;

{$mode objfpc}{$H+}

uses
  SysUtils, Process;

function ExecuterCommande(const Cmd: string): Integer;
var
  Proc: TProcess;
begin
  Proc := TProcess.Create(nil);
  try
    Proc.Executable := {$IFDEF WINDOWS}'cmd.exe'{$ELSE}'/bin/bash'{$ENDIF};
    Proc.Parameters.Add({$IFDEF WINDOWS}'/c'{$ELSE}'-c'{$ENDIF});
    Proc.Parameters.Add(Cmd);
    Proc.Options := Proc.Options + [poWaitOnExit];
    Proc.Execute;
    Result := Proc.ExitStatus;
  finally
    Proc.Free;
  end;
end;

function PythonEstInstalle: Boolean;
begin
  Result := ExecuterCommande('python --version') = 0;
  if not Result then
    Result := ExecuterCommande('python3 --version') = 0;
end;

procedure InstallerDependances;
var
  PythonCmd: string;
begin
  WriteLn('Vérification de Python...');

  if not PythonEstInstalle then
  begin
    WriteLn('ERREUR: Python n''est pas installé !');
    WriteLn('Veuillez installer Python 3.9 ou supérieur depuis python.org');
    Halt(1);
  end;

  WriteLn('Python détecté ✓');
  WriteLn;

  // Déterminer la commande Python
  {$IFDEF WINDOWS}
  PythonCmd := 'python';
  {$ELSE}
  PythonCmd := 'python3';
  {$ENDIF}

  WriteLn('Installation des dépendances Python...');
  WriteLn('Cela peut prendre quelques minutes...');
  WriteLn;

  if ExecuterCommande(PythonCmd + ' -m pip install --upgrade pip') <> 0 then
  begin
    WriteLn('ATTENTION: Mise à jour de pip échouée');
  end;

  if ExecuterCommande(PythonCmd + ' -m pip install -r requirements.txt') = 0 then
  begin
    WriteLn;
    WriteLn('Installation réussie ! ✓');
    WriteLn;
    WriteLn('Dépendances installées :');
    ExecuterCommande(PythonCmd + ' -m pip list | grep -E "numpy|pandas|matplotlib|scikit-learn|requests|flask"');
  end
  else
  begin
    WriteLn;
    WriteLn('ERREUR: Échec de l''installation');
    WriteLn('Essayez manuellement : ' + PythonCmd + ' -m pip install -r requirements.txt');
    Halt(1);
  end;
end;

begin
  WriteLn('=== Installation des dépendances Python ===');
  WriteLn;

  try
    InstallerDependances;
  except
    on E: Exception do
    begin
      WriteLn('Erreur: ', E.Message);
      Halt(1);
    end;
  end;

  WriteLn;
  WriteLn('Appuyez sur Entrée pour continuer...');
  ReadLn;
end.
```

### Documentation utilisateur

**README.md** :
````markdown
# Application FreePascal/Python

Cette application combine FreePascal et Python pour offrir performance et flexibilité.

## Prérequis

- FreePascal 3.2.0 ou supérieur
- Python 3.9 ou supérieur
- Système : Windows 10+ ou Linux (Ubuntu 20.04+)

## Installation

### 1. Installer Python

**Windows:**
```bash
# Télécharger depuis python.org
# Ou via Chocolatey
choco install python
```

**Linux:**
```bash
sudo apt update
sudo apt install python3 python3-pip python3-dev
```

### 2. Installer les dépendances

```bash
# Automatique
fpc install_dependencies.pas
./install_dependencies

# Ou manuel
pip install -r requirements.txt
```

### 3. Compiler l'application

```bash
# Windows
fpc -oapp.exe main.pas

# Linux
fpc -o app main.pas
```

## Utilisation

### Mode interface graphique
```bash
./app
```

### Mode ligne de commande
```bash
./app --cli --input data.csv --output results.json
```

### Exemples

**Analyser des données:**
```bash
./app --analyze sales_data.csv
```

**Générer des visualisations:**
```bash
./app --plot --input data.csv --output graph.png
```

**Prédiction ML:**
```bash
./app --predict --model trained_model.pkl --data new_data.csv
```

## Architecture

L'application utilise trois types d'interfaçage:

1. **Embedding Python** : Pour la visualisation et le ML
2. **Extension Pascal** : Pour les calculs intensifs (libcalc.so)
3. **Service REST** : Pour la communication avec le backend

## Dépannage

### Python non trouvé
````
Erreur: Unable to load Python DLL
```
**Solution**: Ajoutez Python au PATH ou définissez PYTHONHOME

### Module manquant
```
ModuleNotFoundError: No module named 'numpy'
```
**Solution**: `pip install numpy`

### Performances lentes
- Vérifiez que NumPy utilise BLAS optimisé
- Activez le mode release: `fpc -O3`
- Utilisez la vectorisation NumPy

## Support

- Documentation: https://docs.example.com
- Issues: https://github.com/user/project/issues
- Email: support@example.com

## Licence

MIT License - voir LICENSE.md
```

### Tests automatisés

**test_integration.pas** :
```pascal
program TestIntegration;

{$mode objfpc}{$H+}

uses
  SysUtils, PythonEngine, Variants;

var
  TestsPasses: Integer = 0;
  TestsTotal: Integer = 0;

procedure Test(const Nom: string; Condition: Boolean);
begin
  Inc(TestsTotal);
  Write('Test ', TestsTotal, ': ', Nom, ' ... ');

  if Condition then
  begin
    WriteLn('OK ✓');
    Inc(TestsPasses);
  end
  else
    WriteLn('ÉCHEC ✗');
end;

procedure TesterPython;
var
  Engine: TPythonEngine;
  Resultat: Variant;
begin
  WriteLn('=== Tests Python ===');
  WriteLn;

  Engine := TPythonEngine.Create(nil);
  try
    // Test 1: Chargement Python
    try
      Engine.LoadDll;
      Test('Chargement de Python', True);
    except
      Test('Chargement de Python', False);
      Exit;
    end;

    // Test 2: Exécution de code simple
    try
      Engine.ExecString('x = 2 + 2');
      Resultat := Engine.GetMainModule.x;
      Test('Exécution code simple', Integer(Resultat) = 4);
    except
      Test('Exécution code simple', False);
    end;

    // Test 3: Import NumPy
    try
      Engine.ExecString('import numpy as np');
      Test('Import NumPy', True);
    except
      Test('Import NumPy', False);
    end;

    // Test 4: Calcul NumPy
    try
      Engine.ExecStrings([
        'import numpy as np',
        'arr = np.array([1, 2, 3, 4, 5])',
        'moyenne = float(np.mean(arr))'
      ]);
      Resultat := Engine.GetMainModule.moyenne;
      Test('Calcul NumPy', Abs(Double(Resultat) - 3.0) < 0.001);
    except
      Test('Calcul NumPy', False);
    end;

    // Test 5: Gestion des erreurs
    try
      Engine.ExecString('x = 1 / 0');
      Test('Gestion erreur division par zéro', False);
    except
      on E: EPythonError do
        Test('Gestion erreur division par zéro', True);
    end;

  finally
    Engine.Free;
  end;
end;

procedure TesterExtensionPascal;
begin
  WriteLn;
  WriteLn('=== Tests Extension Pascal ===');
  WriteLn;

  // Vérifier que la bibliothèque existe
  {$IFDEF WINDOWS}
  Test('Bibliothèque mathpas.dll existe', FileExists('mathpas.dll'));
  {$ELSE}
  Test('Bibliothèque libmathpas.so existe', FileExists('libmathpas.so'));
  {$ENDIF}

  // TODO: Ajouter tests de chargement dynamique
end;

begin
  WriteLn('╔════════════════════════════════════════╗');
  WriteLn('║   Tests d''intégration Pascal/Python   ║');
  WriteLn('╚════════════════════════════════════════╝');
  WriteLn;

  TesterPython;
  TesterExtensionPascal;

  WriteLn;
  WriteLn('═══════════════════════════════════════════');
  WriteLn(Format('Résultats: %d/%d tests réussis', [TestsPasses, TestsTotal]));

  if TestsPasses = TestsTotal then
  begin
    WriteLn('Tous les tests sont passés ! ✓');
    Halt(0);
  end
  else
  begin
    WriteLn(Format('Échec de %d test(s) ✗', [TestsTotal - TestsPasses]));
    Halt(1);
  end;
end.
```

### Ressources et apprentissage continu

#### Documentation officielle

**Python4Delphi/Lazarus** :
- GitHub : https://github.com/pyscripter/python4delphi
- Wiki : https://github.com/pyscripter/python4delphi/wiki
- Exemples : https://github.com/pyscripter/python4delphi/tree/master/Demos

**Python C API** :
- Documentation : https://docs.python.org/3/c-api/
- Tutorial : https://docs.python.org/3/extending/

**ctypes Python** :
- Documentation : https://docs.python.org/3/library/ctypes.html
- Tutorial : https://docs.python.org/3/library/ctypes.html#module-ctypes

#### Livres recommandés

1. **"Python for Lazarus Developers"** (si disponible)
2. **"Python C Extensions"** - Pour comprendre l'interfaçage bas niveau
3. **"Mastering Python Design Patterns"** - Pour les architectures hybrides

#### Tutoriels et articles

- Forum FreePascal - Section "Python Integration"
- Lazarus Forum - Topics sur Python4Lazarus
- Stack Overflow - Tags `freepascal` + `python`
- Reddit - r/freepascal, r/lazarus

#### Projets open source à étudier

1. **PythonForLazarus-Demos** : Exemples officiels
2. **fpGUI-Python** : Interface graphique avec Python
3. **LazDataAnalysis** : Analyse de données Pascal/Python

### Évolutions futures

#### Tendances émergentes

**1. WebAssembly**
- Compiler FreePascal en WASM
- Exécuter Python dans le navigateur (Pyodide)
- Applications web hybrides Pascal/Python

**2. Machine Learning embarqué**
- TensorFlow Lite depuis Pascal
- ONNX Runtime intégration
- Edge computing avec Python

**3. Cloud et containers**
- Docker avec Pascal + Python
- Kubernetes pour déploiement
- Serverless avec fonctions Pascal

**4. Data Science**
- Intégration Jupyter Notebooks
- Dashboards interactifs (Plotly Dash)
- Pipeline de données Pascal/Python

### Points clés à retenir

#### Avantages de l'approche hybride

✅ **Performance** : Code critique en Pascal, prototypage en Python  
✅ **Écosystème** : Accès aux bibliothèques Python tout en gardant Pascal  
✅ **Flexibilité** : Choisir le bon outil pour chaque tâche  
✅ **Maintenabilité** : Code séparé par responsabilité  
✅ **Évolutivité** : Faire évoluer chaque partie indépendamment

#### Défis à anticiper

⚠️ **Complexité** : Plus de technologies = plus à maintenir  
⚠️ **Débogage** : Tracer les bugs à travers les langages  
⚠️ **Performance** : Overhead de communication entre langages  
⚠️ **Déploiement** : Gérer les dépendances multiples  
⚠️ **Formation** : Équipe doit maîtriser les deux langages

### Derniers conseils pratiques

1. **Commencez simple** : Un seul type d'interfaçage à la fois
2. **Testez tôt** : Vérifiez l'interfaçage dès le début du projet
3. **Documentez bien** : L'interface entre langages doit être claire
4. **Mesurez** : Profiling pour valider les choix de performance
5. **Isolez** : Gardez l'interfaçage dans des modules dédiés
6. **Versionnez** : Gérez les versions de Python et des dépendances
7. **Automatisez** : Scripts d'installation et de test
8. **Communiquez** : Documentation pour les utilisateurs finaux

### Conclusion finale

L'interfaçage entre FreePascal et Python représente une approche puissante et pragmatique du développement logiciel moderne. En combinant :

- **La solidité et la performance de Pascal** pour le code critique et l'interface utilisateur
- **La richesse et la flexibilité de Python** pour l'analyse de données, le machine learning et le prototypage rapide
- **Les techniques d'interopérabilité modernes** (embedding, extensions, IPC)

Vous pouvez créer des applications qui tirent le meilleur parti des deux mondes.

Cette approche hybride est particulièrement pertinente pour :
- Les applications scientifiques et d'analyse de données
- Les systèmes nécessitant à la fois performance et intelligence artificielle
- Les projets où différentes expertises doivent collaborer
- Le portage progressif de code legacy vers des technologies modernes

**Le message essentiel** : Ne voyez pas Pascal et Python comme des alternatives, mais comme des **partenaires complémentaires**. Utilisez chacun là où il excelle, et créez des ponts solides entre eux.

Avec les connaissances acquises dans ce chapitre, vous avez maintenant tous les outils pour créer des applications innovantes qui combinent performance native et écosystème moderne. À vous de jouer ! 🚀

---

## Pour aller plus loin

- **Chapitre 19.4** : COM/ActiveX sous Windows
- **Chapitre 19.5** : D-Bus sous Linux
- **Chapitre 15** : Intelligence Artificielle et Machine Learning (approfondit l'utilisation de Python pour l'IA)
- **Chapitre 9** : Programmation Web (pour les APIs REST Pascal/Python)

**Ressources complémentaires en ligne** :
- Forum FreePascal : https://forum.lazarus.freepascal.org
- Python4Lazarus GitHub : https://github.com/pyscripter/python4delphi
- Wiki FreePascal : https://wiki.freepascal.org

⏭️ [COM/ActiveX sous Windows](/19-interoperabilite-bindings/04-com-activex-windows.md)
