🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 19.1 Création de bibliothèques partagées

## Introduction

Une **bibliothèque partagée** (shared library) est un fichier contenant du code compilé qui peut être utilisé par plusieurs programmes simultanément. Contrairement aux bibliothèques statiques qui sont incorporées dans chaque exécutable, les bibliothèques partagées restent séparées et sont chargées en mémoire au moment de l'exécution.

### Pourquoi créer des bibliothèques partagées ?

#### 1. Réutilisation du code

Vous pouvez écrire du code une fois en FreePascal et l'utiliser depuis :
- D'autres applications FreePascal
- Applications écrites en C, C++, Python, Java, C#, etc.
- Scripts shell ou de scripting
- Applications web via FFI

**Exemple concret** : Vous développez un algorithme complexe de traitement d'images en FreePascal. Plutôt que de le réécrire dans chaque langage, vous créez une bibliothèque partagée utilisable partout.

#### 2. Mise à jour centralisée

Quand vous corrigez un bug ou améliorez une fonctionnalité :
- Un seul fichier à mettre à jour
- Tous les programmes qui l'utilisent bénéficient automatiquement des améliorations
- Pas besoin de recompiler les applications clientes

```
Avant (bibliothèque statique) :
App1.exe (contient le code)    → Mise à jour : recompiler App1
App2.exe (contient le code)    → Mise à jour : recompiler App2
App3.exe (contient le code)    → Mise à jour : recompiler App3

Après (bibliothèque partagée) :
App1.exe → mylib.dll
App2.exe → mylib.dll            → Mise à jour : remplacer mylib.dll
App3.exe → mylib.dll
```

#### 3. Économie de mémoire et d'espace disque

Une seule copie du code en mémoire pour tous les programmes :

```
RAM avec bibliothèques statiques :
┌─────────────┐
│   App1      │  ← 5 MB (dont 2 MB de code lib)
├─────────────┤
│   App2      │  ← 5 MB (dont 2 MB de code lib)
├─────────────┤
│   App3      │  ← 5 MB (dont 2 MB de code lib)
└─────────────┘
Total : 15 MB

RAM avec bibliothèques partagées :
┌─────────────┐
│   App1      │  ← 3 MB
├─────────────┤
│   App2      │  ← 3 MB
├─────────────┤
│   App3      │  ← 3 MB
├─────────────┤
│ mylib.dll   │  ← 2 MB (partagée)
└─────────────┘
Total : 11 MB
```

#### 4. Plugins et extensibilité

Les bibliothèques partagées sont le mécanisme standard pour créer des systèmes de plugins :
- Application principale légère
- Fonctionnalités optionnelles dans des plugins
- Les utilisateurs choisissent ce qu'ils installent
- Développement de plugins par des tiers

**Exemples** : navigateurs web (extensions), éditeurs de texte (plugins), DAW audio (VST)

#### 5. Protection de la propriété intellectuelle

Le code source n'est pas distribué, seulement le binaire compilé :
- Les algorithmes restent cachés
- Décompilation difficile
- Possibilité de licence commerciale

### Terminologie selon les plateformes

Bien que le concept soit le même, chaque système d'exploitation utilise sa propre terminologie et son propre format :

| Plateforme | Extension | Nom complet | Notes |
|------------|-----------|-------------|-------|
| **Windows** | `.dll` | Dynamic Link Library | Peut aussi être `.ocx` (ActiveX), `.drv` (driver) |
| **Linux** | `.so` | Shared Object | Souvent préfixé par `lib` : `libmylib.so` |
| **macOS** | `.dylib` | Dynamic Library | Aussi `.framework` pour les bundles |
| **BSD** | `.so` | Shared Object | Similaire à Linux |

**Note importante** : Sur Linux/Unix, par convention, les bibliothèques sont nommées `libXXX.so`, mais lors du chargement, on peut omettre le préfixe `lib` et l'extension.

### Architecture d'une bibliothèque partagée

#### Vue d'ensemble

```
┌─────────────────────────────────────┐
│   Bibliothèque partagée (mylib)     │
├─────────────────────────────────────┤
│                                     │
│  ┌───────────────────────────────┐  │
│  │  Fonctions exportées (API)    │  │  ← Interface publique
│  └───────────────────────────────┘  │
│                                     │
│  ┌───────────────────────────────┐  │
│  │  Fonctions internes           │  │  ← Code privé
│  │  (non exportées)              │  │
│  └───────────────────────────────┘  │
│                                     │
│  ┌───────────────────────────────┐  │
│  │  Variables globales           │  │  ← État partagé
│  └───────────────────────────────┘  │
│                                     │
│  ┌───────────────────────────────┐  │
│  │  Types et structures          │  │  ← Définitions
│  └───────────────────────────────┘  │
│                                     │
└─────────────────────────────────────┘
```

#### Les parties d'une bibliothèque

**1. L'interface publique (API)**

Ce sont les fonctions et procédures que vous exposez au monde extérieur. Elles doivent être :
- Bien documentées
- Stables (compatibilité entre versions)
- Simples à utiliser
- Sans dépendances complexes

**2. L'implémentation privée**

Le code interne qui fait le travail réel :
- Peut être modifié librement entre les versions
- Peut utiliser des types Pascal complexes
- Optimisations internes

**3. La gestion de l'état**

Les bibliothèques peuvent être :
- **Stateless (sans état)** : chaque appel est indépendant
- **Stateful (avec état)** : maintient des informations entre les appels

```pascal
// Exemple sans état - fonction pure
function Additionner(a, b: Integer): Integer;
begin
  Result := a + b;  // Pas de variable globale
end;

// Exemple avec état - compteur global
var
  CompteurAppels: Integer = 0;

function IncrémenterCompteur(): Integer;
begin
  Inc(CompteurAppels);
  Result := CompteurAppels;
end;
```

### Différences entre bibliothèques statiques et partagées

#### Bibliothèque statique

**Caractéristiques** :
- Extension : `.a` (Unix), `.lib` (Windows)
- Liée au moment de la compilation
- Code incorporé dans l'exécutable final
- Pas de dépendance externe au runtime

**Avantages** :
- Déploiement simple (un seul fichier exécutable)
- Pas de problème de version de bibliothèque
- Optimisation possible entre bibliothèque et code appelant

**Inconvénients** :
- Taille de l'exécutable augmentée
- Mises à jour nécessitent une recompilation
- Mémoire dupliquée si plusieurs programmes utilisent la même bibliothèque

#### Bibliothèque partagée

**Caractéristiques** :
- Extension : `.dll`, `.so`, `.dylib`
- Liée au moment de l'exécution
- Fichier séparé de l'exécutable
- Dépendance externe nécessaire

**Avantages** :
- Mises à jour sans recompilation
- Économie de mémoire et d'espace disque
- Plugins et extensibilité
- Partage entre applications

**Inconvénients** :
- Gestion des dépendances (DLL hell)
- Problèmes de versions
- Déploiement plus complexe

### Processus de création d'une bibliothèque partagée

Le développement d'une bibliothèque partagée suit généralement ces étapes :

#### Étape 1 : Conception de l'API

Définir l'interface publique :
- Quelles fonctions exposer ?
- Quels types de paramètres ?
- Comment gérer les erreurs ?
- Quelle convention d'appel utiliser ?

**Questions à se poser** :
- Qui utilisera cette bibliothèque ? (FreePascal uniquement ? Autres langages ?)
- L'API doit-elle être compatible C ?
- Y aura-t-il plusieurs versions en parallèle ?

#### Étape 2 : Implémentation

Écrire le code en FreePascal :
- Fonctions exportées avec la bonne convention
- Code interne optimisé
- Gestion des erreurs robuste
- Tests unitaires

#### Étape 3 : Compilation

Compiler avec les bonnes options :
```bash
# Windows - Créer une DLL
fpc -o mylib.dll mylib.pas

# Linux - Créer une SO
fpc -o libmylib.so mylib.pas
```

#### Étape 4 : Documentation

Documenter l'API pour les utilisateurs :
- Prototype des fonctions
- Description des paramètres
- Valeurs de retour
- Exemples d'utilisation
- Gestion des erreurs

#### Étape 5 : Distribution

Packager et distribuer :
- Fichier de bibliothèque (.dll, .so)
- Fichiers d'en-tête pour C/C++ (optionnel)
- Documentation
- Exemples de code

### Conventions d'appel : un concept crucial

La **convention d'appel** (calling convention) définit comment les paramètres sont passés et comment la pile est nettoyée après l'appel.

#### Pourquoi c'est important ?

Si l'appelant et la fonction utilisent des conventions différentes :
- Corruption de la pile
- Crash de l'application
- Paramètres incorrects
- Valeurs de retour erronées

#### Les principales conventions

**cdecl (C declaration)**
- Standard C
- L'appelant nettoie la pile
- Support des fonctions variadiques (nombre variable de paramètres)
- Utilisée sur Linux par défaut

```pascal
function ma_fonction(x: Integer): Integer; cdecl;
```

**stdcall (Standard call)**
- Standard Windows
- La fonction appelée nettoie la pile
- Plus efficace que cdecl mais pas de fonctions variadiques
- Utilisée par l'API Windows (Win32 API)

```pascal
function ma_fonction(x: Integer): Integer; stdcall;
```

**register (Register call)**
- Convention Pascal native
- Utilise les registres CPU quand possible
- La plus efficace mais non portable vers d'autres langages

```pascal
function ma_fonction(x: Integer): Integer; register;
```

**Comment choisir ?**
- Pour une bibliothèque **utilisable par C/C++** : `cdecl` (universel)
- Pour une bibliothèque **Windows uniquement** : `stdcall` (compatible API Windows)
- Pour une bibliothèque **FreePascal uniquement** : `register` (performance)

### Types compatibles entre langages

Pour qu'une bibliothèque soit utilisable depuis d'autres langages, utilisez des types simples :

#### Types sûrs (toujours compatibles)

```pascal
// Types de base
Integer, LongInt    // 32 bits signé
Int64               // 64 bits signé
Single              // Flottant 32 bits
Double              // Flottant 64 bits
Byte                // 8 bits non signé
Word                // 16 bits non signé
LongWord            // 32 bits non signé
QWord               // 64 bits non signé

// Pointeurs
PChar               // Chaîne C
Pointer             // Pointeur générique
```

#### Types à éviter dans l'API publique

```pascal
// Types Pascal spécifiques
string              // Structure interne Pascal
AnsiString          // Gestion mémoire Pascal
UnicodeString       // Peut varier selon la plateforme
TObject             // Classes Pascal
set of              // Sets Pascal
dynamic arrays      // Tableaux dynamiques Pascal
```

**À la place, utilisez** :
```pascal
// Pour les chaînes
PChar, PAnsiChar, PWideChar

// Pour les tableaux
Pointer + longueur séparée
array[0..MAX] of Type

// Pour les structures complexes
Handles opaques (pointeur vers structure interne)
```

### Gestion de la mémoire : le défi majeur

La mémoire est le point le plus délicat lors de la création de bibliothèques partagées.

#### Règle d'or

**Celui qui alloue doit libérer.**

```
┌────────────────┐         ┌────────────────┐
│  Application   │         │  Bibliothèque  │
└────────────────┘         └────────────────┘
        │                           │
        │  1. Appel fonction        │
        │──────────────────────────▶│
        │                           │
        │                           │  2. Allocation
        │                           │     mémoire
        │                           │
        │  3. Retour pointeur       │
        │◀──────────────────────────│
        │                           │
        │  4. Utilisation           │
        │                           │
        │  5. Appel free()          │
        │──────────────────────────▶│
        │                           │
        │                           │  6. Libération
        │                           │
```

#### Stratégies de gestion mémoire

**Stratégie 1 : L'appelant alloue, la bibliothèque remplit**

```pascal
// Prototype
procedure GetData(buffer: PByte; size: Integer); cdecl;

// Utilisation côté client
var
  buffer: array[0..1023] of Byte;
begin
  GetData(@buffer[0], SizeOf(buffer));
end;
```

**Avantages** : Pas de problème de libération  
**Inconvénients** : Le client doit connaître la taille nécessaire  

**Stratégie 2 : La bibliothèque alloue et fournit un destructeur**

```pascal
// Allocation
function CreateObject(): Pointer; cdecl;

// Libération
procedure DestroyObject(obj: Pointer); cdecl;

// Utilisation
var
  obj: Pointer;
begin
  obj := CreateObject();
  try
    // Utilisation de obj
  finally
    DestroyObject(obj);
  end;
end;
```

**Avantages** : Taille dynamique, encapsulation  
**Inconvénients** : Deux fonctions nécessaires  

**Stratégie 3 : Handles opaques**

```pascal
type
  TMyHandle = type Pointer;

function CreateHandle(): TMyHandle; cdecl;
procedure CloseHandle(h: TMyHandle); cdecl;
function UseHandle(h: TMyHandle; data: Integer): Integer; cdecl;
```

**Avantages** : Abstraction complète, type-safe  
**Inconvénients** : Plus verbeux  

### Versioning et compatibilité

Une bibliothèque évolue au fil du temps. Il faut gérer les versions.

#### Versionnage sémantique (SemVer)

Format : `MAJOR.MINOR.PATCH`
- **MAJOR** : changements incompatibles dans l'API
- **MINOR** : ajout de fonctionnalités compatible
- **PATCH** : corrections de bugs compatibles

Exemple : `libmylib.so.2.3.1`

#### Types de compatibilité

**Compatibilité binaire (ABI)**
- Ajout de nouvelles fonctions : ✅ OK
- Modification de signatures existantes : ❌ CASSE
- Suppression de fonctions : ❌ CASSE
- Modification de l'ordre des paramètres : ❌ CASSE

**Compatibilité source (API)**
- Renommage de fonctions : ❌ CASSE le code source
- Ajout de paramètres optionnels : Peut-être ✅ selon l'implémentation

#### Bonnes pratiques

1. **Ne jamais modifier une fonction existante**
   - Créez plutôt une nouvelle version : `GetDataV2()`

2. **Marquer les fonctions obsolètes**
   ```pascal
   function OldFunction(): Integer; cdecl; deprecated;
   ```

3. **Documenter les changements**
   - Fichier CHANGELOG.md
   - Notes de version

4. **Tester la compatibilité**
   - Tests de régression
   - Tests avec anciennes versions clientes

### Structure d'un projet de bibliothèque

Organisation recommandée :

```
mylib/
├── src/
│   ├── mylib.pas           # Code principal
│   ├── mylib_internal.pas  # Code interne
│   └── mylib_types.pas     # Types partagés
├── include/
│   └── mylib.h             # Header C (si applicable)
├── examples/
│   ├── pascal/
│   │   └── test.pas        # Exemple Pascal
│   ├── c/
│   │   └── test.c          # Exemple C
│   └── python/
│       └── test.py         # Exemple Python
├── tests/
│   └── test_mylib.pas      # Tests unitaires
├── docs/
│   ├── api.md              # Documentation API
│   └── tutorial.md         # Guide d'utilisation
├── build/
│   └── Makefile            # Script de compilation
└── README.md               # Vue d'ensemble
```

### Outils nécessaires

Pour créer des bibliothèques partagées, vous aurez besoin de :

#### Compilateur FreePascal
- Version récente recommandée
- Support multi-plateforme

#### Outils système

**Windows**
- SDK Windows (pour les en-têtes système)
- Éditeur de ressources (pour les métadonnées DLL)

**Linux**
- GCC (pour tester l'interopérabilité C)
- `ldd` : pour vérifier les dépendances
- `nm` : pour lister les symboles exportés
- `objdump` : pour analyser les binaires

**macOS**
- Xcode Command Line Tools
- `otool` : équivalent de `ldd`

#### Outils de débogage

- GDB : débogueur universel
- Valgrind : détection de fuites mémoire (Linux)
- Dependency Walker : analyse des dépendances DLL (Windows)

### Erreurs courantes à éviter

#### 1. Oublier d'exporter les fonctions

```pascal
// ❌ Fonction non exportée
function MaFonction(): Integer;

// ✅ Fonction exportée
function MaFonction(): Integer; cdecl; export;
```

#### 2. Mélanger les conventions d'appel

```pascal
// ❌ Application appelle en cdecl, bibliothèque en stdcall
function Func(): Integer; stdcall;  // Bibliothèque

// Client appelle sans préciser
Result := Func();  // Utilise la convention par défaut (probablement register)
```

#### 3. Exposer des types Pascal complexes

```pascal
// ❌ Non portable
function GetText(): string; cdecl;

// ✅ Portable
function GetText(buffer: PChar; size: Integer): Integer; cdecl;
```

#### 4. Problèmes de mémoire partagée

```pascal
// ❌ Alloué par la lib, libéré par l'appelant avec un autre allocateur
function AllocateData(): Pointer; cdecl;
begin
  GetMem(Result, 1024);  // Allocateur Pascal
end;

// Client en C fait :
void* data = AllocateData();
free(data);  // Allocateur C - ERREUR !
```

#### 5. Ignorer le multithreading

Si votre bibliothèque sera utilisée par plusieurs threads :
- Protégez l'état partagé (mutexes, critical sections)
- Documentez si elle est thread-safe ou non
- Évitez les variables globales mutables

### Philosophie de conception

Lors de la création d'une bibliothèque partagée, gardez en tête :

#### Principe KISS (Keep It Simple, Stupid)

Une API simple est :
- Plus facile à documenter
- Plus facile à utiliser
- Moins sujette aux erreurs
- Plus stable dans le temps

#### Principe du moindre étonnement

L'API doit se comporter comme l'utilisateur s'y attend :
- Nommage cohérent
- Comportement prévisible
- Conventions standard

#### Encapsulation

Cachez les détails d'implémentation :
- N'exposez que ce qui est nécessaire
- Utilisez des handles opaques
- Permettez l'évolution interne

### Préparation pour la suite

Les sections suivantes détailleront :
- **19.1.1** : Spécificités des DLL Windows
- **19.1.2** : Spécificités des Shared Objects Linux

Chaque plateforme a ses particularités, mais les concepts fondamentaux que nous venons de voir s'appliquent partout.

## Conclusion

Créer des bibliothèques partagées est une compétence puissante qui permet :
- De réutiliser votre code FreePascal partout
- D'optimiser les ressources système
- De créer des architectures modulaires
- D'interagir avec l'écosystème logiciel plus large

Les concepts clés à retenir :
- ✅ Convention d'appel appropriée
- ✅ Types compatibles entre langages
- ✅ Gestion rigoureuse de la mémoire
- ✅ API simple et stable
- ✅ Documentation complète

Dans les sections suivantes, nous passerons à la pratique avec des exemples concrets pour Windows et Linux.

⏭️ [DLL Windows et exports](/19-interoperabilite-bindings/01.1-dll-windows-exports.md)
