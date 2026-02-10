🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 15.1 Bindings TensorFlow pour FreePascal

## Introduction

TensorFlow est une bibliothèque open-source développée par Google pour le calcul numérique et l'apprentissage automatique (Machine Learning). Bien que TensorFlow soit principalement conçu pour Python et C++, il est possible de l'utiliser avec FreePascal grâce à des **bindings** - des interfaces qui permettent à un langage de communiquer avec des bibliothèques écrites dans un autre langage.

### Qu'est-ce qu'un binding ?

Un binding est comme un traducteur entre deux langues différentes. Il permet à votre code FreePascal de "parler" avec les fonctions de TensorFlow qui sont écrites en C/C++. Concrètement, il s'agit de fichiers Pascal qui déclarent les fonctions de la bibliothèque TensorFlow et permettent de les appeler depuis votre programme.

## Pourquoi utiliser TensorFlow avec FreePascal ?

Même si Python est le langage le plus populaire pour le Machine Learning, FreePascal présente plusieurs avantages :

- **Performance** : FreePascal compile en code natif très rapide
- **Typage fort** : Moins d'erreurs à l'exécution grâce au système de types strict
- **Déploiement** : Créer des exécutables autonomes sans dépendances Python
- **Intégration** : Incorporer de l'IA dans vos applications Lazarus existantes
- **Multi-plateforme** : Utiliser le même code sur Windows et Ubuntu

## Architecture et principe de fonctionnement

### La bibliothèque C de TensorFlow

TensorFlow fournit une API C (`libtensorflow.so` sur Linux, `tensorflow.dll` sur Windows) qui sert de pont entre le cœur de TensorFlow (écrit en C++) et d'autres langages. C'est cette API C que nous allons utiliser depuis FreePascal.

```
┌─────────────────────────┐
│   Votre code Pascal     │
└───────────┬─────────────┘
            │
            ↓
┌─────────────────────────┐
│  Bindings FreePascal    │  ← Traduction des appels
└───────────┬─────────────┘
            │
            ↓
┌─────────────────────────┐
│   TensorFlow C API      │  ← libtensorflow.so / .dll
└───────────┬─────────────┘
            │
            ↓
┌─────────────────────────┐
│  TensorFlow Core (C++)  │  ← Le moteur principal
└─────────────────────────┘
```

## Installation des prérequis

### Sur Windows

1. **Télécharger TensorFlow C Library**
   - Rendez-vous sur [https://www.tensorflow.org/install/lang_c](https://www.tensorflow.org/install/lang_c)
   - Téléchargez la version Windows (CPU ou GPU selon vos besoins)
   - Exemple : `libtensorflow-cpu-windows-x86_64-2.x.x.zip`

2. **Extraction et configuration**
   ```
   C:\TensorFlow\
   ├── lib\
   │   └── tensorflow.dll
   └── include\
       └── tensorflow\
           └── c\
               └── c_api.h
   ```

3. **Configurer le PATH**
   - Ajoutez `C:\TensorFlow\lib` à votre variable d'environnement PATH
   - Ou copiez `tensorflow.dll` dans le répertoire de votre exécutable

### Sur Ubuntu/Linux

1. **Installation via le gestionnaire de paquets** (plus simple)
   ```bash
   # Méthode 1 : Via apt (si disponible)
   sudo apt update
   sudo apt install libtensorflow-dev
   ```

2. **Installation manuelle** (pour version plus récente)
   ```bash
   # Télécharger
   wget https://storage.googleapis.com/tensorflow/libtensorflow/libtensorflow-cpu-linux-x86_64-2.x.x.tar.gz

   # Extraire
   sudo tar -C /usr/local -xzf libtensorflow-cpu-linux-x86_64-2.x.x.tar.gz

   # Configurer le linker
   sudo ldconfig /usr/local/lib
   ```

3. **Vérifier l'installation**
   ```bash
   # La bibliothèque devrait être dans :
   ls /usr/local/lib/libtensorflow.so
   # ou
   ls /usr/lib/x86_64-linux-gnu/libtensorflow.so
   ```

## Structure des bindings FreePascal

### Fichier de base : TensorFlow_C_API.pas

Les bindings consistent principalement en un fichier unité Pascal qui déclare toutes les fonctions de l'API C de TensorFlow. Voici la structure typique :

```pascal
unit TensorFlow_C_API;

{$mode objfpc}{$H+}

interface

uses
  ctypes;

const
  {$IFDEF WINDOWS}
  TensorFlowLib = 'tensorflow.dll';
  {$ENDIF}
  {$IFDEF UNIX}
  TensorFlowLib = 'libtensorflow.so';
  {$ENDIF}

type
  // Types opaques (pointeurs vers structures internes)
  PTF_Status = Pointer;
  PTF_Tensor = Pointer;
  PTF_Graph = Pointer;
  PTF_Session = Pointer;
  PTF_SessionOptions = Pointer;

  // Énumérations
  TF_DataType = (
    TF_FLOAT = 1,
    TF_DOUBLE = 2,
    TF_INT32 = 3,
    TF_UINT8 = 4,
    TF_INT16 = 5,
    TF_INT8 = 6,
    TF_STRING = 7,
    TF_INT64 = 9
    // ... autres types
  );

  TF_Code = (
    TF_OK = 0,
    TF_CANCELLED = 1,
    TF_UNKNOWN = 2,
    TF_INVALID_ARGUMENT = 3,
    TF_DEADLINE_EXCEEDED = 4
    // ... autres codes d'erreur
  );

// Déclarations des fonctions externes
function TF_Version(): PChar; cdecl; external TensorFlowLib;

function TF_NewStatus(): PTF_Status; cdecl; external TensorFlowLib;  
procedure TF_DeleteStatus(status: PTF_Status); cdecl; external TensorFlowLib;  
function TF_GetCode(status: PTF_Status): TF_Code; cdecl; external TensorFlowLib;

function TF_NewTensor(dtype: TF_DataType; dims: PInt64; num_dims: Integer;
  data: Pointer; len: csize_t; deallocator: Pointer; deallocator_arg: Pointer): PTF_Tensor;
  cdecl; external TensorFlowLib;

procedure TF_DeleteTensor(tensor: PTF_Tensor); cdecl; external TensorFlowLib;

// ... beaucoup d'autres fonctions

implementation

end.
```

### Explication des éléments clés

#### 1. Les directives de mode

```pascal
{$mode objfpc}{$H+}
```
- `{$mode objfpc}` : Active le mode Object Pascal de FreePascal
- `{$H+}` : Utilise les chaînes longues (AnsiString) par défaut

#### 2. La compilation conditionnelle

```pascal
{$IFDEF WINDOWS}
  TensorFlowLib = 'tensorflow.dll';
{$ENDIF}
{$IFDEF UNIX}
  TensorFlowLib = 'libtensorflow.so';
{$ENDIF}
```
Cette section permet d'utiliser le bon nom de bibliothèque selon le système d'exploitation.

#### 3. Les types opaques

```pascal
PTF_Status = Pointer;  
PTF_Tensor = Pointer;
```
TensorFlow utilise des structures internes dont nous n'avons pas besoin de connaître les détails. On les manipule via des pointeurs opaques.

#### 4. La déclaration externe

```pascal
function TF_Version(): PChar; cdecl; external TensorFlowLib;
```
- `cdecl` : Convention d'appel C (important !)
- `external TensorFlowLib` : La fonction se trouve dans la bibliothèque externe

## Utilisation basique des bindings

### Exemple 1 : Vérifier la version de TensorFlow

```pascal
program VersionTF;

uses
  TensorFlow_C_API;

var
  version: PChar;
begin
  version := TF_Version();
  WriteLn('Version de TensorFlow : ', version);

  ReadLn;
end.
```

**Sortie attendue :**
```
Version de TensorFlow : 2.14.0
```

### Exemple 2 : Créer et manipuler un tenseur simple

Un **tenseur** est la structure de données fondamentale de TensorFlow. C'est un tableau multidimensionnel.

```pascal
program SimpleTensor;

uses
  TensorFlow_C_API;

var
  tensor: PTF_Tensor;
  data: array[0..5] of Single;
  dims: array[0..1] of Int64;
  i: Integer;
  tensorData: PSingle;
begin
  // Initialiser les données : [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]
  for i := 0 to 5 do
    data[i] := i + 1.0;

  // Définir les dimensions : matrice 2x3
  dims[0] := 2;  // 2 lignes
  dims[1] := 3;  // 3 colonnes

  // Créer le tenseur
  tensor := TF_NewTensor(
    TF_FLOAT,              // Type de données : float
    @dims[0],              // Pointeur vers les dimensions
    2,                     // Nombre de dimensions
    @data[0],              // Pointeur vers les données
    SizeOf(data),          // Taille des données en octets
    nil,                   // Pas de fonction de libération personnalisée
    nil                    // Pas d'argument pour le deallocator
  );

  if tensor <> nil then
  begin
    WriteLn('Tenseur créé avec succès !');
    WriteLn('Type : TF_FLOAT');
    WriteLn('Dimensions : 2x3');

    // Libérer le tenseur
    TF_DeleteTensor(tensor);
    WriteLn('Tenseur libéré.');
  end
  else
    WriteLn('Erreur lors de la création du tenseur.');

  ReadLn;
end.
```

### Exemple 3 : Gestion des erreurs avec TF_Status

TensorFlow utilise un objet `TF_Status` pour signaler les erreurs. Voici comment l'utiliser :

```pascal
program StatusExample;

uses
  TensorFlow_C_API;

var
  status: PTF_Status;
  code: TF_Code;
  message: PChar;
begin
  // Créer un objet status
  status := TF_NewStatus();

  // Vérifier le code (devrait être TF_OK initialement)
  code := TF_GetCode(status);

  WriteLn('Code de statut : ', Ord(code));

  if code = TF_OK then
    WriteLn('Statut : OK')
  else
  begin
    message := TF_Message(status);
    WriteLn('Erreur : ', message);
  end;

  // Toujours libérer le status
  TF_DeleteStatus(status);

  ReadLn;
end.
```

## Concepts importants pour débuter

### 1. Gestion de la mémoire

**Règle d'or** : Tout ce que vous créez avec `TF_New...()`, vous devez le libérer avec `TF_Delete...()`.

```pascal
var
  status: PTF_Status;
  tensor: PTF_Tensor;
begin
  // Créer
  status := TF_NewStatus();
  tensor := TF_NewTensor(...);

  try
    // Utiliser...
  finally
    // TOUJOURS libérer
    TF_DeleteTensor(tensor);
    TF_DeleteStatus(status);
  end;
end;
```

### 2. Convention d'appel cdecl

Toutes les fonctions TensorFlow utilisent la convention d'appel C (`cdecl`). C'est crucial pour que l'interface fonctionne correctement. Ne jamais oublier ce mot-clé dans les déclarations.

### 3. Types de données

Les types TensorFlow ne correspondent pas toujours exactement aux types Pascal :

| TensorFlow | FreePascal | Description |
|------------|------------|-------------|
| TF_FLOAT | Single | Réel simple précision (32 bits) |
| TF_DOUBLE | Double | Réel double précision (64 bits) |
| TF_INT32 | Integer / LongInt | Entier 32 bits signé |
| TF_INT64 | Int64 | Entier 64 bits signé |
| TF_UINT8 | Byte | Octet non signé |
| TF_STRING | - | Chaînes (manipulation spéciale) |

### 4. Ordre des dimensions

TensorFlow utilise l'ordre "row-major" (par ligne), comme C. Pour une matrice 2x3 :
```
dims[0] = 2  // nombre de lignes  
dims[1] = 3  // nombre de colonnes
```

## Sources et disponibilité des bindings

### Bindings communautaires

Il n'existe pas (encore) de binding officiel TensorFlow pour FreePascal, mais la communauté a créé plusieurs projets :

1. **Bindings manuels** : Vous pouvez créer vos propres bindings en traduisant les fichiers d'en-tête C
   - Fichier source : `tensorflow/c/c_api.h` dans le code source TensorFlow
   - Traduire les déclarations C en Pascal

2. **Projets GitHub** : Cherchez "tensorflow freepascal" ou "tensorflow pascal" sur GitHub
   - Certains développeurs ont partagé leurs bindings
   - Vérifiez la version de TensorFlow supportée

3. **Alternative : TensorFlow Lite**
   - Plus simple et plus léger que TensorFlow complet
   - Idéal pour l'inférence (utiliser des modèles déjà entraînés)
   - Bindings potentiellement plus faciles à créer

### Créer ses propres bindings (avancé)

Pour les développeurs avancés, voici les étapes pour créer vos propres bindings :

1. **Télécharger les sources TensorFlow**
2. **Analyser `tensorflow/c/c_api.h`**
3. **Traduire en Pascal** :
   - Les `struct` deviennent des pointeurs opaques
   - Les `enum` deviennent des énumérations Pascal
   - Les fonctions gardent les mêmes signatures avec `cdecl; external`

4. **Tester chaque fonction** individuellement

## Limitations et considérations

### Limitations actuelles

- **Pas de binding officiel** : Maintenance à votre charge
- **API bas niveau** : Plus verbeux que Python
- **Documentation** : Principalement orientée Python/C++
- **Communauté** : Plus petite que pour Python

### Quand utiliser TensorFlow avec FreePascal ?

**Cas d'usage appropriés :**
- Intégrer un modèle IA dans une application Lazarus existante
- Déployer des modèles entraînés (inférence uniquement)
- Applications nécessitant des performances maximales
- Systèmes embarqués avec FreePascal

**Quand préférer Python :**
- Entraînement de modèles complexes
- Expérimentation et recherche
- Prototypage rapide
- Accès aux dernières fonctionnalités TensorFlow

## Alternatives et compléments

### 1. Utiliser Python depuis FreePascal

Vous pouvez appeler du code Python depuis FreePascal avec **Python4Lazarus** :

```pascal
uses
  PythonEngine;

var
  PythonEngine: TPythonEngine;
begin
  PythonEngine := TPythonEngine.Create(nil);
  PythonEngine.LoadDll;
  PythonEngine.ExecString('import tensorflow as tf');
  PythonEngine.ExecString('print(tf.__version__)');
end;
```

### 2. TensorFlow Lite

Version allégée de TensorFlow, optimisée pour l'inférence :
- Plus simple à interfacer
- Fichiers de modèles plus petits (.tflite)
- Parfait pour les applications de production

### 3. ONNX (Open Neural Network Exchange)

Format standard d'échange de modèles IA :
- Créer des bindings pour ONNX Runtime
- Compatible avec de nombreux frameworks (PyTorch, TensorFlow, etc.)
- API C plus simple

## Exemple complet : Charger et utiliser un modèle

Voici un exemple conceptuel montrant comment on pourrait charger un modèle TensorFlow pré-entraîné :

```pascal
program LoadModel;

uses
  TensorFlow_C_API;

var
  status: PTF_Status;
  graph: PTF_Graph;
  session: PTF_Session;
  sessionOpts: PTF_SessionOptions;
begin
  status := TF_NewStatus();

  try
    // 1. Créer un graphe
    graph := TF_NewGraph();

    // 2. Charger le modèle depuis un fichier
    // (Fonction TF_LoadSessionFromSavedModel ou similaire)

    // 3. Créer les options de session
    sessionOpts := TF_NewSessionOptions();

    // 4. Créer une session
    session := TF_NewSession(graph, sessionOpts, status);

    if TF_GetCode(status) = TF_OK then
    begin
      WriteLn('Modèle chargé avec succès !');

      // 5. Faire des prédictions
      // (Utiliser TF_SessionRun)

      // 6. Fermer la session
      TF_CloseSession(session, status);
      TF_DeleteSession(session, status);
    end
    else
      WriteLn('Erreur : ', TF_Message(status));

  finally
    TF_DeleteSessionOptions(sessionOpts);
    TF_DeleteGraph(graph);
    TF_DeleteStatus(status);
  end;

  ReadLn;
end.
```

## Ressources et documentation

### Documentation officielle TensorFlow

- **TensorFlow C API** : [https://www.tensorflow.org/install/lang_c](https://www.tensorflow.org/install/lang_c)
- **Fichier d'en-tête de référence** : `tensorflow/c/c_api.h` dans les sources
- **Guide d'installation** : Documentation officielle TensorFlow

### Communauté FreePascal

- **Forum FreePascal** : [https://forum.lazarus.freepascal.org/](https://forum.lazarus.freepascal.org/)
- **Wiki FreePascal** : Recherchez "bindings" et "external libraries"
- **GitHub** : Recherchez des projets existants

### Livres et tutoriels

- Documentation TensorFlow (adaptable pour comprendre les concepts)
- Tutoriels sur les bindings C en FreePascal
- Guides sur l'interfaçage avec des bibliothèques externes

## Conclusion

Les bindings TensorFlow pour FreePascal permettent d'intégrer des capacités d'intelligence artificielle dans vos applications Pascal, en particulier pour :

- **L'inférence** (utiliser des modèles déjà entraînés)
- **Le déploiement** d'applications IA performantes
- **L'intégration** dans des applications Lazarus multi-plateformes

Bien que l'écosystème soit moins mature qu'en Python, FreePascal offre des avantages uniques en termes de performance, de typage et de déploiement. Pour débuter, concentrez-vous sur l'utilisation de modèles pré-entraînés plutôt que sur l'entraînement, et n'hésitez pas à combiner FreePascal avec Python quand c'est pertinent.

La clé du succès est de bien comprendre la gestion mémoire, les conventions d'appel C, et de commencer par des exemples simples avant de passer à des cas d'usage plus complexes.

---

**Prochaines étapes suggérées :**
- Explorer TensorFlow Lite comme alternative plus légère
- Apprendre à créer des bindings pour d'autres bibliothèques C
- Étudier les réseaux de neurones from scratch (chapitre 15.2)
- Découvrir l'intégration Python4Lazarus pour combiner les deux mondes

⏭️ [Réseaux de neurones from scratch](/15-intelligence-artificielle-machine-learning/02-reseaux-neurones-from-scratch.md)
