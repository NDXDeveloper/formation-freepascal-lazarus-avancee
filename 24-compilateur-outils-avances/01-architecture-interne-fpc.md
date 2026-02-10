🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 24.1 Architecture interne du FPC (Free Pascal Compiler)

## Introduction

Le compilateur Free Pascal (FPC) est un compilateur complexe mais élégant qui transforme votre code Pascal en programmes exécutables. Comprendre son architecture interne vous permettra de mieux utiliser le compilateur, d'optimiser vos programmes et même de contribuer au projet FreePascal.

Dans ce chapitre, nous allons explorer les coulisses du FPC de manière accessible, même si vous n'avez jamais étudié la théorie des compilateurs.

---

## Vue d'ensemble du processus de compilation

Avant de plonger dans les détails, voyons le parcours général d'un fichier source Pascal jusqu'au programme exécutable :

```
Code source (.pas)
      ↓
   Scanner (Lexer)
      ↓
   Tokens
      ↓
   Parser
      ↓
   AST (Arbre de Syntaxe Abstraite)
      ↓
   Analyse sémantique
      ↓
   Représentation intermédiaire
      ↓
   Optimisations
      ↓
   Génération de code
      ↓
   Code assembleur
      ↓
   Assembleur
      ↓
   Code objet (.o / .obj)
      ↓
   Éditeur de liens (Linker)
      ↓
   Exécutable final
```

Chaque étape a un rôle précis. Explorons-les une par une.

---

## 1. Le Scanner (Analyse Lexicale)

### Qu'est-ce que c'est ?

Le scanner, aussi appelé **analyseur lexical** ou **lexer**, est la première étape de la compilation. Son rôle est de lire votre fichier source caractère par caractère et de le découper en unités significatives appelées **tokens** (jetons).

### Comment ça fonctionne ?

Imaginez que vous lisez une phrase en français. Vous ne lisez pas lettre par lettre, vous identifiez des mots. Le scanner fait la même chose avec votre code.

**Exemple de code source :**
```pascal
program Hello;  
var
  x: Integer;
begin
  x := 42;
  WriteLn('Valeur: ', x);
end.
```

**Tokens générés :**
- `program` → Mot-clé
- `Hello` → Identifiant
- `;` → Séparateur
- `var` → Mot-clé
- `x` → Identifiant
- `:` → Séparateur
- `Integer` → Identifiant de type
- `:=` → Opérateur d'affectation
- `42` → Littéral numérique
- `'Valeur: '` → Chaîne de caractères
- etc.

### Fichiers sources concernés dans FPC

Dans le code source de FPC, le scanner se trouve principalement dans :
- `compiler/scanner.pas` - Le scanner principal
- `compiler/tokens.pas` - Définitions des tokens

### Points importants

- Le scanner élimine les espaces et les commentaires (sauf si options spéciales)
- Il reconnaît les directives de compilation (`{$...}`)
- Il gère les différents modes de compatibilité (Turbo Pascal, Delphi, etc.)

---

## 2. Le Parser (Analyse Syntaxique)

### Qu'est-ce que c'est ?

Le **parser** (analyseur syntaxique) prend la séquence de tokens et vérifie qu'elle respecte la grammaire du langage Pascal. Il construit également une structure de données appelée **AST** (Abstract Syntax Tree - Arbre de Syntaxe Abstraite).

### Analogie simple

Pensez à la grammaire française : "Le chat mange la souris" est correct, mais "Chat le souris la mange" ne l'est pas, même si les mots sont valides. Le parser vérifie que votre code respecte les règles du Pascal.

### L'arbre de syntaxe abstraite (AST)

L'AST représente la structure hiérarchique de votre programme.

**Exemple de code :**
```pascal
if x > 10 then
  WriteLn('Grand');
```

**Représentation en AST (simplifiée) :**
```
IfStatement
├── Condition: BinaryExpression
│   ├── Left: Variable(x)
│   ├── Operator: >
│   └── Right: Constant(10)
└── ThenPart: CallStatement
    ├── Procedure: WriteLn
    └── Arguments: ['Grand']
```

### Fichiers sources dans FPC

- `compiler/pparser.pas` - Parser principal
- `compiler/pstatmnt.pas` - Analyse des instructions
- `compiler/pexpr.pas` - Analyse des expressions
- `compiler/ptype.pas` - Analyse des types
- `compiler/pdecl.pas` - Analyse des déclarations

### Grammaire du Pascal

FPC utilise une grammaire **LL(1)** modifiée avec quelques extensions. Cela signifie que le parser peut généralement décider quelle règle appliquer en regardant seulement le prochain token.

---

## 3. Analyse Sémantique

### Qu'est-ce que c'est ?

L'**analyse sémantique** vérifie que votre programme a du sens au-delà de la simple syntaxe. C'est ici que le compilateur vérifie les types, la portée des variables, etc.

### Exemples de vérifications sémantiques

**Vérification de types :**
```pascal
var
  x: Integer;
  s: String;
begin
  x := 'Bonjour';  // ERREUR : type incompatible
end.
```

**Vérification de portée :**
```pascal
procedure Test;  
var
  local: Integer;
begin
  local := 5;
end;

begin
  WriteLn(local);  // ERREUR : 'local' n'existe pas ici
end.
```

**Vérification d'utilisation :**
```pascal
var
  x: Integer;
begin
  WriteLn(x);  // AVERTISSEMENT : variable peut-être non initialisée
end.
```

### Table des symboles

Le compilateur maintient une **table des symboles** qui contient toutes les informations sur :
- Les variables et leur type
- Les procédures et fonctions
- Les types définis
- Les constantes
- Les unités importées

### Fichiers sources dans FPC

- `compiler/symtable.pas` - Gestion de la table des symboles
- `compiler/symdef.pas` - Définitions des symboles
- `compiler/htypechk.pas` - Vérification de types

---

## 4. Représentation Intermédiaire

### Qu'est-ce que c'est ?

Après l'analyse sémantique, le FPC transforme l'AST en une **représentation intermédiaire** (IR - Intermediate Representation). C'est un format plus proche du code machine mais encore indépendant de l'architecture cible.

### Pourquoi une représentation intermédiaire ?

- **Indépendance de la plateforme** : Le même IR peut générer du code pour x86, ARM, x86-64, etc.
- **Optimisations** : Il est plus facile d'optimiser sur l'IR que sur l'AST ou le code machine
- **Maintenance** : Ajouter un nouveau backend (cible) est plus simple

### Nœuds de l'arbre intermédiaire

FPC utilise des **nodes** (nœuds) pour représenter les opérations. Chaque nœud représente une opération élémentaire.

**Types de nœuds principaux :**

- `loadn` - Chargement d'une valeur
- `assignn` - Affectation
- `addn` - Addition
- `calln` - Appel de procédure/fonction
- `ifn` - Instruction conditionnelle
- `whilen` - Boucle while
- etc.

**Exemple :**

Code source :
```pascal
x := y + 5;
```

Représentation en nœuds (simplifié) :
```
assignn
├── left: loadn(x)
└── right: addn
    ├── left: loadn(y)
    └── right: constloadn(5)
```

### Fichiers sources dans FPC

- `compiler/node.pas` - Définitions de base des nœuds
- `compiler/nld.pas` - Nœuds de chargement
- `compiler/nadd.pas` - Nœuds d'addition
- `compiler/ncal.pas` - Nœuds d'appel
- `compiler/nflw.pas` - Nœuds de flux de contrôle

---

## 5. Optimisations

### Qu'est-ce que c'est ?

Le **module d'optimisation** améliore le code pour qu'il soit plus rapide ou plus compact, sans changer son comportement.

### Types d'optimisations dans FPC

#### Optimisations au niveau des nœuds

**Pliage de constantes (Constant Folding) :**
```pascal
x := 2 + 3;  // Optimisé en : x := 5;
```

**Propagation de constantes (Constant Propagation) :**
```pascal
const
  PI = 3.14159;
var
  circonference: Double;
begin
  circonference := 2 * PI * rayon;
  // Le compilateur remplace PI par sa valeur
end.
```

**Élimination de code mort (Dead Code Elimination) :**
```pascal
if False then
  WriteLn('Ceci ne sera jamais exécuté');  // Supprimé
```

#### Optimisations au niveau du flux de contrôle

**Simplification de boucles :**
```pascal
// Avant optimisation
for i := 1 to 1 do
  DoSomething;

// Après optimisation
DoSomething;  // La boucle n'itère qu'une fois
```

#### Optimisations de bas niveau

- **Inlining** : Remplacer un appel de fonction par son code
- **Register allocation** : Utiliser au maximum les registres CPU
- **Peephole optimizations** : Remplacer des séquences d'instructions inefficaces

### Niveaux d'optimisation

FPC offre plusieurs niveaux d'optimisation :

- `-O-` : Pas d'optimisation (rapide à compiler, utile pour déboguer)
- `-O1` : Optimisations de base
- `-O2` : Optimisations standard (recommandé)
- `-O3` : Optimisations agressives
- `-O4` : Optimisations maximales (peut augmenter la taille du code)

### Fichiers sources dans FPC

- `compiler/optbase.pas` - Base des optimisations
- `compiler/optloop.pas` - Optimisation des boucles
- `compiler/optssa.pas` - Optimisations SSA (Static Single Assignment)

---

## 6. Génération de Code

### Qu'est-ce que c'est ?

La **génération de code** transforme la représentation intermédiaire en code assembleur spécifique à l'architecture cible (x86, ARM, etc.).

### Architecture du générateur de code

FPC utilise une approche modulaire avec des **backends** pour chaque architecture :

- **x86** : Processeurs Intel/AMD 32 bits
- **x86-64** : Processeurs Intel/AMD 64 bits
- **ARM** : Processeurs ARM (smartphones, Raspberry Pi)
- **PowerPC** : Anciens Mac, consoles
- **SPARC** : Serveurs Sun/Oracle
- **MIPS** : Routeurs, systèmes embarqués
- **AVR** : Microcontrôleurs Arduino
- etc.

### Exemple de génération

**Code Pascal :**
```pascal
x := x + 1;
```

**Code assembleur x86 généré (simplifié) :**
```asm
mov eax, [x]      ; Charger x dans le registre eax  
add eax, 1        ; Ajouter 1  
mov [x], eax      ; Stocker le résultat dans x
```

**Code assembleur ARM généré (simplifié) :**
```asm
ldr r0, [x]       ; Charger x dans le registre r0  
add r0, r0, #1    ; Ajouter 1  
str r0, [x]       ; Stocker le résultat dans x
```

### Allocation de registres

Les registres CPU sont des emplacements de mémoire ultra-rapides mais limités en nombre. Le générateur de code doit décider intelligemment quelles variables mettre dans les registres.

**Registres x86-64 :**
- `rax`, `rbx`, `rcx`, `rdx` : Registres généraux
- `rsi`, `rdi` : Index/pointeurs
- `rsp`, `rbp` : Pile (stack)
- `r8` à `r15` : Registres étendus (64 bits uniquement)

### Conventions d'appel

Chaque plateforme a ses propres **conventions d'appel** qui définissent comment passer les paramètres aux fonctions :

**Windows x86-64 :**
- Les 4 premiers paramètres entiers vont dans : `rcx`, `rdx`, `r8`, `r9`
- Les suivants sur la pile

**Linux x86-64 (System V) :**
- Les 6 premiers paramètres entiers vont dans : `rdi`, `rsi`, `rdx`, `rcx`, `r8`, `r9`
- Les suivants sur la pile

FPC gère automatiquement ces différences !

### Fichiers sources dans FPC

- `compiler/cgbase.pas` - Base de la génération de code
- `compiler/cgobj.pas` - Générateur orienté objet
- `compiler/x86/cgcpu.pas` - Générateur pour x86
- `compiler/arm/cgcpu.pas` - Générateur pour ARM
- `compiler/regvars.pas` - Allocation de registres

---

## 7. L'assembleur interne

### Qu'est-ce que c'est ?

FPC dispose d'un **assembleur interne** qui transforme le code assembleur généré en code machine binaire (code objet).

### Pourquoi un assembleur interne ?

- **Indépendance** : Pas besoin d'installer des outils externes
- **Rapidité** : Plus rapide que d'appeler un assembleur externe
- **Portabilité** : Fonctionne sur toutes les plateformes

### Alternative : assembleur externe

FPC peut aussi utiliser des assembleurs externes si configuré :
- **NASM** (Netwide Assembler)
- **GAS** (GNU Assembler)
- **MASM** (Microsoft Macro Assembler)

Option : `-a` pour activer l'assembleur externe

### Fichiers sources dans FPC

- `compiler/assemble.pas` - Interface assembleur
- `compiler/aggas.pas` - Format GAS
- `compiler/x86/agx86.pas` - Assembleur x86

---

## 8. L'éditeur de liens (Linker)

### Qu'est-ce que c'est ?

L'**éditeur de liens** (linker) combine tous les fichiers objets (.o, .obj) et les bibliothèques pour créer l'exécutable final.

### Son rôle

1. **Résolution des symboles** : Lier les appels de fonctions à leur implémentation
2. **Combinaison des sections** : Code, données, tables
3. **Relocalisation** : Ajuster les adresses mémoire
4. **Génération de l'exécutable** : Créer le fichier .exe, .elf, etc.

### Linkers utilisés par FPC

**Sur Windows :**
- **Linker interne** (par défaut)
- **Microsoft Link** (optionnel)

**Sur Linux/Ubuntu :**
- **GNU ld** (linker GNU)
- **Gold** (linker moderne, plus rapide)

**Sur macOS :**
- **ld** (linker d'Apple)

### Fichiers objets et bibliothèques

**Fichiers objets :**
- `.o` (Unix/Linux)
- `.obj` (Windows)

**Bibliothèques statiques :**
- `.a` (Unix/Linux)
- `.lib` (Windows)

**Bibliothèques dynamiques :**
- `.so` (Unix/Linux)
- `.dll` (Windows)
- `.dylib` (macOS)

### Fichiers sources dans FPC

- `compiler/link.pas` - Interface du linker
- `compiler/t_linux.pas` - Linker Linux
- `compiler/t_win.pas` - Linker Windows

---

## 9. Gestion de la mémoire dans le compilateur

### Table des symboles

Le compilateur maintient en mémoire toutes les informations sur votre programme :
- Variables, fonctions, types
- Portée (scope)
- Informations de type

### Gestion efficace

FPC utilise des techniques avancées :
- **Hash tables** pour recherche rapide des symboles
- **Listes chaînées** pour les scopes imbriqués
- **Pools de mémoire** pour allocation/désallocation rapide

---

## 10. Architecture modulaire du FPC

### Unités principales

Le compilateur lui-même est écrit en Pascal et organisé en unités :

| Unité | Rôle |
|-------|------|
| `scanner.pas` | Analyse lexicale |
| `pparser.pas` | Analyse syntaxique |
| `symtable.pas` | Table des symboles |
| `node.pas` | Arbre intermédiaire |
| `cgbase.pas` | Génération de code |
| `systems.pas` | Configuration des systèmes cibles |

### Hiérarchie des modules

```
compiler/
├── scanner.pas          (Lexer)
├── pparser.pas          (Parser principal)
├── pexpr.pas            (Expressions)
├── pstatmnt.pas         (Instructions)
├── symtable.pas         (Symboles)
├── node.pas             (Nœuds IR)
├── pass_1.pas           (Passe sémantique)
├── optbase.pas          (Optimisations)
├── cgbase.pas           (Génération de code)
├── x86/
│   ├── cgcpu.pas        (Générateur x86)
│   └── agx86.pas        (Assembleur x86)
└── link.pas             (Linker)
```

---

## 11. Processus de compilation multi-passes

### Qu'est-ce qu'une passe ?

Une **passe** (pass) est un parcours complet de la représentation du programme. FPC utilise plusieurs passes :

### Passe 1 : Construction de l'arbre

- Lecture du source
- Création de l'AST
- Construction de la table des symboles

### Passe 2 : Analyse sémantique

- Vérification des types
- Résolution des symboles
- Vérification de la portée

### Passes d'optimisation

- Optimisations au niveau nœuds
- Optimisations au niveau flux
- Optimisations peephole

### Dernière passe : Génération finale

- Génération du code assembleur
- Assemblage
- Liaison

---

## 12. Gestion des unités et dépendances

### Compilation séparée

FPC compile chaque unité séparément et stocke l'interface compilée dans des fichiers `.ppu` (Pascal Unit).

**Avantages :**
- Compilation incrémentale (recompiler seulement ce qui a changé)
- Temps de compilation réduit
- Modularité

### Fichiers .ppu

Les fichiers `.ppu` contiennent :
- L'interface de l'unité compilée
- Les informations de type
- Les symboles exportés
- La version du compilateur utilisé

### Résolution des dépendances

Le compilateur utilise un graphe de dépendances pour déterminer l'ordre de compilation :

```
Unit A → Unit B → Unit D
      → Unit C → Unit D
```

FPC détecte et refuse les dépendances circulaires.

---

## 13. Cross-compilation

### Qu'est-ce que c'est ?

La **cross-compilation** permet de compiler sur une plateforme (ex: Linux) un programme pour une autre plateforme (ex: Windows).

### Comment FPC le permet

Grâce à son architecture modulaire :
1. Le frontend (scanner, parser) est indépendant de la cible
2. Le backend (génération de code) est spécifique à chaque cible
3. Les bibliothèques runtime existent pour chaque plateforme

### Configuration

```bash
# Compiler depuis Linux vers Windows 64 bits
fpc -Twin64 monprogramme.pas

# Compiler depuis Windows vers Linux ARM
fpc -Tlinux -PARM monprogramme.pas
```

**Options importantes :**
- `-T<target>` : Système cible (linux, win64, darwin, etc.)
- `-P<processor>` : Processeur cible (x86_64, ARM, i386, etc.)

---

## 14. Modes de compatibilité

### Différents dialectes Pascal

FPC peut compiler plusieurs "dialectes" de Pascal :

#### Mode FPC (par défaut)

Le mode natif de Free Pascal avec toutes les extensions modernes.

```pascal
{$mode fpc}
```

#### Mode Turbo Pascal

Compatible avec l'ancien Turbo Pascal de Borland (années 1980-90).

```pascal
{$mode tp}
```

Limitations : pas d'unités longues, types plus restreints.

#### Mode Delphi

Compatible avec Delphi de Embarcadero.

```pascal
{$mode delphi}
```

Inclut : propriétés, exceptions, RTTI avancé.

#### Mode ObjFPC

Object Pascal avec extensions orientées objet.

```pascal
{$mode objfpc}
```

### Impact sur le compilateur

Chaque mode modifie :
- Les mots-clés acceptés
- Les règles de typage
- Les fonctionnalités disponibles
- Les bibliothèques par défaut

Le parser adapte son comportement selon le mode actif.

---

## 15. Débogage du compilateur lui-même

### Options de débogage

Si vous développez des extensions pour FPC, vous pouvez déboguer le compilateur :

```bash
# Compiler FPC avec informations de débogage
make DEBUG=1

# Afficher des informations de compilation détaillées
fpc -vd monprogramme.pas
```

**Options verboses :**
- `-vw` : Avertissements
- `-vn` : Notes
- `-vh` : Hints
- `-vi` : Informations générales
- `-vd` : Débogage

### Fichiers de trace

Option `-st` génère un fichier trace montrant :
- Les symboles utilisés
- La résolution de types
- Les optimisations appliquées

---

## 16. Extensions et personnalisations

### Plugins du compilateur

FPC permet d'écrire des **plugins** pour étendre le compilateur :
- Nouveaux backends (cibles)
- Optimisations personnalisées
- Analyseurs statiques

### Fichier de configuration

`fpc.cfg` permet de configurer le comportement par défaut :

```ini
# Chemins de recherche des unités
-Fu/usr/lib/fpc/3.2.2/units/x86_64-linux/rtl

# Niveau d'optimisation par défaut
-O2

# Avertissements activés
-vw
```

---

## Conclusion

Le compilateur Free Pascal est un projet complexe mais bien architecturé. Voici ce que nous avons exploré :

1. **Scanner** : Découpe le code en tokens
2. **Parser** : Vérifie la syntaxe et construit l'AST
3. **Analyse sémantique** : Vérifie le sens du code
4. **Représentation intermédiaire** : Format optimisable
5. **Optimisations** : Améliore les performances
6. **Génération de code** : Produit l'assembleur
7. **Assembleur** : Crée le code objet
8. **Linker** : Produit l'exécutable

Cette architecture modulaire explique pourquoi FPC est si portable et peut cibler autant de plateformes différentes.

### Pour aller plus loin

- **Code source de FPC** : https://gitlab.com/freepascal.org/fpc/source
- **Documentation interne** : Dans le dossier `docs/` des sources
- **Wiki FreePascal** : https://wiki.freepascal.org/Compiler
- **Liste de diffusion** : fpc-devel pour discuter du développement du compilateur

Comprendre l'architecture interne du FPC vous donne les clés pour :
- Optimiser vos programmes efficacement
- Comprendre les messages d'erreur
- Contribuer au projet FreePascal
- Créer des outils d'analyse de code Pascal

⏭️ [Développement de backends](/24-compilateur-outils-avances/02-developpement-backends.md)
