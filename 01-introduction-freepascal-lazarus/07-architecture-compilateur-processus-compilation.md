🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 1.7 Architecture du compilateur et processus de compilation

## Introduction : Qu'est-ce qu'un compilateur ?

### Le rôle fondamental du compilateur

Un compilateur est comme un **traducteur** qui transforme le code que vous écrivez (compréhensible par les humains) en instructions que l'ordinateur peut exécuter directement :

```
Code Pascal (humain) → COMPILATEUR → Code machine (processeur)

"WriteLn('Hello');" → FPC → 01001000 01100101 01101100...
```

### Pourquoi comprendre l'architecture du compilateur ?

Comprendre comment FPC fonctionne vous permet de :
- **Optimiser** vos programmes efficacement
- **Déboguer** des problèmes complexes
- **Comprendre** les messages d'erreur
- **Configurer** le compilateur de manière optimale
- **Résoudre** les problèmes de compilation

## Vue d'ensemble de l'architecture FPC

### Les grandes phases de compilation

Le compilateur FreePascal fonctionne en plusieurs étapes distinctes :

```
┌─────────────────────────────────────────────────┐
│              CODE SOURCE (.pas)                 │
└────────────────────┬────────────────────────────┘
                     ↓
┌─────────────────────────────────────────────────┐
│         1. ANALYSE LEXICALE (Scanner)           │
│         Découpe en tokens/lexèmes               │
└────────────────────┬────────────────────────────┘
                     ↓
┌─────────────────────────────────────────────────┐
│        2. ANALYSE SYNTAXIQUE (Parser)           │
│         Vérification de la grammaire            │
└────────────────────┬────────────────────────────┘
                     ↓
┌─────────────────────────────────────────────────┐
│        3. ANALYSE SÉMANTIQUE                    │
│         Vérification du sens/types              │
└────────────────────┬────────────────────────────┘
                     ↓
┌─────────────────────────────────────────────────┐
│    4. GÉNÉRATION CODE INTERMÉDIAIRE             │
│         Représentation abstraite                │
└────────────────────┬────────────────────────────┘
                     ↓
┌─────────────────────────────────────────────────┐
│         5. OPTIMISATION                         │
│         Amélioration du code                    │
└────────────────────┬────────────────────────────┘
                     ↓
┌─────────────────────────────────────────────────┐
│      6. GÉNÉRATION CODE MACHINE                 │
│         Instructions processeur                 │
└────────────────────┬────────────────────────────┘
                     ↓
┌─────────────────────────────────────────────────┐
│         7. ÉDITION DE LIENS (Linker)            │
│         Création de l'exécutable                │
└────────────────────┬────────────────────────────┘
                     ↓
┌─────────────────────────────────────────────────┐
│           EXÉCUTABLE (.exe, .so, etc.)          │
└─────────────────────────────────────────────────┘
```

### Structure modulaire de FPC

FPC est organisé en modules spécialisés :

```
FPC Compiler
├── Frontend (Partie avant)
│   ├── Scanner (Analyse lexicale)
│   ├── Parser (Analyse syntaxique)
│   └── Type checker (Vérification types)
├── Middle-end (Partie intermédiaire)
│   ├── Node tree (Arbre syntaxique)
│   ├── Optimizer (Optimiseur)
│   └── Register allocator (Allocation registres)
└── Backend (Partie arrière)
    ├── Code generator (Générateur de code)
    ├── Assembler (Assembleur)
    └── Linker interface (Interface éditeur de liens)
```

## Phase 1 : Analyse lexicale (Scanner)

### Qu'est-ce que l'analyse lexicale ?

Le scanner lit le code caractère par caractère et le découpe en **tokens** (unités lexicales) :

```pascal
// Code source
if X > 10 then
  WriteLn('Grand');

// Tokens produits par le scanner
IF          (mot-clé)  
X           (identifiant)
>           (opérateur)
10          (nombre)
THEN        (mot-clé)  
WRITELN     (identifiant)
(           (parenthèse ouvrante)
'Grand'     (chaîne)
)           (parenthèse fermante)
;           (point-virgule)
```

### Types de tokens reconnus

| Type de token | Exemples | Description |
|--------------|----------|-------------|
| **Mots-clés** | `begin`, `end`, `if`, `then` | Mots réservés du langage |
| **Identifiants** | `MyVar`, `Calculate` | Noms de variables, fonctions |
| **Littéraux** | `123`, `3.14`, `'Hello'` | Valeurs constantes |
| **Opérateurs** | `+`, `-`, `*`, `:=` | Opérations |
| **Séparateurs** | `;`, `,`, `(`, `)` | Ponctuation |
| **Commentaires** | `// texte`, `{ texte }` | Ignorés mais préservés |

### Gestion des espaces et commentaires

```pascal
program    Test;  // Espaces ignorés  
var
  X:    Integer;  { Commentaire multiligne
                    ignoré aussi }
begin
  X    :=    10;  // Espaces non significatifs
end.
```

Le scanner :
- Ignore les espaces superflus
- Préserve les sauts de ligne (pour numéros de ligne)
- Supprime les commentaires
- Garde trace des positions (pour messages d'erreur)

## Phase 2 : Analyse syntaxique (Parser)

### Construction de l'arbre syntaxique

Le parser vérifie que les tokens respectent la **grammaire** Pascal et construit un **arbre syntaxique abstrait** (AST) :

```pascal
// Code source
X := A + B * 2;

// Arbre syntaxique abstrait (simplifié)
        :=
       /  \
      X    +
          / \
         A   *
            / \
           B   2
```

### Règles de grammaire

Le parser vérifie des règles comme :

```
Programme ::= PROGRAM Identifiant ';' Bloc '.'  
Bloc ::= Declarations BEGIN Instructions END  
Declaration ::= VAR ListeVariables ':' Type ';'  
Instruction ::= Identifiant ':=' Expression  
Expression ::= Terme (('+' | '-') Terme)*
```

### Détection d'erreurs syntaxiques

```pascal
// Erreurs détectées par le parser
begin
  if X > 10      // ERREUR : 'then' attendu
    WriteLn('OK')

  X := ;         // ERREUR : Expression attendue après ':='

  for := 1 to 10 // ERREUR : Variable attendue après 'for'
end
```

Messages d'erreur typiques :
- `Fatal: Syntax error, "THEN" expected but "WRITELN" found`
- `Fatal: Syntax error, ";" expected but "END" found`
- `Error: Identifier not found "MyVariable"`

## Phase 3 : Analyse sémantique

### Vérification des types

L'analyse sémantique vérifie que le programme a un **sens** :

```pascal
var
  X: Integer;
  S: String;

begin
  X := 'Hello';    // ERREUR : Type incompatible
  S := X + 10;     // ERREUR : Opération invalide
  WriteLn(Y);      // ERREUR : Y non déclaré
end.
```

### Table des symboles

Le compilateur maintient une **table des symboles** :

```
┌──────────────────────────────────────────┐
│           TABLE DES SYMBOLES             │
├──────────┬───────────┬──────────┬────────┤
│ Nom      │ Type      │ Portée   │ Adresse│
├──────────┼───────────┼──────────┼────────┤
│ X        │ Integer   │ Globale  │ $0100  │
│ S        │ String    │ Globale  │ $0104  │
│ Calculate│ Function  │ Globale  │ $0200  │
│ I        │ Integer   │ Locale   │ Stack+4│
└──────────┴───────────┴──────────┴────────┘
```

### Résolution des identifiants

```pascal
program Scopes;

var
  X: Integer;  // X global

procedure Test;  
var
  X: Integer;  // X local (masque le global)
begin
  X := 10;     // Référence le X local
  Scopes.X := 20; // Référence le X global (qualifié)
end;

begin
  X := 5;      // Référence le X global
  Test;
end.
```

## Phase 4 : Représentation intermédiaire

### L'arbre de nœuds (Node Tree)

FPC transforme l'AST en une représentation intermédiaire optimisable :

```pascal
// Code Pascal
if X > 10 then
  Y := X * 2
else
  Y := X + 5;

// Représentation en nœuds (simplifiée)
IF_NODE
├── CONDITION: GREATER_NODE
│   ├── LEFT: LOAD_NODE(X)
│   └── RIGHT: CONST_NODE(10)
├── THEN_BRANCH: ASSIGN_NODE
│   ├── LEFT: VAR_NODE(Y)
│   └── RIGHT: MUL_NODE
│       ├── LEFT: LOAD_NODE(X)
│       └── RIGHT: CONST_NODE(2)
└── ELSE_BRANCH: ASSIGN_NODE
    ├── LEFT: VAR_NODE(Y)
    └── RIGHT: ADD_NODE
        ├── LEFT: LOAD_NODE(X)
        └── RIGHT: CONST_NODE(5)
```

### Types de nœuds

| Type de nœud | Description | Exemple |
|--------------|-------------|---------|
| **CONST_NODE** | Valeur constante | `42`, `'Hello'` |
| **VAR_NODE** | Variable | `X`, `MyVar` |
| **LOAD_NODE** | Chargement valeur | Lire `X` |
| **STORE_NODE** | Stockage valeur | Écrire dans `X` |
| **CALL_NODE** | Appel fonction | `WriteLn()` |
| **BINOP_NODE** | Opération binaire | `+`, `*`, `>` |
| **IF_NODE** | Condition | `if...then...else` |
| **LOOP_NODE** | Boucle | `for`, `while` |

## Phase 5 : Optimisation

### Optimisations locales

FPC applique plusieurs optimisations automatiques :

#### Propagation de constantes

```pascal
// Avant optimisation
X := 10;  
Y := X * 2;  
Z := Y + 5;

// Après optimisation
X := 10;  
Y := 20;  // 10 * 2 calculé à la compilation  
Z := 25;  // 20 + 5 calculé à la compilation
```

#### Élimination de code mort

```pascal
// Avant optimisation
if False then
  WriteLn('Jamais exécuté');  // Code mort

X := 10;  
if X > 5 then    // Toujours vrai
  Y := 1
else
  Y := 2;        // Code mort

// Après optimisation
X := 10;  
Y := 1;
```

#### Simplification d'expressions

```pascal
// Avant optimisation
X := Y * 1;      // Multiplication par 1  
Z := Y + 0;      // Addition de 0  
A := Y div 1;    // Division par 1

// Après optimisation
X := Y;  
Z := Y;  
A := Y;
```

### Optimisations de boucles

#### Déroulement de boucles (Loop unrolling)

```pascal
// Avant optimisation
for I := 1 to 4 do
  Sum := Sum + Array[I];

// Après optimisation (déroulé)
Sum := Sum + Array[1];  
Sum := Sum + Array[2];  
Sum := Sum + Array[3];  
Sum := Sum + Array[4];
```

#### Sortie d'invariants (Loop invariant hoisting)

```pascal
// Avant optimisation
for I := 1 to 100 do
  Result := Result + I * (X + Y);  // X + Y invariant

// Après optimisation
Temp := X + Y;  // Calculé une seule fois  
for I := 1 to 100 do
  Result := Result + I * Temp;
```

### Niveaux d'optimisation

| Niveau | Option | Description | Usage |
|--------|--------|-------------|-------|
| **O-** | `-O-` | Aucune optimisation | Débogage |
| **O1** | `-O1` | Optimisations basiques | Développement |
| **O2** | `-O2` | Optimisations standards | Production normale |
| **O3** | `-O3` | Optimisations agressives | Performance critique |
| **O4** | `-O4` | Expérimental | Tests uniquement |
| **Os** | `-Os` | Optimiser la taille | Systèmes embarqués |

## Phase 6 : Génération de code

### Sélection d'instructions

Le générateur de code transforme la représentation intermédiaire en instructions assembleur :

```pascal
// Code Pascal
X := A + B;

// Code assembleur x86-64 généré
mov  rax, [A]      ; Charger A dans RAX  
add  rax, [B]      ; Ajouter B  
mov  [X], rax      ; Stocker dans X
```

### Allocation de registres

Le compilateur optimise l'utilisation des registres processeur :

```pascal
// Code Pascal
function Calculate(A, B, C: Integer): Integer;  
begin
  Result := (A + B) * C;
end;

// Assembleur optimisé (pseudo-code)
// A dans RDI, B dans RSI, C dans RDX (convention d'appel)
add  rdi, rsi      ; A + B dans RDI  
imul rdi, rdx      ; Multiplier par C  
mov  rax, rdi      ; Résultat dans RAX (retour)  
ret
```

### Génération selon l'architecture

FPC génère du code différent selon la cible :

```pascal
// Même code Pascal
X := X + 1;

// x86-64
inc qword ptr [X]

// ARM
ldr  r0, [X]  
add  r0, r0, #1  
str  r0, [X]

// PowerPC
lwz  r3, X  
addi r3, r3, 1  
stw  r3, X
```

## Phase 7 : Assemblage et édition de liens

### Assemblage

Le code assembleur est transformé en code objet :

```
programme.pas → programme.s → programme.o
   (Pascal)      (Assembleur)    (Objet)
```

Fichiers générés :
- `.ppu` : Unité précompilée Pascal (interface)
- `.o` : Code objet (implémentation)

### Édition de liens (Linking)

L'éditeur de liens combine tous les modules :

```
┌──────────────┐
│ programme.o  │ ←── Code principal
├──────────────┤
│ unit1.o      │ ←── Unités utilisées
├──────────────┤
│ unit2.o      │
├──────────────┤
│ rtl.o        │ ←── Runtime Library
├──────────────┤
│ system.o     │ ←── Bibliothèques système
└──────────────┘
        ↓
   [LINKER]
        ↓
┌──────────────┐
│ programme.exe│ ←── Exécutable final
└──────────────┘
```

### Résolution des symboles

L'éditeur de liens :
1. Résout les références entre modules
2. Calcule les adresses finales
3. Inclut les bibliothèques nécessaires
4. Crée l'exécutable

## Fichiers générés lors de la compilation

### Types de fichiers

| Extension | Description | Contenu |
|-----------|-------------|---------|
| `.pas` | Source Pascal | Code source |
| `.pp` | Source Pascal (alt) | Code source |
| `.ppu` | Pascal Compiled Unit | Interface compilée |
| `.o` | Object file | Code objet |
| `.a` | Archive/Library | Bibliothèque statique |
| `.so`/`.dll` | Shared library | Bibliothèque dynamique |
| `.exe` | Executable | Programme Windows |
| `.compiled` | Compilation info | Métadonnées Lazarus |

### Arborescence de compilation

```
MonProjet/
├── src/
│   ├── main.pas          # Source principal
│   └── utils.pas         # Unité utilitaire
├── lib/
│   ├── main.o           # Code objet principal
│   ├── utils.ppu        # Interface compilée
│   └── utils.o          # Code objet utils
└── bin/
    └── monprojet.exe    # Exécutable final
```

## Compilation d'unités

### Structure d'une unité

```pascal
unit MyUnit;  // Déclaration

interface     // Partie publique (→ .ppu)

uses SysUtils;

type
  TMyClass = class
    procedure DoSomething;
  end;

implementation  // Partie privée (→ .o)

procedure TMyClass.DoSomething;  
begin
  WriteLn('Working...');
end;

initialization  // Code d'initialisation
  // Exécuté au chargement

finalization    // Code de finalisation
  // Exécuté à la fermeture

end.
```

### Processus de compilation d'unité

```
1. Compilation interface → .ppu
   - Types publics
   - Signatures de fonctions
   - Constantes publiques

2. Compilation implementation → .o
   - Code des fonctions
   - Variables privées
   - Code d'initialisation

3. Utilisation par d'autres unités
   - Lecture du .ppu pour vérifier types
   - Liaison avec .o à l'édition de liens
```

### Dépendances et recompilation

FPC utilise un système intelligent de dépendances :

```
main.pas ──uses──> unit1.pas ──uses──> unit2.pas
                        ↓                    ↓
                    unit1.ppu           unit2.ppu
                    unit1.o             unit2.o

Si unit2.pas modifié :
- unit2 recompilée
- unit1 recompilée (dépend de unit2)
- main recompilé (dépend de unit1)
```

## Modes de compilation spéciaux

### Compilation pour débogage

```bash
fpc -g -gl program.pas

# Génère :
# - Symboles de débogage
# - Numéros de ligne
# - Informations variables
```

Impact sur le processus :
- Optimisations désactivées
- Code supplémentaire pour tracer
- Tables de symboles complètes
- Fichier plus gros mais débogable

### Compilation croisée (Cross-compilation)

```bash
fpc -Tlinux -Px86_64 program.pas

# Sur Windows, compile pour Linux 64-bit
```

Processus modifié :
1. Frontend identique (analyse Pascal)
2. Backend différent (génération Linux)
3. Assembleur croisé (as pour Linux)
4. Éditeur de liens croisé (ld pour Linux)

### Compilation de bibliothèques

```bash
fpc -CD library.pas  # DLL/SO dynamique  
fpc -CS library.pas  # Bibliothèque statique
```

Différences :
- Code position-indépendant (PIC)
- Exports de symboles
- Pas de code d'initialisation principal
- Format de sortie différent

## Messages du compilateur

### Types de messages

| Type | Préfixe | Signification | Action requise |
|------|---------|---------------|----------------|
| **Fatal** | `Fatal:` | Erreur bloquante | Correction obligatoire |
| **Error** | `Error:` | Erreur de compilation | Correction nécessaire |
| **Warning** | `Warning:` | Problème potentiel | Vérification recommandée |
| **Note** | `Note:` | Information utile | Pour information |
| **Hint** | `Hint:` | Suggestion | Amélioration possible |

### Exemples de messages

```pascal
// Fatal: Erreur de syntaxe
begin
  if X > 10  // Fatal: Syntax error, "THEN" expected
    WriteLn;

// Error: Type incompatible
var
  I: Integer;
begin
  I := 'Hello';  // Error: Incompatible types

// Warning: Variable non utilisée
var
  Unused: Integer;  // Warning: Local variable "Unused" not used

// Hint: Optimisation possible
if X = True then  // Hint: Comparing to True is redundant

// Note: Information
{$mode delphi}  // Note: Switching to Delphi mode
```

## Outils d'analyse de compilation

### Verbose mode

```bash
# Voir toutes les étapes
fpc -va program.pas

# Sortie détaillée :
# [0.000] Parsing program.pas
# [0.010] Compiling unit system
# [0.020] Loading unit sysutils
# [0.030] Generating code for main
# [0.040] Assembling program
# [0.050] Linking program.exe
```

### Analyse des temps

```bash
# Mesurer les temps de compilation
fpc -vt program.pas

# Résultat :
# Scanner: 0.010s
# Parser: 0.020s
# Code generation: 0.030s
# Assembler: 0.015s
# Linker: 0.025s
# Total: 0.100s
```

### Arbre syntaxique

```bash
# Générer l'AST (pour debug)
fpc -vp program.pas > ast.txt

# Contenu ast.txt :
# Node: Program
#   Node: VarDecl
#     Ident: X
#     Type: Integer
#   Node: Assignment
#     Left: VarRef(X)
#     Right: Const(10)
```

## Optimisation du processus de compilation

### Compilation incrémentale

FPC effectue une compilation incrémentale par défaut : seules les unités dont le source a changé (ou dont les dépendances ont changé) sont recompilées.

```bash
# Par défaut, FPC ne recompile que le nécessaire
fpc program.pas

# Pour forcer la recompilation totale (si nécessaire)
fpc -B program.pas  # -B = Build all (tout recompiler)
```

Gain de temps typique :
- Première compilation : 10 secondes
- Recompilations (incrémental) : 1-2 secondes

### Accélérer la compilation

> **Note** : FPC ne supporte pas la compilation parallèle multi-threads en interne (pas d'option `-J` pour le parallélisme). Cependant, `lazbuild` et les Makefiles permettent de compiler plusieurs packages en parallèle.

```bash
# Compiler plusieurs projets en parallèle avec make
make -j4

# Ou utiliser lazbuild pour les packages Lazarus
lazbuild --build-all monprojet.lpi
```

### Optimiser les temps de compilation

```bash
# Placer les unités compilées sur un disque rapide (SSD ou RAM disk)
mkdir -p /tmp/fpc-units  
fpc -FU/tmp/fpc-units program.pas

# Ou monter un tmpfs pour performance maximale
sudo mount -t tmpfs -o size=1G tmpfs /tmp/fpc-units  
fpc -FU/tmp/fpc-units program.pas
```

## Diagnostic des problèmes de compilation

### Erreurs courantes et solutions

#### "Can't find unit"

```
Problème : Unité introuvable  
Diagnostic : fpc -vut program.pas  
Solution : Ajouter chemin avec -Fu
```

#### "Error while linking"

```
Problème : Édition de liens échouée  
Diagnostic : fpc -vd program.pas  
Solution : Vérifier bibliothèques avec -Fl
```

#### Compilation lente

```
Problème : Temps de compilation excessif  
Diagnostic : fpc -vt program.pas  
Solutions :
- Désactiver antivirus
- Utiliser SSD
- Compilation incrémentale
- Réduire optimisations
```

## Conclusion

### Points clés à retenir

1. **La compilation est un processus en 7 phases** : Du source à l'exécutable
2. **Chaque phase a un rôle spécifique** : Analyse, optimisation, génération
3. **Les optimisations sont automatiques** : Mais configurables
4. **Les messages d'erreur indiquent la phase** : Pour mieux diagnostiquer
5. **La compilation incrémentale économise du temps** : Réutilise le travail fait

### Impact sur votre code

Comprendre le compilateur vous aide à :
- **Écrire du code plus efficace** : Sachant comment il sera optimisé
- **Déboguer plus facilement** : Comprenant les messages d'erreur
- **Optimiser les performances** : Utilisant les bonnes options
- **Résoudre les problèmes** : Sachant où chercher

### Pour aller plus loin

- Étudier le code source de FPC (open source)
- Expérimenter avec les options de compilation
- Analyser le code assembleur généré
- Profiler pour voir l'impact des optimisations

Le compilateur est votre allié : plus vous le comprenez, mieux vous l'utilisez !

⏭️ [Configuration d'environnements de développement dual-boot/VM](/01-introduction-freepascal-lazarus/08-configuration-environnements-dual-boot-vm.md)
