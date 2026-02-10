🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 24.2 Développement de backends

## Introduction

Un **backend** est la partie du compilateur qui génère du code machine spécifique pour une architecture processeur donnée. Si le frontend (scanner, parser, analyseur sémantique) est universel et indépendant de la plateforme, le backend doit connaître intimement le processeur cible.

Dans ce chapitre, nous allons explorer comment fonctionnent les backends du compilateur Free Pascal et comment vous pourriez créer le vôtre pour une nouvelle architecture.

> 💡 **Note pour débutants** : Ce chapitre est technique, mais nous allons progresser pas à pas. Même si vous n'avez jamais écrit de code assembleur, vous comprendrez les concepts fondamentaux.

---

## Qu'est-ce qu'un backend ?

### Analogie simple

Imaginez que vous êtes traducteur. Vous savez traduire du français vers plusieurs langues :

- **Frontend** = Comprendre le français (peu importe la langue cible)
- **Backend anglais** = Générer de l'anglais correct
- **Backend espagnol** = Générer de l'espagnol correct
- **Backend japonais** = Générer du japonais correct

Chaque backend connaît les règles spécifiques de sa langue cible.

De la même façon, le compilateur FPC :
- **Frontend** = Comprend le Pascal (universel)
- **Backend x86** = Génère du code pour processeurs Intel/AMD
- **Backend ARM** = Génère du code pour processeurs ARM
- **Backend AVR** = Génère du code pour microcontrôleurs Arduino

### Rôle du backend

Le backend est responsable de :

1. **Sélection d'instructions** : Choisir les instructions assembleur appropriées
2. **Allocation de registres** : Décider quelles variables vont dans quels registres
3. **Gestion de la pile** : Organiser la mémoire locale (stack frame)
4. **Conventions d'appel** : Respecter les règles de passage de paramètres
5. **Génération du code final** : Produire l'assembleur ou le code objet

---

## Architecture générale d'un backend

### Structure modulaire

Chaque backend FPC suit une architecture standard :

```
compiler/
└── <architecture>/        # Par exemple: x86/, arm/, aarch64/
    ├── cgcpu.pas         # Code generator pour le CPU
    ├── cpubase.pas       # Définitions de base du CPU
    ├── cpuinfo.pas       # Informations sur le CPU
    ├── cgobj.pas         # Génération orientée objet (optionnel)
    ├── aasmcpu.pas       # Assembleur abstrait
    ├── agcpugas.pas      # Sortie format GAS
    ├── itcpugas.pas      # Tables d'instructions GAS
    ├── racpu.pas         # Allocation de registres
    └── ncpuadd.pas       # Nœuds d'addition spécifiques
        ncpucal.pas       # Nœuds d'appel
        ncpumat.pas       # Nœuds mathématiques
        ... (autres nœuds)
```

### Fichiers principaux

#### 1. cpubase.pas - Définitions du CPU

Ce fichier définit les caractéristiques fondamentales du processeur :

```pascal
unit cpubase;

interface

type
  { Registres disponibles }
  tregister = (
    { Registres généraux }
    RS_R0, RS_R1, RS_R2, RS_R3,
    RS_R4, RS_R5, RS_R6, RS_R7,
    { Registre de pile }
    RS_SP,
    { Registre de lien }
    RS_LR,
    { Compteur programme }
    RS_PC,
    { Drapeaux }
    RS_FLAGS
  );

  { Instructions supportées }
  tasmop = (
    A_MOV,      // Déplacement
    A_ADD,      // Addition
    A_SUB,      // Soustraction
    A_MUL,      // Multiplication
    A_CMP,      // Comparaison
    A_B,        // Branchement
    A_BL,       // Branchement avec lien
    // ... autres instructions
  );

  { Modes d'adressage }
  topmode = (
    OP_REGISTER,    // Registre direct
    OP_IMMEDIATE,   // Valeur immédiate
    OP_INDIRECT,    // Indirect via registre
    OP_INDEXED      // Indexé avec déplacement
  );
```

**Explications :**
- `tregister` : Énumération de tous les registres du processeur
- `tasmop` : Toutes les instructions assembleur disponibles
- `topmode` : Les différentes façons d'accéder aux données

#### 2. cgcpu.pas - Générateur de code

C'est le cœur du backend. Il transforme les nœuds de l'arbre intermédiaire en instructions :

```pascal
unit cgcpu;

interface

type
  tcgcpu = class(tcg)
  public
    { Génération d'opérations arithmétiques }
    procedure a_op_const_reg(op: topcg; size: tcgsize;
                             value: tcgint; reg: tregister);
    procedure a_op_reg_reg(op: topcg; size: tcgsize;
                           src, dst: tregister);

    { Chargement et stockage }
    procedure a_load_const_reg(size: tcgsize;
                               value: tcgint; reg: tregister);
    procedure a_load_ref_reg(size: tcgsize;
                             const ref: treference; reg: tregister);

    { Appels de fonction }
    procedure a_call_name(const s: string);

    { Comparaisons et sauts }
    procedure a_cmp_const_reg(size: tcgsize;
                              value: tcgint; reg: tregister);
    procedure a_jmp_cond(cond: topcmp; l: tasmlabel);
  end;
```

**Explications :**
- Chaque méthode génère un type d'instruction spécifique
- `a_op_const_reg` : Opération entre une constante et un registre
- `a_load_ref_reg` : Charger depuis la mémoire vers un registre
- Ces méthodes sont appelées lors de la traversée de l'arbre intermédiaire

#### 3. racpu.pas - Allocation de registres

Gère l'allocation intelligente des registres :

```pascal
unit racpu;

interface

type
  trgcpu = class(trgobj)
  public
    { Obtenir un registre libre }
    function getregister(list: TAsmList;
                        regtype: tregistertype): tregister;

    { Libérer un registre }
    procedure ungetregister(list: TAsmList; r: tregister);

    { Registres à sauvegarder lors d'appels }
    function get_caller_saved_registers: tcpuregisterset;
    function get_callee_saved_registers: tcpuregisterset;
  end;
```

---

## Les registres : concept fondamental

### Qu'est-ce qu'un registre ?

Un **registre** est une petite mémoire ultra-rapide directement dans le processeur. C'est là que le CPU effectue ses calculs.

**Analogie :** Si la RAM est un grand entrepôt, les registres sont des tables de travail juste à côté de l'ouvrier. Beaucoup plus rapide d'accès !

### Types de registres

#### Registres généraux (General Purpose)

Utilisables pour n'importe quelle opération :

**x86-64 (Intel/AMD) :**
```
RAX, RBX, RCX, RDX     - 64 bits
RSI, RDI, RBP, RSP     - 64 bits
R8 à R15               - 64 bits (ajoutés en 64-bit)
```

**ARM :**
```
R0 à R12               - 32 bits généraux
R13 (SP)               - Stack Pointer (pile)
R14 (LR)               - Link Register (retour de fonction)
R15 (PC)               - Program Counter (instruction suivante)
```

#### Registres spéciaux

**Pointeur de pile (Stack Pointer - SP/RSP)**
- Pointe vers le sommet de la pile
- Modifié automatiquement lors des PUSH/POP

**Pointeur d'instruction (Program Counter - PC/RIP)**
- Contient l'adresse de l'instruction en cours
- Modifié lors des sauts et appels

**Registre de drapeaux (Flags/CPSR)**
- Contient les résultats de comparaison
- Zero flag, Carry flag, Overflow flag, etc.

### Exemple d'utilisation

```pascal
// Code Pascal
x := y + 5;

// Généré par le backend x86-64
mov rax, [y]      // Charger y dans RAX
add rax, 5        // Ajouter 5 à RAX
mov [x], rax      // Stocker RAX dans x

// Généré par le backend ARM
ldr r0, [y]       // Charger y dans R0
add r0, r0, #5    // Ajouter 5 à R0
str r0, [x]       // Stocker R0 dans x
```

---

## Sélection d'instructions

### Le problème

Pour une même opération Pascal, il existe souvent plusieurs façons de la traduire en assembleur. Le backend doit choisir la meilleure.

### Exemple : Addition

**Code Pascal :**
```pascal
x := x + 1;
```

**Options x86-64 :**
```asm
; Option 1 : ADD
add [x], 1

; Option 2 : INC (plus court, mais peut être plus lent sur CPU modernes)
inc [x]

; Option 3 : LEA (Load Effective Address - astucieux!)
lea rax, [x+1]
mov [x], rax
```

Le backend choisit selon :
- La taille du code
- La vitesse d'exécution
- Le processeur cible

### Pattern matching

FPC utilise du **pattern matching** pour reconnaître les motifs optimisables :

```pascal
// Dans le code du backend
case node.nodetype of
  addn:
    begin
      if is_const_one(right) then
        generate_inc(left)  // INC au lieu de ADD
      else if is_power_of_two(right) then
        generate_shift(left, log2(right))  // SHL au lieu de MUL
      else
        generate_add(left, right);
    end;
end;
```

---

## Allocation de registres

### Le défi

Les processeurs ont un nombre limité de registres, mais un programme peut avoir des centaines de variables. Comment décider quoi mettre où ?

### Stratégies d'allocation

#### 1. Allocation simple (linear scan)

Parcours linéaire du code, allocation à la demande :

```pascal
procedure simple_allocation;
var
  free_registers: set of tregister;
begin
  free_registers := all_general_registers;

  for each instruction do
  begin
    if needs_register then
    begin
      if free_registers <> [] then
        reg := pick_one(free_registers)
      else
        reg := spill_one_to_memory;  // Pas de registre libre!
    end;
  end;
end;
```

**Avantages :** Simple, rapide à compiler  
**Inconvénients :** Pas optimal  

#### 2. Graph coloring (coloration de graphe)

Méthode sophistiquée utilisée par FPC pour les optimisations avancées :

1. Créer un **graphe d'interférence** : deux variables qui vivent en même temps sont connectées
2. Colorer le graphe avec N couleurs (N = nombre de registres)
3. Chaque couleur = un registre

**Exemple simplifié :**

```pascal
// Variables et leur durée de vie
x := 5;        // x vit de ligne 1 à 3
y := x + 2;    // y vit de ligne 2 à 4
z := x + 1;    // z vit de ligne 3 à 5
w := y + z;    // w vit de ligne 4 à 5

// Graphe d'interférence
x -- y  (vivent ensemble)
x -- z  (vivent ensemble)
y -- z  (vivent ensemble)
y -- w  (vivent ensemble)
z -- w  (vivent ensemble)

// Coloration (allocation)
x → R0
y → R1
z → R2
w → R0  (on peut réutiliser R0, x est mort)
```

#### 3. Register spilling

Quand il n'y a plus de registres libres, on doit **sauvegarder** (spill) une variable en mémoire :

```pascal
// Trop de variables actives
a := b + c;
d := e + f;
g := h + i;
j := k + l;

// Si seulement 3 registres disponibles :
mov r0, [b]
add r0, [c]
mov [a], r0      // Sauvegarder 'a' en mémoire (spill)

mov r0, [e]      // Réutiliser r0
add r0, [f]
mov [d], r0
```

### Registres caller-saved vs callee-saved

Important pour les appels de fonction :

**Caller-saved (volatiles)**
- Le code appelant doit les sauvegarder avant l'appel
- Exemples x86-64 : RAX, RCX, RDX, R8-R11

**Callee-saved (non-volatiles)**
- La fonction appelée doit les préserver
- Exemples x86-64 : RBX, RSI, RDI, R12-R15

```pascal
procedure Example;
var
  x: Integer;  // Supposons x dans RBX (callee-saved)
begin
  x := 5;
  CallSomeFunction;  // RBX sera préservé
  WriteLn(x);        // x vaut toujours 5
end;
```

---

## Conventions d'appel

### Qu'est-ce que c'est ?

Les **conventions d'appel** (calling conventions) définissent comment :
- Passer les paramètres à une fonction
- Retourner la valeur de résultat
- Gérer la pile
- Préserver les registres

### Différences entre plateformes

#### Windows x86-64 (Microsoft)

```pascal
function Add(a, b, c, d: Integer): Integer;
```

**Passage des paramètres :**
```asm
; a → RCX
; b → RDX
; c → R8
; d → R9
; Plus de 4 paramètres → pile

; Retour dans RAX
```

#### Linux x86-64 (System V)

```pascal
function Add(a, b, c, d: Integer): Integer;
```

**Passage des paramètres :**
```asm
; a → RDI
; b → RSI
; c → RDX
; d → RCX
; 5e et 6e → R8, R9
; Plus de 6 paramètres → pile

; Retour dans RAX
```

#### ARM (32-bit)

```pascal
function Add(a, b, c, d: Integer): Integer;
```

**Passage des paramètres :**
```asm
; a → R0
; b → R1
; c → R2
; d → R3
; Plus de 4 paramètres → pile

; Retour dans R0
```

### Implémentation dans le backend

```pascal
procedure tcgcpu.a_call_reg(reg: tregister);
var
  para: tcgpara;
begin
  { Sauvegarder les registres caller-saved }
  save_caller_saved_registers;

  { Préparer les paramètres selon la convention }
  for para in current_procinfo.para do
  begin
    case para.location of
      LOC_REGISTER:
        { Charger dans le bon registre }
        a_load_reg_reg(para.register, calling_convention_register(para.index));
      LOC_STACK:
        { Pousser sur la pile }
        a_load_reg_stack(para.register);
    end;
  end;

  { Appel effectif }
  list.concat(taicpu.op_reg(A_CALL, reg));

  { Restaurer les registres }
  restore_caller_saved_registers;
end;
```

---

## Stack frames (cadres de pile)

### Qu'est-ce qu'un stack frame ?

Un **stack frame** est la zone de la pile allouée pour une fonction. Il contient :
- Les variables locales
- Les paramètres de la fonction
- L'adresse de retour
- Les registres sauvegardés

### Structure typique

```
Pile (croît vers le bas) :

+---------------------------+  <- Sommet de pile (SP)
| Variables locales         |
+---------------------------+
| Registres sauvegardés     |
+---------------------------+
| Paramètres supplémentaires|
+---------------------------+
| Adresse de retour         |
+---------------------------+  <- Base du frame (BP/FP)
| Ancien BP                 |
+---------------------------+
| ... frame précédent       |
```

### Prologue et épilogue

**Prologue** : Code au début de la fonction pour setup le frame

```asm
; x86-64
push rbp           ; Sauvegarder l'ancien BP
mov rbp, rsp       ; Nouveau BP = SP actuel
sub rsp, 32        ; Allouer 32 octets pour variables locales
```

**Épilogue** : Code à la fin pour nettoyer le frame

```asm
; x86-64
mov rsp, rbp       ; Restaurer SP
pop rbp            ; Restaurer ancien BP
ret                ; Retourner
```

### Génération par le backend

```pascal
procedure tcgcpu.g_proc_entry(localsize: longint);
begin
  { Générer le prologue }
  list.concat(taicpu.op_reg(A_PUSH, NR_BP));
  list.concat(taicpu.op_reg_reg(A_MOV, NR_SP, NR_BP));

  if localsize > 0 then
    list.concat(taicpu.op_const_reg(A_SUB, localsize, NR_SP));
end;

procedure tcgcpu.g_proc_exit;
begin
  { Générer l'épilogue }
  list.concat(taicpu.op_reg_reg(A_MOV, NR_BP, NR_SP));
  list.concat(taicpu.op_reg(A_POP, NR_BP));
  list.concat(taicpu.op_none(A_RET));
end;
```

---

## Modes d'adressage

### Qu'est-ce que c'est ?

Les **modes d'adressage** définissent comment accéder aux données en mémoire.

### Types principaux

#### 1. Immédiat

La valeur est dans l'instruction elle-même :

```asm
mov rax, 42        ; 42 est une valeur immédiate
add rbx, 10
```

#### 2. Registre direct

La valeur est dans un registre :

```asm
mov rax, rbx       ; Copier RBX dans RAX
add rcx, rdx       ; RCX = RCX + RDX
```

#### 3. Direct mémoire

Adresse absolue en mémoire :

```asm
mov rax, [0x1000]  ; Charger depuis l'adresse 0x1000
```

#### 4. Indirect registre

L'adresse est dans un registre :

```asm
mov rax, [rbx]     ; Charger depuis l'adresse contenue dans RBX
```

#### 5. Indexé avec déplacement

Adresse = Registre + Constante :

```asm
mov rax, [rbx+8]   ; Charger depuis RBX + 8 octets
```

#### 6. Indexé avec échelle

Adresse = Base + (Index × Échelle) + Déplacement :

```asm
; Accès à un tableau : array[i]
mov rax, [rbx + rcx*4 + 16]
; RBX = adresse de base
; RCX = index
; 4 = taille d'un élément (Integer = 4 octets)
; 16 = offset de départ
```

### Implémentation dans le backend

```pascal
type
  treference = record
    base: tregister;       // Registre de base
    index: tregister;      // Registre d'index
    offset: tcgint;        // Déplacement
    scalefactor: byte;     // Facteur d'échelle (1,2,4,8)
    symbol: tasmsymbol;    // Symbole (pour variables globales)
  end;

procedure tcgcpu.a_load_ref_reg(size: tcgsize;
                                const ref: treference;
                                reg: tregister);
begin
  case ref.refaddr of
    addr_full:
      { [base + index*scale + offset] }
      list.concat(taicpu.op_ref_reg(A_MOV, ref, reg));
    addr_no_index:
      { [base + offset] }
      list.concat(taicpu.op_ref_reg(A_MOV, ref, reg));
  end;
end;
```

---

## Optimisations spécifiques au backend

### Peephole optimizations

Remplacer des séquences d'instructions inefficaces :

**Avant optimisation :**
```asm
mov rax, 0
add rax, rbx
```

**Après optimisation :**
```asm
mov rax, rbx       ; Plus court et plus rapide
```

**Autre exemple :**
```asm
; Avant
push rax
pop rbx

; Après
mov rbx, rax       ; Équivalent, plus efficace
```

### Utilisation d'instructions spécialisées

Certains processeurs ont des instructions optimisées :

#### x86 : LEA (Load Effective Address)

```pascal
// Code Pascal
x := y + 4;

// Naïf
mov rax, [y]
add rax, 4
mov [x], rax

// Optimisé avec LEA
mov rax, [y]
lea rax, [rax+4]   ; LEA peut faire addition sans affecter flags
mov [x], rax
```

#### ARM : LDM/STM (Load/Store Multiple)

```pascal
// Sauvegarder plusieurs registres
// Naïf
str r0, [sp, #-4]!
str r1, [sp, #-4]!
str r2, [sp, #-4]!

// Optimisé
stmdb sp!, {r0-r2}  ; Une seule instruction !
```

### Utilisation de SIMD

Pour les opérations vectorielles :

```pascal
// Code Pascal - Addition de tableaux
for i := 0 to 3 do
  result[i] := a[i] + b[i];

// Naïf (4 instructions)
mov rax, [a]
add rax, [b]
mov [result], rax
; ... répéter 4 fois

// Optimisé avec SSE (x86)
movups xmm0, [a]       ; Charger 4 entiers en une fois
movups xmm1, [b]
addps xmm0, xmm1       ; Additionner les 4 en parallèle
movups [result], xmm0  ; Stocker les 4 résultats
```

---

## Créer un backend simple

### Processeur hypothétique

Imaginons un processeur très simple pour comprendre le processus :

**Caractéristiques :**
- 8 registres généraux : R0 à R7
- Instructions : MOV, ADD, SUB, MUL, CMP, JMP, CALL, RET
- Adressage : registre, immédiat, indirect

### Étape 1 : Définir le CPU (cpubase.pas)

```pascal
unit cpubase;

interface

type
  { Nos 8 registres }
  tregister = (
    RS_R0, RS_R1, RS_R2, RS_R3,
    RS_R4, RS_R5, RS_R6, RS_R7,
    RS_SP,    // R7 = Stack Pointer
    RS_NO     // Pas de registre
  );

  { Nos instructions }
  tasmop = (
    A_NONE,
    A_MOV,    // MOV dest, source
    A_ADD,    // ADD dest, source
    A_SUB,    // SUB dest, source
    A_MUL,    // MUL dest, source
    A_CMP,    // CMP reg1, reg2
    A_JMP,    // JMP address
    A_JE,     // Jump if Equal
    A_JNE,    // Jump if Not Equal
    A_CALL,   // CALL address
    A_RET     // RET
  );

  { Modes opérandes }
  topmode = (
    OP_NONE,
    OP_REGISTER,    // R0
    OP_IMMEDIATE,   // #42
    OP_INDIRECT     // [R0]
  );

const
  { Registres disponibles pour allocation }
  AVAILABLE_REGISTERS = [RS_R0..RS_R6];

  { R7 réservé pour la pile }
  STACK_POINTER = RS_R7;
```

### Étape 2 : Générateur de code (cgcpu.pas)

```pascal
unit cgcpu;

interface

uses
  cpubase, cgbase;

type
  tcgcpu = class(tcg)
  private
    procedure emit(op: tasmop; dest, src: tregister);
    procedure emit_const(op: tasmop; dest: tregister; value: longint);
  public
    { Charger une constante dans un registre }
    procedure a_load_const_reg(size: tcgsize; value: tcgint;
                               reg: tregister); override;

    { Opération reg = reg op reg }
    procedure a_op_reg_reg(op: topcg; size: tcgsize;
                          src, dst: tregister); override;

    { Opération reg = reg op const }
    procedure a_op_const_reg(op: topcg; size: tcgsize;
                            value: tcgint; reg: tregister); override;

    { Appel de procédure }
    procedure a_call_name(const s: string); override;
  end;

implementation

procedure tcgcpu.emit(op: tasmop; dest, src: tregister);
var
  ai: taicpu;
begin
  { Créer l'instruction }
  ai := taicpu.op_reg_reg(op, dest, src);
  { L'ajouter à la liste }
  current_asmdata.CurrAsmList.concat(ai);
end;

procedure tcgcpu.a_load_const_reg(size: tcgsize; value: tcgint;
                                  reg: tregister);
begin
  { MOV reg, #value }
  emit_const(A_MOV, reg, value);
end;

procedure tcgcpu.a_op_reg_reg(op: topcg; size: tcgsize;
                             src, dst: tregister);
var
  asmop: tasmop;
begin
  { Convertir l'opération abstraite en instruction concrète }
  case op of
    OP_ADD: asmop := A_ADD;
    OP_SUB: asmop := A_SUB;
    OP_MUL: asmop := A_MUL;
    else
      InternalError('Operation not supported');
  end;

  { Générer : dst = dst op src }
  emit(asmop, dst, src);
end;

procedure tcgcpu.a_call_name(const s: string);
var
  ai: taicpu;
begin
  { CALL procedure_name }
  ai := taicpu.op_sym(A_CALL, current_asmdata.RefAsmSymbol(s));
  current_asmdata.CurrAsmList.concat(ai);
end;

end.
```

### Étape 3 : Allocation de registres (racpu.pas)

```pascal
unit racpu;

interface

type
  trgcpu = class(trgobj)
  private
    used_registers: set of tregister;
  public
    function getregister(list: TAsmList;
                        regtype: tregistertype): tregister; override;
    procedure ungetregister(list: TAsmList; r: tregister); override;
  end;

implementation

function trgcpu.getregister(list: TAsmList;
                           regtype: tregistertype): tregister;
var
  r: tregister;
begin
  { Chercher un registre libre }
  for r in AVAILABLE_REGISTERS do
  begin
    if not (r in used_registers) then
    begin
      used_registers := used_registers + [r];
      exit(r);
    end;
  end;

  { Pas de registre libre - il faut spiller }
  result := spill_register(list);
end;

procedure trgcpu.ungetregister(list: TAsmList; r: tregister);
begin
  { Marquer le registre comme libre }
  used_registers := used_registers - [r];
end;

end.
```

### Étape 4 : Test du backend

```pascal
program TestBackend;
var
  x, y, z: Integer;
begin
  x := 10;
  y := 20;
  z := x + y;
  WriteLn(z);
end.
```

**Code généré par notre backend hypothétique :**
```asm
; x := 10
MOV R0, #10
MOV [x], R0

; y := 20
MOV R0, #20
MOV [y], R0

; z := x + y
MOV R0, [x]
MOV R1, [y]
ADD R0, R1
MOV [z], R0

; WriteLn(z)
MOV R0, [z]
CALL WriteLn

; Fin du programme
RET
```

---

## Backends existants dans FPC

### Vue d'ensemble

FPC supporte officiellement de nombreuses architectures. Voici les principales :

| Architecture | Bits | Utilisation principale | Statut |
|--------------|------|------------------------|--------|
| **i386** | 32 | PC Intel/AMD anciens | Mature |
| **x86_64** | 64 | PC Intel/AMD modernes | Mature |
| **ARM** | 32 | Smartphones, Raspberry Pi | Mature |
| **AArch64** | 64 | ARM 64 bits (Apple M1, serveurs) | Mature |
| **PowerPC** | 32/64 | Anciens Mac, consoles | Mature |
| **SPARC** | 32/64 | Serveurs Sun/Oracle | Mature |
| **MIPS** | 32/64 | Routeurs, embarqué | Mature |
| **AVR** | 8 | Arduino, microcontrôleurs | Expérimental |
| **RISC-V** | 32/64 | Architecture open source | En développement |
| **WebAssembly** | 32 | Applications web | Expérimental |

### Backend i386 (Intel 32 bits)

**Caractéristiques :**
- Architecture CISC (Complex Instruction Set)
- 8 registres généraux principaux
- Instructions complexes et variées

**Registres principaux :**
```
EAX - Accumulateur (calculs, retour de fonction)
EBX - Base
ECX - Compteur (boucles)
EDX - Données
ESI - Source Index
EDI - Destination Index
EBP - Base Pointer (frame de pile)
ESP - Stack Pointer (sommet de pile)
```

**Exemple de code généré :**
```pascal
function Add(a, b: Integer): Integer;
begin
  Result := a + b;
end;
```

```asm
; Prologue
push ebp
mov ebp, esp

; a est dans [ebp+8], b dans [ebp+12]
mov eax, [ebp+8]      ; Charger a
add eax, [ebp+12]     ; Ajouter b
; Résultat automatiquement dans EAX

; Épilogue
pop ebp
ret
```

### Backend x86_64 (Intel/AMD 64 bits)

**Améliorations par rapport à i386 :**
- 16 registres généraux (au lieu de 8)
- Registres de 64 bits
- Meilleure performance
- Instructions SSE2 par défaut

**Registres :**
```
RAX, RBX, RCX, RDX, RSI, RDI, RBP, RSP  (extensions 64-bit)
R8, R9, R10, R11, R12, R13, R14, R15    (nouveaux)
```

**Exemple de code généré :**
```pascal
function Multiply(a, b: Int64): Int64;
begin
  Result := a * b;
end;
```

```asm
; Windows x64 calling convention
; a dans RCX, b dans RDX
mov rax, rcx
imul rax, rdx     ; Multiplication signée 64 bits
ret               ; Résultat dans RAX
```

### Backend ARM (32 bits)

**Caractéristiques :**
- Architecture RISC (Reduced Instruction Set)
- 16 registres généraux
- Instructions simples et uniformes
- Très économe en énergie (smartphones, tablettes)

**Registres :**
```
R0-R3   - Arguments et valeurs temporaires
R4-R11  - Variables locales (préservés)
R12     - Intra-procedure call scratch
R13(SP) - Stack Pointer
R14(LR) - Link Register (adresse de retour)
R15(PC) - Program Counter
```

**Exemple de code généré :**
```pascal
function Add(a, b: Integer): Integer;
begin
  Result := a + b;
end;
```

```asm
; a dans R0, b dans R1
add r0, r0, r1    ; R0 = R0 + R1
bx lr             ; Retour (branch to LR)
```

**Particularités ARM :**
- Pas de pile matérielle automatique (gérée par logiciel)
- Instructions conditionnelles (tout peut être conditionnel)
- Barrel shifter intégré

```asm
; Instruction conditionnelle
cmp r0, #0        ; Comparer R0 à 0
addgt r1, r1, #1  ; Ajouter 1 à R1 si R0 > 0 (Greater Than)
```

### Backend AArch64 (ARM 64 bits)

**Améliorations :**
- 31 registres généraux de 64 bits
- Meilleure performance
- Support SIMD avancé (NEON)
- Utilisé par Apple Silicon (M1, M2, M3)

**Registres :**
```
X0-X30  - Registres 64 bits
W0-W30  - Accès 32 bits des registres X
SP      - Stack Pointer
PC      - Program Counter (pas directement accessible)
```

**Exemple :**
```asm
; a dans X0, b dans X1
add x0, x0, x1    ; Addition 64 bits
ret               ; Retour
```

### Backend AVR (Arduino)

**Caractéristiques :**
- 8 bits
- 32 registres de 8 bits
- Architecture Harvard (code et données séparés)
- Très limité en ressources

**Particularités :**
```pascal
// Sur Arduino, un Integer est 16 bits !
var
  x: Integer;  // 16 bits sur AVR, 32 bits sur x86
```

**Exemple de code généré :**
```pascal
procedure LED_On;
begin
  PORTB := PORTB or (1 shl 5);  // Allumer la LED pin 13
end;
```

```asm
; AVR assembleur
in r24, PORTB      ; Lire le port B
ori r24, 0x20      ; OR avec 00100000 (bit 5)
out PORTB, r24     ; Écrire dans le port B
ret
```

### Backend WebAssembly

**Caractéristiques :**
- Architecture virtuelle
- Conçu pour le web
- Stack-based (basé sur pile)
- Portable et sécurisé

**Exemple :**
```pascal
function Add(a, b: Integer): Integer;
begin
  Result := a + b;
end;
```

**WebAssembly généré :**
```wasm
(func $Add (param $a i32) (param $b i32) (result i32)
  local.get $a
  local.get $b
  i32.add
)
```

---

## Gestion des différences entre backends

### Code indépendant de l'architecture

Le code Pascal que vous écrivez est généralement portable :

```pascal
program CrossPlatform;
var
  x, y: Integer;
begin
  x := 10;
  y := x * 2;
  WriteLn(y);
end.
```

Ce code compile et fonctionne sur **toutes** les architectures !

### Compilation conditionnelle pour optimisations

Parfois, vous pouvez optimiser pour une architecture spécifique :

```pascal
{$IFDEF CPUX86_64}
  // Code optimisé pour x86-64
  // Utiliser SSE2, registres 64 bits, etc.
{$ENDIF}

{$IFDEF CPUARM}
  // Code optimisé pour ARM
  // Utiliser NEON, instructions conditionnelles, etc.
{$ENDIF}

{$IFDEF CPUAVR}
  // Code optimisé pour AVR
  // Économiser la RAM, utiliser les registres efficacement
{$ENDIF}
```

### Tailles de types variables

Certains types changent de taille selon l'architecture :

```pascal
var
  p: Pointer;    // 4 octets sur 32-bit, 8 octets sur 64-bit
  i: PtrInt;     // Entier de la taille d'un pointeur
  n: NativeInt;  // Entier natif de l'architecture
```

**Utilisation correcte :**
```pascal
// INCORRECT - suppose la taille
if Integer(MyPointer) > 1000 then  // Tronqué sur 64-bit!

// CORRECT - utilise PtrInt/PtrUInt
if PtrUInt(MyPointer) > 1000 then
```

### Alignement mémoire

Les architectures ont des règles d'alignement différentes :

**x86 :** Tolérant, peut accéder à des données non alignées (moins efficace)  
**ARM :** Strict, crash si données non alignées  
**AVR :** Pas de restriction (8 bits)  

```pascal
{$PACKRECORDS 1}  // Pas d'alignement (compact)
type
  TPacked = record
    a: Byte;    // 1 octet
    b: Word;    // 2 octets, peut être non aligné
    c: DWord;   // 4 octets, peut être non aligné
  end;

{$PACKRECORDS DEFAULT}  // Alignement naturel
type
  TAligned = record
    a: Byte;    // 1 octet + 1 octet de padding
    b: Word;    // 2 octets alignés
    c: DWord;   // 4 octets alignés
  end;
```

### Endianness (ordre des octets)

**Little-endian** (x86, x86-64, ARM en général) :
```
Valeur 0x12345678 en mémoire : 78 56 34 12
```

**Big-endian** (PowerPC, certains ARM, réseau) :
```
Valeur 0x12345678 en mémoire : 12 34 56 78
```

**Code portable :**
```pascal
uses
  System.SysUtils;

function SwapEndian(Value: LongWord): LongWord;
begin
  Result := SwapEndian(Value);  // Fonction RTL portable
end;

// Ou utiliser les directives
{$IFDEF ENDIAN_LITTLE}
  // Code pour little-endian
{$ENDIF}
{$IFDEF ENDIAN_BIG}
  // Code pour big-endian
{$ENDIF}
```

---

## Interfaçage avec l'assembleur

### Inline assembler

FPC permet d'écrire de l'assembleur directement dans le code Pascal :

```pascal
function GetCPUID: string;
var
  a, b, c, d: LongWord;
begin
  {$IFDEF CPUX86_64}
  asm
    mov eax, 0           // Fonction CPUID 0
    cpuid
    mov a, eax
    mov b, ebx
    mov c, ecx
    mov d, edx
  end;
  {$ENDIF}

  Result := Format('%x-%x-%x-%x', [a, b, c, d]);
end;
```

### Syntaxe Intel vs AT&T

FPC supporte les deux syntaxes :

**Syntaxe Intel (par défaut) :**
```asm
mov eax, ebx      // destination, source
add eax, 10
```

**Syntaxe AT&T :**
```asm
movl %ebx, %eax   // source, destination (inversé!)
addl $10, %eax
```

### Contraintes et registres

Vous pouvez spécifier quels registres utiliser :

```pascal
procedure FastCopy(src, dst: Pointer; count: Integer);
begin
  asm
    mov esi, src      // Source dans ESI
    mov edi, dst      // Destination dans EDI
    mov ecx, count    // Compteur dans ECX
    rep movsb         // Répéter MOVSB (copy byte)
  end;
end;
```

### Code assembleur externe

Pour du code assembleur complexe, utilisez des fichiers séparés :

**myasm.asm :**
```asm
section .text
global my_fast_function

my_fast_function:
    ; Code assembleur optimisé
    ret
```

**myunit.pas :**
```pascal
unit MyUnit;

interface

function my_fast_function: Integer; cdecl; external;

implementation

end.
```

---

## Debugging d'un backend

### Afficher le code assembleur généré

Option `-a` pour générer le listing assembleur :

```bash
# Générer le fichier .s avec l'assembleur
fpc -a myprogram.pas

# Voir le fichier généré
cat myprogram.s      # Linux
type myprogram.s     # Windows
```

### Options de verbosité

```bash
# Informations détaillées sur la génération de code
fpc -vd myprogram.pas

# Tout afficher (très verbeux!)
fpc -va myprogram.pas
```

### Tracer l'allocation de registres

```bash
# Afficher les décisions d'allocation
fpc -dREGALLOC_DEBUG myprogram.pas
```

### Désassembler le binaire

Pour voir le code machine final :

```bash
# Linux - objdump
objdump -d myprogram

# Windows - dumpbin (Visual Studio)
dumpbin /disasm myprogram.exe

# Alternative multiplateforme - ndisasm (NASM)
ndisasm -b 64 myprogram.exe
```

---

## Optimisations cross-backend

### Code générique optimisé

Le frontend FPC peut optimiser avant même la génération de code :

**Constant folding :**
```pascal
x := 2 + 3;  // Devient : x := 5;
```

**Dead code elimination :**
```pascal
if False then
  DoSomething;  // Complètement supprimé
```

**Loop unrolling :**
```pascal
// Code original
for i := 1 to 4 do
  a[i] := 0;

// Optimisé (déroulé)
a[1] := 0;
a[2] := 0;
a[3] := 0;
a[4] := 0;
```

### Optimisations spécifiques

Certaines optimisations dépendent du backend :

**x86-64 : Utilisation de LEA**
```pascal
x := y * 3;

// Naïf
mov eax, [y]
imul eax, 3

// Optimisé
mov eax, [y]
lea eax, [eax + eax*2]  // eax = eax + eax*2 = eax*3
```

**ARM : Barrel shifter**
```pascal
x := y * 4;

// Naïf
mov r0, [y]
mov r1, #4
mul r0, r0, r1

// Optimisé
ldr r0, [y]
lsl r0, r0, #2     // Shift left 2 = multiplication par 4
```

---

## Portabilité du code

### Écrire du code vraiment portable

**✅ Bonnes pratiques :**

```pascal
// Utiliser les types portables
uses
  SysUtils;

var
  i: NativeInt;      // S'adapte à l'architecture
  p: PtrUInt;        // Entier de taille pointeur
  s: UnicodeString;  // Unicode sur toutes plateformes
```

```pascal
// Vérifier la taille avant de caster
if SizeOf(Pointer) = 8 then
  // Code spécifique 64-bit
else
  // Code spécifique 32-bit
```

```pascal
// Utiliser les constantes système
{$IFDEF CPU64}
  MaxArraySize := 1000000000;
{$ELSE}
  MaxArraySize := 100000000;
{$ENDIF}
```

**❌ Pièges à éviter :**

```pascal
// MAUVAIS - suppose 32 bits
var
  p: Pointer;
  i: Integer;
begin
  i := Integer(p);  // Tronque sur 64-bit !
end;

// MAUVAIS - suppose x86
asm
  mov eax, [ebp+8]  // Ne compile pas sur ARM !
end;

// MAUVAIS - suppose little-endian
type
  TColor = record
    case Boolean of
      False: (Value: LongWord);
      True: (R, G, B, A: Byte);
  end;
```

---

## Tests multi-architecture

### Cross-compilation pour tester

Compiler pour différentes architectures sans quitter votre machine :

```bash
# Depuis Linux x86-64, compiler pour Windows 64-bit
fpc -Twin64 -Px86_64 myprogram.pas

# Depuis Windows, compiler pour Linux ARM
fpc -Tlinux -PARM myprogram.pas

# Compiler pour Raspberry Pi
fpc -Tlinux -PARM -CpARMV7A myprogram.pas
```

### Machines virtuelles

Tester sur des architectures réelles :

**QEMU :** Émulateur multi-architecture
```bash
# Installer QEMU
sudo apt install qemu-user qemu-system-arm

# Exécuter un binaire ARM sur x86
qemu-arm ./myprogram_arm
```

**Docker :** Conteneurs multi-arch
```dockerfile
# Dockerfile pour ARM
FROM arm32v7/debian
COPY myprogram /app/
CMD ["/app/myprogram"]
```

### Tests automatisés

```pascal
// Tests unitaires portables
program TestSuite;

uses
  fpcunit, testregistry;

type
  TPortabilityTests = class(TTestCase)
  published
    procedure TestPointerSize;
    procedure TestEndianness;
    procedure TestAlignment;
  end;

procedure TPortabilityTests.TestPointerSize;
begin
  {$IFDEF CPU64}
  AssertEquals('Pointer size', 8, SizeOf(Pointer));
  {$ELSE}
  AssertEquals('Pointer size', 4, SizeOf(Pointer));
  {$ENDIF}
end;

begin
  RegisterTest(TPortabilityTests);
  RunRegisteredTests;
end.
```

---

## Contribution à un backend existant

### Trouver le code source

```bash
# Cloner le dépôt FPC
git clone https://gitlab.com/freepascal.org/fpc/source.git fpc

# Naviguer vers un backend
cd fpc/compiler/x86_64     # Backend x86-64
cd fpc/compiler/arm        # Backend ARM
cd fpc/compiler/aarch64    # Backend ARM 64-bit
```

### Structure d'un backend

```
x86_64/
├── cgcpu.pas          # Générateur de code principal
├── cpubase.pas        # Définitions de base
├── cpuinfo.pas        # Informations CPU
├── racpu.pas          # Allocation de registres
├── aasmcpu.pas        # Assembleur abstrait
├── agx86.pas          # Sortie format x86
├── ncpuadd.pas        # Nœuds addition
├── ncpucal.pas        # Nœuds appel
└── ...
```

### Ajouter une optimisation

**Exemple : Optimiser x * 2 en x << 1**

Dans `ncpuadd.pas` ou `ncpumat.pas` :

```pascal
procedure tx86_64addnode.pass_generate_code;
begin
  // Vérifier si c'est une multiplication par puissance de 2
  if (nodetype = muln) and
     is_power_of_two(right) and
     fits_in_register(left) then
  begin
    // Utiliser shift au lieu de multiplication
    shift_amount := log2(right.value);
    emit_shift_left(left, shift_amount);
  end
  else
    inherited pass_generate_code;  // Génération standard
end;
```

### Tester votre modification

```bash
# Recompiler le compilateur
make clean
make

# Tester
./compiler/fpc test.pas

# Vérifier l'assembleur généré
./compiler/fpc -a test.pas
cat test.s
```

### Soumettre une contribution

```bash
# Créer une branche
git checkout -b optimize_mul_by_two

# Commiter
git add compiler/x86_64/ncpumat.pas
git commit -m "Optimize multiplication by power of 2 using shifts"

# Créer une Merge Request sur GitLab
# https://gitlab.com/freepascal.org/fpc/source/-/merge_requests
```

---

## Ressources pour aller plus loin

### Documentation officielle

📚 **FPC Internal Documentation**
- `docs/internal.tex` dans les sources FPC
- Architecture détaillée du compilateur

📚 **Processor Manuals**
- Intel 64 and IA-32 Architectures Software Developer's Manual
- ARM Architecture Reference Manual
- AVR Instruction Set Manual

### Livres recommandés

📖 **"Computer Architecture: A Quantitative Approach"** par Hennessy & Patterson
- Concepts fondamentaux des architectures
- RISC vs CISC
- Pipelines et optimisations

📖 **"See MIPS Run"** par Dominic Sweetman
- Excellent pour comprendre RISC
- Très pédagogique

📖 **"ARM System Developer's Guide"**
- Guide complet de l'architecture ARM
- Nombreux exemples

### Sites web et outils

🌐 **Compiler Explorer (godbolt.org)**
- Voir le code assembleur généré par différents compilateurs
- Comparer les backends
- Expérimenter en ligne

🌐 **FPC Wiki - Compiler**
- https://wiki.freepascal.org/Compiler
- Documentation communautaire

🌐 **CPU Manuals**
- https://www.intel.com/sdm (x86)
- https://developer.arm.com/documentation (ARM)

### Communauté

💬 **Liste de diffusion fpc-devel**
- Discussions techniques sur le compilateur
- Questions sur les backends

💬 **Forum Lazarus**
- Section "Advanced"
- Beaucoup d'experts

💬 **GitLab Issues**
- Bugs et feature requests
- Code reviews

---

## Conclusion

Le développement de backends est un domaine fascinant qui se situe à l'intersection de :
- La théorie des compilateurs
- L'architecture des processeurs
- L'optimisation de code
- Le développement système

### Ce que nous avons appris

✅ **Structure d'un backend** : cgcpu, racpu, cpubase, etc.

✅ **Concepts fondamentaux** : registres, instructions, conventions d'appel

✅ **Allocation de registres** : linear scan, graph coloring, spilling

✅ **Optimisations** : peephole, utilisation d'instructions spécialisées

✅ **Portabilité** : écrire du code qui fonctionne sur toutes les architectures

✅ **Backends existants** : x86, ARM, AVR, WebAssembly, etc.

### Prochaines étapes

Pour approfondir vos connaissances :

1. **Étudiez le code source** des backends existants (commencez par x86-64)
2. **Expérimentez** avec des optimisations simples
3. **Contribuez** à FPC en corrigeant des bugs ou en ajoutant des optimisations
4. **Apprenez l'assembleur** d'au moins une architecture (x86-64 ou ARM)
5. **Créez des outils** d'analyse de code assembleur

### Le mot de la fin

Comprendre les backends vous donne un super-pouvoir : vous pouvez **comprendre et optimiser** votre code au niveau le plus bas, tout en écrivant du Pascal portable et élégant.

C'est ce qui fait la beauté de Free Pascal : un langage de haut niveau avec un contrôle de bas niveau quand vous en avez besoin !

---


⏭️ [Optimisations du compilateur](/24-compilateur-outils-avances/03-optimisations-compilateur.md)
