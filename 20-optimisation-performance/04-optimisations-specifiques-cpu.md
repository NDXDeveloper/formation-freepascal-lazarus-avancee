🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 20.4 Optimisations spécifiques CPU

## Introduction

Les processeurs modernes sont des machines extrêmement complexes qui utilisent de nombreuses techniques pour exécuter le code le plus rapidement possible. Comprendre comment fonctionne le CPU permet d'écrire du code qui exploite au mieux ces capacités. Ce chapitre explore les optimisations spécifiques au niveau du processeur, indépendamment du langage de programmation utilisé.

## Architecture du CPU moderne

### Pipeline d'instructions

Un processeur moderne ne traite pas les instructions une par une. Il utilise un **pipeline** pour exécuter plusieurs instructions simultanément à différentes étapes.

**Pipeline simplifié (5 étapes)** :
```
1. FETCH    : Charger l'instruction depuis la mémoire
2. DECODE   : Décoder l'instruction
3. EXECUTE  : Exécuter l'opération
4. MEMORY   : Accéder à la mémoire (si nécessaire)
5. WRITEBACK: Écrire le résultat
```

**Exemple de pipeline en action** :
```
Cycle 1:  [Instr1: FETCH]
Cycle 2:  [Instr1: DECODE] [Instr2: FETCH]
Cycle 3:  [Instr1: EXEC  ] [Instr2: DECODE] [Instr3: FETCH]
Cycle 4:  [Instr1: MEMORY] [Instr2: EXEC  ] [Instr3: DECODE] [Instr4: FETCH]
Cycle 5:  [Instr1: WRITE ] [Instr2: MEMORY] [Instr3: EXEC  ] [Instr4: DECODE] [Instr5: FETCH]
```

Dans un pipeline parfait, une instruction se termine à chaque cycle, même si chaque instruction prend 5 cycles au total.

**IPC (Instructions Per Cycle)** :
- Pipeline idéal : IPC = 1.0 (une instruction par cycle)
- Processeurs modernes : IPC = 2.0 - 4.0 (superscalaire)
- Pipeline avec problèmes : IPC < 1.0

### Exécution superscalaire

Les CPU modernes peuvent exécuter **plusieurs instructions par cycle** grâce à plusieurs unités d'exécution :

```
CPU moderne :
┌─────────────────────────────────────┐
│  2 unités entières (ALU)            │
│  2 unités FPU (float/double)        │
│  2 unités Load/Store (mémoire)      │
│  1 unité de branchement             │
└─────────────────────────────────────┘

Peut exécuter jusqu'à 4-6 instructions par cycle
```

**Exemple** :
```pascal
// Ces 4 instructions peuvent s'exécuter en parallèle
A := B + C;    // ALU #1
D := E + F;    // ALU #2
X := Y * 2.0;  // FPU #1
Load(Z);       // Load/Store
```

### Exécution dans le désordre (Out-of-Order)

Le CPU peut réorganiser les instructions pour maximiser l'utilisation du pipeline :

```pascal
// Code écrit :
A := LoadFromMemory();  // 200 cycles (lent)
B := C + D;             // 1 cycle (rapide)
E := A + 1;             // Dépend de A

// Exécution réelle par le CPU :
// 1. Lancer LoadFromMemory() (asynchrone)
// 2. Exécuter B := C + D (pendant que A charge)
// 3. Attendre A
// 4. Exécuter E := A + 1
```

Le CPU réordonne automatiquement pour éviter que `B := C + D` attende inutilement.

## Branch Prediction (Prédiction de branchement)

### Qu'est-ce qu'un branchement ?

Un **branchement** est un `if`, `while`, `for`, ou tout code qui peut prendre différents chemins :

```pascal
if X > 10 then    // ← Branchement
  DoA()
else
  DoB();
```

### Pourquoi c'est un problème ?

Le CPU ne sait pas quel chemin sera pris avant d'évaluer la condition. En attendant, le pipeline se vide :

```
Sans prédiction :
Cycle 1-5  : Évaluer condition (X > 10)
Cycle 6    : Pipeline vide (stall)
Cycle 7    : Commencer DoA() ou DoB()

Perte : ~10-20 cycles par branchement raté
```

### Branch Predictor (Prédicteur de branchement)

Le CPU **devine** quel chemin sera pris et commence à l'exécuter de manière spéculative :

```
Avec prédiction correcte :
Cycle 1-5  : Évaluer condition + exécuter DoA() (spéculatif)
Cycle 6    : Confirmer prédiction → Continuer
Perte : 0 cycle

Avec prédiction incorrecte :
Cycle 1-5  : Évaluer condition + exécuter DoA() (spéculatif)
Cycle 6    : Prédiction fausse → Annuler + démarrer DoB()
Perte : ~15-20 cycles
```

### Types de prédicteurs

**1. Prédicteur statique** : Toujours prédire "pris" ou "jamais pris"
```pascal
// Prédit "jamais pris"
if RareCondition then  // Presque toujours false
  HandleError();
// Bonne prédiction 99% du temps
```

**2. Prédicteur dynamique** : Apprendre des exécutions passées

**Exemple : Compteur à saturation**
```
État initial : 00 (fortement pas pris)
           ↓
00 → 01 → 10 → 11 (fortement pris)
 ↑              ↓
  ←─────────────
```

```pascal
// Pattern prévisible
for i := 1 to 100 do
begin
  if i mod 2 = 0 then  // Alterne vrai/faux
    DoEven()
  else
    DoOdd();
end;
// Le prédicteur apprend le pattern → ~95% de précision
```

### Taux de prédiction

**Performances** :
- **> 95%** de précision : Excellent (code prévisible)
- **85-95%** : Bon (code avec quelques branches difficiles)
- **< 85%** : Mauvais (code imprévisible, ralentit significativement)

**Mesurer** :
```bash
# Linux avec perf
perf stat -e branches,branch-misses ./programme

# Exemple de sortie :
#   12,345,678  branches
#      456,789  branch-misses  # 3.7% miss rate (excellent)
```

### Optimiser pour la prédiction de branchement

#### ❌ Code imprévisible (mauvais)

```pascal
// Pattern aléatoire → 50% de mispredictions
for i := 0 to 1000000 do
begin
  if Random(2) = 0 then
    ProcessA(Data[i])
  else
    ProcessB(Data[i]);
end;

// Temps : ~150 ms
// Branch miss rate : 50% (très mauvais)
```

#### ✅ Code prévisible (bon)

**Technique 1 : Éliminer le branchement**
```pascal
// Utiliser des opérations arithmétiques au lieu de if
function Max(A, B: Integer): Integer;
begin
  // ❌ Avec branchement
  if A > B then
    Result := A
  else
    Result := B;
end;

function MaxOptimized(A, B: Integer): Integer;
begin
  // ✅ Sans branchement (branchless)
  Result := A + ((B - A) and (Integer(B > A) - 1));
  // ou utiliser l'instruction CMOV sur x86
end;
```

**Technique 2 : Trier avant de traiter**
```pascal
type
  TItem = record
    Value: Integer;
    ShouldProcess: Boolean;
  end;

var
  Items: array of TItem;
  i: Integer;

// ❌ Branchements imprévisibles
for i := 0 to High(Items) do
  if Items[i].ShouldProcess then  // Pattern aléatoire
    Process(Items[i]);

// ✅ Trier d'abord, puis traiter
SortItemsByShouldProcess(Items);  // Tous les True ensemble
for i := 0 to High(Items) do
  if Items[i].ShouldProcess then  // Pattern prévisible
    Process(Items[i])
  else
    Break;  // Sortir quand on atteint les False

// Gain : 2x à 3x plus rapide si beaucoup d'éléments
```

**Technique 3 : Utiliser des tables de lookup**
```pascal
// ❌ Beaucoup de branchements
function GetCategory(Value: Integer): string;
begin
  if Value < 10 then
    Result := 'Low'
  else if Value < 50 then
    Result := 'Medium'
  else if Value < 100 then
    Result := 'High'
  else
    Result := 'Very High';
end;

// ✅ Table de lookup (sans branchement)
const
  CategoryTable: array[0..100] of string = (
    // 0-9: Low
    'Low', 'Low', 'Low', 'Low', 'Low', 'Low', 'Low', 'Low', 'Low', 'Low',
    // 10-49: Medium
    'Medium', 'Medium', ...,
    // 50-99: High
    'High', 'High', ...,
    // 100: Very High
    'Very High'
  );

function GetCategoryOptimized(Value: Integer): string;
begin
  if Value > 100 then Value := 100;
  if Value < 0 then Value := 0;
  Result := CategoryTable[Value];
end;
```

**Technique 4 : Utiliser CMOV (Conditional Move)**
```pascal
{$ASMMODE INTEL}

function MinBranchless(A, B: Integer): Integer;
asm
  mov eax, A
  mov edx, B
  cmp eax, edx
  cmovg eax, edx  // Move si greater (branchless)
end;

// CMOV exécute toujours en 1 cycle, pas de pipeline flush
```

## Cache et hiérarchie mémoire

### Principe de localité

**Localité temporelle** : Une donnée récemment accédée sera probablement réutilisée bientôt.  
**Localité spatiale** : Les données voisines seront probablement accédées ensemble.  

```pascal
// ✅ Bonne localité spatiale
var
  Data: array[0..9999] of Integer;
  i: Integer;
begin
  for i := 0 to 9999 do
    Process(Data[i]);  // Accès séquentiel
end;
// Temps : ~5 ms

// ❌ Mauvaise localité spatiale
begin
  for i := 0 to 9999 do
    Process(Data[Random(10000)]);  // Accès aléatoire
end;
// Temps : ~80 ms (16x plus lent !)
```

### Cache Line (Ligne de cache)

Le cache charge la mémoire par blocs de **64 bytes** (ligne de cache) :

```pascal
type
  TData = record
    A: Integer;  // Offset 0
    B: Integer;  // Offset 4
  end;

var
  Data: TData;

// Accéder à Data.A charge toute la ligne de cache (64 bytes)
// → Data.B est déjà en cache (gratuit)
X := Data.A;
Y := Data.B;  // Presque instantané (cache hit)
```

### False Sharing (Partage factice)

**Problème en multi-threading** :

```pascal
type
  TCounter = record
    Value: Int64;  // 8 bytes
  end;

var
  Counters: array[0..7] of TCounter;  // 8 threads

// Thread 1 modifie Counters[0]
// Thread 2 modifie Counters[1]
// Problème : Les deux sont dans la MÊME ligne de cache (64 bytes)
// → Chaque modification invalide le cache de l'autre thread
```

**Solution : Padding**
```pascal
type
  TCounter = record
    Value: Int64;
    Padding: array[0..6] of Int64;  // Force 64 bytes
  end align 64;

var
  Counters: array[0..7] of TCounter;
// Maintenant chaque compteur est dans sa propre ligne de cache
```

### Prefetching (Préchargement)

Le CPU essaie de deviner quelles données seront nécessaires et les charge en avance.

**Prefetch automatique** :
```pascal
// Pattern séquentiel → Prefetcher détecte et charge en avance
for i := 0 to 1000000 do
  Sum := Sum + Data[i];
// Le CPU charge Data[i+10] pendant qu'il traite Data[i]
```

**Prefetch manuel** :
```pascal
{$ASMMODE INTEL}

procedure ProcessWithPrefetch(Data: PInteger; Count: Integer);
var i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    // Prefetch manuel 64 éléments à l'avance
    if i + 64 < Count then
      asm
        mov rax, Data
        lea rax, [rax + (i + 64) * 4]
        prefetchnta [rax]
      end;

    Process(Data[i]);
  end;
end;
```

**Types de prefetch** :
- `prefetchnta` : Non-temporal (ne pollue pas le cache L3)
- `prefetcht0` : Vers tous les niveaux de cache
- `prefetcht1` : Vers L2/L3
- `prefetcht2` : Vers L3 uniquement

## Optimisations spécifiques aux architectures

### x86/x64 (Intel/AMD)

#### Registres et conventions

**Registres 64-bit** :
```
RAX, RBX, RCX, RDX : Registres généraux
RSI, RDI           : Source/Destination
RBP, RSP           : Base/Stack pointer
R8-R15             : Registres additionnels (x64)
```

**Registres SIMD** :
```
XMM0-XMM15 : 128 bits (SSE)
YMM0-YMM15 : 256 bits (AVX)
ZMM0-ZMM31 : 512 bits (AVX-512)
```

#### Instructions spécifiques Intel

**BSF/BSR** : Bit Scan Forward/Reverse
```pascal
{$ASMMODE INTEL}

function FindFirstSetBit(Value: QWord): Integer;
// Trouve la position du premier bit à 1
asm
  bsf rax, Value
  jnz @Found
  mov rax, -1  // Aucun bit trouvé
@Found:
end;
// Temps : 3 cycles (vs ~20 cycles en boucle)
```

**POPCNT** : Population Count (nombre de bits à 1)
```pascal
function CountBits(Value: QWord): Integer;
asm
  popcnt rax, Value
end;
// Temps : 3 cycles (vs ~64 cycles en boucle)
```

**LZCNT** : Leading Zero Count
```pascal
function LeadingZeros(Value: QWord): Integer;
asm
  lzcnt rax, Value
end;
```

**BMI/BMI2** : Bit Manipulation Instructions
```pascal
// ANDN: AND NOT
function AndNot(A, B: QWord): QWord;
asm
  mov rax, A
  andn rax, rax, B  // rax = A AND (NOT B)
end;

// BZHI: Zero high bits
function ZeroHighBits(Value: QWord; Index: Byte): QWord;
asm
  mov rax, Value
  mov cl, Index
  bzhi rax, rax, rcx  // Mettre à 0 les bits >= Index
end;
```

#### Optimisations de cache Intel

**Cache Associativity** :
- L1: 8-way set associative
- L2: 4-way
- L3: 16-way

**Optimisation** : Éviter les conflits d'associativité
```pascal
// ❌ Mauvais : Stride de 4096 (taille page)
// Cause des conflits de cache
for i := 0 to 999 do
  Access(Data[i * 4096]);

// ✅ Bon : Stride différent
for i := 0 to 999 do
  Access(Data[i * 4097]);  // Prime avec 4096
```

### ARM (Raspberry Pi, mobiles)

#### Registres ARM64

```
X0-X30  : Registres généraux 64-bit
W0-W30  : Registres 32-bit (moitié basse de X0-X30)
SP      : Stack Pointer
PC      : Program Counter
V0-V31  : Registres NEON/FPU (128-bit)
```

#### Instructions ARM spécifiques

**Barrel Shifter** : Décalage gratuit
```pascal
// Sur ARM, le décalage ne coûte rien s'il est combiné
A := B + (C shl 2);  // 1 cycle (décalage gratuit)

// Équivalent en ARM assembly :
// ADD X0, X1, X2, LSL #2
```

**Conditional Execution** : Presque toutes les instructions peuvent être conditionnelles
```asm
CMP X0, X1
ADDGT X2, X3, X4  // ADD si Greater Than (pas de branchement!)
```

**NEON (SIMD ARM)** :
```pascal
// Voir section 20.3 pour détails NEON
```

### Différences AMD vs Intel

| Caractéristique | Intel | AMD (Zen 3+) |
|-----------------|-------|--------------|
| **Pipeline** | 14+ étages | 10+ étages |
| **Cache L3** | Partagé | Par CCX (chiplet) |
| **Latence mémoire** | ~70ns | ~60-80ns selon CCX |
| **AVX-512** | Oui (Ice Lake+) | Non (sauf Zen 4+) |
| **SMT** | Hyper-Threading | SMT |
| **Optimisations** | µop cache | Op cache |

**Code qui performe différemment** :
```pascal
// Calcul intensif avec beaucoup de branchements
// → Intel généralement plus rapide (meilleur predicteur)

// Calcul parallèle multi-threadé
// → AMD compétitif ou plus rapide (plus de cœurs)

// AVX-512
// → Intel beaucoup plus rapide (AMD n'a pas AVX-512 avant Zen 4)
```

## Optimisations du compilateur FreePascal

### Options de compilation

```bash
# Optimisations de base
fpc -O1 programme.pas
# - Élimination du code mort
# - Propagation de constantes

# Optimisations standards (recommandé)
fpc -O2 programme.pas
# - Tout de -O1
# - Loop unrolling (déroulement de boucles)
# - Inline de fonctions
# - Réordonnancement d'instructions

# Optimisations agressives
fpc -O3 programme.pas
# - Tout de -O2
# - Vectorisation automatique
# - Optimisations plus agressives

# Optimisation pour taille
fpc -Os programme.pas
# - Réduire la taille du code
```

### Optimisations spécifiques CPU

```bash
# Generic x86-64
fpc -O3 -Px86_64 programme.pas

# Intel Core (Sandy Bridge+)
fpc -O3 -CpCOREI -CfAVX programme.pas

# Intel Core (Haswell+, avec AVX2)
fpc -O3 -CpCOREAVX2 -CfAVX2 programme.pas

# AMD Ryzen
fpc -O3 -CpZEN -CfAVX2 programme.pas

# ARM Cortex-A53 (Raspberry Pi 3)
fpc -O3 -CpARMV8 programme.pas

# ARM Cortex-A72 (Raspberry Pi 4)
fpc -O3 -CpARMV8 -CfVFPV4 programme.pas
```

### Inline et déroulement de boucles

**Inline de fonctions** :
```pascal
function Square(X: Integer): Integer; inline;
begin
  Result := X * X;
end;

// Le compilateur remplace l'appel par le code directement
Y := Square(5);  // Devient : Y := 5 * 5;
```

**Déroulement de boucles** :
```pascal
// Code source
for i := 0 to 99 do
  A[i] := i;

// Code généré avec -O3 (loop unrolling)
i := 0;
while i <= 96 do
begin
  A[i] := i;
  A[i+1] := i+1;
  A[i+2] := i+2;
  A[i+3] := i+3;  // 4 itérations par boucle
  Inc(i, 4);
end;
// Traiter les 3 derniers éléments
while i <= 99 do
begin
  A[i] := i;
  Inc(i);
end;
```

**Bénéfices** :
- Moins d'overhead de boucle
- Meilleure utilisation du pipeline
- Permet la vectorisation SIMD

## Techniques d'optimisation avancées

### 1. Software Pipelining

Réorganiser le code pour maximiser le parallélisme :

```pascal
// ❌ Version non optimisée
for i := 0 to N - 1 do
begin
  A := Load(i);      // 200 cycles
  B := Process(A);   // 10 cycles (attend A)
  Store(i, B);       // 5 cycles (attend B)
end;
// Total : N × 215 cycles

// ✅ Version avec software pipelining
A0 := Load(0);  // Précharger
for i := 0 to N - 2 do
begin
  A1 := Load(i + 1);     // Charger le suivant
  B0 := Process(A0);     // Traiter le courant (pendant que A1 charge)
  Store(i, B0);          // Stocker le courant
  A0 := A1;              // Préparer l'itération suivante
end;
B0 := Process(A0);
Store(N - 1, B0);
// Total : N × 200 cycles (10% plus rapide)
```

### 2. Loop Fusion (Fusion de boucles)

Combiner plusieurs boucles pour améliorer la localité de cache :

```pascal
// ❌ Deux boucles séparées
for i := 0 to N - 1 do
  A[i] := B[i] + 1;

for i := 0 to N - 1 do
  C[i] := A[i] * 2;

// ✅ Boucle fusionnée
for i := 0 to N - 1 do
begin
  A[i] := B[i] + 1;
  C[i] := A[i] * 2;
end;
// Gain : Meilleure utilisation du cache L1
```

### 3. Loop Interchange (Permutation de boucles)

Réorganiser les boucles imbriquées pour un meilleur accès mémoire :

```pascal
// ❌ Mauvais : Accès par colonnes (cache miss)
for i := 0 to Rows - 1 do
  for j := 0 to Cols - 1 do
    Matrix[j][i] := 0;  // Stride = Cols (mauvais)

// ✅ Bon : Accès par lignes (cache-friendly)
for i := 0 to Cols - 1 do
  for j := 0 to Rows - 1 do
    Matrix[i][j] := 0;  // Stride = 1 (bon)

// Gain : 5x à 10x plus rapide pour grandes matrices
```

### 4. Loop Tiling (Découpage en blocs)

Découper les grandes boucles en blocs qui tiennent dans le cache :

```pascal
const
  TileSize = 64;  // Taille optimale pour cache L1

// ❌ Sans tiling (cache thrashing)
for i := 0 to N - 1 do
  for j := 0 to N - 1 do
    C[i, j] := A[i, j] + B[i, j];

// ✅ Avec tiling
for ii := 0 to (N - 1) div TileSize do
  for jj := 0 to (N - 1) div TileSize do
    for i := ii * TileSize to Min((ii + 1) * TileSize - 1, N - 1) do
      for j := jj * TileSize to Min((jj + 1) * TileSize - 1, N - 1) do
        C[i, j] := A[i, j] + B[i, j];

// Gain : 2x à 3x plus rapide pour N > 1000
```

### 5. Strength Reduction

Remplacer les opérations coûteuses par des opérations plus rapides :

```pascal
// ❌ Multiplication dans la boucle
for i := 0 to N - 1 do
  A[i] := i * 7;

// ✅ Addition incrémentale
Value := 0;
for i := 0 to N - 1 do
begin
  A[i] := Value;
  Value := Value + 7;  // Addition au lieu de multiplication
end;
```

## Profiling au niveau CPU

### Compteurs de performance (PMU)

Les CPUs modernes ont des compteurs matériels pour mesurer :
- Instructions exécutées
- Cycles d'horloge
- Cache hits/misses
- Branch mispredictions
- Stalls de pipeline

**Linux (perf)** :
```bash
# Compteurs de base
perf stat ./programme

# Compteurs spécifiques
perf stat -e cycles,instructions,cache-references,cache-misses,branches,branch-misses ./programme

# Exemple de sortie :
#   987,654,321  cycles
#   1,234,567,890  instructions  # IPC = 1.25
#   12,345,678  cache-references
#      456,789  cache-misses      # 3.7% miss rate
#   234,567,890  branches
#      5,678,901  branch-misses   # 2.4% miss rate
```

**Windows (Intel VTune)** :
- Microarchitecture Exploration
- Memory Access Analysis
- Branch Analysis

### Interpréter les métriques

**IPC (Instructions Per Cycle)** :
- IPC > 2.0 : Excellent (code bien optimisé)
- IPC 1.0-2.0 : Bon à correct
- IPC < 1.0 : Mauvais (beaucoup de stalls)

**Cache Miss Rate** :
- < 1% : Excellent
- 1-5% : Bon
- 5-10% : Moyen
- > 10% : Mauvais

**Branch Miss Rate** :
- < 2% : Excellent
- 2-5% : Bon
- 5-10% : Moyen
- > 10% : Mauvais

## Optimisations multi-plateformes

### Code adaptatif selon le CPU

```pascal
unit CPUOptimized;

interface

type
  TProcessFunc = procedure(Data: Pointer; Size: Integer);

var
  ProcessData: TProcessFunc;  // Pointeur vers la meilleure implémentation

implementation

uses
  CPUFeatures;  // Détection des capacités (voir section 20.3)

var
  Features: TCPUFeatures;

procedure ProcessScalar(Data: Pointer; Size: Integer);
begin
  // Implémentation de base
end;

procedure ProcessSSE(Data: Pointer; Size: Integer);
begin
  // Implémentation SSE
end;

procedure ProcessAVX2(Data: Pointer; Size: Integer);
begin
  // Implémentation AVX2
end;

procedure ProcessNEON(Data: Pointer; Size: Integer);
begin
  // Implémentation ARM NEON
end;

initialization
  // Sélectionner la meilleure implémentation au démarrage
  {$IFDEF CPUX64}
  Features := DetectCPUFeatures;

  if Features.HasAVX2 then
    ProcessData := @ProcessAVX2
  else if Features.HasSSE41 then
    ProcessData := @ProcessSSE
  else
    ProcessData := @ProcessScalar;
  {$ENDIF}

  {$IFDEF CPUARM}
  if HasNEON then
    ProcessData := @ProcessNEON
  else
    ProcessData := @ProcessScalar;
  {$ENDIF}

  {$IF NOT DEFINED(CPUX64) AND NOT DEFINED(CPUARM)}
  ProcessData := @ProcessScalar;
  {$ENDIF}
end.
```

### Directives de compilation conditionnelle

```pascal
procedure OptimizedFunction;
begin
  {$IFDEF CPUX64}
    {$IF DEFINED(CPUAVX2)}
    // Code AVX2
    ProcessAVX2();
    {$ELSEIF DEFINED(CPUSSE41)}
    // Code SSE4.1
    ProcessSSE();
    {$ELSE}
    // Code x64 générique
    ProcessX64();
    {$ENDIF}
  {$ENDIF}

  {$IFDEF CPUARM}
    {$IFDEF CPUNEON}
    // Code NEON
    ProcessNEON();
    {$ELSE}
    // Code ARM générique
    ProcessARM();
    {$ENDIF}
  {$ENDIF}

  {$IF NOT DEFINED(CPUX64) AND NOT DEFINED(CPUARM)}
    // Code portable par défaut
    ProcessScalar();
  {$ENDIF}
end;
```

### Macros pour code multi-architecture

```pascal
// Fichier: platform_macros.inc

{$IFDEF CPUX64}
  {$DEFINE HAS_FAST_MULTIPLY}
  {$DEFINE HAS_64BIT_INTEGERS}
  {$DEFINE CACHE_LINE_SIZE := 64}
{$ENDIF}

{$IFDEF CPUARM}
  {$DEFINE HAS_BARREL_SHIFTER}
  {$IFDEF CPUARM64}
    {$DEFINE HAS_64BIT_INTEGERS}
  {$ENDIF}
  {$DEFINE CACHE_LINE_SIZE := 64}
{$ENDIF}

{$IFDEF CPUI386}
  {$DEFINE CACHE_LINE_SIZE := 64}
{$ENDIF}

// Utilisation dans le code
{$I platform_macros.inc}

procedure AlignedAlloc;
var
  Data: Pointer;
begin
  {$IFDEF HAS_64BIT_INTEGERS}
  Data := GetMem(1024 * 1024);  // 1 MB
  {$ELSE}
  Data := GetMem(512 * 1024);   // 512 KB (32-bit limité)
  {$ENDIF}
end;
```

## Pièges et anti-patterns

### 1. Fausse dépendance de données

```pascal
// ❌ Fausse dépendance : réutilisation de variable
var
  Temp: Integer;
begin
  Temp := A + B;
  Result1 := Temp * 2;
  Temp := C + D;      // Réutilise Temp (dépendance artificielle)
  Result2 := Temp * 3;
end;
// Le CPU doit attendre que Result1 soit calculé

// ✅ Pas de fausse dépendance
var
  Temp1, Temp2: Integer;
begin
  Temp1 := A + B;
  Temp2 := C + D;     // Peut s'exécuter en parallèle
  Result1 := Temp1 * 2;
  Result2 := Temp2 * 3;
end;
```

### 2. Écriture partielle de registre

```pascal
{$ASMMODE INTEL}

// ❌ Mauvais : écriture partielle (pénalité)
procedure BadPartialWrite;
asm
  mov eax, [Data]   // Écrit 32-bit
  mov al, 5         // Écrit 8-bit bas → pénalité!
  mov [Result], eax
end;
// Pénalité : ~5 cycles de stall

// ✅ Bon : écriture complète
procedure GoodFullWrite;
asm
  xor eax, eax      // Mettre à zéro d'abord
  mov al, 5         // Maintenant OK
  mov [Result], eax
end;
```

### 3. Instruction lentes

Certaines instructions sont beaucoup plus lentes que d'autres :

| Instruction | Cycles | Alternative |
|-------------|--------|-------------|
| `DIV` (division) | 20-40 | Multiplication par inverse |
| `IDIV` (division signée) | 25-50 | Shift si puissance de 2 |
| `MUL` (64-bit) | 3-5 | Éviter si possible |
| `IMUL` (32-bit) | 3 | OK |
| `SQRT` | 10-15 | LUT ou approximation |
| `FPU DIV/SQRT` | 15-25 | SSE est plus rapide |

```pascal
// ❌ Division lente
Result := Value div 16;  // ~30 cycles

// ✅ Shift (puissance de 2)
Result := Value shr 4;   // 1 cycle

// ❌ Division par constante non-puissance de 2
Result := Value div 10;  // ~30 cycles

// ✅ Multiplication magique (compilateur peut optimiser)
// Manuellement : Result := (Value * 0xCCCCCCCD) shr 35;
Result := Value div 10;  // Avec -O3, optimisé automatiquement
```

### 4. Latence vs Throughput

**Latence** : Temps pour une opération seule  
**Throughput** : Nombre d'opérations par cycle  

```pascal
// Addition entière :
// Latence = 1 cycle
// Throughput = 3-4 ops/cycle (plusieurs ALU)

// Division entière :
// Latence = 30 cycles
// Throughput = 0.05 ops/cycle (1 divider)

// ❌ Dépendance de latence
A := X div Y;  // 30 cycles
B := A + 1;    // Attend A (stall de 30 cycles)

// ✅ Pas de dépendance
A := X div Y;  // 30 cycles
B := Z + 1;    // Exécute en parallèle (pas de dépendance)
C := A + 1;    // Attend A, mais B est déjà fait
```

### 5. Alignement et accès atomiques

```pascal
// ❌ Non aligné (peut être lent ou crasher)
type
  TData = packed record
    Flag: Byte;
    Value: Int64;  // Non aligné sur 8 bytes!
  end;

var
  D: TData;
begin
  D.Value := 123;  // Accès non aligné (lent ou crash)
end;

// ✅ Aligné
type
  TData = record
    Flag: Byte;
    Padding: array[0..6] of Byte;
    Value: Int64;  // Aligné sur 8 bytes
  end;

// Ou utiliser align
type
  TData = record
    Value: Int64;
  end align 8;
```

## Techniques spécifiques par domaine

### 1. Calcul intensif

**Optimisations** :
- Utiliser FMA (Fused Multiply-Add) si disponible
- Éviter les divisions (utiliser la multiplication par l'inverse)
- Vectoriser avec SIMD (voir section 20.3)
- Minimiser les conversions float ↔ int

```pascal
{$ASMMODE INTEL}

// ❌ Division en boucle
for i := 0 to N - 1 do
  Result[i] := Data[i] / Divisor;

// ✅ Multiplication par inverse
var
  InvDivisor: Single;
begin
  InvDivisor := 1.0 / Divisor;  // Une division
  for i := 0 to N - 1 do
    Result[i] := Data[i] * InvDivisor;  // N multiplications
end;
// Gain : 5x à 10x plus rapide
```

### 2. Traitement de chaînes

**Optimisations** :
- Utiliser des opérations SIMD pour recherche/comparaison
- Éviter les copies inutiles
- Préallouer les buffers

```pascal
{$ASMMODE INTEL}

// Recherche de caractère avec SSE2
function FindCharSSE2(const S: String; C: Char): Integer;
var
  P: PChar;
  Len, i: Integer;
  CharVec: array[0..15] of Char;
begin
  Result := 0;
  Len := Length(S);
  if Len = 0 then Exit;

  // Remplir un vecteur avec le caractère recherché
  for i := 0 to 15 do
    CharVec[i] := C;

  P := PChar(S);
  i := 0;

  // Traiter par blocs de 16 caractères
  while i + 16 <= Len do
  begin
    asm
      mov rax, P
      add rax, i

      movdqu xmm0, [rax]        // Charger 16 chars
      movdqu xmm1, CharVec      // Charger le pattern

      pcmpeqb xmm0, xmm1        // Comparer (0xFF si égal)
      pmovmskb eax, xmm0        // Extraire le masque

      test eax, eax
      jz @NotFound

      // Trouvé : calculer la position
      bsf eax, eax              // Position du premier bit
      add eax, i
      mov Result, eax
      jmp @Done

    @NotFound:
    end;

    Inc(i, 16);
  end;

  // Traiter les caractères restants
  while i < Len do
  begin
    if P[i] = C then
    begin
      Result := i + 1;  // Position (base 1)
      Exit;
    end;
    Inc(i);
  end;

@Done:
end;
// Gain : 8x à 16x plus rapide que Pos()
```

### 3. Compression/Décompression

**Optimisations** :
- Utiliser les instructions BMI/BMI2 pour manipulation de bits
- SIMD pour traitement par blocs
- Tables de lookup pour Huffman

```pascal
// Comptage de bits avec POPCNT
function CountSetBits(Value: QWord): Integer;
asm
  {$IFDEF CPUPOPCNT}
  popcnt rax, Value
  {$ELSE}
  // Fallback sans POPCNT
  mov rax, Value
  mov rcx, rax
  shr rax, 1
  and rax, 0x5555555555555555
  sub rcx, rax
  mov rax, rcx
  shr rcx, 2
  and rax, 0x3333333333333333
  and rcx, 0x3333333333333333
  add rax, rcx
  mov rcx, rax
  shr rax, 4
  add rax, rcx
  and rax, 0x0F0F0F0F0F0F0F0F
  imul rax, rax, 0x0101010101010101
  shr rax, 56
  {$ENDIF}
end;
```

### 4. Cryptographie

**Optimisations** :
- Utiliser AES-NI pour AES
- Utiliser SHA extensions pour SHA-1/SHA-256
- Éviter les branches (timing attacks)
- Opérations en temps constant

```pascal
{$ASMMODE INTEL}

// AES avec instructions AES-NI
procedure AESEncryptBlockNI(const Key, Input: array of Byte; var Output: array of Byte);
asm
  mov rax, Key
  mov rdx, Input
  mov rcx, Output

  movdqu xmm0, [rdx]          // Charger le plaintext
  movdqu xmm1, [rax]          // Charger la clé de round 0

  pxor xmm0, xmm1             // Whitening

  // 10 rounds pour AES-128
  movdqu xmm1, [rax + 16]
  aesenc xmm0, xmm1           // Round 1

  movdqu xmm1, [rax + 32]
  aesenc xmm0, xmm1           // Round 2

  // ... rounds 3-9 ...

  movdqu xmm1, [rax + 160]
  aesenclast xmm0, xmm1       // Round 10 (final)

  movdqu [rcx], xmm0          // Stocker le ciphertext
end;
// Gain : 5x à 10x plus rapide que implémentation pure Pascal
```

## Benchmarking et validation

### Template de benchmark CPU

```pascal
program CPUBenchmark;

{$MODE OBJFPC}{$H+}
{$ASMMODE INTEL}

uses
  SysUtils, DateUtils;

const
  Iterations = 10000000;

var
  StartTime: TDateTime;
  ElapsedMs: Int64;
  A, B, C: Int64;

procedure BenchmarkAddition;
var i: Integer;
begin
  StartTime := Now;
  for i := 1 to Iterations do
    C := A + B;
  ElapsedMs := MilliSecondsBetween(Now, StartTime);
  WriteLn('Addition:         ', ElapsedMs, ' ms');
end;

procedure BenchmarkMultiplication;
var i: Integer;
begin
  StartTime := Now;
  for i := 1 to Iterations do
    C := A * B;
  ElapsedMs := MilliSecondsBetween(Now, StartTime);
  WriteLn('Multiplication:   ', ElapsedMs, ' ms');
end;

procedure BenchmarkDivision;
var i: Integer;
begin
  StartTime := Now;
  for i := 1 to Iterations do
    C := A div B;
  ElapsedMs := MilliSecondsBetween(Now, StartTime);
  WriteLn('Division:         ', ElapsedMs, ' ms');
end;

procedure BenchmarkBitShift;
var i: Integer;
begin
  StartTime := Now;
  for i := 1 to Iterations do
    C := A shr 4;
  ElapsedMs := MilliSecondsBetween(Now, StartTime);
  WriteLn('Bit Shift:        ', ElapsedMs, ' ms');
end;

procedure BenchmarkBranchPredictable;
var i: Integer;
begin
  StartTime := Now;
  for i := 1 to Iterations do
  begin
    if i mod 2 = 0 then  // Pattern prévisible
      C := A + B
    else
      C := A - B;
  end;
  ElapsedMs := MilliSecondsBetween(Now, StartTime);
  WriteLn('Branch (pred.):   ', ElapsedMs, ' ms');
end;

procedure BenchmarkBranchUnpredictable;
var i: Integer;
begin
  StartTime := Now;
  for i := 1 to Iterations do
  begin
    if Random(2) = 0 then  // Pattern aléatoire
      C := A + B
    else
      C := A - B;
  end;
  ElapsedMs := MilliSecondsBetween(Now, StartTime);
  WriteLn('Branch (unpred.): ', ElapsedMs, ' ms');
end;

begin
  Randomize;
  A := 12345;
  B := 67890;

  WriteLn('=== CPU Benchmark ===');
  WriteLn('Iterations: ', Iterations);
  WriteLn;

  BenchmarkAddition;
  BenchmarkMultiplication;
  BenchmarkDivision;
  BenchmarkBitShift;
  BenchmarkBranchPredictable;
  BenchmarkBranchUnpredictable;

  WriteLn;
  WriteLn('Ratios:');
  WriteLn('  Div/Add:  ', (ElapsedDiv / ElapsedAdd):0:1, 'x');
  WriteLn('  Unpred/Pred branch: ', (ElapsedUnpred / ElapsedPred):0:1, 'x');

  ReadLn;
end.

// Exemple de sortie sur Intel i7 :
// === CPU Benchmark ===
// Iterations: 10000000
//
// Addition:         85 ms
// Multiplication:   90 ms
// Division:         2450 ms
// Bit Shift:        82 ms
// Branch (pred.):   180 ms
// Branch (unpred.): 850 ms
//
// Ratios:
//   Div/Add:  28.8x
//   Unpred/Pred branch: 4.7x
```

### Mesurer IPC et efficacité

```bash
# Linux avec perf
perf stat -e cycles,instructions,cache-misses,branch-misses ./programme

# Calculer IPC
# IPC = instructions / cycles
```

```pascal
// Inclure les mesures dans le code (Linux)
{$IFDEF LINUX}
uses
  Linux, BaseUnix;

procedure MeasurePerformance;
var
  PerfFD: Integer;
  Counter: Int64;
begin
  // Ouvrir compteur de cycles
  PerfFD := perf_event_open(...);

  // Code à mesurer
  MyFunction();

  // Lire compteur
  FpRead(PerfFD, Counter, SizeOf(Counter));
  WriteLn('Cycles: ', Counter);
end;
{$ENDIF}
```

## Différences de performance Windows vs Linux

### Ordonnanceur (Scheduler)

**Windows** :
- Priorités dynamiques
- Préemption par quantum (time slice)
- Affinité CPU configurable

**Linux** :
- CFS (Completely Fair Scheduler)
- Meilleure gestion des tâches I/O-bound
- `taskset` pour affinité CPU

```bash
# Linux : fixer l'affinité CPU
taskset -c 0,1 ./programme  # Exécuter sur cores 0 et 1

# Linux : définir la priorité
nice -n -20 ./programme  # Priorité maximale
```

```pascal
// Windows : définir l'affinité
{$IFDEF WINDOWS}
uses Windows;

procedure SetCPUAffinity(Mask: DWORD_PTR);
begin
  SetThreadAffinityMask(GetCurrentThread, Mask);
end;

// Exécuter sur le core 0 uniquement
SetCPUAffinity(1);  // 0001b = core 0

// Exécuter sur cores 0 et 1
SetCPUAffinity(3);  // 0011b = cores 0,1
{$ENDIF}
```

### Gestion de la mémoire

**Windows** :
- HeapAlloc/HeapFree
- Virtual Memory Manager
- Large Pages disponibles avec privilèges

**Linux** :
- malloc/free (glibc)
- Page Cache très agressif
- Transparent Huge Pages (THP)

```bash
# Linux : activer THP
echo always > /sys/kernel/mm/transparent_hugepage/enabled

# Linux : flush page cache (test uniquement!)
sync; echo 3 > /proc/sys/vm/drop_caches
```

### Performance relative

| Opération | Windows | Linux | Différence |
|-----------|---------|-------|------------|
| **Allocation mémoire** | 100% | 90-110% | Similaire |
| **I/O fichier** | 100% | 130-150% | Linux plus rapide |
| **Création thread** | 100% | 120-140% | Linux plus rapide |
| **Appels système** | 100% | 110-120% | Linux légèrement plus rapide |
| **Calcul pur** | 100% | 98-102% | Similaire |

**Note** : Ces chiffres sont approximatifs et dépendent fortement de la charge de travail.

## Checklist d'optimisation CPU

### Avant d'optimiser

- [ ] Profiler avec perf/VTune pour identifier les hotspots
- [ ] Mesurer IPC (devrait être > 1.5)
- [ ] Mesurer branch miss rate (devrait être < 5%)
- [ ] Mesurer cache miss rate (devrait être < 5%)
- [ ] Établir une baseline de performance

### Optimisations à considérer

**Niveau 1 : Algorithme**
- [ ] Choisir le bon algorithme (O(n log n) vs O(n²))
- [ ] Utiliser les bonnes structures de données
- [ ] Éliminer les calculs redondants

**Niveau 2 : Pipeline CPU**
- [ ] Réduire les branchements imprévisibles
- [ ] Éliminer les fausses dépendances
- [ ] Réorganiser le code pour meilleur pipelining

**Niveau 3 : Cache**
- [ ] Améliorer la localité spatiale (accès séquentiels)
- [ ] Améliorer la localité temporelle (réutiliser les données)
- [ ] Aligner les structures sur les lignes de cache

**Niveau 4 : Instructions**
- [ ] Remplacer divisions par multiplications
- [ ] Utiliser bit shifts pour puissances de 2
- [ ] Utiliser SIMD (voir section 20.3)
- [ ] Éviter les instructions lentes

**Niveau 5 : Compilateur**
- [ ] Compiler avec -O3
- [ ] Spécifier l'architecture cible (-CpCOREAVX2)
- [ ] Activer l'inline de fonctions
- [ ] Tester différentes options d'optimisation

### Après optimisation

- [ ] Re-profiler et comparer avec la baseline
- [ ] Vérifier que IPC a augmenté
- [ ] Vérifier que cache/branch misses ont diminué
- [ ] Valider la correction des résultats
- [ ] Tester sur Windows ET Linux
- [ ] Tester sur différents CPUs (Intel/AMD/ARM)

## Outils et ressources

### Outils de mesure

**Linux** :
- `perf` : Profilage et compteurs CPU
- `valgrind --tool=cachegrind` : Simulation de cache
- `likwid` : Interface aux compteurs PMU
- `vtune` : Intel VTune (version Linux)

**Windows** :
- Intel VTune Profiler
- AMD uProf
- Windows Performance Analyzer (WPA)

**Multi-plateforme** :
- `google-perftools` : Profiling CPU
- `gperftools` : Alternative à perf

### Documentation

**Intel** :
- Intel® 64 and IA-32 Architectures Optimization Reference Manual
- Intel® Architecture Instruction Set Extensions Programming Reference
- Intel® Intrinsics Guide

**AMD** :
- Software Optimization Guide for AMD Family Processors
- AMD64 Architecture Programmer's Manual

**ARM** :
- ARM Cortex-A Series Programmer's Guide
- ARM Compiler Optimization Guide

**Livres** :
- "Computer Architecture: A Quantitative Approach" (Hennessy & Patterson)
- "Optimizing Software in C++" (Agner Fog) - Principes applicables à Pascal
- "What Every Programmer Should Know About Memory" (Ulrich Drepper)

### Sites web et blogs

- Agner Fog's optimization resources : https://www.agner.org/optimize/
- Brendan Gregg's blog : http://www.brendangregg.com/
- Intel Developer Zone : https://software.intel.com/
- LWN.net (Linux performance articles)

## Résumé

### Points clés

✅ **Pipeline CPU** : Comprendre comment le CPU exécute les instructions en parallèle

✅ **Branch Prediction** : Écrire du code prévisible (< 5% miss rate)

✅ **Cache** : Localité spatiale et temporelle cruciales

✅ **IPC** : Viser IPC > 2.0 pour code bien optimisé

✅ **Instructions** : Éviter division, sqrt, et autres instructions lentes

✅ **SIMD** : Utiliser quand approprié (voir section 20.3)

✅ **Multi-plateforme** : Code adaptatif selon CPU/OS

✅ **Mesurer toujours** : Profiler avant et après optimisation

### Gains typiques par optimisation

| Optimisation | Gain typique |
|--------------|--------------|
| **Meilleur algorithme** | 10x - 1000x |
| **Éliminer branchements** | 1.2x - 3x |
| **Améliorer localité cache** | 2x - 10x |
| **Loop unrolling** | 1.1x - 1.5x |
| **Remplacer div par mul** | 5x - 30x |
| **SIMD** | 2x - 8x |
| **Alignement mémoire** | 1.1x - 2x |

### Ordre de priorité

1. **Algorithme et structures de données** (impact maximal)
2. **Localité de cache** (souvent le plus important)
3. **Branch prediction** (si beaucoup de branchements)
4. **SIMD et vectorisation** (calcul intensif)
5. **Micro-optimisations** (dernier recours)

### Erreurs à éviter

❌ Optimiser sans mesurer  
❌ Micro-optimiser avant l'algorithme  
❌ Ignorer les différences de plateformes  
❌ Sacrifier la lisibilité sans gain mesurable  
❌ Optimiser du code rarement exécuté

## Conclusion

Les optimisations spécifiques au CPU peuvent transformer un code lent en code rapide, mais elles demandent :
- **Compréhension** de l'architecture CPU
- **Mesure** systématique des performances
- **Validation** des résultats sur différentes plateformes
- **Balance** entre performance et maintenabilité

Commencez toujours par les optimisations de haut niveau (algorithme, structures de données, cache) avant de descendre aux optimisations de bas niveau (instructions spécifiques, assembleur).

Les prochaines sections couvrent :
- **20.5** : Structures de données optimales
- **20.6** : Algorithmes haute performance
- **20.7** : Memory pools et allocateurs custom

L'optimisation est un processus itératif : mesurer → optimiser → valider → répéter !

---

*Note : Ce tutoriel fait partie de la formation "FreePascal/Lazarus - Niveau Développeur Avancé - Edition Multi-plateforme Windows/Ubuntu"*

⏭️ [Structures de données optimales](/20-optimisation-performance/05-structures-donnees-optimales.md)
