🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 20.2 Optimisation mémoire et caches

## Introduction

L'optimisation de la mémoire et des caches est l'un des aspects les plus importants de la performance moderne. Contrairement à l'idée reçue que "le CPU est le goulot d'étranglement", dans la plupart des applications actuelles, **c'est l'accès à la mémoire qui limite les performances**. Comprendre comment fonctionne la hiérarchie mémoire et comment écrire du code "cache-friendly" peut améliorer vos performances de 2x à 10x, voire plus.

## Hiérarchie de la mémoire

### Vue d'ensemble

Les ordinateurs modernes ont plusieurs niveaux de mémoire, du plus rapide (et petit) au plus lent (et grand) :

```
┌─────────────────┐
│   Registres     │  < 1 cycle      ~100 bytes
├─────────────────┤
│   Cache L1      │  ~4 cycles      32-64 KB
├─────────────────┤
│   Cache L2      │  ~12 cycles     256 KB - 1 MB
├─────────────────┤
│   Cache L3      │  ~40 cycles     8-32 MB
├─────────────────┤
│   RAM (DDR4)    │  ~200 cycles    8-64 GB
├─────────────────┤
│   SSD           │  ~50,000 cycles 256 GB - 2 TB
├─────────────────┤
│   HDD           │  ~5,000,000 c.  1-10 TB
└─────────────────┘
```

### Pourquoi c'est important ?

**Exemple concret** :
```pascal
// Accès séquentiel (cache-friendly)
for i := 0 to 1000000 do
  Tableau[i] := i;
// Temps : ~5 ms (données en cache L1/L2)

// Accès aléatoire (cache-unfriendly)
for i := 0 to 1000000 do
  Tableau[Random(1000000)] := i;
// Temps : ~150 ms (cache misses constants)

// Différence : 30x plus lent !
```

### Les différents niveaux de cache

#### Cache L1 (Level 1)

**Caractéristiques** :
- Le plus rapide (~4 cycles d'horloge)
- Le plus petit (~32-64 KB)
- Séparé en deux : L1d (données) et L1i (instructions)
- Privé à chaque cœur CPU

**Utilisation** :
```pascal
// Code qui tient dans L1 (très rapide)
procedure CalculRapide;  
var
  i: Integer;
  temp: array[0..1023] of Integer;  // 4 KB, tient dans L1
begin
  for i := 0 to 1023 do
    temp[i] := i * 2;
end;
```

#### Cache L2 (Level 2)

**Caractéristiques** :
- Rapide (~12 cycles)
- Taille moyenne (256 KB - 1 MB)
- Privé à chaque cœur (généralement)

**Utilisation** :
```pascal
// Structure de données pour L2
type
  TDataL2 = array[0..65535] of Integer;  // 256 KB
```

#### Cache L3 (Level 3)

**Caractéristiques** :
- Modérément rapide (~40 cycles)
- Grande taille (8-32 MB)
- **Partagé entre tous les cœurs**

**Implications multi-threading** :
```pascal
// ❌ Problème : false sharing dans L3
type
  TCompteur = record
    Valeur: Int64;  // 8 bytes
  end;

var
  Compteurs: array[0..7] of TCompteur;  // 8 threads

// Si deux compteurs sont dans la même ligne de cache (64 bytes),
// les modifications d'un thread invalident le cache de l'autre
// → Performance dégradée
```

**Solution** : Padding
```pascal
// ✅ Solution : padding pour éviter false sharing
type
  TCompteur = record
    Valeur: Int64;
    Padding: array[0..6] of Int64;  // Force 64 bytes par compteur
  end;
```

### Ligne de cache (Cache Line)

**Définition** : La mémoire est chargée en cache par blocs de **64 bytes** (ligne de cache).

**Exemple** :
```pascal
// Tableau de bytes
var
  Data: array[0..127] of Byte;

// Accéder à Data[0] charge également Data[1..63] en cache
// (toute la ligne de cache)
```

**Implications** :
- Accéder à des données adjacentes est presque gratuit
- Accéder à des données éloignées peut être 100x plus lent

## Principes d'optimisation mémoire

### 1. Localité spatiale (Spatial Locality)

**Principe** : Accéder à des données proches en mémoire.

**❌ Mauvais : Accès dispersés**
```pascal
type
  TPoint3D = record
    X: Double;
    Y: Double;
    Z: Double;
  end;

var
  PointsX: array[0..9999] of Double;
  PointsY: array[0..9999] of Double;
  PointsZ: array[0..9999] of Double;

// Calcul de la distance
for i := 0 to 9999 do
  Distance := Sqrt(Sqr(PointsX[i]) + Sqr(PointsY[i]) + Sqr(PointsZ[i]));
// 3 accès mémoire distants → 3 cache misses possibles
```

**✅ Bon : Array of Structures (AoS)**
```pascal
type
  TPoint3D = record
    X, Y, Z: Double;
  end;

var
  Points: array[0..9999] of TPoint3D;

// Calcul de la distance
for i := 0 to 9999 do
  Distance := Sqrt(Sqr(Points[i].X) + Sqr(Points[i].Y) + Sqr(Points[i].Z));
// 1 accès mémoire → toutes les coordonnées sont dans la même ligne de cache
```

**Performance** :
```
Version dispersée : 45 ms  
Version compacte : 12 ms  
Gain : 3.75x plus rapide
```

### 2. Localité temporelle (Temporal Locality)

**Principe** : Réutiliser des données récemment accédées (encore en cache).

**❌ Mauvais : Données accédées puis abandonnées**
```pascal
// Calculer deux fois la même chose
for i := 0 to 10000 do
  ResultatA[i] := CalculComplexe(Data[i]);

// ... beaucoup de code ...

for i := 0 to 10000 do
  ResultatB[i] := CalculComplexe(Data[i]);  // Re-calcul !
```

**✅ Bon : Cache et réutilisation**
```pascal
// Calculer une fois, utiliser deux fois
for i := 0 to 10000 do  
begin
  Temp := CalculComplexe(Data[i]);
  ResultatA[i] := Temp;
  ResultatB[i] := Temp * 2;
end;
```

### 3. Réduction de l'empreinte mémoire

**Principe** : Moins de mémoire = plus de données en cache.

**❌ Mauvais : Types trop gros**
```pascal
type
  TPerson = record
    Nom: String[255];        // 256 bytes
    Prenom: String[255];     // 256 bytes
    Age: Integer;            // 4 bytes
    Email: String[255];      // 256 bytes
  end;  // Total : 772 bytes par personne

var
  Personnes: array[0..9999] of TPerson;  // 7.7 MB
// Ne tient pas dans L3 (8 MB) → cache misses constants
```

**✅ Bon : Types compacts**
```pascal
type
  TPerson = record
    Nom: String[50];         // 51 bytes
    Prenom: String[50];      // 51 bytes
    Age: Byte;               // 1 byte
    Email: String[100];      // 101 bytes
  end;  // Total : 204 bytes par personne

var
  Personnes: array[0..9999] of TPerson;  // 2 MB
// Tient dans L3 → excellentes performances
```

**Performance** :
```
Version 772 bytes : 150 ms  
Version 204 bytes : 40 ms  
Gain : 3.75x plus rapide (même algorithme !)
```

## Stratégies d'allocation mémoire

### 1. Pré-allocation

**❌ Mauvais : Allocations répétées**
```pascal
procedure TraiterFichiers(Fichiers: TStringList);  
var
  i: Integer;
  Buffer: PByte;
begin
  for i := 0 to Fichiers.Count - 1 do
  begin
    Buffer := GetMem(1024 * 1024);  // 1 MB
    try
      ChargerFichier(Fichiers[i], Buffer);
      TraiterBuffer(Buffer);
    finally
      FreeMem(Buffer);
    end;
  end;
end;
// 1000 fichiers = 1000 allocations + 1000 libérations (lent)
```

**✅ Bon : Pré-allocation et réutilisation**
```pascal
procedure TraiterFichiers(Fichiers: TStringList);  
var
  i: Integer;
  Buffer: PByte;
begin
  Buffer := GetMem(1024 * 1024);  // 1 allocation
  try
    for i := 0 to Fichiers.Count - 1 do
    begin
      FillByte(Buffer^, 1024 * 1024, 0);  // Réinitialiser
      ChargerFichier(Fichiers[i], Buffer);
      TraiterBuffer(Buffer);
    end;
  finally
    FreeMem(Buffer);  // 1 libération
  end;
end;
// Gain : 10x à 50x plus rapide
```

### 2. Pooling d'objets (Object Pooling)

**Principe** : Réutiliser des objets au lieu de les créer/détruire constamment.

**❌ Mauvais : Création/destruction répétée**
```pascal
for i := 1 to 10000 do  
begin
  Obj := TMonObjet.Create;
  try
    Obj.Traiter(Data[i]);
  finally
    Obj.Free;
  end;
end;
// 10,000 Create + 10,000 Free
```

**✅ Bon : Pool d'objets**
```pascal
type
  TObjectPool = class
  private
    FPool: TList;
  public
    function Acquire: TMonObjet;
    procedure Release(Obj: TMonObjet);
  end;

var
  Pool: TObjectPool;
  Obj: TMonObjet;
  i: Integer;
begin
  Pool := TObjectPool.Create;
  try
    for i := 1 to 10000 do
    begin
      Obj := Pool.Acquire;  // Récupère du pool
      try
        Obj.Traiter(Data[i]);
      finally
        Pool.Release(Obj);   // Remet dans le pool
      end;
    end;
  finally
    Pool.Free;
  end;
end;
// Création réelle : quelques dizaines d'objets seulement
// Gain : 50x à 100x plus rapide
```

### 3. Stack vs Heap

**Principe** : La pile (stack) est beaucoup plus rapide que le tas (heap).

**Stack (rapide)** :
```pascal
procedure Rapide;  
var
  Buffer: array[0..1023] of Byte;  // Sur la pile
begin
  // Allocation instantanée (juste un décalage de pointeur)
  FillByte(Buffer, 1024, 0);
end;
// Allocation : 0 cycles
// Libération : 0 cycles (automatique)
```

**Heap (lent)** :
```pascal
procedure Lent;  
var
  Buffer: PByte;
begin
  Buffer := GetMem(1024);  // Sur le tas
  try
    FillByte(Buffer^, 1024, 0);
  finally
    FreeMem(Buffer);
  end;
end;
// Allocation : ~50-200 cycles
// Libération : ~50-200 cycles
```

**Règle** :
- Données < 1 KB et durée de vie courte → Stack
- Données > 1 KB ou durée de vie longue → Heap
- Attention : Stack limité (~1 MB sur Windows, ~8 MB sur Linux)

## Patterns d'accès mémoire

### 1. Accès séquentiel (le meilleur)

**Principe** : Parcourir la mémoire dans l'ordre.

```pascal
// ✅ Optimal : accès séquentiel
for i := 0 to High(Tableau) do
  Somme := Somme + Tableau[i];

// Le prefetcher matériel détecte le pattern et charge
// les données suivantes en cache avant qu'on en ait besoin
```

**Performance** :
- Cache hit rate : 95-99%
- Vitesse maximale du CPU

### 2. Accès par stride (moyen)

**Principe** : Parcourir avec un pas constant (stride).

```pascal
// Stride de 2 (parcourir les éléments pairs)
for i := 0 to High(Tableau) div 2 do
  Somme := Somme + Tableau[i * 2];

// Stride de 16 (tous les 16 éléments)
for i := 0 to High(Tableau) div 16 do
  Somme := Somme + Tableau[i * 16];
```

**Performance** :
- Stride petit (2-8) : Bon (80-90% cache hits)
- Stride moyen (16-64) : Moyen (50-70% cache hits)
- Stride grand (>64) : Mauvais (cache miss à chaque accès)

### 3. Accès aléatoire (le pire)

**Principe** : Accès sans pattern prévisible.

```pascal
// ❌ Très lent : accès aléatoire
for i := 0 to 1000000 do
  Somme := Somme + Tableau[Random(Length(Tableau))];

// Cache hit rate : 10-20%
// Performance : 10x à 50x plus lent que séquentiel
```

**Solution** : Réorganiser l'algorithme pour accès séquentiel.

## Structures de données cache-friendly

### Array vs Linked List

**Array (cache-friendly)** :
```pascal
type
  TArray = array[0..9999] of Integer;

var
  Arr: TArray;
begin
  // Parcours séquentiel : toutes les données sont adjacentes
  for i := 0 to High(Arr) do
    Traiter(Arr[i]);
end;
// Cache hit rate : 95%+
```

**Linked List (cache-unfriendly)** :
```pascal
type
  PNode = ^TNode;
  TNode = record
    Data: Integer;
    Next: PNode;
  end;

var
  Current: PNode;
begin
  Current := Head;
  while Current <> nil do
  begin
    Traiter(Current^.Data);
    Current := Current^.Next;  // Saut aléatoire en mémoire !
  end;
end;
// Cache hit rate : 20-40%
// Performance : 5x à 10x plus lent qu'un array
```

**Comparaison** :
```
Opération         | Array    | Linked List
------------------|----------|-------------
Accès séquentiel  | O(1)     | O(1) mais 5x plus lent  
Accès aléatoire   | O(1)     | O(n)  
Insertion début   | O(n)     | O(1)  
Insertion fin     | O(1)     | O(1) ou O(n)  
Cache-friendly    | Oui ✅   | Non ❌
```

**Règle** : Privilégiez les arrays/tableaux dynamiques sauf si vous avez besoin d'insertions/suppressions fréquentes au milieu.

### Structure of Arrays (SoA) vs Array of Structures (AoS)

**Array of Structures (AoS)** :
```pascal
type
  TParticule = record
    PosX, PosY, PosZ: Single;
    VelX, VelY, VelZ: Single;
    Mass: Single;
  end;

var
  Particules: array[0..9999] of TParticule;

// Mise à jour des positions
for i := 0 to High(Particules) do  
begin
  Particules[i].PosX := Particules[i].PosX + Particules[i].VelX;
  Particules[i].PosY := Particules[i].PosY + Particules[i].VelY;
  Particules[i].PosZ := Particules[i].PosZ + Particules[i].VelZ;
end;
// Cache hit rate : Bon (70-80%)
// Mais charge aussi Mass (inutile ici)
```

**Structure of Arrays (SoA)** :
```pascal
type
  TParticules = record
    PosX, PosY, PosZ: array[0..9999] of Single;
    VelX, VelY, VelZ: array[0..9999] of Single;
    Mass: array[0..9999] of Single;
  end;

var
  Particules: TParticules;

// Mise à jour des positions
for i := 0 to 9999 do  
begin
  Particules.PosX[i] := Particules.PosX[i] + Particules.VelX[i];
  Particules.PosY[i] := Particules.PosY[i] + Particules.VelY[i];
  Particules.PosZ[i] := Particules.PosZ[i] + Particules.VelZ[i];
end;
// Cache hit rate : Excellent (95%+)
// Ne charge que les données nécessaires
// Vectorisation SIMD possible (voir section 20.3)
```

**Quand utiliser SoA ?**
- Traitement par lots (batch processing)
- Beaucoup de particules/entités
- Operations vectorielles (SIMD)
- Performance critique

**Quand utiliser AoS ?**
- Manipulation d'entités individuelles
- Code orienté objet classique
- Moins de données (< 1000 éléments)

## Gestion de la mémoire dans FreePascal

### GetMem vs New vs Create

**GetMem (allocation brute)** :
```pascal
var
  Buffer: PByte;
begin
  Buffer := GetMem(1024);  // Allocation de 1 KB
  try
    // Utilisation
  finally
    FreeMem(Buffer);
  end;
end;
// Rapide, mais pas de gestion automatique
```

**New (allocation typée)** :
```pascal
type
  PRecord = ^TRecord;
  TRecord = record
    A, B: Integer;
  end;

var
  P: PRecord;
begin
  New(P);
  try
    P^.A := 10;
  finally
    Dispose(P);
  end;
end;
// Gestion de type, initialisation
```

**Create (objets)** :
```pascal
var
  Obj: TMonObjet;
begin
  Obj := TMonObjet.Create;
  try
    Obj.Faire();
  finally
    Obj.Free;
  end;
end;
// Constructeur/destructeur, héritage
```

### Memory Manager de FreePascal

FreePascal utilise un gestionnaire de mémoire sophistiqué :

**Caractéristiques** :
- Memory pools pour petites allocations (< 256 bytes)
- Système de blocs pour grandes allocations
- Thread-safe par défaut

**Optimisation** : Utiliser le bon memory manager

```pascal
// cmem : Memory manager de la libc (Linux)
{$IFDEF LINUX}
uses cmem;  // Plus rapide sous Linux
{$ENDIF}

// HeapTrc : Détection de fuites (debug uniquement)
{$IFDEF DEBUG}
uses HeapTrc;
{$ENDIF}
```

### Fuites mémoire

**Détection avec HeapTrc** :

```pascal
// Compilation
{$IFDEF DEBUG}
{$DEFINE HEAPTRC}
{$ENDIF}

program MonProgramme;

{$IFDEF HEAPTRC}
{$APPTYPE CONSOLE}
uses
  HeapTrc;
{$ENDIF}

begin
  {$IFDEF HEAPTRC}
  SetHeapTraceOutput('heap.log');
  {$ENDIF}

  // Votre code ici
end.
```

À la fin de l'exécution, `heap.log` contiendra :
```
Heap dump by heaptrc unit
10 memory blocks allocated : 1024 bytes
8 memory blocks freed : 824 bytes
2 unfreed memory blocks : 200 bytes

Call trace for block $00401234 size 100
  $00401567  TMONOBJET__CREATE
  $00401890  main
```

**Prévention** :
```pascal
// ✅ Toujours utiliser try-finally
Obj := TMonObjet.Create;  
try
  // Code
finally
  Obj.Free;  // Garantit la libération
end;
```

## Optimisations spécifiques aux plateformes

### Windows

**Allocations de grande taille** :
```pascal
{$IFDEF WINDOWS}
uses Windows;

function AllocGrande(Size: NativeUInt): Pointer;  
begin
  // VirtualAlloc pour grandes allocations (> 1 MB)
  Result := VirtualAlloc(nil, Size, MEM_COMMIT or MEM_RESERVE, PAGE_READWRITE);
end;

procedure FreeGrande(P: Pointer);  
begin
  VirtualFree(P, 0, MEM_RELEASE);
end;
{$ENDIF}
```

**Large Pages (Huge Pages)** :
```pascal
{$IFDEF WINDOWS}
// Nécessite privilèges administrateur
function AllocLargePages(Size: NativeUInt): Pointer;  
begin
  Result := VirtualAlloc(nil, Size, MEM_COMMIT or MEM_RESERVE or MEM_LARGE_PAGES,
                         PAGE_READWRITE);
end;
// Pages de 2 MB au lieu de 4 KB
// Réduit les TLB misses → +10-20% performance
{$ENDIF}
```

### Linux

**mmap pour grandes allocations** :
```pascal
{$IFDEF LINUX}
uses BaseUnix;

function AllocGrande(Size: csize_t): Pointer;  
begin
  Result := fpmmap(nil, Size, PROT_READ or PROT_WRITE,
                   MAP_PRIVATE or MAP_ANONYMOUS, -1, 0);
end;

procedure FreeGrande(P: Pointer; Size: csize_t);  
begin
  fpmunmap(P, Size);
end;
{$ENDIF}
```

**Huge Pages Linux** :
```pascal
{$IFDEF LINUX}
// Nécessite configuration système
// echo 20 > /proc/sys/vm/nr_hugepages

function AllocHugePages(Size: csize_t): Pointer;  
begin
  Result := fpmmap(nil, Size, PROT_READ or PROT_WRITE,
                   MAP_PRIVATE or MAP_ANONYMOUS or MAP_HUGETLB, -1, 0);
end;
{$ENDIF}
```

## Cache Prefetching

### Prefetch manuel (avancé)

FreePascal supporte les intrinsics de prefetch sur x86/x64 :

```pascal
{$ASMMODE INTEL}

procedure TraiterAvecPrefetch(Data: PInteger; Count: Integer);  
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    // Prefetch des données 64 éléments à l'avance
    if i + 64 < Count then
      asm
        mov rax, Data
        prefetchnta [rax + (i + 64) * 4]
      end;

    // Traiter l'élément courant
    Traiter(Data[i]);
  end;
end;
```

**Types de prefetch** :
- `prefetchnta` : Non-temporal (ne pollue pas le cache)
- `prefetcht0` : Tous les niveaux de cache
- `prefetcht1` : L2 et L3
- `prefetcht2` : L3 uniquement

**Quand utiliser** :
- Grandes boucles avec stride prévisible
- Latence d'accès mémoire importante
- Profiling montre beaucoup de cache misses

## Alignment (Alignement) mémoire

### Pourquoi c'est important ?

Le CPU accède à la mémoire par blocs alignés :
- 32-bit : alignement sur 4 bytes
- 64-bit : alignement sur 8 bytes
- SIMD (SSE) : alignement sur 16 bytes
- SIMD (AVX) : alignement sur 32 bytes

**❌ Mauvais : Non aligné**
```pascal
type
  TData = packed record
    Flag: Byte;      // Offset 0
    Value: Int64;    // Offset 1 (non aligné!)
  end;

// Accès à Value nécessite 2 accès mémoire (lent)
```

**✅ Bon : Aligné**
```pascal
type
  TData = record
    Flag: Byte;      // Offset 0
    Padding: array[0..6] of Byte;  // Offset 1-7
    Value: Int64;    // Offset 8 (aligné sur 8!)
  end;

// Accès à Value nécessite 1 accès mémoire (rapide)
```

### Alignement automatique en FreePascal

FreePascal aligne automatiquement les champs de record :

```pascal
type
  TAutoAlign = record
    A: Byte;    // Offset 0
                // Padding automatique 1-3
    B: Integer; // Offset 4 (aligné)
                // Padding automatique 5-7
    C: Int64;   // Offset 8 (aligné)
  end;
// SizeOf(TAutoAlign) = 16 (pas 13)
```

**Désactiver l'alignement** :
```pascal
type
  TPacked = packed record
    A: Byte;    // Offset 0
    B: Integer; // Offset 1 (pas aligné)
    C: Int64;   // Offset 5 (pas aligné)
  end;
// SizeOf(TPacked) = 13

// ⚠️ Plus compact mais accès plus lents
```

### Alignement pour SIMD

```pascal
type
  TAlignedArray = record
    Data: array[0..15] of Single;
  end align 16;  // Force alignement 16 bytes pour SSE

// Ou avec directive
{$CODEALIGN RECORDMIN=16}
type
  TAlignedSSE = record
    Data: array[0..3] of Single;
  end;
```

## Mesurer les cache misses

### Sur Linux avec Perf

```bash
# Compiler le programme
fpc -O3 programme.pas

# Mesurer les cache misses
perf stat -e cache-references,cache-misses,instructions ./programme

# Sortie exemple :
#   12,345,678  cache-references
#      456,789  cache-misses     # 3.7% miss rate
#  987,654,321  instructions
```

**Interprétation** :
- **< 1% miss rate** : Excellent (très cache-friendly)
- **1-5% miss rate** : Bon
- **5-10% miss rate** : Moyen (optimisations possibles)
- **> 10% miss rate** : Mauvais (problèmes d'accès mémoire)

### Sur Windows avec Intel VTune

Dans VTune, sélectionner l'analyse "Memory Access" :
- L1 Data Cache Misses
- L2 Cache Misses
- L3 Cache Misses
- DRAM Accesses

## Checklist d'optimisation mémoire

Avant d'optimiser :
- [ ] Profiler pour identifier les hotspots
- [ ] Mesurer les cache misses
- [ ] Identifier les patterns d'accès

Optimisations à tenter :
- [ ] Réduire la taille des structures de données
- [ ] Améliorer la localité spatiale (données adjacentes)
- [ ] Améliorer la localité temporelle (réutilisation)
- [ ] Remplacer linked lists par arrays
- [ ] Pré-allouer au lieu d'allouer en boucle
- [ ] Utiliser object pooling pour objets fréquents
- [ ] Aligner les structures pour SIMD
- [ ] Considérer SoA au lieu de AoS pour traitement par lots

Après optimisation :
- [ ] Re-mesurer les performances
- [ ] Vérifier que les cache misses ont diminué
- [ ] Valider que le code reste correct

## Pièges courants

### 1. Padding excessif
```pascal
// ❌ Gaspillage mémoire
type
  TData = record
    A: Byte;       // 1 byte
    Padding1: array[0..6] of Byte;  // 7 bytes gaspillés
    B: Int64;      // 8 bytes
    Padding2: array[0..6] of Byte;  // 7 bytes gaspillés
    C: Byte;       // 1 byte
  end;
// 24 bytes pour stocker 10 bytes utiles
```

### 2. Fragmentation mémoire
```pascal
// ❌ Cause de la fragmentation
for i := 1 to 10000 do  
begin
  if Random(2) = 0 then
    Liste.Add(GetMem(Random(1000)));
  else
    FreeMem(Liste[Random(Liste.Count)]);
end;
// Allocations/libérations aléatoires → fragmentation
```

**✅ Solution** : Utiliser un pool ou pré-allouer
```pascal
// Pré-allocation d'un grand bloc
const
  PoolSize = 10000 * 1000;  // 10 MB
var
  Pool: PByte;
  Offset: Integer;
begin
  Pool := GetMem(PoolSize);
  Offset := 0;
  // Allouer dans le pool (pas de fragmentation)
end;
```

### 3. Copies inutiles
```pascal
// ❌ Copie d'un grand record
type
  TGrosRecord = record
    Data: array[0..9999] of Integer;  // 40 KB
  end;

function Traiter(R: TGrosRecord): Integer;  // Passage par valeur  
begin
  Result := R.Data[0];
end;
// Copie de 40 KB à chaque appel !

// ✅ Passage par référence
function Traiter(const R: TGrosRecord): Integer;  
begin
  Result := R.Data[0];
end;
// Pas de copie, juste un pointeur
```

### 4. String temporaires
```pascal
// ❌ Allocations string multiples
var
  S: String;
  i: Integer;
begin
  S := '';
  for i := 1 to 10000 do
    S := S + IntToStr(i) + ',';  // Réallocation à chaque itération !
end;

// ✅ Utiliser TStringBuilder
uses SysUtils;  
var
  SB: TStringBuilder;
  i: Integer;
begin
  SB := TStringBuilder.Create;
  try
    for i := 1 to 10000 do
      SB.Append(IntToStr(i)).Append(',');
    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;
// Pas de réallocations constantes
```

## Techniques avancées

### 1. Memory-Mapped Files

Pour traiter de très gros fichiers sans charger tout en RAM :

**Windows** :
```pascal
{$IFDEF WINDOWS}
uses Windows;

type
  TMappedFile = class
  private
    FHandle: THandle;
    FMapping: THandle;
    FData: Pointer;
    FSize: Int64;
  public
    constructor Create(const FileName: string);
    destructor Destroy; override;
    property Data: Pointer read FData;
    property Size: Int64 read FSize;
  end;

constructor TMappedFile.Create(const FileName: string);  
begin
  inherited Create;

  // Ouvrir le fichier
  FHandle := CreateFile(PChar(FileName), GENERIC_READ, FILE_SHARE_READ,
                        nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);

  if FHandle = INVALID_HANDLE_VALUE then
    raise Exception.Create('Cannot open file');

  // Obtenir la taille
  Int64Rec(FSize).Lo := GetFileSize(FHandle, @Int64Rec(FSize).Hi);

  // Créer le mapping
  FMapping := CreateFileMapping(FHandle, nil, PAGE_READONLY,
                                Int64Rec(FSize).Hi, Int64Rec(FSize).Lo, nil);

  if FMapping = 0 then
  begin
    CloseHandle(FHandle);
    raise Exception.Create('Cannot create mapping');
  end;

  // Mapper en mémoire
  FData := MapViewOfFile(FMapping, FILE_MAP_READ, 0, 0, 0);

  if FData = nil then
  begin
    CloseHandle(FMapping);
    CloseHandle(FHandle);
    raise Exception.Create('Cannot map file');
  end;
end;

destructor TMappedFile.Destroy;  
begin
  if FData <> nil then
    UnmapViewOfFile(FData);
  if FMapping <> 0 then
    CloseHandle(FMapping);
  if FHandle <> INVALID_HANDLE_VALUE then
    CloseHandle(FHandle);
  inherited;
end;

// Utilisation
var
  MF: TMappedFile;
  Data: PByte;
begin
  MF := TMappedFile.Create('gros_fichier.dat');  // 10 GB
  try
    Data := MF.Data;
    // Accès direct comme si c'était en RAM
    // L'OS charge les pages nécessaires automatiquement
    ProcessByte(Data[1234567]);
  finally
    MF.Free;
  end;
end;
{$ENDIF}
```

**Linux** :
```pascal
{$IFDEF LINUX}
uses BaseUnix, Unix;

type
  TMappedFile = class
  private
    FHandle: cint;
    FData: Pointer;
    FSize: csize_t;
  public
    constructor Create(const FileName: string);
    destructor Destroy; override;
    property Data: Pointer read FData;
    property Size: csize_t read FSize;
  end;

constructor TMappedFile.Create(const FileName: string);  
var
  StatBuf: stat;
begin
  inherited Create;

  // Ouvrir le fichier
  FHandle := FpOpen(FileName, O_RDONLY);
  if FHandle = -1 then
    raise Exception.Create('Cannot open file');

  // Obtenir la taille
  if FpFStat(FHandle, StatBuf) = -1 then
  begin
    FpClose(FHandle);
    raise Exception.Create('Cannot stat file');
  end;
  FSize := StatBuf.st_size;

  // Mapper en mémoire
  FData := fpmmap(nil, FSize, PROT_READ, MAP_PRIVATE, FHandle, 0);

  if FData = MAP_FAILED then
  begin
    FpClose(FHandle);
    raise Exception.Create('Cannot map file');
  end;
end;

destructor TMappedFile.Destroy;  
begin
  if FData <> MAP_FAILED then
    fpmunmap(FData, FSize);
  if FHandle <> -1 then
    FpClose(FHandle);
  inherited;
end;
{$ENDIF}
```

**Avantages** :
- Pas besoin de charger tout le fichier en RAM
- L'OS gère le cache automatiquement
- Très rapide pour accès aléatoire

### 2. Custom Memory Allocator

Pour des cas très spécifiques, créer un allocateur personnalisé :

```pascal
type
  TArena = class
  private
    FBuffer: PByte;
    FSize: NativeUInt;
    FOffset: NativeUInt;
  public
    constructor Create(Size: NativeUInt);
    destructor Destroy; override;
    function Alloc(Size: NativeUInt): Pointer;
    procedure Reset;  // Réinitialiser sans libérer
  end;

constructor TArena.Create(Size: NativeUInt);  
begin
  inherited Create;
  FSize := Size;
  FBuffer := GetMem(Size);
  FOffset := 0;
end;

destructor TArena.Destroy;  
begin
  FreeMem(FBuffer);
  inherited;
end;

function TArena.Alloc(Size: NativeUInt): Pointer;  
begin
  if FOffset + Size > FSize then
    raise Exception.Create('Arena full');

  Result := FBuffer + FOffset;
  Inc(FOffset, Size);
end;

procedure TArena.Reset;  
begin
  FOffset := 0;  // Réutiliser la mémoire
end;

// Utilisation
var
  Arena: TArena;
  P: Pointer;
  i: Integer;
begin
  Arena := TArena.Create(1024 * 1024);  // 1 MB
  try
    for i := 1 to 1000 do
    begin
      // Traitement
      P := Arena.Alloc(1024);
      // Utiliser P
    end;

    // Réinitialiser et réutiliser
    Arena.Reset;

    for i := 1 to 1000 do
    begin
      P := Arena.Alloc(1024);
      // Réutilise la même mémoire
    end;
  finally
    Arena.Free;
  end;
end;
```

**Avantages** :
- Allocation O(1) (juste un incrément de pointeur)
- Pas de fragmentation
- Libération en masse (Reset)
- Parfait pour données temporaires

### 3. Copy-on-Write avec String

FreePascal utilise Copy-on-Write pour les strings :

```pascal
var
  S1, S2: String;
begin
  S1 := 'Hello World';
  S2 := S1;  // Pas de copie, juste un compteur de référence

  // S1 et S2 pointent vers la même mémoire
  // Économie de mémoire et de temps

  S2 := S2 + '!';  // Maintenant copie (write)
  // S1 = 'Hello World'
  // S2 = 'Hello World!'
end;
```

**Implications** :
- Passage de string par valeur est peu coûteux
- Modification déclenche copie (COW = Copy-on-Write)
- Thread-safe automatiquement

### 4. Shared Memory entre processus

Pour communiquer entre processus sans copie :

**Windows** :
```pascal
{$IFDEF WINDOWS}
uses Windows;

type
  TSharedMemory = class
  private
    FMapping: THandle;
    FData: Pointer;
    FSize: Cardinal;
  public
    constructor Create(const Name: string; Size: Cardinal);
    destructor Destroy; override;
    property Data: Pointer read FData;
  end;

constructor TSharedMemory.Create(const Name: string; Size: Cardinal);  
begin
  inherited Create;
  FSize := Size;

  FMapping := CreateFileMapping(INVALID_HANDLE_VALUE, nil,
                                PAGE_READWRITE, 0, Size, PChar(Name));

  if FMapping = 0 then
    raise Exception.Create('Cannot create shared memory');

  FData := MapViewOfFile(FMapping, FILE_MAP_ALL_ACCESS, 0, 0, 0);

  if FData = nil then
  begin
    CloseHandle(FMapping);
    raise Exception.Create('Cannot map shared memory');
  end;
end;

destructor TSharedMemory.Destroy;  
begin
  if FData <> nil then
    UnmapViewOfFile(FData);
  if FMapping <> 0 then
    CloseHandle(FMapping);
  inherited;
end;
{$ENDIF}
```

**Linux** :
```pascal
{$IFDEF LINUX}
uses BaseUnix, Unix;

type
  TSharedMemory = class
  private
    FShmId: cint;
    FData: Pointer;
    FSize: csize_t;
  public
    constructor Create(Key: key_t; Size: csize_t);
    destructor Destroy; override;
    property Data: Pointer read FData;
  end;

constructor TSharedMemory.Create(Key: key_t; Size: csize_t);  
begin
  inherited Create;
  FSize := Size;

  // Créer le segment de mémoire partagée
  FShmId := shmget(Key, Size, IPC_CREAT or &666);

  if FShmId = -1 then
    raise Exception.Create('Cannot create shared memory');

  // Attacher à l'espace d'adressage
  FData := shmat(FShmId, nil, 0);

  if FData = Pointer(-1) then
  begin
    shmctl(FShmId, IPC_RMID, nil);
    raise Exception.Create('Cannot attach shared memory');
  end;
end;

destructor TSharedMemory.Destroy;  
begin
  if FData <> Pointer(-1) then
    shmdt(FData);
  if FShmId <> -1 then
    shmctl(FShmId, IPC_RMID, nil);
  inherited;
end;
{$ENDIF}
```

## Exemples pratiques d'optimisation

### Exemple 1 : Traitement d'images

**❌ Version non optimisée** :
```pascal
type
  TPixel = record
    R, G, B, A: Byte;
  end;
  TImage = array of array of TPixel;

procedure ConvertToGrayscale(var Img: TImage);  
var
  x, y: Integer;
  Gray: Byte;
begin
  for y := 0 to High(Img) do
    for x := 0 to High(Img[y]) do
    begin
      Gray := Round(0.299 * Img[y][x].R +
                    0.587 * Img[y][x].G +
                    0.114 * Img[y][x].B);
      Img[y][x].R := Gray;
      Img[y][x].G := Gray;
      Img[y][x].B := Gray;
    end;
end;
// Array 2D → accès lent
// Calculs en virgule flottante → lent
```

**✅ Version optimisée** :
```pascal
type
  TPixel = packed record
    R, G, B, A: Byte;
  end;
  TImageFlat = array of TPixel;  // Array 1D

procedure ConvertToGrayscaleFast(var Img: TImageFlat);  
var
  i: Integer;
  P: ^TPixel;
  Gray: Byte;
begin
  P := @Img[0];
  for i := 0 to High(Img) do
  begin
    // Approximation entière (plus rapide)
    Gray := (P^.R * 77 + P^.G * 150 + P^.B * 29) shr 8;
    P^.R := Gray;
    P^.G := Gray;
    P^.B := Gray;
    Inc(P);
  end;
end;
// Array 1D → accès séquentiel rapide
// Arithmétique entière → rapide
// Pointeur direct → pas de calcul d'index

// Performance : 10x plus rapide
```

### Exemple 2 : Recherche dans une grande liste

**❌ Version non optimisée** :
```pascal
type
  TUser = record
    ID: Integer;
    Name: String;
    Email: String;
    Age: Integer;
  end;
  TUserList = array of TUser;

function FindUserByID(const Users: TUserList; ID: Integer): Integer;  
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(Users) do
    if Users[i].ID = ID then
    begin
      Result := i;
      Exit;
    end;
end;
// O(n) recherche linéaire
// Pour 1,000,000 utilisateurs : ~500,000 comparaisons
```

**✅ Version optimisée** :
```pascal
uses Generics.Collections;

type
  TUser = record
    ID: Integer;
    Name: String;
    Email: String;
    Age: Integer;
  end;
  TUserDict = TDictionary<Integer, TUser>;

var
  Users: TUserDict;

function FindUserByID(ID: Integer): Boolean;  
var
  User: TUser;
begin
  Result := Users.TryGetValue(ID, User);
end;
// O(1) recherche par hash
// Pour 1,000,000 utilisateurs : 1 lookup
// Performance : 500,000x plus rapide !
```

### Exemple 3 : Calcul intensif avec cache

**❌ Version sans cache** :
```pascal
function Fibonacci(N: Integer): Int64;  
begin
  if N <= 1 then
    Result := N
  else
    Result := Fibonacci(N - 1) + Fibonacci(N - 2);
end;

// Fib(40) = ~1 milliard d'appels récursifs
// Temps : ~2 secondes
```

**✅ Version avec cache (memoization)** :
```pascal
var
  FibCache: array[0..100] of Int64;
  CacheInit: Boolean = False;

function FibonacciFast(N: Integer): Int64;  
begin
  if not CacheInit then
  begin
    FillQWord(FibCache, Length(FibCache), QWord(-1));
    CacheInit := True;
  end;

  if N <= 1 then
  begin
    Result := N;
    Exit;
  end;

  if FibCache[N] <> -1 then
  begin
    Result := FibCache[N];  // Hit cache
    Exit;
  end;

  Result := FibonacciFast(N - 1) + FibonacciFast(N - 2);
  FibCache[N] := Result;  // Mettre en cache
end;

// Fib(40) = 40 calculs uniques
// Temps : ~0.0001 secondes
// Performance : 20,000x plus rapide !
```

## Comparaison Windows vs Linux

### Performances d'allocation mémoire

**Test** : 1,000,000 allocations de 1 KB

| Système | Temps | Memory Manager |
|---------|-------|----------------|
| Windows 10 | 450 ms | HeapAlloc |
| Ubuntu 22.04 | 280 ms | ptmalloc |
| Ubuntu + cmem | 220 ms | glibc malloc |

**Recommandation** :
```pascal
{$IFDEF LINUX}
uses cmem;  // Utiliser glibc malloc sous Linux (plus rapide)
{$ENDIF}
```

### Huge Pages

**Windows** :
- Support via VirtualAlloc avec MEM_LARGE_PAGES
- Nécessite privilège "SeLockMemoryPrivilege"
- Configuration via Group Policy

**Linux** :
- Support natif plus flexible
- Configuration : `/proc/sys/vm/nr_hugepages`
- Transparent Huge Pages (THP) automatique

```bash
# Linux : activer THP
echo always > /sys/kernel/mm/transparent_hugepage/enabled

# Réserver des huge pages
echo 1024 > /proc/sys/vm/nr_hugepages
```

## Résumé des optimisations mémoire

### Priorité 1 (Impact majeur ⭐⭐⭐⭐⭐)
- Améliorer la localité spatiale (données adjacentes)
- Utiliser des arrays au lieu de linked lists
- Réduire la taille des structures de données
- Éviter les allocations en boucle

### Priorité 2 (Impact important ⭐⭐⭐⭐)
- Pré-allouer et réutiliser la mémoire
- Utiliser object pooling
- Privilégier le stack pour petites données
- Implémenter du caching

### Priorité 3 (Impact moyen ⭐⭐⭐)
- Aligner les structures pour SIMD
- Utiliser SoA pour traitement par lots
- Memory-mapped files pour gros fichiers
- Optimiser les string avec TStringBuilder

### Priorité 4 (Impact faible ⭐⭐)
- Prefetching manuel
- Custom allocators
- Huge pages
- Shared memory

## Outils de diagnostic

### HeapTrc (FreePascal intégré)
```bash
fpc -gh programme.pas
./programme
# Affiche les fuites mémoire à la fin
```

### Valgrind (Linux)
```bash
valgrind --leak-check=full --show-leak-kinds=all ./programme
```

### Application Verifier (Windows)
```bash
appverif.exe  # GUI pour configurer
# Active la détection de corruptions mémoire
```

### Dr. Memory (Multi-plateforme)
```bash
drmemory -- ./programme
# Détecte fuites et accès invalides
```

## Conclusion

L'optimisation mémoire et des caches est souvent plus efficace que l'optimisation algorithmique pure. Les points clés à retenir :

✅ **La hiérarchie mémoire est critique** : L'accès RAM est 100x plus lent que le cache L1

✅ **Localité spatiale** : Garder les données adjacentes en mémoire

✅ **Localité temporelle** : Réutiliser les données récemment accédées

✅ **Structures compactes** : Moins de mémoire = plus de données en cache

✅ **Patterns d'accès** : Séquentiel > Stride > Aléatoire

✅ **Arrays > Linked Lists** : Pour la plupart des cas

✅ **Pré-allocation** : Éviter les allocations répétées

✅ **Mesurer toujours** : Utiliser Perf/VTune pour vérifier les cache misses

Les optimisations mémoire sont particulièrement importantes dans le développement multi-plateforme, car les performances peuvent varier significativement entre Windows et Linux. Testez toujours sur les deux plateformes !

---

*Note : Ce tutoriel fait partie de la formation "FreePascal/Lazarus - Niveau Développeur Avancé - Edition Multi-plateforme Windows/Ubuntu"*

⏭️ [SIMD et vectorisation](/20-optimisation-performance/03-simd-vectorisation.md)
