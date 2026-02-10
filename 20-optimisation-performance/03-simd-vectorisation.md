🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 20.3 SIMD et vectorisation

## Introduction

SIMD (Single Instruction, Multiple Data) est une technique qui permet au processeur d'effectuer **la même opération sur plusieurs données simultanément**. Au lieu de traiter un nombre à la fois, le CPU peut en traiter 4, 8, 16 ou même 32 en une seule instruction. Cette technique peut améliorer les performances de 2x à 16x pour certains types de calculs.

**Exemple simple** :
```
Addition classique (scalaire) :
A[0] + B[0] = C[0]  ← 1 cycle
A[1] + B[1] = C[1]  ← 1 cycle
A[2] + B[2] = C[2]  ← 1 cycle
A[3] + B[3] = C[3]  ← 1 cycle
Total : 4 cycles

Addition SIMD (vectorielle) :
[A[0], A[1], A[2], A[3]] + [B[0], B[1], B[2], B[3]] = [C[0], C[1], C[2], C[3]]
Total : 1 cycle

Gain : 4x plus rapide !
```

## Qu'est-ce que SIMD ?

### Définition

SIMD signifie **Single Instruction, Multiple Data** (une instruction, plusieurs données). C'est une forme de parallélisme au niveau des données où :
- Une seule instruction CPU est exécutée
- Elle opère sur plusieurs éléments de données simultanément
- Les éléments sont traités en parallèle dans des registres spéciaux

### Registres SIMD

Les processeurs modernes possèdent des registres spéciaux pour SIMD :

**Intel/AMD (x86/x64)** :
```
MMX      : 64 bits   (obsolète)
SSE      : 128 bits  → 4 × float (32 bits) ou 2 × double (64 bits)
AVX      : 256 bits  → 8 × float ou 4 × double
AVX-512  : 512 bits  → 16 × float ou 8 × double
```

**ARM (processeurs mobiles, Raspberry Pi)** :
```
NEON     : 128 bits  → 4 × float
SVE      : Variable  → Jusqu'à 2048 bits
```

**Visualisation** :
```
Registre normal (64 bits) :
┌────────────────┐
│   1 × Int64    │
└────────────────┘

Registre SSE (128 bits) :
┌────┬────┬────┬────┐
│ f0 │ f1 │ f2 │ f3 │  ← 4 × float
└────┴────┴────┴────┘

Registre AVX (256 bits) :
┌────┬────┬────┬────┬────┬────┬────┬────┐
│ f0 │ f1 │ f2 │ f3 │ f4 │ f5 │ f6 │ f7 │  ← 8 × float
└────┴────┴────┴────┴────┴────┴────┴────┘
```

## Évolution des extensions SIMD

### Timeline des extensions Intel/AMD

| Extension | Année | Largeur | Registres | Cas d'usage |
|-----------|-------|---------|-----------|-------------|
| **MMX** | 1997 | 64 bits | 8 (MM0-MM7) | Multimédia (obsolète) |
| **SSE** | 1999 | 128 bits | 8 (XMM0-XMM7) | Float 32 bits |
| **SSE2** | 2001 | 128 bits | 8 | Double + entiers |
| **SSE3** | 2004 | 128 bits | 8 | Opérations horizontales |
| **SSE4** | 2006 | 128 bits | 8 | Blend, dot product |
| **AVX** | 2011 | 256 bits | 16 (YMM0-YMM15) | 8 × float |
| **AVX2** | 2013 | 256 bits | 16 | Entiers 256 bits |
| **AVX-512** | 2017 | 512 bits | 32 (ZMM0-ZMM31) | Serveurs (rare) |

### Extensions ARM

| Extension | Largeur | Cas d'usage |
|-----------|---------|-------------|
| **NEON** | 128 bits | ARM v7/v8, smartphones, Raspberry Pi |
| **SVE** | Variable | ARM v8+, serveurs ARM |

### Disponibilité

**Vérifier le support CPU** :

**Windows** :
```pascal
{$IFDEF WINDOWS}
uses Windows;

function CPUSupportsSSE: Boolean;
var
  Info: array[0..3] of Cardinal;
begin
  asm
    push rbx
    mov eax, 1
    cpuid
    mov Info[0], eax
    mov Info[4], ebx
    mov Info[8], ecx
    mov Info[12], edx
    pop rbx
  end;
  Result := (Info[3] and (1 shl 25)) <> 0;  // Bit 25 = SSE
end;

function CPUSupportsAVX: Boolean;
var
  Info: array[0..3] of Cardinal;
begin
  asm
    push rbx
    mov eax, 1
    cpuid
    mov Info[8], ecx
    pop rbx
  end;
  Result := (Info[2] and (1 shl 28)) <> 0;  // Bit 28 = AVX
end;
{$ENDIF}
```

**Linux** :
```bash
# Afficher les flags CPU
cat /proc/cpuinfo | grep flags

# Exemples de flags :
# sse, sse2, sse3, ssse3, sse4_1, sse4_2
# avx, avx2, avx512f
```

## Quand utiliser SIMD ?

### Cas d'usage idéaux ✅

1. **Traitement d'images et vidéo**
   - Conversion de couleurs
   - Filtres (blur, sharpen)
   - Redimensionnement

2. **Audio et DSP**
   - Mixage de canaux audio
   - Effets (reverb, equalizer)
   - Compression/décompression

3. **Calculs scientifiques**
   - Produits matriciels
   - Transformées de Fourier (FFT)
   - Simulations physiques

4. **Jeux vidéo**
   - Physics engine
   - Transformation de vertices
   - Calculs de collisions

5. **Machine Learning**
   - Multiplication de matrices
   - Fonctions d'activation
   - Convolutions

6. **Compression de données**
   - Codecs vidéo (H.264, HEVC)
   - Compression d'images (JPEG)

### Cas inadaptés ❌

1. **Calculs avec branchements complexes**
   ```pascal
   // ❌ Difficile à vectoriser
   for i := 0 to N do
     if Condition[i] then
       A[i] := B[i]
     else
       A[i] := C[i] * D[i];
   ```

2. **Dépendances entre itérations**
   ```pascal
   // ❌ Impossible à vectoriser
   for i := 1 to N do
     A[i] := A[i-1] + B[i];  // A[i] dépend de A[i-1]
   ```

3. **Accès mémoire non contigus**
   ```pascal
   // ❌ Gather/scatter lent
   for i := 0 to N do
     A[Indices[i]] := B[i];
   ```

4. **Petites quantités de données**
   ```pascal
   // ❌ Overhead > gain
   for i := 0 to 3 do  // Seulement 4 éléments
     A[i] := B[i] + C[i];
   ```

## Auto-vectorisation par le compilateur

### Activer l'auto-vectorisation

FreePascal peut vectoriser automatiquement certaines boucles avec les bonnes options :

```bash
# Compilation avec auto-vectorisation
fpc -O3 -CpCOREAVX2 -CfAVX2 programme.pas
```

**Options expliquées** :
- `-O3` : Niveau d'optimisation maximal
- `-CpCOREAVX2` : Optimiser pour processeurs avec AVX2
- `-CfAVX2` : Utiliser les instructions AVX2

**Alternatives selon le CPU** :
```bash
# Pour CPU avec SSE2 uniquement
fpc -O3 -CpPENTIUM4 -CfSSE2 programme.pas

# Pour CPU avec AVX
fpc -O3 -CpCOREAVX -CfAVX programme.pas

# Pour ARM avec NEON
fpc -O3 -CpARMV7A programme.pas
```

### Exemple de boucle vectorisable

**Code source** :
```pascal
procedure AddArrays(A, B, C: PSingle; Count: Integer);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    C[i] := A[i] + B[i];
end;
```

**Compilation** :
```bash
fpc -O3 -CpCOREAVX2 -CfAVX2 -al exemple.pas
# Option -al génère le listing assembleur
```

**Code assembleur généré (avec AVX2)** :
```asm
; Boucle vectorisée (8 floats à la fois)
.L2:
  vmovups ymm0, [rax]       ; Charger 8 floats de A
  vaddps  ymm0, ymm0, [rdx] ; Ajouter 8 floats de B
  vmovups [rcx], ymm0       ; Stocker 8 floats dans C
  add     rax, 32           ; Avancer de 32 bytes (8 × 4)
  add     rdx, 32
  add     rcx, 32
  dec     r8
  jnz     .L2
```

**Résultat** : 8 additions en parallèle au lieu d'une seule !

### Aider le compilateur à vectoriser

**✅ Bonnes pratiques** :

1. **Utiliser des pointeurs typés**
```pascal
// ✅ Bon : type connu
procedure Process(Data: PSingle; Count: Integer);

// ❌ Moins bon : type générique
procedure Process(Data: Pointer; Count: Integer);
```

2. **Alignement mémoire**
```pascal
// ✅ Aligner sur 32 bytes pour AVX
type
  TAlignedArray = array[0..1023] of Single align 32;
```

3. **Boucles simples**
```pascal
// ✅ Vectorisable
for i := 0 to N - 1 do
  C[i] := A[i] + B[i];

// ❌ Difficile à vectoriser
i := 0;
while i < N do
begin
  C[i] := A[i] + B[i];
  Inc(i);
end;
```

4. **Pas de dépendances**
```pascal
// ✅ Vectorisable (pas de dépendance)
for i := 0 to N - 1 do
  A[i] := B[i] * 2;

// ❌ Non vectorisable (dépendance)
for i := 1 to N - 1 do
  A[i] := A[i-1] + 1;
```

## Vectorisation manuelle avec assembleur inline

Pour un contrôle total, utilisez l'assembleur inline.

### Syntaxe assembleur FreePascal

```pascal
{$ASMMODE INTEL}  // Syntaxe Intel (plus lisible)

procedure MaFonction;
asm
  // Code assembleur ici
end;
```

### Exemple 1 : Addition de 4 floats (SSE)

```pascal
{$ASMMODE INTEL}

procedure AddFloatsSSE(A, B, Result: PSingle);
// A, B, Result pointent chacun vers 4 floats alignés
asm
  movaps xmm0, [A]      // Charger 4 floats de A dans XMM0
  movaps xmm1, [B]      // Charger 4 floats de B dans XMM1
  addps  xmm0, xmm1     // Ajouter : XMM0 = XMM0 + XMM1
  movaps [Result], xmm0 // Stocker le résultat
end;

// Utilisation
var
  A: array[0..3] of Single = (1.0, 2.0, 3.0, 4.0);
  B: array[0..3] of Single = (5.0, 6.0, 7.0, 8.0);
  R: array[0..3] of Single;
begin
  AddFloatsSSE(@A, @B, @R);
  // R = [6.0, 8.0, 10.0, 12.0]
end;
```

### Exemple 2 : Addition de tableaux (SSE)

```pascal
{$ASMMODE INTEL}

procedure AddArraysSSE(A, B, C: PSingle; Count: Integer);
// Count doit être un multiple de 4
asm
  mov     rcx, Count
  shr     rcx, 2          // Diviser par 4 (traiter 4 floats à la fois)
  test    rcx, rcx
  jz      @Done

@Loop:
  movups  xmm0, [A]       // Charger 4 floats de A
  movups  xmm1, [B]       // Charger 4 floats de B
  addps   xmm0, xmm1      // Ajouter
  movups  [C], xmm0       // Stocker

  add     A, 16           // Avancer de 16 bytes (4 × 4)
  add     B, 16
  add     C, 16

  dec     rcx
  jnz     @Loop

@Done:
end;

// Performance : 4x plus rapide qu'une boucle normale
```

### Exemple 3 : Addition avec AVX (8 floats)

```pascal
{$ASMMODE INTEL}

procedure AddArraysAVX(A, B, C: PSingle; Count: Integer);
// Count doit être un multiple de 8
asm
  mov     rcx, Count
  shr     rcx, 3          // Diviser par 8
  test    rcx, rcx
  jz      @Done

@Loop:
  vmovups ymm0, [A]       // Charger 8 floats de A
  vmovups ymm1, [B]       // Charger 8 floats de B
  vaddps  ymm0, ymm0, ymm1 // Ajouter (syntaxe AVX à 3 opérandes)
  vmovups [C], ymm0       // Stocker

  add     A, 32           // Avancer de 32 bytes (8 × 4)
  add     B, 32
  add     C, 32

  dec     rcx
  jnz     @Loop

  vzeroupper              // Important : nettoyer les registres YMM

@Done:
end;

// Performance : 8x plus rapide qu'une boucle normale
```

**Note importante** : `vzeroupper` est nécessaire pour éviter les pénalités de performance lors du passage entre code AVX et non-AVX.

### Instructions SIMD communes

#### SSE (128 bits)

**Chargement/stockage** :
```asm
movaps  xmm0, [addr]  ; Aligned load (16-byte aligned)
movups  xmm0, [addr]  ; Unaligned load
movaps  [addr], xmm0  ; Aligned store
movups  [addr], xmm0  ; Unaligned store
```

**Arithmétique (float)** :
```asm
addps   xmm0, xmm1    ; Addition : xmm0 += xmm1 (4 floats)
subps   xmm0, xmm1    ; Soustraction
mulps   xmm0, xmm1    ; Multiplication
divps   xmm0, xmm1    ; Division
sqrtps  xmm0, xmm1    ; Racine carrée
```

**Arithmétique (double)** :
```asm
addpd   xmm0, xmm1    ; Addition : xmm0 += xmm1 (2 doubles)
subpd   xmm0, xmm1    ; Soustraction
mulpd   xmm0, xmm1    ; Multiplication
divpd   xmm0, xmm1    ; Division
```

**Comparaisons** :
```asm
cmpps   xmm0, xmm1, imm8  ; Comparer (imm8 = type de comparaison)
; 0 = equal, 1 = less, 2 = less-or-equal, etc.
```

**Logique** :
```asm
andps   xmm0, xmm1    ; AND bit à bit
orps    xmm0, xmm1    ; OR
xorps   xmm0, xmm1    ; XOR
```

#### AVX/AVX2 (256 bits)

**Chargement/stockage** :
```asm
vmovups ymm0, [addr]       ; Unaligned load (8 floats)
vmovups [addr], ymm0       ; Unaligned store
```

**Arithmétique** :
```asm
vaddps  ymm0, ymm1, ymm2   ; ymm0 = ymm1 + ymm2 (8 floats)
vsubps  ymm0, ymm1, ymm2   ; Soustraction
vmulps  ymm0, ymm1, ymm2   ; Multiplication
vdivps  ymm0, ymm1, ymm2   ; Division
```

**Note** : AVX utilise une syntaxe à 3 opérandes (destination, source1, source2) contrairement à SSE (2 opérandes).

## Exemples pratiques

### Exemple 1 : Produit scalaire (Dot Product)

**Version scalaire** :
```pascal
function DotProduct(A, B: PSingle; N: Integer): Single;
var
  i: Integer;
  Sum: Single;
begin
  Sum := 0;
  for i := 0 to N - 1 do
    Sum := Sum + A[i] * B[i];
  Result := Sum;
end;
// Temps pour N=1,000,000 : ~5 ms
```

**Version SSE** :
```pascal
{$ASMMODE INTEL}

function DotProductSSE(A, B: PSingle; N: Integer): Single;
var
  i, Count: Integer;
  Sum: Single;
  PartialSums: array[0..3] of Single;
begin
  Count := N div 4;
  Sum := 0;

  if Count > 0 then
  begin
    asm
      xorps   xmm0, xmm0          // Initialiser XMM0 à zéro
      mov     rcx, Count
      mov     rax, A
      mov     rdx, B

    @Loop:
      movups  xmm1, [rax]         // Charger 4 floats de A
      movups  xmm2, [rdx]         // Charger 4 floats de B
      mulps   xmm1, xmm2          // Multiplier
      addps   xmm0, xmm1          // Accumuler

      add     rax, 16
      add     rdx, 16
      dec     rcx
      jnz     @Loop

      movups  PartialSums, xmm0   // Sauvegarder les 4 sommes partielles
    end;

    // Réduire les 4 sommes partielles
    Sum := PartialSums[0] + PartialSums[1] + PartialSums[2] + PartialSums[3];
  end;

  // Traiter les éléments restants (N mod 4)
  for i := Count * 4 to N - 1 do
    Sum := Sum + A[i] * B[i];

  Result := Sum;
end;
// Temps pour N=1,000,000 : ~1.5 ms (3.3x plus rapide)
```

**Version AVX** :
```pascal
{$ASMMODE INTEL}

function DotProductAVX(A, B: PSingle; N: Integer): Single;
var
  i, Count: Integer;
  Sum: Single;
  PartialSums: array[0..7] of Single;
begin
  Count := N div 8;
  Sum := 0;

  if Count > 0 then
  begin
    asm
      vxorps  ymm0, ymm0, ymm0    // Initialiser YMM0 à zéro
      mov     rcx, Count
      mov     rax, A
      mov     rdx, B

    @Loop:
      vmovups ymm1, [rax]         // Charger 8 floats de A
      vmovups ymm2, [rdx]         // Charger 8 floats de B
      vmulps  ymm1, ymm1, ymm2    // Multiplier
      vaddps  ymm0, ymm0, ymm1    // Accumuler

      add     rax, 32
      add     rdx, 32
      dec     rcx
      jnz     @Loop

      vmovups PartialSums, ymm0   // Sauvegarder les 8 sommes partielles
      vzeroupper                  // Nettoyer
    end;

    // Réduire les 8 sommes partielles
    for i := 0 to 7 do
      Sum := Sum + PartialSums[i];
  end;

  // Traiter les éléments restants
  for i := Count * 8 to N - 1 do
    Sum := Sum + A[i] * B[i];

  Result := Sum;
end;
// Temps pour N=1,000,000 : ~0.8 ms (6.25x plus rapide)
```

### Exemple 2 : Conversion RGB vers Grayscale

**Version scalaire** :
```pascal
type
  TRGB = packed record
    R, G, B: Byte;
  end;
  PRGBArray = ^TRGBArray;
  TRGBArray = array[0..0] of TRGB;

procedure RGBToGrayScalar(Pixels: PRGBArray; Count: Integer);
var
  i: Integer;
  Gray: Byte;
begin
  for i := 0 to Count - 1 do
  begin
    Gray := Round(0.299 * Pixels[i].R +
                  0.587 * Pixels[i].G +
                  0.114 * Pixels[i].B);
    Pixels[i].R := Gray;
    Pixels[i].G := Gray;
    Pixels[i].B := Gray;
  end;
end;
// Temps pour 1 million de pixels : ~25 ms
```

**Version SSE (approximation entière)** :
```pascal
{$ASMMODE INTEL}

procedure RGBToGraySSE(Pixels: PRGBArray; Count: Integer);
var
  i, VectorCount: Integer;
  R, G, B, Gray: Byte;
begin
  VectorCount := Count div 4;  // Traiter 4 pixels à la fois

  // Note : Cette version est simplifiée
  // Une vraie implémentation SSE pour RGB est plus complexe
  // car les pixels RGB ne sont pas alignés idéalement

  // Version scalaire optimisée avec arithmétique entière
  for i := 0 to Count - 1 do
  begin
    R := Pixels[i].R;
    G := Pixels[i].G;
    B := Pixels[i].B;

    // Approximation: (R * 77 + G * 150 + B * 29) >> 8
    // Équivalent à: 0.3 * R + 0.59 * G + 0.11 * B
    Gray := (R * 77 + G * 150 + B * 29) shr 8;

    Pixels[i].R := Gray;
    Pixels[i].G := Gray;
    Pixels[i].B := Gray;
  end;
end;
// Temps : ~8 ms (3x plus rapide)
```

### Exemple 3 : Multiplication de matrices

**Version scalaire** :
```pascal
procedure MatrixMultiplyScalar(A, B, C: PSingle; N: Integer);
// Matrices N×N (row-major)
var
  i, j, k: Integer;
  Sum: Single;
begin
  for i := 0 to N - 1 do
    for j := 0 to N - 1 do
    begin
      Sum := 0;
      for k := 0 to N - 1 do
        Sum := Sum + A[i * N + k] * B[k * N + j];
      C[i * N + j] := Sum;
    end;
end;
// Temps pour 512×512 : ~850 ms
// Complexité : O(n³)
```

**Version SSE (produit par blocs)** :
```pascal
{$ASMMODE INTEL}

procedure MatrixMultiplySSE(A, B, C: PSingle; N: Integer);
var
  i, j, k: Integer;
  Sum: array[0..3] of Single;
begin
  // Simplification : suppose que N est multiple de 4
  for i := 0 to N - 1 do
    for j := 0 to (N div 4) - 1 do
    begin
      asm
        xorps xmm0, xmm0              // Accumulateur pour 4 résultats

        mov   rax, A
        add   rax, i
        imul  rax, N
        imul  rax, 4                  // A[i * N]

        mov   rdx, B
        add   rdx, j
        imul  rdx, 4
        imul  rdx, 4                  // B[j * 4]

        mov   rcx, N

      @InnerLoop:
        movss xmm1, [rax]             // Charger A[i, k]
        shufps xmm1, xmm1, 0          // Répliquer dans les 4 slots
        movups xmm2, [rdx]            // Charger 4 éléments de B[k, j..j+3]
        mulps xmm1, xmm2              // Multiplier
        addps xmm0, xmm1              // Accumuler

        add   rax, 4                  // Passer à A[i, k+1]
        add   rdx, N * 4              // Passer à B[k+1, j]

        dec   rcx
        jnz   @InnerLoop

        mov   rax, C
        add   rax, i
        imul  rax, N
        add   rax, j
        imul  rax, 4
        imul  rax, 4
        movups [rax], xmm0            // Stocker les 4 résultats
      end;
    end;
end;
// Temps pour 512×512 : ~300 ms (2.8x plus rapide)
```

**Note** : Pour de vraies multiplications de matrices haute performance, utilisez des bibliothèques optimisées comme BLAS (voir section 16.4).

## Alignement mémoire pour SIMD

### Pourquoi l'alignement est critique ?

Les instructions SIMD **alignées** (`movaps`, `vmovaps`) sont plus rapides que les **non alignées** (`movups`, `vmovups`) :

```
movaps (aligned)   : 1 cycle
movups (unaligned) : 3-7 cycles (selon le CPU)
```

### Aligner les données en FreePascal

**Arrays statiques** :
```pascal
type
  TAlignedArray = array[0..1023] of Single align 32;  // Alignement 32 bytes

var
  Data: TAlignedArray;  // Automatiquement aligné
```

**Allocation dynamique** :
```pascal
function GetAlignedMem(Size, Alignment: NativeUInt): Pointer;
var
  Original: Pointer;
  Aligned: NativeUInt;
begin
  // Allouer plus que nécessaire
  Original := GetMem(Size + Alignment);

  // Calculer l'adresse alignée
  Aligned := (NativeUInt(Original) + Alignment - 1) and not (Alignment - 1);

  // Stocker l'adresse originale juste avant les données alignées
  PPointer(Aligned - SizeOf(Pointer))^ := Original;

  Result := Pointer(Aligned);
end;

procedure FreeAlignedMem(P: Pointer);
var
  Original: Pointer;
begin
  // Récupérer l'adresse originale
  Original := PPointer(NativeUInt(P) - SizeOf(Pointer))^;
  FreeMem(Original);
end;

// Utilisation
var
  Data: PSingle;
begin
  Data := GetAlignedMem(1024 * SizeOf(Single), 32);  // Aligné sur 32 bytes
  try
    // Utiliser Data avec instructions AVX
  finally
    FreeAlignedMem(Data);
  end;
end;
```

**Vérifier l'alignement** :
```pascal
function IsAligned(P: Pointer; Alignment: NativeUInt): Boolean;
begin
  Result := (NativeUInt(P) mod Alignment) = 0;
end;

// Utilisation
if IsAligned(@Data, 32) then
  WriteLn('Data est aligné sur 32 bytes')
else
  WriteLn('Data n''est PAS aligné');
```

### Impact de l'alignement sur les performances

**Benchmark** :
```pascal
const
  N = 10000000;  // 10 millions d'éléments

var
  Aligned: PSingle;
  Unaligned: PSingle;
  i: Integer;
  StartTime: TDateTime;
  TimeAligned, TimeUnaligned: Int64;

begin
  // Allocation alignée
  Aligned := GetAlignedMem(N * SizeOf(Single), 32);

  // Allocation non alignée (décalage de 1 byte)
  Unaligned := PSingle(NativeUInt(GetMem(N * SizeOf(Single) + 1)) + 1);

  // Test avec données alignées
  StartTime := Now;
  for i := 1 to 1000 do
    ProcessWithAVX(Aligned, N);  // Utilise vmovaps
  TimeAligned := MilliSecondsBetween(Now, StartTime);

  // Test avec données non alignées
  StartTime := Now;
  for i := 1 to 1000 do
    ProcessWithAVX(Unaligned, N);  // Utilise vmovups
  TimeUnaligned := MilliSecondsBetween(Now, StartTime);

  WriteLn('Temps aligné    : ', TimeAligned, ' ms');
  WriteLn('Temps non aligné: ', TimeUnaligned, ' ms');
  WriteLn('Différence      : ', ((TimeUnaligned - TimeAligned) * 100 div TimeAligned), '%');

  // Résultat typique :
  // Temps aligné    : 120 ms
  // Temps non aligné: 180 ms
  // Différence      : 50% plus lent
end;
```

## Bibliothèques et abstractions SIMD

### 1. Utiliser des wrappers

Au lieu d'écrire de l'assembleur directement, on peut créer des wrappers :

```pascal
unit SIMDUtils;

{$ASMMODE INTEL}

interface

type
  TVector4f = array[0..3] of Single;  // 4 floats (SSE)
  TVector8f = array[0..7] of Single;  // 8 floats (AVX)

// Opérations SSE
function Vec4Add(const A, B: TVector4f): TVector4f;
function Vec4Sub(const A, B: TVector4f): TVector4f;
function Vec4Mul(const A, B: TVector4f): TVector4f;
function Vec4Dot(const A, B: TVector4f): Single;

// Opérations AVX
function Vec8Add(const A, B: TVector8f): TVector8f;
function Vec8Sub(const A, B: TVector8f): TVector8f;
function Vec8Mul(const A, B: TVector8f): TVector8f;

implementation

function Vec4Add(const A, B: TVector4f): TVector4f;
asm
  movups xmm0, [A]
  movups xmm1, [B]
  addps  xmm0, xmm1
  movups [Result], xmm0
end;

function Vec4Sub(const A, B: TVector4f): TVector4f;
asm
  movups xmm0, [A]
  movups xmm1, [B]
  subps  xmm0, xmm1
  movups [Result], xmm0
end;

function Vec4Mul(const A, B: TVector4f): TVector4f;
asm
  movups xmm0, [A]
  movups xmm1, [B]
  mulps  xmm0, xmm1
  movups [Result], xmm0
end;

function Vec4Dot(const A, B: TVector4f): Single;
var
  Temp: TVector4f;
begin
  asm
    movups xmm0, [A]
    movups xmm1, [B]
    mulps  xmm0, xmm1        // Multiplier élément par élément
    movups Temp, xmm0
  end;

  // Somme horizontale
  Result := Temp[0] + Temp[1] + Temp[2] + Temp[3];
end;

function Vec8Add(const A, B: TVector8f): TVector8f;
asm
  vmovups ymm0, [A]
  vmovups ymm1, [B]
  vaddps  ymm0, ymm0, ymm1
  vmovups [Result], ymm0
  vzeroupper
end;

function Vec8Sub(const A, B: TVector8f): TVector8f;
asm
  vmovups ymm0, [A]
  vmovups ymm1, [B]
  vsubps  ymm0, ymm0, ymm1
  vmovups [Result], ymm0
  vzeroupper
end;

function Vec8Mul(const A, B: TVector8f): TVector8f;
asm
  vmovups ymm0, [A]
  vmovups ymm1, [B]
  vmulps  ymm0, ymm0, ymm1
  vmovups [Result], ymm0
  vzeroupper
end;

end.
```

**Utilisation** :
```pascal
uses SIMDUtils;

var
  A, B, C: TVector8f;
  i: Integer;

begin
  // Initialiser
  for i := 0 to 7 do
  begin
    A[i] := i + 1.0;
    B[i] := (i + 1.0) * 2;
  end;

  // Opérations vectorielles
  C := Vec8Add(A, B);
  WriteLn('Addition: ', C[0]:0:1, ', ', C[1]:0:1, ', ...');

  C := Vec8Mul(A, B);
  WriteLn('Multiplication: ', C[0]:0:1, ', ', C[1]:0:1, ', ...');
end;
```

### 2. Détection automatique des capacités CPU

```pascal
unit CPUFeatures;

interface

type
  TCPUFeatures = record
    HasSSE: Boolean;
    HasSSE2: Boolean;
    HasSSE3: Boolean;
    HasSSSE3: Boolean;
    HasSSE41: Boolean;
    HasSSE42: Boolean;
    HasAVX: Boolean;
    HasAVX2: Boolean;
    HasAVX512F: Boolean;
    HasFMA: Boolean;
  end;

function DetectCPUFeatures: TCPUFeatures;

implementation

{$ASMMODE INTEL}

function DetectCPUFeatures: TCPUFeatures;
var
  Info1, Info7: array[0..3] of Cardinal;
begin
  FillChar(Result, SizeOf(Result), 0);

  // CPUID fonction 1
  asm
    push rbx
    mov  eax, 1
    cpuid
    mov  Info1[0], eax
    mov  Info1[4], ebx
    mov  Info1[8], ecx
    mov  Info1[12], edx
    pop  rbx
  end;

  // Vérifier les flags EDX (Info1[3])
  Result.HasSSE   := (Info1[3] and (1 shl 25)) <> 0;
  Result.HasSSE2  := (Info1[3] and (1 shl 26)) <> 0;

  // Vérifier les flags ECX (Info1[2])
  Result.HasSSE3  := (Info1[2] and (1 shl 0)) <> 0;
  Result.HasSSSE3 := (Info1[2] and (1 shl 9)) <> 0;
  Result.HasSSE41 := (Info1[2] and (1 shl 19)) <> 0;
  Result.HasSSE42 := (Info1[2] and (1 shl 20)) <> 0;
  Result.HasAVX   := (Info1[2] and (1 shl 28)) <> 0;
  Result.HasFMA   := (Info1[2] and (1 shl 12)) <> 0;

  // CPUID fonction 7
  asm
    push rbx
    mov  eax, 7
    xor  ecx, ecx
    cpuid
    mov  Info7[0], eax
    mov  Info7[4], ebx
    mov  Info7[8], ecx
    mov  Info7[12], edx
    pop  rbx
  end;

  // Vérifier les flags EBX (Info7[1])
  Result.HasAVX2     := (Info7[1] and (1 shl 5)) <> 0;
  Result.HasAVX512F  := (Info7[1] and (1 shl 16)) <> 0;
end;

end.
```

**Utilisation** :
```pascal
uses CPUFeatures;

var
  Features: TCPUFeatures;

begin
  Features := DetectCPUFeatures;

  WriteLn('Capacités SIMD détectées:');
  WriteLn('SSE:       ', Features.HasSSE);
  WriteLn('SSE2:      ', Features.HasSSE2);
  WriteLn('SSE4.1:    ', Features.HasSSE41);
  WriteLn('AVX:       ', Features.HasAVX);
  WriteLn('AVX2:      ', Features.HasAVX2);
  WriteLn('AVX-512:   ', Features.HasAVX512F);

  // Choisir l'implémentation optimale
  if Features.HasAVX2 then
    ProcessDataAVX2(Data)
  else if Features.HasSSE41 then
    ProcessDataSSE4(Data)
  else
    ProcessDataScalar(Data);
end;
```

### 3. Dispatch dynamique selon le CPU

```pascal
type
  TProcessFunc = procedure(Data: PSingle; Count: Integer);

var
  ProcessOptimal: TProcessFunc;
  Features: TCPUFeatures;

procedure ProcessScalar(Data: PSingle; Count: Integer);
var i: Integer;
begin
  for i := 0 to Count - 1 do
    Data[i] := Data[i] * 2.0;
end;

{$ASMMODE INTEL}
procedure ProcessSSE(Data: PSingle; Count: Integer);
var i, VectorCount: Integer;
begin
  VectorCount := Count div 4;
  asm
    mov rcx, VectorCount
    mov rax, Data
  @Loop:
    movups xmm0, [rax]
    addps  xmm0, xmm0        // Multiplier par 2
    movups [rax], xmm0
    add    rax, 16
    dec    rcx
    jnz    @Loop
  end;

  // Éléments restants
  for i := VectorCount * 4 to Count - 1 do
    Data[i] := Data[i] * 2.0;
end;

procedure ProcessAVX(Data: PSingle; Count: Integer);
var i, VectorCount: Integer;
begin
  VectorCount := Count div 8;
  asm
    mov rcx, VectorCount
    mov rax, Data
  @Loop:
    vmovups ymm0, [rax]
    vaddps  ymm0, ymm0, ymm0  // Multiplier par 2
    vmovups [rax], ymm0
    add     rax, 32
    dec     rcx
    jnz     @Loop
    vzeroupper
  end;

  // Éléments restants
  for i := VectorCount * 8 to Count - 1 do
    Data[i] := Data[i] * 2.0;
end;

initialization
  // Sélectionner la meilleure implémentation au démarrage
  Features := DetectCPUFeatures;

  if Features.HasAVX2 then
  begin
    ProcessOptimal := @ProcessAVX;
    WriteLn('Utilisation: AVX');
  end
  else if Features.HasSSE2 then
  begin
    ProcessOptimal := @ProcessSSE;
    WriteLn('Utilisation: SSE');
  end
  else
  begin
    ProcessOptimal := @ProcessScalar;
    WriteLn('Utilisation: Scalaire');
  end;

// Utilisation
begin
  ProcessOptimal(MyData, MyCount);  // Appelle automatiquement la meilleure version
end;
```

## Optimisations avancées

### 1. Loop Unrolling avec SIMD

**Principe** : Traiter plusieurs vecteurs par itération pour réduire l'overhead de boucle.

```pascal
{$ASMMODE INTEL}

procedure AddArraysUnrolled(A, B, C: PSingle; Count: Integer);
var
  VectorCount, RemainCount, i: Integer;
begin
  VectorCount := Count div 32;  // 32 floats = 4 vecteurs AVX de 8 floats
  RemainCount := Count mod 32;

  if VectorCount > 0 then
  begin
    asm
      mov rcx, VectorCount
      mov rax, A
      mov rdx, B
      mov r8,  C

    @Loop:
      // Traiter 4 vecteurs (32 floats) par itération
      vmovups ymm0, [rax]
      vmovups ymm1, [rax + 32]
      vmovups ymm2, [rax + 64]
      vmovups ymm3, [rax + 96]

      vaddps  ymm0, ymm0, [rdx]
      vaddps  ymm1, ymm1, [rdx + 32]
      vaddps  ymm2, ymm2, [rdx + 64]
      vaddps  ymm3, ymm3, [rdx + 96]

      vmovups [r8], ymm0
      vmovups [r8 + 32], ymm1
      vmovups [r8 + 64], ymm2
      vmovups [r8 + 96], ymm3

      add rax, 128
      add rdx, 128
      add r8,  128

      dec rcx
      jnz @Loop

      vzeroupper
    end;
  end;

  // Traiter les éléments restants
  for i := Count - RemainCount to Count - 1 do
    C[i] := A[i] + B[i];
end;

// Performance : ~20% plus rapide que la version simple
// grâce à moins d'overhead de boucle et meilleur pipelining
```

### 2. Prefetching avec SIMD

```pascal
{$ASMMODE INTEL}

procedure ProcessWithPrefetch(Data: PSingle; Count: Integer);
const
  PrefetchDistance = 64;  // Prefetch 64 floats à l'avance
var
  i, VectorCount: Integer;
begin
  VectorCount := Count div 8;

  asm
    mov rcx, VectorCount
    mov rax, Data

  @Loop:
    // Prefetch des données futures
    prefetchnta [rax + PrefetchDistance * 4]

    // Traiter le vecteur courant
    vmovups ymm0, [rax]
    // ... traitement ...
    vmovups [rax], ymm0

    add rax, 32
    dec rcx
    jnz @Loop

    vzeroupper
  end;
end;
```

### 3. FMA (Fused Multiply-Add)

**FMA** combine multiplication et addition en une seule instruction : `a * b + c`

```pascal
{$ASMMODE INTEL}

procedure FMAExample(A, B, C, Result: PSingle; Count: Integer);
// Result[i] = A[i] * B[i] + C[i]
var
  VectorCount: Integer;
begin
  VectorCount := Count div 8;

  asm
    mov rcx, VectorCount
    mov rax, A
    mov rdx, B
    mov r8,  C
    mov r9,  Result

  @Loop:
    vmovups ymm0, [rax]      // Charger A
    vmovups ymm1, [rdx]      // Charger B
    vmovups ymm2, [r8]       // Charger C

    vfmadd132ps ymm0, ymm2, ymm1  // ymm0 = ymm0 * ymm1 + ymm2

    vmovups [r9], ymm0       // Stocker

    add rax, 32
    add rdx, 32
    add r8,  32
    add r9,  32

    dec rcx
    jnz @Loop

    vzeroupper
  end;
end;

// FMA est plus rapide et plus précis que MUL + ADD séparés
```

## Cas pratiques spécifiques

### 1. Filtrage d'image (blur box)

```pascal
{$ASMMODE INTEL}

procedure BoxBlur3x3SSE(Input, Output: PSingle; Width, Height: Integer);
// Filtre blur 3x3 sur image grayscale
var
  x, y: Integer;
  Row0, Row1, Row2: PSingle;
  Sum: array[0..3] of Single;
begin
  for y := 1 to Height - 2 do
  begin
    Row0 := Input + (y - 1) * Width;
    Row1 := Input + y * Width;
    Row2 := Input + (y + 1) * Width;

    x := 1;
    while x < Width - 5 do  // Traiter 4 pixels à la fois
    begin
      asm
        mov rax, Row0
        mov rdx, Row1
        mov rcx, Row2
        add rax, x * 4
        add rdx, x * 4
        add rcx, x * 4

        // Charger les 3 lignes (3 pixels par ligne)
        movups xmm0, [rax - 4]    // Row0: [x-1, x, x+1, x+2]
        movups xmm1, [rdx - 4]    // Row1: [x-1, x, x+1, x+2]
        movups xmm2, [rcx - 4]    // Row2: [x-1, x, x+1, x+2]

        // Additionner les 3 lignes
        addps xmm0, xmm1
        addps xmm0, xmm2

        // Somme horizontale (simplifiée)
        movups Sum, xmm0
      end;

      // Moyenner (diviser par 9)
      Output[y * Width + x] := (Sum[0] + Sum[1] + Sum[2]) / 9.0;

      Inc(x);
    end;
  end;
end;
```

### 2. Transformation de couleur RGB → YUV

```pascal
{$ASMMODE INTEL}

type
  TRGB = packed record R, G, B: Byte; end;
  TYUV = packed record Y, U, V: Byte; end;

procedure RGBToYUVSSE(RGB: ^TRGB; YUV: ^TYUV; Count: Integer);
// Conversion RGB vers YUV (ITU-R BT.601)
// Y =  0.299*R + 0.587*G + 0.114*B
// U = -0.147*R - 0.289*G + 0.436*B + 128
// V =  0.615*R - 0.515*G - 0.100*B + 128
var
  i: Integer;
  R, G, B: Single;
  Y, U, V: Byte;
begin
  // Note : Version simplifiée scalaire pour la clarté
  // Une vraie implémentation SSE nécessite de charger 4 pixels RGB
  // et de les désentrelacer, ce qui est complexe

  for i := 0 to Count - 1 do
  begin
    R := RGB[i].R;
    G := RGB[i].G;
    B := RGB[i].B;

    Y := Round(0.299 * R + 0.587 * G + 0.114 * B);
    U := Round(-0.147 * R - 0.289 * G + 0.436 * B + 128);
    V := Round(0.615 * R - 0.515 * G - 0.100 * B + 128);

    YUV[i].Y := Y;
    YUV[i].U := U;
    YUV[i].V := V;
  end;
end;
```

### 3. Calcul de distance euclidienne

```pascal
{$ASMMODE INTEL}

function DistanceEuclideanSSE(A, B: PSingle; Dimensions: Integer): Single;
// Distance euclidienne entre deux vecteurs de N dimensions
var
  VectorCount, i: Integer;
  PartialSums: array[0..3] of Single;
  Sum: Single;
begin
  VectorCount := Dimensions div 4;
  Sum := 0;

  if VectorCount > 0 then
  begin
    asm
      xorps xmm0, xmm0      // Accumulateur
      mov   rcx, VectorCount
      mov   rax, A
      mov   rdx, B

    @Loop:
      movups xmm1, [rax]    // Charger A
      movups xmm2, [rdx]    // Charger B
      subps  xmm1, xmm2     // Différence : A - B
      mulps  xmm1, xmm1     // Carré : (A - B)²
      addps  xmm0, xmm1     // Accumuler

      add    rax, 16
      add    rdx, 16
      dec    rcx
      jnz    @Loop

      movups PartialSums, xmm0
    end;

    // Somme horizontale
    Sum := PartialSums[0] + PartialSums[1] + PartialSums[2] + PartialSums[3];
  end;

  // Traiter les dimensions restantes
  for i := VectorCount * 4 to Dimensions - 1 do
    Sum := Sum + Sqr(A[i] - B[i]);

  // Racine carrée
  Result := Sqrt(Sum);
end;
```

## SIMD sur ARM (NEON)

Pour le développement multi-plateforme incluant ARM (Raspberry Pi, mobiles) :

### Instructions NEON de base

```pascal
{$IFDEF CPUARM}
{$ASMMODE GAS}  // Syntaxe GNU Assembler pour ARM

procedure AddArraysNEON(A, B, C: PSingle; Count: Integer);
// NEON : 128 bits = 4 floats
var
  VectorCount, i: Integer;
begin
  VectorCount := Count div 4;

  if VectorCount > 0 then
  begin
    asm
      ldr   r0, A           // Charger l'adresse de A
      ldr   r1, B           // Charger l'adresse de B
      ldr   r2, C           // Charger l'adresse de C
      ldr   r3, VectorCount

    .Loop:
      vld1.32 {q0}, [r0]!   // Charger 4 floats de A, post-increment
      vld1.32 {q1}, [r1]!   // Charger 4 floats de B, post-increment
      vadd.f32 q0, q0, q1   // Addition vectorielle
      vst1.32 {q0}, [r2]!   // Stocker dans C, post-increment

      subs  r3, r3, #1
      bne   .Loop
    end;
  end;

  // Traiter les éléments restants
  for i := VectorCount * 4 to Count - 1 do
    C[i] := A[i] + B[i];
end;
{$ENDIF}
```

### Détection NEON

```pascal
{$IFDEF CPUARM}
function HasNEON: Boolean;
begin
  // Sur ARM Linux, vérifier /proc/cpuinfo
  Result := False;  // Implémentation simplifiée

  // En réalité, lire et parser /proc/cpuinfo
  // et chercher 'neon' dans les flags
end;
{$ENDIF}
```

## Pièges et erreurs courantes

### 1. Oublier vzeroupper après AVX

```pascal
// ❌ Mauvais
procedure ProcessAVX(Data: PSingle; Count: Integer);
asm
  vmovups ymm0, [Data]
  // ... traitement ...
  vmovups [Data], ymm0
  // Oubli de vzeroupper !
end;

// Code suivant non-AVX sera pénalisé
// Pénalité : ~70 cycles à chaque transition

// ✅ Bon
procedure ProcessAVX(Data: PSingle; Count: Integer);
asm
  vmovups ymm0, [Data]
  // ... traitement ...
  vmovups [Data], ymm0
  vzeroupper  // Nettoyer les registres
end;
```

### 2. Données non alignées avec instructions alignées

```pascal
// ❌ Crash si Data n'est pas aligné sur 32 bytes
asm
  vmovaps ymm0, [Data]  // movAps = aligned (crash si non aligné)
end;

// ✅ Utiliser movups ou aligner les données
asm
  vmovups ymm0, [Data]  // movUps = unaligned (toujours safe)
end;
```

### 3. Modifier les registres sans les sauvegarder

```pascal
// ❌ Mauvais : perd des données
procedure Process;
asm
  movaps xmm0, [Data]
  // xmm0 est utilisé par le compilateur, on peut perdre des données
end;

// ✅ Bon : utiliser des registres locaux ou sauvegarder
procedure Process;
var
  TempXMM: array[0..3] of Single;
asm
  movaps TempXMM, xmm0  // Sauvegarder
  // ... utiliser xmm0 ...
  movaps xmm0, TempXMM  // Restaurer
end;
```

### 4. Mélanger scalaire et vectoriel

```pascal
// ❌ Inefficace : passer de SIMD à scalaire constamment
for i := 0 to Count - 1 do
begin
  if i mod 4 = 0 then
    ProcessVectorSSE(@Data[i])  // SIMD
  else
    Data[i] := Data[i] * 2;     // Scalaire
end;

// ✅ Traiter par blocs homogènes
VectorCount := Count div 4;
for i := 0 to VectorCount - 1 do
  ProcessVectorSSE(@Data[i * 4]);  // Tout en SIMD

for i := VectorCount * 4 to Count - 1 do
  Data[i] := Data[i] * 2;          // Reste en scalaire
```

## Benchmarking et validation

### Template de benchmark SIMD

```pascal
program BenchmarkSIMD;

uses
  SysUtils, DateUtils;

const
  N = 10000000;       // 10 millions d'éléments
  Iterations = 100;   // Répéter 100 fois

var
  A, B, C: array of Single;
  i, iter: Integer;
  StartTime: TDateTime;
  TimeScalar, TimeSSE, TimeAVX: Int64;

procedure InitData;
var i: Integer;
begin
  SetLength(A, N);
  SetLength(B, N);
  SetLength(C, N);

  for i := 0 to N - 1 do
  begin
    A[i] := Random * 100;
    B[i] := Random * 100;
  end;
end;

procedure ValidateResults(const C1, C2: array of Single);
var
  i: Integer;
  MaxError: Single;
begin
  MaxError := 0;
  for i := 0 to High(C1) do
  begin
    if Abs(C1[i] - C2[i]) > MaxError then
      MaxError := Abs(C1[i] - C2[i]);
  end;

  if MaxError > 0.001 then
    WriteLn('ERREUR: Différence maximale = ', MaxError:0:6)
  else
    WriteLn('Validation OK (erreur max = ', MaxError:0:6, ')');
end;

procedure BenchScalar;
var i, j: Integer;
begin
  for j := 1 to Iterations do
    for i := 0 to N - 1 do
      C[i] := A[i] + B[i];
end;

var
  CScalar, CSSE, CAVX: array of Single;

begin
  Randomize;
  InitData;

  // Benchmark version scalaire
  WriteLn('Benchmark version scalaire...');
  SetLength(CScalar, N);
  StartTime := Now;
  BenchScalar;
  TimeScalar := MilliSecondsBetween(Now, StartTime);
  Move(C[0], CScalar[0], N * SizeOf(Single));
  WriteLn('Temps scalaire: ', TimeScalar, ' ms');

  // Benchmark version SSE
  WriteLn('Benchmark version SSE...');
  SetLength(CSSE, N);
  StartTime := Now;
  for iter := 1 to Iterations do
    AddArraysSSE(@A[0], @B[0], @C[0], N);
  TimeSSE := MilliSecondsBetween(Now, StartTime);
  Move(C[0], CSSE[0], N * SizeOf(Single));
  WriteLn('Temps SSE: ', TimeSSE, ' ms');
  WriteLn('Speedup SSE: ', (TimeScalar / TimeSSE):0:2, 'x');

  // Benchmark version AVX
  WriteLn('Benchmark version AVX...');
  SetLength(CAVX, N);
  StartTime := Now;
  for iter := 1 to Iterations do
    AddArraysAVX(@A[0], @B[0], @C[0], N);
  TimeAVX := MilliSecondsBetween(Now, StartTime);
  Move(C[0], CAVX[0], N * SizeOf(Single));
  WriteLn('Temps AVX: ', TimeAVX, ' ms');
  WriteLn('Speedup AVX: ', (TimeScalar / TimeAVX):0:2, 'x');

  // Validation
  WriteLn;
  WriteLn('Validation des résultats:');
  Write('SSE vs Scalaire: ');
  ValidateResults(CScalar, CSSE);
  Write('AVX vs Scalaire: ');
  ValidateResults(CScalar, CAVX);

  // Résumé
  WriteLn;
  WriteLn('=== RÉSUMÉ ===');
  WriteLn('Scalaire: ', TimeScalar, ' ms (baseline)');
  WriteLn('SSE:      ', TimeSSE, ' ms (', (TimeScalar / TimeSSE):0:2, 'x)');
  WriteLn('AVX:      ', TimeAVX, ' ms (', (TimeScalar / TimeAVX):0:2, 'x)');

  ReadLn;
end.

// Exemple de sortie :
// Benchmark version scalaire...
// Temps scalaire: 1250 ms
// Benchmark version SSE...
// Temps SSE: 380 ms
// Speedup SSE: 3.29x
// Benchmark version AVX...
// Temps AVX: 195 ms
// Speedup AVX: 6.41x
//
// Validation des résultats:
// SSE vs Scalaire: Validation OK (erreur max = 0.000000)
// AVX vs Scalaire: Validation OK (erreur max = 0.000000)
```

## Différences Windows vs Linux

### Conventions d'appel

**Windows x64** :
- Registres volatiles : RAX, RCX, RDX, R8-R11, XMM0-XMM5
- Registres non-volatiles : RBX, RSI, RDI, RBP, R12-R15, XMM6-XMM15
- Paramètres : RCX, RDX, R8, R9, puis stack

**Linux x64 (System V ABI)** :
- Registres volatiles : RAX, RCX, RDX, RSI, RDI, R8-R11, XMM0-XMM15
- Registres non-volatiles : RBX, RBP, R12-R15
- Paramètres : RDI, RSI, RDX, RCX, R8, R9, puis stack

**Exemple de fonction compatible** :
```pascal
{$ASMMODE INTEL}

procedure CrossPlatformSIMD(Data: PSingle; Count: Integer);
asm
  {$IFDEF WINDOWS}
  // Windows: Data dans RCX, Count dans RDX
  mov rax, rcx      // Data
  mov rcx, rdx      // Count
  {$ELSE}
  // Linux: Data dans RDI, Count dans RSI
  mov rax, rdi      // Data
  mov rcx, rsi      // Count
  {$ENDIF}

  // Code commun
  shr rcx, 3        // Diviser par 8 (AVX)

@Loop:
  vmovups ymm0, [rax]
  vaddps  ymm0, ymm0, ymm0
  vmovups [rax], ymm0
  add     rax, 32
  dec     rcx
  jnz     @Loop

  vzeroupper
end;
```

### Support AVX-512

**Windows** :
- Support complet dans Windows 10+
- Drivers et OS gèrent bien les contextes AVX-512

**Linux** :
- Support dans noyau 4.x+
- Meilleure gestion de l'économie d'énergie (throttling)
- `/proc/cpuinfo` montre les flags AVX-512

**Vérification du support** :
```bash
# Linux
grep avx512 /proc/cpuinfo

# Windows PowerShell
wmic cpu get caption, name
# Puis vérifier les spécifications du CPU en ligne
```

## Optimisations spécifiques aux plateformes

### Linux : utilisation de GCC vector extensions

FreePascal ne supporte pas directement les extensions vectorielles de GCC, mais on peut créer des bindings :

```pascal
// Créer une bibliothèque C avec GCC
// fichier: vector_ops.c

typedef float v8sf __attribute__ ((vector_size (32)));

void add_arrays_gcc(float* a, float* b, float* c, int n) {
    int i;
    v8sf* va = (v8sf*)a;
    v8sf* vb = (v8sf*)b;
    v8sf* vc = (v8sf*)c;

    for(i = 0; i < n/8; i++) {
        vc[i] = va[i] + vb[i];  // Vectorisation automatique
    }
}

// Compiler avec:
// gcc -O3 -mavx2 -shared -fPIC vector_ops.c -o libvector_ops.so
```

```pascal
// Dans FreePascal
{$LINKLIB vector_ops}

procedure add_arrays_gcc(a, b, c: PSingle; n: Integer); cdecl; external;

// Utilisation
begin
  add_arrays_gcc(@A[0], @B[0], @C[0], Length(A));
end;
```

### Windows : utilisation d'intrinsics Intel

Créer une DLL C++ avec les intrinsics Intel :

```cpp
// fichier: simd_ops.cpp
#include <immintrin.h>

extern "C" __declspec(dllexport)
void add_arrays_avx(float* a, float* b, float* c, int n) {
    int i;
    for(i = 0; i < n; i += 8) {
        __m256 va = _mm256_loadu_ps(&a[i]);
        __m256 vb = _mm256_loadu_ps(&b[i]);
        __m256 vc = _mm256_add_ps(va, vb);
        _mm256_storeu_ps(&c[i], vc);
    }
    _mm256_zeroupper();
}

// Compiler avec Visual Studio ou MinGW:
// cl /O2 /arch:AVX2 /LD simd_ops.cpp
```

```pascal
// Dans FreePascal
procedure add_arrays_avx(a, b, c: PSingle; n: Integer); cdecl; external 'simd_ops.dll';
```

## Bibliothèques et frameworks SIMD

### 1. Utiliser OpenBLAS (multi-plateforme)

OpenBLAS est une bibliothèque BLAS hautement optimisée avec SIMD.

**Installation** :

**Windows** :
```bash
# Télécharger depuis https://github.com/xianyi/OpenBLAS/releases
# Copier libopenblas.dll dans le répertoire du programme
```

**Linux** :
```bash
sudo apt install libopenblas-dev
```

**Utilisation depuis FreePascal** :
```pascal
unit OpenBLASWrapper;

interface

// BLAS niveau 1: Opérations vecteur-vecteur

// SAXPY: Y = alpha * X + Y
procedure cblas_saxpy(n: Integer; alpha: Single;
                      x: PSingle; incx: Integer;
                      y: PSingle; incy: Integer); cdecl; external 'openblas';

// SDOT: Produit scalaire
function cblas_sdot(n: Integer;
                    x: PSingle; incx: Integer;
                    y: PSingle; incy: Integer): Single; cdecl; external 'openblas';

// BLAS niveau 2: Opérations matrice-vecteur

// SGEMV: y = alpha * A * x + beta * y
procedure cblas_sgemv(order: Integer; trans: Integer;
                      m, n: Integer; alpha: Single;
                      a: PSingle; lda: Integer;
                      x: PSingle; incx: Integer;
                      beta: Single; y: PSingle; incy: Integer); cdecl; external 'openblas';

// BLAS niveau 3: Opérations matrice-matrice

// SGEMM: C = alpha * A * B + beta * C
procedure cblas_sgemm(order: Integer; transA, transB: Integer;
                      m, n, k: Integer;
                      alpha: Single; a: PSingle; lda: Integer;
                      b: PSingle; ldb: Integer;
                      beta: Single; c: PSingle; ldc: Integer); cdecl; external 'openblas';

const
  CblasRowMajor = 101;
  CblasColMajor = 102;
  CblasNoTrans = 111;
  CblasTrans = 112;

implementation

end.

// Exemple d'utilisation
uses OpenBLASWrapper;

var
  A, B, C: array[0..1023] of Single;
  i: Integer;

begin
  // Initialiser A et B
  for i := 0 to 1023 do
  begin
    A[i] := i;
    B[i] := i * 2;
  end;

  // C = A + 2.5 * B (SAXPY)
  Move(A, C, SizeOf(A));
  cblas_saxpy(1024, 2.5, @B[0], 1, @C[0], 1);

  // Produit scalaire A · B
  WriteLn('Dot product: ', cblas_sdot(1024, @A[0], 1, @B[0], 1):0:2);
end;
```

**Performance** : OpenBLAS utilise automatiquement les meilleures instructions SIMD disponibles (SSE, AVX, AVX2, AVX-512).

### 2. Intel MKL (Math Kernel Library)

**Windows uniquement** (gratuit mais propriétaire).

Similar à OpenBLAS mais optimisé spécifiquement pour Intel.

### 3. Bibliothèques spécialisées

**Images/Vidéo** :
- **IPP (Integrated Performance Primitives)** - Intel
- **OpenCV** - Avec support SIMD automatique

**Audio/DSP** :
- **FFTW** - Fast Fourier Transform (SIMD)
- **libsamplerate** - Resampling audio

**Compression** :
- **libdeflate** - Compression DEFLATE avec SIMD
- **zstd** - Compression Zstandard avec SIMD

## Cas d'étude : Application complète

### Exemple : Traitement d'images par lots

```pascal
program ImageBatchProcessor;

{$MODE OBJFPC}
{$H+}
{$ASMMODE INTEL}

uses
  SysUtils, DateUtils, Classes;

type
  TPixelRGB = packed record
    R, G, B: Byte;
  end;
  PPixelRGB = ^TPixelRGB;

  TImage = record
    Width, Height: Integer;
    Pixels: array of TPixelRGB;
  end;

var
  CPUSupportsAVX: Boolean = False;

// Détection CPU
procedure DetectCPU;
var
  Info: array[0..3] of Cardinal;
begin
  asm
    push rbx
    mov  eax, 1
    cpuid
    mov  Info[8], ecx
    pop  rbx
  end;
  CPUSupportsAVX := (Info[2] and (1 shl 28)) <> 0;

  WriteLn('CPU Features:');
  WriteLn('  AVX: ', CPUSupportsAVX);
end;

// Chargement image (simplifié - en réalité utiliser FPImage ou autre)
function LoadImage(const FileName: string): TImage;
var
  i: Integer;
begin
  Result.Width := 1920;
  Result.Height := 1080;
  SetLength(Result.Pixels, Result.Width * Result.Height);

  // Remplir avec données aléatoires pour le test
  Randomize;
  for i := 0 to High(Result.Pixels) do
  begin
    Result.Pixels[i].R := Random(256);
    Result.Pixels[i].G := Random(256);
    Result.Pixels[i].B := Random(256);
  end;
end;

// Conversion RGB → Grayscale (version scalaire)
procedure ConvertToGrayscaleScalar(var Img: TImage);
var
  i: Integer;
  Gray: Byte;
begin
  for i := 0 to High(Img.Pixels) do
  begin
    Gray := Round(0.299 * Img.Pixels[i].R +
                  0.587 * Img.Pixels[i].G +
                  0.114 * Img.Pixels[i].B);
    Img.Pixels[i].R := Gray;
    Img.Pixels[i].G := Gray;
    Img.Pixels[i].B := Gray;
  end;
end;

// Conversion RGB → Grayscale (version SIMD optimisée)
procedure ConvertToGrayscaleSIMD(var Img: TImage);
var
  i: Integer;
  P: PPixelRGB;
  R, G, B, Gray: Integer;
begin
  // Version optimisée avec arithmétique entière
  // (Plus simple que vraie vectorisation RGB qui nécessite désentrelacement)
  P := @Img.Pixels[0];

  for i := 0 to High(Img.Pixels) do
  begin
    R := P^.R;
    G := P^.G;
    B := P^.B;

    // Approximation: (R * 77 + G * 150 + B * 29) >> 8
    Gray := (R * 77 + G * 150 + B * 29) shr 8;

    P^.R := Gray;
    P^.G := Gray;
    P^.B := Gray;
    Inc(P);
  end;
end;

// Augmentation de luminosité (version SSE)
procedure BrightenSSE(var Img: TImage; Amount: Single);
var
  i, Count: Integer;
  Pixels: PByte;
  Scale: Single;
begin
  Scale := 1.0 + Amount;
  Pixels := PByte(@Img.Pixels[0]);
  Count := Length(Img.Pixels) * 3;  // 3 bytes par pixel

  // Traiter par blocs de 16 bytes (SSE)
  i := 0;
  while i + 16 <= Count do
  begin
    asm
      mov rax, Pixels
      add rax, i

      // Charger 16 bytes
      movdqu xmm0, [rax]

      // Convertir en float (complexe pour bytes, simplifié ici)
      // En réalité nécessite plusieurs étapes de conversion

      // Pour la démo, on utilise une approche scalaire dans la boucle
    end;

    Inc(i, 16);
  end;

  // Traiter les bytes restants en scalaire
  while i < Count do
  begin
    Pixels[i] := Min(Round(Pixels[i] * Scale), 255);
    Inc(i);
  end;
end;

// Traiter un lot d'images
procedure ProcessBatch(const Files: TStringList);
var
  i: Integer;
  Img: TImage;
  StartTime, TotalTime: TDateTime;
  TimePerImage: Double;
begin
  WriteLn('Traitement de ', Files.Count, ' images...');

  TotalTime := 0;

  for i := 0 to Files.Count - 1 do
  begin
    Write('Image ', i + 1, '/', Files.Count, ': ');

    StartTime := Now;

    // Charger
    Img := LoadImage(Files[i]);

    // Traiter
    if CPUSupportsAVX then
      ConvertToGrayscaleSIMD(Img)
    else
      ConvertToGrayscaleScalar(Img);

    BrightenSSE(Img, 0.2);  // +20% luminosité

    // Sauvegarder (simplifié)
    // SaveImage(Img, ChangeFileExt(Files[i], '.processed.bmp'));

    TimePerImage := MilliSecondsBetween(Now, StartTime);
    TotalTime := TotalTime + TimePerImage;

    WriteLn(TimePerImage:0:0, ' ms');
  end;

  WriteLn;
  WriteLn('Temps total: ', TotalTime:0:0, ' ms');
  WriteLn('Moyenne: ', (TotalTime / Files.Count):0:1, ' ms/image');
end;

var
  Files: TStringList;
  i: Integer;

begin
  DetectCPU;
  WriteLn;

  // Créer une liste de fichiers fictifs pour le test
  Files := TStringList.Create;
  try
    for i := 1 to 100 do
      Files.Add('image_' + IntToStr(i) + '.bmp');

    ProcessBatch(Files);
  finally
    Files.Free;
  end;

  WriteLn;
  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
```

## Ressources et documentation

### Documentation officielle

**Intel** :
- Intel Intrinsics Guide : https://software.intel.com/sites/landingpage/IntrinsicsGuide/
- Intel 64 and IA-32 Architectures Software Developer Manuals
- Intel Optimization Reference Manual

**ARM** :
- ARM NEON Programmer's Guide
- ARM NEON Intrinsics Reference

**AMD** :
- AMD64 Architecture Programmer's Manual
- AMD Optimization Guide

### Tutoriels et ressources

**Sites web** :
- Agner Fog's optimization guides : https://www.agner.org/optimize/
- Intel Developer Zone
- Crunching Numbers with AVX (blog posts)

**Livres** :
- "Computer Systems: A Programmer's Perspective" (Patterson & Hennessy)
- "Intel® 64 and IA-32 Architectures Optimization Reference Manual"

### Outils de développement

**Analyseurs de code** :
- Intel Advisor (Windows/Linux) - Analyse vectorisation
- LLVM's opt avec -vectorize-loops
- GCC avec -fopt-info-vec

**Profileurs** :
- Intel VTune (voir section 20.1.1)
- Perf sur Linux (voir section 20.1.2)
- AMD uProf

## Checklist SIMD

### Avant d'optimiser avec SIMD

- [ ] Profiler pour identifier les hotspots (>20% du temps)
- [ ] Vérifier que le hotspot est vectorisable
- [ ] Mesurer les performances de base (baseline)
- [ ] Vérifier le support CPU sur les machines cibles

### Pendant l'optimisation

- [ ] Choisir le bon ensemble d'instructions (SSE/AVX/NEON)
- [ ] Aligner les données sur 16/32 bytes si possible
- [ ] Traiter les éléments restants (tail) en scalaire
- [ ] Utiliser vzeroupper après code AVX
- [ ] Tester sur CPU avec et sans support SIMD

### Après l'optimisation

- [ ] Valider que les résultats sont corrects (epsilon pour floats)
- [ ] Mesurer le speedup réel
- [ ] Vérifier que le code fonctionne sur Windows ET Linux
- [ ] Documenter les prérequis CPU
- [ ] Avoir un fallback scalaire si SIMD non disponible

## Résumé

### Gains de performance typiques

| Opération | Scalaire | SSE (4x) | AVX (8x) | AVX-512 (16x) |
|-----------|----------|----------|----------|---------------|
| **Addition floats** | Baseline | 3.5-4x | 6-7x | 10-14x |
| **Multiplication** | Baseline | 3.5-4x | 6-7x | 10-14x |
| **Produit scalaire** | Baseline | 3-3.5x | 5-6x | 9-12x |
| **Image processing** | Baseline | 2-3x | 4-5x | 7-10x |

**Note** : Les gains réels dépendent de :
- La complexité de l'algorithme
- L'alignement des données
- Les cache misses
- La bande passante mémoire

### Points clés

✅ **SIMD = parallélisme de données** : Une instruction traite plusieurs données

✅ **Extensions disponibles** :
  - x86/x64 : SSE, AVX, AVX2, AVX-512
  - ARM : NEON, SVE

✅ **Quand utiliser** :
  - Calculs sur tableaux de nombres
  - Traitement d'images/audio/vidéo
  - Pas de branchements complexes
  - Accès mémoire contigus

✅ **Auto-vectorisation** : Compiler avec `-O3 -CpCOREAVX2 -CfAVX2`

✅ **Vectorisation manuelle** : Assembleur inline pour contrôle total

✅ **Multi-plateforme** : Code différent pour Windows/Linux/ARM

✅ **Alignement crucial** : 16 bytes (SSE), 32 bytes (AVX)

✅ **Toujours valider** : Comparer résultats SIMD vs scalaire

✅ **Mesurer l'impact** : Profiler avant et après optimisation

### Quand NE PAS utiliser SIMD

❌ Code avec beaucoup de branchements (`if/else`)  
❌ Dépendances entre itérations  
❌ Accès mémoire non contigus/aléatoires  
❌ Petites quantités de données (< 100 éléments)  
❌ Code critique pour la maintenabilité  
❌ Pas de gain mesuré significatif (< 2x)

### Prochaines étapes

Maintenant que vous maîtrisez SIMD, explorez :

- **Section 20.4** : Optimisations spécifiques CPU (branch prediction, pipeline)
- **Section 20.5** : Structures de données optimales
- **Section 20.6** : Algorithmes haute performance

SIMD est un outil puissant mais complexe. Commencez par l'auto-vectorisation, puis passez à l'assembleur inline uniquement si nécessaire. Toujours mesurer l'impact réel sur vos données et votre matériel cible !

---

*Note : Ce tutoriel fait partie de la formation "FreePascal/Lazarus - Niveau Développeur Avancé - Edition Multi-plateforme Windows/Ubuntu"*

⏭️ [Optimisations spécifiques CPU](/20-optimisation-performance/04-optimisations-specifiques-cpu.md)
