🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 20.10 Optimisation pour Différentes Architectures

## Introduction

L'**optimisation pour différentes architectures** consiste à adapter votre code pour qu'il s'exécute de manière optimale sur diverses configurations matérielles : processeurs Intel/AMD (x86/x64), ARM, architectures 32 bits vs 64 bits, etc.

### Pourquoi s'en préoccuper ?

**Analogie simple :** Imaginez que vous devez transporter des marchandises. Vous n'utiliseriez pas la même méthode avec une petite voiture (32 bits), un camion (64 bits) ou un drone (ARM embarqué). Chaque véhicule a ses forces et faiblesses.

De la même façon, un code optimisé pour un processeur Intel moderne ne sera pas forcément optimal sur un Raspberry Pi ARM.

### Objectifs de ce chapitre

- Comprendre les différences entre architectures
- Détecter l'architecture à l'exécution
- Adapter le code selon le matériel
- Utiliser les instructions spécifiques (SIMD)
- Optimiser pour Windows x64 et Ubuntu x64/ARM

---

## 1. Comprendre les Architectures

### 1.1 Les Principales Architectures

#### x86 (32 bits)
- **Registres** : 32 bits (4 octets)
- **Adressage mémoire** : Limité à 4 GB
- **Utilisation** : Anciens PC, systèmes embarqués légers
- **Exemple** : Intel Pentium, AMD Athlon

#### x86_64 / AMD64 (64 bits)
- **Registres** : 64 bits (8 octets)
- **Adressage mémoire** : Théoriquement 16 exaoctets
- **Utilisation** : PC modernes Windows/Ubuntu
- **Exemple** : Intel Core i7, AMD Ryzen

#### ARM (32/64 bits)
- **Caractéristiques** : Architecture RISC, faible consommation
- **Utilisation** : Smartphones, Raspberry Pi, serveurs (AWS Graviton)
- **Variantes** : ARMv7 (32 bits), ARMv8/AArch64 (64 bits)

#### Autres
- **PowerPC** : Anciens Mac, consoles de jeux
- **MIPS** : Routeurs, systèmes embarqués
- **RISC-V** : Architecture open-source émergente

### 1.2 Tableau Comparatif

| Caractéristique | x86 (32 bits) | x86_64 (64 bits) | ARM (32 bits) | ARM (64 bits) |
|-----------------|---------------|------------------|---------------|---------------|
| Taille registre | 32 bits       | 64 bits          | 32 bits       | 64 bits       |
| Mémoire max     | 4 GB          | 128+ GB          | 4 GB          | 128+ GB       |
| Performance     | Faible        | Élevée           | Moyenne       | Élevée        |
| Consommation    | Moyenne       | Moyenne-Élevée   | Très faible   | Faible        |
| SIMD            | SSE, SSE2     | SSE, AVX, AVX2   | NEON          | NEON          |

---

## 2. Détection de l'Architecture

### 2.1 Détection à la Compilation

FreePascal fournit des directives pour détecter l'architecture lors de la compilation :

```pascal
program ArchDetection;

begin
  WriteLn('=== Détection d''architecture ===');

  // Architecture du CPU
  {$IFDEF CPUX86_64}
  WriteLn('Architecture : x86_64 (64 bits Intel/AMD)');
  {$ENDIF}

  {$IFDEF CPUI386}
  WriteLn('Architecture : i386 (32 bits Intel/AMD)');
  {$ENDIF}

  {$IFDEF CPUARM}
  WriteLn('Architecture : ARM (32 bits)');
  {$ENDIF}

  {$IFDEF CPUAARCH64}
  WriteLn('Architecture : AArch64 (ARM 64 bits)');
  {$ENDIF}

  // Système d'exploitation
  {$IFDEF WINDOWS}
  WriteLn('OS : Windows');
  {$ENDIF}

  {$IFDEF UNIX}
  WriteLn('OS : Unix/Linux');
  {$ENDIF}

  // Endianness (ordre des octets)
  {$IFDEF ENDIAN_LITTLE}
  WriteLn('Endianness : Little Endian');
  {$ENDIF}

  {$IFDEF ENDIAN_BIG}
  WriteLn('Endianness : Big Endian');
  {$ENDIF}

  // Taille des pointeurs
  WriteLn('Taille des pointeurs : ', SizeOf(Pointer), ' octets');
  WriteLn('Taille d''un Integer : ', SizeOf(Integer), ' octets');
  WriteLn('Taille d''un NativeInt : ', SizeOf(NativeInt), ' octets');
end.
```

### 2.2 Types Natifs Adaptatifs

FreePascal fournit des types qui s'adaptent automatiquement à l'architecture :

```pascal
type
  // Types de taille fixe (cross-platform)
  TInt8   = ShortInt;   // Toujours 8 bits
  TInt16  = SmallInt;   // Toujours 16 bits
  TInt32  = LongInt;    // Toujours 32 bits
  TInt64  = Int64;      // Toujours 64 bits

  // Types adaptatifs (dépendent de l'architecture)
  TNative = NativeInt;  // 32 bits sur x86, 64 bits sur x64
  TSize   = SizeUInt;   // Même taille qu'un pointeur

procedure DemonstrateNativeTypes;  
var
  N: NativeInt;
  P: PByte;
  S: SizeUInt;
begin
  WriteLn('=== Types natifs ===');

  // NativeInt : optimal pour l'indexation et l'arithmétique
  N := 1000000;
  WriteLn('NativeInt : ', SizeOf(N), ' octets');

  // PtrInt : pour l'arithmétique sur les pointeurs
  P := nil;
  WriteLn('Pointeur : ', SizeOf(P), ' octets');

  // SizeUInt : pour les tailles mémoire
  S := 1024 * 1024;  // 1 MB
  WriteLn('SizeUInt : ', SizeOf(S), ' octets');

  {$IFDEF CPU64}
  WriteLn('→ Architecture 64 bits détectée');
  {$ELSE}
  WriteLn('→ Architecture 32 bits détectée');
  {$ENDIF}
end;
```

### 2.3 Détection du CPU à l'Exécution

Pour détecter les capacités du CPU pendant l'exécution :

```pascal
uses
  SysUtils;

type
  TCPUFeatures = record
    HasSSE: Boolean;
    HasSSE2: Boolean;
    HasSSE3: Boolean;
    HasAVX: Boolean;
    HasAVX2: Boolean;
    HasNEON: Boolean;
  end;

function DetectCPUFeatures: TCPUFeatures;
{$IFDEF CPUX86_64}
var
  EAX, EBX, ECX, EDX: Cardinal;
{$ENDIF}
begin
  Result.HasSSE := False;
  Result.HasSSE2 := False;
  Result.HasSSE3 := False;
  Result.HasAVX := False;
  Result.HasAVX2 := False;
  Result.HasNEON := False;

  {$IFDEF CPUX86_64}
  // Instruction CPUID pour détecter les fonctionnalités x86
  asm
    mov eax, 1
    cpuid
    mov EAX, eax
    mov EBX, ebx
    mov ECX, ecx
    mov EDX, edx
  end;

  // Analyse des bits
  Result.HasSSE := (EDX and (1 shl 25)) <> 0;
  Result.HasSSE2 := (EDX and (1 shl 26)) <> 0;
  Result.HasSSE3 := (ECX and (1 shl 0)) <> 0;
  Result.HasAVX := (ECX and (1 shl 28)) <> 0;

  // Pour AVX2, il faut CPUID avec EAX=7
  asm
    mov eax, 7
    mov ecx, 0
    cpuid
    mov EBX, ebx
  end;
  Result.HasAVX2 := (EBX and (1 shl 5)) <> 0;
  {$ENDIF}

  {$IFDEF CPUARM}
  // ARM possède toujours NEON sur les processeurs modernes
  Result.HasNEON := True;
  {$ENDIF}
end;

procedure PrintCPUFeatures;  
var
  Features: TCPUFeatures;
begin
  Features := DetectCPUFeatures;

  WriteLn('=== Fonctionnalités du CPU ===');
  WriteLn('SSE   : ', Features.HasSSE);
  WriteLn('SSE2  : ', Features.HasSSE2);
  WriteLn('SSE3  : ', Features.HasSSE3);
  WriteLn('AVX   : ', Features.HasAVX);
  WriteLn('AVX2  : ', Features.HasAVX2);
  WriteLn('NEON  : ', Features.HasNEON);
end;
```

---

## 3. Optimisations Générales Multi-Architecture

### 3.1 Alignement Mémoire

L'alignement mémoire est crucial pour les performances :

```pascal
type
  // Non aligné (peut être lent sur certaines architectures)
  TDataNonAligned = packed record
    A: Byte;
    B: Int64;  // Peut ne pas être aligné sur 8 octets
    C: Integer;
  end;

  // Aligné (optimal)
  TDataAligned = record
    B: Int64;     // Aligné sur 8 octets
    C: Integer;   // Aligné sur 4 octets
    A: Byte;      // 1 octet
    // Le compilateur ajoutera du padding automatiquement
  end;

procedure DemonstrateAlignment;  
begin
  WriteLn('=== Alignement mémoire ===');
  WriteLn('TDataNonAligned : ', SizeOf(TDataNonAligned), ' octets');
  WriteLn('TDataAligned    : ', SizeOf(TDataAligned), ' octets');

  // Sur x86_64, les accès non alignés fonctionnent mais sont plus lents
  // Sur ARM, les accès non alignés peuvent causer des erreurs !
end;

// Forcer un alignement spécifique
type
  TAligned16 = record
    Data: array[0..15] of Byte;
  end align 16;  // Force alignement sur 16 octets (optimal pour SIMD)
```

### 3.2 Utilisation de Types Optimaux

Choisissez les types selon l'architecture :

```pascal
procedure OptimalTypes;  
var
  // Pour l'indexation de tableaux
  Index: NativeInt;  // 32 bits sur x86, 64 bits sur x64

  // Pour les calculs intensifs
  Value: Int64;      // Toujours 64 bits, plus rapide sur x64

  // Pour les compteurs simples
  Counter: Integer;  // Généralement optimal

  // Pour les tailles mémoire
  Size: SizeUInt;    // S'adapte automatiquement
begin
  {$IFDEF CPU64}
  // Sur 64 bits, les opérations 64 bits sont natives et rapides
  Value := 1000000000000;
  WriteLn('Calcul 64 bits natif : ', Value * 2);
  {$ELSE}
  // Sur 32 bits, préférer Integer quand possible
  Counter := 1000000;
  WriteLn('Calcul 32 bits natif : ', Counter * 2);
  {$ENDIF}
end;
```

### 3.3 Optimisation des Boucles

Les boucles doivent être optimisées différemment selon l'architecture :

```pascal
procedure OptimizeLoops;  
var
  Data: array[0..999] of Integer;
  i: Integer;
  Sum: Int64;
begin
  // Initialisation
  for i := 0 to High(Data) do
    Data[i] := i;

  // ❌ Mauvais : calculs redondants
  Sum := 0;
  for i := 0 to High(Data) do
    if i < Length(Data) then  // Length calculé à chaque itération !
      Sum := Sum + Data[i];

  // ✅ Bon : calcul en dehors de la boucle
  Sum := 0;
  for i := 0 to High(Data) do
    Sum := Sum + Data[i];

  // ✅ Meilleur : déroulage de boucle (loop unrolling)
  Sum := 0;
  i := 0;
  while i <= High(Data) - 3 do
  begin
    Sum := Sum + Data[i] + Data[i+1] + Data[i+2] + Data[i+3];
    Inc(i, 4);
  end;
  // Traiter les éléments restants
  while i <= High(Data) do
  begin
    Sum := Sum + Data[i];
    Inc(i);
  end;

  WriteLn('Somme : ', Sum);
end;
```

---

## 4. Optimisations SIMD (Single Instruction Multiple Data)

### 4.1 Qu'est-ce que SIMD ?

**SIMD** permet de traiter plusieurs données en une seule instruction.

**Analogie simple :** Au lieu de laver une assiette à la fois, vous en lavez 4 simultanément (une dans chaque main pour 2 personnes).

**Exemple :** Additionner 4 nombres en une instruction au lieu de 4.

### 4.2 SSE/AVX sur x86_64 (Intel/AMD)

```pascal
{$IFDEF CPUX86_64}
uses
  Math;

// Addition de 4 floats en parallèle avec SSE
procedure AddArraysSSE(const A, B: array of Single; var Result: array of Single);  
var
  i: Integer;
begin
  // Vérifier que les tableaux sont alignés et de même taille
  Assert(Length(A) = Length(B));
  Assert(Length(A) = Length(Result));
  Assert((Length(A) mod 4) = 0, 'La taille doit être multiple de 4');

  i := 0;
  while i < Length(A) do
  begin
    // Cette boucle peut être optimisée par le compilateur avec SSE
    Result[i]   := A[i]   + B[i];
    Result[i+1] := A[i+1] + B[i+1];
    Result[i+2] := A[i+2] + B[i+2];
    Result[i+3] := A[i+3] + B[i+3];
    Inc(i, 4);
  end;
end;

// Version avec assembleur inline SSE (avancé)
procedure AddArraysSSE_ASM(const A, B: array of Single; var Result: array of Single);  
var
  Count: Integer;
begin
  Count := Length(A);

  asm
    mov rax, A        // Pointeur vers A
    mov rbx, B        // Pointeur vers B
    mov rcx, Result   // Pointeur vers Result
    mov rdx, Count    // Nombre d'éléments
    shr rdx, 2        // Diviser par 4 (on traite 4 éléments à la fois)

  @Loop:
    movups xmm0, [rax]      // Charger 4 floats de A
    movups xmm1, [rbx]      // Charger 4 floats de B
    addps xmm0, xmm1        // Additionner les 4 floats en parallèle
    movups [rcx], xmm0      // Stocker le résultat

    add rax, 16             // Avancer de 16 octets (4 floats)
    add rbx, 16
    add rcx, 16

    dec rdx
    jnz @Loop
  end;
end;
{$ENDIF}
```

### 4.3 NEON sur ARM

```pascal
{$IFDEF CPUARM}
// Addition de 4 floats en parallèle avec NEON
procedure AddArraysNEON(const A, B: array of Single; var Result: array of Single);  
var
  i: Integer;
begin
  Assert(Length(A) = Length(B));
  Assert(Length(A) = Length(Result));
  Assert((Length(A) mod 4) = 0);

  i := 0;
  while i < Length(A) do
  begin
    // Le compilateur ARM peut utiliser NEON automatiquement
    Result[i]   := A[i]   + B[i];
    Result[i+1] := A[i+1] + B[i+1];
    Result[i+2] := A[i+2] + B[i+2];
    Result[i+3] := A[i+3] + B[i+3];
    Inc(i, 4);
  end;
end;

// Note : L'assembleur ARM NEON est différent de x86 SSE
// Exemple simplifié (syntaxe conceptuelle)
{
  vld1.32 {q0}, [r0]!   // Charger 4 floats de A
  vld1.32 {q1}, [r1]!   // Charger 4 floats de B
  vadd.f32 q0, q0, q1   // Additionner
  vst1.32 {q0}, [r2]!   // Stocker le résultat
}
{$ENDIF}
```

### 4.4 Version Générique Multi-Architecture

```pascal
type
  TVectorAddProc = procedure(const A, B: array of Single; var Result: array of Single);

var
  VectorAdd: TVectorAddProc;

procedure VectorAdd_Generic(const A, B: array of Single; var Result: array of Single);  
var
  i: Integer;
begin
  for i := 0 to High(A) do
    Result[i] := A[i] + B[i];
end;

procedure InitializeVectorOperations;  
var
  Features: TCPUFeatures;
begin
  Features := DetectCPUFeatures;

  {$IFDEF CPUX86_64}
  if Features.HasSSE then
  begin
    WriteLn('Utilisation de SSE pour les opérations vectorielles');
    VectorAdd := @AddArraysSSE;
  end
  else
  {$ENDIF}
  {$IFDEF CPUARM}
  if Features.HasNEON then
  begin
    WriteLn('Utilisation de NEON pour les opérations vectorielles');
    VectorAdd := @AddArraysNEON;
  end
  else
  {$ENDIF}
  begin
    WriteLn('Utilisation de la version générique');
    VectorAdd := @VectorAdd_Generic;
  end;
end;
```

---

## 5. Optimisations Spécifiques par Architecture

### 5.1 Optimisations x86_64 (Windows/Ubuntu Desktop)

```pascal
{$IFDEF CPUX86_64}
unit OptimizationsX64;

interface

// Utiliser les registres 64 bits efficacement
function FastMultiply64(A, B: Int64): Int64; inline;

// Optimiser les divisions (coûteuses sur x86)
function FastDivideBy10(Value: Integer): Integer; inline;

implementation

function FastMultiply64(A, B: Int64): Int64; inline;  
begin
  // Sur x64, la multiplication 64 bits est native et rapide
  Result := A * B;
end;

function FastDivideBy10(Value: Integer): Integer; inline;  
begin
  // Remplacer la division par une multiplication + shift (plus rapide)
  // 1/10 ≈ 0.1 = 0.000110011... en binaire
  // Approximation : (Value * 13107) shr 17
  Result := (Value * 13107) shr 17;

  // Note : Cette approximation fonctionne pour les valeurs jusqu'à ~65000
  // Pour la division exacte, le compilateur optimise déjà bien
end;

end.
{$ENDIF}
```

### 5.2 Optimisations ARM (Raspberry Pi, Serveurs ARM)

```pascal
{$IFDEF CPUARM}
unit OptimizationsARM;

interface

// Les architectures ARM préfèrent les opérations simples
function OptimizedSum(const Data: array of Integer): Int64;

implementation

function OptimizedSum(const Data: array of Integer): Int64;  
var
  i: Integer;
  Sum1, Sum2, Sum3, Sum4: Int64;
begin
  // Déroulage de boucle pour minimiser les branchements
  // ARM a un pipeline plus court qu'x86
  Sum1 := 0;
  Sum2 := 0;
  Sum3 := 0;
  Sum4 := 0;

  i := 0;
  while i <= High(Data) - 3 do
  begin
    Sum1 := Sum1 + Data[i];
    Sum2 := Sum2 + Data[i+1];
    Sum3 := Sum3 + Data[i+2];
    Sum4 := Sum4 + Data[i+3];
    Inc(i, 4);
  end;

  // Éléments restants
  while i <= High(Data) do
  begin
    Sum1 := Sum1 + Data[i];
    Inc(i);
  end;

  Result := Sum1 + Sum2 + Sum3 + Sum4;
end;

end.
{$ENDIF}
```

### 5.3 Optimisations 32 bits vs 64 bits

```pascal
unit ArchOptimizations;

interface

// Fonction qui s'adapte à l'architecture
function OptimalHashCode(const S: string): NativeUInt;

implementation

function OptimalHashCode(const S: string): NativeUInt;  
var
  i: Integer;
begin
  Result := 0;

  {$IFDEF CPU64}
  // Sur 64 bits, utiliser des opérations 64 bits
  for i := 1 to Length(S) do
    Result := Result * 31 + Ord(S[i]);
  {$ELSE}
  // Sur 32 bits, rester en 32 bits pour éviter les émulations coûteuses
  for i := 1 to Length(S) do
    Result := (Result shl 5) - Result + Ord(S[i]);  // Result * 31 + S[i]
  {$ENDIF}
end;

end.
```

---

## 6. Compilation Multi-Architecture

### 6.1 Options du Compilateur

Optimisations par défaut :

```bash
# Optimisation niveau 3 (maximum)
fpc -O3 program.pas

# Optimisations spécifiques x86_64
fpc -O3 -CpCOREAVX2 -CfAVX2 program.pas

# Pour ARM
fpc -O3 -CpARMV7A program.pas

# Pour Raspberry Pi 4 (ARM Cortex-A72)
fpc -O3 -CpARMV8 -CfVFPV3 program.pas
```

### 6.2 Directives d'Optimisation dans le Code

```pascal
{$OPTIMIZATION LEVEL3}        // Niveau d'optimisation maximum
{$SMARTLINK ON}               // Élimine le code mort
{$INLINE ON}                  // Active l'inlining
{$OVERFLOWCHECKS OFF}         // Désactive vérifications overflow (Release)
{$RANGECHECKS OFF}            // Désactive vérifications range (Release)

program OptimizedProgram;

{$mode objfpc}{$H+}

// Inline pour petites fonctions critiques
function Square(X: Integer): Integer; inline;  
begin
  Result := X * X;
end;

// Register pour variables très utilisées
procedure HotLoop;  
var
  i: Integer register;  // Suggère de garder en registre
  Sum: Int64 register;
begin
  Sum := 0;
  for i := 1 to 1000000 do
    Sum := Sum + Square(i);
  WriteLn(Sum);
end;

begin
  HotLoop;
end.
```

### 6.3 Compilation Croisée (Cross-Compilation)

**Depuis Windows vers ARM (Raspberry Pi) :**

```batch
REM Installer le cross-compiler ARM  
REM Télécharger depuis https://www.freepascal.org/down/arm/

REM Compiler pour ARM Linux  
fpc -Tlinux -Parm -CpARMV7A -O3 program.pas

REM Transférer vers Raspberry Pi  
scp program pi@192.168.1.100:~/
```

**Depuis Ubuntu x64 vers ARM :**

```bash
# Installer cross-compiler
sudo apt-get install fpc-arm-linux

# Compiler
fpc -Tlinux -Parm -CpARMV7A -O3 program.pas

# Transférer
scp program pi@raspberrypi.local:~/
```

---

## 7. Tests de Performance Multi-Architecture

### 7.1 Framework de Test

```pascal
unit ArchBenchmark;

interface

type
  TArchInfo = record
    Name: string;
    Is64Bit: Boolean;
    HasSIMD: Boolean;
    PointerSize: Integer;
  end;

procedure RunArchBenchmark;  
function GetArchInfo: TArchInfo;

implementation

uses
  SysUtils, DateUtils;

function GetArchInfo: TArchInfo;  
begin
  Result.PointerSize := SizeOf(Pointer);
  Result.Is64Bit := Result.PointerSize = 8;

  {$IFDEF CPUX86_64}
  Result.Name := 'x86_64 (Intel/AMD 64 bits)';
  Result.HasSIMD := True;  // SSE au minimum
  {$ENDIF}

  {$IFDEF CPUI386}
  Result.Name := 'i386 (Intel/AMD 32 bits)';
  Result.HasSIMD := False;
  {$ENDIF}

  {$IFDEF CPUARM}
  Result.Name := 'ARM 32 bits';
  Result.HasSIMD := True;  // NEON sur ARM moderne
  {$ENDIF}

  {$IFDEF CPUAARCH64}
  Result.Name := 'ARM 64 bits (AArch64)';
  Result.HasSIMD := True;
  {$ENDIF}
end;

procedure BenchmarkIntegerOperations;  
var
  i: Integer;
  Sum: Int64;
  StartTime: TDateTime;
begin
  StartTime := Now;
  Sum := 0;

  for i := 1 to 100000000 do
    Sum := Sum + i;

  WriteLn('Opérations entières : ', MilliSecondsBetween(Now, StartTime), ' ms');
  WriteLn('Résultat : ', Sum);
end;

procedure BenchmarkFloatOperations;  
var
  i: Integer;
  Sum: Double;
  StartTime: TDateTime;
begin
  StartTime := Now;
  Sum := 0.0;

  for i := 1 to 10000000 do
    Sum := Sum + Sqrt(i);

  WriteLn('Opérations flottantes : ', MilliSecondsBetween(Now, StartTime), ' ms');
  WriteLn('Résultat : ', Sum:0:2);
end;

procedure RunArchBenchmark;  
var
  Info: TArchInfo;
begin
  Info := GetArchInfo;

  WriteLn('╔════════════════════════════════════════════════════╗');
  WriteLn('║  BENCHMARK MULTI-ARCHITECTURE                      ║');
  WriteLn('╚════════════════════════════════════════════════════╝');
  WriteLn;
  WriteLn('Architecture : ', Info.Name);
  WriteLn('64 bits      : ', Info.Is64Bit);
  WriteLn('SIMD         : ', Info.HasSIMD);
  WriteLn('Taille ptr   : ', Info.PointerSize, ' octets');
  WriteLn;
  WriteLn('--- Tests de performance ---');
  WriteLn;

  BenchmarkIntegerOperations;
  BenchmarkFloatOperations;
end;

end.
```

### 7.2 Comparaison des Résultats

Tableau de résultats typiques :

| Architecture | Opérations Int | Opérations Float | Mémoire | Notes |
|--------------|----------------|------------------|---------|-------|
| x86_64 (i7)  | 450 ms         | 890 ms           | Rapide  | Excellent |
| ARM64 (RPi4) | 1200 ms        | 1800 ms          | Moyen   | Efficace |
| x86 (Atom)   | 2100 ms        | 3200 ms          | Lent    | Ancien |

---

## 8. Bonnes Pratiques Multi-Architecture

### ✅ À Faire

1. **Utiliser des types adaptatifs**
```pascal
// ✅ Bon
var
  Index: NativeInt;  // S'adapte automatiquement
  Size: SizeUInt;
```

2. **Tester sur toutes les architectures cibles**
```pascal
// Créer des profils de test
{$IFDEF CPUX86_64}
  RunX64Tests;
{$ENDIF}
{$IFDEF CPUARM}
  RunARMTests;
{$ENDIF}
```

3. **Aligner les structures critiques**
```pascal
type
  TCriticalData = record
    Values: array[0..3] of Single;
  end align 16;  // Alignement optimal pour SIMD
```

4. **Préférer les opérations natives**
```pascal
{$IFDEF CPU64}
// Sur 64 bits, Int64 est natif
function ProcessLarge(Value: Int64): Int64;  
begin
  Result := Value * 2;
end;
{$ELSE}
// Sur 32 bits, préférer Integer
function ProcessLarge(Value: Integer): Integer;  
begin
  Result := Value * 2;
end;
{$ENDIF}
```

5. **Optimiser les accès mémoire**
```pascal
// ✅ Bon : accès séquentiel (cache-friendly)
for i := 0 to High(Matrix) do
  for j := 0 to High(Matrix[i]) do
    Process(Matrix[i][j]);

// ❌ Mauvais : accès non séquentiel
for j := 0 to High(Matrix[0]) do
  for i := 0 to High(Matrix) do
    Process(Matrix[i][j]);  // Sauts dans la mémoire
```

6. **Documenter les dépendances architecturales**
```pascal
{
  Cette fonction utilise SSE2 sur x86_64
  et NEON sur ARM pour accélérer le traitement.

  Performance attendue :
  - x86_64 (SSE2) : ~50ms pour 1M éléments
  - ARM (NEON)    : ~120ms pour 1M éléments
  - Générique     : ~400ms pour 1M éléments
}
procedure OptimizedProcess(Data: PData; Count: Integer);
```

### ❌ À Éviter

1. **Supposer une taille de pointeur fixe**
```pascal
// ❌ Mauvais
var
  P: Integer;  // Impossible de stocker un pointeur sur x64 !

// ✅ Bon
var
  P: PtrInt;   // S'adapte à l'architecture
```

2. **Ignorer l'alignement**
```pascal
// ❌ Mauvais : peut causer des erreurs sur ARM
type
  TBadAlign = packed record
    A: Byte;
    B: Int64;  // Pas aligné sur 8 octets !
  end;

// ✅ Bon
type
  TGoodAlign = record
    B: Int64;
    A: Byte;
  end;
```

3. **Utiliser des inline assembleur non portable**
```pascal
// ❌ Mauvais : fonctionne uniquement sur x86
function GetValue: Integer;  
asm
  mov eax, 42
end;

// ✅ Bon : version conditionnelle
function GetValue: Integer;  
begin
  {$IFDEF CPUX86_64}
  asm
    mov eax, 42
  end;
  {$ELSE}
  Result := 42;
  {$ENDIF}
end;
```

4. **Négliger la compilation croisée**
```pascal
// ❌ Mauvais : chemin codé en dur
const
  DataPath = 'C:\Data\';  // Ne fonctionne pas sur Linux !

// ✅ Bon : chemin adaptatif
const
  {$IFDEF WINDOWS}
  DataPath = 'C:\Data\';
  {$ELSE}
  DataPath = '/home/user/data/';
  {$ENDIF}
```

---

## 9. Cas Pratiques d'Optimisation

### 9.1 Traitement d'Images

```pascal
unit ImageProcessing;

{$mode objfpc}{$H+}

interface

type
  TRGB = packed record
    R, G, B: Byte;
  end;
  TImageData = array of array of TRGB;

// Convertir en niveaux de gris
procedure ConvertToGrayscale(var Image: TImageData);

implementation

{$IFDEF CPUX86_64}
// Version optimisée SSE pour x86_64
procedure ConvertToGrayscale_SSE(var Image: TImageData);  
var
  i, j: Integer;
  Gray: Byte;
begin
  for i := 0 to High(Image) do
    for j := 0 to High(Image[i]) do
    begin
      // Formule : Gray = 0.299*R + 0.587*G + 0.114*B
      // Approximation rapide : Gray = (R + R + R + B + G + G + G + G) shr 3
      Gray := ((Image[i][j].R shl 1) + Image[i][j].R +
               Image[i][j].B +
               (Image[i][j].G shl 2)) shr 3;

      Image[i][j].R := Gray;
      Image[i][j].G := Gray;
      Image[i][j].B := Gray;
    end;
end;
{$ENDIF}

{$IFDEF CPUARM}
// Version optimisée pour ARM
procedure ConvertToGrayscale_ARM(var Image: TImageData);  
var
  i, j: Integer;
  Gray: Byte;
begin
  // ARM préfère les opérations simples
  for i := 0 to High(Image) do
    for j := 0 to High(Image[i]) do
    begin
      Gray := (Image[i][j].R + Image[i][j].G + Image[i][j].B) div 3;
      Image[i][j].R := Gray;
      Image[i][j].G := Gray;
      Image[i][j].B := Gray;
    end;
end;
{$ENDIF}

// Version générique portable
procedure ConvertToGrayscale_Generic(var Image: TImageData);  
var
  i, j: Integer;
  Gray: Byte;
begin
  for i := 0 to High(Image) do
    for j := 0 to High(Image[i]) do
    begin
      // Formule standard
      Gray := Round(0.299 * Image[i][j].R +
                    0.587 * Image[i][j].G +
                    0.114 * Image[i][j].B);
      Image[i][j].R := Gray;
      Image[i][j].G := Gray;
      Image[i][j].B := Gray;
    end;
end;

procedure ConvertToGrayscale(var Image: TImageData);  
begin
  {$IFDEF CPUX86_64}
  ConvertToGrayscale_SSE(Image);
  {$ELSE}
  {$IFDEF CPUARM}
  ConvertToGrayscale_ARM(Image);
  {$ELSE}
  ConvertToGrayscale_Generic(Image);
  {$ENDIF}
  {$ENDIF}
end;

end.
```

### 9.2 Calcul Matriciel

```pascal
unit MatrixOperations;

{$mode objfpc}{$H+}

interface

type
  TMatrix = array of array of Double;

// Multiplication de matrices optimisée
procedure MultiplyMatrices(const A, B: TMatrix; var Result: TMatrix);

implementation

// Version optimisée pour cache (tiling/blocking)
procedure MultiplyMatrices_Tiled(const A, B: TMatrix; var Result: TMatrix);  
const
  BLOCK_SIZE = 64;  // Optimisé pour le cache L1
var
  i, j, k, ii, jj, kk: Integer;
  Sum: Double;
  N: Integer;
begin
  N := Length(A);

  // Initialiser résultat
  SetLength(Result, N, N);
  for i := 0 to N-1 do
    for j := 0 to N-1 do
      Result[i][j] := 0;

  // Multiplication par blocs (cache-friendly)
  for ii := 0 to N-1 by BLOCK_SIZE do
    for jj := 0 to N-1 by BLOCK_SIZE do
      for kk := 0 to N-1 by BLOCK_SIZE do
        // Traiter un bloc
        for i := ii to Min(ii + BLOCK_SIZE - 1, N-1) do
          for j := jj to Min(jj + BLOCK_SIZE - 1, N-1) do
          begin
            Sum := Result[i][j];
            for k := kk to Min(kk + BLOCK_SIZE - 1, N-1) do
              Sum := Sum + A[i][k] * B[k][j];
            Result[i][j] := Sum;
          end;
end;

// Version SIMD pour x86_64
{$IFDEF CPUX86_64}
procedure MultiplyMatrices_SIMD(const A, B: TMatrix; var Result: TMatrix);  
var
  i, j, k: Integer;
  N: Integer;
begin
  N := Length(A);
  SetLength(Result, N, N);

  // Le compilateur peut vectoriser cette boucle avec -O3
  for i := 0 to N-1 do
    for j := 0 to N-1 do
    begin
      Result[i][j] := 0;
      for k := 0 to N-1 do
        Result[i][j] := Result[i][j] + A[i][k] * B[k][j];
    end;
end;
{$ENDIF}

procedure MultiplyMatrices(const A, B: TMatrix; var Result: TMatrix);  
begin
  {$IFDEF CPUX86_64}
  // Sur x86_64, la version SIMD + tiling est la plus rapide
  MultiplyMatrices_Tiled(A, B, Result);
  {$ELSE}
  // Sur ARM, la version tiled simple suffit
  MultiplyMatrices_Tiled(A, B, Result);
  {$ENDIF}
end;

end.
```

### 9.3 Compression de Données

```pascal
unit Compression;

interface

type
  TByteArray = array of Byte;

// Compression RLE (Run-Length Encoding) optimisée
function CompressRLE(const Data: TByteArray): TByteArray;

implementation

function CompressRLE(const Data: TByteArray): TByteArray;  
var
  i, OutPos: Integer;
  Count: Byte;
  CurrentByte: Byte;
  TempResult: TByteArray;
begin
  SetLength(TempResult, Length(Data) * 2);  // Taille max
  OutPos := 0;
  i := 0;

  while i < Length(Data) do
  begin
    CurrentByte := Data[i];
    Count := 1;

    // Compter les répétitions (max 255)
    {$IFDEF CPU64}
    // Sur 64 bits, dérouler la boucle pour meilleures performances
    while (i + Count < Length(Data)) and
          (Data[i + Count] = CurrentByte) and
          (Count < 255) do
    begin
      Inc(Count);
      // Le compilateur peut optimiser ceci efficacement sur x64
    end;
    {$ELSE}
    // Sur 32 bits, version simple
    while (i + Integer(Count) < Length(Data)) and
          (Data[i + Integer(Count)] = CurrentByte) and
          (Count < 255) do
      Inc(Count);
    {$ENDIF}

    // Écrire le compte et la valeur
    TempResult[OutPos] := Count;
    TempResult[OutPos + 1] := CurrentByte;
    Inc(OutPos, 2);
    Inc(i, Count);
  end;

  // Copier uniquement la partie utilisée
  SetLength(Result, OutPos);
  Move(TempResult[0], Result[0], OutPos);
end;

end.
```

---

## 10. Profiling Multi-Architecture

### 10.1 Outils de Profiling par Plateforme

#### Sur Windows x86_64

```pascal
unit WindowsProfiling;

{$IFDEF WINDOWS}
interface

uses
  Windows;

type
  TWindowsProfiler = class
  private
    FFrequency: Int64;
    FStartCounter: Int64;
  public
    constructor Create;
    procedure Start;
    function ElapsedMicroseconds: Int64;
  end;

implementation

constructor TWindowsProfiler.Create;  
begin
  QueryPerformanceFrequency(FFrequency);
end;

procedure TWindowsProfiler.Start;  
begin
  QueryPerformanceCounter(FStartCounter);
end;

function TWindowsProfiler.ElapsedMicroseconds: Int64;  
var
  Counter: Int64;
begin
  QueryPerformanceCounter(Counter);
  Result := ((Counter - FStartCounter) * 1000000) div FFrequency;
end;

{$ENDIF}
end.
```

#### Sur Ubuntu ARM (Raspberry Pi)

```bash
# Profiling avec perf sur ARM
sudo perf record -g ./myprogram  
sudo perf report

# Profiling des cycles CPU
sudo perf stat -e cycles,instructions ./myprogram

# Profiling du cache
sudo perf stat -e cache-references,cache-misses ./myprogram
```

### 10.2 Benchmark Multi-Architecture Complet

```pascal
program ArchBenchmarkComplete;

{$mode objfpc}{$H+}

uses
  SysUtils, DateUtils;

type
  TBenchmarkResult = record
    Name: string;
    TimeMs: Int64;
    Architecture: string;
  end;

var
  Results: array of TBenchmarkResult;

procedure AddResult(const Name: string; TimeMs: Int64);  
var
  Idx: Integer;
begin
  Idx := Length(Results);
  SetLength(Results, Idx + 1);
  Results[Idx].Name := Name;
  Results[Idx].TimeMs := TimeMs;

  {$IFDEF CPUX86_64}
  Results[Idx].Architecture := 'x86_64';
  {$ENDIF}
  {$IFDEF CPUARM}
  Results[Idx].Architecture := 'ARM32';
  {$ENDIF}
  {$IFDEF CPUAARCH64}
  Results[Idx].Architecture := 'ARM64';
  {$ENDIF}
end;

procedure BenchmarkIntegerMath;  
var
  i: Integer;
  Sum: Int64;
  Start: TDateTime;
begin
  Start := Now;
  Sum := 0;

  for i := 1 to 100000000 do
    Sum := Sum + i;

  AddResult('Integer Math', MilliSecondsBetween(Now, Start));
end;

procedure BenchmarkFloatingPoint;  
var
  i: Integer;
  Sum: Double;
  Start: TDateTime;
begin
  Start := Now;
  Sum := 0.0;

  for i := 1 to 10000000 do
    Sum := Sum + Sqrt(i);

  AddResult('Floating Point', MilliSecondsBetween(Now, Start));
end;

procedure BenchmarkMemoryAccess;  
var
  Data: array[0..999999] of Integer;
  i: Integer;
  Sum: Int64;
  Start: TDateTime;
begin
  // Initialisation
  for i := 0 to High(Data) do
    Data[i] := i;

  Start := Now;
  Sum := 0;

  // Accès séquentiel
  for i := 0 to High(Data) do
    Sum := Sum + Data[i];

  AddResult('Memory Access', MilliSecondsBetween(Now, Start));
end;

procedure BenchmarkStringOperations;  
var
  S: string;
  i: Integer;
  Start: TDateTime;
begin
  Start := Now;
  S := '';

  for i := 1 to 10000 do
    S := S + 'X';

  AddResult('String Concat', MilliSecondsBetween(Now, Start));
end;

procedure PrintResults;  
var
  i: Integer;
begin
  WriteLn;
  WriteLn('╔════════════════════════════════════════════════════════╗');
  WriteLn('║           RÉSULTATS BENCHMARK                          ║');
  WriteLn('╚════════════════════════════════════════════════════════╝');
  WriteLn;

  if Length(Results) > 0 then
    WriteLn('Architecture : ', Results[0].Architecture);

  WriteLn('Compilateur  : FPC ', {$I %FPCVERSION%});
  WriteLn('OS           : ', {$I %FPCTARGETOS%});
  WriteLn;

  for i := 0 to High(Results) do
  begin
    WriteLn(Format('%-20s : %6d ms', [Results[i].Name, Results[i].TimeMs]));
  end;

  WriteLn;
end;

procedure SaveResultsToFile;  
var
  F: TextFile;
  i: Integer;
  FileName: string;
begin
  FileName := Format('benchmark_%s_%s.txt',
    [{$I %FPCTARGETCPU%}, FormatDateTime('yyyymmdd_hhnnss', Now)]);

  AssignFile(F, FileName);
  Rewrite(F);
  try
    WriteLn(F, 'BENCHMARK RESULTS');
    WriteLn(F, '================');
    WriteLn(F);
    WriteLn(F, 'Date         : ', DateTimeToStr(Now));
    WriteLn(F, 'Architecture : ', {$I %FPCTARGETCPU%});
    WriteLn(F, 'OS           : ', {$I %FPCTARGETOS%});
    WriteLn(F, 'Compiler     : FPC ', {$I %FPCVERSION%});
    WriteLn(F);

    for i := 0 to High(Results) do
      WriteLn(F, Format('%-20s : %6d ms', [Results[i].Name, Results[i].TimeMs]));

    WriteLn(F);
    WriteLn('Résultats sauvegardés dans : ', FileName);
  finally
    CloseFile(F);
  end;
end;

begin
  WriteLn('Démarrage des benchmarks...');
  WriteLn;

  BenchmarkIntegerMath;
  BenchmarkFloatingPoint;
  BenchmarkMemoryAccess;
  BenchmarkStringOperations;

  PrintResults;
  SaveResultsToFile;

  WriteLn('Terminé.');
  {$IFDEF WINDOWS}
  WriteLn;
  WriteLn('Appuyez sur Entrée...');
  ReadLn;
  {$ENDIF}
end.
```

---

## 11. Optimisations Avancées

### 11.1 Branch Prediction (Prédiction de Branchement)

Les processeurs modernes tentent de prédire les branchements. Aidez-les !

```pascal
// ❌ Mauvais : branchements imprévisibles
procedure ProcessData_Bad(const Data: array of Integer);  
var
  i: Integer;
begin
  for i := 0 to High(Data) do
  begin
    if Random(2) = 0 then  // Totalement aléatoire !
      DoSomething(Data[i])
    else
      DoSomethingElse(Data[i]);
  end;
end;

// ✅ Bon : branchements prévisibles
procedure ProcessData_Good(const Data: array of Integer);  
var
  i: Integer;
begin
  // Traiter tous les cas "if" ensemble
  for i := 0 to High(Data) do
    if Data[i] > 0 then
      DoSomething(Data[i]);

  // Puis tous les cas "else"
  for i := 0 to High(Data) do
    if Data[i] <= 0 then
      DoSomethingElse(Data[i]);
end;

// ✅ Meilleur : sans branchement (branchless)
function Max_Branchless(A, B: Integer): Integer; inline;  
var
  Diff: Integer;
begin
  Diff := A - B;
  // Astuce : utiliser le bit de signe
  Result := A - (Diff and (Diff shr 31));
end;
```

### 11.2 Cache Optimization (Optimisation du Cache)

```pascal
// ❌ Mauvais : accès non aligné sur les lignes de cache
type
  TBadStructure = record
    A: Byte;
    Padding1: array[0..62] of Byte;  // Gaspillage
    B: Byte;
    Padding2: array[0..62] of Byte;
  end;

// ✅ Bon : données regroupées
type
  TGoodStructure = record
    A, B: Byte;
    // Le compilateur gère le padding automatiquement
  end;

// Principe : False Sharing en multithreading
type
  // ❌ Mauvais : deux threads modifient la même ligne de cache
  TSharedCounters = record
    Counter1: Integer;  // Thread 1
    Counter2: Integer;  // Thread 2 (même ligne de cache !)
  end;

  // ✅ Bon : séparation des lignes de cache
  TIsolatedCounters = record
    Counter1: Integer;
    Padding1: array[0..15] of Integer;  // 64 octets de padding
    Counter2: Integer;
    Padding2: array[0..15] of Integer;
  end;
```

### 11.3 Prefetching (Préchargement)

```pascal
// Suggérer au CPU de précharger des données
{$IFDEF CPUX86_64}
procedure PrefetchData(P: Pointer); inline;  
asm
  prefetchnta [P]  // Précharger sans polluer le cache
end;

procedure ProcessLargeArray(const Data: array of Integer);  
var
  i: Integer;
begin
  for i := 0 to High(Data) - 16 do
  begin
    // Précharger les données futures
    if (i mod 16) = 0 then
      PrefetchData(@Data[i + 16]);

    // Traiter les données actuelles
    ProcessItem(Data[i]);
  end;
end;
{$ENDIF}
```

---

## 12. Compilation et Déploiement Multi-Architecture

### 12.1 Script de Build Universel

**build_multiarch.sh (Linux):**

```bash
#!/bin/bash

echo "=== Build Multi-Architecture ==="

# Configuration
PROJECT="myprogram"  
FPC_FLAGS="-O3 -CX -XX"

# x86_64 Linux
echo "Building for x86_64 Linux..."  
fpc $FPC_FLAGS -Tlinux -Px86_64 $PROJECT.pas -o${PROJECT}_x64

# ARM Linux (Raspberry Pi)
echo "Building for ARM Linux..."  
fpc $FPC_FLAGS -Tlinux -Parm -CpARMV7A $PROJECT.pas -o${PROJECT}_armv7

# ARM64 Linux
echo "Building for ARM64 Linux..."  
fpc $FPC_FLAGS -Tlinux -Paarch64 $PROJECT.pas -o${PROJECT}_arm64

echo "Build terminé!"  
ls -lh ${PROJECT}_*
```

**build_multiarch.bat (Windows):**

```batch
@echo off
echo === Build Multi-Architecture ===

set PROJECT=myprogram  
set FPC_FLAGS=-O3 -CX -XX

echo Building for Windows x64...  
fpc %FPC_FLAGS% -Twin64 -Px86_64 %PROJECT%.pas -o%PROJECT%_win64.exe

echo Building for Windows x86...  
fpc %FPC_FLAGS% -Twin32 -Pi386 %PROJECT%.pas -o%PROJECT%_win32.exe

echo Building for ARM Linux (cross-compile)...  
fpc %FPC_FLAGS% -Tlinux -Parm %PROJECT%.pas -o%PROJECT%_armv7

echo Build terminé!  
dir %PROJECT%_*
```

### 12.2 Makefile Multi-Architecture

```makefile
# Makefile pour compilation multi-architecture

PROJECT = myprogram  
FPC = fpc  
FPC_FLAGS = -O3 -CX -XX -Sd

.PHONY: all clean x64 arm arm64 windows

all: x64 arm

x64:
	@echo "Building for x86_64..."
	$(FPC) $(FPC_FLAGS) -Tlinux -Px86_64 $(PROJECT).pas -o$(PROJECT)_x64

arm:
	@echo "Building for ARM..."
	$(FPC) $(FPC_FLAGS) -Tlinux -Parm -CpARMV7A $(PROJECT).pas -o$(PROJECT)_arm

arm64:
	@echo "Building for ARM64..."
	$(FPC) $(FPC_FLAGS) -Tlinux -Paarch64 $(PROJECT).pas -o$(PROJECT)_arm64

windows:
	@echo "Building for Windows x64..."
	$(FPC) $(FPC_FLAGS) -Twin64 -Px86_64 $(PROJECT).pas -o$(PROJECT).exe

clean:
	rm -f $(PROJECT)_* *.o *.ppu

test: all
	@echo "Testing x64 binary..."
	./$(PROJECT)_x64
```

---

## 13. Tests et Validation

### 13.1 Suite de Tests Multi-Architecture

```pascal
unit ArchTests;

{$mode objfpc}{$H+}

interface

procedure RunAllTests;

implementation

uses
  SysUtils;

procedure TestPointerSize;  
begin
  WriteLn('Test : Taille des pointeurs');

  {$IFDEF CPU64}
  Assert(SizeOf(Pointer) = 8, 'Erreur : pointeur devrait faire 8 octets');
  WriteLn('  ✓ Pointeurs 64 bits OK');
  {$ELSE}
  Assert(SizeOf(Pointer) = 4, 'Erreur : pointeur devrait faire 4 octets');
  WriteLn('  ✓ Pointeurs 32 bits OK');
  {$ENDIF}
end;

procedure TestIntegerOperations;  
var
  A, B, C: NativeInt;
begin
  WriteLn('Test : Opérations entières');

  A := 1000000;
  B := 2000000;
  C := A + B;

  Assert(C = 3000000, 'Erreur de calcul');
  WriteLn('  ✓ Addition OK');

  C := B - A;
  Assert(C = 1000000, 'Erreur de calcul');
  WriteLn('  ✓ Soustraction OK');
end;

procedure TestAlignment;  
type
  TAlignedRec = record
    A: Int64;
  end align 8;
var
  Rec: TAlignedRec;
  Addr: PtrUInt;
begin
  WriteLn('Test : Alignement mémoire');

  Addr := PtrUInt(@Rec);
  Assert((Addr mod 8) = 0, 'Erreur : structure mal alignée');
  WriteLn('  ✓ Alignement OK');
end;

procedure RunAllTests;  
begin
  WriteLn('╔════════════════════════════════════════════════════════╗');
  WriteLn('║          SUITE DE TESTS MULTI-ARCHITECTURE             ║');
  WriteLn('╚════════════════════════════════════════════════════════╝');
  WriteLn;
  WriteLn('Architecture : ', {$I %FPCTARGETCPU%});
  WriteLn('OS           : ', {$I %FPCTARGETOS%});
  WriteLn;

  try
    TestPointerSize;
    TestIntegerOperations;
    TestAlignment;

    WriteLn;
    WriteLn('✓ Tous les tests réussis !');
  except
    on E: Exception do
    begin
      WriteLn;
      WriteLn('✗ ÉCHEC : ', E.Message);
    end;
  end;
end;

end.
```

---

## 14. Documentation et Maintenance

### 14.1 Documenter les Optimisations

```pascal
{
  Unit: FastMath
  Purpose: Opérations mathématiques optimisées multi-architecture

  Performance par architecture (relative à générique = 1.0):

  | Fonction    | x86_64 | ARM32 | ARM64 | Générique |
  |-------------|--------|-------|-------|-----------|
  | FastSqrt    | 2.5x   | 1.8x  | 2.1x  | 1.0x      |
  | FastDiv     | 3.0x   | 1.5x  | 1.7x  | 1.0x      |
  | VectorAdd   | 4.0x   | 2.5x  | 3.0x  | 1.0x      |

  Dépendances:
  - x86_64: Requiert SSE2 (présent sur tous les CPU 64 bits)
  - ARM: Utilise NEON si disponible

  Note: Compilez avec -O3 pour de meilleures performances
}
unit FastMath;
```

### 14.2 Matrice de Compatibilité

| Fonction/Méthode | x86 | x86_64 | ARM32 | ARM64 | Notes |
|------------------|-----|--------|-------|-------|-------|
| FastString       | ✓   | ✓      | ✓     | ✓     | Portable |
| SIMD_Add         | ✗   | ✓      | Partiel | ✓  | Requiert SSE2/NEON |
| ASM_Optimize     | ✗   | ✓      | ✗     | ✗     | x64 uniquement |
| Generic_Fallback | ✓   | ✓      | ✓     | ✓     | Toujours disponible |
| CacheOptimize    | ✓   | ✓      | ✓     | ✓     | Adaptatif |
| VectorOps        | ✗   | ✓      | ✓     | ✓     | Auto-sélection |

**Légende :**
- ✓ : Pleinement supporté
- Partiel : Supporté avec limitations
- ✗ : Non supporté
- Auto : Détection automatique

### 14.3 Guide de Migration

```pascal
{
  GUIDE DE MIGRATION VERS ARCHITECTURE CIBLE
  ==========================================

  De x86 vers x86_64:
  -------------------
  1. Remplacer Integer par NativeInt pour l'indexation
  2. Vérifier les casts de pointeurs
  3. Tester l'alignement des structures
  4. Profiter des registres 64 bits

  De x86_64 vers ARM64:
  ---------------------
  1. Remplacer assembleur x86 par code portable
  2. Adapter les optimisations SIMD (SSE → NEON)
  3. Attention aux différences d'endianness
  4. Tester sur matériel réel (Raspberry Pi 4)

  De 32 bits vers 64 bits:
  ------------------------
  1. Utiliser SizeOf(Pointer) au lieu de constantes
  2. Remplacer Cardinal par NativeUInt
  3. Vérifier les calculs d'adresses
  4. Recompiler toutes les dépendances
}
```

---

## 15. Études de Cas Réels

### 15.1 Optimisation d'un Serveur Web

```pascal
unit WebServerOptimized;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Sockets;

type
  TOptimizedWebServer = class
  private
    FPort: Word;
    FThreadCount: Integer;

    {$IFDEF CPUX86_64}
    // Sur x64, utiliser des buffers plus grands
    FBufferSize: Integer;  // 64KB
    {$ELSE}
    // Sur ARM, buffers plus petits pour économiser la mémoire
    FBufferSize: Integer;  // 16KB
    {$ENDIF}

  public
    constructor Create(APort: Word);
    procedure Start;
    procedure Stop;
  end;

implementation

constructor TOptimizedWebServer.Create(APort: Word);  
begin
  FPort := APort;

  // Adapter le nombre de threads selon l'architecture
  {$IFDEF CPUX86_64}
  // Desktop puissant : plus de threads
  FThreadCount := CPUCount * 2;
  FBufferSize := 65536;  // 64KB
  {$ENDIF}

  {$IFDEF CPUARM}
  // ARM embarqué : moins de threads
  FThreadCount := CPUCount;
  FBufferSize := 16384;  // 16KB
  {$ENDIF}

  WriteLn('Serveur Web optimisé');
  WriteLn('  Threads  : ', FThreadCount);
  WriteLn('  Buffer   : ', FBufferSize div 1024, ' KB');
end;

procedure TOptimizedWebServer.Start;  
begin
  WriteLn('Démarrage du serveur sur le port ', FPort);
  // Implémentation du serveur...
end;

procedure TOptimizedWebServer.Stop;  
begin
  WriteLn('Arrêt du serveur');
end;

end.
```

### 15.2 Traitement Vidéo Temps Réel

```pascal
unit VideoProcessing;

{$mode objfpc}{$H+}

interface

type
  TVideoFrame = record
    Width, Height: Integer;
    Data: PByte;
  end;

procedure ProcessFrame(var Frame: TVideoFrame);

implementation

{$IFDEF CPUX86_64}
// Version optimisée SSE pour x64
procedure ProcessFrame_SSE(var Frame: TVideoFrame);  
var
  i, PixelCount: Integer;
  P: PByte;
begin
  PixelCount := Frame.Width * Frame.Height;
  P := Frame.Data;

  // Traitement par blocs de 16 octets (SSE)
  i := 0;
  while i < PixelCount - 15 do
  begin
    // Le compilateur peut vectoriser automatiquement
    // avec les bonnes options (-O3 -CpCOREAVX2)
    Inc(P, 16);
    Inc(i, 16);
  end;

  // Pixels restants
  while i < PixelCount do
  begin
    P^ := P^ div 2;  // Exemple : assombrir
    Inc(P);
    Inc(i);
  end;
end;
{$ENDIF}

{$IFDEF CPUARM}
// Version optimisée NEON pour ARM
procedure ProcessFrame_NEON(var Frame: TVideoFrame);  
var
  i, PixelCount: Integer;
  P: PByte;
begin
  PixelCount := Frame.Width * Frame.Height;
  P := Frame.Data;

  // NEON traite également par blocs
  i := 0;
  while i < PixelCount - 15 do
  begin
    // Traitement optimisé ARM
    Inc(P, 16);
    Inc(i, 16);
  end;

  while i < PixelCount do
  begin
    P^ := P^ div 2;
    Inc(P);
    Inc(i);
  end;
end;
{$ENDIF}

// Version générique
procedure ProcessFrame_Generic(var Frame: TVideoFrame);  
var
  i, PixelCount: Integer;
  P: PByte;
begin
  PixelCount := Frame.Width * Frame.Height;
  P := Frame.Data;

  for i := 0 to PixelCount - 1 do
  begin
    P^ := P^ div 2;
    Inc(P);
  end;
end;

procedure ProcessFrame(var Frame: TVideoFrame);  
begin
  {$IFDEF CPUX86_64}
  ProcessFrame_SSE(Frame);
  {$ELSE}
  {$IFDEF CPUARM}
  ProcessFrame_NEON(Frame);
  {$ELSE}
  ProcessFrame_Generic(Frame);
  {$ENDIF}
  {$ENDIF}
end;

end.
```

### 15.3 Système de Trading Haute Fréquence

```pascal
unit TradingEngine;

{$mode objfpc}{$H+}

interface

type
  TPrice = Int64;  // Prix en centièmes de centime

  TTradingEngine = class
  private
    {$IFDEF CPU64}
    // Sur 64 bits, utiliser des structures plus grandes
    FOrderBookSize: Integer;  // 100000 ordres
    {$ELSE}
    FOrderBookSize: Integer;  // 10000 ordres
    {$ENDIF}

  public
    constructor Create;

    // Opérations critiques optimisées
    function CalculateSpread(Bid, Ask: TPrice): TPrice; inline;
    function ExecuteTrade(Volume: Int64; Price: TPrice): Boolean;
  end;

implementation

constructor TTradingEngine.Create;  
begin
  {$IFDEF CPU64}
  FOrderBookSize := 100000;
  WriteLn('Trading Engine : Mode 64 bits (haute capacité)');
  {$ELSE}
  FOrderBookSize := 10000;
  WriteLn('Trading Engine : Mode 32 bits (capacité standard)');
  {$ENDIF}
end;

function TTradingEngine.CalculateSpread(Bid, Ask: TPrice): TPrice; inline;  
begin
  // Calcul ultra-rapide du spread
  // Sur x64, soustraction 64 bits en une instruction
  Result := Ask - Bid;
end;

function TTradingEngine.ExecuteTrade(Volume: Int64; Price: TPrice): Boolean;  
begin
  // Vérifications rapides
  if (Volume <= 0) or (Price <= 0) then
    Exit(False);

  {$IFDEF CPUX86_64}
  // Sur x64, on peut faire des calculs 64 bits complexes rapidement
  if (Volume * Price) > High(Int64) div 2 then
    Exit(False);  // Prévention overflow
  {$ELSE}
  // Sur 32 bits, être plus prudent avec les grandes valeurs
  if Volume > MaxInt div 1000 then
    Exit(False);
  {$ENDIF}

  // Exécution du trade
  Result := True;
end;

end.
```

---

## 16. Erreurs Courantes et Solutions

### 16.1 Problèmes d'Alignement

```pascal
// ❌ ERREUR : Crash sur ARM avec structure non alignée
type
  TBadPacket = packed record
    Header: Byte;
    DataPointer: Pointer;  // Mal aligné sur ARM !
    Length: Int64;         // Mal aligné !
  end;

// ✅ SOLUTION : Laisser le compilateur gérer l'alignement
type
  TGoodPacket = record
    DataPointer: Pointer;  // Aligné naturellement
    Length: Int64;
    Header: Byte;
  end;

// ✅ ALTERNATIVE : Forcer l'alignement
type
  TAlignedPacket = record
    Header: Byte;
    Padding: array[0..6] of Byte;  // Padding manuel
    DataPointer: Pointer;
    Length: Int64;
  end;
```

### 16.2 Problèmes de Portabilité d'Assembleur

```pascal
// ❌ ERREUR : Assembleur x86 uniquement
function GetCPUCycle: Int64;  
asm
  rdtsc           // Instruction x86 uniquement !
  mov @Result, rax
end;

// ✅ SOLUTION : Versions conditionnelles
function GetCPUCycle: Int64;  
begin
  {$IFDEF CPUX86_64}
  asm
    rdtsc
    shl rdx, 32
    or rax, rdx
    mov @Result, rax
  end;
  {$ELSE}
  // Fallback pour autres architectures
  Result := GetTickCount64;
  {$ENDIF}
end;
```

### 16.3 Problèmes de Tailles de Types

```pascal
// ❌ ERREUR : Suppose que Integer = 4 octets partout
procedure BadFunction;  
var
  Arr: array[0..3] of Byte;
  I: Integer;
begin
  I := 0;
  Move(Arr, I, 4);  // Dangereux si Integer n'est pas 4 octets !
end;

// ✅ SOLUTION : Utiliser SizeOf
procedure GoodFunction;  
var
  Arr: array[0..3] of Byte;
  I: Int32;  // Toujours 4 octets
begin
  I := 0;
  Move(Arr, I, SizeOf(I));
end;
```

### 16.4 Problèmes d'Endianness

```pascal
// ❌ ERREUR : Suppose Little Endian
function ReadInt32(Stream: TStream): Int32;  
begin
  Stream.Read(Result, 4);  // Problème sur Big Endian !
end;

// ✅ SOLUTION : Fonctions de conversion
function ReadInt32(Stream: TStream): Int32;  
begin
  Stream.Read(Result, 4);
  {$IFDEF ENDIAN_BIG}
  Result := SwapEndian(Result);
  {$ENDIF}
end;

// Fonction universelle
function SwapEndian32(Value: LongWord): LongWord; inline;  
begin
  Result := ((Value and $FF) shl 24) or
            ((Value and $FF00) shl 8) or
            ((Value and $FF0000) shr 8) or
            ((Value and $FF000000) shr 24);
end;
```

---

## 17. Outils de Développement Multi-Architecture

### 17.1 Configuration IDE Lazarus

Créer des profils de build pour chaque architecture :

**Project → Project Options → Compiler Options**

**Profil "x86_64 Linux" :**
```
Target OS: Linux  
Target CPU: x86_64  
Custom Options: -O3 -CX -XX -CpCOREAVX
```

**Profil "ARM Raspberry Pi" :**
```
Target OS: Linux  
Target CPU: ARM  
Custom Options: -O3 -CpARMV7A -CfVFPV3
```

**Profil "Windows x64" :**
```
Target OS: Win64  
Target CPU: x86_64  
Custom Options: -O3 -CX -XX
```

### 17.2 Script de Test Multi-Architecture

```bash
#!/bin/bash
# test_all_archs.sh

echo "=== Tests Multi-Architecture ==="

# Fonction de test
test_binary() {
    local arch=$1
    local binary=$2

    echo ""
    echo "Testing $arch..."

    if [ -f "$binary" ]; then
        ./$binary
        if [ $? -eq 0 ]; then
            echo "✓ $arch: PASSED"
        else
            echo "✗ $arch: FAILED"
        fi
    else
        echo "⚠ $arch: Binary not found"
    fi
}

# Compiler pour toutes les architectures
echo "Building for all architectures..."  
make clean  
make all

# Tester chaque binaire
test_binary "x86_64" "myprogram_x64"  
test_binary "ARM" "myprogram_arm"  
test_binary "ARM64" "myprogram_arm64"

echo ""  
echo "=== Tests terminés ==="
```

### 17.3 Intégration Continue (CI/CD)

**Fichier .gitlab-ci.yml :**

```yaml
stages:
  - build
  - test

build_x64:
  stage: build
  image: debian:latest
  before_script:
    - apt-get update
    - apt-get install -y fpc
  script:
    - fpc -O3 -Twin64 -Px86_64 myprogram.pas
  artifacts:
    paths:
      - myprogram
    expire_in: 1 week

build_arm:
  stage: build
  image: debian:latest
  before_script:
    - apt-get update
    - apt-get install -y fpc-arm-linux
  script:
    - fpc -O3 -Tlinux -Parm -CpARMV7A myprogram.pas
  artifacts:
    paths:
      - myprogram
    expire_in: 1 week

test_x64:
  stage: test
  dependencies:
    - build_x64
  script:
    - ./myprogram
  only:
    - master

test_arm:
  stage: test
  tags:
    - arm
  dependencies:
    - build_arm
  script:
    - ./myprogram
  only:
    - master
```

---

## 18. Performance Attendues

### 18.1 Tableau de Performances Typiques

Pour un calcul de **1 million d'itérations** sur différentes architectures :

| Architecture | CPU | Temps (ms) | Rapport | Notes |
|--------------|-----|------------|---------|-------|
| x86_64 (i7-9700K) | 8 cœurs @ 3.6GHz | 150 | 1.0x | Référence |
| x86_64 (Xeon E5) | 12 cœurs @ 2.4GHz | 200 | 1.3x | Serveur |
| ARM64 (RPi 4) | 4 cœurs @ 1.5GHz | 450 | 3.0x | Embarqué |
| ARM32 (RPi 3) | 4 cœurs @ 1.2GHz | 800 | 5.3x | Ancien |
| x86 (Atom) | 2 cœurs @ 1.6GHz | 1200 | 8.0x | Netbook |

### 18.2 Gains d'Optimisation

**Sans optimisation (-O0) vs Optimisé (-O3)** :

| Test | Sans Opt | Avec Opt | Gain |
|------|----------|----------|------|
| Boucles | 1000 ms | 200 ms | 5.0x |
| Math Float | 800 ms | 150 ms | 5.3x |
| Strings | 500 ms | 180 ms | 2.8x |
| SIMD | 400 ms | 80 ms | 5.0x |

**Générique vs Optimisé SIMD** :

| Architecture | Générique | SIMD | Gain |
|--------------|-----------|------|------|
| x86_64 (SSE2) | 400 ms | 100 ms | 4.0x |
| x86_64 (AVX2) | 400 ms | 60 ms | 6.7x |
| ARM (NEON) | 500 ms | 180 ms | 2.8x |

---

## 19. Ressources et Documentation

### 19.1 Documentation Officielle

**FreePascal :**
- [Free Pascal Wiki](https://wiki.freepascal.org)
- [Programmer's Guide](https://www.freepascal.org/docs.html)
- [RTL Reference](https://www.freepascal.org/docs-html/rtl/)

**Architecture Spécifique :**
- [Intel Intrinsics Guide](https://software.intel.com/sites/landingpage/IntrinsicsGuide/)
- [ARM NEON Programming](https://developer.arm.com/architectures/instruction-sets/intrinsics/)
- [x86-64 ABI Documentation](https://wiki.osdev.org/X86-64)

### 19.2 Livres Recommandés

1. **"Computer Architecture: A Quantitative Approach"** - Hennessy & Patterson
2. **"The Art of Multiprocessor Programming"** - Herlihy & Shavit
3. **"Optimizing Compilers for Modern Architectures"** - Allen & Kennedy

### 19.3 Outils Utiles

**Analyse de Performance :**
- **perf** (Linux) : Profiling CPU et cache
- **Intel VTune** : Analyse détaillée x86
- **Valgrind** : Détection de problèmes mémoire
- **gprof** : Profiling GNU

**Émulation et Test :**
- **QEMU** : Émulation multi-architecture
- **Docker** : Containers multi-plateforme
- **WSL2** : Linux sur Windows

---

## 20. Conclusion

### Points Clés à Retenir

🎯 **Architecture Matters**
- Le même code peut avoir des performances très différentes selon l'architecture
- Toujours tester sur les plateformes cibles réelles

📊 **Mesurer, Optimiser, Valider**
1. Profiler pour identifier les goulots
2. Optimiser en fonction de l'architecture
3. Benchmarker pour valider les gains

🔧 **Outils et Techniques**
- Utiliser les types adaptatifs (NativeInt, SizeUInt)
- Exploiter SIMD quand c'est pertinent
- Respecter l'alignement mémoire
- Optimiser pour le cache CPU

⚖️ **Équilibre**
- Portable vs Performant
- Lisibilité vs Optimisation
- Générique vs Spécialisé

### Workflow Recommandé

```
1. Écrire du code portable et lisible
         ↓
2. Profiler sur architectures cibles
         ↓
3. Identifier les 20% qui prennent 80% du temps
         ↓
4. Optimiser ces parties critiques
         ↓
5. Ajouter versions spécialisées (SIMD, ASM)
         ↓
6. Benchmarker et valider
         ↓
7. Documenter les optimisations
```

### Checklist Finale

Avant de déployer sur une nouvelle architecture :

- [ ] Code compilé sans warnings
- [ ] Tests unitaires passés
- [ ] Benchmarks exécutés
- [ ] Profiling effectué
- [ ] Alignement vérifié
- [ ] Endianness pris en compte
- [ ] Tailles de types vérifiées
- [ ] Optimisations documentées
- [ ] Fallback générique disponible
- [ ] Tests sur matériel réel

### Exemple Complet Final

```pascal
program OptimizedApp;

{$mode objfpc}{$H+}
{$OPTIMIZATION LEVEL3}
{$INLINE ON}

uses
  SysUtils, DateUtils;

type
  TArchOptimizer = class
  private
    FArchitecture: string;
    function DetectArchitecture: string;
  public
    constructor Create;
    procedure RunOptimizedCode;
    procedure PrintInfo;
  end;

constructor TArchOptimizer.Create;  
begin
  FArchitecture := DetectArchitecture;
end;

function TArchOptimizer.DetectArchitecture: string;  
begin
  {$IFDEF CPUX86_64}
  Result := 'x86_64 (Intel/AMD 64 bits)';
  {$ENDIF}
  {$IFDEF CPUI386}
  Result := 'i386 (Intel/AMD 32 bits)';
  {$ENDIF}
  {$IFDEF CPUARM}
  Result := 'ARM 32 bits';
  {$ENDIF}
  {$IFDEF CPUAARCH64}
  Result := 'ARM 64 bits (AArch64)';
  {$ENDIF}
end;

procedure TArchOptimizer.PrintInfo;  
begin
  WriteLn('╔════════════════════════════════════════════════════════╗');
  WriteLn('║     APPLICATION OPTIMISÉE MULTI-ARCHITECTURE           ║');
  WriteLn('╚════════════════════════════════════════════════════════╝');
  WriteLn;
  WriteLn('Architecture    : ', FArchitecture);
  WriteLn('Compilateur     : FPC ', {$I %FPCVERSION%});
  WriteLn('OS              : ', {$I %FPCTARGETOS%});
  WriteLn('Taille Pointer  : ', SizeOf(Pointer), ' octets');
  WriteLn('Optimisation    : Niveau 3');
  WriteLn;
end;

procedure TArchOptimizer.RunOptimizedCode;  
var
  i: Integer;
  Sum: Int64;
  StartTime: TDateTime;
begin
  WriteLn('Exécution du code optimisé...');

  StartTime := Now;
  Sum := 0;

  // Code optimisé automatiquement selon l'architecture
  for i := 1 to 100000000 do
    Sum := Sum + i;

  WriteLn('Temps d''exécution : ',
          MilliSecondsBetween(Now, StartTime), ' ms');
  WriteLn('Résultat : ', Sum);
end;

var
  Optimizer: TArchOptimizer;

begin
  Optimizer := TArchOptimizer.Create;
  try
    Optimizer.PrintInfo;
    Optimizer.RunOptimizedCode;

    WriteLn;
    WriteLn('✓ Application terminée avec succès');
  finally
    Optimizer.Free;
  end;

  {$IFDEF WINDOWS}
  WriteLn;
  WriteLn('Appuyez sur Entrée...');
  ReadLn;
  {$ENDIF}
end.
```

---

## 🚀 Prochaines Étapes

Maintenant que vous maîtrisez l'optimisation multi-architecture :

1. **Pratiquez** : Créez des versions optimisées de vos projets existants
2. **Mesurez** : Benchmarkez systématiquement vos optimisations
3. **Approfondissez** : Étudiez l'assembleur de votre architecture cible
4. **Partagez** : Contribuez à la communauté FreePascal/Lazarus
5. **Innovez** : Explorez les nouvelles architectures (RISC-V, Apple Silicon)

**Bonne optimisation ! 🎯**

---

**Note finale :** L'optimisation est un art qui nécessite de l'expérience. Ne vous découragez pas si les premiers résultats ne sont pas spectaculaires. Avec le temps et la pratique, vous développerez une intuition pour savoir où et comment optimiser efficacement vos applications FreePascal/Lazarus sur Windows, Ubuntu, et toutes les autres plateformes !

⏭️ [Comparaison de performance Windows/Linux](/20-optimisation-performance/11-comparaison-performance-windows-linux.md)
