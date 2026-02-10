🔝 Retour au [Sommaire](/SOMMAIRE.md)

# Inline Assembler Multi-architecture (x86, x64, ARM) en FreePascal/Lazarus

## Introduction : Qu'est-ce que l'assembleur inline ?

L'assembleur inline permet d'écrire du code assembleur directement dans votre programme Pascal. C'est comme avoir une "trappe d'accès" qui vous permet de parler directement au processeur dans son propre langage, sans passer par les abstractions de Pascal.

Imaginez Pascal comme un interprète qui traduit vos instructions en langage machine. Avec l'assembleur inline, vous court-circuitez l'interprète et parlez directement à la machine. C'est plus puissant mais aussi plus dangereux !

### Pourquoi utiliser l'assembleur inline ?

1. **Performance extrême** : Pour les sections critiques où chaque cycle compte
2. **Accès au matériel** : Utiliser des instructions spécifiques du processeur
3. **Optimisations manuelles** : Quand vous savez mieux que le compilateur
4. **Instructions spéciales** : SIMD, instructions atomiques, etc.
5. **Compatibilité** : Porter du code assembleur existant

### Les défis du multi-architecture

Chaque architecture de processeur (x86, x64, ARM) a :
- Son propre jeu d'instructions
- Ses propres registres
- Ses propres conventions d'appel
- Sa propre syntaxe

Nous allons apprendre à gérer ces différences proprement.

## Syntaxe de base de l'assembleur inline

### Structure fondamentale

```pascal
procedure SimpleAsmExample;  
begin
  // Code Pascal normal
  WriteLn('Avant assembleur');

  // Bloc assembleur inline
  asm
    // Instructions assembleur ici
    nop  // No Operation - ne fait rien
  end;

  // Retour au Pascal
  WriteLn('Après assembleur');
end;

// Fonction entièrement en assembleur
function AddTwoNumbers(A, B: Integer): Integer; assembler;  
asm
  // Le code assembleur dépend de l'architecture
  {$IFDEF CPUX86}
    // Code pour x86 (32 bits)
    mov eax, A
    add eax, B
    // Le résultat est dans EAX
  {$ENDIF}

  {$IFDEF CPUX64}
    // Code pour x64 (64 bits)
    mov eax, ecx  // Sous Windows x64, premier param dans ECX
    add eax, edx  // Deuxième param dans EDX
  {$ENDIF}
end;
```

### Syntaxes Intel vs AT&T

FreePascal supporte deux syntaxes d'assembleur :

```pascal
// Syntaxe Intel (par défaut dans FreePascal)
{$ASMMODE INTEL}
procedure IntelSyntax;  
asm
  mov eax, ebx      // destination, source
  add eax, 10       // ajoute 10 à eax
  mov [esi], eax    // stocke eax à l'adresse dans esi
end;

// Syntaxe AT&T (utilisée sous Unix/Linux)
{$ASMMODE ATT}
procedure ATTSyntax;  
asm
  movl %ebx, %eax   // source, destination (inversé!)
  addl $10, %eax    // $ pour les constantes
  movl %eax, (%esi) // parenthèses pour déréférencer
end;
```

## Architecture x86 (32 bits)

### Registres x86

```pascal
procedure X86Registers;  
begin
  asm
    // Registres généraux 32 bits
    mov eax, 1    // Accumulateur (souvent pour résultats)
    mov ebx, 2    // Base (préservé entre appels)
    mov ecx, 3    // Compteur (boucles)
    mov edx, 4    // Données

    // Registres d'index et pointeurs
    mov esi, 5    // Source Index
    mov edi, 6    // Destination Index
    mov ebp, 7    // Base Pointer (frame pointer)
    mov esp, 8    // Stack Pointer (NE PAS MODIFIER!)

    // Accès aux parties 16 et 8 bits
    mov ax, 100   // Partie basse 16 bits de EAX
    mov al, 10    // Octet bas de AX
    mov ah, 20    // Octet haut de AX
  end;
end;

// Convention d'appel x86 standard (cdecl)
function MultiplyX86(A, B: Integer): Integer;  
asm
  // Les paramètres sont sur la pile
  mov eax, [ebp+8]   // Premier paramètre (A)
  mov edx, [ebp+12]  // Deuxième paramètre (B)
  imul eax, edx      // Multiplication signée
  // Résultat dans EAX (convention de retour)
end;
```

### Instructions communes x86

```pascal
procedure CommonX86Instructions;  
var
  Value: Integer;
  Arr: array[0..9] of Integer;
begin
  Value := 10;

  asm
    // === Instructions de mouvement ===
    mov eax, 42           // Charge une constante
    mov ebx, Value        // Charge depuis une variable
    mov Value, ecx        // Stocke dans une variable
    lea esi, Arr          // Load Effective Address

    // === Arithmétique ===
    add eax, ebx          // Addition
    sub eax, 5            // Soustraction
    imul eax, ebx         // Multiplication signée
    idiv ebx              // Division signée (quotient dans EAX)
    inc ecx               // Incrémentation
    dec edx               // Décrémentation
    neg eax               // Négation (complément à 2)

    // === Logique ===
    and eax, 0FFh         // ET logique
    or eax, ebx           // OU logique
    xor eax, eax          // XOR (mise à zéro rapide)
    not eax               // NON logique

    // === Décalages ===
    shl eax, 2            // Décalage gauche (×4)
    shr ebx, 1            // Décalage droite (÷2)
    rol eax, 8            // Rotation gauche
    ror ebx, 4            // Rotation droite

    // === Comparaisons et sauts ===
    cmp eax, ebx          // Compare (soustraction sans stocker)
    je @Equal             // Saute si égal
    jne @NotEqual         // Saute si différent
    jg @Greater           // Saute si plus grand (signé)
    jl @Less              // Saute si plus petit (signé)
    jmp @End              // Saut inconditionnel

  @Equal:
    mov ecx, 1
    jmp @End

  @NotEqual:
    mov ecx, 0

  @End:
    // Fin du bloc
  end;
end;
```

### Optimisations x86

```pascal
// Exemple d'optimisation : copie rapide de mémoire
procedure FastMemCopyX86(Dest, Source: Pointer; Size: Integer);  
asm
  push esi
  push edi

  mov esi, Source      // ESI = source
  mov edi, Dest        // EDI = destination
  mov ecx, Size        // ECX = taille

  // Copie par blocs de 4 octets
  shr ecx, 2           // Divise par 4
  rep movsd            // Répète MOVSD ECX fois

  // Copie les octets restants
  mov ecx, Size
  and ecx, 3           // Reste de la division par 4
  rep movsb            // Répète MOVSB ECX fois

  pop edi
  pop esi
end;

// Recherche rapide d'un octet
function FindByteX86(Buffer: PByte; Size: Integer; Value: Byte): Integer;  
asm
  push edi

  mov edi, Buffer      // EDI = buffer
  mov ecx, Size        // ECX = taille
  mov al, Value        // AL = valeur cherchée

  cld                  // Direction flag = forward
  repne scasb          // Scan while not equal

  je @Found
  mov eax, -1          // Non trouvé
  jmp @Exit

@Found:
  mov eax, Size
  sub eax, ecx
  dec eax              // Index de l'élément trouvé

@Exit:
  pop edi
end;
```

## Architecture x64 (64 bits)

### Registres x64

```pascal
procedure X64Registers;  
begin
  asm
    // Registres 64 bits étendus
    mov rax, 1    // Version 64 bits de EAX
    mov rbx, 2    // Version 64 bits de EBX
    mov rcx, 3    // Version 64 bits de ECX
    mov rdx, 4    // Version 64 bits de EDX

    // Nouveaux registres R8-R15
    mov r8, 5
    mov r9, 6
    mov r10, 7
    mov r11, 8
    mov r12, 9    // Préservé entre appels
    mov r13, 10   // Préservé entre appels
    mov r14, 11   // Préservé entre appels
    mov r15, 12   // Préservé entre appels

    // Accès aux différentes tailles
    mov rax, $1234567890ABCDEF  // 64 bits complets
    mov eax, $12345678          // 32 bits bas (efface les 32 hauts!)
    mov ax, $1234               // 16 bits bas
    mov al, $12                 // 8 bits bas

    // Nouveaux registres 8 bits
    mov r8b, 1    // Octet bas de R8
    mov r9w, 2    // Mot (16 bits) bas de R9
    mov r10d, 3   // Double mot (32 bits) bas de R10
  end;
end;
```

### Conventions d'appel x64

```pascal
// Windows x64 (Microsoft ABI)
{$IFDEF WINDOWS}
function AddFourNumbersWin64(A, B, C, D: Int64): Int64;  
asm
  // Paramètres dans : RCX, RDX, R8, R9
  mov rax, rcx    // A dans RCX
  add rax, rdx    // B dans RDX
  add rax, r8     // C dans R8
  add rax, r9     // D dans R9
  // Résultat dans RAX
end;
{$ENDIF}

// Linux/Unix x64 (System V ABI)
{$IFDEF UNIX}
function AddFourNumbersUnix64(A, B, C, D: Int64): Int64;  
asm
  // Paramètres dans : RDI, RSI, RDX, RCX
  mov rax, rdi    // A dans RDI
  add rax, rsi    // B dans RSI
  add rax, rdx    // C dans RDX
  add rax, rcx    // D dans RCX
  // Résultat dans RAX
end;
{$ENDIF}

// Gestion multi-plateforme
function AddNumbersPortable(A, B: Int64): Int64;  
asm
  {$IFDEF WINDOWS}
    mov rax, rcx
    add rax, rdx
  {$ELSE} // Unix/Linux
    mov rax, rdi
    add rax, rsi
  {$ENDIF}
end;
```

### Instructions spécifiques x64

```pascal
procedure X64SpecificInstructions;  
var
  Value64: Int64;
begin
  Value64 := $123456789ABCDEF0;

  asm
    // === Instructions 64 bits ===
    mov rax, Value64          // Charge 64 bits

    // Arithmétique 64 bits
    add rax, rbx
    imul rax, 10

    // Instructions atomiques (thread-safe)
    lock inc qword ptr [Value64]
    lock xadd [Value64], rax  // Exchange and add

    // Instructions de bits
    bsf rcx, rax              // Bit Scan Forward
    bsr rdx, rbx              // Bit Scan Reverse
    popcnt r8, rax            // Count bits set

    // Accès mémoire avec RIP-relative addressing
    lea rax, [rip + Value64]  // Position-independent code

    // Instructions conditionnelles (sans branches)
    cmovz rax, rbx            // Move if zero
    cmovnz rcx, rdx           // Move if not zero
    cmovg r8, r9              // Move if greater
  end;
end;

// Utilisation des instructions SSE en x64
procedure SSEExample;  
var
  AlignedData: array[0..3] of Single;
begin
  AlignedData[0] := 1.0;
  AlignedData[1] := 2.0;
  AlignedData[2] := 3.0;
  AlignedData[3] := 4.0;

  asm
    // SSE est garanti disponible en x64
    lea rax, AlignedData
    movaps xmm0, [rax]       // Charge 4 floats alignés
    addps xmm0, xmm0         // Multiplie par 2
    movaps [rax], xmm0       // Stocke le résultat
  end;
end;
```

## Architecture ARM

### Registres ARM

```pascal
{$IFDEF CPUARM}
procedure ARMRegisters;  
begin
  asm
    // Registres généraux R0-R15
    mov r0, #1      // R0-R3 : arguments et résultats
    mov r1, #2
    mov r2, #3
    mov r3, #4

    mov r4, #5      // R4-R11 : variables locales (préservés)
    mov r5, #6
    mov r6, #7
    mov r7, #8      // Frame pointer dans certains ABI
    mov r8, #9
    mov r9, #10
    mov r10, #11
    mov r11, #12    // Frame pointer dans d'autres ABI

    // Registres spéciaux
    // r12 (IP) : Intra-Procedure scratch
    // r13 (SP) : Stack Pointer
    // r14 (LR) : Link Register (adresse de retour)
    // r15 (PC) : Program Counter
  end;
end;
{$ENDIF}
```

### Instructions ARM de base

```pascal
{$IFDEF CPUARM}
procedure ARMInstructions;  
var
  Value: Integer;
begin
  Value := 10;

  asm
    // === Mouvement de données ===
    mov r0, #42              // Charge une constante immédiate
    ldr r1, =Value           // Charge l'adresse de Value
    ldr r2, [r1]             // Charge la valeur depuis l'adresse
    str r2, [r1]             // Stocke la valeur à l'adresse

    // === Arithmétique ===
    add r0, r1, r2           // r0 = r1 + r2
    sub r3, r4, #10          // r3 = r4 - 10
    mul r5, r6, r7           // r5 = r6 * r7

    // === Instructions avec mise à jour des flags ===
    adds r0, r1, r2          // Addition avec flags
    subs r3, r4, r5          // Soustraction avec flags

    // === Logique ===
    and r0, r1, r2           // ET logique
    orr r3, r4, r5           // OU logique
    eor r6, r7, r8           // XOR
    mvn r9, r10              // NOT (Move Not)

    // === Décalages ===
    lsl r0, r1, #2           // Logical Shift Left
    lsr r2, r3, #1           // Logical Shift Right
    asr r4, r5, #3           // Arithmetic Shift Right
    ror r6, r7, #8           // Rotate Right

    // === Comparaisons et branches ===
    cmp r0, r1               // Compare
    beq label_equal          // Branch if equal
    bne label_not_equal      // Branch if not equal
    bgt label_greater        // Branch if greater than
    blt label_less           // Branch if less than
    b label_end              // Branch always

  label_equal:
    mov r2, #1
    b label_end

  label_not_equal:
    mov r2, #0

  label_end:
    // Continue
  end;
end;
{$ENDIF}
```

### Instructions conditionnelles ARM

Une caractéristique unique d'ARM est l'exécution conditionnelle :

```pascal
{$IFDEF CPUARM}
procedure ARMConditionalExecution;  
var
  A, B, Max: Integer;
begin
  A := 10;
  B := 20;

  asm
    ldr r0, =A
    ldr r1, [r0]         // r1 = A
    ldr r0, =B
    ldr r2, [r0]         // r2 = B

    cmp r1, r2           // Compare A et B

    // Exécution conditionnelle (pas de branches!)
    movgt r3, r1         // Si A > B, Max = A
    movle r3, r2         // Si A <= B, Max = B

    // Autres conditions
    moveq r4, #1         // Move if Equal
    movne r4, #0         // Move if Not Equal
    movge r5, #1         // Move if Greater or Equal
    movlt r5, #0         // Move if Less Than

    // Instructions arithmétiques conditionnelles
    addgt r6, r1, r2     // Add if Greater Than
    sublt r7, r1, r2     // Subtract if Less Than

    ldr r0, =Max
    str r3, [r0]         // Stocke le maximum
  end;
end;
{$ENDIF}
```

### ARM Thumb mode

```pascal
{$IFDEF CPUARM}
// Thumb est un mode 16 bits plus compact
procedure ThumbMode;  
begin
  asm
    .thumb               // Passe en mode Thumb

    // Instructions Thumb (16 bits)
    mov r0, #10
    mov r1, #20
    add r0, r1           // Format plus limité

    .arm                 // Retour en mode ARM (32 bits)
  end;
end;
{$ENDIF}
```

## Instructions SIMD multi-plateformes

### SSE/AVX pour x86/x64

```pascal
{$IFDEF CPUX64}
procedure SIMDVectorAddSSE(const A, B: array of Single; var Result: array of Single);  
var
  I: Integer;
begin
  // Vérifier l'alignement sur 16 octets
  if (PtrUInt(@A[0]) and 15 = 0) and
     (PtrUInt(@B[0]) and 15 = 0) and
     (PtrUInt(@Result[0]) and 15 = 0) then
  begin
    // Version SSE alignée
    I := 0;
    while I <= High(A) - 3 do
    begin
      asm
        mov rax, I
        shl rax, 2              // ×4 pour obtenir l'offset en octets

        lea rcx, A
        movaps xmm0, [rcx+rax]  // Charge 4 floats de A

        lea rcx, B
        movaps xmm1, [rcx+rax]  // Charge 4 floats de B

        addps xmm0, xmm1        // Addition vectorielle

        lea rcx, Result
        movaps [rcx+rax], xmm0  // Stocke 4 résultats
      end;
      Inc(I, 4);
    end;
  end;

  // Traiter les éléments restants
  while I <= High(A) do
  begin
    Result[I] := A[I] + B[I];
    Inc(I);
  end;
end;
{$ENDIF}

// Version AVX pour processeurs plus récents
{$IFDEF CPUX64}
procedure SIMDVectorAddAVX(const A, B: array of Single; var Result: array of Single);  
begin
  {$IFDEF HASAVX}
  asm
    // AVX utilise des registres 256 bits (8 floats)
    vmovaps ymm0, [A]
    vaddps ymm0, ymm0, [B]
    vmovaps [Result], ymm0
  end;
  {$ENDIF}
end;
{$ENDIF}
```

### NEON pour ARM

```pascal
{$IFDEF CPUARM}
procedure SIMDVectorAddNEON(const A, B: array of Single; var Result: array of Single);  
begin
  {$IFDEF HASNEON}
  asm
    // NEON utilise des registres 128 bits
    vld1.32 {q0}, [A]        // Charge 4 floats de A
    vld1.32 {q1}, [B]        // Charge 4 floats de B
    vadd.f32 q2, q0, q1      // Addition vectorielle
    vst1.32 {q2}, [Result]   // Stocke 4 résultats
  end;
  {$ENDIF}
end;
{$ENDIF}
```

## Gestion multi-architecture propre

### Structure de code portable

```pascal
// Fonction optimisée multi-architecture
function FastSquareRoot(Value: Single): Single;  
begin
  {$IFDEF CPUX64}
    {$IFDEF HASSSE}
    asm
      // Version SSE pour x64
      movss xmm0, Value
      sqrtss xmm0, xmm0
      movss Result, xmm0
    end;
    {$ELSE}
    Result := Sqrt(Value);  // Fallback Pascal
    {$ENDIF}
  {$ELSEIF DEFINED(CPUX86)}
    asm
      // Version x87 FPU pour x86
      fld Value
      fsqrt
      fstp Result
    end;
  {$ELSEIF DEFINED(CPUARM)}
    {$IFDEF HASVFP}
    asm
      // Version VFP pour ARM
      vldr s0, Value
      vsqrt.f32 s0, s0
      vstr s0, Result
    end;
    {$ELSE}
    Result := Sqrt(Value);  // Fallback Pascal
    {$ENDIF}
  {$ELSE}
    Result := Sqrt(Value);  // Architecture non supportée
  {$ENDIF}
end;

// Détection des capacités du CPU au runtime
function GetCPUCapabilities: TCPUFeatures;  
begin
  Result := [];

  {$IFDEF CPUX86_64}
  asm
    push rbx

    // CPUID avec EAX=1 pour les features de base
    mov eax, 1
    cpuid

    // Tester les bits de features dans ECX et EDX
    test edx, 1 shl 23
    jz @no_mmx
    bts Result, Ord(cfMMX)
  @no_mmx:

    test edx, 1 shl 25
    jz @no_sse
    bts Result, Ord(cfSSE)
  @no_sse:

    test edx, 1 shl 26
    jz @no_sse2
    bts Result, Ord(cfSSE2)
  @no_sse2:

    test ecx, 1 shl 0
    jz @no_sse3
    bts Result, Ord(cfSSE3)
  @no_sse3:

    test ecx, 1 shl 28
    jz @no_avx
    bts Result, Ord(cfAVX)
  @no_avx:

    pop rbx
  end;
  {$ENDIF}
end;
```

### Macros et helpers pour multi-architecture

```pascal
// Macros pour simplifier le code multi-architecture
{$MACRO ON}

{$IFDEF CPUX64}
  {$IFDEF WINDOWS}
    {$DEFINE PARAM1:=rcx}
    {$DEFINE PARAM2:=rdx}
    {$DEFINE PARAM3:=r8}
    {$DEFINE PARAM4:=r9}
  {$ELSE} // Unix
    {$DEFINE PARAM1:=rdi}
    {$DEFINE PARAM2:=rsi}
    {$DEFINE PARAM3:=rdx}
    {$DEFINE PARAM4:=rcx}
  {$ENDIF}
  {$DEFINE RESULT_REG:=rax}
{$ELSEIF DEFINED(CPUX86)}
  {$DEFINE PARAM1:=[ebp+8]}
  {$DEFINE PARAM2:=[ebp+12]}
  {$DEFINE PARAM3:=[ebp+16]}
  {$DEFINE PARAM4:=[ebp+20]}
  {$DEFINE RESULT_REG:=eax}
{$ELSEIF DEFINED(CPUARM)}
  {$DEFINE PARAM1:=r0}
  {$DEFINE PARAM2:=r1}
  {$DEFINE PARAM3:=r2}
  {$DEFINE PARAM4:=r3}
  {$DEFINE RESULT_REG:=r0}
{$ENDIF}

// Utilisation des macros
function AddTwo(A, B: NativeInt): NativeInt; assembler;  
asm
  {$IFDEF CPUX86}
  mov RESULT_REG, PARAM1
  add RESULT_REG, PARAM2
  {$ELSE}
  mov RESULT_REG, PARAM1
  add RESULT_REG, PARAM2
  {$ENDIF}
end;
```

## Optimisations et bonnes pratiques

### Règles générales

```pascal
// 1. Toujours sauvegarder les registres préservés
procedure SaveRegisters;  
asm
  {$IFDEF CPUX64}
  push rbx
  push rsi
  push rdi
  // Code utilisant rbx, rsi, rdi
  pop rdi
  pop rsi
  pop rbx
  {$ENDIF}

  {$IFDEF CPUX86}
  push ebx
  push esi
  push edi
  // Code utilisant ebx, esi, edi
  pop edi
  pop esi
  pop ebx
  {$ENDIF}
end;

// 2. Alignement mémoire pour performance
type
  {$ALIGN 16}  // Aligne sur 16 octets pour SSE
  TVector4 = record
    X, Y, Z, W: Single;
  end;

// 3. Éviter les dépendances de données
procedure AvoidDependencies;  
asm
  {$IFDEF CPUX64}
  // MAUVAIS : dépendances en chaîne
  mov rax, [mem1]
  add rax, 1         // Attend RAX
  mov [mem2], rax    // Attend RAX

  // BON : instructions indépendantes
  mov rax, [mem1]
  mov rbx, [mem3]    // Peut s'exécuter en parallèle
  add rax, 1
  add rbx, 2         // Peut s'exécuter en parallèle
  mov [mem2], rax
  mov [mem4], rbx
  {$ENDIF}
end;

// 4. Utiliser les instructions appropriées
function CountBits(Value: Cardinal): Integer;  
begin
  {$IFDEF HASPOPCNT}
  asm
    popcnt eax, Value  // Instruction hardware si disponible
  end;
  {$ELSE}
  // Fallback software
  Result := 0;
  while Value <> 0 do
  begin
    Inc(Result);
    Value := Value and (Value - 1);
  end;
  {$ENDIF}
end;
```

### Patterns d'optimisation courants

```pascal
// Multiplication par constantes
function MultiplyBy10(Value: Integer): Integer;  
asm
  {$IFDEF CPUX64}
  mov eax, ecx        // Windows x64
  lea eax, [eax+eax*4]  // ×5
  add eax, eax          // ×2 = ×10 total
  {$ENDIF}
end;

// Division par puissance de 2
function DivideBy8(Value: Integer): Integer;  
asm
  {$IFDEF CPUX64}
  mov eax, ecx
  sar eax, 3          // Shift Arithmetic Right par 3 = division par 8
  {$ENDIF}
  {$IFDEF CPUX86}
  mov eax, Value
  sar eax, 3
  {$ENDIF}
end;

// Mise à zéro rapide
procedure FastZero(var Buffer; Size: Integer);  
asm
  {$IFDEF CPUX64}
  mov rcx, QWORD PTR Buffer
  mov rdx, QWORD PTR Size
  xor rax, rax        // Zéro dans RAX

  // Remplir par blocs de 8 octets
  @loop8:
    cmp rdx, 8
    jl @loop1
    mov [rcx], rax
    add rcx, 8
    sub rdx, 8
    jmp @loop8

  // Octets restants
  @loop1:
    test rdx, rdx
    jz @done
    mov BYTE PTR [rcx], al
    inc rcx
    dec rdx
    jmp @loop1

  @done:
  {$ENDIF}
end;
```

## Débogage du code assembleur

### Techniques de débogage

```pascal
// Utilisation de breakpoints en assembleur
procedure DebugAssembly;  
var
  TestValue: Integer;
begin
  TestValue := 42;

  asm
    {$IFDEF DEBUG}
    int 3               // Breakpoint software (x86/x64)
    {$ENDIF}

    mov eax, TestValue

    // Sauvegarder les valeurs pour inspection
    {$IFDEF DEBUG}
    push eax
    push ebx
    // Appeler une fonction de log
    call DebugPrint
    pop ebx
    pop eax
    {$ENDIF}
  end;
end;

// Vérification des résultats
function TestAssemblyCode: Boolean;  
var
  Input, Output, Expected: Integer;
begin
  Input := 100;
  Expected := 200;

  // Test du code assembleur
  asm
    mov eax, Input
    add eax, eax      // Multiplication par 2
    mov Output, eax
  end;

  Result := (Output = Expected);
  if not Result then
    WriteLn('Erreur : Attendu ', Expected, ', obtenu ', Output);
end;
```

### Monitoring des performances

```pascal
// Mesure de cycles CPU
function MeasureCycles: Int64;  
asm
  {$IFDEF CPUX64}
  rdtsc               // Read Time Stamp Counter
  shl rdx, 32
  or rax, rdx         // Combine EDX:EAX dans RAX
  {$ENDIF}
  {$IFDEF CPUX86}
  rdtsc               // Résultat dans EDX:EAX
  {$ENDIF}
end;

procedure BenchmarkCode;  
var
  StartCycles, EndCycles: Int64;
  Iterations: Integer;
begin
  Iterations := 1000000;

  StartCycles := MeasureCycles;

  // Code à mesurer
  asm
    mov ecx, Iterations
    @loop:
      // Votre code ici
      nop
      dec ecx
      jnz @loop
  end;

  EndCycles := MeasureCycles;

  WriteLn('Cycles par itération : ',
          (EndCycles - StartCycles) div Iterations);
end;
```

## Intégration avec le code Pascal

### Passage de paramètres complexes

```pascal
type
  TMatrix4x4 = array[0..3, 0..3] of Single;
  TVector3 = record
    X, Y, Z: Single;
  end;

// Multiplication matrice-vecteur optimisée
procedure TransformVector(const M: TMatrix4x4; const V: TVector3;
                          var Result: TVector3);
asm
  {$IFDEF CPUX64}
  {$IFDEF HASSSE}
  // Charger le vecteur (ajouter W=1.0)
  movss xmm0, [rdx]TVector3.X
  movss xmm1, [rdx]TVector3.Y
  movss xmm2, [rdx]TVector3.Z
  movss xmm3, DWORD PTR [1.0]

  // Construire le vecteur dans XMM4
  unpcklps xmm0, xmm1
  unpcklps xmm2, xmm3
  movlhps xmm0, xmm2

  // Multiplication avec chaque ligne de la matrice
  movaps xmm1, [rcx]       // Ligne 0
  dpps xmm1, xmm0, 0xF1    // Dot product
  movss [r8]TVector3.X, xmm1

  movaps xmm1, [rcx+16]    // Ligne 1
  dpps xmm1, xmm0, 0xF1
  movss [r8]TVector3.Y, xmm1

  movaps xmm1, [rcx+32]    // Ligne 2
  dpps xmm1, xmm0, 0xF1
  movss [r8]TVector3.Z, xmm1
  {$ENDIF}
  {$ENDIF}
end;

// Appel de fonctions Pascal depuis l'assembleur
function PascalHelper(Value: Integer): Integer;  
begin
  Result := Value * 2 + 10;
end;

procedure CallFromAssembly;  
var
  Input, Output: Integer;
begin
  Input := 5;

  asm
    {$IFDEF CPUX64}
    // Sauvegarder les registres si nécessaire
    push rbx

    // Préparer l'appel
    mov ecx, Input        // Paramètre dans ECX (Windows x64)
    call PascalHelper
    mov Output, eax       // Résultat depuis EAX

    pop rbx
    {$ENDIF}
  end;

  WriteLn('Résultat : ', Output); // Affiche 20
end;
```

### Gestion des exceptions

```pascal
// Protection contre les erreurs en assembleur
procedure SafeAssemblyOperation;  
begin
  try
    asm
      {$IFDEF CPUX64}
      // Code potentiellement dangereux
      mov rax, 0
      // div rax  // Provoquerait une division par zéro
      {$ENDIF}
    end;
  except
    on E: Exception do
      WriteLn('Erreur assembleur : ', E.Message);
  end;
end;

// Vérification avant opérations dangereuses
function SafeDivide(Dividend, Divisor: Integer): Integer;  
asm
  {$IFDEF CPUX64}
  mov eax, ecx          // Dividend
  test edx, edx         // Test si Divisor = 0
  jz @error

  cdq                   // Sign extend EAX dans EDX:EAX
  idiv edx              // Division signée
  jmp @done

  @error:
  mov eax, -1           // Valeur d'erreur

  @done:
  {$ENDIF}
end;
```

## Cas d'usage pratiques

### Cryptographie simple

```pascal
// XOR cipher rapide
procedure XORCipher(var Data; Size: Integer; Key: Byte);  
asm
  {$IFDEF CPUX64}
  mov rcx, QWORD PTR Data
  mov edx, Size
  movzx r8d, Key

  // Construire la clé sur 64 bits
  mov rax, r8
  shl rax, 8
  or rax, r8
  mov r8, rax           // R8 = 0x0000000000000101
  shl rax, 16
  or rax, r8            // RAX = 0x0000000001010101
  mov r8, rax
  shl rax, 32
  or rax, r8            // RAX = 0x0101010101010101

  // XOR par blocs de 8 octets
  @loop8:
    cmp rdx, 8
    jl @loop1
    xor [rcx], rax
    add rcx, 8
    sub rdx, 8
    jmp @loop8

  // Octets restants
  @loop1:
    test rdx, rdx
    jz @done
    xor BYTE PTR [rcx], al
    inc rcx
    dec rdx
    jmp @loop1

  @done:
  {$ENDIF}
end;
```

### Traitement d'images

```pascal
// Conversion RGB vers niveaux de gris
procedure RGBToGrayscale(const Source; var Dest; PixelCount: Integer);
// Formule : Gray = 0.299*R + 0.587*G + 0.114*B
asm
  {$IFDEF CPUX64}
  {$IFDEF HASSSE}
  // Charger les coefficients
  movss xmm4, DWORD PTR [0.299]
  movss xmm5, DWORD PTR [0.587]
  movss xmm6, DWORD PTR [0.114]

  mov rcx, QWORD PTR Source
  mov rdx, QWORD PTR Dest
  mov r8d, PixelCount

  @loop:
    test r8d, r8d
    jz @done

    // Charger RGB (3 octets)
    movzx eax, BYTE PTR [rcx]     // R
    cvtsi2ss xmm0, eax
    mulss xmm0, xmm4

    movzx eax, BYTE PTR [rcx+1]   // G
    cvtsi2ss xmm1, eax
    mulss xmm1, xmm5
    addss xmm0, xmm1

    movzx eax, BYTE PTR [rcx+2]   // B
    cvtsi2ss xmm1, eax
    mulss xmm1, xmm6
    addss xmm0, xmm1

    // Convertir en entier et stocker
    cvtss2si eax, xmm0
    mov BYTE PTR [rdx], al

    add rcx, 3
    inc rdx
    dec r8d
    jmp @loop

  @done:
  {$ENDIF}
  {$ENDIF}
end;
```

### Recherche de motifs

```pascal
// Recherche rapide d'un motif de 4 octets
function FindPattern(const Buffer; Size: Integer; Pattern: Cardinal): Integer;  
asm
  {$IFDEF CPUX64}
  mov r8, rcx           // Buffer
  mov r9d, edx          // Size
  mov r10d, r8d         // Pattern

  xor rax, rax          // Index = 0

  @loop:
    cmp rax, r9
    jge @notfound

    mov edx, [r8+rax]
    cmp edx, r10d
    je @found

    inc rax
    jmp @loop

  @found:
    // RAX contient déjà l'index
    jmp @done

  @notfound:
    mov rax, -1

  @done:
  {$ENDIF}
end;
```

## Recommandations et pièges à éviter

### Pièges courants

```pascal
// 1. Oublier de préserver les registres
procedure BadExample1;  
asm
  {$IFDEF CPUX64}
  // ❌ MAUVAIS : RBX doit être préservé
  mov rbx, 123
  // Utilisation de RBX
  // Pas de restauration !
  {$ENDIF}
end;

procedure GoodExample1;  
asm
  {$IFDEF CPUX64}
  // ✅ BON : Sauvegarde et restauration
  push rbx
  mov rbx, 123
  // Utilisation de RBX
  pop rbx
  {$ENDIF}
end;

// 2. Mauvais alignement de pile
procedure BadExample2;  
asm
  {$IFDEF CPUX64}
  // ❌ MAUVAIS : Pile non alignée sur 16 octets
  push rax         // Pile désalignée
  call SomeFunction
  pop rax
  {$ENDIF}
end;

procedure GoodExample2;  
asm
  {$IFDEF CPUX64}
  // ✅ BON : Maintenir l'alignement
  sub rsp, 8       // Aligner sur 16 octets
  call SomeFunction
  add rsp, 8
  {$ENDIF}
end;

// 3. Confusion entre tailles de registres
procedure BadExample3;  
asm
  {$IFDEF CPUX64}
  // ❌ MAUVAIS : Efface les 32 bits hauts
  mov rax, $FFFFFFFFFFFFFFFF
  mov eax, 1      // RAX = $0000000000000001 maintenant !
  {$ENDIF}
end;

procedure GoodExample3;  
asm
  {$IFDEF CPUX64}
  // ✅ BON : Utiliser la bonne taille
  mov rax, $FFFFFFFFFFFFFFFF
  mov al, 1       // Modifie seulement l'octet bas
  // ou
  and rax, $FFFFFFFFFFFFFF00
  or rax, 1       // Préserve les bits hauts
  {$ENDIF}
end;
```

### Conseils de portabilité

```pascal
// Utiliser des types portables
type
  NativeInt = {$IFDEF CPU64}Int64{$ELSE}Integer{$ENDIF};
  NativeUInt = {$IFDEF CPU64}UInt64{$ELSE}Cardinal{$ENDIF};

// Fonction portable avec fallback
function PortableFunction(Value: NativeInt): NativeInt;  
begin
  {$IF DEFINED(CPUX64) AND DEFINED(HASSSE4)}
    // Version SSE4 optimisée
    asm
      // Code SSE4
    end;
  {$ELSEIF DEFINED(CPUX86)}
    // Version x86
    asm
      // Code x86
    end;
  {$ELSEIF DEFINED(CPUARM) AND DEFINED(HASNEON)}
    // Version ARM NEON
    asm
      // Code NEON
    end;
  {$ELSE}
    // Fallback Pascal pur
    Result := Value * 2;
  {$ENDIF}
end;

// Documentation des dépendances
{
  Cette fonction nécessite :
  - x64: SSE2 minimum, SSE4 pour performance optimale
  - x86: Processeur 586 ou supérieur
  - ARM: VFP ou NEON pour les flottants

  Testé sur :
  - Windows 10 x64 (Intel Core i7)
  - Ubuntu 20.04 x64 (AMD Ryzen)
  - Raspbian (ARM Cortex-A53)
}
procedure DocumentedAssemblyFunction;  
begin
  // ...
end;
```

## Conclusion

L'assembleur inline en FreePascal/Lazarus est un outil puissant pour optimiser les performances et accéder aux fonctionnalités spécifiques du processeur. Les points clés à retenir :

### Avantages
- **Performance maximale** pour les sections critiques
- **Accès direct** aux instructions processeur
- **Contrôle total** sur le code généré
- **SIMD** et autres instructions spécialisées

### Inconvénients
- **Portabilité réduite** entre architectures
- **Maintenance complexe**
- **Débogage difficile**
- **Risques de bugs** subtils

### Bonnes pratiques
1. **Toujours fournir un fallback Pascal** pour la portabilité
2. **Documenter** les prérequis et dépendances
3. **Tester** sur toutes les architectures cibles
4. **Mesurer** les gains de performance réels
5. **Préserver** les registres selon les conventions
6. **Utiliser** les directives conditionnelles appropriées

### Quand utiliser l'assembleur inline
- Boucles critiques avec millions d'itérations
- Traitement de données en masse (images, audio, vidéo)
- Cryptographie et sécurité
- Exploitation d'instructions SIMD
- Interfaçage avec du code système

### Quand l'éviter
- Code rarement exécuté
- Logique métier complexe
- Quand le compilateur optimise déjà bien
- Pour des gains marginaux
- Si la maintenance est prioritaire

L'assembleur inline reste un outil spécialisé. Utilisez-le judicieusement, mesurez toujours les gains, et gardez votre code maintenable avec une documentation claire et des alternatives portables.

⏭️ [Helpers de classe et de record](/03-langage-object-pascal-avance/07-helpers-classe-record.md)
