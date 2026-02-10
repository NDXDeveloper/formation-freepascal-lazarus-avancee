🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 24.3 Optimisations du compilateur

## Introduction

L'**optimisation** est l'art de transformer un programme pour qu'il soit plus rapide, plus compact, ou plus efficace, sans changer son comportement observable. Le compilateur Free Pascal intègre de nombreuses optimisations sophistiquées qui peuvent grandement améliorer les performances de vos applications.

Dans ce chapitre, nous allons explorer :
- Comment fonctionne l'optimisation
- Les différents types d'optimisations dans FPC
- Comment contrôler les optimisations
- Comment écrire du code "optimisable"
- Les pièges et compromis à connaître

> 💡 **Pour les débutants** : Pas de panique si certains concepts semblent complexes. Nous allons les expliquer avec des analogies simples et des exemples concrets !

---

## Qu'est-ce que l'optimisation ?

### Analogie du trajet

Imaginez que vous devez aller du point A au point B :

**Sans optimisation :**
- Vous suivez les instructions au pied de la lettre
- "Allez tout droit, tournez à gauche, faites demi-tour, tournez à droite..."
- Vous arrivez, mais le trajet est long

**Avec optimisation :**
- Vous analysez le trajet global
- Vous supprimez le demi-tour inutile
- Vous combinez certains segments
- Vous arrivez plus vite, par le même chemin final

Le compilateur fait la même chose avec votre code !

### Exemple simple

```pascal
// Code original
var
  x, y, z: Integer;
begin
  x := 5;
  y := 10;
  z := x + y;
  WriteLn(z);
end.
```

**Sans optimisation :**
```asm
; Stocker 5 dans x
mov [x], 5
; Stocker 10 dans y
mov [y], 10
; Charger x
mov eax, [x]
; Charger y
mov ebx, [y]
; Additionner
add eax, ebx
; Stocker dans z
mov [z], eax
; Charger z pour l'affichage
mov eax, [z]  
push eax  
call WriteLn
```

**Avec optimisation :**
```asm
; Le compilateur calcule : 5 + 10 = 15
mov eax, 15  
push eax  
call WriteLn
; x, y, z n'existent même plus !
```

Le code optimisé est **beaucoup plus court et rapide**, mais produit exactement le même résultat !

---

## Niveaux d'optimisation dans FPC

### Options de compilation

FPC propose plusieurs niveaux d'optimisation via l'option `-O` :

| Option | Nom | Description | Utilisation |
|--------|-----|-------------|-------------|
| **-O-** | Aucune optimisation | Code non optimisé, rapide à compiler | Débogage |
| **-O1** | Optimisations de base | Optimisations simples et sûres | Développement |
| **-O2** | Optimisations standard | Bon compromis vitesse/taille | **Recommandé** |
| **-O3** | Optimisations agressives | Optimise davantage, peut augmenter la taille | Production |
| **-O4** | Optimisations maximales | Toutes les optimisations possibles | Performances critiques |

### Utilisation en ligne de commande

```bash
# Sans optimisation (pour déboguer)
fpc -O- myprogram.pas

# Optimisations standard (recommandé)
fpc -O2 myprogram.pas

# Optimisations maximales
fpc -O4 myprogram.pas
```

### Configuration dans Lazarus

Dans l'IDE Lazarus :

1. **Project → Project Options**
2. **Compiler Options → Compilation and Linking**
3. Section **Optimization** :
   - Level: 1, 2, 3, ou 4
   - Cocher les optimisations spécifiques

### Optimisations spécifiques

Vous pouvez activer des optimisations individuelles :

```bash
# Optimiser les registres
fpc -OoREGVAR myprogram.pas

# Optimiser les boucles
fpc -OoLOOPUNROLL myprogram.pas

# Combiner plusieurs optimisations
fpc -O2 -OoREGVAR,LOOPUNROLL myprogram.pas
```

**Liste des optimisations principales :**

- `REGVAR` : Allocation de variables dans les registres
- `STACKFRAME` : Optimisation des frames de pile
- `LOOPUNROLL` : Déroulage de boucles
- `TAILREC` : Optimisation des appels récursifs terminaux
- `CSE` : Élimination de sous-expressions communes
- `DFA` : Analyse de flux de données
- `PEEPHOLE` : Optimisations peephole
- `UNCERTAIN` : Optimisations spéculatives (attention !)

---

## Types d'optimisations

### 1. Optimisations locales (Peephole)

Les **optimisations peephole** examinent de petites séquences d'instructions et les remplacent par des équivalents plus efficaces.

#### Élimination d'instructions redondantes

**Avant :**
```asm
mov eax, ebx  
mov eax, ebx    ; Redondant !
```

**Après :**
```asm
mov eax, ebx
```

#### Simplification algébrique

**Avant :**
```pascal
x := x + 0;  // Ajouter 0 ne fait rien  
y := y * 1;  // Multiplier par 1 ne fait rien  
z := z * 0;  // Multiplier par 0 donne toujours 0
```

**Après :**
```pascal
// x := x + 0  → complètement supprimé
// y := y * 1  → complètement supprimé
z := 0;
```

#### Strength reduction

Remplacer des opérations coûteuses par des équivalents moins coûteux :

**Avant :**
```pascal
x := y * 2;    // Multiplication  
x := y / 2;    // Division  
x := y * 8;    // Multiplication par puissance de 2
```

**Après (code assembleur) :**
```asm
; x := y * 2  →  shift left 1
shl eax, 1

; x := y / 2  →  shift right 1
shr eax, 1

; x := y * 8  →  shift left 3
shl eax, 3
```

Les shifts (décalages) sont **beaucoup plus rapides** que les multiplications et divisions !

#### Utilisation d'instructions spécialisées

**Avant :**
```pascal
x := x + 1;
```

**Après :**
```asm
; Au lieu de : add eax, 1
inc eax        ; Plus court (peut être plus rapide)
```

### 2. Optimisations au niveau des expressions

#### Pliage de constantes (Constant Folding)

Calculer à la compilation ce qui peut l'être :

**Avant :**
```pascal
const
  WIDTH = 800;
  HEIGHT = 600;
var
  area: Integer;
begin
  area := WIDTH * HEIGHT;  // Calculé à la compilation !
end.
```

**Après :**
```pascal
begin
  area := 480000;  // Déjà calculé
end.
```

#### Propagation de constantes (Constant Propagation)

Propager les valeurs constantes à travers le code :

**Avant :**
```pascal
var
  x, y, z: Integer;
begin
  x := 5;
  y := x + 3;    // x est connu = 5
  z := y * 2;    // y est connu = 8
  WriteLn(z);    // z est connu = 16
end.
```

**Après optimisation :**
```pascal
begin
  WriteLn(16);   // Tout calculé !
end.
```

#### Élimination de sous-expressions communes (CSE)

**Avant :**
```pascal
a := b + c;  
d := b + c;      // Recalcule la même chose !  
e := b + c;      // Encore !
```

**Après :**
```pascal
temp := b + c;   // Calculer une fois  
a := temp;  
d := temp;       // Réutiliser  
e := temp;
```

**Code assembleur optimisé :**
```asm
mov eax, [b]  
add eax, [c]     ; Calcul une seule fois  
mov [a], eax  
mov [d], eax     ; Juste copier  
mov [e], eax
```

### 3. Optimisations de flux de contrôle

#### Élimination de code mort (Dead Code Elimination)

Supprimer le code qui ne sera jamais exécuté :

**Avant :**
```pascal
if False then
  DoSomething;   // Ne sera jamais exécuté

if True then
  DoA
else
  DoB;           // DoB ne sera jamais appelé
```

**Après :**
```pascal
// Premier if complètement supprimé
DoA;             // Juste DoA, le if a disparu
```

#### Simplification de conditions

**Avant :**
```pascal
if (x > 5) and (x > 5) then  // Condition dupliquée
  DoSomething;

if x or True then             // Toujours vrai
  DoSomething;

if x and False then           // Toujours faux
  DoSomething;
```

**Après :**
```pascal
if x > 5 then
  DoSomething;

DoSomething;  // Condition toujours vraie

// Code supprimé (condition toujours fausse)
```

#### Fusion de blocs

**Avant :**
```pascal
if condition then
  goto Label1;

Label1:
  DoSomething;
```

**Après :**
```pascal
if condition then
  DoSomething;
// Le goto et le label sont éliminés
```

### 4. Optimisations de boucles

#### Invariant code motion

Sortir de la boucle les calculs qui ne changent pas :

**Avant :**
```pascal
for i := 1 to 1000 do  
begin
  limit := GetMaxValue;  // Ne change pas dans la boucle !
  if data[i] > limit then
    Process(data[i]);
end;
```

**Après :**
```pascal
limit := GetMaxValue;    // Appelé une seule fois  
for i := 1 to 1000 do  
begin
  if data[i] > limit then
    Process(data[i]);
end;
```

**Impact :** Au lieu d'appeler `GetMaxValue` 1000 fois, on l'appelle 1 fois !

#### Loop unrolling (Déroulage de boucles)

Répéter le corps de la boucle plusieurs fois pour réduire le surcoût :

**Avant :**
```pascal
for i := 0 to 3 do
  sum := sum + data[i];
```

**Après déroulage :**
```pascal
sum := sum + data[0];  
sum := sum + data[1];  
sum := sum + data[2];  
sum := sum + data[3];
// La boucle a disparu !
```

**Avantages :**
- Moins de tests et de sauts
- Meilleur pipeline CPU
- Plus rapide

**Inconvénients :**
- Code plus gros
- Ne marche bien que pour petites boucles

#### Loop fusion

Fusionner plusieurs boucles qui parcourent les mêmes données :

**Avant :**
```pascal
for i := 1 to 1000 do
  a[i] := b[i] + c[i];

for i := 1 to 1000 do
  d[i] := a[i] * 2;
```

**Après :**
```pascal
for i := 1 to 1000 do  
begin
  a[i] := b[i] + c[i];
  d[i] := a[i] * 2;      // Dans la même boucle
end;
```

**Avantages :**
- Meilleure utilisation du cache CPU
- Moins de surcoût de boucle
- Plus rapide

### 5. Optimisations de procédures

#### Inlining (Incorporation)

Remplacer l'appel d'une fonction par son code directement :

**Avant :**
```pascal
function Square(x: Integer): Integer; inline;  
begin
  Result := x * x;
end;

var
  y: Integer;
begin
  y := Square(5);
end.
```

**Après inlining :**
```pascal
var
  y: Integer;
begin
  y := 5 * 5;     // Fonction "dépliée" + constant folding
  // Devient : y := 25;
end.
```

**Avantages :**
- Pas de surcoût d'appel de fonction
- Permet d'autres optimisations (ici constant folding)
- Plus rapide

**Inconvénients :**
- Code plus gros si fonction appelée souvent
- Ne marche que pour petites fonctions

**Utilisation :**
```pascal
// Marquer une fonction inline
function FastCalc(x: Integer): Integer; inline;  
begin
  Result := x * 2 + 1;
end;
```

#### Tail call optimization

Optimiser les appels récursifs terminaux :

**Avant (récursion) :**
```pascal
function Factorial(n: Integer): Integer;  
begin
  if n <= 1 then
    Result := 1
  else
    Result := n * Factorial(n - 1);  // Appel récursif
end;
```

**Problème :** Chaque appel consomme de la pile. Pour Factorial(10000), débordement de pile !

**Solution avec accumulation :**
```pascal
function FactorialTail(n, acc: Integer): Integer;  
begin
  if n <= 1 then
    Result := acc
  else
    Result := FactorialTail(n - 1, n * acc);  // Tail call
end;
```

**Après optimisation :**
```pascal
function FactorialTail(n, acc: Integer): Integer;  
label
  Start;
begin  
Start:
  if n <= 1 then
    Result := acc
  else
  begin
    acc := n * acc;   // Transformé en boucle !
    n := n - 1;
    goto Start;
  end;
end;
```

Le compilateur transforme la récursion en boucle : plus de risque de débordement !

#### Dead store elimination

Supprimer les affectations inutiles :

**Avant :**
```pascal
procedure Example;  
var
  x: Integer;
begin
  x := 5;      // Première affectation
  x := 10;     // Écrase la première - la première est inutile !
  WriteLn(x);
end;
```

**Après :**
```pascal
procedure Example;  
var
  x: Integer;
begin
  x := 10;     // Première affectation supprimée
  WriteLn(x);
end;
```

### 6. Optimisations de registres

#### Register allocation

Placer les variables les plus utilisées dans les registres CPU plutôt qu'en mémoire :

**Sans optimisation :**
```pascal
function Calculate(a, b, c: Integer): Integer;  
begin
  Result := (a + b) * c;
end;
```

**Code généré non optimisé :**
```asm
mov eax, [a]     ; Charger depuis mémoire  
add eax, [b]     ; Charger depuis mémoire  
mov ebx, [c]     ; Charger depuis mémoire  
imul eax, ebx  
mov [Result], eax ; Stocker en mémoire
```

**Code optimisé (variables en registres) :**
```asm
; a déjà dans eax, b dans ebx, c dans ecx
add eax, ebx     ; Directement dans les registres !  
imul eax, ecx
; Result déjà dans eax (convention d'appel)
```

**Impact :** Accès registre = **50-100x plus rapide** que l'accès mémoire !

#### Register coalescing

Réduire les copies entre registres :

**Avant :**
```asm
mov eax, ebx  
mov ecx, eax     ; Copie inutile
```

**Après :**
```asm
mov ecx, ebx     ; Copie directe
```

### 7. Optimisations mémoire

#### Array bounds check elimination

Supprimer les vérifications de limites quand elles sont prouvées inutiles :

**Avant (avec vérifications) :**
```pascal
var
  arr: array[1..10] of Integer;
  i: Integer;
begin
  for i := 1 to 10 do
    arr[i] := 0;  // Vérification : i in [1..10] ?
end;
```

**Après optimisation :**
```pascal
// Le compilateur sait que i est toujours dans [1..10]
// Les vérifications sont supprimées
for i := 1 to 10 do
  arr[i] := 0;  // Pas de vérification
```

**Impact :** Environ 10-20% plus rapide pour code intensif en tableaux.

**Contrôle manuel :**
```pascal
{$R+}  // Range checking ON (mode debug)
{$R-}  // Range checking OFF (mode release)
```

#### String optimizations

Optimisations spécifiques aux chaînes :

**Copy-on-write :**
```pascal
var
  s1, s2: String;
begin
  s1 := 'Hello World';
  s2 := s1;            // Pas de copie ! Juste une référence
  // s2 partage la même mémoire que s1

  s2 := s2 + '!';      // MAINTENANT il y a copie (modification)
end;
```

**Short string optimization :**
```pascal
var
  s: String[15];  // ShortString - pas d'allocation heap
begin
  s := 'Hello';   // Très rapide, pas de gestion mémoire
end;
```

### 8. Optimisations de branchements

#### Branch prediction hints

Aider le CPU à prédire les branchements :

```pascal
// Indiquer qu'une condition est probable ou improbable
if Unlikely(error <> 0) then
  HandleError;

if Likely(status = OK) then
  Continue;
```

#### Branch elimination via cmov

Utiliser des instructions conditionnelles au lieu de sauts :

**Avant :**
```pascal
if x > y then
  max := x
else
  max := y;
```

**Code naïf :**
```asm
cmp eax, ebx  
jle else_branch  
mov ecx, eax  
jmp end_if  
else_branch:  
mov ecx, ebx  
end_if:
```

**Code optimisé (x86) :**
```asm
cmp eax, ebx  
cmovg ecx, eax   ; Conditional move - pas de saut !  
cmovle ecx, ebx
```

**Avantage :** Pas de saut = meilleur pipeline CPU

---

## Optimisations spécifiques aux plateformes

### Windows vs Linux

Certaines optimisations dépendent du système d'exploitation :

```pascal
{$IFDEF WINDOWS}
  // Utiliser les API Windows optimisées
  CopyMemory(@dest, @src, size);
{$ENDIF}

{$IFDEF LINUX}
  // Utiliser memcpy optimisé de glibc
  Move(src, dest, size);
{$ENDIF}
```

### x86-64 vs ARM

Optimisations spécifiques à l'architecture :

**x86-64 : REP MOVSB optimisé**
```pascal
// FPC peut utiliser REP MOVSB (très rapide sur CPU modernes)
Move(source, dest, large_size);
```

**ARM : Load/Store Multiple**
```pascal
// FPC utilise LDM/STM pour copier plusieurs valeurs
Move(source, dest, 16);  // Optimisé en une instruction
```

### Vectorisation (SIMD)

Utiliser les instructions vectorielles (SSE, AVX, NEON) :

**Code scalaire :**
```pascal
for i := 0 to 3 do
  result[i] := a[i] + b[i];  // 4 additions séquentielles
```

**Code vectorisé (conceptuel) :**
```pascal
// FPC peut générer du code SSE/AVX
result_vector := a_vector + b_vector;  // 4 additions en parallèle
```

**Activation :**
```bash
# x86-64 avec SSE2
fpc -CfSSE2 myprogram.pas

# x86-64 avec AVX
fpc -CfAVX myprogram.pas

# ARM avec NEON
fpc -CfNEON myprogram.pas
```

---

## Contrôler les optimisations

### Directives de compilation

#### Au niveau du fichier

```pascal
{$OPTIMIZATION ON}   // ou OFF
{$OPTIMIZATION LEVEL2}
{$OPTIMIZATION REGVAR}
```

#### Au niveau d'une fonction

```pascal
procedure CriticalCode;
{$OPTIMIZATION ON}
begin
  // Code avec optimisations
end;

procedure DebugCode;
{$OPTIMIZATION OFF}
begin
  // Code sans optimisations (facile à déboguer)
end;
```

### Attributs et hints

```pascal
// Forcer l'inlining
function FastCalc(x: Integer): Integer; inline;

// Empêcher l'inlining
function ComplexCalc(x: Integer): Integer;  
begin
  // Code complexe
end; {$OPTIMIZATION NOINLINE}

// Indiquer qu'une variable est volatile
var
  hardware_register: LongWord; volatile;
```

### Options de compilation avancées

```bash
# Optimiser pour la taille
fpc -Os myprogram.pas

# Optimiser pour la vitesse
fpc -O3 myprogram.pas

# Optimiser pour un processeur spécifique
fpc -Op3 myprogram.pas     # Pentium 3 et supérieur  
fpc -CpARMV7A myprogram.pas # ARM v7-A

# Désactiver des optimisations spécifiques
fpc -O2 -OoNOREGVAR myprogram.pas
```

---

## Écrire du code optimisable

### Bonnes pratiques

#### 1. Utilisez des types appropriés

**Mauvais :**
```pascal
var
  x: Extended;  // 80 bits, lent sur certains CPU
begin
  x := 5.0;
end;
```

**Bon :**
```pascal
var
  x: Double;    // 64 bits, registres SSE
begin
  x := 5.0;
end;
```

#### 2. Évitez les calculs répétitifs

**Mauvais :**
```pascal
for i := 0 to GetLength - 1 do  // GetLength appelé à chaque itération !
  Process(data[i]);
```

**Bon :**
```pascal
len := GetLength;  // Appelé une fois  
for i := 0 to len - 1 do
  Process(data[i]);
```

#### 3. Utilisez const pour les paramètres

**Moins optimisé :**
```pascal
procedure Process(s: string);  // Copie de la chaîne  
begin
  WriteLn(Length(s));
end;
```

**Plus optimisé :**
```pascal
procedure Process(const s: string);  // Pas de copie !  
begin
  WriteLn(Length(s));
end;
```

#### 4. Préférez les boucles for

**Moins optimisé :**
```pascal
i := 0;  
while i < 1000 do  
begin
  Process(i);
  Inc(i);
end;
```

**Plus optimisé :**
```pascal
for i := 0 to 999 do  // Le compilateur optimise mieux les for
  Process(i);
```

#### 5. Localisez les variables

**Moins optimisé :**
```pascal
var
  Global: Integer;  // Variable globale

procedure Test;  
begin
  Global := Global + 1;  // Accès mémoire
end;
```

**Plus optimisé :**
```pascal
procedure Test;  
var
  Local: Integer;   // Variable locale → registre
begin
  Local := 0;
  Local := Local + 1;  // Reste dans un registre
end;
```

#### 6. Évitez les conversions de types inutiles

**Mauvais :**
```pascal
var
  x: Integer;
  y: Double;
begin
  y := x;           // Conversion Int → Double
  x := Round(y);    // Conversion Double → Int
end;
```

**Bon :**
```pascal
var
  x: Integer;
begin
  x := x + 1;  // Pas de conversion
end;
```

### Pièges à éviter

#### 1. Micro-optimisations prématurées

**Ne faites pas ça :**
```pascal
// Code illisible pour gagner 0.001%
x := (y shl 3) - (y shl 1);  // x := y * 6  (pourquoi ??)
```

**Faites plutôt ça :**
```pascal
x := y * 6;  // Clair ! Le compilateur optimisera si nécessaire
```

**Règle d'or :** Écrivez du code clair d'abord, optimisez ensuite si nécessaire.

#### 2. Désactiver toutes les vérifications

**Dangereux :**
```pascal
{$R-}  // Range checking OFF
{$I-}  // I/O checking OFF
{$Q-}  // Overflow checking OFF
// Programme plus rapide mais bugs cachés !
```

**Mieux :**
```pascal
{$IFDEF DEBUG}
  {$R+} {$I+} {$Q+}  // Vérifications en debug
{$ELSE}
  {$R-} {$I-} {$Q-}  // Optimisations en release
{$ENDIF}
```

#### 3. Optimiser sans mesurer

Toujours **profiler** avant d'optimiser !

```pascal
// Utilisez des timers pour mesurer
var
  start, stop: TDateTime;
begin
  start := Now;

  // Code à mesurer
  for i := 1 to 1000000 do
    DoSomething;

  stop := Now;
  WriteLn('Temps: ', MilliSecondsBetween(stop, start), ' ms');
end;
```

---

## Profiling et mesure

### Outils de profiling

#### 1. Built-in profiling

FPC peut générer des informations de profiling :

```bash
# Compiler avec profiling
fpc -pg myprogram.pas

# Exécuter le programme
./myprogram

# Analyser les résultats
gprof myprogram gmon.out > profile.txt
```

**Fichier profile.txt :**
```
  %   cumulative   self              self     total
 time   seconds   seconds    calls  ms/call  ms/call  name
 45.2      1.23     1.23   100000     0.01     0.02  SlowFunction
 23.7      1.87     0.64    50000     0.01     0.01  MediumFunction
 15.3      2.29     0.42  1000000     0.00     0.00  FastFunction
```

Vous voyez immédiatement où le temps est passé !

#### 2. Valgrind (Linux)

Profiler détaillé avec informations cache :

```bash
# Profiling avec cachegrind
valgrind --tool=cachegrind ./myprogram

# Analyser
cg_annotate cachegrind.out.<pid>
```

#### 3. Instrumentation manuelle

Ajouter des mesures dans votre code :

```pascal
uses
  SysUtils, DateUtils;

var
  ProfileData: record
    FunctionACalls: Integer;
    FunctionATime: Int64;
    FunctionBCalls: Integer;
    FunctionBTime: Int64;
  end;

procedure StartProfile(var calls: Integer; var time: Int64);  
begin
  Inc(calls);
  time := time - GetTickCount64;
end;

procedure StopProfile(var time: Int64);  
begin
  time := time + GetTickCount64;
end;

procedure FunctionA;  
begin
  StartProfile(ProfileData.FunctionACalls, ProfileData.FunctionATime);

  // Code de la fonction

  StopProfile(ProfileData.FunctionATime);
end;

// À la fin du programme
WriteLn('FunctionA: ', ProfileData.FunctionACalls, ' appels, ',
        ProfileData.FunctionATime, ' ms');
```

### Benchmarking

Comparer plusieurs approches :

```pascal
procedure BenchmarkApproach;  
const
  ITERATIONS = 1000000;
var
  i: Integer;
  start: QWord;
begin
  // Approche 1
  start := GetTickCount64;
  for i := 1 to ITERATIONS do
    Approach1;
  WriteLn('Approche 1: ', GetTickCount64 - start, ' ms');

  // Approche 2
  start := GetTickCount64;
  for i := 1 to ITERATIONS do
    Approach2;
  WriteLn('Approche 2: ', GetTickCount64 - start, ' ms');
end;
```

---

## Cas pratiques d'optimisation

### Cas 1 : Tri de tableau

**Code initial (non optimisé) :**
```pascal
procedure BubbleSort(var arr: array of Integer);  
var
  i, j, temp: Integer;
begin
  for i := Low(arr) to High(arr) do
    for j := Low(arr) to High(arr) - 1 do
      if arr[j] > arr[j + 1] then
      begin
        temp := arr[j];
        arr[j] := arr[j + 1];
        arr[j + 1] := temp;
      end;
end;
```

**Optimisation 1 : Réduire les comparaisons**
```pascal
procedure BubbleSortOpt1(var arr: array of Integer);  
var
  i, j, temp: Integer;
  n: Integer;
begin
  n := High(arr);
  for i := Low(arr) to n do
    for j := Low(arr) to n - i do  // Réduire la portée
      if arr[j] > arr[j + 1] then
      begin
        temp := arr[j];
        arr[j] := arr[j + 1];
        arr[j + 1] := temp;
      end;
end;
```

**Optimisation 2 : Sortie anticipée**
```pascal
procedure BubbleSortOpt2(var arr: array of Integer);  
var
  i, j, temp: Integer;
  swapped: Boolean;
  n: Integer;
begin
  n := High(arr);
  repeat
    swapped := False;
    for j := Low(arr) to n - 1 do
      if arr[j] > arr[j + 1] then
      begin
        temp := arr[j];
        arr[j] := arr[j + 1];
        arr[j + 1] := temp;
        swapped := True;
      end;
    Dec(n);
  until not swapped;  // Sortir si déjà trié
end;
```

**Optimisation 3 : Meilleur algorithme**
```pascal
procedure QuickSort(var arr: array of Integer; left, right: Integer);  
var
  i, j, pivot, temp: Integer;
begin
  if left < right then
  begin
    pivot := arr[(left + right) div 2];
    i := left;
    j := right;

    repeat
      while arr[i] < pivot do Inc(i);
      while arr[j] > pivot do Dec(j);

      if i <= j then
      begin
        temp := arr[i];
        arr[i] := arr[j];
        arr[j] := temp;
        Inc(i);
        Dec(j);
      end;
    until i > j;

    QuickSort(arr, left, j);
    QuickSort(arr, i, right);
  end;
end;
```

**Résultats :**
- BubbleSort : O(n²) - 10 secondes pour 10000 éléments
- BubbleSortOpt1 : O(n²) - 7 secondes (30% plus rapide)
- BubbleSortOpt2 : O(n²) meilleur cas O(n) - 5 secondes si presque trié
- QuickSort : O(n log n) - 0.05 secondes ! **200x plus rapide**

### Cas 2 : Recherche dans tableau

**Code initial :**
```pascal
function FindValue(const arr: array of Integer; value: Integer): Integer;  
var
  i: Integer;
begin
  Result := -1;
  for i := Low(arr) to High(arr) do
    if arr[i] = value then
    begin
      Result := i;
      Exit;  // Trouver → sortir immédiatement
    end;
end;
```

**Optimisation : Tableau trié + recherche binaire**
```pascal
function BinarySearch(const arr: array of Integer; value: Integer): Integer;  
var
  left, right, mid: Integer;
begin
  left := Low(arr);
  right := High(arr);
  Result := -1;

  while left <= right do
  begin
    mid := (left + right) div 2;

    if arr[mid] = value then
    begin
      Result := mid;
      Exit;
    end
    else if arr[mid] < value then
      left := mid + 1
    else
      right := mid - 1;
  end;
end;
```

**Résultats :**
- Recherche linéaire : O(n) - 10000 comparaisons max
- Recherche binaire : O(log n) - 14 comparaisons max ! **700x moins**

### Cas 3 : Concaténation de chaînes

**Code initial (TRÈS LENT) :**
```pascal
var
  result: string;
  i: Integer;
begin
  result := '';
  for i := 1 to 10000 do
    result := result + 'item' + IntToStr(i) + ',';
  // Temps : ~5 secondes (chaque += réalloue !)
end;
```

**Optimisation 1 : StringBuilder**
```pascal
uses
  Classes;

var
  sb: TStringBuilder;
  i: Integer;
begin
  sb := TStringBuilder.Create;
  try
    for i := 1 to 10000 do
    begin
      sb.Append('item');
      sb.Append(i);
      sb.Append(',');
    end;
    result := sb.ToString;
  finally
    sb.Free;
  end;
  // Temps : ~0.05 secondes ! **100x plus rapide**
end;
```

**Optimisation 2 : TStringList**
```pascal
uses
  Classes;

var
  list: TStringList;
  i: Integer;
begin
  list := TStringList.Create;
  try
    for i := 1 to 10000 do
      list.Add('item' + IntToStr(i));
    result := list.CommaText;
  finally
    list.Free;
  end;
  // Temps : ~0.08 secondes
end;
```

### Cas 4 : Calculs mathématiques

**Code initial :**
```pascal
function Distance(x1, y1, x2, y2: Double): Double;  
begin
  Result := Sqrt(Sqr(x2 - x1) + Sqr(y2 - y1));
end;

// Appelé 1 million de fois
for i := 1 to 1000000 do
  d := Distance(points[i].x, points[i].y, target.x, target.y);
```

**Optimisation 1 : Distance carrée (éviter sqrt)**
```pascal
function DistanceSquared(x1, y1, x2, y2: Double): Double; inline;  
begin
  Result := Sqr(x2 - x1) + Sqr(y2 - y1);
end;

// Si on compare juste des distances, pas besoin de sqrt !
for i := 1 to 1000000 do  
begin
  dsq := DistanceSquared(points[i].x, points[i].y, target.x, target.y);
  if dsq < threshold_squared then  // Comparer les carrés
    ProcessPoint(i);
end;
// Gain : ~40% plus rapide (sqrt est coûteux)
```

**Optimisation 2 : Approximation rapide**
```pascal
function FastDistance(x1, y1, x2, y2: Double): Double; inline;  
var
  dx, dy, min, max: Double;
begin
  dx := Abs(x2 - x1);
  dy := Abs(y2 - y1);

  if dx < dy then
  begin
    min := dx;
    max := dy;
  end
  else
  begin
    min := dy;
    max := dx;
  end;

  // Approximation : max + min/2 (erreur ~8% max)
  Result := max + min * 0.5;
end;
// Gain : ~70% plus rapide, précision suffisante pour beaucoup de cas
```

---

## Optimisations avancées

### Profile-Guided Optimization (PGO)

Optimiser en fonction de l'utilisation réelle :

**Étape 1 : Compiler avec instrumentation**
```bash
fpc -pg myprogram.pas
```

**Étape 2 : Exécuter avec données réelles**
```bash
./myprogram < typical_input.txt
# Génère gmon.out
```

**Étape 3 : Recompiler avec profil**
```bash
fpc -FWgmon.out -O3 myprogram.pas
# Le compilateur optimise selon l'usage réel
```

### Link-Time Optimization (LTO)

Optimiser entre les unités :

```bash
# Activer LTO
fpc -O3 -CX -XX myprogram.pas
```

**Avantages :**
- Inlining entre unités
- Élimination de code mort global
- Meilleures optimisations globales

**Inconvénients :**
- Compilation plus lente
- Fichiers objets plus gros

### Whole Program Optimization

Optimiser le programme entier :

```bash
fpc -O3 -Owdevirtcalls -CX myprogram.pas
```

**Options :**
- `-Owdevirtcalls` : Dévirtualisation d'appels
- `-Owregvars` : Variables globales en registres
- `-CX` : Smart linking

---

## Optimisations spécifiques Windows/Linux

### Windows : FastMM et Memory Manager

**Remplacer le gestionnaire mémoire par défaut :**
```pascal
program MyApp;

{$IFDEF WINDOWS}
uses
  FastMM4;  // Gestionnaire mémoire optimisé
{$ENDIF}

begin
  // Votre code
end.
```

**Gain :** Jusqu'à 30% plus rapide pour code intensif en allocations.

### Linux : jemalloc

**Lier avec jemalloc :**
```bash
# Compiler
fpc myprogram.pas

# Exécuter avec jemalloc
LD_PRELOAD=/usr/lib/libjemalloc.so ./myprogram
```

### Optimisations système d'exploitation

**Windows : Large pages**
```pascal
{$IFDEF WINDOWS}
uses
  Windows;

// Allouer avec large pages (2MB au lieu de 4KB)
mem := VirtualAlloc(nil, size, MEM_COMMIT or MEM_LARGE_PAGES, PAGE_READWRITE);
{$ENDIF}
```

**Linux : Huge pages**
```bash
# Configurer huge pages
echo 512 > /proc/sys/vm/nr_hugepages

# Compiler avec support
fpc -k-lhugetlbfs myprogram.pas
```

---

## Comprendre les compromis

### Vitesse vs Taille

| Optimisation | Vitesse | Taille | Quand utiliser |
|--------------|---------|--------|----------------|
| `-Os` | + | +++ | Systèmes embarqués, bande passante limitée |
| `-O2` | ++ | ++ | **Par défaut** - bon équilibre |
| `-O3` | +++ | + | Serveurs, applications de calcul |
| Loop unrolling | ++++ | - | Boucles critiques courtes |
| Inlining | +++ | -- | Petites fonctions appelées souvent |

### Optimisation vs Débogage

**Mode Debug :**
```pascal
{$IFDEF DEBUG}
  {$OPTIMIZATION OFF}
  {$ASSERTIONS ON}
  {$RANGECHECKS ON}
  {$OVERFLOWCHECKS ON}
  {$DEBUGINFO ON}
{$ENDIF}
```

**Mode Release :**
```pascal
{$IFDEF RELEASE}
  {$OPTIMIZATION LEVEL3}
  {$ASSERTIONS OFF}
  {$RANGECHECKS OFF}
  {$OVERFLOWCHECKS OFF}
  {$DEBUGINFO OFF}
  {$INLINE ON}
{$ENDIF}
```

### Portable vs Optimisé

**Code portable (fonctionne partout) :**
```pascal
procedure GenericCopy(src, dst: Pointer; count: Integer);  
var
  i: Integer;
begin
  for i := 0 to count - 1 do
    PByte(dst)[i] := PByte(src)[i];
end;
```

**Code optimisé (spécifique x86-64) :**
```pascal
procedure OptimizedCopy(src, dst: Pointer; count: Integer);  
begin
  {$IFDEF CPUX86_64}
  asm
    mov rsi, src
    mov rdi, dst
    mov rcx, count
    rep movsb  // Instruction ultra-optimisée
  end;
  {$ELSE}
  Move(src^, dst^, count);  // Fallback portable
  {$ENDIF}
end;
```

---

## Mythes et réalités de l'optimisation

### Mythe 1 : "L'assembleur est toujours plus rapide"

**FAUX !** Le compilateur moderne optimise souvent mieux :

```pascal
// Code Pascal simple
function Sum(const arr: array of Integer): Integer;  
var
  i: Integer;
begin
  Result := 0;
  for i := Low(arr) to High(arr) do
    Result := Result + arr[i];
end;
```

Le compilateur peut :
- Vectoriser avec SSE/AVX
- Dérouler la boucle
- Utiliser les meilleurs registres
- S'adapter au CPU cible

À moins d'être expert en assembleur ET de profiler soigneusement, laissez le compilateur optimiser !

### Mythe 2 : "Inline rend toujours plus rapide"

**FAUX !** Inline peut ralentir si :
- La fonction est grosse → code bloat → cache miss
- La fonction est appelée rarement → overhead inutile
- La fonction est complexe → empêche d'autres optimisations

**Bon usage d'inline :**
```pascal
// OUI - petite fonction, appelée souvent
function Square(x: Integer): Integer; inline;  
begin
  Result := x * x;
end;

// NON - grosse fonction
function ComplexCalculation(data: TArray<Double>): TResult; inline;  
begin
  // 100 lignes de code...
end;
```

### Mythe 3 : "Les boucles for sont plus lentes que while"

**FAUX !** C'est l'inverse :

```pascal
// Le compilateur optimise très bien les for
for i := 0 to 999 do
  Process(i);

// Les while sont plus difficiles à optimiser
i := 0;  
while i < 1000 do  
begin
  Process(i);
  Inc(i);
end;
```

Le `for` permet au compilateur de mieux comprendre l'intention et d'optimiser.

### Mythe 4 : "Goto est plus rapide"

**PEUT ÊTRE VRAI, mais...**

```pascal
// Avec goto - difficile à optimiser
procedure WithGoto;  
label
  Start, End;
begin
  Start:
  if condition then
    goto End;
  // code
  goto Start;
  End:
end;

// Sans goto - le compilateur optimise mieux
procedure WithoutGoto;  
begin
  while not condition do
  begin
    // code
  end;
end;
```

Le code structuré permet de meilleures optimisations du compilateur.

### Mythe 5 : "Il faut toujours optimiser"

**FAUX !** L'optimisation prématurée est la racine du mal.

**Règle du 90/10 :**
- 90% du temps d'exécution se passe dans 10% du code
- Optimisez ces 10% seulement !
- Le reste : gardez-le simple et lisible

**Processus correct :**
1. Écrire du code clair et correct
2. **Profiler** pour identifier les bottlenecks
3. Optimiser **seulement** les parties critiques
4. **Mesurer** l'amélioration
5. Si gain insuffisant, recommencer étape 2

---

## Checklist d'optimisation

### Avant d'optimiser

☑️ Le code fonctionne correctement

☑️ J'ai des tests unitaires

☑️ J'ai profilé et identifié les bottlenecks

☑️ J'ai mesuré la performance actuelle

☑️ L'optimisation en vaut vraiment la peine

### Optimisations de base (toujours faire)

☑️ Compiler avec `-O2` minimum

☑️ Utiliser `const` pour paramètres non modifiés

☑️ Utiliser types appropriés (Integer, Double, etc.)

☑️ Sortir les invariants des boucles

☑️ Utiliser boucles `for` plutôt que `while` quand possible

### Optimisations intermédiaires

☑️ Marquer fonctions critiques `inline`

☑️ Réduire les allocations mémoire

☑️ Utiliser structures de données appropriées

☑️ Éviter conversions de types inutiles

☑️ Précharger les données (cache-friendly)

### Optimisations avancées

☑️ Profiling détaillé (gprof, valgrind)

☑️ Optimisations algorithmiques

☑️ Vectorisation SIMD si applicable

☑️ Multithreading si parallélisable

☑️ Optimisations spécifiques plateforme

### Après optimisation

☑️ Mesurer l'amélioration réelle

☑️ Vérifier que le code fonctionne toujours

☑️ Refaire passer les tests

☑️ Documenter les optimisations non triviales

☑️ Profiler à nouveau pour prochaines améliorations

---

## Outils et ressources

### Outils de profiling

**Linux :**
- `gprof` - Profiling fonction par fonction
- `perf` - Profiling système complet
- `valgrind` - Cache profiling, leak detection
- `cachegrind` - Analyse cache CPU

**Windows :**
- `AQTime` (Embarcadero) - Profiler complet
- `Intel VTune` - Profiler haute performance
- `Very Sleepy` - Profiler gratuit
- `Windows Performance Toolkit`

**Multi-plateforme :**
- `gperftools` - Google Performance Tools
- Instrumentation manuelle avec timers

### Benchmarking

```pascal
unit BenchmarkUtils;

interface

uses
  SysUtils, DateUtils;

type
  TBenchmark = class
  private
    FStart: QWord;
    FName: string;
  public
    constructor Create(const AName: string);
    destructor Destroy; override;
  end;

implementation

constructor TBenchmark.Create(const AName: string);  
begin
  FName := AName;
  FStart := GetTickCount64;
  WriteLn('Benchmark "', FName, '" started...');
end;

destructor TBenchmark.Destroy;  
var
  elapsed: QWord;
begin
  elapsed := GetTickCount64 - FStart;
  WriteLn('Benchmark "', FName, '" completed in ', elapsed, ' ms');
  inherited;
end;

end.

// Utilisation :
var
  bench: TBenchmark;
begin
  bench := TBenchmark.Create('Mon algorithme');
  try
    // Code à benchmarker
    for i := 1 to 1000000 do
      DoSomething;
  finally
    bench.Free;  // Affiche automatiquement le temps
  end;
end;
```

### Documentation

📚 **FPC Optimization Guide**
- Wiki FreePascal : https://wiki.freepascal.org/Optimization

📚 **Compiler Options**
- `fpc -h` - Liste complète des options
- Documentation officielle FPC

📚 **Architecture Manuals**
- Intel Optimization Manual
- ARM Cortex Optimization Guide
- Agner Fog's optimization resources (excellent !)

### Livres recommandés

📖 **"Computer Systems: A Programmer's Perspective"**
- Comprendre le hardware pour optimiser
- Cache, pipeline, mémoire

📖 **"Code Optimization: Effective Memory Use"**
- Optimisations pratiques
- Nombreux exemples

📖 **"The Pragmatic Programmer"**
- Philosophie de l'optimisation
- Quand et comment optimiser

---

## Conclusion

### Ce que nous avons appris

✅ **Les types d'optimisations** : locale, expressions, contrôle, boucles, procédures, registres, mémoire

✅ **Contrôler les optimisations** : Options `-O`, directives, attributs

✅ **Écrire du code optimisable** : Bonnes pratiques, pièges à éviter

✅ **Profiler et mesurer** : Outils et techniques

✅ **Cas pratiques** : Optimisations réelles sur vrais problèmes

✅ **Comprendre les compromis** : Vitesse vs taille, debug vs release, portable vs optimisé

### Principes fondamentaux à retenir

🎯 **Règle #1 : Profiler avant d'optimiser**
Ne devinez jamais, mesurez !

🎯 **Règle #2 : Clarté d'abord**
Code lisible > code "clever"

🎯 **Règle #3 : Algorithme avant micro-optimisation**
O(n log n) vs O(n²) bat n'importe quelle optimisation bas niveau

🎯 **Règle #4 : Faire confiance au compilateur**
Il est souvent plus malin que vous

🎯 **Règle #5 : Mesurer l'impact**
Si vous ne mesurez pas, vous ne savez pas si ça marche

### La philosophie de l'optimisation

> "Premature optimization is the root of all evil" - Donald Knuth

Mais aussi :

> "Mature optimization is the key to success" - Anonyme

**Le bon équilibre :**
1. Écrire du code **correct** et **clair**
2. Le faire fonctionner
3. **Profiler** pour identifier les vrais problèmes
4. **Optimiser** ce qui compte vraiment
5. **Mesurer** les résultats
6. Répéter si nécessaire

### Prochaines étapes

Pour continuer à progresser :

1. **Pratiquez** avec de vrais projets
2. **Profilez** régulièrement vos applications
3. **Expérimentez** avec différentes optimisations
4. **Lisez** le code source de FPC pour comprendre les optimisations internes
5. **Contribuez** en partageant vos découvertes

### Le mot de la fin

L'optimisation est un **art** autant qu'une **science**. Cela demande :
- De la compréhension technique
- De l'expérience pratique
- De l'intuition
- De la mesure rigoureuse
- De la patience

Mais quand vous voyez votre programme passer de 10 secondes à 0.1 secondes grâce à vos optimisations, la satisfaction est immense !

**Bonne optimisation ! 🚀**

---

*N'oubliez pas : le code le plus rapide est celui qui n'a pas besoin de s'exécuter. Réfléchissez d'abord à l'algorithme !*

⏭️ [Génération de code custom](/24-compilateur-outils-avances/04-generation-code-custom.md)
