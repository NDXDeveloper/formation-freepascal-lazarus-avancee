🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 3.11 Optimisations du compilateur FreePascal

## Introduction : Qu'est-ce que l'optimisation du compilateur ?

Imaginez que vous écrivez une recette de cuisine. Vous pourriez écrire : "Prendre un œuf, le casser, le mettre dans le bol, prendre un autre œuf, le casser, le mettre dans le bol". Une personne intelligente lirait cela et penserait : "Je vais prendre les deux œufs d'un coup et les casser l'un après l'autre". C'est exactement ce que fait l'optimisation du compilateur : elle rend votre code plus efficace sans changer le résultat final.

Le compilateur FreePascal peut transformer votre code pour qu'il :
- S'exécute plus rapidement
- Utilise moins de mémoire
- Produise des fichiers exécutables plus petits

## Les niveaux d'optimisation de base

### Niveau 0 : Aucune optimisation (-O0 ou -O-)

C'est le mode par défaut pour le débogage. Le compilateur traduit votre code tel quel, sans chercher à l'améliorer.

```pascal
// Votre code
var
  i, total: Integer;
begin
  total := 0;
  for i := 1 to 100 do
    total := total + 1;
  WriteLn(total);
end.
```

**Compilation :** `fpc -O0 monprogramme.pas`

**Avantages :**
- Débogage facile
- Compilation rapide
- Le code assembleur correspond exactement à votre code source

**Inconvénients :**
- Programme plus lent
- Fichier exécutable plus gros

### Niveau 1 : Optimisations légères (-O1)

Le compilateur applique des optimisations simples qui ne ralentissent pas trop la compilation.

```pascal
// Le compilateur peut simplifier ceci :
x := 5 + 3;  // Devient directement : x := 8;

// Ou éliminer du code inutile :
y := 10;
y := 20;  // La première ligne est supprimée
```

**Compilation :** `fpc -O1 monprogramme.pas`

**Optimisations appliquées :**
- Calcul des constantes à la compilation
- Suppression du code mort (code jamais exécuté)
- Simplification des expressions

### Niveau 2 : Optimisations standard (-O2)

C'est le niveau recommandé pour la plupart des programmes en production.

```pascal
// Exemple : boucle qui sera optimisée
var
  i: Integer;
  tableau: array[1..1000] of Integer;
begin
  // Cette boucle sera optimisée pour être plus rapide
  for i := 1 to 1000 do
    tableau[i] := i * 2;
end.
```

**Compilation :** `fpc -O2 monprogramme.pas`

**Optimisations supplémentaires :**
- Réorganisation des instructions pour une meilleure performance
- Utilisation optimale des registres du processeur
- Déroulement partiel des boucles simples

### Niveau 3 : Optimisations agressives (-O3)

Le compilateur prend plus de temps mais produit un code très optimisé.

**Compilation :** `fpc -O3 monprogramme.pas`

**Optimisations supplémentaires :**
- Inline automatique de petites fonctions
- Optimisations mathématiques avancées
- Réorganisation agressive du code

### Niveau 4 : Optimisations maximales (-O4)

Attention : peut parfois causer des problèmes avec certains codes !

**Compilation :** `fpc -O4 monprogramme.pas`

## Optimisations spécifiques importantes

### 1. Optimisation de la taille (-Os)

Produit le plus petit exécutable possible, utile pour les systèmes embarqués.

```bash
# Windows
fpc -Os monprogramme.pas

# Ubuntu/Linux
fpc -Os monprogramme.pas
```

### 2. Inline de fonctions (-Si)

Le compilateur remplace les appels de petites fonctions par leur code directement.

```pascal
// Sans inline : chaque appel est un saut dans le programme
function Double(x: Integer): Integer; inline;
begin
  Result := x * 2;
end;

// Utilisation
y := Double(5);  // Sera remplacé par : y := 5 * 2;
```

### 3. Optimisations pour processeur spécifique

```bash
# Pour un processeur Intel Core moderne
fpc -CpCOREI -O3 monprogramme.pas

# Pour un processeur AMD Ryzen
fpc -CpZEN -O3 monprogramme.pas

# Pour voir tous les processeurs supportés
fpc -ic
```

## Configuration dans Lazarus

### Méthode graphique (recommandée pour débutants)

1. **Ouvrez votre projet** dans Lazarus
2. **Menu Projet → Options du projet**
3. **Section "Compilateur"** dans l'arborescence à gauche
4. **Onglet "Compilation et édition de liens"**

Vous verrez plusieurs options :

- **Optimisation** : Choisissez le niveau (0 à 3)
- **Optimiser pour** :
  - "Plus rapide" : génère du code plus rapide
  - "Plus petit" : génère un exécutable plus petit
- **Processeur cible** : Sélectionnez votre processeur

### Configuration par profils

Créez différents modes de compilation :

1. **Mode Debug** (développement)
   - Optimisation : Niveau 0
   - Générer les infos de débogage : Oui
   - Vérifications : Toutes activées

2. **Mode Release** (production)
   - Optimisation : Niveau 2 ou 3
   - Générer les infos de débogage : Non
   - Vérifications : Désactivées

## Optimisations Windows vs Ubuntu

### Spécificités Windows

```pascal
{$IFDEF WINDOWS}
  {$OPTIMIZATION ON}
  {$SMARTLINK ON}  // Réduit la taille de l'exécutable
{$ENDIF}
```

**Compilation optimisée Windows :**
```batch
fpc -O3 -XX -CX -Xs monprogramme.pas
```

- `-XX` : Smart linking (élimine le code non utilisé)
- `-CX` : Recrée les tables d'import
- `-Xs` : Strip les symboles (réduit la taille)

### Spécificités Ubuntu/Linux

```pascal
{$IFDEF LINUX}
  {$OPTIMIZATION ON}
  {$LINKLIB c}  // Optimise les liens avec les bibliothèques système
{$ENDIF}
```

**Compilation optimisée Ubuntu :**
```bash
fpc -O3 -XX -CX -Xs monprogramme.pas
```

Sur Ubuntu, vous pouvez aussi utiliser :
```bash
# Après compilation, réduire encore la taille
strip monprogramme
```

## Directives d'optimisation dans le code

Vous pouvez activer/désactiver les optimisations directement dans votre code :

```pascal
{$OPTIMIZATION OFF}  // Désactive l'optimisation
procedure CodeCritiquePourDebug;
begin
  // Code qui doit rester exactement comme écrit
  // pour le débogage
end;
{$OPTIMIZATION ON}  // Réactive l'optimisation

{$OPTIMIZATION LEVEL2}  // Force le niveau 2
procedure CodeOptimise;
begin
  // Code qui sera optimisé niveau 2
end;
{$OPTIMIZATION DEFAULT}  // Retour au niveau par défaut
```

## Mesurer l'impact des optimisations

### Code de test simple

```pascal
program TestOptimisation;
uses SysUtils;

var
  Start, Stop: TDateTime;
  i, j, resultat: Int64;

begin
  WriteLn('Test de performance...');

  Start := Now;

  // Code à tester
  resultat := 0;
  for i := 1 to 100000000 do
  begin
    resultat := resultat + i;
    if (i mod 2) = 0 then
      resultat := resultat - (i div 2);
  end;

  Stop := Now;

  WriteLn('Resultat: ', resultat);
  WriteLn('Temps: ', FormatDateTime('ss.zzz', Stop - Start), ' secondes');
end.
```

### Comparer les résultats

```bash
# Sans optimisation
fpc -O0 test.pas
./test
# Temps: 02.345 secondes

# Avec optimisation niveau 2
fpc -O2 test.pas
./test
# Temps: 00.876 secondes

# Avec optimisation niveau 3
fpc -O3 test.pas
./test
# Temps: 00.654 secondes
```

## Bonnes pratiques et pièges à éviter

### ✅ Bonnes pratiques

1. **Développement** : Utilisez `-O0` pour faciliter le débogage
2. **Tests** : Passez progressivement à `-O1` puis `-O2`
3. **Production** : Utilisez `-O2` par défaut (bon équilibre)
4. **Critique** : `-O3` uniquement si les performances sont cruciales

### ⚠️ Pièges courants

1. **Ne pas tester après optimisation**
   ```pascal
   // Ce code peut se comporter différemment avec -O3
   var p: PInteger;
   begin
     p := nil;
     if Assigned(p) and (p^ > 0) then  // Danger avec optimisations
       WriteLn('OK');
   end;
   ```

2. **Supposer que plus = mieux**
   - `-O4` n'est pas toujours plus rapide que `-O3`
   - `-Os` peut être plus rapide que `-O3` sur des petits programmes

3. **Oublier l'architecture cible**
   ```bash
   # Mauvais : compile pour processeur générique
   fpc -O3 programme.pas

   # Bon : compile pour votre processeur
   fpc -O3 -CpCOREI7 programme.pas
   ```

## Optimisations avancées (pour aller plus loin)

### Analyse du code assembleur généré

```bash
# Génère le fichier assembleur pour analyse
fpc -O2 -al programme.pas

# Ouvre programme.s pour voir le code assembleur
```

### Profiling pour identifier où optimiser

```pascal
// Utilisez un profiler pour voir où votre programme passe du temps
// Sous Windows : utilisez gprof ou des outils comme Very Sleepy
// Sous Ubuntu : utilisez gprof, valgrind ou perf

// Compilation avec support du profiling
fpc -O2 -pg programme.pas
```

### Optimisations manuelles complémentaires

```pascal
// Utilisez des types appropriés
var
  i: Byte;     // Si vous savez que i < 256
  // au lieu de
  i: Integer;  // Qui prend plus de mémoire

// Pré-calculez quand possible
const
  PI_TIMES_2 = 6.28318;  // Au lieu de calculer PI * 2 à chaque fois

// Évitez les divisions (lentes)
x := y div 2;   // Lent
x := y shr 1;   // Rapide (division par 2 en décalant les bits)
```

## Tableau récapitulatif

| Option | Utilisation | Vitesse | Taille | Débogage |
|--------|------------|---------|--------|----------|
| -O0 | Développement | ❌ Lente | ❌ Grande | ✅ Facile |
| -O1 | Debug avancé | ⚡ Correcte | 📦 Moyenne | ✅ Possible |
| -O2 | Production | ⚡⚡ Rapide | 📦 Raisonnable | ⚠️ Difficile |
| -O3 | Performance | ⚡⚡⚡ Très rapide | 📦 Variable | ❌ Très difficile |
| -Os | Embarqué | ⚡ Correcte | 📦✅ Minimale | ❌ Difficile |


Les optimisations du compilateur FreePascal sont un outil puissant pour améliorer les performances de vos programmes. Commencez simple avec `-O0` pendant le développement, puis augmentez progressivement le niveau d'optimisation. Testez toujours votre programme après avoir changé les optimisations, car certains bugs peuvent n'apparaître qu'avec certains niveaux d'optimisation.

Pour la plupart des applications, `-O2` offre le meilleur compromis entre performance et fiabilité. N'oubliez pas que l'optimisation prématurée est la racine de tous les maux : écrivez d'abord du code correct et lisible, optimisez ensuite si nécessaire !


## Optimisations
1. [Introduction aux optimisations](#introduction)
2. [Architecture du processus de compilation](#architecture)
3. [Les niveaux d'optimisation détaillés](#niveaux)
4. [Optimisations spécifiques du compilateur](#specifiques)
5. [Optimisations par type de données](#types)
6. [Optimisations des structures de contrôle](#structures)
7. [Optimisations mémoire](#memoire)
8. [Optimisations multi-plateformes](#multiplateforme)
9. [Directives et pragmas](#directives)
10. [Optimisations manuelles complémentaires](#manuelles)
11. [Profiling et analyse](#profiling)
12. [Cas pratiques et exemples](#pratiques)
13. [Dépannage et résolution de problèmes](#depannage)

## 1. Introduction aux optimisations {#introduction}

### Qu'est-ce que l'optimisation du compilateur ?

L'optimisation du compilateur est un ensemble de transformations automatiques appliquées à votre code source pour améliorer ses performances sans modifier son comportement observable. C'est comme avoir un assistant intelligent qui réécrit votre code pour le rendre plus efficace tout en gardant exactement le même résultat.

Prenons un exemple concret :

```pascal
// Code original écrit par le développeur
function CalculerSomme: Integer;
var
  i, total: Integer;
begin
  total := 0;
  for i := 1 to 5 do
  begin
    total := total + i;
  end;
  Result := total;
end;

// Après optimisation, le compilateur pourrait générer l'équivalent de :
function CalculerSomme: Integer;
begin
  Result := 15;  // Le compilateur a calculé 1+2+3+4+5 = 15 à la compilation
end;
```

### Pourquoi optimiser ?

Les optimisations du compilateur permettent de :

1. **Améliorer les performances d'exécution**
   - Réduction du temps d'exécution de 20% à 500% selon les cas
   - Meilleure utilisation du cache processeur
   - Moins d'instructions machine générées

2. **Réduire l'utilisation mémoire**
   - Élimination des variables temporaires inutiles
   - Réutilisation optimale des registres
   - Compression des structures de données

3. **Diminuer la taille des exécutables**
   - Suppression du code mort
   - Factorisation du code dupliqué
   - Élimination des symboles inutiles

4. **Optimiser la consommation énergétique**
   - Moins d'instructions = moins de cycles processeur
   - Important pour les applications mobiles et embarquées

### Les phases de compilation FreePascal

```
Code source (.pas)
        ↓
[Analyse lexicale] → Découpage en tokens
        ↓
[Analyse syntaxique] → Arbre syntaxique abstrait (AST)
        ↓
[Analyse sémantique] → Vérification des types
        ↓
[OPTIMISATIONS DE HAUT NIVEAU] ← Nous sommes ici
        ↓
[Génération de code intermédiaire]
        ↓
[OPTIMISATIONS DE BAS NIVEAU] ← Et ici
        ↓
[Génération de code machine]
        ↓
[Édition de liens]
        ↓
Exécutable final (.exe ou binaire)
```

## 2. Architecture du processus de compilation {#architecture}

### Les différentes passes d'optimisation

FreePascal effectue plusieurs passes d'optimisation :

#### Passe 1 : Optimisations indépendantes de la plateforme

```pascal
// Exemple : Propagation de constantes
const
  LARGEUR = 100;
  HAUTEUR = 50;

var
  surface: Integer;
begin
  surface := LARGEUR * HAUTEUR;  // Devient : surface := 5000;
end;
```

#### Passe 2 : Optimisations de flot de contrôle

```pascal
// Exemple : Élimination de code inaccessible
procedure Test(x: Integer);
begin
  if x > 0 then
    WriteLn('Positif')
  else if x < 0 then
    WriteLn('Négatif')
  else
    WriteLn('Zéro');

  Exit;  // Tout ce qui suit est éliminé

  WriteLn('Ce code ne sera jamais exécuté');  // Supprimé à la compilation
end;
```

#### Passe 3 : Optimisations spécifiques à l'architecture

```pascal
// Sur x86-64, utilisation des instructions SIMD pour les tableaux
var
  a, b, c: array[0..3] of Single;
  i: Integer;
begin
  // Sans optimisation : 4 additions séparées
  for i := 0 to 3 do
    c[i] := a[i] + b[i];

  // Avec optimisation : Une seule instruction SIMD (ADDPS)
  // qui additionne les 4 valeurs simultanément
end;
```

### Le système de représentation intermédiaire

FreePascal utilise une représentation intermédiaire (IR) pour faciliter les optimisations :

```pascal
// Code Pascal
x := y + z * 2;

// Représentation intermédiaire (simplifié)
LOAD  y, R1
LOAD  z, R2
MUL   R2, 2, R3
ADD   R1, R3, R4
STORE R4, x

// Après optimisation (si z est constant = 5)
LOAD  y, R1
ADD   R1, 10, R2  // z*2 = 5*2 = 10 calculé à la compilation
STORE R2, x
```

## 3. Les niveaux d'optimisation détaillés {#niveaux}

### Niveau -O0 ou -O- : Aucune optimisation

C'est le mode de compilation le plus basique, utilisé principalement pour le débogage.

```pascal
program TestO0;
var
  a, b, c: Integer;
begin
  a := 10;
  b := 20;
  c := a + b;
  WriteLn(c);
end.
```

**Compilation :**
```bash
fpc -O0 -al test.pas  # -al génère le fichier assembleur
```

**Caractéristiques du code généré :**
- Chaque ligne Pascal correspond à des instructions assembleur
- Toutes les variables sont stockées en mémoire
- Aucune optimisation des registres
- Instructions de débogage préservées
- Taille typique : 100% (référence)
- Vitesse : La plus lente

**Assembleur généré (extrait simplifié) :**
```asm
movl $10, -4(%rbp)   # a := 10
movl $20, -8(%rbp)   # b := 20
movl -4(%rbp), %eax  # Charge a
addl -8(%rbp), %eax  # Ajoute b
movl %eax, -12(%rbp) # Stocke dans c
```

### Niveau -O1 : Optimisations de base

Premier niveau d'optimisation, bon compromis compilation/performance.

**Optimisations activées :**

1. **Constant folding (pliage de constantes)**
```pascal
// Avant
x := 2 + 3 * 4;
y := 100 div 5;

// Après optimisation O1
x := 14;  // Calculé à la compilation
y := 20;  // Calculé à la compilation
```

2. **Dead code elimination (élimination du code mort)**
```pascal
// Avant
procedure Test;
var
  unused: Integer;
begin
  unused := 42;  // Cette ligne sera supprimée
  WriteLn('Hello');
end;

// Après optimisation O1
procedure Test;
begin
  WriteLn('Hello');
end;
```

3. **Common subexpression elimination (CSE)**
```pascal
// Avant
a := b * c + 10;
d := b * c + 20;

// Après optimisation O1
temp := b * c;  // Calcul fait une seule fois
a := temp + 10;
d := temp + 20;
```

**Performance :**
- Compilation : ~10% plus lente qu'en -O0
- Exécution : 20-40% plus rapide qu'en -O0
- Taille : 90-95% de -O0

### Niveau -O2 : Optimisations standard

Niveau recommandé pour la production, équilibre optimal.

**Optimisations supplémentaires :**

1. **Loop optimizations (optimisations de boucles)**
```pascal
// Loop unrolling partiel
// Avant
for i := 1 to 8 do
  arr[i] := i * 2;

// Après optimisation O2 (déroulement par 4)
// Pseudo-code illustratif (step n'existe pas en Pascal) :
//   for i := 1 to 8 step 4 do ...
// Le compilateur génère l'équivalent de :
i := 1;
while i <= 8 do
begin
  arr[i] := i * 2;
  arr[i+1] := (i+1) * 2;
  arr[i+2] := (i+2) * 2;
  arr[i+3] := (i+3) * 2;
  Inc(i, 4);
end;
```

2. **Instruction scheduling (ordonnancement des instructions)**
```pascal
// Réorganisation pour éviter les dépendances
// Avant
a := b + c;  // Instruction 1
d := a * 2;  // Dépend de 1, doit attendre
e := f + g;  // Indépendant

// Après optimisation O2
a := b + c;  // Instruction 1
e := f + g;  // Exécuté en parallèle avec 1
d := a * 2;  // Maintenant a est prêt
```

3. **Register allocation (allocation de registres)**
```pascal
// Variables fréquemment utilisées gardées dans les registres
function Calculate: Integer;
var
  i, sum: Integer;  // Seront dans des registres, pas en mémoire
begin
  sum := 0;
  for i := 1 to 1000000 do
    sum := sum + i;
  Result := sum;
end;
```

4. **Function inlining sélectif**
```pascal
function Small(x: Integer): Integer; inline;
begin
  Result := x * 2;
end;

// L'appel Small(5) devient directement 5 * 2 dans le code
```

**Performance :**
- Compilation : 30-50% plus lente qu'en -O0
- Exécution : 50-100% plus rapide qu'en -O0
- Taille : 85-95% de -O0

### Niveau -O3 : Optimisations agressives

Optimisations maximales pour la vitesse, peut augmenter la taille.

**Optimisations supplémentaires :**

1. **Aggressive inlining**
```pascal
// Même les fonctions moyennes sont inline
function ProcessData(const data: array of Integer): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to High(data) do
    Result := Result + data[i] * data[i];
end;

// Tout appel à ProcessData est remplacé par son code
```

2. **Loop vectorization (vectorisation des boucles)**
```pascal
// Utilisation automatique des instructions SIMD
var
  a, b, c: array[0..1023] of Single;
  i: Integer;
begin
  // Traité par blocs de 4 ou 8 valeurs simultanément
  for i := 0 to 1023 do
    c[i] := a[i] * b[i] + 1.0;
end;
```

3. **Predictive commoning**
```pascal
// Prédiction et pré-calcul des valeurs
for i := 2 to n do
  a[i] := a[i-1] + a[i-2];  // Pattern détecté et optimisé
```

**Performance :**
- Compilation : 100-200% plus lente qu'en -O0
- Exécution : 80-200% plus rapide qu'en -O0
- Taille : 95-120% de -O0 (peut être plus gros!)

### Niveau -O4 : Optimisations expérimentales

⚠️ **Attention : Peut causer des problèmes de stabilité**

```pascal
{$OPTIMIZATION LEVEL4}
// Active des optimisations expérimentales
// - Transformations mathématiques agressives
// - Réorganisations de code risquées
// - Suppositions sur le comportement du programme
```

**Utilisation :**
```bash
fpc -O4 -Ooloopunroll -Oofastmath programme.pas
```

### Niveau -Os : Optimisation pour la taille

Priorité à la taille minimale de l'exécutable.

```pascal
// Exemple de différence
// -O2 pourrait dérouler cette boucle (plus rapide, plus gros)
// -Os garde la boucle compacte (plus lent, plus petit)
for i := 1 to 100 do
  Process(i);
```

**Techniques utilisées :**
- Pas de déroulement de boucles
- Pas d'inlining agressif
- Partage maximal du code
- Compression des tables de données

**Performance :**
- Taille : 60-80% de -O0
- Vitesse : Souvent comparable à -O1

## 4. Optimisations spécifiques du compilateur {#specifiques}

### Smart Linking (-XX)

Élimine tout code non utilisé de l'exécutable final.

```pascal
unit MaBibliotheque;

interface

procedure UtiliseDansLeProgramme;
procedure JamaisAppelee;  // Cette procédure sera éliminée
procedure AutreFonctionInutile;  // Celle-ci aussi

implementation

procedure UtiliseDansLeProgramme;
begin
  WriteLn('Je suis utilisée');
end;

procedure JamaisAppelee;
begin
  WriteLn('Code inutile de 1000 lignes...');
  // Tout ce code sera supprimé de l'exécutable
end;

procedure AutreFonctionInutile;
begin
  // Également supprimée
end;

end.
```

**Activation :**
```bash
# Windows et Linux
fpc -XX programme.pas

# Réduction typique : 30-70% de la taille
```

### Whole Program Optimization (-OWall)

Optimisation globale du programme entier.

```pascal
// Fichier unit1.pas
unit Unit1;
interface
function GetValue: Integer;
implementation
function GetValue: Integer;
begin
  Result := 42;  // Toujours retourne 42
end;
end.

// Fichier main.pas
uses Unit1;
var x: Integer;
begin
  x := GetValue;  // Avec -OWall, devient directement x := 42
  WriteLn(x);
end;
```

**Compilation :**
```bash
fpc -OWall main.pas
```

### Link Time Optimization (LTO)

Optimisation au moment de l'édition de liens.

```bash
# Active LTO pour une optimisation maximale inter-unités
fpc -Clflto -k-flto programme.pas
```

**Avantages :**
- Inline inter-unités
- Élimination de code mort global
- Optimisation des appels virtuels

### Optimisations mathématiques (-Oofastmath)

⚠️ **Attention : Peut violer les standards IEEE 754**

```pascal
{$OPTIMIZATION FASTMATH}
var
  x, y, z: Double;
begin
  // Sans fastmath : respect strict de l'ordre des opérations
  x := (y + z) + 1.0;

  // Avec fastmath : réorganisation possible
  x := y + (z + 1.0);  // Si plus efficace
end;
```

**Transformations appliquées :**
```pascal
// Simplifications mathématiques
x * 1.0 → x
x / 1.0 → x
x + 0.0 → x
x - x → 0.0
x / x → 1.0 (dangereux si x=0!)

// Réorganisations
(a + b) + c → a + (b + c)  // Si plus efficace
a * b + a * c → a * (b + c)  // Factorisation
```

## 5. Optimisations par type de données {#types}

### Optimisation des entiers

```pascal
// Utilisation du bon type selon la plage
var
  age: Byte;           // 0..255, 1 octet
  temperature: ShortInt; // -128..127, 1 octet
  compteur: Word;      // 0..65535, 2 octets
  population: Cardinal; // 0..4294967295, 4 octets
  dette: Int64;        // Très grandes valeurs, 8 octets
```

**Optimisations automatiques :**
```pascal
// Division par puissance de 2
x := y div 8;   // Transformé en : x := y shr 3

// Multiplication par puissance de 2
x := y * 16;    // Transformé en : x := y shl 4

// Modulo puissance de 2
x := y mod 32;  // Transformé en : x := y and 31
```

### Optimisation des réels

```pascal
// Précision et performance
var
  // Single : 4 octets, plus rapide, moins précis
  vitesse: Single;

  // Double : 8 octets, standard, bon compromis
  prix: Double;

  // Extended : 10 octets (x86), maximum de précision
  calcul_scientifique: Extended;
```

**Optimisations SSE/AVX :**
```pascal
{$MODESWITCH ADVANCEDRECORDS}
type
  TVector4f = record
    x, y, z, w: Single;
    class operator +(const a, b: TVector4f): TVector4f;
  end;

// Utilise une seule instruction SIMD pour additionner 4 floats
```

### Optimisation des chaînes

```pascal
// Choix du type de chaîne approprié
var
  // ShortString : 255 caractères max, allocation statique, rapide
  nom: String[50];

  // AnsiString : Taille dynamique, compteur de référence
  texte: AnsiString;

  // UnicodeString : Pour le support international
  international: UnicodeString;

  // RawByteString : Pour les données binaires
  donnees: RawByteString;
```

**Optimisations de concaténation :**
```pascal
// Mauvais : réallocations multiples
s := '';
for i := 1 to 1000 do
  s := s + IntToStr(i) + ', ';

// Bon : pré-allocation
SetLength(s, 10000);  // Pré-alloue la mémoire
p := 1;
for i := 1 to 1000 do
begin
  tmp := IntToStr(i) + ', ';
  Move(tmp[1], s[p], Length(tmp));
  Inc(p, Length(tmp));
end;
SetLength(s, p-1);
```

### Optimisation des tableaux

```pascal
// Tableaux statiques : plus rapides, taille fixe
var
  static: array[0..99] of Integer;  // Sur la pile, accès direct

// Tableaux dynamiques : flexibles mais plus lents
var
  dynamic: array of Integer;  // Sur le tas, indirection

// Tableaux alignés pour SIMD
type
  {$ALIGN 16}  // Alignement pour SSE
  TAlignedArray = array[0..15] of Single;
```

**Parcours optimisé :**
```pascal
// Mauvais : calcul de High à chaque itération
for i := 0 to High(arr) do
  Process(arr[i]);

// Bon : calcul une seule fois
len := High(arr);
for i := 0 to len do
  Process(arr[i]);

// Encore mieux : pointeurs pour gros tableaux
p := @arr[0];
for i := 0 to len do
begin
  Process(p^);
  Inc(p);
end;
```

## 6. Optimisations des structures de contrôle {#structures}

### Optimisation des conditions

```pascal
// Ordre des conditions : du plus probable au moins probable
if (x > 0) and (x < 100) then  // Cas fréquent en premier
  ProcessNormal(x)
else if x = 0 then              // Cas rare
  ProcessZero
else                            // Cas très rare
  ProcessSpecial(x);

// Court-circuit booléen
if FastCheck and ThenSlowCheck then  // FastCheck évalué d'abord
  DoSomething;

// Transformation en table de saut
case x of
  0: HandleZero;      // Compilé en table
  1: HandleOne;       // de saut directe
  2: HandleTwo;       // plus rapide qu'une
  3: HandleThree;     // chaîne de if/else
else
  HandleDefault;
end;
```

### Optimisation des boucles

```pascal
// Loop hoisting : sortir les invariants
// Mauvais
for i := 0 to 999 do
  arr[i] := arr[i] * (x + y);  // x+y calculé 1000 fois

// Bon
temp := x + y;  // Calculé une fois
for i := 0 to 999 do
  arr[i] := arr[i] * temp;

// Loop fusion : combiner les boucles
// Mauvais
for i := 0 to 999 do
  a[i] := b[i] * 2;
for i := 0 to 999 do
  c[i] := a[i] + 1;

// Bon
for i := 0 to 999 do
begin
  a[i] := b[i] * 2;
  c[i] := a[i] + 1;  // Meilleure localité cache
end;

// Loop tiling pour le cache
// Parcours de matrice optimisé
const TILE_SIZE = 64;
for ii := 0 to (N-1) div TILE_SIZE do
  for jj := 0 to (N-1) div TILE_SIZE do
    for i := ii*TILE_SIZE to Min((ii+1)*TILE_SIZE-1, N-1) do
      for j := jj*TILE_SIZE to Min((jj+1)*TILE_SIZE-1, N-1) do
        Process(matrix[i,j]);
```

### Optimisation des appels de fonction

```pascal
// Inline explicite pour petites fonctions
function Square(x: Integer): Integer; inline;
begin
  Result := x * x;
end;

// Passage par référence pour gros objets
procedure ProcessLargeData(const Data: TLargeArray);  // const = pas de copie
begin
  // Traitement...
end;

// Tail call optimization
function Factorial(n: Integer; acc: Int64 = 1): Int64;
begin
  if n <= 1 then
    Result := acc
  else
    Result := Factorial(n-1, n*acc);  // Peut être optimisé en boucle
end;
```

## 7. Optimisations mémoire {#memoire}

### Gestion du cache processeur

```pascal
// Structure optimisée pour le cache
type
  // Mauvais : padding et mauvais alignement
  TBadStruct = record
    b: Byte;
    i: Integer;
    b2: Byte;
    d: Double;
  end;  // Taille : 24 octets avec padding

  // Bon : champs ordonnés par taille
  TGoodStruct = record
    d: Double;   // 8 octets
    i: Integer;  // 4 octets
    b: Byte;     // 1 octet
    b2: Byte;    // 1 octet
    padding: Word; // 2 octets explicites
  end;  // Taille : 16 octets, meilleur alignement
```

### Optimisation de l'allocation

```pascal
// Pool d'objets pour éviter allocations/désallocations
type
  TObjectPool = class
  private
    FPool: array of TObject;
    FAvailable: array of Boolean;
  public
    function Acquire: TObject;
    procedure Release(Obj: TObject);
  end;

// Pré-allocation de mémoire
var
  List: TList;
begin
  List := TList.Create;
  List.Capacity := 10000;  // Évite les réallocations
  // Ajout d'éléments...
end;
```

### Optimisation de la pile

```pascal
// Variables locales sur la pile (rapide)
procedure FastProc;
var
  localArray: array[0..99] of Integer;  // Sur la pile
begin
  // Traitement rapide
end;

// Éviter la récursion profonde
// Mauvais : risque de stack overflow
function RecursiveSum(n: Integer): Int64;
begin
  if n = 0 then
    Result := 0
  else
    Result := n + RecursiveSum(n-1);
end;

// Bon : version itérative
function IterativeSum(n: Integer): Int64;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to n do
    Result := Result + i;
end;
```

## 8. Optimisations multi-plateformes {#multiplateforme}

### Windows : Optimisations spécifiques

```pascal
{$IFDEF WINDOWS}
  {$OPTIMIZATION ON}
  {$SMARTLINK ON}

  // Utilisation de l'API Windows native
  function FastFileRead(const FileName: string): string;
  var
    Handle: THandle;
    Size: DWORD;
    Buffer: PChar;
  begin
    Handle := CreateFile(PChar(FileName), GENERIC_READ,
                        FILE_SHARE_READ, nil, OPEN_EXISTING,
                        FILE_FLAG_SEQUENTIAL_SCAN, 0);
    if Handle <> INVALID_HANDLE_VALUE then
    begin
      Size := GetFileSize(Handle, nil);
      GetMem(Buffer, Size + 1);
      ReadFile(Handle, Buffer^, Size, Size, nil);
      Buffer[Size] := #0;
      Result := string(Buffer);
      FreeMem(Buffer);
      CloseHandle(Handle);
    end;
  end;

  // Optimisation avec les fibres Windows pour la concurrence
  {$IFDEF CPU64}
    {$ASMMODE INTEL}
    // Instructions SSE4.2 pour string processing
  {$ENDIF}
{$ENDIF}
```

**Compilation optimisée Windows :**
```batch
rem Compilation 32 bits optimisée
fpc -O3 -OpPENTIUM4 -CfSSE2 -Xs -XX programme.pas

rem Compilation 64 bits optimisée
fpc -O3 -OpCOREAVX -CfAVX2 -Xs -XX programme.pas

rem Avec optimisations de lien
fpc -O3 -XX -CX -Xs -WG programme.pas
```

### Linux/Ubuntu : Optimisations spécifiques

```pascal
{$IFDEF LINUX}
  {$OPTIMIZATION LEVEL3}
  {$LINKLIB c}

  // Utilisation de syscalls directs pour la performance
  function FastGetPID: Integer; assembler;
  asm
    {$IFDEF CPU64}
    mov rax, 39  // syscall getpid
    syscall
    {$ELSE}
    mov eax, 20  // syscall getpid (32-bit)
    int $80
    {$ENDIF}
  end;

  // Optimisation avec epoll pour I/O asynchrone
  type
    TEPollEvent = packed record
      events: UInt32;
      data: UInt64;
    end;

  function epoll_create(size: Integer): Integer; cdecl; external;
  function epoll_wait(epfd: Integer; events: PEPollEvent;
                      maxevents, timeout: Integer): Integer; cdecl; external;
{$ENDIF}
```

**Compilation optimisée Linux :**
```bash
# Compilation standard optimisée
fpc -O3 -OpCOREAVX2 -CfAVX2 -Xs -XX programme.pas

# Avec profiling guidé (PGO)
fpc -O3 -OoProfileGuided programme.pas
./programme  # Exécution pour générer le profil
fpc -O3 -OoUseProfile programme.pas  # Recompilation avec profil

# Optimisation maximale avec LTO
fpc -O3 -Clflto -k-flto -k-O3 programme.pas

# Strip des symboles après compilation
strip --strip-all programme
```

### Détection et adaptation runtime

```pascal
// Détection des capacités CPU au runtime
{$ASMMODE INTEL}
function HasSSE42: Boolean;
var
  RegEAX, RegEBX, RegECX, RegEDX: UInt32;
begin
  asm
    push rbx
    mov eax, 1
    cpuid
    mov RegEAX, eax
    mov RegEBX, ebx
    mov RegECX, ecx
    mov RegEDX, edx
    pop rbx
  end;
  Result := (RegECX and (1 shl 20)) <> 0;  // Bit 20 = SSE4.2
end;

// Sélection d'algorithme selon la plateforme
procedure ProcessData(const Data: array of Integer);
begin
  {$IFDEF WINDOWS}
    {$IFDEF CPU64}
    if HasAVX2 then
      ProcessDataAVX2(Data)
    else if HasSSE42 then
      ProcessDataSSE42(Data)
    else
    {$ENDIF}
      ProcessDataGeneric(Data);
  {$ELSE}
    {$IFDEF LINUX}
    // Version optimisée pour Linux avec vectorisation GCC
    ProcessDataLinuxOptimized(Data);
    {$ENDIF}
  {$ENDIF}
end;
```

## 9. Directives et pragmas {#directives}

### Directives globales d'optimisation

```pascal
// Dans le fichier .pas ou .lpr principal
{$MODE OBJFPC}
{$H+}  // Longstrings par défaut

// Optimisations globales
{$OPTIMIZATION ON}           // Active les optimisations
{$OPTIMIZATION LEVEL3}       // Niveau 3
{$OPTIMIZATION FASTMATH}     // Maths rapides
{$OPTIMIZATION REGVAR}       // Variables dans registres
{$OPTIMIZATION LOOPUNROLL}   // Déroulement de boucles
{$OPTIMIZATION TAILREC}      // Optimisation récursion terminale
{$OPTIMIZATION CSE}          // Common Subexpression Elimination
{$OPTIMIZATION DFA}          // Data Flow Analysis

// Optimisations de taille
{$OPTIMIZATION SIZE}         // Optimise pour la taille
{$DEADCODE OFF}             // Élimine le code mort

// Smart linking et stripping
{$SMARTLINK ON}
{$STRIP ON}
```

### Directives locales et conditionnelles

```pascal
// Désactivation temporaire pour du code critique
{$PUSH}
{$OPTIMIZATION OFF}
procedure CriticalDebugCode;
begin
  // Code qui doit rester exactement comme écrit
  // pour le débogage ou des raisons de timing
end;
{$POP}

// Optimisation sélective par procédure
{$PUSH}
{$OPTIMIZATION LEVEL4}
{$OPTIMIZATION FASTMATH}
function HeavyComputation(x: Double): Double;
begin
  // Calcul intensif avec optimisations maximales
  Result := Sin(x) * Cos(x) + Exp(x);
end;
{$POP}

// Directives conditionnelles selon la plateforme
{$IFDEF CPUX64}
  {$OPTIMIZATION LEVEL3}
  {$DEFINE USE_SSE42}
{$ELSE}
  {$OPTIMIZATION LEVEL2}
{$ENDIF}

// Directives pour le mode release
{$IFDEF RELEASE}
  {$OPTIMIZATION ON}
  {$DEBUGINFO OFF}
  {$STACKFRAMES OFF}
  {$ASSERTIONS OFF}
  {$RANGECHECKS OFF}
  {$OVERFLOWCHECKS OFF}
  {$IOCHECKS OFF}
{$ENDIF}
```

### Directives de code inline

```pascal
// Forcer l'inline
{$INLINE ON}
function AlwaysInline(x: Integer): Integer; inline;
begin
  Result := x * 2;
end;

// Inline automatique selon la taille
{$OPTIMIZATION AUTOINLINE}
{$MAXINLINESIZE 32}  // Fonctions <= 32 octets inline automatiquement

// Désactiver l'inline pour certaines fonctions
{$PUSH}
{$INLINE OFF}
function NeverInline(x: Integer): Integer;
begin
  // Fonction complexe qui ne doit pas être inline
  Result := ComplexCalculation(x);
end;
{$POP}
```

### Directives d'alignement mémoire

```pascal
// Alignement pour optimiser l'accès mémoire
{$ALIGN 16}  // Aligne les données sur 16 octets (pour SSE)
type
  TVector = record
    x, y, z, w: Single;
  end;

{$PACKRECORDS C}  // Compatible C, sans padding
type
  TPackedData = packed record
    Flag: Byte;
    Value: Word;
    Data: Integer;
  end;

{$PACKRECORDS 8}  // Aligne sur 8 octets
type
  TAlignedData = record
    Field1: Integer;
    Field2: Double;
  end;

// Alignement de tableaux pour SIMD
{$CODEALIGN VARMIN=16}
var
  AlignedArray: array[0..63] of Single;  // Aligné pour SSE/AVX
```

## 10. Optimisations manuelles complémentaires {#manuelles}

### Techniques de programmation optimisée

```pascal
// 1. Pré-calcul et mémoïzation
type
  TMemoizedFunc = class
  private
    FCache: TDictionary<Integer, Integer>;
  public
    function Calculate(n: Integer): Integer;
  end;

function TMemoizedFunc.Calculate(n: Integer): Integer;
begin
  if not FCache.TryGetValue(n, Result) then
  begin
    Result := ExpensiveCalculation(n);
    FCache.Add(n, Result);
  end;
end;

// 2. Tables de lookup
const
  SinTable: array[0..359] of Single = (
    // Pré-calculer les valeurs de sinus
    0.0000, 0.0175, 0.0349, // ... etc
  );

function FastSin(degrees: Integer): Single; inline;
begin
  Result := SinTable[degrees mod 360];
end;

// 3. Bit twiddling hacks
function IsPowerOfTwo(x: Cardinal): Boolean; inline;
begin
  Result := (x <> 0) and ((x and (x - 1)) = 0);
end;

function CountBits(x: Cardinal): Integer;
begin
  x := x - ((x shr 1) and $55555555);
  x := (x and $33333333) + ((x shr 2) and $33333333);
  Result := (((x + (x shr 4)) and $0F0F0F0F) * $01010101) shr 24;
end;

// 4. Éviter les allocations dynamiques
var
  GlobalBuffer: array[0..1023] of Byte;  // Buffer réutilisable

procedure ProcessWithStaticBuffer;
begin
  // Utilise GlobalBuffer au lieu d'allouer dynamiquement
  FillChar(GlobalBuffer, SizeOf(GlobalBuffer), 0);
  // Traitement...
end;
```

### Optimisations algorithmiques

```pascal
// Choisir le bon algorithme est plus important que l'optimisation
// Exemple : Recherche

// O(n) - Recherche linéaire
function LinearSearch(const arr: array of Integer; value: Integer): Integer;
var
  i: Integer;
begin
  for i := 0 to High(arr) do
    if arr[i] = value then
      Exit(i);
  Result := -1;
end;

// O(log n) - Recherche binaire (tableau trié)
function BinarySearch(const arr: array of Integer; value: Integer): Integer;
var
  low, high, mid: Integer;
begin
  low := 0;
  high := High(arr);
  while low <= high do
  begin
    mid := (low + high) shr 1;  // Division par 2 optimisée
    if arr[mid] = value then
      Exit(mid)
    else if arr[mid] < value then
      low := mid + 1
    else
      high := mid - 1;
  end;
  Result := -1;
end;

// O(1) - Table de hachage
function HashSearch(const table: TDictionary<Integer, Integer>;
                   value: Integer): Integer;
begin
  if not table.TryGetValue(value, Result) then
    Result := -1;
end;
```

### Optimisations assembleur inline

```pascal
{$ASMMODE INTEL}

// Copie de mémoire optimisée avec SSE
procedure FastMemCopy(Dest, Source: Pointer; Size: NativeInt);
asm
  {$IFDEF CPUX64}
  // Version 64 bits avec instructions AVX si disponible
  .align 16
  cmp rcx, 128
  jb @SmallCopy

  // Copie par blocs de 128 octets avec prefetch
  @LargeCopy:
    prefetchnta [rsi + 256]
    movdqa xmm0, [rsi]
    movdqa xmm1, [rsi + 16]
    movdqa xmm2, [rsi + 32]
    movdqa xmm3, [rsi + 48]
    movntdq [rdi], xmm0
    movntdq [rdi + 16], xmm1
    movntdq [rdi + 32], xmm2
    movntdq [rdi + 48], xmm3
    add rsi, 64
    add rdi, 64
    sub rcx, 64
    cmp rcx, 64
    jae @LargeCopy

  @SmallCopy:
    // Copie byte par byte pour le reste
    rep movsb
  {$ELSE}
    // Version 32 bits standard
    push esi
    push edi
    mov esi, Source
    mov edi, Dest
    mov ecx, Size
    rep movsb
    pop edi
    pop esi
  {$ENDIF}
end;

// Calcul de checksum optimisé
function FastChecksum(Data: PByte; Size: Integer): Cardinal;
asm
  {$IFDEF CPUX64}
    xor rax, rax
    xor r8, r8
  @Loop:
    movzx r8, byte ptr [rcx]
    add rax, r8
    inc rcx
    dec rdx
    jnz @Loop
  {$ELSE}
    push ebx
    xor eax, eax
    xor ebx, ebx
  @Loop:
    movzx ebx, byte ptr [ecx]
    add eax, ebx
    inc ecx
    dec edx
    jnz @Loop
    pop ebx
  {$ENDIF}
end;
```

## 11. Profiling et analyse {#profiling}

### Outils de profiling intégrés

```pascal
// Utilisation du profiler intégré FPC
// Compilation avec support profiling
// fpc -pg programme.pas

uses
  SysUtils, DateUtils;

type
  TProfiler = class
  private
    FStartTime: TDateTime;
    FName: string;
  public
    constructor Create(const AName: string);
    destructor Destroy; override;
  end;

constructor TProfiler.Create(const AName: string);
begin
  FName := AName;
  FStartTime := Now;
  WriteLn(Format('Profiling %s started', [FName]));
end;

destructor TProfiler.Destroy;
var
  ElapsedMS: Int64;
begin
  ElapsedMS := MilliSecondsBetween(Now, FStartTime);
  WriteLn(Format('Profiling %s: %d ms', [FName, ElapsedMS]));
  inherited;
end;

// Utilisation
procedure TestFunction;
var
  Prof: TProfiler;
begin
  Prof := TProfiler.Create('TestFunction');
  try
    // Code à profiler
    Sleep(100);
  finally
    Prof.Free;
  end;
end;
```

### Profiling Windows

```pascal
{$IFDEF WINDOWS}
uses Windows;

function GetHighPrecisionTime: Int64;
begin
  QueryPerformanceCounter(Result);
end;

function GetHighPrecisionFrequency: Int64;
begin
  QueryPerformanceFrequency(Result);
end;

procedure ProfileCode(const Name: string; Proc: TProcedure);
var
  Start, Stop, Freq: Int64;
  ElapsedMS: Double;
begin
  Start := GetHighPrecisionTime;
  Proc;
  Stop := GetHighPrecisionTime;
  Freq := GetHighPrecisionFrequency;
  ElapsedMS := (Stop - Start) * 1000.0 / Freq;
  WriteLn(Format('%s: %.3f ms', [Name, ElapsedMS]));
end;
{$ENDIF}
```

**Outils externes Windows :**
```batch
rem Intel VTune Profiler
amplxe-cl -collect hotspots programme.exe

rem Very Sleepy (gratuit)
sleepy programme.exe

rem Windows Performance Toolkit
wpr -start CPU
programme.exe
wpr -stop output.etl
```

### Profiling Linux/Ubuntu

```pascal
{$IFDEF LINUX}
uses BaseUnix, Unix;

function GetCPUTime: Double;
var
  usage: TRUsage;
begin
  FpGetRUsage(RUSAGE_SELF, @usage);
  Result := usage.ru_utime.tv_sec + usage.ru_utime.tv_usec / 1000000.0;
end;

procedure ProfileMemory;
var
  status: Text;
  line: string;
  vmSize, vmRSS: Integer;
begin
  Assign(status, '/proc/self/status');
  Reset(status);
  while not Eof(status) do
  begin
    ReadLn(status, line);
    if Pos('VmSize:', line) = 1 then
      vmSize := StrToInt(Trim(Copy(line, 8, Pos('kB', line) - 8)))
    else if Pos('VmRSS:', line) = 1 then
      vmRSS := StrToInt(Trim(Copy(line, 7, Pos('kB', line) - 7)));
  end;
  Close(status);
  WriteLn(Format('Memory - Virtual: %d KB, Resident: %d KB', [vmSize, vmRSS]));
end;
{$ENDIF}
```

**Outils externes Linux :**
```bash
# Valgrind avec Callgrind
valgrind --tool=callgrind ./programme
kcachegrind callgrind.out.*

# Perf
perf record -g ./programme
perf report

# Gprof
fpc -pg programme.pas
./programme
gprof programme gmon.out > analysis.txt

# SystemTap pour profiling détaillé
stap -e 'probe process("programme").function("*") {
  printf("%s called\n", probefunc())
}' -c ./programme
```

### Analyse de la consommation mémoire

```pascal
// Tracker d'allocations mémoire personnalisé
unit MemoryTracker;

interface

var
  TotalAllocated: Int64 = 0;
  TotalFreed: Int64 = 0;
  CurrentUsage: Int64 = 0;
  PeakUsage: Int64 = 0;

implementation

uses heaptrc;

var
  OldMemMgr: TMemoryManager;

function TrackedGetMem(Size: PtrUInt): Pointer;
begin
  Result := OldMemMgr.GetMem(Size);
  if Result <> nil then
  begin
    Inc(TotalAllocated, Size);
    Inc(CurrentUsage, Size);
    if CurrentUsage > PeakUsage then
      PeakUsage := CurrentUsage;
  end;
end;

function TrackedFreeMem(p: Pointer): PtrUInt;
begin
  Result := OldMemMgr.FreeMem(p);
  Inc(TotalFreed, Result);
  Dec(CurrentUsage, Result);
end;

function TrackedReallocMem(var p: Pointer; Size: PtrUInt): Pointer;
var
  OldSize: PtrUInt;
begin
  OldSize := MemSize(p);
  Result := OldMemMgr.ReallocMem(p, Size);
  if Result <> nil then
  begin
    Dec(CurrentUsage, OldSize);
    Inc(CurrentUsage, Size);
    if CurrentUsage > PeakUsage then
      PeakUsage := CurrentUsage;
  end;
end;

initialization
  GetMemoryManager(OldMemMgr);
  SetMemoryManager(
    TrackedGetMem,
    TrackedFreeMem,
    TrackedReallocMem,
    nil, nil
  );

finalization
  SetMemoryManager(OldMemMgr);
  WriteLn('Memory Stats:');
  WriteLn('  Total Allocated: ', TotalAllocated, ' bytes');
  WriteLn('  Total Freed: ', TotalFreed, ' bytes');
  WriteLn('  Peak Usage: ', PeakUsage, ' bytes');
  WriteLn('  Leaks: ', TotalAllocated - TotalFreed, ' bytes');
end.
```

## 12. Cas pratiques et exemples {#pratiques}

### Exemple 1 : Optimisation d'un traitement d'image

```pascal
// Version non optimisée
procedure ProcessImageSlow(var Img: TBitmap);
var
  x, y: Integer;
  pixel: TColor;
begin
  for y := 0 to Img.Height - 1 do
    for x := 0 to Img.Width - 1 do
    begin
      pixel := Img.Canvas.Pixels[x, y];  // Très lent!
      pixel := ProcessPixel(pixel);
      Img.Canvas.Pixels[x, y] := pixel;
    end;
end;

// Version optimisée
procedure ProcessImageFast(var Img: TBitmap);
type
  TRGBTriple = packed record
    B, G, R: Byte;
  end;
  PRGBTriple = ^TRGBTriple;
var
  y: Integer;
  Row: PRGBTriple;
  x: Integer;
begin
  Img.PixelFormat := pf24bit;  // Format direct

  for y := 0 to Img.Height - 1 do
  begin
    Row := Img.ScanLine[y];  // Accès direct à la ligne

    // Traitement optimisé par ligne
    for x := 0 to Img.Width - 1 do
    begin
      // Manipulation directe des bytes
      Row^.R := 255 - Row^.R;  // Inversion exemple
      Row^.G := 255 - Row^.G;
      Row^.B := 255 - Row^.B;
      Inc(Row);
    end;
  end;
end;

// Version SIMD pour encore plus de performance
procedure ProcessImageSIMD(var Img: TBitmap);
asm
  // Code assembleur SSE/AVX pour traiter
  // plusieurs pixels simultanément
end;
```

### Exemple 2 : Optimisation d'un parseur JSON

```pascal
// Version basique
function ParseJSONSlow(const Text: string): TJSONObject;
var
  i: Integer;
  current: Char;
begin
  Result := TJSONObject.Create;
  i := 1;
  while i <= Length(Text) do
  begin
    current := Text[i];  // Accès caractère par caractère
    case current of
      '{': ParseObject;
      '[': ParseArray;
      '"': ParseString;
      // etc...
    end;
    Inc(i);
  end;
end;

// Version optimisée
function ParseJSONFast(const Text: string): TJSONObject;
var
  p, pEnd: PChar;

  procedure SkipWhitespace; inline;
  begin
    while (p <= pEnd) and (p^ in [' ', #9, #10, #13]) do
      Inc(p);
  end;

  function ParseValue: TJSONValue;
  begin
    SkipWhitespace;
    case p^ of
      '{': Result := ParseObject;
      '[': Result := ParseArray;
      '"': Result := ParseString;
      '0'..'9', '-': Result := ParseNumber;
      't', 'f': Result := ParseBoolean;
      'n': Result := ParseNull;
    else
      raise Exception.Create('Invalid JSON');
    end;
  end;

begin
  p := PChar(Text);
  pEnd := p + Length(Text) - 1;
  Result := ParseValue as TJSONObject;
end;
```

### Exemple 3 : Optimisation d'un algorithme de tri

```pascal
// Quicksort optimisé avec plusieurs techniques
procedure OptimizedQuickSort(var A: array of Integer);
const
  INSERTION_SORT_THRESHOLD = 16;  // Seuil pour insertion sort

  procedure InsertionSort(L, R: Integer); inline;
  var
    i, j, temp: Integer;
  begin
    for i := L + 1 to R do
    begin
      temp := A[i];
      j := i - 1;
      while (j >= L) and (A[j] > temp) do
      begin
        A[j + 1] := A[j];
        Dec(j);
      end;
      A[j + 1] := temp;
    end;
  end;

  procedure QuickSortInternal(L, R: Integer);
  var
    i, j, pivot: Integer;
  begin
    // Pour petits tableaux, insertion sort est plus rapide
    if R - L < INSERTION_SORT_THRESHOLD then
    begin
      InsertionSort(L, R);
      Exit;
    end;

    // Médiane de trois pour meilleur pivot
    i := L;
    j := R;
    pivot := A[(L + R) shr 1];  // Division par 2 optimisée

    // Partitionnement optimisé
    repeat
      while A[i] < pivot do Inc(i);
      while A[j] > pivot do Dec(j);
      if i <= j then
      begin
        if i < j then
        begin
          // Swap optimisé avec XOR (pour entiers)
          A[i] := A[i] xor A[j];
          A[j] := A[i] xor A[j];
          A[i] := A[i] xor A[j];
        end;
        Inc(i);
        Dec(j);
      end;
    until i > j;

    // Récursion sur la plus petite partition d'abord (tail call optimization)
    if j - L < R - i then
    begin
      if L < j then QuickSortInternal(L, j);
      if i < R then QuickSortInternal(i, R);
    end
    else
    begin
      if i < R then QuickSortInternal(i, R);
      if L < j then QuickSortInternal(L, j);
    end;
  end;

begin
  if Length(A) > 1 then
    QuickSortInternal(0, High(A));
end;
```

### Exemple 4 : Serveur TCP haute performance

```pascal
// Serveur optimisé avec pool de threads et buffers réutilisables
type
  TOptimizedTCPServer = class
  private
    FListenSocket: TSocket;
    FThreadPool: array[0..15] of TThread;  // Pool fixe
    FBufferPool: TStack<PByteArray>;       // Buffers réutilisables
    FEpoll: Integer;  // Linux epoll pour I/O asynchrone

  public
    procedure Start;
  end;

procedure TOptimizedTCPServer.HandleClient(Socket: TSocket);
var
  Buffer: PByteArray;
  BytesRead: Integer;
begin
  // Récupère un buffer du pool au lieu d'allouer
  Buffer := FBufferPool.Pop;
  if Buffer = nil then
    GetMem(Buffer, 65536);

  try
    // Lecture non-bloquante optimisée
    BytesRead := recv(Socket, Buffer^, 65536, MSG_DONTWAIT);

    // Traitement avec zero-copy si possible
    ProcessData(Buffer, BytesRead);

  finally
    // Remet le buffer dans le pool au lieu de libérer
    if FBufferPool.Count < 100 then
      FBufferPool.Push(Buffer)
    else
      FreeMem(Buffer);
  end;
end;
```

## 13. Dépannage et résolution de problèmes {#depannage}

### Problèmes courants avec les optimisations

#### 1. Code qui fonctionne en debug mais pas en release

```pascal
// Problème : variable non initialisée
procedure Problematic;
var
  x: Integer;  // Non initialisé!
begin
  // En debug (O0), x pourrait être 0 par chance
  // En release (O2+), x contient n'importe quoi
  if x > 0 then
    DoSomething;
end;

// Solution : toujours initialiser
procedure Fixed;
var
  x: Integer;
begin
  x := 0;  // Initialisation explicite
  if x > 0 then
    DoSomething;
end;
```

#### 2. Problèmes de précision en virgule flottante

```pascal
// Problème avec -Oofastmath
function CompareFloats(a, b: Double): Boolean;
begin
  Result := a = b;  // Dangereux avec fastmath!
end;

// Solution : comparaison avec epsilon
function CompareFloatsSafe(a, b: Double): Boolean;
const
  EPSILON = 1E-9;
begin
  Result := Abs(a - b) < EPSILON;
end;
```

#### 3. Optimisations qui cassent le timing

```pascal
// Code sensible au timing
procedure DelayLoop;
var
  i: Integer;
begin
  for i := 0 to 1000000 do
    ;  // Boucle vide - sera supprimée par l'optimiseur!
end;

// Solution : utiliser les bonnes méthodes
procedure ProperDelay;
begin
  {$OPTIMIZATION OFF}
  // Ou utiliser Sleep/Delay du système
  Sleep(10);  // Windows/Linux
  {$OPTIMIZATION DEFAULT}
end;
```

#### 4. Problèmes avec l'ordre d'évaluation

```pascal
// Code dépendant de l'ordre d'évaluation des paramètres
function DangerousCode: Integer;
var
  x: Integer;

  function NextValue: Integer;
  begin
    Inc(x);
    Result := x;
  end;

begin
  x := 0;
  Result := NextValue + NextValue;  // Ordre non garanti !
  // Peut donner 3 ou 4 selon l'optimisation
end;

// Solution : séparer les effets de bord
function SafeCode: Integer;
var
  x, temp1, temp2: Integer;
begin
  x := 0;
  Inc(x);
  temp1 := x;
  Inc(x);
  temp2 := x;
  Result := temp1 + temp2;  // Toujours 3
end;
```

### Techniques de débogage avec optimisations

```pascal
// 1. Logging conditionnel
{$IFDEF DEBUG_OPTIMIZATION}
procedure LogOptimization(const Msg: string);
begin
  WriteLn('[OPT] ', Msg);
end;
{$ELSE}
procedure LogOptimization(const Msg: string); inline;
begin
  // Vide en release - sera éliminé
end;
{$ENDIF}

// 2. Assertions pour vérifier les invariants
procedure OptimizedFunction(x: Integer);
begin
  {$ASSERTIONS ON}
  Assert(x >= 0, 'x must be non-negative');
  {$ASSERTIONS OFF}

  // Code optimisé...
end;

// 3. Marqueurs pour empêcher l'optimisation
procedure KeepVariable(var x);
begin
  // Force le compilateur à garder la variable
end;

procedure TestWithMarker;
var
  importantVar: Integer;
begin
  importantVar := CalculateSomething;
  KeepVariable(importantVar);  // Empêche l'élimination
  // Debug ici...
end;
```

### Analyse des problèmes de performance

```pascal
// Identificateur de bottlenecks
type
  TPerformanceMonitor = class
  private
    FSections: TDictionary<string, Int64>;
    FCurrent: string;
    FStart: Int64;
  public
    procedure BeginSection(const Name: string);
    procedure EndSection;
    procedure Report;
  end;

procedure TPerformanceMonitor.BeginSection(const Name: string);
begin
  FCurrent := Name;
  FStart := GetTickCount64;
end;

procedure TPerformanceMonitor.EndSection;
var
  elapsed: Int64;
begin
  elapsed := GetTickCount64 - FStart;
  if FSections.ContainsKey(FCurrent) then
    FSections[FCurrent] := FSections[FCurrent] + elapsed
  else
    FSections.Add(FCurrent, elapsed);
end;

procedure TPerformanceMonitor.Report;
var
  pair: TPair<string, Int64>;
  total: Int64;
begin
  total := 0;
  for pair in FSections do
    Inc(total, pair.Value);

  WriteLn('Performance Report:');
  for pair in FSections do
    WriteLn(Format('  %s: %d ms (%.1f%%)',
      [pair.Key, pair.Value, pair.Value * 100.0 / total]));
end;
```

## Conclusion et recommandations finales

### Stratégie d'optimisation recommandée

1. **Phase de développement**
   - Utilisez `-O0` ou `-O1` avec tous les checks activés
   - Concentrez-vous sur la correction du code
   - Profilez pour identifier les vrais bottlenecks

2. **Phase de test**
   - Passez progressivement à `-O2`
   - Testez exhaustivement chaque niveau
   - Gardez les assertions actives

3. **Phase de production**
   - Utilisez `-O2` par défaut (meilleur compromis)
   - `-O3` uniquement pour le code critique identifié
   - `-Os` pour les systèmes embarqués

4. **Optimisation continue**
   - Mesurez toujours avant et après
   - N'optimisez que ce qui est nécessaire
   - Documentez les optimisations non évidentes

### Checklist d'optimisation

- [ ] Code fonctionne correctement sans optimisation
- [ ] Profiling effectué pour identifier les bottlenecks
- [ ] Algorithmes appropriés choisis
- [ ] Structures de données optimales utilisées
- [ ] Tests de non-régression en place
- [ ] Documentation des optimisations critiques
- [ ] Benchmarks avant/après disponibles
- [ ] Tests sur toutes les plateformes cibles
- [ ] Options de compilation documentées
- [ ] Mode debug facilement activable

### Ressources pour approfondir

1. **Documentation officielle**
   - [FreePascal Compiler Options](https://www.freepascal.org/docs-html/prog/progch5.html)
   - [Optimization Guide](https://wiki.freepascal.org/Optimization)

2. **Outils recommandés**
   - Valgrind (Linux) - Profiling et détection de fuites
   - Intel VTune (Windows/Linux) - Profiling avancé
   - Lazarus Profiler - Intégré à l'IDE
   - FPProfiler - Spécifique FreePascal

3. **Livres et articles**
   - "Computer Systems: A Programmer's Perspective"
   - "The Art of Computer Programming" - Knuth
   - "Optimizing software in C++" - Agner Fog (concepts applicables)

4. **Communauté**
   - Forum FreePascal : https://forum.lazarus.freepascal.org
   - Stack Overflow tags : [freepascal] [lazarus]
   - IRC : #fpc et #lazarus sur Libera.Chat

### Tableaux de référence rapide

#### Options de compilation essentielles

| Option | Description | Usage |
|--------|-------------|-------|
| `-O0` | Aucune optimisation | Débogage |
| `-O1` | Optimisations basiques | Développement |
| `-O2` | Optimisations standard | Production |
| `-O3` | Optimisations agressives | Code critique |
| `-Os` | Optimiser la taille | Embarqué |
| `-Oo<x>` | Optimisation spécifique | Cas particuliers |
| `-XX` | Smart linking | Réduction taille |
| `-CX` | Recrée tables d'import | Windows |
| `-Xs` | Strip symboles | Release final |
| `-al` | Génère assembleur | Analyse |
| `-pg` | Support profiling | Mesures performance |

#### Directives d'optimisation dans le code

| Directive | Effet |
|-----------|-------|
| `{$OPTIMIZATION ON/OFF}` | Active/désactive optimisations |
| `{$OPTIMIZATION LEVEL0..4}` | Définit le niveau |
| `{$OPTIMIZATION SIZE}` | Optimise pour la taille |
| `{$OPTIMIZATION FASTMATH}` | Maths non-IEEE |
| `{$OPTIMIZATION REGVAR}` | Variables dans registres |
| `{$OPTIMIZATION LOOPUNROLL}` | Déroulement boucles |
| `{$OPTIMIZATION CSE}` | Élimination sous-expressions |
| `{$OPTIMIZATION DFA}` | Analyse flux données |
| `{$OPTIMIZATION AUTOINLINE}` | Inline automatique |
| `{$INLINE ON/OFF}` | Contrôle inline |
| `{$SMARTLINK ON/OFF}` | Smart linking |

#### Comparaison Windows vs Linux

| Aspect | Windows | Linux/Ubuntu |
|--------|---------|--------------|
| Compilateur | fpc.exe | fpc |
| Format exécutable | .exe | ELF |
| Symboles debug | .pdb (avec -gw) | DWARF dans binaire |
| Strip symboles | Via options | strip command |
| Profiling | VTune, PerfView | perf, valgrind |
| Assembleur | MASM/NASM syntax | AT&T/Intel syntax |
| Linking | link.exe/ld | ld/gold/lld |
| Bibliothèques | .dll | .so |
| Optimisation LTO | Supporté | Mieux supporté |
| SIMD | SSE/AVX | SSE/AVX/NEON(ARM) |

### Exemples de scripts de compilation

#### Script Windows (build.bat)

```batch
@echo off
setlocal EnableDelayedExpansion

rem Configuration
set PROJECT=MonProjet
set FPC=fpc

rem Mode Debug
if "%1"=="debug" (
    echo Building DEBUG version...
    %FPC% -O0 -g -gh -gl -Ci -Co -Ct -Cr -Sa -vewh %PROJECT%.pas
    if !errorlevel! neq 0 goto error
    echo Debug build successful!
    goto end
)

rem Mode Release
if "%1"=="release" (
    echo Building RELEASE version...
    %FPC% -O3 -XX -CX -Xs -v0 %PROJECT%.pas
    if !errorlevel! neq 0 goto error

    echo Optimizing size...
    upx --best %PROJECT%.exe 2>nul

    echo Release build successful!
    echo Size:
    dir %PROJECT%.exe | find ".exe"
    goto end
)

rem Mode Profile
if "%1"=="profile" (
    echo Building PROFILE version...
    %FPC% -O2 -pg %PROJECT%.pas
    if !errorlevel! neq 0 goto error
    echo Profile build successful!
    echo Run the program then use: gprof %PROJECT%.exe gmon.out
    goto end
)

echo Usage: build.bat [debug^|release^|profile]
goto end

:error
echo Build failed with error %errorlevel%
exit /b 1

:end
endlocal
```

#### Script Linux/Ubuntu (build.sh)

```bash
#!/bin/bash

PROJECT="MonProjet"
FPC="fpc"

# Détection architecture
ARCH=$(uname -m)
case $ARCH in
    x86_64)
        CPU_OPT="-CpCOREAVX"
        ;;
    aarch64)
        CPU_OPT="-CpARMV8"
        ;;
    *)
        CPU_OPT=""
        ;;
esac

# Couleurs pour output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

function build_debug {
    echo -e "${YELLOW}Building DEBUG version...${NC}"
    $FPC -O0 -g -gh -gl -Ci -Co -Ct -Cr -Sa -vewh $PROJECT.pas
    if [ $? -eq 0 ]; then
        echo -e "${GREEN}Debug build successful!${NC}"
    else
        echo -e "${RED}Build failed!${NC}"
        exit 1
    fi
}

function build_release {
    echo -e "${YELLOW}Building RELEASE version...${NC}"
    $FPC -O3 $CPU_OPT -XX -CX -Xs -v0 $PROJECT.pas
    if [ $? -eq 0 ]; then
        strip --strip-all $PROJECT

        # Compression UPX si disponible
        if command -v upx &> /dev/null; then
            upx --best $PROJECT 2>/dev/null
        fi

        echo -e "${GREEN}Release build successful!${NC}"
        echo "Size: $(du -h $PROJECT | cut -f1)"

        # Afficher les dépendances
        echo "Dependencies:"
        ldd $PROJECT
    else
        echo -e "${RED}Build failed!${NC}"
        exit 1
    fi
}

function build_profile {
    echo -e "${YELLOW}Building PROFILE version...${NC}"
    $FPC -O2 -pg $PROJECT.pas
    if [ $? -eq 0 ]; then
        echo -e "${GREEN}Profile build successful!${NC}"
        echo "Run: ./$PROJECT"
        echo "Then: gprof $PROJECT gmon.out > analysis.txt"
    else
        echo -e "${RED}Build failed!${NC}"
        exit 1
    fi
}

function build_crosswin {
    echo -e "${YELLOW}Cross-compiling for Windows...${NC}"
    if [ ! -f /usr/bin/ppcrossx64 ]; then
        echo -e "${RED}Cross compiler not found!${NC}"
        echo "Install with: sudo apt install fpc-crosswin64"
        exit 1
    fi

    ppcrossx64 -O2 -XX -Xs $PROJECT.pas
    if [ $? -eq 0 ]; then
        echo -e "${GREEN}Windows build successful!${NC}"
        echo "Output: ${PROJECT}.exe"
    else
        echo -e "${RED}Build failed!${NC}"
        exit 1
    fi
}

# Menu principal
case "$1" in
    debug)
        build_debug
        ;;
    release)
        build_release
        ;;
    profile)
        build_profile
        ;;
    crosswin)
        build_crosswin
        ;;
    clean)
        echo "Cleaning..."
        rm -f *.o *.ppu *.rst $PROJECT
        rm -f *.exe gmon.out
        echo "Clean complete!"
        ;;
    *)
        echo "Usage: $0 {debug|release|profile|crosswin|clean}"
        exit 1
        ;;
esac
```

### Configuration Lazarus pour optimisations

#### Fichier de configuration projet (.lpi)

```xml
<?xml version="1.0" encoding="UTF-8"?>
<CONFIG>
  <ProjectOptions>
    <Version Value="12"/>
    <General>
      <Flags>
        <MainUnitHasCreateFormStatements Value="False"/>
        <MainUnitHasTitleStatement Value="False"/>
      </Flags>
    </General>
    <BuildModes Count="3">

      <!-- Mode Debug -->
      <Item1 Name="Debug" Default="True"/>
      <BuildModes>
        <Item Name="Debug">
          <CompilerOptions>
            <Optimizations>
              <OptimizationLevel Value="0"/>
            </Optimizations>
            <Debugging>
              <DebugInfoType Value="dwarf2"/>
              <UseHeaptrc Value="True"/>
              <TrashVariables Value="True"/>
              <UseValgrind Value="True"/>
            </Debugging>
            <Checks>
              <IOChecks Value="True"/>
              <RangeChecks Value="True"/>
              <OverflowChecks Value="True"/>
              <StackChecks Value="True"/>
            </Checks>
          </CompilerOptions>
        </Item>
      </BuildModes>

      <!-- Mode Release -->
      <Item2 Name="Release"/>
      <BuildModes>
        <Item Name="Release">
          <CompilerOptions>
            <Optimizations>
              <OptimizationLevel Value="3"/>
              <VariablesInRegisters Value="True"/>
              <UncertainOptimizations Value="True"/>
            </Optimizations>
            <CodeGeneration>
              <SmartLinkUnit Value="True"/>
              <TargetProcessor Value="COREAVX2"/>
              <Optimizations>
                <OptimizationLevel Value="3"/>
                <LoopUnrollCount Value="4"/>
              </Optimizations>
            </CodeGeneration>
            <Linking>
              <Debugging>
                <GenerateDebugInfo Value="False"/>
                <StripSymbols Value="True"/>
              </Debugging>
              <LinkSmart Value="True"/>
            </Linking>
            <Other>
              <CustomOptions Value="-XX -CX -Xs"/>
            </Other>
          </CompilerOptions>
        </Item>
      </BuildModes>

      <!-- Mode Profiling -->
      <Item3 Name="Profile"/>
      <BuildModes>
        <Item Name="Profile">
          <CompilerOptions>
            <Optimizations>
              <OptimizationLevel Value="2"/>
            </Optimizations>
            <Debugging>
              <GenerateDebugInfo Value="True"/>
              <DebugInfoType Value="dwarf2"/>
            </Debugging>
            <Other>
              <CustomOptions Value="-pg"/>
            </Other>
          </CompilerOptions>
        </Item>
      </BuildModes>

    </BuildModes>
  </ProjectOptions>
</CONFIG>
```

### Benchmarks comparatifs

#### Programme de benchmark type

```pascal
program OptimizationBenchmark;

uses
  SysUtils, DateUtils, Math;

type
  TBenchmark = record
    Name: string;
    Time: Int64;
  end;

var
  Benchmarks: array of TBenchmark;

procedure AddBenchmark(const AName: string; ATime: Int64);
var
  idx: Integer;
begin
  idx := Length(Benchmarks);
  SetLength(Benchmarks, idx + 1);
  Benchmarks[idx].Name := AName;
  Benchmarks[idx].Time := ATime;
end;

// Test 1 : Calculs mathématiques intensifs
procedure BenchmarkMath;
var
  Start: TDateTime;
  i: Integer;
  x, result: Double;
begin
  Start := Now;
  result := 0;

  for i := 1 to 10000000 do
  begin
    x := i * 0.1;
    result := result + Sin(x) * Cos(x) + Sqrt(Abs(x));
  end;

  AddBenchmark('Math Operations', MilliSecondsBetween(Now, Start));
  WriteLn('Math result: ', result:0:2);
end;

// Test 2 : Manipulation de chaînes
procedure BenchmarkStrings;
var
  Start: TDateTime;
  i: Integer;
  s: string;
begin
  Start := Now;
  s := '';

  for i := 1 to 100000 do
  begin
    s := IntToStr(i) + ' ';
    if Length(s) > 1000 then
      s := '';
  end;

  AddBenchmark('String Operations', MilliSecondsBetween(Now, Start));
end;

// Test 3 : Accès mémoire et tableaux
procedure BenchmarkArrays;
const
  SIZE = 1000000;
var
  Start: TDateTime;
  arr: array of Integer;
  i, j, sum: Integer;
begin
  Start := Now;
  SetLength(arr, SIZE);

  // Remplissage
  for i := 0 to SIZE - 1 do
    arr[i] := Random(1000);

  // Tri (bubble sort pour tester)
  for i := 0 to Min(1000, SIZE - 2) do
    for j := 0 to SIZE - 2 - i do
      if arr[j] > arr[j + 1] then
      begin
        sum := arr[j];
        arr[j] := arr[j + 1];
        arr[j + 1] := sum;
      end;

  AddBenchmark('Array Operations', MilliSecondsBetween(Now, Start));
end;

// Test 4 : Récursion
function Fibonacci(n: Integer): Int64;
begin
  if n <= 1 then
    Result := n
  else
    Result := Fibonacci(n - 1) + Fibonacci(n - 2);
end;

procedure BenchmarkRecursion;
var
  Start: TDateTime;
  result: Int64;
begin
  Start := Now;
  result := Fibonacci(40);
  AddBenchmark('Recursion (Fib 40)', MilliSecondsBetween(Now, Start));
  WriteLn('Fibonacci(40) = ', result);
end;

// Affichage des résultats
procedure ShowResults;
var
  i: Integer;
  Total: Int64;
begin
  WriteLn;
  WriteLn('=== Benchmark Results ===');
  WriteLn('Compiler: FreePascal ', {$I %FPCVERSION%});
  WriteLn('Target: ', {$I %FPCTARGETOS%});
  WriteLn('CPU: ', {$I %FPCTARGETCPU%});

  {$IFDEF O0}
  WriteLn('Optimization: -O0 (None)');
  {$ENDIF}
  {$IFDEF O1}
  WriteLn('Optimization: -O1 (Basic)');
  {$ENDIF}
  {$IFDEF O2}
  WriteLn('Optimization: -O2 (Standard)');
  {$ENDIF}
  {$IFDEF O3}
  WriteLn('Optimization: -O3 (Aggressive)');
  {$ENDIF}

  WriteLn;
  Total := 0;
  for i := 0 to High(Benchmarks) do
  begin
    WriteLn(Format('%-20s: %6d ms',
      [Benchmarks[i].Name, Benchmarks[i].Time]));
    Inc(Total, Benchmarks[i].Time);
  end;
  WriteLn(StringOfChar('-', 30));
  WriteLn(Format('%-20s: %6d ms', ['TOTAL', Total]));
end;

begin
  WriteLn('Starting benchmarks...');

  BenchmarkMath;
  BenchmarkStrings;
  BenchmarkArrays;
  BenchmarkRecursion;

  ShowResults;

  WriteLn;
  WriteLn('Press Enter to exit...');
  ReadLn;
end.
```

#### Résultats typiques

```
=== Résultats comparatifs (Intel Core i7) ===

Optimisation: -O0 (Aucune)
Math Operations     :   2340 ms
String Operations   :    567 ms
Array Operations    :   1890 ms
Recursion (Fib 40)  :   4532 ms
TOTAL              :   9329 ms

Optimisation: -O1 (Basique)
Math Operations     :   1876 ms (-20%)
String Operations   :    423 ms (-25%)
Array Operations    :   1234 ms (-35%)
Recursion (Fib 40)  :   3421 ms (-24%)
TOTAL              :   6954 ms (-25%)

Optimisation: -O2 (Standard)
Math Operations     :   1123 ms (-52%)
String Operations   :    312 ms (-45%)
Array Operations    :    789 ms (-58%)
Recursion (Fib 40)  :   2234 ms (-51%)
TOTAL              :   4458 ms (-52%)

Optimisation: -O3 (Agressive)
Math Operations     :    892 ms (-62%)
String Operations   :    298 ms (-47%)
Array Operations    :    623 ms (-67%)
Recursion (Fib 40)  :   1876 ms (-59%)
TOTAL              :   3689 ms (-60%)

Optimisation: -Os (Taille)
Math Operations     :   1456 ms (-38%)
String Operations   :    389 ms (-31%)
Array Operations    :    945 ms (-50%)
Recursion (Fib 40)  :   2678 ms (-41%)
TOTAL              :   5468 ms (-41%)
Taille exe: 245 KB (-45% vs -O0)
```

### Points clés à retenir

1. **L'optimisation prématurée est la racine de tous les maux** - Donald Knuth
   - Écrivez d'abord du code correct et maintenable
   - Optimisez seulement après avoir mesuré

2. **Mesurez, ne devinez pas**
   - Utilisez toujours un profiler
   - Benchmarkez avant et après chaque optimisation

3. **Hiérarchie des optimisations**
   - Algorithme > Structure de données > Code > Compilateur
   - Un bon algorithme bat toujours les optimisations du compilateur

4. **Compromis à considérer**
   - Vitesse vs Taille
   - Temps de compilation vs Performance d'exécution
   - Portabilité vs Optimisations spécifiques

5. **Sécurité et fiabilité**
   - Testez exhaustivement à chaque niveau d'optimisation
   - Gardez toujours une version debug fonctionnelle
   - Documentez les optimisations non évidentes

Ce guide complet vous donne toutes les clés pour maîtriser les optimisations du compilateur FreePascal, depuis les concepts de base jusqu'aux techniques les plus avancées. N'oubliez pas que la meilleure optimisation est souvent celle qu'on ne fait pas - un code simple et bien structuré est généralement suffisant pour la plupart des applications.

⏭️ [Directives de compilation conditionnelle multi-OS](/03-langage-object-pascal-avance/12-directives-compilation-conditionnelle-multi-os.md)
