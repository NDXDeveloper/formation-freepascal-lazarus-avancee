🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 14.10 Optimisation mémoire pour embarqué

## Introduction

Les systèmes embarqués disposent de **ressources mémoire très limitées** comparé aux PC modernes. Là où un PC possède des gigaoctets de RAM, un microcontrôleur typique n'a que quelques kilooctets.

**Exemples de contraintes mémoire** :

| Microcontrôleur | Flash (code) | RAM (données) | Prix indicatif |
|-----------------|--------------|---------------|----------------|
| ATtiny85 (AVR) | 8 Ko | 512 octets | ~1€ |
| STM32F103C8 | 64 Ko | 20 Ko | ~2€ |
| ESP32 | 4 Mo | 520 Ko | ~5€ |
| STM32H743 | 2 Mo | 1 Mo | ~15€ |
| PC moderne | > 256 Go SSD | > 8 Go RAM | > 500€ |

Sur un ATtiny85 avec 512 octets de RAM, **chaque octet compte** ! L'optimisation mémoire n'est pas une option mais une nécessité absolue.

## Types de mémoire embarquée

### Flash (mémoire programme)

La **Flash** est une mémoire **non-volatile** (persistante) où réside votre programme compilé.

```pascal
// Ce code est stocké en Flash
procedure CalculerTemperature;
begin
  Temperature := (ADC_Value * 3.3) / 4096.0;
end;

// Ces constantes sont également en Flash
const
  MESSAGE_BIENVENUE = 'Système initialisé';
  TABLE_SINUS: array[0..359] of Integer = (0, 17, 34, ...);
```

**Caractéristiques** :
- ✓ Grande taille (64 Ko à plusieurs Mo)
- ✓ Conservée à l'extinction
- ✗ Lecture seule à l'exécution
- ✗ Nombre d'écritures limité (10k-100k cycles)

### RAM (mémoire de travail)

La **RAM** est une mémoire **volatile** (effacée à l'extinction) pour les données dynamiques.

```pascal
// Variables en RAM
var
  Compteur: Integer;           // 2 ou 4 octets
  Buffer: array[0..99] of Byte; // 100 octets
  Temperature: Real;            // 4 ou 8 octets
```

**Caractéristiques** :
- ✓ Lecture/écriture rapide
- ✓ Cycles illimités
- ✗ Taille très limitée (Ko)
- ✗ Effacée à l'extinction

### EEPROM (mémoire persistante)

L'**EEPROM** est une mémoire **non-volatile** pour sauvegarder des paramètres.

```pascal
// Sauvegarde configuration
procedure SauvegarderConfig;
begin
  EEPROM_Write(0, CalibrationOffset);
  EEPROM_Write(4, SeuilAlarme);
end;

procedure ChargerConfig;
begin
  CalibrationOffset := EEPROM_Read(0);
  SeuilAlarme := EEPROM_Read(4);
end;
```

**Caractéristiques** :
- ✓ Persistante
- ✓ Modifiable
- ✗ Très petite (512 octets à 4 Ko)
- ✗ Écritures lentes
- ✗ Cycles limités (100k-1M)

## Anatomie de la mémoire d'un programme

Voici comment la mémoire est organisée dans un microcontrôleur :

```
┌─────────────────────┐ Adresse haute
│   Stack (pile)      │ ← Grandit vers le bas
│         ↓           │
│                     │
│   Espace libre      │
│                     │
│         ↑           │
│   Heap (tas)        │ ← Grandit vers le haut
├─────────────────────┤
│   BSS               │ Variables non initialisées
│   (zéro au départ)  │
├─────────────────────┤
│   Data              │ Variables initialisées
│   (copiées depuis   │ (initialisées au démarrage)
│    Flash)           │
├─────────────────────┤ Adresse basse
│   Text (code)       │ Programme en Flash
└─────────────────────┘
```

### Sections mémoire

**Text (Code)** :
```pascal
// Stocké en Flash, ne consomme pas de RAM
procedure MaFonction;
begin
  // Instructions machine ici
end;
```

**Data (données initialisées)** :
```pascal
// Consomme RAM + Flash (valeur initiale en Flash)
var
  Compteur: Integer = 100;  // Valeur initiale stockée en Flash
                             // Copiée en RAM au démarrage
```

**BSS (données non initialisées)** :
```pascal
// Consomme uniquement RAM (mis à zéro au démarrage)
var
  Buffer: array[0..255] of Byte;  // Pas de valeur initiale
                                   // = pas de copie Flash nécessaire
```

**Stack (pile)** :
```pascal
// Variables locales et appels de fonction
procedure Fonction;
var
  Local: Integer;  // Sur la pile (stack)
  Tableau: array[0..99] of Byte;  // 100 octets sur la pile !
begin
  // ...
end;
```

**Heap (tas)** :
```pascal
// Allocation dynamique
var
  Pointeur: ^Integer;
begin
  New(Pointeur);  // Alloue sur le heap
  // ...
  Dispose(Pointeur);
end;
```

## Mesurer l'utilisation mémoire

### Avec le compilateur FreePascal

Après compilation, utilisez les outils pour voir la taille :

```bash
# Taille des sections
arm-none-eabi-size programme.elf

# Résultat typique :
#    text    data     bss     dec     hex filename
#   15240     256    2048   17544    4488 programme.elf
#
# text = code en Flash
# data = variables initialisées (RAM + copie Flash)
# bss  = variables non initialisées (RAM seulement)
```

### Dans le code source

```pascal
{$INFO 'Taille de TStructure: ' + IntToStr(SizeOf(TStructure))}

// Affiche pendant la compilation
type
  TMaStructure = record
    A: Byte;      // 1 octet
    B: Integer;   // 4 octets
    C: Real;      // 4 octets
  end;

begin
  WriteLn('Taille: ', SizeOf(TMaStructure));  // Affiche à l'exécution
end;
```

### Carte mémoire (memory map)

Générer un fichier map détaillé :

```bash
fpc -Xm programme.pas

# Crée programme.map avec détails de chaque symbole
```

Exemple de contenu :
```
Symbol                  Address    Size
main                    0x08000100  120
CalculerTemperature     0x08000178   84
Buffer_Global           0x20000000  256
```

## Optimisation des types de données

### Choisir le bon type

```pascal
// ✗ GASPILLAGE pour un compteur 0-100
var
  Compteur: Int64;  // 8 octets !

// ✓ OPTIMAL
var
  Compteur: Byte;   // 1 octet suffit pour 0-255
```

**Tableau des types et tailles** :

| Type | Taille | Plage | Usage |
|------|--------|-------|-------|
| Boolean | 1 octet | True/False | Drapeaux |
| Byte | 1 octet | 0..255 | Compteurs, buffers |
| ShortInt | 1 octet | -128..127 | Petites valeurs signées |
| Word | 2 octets | 0..65535 | Compteurs moyens |
| SmallInt | 2 octets | -32768..32767 | Valeurs signées |
| Integer | 4 octets* | Large | Calculs généraux |
| LongInt | 4 octets | -2G..2G | Grands compteurs |
| Single | 4 octets | ±3.4E38 | Flottants |
| Real | 8 octets | ±1.7E308 | Haute précision |

*Dépend de la plateforme

```pascal
// Exemple d'optimisation
type
  // ✗ AVANT : 16 octets
  TCapteur_Mauvais = record
    ID: Integer;           // 4 octets (0-10 suffirait)
    Temperature: Real;     // 8 octets (Single suffit)
    Actif: Integer;        // 4 octets (Boolean suffit)
  end;

  // ✓ APRÈS : 6 octets (62% d'économie !)
  TCapteur_Optimal = record
    ID: Byte;              // 1 octet (0-255)
    Temperature: Single;   // 4 octets
    Actif: Boolean;        // 1 octet
  end;
```

### Empaquetage de structures (packed)

Le compilateur aligne les structures en mémoire pour performance, mais cela gaspille de l'espace.

```pascal
type
  // Sans packed : alignement automatique
  TNonPacked = record
    A: Byte;     // 1 octet
    // 3 octets de padding !
    B: Integer;  // 4 octets
    C: Byte;     // 1 octet
    // 3 octets de padding !
  end;
  // Taille réelle : 12 octets

  // Avec packed : pas de padding
  TPacked = packed record
    A: Byte;     // 1 octet
    B: Integer;  // 4 octets
    C: Byte;     // 1 octet
  end;
  // Taille réelle : 6 octets (50% d'économie)

begin
  WriteLn('Sans packed: ', SizeOf(TNonPacked));  // 12
  WriteLn('Avec packed: ', SizeOf(TPacked));     // 6
end;
```

**Attention** : sur ARM, l'accès à des données non alignées peut être plus lent, voire causer une exception. Utilisez `packed` avec discernement.

### Champs de bits (bitpacking)

Économiser encore plus avec des champs de bits :

```pascal
type
  // 4 octets pour 4 booléens = 96% de gaspillage
  TEtat_Mauvais = record
    LED1: Boolean;
    LED2: Boolean;
    Alarme: Boolean;
    Moteur: Boolean;
  end;

  // Méthode 1 : Masques manuels (1 octet)
var
  Etat: Byte;  // 8 bits disponibles

const
  FLAG_LED1   = $01;  // Bit 0
  FLAG_LED2   = $02;  // Bit 1
  FLAG_ALARME = $04;  // Bit 2
  FLAG_MOTEUR = $08;  // Bit 3

procedure SetLED1(Active: Boolean);
begin
  if Active then
    Etat := Etat or FLAG_LED1
  else
    Etat := Etat and (not FLAG_LED1);
end;

function GetLED1: Boolean;
begin
  Result := (Etat and FLAG_LED1) <> 0;
end;

// Méthode 2 : Bitpacked record (non standard FPC)
type
  TEtat_Optimal = bitpacked record
    LED1: Boolean;     // 1 bit
    LED2: Boolean;     // 1 bit
    Alarme: Boolean;   // 1 bit
    Moteur: Boolean;   // 1 bit
    Reserved: 0..15;   // 4 bits inutilisés
  end;
  // Taille : 1 octet (au lieu de 4)
```

### Énumérations compactes

```pascal
type
  // Par défaut : 4 octets par valeur
  TEtat = (esIdle, esRunning, esPaused, esStopped);

  // Compact : 1 octet suffit
  TEtatCompact = (esIdle, esRunning, esPaused, esStopped);

var
  Etat: TEtatCompact;  // Forcer taille minimale

// Ou spécifier explicitement la taille
type
  TEtat8 = (
    esIdle = 0,
    esRunning = 1,
    esPaused = 2,
    esStopped = 3
  );

var
  Etat: Byte;  // Manipuler comme Byte
begin
  Etat := Ord(esRunning);
end;
```

## Optimisation des tableaux

### Tableaux statiques vs dynamiques

```pascal
// ✓ STATIQUE : taille connue à la compilation
var
  Buffer: array[0..99] of Byte;  // 100 octets en BSS

// ✗ DYNAMIQUE : allocation runtime (déconseillé en embarqué)
var
  Buffer: array of Byte;
begin
  SetLength(Buffer, 100);  // Alloue sur le heap
                            // Fragmentation possible !
end;
```

**Préférez toujours les tableaux statiques** en embarqué.

### Tableaux multidimensionnels

```pascal
// Ordre de stockage : row-major
var
  Matrice: array[0..9, 0..9] of Byte;  // 100 octets

// Accès séquentiel optimal (cache-friendly)
for i := 0 to 9 do
  for j := 0 to 9 do
    Matrice[i, j] := 0;  // ✓ Bon ordre

// Accès non optimal
for j := 0 to 9 do
  for i := 0 to 9 do
    Matrice[i, j] := 0;  // ✗ Saute dans la mémoire
```

### Réutilisation de buffers

```pascal
// ✗ MAUVAIS : 3 buffers différents
var
  BufferUART: array[0..255] of Byte;
  BufferSPI: array[0..255] of Byte;
  BufferI2C: array[0..255] of Byte;
  // Total : 768 octets

// ✓ BON : 1 buffer partagé
var
  BufferCommun: array[0..255] of Byte;

procedure CommuniquerUART;
begin
  // Utilise BufferCommun
end;

procedure CommuniquerSPI;
begin
  // Réutilise le même BufferCommun
  // (pas en même temps que UART)
end;
```

### Compression de données

```pascal
// Stocker 8 états booléens
// ✗ AVANT : 8 octets
var
  Etat1, Etat2, Etat3, Etat4: Boolean;
  Etat5, Etat6, Etat7, Etat8: Boolean;

// ✓ APRÈS : 1 octet
var
  Etats: Byte;  // 8 bits pour 8 états

function GetEtat(Numero: Byte): Boolean; inline;
begin
  Result := (Etats and (1 shl Numero)) <> 0;
end;

procedure SetEtat(Numero: Byte; Valeur: Boolean); inline;
begin
  if Valeur then
    Etats := Etats or (1 shl Numero)
  else
    Etats := Etats and (not (1 shl Numero));
end;

// Utilisation
begin
  SetEtat(0, True);   // Etat1 = True
  SetEtat(5, False);  // Etat6 = False

  if GetEtat(3) then  // Tester Etat4
    WriteLn('Etat4 actif');
end;
```

## Optimisation de la pile (stack)

### Dimensionner correctement la pile

```pascal
// Configuration initiale (fichier de démarrage)
{$STACKSIZE 2048}  // 2 Ko de pile

// Trop petit → débordement (stack overflow)
// Trop grand → gaspillage RAM
```

### Éviter les grosses variables locales

```pascal
// ✗ DANGEREUX : 1024 octets sur la pile !
procedure Traiter;
var
  GrosBuffer: array[0..1023] of Byte;  // LOCAL = pile
begin
  // Risque de débordement de pile
end;

// ✓ SOLUTION 1 : Variable globale
var
  BufferGlobal: array[0..1023] of Byte;  // BSS, pas la pile

procedure Traiter;
begin
  // Utilise BufferGlobal
end;

// ✓ SOLUTION 2 : Allocation statique explicite
var
  BufferStatique: array[0..1023] of Byte; static;

procedure Traiter;
begin
  // BufferStatique est en BSS, pas sur la pile
end;
```

### Limiter la profondeur de récursion

```pascal
// ✗ DANGEREUX : récursion profonde
function Fibonacci(n: Integer): Integer;
begin
  if n <= 1 then
    Result := n
  else
    Result := Fibonacci(n-1) + Fibonacci(n-2);
  // Fibonacci(20) → 21891 appels !
end;

// ✓ SOLUTION : version itérative
function FibonacciIteratif(n: Integer): Integer;
var
  a, b, i, temp: Integer;
begin
  a := 0;
  b := 1;
  for i := 2 to n do
  begin
    temp := a + b;
    a := b;
    b := temp;
  end;
  Result := b;
end;
```

### Surveiller l'usage de la pile

```pascal
// Pattern magique pour détecter débordement
const
  STACK_CANARY = $DEADBEEF;

var
  StackGuard: LongWord = STACK_CANARY;

procedure CheckStackOverflow;
begin
  if StackGuard <> STACK_CANARY then
  begin
    // Pile corrompue !
    UART_SendString('STACK OVERFLOW!'#13#10);
    while True do ;  // Arrêt
  end;
end;

// Appeler régulièrement
begin
  while True do
  begin
    CheckStackOverflow();
    TraiterTaches();
  end;
end;
```

## Optimisation du tas (heap)

### Éviter l'allocation dynamique

```pascal
// ✗ À ÉVITER en embarqué
var
  p: ^array of Byte;
begin
  New(p);            // Allocation
  SetLength(p^, 256);
  // Utilisation
  Dispose(p);        // Libération
end;

// ✓ PRÉFÉRER allocation statique
var
  Buffer: array[0..255] of Byte;
begin
  // Utilisation directe
end;
```

**Pourquoi éviter l'allocation dynamique ?**

1. **Fragmentation** : après plusieurs alloc/free, le heap devient fragmenté
2. **Imprévisibilité** : temps d'allocation variable
3. **Gaspillage** : overhead de gestion (pointeurs, taille, etc.)
4. **Debugging difficile** : fuites mémoire hard à tracer

### Pools de mémoire

Si vous devez absolument allouer dynamiquement :

```pascal
const
  POOL_SIZE = 10;
  BLOCK_SIZE = 64;

type
  TMemoryPool = record
    Blocs: array[0..POOL_SIZE-1, 0..BLOCK_SIZE-1] of Byte;
    Utilise: array[0..POOL_SIZE-1] of Boolean;
  end;

var
  Pool: TMemoryPool;

function AllouerBloc: Pointer;
var
  i: Integer;
begin
  for i := 0 to POOL_SIZE-1 do
  begin
    if not Pool.Utilise[i] then
    begin
      Pool.Utilise[i] := True;
      Exit(@Pool.Blocs[i]);
    end;
  end;
  Result := nil;  // Pool plein !
end;

procedure LibererBloc(p: Pointer);
var
  i: Integer;
  Offset: PtrUInt;
begin
  Offset := PtrUInt(p) - PtrUInt(@Pool.Blocs[0]);
  i := Offset div BLOCK_SIZE;

  if (i >= 0) and (i < POOL_SIZE) then
    Pool.Utilise[i] := False;
end;

// Utilisation
var
  Buffer: Pointer;
begin
  Buffer := AllouerBloc();
  if Buffer <> nil then
  begin
    // Utiliser Buffer
    LibererBloc(Buffer);
  end;
end;
```

### Désactiver complètement le heap

```pascal
{$IFDEF EMBEDDED}
  {$MEMORY 1024, 0}  // 1 Ko stack, 0 Ko heap
{$ENDIF}

// New/Dispose/GetMem/FreeMem ne fonctionneront plus
// → Force l'allocation statique uniquement
```

## Optimisation des chaînes de caractères

### ShortString vs String

```pascal
// String (AnsiString) : allocation dynamique
var
  s: String;
begin
  s := 'Bonjour';  // Alloue sur le heap
end;

// ShortString : taille fixe, pas d'allocation
var
  s: ShortString;  // Max 255 caractères
begin
  s := 'Bonjour';  // Stocké directement dans s
end;

// Taille fixe personnalisée
var
  s: String[20];   // Max 20 caractères
begin
  s := 'Bonjour';  // 1 octet longueur + 20 octets max = 21 octets
end;
```

**En embarqué, préférez toujours ShortString !**

### Chaînes constantes en Flash

```pascal
// ✗ CONSOMME RAM
var
  Message: String = 'Erreur critique détectée !';

// ✓ RESTE EN FLASH
const
  Message: String = 'Erreur critique détectée !';

// Ou mieux : tableau de caractères
const
  Message: array[0..26] of Char = 'Erreur critique détectée !';
```

### Formatage optimisé

```pascal
// ✗ Format() alloue une String (heap)
UART_Send(Format('Temp: %d', [Temperature]));

// ✓ Conversion manuelle (pas d'allocation)
procedure IntToStr(Value: Integer; var Buffer: ShortString);
var
  i, Digit: Integer;
  Negatif: Boolean;
begin
  Buffer := '';
  Negatif := Value < 0;
  if Negatif then Value := -Value;

  repeat
    Digit := Value mod 10;
    Buffer := Chr(Ord('0') + Digit) + Buffer;
    Value := Value div 10;
  until Value = 0;

  if Negatif then
    Buffer := '-' + Buffer;
end;

var
  TempStr: ShortString;
begin
  IntToStr(Temperature, TempStr);
  UART_Send('Temp: ' + TempStr);
end;
```

## Optimisation du code

### Fonctions inline

```pascal
// Fonction normale : overhead d'appel
function Carre(x: Integer): Integer;
begin
  Result := x * x;
end;

// Fonction inline : code inséré directement
function Carre(x: Integer): Integer; inline;
begin
  Result := x * x;
end;

// Le compilateur remplace Carre(5) par (5 * 5)
// Économise : pile, saut, retour
```

### Directives d'optimisation

```pascal
{$OPTIMIZATION LEVEL3}      // Optimisation maximale
{$SMARTLINK ON}             // Éliminer code mort
{$INLINE ON}                // Activer inlining

// Par fonction
procedure Critique; inline;
begin
  // Code critique
end;

procedure PasOptimisee; {$OPTIMIZATION OFF}
begin
  // Debug plus facile
end;
```

### Éviter les conversions de types

```pascal
// ✗ Conversions coûteuses
var
  a: Byte;
  b: Integer;
  c: Real;
begin
  c := a + b;  // Byte → Integer → Real (2 conversions)
end;

// ✓ Utiliser le même type
var
  a, b: Integer;
  c: Integer;
begin
  c := a + b;  // Pas de conversion
end;
```

## Stockage en Flash (PROGMEM)

### Constantes en mémoire programme

Sur AVR, le mot-clé `progmem` place les données en Flash :

```pascal
{$IFDEF CPUAVR}
const
  GrandeTable: array[0..999] of Word = (...) progmem;
  // Reste en Flash, ne consomme pas de RAM

function LireTable(Index: Word): Word;
begin
  Result := pgm_read_word(@GrandeTable[Index]);
end;
{$ENDIF}
```

Sur ARM (STM32), utilisez `const` et le linker s'en charge :

```pascal
const
  // Automatiquement en Flash sur ARM
  TableSinus: array[0..359] of Integer = (
    0, 17, 34, 52, 69, 87, 104, 121, ...
  );
```

### Chaînes en Flash

```pascal
// Macro helper pour chaînes Flash (AVR)
{$IFDEF CPUAVR}
type
  PFlashString = ^String;

function FlashStr(const s: String): PFlashString; inline;
begin
  Result := @s;
end;

const
  MSG1 = 'Message long en Flash' progmem;
  MSG2 = 'Autre message' progmem;

procedure AfficherMessage(fs: PFlashString);
var
  c: Char;
  i: Integer;
begin
  i := 0;
  repeat
    c := pgm_read_byte(@fs^[i]);
    if c = #0 then Break;
    UART_SendChar(c);
    Inc(i);
  until False;
end;

begin
  AfficherMessage(FlashStr(MSG1));
end;
{$ENDIF}
```

## Techniques avancées

### Overlay (recouvrement mémoire)

Réutiliser la même zone mémoire pour différentes structures :

```pascal
type
  TBuffer = record
    case Integer of
      0: (Bytes: array[0..255] of Byte);
      1: (Words: array[0..127] of Word);
      2: (Longs: array[0..63] of LongWord);
  end;

var
  Buffer: TBuffer;

begin
  // Utiliser comme tableau de bytes
  Buffer.Bytes[0] := 42;

  // Ou comme tableau de words (même mémoire !)
  Buffer.Words[0] := 1000;
end;
```

### Union/Variant records

```pascal
type
  TData = record
    case DataType: Byte of
      0: (ByteValue: Byte);
      1: (IntValue: Integer);
      2: (FloatValue: Single);
  end;

var
  Data: TData;

begin
  Data.DataType := 2;
  Data.FloatValue := 3.14;
  // Consomme : 1 octet (type) + 4 octets (max des variantes)
  // = 5 octets au lieu de 1+1+4+4 = 10 octets
end;
```

### Compression run-length

```pascal
// Compresser données répétitives
type
  TRLEData = record
    Count: Byte;
    Value: Byte;
  end;

// Données non compressées : 100 octets
const
  DataRaw: array[0..99] of Byte = (
    0, 0, 0, 0, 0, 0, 0, 0,  // 8x 0
    255, 255, 255, 255, ...
  );

// Données compressées : 6 octets (94% d'économie !)
const
  DataRLE: array[0..2] of TRLEData = (
    (Count: 8; Value: 0),
    (Count: 50; Value: 255),
    (Count: 42; Value: 128)
  );

procedure DecompresserRLE(const Src: array of TRLEData;
                          var Dest: array of Byte);
var
  i, j, DestIndex: Integer;
begin
  DestIndex := 0;
  for i := Low(Src) to High(Src) do
    for j := 1 to Src[i].Count do
    begin
      Dest[DestIndex] := Src[i].Value;
      Inc(DestIndex);
    end;
end;
```

### Lookup tables

Précalculer au lieu de calculer :

```pascal
// ✗ Calcul runtime (lent, consomme cycles CPU)
function Sinus(Angle: Integer): Integer;
begin
  Result := Round(Sin(Angle * Pi / 180) * 1000);
end;

// ✓ Table précalculée (rapide, consomme Flash)
const
  TableSinus: array[0..359] of Integer = (
    0, 17, 34, 52, 69, 87, 104, 121, 139, 156, ...
  );

function Sinus(Angle: Integer): Integer;
begin
  Result := TableSinus[Angle mod 360];
end;
```

**Trade-off** : Flash contre cycles CPU. Sur microcontrôleur, la Flash est souvent plus abondante que les cycles.

## Stratégies globales

### Analyse statique

Avant d'optimiser, **mesurez** :

```pascal
// Ajouter des compteurs
var
  AllocCount: LongWord = 0;
  MaxStackUsage: Word = 0;
  CurrentRAMUsage: Word = 0;

{$IFDEF DEBUG}
procedure TrackAllocation(Size: Word);
begin
  Inc(AllocCount);
  Inc(CurrentRAMUsage, Size);

  if CurrentRAMUsage > MaxStackUsage then
    MaxStackUsage := CurrentRAMUsage;

  UART_Printf('Alloc: %u bytes, Total: %u, Max: %u'#13#10,
              Size, CurrentRAMUsage, MaxStackUsage);
end;

procedure TrackDeallocation(Size: Word);
begin
  Dec(CurrentRAMUsage, Size);
end;
{$ENDIF}

// Wrapper pour New/Dispose
function MyNew(Size: Word): Pointer;
begin
  Result := GetMem(Size);
  {$IFDEF DEBUG}
  TrackAllocation(Size);
  {$ENDIF}
end;

procedure MyDispose(p: Pointer; Size: Word);
begin
  {$IFDEF DEBUG}
  TrackDeallocation(Size);
  {$ENDIF}
  FreeMem(p, Size);
end;
```

### Profiling mémoire

```pascal
// Mesurer l'utilisation de la pile
var
  StackStart: ^Byte;
  StackEnd: ^Byte;

procedure InitStackMonitoring;
var
  Dummy: Byte;
begin
  StackStart := @Dummy;

  // Remplir la pile avec un pattern
  FillChar(Dummy, 1024, $AA);
end;

function GetStackUsage: Word;
var
  Current: ^Byte;
  Dummy: Byte;
  Usage: PtrUInt;
begin
  Current := @Dummy;
  Usage := PtrUInt(StackStart) - PtrUInt(Current);
  Result := Usage;
end;

function GetMaxStackUsage: Word;
var
  p: ^Byte;
  Count: Word;
begin
  // Chercher jusqu'où le pattern $AA a été écrasé
  p := StackEnd;
  Count := 0;

  while (p < StackStart) and (p^ <> $AA) do
  begin
    Inc(Count);
    Inc(p);
  end;

  Result := Count;
end;

// Utilisation
begin
  InitStackMonitoring();

  // Exécuter le programme...

  WriteLn('Stack utilisée: ', GetStackUsage(), ' octets');
  WriteLn('Stack max: ', GetMaxStackUsage(), ' octets');
end;
```

### Cartographie mémoire

Créer un rapport détaillé :

```pascal
procedure AfficherCartographieMemoire;
begin
  WriteLn('=== CARTOGRAPHIE MÉMOIRE ===');
  WriteLn;

  WriteLn('Flash (code):');
  WriteLn('  Utilisée: ', CodeSize, ' octets');
  WriteLn('  Total: ', FLASH_SIZE, ' octets');
  WriteLn('  Libre: ', FLASH_SIZE - CodeSize, ' octets');
  WriteLn('  Utilisation: ', (CodeSize * 100) div FLASH_SIZE, '%');
  WriteLn;

  WriteLn('RAM:');
  WriteLn('  Variables globales: ', DataSize + BSSSize, ' octets');
  WriteLn('  Pile (stack): ', StackSize, ' octets');
  WriteLn('  Tas (heap): ', HeapSize, ' octets');
  WriteLn('  Total utilisé: ', DataSize + BSSSize + StackSize + HeapSize);
  WriteLn('  Total disponible: ', RAM_SIZE, ' octets');
  WriteLn('  Libre: ', RAM_SIZE - (DataSize + BSSSize + StackSize + HeapSize));
  WriteLn('  Utilisation: ', ((DataSize + BSSSize + StackSize + HeapSize) * 100) div RAM_SIZE, '%');
  WriteLn;

  WriteLn('Détails variables globales:');
  WriteLn('  Buffer UART: ', SizeOf(BufferUART), ' octets');
  WriteLn('  Buffer SPI: ', SizeOf(BufferSPI), ' octets');
  WriteLn('  Table sinus: ', SizeOf(TableSinus), ' octets');
  WriteLn('  Total: ', SizeOf(BufferUART) + SizeOf(BufferSPI) + SizeOf(TableSinus));
end;
```

## Optimisation par composant

### Protocoles de communication

#### UART/Serial - Buffers circulaires

```pascal
const
  UART_BUFFER_SIZE = 64;  // Ajuster selon besoin

type
  TCircularBuffer = record
    Data: array[0..UART_BUFFER_SIZE-1] of Byte;
    Head: Byte;  // Position écriture
    Tail: Byte;  // Position lecture
    Count: Byte; // Nombre d'éléments
  end;

var
  RxBuffer: TCircularBuffer;

procedure BufferPush(var Buf: TCircularBuffer; Value: Byte);
begin
  if Buf.Count < UART_BUFFER_SIZE then
  begin
    Buf.Data[Buf.Head] := Value;
    Buf.Head := (Buf.Head + 1) mod UART_BUFFER_SIZE;
    Inc(Buf.Count);
  end;
  // Sinon : buffer plein, données perdues
end;

function BufferPop(var Buf: TCircularBuffer): Byte;
begin
  Result := 0;
  if Buf.Count > 0 then
  begin
    Result := Buf.Data[Buf.Tail];
    Buf.Tail := (Buf.Tail + 1) mod UART_BUFFER_SIZE;
    Dec(Buf.Count);
  end;
end;

// Interruption UART RX
procedure USART1_IRQHandler; interrupt;
begin
  if (USART1^.SR and USART_SR_RXNE) <> 0 then
  begin
    BufferPush(RxBuffer, USART1^.DR and $FF);
  end;
end;

// Dans le programme principal
var
  Data: Byte;
begin
  if RxBuffer.Count > 0 then
  begin
    Data := BufferPop(RxBuffer);
    TraiterDonnee(Data);
  end;
end;
```

#### I2C/SPI - Buffers optimisés

```pascal
// Au lieu de buffers séparés pour chaque périphérique
var
  BufferI2C_BME280: array[0..31] of Byte;
  BufferI2C_MPU6050: array[0..31] of Byte;
  BufferSPI_SDCard: array[0..511] of Byte;
  // Total : 575 octets

// Utiliser un buffer partagé avec union
type
  TCommunicationBuffer = record
    case Byte of
      0: (I2C: array[0..31] of Byte);
      1: (SPI: array[0..511] of Byte);
  end;

var
  CommBuffer: TCommunicationBuffer;
  // Total : 512 octets (économie de 63 octets)

// Règle : ne jamais utiliser I2C et SPI simultanément avec ce buffer
procedure LireBME280;
begin
  I2C_Read(BME280_ADDR, @CommBuffer.I2C, 8);
  // Traiter immédiatement
  Temperature := ConvertirTemp(CommBuffer.I2C);
end;

procedure LireSDCard;
begin
  SPI_ReadBlock(@CommBuffer.SPI, 512);
  // Traiter immédiatement
  TraiterBlock(CommBuffer.SPI);
end;
```

### Affichage LCD/OLED

#### Buffer d'écran optimisé

```pascal
// Écran OLED 128x64 monochrome
const
  SCREEN_WIDTH = 128;
  SCREEN_HEIGHT = 64;

// ✗ GASPILLAGE : 1 octet par pixel
var
  FrameBuffer_Mauvais: array[0..SCREEN_WIDTH-1, 0..SCREEN_HEIGHT-1] of Byte;
  // 8192 octets !

// ✓ OPTIMAL : 1 bit par pixel
var
  FrameBuffer: array[0..(SCREEN_WIDTH * SCREEN_HEIGHT) div 8 - 1] of Byte;
  // 1024 octets (8 fois moins !)

procedure SetPixel(x, y: Byte; Couleur: Boolean);
var
  ByteIndex, BitIndex: Word;
begin
  ByteIndex := (y div 8) * SCREEN_WIDTH + x;
  BitIndex := y mod 8;

  if Couleur then
    FrameBuffer[ByteIndex] := FrameBuffer[ByteIndex] or (1 shl BitIndex)
  else
    FrameBuffer[ByteIndex] := FrameBuffer[ByteIndex] and (not (1 shl BitIndex));
end;

function GetPixel(x, y: Byte): Boolean;
var
  ByteIndex, BitIndex: Word;
begin
  ByteIndex := (y div 8) * SCREEN_WIDTH + x;
  BitIndex := y mod 8;
  Result := (FrameBuffer[ByteIndex] and (1 shl BitIndex)) <> 0;
end;
```

#### Affichage partiel

```pascal
// Au lieu de rafraîchir tout l'écran
procedure RefreshFullScreen;
begin
  SPI_WriteBuffer(@FrameBuffer, 1024);  // 1024 octets à envoyer
end;

// Rafraîchir uniquement la zone modifiée
type
  TDirtyRect = record
    x1, y1, x2, y2: Byte;
    Dirty: Boolean;
  end;

var
  DirtyRegion: TDirtyRect;

procedure MarkDirty(x, y, w, h: Byte);
begin
  if not DirtyRegion.Dirty then
  begin
    DirtyRegion.x1 := x;
    DirtyRegion.y1 := y;
    DirtyRegion.x2 := x + w;
    DirtyRegion.y2 := y + h;
    DirtyRegion.Dirty := True;
  end
  else
  begin
    // Étendre la région sale
    if x < DirtyRegion.x1 then DirtyRegion.x1 := x;
    if y < DirtyRegion.y1 then DirtyRegion.y1 := y;
    if (x + w) > DirtyRegion.x2 then DirtyRegion.x2 := x + w;
    if (y + h) > DirtyRegion.y2 then DirtyRegion.y2 := y + h;
  end;
end;

procedure RefreshDirtyRegion;
var
  x, y, ByteIndex: Word;
begin
  if not DirtyRegion.Dirty then Exit;

  // N'envoyer que les octets modifiés
  for y := DirtyRegion.y1 to DirtyRegion.y2 do
    for x := DirtyRegion.x1 to DirtyRegion.x2 do
    begin
      ByteIndex := (y div 8) * SCREEN_WIDTH + x;
      OLED_WritePixelByte(x, y, FrameBuffer[ByteIndex]);
    end;

  DirtyRegion.Dirty := False;
end;
```

### Capteurs et échantillonnage

#### Moyennage glissant optimisé

```pascal
// ✗ Version avec historique complet
const
  SAMPLES = 16;

var
  History: array[0..SAMPLES-1] of Word;  // 32 octets
  Index: Byte;

function Average_Lourd: Word;
var
  i: Byte;
  Sum: LongWord;
begin
  Sum := 0;
  for i := 0 to SAMPLES-1 do
    Sum := Sum + History[i];
  Result := Sum div SAMPLES;
end;

// ✓ Version optimisée (moyenne cumulative)
var
  RunningSum: LongWord;   // 4 octets
  SampleCount: Byte;

function Average_Leger(NewValue: Word): Word;
begin
  if SampleCount < SAMPLES then
  begin
    Inc(RunningSum, NewValue);
    Inc(SampleCount);
  end
  else
  begin
    // Soustraire ancienne valeur, ajouter nouvelle
    // (approximation sans historique)
    RunningSum := RunningSum - (RunningSum div SAMPLES) + NewValue;
  end;

  Result := RunningSum div SAMPLES;
end;
// Économie : 32 - 4 = 28 octets
```

#### Filtre IIR au lieu de FIR

```pascal
// Filtre FIR (nécessite historique)
const
  FIR_TAPS = 8;

var
  FIR_History: array[0..FIR_TAPS-1] of Integer;  // 16-32 octets
  FIR_Coeffs: array[0..FIR_TAPS-1] of Integer;

function FIR_Filter(Input: Integer): Integer;
var
  i: Integer;
  Sum: LongInt;
begin
  // Shift historique
  for i := FIR_TAPS-1 downto 1 do
    FIR_History[i] := FIR_History[i-1];
  FIR_History[0] := Input;

  // Convolution
  Sum := 0;
  for i := 0 to FIR_TAPS-1 do
    Sum := Sum + FIR_History[i] * FIR_Coeffs[i];

  Result := Sum div 256;
end;

// Filtre IIR (pas d'historique nécessaire)
var
  IIR_State: Integer;  // 2-4 octets seulement !

function IIR_Filter(Input: Integer): Integer;
const
  ALPHA = 64;  // Coefficient (0-255), 64 = ~25%
begin
  // Filtre passe-bas simple : y[n] = α*x[n] + (1-α)*y[n-1]
  IIR_State := ((Input * ALPHA) + (IIR_State * (256 - ALPHA))) div 256;
  Result := IIR_State;
end;
// Économie : 16-32 octets → 2-4 octets
```

## Gestion de configuration

### EEPROM pour paramètres persistants

```pascal
type
  TConfiguration = packed record
    MagicNumber: Word;        // Vérification validité
    Version: Byte;            // Version config
    CalibrationTemp: Single;  // 4 octets
    SeuilAlarme: Word;        // 2 octets
    EnableLogging: Boolean;   // 1 octet
    Checksum: Byte;           // CRC simple
  end;

const
  CONFIG_MAGIC = $C0DE;
  CONFIG_VERSION = 1;
  CONFIG_ADDR = 0;

var
  Config: TConfiguration;

function CalculerChecksum(const Data: TConfiguration): Byte;
var
  i: Integer;
  p: ^Byte;
  Sum: Byte;
begin
  Sum := 0;
  p := @Data;
  for i := 0 to SizeOf(TConfiguration) - 2 do  // -2 pour exclure checksum
  begin
    Sum := Sum xor p^;
    Inc(p);
  end;
  Result := Sum;
end;

procedure SauvegarderConfig;
var
  p: ^Byte;
  i: Integer;
begin
  Config.MagicNumber := CONFIG_MAGIC;
  Config.Version := CONFIG_VERSION;
  Config.Checksum := CalculerChecksum(Config);

  p := @Config;
  for i := 0 to SizeOf(TConfiguration) - 1 do
  begin
    EEPROM_WriteByte(CONFIG_ADDR + i, p^);
    Inc(p);
  end;
end;

function ChargerConfig: Boolean;
var
  p: ^Byte;
  i: Integer;
begin
  p := @Config;
  for i := 0 to SizeOf(TConfiguration) - 1 do
  begin
    p^ := EEPROM_ReadByte(CONFIG_ADDR + i);
    Inc(p);
  end;

  // Vérifier validité
  if (Config.MagicNumber <> CONFIG_MAGIC) or
     (Config.Version <> CONFIG_VERSION) or
     (Config.Checksum <> CalculerChecksum(Config)) then
  begin
    // Configuration invalide, charger défauts
    ChargerConfigDefaut();
    Exit(False);
  end;

  Result := True;
end;

procedure ChargerConfigDefaut;
begin
  Config.CalibrationTemp := 0.0;
  Config.SeuilAlarme := 100;
  Config.EnableLogging := True;
end;
```

### Compression de configuration

```pascal
// Au lieu de stocker des booléens séparés
type
  TConfig_NonOptimal = record
    EnableUART: Boolean;      // 1 octet
    EnableSPI: Boolean;       // 1 octet
    EnableI2C: Boolean;       // 1 octet
    EnableADC: Boolean;       // 1 octet
    EnablePWM: Boolean;       // 1 octet
    EnableTimer: Boolean;     // 1 octet
    EnableWatchdog: Boolean;  // 1 octet
    DebugMode: Boolean;       // 1 octet
  end;  // Total : 8 octets

// Compacter dans un seul octet
type
  TConfigFlags = Byte;

const
  FLAG_UART     = $01;
  FLAG_SPI      = $02;
  FLAG_I2C      = $04;
  FLAG_ADC      = $08;
  FLAG_PWM      = $10;
  FLAG_TIMER    = $20;
  FLAG_WATCHDOG = $40;
  FLAG_DEBUG    = $80;

var
  ConfigFlags: TConfigFlags;  // 1 octet seulement !

procedure SetFlag(Flag: Byte; Value: Boolean);
begin
  if Value then
    ConfigFlags := ConfigFlags or Flag
  else
    ConfigFlags := ConfigFlags and (not Flag);
end;

function GetFlag(Flag: Byte): Boolean;
begin
  Result := (ConfigFlags and Flag) <> 0;
end;

// Utilisation
begin
  SetFlag(FLAG_UART, True);
  SetFlag(FLAG_DEBUG, False);

  if GetFlag(FLAG_UART) then
    InitUART();
end;
```

## Techniques de partage mémoire

### Variables de registres

```pascal
// Forcer le compilateur à garder en registre (pas en RAM)
function CalculRapide(a, b: Integer): Integer;
var
  temp: Integer register;  // Hint : utiliser registre CPU
begin
  temp := a * b;
  Result := temp + 42;
end;
```

### Variables volatiles

```pascal
// Variable modifiée par interruption
var
  TickCount: LongWord volatile;

procedure SysTick_Handler; interrupt;
begin
  Inc(TickCount);  // Modifié par interruption
end;

// Le compilateur ne mettra pas en cache
function GetTicks: LongWord;
begin
  Result := TickCount;  // Toujours relu depuis RAM
end;
```

### Sections de mémoire personnalisées

```pascal
// Placer variables dans des sections spécifiques
var
  FastData: Integer; section '.fastram';  // RAM rapide (CCM sur STM32)
  SlowData: array[0..1023] of Byte; section '.slowram';  // RAM externe

// Configuration linker script (.ld)
// SECTIONS {
//   .fastram : { *(.fastram) } > CCMRAM
//   .slowram : { *(.slowram) } > EXTRAM
// }
```

## Exemples complets optimisés

### Station météo ultra-compacte (< 512 octets RAM)

```pascal
program StationMeteoCompact;

{$MODE OBJFPC}
{$MEMORY 512, 0}  // 512 octets stack, 0 heap

const
  // Tout en Flash
  MSG_TEMP: array[0..5] of Char = 'Temp: ';
  MSG_HUM: array[0..5] of Char = 'Hum: ';

type
  // Structure compacte
  TReading = packed record
    Temp: ShortInt;    // -128..127 (1 octet)
    Humidity: Byte;    // 0..100 (1 octet)
  end;

var
  // Variables globales minimales
  CurrentReading: TReading;  // 2 octets
  LastReading: TReading;     // 2 octets
  TickCount: Word;           // 2 octets

  // Buffer partagé pour toutes communications
  CommBuffer: array[0..31] of Byte;  // 32 octets

  // Total variables : ~40 octets seulement

procedure LireCapteur;
var
  RawTemp: Word;
begin
  // Lire I2C dans CommBuffer
  I2C_Read(BME280_ADDR, $FA, @CommBuffer, 3);

  // Convertir
  RawTemp := (CommBuffer[0] shl 4) or (CommBuffer[1] shr 4);
  CurrentReading.Temp := (RawTemp - 12800) div 100;  // Simplification

  // Humidity
  I2C_Read(BME280_ADDR, $FD, @CommBuffer, 2);
  CurrentReading.Humidity := CommBuffer[0] div 2;  // 0-200 → 0-100
end;

procedure AfficherLCD;
var
  s: String[10];  // 11 octets sur pile
begin
  // Afficher température
  LCD_SetCursor(0, 0);
  LCD_WriteString(MSG_TEMP);
  Str(CurrentReading.Temp, s);
  LCD_WriteString(s);
  LCD_WriteChar('C');

  // Afficher humidité
  LCD_SetCursor(0, 1);
  LCD_WriteString(MSG_HUM);
  Str(CurrentReading.Humidity, s);
  LCD_WriteString(s);
  LCD_WriteChar('%');
end;

procedure LoggerSDCard;
var
  i: Byte;
begin
  // Format CSV compact dans CommBuffer
  CommBuffer[0] := Byte('0') + (TickCount div 1000) mod 10;
  CommBuffer[1] := Byte(',');
  CommBuffer[2] := Byte('0') + CurrentReading.Temp div 10;
  CommBuffer[3] := Byte('0') + CurrentReading.Temp mod 10;
  CommBuffer[4] := Byte(',');
  CommBuffer[5] := Byte('0') + CurrentReading.Humidity div 10;
  CommBuffer[6] := Byte('0') + CurrentReading.Humidity mod 10;
  CommBuffer[7] := 13;  // CR
  CommBuffer[8] := 10;  // LF

  // Écrire sur SD
  SDCard_Write(@CommBuffer, 9);
end;

// Interruption timer (1 Hz)
procedure Timer_IRQHandler; interrupt;
begin
  Inc(TickCount);
  TIM2^.SR := 0;  // Clear flag
end;

begin
  // Initialisation
  InitPeripherals();

  // Boucle principale
  while True do
  begin
    if TickCount mod 10 = 0 then  // Toutes les 10 secondes
    begin
      LireCapteur();
      AfficherLCD();

      if TickCount mod 60 = 0 then  // Toutes les minutes
        LoggerSDCard();
    end;

    // Sleep
    WaitForInterrupt();
  end;
end.

// Résultat : programme fonctionnel en < 512 octets RAM !
```

### Contrôleur moteur optimisé

```pascal
program MotorController;

{$MODE OBJFPC}
{$OPTIMIZATION LEVEL3}
{$SMARTLINK ON}

type
  // État moteur compact (1 octet)
  TMotorState = (msIdle, msAccel, msRunning, msDecel, msStopped);

  // Commande moteur (5 octets)
  TMotorCommand = packed record
    State: TMotorState;     // 1 octet
    Speed: Byte;            // 0-100% (1 octet)
    Direction: Boolean;     // 1 octet (packed pourrait réduire)
    Duration: Word;         // millisecondes (2 octets)
  end;

var
  // Variables moteur
  Motor1, Motor2: TMotorCommand;  // 10 octets

  // Buffer commandes série (circular)
  CmdBuffer: array[0..7] of Byte;  // 8 octets
  CmdHead, CmdTail: Byte;          // 2 octets

  // Total : ~20 octets

procedure SetMotorPWM(Speed: Byte; Direction: Boolean); inline;
begin
  if Direction then
    GPIO_Set(DIR_PIN)
  else
    GPIO_Clear(DIR_PIN);

  TIM3^.CCR1 := (Speed * 255) div 100;  // PWM 0-255
end;

procedure UpdateMotor(var Cmd: TMotorCommand); inline;
begin
  case Cmd.State of
    msAccel:
    begin
      if Cmd.Speed < 100 then
        Inc(Cmd.Speed, 2)  // Rampe accélération
      else
        Cmd.State := msRunning;
    end;

    msDecel:
    begin
      if Cmd.Speed > 0 then
        Dec(Cmd.Speed, 2)  // Rampe décélération
      else
        Cmd.State := msStopped;
    end;
  end;

  SetMotorPWM(Cmd.Speed, Cmd.Direction);

  // Gestion durée
  if Cmd.Duration > 0 then
  begin
    Dec(Cmd.Duration);
    if Cmd.Duration = 0 then
      Cmd.State := msDecel;
  end;
end;

// Interruption 100 Hz (10ms)
procedure Timer_IRQHandler; interrupt;
begin
  UpdateMotor(Motor1);
  UpdateMotor(Motor2);
  TIM2^.SR := 0;
end;

// Commande depuis UART
procedure ProcessCommand(Cmd: Byte);
begin
  case Cmd and $F0 of
    $10:  // Motor 1 Forward
    begin
      Motor1.State := msAccel;
      Motor1.Direction := True;
      Motor1.Duration := 0;  // Infini
    end;

    $20:  // Motor 1 Reverse
    begin
      Motor1.State := msAccel;
      Motor1.Direction := False;
      Motor1.Duration := 0;
    end;

    $00:  // Motor 1 Stop
      Motor1.State := msDecel;

    // Etc pour Motor 2...
  end;
end;

begin
  InitPeripherals();

  Motor1.State := msIdle;
  Motor2.State := msIdle;

  while True do
  begin
    if UART_Available() then
    begin
      ProcessCommand(UART_Read());
    end;

    WaitForInterrupt();
  end;
end.
```

## Détection de fuites mémoire

### Wrapper avec compteurs

```pascal
{$IFDEF DEBUG}
var
  TotalAllocated: LongWord = 0;
  TotalFreed: LongWord = 0;
  CurrentUsage: LongWord = 0;
  PeakUsage: LongWord = 0;
  AllocationCount: LongWord = 0;
  FreeCount: LongWord = 0;

function TrackedGetMem(Size: PtrUInt): Pointer;
begin
  Result := GetMem(Size);
  if Result <> nil then
  begin
    Inc(TotalAllocated, Size);
    Inc(CurrentUsage, Size);
    Inc(AllocationCount);

    if CurrentUsage > PeakUsage then
      PeakUsage := CurrentUsage;

    UART_Printf('[ALLOC] %u bytes at %p, total: %u'#13#10,
                Size, Result, CurrentUsage);
  end;
end;

procedure TrackedFreeMem(p: Pointer; Size: PtrUInt);
begin
  if p <> nil then
  begin
    FreeMem(p, Size);
    Inc(TotalFreed, Size);
    Dec(CurrentUsage, Size);
    Inc(FreeCount);

    UART_Printf('[FREE] %u bytes at %p, total: %u'#13#10,
                Size, p, CurrentUsage);
  end;
end;

procedure PrintMemoryStats;
begin
  WriteLn('=== MEMORY STATISTICS ===');
  WriteLn('Total allocated: ', TotalAllocated, ' bytes');
  WriteLn('Total freed: ', TotalFreed, ' bytes');
  WriteLn('Current usage: ', CurrentUsage, ' bytes');
  WriteLn('Peak usage: ', PeakUsage, ' bytes');
  WriteLn('Allocations: ', AllocationCount);
  WriteLn('Frees: ', FreeCount);
  WriteLn('Potential leaks: ', TotalAllocated - TotalFreed, ' bytes');
end;
{$ENDIF}
```

## Optimisation finale : checklist

### ✓ Avant de compiler

- [ ] Toutes les variables sont du plus petit type possible
- [ ] Structures utilisent `packed` quand approprié
- [ ] Pas d'allocation dynamique (ou pools uniquement)
- [ ] Chaînes sont ShortString ou String[n]
- [ ] Constantes en Flash (const)
- [ ] Tableaux de taille minimale nécessaire
- [ ] Buffers partagés plutôt que multiples
- [ ] Variables locales ne dépassent pas 100 octets
- [ ] Pas de récursion profonde
- [ ] Fonctions critiques sont inline

### ✓ Options de compilation

```bash
# Optimisation maximale
fpc -O3 -CX -XX -Xs programme.pas

# Explications :
# -O3 : Optimisation niveau 3
# -CX : Création exécutable minimal (SmartLink)
# -XX : Exécutable sans informations de debug
# -Xs : Strip symbols (enlever symboles debug)

# Pour voir la taille
fpc -al programme.pas  # Génère programme.s (assembleur)

# Désactiver runtime checks (gain de taille)
fpc -Ct- -Cr- -Co- -Ci- programme.pas
# -Ct- : Pas de test débordement pile
# -Cr- : Pas de test range check
# -Co- : Pas de test overflow
# -Ci- : Pas de test I/O
```

### ✓ Après compilation

```bash
# Vérifier les tailles
arm-none-eabi-size -A programme.elf

# Analyser les sections
arm-none-eabi-nm --size-sort programme.elf | tail -20

# Générer memory map détaillé
arm-none-eabi-objdump -h programme.elf
```

### ✓ À l'exécution

- [ ] Mesurer utilisation pile maximale
- [ ] Vérifier qu'il reste au moins 20% RAM libre
- [ ] Tester sous charge maximale
- [ ] Pas de débordement de buffers
- [ ] Pas de fuites mémoire détectées

## Cas d'étude : Optimiser un projet existant

### Projet initial (trop gourmand)

```pascal
program ProjetGourmand;

type
  // Structure non optimisée
  TSensorData = record
    SensorID: Integer;           // 4 octets
    Temperature: Real;           // 8 octets
    Humidity: Real;              // 8 octets
    Pressure: Real;              // 8 octets
    Timestamp: String;           // Allocation dynamique !
    IsValid: Integer;            // 4 octets pour un booléen !
  end;

var
  // Tableaux surdimensionnés
  SensorHistory: array[0..999] of TSensorData;  // ~32 Ko !

  // Buffers multiples
  UARTBuffer: array[0..511] of Byte;   // 512 octets
  I2CBuffer: array[0..511] of Byte;    // 512 octets
  SPIBuffer: array[0..1023] of Byte;   // 1024 octets
  TempBuffer: array[0..255] of Byte;   // 256 octets

  // Chaînes non optimisées
  StatusMessage: String;               // Dynamique
  ErrorMessage: String;                // Dynamique
  LogMessage: String;                  // Dynamique

// Utilisation RAM : > 36 Ko
// Pour un MCU avec 20 Ko RAM → Ne compile pas !
```

### Après optimisation

```pascal
program ProjetOptimise;

type
  // Structure compacte et packed
  TSensorData = packed record
    SensorID: Byte;              // 1 octet (0-255 suffit)
    Temperature: SmallInt;       // 2 octets (1/10 degré : -327..327)
    Humidity: Byte;              // 1 octet (0-100%)
    Pressure: Word;              // 2 octets (hPa : 0-65535)
    Timestamp: LongWord;         // 4 octets (secondes depuis démarrage)
    Flags: Byte;                 // 1 octet (IsValid + 7 autres flags)
  end;  // Total : 11 octets au lieu de 32+

const
  HISTORY_SIZE = 64;  // Réduit de 1000 à 64

var
  // Historique réduit et optimisé
  SensorHistory: array[0..HISTORY_SIZE-1] of TSensorData;  // 704 octets
  HistoryIndex: Byte;

  // Buffer unique partagé
  CommBuffer: array[0..255] of Byte;  // 256 octets seulement

  // Chaînes fixes
  StatusMessage: String[32];   // 33 octets
  ErrorCode: Byte;             // Code erreur au lieu de message

// Utilisation RAM : ~1 Ko (97% d'économie !)
// Fonctionne parfaitement sur 20 Ko RAM

procedure StockerLecture(ID: Byte; Temp: Real; Hum, Press: Word);
var
  Reading: TSensorData;
begin
  Reading.SensorID := ID;
  Reading.Temperature := Round(Temp * 10);  // Conversion: 25.7°C → 257
  Reading.Humidity := Hum;
  Reading.Pressure := Press;
  Reading.Timestamp := GetTickCount() div 1000;
  Reading.Flags := $01;  // Bit 0 = IsValid

  // Historique circulaire
  SensorHistory[HistoryIndex] := Reading;
  HistoryIndex := (HistoryIndex + 1) mod HISTORY_SIZE;
end;

function RecupererTemperature(const Reading: TSensorData): Real;
begin
  Result := Reading.Temperature / 10.0;  // 257 → 25.7°C
end;
```

## Techniques extrêmes (< 1 Ko RAM)

### Calcul en place (in-place computation)

```pascal
// Éviter tableaux temporaires
procedure TriBulleInPlace(var Arr: array of Word);
var
  i, j: Byte;
  temp: Word;
begin
  for i := 0 to High(Arr) - 1 do
    for j := 0 to High(Arr) - i - 1 do
      if Arr[j] > Arr[j + 1] then
      begin
        // Swap sans variable temporaire
        Arr[j] := Arr[j] xor Arr[j + 1];
        Arr[j + 1] := Arr[j] xor Arr[j + 1];
        Arr[j] := Arr[j] xor Arr[j + 1];
      end;
end;
```

### Compression de données en temps réel

```pascal
// Stocker échantillons avec compression Delta
type
  TCompressedSample = packed record
    BaseValue: Word;          // Valeur de référence
    Deltas: array[0..7] of ShortInt;  // 8 deltas (différences)
  end;  // 10 octets pour 9 valeurs (au lieu de 18)

var
  Samples: TCompressedSample;

procedure AjouterEchantillon(Index: Byte; Value: Word);
var
  Delta: Integer;
begin
  if Index = 0 then
    Samples.BaseValue := Value
  else
  begin
    Delta := Value - Samples.BaseValue;

    // Si delta trop grand, créer nouveau bloc
    if (Delta < -127) or (Delta > 127) then
    begin
      Samples.BaseValue := Value;
      FillChar(Samples.Deltas, SizeOf(Samples.Deltas), 0);
    end
    else
      Samples.Deltas[Index - 1] := Delta;
  end;
end;

function RecupererEchantillon(Index: Byte): Word;
var
  i: Byte;
  Value: Integer;
begin
  Value := Samples.BaseValue;

  for i := 0 to Index - 1 do
    Value := Value + Samples.Deltas[i];

  Result := Value;
end;
```

### Machine à états sans données

```pascal
// Au lieu de stocker l'état dans une variable
type
  TState = (stIdle, stReading, stProcessing, stSending);

var
  CurrentState: TState;  // 1 octet

// Utiliser des flags matériels comme état
function GetCurrentState: TState;
begin
  if (USART1^.SR and USART_SR_TC) = 0 then
    Exit(stSending);

  if (ADC1^.SR and ADC_SR_EOC) <> 0 then
    Exit(stReading);

  if (DMA1^.ISR and DMA_ISR_TCIF1) <> 0 then
    Exit(stProcessing);

  Result := stIdle;
end;

// Économie : 1 octet de RAM (l'état est dans les registres matériels)
```

### Réutilisation de la pile

```pascal
// Fonction récursive transformée en itérative avec pile explicite
const
  STACK_SIZE = 16;

type
  TStackItem = record
    Value: Word;
    State: Byte;
  end;

var
  Stack: array[0..STACK_SIZE-1] of TStackItem;
  StackPtr: Byte = 0;

procedure Push(Value: Word; State: Byte);
begin
  if StackPtr < STACK_SIZE then
  begin
    Stack[StackPtr].Value := Value;
    Stack[StackPtr].State := State;
    Inc(StackPtr);
  end;
end;

function Pop(var Value: Word; var State: Byte): Boolean;
begin
  if StackPtr > 0 then
  begin
    Dec(StackPtr);
    Value := Stack[StackPtr].Value;
    State := Stack[StackPtr].State;
    Result := True;
  end
  else
    Result := False;
end;

// Permet de contrôler exactement l'utilisation mémoire
```

## Outils et diagnostics

### Script d'analyse mémoire

```bash
#!/bin/bash
# analyze_memory.sh

ELF_FILE=$1

echo "=== ANALYSE MÉMOIRE ==="
echo

echo "--- Tailles des sections ---"
arm-none-eabi-size -A $ELF_FILE
echo

echo "--- Top 20 symboles (code) ---"
arm-none-eabi-nm --size-sort -C $ELF_FILE | grep ' T ' | tail -20
echo

echo "--- Top 20 symboles (données) ---"
arm-none-eabi-nm --size-sort -C $ELF_FILE | grep ' [DdBb] ' | tail -20
echo

echo "--- Détails des sections ---"
arm-none-eabi-objdump -h $ELF_FILE
echo

echo "--- Calcul utilisation RAM ---"
DATA=$(arm-none-eabi-size -A $ELF_FILE | grep '.data' | awk '{print $2}')
BSS=$(arm-none-eabi-size -A $ELF_FILE | grep '.bss' | awk '{print $2}')
TOTAL=$((DATA + BSS))
echo "RAM utilisée : $TOTAL octets"
echo "  .data : $DATA octets"
echo "  .bss  : $BSS octets"
```

### Moniteur RAM en temps réel

```pascal
{$IFDEF DEBUG}
type
  TMemorySnapshot = record
    Timestamp: LongWord;
    FreeRAM: Word;
    UsedStack: Word;
    LargestFreeBlock: Word;
  end;

const
  SNAPSHOT_COUNT = 32;

var
  Snapshots: array[0..SNAPSHOT_COUNT-1] of TMemorySnapshot;
  SnapshotIndex: Byte = 0;

procedure TakeMemorySnapshot;
var
  Snap: TMemorySnapshot;
begin
  Snap.Timestamp := GetTickCount();
  Snap.FreeRAM := GetFreeRAM();
  Snap.UsedStack := GetStackUsage();
  Snap.LargestFreeBlock := GetLargestFreeBlock();

  Snapshots[SnapshotIndex] := Snap;
  SnapshotIndex := (SnapshotIndex + 1) mod SNAPSHOT_COUNT;
end;

procedure DumpMemoryHistory;
var
  i: Byte;
begin
  WriteLn('=== HISTORIQUE MÉMOIRE ===');
  for i := 0 to SNAPSHOT_COUNT - 1 do
  begin
    with Snapshots[i] do
    begin
      if Timestamp > 0 then
      begin
        WriteLn(Timestamp:8, ' ms | RAM: ', FreeRAM:5, ' | Stack: ',
                UsedStack:4, ' | Largest: ', LargestFreeBlock:5);
      end;
    end;
  end;
end;

// Appeler périodiquement
begin
  while True do
  begin
    if (GetTickCount() mod 1000) = 0 then
      TakeMemorySnapshot();

    // Travail normal...
  end;
end;
{$ENDIF}
```

### Visualisation mémoire

```pascal
procedure AfficherCarteMemoire;
const
  RAM_START = $20000000;
  RAM_SIZE = 20480;  // 20 Ko
  BLOCK_SIZE = 256;  // 1 caractère = 256 octets

var
  i, Used: Word;
  p: ^Byte;
  Char: Char;
begin
  WriteLn('Carte mémoire RAM (chaque caractère = 256 octets) :');
  WriteLn('[# = utilisé, . = libre]');
  Write('[');

  p := Pointer(RAM_START);
  for i := 0 to (RAM_SIZE div BLOCK_SIZE) - 1 do
  begin
    // Vérifier si le bloc contient des données non-nulles
    Used := 0;
    for j := 0 to BLOCK_SIZE - 1 do
    begin
      if p^ <> 0 then Inc(Used);
      Inc(p);
    end;

    if Used > (BLOCK_SIZE div 4) then
      Write('#')
    else if Used > 0 then
      Write(':')
    else
      Write('.');
  end;

  WriteLn(']');
  WriteLn('Total : ', RAM_SIZE, ' octets');
end;
```

## Études de cas réels

### Arduino Uno (2 Ko RAM)

```pascal
// Projet : Contrôleur de serre automatisé
// RAM disponible : 2048 octets
// Après stack (512 octets) : 1536 octets disponibles

type
  TSensorValues = packed record
    Temperature: ShortInt;    // 1 octet (-128..127)
    Humidity: Byte;           // 1 octet (0..100)
    SoilMoisture: Byte;       // 1 octet (0..100)
    LightLevel: Byte;         // 1 octet (0..255)
  end;  // 4 octets total

const
  HISTORY_SIZE = 24;  // 24 heures d'historique

var
  CurrentValues: TSensorValues;              // 4 octets
  History: array[0..HISTORY_SIZE-1] of TSensorValues;  // 96 octets

  // État système (utiliser bits)
  SystemFlags: Byte;  // 1 octet
  // Bit 0: Pompe eau active
  // Bit 1: Ventilateur actif
  // Bit 2: Lumière active
  // Bit 3: Alarme température
  // Bit 4: Alarme humidité
  // Bit 5-7: Réservé

  // Paramètres
  Config: packed record
    TempMin, TempMax: ShortInt;      // 2 octets
    HumMin, HumMax: Byte;            // 2 octets
    SoilMin: Byte;                   // 1 octet
    WateringDuration: Byte;          // 1 octet (secondes)
  end;  // 7 octets

  // Buffers communication (partagé)
  CommBuffer: array[0..63] of Byte;  // 64 octets

  // Timers
  LastWatering: LongWord;            // 4 octets
  LastReading: LongWord;             // 4 octets

// Total utilisé : ~185 octets
// Reste libre : ~1350 octets (87% libre !)

procedure LireCapteurs;
begin
  CurrentValues.Temperature := ADC_ReadTemp();
  CurrentValues.Humidity := ADC_ReadHum();
  CurrentValues.SoilMoisture := ADC_ReadSoil();
  CurrentValues.LightLevel := ADC_ReadLight();

  // Vérifier alarmes
  if (CurrentValues.Temperature < Config.TempMin) or
     (CurrentValues.Temperature > Config.TempMax) then
    SystemFlags := SystemFlags or $08;  // Bit 3

  if CurrentValues.SoilMoisture < Config.SoilMin then
    Arroser();
end;

procedure Arroser;
begin
  if (GetTickCount() - LastWatering) > 3600000 then  // Min 1h entre arrosages
  begin
    SystemFlags := SystemFlags or $01;  // Activer pompe
    LastWatering := GetTickCount();

    // Timer pour arrêt
    // (géré par interruption)
  end;
end;
```

### STM32F103 (20 Ko RAM)

```pascal
// Projet : Oscilloscope numérique simple
// RAM disponible : 20480 octets
// Stratégie : Buffer acquisition DMA + affichage

const
  SAMPLE_RATE = 100000;  // 100 kHz
  BUFFER_SIZE = 1024;     // 1024 échantillons

type
  // 12 bits ADC stockés sur 16 bits
  TSample = Word;

var
  // Double buffering DMA
  SampleBuffer1: array[0..BUFFER_SIZE-1] of TSample;  // 2048 octets
  SampleBuffer2: array[0..BUFFER_SIZE-1] of TSample;  // 2048 octets
  ActiveBuffer: ^TSample;

  // Paramètres d'affichage
  DisplayParams: packed record
    TriggerLevel: Word;       // 2 octets
    TriggerSlope: Boolean;    // 1 octet
    TimeDiv: Byte;            // 1 octet (0-10 = 1µs-1s/div)
    VoltDiv: Byte;            // 1 octet (0-7 = 10mV-5V/div)
    Offset: SmallInt;         // 2 octets
    Running: Boolean;         // 1 octet
  end;  // 8 octets

  // Buffer écran (128x64 monochrome)
  FrameBuffer: array[0..1023] of Byte;  // 1024 octets

  // Stats
  Stats: packed record
    Min, Max, Avg: Word;      // 6 octets
    Frequency: LongWord;      // 4 octets
  end;  // 10 octets

// Total : ~7 Ko
// Reste : ~13 Ko pour stack et autres variables

procedure AnalyserSignal;
var
  i: Word;
  Sum: LongWord;
  LastZeroCross, CurrentZeroCross: Word;
begin
  Stats.Min := 4095;
  Stats.Max := 0;
  Sum := 0;

  for i := 0 to BUFFER_SIZE - 1 do
  begin
    if ActiveBuffer[i] < Stats.Min then
      Stats.Min := ActiveBuffer[i];
    if ActiveBuffer[i] > Stats.Max then
      Stats.Max := ActiveBuffer[i];
    Sum := Sum + ActiveBuffer[i];

    // Détection zéro-crossing pour fréquence
    if (i > 0) and
       (ActiveBuffer[i-1] < DisplayParams.TriggerLevel) and
       (ActiveBuffer[i] >= DisplayParams.TriggerLevel) then
    begin
      CurrentZeroCross := i;
      if LastZeroCross > 0 then
      begin
        // Calculer fréquence
        Stats.Frequency := SAMPLE_RATE div (CurrentZeroCross - LastZeroCross);
      end;
      LastZeroCross := CurrentZeroCross;
    end;
  end;

  Stats.Avg := Sum div BUFFER_SIZE;
end;

procedure DessinerSignal;
var
  i, x, y: Word;
  Sample: Word;
begin
  // Effacer écran
  FillChar(FrameBuffer, SizeOf(FrameBuffer), 0);

  // Dessiner échantillons
  for i := 0 to 127 do  // 128 pixels de large
  begin
    x := i;
    Sample := ActiveBuffer[i * 8];  // Sous-échantillonner 1024→128

    // Convertir échantillon en Y (0-63)
    y := 63 - ((Sample * 64) div 4096);

    SetPixel(x, y);
  end;

  // Envoyer à l'écran
  OLED_Update(@FrameBuffer);
end;
```

## Bonnes pratiques finales

### 1. Planifier avant de coder

```
Avant d'écrire du code :
1. Estimer la RAM nécessaire
2. Lister toutes les variables globales
3. Calculer les buffers nécessaires
4. Vérifier que total < 80% RAM disponible
```

### 2. Mesurer régulièrement

```pascal
// Ajouter dans chaque build
{$INFO 'Variables globales : ' + IntToStr(SizeOfGlobals)}
{$INFO 'Plus grosse fonction : ' + IntToStr(SizeOfBiggestFunction)}

// Compiler avec -va pour voir toutes les infos
```

### 3. Documenter les contraintes

```pascal
// En-tête de fichier
{
  Contraintes mémoire :
  - RAM totale : 20 Ko
  - Stack : 2 Ko (max 10 niveaux appels)
  - Variables globales : 5 Ko
  - Buffers : 8 Ko
  - Heap : Désactivé
  - Marge : 5 Ko (25%)
}

const
  RAM_TOTAL = 20480;
  RAM_STACK = 2048;
  RAM_GLOBALS = 5120;
  RAM_BUFFERS = 8192;
  RAM_MARGIN = 5120;
```

### 4. Tester les limites

```pascal
procedure TestLimitesPile;
var
  GrosTableau: array[0..511] of Byte;  // 512 octets sur pile
begin
  FillChar(GrosTableau, SizeOf(GrosTableau), $AA);

  // Vérifier débordement
  if not VerifierIntegritePile() then
  begin
    UART_Send('ERREUR: Pile corrompue!'#13#10);
    while True do ;
  end;

  // Test récursif
  TestLimitesPile();  // Augmenter profondeur jusqu'au crash
end;

// Exécuter en DEBUG pour trouver les limites
```

### 5. Optimiser progressivement

```
Ordre d'optimisation :
1. Éliminer gaspillages évidents (types trop grands)
2. Partager les buffers
3. Utiliser packed records
4. Compresser les données
5. Techniques avancées seulement si nécessaire
```

## Ressources et références

### Outils recommandés

- **arm-none-eabi-size** : Analyser tailles binaires
- **arm-none-eabi-nm** : Lister symboles et tailles
- **Bloaty McBloatface** : Analyseur détaillé de binaires
- **Valgrind** : Détection fuites (sur simulateur)
- **heaptrack** : Profiling allocation mémoire

### Documentation

- **ARM Cortex-M Programming Guide** : Comprendre la mémoire ARM
- **Embedded C Coding Standard** : Bonnes pratiques (applicable à Pascal)
- **Making Embedded Systems** (Elecia White) : Chapitre sur gestion mémoire

### Communauté

- **Forum Lazarus** : Section Embedded
- **FreePascal Wiki** : Page "Embedded Systems"
- **Stack Overflow** : Tag [freepascal] + [embedded]

## Conclusion

L'optimisation mémoire en embarqué est un **art autant qu'une science**. Les principes clés sont :

1. **Mesurer** : Connaître exactement l'utilisation
2. **Planifier** : Anticiper les besoins avant de coder
3. **Choisir** : Le bon type pour chaque donnée
4. **Partager** : Réutiliser les buffers quand possible
5. **Compacter** : Utiliser packed, bits, compression
6. **Tester** : Vérifier sous charge maximale

Avec FreePascal, vous disposez d'un langage de haut niveau tout en gardant le **contrôle total** sur la mémoire, comme en C mais avec une syntaxe plus claire et sûre.

Un microcontrôleur avec seulement **512 octets de RAM** peut accomplir des tâches remarquables si la mémoire est gérée intelligemment. L'optimisation n'est pas une contrainte mais une opportunité de créer du code **élégant, efficace et robuste**.

Bonne optimisation ! 🚀

⏭️ [Edge computing](/14-systemes-embarques-iot/11-edge-computing.md)
