🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 1.6 Modes de compatibilité (Turbo Pascal, Delphi, ObjFPC)

## Introduction : Pourquoi différents modes ?

### L'histoire derrière les modes

FreePascal est unique dans sa capacité à "parler" plusieurs dialectes du langage Pascal. Cette polyvalence n'est pas accidentelle mais résulte de l'histoire riche du Pascal :

```
1970 : Pascal original (Niklaus Wirth)
  ↓
1983 : Turbo Pascal (Borland)
  ↓
1995 : Delphi (Borland) - Object Pascal
  ↓
1993-aujourd'hui : FreePascal
  ├── Mode TP (compatibilité Turbo Pascal)
  ├── Mode Delphi (compatibilité maximale)
  ├── Mode ObjFPC (innovations FreePascal)
  └── Autres modes (MacPas, ISO, etc.)
```

### Pourquoi avoir plusieurs modes ?

Les modes existent pour trois raisons principales :

1. **Migration facilitée** : Compiler du code existant sans modifications
2. **Compatibilité** : Partager du code entre différents compilateurs
3. **Philosophies différentes** : Chaque mode reflète une approche du langage

### Comment fonctionnent les modes

Un mode de compatibilité affecte :
- **La syntaxe** acceptée par le compilateur
- **Les mots-clés** disponibles
- **Le comportement** de certaines constructions
- **Les unités** automatiquement incluses
- **Les directives** de compilation reconnues

## Vue d'ensemble des modes principaux

### Tableau comparatif rapide

| Mode | Origine | Usage principal | Complexité | Modernité |
|------|---------|-----------------|------------|-----------|
| **TP** | Turbo Pascal 7 | Code legacy DOS | Simple | ⭐ |
| **FPC** | FreePascal natif | Projets simples | Moyenne | ⭐⭐ |
| **ObjFPC** | FreePascal objet | Nouveaux projets | Moyenne | ⭐⭐⭐⭐ |
| **Delphi** | Borland Delphi | Migration Delphi | Complexe | ⭐⭐⭐ |
| **MacPas** | Mac Pascal | Code Mac classic | Simple | ⭐ |
| **ISO** | Standard ISO | Académique | Très simple | ⭐ |

### Activation d'un mode

Les modes peuvent être activés de trois façons :

```pascal
// 1. Dans le code source (recommandé)
{$MODE OBJFPC}

// 2. En ligne de commande
fpc -Mobjfpc program.pas

// 3. Dans fpc.cfg
-Mobjfpc
```

## Mode Turbo Pascal (TP)

### Présentation du mode TP

Le mode Turbo Pascal reproduit le comportement de Turbo Pascal 7.0, le dernier de la lignée DOS :

```pascal
{$MODE TP}

program HelloTP;
uses Crt, Dos;

var
  Name: string[80];  { Strings courts (ShortString) }

begin
  ClrScr;
  Write('Enter your name: ');
  ReadLn(Name);
  WriteLn('Hello, ', Name);
  ReadKey;
end.
```

### Caractéristiques du mode TP

#### Ce qui est disponible

- **Strings courts** : Limités à 255 caractères
- **Syntaxe procédurale** : Pas de classes
- **Unités DOS** : Crt, Dos, Graph
- **Pointeurs simples** : Syntaxe @ et ^
- **Assembleur inline** : Syntaxe `asm...end`
- **Interruptions** : Support des interrupts DOS

> **Attention** : En mode TP, le type `Integer` est de **16 bits** (plage -32768..32767), comme dans le Turbo Pascal original. Dans tous les autres modes (FPC, ObjFPC, Delphi), `Integer` est de **32 bits** (plage -2147483648..2147483647), quel que soit le processeur cible. C'est le **mode** qui détermine la taille d'`Integer`, pas la plateforme.

#### Ce qui n'est PAS disponible

- ❌ Classes et objets modernes
- ❌ Strings longs (AnsiString)
- ❌ Surcharge d'opérateurs
- ❌ Génériques
- ❌ Propriétés avancées
- ❌ Exceptions try...except

### Syntaxe spécifique TP

```pascal
{$MODE TP}

program TPExample;

type
  { Objects old-style (pas des classes) }
  PMyObject = ^TMyObject;
  TMyObject = object
    X, Y: Integer;
    procedure Init(AX, AY: Integer);
    procedure Show;
  end;

procedure TMyObject.Init(AX, AY: Integer);
begin
  X := AX;
  Y := AY;
end;

procedure TMyObject.Show;
begin
  WriteLn('Position: ', X, ',', Y);
end;

var
  Obj: TMyObject;
  P: PMyObject;
  S: string[20];  { Strings courts obligatoires en mode TP }

begin
  { Syntaxe @ pour adresse }
  P := @Obj;
  P^.Init(10, 20);
  P^.Show;

  { Strings courts (maximum 255 caractères) }
  S := 'Maximum 20 chars';
  WriteLn(S);
end.
```

### Cas d'usage du mode TP

#### Quand utiliser le mode TP

✅ **Code legacy DOS** : Maintenir d'anciennes applications  
✅ **Éducation** : Apprendre les bases du Pascal  
✅ **Systèmes embarqués** : Code simple et prévisible  
✅ **Nostalgie** : Recréer l'expérience Turbo Pascal

#### Exemple : Migration d'un programme DOS

```pascal
{$MODE TP}
{ Programme Turbo Pascal original - fonctionne sans modification }

program DOSMenu;
uses Crt;

procedure DrawBox(X1, Y1, X2, Y2: Byte);
var
  I: Byte;
begin
  GotoXY(X1, Y1); Write('┌');
  for I := X1+1 to X2-1 do Write('─');
  Write('┐');

  for I := Y1+1 to Y2-1 do
  begin
    GotoXY(X1, I); Write('│');
    GotoXY(X2, I); Write('│');
  end;

  GotoXY(X1, Y2); Write('└');
  for I := X1+1 to X2-1 do Write('─');
  Write('┘');
end;

begin
  ClrScr;
  TextColor(Yellow);
  DrawBox(10, 5, 70, 20);
  GotoXY(35, 12);
  WriteLn('MENU DOS');
  ReadKey;
end.
```

## Mode FPC (par défaut)

### Présentation du mode FPC

Le mode FPC est le mode natif de FreePascal, un équilibre entre simplicité et fonctionnalités :

```pascal
{$MODE FPC}

program HelloFPC;

type
  TPoint = record
    X, Y: Integer;
  end;

var
  S: AnsiString;  { Strings longs disponibles }
  P: TPoint;

begin
  S := 'Hello from FPC mode';
  P.X := 10;
  P.Y := 20;
  WriteLn(S, ' at position ', P.X, ',', P.Y);
end.
```

### Caractéristiques du mode FPC

#### Fonctionnalités disponibles

- **Strings longs** : AnsiString sans limite
- **Procédural et objet** : Support des deux paradigmes
- **Surcharge basique** : Fonctions avec même nom
- **Classes simples** : POO disponible
- **Opérateurs C** : +=, -=, etc. (avec {$COPERATORS ON})

#### Syntaxe particulière

```pascal
{$MODE FPC}

program FPCFeatures;

type
  { Classes disponibles }
  TMyClass = class
  private
    FValue: Integer;
  public
    constructor Create(AValue: Integer);
    property Value: Integer read FValue write FValue;
  end;

constructor TMyClass.Create(AValue: Integer);
begin
  FValue := AValue;
end;

{ Surcharge de fonctions }
function Add(A, B: Integer): Integer; overload;
begin
  Result := A + B;
end;

function Add(A, B: Double): Double; overload;
begin
  Result := A + B;
end;

var
  Obj: TMyClass;

begin
  Obj := TMyClass.Create(42);
  WriteLn('Value: ', Obj.Value);
  WriteLn('Int: ', Add(1, 2));
  WriteLn('Float: ', Add(1.5, 2.5):0:1);
  Obj.Free;
end.
```

## Mode ObjFPC (recommandé)

### Présentation du mode ObjFPC

ObjFPC est le mode **recommandé pour les nouveaux projets**. Il combine le meilleur de FPC avec des extensions orientées objet :

```pascal
{$MODE OBJFPC}{$H+}  { $H+ active les strings longs par défaut }

program HelloObjFPC;

type
  { Syntaxe moderne et claire }
  TGreeter = class
  private
    FName: string;
  public
    constructor Create(const AName: string);
    procedure SayHello;
    property Name: string read FName write FName;
  end;

constructor TGreeter.Create(const AName: string);
begin
  inherited Create;  { Appel explicite du parent }
  FName := AName;
end;

procedure TGreeter.SayHello;
begin
  WriteLn('Hello, ', FName, '!');
end;

var
  Greeter: TGreeter;

begin
  Greeter := TGreeter.Create('World');
  try
    Greeter.SayHello;
  finally
    Greeter.Free;  { Gestion propre de la mémoire }
  end;
end.
```

### Caractéristiques distinctives d'ObjFPC

#### Différences avec le mode FPC standard

| Fonctionnalité | FPC | ObjFPC | Explication |
|----------------|-----|---------|-------------|
| **@ retourne** | Pointeur | Adresse procédure | Plus cohérent |
| **Inherited** | Implicite | Explicite requis | Plus clair |
| **Objc** | Non | Oui | Objective-C |
| **Blocks** | Non | Oui | Blocks Mac/iOS |
| **Opérateurs C** | Option | Inclus | +=, -=, etc. |

#### Syntaxe avancée ObjFPC

```pascal
{$MODE OBJFPC}{$H+}

program ObjFPCAdvanced;

uses
  SysUtils, Math;  { SysUtils requis pour Exception, Math pour Sqrt }

type
  { Génériques (templates) }
  generic TList<T> = class
  private
    FItems: array of T;
    FCount: Integer;
  public
    procedure Add(const Item: T);
    function Get(Index: Integer): T;
    property Count: Integer read FCount;
  end;

  { Spécialisation }
  TIntList = specialize TList<Integer>;
  TStringList = specialize TList<string>;

  { Helpers de classe }
  TIntegerHelper = type helper for Integer
    function IsEven: Boolean;
    function IsPrime: Boolean;
    function ToString: string;
  end;

{ Implémentation du helper }
function TIntegerHelper.IsEven: Boolean;
begin
  Result := (Self mod 2) = 0;
end;

function TIntegerHelper.IsPrime: Boolean;
var
  I: Integer;
begin
  if Self < 2 then Exit(False);
  for I := 2 to Trunc(Sqrt(Self)) do
    if (Self mod I) = 0 then Exit(False);
  Result := True;
end;

function TIntegerHelper.ToString: string;
begin
  Str(Self, Result);
end;

{ Implémentation générique }
procedure TList.Add(const Item: T);
begin
  SetLength(FItems, FCount + 1);
  FItems[FCount] := Item;
  Inc(FCount);
end;

function TList.Get(Index: Integer): T;
begin
  if (Index >= 0) and (Index < FCount) then
    Result := FItems[Index]
  else
    raise Exception.Create('Index out of bounds');
end;

var
  Numbers: TIntList;
  N: Integer;

begin
  Numbers := TIntList.Create;
  try
    { Utilisation du helper }
    for N := 1 to 20 do
    begin
      if N.IsPrime then
      begin
        Numbers.Add(N);
        WriteLn(N.ToString, ' is prime');
      end;
    end;

    WriteLn('Found ', Numbers.Count, ' prime numbers');
  finally
    Numbers.Free;
  end;
end.
```

### Fonctionnalités exclusives ObjFPC

#### Opérateurs avancés

```pascal
{$MODE OBJFPC}{$H+}

program VectorDemo;

type
  TVector = record
    X, Y: Double;
  end;

{ En mode ObjFPC, les opérateurs sont des fonctions globales }
operator +(const A, B: TVector): TVector;
begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
end;

operator *(const A: TVector; B: Double): TVector;
begin
  Result.X := A.X * B;
  Result.Y := A.Y * B;
end;

operator =(const A, B: TVector): Boolean;
begin
  Result := (A.X = B.X) and (A.Y = B.Y);
end;

var
  V1, V2, V3: TVector;
begin
  V1.X := 1; V1.Y := 2;
  V2.X := 3; V2.Y := 4;

  V3 := V1 + V2;           { Utilise l'opérateur + }
  V3 := V3 * 2.0;          { Utilise l'opérateur * }

  if V1 = V2 then          { Utilise l'opérateur = }
    WriteLn('Vectors are equal')
  else
    WriteLn('Vectors are different');
end.
```

> **Note** : En mode Delphi, les opérateurs se déclarent avec `class operator` à l'intérieur du record. En mode ObjFPC, on utilise des fonctions `operator` globales.

## Mode Delphi

### Présentation du mode Delphi

Le mode Delphi assure une **compatibilité maximale** avec le code Delphi :

```pascal
{$MODE DELPHI}

program HelloDelphi;

uses
  SysUtils, Classes;  { Unités Delphi-compatible }

type
  TCustomer = class(TPersistent)
  private
    FName: string;
    FAge: Integer;
    function GetInfo: string;
  public
    constructor Create(const AName: string; AAge: Integer);
    property Name: string read FName write FName;
    property Age: Integer read FAge write FAge;
    property Info: string read GetInfo;
  end;

constructor TCustomer.Create(const AName: string; AAge: Integer);
begin
  inherited Create;  { inherited peut être implicite en Delphi }
  FName := AName;
  FAge := AAge;
end;

function TCustomer.GetInfo: string;
begin
  Result := Format('%s (%d years)', [FName, FAge]);
end;

var
  Customer: TCustomer;

begin
  Customer := TCustomer.Create('John Doe', 30);
  try
    WriteLn(Customer.Info);
  finally
    Customer.Free;
  end;
end.
```

### Spécificités du mode Delphi

#### Différences syntaxiques principales

```pascal
{$MODE DELPHI}

type
  TMyClass = class
  public
    { @ retourne un pointeur non-typé (Delphi) }
    procedure DoSomething;
  end;

procedure TestPointers;
var
  Obj: TMyClass;
  P: Pointer;
  Proc: TProcedure;
begin
  Obj := TMyClass.Create;

  { En mode Delphi, @ retourne Pointer }
  P := @Obj;  { P est un Pointer }

  { Pour l'adresse d'une méthode }
  Proc := @Obj.DoSomething;  { Compatible Delphi }

  { Inherited est optionnel }
  { Les strings sont AnsiString par défaut }
  { Les sets peuvent avoir plus de 256 éléments }
end;

{ Propriétés array (Delphi style) }
type
  TStringArray = class
  private
    FItems: array of string;
    function GetItem(Index: Integer): string;
    procedure SetItem(Index: Integer; const Value: string);
  public
    property Items[Index: Integer]: string
      read GetItem write SetItem; default;  { default property }
  end;

var
  SA: TStringArray;
begin
  SA := TStringArray.Create;
  SA[0] := 'Hello';  { Utilise la propriété default }
  WriteLn(SA[0]);
end;
```

#### Compatibilité des unités

```pascal
{$MODE DELPHI}

uses
  { Unités Delphi standard reconnues }
  SysUtils,     { Utilitaires système }
  Classes,      { TList, TStringList, etc. }
  Math,         { Fonctions mathématiques }
  DateUtils,    { Manipulation dates }
  StrUtils,     { Manipulation strings }
  Variants,     { Support Variant }
  Types,        { Types communs }
  IniFiles,     { Fichiers INI }
  Registry;     { Registre Windows }

{ Code Delphi typique fonctionne sans modification }
procedure DelphiStyleCode;
var
  SL: TStringList;
  I: Integer;
begin
  SL := TStringList.Create;
  try
    SL.Add('Line 1');
    SL.Add('Line 2');

    for I := 0 to SL.Count - 1 do
      WriteLn(SL[I]);

    SL.SaveToFile('output.txt');
  finally
    SL.Free;
  end;
end;
```

### Migration Delphi vers FreePascal

#### Code portable Delphi/FreePascal

```pascal
{ Code qui compile dans les deux environnements }
{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

unit Portable;

interface

uses
  {$IFDEF FPC}
  Classes, SysUtils
  {$ELSE}
  System.Classes, System.SysUtils  { Namespaces Delphi XE2+ }
  {$ENDIF};

type
  TPortableClass = class
  private
    FValue: string;
  public
    constructor Create;
    property Value: string read FValue write FValue;
  end;

implementation

constructor TPortableClass.Create;
begin
  {$IFDEF FPC}
  inherited Create;  { Explicite pour FPC }
  {$ELSE}
  inherited;         { Implicite OK pour Delphi }
  {$ENDIF}

  FValue := 'Portable';
end;

end.
```

## Autres modes disponibles

### Mode MacPas

Pour compatibilité avec Mac Pascal traditionnel :

```pascal
{$MODE MACPAS}

program MacPasExample;

type
  Str255 = string[255];

procedure MacStyleProc(var S: Str255);
begin
  S := 'Mac Pascal style';
end;

var
  MyStr: Str255;

begin
  MacStyleProc(MyStr);
  WriteLn(MyStr);

  { Syntaxe spécifique Mac Pascal }
  { UNIV parameters, CYCLE instead of Continue, etc. }
end.
```

### Mode ISO

Pour Pascal standard ISO 7185 :

```pascal
{$MODE ISO}

program ISOPascal(Output);

var
  I: Integer;

begin
  for I := 1 to 10 do
    WriteLn(Output, 'ISO Pascal line ', I);
end.
```

### Mode Extended Pascal

Pour Pascal étendu ISO 10206 :

```pascal
{$MODE EXTENDEDPASCAL}

program ExtPascal;

var
  S: string(100);  { String avec taille max }

begin
  S := 'Extended Pascal features';
  WriteLn(S);
end.
```

## Comparaison détaillée des modes

### Tableau des fonctionnalités

| Fonctionnalité | TP | FPC | ObjFPC | Delphi | MacPas | ISO |
|----------------|-----|-----|---------|---------|---------|-----|
| **Classes** | ❌ | ✅ | ✅ | ✅ | ⚠️ | ❌ |
| **Objects (old)** | ✅ | ✅ | ✅ | ✅ | ✅ | ❌ |
| **Strings longs** | ❌ | ✅ | ✅ | ✅ | ❌ | ❌ |
| **Exceptions** | ❌ | ✅ | ✅ | ✅ | ❌ | ❌ |
| **Génériques** | ❌ | ⚠️ | ✅ | ✅ | ❌ | ❌ |
| **Surcharge** | ❌ | ✅ | ✅ | ✅ | ❌ | ❌ |
| **Properties** | ❌ | ✅ | ✅ | ✅ | ⚠️ | ❌ |
| **Interfaces** | ❌ | ✅ | ✅ | ✅ | ❌ | ❌ |
| **Opérateurs** | ❌ | ⚠️ | ✅ | ⚠️ | ❌ | ❌ |
| **Inline** | ❌ | ✅ | ✅ | ✅ | ❌ | ❌ |
| **Variants** | ❌ | ✅ | ✅ | ✅ | ❌ | ❌ |

✅ = Supporté complètement  
⚠️ = Support partiel ou avec options  
❌ = Non supporté

### Comportements différents selon le mode

#### Exemple : Gestion de @

> **Important** : Un seul `{$MODE}` est autorisé par unité ou programme. Les exemples ci-dessous montrent le comportement de `@` selon le mode choisi, chacun dans un programme séparé.

**En mode TP ou FPC :**

```pascal
{$MODE FPC}
program AtOperatorTP;
type
  TProc = procedure;
procedure MyProc;
begin
  WriteLn('Called');
end;
var
  P: Pointer;
begin
  P := @MyProc;      { P = pointeur vers MyProc }
end.
```

**En mode ObjFPC :**

```pascal
{$MODE OBJFPC}
program AtOperatorObjFPC;
type
  TProc = procedure;
procedure MyProc;
begin
  WriteLn('Called');
end;
var
  Proc: TProc;
begin
  Proc := @MyProc;          { @ obligatoire pour affecter une procédure }
  { P := @MyProc;  -- ne compile pas directement, il faut caster }
end.
```

**En mode Delphi :**

```pascal
{$MODE DELPHI}
program AtOperatorDelphi;
type
  TProc = procedure;
procedure MyProc;
begin
  WriteLn('Called');
end;
var
  P: Pointer;
  Proc: TProc;
begin
  P := @MyProc;      { P = pointeur non-typé }
  Proc := MyProc;    { @ optionnel pour procédures }
end.
```

#### Exemple : Inherited

> **Important** : Un seul `{$MODE}` par unité. Ces exemples montrent le comportement de `inherited` selon le mode, chacun supposant être dans un fichier séparé.

```pascal
{ En mode ObjFPC : inherited Create est OBLIGATOIRE }
inherited Create;

{ En mode Delphi : inherited seul est suffisant }
inherited;

{ En mode FPC : inherited Create est recommandé mais pas obligatoire }
inherited Create;
```

## Choix du mode approprié

### Arbre de décision

```
Nouveau projet ?
├── OUI → ObjFPC (recommandé)
└── NON → Code existant ?
    ├── Turbo Pascal → Mode TP
    ├── Delphi → Mode Delphi
    ├── Mac Pascal → Mode MacPas
    └── Autre → Mode FPC
```

### Recommandations par cas d'usage

#### Nouveaux projets

```pascal
{$MODE OBJFPC}{$H+}
{ Recommandé pour tous les nouveaux projets }
{ - Syntaxe moderne et claire }
{ - Toutes les fonctionnalités disponibles }
{ - Bonnes pratiques encouragées }
```

#### Migration depuis Delphi

```pascal
{$MODE DELPHI}
{ Pour code Delphi existant }
{ - Compatibilité maximale }
{ - Minimum de modifications }
{ - Support des idiomes Delphi }
```

#### Code legacy DOS

```pascal
{$MODE TP}
{ Pour ancien code Turbo Pascal }
{ - Compile le code DOS original }
{ - Support CRT, Graph, etc. }
{ - Pas de modifications nécessaires }
```

#### Projets multi-compilateurs

```pascal
{$IFDEF FPC}
  {$MODE DELPHI}  { Ou OBJFPC selon préférence }
{$ENDIF}
{ Pour code devant compiler avec FPC et Delphi }
```

## Migration entre modes

### Stratégie de migration progressive

#### Étape 1 : Compilation dans le mode original

```pascal
{$MODE TP}  { Si code Turbo Pascal }
{ Vérifier que tout compile }
```

#### Étape 2 : Identifier les incompatibilités

```pascal
{$MODE FPC}  { Passer en mode plus moderne }
{ Noter les erreurs de compilation }
```

#### Étape 3 : Adapter progressivement

> **Rappel** : On ne peut avoir qu'un seul `{$MODE}` par fichier source. Pour une migration progressive, utilisez des directives conditionnelles `{$IFDEF}` autour des constructions spécifiques, pas autour des modes.

```pascal
{$MODE OBJFPC}{$H+}

{ Adapter les types selon la cible }
{$IFDEF LEGACY_STRINGS}
type
  TMyString = string[255];  { Compatible ShortString }
{$ELSE}
type
  TMyString = string;       { AnsiString en ObjFPC avec $H+ }
{$ENDIF}
```

### Patterns de code multi-modes

```pascal
unit MultiMode;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

interface

type
  {$IFDEF MODE_TP}
  TMyString = string[255];
  {$ELSE}
  TMyString = string;
  {$ENDIF}

  TMyClass = class
  private
    FValue: TMyString;
  public
    constructor Create;
    property Value: TMyString read FValue write FValue;
  end;

implementation

constructor TMyClass.Create;
begin
  {$IFDEF MODE_OBJFPC}
  inherited Create;  { Explicite pour ObjFPC }
  {$ELSE}
  inherited;         { Implicite pour autres }
  {$ENDIF}

  FValue := 'Multi-mode compatible';
end;

end.
```

## Meilleures pratiques

### Documentation du mode utilisé

Toujours documenter le mode au début du fichier :

```pascal
{==============================================================================
  Unit: MyUnit
  Author: Your Name
  Date: 2024-01-15
  Compiler Mode: OBJFPC

  Description: This unit requires ObjFPC mode for generics support

  Dependencies:
  - FreePascal 3.2.0 or higher
  - ObjFPC mode required
==============================================================================}

{$MODE OBJFPC}{$H+}

unit MyUnit;
```

### Configuration projet

Dans le fichier `.lpi` (Lazarus) ou `.cfg` :

```xml
<!-- Lazarus project file -->
<CompilerOptions>
  <Parsing>
    <SyntaxMode Value="objfpc"/>
    <UseAnsiStrings Value="True"/>
  </Parsing>
</CompilerOptions>
```

### Tests multi-modes

Script de test pour vérifier la compatibilité :

```bash
#!/bin/bash
# test-modes.sh

echo "Testing compilation in different modes..."

for MODE in tp fpc objfpc delphi; do
    echo "Testing mode: $MODE"
    fpc -M$MODE test.pas -o test_$MODE
    if [ $? -eq 0 ]; then
        echo "✓ $MODE compiled successfully"
    else
        echo "✗ $MODE failed"
    fi
done
```

## Conclusion

### Récapitulatif des modes

- **TP** : Pour code Turbo Pascal legacy
- **FPC** : Mode de base FreePascal
- **ObjFPC** : Moderne et recommandé pour nouveaux projets
- **Delphi** : Compatibilité maximale avec Delphi
- **MacPas** : Code Mac Pascal historique
- **ISO** : Conformité standard académique

### Conseils finaux

1. **Commencez avec ObjFPC** pour les nouveaux projets
2. **Utilisez Delphi** uniquement pour la compatibilité
3. **Documentez** toujours le mode utilisé
4. **Testez** avec plusieurs modes si nécessaire
5. **Évitez** de mélanger les modes dans un même projet

### Le mode idéal

Il n'y a pas de mode "parfait", mais des modes adaptés :
- **Pour apprendre** : ObjFPC (moderne et propre)
- **Pour migrer** : Le mode correspondant au code source
- **Pour innover** : ObjFPC (toutes les fonctionnalités)
- **Pour maintenir** : Le mode original du projet

Le choix du mode est une décision importante qui affecte tout le projet. Prenez le temps de bien choisir au début, car changer de mode plus tard peut nécessiter des modifications importantes du code.

⏭️ [Architecture du compilateur et processus de compilation](/01-introduction-freepascal-lazarus/07-architecture-compilateur-processus-compilation.md)
