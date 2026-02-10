🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 3.1 Spécificités FreePascal vs Delphi

## Introduction : Deux Branches d'un Même Arbre

FreePascal et Delphi partagent une histoire commune mais ont évolué différemment. Imaginez deux frères jumeaux : l'un (Delphi) a choisi une carrière commerciale chez Embarcadero, l'autre (FreePascal) a embrassé la philosophie open source. Bien qu'ils parlent essentiellement le même langage, chacun a développé ses propres particularités et accents.

Pour les débutants, il est important de comprendre que FreePascal n'est pas un "clone" de Delphi, mais plutôt un compilateur indépendant qui offre une excellente compatibilité avec le code Delphi tout en apportant ses propres innovations.

## Les Modes de Compatibilité : Choisir son Dialecte

### Comprendre les Modes du Compilateur

FreePascal est polyglotte : il peut "parler" plusieurs dialectes de Pascal. C'est comme un traducteur qui maîtrise plusieurs variantes d'une langue.

```pascal
// Mode FreePascal natif (recommandé pour les nouveaux projets)
{$mode objfpc}
{$H+}  // Strings longs activés

// Mode Delphi (pour compatibilité maximale avec code Delphi existant)
{$mode delphi}

// Mode Turbo Pascal (pour code historique)
{$mode tp}

// Mode ISO Pascal (standard académique)
{$mode iso}
```

### Pourquoi Différents Modes ?

Chaque mode active différentes fonctionnalités et comportements :

```pascal
// En mode ObjFPC
{$mode objfpc}
type
  TMyClass = class
    procedure DoSomething(const S: string);  // 'const' recommandé pour optimisation
  end;
```

```pascal
// En mode Delphi
{$mode delphi}
type
  TMyClass = class
    procedure DoSomething(S: string);  // Delphi accepte sans 'const'
  end;
```

> **Note** : Un seul `{$mode}` est autorisé par unité ou programme. Les deux exemples ci-dessus représentent deux fichiers distincts.

**Conseil pour débutants** : Utilisez `{$mode objfpc}` pour les nouveaux projets. Ce mode encourage les bonnes pratiques et offre les meilleures optimisations.

## Différences de Syntaxe et de Comportement

### 1. Généricité : Syntaxes Différentes

La généricité permet de créer des types qui fonctionnent avec différents types de données. C'est comme avoir un moule qui peut fabriquer des objets de différentes matières.

```pascal
// Syntaxe FreePascal (mode objfpc)
type
  // Déclaration d'un type générique
  generic TList<T> = class
    procedure Add(Item: T);
  end;

  // Utilisation : mot-clé 'specialize'
  TIntegerList = specialize TList<Integer>;
  TStringList = specialize TList<String>;

// Syntaxe Delphi (mode delphi)
type
  // Déclaration plus directe
  TList<T> = class
    procedure Add(Item: T);
  end;

  // Utilisation directe sans 'specialize'
  TIntegerList = TList<Integer>;
  TStringList = TList<String>;
```

**Pour les débutants** : La syntaxe FreePascal avec `generic` et `specialize` est plus explicite - elle dit clairement "ceci est un modèle générique" et "maintenant je crée une version spécifique".

### 2. Gestion des Propriétés de Classe

Les propriétés de classe sont comme des variables globales mais attachées à une classe.

```pascal
// FreePascal permet les propriétés de classe statiques
type
  TConfiguration = class
  private
    class var FAppName: string;
  public
    class property AppName: string read FAppName write FAppName;
  end;

// Utilisation sans créer d'instance
begin
  TConfiguration.AppName := 'MonApplication';
  WriteLn(TConfiguration.AppName);
end;

// Delphi nécessite souvent des méthodes getter/setter pour le même résultat
type
  TConfiguration = class
  private
    class var FAppName: string;
    class function GetAppName: string; static;
    class procedure SetAppName(const Value: string); static;
  public
    class property AppName: string read GetAppName write SetAppName;
  end;
```

### 3. Opérateurs : Plus de Flexibilité en FreePascal

FreePascal offre plus d'opérateurs surchargeables que Delphi :

```pascal
{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

uses
  Math;  // Pour Power()

type
  TVector = record
    X, Y: Double;
    // Les méthodes sur records nécessitent {$modeswitch advancedrecords}
  end;

// En ObjFPC, les opérateurs sur records sont déclarés globalement
// FreePascal supporte plus d'opérateurs que Delphi
operator + (A, B: TVector): TVector;  
operator ** (A: TVector; B: Double): TVector;  // Puissance - pas en Delphi  
operator >< (A, B: TVector): Boolean;          // Symétrique - spécifique FPC

implementation

operator +(A, B: TVector): TVector;  
begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
end;

operator **(A: TVector; B: Double): TVector;  
begin
  // Élévation à la puissance
  Result.X := Power(A.X, B);
  Result.Y := Power(A.Y, B);
end;

operator ><(A, B: TVector): Boolean;  
begin
  // Test de différence symétrique (exemple personnalisé)
  Result := (A.X <> B.X) or (A.Y <> B.Y);
end;
```

> **Note** : En mode Delphi (`{$mode delphi}`), les opérateurs peuvent être déclarés à l'intérieur du record avec `class operator`. En mode ObjFPC, ils sont toujours déclarés globalement.

### 4. Inline Assembly : Approches Différentes

L'assembleur inline permet d'écrire du code machine directement. C'est comme pouvoir parler directement au processeur dans sa langue natale.

```pascal
// FreePascal - syntaxe AT&T par défaut sur Linux
function AddFPC(a, b: Integer): Integer; assembler;  
asm
  {$ifdef CPUX86_64}
    // Syntaxe AT&T (source, destination)
    movl %edi, %eax
    addl %esi, %eax
  {$else}
    mov eax, a
    add eax, b
  {$endif}
end;

// Delphi - syntaxe Intel
function AddDelphi(a, b: Integer): Integer;  
asm
  // Syntaxe Intel (destination, source)
  mov eax, a
  add eax, b
end;

// FreePascal peut aussi utiliser la syntaxe Intel
{$asmmode intel}
function AddIntel(a, b: Integer): Integer; assembler;  
asm
  mov eax, a
  add eax, b
end;
```

## Fonctionnalités Exclusives ou Étendues

### 1. Fonctionnalités Uniques à FreePascal

#### Support Multi-Plateforme Natif

FreePascal brille par son support natif de nombreuses plateformes :

```pascal
program PlatformDemo;  
begin
  {$IFDEF WINDOWS}
    WriteLn('Compilation pour Windows');
    WriteLn('Séparateur de chemin : \');
  {$ENDIF}

  {$IFDEF UNIX}
    WriteLn('Compilation pour Unix/Linux');
    WriteLn('Séparateur de chemin : /');
    {$IFDEF LINUX}
      WriteLn('Spécifiquement Linux');
    {$ENDIF}
    {$IFDEF FREEBSD}
      WriteLn('Spécifiquement FreeBSD');
    {$ENDIF}
  {$ENDIF}

  {$IFDEF DARWIN}
    WriteLn('Compilation pour macOS');
  {$ENDIF}

  {$IFDEF ANDROID}
    WriteLn('Compilation pour Android');
  {$ENDIF}

  {$IFDEF CPUARM}
    WriteLn('Compilation pour architecture ARM (Raspberry Pi, etc.)');
  {$ENDIF}
end.
```

#### Bitpacked Records et Arrays

FreePascal permet un contrôle très fin de la mémoire avec les structures bitpacked :

```pascal
type
  // Structure compactée au niveau du bit
  TBitFlags = bitpacked record
    Flag1: Boolean;      // 1 bit
    Flag2: Boolean;      // 1 bit
    Priority: 0..7;      // 3 bits
    Category: 0..3;      // 2 bits
    Reserved: Boolean;   // 1 bit
  end;  // Total : 8 bits = 1 octet exactement

  // Tableau bitpacked
  TBitArray = bitpacked array[0..31] of Boolean;  // 32 bits = 4 octets

var
  Flags: TBitFlags;
  Bits: TBitArray;
begin
  WriteLn('Taille de TBitFlags : ', SizeOf(TBitFlags), ' octet(s)');
  WriteLn('Taille de TBitArray : ', SizeOf(TBitArray), ' octet(s)');

  // Utilisation normale malgré le compactage
  Flags.Priority := 5;
  Flags.Flag1 := True;
  Bits[15] := True;
end;
```

#### Helpers Étendus

FreePascal étend le concept de helpers au-delà de ce que propose Delphi :

```pascal
{$mode objfpc}{$H+}
{$modeswitch typehelpers}  // Active les helpers pour types simples

type
  // Helper pour type simple (pas disponible en Delphi)
  TIntegerHelper = type helper for Integer
    function ToBinary: string;
    function IsPrime: Boolean;
    function ToRoman: string;
  end;

  // Helper pour enregistrement
  TPointHelper = record helper for TPoint
    procedure Offset(DX, DY: Integer);
    function Distance(Other: TPoint): Double;
  end;

implementation

function TIntegerHelper.ToBinary: string;  
var
  I: Integer;
begin
  Result := '';
  for I := 31 downto 0 do
    if (Self and (1 shl I)) <> 0 then
      Result := Result + '1'
    else if Result <> '' then
      Result := Result + '0';
  if Result = '' then Result := '0';
end;

function TIntegerHelper.IsPrime: Boolean;  
var
  I: Integer;
begin
  if Self <= 1 then Exit(False);
  if Self <= 3 then Exit(True);
  if (Self mod 2 = 0) or (Self mod 3 = 0) then Exit(False);
  I := 5;
  while I * I <= Self do
  begin
    if (Self mod I = 0) or (Self mod (I + 2) = 0) then
      Exit(False);
    Inc(I, 6);
  end;
  Result := True;
end;

// Utilisation naturelle
var
  N: Integer;
begin
  N := 42;
  WriteLn(N.ToBinary);     // Affiche : 101010
  WriteLn(17.IsPrime);     // Affiche : TRUE
end;
```

### 2. Gestion de la Mémoire

#### Différences dans les Chaînes

```pascal
// FreePascal - Contrôle précis du type de chaîne
{$H+}  // Chaînes longues (AnsiString) par défaut
{$H-}  // Chaînes courtes (ShortString) par défaut

type
  // FreePascal offre plus de types de chaînes
  TName = String[50];           // ShortString limité à 50 caractères
  TDescription = AnsiString;    // Chaîne ANSI dynamique
  TUnicodeText = UnicodeString; // Chaîne Unicode
  TUtf8Text = UTF8String;       // Chaîne UTF-8
  TRawData = RawByteString;     // Chaîne sans encodage spécifique

// Delphi moderne utilise principalement String (=UnicodeString)
```

#### Gestion des Interfaces

```pascal
// FreePascal - Support COM et CORBA
type
  // Interface style COM (compatible Windows)
  {$INTERFACES COM}
  IMyInterface = interface
    ['{12345678-1234-1234-1234-123456789012}']  // GUID obligatoire
    procedure DoSomething;
  end;

  // Interface style CORBA (multi-plateforme)
  {$INTERFACES CORBA}
  IMyCorbaInterface = interface
    ['IMyCorbaInterface']  // Nom simple, pas de GUID
    procedure DoSomething;
  end;

// Delphi utilise principalement COM
```

## Optimisations et Performances

### Optimisations Spécifiques FreePascal

FreePascal offre des directives d'optimisation fines :

```pascal
{$OPTIMIZATION ON}
{$OPTIMIZATION LEVEL3}        // Niveau d'optimisation maximal
{$OPTIMIZATION REGVAR}         // Variables dans registres
{$OPTIMIZATION PEEPHOLE}       // Optimisation peephole
{$OPTIMIZATION CSE}            // Common Subexpression Elimination
{$OPTIMIZATION ASMCSE}         // CSE pour assembleur
{$OPTIMIZATION LOOPUNROLL}     // Déroulement de boucles

procedure OptimizedLoop;  
var
  I: Integer;
  Sum: Int64;
begin
  Sum := 0;
  // Cette boucle sera optimisée par le compilateur
  for I := 1 to 1000000 do
    Inc(Sum, I);
  WriteLn('Somme : ', Sum);
end;
```

### Inlining Plus Flexible

```pascal
// FreePascal permet un contrôle plus fin de l'inlining
function FastAdd(A, B: Integer): Integer; inline;  
begin
  Result := A + B;
end;

// Forcer ou empêcher l'inlining
{$INLINE ON}   // Active l'inlining
{$INLINE OFF}  // Désactive l'inlining
{$INLINE AUTO} // Laisse le compilateur décider

// Macro inline (FreePascal uniquement)
{$MACRO ON}
{$DEFINE SQUARE(x) := (x)*(x)}

var
  Value: Integer;
begin
  Value := SQUARE(5);  // Remplacé par (5)*(5) à la compilation
end;
```

## Bibliothèques et Écosystème

### RTL (Run-Time Library) Étendue

FreePascal offre une RTL plus riche pour le multi-plateforme :

```pascal
uses
  // Unités communes
  SysUtils, Classes, Math,

  // Unités spécifiques FreePascal
  FileUtil,     // Utilitaires fichiers portables
  LazFileUtils, // Version Lazarus encore plus portable
  URIParser,    // Parsing d'URI
  Process,      // Exécution de processus portable
  Pipes,        // Pipes nommés portables

  // Unités Unix/Linux
  {$IFDEF UNIX}
  BaseUnix, Unix, Users,
  {$ENDIF}

  // Unités Windows
  {$IFDEF WINDOWS}
  Windows, Registry, ShellAPI;
  {$ENDIF}
```

### Packages et Gestion des Dépendances

```pascal
// FreePascal/Lazarus utilise des fichiers .lpk pour les packages
// Delphi utilise des fichiers .dpk

// Package Lazarus typique (fichier .lpk)
<?xml version="1.0" encoding="UTF-8"?>
<CONFIG>
  <Package Version="4">
    <Name Value="MyPackage"/>
    <Type Value="RunAndDesignTime"/>
    <CompilerOptions>
      <SearchPaths>
        <UnitOutputDirectory Value="lib/$(TargetCPU)-$(TargetOS)"/>
      </SearchPaths>
    </CompilerOptions>
    <Files Count="1">
      <Item1>
        <Filename Value="myunit.pas"/>
        <UnitName Value="MyUnit"/>
      </Item1>
    </Files>
    <RequiredPkgs Count="1">
      <Item1>
        <PackageName Value="LCL"/>
      </Item1>
    </RequiredPkgs>
  </Package>
</CONFIG>
```

## Directives de Compilation Avancées

### Directives Exclusives FreePascal

```pascal
// Vérifications à la compilation
{$ASSERTIONS ON}          // Active les assertions
{$RANGECHECKS ON}        // Vérifie les débordements
{$OVERFLOWCHECKS ON}     // Vérifie les overflows
{$OBJECTCHECKS ON}       // Vérifie les appels de méthodes
{$IOCHECKS ON}           // Vérifie les E/S

// Directives de mémoire
{$MEMORY 262144,1048576} // Taille heap min/max
{$STACKSIZE 65536}       // Taille de pile
{$MINSTACKSIZE 4096}     // Pile minimale

// Directives de linkage
{$LINKLIB c}             // Lie avec libc
{$LINKFRAMEWORK CoreFoundation} // macOS frameworks

// Smartlinking (réduction taille exécutable)
{$SMARTLINK ON}          // Élimine code non utilisé

// Informations de débogage
{$DEBUGINFO ON}          // Génère infos debug
{$LINEINFO ON}           // Numéros de ligne
{$ASSERTIONS ON}         // Active assertions
```

## Migration de Code Entre FreePascal et Delphi

### Stratégies de Portabilité

Pour écrire du code compatible avec les deux compilateurs :

```pascal
// Utiliser les directives conditionnelles
{$IFDEF FPC}
  // Code spécifique FreePascal
  {$mode delphi}  // Active le mode Delphi pour compatibilité
  {$H+}           // Chaînes longues
{$ENDIF}

{$IFDEF DCC}  // Ou {$IFNDEF FPC}
  // Code spécifique Delphi
{$ENDIF}

// Définir des macros pour les différences
{$IFDEF FPC}
  {$DEFINE USE_SPECIALIZE}  // FreePascal nécessite 'specialize'
{$ELSE}
  {$UNDEF USE_SPECIALIZE}   // Delphi n'en a pas besoin
{$ENDIF}

type
  {$IFDEF USE_SPECIALIZE}
  TIntList = specialize TList<Integer>;
  {$ELSE}
  TIntList = TList<Integer>;
  {$ENDIF}
```

### Table de Correspondance Rapide

| Fonctionnalité | FreePascal | Delphi |
|---------------|------------|---------|
| Généricité | `generic`/`specialize` | Syntaxe directe `<T>` |
| Chaînes par défaut | AnsiString ({$H+}) | UnicodeString |
| Assembleur inline | AT&T ou Intel | Intel uniquement |
| Helpers | Types simples + classes | Classes principalement |
| Interfaces | COM + CORBA | COM principalement |
| Plateformes | Multi-plateforme natif | Windows + mobile |
| Licence | Open Source (GPL/LGPL) | Commercial |
| IDE | Lazarus (gratuit) | RAD Studio (payant) |

## Conseils Pratiques pour Débutants

### Quand Choisir FreePascal

1. **Projets Open Source** : La licence libre facilite la distribution
2. **Applications Multi-Plateformes** : Support natif Windows/Linux/macOS
3. **Systèmes Embarqués** : Support ARM, Raspberry Pi, microcontrôleurs
4. **Budget Limité** : Totalement gratuit avec IDE complet
5. **Apprentissage** : Excellente plateforme pédagogique

### Quand Considérer Delphi

1. **Développement Mobile** : iOS/Android natif
2. **Support Commercial** : Besoin de support entreprise
3. **Composants Tiers** : Vaste marché de composants commerciaux
4. **RAD Visuel** : Outils de design plus sophistiqués
5. **Base de Code Existante** : Si vous avez déjà du code Delphi

### Bonnes Pratiques pour Code Portable

```pascal
unit PortableUnit;

{$IFDEF FPC}
  {$mode delphi}      // Maximise la compatibilité
  {$H+}               // Chaînes longues
{$ENDIF}

interface

uses
  SysUtils, Classes   // Unités standard portables
  {$IFDEF FPC}
  , FileUtil          // Unité spécifique FPC
  {$ELSE}
  , System.IOUtils    // Équivalent Delphi
  {$ENDIF};

type
  // Utiliser des types portables
  TFileName = string;  // Pas WideString ou AnsiString
  TFileSize = Int64;   // Pas Integer pour grandes tailles

// Définir des fonctions wrapper pour les différences
function GetApplicationPath: string;  
function FileExists(const FileName: string): Boolean;

implementation

function GetApplicationPath: string;  
begin
  {$IFDEF FPC}
  Result := ExtractFilePath(ParamStr(0));
  {$ELSE}
  Result := ExtractFilePath(Application.ExeName);
  {$ENDIF}
end;

function FileExists(const FileName: string): Boolean;  
begin
  {$IFDEF FPC}
  Result := FileUtil.FileExists(FileName);
  {$ELSE}
  Result := System.SysUtils.FileExists(FileName);
  {$ENDIF}
end;

end.
```

## Conclusion

FreePascal et Delphi sont comme deux dialectes d'une même langue. FreePascal privilégie l'ouverture, la portabilité et la liberté, tandis que Delphi se concentre sur l'intégration commerciale et les outils RAD sophistiqués.

Pour un débutant, commencer avec FreePascal offre plusieurs avantages :
- Gratuité totale
- Excellente documentation communautaire
- Apprentissage des concepts fondamentaux sans fioritures
- Possibilité de migrer vers Delphi plus tard si nécessaire

La clé est de comprendre que malgré leurs différences, les concepts fondamentaux restent les mêmes. Un développeur maîtrisant l'un peut facilement s'adapter à l'autre. FreePascal offre même un mode Delphi qui facilite grandement la transition dans les deux sens.

L'important est de choisir l'outil qui correspond le mieux à vos besoins actuels tout en gardant à l'esprit que le code bien écrit peut être rendu portable avec un effort minimal.

⏭️ [Généricité avancée et spécialisation](/03-langage-object-pascal-avance/02-genericite-avancee-specialisation.md)
