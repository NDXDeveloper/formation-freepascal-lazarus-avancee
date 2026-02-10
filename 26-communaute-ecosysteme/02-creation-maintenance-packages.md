🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 26.2 Création et maintenance de packages

## Introduction

La création de packages est l'une des meilleures façons de contribuer à l'écosystème FreePascal/Lazarus. Un package bien conçu et maintenu peut bénéficier à des milliers de développeurs dans le monde entier. Ce chapitre vous guidera pas à pas dans la création, la distribution et la maintenance de packages de qualité professionnelle.

### Qu'est-ce qu'un package ?

Un **package** est un ensemble cohérent de code réutilisable, organisé et distribué comme une unité. Dans l'écosystème Lazarus, un package peut contenir :

- **Composants visuels** : Boutons, grilles, graphiques personnalisés
- **Composants non-visuels** : Connexions base de données, timers, services
- **Bibliothèques de code** : Fonctions utilitaires, algorithmes, helpers
- **Outils IDE** : Extensions de l'environnement de développement
- **Resources** : Images, icônes, templates

### Pourquoi créer un package ?

**Avantages pour la communauté :**
- Partager votre travail et aider d'autres développeurs
- Éviter la duplication d'efforts
- Standardiser des solutions à des problèmes communs
- Enrichir l'écosystème FreePascal/Lazarus

**Avantages personnels :**
- Améliorer vos compétences en développement
- Obtenir des retours et contributions de la communauté
- Développer votre réputation professionnelle
- Portfolio de projets open source
- Apprendre les meilleures pratiques

**Avantages techniques :**
- Réutilisabilité du code entre projets
- Gestion centralisée des mises à jour
- Facilité de distribution
- Installation simplifiée pour les utilisateurs
- Intégration avec l'IDE

## Types de packages

### Packages runtime (exécution)

Les packages runtime contiennent du code qui sera utilisé dans les applications finales :

```pascal
// Exemple : Package de cryptographie
unit MyCryptoUnit;

interface

function EncryptData(const AData: string; AKey: string): string;  
function DecryptData(const AData: string; AKey: string): string;

implementation
// ... implémentation
end.
```

**Caractéristiques :**
- Ne dépendent pas de l'IDE
- Peuvent être utilisés dans n'importe quel projet
- Compilés dans l'application finale
- Pas d'interaction avec l'environnement de développement

### Packages designtime (conception)

Les packages designtime étendent l'IDE Lazarus lui-même :

```pascal
// Exemple : Editeur de propriété personnalisé
unit MyPropertyEditor;

interface

uses
  Classes, PropEdits;

type
  TMyCustomEditor = class(TPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

implementation
// ... implémentation
end.
```

**Caractéristiques :**
- S'exécutent dans l'IDE
- Ajoutent des fonctionnalités au designer
- Éditeurs de propriétés personnalisés
- Actions et menus IDE
- Ne sont pas compilés dans l'application finale

### Packages hybrides (runtime + designtime)

La plupart des packages contiennent les deux aspects :

```
MyPackage/
├── runtime/          # Code pour l'application
│   └── mycomponent.pas
├── designtime/       # Code pour l'IDE
│   └── mycomponent_reg.pas
└── mypackage.lpk     # Fichier package
```

## Créer votre premier package

### Étape 1 : Nouveau package dans Lazarus

**Procédure dans l'IDE :**

1. Ouvrir Lazarus
2. Menu : **Package → New Package**
3. Boîte de dialogue "Create new package" :
   - **Package name** : MonPremierPackage
   - **Package type** : Package (.lpk)
   - **Location** : Choisir un répertoire

4. Cliquer **OK**

Lazarus crée un fichier `.lpk` (Lazarus Package) qui est le fichier de description du package.

### Étape 2 : Structure du fichier .lpk

Le fichier `.lpk` est un fichier XML qui décrit le package :

```xml
<?xml version="1.0" encoding="UTF-8"?>
<CONFIG>
  <Package Version="5">
    <Name Value="MonPremierPackage"/>
    <Type Value="RunAndDesignTime"/>
    <Author Value="Votre Nom"/>
    <CompilerOptions>
      <Version Value="11"/>
      <SearchPaths>
        <UnitOutputDirectory Value="lib/$(TargetCPU)-$(TargetOS)"/>
      </SearchPaths>
    </CompilerOptions>
    <Description Value="Description de mon package"/>
    <License Value="LGPL with modification, or GPL"/>
    <Version Major="1" Minor="0" Release="0"/>
    <Files Count="0"/>
    <RequiredPkgs Count="1">
      <Item1>
        <PackageName Value="LCL"/>
      </Item1>
    </RequiredPkgs>
    <UsageOptions>
      <UnitPath Value="$(PkgOutDir)"/>
    </UsageOptions>
    <PublishOptions>
      <Version Value="2"/>
    </PublishOptions>
  </Package>
</CONFIG>
```

**Éléments importants :**
- **Name** : Nom unique du package
- **Type** : RunAndDesignTime, RunTimeOnly, ou DesignTimeOnly
- **Author** : Votre nom
- **CompilerOptions** : Options de compilation
- **Files** : Liste des unités incluses
- **RequiredPkgs** : Dépendances

### Étape 3 : Ajouter une unité simple

Créons une unité avec des fonctions utilitaires :

```pascal
unit StringHelpers;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  { TStringHelper - Extensions pour les chaînes }
  TStringHelper = class
  public
    class function Reverse(const AText: string): string;
    class function CountWords(const AText: string): Integer;
    class function Capitalize(const AText: string): string;
    class function RemoveSpaces(const AText: string): string;
  end;

implementation

uses
  StrUtils;

class function TStringHelper.Reverse(const AText: string): string;  
var
  i: Integer;
begin
  Result := '';
  for i := Length(AText) downto 1 do
    Result := Result + AText[i];
end;

class function TStringHelper.CountWords(const AText: string): Integer;  
var
  InWord: Boolean;
  i: Integer;
begin
  Result := 0;
  InWord := False;

  for i := 1 to Length(AText) do
  begin
    if AText[i] in [' ', #9, #10, #13] then
      InWord := False
    else if not InWord then
    begin
      InWord := True;
      Inc(Result);
    end;
  end;
end;

class function TStringHelper.Capitalize(const AText: string): string;  
begin
  if Length(AText) = 0 then
    Exit('');
  Result := UpperCase(AText[1]) + LowerCase(Copy(AText, 2, Length(AText)));
end;

class function TStringHelper.RemoveSpaces(const AText: string): string;  
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(AText) do
    if AText[i] <> ' ' then
      Result := Result + AText[i];
end;

end.
```

**Ajouter l'unité au package :**

1. Dans l'éditeur de package : **Add → New file**
2. Ou **Add → Existing file** si le fichier existe
3. Sauvegarder le package (Ctrl+S)

### Étape 4 : Compiler le package

Dans l'éditeur de package :

1. Cliquer sur **Compile**
2. Vérifier qu'il n'y a pas d'erreurs
3. Les fichiers compilés sont créés dans `lib/$(TargetCPU)-$(TargetOS)/`

**Structure après compilation :**

```
MonPremierPackage/
├── stringhelpers.pas
├── monpremierpackage.lpk
└── lib/
    └── x86_64-linux/
        ├── stringhelpers.ppu
        └── monpremierpackage.compiled
```

### Étape 5 : Utiliser le package

**Dans un projet :**

1. Créer un nouveau projet
2. Menu : **Project → Project Inspector**
3. **Add → New Requirement**
4. Sélectionner **MonPremierPackage**
5. Utiliser les unités dans votre code :

```pascal
program TestPackage;

uses
  StringHelpers;

var
  Text: string;
begin
  Text := 'hello world';
  WriteLn('Original: ', Text);
  WriteLn('Reversed: ', TStringHelper.Reverse(Text));
  WriteLn('Words: ', TStringHelper.CountWords(Text));
  WriteLn('Capitalized: ', TStringHelper.Capitalize(Text));
end.
```

## Créer un composant visuel

### Composant simple : TCustomLabel amélioré

Créons un composant Label avec des fonctionnalités supplémentaires :

```pascal
unit EnhancedLabel;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, Graphics, LCLType;

type
  { TEnhancedLabel - Label avec effet d'ombre et rotation }
  TEnhancedLabel = class(TCustomLabel)
  private
    FShadowColor: TColor;
    FShadowOffset: Integer;
    FShowShadow: Boolean;
    FRotationAngle: Integer;

    procedure SetShadowColor(AValue: TColor);
    procedure SetShadowOffset(AValue: Integer);
    procedure SetShowShadow(AValue: Boolean);
    procedure SetRotationAngle(AValue: Integer);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    // Propriétés héritées rendues published
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property Caption;
    property Color;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentFont;
    property Visible;
    property OnClick;
    property OnDblClick;

    // Nouvelles propriétés
    property ShadowColor: TColor read FShadowColor write SetShadowColor default clGray;
    property ShadowOffset: Integer read FShadowOffset write SetShadowOffset default 2;
    property ShowShadow: Boolean read FShowShadow write SetShowShadow default True;
    property RotationAngle: Integer read FRotationAngle write SetRotationAngle default 0;
  end;

implementation

uses
  LCLIntf;

constructor TEnhancedLabel.Create(AOwner: TComponent);  
begin
  inherited Create(AOwner);
  FShadowColor := clGray;
  FShadowOffset := 2;
  FShowShadow := True;
  FRotationAngle := 0;
  Width := 100;
  Height := 20;
end;

procedure TEnhancedLabel.SetShadowColor(AValue: TColor);  
begin
  if FShadowColor = AValue then Exit;
  FShadowColor := AValue;
  Invalidate;
end;

procedure TEnhancedLabel.SetShadowOffset(AValue: Integer);  
begin
  if FShadowOffset = AValue then Exit;
  FShadowOffset := AValue;
  Invalidate;
end;

procedure TEnhancedLabel.SetShowShadow(AValue: Boolean);  
begin
  if FShowShadow = AValue then Exit;
  FShowShadow := AValue;
  Invalidate;
end;

procedure TEnhancedLabel.SetRotationAngle(AValue: Integer);  
begin
  if FRotationAngle = AValue then Exit;
  FRotationAngle := AValue mod 360;
  Invalidate;
end;

procedure TEnhancedLabel.Paint;  
var
  TextStyle: TTextStyle;
  R: TRect;
  OldFont: TFont;
begin
  if not Visible then Exit;

  Canvas.Brush.Color := Color;
  Canvas.FillRect(ClientRect);

  R := ClientRect;
  TextStyle := Canvas.TextStyle;
  TextStyle.Alignment := Alignment;
  TextStyle.Layout := tlCenter;
  TextStyle.Opaque := False;
  TextStyle.Clipping := True;

  // Dessiner l'ombre si activée
  if FShowShadow and (FShadowOffset > 0) then
  begin
    OldFont := TFont.Create;
    try
      OldFont.Assign(Canvas.Font);
      Canvas.Font.Color := FShadowColor;
      Canvas.TextRect(
        Rect(R.Left + FShadowOffset, R.Top + FShadowOffset,
             R.Right + FShadowOffset, R.Bottom + FShadowOffset),
        0, 0, Caption, TextStyle
      );
      Canvas.Font.Assign(OldFont);
    finally
      OldFont.Free;
    end;
  end;

  // Dessiner le texte principal
  Canvas.Font.Color := Font.Color;
  Canvas.TextRect(R, 0, 0, Caption, TextStyle);
end;

end.
```

### Unité d'enregistrement (Registration)

Pour que le composant apparaisse dans la palette de Lazarus :

```pascal
unit EnhancedLabel_Reg;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, EnhancedLabel;

procedure Register;

implementation

procedure Register;  
begin
  // Enregistrer le composant dans la palette
  RegisterComponents('Custom', [TEnhancedLabel]);
end;

initialization
  // Enregistrer l'icône du composant (optionnel)
  {$I enhancedlabel_icon.lrs}

end.
```

### Créer une icône pour le composant

**Étape 1 : Créer l'image**
- Créer une image 24x24 pixels (PNG ou BMP)
- Nommer : `enhancedlabel_icon.png`

**Étape 2 : Convertir en ressource**

```bash
# Utiliser lazres (outil Lazarus)
lazres enhancedlabel_icon.lrs enhancedlabel_icon.png
```

**Étape 3 : Utiliser dans l'enregistrement**

```pascal
procedure Register;  
begin
  RegisterComponents('Custom', [TEnhancedLabel]);
end;

initialization
  {$I enhancedlabel_icon.lrs}
end.
```

### Ajouter au package

1. Ajouter `enhancedlabel.pas` au package (Runtime)
2. Ajouter `enhancedlabel_reg.pas` au package (Designtime)
3. Compiler le package
4. **Install** le package dans l'IDE
5. Redémarrer Lazarus

Le composant apparaît maintenant dans la palette **Custom** !

## Structure recommandée d'un package

### Organisation des fichiers

```
MonPackage/
├── README.md                 # Documentation principale
├── LICENSE.txt               # Licence
├── CHANGELOG.md              # Historique des versions
├── monpackage.lpk            # Fichier package
├── src/                      # Code source
│   ├── runtime/              # Composants runtime
│   │   ├── mycomponent.pas
│   │   └── myhelpers.pas
│   ├── designtime/           # Enregistrement et éditeurs
│   │   ├── mycomponent_reg.pas
│   │   └── mycomponent_editor.pas
│   └── resources/            # Ressources
│       ├── icons/
│       └── templates/
├── docs/                     # Documentation
│   ├── manual.md
│   └── examples.md
├── examples/                 # Exemples d'utilisation
│   ├── simple/
│   │   └── simple_demo.lpi
│   └── advanced/
│       └── advanced_demo.lpi
├── tests/                    # Tests unitaires
│   └── test_mycomponent.pas
└── lib/                      # Fichiers compilés (généré)
    ├── x86_64-linux/
    └── x86_64-win64/
```

### Fichier README.md

````markdown
# MonPackage

## Description
Brève description de votre package et de ses fonctionnalités principales.

## Fonctionnalités

- Fonctionnalité 1
- Fonctionnalité 2
- Fonctionnalité 3

## Installation

### Via Online Package Manager (OPM)

1. Ouvrir Lazarus
2. Package → Online Package Manager
3. Rechercher "MonPackage"
4. Cliquer Install

### Installation manuelle

1. Télécharger le package
2. Ouvrir monpackage.lpk dans Lazarus
3. Compiler
4. Install
5. Redémarrer Lazarus

## Utilisation

```pascal
uses
  MyComponent;

procedure TForm1.FormCreate(Sender: TObject);  
var
  MyComp: TMyComponent;
begin
  MyComp := TMyComponent.Create(Self);
  MyComp.Parent := Self;
  MyComp.Property1 := 'Value';
end;
```

## Documentation complète

Voir le dossier [docs/](docs/) pour la documentation détaillée.

## Exemples

Des exemples complets sont disponibles dans le dossier [examples/](examples/).

## Licence

LGPL avec modification, voir [LICENSE.txt](LICENSE.txt)

## Contributions

Les contributions sont les bienvenues ! Voir [CONTRIBUTING.md](CONTRIBUTING.md)

## Support

- Issues GitHub : https://github.com/username/monpackage/issues
- Forum Lazarus : Section appropriate
- Email : votre@email.com
````

## Gestion des dépendances

### Déclarer des dépendances

Un package peut dépendre d'autres packages :

**Dans l'éditeur de package :**

1. Section **Required Packages**
2. **Add** → Choisir le package
3. Types de dépendances :
   - **LCL** : Presque toujours nécessaire pour composants visuels
   - **FCL** : Free Component Library
   - **LazUtils** : Utilitaires Lazarus
   - Autres packages tiers

**Dans le .lpk :**

```xml
<RequiredPkgs Count="3">
  <Item1>
    <PackageName Value="LCL"/>
  </Item1>
  <Item2>
    <PackageName Value="LazUtils"/>
  </Item2>
  <Item3>
    <PackageName Value="SynEdit"/>
    <MinVersion Major="1" Minor="0" Valid="True"/>
  </Item3>
</RequiredPkgs>
```

### Versions minimales

Spécifier les versions minimales des dépendances :

```xml
<Item>
  <PackageName Value="MonAutrePackage"/>
  <MinVersion Major="2" Minor="5" Release="1" Valid="True"/>
</Item>
```

### Dépendances optionnelles

Pour les fonctionnalités optionnelles, utiliser la compilation conditionnelle :

```pascal
unit MyUnit;

interface

uses
  Classes, SysUtils
  {$IFDEF USE_SYNEDI}
  , SynEdit, SynHighlighterPas
  {$ENDIF};

type
  TMyComponent = class(TComponent)
  private
    {$IFDEF USE_SYNEDI}
    FSynEdit: TSynEdit;
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

constructor TMyComponent.Create(AOwner: TComponent);  
begin
  inherited Create(AOwner);
  {$IFDEF USE_SYNEDI}
  FSynEdit := TSynEdit.Create(Self);
  {$ENDIF}
end;

end.
```

## Compatibilité multi-plateforme

### Compilation conditionnelle

```pascal
unit PlatformSpecific;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils
  {$IFDEF WINDOWS}
  , Windows, Registry
  {$ENDIF}
  {$IFDEF UNIX}
  , BaseUnix, Unix
  {$ENDIF};

function GetUserConfigDir: string;  
function GetTempDir: string;

implementation

function GetUserConfigDir: string;  
begin
  {$IFDEF WINDOWS}
  Result := GetEnvironmentVariable('APPDATA');
  {$ENDIF}

  {$IFDEF UNIX}
  Result := GetEnvironmentVariable('HOME') + '/.config';
  {$ENDIF}

  {$IFDEF DARWIN}
  Result := GetEnvironmentVariable('HOME') + '/Library/Application Support';
  {$ENDIF}
end;

function GetTempDir: string;  
begin
  {$IFDEF WINDOWS}
  Result := GetEnvironmentVariable('TEMP');
  {$ENDIF}

  {$IFDEF UNIX}
  Result := '/tmp';
  {$ENDIF}
end;

end.
```

### Widgetsets différents

Pour les composants LCL, gérer les différences entre widgetsets :

```pascal
procedure TMyComponent.Paint;  
begin
  {$IFDEF LCLWin32}
  // Code spécifique Windows
  PaintWindows;
  {$ENDIF}

  {$IFDEF LCLGtk2}
  // Code spécifique GTK2
  PaintGTK;
  {$ENDIF}

  {$IFDEF LCLQt}
  // Code spécifique Qt
  PaintQt;
  {$ENDIF}

  // Code commun
  PaintCommon;
end;
```

### Tests multi-plateformes

Tester sur toutes les plateformes cibles :

```bash
# Linux
lazbuild --build-mode=Release mypackage.lpk

# Windows (cross-compilation depuis Linux)
lazbuild --build-mode=Release --os=win64 --cpu=x86_64 mypackage.lpk

# macOS
lazbuild --build-mode=Release --os=darwin --cpu=x86_64 mypackage.lpk
```

## Documentation du package

### Documentation inline (PasDoc)

```pascal
{**
  TMyComponent est un composant qui...

  @bold(Exemple d'utilisation :)
  @longcode(#
  var
    MyComp: TMyComponent;
  begin
    MyComp := TMyComponent.Create(Self);
    MyComp.Property1 := 'Value';
    MyComp.DoSomething;
  end;
  #)

  @seealso(TRelatedComponent)
  @author(Votre Nom)
}
TMyComponent = class(TComponent)  
private
  FProperty1: string;  //<! Description courte de la propriété

  {** Setter pour Property1 avec validation }
  procedure SetProperty1(const AValue: string);

public
  {**
    Crée une nouvelle instance du composant.
    @param(AOwner Propriétaire du composant)
  }
  constructor Create(AOwner: TComponent); override;

  {**
    Détruit l'instance et libère les ressources.
    @raises(Exception Si des ressources ne peuvent être libérées)
  }
  destructor Destroy; override;

  {**
    Effectue une action importante.
    @param(AParam Paramètre de l'action)
    @returns(True si succès, False sinon)
  }
  function DoSomething(AParam: Integer): Boolean;

published
  {** Propriété principale du composant }
  property Property1: string read FProperty1 write SetProperty1;
end;
```

### Générer la documentation

```bash
# Installer PasDoc
sudo apt install pasdoc  # Linux
# ou télécharger depuis https://pasdoc.github.io/

# Générer la documentation HTML
pasdoc \
  --format html \
  --output docs/html \
  --title "MonPackage Documentation" \
  --introduction docs/intro.txt \
  src/*.pas
```

### Guide utilisateur

Créer un guide utilisateur complet dans `docs/manual.md` :

````markdown
# Guide utilisateur MonPackage

## Installation

[Voir README.md](../README.md#installation)

## Premiers pas

### Créer votre premier composant

1. Ajouter MonPackage aux dépendances du projet
2. Placer un TMyComponent sur votre formulaire
3. Configurer les propriétés...

### Configuration de base

```pascal
MyComponent.Property1 := 'valeur';  
MyComponent.Property2 := 42;  
MyComponent.Active := True;
```

## Référence API

### TMyComponent

Description détaillée...

#### Propriétés

- **Property1** : Description...
- **Property2** : Description...

#### Méthodes

- **DoSomething** : Description...
- **Initialize** : Description...

## Exemples avancés

### Exemple 1 : Utilisation basique

[Code et explications]

### Exemple 2 : Configuration avancée

[Code et explications]

## FAQ

### Comment faire X ?

Réponse...

### Que faire si Y ne fonctionne pas ?

Solution...
````

## Tests du package

### Créer des tests unitaires

```pascal
unit TestMyComponent;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, MyComponent;

type
  TTestMyComponent = class(TTestCase)
  private
    FComponent: TMyComponent;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCreate;
    procedure TestProperty1;
    procedure TestDoSomething;
    procedure TestEdgeCases;
  end;

implementation

procedure TTestMyComponent.SetUp;  
begin
  FComponent := TMyComponent.Create(nil);
end;

procedure TTestMyComponent.TearDown;  
begin
  FComponent.Free;
end;

procedure TTestMyComponent.TestCreate;  
begin
  AssertNotNull('Component should be created', FComponent);
  AssertEquals('Default value', '', FComponent.Property1);
end;

procedure TTestMyComponent.TestProperty1;  
begin
  FComponent.Property1 := 'Test';
  AssertEquals('Property1 should be set', 'Test', FComponent.Property1);
end;

procedure TTestMyComponent.TestDoSomething;  
var
  Result: Boolean;
begin
  Result := FComponent.DoSomething(10);
  AssertTrue('DoSomething should succeed', Result);
end;

procedure TTestMyComponent.TestEdgeCases;  
begin
  // Tester les cas limites
  FComponent.Property1 := '';
  AssertEquals('Empty string', '', FComponent.Property1);

  FComponent.Property1 := StringOfChar('A', 1000);
  AssertEquals('Long string', 1000, Length(FComponent.Property1));
end;

initialization
  RegisterTest(TTestMyComponent);

end.
```

### Projet de test

Créer un projet console pour exécuter les tests :

```pascal
program RunTests;

{$mode objfpc}{$H+}

uses
  Classes, consoletestrunner,
  TestMyComponent;

type
  TMyTestRunner = class(TTestRunner)
  protected
    // Personnalisation si nécessaire
  end;

var
  Application: TMyTestRunner;

begin
  Application := TMyTestRunner.Create(nil);
  Application.Initialize;
  Application.Title := 'MonPackage Tests';
  Application.Run;
  Application.Free;
end.
```

### Exécution automatique

```bash
#!/bin/bash
# test_package.sh

echo "Compilation des tests..."  
lazbuild tests/runtests.lpi

echo "Exécution des tests..."
./tests/runtests --all --format=plain

if [ $? -eq 0 ]; then
    echo "✓ Tous les tests ont réussi"
    exit 0
else
    echo "✗ Certains tests ont échoué"
    exit 1
fi
```

## Gestion des versions

### Schéma de versionnement

Utiliser le **Semantic Versioning** (SemVer) : `MAJOR.MINOR.PATCH`

- **MAJOR** : Changements incompatibles avec les versions précédentes
- **MINOR** : Nouvelles fonctionnalités rétrocompatibles
- **PATCH** : Corrections de bugs rétrocompatibles

**Exemples :**
- `1.0.0` : Version initiale stable
- `1.1.0` : Ajout de fonctionnalités
- `1.1.1` : Correction de bugs
- `2.0.0` : Changements majeurs incompatibles

### Fichier CHANGELOG.md

```markdown
# Changelog

Toutes les modifications notables de ce projet sont documentées ici.

Le format est basé sur [Keep a Changelog](https://keepachangelog.com/)  
et ce projet adhère au [Semantic Versioning](https://semver.org/).

## [Non publié]

### Ajouté
- Nouvelle fonctionnalité X

### Modifié
- Amélioration de la performance Y

### Corrigé
- Bug dans la méthode Z

## [1.2.0] - 2024-01-15

### Ajouté
- Nouvelle propriété `AutoSave`
- Support pour le format XML
- Éditeur de propriété personnalisé

### Modifié
- Optimisation du rendu (30% plus rapide)
- Mise à jour de la documentation

### Corrigé
- Correction du crash lors du redimensionnement
- Fuite mémoire dans TMyList

### Déprécié
- La méthode `OldMethod` sera supprimée en v2.0.0

## [1.1.0] - 2023-12-01

### Ajouté
- Support Linux/macOS
- Tests unitaires complets

## [1.0.0] - 2023-10-15

### Ajouté
- Version initiale
- Composant TMyComponent
- Documentation de base
```

### Tagging Git

```bash
# Créer un tag pour une version
git tag -a v1.2.0 -m "Version 1.2.0 - Support multi-plateforme"

# Pousser le tag
git push origin v1.2.0

# Lister tous les tags
git tag -l

# Checkout d'une version spécifique
git checkout v1.2.0
```

## Distribution du package

### Via GitHub/GitLab

**Structure recommandée :**

```
monpackage/
├── .gitignore
├── README.md
├── LICENSE.txt
├── CHANGELOG.md
├── CONTRIBUTING.md
├── monpackage.lpk
├── src/
├── docs/
├── examples/
└── tests/
```

**Fichier .gitignore :**

```gitignore
# Lazarus
*.lps
*.compiled
*.bak
*.or
*.ppu
*.o
*.rst
backup/  
lib/
*.res

# OS
.DS_Store
Thumbs.db
*~

# IDE
.idea/
*.user
```

### Via Online Package Manager (OPM)

Le **Online Package Manager** est le système de distribution officiel pour Lazarus.

**Étapes pour publier :**

1. **Préparer le package** :
   - Code testé et fonctionnel
   - Documentation complète
   - Fichier .lpk correct
   - Version taggée sur Git

2. **Créer un fichier JSON de description** :

```json
{
  "name": "monpackage",
  "display_name": "Mon Package Génial",
  "category": "Graphics",
  "license": "LGPL with modification",
  "version": "1.2.0",
  "description": "Description courte du package",
  "author": {
    "name": "Votre Nom",
    "email": "votre@email.com"
  },
  "lazarus_min_version": "2.2.0",
  "fpc_min_version": "3.2.2",
  "supported_widget_set": [
    {
      "name": "win32",
      "os": "win32"
    },
    {
      "name": "gtk2",
      "os": "linux"
    },
    {
      "name": "qt5",
      "os": "linux"
    }
  ],
  "package_type": "designtime and runtime",
  "home_page_url": "https://github.com/username/monpackage",
  "download_url": "https://github.com/username/monpackage/archive/refs/tags/v1.2.0.zip",
  "community_description": "Description détaillée avec fonctionnalités, captures d'écran, etc."
}
```

3. **Soumettre au dépôt OPM** :
   - Fork du dépôt : https://github.com/LazarusPackageManager/PackageList
   - Ajouter votre JSON dans le dossier approprié
   - Créer une Pull Request

4. **Après acceptation** :
   - Le package apparaît dans OPM
   - Les utilisateurs peuvent l'installer facilement

### Création d'un installateur

Pour une distribution hors OPM :

**Script d'installation Linux :**

```bash
#!/bin/bash
# install.sh

PACKAGE_NAME="monpackage"  
LAZARUS_DIR="$HOME/.lazarus"

echo "Installation de $PACKAGE_NAME..."

# Créer le répertoire si nécessaire
mkdir -p "$LAZARUS_DIR/packages"

# Copier les fichiers
cp -r . "$LAZARUS_DIR/packages/$PACKAGE_NAME"

echo "Fichiers copiés."  
echo ""  
echo "Pour terminer l'installation :"  
echo "1. Ouvrir Lazarus"  
echo "2. Package → Open Package File"  
echo "3. Naviguer vers $LAZARUS_DIR/packages/$PACKAGE_NAME/$PACKAGE_NAME.lpk"  
echo "4. Cliquer sur Compile puis Install"  
echo "5. Redémarrer Lazarus"
```

**Script PowerShell Windows :**

```powershell
# install.ps1

$PackageName = "monpackage"
$LazarusDir = "$env:APPDATA\lazarus"

Write-Host "Installation de $PackageName..."

# Créer le répertoire
New-Item -ItemType Directory -Force -Path "$LazarusDir\packages" | Out-Null

# Copier les fichiers
Copy-Item -Path ".\*" -Destination "$LazarusDir\packages\$PackageName" -Recurse -Force

Write-Host "Fichiers copiés."  
Write-Host ""  
Write-Host "Pour terminer l'installation :"  
Write-Host "1. Ouvrir Lazarus"  
Write-Host "2. Package → Open Package File"  
Write-Host "3. Naviguer vers $LazarusDir\packages\$PackageName\$PackageName.lpk"  
Write-Host "4. Cliquer sur Compile puis Install"  
Write-Host "5. Redémarrer Lazarus"
```

## Maintenance du package

### Gestion des issues

**Sur GitHub/GitLab :**

1. **Labels appropriés** :
   - `bug` : Dysfonctionnements
   - `enhancement` : Nouvelles fonctionnalités
   - `documentation` : Améliorations doc
   - `question` : Questions des utilisateurs
   - `good first issue` : Pour nouveaux contributeurs

2. **Templates d'issues** :

````markdown
<!-- .github/ISSUE_TEMPLATE/bug_report.md -->
---
name: Bug Report  
about: Signaler un bug  
title: '[BUG] '  
labels: bug
---

## Description du bug
Description claire et concise.

## Étapes pour reproduire
1. Aller à '...'
2. Cliquer sur '...'
3. Voir l'erreur

## Comportement attendu
Description du comportement attendu.

## Comportement actuel
Description de ce qui se passe réellement.

## Environnement
- OS: [Windows 10, Ubuntu 22.04, etc.]
- Lazarus version: [2.2.6]
- FreePascal version: [3.2.2]
- Package version: [1.2.0]

## Code pour reproduire
```pascal
// Code minimal reproduisant le problème
```

## Informations supplémentaires
Captures d'écran, messages d'erreur, etc.
````

### Répondre aux utilisateurs

**Bonnes pratiques :**

- Répondre rapidement (dans les 48h si possible)
- Être courtois et patient
- Demander des informations supplémentaires si nécessaire
- Fournir des solutions ou workarounds
- Expliquer les décisions techniques

**Exemple de réponse :**

````markdown
Merci pour ce rapport détaillé !

Le problème vient de la méthode `X` qui ne gère pas correctement  
le cas Y. Voici un workaround temporaire :

```pascal
// Workaround
if Condition then
  MyComponent.Method1
else
  MyComponent.Method2;
```

Je vais corriger cela dans la prochaine version (1.2.1).
````

### Mises à jour et patches

**Workflow de correction :**

1. **Reproduire le bug localement**
2. **Créer une branche** : `git checkout -b fix-issue-123`
3. **Écrire un test** qui échoue
4. **Corriger le bug**
5. **Vérifier** que le test passe
6. **Commit** : `git commit -m "Fix #123: Description"`
7. **Pousser** et créer une Pull Request
8. **Merger** après validation
9. **Créer une release** patch

### Release process

```bash
#!/bin/bash
# release.sh

VERSION=$1

if [ -z "$VERSION" ]; then
    echo "Usage: ./release.sh VERSION"
    echo "Example: ./release.sh 1.2.1"
    exit 1
fi

echo "Preparing release $VERSION..."

# 1. Mettre à jour CHANGELOG
echo "Please update CHANGELOG.md"  
read -p "Press enter when done..."

# 2. Mettre à jour version dans .lpk
# (nécessite édition manuelle ou script XML)

# 3. Commit
git add .  
git commit -m "Release v$VERSION"

# 4. Tag
git tag -a "v$VERSION" -m "Release v$VERSION"

# 5. Push
git push origin main  
git push origin "v$VERSION"

echo "✓ Release $VERSION created!"  
echo "Don't forget to:"  
echo "- Create GitHub release with changelog"  
echo "- Update OPM repository if applicable"  
echo "- Announce on forums/social media"
```

## Bonnes pratiques avancées

### Performance et optimisation

```pascal
// ✅ BON : Utiliser des constantes
const
  BufferSize = 4096;

// ❌ MAUVAIS : Réallouer à chaque fois
procedure ProcessData(const AData: array of Byte);  
var
  Buffer: array of Byte;
begin
  SetLength(Buffer, Length(AData));  // Allocation coûteuse
  // ...
end;

// ✅ BON : Réutiliser un buffer
type
  TMyComponent = class
  private
    FBuffer: array of Byte;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ProcessData(const AData: array of Byte);
  end;

constructor TMyComponent.Create(AOwner: TComponent);  
begin
  inherited;
  SetLength(FBuffer, BufferSize);  // Une seule fois
end;
```

### Gestion mémoire

```pascal
// ✅ BON : Libération explicite
procedure TMyComponent.DoSomething;  
var
  List: TStringList;
begin
  List := TStringList.Create;
  try
    // Utiliser List
    List.Add('item');
  finally
    List.Free;  // Toujours libérer
  end;
end;

// ✅ BON : Éviter les fuites avec les objets owned
constructor TMyComponent.Create(AOwner: TComponent);  
begin
  inherited Create(AOwner);
  FInternalList := TStringList.Create;  // Créé
end;

destructor TMyComponent.Destroy;  
begin
  FInternalList.Free;  // Libéré
  inherited Destroy;
end;
```

### Thread-safety

```pascal
uses
  SyncObjs;

type
  TThreadSafeComponent = class(TComponent)
  private
    FLock: TCriticalSection;
    FData: TStringList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure AddItem(const AItem: string);
    function GetItem(AIndex: Integer): string;
  end;

constructor TThreadSafeComponent.Create(AOwner: TComponent);  
begin
  inherited;
  FLock := TCriticalSection.Create;
  FData := TStringList.Create;
end;

destructor TThreadSafeComponent.Destroy;  
begin
  FData.Free;
  FLock.Free;
  inherited;
end;

procedure TThreadSafeComponent.AddItem(const AItem: string);  
begin
  FLock.Enter;
  try
    FData.Add(AItem);
  finally
    FLock.Leave;
  end;
end;
```

### Rétrocompatibilité

```pascal
// Marquer les méthodes obsolètes
type
  TMyComponent = class(TComponent)
  public
    // Ancienne méthode dépréciée
    procedure OldMethod; deprecated 'Use NewMethod instead';

    // Nouvelle méthode
    procedure NewMethod;
  end;

// Supporter anciennes propriétés
type
  TMyComponent = class(TComponent)
  private
    FNewProperty: string;

    // Ancienne propriété redirigée
    function GetOldProperty: string; deprecated;
    procedure SetOldProperty(const AValue: string); deprecated;
  published
    property OldProperty: string
      read GetOldProperty write SetOldProperty
      stored False;  // Ne pas sauvegarder
    property NewProperty: string
      read FNewProperty write FNewProperty;
  end;

function TMyComponent.GetOldProperty: string;  
begin
  Result := FNewProperty;  // Rediriger vers nouvelle propriété
end;
```

## Exemples de packages réels

### Package simple : StringUtils

```pascal
unit StringUtils;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils;

type
  TStringArray = array of string;

  { TStringUtils - Utilitaires pour chaînes }
  TStringUtils = class
  public
    class function IsEmpty(const S: string): Boolean;
    class function IsNumeric(const S: string): Boolean;
    class function IsAlpha(const S: string): Boolean;
    class function Contains(const S, SubStr: string): Boolean;
    class function StartsWith(const S, Prefix: string): Boolean;
    class function EndsWith(const S, Suffix: string): Boolean;
    class function Split(const S, Delimiter: string): TStringArray;
    class function Join(const Arr: array of string; const Separator: string): string;
  end;

implementation

uses
  StrUtils;

class function TStringUtils.IsEmpty(const S: string): Boolean;  
begin
  Result := Trim(S) = '';
end;

class function TStringUtils.IsNumeric(const S: string): Boolean;  
var
  i: Integer;
  HasDigit: Boolean;
begin
  Result := False;
  HasDigit := False;

  for i := 1 to Length(S) do
  begin
    if S[i] in ['0'..'9'] then
      HasDigit := True
    else if not (S[i] in ['-', '+', '.', ',']) then
      Exit(False);
  end;

  Result := HasDigit;
end;

class function TStringUtils.Contains(const S, SubStr: string): Boolean;  
begin
  Result := Pos(SubStr, S) > 0;
end;

// ... autres implémentations

end.
```

### Package complexe : Logger

```pascal
unit Logger;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs;

type
  TLogLevel = (llDebug, llInfo, llWarning, llError, llFatal);

  TLogEvent = procedure(ASender: TObject; ALevel: TLogLevel;
    const AMessage: string) of object;

  { TLogger - Système de logging thread-safe }
  TLogger = class(TComponent)
  private
    FLock: TCriticalSection;
    FLogFile: TextFile;
    FFileName: string;
    FMinLevel: TLogLevel;
    FOnLog: TLogEvent;
    FActive: Boolean;

    procedure WriteToFile(const AText: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Debug(const AMessage: string);
    procedure Info(const AMessage: string);
    procedure Warning(const AMessage: string);
    procedure Error(const AMessage: string);
    procedure Fatal(const AMessage: string);
    procedure Log(ALevel: TLogLevel; const AMessage: string);

  published
    property FileName: string read FFileName write FFileName;
    property MinLevel: TLogLevel read FMinLevel write FMinLevel default llInfo;
    property Active: Boolean read FActive write FActive default True;
    property OnLog: TLogEvent read FOnLog write FOnLog;
  end;

procedure Register;

implementation

procedure Register;  
begin
  RegisterComponents('Utilities', [TLogger]);
end;

constructor TLogger.Create(AOwner: TComponent);  
begin
  inherited Create(AOwner);
  FLock := TCriticalSection.Create;
  FMinLevel := llInfo;
  FActive := True;
end;

destructor TLogger.Destroy;  
begin
  if FFileName <> '' then
    CloseFile(FLogFile);
  FLock.Free;
  inherited Destroy;
end;

procedure TLogger.Log(ALevel: TLogLevel; const AMessage: string);  
const
  LevelStr: array[TLogLevel] of string = (
    'DEBUG', 'INFO', 'WARNING', 'ERROR', 'FATAL'
  );
var
  LogLine: string;
begin
  if not FActive or (ALevel < FMinLevel) then
    Exit;

  FLock.Enter;
  try
    LogLine := Format('[%s] %s: %s', [
      FormatDateTime('yyyy-mm-dd hh:nn:ss', Now),
      LevelStr[ALevel],
      AMessage
    ]);

    if FFileName <> '' then
      WriteToFile(LogLine);

    if Assigned(FOnLog) then
      FOnLog(Self, ALevel, AMessage);
  finally
    FLock.Leave;
  end;
end;

procedure TLogger.Debug(const AMessage: string);  
begin
  Log(llDebug, AMessage);
end;

procedure TLogger.Info(const AMessage: string);  
begin
  Log(llInfo, AMessage);
end;

// ... autres méthodes

end.
```

## Conclusion

Créer et maintenir un package de qualité est un processus enrichissant qui demande :

- **Planification** : Bien définir le scope et les fonctionnalités
- **Rigueur** : Code propre, testé et documenté
- **Communication** : Répondre aux utilisateurs, être transparent
- **Patience** : Les packages mûrissent avec le temps
- **Engagement** : Maintenance continue et évolution

**Points clés à retenir :**

✅ Structure organisée et claire  
✅ Documentation complète et à jour  
✅ Tests unitaires exhaustifs  
✅ Compatibilité multi-plateforme  
✅ Gestion de versions rigoureuse  
✅ Distribution facilitée (OPM)  
✅ Support communautaire actif

En suivant ces bonnes pratiques, vous créerez des packages qui seront appréciés et utilisés par la communauté FreePascal/Lazarus pendant de nombreuses années !

**Prochaine étape** : Publiez votre premier package et rejoignez la communauté des créateurs ! 🚀

⏭️ [Forums et ressources communautaires](/26-communaute-ecosysteme/03-forums-ressources-communautaires.md)
