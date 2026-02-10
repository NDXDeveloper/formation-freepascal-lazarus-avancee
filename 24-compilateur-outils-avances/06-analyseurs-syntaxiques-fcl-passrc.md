🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 24.6 Analyseurs syntaxiques (fcl-passrc)

## Introduction

Un **analyseur syntaxique** (ou *parser*) est un programme qui lit du code source et le transforme en une structure de données exploitable. FreePascal fournit la bibliothèque **fcl-passrc** qui permet d'analyser du code Pascal et de le manipuler programmatiquement.

**Analogie simple :** Imaginez que le code source est un texte en français. Un analyseur syntaxique est comme un professeur de grammaire qui décompose chaque phrase en sujet, verbe, complément, etc. Au lieu de phrases, fcl-passrc décompose le code Pascal en classes, fonctions, variables, etc.

---

## Pourquoi analyser du code Pascal ?

### Cas d'usage courants

**1. Génération automatique de documentation**
- Extraire les commentaires et signatures de fonctions
- Créer une documentation API automatiquement

**2. Refactoring et transformation de code**
- Renommer des identifiants dans tout un projet
- Convertir du code d'une version à une autre

**3. Analyse statique et qualité du code**
- Détecter les fonctions non utilisées
- Trouver du code dupliqué
- Analyser la complexité cyclomatique

**4. Outils de développement**
- Autocomplétion intelligente
- Navigation dans le code (aller à la définition)
- Analyse de dépendances

**5. Migration de code**
- Convertir du Delphi vers FreePascal
- Adapter du code pour différentes plateformes

---

## Architecture de fcl-passrc

### Les composants principaux

```
Code Source Pascal
       ↓
[Scanner] ← Décompose en tokens (mots-clés, identifiants, etc.)
       ↓
[Parser]  ← Construit l'arbre syntaxique
       ↓
Arbre Syntaxique (AST)
```

**1. Scanner (TPascalScanner)**
- Lit le fichier caractère par caractère
- Reconnaît les mots-clés, nombres, chaînes, opérateurs
- Produit un flux de "tokens" (unités lexicales)

**2. Parser (TPasParser)**
- Analyse la séquence de tokens
- Construit l'arbre syntaxique abstrait (AST)
- Vérifie la syntaxe

**3. Arbre Syntaxique (TPasElement et descendants)**
- Représente la structure du code
- Permet de naviguer dans le code de manière hiérarchique

---

## Installation et configuration

### Unités nécessaires

```pascal
uses
  PasTree,      // Classes de l'arbre syntaxique
  PScanner,     // Scanner (lecture et tokenisation)
  PParser,      // Parser (analyse syntaxique)
  SysUtils,     // Utilitaires système
  Classes;      // Listes et collections
```

### Configuration dans Lazarus

Dans votre projet (.lpi), ajoutez la dépendance :
```xml
<RequiredPkgs>
  <Item>
    <PackageName Value="FCL"/>
  </Item>
</RequiredPkgs>
```

Ou en ligne de commande :
```bash
fpc -Fu/usr/lib/fpc/3.2.2/units/x86_64-linux/fcl-passrc monprogramme.pas
```

---

## Premier exemple : Analyser une unité simple

### Code à analyser

Créons d'abord un fichier Pascal simple à analyser.

**Fichier : `sample.pas`**
```pascal
unit Sample;

interface

type
  TCalculator = class
  private
    FResult: Integer;
  public
    function Add(A, B: Integer): Integer;
    function Subtract(A, B: Integer): Integer;
    property Result: Integer read FResult;
  end;

implementation

function TCalculator.Add(A, B: Integer): Integer;
begin
  Result := A + B;
  FResult := Result;
end;

function TCalculator.Subtract(A, B: Integer): Integer;
begin
  Result := A - B;
  FResult := Result;
end;

end.
```

### Analyseur de base

```pascal
program SimpleParser;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, PasTree, PScanner, PParser;

type
  { Résolveur de fichiers simple }
  TSimpleFileResolver = class(TBaseFileResolver)
  public
    function FindSourceFile(const AName: String): TLineReader; override;
  end;

function TSimpleFileResolver.FindSourceFile(const AName: String): TLineReader;
var
  FileStream: TFileStream;
begin
  if not FileExists(AName) then
  begin
    Result := nil;
    Exit;
  end;

  FileStream := TFileStream.Create(AName, fmOpenRead);
  Result := TFileLineReader.Create(FileStream);
end;

procedure ParsePascalFile(const FileName: string);
var
  FileResolver: TSimpleFileResolver;
  Engine: TPasTreeContainer;
  Scanner: TPascalScanner;
  Parser: TPasParser;
  Module: TPasModule;
begin
  // Créer les composants
  FileResolver := TSimpleFileResolver.Create;
  Engine := TPasTreeContainer.Create;
  Scanner := TPascalScanner.Create(FileResolver);
  Parser := TPasParser.Create(Scanner, FileResolver, Engine);

  try
    // Ouvrir le fichier
    Scanner.OpenFile(FileName);

    // Parser le fichier
    Parser.ParseMain(Module);

    if Assigned(Module) then
    begin
      WriteLn('Unité analysée : ', Module.Name);
      WriteLn('Type de module : ', Module.ClassName);
    end
    else
      WriteLn('Erreur : impossible d''analyser le fichier');

  finally
    Parser.Free;
    Scanner.Free;
    Engine.Free;
    FileResolver.Free;
  end;
end;

begin
  if ParamCount < 1 then
  begin
    WriteLn('Usage: ', ExtractFileName(ParamStr(0)), ' <fichier.pas>');
    Exit;
  end;

  ParsePascalFile(ParamStr(1));
end.
```

**Résultat :**
```
Unité analysée : Sample
Type de module : TPasModule
```

---

## Explorer l'arbre syntaxique

### Structure de l'arbre

L'arbre syntaxique est composé d'objets dérivés de `TPasElement` :

```
TPasElement (classe de base)
  ├─ TPasModule (unité ou programme)
  │   ├─ InterfaceSection
  │   └─ ImplementationSection
  ├─ TPasType (types)
  │   ├─ TPasClassType (classes)
  │   ├─ TPasRecordType (records)
  │   └─ TPasEnumType (énumérations)
  ├─ TPasProcedure (procédures/fonctions)
  ├─ TPasProperty (propriétés)
  └─ TPasVariable (variables)
```

### Parcourir les éléments d'une unité

```pascal
procedure ExploreModule(Module: TPasModule);
var
  i: Integer;
  Element: TPasElement;
begin
  WriteLn('=== Exploration de l''unité : ', Module.Name, ' ===');
  WriteLn;

  // Section interface
  if Assigned(Module.InterfaceSection) then
  begin
    WriteLn('--- Section Interface ---');
    for i := 0 to Module.InterfaceSection.Declarations.Count - 1 do
    begin
      Element := TPasElement(Module.InterfaceSection.Declarations[i]);
      WriteLn('  - ', Element.ClassName, ' : ', Element.Name);
    end;
  end;

  WriteLn;

  // Section implementation
  if Assigned(Module.ImplementationSection) then
  begin
    WriteLn('--- Section Implementation ---');
    for i := 0 to Module.ImplementationSection.Declarations.Count - 1 do
    begin
      Element := TPasElement(Module.ImplementationSection.Declarations[i]);
      WriteLn('  - ', Element.ClassName, ' : ', Element.Name);
    end;
  end;
end;
```

**Résultat :**
```
=== Exploration de l'unité : Sample ===

--- Section Interface ---
  - TPasClassType : TCalculator

--- Section Implementation ---
  - TPasProcedure : Add
  - TPasProcedure : Subtract
```

---

## Analyser une classe

### Extraire les informations d'une classe

```pascal
procedure AnalyzeClass(ClassType: TPasClassType);
var
  i: Integer;
  Member: TPasElement;
  Proc: TPasProcedure;
  Prop: TPasProperty;
  Variable: TPasVariable;
begin
  WriteLn('Classe : ', ClassType.Name);

  if ClassType.AncestorType <> nil then
    WriteLn('  Hérite de : ', ClassType.AncestorType.Name);

  WriteLn('  Membres :');

  for i := 0 to ClassType.Members.Count - 1 do
  begin
    Member := TPasElement(ClassType.Members[i]);

    // Analyser selon le type de membre
    if Member is TPasProcedure then
    begin
      Proc := TPasProcedure(Member);
      Write('    [Méthode] ', Proc.Name);

      if Proc is TPasFunction then
        WriteLn(' : ', TPasFunction(Proc).ResultEl.ResultType.Name)
      else
        WriteLn(' (procédure)');
    end
    else if Member is TPasProperty then
    begin
      Prop := TPasProperty(Member);
      WriteLn('    [Propriété] ', Prop.Name, ' : ', Prop.VarType.Name);
    end
    else if Member is TPasVariable then
    begin
      Variable := TPasVariable(Member);
      WriteLn('    [Champ] ', Variable.Name, ' : ', Variable.VarType.Name);
    end
    else
      WriteLn('    [Autre] ', Member.ClassName, ' : ', Member.Name);
  end;
end;
```

**Résultat :**
```
Classe : TCalculator
  Membres :
    [Champ] FResult : Integer
    [Méthode] Add : Integer
    [Méthode] Subtract : Integer
    [Propriété] Result : Integer
```

---

## Analyser les fonctions et procédures

### Extraire les paramètres

```pascal
procedure AnalyzeProcedure(Proc: TPasProcedure);
var
  i, j: Integer;
  Arg: TPasArgument;
begin
  WriteLn('Procédure/Fonction : ', Proc.Name);

  // Type (fonction ou procédure)
  if Proc is TPasFunction then
    WriteLn('  Type : Fonction, retourne ', TPasFunction(Proc).ResultEl.ResultType.Name)
  else
    WriteLn('  Type : Procédure');

  // Paramètres
  if Proc.ProcType.Args.Count > 0 then
  begin
    WriteLn('  Paramètres :');
    for i := 0 to Proc.ProcType.Args.Count - 1 do
    begin
      Arg := TPasArgument(Proc.ProcType.Args[i]);
      Write('    - ', Arg.Name, ' : ', Arg.ArgType.Name);

      // Modificateurs
      case Arg.Access of
        argDefault: Write(' (par valeur)');
        argVar: Write(' (var)');
        argConst: Write(' (const)');
        argOut: Write(' (out)');
      end;

      WriteLn;
    end;
  end
  else
    WriteLn('  Aucun paramètre');

  WriteLn;
end;
```

**Résultat :**
```
Procédure/Fonction : Add
  Type : Fonction, retourne Integer
  Paramètres :
    - A : Integer (par valeur)
    - B : Integer (par valeur)

Procédure/Fonction : Subtract
  Type : Fonction, retourne Integer
  Paramètres :
    - A : Integer (par valeur)
    - B : Integer (par valeur)
```

---

## Analyser les types

### Détecter et analyser différents types

```pascal
procedure AnalyzeType(PasType: TPasType);
begin
  WriteLn('Type : ', PasType.Name);

  if PasType is TPasClassType then
  begin
    WriteLn('  Nature : Classe');
    AnalyzeClass(TPasClassType(PasType));
  end
  else if PasType is TPasRecordType then
  begin
    WriteLn('  Nature : Record');
    // Analyser les champs du record
  end
  else if PasType is TPasEnumType then
  begin
    WriteLn('  Nature : Énumération');
    // Lister les valeurs
  end
  else if PasType is TPasArrayType then
  begin
    WriteLn('  Nature : Tableau');
    // Analyser le type des éléments
  end
  else if PasType is TPasPointerType then
  begin
    WriteLn('  Nature : Pointeur');
    // Type pointé
  end
  else
    WriteLn('  Nature : ', PasType.ClassName);
end;
```

---

## Cas pratiques

### 1. Générateur de documentation

```pascal
program DocGenerator;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, PasTree, PScanner, PParser;

procedure GenerateClassDocumentation(ClassType: TPasClassType; Output: TStrings);
var
  i: Integer;
  Member: TPasElement;
  Proc: TPasProcedure;
begin
  Output.Add('## Classe : ' + ClassType.Name);
  Output.Add('');

  if ClassType.AncestorType <> nil then
    Output.Add('**Hérite de :** ' + ClassType.AncestorType.Name);

  Output.Add('');
  Output.Add('### Méthodes publiques');
  Output.Add('');

  for i := 0 to ClassType.Members.Count - 1 do
  begin
    Member := TPasElement(ClassType.Members[i]);

    if (Member is TPasProcedure) and (Member.Visibility = visPublic) then
    begin
      Proc := TPasProcedure(Member);

      if Proc is TPasFunction then
        Output.Add('- `' + Proc.Name + '()` : ' +
          TPasFunction(Proc).ResultEl.ResultType.Name)
      else
        Output.Add('- `' + Proc.Name + '()`');
    end;
  end;

  Output.Add('');
end;

procedure GenerateDocumentation(const InputFile, OutputFile: string);
var
  FileResolver: TSimpleFileResolver;
  Engine: TPasTreeContainer;
  Scanner: TPascalScanner;
  Parser: TPasParser;
  Module: TPasModule;
  Output: TStringList;
  i: Integer;
  Element: TPasElement;
begin
  Output := TStringList.Create;
  FileResolver := TSimpleFileResolver.Create;
  Engine := TPasTreeContainer.Create;
  Scanner := TPascalScanner.Create(FileResolver);
  Parser := TPasParser.Create(Scanner, FileResolver, Engine);

  try
    Scanner.OpenFile(InputFile);
    Parser.ParseMain(Module);

    if Assigned(Module) then
    begin
      Output.Add('# Documentation : ' + Module.Name);
      Output.Add('');

      if Assigned(Module.InterfaceSection) then
      begin
        for i := 0 to Module.InterfaceSection.Declarations.Count - 1 do
        begin
          Element := TPasElement(Module.InterfaceSection.Declarations[i]);

          if Element is TPasClassType then
            GenerateClassDocumentation(TPasClassType(Element), Output);
        end;
      end;

      Output.SaveToFile(OutputFile);
      WriteLn('Documentation générée : ', OutputFile);
    end;

  finally
    Output.Free;
    Parser.Free;
    Scanner.Free;
    Engine.Free;
    FileResolver.Free;
  end;
end;

begin
  if ParamCount < 2 then
  begin
    WriteLn('Usage: ', ExtractFileName(ParamStr(0)), ' <input.pas> <output.md>');
    Exit;
  end;

  GenerateDocumentation(ParamStr(1), ParamStr(2));
end.
```

### 2. Détecteur de code mort

```pascal
program DeadCodeDetector;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, PasTree, PScanner, PParser, Contnrs;

type
  TProcedureInfo = class
    Name: string;
    Used: Boolean;
  end;

procedure FindUnusedProcedures(Module: TPasModule);
var
  ProcList: TObjectList;
  i, j: Integer;
  Element: TPasElement;
  Proc: TPasProcedure;
  ProcInfo: TProcedureInfo;
  Found: Boolean;
begin
  ProcList := TObjectList.Create(True);
  try
    // Collecter toutes les procédures
    if Assigned(Module.ImplementationSection) then
    begin
      for i := 0 to Module.ImplementationSection.Declarations.Count - 1 do
      begin
        Element := TPasElement(Module.ImplementationSection.Declarations[i]);

        if Element is TPasProcedure then
        begin
          ProcInfo := TProcedureInfo.Create;
          ProcInfo.Name := Element.Name;
          ProcInfo.Used := False;
          ProcList.Add(ProcInfo);
        end;
      end;
    end;

    // TODO: Analyser les appels de fonctions dans le code
    // (nécessite une analyse plus approfondie de l'AST)

    // Afficher les procédures potentiellement inutilisées
    WriteLn('=== Procédures potentiellement non utilisées ===');
    for i := 0 to ProcList.Count - 1 do
    begin
      ProcInfo := TProcedureInfo(ProcList[i]);
      if not ProcInfo.Used then
        WriteLn('  - ', ProcInfo.Name);
    end;

  finally
    ProcList.Free;
  end;
end;
```

### 3. Convertisseur de syntaxe

```pascal
program SyntaxConverter;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, PasTree, PScanner, PParser;

{ Convertir les propriétés avec getters/setters explicites
  en propriétés modernes avec champs automatiques }
procedure ModernizeProperties(ClassType: TPasClassType);
var
  i: Integer;
  Member: TPasElement;
  Prop: TPasProperty;
begin
  WriteLn('Modernisation de la classe : ', ClassType.Name);

  for i := 0 to ClassType.Members.Count - 1 do
  begin
    Member := TPasElement(ClassType.Members[i]);

    if Member is TPasProperty then
    begin
      Prop := TPasProperty(Member);

      // Suggérer une version simplifiée
      if (Prop.ReadAccessorName <> '') and (Prop.WriteAccessorName <> '') then
      begin
        WriteLn('  Propriété : ', Prop.Name);
        WriteLn('    Ancien : property ', Prop.Name, ': ', Prop.VarType.Name,
          ' read ', Prop.ReadAccessorName, ' write ', Prop.WriteAccessorName, ';');
        WriteLn('    Suggéré : property ', Prop.Name, ': ', Prop.VarType.Name, ';');
        WriteLn;
      end;
    end;
  end;
end;
```

---

## Analyse multi-fichiers

### Analyser un projet complet

```pascal
program ProjectAnalyzer;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, PasTree, PScanner, PParser, Contnrs;

type
  TProjectAnalyzer = class
  private
    FModules: TObjectList;
    FFileResolver: TBaseFileResolver;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AnalyzeFile(const FileName: string);
    procedure AnalyzeProject(const ProjectDir: string);
    procedure GenerateReport;
  end;

constructor TProjectAnalyzer.Create;
begin
  inherited Create;
  FModules := TObjectList.Create(False);
  FFileResolver := TSimpleFileResolver.Create;
end;

destructor TProjectAnalyzer.Destroy;
begin
  FModules.Free;
  FFileResolver.Free;
  inherited Destroy;
end;

procedure TProjectAnalyzer.AnalyzeFile(const FileName: string);
var
  Engine: TPasTreeContainer;
  Scanner: TPascalScanner;
  Parser: TPasParser;
  Module: TPasModule;
begin
  Engine := TPasTreeContainer.Create;
  Scanner := TPascalScanner.Create(FFileResolver);
  Parser := TPasParser.Create(Scanner, FFileResolver, Engine);

  try
    Scanner.OpenFile(FileName);
    Parser.ParseMain(Module);

    if Assigned(Module) then
    begin
      FModules.Add(Module);
      WriteLn('Analysé : ', FileName, ' (', Module.Name, ')');
    end;

  finally
    Parser.Free;
    Scanner.Free;
    Engine.Free;
  end;
end;

procedure TProjectAnalyzer.AnalyzeProject(const ProjectDir: string);
var
  SearchRec: TSearchRec;
  FilePath: string;
begin
  WriteLn('Analyse du projet : ', ProjectDir);
  WriteLn;

  // Rechercher tous les fichiers .pas
  if FindFirst(ProjectDir + '*.pas', faAnyFile, SearchRec) = 0 then
  begin
    repeat
      FilePath := ProjectDir + SearchRec.Name;
      AnalyzeFile(FilePath);
    until FindNext(SearchRec) <> 0;
    FindClose(SearchRec);
  end;

  WriteLn;
  WriteLn('Nombre de modules analysés : ', FModules.Count);
end;

procedure TProjectAnalyzer.GenerateReport;
var
  i: Integer;
  Module: TPasModule;
begin
  WriteLn('=== Rapport d''analyse ===');
  WriteLn;

  for i := 0 to FModules.Count - 1 do
  begin
    Module := TPasModule(FModules[i]);
    WriteLn('Module : ', Module.Name);

    if Assigned(Module.InterfaceSection) then
      WriteLn('  Déclarations interface : ',
        Module.InterfaceSection.Declarations.Count);

    if Assigned(Module.ImplementationSection) then
      WriteLn('  Déclarations implementation : ',
        Module.ImplementationSection.Declarations.Count);

    WriteLn;
  end;
end;

var
  Analyzer: TProjectAnalyzer;
begin
  if ParamCount < 1 then
  begin
    WriteLn('Usage: ', ExtractFileName(ParamStr(0)), ' <répertoire_projet>');
    Exit;
  end;

  Analyzer := TProjectAnalyzer.Create;
  try
    Analyzer.AnalyzeProject(IncludeTrailingPathDelimiter(ParamStr(1)));
    Analyzer.GenerateReport;
  finally
    Analyzer.Free;
  end;
end.
```

---

## Gestion des erreurs

### Capturer les erreurs de parsing

```pascal
procedure SafeParseFile(const FileName: string);
var
  FileResolver: TSimpleFileResolver;
  Engine: TPasTreeContainer;
  Scanner: TPascalScanner;
  Parser: TPasParser;
  Module: TPasModule;
begin
  FileResolver := TSimpleFileResolver.Create;
  Engine := TPasTreeContainer.Create;
  Scanner := TPascalScanner.Create(FileResolver);
  Parser := TPasParser.Create(Scanner, FileResolver, Engine);

  try
    try
      Scanner.OpenFile(FileName);
      Parser.ParseMain(Module);

      if Assigned(Module) then
        WriteLn('Succès : ', Module.Name)
      else
        WriteLn('Erreur : Module nil');

    except
      on E: EScannerError do
        WriteLn('Erreur scanner : ', E.Message);
      on E: EParserError do
        WriteLn('Erreur parsing : ', E.Message);
      on E: Exception do
        WriteLn('Erreur : ', E.Message);
    end;

  finally
    Parser.Free;
    Scanner.Free;
    Engine.Free;
    FileResolver.Free;
  end;
end;
```

---

## Limitations et considérations

### 1. Parsing partiel

fcl-passrc peut analyser la **structure** du code, mais pas le **contenu des fonctions** :

```pascal
// ✅ fcl-passrc peut analyser :
function Calculate(X: Integer): Integer;  // Signature

// ❌ fcl-passrc ne peut PAS analyser en détail :
begin
  Result := X * 2 + 5;  // Code dans le corps de la fonction
end;
```

### 2. Dépendances externes

Si votre code utilise des unités externes, vous devez configurer le résolveur de fichiers pour les trouver.

### 3. Performance

L'analyse de gros projets peut être lente. Pour optimiser :
- Analyser uniquement les fichiers nécessaires
- Mettre en cache les résultats
- Paralléliser l'analyse si possible

### 4. Compatibilité dialectes

fcl-passrc supporte principalement le dialecte FreePascal. Le code Delphi peut nécessiter des ajustements.

---

## Outils basés sur fcl-passrc

### 1. FPDoc (générateur de documentation)

FreePascal inclut **fpdoc** qui utilise fcl-passrc pour générer de la documentation :

```bash
fpdoc --input=myunit.pas --output=documentation.html --format=html
```

### 2. PasDoc (documentation alternative)

PasDoc est un générateur de documentation externe qui utilise aussi fcl-passrc :

```bash
pasdoc --format html --output docs myunit.pas
```

### 3. CodeTools (IDE Lazarus)

L'IDE Lazarus utilise fcl-passrc pour :
- L'autocomplétion
- La navigation dans le code
- Le refactoring

---

## Aller plus loin

### Créer un analyseur personnalisé

```pascal
type
  TCustomAnalyzer = class
  private
    FClassCount: Integer;
    FMethodCount: Integer;
    FLinesOfCode: Integer;
  public
    procedure AnalyzeElement(Element: TPasElement);
    procedure PrintStatistics;
  end;

procedure TCustomAnalyzer.AnalyzeElement(Element: TPasElement);
var
  i: Integer;
  ClassType: TPasClassType;
  Member: TPasElement;
begin
  if Element is TPasClassType then
  begin
    Inc(FClassCount);
    ClassType := TPasClassType(Element);

    for i := 0 to ClassType.Members.Count - 1 do
    begin
      Member := TPasElement(ClassType.Members[i]);

      if Member is TPasProcedure then
        Inc(FMethodCount);

      // Analyser récursivement
      AnalyzeElement(Member);
    end;
  end;
end;

procedure TCustomAnalyzer.PrintStatistics;
begin
  WriteLn('=== Statistiques ===');
  WriteLn('Classes : ', FClassCount);
  WriteLn('Méthodes : ', FMethodCount);
  WriteLn('Lignes de code : ', FLinesOfCode);
end;
```

---

## Bonnes pratiques

### 1. Libérer les ressources

Toujours libérer les objets dans le bon ordre :

```pascal
try
  // Utilisation
finally
  Parser.Free;      // 1. Parser
  Scanner.Free;     // 2. Scanner
  Engine.Free;      // 3. Engine
  FileResolver.Free;// 4. FileResolver
end;
```

### 2. Vérifier les pointeurs nil

```pascal
if Assigned(Module) and Assigned(Module.InterfaceSection) then
begin
  // Utilisation sûre
end;
```

### 3. Gérer les exceptions

```pascal
try
  // Parse
except
  on E: Exception do
  begin
    WriteLn('Erreur : ', E.Message);
    // Logging, nettoyage, etc.
  end;
end;
```

### 4. Mode de compatibilité

Configurer le scanner pour le dialecte approprié :

```pascal
Scanner.Options := Scanner.Options + [po_delphi];  // Mode Delphi
```

---

## Conclusion

La bibliothèque **fcl-passrc** est un outil puissant qui permet :

✅ **Analyser** la structure du code Pascal  
✅ **Générer** de la documentation automatiquement  
✅ **Créer** des outils de refactoring  
✅ **Développer** des analyseurs de qualité de code  
✅ **Migrer** du code entre dialectes

**Points clés à retenir :**
- fcl-passrc analyse la **structure**, pas l'exécution
- L'arbre syntaxique est composé de **TPasElement**
- Toujours gérer les **ressources et exceptions**
- Idéal pour les **outils de développement**

**Prochaines étapes :**
1. Créer un analyseur simple pour vos projets
2. Générer de la documentation automatique
3. Développer des outils de qualité de code
4. Explorer les possibilités de refactoring automatique

⏭️ [Outils de build personnalisés](/24-compilateur-outils-avances/07-outils-build-personnalises.md)
