🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 24.4 Génération de code custom

## Introduction

La génération de code custom consiste à créer des programmes qui écrivent automatiquement du code source Pascal. Cette technique permet d'automatiser des tâches répétitives, de générer des structures complexes à partir de modèles, ou de créer des interfaces à partir de spécifications externes.

Bien que ce soit un sujet avancé, nous allons l'aborder progressivement pour le rendre accessible à tous.

---

## Pourquoi générer du code ?

### Cas d'usage courants

**1. Génération d'interfaces depuis des bases de données**
- Créer automatiquement des classes Pascal à partir de tables SQL
- Générer des formulaires de saisie basés sur la structure des données

**2. Création de wrappers pour bibliothèques externes**
- Générer des bindings Pascal pour des bibliothèques C/C++
- Automatiser la création d'interfaces pour des API REST

**3. Automatisation de code répétitif**
- Générer des getters/setters pour de nombreuses propriétés
- Créer des sérialisations JSON/XML automatiques

**4. Métaprogrammation**
- Générer du code à partir de templates
- Créer des DSL (Domain Specific Languages)

---

## Les différentes approches

### 1. Génération à la compilation (macros)

FreePascal supporte les macros du préprocesseur qui permettent une génération simple de code.

```pascal
{$MACRO ON}
{$DEFINE PROPERTY_GETTER := function Get}
{$DEFINE PROPERTY_SETTER := procedure Set}

type
  TMyClass = class
  private
    FName: string;
    PROPERTY_GETTER Name: string;
    PROPERTY_SETTER Name(const AValue: string);
  end;
```

**Avantages :**
- Intégré au compilateur
- Très rapide
- Pas de dépendances externes

**Limites :**
- Fonctionnalités limitées
- Syntaxe parfois complexe

### 2. Génération avant compilation (outils externes)

Des programmes externes génèrent des fichiers `.pas` qui sont ensuite compilés.

```pascal
program CodeGenerator;

uses
  SysUtils, Classes;

procedure GenerateClassFromTable(const TableName: string);  
var
  Output: TStringList;
begin
  Output := TStringList.Create;
  try
    Output.Add('unit Generated_' + TableName + ';');
    Output.Add('');
    Output.Add('interface');
    Output.Add('');
    Output.Add('type');
    Output.Add('  T' + TableName + ' = class');
    Output.Add('  private');
    Output.Add('    FID: Integer;');
    Output.Add('    FName: string;');
    Output.Add('  public');
    Output.Add('    property ID: Integer read FID write FID;');
    Output.Add('    property Name: string read FName write FName;');
    Output.Add('  end;');
    Output.Add('');
    Output.Add('implementation');
    Output.Add('');
    Output.Add('end.');

    Output.SaveToFile('Generated_' + TableName + '.pas');
  finally
    Output.Free;
  end;
end;

begin
  GenerateClassFromTable('Customer');
  GenerateClassFromTable('Product');
  GenerateClassFromTable('Order');
end.
```

**Avantages :**
- Contrôle total sur la génération
- Peut analyser des sources externes (XML, JSON, bases de données)
- Facilite la maintenance de code répétitif

**Limites :**
- Nécessite une étape de build supplémentaire
- Le code généré doit être géré dans le contrôle de version

### 3. Génération à l'exécution (RTTI et réflexion)

Utilisation de la RTTI (Run-Time Type Information) pour générer dynamiquement du code.

```pascal
uses
  TypInfo, Rtti;

procedure GeneratePropertyAccessors(AClass: TClass);  
var
  Context: TRttiContext;
  RType: TRttiType;
  Prop: TRttiProperty;
  Code: TStringList;
begin
  Context := TRttiContext.Create;
  Code := TStringList.Create;
  try
    RType := Context.GetType(AClass);

    Code.Add('// Accesseurs générés automatiquement');

    for Prop in RType.GetProperties do
    begin
      Code.Add(Format('function Get%s: %s;',
        [Prop.Name, Prop.PropertyType.Name]));
      Code.Add(Format('procedure Set%s(const Value: %s);',
        [Prop.Name, Prop.PropertyType.Name]));
    end;

    WriteLn(Code.Text);
  finally
    Code.Free;
    Context.Free;
  end;
end;
```

---

## Techniques avancées de génération

### 1. Templates et substitution

Utilisation de fichiers template avec des marqueurs à remplacer.

**Fichier template : `class_template.txt`**
```
unit {{UNIT_NAME}};

interface

type
  {{CLASS_NAME}} = class
  private
{{PRIVATE_FIELDS}}
  public
{{PUBLIC_PROPERTIES}}
  end;

implementation

end.
```

**Code de génération :**
```pascal
program TemplateGenerator;

uses
  SysUtils, Classes;

type
  TTemplateEngine = class
  private
    FTemplate: string;
  public
    constructor Create(const TemplateFile: string);
    function Replace(const Marker, Value: string): TTemplateEngine;
    procedure SaveTo(const OutputFile: string);
  end;

constructor TTemplateEngine.Create(const TemplateFile: string);  
var
  TemplateList: TStringList;
begin
  TemplateList := TStringList.Create;
  try
    TemplateList.LoadFromFile(TemplateFile);
    FTemplate := TemplateList.Text;
  finally
    TemplateList.Free;
  end;
end;

function TTemplateEngine.Replace(const Marker, Value: string): TTemplateEngine;  
begin
  FTemplate := StringReplace(FTemplate, '{{' + Marker + '}}',
    Value, [rfReplaceAll]);
  Result := Self; // Pour le chaînage
end;

procedure TTemplateEngine.SaveTo(const OutputFile: string);  
var
  Output: TStringList;
begin
  Output := TStringList.Create;
  try
    Output.Text := FTemplate;
    Output.SaveToFile(OutputFile);
  finally
    Output.Free;
  end;
end;

// Utilisation
var
  Engine: TTemplateEngine;
begin
  Engine := TTemplateEngine.Create('class_template.txt');
  try
    Engine
      .Replace('UNIT_NAME', 'GeneratedCustomer')
      .Replace('CLASS_NAME', 'TCustomer')
      .Replace('PRIVATE_FIELDS', '    FID: Integer;' + sLineBreak +
                                 '    FName: string;')
      .Replace('PUBLIC_PROPERTIES', '    property ID: Integer read FID write FID;' +
                                    sLineBreak + '    property Name: string read FName write FName;')
      .SaveTo('GeneratedCustomer.pas');
  finally
    Engine.Free;
  end;
end.
```

### 2. Génération depuis des métadonnées

Lecture de fichiers de configuration pour générer du code.

**Fichier de configuration : `entities.json`**
```json
{
  "entities": [
    {
      "name": "Customer",
      "fields": [
        {"name": "ID", "type": "Integer"},
        {"name": "Name", "type": "string"},
        {"name": "Email", "type": "string"}
      ]
    },
    {
      "name": "Product",
      "fields": [
        {"name": "ID", "type": "Integer"},
        {"name": "Title", "type": "string"},
        {"name": "Price", "type": "Currency"}
      ]
    }
  ]
}
```

**Générateur :**
```pascal
program JSONCodeGenerator;

uses
  SysUtils, Classes, fpjson, jsonparser;

type
  TFieldDef = record
    Name: string;
    FieldType: string;
  end;

  TEntityDef = record
    Name: string;
    Fields: array of TFieldDef;
  end;

procedure GenerateClassFromEntity(const Entity: TEntityDef);  
var
  Code: TStringList;
  Field: TFieldDef;
begin
  Code := TStringList.Create;
  try
    Code.Add('unit Generated' + Entity.Name + ';');
    Code.Add('');
    Code.Add('interface');
    Code.Add('');
    Code.Add('type');
    Code.Add('  T' + Entity.Name + ' = class');
    Code.Add('  private');

    // Champs privés
    for Field in Entity.Fields do
      Code.Add(Format('    F%s: %s;', [Field.Name, Field.FieldType]));

    Code.Add('  public');

    // Propriétés publiques
    for Field in Entity.Fields do
      Code.Add(Format('    property %s: %s read F%s write F%s;',
        [Field.Name, Field.FieldType, Field.Name, Field.Name]));

    Code.Add('  end;');
    Code.Add('');
    Code.Add('implementation');
    Code.Add('');
    Code.Add('end.');

    Code.SaveToFile('Generated' + Entity.Name + '.pas');
    WriteLn('Généré : Generated' + Entity.Name + '.pas');
  finally
    Code.Free;
  end;
end;

procedure ParseAndGenerate(const JSONFile: string);  
var
  JSONData: TJSONData;
  JSONArray: TJSONArray;
  EntityObj, FieldObj: TJSONObject;
  FieldsArray: TJSONArray;
  FileContent: TStringList;
  Entity: TEntityDef;
  i, j: Integer;
begin
  FileContent := TStringList.Create;
  try
    FileContent.LoadFromFile(JSONFile);
    JSONData := GetJSON(FileContent.Text);
  finally
    FileContent.Free;
  end;
  try
    JSONArray := TJSONObject(JSONData).Arrays['entities'];

    for i := 0 to JSONArray.Count - 1 do
    begin
      EntityObj := TJSONObject(JSONArray[i]);
      Entity.Name := EntityObj.Strings['name'];

      FieldsArray := EntityObj.Arrays['fields'];
      SetLength(Entity.Fields, FieldsArray.Count);

      for j := 0 to FieldsArray.Count - 1 do
      begin
        FieldObj := TJSONObject(FieldsArray[j]);
        Entity.Fields[j].Name := FieldObj.Strings['name'];
        Entity.Fields[j].FieldType := FieldObj.Strings['type'];
      end;

      GenerateClassFromEntity(Entity);
    end;
  finally
    JSONData.Free;
  end;
end;

begin
  ParseAndGenerate('entities.json');
end.
```

### 3. Génération de code multi-plateforme

Génération de code qui s'adapte automatiquement à la plateforme cible.

```pascal
program CrossPlatformGenerator;

uses
  SysUtils, Classes, TypInfo;

type
  TTargetPlatform = (tpWindows, tpLinux, tpMacOS);

procedure GeneratePlatformSpecificCode(Platform: TTargetPlatform);  
var
  Code: TStringList;
begin
  Code := TStringList.Create;
  try
    Code.Add('unit PlatformUtils;');
    Code.Add('');
    Code.Add('interface');
    Code.Add('');

    case Platform of
      tpWindows:
        begin
          Code.Add('{$IFDEF WINDOWS}');
          Code.Add('uses Windows;');
          Code.Add('');
          Code.Add('function GetTempPath: string;');
          Code.Add('begin');
          Code.Add('  Result := GetEnvironmentVariable(''TEMP'');');
          Code.Add('end;');
          Code.Add('{$ENDIF}');
        end;

      tpLinux:
        begin
          Code.Add('{$IFDEF LINUX}');
          Code.Add('uses BaseUnix;');
          Code.Add('');
          Code.Add('function GetTempPath: string;');
          Code.Add('begin');
          Code.Add('  Result := ''/tmp'';');
          Code.Add('end;');
          Code.Add('{$ENDIF}');
        end;
    end;

    Code.Add('');
    Code.Add('implementation');
    Code.Add('');
    Code.Add('end.');

    Code.SaveToFile('PlatformUtils_' + GetEnumName(TypeInfo(TTargetPlatform),
      Ord(Platform)) + '.pas');
  finally
    Code.Free;
  end;
end;

begin
  GeneratePlatformSpecificCode(tpWindows);
  GeneratePlatformSpecificCode(tpLinux);
end.
```

---

## Utilisation de fcl-passrc pour l'analyse syntaxique

FreePascal fournit la bibliothèque **fcl-passrc** qui permet d'analyser du code Pascal existant et de le manipuler.

```pascal
program ParsePascalCode;

uses
  SysUtils, Classes, PasTree, PScanner, PParser;

procedure AnalyzeUnit(const FileName: string);  
var
  Module: TPasModule;
  Scanner: TPascalScanner;
  Parser: TPasParser;
  FileResolver: TFileResolver;
  Engine: TPasTreeContainer;
begin
  FileResolver := TFileResolver.Create;
  Engine := TPasTreeContainer.Create;
  Scanner := TPascalScanner.Create(FileResolver);
  Parser := TPasParser.Create(Scanner, FileResolver, Engine);

  try
    Scanner.OpenFile(FileName);
    Parser.ParseMain(Module);

    if Assigned(Module) then
    begin
      WriteLn('Unité analysée : ', Module.Name);
      // Ici on peut parcourir l'arbre syntaxique
      // et générer du code basé sur l'analyse
    end;
  finally
    Parser.Free;
    Scanner.Free;
    Engine.Free;
    FileResolver.Free;
  end;
end;

begin
  AnalyzeUnit('MonUnite.pas');
end.
```

---

## Bonnes pratiques

### 1. Séparation du code généré et manuel

**Structure recommandée :**
```
project/
  ├── src/           (code manuel)
  ├── generated/     (code généré)
  └── templates/     (templates de génération)
```

**Marquage du code généré :**
```pascal
// Ce fichier a été généré automatiquement le 09/10/2025 à 15:30
// NE PAS MODIFIER MANUELLEMENT
// Générateur : EntityGenerator v1.0
// Source : entities.json

unit GeneratedCustomer;
```

### 2. Gestion de la régénération

```pascal
procedure SafeGenerate(const OutputFile: string);  
begin
  // Vérifier si le fichier existe
  if FileExists(OutputFile) then
  begin
    // Sauvegarder l'ancienne version
    CopyFile(PChar(OutputFile), PChar(OutputFile + '.bak'), False);
  end;

  // Générer le nouveau fichier
  GenerateCode(OutputFile);

  // Vérifier que la génération a réussi
  if not FileExists(OutputFile) then
  begin
    // Restaurer la sauvegarde
    if FileExists(OutputFile + '.bak') then
      RenameFile(OutputFile + '.bak', OutputFile);
  end
  else
  begin
    // Supprimer la sauvegarde
    DeleteFile(OutputFile + '.bak');
  end;
end;
```

### 3. Documentation automatique

Inclure des commentaires dans le code généré :

```pascal
Code.Add('  /// <summary>');  
Code.Add('  /// Représente un client dans le système');  
Code.Add('  /// </summary>');  
Code.Add('  /// <remarks>');  
Code.Add('  /// Cette classe a été générée automatiquement');  
Code.Add('  /// à partir de la table Customer');  
Code.Add('  /// </remarks>');  
Code.Add('  TCustomer = class');
```

### 4. Validation du code généré

```pascal
function ValidateGeneratedCode(const PascalFile: string): Boolean;  
var
  Process: TProcess;
begin
  // Tester la compilation du fichier généré
  Process := TProcess.Create(nil);
  try
    Process.Executable := 'fpc';
    Process.Parameters.Add('-l');  // Vérification syntaxique seulement
    Process.Parameters.Add(PascalFile);
    Process.Options := [poWaitOnExit, poUsePipes];
    Process.Execute;

    Result := Process.ExitStatus = 0;
  finally
    Process.Free;
  end;
end;
```

---

## Intégration dans le processus de build

### Sous Windows

**Script batch : `generate.bat`**
```batch
@echo off
echo Génération du code...  
generator.exe entities.json  
if %ERRORLEVEL% NEQ 0 (
    echo Erreur lors de la génération
    exit /b 1
)
echo Compilation...  
lazbuild project.lpi
```

### Sous Linux/Ubuntu

**Script bash : `generate.sh`**
```bash
#!/bin/bash
echo "Génération du code..."
./generator entities.json
if [ $? -ne 0 ]; then
    echo "Erreur lors de la génération"
    exit 1
fi  
echo "Compilation..."  
lazbuild project.lpi
```

### Makefile universel

```makefile
# Makefile pour génération et compilation

.PHONY: all generate compile clean

all: generate compile

generate:
	@echo "Génération du code..."
	./generator entities.json

compile:
	@echo "Compilation..."
	lazbuild project.lpi

clean:
	@echo "Nettoyage..."
	rm -f generated/*.pas
	rm -f *.o *.ppu
```

---

## Outils et bibliothèques utiles

### 1. mORMot Code Generator
Framework qui inclut des outils de génération de code pour interfaces REST, ORM, etc.

### 2. Pas2JS
Transpileur Pascal vers JavaScript qui génère automatiquement du code JS.

### 3. JEDI Code Library (JCL)
Contient des utilitaires pour la manipulation de templates et la génération de code.

### 4. Pascal Script
Permet d'exécuter du Pascal à la volée pour générer du code dynamiquement.

---

## Cas d'usage avancés

### 1. Génération d'API REST à partir d'OpenAPI/Swagger

```pascal
// Lire une spécification OpenAPI YAML/JSON
// Générer les classes, les contrôleurs et les routes
procedure GenerateRESTAPIFromOpenAPI(const SpecFile: string);  
begin
  // Parser le fichier OpenAPI
  // Pour chaque endpoint, générer :
  //   - Les classes de modèle (DTOs)
  //   - Les contrôleurs
  //   - La documentation
end;
```

### 2. Génération de tests unitaires

```pascal
procedure GenerateTestsForClass(const ClassName: string);  
var
  Code: TStringList;
begin
  Code := TStringList.Create;
  try
    Code.Add('unit Test' + ClassName + ';');
    Code.Add('');
    Code.Add('interface');
    Code.Add('');
    Code.Add('uses');
    Code.Add('  fpcunit, ' + ClassName + ';');
    Code.Add('');
    Code.Add('type');
    Code.Add('  T' + ClassName + 'Test = class(TTestCase)');
    Code.Add('  published');
    Code.Add('    procedure TestCreate;');
    Code.Add('    procedure TestDestroy;');
    Code.Add('  end;');
    Code.Add('');
    Code.Add('implementation');
    Code.Add('');
    Code.Add('procedure T' + ClassName + 'Test.TestCreate;');
    Code.Add('var');
    Code.Add('  Instance: T' + ClassName + ';');
    Code.Add('begin');
    Code.Add('  Instance := T' + ClassName + '.Create;');
    Code.Add('  try');
    Code.Add('    AssertNotNull(Instance);');
    Code.Add('  finally');
    Code.Add('    Instance.Free;');
    Code.Add('  end;');
    Code.Add('end;');
    Code.Add('');
    Code.Add('end.');

    Code.SaveToFile('Test' + ClassName + '.pas');
  finally
    Code.Free;
  end;
end;
```

### 3. Migration de code entre plateformes

Générer automatiquement les directives de compilation conditionnelle :

```pascal
procedure AddPlatformDirectives(const SourceFile, OutputFile: string);  
var
  Source, Output: TStringList;
  i: Integer;
  Line: string;
begin
  Source := TStringList.Create;
  Output := TStringList.Create;
  try
    Source.LoadFromFile(SourceFile);

    for i := 0 to Source.Count - 1 do
    begin
      Line := Source[i];

      // Détecter les appels à des APIs Windows
      if Pos('Windows.', Line) > 0 then
      begin
        Output.Add('{$IFDEF WINDOWS}');
        Output.Add(Line);
        Output.Add('{$ENDIF}');
      end
      // Détecter les appels à des APIs Linux
      else if Pos('BaseUnix.', Line) > 0 then
      begin
        Output.Add('{$IFDEF LINUX}');
        Output.Add(Line);
        Output.Add('{$ENDIF}');
      end
      else
        Output.Add(Line);
    end;

    Output.SaveToFile(OutputFile);
  finally
    Source.Free;
    Output.Free;
  end;
end;
```

---

## Conclusion

La génération de code custom est un outil puissant qui permet :
- **D'automatiser** les tâches répétitives
- **D'améliorer** la maintenabilité du code
- **De réduire** les erreurs humaines
- **D'accélérer** le développement

Bien que l'apprentissage initial puisse sembler complexe, maîtriser ces techniques vous rendra beaucoup plus productif, particulièrement dans les projets de grande envergure ou nécessitant du code multi-plateforme.

**Prochaines étapes recommandées :**
1. Créer votre premier générateur simple (par exemple, générateur de getters/setters)
2. Expérimenter avec les templates
3. Explorer fcl-passrc pour l'analyse de code existant
4. Intégrer la génération dans votre processus de build

⏭️ [Preprocesseur et macros](/24-compilateur-outils-avances/05-preprocesseur-macros.md)
