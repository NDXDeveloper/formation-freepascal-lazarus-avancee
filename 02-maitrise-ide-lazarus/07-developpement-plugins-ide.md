🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 2.7 Développement de plugins IDE

## Introduction : Étendre Lazarus selon vos besoins

Lazarus n'est pas un IDE figé. C'est une plateforme extensible que vous pouvez adapter et enrichir selon vos besoins. Imaginez pouvoir ajouter votre propre bouton dans la barre d'outils, créer un nouveau panneau d'analyse de code, ou automatiser des tâches répétitives. C'est exactement ce que permettent les plugins IDE !

**Qu'est-ce qu'un plugin IDE ?**

Un plugin (ou extension) IDE est un package Lazarus spécial qui ajoute de nouvelles fonctionnalités à l'environnement de développement lui-même. Contrairement aux composants qui enrichissent vos applications, les plugins enrichissent Lazarus.

**Exemples de ce que vous pouvez faire :**
- 🔧 **Nouveaux outils** : Analyseurs, générateurs de code
- 📊 **Panneaux personnalisés** : Statistiques, documentation
- 🎨 **Éditeurs spécialisés** : Éditeurs JSON, XML, SQL
- ⚡ **Automatisation** : Macros, templates, snippets
- 🔌 **Intégrations** : Connexion à des services externes
- 📝 **Assistants** : Wizards pour créer du code

## Architecture des plugins Lazarus

### Comprendre le système de plugins

Lazarus utilise son propre système de packages pour les plugins. Un plugin est essentiellement un package avec des points d'ancrage (hooks) dans l'IDE.

```
Architecture d'un plugin IDE :
├── Package (.lpk)
│   ├── Type : Design-time
│   ├── Installation : Recompile l'IDE
│   └── Chargement : Au démarrage
├── Points d'extension
│   ├── Menus
│   ├── Barres d'outils
│   ├── Fenêtres
│   └── Éditeur
└── API IDE
    ├── IDEIntf (interfaces)
    ├── LazIDEIntf (classes)
    └── SynEdit (éditeur)
```

### Les unités essentielles

Pour développer des plugins, vous devez connaître ces unités :

```pascal
// Unités principales pour plugins IDE
uses
  // Interfaces de base
  IDEIntf,          // Types et interfaces IDE
  IDECommands,      // Système de commandes
  MenuIntf,         // Manipulation des menus
  ToolBarIntf,      // Barres d'outils

  // Éditeur
  SrcEditorIntf,    // Interface éditeur de code
  CodeToolsStructs, // Structures CodeTools
  CodeCache,        // Cache du code

  // Projet
  ProjectIntf,      // Interface projet
  PackageIntf,      // Interface packages

  // Fenêtres
  IDEWindowIntf,    // Fenêtres IDE
  Forms, Controls,  // VCL/LCL standard
  Dialogs;
```

## Créer votre premier plugin

### Étape 1 : Créer le package

**Menu : Paquet → Nouveau paquet**

```
Configuration du package :
├── Nom : MonPremierPlugin
├── Type : Design-time only
├── Description : Mon premier plugin Lazarus
└── Version : 0.1.0.0
```

### Étape 2 : Structure de base

Créez une unité principale `MonPlugin.pas` :

```pascal
unit MonPlugin;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs,
  IDEIntf, MenuIntf, ToolBarIntf;

type
  { TMonPlugin }
  TMonPlugin = class
  private
    FMenuItemMonPlugin: TIDEMenuCommand;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ExecuteMonAction(Sender: TObject);
  end;

var
  MonPlugin: TMonPlugin;

procedure Register;

implementation

procedure Register;  
begin
  // Point d'entrée du plugin
  MonPlugin := TMonPlugin.Create;
end;

{ TMonPlugin }

constructor TMonPlugin.Create;  
begin
  // Ajouter un élément au menu Outils
  FMenuItemMonPlugin := RegisterIDEMenuCommand(
    itmSecondaryTools,           // Parent (menu Outils)
    'MonPluginCommand',          // Nom unique
    'Mon Plugin Action',         // Texte affiché
    nil,                        // Raccourci clavier
    nil,                        // Icône
    @ExecuteMonAction           // Méthode à exécuter
  );
end;

destructor TMonPlugin.Destroy;  
begin
  // Nettoyage si nécessaire
  inherited Destroy;
end;

procedure TMonPlugin.ExecuteMonAction(Sender: TObject);  
begin
  ShowMessage('Mon premier plugin fonctionne !');
end;

end.
```

### Étape 3 : Configurer le package

Dans l'éditeur de package :

1. **Ajouter le fichier** : MonPlugin.pas
2. **Ajouter les dépendances** :
   - IDEIntf
   - LCL
   - FCL

3. **Options du package** :
```
Options → IDE Integration :
├── Type : IDE Plugin
├── Register procedure : Register
└── ☑ Rebuild IDE automatically
```

### Étape 4 : Installer le plugin

1. **Compiler** : Bouton "Compiler" dans l'éditeur de package
2. **Installer** : Bouton "Installer"
3. Lazarus va se recompiler et redémarrer
4. Votre plugin est maintenant dans le menu Outils !

## Ajouter des fonctionnalités

### Ajouter un élément de menu

```pascal
procedure AddMenuItem;  
var
  MenuItem: TIDEMenuCommand;
begin
  // Ajouter dans le menu Fichier
  MenuItem := RegisterIDEMenuCommand(
    itmFileNew,                    // Après "Nouveau"
    'cmdMyNewFile',                // Identifiant unique
    'Nouveau fichier spécial...',  // Texte
    nil,                           // Pas de raccourci
    nil,                           // Pas d'icône
    @CreateSpecialFile             // Action
  );

  // Avec raccourci clavier
  MenuItem := RegisterIDEMenuCommand(
    itmSourceEditor,
    'cmdFormatSpecial',
    'Format spécial',
    CleanIDEShortCut(VK_F, [ssCtrl, ssShift]), // Ctrl+Shift+F
    nil,
    @FormatSpecial
  );
end;
```

### Ajouter un bouton dans la barre d'outils

```pascal
uses ToolBarIntf, Graphics;

procedure AddToolBarButton;  
var
  ToolButton: TIDEButtonCommand;
  Bitmap: TBitmap;
begin
  // Créer l'icône
  Bitmap := TBitmap.Create;
  try
    Bitmap.LoadFromFile('icon.bmp');
    // ou créer programmatiquement
    Bitmap.Width := 16;
    Bitmap.Height := 16;
    Bitmap.Canvas.Brush.Color := clBlue;
    Bitmap.Canvas.FillRect(0, 0, 16, 16);

    // Créer le bouton
    ToolButton := RegisterIDEButtonCommand(
      'MyToolButton',              // Nom
      'Mon bouton',                // Hint
      'Cliquez pour l''action',    // Description
      Bitmap,                      // Icône
      @MyButtonClick              // Action
    );

    // L'ajouter à une barre d'outils
    ToolButton.ToolBar := tbMain;  // Barre principale
  finally
    Bitmap.Free;
  end;
end;
```

### Créer une fenêtre personnalisée

```pascal
unit MyPluginWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, Controls, StdCtrls, IDEWindowIntf;

type
  { TMyPluginForm }
  TMyPluginForm = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  MyPluginForm: TMyPluginForm;

procedure ShowMyPluginWindow;

implementation

procedure ShowMyPluginWindow;  
begin
  if MyPluginForm = nil then
  begin
    MyPluginForm := TMyPluginForm.Create(Application);
    // Enregistrer comme fenêtre IDE
    IDEWindowCreators.Add(
      'MyPluginWindow',           // Nom unique
      @CreateMyPluginWindow,      // Créateur
      'Mon Plugin',               // Titre
      'Fenêtre de mon plugin',    // Description
      [iwcfMulti]                  // Flags
    );
  end;
  MyPluginForm.Show;
end;

constructor TMyPluginForm.Create(AOwner: TComponent);  
begin
  inherited Create(AOwner);
  Caption := 'Mon Plugin';
  Width := 400;
  Height := 300;

  Memo1 := TMemo.Create(Self);
  Memo1.Parent := Self;
  Memo1.Align := alClient;

  Button1 := TButton.Create(Self);
  Button1.Parent := Self;
  Button1.Caption := 'Analyser';
  Button1.Align := alBottom;
  Button1.OnClick := @Button1Click;
end;

procedure TMyPluginForm.Button1Click(Sender: TObject);  
begin
  Memo1.Lines.Add('Analyse en cours...');
  // Votre code d'analyse
end;

end.
```

## Interagir avec l'éditeur de code

### Accéder au code source actuel

```pascal
uses SrcEditorIntf;

procedure AnalyzeCurrentSource;  
var
  SourceEditor: TSourceEditorInterface;
  Source: string;
  Line, Col: Integer;
begin
  // Obtenir l'éditeur actif
  SourceEditor := SourceEditorManagerIntf.ActiveEditor;
  if SourceEditor = nil then
  begin
    ShowMessage('Aucun fichier ouvert');
    Exit;
  end;

  // Obtenir le texte complet
  Source := SourceEditor.GetText(False);

  // Position du curseur
  Line := SourceEditor.CursorTextXY.Y;
  Col := SourceEditor.CursorTextXY.X;

  // Nom du fichier
  ShowMessage('Fichier : ' + SourceEditor.FileName);
  ShowMessage('Position : Ligne ' + IntToStr(Line) +
              ', Colonne ' + IntToStr(Col));
end;
```

### Modifier le code source

```pascal
procedure InsertCodeAtCursor;  
var
  SourceEditor: TSourceEditorInterface;
  CodeToInsert: string;
begin
  SourceEditor := SourceEditorManagerIntf.ActiveEditor;
  if SourceEditor = nil then Exit;

  CodeToInsert := '// Code inséré par mon plugin' + LineEnding;

  // Insérer à la position du curseur
  SourceEditor.InsertTextAtCaret(CodeToInsert);

  // Ou remplacer une sélection
  if SourceEditor.SelectionAvailable then
    SourceEditor.ReplaceSelection(CodeToInsert);
end;
```

### Ajouter des marqueurs dans l'éditeur

```pascal
procedure AddBookmark;  
var
  SourceEditor: TSourceEditorInterface;
  Line: Integer;
begin
  SourceEditor := SourceEditorManagerIntf.ActiveEditor;
  if SourceEditor = nil then Exit;

  Line := SourceEditor.CursorTextXY.Y;

  // Ajouter un bookmark
  SourceEditor.SetBookmark(1, Line);

  // Ajouter un point d'arrêt
  SourceEditor.ToggleBreakpoint(Line);
end;
```

## Interagir avec le projet

### Accéder aux informations du projet

```pascal
uses ProjectIntf, LazIDEIntf;

procedure ShowProjectInfo;  
var
  Project: TLazProject;
  i: Integer;
  FileItem: TLazProjectFile;
begin
  // Obtenir le projet actuel
  Project := LazarusIDE.ActiveProject;
  if Project = nil then
  begin
    ShowMessage('Aucun projet ouvert');
    Exit;
  end;

  ShowMessage('Projet : ' + Project.ProjectInfoFile);
  ShowMessage('Titre : ' + Project.Title);
  ShowMessage('Répertoire : ' + Project.Directory);

  // Lister les fichiers
  for i := 0 to Project.FileCount - 1 do
  begin
    FileItem := Project.Files[i];
    ShowMessage('Fichier : ' + FileItem.Filename);
  end;
end;
```

### Modifier les options du projet

```pascal
procedure SetProjectOptions;  
var
  Project: TLazProject;
begin
  Project := LazarusIDE.ActiveProject;
  if Project = nil then Exit;

  // Modifier le titre
  Project.Title := 'Nouveau titre';

  // Ajouter un chemin de recherche
  Project.CompilerOptions.IncludePath.Add('$(ProjPath)/include');

  // Définir le mode de compilation
  Project.BuildModes.ActiveMode.CompilerOptions.OptimizationLevel := 2;

  // Sauvegarder les modifications
  Project.Modified := True;
  LazarusIDE.DoSaveProject;
end;
```

## Créer des assistants (Wizards)

### Assistant de nouveau fichier

```pascal
unit MyFileWizard;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, Controls, StdCtrls, EditBtn,
  NewItemIntf, ProjectIntf;

type
  { TMyFileWizard }
  TMyFileWizard = class(TNewItemProject)
  public
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    procedure Execute(var Filename: string;
                     var Source: string); override;
  end;

  { TMyWizardForm }
  TMyWizardForm = class(TForm)
    EdtClassName: TEdit;
    EdtNamespace: TEdit;
    BtnOK: TButton;
    BtnCancel: TButton;
  public
    constructor Create(AOwner: TComponent); override;
  end;

procedure Register;

implementation

procedure Register;  
begin
  RegisterNewItemCategory(TNewItemProject);
  RegisterProjectDescriptor(TMyFileWizard.Create);
end;

{ TMyFileWizard }

function TMyFileWizard.GetLocalizedName: string;  
begin
  Result := 'Ma classe personnalisée';
end;

function TMyFileWizard.GetLocalizedDescription: string;  
begin
  Result := 'Crée une classe avec template personnalisé';
end;

procedure TMyFileWizard.Execute(var Filename: string;
                               var Source: string);
var
  WizardForm: TMyWizardForm;
begin
  WizardForm := TMyWizardForm.Create(nil);
  try
    if WizardForm.ShowModal = mrOK then
    begin
      // Générer le code
      Source := 'unit ' + WizardForm.EdtNamespace.Text + ';' + LineEnding +
                LineEnding +
                'interface' + LineEnding +
                LineEnding +
                'type' + LineEnding +
                '  T' + WizardForm.EdtClassName.Text + ' = class' + LineEnding +
                '  private' + LineEnding +
                '  public' + LineEnding +
                '  end;' + LineEnding +
                LineEnding +
                'implementation' + LineEnding +
                LineEnding +
                'end.';

      Filename := LowerCase(WizardForm.EdtClassName.Text) + '.pas';
    end;
  finally
    WizardForm.Free;
  end;
end;

{ TMyWizardForm }

constructor TMyWizardForm.Create(AOwner: TComponent);  
begin
  inherited Create(AOwner);
  Caption := 'Assistant nouvelle classe';
  Width := 400;
  Height := 200;
  Position := poScreenCenter;

  // Créer les contrôles
  EdtClassName := TEdit.Create(Self);
  EdtClassName.Parent := Self;
  EdtClassName.Text := 'MyClass';
  EdtClassName.Top := 20;
  EdtClassName.Left := 20;
  EdtClassName.Width := 350;

  EdtNamespace := TEdit.Create(Self);
  EdtNamespace.Parent := Self;
  EdtNamespace.Text := 'MyUnit';
  EdtNamespace.Top := 60;
  EdtNamespace.Left := 20;
  EdtNamespace.Width := 350;

  BtnOK := TButton.Create(Self);
  BtnOK.Parent := Self;
  BtnOK.Caption := 'OK';
  BtnOK.ModalResult := mrOK;
  BtnOK.Top := 120;
  BtnOK.Left := 220;

  BtnCancel := TButton.Create(Self);
  BtnCancel.Parent := Self;
  BtnCancel.Caption := 'Annuler';
  BtnCancel.ModalResult := mrCancel;
  BtnCancel.Top := 120;
  BtnCancel.Left := 300;
end;

end.
```

## Utiliser les Code Tools

### Analyser le code Pascal

```pascal
uses CodeToolManager, CodeCache, CodeTree;

procedure AnalyzePascalCode;  
var
  Code: TCodeBuffer;
  Tool: TCodeTool;
  Node: TCodeTreeNode;
begin
  // Obtenir le buffer de code actuel
  Code := CodeToolBoss.GetMainCode;
  if Code = nil then Exit;

  // Parser le code
  if not CodeToolBoss.Explore(Code, Tool, False) then
  begin
    ShowMessage('Erreur de parsing : ' + CodeToolBoss.ErrorMessage);
    Exit;
  end;

  // Parcourir l'arbre syntaxique
  Node := Tool.Tree.Root;
  while Node <> nil do
  begin
    case Node.Desc of
      ctnProcedure:
        ShowMessage('Procédure trouvée : ' + Tool.ExtractProcName(Node));
      ctnVarDefinition:
        ShowMessage('Variable : ' + Tool.ExtractDefinitionName(Node));
      ctnClass:
        ShowMessage('Classe : ' + Tool.ExtractClassName(Node));
    end;
    Node := Node.Next;
  end;
end;
```

### Générer du code automatiquement

```pascal
procedure GenerateProperty;  
var
  SourceEditor: TSourceEditorInterface;
  FieldName, PropName: string;
  Code: string;
begin
  SourceEditor := SourceEditorManagerIntf.ActiveEditor;
  if SourceEditor = nil then Exit;

  // Demander le nom
  FieldName := InputBox('Générateur', 'Nom du champ :', 'FValue');
  PropName := Copy(FieldName, 2, Length(FieldName)); // Retirer le F

  // Générer le code
  Code := Format(
    '  private' + LineEnding +
    '    %s: Integer;' + LineEnding +
    '    procedure Set%s(AValue: Integer);' + LineEnding +
    '  public' + LineEnding +
    '    property %s: Integer read %s write Set%s;' + LineEnding,
    [FieldName, PropName, PropName, FieldName, PropName]
  );

  // Insérer
  SourceEditor.InsertTextAtCaret(Code);

  // Générer l'implémentation avec Code Tools
  CodeToolBoss.CompleteCode(SourceEditor.CodeBuffer);
end;
```

## Créer des éditeurs de propriétés

### Éditeur personnalisé pour l'Object Inspector

```pascal
unit MyPropertyEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, PropEdits, Graphics, Forms, Dialogs;

type
  { TMyColorPropertyEditor }
  TMyColorPropertyEditor = class(TPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
    function GetValue: string; override;
    procedure SetValue(const AValue: string); override;
  end;

procedure Register;

implementation

procedure Register;  
begin
  // Enregistrer l'éditeur pour un type spécifique
  RegisterPropertyEditor(
    TypeInfo(TColor),           // Type
    TMyComponent,                // Classe (nil = toutes)
    'SpecialColor',             // Propriété ('' = toutes)
    TMyColorPropertyEditor      // Éditeur
  );
end;

{ TMyColorPropertyEditor }

function TMyColorPropertyEditor.GetAttributes: TPropertyAttributes;  
begin
  Result := [paDialog, paRevertable];
end;

procedure TMyColorPropertyEditor.Edit;  
var
  ColorDialog: TColorDialog;
begin
  ColorDialog := TColorDialog.Create(nil);
  try
    ColorDialog.Color := GetOrdValue;
    if ColorDialog.Execute then
      SetOrdValue(ColorDialog.Color);
  finally
    ColorDialog.Free;
  end;
end;

function TMyColorPropertyEditor.GetValue: string;  
begin
  Result := ColorToString(GetOrdValue);
end;

procedure TMyColorPropertyEditor.SetValue(const AValue: string);  
begin
  SetOrdValue(StringToColor(AValue));
end;

end.
```

## Gestion des événements IDE

### S'abonner aux événements

```pascal
uses IDECommands, LazIDEIntf;

type
  { TIDEEventHandler }
  TIDEEventHandler = class
  private
    procedure OnProjectOpened(Sender: TObject; AProject: TLazProject);
    procedure OnProjectClosed(Sender: TObject; AProject: TLazProject);
    procedure OnSourceEditorModified(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
  end;

constructor TIDEEventHandler.Create;  
begin
  // S'abonner aux événements
  LazarusIDE.AddHandlerOnProjectOpened(@OnProjectOpened);
  LazarusIDE.AddHandlerOnProjectClosed(@OnProjectClosed);
  SourceEditorManagerIntf.RegisterChangeEvent(semEditorModified,
                                              @OnSourceEditorModified);
end;

destructor TIDEEventHandler.Destroy;  
begin
  // Se désabonner
  LazarusIDE.RemoveHandlerOnProjectOpened(@OnProjectOpened);
  LazarusIDE.RemoveHandlerOnProjectClosed(@OnProjectClosed);
  SourceEditorManagerIntf.UnregisterChangeEvent(semEditorModified,
                                                @OnSourceEditorModified);
  inherited;
end;

procedure TIDEEventHandler.OnProjectOpened(Sender: TObject;
                                          AProject: TLazProject);
begin
  ShowMessage('Projet ouvert : ' + AProject.Title);
end;

procedure TIDEEventHandler.OnProjectClosed(Sender: TObject;
                                          AProject: TLazProject);
begin
  ShowMessage('Projet fermé : ' + AProject.Title);
end;

procedure TIDEEventHandler.OnSourceEditorModified(Sender: TObject);  
begin
  // Code modifié
  // Attention : appelé très souvent !
end;
```

## Debugging et tests

### Déboguer votre plugin

```pascal
uses SysUtils, Classes;

// Technique 1 : Messages de debug
{$IFDEF DEBUG}
procedure DebugLog(const Msg: string);  
begin
  WriteLn('PLUGIN: ' + Msg);
  // Ou dans un fichier
  with TStringList.Create do
  try
    if FileExists('plugin_debug.log') then
      LoadFromFile('plugin_debug.log');
    Add(FormatDateTime('hh:nn:ss', Now) + ' - ' + Msg);
    SaveToFile('plugin_debug.log');
  finally
    Free;
  end;
end;
{$ENDIF}

// Technique 2 : Utiliser une seconde instance
// 1. Compiler Lazarus avec symboles debug
// 2. Lancer Lazarus depuis Lazarus
// 3. Déboguer normalement
```

### Tester le plugin

```pascal
unit TestMyPlugin;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  MyPlugin;

type
  TTestMyPlugin = class(TTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestMenuCreation;
    procedure TestActionExecution;
  end;

implementation

procedure TTestMyPlugin.SetUp;  
begin
  // Initialiser
end;

procedure TTestMyPlugin.TearDown;  
begin
  // Nettoyer
end;

procedure TTestMyPlugin.TestMenuCreation;  
begin
  AssertNotNull('Menu doit être créé', MonPlugin.FMenuItemMonPlugin);
end;

procedure TTestMyPlugin.TestActionExecution;  
begin
  // Tester l'action
  MonPlugin.ExecuteMonAction(nil);
  // Vérifier le résultat
end;

initialization
  RegisterTest(TTestMyPlugin);
end.
```

## Distribution du plugin

### Créer un package distribuable

Structure recommandée :
```
MonPlugin/
├── MonPlugin.lpk          # Package
├── src/
│   ├── MonPlugin.pas      # Source principal
│   └── autres.pas
├── docs/
│   ├── README.md          # Documentation
│   └── INSTALL.md         # Instructions
├── examples/              # Exemples d'utilisation
├── images/               # Icônes et images
└── LICENSE               # Licence
```

### Documentation README.md

```markdown
# Mon Plugin pour Lazarus

## Description
Ce plugin ajoute [fonctionnalité] à l'IDE Lazarus.

## Installation
1. Ouvrir MonPlugin.lpk dans Lazarus
2. Compiler le package
3. Installer (l'IDE va redémarrer)

## Utilisation
- Menu : Outils → Mon Plugin
- Raccourci : Ctrl+Shift+M

## Configuration
Options → Mon Plugin Settings

## Compatibilité
- Lazarus 2.0+
- FPC 3.2+
- Windows, Linux, macOS

## Licence
LGPL v3
```

### Publication sur OPM

Pour publier sur Online Package Manager :

1. **Préparer le package**
   - Version stable
   - Documentation complète
   - Licence claire

2. **Créer un dépôt GitHub/GitLab**

3. **Soumettre à OPM**
   - Fork le dépôt OPM
   - Ajouter votre package
   - Pull Request

## Exemples de plugins populaires

### Anchor Docking
```
Fonctionnalité : Fenêtres ancrables  
Complexité : Élevée  
Apprentissage : Gestion des fenêtres
```

### PascalScript
```
Fonctionnalité : Scripting dans l'IDE  
Complexité : Moyenne  
Apprentissage : Intégration de scripting
```

### Editor Macro Script
```
Fonctionnalité : Macros pour l'éditeur  
Complexité : Moyenne  
Apprentissage : Automatisation éditeur
```

## Bonnes pratiques

### Architecture propre

```pascal
// Séparer les responsabilités
MonPlugin/
├── Core/           # Logique métier
├── UI/            # Interface utilisateur
├── Commands/      # Actions et commandes
└── Utils/         # Utilitaires
```

### Gestion d'erreurs

```pascal
procedure SafeExecute;  
begin
  try
    // Code risqué
    DangerousOperation;
  except
    on E: Exception do
    begin
      // Logger l'erreur
      IDEMessagesWindow.AddCustomMessage(
        mluError,
        'MonPlugin: ' + E.Message
      );
      // Ne pas crasher l'IDE !
    end;
  end;
end;
```

### Performance

```pascal
// Éviter les opérations lourdes dans le thread principal
// Note : les procédures anonymes nécessitent le modeswitch suivant en mode ObjFPC :
{$modeswitch anonymousfunctions}

procedure HeavyOperation;  
begin
  // Utiliser un thread
  TThread.CreateAnonymousThread(
    procedure
    begin
      // Opération lourde
      ProcessBigData;

      // Retour au thread principal
      TThread.Synchronize(nil,
        procedure
        begin
          UpdateUI;
        end
      );
    end
  ).Start;
end;
```

> **Note** : Les procédures anonymes ne sont pas disponibles en mode ObjFPC par défaut.
> Il faut ajouter `{$modeswitch anonymousfunctions}` (FPC 3.3.1+) ou utiliser
> `{$mode delphi}`. Alternativement, créez une sous-classe de `TThread` classique.

## Ressources et documentation

### Documentation officielle

```
Ressources essentielles :
├── Wiki Lazarus
│   └── wiki.freepascal.org/Extending_the_IDE
├── Sources Lazarus
│   └── lazarus/ide/ (exemples)
├── Forum Lazarus
│   └── forum.lazarus.freepascal.org
└── Packages existants
    └── Étudier le code source
```

### Unités de référence

```pascal
// Étudier ces unités pour comprendre l'API
lazarus/ideintf/
├── IDEIntf.pas           # Base
├── MenuIntf.pas          # Menus
├── SrcEditorIntf.pas     # Éditeur
├── ProjectIntf.pas       # Projets
├── PackageIntf.pas       # Packages
└── PropEdits.pas         # Éditeurs de propriétés
```

## Conclusion

Le développement de plugins IDE ouvre des possibilités infinies pour personnaliser et améliorer Lazarus. Vous pouvez automatiser vos tâches répétitives, ajouter des outils spécialisés, ou créer des intégrations avec d'autres systèmes.

**Points clés à retenir :**
- 🎯 Commencez simple avec un menu ou bouton
- 📚 Étudiez les plugins existants
- 🔧 Utilisez l'API IDEIntf
- ⚠️ Gérez les erreurs pour ne pas crasher l'IDE
- 🚀 Partagez vos créations avec la communauté

Le développement de plugins est un excellent moyen d'approfondir votre connaissance de Lazarus tout en créant des outils utiles pour vous et la communauté. Commencez petit, expérimentez, et n'ayez pas peur de regarder le code source de Lazarus lui-même pour comprendre comment les choses fonctionnent !

⏭️ [Cross-compilation Windows↔Linux](/02-maitrise-ide-lazarus/08-cross-compilation-windows-linux.md)
