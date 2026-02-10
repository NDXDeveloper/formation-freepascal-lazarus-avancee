🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 25.4 IDE ou éditeur de code avancé portable

## Introduction

Dans ce chapitre, nous allons explorer la conception et le développement d'un IDE (Integrated Development Environment) ou éditeur de code avancé qui fonctionne de manière identique sur Windows et Ubuntu. Ce type de projet représente un défi architectural majeur qui met en œuvre la plupart des concepts avancés de FreePascal/Lazarus.

Un éditeur de code moderne doit offrir des fonctionnalités sophistiquées tout en restant performant et agréable à utiliser. Nous verrons comment construire un tel outil en tirant parti des capacités multi-plateformes de Lazarus.

## Objectifs du projet

Avant de commencer, définissons les objectifs principaux de notre éditeur :

### Fonctionnalités essentielles
- **Édition multi-onglets** : gestion de plusieurs fichiers simultanément
- **Coloration syntaxique** : support de plusieurs langages de programmation
- **Numérotation des lignes** : avec zones de pliage de code
- **Auto-complétion intelligente** : suggestions contextuelles
- **Recherche et remplacement** : avec expressions régulières
- **Gestion de projets** : arborescence de fichiers

### Fonctionnalités avancées
- **Analyse syntaxique** : détection d'erreurs en temps réel
- **Refactoring** : renommage de symboles, extraction de méthodes
- **Intégration Git** : visualisation des modifications, commits
- **Terminal intégré** : exécution de commandes
- **Thèmes personnalisables** : mode clair et sombre
- **Plugins extensibles** : architecture modulaire

### Exigences multi-plateformes
- **Interface native** : utilisation du widgetset approprié (Win32/GTK)
- **Chemins portables** : gestion uniforme des fichiers
- **Configuration unifiée** : fichiers de configuration compatibles
- **Performances équivalentes** : optimisation pour chaque OS

## Architecture globale

### Structure modulaire

Notre éditeur sera organisé en modules distincts pour faciliter la maintenance et l'évolution :

```
MonEditeur/
├── Core/              (Noyau de l'éditeur)
│   ├── Editor.pas     (Composant d'édition)
│   ├── Document.pas   (Gestion des documents)
│   ├── Project.pas    (Gestion de projets)
│   └── Config.pas     (Configuration)
├── Syntax/            (Coloration syntaxique)
│   ├── Highlighter.pas
│   ├── Parser.pas
│   └── Languages/     (Définitions de langages)
├── UI/                (Interface utilisateur)
│   ├── MainForm.pas
│   ├── TabManager.pas
│   └── Dialogs/
├── Features/          (Fonctionnalités avancées)
│   ├── Completion.pas
│   ├── Search.pas
│   ├── Refactor.pas
│   └── Terminal.pas
├── Plugins/           (Système de plugins)
│   ├── PluginAPI.pas
│   └── PluginManager.pas
└── Platform/          (Code spécifique OS)
    ├── Windows/
    └── Linux/
```

### Séparation des préoccupations

**Principe fondamental** : séparer la logique métier de l'interface graphique et du code spécifique à chaque plateforme.

```pascal
// Interface abstraite pour les opérations système
type
  IPlatformServices = interface
    function GetConfigDirectory: string;
    function GetTempDirectory: string;
    function OpenTerminal(const ADirectory: string): Boolean;
    function GetDefaultShell: string;
  end;

// Implémentation Windows
type
  TWinPlatformServices = class(TInterfacedObject, IPlatformServices)
  public
    function GetConfigDirectory: string;
    // %APPDATA%\MonEditeur
    function GetDefaultShell: string;
    // cmd.exe ou PowerShell
  end;

// Implémentation Linux
type
  TLinuxPlatformServices = class(TInterfacedObject, IPlatformServices)
  public
    function GetConfigDirectory: string;
    // ~/.config/monediteur
    function GetDefaultShell: string;
    // bash ou zsh
  end;
```

## Composant d'édition principal

### Choix du composant de base

Lazarus offre plusieurs options pour créer un éditeur de texte :

1. **TMemo** : composant basique, limité pour un IDE
2. **TSynEdit** : éditeur avec coloration syntaxique intégré
3. **TATSynEdit** : éditeur moderne très performant
4. **Composant personnalisé** : contrôle total mais beaucoup de travail

**Recommandation** : Utiliser **TSynEdit** ou **TATSynEdit** qui offrent déjà de nombreuses fonctionnalités avancées.

### Personnalisation du composant d'édition

```pascal
type
  TCustomCodeEditor = class(TSynEdit)
  private
    FDocument: TDocument;
    FModified: Boolean;
    FLanguage: string;
    procedure OnTextChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure LoadFromFile(const AFileName: string);
    procedure SaveToFile(const AFileName: string);
    procedure ApplyTheme(ATheme: TEditorTheme);

    property Document: TDocument read FDocument;
    property Modified: Boolean read FModified;
    property Language: string read FLanguage write SetLanguage;
  end;
```

### Gestion des documents

Chaque fichier ouvert est représenté par un objet document :

```pascal
type
  TDocument = class
  private
    FFileName: string;
    FEncoding: string;
    FLineEnding: string; // CRLF (Windows) ou LF (Unix)
    FModified: Boolean;
    FLanguage: string;
  public
    constructor Create;
    function DetectEncoding: string;
    function DetectLanguage: string;
    procedure ConvertLineEndings(const ATarget: string);

    property FileName: string read FFileName write FFileName;
    property Modified: Boolean read FModified write FModified;
  end;
```

**Point important** : La détection automatique de l'encodage (UTF-8, ANSI, UTF-16) et des fins de ligne est cruciale pour un éditeur multi-plateforme.

## Coloration syntaxique avancée

### Architecture du système de highlighting

La coloration syntaxique doit être :
- **Rapide** : ne pas ralentir l'édition
- **Extensible** : facile d'ajouter de nouveaux langages
- **Personnalisable** : thèmes de couleurs

```pascal
type
  // Définition abstraite d'un highlighter
  TLanguageHighlighter = class abstract
  protected
    FTokens: array of TTokenType;
    FColors: TTokenColorMap;
  public
    procedure ParseLine(const ALine: string;
                       ATokens: TList); virtual; abstract;
    procedure ApplyColors(AEditor: TSynEdit); virtual;

    class function GetLanguageName: string; virtual; abstract;
    class function GetFileExtensions: TStringArray; virtual; abstract;
  end;

  // Exemple pour Pascal
  TPascalHighlighter = class(TLanguageHighlighter)
  public
    procedure ParseLine(const ALine: string; ATokens: TList); override;
    class function GetLanguageName: string; override;
    // Result := 'Pascal';
    class function GetFileExtensions: TStringArray; override;
    // Result := ['.pas', '.pp', '.lpr', '.inc'];
  end;
```

### Enregistrement des highlighters

Un système de registre permet d'associer automatiquement le bon highlighter selon l'extension du fichier :

```pascal
type
  THighlighterRegistry = class
  private
    class var FHighlighters: TList<TLanguageHighlighterClass>;
  public
    class procedure RegisterHighlighter(AClass: TLanguageHighlighterClass);
    class function FindHighlighterForFile(const AFileName: string): TLanguageHighlighter;
    class function FindHighlighterByName(const AName: string): TLanguageHighlighter;
  end;

// Utilisation
initialization
  THighlighterRegistry.RegisterHighlighter(TPascalHighlighter);
  THighlighterRegistry.RegisterHighlighter(TCHighlighter);
  THighlighterRegistry.RegisterHighlighter(TPythonHighlighter);
end.
```

### Thèmes de couleurs

Les thèmes doivent être stockés dans un format portable (JSON, XML ou INI) :

```pascal
type
  TEditorTheme = class
  private
    FName: string;
    FBackgroundColor: TColor;
    FTextColor: TColor;
    FTokenColors: TDictionary<TTokenType, TColor>;
  public
    procedure LoadFromFile(const AFileName: string);
    procedure SaveToFile(const AFileName: string);
    procedure ApplyToEditor(AEditor: TSynEdit);

    property Name: string read FName;
  end;
```

Exemple de fichier thème (JSON) :

```json
{
  "name": "Dark Modern",
  "background": "#1E1E1E",
  "foreground": "#D4D4D4",
  "tokens": {
    "keyword": "#569CD6",
    "string": "#CE9178",
    "comment": "#6A9955",
    "number": "#B5CEA8",
    "operator": "#D4D4D4"
  }
}
```

## Auto-complétion intelligente

### Principes de l'auto-complétion

L'auto-complétion doit être :
- **Contextuelle** : suggérer uniquement ce qui est pertinent
- **Rapide** : affichage en moins de 100ms
- **Non intrusive** : ne pas gêner la frappe

### Architecture du système de complétion

```pascal
type
  TCompletionItem = record
    Text: string;
    Kind: TCompletionKind; // (ckKeyword, ckFunction, ckVariable...)
    Description: string;
    InsertText: string;
    Icon: TBitmap;
  end;

  TCompletionProvider = class abstract
  public
    procedure GetCompletions(
      const ADocument: TDocument;
      const APosition: TPoint;
      AList: TList<TCompletionItem>
    ); virtual; abstract;
  end;
```

### Analyse du contexte

Pour fournir des suggestions pertinentes, l'éditeur doit analyser :
- La position du curseur
- Le mot en cours de frappe
- Le contexte syntaxique (dans une fonction, une classe, etc.)
- Les symboles disponibles dans la portée actuelle

```pascal
type
  TCodeContext = class
  private
    FCurrentWord: string;
    FCurrentLine: string;
    FScope: TScopeInfo;
  public
    procedure Analyze(const ADocument: TDocument; APosition: TPoint);
    function GetVisibleSymbols: TList<TSymbol>;
    function IsInStringLiteral: Boolean;
    function IsInComment: Boolean;
  end;
```

### Fenêtre de complétion

La fenêtre doit être positionnée intelligemment selon l'OS :

```pascal
procedure TCompletionWindow.Show(AEditor: TSynEdit; APosition: TPoint);
var
  ScreenPos: TPoint;
  WorkArea: TRect;
begin
  // Calculer la position à l'écran
  ScreenPos := AEditor.ClientToScreen(APosition);

  // Obtenir la zone de travail (écran moins barres de tâches)
  {$IFDEF WINDOWS}
  WorkArea := Screen.WorkAreaRect;
  {$ENDIF}
  {$IFDEF LINUX}
  WorkArea := Screen.MonitorFromPoint(ScreenPos).WorkareaRect;
  {$ENDIF}

  // Ajuster si la fenêtre dépasse
  if ScreenPos.Y + Self.Height > WorkArea.Bottom then
    ScreenPos.Y := ScreenPos.Y - Self.Height - 20; // Au-dessus du curseur

  Self.Left := ScreenPos.X;
  Self.Top := ScreenPos.Y;
  Self.Visible := True;
end;
```

## Recherche et remplacement

### Interface de recherche

Une bonne interface de recherche offre :
- Recherche simple et recherche avec expressions régulières
- Sensibilité à la casse
- Mot entier uniquement
- Recherche dans tous les fichiers du projet
- Historique des recherches

```pascal
type
  TSearchOptions = record
    Text: string;
    CaseSensitive: Boolean;
    WholeWord: Boolean;
    UseRegex: Boolean;
    SearchInFiles: Boolean;
    FilePattern: string;
  end;

  TSearchEngine = class
  public
    function FindNext(const AText: string;
                     AOptions: TSearchOptions): TSearchResult;
    function FindAll(const AText: string;
                    AOptions: TSearchOptions): TArray<TSearchResult>;
    function ReplaceNext(const ASearch, AReplace: string;
                        AOptions: TSearchOptions): Boolean;
    function ReplaceAll(const ASearch, AReplace: string;
                       AOptions: TSearchOptions): Integer;
  end;
```

### Expressions régulières multi-plateformes

FreePascal inclut l'unité **RegExpr** qui fonctionne de manière identique sur tous les OS :

```pascal
uses
  RegExpr;

function TSearchEngine.FindWithRegex(const APattern, AText: string): Boolean;
var
  RegEx: TRegExpr;
begin
  RegEx := TRegExpr.Create;
  try
    RegEx.Expression := APattern;
    Result := RegEx.Exec(AText);
  finally
    RegEx.Free;
  end;
end;
```

### Recherche incrémentale

La recherche incrémentale met en surbrillance les résultats pendant la frappe :

```pascal
procedure TMainForm.SearchEditChange(Sender: TObject);
var
  SearchText: string;
begin
  SearchText := SearchEdit.Text;

  if SearchText = '' then
  begin
    ClearHighlights;
    Exit;
  end;

  // Rechercher et mettre en surbrillance
  HighlightAllOccurrences(ActiveEditor, SearchText);
end;
```

## Gestion de projets et arborescence

### Structure de projet

Un projet regroupe plusieurs fichiers et conserve des métadonnées :

```pascal
type
  TProjectFile = class
  private
    FFileName: string;
    FRelativePath: string;
    FIsOpen: Boolean;
  end;

  TProject = class
  private
    FName: string;
    FRootPath: string;
    FFiles: TObjectList<TProjectFile>;
    FActiveFile: TProjectFile;
  public
    procedure LoadFromFile(const AFileName: string);
    procedure SaveToFile(const AFileName: string);
    procedure AddFile(const AFileName: string);
    procedure RemoveFile(AFile: TProjectFile);

    property Name: string read FName write FName;
    property Files: TObjectList<TProjectFile> read FFiles;
  end;
```

### Arborescence de fichiers (TreeView)

L'arborescence doit afficher la structure du projet avec des icônes appropriées :

```pascal
procedure TProjectPanel.BuildFileTree(AProject: TProject);
var
  RootNode, FileNode: TTreeNode;
  ProjectFile: TProjectFile;
begin
  TreeView.BeginUpdate;
  try
    TreeView.Items.Clear;

    // Nœud racine du projet
    RootNode := TreeView.Items.Add(nil, AProject.Name);
    RootNode.ImageIndex := IMG_PROJECT;

    // Ajouter les fichiers
    for ProjectFile in AProject.Files do
    begin
      FileNode := TreeView.Items.AddChild(RootNode,
                                          ProjectFile.FileName);
      FileNode.ImageIndex := GetIconForFile(ProjectFile.FileName);
      FileNode.Data := ProjectFile;
    end;

    RootNode.Expand;
  finally
    TreeView.EndUpdate;
  end;
end;

function TProjectPanel.GetIconForFile(const AFileName: string): Integer;
var
  Ext: string;
begin
  Ext := LowerCase(ExtractFileExt(AFileName));

  case Ext of
    '.pas', '.pp', '.lpr': Result := IMG_PASCAL;
    '.c', '.cpp', '.h': Result := IMG_C;
    '.py': Result := IMG_PYTHON;
    '.txt', '.md': Result := IMG_TEXT;
    else Result := IMG_FILE;
  end;
end;
```

### Surveillance du système de fichiers

Pour détecter les modifications externes (fichier modifié par un autre éditeur), nous utilisons :

**Sous Windows** : l'API `FindFirstChangeNotification`  
**Sous Linux** : inotify  

```pascal
{$IFDEF WINDOWS}
uses
  Windows;

type
  TFileWatcher = class
  private
    FHandle: THandle;
    FDirectory: string;
  public
    constructor Create(const ADirectory: string);
    destructor Destroy; override;
    function CheckForChanges: Boolean;
  end;

constructor TFileWatcher.Create(const ADirectory: string);
begin
  FDirectory := ADirectory;
  FHandle := FindFirstChangeNotification(
    PChar(ADirectory),
    True, // Surveiller les sous-dossiers
    FILE_NOTIFY_CHANGE_LAST_WRITE
  );
end;
{$ENDIF}

{$IFDEF LINUX}
// Utilisation de inotify via l'unité inotify de FPC
uses
  inotify;

type
  TFileWatcher = class
  private
    FInotifyFD: Integer;
    FWatchDescriptor: Integer;
  public
    constructor Create(const ADirectory: string);
    destructor Destroy; override;
    function CheckForChanges: Boolean;
  end;
{$ENDIF}
```

## Terminal intégré

### Intégration d'un terminal

Un terminal intégré améliore grandement la productivité. Les approches diffèrent selon l'OS :

**Windows** : utilisation d'un contrôle qui encapsule cmd.exe ou PowerShell  
**Linux** : utilisation de VTE (Virtual Terminal Emulator) via GTK  

### Approche portable avec redirection de processus

Une solution simple et portable consiste à créer un processus et rediriger ses entrées/sorties :

```pascal
type
  TIntegratedTerminal = class
  private
    FProcess: TProcess;
    FOutputMemo: TMemo;
    FInputEdit: TEdit;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Start;
    procedure ExecuteCommand(const ACommand: string);
    procedure ReadOutput;
  end;

constructor TIntegratedTerminal.Create(AOwner: TComponent);
begin
  inherited Create;

  FProcess := TProcess.Create(nil);
  FProcess.Options := [poUsePipes, poStderrToOutPut];

  {$IFDEF WINDOWS}
  FProcess.Executable := 'cmd.exe';
  {$ENDIF}
  {$IFDEF LINUX}
  FProcess.Executable := '/bin/bash';
  {$ENDIF}
end;

procedure TIntegratedTerminal.ExecuteCommand(const ACommand: string);
var
  Command: string;
begin
  Command := ACommand + LineEnding;
  FProcess.Input.Write(Command[1], Length(Command));

  // Lire la sortie
  ReadOutput;
end;

procedure TIntegratedTerminal.ReadOutput;
var
  Buffer: array[0..1023] of Char;
  Count: Integer;
  Output: string;
begin
  while FProcess.Output.NumBytesAvailable > 0 do
  begin
    Count := FProcess.Output.Read(Buffer, SizeOf(Buffer));
    SetString(Output, Buffer, Count);
    FOutputMemo.Lines.Add(Output);
  end;
end;
```

### Terminal avec coloration

Pour améliorer la lisibilité, on peut interpréter les codes ANSI de couleur :

```pascal
procedure ParseANSIColor(const AText: string; AMemo: TMemo);
const
  ESC = #27;
var
  i: Integer;
  CurrentColor: TColor;
begin
  CurrentColor := clBlack;
  i := 1;

  while i <= Length(AText) do
  begin
    if (AText[i] = ESC) and (AText[i+1] = '[') then
    begin
      // Analyser le code de couleur ANSI
      // Exemple : ESC[31m pour rouge
      CurrentColor := ParseColorCode(AText, i);
    end
    else
    begin
      // Ajouter le caractère avec la couleur actuelle
      AddColoredChar(AMemo, AText[i], CurrentColor);
    end;
    Inc(i);
  end;
end;
```

## Système de plugins extensible

### Architecture de plugins

Un système de plugins permet d'étendre les fonctionnalités sans modifier le code principal :

```pascal
type
  // Interface que tous les plugins doivent implémenter
  IEditorPlugin = interface
    ['{12345678-1234-1234-1234-123456789ABC}']
    function GetName: string;
    function GetVersion: string;
    function GetDescription: string;
    procedure Initialize(AEditor: IEditorApplication);
    procedure Finalize;
  end;

  // Interface d'accès à l'éditeur fournie aux plugins
  IEditorApplication = interface
    function GetActiveDocument: TDocument;
    procedure RegisterCommand(const AName: string; AHandler: TNotifyEvent);
    procedure RegisterMenuItem(const ACaption, ACommand: string);
    procedure ShowMessage(const AText: string);
  end;
```

### Chargement dynamique des plugins

Les plugins sont des bibliothèques dynamiques (DLL sous Windows, .so sous Linux) :

```pascal
type
  TPluginManager = class
  private
    FPlugins: TList<IEditorPlugin>;
    FLibraries: TList<TLibHandle>;
  public
    procedure LoadPluginsFromDirectory(const ADirectory: string);
    procedure UnloadAllPlugins;
  end;

procedure TPluginManager.LoadPluginsFromDirectory(const ADirectory: string);
var
  SearchRec: TSearchRec;
  PluginPath: string;
  LibHandle: TLibHandle;
  GetPluginProc: function: IEditorPlugin;
  Plugin: IEditorPlugin;
begin
  {$IFDEF WINDOWS}
  if FindFirst(ADirectory + '\*.dll', faAnyFile, SearchRec) = 0 then
  {$ENDIF}
  {$IFDEF LINUX}
  if FindFirst(ADirectory + '/*.so', faAnyFile, SearchRec) = 0 then
  {$ENDIF}
  begin
    repeat
      PluginPath := ADirectory + DirectorySeparator + SearchRec.Name;

      // Charger la bibliothèque
      LibHandle := LoadLibrary(PChar(PluginPath));
      if LibHandle <> NilHandle then
      begin
        // Obtenir la fonction d'export
        GetPluginProc := GetProcAddress(LibHandle, 'GetPlugin');
        if Assigned(GetPluginProc) then
        begin
          Plugin := GetPluginProc();
          Plugin.Initialize(FEditorApp);
          FPlugins.Add(Plugin);
          FLibraries.Add(LibHandle);
        end;
      end;
    until FindNext(SearchRec) <> 0;
    FindClose(SearchRec);
  end;
end;
```

### Exemple de plugin simple

```pascal
// Dans un projet de bibliothèque (DLL/SO)
library HelloPlugin;

uses
  EditorPluginAPI;

type
  THelloPlugin = class(TInterfacedObject, IEditorPlugin)
  private
    FEditor: IEditorApplication;
  public
    function GetName: string;
    function GetVersion: string;
    function GetDescription: string;
    procedure Initialize(AEditor: IEditorApplication);
    procedure Finalize;
    procedure OnHelloCommand(Sender: TObject);
  end;

function THelloPlugin.GetName: string;
begin
  Result := 'Hello Plugin';
end;

procedure THelloPlugin.Initialize(AEditor: IEditorApplication);
begin
  FEditor := AEditor;
  FEditor.RegisterCommand('hello', OnHelloCommand);
  FEditor.RegisterMenuItem('Say Hello', 'hello');
end;

procedure THelloPlugin.OnHelloCommand(Sender: TObject);
begin
  FEditor.ShowMessage('Hello from plugin!');
end;

function GetPlugin: IEditorPlugin; stdcall;
begin
  Result := THelloPlugin.Create;
end;

exports
  GetPlugin;

begin
end.
```

## Intégration Git

### Commandes Git de base

L'intégration de Git améliore le workflow de développement :

```pascal
type
  TGitRepository = class
  private
    FRootPath: string;
    function ExecuteGit(const ACommand: string): string;
  public
    constructor Create(const ARootPath: string);

    function IsRepository: Boolean;
    function GetStatus: TGitStatus;
    function GetBranches: TStringList;
    function GetCurrentBranch: string;

    procedure Add(const AFiles: TStringArray);
    procedure Commit(const AMessage: string);
    procedure Push;
    procedure Pull;
    procedure Checkout(const ABranch: string);
  end;

function TGitRepository.ExecuteGit(const ACommand: string): string;
var
  Process: TProcess;
  Output: TStringList;
begin
  Process := TProcess.Create(nil);
  Output := TStringList.Create;
  try
    Process.Executable := 'git';
    Process.Parameters.Add(ACommand);
    Process.CurrentDirectory := FRootPath;
    Process.Options := [poUsePipes, poWaitOnExit];

    Process.Execute;
    Output.LoadFromStream(Process.Output);
    Result := Output.Text;
  finally
    Output.Free;
    Process.Free;
  end;
end;
```

### Visualisation des modifications

Affichage des différences dans l'éditeur :

```pascal
type
  TDiffView = class
  private
    FOriginalText: TStringList;
    FModifiedText: TStringList;
    FDiffEditor: TSynEdit;
  public
    procedure ComputeDiff;
    procedure DisplayDiff;
  end;

procedure TDiffView.DisplayDiff;
var
  i: Integer;
  Line: string;
begin
  FDiffEditor.Lines.Clear;

  for i := 0 to FDiffEditor.Lines.Count - 1 do
  begin
    Line := FDiffEditor.Lines[i];

    if (Length(Line) > 0) and (Line[1] = '+') then
      // Ligne ajoutée - couleur verte
      FDiffEditor.Lines.Objects[i] := TObject(clGreen)
    else if (Length(Line) > 0) and (Line[1] = '-') then
      // Ligne supprimée - couleur rouge
      FDiffEditor.Lines.Objects[i] := TObject(clRed)
    else
      // Ligne inchangée
      FDiffEditor.Lines.Objects[i] := TObject(clBlack);
  end;
end;
```

## Configuration et persistance

### Fichiers de configuration portables

Utiliser un format de configuration identique sur tous les OS :

```pascal
type
  TEditorConfig = class
  private
    FConfigFile: string;
    FSettings: TJSONObject;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Load;
    procedure Save;

    function GetString(const AKey, ADefault: string): string;
    function GetInteger(const AKey: string; ADefault: Integer): Integer;
    function GetBoolean(const AKey: string; ADefault: Boolean): Boolean;

    procedure SetString(const AKey, AValue: string);
    procedure SetInteger(const AKey: string; AValue: Integer);
    procedure SetBoolean(const AKey: string; AValue: Boolean);
  end;

constructor TEditorConfig.Create;
begin
  inherited Create;

  {$IFDEF WINDOWS}
  FConfigFile := GetEnvironmentVariable('APPDATA') +
                 '\MonEditeur\config.json';
  {$ENDIF}
  {$IFDEF LINUX}
  FConfigFile := GetEnvironmentVariable('HOME') +
                 '/.config/monediteur/config.json';
  {$ENDIF}

  FSettings := TJSONObject.Create;
  Load;
end;

procedure TEditorConfig.Load;
var
  FileContent: string;
begin
  if FileExists(FConfigFile) then
  begin
    FileContent := ReadFileToString(FConfigFile);
    FSettings := TJSONObject(GetJSON(FileContent));
  end;
end;

procedure TEditorConfig.Save;
var
  JSONText: string;
begin
  JSONText := FSettings.FormatJSON;
  WriteStringToFile(FConfigFile, JSONText);
end;
```

### Session de travail

Sauvegarder l'état de l'éditeur (fichiers ouverts, positions, etc.) :

```pascal
type
  TEditorSession = class
  private
    FOpenFiles: TStringList;
    FActiveFile: string;
    FCursorPositions: TDictionary<string, TPoint>;
  public
    procedure Save;
    procedure Restore;
  end;

procedure TEditorSession.Save;
var
  SessionData: TJSONObject;
  FilesArray: TJSONArray;
  i: Integer;
begin
  SessionData := TJSONObject.Create;
  try
    // Liste des fichiers ouverts
    FilesArray := TJSONArray.Create;
    for i := 0 to FOpenFiles.Count - 1 do
      FilesArray.Add(FOpenFiles[i]);
    SessionData.Add('openFiles', FilesArray);

    // Fichier actif
    SessionData.Add('activeFile', FActiveFile);

    // Positions du curseur pour chaque fichier
    // ... (sauvegarde des positions)

    // Écrire dans le fichier de session
    WriteJSONToFile(GetSessionFile, SessionData);
  finally
    SessionData.Free;
  end;
end;
```

## Thèmes et apparence personnalisable

### Système de thèmes complet

Un thème doit définir l'apparence de l'ensemble de l'interface :

```pascal
type
  TUITheme = class
  private
    FName: string;
    FDark: Boolean;

    // Couleurs générales
    FBackgroundColor: TColor;
    FForegroundColor: TColor;
    FBorderColor: TColor;

    // Couleurs de l'éditeur
    FEditorBackground: TColor;
    FEditorForeground: TColor;
    FEditorLineHighlight: TColor;
    FEditorSelection: TColor;

    // Couleurs des éléments d'interface
    FMenuBackground: TColor;
    FMenuForeground: TColor;
    FToolbarBackground: TColor;
    FButtonBackground: TColor;
    FButtonHover: TColor;

    // Couleurs syntaxiques
    FKeywordColor: TColor;
    FStringColor: TColor;
    FCommentColor: TColor;
    FNumberColor: TColor;
    FOperatorColor: TColor;
    FIdentifierColor: TColor;
  public
    constructor Create;
    procedure LoadFromFile(const AFileName: string);
    procedure SaveToFile(const AFileName: string);
    procedure ApplyToApplication;

    property Name: string read FName write FName;
    property IsDark: Boolean read FDark write FDark;
  end;

procedure TUITheme.ApplyToApplication;
begin
  // Appliquer aux formulaires principaux
  Application.MainForm.Color := FBackgroundColor;

  // Appliquer à tous les éditeurs ouverts
  ApplyToEditors;

  // Appliquer aux panneaux
  ApplyToPanels;

  // Appliquer aux menus et barres d'outils
  ApplyToMenus;
end;

procedure TUITheme.ApplyToEditors;
var
  i: Integer;
  Editor: TSynEdit;
begin
  for i := 0 to EditorManager.EditorCount - 1 do
  begin
    Editor := EditorManager.GetEditor(i);

    Editor.Color := FEditorBackground;
    Editor.Font.Color := FEditorForeground;
    Editor.SelectedColor.Background := FEditorSelection;

    // Appliquer les couleurs syntaxiques au highlighter
    if Assigned(Editor.Highlighter) then
    begin
      Editor.Highlighter.KeywordAttribute.Foreground := FKeywordColor;
      Editor.Highlighter.StringAttribute.Foreground := FStringColor;
      Editor.Highlighter.CommentAttribute.Foreground := FCommentColor;
      Editor.Highlighter.NumberAttribute.Foreground := FNumberColor;
    end;
  end;
end;
```

### Thèmes prédéfinis

Fournir plusieurs thèmes populaires :

```pascal
function CreateDefaultTheme: TUITheme;
begin
  Result := TUITheme.Create;
  Result.Name := 'Default Light';
  Result.IsDark := False;
  Result.FBackgroundColor := clWhite;
  Result.FForegroundColor := clBlack;
  Result.FEditorBackground := clWhite;
  Result.FKeywordColor := clBlue;
  Result.FStringColor := clMaroon;
  Result.FCommentColor := clGreen;
end;

function CreateDarkTheme: TUITheme;
begin
  Result := TUITheme.Create;
  Result.Name := 'Dark Modern';
  Result.IsDark := True;
  Result.FBackgroundColor := RGBToColor(30, 30, 30);
  Result.FForegroundColor := RGBToColor(212, 212, 212);
  Result.FEditorBackground := RGBToColor(30, 30, 30);
  Result.FKeywordColor := RGBToColor(86, 156, 214);
  Result.FStringColor := RGBToColor(206, 145, 120);
  Result.FCommentColor := RGBToColor(106, 153, 85);
end;

function CreateMonokaiTheme: TUITheme;
begin
  Result := TUITheme.Create;
  Result.Name := 'Monokai';
  Result.IsDark := True;
  Result.FBackgroundColor := RGBToColor(39, 40, 34);
  Result.FForegroundColor := RGBToColor(248, 248, 242);
  Result.FKeywordColor := RGBToColor(249, 38, 114);
  Result.FStringColor := RGBToColor(230, 219, 116);
  Result.FCommentColor := RGBToColor(117, 113, 94);
end;
```

### Adaptation selon l'OS

Certains éléments visuels doivent s'adapter à l'OS :

```pascal
procedure TUITheme.ApplyPlatformSpecificSettings;
begin
  {$IFDEF WINDOWS}
  // Sous Windows, utiliser les styles visuels natifs si disponibles
  if ThemeServices.ThemesEnabled then
  begin
    Application.Style := 'Windows10';
  end;
  {$ENDIF}

  {$IFDEF LINUX}
  // Sous Linux/GTK, respecter le thème système si souhaité
  if ShouldUseSystemTheme then
  begin
    InheritSystemColors;
  end;
  {$ENDIF}
end;
```

## Performances et optimisations

### Rendu optimisé

Pour un éditeur fluide, le rendu doit être optimisé :

```pascal
type
  TOptimizedEditor = class(TSynEdit)
  private
    FDirtyRect: TRect;
    FNeedsFullRepaint: Boolean;
  protected
    procedure Paint; override;
    procedure InvalidateLines(AFirstLine, ALastLine: Integer);
  public
    procedure OptimizedRepaint;
  end;

procedure TOptimizedEditor.Paint;
begin
  // Ne repeindre que les zones modifiées
  if not FNeedsFullRepaint then
  begin
    Canvas.ClipRect := FDirtyRect;
  end;

  inherited Paint;

  FNeedsFullRepaint := False;
end;

procedure TOptimizedEditor.InvalidateLines(AFirstLine, ALastLine: Integer);
var
  R: TRect;
begin
  // Calculer le rectangle à redessiner
  R.Top := LineToPixels(AFirstLine);
  R.Bottom := LineToPixels(ALastLine + 1);
  R.Left := 0;
  R.Right := Width;

  FDirtyRect := R;
  Invalidate;
end;
```

### Chargement paresseux (Lazy Loading)

Pour les gros fichiers, charger le contenu progressivement :

```pascal
type
  TLargeFileEditor = class
  private
    FFileName: string;
    FFileSize: Int64;
    FLoadedChunks: TList<TFileChunk>;
    FChunkSize: Integer;
  public
    constructor Create;
    procedure LoadChunk(AChunkIndex: Integer);
    procedure UnloadChunk(AChunkIndex: Integer);
    function GetLine(ALineNumber: Integer): string;
  end;

constructor TLargeFileEditor.Create;
begin
  inherited Create;
  FChunkSize := 1024 * 1024; // 1 Mo par chunk
  FLoadedChunks := TList<TFileChunk>.Create;
end;

function TLargeFileEditor.GetLine(ALineNumber: Integer): string;
var
  ChunkIndex: Integer;
  Chunk: TFileChunk;
begin
  // Déterminer dans quel chunk se trouve cette ligne
  ChunkIndex := ALineNumber div LinesPerChunk;

  // Charger le chunk si nécessaire
  Chunk := FindOrLoadChunk(ChunkIndex);

  // Retourner la ligne du chunk
  Result := Chunk.GetLine(ALineNumber mod LinesPerChunk);
end;
```

### Cache de coloration syntaxique

Mettre en cache les résultats du highlighting :

```pascal
type
  TSyntaxCache = class
  private
    FCache: TDictionary<Integer, TLineTokens>;
    FMaxCacheSize: Integer;
  public
    constructor Create(AMaxSize: Integer);
    function GetTokens(ALineNumber: Integer): TLineTokens;
    procedure SetTokens(ALineNumber: Integer; ATokens: TLineTokens);
    procedure InvalidateLine(ALineNumber: Integer);
    procedure Clear;
  end;

function TSyntaxCache.GetTokens(ALineNumber: Integer): TLineTokens;
begin
  if not FCache.TryGetValue(ALineNumber, Result) then
  begin
    // Pas en cache, doit être recalculé
    Result := nil;
  end;
end;

procedure TSyntaxCache.SetTokens(ALineNumber: Integer; ATokens: TLineTokens);
begin
  // Limiter la taille du cache
  if FCache.Count >= FMaxCacheSize then
  begin
    // Supprimer les entrées les plus anciennes (stratégie LRU)
    RemoveOldestEntries;
  end;

  FCache.AddOrSetValue(ALineNumber, ATokens);
end;
```

### Multithreading pour les tâches lourdes

Utiliser des threads pour ne pas bloquer l'interface :

```pascal
type
  TBackgroundTask = class(TThread)
  private
    FOnComplete: TNotifyEvent;
  protected
    procedure Execute; override;
    procedure SyncComplete;
  public
    constructor Create(AOnComplete: TNotifyEvent);
  end;

  TSyntaxAnalyzer = class(TBackgroundTask)
  private
    FDocument: TDocument;
    FResults: TAnalysisResults;
  protected
    procedure Execute; override;
  end;

procedure TSyntaxAnalyzer.Execute;
begin
  // Analyse syntaxique en arrière-plan
  FResults := AnalyzeDocument(FDocument);

  // Synchroniser avec le thread principal pour afficher les résultats
  Synchronize(@SyncComplete);
end;

// Utilisation
procedure TMainForm.AnalyzeCurrentDocument;
var
  Analyzer: TSyntaxAnalyzer;
begin
  Analyzer := TSyntaxAnalyzer.Create(@OnAnalysisComplete);
  Analyzer.FDocument := CurrentDocument;
  Analyzer.Start;
end;
```

## Accessibilité

### Support des lecteurs d'écran

Rendre l'éditeur accessible aux utilisateurs malvoyants :

```pascal
type
  TAccessibleEditor = class(TSynEdit)
  private
    FAccessibleName: string;
    FAccessibleDescription: string;
  protected
    procedure SetAccessibleInfo;
  public
    property AccessibleName: string read FAccessibleName write FAccessibleName;
    property AccessibleDescription: string read FAccessibleDescription
                                          write FAccessibleDescription;
  end;

procedure TAccessibleEditor.SetAccessibleInfo;
begin
  {$IFDEF WINDOWS}
  // Utiliser MSAA (Microsoft Active Accessibility)
  Self.AccessibleRole := ROLE_SYSTEM_TEXT;
  Self.AccessibleName := FAccessibleName;
  Self.AccessibleDescription := FAccessibleDescription;
  {$ENDIF}

  {$IFDEF LINUX}
  // Utiliser ATK (Accessibility Toolkit) via GTK
  if Assigned(Self.Handle) then
  begin
    gtk_widget_set_accessible_name(Self.Handle, PChar(FAccessibleName));
    gtk_widget_set_accessible_description(Self.Handle,
                                          PChar(FAccessibleDescription));
  end;
  {$ENDIF}
end;
```

### Navigation au clavier

Toutes les fonctionnalités doivent être accessibles au clavier :

```pascal
procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  // Raccourcis clavier standards
  if ssCtrl in Shift then
  begin
    case Key of
      Ord('N'): ActionNewFile.Execute;      // Nouveau fichier
      Ord('O'): ActionOpenFile.Execute;     // Ouvrir
      Ord('S'): ActionSaveFile.Execute;     // Sauvegarder
      Ord('F'): ActionFind.Execute;         // Rechercher
      Ord('H'): ActionReplace.Execute;      // Remplacer
      Ord('W'): ActionCloseTab.Execute;     // Fermer l'onglet
      Ord('Z'): ActionUndo.Execute;         // Annuler
      Ord('Y'): ActionRedo.Execute;         // Refaire
    end;
  end;

  // Navigation entre onglets
  if (ssCtrl in Shift) and (Key = VK_TAB) then
  begin
    if ssShift in Shift then
      SwitchToPreviousTab
    else
      SwitchToNextTab;
    Key := 0;
  end;
end;
```

### Contraste et tailles de police

Permettre l'ajustement de la taille du texte :

```pascal
procedure TMainForm.IncreaseFontSize;
var
  i: Integer;
begin
  for i := 0 to PageControl.PageCount - 1 do
  begin
    with GetEditorFromPage(PageControl.Pages[i]) do
    begin
      Font.Size := Font.Size + 1;
      Gutter.Font.Size := Gutter.Font.Size + 1;
    end;
  end;

  SaveFontSizeToConfig;
end;

procedure TMainForm.DecreaseFontSize;
var
  i: Integer;
begin
  for i := 0 to PageControl.PageCount - 1 do
  begin
    with GetEditorFromPage(PageControl.Pages[i]) do
    begin
      if Font.Size > 6 then  // Taille minimale
      begin
        Font.Size := Font.Size - 1;
        Gutter.Font.Size := Gutter.Font.Size - 1;
      end;
    end;
  end;

  SaveFontSizeToConfig;
end;
```

## Support High-DPI et mise à l'échelle

### Détection de la mise à l'échelle

Détecter et s'adapter aux écrans haute résolution :

```pascal
function GetScreenScaleFactor: Double;
{$IFDEF WINDOWS}
var
  DC: HDC;
  LogPixelsX: Integer;
begin
  DC := GetDC(0);
  try
    LogPixelsX := GetDeviceCaps(DC, LOGPIXELSX);
    Result := LogPixelsX / 96.0;  // 96 DPI = 100%
  finally
    ReleaseDC(0, DC);
  end;
end;
{$ENDIF}

{$IFDEF LINUX}
begin
  // Sous GTK, obtenir le facteur d'échelle
  Result := gdk_screen_get_resolution(gdk_screen_get_default) / 96.0;
end;
{$ENDIF}

procedure TMainForm.AdjustForDPI;
var
  ScaleFactor: Double;
begin
  ScaleFactor := GetScreenScaleFactor;

  if ScaleFactor > 1.0 then
  begin
    // Ajuster les tailles de police
    Font.Height := Round(Font.Height * ScaleFactor);

    // Ajuster les icônes de barre d'outils
    LoadScaledIcons(ScaleFactor);

    // Ajuster les marges et espacements
    AdjustLayoutForScale(ScaleFactor);
  end;
end;
```

### Images et icônes vectorielles

Utiliser des icônes SVG qui s'adaptent à toutes les résolutions :

```pascal
type
  TSVGIconManager = class
  private
    FSVGCache: TDictionary<string, TBitmap>;
  public
    function GetIcon(const AName: string; ASize: Integer): TBitmap;
    procedure LoadSVGIcon(const AFileName: string; ASize: Integer;
                         out ABitmap: TBitmap);
  end;

procedure TSVGIconManager.LoadSVGIcon(const AFileName: string;
  ASize: Integer; out ABitmap: TBitmap);
var
  SVG: TSVG;
begin
  SVG := TSVG.Create;
  try
    SVG.LoadFromFile(AFileName);

    ABitmap := TBitmap.Create;
    ABitmap.SetSize(ASize, ASize);

    // Rendre le SVG à la taille demandée
    SVG.PaintTo(ABitmap.Canvas, 0, 0, ASize, ASize);
  finally
    SVG.Free;
  end;
end;
```

## Gestion des erreurs et stabilité

### Gestion globale des exceptions

Intercepter toutes les exceptions pour éviter les plantages :

```pascal
procedure TMainForm.ApplicationException(Sender: TObject; E: Exception);
var
  ErrorMsg: string;
  LogFile: TextFile;
begin
  ErrorMsg := Format('[%s] %s: %s',
    [FormatDateTime('yyyy-mm-dd hh:nn:ss', Now),
     E.ClassName,
     E.Message]);

  // Logger l'erreur
  try
    AssignFile(LogFile, GetErrorLogPath);
    if FileExists(GetErrorLogPath) then
      Append(LogFile)
    else
      Rewrite(LogFile);

    WriteLn(LogFile, ErrorMsg);
    WriteLn(LogFile, GetStackTrace);
    CloseFile(LogFile);
  except
    // Si même le logging échoue, ne rien faire
  end;

  // Afficher un message à l'utilisateur
  ShowMessage('Une erreur s''est produite : ' + E.Message + sLineBreak +
              'Détails enregistrés dans ' + GetErrorLogPath);
end;

// Dans l'initialisation du formulaire
procedure TMainForm.FormCreate(Sender: TObject);
begin
  Application.OnException := @ApplicationException;
end;
```

### Sauvegarde automatique

Éviter la perte de données en cas de plantage :

```pascal
type
  TAutoSaveManager = class
  private
    FTimer: TTimer;
    FInterval: Integer;
    FBackupDirectory: string;
  public
    constructor Create(AInterval: Integer);
    destructor Destroy; override;
    procedure OnTimer(Sender: TObject);
    procedure SaveAllModifiedDocuments;
    function RestoreFromBackup: TStringList;
  end;

procedure TAutoSaveManager.SaveAllModifiedDocuments;
var
  i: Integer;
  Editor: TCustomCodeEditor;
  BackupFile: string;
begin
  for i := 0 to EditorManager.EditorCount - 1 do
  begin
    Editor := EditorManager.GetEditor(i);

    if Editor.Modified then
    begin
      // Créer un nom de fichier de backup
      BackupFile := FBackupDirectory + DirectorySeparator +
                    ExtractFileName(Editor.Document.FileName) +
                    '.autosave';

      try
        Editor.SaveToFile(BackupFile);
      except
        on E: Exception do
          // Logger mais ne pas interrompre
          LogError('AutoSave failed: ' + E.Message);
      end;
    end;
  end;
end;

function TAutoSaveManager.RestoreFromBackup: TStringList;
var
  SearchRec: TSearchRec;
begin
  Result := TStringList.Create;

  if FindFirst(FBackupDirectory + DirectorySeparator + '*.autosave',
               faAnyFile, SearchRec) = 0 then
  begin
    repeat
      Result.Add(FBackupDirectory + DirectorySeparator + SearchRec.Name);
    until FindNext(SearchRec) <> 0;
    FindClose(SearchRec);
  end;
end;
```

## Déploiement et distribution

### Packaging Windows

Créer un installateur avec Inno Setup :

```pascal
; Script Inno Setup
[Setup]
AppName=MonEditeur
AppVersion=1.0
DefaultDirName={pf}\MonEditeur
DefaultGroupName=MonEditeur
OutputDir=Output
OutputBaseFilename=MonEditeur-Setup-Win64

[Files]
Source: "bin\Windows\MonEditeur.exe"; DestDir: "{app}"
Source: "bin\Windows\*.dll"; DestDir: "{app}"
Source: "themes\*"; DestDir: "{app}\themes"; Flags: recursesubdirs
Source: "plugins\*"; DestDir: "{app}\plugins"; Flags: recursesubdirs

[Icons]
Name: "{group}\MonEditeur"; Filename: "{app}\MonEditeur.exe"
Name: "{commondesktop}\MonEditeur"; Filename: "{app}\MonEditeur.exe"

[Registry]
; Association de fichiers
Root: HKCR; Subkey: ".txt"; ValueType: string; ValueData: "MonEditeur.TextFile"
Root: HKCR; Subkey: "MonEditeur.TextFile"; ValueType: string; ValueData: "Text File"
Root: HKCR; Subkey: "MonEditeur.TextFile\DefaultIcon"; ValueType: string;
     ValueData: "{app}\MonEditeur.exe,0"
Root: HKCR; Subkey: "MonEditeur.TextFile\shell\open\command"; ValueType: string;
     ValueData: """{app}\MonEditeur.exe"" ""%1"""
```

### Packaging Linux (DEB)

Créer un paquet Debian :

```bash
# Structure du paquet
monediteur_1.0-1_amd64/
├── DEBIAN/
│   ├── control
│   ├── postinst
│   └── prerm
└── usr/
    ├── bin/
    │   └── monediteur
    ├── share/
    │   ├── applications/
    │   │   └── monediteur.desktop
    │   ├── icons/
    │   │   └── hicolor/
    │   │       └── 256x256/
    │   │           └── apps/
    │   │               └── monediteur.png
    │   └── monediteur/
    │       ├── themes/
    │       └── plugins/
```

Fichier `DEBIAN/control` :

```
Package: monediteur
Version: 1.0-1
Section: editors
Priority: optional
Architecture: amd64
Depends: libgtk-3-0, libglib2.0-0
Maintainer: Votre Nom <email@example.com>
Description: Éditeur de code moderne et portable
 MonEditeur est un éditeur de code avancé avec coloration
 syntaxique, auto-complétion et support de multiples langages.
```

Fichier `.desktop` pour l'intégration au bureau :

```ini
[Desktop Entry]
Type=Application
Name=MonEditeur
Comment=Éditeur de code avancé
Exec=monediteur %F
Icon=monediteur
Terminal=false
Categories=Development;TextEditor;
MimeType=text/plain;text/x-pascal;text/x-c;
```

### AppImage (distribution universelle Linux)

Créer un AppImage portable :

```bash
#!/bin/bash
# build-appimage.sh

APP_NAME="MonEditeur"
APP_DIR="${APP_NAME}.AppDir"

# Créer la structure
mkdir -p "${APP_DIR}/usr/bin"
mkdir -p "${APP_DIR}/usr/lib"
mkdir -p "${APP_DIR}/usr/share/applications"
mkdir -p "${APP_DIR}/usr/share/icons"

# Copier l'exécutable et les dépendances
cp bin/Linux/monediteur "${APP_DIR}/usr/bin/"
ldd bin/Linux/monediteur | grep "=> /" | awk '{print $3}' | \
    xargs -I '{}' cp -v '{}' "${APP_DIR}/usr/lib/"

# Créer le fichier AppRun
cat > "${APP_DIR}/AppRun" << 'EOF'
#!/bin/bash
SELF=$(readlink -f "$0")
HERE=${SELF%/*}
export LD_LIBRARY_PATH="${HERE}/usr/lib:${LD_LIBRARY_PATH}"
exec "${HERE}/usr/bin/monediteur" "$@"
EOF

chmod +x "${APP_DIR}/AppRun"

# Copier les icônes et fichier .desktop
cp resources/monediteur.desktop "${APP_DIR}/"
cp resources/monediteur.png "${APP_DIR}/"

# Créer l'AppImage
appimagetool "${APP_DIR}" MonEditeur-x86_64.AppImage
```

### Distribution multi-plateforme

Script de build automatisé pour tous les OS :

```pascal
program BuildAll;

{$mode objfpc}{$H+}

uses
  SysUtils, Process;

procedure ExecuteCommand(const ACommand: string);
var
  Process: TProcess;
begin
  WriteLn('Executing: ', ACommand);
  Process := TProcess.Create(nil);
  try
    Process.CommandLine := ACommand;
    Process.Options := [poWaitOnExit];
    Process.Execute;
    WriteLn('Exit code: ', Process.ExitStatus);
  finally
    Process.Free;
  end;
end;

procedure BuildWindows;
begin
  WriteLn('Building for Windows...');
  {$IFDEF WINDOWS}
  ExecuteCommand('lazbuild --build-mode=Release-Win64 MonEditeur.lpi');
  {$ELSE}
  ExecuteCommand('lazbuild --build-mode=Release-Win64 ' +
                '--os=win64 --cpu=x86_64 MonEditeur.lpi');
  {$ENDIF}
  WriteLn('Windows build complete.');
end;

procedure BuildLinux;
begin
  WriteLn('Building for Linux...');
  {$IFDEF LINUX}
  ExecuteCommand('lazbuild --build-mode=Release-Linux64 MonEditeur.lpi');
  {$ELSE}
  ExecuteCommand('lazbuild --build-mode=Release-Linux64 ' +
                '--os=linux --cpu=x86_64 MonEditeur.lpi');
  {$ENDIF}
  WriteLn('Linux build complete.');
end;

procedure CreatePackages;
begin
  WriteLn('Creating distribution packages...');

  {$IFDEF WINDOWS}
  ExecuteCommand('iscc setup-windows.iss');
  {$ENDIF}

  {$IFDEF LINUX}
  ExecuteCommand('./build-deb.sh');
  ExecuteCommand('./build-appimage.sh');
  {$ENDIF}
end;

begin
  WriteLn('=== MonEditeur Build Script ===');
  WriteLn;

  try
    BuildWindows;
    WriteLn;
    BuildLinux;
    WriteLn;
    CreatePackages;
    WriteLn;
    WriteLn('Build process completed successfully!');
  except
    on E: Exception do
    begin
      WriteLn('Error: ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
```

## Tests et assurance qualité

### Tests unitaires des composants

```pascal
unit EditorTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, Editor, Document;

type
  TEditorTest = class(TTestCase)
  published
    procedure TestCreateDocument;
    procedure TestLoadFile;
    procedure TestSaveFile;
    procedure TestUndo;
    procedure TestRedo;
    procedure TestSearch;
  end;

implementation

procedure TEditorTest.TestCreateDocument;
var
  Doc: TDocument;
begin
  Doc := TDocument.Create;
  try
    AssertNotNull('Document should be created', Doc);
    AssertEquals('New document should not be modified', False, Doc.Modified);
  finally
    Doc.Free;
  end;
end;

procedure TEditorTest.TestLoadFile;
var
  Doc: TDocument;
  TestFile: string;
begin
  TestFile := 'test.txt';

  // Créer un fichier de test
  with TStringList.Create do
  try
    Add('Line 1');
    Add('Line 2');
    SaveToFile(TestFile);
  finally
    Free;
  end;

  // Tester le chargement
  Doc := TDocument.Create;
  try
    Doc.LoadFromFile(TestFile);
    AssertEquals('Should load 2 lines', 2, Doc.Lines.Count);
    AssertEquals('First line', 'Line 1', Doc.Lines[0]);
  finally
    Doc.Free;
    DeleteFile(TestFile);
  end;
end;

initialization
  RegisterTest(TEditorTest);

end.
```

### Tests d'intégration cross-platform

```pascal
program IntegrationTests;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, MainForm, Editor, Project;

procedure TestFileOperations;
var
  Editor: TCustomCodeEditor;
  TestFile: string;
begin
  WriteLn('Testing file operations...');

  {$IFDEF WINDOWS}
  TestFile := 'C:\Temp\test.txt';
  {$ENDIF}
  {$IFDEF LINUX}
  TestFile := '/tmp/test.txt';
  {$ENDIF}

  Editor := TCustomCodeEditor.Create(nil);
  try
    Editor.Lines.Text := 'Test content';
    Editor.SaveToFile(TestFile);

    Editor.LoadFromFile(TestFile);
    Assert(Editor.Lines.Text = 'Test content', 'Content mismatch');

    WriteLn('  File operations: OK');
  finally
    Editor.Free;
    DeleteFile(TestFile);
  end;
end;

procedure TestProjectManagement;
var
  Project: TProject;
begin
  WriteLn('Testing project management...');

  Project := TProject.Create;
  try
    Project.Name := 'Test Project';
    Project.AddFile('file1.pas');
    Project.AddFile('file2.pas');

    Assert(Project.Files.Count = 2, 'File count mismatch');
    WriteLn('  Project management: OK');
  finally
    Project.Free;
  end;
end;

begin
  WriteLn('=== Running Integration Tests ===');
  WriteLn;

  try
    TestFileOperations;
    TestProjectManagement;

    WriteLn;
    WriteLn('All tests passed!');
  except
    on E: Exception do
    begin
      WriteLn('Test failed: ', E.Message);
      ExitCode := 1;
    end;
  end;

  ReadLn; // Pause pour lire les résultats
end.
```

### Tests automatisés dans CI/CD

Configuration pour GitHub Actions qui teste sur Windows et Ubuntu :

```yaml
# .github/workflows/build-and-test.yml
name: Build and Test

on:
  push:
    branches: [ main, develop ]
  pull_request:
    branches: [ main ]

jobs:
  test-windows:
    runs-on: windows-latest
    steps:
    - uses: actions/checkout@v2

    - name: Install Lazarus
      run: |
        choco install lazarus -y

    - name: Build Project
      run: |
        lazbuild --build-mode=Release MonEditeur.lpi

    - name: Run Tests
      run: |
        cd tests
        lazbuild EditorTests.lpi
        .\EditorTests.exe

    - name: Upload Artifact
      uses: actions/upload-artifact@v2
      with:
        name: MonEditeur-Windows
        path: bin/Windows/MonEditeur.exe

  test-linux:
    runs-on: ubuntu-latest
    steps:
    - uses: actions/checkout@v2

    - name: Install Lazarus
      run: |
        sudo apt-get update
        sudo apt-get install -y lazarus lcl lcl-gtk2

    - name: Build Project
      run: |
        lazbuild --build-mode=Release MonEditeur.lpi

    - name: Run Tests
      run: |
        cd tests
        lazbuild EditorTests.lpi
        ./EditorTests

    - name: Upload Artifact
      uses: actions/upload-artifact@v2
      with:
        name: MonEditeur-Linux
        path: bin/Linux/monediteur
```

### Tests de performance

Mesurer les performances sur différentes plateformes :

```pascal
unit PerformanceTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils;

type
  TPerformanceTest = class
  private
    FStartTime: TDateTime;
    FEndTime: TDateTime;
  public
    procedure StartTimer;
    procedure StopTimer;
    function GetElapsedMilliseconds: Int64;
    procedure ReportResult(const ATestName: string);
  end;

procedure RunPerformanceTests;

implementation

procedure TPerformanceTest.StartTimer;
begin
  FStartTime := Now;
end;

procedure TPerformanceTest.StopTimer;
begin
  FEndTime := Now;
end;

function TPerformanceTest.GetElapsedMilliseconds: Int64;
begin
  Result := MilliSecondsBetween(FEndTime, FStartTime);
end;

procedure TPerformanceTest.ReportResult(const ATestName: string);
begin
  WriteLn(Format('%s: %d ms', [ATestName, GetElapsedMilliseconds]));
end;

procedure TestLargeFileLoading;
var
  Test: TPerformanceTest;
  Editor: TCustomCodeEditor;
  Lines: TStringList;
  i: Integer;
begin
  WriteLn('Testing large file loading...');

  // Créer un fichier de test avec 100 000 lignes
  Lines := TStringList.Create;
  try
    for i := 1 to 100000 do
      Lines.Add(Format('Line %d: Some test content here', [i]));
    Lines.SaveToFile('large_test.txt');
  finally
    Lines.Free;
  end;

  Test := TPerformanceTest.Create;
  Editor := TCustomCodeEditor.Create(nil);
  try
    Test.StartTimer;
    Editor.LoadFromFile('large_test.txt');
    Test.StopTimer;

    Test.ReportResult('Load 100K lines');
  finally
    Editor.Free;
    Test.Free;
    DeleteFile('large_test.txt');
  end;
end;

procedure TestSearchPerformance;
var
  Test: TPerformanceTest;
  Editor: TCustomCodeEditor;
  i, Count: Integer;
begin
  WriteLn('Testing search performance...');

  Editor := TCustomCodeEditor.Create(nil);
  Test := TPerformanceTest.Create;
  try
    // Créer du contenu de test
    for i := 1 to 10000 do
      Editor.Lines.Add('Some text with the word test in it');

    Test.StartTimer;
    Count := SearchAllOccurrences(Editor, 'test');
    Test.StopTimer;

    WriteLn(Format('  Found %d occurrences', [Count]));
    Test.ReportResult('Search in 10K lines');
  finally
    Test.Free;
    Editor.Free;
  end;
end;

procedure TestSyntaxHighlighting;
var
  Test: TPerformanceTest;
  Editor: TSynEdit;
  i: Integer;
begin
  WriteLn('Testing syntax highlighting...');

  Editor := TSynEdit.Create(nil);
  Test := TPerformanceTest.Create;
  try
    Editor.Highlighter := TPascalHighlighter.Create(nil);

    // Créer du code Pascal de test
    for i := 1 to 5000 do
    begin
      Editor.Lines.Add('procedure Test' + IntToStr(i) + ';');
      Editor.Lines.Add('begin');
      Editor.Lines.Add('  WriteLn(''Hello World'');');
      Editor.Lines.Add('end;');
    end;

    Test.StartTimer;
    Editor.Invalidate; // Force le rehighlighting
    Test.StopTimer;

    Test.ReportResult('Highlight 20K lines');
  finally
    Test.Free;
    Editor.Free;
  end;
end;

procedure RunPerformanceTests;
begin
  WriteLn('=== Performance Tests ===');
  WriteLn;

  {$IFDEF WINDOWS}
  WriteLn('Platform: Windows');
  {$ENDIF}
  {$IFDEF LINUX}
  WriteLn('Platform: Linux');
  {$ENDIF}

  WriteLn('Compiler: ', {$I %FPCVERSION%});
  WriteLn;

  TestLargeFileLoading;
  TestSearchPerformance;
  TestSyntaxHighlighting;

  WriteLn;
  WriteLn('Performance tests completed.');
end;

end.
```

## Documentation utilisateur

### Aide intégrée

Intégrer une documentation accessible depuis l'éditeur :

```pascal
type
  THelpSystem = class
  private
    FHelpFile: string;
    FHelpViewer: THelpViewer;
  public
    constructor Create;
    procedure ShowHelp(const ATopic: string);
    procedure ShowContextHelp(AContext: Integer);
    function SearchHelp(const AQuery: string): TStringList;
  end;

constructor THelpSystem.Create;
begin
  inherited Create;

  {$IFDEF WINDOWS}
  FHelpFile := ExtractFilePath(ParamStr(0)) + 'help\MonEditeur.chm';
  {$ENDIF}
  {$IFDEF LINUX}
  FHelpFile := '/usr/share/doc/monediteur/help.html';
  {$ENDIF}
end;

procedure THelpSystem.ShowHelp(const ATopic: string);
begin
  {$IFDEF WINDOWS}
  // Utiliser le visualiseur CHM de Windows
  HtmlHelp(0, PChar(FHelpFile), HH_DISPLAY_TOPIC, 0);
  {$ENDIF}

  {$IFDEF LINUX}
  // Ouvrir le navigateur par défaut
  ExecuteProcess('xdg-open', [FHelpFile + '#' + ATopic]);
  {$ENDIF}
end;

procedure THelpSystem.ShowContextHelp(AContext: Integer);
var
  Topic: string;
begin
  case AContext of
    CTX_EDITOR: Topic := 'editor-basics';
    CTX_SEARCH: Topic := 'search-replace';
    CTX_PROJECT: Topic := 'project-management';
    CTX_SETTINGS: Topic := 'preferences';
  else
    Topic := 'index';
  end;

  ShowHelp(Topic);
end;
```

### Système de tutoriels interactifs

Guide l'utilisateur dans ses premières utilisations :

```pascal
type
  TTutorial = class
  private
    FSteps: TList<TTutorialStep>;
    FCurrentStep: Integer;
    FOverlay: TTutorialOverlay;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Start;
    procedure NextStep;
    procedure PreviousStep;
    procedure Finish;
    function IsCompleted: Boolean;
  end;

  TTutorialStep = class
  private
    FTitle: string;
    FDescription: string;
    FTargetControl: TControl;
    FHighlightRect: TRect;
  public
    procedure Show;
    procedure Hide;
    property Title: string read FTitle;
    property Description: string read FDescription;
  end;

procedure TTutorial.Start;
begin
  FCurrentStep := 0;
  FOverlay := TTutorialOverlay.Create(Application.MainForm);
  FOverlay.Show;

  FSteps[FCurrentStep].Show;
end;

procedure TTutorialStep.Show;
begin
  // Afficher une bulle d'information près du contrôle ciblé
  with TTutorialBubble.Create(FTargetControl.Owner) do
  begin
    Caption := FTitle;
    Text := FDescription;

    // Positionner la bulle
    if Assigned(FTargetControl) then
    begin
      Left := FTargetControl.Left + FTargetControl.Width + 10;
      Top := FTargetControl.Top;
    end;

    Show;
  end;

  // Mettre en surbrillance le contrôle
  if Assigned(FTargetControl) then
    HighlightControl(FTargetControl);
end;

// Exemple de tutoriel pour les nouveaux utilisateurs
procedure CreateFirstUseTutorial: TTutorial;
begin
  Result := TTutorial.Create;

  // Étape 1 : Créer un nouveau fichier
  with Result.AddStep do
  begin
    Title := 'Créer un nouveau fichier';
    Description := 'Cliquez sur ce bouton ou utilisez Ctrl+N pour créer un nouveau fichier';
    TargetControl := MainForm.NewFileButton;
  end;

  // Étape 2 : Écrire du code
  with Result.AddStep do
  begin
    Title := 'Zone d''édition';
    Description := 'Écrivez votre code ici. La coloration syntaxique s''applique automatiquement';
    TargetControl := MainForm.ActiveEditor;
  end;

  // Étape 3 : Sauvegarder
  with Result.AddStep do
  begin
    Title := 'Sauvegarder le fichier';
    Description := 'Utilisez Ctrl+S ou ce bouton pour sauvegarder votre travail';
    TargetControl := MainForm.SaveButton;
  end;
end;
```

## Fonctionnalités avancées spécifiques

### Minimap (carte de navigation)

Une miniature du code pour naviguer rapidement :

```pascal
type
  TMinimap = class(TCustomControl)
  private
    FEditor: TSynEdit;
    FScale: Double;
    FVisibleRect: TRect;
  protected
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
                       X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AttachToEditor(AEditor: TSynEdit);
    procedure UpdateView;
  end;

procedure TMinimap.Paint;
var
  i: Integer;
  LineY: Integer;
  LineText: string;
begin
  inherited Paint;

  // Dessiner un aperçu miniature du code
  Canvas.Brush.Color := FEditor.Color;
  Canvas.FillRect(ClientRect);

  Canvas.Font.Size := 1; // Très petite police
  Canvas.Font.Name := 'Courier New';

  for i := 0 to FEditor.Lines.Count - 1 do
  begin
    LineY := Round(i * FScale);
    if LineY > Height then Break;

    LineText := FEditor.Lines[i];
    Canvas.TextOut(2, LineY, LineText);
  end;

  // Dessiner le rectangle de la zone visible
  Canvas.Brush.Style := bsClear;
  Canvas.Pen.Color := clHighlight;
  Canvas.Pen.Width := 2;
  Canvas.Rectangle(FVisibleRect);
end;

procedure TMinimap.MouseDown(Button: TMouseButton; Shift: TShiftState;
                             X, Y: Integer);
var
  ClickedLine: Integer;
begin
  inherited MouseDown(Button, Shift, X, Y);

  // Calculer la ligne cliquée
  ClickedLine := Round(Y / FScale);

  // Faire défiler l'éditeur à cette ligne
  FEditor.TopLine := ClickedLine;
  UpdateView;
end;
```

### Comparaison de fichiers (Diff Viewer)

Outil pour comparer deux versions d'un fichier :

```pascal
type
  TDiffType = (dtEqual, dtInsert, dtDelete, dtModify);

  TDiffLine = record
    LineType: TDiffType;
    LeftLine: string;
    RightLine: string;
    LeftNumber: Integer;
    RightNumber: Integer;
  end;

  TDiffViewer = class(TCustomControl)
  private
    FLeftEditor: TSynEdit;
    FRightEditor: TSynEdit;
    FDiffLines: array of TDiffLine;
  public
    procedure CompareFiles(const ALeftFile, ARightFile: string);
    procedure NextDifference;
    procedure PreviousDifference;
  end;

procedure TDiffViewer.CompareFiles(const ALeftFile, ARightFile: string);
var
  LeftLines, RightLines: TStringList;
  i, j: Integer;
  Diff: TDiffLine;
begin
  LeftLines := TStringList.Create;
  RightLines := TStringList.Create;
  try
    LeftLines.LoadFromFile(ALeftFile);
    RightLines.LoadFromFile(ARightFile);

    SetLength(FDiffLines, 0);

    // Algorithm de comparaison simple (Myers diff serait mieux)
    i := 0;
    j := 0;

    while (i < LeftLines.Count) or (j < RightLines.Count) do
    begin
      if (i < LeftLines.Count) and (j < RightLines.Count) then
      begin
        if LeftLines[i] = RightLines[j] then
        begin
          // Lignes identiques
          Diff.LineType := dtEqual;
          Diff.LeftLine := LeftLines[i];
          Diff.RightLine := RightLines[j];
          Diff.LeftNumber := i + 1;
          Diff.RightNumber := j + 1;
          Inc(i);
          Inc(j);
        end
        else
        begin
          // Lignes différentes
          Diff.LineType := dtModify;
          Diff.LeftLine := LeftLines[i];
          Diff.RightLine := RightLines[j];
          Diff.LeftNumber := i + 1;
          Diff.RightNumber := j + 1;
          Inc(i);
          Inc(j);
        end;
      end
      else if i < LeftLines.Count then
      begin
        // Ligne supprimée
        Diff.LineType := dtDelete;
        Diff.LeftLine := LeftLines[i];
        Diff.RightLine := '';
        Diff.LeftNumber := i + 1;
        Diff.RightNumber := 0;
        Inc(i);
      end
      else
      begin
        // Ligne ajoutée
        Diff.LineType := dtInsert;
        Diff.LeftLine := '';
        Diff.RightLine := RightLines[j];
        Diff.LeftNumber := 0;
        Diff.RightNumber := j + 1;
        Inc(j);
      end;

      SetLength(FDiffLines, Length(FDiffLines) + 1);
      FDiffLines[High(FDiffLines)] := Diff;
    end;

    DisplayDiff;
  finally
    LeftLines.Free;
    RightLines.Free;
  end;
end;

procedure TDiffViewer.DisplayDiff;
var
  i: Integer;
  DiffLine: TDiffLine;
  LeftAttr, RightAttr: TSynHighlighterAttributes;
begin
  FLeftEditor.Lines.Clear;
  FRightEditor.Lines.Clear;

  for i := 0 to High(FDiffLines) do
  begin
    DiffLine := FDiffLines[i];

    case DiffLine.LineType of
      dtEqual:
      begin
        FLeftEditor.Lines.Add(DiffLine.LeftLine);
        FRightEditor.Lines.Add(DiffLine.RightLine);
      end;

      dtDelete:
      begin
        FLeftEditor.Lines.Add(DiffLine.LeftLine);
        // Colorer en rouge
        SetLineColor(FLeftEditor, i, clRed);
        FRightEditor.Lines.Add('');
      end;

      dtInsert:
      begin
        FLeftEditor.Lines.Add('');
        FRightEditor.Lines.Add(DiffLine.RightLine);
        // Colorer en vert
        SetLineColor(FRightEditor, i, clGreen);
      end;

      dtModify:
      begin
        FLeftEditor.Lines.Add(DiffLine.LeftLine);
        FRightEditor.Lines.Add(DiffLine.RightLine);
        // Colorer en orange
        SetLineColor(FLeftEditor, i, clOrange);
        SetLineColor(FRightEditor, i, clOrange);
      end;
    end;
  end;

  // Synchroniser le défilement des deux éditeurs
  FLeftEditor.OnScroll := @SyncScroll;
  FRightEditor.OnScroll := @SyncScroll;
end;
```

### Recherche de symboles dans le projet

Navigation rapide vers les définitions :

```pascal
type
  TSymbolKind = (skFunction, skProcedure, skClass, skVariable, skConstant);

  TSymbol = class
  private
    FName: string;
    FKind: TSymbolKind;
    FFileName: string;
    FLine: Integer;
    FColumn: Integer;
  public
    property Name: string read FName;
    property Kind: TSymbolKind read FKind;
    property FileName: string read FFileName;
    property Line: Integer read FLine;
  end;

  TSymbolFinder = class
  private
    FSymbols: TObjectList<TSymbol>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure IndexProject(AProject: TProject);
    function FindSymbol(const AName: string): TList<TSymbol>;
    function FindSymbolsInFile(const AFileName: string): TList<TSymbol>;
  end;

procedure TSymbolFinder.IndexProject(AProject: TProject);
var
  i: Integer;
  ProjectFile: TProjectFile;
  Parser: TCodeParser;
begin
  FSymbols.Clear;

  for i := 0 to AProject.Files.Count - 1 do
  begin
    ProjectFile := AProject.Files[i];

    // Analyser chaque fichier
    Parser := TCodeParser.Create;
    try
      Parser.ParseFile(ProjectFile.FileName);

      // Ajouter les symboles trouvés
      FSymbols.AddRange(Parser.Symbols);
    finally
      Parser.Free;
    end;
  end;
end;

function TSymbolFinder.FindSymbol(const AName: string): TList<TSymbol>;
var
  Symbol: TSymbol;
begin
  Result := TList<TSymbol>.Create;

  for Symbol in FSymbols do
  begin
    if SameText(Symbol.Name, AName) or
       ContainsText(Symbol.Name, AName) then
      Result.Add(Symbol);
  end;
end;

// Interface utilisateur pour la recherche rapide de symboles
procedure TMainForm.ShowSymbolSearch;
var
  SearchDialog: TSymbolSearchDialog;
  Results: TList<TSymbol>;
  SelectedSymbol: TSymbol;
begin
  SearchDialog := TSymbolSearchDialog.Create(Self);
  try
    SearchDialog.SymbolFinder := FSymbolFinder;

    if SearchDialog.ShowModal = mrOk then
    begin
      SelectedSymbol := SearchDialog.SelectedSymbol;

      // Ouvrir le fichier et aller à la ligne
      OpenFile(SelectedSymbol.FileName);
      ActiveEditor.CaretY := SelectedSymbol.Line;
      ActiveEditor.CaretX := SelectedSymbol.Column;
      ActiveEditor.EnsureCursorPosVisible;
    end;
  finally
    SearchDialog.Free;
  end;
end;
```

### Macros enregistrables

Permettre d'enregistrer et rejouer des séquences d'actions :

```pascal
type
  TMacroAction = class
  private
    FActionType: string;
    FParameters: TStringList;
  public
    constructor Create(const AType: string);
    destructor Destroy; override;
    procedure Execute(AEditor: TSynEdit);
  end;

  TMacroRecorder = class
  private
    FRecording: Boolean;
    FActions: TObjectList<TMacroAction>;
    FCurrentMacro: string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure StartRecording(const AMacroName: string);
    procedure StopRecording;
    procedure RecordAction(const AActionType: string;
                          const AParams: array of string);
    procedure PlayMacro(const AMacroName: string; AEditor: TSynEdit);
    procedure SaveMacro(const AMacroName, AFileName: string);
    procedure LoadMacro(const AFileName: string);
  end;

procedure TMacroRecorder.RecordAction(const AActionType: string;
  const AParams: array of string);
var
  Action: TMacroAction;
  i: Integer;
begin
  if not FRecording then Exit;

  Action := TMacroAction.Create(AActionType);

  for i := 0 to High(AParams) do
    Action.FParameters.Add(AParams[i]);

  FActions.Add(Action);
end;

procedure TMacroRecorder.PlayMacro(const AMacroName: string;
  AEditor: TSynEdit);
var
  Action: TMacroAction;
begin
  for Action in FActions do
    Action.Execute(AEditor);
end;

procedure TMacroAction.Execute(AEditor: TSynEdit);
begin
  case FActionType of
    'insert_text':
      AEditor.InsertTextAtCaret(FParameters[0]);

    'delete_line':
      AEditor.ExecuteCommand(ecDeleteLine, ' ', nil);

    'move_up':
      AEditor.CaretY := AEditor.CaretY - StrToInt(FParameters[0]);

    'move_down':
      AEditor.CaretY := AEditor.CaretY + StrToInt(FParameters[0]);

    'find_replace':
      FindAndReplace(AEditor, FParameters[0], FParameters[1]);
  end;
end;

// Utilisation dans l'interface
procedure TMainForm.ToggleMacroRecording;
begin
  if FMacroRecorder.IsRecording then
  begin
    FMacroRecorder.StopRecording;
    StatusBar.Panels[2].Text := 'Macro enregistrée';
    RecordButton.ImageIndex := IMG_RECORD;
  end
  else
  begin
    FMacroRecorder.StartRecording('UserMacro1');
    StatusBar.Panels[2].Text := 'Enregistrement en cours...';
    RecordButton.ImageIndex := IMG_STOP;
  end;
end;
```

## Conclusion et bonnes pratiques

### Principes de développement multi-plateforme

Pour réussir un éditeur portable, suivez ces principes :

**1. Abstraction des API système**
- Utilisez toujours des interfaces pour isoler le code spécifique à chaque plateforme
- Créez une couche d'abstraction pour les opérations système

**2. Tests sur les deux plateformes**
- Testez régulièrement sur Windows ET Ubuntu
- Utilisez des machines virtuelles ou le dual-boot
- Automatisez les tests avec CI/CD

**3. Respect des conventions de chaque OS**
- Emplacements des fichiers de configuration
- Raccourcis clavier natifs
- Apparence et comportement natifs

**4. Gestion des chemins de fichiers**
```pascal
// TOUJOURS utiliser les fonctions portables
GoodPath := IncludeTrailingPathDelimiter(BaseDir) + 'subdir' +
            DirectorySeparator + 'file.txt';

// JAMAIS de chemins en dur
BadPath := 'C:\Program Files\MonApp\file.txt';  // Windows only!
```

**5. Encodage des fichiers**
```pascal
// En FreePascal, l'encodage UTF-8 est géré par défaut
// avec {$H+} et les unités LazUTF8/LazFileUtils
uses
  LazUTF8;

// Charger et sauvegarder en UTF-8
MyList.LoadFromFile(FileName);  // UTF-8 par défaut avec {$H+}
MyList.SaveToFile(FileName);
```

**6. Fins de ligne**
```pascal
// Utiliser la constante portable
const
  NewLine = LineEnding;  // CRLF sur Windows, LF sur Unix

// Ou détecter automatiquement
function DetectLineEnding(const AText: string): string;
begin
  if Pos(#13#10, AText) > 0 then
    Result := #13#10  // Windows
  else if Pos(#10, AText) > 0 then
    Result := #10     // Unix
  else
    Result := LineEnding;  // Par défaut
end;
```

### Performance et optimisation

**Optimisations générales :**

1. **Rendu différé** : ne redessiner que ce qui change
2. **Virtualisation** : pour les grandes listes, n'afficher que les éléments visibles
3. **Cache intelligent** : mettre en cache les calculs coûteux
4. **Multithreading** : déporter les tâches lourdes dans des threads
5. **Lazy loading** : charger le contenu à la demande

**Optimisations spécifiques Windows :**
```pascal
{$IFDEF WINDOWS}
// Utiliser les messages Windows pour une meilleure réactivité
procedure TCustomEditor.WMPaint(var Message: TWMPaint);
begin
  // Optimisations de rendu Windows
  inherited;
end;
{$ENDIF}
```

**Optimisations spécifiques Linux :**
```pascal
{$IFDEF LINUX}
// Optimiser pour GTK/X11
procedure TCustomEditor.Paint;
begin
  // Réduire les appels de rendu GTK coûteux
  Canvas.BeginUpdate;
  try
    // ... rendu ...
  finally
    Canvas.EndUpdate;
  end;
end;
{$ENDIF}
```

### Checklist finale avant publication

**Code et qualité :**
- [ ] Tous les tests passent sur Windows et Ubuntu
- [ ] Aucune dépendance spécifique à un OS dans le code portable
- [ ] Le code est documenté et commenté
- [ ] Pas de fuites mémoire (testé avec Heaptrc)
- [ ] Performance acceptable sur les deux OS

**Interface utilisateur :**
- [ ] Thèmes clair et sombre fonctionnels
- [ ] Support High-DPI sur les deux OS
- [ ] Accessibilité au clavier complète
- [ ] Toutes les chaînes sont traduisibles
- [ ] Icônes et ressources de qualité

**Documentation :**
- [ ] Manuel utilisateur complet
- [ ] Documentation API pour les développeurs de plugins
- [ ] Tutoriels pour les débutants
- [ ] FAQ et résolution de problèmes

**Distribution :**
- [ ] Installateur Windows (Inno Setup ou MSI)
- [ ] Paquet DEB pour Ubuntu/Debian
- [ ] AppImage pour Linux universel
- [ ] Signatures et checksums de sécurité
- [ ] Page de téléchargement avec instructions claires

**Support :**
- [ ] Site web ou page GitHub avec documentation
- [ ] Système de rapport de bugs
- [ ] Forum ou canal de discussion
- [ ] Changelog détaillé

### Ressources et apprentissage continu

**Documentation officielle :**
- Wiki FreePascal : https://wiki.freepascal.org
- Wiki Lazarus : https://wiki.lazarus.freepascal.org
- Documentation LCL : https://lazarus-ccr.sourceforge.io

**Communauté :**
- Forum Lazarus : https://forum.lazarus.freepascal.org
- Reddit r/lazarus
- Discord FreePascal/Lazarus

**Exemples de projets :**
- **Lazarus IDE** lui-même : excellent exemple d'IDE multi-plateforme
- **Double Commander** : gestionnaire de fichiers en FreePascal
- **PeaZip** : utilitaire d'archivage

**Packages utiles :**
- **BGRABitmap** : graphiques avancés
- **Synapse** : réseau
- **ZeosLib** : bases de données
- **mORMot** : framework SOA/REST

### Évolutions futures possibles

**Fonctionnalités à considérer :**

1. **Support macOS** : compléter le trio Windows/Linux/macOS
2. **LSP (Language Server Protocol)** : support moderne des langages
3. **Remote Development** : édition de fichiers distants
4. **Collaboration en temps réel** : édition collaborative à plusieurs
5. **Intégration IA** : complétion de code assistée par IA
6. **Notebooks interactifs** : style Jupyter pour le prototypage
7. **Débogueur visuel avancé** : inspection de variables, breakpoints conditionnels
8. **Profiler intégré** : analyse de performance en temps réel
9. **Gestionnaire de packages** : installation facile de bibliothèques
10. **Mode Zen** : interface minimaliste sans distraction

### Architecture extensible pour le futur

Concevoir l'éditeur pour faciliter l'ajout de nouvelles fonctionnalités :

```pascal
type
  // Interface de base pour les extensions
  IEditorExtension = interface
    ['{XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX}']
    function GetName: string;
    function GetDescription: string;
    procedure Initialize(AContext: IEditorContext);
    procedure Shutdown;
    procedure OnDocumentOpened(ADocument: TDocument);
    procedure OnDocumentClosed(ADocument: TDocument);
  end;

  // Contexte fourni aux extensions
  IEditorContext = interface
    function GetMainForm: TForm;
    function GetActiveEditor: TSynEdit;
    function GetActiveDocument: TDocument;
    function GetProject: TProject;
    procedure RegisterCommand(const AName: string; AHandler: TCommandHandler);
    procedure AddMenuItem(const APath, ACommand: string);
    procedure AddToolbarButton(const ACaption, ACommand: string; AIcon: TBitmap);
  end;

  // Gestionnaire d'extensions
  TExtensionManager = class
  private
    FExtensions: TList<IEditorExtension>;
    FContext: IEditorContext;
  public
    constructor Create(AContext: IEditorContext);
    destructor Destroy; override;
    procedure LoadExtension(const AFileName: string);
    procedure UnloadExtension(AExtension: IEditorExtension);
    procedure NotifyDocumentOpened(ADocument: TDocument);
  end;
```

**Exemple d'extension LSP (Language Server Protocol) :**

```pascal
unit LSPExtension;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, EditorExtensionAPI, LSPClient;

type
  TLSPExtension = class(TInterfacedObject, IEditorExtension)
  private
    FContext: IEditorContext;
    FLSPClients: TDictionary<string, TLSPClient>;
  public
    function GetName: string;
    function GetDescription: string;
    procedure Initialize(AContext: IEditorContext);
    procedure Shutdown;
    procedure OnDocumentOpened(ADocument: TDocument);
    procedure OnDocumentClosed(ADocument: TDocument);

    procedure StartLanguageServer(const ALanguage: string);
    procedure RequestCompletion(ADocument: TDocument; APosition: TPoint);
    procedure RequestHover(ADocument: TDocument; APosition: TPoint);
    procedure RequestDefinition(ADocument: TDocument; APosition: TPoint);
  end;

implementation

function TLSPExtension.GetName: string;
begin
  Result := 'Language Server Protocol Support';
end;

procedure TLSPExtension.Initialize(AContext: IEditorContext);
begin
  FContext := AContext;
  FLSPClients := TDictionary<string, TLSPClient>.Create;

  // Enregistrer les commandes LSP
  FContext.RegisterCommand('lsp.goto_definition', @OnGotoDefinition);
  FContext.RegisterCommand('lsp.find_references', @OnFindReferences);

  // Ajouter des éléments de menu
  FContext.AddMenuItem('Tools/LSP/Go to Definition', 'lsp.goto_definition');
  FContext.AddMenuItem('Tools/LSP/Find References', 'lsp.find_references');
end;

procedure TLSPExtension.OnDocumentOpened(ADocument: TDocument);
var
  Language: string;
  Client: TLSPClient;
begin
  Language := ADocument.DetectLanguage;

  if not FLSPClients.ContainsKey(Language) then
    StartLanguageServer(Language);

  if FLSPClients.TryGetValue(Language, Client) then
  begin
    // Notifier le serveur de l'ouverture du document
    Client.SendNotification('textDocument/didOpen',
      CreateDidOpenParams(ADocument));
  end;
end;

procedure TLSPExtension.StartLanguageServer(const ALanguage: string);
var
  Client: TLSPClient;
  ServerPath: string;
begin
  // Déterminer le chemin du serveur selon le langage
  case ALanguage of
    'pascal': ServerPath := 'pasls';  // Pascal Language Server
    'python': ServerPath := 'pylsp';
    'javascript': ServerPath := 'typescript-language-server';
    'rust': ServerPath := 'rust-analyzer';
  else
    Exit; // Pas de serveur disponible pour ce langage
  end;

  Client := TLSPClient.Create(ServerPath);
  Client.OnCompletionResponse := @HandleCompletionResponse;
  Client.OnHoverResponse := @HandleHoverResponse;
  Client.OnDefinitionResponse := @HandleDefinitionResponse;

  Client.Start;
  Client.SendInitialize(GetInitializeParams);

  FLSPClients.Add(ALanguage, Client);
end;

procedure TLSPExtension.RequestCompletion(ADocument: TDocument;
  APosition: TPoint);
var
  Client: TLSPClient;
  Params: TJSONObject;
begin
  if FLSPClients.TryGetValue(ADocument.Language, Client) then
  begin
    Params := TJSONObject.Create;
    Params.Add('textDocument', CreateTextDocumentIdentifier(ADocument));
    Params.Add('position', CreatePosition(APosition));

    Client.SendRequest('textDocument/completion', Params);
  end;
end;

procedure TLSPExtension.HandleCompletionResponse(AResponse: TJSONObject);
var
  Items: TJSONArray;
  Item: TJSONObject;
  i: Integer;
  Completion: TCompletionItem;
begin
  Items := AResponse.GetArray('items');

  for i := 0 to Items.Count - 1 do
  begin
    Item := Items.Items[i] as TJSONObject;

    Completion.Text := Item.Get('label', '');
    Completion.Kind := MapLSPKindToCompletionKind(Item.Get('kind', 0));
    Completion.Detail := Item.Get('detail', '');
    Completion.Documentation := Item.Get('documentation', '');

    // Ajouter à la liste de complétion
    FContext.GetActiveEditor.AddCompletionItem(Completion);
  end;

  // Afficher la fenêtre de complétion
  FContext.GetActiveEditor.ShowCompletionWindow;
end;

end.
```

### Exemple complet : Mini-éditeur portable

Voici un exemple complet mais simplifié d'un éditeur minimal fonctionnel :

```pascal
program MiniEditor;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, Forms, SysUtils, Classes, StdCtrls, Menus, Dialogs,
  SynEdit, SynHighlighterPas, ComCtrls;

type
  TMainForm = class(TForm)
    MainMenu: TMainMenu;
    MenuFile: TMenuItem;
    MenuNew: TMenuItem;
    MenuOpen: TMenuItem;
    MenuSave: TMenuItem;
    MenuSaveAs: TMenuItem;
    MenuQuit: TMenuItem;
    MenuEdit: TMenuItem;
    MenuUndo: TMenuItem;
    MenuRedo: TMenuItem;
    MenuCut: TMenuItem;
    MenuCopy: TMenuItem;
    MenuPaste: TMenuItem;
    StatusBar: TStatusBar;
    Editor: TSynEdit;
    Highlighter: TSynPasSyn;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;

    procedure FormCreate(Sender: TObject);
    procedure MenuNewClick(Sender: TObject);
    procedure MenuOpenClick(Sender: TObject);
    procedure MenuSaveClick(Sender: TObject);
    procedure MenuSaveAsClick(Sender: TObject);
    procedure MenuQuitClick(Sender: TObject);
    procedure MenuUndoClick(Sender: TObject);
    procedure MenuRedoClick(Sender: TObject);
    procedure MenuCutClick(Sender: TObject);
    procedure MenuCopyClick(Sender: TObject);
    procedure MenuPasteClick(Sender: TObject);
    procedure EditorChange(Sender: TObject);
    procedure EditorStatusChange(Sender: TObject; Changes: TSynStatusChanges);
  private
    FCurrentFile: string;
    FModified: Boolean;
    procedure UpdateTitle;
    procedure UpdateStatusBar;
    function ConfirmSave: Boolean;
  public
    procedure NewFile;
    procedure OpenFile(const AFileName: string);
    procedure SaveFile(const AFileName: string);
  end;

var
  MainForm: TMainForm;

{$R *.lfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  // Configuration de l'éditeur
  Editor.Align := alClient;
  Editor.Highlighter := Highlighter;
  Editor.Font.Name := 'Courier New';
  Editor.Font.Size := 10;
  Editor.Gutter.ShowLineNumbers := True;
  Editor.Options := Editor.Options + [eoTabsToSpaces];
  Editor.TabWidth := 2;

  // Configuration du StatusBar
  StatusBar.Panels.Add;
  StatusBar.Panels.Add;
  StatusBar.Panels.Add;
  StatusBar.Panels[0].Width := 200;
  StatusBar.Panels[1].Width := 100;
  StatusBar.Panels[2].Width := 100;

  FCurrentFile := '';
  FModified := False;
  UpdateTitle;
  UpdateStatusBar;
end;

procedure TMainForm.NewFile;
begin
  if FModified and not ConfirmSave then
    Exit;

  Editor.Lines.Clear;
  FCurrentFile := '';
  FModified := False;
  UpdateTitle;
end;

procedure TMainForm.OpenFile(const AFileName: string);
begin
  if FModified and not ConfirmSave then
    Exit;

  try
    Editor.Lines.LoadFromFile(AFileName);
    FCurrentFile := AFileName;
    FModified := False;
    UpdateTitle;
    StatusBar.Panels[0].Text := 'Fichier ouvert : ' +
                                ExtractFileName(AFileName);
  except
    on E: Exception do
      ShowMessage('Erreur lors de l''ouverture : ' + E.Message);
  end;
end;

procedure TMainForm.SaveFile(const AFileName: string);
begin
  try
    Editor.Lines.SaveToFile(AFileName);
    FCurrentFile := AFileName;
    FModified := False;
    UpdateTitle;
    StatusBar.Panels[0].Text := 'Fichier sauvegardé : ' +
                                ExtractFileName(AFileName);
  except
    on E: Exception do
      ShowMessage('Erreur lors de la sauvegarde : ' + E.Message);
  end;
end;

procedure TMainForm.UpdateTitle;
var
  Title: string;
begin
  if FCurrentFile = '' then
    Title := 'Sans titre'
  else
    Title := ExtractFileName(FCurrentFile);

  if FModified then
    Title := Title + ' *';

  Caption := Title + ' - Mini Éditeur';
end;

procedure TMainForm.UpdateStatusBar;
begin
  StatusBar.Panels[1].Text := Format('Ligne: %d', [Editor.CaretY]);
  StatusBar.Panels[2].Text := Format('Col: %d', [Editor.CaretX]);
end;

function TMainForm.ConfirmSave: Boolean;
var
  Response: Integer;
begin
  Response := MessageDlg('Document modifié',
                        'Le document a été modifié. Voulez-vous sauvegarder ?',
                        mtConfirmation, [mbYes, mbNo, mbCancel], 0);

  case Response of
    mrYes:
    begin
      MenuSaveClick(nil);
      Result := not FModified; // True si sauvegarde réussie
    end;
    mrNo:
      Result := True;
    mrCancel:
      Result := False;
  end;
end;

// Gestionnaires de menu

procedure TMainForm.MenuNewClick(Sender: TObject);
begin
  NewFile;
end;

procedure TMainForm.MenuOpenClick(Sender: TObject);
begin
  if OpenDialog.Execute then
    OpenFile(OpenDialog.FileName);
end;

procedure TMainForm.MenuSaveClick(Sender: TObject);
begin
  if FCurrentFile = '' then
    MenuSaveAsClick(Sender)
  else
    SaveFile(FCurrentFile);
end;

procedure TMainForm.MenuSaveAsClick(Sender: TObject);
begin
  if SaveDialog.Execute then
    SaveFile(SaveDialog.FileName);
end;

procedure TMainForm.MenuQuitClick(Sender: TObject);
begin
  if not FModified or ConfirmSave then
    Close;
end;

procedure TMainForm.MenuUndoClick(Sender: TObject);
begin
  Editor.Undo;
end;

procedure TMainForm.MenuRedoClick(Sender: TObject);
begin
  Editor.Redo;
end;

procedure TMainForm.MenuCutClick(Sender: TObject);
begin
  Editor.CutToClipboard;
end;

procedure TMainForm.MenuCopyClick(Sender: TObject);
begin
  Editor.CopyToClipboard;
end;

procedure TMainForm.MenuPasteClick(Sender: TObject);
begin
  Editor.PasteFromClipboard;
end;

procedure TMainForm.EditorChange(Sender: TObject);
begin
  FModified := True;
  UpdateTitle;
end;

procedure TMainForm.EditorStatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
begin
  if [scCaretX, scCaretY] * Changes <> [] then
    UpdateStatusBar;
end;

begin
  Application.Title := 'Mini Éditeur';
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
```

### Récapitulatif des concepts clés

**Architecture :**
- Séparation claire entre logique métier et interface
- Abstraction des API système
- Architecture modulaire et extensible
- Support des plugins

**Multi-plateforme :**
- Utilisation de directives de compilation conditionnelle
- Gestion portable des chemins et fichiers
- Respect des conventions de chaque OS
- Tests sur toutes les plateformes cibles

**Fonctionnalités essentielles :**
- Édition de texte avec coloration syntaxique
- Gestion de projets multi-fichiers
- Recherche et remplacement avancés
- Auto-complétion intelligente
- Intégration Git
- Terminal intégré

**Performance :**
- Rendu optimisé et différé
- Chargement paresseux pour gros fichiers
- Cache de coloration syntaxique
- Multithreading pour tâches lourdes

**Qualité et maintenabilité :**
- Tests unitaires et d'intégration
- Gestion robuste des erreurs
- Sauvegarde automatique
- Documentation complète
- Code propre et commenté

### Mot de la fin

Développer un IDE ou éditeur de code complet est un projet ambitieux qui permet de mettre en pratique la majorité des concepts avancés de FreePascal et Lazarus. Ce type de projet démontre :

- **La maîtrise technique** : programmation avancée, architecture logicielle
- **La rigueur** : gestion de projet complexe, tests, documentation
- **La créativité** : interface utilisateur, fonctionnalités innovantes
- **La portabilité** : développement réellement multi-plateforme

Les éditeurs de code sont des outils que nous utilisons quotidiennement, et en créer un vous donne une compréhension profonde de leur fonctionnement interne. C'est également un excellent projet portfolio qui démontre vos compétences de développeur avancé.

N'hésitez pas à vous inspirer de projets open source existants comme Lazarus lui-même, Geany, ou Notepad++. Étudiez leur code source, participez aux discussions communautaires, et surtout : **codez, testez, itérez !**

Bonne chance dans vos développements ! 🚀

⏭️ [Moteur de workflow/BPM](/25-projets-complexes-etudes-cas/05-moteur-workflow-bpm.md)
