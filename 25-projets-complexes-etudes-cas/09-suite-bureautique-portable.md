🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 25.9 Suite Bureautique Portable

## Introduction

Une suite bureautique complète comprend généralement plusieurs applications interconnectées : traitement de texte, tableur, présentation, gestionnaire de base de données, et éditeur de diagrammes. Créer une telle suite avec FreePascal et Lazarus est un projet ambitieux qui démontre la puissance de ces outils pour le développement multi-plateforme.

Dans ce chapitre, nous allons explorer comment concevoir et implémenter une suite bureautique portable fonctionnant aussi bien sous Windows que sous Ubuntu.

## Qu'est-ce qu'une Suite Bureautique ?

Une suite bureautique est un ensemble d'applications de productivité qui partagent :
- **Une interface utilisateur cohérente**
- **Des formats de fichiers compatibles**
- **Des fonctionnalités d'import/export**
- **Un système de plugins commun**

### Composants Principaux

1. **Traitement de Texte** : Création et édition de documents texte
2. **Tableur** : Calculs, graphiques et analyses de données
3. **Présentation** : Diaporamas et supports visuels
4. **Base de Données** : Gestion de données structurées
5. **Éditeur de Diagrammes** : Schémas et organigrammes

## Architecture Générale

### Structure Modulaire

```
SuiteBureautique/
├── Core/                    # Noyau commun
│   ├── FileFormats/        # Formats de fichiers
│   ├── UI/                 # Composants d'interface
│   ├── Plugins/            # Système de plugins
│   └── Utils/              # Utilitaires
├── TextEditor/             # Traitement de texte
├── Spreadsheet/            # Tableur
├── Presentation/           # Présentations
├── Database/               # Base de données
└── Diagrams/               # Diagrammes
```

### Architecture en Couches

```
┌─────────────────────────────────────┐
│   Applications Bureautiques         │
├─────────────────────────────────────┤
│   Framework Commun (Core)           │
├─────────────────────────────────────┤
│   LCL (Lazarus Component Library)   │
├─────────────────────────────────────┤
│   FreePascal Runtime                │
├─────────────────────────────────────┤
│   Système d'Exploitation            │
│   (Windows / Ubuntu)                │
└─────────────────────────────────────┘
```

## Le Noyau Commun (Core)

### Gestionnaire de Documents

Tous les composants de la suite partagent une classe de base pour les documents :

> **Note :** Les extraits de code de ce chapitre utilisent `TDictionary<K,V>` et `TList<T>` de `Generics.Collections`, qui nécessitent `{$mode delphi}` et `uses Generics.Collections`.

```pascal
type
  TDocumentFormat = (dfNative, dfODF, dfMSOffice, dfPDF, dfHTML);

  TDocumentState = (dsNew, dsModified, dsSaved);

  TBaseDocument = class
  private
    FFileName: string;
    FModified: Boolean;
    FState: TDocumentState;
    FCreationDate: TDateTime;
    FModificationDate: TDateTime;
    FAuthor: string;
    FTitle: string;
  protected
    procedure SetModified(Value: Boolean); virtual;
    function GetCanUndo: Boolean; virtual; abstract;
    function GetCanRedo: Boolean; virtual; abstract;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    // Opérations de base
    function New: Boolean; virtual; abstract;
    function Open(const AFileName: string): Boolean; virtual; abstract;
    function Save: Boolean; virtual; abstract;
    function SaveAs(const AFileName: string): Boolean; virtual; abstract;
    function Close: Boolean; virtual;

    // Undo/Redo
    procedure Undo; virtual; abstract;
    procedure Redo; virtual; abstract;

    // Export/Import
    function ExportTo(const AFileName: string;
      AFormat: TDocumentFormat): Boolean; virtual; abstract;
    function ImportFrom(const AFileName: string;
      AFormat: TDocumentFormat): Boolean; virtual; abstract;

    // Propriétés
    property FileName: string read FFileName write FFileName;
    property Modified: Boolean read FModified write SetModified;
    property State: TDocumentState read FState;
    property Title: string read FTitle write FTitle;
    property Author: string read FAuthor write FAuthor;
    property CanUndo: Boolean read GetCanUndo;
    property CanRedo: Boolean read GetCanRedo;
  end;

constructor TBaseDocument.Create;  
begin
  inherited Create;
  FState := dsNew;
  FModified := False;
  FCreationDate := Now;
  FModificationDate := Now;
  FAuthor := GetEnvironmentVariable('USERNAME');
end;

procedure TBaseDocument.SetModified(Value: Boolean);  
begin
  if FModified <> Value then
  begin
    FModified := Value;
    if Value then
    begin
      FState := dsModified;
      FModificationDate := Now;
    end;
  end;
end;

function TBaseDocument.Close: Boolean;  
begin
  Result := True;
  if FModified then
  begin
    case MessageDlg('Document modifié',
                    'Voulez-vous enregistrer les modifications ?',
                    mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
      mrYes: Result := Save;
      mrCancel: Result := False;
    end;
  end;
end;
```

### Gestionnaire de Commandes (Undo/Redo)

Un système d'annulation robuste est essentiel :

```pascal
type
  TCommand = class
  public
    function Execute: Boolean; virtual; abstract;
    function Undo: Boolean; virtual; abstract;
    function GetDescription: string; virtual; abstract;
  end;

  TCommandManager = class
  private
    FUndoStack: TStack<TCommand>;
    FRedoStack: TStack<TCommand>;
    FMaxUndoLevels: Integer;
  public
    constructor Create(AMaxLevels: Integer = 100);
    destructor Destroy; override;

    procedure ExecuteCommand(ACommand: TCommand);
    procedure Undo;
    procedure Redo;
    procedure Clear;

    function CanUndo: Boolean;
    function CanRedo: Boolean;
    function GetUndoDescription: string;
    function GetRedoDescription: string;
  end;

constructor TCommandManager.Create(AMaxLevels: Integer);  
begin
  inherited Create;
  FMaxUndoLevels := AMaxLevels;
  FUndoStack := TStack<TCommand>.Create;
  FRedoStack := TStack<TCommand>.Create;
end;

destructor TCommandManager.Destroy;  
begin
  Clear;
  FUndoStack.Free;
  FRedoStack.Free;
  inherited;
end;

procedure TCommandManager.ExecuteCommand(ACommand: TCommand);  
begin
  if ACommand.Execute then
  begin
    // Ajouter à la pile d'annulation
    FUndoStack.Push(ACommand);

    // Limiter la taille de la pile
    while FUndoStack.Count > FMaxUndoLevels do
    begin
      FUndoStack.Extract.Free;
    end;

    // Vider la pile de rétablissement
    while FRedoStack.Count > 0 do
      FRedoStack.Pop.Free;
  end
  else
    ACommand.Free;
end;

procedure TCommandManager.Undo;  
var
  Cmd: TCommand;
begin
  if CanUndo then
  begin
    Cmd := FUndoStack.Pop;
    if Cmd.Undo then
      FRedoStack.Push(Cmd)
    else
      Cmd.Free;
  end;
end;

procedure TCommandManager.Redo;  
var
  Cmd: TCommand;
begin
  if CanRedo then
  begin
    Cmd := FRedoStack.Pop;
    if Cmd.Execute then
      FUndoStack.Push(Cmd)
    else
      Cmd.Free;
  end;
end;

function TCommandManager.CanUndo: Boolean;  
begin
  Result := FUndoStack.Count > 0;
end;

function TCommandManager.CanRedo: Boolean;  
begin
  Result := FRedoStack.Count > 0;
end;
```

### Interface Utilisateur Commune

Créez des composants réutilisables pour toute la suite :

```pascal
type
  TOfficeToolbar = class(TToolBar)
  private
    procedure CreateStandardButtons;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TOfficeMainMenu = class(TMainMenu)
  private
    procedure CreateStandardMenu;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TOfficeStatusBar = class(TStatusBar)
  private
    FPositionPanel: TStatusPanel;
    FModifiedPanel: TStatusPanel;
    FZoomPanel: TStatusPanel;
  public
    constructor Create(AOwner: TComponent); override;
    procedure UpdatePosition(Row, Col: Integer);
    procedure UpdateModified(Modified: Boolean);
    procedure UpdateZoom(Zoom: Integer);
  end;

constructor TOfficeToolbar.Create(AOwner: TComponent);  
begin
  inherited Create(AOwner);
  ShowCaptions := True;
  CreateStandardButtons;
end;

procedure TOfficeToolbar.CreateStandardButtons;  
var
  Btn: TToolButton;
begin
  // Nouveau
  Btn := TToolButton.Create(Self);
  Btn.Caption := 'Nouveau';
  Btn.ImageIndex := 0;
  Btn.Parent := Self;

  // Ouvrir
  Btn := TToolButton.Create(Self);
  Btn.Caption := 'Ouvrir';
  Btn.ImageIndex := 1;
  Btn.Parent := Self;

  // Enregistrer
  Btn := TToolButton.Create(Self);
  Btn.Caption := 'Enregistrer';
  Btn.ImageIndex := 2;
  Btn.Parent := Self;

  // Séparateur
  Btn := TToolButton.Create(Self);
  Btn.Style := tbsSeparator;
  Btn.Parent := Self;

  // Annuler
  Btn := TToolButton.Create(Self);
  Btn.Caption := 'Annuler';
  Btn.ImageIndex := 3;
  Btn.Parent := Self;

  // Rétablir
  Btn := TToolButton.Create(Self);
  Btn.Caption := 'Rétablir';
  Btn.ImageIndex := 4;
  Btn.Parent := Self;
end;
```

## Composant 1 : Traitement de Texte

### Structure du Document Texte

```pascal
type
  TTextStyle = record
    FontName: string;
    FontSize: Integer;
    Bold: Boolean;
    Italic: Boolean;
    Underline: Boolean;
    Color: TColor;
  end;

  TParagraphAlignment = (paLeft, paCenter, paRight, paJustify);

  TParagraph = class
  private
    FText: string;
    FAlignment: TParagraphAlignment;
    FLeftIndent: Integer;
    FRightIndent: Integer;
    FSpaceBefore: Integer;
    FSpaceAfter: Integer;
  public
    property Text: string read FText write FText;
    property Alignment: TParagraphAlignment read FAlignment write FAlignment;
  end;

  TTextDocument = class(TBaseDocument)
  private
    FParagraphs: TObjectList<TParagraph>;
    FDefaultStyle: TTextStyle;
    FCommandManager: TCommandManager;
  public
    constructor Create; override;
    destructor Destroy; override;

    // Implémentation des méthodes abstraites
    function New: Boolean; override;
    function Open(const AFileName: string): Boolean; override;
    function Save: Boolean; override;
    function SaveAs(const AFileName: string): Boolean; override;

    procedure Undo; override;
    procedure Redo; override;

    function GetCanUndo: Boolean; override;
    function GetCanRedo: Boolean; override;

    // Opérations sur le texte
    procedure InsertText(const AText: string; Position: Integer);
    procedure DeleteText(StartPos, Length: Integer);
    procedure FormatText(StartPos, Length: Integer; const Style: TTextStyle);

    // Gestion des paragraphes
    function AddParagraph(const AText: string): TParagraph;
    procedure DeleteParagraph(Index: Integer);

    property Paragraphs: TObjectList<TParagraph> read FParagraphs;
  end;

constructor TTextDocument.Create;  
begin
  inherited Create;
  FParagraphs := TObjectList<TParagraph>.Create(True);
  FCommandManager := TCommandManager.Create;

  // Style par défaut
  FDefaultStyle.FontName := 'Arial';
  FDefaultStyle.FontSize := 12;
  FDefaultStyle.Bold := False;
  FDefaultStyle.Italic := False;
  FDefaultStyle.Underline := False;
  FDefaultStyle.Color := clBlack;
end;

destructor TTextDocument.Destroy;  
begin
  FCommandManager.Free;
  FParagraphs.Free;
  inherited;
end;

function TTextDocument.AddParagraph(const AText: string): TParagraph;  
begin
  Result := TParagraph.Create;
  Result.Text := AText;
  Result.Alignment := paLeft;
  FParagraphs.Add(Result);
  SetModified(True);
end;
```

### Éditeur de Texte Visuel

```pascal
type
  TTextEditor = class(TForm)
  private
    FDocument: TTextDocument;
    FEditor: TRichMemo;  // ou TRichEdit
    FToolbar: TOfficeToolbar;
    FMenuBar: TOfficeMainMenu;
    FStatusBar: TOfficeStatusBar;

    procedure SetupUI;
    procedure UpdateUI;

    // Gestionnaires d'événements
    procedure OnNewClick(Sender: TObject);
    procedure OnOpenClick(Sender: TObject);
    procedure OnSaveClick(Sender: TObject);
    procedure OnUndoClick(Sender: TObject);
    procedure OnRedoClick(Sender: TObject);

    // Formatage
    procedure OnBoldClick(Sender: TObject);
    procedure OnItalicClick(Sender: TObject);
    procedure OnUnderlineClick(Sender: TObject);
    procedure OnFontChange(Sender: TObject);
    procedure OnAlignmentChange(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Document: TTextDocument read FDocument;
  end;

constructor TTextEditor.Create(AOwner: TComponent);  
begin
  inherited Create(AOwner);
  FDocument := TTextDocument.Create;
  SetupUI;
end;

procedure TTextEditor.SetupUI;  
begin
  Caption := 'Traitement de Texte - Sans titre';
  Width := 800;
  Height := 600;
  Position := poScreenCenter;

  // Barre de menu
  FMenuBar := TOfficeMainMenu.Create(Self);
  Menu := FMenuBar;

  // Barre d'outils
  FToolbar := TOfficeToolbar.Create(Self);
  FToolbar.Parent := Self;
  FToolbar.Align := alTop;

  // Éditeur
  FEditor := TRichMemo.Create(Self);
  FEditor.Parent := Self;
  FEditor.Align := alClient;
  FEditor.ScrollBars := ssAutoBoth;
  FEditor.Font.Name := 'Arial';
  FEditor.Font.Size := 12;

  // Barre d'état
  FStatusBar := TOfficeStatusBar.Create(Self);
  FStatusBar.Parent := Self;

  UpdateUI;
end;

procedure TTextEditor.OnBoldClick(Sender: TObject);  
begin
  if FEditor.SelLength > 0 then
  begin
    if fsBold in FEditor.SelAttributes.Style then
      FEditor.SelAttributes.Style := FEditor.SelAttributes.Style - [fsBold]
    else
      FEditor.SelAttributes.Style := FEditor.SelAttributes.Style + [fsBold];

    FDocument.Modified := True;
  end;
end;

procedure TTextEditor.OnSaveClick(Sender: TObject);  
begin
  if FDocument.FileName = '' then
    OnSaveAsClick(Sender)
  else
    FDocument.Save;
end;
```

### Format de Fichier Natif

Créez un format simple mais extensible :

```pascal
type
  TTextFileFormat = class
  public
    class function SaveToFile(Document: TTextDocument;
      const FileName: string): Boolean;
    class function LoadFromFile(Document: TTextDocument;
      const FileName: string): Boolean;
  end;

class function TTextFileFormat.SaveToFile(Document: TTextDocument;
  const FileName: string): Boolean;
var
  Stream: TFileStream;
  Writer: TWriter;
  Para: TParagraph;
begin
  Result := False;
  try
    Stream := TFileStream.Create(FileName, fmCreate);
    try
      Writer := TWriter.Create(Stream, 4096);
      try
        // En-tête du fichier
        Writer.WriteSignature;
        Writer.WriteString('TEXTDOC');
        Writer.WriteInteger(1); // Version

        // Métadonnées
        Writer.WriteString(Document.Title);
        Writer.WriteString(Document.Author);
        Writer.WriteFloat(Document.CreationDate);

        // Contenu
        Writer.WriteInteger(Document.Paragraphs.Count);
        for Para in Document.Paragraphs do
        begin
          Writer.WriteString(Para.Text);
          Writer.WriteInteger(Ord(Para.Alignment));
          Writer.WriteInteger(Para.LeftIndent);
          Writer.WriteInteger(Para.RightIndent);
        end;

        Result := True;
      finally
        Writer.Free;
      end;
    finally
      Stream.Free;
    end;
  except
    on E: Exception do
      ShowMessage('Erreur lors de l''enregistrement : ' + E.Message);
  end;
end;

class function TTextFileFormat.LoadFromFile(Document: TTextDocument;
  const FileName: string): Boolean;
var
  Stream: TFileStream;
  Reader: TReader;
  Signature: string;
  Version: Integer;
  Count, i: Integer;
  Para: TParagraph;
begin
  Result := False;
  try
    Stream := TFileStream.Create(FileName, fmOpenRead);
    try
      Reader := TReader.Create(Stream, 4096);
      try
        // Vérifier la signature
        Reader.ReadSignature;
        Signature := Reader.ReadString;
        if Signature <> 'TEXTDOC' then
        begin
          ShowMessage('Format de fichier invalide');
          Exit;
        end;

        Version := Reader.ReadInteger;

        // Lire les métadonnées
        Document.Title := Reader.ReadString;
        Document.Author := Reader.ReadString;
        Document.CreationDate := Reader.ReadFloat;

        // Lire le contenu
        Document.Paragraphs.Clear;
        Count := Reader.ReadInteger;
        for i := 0 to Count - 1 do
        begin
          Para := TParagraph.Create;
          Para.Text := Reader.ReadString;
          Para.Alignment := TParagraphAlignment(Reader.ReadInteger);
          Para.LeftIndent := Reader.ReadInteger;
          Para.RightIndent := Reader.ReadInteger;
          Document.Paragraphs.Add(Para);
        end;

        Result := True;
      finally
        Reader.Free;
      end;
    finally
      Stream.Free;
    end;
  except
    on E: Exception do
      ShowMessage('Erreur lors du chargement : ' + E.Message);
  end;
end;
```

## Composant 2 : Tableur

### Structure d'une Feuille de Calcul

```pascal
type
  TCellValue = record
    case ValueType: (vtEmpty, vtNumber, vtString, vtFormula, vtError) of
      vtNumber: (NumberValue: Double);
      vtString: (StringValue: string);
      vtFormula: (FormulaText: string);
      vtError: (ErrorCode: Integer);
  end;

  TCell = class
  private
    FValue: TCellValue;
    FRow: Integer;
    FColumn: Integer;
    FFormatting: TCellFormat;
  public
    constructor Create(ARow, ACol: Integer);

    procedure SetValue(const Value: Variant);
    function GetValue: Variant;
    function GetDisplayText: string;

    property Row: Integer read FRow;
    property Column: Integer read FColumn;
    property Value: TCellValue read FValue;
  end;

  TWorksheet = class
  private
    FName: string;
    FCells: TDictionary<string, TCell>;
    FRowCount: Integer;
    FColumnCount: Integer;

    function GetCellKey(Row, Col: Integer): string;
  public
    constructor Create(const AName: string);
    destructor Destroy; override;

    function GetCell(Row, Col: Integer): TCell;
    procedure SetCellValue(Row, Col: Integer; const Value: Variant);
    function GetCellValue(Row, Col: Integer): Variant;

    procedure InsertRow(Index: Integer);
    procedure DeleteRow(Index: Integer);
    procedure InsertColumn(Index: Integer);
    procedure DeleteColumn(Index: Integer);

    property Name: string read FName write FName;
    property RowCount: Integer read FRowCount;
    property ColumnCount: Integer read FColumnCount;
  end;

  TSpreadsheetDocument = class(TBaseDocument)
  private
    FWorksheets: TObjectList<TWorksheet>;
    FActiveSheet: TWorksheet;
    FCommandManager: TCommandManager;
  public
    constructor Create; override;
    destructor Destroy; override;

    function AddWorksheet(const AName: string): TWorksheet;
    procedure DeleteWorksheet(Index: Integer);
    procedure SetActiveWorksheet(Sheet: TWorksheet);

    property Worksheets: TObjectList<TWorksheet> read FWorksheets;
    property ActiveSheet: TWorksheet read FActiveSheet;
  end;

constructor TWorksheet.Create(const AName: string);  
begin
  inherited Create;
  FName := AName;
  FCells := TDictionary<string, TCell>.Create;
  FRowCount := 100;
  FColumnCount := 26;
end;

function TWorksheet.GetCellKey(Row, Col: Integer): string;  
begin
  Result := Format('%d:%d', [Row, Col]);
end;

function TWorksheet.GetCell(Row, Col: Integer): TCell;  
var
  Key: string;
begin
  Key := GetCellKey(Row, Col);
  if not FCells.TryGetValue(Key, Result) then
  begin
    Result := TCell.Create(Row, Col);
    FCells.Add(Key, Result);
  end;
end;

procedure TWorksheet.SetCellValue(Row, Col: Integer; const Value: Variant);  
var
  Cell: TCell;
begin
  Cell := GetCell(Row, Col);
  Cell.SetValue(Value);
end;
```

### Moteur de Calcul

Implémentez un moteur pour évaluer les formules :

```pascal
type
  TFormulaEngine = class
  private
    FWorksheet: TWorksheet;

    function ParseFormula(const Formula: string): TASTNode;
    function EvaluateNode(Node: TASTNode): Variant;
    function GetCellReference(const Ref: string): Variant;
  public
    constructor Create(AWorksheet: TWorksheet);
    function Evaluate(const Formula: string): Variant;
  end;

constructor TFormulaEngine.Create(AWorksheet: TWorksheet);  
begin
  inherited Create;
  FWorksheet := AWorksheet;
end;

function TFormulaEngine.Evaluate(const Formula: string): Variant;  
var
  AST: TASTNode;
begin
  try
    // Retirer le signe '=' au début
    if (Length(Formula) > 0) and (Formula[1] = '=') then
    begin
      AST := ParseFormula(Copy(Formula, 2, Length(Formula)));
      try
        Result := EvaluateNode(AST);
      finally
        AST.Free;
      end;
    end
    else
      Result := Formula;
  except
    on E: Exception do
      Result := '#ERROR!';
  end;
end;

function TFormulaEngine.GetCellReference(const Ref: string): Variant;  
var
  Col: string;
  Row: Integer;
  ColNum: Integer;
  i: Integer;
begin
  // Convertir A1, B2, etc. en coordonnées
  Col := '';
  for i := 1 to Length(Ref) do
  begin
    if Ref[i] in ['A'..'Z', 'a'..'z'] then
      Col := Col + UpCase(Ref[i])
    else
      Break;
  end;

  Row := StrToIntDef(Copy(Ref, Length(Col) + 1, Length(Ref)), 0);

  // Convertir la lettre de colonne en nombre
  ColNum := 0;
  for i := 1 to Length(Col) do
    ColNum := ColNum * 26 + (Ord(Col[i]) - Ord('A') + 1);

  Result := FWorksheet.GetCellValue(Row, ColNum);
end;
```

### Interface du Tableur

```pascal
type
  TSpreadsheetGrid = class(TStringGrid)
  private
    FWorksheet: TWorksheet;
    FFormulaBar: TEdit;

    procedure UpdateCell(Row, Col: Integer);
    procedure OnCellSelect(Sender: TObject; aCol, aRow: Integer);
    procedure OnCellEdit(Sender: TObject; aCol, aRow: Integer; var Value: string);
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetWorksheet(AWorksheet: TWorksheet);
  end;

constructor TSpreadsheetGrid.Create(AOwner: TComponent);  
begin
  inherited Create(AOwner);

  // Configuration de la grille
  FixedCols := 1;
  FixedRows := 1;
  RowCount := 101;  // 100 lignes + en-tête
  ColCount := 27;   // 26 colonnes + en-tête
  DefaultColWidth := 80;
  DefaultRowHeight := 25;
  Options := Options + [goEditing, goTabs];

  // En-têtes de colonnes (A, B, C, ...)
  for var i := 1 to 26 do
    Cells[i, 0] := Chr(Ord('A') + i - 1);

  // En-têtes de lignes (1, 2, 3, ...)
  for var i := 1 to 100 do
    Cells[0, i] := IntToStr(i);

  OnSelectCell := OnCellSelect;
end;

procedure TSpreadsheetGrid.OnCellSelect(Sender: TObject; aCol, aRow: Integer);  
var
  Cell: TCell;
begin
  if (aCol > 0) and (aRow > 0) and Assigned(FWorksheet) then
  begin
    Cell := FWorksheet.GetCell(aRow, aCol);

    // Afficher la formule dans la barre de formules
    if Cell.Value.ValueType = vtFormula then
      FFormulaBar.Text := Cell.Value.FormulaText
    else
      FFormulaBar.Text := Cell.GetDisplayText;
  end;
end;
```

## Composant 3 : Présentations

### Structure d'une Présentation

```pascal
type
  TSlideObject = class
  private
    FLeft, FTop, FWidth, FHeight: Integer;
    FVisible: Boolean;
  public
    procedure Draw(Canvas: TCanvas); virtual; abstract;
    procedure Move(DX, DY: Integer);
    procedure Resize(DW, DH: Integer);

    property Left: Integer read FLeft write FLeft;
    property Top: Integer read FTop write FTop;
    property Width: Integer read FWidth write FWidth;
    property Height: Integer read FHeight write FHeight;
  end;

  TTextBox = class(TSlideObject)
  private
    FText: string;
    FFont: TFont;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Draw(Canvas: TCanvas); override;

    property Text: string read FText write FText;
    property Font: TFont read FFont;
  end;

  TImageBox = class(TSlideObject)
  private
    FImage: TPicture;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Draw(Canvas: TCanvas); override;
    procedure LoadFromFile(const FileName: string);

    property Image: TPicture read FImage;
  end;

  TSlide = class
  private
    FTitle: string;
    FObjects: TObjectList<TSlideObject>;
    FBackground: TColor;
    FTransition: string;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddObject(Obj: TSlideObject);
    procedure RemoveObject(Obj: TSlideObject);
    procedure Draw(Canvas: TCanvas);

    property Title: string read FTitle write FTitle;
    property Objects: TObjectList<TSlideObject> read FObjects;
    property Background: TColor read FBackground write FBackground;
  end;

  TPresentationDocument = class(TBaseDocument)
  private
    FSlides: TObjectList<TSlide>;
    FCurrentSlide: Integer;
  public
    constructor Create; override;
    destructor Destroy; override;

    function AddSlide: TSlide;
    procedure DeleteSlide(Index: Integer);
    procedure MoveSlide(FromIndex, ToIndex: Integer);

    property Slides: TObjectList<TSlide> read FSlides;
    property CurrentSlide: Integer read FCurrentSlide write FCurrentSlide;
  end;

constructor TSlide.Create;  
begin
  inherited Create;
  FObjects := TObjectList<TSlideObject>.Create(True);
  FBackground := clWhite;
  FTransition := 'Aucune';
end;

procedure TSlide.Draw(Canvas: TCanvas);  
var
  Obj: TSlideObject;
begin
  // Dessiner l'arrière-plan
  Canvas.Brush.Color := FBackground;
  Canvas.FillRect(Canvas.ClipRect);

  // Dessiner tous les objets
  for Obj in FObjects do
  begin
    if Obj.FVisible then
      Obj.Draw(Canvas);
  end;
end;

procedure TTextBox.Draw(Canvas: TCanvas);  
var
  R: TRect;
begin
  R := Rect(FLeft, FTop, FLeft + FWidth, FTop + FHeight);

  Canvas.Font.Assign(FFont);
  Canvas.Brush.Style := bsClear;
  Canvas.TextRect(R, FLeft + 5, FTop + 5, FText);
end;

procedure TImageBox.Draw(Canvas: TCanvas);  
var
  R: TRect;
begin
  if Assigned(FImage.Graphic) then
  begin
    R := Rect(FLeft, FTop, FLeft + FWidth, FTop + FHeight);
    Canvas.StretchDraw(R, FImage.Graphic);
  end;
end;
```

### Éditeur de Présentation

```pascal
type
  TPresentationEditor = class(TForm)
  private
    FDocument: TPresentationDocument;
    FSlidePanel: TPanel;
    FToolPanel: TPanel;
    FSlideList: TListBox;
    FCanvas: TPaintBox;
    FSelectedObject: TSlideObject;

    procedure SetupUI;
    procedure OnSlideSelect(Sender: TObject);
    procedure OnCanvasPaint(Sender: TObject);
    procedure OnCanvasMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OnCanvasMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure OnCanvasMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);

    procedure AddTextBox;
    procedure AddImage;
    procedure UpdateSlideList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure StartPresentation;
  end;

constructor TPresentationEditor.Create(AOwner: TComponent);  
begin
  inherited Create(AOwner);
  FDocument := TPresentationDocument.Create;
  SetupUI;
end;

procedure TPresentationEditor.SetupUI;  
begin
  Caption := 'Éditeur de Présentation';
  Width := 1024;
  Height := 768;
  Position := poScreenCenter;

  // Panneau de gauche pour la liste des diapositives
  FSlideList := TListBox.Create(Self);
  FSlideList.Parent := Self;
  FSlideList.Align := alLeft;
  FSlideList.Width := 150;
  FSlideList.OnClick := OnSlideSelect;

  // Zone de dessin principale
  FSlidePanel := TPanel.Create(Self);
  FSlidePanel.Parent := Self;
  FSlidePanel.Align := alClient;
  FSlidePanel.BevelOuter := bvNone;

  FCanvas := TPaintBox.Create(Self);
  FCanvas.Parent := FSlidePanel;
  FCanvas.Align := alClient;
  FCanvas.OnPaint := OnCanvasPaint;
  FCanvas.OnMouseDown := OnCanvasMouseDown;
  FCanvas.OnMouseMove := OnCanvasMouseMove;
  FCanvas.OnMouseUp := OnCanvasMouseUp;

  // Panneau d'outils à droite
  FToolPanel := TPanel.Create(Self);
  FToolPanel.Parent := Self;
  FToolPanel.Align := alRight;
  FToolPanel.Width := 200;
end;

procedure TPresentationEditor.OnCanvasPaint(Sender: TObject);  
var
  CurrentSlide: TSlide;
begin
  if (FDocument.CurrentSlide >= 0) and
     (FDocument.CurrentSlide < FDocument.Slides.Count) then
  begin
    CurrentSlide := FDocument.Slides[FDocument.CurrentSlide];
    CurrentSlide.Draw(FCanvas.Canvas);
  end;
end;

procedure TPresentationEditor.StartPresentation;  
var
  PresentationForm: TForm;
  PaintBox: TPaintBox;
  CurrentSlideIndex: Integer;

  procedure DrawCurrentSlide;
  begin
    if (CurrentSlideIndex >= 0) and
       (CurrentSlideIndex < FDocument.Slides.Count) then
    begin
      FDocument.Slides[CurrentSlideIndex].Draw(PaintBox.Canvas);
    end;
  end;

  procedure OnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  begin
    case Key of
      VK_RIGHT, VK_SPACE, VK_RETURN:
      begin
        if CurrentSlideIndex < FDocument.Slides.Count - 1 then
        begin
          Inc(CurrentSlideIndex);
          DrawCurrentSlide;
        end;
      end;

      VK_LEFT:
      begin
        if CurrentSlideIndex > 0 then
        begin
          Dec(CurrentSlideIndex);
          DrawCurrentSlide;
        end;
      end;

      VK_ESCAPE:
        PresentationForm.Close;
    end;
  end;

begin
  CurrentSlideIndex := 0;

  // Créer une fenêtre plein écran
  PresentationForm := TForm.Create(nil);
  try
    PresentationForm.BorderStyle := bsNone;
    PresentationForm.WindowState := wsFullScreen;
    PresentationForm.Color := clBlack;
    PresentationForm.KeyPreview := True;
    PresentationForm.OnKeyDown := OnKeyDown;

    PaintBox := TPaintBox.Create(PresentationForm);
    PaintBox.Parent := PresentationForm;
    PaintBox.Align := alClient;
    PaintBox.OnPaint := procedure(Sender: TObject)
    begin
      DrawCurrentSlide;
    end;

    DrawCurrentSlide;
    PresentationForm.ShowModal;
  finally
    PresentationForm.Free;
  end;
end;
```

## Composant 4 : Gestionnaire de Base de Données

### Structure de la Base de Données

```pascal
type
  TFieldType = (ftInteger, ftString, ftFloat, ftDate, ftBoolean, ftMemo);

  TFieldDef = class
  private
    FName: string;
    FFieldType: TFieldType;
    FSize: Integer;
    FRequired: Boolean;
  public
    property Name: string read FName write FName;
    property FieldType: TFieldType read FFieldType write FFieldType;
    property Size: Integer read FSize write FSize;
    property Required: Boolean read FRequired write FRequired;
  end;

  TTableDef = class
  private
    FName: string;
    FFields: TObjectList<TFieldDef>;
    FPrimaryKey: string;
  public
    constructor Create(const AName: string);
    destructor Destroy; override;

    function AddField(const AName: string; AType: TFieldType;
      ASize: Integer = 0): TFieldDef;

    property Name: string read FName write FName;
    property Fields: TObjectList<TFieldDef> read FFields;
    property PrimaryKey: string read FPrimaryKey write FPrimaryKey;
  end;

  TDatabaseDocument = class(TBaseDocument)
  private
    FConnection: TSQLConnection;
    FTables: TObjectList<TTableDef>;
    FQueries: TStringList;
  public
    constructor Create; override;
    destructor Destroy; override;

    function Connect(const AConnectionString: string): Boolean;
    procedure Disconnect;

    function CreateTable(TableDef: TTableDef): Boolean;
    function ExecuteQuery(const SQL: string): Boolean;
    function GetTableData(const TableName: string): TDataSet;

    property Tables: TObjectList<TTableDef> read FTables;
  end;

constructor TTableDef.Create(const AName: string);  
begin
  inherited Create;
  FName := AName;
  FFields := TObjectList<TFieldDef>.Create(True);
end;

function TTableDef.AddField(const AName: string; AType: TFieldType;
  ASize: Integer): TFieldDef;
begin
  Result := TFieldDef.Create;
  Result.Name := AName;
  Result.FieldType := AType;
  Result.Size := ASize;
  FFields.Add(Result);
end;
```

### Interface du Gestionnaire de Base de Données

```pascal
type
  TDatabaseManager = class(TForm)
  private
    FDocument: TDatabaseDocument;
    FTableList: TListBox;
    FDataGrid: TDBGrid;
    FQueryEditor: TSynEdit;
    FConnectionPanel: TPanel;

    procedure SetupUI;
    procedure OnTableSelect(Sender: TObject);
    procedure OnExecuteQuery(Sender: TObject);
    procedure OnCreateTable(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

procedure TDatabaseManager.SetupUI;  
var
  Splitter: TSplitter;
begin
  Caption := 'Gestionnaire de Base de Données';
  Width := 900;
  Height := 600;

  // Panneau de connexion
  FConnectionPanel := TPanel.Create(Self);
  FConnectionPanel.Parent := Self;
  FConnectionPanel.Align := alTop;
  FConnectionPanel.Height := 60;

  // Liste des tables
  FTableList := TListBox.Create(Self);
  FTableList.Parent := Self;
  FTableList.Align := alLeft;
  FTableList.Width := 200;
  FTableList.OnClick := OnTableSelect;

  Splitter := TSplitter.Create(Self);
  Splitter.Parent := Self;
  Splitter.Align := alLeft;

  // Grille de données
  FDataGrid := TDBGrid.Create(Self);
  FDataGrid.Parent := Self;
  FDataGrid.Align := alClient;
  FDataGrid.Options := FDataGrid.Options + [dgEditing];
end;
```

## Formats de Fichiers Communs

### Support ODF (OpenDocument Format)

```pascal
type
  TODFExporter = class
  public
    class function ExportText(Doc: TTextDocument;
      const FileName: string): Boolean;
    class function ExportSpreadsheet(Doc: TSpreadsheetDocument;
      const FileName: string): Boolean;
    class function ExportPresentation(Doc: TPresentationDocument;
      const FileName: string): Boolean;
  end;

  TODFImporter = class
  public
    class function ImportText(Doc: TTextDocument;
      const FileName: string): Boolean;
    class function ImportSpreadsheet(Doc: TSpreadsheetDocument;
      const FileName: string): Boolean;
    class function ImportPresentation(Doc: TPresentationDocument;
      const FileName: string): Boolean;
  end;

class function TODFExporter.ExportText(Doc: TTextDocument;
  const FileName: string): Boolean;
var
  Zip: TZipper;
  XMLDoc: TXMLDocument;
  RootNode, BodyNode, ParaNode: TDOMNode;
  Para: TParagraph;
  ContentStream: TStringStream;
begin
  Result := False;
  try
    // Créer le document XML content.xml
    XMLDoc := TXMLDocument.Create;
    try
      RootNode := XMLDoc.CreateElement('office:document-content');
      XMLDoc.AppendChild(RootNode);

      BodyNode := XMLDoc.CreateElement('office:body');
      RootNode.AppendChild(BodyNode);

      // Ajouter les paragraphes
      for Para in Doc.Paragraphs do
      begin
        ParaNode := XMLDoc.CreateElement('text:p');
        ParaNode.TextContent := Para.Text;
        BodyNode.AppendChild(ParaNode);
      end;

      // Convertir en chaîne
      ContentStream := TStringStream.Create('');
      try
        WriteXMLFile(XMLDoc, ContentStream);

        // Créer l'archive ZIP (format ODT)
        Zip := TZipper.Create;
        try
          Zip.FileName := FileName;

          // Ajouter content.xml
          Zip.Entries.AddFileEntry(ContentStream, 'content.xml');

          // Ajouter meta.xml (métadonnées)
          // Ajouter styles.xml (styles)
          // Ajouter META-INF/manifest.xml

          Zip.ZipAllFiles;
          Result := True;
        finally
          Zip.Free;
        end;
      finally
        ContentStream.Free;
      end;
    finally
      XMLDoc.Free;
    end;
  except
    on E: Exception do
      ShowMessage('Erreur d''export ODF : ' + E.Message);
  end;
end;
```

### Support PDF

```pascal
type
  TPDFExporter = class
  private
    class procedure AddTextToPDF(PDF: TPDFDocument; Doc: TTextDocument);
    class procedure AddSpreadsheetToPDF(PDF: TPDFDocument; Doc: TSpreadsheetDocument);
  public
    class function ExportToPDF(Doc: TBaseDocument;
      const FileName: string): Boolean;
  end;

class function TPDFExporter.ExportToPDF(Doc: TBaseDocument;
  const FileName: string): Boolean;
var
  PDF: TPDFDocument;
begin
  Result := False;
  try
    PDF := TPDFDocument.Create(nil);
    try
      PDF.StartDocument;

      if Doc is TTextDocument then
        AddTextToPDF(PDF, TTextDocument(Doc))
      else if Doc is TSpreadsheetDocument then
        AddSpreadsheetToPDF(PDF, TSpreadsheetDocument(Doc));

      PDF.SaveToFile(FileName);
      Result := True;
    finally
      PDF.Free;
    end;
  except
    on E: Exception do
      ShowMessage('Erreur d''export PDF : ' + E.Message);
  end;
end;
```

## Système de Plugins

### Architecture de Plugins

```pascal
type
  IOfficePlugin = interface
    ['{12345678-1234-1234-1234-123456789012}']
    function GetName: string;
    function GetVersion: string;
    function GetDescription: string;
    procedure Initialize(App: TApplication);
    procedure Finalize;
  end;

  TPluginManager = class
  private
    FPlugins: TList<IOfficePlugin>;
    FPluginPath: string;

    procedure LoadPlugin(const FileName: string);
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadAllPlugins;
    procedure UnloadAllPlugins;
    function GetPlugin(const Name: string): IOfficePlugin;

    property Plugins: TList<IOfficePlugin> read FPlugins;
  end;

constructor TPluginManager.Create;  
begin
  inherited Create;
  FPlugins := TList<IOfficePlugin>.Create;

  {$IFDEF WINDOWS}
  FPluginPath := ExtractFilePath(ParamStr(0)) + 'plugins\';
  {$ENDIF}
  {$IFDEF UNIX}
  FPluginPath := ExtractFilePath(ParamStr(0)) + 'plugins/';
  {$ENDIF}
end;

procedure TPluginManager.LoadAllPlugins;  
var
  SearchRec: TSearchRec;
  PluginFile: string;
begin
  {$IFDEF WINDOWS}
  if FindFirst(FPluginPath + '*.dll', faAnyFile, SearchRec) = 0 then
  {$ENDIF}
  {$IFDEF UNIX}
  if FindFirst(FPluginPath + '*.so', faAnyFile, SearchRec) = 0 then
  {$ENDIF}
  begin
    repeat
      PluginFile := FPluginPath + SearchRec.Name;
      LoadPlugin(PluginFile);
    until FindNext(SearchRec) <> 0;
    FindClose(SearchRec);
  end;
end;
```

### Exemple de Plugin : Correcteur Orthographique

```pascal
type
  TSpellCheckerPlugin = class(TInterfacedObject, IOfficePlugin)
  private
    FDictionary: TStringList;

    procedure LoadDictionary(const Language: string);
  public
    constructor Create;
    destructor Destroy; override;

    // IOfficePlugin
    function GetName: string;
    function GetVersion: string;
    function GetDescription: string;
    procedure Initialize(App: TApplication);
    procedure Finalize;

    // Fonctionnalités du correcteur
    function CheckWord(const Word: string): Boolean;
    function GetSuggestions(const Word: string): TStringList;
    procedure AddToDictionary(const Word: string);
  end;

constructor TSpellCheckerPlugin.Create;  
begin
  inherited Create;
  FDictionary := TStringList.Create;
  FDictionary.Sorted := True;
  LoadDictionary('fr-FR');
end;

procedure TSpellCheckerPlugin.LoadDictionary(const Language: string);  
var
  DictFile: string;
begin
  {$IFDEF WINDOWS}
  DictFile := ExtractFilePath(ParamStr(0)) + 'dictionaries\' + Language + '.dic';
  {$ENDIF}
  {$IFDEF UNIX}
  DictFile := ExtractFilePath(ParamStr(0)) + 'dictionaries/' + Language + '.dic';
  {$ENDIF}

  if FileExists(DictFile) then
    FDictionary.LoadFromFile(DictFile);
end;

function TSpellCheckerPlugin.CheckWord(const Word: string): Boolean;  
begin
  Result := FDictionary.IndexOf(LowerCase(Word)) >= 0;
end;

function TSpellCheckerPlugin.GetSuggestions(const Word: string): TStringList;  
var
  i: Integer;
  DictWord: string;
  Distance: Integer;
begin
  Result := TStringList.Create;

  // Algorithme de distance de Levenshtein simplifié
  for i := 0 to FDictionary.Count - 1 do
  begin
    DictWord := FDictionary[i];
    Distance := LevenshteinDistance(LowerCase(Word), DictWord);

    if Distance <= 2 then  // Mots similaires
      Result.Add(DictWord);

    if Result.Count >= 10 then
      Break;
  end;
end;
```

## Intégration et Communication Inter-Applications

### Système de Messagerie Interne

```pascal
type
  TOfficeMessage = record
    Sender: string;
    Receiver: string;
    MessageType: string;
    Data: Variant;
  end;

  TMessageBus = class
  private
    FSubscribers: TDictionary<string, TList<TNotifyEvent>>;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Subscribe(const MessageType: string; Handler: TNotifyEvent);
    procedure Unsubscribe(const MessageType: string; Handler: TNotifyEvent);
    procedure Publish(const Msg: TOfficeMessage);
  end;

constructor TMessageBus.Create;  
begin
  inherited Create;
  FSubscribers := TDictionary<string, TList<TNotifyEvent>>.Create;
end;

procedure TMessageBus.Publish(const Msg: TOfficeMessage);  
var
  Handlers: TList<TNotifyEvent>;
  Handler: TNotifyEvent;
begin
  if FSubscribers.TryGetValue(Msg.MessageType, Handlers) then
  begin
    for Handler in Handlers do
      Handler(Self);
  end;
end;
```

### Copier-Coller Universel

```pascal
type
  TOfficeClipboard = class
  private
    FData: TMemoryStream;
    FFormat: string;

    class var FInstance: TOfficeClipboard;
  public
    constructor Create;
    destructor Destroy; override;

    class function Instance: TOfficeClipboard;

    procedure SetData(const AFormat: string; AData: TStream);
    function GetData(const AFormat: string): TStream;
    function HasFormat(const AFormat: string): Boolean;

    // Formats spécifiques
    procedure SetText(const Text: string);
    function GetText: string;
    procedure SetTable(Data: TStringGrid);
    function GetTable: TStringGrid;
  end;

class function TOfficeClipboard.Instance: TOfficeClipboard;  
begin
  if not Assigned(FInstance) then
    FInstance := TOfficeClipboard.Create;
  Result := FInstance;
end;

procedure TOfficeClipboard.SetText(const Text: string);  
var
  Stream: TStringStream;
begin
  Stream := TStringStream.Create(Text);
  try
    SetData('text/plain', Stream);
  finally
    Stream.Free;
  end;
end;

function TOfficeClipboard.GetText: string;  
var
  Stream: TStream;
  StringStream: TStringStream;
begin
  Stream := GetData('text/plain');
  if Assigned(Stream) then
  begin
    StringStream := TStringStream.Create('');
    try
      StringStream.CopyFrom(Stream, 0);
      Result := StringStream.DataString;
    finally
      StringStream.Free;
    end;
  end
  else
    Result := '';
end;
```

## Configuration et Préférences

### Gestionnaire de Paramètres

```pascal
type
  TOfficeSettings = class
  private
    FSettings: TJSONObject;
    FFileName: string;

    class var FInstance: TOfficeSettings;
  public
    constructor Create;
    destructor Destroy; override;

    class function Instance: TOfficeSettings;

    procedure Load;
    procedure Save;

    function GetString(const Key, Default: string): string;
    procedure SetString(const Key, Value: string);
    function GetInteger(const Key: string; Default: Integer): Integer;
    procedure SetInteger(const Key: string; Value: Integer);
    function GetBoolean(const Key: string; Default: Boolean): Boolean;
    procedure SetBoolean(const Key: string; Value: Boolean);
  end;

constructor TOfficeSettings.Create;  
begin
  inherited Create;
  FSettings := TJSONObject.Create;

  {$IFDEF WINDOWS}
  FFileName := GetEnvironmentVariable('APPDATA') + '\OfficeSuite\settings.json';
  {$ENDIF}
  {$IFDEF UNIX}
  FFileName := GetEnvironmentVariable('HOME') + '/.config/officesuite/settings.json';
  {$ENDIF}

  Load;
end;

procedure TOfficeSettings.Load;  
var
  JSONStr: string;
  JSONParser: TJSONParser;
begin
  if FileExists(FFileName) then
  begin
    try
      with TStringList.Create do
      try
        LoadFromFile(FFileName);
        JSONStr := Text;
      finally
        Free;
      end;

      JSONParser := TJSONParser.Create(JSONStr);
      try
        FSettings.Free;
        FSettings := JSONParser.Parse as TJSONObject;
      finally
        JSONParser.Free;
      end;
    except
      on E: Exception do
        ShowMessage('Erreur de chargement des paramètres : ' + E.Message);
    end;
  end;
end;

procedure TOfficeSettings.Save;  
var
  JSONStr: string;
  Dir: string;
begin
  try
    Dir := ExtractFileDir(FFileName);
    if not DirectoryExists(Dir) then
      ForceDirectories(Dir);

    JSONStr := FSettings.FormatJSON;
    with TStringList.Create do
    try
      Text := JSONStr;
      SaveToFile(FFileName);
    finally
      Free;
    end;
  except
    on E: Exception do
      ShowMessage('Erreur de sauvegarde des paramètres : ' + E.Message);
  end;
end;
```

## Aspects Multi-Plateformes

### Gestion des Chemins

```pascal
type
  TPathManager = class
  public
    class function GetDocumentsPath: string;
    class function GetTemplatePath: string;
    class function GetPluginPath: string;
    class function GetUserDataPath: string;
    class function NormalizePath(const Path: string): string;
  end;

class function TPathManager.GetDocumentsPath: string;  
begin
  {$IFDEF WINDOWS}
  Result := GetEnvironmentVariable('USERPROFILE') + '\Documents\';
  {$ENDIF}
  {$IFDEF UNIX}
  Result := GetEnvironmentVariable('HOME') + '/Documents/';
  {$ENDIF}
end;

class function TPathManager.GetUserDataPath: string;  
begin
  {$IFDEF WINDOWS}
  Result := GetEnvironmentVariable('APPDATA') + '\OfficeSuite\';
  {$ENDIF}
  {$IFDEF UNIX}
  Result := GetEnvironmentVariable('HOME') + '/.local/share/officesuite/';
  {$ENDIF}

  if not DirectoryExists(Result) then
    ForceDirectories(Result);
end;

class function TPathManager.NormalizePath(const Path: string): string;  
begin
  Result := Path;
  {$IFDEF WINDOWS}
  Result := StringReplace(Result, '/', '\', [rfReplaceAll]);
  {$ENDIF}
  {$IFDEF UNIX}
  Result := StringReplace(Result, '\', '/', [rfReplaceAll]);
  {$ENDIF}
end;
```

### Thèmes et Apparence

```pascal
type
  TThemeManager = class
  private
    FCurrentTheme: string;
    FThemes: TDictionary<string, TTheme>;

    procedure ApplyThemeToForm(Form: TForm; Theme: TTheme);
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadThemes;
    procedure ApplyTheme(const ThemeName: string);
    function GetAvailableThemes: TStringList;

    property CurrentTheme: string read FCurrentTheme;
  end;

procedure TThemeManager.ApplyTheme(const ThemeName: string);  
var
  Theme: TTheme;
  i: Integer;
begin
  if FThemes.TryGetValue(ThemeName, Theme) then
  begin
    FCurrentTheme := ThemeName;

    // Appliquer aux formulaires ouverts
    for i := 0 to Screen.FormCount - 1 do
      ApplyThemeToForm(Screen.Forms[i], Theme);
  end;
end;

procedure TThemeManager.ApplyThemeToForm(Form: TForm; Theme: TTheme);  
var
  i: Integer;
  Component: TComponent;
begin
  Form.Color := Theme.BackgroundColor;
  Form.Font.Color := Theme.TextColor;

  // Appliquer récursivement aux composants
  for i := 0 to Form.ComponentCount - 1 do
  begin
    Component := Form.Components[i];

    if Component is TPanel then
      TPanel(Component).Color := Theme.PanelColor
    else if Component is TButton then
    begin
      TButton(Component).Color := Theme.ButtonColor;
      TButton(Component).Font.Color := Theme.ButtonTextColor;
    end;
    // ... autres composants
  end;
end;
```

## Fonctionnalités Avancées

### Collaboration en Temps Réel

```pascal
type
  TCollaborationClient = class
  private
    FSocket: TWebSocketClient;
    FDocumentID: string;
    FUserID: string;

    procedure OnMessageReceived(const Msg: string);
    procedure SendChange(const Change: TDocumentChange);
  public
    constructor Create(const ServerURL: string);
    destructor Destroy; override;

    procedure Connect(const DocID, UserID: string);
    procedure Disconnect;
    procedure BroadcastChange(const Change: TDocumentChange);
  end;

procedure TCollaborationClient.BroadcastChange(const Change: TDocumentChange);  
var
  JSONMsg: TJSONObject;
begin
  JSONMsg := TJSONObject.Create;
  try
    JSONMsg.Add('type', 'document_change');
    JSONMsg.Add('document_id', FDocumentID);
    JSONMsg.Add('user_id', FUserID);
    JSONMsg.Add('change', Change.ToJSON);

    FSocket.Send(JSONMsg.AsJSON);
  finally
    JSONMsg.Free;
  end;
end;
```

### Historique des Versions

```pascal
type
  TDocumentVersion = class
  private
    FVersion: Integer;
    FTimestamp: TDateTime;
    FAuthor: string;
    FComment: string;
    FData: TMemoryStream;
  public
    constructor Create;
    destructor Destroy; override;

    property Version: Integer read FVersion write FVersion;
    property Timestamp: TDateTime read FTimestamp;
    property Author: string read FAuthor write FAuthor;
    property Comment: string read FComment write FComment;
  end;

  TVersionControl = class
  private
    FVersions: TObjectList<TDocumentVersion>;
    FCurrentVersion: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    function SaveVersion(Document: TBaseDocument;
      const Comment: string): Integer;
    function RestoreVersion(Version: Integer): TBaseDocument;
    function CompareVersions(V1, V2: Integer): string;

    property Versions: TObjectList<TDocumentVersion> read FVersions;
    property CurrentVersion: Integer read FCurrentVersion;
  end;
```

### Recherche Globale

```pascal
type
  TSearchResult = record
    FileName: string;
    DocumentType: string;
    Location: string;
    Preview: string;
    Relevance: Double;
  end;

  TGlobalSearch = class
  private
    FIndexPath: string;
    FIndex: TDictionary<string, TList<TSearchResult>>;

    procedure IndexDocument(const FileName: string);
    function CalculateRelevance(const SearchTerm, Content: string): Double;
    function ExtractPreview(const Content: string; Position: Integer): string;
  public
    constructor Create;
    destructor Destroy; override;

    procedure BuildIndex(const SearchPath: string);
    function Search(const Query: string): TArray<TSearchResult>;
    procedure UpdateIndex(const FileName: string);

    property IndexPath: string read FIndexPath write FIndexPath;
  end;

constructor TGlobalSearch.Create;  
begin
  inherited Create;
  FIndex := TDictionary<string, TList<TSearchResult>>.Create;
  FIndexPath := TPathManager.GetUserDataPath + 'search.idx';
end;

destructor TGlobalSearch.Destroy;  
var
  List: TList<TSearchResult>;
begin
  for List in FIndex.Values do
    List.Free;
  FIndex.Free;
  inherited;
end;

procedure TGlobalSearch.IndexDocument(const FileName: string);  
var
  Content: TStringList;
  Words: TStringList;
  Word: string;
  i, Position: Integer;
  Result: TSearchResult;
  ResultList: TList<TSearchResult>;
begin
  Content := TStringList.Create;
  Words := TStringList.Create;
  try
    // Charger le contenu du document
    Content.LoadFromFile(FileName);

    // Extraire les mots
    ExtractWords(Content.Text, Words);

    // Indexer chaque mot
    for i := 0 to Words.Count - 1 do
    begin
      Word := LowerCase(Words[i]);

      if Length(Word) < 3 then
        Continue; // Ignorer les mots trop courts

      if not FIndex.TryGetValue(Word, ResultList) then
      begin
        ResultList := TList<TSearchResult>.Create;
        FIndex.Add(Word, ResultList);
      end;

      // Créer un résultat de recherche
      Result.FileName := FileName;
      Result.DocumentType := DetectDocumentType(FileName);
      Result.Location := Format('Position %d', [i]);
      Result.Preview := ExtractPreview(Content.Text, i * 10);
      Result.Relevance := 1.0;

      ResultList.Add(Result);
    end;
  finally
    Words.Free;
    Content.Free;
  end;
end;

function TGlobalSearch.Search(const Query: string): TArray<TSearchResult>;  
var
  Words: TStringList;
  Word: string;
  ResultList: TList<TSearchResult>;
  AllResults: TList<TSearchResult>;
  Result: TSearchResult;
begin
  AllResults := TList<TSearchResult>.Create;
  Words := TStringList.Create;
  try
    // Découper la requête en mots
    ExtractWords(Query, Words);

    // Rechercher chaque mot
    for Word in Words do
    begin
      if FIndex.TryGetValue(LowerCase(Word), ResultList) then
      begin
        for Result in ResultList do
          AllResults.Add(Result);
      end;
    end;

    // Trier par pertinence
    AllResults.Sort(TComparer<TSearchResult>.Construct(
      function(const A, B: TSearchResult): Integer
      begin
        Result := CompareValue(B.Relevance, A.Relevance);
      end
    ));

    Result := AllResults.ToArray;
  finally
    Words.Free;
    AllResults.Free;
  end;
end;

function TGlobalSearch.ExtractPreview(const Content: string;
  Position: Integer): string;
const
  PreviewLength = 150;
var
  StartPos, EndPos: Integer;
begin
  StartPos := Max(1, Position - PreviewLength div 2);
  EndPos := Min(Length(Content), Position + PreviewLength div 2);

  Result := Copy(Content, StartPos, EndPos - StartPos);

  // Ajouter des ellipses si nécessaire
  if StartPos > 1 then
    Result := '...' + Result;
  if EndPos < Length(Content) then
    Result := Result + '...';
end;
```

## Macros et Automatisation

### Enregistreur de Macros

```pascal
type
  TMacroAction = class
  private
    FActionType: string;
    FParameters: TJSONObject;
  public
    constructor Create(const AType: string);
    destructor Destroy; override;

    procedure Execute; virtual; abstract;
    function ToJSON: TJSONObject;

    property ActionType: string read FActionType;
    property Parameters: TJSONObject read FParameters;
  end;

  TMacro = class
  private
    FName: string;
    FActions: TObjectList<TMacroAction>;
    FDescription: string;
    FShortcut: TShortCut;
  public
    constructor Create(const AName: string);
    destructor Destroy; override;

    procedure AddAction(Action: TMacroAction);
    procedure Execute;
    procedure SaveToFile(const FileName: string);
    procedure LoadFromFile(const FileName: string);

    property Name: string read FName write FName;
    property Description: string read FDescription write FDescription;
    property Shortcut: TShortCut read FShortcut write FShortcut;
  end;

  TMacroRecorder = class
  private
    FRecording: Boolean;
    FCurrentMacro: TMacro;

    procedure RecordAction(Action: TMacroAction);
  public
    constructor Create;
    destructor Destroy; override;

    procedure StartRecording(const MacroName: string);
    procedure StopRecording;
    function GetMacro: TMacro;

    property Recording: Boolean read FRecording;
  end;

constructor TMacro.Create(const AName: string);  
begin
  inherited Create;
  FName := AName;
  FActions := TObjectList<TMacroAction>.Create(True);
end;

procedure TMacro.Execute;  
var
  Action: TMacroAction;
begin
  for Action in FActions do
  begin
    try
      Action.Execute;
    except
      on E: Exception do
        ShowMessage(Format('Erreur lors de l''exécution de la macro : %s',
          [E.Message]));
    end;
  end;
end;

procedure TMacro.SaveToFile(const FileName: string);  
var
  JSONObj: TJSONObject;
  JSONArray: TJSONArray;
  Action: TMacroAction;
  Content: string;
begin
  JSONObj := TJSONObject.Create;
  try
    JSONObj.Add('name', FName);
    JSONObj.Add('description', FDescription);

    JSONArray := TJSONArray.Create;
    for Action in FActions do
      JSONArray.Add(Action.ToJSON);

    JSONObj.Add('actions', JSONArray);

    Content := JSONObj.FormatJSON;
    with TStringList.Create do
    try
      Text := Content;
      SaveToFile(FileName);
    finally
      Free;
    end;
  finally
    JSONObj.Free;
  end;
end;
```

### Actions de Macro Spécifiques

```pascal
type
  TInsertTextAction = class(TMacroAction)
  private
    FText: string;
  public
    constructor Create(const AText: string);
    procedure Execute; override;
  end;

  TFormatTextAction = class(TMacroAction)
  private
    FBold: Boolean;
    FItalic: Boolean;
    FFontSize: Integer;
  public
    constructor Create(ABold, AItalic: Boolean; AFontSize: Integer);
    procedure Execute; override;
  end;

  TInsertTableAction = class(TMacroAction)
  private
    FRows: Integer;
    FCols: Integer;
  public
    constructor Create(ARows, ACols: Integer);
    procedure Execute; override;
  end;

constructor TInsertTextAction.Create(const AText: string);  
begin
  inherited Create('InsertText');
  FText := AText;
  FParameters.Add('text', FText);
end;

procedure TInsertTextAction.Execute;  
var
  ActiveEditor: TTextEditor;
begin
  // Obtenir l'éditeur actif
  ActiveEditor := GetActiveTextEditor;
  if Assigned(ActiveEditor) then
    ActiveEditor.InsertText(FText);
end;
```

## Templates et Modèles

### Gestionnaire de Modèles

```pascal
type
  TDocumentTemplate = class
  private
    FName: string;
    FCategory: string;
    FDescription: string;
    FThumbnail: TPicture;
    FFilePath: string;
  public
    constructor Create;
    destructor Destroy; override;

    function CreateDocument: TBaseDocument;

    property Name: string read FName write FName;
    property Category: string read FCategory write FCategory;
    property Description: string read FDescription write FDescription;
    property Thumbnail: TPicture read FThumbnail;
    property FilePath: string read FFilePath write FFilePath;
  end;

  TTemplateManager = class
  private
    FTemplates: TObjectList<TDocumentTemplate>;
    FTemplatePath: string;

    procedure LoadTemplatesFromDirectory;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadTemplates;
    function GetTemplate(const Name: string): TDocumentTemplate;
    function GetTemplatesByCategory(const Category: string): TArray<TDocumentTemplate>;
    procedure AddTemplate(Template: TDocumentTemplate);

    property Templates: TObjectList<TDocumentTemplate> read FTemplates;
  end;

constructor TTemplateManager.Create;  
begin
  inherited Create;
  FTemplates := TObjectList<TDocumentTemplate>.Create(True);
  FTemplatePath := TPathManager.GetUserDataPath + 'templates/';

  if not DirectoryExists(FTemplatePath) then
    ForceDirectories(FTemplatePath);

  LoadTemplates;
end;

procedure TTemplateManager.LoadTemplates;  
var
  SearchRec: TSearchRec;
  Template: TDocumentTemplate;
  TemplateFile: string;
begin
  FTemplates.Clear;

  if FindFirst(FTemplatePath + '*.*', faAnyFile, SearchRec) = 0 then
  begin
    repeat
      if (SearchRec.Attr and faDirectory) = 0 then
      begin
        TemplateFile := FTemplatePath + SearchRec.Name;

        Template := TDocumentTemplate.Create;
        Template.Name := ChangeFileExt(SearchRec.Name, '');
        Template.FilePath := TemplateFile;

        FTemplates.Add(Template);
      end;
    until FindNext(SearchRec) <> 0;
    FindClose(SearchRec);
  end;
end;

function TTemplateManager.GetTemplatesByCategory(
  const Category: string): TArray<TDocumentTemplate>;
var
  List: TList<TDocumentTemplate>;
  Template: TDocumentTemplate;
begin
  List := TList<TDocumentTemplate>.Create;
  try
    for Template in FTemplates do
    begin
      if Template.Category = Category then
        List.Add(Template);
    end;
    Result := List.ToArray;
  finally
    List.Free;
  end;
end;
```

### Dialogue de Sélection de Modèle

```pascal
type
  TTemplateDialog = class(TForm)
  private
    FListView: TListView;
    FPreview: TImage;
    FDescription: TMemo;
    FSelectedTemplate: TDocumentTemplate;

    procedure SetupUI;
    procedure PopulateTemplates;
    procedure OnTemplateSelect(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;

    class function ShowDialog(out Template: TDocumentTemplate): Boolean;
  end;

constructor TTemplateDialog.Create(AOwner: TComponent);  
begin
  inherited Create(AOwner);
  SetupUI;
  PopulateTemplates;
end;

procedure TTemplateDialog.SetupUI;  
var
  Panel: TPanel;
  ButtonPanel: TPanel;
  OKBtn, CancelBtn: TButton;
begin
  Caption := 'Choisir un modèle';
  Width := 800;
  Height := 600;
  Position := poScreenCenter;

  // Liste des modèles
  FListView := TListView.Create(Self);
  FListView.Parent := Self;
  FListView.Align := alLeft;
  FListView.Width := 300;
  FListView.ViewStyle := vsReport;
  FListView.Columns.Add.Caption := 'Nom';
  FListView.Columns.Add.Caption := 'Catégorie';
  FListView.OnSelectItem := OnTemplateSelect;

  // Panneau de droite
  Panel := TPanel.Create(Self);
  Panel.Parent := Self;
  Panel.Align := alClient;
  Panel.BevelOuter := bvNone;

  // Aperçu
  FPreview := TImage.Create(Self);
  FPreview.Parent := Panel;
  FPreview.Align := alTop;
  FPreview.Height := 300;
  FPreview.Proportional := True;
  FPreview.Center := True;

  // Description
  FDescription := TMemo.Create(Self);
  FDescription.Parent := Panel;
  FDescription.Align := alClient;
  FDescription.ReadOnly := True;
  FDescription.ScrollBars := ssVertical;

  // Boutons
  ButtonPanel := TPanel.Create(Self);
  ButtonPanel.Parent := Self;
  ButtonPanel.Align := alBottom;
  ButtonPanel.Height := 50;

  OKBtn := TButton.Create(Self);
  OKBtn.Parent := ButtonPanel;
  OKBtn.Caption := 'OK';
  OKBtn.ModalResult := mrOK;
  OKBtn.Left := ButtonPanel.Width - 180;
  OKBtn.Top := 10;

  CancelBtn := TButton.Create(Self);
  CancelBtn.Parent := ButtonPanel;
  CancelBtn.Caption := 'Annuler';
  CancelBtn.ModalResult := mrCancel;
  CancelBtn.Left := ButtonPanel.Width - 90;
  CancelBtn.Top := 10;
end;

class function TTemplateDialog.ShowDialog(
  out Template: TDocumentTemplate): Boolean;
var
  Dialog: TTemplateDialog;
begin
  Dialog := TTemplateDialog.Create(nil);
  try
    Result := Dialog.ShowModal = mrOK;
    if Result then
      Template := Dialog.FSelectedTemplate;
  finally
    Dialog.Free;
  end;
end;
```

## Impression et Aperçu

### Gestionnaire d'Impression

```pascal
type
  TPrintSettings = record
    PageSize: string;        // A4, Letter, etc.
    Orientation: string;     // Portrait, Landscape
    Margins: record
      Top, Bottom, Left, Right: Integer;
    end;
    ColorMode: Boolean;      // Couleur ou N&B
    Quality: string;         // Draft, Normal, High
    Copies: Integer;
  end;

  TPrintManager = class
  private
    FSettings: TPrintSettings;
    FPrinterSetupDialog: TPrinterSetupDialog;
    FPrintDialog: TPrintDialog;

    procedure InitializeDefaults;
  public
    constructor Create;
    destructor Destroy; override;

    function ShowPrinterDialog: Boolean;
    function ShowPrintDialog: Boolean;

    procedure PrintDocument(Document: TBaseDocument);
    procedure PrintPreview(Document: TBaseDocument);

    property Settings: TPrintSettings read FSettings write FSettings;
  end;

constructor TPrintManager.Create;  
begin
  inherited Create;
  FPrinterSetupDialog := TPrinterSetupDialog.Create(nil);
  FPrintDialog := TPrintDialog.Create(nil);
  InitializeDefaults;
end;

procedure TPrintManager.InitializeDefaults;  
begin
  FSettings.PageSize := 'A4';
  FSettings.Orientation := 'Portrait';
  FSettings.Margins.Top := 20;
  FSettings.Margins.Bottom := 20;
  FSettings.Margins.Left := 20;
  FSettings.Margins.Right := 20;
  FSettings.ColorMode := True;
  FSettings.Quality := 'Normal';
  FSettings.Copies := 1;
end;

procedure TPrintManager.PrintDocument(Document: TBaseDocument);  
var
  Printer: TPrinter;
  Page: Integer;
begin
  if not ShowPrintDialog then
    Exit;

  Printer := TPrinter.Create;
  try
    Printer.BeginDoc;
    try
      // Imprimer chaque page
      for Page := 1 to GetPageCount(Document) do
      begin
        if Page > 1 then
          Printer.NewPage;

        RenderPage(Document, Printer.Canvas, Page);
      end;
    finally
      Printer.EndDoc;
    end;
  finally
    Printer.Free;
  end;
end;

procedure TPrintManager.PrintPreview(Document: TBaseDocument);  
var
  PreviewForm: TForm;
  ScrollBox: TScrollBox;
  PaintBox: TPaintBox;

  procedure OnPaintBoxPaint(Sender: TObject);
  begin
    // Dessiner l'aperçu
    RenderPage(Document, PaintBox.Canvas, 1);
  end;

begin
  PreviewForm := TForm.Create(nil);
  try
    PreviewForm.Caption := 'Aperçu avant impression';
    PreviewForm.Width := 800;
    PreviewForm.Height := 600;
    PreviewForm.Position := poScreenCenter;

    ScrollBox := TScrollBox.Create(PreviewForm);
    ScrollBox.Parent := PreviewForm;
    ScrollBox.Align := alClient;

    PaintBox := TPaintBox.Create(ScrollBox);
    PaintBox.Parent := ScrollBox;
    PaintBox.Width := 595;  // Largeur A4 en pixels
    PaintBox.Height := 842; // Hauteur A4 en pixels
    PaintBox.OnPaint := OnPaintBoxPaint;

    PreviewForm.ShowModal;
  finally
    PreviewForm.Free;
  end;
end;
```

## Import/Export Avancés

### Gestionnaire d'Import/Export

```pascal
type
  IFileFormatHandler = interface
    ['{23456789-2345-2345-2345-234567890123}']
    function GetFormatName: string;
    function GetExtensions: TStringDynArray; // uses Types
    function CanImport: Boolean;
    function CanExport: Boolean;
    function Import(const FileName: string): TBaseDocument;
    function Export(Document: TBaseDocument; const FileName: string): Boolean;
  end;

  // Note : nécessite {$mode delphi} et uses Generics.Collections
  TFormatHandlerRegistry = class
  private
    FHandlers: TList<IFileFormatHandler>;
  public
    constructor Create;
    destructor Destroy; override;

    procedure RegisterHandler(Handler: IFileFormatHandler);
    function GetHandler(const Extension: string): IFileFormatHandler;
    function GetImportFilters: string;
    function GetExportFilters: string;
  end;

  TDOCXHandler = class(TInterfacedObject, IFileFormatHandler)
  public
    function GetFormatName: string;
    function GetExtensions: TStringDynArray; // uses Types
    function CanImport: Boolean;
    function CanExport: Boolean;
    function Import(const FileName: string): TBaseDocument;
    function Export(Document: TBaseDocument; const FileName: string): Boolean;
  end;

function TDOCXHandler.GetFormatName: string;  
begin
  Result := 'Microsoft Word Document';
end;

function TDOCXHandler.GetExtensions: TStringDynArray;  
begin
  SetLength(Result, 1);
  Result[0] := '.docx';
end;

function TDOCXHandler.Import(const FileName: string): TBaseDocument;  
var
  Zip: TUnZipper;
  XMLDoc: TXMLDocument;
  TempDir: string;
  ContentFile: string;
  Doc: TTextDocument;
begin
  Doc := TTextDocument.Create;
  try
    // Extraire le fichier DOCX (format ZIP)
    TempDir := GetTempDir + 'docx_import\';
    ForceDirectories(TempDir);

    Zip := TUnZipper.Create;
    try
      Zip.FileName := FileName;
      Zip.OutputPath := TempDir;
      Zip.UnZipAllFiles;
    finally
      Zip.Free;
    end;

    // Lire le contenu XML
    ContentFile := TempDir + 'word\document.xml';
    if FileExists(ContentFile) then
    begin
      ReadXMLFile(XMLDoc, ContentFile);
      try
        // Parser le XML et remplir le document
        ParseDOCXContent(XMLDoc, Doc);
      finally
        XMLDoc.Free;
      end;
    end;

    Result := Doc;
  except
    Doc.Free;
    raise;
  end;
end;
```

### Handler pour Format ODT

```pascal
type
  TODTHandler = class(TInterfacedObject, IFileFormatHandler)
  public
    function GetFormatName: string;
    function GetExtensions: TStringDynArray; // uses Types
    function CanImport: Boolean;
    function CanExport: Boolean;
    function Import(const FileName: string): TBaseDocument;
    function Export(Document: TBaseDocument; const FileName: string): Boolean;
  end;

function TODTHandler.Export(Document: TBaseDocument;
  const FileName: string): Boolean;
var
  Zip: TZipper;
  ContentXML: TXMLDocument;
  ManifestXML: TXMLDocument;
  TempDir: string;
begin
  Result := False;
  try
    TempDir := GetTempDir + 'odt_export\';
    ForceDirectories(TempDir);

    // Créer content.xml
    ContentXML := CreateODTContent(Document);
    try
      WriteXMLFile(ContentXML, TempDir + 'content.xml');
    finally
      ContentXML.Free;
    end;

    // Créer manifest.xml
    ManifestXML := CreateODTManifest;
    try
      WriteXMLFile(ManifestXML, TempDir + 'META-INF\manifest.xml');
    finally
      ManifestXML.Free;
    end;

    // Créer meta.xml
    CreateODTMeta(Document, TempDir + 'meta.xml');

    // Créer styles.xml
    CreateODTStyles(TempDir + 'styles.xml');

    // Compresser en fichier ODT
    Zip := TZipper.Create;
    try
      Zip.FileName := FileName;
      Zip.Entries.AddFileEntry(TempDir + 'content.xml', 'content.xml');
      Zip.Entries.AddFileEntry(TempDir + 'meta.xml', 'meta.xml');
      Zip.Entries.AddFileEntry(TempDir + 'styles.xml', 'styles.xml');
      Zip.Entries.AddFileEntry(TempDir + 'META-INF\manifest.xml',
        'META-INF/manifest.xml');
      Zip.ZipAllFiles;
      Result := True;
    finally
      Zip.Free;
    end;
  except
    on E: Exception do
      ShowMessage('Erreur d''export ODT : ' + E.Message);
  end;
end;
```

## Déploiement et Installation

### Script d'Installation Multi-Plateforme

#### Pour Windows (Inno Setup Script)

```pascal
[Setup]
AppName=Suite Bureautique  
AppVersion=1.0  
DefaultDirName={pf}\OfficeSuite  
DefaultGroupName=Suite Bureautique  
OutputDir=.\Output  
OutputBaseFilename=OfficeSuiteSetup

[Files]
Source: "bin\Windows\*"; DestDir: "{app}"; Flags: recursesubdirs  
Source: "templates\*"; DestDir: "{app}\templates"; Flags: recursesubdirs  
Source: "plugins\*"; DestDir: "{app}\plugins"; Flags: recursesubdirs

[Icons]
Name: "{group}\Traitement de Texte"; Filename: "{app}\texteditor.exe"  
Name: "{group}\Tableur"; Filename: "{app}\spreadsheet.exe"  
Name: "{group}\Présentation"; Filename: "{app}\presentation.exe"  
Name: "{group}\Base de Données"; Filename: "{app}\database.exe"

[Registry]
Root: HKCR; Subkey: ".odt"; ValueType: string; ValueData: "OfficeSuite.Document"  
Root: HKCR; Subkey: "OfficeSuite.Document"; ValueType: string; ValueData: "Document OfficeSuite"  
Root: HKCR; Subkey: "OfficeSuite.Document\DefaultIcon"; ValueType: string; ValueData: "{app}\texteditor.exe,0"  
Root: HKCR; Subkey: "OfficeSuite.Document\shell\open\command"; ValueType: string; ValueData: """{app}\texteditor.exe"" ""%1"""
```

#### Pour Ubuntu (Script Debian)

```bash
#!/bin/bash
# install.sh - Script d'installation pour Ubuntu

INSTALL_DIR="/opt/officesuite"  
BIN_DIR="/usr/local/bin"  
DESKTOP_DIR="/usr/share/applications"  
ICON_DIR="/usr/share/icons/hicolor/256x256/apps"

echo "Installation de la Suite Bureautique..."

# Créer les répertoires
sudo mkdir -p "$INSTALL_DIR"  
sudo mkdir -p "$ICON_DIR"

# Copier les fichiers
sudo cp -r bin/Linux/* "$INSTALL_DIR/"  
sudo cp -r templates "$INSTALL_DIR/"  
sudo cp -r plugins "$INSTALL_DIR/"  
sudo cp icons/* "$ICON_DIR/"

# Créer des liens symboliques
sudo ln -sf "$INSTALL_DIR/texteditor" "$BIN_DIR/officesuite-text"  
sudo ln -sf "$INSTALL_DIR/spreadsheet" "$BIN_DIR/officesuite-calc"  
sudo ln -sf "$INSTALL_DIR/presentation" "$BIN_DIR/officesuite-present"  
sudo ln -sf "$INSTALL_DIR/database" "$BIN_DIR/officesuite-db"

# Créer les entrées de menu
cat > /tmp/texteditor.desktop << EOF
[Desktop Entry]
Name=Traitement de Texte  
Comment=Éditeur de documents texte  
Exec=officesuite-text %F  
Icon=officesuite-text  
Terminal=false  
Type=Application  
Categories=Office;WordProcessor;  
MimeType=application/vnd.oasis.opendocument.text;application/msword;  
EOF

sudo mv /tmp/texteditor.desktop "$DESKTOP_DIR/"

# Mettre à jour la base de données MIME
sudo update-desktop-database  
sudo update-mime-database /usr/share/mime

echo "Installation terminée!"
```

### Création d'un Package Debian

```bash
# Créer la structure du package
mkdir -p officesuite_1.0-1/DEBIAN  
mkdir -p officesuite_1.0-1/opt/officesuite  
mkdir -p officesuite_1.0-1/usr/share/applications  
mkdir -p officesuite_1.0-1/usr/share/icons

# Créer le fichier control
cat > officesuite_1.0-1/DEBIAN/control << EOF  
Package: officesuite  
Version: 1.0-1  
Section: office  
Priority: optional  
Architecture: amd64  
Depends: libgtk-3-0, libsqlite3-0  
Maintainer: Votre Nom <email@example.com>  
Description: Suite bureautique complète
 Suite bureautique incluant traitement de texte,
 tableur, présentation et base de données.
EOF

# Copier les fichiers
cp -r bin/Linux/* officesuite_1.0-1/opt/officesuite/  
cp -r templates officesuite_1.0-1/opt/officesuite/  
cp *.desktop officesuite_1.0-1/usr/share/applications/  
cp icons/* officesuite_1.0-1/usr/share/icons/

# Créer le package
dpkg-deb --build officesuite_1.0-1
```

## Mises à Jour Automatiques

### Vérificateur de Mises à Jour

```pascal
type
  TUpdateInfo = record
    Version: string;
    ReleaseDate: TDateTime;
    DownloadURL: string;
    ChangeLog: string;
    Critical: Boolean;
  end;

  TUpdateChecker = class
  private
    FCurrentVersion: string;
    FUpdateURL: string;

    function GetCurrentVersion: string;
    function CompareVersions(V1, V2: string): Integer;
  public
    constructor Create(const AUpdateURL: string);

    function CheckForUpdates: Boolean;
    function GetUpdateInfo: TUpdateInfo;
    procedure DownloadUpdate(const Info: TUpdateInfo);
    procedure InstallUpdate(const FileName: string);

    property CurrentVersion: string read FCurrentVersion;
  end;

constructor TUpdateChecker.Create(const AUpdateURL: string);  
begin
  inherited Create;
  FUpdateURL := AUpdateURL;
  FCurrentVersion := GetCurrentVersion;
end;

function TUpdateChecker.CheckForUpdates: Boolean;  
var
  HTTP: TFPHTTPClient;
  Response: string;
  JSONData: TJSONData;
  LatestVersion: string;
begin
  Result := False;
  try
    HTTP := TFPHTTPClient.Create(nil);
    try
      Response := HTTP.Get(FUpdateURL + '/version.json');

      JSONData := GetJSON(Response);
      try
        LatestVersion := JSONData.FindPath('version').AsString;
        Result := CompareVersions(LatestVersion, FCurrentVersion) > 0;
      finally
        JSONData.Free;
      end;
    finally
      HTTP.Free;
    end;
  except
    on E: Exception do
    begin
      // Erreur silencieuse - pas de connexion internet
      Result := False;
    end;
  end;
end;

function TUpdateChecker.GetUpdateInfo: TUpdateInfo;  
var
  HTTP: TFPHTTPClient;
  Response: string;
  JSONData: TJSONObject;
begin
  try
    HTTP := TFPHTTPClient.Create(nil);
    try
      Response := HTTP.Get(FUpdateURL + '/update-info.json');

      JSONData := GetJSON(Response) as TJSONObject;
      try
        Result.Version := JSONData.Get('version', '');
        Result.ReleaseDate := ISO8601ToDate(JSONData.Get('release_date', ''));
        Result.DownloadURL := JSONData.Get('download_url', '');
        Result.ChangeLog := JSONData.Get('changelog', '');
        Result.Critical := JSONData.Get('critical', False);
      finally
        JSONData.Free;
      end;
    finally
      HTTP.Free;
    end;
  except
    on E: Exception do
      raise Exception.Create('Impossible de récupérer les informations de mise à jour');
  end;
end;

procedure TUpdateChecker.DownloadUpdate(const Info: TUpdateInfo);  
var
  HTTP: TFPHTTPClient;
  DownloadStream: TFileStream;
  TotalSize, Downloaded: Int64;
  ProgressForm: TForm;
  ProgressBar: TProgressBar;
  StatusLabel: TLabel;

  procedure UpdateProgress(Sender: TObject; const ContentLength, CurrentPos: Int64);
  begin
    if ContentLength > 0 then
    begin
      ProgressBar.Position := Round((CurrentPos / ContentLength) * 100);
      StatusLabel.Caption := Format('Téléchargement : %d / %d MB',
        [CurrentPos div 1048576, ContentLength div 1048576]);
      Application.ProcessMessages;
    end;
  end;

var
  UpdateFile: string;
begin
  {$IFDEF WINDOWS}
  UpdateFile := GetTempDir + 'officesuite_update.exe';
  {$ENDIF}
  {$IFDEF UNIX}
  UpdateFile := GetTempDir + 'officesuite_update.deb';
  {$ENDIF}

  // Créer la fenêtre de progression
  ProgressForm := TForm.Create(nil);
  try
    ProgressForm.Caption := 'Téléchargement de la mise à jour';
    ProgressForm.Width := 400;
    ProgressForm.Height := 150;
    ProgressForm.Position := poScreenCenter;
    ProgressForm.BorderStyle := bsDialog;

    StatusLabel := TLabel.Create(ProgressForm);
    StatusLabel.Parent := ProgressForm;
    StatusLabel.Caption := 'Préparation du téléchargement...';
    StatusLabel.Left := 20;
    StatusLabel.Top := 20;

    ProgressBar := TProgressBar.Create(ProgressForm);
    ProgressBar.Parent := ProgressForm;
    ProgressBar.Left := 20;
    ProgressBar.Top := 50;
    ProgressBar.Width := 360;
    ProgressBar.Height := 25;

    ProgressForm.Show;

    // Télécharger le fichier
    HTTP := TFPHTTPClient.Create(nil);
    try
      HTTP.OnDataReceived := UpdateProgress;

      DownloadStream := TFileStream.Create(UpdateFile, fmCreate);
      try
        HTTP.Get(Info.DownloadURL, DownloadStream);
      finally
        DownloadStream.Free;
      end;
    finally
      HTTP.Free;
    end;

    ShowMessage('Téléchargement terminé. L''installation va commencer.');
    InstallUpdate(UpdateFile);

  finally
    ProgressForm.Free;
  end;
end;

procedure TUpdateChecker.InstallUpdate(const FileName: string);
{$IFDEF WINDOWS}
var
  StartInfo: TStartupInfo;
  ProcInfo: TProcessInformation;
  CommandLine: string;
{$ENDIF}
begin
  {$IFDEF WINDOWS}
  // Lancer l'installateur Windows
  CommandLine := Format('"%s" /SILENT', [FileName]);

  FillChar(StartInfo, SizeOf(StartInfo), 0);
  StartInfo.cb := SizeOf(StartInfo);

  if CreateProcess(nil, PChar(CommandLine), nil, nil, False,
    0, nil, nil, StartInfo, ProcInfo) then
  begin
    CloseHandle(ProcInfo.hProcess);
    CloseHandle(ProcInfo.hThread);
    Application.Terminate; // Fermer l'application actuelle
  end
  else
    raise Exception.Create('Impossible de lancer l''installateur');
  {$ENDIF}

  {$IFDEF UNIX}
  // Installer le package Debian
  if ExecuteProcess('pkexec', ['dpkg', '-i', FileName]) = 0 then
  begin
    ShowMessage('Mise à jour installée avec succès. Veuillez redémarrer l''application.');
    Application.Terminate;
  end
  else
    raise Exception.Create('Erreur lors de l''installation');
  {$ENDIF}
end;

function TUpdateChecker.CompareVersions(V1, V2: string): Integer;  
var
  Parts1, Parts2: TStringList;
  i, Num1, Num2: Integer;
begin
  Parts1 := TStringList.Create;
  Parts2 := TStringList.Create;
  try
    Parts1.Delimiter := '.';
    Parts1.StrictDelimiter := True;
    Parts1.DelimitedText := V1;

    Parts2.Delimiter := '.';
    Parts2.StrictDelimiter := True;
    Parts2.DelimitedText := V2;

    for i := 0 to Max(Parts1.Count, Parts2.Count) - 1 do
    begin
      if i < Parts1.Count then
        Num1 := StrToIntDef(Parts1[i], 0)
      else
        Num1 := 0;

      if i < Parts2.Count then
        Num2 := StrToIntDef(Parts2[i], 0)
      else
        Num2 := 0;

      if Num1 > Num2 then
        Exit(1)
      else if Num1 < Num2 then
        Exit(-1);
    end;

    Result := 0; // Versions identiques
  finally
    Parts1.Free;
    Parts2.Free;
  end;
end;
```

### Dialogue de Mise à Jour

```pascal
type
  TUpdateDialog = class(TForm)
  private
    FUpdateInfo: TUpdateInfo;
    FChangeLogMemo: TMemo;
    FUpdateBtn: TButton;
    FLaterBtn: TButton;
    FSkipBtn: TButton;

    procedure SetupUI;
    procedure OnUpdateClick(Sender: TObject);
    procedure OnLaterClick(Sender: TObject);
    procedure OnSkipClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent; const AUpdateInfo: TUpdateInfo); reintroduce;

    class function ShowDialog(const UpdateInfo: TUpdateInfo): TModalResult;
  end;

constructor TUpdateDialog.Create(AOwner: TComponent;
  const AUpdateInfo: TUpdateInfo);
begin
  inherited Create(AOwner);
  FUpdateInfo := AUpdateInfo;
  SetupUI;
end;

procedure TUpdateDialog.SetupUI;  
var
  TitleLabel: TLabel;
  VersionLabel: TLabel;
  DateLabel: TLabel;
  ChangeLogLabel: TLabel;
  ButtonPanel: TPanel;
begin
  Caption := 'Mise à jour disponible';
  Width := 500;
  Height := 400;
  Position := poScreenCenter;
  BorderStyle := bsDialog;

  // Titre
  TitleLabel := TLabel.Create(Self);
  TitleLabel.Parent := Self;
  TitleLabel.Caption := 'Une nouvelle version est disponible !';
  TitleLabel.Font.Size := 12;
  TitleLabel.Font.Style := [fsBold];
  TitleLabel.Left := 20;
  TitleLabel.Top := 20;

  // Version
  VersionLabel := TLabel.Create(Self);
  VersionLabel.Parent := Self;
  VersionLabel.Caption := Format('Version : %s', [FUpdateInfo.Version]);
  VersionLabel.Left := 20;
  VersionLabel.Top := 50;

  // Date
  DateLabel := TLabel.Create(Self);
  DateLabel.Parent := Self;
  DateLabel.Caption := Format('Date de sortie : %s',
    [FormatDateTime('dd/mm/yyyy', FUpdateInfo.ReleaseDate)]);
  DateLabel.Left := 20;
  DateLabel.Top := 70;

  // Mise à jour critique
  if FUpdateInfo.Critical then
  begin
    var CriticalLabel := TLabel.Create(Self);
    CriticalLabel.Parent := Self;
    CriticalLabel.Caption := '⚠ Mise à jour de sécurité importante';
    CriticalLabel.Font.Color := clRed;
    CriticalLabel.Font.Style := [fsBold];
    CriticalLabel.Left := 20;
    CriticalLabel.Top := 90;
  end;

  // Changelog
  ChangeLogLabel := TLabel.Create(Self);
  ChangeLogLabel.Parent := Self;
  ChangeLogLabel.Caption := 'Nouveautés :';
  ChangeLogLabel.Left := 20;
  ChangeLogLabel.Top := 120;

  FChangeLogMemo := TMemo.Create(Self);
  FChangeLogMemo.Parent := Self;
  FChangeLogMemo.Left := 20;
  FChangeLogMemo.Top := 140;
  FChangeLogMemo.Width := 460;
  FChangeLogMemo.Height := 150;
  FChangeLogMemo.ReadOnly := True;
  FChangeLogMemo.ScrollBars := ssVertical;
  FChangeLogMemo.Text := FUpdateInfo.ChangeLog;

  // Panneau de boutons
  ButtonPanel := TPanel.Create(Self);
  ButtonPanel.Parent := Self;
  ButtonPanel.Align := alBottom;
  ButtonPanel.Height := 60;
  ButtonPanel.BevelOuter := bvNone;

  FUpdateBtn := TButton.Create(Self);
  FUpdateBtn.Parent := ButtonPanel;
  FUpdateBtn.Caption := 'Mettre à jour maintenant';
  FUpdateBtn.Width := 150;
  FUpdateBtn.Height := 30;
  FUpdateBtn.Left := ButtonPanel.Width - 330;
  FUpdateBtn.Top := 15;
  FUpdateBtn.OnClick := OnUpdateClick;

  FLaterBtn := TButton.Create(Self);
  FLaterBtn.Parent := ButtonPanel;
  FLaterBtn.Caption := 'Plus tard';
  FLaterBtn.Width := 80;
  FLaterBtn.Height := 30;
  FLaterBtn.Left := ButtonPanel.Width - 170;
  FLaterBtn.Top := 15;
  FLaterBtn.OnClick := OnLaterClick;

  FSkipBtn := TButton.Create(Self);
  FSkipBtn.Parent := ButtonPanel;
  FSkipBtn.Caption := 'Ignorer';
  FSkipBtn.Width := 80;
  FSkipBtn.Height := 30;
  FSkipBtn.Left := ButtonPanel.Width - 85;
  FSkipBtn.Top := 15;
  FSkipBtn.OnClick := OnSkipClick;
  FSkipBtn.Enabled := not FUpdateInfo.Critical;
end;

procedure TUpdateDialog.OnUpdateClick(Sender: TObject);  
begin
  ModalResult := mrYes;
end;

procedure TUpdateDialog.OnLaterClick(Sender: TObject);  
begin
  ModalResult := mrNo;
end;

procedure TUpdateDialog.OnSkipClick(Sender: TObject);  
begin
  ModalResult := mrIgnore;
end;

class function TUpdateDialog.ShowDialog(
  const UpdateInfo: TUpdateInfo): TModalResult;
var
  Dialog: TUpdateDialog;
begin
  Dialog := TUpdateDialog.Create(nil, UpdateInfo);
  try
    Result := Dialog.ShowModal;
  finally
    Dialog.Free;
  end;
end;
```

## Gestion des Licences et Activation

### Système de Licence

```pascal
type
  TLicenseType = (ltTrial, ltPersonal, ltProfessional, ltEnterprise);

  TLicenseInfo = record
    LicenseKey: string;
    LicenseType: TLicenseType;
    RegisteredTo: string;
    Company: string;
    ExpiryDate: TDateTime;
    MachineID: string;
    Activated: Boolean;
  end;

  TLicenseManager = class
  private
    FLicense: TLicenseInfo;
    FLicenseFile: string;

    function GenerateMachineID: string;
    function ValidateLicenseKey(const Key: string): Boolean;
    function EncryptLicenseData(const Data: string): string;
    function DecryptLicenseData(const Data: string): string;
  public
    constructor Create;
    destructor Destroy; override;

    function LoadLicense: Boolean;
    function SaveLicense: Boolean;
    function ActivateLicense(const Key, Name, Company: string): Boolean;
    function DeactivateLicense: Boolean;
    function IsLicenseValid: Boolean;
    function GetDaysRemaining: Integer;

    property License: TLicenseInfo read FLicense;
  end;

constructor TLicenseManager.Create;  
begin
  inherited Create;

  {$IFDEF WINDOWS}
  FLicenseFile := GetEnvironmentVariable('PROGRAMDATA') +
    '\OfficeSuite\license.dat';
  {$ENDIF}
  {$IFDEF UNIX}
  FLicenseFile := '/var/lib/officesuite/license.dat';
  {$ENDIF}

  LoadLicense;
end;

function TLicenseManager.GenerateMachineID: string;  
var
  {$IFDEF WINDOWS}
  VolumeSerialNumber: DWORD;
  MaxComponentLength: DWORD;
  FileSystemFlags: DWORD;
  {$ENDIF}
  {$IFDEF UNIX}
  Process: TProcess;
  Output: TStringList;
  {$ENDIF}
begin
  {$IFDEF WINDOWS}
  GetVolumeInformation('C:\', nil, 0, @VolumeSerialNumber,
    MaxComponentLength, FileSystemFlags, nil, 0);
  Result := IntToHex(VolumeSerialNumber, 8);
  {$ENDIF}

  {$IFDEF UNIX}
  Process := TProcess.Create(nil);
  Output := TStringList.Create;
  try
    Process.Executable := 'cat';
    Process.Parameters.Add('/etc/machine-id');
    Process.Options := [poWaitOnExit, poUsePipes];
    Process.Execute;

    Output.LoadFromStream(Process.Output);
    if Output.Count > 0 then
      Result := Trim(Output[0])
    else
      Result := '';
  finally
    Output.Free;
    Process.Free;
  end;
  {$ENDIF}
end;

function TLicenseManager.ValidateLicenseKey(const Key: string): Boolean;  
var
  Parts: TStringList;
  CheckSum: Integer;
  i: Integer;
begin
  Result := False;

  // Format attendu : XXXX-XXXX-XXXX-XXXX
  Parts := TStringList.Create;
  try
    Parts.Delimiter := '-';
    Parts.StrictDelimiter := True;
    Parts.DelimitedText := Key;

    if Parts.Count <> 4 then
      Exit;

    // Vérifier chaque partie
    for i := 0 to Parts.Count - 1 do
    begin
      if Length(Parts[i]) <> 4 then
        Exit;
    end;

    // Calculer et vérifier le checksum (dernière partie)
    CheckSum := 0;
    for i := 0 to 2 do
      CheckSum := CheckSum + StrToIntDef('$' + Parts[i], 0);

    Result := IntToHex(CheckSum mod 65536, 4) = Parts[3];
  finally
    Parts.Free;
  end;
end;

function TLicenseManager.ActivateLicense(const Key, Name,
  Company: string): Boolean;
var
  HTTP: TFPHTTPClient;
  Request, Response: TJSONObject;
  PostData: TStringStream;
begin
  Result := False;

  // Valider le format de la clé
  if not ValidateLicenseKey(Key) then
  begin
    ShowMessage('Clé de licence invalide');
    Exit;
  end;

  try
    HTTP := TFPHTTPClient.Create(nil);
    try
      // Préparer la requête d'activation
      Request := TJSONObject.Create;
      try
        Request.Add('license_key', Key);
        Request.Add('machine_id', GenerateMachineID);
        Request.Add('name', Name);
        Request.Add('company', Company);

        PostData := TStringStream.Create(Request.AsJSON);
        try
          HTTP.RequestBody := PostData;
          HTTP.AddHeader('Content-Type', 'application/json');

          // Envoyer au serveur d'activation
          var ResponseStr := HTTP.Post('https://license.officesuite.com/activate');

          Response := GetJSON(ResponseStr) as TJSONObject;
          try
            if Response.Get('success', False) then
            begin
              FLicense.LicenseKey := Key;
              FLicense.RegisteredTo := Name;
              FLicense.Company := Company;
              FLicense.LicenseType := TLicenseType(Response.Get('type', 0));
              FLicense.ExpiryDate := ISO8601ToDate(Response.Get('expiry', ''));
              FLicense.MachineID := GenerateMachineID;
              FLicense.Activated := True;

              SaveLicense;
              Result := True;
            end
            else
              ShowMessage('Erreur d''activation : ' +
                Response.Get('message', 'Erreur inconnue'));
          finally
            Response.Free;
          end;
        finally
          PostData.Free;
        end;
      finally
        Request.Free;
      end;
    finally
      HTTP.Free;
    end;
  except
    on E: Exception do
      ShowMessage('Erreur de connexion : ' + E.Message);
  end;
end;

function TLicenseManager.IsLicenseValid: Boolean;  
begin
  Result := FLicense.Activated and
            (FLicense.MachineID = GenerateMachineID) and
            ((FLicense.LicenseType = ltTrial) or (FLicense.ExpiryDate > Now));
end;

function TLicenseManager.GetDaysRemaining: Integer;  
begin
  if FLicense.LicenseType = ltTrial then
    Result := DaysBetween(Now, FLicense.ExpiryDate)
  else
    Result := -1; // Illimité
end;
```

### Dialogue d'Activation

```pascal
type
  TActivationDialog = class(TForm)
  private
    FLicenseKeyEdit: TEdit;
    FNameEdit: TEdit;
    FCompanyEdit: TEdit;
    FTrialBtn: TButton;
    FActivateBtn: TButton;

    procedure SetupUI;
    procedure OnActivateClick(Sender: TObject);
    procedure OnTrialClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;

    class function ShowDialog(out LicenseManager: TLicenseManager): Boolean;
  end;

procedure TActivationDialog.SetupUI;  
var
  Panel: TPanel;
  Label1, Label2, Label3: TLabel;
begin
  Caption := 'Activation de la Suite Bureautique';
  Width := 450;
  Height := 300;
  Position := poScreenCenter;
  BorderStyle := bsDialog;

  Panel := TPanel.Create(Self);
  Panel.Parent := Self;
  Panel.Align := alTop;
  Panel.Height := 80;
  Panel.BevelOuter := bvNone;
  Panel.Color := clWhite;

  var TitleLabel := TLabel.Create(Self);
  TitleLabel.Parent := Panel;
  TitleLabel.Caption := 'Bienvenue dans la Suite Bureautique';
  TitleLabel.Font.Size := 14;
  TitleLabel.Font.Style := [fsBold];
  TitleLabel.Left := 20;
  TitleLabel.Top := 20;

  var SubLabel := TLabel.Create(Self);
  SubLabel.Parent := Panel;
  SubLabel.Caption := 'Veuillez entrer votre clé de licence pour continuer';
  SubLabel.Left := 20;
  SubLabel.Top := 50;

  // Clé de licence
  Label1 := TLabel.Create(Self);
  Label1.Parent := Self;
  Label1.Caption := 'Clé de licence :';
  Label1.Left := 20;
  Label1.Top := 100;

  FLicenseKeyEdit := TEdit.Create(Self);
  FLicenseKeyEdit.Parent := Self;
  FLicenseKeyEdit.Left := 20;
  FLicenseKeyEdit.Top := 120;
  FLicenseKeyEdit.Width := 410;
  FLicenseKeyEdit.TextHint := 'XXXX-XXXX-XXXX-XXXX';

  // Nom
  Label2 := TLabel.Create(Self);
  Label2.Parent := Self;
  Label2.Caption := 'Nom :';
  Label2.Left := 20;
  Label2.Top := 150;

  FNameEdit := TEdit.Create(Self);
  FNameEdit.Parent := Self;
  FNameEdit.Left := 20;
  FNameEdit.Top := 170;
  FNameEdit.Width := 410;

  // Société
  Label3 := TLabel.Create(Self);
  Label3.Parent := Self;
  Label3.Caption := 'Société (optionnel) :';
  Label3.Left := 20;
  Label3.Top := 200;

  FCompanyEdit := TEdit.Create(Self);
  FCompanyEdit.Parent := Self;
  FCompanyEdit.Left := 20;
  FCompanyEdit.Top := 220;
  FCompanyEdit.Width := 410;

  // Boutons
  FActivateBtn := TButton.Create(Self);
  FActivateBtn.Parent := Self;
  FActivateBtn.Caption := 'Activer';
  FActivateBtn.Left := 260;
  FActivateBtn.Top := 260;
  FActivateBtn.Width := 80;
  FActivateBtn.OnClick := OnActivateClick;
  FActivateBtn.Default := True;

  FTrialBtn := TButton.Create(Self);
  FTrialBtn.Parent := Self;
  FTrialBtn.Caption := 'Essai gratuit (30 jours)';
  FTrialBtn.Left := 350;
  FTrialBtn.Top := 260;
  FTrialBtn.Width := 150;
  FTrialBtn.OnClick := OnTrialClick;
end;

procedure TActivationDialog.OnActivateClick(Sender: TObject);  
var
  LicMgr: TLicenseManager;
begin
  if Trim(FLicenseKeyEdit.Text) = '' then
  begin
    ShowMessage('Veuillez entrer une clé de licence');
    FLicenseKeyEdit.SetFocus;
    Exit;
  end;

  if Trim(FNameEdit.Text) = '' then
  begin
    ShowMessage('Veuillez entrer votre nom');
    FNameEdit.SetFocus;
    Exit;
  end;

  LicMgr := TLicenseManager.Create;
  try
    if LicMgr.ActivateLicense(FLicenseKeyEdit.Text,
                              FNameEdit.Text,
                              FCompanyEdit.Text) then
    begin
      ShowMessage('Licence activée avec succès !');
      ModalResult := mrOK;
    end;
  finally
    LicMgr.Free;
  end;
end;

procedure TActivationDialog.OnTrialClick(Sender: TObject);  
var
  LicMgr: TLicenseManager;
begin
  LicMgr := TLicenseManager.Create;
  try
    LicMgr.License.LicenseType := ltTrial;
    LicMgr.License.RegisteredTo := 'Version d''évaluation';
    LicMgr.License.ExpiryDate := Now + 30;
    LicMgr.License.Activated := True;
    LicMgr.License.MachineID := LicMgr.GenerateMachineID;
    LicMgr.SaveLicense;

    ShowMessage('Version d''essai activée pour 30 jours');
    ModalResult := mrOK;
  finally
    LicMgr.Free;
  end;
end;
```

## Télémétrie et Rapports d'Erreur

### Collecteur de Télémétrie

```pascal
type
  TTelemetryEvent = record
    EventType: string;
    EventData: TJSONObject;
    Timestamp: TDateTime;
    UserID: string;
  end;

  TTelemetryManager = class
  private
    FEnabled: Boolean;
    FAnonymous: Boolean;
    FEvents: TList<TTelemetryEvent>;
    FUserID: string;

    procedure SendBatch;
  public
    constructor Create;
    destructor Destroy; override;

    procedure TrackEvent(const EventType: string; Data: TJSONObject = nil);
    procedure TrackFeatureUsage(const Feature: string);
    procedure TrackPerformance(const Operation: string; Duration: Integer);
    procedure Flush;

    property Enabled: Boolean read FEnabled write FEnabled;
    property Anonymous: Boolean read FAnonymous write FAnonymous;
  end;

constructor TTelemetryManager.Create;  
begin
  inherited Create;
  FEvents := TList<TTelemetryEvent>.Create;
  FEnabled := TOfficeSettings.Instance.GetBoolean('telemetry_enabled', True);
  FAnonymous := TOfficeSettings.Instance.GetBoolean('telemetry_anonymous', True);

  if FAnonymous then
    FUserID := GenerateUUID
  else
    FUserID := TOfficeSettings.Instance.GetString('user_id', '');
end;

procedure TTelemetryManager.TrackEvent(const EventType: string;
  Data: TJSONObject);
var
  Event: TTelemetryEvent;
begin
  if not FEnabled then
    Exit;

  Event.EventType := EventType;
  Event.EventData := Data;
  Event.Timestamp := Now;
  Event.UserID := FUserID;

  FEvents.Add(Event);

  // Envoyer par batch de 10 événements
  if FEvents.Count >= 10 then
    SendBatch;
end;

procedure TTelemetryManager.TrackFeatureUsage(const Feature: string);  
var
  Data: TJSONObject;
begin
  Data := TJSONObject.Create;
  try
    Data.Add('feature', Feature);
    TrackEvent('feature_usage', Data);
  finally
    Data.Free;
  end;
end;

procedure TTelemetryManager.SendBatch;  
var
  HTTP: TFPHTTPClient;
  JSONArray: TJSONArray;
  Event: TTelemetryEvent;
  PostData: TStringStream;
begin
  if FEvents.Count = 0 then
    Exit;

  try
    JSONArray := TJSONArray.Create;
    try
      for Event in FEvents do
      begin
        var EventObj := TJSONObject.Create;
        EventObj.Add('type', Event.EventType);
        EventObj.Add('timestamp', DateTimeToISO8601(Event.Timestamp));
        EventObj.Add('user_id', Event.UserID);
        if Assigned(Event.EventData) then
          EventObj.Add('data', Event.EventData.Clone as TJSONObject);
        JSONArray.Add(EventObj);
      end;

      HTTP := TFPHTTPClient.Create(nil);
      try
        PostData := TStringStream.Create(JSONArray.AsJSON);
        try
          HTTP.RequestBody := PostData;
          HTTP.AddHeader('Content-Type', 'application/json');
          HTTP.Post('https://telemetry.officesuite.com/events');
        finally
          PostData.Free;
        end;
      finally
        HTTP.Free;
      end;

      FEvents.Clear;
    finally
      JSONArray.Free;
    end;
  except
    // Erreur silencieuse - la télémétrie ne doit pas bloquer l'application
  end;
end;
```

### Rapport d'Erreur Automatique

```pascal
type
  TCrashReporter = class
  private
    FEnabled: Boolean;

    function GatherSystemInfo: TJSONObject;
    function GatherApplicationState: TJSONObject;
  public
    constructor Create;

    procedure ReportException(E: Exception; const Context: string = '');
    procedure EnableCrashReporting(Enabled: Boolean);

    property Enabled: Boolean read FEnabled write FEnabled;
  end;

constructor TCrashReporter.Create;  
begin
  inherited Create;
  FEnabled := TOfficeSettings.Instance.GetBoolean('crash_reporting', True);
end;

function TCrashReporter.GatherSystemInfo: TJSONObject;  
var
  Process: TProcess;
  Output: TStringList;
begin
  Result := TJSONObject.Create;

  {$IFDEF WINDOWS}
  Result.Add('os', 'Windows');
  Result.Add('os_version', TOSVersion.ToString);
  Result.Add('architecture', {$IFDEF CPU64}'x64'{$ELSE}'x86'{$ENDIF});
  {$ENDIF}

  {$IFDEF UNIX}
  Result.Add('os', 'Linux');

  // Obtenir la version du système
  Process := TProcess.Create(nil);
  Output := TStringList.Create;
  try
    Process.Executable := 'lsb_release';
    Process.Parameters.Add('-d');
    Process.Parameters.Add('-s');
    Process.Options := [poWaitOnExit, poUsePipes];
    Process.Execute;

    Output.LoadFromStream(Process.Output);
    if Output.Count > 0 then
      Result.Add('os_version', Trim(Output[0]));
  finally
    Output.Free;
    Process.Free;
  end;

  Result.Add('architecture', {$IFDEF CPUX64}'x64'{$ELSE}'x86'{$ENDIF});
  {$ENDIF}

  // Informations communes
  Result.Add('app_version', GetCurrentVersion);
  Result.Add('fpc_version', {$I %FPCVERSION%});
  Result.Add('memory_available', GetAvailableMemory);
end;

function TCrashReporter.GatherApplicationState: TJSONObject;  
var
  i: Integer;
  FormList: TJSONArray;
begin
  Result := TJSONObject.Create;

  // Document actif
  if Assigned(ActiveDocument) then
  begin
    Result.Add('active_document', ActiveDocument.FileName);
    Result.Add('document_modified', ActiveDocument.Modified);
  end;

  // Formulaires ouverts
  FormList := TJSONArray.Create;
  for i := 0 to Screen.FormCount - 1 do
    FormList.Add(Screen.Forms[i].ClassName);
  Result.Add('open_forms', FormList);

  // Dernière action utilisateur
  Result.Add('last_action', GetLastUserAction);
end;

procedure TCrashReporter.ReportException(E: Exception; const Context: string);  
var
  HTTP: TFPHTTPClient;
  Report: TJSONObject;
  PostData: TStringStream;
  StackTrace: TStringList;
begin
  if not FEnabled then
    Exit;

  try
    Report := TJSONObject.Create;
    try
      // Informations sur l'exception
      Report.Add('exception_class', E.ClassName);
      Report.Add('exception_message', E.Message);
      Report.Add('context', Context);
      Report.Add('timestamp', DateTimeToISO8601(Now));

      // Stack trace
      StackTrace := TStringList.Create;
      try
        GetStackTrace(StackTrace);
        Report.Add('stack_trace', StackTrace.Text);
      finally
        StackTrace.Free;
      end;

      // Informations système
      Report.Add('system_info', GatherSystemInfo);

      // État de l'application
      Report.Add('app_state', GatherApplicationState);

      // Envoyer le rapport
      HTTP := TFPHTTPClient.Create(nil);
      try
        PostData := TStringStream.Create(Report.AsJSON);
        try
          HTTP.RequestBody := PostData;
          HTTP.AddHeader('Content-Type', 'application/json');
          HTTP.Post('https://crash.officesuite.com/report');
        finally
          PostData.Free;
        end;
      finally
        HTTP.Free;
      end;
    finally
      Report.Free;
    end;
  except
    // En cas d'erreur lors du rapport, sauvegarder localement
    SaveCrashReportLocally(Report);
  end;
end;

procedure SaveCrashReportLocally(Report: TJSONObject);  
var
  CrashDir: string;
  FileName: string;
begin
  CrashDir := TPathManager.GetUserDataPath + 'crashes/';
  ForceDirectories(CrashDir);

  FileName := CrashDir + 'crash_' +
    FormatDateTime('yyyymmdd_hhnnss', Now) + '.json';

  with TStringList.Create do
  try
    Text := Report.FormatJSON;
    SaveToFile(FileName);
  finally
    Free;
  end;
end;
```

## Localisation et Internationalisation

### Gestionnaire de Langues

```pascal
type
  TLanguageManager = class
  private
    FCurrentLanguage: string;
    FTranslations: TDictionary<string, TDictionary<string, string>>;
    FAvailableLanguages: TStringList;

    procedure LoadLanguage(const LanguageCode: string);
    procedure LoadTranslationFile(const FileName: string);
  public
    constructor Create;
    destructor Destroy; override;

    function Translate(const Key: string): string;
    function TranslateF(const Key: string; const Args: array of const): string;
    procedure SetLanguage(const LanguageCode: string);
    function GetAvailableLanguages: TStringList;

    property CurrentLanguage: string read FCurrentLanguage;
  end;

constructor TLanguageManager.Create;  
var
  SearchRec: TSearchRec;
  LangDir: string;
  LangCode: string;
begin
  inherited Create;
  FTranslations := TDictionary<string, TDictionary<string, string>>.Create;
  FAvailableLanguages := TStringList.Create;

  LangDir := ExtractFilePath(ParamStr(0)) + 'languages/';

  // Charger les langues disponibles
  if FindFirst(LangDir + '*.json', faAnyFile, SearchRec) = 0 then
  begin
    repeat
      LangCode := ChangeFileExt(SearchRec.Name, '');
      FAvailableLanguages.Add(LangCode);
    until FindNext(SearchRec) <> 0;
    FindClose(SearchRec);
  end;

  // Charger la langue par défaut
  FCurrentLanguage := TOfficeSettings.Instance.GetString('language', 'fr-FR');
  LoadLanguage(FCurrentLanguage);
end;

destructor TLanguageManager.Destroy;  
var
  Dict: TDictionary<string, string>;
begin
  for Dict in FTranslations.Values do
    Dict.Free;
  FTranslations.Free;
  FAvailableLanguages.Free;
  inherited;
end;

procedure TLanguageManager.LoadLanguage(const LanguageCode: string);  
var
  FileName: string;
begin
  FileName := ExtractFilePath(ParamStr(0)) +
    'languages/' + LanguageCode + '.json';

  if FileExists(FileName) then
    LoadTranslationFile(FileName)
  else
    ShowMessage('Fichier de langue non trouvé : ' + LanguageCode);
end;

procedure TLanguageManager.LoadTranslationFile(const FileName: string);  
var
  JSONStr: string;
  JSONData: TJSONObject;
  Dict: TDictionary<string, string>;
  i: Integer;
  Key: string;
begin
  with TStringList.Create do
  try
    LoadFromFile(FileName);
    JSONStr := Text;
  finally
    Free;
  end;

  JSONData := GetJSON(JSONStr) as TJSONObject;
  try
    Dict := TDictionary<string, string>.Create;

    for i := 0 to JSONData.Count - 1 do
    begin
      Key := JSONData.Names[i];
      Dict.Add(Key, JSONData.Get(Key, ''));
    end;

    if FTranslations.ContainsKey(FCurrentLanguage) then
      FTranslations[FCurrentLanguage].Free;

    FTranslations.AddOrSetValue(FCurrentLanguage, Dict);
  finally
    JSONData.Free;
  end;
end;

function TLanguageManager.Translate(const Key: string): string;  
var
  Dict: TDictionary<string, string>;
begin
  if FTranslations.TryGetValue(FCurrentLanguage, Dict) then
  begin
    if not Dict.TryGetValue(Key, Result) then
      Result := Key; // Retourner la clé si traduction non trouvée
  end
  else
    Result := Key;
end;

function TLanguageManager.TranslateF(const Key: string;
  const Args: array of const): string;
begin
  Result := Format(Translate(Key), Args);
end;

procedure TLanguageManager.SetLanguage(const LanguageCode: string);  
begin
  if FAvailableLanguages.IndexOf(LanguageCode) >= 0 then
  begin
    FCurrentLanguage := LanguageCode;
    LoadLanguage(LanguageCode);
    TOfficeSettings.Instance.SetString('language', LanguageCode);
    TOfficeSettings.Instance.Save;

    // Actualiser toutes les fenêtres ouvertes
    RefreshAllForms;
  end;
end;
```

### Fichier de Traduction (JSON)

```json
// languages/fr-FR.json
{
  "app.title": "Suite Bureautique",
  "menu.file": "Fichier",
  "menu.file.new": "Nouveau",
  "menu.file.open": "Ouvrir",
  "menu.file.save": "Enregistrer",
  "menu.file.saveas": "Enregistrer sous...",
  "menu.file.print": "Imprimer",
  "menu.file.exit": "Quitter",

  "menu.edit": "Édition",
  "menu.edit.undo": "Annuler",
  "menu.edit.redo": "Rétablir",
  "menu.edit.cut": "Couper",
  "menu.edit.copy": "Copier",
  "menu.edit.paste": "Coller",

  "menu.format": "Format",
  "menu.format.bold": "Gras",
  "menu.format.italic": "Italique",
  "menu.format.underline": "Souligné",

  "dialog.save.title": "Enregistrer le document",
  "dialog.open.title": "Ouvrir un document",
  "dialog.unsaved.message": "Le document contient des modifications non enregistrées. Voulez-vous les enregistrer ?",

  "error.file.notfound": "Fichier introuvable : %s",
  "error.file.cannotopen": "Impossible d'ouvrir le fichier : %s",
  "error.file.cannotsave": "Impossible d'enregistrer le fichier : %s"
}
```

```json
// languages/en-US.json
{
  "app.title": "Office Suite",
  "menu.file": "File",
  "menu.file.new": "New",
  "menu.file.open": "Open",
  "menu.file.save": "Save",
  "menu.file.saveas": "Save As...",
  "menu.file.print": "Print",
  "menu.file.exit": "Exit",

  "menu.edit": "Edit",
  "menu.edit.undo": "Undo",
  "menu.edit.redo": "Redo",
  "menu.edit.cut": "Cut",
  "menu.edit.copy": "Copy",
  "menu.edit.paste": "Paste",

  "menu.format": "Format",
  "menu.format.bold": "Bold",
  "menu.format.italic": "Italic",
  "menu.format.underline": "Underline",

  "dialog.save.title": "Save Document",
  "dialog.open.title": "Open Document",
  "dialog.unsaved.message": "The document has unsaved changes. Do you want to save them?",

  "error.file.notfound": "File not found: %s",
  "error.file.cannotopen": "Cannot open file: %s",
  "error.file.cannotsave": "Cannot save file: %s"
}
```

### Utilisation dans l'Interface

```pascal
procedure TMainForm.CreateMenu;  
var
  Lang: TLanguageManager;
begin
  Lang := TLanguageManager.Instance;

  // Menu Fichier
  MenuFile.Caption := Lang.Translate('menu.file');
  MenuFileNew.Caption := Lang.Translate('menu.file.new');
  MenuFileOpen.Caption := Lang.Translate('menu.file.open');
  MenuFileSave.Caption := Lang.Translate('menu.file.save');

  // Menu Édition
  MenuEdit.Caption := Lang.Translate('menu.edit');
  MenuEditUndo.Caption := Lang.Translate('menu.edit.undo');
  MenuEditRedo.Caption := Lang.Translate('menu.edit.redo');
end;
```

## Aide Intégrée et Documentation

### Système d'Aide

```pascal
type
  THelpSystem = class
  private
    FHelpFile: string;
    FHelpBrowser: TForm;
    FContentTree: TTreeView;
    FHelpViewer: THTMLViewer;

    procedure LoadHelpIndex;
    procedure ShowHelpTopic(const TopicID: string);
  public
    constructor Create;
    destructor Destroy; override;

    procedure ShowHelp(const Context: string = '');
    procedure ShowContextHelp(Control: TControl);
    procedure SearchHelp(const Query: string);
  end;

constructor THelpSystem.Create;  
begin
  inherited Create;

  {$IFDEF WINDOWS}
  FHelpFile := ExtractFilePath(ParamStr(0)) + 'help\help.html';
  {$ENDIF}
  {$IFDEF UNIX}
  FHelpFile := ExtractFilePath(ParamStr(0)) + 'help/help.html';
  {$ENDIF}
end;

procedure THelpSystem.ShowHelp(const Context: string);  
begin
  if not Assigned(FHelpBrowser) then
  begin
    FHelpBrowser := TForm.Create(nil);
    FHelpBrowser.Caption := 'Aide';
    FHelpBrowser.Width := 800;
    FHelpBrowser.Height := 600;
    FHelpBrowser.Position := poScreenCenter;

    // Arbre de navigation
    FContentTree := TTreeView.Create(FHelpBrowser);
    FContentTree.Parent := FHelpBrowser;
    FContentTree.Align := alLeft;
    FContentTree.Width := 200;

    // Visionneuse HTML
    FHelpViewer := THTMLViewer.Create(FHelpBrowser);
    FHelpViewer.Parent := FHelpBrowser;
    FHelpViewer.Align := alClient;

    LoadHelpIndex;
  end;

  if Context <> '' then
    ShowHelpTopic(Context);

  FHelpBrowser.Show;
end;

procedure THelpSystem.ShowContextHelp(Control: TControl);  
var
  HelpContext: string;
begin
  // Obtenir le contexte d'aide du contrôle
  HelpContext := Control.HelpKeyword;

  if HelpContext = '' then
    HelpContext := Control.Name;

  ShowHelp(HelpContext);
end;
```

### Assistant de Démarrage

```pascal
type
  TWelcomeWizard = class(TForm)
  private
    FPageControl: TPageControl;
    FCurrentPage: Integer;
    FNextBtn: TButton;
    FBackBtn: TButton;
    FFinishBtn: TButton;

    procedure SetupPages;
    procedure OnNextClick(Sender: TObject);
    procedure OnBackClick(Sender: TObject);
    procedure OnFinishClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;

    class function ShowWizard: Boolean;
  end;

constructor TWelcomeWizard.Create(AOwner: TComponent);  
begin
  inherited Create(AOwner);

  Caption := 'Bienvenue dans la Suite Bureautique';
  Width := 600;
  Height := 450;
  Position := poScreenCenter;
  BorderStyle := bsDialog;

  FPageControl := TPageControl.Create(Self);
  FPageControl.Parent := Self;
  FPageControl.Align := alClient;
  FPageControl.TabPosition := tpBottom;

  SetupPages;

  // Boutons
  var ButtonPanel := TPanel.Create(Self);
  ButtonPanel.Parent := Self;
  ButtonPanel.Align := alBottom;
  ButtonPanel.Height := 50;

  FBackBtn := TButton.Create(Self);
  FBackBtn.Parent := ButtonPanel;
  FBackBtn.Caption := '< Précédent';
  FBackBtn.Left := 10;
  FBackBtn.Top := 10;
  FBackBtn.OnClick := OnBackClick;
  FBackBtn.Enabled := False;

  FNextBtn := TButton.Create(Self);
  FNextBtn.Parent := ButtonPanel;
  FNextBtn.Caption := 'Suivant >';
  FNextBtn.Left := 110;
  FNextBtn.Top := 10;
  FNextBtn.OnClick := OnNextClick;

  FFinishBtn := TButton.Create(Self);
  FFinishBtn.Parent := ButtonPanel;
  FFinishBtn.Caption := 'Terminer';
  FFinishBtn.Left := 210;
  FFinishBtn.Top := 10;
  FFinishBtn.OnClick := OnFinishClick;
  FFinishBtn.Visible := False;
end;

procedure TWelcomeWizard.SetupPages;  
var
  Page: TTabSheet;
  Panel: TPanel;
  Label1: TLabel;
begin
  // Page 1 : Bienvenue
  Page := TTabSheet.Create(FPageControl);
  Page.PageControl := FPageControl;
  Page.Caption := 'Bienvenue';

  Panel := TPanel.Create(Page);
  Panel.Parent := Page;
  Panel.Align := alClient;
  Panel.BevelOuter := bvNone;

  Label1 := TLabel.Create(Panel);
  Label1.Parent := Panel;
  Label1.Caption := 'Bienvenue dans la Suite Bureautique !';
  Label1.Font.Size := 16;
  Label1.Font.Style := [fsBold];
  Label1.Left := 20;
  Label1.Top := 20;

  var Label2 := TLabel.Create(Panel);
  Label2.Parent := Panel;
  Label2.Caption := 'Cet assistant va vous guider dans la configuration initiale.';
  Label2.Left := 20;
  Label2.Top := 60;
  Label2.WordWrap := True;
  Label2.Width := 540;

  // Page 2 : Paramètres
  Page := TTabSheet.Create(FPageControl);
  Page.PageControl := FPageControl;
  Page.Caption := 'Paramètres';

  Panel := TPanel.Create(Page);
  Panel.Parent := Page;
  Panel.Align := alClient;
  Panel.BevelOuter := bvNone;

  var LangLabel := TLabel.Create(Panel);
  LangLabel.Parent := Panel;
  LangLabel.Caption := 'Langue :';
  LangLabel.Left := 20;
  LangLabel.Top := 20;

  var LangCombo := TComboBox.Create(Panel);
  LangCombo.Parent := Panel;
  LangCombo.Left := 20;
  LangCombo.Top := 40;
  LangCombo.Items.AddStrings(['Français', 'English', 'Español', 'Deutsch']);
  LangCombo.ItemIndex := 0;

  // Page 3 : Templates
  Page := TTabSheet.Create(FPageControl);
  Page.PageControl := FPageControl;
  Page.Caption := 'Modèles';

  Panel := TPanel.Create(Page);
  Panel.Parent := Panel;
  Panel.Align := alClient;
  Panel.BevelOuter := bvNone;

  var TemplateLabel := TLabel.Create(Panel);
  TemplateLabel.Parent := Panel;
  TemplateLabel.Caption := 'Choisissez les modèles à installer :';
  TemplateLabel.Left := 20;
  TemplateLabel.Top := 20;

  var CheckListBox := TCheckListBox.Create(Panel);
  CheckListBox.Parent := Panel;
  CheckListBox.Left := 20;
  CheckListBox.Top := 50;
  CheckListBox.Width := 540;
  CheckListBox.Height := 250;
  CheckListBox.Items.AddStrings([
    'Lettre professionnelle',
    'CV moderne',
    'Rapport technique',
    'Facture',
    'Budget personnel',
    'Planning hebdomadaire'
  ]);

  // Tout cocher par défaut
  for var i := 0 to CheckListBox.Items.Count - 1 do
    CheckListBox.Checked[i] := True;
end;

procedure TWelcomeWizard.OnNextClick(Sender: TObject);  
begin
  if FCurrentPage < FPageControl.PageCount - 1 then
  begin
    Inc(FCurrentPage);
    FPageControl.ActivePageIndex := FCurrentPage;

    FBackBtn.Enabled := True;

    if FCurrentPage = FPageControl.PageCount - 1 then
    begin
      FNextBtn.Visible := False;
      FFinishBtn.Visible := True;
    end;
  end;
end;

procedure TWelcomeWizard.OnBackClick(Sender: TObject);  
begin
  if FCurrentPage > 0 then
  begin
    Dec(FCurrentPage);
    FPageControl.ActivePageIndex := FCurrentPage;

    FNextBtn.Visible := True;
    FFinishBtn.Visible := False;

    if FCurrentPage = 0 then
      FBackBtn.Enabled := False;
  end;
end;

procedure TWelcomeWizard.OnFinishClick(Sender: TObject);  
begin
  // Sauvegarder les préférences
  TOfficeSettings.Instance.SetBoolean('first_run', False);
  TOfficeSettings.Instance.Save;

  ModalResult := mrOK;
end;

class function TWelcomeWizard.ShowWizard: Boolean;  
var
  Wizard: TWelcomeWizard;
begin
  Wizard := TWelcomeWizard.Create(nil);
  try
    Result := Wizard.ShowModal = mrOK;
  finally
    Wizard.Free;
  end;
end;
```

## Programme Principal de la Suite

### Lanceur Principal

```pascal
program OfficeSuite;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, Forms, SysUtils,
  MainLauncher, TextEditor, Spreadsheet, Presentation, Database,
  OfficeSettings, LicenseManager, UpdateChecker, LanguageManager;

var
  LicMgr: TLicenseManager;
  UpdateChecker: TUpdateChecker;
  Settings: TOfficeSettings;

begin
  Application.Title := 'Suite Bureautique';
  Application.Initialize;

  // Initialiser les paramètres
  Settings := TOfficeSettings.Instance;

  // Vérifier si c'est la première exécution
  if Settings.GetBoolean('first_run', True) then
  begin
    if TWelcomeWizard.ShowWizard then
      Settings.SetBoolean('first_run', False);
  end;

  // Vérifier la licence
  LicMgr := TLicenseManager.Create;
  try
    if not LicMgr.IsLicenseValid then
    begin
      if not TActivationDialog.ShowDialog(LicMgr) then
      begin
        MessageDlg('Licence requise',
          'Une licence valide est nécessaire pour utiliser cette application.',
          mtError, [mbOK], 0);
        Exit;
      end;
    end;

    // Afficher les jours restants pour les versions d'essai
    if LicMgr.License.LicenseType = ltTrial then
    begin
      var DaysLeft := LicMgr.GetDaysRemaining;
      if DaysLeft <= 7 then
        MessageDlg('Version d''essai',
          Format('Il vous reste %d jours d''essai', [DaysLeft]),
          mtInformation, [mbOK], 0);
    end;
  finally
    LicMgr.Free;
  end;

  // Vérifier les mises à jour (en arrière-plan)
  if Settings.GetBoolean('check_updates', True) then
  begin
    TThread.CreateAnonymousThread(procedure
    var
      Checker: TUpdateChecker;
      UpdateInfo: TUpdateInfo;
    begin
      Checker := TUpdateChecker.Create('https://update.officesuite.com');
      try
        if Checker.CheckForUpdates then
        begin
          UpdateInfo := Checker.GetUpdateInfo;
          TThread.Synchronize(nil, procedure
          begin
            if TUpdateDialog.ShowDialog(UpdateInfo) = mrYes then
              Checker.DownloadUpdate(UpdateInfo);
          end);
        end;
      finally
        Checker.Free;
      end;
    end).Start;
  end;

  // Créer et afficher le lanceur principal
  Application.CreateForm(TMainLauncherForm, MainLauncherForm);
  Application.Run;
end.
```

### Formulaire Lanceur

```pascal
type
  TMainLauncherForm = class(TForm)
  private
    procedure SetupUI;
    procedure OnTextEditorClick(Sender: TObject);
    procedure OnSpreadsheetClick(Sender: TObject);
    procedure OnPresentationClick(Sender: TObject);
    procedure OnDatabaseClick(Sender: TObject);
    procedure OnRecentFileClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  end;

constructor TMainLauncherForm.Create(AOwner: TComponent);  
begin
  inherited Create(AOwner);
  SetupUI;
end;

procedure TMainLauncherForm.SetupUI;  
var
  Panel: TPanel;
  Btn: TButton;
  RecentList: TListBox;
begin
  Caption := 'Suite Bureautique - Accueil';
  Width := 800;
  Height := 600;
  Position := poScreenCenter;

  // Panneau de gauche : Applications
  Panel := TPanel.Create(Self);
  Panel.Parent := Self;
  Panel.Align := alLeft;
  Panel.Width := 300;
  Panel.Caption := '';

  var TitleLabel := TLabel.Create(Panel);
  TitleLabel.Parent := Panel;
  TitleLabel.Caption := 'Nouvelle Document';
  TitleLabel.Font.Size := 12;
  TitleLabel.Font.Style := [fsBold];
  TitleLabel.Left := 20;
  TitleLabel.Top := 20;

  // Bouton Traitement de texte
  Btn := TButton.Create(Panel);
  Btn.Parent := Panel;
  Btn.Caption := 'Traitement de texte';
  Btn.Left := 20;
  Btn.Top := 60;
  Btn.Width := 260;
  Btn.Height := 40;
  Btn.OnClick := OnTextEditorClick;

  // Bouton Tableur
  Btn := TButton.Create(Panel);
  Btn.Parent := Panel;
  Btn.Caption := 'Tableur';
  Btn.Left := 20;
  Btn.Top := 110;
  Btn.Width := 260;
  Btn.Height := 40;
  Btn.OnClick := OnSpreadsheetClick;

  // Bouton Présentation
  Btn := TButton.Create(Panel);
  Btn.Parent := Panel;
  Btn.Caption := 'Présentation';
  Btn.Left := 20;
  Btn.Top := 160;
  Btn.Width := 260;
  Btn.Height := 40;
  Btn.OnClick := OnPresentationClick;

  // Bouton Base de données
  Btn := TButton.Create(Panel);
  Btn.Parent := Panel;
  Btn.Caption := 'Base de données';
  Btn.Left := 20;
  Btn.Top := 210;
  Btn.Width := 260;
  Btn.Height := 40;
  Btn.OnClick := OnDatabaseClick;

  // Panneau de droite : Fichiers récents
  Panel := TPanel.Create(Self);
  Panel.Parent := Self;
  Panel.Align := alClient;
  Panel.Caption := '';

  TitleLabel := TLabel.Create(Panel);
  TitleLabel.Parent := Panel;
  TitleLabel.Caption := 'Fichiers récents';
  TitleLabel.Font.Size := 12;
  TitleLabel.Font.Style := [fsBold];
  TitleLabel.Left := 20;
  TitleLabel.Top := 20;

  RecentList := TListBox.Create(Panel);
  RecentList.Parent := Panel;
  RecentList.Left := 20;
  RecentList.Top := 60;
  RecentList.Width := Panel.Width - 40;
  RecentList.Height := Panel.Height - 80;
  RecentList.Anchors := [akLeft, akTop, akRight, akBottom];
  RecentList.OnDblClick := OnRecentFileClick;

  // Charger les fichiers récents
  LoadRecentFiles(RecentList);
end;

procedure TMainLauncherForm.OnTextEditorClick(Sender: TObject);  
var
  Editor: TTextEditor;
begin
  Editor := TTextEditor.Create(nil);
  Editor.Show;
end;

procedure TMainLauncherForm.OnSpreadsheetClick(Sender: TObject);  
var
  Spreadsheet: TSpreadsheetForm;
begin
  Spreadsheet := TSpreadsheetForm.Create(nil);
  Spreadsheet.Show;
end;

procedure TMainLauncherForm.OnPresentationClick(Sender: TObject);  
var
  Presentation: TPresentationEditor;
begin
  Presentation := TPresentationEditor.Create(nil);
  Presentation.Show;
end;

procedure TMainLauncherForm.OnDatabaseClick(Sender: TObject);  
var
  Database: TDatabaseManager;
begin
  Database := TDatabaseManager.Create(nil);
  Database.Show;
end;

procedure TMainLauncherForm.OnRecentFileClick(Sender: TObject);  
var
  RecentList: TListBox;
  FileName: string;
  Ext: string;
begin
  RecentList := Sender as TListBox;
  if RecentList.ItemIndex >= 0 then
  begin
    FileName := RecentList.Items[RecentList.ItemIndex];
    Ext := LowerCase(ExtractFileExt(FileName));

    case Ext of
      '.odt', '.doc', '.docx', '.txt':
      begin
        var Editor := TTextEditor.Create(nil);
        Editor.Show;
        Editor.OpenFile(FileName);
      end;

      '.ods', '.xls', '.xlsx', '.csv':
      begin
        var Spreadsheet := TSpreadsheetForm.Create(nil);
        Spreadsheet.Show;
        Spreadsheet.OpenFile(FileName);
      end;

      '.odp', '.ppt', '.pptx':
      begin
        var Presentation := TPresentationEditor.Create(nil);
        Presentation.Show;
        Presentation.OpenFile(FileName);
      end;

      '.odb', '.db', '.sqlite':
      begin
        var Database := TDatabaseManager.Create(nil);
        Database.Show;
        Database.OpenFile(FileName);
      end;
    end;
  end;
end;

procedure LoadRecentFiles(ListBox: TListBox);  
var
  Settings: TOfficeSettings;
  RecentFiles: TJSONArray;
  i: Integer;
begin
  Settings := TOfficeSettings.Instance;

  // Charger la liste des fichiers récents depuis les paramètres
  var JSONStr := Settings.GetString('recent_files', '[]');
  RecentFiles := GetJSON(JSONStr) as TJSONArray;
  try
    ListBox.Items.Clear;
    for i := 0 to Min(RecentFiles.Count - 1, 9) do // Maximum 10 fichiers
    begin
      var FileName := RecentFiles.Strings[i];
      if FileExists(FileName) then
        ListBox.Items.Add(FileName);
    end;
  finally
    RecentFiles.Free;
  end;
end;

procedure AddToRecentFiles(const FileName: string);  
var
  Settings: TOfficeSettings;
  RecentFiles: TJSONArray;
  i: Integer;
  Found: Boolean;
begin
  Settings := TOfficeSettings.Instance;

  var JSONStr := Settings.GetString('recent_files', '[]');
  RecentFiles := GetJSON(JSONStr) as TJSONArray;
  try
    // Vérifier si le fichier existe déjà
    Found := False;
    for i := 0 to RecentFiles.Count - 1 do
    begin
      if RecentFiles.Strings[i] = FileName then
      begin
        Found := True;
        RecentFiles.Delete(i);
        Break;
      end;
    end;

    // Ajouter en première position
    RecentFiles.Insert(0, FileName);

    // Limiter à 10 fichiers
    while RecentFiles.Count > 10 do
      RecentFiles.Delete(RecentFiles.Count - 1);

    Settings.SetString('recent_files', RecentFiles.AsJSON);
    Settings.Save;
  finally
    RecentFiles.Free;
  end;
end;
```

## Tests et Validation

### Tests Unitaires

```pascal
unit TestDocumentCore;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  DocumentCore, TextDocument, SpreadsheetDocument;

type
  TTestTextDocument = class(TTestCase)
  published
    procedure TestCreateDocument;
    procedure TestSaveAndLoad;
    procedure TestUndoRedo;
    procedure TestAddParagraph;
    procedure TestModifiedFlag;
  end;

  TTestSpreadsheetDocument = class(TTestCase)
  published
    procedure TestCreateWorksheet;
    procedure TestCellOperations;
    procedure TestFormulas;
  end;

implementation

{ TTestTextDocument }

procedure TTestTextDocument.TestCreateDocument;  
var
  Doc: TTextDocument;
begin
  Doc := TTextDocument.Create;
  try
    AssertNotNull('Document créé', Doc);
    AssertEquals('État initial', dsNew, Doc.State);
    AssertFalse('Pas modifié', Doc.Modified);
  finally
    Doc.Free;
  end;
end;

procedure TTestTextDocument.TestSaveAndLoad;  
var
  Doc1, Doc2: TTextDocument;
  TempFile: string;
begin
  TempFile := GetTempDir + 'test_doc.odt';

  Doc1 := TTextDocument.Create;
  try
    Doc1.AddParagraph('Test paragraph 1');
    Doc1.AddParagraph('Test paragraph 2');
    Doc1.Title := 'Test Document';

    AssertTrue('Sauvegarde réussie', Doc1.SaveAs(TempFile));

    Doc2 := TTextDocument.Create;
    try
      AssertTrue('Chargement réussi', Doc2.Open(TempFile));
      AssertEquals('Nombre de paragraphes', 2, Doc2.Paragraphs.Count);
      AssertEquals('Titre', 'Test Document', Doc2.Title);
      AssertEquals('Premier paragraphe', 'Test paragraph 1',
        Doc2.Paragraphs[0].Text);
    finally
      Doc2.Free;
    end;
  finally
    Doc1.Free;
    DeleteFile(TempFile);
  end;
end;

procedure TTestTextDocument.TestUndoRedo;  
var
  Doc: TTextDocument;
begin
  Doc := TTextDocument.Create;
  try
    AssertFalse('Pas de undo initial', Doc.CanUndo);

    Doc.AddParagraph('Paragraph 1');
    AssertTrue('Peut annuler', Doc.CanUndo);
    AssertFalse('Pas de redo', Doc.CanRedo);

    Doc.Undo;
    AssertEquals('Paragraphes supprimés', 0, Doc.Paragraphs.Count);
    AssertTrue('Peut rétablir', Doc.CanRedo);

    Doc.Redo;
    AssertEquals('Paragraphe rétabli', 1, Doc.Paragraphs.Count);
  finally
    Doc.Free;
  end;
end;

procedure TTestTextDocument.TestModifiedFlag;  
var
  Doc: TTextDocument;
begin
  Doc := TTextDocument.Create;
  try
    AssertFalse('Non modifié initial', Doc.Modified);

    Doc.AddParagraph('Test');
    AssertTrue('Modifié après ajout', Doc.Modified);

    Doc.Modified := False;
    AssertFalse('Flag réinitialisé', Doc.Modified);
  finally
    Doc.Free;
  end;
end;

{ TTestSpreadsheetDocument }

procedure TTestSpreadsheetDocument.TestCreateWorksheet;  
var
  Doc: TSpreadsheetDocument;
  Sheet: TWorksheet;
begin
  Doc := TSpreadsheetDocument.Create;
  try
    Sheet := Doc.AddWorksheet('Feuille1');
    AssertNotNull('Feuille créée', Sheet);
    AssertEquals('Nom de feuille', 'Feuille1', Sheet.Name);
    AssertEquals('Nombre de feuilles', 1, Doc.Worksheets.Count);
  finally
    Doc.Free;
  end;
end;

procedure TTestSpreadsheetDocument.TestCellOperations;  
var
  Doc: TSpreadsheetDocument;
  Sheet: TWorksheet;
begin
  Doc := TSpreadsheetDocument.Create;
  try
    Sheet := Doc.AddWorksheet('Test');

    Sheet.SetCellValue(1, 1, 42);
    AssertEquals('Valeur numérique', 42, Integer(Sheet.GetCellValue(1, 1)));

    Sheet.SetCellValue(2, 1, 'Hello');
    AssertEquals('Valeur texte', 'Hello', string(Sheet.GetCellValue(2, 1)));
  finally
    Doc.Free;
  end;
end;

procedure TTestSpreadsheetDocument.TestFormulas;  
var
  Doc: TSpreadsheetDocument;
  Sheet: TWorksheet;
  Engine: TFormulaEngine;
begin
  Doc := TSpreadsheetDocument.Create;
  try
    Sheet := Doc.AddWorksheet('Test');
    Sheet.SetCellValue(1, 1, 10);
    Sheet.SetCellValue(1, 2, 20);

    Engine := TFormulaEngine.Create(Sheet);
    try
      var Result := Engine.Evaluate('=A1+B1');
      AssertEquals('Formule addition', 30, Integer(Result));
    finally
      Engine.Free;
    end;
  finally
    Doc.Free;
  end;
end;

initialization
  RegisterTest(TTestTextDocument);
  RegisterTest(TTestSpreadsheetDocument);

end.
```

### Tests d'Intégration

```pascal
unit TestIntegration;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  DocumentCore, FormatHandlers, ClipboardManager;

type
  TTestImportExport = class(TTestCase)
  published
    procedure TestODTImportExport;
    procedure TestDOCXImport;
    procedure TestPDFExport;
  end;

  TTestClipboard = class(TTestCase)
  published
    procedure TestCopyPasteText;
    procedure TestCopyPasteTable;
  end;

implementation

{ TTestImportExport }

procedure TTestImportExport.TestODTImportExport;  
var
  Doc1, Doc2: TTextDocument;
  Handler: TODTHandler;
  TempFile: string;
begin
  TempFile := GetTempDir + 'test.odt';

  Doc1 := TTextDocument.Create;
  Handler := TODTHandler.Create;
  try
    Doc1.AddParagraph('Test ODT');
    Doc1.Title := 'Document Test';

    AssertTrue('Export ODT', Handler.Export(Doc1, TempFile));
    AssertTrue('Fichier créé', FileExists(TempFile));

    Doc2 := Handler.Import(TempFile) as TTextDocument;
    try
      AssertNotNull('Import réussi', Doc2);
      AssertEquals('Titre préservé', 'Document Test', Doc2.Title);
    finally
      Doc2.Free;
    end;
  finally
    Handler.Free;
    Doc1.Free;
    DeleteFile(TempFile);
  end;
end;

initialization
  RegisterTest(TTestImportExport);
  RegisterTest(TTestClipboard);

end.
```

## Documentation Utilisateur

### Manuel Utilisateur (Markdown)

````markdown
# Manuel Utilisateur - Suite Bureautique

## Table des Matières

1. [Introduction](#introduction)
2. [Installation](#installation)
3. [Premiers Pas](#premiers-pas)
4. [Traitement de Texte](#traitement-de-texte)
5. [Tableur](#tableur)
6. [Présentation](#presentation)
7. [Base de Données](#base-de-donnees)
8. [Fonctionnalités Avancées](#fonctionnalites-avancees)

## Introduction

La Suite Bureautique est une solution complète et multi-plateforme pour tous vos besoins en productivité. Elle fonctionne de manière identique sous Windows et Ubuntu.

### Caractéristiques Principales

- **Traitement de texte** : Créez des documents professionnels
- **Tableur** : Effectuez des calculs et analyses
- **Présentation** : Réalisez des diaporamas percutants
- **Base de données** : Gérez vos données efficacement
- **Compatible** : Supporte les formats Microsoft Office et OpenDocument
- **Multi-plateforme** : Windows et Ubuntu

## Installation

### Windows

1. Téléchargez `OfficeSuiteSetup.exe`
2. Double-cliquez sur le fichier
3. Suivez les instructions de l'assistant
4. Lancez l'application depuis le menu Démarrer

### Ubuntu

#### Méthode 1 : Package DEB

```bash
sudo dpkg -i officesuite_1.0-1_amd64.deb  
sudo apt-get install -f
```

#### Méthode 2 : Script d'installation

```bash
chmod +x install.sh  
sudo ./install.sh
```

## Premiers Pas

### Créer un Nouveau Document

1. Lancez la Suite Bureautique
2. Cliquez sur le type de document souhaité :
   - **Traitement de texte** pour une lettre ou un rapport
   - **Tableur** pour des calculs
   - **Présentation** pour un diaporama
   - **Base de données** pour gérer des données

### Ouvrir un Document Existant

**Méthode 1** : Depuis le lanceur
- Cliquez sur un fichier dans la liste "Fichiers récents"

**Méthode 2** : Depuis l'application
- Menu **Fichier** → **Ouvrir**
- Raccourci : `Ctrl+O` (Windows) ou `Cmd+O` (Ubuntu)

### Enregistrer un Document

- **Première sauvegarde** : Menu **Fichier** → **Enregistrer sous**
- **Sauvegardes suivantes** : Menu **Fichier** → **Enregistrer**
- Raccourci : `Ctrl+S` (Windows) ou `Cmd+S` (Ubuntu)

## Traitement de Texte

### Formatage de Base

#### Gras, Italique, Souligné

- **Gras** : `Ctrl+B` ou bouton **G**
- **Italique** : `Ctrl+I` ou bouton **I**
- **Souligné** : `Ctrl+U` ou bouton **S**

#### Changer la Police

1. Sélectionnez le texte
2. Utilisez le menu déroulant de police
3. Choisissez la taille dans le menu adjacent

#### Alignement

- **Gauche** : `Ctrl+L`
- **Centre** : `Ctrl+E`
- **Droite** : `Ctrl+R`
- **Justifié** : `Ctrl+J`

### Insertion d'Éléments

#### Images

1. Menu **Insertion** → **Image**
2. Sélectionnez votre fichier
3. Redimensionnez en tirant les poignées

#### Tableaux

1. Menu **Insertion** → **Tableau**
2. Choisissez le nombre de lignes et colonnes
3. Cliquez sur **OK**

#### En-têtes et Pieds de Page

1. Menu **Insertion** → **En-tête et pied de page**
2. Tapez votre contenu
3. Utilisez les variables pour numéros de page, date, etc.

## Tableur

### Saisie de Données

- Cliquez sur une cellule
- Tapez votre valeur
- Appuyez sur `Entrée` pour valider

### Formules

#### Formules de Base

````
=A1+B1          Addition
=A1-B1          Soustraction
=A1*B1          Multiplication
=A1/B1          Division
=SOMME(A1:A10)  Somme d'une plage
=MOYENNE(A1:A10) Moyenne
=MAX(A1:A10)    Maximum
=MIN(A1:A10)    Minimum
```

#### Références

- **Référence relative** : `A1` (change lors de la copie)
- **Référence absolue** : `$A$1` (ne change pas)
- **Référence mixte** : `$A1` ou `A$1`

### Graphiques

1. Sélectionnez vos données
2. Menu **Insertion** → **Graphique**
3. Choisissez le type de graphique
4. Personnalisez les options
5. Cliquez sur **Créer**

## Présentation

### Créer une Diapositive

1. Menu **Diapositive** → **Nouvelle diapositive**
2. Choisissez une disposition
3. Ajoutez votre contenu

### Ajouter du Contenu

#### Zone de Texte

1. Bouton **Zone de texte**
2. Tracez la zone sur la diapositive
3. Tapez votre texte

#### Image

1. Bouton **Image**
2. Sélectionnez le fichier
3. Positionnez l'image

### Transitions

1. Sélectionnez une diapositive
2. Menu **Transition**
3. Choisissez l'effet
4. Réglez la durée

### Mode Présentation

- **Démarrer** : `F5`
- **Diapositive suivante** : `Flèche droite` ou `Espace`
- **Diapositive précédente** : `Flèche gauche`
- **Quitter** : `Échap`

## Fonctionnalités Avancées

### Macros

#### Enregistrer une Macro

1. Menu **Outils** → **Macros** → **Enregistrer une macro**
2. Effectuez vos actions
3. Menu **Outils** → **Macros** → **Arrêter l'enregistrement**
4. Donnez un nom à la macro

#### Exécuter une Macro

1. Menu **Outils** → **Macros** → **Exécuter**
2. Sélectionnez la macro
3. Cliquez sur **Exécuter**

### Modèles

#### Utiliser un Modèle

1. Au démarrage, cliquez sur **Nouveau à partir d'un modèle**
2. Parcourez les modèles
3. Sélectionnez celui qui vous convient

#### Créer un Modèle

1. Créez votre document
2. Menu **Fichier** → **Enregistrer comme modèle**
3. Donnez un nom
4. Le modèle sera disponible pour de futurs documents

### Collaboration

#### Activer le Partage

1. Menu **Fichier** → **Partager**
2. Entrez les adresses email des collaborateurs
3. Définissez les permissions
4. Cliquez sur **Partager**

#### Suivi des Modifications

1. Menu **Édition** → **Suivi des modifications** → **Activer**
2. Les modifications sont surlignées
3. Menu **Édition** → **Accepter/Refuser les modifications**

## Raccourcis Clavier

### Communs à Toutes les Applications

| Action | Windows/Linux |
|--------|---------------|
| Nouveau | `Ctrl+N` |
| Ouvrir | `Ctrl+O` |
| Enregistrer | `Ctrl+S` |
| Imprimer | `Ctrl+P` |
| Annuler | `Ctrl+Z` |
| Rétablir | `Ctrl+Y` |
| Couper | `Ctrl+X` |
| Copier | `Ctrl+C` |
| Coller | `Ctrl+V` |
| Rechercher | `Ctrl+F` |

## Dépannage

### Le document ne s'ouvre pas

1. Vérifiez que le format est supporté
2. Essayez avec **Ouvrir et réparer**
3. Vérifiez les permissions du fichier

### L'application plante

1. Vérifiez les mises à jour
2. Désactivez les plugins récents
3. Réinitialisez les préférences

### Problèmes d'impression

1. Vérifiez que l'imprimante est installée
2. Testez avec **Aperçu avant impression**
3. Mettez à jour les pilotes d'imprimante

## Support

### Obtenir de l'Aide

- **Documentation en ligne** : https://docs.officesuite.com
- **Forum communautaire** : https://forum.officesuite.com
- **Support email** : support@officesuite.com
- **Aide intégrée** : Appuyez sur `F1` dans l'application

### Signaler un Bug

1. Menu **Aide** → **Signaler un problème**
2. Décrivez le problème
3. Incluez les étapes pour le reproduire
4. Cliquez sur **Envoyer**
```

## Conclusion

Vous avez maintenant une suite bureautique complète et portable ! Voici un résumé des points clés :

### Ce que nous avons construit

1. **Architecture modulaire** avec un noyau commun partagé
2. **Quatre applications principales** : Traitement de texte, Tableur, Présentation, Base de données
3. **Système de plugins** extensible
4. **Gestion des formats** : natifs, ODF, Microsoft Office, PDF
5. **Fonctionnalités avancées** : macros, templates, collaboration, versionning
6. **Multi-plateforme** : Code unique pour Windows et Ubuntu
7. **Système complet** : licence, mises à jour, télémétrie, localisation

### Aspects Multi-plateformes Maîtrisés

- **Chemins de fichiers** adaptés à chaque OS
- **Installation** : Inno Setup (Windows) et DEB (Ubuntu)
- **Intégration système** : Registre Windows vs fichiers de configuration Linux
- **Thèmes** adaptés à chaque environnement

### Évolutions Possibles

- **Synchronisation cloud** (Dropbox, Google Drive, OneDrive)
- **Application mobile** (Android/iOS)
- **Mode sombre** automatique
- **IA intégrée** pour suggestions et corrections
- **Collaboration temps réel** améliorée
- **Support de plus de formats**

### Ressources Complémentaires

- **Documentation FreePascal** : https://www.freepascal.org/docs.html
- **Wiki Lazarus** : https://wiki.lazarus.freepascal.org/
- **Forum FreePascal** : https://forum.lazarus.freepascal.org/
- **Exemples de code** : Répertoire `examples` de Lazarus

Ce projet démontre la puissance de FreePascal et Lazarus pour créer des applications professionnelles multi-plateformes complexes. Avec une bonne architecture et une planification rigoureuse, il est possible de rivaliser avec des suites bureautiques commerciales !

Bon développement ! 🚀

⏭️ [Système de gestion hospitalière](/25-projets-complexes-etudes-cas/10-systeme-gestion-hospitaliere.md)
