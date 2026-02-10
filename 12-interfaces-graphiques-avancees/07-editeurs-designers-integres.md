🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 12.7 Éditeurs et designers intégrés

## Introduction

Les **éditeurs et designers intégrés** sont des composants qui permettent à vos utilisateurs de créer, modifier et manipuler du contenu directement dans votre application. Pensez à des outils comme :

- L'éditeur de texte riche dans Microsoft Word
- Le designer de formulaires dans Visual Studio
- L'éditeur de formules dans Excel
- Le designer de rapports dans Crystal Reports

Dans ce chapitre, nous allons apprendre à créer ces types d'éditeurs dans vos applications FreePascal/Lazarus.

## Pourquoi créer des éditeurs intégrés ?

### Avantages pour l'utilisateur
- **Productivité** : Pas besoin de changer d'application
- **Cohérence** : Interface familière et intégrée
- **Efficacité** : Modifications en temps réel
- **Simplicité** : Tout est au même endroit

### Avantages pour le développeur
- **Valeur ajoutée** : Fonctionnalité différenciante
- **Contrôle** : Personnalisation complète
- **Intégration** : Connexion directe avec votre logique métier
- **Flexibilité** : Adaptation aux besoins spécifiques

## Types d'éditeurs courants

### 1. Éditeurs de texte
- Éditeur de texte simple (TMemo amélioré)
- Éditeur de code avec coloration syntaxique
- Éditeur de texte riche (RTF, HTML)
- Éditeur Markdown

### 2. Éditeurs visuels
- Designer de formulaires/interfaces
- Éditeur de rapports
- Designer de workflows/diagrammes
- Éditeur de graphiques

### 3. Éditeurs spécialisés
- Éditeur de formules mathématiques
- Éditeur de requêtes SQL
- Éditeur d'expressions régulières
- Éditeur de couleurs/thèmes

## Éditeur de texte avec coloration syntaxique

### Utilisation de SynEdit

**SynEdit** est le composant principal pour créer des éditeurs de code dans Lazarus. Il offre :
- Coloration syntaxique pour de nombreux langages
- Numérotation des lignes
- Pliage de code
- Auto-complétion
- Marque-pages et points d'arrêt

#### Installation et configuration

SynEdit est inclus dans Lazarus par défaut. Voici comment l'utiliser :

```pascal
unit CodeEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  SynEdit, SynHighlighterPas, SynHighlighterSQL, SynHighlighterHTML,
  SynEditMarkupHighAll, SynCompletion, ExtCtrls, StdCtrls, Menus;

type
  TfrmCodeEditor = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    FSynEdit: TSynEdit;
    FHighlighterPascal: TSynPasSyn;
    FHighlighterSQL: TSynSQLSyn;
    FHighlighterHTML: TSynHTMLSyn;
    FSynCompletion: TSynCompletion;

    procedure SetupEditor;
    procedure SetupHighlighters;
    procedure SetupCompletion;
    procedure ConfigureEditorOptions;

    procedure OnChangeLanguage(Sender: TObject);
  public
    procedure LoadFile(const AFileName: string);
    procedure SaveFile(const AFileName: string);
    procedure SetLanguage(const ALanguage: string);
  end;

var
  frmCodeEditor: TfrmCodeEditor;

implementation

{$R *.lfm}

procedure TfrmCodeEditor.FormCreate(Sender: TObject);  
begin
  Caption := 'Éditeur de Code';
  Width := 1000;
  Height := 700;
  Position := poScreenCenter;

  SetupEditor;
  SetupHighlighters;
  SetupCompletion;
  ConfigureEditorOptions;
end;

procedure TfrmCodeEditor.SetupEditor;  
begin
  // Création de l'éditeur principal
  FSynEdit := TSynEdit.Create(Self);
  FSynEdit.Parent := Self;
  FSynEdit.Align := alClient;

  // Configuration de base
  FSynEdit.Font.Name := 'Courier New';
  FSynEdit.Font.Size := 10;
  FSynEdit.TabWidth := 2;
  FSynEdit.WantTabs := True;

  // Affichage
  FSynEdit.Gutter.Visible := True;
  FSynEdit.Gutter.ShowLineNumbers := True;
  FSynEdit.Gutter.AutoSize := True;
  FSynEdit.RightEdge := 80;
  FSynEdit.RightEdgeColor := clSilver;

  // Comportement
  FSynEdit.Options := FSynEdit.Options + [eoAutoIndent, eoTabsToSpaces];
  FSynEdit.Options := FSynEdit.Options + [eoTrimTrailingSpaces];
end;

procedure TfrmCodeEditor.SetupHighlighters;  
begin
  // Highlighter Pascal
  FHighlighterPascal := TSynPasSyn.Create(Self);

  // Highlighter SQL
  FHighlighterSQL := TSynSQLSyn.Create(Self);

  // Highlighter HTML
  FHighlighterHTML := TSynHTMLSyn.Create(Self);

  // Par défaut : Pascal
  FSynEdit.Highlighter := FHighlighterPascal;
end;

procedure TfrmCodeEditor.SetupCompletion;  
begin
  // Auto-complétion
  FSynCompletion := TSynCompletion.Create(Self);
  FSynCompletion.Editor := FSynEdit;
  FSynCompletion.ShortCut := 16416; // Ctrl+Space

  // Ajouter des mots-clés Pascal
  FSynCompletion.ItemList.Add('begin');
  FSynCompletion.ItemList.Add('end');
  FSynCompletion.ItemList.Add('procedure');
  FSynCompletion.ItemList.Add('function');
  FSynCompletion.ItemList.Add('if');
  FSynCompletion.ItemList.Add('then');
  FSynCompletion.ItemList.Add('else');
  FSynCompletion.ItemList.Add('while');
  FSynCompletion.ItemList.Add('for');
  FSynCompletion.ItemList.Add('repeat');
  FSynCompletion.ItemList.Add('until');
  FSynCompletion.ItemList.Add('case');
  FSynCompletion.ItemList.Add('try');
  FSynCompletion.ItemList.Add('except');
  FSynCompletion.ItemList.Add('finally');
end;

procedure TfrmCodeEditor.ConfigureEditorOptions;  
begin
  // Couleurs personnalisées
  FSynEdit.Color := clWhite;
  FSynEdit.Font.Color := clBlack;

  // Ligne active
  FSynEdit.ActiveLineColor := RGB(255, 255, 200);

  // Sélection
  FSynEdit.SelectedColor.Background := RGB(200, 220, 255);
  FSynEdit.SelectedColor.Foreground := clBlack;

  // Gutter (marge)
  FSynEdit.Gutter.Color := RGB(240, 240, 240);
end;

procedure TfrmCodeEditor.LoadFile(const AFileName: string);  
begin
  if FileExists(AFileName) then
  begin
    FSynEdit.Lines.LoadFromFile(AFileName);

    // Détection automatique du langage
    if LowerCase(ExtractFileExt(AFileName)) = '.pas' then
      SetLanguage('Pascal')
    else if LowerCase(ExtractFileExt(AFileName)) = '.sql' then
      SetLanguage('SQL')
    else if (LowerCase(ExtractFileExt(AFileName)) = '.html') or
            (LowerCase(ExtractFileExt(AFileName)) = '.htm') then
      SetLanguage('HTML');
  end;
end;

procedure TfrmCodeEditor.SaveFile(const AFileName: string);  
begin
  FSynEdit.Lines.SaveToFile(AFileName);
end;

procedure TfrmCodeEditor.SetLanguage(const ALanguage: string);  
begin
  if ALanguage = 'Pascal' then
    FSynEdit.Highlighter := FHighlighterPascal
  else if ALanguage = 'SQL' then
    FSynEdit.Highlighter := FHighlighterSQL
  else if ALanguage = 'HTML' then
    FSynEdit.Highlighter := FHighlighterHTML
  else
    FSynEdit.Highlighter := nil; // Pas de coloration
end;

end.
```

### Fonctionnalités avancées de l'éditeur

#### Pliage de code (Code Folding)

```pascal
procedure TfrmCodeEditor.EnableCodeFolding;  
begin
  FSynEdit.Options := FSynEdit.Options + [eoFoldedCopyPaste];

  // Configuration du pliage
  with FSynEdit.Gutter.CodeFoldPart do
  begin
    Visible := True;
    Width := 14;
    MarkupInfo.Background := clBtnFace;
  end;
end;
```

#### Recherche et remplacement

```pascal
uses
  SynEditSearch;

type
  TCodeEditorSearch = class
  private
    FEditor: TSynEdit;
    FSearchEngine: TSynEditSearch;
  public
    constructor Create(AEditor: TSynEdit);
    destructor Destroy; override;

    function Find(const ASearchText: string; ACaseSensitive,
                  AWholeWord: Boolean): Boolean;
    function Replace(const ASearchText, AReplaceText: string;
                    APrompt: Boolean): Integer;
    procedure FindNext;
  end;

constructor TCodeEditorSearch.Create(AEditor: TSynEdit);  
begin
  inherited Create;
  FEditor := AEditor;
  FSearchEngine := TSynEditSearch.Create;
end;

destructor TCodeEditorSearch.Destroy;  
begin
  FSearchEngine.Free;
  inherited Destroy;
end;

function TCodeEditorSearch.Find(const ASearchText: string;
  ACaseSensitive, AWholeWord: Boolean): Boolean;
var
  Options: TSynSearchOptions;
  StartPos, EndPos: TPoint;
begin
  Options := [];

  if ACaseSensitive then
    Options := Options + [ssoMatchCase];
  if AWholeWord then
    Options := Options + [ssoWholeWord];

  FSearchEngine.Pattern := ASearchText;
  FSearchEngine.Options := Options;

  // Commencer après la position actuelle
  StartPos := FEditor.CaretXY;
  EndPos := Point(1, FEditor.Lines.Count);

  Result := FSearchEngine.FindNextOne(FEditor, StartPos, EndPos) > 0;

  if Result then
  begin
    // Sélectionner le texte trouvé
    FEditor.BlockBegin := FSearchEngine.GetFoundStart;
    FEditor.BlockEnd := FSearchEngine.GetFoundEnd;
    FEditor.EnsureCursorPosVisible;
  end;
end;

function TCodeEditorSearch.Replace(const ASearchText, AReplaceText: string;
  APrompt: Boolean): Integer;
var
  Options: TSynSearchOptions;
  StartPos, EndPos: TPoint;
  FoundCount: Integer;
  UserResponse: TModalResult;
begin
  FoundCount := 0;
  Options := [ssoMatchCase];

  FSearchEngine.Pattern := ASearchText;
  FSearchEngine.Options := Options;

  StartPos := Point(1, 1);
  EndPos := Point(Length(FEditor.Lines[FEditor.Lines.Count - 1]),
                  FEditor.Lines.Count);

  while FSearchEngine.FindNextOne(FEditor, StartPos, EndPos) > 0 do
  begin
    FEditor.BlockBegin := FSearchEngine.GetFoundStart;
    FEditor.BlockEnd := FSearchEngine.GetFoundEnd;
    FEditor.EnsureCursorPosVisible;

    if APrompt then
    begin
      UserResponse := MessageDlg('Remplacement',
        'Remplacer cette occurrence ?',
        mtConfirmation, [mbYes, mbNo, mbCancel], 0);

      if UserResponse = mrCancel then
        Break;

      if UserResponse = mrYes then
      begin
        FEditor.SelText := AReplaceText;
        Inc(FoundCount);
      end;
    end
    else
    begin
      FEditor.SelText := AReplaceText;
      Inc(FoundCount);
    end;

    StartPos := FEditor.BlockEnd;
  end;

  Result := FoundCount;
end;

procedure TCodeEditorSearch.FindNext;  
begin
  // Réutiliser la dernière recherche
  if FSearchEngine.Pattern <> '' then
  begin
    Find(FSearchEngine.Pattern,
         ssoMatchCase in FSearchEngine.Options,
         ssoWholeWord in FSearchEngine.Options);
  end;
end;
```

#### Marqueurs et signets

```pascal
procedure TfrmCodeEditor.AddBookmark(ALine: Integer);  
begin
  // Ajouter un signet à la ligne spécifiée
  FSynEdit.SetBookMark(ALine, ALine, 0);
end;

procedure TfrmCodeEditor.RemoveBookmark(ALine: Integer);  
begin
  FSynEdit.ClearBookMark(ALine);
end;

procedure TfrmCodeEditor.NextBookmark;  
var
  i, CurrentLine: Integer;
begin
  CurrentLine := FSynEdit.CaretY;

  // Chercher le prochain signet
  for i := CurrentLine + 1 to FSynEdit.Lines.Count do
  begin
    if FSynEdit.IsBookmark(i) then
    begin
      FSynEdit.CaretY := i;
      FSynEdit.EnsureCursorPosVisible;
      Exit;
    end;
  end;

  // Si rien trouvé, revenir au début
  for i := 1 to CurrentLine do
  begin
    if FSynEdit.IsBookmark(i) then
    begin
      FSynEdit.CaretY := i;
      FSynEdit.EnsureCursorPosVisible;
      Exit;
    end;
  end;
end;
```

## Éditeur de texte riche (RTF)

Pour créer un éditeur de texte avec formatage (gras, italique, couleurs, etc.) :

```pascal
unit RichTextEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, Menus, Buttons, ColorBox;

type
  TfrmRichEditor = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    FRichEdit: TRichMemo;
    FToolbar: TToolBar;
    FFontCombo: TComboBox;
    FFontSizeCombo: TComboBox;
    FBoldBtn: TToolButton;
    FItalicBtn: TToolButton;
    FUnderlineBtn: TToolButton;
    FColorBtn: TToolButton;
    FAlignLeftBtn: TToolButton;
    FAlignCenterBtn: TToolButton;
    FAlignRightBtn: TToolButton;

    procedure SetupRichEdit;
    procedure SetupToolbar;
    procedure CreateFormatButtons;

    procedure OnBoldClick(Sender: TObject);
    procedure OnItalicClick(Sender: TObject);
    procedure OnUnderlineClick(Sender: TObject);
    procedure OnColorClick(Sender: TObject);
    procedure OnFontChange(Sender: TObject);
    procedure OnFontSizeChange(Sender: TObject);
    procedure OnAlignClick(Sender: TObject);
    procedure OnSelectionChange(Sender: TObject);
  public
    procedure LoadRTF(const AFileName: string);
    procedure SaveRTF(const AFileName: string);
  end;

var
  frmRichEditor: TfrmRichEditor;

implementation

{$R *.lfm}

procedure TfrmRichEditor.FormCreate(Sender: TObject);  
begin
  Caption := 'Éditeur de Texte Riche';
  Width := 900;
  Height := 600;
  Position := poScreenCenter;

  SetupToolbar;
  SetupRichEdit;
  CreateFormatButtons;
end;

procedure TfrmRichEditor.SetupToolbar;  
begin
  FToolbar := TToolBar.Create(Self);
  FToolbar.Parent := Self;
  FToolbar.Align := alTop;
  FToolbar.Height := 40;
  FToolbar.ShowCaptions := False;
  FToolbar.Images := nil; // Vous pouvez ajouter une ImageList
end;

procedure TfrmRichEditor.SetupRichEdit;  
begin
  FRichEdit := TRichMemo.Create(Self);
  FRichEdit.Parent := Self;
  FRichEdit.Align := alClient;
  FRichEdit.ScrollBars := ssBoth;
  FRichEdit.OnSelectionChange := @OnSelectionChange;

  // Configuration par défaut
  FRichEdit.Font.Name := 'Arial';
  FRichEdit.Font.Size := 11;
end;

procedure TfrmRichEditor.CreateFormatButtons;  
var
  Separator: TToolButton;
begin
  // Combo police
  FFontCombo := TComboBox.Create(FToolbar);
  FFontCombo.Parent := FToolbar;
  FFontCombo.Width := 150;
  FFontCombo.Style := csDropDownList;
  FFontCombo.Items.Assign(Screen.Fonts);
  FFontCombo.ItemIndex := FFontCombo.Items.IndexOf('Arial');
  FFontCombo.OnChange := @OnFontChange;

  // Combo taille
  FFontSizeCombo := TComboBox.Create(FToolbar);
  FFontSizeCombo.Parent := FToolbar;
  FFontSizeCombo.Width := 60;
  FFontSizeCombo.Style := csDropDownList;
  FFontSizeCombo.Items.Add('8');
  FFontSizeCombo.Items.Add('9');
  FFontSizeCombo.Items.Add('10');
  FFontSizeCombo.Items.Add('11');
  FFontSizeCombo.Items.Add('12');
  FFontSizeCombo.Items.Add('14');
  FFontSizeCombo.Items.Add('16');
  FFontSizeCombo.Items.Add('18');
  FFontSizeCombo.Items.Add('20');
  FFontSizeCombo.Items.Add('24');
  FFontSizeCombo.ItemIndex := 3; // 11
  FFontSizeCombo.OnChange := @OnFontSizeChange;

  // Séparateur
  Separator := TToolButton.Create(FToolbar);
  Separator.Parent := FToolbar;
  Separator.Style := tbsSeparator;

  // Bouton Gras
  FBoldBtn := TToolButton.Create(FToolbar);
  FBoldBtn.Parent := FToolbar;
  FBoldBtn.Caption := 'G';
  FBoldBtn.Font.Style := [fsBold];
  FBoldBtn.Style := tbsCheck;
  FBoldBtn.OnClick := @OnBoldClick;

  // Bouton Italique
  FItalicBtn := TToolButton.Create(FToolbar);
  FItalicBtn.Parent := FToolbar;
  FItalicBtn.Caption := 'I';
  FItalicBtn.Font.Style := [fsItalic];
  FItalicBtn.Style := tbsCheck;
  FItalicBtn.OnClick := @OnItalicClick;

  // Bouton Souligné
  FUnderlineBtn := TToolButton.Create(FToolbar);
  FUnderlineBtn.Parent := FToolbar;
  FUnderlineBtn.Caption := 'S';
  FUnderlineBtn.Font.Style := [fsUnderline];
  FUnderlineBtn.Style := tbsCheck;
  FUnderlineBtn.OnClick := @OnUnderlineClick;

  // Séparateur
  Separator := TToolButton.Create(FToolbar);
  Separator.Parent := FToolbar;
  Separator.Style := tbsSeparator;

  // Bouton Couleur
  FColorBtn := TToolButton.Create(FToolbar);
  FColorBtn.Parent := FToolbar;
  FColorBtn.Caption := 'A';
  FColorBtn.OnClick := @OnColorClick;

  // Séparateur
  Separator := TToolButton.Create(FToolbar);
  Separator.Parent := FToolbar;
  Separator.Style := tbsSeparator;

  // Alignement gauche
  FAlignLeftBtn := TToolButton.Create(FToolbar);
  FAlignLeftBtn.Parent := FToolbar;
  FAlignLeftBtn.Caption := '←';
  FAlignLeftBtn.Tag := Ord(taLeftJustify);
  FAlignLeftBtn.Grouped := True;
  FAlignLeftBtn.OnClick := @OnAlignClick;

  // Alignement centré
  FAlignCenterBtn := TToolButton.Create(FToolbar);
  FAlignCenterBtn.Parent := FToolbar;
  FAlignCenterBtn.Caption := '↔';
  FAlignCenterBtn.Tag := Ord(taCenter);
  FAlignCenterBtn.Grouped := True;
  FAlignCenterBtn.OnClick := @OnAlignClick;

  // Alignement droite
  FAlignRightBtn := TToolButton.Create(FToolbar);
  FAlignRightBtn.Parent := FToolbar;
  FAlignRightBtn.Caption := '→';
  FAlignRightBtn.Tag := Ord(taRightJustify);
  FAlignRightBtn.Grouped := True;
  FAlignRightBtn.OnClick := @OnAlignClick;
end;

procedure TfrmRichEditor.OnBoldClick(Sender: TObject);  
var
  Style: TFontStyles;
begin
  if FRichEdit.SelLength > 0 then
  begin
    Style := FRichEdit.SelAttributes.Style;
    if fsBold in Style then
      Style := Style - [fsBold]
    else
      Style := Style + [fsBold];
    FRichEdit.SelAttributes.Style := Style;
  end;
end;

procedure TfrmRichEditor.OnItalicClick(Sender: TObject);  
var
  Style: TFontStyles;
begin
  if FRichEdit.SelLength > 0 then
  begin
    Style := FRichEdit.SelAttributes.Style;
    if fsItalic in Style then
      Style := Style - [fsItalic]
    else
      Style := Style + [fsItalic];
    FRichEdit.SelAttributes.Style := Style;
  end;
end;

procedure TfrmRichEditor.OnUnderlineClick(Sender: TObject);  
var
  Style: TFontStyles;
begin
  if FRichEdit.SelLength > 0 then
  begin
    Style := FRichEdit.SelAttributes.Style;
    if fsUnderline in Style then
      Style := Style - [fsUnderline]
    else
      Style := Style + [fsUnderline];
    FRichEdit.SelAttributes.Style := Style;
  end;
end;

procedure TfrmRichEditor.OnColorClick(Sender: TObject);  
var
  ColorDialog: TColorDialog;
begin
  ColorDialog := TColorDialog.Create(nil);
  try
    if ColorDialog.Execute then
    begin
      if FRichEdit.SelLength > 0 then
        FRichEdit.SelAttributes.Color := ColorDialog.Color;
    end;
  finally
    ColorDialog.Free;
  end;
end;

procedure TfrmRichEditor.OnFontChange(Sender: TObject);  
begin
  if FFontCombo.ItemIndex >= 0 then
  begin
    if FRichEdit.SelLength > 0 then
      FRichEdit.SelAttributes.Name := FFontCombo.Text
    else
      FRichEdit.Font.Name := FFontCombo.Text;
  end;
end;

procedure TfrmRichEditor.OnFontSizeChange(Sender: TObject);  
var
  Size: Integer;
begin
  if FFontSizeCombo.ItemIndex >= 0 then
  begin
    Size := StrToIntDef(FFontSizeCombo.Text, 11);
    if FRichEdit.SelLength > 0 then
      FRichEdit.SelAttributes.Size := Size
    else
      FRichEdit.Font.Size := Size;
  end;
end;

procedure TfrmRichEditor.OnAlignClick(Sender: TObject);  
var
  Alignment: TAlignment;
begin
  Alignment := TAlignment((Sender as TToolButton).Tag);
  FRichEdit.Paragraph.Alignment := Alignment;
end;

procedure TfrmRichEditor.OnSelectionChange(Sender: TObject);  
begin
  // Mettre à jour les boutons selon la sélection
  if FRichEdit.SelLength > 0 then
  begin
    FBoldBtn.Down := fsBold in FRichEdit.SelAttributes.Style;
    FItalicBtn.Down := fsItalic in FRichEdit.SelAttributes.Style;
    FUnderlineBtn.Down := fsUnderline in FRichEdit.SelAttributes.Style;

    FFontCombo.ItemIndex := FFontCombo.Items.IndexOf(
      FRichEdit.SelAttributes.Name);
    FFontSizeCombo.Text := IntToStr(FRichEdit.SelAttributes.Size);
  end;
end;

procedure TfrmRichEditor.LoadRTF(const AFileName: string);  
begin
  if FileExists(AFileName) then
    FRichEdit.Lines.LoadFromFile(AFileName);
end;

procedure TfrmRichEditor.SaveRTF(const AFileName: string);  
begin
  FRichEdit.Lines.SaveToFile(AFileName);
end;

end.
```

## Designer de formulaires visuel

Créer un designer qui permet aux utilisateurs de concevoir des interfaces graphiques :

```pascal
unit FormDesigner;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Menus, Buttons, ComCtrls;

type
  TDesignerComponent = class
    Control: TControl;
    Selected: Boolean;
    Rect: TRect;
  end;

  TfrmFormDesigner = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FDesignPanel: TPanel;
    FToolbox: TPanel;
    FPropertyGrid: TListView;
    FComponents: TList;
    FSelectedComponent: TDesignerComponent;
    FDragging: Boolean;
    FResizing: Boolean;
    FDragStart: TPoint;

    procedure SetupInterface;
    procedure CreateToolbox;
    procedure CreatePropertyGrid;

    procedure OnToolboxButtonClick(Sender: TObject);
    procedure OnDesignPanelMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OnDesignPanelMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure OnDesignPanelMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OnDesignPanelPaint(Sender: TObject);

    procedure AddComponent(AComponentClass: TControlClass; X, Y: Integer);
    procedure SelectComponent(AComponent: TDesignerComponent);
    procedure DrawSelectionHandles(ACanvas: TCanvas; ARect: TRect);
    procedure UpdatePropertyGrid;

  public
    procedure SaveDesign(const AFileName: string);
    procedure LoadDesign(const AFileName: string);
  end;

var
  frmFormDesigner: TfrmFormDesigner;

implementation

{$R *.lfm}

uses
  IniFiles;

procedure TfrmFormDesigner.FormCreate(Sender: TObject);  
begin
  Caption := 'Designer de Formulaires';
  Width := 1200;
  Height := 800;
  Position := poScreenCenter;

  FComponents := TList.Create;
  FDragging := False;
  FResizing := False;
  FSelectedComponent := nil;

  SetupInterface;
end;

procedure TfrmFormDesigner.FormDestroy(Sender: TObject);  
var
  i: Integer;
begin
  for i := 0 to FComponents.Count - 1 do
    TDesignerComponent(FComponents[i]).Free;
  FComponents.Free;
end;

procedure TfrmFormDesigner.SetupInterface;  
begin
  CreateToolbox;
  CreatePropertyGrid;

  // Panel de design au centre
  FDesignPanel := TPanel.Create(Self);
  FDesignPanel.Parent := Self;
  FDesignPanel.Align := alClient;
  FDesignPanel.Color := clWhite;
  FDesignPanel.BevelOuter := bvNone;
  FDesignPanel.OnMouseDown := @OnDesignPanelMouseDown;
  FDesignPanel.OnMouseMove := @OnDesignPanelMouseMove;
  FDesignPanel.OnMouseUp := @OnDesignPanelMouseUp;
  FDesignPanel.OnPaint := @OnDesignPanelPaint;
end;

procedure TfrmFormDesigner.CreateToolbox;  
var
  Panel: TPanel;
  Btn: TSpeedButton;
begin
  // Panel gauche pour la boîte à outils
  Panel := TPanel.Create(Self);
  Panel.Parent := Self;
  Panel.Align := alLeft;
  Panel.Width := 120;
  Panel.Caption := '';

  FToolbox := Panel;

  // Label titre
  with TLabel.Create(Panel) do
  begin
    Parent := Panel;
    Align := alTop;
    Caption := ' Boîte à outils';
    Font.Style := [fsBold];
    Height := 25;
    Alignment := taCenter;
    Layout := tlCenter;
  end;

  // Bouton Label
  Btn := TSpeedButton.Create(Panel);
  Btn.Parent := Panel;
  Btn.Caption := 'Label';
  Btn.Left := 10;
  Btn.Top := 35;
  Btn.Width := 100;
  Btn.Tag := 1; // Identifiant du type
  Btn.OnClick := @OnToolboxButtonClick;

  // Bouton Edit
  Btn := TSpeedButton.Create(Panel);
  Btn.Parent := Panel;
  Btn.Caption := 'Edit';
  Btn.Left := 10;
  Btn.Top := 65;
  Btn.Width := 100;
  Btn.Tag := 2;
  Btn.OnClick := @OnToolboxButtonClick;

  // Bouton Button
  Btn := TSpeedButton.Create(Panel);
  Btn.Parent := Panel;
  Btn.Caption := 'Button';
  Btn.Left := 10;
  Btn.Top := 95;
  Btn.Width := 100;
  Btn.Tag := 3;
  Btn.OnClick := @OnToolboxButtonClick;

  // Bouton CheckBox
  Btn := TSpeedButton.Create(Panel);
  Btn.Parent := Panel;
  Btn.Caption := 'CheckBox';
  Btn.Left := 10;
  Btn.Top := 125;
  Btn.Width := 100;
  Btn.Tag := 4;
  Btn.OnClick := @OnToolboxButtonClick;

  // Bouton Panel
  Btn := TSpeedButton.Create(Panel);
  Btn.Parent := Panel;
  Btn.Caption := 'Panel';
  Btn.Left := 10;
  Btn.Top := 155;
  Btn.Width := 100;
  Btn.Tag := 5;
  Btn.OnClick := @OnToolboxButtonClick;

  // Bouton Memo
  Btn := TSpeedButton.Create(Panel);
  Btn.Parent := Panel;
  Btn.Caption := 'Memo';
  Btn.Left := 10;
  Btn.Top := 185;
  Btn.Width := 100;
  Btn.Tag := 6;
  Btn.OnClick := @OnToolboxButtonClick;
end;

procedure TfrmFormDesigner.CreatePropertyGrid;  
var
  Panel: TPanel;
  Column: TListColumn;
begin
  // Panel droit pour les propriétés
  Panel := TPanel.Create(Self);
  Panel.Parent := Self;
  Panel.Align := alRight;
  Panel.Width := 250;
  Panel.Caption := '';

  // Label titre
  with TLabel.Create(Panel) do
  begin
    Parent := Panel;
    Align := alTop;
    Caption := ' Propriétés';
    Font.Style := [fsBold];
    Height := 25;
    Alignment := taCenter;
    Layout := tlCenter;
  end;

  // ListView pour les propriétés
  FPropertyGrid := TListView.Create(Panel);
  FPropertyGrid.Parent := Panel;
  FPropertyGrid.Align := alClient;
  FPropertyGrid.ViewStyle := vsReport;
  FPropertyGrid.RowSelect := True;
  FPropertyGrid.ReadOnly := False;

  // Colonnes
  Column := FPropertyGrid.Columns.Add;
  Column.Caption := 'Propriété';
  Column.Width := 100;

  Column := FPropertyGrid.Columns.Add;
  Column.Caption := 'Valeur';
  Column.Width := 140;
end;

procedure TfrmFormDesigner.OnToolboxButtonClick(Sender: TObject);  
var
  ComponentType: Integer;
begin
  ComponentType := (Sender as TSpeedButton).Tag;

  // Changer le curseur pour indiquer qu'on va placer un composant
  FDesignPanel.Cursor := crCross;

  // Stocker le type de composant à créer
  FDesignPanel.Tag := ComponentType;
end;

procedure TfrmFormDesigner.OnDesignPanelMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ComponentType: Integer;
  i: Integer;
  DesignComp: TDesignerComponent;
  HitComponent: TDesignerComponent;
begin
  if Button = mbLeft then
  begin
    ComponentType := FDesignPanel.Tag;

    if ComponentType > 0 then
    begin
      // Créer un nouveau composant
      case ComponentType of
        1: AddComponent(TLabel, X, Y);
        2: AddComponent(TEdit, X, Y);
        3: AddComponent(TButton, X, Y);
        4: AddComponent(TCheckBox, X, Y);
        5: AddComponent(TPanel, X, Y);
        6: AddComponent(TMemo, X, Y);
      end;

      FDesignPanel.Tag := 0;
      FDesignPanel.Cursor := crDefault;
    end
    else
    begin
      // Sélectionner un composant existant
      HitComponent := nil;

      for i := FComponents.Count - 1 downto 0 do
      begin
        DesignComp := TDesignerComponent(FComponents[i]);
        if PtInRect(DesignComp.Rect, Point(X, Y)) then
        begin
          HitComponent := DesignComp;
          Break;
        end;
      end;

      if Assigned(HitComponent) then
      begin
        SelectComponent(HitComponent);
        FDragging := True;
        FDragStart := Point(X, Y);
      end
      else
      begin
        SelectComponent(nil);
      end;
    end;
  end;
end;

procedure TfrmFormDesigner.OnDesignPanelMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  DeltaX, DeltaY: Integer;
begin
  if FDragging and Assigned(FSelectedComponent) then
  begin
    // Déplacer le composant
    DeltaX := X - FDragStart.X;
    DeltaY := Y - FDragStart.Y;

    FSelectedComponent.Control.Left := FSelectedComponent.Control.Left + DeltaX;
    FSelectedComponent.Control.Top := FSelectedComponent.Control.Top + DeltaY;

    FSelectedComponent.Rect := FSelectedComponent.Control.BoundsRect;

    FDragStart := Point(X, Y);
    FDesignPanel.Invalidate;
    UpdatePropertyGrid;
  end;
end;

procedure TfrmFormDesigner.OnDesignPanelMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FDragging := False;
  FResizing := False;
end;

procedure TfrmFormDesigner.OnDesignPanelPaint(Sender: TObject);  
var
  i: Integer;
  DesignComp: TDesignerComponent;
begin
  // Dessiner les poignées de sélection pour le composant sélectionné
  if Assigned(FSelectedComponent) then
  begin
    DrawSelectionHandles(FDesignPanel.Canvas, FSelectedComponent.Rect);
  end;
end;

procedure TfrmFormDesigner.AddComponent(AComponentClass: TControlClass;
  X, Y: Integer);
var
  NewControl: TControl;
  DesignComp: TDesignerComponent;
begin
  // Créer le contrôle
  NewControl := AComponentClass.Create(FDesignPanel);
  NewControl.Parent := FDesignPanel;
  NewControl.Left := X;
  NewControl.Top := Y;

  // Configuration par défaut selon le type
  if NewControl is TLabel then
  begin
    (NewControl as TLabel).Caption := 'Label' + IntToStr(FComponents.Count + 1);
    NewControl.Width := 60;
    NewControl.Height := 20;
  end
  else if NewControl is TEdit then
  begin
    (NewControl as TEdit).Text := '';
    NewControl.Width := 120;
    NewControl.Height := 25;
  end
  else if NewControl is TButton then
  begin
    (NewControl as TButton).Caption := 'Button' + IntToStr(FComponents.Count + 1);
    NewControl.Width := 80;
    NewControl.Height := 30;
  end
  else if NewControl is TCheckBox then
  begin
    (NewControl as TCheckBox).Caption := 'CheckBox' + IntToStr(FComponents.Count + 1);
    NewControl.Width := 100;
    NewControl.Height := 20;
  end
  else if NewControl is TPanel then
  begin
    (NewControl as TPanel).Caption := '';
    (NewControl as TPanel).BevelOuter := bvLowered;
    NewControl.Width := 150;
    NewControl.Height := 100;
  end
  else if NewControl is TMemo then
  begin
    (NewControl as TMemo).Lines.Clear;
    NewControl.Width := 200;
    NewControl.Height := 100;
  end;

  // Créer l'objet designer
  DesignComp := TDesignerComponent.Create;
  DesignComp.Control := NewControl;
  DesignComp.Selected := False;
  DesignComp.Rect := NewControl.BoundsRect;

  FComponents.Add(DesignComp);

  // Sélectionner automatiquement
  SelectComponent(DesignComp);
end;

procedure TfrmFormDesigner.SelectComponent(AComponent: TDesignerComponent);  
var
  i: Integer;
begin
  // Désélectionner tout
  for i := 0 to FComponents.Count - 1 do
    TDesignerComponent(FComponents[i]).Selected := False;

  // Sélectionner le nouveau composant
  FSelectedComponent := AComponent;
  if Assigned(FSelectedComponent) then
  begin
    FSelectedComponent.Selected := True;
    UpdatePropertyGrid;
  end
  else
  begin
    FPropertyGrid.Items.Clear;
  end;

  FDesignPanel.Invalidate;
end;

procedure TfrmFormDesigner.DrawSelectionHandles(ACanvas: TCanvas; ARect: TRect);  
const
  HandleSize = 6;
var
  Points: array[0..7] of TPoint;
  i: Integer;
begin
  // Dessiner le cadre de sélection
  ACanvas.Pen.Color := clBlue;
  ACanvas.Pen.Width := 2;
  ACanvas.Pen.Style := psDot;
  ACanvas.Brush.Style := bsClear;
  ACanvas.Rectangle(ARect);

  // Calculer les positions des poignées
  Points[0] := Point(ARect.Left, ARect.Top);                    // Haut-gauche
  Points[1] := Point((ARect.Left + ARect.Right) div 2, ARect.Top); // Haut-centre
  Points[2] := Point(ARect.Right, ARect.Top);                   // Haut-droite
  Points[3] := Point(ARect.Right, (ARect.Top + ARect.Bottom) div 2); // Droite-centre
  Points[4] := Point(ARect.Right, ARect.Bottom);                // Bas-droite
  Points[5] := Point((ARect.Left + ARect.Right) div 2, ARect.Bottom); // Bas-centre
  Points[6] := Point(ARect.Left, ARect.Bottom);                 // Bas-gauche
  Points[7] := Point(ARect.Left, (ARect.Top + ARect.Bottom) div 2); // Gauche-centre

  // Dessiner les poignées
  ACanvas.Brush.Style := bsSolid;
  ACanvas.Brush.Color := clWhite;
  ACanvas.Pen.Style := psSolid;
  ACanvas.Pen.Color := clBlue;

  for i := 0 to 7 do
  begin
    ACanvas.Rectangle(
      Points[i].X - HandleSize div 2,
      Points[i].Y - HandleSize div 2,
      Points[i].X + HandleSize div 2,
      Points[i].Y + HandleSize div 2
    );
  end;
end;

procedure TfrmFormDesigner.UpdatePropertyGrid;  
var
  Item: TListItem;
begin
  FPropertyGrid.Items.Clear;

  if not Assigned(FSelectedComponent) then Exit;

  // Nom
  Item := FPropertyGrid.Items.Add;
  Item.Caption := 'Name';
  Item.SubItems.Add(FSelectedComponent.Control.Name);

  // Position X
  Item := FPropertyGrid.Items.Add;
  Item.Caption := 'Left';
  Item.SubItems.Add(IntToStr(FSelectedComponent.Control.Left));

  // Position Y
  Item := FPropertyGrid.Items.Add;
  Item.Caption := 'Top';
  Item.SubItems.Add(IntToStr(FSelectedComponent.Control.Top));

  // Largeur
  Item := FPropertyGrid.Items.Add;
  Item.Caption := 'Width';
  Item.SubItems.Add(IntToStr(FSelectedComponent.Control.Width));

  // Hauteur
  Item := FPropertyGrid.Items.Add;
  Item.Caption := 'Height';
  Item.SubItems.Add(IntToStr(FSelectedComponent.Control.Height));

  // Caption/Text selon le type
  if FSelectedComponent.Control is TLabel then
  begin
    Item := FPropertyGrid.Items.Add;
    Item.Caption := 'Caption';
    Item.SubItems.Add((FSelectedComponent.Control as TLabel).Caption);
  end
  else if FSelectedComponent.Control is TButton then
  begin
    Item := FPropertyGrid.Items.Add;
    Item.Caption := 'Caption';
    Item.SubItems.Add((FSelectedComponent.Control as TButton).Caption);
  end
  else if FSelectedComponent.Control is TEdit then
  begin
    Item := FPropertyGrid.Items.Add;
    Item.Caption := 'Text';
    Item.SubItems.Add((FSelectedComponent.Control as TEdit).Text);
  end;
end;

procedure TfrmFormDesigner.SaveDesign(const AFileName: string);  
var
  Ini: TIniFile;
  i: Integer;
  DesignComp: TDesignerComponent;
  Section: string;
begin
  Ini := TIniFile.Create(AFileName);
  try
    Ini.WriteInteger('Design', 'ComponentCount', FComponents.Count);

    for i := 0 to FComponents.Count - 1 do
    begin
      DesignComp := TDesignerComponent(FComponents[i]);
      Section := 'Component' + IntToStr(i);

      Ini.WriteString(Section, 'Class', DesignComp.Control.ClassName);
      Ini.WriteInteger(Section, 'Left', DesignComp.Control.Left);
      Ini.WriteInteger(Section, 'Top', DesignComp.Control.Top);
      Ini.WriteInteger(Section, 'Width', DesignComp.Control.Width);
      Ini.WriteInteger(Section, 'Height', DesignComp.Control.Height);

      if DesignComp.Control is TLabel then
        Ini.WriteString(Section, 'Caption', (DesignComp.Control as TLabel).Caption)
      else if DesignComp.Control is TButton then
        Ini.WriteString(Section, 'Caption', (DesignComp.Control as TButton).Caption)
      else if DesignComp.Control is TEdit then
        Ini.WriteString(Section, 'Text', (DesignComp.Control as TEdit).Text);
    end;
  finally
    Ini.Free;
  end;
end;

procedure TfrmFormDesigner.LoadDesign(const AFileName: string);  
var
  Ini: TIniFile;
  i, Count: Integer;
  Section, ClassName: string;
  Left, Top, Width, Height: Integer;
  Caption: string;
  NewControl: TControl;
  DesignComp: TDesignerComponent;
begin
  if not FileExists(AFileName) then Exit;

  // Effacer le design actuel
  for i := FComponents.Count - 1 downto 0 do
  begin
    TDesignerComponent(FComponents[i]).Control.Free;
    TDesignerComponent(FComponents[i]).Free;
  end;
  FComponents.Clear;

  Ini := TIniFile.Create(AFileName);
  try
    Count := Ini.ReadInteger('Design', 'ComponentCount', 0);

    for i := 0 to Count - 1 do
    begin
      Section := 'Component' + IntToStr(i);

      ClassName := Ini.ReadString(Section, 'Class', '');
      Left := Ini.ReadInteger(Section, 'Left', 0);
      Top := Ini.ReadInteger(Section, 'Top', 0);
      Width := Ini.ReadInteger(Section, 'Width', 100);
      Height := Ini.ReadInteger(Section, 'Height', 30);
      Caption := Ini.ReadString(Section, 'Caption', '');

      // Créer le composant
      NewControl := nil;
      if ClassName = 'TLabel' then
      begin
        NewControl := TLabel.Create(FDesignPanel);
        (NewControl as TLabel).Caption := Caption;
      end
      else if ClassName = 'TButton' then
      begin
        NewControl := TButton.Create(FDesignPanel);
        (NewControl as TButton).Caption := Caption;
      end
      else if ClassName = 'TEdit' then
      begin
        NewControl := TEdit.Create(FDesignPanel);
        (NewControl as TEdit).Text := Ini.ReadString(Section, 'Text', '');
      end
      else if ClassName = 'TCheckBox' then
      begin
        NewControl := TCheckBox.Create(FDesignPanel);
        (NewControl as TCheckBox).Caption := Caption;
      end
      else if ClassName = 'TPanel' then
      begin
        NewControl := TPanel.Create(FDesignPanel);
        (NewControl as TPanel).BevelOuter := bvLowered;
      end
      else if ClassName = 'TMemo' then
      begin
        NewControl := TMemo.Create(FDesignPanel);
      end;

      if Assigned(NewControl) then
      begin
        NewControl.Parent := FDesignPanel;
        NewControl.Left := Left;
        NewControl.Top := Top;
        NewControl.Width := Width;
        NewControl.Height := Height;

        DesignComp := TDesignerComponent.Create;
        DesignComp.Control := NewControl;
        DesignComp.Selected := False;
        DesignComp.Rect := NewControl.BoundsRect;

        FComponents.Add(DesignComp);
      end;
    end;
  finally
    Ini.Free;
  end;

  FDesignPanel.Invalidate;
end;

end.
```

## Éditeur de requêtes SQL visuel

Un éditeur qui aide les utilisateurs à construire des requêtes SQL de manière visuelle :

```pascal
unit SQLQueryBuilder;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, Buttons, Menus;

type
  TQueryType = (qtSelect, qtInsert, qtUpdate, qtDelete);

  TfrmSQLQueryBuilder = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    // Interface
    FNotebook: TNotebook;
    FTablesListBox: TListBox;
    FColumnsListBox: TCheckListBox;
    FWherePanel: TPanel;
    FOrderByPanel: TPanel;
    FSQLMemo: TMemo;

    // Données
    FQueryType: TQueryType;
    FSelectedTable: string;
    FSelectedColumns: TStringList;
    FWhereConditions: TStringList;
    FOrderByColumns: TStringList;

    procedure SetupInterface;
    procedure CreateTablesPanel;
    procedure CreateColumnsPanel;
    procedure CreateWherePanel;
    procedure CreateOrderByPanel;
    procedure CreateSQLPreview;

    procedure OnTableSelect(Sender: TObject);
    procedure OnColumnCheck(Sender: TObject);
    procedure OnQueryTypeChange(Sender: TObject);
    procedure OnAddWhereCondition(Sender: TObject);
    procedure OnDeleteWhereCondition(Sender: TObject);
    procedure OnGenerateSQL(Sender: TObject);

    function BuildSelectQuery: string;
    function BuildInsertQuery: string;
    function BuildUpdateQuery: string;
    function BuildDeleteQuery: string;

  public
    procedure LoadTables(ATables: TStrings);
    procedure LoadColumns(ATable: string; AColumns: TStrings);
    function GetGeneratedSQL: string;
  end;

var
  frmSQLQueryBuilder: TfrmSQLQueryBuilder;

implementation

{$R *.lfm}

procedure TfrmSQLQueryBuilder.FormCreate(Sender: TObject);  
begin
  Caption := 'Constructeur de Requêtes SQL';
  Width := 900;
  Height := 600;
  Position := poScreenCenter;

  FSelectedColumns := TStringList.Create;
  FWhereConditions := TStringList.Create;
  FOrderByColumns := TStringList.Create;
  FQueryType := qtSelect;

  SetupInterface;
end;

procedure TfrmSQLQueryBuilder.SetupInterface;  
var
  MainPanel: TPanel;
  TopPanel: TPanel;
  RadioGroup: TRadioGroup;
begin
  // Panel principal
  MainPanel := TPanel.Create(Self);
  MainPanel.Parent := Self;
  MainPanel.Align := alClient;
  MainPanel.BevelOuter := bvNone;

  // Panel du haut pour le type de requête
  TopPanel := TPanel.Create(Self);
  TopPanel.Parent := Self;
  TopPanel.Align := alTop;
  TopPanel.Height := 60;
  TopPanel.BevelOuter := bvNone;

  RadioGroup := TRadioGroup.Create(TopPanel);
  RadioGroup.Parent := TopPanel;
  RadioGroup.Align := alClient;
  RadioGroup.Caption := 'Type de requête';
  RadioGroup.Items.Add('SELECT');
  RadioGroup.Items.Add('INSERT');
  RadioGroup.Items.Add('UPDATE');
  RadioGroup.Items.Add('DELETE');
  RadioGroup.ItemIndex := 0;
  RadioGroup.Columns := 4;
  RadioGroup.OnClick := @OnQueryTypeChange;

  // Notebook pour les différents panneaux
  FNotebook := TNotebook.Create(MainPanel);
  FNotebook.Parent := MainPanel;
  FNotebook.Align := alClient;

  CreateTablesPanel;
  CreateColumnsPanel;
  CreateWherePanel;
  CreateOrderByPanel;
  CreateSQLPreview;
end;

procedure TfrmSQLQueryBuilder.CreateTablesPanel;  
var
  Page: TPage;
  Panel: TPanel;
  Label1: TLabel;
begin
  Page := FNotebook.Pages.Add('Tables');

  Panel := TPanel.Create(Page);
  Panel.Parent := Page;
  Panel.Align := alClient;
  Panel.BevelOuter := bvNone;

  Label1 := TLabel.Create(Panel);
  Label1.Parent := Panel;
  Label1.Align := alTop;
  Label1.Caption := 'Sélectionnez une table :';
  Label1.Height := 25;

  FTablesListBox := TListBox.Create(Panel);
  FTablesListBox.Parent := Panel;
  FTablesListBox.Align := alClient;
  FTablesListBox.OnClick := @OnTableSelect;
end;

procedure TfrmSQLQueryBuilder.CreateColumnsPanel;  
var
  Page: TPage;
  Panel: TPanel;
  Label1: TLabel;
begin
  Page := FNotebook.Pages.Add('Columns');

  Panel := TPanel.Create(Page);
  Panel.Parent := Page;
  Panel.Align := alClient;
  Panel.BevelOuter := bvNone;

  Label1 := TLabel.Create(Panel);
  Label1.Parent := Panel;
  Label1.Align := alTop;
  Label1.Caption := 'Sélectionnez les colonnes :';
  Label1.Height := 25;

  FColumnsListBox := TCheckListBox.Create(Panel);
  FColumnsListBox.Parent := Panel;
  FColumnsListBox.Align := alClient;
  FColumnsListBox.OnClickCheck := @OnColumnCheck;
end;

procedure TfrmSQLQueryBuilder.CreateWherePanel;  
var
  Page: TPage;
  Panel, ButtonPanel: TPanel;
  Label1: TLabel;
  Btn: TButton;
begin
  Page := FNotebook.Pages.Add('Where');

  FWherePanel := TPanel.Create(Page);
  FWherePanel.Parent := Page;
  FWherePanel.Align := alClient;
  FWherePanel.BevelOuter := bvNone;

  Label1 := TLabel.Create(FWherePanel);
  Label1.Parent := FWherePanel;
  Label1.Align := alTop;
  Label1.Caption := 'Conditions WHERE :';
  Label1.Height := 25;

  // Panel pour les boutons
  ButtonPanel := TPanel.Create(FWherePanel);
  ButtonPanel.Parent := FWherePanel;
  ButtonPanel.Align := alBottom;
  ButtonPanel.Height := 40;
  ButtonPanel.BevelOuter := bvNone;

  Btn := TButton.Create(ButtonPanel);
  Btn.Parent := ButtonPanel;
  Btn.Caption := 'Ajouter une condition';
  Btn.Left := 10;
  Btn.Top := 5;
  Btn.Width := 150;
  Btn.OnClick := @OnAddWhereCondition;
end;

procedure TfrmSQLQueryBuilder.CreateOrderByPanel;  
var
  Page: TPage;
  Panel: TPanel;
  Label1: TLabel;
begin
  Page := FNotebook.Pages.Add('OrderBy');

  FOrderByPanel := TPanel.Create(Page);
  FOrderByPanel.Parent := Page;
  FOrderByPanel.Align := alClient;
  FOrderByPanel.BevelOuter := bvNone;

  Label1 := TLabel.Create(FOrderByPanel);
  Label1.Parent := FOrderByPanel;
  Label1.Align := alTop;
  Label1.Caption := 'Ordre de tri (ORDER BY) :';
  Label1.Height := 25;
end;

procedure TfrmSQLQueryBuilder.CreateSQLPreview;  
var
  Page: TPage;
  Panel, ButtonPanel: TPanel;
  Label1: TLabel;
  Btn: TButton;
begin
  Page := FNotebook.Pages.Add('SQL');

  Panel := TPanel.Create(Page);
  Panel.Parent := Page;
  Panel.Align := alClient;
  Panel.BevelOuter := bvNone;

  Label1 := TLabel.Create(Panel);
  Label1.Parent := Panel;
  Label1.Align := alTop;
  Label1.Caption := 'Requête SQL générée :';
  Label1.Height := 25;

  FSQLMemo := TMemo.Create(Panel);
  FSQLMemo.Parent := Panel;
  FSQLMemo.Align := alClient;
  FSQLMemo.ScrollBars := ssBoth;
  FSQLMemo.Font.Name := 'Courier New';
  FSQLMemo.Font.Size := 10;
  FSQLMemo.ReadOnly := True;

  // Bouton génération
  ButtonPanel := TPanel.Create(Panel);
  ButtonPanel.Parent := Panel;
  ButtonPanel.Align := alBottom;
  ButtonPanel.Height := 40;
  ButtonPanel.BevelOuter := bvNone;

  Btn := TButton.Create(ButtonPanel);
  Btn.Parent := ButtonPanel;
  Btn.Caption := 'Générer SQL';
  Btn.Left := 10;
  Btn.Top := 5;
  Btn.Width := 120;
  Btn.OnClick := @OnGenerateSQL;
end;

procedure TfrmSQLQueryBuilder.OnTableSelect(Sender: TObject);  
begin
  if FTablesListBox.ItemIndex >= 0 then
  begin
    FSelectedTable := FTablesListBox.Items[FTablesListBox.ItemIndex];

    // Charger les colonnes de cette table (à implémenter)
    // Pour l'exemple, on ajoute des colonnes fictives
    FColumnsListBox.Items.Clear;
    FColumnsListBox.Items.Add('id');
    FColumnsListBox.Items.Add('name');
    FColumnsListBox.Items.Add('email');
    FColumnsListBox.Items.Add('created_at');

    // Passer à l'onglet colonnes
    FNotebook.PageIndex := 1;
  end;
end;

procedure TfrmSQLQueryBuilder.OnColumnCheck(Sender: TObject);  
var
  i: Integer;
begin
  FSelectedColumns.Clear;

  for i := 0 to FColumnsListBox.Items.Count - 1 do
  begin
    if FColumnsListBox.Checked[i] then
      FSelectedColumns.Add(FColumnsListBox.Items[i]);
  end;
end;

procedure TfrmSQLQueryBuilder.OnQueryTypeChange(Sender: TObject);  
var
  Index: Integer;
begin
  Index := (Sender as TRadioGroup).ItemIndex;

  case Index of
    0: FQueryType := qtSelect;
    1: FQueryType := qtInsert;
    2: FQueryType := qtUpdate;
    3: FQueryType := qtDelete;
  end;

  // Réinitialiser
  FSelectedColumns.Clear;
  FWhereConditions.Clear;
  FOrderByColumns.Clear;
end;

procedure TfrmSQLQueryBuilder.OnAddWhereCondition(Sender: TObject);  
var
  Panel: TPanel;
  ComboColumn, ComboOperator: TComboBox;
  EditValue: TEdit;
  BtnDelete: TButton;
  Y: Integer;
begin
  // Calculer la position Y
  Y := 30 + (FWhereConditions.Count * 35);

  // Panel pour une condition
  Panel := TPanel.Create(FWherePanel);
  Panel.Parent := FWherePanel;
  Panel.Top := Y;
  Panel.Left := 10;
  Panel.Width := FWherePanel.Width - 20;
  Panel.Height := 30;
  Panel.Anchors := [akLeft, akTop, akRight];
  Panel.BevelOuter := bvNone;

  // Combo colonne
  ComboColumn := TComboBox.Create(Panel);
  ComboColumn.Parent := Panel;
  ComboColumn.Left := 0;
  ComboColumn.Top := 5;
  ComboColumn.Width := 120;
  ComboColumn.Style := csDropDownList;
  ComboColumn.Items.Assign(FColumnsListBox.Items);

  // Combo opérateur
  ComboOperator := TComboBox.Create(Panel);
  ComboOperator.Parent := Panel;
  ComboOperator.Left := 130;
  ComboOperator.Top := 5;
  ComboOperator.Width := 80;
  ComboOperator.Style := csDropDownList;
  ComboOperator.Items.Add('=');
  ComboOperator.Items.Add('<>');
  ComboOperator.Items.Add('>');
  ComboOperator.Items.Add('<');
  ComboOperator.Items.Add('>=');
  ComboOperator.Items.Add('<=');
  ComboOperator.Items.Add('LIKE');
  ComboOperator.ItemIndex := 0;

  // Edit valeur
  EditValue := TEdit.Create(Panel);
  EditValue.Parent := Panel;
  EditValue.Left := 220;
  EditValue.Top := 5;
  EditValue.Width := 150;

  // Bouton supprimer
  BtnDelete := TButton.Create(Panel);
  BtnDelete.Parent := Panel;
  BtnDelete.Left := 380;
  BtnDelete.Top := 3;
  BtnDelete.Width := 60;
  BtnDelete.Height := 25;
  BtnDelete.Caption := 'Suppr.';
  BtnDelete.Tag := PtrInt(Panel);
  BtnDelete.OnClick := @OnDeleteWhereCondition;

  FWhereConditions.Add('condition');
end;

procedure TfrmSQLQueryBuilder.OnDeleteWhereCondition(Sender: TObject);  
begin
  TPanel(TButton(Sender).Tag).Free;
end;

procedure TfrmSQLQueryBuilder.OnGenerateSQL(Sender: TObject);  
var
  SQL: string;
begin
  case FQueryType of
    qtSelect: SQL := BuildSelectQuery;
    qtInsert: SQL := BuildInsertQuery;
    qtUpdate: SQL := BuildUpdateQuery;
    qtDelete: SQL := BuildDeleteQuery;
  end;

  FSQLMemo.Lines.Text := SQL;
end;

function TfrmSQLQueryBuilder.BuildSelectQuery: string;  
var
  SQL: string;
  i: Integer;
  ColumnsStr: string;
  WhereStr: string;
begin
  // Construire la liste des colonnes
  if FSelectedColumns.Count = 0 then
    ColumnsStr := '*'
  else
  begin
    ColumnsStr := '';
    for i := 0 to FSelectedColumns.Count - 1 do
    begin
      if i > 0 then
        ColumnsStr := ColumnsStr + ', ';
      ColumnsStr := ColumnsStr + FSelectedColumns[i];
    end;
  end;

  // Requête de base
  SQL := 'SELECT ' + ColumnsStr + LineEnding;
  SQL := SQL + 'FROM ' + FSelectedTable;

  // Conditions WHERE
  if FWhereConditions.Count > 0 then
  begin
    // Pour l'exemple simplifié, on suppose une condition simple
    SQL := SQL + LineEnding + 'WHERE ';
    // Ici il faudrait parser les panels créés dynamiquement
    SQL := SQL + '1=1'; // Placeholder
  end;

  // ORDER BY
  if FOrderByColumns.Count > 0 then
  begin
    SQL := SQL + LineEnding + 'ORDER BY ';
    for i := 0 to FOrderByColumns.Count - 1 do
    begin
      if i > 0 then
        SQL := SQL + ', ';
      SQL := SQL + FOrderByColumns[i];
    end;
  end;

  SQL := SQL + ';';

  Result := SQL;
end;

function TfrmSQLQueryBuilder.BuildInsertQuery: string;  
var
  SQL: string;
  i: Integer;
  ColumnsStr, ValuesStr: string;
begin
  if FSelectedColumns.Count = 0 then
  begin
    Result := '-- Sélectionnez des colonnes';
    Exit;
  end;

  // Colonnes
  ColumnsStr := '';
  ValuesStr := '';

  for i := 0 to FSelectedColumns.Count - 1 do
  begin
    if i > 0 then
    begin
      ColumnsStr := ColumnsStr + ', ';
      ValuesStr := ValuesStr + ', ';
    end;
    ColumnsStr := ColumnsStr + FSelectedColumns[i];
    ValuesStr := ValuesStr + '?'; // Paramètre
  end;

  SQL := 'INSERT INTO ' + FSelectedTable + LineEnding;
  SQL := SQL + '  (' + ColumnsStr + ')' + LineEnding;
  SQL := SQL + 'VALUES' + LineEnding;
  SQL := SQL + '  (' + ValuesStr + ');';

  Result := SQL;
end;

function TfrmSQLQueryBuilder.BuildUpdateQuery: string;  
var
  SQL: string;
  i: Integer;
  SetStr: string;
begin
  if FSelectedColumns.Count = 0 then
  begin
    Result := '-- Sélectionnez des colonnes';
    Exit;
  end;

  // SET
  SetStr := '';
  for i := 0 to FSelectedColumns.Count - 1 do
  begin
    if i > 0 then
      SetStr := SetStr + ',' + LineEnding + '  ';
    SetStr := SetStr + FSelectedColumns[i] + ' = ?';
  end;

  SQL := 'UPDATE ' + FSelectedTable + LineEnding;
  SQL := SQL + 'SET ' + SetStr + LineEnding;
  SQL := SQL + 'WHERE id = ?;';

  Result := SQL;
end;

function TfrmSQLQueryBuilder.BuildDeleteQuery: string;  
var
  SQL: string;
begin
  SQL := 'DELETE FROM ' + FSelectedTable + LineEnding;
  SQL := SQL + 'WHERE id = ?;';

  Result := SQL;
end;

procedure TfrmSQLQueryBuilder.LoadTables(ATables: TStrings);  
begin
  FTablesListBox.Items.Assign(ATables);
end;

procedure TfrmSQLQueryBuilder.LoadColumns(ATable: string; AColumns: TStrings);  
begin
  FSelectedTable := ATable;
  FColumnsListBox.Items.Assign(AColumns);
end;

function TfrmSQLQueryBuilder.GetGeneratedSQL: string;  
begin
  Result := FSQLMemo.Lines.Text;
end;

end.
```

## Éditeur de diagrammes et workflows

Un éditeur pour créer des diagrammes de flux (flowcharts) ou des workflows :

```pascal
unit DiagramEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Menus, Buttons, Types;

type
  TNodeType = (ntStart, ntEnd, ntProcess, ntDecision, ntData);
  TConnectionType = (ctStraight, ctElbow);

  TDiagramNode = class
    NodeType: TNodeType;
    X, Y: Integer;
    Width, Height: Integer;
    Caption: string;
    Selected: Boolean;
    Color: TColor;
  end;

  TDiagramConnection = class
    FromNode: TDiagramNode;
    ToNode: TDiagramNode;
    ConnectionType: TConnectionType;
    Label_: string;
    Selected: Boolean;
  end;

  TfrmDiagramEditor = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FCanvas: TPanel;
    FToolbox: TPanel;
    FNodes: TList;
    FConnections: TList;
    FSelectedNode: TDiagramNode;
    FDragging: Boolean;
    FDragStart: TPoint;
    FConnecting: Boolean;
    FConnectionStart: TDiagramNode;

    procedure SetupInterface;
    procedure CreateToolbox;

    procedure OnCanvasMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OnCanvasMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure OnCanvasMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OnCanvasPaint(Sender: TObject);

    procedure OnAddNodeClick(Sender: TObject);
    procedure OnConnectClick(Sender: TObject);

    procedure DrawNode(ACanvas: TCanvas; ANode: TDiagramNode);
    procedure DrawConnection(ACanvas: TCanvas; AConnection: TDiagramConnection);
    function FindNodeAt(X, Y: Integer): TDiagramNode;
    procedure AddNode(AType: TNodeType; X, Y: Integer);
    procedure AddConnection(AFrom, ATo: TDiagramNode);

  public
    procedure SaveDiagram(const AFileName: string);
    procedure LoadDiagram(const AFileName: string);
    procedure ExportToPNG(const AFileName: string);
  end;

var
  frmDiagramEditor: TfrmDiagramEditor;

implementation

{$R *.lfm}

uses
  IniFiles, IntfGraphics, FPImage, FPWritePNG;

procedure TfrmDiagramEditor.FormCreate(Sender: TObject);  
begin
  Caption := 'Éditeur de Diagrammes';
  Width := 1000;
  Height := 700;
  Position := poScreenCenter;

  FNodes := TList.Create;
  FConnections := TList.Create;
  FDragging := False;
  FConnecting := False;
  FSelectedNode := nil;

  SetupInterface;
end;

procedure TfrmDiagramEditor.FormDestroy(Sender: TObject);  
var
  i: Integer;
begin
  for i := 0 to FNodes.Count - 1 do
    TDiagramNode(FNodes[i]).Free;
  FNodes.Free;

  for i := 0 to FConnections.Count - 1 do
    TDiagramConnection(FConnections[i]).Free;
  FConnections.Free;
end;

procedure TfrmDiagramEditor.SetupInterface;  
begin
  CreateToolbox;

  // Canvas de dessin
  FCanvas := TPanel.Create(Self);
  FCanvas.Parent := Self;
  FCanvas.Align := alClient;
  FCanvas.Color := clWhite;
  FCanvas.BevelOuter := bvNone;
  FCanvas.OnMouseDown := @OnCanvasMouseDown;
  FCanvas.OnMouseMove := @OnCanvasMouseMove;
  FCanvas.OnMouseUp := @OnCanvasMouseUp;
  FCanvas.OnPaint := @OnCanvasPaint;
  FCanvas.DoubleBuffered := True;
end;

procedure TfrmDiagramEditor.CreateToolbox;  
var
  Panel: TPanel;
  Btn: TSpeedButton;
  Y: Integer;
begin
  Panel := TPanel.Create(Self);
  Panel.Parent := Self;
  Panel.Align := alLeft;
  Panel.Width := 150;
  Panel.Caption := '';

  FToolbox := Panel;

  with TLabel.Create(Panel) do
  begin
    Parent := Panel;
    Align := alTop;
    Caption := ' Formes';
    Font.Style := [fsBold];
    Height := 25;
  end;

  Y := 30;

  // Bouton Début
  Btn := TSpeedButton.Create(Panel);
  Btn.Parent := Panel;
  Btn.Caption := 'Début (Ovale)';
  Btn.Left := 10;
  Btn.Top := Y;
  Btn.Width := 130;
  Btn.Tag := Ord(ntStart);
  Btn.OnClick := @OnAddNodeClick;
  Inc(Y, 30);

  // Bouton Fin
  Btn := TSpeedButton.Create(Panel);
  Btn.Parent := Panel;
  Btn.Caption := 'Fin (Ovale)';
  Btn.Left := 10;
  Btn.Top := Y;
  Btn.Width := 130;
  Btn.Tag := Ord(ntEnd);
  Btn.OnClick := @OnAddNodeClick;
  Inc(Y, 30);

  // Bouton Processus
  Btn := TSpeedButton.Create(Panel);
  Btn.Parent := Panel;
  Btn.Caption := 'Processus';
  Btn.Left := 10;
  Btn.Top := Y;
  Btn.Width := 130;
  Btn.Tag := Ord(ntProcess);
  Btn.OnClick := @OnAddNodeClick;
  Inc(Y, 30);

  // Bouton Décision
  Btn := TSpeedButton.Create(Panel);
  Btn.Parent := Panel;
  Btn.Caption := 'Décision';
  Btn.Left := 10;
  Btn.Top := Y;
  Btn.Width := 130;
  Btn.Tag := Ord(ntDecision);
  Btn.OnClick := @OnAddNodeClick;
  Inc(Y, 30);

  // Bouton Données
  Btn := TSpeedButton.Create(Panel);
  Btn.Parent := Panel;
  Btn.Caption := 'Données';
  Btn.Left := 10;
  Btn.Top := Y;
  Btn.Width := 130;
  Btn.Tag := Ord(ntData);
  Btn.OnClick := @OnAddNodeClick;
  Inc(Y, 40);

  // Séparateur
  with TLabel.Create(Panel) do
  begin
    Parent := Panel;
    Left := 10;
    Top := Y;
    Width := 130;
    Height := 2;
    Color := clGray;
  end;
  Inc(Y, 10);

  // Bouton Connexion
  Btn := TSpeedButton.Create(Panel);
  Btn.Parent := Panel;
  Btn.Caption := 'Connecter';
  Btn.Left := 10;
  Btn.Top := Y;
  Btn.Width := 130;
  Btn.OnClick := @OnConnectClick;
end;

procedure TfrmDiagramEditor.OnAddNodeClick(Sender: TObject);  
var
  NodeType: TNodeType;
begin
  NodeType := TNodeType((Sender as TSpeedButton).Tag);
  FCanvas.Cursor := crCross;
  FCanvas.Tag := Ord(NodeType);
end;

procedure TfrmDiagramEditor.OnConnectClick(Sender: TObject);  
begin
  FConnecting := True;
  FCanvas.Cursor := crHandPoint;
  ShowMessage('Cliquez sur le premier nœud, puis sur le second pour créer une connexion.');
end;

procedure TfrmDiagramEditor.OnCanvasMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  NodeType: TNodeType;
  HitNode: TDiagramNode;
  i: Integer;
begin
  if Button = mbLeft then
  begin
    if FCanvas.Tag > 0 then
    begin
      // Créer un nouveau nœud
      NodeType := TNodeType(FCanvas.Tag);
      AddNode(NodeType, X - 60, Y - 30);
      FCanvas.Tag := 0;
      FCanvas.Cursor := crDefault;
    end
    else if FConnecting then
    begin
      // Mode connexion
      HitNode := FindNodeAt(X, Y);
      if Assigned(HitNode) then
      begin
        if not Assigned(FConnectionStart) then
        begin
          FConnectionStart := HitNode;
          HitNode.Selected := True;
          FCanvas.Invalidate;
        end
        else
        begin
          AddConnection(FConnectionStart, HitNode);
          FConnectionStart.Selected := False;
          FConnectionStart := nil;
          FConnecting := False;
          FCanvas.Cursor := crDefault;
          FCanvas.Invalidate;
        end;
      end;
    end
    else
    begin
      // Sélectionner et déplacer un nœud
      HitNode := FindNodeAt(X, Y);
      if Assigned(HitNode) then
      begin
        FSelectedNode := HitNode;
        FDragging := True;
        FDragStart := Point(X, Y);

        // Désélectionner tous les autres
        for i := 0 to FNodes.Count - 1 do
          TDiagramNode(FNodes[i]).Selected := False;

        HitNode.Selected := True;
        FCanvas.Invalidate;
      end;
    end;
  end;
end;

procedure TfrmDiagramEditor.OnCanvasMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  DeltaX, DeltaY: Integer;
begin
  if FDragging and Assigned(FSelectedNode) then
  begin
    DeltaX := X - FDragStart.X;
    DeltaY := Y - FDragStart.Y;

    FSelectedNode.X := FSelectedNode.X + DeltaX;
    FSelectedNode.Y := FSelectedNode.Y + DeltaY;

    FDragStart := Point(X, Y);
    FCanvas.Invalidate;
  end;
end;

procedure TfrmDiagramEditor.OnCanvasMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FDragging := False;
end;

procedure TfrmDiagramEditor.OnCanvasPaint(Sender: TObject);  
var
  i: Integer;
begin
  // Dessiner d'abord les connexions
  for i := 0 to FConnections.Count - 1 do
    DrawConnection(FCanvas.Canvas, TDiagramConnection(FConnections[i]));

  // Puis les nœuds
  for i := 0 to FNodes.Count - 1 do
    DrawNode(FCanvas.Canvas, TDiagramNode(FNodes[i]));
end;

procedure DrawArrow(ACanvas: TCanvas; X, Y, FromX, FromY: Integer); forward;

procedure TfrmDiagramEditor.DrawNode(ACanvas: TCanvas; ANode: TDiagramNode);  
var
  R: TRect;
  Points: array[0..3] of TPoint;
  TextHeight: Integer;
begin
  R := Rect(ANode.X, ANode.Y, ANode.X + ANode.Width, ANode.Y + ANode.Height);

  // Configuration du pinceau et du stylo
  ACanvas.Brush.Color := ANode.Color;
  ACanvas.Pen.Color := clBlack;
  ACanvas.Pen.Width := 2;

  if ANode.Selected then
    ACanvas.Pen.Color := clBlue;

  // Dessiner selon le type
  case ANode.NodeType of
    ntStart, ntEnd:
      begin
        // Ovale
        ACanvas.Ellipse(R);
      end;

    ntProcess:
      begin
        // Rectangle
        ACanvas.Rectangle(R);
      end;

    ntDecision:
      begin
        // Losange
        Points[0] := Point((R.Left + R.Right) div 2, R.Top);
        Points[1] := Point(R.Right, (R.Top + R.Bottom) div 2);
        Points[2] := Point((R.Left + R.Right) div 2, R.Bottom);
        Points[3] := Point(R.Left, (R.Top + R.Bottom) div 2);
        ACanvas.Polygon(Points);
      end;

    ntData:
      begin
        // Parallélogramme
        Points[0] := Point(R.Left + 20, R.Top);
        Points[1] := Point(R.Right, R.Top);
        Points[2] := Point(R.Right - 20, R.Bottom);
        Points[3] := Point(R.Left, R.Bottom);
        ACanvas.Polygon(Points);
      end;
  end;

  // Dessiner le texte
  ACanvas.Font.Color := clBlack;
  ACanvas.Font.Style := [];
  ACanvas.Brush.Style := bsClear;

  TextHeight := ACanvas.TextHeight(ANode.Caption);
  ACanvas.TextOut(
    R.Left + (R.Right - R.Left - ACanvas.TextWidth(ANode.Caption)) div 2,
    R.Top + (R.Bottom - R.Top - TextHeight) div 2,
    ANode.Caption
  );

  ACanvas.Brush.Style := bsSolid;
end;

procedure TfrmDiagramEditor.DrawConnection(ACanvas: TCanvas;
  AConnection: TDiagramConnection);
var
  X1, Y1, X2, Y2: Integer;
begin
  if not Assigned(AConnection.FromNode) or not Assigned(AConnection.ToNode) then
    Exit;

  // Points de départ et d'arrivée (centre des nœuds)
  X1 := AConnection.FromNode.X + AConnection.FromNode.Width div 2;
  Y1 := AConnection.FromNode.Y + AConnection.FromNode.Height div 2;
  X2 := AConnection.ToNode.X + AConnection.ToNode.Width div 2;
  Y2 := AConnection.ToNode.Y + AConnection.ToNode.Height div 2;

  ACanvas.Pen.Color := clBlack;
  ACanvas.Pen.Width := 2;

  if AConnection.Selected then
    ACanvas.Pen.Color := clBlue;

  // Dessiner la ligne
  ACanvas.MoveTo(X1, Y1);

  case AConnection.ConnectionType of
    ctStraight:
      begin
        ACanvas.LineTo(X2, Y2);
      end;

    ctElbow:
      begin
        // Ligne en coude
        ACanvas.LineTo(X1, Y2);
        ACanvas.LineTo(X2, Y2);
      end;
  end;

  // Dessiner la flèche
  DrawArrow(ACanvas, X2, Y2, X1, Y1);
end;

procedure DrawArrow(ACanvas: TCanvas; X, Y, FromX, FromY: Integer);  
var
  Angle: Double;
  ArrowSize: Integer;
  P1, P2: TPoint;
begin
  ArrowSize := 10;

  // Calculer l'angle
  Angle := ArcTan2(Y - FromY, X - FromX);

  // Points de la flèche
  P1.X := X - Round(ArrowSize * Cos(Angle - Pi / 6));
  P1.Y := Y - Round(ArrowSize * Sin(Angle - Pi / 6));

  P2.X := X - Round(ArrowSize * Cos(Angle + Pi / 6));
  P2.Y := Y - Round(ArrowSize * Sin(Angle + Pi / 6));

  // Dessiner la flèche
  ACanvas.MoveTo(X, Y);
  ACanvas.LineTo(P1.X, P1.Y);
  ACanvas.MoveTo(X, Y);
  ACanvas.LineTo(P2.X, P2.Y);
end;

function TfrmDiagramEditor.FindNodeAt(X, Y: Integer): TDiagramNode;  
var
  i: Integer;
  Node: TDiagramNode;
  R: TRect;
begin
  Result := nil;

  for i := FNodes.Count - 1 downto 0 do
  begin
    Node := TDiagramNode(FNodes[i]);
    R := Rect(Node.X, Node.Y, Node.X + Node.Width, Node.Y + Node.Height);

    if PtInRect(R, Point(X, Y)) then
    begin
      Result := Node;
      Exit;
    end;
  end;
end;

procedure TfrmDiagramEditor.AddNode(AType: TNodeType; X, Y: Integer);  
var
  Node: TDiagramNode;
begin
  Node := TDiagramNode.Create;
  Node.NodeType := AType;
  Node.X := X;
  Node.Y := Y;
  Node.Width := 120;
  Node.Height := 60;
  Node.Selected := False;
  Node.Color := clWhite;

  case AType of
    ntStart:
      begin
        Node.Caption := 'Début';
        Node.Color := RGB(200, 255, 200);
      end;
    ntEnd:
      begin
        Node.Caption := 'Fin';
        Node.Color := RGB(255, 200, 200);
      end;
    ntProcess:
      begin
        Node.Caption := 'Processus';
        Node.Color := RGB(200, 220, 255);
      end;
    ntDecision:
      begin
        Node.Caption := 'Décision ?';
        Node.Color := RGB(255, 255, 200);
      end;
    ntData:
      begin
        Node.Caption := 'Données';
        Node.Color := RGB(220, 220, 220);
      end;
  end;

  FNodes.Add(Node);
  FCanvas.Invalidate;
end;

procedure TfrmDiagramEditor.AddConnection(AFrom, ATo: TDiagramNode);  
var
  Connection: TDiagramConnection;
begin
  Connection := TDiagramConnection.Create;
  Connection.FromNode := AFrom;
  Connection.ToNode := ATo;
  Connection.ConnectionType := ctStraight;
  Connection.Label_ := '';
  Connection.Selected := False;

  FConnections.Add(Connection);
  FCanvas.Invalidate;
end;

procedure TfrmDiagramEditor.SaveDiagram(const AFileName: string);  
var
  Ini: TIniFile;
  i: Integer;
  Node: TDiagramNode;
  Conn: TDiagramConnection;
  Section: string;
begin
  Ini := TIniFile.Create(AFileName);
  try
    // Sauvegarder les nœuds
    Ini.WriteInteger('Diagram', 'NodeCount', FNodes.Count);

    for i := 0 to FNodes.Count - 1 do
    begin
      Node := TDiagramNode(FNodes[i]);
      Section := 'Node' + IntToStr(i);

      Ini.WriteInteger(Section, 'Type', Ord(Node.NodeType));
      Ini.WriteInteger(Section, 'X', Node.X);
      Ini.WriteInteger(Section, 'Y', Node.Y);
      Ini.WriteInteger(Section, 'Width', Node.Width);
      Ini.WriteInteger(Section, 'Height', Node.Height);
      Ini.WriteString(Section, 'Caption', Node.Caption);
      Ini.WriteInteger(Section, 'Color', Node.Color);
    end;

    // Sauvegarder les connexions
    Ini.WriteInteger('Diagram', 'ConnectionCount', FConnections.Count);

    for i := 0 to FConnections.Count - 1 do
    begin
      Conn := TDiagramConnection(FConnections[i]);
      Section := 'Connection' + IntToStr(i);

      Ini.WriteInteger(Section, 'From', FNodes.IndexOf(Conn.FromNode));
      Ini.WriteInteger(Section, 'To', FNodes.IndexOf(Conn.ToNode));
      Ini.WriteInteger(Section, 'Type', Ord(Conn.ConnectionType));
      Ini.WriteString(Section, 'Label', Conn.Label_);
    end;
  finally
    Ini.Free;
  end;
end;

procedure TfrmDiagramEditor.LoadDiagram(const AFileName: string);  
var
  Ini: TIniFile;
  i, NodeCount, ConnCount: Integer;
  Node: TDiagramNode;
  Conn: TDiagramConnection;
  Section: string;
  FromIndex, ToIndex: Integer;
begin
  if not FileExists(AFileName) then Exit;

  // Effacer le diagramme actuel
  for i := 0 to FNodes.Count - 1 do
    TDiagramNode(FNodes[i]).Free;
  FNodes.Clear;

  for i := 0 to FConnections.Count - 1 do
    TDiagramConnection(FConnections[i]).Free;
  FConnections.Clear;

  Ini := TIniFile.Create(AFileName);
  try
    // Charger les nœuds
    NodeCount := Ini.ReadInteger('Diagram', 'NodeCount', 0);

    for i := 0 to NodeCount - 1 do
    begin
      Section := 'Node' + IntToStr(i);

      Node := TDiagramNode.Create;
      Node.NodeType := TNodeType(Ini.ReadInteger(Section, 'Type', 0));
      Node.X := Ini.ReadInteger(Section, 'X', 0);
      Node.Y := Ini.ReadInteger(Section, 'Y', 0);
      Node.Width := Ini.ReadInteger(Section, 'Width', 120);
      Node.Height := Ini.ReadInteger(Section, 'Height', 60);
      Node.Caption := Ini.ReadString(Section, 'Caption', '');
      Node.Color := TColor(Ini.ReadInteger(Section, 'Color', clWhite));
      Node.Selected := False;

      FNodes.Add(Node);
    end;

    // Charger les connexions
    ConnCount := Ini.ReadInteger('Diagram', 'ConnectionCount', 0);

    for i := 0 to ConnCount - 1 do
    begin
      Section := 'Connection' + IntToStr(i);

      FromIndex := Ini.ReadInteger(Section, 'From', -1);
      ToIndex := Ini.ReadInteger(Section, 'To', -1);

      if (FromIndex >= 0) and (FromIndex < FNodes.Count) and
         (ToIndex >= 0) and (ToIndex < FNodes.Count) then
      begin
        Conn := TDiagramConnection.Create;
        Conn.FromNode := TDiagramNode(FNodes[FromIndex]);
        Conn.ToNode := TDiagramNode(FNodes[ToIndex]);
        Conn.ConnectionType := TConnectionType(Ini.ReadInteger(Section, 'Type', 0));
        Conn.Label_ := Ini.ReadString(Section, 'Label', '');
        Conn.Selected := False;

        FConnections.Add(Conn);
      end;
    end;
  finally
    Ini.Free;
  end;

  FCanvas.Invalidate;
end;

procedure TfrmDiagramEditor.ExportToPNG(const AFileName: string);  
var
  Bitmap: TBitmap;
  IntfImg: TLazIntfImage;
  Writer: TFPWriterPNG;
  i: Integer;
begin
  // Créer un bitmap de la taille du canvas
  Bitmap := TBitmap.Create;
  try
    Bitmap.SetSize(FCanvas.Width, FCanvas.Height);

    // Dessiner sur le bitmap
    Bitmap.Canvas.Brush.Color := clWhite;
    Bitmap.Canvas.FillRect(0, 0, Bitmap.Width, Bitmap.Height);

    // Dessiner les connexions
    for i := 0 to FConnections.Count - 1 do
      DrawConnection(Bitmap.Canvas, TDiagramConnection(FConnections[i]));

    // Dessiner les nœuds
    for i := 0 to FNodes.Count - 1 do
      DrawNode(Bitmap.Canvas, TDiagramNode(FNodes[i]));

    // Convertir en PNG
    IntfImg := TLazIntfImage.Create(0, 0);
    Writer := TFPWriterPNG.Create;
    try
      IntfImg.LoadFromBitmap(Bitmap.Handle, Bitmap.MaskHandle);
      IntfImg.SaveToFile(AFileName, Writer);
    finally
      Writer.Free;
      IntfImg.Free;
    end;
  finally
    Bitmap.Free;
  end;
end;

end.
```

## Éditeur de couleurs et palettes

Un éditeur pour créer et gérer des palettes de couleurs :

```pascal
unit ColorPaletteEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Spin, ColorBox, Grids;

type
  TColorPalette = class
    Name: string;
    Colors: array of TColor;
  end;

  TfrmColorPaletteEditor = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    FPalettes: TList;
    FCurrentPalette: TColorPalette;

    FPaletteListBox: TListBox;
    FColorGrid: TDrawGrid;
    FColorPreview: TPanel;
    FColorPicker: TColorBox;

    FRedSpin: TSpinEdit;
    FGreenSpin: TSpinEdit;
    FBlueSpin: TSpinEdit;
    FHexEdit: TEdit;

    procedure SetupInterface;
    procedure CreatePaletteList;
    procedure CreateColorGrid;
    procedure CreateColorControls;

    procedure OnNewPalette(Sender: TObject);
    procedure OnAddColorClick(Sender: TObject);
    procedure OnPaletteSelect(Sender: TObject);
    procedure OnColorGridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure OnColorGridSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure OnColorPickerChange(Sender: TObject);
    procedure OnRGBChange(Sender: TObject);
    procedure OnHexChange(Sender: TObject);

    procedure UpdateColorPreview(AColor: TColor);
    procedure UpdateRGBSpins(AColor: TColor);
    procedure UpdateHexEdit(AColor: TColor);

  public
    procedure AddPalette(const AName: string);
    procedure AddColorToPalette(AColor: TColor);
    procedure SavePalettes(const AFileName: string);
    procedure LoadPalettes(const AFileName: string);
  end;

var
  frmColorPaletteEditor: TfrmColorPaletteEditor;

implementation

{$R *.lfm}

uses
  IniFiles, LCLType;

procedure TfrmColorPaletteEditor.FormCreate(Sender: TObject);  
begin
  Caption := 'Éditeur de Palettes de Couleurs';
  Width := 900;
  Height := 600;
  Position := poScreenCenter;

  FPalettes := TList.Create;
  FCurrentPalette := nil;

  SetupInterface;

  // Créer une palette par défaut
  AddPalette('Palette par défaut');
end;

procedure TfrmColorPaletteEditor.SetupInterface;  
begin
  CreatePaletteList;
  CreateColorGrid;
  CreateColorControls;
end;

procedure TfrmColorPaletteEditor.CreatePaletteList;  
var
  Panel: TPanel;
  Label1: TLabel;
  BtnAdd, BtnDelete: TButton;
begin
  // Panel gauche pour la liste des palettes
  Panel := TPanel.Create(Self);
  Panel.Parent := Self;
  Panel.Align := alLeft;
  Panel.Width := 200;
  Panel.Caption := '';

  // Titre
  Label1 := TLabel.Create(Panel);
  Label1.Parent := Panel;
  Label1.Align := alTop;
  Label1.Caption := ' Palettes';
  Label1.Font.Style := [fsBold];
  Label1.Height := 25;

  // ListBox
  FPaletteListBox := TListBox.Create(Panel);
  FPaletteListBox.Parent := Panel;
  FPaletteListBox.Align := alClient;
  FPaletteListBox.OnClick := @OnPaletteSelect;

  // Boutons
  Panel := TPanel.Create(Panel);
  Panel.Parent := FPaletteListBox.Parent;
  Panel.Align := alBottom;
  Panel.Height := 40;
  Panel.BevelOuter := bvNone;

  BtnAdd := TButton.Create(Panel);
  BtnAdd.Parent := Panel;
  BtnAdd.Caption := 'Nouvelle';
  BtnAdd.Left := 10;
  BtnAdd.Top := 5;
  BtnAdd.Width := 85;
  BtnAdd.OnClick := @OnNewPalette;

  BtnDelete := TButton.Create(Panel);
  BtnDelete.Parent := Panel;
  BtnDelete.Caption := 'Supprimer';
  BtnDelete.Left := 100;
  BtnDelete.Top := 5;
  BtnDelete.Width := 85;
end;

procedure TfrmColorPaletteEditor.CreateColorGrid;  
var
  Panel: TPanel;
  Label1: TLabel;
begin
  // Panel central pour la grille de couleurs
  Panel := TPanel.Create(Self);
  Panel.Parent := Self;
  Panel.Align := alClient;
  Panel.Caption := '';

  // Titre
  Label1 := TLabel.Create(Panel);
  Label1.Parent := Panel;
  Label1.Align := alTop;
  Label1.Caption := ' Couleurs de la palette';
  Label1.Font.Style := [fsBold];
  Label1.Height := 25;

  // Grille
  FColorGrid := TDrawGrid.Create(Panel);
  FColorGrid.Parent := Panel;
  FColorGrid.Align := alClient;
  FColorGrid.ColCount := 8;
  FColorGrid.RowCount := 8;
  FColorGrid.DefaultColWidth := 60;
  FColorGrid.DefaultRowHeight := 60;
  FColorGrid.FixedCols := 0;
  FColorGrid.FixedRows := 0;
  FColorGrid.Options := [goDrawFocusSelected];
  FColorGrid.OnDrawCell := @OnColorGridDrawCell;
  FColorGrid.OnSelectCell := @OnColorGridSelectCell;
end;

procedure TfrmColorPaletteEditor.CreateColorControls;  
var
  Panel, SubPanel: TPanel;
  Label1: TLabel;
begin
  // Panel droit pour les contrôles de couleur
  Panel := TPanel.Create(Self);
  Panel.Parent := Self;
  Panel.Align := alRight;
  Panel.Width := 250;
  Panel.Caption := '';

  // Titre
  Label1 := TLabel.Create(Panel);
  Label1.Parent := Panel;
  Label1.Align := alTop;
  Label1.Caption := ' Éditeur de couleur';
  Label1.Font.Style := [fsBold];
  Label1.Height := 25;

  // Prévisualisation
  FColorPreview := TPanel.Create(Panel);
  FColorPreview.Parent := Panel;
  FColorPreview.Left := 10;
  FColorPreview.Top := 35;
  FColorPreview.Width := 230;
  FColorPreview.Height := 100;
  FColorPreview.Color := clWhite;
  FColorPreview.BevelOuter := bvLowered;

  // ColorBox
  Label1 := TLabel.Create(Panel);
  Label1.Parent := Panel;
  Label1.Left := 10;
  Label1.Top := 145;
  Label1.Caption := 'Sélecteur de couleur:';

  FColorPicker := TColorBox.Create(Panel);
  FColorPicker.Parent := Panel;
  FColorPicker.Left := 10;
  FColorPicker.Top := 165;
  FColorPicker.Width := 230;
  FColorPicker.Style := [cbStandardColors, cbExtendedColors, cbCustomColor];
  FColorPicker.OnChange := @OnColorPickerChange;

  // RGB
  Label1 := TLabel.Create(Panel);
  Label1.Parent := Panel;
  Label1.Left := 10;
  Label1.Top := 200;
  Label1.Caption := 'Valeurs RGB:';

  // Rouge
  SubPanel := TPanel.Create(Panel);
  SubPanel.Parent := Panel;
  SubPanel.Left := 10;
  SubPanel.Top := 220;
  SubPanel.Width := 230;
  SubPanel.Height := 30;
  SubPanel.BevelOuter := bvNone;

  Label1 := TLabel.Create(SubPanel);
  Label1.Parent := SubPanel;
  Label1.Left := 0;
  Label1.Top := 8;
  Label1.Caption := 'R:';

  FRedSpin := TSpinEdit.Create(SubPanel);
  FRedSpin.Parent := SubPanel;
  FRedSpin.Left := 20;
  FRedSpin.Top := 3;
  FRedSpin.Width := 80;
  FRedSpin.MinValue := 0;
  FRedSpin.MaxValue := 255;
  FRedSpin.OnChange := @OnRGBChange;

  // Vert
  SubPanel := TPanel.Create(Panel);
  SubPanel.Parent := Panel;
  SubPanel.Left := 10;
  SubPanel.Top := 255;
  SubPanel.Width := 230;
  SubPanel.Height := 30;
  SubPanel.BevelOuter := bvNone;

  Label1 := TLabel.Create(SubPanel);
  Label1.Parent := SubPanel;
  Label1.Left := 0;
  Label1.Top := 8;
  Label1.Caption := 'G:';

  FGreenSpin := TSpinEdit.Create(SubPanel);
  FGreenSpin.Parent := SubPanel;
  FGreenSpin.Left := 20;
  FGreenSpin.Top := 3;
  FGreenSpin.Width := 80;
  FGreenSpin.MinValue := 0;
  FGreenSpin.MaxValue := 255;
  FGreenSpin.OnChange := @OnRGBChange;

  // Bleu
  SubPanel := TPanel.Create(Panel);
  SubPanel.Parent := Panel;
  SubPanel.Left := 10;
  SubPanel.Top := 290;
  SubPanel.Width := 230;
  SubPanel.Height := 30;
  SubPanel.BevelOuter := bvNone;

  Label1 := TLabel.Create(SubPanel);
  Label1.Parent := SubPanel;
  Label1.Left := 0;
  Label1.Top := 8;
  Label1.Caption := 'B:';

  FBlueSpin := TSpinEdit.Create(SubPanel);
  FBlueSpin.Parent := SubPanel;
  FBlueSpin.Left := 20;
  FBlueSpin.Top := 3;
  FBlueSpin.Width := 80;
  FBlueSpin.MinValue := 0;
  FBlueSpin.MaxValue := 255;
  FBlueSpin.OnChange := @OnRGBChange;

  // Hexadécimal
  Label1 := TLabel.Create(Panel);
  Label1.Parent := Panel;
  Label1.Left := 10;
  Label1.Top := 330;
  Label1.Caption := 'Hexadécimal:';

  FHexEdit := TEdit.Create(Panel);
  FHexEdit.Parent := Panel;
  FHexEdit.Left := 10;
  FHexEdit.Top := 350;
  FHexEdit.Width := 120;
  FHexEdit.Text := '#FFFFFF';
  FHexEdit.OnChange := @OnHexChange;

  // Bouton Ajouter
  with TButton.Create(Panel) do
  begin
    Parent := Panel;
    Left := 10;
    Top := 390;
    Width := 230;
    Height := 35;
    Caption := 'Ajouter à la palette';
    OnClick := @OnAddColorClick;
  end;
end;

procedure TfrmColorPaletteEditor.OnNewPalette(Sender: TObject);  
var
  PaletteName: string;
begin
  PaletteName := InputBox('Nouvelle palette', 'Nom de la palette:', '');
  if PaletteName <> '' then
    AddPalette(PaletteName);
end;

procedure TfrmColorPaletteEditor.OnAddColorClick(Sender: TObject);  
begin
  if Assigned(FCurrentPalette) then
    AddColorToPalette(FColorPreview.Color);
end;

procedure TfrmColorPaletteEditor.OnPaletteSelect(Sender: TObject);  
var
  Index: Integer;
begin
  Index := FPaletteListBox.ItemIndex;
  if (Index >= 0) and (Index < FPalettes.Count) then
  begin
    FCurrentPalette := TColorPalette(FPalettes[Index]);
    FColorGrid.Invalidate;
  end;
end;

procedure TfrmColorPaletteEditor.OnColorGridDrawCell(Sender: TObject;
  aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  Index: Integer;
  ColorValue: TColor;
begin
  Index := aRow * FColorGrid.ColCount + aCol;

  if Assigned(FCurrentPalette) and (Index < Length(FCurrentPalette.Colors)) then
  begin
    ColorValue := FCurrentPalette.Colors[Index];

    // Dessiner la couleur
    FColorGrid.Canvas.Brush.Color := ColorValue;
    FColorGrid.Canvas.FillRect(aRect);

    // Bordure
    FColorGrid.Canvas.Brush.Style := bsClear;
    FColorGrid.Canvas.Pen.Color := clBlack;
    FColorGrid.Canvas.Rectangle(aRect);
  end
  else
  begin
    // Case vide
    FColorGrid.Canvas.Brush.Color := clWhite;
    FColorGrid.Canvas.FillRect(aRect);
  end;
end;

procedure TfrmColorPaletteEditor.OnColorGridSelectCell(Sender: TObject;
  aCol, aRow: Integer; var CanSelect: Boolean);
var
  Index: Integer;
  ColorValue: TColor;
begin
  Index := aRow * FColorGrid.ColCount + aCol;
  CanSelect := True;

  if Assigned(FCurrentPalette) and (Index < Length(FCurrentPalette.Colors)) then
  begin
    ColorValue := FCurrentPalette.Colors[Index];
    UpdateColorPreview(ColorValue);
    UpdateRGBSpins(ColorValue);
    UpdateHexEdit(ColorValue);
    FColorPicker.Selected := ColorValue;
  end;
end;

procedure TfrmColorPaletteEditor.OnColorPickerChange(Sender: TObject);  
begin
  UpdateColorPreview(FColorPicker.Selected);
  UpdateRGBSpins(FColorPicker.Selected);
  UpdateHexEdit(FColorPicker.Selected);
end;

procedure TfrmColorPaletteEditor.OnRGBChange(Sender: TObject);  
var
  NewColor: TColor;
begin
  NewColor := RGB(FRedSpin.Value, FGreenSpin.Value, FBlueSpin.Value);
  UpdateColorPreview(NewColor);
  UpdateHexEdit(NewColor);
  FColorPicker.Selected := NewColor;
end;

procedure TfrmColorPaletteEditor.OnHexChange(Sender: TObject);  
var
  HexStr: string;
  NewColor: TColor;
begin
  HexStr := FHexEdit.Text;
  if (Length(HexStr) = 7) and (HexStr[1] = '#') then
  begin
    try
      NewColor := StrToInt('$' + Copy(HexStr, 2, 6));
      UpdateColorPreview(NewColor);
      UpdateRGBSpins(NewColor);
      FColorPicker.Selected := NewColor;
    except
      // Erreur de conversion, ignorer
    end;
  end;
end;

procedure TfrmColorPaletteEditor.UpdateColorPreview(AColor: TColor);  
begin
  FColorPreview.Color := AColor;

  // Afficher le nom de la couleur si elle est standard
  FColorPreview.Caption := ColorToString(AColor);
end;

procedure TfrmColorPaletteEditor.UpdateRGBSpins(AColor: TColor);  
var
  R, G, B: Byte;
begin
  RedGreenBlue(AColor, R, G, B);

  FRedSpin.OnChange := nil;
  FGreenSpin.OnChange := nil;
  FBlueSpin.OnChange := nil;

  FRedSpin.Value := R;
  FGreenSpin.Value := G;
  FBlueSpin.Value := B;

  FRedSpin.OnChange := @OnRGBChange;
  FGreenSpin.OnChange := @OnRGBChange;
  FBlueSpin.OnChange := @OnRGBChange;
end;

procedure TfrmColorPaletteEditor.UpdateHexEdit(AColor: TColor);  
var
  R, G, B: Byte;
begin
  RedGreenBlue(AColor, R, G, B);

  FHexEdit.OnChange := nil;
  FHexEdit.Text := Format('#%.2X%.2X%.2X', [R, G, B]);
  FHexEdit.OnChange := @OnHexChange;
end;

procedure TfrmColorPaletteEditor.AddPalette(const AName: string);  
var
  Palette: TColorPalette;
begin
  Palette := TColorPalette.Create;
  Palette.Name := AName;
  SetLength(Palette.Colors, 0);

  FPalettes.Add(Palette);
  FPaletteListBox.Items.Add(AName);
  FPaletteListBox.ItemIndex := FPaletteListBox.Items.Count - 1;

  FCurrentPalette := Palette;
end;

procedure TfrmColorPaletteEditor.AddColorToPalette(AColor: TColor);  
var
  Index: Integer;
begin
  if not Assigned(FCurrentPalette) then Exit;

  Index := Length(FCurrentPalette.Colors);
  SetLength(FCurrentPalette.Colors, Index + 1);
  FCurrentPalette.Colors[Index] := AColor;

  FColorGrid.Invalidate;
end;

procedure TfrmColorPaletteEditor.SavePalettes(const AFileName: string);  
var
  Ini: TIniFile;
  i, j: Integer;
  Palette: TColorPalette;
  Section: string;
begin
  Ini := TIniFile.Create(AFileName);
  try
    Ini.WriteInteger('Palettes', 'Count', FPalettes.Count);

    for i := 0 to FPalettes.Count - 1 do
    begin
      Palette := TColorPalette(FPalettes[i]);
      Section := 'Palette' + IntToStr(i);

      Ini.WriteString(Section, 'Name', Palette.Name);
      Ini.WriteInteger(Section, 'ColorCount', Length(Palette.Colors));

      for j := 0 to High(Palette.Colors) do
      begin
        Ini.WriteInteger(Section, 'Color' + IntToStr(j), Palette.Colors[j]);
      end;
    end;
  finally
    Ini.Free;
  end;
end;

procedure TfrmColorPaletteEditor.LoadPalettes(const AFileName: string);  
begin
  // À implémenter : charger depuis le fichier
end;

end.
```

## Bonnes pratiques pour les éditeurs intégrés

### 1. Performance et réactivité

```pascal
// ✅ BON : Utiliser le double buffering pour éviter le scintillement
procedure TCustomEditor.SetupCanvas;  
begin
  FCanvas.DoubleBuffered := True;
  FCanvas.OnPaint := @OnCanvasPaint;
end;

// ✅ BON : Invalider seulement la zone modifiée
procedure TCustomEditor.UpdateRegion(ARect: TRect);  
begin
  InvalidateRect(FCanvas.Handle, @ARect, False);
end;

// ❌ MAUVAIS : Tout redessiner à chaque modification
procedure TCustomEditor.UpdateBad;  
begin
  FCanvas.Invalidate; // Redessine tout !
end;
```

### 2. Annuler/Refaire (Undo/Redo)

```pascal
type
  TEditorCommand = class
    procedure Execute; virtual; abstract;
    procedure Undo; virtual; abstract;
  end;

  TUndoManager = class
  private
    FUndoStack: TList;
    FRedoStack: TList;
    FMaxUndoLevels: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure ExecuteCommand(ACommand: TEditorCommand);
    procedure Undo;
    procedure Redo;
    function CanUndo: Boolean;
    function CanRedo: Boolean;
  end;

constructor TUndoManager.Create;  
begin
  inherited Create;
  FUndoStack := TList.Create;
  FRedoStack := TList.Create;
  FMaxUndoLevels := 50;
end;

destructor TUndoManager.Destroy;  
begin
  // Libérer les commandes
  while FUndoStack.Count > 0 do
  begin
    TEditorCommand(FUndoStack[0]).Free;
    FUndoStack.Delete(0);
  end;
  FUndoStack.Free;

  while FRedoStack.Count > 0 do
  begin
    TEditorCommand(FRedoStack[0]).Free;
    FRedoStack.Delete(0);
  end;
  FRedoStack.Free;

  inherited Destroy;
end;

procedure TUndoManager.ExecuteCommand(ACommand: TEditorCommand);  
begin
  // Exécuter la commande
  ACommand.Execute;

  // Ajouter à la pile d'annulation
  FUndoStack.Add(ACommand);

  // Limiter la taille
  while FUndoStack.Count > FMaxUndoLevels do
  begin
    TEditorCommand(FUndoStack[0]).Free;
    FUndoStack.Delete(0);
  end;

  // Vider la pile de rétablissement
  while FRedoStack.Count > 0 do
  begin
    TEditorCommand(FRedoStack[0]).Free;
    FRedoStack.Delete(0);
  end;
end;

procedure TUndoManager.Undo;  
var
  Command: TEditorCommand;
begin
  if CanUndo then
  begin
    Command := TEditorCommand(FUndoStack[FUndoStack.Count - 1]);
    Command.Undo;

    FUndoStack.Delete(FUndoStack.Count - 1);
    FRedoStack.Add(Command);
  end;
end;

procedure TUndoManager.Redo;  
var
  Command: TEditorCommand;
begin
  if CanRedo then
  begin
    Command := TEditorCommand(FRedoStack[FRedoStack.Count - 1]);
    Command.Execute;

    FRedoStack.Delete(FRedoStack.Count - 1);
    FUndoStack.Add(Command);
  end;
end;

function TUndoManager.CanUndo: Boolean;  
begin
  Result := FUndoStack.Count > 0;
end;

function TUndoManager.CanRedo: Boolean;  
begin
  Result := FRedoStack.Count > 0;
end;
```

### 3. Sauvegarde automatique

```pascal
type
  TAutoSaveManager = class
  private
    FTimer: TTimer;
    FEditor: TObject;
    FAutoSaveInterval: Integer; // en millisecondes
    FAutoSavePath: string;
    FModified: Boolean;

    procedure OnTimer(Sender: TObject);
  public
    constructor Create(AEditor: TObject);
    destructor Destroy; override;

    procedure Start;
    procedure Stop;
    procedure MarkModified;

    property AutoSaveInterval: Integer read FAutoSaveInterval write FAutoSaveInterval;
  end;

constructor TAutoSaveManager.Create(AEditor: TObject);  
begin
  inherited Create;
  FEditor := AEditor;
  FAutoSaveInterval := 60000; // 1 minute par défaut
  FModified := False;

  {$IFDEF WINDOWS}
  FAutoSavePath := GetEnvironmentVariable('TEMP') + '\autosave\';
  {$ELSE}
  FAutoSavePath := GetEnvironmentVariable('HOME') + '/.autosave/';
  {$ENDIF}

  ForceDirectories(FAutoSavePath);

  FTimer := TTimer.Create(nil);
  FTimer.Interval := FAutoSaveInterval;
  FTimer.OnTimer := @OnTimer;
  FTimer.Enabled := False;
end;

destructor TAutoSaveManager.Destroy;  
begin
  FTimer.Free;
  inherited Destroy;
end;

procedure TAutoSaveManager.OnTimer(Sender: TObject);  
var
  FileName: string;
begin
  if FModified then
  begin
    FileName := FAutoSavePath + 'autosave_' +
                FormatDateTime('yyyymmdd_hhnnss', Now) + '.tmp';

    // Appeler la méthode de sauvegarde de l'éditeur
    // (À adapter selon votre éditeur)
    // TCustomEditor(FEditor).SaveToFile(FileName);

    FModified := False;
  end;
end;

procedure TAutoSaveManager.Start;  
begin
  FTimer.Enabled := True;
end;

procedure TAutoSaveManager.Stop;  
begin
  FTimer.Enabled := False;
end;

procedure TAutoSaveManager.MarkModified;  
begin
  FModified := True;
end;
```

### 4. Validation en temps réel

```pascal
type
  TValidationRule = class
    function Validate(const AValue: string): Boolean; virtual; abstract;
    function GetErrorMessage: string; virtual; abstract;
  end;

  TEmailValidationRule = class(TValidationRule)
    function Validate(const AValue: string): Boolean; override;
    function GetErrorMessage: string; override;
  end;

  TValidator = class
  private
    FRules: TList;
    FErrorMessages: TStringList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddRule(ARule: TValidationRule);
    function ValidateValue(const AValue: string): Boolean;
    function GetAllErrors: string;
  end;

function TEmailValidationRule.Validate(const AValue: string): Boolean;  
var
  AtPos: Integer;
begin
  // Validation simple d'email
  AtPos := Pos('@', AValue);
  Result := (AtPos > 1) and (AtPos < Length(AValue)) and
            (Pos('.', AValue) > AtPos);
end;

function TEmailValidationRule.GetErrorMessage: string;  
begin
  Result := 'Adresse email invalide';
end;

constructor TValidator.Create;  
begin
  inherited Create;
  FRules := TList.Create;
  FErrorMessages := TStringList.Create;
end;

destructor TValidator.Destroy;  
var
  i: Integer;
begin
  for i := 0 to FRules.Count - 1 do
    TValidationRule(FRules[i]).Free;
  FRules.Free;
  FErrorMessages.Free;
  inherited Destroy;
end;

procedure TValidator.AddRule(ARule: TValidationRule);  
begin
  FRules.Add(ARule);
end;

function TValidator.ValidateValue(const AValue: string): Boolean;  
var
  i: Integer;
  Rule: TValidationRule;
begin
  Result := True;
  FErrorMessages.Clear;

  for i := 0 to FRules.Count - 1 do
  begin
    Rule := TValidationRule(FRules[i]);
    if not Rule.Validate(AValue) then
    begin
      Result := False;
      FErrorMessages.Add(Rule.GetErrorMessage);
    end;
  end;
end;

function TValidator.GetAllErrors: string;  
begin
  Result := FErrorMessages.Text;
end;
```

### 5. Export multi-formats

```pascal
type
  TExportFormat = (efPNG, efJPEG, efBMP, efSVG, efPDF);

  TEditorExporter = class
  public
    class procedure ExportToFile(AEditor: TObject; const AFileName: string;
      AFormat: TExportFormat);
    class procedure ExportToPNG(ACanvas: TCanvas; const AFileName: string);
    class procedure ExportToSVG(AEditor: TObject; const AFileName: string);
    class procedure ExportToPDF(AEditor: TObject; const AFileName: string);
  end;

class procedure TEditorExporter.ExportToFile(AEditor: TObject;
  const AFileName: string; AFormat: TExportFormat);
begin
  case AFormat of
    efPNG: ; // Déjà implémenté dans les exemples précédents
    efSVG: ExportToSVG(AEditor, AFileName);
    efPDF: ExportToPDF(AEditor, AFileName);
  end;
end;

class procedure TEditorExporter.ExportToSVG(AEditor: TObject;
  const AFileName: string);
var
  SVG: TStringList;
begin
  SVG := TStringList.Create;
  try
    SVG.Add('<?xml version="1.0" encoding="UTF-8"?>');
    SVG.Add('<svg xmlns="http://www.w3.org/2000/svg" width="800" height="600">');

    // Ajouter les éléments SVG
    // (À adapter selon le contenu de votre éditeur)

    SVG.Add('</svg>');
    SVG.SaveToFile(AFileName);
  finally
    SVG.Free;
  end;
end;

class procedure TEditorExporter.ExportToPDF(AEditor: TObject;
  const AFileName: string);
begin
  // Nécessite une bibliothèque PDF comme fpPDF
  // Implémentation simplifiée
  ShowMessage('Export PDF : nécessite la bibliothèque fpPDF');
end;
```

## Éditeur de templates/modèles

Un éditeur permettant de créer des modèles réutilisables :

```pascal
unit TemplateEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, Menus;

type
  TTemplate = class
    Name: string;
    Content: string;
    Variables: TStringList;
    Category: string;
  end;

  TfrmTemplateEditor = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    FTemplates: TList;
    FCurrentTemplate: TTemplate;

    FTemplateList: TListView;
    FEditor: TMemo;
    FVariablesList: TListBox;
    FPreview: TMemo;

    procedure SetupInterface;
    procedure CreateTemplateList;
    procedure CreateEditor;
    procedure CreateVariablePanel;
    procedure CreatePreview;

    procedure OnNewTemplate(Sender: TObject);
    procedure OnTemplateSelect(Sender: TObject);
    procedure OnEditorChange(Sender: TObject);
    procedure OnInsertVariable(Sender: TObject);
    procedure OnGeneratePreview(Sender: TObject);

    procedure ParseVariables;
    function ReplaceVariables(const ATemplate: string;
                             AValues: TStringList): string;

  public
    procedure AddTemplate(const AName, ACategory: string);
    procedure SaveTemplates(const AFileName: string);
    procedure LoadTemplates(const AFileName: string);
  end;

var
  frmTemplateEditor: TfrmTemplateEditor;

implementation

{$R *.lfm}

uses
  IniFiles, StrUtils;

procedure TfrmTemplateEditor.FormCreate(Sender: TObject);  
begin
  Caption := 'Éditeur de Modèles';
  Width := 1100;
  Height := 700;
  Position := poScreenCenter;

  FTemplates := TList.Create;
  FCurrentTemplate := nil;

  SetupInterface;
end;

procedure TfrmTemplateEditor.SetupInterface;  
begin
  CreateTemplateList;
  CreateEditor;
  CreateVariablePanel;
  CreatePreview;
end;

procedure TfrmTemplateEditor.CreateTemplateList;  
var
  Panel: TPanel;
  Label1: TLabel;
  BtnNew: TButton;
  Column: TListColumn;
begin
  Panel := TPanel.Create(Self);
  Panel.Parent := Self;
  Panel.Align := alLeft;
  Panel.Width := 250;
  Panel.Caption := '';

  Label1 := TLabel.Create(Panel);
  Label1.Parent := Panel;
  Label1.Align := alTop;
  Label1.Caption := ' Modèles';
  Label1.Font.Style := [fsBold];
  Label1.Height := 25;

  FTemplateList := TListView.Create(Panel);
  FTemplateList.Parent := Panel;
  FTemplateList.Align := alClient;
  FTemplateList.ViewStyle := vsReport;
  FTemplateList.RowSelect := True;
  FTemplateList.OnSelectItem := @OnTemplateSelect;

  Column := FTemplateList.Columns.Add;
  Column.Caption := 'Nom';
  Column.Width := 150;

  Column := FTemplateList.Columns.Add;
  Column.Caption := 'Catégorie';
  Column.Width := 90;

  // Bouton nouveau
  Panel := TPanel.Create(Panel);
  Panel.Parent := FTemplateList.Parent;
  Panel.Align := alBottom;
  Panel.Height := 40;
  Panel.BevelOuter := bvNone;

  BtnNew := TButton.Create(Panel);
  BtnNew.Parent := Panel;
  BtnNew.Caption := 'Nouveau modèle';
  BtnNew.Left := 10;
  BtnNew.Top := 5;
  BtnNew.Width := 230;
  BtnNew.OnClick := @OnNewTemplate;
end;

procedure TfrmTemplateEditor.CreateEditor;  
var
  Panel: TPanel;
  Label1: TLabel;
begin
  Panel := TPanel.Create(Self);
  Panel.Parent := Self;
  Panel.Align := alClient;
  Panel.Caption := '';

  Label1 := TLabel.Create(Panel);
  Label1.Parent := Panel;
  Label1.Align := alTop;
  Label1.Caption := ' Éditeur de modèle (utilisez {{variable}} pour les variables)';
  Label1.Font.Style := [fsBold];
  Label1.Height := 25;

  FEditor := TMemo.Create(Panel);
  FEditor.Parent := Panel;
  FEditor.Align := alClient;
  FEditor.ScrollBars := ssBoth;
  FEditor.Font.Name := 'Courier New';
  FEditor.Font.Size := 10;
  FEditor.OnChange := @OnEditorChange;
  FEditor.WordWrap := False;
end;

procedure TfrmTemplateEditor.CreateVariablePanel;  
var
  Panel: TPanel;
  Label1: TLabel;
  BtnInsert: TButton;
begin
  Panel := TPanel.Create(Self);
  Panel.Parent := Self;
  Panel.Align := alRight;
  Panel.Width := 200;
  Panel.Caption := '';

  Label1 := TLabel.Create(Panel);
  Label1.Parent := Panel;
  Label1.Align := alTop;
  Label1.Caption := ' Variables détectées';
  Label1.Font.Style := [fsBold];
  Label1.Height := 25;

  FVariablesList := TListBox.Create(Panel);
  FVariablesList.Parent := Panel;
  FVariablesList.Align := alClient;

  Panel := TPanel.Create(Panel);
  Panel.Parent := FVariablesList.Parent;
  Panel.Align := alBottom;
  Panel.Height := 40;
  Panel.BevelOuter := bvNone;

  BtnInsert := TButton.Create(Panel);
  BtnInsert.Parent := Panel;
  BtnInsert.Caption := 'Insérer variable';
  BtnInsert.Left := 10;
  BtnInsert.Top := 5;
  BtnInsert.Width := 180;
  BtnInsert.OnClick := @OnInsertVariable;
end;

procedure TfrmTemplateEditor.CreatePreview;  
var
  Splitter: TSplitter;
  Panel: TPanel;
  Label1: TLabel;
  BtnGenerate: TButton;
begin
  Splitter := TSplitter.Create(Self);
  Splitter.Parent := Self;
  Splitter.Align := alBottom;
  Splitter.Height := 5;

  Panel := TPanel.Create(Self);
  Panel.Parent := Self;
  Panel.Align := alBottom;
  Panel.Height := 200;
  Panel.Caption := '';

  Label1 := TLabel.Create(Panel);
  Label1.Parent := Panel;
  Label1.Align := alTop;
  Label1.Caption := ' Aperçu';
  Label1.Font.Style := [fsBold];
  Label1.Height := 25;

  FPreview := TMemo.Create(Panel);
  FPreview.Parent := Panel;
  FPreview.Align := alClient;
  FPreview.ScrollBars := ssBoth;
  FPreview.ReadOnly := True;
  FPreview.Color := RGB(250, 250, 250);

  Panel := TPanel.Create(Panel);
  Panel.Parent := FPreview.Parent;
  Panel.Align := alBottom;
  Panel.Height := 40;
  Panel.BevelOuter := bvNone;

  BtnGenerate := TButton.Create(Panel);
  BtnGenerate.Parent := Panel;
  BtnGenerate.Caption := 'Générer aperçu';
  BtnGenerate.Left := 10;
  BtnGenerate.Top := 5;
  BtnGenerate.Width := 150;
  BtnGenerate.OnClick := @OnGeneratePreview;
end;

procedure TfrmTemplateEditor.OnNewTemplate(Sender: TObject);  
var
  TemplateName: string;
begin
  TemplateName := InputBox('Nouveau modèle', 'Nom du modèle:', '');
  if TemplateName <> '' then
    AddTemplate(TemplateName, 'Général');
end;

procedure TfrmTemplateEditor.OnTemplateSelect(Sender: TObject);  
var
  Item: TListItem;
begin
  Item := FTemplateList.Selected;
  if Assigned(Item) and (Item.Index < FTemplates.Count) then
  begin
    FCurrentTemplate := TTemplate(FTemplates[Item.Index]);
    FEditor.Lines.Text := FCurrentTemplate.Content;
    ParseVariables;
  end;
end;

procedure TfrmTemplateEditor.OnEditorChange(Sender: TObject);  
begin
  if Assigned(FCurrentTemplate) then
  begin
    FCurrentTemplate.Content := FEditor.Lines.Text;
    ParseVariables;
  end;
end;

procedure TfrmTemplateEditor.OnInsertVariable(Sender: TObject);  
var
  VarName: string;
begin
  VarName := InputBox('Nouvelle variable', 'Nom de la variable:', '');
  if VarName <> '' then
  begin
    FEditor.SelText := '{{' + VarName + '}}';
    ParseVariables;
  end;
end;

procedure TfrmTemplateEditor.OnGeneratePreview(Sender: TObject);  
var
  Values: TStringList;
  i: Integer;
  VarName, VarValue: string;
  Result: string;
begin
  if not Assigned(FCurrentTemplate) then Exit;

  Values := TStringList.Create;
  try
    // Demander les valeurs pour chaque variable
    for i := 0 to FVariablesList.Items.Count - 1 do
    begin
      VarName := FVariablesList.Items[i];
      VarValue := InputBox('Valeur de la variable',
                          'Entrez la valeur pour ' + VarName + ':', '');
      Values.Values[VarName] := VarValue;
    end;

    // Générer l'aperçu
    Result := ReplaceVariables(FCurrentTemplate.Content, Values);
    FPreview.Lines.Text := Result;
  finally
    Values.Free;
  end;
end;

procedure TfrmTemplateEditor.ParseVariables;  
var
  Content: string;
  StartPos, EndPos: Integer;
  VarName: string;
begin
  if not Assigned(FCurrentTemplate) then Exit;

  FVariablesList.Items.Clear;
  Content := FCurrentTemplate.Content;

  // Chercher toutes les variables {{xxx}}
  StartPos := 1;
  while StartPos <= Length(Content) do
  begin
    StartPos := PosEx('{{', Content, StartPos);
    if StartPos = 0 then Break;

    EndPos := PosEx('}}', Content, StartPos + 2);
    if EndPos = 0 then Break;

    VarName := Copy(Content, StartPos + 2, EndPos - StartPos - 2);
    VarName := Trim(VarName);

    if (VarName <> '') and (FVariablesList.Items.IndexOf(VarName) = -1) then
      FVariablesList.Items.Add(VarName);

    StartPos := EndPos + 2;
  end;
end;

function TfrmTemplateEditor.ReplaceVariables(const ATemplate: string;
  AValues: TStringList): string;
var
  i: Integer;
  VarName, VarValue: string;
begin
  Result := ATemplate;

  for i := 0 to AValues.Count - 1 do
  begin
    VarName := AValues.Names[i];
    VarValue := AValues.ValueFromIndex[i];

    Result := StringReplace(Result, '{{' + VarName + '}}', VarValue,
                           [rfReplaceAll, rfIgnoreCase]);
  end;
end;

procedure TfrmTemplateEditor.AddTemplate(const AName, ACategory: string);  
var
  Template: TTemplate;
  Item: TListItem;
begin
  Template := TTemplate.Create;
  Template.Name := AName;
  Template.Category := ACategory;
  Template.Content := '';
  Template.Variables := TStringList.Create;

  FTemplates.Add(Template);

  Item := FTemplateList.Items.Add;
  Item.Caption := AName;
  Item.SubItems.Add(ACategory);

  FCurrentTemplate := Template;
  FTemplateList.Selected := Item;
end;

procedure TfrmTemplateEditor.SaveTemplates(const AFileName: string);  
var
  Ini: TIniFile;
  i: Integer;
  Template: TTemplate;
  Section: string;
begin
  Ini := TIniFile.Create(AFileName);
  try
    Ini.WriteInteger('Templates', 'Count', FTemplates.Count);

    for i := 0 to FTemplates.Count - 1 do
    begin
      Template := TTemplate(FTemplates[i]);
      Section := 'Template' + IntToStr(i);

      Ini.WriteString(Section, 'Name', Template.Name);
      Ini.WriteString(Section, 'Category', Template.Category);
      Ini.WriteString(Section, 'Content', Template.Content);
    end;
  finally
    Ini.Free;
  end;
end;

procedure TfrmTemplateEditor.LoadTemplates(const AFileName: string);  
begin
  // À implémenter
end;

end.
```

## Compatibilité multi-plateforme

### Gestion des différences Windows/Ubuntu

```pascal
unit EditorPlatform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics;

type
  TPlatformHelper = class
  public
    class function GetDefaultFont: string;
    class function GetDefaultFontSize: Integer;
    class function GetConfigPath: string;
    class function GetTempPath: string;
    class procedure ConfigureEditor(AEditor: TObject);
  end;

implementation

uses
  Forms;

class function TPlatformHelper.GetDefaultFont: string;  
begin
  {$IFDEF WINDOWS}
  Result := 'Consolas';
  {$ENDIF}

  {$IFDEF UNIX}
  Result := 'Monospace';
  {$ENDIF}
end;

class function TPlatformHelper.GetDefaultFontSize: Integer;  
begin
  {$IFDEF WINDOWS}
  Result := 10;
  {$ENDIF}

  {$IFDEF UNIX}
  Result := 11;
  {$ENDIF}
end;

class function TPlatformHelper.GetConfigPath: string;  
begin
  {$IFDEF WINDOWS}
  Result := GetEnvironmentVariable('APPDATA') + PathDelim + 'MyEditor' + PathDelim;
  {$ENDIF}

  {$IFDEF UNIX}
  Result := GetEnvironmentVariable('HOME') + PathDelim + '.config' +
            PathDelim + 'myeditor' + PathDelim;
  {$ENDIF}

  ForceDirectories(Result);
end;

class function TPlatformHelper.GetTempPath: string;  
begin
  {$IFDEF WINDOWS}
  Result := GetEnvironmentVariable('TEMP') + PathDelim;
  {$ENDIF}

  {$IFDEF UNIX}
  Result := '/tmp/';
  {$ENDIF}
end;

class procedure TPlatformHelper.ConfigureEditor(AEditor: TObject);  
begin
  // Configuration spécifique à la plateforme
  {$IFDEF WINDOWS}
  // Configuration Windows
  {$ENDIF}

  {$IFDEF UNIX}
  // Configuration Linux/Unix
  {$ENDIF}
end;

end.
```

## Résumé et conseils finaux

### Checklist pour créer un éditeur intégré

#### Fonctionnalités de base
- [ ] Édition de texte de base
- [ ] Sauvegarde/Chargement
- [ ] Copier/Coller/Couper
- [ ] Annuler/Refaire (Undo/Redo)
- [ ] Rechercher/Remplacer

#### Interface utilisateur
- [ ] Barre d'outils intuitive
- [ ] Raccourcis clavier
- [ ] Menus contextuels
- [ ] Glisser-déposer (si applicable)
- [ ] Indicateurs visuels clairs

#### Performance
- [ ] Double buffering pour éviter le scintillement
- [ ] Invalidation partielle (pas tout redessiner)
- [ ] Chargement paresseux si nécessaire
- [ ] Optimisation des grandes quantités de données

#### Robustesse
- [ ] Validation des données
- [ ] Gestion des erreurs
- [ ] Sauvegarde automatique
- [ ] Récupération après crash
- [ ] Tests avec données invalides

#### Expérience utilisateur
- [ ] Feedback visuel immédiat
- [ ] Messages d'erreur clairs
- [ ] Aide contextuelle
- [ ] Personnalisation (thèmes, raccourcis)
- [ ] Export multi-formats

#### Multi-plateforme
- [ ] Testé sur Windows
- [ ] Testé sur Ubuntu/Linux
- [ ] Chemins portables
- [ ] Polices appropriées par OS
- [ ] Raccourcis adaptés

### Pièges courants à éviter

```pascal
// ❌ MAUVAIS : Redessiner tout à chaque modification
procedure OnChange;  
begin
  Invalidate; // Redessine TOUT !
end;

// ✅ BON : Invalider seulement la zone modifiée
procedure OnChange;  
begin
  InvalidateRect(Handle, @ModifiedRect, False);
end;

// ❌ MAUVAIS : Pas de limite sur l'historique
procedure AddToUndo(ACommand: TCommand);  
begin
  FUndoList.Add(ACommand); // Fuite mémoire potentielle !
end;

// ✅ BON : Limiter la taille de l'historique
procedure AddToUndo(ACommand: TCommand);  
begin
  FUndoList.Add(ACommand);
  while FUndoList.Count > FMaxUndoLevels do
  begin
    TCommand(FUndoList[0]).Free;
    FUndoList.Delete(0);
  end;
end;

// ❌ MAUVAIS : Chemins codés en dur
ConfigFile := 'C:\Users\Admin\config.ini';

// ✅ BON : Chemins portables
ConfigFile := TPlatformHelper.GetConfigPath + 'config.ini';
```

### Ressources et bibliothèques utiles

#### Pour les éditeurs de texte
- **SynEdit** : Éditeur avec coloration syntaxique (inclus dans Lazarus)
- **ATSynEdit** : Alternative moderne à SynEdit
- **TRichMemo** : Éditeur de texte riche multi-plateforme

#### Pour les designers visuels
- **BGRABitmap** : Graphiques avancés avec anti-aliasing
- **AggPas** : Bibliothèque de rendu vectoriel
- **fpvectorial** : Lecture/écriture de formats vectoriels

#### Pour les diagrammes
- **GraphViz** : Génération automatique de graphes
- **PlantUML** : Diagrammes UML à partir de texte

### Exemple minimal d'éditeur fonctionnel

```pascal
unit SimpleEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Menus, Dialogs;

type
  TfrmSimpleEditor = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    FEditor: TMemo;
    FMainMenu: TMainMenu;
    FModified: Boolean;
    FFileName: string;

    procedure CreateMenu;
    procedure OnNew(Sender: TObject);
    procedure OnOpen(Sender: TObject);
    procedure OnSave(Sender: TObject);
    procedure OnEditorChange(Sender: TObject);
  end;

var
  frmSimpleEditor: TfrmSimpleEditor;

implementation

{$R *.lfm}

procedure TfrmSimpleEditor.FormCreate(Sender: TObject);  
begin
  Caption := 'Éditeur Simple';
  Width := 800;
  Height := 600;

  FEditor := TMemo.Create(Self);
  FEditor.Parent := Self;
  FEditor.Align := alClient;
  FEditor.ScrollBars := ssBoth;
  FEditor.OnChange := @OnEditorChange;

  FModified := False;
  FFileName := '';

  CreateMenu;
end;

procedure TfrmSimpleEditor.CreateMenu;  
var
  mnuFile: TMenuItem;
  Item: TMenuItem;
begin
  FMainMenu := TMainMenu.Create(Self);
  Self.Menu := FMainMenu;

  mnuFile := TMenuItem.Create(FMainMenu);
  mnuFile.Caption := 'Fichier';
  FMainMenu.Items.Add(mnuFile);

  Item := TMenuItem.Create(mnuFile);
  Item.Caption := 'Nouveau';
  Item.ShortCut := ShortCut(VK_N, [ssCtrl]);
  Item.OnClick := @OnNew;
  mnuFile.Add(Item);

  Item := TMenuItem.Create(mnuFile);
  Item.Caption := 'Ouvrir...';
  Item.ShortCut := ShortCut(VK_O, [ssCtrl]);
  Item.OnClick := @OnOpen;
  mnuFile.Add(Item);

  Item := TMenuItem.Create(mnuFile);
  Item.Caption := 'Enregistrer';
  Item.ShortCut := ShortCut(VK_S, [ssCtrl]);
  Item.OnClick := @OnSave;
  mnuFile.Add(Item);
end;

procedure TfrmSimpleEditor.OnNew(Sender: TObject);  
begin
  if FModified then
  begin
    if MessageDlg('Sauvegarder ?', 'Voulez-vous sauvegarder les modifications ?',
       mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      OnSave(nil);
  end;

  FEditor.Clear;
  FFileName := '';
  FModified := False;
  Caption := 'Éditeur Simple - Sans titre';
end;

procedure TfrmSimpleEditor.OnOpen(Sender: TObject);  
var
  OpenDialog: TOpenDialog;
begin
  OpenDialog := TOpenDialog.Create(nil);
  try
    if OpenDialog.Execute then
    begin
      FEditor.Lines.LoadFromFile(OpenDialog.FileName);
      FFileName := OpenDialog.FileName;
      FModified := False;
      Caption := 'Éditeur Simple - ' + ExtractFileName(FFileName);
    end;
  finally
    OpenDialog.Free;
  end;
end;

procedure TfrmSimpleEditor.OnSave(Sender: TObject);  
var
  SaveDialog: TSaveDialog;
begin
  if FFileName = '' then
  begin
    SaveDialog := TSaveDialog.Create(nil);
    try
      if SaveDialog.Execute then
        FFileName := SaveDialog.FileName
      else
        Exit;
    finally
      SaveDialog.Free;
    end;
  end;

  FEditor.Lines.SaveToFile(FFileName);
  FModified := False;
  Caption := 'Éditeur Simple - ' + ExtractFileName(FFileName);
end;

procedure TfrmSimpleEditor.OnEditorChange(Sender: TObject);  
begin
  FModified := True;
  if FFileName <> '' then
    Caption := 'Éditeur Simple - ' + ExtractFileName(FFileName) + ' *'
  else
    Caption := 'Éditeur Simple - Sans titre *';
end;

end.
```

## Conclusion

Les éditeurs et designers intégrés sont des composants puissants qui enrichissent considérablement vos applications. En suivant les bonnes pratiques et en utilisant les bibliothèques appropriées, vous pouvez créer des outils professionnels et performants.

### Points clés à retenir

1. **Commencez simple** : Un éditeur de base fonctionnel vaut mieux qu'un éditeur complexe bugué
2. **Performance** : Optimisez le rendu et les mises à jour
3. **UX** : L'expérience utilisateur est primordiale
4. **Testez** : Sur les deux plateformes (Windows/Ubuntu)
5. **Documentez** : Le code d'éditeur peut devenir complexe

### Prochaines étapes

Pour aller plus loin :
- Explorez SynEdit en profondeur
- Étudiez le code source de Lazarus IDE
- Implémentez l'auto-complétion intelligente

# 12.7 Éditeurs et designers intégrés - Exemples avancés et conclusion

## Éditeur d'expressions régulières (Regex)

Un outil pratique pour créer et tester des expressions régulières en temps réel :

```pascal
unit RegexEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, RegExpr;

type
  TfrmRegexEditor = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FPatternEdit: TEdit;
    FTestTextMemo: TMemo;
    FResultsMemo: TMemo;
    FMatchesListView: TListView;
    FOptionsPanel: TPanel;

    FCaseSensitiveCheck: TCheckBox;
    FMultiLineCheck: TCheckBox;

    FRegex: TRegExpr;

    procedure SetupInterface;
    procedure CreatePatternPanel;
    procedure CreateTestTextPanel;
    procedure CreateResultsPanel;
    procedure CreateOptionsPanel;
    procedure CreateQuickReferencePanel;

    procedure OnPatternChange(Sender: TObject);
    procedure OnTestTextChange(Sender: TObject);
    procedure OnOptionsChange(Sender: TObject);
    procedure OnTestRegex(Sender: TObject);

    procedure HighlightMatches;
    procedure DisplayMatches;
    procedure ShowError(const AMessage: string);

  end;

var
  frmRegexEditor: TfrmRegexEditor;

implementation

{$R *.lfm}

procedure TfrmRegexEditor.FormCreate(Sender: TObject);  
begin
  Caption := 'Testeur d''Expressions Régulières';
  Width := 1000;
  Height := 700;
  Position := poScreenCenter;

  FRegex := TRegExpr.Create;

  SetupInterface;
end;

procedure TfrmRegexEditor.FormDestroy(Sender: TObject);  
begin
  FRegex.Free;
end;

procedure TfrmRegexEditor.SetupInterface;  
begin
  CreatePatternPanel;
  CreateOptionsPanel;
  CreateTestTextPanel;
  CreateResultsPanel;
  CreateQuickReferencePanel;
end;

procedure TfrmRegexEditor.CreatePatternPanel;  
var
  Panel: TPanel;
  Label1: TLabel;
  BtnTest: TButton;
begin
  Panel := TPanel.Create(Self);
  Panel.Parent := Self;
  Panel.Align := alTop;
  Panel.Height := 80;
  Panel.BevelOuter := bvNone;

  Label1 := TLabel.Create(Panel);
  Label1.Parent := Panel;
  Label1.Left := 10;
  Label1.Top := 10;
  Label1.Caption := 'Expression régulière :';
  Label1.Font.Style := [fsBold];

  FPatternEdit := TEdit.Create(Panel);
  FPatternEdit.Parent := Panel;
  FPatternEdit.Left := 10;
  FPatternEdit.Top := 30;
  FPatternEdit.Width := 700;
  FPatternEdit.Font.Name := 'Courier New';
  FPatternEdit.Font.Size := 10;
  FPatternEdit.OnChange := @OnPatternChange;

  BtnTest := TButton.Create(Panel);
  BtnTest.Parent := Panel;
  BtnTest.Left := 720;
  BtnTest.Top := 28;
  BtnTest.Width := 100;
  BtnTest.Height := 27;
  BtnTest.Caption := 'Tester';
  BtnTest.OnClick := @OnTestRegex;
  BtnTest.Default := True;

  Label1 := TLabel.Create(Panel);
  Label1.Parent := Panel;
  Label1.Left := 10;
  Label1.Top := 60;
  Label1.Caption := 'Appuyez sur Entrée ou cliquez sur "Tester" pour voir les résultats';
  Label1.Font.Color := clGray;
end;

procedure TfrmRegexEditor.CreateOptionsPanel;  
var
  Panel: TPanel;
begin
  Panel := TPanel.Create(Self);
  Panel.Parent := Self;
  Panel.Align := alTop;
  Panel.Height := 40;
  Panel.BevelOuter := bvNone;

  FOptionsPanel := Panel;

  FCaseSensitiveCheck := TCheckBox.Create(Panel);
  FCaseSensitiveCheck.Parent := Panel;
  FCaseSensitiveCheck.Left := 10;
  FCaseSensitiveCheck.Top := 10;
  FCaseSensitiveCheck.Caption := 'Sensible à la casse';
  FCaseSensitiveCheck.OnClick := @OnOptionsChange;

  FMultiLineCheck := TCheckBox.Create(Panel);
  FMultiLineCheck.Parent := Panel;
  FMultiLineCheck.Left := 200;
  FMultiLineCheck.Top := 10;
  FMultiLineCheck.Caption := 'Multi-lignes (^ et $ pour chaque ligne)';
  FMultiLineCheck.OnClick := @OnOptionsChange;
end;

procedure TfrmRegexEditor.CreateTestTextPanel;  
var
  Panel: TPanel;
  Label1: TLabel;
begin
  Panel := TPanel.Create(Self);
  Panel.Parent := Self;
  Panel.Align := alClient;
  Panel.Caption := '';
  Panel.BevelOuter := bvNone;

  Label1 := TLabel.Create(Panel);
  Label1.Parent := Panel;
  Label1.Align := alTop;
  Label1.Caption := ' Texte de test :';
  Label1.Font.Style := [fsBold];
  Label1.Height := 25;

  FTestTextMemo := TMemo.Create(Panel);
  FTestTextMemo.Parent := Panel;
  FTestTextMemo.Align := alClient;
  FTestTextMemo.ScrollBars := ssBoth;
  FTestTextMemo.Font.Name := 'Courier New';
  FTestTextMemo.Font.Size := 10;
  FTestTextMemo.OnChange := @OnTestTextChange;
  FTestTextMemo.Lines.Text :=
    'Exemple de texte pour tester les regex.' + LineEnding +
    'Email: test@exemple.com' + LineEnding +
    'Téléphone: 06 12 34 56 78' + LineEnding +
    'Date: 03/10/2025';
end;

procedure TfrmRegexEditor.CreateResultsPanel;  
var
  Splitter: TSplitter;
  Panel, SubPanel: TPanel;
  Label1: TLabel;
  PageControl: TPageControl;
  TabSheet: TTabSheet;
  Column: TListColumn;
begin
  Splitter := TSplitter.Create(Self);
  Splitter.Parent := Self;
  Splitter.Align := alBottom;
  Splitter.Height := 5;

  Panel := TPanel.Create(Self);
  Panel.Parent := Self;
  Panel.Align := alBottom;
  Panel.Height := 250;
  Panel.Caption := '';
  Panel.BevelOuter := bvNone;

  Label1 := TLabel.Create(Panel);
  Label1.Parent := Panel;
  Label1.Align := alTop;
  Label1.Caption := ' Résultats :';
  Label1.Font.Style := [fsBold];
  Label1.Height := 25;

  PageControl := TPageControl.Create(Panel);
  PageControl.Parent := Panel;
  PageControl.Align := alClient;

  // Onglet Correspondances
  TabSheet := TTabSheet.Create(PageControl);
  TabSheet.PageControl := PageControl;
  TabSheet.Caption := 'Correspondances';

  FMatchesListView := TListView.Create(TabSheet);
  FMatchesListView.Parent := TabSheet;
  FMatchesListView.Align := alClient;
  FMatchesListView.ViewStyle := vsReport;
  FMatchesListView.RowSelect := True;

  Column := FMatchesListView.Columns.Add;
  Column.Caption := '#';
  Column.Width := 50;

  Column := FMatchesListView.Columns.Add;
  Column.Caption := 'Correspondance';
  Column.Width := 300;

  Column := FMatchesListView.Columns.Add;
  Column.Caption := 'Position';
  Column.Width := 100;

  Column := FMatchesListView.Columns.Add;
  Column.Caption := 'Longueur';
  Column.Width := 100;

  // Onglet Détails
  TabSheet := TTabSheet.Create(PageControl);
  TabSheet.PageControl := PageControl;
  TabSheet.Caption := 'Détails';

  FResultsMemo := TMemo.Create(TabSheet);
  FResultsMemo.Parent := TabSheet;
  FResultsMemo.Align := alClient;
  FResultsMemo.ScrollBars := ssBoth;
  FResultsMemo.Font.Name := 'Courier New';
  FResultsMemo.Font.Size := 9;
  FResultsMemo.ReadOnly := True;
  FResultsMemo.Color := RGB(250, 250, 250);
end;

procedure TfrmRegexEditor.CreateQuickReferencePanel;  
var
  Panel: TPanel;
  Memo: TMemo;
begin
  Panel := TPanel.Create(Self);
  Panel.Parent := Self;
  Panel.Align := alRight;
  Panel.Width := 250;
  Panel.Caption := '';

  with TLabel.Create(Panel) do
  begin
    Parent := Panel;
    Align := alTop;
    Caption := ' Référence rapide';
    Font.Style := [fsBold];
    Height := 25;
  end;

  Memo := TMemo.Create(Panel);
  Memo.Parent := Panel;
  Memo.Align := alClient;
  Memo.ReadOnly := True;
  Memo.ScrollBars := ssVertical;
  Memo.Font.Name := 'Courier New';
  Memo.Font.Size := 8;
  Memo.Color := RGB(255, 255, 230);

  Memo.Lines.Add('CARACTÈRES');
  Memo.Lines.Add('  .    Tout caractère');
  Memo.Lines.Add('  \d   Chiffre [0-9]');
  Memo.Lines.Add('  \D   Non-chiffre');
  Memo.Lines.Add('  \w   Mot [a-zA-Z0-9_]');
  Memo.Lines.Add('  \W   Non-mot');
  Memo.Lines.Add('  \s   Espace blanc');
  Memo.Lines.Add('  \S   Non-espace');
  Memo.Lines.Add('');
  Memo.Lines.Add('QUANTIFICATEURS');
  Memo.Lines.Add('  *    0 ou plus');
  Memo.Lines.Add('  +    1 ou plus');
  Memo.Lines.Add('  ?    0 ou 1');
  Memo.Lines.Add('  {n}  Exactement n');
  Memo.Lines.Add('  {n,} Au moins n');
  Memo.Lines.Add('  {n,m} Entre n et m');
  Memo.Lines.Add('');
  Memo.Lines.Add('ANCRES');
  Memo.Lines.Add('  ^    Début de ligne');
  Memo.Lines.Add('  $    Fin de ligne');
  Memo.Lines.Add('  \b   Frontière de mot');
  Memo.Lines.Add('');
  Memo.Lines.Add('GROUPES');
  Memo.Lines.Add('  (x)  Groupe de capture');
  Memo.Lines.Add('  (?:x) Groupe non-capturant');
  Memo.Lines.Add('  [abc] Ensemble [a ou b ou c]');
  Memo.Lines.Add('  [^abc] Négation');
  Memo.Lines.Add('');
  Memo.Lines.Add('EXEMPLES');
  Memo.Lines.Add('  Email:');
  Memo.Lines.Add('  [\w.-]+@[\w.-]+\.\w+');
  Memo.Lines.Add('');
  Memo.Lines.Add('  Téléphone FR:');
  Memo.Lines.Add('  0[1-9](\s?\d{2}){4}');
  Memo.Lines.Add('');
  Memo.Lines.Add('  Date JJ/MM/AAAA:');
  Memo.Lines.Add('  \d{2}/\d{2}/\d{4}');
end;

procedure TfrmRegexEditor.OnPatternChange(Sender: TObject);  
begin
  // La validation se fait au clic sur "Tester"
end;

procedure TfrmRegexEditor.OnTestTextChange(Sender: TObject);  
begin
  // Auto-test si le pattern est valide
  if FPatternEdit.Text <> '' then
    OnTestRegex(nil);
end;

procedure TfrmRegexEditor.OnOptionsChange(Sender: TObject);  
begin
  OnTestRegex(nil);
end;

procedure TfrmRegexEditor.OnTestRegex(Sender: TObject);  
begin
  if FPatternEdit.Text = '' then
  begin
    ShowError('Entrez une expression régulière');
    Exit;
  end;

  try
    // Configuration du regex
    FRegex.Expression := FPatternEdit.Text;
    FRegex.ModifierI := not FCaseSensitiveCheck.Checked; // i = insensible
    FRegex.ModifierM := FMultiLineCheck.Checked; // m = multi-lignes

    // Test de compilation
    FRegex.Compile;

    // Afficher les résultats
    DisplayMatches;
    HighlightMatches;

    FPatternEdit.Color := clWindow;
    FPatternEdit.Font.Color := clGreen;
  except
    on E: Exception do
    begin
      ShowError('Erreur : ' + E.Message);
      FPatternEdit.Color := RGB(255, 200, 200);
      FPatternEdit.Font.Color := clRed;

      FMatchesListView.Items.Clear;
      FResultsMemo.Lines.Clear;
      FResultsMemo.Lines.Add('ERREUR DE SYNTAXE');
      FResultsMemo.Lines.Add('');
      FResultsMemo.Lines.Add(E.Message);
    end;
  end;
end;

procedure TfrmRegexEditor.DisplayMatches;  
var
  TestText: string;
  MatchCount: Integer;
  Item: TListItem;
  Details: TStringList;
  i: Integer;
begin
  TestText := FTestTextMemo.Lines.Text;
  MatchCount := 0;

  FMatchesListView.Items.Clear;
  Details := TStringList.Create;
  try
    Details.Add('RÉSULTATS DE LA RECHERCHE');
    Details.Add('========================');
    Details.Add('');
    Details.Add('Pattern : ' + FPatternEdit.Text);
    Details.Add('Options : ' +
      IfThen(FCaseSensitiveCheck.Checked, 'Sensible à la casse', 'Insensible') + ', ' +
      IfThen(FMultiLineCheck.Checked, 'Multi-lignes', 'Mono-ligne'));
    Details.Add('');
    Details.Add('CORRESPONDANCES :');
    Details.Add('');

    if FRegex.Exec(TestText) then
    begin
      repeat
        Inc(MatchCount);

        // Ajouter à la ListView
        Item := FMatchesListView.Items.Add;
        Item.Caption := IntToStr(MatchCount);
        Item.SubItems.Add(FRegex.Match[0]);
        Item.SubItems.Add(IntToStr(FRegex.MatchPos[0]));
        Item.SubItems.Add(IntToStr(FRegex.MatchLen[0]));

        // Ajouter aux détails
        Details.Add(Format('Match #%d:', [MatchCount]));
        Details.Add('  Texte    : "' + FRegex.Match[0] + '"');
        Details.Add('  Position : ' + IntToStr(FRegex.MatchPos[0]));
        Details.Add('  Longueur : ' + IntToStr(FRegex.MatchLen[0]));

        // Groupes de capture
        if FRegex.SubExprMatchCount > 0 then
        begin
          Details.Add('  Groupes de capture :');
          for i := 1 to FRegex.SubExprMatchCount do
          begin
            if FRegex.Match[i] <> '' then
              Details.Add(Format('    Groupe %d : "%s"', [i, FRegex.Match[i]]));
          end;
        end;
        Details.Add('');

      until not FRegex.ExecNext;

      Details.Add('');
      Details.Add(Format('Total : %d correspondance(s) trouvée(s)', [MatchCount]));
    end
    else
    begin
      Details.Add('Aucune correspondance trouvée');
    end;

    FResultsMemo.Lines.Assign(Details);
  finally
    Details.Free;
  end;
end;

procedure TfrmRegexEditor.HighlightMatches;  
begin
  // Note : La mise en évidence dans TMemo est limitée
  // Pour un vrai éditeur, utiliser SynEdit avec un highlighter personnalisé
end;

procedure TfrmRegexEditor.ShowError(const AMessage: string);  
begin
  // Afficher dans la barre d'état ou un label
  Application.MessageBox(PChar(AMessage), 'Erreur', MB_OK or MB_ICONERROR);
end;

end.
```

## Éditeur de formules mathématiques

Un éditeur pour créer et évaluer des formules mathématiques :

```pascal
unit MathFormulaEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons, Math;

type
  TfrmMathFormulaEditor = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    FFormulaEdit: TEdit;
    FResultLabel: TLabel;
    FVariablesPanel: TPanel;
    FButtonsPanel: TPanel;
    FHistoryMemo: TMemo;

    FVariables: TStringList;

    procedure SetupInterface;
    procedure CreateFormulaPanel;
    procedure CreateButtonsPanel;
    procedure CreateVariablesPanel;
    procedure CreateHistoryPanel;

    procedure OnFormulaChange(Sender: TObject);
    procedure OnCalculate(Sender: TObject);
    procedure OnInsertFunction(Sender: TObject);
    procedure OnInsertOperator(Sender: TObject);
    procedure OnVariableChange(Sender: TObject);

    function EvaluateFormula(const AFormula: string): Double;
    function ValidateFormula(const AFormula: string): Boolean;
    procedure ShowResult(AValue: Double);

  public
    destructor Destroy; override;
  end;

var
  frmMathFormulaEditor: TfrmMathFormulaEditor;

implementation

{$R *.lfm}

procedure TfrmMathFormulaEditor.FormCreate(Sender: TObject);  
begin
  Caption := 'Éditeur de Formules Mathématiques';
  Width := 800;
  Height := 600;
  Position := poScreenCenter;

  FVariables := TStringList.Create;
  // Variables par défaut
  FVariables.Values['x'] := '0';
  FVariables.Values['y'] := '0';
  FVariables.Values['pi'] := FloatToStr(Pi);
  FVariables.Values['e'] := FloatToStr(Exp(1));

  SetupInterface;
end;

destructor TfrmMathFormulaEditor.Destroy;  
begin
  FVariables.Free;
  inherited Destroy;
end;

procedure TfrmMathFormulaEditor.SetupInterface;  
begin
  CreateFormulaPanel;
  CreateButtonsPanel;
  CreateVariablesPanel;
  CreateHistoryPanel;
end;

procedure TfrmMathFormulaEditor.CreateFormulaPanel;  
var
  Panel: TPanel;
  Label1: TLabel;
  BtnCalculate: TButton;
begin
  Panel := TPanel.Create(Self);
  Panel.Parent := Self;
  Panel.Align := alTop;
  Panel.Height := 120;
  Panel.BevelOuter := bvNone;

  Label1 := TLabel.Create(Panel);
  Label1.Parent := Panel;
  Label1.Left := 10;
  Label1.Top := 10;
  Label1.Caption := 'Formule mathématique :';
  Label1.Font.Style := [fsBold];

  FFormulaEdit := TEdit.Create(Panel);
  FFormulaEdit.Parent := Panel;
  FFormulaEdit.Left := 10;
  FFormulaEdit.Top := 30;
  FFormulaEdit.Width := 600;
  FFormulaEdit.Font.Name := 'Courier New';
  FFormulaEdit.Font.Size := 12;
  FFormulaEdit.OnChange := @OnFormulaChange;
  FFormulaEdit.Text := 'sin(x) * cos(y) + sqrt(25)';

  BtnCalculate := TButton.Create(Panel);
  BtnCalculate.Parent := Panel;
  BtnCalculate.Left := 620;
  BtnCalculate.Top := 28;
  BtnCalculate.Width := 100;
  BtnCalculate.Height := 27;
  BtnCalculate.Caption := 'Calculer';
  BtnCalculate.OnClick := @OnCalculate;
  BtnCalculate.Default := True;

  Label1 := TLabel.Create(Panel);
  Label1.Parent := Panel;
  Label1.Left := 10;
  Label1.Top := 70;
  Label1.Caption := 'Résultat :';
  Label1.Font.Style := [fsBold];

  FResultLabel := TLabel.Create(Panel);
  FResultLabel.Parent := Panel;
  FResultLabel.Left := 100;
  FResultLabel.Top := 70;
  FResultLabel.Font.Size := 14;
  FResultLabel.Font.Color := clBlue;
  FResultLabel.Caption := '0';
end;

procedure TfrmMathFormulaEditor.CreateButtonsPanel;  
var
  Panel: TPanel;
  Btn: TSpeedButton;
  X, Y: Integer;

  procedure AddButton(ACaption: string; ALeft, ATop: Integer; ATag: Integer);
  begin
    Btn := TSpeedButton.Create(Panel);
    Btn.Parent := Panel;
    Btn.Caption := ACaption;
    Btn.Left := ALeft;
    Btn.Top := ATop;
    Btn.Width := 60;
    Btn.Height := 30;
    Btn.Tag := ATag;
    if ATag < 100 then
      Btn.OnClick := @OnInsertOperator
    else
      Btn.OnClick := @OnInsertFunction;
  end;

begin
  Panel := TPanel.Create(Self);
  Panel.Parent := Self;
  Panel.Align := alTop;
  Panel.Height := 150;
  Panel.Caption := '';

  with TLabel.Create(Panel) do
  begin
    Parent := Panel;
    Left := 10;
    Top := 5;
    Caption := 'Opérateurs et fonctions :';
    Font.Style := [fsBold];
  end;

  // Opérateurs de base
  Y := 25;
  AddButton('+', 10, Y, 1);
  AddButton('-', 75, Y, 2);
  AddButton('*', 140, Y, 3);
  AddButton('/', 205, Y, 4);
  AddButton('^', 270, Y, 5);
  AddButton('(', 335, Y, 6);
  AddButton(')', 400, Y, 7);

  // Fonctions trigonométriques
  Y := 60;
  AddButton('sin', 10, Y, 101);
  AddButton('cos', 75, Y, 102);
  AddButton('tan', 140, Y, 103);
  AddButton('asin', 205, Y, 104);
  AddButton('acos', 270, Y, 105);
  AddButton('atan', 335, Y, 106);

  // Fonctions mathématiques
  Y := 95;
  AddButton('sqrt', 10, Y, 201);
  AddButton('abs', 75, Y, 202);
  AddButton('ln', 140, Y, 203);
  AddButton('log', 205, Y, 204);
  AddButton('exp', 270, Y, 205);
  AddButton('pow', 335, Y, 206);
end;

procedure TfrmMathFormulaEditor.CreateVariablesPanel;  
var
  Panel: TPanel;
  Label1: TLabel;
  i, Y: Integer;
  VarLabel: TLabel;
  VarEdit: TEdit;
begin
  Panel := TPanel.Create(Self);
  Panel.Parent := Self;
  Panel.Align := alRight;
  Panel.Width := 200;
  Panel.Caption := '';

  Label1 := TLabel.Create(Panel);
  Label1.Parent := Panel;
  Label1.Align := alTop;
  Label1.Caption := ' Variables';
  Label1.Font.Style := [fsBold];
  Label1.Height := 25;

  FVariablesPanel := Panel;

  // Afficher les variables
  Y := 30;
  for i := 0 to FVariables.Count - 1 do
  begin
    VarLabel := TLabel.Create(Panel);
    VarLabel.Parent := Panel;
    VarLabel.Left := 10;
    VarLabel.Top := Y;
    VarLabel.Caption := FVariables.Names[i] + ' =';

    VarEdit := TEdit.Create(Panel);
    VarEdit.Parent := Panel;
    VarEdit.Left := 50;
    VarEdit.Top := Y - 3;
    VarEdit.Width := 130;
    VarEdit.Text := FVariables.ValueFromIndex[i];
    VarEdit.Name := 'Var_' + FVariables.Names[i];
    VarEdit.OnChange := @OnVariableChange;

    Inc(Y, 30);
  end;
end;

procedure TfrmMathFormulaEditor.CreateHistoryPanel;  
var
  Panel: TPanel;
  Label1: TLabel;
begin
  Panel := TPanel.Create(Self);
  Panel.Parent := Self;
  Panel.Align := alClient;
  Panel.Caption := '';

  Label1 := TLabel.Create(Panel);
  Label1.Parent := Panel;
  Label1.Align := alTop;
  Label1.Caption := ' Historique';
  Label1.Font.Style := [fsBold];
  Label1.Height := 25;

  FHistoryMemo := TMemo.Create(Panel);
  FHistoryMemo.Parent := Panel;
  FHistoryMemo.Align := alClient;
  FHistoryMemo.ScrollBars := ssBoth;
  FHistoryMemo.ReadOnly := True;
  FHistoryMemo.Font.Name := 'Courier New';
end;

procedure TfrmMathFormulaEditor.OnVariableChange(Sender: TObject);  
begin
  FVariables.Values[Copy(TEdit(Sender).Name, 5, 100)] := TEdit(Sender).Text;
end;

procedure TfrmMathFormulaEditor.OnFormulaChange(Sender: TObject);  
begin
  // Validation en temps réel (optionnel)
  if ValidateFormula(FFormulaEdit.Text) then
    FFormulaEdit.Font.Color := clBlack
  else
    FFormulaEdit.Font.Color := clRed;
end;

procedure TfrmMathFormulaEditor.OnCalculate(Sender: TObject);  
var
  Result: Double;
  Formula: string;
begin
  Formula := FFormulaEdit.Text;

  if not ValidateFormula(Formula) then
  begin
    ShowMessage('Formule invalide');
    Exit;
  end;

  try
    Result := EvaluateFormula(Formula);
    ShowResult(Result);

    // Ajouter à l'historique
    FHistoryMemo.Lines.Add(Format('%s = %g', [Formula, Result]));
  except
    on E: Exception do
    begin
      ShowMessage('Erreur de calcul : ' + E.Message);
      FResultLabel.Caption := 'Erreur';
      FResultLabel.Font.Color := clRed;
    end;
  end;
end;

procedure TfrmMathFormulaEditor.OnInsertOperator(Sender: TObject);  
var
  OperatorStr: string;
begin
  case (Sender as TSpeedButton).Tag of
    1: OperatorStr := '+';
    2: OperatorStr := '-';
    3: OperatorStr := '*';
    4: OperatorStr := '/';
    5: OperatorStr := '^';
    6: OperatorStr := '(';
    7: OperatorStr := ')';
  end;

  FFormulaEdit.SelText := OperatorStr;
  FFormulaEdit.SetFocus;
end;

procedure TfrmMathFormulaEditor.OnInsertFunction(Sender: TObject);  
var
  Func: string;
begin
  case (Sender as TSpeedButton).Tag of
    101: Func := 'sin()';
    102: Func := 'cos()';
    103: Func := 'tan()';
    104: Func := 'asin()';
    105: Func := 'acos()';
    106: Func := 'atan()';
    201: Func := 'sqrt()';
    202: Func := 'abs()';
    203: Func := 'ln()';
    204: Func := 'log()';
    205: Func := 'exp()';
    206: Func := 'pow(,)';
  end;

  FFormulaEdit.SelText := Func;
  FFormulaEdit.SetFocus;
  // Positionner le curseur entre les parenthèses
  FFormulaEdit.SelStart := FFormulaEdit.SelStart - Length(Func) + Pos('(', Func);
end;

function TfrmMathFormulaEditor.ValidateFormula(const AFormula: string): Boolean;  
var
  OpenParen, CloseParen: Integer;
  i: Integer;
begin
  Result := False;

  if Trim(AFormula) = '' then Exit;

  // Vérifier l'équilibre des parenthèses
  OpenParen := 0;
  CloseParen := 0;

  for i := 1 to Length(AFormula) do
  begin
    if AFormula[i] = '(' then Inc(OpenParen);
    if AFormula[i] = ')' then Inc(CloseParen);
  end;

  Result := (OpenParen = CloseParen);
end;

function EvaluateSimpleExpression(const AExpr: string): Double; forward;

function TfrmMathFormulaEditor.EvaluateFormula(const AFormula: string): Double;  
var
  Formula: string;
  i: Integer;
begin
  Formula := AFormula;

  // Remplacer les variables par leurs valeurs
  for i := 0 to FVariables.Count - 1 do
  begin
    Formula := StringReplace(Formula, FVariables.Names[i],
                            FVariables.ValueFromIndex[i],
                            [rfReplaceAll, rfIgnoreCase]);
  end;

  // Note : Ceci est une implémentation simplifiée
  // Pour une vraie évaluation d'expressions, utilisez une bibliothèque
  // comme fpExprPars ou implémentez un parser complet

  // Implémentation basique pour les cas simples
  Result := EvaluateSimpleExpression(Formula);
end;

function EvaluateSimpleExpression(const AExpr: string): Double;  
var
  Expr: string;
begin
  Expr := Trim(AExpr);

  // Évaluation très simplifiée - Dans un vrai projet, utilisez un parser
  // Exemple : pour "2+3" retourne 5

  // Ici, vous devriez implémenter un vrai parseur d'expressions
  // ou utiliser une bibliothèque existante

  Result := 0; // Placeholder
end;

procedure TfrmMathFormulaEditor.ShowResult(AValue: Double);  
begin
  FResultLabel.Caption := FloatToStrF(AValue, ffGeneral, 15, 6);
  FResultLabel.Font.Color := clBlue;
end;

end.
```

## Tableau comparatif des éditeurs

| Type d'éditeur | Difficulté | Cas d'usage | Bibliothèques |
|----------------|------------|-------------|---------------|
| **Texte simple** | ⭐ Facile | Notes, logs | TMemo |
| **Code source** | ⭐⭐ Moyen | IDE, éditeur | SynEdit, ATSynEdit |
| **Texte riche** | ⭐⭐ Moyen | Documents | TRichMemo |
| **SQL** | ⭐⭐⭐ Avancé | Administration BD | Custom |
| **Diagrammes** | ⭐⭐⭐⭐ Expert | Modélisation | BGRABitmap |
| **Formulaires** | ⭐⭐⭐⭐ Expert | RAD tools | Custom |
| **Regex** | ⭐⭐ Moyen | Validation | RegExpr |
| **Formules** | ⭐⭐⭐ Avancé | Calculatrice | Math, parser |

## Patterns de conception pour éditeurs

### 1. Pattern MVC (Model-View-Controller)

```pascal
type
  // Model : Données
  TEditorModel = class
  private
    FContent: string;
    FModified: Boolean;
    FObservers: TList;
  public
    procedure SetContent(const AContent: string);
    procedure NotifyObservers;
    property Content: string read FContent;
    property Modified: Boolean read FModified;
  end;

  // View : Affichage
  TEditorView = class
  private
    FEditor: TMemo;
    FModel: TEditorModel;
  public
    procedure Update;
    procedure DisplayContent;
  end;

  // Controller : Logique
  TEditorController = class
  private
    FModel: TEditorModel;
    FView: TEditorView;
  public
    procedure OnUserInput(const AText: string);
    procedure Save;
    procedure Load;
  end;
```

### 2. Pattern Command pour Undo/Redo

```pascal
type
  ICommand = interface
    procedure Execute;
    procedure Undo;
  end;

  TInsertTextCommand = class(TInterfacedObject, ICommand)
  private
    FEditor: TMemo;
    FPosition: Integer;
    FText: string;
  public
    constructor Create(AEditor: TMemo; APosition: Integer; const AText: string);
    procedure Execute;
    procedure Undo;
  end;

constructor TInsertTextCommand.Create(AEditor: TMemo; APosition: Integer;
  const AText: string);
begin
  inherited Create;
  FEditor := AEditor;
  FPosition := APosition;
  FText := AText;
end;

procedure TInsertTextCommand.Execute;  
begin
  FEditor.SelStart := FPosition;
  FEditor.SelLength := 0;
  FEditor.SelText := FText;
end;

procedure TInsertTextCommand.Undo;  
begin
  FEditor.SelStart := FPosition;
  FEditor.SelLength := Length(FText);
  FEditor.SelText := '';
end;
```

### 3. Pattern Observer pour les notifications

```pascal
type
  IEditorObserver = interface
    procedure OnContentChanged;
    procedure OnSelectionChanged;
    procedure OnCursorMoved;
  end;

  TEditorSubject = class
  private
    FObservers: TInterfaceList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Attach(AObserver: IEditorObserver);
    procedure Detach(AObserver: IEditorObserver);
    procedure NotifyContentChanged;
    procedure NotifySelectionChanged;
  end;

constructor TEditorSubject.Create;  
begin
  inherited Create;
  FObservers := TInterfaceList.Create;
end;

destructor TEditorSubject.Destroy;  
begin
  FObservers.Free;
  inherited Destroy;
end;

procedure TEditorSubject.Attach(AObserver: IEditorObserver);  
begin
  if FObservers.IndexOf(AObserver) = -1 then
    FObservers.Add(AObserver);
end;

procedure TEditorSubject.NotifyContentChanged;  
var
  i: Integer;
begin
  for i := 0 to FObservers.Count - 1 do
    (FObservers[i] as IEditorObserver).OnContentChanged;
end;
```

## Optimisations avancées

### 1. Rendering virtuel pour grandes données

```pascal
type
  TVirtualEditor = class
  private
    FVisibleLines: Integer;
    FTopLine: Integer;
    FTotalLines: Integer;
    FLineHeight: Integer;

    procedure OnScroll(Sender: TObject);
    procedure RenderVisibleLines;
  public
    procedure Paint; override;
  end;

procedure TVirtualEditor.RenderVisibleLines;  
var
  i, LineIndex: Integer;
  Y: Integer;
begin
  Y := 0;

  // Ne dessiner que les lignes visibles
  for i := 0 to FVisibleLines - 1 do
  begin
    LineIndex := FTopLine + i;
    if LineIndex >= FTotalLines then Break;

    // Dessiner uniquement cette ligne
    DrawLine(LineIndex, Y);
    Inc(Y, FLineHeight);
  end;
end;
```

### 2. Cache de rendu

```pascal
type
  TRenderProc = procedure;

  TRenderCache = class
  private
    FCachedBitmap: TBitmap;
    FCacheValid: Boolean;
    FLastContent: string;
  public
    procedure Invalidate;
    function GetCachedBitmap: TBitmap;
    procedure UpdateCache(AContent: string; ARenderFunc: TRenderProc);
  end;

procedure TRenderCache.UpdateCache(AContent: string; ARenderFunc: TRenderProc);  
begin
  if not FCacheValid or (AContent <> FLastContent) then
  begin
    ARenderFunc; // Fonction de rendu
    FLastContent := AContent;
    FCacheValid := True;
  end;
end;
```

### 3. Chargement progressif

```pascal
type
  TProgressiveLoader = class
  private
    FLoadTimer: TTimer;
    FCurrentLine: Integer;
    FTotalLines: Integer;
    FChunkSize: Integer;

    procedure OnLoadTimer(Sender: TObject);
  public
    procedure LoadFileProgressively(const AFileName: string);
  end;

procedure TProgressiveLoader.LoadFileProgressively(const AFileName: string);  
begin
  FCurrentLine := 0;
  FChunkSize := 100; // Charger 100 lignes à la fois

  FLoadTimer.Enabled := True;
end;

procedure TProgressiveLoader.OnLoadTimer(Sender: TObject);  
var
  i: Integer;
begin
  // Charger un chunk de lignes
  for i := FCurrentLine to Min(FCurrentLine + FChunkSize - 1, FTotalLines - 1) do
  begin
    // Charger et afficher la ligne i
  end;

  Inc(FCurrentLine, FChunkSize);

  if FCurrentLine >= FTotalLines then
    FLoadTimer.Enabled := False;
end;
```

## Conclusion

Les éditeurs et designers intégrés sont des composants puissants qui enrichissent considérablement vos applications. En suivant les bonnes pratiques et en utilisant les bibliothèques appropriées, vous pouvez créer des outils professionnels et performants.

### Points clés à retenir

1. **Commencez simple** : Un éditeur de base fonctionnel vaut mieux qu'un éditeur complexe bugué

2. **Performance** : Optimisez le rendu et les mises à jour
   - Utilisez le double buffering
   - Invalidez seulement les zones modifiées
   - Implémentez le rendu virtuel pour de grandes données

3. **UX** : L'expérience utilisateur est primordiale
   - Feedback immédiat
   - Raccourcis clavier intuitifs
   - Messages d'erreur clairs
   - Sauvegarde automatique

4. **Testez** : Sur les deux plateformes (Windows/Ubuntu)
   - Chemins portables
   - Polices adaptées
   - Raccourcis standards

5. **Documentez** : Le code d'éditeur peut devenir complexe
   - Commentez les algorithmes
   - Expliquez les choix de design
   - Créez des diagrammes

### Architecture recommandée

```
┌─────────────────────────────────────────┐
│           Interface Utilisateur         │
│  (SynEdit, TMemo, Custom Controls)      │
└──────────────────┬──────────────────────┘
                   │
┌──────────────────▼──────────────────────┐
│            Controller Layer             │
│  (Gestion des commandes, validation)    │
└──────────────────┬──────────────────────┘
                   │
┌──────────────────▼──────────────────────┐
│            Business Logic               │
│  (Parser, Evaluateur, Validator)        │
└──────────────────┬──────────────────────┘
                   │
┌──────────────────▼──────────────────────┐
│            Data Model                   │
│  (Document, Undo/Redo, State)           │
└─────────────────────────────────────────┘
```

### Prochaines étapes

Pour aller plus loin :

1. **Explorez SynEdit en profondeur**
   - Créez des highlighters personnalisés
   - Implémentez l'auto-complétion intelligente
   - Ajoutez le pliage de code

2. **Étudiez le code source de Lazarus IDE**
   - Architecture modulaire
   - Système de plugins
   - Gestion des fichiers

3. **Implémentez des fonctionnalités avancées**
   - Collaboration en temps réel
   - Historique de versions
   - Snippets de code
   - Refactoring automatique

4. **Optimisez pour de gros fichiers**
   - Rendu virtuel
   - Chargement paresseux
   - Indexation pour la recherche

### Ressources complémentaires

#### Documentation officielle
- **Lazarus Wiki** : https://wiki.lazarus.freepascal.org/
- **FreePascal Documentation** : https://www.freepascal.org/docs.html
- **SynEdit Documentation** : Dans le code source de Lazarus

#### Forums et communauté
- **Forum Lazarus** : https://forum.lazarus.freepascal.org/
- **Pascal Game Development** : Pour les éditeurs graphiques
- **Stack Overflow** : Tag "lazarus" et "freepascal"

#### Projets open source à étudier
- **Lazarus IDE** : L'IDE lui-même
- **Double Commander** : Gestionnaire de fichiers
- **Notepad++ alternatives** : Éditeurs de texte

#### Bibliothèques utiles
- **SynEdit** : Éditeur de code (inclus)
- **ATSynEdit** : Alternative moderne
- **BGRABitmap** : Graphiques avancés
- **fpExprPars** : Parseur d'expressions
- **RegExpr** : Expressions régulières
- **fpvectorial** : Formats vectoriels

### Checklist projet d'éditeur

Avant de déployer votre éditeur :

#### Fonctionnalités de base
- [ ] Nouveau/Ouvrir/Enregistrer/Enregistrer sous
- [ ] Couper/Copier/Coller
- [ ] Annuler/Refaire (au moins 50 niveaux)
- [ ] Rechercher/Remplacer
- [ ] Aller à la ligne
- [ ] Sélection multiple

#### Interface
- [ ] Barre de menus complète
- [ ] Barre d'outils avec icônes
- [ ] Barre d'état avec informations
- [ ] Menu contextuel
- [ ] Glisser-déposer de fichiers
- [ ] Raccourcis clavier standards

#### Qualité
- [ ] Pas de fuite mémoire
- [ ] Gestion des gros fichiers (>10 MB)
- [ ] Encodages multiples (UTF-8, ISO, etc.)
- [ ] Détection automatique de l'encodage
- [ ] Fins de ligne multiples (CRLF, LF, CR)
- [ ] Sauvegarde de secours

#### Multi-plateforme
- [ ] Testé sur Windows 10/11
- [ ] Testé sur Ubuntu 22.04/24.04
- [ ] Chemins relatifs et absolus
- [ ] Polices par défaut adaptées
- [ ] Thèmes clair et sombre

#### Documentation
- [ ] Aide intégrée (F1)
- [ ] Fichier README
- [ ] Exemples d'utilisation
- [ ] Changelog
- [ ] Licence claire

### Message final

Créer un éditeur ou un designer est un projet ambitieux mais très gratifiant. Vous apprendrez énormément sur :
- L'architecture logicielle
- L'optimisation de performance
- L'expérience utilisateur
- La gestion d'état complexe
- Les patterns de conception

N'ayez pas peur de commencer petit et d'itérer. Les meilleurs éditeurs ont commencé comme de simples éditeurs de texte avant d'évoluer vers des outils puissants.

**Bonne chance dans votre développement ! 🚀**

---

*Fin du chapitre 12.7 - Éditeurs et designers intégrés*

**Pour continuer votre apprentissage**, consultez les chapitres suivants du programme de formation :
- 12.8 Graphiques vectoriels SVG
- 12.9 Traitement d'images avancé
- 12.10 Vision par ordinateur avec OpenCV

**N'oubliez pas** : La pratique est essentielle. Créez des petits projets d'éditeurs pour chaque concept appris, et progressivement, vous maîtriserez cet art complexe qu'est le développement d'éditeurs intégrés.

⏭️ [Graphiques vectoriels SVG](/12-interfaces-graphiques-avancees/08-graphiques-vectoriels-svg.md)
