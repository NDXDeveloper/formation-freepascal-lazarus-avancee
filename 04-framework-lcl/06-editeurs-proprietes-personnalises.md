🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 4.6 Éditeurs de propriétés personnalisés

## Introduction : Pourquoi créer des éditeurs de propriétés ?

Quand vous travaillez dans Lazarus, vous avez remarqué que certaines propriétés ont des éditeurs spéciaux. Par exemple :
- La propriété **Color** affiche une palette de couleurs
- La propriété **Font** ouvre une boîte de dialogue complète
- Les propriétés de type **TStrings** ouvrent un éditeur de texte multiligne

Ces éditeurs personnalisés rendent l'IDE plus convivial et évitent les erreurs. Imaginez devoir taper manuellement une couleur en hexadécimal au lieu de la choisir dans une palette !

Dans ce chapitre, nous allons apprendre à créer nos propres éditeurs de propriétés pour rendre nos composants aussi professionnels que ceux fournis avec Lazarus.

## Comprendre les types d'éditeurs

### Les différentes catégories d'éditeurs

Il existe plusieurs types d'éditeurs de propriétés :

1. **Éditeurs simples** : Modifient directement la valeur dans l'inspecteur
2. **Éditeurs avec dialogue** : Ouvrent une fenêtre pour éditer la valeur
3. **Éditeurs avec liste déroulante** : Proposent des choix prédéfinis
4. **Éditeurs avec sous-propriétés** : Permettent d'éditer des propriétés complexes

```pascal
// Exemple de propriété qui pourrait bénéficier d'un éditeur personnalisé
type
  TEmailAddress = string;
  TPhoneNumber = string;
  TColorPair = record
    ForegroundColor: TColor;
    BackgroundColor: TColor;
  end;

  TMyComponent = class(TComponent)
  published
    property Email: TEmailAddress;      // Pourrait valider le format email
    property Phone: TPhoneNumber;        // Pourrait formater le numéro
    property Colors: TColorPair;         // Pourrait avoir un éditeur spécial
  end;
```

## Créer un éditeur simple de propriété

### Structure de base d'un éditeur

Commençons par un éditeur simple qui valide une adresse email :

```pascal
unit EmailPropertyEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PropEdits, Forms, Dialogs;

type
  // Notre éditeur personnalisé pour les emails
  TEmailPropertyEditor = class(TStringPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
    function GetValue: string; override;
    procedure SetValue(const NewValue: string); override;
  end;

implementation

function TEmailPropertyEditor.GetAttributes: TPropertyAttributes;  
begin
  // Définir les capacités de l'éditeur
  Result := [paDialog,        // Peut ouvrir une boîte de dialogue
             paAutoUpdate,     // Met à jour automatiquement
             paValueList,      // Peut proposer une liste de valeurs
             paMultiSelect];   // Peut éditer plusieurs composants à la fois
end;

function TEmailPropertyEditor.GetValue: string;  
begin
  // Récupérer la valeur actuelle
  Result := GetStrValue;

  // On peut la formater pour l'affichage
  if Result = '' then
    Result := '(aucun email)'
  else if Pos('@', Result) = 0 then
    Result := Result + ' (invalide!)';
end;

procedure TEmailPropertyEditor.SetValue(const NewValue: string);  
var
  Email: string;
begin
  Email := Trim(NewValue);

  // Validation simple
  if Email <> '' then
  begin
    // Vérifier qu'il y a un @
    if Pos('@', Email) = 0 then
      raise Exception.Create('Une adresse email doit contenir "@"');

    // Vérifier qu'il y a un point après le @
    if Pos('.', Copy(Email, Pos('@', Email), Length(Email))) = 0 then
      raise Exception.Create('Le domaine doit contenir un point');
  end;

  // Si tout est OK, définir la valeur
  SetStrValue(Email);
end;

procedure TEmailPropertyEditor.Edit;  
var
  EmailDialog: TForm;
  Edit: TEdit;
  BtnOK, BtnCancel: TButton;
begin
  // Créer une boîte de dialogue personnalisée
  EmailDialog := TForm.Create(nil);
  try
    EmailDialog.Caption := 'Éditeur d''adresse email';
    EmailDialog.Position := poScreenCenter;
    EmailDialog.BorderStyle := bsDialog;
    EmailDialog.Width := 400;
    EmailDialog.Height := 150;

    // Ajouter un champ de saisie
    Edit := TEdit.Create(EmailDialog);
    Edit.Parent := EmailDialog;
    Edit.SetBounds(10, 20, 370, 25);
    Edit.Text := GetStrValue;

    // Ajouter des boutons
    BtnOK := TButton.Create(EmailDialog);
    BtnOK.Parent := EmailDialog;
    BtnOK.Caption := 'OK';
    BtnOK.SetBounds(220, 80, 75, 25);
    BtnOK.ModalResult := mrOK;
    BtnOK.Default := True;

    BtnCancel := TButton.Create(EmailDialog);
    BtnCancel.Parent := EmailDialog;
    BtnCancel.Caption := 'Annuler';
    BtnCancel.SetBounds(305, 80, 75, 25);
    BtnCancel.ModalResult := mrCancel;
    BtnCancel.Cancel := True;

    // Afficher la boîte de dialogue
    if EmailDialog.ShowModal = mrOK then
    begin
      try
        SetValue(Edit.Text);
      except
        on E: Exception do
          ShowMessage('Erreur : ' + E.Message);
      end;
    end;
  finally
    EmailDialog.Free;
  end;
end;
```

## Éditeur avec liste de valeurs

### Créer un éditeur qui propose des choix

```pascal
unit CountryPropertyEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PropEdits, TypInfo;

type
  TCountryPropertyEditor = class(TStringPropertyEditor)
  private
    FCountries: TStringList;
    procedure LoadCountries;
  public
    constructor Create(Hook: TPropertyEditorHook; APropCount: Integer); override;
    destructor Destroy; override;

    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const NewValue: string); override;
  end;

implementation

constructor TCountryPropertyEditor.Create(Hook: TPropertyEditorHook; APropCount: Integer);  
begin
  inherited Create(Hook, APropCount);
  FCountries := TStringList.Create;
  LoadCountries;
end;

destructor TCountryPropertyEditor.Destroy;  
begin
  FCountries.Free;
  inherited Destroy;
end;

procedure TCountryPropertyEditor.LoadCountries;  
begin
  // Charger la liste des pays
  FCountries.Add('France');
  FCountries.Add('Allemagne');
  FCountries.Add('Espagne');
  FCountries.Add('Italie');
  FCountries.Add('Royaume-Uni');
  FCountries.Add('États-Unis');
  FCountries.Add('Canada');
  FCountries.Add('Japon');
  FCountries.Add('Chine');
  FCountries.Add('Brésil');

  FCountries.Sort; // Trier alphabétiquement
end;

function TCountryPropertyEditor.GetAttributes: TPropertyAttributes;  
begin
  // Indiquer qu'on a une liste de valeurs et qu'on peut trier
  Result := [paValueList, paSortList, paAutoUpdate];
end;

procedure TCountryPropertyEditor.GetValues(Proc: TGetStrProc);  
var
  i: Integer;
begin
  // Fournir la liste des valeurs possibles
  for i := 0 to FCountries.Count - 1 do
    Proc(FCountries[i]);
end;

procedure TCountryPropertyEditor.SetValue(const NewValue: string);  
begin
  // Vérifier que la valeur est dans la liste
  if (NewValue <> '') and (FCountries.IndexOf(NewValue) = -1) then
  begin
    // Proposition : ajouter le pays s'il n'existe pas
    if MessageDlg('Nouveau pays',
                  Format('"%s" n''est pas dans la liste. L''ajouter ?', [NewValue]),
                  mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      FCountries.Add(NewValue);
      FCountries.Sort;
    end
    else
      Exit; // Annuler la modification
  end;

  SetStrValue(NewValue);
end;
```

## Éditeur pour types complexes

### Éditeur pour un record ou une classe

```pascal
unit ColorPairEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PropEdits, Graphics, Forms, Controls, StdCtrls, Dialogs;

type
  // Notre type complexe
  TColorPair = class(TPersistent)
  private
    FForeground: TColor;
    FBackground: TColor;
  public
    procedure Assign(Source: TPersistent); override;
    function ToString: string;
  published
    property Foreground: TColor read FForeground write FForeground;
    property Background: TColor read FBackground write FBackground;
  end;

  // L'éditeur pour ce type
  TColorPairPropertyEditor = class(TClassPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
    function GetValue: string; override;
  end;

  // La boîte de dialogue d'édition
  TColorPairEditDialog = class(TForm)
  private
    FForegroundBtn: TButton;
    FBackgroundBtn: TButton;
    FPreview: TPanel;
    FOKBtn: TButton;
    FCancelBtn: TButton;
    FColorPair: TColorPair;

    procedure ForegroundBtnClick(Sender: TObject);
    procedure BackgroundBtnClick(Sender: TObject);
    procedure UpdatePreview;
  public
    constructor CreateNew(AOwner: TComponent; Dummy: Integer = 0); override;

    property ColorPair: TColorPair read FColorPair write FColorPair;
  end;

implementation

{ TColorPair }

procedure TColorPair.Assign(Source: TPersistent);  
begin
  if Source is TColorPair then
  begin
    FForeground := TColorPair(Source).Foreground;
    FBackground := TColorPair(Source).Background;
  end
  else
    inherited Assign(Source);
end;

function TColorPair.ToString: string;  
begin
  Result := Format('Fg:%s, Bg:%s',
    [ColorToString(FForeground), ColorToString(FBackground)]);
end;

{ TColorPairPropertyEditor }

function TColorPairPropertyEditor.GetAttributes: TPropertyAttributes;  
begin
  Result := [paDialog, paSubProperties, paReadOnly];
end;

function TColorPairPropertyEditor.GetValue: string;  
var
  ColorPair: TColorPair;
begin
  ColorPair := TColorPair(GetObjectValue);
  if Assigned(ColorPair) then
    Result := ColorPair.ToString
  else
    Result := '(vide)';
end;

procedure TColorPairPropertyEditor.Edit;  
var
  Dialog: TColorPairEditDialog;
  ColorPair: TColorPair;
begin
  ColorPair := TColorPair(GetObjectValue);
  if not Assigned(ColorPair) then
    Exit;

  Dialog := TColorPairEditDialog.CreateNew(nil);
  try
    Dialog.ColorPair := ColorPair;

    if Dialog.ShowModal = mrOK then
    begin
      // Les modifications ont été faites directement sur l'objet
      Modified; // Notifier l'IDE que la propriété a changé
    end;
  finally
    Dialog.Free;
  end;
end;

{ TColorPairEditDialog }

constructor TColorPairEditDialog.CreateNew(AOwner: TComponent; Dummy: Integer);  
begin
  inherited CreateNew(AOwner, Dummy);

  Caption := 'Éditeur de paire de couleurs';
  Position := poScreenCenter;
  BorderStyle := bsDialog;
  Width := 400;
  Height := 250;

  // Bouton pour la couleur de premier plan
  FForegroundBtn := TButton.Create(Self);
  FForegroundBtn.Parent := Self;
  FForegroundBtn.SetBounds(10, 20, 150, 30);
  FForegroundBtn.Caption := 'Couleur de texte...';
  FForegroundBtn.OnClick := @ForegroundBtnClick;

  // Bouton pour la couleur de fond
  FBackgroundBtn := TButton.Create(Self);
  FBackgroundBtn.Parent := Self;
  FBackgroundBtn.SetBounds(10, 60, 150, 30);
  FBackgroundBtn.Caption := 'Couleur de fond...';
  FBackgroundBtn.OnClick := @BackgroundBtnClick;

  // Panneau de prévisualisation
  FPreview := TPanel.Create(Self);
  FPreview.Parent := Self;
  FPreview.SetBounds(180, 20, 200, 70);
  FPreview.Caption := 'Aperçu du texte';
  FPreview.BevelOuter := bvLowered;

  // Boutons OK et Annuler
  FOKBtn := TButton.Create(Self);
  FOKBtn.Parent := Self;
  FOKBtn.SetBounds(220, 180, 75, 25);
  FOKBtn.Caption := 'OK';
  FOKBtn.ModalResult := mrOK;
  FOKBtn.Default := True;

  FCancelBtn := TButton.Create(Self);
  FCancelBtn.Parent := Self;
  FCancelBtn.SetBounds(305, 180, 75, 25);
  FCancelBtn.Caption := 'Annuler';
  FCancelBtn.ModalResult := mrCancel;
  FCancelBtn.Cancel := True;
end;

procedure TColorPairEditDialog.ForegroundBtnClick(Sender: TObject);  
var
  ColorDialog: TColorDialog;
begin
  ColorDialog := TColorDialog.Create(nil);
  try
    ColorDialog.Color := FColorPair.Foreground;
    if ColorDialog.Execute then
    begin
      FColorPair.Foreground := ColorDialog.Color;
      UpdatePreview;
    end;
  finally
    ColorDialog.Free;
  end;
end;

procedure TColorPairEditDialog.BackgroundBtnClick(Sender: TObject);  
var
  ColorDialog: TColorDialog;
begin
  ColorDialog := TColorDialog.Create(nil);
  try
    ColorDialog.Color := FColorPair.Background;
    if ColorDialog.Execute then
    begin
      FColorPair.Background := ColorDialog.Color;
      UpdatePreview;
    end;
  finally
    ColorDialog.Free;
  end;
end;

procedure TColorPairEditDialog.UpdatePreview;  
begin
  FPreview.Font.Color := FColorPair.Foreground;
  FPreview.Color := FColorPair.Background;
end;
```

## Éditeur inline (dans la grille de propriétés)

### Créer un éditeur qui s'affiche directement dans l'inspecteur

```pascal
unit PercentageEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PropEdits, Controls, StdCtrls, ComCtrls, Graphics;

type
  // Type pour représenter un pourcentage (0-100)
  TPercentage = type Integer;

  // Éditeur avec barre de progression intégrée
  TPercentagePropertyEditor = class(TIntegerPropertyEditor)
  private
    FTrackBar: TTrackBar;
  protected
    procedure PropDrawValue(ACanvas: TCanvas; const ARect: TRect;
      AState: TPropEditDrawState); override;
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure SetValue(const NewValue: string); override;
    function GetValue: string; override;
  end;

implementation

function TPercentagePropertyEditor.GetAttributes: TPropertyAttributes;  
begin
  Result := inherited GetAttributes + [paCustomDrawn];
end;

function TPercentagePropertyEditor.GetValue: string;  
begin
  Result := IntToStr(GetOrdValue) + '%';
end;

procedure TPercentagePropertyEditor.SetValue(const NewValue: string);  
var
  S: string;
  V: Integer;
begin
  S := Trim(NewValue);

  // Enlever le signe % s'il est présent
  if (Length(S) > 0) and (S[Length(S)] = '%') then
    S := Copy(S, 1, Length(S) - 1);

  V := StrToIntDef(S, 0);

  // Limiter entre 0 et 100
  if V < 0 then V := 0;
  if V > 100 then V := 100;

  SetOrdValue(V);
end;

procedure TPercentagePropertyEditor.PropDrawValue(ACanvas: TCanvas;
  const ARect: TRect; AState: TPropEditDrawState);
var
  Value: Integer;
  ProgressRect, TextRect: TRect;
  S: string;
  TextWidth, TextHeight: Integer;
begin
  Value := GetOrdValue;
  S := GetValue;

  // Dessiner le fond
  if pedsSelected in AState then
    ACanvas.Brush.Color := clHighlight
  else
    ACanvas.Brush.Color := clWindow;
  ACanvas.FillRect(ARect);

  // Calculer la zone de la barre de progression
  ProgressRect := ARect;
  InflateRect(ProgressRect, -2, -2);
  ProgressRect.Right := ProgressRect.Left +
    MulDiv(ProgressRect.Right - ProgressRect.Left, Value, 100);

  // Dessiner la barre de progression
  ACanvas.Brush.Color := clSkyBlue;
  ACanvas.FillRect(ProgressRect);

  // Dessiner le texte
  ACanvas.Font.Color := clBlack;
  ACanvas.Brush.Style := bsClear;

  TextWidth := ACanvas.TextWidth(S);
  TextHeight := ACanvas.TextHeight(S);
  TextRect := ARect;

  ACanvas.TextOut(
    TextRect.Left + (TextRect.Right - TextRect.Left - TextWidth) div 2,
    TextRect.Top + (TextRect.Bottom - TextRect.Top - TextHeight) div 2,
    S
  );

  // Dessiner le cadre
  ACanvas.Brush.Style := bsClear;
  ACanvas.Pen.Color := clGray;
  ACanvas.Rectangle(ARect);
end;
```

## Éditeur de composant (Component Editor)

### Différence avec l'éditeur de propriété

Un **éditeur de composant** agit sur le composant entier (menu contextuel, double-clic) tandis qu'un **éditeur de propriété** agit sur une propriété spécifique.

```pascal
unit MyComponentEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComponentEditors, Forms, Dialogs, Menus;

type
  // Composant exemple
  TConfigurablePanel = class(TPanel)
  private
    FConfigFile: string;
  published
    property ConfigFile: string read FConfigFile write FConfigFile;
  end;

  // Éditeur de composant
  TConfigurablePanelEditor = class(TComponentEditor)
  private
    procedure ShowConfigDialog;
    procedure LoadPreset(PresetName: string);
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure Edit; override;
  end;

implementation

procedure TConfigurablePanelEditor.Edit;  
begin
  // Action lors du double-clic sur le composant
  ShowConfigDialog;
end;

function TConfigurablePanelEditor.GetVerbCount: Integer;  
begin
  // Nombre d'éléments dans le menu contextuel
  Result := 4;
end;

function TConfigurablePanelEditor.GetVerb(Index: Integer): string;  
begin
  // Texte des éléments du menu contextuel
  case Index of
    0: Result := 'Configurer...';
    1: Result := '-';  // Séparateur
    2: Result := 'Charger preset Standard';
    3: Result := 'Charger preset Sombre';
  end;
end;

procedure TConfigurablePanelEditor.ExecuteVerb(Index: Integer);  
begin
  // Actions du menu contextuel
  case Index of
    0: ShowConfigDialog;
    2: LoadPreset('Standard');
    3: LoadPreset('Sombre');
  end;
end;

procedure TConfigurablePanelEditor.ShowConfigDialog;  
var
  Dialog: TForm;
  Panel: TConfigurablePanel;
begin
  Panel := TConfigurablePanel(Component);

  Dialog := TForm.Create(nil);
  try
    Dialog.Caption := 'Configuration de ' + Panel.Name;
    Dialog.Position := poScreenCenter;
    Dialog.Width := 400;
    Dialog.Height := 300;

    // Ajouter les contrôles de configuration...
    // (Code simplifié pour l'exemple)

    if Dialog.ShowModal = mrOK then
    begin
      // Appliquer les changements
      Panel.Color := clSilver;  // Exemple
      Designer.Modified;  // Notifier l'IDE des changements
    end;
  finally
    Dialog.Free;
  end;
end;

procedure TConfigurablePanelEditor.LoadPreset(PresetName: string);  
var
  Panel: TConfigurablePanel;
begin
  Panel := TConfigurablePanel(Component);

  if PresetName = 'Standard' then
  begin
    Panel.Color := clBtnFace;
    Panel.Font.Color := clWindowText;
    Panel.BorderStyle := bsSingle;
  end
  else if PresetName = 'Sombre' then
  begin
    Panel.Color := $00202020;
    Panel.Font.Color := clWhite;
    Panel.BorderStyle := bsNone;
  end;

  Designer.Modified;
end;
```

## Enregistrement des éditeurs

### Comment enregistrer vos éditeurs dans l'IDE

```pascal
unit RegisterMyEditors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PropEdits, ComponentEditors;

procedure Register;

implementation

uses
  EmailPropertyEditor, CountryPropertyEditor, ColorPairEditor,
  PercentageEditor, MyComponentEditor;

procedure Register;  
begin
  // Enregistrer les éditeurs de propriétés

  // Pour un type spécifique
  RegisterPropertyEditor(TypeInfo(TEmailAddress), nil, '',
    TEmailPropertyEditor);

  // Pour une propriété spécifique d'un composant
  RegisterPropertyEditor(TypeInfo(string), TMyComponent, 'Country',
    TCountryPropertyEditor);

  // Pour une classe
  RegisterPropertyEditor(TypeInfo(TColorPair), nil, '',
    TColorPairPropertyEditor);

  // Pour un type avec contraintes
  RegisterPropertyEditor(TypeInfo(TPercentage), nil, '',
    TPercentagePropertyEditor);

  // Enregistrer les éditeurs de composants
  RegisterComponentEditor(TConfigurablePanel, TConfigurablePanelEditor);
end;

end.
```

## Techniques avancées

### Éditeur avec prévisualisation en temps réel

```pascal
unit GradientEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PropEdits, Graphics, Forms, Controls, ExtCtrls;

type
  TGradientColors = class(TPersistent)
  private
    FStartColor: TColor;
    FEndColor: TColor;
    FOnChange: TNotifyEvent;

    procedure SetStartColor(Value: TColor);
    procedure SetEndColor(Value: TColor);
  public
    procedure Assign(Source: TPersistent); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property StartColor: TColor read FStartColor write SetStartColor;
    property EndColor: TColor read FEndColor write SetEndColor;
  end;

  TGradientEditor = class(TClassPropertyEditor)
  private
    FPreviewForm: TForm;
    FPreviewPanel: TPanel;
    FGradient: TGradientColors;

    procedure UpdatePreview;
    procedure DrawGradient(Canvas: TCanvas; Rect: TRect);
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

implementation

procedure TGradientColors.SetStartColor(Value: TColor);  
begin
  if FStartColor <> Value then
  begin
    FStartColor := Value;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TGradientColors.SetEndColor(Value: TColor);  
begin
  if FEndColor <> Value then
  begin
    FEndColor := Value;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TGradientEditor.Edit;  
var
  ColorDialog: TColorDialog;
  StartBtn, EndBtn: TButton;
begin
  FGradient := TGradientColors(GetObjectValue);
  if not Assigned(FGradient) then Exit;

  // Créer la fenêtre de prévisualisation
  FPreviewForm := TForm.Create(nil);
  try
    FPreviewForm.Caption := 'Éditeur de gradient';
    FPreviewForm.Position := poScreenCenter;
    FPreviewForm.Width := 400;
    FPreviewForm.Height := 200;

    // Panel de prévisualisation
    FPreviewPanel := TPanel.Create(FPreviewForm);
    FPreviewPanel.Parent := FPreviewForm;
    FPreviewPanel.SetBounds(10, 10, 370, 80);
    FPreviewPanel.BevelOuter := bvLowered;
    FPreviewPanel.OnPaint := @PreviewPanelPaint;

    // Boutons pour choisir les couleurs
    StartBtn := TButton.Create(FPreviewForm);
    StartBtn.Parent := FPreviewForm;
    StartBtn.SetBounds(10, 100, 150, 30);
    StartBtn.Caption := 'Couleur de début...';
    StartBtn.OnClick := @StartColorClick;

    EndBtn := TButton.Create(FPreviewForm);
    EndBtn.Parent := FPreviewForm;
    EndBtn.SetBounds(230, 100, 150, 30);
    EndBtn.Caption := 'Couleur de fin...';
    EndBtn.OnClick := @EndColorClick;

    UpdatePreview;

    if FPreviewForm.ShowModal = mrOK then
      Modified;

  finally
    FPreviewForm.Free;
  end;
end;

procedure TGradientEditor.UpdatePreview;  
begin
  if Assigned(FPreviewPanel) then
    FPreviewPanel.Invalidate;
end;

procedure TGradientEditor.DrawGradient(Canvas: TCanvas; Rect: TRect);  
var
  i, Steps: Integer;
  R1, G1, B1, R2, G2, B2: Byte;
  StepR, StepG, StepB: Double;
  CurrentR, CurrentG, CurrentB: Double;
  StepWidth: Double;
  CurrentRect: TRect;
begin
  // Décomposer les couleurs
  R1 := GetRValue(ColorToRGB(FGradient.StartColor));
  G1 := GetGValue(ColorToRGB(FGradient.StartColor));
  B1 := GetBValue(ColorToRGB(FGradient.StartColor));

  R2 := GetRValue(ColorToRGB(FGradient.EndColor));
  G2 := GetGValue(ColorToRGB(FGradient.EndColor));
  B2 := GetBValue(ColorToRGB(FGradient.EndColor));

  // Calculer le gradient
  Steps := Rect.Right - Rect.Left;
  if Steps <= 0 then Exit;

  StepR := (R2 - R1) / Steps;
  StepG := (G2 - G1) / Steps;
  StepB := (B2 - B1) / Steps;

  CurrentR := R1;
  CurrentG := G1;
  CurrentB := B1;

  // Dessiner le gradient
  for i := 0 to Steps - 1 do
  begin
    Canvas.Pen.Color := RGB(Round(CurrentR), Round(CurrentG), Round(CurrentB));
    Canvas.Line(Rect.Left + i, Rect.Top, Rect.Left + i, Rect.Bottom);

    CurrentR := CurrentR + StepR;
    CurrentG := CurrentG + StepG;
    CurrentB := CurrentB + StepB;
  end;
end;
```

## Bonnes pratiques pour les éditeurs

### Conseils pour créer des éditeurs professionnels

1. **Validation intelligente** : Validez les entrées mais proposez des corrections
```pascal
procedure TPhoneEditor.SetValue(const NewValue: string);  
var
  Cleaned: string;
begin
  // Nettoyer automatiquement le format
  Cleaned := StringReplace(NewValue, ' ', '', [rfReplaceAll]);
  Cleaned := StringReplace(Cleaned, '-', '', [rfReplaceAll]);
  Cleaned := StringReplace(Cleaned, '.', '', [rfReplaceAll]);

  // Reformater
  if Length(Cleaned) = 10 then
    SetStrValue(Format('%s-%s-%s-%s-%s',
      [Copy(Cleaned, 1, 2), Copy(Cleaned, 3, 2),
       Copy(Cleaned, 5, 2), Copy(Cleaned, 7, 2),
       Copy(Cleaned, 9, 2)]))
  else if Length(Cleaned) > 0 then
    ShowMessage('Un numéro de téléphone doit contenir 10 chiffres');
end;
```

2. **Feedback visuel immédiat** : Montrez les erreurs visuellement
```pascal
procedure TEmailEditor.PropDrawValue(ACanvas: TCanvas; const ARect: TRect;
  AState: TPropEditDrawState);
var
  Email: string;
  IsValid: Boolean;
begin
  Email := GetStrValue;
  IsValid := IsValidEmail(Email);

  // Changer la couleur selon la validité
  if IsValid then
    ACanvas.Font.Color := clGreen
  else if Email <> '' then
    ACanvas.Font.Color := clRed
  else
    ACanvas.Font.Color := clGray;

  // Dessiner le texte
  inherited PropDrawValue(ACanvas, ARect, AState);
end;
```

3. **Aide contextuelle** : Fournissez des informations utiles
```pascal
function TComplexEditor.GetHint: string;  
begin
  Result := 'Format attendu : XXX-XXX-XXXX' + LineEnding +
            'Exemple : 123-456-7890' + LineEnding +
            'Double-cliquez pour ouvrir l''éditeur avancé';
end;
```

### Gestion des erreurs utilisateur

```pascal
unit SafePropertyEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PropEdits, Dialogs;

type
  TSafePropertyEditor = class(TPropertyEditor)
  protected
    function ValidateValue(const Value: string; out ErrorMsg: string): Boolean; virtual;
    procedure ShowError(const Msg: string);
    procedure ShowHint(const Msg: string);
  public
    procedure SetValue(const NewValue: string); override;
  end;

implementation

procedure TSafePropertyEditor.SetValue(const NewValue: string);  
var
  ErrorMsg: string;
  CorrectedValue: string;
begin
  if not ValidateValue(NewValue, ErrorMsg) then
  begin
    // Proposer une correction
    if MessageDlg('Erreur de validation',
                  ErrorMsg + LineEnding + LineEnding +
                  'Voulez-vous que je corrige automatiquement ?',
                  mtError, [mbYes, mbNo], 0) = mrYes then
    begin
      CorrectedValue := AutoCorrect(NewValue);
      if ValidateValue(CorrectedValue, ErrorMsg) then
        inherited SetValue(CorrectedValue)
      else
        ShowError('Impossible de corriger automatiquement');
    end;
  end
  else
    inherited SetValue(NewValue);
end;

procedure TSafePropertyEditor.ShowError(const Msg: string);  
begin
  MessageDlg('Erreur', Msg, mtError, [mbOK], 0);
end;

procedure TSafePropertyEditor.ShowHint(const Msg: string);  
begin
  MessageDlg('Conseil', Msg, mtInformation, [mbOK], 0);
end;
```

## Éditeurs multi-propriétés

### Éditer plusieurs propriétés en même temps

```pascal
unit MultiPropertyEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PropEdits, Forms, StdCtrls, Controls;

type
  // Éditeur qui modifie plusieurs propriétés liées
  TDimensionEditor = class(TPropertyEditor)
  private
    procedure UpdateLinkedProperties(NewWidth, NewHeight: Integer);
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
    function AllEqual: Boolean; override;
  end;

  TDimensionDialog = class(TForm)
  private
    FWidthEdit: TEdit;
    FHeightEdit: TEdit;
    FAspectRatio: TCheckBox;
    FOriginalWidth: Integer;
    FOriginalHeight: Integer;

    procedure AspectRatioChange(Sender: TObject);
    procedure WidthEditChange(Sender: TObject);
    procedure HeightEditChange(Sender: TObject);
  public
    constructor CreateNew(AOwner: TComponent; Dummy: Integer = 0); override;

    property WidthValue: Integer read GetWidth write SetWidth;
    property HeightValue: Integer read GetHeight write SetHeight;
  end;

implementation

function TDimensionEditor.GetAttributes: TPropertyAttributes;  
begin
  Result := [paDialog, paMultiSelect];
end;

procedure TDimensionEditor.Edit;  
var
  Dialog: TDimensionDialog;
  Component: TComponent;
  WidthProp, HeightProp: TPropertyEditor;
  i: Integer;
begin
  Dialog := TDimensionDialog.CreateNew(nil);
  try
    // Récupérer les valeurs actuelles
    Component := GetComponent(0) as TComponent;

    // Trouver les propriétés Width et Height
    WidthProp := GetPropertyEditor(Component, 'Width');
    HeightProp := GetPropertyEditor(Component, 'Height');

    if Assigned(WidthProp) and Assigned(HeightProp) then
    begin
      Dialog.WidthValue := WidthProp.GetOrdValue;
      Dialog.HeightValue := HeightProp.GetOrdValue;

      if Dialog.ShowModal = mrOK then
      begin
        // Appliquer à tous les composants sélectionnés
        for i := 0 to PropCount - 1 do
        begin
          Component := GetComponent(i) as TComponent;
          UpdateLinkedProperties(Dialog.WidthValue, Dialog.HeightValue);
        end;
        Modified;
      end;
    end;
  finally
    Dialog.Free;
  end;
end;

procedure TDimensionEditor.UpdateLinkedProperties(NewWidth, NewHeight: Integer);  
var
  Component: TComponent;
  WidthProp, HeightProp: TPropertyEditor;
begin
  Component := GetComponent(0) as TComponent;

  // Mettre à jour Width
  WidthProp := GetPropertyEditor(Component, 'Width');
  if Assigned(WidthProp) then
    WidthProp.SetOrdValue(NewWidth);

  // Mettre à jour Height
  HeightProp := GetPropertyEditor(Component, 'Height');
  if Assigned(HeightProp) then
    HeightProp.SetOrdValue(NewHeight);
end;

constructor TDimensionDialog.CreateNew(AOwner: TComponent; Dummy: Integer);  
begin
  inherited;

  Caption := 'Dimensions';
  Position := poScreenCenter;
  BorderStyle := bsDialog;
  Width := 300;
  Height := 200;

  // Champ largeur
  TLabel.Create(Self).SetBounds(10, 20, 80, 20);
  TLabel(Controls[Controls.Count-1]).Caption := 'Largeur:';
  TLabel(Controls[Controls.Count-1]).Parent := Self;

  FWidthEdit := TEdit.Create(Self);
  FWidthEdit.SetBounds(100, 20, 100, 25);
  FWidthEdit.Parent := Self;
  FWidthEdit.OnChange := @WidthEditChange;

  // Champ hauteur
  TLabel.Create(Self).SetBounds(10, 50, 80, 20);
  TLabel(Controls[Controls.Count-1]).Caption := 'Hauteur:';
  TLabel(Controls[Controls.Count-1]).Parent := Self;

  FHeightEdit := TEdit.Create(Self);
  FHeightEdit.SetBounds(100, 50, 100, 25);
  FHeightEdit.Parent := Self;
  FHeightEdit.OnChange := @HeightEditChange;

  // Case à cocher pour le ratio
  FAspectRatio := TCheckBox.Create(Self);
  FAspectRatio.SetBounds(10, 90, 200, 25);
  FAspectRatio.Caption := 'Conserver les proportions';
  FAspectRatio.Parent := Self;
  FAspectRatio.OnChange := @AspectRatioChange;
end;

procedure TDimensionDialog.AspectRatioChange(Sender: TObject);  
begin
  if FAspectRatio.Checked then
  begin
    FOriginalWidth := StrToIntDef(FWidthEdit.Text, 100);
    FOriginalHeight := StrToIntDef(FHeightEdit.Text, 100);
  end;
end;

procedure TDimensionDialog.WidthEditChange(Sender: TObject);  
var
  NewWidth, NewHeight: Integer;
begin
  if FAspectRatio.Checked and (FOriginalWidth > 0) then
  begin
    NewWidth := StrToIntDef(FWidthEdit.Text, FOriginalWidth);
    NewHeight := MulDiv(NewWidth, FOriginalHeight, FOriginalWidth);
    FHeightEdit.OnChange := nil;  // Éviter la récursion
    FHeightEdit.Text := IntToStr(NewHeight);
    FHeightEdit.OnChange := @HeightEditChange;
  end;
end;
```

## Intégration avec l'IDE

### Utiliser les services de l'IDE

```pascal
unit IDEIntegratedEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PropEdits, ComponentEditors,
  IDEIntf, ProjectIntf, LazIDEIntf;

type
  TIDEAwareEditor = class(TPropertyEditor)
  private
    procedure OpenRelatedFile;
    procedure CreateNewResource;
    function GetProjectPath: string;
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

implementation

function TIDEAwareEditor.GetProjectPath: string;  
var
  Project: TLazProject;
begin
  Result := '';
  Project := LazarusIDE.ActiveProject;
  if Assigned(Project) then
    Result := ExtractFilePath(Project.ProjectInfoFile);
end;

procedure TIDEAwareEditor.OpenRelatedFile;  
var
  FileName: string;
begin
  FileName := GetStrValue;

  if FileName <> '' then
  begin
    // Chemin relatif au projet
    if not FileIsAbsolute(FileName) then
      FileName := GetProjectPath + FileName;

    if FileExists(FileName) then
    begin
      // Ouvrir dans l'éditeur de l'IDE
      if LazarusIDE.DoOpenEditorFile(FileName, -1, -1, []) = mrOK then
        ShowMessage('Fichier ouvert dans l''éditeur')
      else
        ShowMessage('Impossible d''ouvrir le fichier');
    end
    else
    begin
      if MessageDlg('Fichier introuvable',
                    'Le fichier n''existe pas. Voulez-vous le créer ?',
                    mtConfirmation, [mbYes, mbNo], 0) = mrYes then
        CreateNewResource;
    end;
  end;
end;

procedure TIDEAwareEditor.CreateNewResource;  
var
  FileName: string;
  FileStream: TFileStream;
begin
  FileName := GetStrValue;

  if not FileIsAbsolute(FileName) then
    FileName := GetProjectPath + FileName;

  try
    FileStream := TFileStream.Create(FileName, fmCreate);
    try
      // Créer un fichier modèle selon l'extension
      if ExtractFileExt(FileName) = '.ini' then
        FileStream.WriteAnsiString('[Settings]' + LineEnding)
      else if ExtractFileExt(FileName) = '.xml' then
        FileStream.WriteAnsiString('<?xml version="1.0"?>' + LineEnding +
                                  '<root></root>');
    finally
      FileStream.Free;
    end;

    // Ouvrir le nouveau fichier
    LazarusIDE.DoOpenEditorFile(FileName, -1, -1, []);

    // Ajouter au projet
    LazarusIDE.ActiveProject.AddFile(FileName, False);

    ShowMessage('Fichier créé et ajouté au projet');
  except
    on E: Exception do
      ShowMessage('Erreur : ' + E.Message);
  end;
end;

procedure TIDEAwareEditor.Edit;  
begin
  OpenRelatedFile;
end;

function TIDEAwareEditor.GetAttributes: TPropertyAttributes;  
begin
  Result := [paDialog];
end;
```

### Éditeur avec aperçu en direct

```pascal
unit LivePreviewEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PropEdits, Forms, ExtCtrls, StdCtrls, Graphics;

type
  TFormulaPropertyEditor = class(TPropertyEditor)
  private
    FPreviewForm: TForm;
    FFormulaEdit: TEdit;
    FPreviewPanel: TPanel;
    FUpdateTimer: TTimer;

    procedure FormulaEditChange(Sender: TObject);
    procedure UpdatePreview(Sender: TObject);
    procedure DrawFormula(Canvas: TCanvas; const Formula: string);
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

implementation

uses
  Math;

procedure TFormulaPropertyEditor.Edit;  
begin
  FPreviewForm := TForm.Create(nil);
  try
    FPreviewForm.Caption := 'Éditeur de formule avec aperçu';
    FPreviewForm.Position := poScreenCenter;
    FPreviewForm.Width := 600;
    FPreviewForm.Height := 400;

    // Zone de saisie
    FFormulaEdit := TEdit.Create(FPreviewForm);
    FFormulaEdit.Parent := FPreviewForm;
    FFormulaEdit.SetBounds(10, 10, 580, 25);
    FFormulaEdit.Text := GetStrValue;
    FFormulaEdit.OnChange := @FormulaEditChange;

    // Zone de prévisualisation
    FPreviewPanel := TPanel.Create(FPreviewForm);
    FPreviewPanel.Parent := FPreviewForm;
    FPreviewPanel.SetBounds(10, 45, 580, 300);
    FPreviewPanel.BevelOuter := bvLowered;
    FPreviewPanel.Color := clWhite;
    FPreviewPanel.OnPaint := @PreviewPanelPaint;

    // Timer pour mise à jour différée
    FUpdateTimer := TTimer.Create(FPreviewForm);
    FUpdateTimer.Interval := 500;  // Attendre 500ms après la frappe
    FUpdateTimer.Enabled := False;
    FUpdateTimer.OnTimer := @UpdatePreview;

    // Boutons
    with TButton.Create(FPreviewForm) do
    begin
      Parent := FPreviewForm;
      SetBounds(430, 355, 75, 25);
      Caption := 'OK';
      ModalResult := mrOK;
    end;

    with TButton.Create(FPreviewForm) do
    begin
      Parent := FPreviewForm;
      SetBounds(515, 355, 75, 25);
      Caption := 'Annuler';
      ModalResult := mrCancel;
    end;

    // Affichage initial
    UpdatePreview(nil);

    if FPreviewForm.ShowModal = mrOK then
    begin
      SetStrValue(FFormulaEdit.Text);
      Modified;
    end;
  finally
    FPreviewForm.Free;
  end;
end;

procedure TFormulaPropertyEditor.FormulaEditChange(Sender: TObject);  
begin
  // Réinitialiser le timer à chaque frappe
  FUpdateTimer.Enabled := False;
  FUpdateTimer.Enabled := True;
end;

procedure TFormulaPropertyEditor.UpdatePreview(Sender: TObject);  
begin
  FUpdateTimer.Enabled := False;
  FPreviewPanel.Invalidate;
end;

procedure TFormulaPropertyEditor.DrawFormula(Canvas: TCanvas; const Formula: string);  
var
  x, y: Double;
  PixelX, PixelY: Integer;
  i: Integer;
  Error: Boolean;
begin
  Error := False;

  try
    // Dessiner les axes
    Canvas.Pen.Color := clGray;
    Canvas.Line(0, Canvas.Height div 2, Canvas.Width, Canvas.Height div 2);
    Canvas.Line(Canvas.Width div 2, 0, Canvas.Width div 2, Canvas.Height);

    // Dessiner la formule
    Canvas.Pen.Color := clBlue;
    Canvas.Pen.Width := 2;

    for i := 0 to Canvas.Width - 1 do
    begin
      x := (i - Canvas.Width / 2) / 50;  // Échelle

      // Évaluer la formule (simplifié pour l'exemple)
      // Dans un vrai éditeur, utilisez un parser d'expressions
      if Formula = 'sin(x)' then
        y := Sin(x)
      else if Formula = 'cos(x)' then
        y := Cos(x)
      else if Formula = 'x^2' then
        y := x * x
      else if Formula = 'sqrt(x)' then
        y := Sqrt(Abs(x))
      else
      begin
        Error := True;
        Break;
      end;

      PixelY := Round(Canvas.Height / 2 - y * 50);

      if i = 0 then
        Canvas.MoveTo(i, PixelY)
      else
        Canvas.LineTo(i, PixelY);
    end;

  except
    Error := True;
  end;

  if Error then
  begin
    Canvas.Font.Color := clRed;
    Canvas.TextOut(10, 10, 'Erreur dans la formule');
  end;
end;
```

## Éditeurs pour composants visuels

### Éditeur avec manipulation directe

```pascal
unit VisualEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComponentEditors, Controls, Graphics, Forms;

type
  // Éditeur qui permet de manipuler visuellement le composant
  TVisualComponentEditor = class(TComponentEditor)
  private
    FDesignPanel: TPanel;
    FHandles: array[0..7] of TShape;
    FDragging: Boolean;
    FDragHandle: Integer;

    procedure CreateHandles;
    procedure UpdateHandles;
    procedure HandleMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure HandleMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure HandleMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  public
    procedure Edit; override;
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

implementation

procedure TVisualComponentEditor.Edit;  
var
  EditForm: TForm;
  Control: TControl;
begin
  Control := Component as TControl;

  EditForm := TForm.Create(nil);
  try
    EditForm.Caption := 'Éditeur visuel - ' + Control.Name;
    EditForm.Position := poScreenCenter;
    EditForm.Width := 600;
    EditForm.Height := 400;

    // Panneau de conception
    FDesignPanel := TPanel.Create(EditForm);
    FDesignPanel.Parent := EditForm;
    FDesignPanel.Align := alClient;
    FDesignPanel.BevelOuter := bvLowered;
    FDesignPanel.Color := clWhite;

    // Créer les poignées de redimensionnement
    CreateHandles;
    UpdateHandles;

    // Événements pour le drag & drop
    FDesignPanel.OnMouseDown := @HandleMouseDown;
    FDesignPanel.OnMouseMove := @HandleMouseMove;
    FDesignPanel.OnMouseUp := @HandleMouseUp;

    if EditForm.ShowModal = mrOK then
    begin
      // Appliquer les modifications
      Designer.Modified;
    end;
  finally
    EditForm.Free;
  end;
end;

procedure TVisualComponentEditor.CreateHandles;  
var
  i: Integer;
begin
  for i := 0 to 7 do
  begin
    FHandles[i] := TShape.Create(FDesignPanel);
    FHandles[i].Parent := FDesignPanel;
    FHandles[i].Shape := stRectangle;
    FHandles[i].Brush.Color := clBlack;
    FHandles[i].Pen.Color := clWhite;
    FHandles[i].Width := 8;
    FHandles[i].Height := 8;
    FHandles[i].Cursor := crSizeAll;
    FHandles[i].Tag := i;
    FHandles[i].OnMouseDown := @HandleMouseDown;
  end;
end;

procedure TVisualComponentEditor.UpdateHandles;  
var
  R: TRect;
  Control: TControl;
begin
  Control := Component as TControl;
  R := Control.BoundsRect;

  // Positionner les 8 poignées
  FHandles[0].SetBounds(R.Left - 4, R.Top - 4, 8, 8);      // Haut-gauche
  FHandles[1].SetBounds(R.Left + (R.Right - R.Left) div 2 - 4, R.Top - 4, 8, 8); // Haut-centre
  FHandles[2].SetBounds(R.Right - 4, R.Top - 4, 8, 8);     // Haut-droit
  FHandles[3].SetBounds(R.Right - 4, R.Top + (R.Bottom - R.Top) div 2 - 4, 8, 8); // Milieu-droit
  FHandles[4].SetBounds(R.Right - 4, R.Bottom - 4, 8, 8);  // Bas-droit
  FHandles[5].SetBounds(R.Left + (R.Right - R.Left) div 2 - 4, R.Bottom - 4, 8, 8); // Bas-centre
  FHandles[6].SetBounds(R.Left - 4, R.Bottom - 4, 8, 8);   // Bas-gauche
  FHandles[7].SetBounds(R.Left - 4, R.Top + (R.Bottom - R.Top) div 2 - 4, 8, 8); // Milieu-gauche
end;

function TVisualComponentEditor.GetVerbCount: Integer;  
begin
  Result := 3;
end;

function TVisualComponentEditor.GetVerb(Index: Integer): string;  
begin
  case Index of
    0: Result := 'Éditer visuellement...';
    1: Result := 'Aligner au centre';
    2: Result := 'Réinitialiser la taille';
  end;
end;

procedure TVisualComponentEditor.ExecuteVerb(Index: Integer);  
var
  Control: TControl;
begin
  Control := Component as TControl;

  case Index of
    0: Edit;
    1: begin
         Control.Left := (Control.Parent.Width - Control.Width) div 2;
         Control.Top := (Control.Parent.Height - Control.Height) div 2;
         Designer.Modified;
       end;
    2: begin
         Control.Width := 100;
         Control.Height := 50;
         Designer.Modified;
       end;
  end;
end;
```

## Débogage des éditeurs

### Techniques de débogage pour les éditeurs

```pascal
unit DebugPropertyEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PropEdits, Dialogs;

type
  TDebugPropertyEditor = class(TPropertyEditor)
  private
    procedure LogDebug(const Msg: string);
    procedure ShowDebugInfo;
  protected
    procedure Initialize; override;
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure SetValue(const NewValue: string); override;
    procedure Edit; override;
  end;

implementation

procedure TDebugPropertyEditor.LogDebug(const Msg: string);  
var
  F: TextFile;
begin
  {$IFDEF DEBUG}
  AssignFile(F, 'PropertyEditor.log');
  if FileExists('PropertyEditor.log') then
    Append(F)
  else
    Rewrite(F);

  WriteLn(F, DateTimeToStr(Now) + ' - ' + Msg);
  CloseFile(F);
  {$ENDIF}
end;

procedure TDebugPropertyEditor.Initialize;  
begin
  inherited;
  LogDebug('Éditeur initialisé pour ' + GetPropInfo^.Name);
  LogDebug('Type: ' + GetPropType^.Name);
  LogDebug('Composant: ' + GetComponent(0).ClassName);
end;

function TDebugPropertyEditor.GetAttributes: TPropertyAttributes;  
begin
  Result := inherited GetAttributes;
  LogDebug('GetAttributes appelé, résultat: ' + SetToString(PTypeInfo(TypeInfo(TPropertyAttributes)), Integer(Result), True));
end;

function TDebugPropertyEditor.GetValue: string;  
begin
  Result := inherited GetValue;
  LogDebug('GetValue appelé, résultat: ' + Result);
end;

procedure TDebugPropertyEditor.SetValue(const NewValue: string);  
begin
  LogDebug('SetValue appelé avec: ' + NewValue);
  try
    inherited SetValue(NewValue);
    LogDebug('SetValue réussi');
  except
    on E: Exception do
    begin
      LogDebug('Erreur dans SetValue: ' + E.Message);
      raise;
    end;
  end;
end;

procedure TDebugPropertyEditor.Edit;  
begin
  LogDebug('Edit appelé');
  ShowDebugInfo;
end;

procedure TDebugPropertyEditor.ShowDebugInfo;  
var
  Info: TStringList;
  i: Integer;
begin
  Info := TStringList.Create;
  try
    Info.Add('=== Informations de débogage ===');
    Info.Add('');
    Info.Add('Propriété : ' + GetName);
    Info.Add('Type : ' + GetPropType^.Name);
    Info.Add('Valeur actuelle : ' + GetValue);
    Info.Add('');
    Info.Add('Composants sélectionnés : ' + IntToStr(PropCount));

    for i := 0 to PropCount - 1 do
      Info.Add('  - ' + GetComponent(i).Name + ' (' +
               GetComponent(i).ClassName + ')');

    Info.Add('');
    Info.Add('Attributs : ' + SetToString(
      PTypeInfo(TypeInfo(TPropertyAttributes)),
      Integer(GetAttributes), True));

    ShowMessage(Info.Text);
  finally
    Info.Free;
  end;
end;
```

## Conclusion et points clés

### Récapitulatif des concepts importants

1. **Types d'éditeurs** :
   - Éditeurs de propriétés : Pour une propriété spécifique
   - Éditeurs de composants : Pour le composant entier
   - Éditeurs inline : S'affichent dans la grille
   - Éditeurs avec dialogue : Ouvrent une fenêtre

2. **Méthodes essentielles** :
   - `GetAttributes` : Définit les capacités
   - `GetValue/SetValue` : Lecture/écriture de la valeur
   - `Edit` : Action du double-clic
   - `GetValues` : Liste de valeurs possibles

3. **Bonnes pratiques** :
   - Valider intelligemment les entrées
   - Fournir un feedback visuel
   - Gérer les erreurs gracieusement
   - Intégrer avec l'IDE

4. **Enregistrement** :
   - Utiliser `RegisterPropertyEditor` pour les propriétés
   - Utiliser `RegisterComponentEditor` pour les composants
   - Spécifier le type ou la classe cible

5. **Débogage** :
   - Logger les appels de méthodes
   - Vérifier les types et valeurs
   - Tester avec plusieurs composants sélectionnés

Les éditeurs de propriétés personnalisés transforment l'expérience de développement en rendant vos composants aussi professionnels et conviviaux que ceux fournis avec Lazarus. Avec ces techniques, vous pouvez créer des éditeurs qui non seulement facilitent la saisie des données, mais aussi préviennent les erreurs et guident les utilisateurs vers les bonnes pratiques !

⏭️ [Composants composites et frames](/04-framework-lcl/07-composants-composites-frames.md)
