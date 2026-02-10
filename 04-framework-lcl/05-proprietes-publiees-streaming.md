🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 4.5 Propriétés publiées et streaming

## Introduction : Qu'est-ce que le streaming ?

Imaginez que vous créez une belle interface dans Lazarus. Vous placez des boutons, définissez leurs couleurs, leurs tailles, leurs positions. Quand vous sauvegardez votre projet, toutes ces informations doivent être stockées quelque part. C'est là qu'intervient le **streaming**.

Le streaming est le processus qui permet de :
- **Sauvegarder** l'état de vos composants dans un fichier (.lfm pour Lazarus)
- **Recharger** cet état quand vous ouvrez votre projet
- **Créer** les composants à l'exécution avec toutes leurs propriétés

C'est comme prendre une photo de votre interface : le streaming capture tout et peut recréer exactement la même chose plus tard.

## Comprendre les propriétés publiées

### Les sections de visibilité

En Pascal, une classe peut avoir différentes sections de visibilité :

```pascal
type
  TMyComponent = class(TComponent)
  private
    // Visible uniquement dans cette unité
    FInternalData: string;

  protected
    // Visible dans cette classe et ses descendants
    procedure InternalMethod;

  public
    // Visible partout dans le code
    procedure DoSomething;
    property PublicProperty: string read FInternalData;

  published
    // MAGIQUE ! Visible dans l'inspecteur d'objets et sauvegardé
    property SavedProperty: string read FInternalData write SetInternalData;
  end;
```

La section **published** est spéciale : tout ce qui s'y trouve est :
- Visible dans l'inspecteur d'objets de Lazarus
- Automatiquement sauvegardé dans le fichier .lfm
- Rechargé quand vous ouvrez le projet

### Exemple concret : Un composant horloge

Créons un composant qui affiche l'heure pour comprendre les propriétés publiées :

```pascal
unit ClockLabel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, ExtCtrls, Graphics;

type
  TTimeFormat = (tf24Hour, tf12Hour, tfCustom);

  TClockLabel = class(TCustomLabel)
  private
    FTimer: TTimer;
    FTimeFormat: TTimeFormat;
    FCustomFormat: string;
    FShowSeconds: Boolean;
    FUpdateInterval: Integer;
    FPrefix: string;
    FSuffix: string;
    FActive: Boolean;

    procedure SetTimeFormat(AValue: TTimeFormat);
    procedure SetCustomFormat(const AValue: string);
    procedure SetShowSeconds(AValue: Boolean);
    procedure SetUpdateInterval(AValue: Integer);
    procedure SetPrefix(const AValue: string);
    procedure SetSuffix(const AValue: string);
    procedure SetActive(AValue: Boolean);

    procedure UpdateTime(Sender: TObject);
    function GetFormattedTime: string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // Méthode publique mais non sauvegardée
    procedure RefreshNow;

  published
    // Ces propriétés apparaissent dans l'inspecteur et sont sauvegardées
    property Active: Boolean read FActive write SetActive default True;
    property TimeFormat: TTimeFormat read FTimeFormat write SetTimeFormat default tf24Hour;
    property CustomFormat: string read FCustomFormat write SetCustomFormat;
    property ShowSeconds: Boolean read FShowSeconds write SetShowSeconds default True;
    property UpdateInterval: Integer read FUpdateInterval write SetUpdateInterval default 1000;
    property Prefix: string read FPrefix write SetPrefix;
    property Suffix: string read FSuffix write SetSuffix;

    // Propriétés héritées qu'on veut exposer
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property Color;
    property Font;
    property ParentColor;
    property ParentFont;
    property Visible;
    property OnClick;
    property OnDblClick;
  end;
```

### Implémentation avec gestion du streaming

```pascal
implementation

constructor TClockLabel.Create(AOwner: TComponent);  
begin
  inherited Create(AOwner);

  // Valeurs par défaut IMPORTANTES pour le streaming
  FActive := True;
  FTimeFormat := tf24Hour;
  FShowSeconds := True;
  FUpdateInterval := 1000;
  FCustomFormat := 'hh:nn:ss';
  FPrefix := '';
  FSuffix := '';

  // Créer le timer interne
  FTimer := TTimer.Create(Self);
  FTimer.Interval := FUpdateInterval;
  FTimer.OnTimer := @UpdateTime;
  FTimer.Enabled := FActive;

  // Mise à jour initiale
  UpdateTime(nil);
end;

destructor TClockLabel.Destroy;  
begin
  FTimer.Enabled := False;
  // FTimer est détruit automatiquement (Self est son propriétaire)
  inherited Destroy;
end;

procedure TClockLabel.SetActive(AValue: Boolean);  
begin
  if FActive <> AValue then
  begin
    FActive := AValue;
    FTimer.Enabled := FActive;
    if FActive then
      UpdateTime(nil);
  end;
end;

procedure TClockLabel.SetTimeFormat(AValue: TTimeFormat);  
begin
  if FTimeFormat <> AValue then
  begin
    FTimeFormat := AValue;
    UpdateTime(nil);
  end;
end;

procedure TClockLabel.SetUpdateInterval(AValue: Integer);  
begin
  if (AValue >= 100) and (FUpdateInterval <> AValue) then
  begin
    FUpdateInterval := AValue;
    FTimer.Interval := AValue;
  end;
end;

function TClockLabel.GetFormattedTime: string;  
var
  FormatStr: string;
begin
  case FTimeFormat of
    tf24Hour:
      if FShowSeconds then
        FormatStr := 'hh:nn:ss'
      else
        FormatStr := 'hh:nn';

    tf12Hour:
      if FShowSeconds then
        FormatStr := 'hh:nn:ss AM/PM'
      else
        FormatStr := 'hh:nn AM/PM';

    tfCustom:
      FormatStr := FCustomFormat;
  end;

  Result := FPrefix + FormatDateTime(FormatStr, Now) + FSuffix;
end;

procedure TClockLabel.UpdateTime(Sender: TObject);  
begin
  Caption := GetFormattedTime;
end;

// Les autres setters...
```

## Types de propriétés streamables

### Types simples automatiquement streamés

Ces types sont automatiquement gérés par le système de streaming :

```pascal
published
  // Types de base
  property IntegerProp: Integer read FInt write FInt;
  property BooleanProp: Boolean read FBool write FBool;
  property StringProp: string read FStr write FStr;
  property FloatProp: Double read FFloat write FFloat;

  // Énumérations
  property EnumProp: TAlignment read FAlign write FAlign;

  // Ensembles
  property SetProp: TAnchors read FAnchors write FAnchors;

  // Classes (avec certaines conditions)
  property FontProp: TFont read FFont write SetFont;
  property StringListProp: TStringList read FList write SetList;
end;
```

### Types qui nécessitent un traitement spécial

```pascal
type
  TComplexData = record
    X, Y: Integer;
    Name: string;
  end;

  TMyComponent = class(TComponent)
  private
    FComplexData: TComplexData;
    FBinaryData: TBytes;

    // Méthodes pour convertir en types streamables
    function GetComplexAsString: string;
    procedure SetComplexFromString(const Value: string);
  published
    // On ne peut pas publier directement TComplexData
    // Mais on peut publier une représentation string
    property ComplexData: string read GetComplexAsString write SetComplexFromString;
  end;

function TMyComponent.GetComplexAsString: string;  
begin
  // Convertir la structure en string
  Result := Format('%d,%d,%s', [FComplexData.X, FComplexData.Y, FComplexData.Name]);
end;

procedure TMyComponent.SetComplexFromString(const Value: string);  
var
  Parts: TStringList;
begin
  Parts := TStringList.Create;
  try
    Parts.Delimiter := ',';
    Parts.DelimitedText := Value;
    if Parts.Count >= 3 then
    begin
      FComplexData.X := StrToIntDef(Parts[0], 0);
      FComplexData.Y := StrToIntDef(Parts[1], 0);
      FComplexData.Name := Parts[2];
    end;
  finally
    Parts.Free;
  end;
end;
```

## Le processus de streaming en détail

### Comment Lazarus sauvegarde vos composants

Quand vous sauvegardez votre formulaire, Lazarus génère un fichier .lfm qui ressemble à :

```
object Form1: TForm1
  Left = 100
  Top = 50
  Width = 640
  Height = 480
  Caption = 'Mon Application'
  Color = clBtnFace
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'

  object ClockLabel1: TClockLabel
    Left = 10
    Top = 10
    Width = 100
    Height = 20
    Active = True
    TimeFormat = tf24Hour
    ShowSeconds = True
    Prefix = 'Heure: '
    Font.Style = [fsBold]
  end

  object Button1: TButton
    Left = 10
    Top = 40
    Width = 75
    Height = 25
    Caption = 'OK'
    OnClick = Button1Click
  end
end
```

### Le cycle de vie du streaming

```pascal
type
  TStreamableComponent = class(TComponent)
  protected
    // Appelé AVANT le streaming (sauvegarde)
    procedure DefineProperties(Filer: TFiler); override;

    // Appelé APRÈS le chargement depuis le stream
    procedure Loaded; override;

    // Pour la compatibilité ascendante
    procedure ReadState(Reader: TReader); override;
    procedure WriteState(Writer: TWriter); override;
  end;

procedure TStreamableComponent.Loaded;  
begin
  inherited Loaded;

  // Le composant est complètement chargé
  // C'est le bon moment pour :
  // - Initialiser des éléments qui dépendent d'autres propriétés
  // - Démarrer des timers
  // - Se connecter à des ressources

  if not (csDesigning in ComponentState) then
  begin
    // Code qui ne s'exécute qu'à l'exécution (pas en conception)
    ConnectToDatabase;
    StartBackgroundThread;
  end;
end;
```

## Propriétés avec valeurs par défaut

### L'importance des valeurs par défaut

Les valeurs par défaut sont cruciales pour l'efficacité du streaming :

```pascal
type
  TOptimizedComponent = class(TComponent)
  private
    FCount: Integer;
    FEnabled: Boolean;
    FText: string;
    FColor: TColor;
  published
    // Avec 'default', la propriété n'est sauvée que si différente de la valeur par défaut
    property Count: Integer read FCount write FCount default 0;
    property Enabled: Boolean read FEnabled write FEnabled default True;

    // Les strings n'ont pas de 'default' (toujours sauvegardés)
    property Text: string read FText write FText;

    // Pour les couleurs, utiliser les constantes
    property Color: TColor read FColor write FColor default clBtnFace;
  end;

constructor TOptimizedComponent.Create(AOwner: TComponent);  
begin
  inherited Create(AOwner);

  // CRUCIAL : Initialiser aux mêmes valeurs que 'default'
  FCount := 0;        // Correspond au default
  FEnabled := True;   // Correspond au default
  FText := '';        // Pas de default pour les strings
  FColor := clBtnFace; // Correspond au default
end;
```

### Propriétés sans valeur par défaut

```pascal
published
  // Ces propriétés sont TOUJOURS sauvegardées
  property AlwaysSaved: Integer read FValue write FValue nodefault;

  // Les objets n'ont jamais de valeur par défaut
  property Font: TFont read FFont write SetFont;

  // Les strings non plus
  property Caption: string read FCaption write FCaption;
```

## Propriétés stored : Contrôle fin du streaming

### Utilisation de la directive stored

La directive `stored` permet de contrôler si une propriété est sauvegardée :

```pascal
type
  TSmartComponent = class(TComponent)
  private
    FAutoSize: Boolean;
    FWidth: Integer;
    FHeight: Integer;
    FCustomData: string;
    FIsModified: Boolean;

    function IsCustomDataStored: Boolean;
    function IsSizeStored: Boolean;
  published
    // Toujours sauvegardé
    property AutoSize: Boolean read FAutoSize write FAutoSize;

    // Sauvegardé seulement si AutoSize = False
    property Width: Integer read FWidth write FWidth stored IsSizeStored;
    property Height: Integer read FHeight write FHeight stored IsSizeStored;

    // Sauvegardé seulement si modifié
    property CustomData: string read FCustomData write FCustomData stored IsCustomDataStored;

    // Jamais sauvegardé
    property RuntimeOnly: Boolean read FIsModified write FIsModified stored False;
  end;

function TSmartComponent.IsSizeStored: Boolean;  
begin
  // Ne sauvegarder les dimensions que si AutoSize est désactivé
  Result := not FAutoSize;
end;

function TSmartComponent.IsCustomDataStored: Boolean;  
begin
  // Ne sauvegarder que si non vide
  Result := FCustomData <> '';
end;
```

## DefineProperties : Streaming personnalisé

### Sauvegarder des données complexes

```pascal
type
  TPointList = array of TPoint;

  TDrawingComponent = class(TComponent)
  private
    FPoints: TPointList;
    FPenWidth: Integer;

    procedure ReadPoints(Reader: TReader);
    procedure WritePoints(Writer: TWriter);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  published
    property PenWidth: Integer read FPenWidth write FPenWidth default 1;
  end;

procedure TDrawingComponent.DefineProperties(Filer: TFiler);  
begin
  inherited DefineProperties(Filer);

  // Définir une propriété virtuelle pour les points
  Filer.DefineProperty('Points', @ReadPoints, @WritePoints, Length(FPoints) > 0);
end;

procedure TDrawingComponent.WritePoints(Writer: TWriter);  
var
  i: Integer;
begin
  Writer.WriteListBegin;
  for i := 0 to High(FPoints) do
  begin
    Writer.WriteInteger(FPoints[i].X);
    Writer.WriteInteger(FPoints[i].Y);
  end;
  Writer.WriteListEnd;
end;

procedure TDrawingComponent.ReadPoints(Reader: TReader);  
var
  i: Integer;
begin
  SetLength(FPoints, 0);
  Reader.ReadListBegin;
  while not Reader.EndOfList do
  begin
    i := Length(FPoints);
    SetLength(FPoints, i + 1);
    FPoints[i].X := Reader.ReadInteger;
    FPoints[i].Y := Reader.ReadInteger;
  end;
  Reader.ReadListEnd;
end;
```

### Streaming de données binaires

```pascal
type
  TImageComponent = class(TComponent)
  private
    FImageData: TMemoryStream;
    FImageFormat: string;

    procedure ReadImageData(Stream: TStream);
    procedure WriteImageData(Stream: TStream);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure LoadFromFile(const FileName: string);
  published
    property ImageFormat: string read FImageFormat write FImageFormat;
  end;

constructor TImageComponent.Create(AOwner: TComponent);  
begin
  inherited Create(AOwner);
  FImageData := TMemoryStream.Create;
end;

destructor TImageComponent.Destroy;  
begin
  FImageData.Free;
  inherited Destroy;
end;

procedure TImageComponent.DefineProperties(Filer: TFiler);  
begin
  inherited DefineProperties(Filer);

  // Propriété binaire pour les données d'image
  Filer.DefineBinaryProperty('ImageData', @ReadImageData, @WriteImageData,
                             FImageData.Size > 0);
end;

procedure TImageComponent.WriteImageData(Stream: TStream);  
begin
  FImageData.Position := 0;
  Stream.CopyFrom(FImageData, FImageData.Size);
end;

procedure TImageComponent.ReadImageData(Stream: TStream);  
begin
  FImageData.Clear;
  FImageData.CopyFrom(Stream, Stream.Size - Stream.Position);
end;
```

## Compatibilité et versioning

### Gérer l'évolution des composants

```pascal
type
  TVersionedComponent = class(TComponent)
  private
    FVersion: Integer;
    FNewProperty: string;
    FOldProperty: string; // Obsolète mais gardée pour compatibilité

    procedure ReadOldProperty(Reader: TReader);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadState(Reader: TReader); override;
  published
    property Version: Integer read FVersion write FVersion default 2;
    property NewProperty: string read FNewProperty write FNewProperty;
    // OldProperty n'est plus publiée mais peut être lue
  end;

procedure TVersionedComponent.DefineProperties(Filer: TFiler);  
begin
  inherited DefineProperties(Filer);

  // Lire l'ancienne propriété si elle existe (compatibilité)
  Filer.DefineProperty('OldProperty', @ReadOldProperty, nil, False);
end;

procedure TVersionedComponent.ReadOldProperty(Reader: TReader);  
begin
  FOldProperty := Reader.ReadString;
  // Convertir vers la nouvelle propriété
  if FNewProperty = '' then
    FNewProperty := FOldProperty;
end;

procedure TVersionedComponent.ReadState(Reader: TReader);  
begin
  // Détecter la version avant de lire
  FVersion := 1; // Version par défaut pour anciens fichiers

  inherited ReadState(Reader);

  // Migration si nécessaire
  if FVersion = 1 then
  begin
    // Migrer les anciennes données
    FNewProperty := UpgradeOldFormat(FOldProperty);
    FVersion := 2;
  end;
end;
```

## Collections et propriétés complexes

### Créer une propriété collection

```pascal
type
  // Un élément de la collection
  TChartPoint = class(TCollectionItem)
  private
    FX: Double;
    FY: Double;
    FLabel: string;
    FColor: TColor;
  published
    property X: Double read FX write FX;
    property Y: Double read FY write FY;
    property Label_: string read FLabel write FLabel;
    property Color: TColor read FColor write FColor default clBlack;
  end;

  // La collection
  TChartPoints = class(TCollection)
  private
    FOwner: TComponent;
    function GetItem(Index: Integer): TChartPoint;
    procedure SetItem(Index: Integer; Value: TChartPoint);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TComponent);
    function Add: TChartPoint;
    property Items[Index: Integer]: TChartPoint read GetItem write SetItem; default;
  end;

  // Le composant qui utilise la collection
  TChartComponent = class(TComponent)
  private
    FPoints: TChartPoints;
    procedure SetPoints(Value: TChartPoints);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Points: TChartPoints read FPoints write SetPoints;
  end;

// Implémentation de la collection
constructor TChartPoints.Create(AOwner: TComponent);  
begin
  inherited Create(TChartPoint);
  FOwner := AOwner;
end;

function TChartPoints.GetOwner: TPersistent;  
begin
  Result := FOwner;
end;

function TChartPoints.Add: TChartPoint;  
begin
  Result := TChartPoint(inherited Add);
end;

// Implémentation du composant
constructor TChartComponent.Create(AOwner: TComponent);  
begin
  inherited Create(AOwner);
  FPoints := TChartPoints.Create(Self);
end;

destructor TChartComponent.Destroy;  
begin
  FPoints.Free;
  inherited Destroy;
end;

procedure TChartComponent.SetPoints(Value: TChartPoints);  
begin
  FPoints.Assign(Value);
end;
```

## Streaming et événements

### Gérer les références aux méthodes

```pascal
type
  TEventComponent = class(TComponent)
  private
    FOnChange: TNotifyEvent;
    FOnCalculate: TNotifyEvent;
    FAutoTrigger: Boolean;
  protected
    procedure Loaded; override;
  published
    // Les événements sont automatiquement streamés
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnCalculate: TNotifyEvent read FOnCalculate write FOnCalculate;
    property AutoTrigger: Boolean read FAutoTrigger write FAutoTrigger default False;
  end;

procedure TEventComponent.Loaded;  
begin
  inherited Loaded;

  // Après le chargement, on peut utiliser les événements
  if FAutoTrigger and Assigned(FOnChange) then
  begin
    // Mais attention : en conception, les méthodes n'existent pas !
    if not (csDesigning in ComponentState) then
      FOnChange(Self);
  end;
end;
```

## Optimisation du streaming

### Réduire la taille des fichiers .lfm

```pascal
type
  TOptimizedComponent = class(TComponent)
  private
    // Utiliser des types plus petits quand possible
    FFlags: Byte; // Au lieu de plusieurs Boolean

    function GetFlag1: Boolean;
    procedure SetFlag1(Value: Boolean);
    function GetFlag2: Boolean;
    procedure SetFlag2(Value: Boolean);
  published
    // Exposer comme propriétés séparées
    property Flag1: Boolean read GetFlag1 write SetFlag1 stored True;
    property Flag2: Boolean read GetFlag2 write SetFlag2 stored True;
  end;

function TOptimizedComponent.GetFlag1: Boolean;  
begin
  Result := (FFlags and 1) <> 0;
end;

procedure TOptimizedComponent.SetFlag1(Value: Boolean);  
begin
  if Value then
    FFlags := FFlags or 1
  else
    FFlags := FFlags and not 1;
end;
```

### Lazy loading pour les ressources lourdes

```pascal
type
  TResourceComponent = class(TComponent)
  private
    FImagePath: string;
    FImage: TBitmap;
    FImageLoaded: Boolean;

    function GetImage: TBitmap;
    procedure SetImagePath(const Value: string);
  protected
    procedure Loaded; override;
  public
    destructor Destroy; override;

    // L'image n'est pas streamée, seulement son chemin
    property Image: TBitmap read GetImage;
  published
    // Seul le chemin est sauvegardé
    property ImagePath: string read FImagePath write SetImagePath;
  end;

function TResourceComponent.GetImage: TBitmap;  
begin
  if not FImageLoaded and (FImagePath <> '') then
  begin
    if not Assigned(FImage) then
      FImage := TBitmap.Create;

    if FileExists(FImagePath) then
    begin
      FImage.LoadFromFile(FImagePath);
      FImageLoaded := True;
    end;
  end;
  Result := FImage;
end;

procedure TResourceComponent.Loaded;  
begin
  inherited Loaded;

  // En conception, ne pas charger les ressources lourdes
  if csDesigning in ComponentState then
    Exit;

  // Charger si nécessaire à l'exécution
  if FImagePath <> '' then
    GetImage; // Déclenche le chargement lazy
end;
```

## Débogage du streaming

### Tracer le processus de streaming

```pascal
type
  TDebugComponent = class(TComponent)
  private
    FValue: Integer;
  protected
    procedure ReadState(Reader: TReader); override;
    procedure WriteState(Writer: TWriter); override;
    procedure Loaded; override;
  published
    property Value: Integer read FValue write FValue;
  end;

procedure TDebugComponent.ReadState(Reader: TReader);  
begin
  {$IFDEF DEBUG}
  WriteLn('ReadState: Début de lecture');
  {$ENDIF}

  inherited ReadState(Reader);

  {$IFDEF DEBUG}
  WriteLn('ReadState: Value = ', FValue);
  {$ENDIF}
end;

procedure TDebugComponent.WriteState(Writer: TWriter);  
begin
  {$IFDEF DEBUG}
  WriteLn('WriteState: Début d''écriture');
  WriteLn('WriteState: Value = ', FValue);
  {$ENDIF}

  inherited WriteState(Writer);
end;

procedure TDebugComponent.Loaded;  
begin
  {$IFDEF DEBUG}
  WriteLn('Loaded: Composant complètement chargé');
  {$ENDIF}

  inherited Loaded;
end;
```

## Points clés à retenir

1. **Published = Visible + Sauvegardé** : Tout ce qui est dans la section published apparaît dans l'inspecteur et est automatiquement streamé

2. **Valeurs par défaut** : Utilisez la directive `default` pour optimiser la taille des fichiers .lfm

3. **Initialisation cohérente** : Les valeurs dans le constructeur doivent correspondre aux valeurs `default`

4. **Types streamables** : Tous les types ne peuvent pas être publiés directement

5. **DefineProperties** : Pour un contrôle total sur ce qui est sauvegardé et comment

6. **Stored** : Permet de contrôler finement quand une propriété est sauvegardée

7. **Loaded** : Le bon endroit pour initialiser après le chargement

8. **Collections** : Utilisez TCollection pour les propriétés avec plusieurs éléments

9. **Compatibilité** : DefineProperties permet de gérer les anciennes versions

10. **Performance** : Évitez de streamer les données volumineuses, utilisez des références

Le streaming est la magie qui permet à Lazarus de sauvegarder et restaurer vos interfaces. En maîtrisant ces concepts, vous pouvez créer des composants professionnels qui s'intègrent parfaitement dans l'IDE et offrent une expérience de développement fluide à leurs utilisateurs !

⏭️ [Éditeurs de propriétés personnalisés](/04-framework-lcl/06-editeurs-proprietes-personnalises.md)
