🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 13.9 Optimisation pour appareils mobiles

## Introduction

L'optimisation pour appareils mobiles est cruciale lors du développement d'applications FreePascal destinées à Android ou iOS. Les appareils mobiles présentent des contraintes spécifiques : ressources limitées (batterie, mémoire, CPU), connectivité variable, écrans tactiles de différentes tailles, et des attentes utilisateur élevées en termes de fluidité.

Ce chapitre explore les techniques d'optimisation pour créer des applications mobiles performantes, réactives et économes en ressources avec FreePascal.

## Contraintes des appareils mobiles

### Comparaison Desktop vs Mobile

| Aspect | Desktop | Mobile | Impact |
|--------|---------|--------|--------|
| **CPU** | Puissant, refroidissement actif | Limité, throttling thermique | Optimisation calculs |
| **RAM** | 8-32 GB typique | 2-8 GB typique | Gestion mémoire stricte |
| **Batterie** | Illimitée (secteur) | Limitée (3000-5000 mAh) | Économie d'énergie critique |
| **Écran** | Grand, haute résolution | Petit, tactile, diverses résolutions | UI adaptative |
| **Stockage** | SSD rapide | eMMC/UFS plus lent | I/O optimisées |
| **Réseau** | Filaire/WiFi stable | 4G/5G/WiFi variable | Gestion déconnexion |
| **Capteurs** | Limités | Nombreux (GPS, accéléromètre...) | Exploitation intelligente |

### Limitations typiques

**Mémoire** :
- Applications en arrière-plan peuvent être tuées par l'OS
- Limite de mémoire par application (256-512 MB sur Android, moins sur iOS)
- Pas de swap ou swap très limité

**Processeur** :
- Throttling thermique après quelques secondes de charge intensive
- Économie d'énergie : réduction de fréquence automatique
- Cœurs hétérogènes (big.LITTLE sur ARM)

**Batterie** :
- Consommation élevée = mauvaises notes sur les stores
- Wake locks et services en arrière-plan surveillés par l'OS
- Localisation GPS très énergivore

## Optimisation de la mémoire

### Gestion automatique vs manuelle

FreePascal offre plusieurs modes de gestion mémoire :

```pascal
program MemoryManagement;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes;

// Utiliser des objets légers quand possible
type
  // ❌ LOURD - Hérite de TObject, overhead important
  THeavyData = class
  private
    FValue: Integer;
  public
    property Value: Integer read FValue write FValue;
  end;

  // ✅ LÉGER - Record, allocation sur la pile
  TLightData = record
    Value: Integer;
  end;

var
  // Pour les données temporaires, préférer les records
  lightData: TLightData;

  // Pour les objets complexes avec méthodes, utiliser les classes
  heavyData: THeavyData;

begin
  // Record : allocation automatique sur la pile
  lightData.Value := 42;
  WriteLn('Light data: ', lightData.Value);
  // Pas de libération nécessaire

  // Classe : allocation manuelle sur le tas
  heavyData := THeavyData.Create;
  try
    heavyData.Value := 42;
    WriteLn('Heavy data: ', heavyData.Value);
  finally
    heavyData.Free;  // Libération manuelle obligatoire
  end;
end.
```

### Object pools (pools d'objets)

Réutiliser les objets plutôt que de les créer/détruire constamment :

```pascal
unit ObjectPool;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections;

type
  generic TObjectPool<T: class> = class
  private
    type TList = specialize TObjectList<T>;
  private
    FAvailable: TList;
    FInUse: TList;
    FCreateFunc: function: T;
    FMaxSize: Integer;
  public
    constructor Create(CreateFunc: function: T; MaxSize: Integer = 50);
    destructor Destroy; override;

    function Acquire: T;
    procedure Release(Obj: T);
    procedure Clear;

    property AvailableCount: Integer read GetAvailableCount;
    property InUseCount: Integer read GetInUseCount;
  end;

implementation

constructor TObjectPool.Create(CreateFunc: function: T; MaxSize: Integer);  
begin
  inherited Create;
  FCreateFunc := CreateFunc;
  FMaxSize := MaxSize;
  FAvailable := TList.Create(True);  // Possède les objets
  FInUse := TList.Create(False);     // Ne possède pas les objets
end;

destructor TObjectPool.Destroy;  
begin
  Clear;
  FAvailable.Free;
  FInUse.Free;
  inherited Destroy;
end;

function TObjectPool.Acquire: T;  
begin
  if FAvailable.Count > 0 then
  begin
    // Réutiliser un objet existant
    Result := FAvailable[FAvailable.Count - 1];
    FAvailable.Delete(FAvailable.Count - 1);
  end
  else
  begin
    // Créer un nouvel objet
    Result := FCreateFunc();
  end;

  FInUse.Add(Result);
end;

procedure TObjectPool.Release(Obj: T);  
begin
  if FInUse.Remove(Obj) >= 0 then
  begin
    if FAvailable.Count < FMaxSize then
      FAvailable.Add(Obj)
    else
      Obj.Free;  // Pool plein, détruire l'objet
  end;
end;

procedure TObjectPool.Clear;  
begin
  FAvailable.Clear;
  FInUse.Clear;
end;

end.
```

**Utilisation** :

```pascal
program UseObjectPool;

{$mode objfpc}{$H+}

uses
  ObjectPool;

type
  TMessage = class
    Text: string;
    Timestamp: TDateTime;
  end;

  TMessagePool = specialize TObjectPool<TMessage>;

function CreateMessage: TMessage;  
begin
  Result := TMessage.Create;
end;

var
  pool: TMessagePool;
  msg: TMessage;
  i: Integer;

begin
  // Créer le pool
  pool := TMessagePool.Create(@CreateMessage, 100);
  try
    // Utiliser le pool
    for i := 1 to 1000 do
    begin
      // Acquérir un objet du pool
      msg := pool.Acquire;
      try
        msg.Text := 'Message ' + IntToStr(i);
        msg.Timestamp := Now;

        // Traiter le message
        WriteLn(msg.Text);
      finally
        // Remettre l'objet dans le pool
        pool.Release(msg);
      end;
    end;

    WriteLn('Pool - Disponibles: ', pool.AvailableCount,
            ', En cours: ', pool.InUseCount);
  finally
    pool.Free;
  end;
end.
```

### Gestion des chaînes de caractères

Les chaînes sont souvent la source principale de consommation mémoire :

```pascal
program StringOptimization;

{$mode objfpc}{$H+}

uses
  SysUtils;

// ❌ MAUVAIS - Concaténation répétée (O(n²))
procedure BadConcat;  
var
  result: string;
  i: Integer;
begin
  result := '';
  for i := 1 to 1000 do
    result := result + IntToStr(i) + ',';  // Réallocation à chaque itération
end;

// ✅ BON - Utiliser TStringBuilder (O(n))
procedure GoodConcat;  
var
  sb: TStringBuilder;
  i: Integer;
begin
  sb := TStringBuilder.Create;
  try
    sb.Capacity := 5000;  // Pré-allouer la capacité

    for i := 1 to 1000 do
    begin
      sb.Append(IntToStr(i));
      sb.Append(',');
    end;

    WriteLn(sb.ToString);
  finally
    sb.Free;
  end;
end;

// ✅ MEILLEUR - Éviter les allocations inutiles
procedure BestApproach;  
const
  BUFFER_SIZE = 5000;
var
  buffer: array[0..BUFFER_SIZE-1] of Char;
  position: Integer;
  i: Integer;
  numStr: string;
begin
  position := 0;

  for i := 1 to 1000 do
  begin
    numStr := IntToStr(i);
    Move(numStr[1], buffer[position], Length(numStr));
    Inc(position, Length(numStr));

    buffer[position] := ',';
    Inc(position);
  end;

  WriteLn(buffer);  // Utiliser directement le buffer
end;

begin
  WriteLn('Test des méthodes de concaténation');
  GoodConcat;
end.
```

### Détection des fuites mémoire

```pascal
program MemoryLeakDetection;

{$mode objfpc}{$H+}

uses
  {$IFDEF DEBUG}
  HeapTrc,  // Activer le traçage du tas en mode debug
  {$ENDIF}
  SysUtils, Classes;

type
  TDataHolder = class
  private
    FData: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
  end;

constructor TDataHolder.Create;  
begin
  inherited Create;
  FData := TStringList.Create;
end;

destructor TDataHolder.Destroy;  
begin
  FData.Free;  // Important : libérer les ressources
  inherited Destroy;
end;

var
  holder: TDataHolder;

begin
  {$IFDEF DEBUG}
  // Configurer HeapTrc
  SetHeapTraceOutput('heaptrc.log');
  {$ENDIF}

  holder := TDataHolder.Create;
  try
    holder.FData.Add('Test');
    WriteLn('Données créées');
  finally
    holder.Free;
  end;

  WriteLn('Programme terminé');
  // HeapTrc affichera un rapport des fuites mémoire dans heaptrc.log
end.
```

## Optimisation du CPU

### Calculs asynchrones et threads

Ne bloquez jamais l'interface utilisateur :

```pascal
unit AsyncTask;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TProgressEvent = procedure(Progress: Integer) of object;
  TCompleteEvent = procedure(Result: string) of object;

  TAsyncTask = class(TThread)
  private
    FProgress: Integer;
    FResult: string;
    FOnProgress: TProgressEvent;
    FOnComplete: TCompleteEvent;

    procedure DoProgress;
    procedure DoComplete;
  protected
    procedure Execute; override;
  public
    constructor Create;

    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
    property OnComplete: TCompleteEvent read FOnComplete write FOnComplete;
  end;

implementation

constructor TAsyncTask.Create;  
begin
  inherited Create(True);  // Créer suspendu
  FreeOnTerminate := True;
end;

procedure TAsyncTask.DoProgress;  
begin
  if Assigned(FOnProgress) then
    FOnProgress(FProgress);
end;

procedure TAsyncTask.DoComplete;  
begin
  if Assigned(FOnComplete) then
    FOnComplete(FResult);
end;

procedure TAsyncTask.Execute;  
var
  i: Integer;
begin
  // Simuler un travail long
  for i := 1 to 100 do
  begin
    if Terminated then Exit;

    // Travail intensif
    Sleep(50);  // Remplacer par vraie tâche

    // Mettre à jour la progression
    FProgress := i;
    Synchronize(@DoProgress);
  end;

  // Résultat final
  FResult := 'Tâche terminée avec succès';
  Synchronize(@DoComplete);
end;

end.
```

**Utilisation dans une application mobile** :

```pascal
program MobileAsyncExample;

{$mode objfpc}{$H+}

uses
  AsyncTask, Forms, StdCtrls, ComCtrls;

type
  TMainForm = class(TForm)
    btnStart: TButton;
    ProgressBar: TProgressBar;
    lblStatus: TLabel;

    procedure btnStartClick(Sender: TObject);
    procedure OnTaskProgress(Progress: Integer);
    procedure OnTaskComplete(Result: string);
  end;

procedure TMainForm.btnStartClick(Sender: TObject);  
var
  task: TAsyncTask;
begin
  btnStart.Enabled := False;
  lblStatus.Caption := 'Traitement en cours...';

  task := TAsyncTask.Create;
  task.OnProgress := @OnTaskProgress;
  task.OnComplete := @OnTaskComplete;
  task.Start;
end;

procedure TMainForm.OnTaskProgress(Progress: Integer);  
begin
  ProgressBar.Position := Progress;
  Application.ProcessMessages;  // Garder l'UI réactive
end;

procedure TMainForm.OnTaskComplete(Result: string);  
begin
  lblStatus.Caption := Result;
  btnStart.Enabled := True;
  ProgressBar.Position := 0;
end;

var
  MainForm: TMainForm;

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
```

### Optimisation des boucles

```pascal
program LoopOptimization;

{$mode objfpc}{$H+}

uses
  SysUtils;

type
  TDataArray = array[0..9999] of Integer;

var
  data: TDataArray;
  i: Integer;

// ❌ LENT - Accès répété aux propriétés
procedure SlowLoop;  
var
  i: Integer;
  sum: Integer;
begin
  sum := 0;
  for i := 0 to High(data) do
    sum := sum + data[i];  // OK, mais peut être optimisé
end;

// ✅ RAPIDE - Utiliser des variables locales
procedure FastLoop;  
var
  i, sum, len: Integer;
  localData: ^TDataArray;
begin
  sum := 0;
  len := High(data);
  localData := @data;

  for i := 0 to len do
    sum := sum + localData^[i];
end;

// ✅ PLUS RAPIDE - Déroulement de boucle (loop unrolling)
procedure FasterLoop;  
var
  i, sum: Integer;
begin
  sum := 0;
  i := 0;

  // Traiter 4 éléments à la fois
  while i <= High(data) - 3 do
  begin
    sum := sum + data[i] + data[i+1] + data[i+2] + data[i+3];
    Inc(i, 4);
  end;

  // Traiter les éléments restants
  while i <= High(data) do
  begin
    sum := sum + data[i];
    Inc(i);
  end;
end;

begin
  // Initialiser les données
  for i := 0 to High(data) do
    data[i] := i;

  WriteLn('Optimisation de boucles');
  FastLoop;
end.
```

### Lazy loading (chargement paresseux)

Ne chargez que ce qui est nécessaire, quand c'est nécessaire :

```pascal
unit LazyImage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics;

type
  TLazyImage = class
  private
    FFilename: string;
    FBitmap: TBitmap;
    FLoaded: Boolean;

    procedure EnsureLoaded;
  public
    constructor Create(const Filename: string);
    destructor Destroy; override;

    function GetBitmap: TBitmap;
    procedure Unload;  // Libérer la mémoire si nécessaire

    property Loaded: Boolean read FLoaded;
  end;

implementation

constructor TLazyImage.Create(const Filename: string);  
begin
  inherited Create;
  FFilename := Filename;
  FBitmap := nil;
  FLoaded := False;
end;

destructor TLazyImage.Destroy;  
begin
  if Assigned(FBitmap) then
    FBitmap.Free;
  inherited Destroy;
end;

procedure TLazyImage.EnsureLoaded;  
begin
  if not FLoaded then
  begin
    FBitmap := TBitmap.Create;
    try
      FBitmap.LoadFromFile(FFilename);
      FLoaded := True;
    except
      on E: Exception do
      begin
        FBitmap.Free;
        FBitmap := nil;
        raise;
      end;
    end;
  end;
end;

function TLazyImage.GetBitmap: TBitmap;  
begin
  EnsureLoaded;
  Result := FBitmap;
end;

procedure TLazyImage.Unload;  
begin
  if Assigned(FBitmap) then
  begin
    FBitmap.Free;
    FBitmap := nil;
    FLoaded := False;
  end;
end;

end.
```

## Optimisation de la batterie

### Éviter les wake locks inutiles

```pascal
unit PowerManagement;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TPowerManager = class
  private
    FWakeLockActive: Boolean;
  public
    procedure AcquireWakeLock;
    procedure ReleaseWakeLock;

    property WakeLockActive: Boolean read FWakeLockActive;
  end;

implementation

{$IFDEF ANDROID}
uses
  jni, AndroidAPI;
{$ENDIF}

procedure TPowerManager.AcquireWakeLock;  
begin
  if not FWakeLockActive then
  begin
    {$IFDEF ANDROID}
    // Code JNI pour acquérir un wake lock Android
    // PowerManager pm = (PowerManager) getSystemService(POWER_SERVICE);
    // wakeLock = pm.newWakeLock(PowerManager.PARTIAL_WAKE_LOCK, "MyWakeLock");
    // wakeLock.acquire();
    {$ENDIF}

    FWakeLockActive := True;
  end;
end;

procedure TPowerManager.ReleaseWakeLock;  
begin
  if FWakeLockActive then
  begin
    {$IFDEF ANDROID}
    // Code JNI pour libérer le wake lock
    // if (wakeLock != null && wakeLock.isHeld()) {
    //     wakeLock.release();
    // }
    {$ENDIF}

    FWakeLockActive := False;
  end;
end;

end.
```

**Utilisation** :

```pascal
program BatteryOptimizedApp;

{$mode objfpc}{$H+}

uses
  PowerManagement;

var
  powerMgr: TPowerManager;

procedure DoHeavyWork;  
begin
  // Acquérir le wake lock seulement pendant le travail
  powerMgr.AcquireWakeLock;
  try
    // Travail intensif qui nécessite le CPU actif
    WriteLn('Traitement en cours...');
    Sleep(5000);
  finally
    // Toujours libérer le wake lock
    powerMgr.ReleaseWakeLock;
  end;
end;

begin
  powerMgr := TPowerManager.Create;
  try
    DoHeavyWork;

    // Le reste du temps, l'appareil peut se mettre en veille
    WriteLn('Travail terminé, l''appareil peut se mettre en veille');
  finally
    powerMgr.Free;
  end;
end.
```

### Optimisation de la localisation GPS

Le GPS est l'un des composants les plus énergivores :

```pascal
unit LocationOptimizer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TLocationAccuracy = (laLow, laMedium, laHigh);
  TLocationUpdate = procedure(Lat, Lon: Double) of object;

  TLocationManager = class
  private
    FAccuracy: TLocationAccuracy;
    FUpdateInterval: Integer;  // en millisecondes
    FOnUpdate: TLocationUpdate;
    FActive: Boolean;
  public
    constructor Create;

    procedure StartUpdates(Accuracy: TLocationAccuracy; IntervalMs: Integer);
    procedure StopUpdates;

    property OnUpdate: TLocationUpdate read FOnUpdate write FOnUpdate;
    property Active: Boolean read FActive;
  end;

implementation

constructor TLocationManager.Create;  
begin
  inherited Create;
  FActive := False;
end;

procedure TLocationManager.StartUpdates(Accuracy: TLocationAccuracy; IntervalMs: Integer);  
begin
  FAccuracy := Accuracy;
  FUpdateInterval := IntervalMs;

  {$IFDEF ANDROID}
  // Configurer le LocationManager Android selon la précision
  case FAccuracy of
    laLow:    // PRIORITY_LOW_POWER (économie d'énergie)
      WriteLn('GPS: Mode économie d''énergie');
    laMedium: // PRIORITY_BALANCED_POWER_ACCURACY
      WriteLn('GPS: Mode équilibré');
    laHigh:   // PRIORITY_HIGH_ACCURACY
      WriteLn('GPS: Mode haute précision');
  end;
  {$ENDIF}

  FActive := True;
end;

procedure TLocationManager.StopUpdates;  
begin
  if FActive then
  begin
    {$IFDEF ANDROID}
    // Arrêter les mises à jour GPS
    {$ENDIF}

    FActive := False;
  end;
end;

end.
```

**Exemple d'utilisation intelligente** :

```pascal
program SmartGPS;

{$mode objfpc}{$H+}

uses
  LocationOptimizer, SysUtils;

var
  locMgr: TLocationManager;
  lastUpdateTime: TDateTime;

procedure OnLocationUpdate(Lat, Lon: Double);  
begin
  WriteLn(Format('Position: %.6f, %.6f', [Lat, Lon]));
  lastUpdateTime := Now;
end;

procedure OptimizedTracking;  
begin
  locMgr := TLocationManager.Create;
  try
    locMgr.OnUpdate := @OnLocationUpdate;

    // Utiliser la précision minimale nécessaire
    // ❌ Éviter : laHigh en permanence
    // ✅ Préférer : laMedium ou laLow quand possible

    // Exemple : navigation active
    WriteLn('Navigation active - GPS haute précision');
    locMgr.StartUpdates(laHigh, 1000);  // 1 seconde
    Sleep(10000);  // Simuler 10s de navigation

    // Passage en arrière-plan : réduire la précision
    WriteLn('Application en arrière-plan - GPS économique');
    locMgr.StopUpdates;
    locMgr.StartUpdates(laLow, 60000);  // 1 minute
    Sleep(5000);

    // Arrêt complet quand inutile
    WriteLn('GPS désactivé');
    locMgr.StopUpdates;

  finally
    locMgr.Free;
  end;
end;

begin
  OptimizedTracking;
end.
```

### Batch processing (traitement par lots)

Regroupez les opérations pour minimiser les réveils du CPU :

```pascal
unit BatchProcessor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections;

type
  TBatchItem = record
    Data: string;
    Timestamp: TDateTime;
  end;

  TBatchList = specialize TList<TBatchItem>;

  TBatchProcessor = class
  private
    FQueue: TBatchList;
    FBatchSize: Integer;
    FMaxWaitTime: Integer;  // en millisecondes
    FLastProcessTime: TDateTime;

    procedure ProcessBatch;
  public
    constructor Create(BatchSize: Integer = 50; MaxWaitMs: Integer = 30000);
    destructor Destroy; override;

    procedure Add(const Data: string);
    procedure Flush;  // Forcer le traitement immédiat
  end;

implementation

constructor TBatchProcessor.Create(BatchSize: Integer; MaxWaitMs: Integer);  
begin
  inherited Create;
  FQueue := TBatchList.Create;
  FBatchSize := BatchSize;
  FMaxWaitTime := MaxWaitMs;
  FLastProcessTime := Now;
end;

destructor TBatchProcessor.Destroy;  
begin
  Flush;  // Traiter les éléments restants
  FQueue.Free;
  inherited Destroy;
end;

procedure TBatchProcessor.ProcessBatch;  
var
  i: Integer;
  item: TBatchItem;
begin
  if FQueue.Count = 0 then Exit;

  WriteLn(Format('Traitement par lot de %d éléments', [FQueue.Count]));

  // Traiter tous les éléments du lot
  for i := 0 to FQueue.Count - 1 do
  begin
    item := FQueue[i];
    // Traitement réel ici (envoi réseau, écriture base de données, etc.)
    WriteLn('  - ', item.Data);
  end;

  FQueue.Clear;
  FLastProcessTime := Now;
end;

procedure TBatchProcessor.Add(const Data: string);  
var
  item: TBatchItem;
  waitTime: Integer;
begin
  item.Data := Data;
  item.Timestamp := Now;
  FQueue.Add(item);

  // Traiter si le lot est plein
  if FQueue.Count >= FBatchSize then
  begin
    ProcessBatch;
    Exit;
  end;

  // Traiter si le délai maximal est dépassé
  waitTime := MilliSecondsBetween(Now, FLastProcessTime);
  if waitTime >= FMaxWaitTime then
    ProcessBatch;
end;

procedure TBatchProcessor.Flush;  
begin
  ProcessBatch;
end;

end.
```

**Exemple d'utilisation** :

```pascal
program UseBatchProcessor;

{$mode objfpc}{$H+}

uses
  BatchProcessor, SysUtils;

var
  processor: TBatchProcessor;
  i: Integer;

begin
  // Créer un processeur par lots
  // Taille du lot : 10, Délai max : 5 secondes
  processor := TBatchProcessor.Create(10, 5000);
  try
    // Ajouter des données
    // Au lieu d'envoyer immédiatement chaque élément (coûteux en batterie),
    // ils sont regroupés et traités ensemble
    for i := 1 to 25 do
    begin
      processor.Add('Élément ' + IntToStr(i));
      Sleep(200);  // Simuler un délai entre ajouts
    end;

    // Forcer le traitement des éléments restants
    processor.Flush;

  finally
    processor.Free;
  end;
end.
```

## Optimisation de l'interface utilisateur

### Rendu adaptatif selon la résolution

```pascal
unit AdaptiveUI;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls;

type
  TScreenDensity = (sdLow, sdMedium, sdHigh, sdExtraHigh);

  TUIScaler = class
  private
    class function GetScreenDensity: TScreenDensity;
    class function GetScaleFactor: Single;
  public
    class procedure ScaleControl(Control: TControl);
    class procedure ScaleForm(Form: TForm);
    class function ScaleValue(Value: Integer): Integer;
  end;

implementation

class function TUIScaler.GetScreenDensity: TScreenDensity;  
var
  dpi: Integer;
begin
  {$IFDEF ANDROID}
  dpi := Screen.PixelsPerInch;
  {$ELSE}
  dpi := 96;  // DPI par défaut pour desktop
  {$ENDIF}

  if dpi <= 120 then
    Result := sdLow
  else if dpi <= 160 then
    Result := sdMedium
  else if dpi <= 240 then
    Result := sdHigh
  else
    Result := sdExtraHigh;
end;

class function TUIScaler.GetScaleFactor: Single;  
begin
  case GetScreenDensity of
    sdLow:       Result := 0.75;
    sdMedium:    Result := 1.0;
    sdHigh:      Result := 1.5;
    sdExtraHigh: Result := 2.0;
  end;
end;

class procedure TUIScaler.ScaleControl(Control: TControl);  
var
  factor: Single;
begin
  factor := GetScaleFactor;

  Control.Width := Round(Control.Width * factor);
  Control.Height := Round(Control.Height * factor);
  Control.Left := Round(Control.Left * factor);
  Control.Top := Round(Control.Top * factor);

  if Control is TWinControl then
  begin
    // Adapter la police
    TWinControl(Control).Font.Size := Round(TWinControl(Control).Font.Size * factor);
  end;
end;

class procedure TUIScaler.ScaleForm(Form: TForm);  
var
  i: Integer;
begin
  // Adapter le formulaire
  Form.Width := Round(Form.Width * GetScaleFactor);
  Form.Height := Round(Form.Height * GetScaleFactor);

  // Adapter tous les contrôles enfants
  for i := 0 to Form.ControlCount - 1 do
    ScaleControl(Form.Controls[i]);
end;

class function TUIScaler.ScaleValue(Value: Integer): Integer;  
begin
  Result := Round(Value * GetScaleFactor);
end;

end.
```

**Utilisation dans une application** :

```pascal
program AdaptiveUIApp;

{$mode objfpc}{$H+}

uses
  Forms, StdCtrls, AdaptiveUI;

type
  TMainForm = class(TForm)
    btnSubmit: TButton;
    lblTitle: TLabel;

    procedure FormCreate(Sender: TObject);
  end;

procedure TMainForm.FormCreate(Sender: TObject);  
begin
  // Adapter automatiquement l'interface selon l'écran
  TUIScaler.ScaleForm(Self);

  // Ou adapter manuellement certains éléments
  lblTitle.Font.Size := TUIScaler.ScaleValue(16);
  btnSubmit.Height := TUIScaler.ScaleValue(48);
end;

var
  MainForm: TMainForm;

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
```

### Virtual ScrollBox (liste virtualisée)

Pour afficher de grandes quantités de données sans consommer trop de mémoire :

```pascal
unit VirtualList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, Forms;

type
  TGetItemTextEvent = procedure(Index: Integer; var Text: string) of object;

  TVirtualListBox = class(TCustomControl)
  private
    FItemCount: Integer;
    FItemHeight: Integer;
    FTopIndex: Integer;
    FVisibleItems: Integer;
    FOnGetItemText: TGetItemTextEvent;

    procedure CalculateVisibleItems;
    procedure UpdateScrollBar;
  protected
    procedure Paint; override;
    procedure Resize; override;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint); override;
  public
    constructor Create(AOwner: TComponent); override;

    property ItemCount: Integer read FItemCount write SetItemCount;
    property ItemHeight: Integer read FItemHeight write SetItemHeight;
    property TopIndex: Integer read FTopIndex write SetTopIndex;

    property OnGetItemText: TGetItemTextEvent read FOnGetItemText write FOnGetItemText;
  end;

implementation

constructor TVirtualListBox.Create(AOwner: TComponent);  
begin
  inherited Create(AOwner);
  FItemCount := 0;
  FItemHeight := 40;
  FTopIndex := 0;
  CalculateVisibleItems;
end;

procedure TVirtualListBox.CalculateVisibleItems;  
begin
  if FItemHeight > 0 then
    FVisibleItems := (Height div FItemHeight) + 2  // +2 pour le défilement fluide
  else
    FVisibleItems := 0;
end;

procedure TVirtualListBox.Paint;  
var
  i, y: Integer;
  itemIndex: Integer;
  itemText: string;
  itemRect: TRect;
begin
  inherited Paint;

  Canvas.Brush.Color := clWhite;
  Canvas.FillRect(ClientRect);

  y := 0;
  for i := 0 to FVisibleItems - 1 do
  begin
    itemIndex := FTopIndex + i;

    if itemIndex >= FItemCount then
      Break;

    // Obtenir le texte de l'élément
    itemText := 'Item ' + IntToStr(itemIndex);
    if Assigned(FOnGetItemText) then
      FOnGetItemText(itemIndex, itemText);

    // Dessiner l'élément
    itemRect := Rect(0, y, Width, y + FItemHeight);

    // Alterner les couleurs de fond
    if Odd(itemIndex) then
      Canvas.Brush.Color := clWindow
    else
      Canvas.Brush.Color := $F0F0F0;

    Canvas.FillRect(itemRect);

    // Dessiner le texte
    Canvas.Brush.Style := bsClear;
    Canvas.TextRect(itemRect, 10, y + 10, itemText);

    Inc(y, FItemHeight);
  end;
end;

procedure TVirtualListBox.Resize;  
begin
  inherited Resize;
  CalculateVisibleItems;
  Invalidate;
end;

procedure TVirtualListBox.MouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint);
begin
  inherited MouseWheel(Shift, WheelDelta, MousePos);

  if WheelDelta > 0 then
    TopIndex := TopIndex - 3  // Défiler vers le haut
  else
    TopIndex := TopIndex + 3; // Défiler vers le bas
end;

procedure TVirtualListBox.SetItemCount(Value: Integer);  
begin
  if FItemCount <> Value then
  begin
    FItemCount := Value;
    UpdateScrollBar;
    Invalidate;
  end;
end;

procedure TVirtualListBox.SetItemHeight(Value: Integer);  
begin
  if FItemHeight <> Value then
  begin
    FItemHeight := Value;
    CalculateVisibleItems;
    Invalidate;
  end;
end;

procedure TVirtualListBox.SetTopIndex(Value: Integer);  
begin
  // Limiter le défilement
  if Value < 0 then
    Value := 0
  else if Value > FItemCount - FVisibleItems then
    Value := FItemCount - FVisibleItems;

  if Value < 0 then
    Value := 0;

  if FTopIndex <> Value then
  begin
    FTopIndex := Value;
    Invalidate;
  end;
end;

procedure TVirtualListBox.UpdateScrollBar;  
begin
  // Configurer la barre de défilement
  // À implémenter selon les besoins
end;

end.
```

**Utilisation** :

```pascal
program VirtualListDemo;

{$mode objfpc}{$H+}

uses
  Forms, VirtualList;

type
  TMainForm = class(TForm)
    virtualList: TVirtualListBox;

    procedure FormCreate(Sender: TObject);
    procedure GetItemText(Index: Integer; var Text: string);
  end;

procedure TMainForm.FormCreate(Sender: TObject);  
begin
  virtualList := TVirtualListBox.Create(Self);
  virtualList.Parent := Self;
  virtualList.Align := alClient;
  virtualList.OnGetItemText := @GetItemText;

  // Afficher 10000 éléments sans consommer 10000x la mémoire!
  virtualList.ItemCount := 10000;
end;

procedure TMainForm.GetItemText(Index: Integer; var Text: string);  
begin
  // Générer le texte dynamiquement (pas de stockage)
  Text := Format('Élément #%d - Contenu généré à la demande', [Index]);
end;

var
  MainForm: TMainForm;

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
```

### Animations performantes

```pascal
unit SmoothAnimation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ExtCtrls;

type
  TEasingFunction = function(t: Single): Single;

  TPropertyAnimation = class(TComponent)
  private
    FControl: TControl;
    FTimer: TTimer;
    FStartValue: Integer;
    FEndValue: Integer;
    FCurrentValue: Integer;
    FDuration: Integer;
    FStartTime: TDateTime;
    FRunning: Boolean;
    FEasing: TEasingFunction;
    FOnComplete: TNotifyEvent;

    procedure TimerTick(Sender: TObject);
    procedure UpdateProperty(Value: Integer); virtual; abstract;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Start(FromValue, ToValue, DurationMs: Integer);
    procedure Stop;

    property Control: TControl read FControl write FControl;
    property Running: Boolean read FRunning;
    property OnComplete: TNotifyEvent read FOnComplete write FOnComplete;
  end;

  TPositionAnimation = class(TPropertyAnimation)
  private
    FAnimateX: Boolean;
  protected
    procedure UpdateProperty(Value: Integer); override;
  public
    property AnimateX: Boolean read FAnimateX write FAnimateX;
  end;

  TOpacityAnimation = class(TPropertyAnimation)
  protected
    procedure UpdateProperty(Value: Integer); override;
  end;

// Fonctions d'easing
function EaseLinear(t: Single): Single;  
function EaseInQuad(t: Single): Single;  
function EaseOutQuad(t: Single): Single;  
function EaseInOutQuad(t: Single): Single;

implementation

uses
  Math, DateUtils;

// Fonctions d'easing
function EaseLinear(t: Single): Single;  
begin
  Result := t;
end;

function EaseInQuad(t: Single): Single;  
begin
  Result := t * t;
end;

function EaseOutQuad(t: Single): Single;  
begin
  Result := t * (2 - t);
end;

function EaseInOutQuad(t: Single): Single;  
begin
  if t < 0.5 then
    Result := 2 * t * t
  else
    Result := -1 + (4 - 2 * t) * t;
end;

{ TPropertyAnimation }

constructor TPropertyAnimation.Create(AOwner: TComponent);  
begin
  inherited Create(AOwner);
  FTimer := TTimer.Create(Self);
  FTimer.Interval := 16;  // ~60 FPS
  FTimer.OnTimer := @TimerTick;
  FTimer.Enabled := False;
  FRunning := False;
  FEasing := @EaseOutQuad;  // Easing par défaut
end;

destructor TPropertyAnimation.Destroy;  
begin
  Stop;
  inherited Destroy;
end;

procedure TPropertyAnimation.Start(FromValue, ToValue, DurationMs: Integer);  
begin
  FStartValue := FromValue;
  FEndValue := ToValue;
  FDuration := DurationMs;
  FStartTime := Now;
  FRunning := True;
  FTimer.Enabled := True;
end;

procedure TPropertyAnimation.Stop;  
begin
  FRunning := False;
  FTimer.Enabled := False;
end;

procedure TPropertyAnimation.TimerTick(Sender: TObject);  
var
  elapsed: Integer;
  progress: Single;
  easedProgress: Single;
begin
  if not Assigned(FControl) then
  begin
    Stop;
    Exit;
  end;

  elapsed := MilliSecondsBetween(Now, FStartTime);

  if elapsed >= FDuration then
  begin
    // Animation terminée
    UpdateProperty(FEndValue);
    Stop;

    if Assigned(FOnComplete) then
      FOnComplete(Self);
  end
  else
  begin
    // Calculer la progression
    progress := elapsed / FDuration;
    easedProgress := FEasing(progress);

    // Interpoler la valeur
    FCurrentValue := FStartValue + Round((FEndValue - FStartValue) * easedProgress);
    UpdateProperty(FCurrentValue);
  end;
end;

{ TPositionAnimation }

procedure TPositionAnimation.UpdateProperty(Value: Integer);  
begin
  if not Assigned(FControl) then Exit;

  if FAnimateX then
    FControl.Left := Value
  else
    FControl.Top := Value;
end;

{ TOpacityAnimation }

procedure TOpacityAnimation.UpdateProperty(Value: Integer);  
begin
  if not Assigned(FControl) then Exit;

  // Adapter selon le framework
  // FControl.Opacity := Value / 100.0;
end;

end.
```

**Exemple d'utilisation** :

```pascal
program AnimationDemo;

{$mode objfpc}{$H+}

uses
  Forms, StdCtrls, ExtCtrls, SmoothAnimation;

type
  TMainForm = class(TForm)
    btnAnimate: TButton;
    pnlBox: TPanel;

    procedure btnAnimateClick(Sender: TObject);
    procedure AnimationComplete(Sender: TObject);
  private
    animation: TPositionAnimation;
  end;

procedure TMainForm.btnAnimateClick(Sender: TObject);  
begin
  if not Assigned(animation) then
  begin
    animation := TPositionAnimation.Create(Self);
    animation.Control := pnlBox;
    animation.AnimateX := True;
    animation.OnComplete := @AnimationComplete;
  end;

  // Animer de la position actuelle à 300 pixels en 500ms
  animation.Start(pnlBox.Left, 300, 500);
end;

procedure TMainForm.AnimationComplete(Sender: TObject);  
begin
  ShowMessage('Animation terminée!');
end;

var
  MainForm: TMainForm;

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
```

## Optimisation réseau

### Cache intelligent

```pascal
unit NetworkCache;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections;

type
  TCacheEntry = record
    Data: string;
    Timestamp: TDateTime;
    ExpiresIn: Integer;  // en secondes
  end;

  TNetworkCache = class
  private
    type TCacheMap = specialize TDictionary<string, TCacheEntry>;
  private
    FCache: TCacheMap;
    FMaxSize: Integer;

    function IsExpired(const Entry: TCacheEntry): Boolean;
    procedure Cleanup;
  public
    constructor Create(MaxSize: Integer = 100);
    destructor Destroy; override;

    procedure Put(const Key, Data: string; ExpiresInSeconds: Integer = 3600);
    function Get(const Key: string; out Data: string): Boolean;
    procedure Clear;
    procedure Remove(const Key: string);

    property Count: Integer read GetCount;
  end;

implementation

uses
  DateUtils;

constructor TNetworkCache.Create(MaxSize: Integer);  
begin
  inherited Create;
  FCache := TCacheMap.Create;
  FMaxSize := MaxSize;
end;

destructor TNetworkCache.Destroy;  
begin
  FCache.Free;
  inherited Destroy;
end;

function TNetworkCache.IsExpired(const Entry: TCacheEntry): Boolean;  
var
  elapsed: Integer;
begin
  elapsed := SecondsBetween(Now, Entry.Timestamp);
  Result := elapsed >= Entry.ExpiresIn;
end;

procedure TNetworkCache.Cleanup;  
var
  keysToRemove: TStringList;
  key: string;
  entry: TCacheEntry;
begin
  keysToRemove := TStringList.Create;
  try
    // Identifier les entrées expirées
    for key in FCache.Keys do
    begin
      if FCache.TryGetValue(key, entry) and IsExpired(entry) then
        keysToRemove.Add(key);
    end;

    // Supprimer les entrées expirées
    for key in keysToRemove do
      FCache.Remove(key);

    // Si le cache est encore trop grand, supprimer les plus anciennes
    while FCache.Count > FMaxSize do
    begin
      // Trouver et supprimer l'entrée la plus ancienne
      // (implémentation simplifiée)
      Break;
    end;
  finally
    keysToRemove.Free;
  end;
end;

procedure TNetworkCache.Put(const Key, Data: string; ExpiresInSeconds: Integer);  
var
  entry: TCacheEntry;
begin
  entry.Data := Data;
  entry.Timestamp := Now;
  entry.ExpiresIn := ExpiresInSeconds;

  FCache.AddOrSetValue(Key, entry);

  // Nettoyer si nécessaire
  if FCache.Count > FMaxSize then
    Cleanup;
end;

function TNetworkCache.Get(const Key: string; out Data: string): Boolean;  
var
  entry: TCacheEntry;
begin
  Result := False;

  if FCache.TryGetValue(Key, entry) then
  begin
    if not IsExpired(entry) then
    begin
      Data := entry.Data;
      Result := True;
    end
    else
    begin
      // Supprimer l'entrée expirée
      FCache.Remove(Key);
    end;
  end;
end;

procedure TNetworkCache.Clear;  
begin
  FCache.Clear;
end;

procedure TNetworkCache.Remove(const Key: string);  
begin
  FCache.Remove(Key);
end;

function TNetworkCache.GetCount: Integer;  
begin
  Result := FCache.Count;
end;

end.
```

### Requêtes réseau optimisées

```pascal
unit OptimizedHTTP;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient, NetworkCache;

type
  THTTPManager = class
  private
    FClient: TFPHTTPClient;
    FCache: TNetworkCache;
    FTimeout: Integer;
    FUserAgent: string;
  public
    constructor Create;
    destructor Destroy; override;

    function Get(const URL: string; UseCache: Boolean = True): string;
    function Post(const URL, Data: string): string;

    property Timeout: Integer read FTimeout write FTimeout;
    property UserAgent: string read FUserAgent write FUserAgent;
  end;

implementation

constructor THTTPManager.Create;  
begin
  inherited Create;
  FClient := TFPHTTPClient.Create(nil);
  FCache := TNetworkCache.Create;
  FTimeout := 30000;  // 30 secondes
  FUserAgent := 'FreePascal Mobile App/1.0';

  FClient.ConnectTimeout := FTimeout;
  FClient.IOTimeout := FTimeout;
end;

destructor THTTPManager.Destroy;  
begin
  FClient.Free;
  FCache.Free;
  inherited Destroy;
end;

function THTTPManager.Get(const URL: string; UseCache: Boolean): string;  
var
  cachedData: string;
begin
  // Vérifier le cache d'abord
  if UseCache and FCache.Get(URL, cachedData) then
  begin
    WriteLn('Cache HIT: ', URL);
    Result := cachedData;
    Exit;
  end;

  WriteLn('Cache MISS: ', URL);

  // Télécharger depuis le réseau
  try
    FClient.AddHeader('User-Agent', FUserAgent);
    FClient.AddHeader('Accept-Encoding', 'gzip, deflate');

    Result := FClient.Get(URL);

    // Mettre en cache
    if UseCache then
      FCache.Put(URL, Result, 3600);  // Cache 1 heure

  except
    on E: Exception do
    begin
      WriteLn('Erreur réseau: ', E.Message);
      raise;
    end;
  end;
end;

function THTTPManager.Post(const URL, Data: string): string;  
var
  stream: TStringStream;
begin
  stream := TStringStream.Create(Data);
  try
    FClient.AddHeader('Content-Type', 'application/json');
    Result := FClient.FormPost(URL, stream);
  finally
    stream.Free;
  end;
end;

end.
```

### Synchronisation différée (deferred sync)

```pascal
unit DeferredSync;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections;

type
  TSyncItem = record
    ID: string;
    Data: string;
    Timestamp: TDateTime;
    Retries: Integer;
  end;

  TSyncQueue = specialize TList<TSyncItem>;

  TDeferredSyncManager = class
  private
    FQueue: TSyncQueue;
    FMaxRetries: Integer;
    FSyncing: Boolean;

    procedure SaveQueueToDisk;
    procedure LoadQueueFromDisk;
    function SendItem(const Item: TSyncItem): Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(const ID, Data: string);
    procedure ProcessQueue;
    procedure Clear;

    property QueueSize: Integer read GetQueueSize;
    property Syncing: Boolean read FSyncing;
  end;

implementation

uses
  fpjson, jsonparser;

const
  QUEUE_FILE = 'sync_queue.json';

constructor TDeferredSyncManager.Create;  
begin
  inherited Create;
  FQueue := TSyncQueue.Create;
  FMaxRetries := 3;
  FSyncing := False;

  LoadQueueFromDisk;
end;

destructor TDeferredSyncManager.Destroy;  
begin
  SaveQueueToDisk;
  FQueue.Free;
  inherited Destroy;
end;

procedure TDeferredSyncManager.Add(const ID, Data: string);  
var
  item: TSyncItem;
begin
  item.ID := ID;
  item.Data := Data;
  item.Timestamp := Now;
  item.Retries := 0;

  FQueue.Add(item);
  SaveQueueToDisk;

  WriteLn(Format('Élément ajouté à la file : %s (Total: %d)', [ID, FQueue.Count]));
end;

procedure TDeferredSyncManager.ProcessQueue;  
var
  i: Integer;
  item: TSyncItem;
  success: Boolean;
begin
  if FSyncing or (FQueue.Count = 0) then
    Exit;

  FSyncing := True;
  try
    WriteLn('Traitement de la file de synchronisation...');

    i := 0;
    while i < FQueue.Count do
    begin
      item := FQueue[i];

      WriteLn(Format('Envoi de %s (tentative %d/%d)',
        [item.ID, item.Retries + 1, FMaxRetries]));

      success := SendItem(item);

      if success then
      begin
        WriteLn('  ✓ Succès');
        FQueue.Delete(i);
        // Ne pas incrémenter i car l'élément a été supprimé
      end
      else
      begin
        WriteLn('  ✗ Échec');
        Inc(item.Retries);

        if item.Retries >= FMaxRetries then
        begin
          WriteLn('  ⚠ Nombre maximum de tentatives atteint, suppression');
          FQueue.Delete(i);
        end
        else
        begin
          FQueue[i] := item;
          Inc(i);
        end;
      end;
    end;

    SaveQueueToDisk;
    WriteLn(Format('Synchronisation terminée. Éléments restants: %d', [FQueue.Count]));

  finally
    FSyncing := False;
  end;
end;

procedure TDeferredSyncManager.Clear;  
begin
  FQueue.Clear;
  SaveQueueToDisk;
end;

function TDeferredSyncManager.SendItem(const Item: TSyncItem): Boolean;  
begin
  // Simuler l'envoi réseau
  // Dans une vraie application, utiliser THTTPManager
  try
    Sleep(100);  // Simuler latence réseau
    Result := Random > 0.3;  // 70% de succès
  except
    Result := False;
  end;
end;

procedure TDeferredSyncManager.SaveQueueToDisk;  
var
  jsonArray: TJSONArray;
  jsonItem: TJSONObject;
  item: TSyncItem;
  fileStream: TFileStream;
begin
  jsonArray := TJSONArray.Create;
  try
    for item in FQueue do
    begin
      jsonItem := TJSONObject.Create;
      jsonItem.Add('id', item.ID);
      jsonItem.Add('data', item.Data);
      jsonItem.Add('timestamp', DateTimeToStr(item.Timestamp));
      jsonItem.Add('retries', item.Retries);
      jsonArray.Add(jsonItem);
    end;

    fileStream := TFileStream.Create(QUEUE_FILE, fmCreate);
    try
      fileStream.WriteAnsiString(jsonArray.AsJSON);
    finally
      fileStream.Free;
    end;
  finally
    jsonArray.Free;
  end;
end;

procedure TDeferredSyncManager.LoadQueueFromDisk;  
var
  jsonData: TJSONData;
  jsonArray: TJSONArray;
  jsonItem: TJSONObject;
  item: TSyncItem;
  i: Integer;
  fileStream: TFileStream;
  jsonString: string;
begin
  if not FileExists(QUEUE_FILE) then
    Exit;

  try
    fileStream := TFileStream.Create(QUEUE_FILE, fmOpenRead);
    try
      SetLength(jsonString, fileStream.Size);
      fileStream.Read(jsonString[1], fileStream.Size);
    finally
      fileStream.Free;
    end;

    jsonData := GetJSON(jsonString);
    try
      if jsonData is TJSONArray then
      begin
        jsonArray := TJSONArray(jsonData);

        for i := 0 to jsonArray.Count - 1 do
        begin
          jsonItem := TJSONObject(jsonArray[i]);

          item.ID := jsonItem.Get('id', '');
          item.Data := jsonItem.Get('data', '');
          item.Timestamp := StrToDateTime(jsonItem.Get('timestamp', DateTimeToStr(Now)));
          item.Retries := jsonItem.Get('retries', 0);

          FQueue.Add(item);
        end;

        WriteLn(Format('%d éléments chargés depuis la file', [FQueue.Count]));
      end;
    finally
      jsonData.Free;
    end;
  except
    on E: Exception do
      WriteLn('Erreur lors du chargement de la file: ', E.Message);
  end;
end;

function TDeferredSyncManager.GetQueueSize: Integer;  
begin
  Result := FQueue.Count;
end;

end.
```

**Utilisation** :

```pascal
program DeferredSyncDemo;

{$mode objfpc}{$H+}

uses
  DeferredSync, SysUtils;

var
  syncMgr: TDeferredSyncManager;
  i: Integer;

begin
  Randomize;

  syncMgr := TDeferredSyncManager.Create;
  try
    // Ajouter des éléments à synchroniser
    WriteLn('Ajout d''éléments à la file...');
    for i := 1 to 10 do
      syncMgr.Add('item-' + IntToStr(i), '{"value":' + IntToStr(i) + '}');

    WriteLn;
    WriteLn('Tentative de synchronisation...');
    syncMgr.ProcessQueue;

    WriteLn;
    WriteLn(Format('Éléments restants dans la file: %d', [syncMgr.QueueSize]));

    // Dans une vraie application, ProcessQueue serait appelé:
    // - Quand la connexion réseau est rétablie
    // - Périodiquement en arrière-plan
    // - Quand l'utilisateur déclenche une synchronisation manuelle

  finally
    syncMgr.Free;
  end;
end.
```

## Optimisation du stockage

### Compression de données

```pascal
unit DataCompression;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TCompressionManager = class
  public
    class function Compress(const Data: string): TBytes;
    class function Decompress(const Data: TBytes): string;

    class procedure CompressFile(const SourceFile, DestFile: string);
    class procedure DecompressFile(const SourceFile, DestFile: string);
  end;

implementation

uses
  zstream;

class function TCompressionManager.Compress(const Data: string): TBytes;  
var
  input: TStringStream;
  output: TBytesStream;
  compressor: TCompressionStream;
begin
  input := TStringStream.Create(Data);
  output := TBytesStream.Create;
  try
    compressor := TCompressionStream.Create(clDefault, output);
    try
      compressor.CopyFrom(input, input.Size);
    finally
      compressor.Free;
    end;

    Result := output.Bytes;
    SetLength(Result, output.Size);
  finally
    input.Free;
    output.Free;
  end;
end;

class function TCompressionManager.Decompress(const Data: TBytes): string;  
var
  input: TBytesStream;
  output: TStringStream;
  decompressor: TDecompressionStream;
begin
  input := TBytesStream.Create(Data);
  output := TStringStream.Create('');
  try
    decompressor := TDecompressionStream.Create(input);
    try
      output.CopyFrom(decompressor, decompressor.Size);
    finally
      decompressor.Free;
    end;

    Result := output.DataString;
  finally
    input.Free;
    output.Free;
  end;
end;

class procedure TCompressionManager.CompressFile(const SourceFile, DestFile: string);  
var
  input, output: TFileStream;
  compressor: TCompressionStream;
begin
  input := TFileStream.Create(SourceFile, fmOpenRead);
  output := TFileStream.Create(DestFile, fmCreate);
  try
    compressor := TCompressionStream.Create(clMax, output);
    try
      compressor.CopyFrom(input, input.Size);
    finally
      compressor.Free;
    end;
  finally
    input.Free;
    output.Free;
  end;
end;

class procedure TCompressionManager.DecompressFile(const SourceFile, DestFile: string);  
var
  input, output: TFileStream;
  decompressor: TDecompressionStream;
begin
  input := TFileStream.Create(SourceFile, fmOpenRead);
  output := TFileStream.Create(DestFile, fmCreate);
  try
    decompressor := TDecompressionStream.Create(input);
    try
      output.CopyFrom(decompressor, 0);
    finally
      decompressor.Free;
    end;
  finally
    input.Free;
    output.Free;
  end;
end;

end.
```

**Exemple d'utilisation** :

```pascal
program CompressionDemo;

{$mode objfpc}{$H+}

uses
  SysUtils, DataCompression;

var
  originalData: string;
  compressed: TBytes;
  decompressed: string;
  originalSize, compressedSize: Integer;
  ratio: Double;

begin
  // Créer des données de test
  originalData := StringOfChar('A', 1000) + StringOfChar('B', 1000);
  originalSize := Length(originalData);

  WriteLn('=== Test de compression ===');
  WriteLn('Taille originale: ', originalSize, ' octets');

  // Compresser
  compressed := TCompressionManager.Compress(originalData);
  compressedSize := Length(compressed);

  WriteLn('Taille compressée: ', compressedSize, ' octets');

  ratio := (1 - (compressedSize / originalSize)) * 100;
  WriteLn('Ratio de compression: ', ratio:0:2, '%');

  // Décompresser
  decompressed := TCompressionManager.Decompress(compressed);

  if decompressed = originalData then
    WriteLn('✓ Décompression réussie')
  else
    WriteLn('✗ Erreur de décompression');

  WriteLn;
  WriteLn('=== Compression de fichier ===');

  // Créer un fichier de test
  with TStringList.Create do
  try
    Add('Ligne 1');
    Add('Ligne 2');
    Add('Ligne 3');
    SaveToFile('test.txt');
  finally
    Free;
  end;

  // Compresser le fichier
  TCompressionManager.CompressFile('test.txt', 'test.txt.gz');
  WriteLn('Fichier compressé créé: test.txt.gz');

  // Décompresser
  TCompressionManager.DecompressFile('test.txt.gz', 'test_restored.txt');
  WriteLn('Fichier décompressé: test_restored.txt');
end.
```

### Base de données SQLite optimisée

```pascal
unit OptimizedDatabase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, sqlite3conn;

type
  TDatabaseManager = class
  private
    FConnection: TSQLite3Connection;
    FTransaction: TSQLTransaction;

    procedure ConfigureForMobile;
  public
    constructor Create(const DatabaseFile: string);
    destructor Destroy; override;

    procedure BeginBatch;
    procedure EndBatch;
    procedure Vacuum;
    procedure Analyze;

    function ExecuteQuery(const SQL: string): TSQLQuery;
    procedure ExecuteSQL(const SQL: string);

    property Connection: TSQLite3Connection read FConnection;
  end;

implementation

constructor TDatabaseManager.Create(const DatabaseFile: string);  
begin
  inherited Create;

  FConnection := TSQLite3Connection.Create(nil);
  FTransaction := TSQLTransaction.Create(nil);

  FConnection.DatabaseName := DatabaseFile;
  FConnection.Transaction := FTransaction;
  FTransaction.Database := FConnection;

  FConnection.Open;

  ConfigureForMobile;
end;

destructor TDatabaseManager.Destroy;  
begin
  if FConnection.Connected then
    FConnection.Close;

  FTransaction.Free;
  FConnection.Free;

  inherited Destroy;
end;

procedure TDatabaseManager.ConfigureForMobile;  
begin
  // Optimisations SQLite pour mobile

  // Mode journal WAL (Write-Ahead Logging) - meilleur pour les lectures concurrentes
  ExecuteSQL('PRAGMA journal_mode = WAL');

  // Synchronisation normale (compromis performance/sécurité)
  ExecuteSQL('PRAGMA synchronous = NORMAL');

  // Augmenter la taille du cache (en pages)
  ExecuteSQL('PRAGMA cache_size = -2000');  // -2000 = 2 MB

  // Activer le mode mémoire partagée
  ExecuteSQL('PRAGMA shared_cache = ON');

  // Définir le nombre de pages en mémoire
  ExecuteSQL('PRAGMA page_size = 4096');

  // Activer les clés étrangères
  ExecuteSQL('PRAGMA foreign_keys = ON');

  // Optimiser l'allocation temporaire
  ExecuteSQL('PRAGMA temp_store = MEMORY');
end;

procedure TDatabaseManager.BeginBatch;  
begin
  FTransaction.StartTransaction;
end;

procedure TDatabaseManager.EndBatch;  
begin
  FTransaction.Commit;
end;

procedure TDatabaseManager.Vacuum;  
begin
  // Compacter la base de données (récupérer l'espace)
  ExecuteSQL('VACUUM');
end;

procedure TDatabaseManager.Analyze;  
begin
  // Mettre à jour les statistiques pour l'optimiseur
  ExecuteSQL('ANALYZE');
end;

function TDatabaseManager.ExecuteQuery(const SQL: string): TSQLQuery;  
begin
  Result := TSQLQuery.Create(nil);
  Result.Database := FConnection;
  Result.SQL.Text := SQL;
  Result.Open;
end;

procedure TDatabaseManager.ExecuteSQL(const SQL: string);  
var
  query: TSQLQuery;
begin
  query := TSQLQuery.Create(nil);
  try
    query.Database := FConnection;
    query.SQL.Text := SQL;
    query.ExecSQL;
    FTransaction.Commit;
  finally
    query.Free;
  end;
end;

end.
```

**Utilisation avec transactions batch** :

```pascal
program OptimizedDBDemo;

{$mode objfpc}{$H+}

uses
  SysUtils, OptimizedDatabase;

var
  db: TDatabaseManager;
  i: Integer;
  startTime: TDateTime;

begin
  db := TDatabaseManager.Create('mobile_app.db');
  try
    // Créer une table
    db.ExecuteSQL(
      'CREATE TABLE IF NOT EXISTS messages (' +
      '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
      '  content TEXT,' +
      '  timestamp INTEGER' +
      ')'
    );

    WriteLn('=== Insertion avec transactions batch ===');

    startTime := Now;

    // ❌ LENT - Insertion sans batch (1000 commits)
    (*
    for i := 1 to 1000 do
    begin
      db.ExecuteSQL(Format(
        'INSERT INTO messages (content, timestamp) VALUES (''Message %d'', %d)',
        [i, DateTimeToUnix(Now)]
      ));
    end;
    *)

    // ✅ RAPIDE - Insertion avec batch (1 seul commit)
    db.BeginBatch;
    try
      for i := 1 to 1000 do
      begin
        db.ExecuteSQL(Format(
          'INSERT INTO messages (content, timestamp) VALUES (''Message %d'', %d)',
          [i, DateTimeToUnix(Now)]
        ));
      end;
      db.EndBatch;
    except
      // En cas d'erreur, le rollback est automatique
      raise;
    end;

    WriteLn('Temps écoulé: ', MilliSecondsBetween(Now, startTime), ' ms');
    WriteLn('1000 enregistrements insérés');

    // Optimiser périodiquement
    WriteLn;
    WriteLn('Optimisation de la base de données...');
    db.Analyze;
    WriteLn('✓ Terminé');

  finally
    db.Free;
  end;
end.
```

## Monitoring et profilage

### Surveillance de la performance

```pascal
unit PerformanceMonitor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections;

type
  TPerformanceMetric = record
    Name: string;
    StartTime: TDateTime;
    Duration: Int64;  // en millisecondes
    MemoryBefore: Int64;
    MemoryAfter: Int64;
  end;

  TMetricsList = specialize TList<TPerformanceMetric>;

  TPerformanceMonitor = class
  private
    FMetrics: TMetricsList;
    FCurrentMetric: TPerformanceMetric;
    FMonitoring: Boolean;

    function GetMemoryUsage: Int64;
  public
    constructor Create;
    destructor Destroy; override;

    procedure StartMeasure(const MetricName: string);
    procedure StopMeasure;

    procedure PrintReport;
    procedure SaveReport(const Filename: string);
    procedure Clear;

    property Monitoring: Boolean read FMonitoring;
  end;

implementation

uses
  DateUtils;

constructor TPerformanceMonitor.Create;  
begin
  inherited Create;
  FMetrics := TMetricsList.Create;
  FMonitoring := False;
end;

destructor TPerformanceMonitor.Destroy;  
begin
  FMetrics.Free;
  inherited Destroy;
end;

function TPerformanceMonitor.GetMemoryUsage: Int64;  
begin
  {$IFDEF WINDOWS}
  Result := GetHeapStatus.TotalAllocated;
  {$ELSE}
  // Pour Unix/Linux, utiliser getrusage ou lire /proc/self/status
  Result := 0;
  {$ENDIF}
end;

procedure TPerformanceMonitor.StartMeasure(const MetricName: string);  
begin
  if FMonitoring then
    raise Exception.Create('Une mesure est déjà en cours');

  FCurrentMetric.Name := MetricName;
  FCurrentMetric.StartTime := Now;
  FCurrentMetric.MemoryBefore := GetMemoryUsage;
  FMonitoring := True;
end;

procedure TPerformanceMonitor.StopMeasure;  
begin
  if not FMonitoring then
    raise Exception.Create('Aucune mesure en cours');

  FCurrentMetric.Duration := MilliSecondsBetween(Now, FCurrentMetric.StartTime);
  FCurrentMetric.MemoryAfter := GetMemoryUsage;

  FMetrics.Add(FCurrentMetric);
  FMonitoring := False;
end;

procedure TPerformanceMonitor.PrintReport;  
var
  metric: TPerformanceMetric;
  totalDuration: Int64;
  memDiff: Int64;
begin
  WriteLn('═══════════════════════════════════════════════════');
  WriteLn('  Rapport de performance');
  WriteLn('═══════════════════════════════════════════════════');
  WriteLn;

  totalDuration := 0;

  for metric in FMetrics do
  begin
    memDiff := metric.MemoryAfter - metric.MemoryBefore;

    WriteLn(metric.Name);
    WriteLn('  Durée: ', metric.Duration, ' ms');
    WriteLn('  Mémoire: ', memDiff div 1024, ' KB');
    WriteLn;

    Inc(totalDuration, metric.Duration);
  end;

  WriteLn('───────────────────────────────────────────────────');
  WriteLn('Total: ', totalDuration, ' ms pour ', FMetrics.Count, ' opérations');
  WriteLn('═══════════════════════════════════════════════════');
end;

procedure TPerformanceMonitor.SaveReport(const Filename: string);  
var
  report: TStringList;
  metric: TPerformanceMetric;
  memDiff: Int64;
begin
  report := TStringList.Create;
  try
    report.Add('Rapport de performance');
    report.Add('Date: ' + DateTimeToStr(Now));
    report.Add('');

    for metric in FMetrics do
    begin
      memDiff := metric.MemoryAfter - metric.MemoryBefore;

      report.Add(Format('%s,%d,%d',
        [metric.Name, metric.Duration, memDiff div 1024]));
    end;

    report.SaveToFile(Filename);
  finally
    report.Free;
  end;
end;

procedure TPerformanceMonitor.Clear;  
begin
  FMetrics.Clear;
end;

end.
```

**Utilisation** :

```pascal
program MonitoringDemo;

{$mode objfpc}{$H+}

uses
  SysUtils, PerformanceMonitor;

var
  monitor: TPerformanceMonitor;
  i: Integer;
  data: array[0..9999] of Integer;

procedure HeavyOperation;  
var
  j, k: Integer;
begin
  for j := 0 to High(data) do
    for k := 0 to 100 do
      data[j] := data[j] + k;
end;

begin
  monitor := TPerformanceMonitor.Create;
  try
    // Mesurer plusieurs opérations

    monitor.StartMeasure('Initialisation du tableau');
    for i := 0 to High(data) do
      data[i] := i;
    monitor.StopMeasure;

    monitor.StartMeasure('Calcul intensif');
    HeavyOperation;
    monitor.StopMeasure;

    monitor.StartMeasure('Tri du tableau');
    // Simuler un tri
    Sleep(100);
    monitor.StopMeasure;

    // Afficher le rapport
    monitor.PrintReport;

    // Sauvegarder pour analyse ultérieure
    monitor.SaveReport('performance_report.csv');

  finally
    monitor.Free;
  end;
end.
```

## Bonnes pratiques générales

### Checklist d'optimisation mobile

**Mémoire** :
- ✅ Utiliser des records plutôt que des classes pour les données simples
- ✅ Implémenter des object pools pour les objets fréquemment créés/détruits
- ✅ Libérer les ressources inutilisées (images, caches)
- ✅ Utiliser lazy loading pour les données volumineuses
- ✅ Éviter les fuites mémoire (HeapTrc en debug)

**CPU** :
- ✅ Déplacer les tâches longues dans des threads
- ✅ Utiliser des animations GPU plutôt que CPU
- ✅ Optimiser les boucles critiques
- ✅ Éviter les calculs répétés (mise en cache)
- ✅ Utiliser des algorithmes efficaces (O(n log n) vs O(n²))

**Batterie** :
- ✅ Minimiser l'utilisation du GPS (précision adaptative)
- ✅ Regrouper les opérations réseau (batch processing)
- ✅ Libérer les wake locks rapidement
- ✅ Utiliser les capteurs avec parcimonie
- ✅ Réduire la fréquence de mise à jour en arrière-plan

**Réseau** :
- ✅ Implémenter un cache intelligent
- ✅ Compresser les données transférées
- ✅ Gérer les déconnexions gracieusement
- ✅ Différer les synchronisations non critiques
- ✅ Utiliser des timeouts appropriés

**Stockage** :
- ✅ Compresser les données volumineuses
- ✅ Optimiser la base de données SQLite
- ✅ Nettoyer les fichiers temporaires
- ✅ Utiliser des transactions batch
- ✅ Vacuum périodique de la base

**Interface utilisateur** :
- ✅ Adapter l'UI selon la densité d'écran
- ✅ Utiliser des listes virtualisées pour grandes quantités
- ✅ Animations fluides à 60 FPS
- ✅ Feedback immédiat sur les actions utilisateur
- ✅ Gestes tactiles optimisés

### Pattern de conception pour mobile

```pascal
unit MobilePatterns;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  // Singleton pour gestionnaires globaux
  TAppManager = class
  private
    class var FInstance: TAppManager;
    constructor Create;
  public
    class function Instance: TAppManager;
    class procedure FreeInstance;

    procedure Initialize;
    procedure Shutdown;
  end;

  // Observer pattern pour événements d'application
  TMobileEvent = (meAppPaused, meAppResumed, meMemoryWarning, meNetworkChanged);

  IMobileEventObserver = interface
    ['{A1B2C3D4-E5F6-7890-ABCD-EF1234567890}']
    procedure OnMobileEvent(Event: TMobileEvent);
  end;

  TMobileEventManager = class
  private
    FObservers: TInterfaceList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure RegisterObserver(Observer: IMobileEventObserver);
    procedure UnregisterObserver(Observer: IMobileEventObserver);
    procedure NotifyEvent(Event: TMobileEvent);
  end;

implementation

{ TAppManager }

class var
  FreeOnTerminate: Boolean = True;

constructor TAppManager.Create;  
begin
  inherited Create;
end;

class function TAppManager.Instance: TAppManager;  
begin
  if not Assigned(FInstance) then
    FInstance := TAppManager.Create;
  Result := FInstance;
end;

class procedure TAppManager.FreeInstance;  
begin
  if Assigned(FInstance) then
  begin
    FInstance.Free;
    FInstance := nil;
  end;
end;

procedure TAppManager.Initialize;  
begin
  WriteLn('Application initialisée');
end;

procedure TAppManager.Shutdown;  
begin
  WriteLn('Application arrêtée');
end;

{ TMobileEventManager }

constructor TMobileEventManager.Create;  
begin
  inherited Create;
  FObservers := TInterfaceList.Create;
end;

destructor TMobileEventManager.Destroy;  
begin
  FObservers.Free;
  inherited Destroy;
end;

procedure TMobileEventManager.RegisterObserver(Observer: IMobileEventObserver);  
begin
  if FObservers.IndexOf(Observer) < 0 then
    FObservers.Add(Observer);
end;

procedure TMobileEventManager.UnregisterObserver(Observer: IMobileEventObserver);  
begin
  FObservers.Remove(Observer);
end;

procedure TMobileEventManager.NotifyEvent(Event: TMobileEvent);  
var
  i: Integer;
  observer: IMobileEventObserver;
begin
  for i := 0 to FObservers.Count - 1 do
  begin
    observer := FObservers[i] as IMobileEventObserver;
    observer.OnMobileEvent(Event);
  end;
end;

end.
```

### Tests de performance

```pascal
program PerformanceTests;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, PerformanceMonitor;

procedure TestStringConcatenation(monitor: TPerformanceMonitor);  
var
  i: Integer;
  result: string;
begin
  monitor.StartMeasure('Concaténation naïve (1000 itérations)');
  result := '';
  for i := 1 to 1000 do
    result := result + IntToStr(i);
  monitor.StopMeasure;

  monitor.StartMeasure('TStringBuilder (1000 itérations)');
  with TStringBuilder.Create do
  try
    for i := 1 to 1000 do
      Append(IntToStr(i));
    result := ToString;
  finally
    Free;
  end;
  monitor.StopMeasure;
end;

procedure TestListOperations(monitor: TPerformanceMonitor);  
var
  list: TStringList;
  i: Integer;
begin
  list := TStringList.Create;
  try
    monitor.StartMeasure('Ajout 10000 éléments TStringList');
    for i := 0 to 9999 do
      list.Add('Item ' + IntToStr(i));
    monitor.StopMeasure;

    monitor.StartMeasure('Recherche linéaire (Sorted=False)');
    list.Sorted := False;
    for i := 0 to 99 do
      list.IndexOf('Item 5000');
    monitor.StopMeasure;

    monitor.StartMeasure('Recherche dichotomique (Sorted=True)');
    list.Sorted := True;
    for i := 0 to 99 do
      list.IndexOf('Item 5000');
    monitor.StopMeasure;
  finally
    list.Free;
  end;
end;

var
  monitor: TPerformanceMonitor;

begin
  monitor := TPerformanceMonitor.Create;
  try
    WriteLn('=== Tests de performance ===');
    WriteLn;

    TestStringConcatenation(monitor);
    TestListOperations(monitor);

    WriteLn;
    monitor.PrintReport;
  finally
    monitor.Free;
  end;
end.
```

## Ressources et outils

### Outils de profilage

| Outil | Plateforme | Usage | Lien |
|-------|-----------|-------|------|
| **Android Profiler** | Android | CPU, Mémoire, Réseau, Batterie | Android Studio |
| **Instruments** | iOS | Performance complète | Xcode |
| **Valgrind** | Linux/Android | Fuites mémoire, profilage | https://valgrind.org/ |
| **HeapTrc** | FreePascal | Fuites mémoire | Intégré FPC |
| **SQLite Analyzer** | Multi | Optimisation SQLite | https://sqlite.org/ |

### Documentation recommandée

- **Android Performance Patterns** : Vidéos Google sur les patterns de performance
- **iOS Performance Guidelines** : Documentation officielle Apple
- **SQLite Mobile Optimization** : Wiki SQLite
- **Mobile Web Performance** : MDN Web Docs

### Bibliothèques utiles

```pascal
// Gestion d'images optimisée
uses
  BGRABitmap,        // Manipulation d'images performante
  BGRABitmapTypes,
  IntfGraphics;

// Structures de données efficaces
uses
  Generics.Collections,  // TDictionary, TList<T>, etc.
  fgl;                   // Free Generics Library

// Réseau optimisé
uses
  fphttpclient,      // Client HTTP léger
  opensslsockets;    // Support HTTPS

// Base de données
uses
  sqldb,             // Framework unifié
  sqlite3conn,       // SQLite optimisé
  sqlite3dyn;        // Binding dynamique
```

## Conclusion

L'optimisation pour appareils mobiles avec FreePascal nécessite une approche holistique qui prend en compte tous les aspects : mémoire, CPU, batterie, réseau, stockage et interface utilisateur.

### Points clés à retenir

✅ **Mémoire** : Utilisez des structures légères, implémentez des pools d'objets, gérez activement les ressources

✅ **CPU** : Déportez les tâches lourdes dans des threads, optimisez les boucles critiques, utilisez des algorithmes efficaces

✅ **Batterie** : Minimisez l'utilisation des capteurs énergivores (GPS, caméra), regroupez les opérations réseau, libérez les wake locks

✅ **Réseau** : Implémentez un cache intelligent, compressez les données, gérez les déconnexions, différez les synchronisations

✅ **Stockage** : Compressez les données volumineuses, optimisez SQLite, nettoyez régulièrement

✅ **UI** : Adaptez selon la densité d'écran, virtualisez les listes longues, animations fluides à 60 FPS

### Workflow d'optimisation

1. **Mesurer** : Profiler avant d'optimiser
2. **Identifier** : Trouver les vrais goulots d'étranglement
3. **Optimiser** : Appliquer les techniques appropriées
4. **Vérifier** : Mesurer les gains réels
5. **Itérer** : Répéter le processus

### Recommandations finales

- **Testez sur vrais appareils** : Les émulateurs ne reflètent pas la réalité
- **Testez sur différentes gammes** : Bas, moyen et haut de gamme
- **Surveillez les métriques** : CPU, RAM, batterie, réseau
- **Optimisez progressivement** : Pas de micro-optimisation prématurée
- **Documentez les optimisations** : Pour la maintenance future
- **Restez informé** : Les plateformes mobiles évoluent rapidement

> 💡 **Conseil final** : L'optimisation mobile est un équilibre constant entre performance, consommation d'énergie et expérience utilisateur. Priorisez toujours l'expérience utilisateur, puis optimisez ce qui compte vraiment pour vos utilisateurs.

**Bon développement mobile avec FreePascal ! 📱⚡**

⏭️ [Distribution sur stores](/13-developpement-mobile-embarque/10-distribution-stores.md)
