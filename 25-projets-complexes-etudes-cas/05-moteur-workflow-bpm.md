🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 25.5 Moteur de workflow/BPM

## Introduction

Un moteur de workflow (ou BPM - Business Process Management) est un système qui permet de définir, d'exécuter et de superviser des processus métier automatisés. Dans ce tutoriel, nous allons construire un moteur de workflow complet et multi-plateforme avec FreePascal/Lazarus.

### Qu'est-ce qu'un workflow ?

Un workflow représente une séquence d'étapes (tâches) qui doivent être exécutées dans un ordre défini pour accomplir un processus métier. Par exemple :
- Processus de validation de commande
- Circuit d'approbation de documents
- Flux de traitement de demandes
- Automatisation de processus administratifs

### Concepts fondamentaux

**Processus** : Le workflow complet, de son début à sa fin

**Activité/Tâche** : Une étape individuelle du processus

**Transition** : Le passage d'une activité à une autre

**Condition** : Règle qui détermine quelle transition emprunter

**Participant** : Utilisateur ou système qui exécute une activité

---

## Architecture du moteur de workflow

### Vue d'ensemble

Notre moteur de workflow comportera plusieurs composants clés :

1. **Modèle de données** : Représentation des processus, activités et transitions
2. **Moteur d'exécution** : Gestion de l'avancement des workflows
3. **Gestionnaire de règles** : Évaluation des conditions
4. **Interface de conception** : Éditeur graphique de workflows
5. **Interface d'exécution** : Visualisation et interaction avec les workflows actifs
6. **Persistance** : Sauvegarde en base de données (Windows/Ubuntu)

### Schéma conceptuel

```
┌─────────────────────────────────────────────┐
│         Interface de conception             │
│    (Éditeur graphique de workflows)         │
└──────────────────┬──────────────────────────┘
                   │
                   ▼
┌─────────────────────────────────────────────┐
│         Modèle de workflow                  │
│  (Processus, Activités, Transitions)        │
└──────────────────┬──────────────────────────┘
                   │
                   ▼
┌─────────────────────────────────────────────┐
│         Moteur d'exécution                  │
│  (Gestion des instances de workflow)        │
└──────────────────┬──────────────────────────┘
                   │
                   ▼
┌─────────────────────────────────────────────┐
│         Couche de persistance               │
│  (Base de données multi-plateforme)         │
└─────────────────────────────────────────────┘
```

---

## Modèle de données

### Classes de base

Commençons par définir les structures de données fondamentales :

```pascal
unit WorkflowModel;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections;

type
  // Types énumérés
  TActivityType = (atStart, atTask, atDecision, atParallel, atEnd);
  TActivityStatus = (asNotStarted, asActive, asCompleted, asCancelled);
  TTransitionType = (ttDefault, ttConditional);

  // Déclarations anticipées
  TWorkflowProcess = class;
  TActivity = class;
  TTransition = class;
  TWorkflowInstance = class;

  // Activité dans un processus
  TActivity = class
  private
    FId: string;
    FName: string;
    FActivityType: TActivityType;
    FDescription: string;
    FAssignedTo: string;
    FPosition: TPoint;
    FIncomingTransitions: TList;
    FOutgoingTransitions: TList;
    FProperties: TStringList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddIncomingTransition(ATransition: TTransition);
    procedure AddOutgoingTransition(ATransition: TTransition);

    property Id: string read FId write FId;
    property Name: string read FName write FName;
    property ActivityType: TActivityType read FActivityType write FActivityType;
    property Description: string read FDescription write FDescription;
    property AssignedTo: string read FAssignedTo write FAssignedTo;
    property Position: TPoint read FPosition write FPosition;
    property Properties: TStringList read FProperties;
  end;

  // Transition entre activités
  TTransition = class
  private
    FId: string;
    FName: string;
    FSourceActivity: TActivity;
    FTargetActivity: TActivity;
    FTransitionType: TTransitionType;
    FCondition: string;
  public
    constructor Create;

    property Id: string read FId write FId;
    property Name: string read FName write FName;
    property SourceActivity: TActivity read FSourceActivity write FSourceActivity;
    property TargetActivity: TActivity read FTargetActivity write FTargetActivity;
    property TransitionType: TTransitionType read FTransitionType write FTransitionType;
    property Condition: string read FCondition write FCondition;
  end;

  // Définition d'un processus
  TWorkflowProcess = class
  private
    FId: string;
    FName: string;
    FDescription: string;
    FVersion: string;
    FActivities: TObjectList<TActivity>;
    FTransitions: TObjectList<TTransition>;
  public
    constructor Create;
    destructor Destroy; override;

    function AddActivity(AName: string; AType: TActivityType): TActivity;
    function AddTransition(ASource, ATarget: TActivity): TTransition;
    function FindActivity(AId: string): TActivity;
    function GetStartActivity: TActivity;

    procedure SaveToFile(const AFileName: string);
    procedure LoadFromFile(const AFileName: string);

    property Id: string read FId write FId;
    property Name: string read FName write FName;
    property Description: string read FDescription write FDescription;
    property Version: string read FVersion write FVersion;
    property Activities: TObjectList<TActivity> read FActivities;
    property Transitions: TObjectList<TTransition> read FTransitions;
  end;

  // Instance d'exécution d'un processus
  TWorkflowInstance = class
  private
    FId: string;
    FProcessId: string;
    FCurrentActivity: TActivity;
    FStatus: TActivityStatus;
    FVariables: TStringList;
    FStartTime: TDateTime;
    FEndTime: TDateTime;
    FActivityHistory: TList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetVariable(const AName, AValue: string);
    function GetVariable(const AName: string): string;

    property Id: string read FId write FId;
    property ProcessId: string read FProcessId write FProcessId;
    property CurrentActivity: TActivity read FCurrentActivity write FCurrentActivity;
    property Status: TActivityStatus read FStatus write FStatus;
    property Variables: TStringList read FVariables;
  end;

implementation

// Implémentation de TActivity

constructor TActivity.Create;  
begin
  inherited Create;
  FId := TGUID.NewGuid.ToString;
  FIncomingTransitions := TList.Create;
  FOutgoingTransitions := TList.Create;
  FProperties := TStringList.Create;
end;

destructor TActivity.Destroy;  
begin
  FIncomingTransitions.Free;
  FOutgoingTransitions.Free;
  FProperties.Free;
  inherited Destroy;
end;

procedure TActivity.AddIncomingTransition(ATransition: TTransition);  
begin
  if FIncomingTransitions.IndexOf(ATransition) = -1 then
    FIncomingTransitions.Add(ATransition);
end;

procedure TActivity.AddOutgoingTransition(ATransition: TTransition);  
begin
  if FOutgoingTransitions.IndexOf(ATransition) = -1 then
    FOutgoingTransitions.Add(ATransition);
end;

// Implémentation de TTransition

constructor TTransition.Create;  
begin
  inherited Create;
  FId := TGUID.NewGuid.ToString;
  FTransitionType := ttDefault;
end;

// Implémentation de TWorkflowProcess

constructor TWorkflowProcess.Create;  
begin
  inherited Create;
  FId := TGUID.NewGuid.ToString;
  FActivities := TObjectList<TActivity>.Create(True);
  FTransitions := TObjectList<TTransition>.Create(True);
end;

destructor TWorkflowProcess.Destroy;  
begin
  FActivities.Free;
  FTransitions.Free;
  inherited Destroy;
end;

function TWorkflowProcess.AddActivity(AName: string; AType: TActivityType): TActivity;  
begin
  Result := TActivity.Create;
  Result.Name := AName;
  Result.ActivityType := AType;
  FActivities.Add(Result);
end;

function TWorkflowProcess.AddTransition(ASource, ATarget: TActivity): TTransition;  
begin
  Result := TTransition.Create;
  Result.SourceActivity := ASource;
  Result.TargetActivity := ATarget;
  ASource.AddOutgoingTransition(Result);
  ATarget.AddIncomingTransition(Result);
  FTransitions.Add(Result);
end;

function TWorkflowProcess.FindActivity(AId: string): TActivity;  
var
  Activity: TActivity;
begin
  Result := nil;
  for Activity in FActivities do
    if Activity.Id = AId then
      Exit(Activity);
end;

function TWorkflowProcess.GetStartActivity: TActivity;  
var
  Activity: TActivity;
begin
  Result := nil;
  for Activity in FActivities do
    if Activity.ActivityType = atStart then
      Exit(Activity);
end;

// Implémentation de TWorkflowInstance

constructor TWorkflowInstance.Create;  
begin
  inherited Create;
  FId := TGUID.NewGuid.ToString;
  FVariables := TStringList.Create;
  FActivityHistory := TList.Create;
  FStatus := asNotStarted;
end;

destructor TWorkflowInstance.Destroy;  
begin
  FVariables.Free;
  FActivityHistory.Free;
  inherited Destroy;
end;

procedure TWorkflowInstance.SetVariable(const AName, AValue: string);  
begin
  FVariables.Values[AName] := AValue;
end;

function TWorkflowInstance.GetVariable(const AName: string): string;  
begin
  Result := FVariables.Values[AName];
end;

end.
```

---

## Moteur d'exécution

Le moteur d'exécution gère l'avancement des instances de workflow :

```pascal
unit WorkflowEngine;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, WorkflowModel, Generics.Collections;

type
  TWorkflowEvent = procedure(Instance: TWorkflowInstance; Activity: TActivity) of object;

  TWorkflowEngine = class
  private
    FProcesses: TObjectList<TWorkflowProcess>;
    FInstances: TObjectList<TWorkflowInstance>;
    FOnActivityStarted: TWorkflowEvent;
    FOnActivityCompleted: TWorkflowEvent;
  public
    constructor Create;
    destructor Destroy; override;

    // Gestion des processus
    procedure RegisterProcess(AProcess: TWorkflowProcess);
    function GetProcess(AId: string): TWorkflowProcess;

    // Gestion des instances
    function CreateInstance(AProcessId: string): TWorkflowInstance;
    function GetInstance(AId: string): TWorkflowInstance;

    // Exécution
    procedure StartInstance(AInstance: TWorkflowInstance);
    procedure CompleteActivity(AInstance: TWorkflowInstance;
      AData: TStringList = nil);
    function EvaluateCondition(ACondition: string;
      AInstance: TWorkflowInstance): Boolean;

    property OnActivityStarted: TWorkflowEvent read FOnActivityStarted
      write FOnActivityStarted;
    property OnActivityCompleted: TWorkflowEvent read FOnActivityCompleted
      write FOnActivityCompleted;
  end;

implementation

constructor TWorkflowEngine.Create;  
begin
  inherited Create;
  FProcesses := TObjectList<TWorkflowProcess>.Create(False);
  FInstances := TObjectList<TWorkflowInstance>.Create(True);
end;

destructor TWorkflowEngine.Destroy;  
begin
  FProcesses.Free;
  FInstances.Free;
  inherited Destroy;
end;

procedure TWorkflowEngine.RegisterProcess(AProcess: TWorkflowProcess);  
begin
  FProcesses.Add(AProcess);
end;

function TWorkflowEngine.GetProcess(AId: string): TWorkflowProcess;  
var
  Process: TWorkflowProcess;
begin
  Result := nil;
  for Process in FProcesses do
    if Process.Id = AId then
      Exit(Process);
end;

function TWorkflowEngine.CreateInstance(AProcessId: string): TWorkflowInstance;  
var
  Process: TWorkflowProcess;
begin
  Process := GetProcess(AProcessId);
  if Process = nil then
    raise Exception.Create('Processus non trouvé: ' + AProcessId);

  Result := TWorkflowInstance.Create;
  Result.ProcessId := AProcessId;
  Result.CurrentActivity := Process.GetStartActivity;
  FInstances.Add(Result);
end;

function TWorkflowEngine.GetInstance(AId: string): TWorkflowInstance;  
var
  Instance: TWorkflowInstance;
begin
  Result := nil;
  for Instance in FInstances do
    if Instance.Id = AId then
      Exit(Instance);
end;

procedure TWorkflowEngine.StartInstance(AInstance: TWorkflowInstance);  
begin
  if AInstance.Status <> asNotStarted then
    raise Exception.Create('Instance déjà démarrée');

  AInstance.Status := asActive;
  AInstance.StartTime := Now;

  if Assigned(FOnActivityStarted) then
    FOnActivityStarted(AInstance, AInstance.CurrentActivity);
end;

procedure TWorkflowEngine.CompleteActivity(AInstance: TWorkflowInstance;
  AData: TStringList = nil);
var
  Process: TWorkflowProcess;
  CurrentActivity: TActivity;
  Transition: TTransition;
  NextActivity: TActivity;
  i: Integer;
begin
  if AInstance.Status <> asActive then
    raise Exception.Create('Instance non active');

  // Mettre à jour les variables si fournies
  if Assigned(AData) then
    for i := 0 to AData.Count - 1 do
      AInstance.SetVariable(AData.Names[i], AData.ValueFromIndex[i]);

  CurrentActivity := AInstance.CurrentActivity;

  if Assigned(FOnActivityCompleted) then
    FOnActivityCompleted(AInstance, CurrentActivity);

  // Si c'est une activité de fin, terminer l'instance
  if CurrentActivity.ActivityType = atEnd then
  begin
    AInstance.Status := asCompleted;
    AInstance.EndTime := Now;
    Exit;
  end;

  // Trouver la prochaine activité
  Process := GetProcess(AInstance.ProcessId);
  NextActivity := nil;

  for i := 0 to CurrentActivity.FOutgoingTransitions.Count - 1 do
  begin
    Transition := TTransition(CurrentActivity.FOutgoingTransitions[i]);

    // Évaluer la condition si présente
    if Transition.TransitionType = ttConditional then
    begin
      if EvaluateCondition(Transition.Condition, AInstance) then
      begin
        NextActivity := Transition.TargetActivity;
        Break;
      end;
    end
    else
    begin
      NextActivity := Transition.TargetActivity;
      Break;
    end;
  end;

  if NextActivity = nil then
    raise Exception.Create('Aucune transition valide trouvée');

  AInstance.CurrentActivity := NextActivity;

  if Assigned(FOnActivityStarted) then
    FOnActivityStarted(AInstance, NextActivity);
end;

function TWorkflowEngine.EvaluateCondition(ACondition: string;
  AInstance: TWorkflowInstance): Boolean;
var
  Expression: string;
begin
  // Système simple d'évaluation de conditions
  // Format: "variable operator value"
  // Exemple: "amount > 1000"

  Expression := StringReplace(ACondition, ' ', '', [rfReplaceAll]);

  // Ici, implémentation simplifiée
  // Dans un système réel, utiliser un parser d'expressions
  Result := True; // Par défaut

  // TODO: Implémenter un évaluateur d'expressions complet
end;

end.
```

---

## Persistance multi-plateforme

Système de sauvegarde compatible Windows et Ubuntu :

```pascal
unit WorkflowPersistence;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb,
  {$IFDEF WINDOWS}
  sqlite3conn
  {$ENDIF}
  {$IFDEF UNIX}
  sqlite3conn
  {$ENDIF}
  , WorkflowModel;

type
  TWorkflowDatabase = class
  private
    FConnection: TSQLConnection;
    FTransaction: TSQLTransaction;
    FQuery: TSQLQuery;
    FDatabasePath: string;

    procedure CreateTables;
    procedure InitializeConnection;
  public
    constructor Create(const ADatabasePath: string);
    destructor Destroy; override;

    // Opérations sur les processus
    procedure SaveProcess(AProcess: TWorkflowProcess);
    function LoadProcess(AId: string): TWorkflowProcess;

    // Opérations sur les instances
    procedure SaveInstance(AInstance: TWorkflowInstance);
    function LoadInstance(AId: string): TWorkflowInstance;
    procedure UpdateInstanceStatus(AId: string; AStatus: TActivityStatus);

    property Connection: TSQLConnection read FConnection;
  end;

implementation

constructor TWorkflowDatabase.Create(const ADatabasePath: string);  
begin
  inherited Create;
  FDatabasePath := ADatabasePath;

  FConnection := TSQLite3Connection.Create(nil);
  FTransaction := TSQLTransaction.Create(nil);
  FQuery := TSQLQuery.Create(nil);

  InitializeConnection;
  CreateTables;
end;

destructor TWorkflowDatabase.Destroy;  
begin
  FQuery.Free;
  FTransaction.Free;
  FConnection.Free;
  inherited Destroy;
end;

procedure TWorkflowDatabase.InitializeConnection;  
begin
  {$IFDEF WINDOWS}
  // Chemin Windows
  FConnection.DatabaseName := FDatabasePath;
  {$ENDIF}

  {$IFDEF UNIX}
  // Chemin Linux/Ubuntu
  FConnection.DatabaseName := FDatabasePath;
  {$ENDIF}

  FConnection.Transaction := FTransaction;
  FQuery.Database := FConnection;
  FQuery.Transaction := FTransaction;

  FConnection.Open;
end;

procedure TWorkflowDatabase.CreateTables;  
begin
  FTransaction.Active := True;

  try
    // Table des processus
    FQuery.SQL.Text :=
      'CREATE TABLE IF NOT EXISTS workflow_processes (' +
      '  id TEXT PRIMARY KEY,' +
      '  name TEXT NOT NULL,' +
      '  description TEXT,' +
      '  version TEXT,' +
      '  definition TEXT,' +
      '  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP' +
      ')';
    FQuery.ExecSQL;

    // Table des activités
    FQuery.SQL.Text :=
      'CREATE TABLE IF NOT EXISTS workflow_activities (' +
      '  id TEXT PRIMARY KEY,' +
      '  process_id TEXT NOT NULL,' +
      '  name TEXT NOT NULL,' +
      '  activity_type INTEGER,' +
      '  description TEXT,' +
      '  assigned_to TEXT,' +
      '  position_x INTEGER,' +
      '  position_y INTEGER,' +
      '  FOREIGN KEY (process_id) REFERENCES workflow_processes(id)' +
      ')';
    FQuery.ExecSQL;

    // Table des transitions
    FQuery.SQL.Text :=
      'CREATE TABLE IF NOT EXISTS workflow_transitions (' +
      '  id TEXT PRIMARY KEY,' +
      '  process_id TEXT NOT NULL,' +
      '  name TEXT,' +
      '  source_activity_id TEXT NOT NULL,' +
      '  target_activity_id TEXT NOT NULL,' +
      '  transition_type INTEGER,' +
      '  condition TEXT,' +
      '  FOREIGN KEY (process_id) REFERENCES workflow_processes(id)' +
      ')';
    FQuery.ExecSQL;

    // Table des instances
    FQuery.SQL.Text :=
      'CREATE TABLE IF NOT EXISTS workflow_instances (' +
      '  id TEXT PRIMARY KEY,' +
      '  process_id TEXT NOT NULL,' +
      '  current_activity_id TEXT,' +
      '  status INTEGER,' +
      '  variables TEXT,' +
      '  start_time TIMESTAMP,' +
      '  end_time TIMESTAMP,' +
      '  FOREIGN KEY (process_id) REFERENCES workflow_processes(id)' +
      ')';
    FQuery.ExecSQL;

    // Table de l'historique
    FQuery.SQL.Text :=
      'CREATE TABLE IF NOT EXISTS workflow_history (' +
      '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
      '  instance_id TEXT NOT NULL,' +
      '  activity_id TEXT NOT NULL,' +
      '  timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP,' +
      '  action TEXT,' +
      '  user_id TEXT,' +
      '  FOREIGN KEY (instance_id) REFERENCES workflow_instances(id)' +
      ')';
    FQuery.ExecSQL;

    FTransaction.Commit;
  except
    FTransaction.Rollback;
    raise;
  end;
end;

procedure TWorkflowDatabase.SaveProcess(AProcess: TWorkflowProcess);  
var
  Activity: TActivity;
  Transition: TTransition;
begin
  FTransaction.Active := True;

  try
    // Sauvegarder le processus
    FQuery.SQL.Text :=
      'INSERT OR REPLACE INTO workflow_processes ' +
      '(id, name, description, version) VALUES ' +
      '(:id, :name, :description, :version)';
    FQuery.ParamByName('id').AsString := AProcess.Id;
    FQuery.ParamByName('name').AsString := AProcess.Name;
    FQuery.ParamByName('description').AsString := AProcess.Description;
    FQuery.ParamByName('version').AsString := AProcess.Version;
    FQuery.ExecSQL;

    // Sauvegarder les activités
    for Activity in AProcess.Activities do
    begin
      FQuery.SQL.Text :=
        'INSERT OR REPLACE INTO workflow_activities ' +
        '(id, process_id, name, activity_type, description, ' +
        'assigned_to, position_x, position_y) VALUES ' +
        '(:id, :process_id, :name, :activity_type, :description, ' +
        ':assigned_to, :position_x, :position_y)';
      FQuery.ParamByName('id').AsString := Activity.Id;
      FQuery.ParamByName('process_id').AsString := AProcess.Id;
      FQuery.ParamByName('name').AsString := Activity.Name;
      FQuery.ParamByName('activity_type').AsInteger := Ord(Activity.ActivityType);
      FQuery.ParamByName('description').AsString := Activity.Description;
      FQuery.ParamByName('assigned_to').AsString := Activity.AssignedTo;
      FQuery.ParamByName('position_x').AsInteger := Activity.Position.X;
      FQuery.ParamByName('position_y').AsInteger := Activity.Position.Y;
      FQuery.ExecSQL;
    end;

    // Sauvegarder les transitions
    for Transition in AProcess.Transitions do
    begin
      FQuery.SQL.Text :=
        'INSERT OR REPLACE INTO workflow_transitions ' +
        '(id, process_id, name, source_activity_id, target_activity_id, ' +
        'transition_type, condition) VALUES ' +
        '(:id, :process_id, :name, :source_activity_id, :target_activity_id, ' +
        ':transition_type, :condition)';
      FQuery.ParamByName('id').AsString := Transition.Id;
      FQuery.ParamByName('process_id').AsString := AProcess.Id;
      FQuery.ParamByName('name').AsString := Transition.Name;
      FQuery.ParamByName('source_activity_id').AsString :=
        Transition.SourceActivity.Id;
      FQuery.ParamByName('target_activity_id').AsString :=
        Transition.TargetActivity.Id;
      FQuery.ParamByName('transition_type').AsInteger :=
        Ord(Transition.TransitionType);
      FQuery.ParamByName('condition').AsString := Transition.Condition;
      FQuery.ExecSQL;
    end;

    FTransaction.Commit;
  except
    FTransaction.Rollback;
    raise;
  end;
end;

function TWorkflowDatabase.LoadProcess(AId: string): TWorkflowProcess;  
begin
  Result := TWorkflowProcess.Create;

  // Charger le processus
  FQuery.SQL.Text :=
    'SELECT * FROM workflow_processes WHERE id = :id';
  FQuery.ParamByName('id').AsString := AId;
  FQuery.Open;

  if not FQuery.EOF then
  begin
    Result.Id := FQuery.FieldByName('id').AsString;
    Result.Name := FQuery.FieldByName('name').AsString;
    Result.Description := FQuery.FieldByName('description').AsString;
    Result.Version := FQuery.FieldByName('version').AsString;
  end;

  FQuery.Close;

  // TODO: Charger les activités et transitions
end;

procedure TWorkflowDatabase.SaveInstance(AInstance: TWorkflowInstance);  
begin
  FTransaction.Active := True;

  try
    FQuery.SQL.Text :=
      'INSERT OR REPLACE INTO workflow_instances ' +
      '(id, process_id, current_activity_id, status, variables, ' +
      'start_time, end_time) VALUES ' +
      '(:id, :process_id, :current_activity_id, :status, :variables, ' +
      ':start_time, :end_time)';
    FQuery.ParamByName('id').AsString := AInstance.Id;
    FQuery.ParamByName('process_id').AsString := AInstance.ProcessId;
    if Assigned(AInstance.CurrentActivity) then
      FQuery.ParamByName('current_activity_id').AsString :=
        AInstance.CurrentActivity.Id
    else
      FQuery.ParamByName('current_activity_id').Clear;
    FQuery.ParamByName('status').AsInteger := Ord(AInstance.Status);
    FQuery.ParamByName('variables').AsString := AInstance.Variables.Text;
    FQuery.ParamByName('start_time').AsDateTime := AInstance.StartTime;
    FQuery.ParamByName('end_time').AsDateTime := AInstance.EndTime;
    FQuery.ExecSQL;

    FTransaction.Commit;
  except
    FTransaction.Rollback;
    raise;
  end;
end;

function TWorkflowDatabase.LoadInstance(AId: string): TWorkflowInstance;  
begin
  Result := TWorkflowInstance.Create;

  FQuery.SQL.Text :=
    'SELECT * FROM workflow_instances WHERE id = :id';
  FQuery.ParamByName('id').AsString := AId;
  FQuery.Open;

  if not FQuery.EOF then
  begin
    Result.Id := FQuery.FieldByName('id').AsString;
    Result.ProcessId := FQuery.FieldByName('process_id').AsString;
    Result.Status := TActivityStatus(FQuery.FieldByName('status').AsInteger);
    Result.Variables.Text := FQuery.FieldByName('variables').AsString;
    Result.StartTime := FQuery.FieldByName('start_time').AsDateTime;
    Result.EndTime := FQuery.FieldByName('end_time').AsDateTime;
  end;

  FQuery.Close;
end;

procedure TWorkflowDatabase.UpdateInstanceStatus(AId: string;
  AStatus: TActivityStatus);
begin
  FTransaction.Active := True;

  try
    FQuery.SQL.Text :=
      'UPDATE workflow_instances SET status = :status WHERE id = :id';
    FQuery.ParamByName('id').AsString := AId;
    FQuery.ParamByName('status').AsInteger := Ord(AStatus);
    FQuery.ExecSQL;

    FTransaction.Commit;
  except
    FTransaction.Rollback;
    raise;
  end;
end;

end.
```

---

## Interface graphique - Éditeur de workflow

Créons un éditeur visuel pour concevoir les workflows :

```pascal
unit WorkflowDesigner;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Menus, ComCtrls, StdCtrls, WorkflowModel;

type
  TWorkflowDesignerForm = class(TForm)
    MainMenu1: TMainMenu;
    MenuFile: TMenuItem;
    MenuNew: TMenuItem;
    MenuOpen: TMenuItem;
    MenuSave: TMenuItem;
    MenuEdit: TMenuItem;
    MenuAddActivity: TMenuItem;
    ToolBar1: TToolBar;
    BtnStart: TToolButton;
    BtnTask: TToolButton;
    BtnDecision: TToolButton;
    BtnEnd: TToolButton;
    PaintBox: TPaintBox;
    StatusBar1: TStatusBar;

    procedure FormCreate(Sender: TObject);
    procedure PaintBoxPaint(Sender: TObject);
    procedure PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure BtnStartClick(Sender: TObject);
    procedure BtnTaskClick(Sender: TObject);
    procedure BtnDecisionClick(Sender: TObject);
    procedure BtnEndClick(Sender: TObject);
    procedure MenuNewClick(Sender: TObject);
    procedure MenuSaveClick(Sender: TObject);
    procedure MenuOpenClick(Sender: TObject);
  private
    FProcess: TWorkflowProcess;
    FSelectedActivity: TActivity;
    FDraggingActivity: TActivity;
    FConnectingFrom: TActivity;
    FCurrentTool: TActivityType;
    FDragOffset: TPoint;

    procedure DrawActivity(AActivity: TActivity);
    procedure DrawTransition(ATransition: TTransition);
    function FindActivityAt(X, Y: Integer): TActivity;
    function IsPointInActivity(AActivity: TActivity; X, Y: Integer): Boolean;
  public
    property Process: TWorkflowProcess read FProcess;
  end;

implementation

procedure TWorkflowDesignerForm.FormCreate(Sender: TObject);  
begin
  FProcess := TWorkflowProcess.Create;
  FProcess.Name := 'Nouveau processus';
  FCurrentTool := atTask;
  FSelectedActivity := nil;
  FDraggingActivity := nil;
  FConnectingFrom := nil;
end;

procedure TWorkflowDesignerForm.PaintBoxPaint(Sender: TObject);  
var
  Activity: TActivity;
  Transition: TTransition;
begin
  // Dessiner la grille
  PaintBox.Canvas.Pen.Color := clSilver;
  PaintBox.Canvas.Pen.Style := psDot;

  // Dessiner toutes les transitions d'abord
  for Transition in FProcess.Transitions do
    DrawTransition(Transition);

  // Puis dessiner toutes les activités par-dessus
  for Activity in FProcess.Activities do
    DrawActivity(Activity);
end;

procedure TWorkflowDesignerForm.DrawActivity(AActivity: TActivity);  
var
  Rect: TRect;
  TextWidth, TextHeight: Integer;
const
  ACTIVITY_WIDTH = 100;
  ACTIVITY_HEIGHT = 60;
begin
  Rect.Left := AActivity.Position.X - ACTIVITY_WIDTH div 2;
  Rect.Top := AActivity.Position.Y - ACTIVITY_HEIGHT div 2;
  Rect.Right := AActivity.Position.X + ACTIVITY_WIDTH div 2;
  Rect.Bottom := AActivity.Position.Y + ACTIVITY_HEIGHT div 2;

  // Définir la couleur selon le type d'activité
  case AActivity.ActivityType of
    atStart:
      begin
        PaintBox.Canvas.Brush.Color := clLime;
        PaintBox.Canvas.Ellipse(Rect);
      end;
    atTask:
      begin
        PaintBox.Canvas.Brush.Color := clSkyBlue;
        PaintBox.Canvas.Rectangle(Rect);
      end;
    atDecision:
      begin
        PaintBox.Canvas.Brush.Color := clYellow;
        // Dessiner un losange
        PaintBox.Canvas.Polygon([
          Point(AActivity.Position.X, Rect.Top),
          Point(Rect.Right, AActivity.Position.Y),
          Point(AActivity.Position.X, Rect.Bottom),
          Point(Rect.Left, AActivity.Position.Y)
        ]);
      end;
    atEnd:
      begin
        PaintBox.Canvas.Brush.Color := clRed;
        PaintBox.Canvas.Ellipse(Rect);
      end;
  end;

  // Surbrillance si sélectionné
  if AActivity = FSelectedActivity then
  begin
    PaintBox.Canvas.Pen.Color := clBlue;
    PaintBox.Canvas.Pen.Width := 3;
    PaintBox.Canvas.Brush.Style := bsClear;
    PaintBox.Canvas.Rectangle(Rect.Left - 5, Rect.Top - 5,
                               Rect.Right + 5, Rect.Bottom + 5);
    PaintBox.Canvas.Pen.Width := 1;
    PaintBox.Canvas.Brush.Style := bsSolid;
  end;

  // Dessiner le texte
  PaintBox.Canvas.Font.Color := clBlack;
  TextWidth := PaintBox.Canvas.TextWidth(AActivity.Name);
  TextHeight := PaintBox.Canvas.TextHeight(AActivity.Name);
  PaintBox.Canvas.TextOut(
    AActivity.Position.X - TextWidth div 2,
    AActivity.Position.Y - TextHeight div 2,
    AActivity.Name
  );
end;

procedure TWorkflowDesignerForm.DrawTransition(ATransition: TTransition);  
var
  StartPoint, EndPoint: TPoint;
  ArrowSize: Integer;
begin
  if not Assigned(ATransition.SourceActivity) or
     not Assigned(ATransition.TargetActivity) then
    Exit;

  StartPoint := ATransition.SourceActivity.Position;
  EndPoint := ATransition.TargetActivity.Position;

  // Dessiner la ligne
  PaintBox.Canvas.Pen.Color := clBlack;
  PaintBox.Canvas.Pen.Width := 2;

  if ATransition.TransitionType = ttConditional then
    PaintBox.Canvas.Pen.Style := psDash
  else
    PaintBox.Canvas.Pen.Style := psSolid;

  PaintBox.Canvas.MoveTo(StartPoint.X, StartPoint.Y);
  PaintBox.Canvas.LineTo(EndPoint.X, EndPoint.Y);

  // Dessiner la flèche
  ArrowSize := 10;
  // TODO: Calculer l'angle et dessiner une vraie flèche

  PaintBox.Canvas.Pen.Style := psSolid;
  PaintBox.Canvas.Pen.Width := 1;
end;

function TWorkflowDesignerForm.FindActivityAt(X, Y: Integer): TActivity;  
var
  Activity: TActivity;
begin
  Result := nil;
  for Activity in FProcess.Activities do
    if IsPointInActivity(Activity, X, Y) then
      Exit(Activity);
end;

function TWorkflowDesignerForm.IsPointInActivity(AActivity: TActivity;
  X, Y: Integer): Boolean;
const
  ACTIVITY_WIDTH = 100;
  ACTIVITY_HEIGHT = 60;
var
  Rect: TRect;
begin
  Rect.Left := AActivity.Position.X - ACTIVITY_WIDTH div 2;
  Rect.Top := AActivity.Position.Y - ACTIVITY_HEIGHT div 2;
  Rect.Right := AActivity.Position.X + ACTIVITY_WIDTH div 2;
  Rect.Bottom := AActivity.Position.Y + ACTIVITY_HEIGHT div 2;

  Result := (X >= Rect.Left) and (X <= Rect.Right) and
            (Y >= Rect.Top) and (Y <= Rect.Bottom);
end;

procedure TWorkflowDesignerForm.PaintBoxMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Activity: TActivity;
begin
  if Button = mbLeft then
  begin
    Activity := FindActivityAt(X, Y);

    if Assigned(Activity) then
    begin
      // Commencer à déplacer l'activité
      FDraggingActivity := Activity;
      FDragOffset.X := X - Activity.Position.X;
      FDragOffset.Y := Y - Activity.Position.Y;
      FSelectedActivity := Activity;
    end
    else
    begin
      // Créer une nouvelle activité
      Activity := FProcess.AddActivity('Activité ' +
        IntToStr(FProcess.Activities.Count + 1), FCurrentTool);
      Activity.Position := Point(X, Y);
      FSelectedActivity := Activity;
    end;

    PaintBox.Invalidate;
  end
  else if Button = mbRight then
  begin
    // Commencer à créer une connexion
    Activity := FindActivityAt(X, Y);
    if Assigned(Activity) then
      FConnectingFrom := Activity;
  end;
end;

procedure TWorkflowDesignerForm.PaintBoxMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FDraggingActivity) then
  begin
    FDraggingActivity.Position := Point(X - FDragOffset.X, Y - FDragOffset.Y);
    PaintBox.Invalidate;

    StatusBar1.SimpleText := Format('Position: %d, %d',
      [FDraggingActivity.Position.X, FDraggingActivity.Position.Y]);
  end;
end;

procedure TWorkflowDesignerForm.PaintBoxMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  TargetActivity: TActivity;
begin
  if Button = mbLeft then
  begin
    FDraggingActivity := nil;
  end
  else if Button = mbRight then
  begin
    if Assigned(FConnectingFrom) then
    begin
      TargetActivity := FindActivityAt(X, Y);
      if Assigned(TargetActivity) and (TargetActivity <> FConnectingFrom) then
      begin
        FProcess.AddTransition(FConnectingFrom, TargetActivity);
        PaintBox.Invalidate;
      end;
      FConnectingFrom := nil;
    end;
  end;
end;

procedure TWorkflowDesignerForm.BtnStartClick(Sender: TObject);  
begin
  FCurrentTool := atStart;
  StatusBar1.SimpleText := 'Outil: Début';
end;

procedure TWorkflowDesignerForm.BtnTaskClick(Sender: TObject);  
begin
  FCurrentTool := atTask;
  StatusBar1.SimpleText := 'Outil: Tâche';
end;

procedure TWorkflowDesignerForm.BtnDecisionClick(Sender: TObject);  
begin
  FCurrentTool := atDecision;
  StatusBar1.SimpleText := 'Outil: Décision';
end;

procedure TWorkflowDesignerForm.BtnEndClick(Sender: TObject);  
begin
  FCurrentTool := atEnd;
  StatusBar1.SimpleText := 'Outil: Fin';
end;

procedure TWorkflowDesignerForm.MenuNewClick(Sender: TObject);  
begin
  FProcess.Free;
  FProcess := TWorkflowProcess.Create;
  FProcess.Name := 'Nouveau processus';
  FSelectedActivity := nil;
  PaintBox.Invalidate;
end;

procedure TWorkflowDesignerForm.MenuSaveClick(Sender: TObject);  
var
  SaveDialog: TSaveDialog;
begin
  SaveDialog := TSaveDialog.Create(nil);
  try
    SaveDialog.Filter := 'Fichiers workflow (*.wflow)|*.wflow';
    SaveDialog.DefaultExt := 'wflow';

    if SaveDialog.Execute then
    begin
      FProcess.SaveToFile(SaveDialog.FileName);
      StatusBar1.SimpleText := 'Processus sauvegardé: ' + SaveDialog.FileName;
    end;
  finally
    SaveDialog.Free;
  end;
end;

procedure TWorkflowDesignerForm.MenuOpenClick(Sender: TObject);  
var
  OpenDialog: TOpenDialog;
begin
  OpenDialog := TOpenDialog.Create(nil);
  try
    OpenDialog.Filter := 'Fichiers workflow (*.wflow)|*.wflow';

    if OpenDialog.Execute then
    begin
      FProcess.Free;
      FProcess := TWorkflowProcess.Create;
      FProcess.LoadFromFile(OpenDialog.FileName);
      PaintBox.Invalidate;
      StatusBar1.SimpleText := 'Processus chargé: ' + OpenDialog.FileName;
    end;
  finally
    OpenDialog.Free;
  end;
end;

end.
```

---

## Interface d'exécution - Tableau de bord

Créons maintenant une interface pour suivre l'exécution des workflows :

```pascal
unit WorkflowDashboard;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ExtCtrls, Grids, WorkflowModel, WorkflowEngine;

type
  TWorkflowDashboardForm = class(TForm)
    PageControl1: TPageControl;
    TabInstances: TTabSheet;
    TabHistory: TTabSheet;
    InstancesGrid: TStringGrid;
    HistoryGrid: TStringGrid;
    Panel1: TPanel;
    BtnRefresh: TButton;
    BtnStartNew: TButton;
    BtnComplete: TButton;
    ProcessComboBox: TComboBox;
    Label1: TLabel;

    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BtnRefreshClick(Sender: TObject);
    procedure BtnStartNewClick(Sender: TObject);
    procedure BtnCompleteClick(Sender: TObject);
    procedure InstancesGridSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
  private
    FEngine: TWorkflowEngine;
    FSelectedInstance: TWorkflowInstance;

    procedure LoadProcesses;
    procedure LoadInstances;
    procedure LoadHistory(AInstanceId: string);
    procedure OnActivityStarted(Instance: TWorkflowInstance;
      Activity: TActivity);
    procedure OnActivityCompleted(Instance: TWorkflowInstance;
      Activity: TActivity);
  public
    property Engine: TWorkflowEngine read FEngine write FEngine;
  end;

implementation

procedure TWorkflowDashboardForm.FormCreate(Sender: TObject);  
begin
  // Initialiser la grille des instances
  InstancesGrid.ColCount := 6;
  InstancesGrid.RowCount := 1;
  InstancesGrid.Cells[0, 0] := 'ID Instance';
  InstancesGrid.Cells[1, 0] := 'Processus';
  InstancesGrid.Cells[2, 0] := 'Activité courante';
  InstancesGrid.Cells[3, 0] := 'Statut';
  InstancesGrid.Cells[4, 0] := 'Début';
  InstancesGrid.Cells[5, 0] := 'Fin';

  InstancesGrid.ColWidths[0] := 150;
  InstancesGrid.ColWidths[1] := 150;
  InstancesGrid.ColWidths[2] := 150;
  InstancesGrid.ColWidths[3] := 100;
  InstancesGrid.ColWidths[4] := 120;
  InstancesGrid.ColWidths[5] := 120;

  // Initialiser la grille de l'historique
  HistoryGrid.ColCount := 4;
  HistoryGrid.RowCount := 1;
  HistoryGrid.Cells[0, 0] := 'Horodatage';
  HistoryGrid.Cells[1, 0] := 'Activité';
  HistoryGrid.Cells[2, 0] := 'Action';
  HistoryGrid.Cells[3, 0] := 'Utilisateur';

  HistoryGrid.ColWidths[0] := 150;
  HistoryGrid.ColWidths[1] := 200;
  HistoryGrid.ColWidths[2] := 150;
  HistoryGrid.ColWidths[3] := 150;
end;

procedure TWorkflowDashboardForm.FormShow(Sender: TObject);  
begin
  if Assigned(FEngine) then
  begin
    LoadProcesses;
    LoadInstances;
  end;
end;

procedure TWorkflowDashboardForm.LoadProcesses;  
var
  Process: TWorkflowProcess;
begin
  ProcessComboBox.Clear;

  if not Assigned(FEngine) then
    Exit;

  // Charger la liste des processus disponibles
  // Note: Dans une implémentation complète, ceci viendrait de la base de données
  ProcessComboBox.Items.Add('Processus de validation');
  ProcessComboBox.Items.Add('Processus d''approbation');
  ProcessComboBox.Items.Add('Processus de traitement');

  if ProcessComboBox.Items.Count > 0 then
    ProcessComboBox.ItemIndex := 0;
end;

procedure TWorkflowDashboardForm.LoadInstances;  
var
  Instance: TWorkflowInstance;
  Row: Integer;
  StatusStr: string;
begin
  if not Assigned(FEngine) then
    Exit;

  // Effacer les lignes existantes (sauf l'en-tête)
  InstancesGrid.RowCount := 1;

  // Charger toutes les instances actives
  Row := 1;
  for Instance in FEngine.FInstances do
  begin
    InstancesGrid.RowCount := Row + 1;

    InstancesGrid.Cells[0, Row] := Copy(Instance.Id, 1, 30);
    InstancesGrid.Cells[1, Row] := Copy(Instance.ProcessId, 1, 30);

    if Assigned(Instance.CurrentActivity) then
      InstancesGrid.Cells[2, Row] := Instance.CurrentActivity.Name
    else
      InstancesGrid.Cells[2, Row] := '-';

    case Instance.Status of
      asNotStarted: StatusStr := 'Non démarré';
      asActive: StatusStr := 'Actif';
      asCompleted: StatusStr := 'Terminé';
      asCancelled: StatusStr := 'Annulé';
    end;
    InstancesGrid.Cells[3, Row] := StatusStr;

    InstancesGrid.Cells[4, Row] := DateTimeToStr(Instance.StartTime);

    if Instance.Status = asCompleted then
      InstancesGrid.Cells[5, Row] := DateTimeToStr(Instance.EndTime)
    else
      InstancesGrid.Cells[5, Row] := '-';

    Inc(Row);
  end;
end;

procedure TWorkflowDashboardForm.LoadHistory(AInstanceId: string);  
begin
  // TODO: Charger l'historique depuis la base de données
  HistoryGrid.RowCount := 1;
end;

procedure TWorkflowDashboardForm.BtnRefreshClick(Sender: TObject);  
begin
  LoadInstances;
end;

procedure TWorkflowDashboardForm.BtnStartNewClick(Sender: TObject);  
var
  Instance: TWorkflowInstance;
  ProcessId: string;
begin
  if not Assigned(FEngine) then
  begin
    ShowMessage('Aucun moteur de workflow configuré');
    Exit;
  end;

  if ProcessComboBox.ItemIndex < 0 then
  begin
    ShowMessage('Veuillez sélectionner un processus');
    Exit;
  end;

  // Dans une implémentation réelle, récupérer le vrai ID du processus
  ProcessId := 'process-' + IntToStr(ProcessComboBox.ItemIndex + 1);

  try
    Instance := FEngine.CreateInstance(ProcessId);
    FEngine.StartInstance(Instance);

    ShowMessage('Nouvelle instance créée: ' + Copy(Instance.Id, 1, 30));
    LoadInstances;
  except
    on E: Exception do
      ShowMessage('Erreur lors de la création: ' + E.Message);
  end;
end;

procedure TWorkflowDashboardForm.BtnCompleteClick(Sender: TObject);  
begin
  if not Assigned(FSelectedInstance) then
  begin
    ShowMessage('Veuillez sélectionner une instance');
    Exit;
  end;

  if FSelectedInstance.Status <> asActive then
  begin
    ShowMessage('Cette instance n''est pas active');
    Exit;
  end;

  try
    FEngine.CompleteActivity(FSelectedInstance);
    ShowMessage('Activité complétée. Activité courante: ' +
                FSelectedInstance.CurrentActivity.Name);
    LoadInstances;
  except
    on E: Exception do
      ShowMessage('Erreur: ' + E.Message);
  end;
end;

procedure TWorkflowDashboardForm.InstancesGridSelectCell(Sender: TObject;
  aCol, aRow: Integer; var CanSelect: Boolean);
var
  InstanceId: string;
  Instance: TWorkflowInstance;
begin
  if aRow = 0 then
    Exit;

  InstanceId := InstancesGrid.Cells[0, aRow];

  // Rechercher l'instance correspondante
  FSelectedInstance := nil;
  for Instance in FEngine.FInstances do
  begin
    if Copy(Instance.Id, 1, 30) = InstanceId then
    begin
      FSelectedInstance := Instance;
      Break;
    end;
  end;

  if Assigned(FSelectedInstance) then
    LoadHistory(FSelectedInstance.Id);
end;

procedure TWorkflowDashboardForm.OnActivityStarted(Instance: TWorkflowInstance;
  Activity: TActivity);
begin
  // Appelé quand une activité démarre
  // Peut être utilisé pour notifier les utilisateurs, logger, etc.
end;

procedure TWorkflowDashboardForm.OnActivityCompleted(Instance: TWorkflowInstance;
  Activity: TActivity);
begin
  // Appelé quand une activité se termine
  // Peut déclencher des actions automatiques, notifications, etc.
end;

end.
```

---

## Système de règles et conditions

Pour gérer les décisions dans le workflow, implémentons un évaluateur d'expressions :

```pascal
unit WorkflowRules;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WorkflowModel;

type
  TOperator = (opEqual, opNotEqual, opGreater, opLess,
               opGreaterOrEqual, opLessOrEqual,
               opAnd, opOr, opNot, opContains);

  TExpressionNode = class
  private
    FOperator: TOperator;
    FLeftOperand: string;
    FRightOperand: string;
    FLeftChild: TExpressionNode;
    FRightChild: TExpressionNode;
  public
    destructor Destroy; override;

    property Operator: TOperator read FOperator write FOperator;
    property LeftOperand: string read FLeftOperand write FLeftOperand;
    property RightOperand: string read FRightOperand write FRightOperand;
    property LeftChild: TExpressionNode read FLeftChild write FLeftChild;
    property RightChild: TExpressionNode read FRightChild write FRightChild;
  end;

  TRuleEngine = class
  private
    function ParseExpression(const AExpression: string): TExpressionNode;
    function EvaluateNode(ANode: TExpressionNode;
      AInstance: TWorkflowInstance): Boolean;
    function CompareValues(const ALeft, ARight: string;
      AOperator: TOperator): Boolean;
    function GetVariableValue(const AVarName: string;
      AInstance: TWorkflowInstance): string;
  public
    function Evaluate(const ACondition: string;
      AInstance: TWorkflowInstance): Boolean;
  end;

implementation

uses
  StrUtils;

destructor TExpressionNode.Destroy;  
begin
  if Assigned(FLeftChild) then
    FLeftChild.Free;
  if Assigned(FRightChild) then
    FRightChild.Free;
  inherited Destroy;
end;

function TRuleEngine.Evaluate(const ACondition: string;
  AInstance: TWorkflowInstance): Boolean;
var
  Node: TExpressionNode;
begin
  Result := False;

  if Trim(ACondition) = '' then
  begin
    Result := True;  // Pas de condition = toujours vrai
    Exit;
  end;

  try
    Node := ParseExpression(ACondition);
    try
      Result := EvaluateNode(Node, AInstance);
    finally
      Node.Free;
    end;
  except
    on E: Exception do
    begin
      // En cas d'erreur de parsing, logger et retourner False
      WriteLn('Erreur évaluation condition: ', E.Message);
      Result := False;
    end;
  end;
end;

function TRuleEngine.ParseExpression(const AExpression: string): TExpressionNode;  
var
  Expr: string;
  Pos: Integer;
begin
  Result := TExpressionNode.Create;
  Expr := Trim(AExpression);

  // Exemple simple: "variable operator value"
  // Exemple: "amount > 1000"

  // Chercher les opérateurs AND, OR en premier (plus faible priorité)
  Pos := System.Pos(' AND ', UpperCase(Expr));
  if Pos > 0 then
  begin
    Result.Operator := opAnd;
    Result.LeftChild := ParseExpression(Copy(Expr, 1, Pos - 1));
    Result.RightChild := ParseExpression(Copy(Expr, Pos + 5, Length(Expr)));
    Exit;
  end;

  Pos := System.Pos(' OR ', UpperCase(Expr));
  if Pos > 0 then
  begin
    Result.Operator := opOr;
    Result.LeftChild := ParseExpression(Copy(Expr, 1, Pos - 1));
    Result.RightChild := ParseExpression(Copy(Expr, Pos + 4, Length(Expr)));
    Exit;
  end;

  // Chercher les opérateurs de comparaison
  if System.Pos('>=', Expr) > 0 then
  begin
    Pos := System.Pos('>=', Expr);
    Result.Operator := opGreaterOrEqual;
    Result.LeftOperand := Trim(Copy(Expr, 1, Pos - 1));
    Result.RightOperand := Trim(Copy(Expr, Pos + 2, Length(Expr)));
  end
  else if System.Pos('<=', Expr) > 0 then
  begin
    Pos := System.Pos('<=', Expr);
    Result.Operator := opLessOrEqual;
    Result.LeftOperand := Trim(Copy(Expr, 1, Pos - 1));
    Result.RightOperand := Trim(Copy(Expr, Pos + 2, Length(Expr)));
  end
  else if System.Pos('!=', Expr) > 0 then
  begin
    Pos := System.Pos('!=', Expr);
    Result.Operator := opNotEqual;
    Result.LeftOperand := Trim(Copy(Expr, 1, Pos - 1));
    Result.RightOperand := Trim(Copy(Expr, Pos + 2, Length(Expr)));
  end
  else if System.Pos('>', Expr) > 0 then
  begin
    Pos := System.Pos('>', Expr);
    Result.Operator := opGreater;
    Result.LeftOperand := Trim(Copy(Expr, 1, Pos - 1));
    Result.RightOperand := Trim(Copy(Expr, Pos + 1, Length(Expr)));
  end
  else if System.Pos('<', Expr) > 0 then
  begin
    Pos := System.Pos('<', Expr);
    Result.Operator := opLess;
    Result.LeftOperand := Trim(Copy(Expr, 1, Pos - 1));
    Result.RightOperand := Trim(Copy(Expr, Pos + 1, Length(Expr)));
  end
  else if System.Pos('=', Expr) > 0 then
  begin
    Pos := System.Pos('=', Expr);
    Result.Operator := opEqual;
    Result.LeftOperand := Trim(Copy(Expr, 1, Pos - 1));
    Result.RightOperand := Trim(Copy(Expr, Pos + 1, Length(Expr)));
  end
  else if System.Pos(' CONTAINS ', UpperCase(Expr)) > 0 then
  begin
    Pos := System.Pos(' CONTAINS ', UpperCase(Expr));
    Result.Operator := opContains;
    Result.LeftOperand := Trim(Copy(Expr, 1, Pos - 1));
    Result.RightOperand := Trim(Copy(Expr, Pos + 10, Length(Expr)));
  end
  else
  begin
    // Expression simple (variable seule, considérée comme booléenne)
    Result.Operator := opEqual;
    Result.LeftOperand := Expr;
    Result.RightOperand := 'true';
  end;
end;

function TRuleEngine.EvaluateNode(ANode: TExpressionNode;
  AInstance: TWorkflowInstance): Boolean;
var
  LeftValue, RightValue: string;
begin
  Result := False;

  // Si ce sont des opérateurs logiques, évaluer les enfants
  case ANode.Operator of
    opAnd:
      Result := EvaluateNode(ANode.LeftChild, AInstance) and
                EvaluateNode(ANode.RightChild, AInstance);
    opOr:
      Result := EvaluateNode(ANode.LeftChild, AInstance) or
                EvaluateNode(ANode.RightChild, AInstance);
    opNot:
      Result := not EvaluateNode(ANode.LeftChild, AInstance);
  else
    // Opérateur de comparaison
    LeftValue := GetVariableValue(ANode.LeftOperand, AInstance);
    RightValue := ANode.RightOperand;

    // Enlever les guillemets si présents
    if (Length(RightValue) > 1) and
       (RightValue[1] = '"') and
       (RightValue[Length(RightValue)] = '"') then
      RightValue := Copy(RightValue, 2, Length(RightValue) - 2);

    Result := CompareValues(LeftValue, RightValue, ANode.Operator);
  end;
end;

function TRuleEngine.CompareValues(const ALeft, ARight: string;
  AOperator: TOperator): Boolean;
var
  LeftNum, RightNum: Double;
  IsNumeric: Boolean;
begin
  Result := False;

  // Essayer de convertir en nombres
  IsNumeric := TryStrToFloat(ALeft, LeftNum) and TryStrToFloat(ARight, RightNum);

  case AOperator of
    opEqual:
      if IsNumeric then
        Result := LeftNum = RightNum
      else
        Result := ALeft = ARight;

    opNotEqual:
      if IsNumeric then
        Result := LeftNum <> RightNum
      else
        Result := ALeft <> ARight;

    opGreater:
      if IsNumeric then
        Result := LeftNum > RightNum
      else
        Result := ALeft > ARight;

    opLess:
      if IsNumeric then
        Result := LeftNum < RightNum
      else
        Result := ALeft < ARight;

    opGreaterOrEqual:
      if IsNumeric then
        Result := LeftNum >= RightNum
      else
        Result := ALeft >= ARight;

    opLessOrEqual:
      if IsNumeric then
        Result := LeftNum <= RightNum
      else
        Result := ALeft <= ARight;

    opContains:
      Result := Pos(ARight, ALeft) > 0;
  end;
end;

function TRuleEngine.GetVariableValue(const AVarName: string;
  AInstance: TWorkflowInstance): string;
begin
  Result := AInstance.GetVariable(AVarName);

  // Si la variable n'existe pas, retourner une chaîne vide
  if Result = '' then
    Result := '';
end;

end.
```

---

## Format de sauvegarde XML

Pour sauvegarder et charger les processus de workflow, utilisons le format XML :

```pascal
unit WorkflowXML;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DOM, XMLRead, XMLWrite, WorkflowModel;

type
  TWorkflowXMLSerializer = class
  public
    class procedure SaveToXML(AProcess: TWorkflowProcess;
      const AFileName: string);
    class function LoadFromXML(const AFileName: string): TWorkflowProcess;
  end;

implementation

class procedure TWorkflowXMLSerializer.SaveToXML(AProcess: TWorkflowProcess;
  const AFileName: string);
var
  Doc: TXMLDocument;
  RootNode, ProcessNode, ActivitiesNode, TransitionsNode: TDOMNode;
  ActivityNode, TransitionNode, PropNode: TDOMNode;
  Activity: TActivity;
  Transition: TTransition;
  i: Integer;
begin
  Doc := TXMLDocument.Create;
  try
    // Créer le nœud racine
    RootNode := Doc.CreateElement('workflow');
    Doc.AppendChild(RootNode);

    // Informations du processus
    ProcessNode := Doc.CreateElement('process');
    TDOMElement(ProcessNode).SetAttribute('id', AProcess.Id);
    TDOMElement(ProcessNode).SetAttribute('name', AProcess.Name);
    TDOMElement(ProcessNode).SetAttribute('version', AProcess.Version);

    PropNode := Doc.CreateElement('description');
    PropNode.TextContent := AProcess.Description;
    ProcessNode.AppendChild(PropNode);

    RootNode.AppendChild(ProcessNode);

    // Sauvegarder les activités
    ActivitiesNode := Doc.CreateElement('activities');
    ProcessNode.AppendChild(ActivitiesNode);

    for Activity in AProcess.Activities do
    begin
      ActivityNode := Doc.CreateElement('activity');
      TDOMElement(ActivityNode).SetAttribute('id', Activity.Id);
      TDOMElement(ActivityNode).SetAttribute('name', Activity.Name);
      TDOMElement(ActivityNode).SetAttribute('type',
        IntToStr(Ord(Activity.ActivityType)));
      TDOMElement(ActivityNode).SetAttribute('x',
        IntToStr(Activity.Position.X));
      TDOMElement(ActivityNode).SetAttribute('y',
        IntToStr(Activity.Position.Y));

      if Activity.Description <> '' then
      begin
        PropNode := Doc.CreateElement('description');
        PropNode.TextContent := Activity.Description;
        ActivityNode.AppendChild(PropNode);
      end;

      if Activity.AssignedTo <> '' then
      begin
        PropNode := Doc.CreateElement('assigned-to');
        PropNode.TextContent := Activity.AssignedTo;
        ActivityNode.AppendChild(PropNode);
      end;

      // Propriétés personnalisées
      if Activity.Properties.Count > 0 then
      begin
        PropNode := Doc.CreateElement('properties');
        for i := 0 to Activity.Properties.Count - 1 do
        begin
          TDOMElement(PropNode).SetAttribute(
            Activity.Properties.Names[i],
            Activity.Properties.ValueFromIndex[i]
          );
        end;
        ActivityNode.AppendChild(PropNode);
      end;

      ActivitiesNode.AppendChild(ActivityNode);
    end;

    // Sauvegarder les transitions
    TransitionsNode := Doc.CreateElement('transitions');
    ProcessNode.AppendChild(TransitionsNode);

    for Transition in AProcess.Transitions do
    begin
      TransitionNode := Doc.CreateElement('transition');
      TDOMElement(TransitionNode).SetAttribute('id', Transition.Id);
      TDOMElement(TransitionNode).SetAttribute('name', Transition.Name);
      TDOMElement(TransitionNode).SetAttribute('source',
        Transition.SourceActivity.Id);
      TDOMElement(TransitionNode).SetAttribute('target',
        Transition.TargetActivity.Id);
      TDOMElement(TransitionNode).SetAttribute('type',
        IntToStr(Ord(Transition.TransitionType)));

      if Transition.Condition <> '' then
      begin
        PropNode := Doc.CreateElement('condition');
        PropNode.TextContent := Transition.Condition;
        TransitionNode.AppendChild(PropNode);
      end;

      TransitionsNode.AppendChild(TransitionNode);
    end;

    // Sauvegarder le fichier
    WriteXMLFile(Doc, AFileName);
  finally
    Doc.Free;
  end;
end;

class function TWorkflowXMLSerializer.LoadFromXML(
  const AFileName: string): TWorkflowProcess;
var
  Doc: TXMLDocument;
  RootNode, ProcessNode, ActivitiesNode, TransitionsNode: TDOMNode;
  ActivityNode, TransitionNode, PropNode: TDOMNode;
  Activity: TActivity;
  Transition: TTransition;
  i: Integer;
  SourceId, TargetId: string;
begin
  Result := TWorkflowProcess.Create;

  ReadXMLFile(Doc, AFileName);
  try
    RootNode := Doc.DocumentElement;

    // Charger les informations du processus
    ProcessNode := RootNode.FindNode('process');
    if Assigned(ProcessNode) then
    begin
      Result.Id := TDOMElement(ProcessNode).GetAttribute('id');
      Result.Name := TDOMElement(ProcessNode).GetAttribute('name');
      Result.Version := TDOMElement(ProcessNode).GetAttribute('version');

      PropNode := ProcessNode.FindNode('description');
      if Assigned(PropNode) then
        Result.Description := PropNode.TextContent;

      // Charger les activités
      ActivitiesNode := ProcessNode.FindNode('activities');
      if Assigned(ActivitiesNode) then
      begin
        ActivityNode := ActivitiesNode.FirstChild;
        while Assigned(ActivityNode) do
        begin
          if ActivityNode.NodeName = 'activity' then
          begin
            Activity := TActivity.Create;
            Activity.Id := TDOMElement(ActivityNode).GetAttribute('id');
            Activity.Name := TDOMElement(ActivityNode).GetAttribute('name');
            Activity.ActivityType := TActivityType(
              StrToInt(TDOMElement(ActivityNode).GetAttribute('type')));
            Activity.Position := Point(
              StrToInt(TDOMElement(ActivityNode).GetAttribute('x')),
              StrToInt(TDOMElement(ActivityNode).GetAttribute('y'))
            );

            PropNode := ActivityNode.FindNode('description');
            if Assigned(PropNode) then
              Activity.Description := PropNode.TextContent;

            PropNode := ActivityNode.FindNode('assigned-to');
            if Assigned(PropNode) then
              Activity.AssignedTo := PropNode.TextContent;

            Result.Activities.Add(Activity);
          end;
          ActivityNode := ActivityNode.NextSibling;
        end;
      end;

      // Charger les transitions
      TransitionsNode := ProcessNode.FindNode('transitions');
      if Assigned(TransitionsNode) then
      begin
        TransitionNode := TransitionsNode.FirstChild;
        while Assigned(TransitionNode) do
        begin
          if TransitionNode.NodeName = 'transition' then
          begin
            Transition := TTransition.Create;
            Transition.Id := TDOMElement(TransitionNode).GetAttribute('id');
            Transition.Name := TDOMElement(TransitionNode).GetAttribute('name');

            SourceId := TDOMElement(TransitionNode).GetAttribute('source');
            TargetId := TDOMElement(TransitionNode).GetAttribute('target');

            Transition.SourceActivity := Result.FindActivity(SourceId);
            Transition.TargetActivity := Result.FindActivity(TargetId);

            if Assigned(Transition.SourceActivity) and
               Assigned(Transition.TargetActivity) then
            begin
              Transition.TransitionType := TTransitionType(
                StrToInt(TDOMElement(TransitionNode).GetAttribute('type')));

              PropNode := TransitionNode.FindNode('condition');
              if Assigned(PropNode) then
                Transition.Condition := PropNode.TextContent;

              Transition.SourceActivity.AddOutgoingTransition(Transition);
              Transition.TargetActivity.AddIncomingTransition(Transition);

              Result.Transitions.Add(Transition);
            end
            else
              Transition.Free;
          end;
          TransitionNode := TransitionNode.NextSibling;
        end;
      end;
    end;
  finally
    Doc.Free;
  end;
end;

end.
```

---

## Implémentation complète dans WorkflowModel

Ajoutons les méthodes SaveToFile et LoadFromFile :

```pascal
// À ajouter dans l'implémentation de TWorkflowProcess

procedure TWorkflowProcess.SaveToFile(const AFileName: string);  
begin
  TWorkflowXMLSerializer.SaveToXML(Self, AFileName);
end;

procedure TWorkflowProcess.LoadFromFile(const AFileName: string);  
var
  LoadedProcess: TWorkflowProcess;
  Activity: TActivity;
  Transition: TTransition;
begin
  LoadedProcess := TWorkflowXMLSerializer.LoadFromXML(AFileName);
  try
    // Copier les propriétés
    Self.Id := LoadedProcess.Id;
    Self.Name := LoadedProcess.Name;
    Self.Description := LoadedProcess.Description;
    Self.Version := LoadedProcess.Version;

    // Transférer les activités
    Self.Activities.Clear;
    for Activity in LoadedProcess.Activities do
      Self.Activities.Add(Activity);
    LoadedProcess.Activities.OwnsObjects := False;

    // Transférer les transitions
    Self.Transitions.Clear;
    for Transition in LoadedProcess.Transitions do
      Self.Transitions.Add(Transition);
    LoadedProcess.Transitions.OwnsObjects := False;
  finally
    LoadedProcess.Free;
  end;
end;
```

---

## Notifications et événements

Système de notifications pour informer les utilisateurs :

```pascal
unit WorkflowNotifications;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WorkflowModel;

type
  TNotificationType = (ntEmail, ntSMS, ntInApp, ntWebhook);

  TNotificationMessage = class
  private
    FRecipient: string;
    FSubject: string;
    FBody: string;
    FNotificationType: TNotificationType;
    FTimestamp: TDateTime;
  public
    property Recipient: string read FRecipient write FRecipient;
    property Subject: string read FSubject write FSubject;
    property Body: string read FBody write FBody;
    property NotificationType: TNotificationType read FNotificationType
      write FNotificationType;
    property Timestamp: TDateTime read FTimestamp write FTimestamp;
  end;

  INotificationProvider = interface
    ['{B5E8F7D2-3C4A-4F9E-A123-456789ABCDEF}']
    procedure Send(AMessage: TNotificationMessage);
  end;

  TEmailNotificationProvider = class(TInterfacedObject, INotificationProvider)
  private
    FSMTPHost: string;
    FSMTPPort: Integer;
    FUsername: string;
    FPassword: string;
  public
    constructor Create(const AHost: string; APort: Integer;
      const AUsername, APassword: string);
    procedure Send(AMessage: TNotificationMessage);
  end;

  TNotificationManager = class
  private
    FProviders: array[TNotificationType] of INotificationProvider;
  public
    procedure RegisterProvider(AType: TNotificationType;
      AProvider: INotificationProvider);
    procedure NotifyActivityAssigned(AActivity: TActivity;
      AInstance: TWorkflowInstance);
    procedure NotifyActivityCompleted(AActivity: TActivity;
      AInstance: TWorkflowInstance);
    procedure NotifyProcessCompleted(AInstance: TWorkflowInstance);
  end;

implementation

uses
  {$IFDEF WINDOWS}
  // Utiliser une bibliothèque SMTP Windows
  {$ENDIF}
  {$IFDEF UNIX}
  // Utiliser une bibliothèque SMTP Linux
  {$ENDIF}
  // Synapse ou Indy pour compatibilité multi-plateforme
  ssl_openssl, smtpsend;

constructor TEmailNotificationProvider.Create(const AHost: string;
  APort: Integer; const AUsername, APassword: string);
begin
  inherited Create;
  FSMTPHost := AHost;
  FSMTPPort := APort;
  FUsername := AUsername;
  FPassword := APassword;
end;

procedure TEmailNotificationProvider.Send(AMessage: TNotificationMessage);  
var
  SMTP: TSMTPSend;
  EmailBody: TStringList;
begin
  SMTP := TSMTPSend.Create;
  EmailBody := TStringList.Create;
  try
    // Configuration du serveur SMTP
    SMTP.TargetHost := FSMTPHost;
    SMTP.TargetPort := IntToStr(FSMTPPort);
    SMTP.Username := FUsername;
    SMTP.Password := FPassword;

    // Construction de l'email
    EmailBody.Add('From: workflow@example.com');
    EmailBody.Add('To: ' + AMessage.Recipient);
    EmailBody.Add('Subject: ' + AMessage.Subject);
    EmailBody.Add('');
    EmailBody.Add(AMessage.Body);

    // Envoi
    if SMTP.Login then
    begin
      SMTP.MailFrom(FUsername, Length(FUsername));
      SMTP.MailTo(AMessage.Recipient, Length(AMessage.Recipient));
      SMTP.MailData(EmailBody);
      SMTP.Logout;
    end;
  finally
    EmailBody.Free;
    SMTP.Free;
  end;
end;

procedure TNotificationManager.RegisterProvider(AType: TNotificationType;
  AProvider: INotificationProvider);
begin
  FProviders[AType] := AProvider;
end;

procedure TNotificationManager.NotifyActivityAssigned(AActivity: TActivity;
  AInstance: TWorkflowInstance);
var
  Message: TNotificationMessage;
begin
  if AActivity.AssignedTo = '' then
    Exit;

  Message := TNotificationMessage.Create;
  try
    Message.Recipient := AActivity.AssignedTo;
    Message.Subject := 'Nouvelle tâche assignée: ' + AActivity.Name;
    Message.Body := Format(
      'Une nouvelle tâche vous a été assignée dans le workflow "%s".'#13#10 +
      'Tâche: %s'#13#10 +
      'Description: %s'#13#10 +
      'Instance: %s',
      [AInstance.ProcessId, AActivity.Name, AActivity.Description, AInstance.Id]
    );
    Message.NotificationType := ntEmail;
    Message.Timestamp := Now;

    if Assigned(FProviders[ntEmail]) then
      FProviders[ntEmail].Send(Message);
  finally
    Message.Free;
  end;
end;

procedure TNotificationManager.NotifyActivityCompleted(AActivity: TActivity;
  AInstance: TWorkflowInstance);
var
  Message: TNotificationMessage;
begin
  if AActivity.AssignedTo = '' then
    Exit;

  Message := TNotificationMessage.Create;
  try
    Message.Recipient := AActivity.AssignedTo;
    Message.Subject := 'Tâche complétée: ' + AActivity.Name;
    Message.Body := Format(
      'La tâche "%s" a été complétée dans le workflow.'#13#10 +
      'Instance: %s',
      [AActivity.Name, AInstance.Id]
    );
    Message.NotificationType := ntEmail;
    Message.Timestamp := Now;

    if Assigned(FProviders[ntEmail]) then
      FProviders[ntEmail].Send(Message);
  finally
    Message.Free;
  end;
end;

procedure TNotificationManager.NotifyProcessCompleted(
  AInstance: TWorkflowInstance);
var
  Message: TNotificationMessage;
begin
  Message := TNotificationMessage.Create;
  try
    Message.Recipient := 'admin@example.com';
    Message.Subject := 'Processus terminé';
    Message.Body := Format(
      'Le processus de workflow a été complété.'#13#10 +
      'Processus: %s'#13#10 +
      'Instance: %s'#13#10 +
      'Démarré: %s'#13#10 +
      'Terminé: %s',
      [AInstance.ProcessId, AInstance.Id,
       DateTimeToStr(AInstance.StartTime),
       DateTimeToStr(AInstance.EndTime)]
    );
    Message.NotificationType := ntEmail;
    Message.Timestamp := Now;

    if Assigned(FProviders[ntEmail]) then
      FProviders[ntEmail].Send(Message);
  finally
    Message.Free;
  end;
end;

end.
```

---

## API REST pour le workflow

Créons une API REST pour permettre l'interaction externe avec le moteur :

```pascal
unit WorkflowAPI;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpserver, fpjson, jsonparser,
  WorkflowModel, WorkflowEngine, WorkflowPersistence;

type
  TWorkflowAPIServer = class
  private
    FServer: TFPHTTPServer;
    FEngine: TWorkflowEngine;
    FDatabase: TWorkflowDatabase;
    FPort: Integer;

    procedure HandleRequest(Sender: TObject;
      var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse);
    procedure HandleGetProcesses(AResponse: TFPHTTPConnectionResponse);
    procedure HandleGetProcess(const AProcessId: string;
      AResponse: TFPHTTPConnectionResponse);
    procedure HandleCreateInstance(const AProcessId: string;
      ARequest: TFPHTTPConnectionRequest;
      AResponse: TFPHTTPConnectionResponse);
    procedure HandleGetInstance(const AInstanceId: string;
      AResponse: TFPHTTPConnectionResponse);
    procedure HandleCompleteActivity(const AInstanceId: string;
      ARequest: TFPHTTPConnectionRequest;
      AResponse: TFPHTTPConnectionResponse);
    function InstanceToJSON(AInstance: TWorkflowInstance): TJSONObject;
    function ProcessToJSON(AProcess: TWorkflowProcess): TJSONObject;
  public
    constructor Create(AEngine: TWorkflowEngine;
      ADatabase: TWorkflowDatabase; APort: Integer);
    destructor Destroy; override;

    procedure Start;
    procedure Stop;
  end;

implementation

constructor TWorkflowAPIServer.Create(AEngine: TWorkflowEngine;
  ADatabase: TWorkflowDatabase; APort: Integer);
begin
  inherited Create;
  FEngine := AEngine;
  FDatabase := ADatabase;
  FPort := APort;

  FServer := TFPHTTPServer.Create(nil);
  FServer.Port := FPort;
  FServer.OnRequest := @HandleRequest;
end;

destructor TWorkflowAPIServer.Destroy;  
begin
  if FServer.Active then
    FServer.Active := False;
  FServer.Free;
  inherited Destroy;
end;

procedure TWorkflowAPIServer.Start;  
begin
  FServer.Active := True;
  WriteLn('Serveur API démarré sur le port ', FPort);
end;

procedure TWorkflowAPIServer.Stop;  
begin
  FServer.Active := False;
  WriteLn('Serveur API arrêté');
end;

procedure TWorkflowAPIServer.HandleRequest(Sender: TObject;
  var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
var
  Path: string;
  Parts: TStringList;
begin
  Path := ARequest.PathInfo;
  AResponse.ContentType := 'application/json';

  try
    // Router les requêtes
    if Path = '/api/processes' then
    begin
      if ARequest.Method = 'GET' then
        HandleGetProcesses(AResponse);
    end
    else if Copy(Path, 1, 15) = '/api/processes/' then
    begin
      Parts := TStringList.Create;
      try
        Parts.Delimiter := '/';
        Parts.StrictDelimiter := True;
        Parts.DelimitedText := Path;
        if Parts.Count >= 4 then
        begin
          if ARequest.Method = 'GET' then
            HandleGetProcess(Parts[3], AResponse)
          else if (ARequest.Method = 'POST') and
                  (Copy(Path, Length(Path) - 9, 10) = '/instances') then
            HandleCreateInstance(Parts[3], ARequest, AResponse);
        end;
      finally
        Parts.Free;
      end;
    end
    else if Copy(Path, 1, 15) = '/api/instances/' then
    begin
      Parts := TStringList.Create;
      try
        Parts.Delimiter := '/';
        Parts.StrictDelimiter := True;
        Parts.DelimitedText := Path;
        if Parts.Count >= 4 then
        begin
          if ARequest.Method = 'GET' then
            HandleGetInstance(Parts[3], AResponse)
          else if (ARequest.Method = 'POST') and
                  (Copy(Path, Length(Path) - 8, 9) = '/complete') then
            HandleCompleteActivity(Parts[3], ARequest, AResponse);
        end;
      finally
        Parts.Free;
      end;
    end
    else
    begin
      AResponse.Code := 404;
      AResponse.Content := '{"error": "Endpoint non trouvé"}';
    end;
  except
    on E: Exception do
    begin
      AResponse.Code := 500;
      AResponse.Content := Format('{"error": "%s"}', [E.Message]);
    end;
  end;
end;

procedure TWorkflowAPIServer.HandleGetProcesses(
  AResponse: TFPHTTPConnectionResponse);
var
  JSONArray: TJSONArray;
  Process: TWorkflowProcess;
begin
  JSONArray := TJSONArray.Create;
  try
    for Process in FEngine.FProcesses do
      JSONArray.Add(ProcessToJSON(Process));

    AResponse.Content := JSONArray.AsJSON;
    AResponse.Code := 200;
  finally
    JSONArray.Free;
  end;
end;

procedure TWorkflowAPIServer.HandleGetProcess(const AProcessId: string;
  AResponse: TFPHTTPConnectionResponse);
var
  Process: TWorkflowProcess;
  JSONObj: TJSONObject;
begin
  Process := FEngine.GetProcess(AProcessId);

  if Assigned(Process) then
  begin
    JSONObj := ProcessToJSON(Process);
    try
      AResponse.Content := JSONObj.AsJSON;
      AResponse.Code := 200;
    finally
      JSONObj.Free;
    end;
  end
  else
  begin
    AResponse.Code := 404;
    AResponse.Content := '{"error": "Processus non trouvé"}';
  end;
end;

procedure TWorkflowAPIServer.HandleCreateInstance(const AProcessId: string;
  ARequest: TFPHTTPConnectionRequest;
  AResponse: TFPHTTPConnectionResponse);
var
  Instance: TWorkflowInstance;
  JSONObj: TJSONObject;
begin
  try
    Instance := FEngine.CreateInstance(AProcessId);
    FEngine.StartInstance(Instance);

    // Sauvegarder en base de données
    FDatabase.SaveInstance(Instance);

    JSONObj := InstanceToJSON(Instance);
    try
      AResponse.Content := JSONObj.AsJSON;
      AResponse.Code := 201;
    finally
      JSONObj.Free;
    end;
  except
    on E: Exception do
    begin
      AResponse.Code := 400;
      AResponse.Content := Format('{"error": "%s"}', [E.Message]);
    end;
  end;
end;

procedure TWorkflowAPIServer.HandleGetInstance(const AInstanceId: string;
  AResponse: TFPHTTPConnectionResponse);
var
  Instance: TWorkflowInstance;
  JSONObj: TJSONObject;
begin
  Instance := FEngine.GetInstance(AInstanceId);

  if Assigned(Instance) then
  begin
    JSONObj := InstanceToJSON(Instance);
    try
      AResponse.Content := JSONObj.AsJSON;
      AResponse.Code := 200;
    finally
      JSONObj.Free;
    end;
  end
  else
  begin
    AResponse.Code := 404;
    AResponse.Content := '{"error": "Instance non trouvée"}';
  end;
end;

procedure TWorkflowAPIServer.HandleCompleteActivity(const AInstanceId: string;
  ARequest: TFPHTTPConnectionRequest;
  AResponse: TFPHTTPConnectionResponse);
var
  Instance: TWorkflowInstance;
  JSONData: TJSONObject;
  Parser: TJSONParser;
  Data: TStringList;
  i: Integer;
  JSONObj: TJSONObject;
begin
  Instance := FEngine.GetInstance(AInstanceId);

  if not Assigned(Instance) then
  begin
    AResponse.Code := 404;
    AResponse.Content := '{"error": "Instance non trouvée"}';
    Exit;
  end;

  Data := TStringList.Create;
  try
    // Parser les données JSON si fournies
    if ARequest.Content <> '' then
    begin
      Parser := TJSONParser.Create(ARequest.Content, [joUTF8]);
      try
        JSONData := Parser.Parse as TJSONObject;
        try
          // Extraire les variables
          for i := 0 to JSONData.Count - 1 do
            Data.Add(JSONData.Names[i] + '=' + JSONData.Items[i].AsString);
        finally
          JSONData.Free;
        end;
      finally
        Parser.Free;
      end;
    end;

    // Compléter l'activité
    FEngine.CompleteActivity(Instance, Data);

    // Mettre à jour en base de données
    FDatabase.SaveInstance(Instance);

    JSONObj := InstanceToJSON(Instance);
    try
      AResponse.Content := JSONObj.AsJSON;
      AResponse.Code := 200;
    finally
      JSONObj.Free;
    end;
  finally
    Data.Free;
  end;
end;

function TWorkflowAPIServer.InstanceToJSON(
  AInstance: TWorkflowInstance): TJSONObject;
var
  StatusStr: string;
begin
  Result := TJSONObject.Create;
  Result.Add('id', AInstance.Id);
  Result.Add('processId', AInstance.ProcessId);

  if Assigned(AInstance.CurrentActivity) then
  begin
    Result.Add('currentActivity', AInstance.CurrentActivity.Name);
    Result.Add('currentActivityId', AInstance.CurrentActivity.Id);
  end
  else
  begin
    Result.Add('currentActivity', TJSONNull.Create);
    Result.Add('currentActivityId', TJSONNull.Create);
  end;

  case AInstance.Status of
    asNotStarted: StatusStr := 'not_started';
    asActive: StatusStr := 'active';
    asCompleted: StatusStr := 'completed';
    asCancelled: StatusStr := 'cancelled';
  end;
  Result.Add('status', StatusStr);

  Result.Add('startTime', DateTimeToStr(AInstance.StartTime));
  if AInstance.Status = asCompleted then
    Result.Add('endTime', DateTimeToStr(AInstance.EndTime))
  else
    Result.Add('endTime', TJSONNull.Create);
end;

function TWorkflowAPIServer.ProcessToJSON(
  AProcess: TWorkflowProcess): TJSONObject;
var
  ActivitiesArray: TJSONArray;
  TransitionsArray: TJSONArray;
  Activity: TActivity;
  Transition: TTransition;
  ActivityObj, TransitionObj: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.Add('id', AProcess.Id);
  Result.Add('name', AProcess.Name);
  Result.Add('description', AProcess.Description);
  Result.Add('version', AProcess.Version);

  // Activités
  ActivitiesArray := TJSONArray.Create;
  for Activity in AProcess.Activities do
  begin
    ActivityObj := TJSONObject.Create;
    ActivityObj.Add('id', Activity.Id);
    ActivityObj.Add('name', Activity.Name);
    ActivityObj.Add('type', Ord(Activity.ActivityType));
    ActivityObj.Add('description', Activity.Description);
    ActivityObj.Add('assignedTo', Activity.AssignedTo);
    ActivitiesArray.Add(ActivityObj);
  end;
  Result.Add('activities', ActivitiesArray);

  // Transitions
  TransitionsArray := TJSONArray.Create;
  for Transition in AProcess.Transitions do
  begin
    TransitionObj := TJSONObject.Create;
    TransitionObj.Add('id', Transition.Id);
    TransitionObj.Add('name', Transition.Name);
    TransitionObj.Add('source', Transition.SourceActivity.Id);
    TransitionObj.Add('target', Transition.TargetActivity.Id);
    TransitionObj.Add('condition', Transition.Condition);
    TransitionsArray.Add(TransitionObj);
  end;
  Result.Add('transitions', TransitionsArray);
end;

end.
```

---

## Programme principal - Application complète

Créons l'application principale qui intègre tous les composants :

```pascal
program WorkflowApp;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, Forms, Interfaces,
  WorkflowModel, WorkflowEngine, WorkflowPersistence, WorkflowAPI,
  WorkflowDesigner, WorkflowDashboard, WorkflowNotifications;

type
  TWorkflowApplication = class
  private
    FEngine: TWorkflowEngine;
    FDatabase: TWorkflowDatabase;
    FAPIServer: TWorkflowAPIServer;
    FNotificationManager: TNotificationManager;
    FDesignerForm: TWorkflowDesignerForm;
    FDashboardForm: TWorkflowDashboardForm;

    procedure InitializeComponents;
    procedure LoadSampleProcess;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Run;
  end;

constructor TWorkflowApplication.Create;  
begin
  inherited Create;
  InitializeComponents;
end;

destructor TWorkflowApplication.Destroy;  
begin
  if Assigned(FAPIServer) then
    FAPIServer.Stop;

  FAPIServer.Free;
  FNotificationManager.Free;
  FDatabase.Free;
  FEngine.Free;
  inherited Destroy;
end;

procedure TWorkflowApplication.InitializeComponents;  
var
  DatabasePath: string;
  EmailProvider: INotificationProvider;
begin
  // Initialiser le chemin de la base de données selon la plateforme
  {$IFDEF WINDOWS}
  DatabasePath := ExtractFilePath(ParamStr(0)) + 'workflow.db';
  {$ENDIF}

  {$IFDEF UNIX}
  DatabasePath := GetEnvironmentVariable('HOME') + '/.workflow/workflow.db';
  // Créer le répertoire s'il n'existe pas
  if not DirectoryExists(ExtractFilePath(DatabasePath)) then
    ForceDirectories(ExtractFilePath(DatabasePath));
  {$ENDIF}

  WriteLn('Base de données: ', DatabasePath);

  // Créer les composants
  FDatabase := TWorkflowDatabase.Create(DatabasePath);
  FEngine := TWorkflowEngine.Create;

  // Gestionnaire de notifications
  FNotificationManager := TNotificationManager.Create;

  // Configurer le fournisseur d'email (exemple)
  EmailProvider := TEmailNotificationProvider.Create(
    'smtp.example.com', 587, 'user@example.com', 'password');
  FNotificationManager.RegisterProvider(ntEmail, EmailProvider);

  // Serveur API (port 8080)
  FAPIServer := TWorkflowAPIServer.Create(FEngine, FDatabase, 8080);

  // Charger un processus exemple
  LoadSampleProcess;
end;

procedure TWorkflowApplication.LoadSampleProcess;  
var
  Process: TWorkflowProcess;
  ActivityStart, ActivityReview, ActivityApprove, ActivityReject, ActivityEnd: TActivity;
  Transition: TTransition;
begin
  // Créer un processus de validation de demande
  Process := TWorkflowProcess.Create;
  Process.Name := 'Processus de validation de demande';
  Process.Description := 'Workflow pour valider les demandes des utilisateurs';
  Process.Version := '1.0';

  // Créer les activités
  ActivityStart := Process.AddActivity('Début', atStart);
  ActivityStart.Position := Point(100, 100);

  ActivityReview := Process.AddActivity('Revue de la demande', atTask);
  ActivityReview.Position := Point(300, 100);
  ActivityReview.Description := 'Examiner la demande et vérifier les informations';
  ActivityReview.AssignedTo := 'reviewer@example.com';

  ActivityApprove := Process.AddActivity('Approuver', atDecision);
  ActivityApprove.Position := Point(500, 100);
  ActivityApprove.Description := 'Décision d''approbation';

  ActivityReject := Process.AddActivity('Rejeter', atTask);
  ActivityReject.Position := Point(500, 250);
  ActivityReject.Description := 'Rejeter la demande et notifier l''utilisateur';
  ActivityReject.AssignedTo := 'admin@example.com';

  ActivityEnd := Process.AddActivity('Fin', atEnd);
  ActivityEnd.Position := Point(700, 100);

  // Créer les transitions
  Process.AddTransition(ActivityStart, ActivityReview);

  Process.AddTransition(ActivityReview, ActivityApprove);

  Transition := Process.AddTransition(ActivityApprove, ActivityEnd);
  Transition.TransitionType := ttConditional;
  Transition.Condition := 'approved = true';
  Transition.Name := 'Approuvé';

  Transition := Process.AddTransition(ActivityApprove, ActivityReject);
  Transition.TransitionType := ttConditional;
  Transition.Condition := 'approved = false';
  Transition.Name := 'Rejeté';

  Process.AddTransition(ActivityReject, ActivityEnd);

  // Enregistrer le processus
  FEngine.RegisterProcess(Process);

  // Sauvegarder en base de données
  FDatabase.SaveProcess(Process);

  WriteLn('Processus exemple chargé: ', Process.Name);
end;

procedure TWorkflowApplication.Run;  
begin
  Application.Initialize;

  // Démarrer le serveur API
  FAPIServer.Start;

  // Créer les formulaires
  Application.CreateForm(TWorkflowDesignerForm, FDesignerForm);
  Application.CreateForm(TWorkflowDashboardForm, FDashboardForm);

  // Configurer le tableau de bord
  FDashboardForm.Engine := FEngine;

  // Afficher le designer au démarrage
  FDesignerForm.Show;

  WriteLn('Application de workflow démarrée');
  WriteLn('Designer: Ouvrir le formulaire de conception');
  WriteLn('Dashboard: Tableau de bord des instances');
  WriteLn('API: http://localhost:8080/api/');

  Application.Run;
end;

var
  App: TWorkflowApplication;

begin
  App := TWorkflowApplication.Create;
  try
    App.Run;
  finally
    App.Free;
  end;
end.
```

---

## Configuration multi-plateforme

### Fichier de configuration (workflow.conf)

```ini
[Database]
; Chemin vers la base de données
; Windows: chemin absolu ou relatif
; Linux: peut utiliser ~ pour le répertoire utilisateur
Path={$IFDEF WINDOWS}.\data\workflow.db{$ELSE}~/.workflow/workflow.db{$ENDIF}  
Type=SQLite

[API]
; Port du serveur API
Port=8080
; Activer HTTPS
EnableSSL=false
; Certificat SSL (si activé)
SSLCertificate=  
SSLKey=

[SMTP]
; Configuration du serveur email
Host=smtp.example.com  
Port=587  
Username=workflow@example.com  
Password=changeme  
UseTLS=true

[Logging]
; Niveau de log: debug, info, warning, error
Level=info
; Fichier de log
{$IFDEF WINDOWS}
File=.\logs\workflow.log
{$ELSE}
File=/var/log/workflow/workflow.log
{$ENDIF}

[Security]
; Activer l'authentification
RequireAuth=true
; Méthode: basic, jwt, oauth
AuthMethod=jwt
; Secret JWT
JWTSecret=your-secret-key-here
```

### Gestionnaire de configuration

```pascal
unit WorkflowConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles;

type
  TWorkflowConfig = class
  private
    FIniFile: TIniFile;
    FConfigPath: string;

    function GetDatabasePath: string;
    function GetAPIPort: Integer;
    function GetSMTPHost: string;
    function GetSMTPPort: Integer;
    function GetSMTPUsername: string;
    function GetSMTPPassword: string;
    function GetLogLevel: string;
    function GetLogFile: string;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Load;
    procedure Save;

    property DatabasePath: string read GetDatabasePath;
    property APIPort: Integer read GetAPIPort;
    property SMTPHost: string read GetSMTPHost;
    property SMTPPort: Integer read GetSMTPPort;
    property SMTPUsername: string read GetSMTPUsername;
    property SMTPPassword: string read GetSMTPPassword;
    property LogLevel: string read GetLogLevel;
    property LogFile: string read GetLogFile;
  end;

implementation

constructor TWorkflowConfig.Create;  
begin
  inherited Create;

  {$IFDEF WINDOWS}
  FConfigPath := ExtractFilePath(ParamStr(0)) + 'workflow.conf';
  {$ENDIF}

  {$IFDEF UNIX}
  // Chercher d'abord dans /etc, puis dans le home de l'utilisateur
  if FileExists('/etc/workflow/workflow.conf') then
    FConfigPath := '/etc/workflow/workflow.conf'
  else
    FConfigPath := GetEnvironmentVariable('HOME') + '/.workflow/workflow.conf';
  {$ENDIF}

  Load;
end;

destructor TWorkflowConfig.Destroy;  
begin
  FIniFile.Free;
  inherited Destroy;
end;

procedure TWorkflowConfig.Load;  
begin
  if Assigned(FIniFile) then
    FIniFile.Free;

  FIniFile := TIniFile.Create(FConfigPath);

  // Créer une configuration par défaut si le fichier n'existe pas
  if not FileExists(FConfigPath) then
  begin
    {$IFDEF WINDOWS}
    FIniFile.WriteString('Database', 'Path', '.\data\workflow.db');
    {$ENDIF}
    {$IFDEF UNIX}
    FIniFile.WriteString('Database', 'Path',
      GetEnvironmentVariable('HOME') + '/.workflow/workflow.db');
    {$ENDIF}

    FIniFile.WriteInteger('API', 'Port', 8080);
    FIniFile.WriteBool('API', 'EnableSSL', False);

    FIniFile.WriteString('SMTP', 'Host', 'smtp.example.com');
    FIniFile.WriteInteger('SMTP', 'Port', 587);
    FIniFile.WriteBool('SMTP', 'UseTLS', True);

    FIniFile.WriteString('Logging', 'Level', 'info');

    FIniFile.UpdateFile;
  end;
end;

procedure TWorkflowConfig.Save;  
begin
  if Assigned(FIniFile) then
    FIniFile.UpdateFile;
end;

function TWorkflowConfig.GetDatabasePath: string;  
begin
  Result := FIniFile.ReadString('Database', 'Path', 'workflow.db');

  // Résoudre le chemin relatif
  if not FileUtil.FileIsAbsolute(Result) then
    Result := ExpandFileName(Result);
end;

function TWorkflowConfig.GetAPIPort: Integer;  
begin
  Result := FIniFile.ReadInteger('API', 'Port', 8080);
end;

function TWorkflowConfig.GetSMTPHost: string;  
begin
  Result := FIniFile.ReadString('SMTP', 'Host', '');
end;

function TWorkflowConfig.GetSMTPPort: Integer;  
begin
  Result := FIniFile.ReadInteger('SMTP', 'Port', 587);
end;

function TWorkflowConfig.GetSMTPUsername: string;  
begin
  Result := FIniFile.ReadString('SMTP', 'Username', '');
end;

function TWorkflowConfig.GetSMTPPassword: string;  
begin
  Result := FIniFile.ReadString('SMTP', 'Password', '');
end;

function TWorkflowConfig.GetLogLevel: string;  
begin
  Result := FIniFile.ReadString('Logging', 'Level', 'info');
end;

function TWorkflowConfig.GetLogFile: string;  
begin
  Result := FIniFile.ReadString('Logging', 'File', 'workflow.log');
end;

end.
```

---

## Système de journalisation (Logging)

```pascal
unit WorkflowLogging;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TLogLevel = (llDebug, llInfo, llWarning, llError);

  TWorkflowLogger = class
  private
    FLogFile: string;
    FLogLevel: TLogLevel;
    FFileStream: TFileStream;

    function LogLevelToString(ALevel: TLogLevel): string;
    procedure WriteToFile(const AMessage: string);
  public
    constructor Create(const ALogFile: string; ALevel: TLogLevel);
    destructor Destroy; override;

    procedure Debug(const AMessage: string);
    procedure Info(const AMessage: string);
    procedure Warning(const AMessage: string);
    procedure Error(const AMessage: string);
    procedure Log(ALevel: TLogLevel; const AMessage: string);
  end;

implementation

constructor TWorkflowLogger.Create(const ALogFile: string; ALevel: TLogLevel);  
var
  LogDir: string;
begin
  inherited Create;
  FLogFile := ALogFile;
  FLogLevel := ALevel;

  // Créer le répertoire de logs s'il n'existe pas
  LogDir := ExtractFilePath(FLogFile);
  if (LogDir <> '') and (not DirectoryExists(LogDir)) then
    ForceDirectories(LogDir);

  // Ouvrir le fichier en mode ajout
  try
    if FileExists(FLogFile) then
      FFileStream := TFileStream.Create(FLogFile, fmOpenReadWrite or fmShareDenyWrite)
    else
      FFileStream := TFileStream.Create(FLogFile, fmCreate or fmShareDenyWrite);

    // Se positionner à la fin du fichier
    FFileStream.Seek(0, soEnd);
  except
    on E: Exception do
    begin
      WriteLn('Erreur création fichier log: ', E.Message);
      FFileStream := nil;
    end;
  end;
end;

destructor TWorkflowLogger.Destroy;  
begin
  if Assigned(FFileStream) then
    FFileStream.Free;
  inherited Destroy;
end;

function TWorkflowLogger.LogLevelToString(ALevel: TLogLevel): string;  
begin
  case ALevel of
    llDebug: Result := 'DEBUG';
    llInfo: Result := 'INFO';
    llWarning: Result := 'WARNING';
    llError: Result := 'ERROR';
  end;
end;

procedure TWorkflowLogger.WriteToFile(const AMessage: string);  
var
  Line: string;
begin
  Line := AMessage + LineEnding;

  if Assigned(FFileStream) then
  begin
    try
      FFileStream.Write(Line[1], Length(Line));
      FFileStream.Flush;
    except
      // Si l'écriture échoue, écrire sur la console
      WriteLn(AMessage);
    end;
  end
  else
    WriteLn(AMessage);
end;

procedure TWorkflowLogger.Log(ALevel: TLogLevel; const AMessage: string);  
var
  LogMessage: string;
  Timestamp: string;
begin
  // Vérifier le niveau de log
  if ALevel < FLogLevel then
    Exit;

  // Formater le message
  DateTimeToString(Timestamp, 'yyyy-mm-dd hh:nn:ss', Now);
  LogMessage := Format('[%s] [%s] %s',
    [Timestamp, LogLevelToString(ALevel), AMessage]);

  // Écrire dans le fichier et la console
  WriteToFile(LogMessage);
  WriteLn(LogMessage);
end;

procedure TWorkflowLogger.Debug(const AMessage: string);  
begin
  Log(llDebug, AMessage);
end;

procedure TWorkflowLogger.Info(const AMessage: string);  
begin
  Log(llInfo, AMessage);
end;

procedure TWorkflowLogger.Warning(const AMessage: string);  
begin
  Log(llWarning, AMessage);
end;

procedure TWorkflowLogger.Error(const AMessage: string);  
begin
  Log(llError, AMessage);
end;

end.
```

---

## Scripts de déploiement

### Script Windows (deploy.bat)

```batch
@echo off
echo ========================================  
echo Déploiement du moteur de workflow  
echo ========================================

REM Créer les répertoires nécessaires  
if not exist "data" mkdir data  
if not exist "logs" mkdir logs  
if not exist "backup" mkdir backup

REM Copier la configuration par défaut si elle n'existe pas  
if not exist "workflow.conf" (
    echo Création de la configuration par défaut...
    copy workflow.conf.template workflow.conf
)

REM Compiler l'application  
echo Compilation de l'application...  
lazbuild WorkflowApp.lpi

if errorlevel 1 (
    echo Erreur lors de la compilation!
    pause
    exit /b 1
)

REM Copier les DLLs nécessaires  
echo Copie des bibliothèques...  
copy "%ProgramFiles%\FreePascal\fpc\3.2.2\bin\i386-win32\*.dll" .

echo Déploiement terminé avec succès!  
echo.  
echo Pour lancer l'application: WorkflowApp.exe  
pause
```

### Script Linux/Ubuntu (deploy.sh)

```bash
#!/bin/bash

echo "========================================"  
echo "Déploiement du moteur de workflow"  
echo "========================================"

# Créer les répertoires nécessaires
mkdir -p ~/.workflow/data  
mkdir -p ~/.workflow/logs  
mkdir -p ~/.workflow/backup

# Copier la configuration par défaut si elle n'existe pas
if [ ! -f ~/.workflow/workflow.conf ]; then
    echo "Création de la configuration par défaut..."
    cp workflow.conf.template ~/.workflow/workflow.conf
fi

# Compiler l'application
echo "Compilation de l'application..."  
lazbuild WorkflowApp.lpi

if [ $? -ne 0 ]; then
    echo "Erreur lors de la compilation!"
    exit 1
fi

# Rendre l'exécutable
chmod +x WorkflowApp

# Créer un service systemd (optionnel)
echo "Voulez-vous installer le service systemd? (o/n)"  
read install_service

if [ "$install_service" = "o" ]; then
    sudo cp workflow.service /etc/systemd/system/
    sudo systemctl daemon-reload
    sudo systemctl enable workflow
    echo "Service installé. Utilisez 'sudo systemctl start workflow' pour démarrer"
fi

echo "Déploiement terminé avec succès!"  
echo ""  
echo "Pour lancer l'application: ./WorkflowApp"  
echo "Configuration: ~/.workflow/workflow.conf"  
echo "Logs: ~/.workflow/logs/workflow.log"
```

### Fichier de service systemd (workflow.service)

```ini
[Unit]
Description=Moteur de workflow BPM  
After=network.target

[Service]
Type=simple  
User=workflow  
WorkingDirectory=/opt/workflow  
ExecStart=/opt/workflow/WorkflowApp  
Restart=on-failure  
RestartSec=10

# Limites de sécurité
NoNewPrivileges=true  
PrivateTmp=true  
ProtectSystem=strict  
ProtectHome=true  
ReadWritePaths=/opt/workflow /var/log/workflow

[Install]
WantedBy=multi-user.target
```

---

## Tests unitaires

```pascal
unit WorkflowTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  WorkflowModel, WorkflowEngine, WorkflowRules;

type
  TWorkflowModelTest = class(TTestCase)
  published
    procedure TestCreateProcess;
    procedure TestAddActivity;
    procedure TestAddTransition;
    procedure TestFindActivity;
  end;

  TWorkflowEngineTest = class(TTestCase)
  private
    FEngine: TWorkflowEngine;
    FProcess: TWorkflowProcess;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCreateInstance;
    procedure TestStartInstance;
    procedure TestCompleteActivity;
    procedure TestWorkflowExecution;
  end;

  TWorkflowRulesTest = class(TTestCase)
  private
    FRuleEngine: TRuleEngine;
    FInstance: TWorkflowInstance;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestSimpleCondition;
    procedure TestComplexCondition;
    procedure TestNumericComparison;
    procedure TestStringComparison;
  end;

implementation

// Tests du modèle

procedure TWorkflowModelTest.TestCreateProcess;  
var
  Process: TWorkflowProcess;
begin
  Process := TWorkflowProcess.Create;
  try
    AssertNotNull('Le processus doit être créé', Process);
    AssertEquals('Le processus doit avoir un ID', 36, Length(Process.Id));
  finally
    Process.Free;
  end;
end;

procedure TWorkflowModelTest.TestAddActivity;  
var
  Process: TWorkflowProcess;
  Activity: TActivity;
begin
  Process := TWorkflowProcess.Create;
  try
    Activity := Process.AddActivity('Test', atTask);
    AssertNotNull('L''activité doit être créée', Activity);
    AssertEquals('Nom de l''activité', 'Test', Activity.Name);
    AssertEquals('Type d''activité', Ord(atTask), Ord(Activity.ActivityType));
    AssertEquals('Nombre d''activités', 1, Process.Activities.Count);
  finally
    Process.Free;
  end;
end;

procedure TWorkflowModelTest.TestAddTransition;  
var
  Process: TWorkflowProcess;
  Act1, Act2: TActivity;
  Trans: TTransition;
begin
  Process := TWorkflowProcess.Create;
  try
    Act1 := Process.AddActivity('Activité 1', atStart);
    Act2 := Process.AddActivity('Activité 2', atTask);
    Trans := Process.AddTransition(Act1, Act2);

    AssertNotNull('La transition doit être créée', Trans);
    AssertEquals('Source de la transition', Act1, Trans.SourceActivity);
    AssertEquals('Cible de la transition', Act2, Trans.TargetActivity);
    AssertEquals('Nombre de transitions', 1, Process.Transitions.Count);
  finally
    Process.Free;
  end;
end;

procedure TWorkflowModelTest.TestFindActivity;  
var
  Process: TWorkflowProcess;
  Activity, Found: TActivity;
begin
  Process := TWorkflowProcess.Create;
  try
    Activity := Process.AddActivity('Test', atTask);
    Found := Process.FindActivity(Activity.Id);

    AssertNotNull('L''activité doit être trouvée', Found);
    AssertEquals('ID de l''activité', Activity.Id, Found.Id);
  finally
    Process.Free;
  end;
end;

// Tests du moteur

procedure TWorkflowEngineTest.SetUp;  
var
  Act1, Act2, Act3: TActivity;
begin
  FEngine := TWorkflowEngine.Create;
  FProcess := TWorkflowProcess.Create;
  FProcess.Name := 'Test Process';

  // Créer un processus simple
  Act1 := FProcess.AddActivity('Début', atStart);
  Act2 := FProcess.AddActivity('Tâche 1', atTask);
  Act3 := FProcess.AddActivity('Fin', atEnd);

  FProcess.AddTransition(Act1, Act2);
  FProcess.AddTransition(Act2, Act3);

  FEngine.RegisterProcess(FProcess);
end;

procedure TWorkflowEngineTest.TearDown;  
begin
  FEngine.Free;
  FProcess.Free;
end;

procedure TWorkflowEngineTest.TestCreateInstance;  
var
  Instance: TWorkflowInstance;
begin
  Instance := FEngine.CreateInstance(FProcess.Id);

  AssertNotNull('L''instance doit être créée', Instance);
  AssertEquals('ID du processus', FProcess.Id, Instance.ProcessId);
  AssertEquals('Statut initial', Ord(asNotStarted), Ord(Instance.Status));
  AssertNotNull('Activité courante', Instance.CurrentActivity);
  AssertEquals('Type de l''activité initiale', Ord(atStart),
    Ord(Instance.CurrentActivity.ActivityType));
end;

procedure TWorkflowEngineTest.TestStartInstance;  
var
  Instance: TWorkflowInstance;
begin
  Instance := FEngine.CreateInstance(FProcess.Id);
  FEngine.StartInstance(Instance);

  AssertEquals('Statut après démarrage', Ord(asActive), Ord(Instance.Status));
  AssertTrue('Date de début définie', Instance.StartTime > 0);
end;

procedure TWorkflowEngineTest.TestCompleteActivity;  
var
  Instance: TWorkflowInstance;
  InitialActivity: TActivity;
begin
  Instance := FEngine.CreateInstance(FProcess.Id);
  FEngine.StartInstance(Instance);

  InitialActivity := Instance.CurrentActivity;
  FEngine.CompleteActivity(Instance);

  AssertTrue('L''activité a changé', Instance.CurrentActivity <> InitialActivity);
  AssertEquals('Type de la nouvelle activité', Ord(atTask),
    Ord(Instance.CurrentActivity.ActivityType));
end;

procedure TWorkflowEngineTest.TestWorkflowExecution;  
var
  Instance: TWorkflowInstance;
begin
  Instance := FEngine.CreateInstance(FProcess.Id);
  FEngine.StartInstance(Instance);

  // Compléter toutes les activités
  while Instance.Status = asActive do
    FEngine.CompleteActivity(Instance);

  AssertEquals('Statut final', Ord(asCompleted), Ord(Instance.Status));
  AssertEquals('Type de l''activité finale', Ord(atEnd),
    Ord(Instance.CurrentActivity.ActivityType));
  AssertTrue('Date de fin définie', Instance.EndTime > 0);
  AssertTrue('Durée d''exécution positive',
    Instance.EndTime > Instance.StartTime);
end;

// Tests du moteur de règles

procedure TWorkflowRulesTest.SetUp;  
begin
  FRuleEngine := TRuleEngine.Create;
  FInstance := TWorkflowInstance.Create;
end;

procedure TWorkflowRulesTest.TearDown;  
begin
  FRuleEngine.Free;
  FInstance.Free;
end;

procedure TWorkflowRulesTest.TestSimpleCondition;  
begin
  FInstance.SetVariable('status', 'approved');

  AssertTrue('Condition égale vraie',
    FRuleEngine.Evaluate('status = approved', FInstance));
  AssertFalse('Condition égale fausse',
    FRuleEngine.Evaluate('status = rejected', FInstance));
end;

procedure TWorkflowRulesTest.TestComplexCondition;  
begin
  FInstance.SetVariable('amount', '1500');
  FInstance.SetVariable('status', 'approved');

  AssertTrue('Condition AND vraie',
    FRuleEngine.Evaluate('amount > 1000 AND status = approved', FInstance));
  AssertFalse('Condition AND fausse',
    FRuleEngine.Evaluate('amount > 2000 AND status = approved', FInstance));
  AssertTrue('Condition OR vraie',
    FRuleEngine.Evaluate('amount > 2000 OR status = approved', FInstance));
end;

procedure TWorkflowRulesTest.TestNumericComparison;  
begin
  FInstance.SetVariable('amount', '1500');

  AssertTrue('Supérieur', FRuleEngine.Evaluate('amount > 1000', FInstance));
  AssertTrue('Supérieur ou égal',
    FRuleEngine.Evaluate('amount >= 1500', FInstance));
  AssertFalse('Inférieur', FRuleEngine.Evaluate('amount < 1000', FInstance));
  AssertTrue('Inférieur ou égal',
    FRuleEngine.Evaluate('amount <= 1500', FInstance));
end;

procedure TWorkflowRulesTest.TestStringComparison;  
begin
  FInstance.SetVariable('department', 'IT Department');

  AssertTrue('Contains',
    FRuleEngine.Evaluate('department CONTAINS IT', FInstance));
  AssertFalse('Not contains',
    FRuleEngine.Evaluate('department CONTAINS HR', FInstance));
end;

initialization
  RegisterTest(TWorkflowModelTest);
  RegisterTest(TWorkflowEngineTest);
  RegisterTest(TWorkflowRulesTest);

end.
```

---

## Programme de tests

```pascal
program WorkflowTestRunner;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, fpcunit, testregistry, consoletestrunner,
  WorkflowTests;

type
  TWorkflowTestRunner = class(TTestRunner)
  protected
    procedure WriteCustomHelp; override;
  end;

procedure TWorkflowTestRunner.WriteCustomHelp;  
begin
  inherited WriteCustomHelp;
  WriteLn('Tests du moteur de workflow BPM');
  WriteLn('');
  WriteLn('Exemples:');
  WriteLn('  WorkflowTestRunner --all          : Exécuter tous les tests');
  WriteLn('  WorkflowTestRunner --suite=Engine : Tests du moteur seulement');
  WriteLn('  WorkflowTestRunner --format=plain : Format de sortie simple');
end;

var
  App: TWorkflowTestRunner;

begin
  App := TWorkflowTestRunner.Create(nil);
  try
    App.Initialize;
    App.Title := 'Tests du moteur de workflow';
    App.Run;
  finally
    App.Free;
  end;
end.
```

---

## Optimisation et performance

### Indexation de la base de données

```sql
-- Index pour améliorer les performances des requêtes

-- Index sur les instances
CREATE INDEX IF NOT EXISTS idx_instances_process  
ON workflow_instances(process_id);

CREATE INDEX IF NOT EXISTS idx_instances_status  
ON workflow_instances(status);

CREATE INDEX IF NOT EXISTS idx_instances_current_activity  
ON workflow_instances(current_activity_id);

-- Index sur l'historique
CREATE INDEX IF NOT EXISTS idx_history_instance  
ON workflow_history(instance_id);

CREATE INDEX IF NOT EXISTS idx_history_timestamp  
ON workflow_history(timestamp);

-- Index sur les activités
CREATE INDEX IF NOT EXISTS idx_activities_process  
ON workflow_activities(process_id);

-- Index sur les transitions
CREATE INDEX IF NOT EXISTS idx_transitions_source  
ON workflow_transitions(source_activity_id);

CREATE INDEX IF NOT EXISTS idx_transitions_target  
ON workflow_transitions(target_activity_id);
```

### Cache en mémoire

```pascal
unit WorkflowCache;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, WorkflowModel;

type
  TWorkflowCache = class
  private
    FProcessCache: TDictionary<string, TWorkflowProcess>;
    FInstanceCache: TDictionary<string, TWorkflowInstance>;
    FMaxCacheSize: Integer;
    FCacheHits: Integer;
    FCacheMisses: Integer;

    procedure CleanupCache;
  public
    constructor Create(AMaxSize: Integer = 100);
    destructor Destroy; override;

    // Gestion du cache de processus
    procedure AddProcess(AProcess: TWorkflowProcess);
    function GetProcess(const AProcessId: string): TWorkflowProcess;
    procedure RemoveProcess(const AProcessId: string);

    // Gestion du cache d'instances
    procedure AddInstance(AInstance: TWorkflowInstance);
    function GetInstance(const AInstanceId: string): TWorkflowInstance;
    procedure RemoveInstance(const AInstanceId: string);

    // Statistiques
    function GetCacheHitRate: Double;
    procedure ClearCache;

    property CacheHits: Integer read FCacheHits;
    property CacheMisses: Integer read FCacheMisses;
  end;

implementation

constructor TWorkflowCache.Create(AMaxSize: Integer);  
begin
  inherited Create;
  FProcessCache := TDictionary<string, TWorkflowProcess>.Create;
  FInstanceCache := TDictionary<string, TWorkflowInstance>.Create;
  FMaxCacheSize := AMaxSize;
  FCacheHits := 0;
  FCacheMisses := 0;
end;

destructor TWorkflowCache.Destroy;  
begin
  FProcessCache.Free;
  FInstanceCache.Free;
  inherited Destroy;
end;

procedure TWorkflowCache.CleanupCache;  
begin
  // Stratégie simple: vider la moitié du cache si la limite est atteinte
  if FProcessCache.Count + FInstanceCache.Count > FMaxCacheSize then
  begin
    // Dans une implémentation réelle, utiliser une stratégie LRU
    // (Least Recently Used)
    if FInstanceCache.Count > FMaxCacheSize div 2 then
    begin
      FInstanceCache.Clear;
      WriteLn('Cache d''instances vidé (limite atteinte)');
    end;
  end;
end;

procedure TWorkflowCache.AddProcess(AProcess: TWorkflowProcess);  
begin
  CleanupCache;
  FProcessCache.AddOrSetValue(AProcess.Id, AProcess);
end;

function TWorkflowCache.GetProcess(const AProcessId: string): TWorkflowProcess;  
begin
  if FProcessCache.TryGetValue(AProcessId, Result) then
    Inc(FCacheHits)
  else
  begin
    Inc(FCacheMisses);
    Result := nil;
  end;
end;

procedure TWorkflowCache.RemoveProcess(const AProcessId: string);  
begin
  FProcessCache.Remove(AProcessId);
end;

procedure TWorkflowCache.AddInstance(AInstance: TWorkflowInstance);  
begin
  CleanupCache;
  FInstanceCache.AddOrSetValue(AInstance.Id, AInstance);
end;

function TWorkflowCache.GetInstance(const AInstanceId: string): TWorkflowInstance;  
begin
  if FInstanceCache.TryGetValue(AInstanceId, Result) then
    Inc(FCacheHits)
  else
  begin
    Inc(FCacheMisses);
    Result := nil;
  end;
end;

procedure TWorkflowCache.RemoveInstance(const AInstanceId: string);  
begin
  FInstanceCache.Remove(AInstanceId);
end;

function TWorkflowCache.GetCacheHitRate: Double;  
var
  Total: Integer;
begin
  Total := FCacheHits + FCacheMisses;
  if Total > 0 then
    Result := (FCacheHits / Total) * 100
  else
    Result := 0;
end;

procedure TWorkflowCache.ClearCache;  
begin
  FProcessCache.Clear;
  FInstanceCache.Clear;
  FCacheHits := 0;
  FCacheMisses := 0;
end;

end.
```

### Pool de threads pour l'exécution asynchrone

```pascal
unit WorkflowThreadPool;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WorkflowModel, WorkflowEngine;

type
  TWorkflowTask = class(TThread)
  private
    FEngine: TWorkflowEngine;
    FInstance: TWorkflowInstance;
    FOnComplete: TNotifyEvent;
  protected
    procedure Execute; override;
  public
    constructor Create(AEngine: TWorkflowEngine;
      AInstance: TWorkflowInstance; AOnComplete: TNotifyEvent);
  end;

  TWorkflowThreadPool = class
  private
    FEngine: TWorkflowEngine;
    FMaxThreads: Integer;
    FActiveThreads: Integer;
    FTaskQueue: TThreadList;

    procedure OnTaskComplete(Sender: TObject);
    procedure ProcessNextTask;
  public
    constructor Create(AEngine: TWorkflowEngine; AMaxThreads: Integer = 4);
    destructor Destroy; override;

    procedure ExecuteAsync(AInstance: TWorkflowInstance);
    function GetActiveThreadCount: Integer;
  end;

implementation

// TWorkflowTask

constructor TWorkflowTask.Create(AEngine: TWorkflowEngine;
  AInstance: TWorkflowInstance; AOnComplete: TNotifyEvent);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FEngine := AEngine;
  FInstance := AInstance;
  FOnComplete := AOnComplete;
end;

procedure TWorkflowTask.Execute;  
begin
  try
    // Exécuter le workflow jusqu'à la fin ou jusqu'à une activité manuelle
    while (FInstance.Status = asActive) and
          (FInstance.CurrentActivity.ActivityType <> atTask) do
    begin
      FEngine.CompleteActivity(FInstance);

      // Vérifier si le thread doit s'arrêter
      if Terminated then
        Break;
    end;

    // Notifier la fin
    if Assigned(FOnComplete) then
      Synchronize(@FOnComplete);
  except
    on E: Exception do
      WriteLn('Erreur dans le thread de workflow: ', E.Message);
  end;
end;

// TWorkflowThreadPool

constructor TWorkflowThreadPool.Create(AEngine: TWorkflowEngine;
  AMaxThreads: Integer);
begin
  inherited Create;
  FEngine := AEngine;
  FMaxThreads := AMaxThreads;
  FActiveThreads := 0;
  FTaskQueue := TThreadList.Create;
end;

destructor TWorkflowThreadPool.Destroy;  
begin
  // Attendre que tous les threads se terminent
  while FActiveThreads > 0 do
    Sleep(100);

  FTaskQueue.Free;
  inherited Destroy;
end;

procedure TWorkflowThreadPool.ExecuteAsync(AInstance: TWorkflowInstance);  
var
  List: TList;
begin
  if FActiveThreads < FMaxThreads then
  begin
    // Créer un nouveau thread
    Inc(FActiveThreads);
    TWorkflowTask.Create(FEngine, AInstance, @OnTaskComplete).Start;
  end
  else
  begin
    // Ajouter à la file d'attente
    List := FTaskQueue.LockList;
    try
      List.Add(AInstance);
    finally
      FTaskQueue.UnlockList;
    end;
  end;
end;

procedure TWorkflowThreadPool.OnTaskComplete(Sender: TObject);  
begin
  Dec(FActiveThreads);
  ProcessNextTask;
end;

procedure TWorkflowThreadPool.ProcessNextTask;  
var
  List: TList;
  Instance: TWorkflowInstance;
begin
  List := FTaskQueue.LockList;
  try
    if List.Count > 0 then
    begin
      Instance := TWorkflowInstance(List[0]);
      List.Delete(0);

      // Lancer le prochain thread
      ExecuteAsync(Instance);
    end;
  finally
    FTaskQueue.UnlockList;
  end;
end;

function TWorkflowThreadPool.GetActiveThreadCount: Integer;  
begin
  Result := FActiveThreads;
end;

end.
```

---

## Métriques et monitoring

```pascal
unit WorkflowMetrics;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, DateUtils, Generics.Collections;

type
  TWorkflowMetrics = class
  private
    FTotalInstances: Integer;
    FActiveInstances: Integer;
    FCompletedInstances: Integer;
    FFailedInstances: Integer;
    FAverageExecutionTime: Double;
    FExecutionTimes: TList<Double>;
    FStartTime: TDateTime;

    procedure CalculateAverageExecutionTime;
  public
    constructor Create;
    destructor Destroy; override;

    procedure RecordInstanceStart;
    procedure RecordInstanceComplete(ExecutionTime: Double);
    procedure RecordInstanceFailed;

    function GetMetricsReport: string;
    function GetMetricsJSON: string;
    procedure Reset;

    property TotalInstances: Integer read FTotalInstances;
    property ActiveInstances: Integer read FActiveInstances;
    property CompletedInstances: Integer read FCompletedInstances;
    property FailedInstances: Integer read FFailedInstances;
    property AverageExecutionTime: Double read FAverageExecutionTime;
  end;

implementation

uses
  fpjson;

constructor TWorkflowMetrics.Create;  
begin
  inherited Create;
  FExecutionTimes := TList<Double>.Create;
  FStartTime := Now;
  Reset;
end;

destructor TWorkflowMetrics.Destroy;  
begin
  FExecutionTimes.Free;
  inherited Destroy;
end;

procedure TWorkflowMetrics.Reset;  
begin
  FTotalInstances := 0;
  FActiveInstances := 0;
  FCompletedInstances := 0;
  FFailedInstances := 0;
  FAverageExecutionTime := 0;
  FExecutionTimes.Clear;
  FStartTime := Now;
end;

procedure TWorkflowMetrics.RecordInstanceStart;  
begin
  Inc(FTotalInstances);
  Inc(FActiveInstances);
end;

procedure TWorkflowMetrics.RecordInstanceComplete(ExecutionTime: Double);  
begin
  Dec(FActiveInstances);
  Inc(FCompletedInstances);
  FExecutionTimes.Add(ExecutionTime);
  CalculateAverageExecutionTime;
end;

procedure TWorkflowMetrics.RecordInstanceFailed;  
begin
  Dec(FActiveInstances);
  Inc(FFailedInstances);
end;

procedure TWorkflowMetrics.CalculateAverageExecutionTime;  
var
  Total: Double;
  Time: Double;
begin
  if FExecutionTimes.Count = 0 then
  begin
    FAverageExecutionTime := 0;
    Exit;
  end;

  Total := 0;
  for Time in FExecutionTimes do
    Total := Total + Time;

  FAverageExecutionTime := Total / FExecutionTimes.Count;
end;

function TWorkflowMetrics.GetMetricsReport: string;  
var
  Uptime: Double;
  ThroughputPerHour: Double;
begin
  Uptime := SecondsBetween(Now, FStartTime) / 3600; // en heures

  if Uptime > 0 then
    ThroughputPerHour := FCompletedInstances / Uptime
  else
    ThroughputPerHour := 0;

  Result := Format(
    'Métriques du moteur de workflow'#13#10 +
    '================================'#13#10 +
    'Temps de fonctionnement: %.2f heures'#13#10 +
    'Total d''instances: %d'#13#10 +
    'Instances actives: %d'#13#10 +
    'Instances terminées: %d'#13#10 +
    'Instances échouées: %d'#13#10 +
    'Temps d''exécution moyen: %.2f secondes'#13#10 +
    'Débit: %.2f instances/heure'#13#10 +
    'Taux de réussite: %.2f%%',
    [
      Uptime,
      FTotalInstances,
      FActiveInstances,
      FCompletedInstances,
      FFailedInstances,
      FAverageExecutionTime,
      ThroughputPerHour,
      (FCompletedInstances / FTotalInstances) * 100
    ]
  );
end;

function TWorkflowMetrics.GetMetricsJSON: string;  
var
  JSONObj: TJSONObject;
  Uptime: Double;
begin
  Uptime := SecondsBetween(Now, FStartTime) / 3600;

  JSONObj := TJSONObject.Create;
  try
    JSONObj.Add('uptime_hours', Uptime);
    JSONObj.Add('total_instances', FTotalInstances);
    JSONObj.Add('active_instances', FActiveInstances);
    JSONObj.Add('completed_instances', FCompletedInstances);
    JSONObj.Add('failed_instances', FFailedInstances);
    JSONObj.Add('average_execution_time', FAverageExecutionTime);

    if FTotalInstances > 0 then
      JSONObj.Add('success_rate',
        (FCompletedInstances / FTotalInstances) * 100)
    else
      JSONObj.Add('success_rate', 0);

    Result := JSONObj.AsJSON;
  finally
    JSONObj.Free;
  end;
end;

end.
```

---

## Documentation utilisateur

### Guide de démarrage rapide

````markdown
# Guide de démarrage rapide - Moteur de workflow BPM

## Installation

### Windows

1. Télécharger l'archive `workflow-windows.zip`
2. Extraire dans un répertoire de votre choix
3. Exécuter `deploy.bat` pour initialiser l'environnement
4. Lancer `WorkflowApp.exe`

### Linux/Ubuntu

1. Télécharger l'archive `workflow-linux.tar.gz`
2. Extraire: `tar -xzf workflow-linux.tar.gz`
3. Rendre le script exécutable: `chmod +x deploy.sh`
4. Exécuter: `./deploy.sh`
5. Lancer: `./WorkflowApp`

## Premiers pas

### 1. Créer votre premier workflow

1. Ouvrir l'éditeur de workflow
2. Cliquer sur "Nouveau processus"
3. Utiliser la barre d'outils pour ajouter des activités:
   - **Début** (cercle vert): Point de départ du workflow
   - **Tâche** (rectangle bleu): Activité manuelle ou automatique
   - **Décision** (losange jaune): Point de branchement conditionnel
   - **Fin** (cercle rouge): Point de terminaison

4. Connecter les activités:
   - Clic droit sur l'activité source
   - Maintenir et glisser vers l'activité cible
   - Relâcher pour créer la transition

5. Configurer les propriétés:
   - Double-cliquer sur une activité pour éditer ses propriétés
   - Définir le nom, la description, l'utilisateur assigné
   - Pour les décisions, définir les conditions

6. Sauvegarder le processus: Menu Fichier > Enregistrer

### 2. Exécuter un workflow

1. Ouvrir le tableau de bord
2. Sélectionner un processus dans la liste déroulante
3. Cliquer sur "Démarrer nouvelle instance"
4. L'instance apparaît dans la grille avec le statut "Actif"
5. Sélectionner l'instance et cliquer sur "Compléter activité"
6. Répéter jusqu'à ce que le workflow soit terminé

### 3. Utiliser l'API REST

L'API REST est accessible sur `http://localhost:8080/api/`

#### Créer une instance:
```bash
curl -X POST http://localhost:8080/api/processes/{process-id}/instances
```

#### Obtenir le statut d'une instance:
```bash
curl http://localhost:8080/api/instances/{instance-id}
```

#### Compléter une activité:
```bash
curl -X POST http://localhost:8080/api/instances/{instance-id}/complete \
  -H "Content-Type: application/json" \
  -d '{"variable1": "valeur1", "variable2": "valeur2"}'
```

## Configuration

Le fichier `workflow.conf` contient tous les paramètres:

- **[Database]**: Emplacement et type de base de données
- **[API]**: Port et configuration SSL
- **[SMTP]**: Serveur email pour les notifications
- **[Logging]**: Niveau de log et fichier de sortie

## Résolution de problèmes

### Le serveur API ne démarre pas

- Vérifier que le port 8080 n'est pas déjà utilisé
- Modifier le port dans `workflow.conf` si nécessaire
- Vérifier les logs dans `logs/workflow.log`

### Les emails ne sont pas envoyés

- Vérifier la configuration SMTP dans `workflow.conf`
- Tester la connectivité au serveur SMTP
- Vérifier les logs pour les erreurs d'authentification

### Erreur de base de données

- Vérifier que le répertoire de la base de données existe
- S'assurer que l'application a les droits d'écriture
- Supprimer `workflow.db` pour recréer une base vierge

## Support

- Documentation complète: `docs/manual.pdf`
- Forum communautaire: https://forum.workflow-bpm.example.com
- Issues GitHub: https://github.com/username/workflow-bpm/issues
````

---

## Exemples d'utilisation

### Exemple 1: Processus d'approbation de congés

```pascal
procedure CreateVacationApprovalWorkflow;  
var
  Process: TWorkflowProcess;
  ActStart, ActRequest, ActManagerReview, ActHRReview: TActivity;
  ActApproved, ActRejected, ActEnd: TActivity;
  Trans: TTransition;
begin
  Process := TWorkflowProcess.Create;
  Process.Name := 'Approbation de congés';
  Process.Description := 'Workflow pour gérer les demandes de congés';

  // Créer les activités
  ActStart := Process.AddActivity('Début', atStart);

  ActRequest := Process.AddActivity('Soumettre la demande', atTask);
  ActRequest.Description := 'L''employé soumet sa demande de congés';
  ActRequest.Properties.Values['form'] := 'vacation_request';

  ActManagerReview := Process.AddActivity('Revue du manager', atTask);
  ActManagerReview.Description := 'Le manager examine la demande';
  ActManagerReview.AssignedTo := 'manager@company.com';

  ActHRReview := Process.AddActivity('Validation RH', atTask);
  ActHRReview.Description := 'Les RH valident la disponibilité';
  ActHRReview.AssignedTo := 'hr@company.com';

  ActApproved := Process.AddActivity('Approuvé', atTask);
  ActApproved.Description := 'Notifier l''employé de l''approbation';

  ActRejected := Process.AddActivity('Rejeté', atTask);
  ActRejected.Description := 'Notifier l''employé du refus';

  ActEnd := Process.AddActivity('Fin', atEnd);

  // Créer les transitions
  Process.AddTransition(ActStart, ActRequest);
  Process.AddTransition(ActRequest, ActManagerReview);

  Trans := Process.AddTransition(ActManagerReview, ActHRReview);
  Trans.Condition := 'manager_approved = true';

  Trans := Process.AddTransition(ActManagerReview, ActRejected);
  Trans.Condition := 'manager_approved = false';

  Trans := Process.AddTransition(ActHRReview, ActApproved);
  Trans.Condition := 'hr_approved = true';

  Trans := Process.AddTransition(ActHRReview, ActRejected);
  Trans.Condition := 'hr_approved = false';

  Process.AddTransition(ActApproved, ActEnd);
  Process.AddTransition(ActRejected, ActEnd);

  // Sauvegarder
  Process.SaveToFile('vacation_approval.wflow');
  Process.Free;
end;
```

### Exemple 2: Processus de traitement de commande

```pascal
procedure CreateOrderProcessingWorkflow;  
var
  Process: TWorkflowProcess;
  ActStart, ActValidate, ActCheckStock, ActProcessPayment: TActivity;
  ActShip, ActRefund, ActEnd: TActivity;
  Trans: TTransition;
begin
  Process := TWorkflowProcess.Create;
  Process.Name := 'Traitement de commande';

  // Activités
  ActStart := Process.AddActivity('Début', atStart);

  ActValidate := Process.AddActivity('Valider la commande', atTask);
  ActValidate.Description := 'Vérifier les informations de la commande';

  ActCheckStock := Process.AddActivity('Vérifier le stock', atTask);
  ActCheckStock.Description := 'Contrôler la disponibilité des produits';

  ActProcessPayment := Process.AddActivity('Traiter le paiement', atTask);
  ActProcessPayment.Description := 'Effectuer la transaction financière';

  ActShip := Process.AddActivity('Expédier', atTask);
  ActShip.Description := 'Préparer et expédier la commande';

# 25.5 Moteur de workflow/BPM - Partie 5 (Finale)

## Exemples d'utilisation (suite)

### Exemple 2: Processus de traitement de commande (suite)

```pascal
  ActRefund := Process.AddActivity('Rembourser', atTask);
  ActRefund.Description := 'Traiter le remboursement';

  ActEnd := Process.AddActivity('Fin', atEnd);

  // Transitions
  Process.AddTransition(ActStart, ActValidate);

  Trans := Process.AddTransition(ActValidate, ActCheckStock);
  Trans.Condition := 'order_valid = true';

  Trans := Process.AddTransition(ActValidate, ActEnd);
  Trans.Condition := 'order_valid = false';

  Trans := Process.AddTransition(ActCheckStock, ActProcessPayment);
  Trans.Condition := 'stock_available = true';

  Trans := Process.AddTransition(ActCheckStock, ActRefund);
  Trans.Condition := 'stock_available = false';

  Trans := Process.AddTransition(ActProcessPayment, ActShip);
  Trans.Condition := 'payment_successful = true';

  Trans := Process.AddTransition(ActProcessPayment, ActRefund);
  Trans.Condition := 'payment_successful = false';

  Process.AddTransition(ActShip, ActEnd);
  Process.AddTransition(ActRefund, ActEnd);

  // Sauvegarder
  Process.SaveToFile('order_processing.wflow');
  Process.Free;
end;
```

### Exemple 3: Utilisation de l'API depuis un client

```pascal
program WorkflowAPIClient;

{$mode objfpc}{$H+}

uses
  SysUtils, fphttpclient, fpjson, jsonparser;

procedure CreateAndExecuteWorkflowInstance;  
var
  HTTPClient: TFPHTTPClient;
  Response: string;
  JSONData: TJSONObject;
  Parser: TJSONParser;
  InstanceId: string;
  ProcessId: string;
begin
  HTTPClient := TFPHTTPClient.Create(nil);
  try
    ProcessId := 'vacation-approval-001';

    // 1. Créer une nouvelle instance
    WriteLn('Création d''une nouvelle instance...');
    Response := HTTPClient.Post(
      Format('http://localhost:8080/api/processes/%s/instances', [ProcessId]),
      ''
    );

    Parser := TJSONParser.Create(Response, [joUTF8]);
    try
      JSONData := Parser.Parse as TJSONObject;
      try
        InstanceId := JSONData.Get('id', '');
        WriteLn('Instance créée: ', InstanceId);
      finally
        JSONData.Free;
      end;
    finally
      Parser.Free;
    end;

    // 2. Soumettre la demande avec des données
    WriteLn('Soumission de la demande...');
    Response := HTTPClient.Post(
      Format('http://localhost:8080/api/instances/%s/complete', [InstanceId]),
      '{"employee_name": "Jean Dupont", "start_date": "2025-07-01", ' +
      '"end_date": "2025-07-15", "days": "10"}'
    );
    WriteLn('Activité complétée');

    // 3. Approbation du manager
    WriteLn('Approbation du manager...');
    Response := HTTPClient.Post(
      Format('http://localhost:8080/api/instances/%s/complete', [InstanceId]),
      '{"manager_approved": "true", "manager_comment": "Approuvé"}'
    );
    WriteLn('Manager a approuvé');

    // 4. Validation RH
    WriteLn('Validation RH...');
    Response := HTTPClient.Post(
      Format('http://localhost:8080/api/instances/%s/complete', [InstanceId]),
      '{"hr_approved": "true", "hr_comment": "Disponibilité confirmée"}'
    );
    WriteLn('RH a validé');

    // 5. Vérifier le statut final
    WriteLn('Vérification du statut...');
    Response := HTTPClient.Get(
      Format('http://localhost:8080/api/instances/%s', [InstanceId])
    );

    Parser := TJSONParser.Create(Response, [joUTF8]);
    try
      JSONData := Parser.Parse as TJSONObject;
      try
        WriteLn('Statut: ', JSONData.Get('status', ''));
        WriteLn('Activité courante: ', JSONData.Get('currentActivity', ''));
      finally
        JSONData.Free;
      end;
    finally
      Parser.Free;
    end;

  finally
    HTTPClient.Free;
  end;
end;

begin
  try
    CreateAndExecuteWorkflowInstance;
  except
    on E: Exception do
      WriteLn('Erreur: ', E.Message);
  end;
end.
```

---

## Extensions avancées

### Intégration avec des services externes

```pascal
unit WorkflowIntegrations;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient, fpjson, WorkflowModel;

type
  IExternalService = interface
    ['{F5A3B2C1-9D8E-4F7A-B6C5-D4E3F2A1B0C9}']
    function Execute(AInstance: TWorkflowInstance;
      AActivity: TActivity): Boolean;
    function GetServiceName: string;
  end;

  TSlackNotificationService = class(TInterfacedObject, IExternalService)
  private
    FWebhookURL: string;
  public
    constructor Create(const AWebhookURL: string);
    function Execute(AInstance: TWorkflowInstance;
      AActivity: TActivity): Boolean;
    function GetServiceName: string;
  end;

  TEmailService = class(TInterfacedObject, IExternalService)
  private
    FSMTPHost: string;
    FSMTPPort: Integer;
    FUsername: string;
    FPassword: string;
  public
    constructor Create(const AHost: string; APort: Integer;
      const AUser, APass: string);
    function Execute(AInstance: TWorkflowInstance;
      AActivity: TActivity): Boolean;
    function GetServiceName: string;
  end;

  TWebhookService = class(TInterfacedObject, IExternalService)
  private
    FTargetURL: string;
  public
    constructor Create(const ATargetURL: string);
    function Execute(AInstance: TWorkflowInstance;
      AActivity: TActivity): Boolean;
    function GetServiceName: string;
  end;

  TServiceRegistry = class
  private
    FServices: TStringList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure RegisterService(const AName: string; AService: IExternalService);
    function GetService(const AName: string): IExternalService;
    function ExecuteService(const AName: string;
      AInstance: TWorkflowInstance; AActivity: TActivity): Boolean;
  end;

implementation

// TSlackNotificationService

constructor TSlackNotificationService.Create(const AWebhookURL: string);  
begin
  inherited Create;
  FWebhookURL := AWebhookURL;
end;

function TSlackNotificationService.Execute(AInstance: TWorkflowInstance;
  AActivity: TActivity): Boolean;
var
  HTTPClient: TFPHTTPClient;
  JSONPayload: TJSONObject;
  Response: string;
begin
  Result := False;
  HTTPClient := TFPHTTPClient.Create(nil);
  try
    // Construire le message JSON pour Slack
    JSONPayload := TJSONObject.Create;
    try
      JSONPayload.Add('text', Format(
        'Workflow notification: Activity "%s" in process "%s"',
        [AActivity.Name, AInstance.ProcessId]
      ));

      HTTPClient.AddHeader('Content-Type', 'application/json');
      Response := HTTPClient.Post(FWebhookURL, JSONPayload.AsJSON);
      Result := True;
    finally
      JSONPayload.Free;
    end;
  finally
    HTTPClient.Free;
  end;
end;

function TSlackNotificationService.GetServiceName: string;  
begin
  Result := 'slack_notification';
end;

// TEmailService

constructor TEmailService.Create(const AHost: string; APort: Integer;
  const AUser, APass: string);
begin
  inherited Create;
  FSMTPHost := AHost;
  FSMTPPort := APort;
  FUsername := AUser;
  FPassword := APass;
end;

function TEmailService.Execute(AInstance: TWorkflowInstance;
  AActivity: TActivity): Boolean;
begin
  // Implémentation similaire à TEmailNotificationProvider
  Result := True;
  WriteLn('Email envoyé pour l''activité: ', AActivity.Name);
end;

function TEmailService.GetServiceName: string;  
begin
  Result := 'email_service';
end;

// TWebhookService

constructor TWebhookService.Create(const ATargetURL: string);  
begin
  inherited Create;
  FTargetURL := ATargetURL;
end;

function TWebhookService.Execute(AInstance: TWorkflowInstance;
  AActivity: TActivity): Boolean;
var
  HTTPClient: TFPHTTPClient;
  JSONPayload: TJSONObject;
  Response: string;
begin
  Result := False;
  HTTPClient := TFPHTTPClient.Create(nil);
  try
    JSONPayload := TJSONObject.Create;
    try
      JSONPayload.Add('instance_id', AInstance.Id);
      JSONPayload.Add('activity_id', AActivity.Id);
      JSONPayload.Add('activity_name', AActivity.Name);
      JSONPayload.Add('timestamp', DateTimeToStr(Now));

      HTTPClient.AddHeader('Content-Type', 'application/json');
      Response := HTTPClient.Post(FTargetURL, JSONPayload.AsJSON);
      Result := True;
    finally
      JSONPayload.Free;
    end;
  finally
    HTTPClient.Free;
  end;
end;

function TWebhookService.GetServiceName: string;  
begin
  Result := 'webhook';
end;

// TServiceRegistry

constructor TServiceRegistry.Create;  
begin
  inherited Create;
  FServices := TStringList.Create;
  FServices.OwnsObjects := True;
end;

destructor TServiceRegistry.Destroy;  
begin
  FServices.Free;
  inherited Destroy;
end;

procedure TServiceRegistry.RegisterService(const AName: string;
  AService: IExternalService);
begin
  FServices.AddObject(AName, TObject(AService));
end;

function TServiceRegistry.GetService(const AName: string): IExternalService;  
var
  Index: Integer;
begin
  Result := nil;
  Index := FServices.IndexOf(AName);
  if Index >= 0 then
    Result := IExternalService(FServices.Objects[Index]);
end;

function TServiceRegistry.ExecuteService(const AName: string;
  AInstance: TWorkflowInstance; AActivity: TActivity): Boolean;
var
  Service: IExternalService;
begin
  Service := GetService(AName);
  if Assigned(Service) then
    Result := Service.Execute(AInstance, AActivity)
  else
  begin
    WriteLn('Service non trouvé: ', AName);
    Result := False;
  end;
end;

end.
```

---

## Sécurité et authentification

```pascal
unit WorkflowSecurity;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DCPsha256, DCPrijndael, base64;

type
  TUserRole = (urViewer, urUser, urManager, urAdmin);

  TWorkflowUser = class
  private
    FUsername: string;
    FPasswordHash: string;
    FRole: TUserRole;
    FEmail: string;
    FEnabled: Boolean;
  public
    property Username: string read FUsername write FUsername;
    property PasswordHash: string read FPasswordHash write FPasswordHash;
    property Role: TUserRole read FRole write FRole;
    property Email: string read FEmail write FEmail;
    property Enabled: Boolean read FEnabled write FEnabled;
  end;

  TAuthenticationManager = class
  private
    FUsers: TStringList;
    FSecretKey: string;

    function HashPassword(const APassword: string): string;
    function GenerateToken(AUser: TWorkflowUser): string;
  public
    constructor Create(const ASecretKey: string);
    destructor Destroy; override;

    function RegisterUser(const AUsername, APassword, AEmail: string;
      ARole: TUserRole): Boolean;
    function AuthenticateUser(const AUsername, APassword: string): string;
    function ValidateToken(const AToken: string): TWorkflowUser;
    function ChangePassword(const AUsername, AOldPassword, ANewPassword: string): Boolean;
    function HasPermission(AUser: TWorkflowUser; const AAction: string): Boolean;
  end;

implementation

uses
  DateUtils;

constructor TAuthenticationManager.Create(const ASecretKey: string);  
begin
  inherited Create;
  FSecretKey := ASecretKey;
  FUsers := TStringList.Create;
  FUsers.OwnsObjects := True;
end;

destructor TAuthenticationManager.Destroy;  
begin
  FUsers.Free;
  inherited Destroy;
end;

function TAuthenticationManager.HashPassword(const APassword: string): string;  
var
  Hash: TDCP_sha256;
  Digest: array[0..31] of Byte;
begin
  Hash := TDCP_sha256.Create(nil);
  try
    Hash.Init;
    Hash.UpdateStr(APassword + FSecretKey); // Salt avec la clé secrète
    Hash.Final(Digest);
    Result := EncodeStringBase64(
      Copy(PChar(@Digest), 0, SizeOf(Digest))
    );
  finally
    Hash.Free;
  end;
end;

function TAuthenticationManager.RegisterUser(const AUsername, APassword,
  AEmail: string; ARole: TUserRole): Boolean;
var
  User: TWorkflowUser;
begin
  Result := False;

  // Vérifier si l'utilisateur existe déjà
  if FUsers.IndexOf(AUsername) >= 0 then
    Exit;

  User := TWorkflowUser.Create;
  User.Username := AUsername;
  User.PasswordHash := HashPassword(APassword);
  User.Email := AEmail;
  User.Role := ARole;
  User.Enabled := True;

  FUsers.AddObject(AUsername, User);
  Result := True;
end;

function TAuthenticationManager.AuthenticateUser(const AUsername,
  APassword: string): string;
var
  Index: Integer;
  User: TWorkflowUser;
  PasswordHash: string;
begin
  Result := '';
  Index := FUsers.IndexOf(AUsername);

  if Index >= 0 then
  begin
    User := TWorkflowUser(FUsers.Objects[Index]);
    PasswordHash := HashPassword(APassword);

    if (User.PasswordHash = PasswordHash) and User.Enabled then
      Result := GenerateToken(User);
  end;
end;

function TAuthenticationManager.GenerateToken(AUser: TWorkflowUser): string;  
var
  JSONObj: TJSONObject;
  Cipher: TDCP_rijndael;
  Plaintext, Ciphertext: string;
begin
  // Créer un token JWT simplifié
  JSONObj := TJSONObject.Create;
  try
    JSONObj.Add('username', AUser.Username);
    JSONObj.Add('role', Ord(AUser.Role));
    JSONObj.Add('issued_at', DateTimeToStr(Now));
    JSONObj.Add('expires_at', DateTimeToStr(IncHour(Now, 24)));

    Plaintext := JSONObj.AsJSON;
  finally
    JSONObj.Free;
  end;

  // Chiffrer le token
  Cipher := TDCP_rijndael.Create(nil);
  try
    Cipher.InitStr(FSecretKey, TDCP_sha256);
    Ciphertext := '';
    Cipher.EncryptString(Plaintext, Ciphertext);
    Result := EncodeStringBase64(Ciphertext);
  finally
    Cipher.Free;
  end;
end;

function TAuthenticationManager.ValidateToken(const AToken: string): TWorkflowUser;  
var
  Cipher: TDCP_rijndael;
  Ciphertext, Plaintext: string;
  JSONObj: TJSONObject;
  Parser: TJSONParser;
  Username: string;
  ExpiresAt: TDateTime;
  Index: Integer;
begin
  Result := nil;

  try
    // Déchiffrer le token
    Ciphertext := DecodeStringBase64(AToken);
    Cipher := TDCP_rijndael.Create(nil);
    try
      Cipher.InitStr(FSecretKey, TDCP_sha256);
      Plaintext := '';
      Cipher.DecryptString(Ciphertext, Plaintext);
    finally
      Cipher.Free;
    end;

    // Parser le JSON
    Parser := TJSONParser.Create(Plaintext, [joUTF8]);
    try
      JSONObj := Parser.Parse as TJSONObject;
      try
        Username := JSONObj.Get('username', '');
        ExpiresAt := StrToDateTime(JSONObj.Get('expires_at', ''));

        // Vérifier l'expiration
        if Now > ExpiresAt then
          Exit;

        // Récupérer l'utilisateur
        Index := FUsers.IndexOf(Username);
        if Index >= 0 then
          Result := TWorkflowUser(FUsers.Objects[Index]);
      finally
        JSONObj.Free;
      end;
    finally
      Parser.Free;
    end;
  except
    Result := nil;
  end;
end;

function TAuthenticationManager.ChangePassword(const AUsername, AOldPassword,
  ANewPassword: string): Boolean;
var
  Index: Integer;
  User: TWorkflowUser;
begin
  Result := False;
  Index := FUsers.IndexOf(AUsername);

  if Index >= 0 then
  begin
    User := TWorkflowUser(FUsers.Objects[Index]);
    if User.PasswordHash = HashPassword(AOldPassword) then
    begin
      User.PasswordHash := HashPassword(ANewPassword);
      Result := True;
    end;
  end;
end;

function TAuthenticationManager.HasPermission(AUser: TWorkflowUser;
  const AAction: string): Boolean;
begin
  Result := False;

  case AUser.Role of
    urViewer:
      Result := (AAction = 'view');
    urUser:
      Result := (AAction = 'view') or (AAction = 'execute');
    urManager:
      Result := (AAction = 'view') or (AAction = 'execute') or
                (AAction = 'create') or (AAction = 'modify');
    urAdmin:
      Result := True; // Tous les droits
  end;
end;

end.
```

---

## Conclusion et bonnes pratiques

### Meilleures pratiques

#### 1. Conception de workflows

- **Simplicité**: Garder les workflows aussi simples que possible
- **Modularité**: Diviser les processus complexes en sous-processus
- **Cohérence**: Utiliser des conventions de nommage claires
- **Documentation**: Documenter chaque activité et transition
- **Testabilité**: Concevoir des workflows faciles à tester

#### 2. Performance

- **Indexation**: Créer des index sur les colonnes fréquemment interrogées
- **Cache**: Utiliser le cache pour les processus fréquemment accédés
- **Pagination**: Limiter le nombre de résultats retournés par les requêtes
- **Asynchrone**: Utiliser le pool de threads pour les tâches longues
- **Monitoring**: Surveiller les métriques de performance régulièrement

#### 3. Sécurité

- **Authentification**: Toujours authentifier les utilisateurs
- **Autorisation**: Vérifier les permissions pour chaque action
- **Chiffrement**: Chiffrer les données sensibles
- **Audit**: Logger toutes les actions importantes
- **Validation**: Valider toutes les entrées utilisateur

#### 4. Maintenance

- **Versioning**: Gérer les versions des processus
- **Backup**: Sauvegarder régulièrement la base de données
- **Logs**: Conserver des logs détaillés
- **Documentation**: Maintenir la documentation à jour
- **Tests**: Exécuter des tests automatisés régulièrement

### Points clés à retenir

1. **Architecture modulaire**: Le moteur est divisé en composants indépendants (modèle, moteur, persistance, API)

2. **Multi-plateforme**: Le code fonctionne identiquement sur Windows et Linux grâce aux directives de compilation conditionnelle

3. **Extensibilité**: Le système peut être étendu avec des services externes, des plugins et des intégrations

4. **Scalabilité**: L'utilisation du cache, du pool de threads et de l'indexation permet de gérer de gros volumes

5. **Standards**: L'API REST et le format XML permettent l'interopérabilité avec d'autres systèmes

### Évolutions possibles

- **Interface web**: Créer une interface web avec Pas2JS
- **Mobile**: Développer des applications mobiles pour Android/iOS
- **Cloud**: Déployer sur AWS, Azure ou Google Cloud
- **Big Data**: Intégrer avec des systèmes de traitement de données massives
- **IA/ML**: Ajouter des capacités de prédiction et d'optimisation
- **Blockchain**: Utiliser la blockchain pour l'audit et la traçabilité
- **Microservices**: Diviser le moteur en microservices pour une meilleure scalabilité

### Ressources complémentaires

#### Documentation

- **Manuel utilisateur complet**: Guide détaillé pour les utilisateurs finaux
- **Documentation développeur**: API reference et guides d'extension
- **Tutoriels vidéo**: Séries de vidéos sur YouTube
- **Blog technique**: Articles sur les fonctionnalités avancées

#### Communauté

- **Forum**: Discussions et entraide entre utilisateurs
- **GitHub**: Code source, issues et pull requests
- **Discord**: Chat en temps réel avec la communauté
- **Meetups**: Rencontres régionales d'utilisateurs

#### Formation

- **Formation en ligne**: Cours vidéo structurés
- **Certification**: Programme de certification officielle
- **Workshops**: Ateliers pratiques en présentiel
- **Consulting**: Services de conseil et d'implémentation

---

## Récapitulatif du tutoriel

Ce tutoriel complet a couvert la création d'un moteur de workflow/BPM professionnel avec FreePascal/Lazarus :

### Partie 1: Fondations
- Concepts de base des workflows
- Architecture du système
- Modèle de données (processus, activités, transitions)
- Moteur d'exécution

### Partie 2: Persistance et règles
- Base de données SQLite multi-plateforme
- Système de règles et évaluateur d'expressions
- Sauvegarde/chargement en XML

### Partie 3: API et déploiement
- API REST complète
- Application principale intégrée
- Configuration multi-plateforme
- Scripts de déploiement Windows/Linux
- Système de journalisation

### Partie 4: Tests et optimisation
- Tests unitaires avec FPCUnit
- Cache en mémoire
- Pool de threads
- Métriques et monitoring

### Partie 5: Extensions et finalisation
- Exemples concrets d'utilisation
- Intégrations avec services externes
- Sécurité et authentification
- Bonnes pratiques et évolutions

Le moteur résultant est un système complet, performant et extensible, capable de gérer des workflows complexes en production sur Windows et Linux.


**Bonne chance dans vos développements de workflows !**

⏭️ [Système de monitoring distribué](/25-projets-complexes-etudes-cas/06-systeme-monitoring-distribue.md)
