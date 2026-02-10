🔝 Retour au [Sommaire](/SOMMAIRE.md)

# PowerShell et scripts système avec FreePascal/Lazarus

## Introduction : Qu'est-ce que PowerShell ?

### Comprendre PowerShell

**PowerShell** est le shell (interface en ligne de commande) moderne de Windows. C'est bien plus qu'un simple remplacement de l'invite de commandes (cmd.exe) : c'est un environnement de script complet et puissant.

### Différences avec l'invite de commandes classique

| Invite de commandes (cmd) | PowerShell |
|---------------------------|------------|
| Commandes texte simples | Objets .NET complets |
| Scripts .bat limités | Scripts .ps1 puissants |
| Syntaxe héritée de DOS | Syntaxe moderne et cohérente |
| Fonctionnalités basiques | Accès complet au système |

### Pourquoi utiliser PowerShell depuis FreePascal ?

- **Automatisation** : Exécuter des tâches système complexes
- **Administration** : Gérer Windows et ses services
- **Intégration** : Accéder aux fonctionnalités Windows modernes
- **Déploiement** : Installer et configurer des applications
- **Maintenance** : Diagnostiquer et réparer des problèmes

## Concepts fondamentaux de PowerShell

### Les cmdlets

Les **cmdlets** (command-lets) sont les commandes PowerShell. Elles suivent le format `Verbe-Nom` :

```powershell
# Exemples de cmdlets
Get-Process      # Obtenir la liste des processus
Set-Location     # Changer de répertoire
New-Item         # Créer un fichier ou dossier
Remove-Item      # Supprimer un élément
Start-Service    # Démarrer un service
```

### Les objets

PowerShell travaille avec des objets, pas du texte :

```powershell
# Obtenir des informations sur un processus
$process = Get-Process "notepad"
$process.Id           # ID du processus
$process.WorkingSet   # Mémoire utilisée
$process.StartTime    # Heure de démarrage
```

### Les pipes

Le pipe (`|`) permet de chaîner les commandes :

```powershell
# Obtenir les 5 processus utilisant le plus de mémoire
Get-Process | Sort-Object WorkingSet -Descending | Select-Object -First 5
```

## Exécuter PowerShell depuis FreePascal

### Méthode simple : ShellExecute

```pascal
uses
  Windows, ShellAPI, SysUtils;

procedure ExecuterCommandePowerShell(const Commande: string);
var
  Params: string;
begin
  // Encoder la commande pour PowerShell
  Params := '-NoProfile -ExecutionPolicy Bypass -Command "' + Commande + '"';

  // Exécuter PowerShell
  ShellExecute(0, 'open', 'powershell.exe', PChar(Params), nil, SW_SHOW);
end;

// Exemple d'utilisation
begin
  ExecuterCommandePowerShell('Get-Process | Out-GridView');
end;
```

### Méthode avancée : TProcess avec capture de sortie

```pascal
uses
  Classes, SysUtils, Process;

function ExecuterPowerShell(const Commande: string; var Sortie: string): Boolean;
var
  Process: TProcess;
  OutputList: TStringList;
  Params: string;
begin
  Result := False;
  Sortie := '';

  Process := TProcess.Create(nil);
  OutputList := TStringList.Create;
  try
    // Configurer le processus
    Process.Executable := 'powershell.exe';

    // Paramètres PowerShell
    Process.Parameters.Add('-NoProfile');
    Process.Parameters.Add('-NoLogo');
    Process.Parameters.Add('-NonInteractive');
    Process.Parameters.Add('-ExecutionPolicy');
    Process.Parameters.Add('Bypass');
    Process.Parameters.Add('-Command');
    Process.Parameters.Add(Commande);

    // Options pour capturer la sortie
    Process.Options := [poWaitOnExit, poUsePipes, poNoConsole];
    Process.ShowWindow := swoHide;

    // Exécuter
    Process.Execute;

    // Lire la sortie
    OutputList.LoadFromStream(Process.Output);
    Sortie := OutputList.Text;

    Result := Process.ExitStatus = 0;

  finally
    OutputList.Free;
    Process.Free;
  end;
end;

// Exemple d'utilisation
var
  Resultat: string;
begin
  if ExecuterPowerShell('Get-Date -Format "yyyy-MM-dd HH:mm:ss"', Resultat) then
    WriteLn('Date/heure : ', Resultat)
  else
    WriteLn('Erreur lors de l''exécution');
end;
```

### Classe wrapper PowerShell complète

```pascal
unit PowerShellWrapper;

interface

uses
  Classes, SysUtils, Process, fpjson, jsonparser;

type
  TPowerShell = class
  private
    FProcess: TProcess;
    FOutputBuffer: TStringList;
    FErrorBuffer: TStringList;
    FLastExitCode: Integer;

    function PrepareCommand(const Command: string): string;
  public
    constructor Create;
    destructor Destroy; override;

    function Execute(const Command: string): Boolean;
    function ExecuteScript(const ScriptFile: string): Boolean;
    function ExecuteAndGetOutput(const Command: string): string;
    function ExecuteAndGetJSON(const Command: string): TJSONData;

    procedure GetSystemInfo;
    procedure GetProcessList(List: TStrings);
    procedure GetServiceList(List: TStrings);

    property Output: TStringList read FOutputBuffer;
    property Errors: TStringList read FErrorBuffer;
    property LastExitCode: Integer read FLastExitCode;
  end;

implementation

constructor TPowerShell.Create;
begin
  FOutputBuffer := TStringList.Create;
  FErrorBuffer := TStringList.Create;
  FLastExitCode := 0;
end;

destructor TPowerShell.Destroy;
begin
  FOutputBuffer.Free;
  FErrorBuffer.Free;
  inherited;
end;

function TPowerShell.PrepareCommand(const Command: string): string;
begin
  // Échapper les caractères spéciaux si nécessaire
  Result := StringReplace(Command, '"', '\"', [rfReplaceAll]);
end;

function TPowerShell.Execute(const Command: string): Boolean;
begin
  FOutputBuffer.Clear;
  FErrorBuffer.Clear;

  FProcess := TProcess.Create(nil);
  try
    FProcess.Executable := 'powershell.exe';
    FProcess.Parameters.Add('-NoProfile');
    FProcess.Parameters.Add('-NoLogo');
    FProcess.Parameters.Add('-NonInteractive');
    FProcess.Parameters.Add('-ExecutionPolicy');
    FProcess.Parameters.Add('Bypass');
    FProcess.Parameters.Add('-OutputFormat');
    FProcess.Parameters.Add('Text');
    FProcess.Parameters.Add('-Command');
    FProcess.Parameters.Add(PrepareCommand(Command));

    FProcess.Options := [poWaitOnExit, poUsePipes, poNoConsole];
    FProcess.ShowWindow := swoHide;

    try
      FProcess.Execute;

      // Capturer la sortie standard
      FOutputBuffer.LoadFromStream(FProcess.Output);

      // Capturer les erreurs
      if FProcess.Stderr.Size > 0 then
        FErrorBuffer.LoadFromStream(FProcess.Stderr);

      FLastExitCode := FProcess.ExitStatus;
      Result := FLastExitCode = 0;

    except
      on E: Exception do
      begin
        FErrorBuffer.Add('Exception: ' + E.Message);
        FLastExitCode := -1;
        Result := False;
      end;
    end;

  finally
    FProcess.Free;
  end;
end;

function TPowerShell.ExecuteScript(const ScriptFile: string): Boolean;
var
  Command: string;
begin
  if not FileExists(ScriptFile) then
  begin
    FErrorBuffer.Add('Script file not found: ' + ScriptFile);
    Result := False;
    Exit;
  end;

  Command := '& "' + ScriptFile + '"';
  Result := Execute(Command);
end;

function TPowerShell.ExecuteAndGetOutput(const Command: string): string;
begin
  if Execute(Command) then
    Result := FOutputBuffer.Text
  else
    Result := '';
end;

function TPowerShell.ExecuteAndGetJSON(const Command: string): TJSONData;
var
  JsonCommand: string;
  JsonOutput: string;
begin
  Result := nil;

  // Ajouter ConvertTo-Json à la commande
  JsonCommand := '(' + Command + ') | ConvertTo-Json -Depth 10';

  if Execute(JsonCommand) then
  begin
    JsonOutput := FOutputBuffer.Text;
    if JsonOutput <> '' then
    begin
      try
        Result := GetJSON(JsonOutput);
      except
        on E: Exception do
        begin
          FErrorBuffer.Add('JSON Parse Error: ' + E.Message);
          Result := nil;
        end;
      end;
    end;
  end;
end;

procedure TPowerShell.GetSystemInfo;
begin
  Execute('Get-ComputerInfo | Select-Object CsName, OsName, OsVersion, ' +
          'OsArchitecture, CsProcessors, CsTotalPhysicalMemory');
end;

procedure TPowerShell.GetProcessList(List: TStrings);
begin
  List.Clear;

  if Execute('Get-Process | Select-Object Id, ProcessName, WorkingSet | ' +
             'ForEach-Object { "$($_.Id)`t$($_.ProcessName)`t$($_.WorkingSet)" }') then
  begin
    List.Assign(FOutputBuffer);
  end;
end;

procedure TPowerShell.GetServiceList(List: TStrings);
begin
  List.Clear;

  if Execute('Get-Service | Select-Object Name, DisplayName, Status | ' +
             'ForEach-Object { "$($_.Name)`t$($_.DisplayName)`t$($_.Status)" }') then
  begin
    List.Assign(FOutputBuffer);
  end;
end;

end.
```

## Génération de scripts PowerShell

### Générateur de scripts PowerShell

```pascal
unit PSScriptGenerator;

interface

uses
  Classes, SysUtils;

type
  TPSScriptGenerator = class
  private
    FScript: TStringList;
    FIndentLevel: Integer;

    procedure AddLine(const Line: string);
    procedure Indent;
    procedure Unindent;
  public
    constructor Create;
    destructor Destroy; override;

    // Commandes de base
    procedure AddComment(const Comment: string);
    procedure AddVariable(const Name, Value: string);
    procedure AddCommand(const Command: string);

    // Structures de contrôle
    procedure BeginIf(const Condition: string);
    procedure AddElseIf(const Condition: string);
    procedure AddElse;
    procedure EndIf;

    procedure BeginForEach(const Item, Collection: string);
    procedure EndForEach;

    procedure BeginTryCatch;
    procedure BeginCatch;
    procedure EndTryCatch;

    // Fonctions système
    procedure AddGetProcess(const ProcessName: string = '');
    procedure AddGetService(const ServiceName: string = '');
    procedure AddFileOperation(const Operation, Path: string);
    procedure AddRegistryOperation(const Operation, Key, Value: string);

    // Génération et sauvegarde
    function GenerateScript: string;
    procedure SaveToFile(const FileName: string);
  end;

implementation

constructor TPSScriptGenerator.Create;
begin
  FScript := TStringList.Create;
  FIndentLevel := 0;

  // En-tête du script
  AddComment('Script PowerShell généré automatiquement');
  AddComment('Date : ' + DateTimeToStr(Now));
  AddLine('');
  AddLine('# Configuration');
  AddLine('$ErrorActionPreference = "Stop"');
  AddLine('');
end;

destructor TPSScriptGenerator.Destroy;
begin
  FScript.Free;
  inherited;
end;

procedure TPSScriptGenerator.AddLine(const Line: string);
var
  IndentStr: string;
  i: Integer;
begin
  IndentStr := '';
  for i := 1 to FIndentLevel * 4 do
    IndentStr := IndentStr + ' ';

  FScript.Add(IndentStr + Line);
end;

procedure TPSScriptGenerator.Indent;
begin
  Inc(FIndentLevel);
end;

procedure TPSScriptGenerator.Unindent;
begin
  if FIndentLevel > 0 then
    Dec(FIndentLevel);
end;

procedure TPSScriptGenerator.AddComment(const Comment: string);
begin
  AddLine('# ' + Comment);
end;

procedure TPSScriptGenerator.AddVariable(const Name, Value: string);
begin
  if Pos(' ', Value) > 0 then
    AddLine('$' + Name + ' = "' + Value + '"')
  else
    AddLine('$' + Name + ' = ' + Value);
end;

procedure TPSScriptGenerator.AddCommand(const Command: string);
begin
  AddLine(Command);
end;

procedure TPSScriptGenerator.BeginIf(const Condition: string);
begin
  AddLine('if (' + Condition + ') {');
  Indent;
end;

procedure TPSScriptGenerator.AddElseIf(const Condition: string);
begin
  Unindent;
  AddLine('} elseif (' + Condition + ') {');
  Indent;
end;

procedure TPSScriptGenerator.AddElse;
begin
  Unindent;
  AddLine('} else {');
  Indent;
end;

procedure TPSScriptGenerator.EndIf;
begin
  Unindent;
  AddLine('}');
end;

procedure TPSScriptGenerator.BeginForEach(const Item, Collection: string);
begin
  AddLine('foreach ($' + Item + ' in ' + Collection + ') {');
  Indent;
end;

procedure TPSScriptGenerator.EndForEach;
begin
  Unindent;
  AddLine('}');
end;

procedure TPSScriptGenerator.BeginTryCatch;
begin
  AddLine('try {');
  Indent;
end;

procedure TPSScriptGenerator.BeginCatch;
begin
  Unindent;
  AddLine('} catch {');
  Indent;
end;

procedure TPSScriptGenerator.EndTryCatch;
begin
  Unindent;
  AddLine('}');
end;

procedure TPSScriptGenerator.AddGetProcess(const ProcessName: string);
begin
  if ProcessName = '' then
    AddLine('$processes = Get-Process')
  else
    AddLine('$process = Get-Process -Name "' + ProcessName + '" -ErrorAction SilentlyContinue');
end;

procedure TPSScriptGenerator.AddGetService(const ServiceName: string);
begin
  if ServiceName = '' then
    AddLine('$services = Get-Service')
  else
    AddLine('$service = Get-Service -Name "' + ServiceName + '" -ErrorAction SilentlyContinue');
end;

procedure TPSScriptGenerator.AddFileOperation(const Operation, Path: string);
begin
  if LowerCase(Operation) = 'create' then
    AddLine('New-Item -Path "' + Path + '" -ItemType File -Force')
  else if LowerCase(Operation) = 'delete' then
    AddLine('Remove-Item -Path "' + Path + '" -Force')
  else if LowerCase(Operation) = 'exists' then
    AddLine('$exists = Test-Path -Path "' + Path + '"')
  else if LowerCase(Operation) = 'copy' then
    AddLine('Copy-Item -Path "' + Path + '" -Destination "' + Path + '.bak"')
  else if LowerCase(Operation) = 'move' then
    AddLine('Move-Item -Path "' + Path + '" -Destination "' + Path + '.old"');
end;

procedure TPSScriptGenerator.AddRegistryOperation(const Operation, Key, Value: string);
var
  RegPath: string;
begin
  RegPath := 'Registry::' + Key;

  if LowerCase(Operation) = 'get' then
    AddLine('$regValue = Get-ItemProperty -Path "' + RegPath + '" -Name "' + Value + '"')
  else if LowerCase(Operation) = 'set' then
    AddLine('Set-ItemProperty -Path "' + RegPath + '" -Name "' + Value + '" -Value $value')
  else if LowerCase(Operation) = 'create' then
    AddLine('New-Item -Path "' + RegPath + '" -Force')
  else if LowerCase(Operation) = 'delete' then
    AddLine('Remove-ItemProperty -Path "' + RegPath + '" -Name "' + Value + '"');
end;

function TPSScriptGenerator.GenerateScript: string;
begin
  Result := FScript.Text;
end;

procedure TPSScriptGenerator.SaveToFile(const FileName: string);
begin
  FScript.SaveToFile(FileName);
end;

end.
```

### Exemple d'utilisation du générateur

```pascal
program ExempleGenerateurPS;

uses
  PSScriptGenerator, SysUtils;

procedure GenererScriptMaintenance;
var
  Generator: TPSScriptGenerator;
begin
  Generator := TPSScriptGenerator.Create;
  try
    // Ajouter des variables
    Generator.AddComment('Variables de configuration');
    Generator.AddVariable('logPath', 'C:\Logs\maintenance.log');
    Generator.AddVariable('backupPath', 'C:\Backup');
    Generator.AddLine('');

    // Fonction de log
    Generator.AddComment('Fonction de journalisation');
    Generator.AddLine('function Write-Log {');
    Generator.Indent;
    Generator.AddLine('param([string]$Message)');
    Generator.AddLine('$timestamp = Get-Date -Format "yyyy-MM-dd HH:mm:ss"');
    Generator.AddLine('"$timestamp - $Message" | Add-Content -Path $logPath');
    Generator.Unindent;
    Generator.AddLine('}');
    Generator.AddLine('');

    // Début du script principal
    Generator.AddComment('Script principal');
    Generator.AddLine('Write-Log "Début de la maintenance"');
    Generator.AddLine('');

    // Vérifier un service
    Generator.AddComment('Vérification du service');
    Generator.AddGetService('MonService');
    Generator.BeginIf('$service -eq $null');
    Generator.AddLine('Write-Log "Service non trouvé"');
    Generator.AddElseIf('$service.Status -ne "Running"');
    Generator.AddLine('Write-Log "Redémarrage du service"');
    Generator.AddLine('Start-Service -Name "MonService"');
    Generator.AddElse;
    Generator.AddLine('Write-Log "Service OK"');
    Generator.EndIf;
    Generator.AddLine('');

    // Nettoyage des fichiers temporaires
    Generator.AddComment('Nettoyage des fichiers temporaires');
    Generator.BeginTryCatch;
    Generator.AddLine('$tempFiles = Get-ChildItem -Path "C:\Temp" -Filter "*.tmp"');
    Generator.BeginForEach('file', '$tempFiles');
    Generator.AddLine('Remove-Item $file.FullName -Force');
    Generator.AddLine('Write-Log "Supprimé : $($file.Name)"');
    Generator.EndForEach;
    Generator.BeginCatch;
    Generator.AddLine('Write-Log "Erreur : $_"');
    Generator.EndTryCatch;
    Generator.AddLine('');

    // Fin du script
    Generator.AddLine('Write-Log "Maintenance terminée"');

    // Sauvegarder le script
    Generator.SaveToFile('maintenance.ps1');
    WriteLn('Script PowerShell généré : maintenance.ps1');

  finally
    Generator.Free;
  end;
end;

begin
  GenererScriptMaintenance;
end;
```

## Scripts système courants

### Gestion des processus

```pascal
unit ProcessManagement;

interface

uses
  Classes, SysUtils, PowerShellWrapper;

type
  TProcessManager = class
  private
    FPowerShell: TPowerShell;
  public
    constructor Create;
    destructor Destroy; override;

    function IsProcessRunning(const ProcessName: string): Boolean;
    function GetProcessInfo(const ProcessName: string): string;
    function KillProcess(const ProcessName: string): Boolean;
    function StartProcess(const ExePath: string; const Arguments: string = ''): Boolean;
    function GetProcessMemory(const ProcessName: string): Int64;
    procedure ListTopProcessesByMemory(Count: Integer; List: TStrings);
  end;

implementation

constructor TProcessManager.Create;
begin
  FPowerShell := TPowerShell.Create;
end;

destructor TProcessManager.Destroy;
begin
  FPowerShell.Free;
  inherited;
end;

function TProcessManager.IsProcessRunning(const ProcessName: string): Boolean;
var
  Command: string;
  Output: string;
begin
  Command := Format('(Get-Process -Name "%s" -ErrorAction SilentlyContinue) -ne $null',
                    [ProcessName]);
  Output := FPowerShell.ExecuteAndGetOutput(Command);
  Result := Trim(Output) = 'True';
end;

function TProcessManager.GetProcessInfo(const ProcessName: string): string;
var
  Command: string;
begin
  Command := Format(
    'Get-Process -Name "%s" -ErrorAction SilentlyContinue | ' +
    'Select-Object Id, ProcessName, StartTime, WorkingSet, CPU | ' +
    'Format-List',
    [ProcessName]
  );
  Result := FPowerShell.ExecuteAndGetOutput(Command);
end;

function TProcessManager.KillProcess(const ProcessName: string): Boolean;
var
  Command: string;
begin
  Command := Format('Stop-Process -Name "%s" -Force -ErrorAction SilentlyContinue',
                    [ProcessName]);
  Result := FPowerShell.Execute(Command);
end;

function TProcessManager.StartProcess(const ExePath: string; const Arguments: string): Boolean;
var
  Command: string;
begin
  if Arguments = '' then
    Command := Format('Start-Process -FilePath "%s" -PassThru', [ExePath])
  else
    Command := Format('Start-Process -FilePath "%s" -ArgumentList "%s" -PassThru',
                      [ExePath, Arguments]);

  Result := FPowerShell.Execute(Command);
end;

function TProcessManager.GetProcessMemory(const ProcessName: string): Int64;
var
  Command: string;
  Output: string;
begin
  Result := 0;

  Command := Format(
    '$p = Get-Process -Name "%s" -ErrorAction SilentlyContinue; ' +
    'if ($p) { $p.WorkingSet64 } else { 0 }',
    [ProcessName]
  );

  Output := FPowerShell.ExecuteAndGetOutput(Command);
  Result := StrToInt64Def(Trim(Output), 0);
end;

procedure TProcessManager.ListTopProcessesByMemory(Count: Integer; List: TStrings);
var
  Command: string;
begin
  Command := Format(
    'Get-Process | Sort-Object WorkingSet64 -Descending | ' +
    'Select-Object -First %d | ' +
    'ForEach-Object { "$($_.ProcessName)`t$([math]::Round($_.WorkingSet64/1MB, 2)) MB" }',
    [Count]
  );

  if FPowerShell.Execute(Command) then
    List.Assign(FPowerShell.Output);
end;

end.
```

### Gestion des services Windows

```pascal
unit ServiceManagement;

interface

uses
  Classes, SysUtils, PowerShellWrapper;

type
  TServiceStatus = (ssUnknown, ssStopped, ssStartPending, ssStopPending,
                    ssRunning, ssContinuePending, ssPausePending, ssPaused);

  TServiceManager = class
  private
    FPowerShell: TPowerShell;

    function StatusStringToEnum(const Status: string): TServiceStatus;
  public
    constructor Create;
    destructor Destroy; override;

    function GetServiceStatus(const ServiceName: string): TServiceStatus;
    function StartService(const ServiceName: string): Boolean;
    function StopService(const ServiceName: string): Boolean;
    function RestartService(const ServiceName: string): Boolean;
    function ServiceExists(const ServiceName: string): Boolean;
    function InstallService(const ServiceName, BinaryPath, DisplayName: string): Boolean;
    function UninstallService(const ServiceName: string): Boolean;
    procedure GetServiceList(List: TStrings; RunningOnly: Boolean = False);
  end;

implementation

constructor TServiceManager.Create;
begin
  FPowerShell := TPowerShell.Create;
end;

destructor TServiceManager.Destroy;
begin
  FPowerShell.Free;
  inherited;
end;

function TServiceManager.StatusStringToEnum(const Status: string): TServiceStatus;
begin
  if Status = 'Stopped' then Result := ssStopped
  else if Status = 'StartPending' then Result := ssStartPending
  else if Status = 'StopPending' then Result := ssStopPending
  else if Status = 'Running' then Result := ssRunning
  else if Status = 'ContinuePending' then Result := ssContinuePending
  else if Status = 'PausePending' then Result := ssPausePending
  else if Status = 'Paused' then Result := ssPaused
  else Result := ssUnknown;
end;

function TServiceManager.GetServiceStatus(const ServiceName: string): TServiceStatus;
var
  Command, Output: string;
begin
  Command := Format('(Get-Service -Name "%s" -ErrorAction SilentlyContinue).Status',
                    [ServiceName]);
  Output := Trim(FPowerShell.ExecuteAndGetOutput(Command));
  Result := StatusStringToEnum(Output);
end;

function TServiceManager.StartService(const ServiceName: string): Boolean;
var
  Command: string;
begin
  Command := Format('Start-Service -Name "%s" -PassThru | Out-Null', [ServiceName]);
  Result := FPowerShell.Execute(Command);
end;

function TServiceManager.StopService(const ServiceName: string): Boolean;
var
  Command: string;
begin
  Command := Format('Stop-Service -Name "%s" -Force -PassThru | Out-Null', [ServiceName]);
  Result := FPowerShell.Execute(Command);
end;

function TServiceManager.RestartService(const ServiceName: string): Boolean;
var
  Command: string;
begin
  Command := Format('Restart-Service -Name "%s" -Force -PassThru | Out-Null', [ServiceName]);
  Result := FPowerShell.Execute(Command);
end;

function TServiceManager.ServiceExists(const ServiceName: string): Boolean;
var
  Command, Output: string;
begin
  Command := Format('(Get-Service -Name "%s" -ErrorAction SilentlyContinue) -ne $null',
                    [ServiceName]);
  Output := FPowerShell.ExecuteAndGetOutput(Command);
  Result := Trim(Output) = 'True';
end;

function TServiceManager.InstallService(const ServiceName, BinaryPath, DisplayName: string): Boolean;
var
  Command: string;
begin
  Command := Format(
    'New-Service -Name "%s" -BinaryPathName "%s" -DisplayName "%s" -StartupType Automatic',
    [ServiceName, BinaryPath, DisplayName]
  );
  Result := FPowerShell.Execute(Command);
end;

function TServiceManager.UninstallService(const ServiceName: string): Boolean;
var
  Command: string;
begin
  // Arrêter le service d'abord
  StopService(ServiceName);

  // PowerShell 6+ ou utiliser sc.exe pour les versions antérieures
  Command := Format(
    'if (Get-Command Remove-Service -ErrorAction SilentlyContinue) { ' +
    '  Remove-Service -Name "%s" ' +
    '} else { ' +
    '  & sc.exe delete "%s" ' +
    '}',
    [ServiceName, ServiceName]
  );
  Result := FPowerShell.Execute(Command);
end;

procedure TServiceManager.GetServiceList(List: TStrings; RunningOnly: Boolean);
var
  Command: string;
begin
  if RunningOnly then
    Command := 'Get-Service | Where-Object {$_.Status -eq "Running"} | ' +
               'ForEach-Object { "$($_.Name)`t$($_.DisplayName)" }'
  else
    Command := 'Get-Service | ForEach-Object { "$($_.Name)`t$($_.DisplayName)`t$($_.Status)" }';

  if FPowerShell.Execute(Command) then
    List.Assign(FPowerShell.Output);
end;

end.
```

### Gestion du registre Windows

```pascal
unit RegistryManagement;

interface

uses
  Classes, SysUtils, PowerShellWrapper;

type
  TRegistryManager = class
  private
    FPowerShell: TPowerShell;

    function ConvertHiveToPS(Hive: string): string;
  public
    constructor Create;
    destructor Destroy; override;

    function ReadValue(const Hive, Key, ValueName: string): string;
    function WriteValue(const Hive, Key, ValueName, Value: string;
                       ValueType: string = 'String'): Boolean;
    function DeleteValue(const Hive, Key, ValueName: string): Boolean;
    function CreateKey(const Hive, Key: string): Boolean;
    function DeleteKey(const Hive, Key: string): Boolean;
    function KeyExists(const Hive, Key: string): Boolean;
    procedure ListSubKeys(const Hive, Key: string; List: TStrings);
    procedure ListValues(const Hive, Key: string; List: TStrings);
    function ExportKey(const Hive, Key, FilePath: string): Boolean;
  end;

implementation

constructor TRegistryManager.Create;
begin
  FPowerShell := TPowerShell.Create;
end;

destructor TRegistryManager.Destroy;
begin
  FPowerShell.Free;
  inherited;
end;

function TRegistryManager.ConvertHiveToPS(Hive: string): string;
begin
  Hive := UpperCase(Hive);
  if (Hive = 'HKLM') or (Hive = 'HKEY_LOCAL_MACHINE') then
    Result := 'HKLM:'
  else if (Hive = 'HKCU') or (Hive = 'HKEY_CURRENT_USER') then
    Result := 'HKCU:'
  else if (Hive = 'HKCR') or (Hive = 'HKEY_CLASSES_ROOT') then
    Result := 'HKCR:'
  else if (Hive = 'HKU') or (Hive = 'HKEY_USERS') then
    Result := 'HKU:'
  else if (Hive = 'HKCC') or (Hive = 'HKEY_CURRENT_CONFIG') then
    Result := 'HKCC:'
  else
    Result := Hive;
end;

function TRegistryManager.ReadValue(const Hive, Key, ValueName: string): string;
var
  Command: string;
begin
  Command := Format(
    '$value = Get-ItemProperty -Path "%s\%s" -Name "%s" -ErrorAction SilentlyContinue; ' +
    'if ($value) { $value.%s } else { "" }',
    [ConvertHiveToPS(Hive), Key, ValueName, ValueName]
  );
  Result := Trim(FPowerShell.ExecuteAndGetOutput(Command));
end;

function TRegistryManager.WriteValue(const Hive, Key, ValueName, Value: string;
                                    ValueType: string): Boolean;
var
  Command: string;
  PSType: string;
begin
  // Convertir le type en type PowerShell
  if ValueType = 'DWORD' then
    PSType := 'DWord'
  else if ValueType = 'QWORD' then
    PSType := 'QWord'
  else if ValueType = 'Binary' then
    PSType := 'Binary'
  else if ValueType = 'MultiString' then
    PSType := 'MultiString'
  else if ValueType = 'ExpandString' then
    PSType := 'ExpandString'
  else
    PSType := 'String';

  // Créer la clé si elle n'existe pas
  Command := Format(
    'if (!(Test-Path "%s\%s")) { ' +
    '  New-Item -Path "%s\%s" -Force | Out-Null ' +
    '} ' +
    'Set-ItemProperty -Path "%s\%s" -Name "%s" -Value "%s" -Type %s',
    [ConvertHiveToPS(Hive), Key,
     ConvertHiveToPS(Hive), Key,
     ConvertHiveToPS(Hive), Key, ValueName, Value, PSType]
  );

  Result := FPowerShell.Execute(Command);
end;

function TRegistryManager.DeleteValue(const Hive, Key, ValueName: string): Boolean;
var
  Command: string;
begin
  Command := Format(
    'Remove-ItemProperty -Path "%s\%s" -Name "%s" -Force -ErrorAction SilentlyContinue',
    [ConvertHiveToPS(Hive), Key, ValueName]
  );
  Result := FPowerShell.Execute(Command);
end;

function TRegistryManager.CreateKey(const Hive, Key: string): Boolean;
var
  Command: string;
begin
  Command := Format(
    'New-Item -Path "%s\%s" -Force | Out-Null',
    [ConvertHiveToPS(Hive), Key]
  );
  Result := FPowerShell.Execute(Command);
end;

function TRegistryManager.DeleteKey(const Hive, Key: string): Boolean;
var
  Command: string;
begin
  Command := Format(
    'Remove-Item -Path "%s\%s" -Recurse -Force -ErrorAction SilentlyContinue',
    [ConvertHiveToPS(Hive), Key]
  );
  Result := FPowerShell.Execute(Command);
end;

function TRegistryManager.KeyExists(const Hive, Key: string): Boolean;
var
  Command, Output: string;
begin
  Command := Format(
    'Test-Path -Path "%s\%s"',
    [ConvertHiveToPS(Hive), Key]
  );
  Output := FPowerShell.ExecuteAndGetOutput(Command);
  Result := Trim(Output) = 'True';
end;

procedure TRegistryManager.ListSubKeys(const Hive, Key: string; List: TStrings);
var
  Command: string;
begin
  Command := Format(
    'Get-ChildItem -Path "%s\%s" -ErrorAction SilentlyContinue | ' +
    'ForEach-Object { $_.PSChildName }',
    [ConvertHiveToPS(Hive), Key]
  );

  if FPowerShell.Execute(Command) then
    List.Assign(FPowerShell.Output);
end;

procedure TRegistryManager.ListValues(const Hive, Key: string; List: TStrings);
var
  Command: string;
begin
  Command := Format(
    'Get-ItemProperty -Path "%s\%s" | ' +
    'Get-Member -MemberType NoteProperty | ' +
    'Where-Object { $_.Name -notlike "PS*" } | ' +
    'ForEach-Object { $_.Name }',
    [ConvertHiveToPS(Hive), Key]
  );

  if FPowerShell.Execute(Command) then
    List.Assign(FPowerShell.Output);
end;

function TRegistryManager.ExportKey(const Hive, Key, FilePath: string): Boolean;
var
  Command: string;
  FullPath: string;
begin
  // Construire le chemin complet de la clé
  if Pos(':', Hive) > 0 then
    FullPath := Hive + '\' + Key
  else
    FullPath := Hive + '\' + Key;

  Command := Format(
    '& reg export "%s" "%s" /y',
    [FullPath, FilePath]
  );

  Result := FPowerShell.Execute(Command);
end;
```

## Gestion des fichiers et dossiers

### Opérations avancées sur les fichiers

```pascal
unit FileManagement;

interface

uses
  Classes, SysUtils, PowerShellWrapper;

type
  TFileInfo = record
    Name: string;
    FullPath: string;
    Size: Int64;
    CreationTime: TDateTime;
    LastWriteTime: TDateTime;
    IsReadOnly: Boolean;
    IsHidden: Boolean;
    IsSystem: Boolean;
  end;

  TFileManager = class
  private
    FPowerShell: TPowerShell;

    function ParseFileInfo(const PSOutput: string): TFileInfo;
  public
    constructor Create;
    destructor Destroy; override;

    function GetFileInfo(const FilePath: string): TFileInfo;
    function CopyFile(const Source, Destination: string; Overwrite: Boolean = False): Boolean;
    function MoveFile(const Source, Destination: string): Boolean;
    function DeleteFile(const FilePath: string; Force: Boolean = False): Boolean;
    function FileExists(const FilePath: string): Boolean;

    function CreateDirectory(const Path: string): Boolean;
    function DeleteDirectory(const Path: string; Recursive: Boolean = False): Boolean;
    function DirectoryExists(const Path: string): Boolean;

    procedure ListFiles(const Path, Pattern: string; List: TStrings; Recursive: Boolean = False);
    procedure ListDirectories(const Path: string; List: TStrings);

    function GetFileHash(const FilePath: string; Algorithm: string = 'SHA256'): string;
    function CompareFiles(const File1, File2: string): Boolean;

    function CompressFiles(const SourcePath, ArchivePath: string): Boolean;
    function ExtractArchive(const ArchivePath, DestinationPath: string): Boolean;

    function GetDiskUsage(const Path: string): Int64;
    procedure FindLargeFiles(const Path: string; MinSizeMB: Integer; List: TStrings);
    procedure FindOldFiles(const Path: string; DaysOld: Integer; List: TStrings);
  end;

implementation

constructor TFileManager.Create;
begin
  FPowerShell := TPowerShell.Create;
end;

destructor TFileManager.Destroy;
begin
  FPowerShell.Free;
  inherited;
end;

function TFileManager.GetFileInfo(const FilePath: string): TFileInfo;
var
  Command: string;
  JsonData: TJSONData;
  JsonObj: TJSONObject;
begin
  FillChar(Result, SizeOf(Result), 0);

  Command := Format(
    'Get-Item -Path "%s" -ErrorAction SilentlyContinue | ' +
    'Select-Object Name, FullName, Length, CreationTime, LastWriteTime, ' +
    'IsReadOnly, @{Name="IsHidden";Expression={$_.Attributes -band [System.IO.FileAttributes]::Hidden}}, ' +
    '@{Name="IsSystem";Expression={$_.Attributes -band [System.IO.FileAttributes]::System}}',
    [FilePath]
  );

  JsonData := FPowerShell.ExecuteAndGetJSON(Command);
  if Assigned(JsonData) then
  begin
    try
      JsonObj := JsonData as TJSONObject;

      Result.Name := JsonObj.Get('Name', '');
      Result.FullPath := JsonObj.Get('FullName', '');
      Result.Size := JsonObj.Get('Length', 0);
      // Conversion des dates PowerShell
      Result.CreationTime := Now; // Simplification
      Result.LastWriteTime := Now;
      Result.IsReadOnly := JsonObj.Get('IsReadOnly', False);
      Result.IsHidden := JsonObj.Get('IsHidden', 0) <> 0;
      Result.IsSystem := JsonObj.Get('IsSystem', 0) <> 0;

    finally
      JsonData.Free;
    end;
  end;
end;

function TFileManager.CopyFile(const Source, Destination: string; Overwrite: Boolean): Boolean;
var
  Command: string;
begin
  if Overwrite then
    Command := Format('Copy-Item -Path "%s" -Destination "%s" -Force', [Source, Destination])
  else
    Command := Format('Copy-Item -Path "%s" -Destination "%s"', [Source, Destination]);

  Result := FPowerShell.Execute(Command);
end;

function TFileManager.MoveFile(const Source, Destination: string): Boolean;
var
  Command: string;
begin
  Command := Format('Move-Item -Path "%s" -Destination "%s" -Force', [Source, Destination]);
  Result := FPowerShell.Execute(Command);
end;

function TFileManager.DeleteFile(const FilePath: string; Force: Boolean): Boolean;
var
  Command: string;
begin
  if Force then
    Command := Format('Remove-Item -Path "%s" -Force', [FilePath])
  else
    Command := Format('Remove-Item -Path "%s"', [FilePath]);

  Result := FPowerShell.Execute(Command);
end;

function TFileManager.FileExists(const FilePath: string): Boolean;
var
  Command, Output: string;
begin
  Command := Format('Test-Path -Path "%s" -PathType Leaf', [FilePath]);
  Output := FPowerShell.ExecuteAndGetOutput(Command);
  Result := Trim(Output) = 'True';
end;

function TFileManager.CreateDirectory(const Path: string): Boolean;
var
  Command: string;
begin
  Command := Format('New-Item -Path "%s" -ItemType Directory -Force | Out-Null', [Path]);
  Result := FPowerShell.Execute(Command);
end;

function TFileManager.DeleteDirectory(const Path: string; Recursive: Boolean): Boolean;
var
  Command: string;
begin
  if Recursive then
    Command := Format('Remove-Item -Path "%s" -Recurse -Force', [Path])
  else
    Command := Format('Remove-Item -Path "%s"', [Path]);

  Result := FPowerShell.Execute(Command);
end;

function TFileManager.DirectoryExists(const Path: string): Boolean;
var
  Command, Output: string;
begin
  Command := Format('Test-Path -Path "%s" -PathType Container', [Path]);
  Output := FPowerShell.ExecuteAndGetOutput(Command);
  Result := Trim(Output) = 'True';
end;

procedure TFileManager.ListFiles(const Path, Pattern: string; List: TStrings; Recursive: Boolean);
var
  Command: string;
begin
  if Recursive then
    Command := Format(
      'Get-ChildItem -Path "%s" -Filter "%s" -File -Recurse | ' +
      'ForEach-Object { $_.FullName }',
      [Path, Pattern])
  else
    Command := Format(
      'Get-ChildItem -Path "%s" -Filter "%s" -File | ' +
      'ForEach-Object { $_.FullName }',
      [Path, Pattern]);

  if FPowerShell.Execute(Command) then
    List.Assign(FPowerShell.Output);
end;

procedure TFileManager.ListDirectories(const Path: string; List: TStrings);
var
  Command: string;
begin
  Command := Format(
    'Get-ChildItem -Path "%s" -Directory | ForEach-Object { $_.FullName }',
    [Path]
  );

  if FPowerShell.Execute(Command) then
    List.Assign(FPowerShell.Output);
end;

function TFileManager.GetFileHash(const FilePath: string; Algorithm: string): string;
var
  Command: string;
begin
  Command := Format(
    '(Get-FileHash -Path "%s" -Algorithm %s).Hash',
    [FilePath, Algorithm]
  );
  Result := Trim(FPowerShell.ExecuteAndGetOutput(Command));
end;

function TFileManager.CompareFiles(const File1, File2: string): Boolean;
var
  Command, Output: string;
begin
  Command := Format(
    '$hash1 = (Get-FileHash -Path "%s").Hash; ' +
    '$hash2 = (Get-FileHash -Path "%s").Hash; ' +
    '$hash1 -eq $hash2',
    [File1, File2]
  );
  Output := FPowerShell.ExecuteAndGetOutput(Command);
  Result := Trim(Output) = 'True';
end;

function TFileManager.CompressFiles(const SourcePath, ArchivePath: string): Boolean;
var
  Command: string;
begin
  Command := Format(
    'Compress-Archive -Path "%s" -DestinationPath "%s" -Force',
    [SourcePath, ArchivePath]
  );
  Result := FPowerShell.Execute(Command);
end;

function TFileManager.ExtractArchive(const ArchivePath, DestinationPath: string): Boolean;
var
  Command: string;
begin
  Command := Format(
    'Expand-Archive -Path "%s" -DestinationPath "%s" -Force',
    [ArchivePath, DestinationPath]
  );
  Result := FPowerShell.Execute(Command);
end;

function TFileManager.GetDiskUsage(const Path: string): Int64;
var
  Command, Output: string;
begin
  Command := Format(
    '(Get-ChildItem -Path "%s" -Recurse -ErrorAction SilentlyContinue | ' +
    'Measure-Object -Property Length -Sum).Sum',
    [Path]
  );
  Output := FPowerShell.ExecuteAndGetOutput(Command);
  Result := StrToInt64Def(Trim(Output), 0);
end;

procedure TFileManager.FindLargeFiles(const Path: string; MinSizeMB: Integer; List: TStrings);
var
  Command: string;
begin
  Command := Format(
    'Get-ChildItem -Path "%s" -Recurse -File -ErrorAction SilentlyContinue | ' +
    'Where-Object { $_.Length -gt %d } | ' +
    'Sort-Object Length -Descending | ' +
    'ForEach-Object { "$($_.FullName)`t$([math]::Round($_.Length/1MB, 2)) MB" }',
    [Path, MinSizeMB * 1024 * 1024]
  );

  if FPowerShell.Execute(Command) then
    List.Assign(FPowerShell.Output);
end;

procedure TFileManager.FindOldFiles(const Path: string; DaysOld: Integer; List: TStrings);
var
  Command: string;
begin
  Command := Format(
    '$date = (Get-Date).AddDays(-%d); ' +
    'Get-ChildItem -Path "%s" -Recurse -File -ErrorAction SilentlyContinue | ' +
    'Where-Object { $_.LastWriteTime -lt $date } | ' +
    'ForEach-Object { "$($_.FullName)`t$($_.LastWriteTime)" }',
    [DaysOld, Path]
  );

  if FPowerShell.Execute(Command) then
    List.Assign(FPowerShell.Output);
end;
```

## Scripts de maintenance système

### Nettoyage automatique

```pascal
unit SystemMaintenance;

interface

uses
  Classes, SysUtils, PowerShellWrapper;

type
  TMaintenanceTask = (mtCleanTemp, mtCleanLogs, mtDefrag, mtCheckDisk,
                     mtUpdateWindows, mtCleanRegistry, mtOptimizeStartup);
  TMaintenanceTasks = set of TMaintenanceTask;

  TSystemMaintenance = class
  private
    FPowerShell: TPowerShell;
    FLogFile: string;

    procedure LogMessage(const Message: string);
  public
    constructor Create;
    destructor Destroy; override;

    // Tâches de nettoyage
    function CleanTempFiles: Boolean;
    function CleanWindowsTemp: Boolean;
    function CleanBrowserCache: Boolean;
    function CleanRecycleBin: Boolean;
    function CleanOldLogs(DaysToKeep: Integer = 30): Boolean;

    // Optimisation système
    function DefragmentDisk(const Drive: string = 'C:'): Boolean;
    function CheckDisk(const Drive: string = 'C:'): Boolean;
    function OptimizeStartup: Boolean;

    // Maintenance réseau
    function FlushDNSCache: Boolean;
    function ResetNetworkStack: Boolean;

    // Tâche complète
    function RunMaintenance(Tasks: TMaintenanceTasks): Boolean;

    property LogFile: string read FLogFile write FLogFile;
  end;

implementation

constructor TSystemMaintenance.Create;
begin
  FPowerShell := TPowerShell.Create;
  FLogFile := GetEnvironmentVariable('TEMP') + '\maintenance_' +
              FormatDateTime('yyyymmdd', Now) + '.log';
end;

destructor TSystemMaintenance.Destroy;
begin
  FPowerShell.Free;
  inherited;
end;

procedure TSystemMaintenance.LogMessage(const Message: string);
var
  Command: string;
begin
  Command := Format(
    '$timestamp = Get-Date -Format "yyyy-MM-dd HH:mm:ss"; ' +
    '"{0} - {1}" | Add-Content -Path "{2}"',
    ['$timestamp', Message, FLogFile]
  );
  FPowerShell.Execute(Command);
end;

function TSystemMaintenance.CleanTempFiles: Boolean;
var
  Script: TStringList;
begin
  LogMessage('Début du nettoyage des fichiers temporaires');

  Script := TStringList.Create;
  try
    Script.Add('# Nettoyage des fichiers temporaires utilisateur');
    Script.Add('$tempPath = $env:TEMP');
    Script.Add('$filesToDelete = Get-ChildItem -Path $tempPath -Recurse -ErrorAction SilentlyContinue | ');
    Script.Add('    Where-Object { $_.LastWriteTime -lt (Get-Date).AddDays(-1) }');
    Script.Add('');
    Script.Add('$deletedCount = 0');
    Script.Add('$freedSpace = 0');
    Script.Add('');
    Script.Add('foreach ($file in $filesToDelete) {');
    Script.Add('    try {');
    Script.Add('        $size = $file.Length');
    Script.Add('        Remove-Item -Path $file.FullName -Force -Recurse -ErrorAction Stop');
    Script.Add('        $deletedCount++');
    Script.Add('        $freedSpace += $size');
    Script.Add('    } catch {');
    Script.Add('        # Fichier en cours d''utilisation');
    Script.Add('    }');
    Script.Add('}');
    Script.Add('');
    Script.Add('Write-Output "Fichiers supprimés: $deletedCount"');
    Script.Add('Write-Output "Espace libéré: $([math]::Round($freedSpace/1MB, 2)) MB"');

    Result := FPowerShell.Execute(Script.Text);

    if Result then
      LogMessage('Nettoyage terminé: ' + FPowerShell.Output.Text)
    else
      LogMessage('Erreur lors du nettoyage: ' + FPowerShell.Errors.Text);

  finally
    Script.Free;
  end;
end;

function TSystemMaintenance.CleanWindowsTemp: Boolean;
var
  Command: string;
begin
  LogMessage('Nettoyage du dossier Windows\Temp');

  Command :=
    '# Nécessite des privilèges administrateur' + sLineBreak +
    '$winTemp = "$env:windir\Temp"' + sLineBreak +
    'if (Test-Path $winTemp) {' + sLineBreak +
    '    Get-ChildItem -Path $winTemp -Recurse -ErrorAction SilentlyContinue | ' + sLineBreak +
    '    Where-Object { $_.LastWriteTime -lt (Get-Date).AddDays(-7) } | ' + sLineBreak +
    '    Remove-Item -Force -Recurse -ErrorAction SilentlyContinue' + sLineBreak +
    '}';

  Result := FPowerShell.Execute(Command);
end;

function TSystemMaintenance.CleanBrowserCache: Boolean;
var
  Script: TStringList;
begin
  LogMessage('Nettoyage du cache des navigateurs');

  Script := TStringList.Create;
  try
    // Chrome
    Script.Add('# Nettoyage Chrome');
    Script.Add('$chromePath = "$env:LOCALAPPDATA\Google\Chrome\User Data\Default\Cache"');
    Script.Add('if (Test-Path $chromePath) {');
    Script.Add('    Remove-Item -Path "$chromePath\*" -Force -ErrorAction SilentlyContinue');
    Script.Add('}');
    Script.Add('');

    // Firefox
    Script.Add('# Nettoyage Firefox');
    Script.Add('$firefoxPath = "$env:LOCALAPPDATA\Mozilla\Firefox\Profiles"');
    Script.Add('if (Test-Path $firefoxPath) {');
    Script.Add('    Get-ChildItem -Path $firefoxPath -Directory | ForEach-Object {');
    Script.Add('        $cachePath = Join-Path $_.FullName "cache2"');
    Script.Add('        if (Test-Path $cachePath) {');
    Script.Add('            Remove-Item -Path "$cachePath\*" -Recurse -Force -ErrorAction SilentlyContinue');
    Script.Add('        }');
    Script.Add('    }');
    Script.Add('}');
    Script.Add('');

    // Edge
    Script.Add('# Nettoyage Edge');
    Script.Add('$edgePath = "$env:LOCALAPPDATA\Microsoft\Edge\User Data\Default\Cache"');
    Script.Add('if (Test-Path $edgePath) {');
    Script.Add('    Remove-Item -Path "$edgePath\*" -Force -ErrorAction SilentlyContinue');
    Script.Add('}');

    Result := FPowerShell.Execute(Script.Text);

  finally
    Script.Free;
  end;
end;

function TSystemMaintenance.CleanRecycleBin: Boolean;
var
  Command: string;
begin
  LogMessage('Vidage de la corbeille');

  Command := 'Clear-RecycleBin -Force -ErrorAction SilentlyContinue';
  Result := FPowerShell.Execute(Command);
end;

function TSystemMaintenance.CleanOldLogs(DaysToKeep: Integer): Boolean;
var
  Command: string;
begin
  LogMessage(Format('Nettoyage des logs de plus de %d jours', [DaysToKeep]));

  Command := Format(
    '$date = (Get-Date).AddDays(-%d); ' +
    'Get-ChildItem -Path "$env:windir\Logs", "$env:windir\System32\LogFiles" ' +
    '-Include *.log, *.evt, *.evtx -Recurse -ErrorAction SilentlyContinue | ' +
    'Where-Object { $_.LastWriteTime -lt $date -and !$_.PSIsContainer } | ' +
    'Remove-Item -Force -ErrorAction SilentlyContinue',
    [DaysToKeep]
  );

  Result := FPowerShell.Execute(Command);
end;

function TSystemMaintenance.DefragmentDisk(const Drive: string): Boolean;
var
  Command: string;
begin
  LogMessage('Défragmentation du disque ' + Drive);

  Command := Format(
    'Optimize-Volume -DriveLetter %s -Defrag -Verbose',
    [Copy(Drive, 1, 1)]
  );

  Result := FPowerShell.Execute(Command);
end;

function TSystemMaintenance.CheckDisk(const Drive: string): Boolean;
var
  Command: string;
begin
  LogMessage('Vérification du disque ' + Drive);

  Command := Format(
    'Repair-Volume -DriveLetter %s -Scan',
    [Copy(Drive, 1, 1)]
  );

  Result := FPowerShell.Execute(Command);
end;

function TSystemMaintenance.OptimizeStartup: Boolean;
var
  Script: TStringList;
begin
  LogMessage('Optimisation du démarrage');

  Script := TStringList.Create;
  try
    Script.Add('# Désactiver les services inutiles au démarrage');
    Script.Add('$servicesToDisable = @(');
    Script.Add('    "Windows Search",  # Si non utilisé');
    Script.Add('    "Print Spooler",   # Si pas d''imprimante');
    Script.Add('    "Fax"              # Service de fax');
    Script.Add(')');
    Script.Add('');
    Script.Add('foreach ($service in $servicesToDisable) {');
    Script.Add('    $svc = Get-Service -Name $service -ErrorAction SilentlyContinue');
    Script.Add('    if ($svc -and $svc.StartType -ne "Disabled") {');
    Script.Add('        Set-Service -Name $service -StartupType Disabled');
    Script.Add('        Write-Output "Service désactivé: $service"');
    Script.Add('    }');
    Script.Add('}');

    Result := FPowerShell.Execute(Script.Text);

  finally
    Script.Free;
  end;
end;

function TSystemMaintenance.FlushDNSCache: Boolean;
var
  Command: string;
begin
  LogMessage('Vidage du cache DNS');

  Command := 'Clear-DnsClientCache';
  Result := FPowerShell.Execute(Command);
end;

function TSystemMaintenance.ResetNetworkStack: Boolean;
var
  Script: TStringList;
begin
  LogMessage('Réinitialisation de la pile réseau');

  Script := TStringList.Create;
  try
    Script.Add('# Réinitialisation complète du réseau');
    Script.Add('netsh winsock reset');
    Script.Add('netsh int ip reset');
    Script.Add('ipconfig /release');
    Script.Add('ipconfig /renew');
    Script.Add('ipconfig /flushdns');

    Result := FPowerShell.Execute(Script.Text);

  finally
    Script.Free;
  end;
end;

function TSystemMaintenance.RunMaintenance(Tasks: TMaintenanceTasks): Boolean;
begin
  Result := True;
  LogMessage('=== Début de la maintenance système ===');

  if mtCleanTemp in Tasks then
    Result := Result and CleanTempFiles;

  if mtCleanLogs in Tasks then
    Result := Result and CleanOldLogs;

  if mtDefrag in Tasks then
    Result := Result and DefragmentDisk;

  if mtCheckDisk in Tasks then
    Result := Result and CheckDisk;

  if mtOptimizeStartup in Tasks then
    Result := Result and OptimizeStartup;

  LogMessage('=== Fin de la maintenance système ===');
end;
```

## Application complète d'administration système

```pascal
program SystemAdmin;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils,
  PowerShellWrapper, ProcessManagement, ServiceManagement,
  RegistryManagement, FileManagement, SystemMaintenance;

type
  TSystemAdmin = class
  private
    FPowerShell: TPowerShell;
    FProcessManager: TProcessManager;
    FServiceManager: TServiceManager;
    FRegistryManager: TRegistryManager;
    FFileManager: TFileManager;
    FMaintenance: TSystemMaintenance;

    procedure ShowMenu;
    procedure ProcessMenu;
    procedure ServiceMenu;
    procedure FileMenu;
    procedure MaintenanceMenu;
    procedure SystemInfoMenu;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Run;
  end;

constructor TSystemAdmin.Create;
begin
  WriteLn('Initialisation des modules...');

  FPowerShell := TPowerShell.Create;
  FProcessManager := TProcessManager.Create;
  FServiceManager := TServiceManager.Create;
  FRegistryManager := TRegistryManager.Create;
  FFileManager := TFileManager.Create;
  FMaintenance := TSystemMaintenance.Create;

  WriteLn('Modules chargés avec succès.');
  WriteLn;
end;

destructor TSystemAdmin.Destroy;
begin
  FPowerShell.Free;
  FProcessManager.Free;
  FServiceManager.Free;
  FRegistryManager.Free;
  FFileManager.Free;
  FMaintenance.Free;
  inherited;
end;

procedure TSystemAdmin.ShowMenu;
begin
  WriteLn('========================================');
  WriteLn('    ADMINISTRATION SYSTÈME WINDOWS');
  WriteLn('========================================');
  WriteLn;
  WriteLn('1. Gestion des processus');
  WriteLn('2. Gestion des services');
  WriteLn('3. Gestion des fichiers');
  WriteLn('4. Maintenance système');
  WriteLn('5. Informations système');
  WriteLn('6. Exécuter une commande PowerShell');
  WriteLn('0. Quitter');
  WriteLn;
  Write('Votre choix : ');
end;

procedure TSystemAdmin.ProcessMenu;
var
  Choice: string;
  ProcessName: string;
  ProcessList: TStringList;
  i: Integer;
begin
  repeat
    WriteLn;
    WriteLn('=== GESTION DES PROCESSUS ===');
    WriteLn('1. Lister les processus');
    WriteLn('2. Vérifier si un processus existe');
    WriteLn('3. Informations sur un processus');
    WriteLn('4. Terminer un processus');
    WriteLn('5. Démarrer un processus');
    WriteLn('6. Top 10 processus (mémoire)');
    WriteLn('0. Retour');
    Write('Choix : ');
    ReadLn(Choice);

    case Choice of
      '1':
        begin
          WriteLn('Liste des processus en cours :');
          ProcessList := TStringList.Create;
          try
            FProcessManager.ListTopProcessesByMemory(20, ProcessList);
            for i := 0 to ProcessList.Count - 1 do
              WriteLn('  ', ProcessList[i]);
          finally
            ProcessList.Free;
          end;
        end;

      '2':
        begin
          Write('Nom du processus : ');
          ReadLn(ProcessName);
          if FProcessManager.IsProcessRunning(ProcessName) then
            WriteLn('Le processus ', ProcessName, ' est en cours d''exécution')
          else
            WriteLn('Le processus ', ProcessName, ' n''est pas trouvé');
        end;

      '3':
        begin
          Write('Nom du processus : ');
          ReadLn(ProcessName);
          WriteLn(FProcessManager.GetProcessInfo(ProcessName));
        end;

      '4':
        begin
          Write('Nom du processus à terminer : ');
          ReadLn(ProcessName);
          if FProcessManager.KillProcess(ProcessName) then
            WriteLn('Processus terminé')
          else
            WriteLn('Impossible de terminer le processus');
        end;

      '5':
        begin
          Write('Chemin du programme : ');
          ReadLn(ProcessName);
          if FProcessManager.StartProcess(ProcessName) then
            WriteLn('Processus démarré')
          else
            WriteLn('Impossible de démarrer le processus');
        end;

      '6':
        begin
          ProcessList := TStringList.Create;
          try
            FProcessManager.ListTopProcessesByMemory(10, ProcessList);
            WriteLn('Top 10 processus par utilisation mémoire :');
            for i := 0 to ProcessList.Count - 1 do
              WriteLn('  ', i + 1, '. ', ProcessList[i]);
          finally
            ProcessList.Free;
          end;
        end;
    end;

  until Choice = '0';
end;

procedure TSystemAdmin.ServiceMenu;
var
  Choice: string;
  ServiceName: string;
  ServiceList: TStringList;
  i: Integer;
  Status: TServiceStatus;
begin
  repeat
    WriteLn;
    WriteLn('=== GESTION DES SERVICES ===');
    WriteLn('1. Lister tous les services');
    WriteLn('2. Lister les services en cours');
    WriteLn('3. État d''un service');
    WriteLn('4. Démarrer un service');
    WriteLn('5. Arrêter un service');
    WriteLn('6. Redémarrer un service');
    WriteLn('0. Retour');
    Write('Choix : ');
    ReadLn(Choice);

    case Choice of
      '1':
        begin
          ServiceList := TStringList.Create;
          try
            FServiceManager.GetServiceList(ServiceList, False);
            WriteLn('Services installés : ', ServiceList.Count);
            for i := 0 to Min(20, ServiceList.Count - 1) do
              WriteLn('  ', ServiceList[i]);
            if ServiceList.Count > 20 then
              WriteLn('  ... et ', ServiceList.Count - 20, ' autres');
          finally
            ServiceList.Free;
          end;
        end;

      '2':
        begin
          ServiceList := TStringList.Create;
          try
            FServiceManager.GetServiceList(ServiceList, True);
            WriteLn('Services en cours d''exécution : ', ServiceList.Count);
            for i := 0 to Min(15, ServiceList.Count - 1) do
              WriteLn('  ', ServiceList[i]);
          finally
            ServiceList.Free;
          end;
        end;

      '3':
        begin
          Write('Nom du service : ');
          ReadLn(ServiceName);
          Status := FServiceManager.GetServiceStatus(ServiceName);
          case Status of
            ssRunning: WriteLn('État : En cours d''exécution');
            ssStopped: WriteLn('État : Arrêté');
            ssPaused: WriteLn('État : En pause');
            ssStartPending: WriteLn('État : Démarrage en cours');
            ssStopPending: WriteLn('État : Arrêt en cours');
            else WriteLn('État : Inconnu');
          end;
        end;

      '4':
        begin
          Write('Nom du service à démarrer : ');
          ReadLn(ServiceName);
          if FServiceManager.StartService(ServiceName) then
            WriteLn('Service démarré')
          else
            WriteLn('Impossible de démarrer le service');
        end;

      '5':
        begin
          Write('Nom du service à arrêter : ');
          ReadLn(ServiceName);
          if FServiceManager.StopService(ServiceName) then
            WriteLn('Service arrêté')
          else
            WriteLn('Impossible d''arrêter le service');
        end;

      '6':
        begin
          Write('Nom du service à redémarrer : ');
          ReadLn(ServiceName);
          if FServiceManager.RestartService(ServiceName) then
            WriteLn('Service redémarré')
          else
            WriteLn('Impossible de redémarrer le service');
        end;
    end;

  until Choice = '0';
end;

procedure TSystemAdmin.FileMenu;
var
  Choice: string;
  Path, Pattern, Destination: string;
  FileList: TStringList;
  i: Integer;
  Size: Int64;
begin
  repeat
    WriteLn;
    WriteLn('=== GESTION DES FICHIERS ===');
    WriteLn('1. Lister les fichiers');
    WriteLn('2. Copier un fichier');
    WriteLn('3. Déplacer un fichier');
    WriteLn('4. Supprimer un fichier');
    WriteLn('5. Créer un dossier');
    WriteLn('6. Calculer la taille d''un dossier');
    WriteLn('7. Rechercher les gros fichiers');
    WriteLn('8. Compresser des fichiers');
    WriteLn('9. Hash d''un fichier');
    WriteLn('0. Retour');
    Write('Choix : ');
    ReadLn(Choice);

    case Choice of
      '1':
        begin
          Write('Chemin du dossier : ');
          ReadLn(Path);
          Write('Filtre (ex: *.txt) : ');
          ReadLn(Pattern);
          if Pattern = '' then Pattern := '*.*';

          FileList := TStringList.Create;
          try
            FFileManager.ListFiles(Path, Pattern, FileList, False);
            WriteLn('Fichiers trouvés : ', FileList.Count);
            for i := 0 to Min(20, FileList.Count - 1) do
              WriteLn('  ', FileList[i]);
          finally
            FileList.Free;
          end;
        end;

      '2':
        begin
          Write('Fichier source : ');
          ReadLn(Path);
          Write('Destination : ');
          ReadLn(Destination);
          if FFileManager.CopyFile(Path, Destination, True) then
            WriteLn('Fichier copié')
          else
            WriteLn('Erreur lors de la copie');
        end;

      '3':
        begin
          Write('Fichier source : ');
          ReadLn(Path);
          Write('Destination : ');
          ReadLn(Destination);
          if FFileManager.MoveFile(Path, Destination) then
            WriteLn('Fichier déplacé')
          else
            WriteLn('Erreur lors du déplacement');
        end;

      '4':
        begin
          Write('Fichier à supprimer : ');
          ReadLn(Path);
          Write('Confirmer la suppression (O/N) : ');
          ReadLn(Choice);
          if UpperCase(Choice) = 'O' then
          begin
            if FFileManager.DeleteFile(Path, True) then
              WriteLn('Fichier supprimé')
            else
              WriteLn('Erreur lors de la suppression');
          end;
        end;

      '5':
        begin
          Write('Chemin du nouveau dossier : ');
          ReadLn(Path);
          if FFileManager.CreateDirectory(Path) then
            WriteLn('Dossier créé')
          else
            WriteLn('Erreur lors de la création');
        end;

      '6':
        begin
          Write('Chemin du dossier : ');
          ReadLn(Path);
          Size := FFileManager.GetDiskUsage(Path);
          WriteLn('Taille totale : ', Size div (1024*1024), ' MB');
        end;

      '7':
        begin
          Write('Chemin de recherche : ');
          ReadLn(Path);
          FileList := TStringList.Create;
          try
            FFileManager.FindLargeFiles(Path, 100, FileList); // > 100 MB
            WriteLn('Fichiers de plus de 100 MB :');
            for i := 0 to Min(10, FileList.Count - 1) do
              WriteLn('  ', FileList[i]);
          finally
            FileList.Free;
          end;
        end;

      '8':
        begin
          Write('Fichiers/Dossier à compresser : ');
          ReadLn(Path);
          Write('Nom de l''archive (avec .zip) : ');
          ReadLn(Destination);
          if FFileManager.CompressFiles(Path, Destination) then
            WriteLn('Archive créée')
          else
            WriteLn('Erreur lors de la compression');
        end;

      '9':
        begin
          Write('Fichier : ');
          ReadLn(Path);
          WriteLn('SHA256 : ', FFileManager.GetFileHash(Path, 'SHA256'));
        end;
    end;

  until Choice = '0';
end;

procedure TSystemAdmin.MaintenanceMenu;
var
  Choice: string;
  Tasks: TMaintenanceTasks;
begin
  repeat
    WriteLn;
    WriteLn('=== MAINTENANCE SYSTÈME ===');
    WriteLn('1. Nettoyer les fichiers temporaires');
    WriteLn('2. Nettoyer le cache des navigateurs');
    WriteLn('3. Vider la corbeille');
    WriteLn('4. Nettoyer les vieux logs');
    WriteLn('5. Vider le cache DNS');
    WriteLn('6. Réinitialiser la pile réseau');
    WriteLn('7. Maintenance complète');
    WriteLn('0. Retour');
    Write('Choix : ');
    ReadLn(Choice);

    case Choice of
      '1':
        begin
          Write('Nettoyage en cours...');
          if FMaintenance.CleanTempFiles then
            WriteLn(' Terminé')
          else
            WriteLn(' Erreur');
        end;

      '2':
        begin
          Write('Nettoyage du cache...');
          if FMaintenance.CleanBrowserCache then
            WriteLn(' Terminé')
          else
            WriteLn(' Erreur');
        end;

      '3':
        begin
          Write('Vidage de la corbeille...');
          if FMaintenance.CleanRecycleBin then
            WriteLn(' Terminé')
          else
            WriteLn(' Erreur');
        end;

      '4':
        begin
          Write('Nettoyage des logs...');
          if FMaintenance.CleanOldLogs(30) then
            WriteLn(' Terminé')
          else
            WriteLn(' Erreur');
        end;

      '5':
        begin
          Write('Vidage du cache DNS...');
          if FMaintenance.FlushDNSCache then
            WriteLn(' Terminé')
          else
            WriteLn(' Erreur');
        end;

      '6':
        begin
          WriteLn('Réinitialisation réseau (nécessite redémarrage)...');
          if FMaintenance.ResetNetworkStack then
            WriteLn('Terminé - Veuillez redémarrer')
          else
            WriteLn('Erreur');
        end;

      '7':
        begin
          WriteLn('Maintenance complète en cours...');
          Tasks := [mtCleanTemp, mtCleanLogs];
          if FMaintenance.RunMaintenance(Tasks) then
            WriteLn('Maintenance terminée')
          else
            WriteLn('Certaines tâches ont échoué');
        end;
    end;

  until Choice = '0';
end;

procedure TSystemAdmin.SystemInfoMenu;
var
  Output: string;
  Command: string;
begin
  WriteLn;
  WriteLn('=== INFORMATIONS SYSTÈME ===');
  WriteLn;

  // Version Windows
  Command := '(Get-CimInstance Win32_OperatingSystem).Caption';
  Output := FPowerShell.ExecuteAndGetOutput(Command);
  WriteLn('Système : ', Trim(Output));

  // Architecture
  Command := '(Get-CimInstance Win32_OperatingSystem).OSArchitecture';
  Output := FPowerShell.ExecuteAndGetOutput(Command);
  WriteLn('Architecture : ', Trim(Output));

  // Processeur
  Command := '(Get-CimInstance Win32_Processor).Name';
  Output := FPowerShell.ExecuteAndGetOutput(Command);
  WriteLn('Processeur : ', Trim(Output));

  // Mémoire
  Command := '[math]::Round((Get-CimInstance Win32_OperatingSystem).TotalVisibleMemorySize/1MB, 2)';
  Output := FPowerShell.ExecuteAndGetOutput(Command);
  WriteLn('RAM totale : ', Trim(Output), ' GB');

  Command := '[math]::Round((Get-CimInstance Win32_OperatingSystem).FreePhysicalMemory/1MB, 2)';
  Output := FPowerShell.ExecuteAndGetOutput(Command);
  WriteLn('RAM disponible : ', Trim(Output), ' GB');

  // Disques
  WriteLn;
  WriteLn('Disques :');
  Command := 'Get-PSDrive -PSProvider FileSystem | Where-Object {$_.Used -gt 0} | ' +
             'ForEach-Object { "$($_.Name): $([math]::Round($_.Used/1GB, 2)) GB utilisés sur $([math]::Round(($_.Used+$_.Free)/1GB, 2)) GB" }';
  FPowerShell.Execute(Command);
  WriteLn(FPowerShell.Output.Text);

  WriteLn;
  Write('Appuyez sur Entrée pour continuer...');
  ReadLn;
end;

procedure TSystemAdmin.Run;
var
  Choice: string;
  Command: string;
begin
  repeat
    ShowMenu;
    ReadLn(Choice);

    case Choice of
      '1': ProcessMenu;
      '2': ServiceMenu;
      '3': FileMenu;
      '4': MaintenanceMenu;
      '5': SystemInfoMenu;

      '6':
        begin
          WriteLn;
          WriteLn('Entrez la commande PowerShell (ou "exit" pour revenir) :');
          Write('PS> ');
          ReadLn(Command);

          if LowerCase(Command) <> 'exit' then
          begin
            if FPowerShell.Execute(Command) then
            begin
              WriteLn('Résultat :');
              WriteLn(FPowerShell.Output.Text);
            end
            else
            begin
              WriteLn('Erreur :');
              WriteLn(FPowerShell.Errors.Text);
            end;
          end;
        end;

      '0':
        begin
          WriteLn('Au revoir !');
        end;

      else
        WriteLn('Choix invalide');
    end;

  until Choice = '0';
end;

// Programme principal
var
  Admin: TSystemAdmin;
begin
  WriteLn('========================================');
  WriteLn('   Administration Système FreePascal');
  WriteLn('          avec PowerShell');
  WriteLn('========================================');
  WriteLn;

  // Vérifier les privilèges administrateur
  if not IsUserAdmin then
  begin
    WriteLn('ATTENTION : Certaines fonctions nécessitent');
    WriteLn('des privilèges administrateur.');
    WriteLn;
  end;

  try
    Admin := TSystemAdmin.Create;
    try
      Admin.Run;
    finally
      Admin.Free;
    end;
  except
    on E: Exception do
    begin
      WriteLn('Erreur fatale : ', E.Message);
      WriteLn('Appuyez sur Entrée pour quitter...');
      ReadLn;
    end;
  end;
end.

// Fonction helper pour vérifier les droits admin
function IsUserAdmin: Boolean;
var
  PS: TPowerShell;
  Output: string;
begin
  PS := TPowerShell.Create;
  try
    Output := PS.ExecuteAndGetOutput(
      '([Security.Principal.WindowsPrincipal] ' +
      '[Security.Principal.WindowsIdentity]::GetCurrent())' +
      '.IsInRole([Security.Principal.WindowsBuiltInRole] "Administrator")'
    );
    Result := Trim(Output) = 'True';
  finally
    PS.Free;
  end;
end;
```

## Scripts PowerShell utiles préfabriqués

### Collection de scripts prêts à l'emploi

```pascal
unit PowerShellScripts;

interface

const
  // Script de rapport système complet
  SCRIPT_SYSTEM_REPORT =
    '$report = @{}' + #13#10 +
    '$report.ComputerName = $env:COMPUTERNAME' + #13#10 +
    '$report.OS = (Get-CimInstance Win32_OperatingSystem).Caption' + #13#10 +
    '$report.LastBoot = (Get-CimInstance Win32_OperatingSystem).LastBootUpTime' + #13#10 +
    '$report.CPU = (Get-CimInstance Win32_Processor).Name' + #13#10 +
    '$report.RAM = [math]::Round((Get-CimInstance Win32_OperatingSystem).TotalVisibleMemorySize/1MB, 2)' + #13#10 +
    '$report.DiskUsage = Get-PSDrive -PSProvider FileSystem | Where-Object {$_.Used -gt 0} | ' +
    'Select-Object Name, @{N="UsedGB";E={[math]::Round($_.Used/1GB,2)}}, @{N="FreeGB";E={[math]::Round($_.Free/1GB,2)}}' + #13#10 +
    '$report | ConvertTo-Json -Depth 3';

  // Script de sauvegarde
  SCRIPT_BACKUP =
    'param($Source, $Destination)' + #13#10 +
    '$timestamp = Get-Date -Format "yyyyMMdd_HHmmss"' + #13#10 +
    '$backupPath = "$Destination\Backup_$timestamp"' + #13#10 +
    'New-Item -ItemType Directory -Path $backupPath -Force | Out-Null' + #13#10 +
    'robocopy $Source $backupPath /E /MT:8 /R:3 /W:10 /NP /LOG:"$backupPath\backup.log"' + #13#10 +
    'if ($LASTEXITCODE -le 7) { Write-Output "Backup successful" } else { Write-Error "Backup failed" }';

  // Script de monitoring
  SCRIPT_MONITOR_RESOURCES =
    'while ($true) {' + #13#10 +
    '    Clear-Host' + #13#10 +
    '    Write-Host "=== Monitoring Système ===" -ForegroundColor Cyan' + #13#10 +
    '    Write-Host "Date: $(Get-Date -Format ''yyyy-MM-dd HH:mm:ss'')"' + #13#10 +
    '    Write-Host ""' + #13#10 +
    '    ' + #13#10 +
    '    # CPU' + #13#10 +
    '    $cpu = (Get-Counter "\Processor(_Total)\% Processor Time").CounterSamples.CookedValue' + #13#10 +
    '    Write-Host "CPU: $([math]::Round($cpu, 2))%"' + #13#10 +
    '    ' + #13#10 +
    '    # Mémoire' + #13#10 +
    '    $os = Get-CimInstance Win32_OperatingSystem' + #13#10 +
    '    $memUsed = [math]::Round(($os.TotalVisibleMemorySize - $os.FreePhysicalMemory) / 1MB, 2)' + #13#10 +
    '    $memTotal = [math]::Round($os.TotalVisibleMemorySize / 1MB, 2)' + #13#10 +
    '    Write-Host "RAM: $memUsed GB / $memTotal GB"' + #13#10 +
    '    ' + #13#10 +
    '    # Top processus' + #13#10 +
    '    Write-Host ""' + #13#10 +
    '    Write-Host "Top 5 processus (CPU):"' + #13#10 +
    '    Get-Process | Sort-Object CPU -Descending | Select-Object -First 5 | ' +
    '    ForEach-Object { Write-Host "  $($_.ProcessName): $([math]::Round($_.CPU, 2))s" }' + #13#10 +
    '    ' + #13#10 +
    '    Start-Sleep -Seconds 5' + #13#10 +
    '}';

  // Script de sécurité
  SCRIPT_SECURITY_CHECK =
    '# Vérifications de sécurité' + #13#10 +
    '$results = @()' + #13#10 +
    '' + #13#10 +
    '# Windows Defender' + #13#10 +
    '$defender = Get-MpComputerStatus' + #13#10 +
    '$results += [PSCustomObject]@{' + #13#10 +
    '    Check = "Windows Defender"' + #13#10 +
    '    Status = if ($defender.RealTimeProtectionEnabled) {"OK"} else {"WARNING"}' + #13#10 +
    '    Details = "RealTime: $($defender.RealTimeProtectionEnabled)"' + #13#10 +
    '}' + #13#10 +
    '' + #13#10 +
    '# Firewall' + #13#10 +
    '$firewall = Get-NetFirewallProfile | Where-Object {$_.Name -eq "Domain"}' + #13#10 +
    '$results += [PSCustomObject]@{' + #13#10 +
    '    Check = "Firewall"' + #13#10 +
    '    Status = if ($firewall.Enabled) {"OK"} else {"WARNING"}' + #13#10 +
    '    Details = "Domain: $($firewall.Enabled)"' + #13#10 +
    '}' + #13#10 +
    '' + #13#10 +
    '# Updates' + #13#10 +
    '$lastUpdate = (Get-HotFix | Sort-Object InstalledOn -Descending | Select-Object -First 1).InstalledOn' + #13#10 +
    '$daysSinceUpdate = (Get-Date) - $lastUpdate' + #13#10 +
    '$results += [PSCustomObject]@{' + #13#10 +
    '    Check = "Windows Update"' + #13#10 +
    '    Status = if ($daysSinceUpdate.Days -lt 30) {"OK"} else {"WARNING"}' + #13#10 +
    '    Details = "Last update: $($daysSinceUpdate.Days) days ago"' + #13#10 +
    '}' + #13#10 +
    '' + #13#10 +
    '$results | Format-Table -AutoSize';

implementation

end.
```

## Sécurité et bonnes pratiques

### Exécution sécurisée de scripts

```pascal
unit SecurePowerShell;

interface

uses
  Classes, SysUtils, PowerShellWrapper;

type
  TSecurePowerShell = class(TPowerShell)
  private
    FAllowedCommands: TStringList;
    FBlockedCommands: TStringList;

    function ValidateCommand(const Command: string): Boolean;
    function SanitizeInput(const Input: string): string;
  public
    constructor Create;
    destructor Destroy; override;

    function ExecuteSecure(const Command: string): Boolean;
    procedure AddAllowedCommand(const Command: string);
    procedure AddBlockedCommand(const Command: string);
  end;

implementation

constructor TSecurePowerShell.Create;
begin
  inherited Create;

  FAllowedCommands := TStringList.Create;
  FBlockedCommands := TStringList.Create;

  // Commandes dangereuses à bloquer par défaut
  FBlockedCommands.Add('Remove-Item -Recurse -Force C:\');
  FBlockedCommands.Add('Format-');
  FBlockedCommands.Add('Clear-Disk');
  FBlockedCommands.Add('Remove-Partition');
  FBlockedCommands.Add('Stop-Computer');
  FBlockedCommands.Add('Restart-Computer');

  // Cmdlets potentiellement dangereux
  FBlockedCommands.Add('Set-ExecutionPolicy Unrestricted');
  FBlockedCommands.Add('Disable-WindowsDefender');
  FBlockedCommands.Add('Stop-Service WinDefend');
end;

destructor TSecurePowerShell.Destroy;
begin
  FAllowedCommands.Free;
  FBlockedCommands.Free;
  inherited;
end;

function TSecurePowerShell.ValidateCommand(const Command: string): Boolean;
var
  i: Integer;
  LowerCommand: string;
begin
  Result := True;
  LowerCommand := LowerCase(Command);

  // Vérifier les commandes bloquées
  for i := 0 to FBlockedCommands.Count - 1 do
  begin
    if Pos(LowerCase(FBlockedCommands[i]), LowerCommand) > 0 then
    begin
      Result := False;
      Exit;
    end;
  end;

  // Si liste blanche définie, vérifier
  if FAllowedCommands.Count > 0 then
  begin
    Result := False;
    for i := 0 to FAllowedCommands.Count - 1 do
    begin
      if Pos(LowerCase(FAllowedCommands[i]), LowerCommand) > 0 then
      begin
        Result := True;
        Break;
      end;
    end;
  end;
end;

function TSecurePowerShell.SanitizeInput(const Input: string): string;
begin
  Result := Input;

  // Échapper les caractères dangereux
  Result := StringReplace(Result, ';', '`;', [rfReplaceAll]);
  Result := StringReplace(Result, '|', '`|', [rfReplaceAll]);
  Result := StringReplace(Result, '&', '`&', [rfReplaceAll]);

  // Empêcher l'injection de commandes
  Result := StringReplace(Result, '$', '`$', [rfReplaceAll]);
  Result := StringReplace(Result, '`n', '', [rfReplaceAll]);
  Result := StringReplace(Result, '`r', '', [rfReplaceAll]);

  // Limiter la longueur pour éviter les buffer overflows
  if Length(Result) > 1000 then
    Result := Copy(Result, 1, 1000);
end;

function TSecurePowerShell.ExecuteSecure(const Command: string): Boolean;
var
  SanitizedCommand: string;
begin
  // Valider la commande
  if not ValidateCommand(Command) then
  begin
    FErrorBuffer.Add('Commande bloquée pour des raisons de sécurité');
    Result := False;
    Exit;
  end;

  // Nettoyer les entrées
  SanitizedCommand := SanitizeInput(Command);

  // Exécuter avec restrictions
  Result := inherited Execute(
    '-ExecutionPolicy Restricted -NoProfile ' +
    '-Command "' + SanitizedCommand + '"'
  );
end;

procedure TSecurePowerShell.AddAllowedCommand(const Command: string);
begin
  FAllowedCommands.Add(Command);
end;

procedure TSecurePowerShell.AddBlockedCommand(const Command: string);
begin
  FBlockedCommands.Add(Command);
end;
```

### Gestion des droits et élévation

```pascal
unit PowerShellElevation;

interface

uses
  Windows, SysUtils, ShellAPI, PowerShellWrapper;

type
  TElevatedPowerShell = class
  private
    FIsElevated: Boolean;

    function CheckElevation: Boolean;
  public
    constructor Create;

    function RunElevated(const Script: string): Boolean;
    function RunAsUser(const Script: string; const Username, Password: string): Boolean;

    property IsElevated: Boolean read FIsElevated;
  end;

implementation

constructor TElevatedPowerShell.Create;
begin
  FIsElevated := CheckElevation;
end;

function TElevatedPowerShell.CheckElevation: Boolean;
var
  TokenHandle: THandle;
  Elevation: TOKEN_ELEVATION;
  ReturnLength: DWORD;
begin
  Result := False;

  if OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, TokenHandle) then
  begin
    try
      if GetTokenInformation(TokenHandle, TokenElevation, @Elevation,
                            SizeOf(Elevation), ReturnLength) then
        Result := Elevation.TokenIsElevated <> 0;
    finally
      CloseHandle(TokenHandle);
    end;
  end;
end;

function TElevatedPowerShell.RunElevated(const Script: string): Boolean;
var
  ScriptFile: string;
  ScriptContent: TStringList;
  ShellInfo: TShellExecuteInfo;
  PS: TPowerShell;
begin
  Result := False;

  // Si déjà élevé, exécuter directement
  if FIsElevated then
  begin
    PS := TPowerShell.Create;
    try
      Result := PS.ExecuteScript(Script);
    finally
      PS.Free;
    end;
    Exit;
  end;

  // Créer un fichier script temporaire
  ScriptFile := GetEnvironmentVariable('TEMP') + '\temp_elevated.ps1';

  ScriptContent := TStringList.Create;
  try
    ScriptContent.Text := Script;
    ScriptContent.SaveToFile(ScriptFile);
  finally
    ScriptContent.Free;
  end;

  // Exécuter avec élévation
  FillChar(ShellInfo, SizeOf(ShellInfo), 0);
  ShellInfo.cbSize := SizeOf(TShellExecuteInfo);
  ShellInfo.fMask := SEE_MASK_NOCLOSEPROCESS;
  ShellInfo.lpVerb := 'runas';
  ShellInfo.lpFile := 'powershell.exe';
  ShellInfo.lpParameters := PChar('-ExecutionPolicy Bypass -File "' + ScriptFile + '"');
  ShellInfo.nShow := SW_HIDE;

  if ShellExecuteEx(@ShellInfo) then
  begin
    WaitForSingleObject(ShellInfo.hProcess, INFINITE);
    CloseHandle(ShellInfo.hProcess);
    Result := True;
  end;

  // Nettoyer
  DeleteFile(ScriptFile);
end;

function TElevatedPowerShell.RunAsUser(const Script: string;
                                       const Username, Password: string): Boolean;
var
  Command: string;
  PS: TPowerShell;
begin
  // Créer un credential sécurisé
  Command := Format(
    '$secpasswd = ConvertTo-SecureString "%s" -AsPlainText -Force; ' +
    '$cred = New-Object System.Management.Automation.PSCredential ("%s", $secpasswd); ' +
    'Start-Process powershell.exe -Credential $cred -ArgumentList ''-Command %s''',
    [Password, Username, Script]
  );

  PS := TPowerShell.Create;
  try
    Result := PS.Execute(Command);
  finally
    PS.Free;
  end;
end;
```

## Intégration avec d'autres technologies

### Interaction avec WMI

```pascal
unit WMIPowerShell;

interface

uses
  Classes, SysUtils, PowerShellWrapper;

type
  TWMIQuery = class
  private
    FPowerShell: TPowerShell;
  public
    constructor Create;
    destructor Destroy; override;

    function GetWMIProperty(const ClassName, PropertyName: string): string;
    function GetBIOSInfo: string;
    function GetNetworkAdapters: string;
    function GetInstalledSoftware: string;
    function GetEventLogs(const LogName: string; Count: Integer = 10): string;
  end;

implementation

constructor TWMIQuery.Create;
begin
  FPowerShell := TPowerShell.Create;
end;

destructor TWMIQuery.Destroy;
begin
  FPowerShell.Free;
  inherited;
end;

function TWMIQuery.GetWMIProperty(const ClassName, PropertyName: string): string;
var
  Command: string;
begin
  Command := Format('(Get-WmiObject -Class %s).%s', [ClassName, PropertyName]);
  Result := FPowerShell.ExecuteAndGetOutput(Command);
end;

function TWMIQuery.GetBIOSInfo: string;
var
  Command: string;
begin
  Command := 'Get-WmiObject -Class Win32_BIOS | ' +
             'Select-Object Manufacturer, Name, Version, SerialNumber | ' +
             'Format-List';
  Result := FPowerShell.ExecuteAndGetOutput(Command);
end;

function TWMIQuery.GetNetworkAdapters: string;
var
  Command: string;
begin
  Command := 'Get-WmiObject -Class Win32_NetworkAdapter | ' +
             'Where-Object { $_.NetEnabled -eq $true } | ' +
             'Select-Object Name, MACAddress, Speed | ' +
             'Format-Table -AutoSize';
  Result := FPowerShell.ExecuteAndGetOutput(Command);
end;

function TWMIQuery.GetInstalledSoftware: string;
var
  Command: string;
begin
  Command := 'Get-WmiObject -Class Win32_Product | ' +
             'Select-Object Name, Version, Vendor, InstallDate | ' +
             'Sort-Object Name | ' +
             'Format-Table -AutoSize';
  Result := FPowerShell.ExecuteAndGetOutput(Command);
end;

function TWMIQuery.GetEventLogs(const LogName: string; Count: Integer): string;
var
  Command: string;
begin
  Command := Format(
    'Get-EventLog -LogName %s -Newest %d | ' +
    'Select-Object TimeGenerated, EntryType, Source, Message | ' +
    'Format-Table -AutoSize',
    [LogName, Count]
  );
  Result := FPowerShell.ExecuteAndGetOutput(Command);
end;
```

### Interaction avec Active Directory

```pascal
unit ADPowerShell;

interface

uses
  Classes, SysUtils, PowerShellWrapper;

type
  TActiveDirectory = class
  private
    FPowerShell: TPowerShell;
    FDomain: string;
  public
    constructor Create(const Domain: string = '');
    destructor Destroy; override;

    function GetUser(const Username: string): string;
    function GetUserGroups(const Username: string): string;
    function GetComputerInfo(const ComputerName: string): string;
    function IsUserInGroup(const Username, GroupName: string): Boolean;
    procedure DisableUser(const Username: string);
    procedure EnableUser(const Username: string);
    procedure ResetPassword(const Username, NewPassword: string);
  end;

implementation

constructor TActiveDirectory.Create(const Domain: string);
begin
  FPowerShell := TPowerShell.Create;
  FDomain := Domain;

  // Vérifier que le module AD est disponible
  FPowerShell.Execute('Import-Module ActiveDirectory -ErrorAction SilentlyContinue');
end;

destructor TActiveDirectory.Destroy;
begin
  FPowerShell.Free;
  inherited;
end;

function TActiveDirectory.GetUser(const Username: string): string;
var
  Command: string;
begin
  Command := Format('Get-ADUser -Identity %s -Properties * | Format-List', [Username]);
  Result := FPowerShell.ExecuteAndGetOutput(Command);
end;

function TActiveDirectory.GetUserGroups(const Username: string): string;
var
  Command: string;
begin
  Command := Format(
    'Get-ADPrincipalGroupMembership -Identity %s | Select-Object Name',
    [Username]
  );
  Result := FPowerShell.ExecuteAndGetOutput(Command);
end;

function TActiveDirectory.GetComputerInfo(const ComputerName: string): string;
var
  Command: string;
begin
  Command := Format(
    'Get-ADComputer -Identity %s -Properties * | Format-List',
    [ComputerName]
  );
  Result := FPowerShell.ExecuteAndGetOutput(Command);
end;

function TActiveDirectory.IsUserInGroup(const Username, GroupName: string): Boolean;
var
  Command, Output: string;
begin
  Command := Format(
    '(Get-ADGroupMember -Identity "%s" | Where-Object {$_.SamAccountName -eq "%s"}) -ne $null',
    [GroupName, Username]
  );
  Output := FPowerShell.ExecuteAndGetOutput(Command);
  Result := Trim(Output) = 'True';
end;

procedure TActiveDirectory.DisableUser(const Username: string);
begin
  FPowerShell.Execute(Format('Disable-ADAccount -Identity %s', [Username]));
end;

procedure TActiveDirectory.EnableUser(const Username: string);
begin
  FPowerShell.Execute(Format('Enable-ADAccount -Identity %s', [Username]));
end;

procedure TActiveDirectory.ResetPassword(const Username, NewPassword: string);
var
  Command: string;
begin
  Command := Format(
    '$pwd = ConvertTo-SecureString "%s" -AsPlainText -Force; ' +
    'Set-ADAccountPassword -Identity %s -NewPassword $pwd -Reset',
    [NewPassword, Username]
  );
  FPowerShell.Execute(Command);
end;
```

## Dépannage et résolution de problèmes

### Problèmes courants et solutions

```pascal
unit PowerShellTroubleshooting;

interface

type
  TPowerShellError = record
    ErrorCode: Integer;
    Description: string;
    Solution: string;
  end;

const
  CommonErrors: array[0..9] of TPowerShellError = (
    (ErrorCode: 0;
     Description: 'Commande introuvable';
     Solution: 'Vérifier l''orthographe de la cmdlet ou importer le module nécessaire'),

    (ErrorCode: 1;
     Description: 'Execution Policy bloque le script';
     Solution: 'Utiliser -ExecutionPolicy Bypass ou Set-ExecutionPolicy RemoteSigned'),

    (ErrorCode: 2;
     Description: 'Accès refusé';
     Solution: 'Exécuter avec des privilèges administrateur'),

    (ErrorCode: 3;
     Description: 'Module non trouvé';
     Solution: 'Installer le module avec Install-Module ou Import-Module'),

    (ErrorCode: 4;
     Description: 'Syntaxe incorrecte';
     Solution: 'Vérifier les guillemets, parenthèses et accolades'),

    (ErrorCode: 5;
     Description: 'Timeout dépassé';
     Solution: 'Augmenter le timeout ou optimiser la commande'),

    (ErrorCode: 6;
     Description: 'Mémoire insuffisante';
     Solution: 'Fermer des applications ou traiter les données par lots'),

    (ErrorCode: 7;
     Description: 'Version PowerShell incompatible';
     Solution: 'Mettre à jour PowerShell ou adapter le script'),

    (ErrorCode: 8;
     Description: 'Encodage de caractères';
     Solution: 'Utiliser UTF-8 ou spécifier l''encodage avec -Encoding'),

    (ErrorCode: 9;
     Description: 'Pipeline vide';
     Solution: 'Vérifier que la commande précédente retourne des données')
  );

function DiagnosePowerShellError(const ErrorMessage: string): string;
function GetPowerShellVersion: string;
function TestPowerShellConnectivity: Boolean;

implementation

uses
  SysUtils, PowerShellWrapper;

function DiagnosePowerShellError(const ErrorMessage: string): string;
var
  LowerError: string;
begin
  Result := 'Erreur inconnue';
  LowerError := LowerCase(ErrorMessage);

  if Pos('is not recognized', LowerError) > 0 then
    Result := CommonErrors[0].Solution
  else if Pos('execution policies', LowerError) > 0 then
    Result := CommonErrors[1].Solution
  else if Pos('access is denied', LowerError) > 0 then
    Result := CommonErrors[2].Solution
  else if Pos('module', LowerError) > 0 then
    Result := CommonErrors[3].Solution
  else if Pos('syntax', LowerError) > 0 then
    Result := CommonErrors[4].Solution
  else if Pos('timeout', LowerError) > 0 then
    Result := CommonErrors[5].Solution
  else if Pos('memory', LowerError) > 0 then
    Result := CommonErrors[6].Solution
  else if Pos('version', LowerError) > 0 then
    Result := CommonErrors[7].Solution
  else if Pos('encoding', LowerError) > 0 then
    Result := CommonErrors[8].Solution;
end;

function GetPowerShellVersion: string;
var
  PS: TPowerShell;
begin
  PS := TPowerShell.Create;
  try
    Result := PS.ExecuteAndGetOutput('$PSVersionTable.PSVersion.ToString()');
  finally
    PS.Free;
  end;
end;

function TestPowerShellConnectivity: Boolean;
var
  PS: TPowerShell;
  Output: string;
begin
  PS := TPowerShell.Create;
  try
    Output := PS.ExecuteAndGetOutput('Write-Output "Test"');
    Result := Trim(Output) = 'Test';
  finally
    PS.Free;
  end;
end;
```

## Optimisation et performance

### Conseils pour améliorer les performances

```pascal
unit PowerShellOptimization;

interface

uses
  Classes, SysUtils;

type
  TOptimizationTips = class
  public
    class function OptimizeScript(const Script: string): string;
    class function ConvertToParallelProcessing(const Script: string): string;
    class function AddProgressReporting(const Script: string): string;
  end;

implementation

class function TOptimizationTips.OptimizeScript(const Script: string): string;
var
  Lines: TStringList;
  i: Integer;
begin
  Lines := TStringList.Create;
  try
    Lines.Text := Script;

    for i := 0 to Lines.Count - 1 do
    begin
      // Remplacer Where-Object par filtres natifs quand possible
      if Pos('| Where-Object', Lines[i]) > 0 then
      begin
        // Exemple : Get-Process | Where-Object {$_.CPU -gt 10}
        // Devient : Get-Process | Where CPU -gt 10
        Lines[i] := StringReplace(Lines[i],
          '| Where-Object {$_.', '| Where ',
          [rfReplaceAll]);
        Lines[i] := StringReplace(Lines[i], '}', '', [rfReplaceAll]);
      end;

      // Utiliser -Filter au lieu de Where-Object pour Get-ChildItem
      if Pos('Get-ChildItem', Lines[i]) > 0 then
      begin
        if Pos('Where-Object', Lines[i]) > 0 then
        begin
          // Optimiser avec -Filter
          Lines[i] := StringReplace(Lines[i],
            '| Where-Object',
            '-Filter',
            []);
        end;
      end;

      // Éviter Select-Object *
      Lines[i] := StringReplace(Lines[i],
        'Select-Object *',
        'Select-Object -Property *',
        [rfReplaceAll]);
    end;

    Result := Lines.Text;

  finally
    Lines.Free;
  end;
end;

class function TOptimizationTips.ConvertToParallelProcessing(const Script: string): string;
begin
  // Convertir ForEach-Object en traitement parallèle
  Result := StringReplace(Script,
    'ForEach-Object {',
    'ForEach-Object -Parallel { $using:',
    [rfReplaceAll]);

  // Ajouter ThrottleLimit pour contrôler la parallélisation
  Result := Result + ' -ThrottleLimit 5';
end;

class function TOptimizationTips.AddProgressReporting(const Script: string): string;
var
  Lines: TStringList;
begin
  Lines := TStringList.Create;
  try
    Lines.Add('$stopwatch = [System.Diagnostics.Stopwatch]::StartNew()');
    Lines.Add('$progress = 0');
    Lines.Add('');
    Lines.Text := Lines.Text + Script;
    Lines.Add('');
    Lines.Add('$stopwatch.Stop()');
    Lines.Add('Write-Host "Temps d''exécution: $($stopwatch.Elapsed.TotalSeconds) secondes"');

    Result := Lines.Text;

  finally
    Lines.Free;
  end;
end;
```

## Tableau récapitulatif des cmdlets essentielles

| Catégorie | Cmdlet | Description | Exemple |
|-----------|--------|-------------|---------|
| **Processus** | Get-Process | Liste les processus | `Get-Process chrome` |
| | Stop-Process | Arrête un processus | `Stop-Process -Name notepad` |
| | Start-Process | Démarre un processus | `Start-Process calc.exe` |
| **Services** | Get-Service | Liste les services | `Get-Service win*` |
| | Start-Service | Démarre un service | `Start-Service Spooler` |
| | Stop-Service | Arrête un service | `Stop-Service Spooler` |
| **Fichiers** | Get-ChildItem | Liste les fichiers | `Get-ChildItem -Recurse` |
| | Copy-Item | Copie des fichiers | `Copy-Item a.txt b.txt` |
| | Remove-Item | Supprime des fichiers | `Remove-Item *.tmp` |
| **Registre** | Get-ItemProperty | Lit le registre | `Get-ItemProperty HKLM:\Software` |
| | Set-ItemProperty | Modifie le registre | `Set-ItemProperty -Name "Value"` |
| **Réseau** | Test-Connection | Ping | `Test-Connection google.com` |
| | Get-NetAdapter | Liste les cartes réseau | `Get-NetAdapter` |
| **Système** | Get-ComputerInfo | Infos système | `Get-ComputerInfo` |
| | Get-EventLog | Journaux d'événements | `Get-EventLog System` |

## Ressources et documentation

### Liens utiles

- **Documentation officielle** : https://docs.microsoft.com/powershell
- **PowerShell Gallery** : https://www.powershellgallery.com
- **GitHub PowerShell** : https://github.com/PowerShell/PowerShell

### Commandes d'aide intégrées

```powershell
# Aide sur une cmdlet
Get-Help Get-Process -Full

# Exemples d'utilisation
Get-Help Get-Process -Examples

# Aide en ligne
Get-Help Get-Process -Online

# Mettre à jour l'aide
Update-Help

# Rechercher des commandes
Get-Command *process*

# Voir les alias
Get-Alias
```

## Conclusion

PowerShell est un outil extrêmement puissant pour l'administration système sous Windows. Son intégration avec FreePascal/Lazarus ouvre de nombreuses possibilités pour créer des applications d'administration sophistiquées.

### Points clés à retenir

1. **PowerShell est orienté objet** : Contrairement à cmd.exe, PowerShell manipule des objets .NET
2. **Sécurité par défaut** : Les scripts sont bloqués par défaut, utilisez ExecutionPolicy avec précaution
3. **Intégration complète** : Accès à WMI, .NET, COM, et toutes les API Windows
4. **Automatisation puissante** : Idéal pour les tâches répétitives et la maintenance
5. **Cross-platform** : PowerShell Core fonctionne aussi sur Linux et macOS

### Avantages de l'intégration FreePascal/PowerShell

- **Interface graphique** : Créez des GUI pour vos scripts PowerShell
- **Compilation native** : Applications autonomes sans dépendances
- **Contrôle précis** : Validation et sécurisation des entrées
- **Performance** : Code natif FreePascal avec la puissance de PowerShell
- **Déploiement** : Distribution simplifiée de vos outils d'administration

### Pour aller plus loin

1. **Explorez PowerShell 7+** : Version cross-platform avec de nouvelles fonctionnalités
2. **Apprenez DSC** : Desired State Configuration pour la gestion de configuration
3. **Utilisez les modules** : Étendez PowerShell avec des modules spécialisés
4. **Créez vos cmdlets** : Développez vos propres cmdlets en C# ou F#
5. **Automatisez avec Azure** : Intégration avec Azure Automation et Azure Functions

Avec ces connaissances, vous êtes maintenant capable de créer des applications FreePascal/Lazarus puissantes qui exploitent pleinement les capacités de PowerShell pour l'administration système Windows. Cette combinaison offre le meilleur des deux mondes : la robustesse et la performance du code natif avec la flexibilité et la puissance des scripts PowerShell.

⏭️ [WMI (Windows Management Instrumentation)](/06-specificites-windows/10-wmi-windows-management-instrumentation.md)
