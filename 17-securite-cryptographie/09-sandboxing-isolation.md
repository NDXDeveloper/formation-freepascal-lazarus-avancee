🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 17.9 Sandboxing et isolation

## Introduction

Le sandboxing (bac à sable en français) est une technique de sécurité qui consiste à exécuter un programme dans un environnement isolé et contrôlé, limitant son accès aux ressources système. C'est comme construire une "boîte sécurisée" autour de votre application pour éviter qu'elle ne puisse nuire au système ou accéder à des données sensibles en cas de compromission.

## Concepts fondamentaux

### Qu'est-ce que le sandboxing ?

Imaginez un enfant qui joue dans un bac à sable au parc : il peut construire des châteaux et s'amuser librement, mais il ne peut pas sortir du bac et aller sur la route. De la même manière, une application dans un sandbox peut fonctionner normalement, mais ne peut pas :

- Accéder à des fichiers en dehors de son répertoire autorisé
- Modifier des paramètres système critiques
- Communiquer avec d'autres processus non autorisés
- Consommer des ressources système de manière excessive

### Pourquoi utiliser le sandboxing ?

**Avantages de sécurité :**
- **Protection contre les exploits** : Si votre application est compromise, les dégâts sont limités
- **Principe du moindre privilège** : L'application n'a accès qu'à ce dont elle a besoin
- **Isolation des composants** : Séparer les parties sensibles des parties exposées
- **Protection de la vie privée** : Limiter l'accès aux données personnelles

**Cas d'usage typiques :**
- Applications manipulant des données non fiables (fichiers uploadés, contenu web)
- Plugins et extensions
- Applications multi-utilisateurs
- Traitement de données sensibles

## Niveaux d'isolation

### 1. Isolation au niveau processus (léger)

Le niveau le plus simple : créer des processus séparés avec des droits limités.

### 2. Isolation au niveau conteneur (moyen)

Utilisation de technologies comme Docker ou les namespaces Linux.

### 3. Isolation au niveau machine virtuelle (fort)

Virtualisation complète du système d'exploitation.

## Sandboxing sous Windows

### 1. Exécution avec des privilèges réduits

#### Création d'un token restreint

```pascal
unit WindowsSandbox;

{$IFDEF WINDOWS}

interface

uses
  Windows, SysUtils;

type
  TSandboxedProcess = class
  private
    FProcessInfo: TProcessInformation;
    FStartupInfo: TStartupInfo;
    FRestrictedToken: THandle;
  public
    constructor Create;
    destructor Destroy; override;
    function Execute(const CommandLine: string): Boolean;
    function IsRunning: Boolean;
    procedure Terminate;
    property ProcessInfo: TProcessInformation read FProcessInfo;
  end;

implementation

constructor TSandboxedProcess.Create;  
var
  SidsToDisable: array[0..0] of TSIDAndAttributes;
  AdminSid: PSID;
begin
  inherited Create;
  FRestrictedToken := 0;

  // Créer un SID pour les administrateurs
  AllocateAndInitializeSid(
    SECURITY_NT_AUTHORITY,
    2,
    SECURITY_BUILTIN_DOMAIN_RID,
    DOMAIN_ALIAS_RID_ADMINS,
    0, 0, 0, 0, 0, 0,
    AdminSid
  );

  try
    // Créer un token restreint en désactivant les droits administrateur
    SidsToDisable[0].Sid := AdminSid;
    SidsToDisable[0].Attributes := 0;

    if not CreateRestrictedToken(
      GetCurrentProcess,
      DISABLE_MAX_PRIVILEGE,
      1,
      @SidsToDisable,
      0,
      nil,
      0,
      nil,
      @FRestrictedToken
    ) then
      raise Exception.Create('Impossible de créer un token restreint');
  finally
    FreeSid(AdminSid);
  end;
end;

destructor TSandboxedProcess.Destroy;  
begin
  if FProcessInfo.hProcess <> 0 then
    CloseHandle(FProcessInfo.hProcess);
  if FProcessInfo.hThread <> 0 then
    CloseHandle(FProcessInfo.hThread);
  if FRestrictedToken <> 0 then
    CloseHandle(FRestrictedToken);
  inherited;
end;

function TSandboxedProcess.Execute(const CommandLine: string): Boolean;  
var
  CmdLine: string;
begin
  ZeroMemory(@FStartupInfo, SizeOf(FStartupInfo));
  FStartupInfo.cb := SizeOf(FStartupInfo);
  FStartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  FStartupInfo.wShowWindow := SW_SHOW;

  CmdLine := CommandLine;

  // Créer le processus avec le token restreint
  Result := CreateProcessAsUser(
    FRestrictedToken,
    nil,
    PChar(CmdLine),
    nil,
    nil,
    False,
    CREATE_NEW_CONSOLE,
    nil,
    nil,
    FStartupInfo,
    FProcessInfo
  );
end;

function TSandboxedProcess.IsRunning: Boolean;  
var
  ExitCode: DWORD;
begin
  Result := False;
  if FProcessInfo.hProcess <> 0 then
  begin
    GetExitCodeProcess(FProcessInfo.hProcess, ExitCode);
    Result := (ExitCode = STILL_ACTIVE);
  end;
end;

procedure TSandboxedProcess.Terminate;  
begin
  if FProcessInfo.hProcess <> 0 then
    TerminateProcess(FProcessInfo.hProcess, 1);
end;

end.

{$ENDIF}
```

**Utilisation :**

```pascal
var
  Sandbox: TSandboxedProcess;
begin
  Sandbox := TSandboxedProcess.Create;
  try
    if Sandbox.Execute('notepad.exe') then
      ShowMessage('Processus démarré avec privilèges limités')
    else
      ShowMessage('Erreur : ' + SysErrorMessage(GetLastError));
  finally
    Sandbox.Free;
  end;
end;
```

### 2. Restriction d'accès aux fichiers

#### Limitation à un répertoire spécifique

```pascal
unit FileSystemSandbox;

{$IFDEF WINDOWS}

interface

uses
  Windows, SysUtils, Classes;

type
  TFileAccessMode = (famReadOnly, famReadWrite, famNoAccess);

  TFileSandbox = class
  private
    FAllowedPaths: TStringList;
    FDefaultMode: TFileAccessMode;
    function IsPathAllowed(const Path: string): Boolean;
    function NormalizePath(const Path: string): string;
  public
    constructor Create(DefaultMode: TFileAccessMode = famNoAccess);
    destructor Destroy; override;

    procedure AddAllowedPath(const Path: string);
    procedure RemoveAllowedPath(const Path: string);

    function CanRead(const FileName: string): Boolean;
    function CanWrite(const FileName: string): Boolean;

    function SafeFileOpen(const FileName: string; Mode: Word): THandle;
    procedure SafeFileClose(Handle: THandle);
  end;

implementation

uses
  StrUtils;

constructor TFileSandbox.Create(DefaultMode: TFileAccessMode);  
begin
  inherited Create;
  FAllowedPaths := TStringList.Create;
  FAllowedPaths.Sorted := True;
  FAllowedPaths.Duplicates := dupIgnore;
  FDefaultMode := DefaultMode;
end;

destructor TFileSandbox.Destroy;  
begin
  FAllowedPaths.Free;
  inherited;
end;

function TFileSandbox.NormalizePath(const Path: string): string;  
begin
  // Convertir en chemin absolu et normaliser
  Result := ExpandFileName(Path);
  Result := UpperCase(Result);
  // Assurer qu'on se termine par un backslash pour les répertoires
  if DirectoryExists(Result) and (Result[Length(Result)] <> '\') then
    Result := Result + '\';
end;

procedure TFileSandbox.AddAllowedPath(const Path: string);  
begin
  FAllowedPaths.Add(NormalizePath(Path));
end;

procedure TFileSandbox.RemoveAllowedPath(const Path: string);  
var
  Index: Integer;
begin
  Index := FAllowedPaths.IndexOf(NormalizePath(Path));
  if Index >= 0 then
    FAllowedPaths.Delete(Index);
end;

function TFileSandbox.IsPathAllowed(const Path: string): Boolean;  
var
  NormalPath: string;
  i: Integer;
begin
  Result := False;
  NormalPath := NormalizePath(Path);

  // Vérifier si le chemin commence par un chemin autorisé
  for i := 0 to FAllowedPaths.Count - 1 do
  begin
    if StartsText(FAllowedPaths[i], NormalPath) then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function TFileSandbox.CanRead(const FileName: string): Boolean;  
begin
  Result := IsPathAllowed(FileName) or (FDefaultMode in [famReadOnly, famReadWrite]);
end;

function TFileSandbox.CanWrite(const FileName: string): Boolean;  
begin
  Result := IsPathAllowed(FileName) and (FDefaultMode = famReadWrite);
end;

function TFileSandbox.SafeFileOpen(const FileName: string; Mode: Word): THandle;  
begin
  Result := INVALID_HANDLE_VALUE;

  // Vérifier les permissions
  if (Mode and (fmOpenRead or fmOpenWrite) = fmOpenRead) then
  begin
    if not CanRead(FileName) then
      raise Exception.CreateFmt('Accès en lecture refusé : %s', [FileName]);
  end;

  if (Mode and fmOpenWrite = fmOpenWrite) then
  begin
    if not CanWrite(FileName) then
      raise Exception.CreateFmt('Accès en écriture refusé : %s', [FileName]);
  end;

  // Ouvrir le fichier si autorisé
  Result := FileOpen(FileName, Mode);
end;

procedure TFileSandbox.SafeFileClose(Handle: THandle);  
begin
  if Handle <> INVALID_HANDLE_VALUE then
    FileClose(Handle);
end;

end.

{$ENDIF}
```

**Exemple d'utilisation :**

```pascal
var
  Sandbox: TFileSandbox;
  FileHandle: THandle;
begin
  Sandbox := TFileSandbox.Create(famNoAccess);
  try
    // Autoriser uniquement le répertoire de données
    Sandbox.AddAllowedPath('C:\MyApp\Data\');

    // Ceci fonctionnera
    FileHandle := Sandbox.SafeFileOpen('C:\MyApp\Data\config.txt', fmOpenRead);
    if FileHandle <> INVALID_HANDLE_VALUE then
    begin
      // Traiter le fichier
      Sandbox.SafeFileClose(FileHandle);
    end;

    // Ceci lèvera une exception
    try
      FileHandle := Sandbox.SafeFileOpen('C:\Windows\System32\config.sys', fmOpenRead);
    except
      on E: Exception do
        ShowMessage('Accès bloqué : ' + E.Message);
    end;
  finally
    Sandbox.Free;
  end;
end;
```

### 3. Jobs et limitation de ressources

Les Jobs Windows permettent de limiter les ressources CPU, mémoire, et réseau.

```pascal
unit WindowsJobSandbox;

{$IFDEF WINDOWS}

interface

uses
  Windows, SysUtils;

type
  TResourceLimits = record
    MaxMemoryMB: Cardinal;        // Mémoire maximale en MB
    MaxCPUPercent: Cardinal;      // Pourcentage CPU max (0-100)
    MaxProcesses: Cardinal;       // Nombre max de processus
    AllowNetworkAccess: Boolean;  // Autoriser l'accès réseau
  end;

  TJobSandbox = class
  private
    FJobHandle: THandle;
    FLimits: TResourceLimits;
    procedure ApplyLimits;
  public
    constructor Create(const Limits: TResourceLimits);
    destructor Destroy; override;

    function AddProcess(ProcessHandle: THandle): Boolean;
    function CreateSandboxedProcess(const CommandLine: string): THandle;
    procedure Terminate;
  end;

implementation

constructor TJobSandbox.Create(const Limits: TResourceLimits);  
begin
  inherited Create;
  FLimits := Limits;

  // Créer un objet Job
  FJobHandle := CreateJobObject(nil, nil);
  if FJobHandle = 0 then
    raise Exception.Create('Impossible de créer un Job Object');

  ApplyLimits;
end;

destructor TJobSandbox.Destroy;  
begin
  if FJobHandle <> 0 then
    CloseHandle(FJobHandle);
  inherited;
end;

procedure TJobSandbox.ApplyLimits;  
var
  BasicLimit: JOBOBJECT_BASIC_LIMIT_INFORMATION;
  ExtendedLimit: JOBOBJECT_EXTENDED_LIMIT_INFORMATION;
begin
  ZeroMemory(@ExtendedLimit, SizeOf(ExtendedLimit));

  // Limites de base
  BasicLimit.LimitFlags := 0;

  // Limite mémoire
  if FLimits.MaxMemoryMB > 0 then
  begin
    ExtendedLimit.ProcessMemoryLimit := FLimits.MaxMemoryMB * 1024 * 1024;
    ExtendedLimit.BasicLimitInformation.LimitFlags :=
      ExtendedLimit.BasicLimitInformation.LimitFlags or JOB_OBJECT_LIMIT_PROCESS_MEMORY;
  end;

  // Limite nombre de processus
  if FLimits.MaxProcesses > 0 then
  begin
    ExtendedLimit.BasicLimitInformation.ActiveProcessLimit := FLimits.MaxProcesses;
    ExtendedLimit.BasicLimitInformation.LimitFlags :=
      ExtendedLimit.BasicLimitInformation.LimitFlags or JOB_OBJECT_LIMIT_ACTIVE_PROCESS;
  end;

  // Empêcher la création de processus enfants hors du job
  ExtendedLimit.BasicLimitInformation.LimitFlags :=
    ExtendedLimit.BasicLimitInformation.LimitFlags or JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE;

  // Appliquer les limites
  SetInformationJobObject(
    FJobHandle,
    JobObjectExtendedLimitInformation,
    @ExtendedLimit,
    SizeOf(ExtendedLimit)
  );
end;

function TJobSandbox.AddProcess(ProcessHandle: THandle): Boolean;  
begin
  Result := AssignProcessToJobObject(FJobHandle, ProcessHandle);
end;

function TJobSandbox.CreateSandboxedProcess(const CommandLine: string): THandle;  
var
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  CmdLine: string;
begin
  Result := 0;

  ZeroMemory(@StartupInfo, SizeOf(StartupInfo));
  StartupInfo.cb := SizeOf(StartupInfo);

  CmdLine := CommandLine;

  // Créer le processus en état suspendu
  if CreateProcess(
    nil,
    PChar(CmdLine),
    nil,
    nil,
    False,
    CREATE_SUSPENDED,
    nil,
    nil,
    StartupInfo,
    ProcessInfo
  ) then
  begin
    // Assigner au job
    if AddProcess(ProcessInfo.hProcess) then
    begin
      // Reprendre l'exécution
      ResumeThread(ProcessInfo.hThread);
      Result := ProcessInfo.hProcess;
    end
    else
    begin
      TerminateProcess(ProcessInfo.hProcess, 1);
      CloseHandle(ProcessInfo.hProcess);
    end;

    CloseHandle(ProcessInfo.hThread);
  end;
end;

procedure TJobSandbox.Terminate;  
begin
  if FJobHandle <> 0 then
    TerminateJobObject(FJobHandle, 0);
end;

end.

{$ENDIF}
```

**Exemple d'utilisation :**

```pascal
var
  Limits: TResourceLimits;
  JobSandbox: TJobSandbox;
  ProcessHandle: THandle;
begin
  // Définir des limites strictes
  Limits.MaxMemoryMB := 100;        // Max 100 MB
  Limits.MaxCPUPercent := 50;       // Max 50% CPU
  Limits.MaxProcesses := 1;         // Un seul processus
  Limits.AllowNetworkAccess := False;

  JobSandbox := TJobSandbox.Create(Limits);
  try
    ProcessHandle := JobSandbox.CreateSandboxedProcess('myapp.exe params');
    if ProcessHandle <> 0 then
    begin
      ShowMessage('Processus créé avec limitations de ressources');
      // Attendre ou faire autre chose
      WaitForSingleObject(ProcessHandle, INFINITE);
      CloseHandle(ProcessHandle);
    end;
  finally
    JobSandbox.Free;
  end;
end;
```

## Sandboxing sous Linux/Ubuntu

### 1. Utilisation des namespaces

Les namespaces Linux permettent d'isoler différentes ressources système.

```pascal
unit LinuxSandbox;

{$IFDEF UNIX}

interface

uses
  BaseUnix, Unix, SysUtils, Classes;

type
  TNamespaceType = (
    nsIPC,      // Isolation IPC
    nsNetwork,  // Isolation réseau
    nsMount,    // Isolation système de fichiers
    nsPID,      // Isolation PID
    nsUser      // Isolation utilisateur
  );

  TNamespaceTypes = set of TNamespaceType;

  TLinuxSandbox = class
  private
    FNamespaces: TNamespaceTypes;
    FRootPath: string;
    procedure SetupNamespaces;
    procedure SetupChroot;
  public
    constructor Create(Namespaces: TNamespaceTypes; const RootPath: string = '');
    function Execute(const Command: string; Args: array of string): Integer;
  end;

implementation

uses
  Process;

constructor TLinuxSandbox.Create(Namespaces: TNamespaceTypes; const RootPath: string);  
begin
  inherited Create;
  FNamespaces := Namespaces;
  FRootPath := RootPath;
end;

procedure TLinuxSandbox.SetupNamespaces;  
var
  Flags: Integer;
begin
  Flags := 0;

  // Convertir les namespaces en flags
  if nsIPC in FNamespaces then
    Flags := Flags or $08000000;  // CLONE_NEWIPC
  if nsNetwork in FNamespaces then
    Flags := Flags or $40000000;  // CLONE_NEWNET
  if nsMount in FNamespaces then
    Flags := Flags or $00020000;  // CLONE_NEWNS
  if nsPID in FNamespaces then
    Flags := Flags or $20000000;  // CLONE_NEWPID
  if nsUser in FNamespaces then
    Flags := Flags or $10000000;  // CLONE_NEWUSER

  // Note : En Pascal, l'appel direct à unshare() nécessite des bindings
  // Pour simplifier, on utilise l'outil système 'unshare'
end;

procedure TLinuxSandbox.SetupChroot;  
begin
  if FRootPath <> '' then
  begin
    if fpChroot(PChar(FRootPath)) <> 0 then
      raise Exception.CreateFmt('Impossible de chroot vers %s', [FRootPath]);
    fpChdir('/');
  end;
end;

function TLinuxSandbox.Execute(const Command: string; Args: array of string): Integer;  
var
  Process: TProcess;
  i: Integer;
  UnshareCmd: string;
begin
  Process := TProcess.Create(nil);
  try
    // Construire la commande avec unshare
    UnshareCmd := 'unshare';

    if nsIPC in FNamespaces then
      Process.Parameters.Add('--ipc');
    if nsNetwork in FNamespaces then
      Process.Parameters.Add('--net');
    if nsMount in FNamespaces then
      Process.Parameters.Add('--mount');
    if nsPID in FNamespaces then
      Process.Parameters.Add('--pid');
    if nsUser in FNamespaces then
      Process.Parameters.Add('--user');

    // Ajouter la commande à exécuter
    Process.Parameters.Add(Command);
    for i := Low(Args) to High(Args) do
      Process.Parameters.Add(Args[i]);

    Process.Executable := UnshareCmd;
    Process.Options := [poWaitOnExit];

    Process.Execute;
    Result := Process.ExitStatus;
  finally
    Process.Free;
  end;
end;

end.

{$ENDIF}
```

**Utilisation :**

```pascal
var
  Sandbox: TLinuxSandbox;
  ExitCode: Integer;
begin
  // Créer un sandbox avec isolation réseau et PID
  Sandbox := TLinuxSandbox.Create([nsNetwork, nsPID]);
  try
    ExitCode := Sandbox.Execute('/usr/bin/myapp', ['--safe-mode']);
    WriteLn('Processus terminé avec code : ', ExitCode);
  finally
    Sandbox.Free;
  end;
end;
```

### 2. Utilisation de seccomp (Secure Computing Mode)

Seccomp permet de filtrer les appels système autorisés.

```pascal
unit SeccompSandbox;

{$IFDEF LINUX}

interface

uses
  SysUtils, Classes;

type
  TSystemCall = (
    scRead,
    scWrite,
    scOpen,
    scClose,
    scExit,
    scBrk,
    scMmap,
    scMunmap
    // ... autres appels système
  );

  TSystemCalls = set of TSystemCall;

  TSeccompSandbox = class
  private
    FAllowedCalls: TSystemCalls;
    procedure SetupSeccompFilter;
  public
    constructor Create(AllowedCalls: TSystemCalls);
    procedure ApplyFilter;
  end;

implementation

uses
  Process;

constructor TSeccompSandbox.Create(AllowedCalls: TSystemCalls);  
begin
  inherited Create;
  FAllowedCalls := AllowedCalls;
end;

procedure TSeccompSandbox.SetupSeccompFilter;  
begin
  // Note : L'implémentation complète nécessiterait des bindings
  // vers libseccomp ou l'écriture directe de filtres BPF
  // Pour la démonstration, on utilise l'approche simple
end;

procedure TSeccompSandbox.ApplyFilter;  
var
  Process: TProcess;
begin
  // Utiliser l'outil seccomp-bpf ou systrace pour appliquer le filtre
  // Exemple simplifié avec firejail qui utilise seccomp en arrière-plan
  Process := TProcess.Create(nil);
  try
    Process.Executable := 'firejail';
    Process.Parameters.Add('--seccomp');
    Process.Options := [poWaitOnExit];
    Process.Execute;
  finally
    Process.Free;
  end;
end;

end.

{$ENDIF}
```

### 3. Firejail - Solution clé en main

Firejail est un outil de sandboxing facile à utiliser sous Linux.

```pascal
unit FirejailSandbox;

{$IFDEF LINUX}

interface

uses
  Classes, SysUtils, Process;

type
  TFirejailOptions = record
    NoNetwork: Boolean;
    NoSound: Boolean;
    Private: Boolean;           // Filesystem privé
    PrivateDev: Boolean;        // /dev privé
    ReadOnlyPaths: TStringList; // Chemins en lecture seule
    WhitelistPaths: TStringList;// Chemins autorisés
  end;

  TFirejailSandbox = class
  private
    FOptions: TFirejailOptions;
    function BuildCommandLine: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    function Execute(const Command: string; Args: TStringList = nil): Integer;
    property Options: TFirejailOptions read FOptions write FOptions;
  end;

implementation

constructor TFirejailSandbox.Create;  
begin
  inherited Create;
  FOptions.ReadOnlyPaths := TStringList.Create;
  FOptions.WhitelistPaths := TStringList.Create;

  // Valeurs par défaut sécurisées
  FOptions.NoNetwork := False;
  FOptions.NoSound := False;
  FOptions.Private := True;
  FOptions.PrivateDev := True;
end;

destructor TFirejailSandbox.Destroy;  
begin
  FOptions.ReadOnlyPaths.Free;
  FOptions.WhitelistPaths.Free;
  inherited;
end;

function TFirejailSandbox.BuildCommandLine: TStringList;  
var
  i: Integer;
begin
  Result := TStringList.Create;

  if FOptions.NoNetwork then
    Result.Add('--net=none');

  if FOptions.NoSound then
    Result.Add('--nosound');

  if FOptions.Private then
    Result.Add('--private');

  if FOptions.PrivateDev then
    Result.Add('--private-dev');

  // Chemins en lecture seule
  for i := 0 to FOptions.ReadOnlyPaths.Count - 1 do
    Result.Add('--read-only=' + FOptions.ReadOnlyPaths[i]);

  // Chemins autorisés (whitelist)
  for i := 0 to FOptions.WhitelistPaths.Count - 1 do
    Result.Add('--whitelist=' + FOptions.WhitelistPaths[i]);
end;

function TFirejailSandbox.Execute(const Command: string; Args: TStringList): Integer;  
var
  Process: TProcess;
  CmdLine: TStringList;
  i: Integer;
begin
  Process := TProcess.Create(nil);
  CmdLine := BuildCommandLine;
  try
    Process.Executable := 'firejail';

    // Ajouter les options
    for i := 0 to CmdLine.Count - 1 do
      Process.Parameters.Add(CmdLine[i]);

    // Ajouter la commande
    Process.Parameters.Add(Command);

    // Ajouter les arguments
    if Assigned(Args) then
      for i := 0 to Args.Count - 1 do
        Process.Parameters.Add(Args[i]);

    Process.Options := [poWaitOnExit, poUsePipes];
    Process.Execute;

    Result := Process.ExitStatus;
  finally
    CmdLine.Free;
    Process.Free;
  end;
end;

end.

{$ENDIF}
```

**Exemple d'utilisation :**

```pascal
var
  Sandbox: TFirejailSandbox;
  Args: TStringList;
begin
  Sandbox := TFirejailSandbox.Create;
  Args := TStringList.Create;
  try
    // Configuration du sandbox
    Sandbox.Options.NoNetwork := True;  // Pas d'accès réseau
    Sandbox.Options.Private := True;    // Filesystem isolé

    // Autoriser uniquement certains répertoires
    Sandbox.Options.WhitelistPaths.Add('/home/user/data');

    // Mettre /etc en lecture seule
    Sandbox.Options.ReadOnlyPaths.Add('/etc');

    // Arguments pour l'application
    Args.Add('--config');
    Args.Add('/home/user/data/config.ini');

    // Exécuter
    if Sandbox.Execute('/usr/local/bin/myapp', Args) = 0 then
      WriteLn('Exécution réussie')
    else
      WriteLn('Erreur d''exécution');
  finally
    Args.Free;
    Sandbox.Free;
  end;
end;
```

## Solution multi-plateforme : Docker

Docker offre une solution de sandboxing portable entre Windows et Linux.

```pascal
unit DockerSandbox;

interface

uses
  Classes, SysUtils, Process;

type
  TDockerSandbox = class
  private
    FImageName: string;
    FContainerName: string;
    FVolumes: TStringList;
    FEnvironmentVars: TStringList;
    FNetworkMode: string;
    FMemoryLimit: string;
    FCPULimit: string;
  public
    constructor Create(const ImageName: string);
    destructor Destroy; override;

    procedure AddVolume(const HostPath, ContainerPath: string; ReadOnly: Boolean = False);
    procedure AddEnvironmentVar(const Name, Value: string);

    function Run(const Command: string = ''): Integer;
    function Stop: Boolean;
    function Remove: Boolean;
    function GetLogs: string;

    property ImageName: string read FImageName write FImageName;
    property ContainerName: string read FContainerName write FContainerName;
    property NetworkMode: string read FNetworkMode write FNetworkMode;
    property MemoryLimit: string read FMemoryLimit write FMemoryLimit;
    property CPULimit: string read FCPULimit write FCPULimit;
  end;

implementation

constructor TDockerSandbox.Create(const ImageName: string);  
begin
  inherited Create;
  FImageName := ImageName;
  FContainerName := 'sandbox_' + FormatDateTime('yyyymmddhhnnss', Now);
  FVolumes := TStringList.Create;
  FEnvironmentVars := TStringList.Create;
  FNetworkMode := 'bridge'; // Par défaut
  FMemoryLimit := '';
  FCPULimit := '';
end;

destructor TDockerSandbox.Destroy;  
begin
  FVolumes.Free;
  FEnvironmentVars.Free;
  inherited;
end;

procedure TDockerSandbox.AddVolume(const HostPath, ContainerPath: string; ReadOnly: Boolean);  
var
  VolumeSpec: string;
begin
  VolumeSpec := HostPath + ':' + ContainerPath;
  if ReadOnly then
    VolumeSpec := VolumeSpec + ':ro';
  FVolumes.Add(VolumeSpec);
end;

procedure TDockerSandbox.AddEnvironmentVar(const Name, Value: string);  
begin
  FEnvironmentVars.Add(Name + '=' + Value);
end;

function TDockerSandbox.Run(const Command: string): Integer;  
var
  Process: TProcess;
  i: Integer;
begin
  Process := TProcess.Create(nil);
  try
    Process.Executable := 'docker';
    Process.Parameters.Add('run');
    Process.Parameters.Add('--name');
    Process.Parameters.Add(FContainerName);

    // Mode réseau
    if FNetworkMode <> '' then
    begin
      Process.Parameters.Add('--network');
      Process.Parameters.Add(FNetworkMode);
    end;

    // Limites mémoire
    if FMemoryLimit <> '' then
    begin
      Process.Parameters.Add('--memory');
      Process.Parameters.Add(FMemoryLimit);
    end;

    // Limites CPU
    if FCPULimit <> '' then
    begin
      Process.Parameters.Add('--cpus');
      Process.Parameters.Add(FCPULimit);
    end;

    // Volumes
    for i := 0 to FVolumes.Count - 1 do
    begin
      Process.Parameters.Add('-v');
      Process.Parameters.Add(FVolumes[i]);
    end;

    // Variables d'environnement
    for i := 0 to FEnvironmentVars.Count - 1 do
    begin
      Process.Parameters.Add('-e');
      Process.Parameters.Add(FEnvironmentVars[i]);
    end;

    // Supprimer automatiquement après arrêt
    Process.Parameters.Add('--rm');

    // Image
    Process.Parameters.Add(FImageName);

    // Commande optionnelle
    if Command <> '' then
      Process.Parameters.Add(Command);

    Process.Options := [poWaitOnExit, poUsePipes];
    Process.Execute;

    Result := Process.ExitStatus;
  finally
    Process.Free;
  end;
end;

function TDockerSandbox.Stop: Boolean;  
var
  Process: TProcess;
begin
  Process := TProcess.Create(nil);
  try
    Process.Executable := 'docker';
    Process.Parameters.Add('stop');
    Process.Parameters.Add(FContainerName);
    Process.Options := [poWaitOnExit];
    Process.Execute;
    Result := (Process.ExitStatus = 0);
  finally
    Process.Free;
  end;
end;

function TDockerSandbox.Remove: Boolean;  
var
  Process: TProcess;
begin
  Process := TProcess.Create(nil);
  try
    Process.Executable := 'docker';
    Process.Parameters.Add('rm');
    Process.Parameters.Add('-f');
    Process.Parameters.Add(FContainerName);
    Process.Options := [poWaitOnExit];
    Process.Execute;
    Result := (Process.ExitStatus = 0);
  finally
    Process.Free;
  end;
end;

function TDockerSandbox.GetLogs: string;  
var
  Process: TProcess;
  Output: TStringList;
begin
  Result := '';
  Process := TProcess.Create(nil);
  Output := TStringList.Create;
  try
    Process.Executable := 'docker';
    Process.Parameters.Add('logs');
    Process.Parameters.Add(FContainerName);
    Process.Options := [poWaitOnExit, poUsePipes];
    Process.Execute;

    Output.LoadFromStream(Process.Output);
    Result := Output.Text;
  finally
    Output.Free;
    Process.Free;
  end;
end;

end.
```

**Exemple d'utilisation Docker :**

```pascal
var
  Sandbox: TDockerSandbox;
begin
  // Créer un sandbox Docker basé sur Ubuntu
  Sandbox := TDockerSandbox.Create('ubuntu:22.04');
  try
    // Configuration du conteneur
    Sandbox.NetworkMode := 'none';           // Pas d'accès réseau
    Sandbox.MemoryLimit := '512m';           // Max 512 MB
    Sandbox.CPULimit := '0.5';               // Max 50% d'un CPU

    // Monter un volume en lecture seule
    Sandbox.AddVolume('C:\MyApp\Data', '/data', True);

    // Variables d'environnement
    Sandbox.AddEnvironmentVar('APP_MODE', 'sandbox');

    // Exécuter une commande
    if Sandbox.Run('/bin/bash -c "ls /data"') = 0 then
      WriteLn('Exécution réussie')
    else
      WriteLn('Erreur : ', Sandbox.GetLogs);
  finally
    Sandbox.Free;
  end;
end;
```

## Sandbox applicatif : Framework de plugins

Pour isoler des plugins ou extensions dans votre application.

```pascal
unit PluginSandbox;

interface

uses
  Classes, SysUtils, SyncObjs;

type
  TPluginPermissions = set of (
    ppFileRead,      // Lecture fichiers
    ppFileWrite,     // Écriture fichiers
    ppNetwork,       // Accès réseau
    ppRegistry,      // Accès registre (Windows)
    ppProcessCreate, // Créer des processus
    ppSystemInfo     // Info système
  );

  TPluginSandbox = class
  private
    FPermissions: TPluginPermissions;
    FAllowedPaths: TStringList;
    FMaxMemoryMB: Cardinal;
    FTimeoutSeconds: Cardinal;
    FLock: TCriticalSection;

    function CheckPermission(Permission: TPluginPermissions): Boolean;
    function IsPathAllowed(const Path: string): Boolean;
  public
    constructor Create(Permissions: TPluginPermissions);
    destructor Destroy; override;

    // Méthodes sécurisées pour les plugins
    function SafeFileRead(const FileName: string; out Content: string): Boolean;
    function SafeFileWrite(const FileName, Content: string): Boolean;
    function SafeHttpGet(const URL: string; out Response: string): Boolean;
    function SafeCreateProcess(const Command: string): Boolean;

    procedure AddAllowedPath(const Path: string);

    property Permissions: TPluginPermissions read FPermissions;
    property MaxMemoryMB: Cardinal read FMaxMemoryMB write FMaxMemoryMB;
    property TimeoutSeconds: Cardinal read FTimeoutSeconds write FTimeoutSeconds;
  end;

implementation

uses
  {$IFDEF WINDOWS}Windows,{$ENDIF}
  StrUtils;

constructor TPluginSandbox.Create(Permissions: TPluginPermissions);  
begin
  inherited Create;
  FPermissions := Permissions;
  FAllowedPaths := TStringList.Create;
  FAllowedPaths.Sorted := True;
  FLock := TCriticalSection.Create;
  FMaxMemoryMB := 100;
  FTimeoutSeconds := 30;
end;

destructor TPluginSandbox.Destroy;  
begin
  FLock.Free;
  FAllowedPaths.Free;
  inherited;
end;

function TPluginSandbox.CheckPermission(Permission: TPluginPermissions): Boolean;  
begin
  Result := Permission <= FPermissions;
  if not Result then
    raise Exception.Create('Permission refusée');
end;

function TPluginSandbox.IsPathAllowed(const Path: string): Boolean;  
var
  NormalPath: string;
  i: Integer;
begin
  Result := False;
  NormalPath := UpperCase(ExpandFileName(Path));

  for i := 0 to FAllowedPaths.Count - 1 do
  begin
    if StartsText(FAllowedPaths[i], NormalPath) then
    begin
      Result := True;
      Break;
    end;
  end;
end;

procedure TPluginSandbox.AddAllowedPath(const Path: string);  
begin
  FLock.Enter;
  try
    FAllowedPaths.Add(UpperCase(ExpandFileName(Path)));
  finally
    FLock.Leave;
  end;
end;

function TPluginSandbox.SafeFileRead(const FileName: string; out Content: string): Boolean;  
var
  FileStream: TFileStream;
  StringStream: TStringStream;
begin
  Result := False;
  Content := '';

  // Vérifier les permissions
  CheckPermission([ppFileRead]);

  // Vérifier le chemin
  if not IsPathAllowed(FileName) then
    raise Exception.CreateFmt('Accès refusé au fichier : %s', [FileName]);

  // Vérifier l'existence
  if not FileExists(FileName) then
    Exit;

  FLock.Enter;
  try
    FileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    try
      StringStream := TStringStream.Create('');
      try
        // Limiter la taille de lecture
        if FileStream.Size > FMaxMemoryMB * 1024 * 1024 then
          raise Exception.Create('Fichier trop volumineux');

        StringStream.CopyFrom(FileStream, 0);
        Content := StringStream.DataString;
        Result := True;
      finally
        StringStream.Free;
      end;
    finally
      FileStream.Free;
    end;
  finally
    FLock.Leave;
  end;
end;

function TPluginSandbox.SafeFileWrite(const FileName, Content: string): Boolean;  
var
  FileStream: TFileStream;
  StringStream: TStringStream;
begin
  Result := False;

  // Vérifier les permissions
  CheckPermission([ppFileWrite]);

  // Vérifier le chemin
  if not IsPathAllowed(FileName) then
    raise Exception.CreateFmt('Accès refusé au fichier : %s', [FileName]);

  // Vérifier la taille
  if Length(Content) > FMaxMemoryMB * 1024 * 1024 then
    raise Exception.Create('Contenu trop volumineux');

  FLock.Enter;
  try
    StringStream := TStringStream.Create(Content);
    try
      FileStream := TFileStream.Create(FileName, fmCreate);
      try
        FileStream.CopyFrom(StringStream, 0);
        Result := True;
      finally
        FileStream.Free;
      end;
    finally
      StringStream.Free;
    end;
  finally
    FLock.Leave;
  end;
end;

function TPluginSandbox.SafeHttpGet(const URL: string; out Response: string): Boolean;  
begin
  Result := False;
  Response := '';

  // Vérifier les permissions
  CheckPermission([ppNetwork]);

  // Implémentation avec une bibliothèque HTTP
  // (synapse, indy, fphttpclient, etc.)
  // Avec timeout et limite de taille

  raise Exception.Create('Non implémenté dans cet exemple');
end;

function TPluginSandbox.SafeCreateProcess(const Command: string): Boolean;  
begin
  Result := False;

  // Vérifier les permissions
  CheckPermission([ppProcessCreate]);

  // Créer un processus avec restrictions
  // Utiliser les techniques vues précédemment

  raise Exception.Create('Non implémenté dans cet exemple');
end;

end.
```

**Exemple d'utilisation du sandbox de plugins :**

```pascal
var
  Sandbox: TPluginSandbox;
  Content: string;
begin
  // Créer un sandbox avec permissions limitées
  Sandbox := TPluginSandbox.Create([ppFileRead, ppFileWrite]);
  try
    // Configurer
    Sandbox.MaxMemoryMB := 50;
    Sandbox.TimeoutSeconds := 10;
    Sandbox.AddAllowedPath('C:\MyApp\Plugins\Data\');

    // Le plugin peut maintenant utiliser le sandbox
    if Sandbox.SafeFileRead('C:\MyApp\Plugins\Data\config.txt', Content) then
      WriteLn('Fichier lu : ', Length(Content), ' octets');

    // Ceci échouera (chemin non autorisé)
    try
      Sandbox.SafeFileRead('C:\Windows\System32\config.sys', Content);
    except
      on E: Exception do
        WriteLn('Bloqué : ', E.Message);
    end;
  finally
    Sandbox.Free;
  end;
end;
```

## Isolation mémoire et données sensibles

Protéger les données en mémoire contre les dumps ou l'accès non autorisé.

```pascal
unit SecureMemory;

interface

uses
  SysUtils, Classes;

type
  TSecureString = class
  private
    FData: Pointer;
    FSize: Cardinal;
    FLocked: Boolean;
    procedure AllocateSecure(Size: Cardinal);
    procedure FreeSecure;
    procedure LockMemory;
    procedure UnlockMemory;
  public
    constructor Create(const Value: string);
    destructor Destroy; override;

    function GetValue: string;
    procedure SetValue(const Value: string);
    procedure Clear;

    property Locked: Boolean read FLocked;
  end;

implementation

uses
  {$IFDEF WINDOWS}
  Windows
  {$ENDIF}
  {$IFDEF UNIX}
  BaseUnix
  {$ENDIF};

constructor TSecureString.Create(const Value: string);  
begin
  inherited Create;
  FData := nil;
  FSize := 0;
  FLocked := False;
  SetValue(Value);
end;

destructor TSecureString.Destroy;  
begin
  Clear;
  FreeSecure;
  inherited;
end;

procedure TSecureString.AllocateSecure(Size: Cardinal);  
begin
  if FData <> nil then
    FreeSecure;

  {$IFDEF WINDOWS}
  // Allouer de la mémoire non paginable (ne sera pas swappée sur disque)
  FData := VirtualAlloc(nil, Size, MEM_COMMIT or MEM_RESERVE, PAGE_READWRITE);
  if FData = nil then
    raise Exception.Create('Impossible d''allouer de la mémoire sécurisée');

  // Verrouiller en mémoire
  if not VirtualLock(FData, Size) then
    raise Exception.Create('Impossible de verrouiller la mémoire');
  {$ENDIF}

  {$IFDEF UNIX}
  // Sous Linux, utiliser mlock
  FData := GetMem(Size);
  if mlock(FData, Size) <> 0 then
    raise Exception.Create('Impossible de verrouiller la mémoire');
  {$ENDIF}

  FSize := Size;
  FLocked := True;
end;

procedure TSecureString.FreeSecure;  
begin
  if FData <> nil then
  begin
    // Effacer la mémoire avant de la libérer
    FillChar(FData^, FSize, 0);

    {$IFDEF WINDOWS}
    VirtualUnlock(FData, FSize);
    VirtualFree(FData, 0, MEM_RELEASE);
    {$ENDIF}

    {$IFDEF UNIX}
    munlock(FData, FSize);
    FreeMem(FData);
    {$ENDIF}

    FData := nil;
    FSize := 0;
    FLocked := False;
  end;
end;

procedure TSecureString.LockMemory;  
begin
  if not FLocked and (FData <> nil) then
  begin
    {$IFDEF WINDOWS}
    VirtualLock(FData, FSize);
    {$ENDIF}
    {$IFDEF UNIX}
    mlock(FData, FSize);
    {$ENDIF}
    FLocked := True;
  end;
end;

procedure TSecureString.UnlockMemory;  
begin
  if FLocked and (FData <> nil) then
  begin
    {$IFDEF WINDOWS}
    VirtualUnlock(FData, FSize);
    {$ENDIF}
    {$IFDEF UNIX}
    munlock(FData, FSize);
    {$ENDIF}
    FLocked := False;
  end;
end;

function TSecureString.GetValue: string;  
begin
  Result := '';
  if (FData <> nil) and (FSize > 0) then
  begin
    SetLength(Result, FSize);
    Move(FData^, Result[1], FSize);
  end;
end;

procedure TSecureString.SetValue(const Value: string);  
var
  Len: Cardinal;
begin
  Clear;

  Len := Length(Value);
  if Len > 0 then
  begin
    AllocateSecure(Len);
    Move(Value[1], FData^, Len);
  end;
end;

procedure TSecureString.Clear;  
begin
  if FData <> nil then
    FillChar(FData^, FSize, 0);
end;

end.
```

**Utilisation de la mémoire sécurisée :**

```pascal
var
  SecurePassword: TSecureString;
  Password: string;
begin
  SecurePassword := TSecureString.Create('');
  try
    // Stocker un mot de passe de manière sécurisée
    SecurePassword.SetValue('MotDePasseSecret123');

    // La mémoire est verrouillée et ne peut pas être swappée sur disque
    WriteLn('Mémoire verrouillée : ', SecurePassword.Locked);

    // Utiliser le mot de passe
    Password := SecurePassword.GetValue;
    // ... faire quelque chose avec

    // Effacer immédiatement
    FillChar(Password[1], Length(Password), 0);
    Password := '';

    // Le mot de passe est automatiquement effacé à la destruction
  finally
    SecurePassword.Free;
  end;
end;
```

## Gestion des privilèges et capabilities (Linux)

Sous Linux, utiliser les capabilities plutôt que root.

```pascal
unit LinuxCapabilities;

{$IFDEF LINUX}

interface

uses
  SysUtils, Process;

type
  TCapability = (
    capChown,           // Changer ownership fichiers
    capDacOverride,     // Bypass permissions fichiers
    capNetBindService,  // Bind ports < 1024
    capNetRaw,          // Utiliser RAW sockets
    capSysAdmin,        // Admin système
    capSysTime          // Modifier l'horloge système
  );

  TCapabilities = set of TCapability;

  TCapabilityManager = class
  public
    class function GetCapabilities(const Executable: string): TCapabilities;
    class function SetCapabilities(const Executable: string; Caps: TCapabilities): Boolean;
    class function DropCapabilities: Boolean;
  end;

implementation

const
  CAP_NAMES: array[TCapability] of string = (
    'cap_chown',
    'cap_dac_override',
    'cap_net_bind_service',
    'cap_net_raw',
    'cap_sys_admin',
    'cap_sys_time'
  );

class function TCapabilityManager.GetCapabilities(const Executable: string): TCapabilities;  
var
  Process: TProcess;
  Output: TStringList;
  i: Integer;
  Cap: TCapability;
begin
  Result := [];
  Process := TProcess.Create(nil);
  Output := TStringList.Create;
  try
    Process.Executable := 'getcap';
    Process.Parameters.Add(Executable);
    Process.Options := [poWaitOnExit, poUsePipes];
    Process.Execute;

    Output.LoadFromStream(Process.Output);

    // Parser la sortie
    for Cap := Low(TCapability) to High(TCapability) do
    begin
      for i := 0 to Output.Count - 1 do
      begin
        if Pos(CAP_NAMES[Cap], Output[i]) > 0 then
        begin
          Include(Result, Cap);
          Break;
        end;
      end;
    end;
  finally
    Output.Free;
    Process.Free;
  end;
end;

class function TCapabilityManager.SetCapabilities(const Executable: string;
  Caps: TCapabilities): Boolean;
var
  Process: TProcess;
  CapString: string;
  Cap: TCapability;
begin
  // Construire la chaîne de capabilities
  CapString := '';
  for Cap := Low(TCapability) to High(TCapability) do
  begin
    if Cap in Caps then
    begin
      if CapString <> '' then
        CapString := CapString + ',';
      CapString := CapString + CAP_NAMES[Cap];
    end;
  end;

  if CapString = '' then
    CapString := 'none';

  Process := TProcess.Create(nil);
  try
    Process.Executable := 'setcap';
    Process.Parameters.Add(CapString + '=+ep');
    Process.Parameters.Add(Executable);
    Process.Options := [poWaitOnExit];
    Process.Execute;

    Result := (Process.ExitStatus = 0);
  finally
    Process.Free;
  end;
end;

class function TCapabilityManager.DropCapabilities: Boolean;  
var
  Process: TProcess;
begin
  // Abandonner tous les privilèges du processus courant
  Process := TProcess.Create(nil);
  try
    Process.Executable := 'capsh';
    Process.Parameters.Add('--drop=all');
    Process.Parameters.Add('--');
    Process.Options := [poWaitOnExit];
    Process.Execute;

    Result := (Process.ExitStatus = 0);
  finally
    Process.Free;
  end;
end;

end.

{$ENDIF}
```

**Utilisation des capabilities :**

```bash
# Donner uniquement la permission de bind sur port 80
sudo setcap 'cap_net_bind_service=+ep' /usr/local/bin/mywebserver

# L'application peut maintenant écouter sur port 80 sans être root
```

```pascal
{$IFDEF LINUX}
var
  Caps: TCapabilities;
begin
  // Vérifier les capabilities de notre exécutable
  Caps := TCapabilityManager.GetCapabilities(ParamStr(0));

  if capNetBindService in Caps then
    WriteLn('Peut bind sur ports privilégiés')
  else
    WriteLn('Ne peut pas bind sur ports < 1024');

  // Abandonner tous les privilèges après initialisation
  TCapabilityManager.DropCapabilities;
end;
{$ENDIF}
```

## Bonnes pratiques de sandboxing

### 1. Principe du moindre privilège

Toujours accorder le minimum de permissions nécessaires.

```pascal
// ❌ Mauvais : Permissions trop larges
Sandbox := TPluginSandbox.Create([ppFileRead, ppFileWrite, ppNetwork,
                                   ppRegistry, ppProcessCreate, ppSystemInfo]);

// ✅ Bon : Uniquement ce qui est nécessaire
Sandbox := TPluginSandbox.Create([ppFileRead]);  
Sandbox.AddAllowedPath('C:\MyApp\PluginData\');
```

### 2. Défense en profondeur

Combiner plusieurs couches de protection.

```pascal
procedure ExecuteUntrustedCode(const PluginPath: string);  
var
  ProcessSandbox: TSandboxedProcess;
  FileSandbox: TFileSandbox;
  JobSandbox: TJobSandbox;
  Limits: TResourceLimits;
begin
  // Couche 1 : Restriction de fichiers
  FileSandbox := TFileSandbox.Create(famReadOnly);
  FileSandbox.AddAllowedPath(ExtractFilePath(PluginPath));

  // Couche 2 : Processus avec droits limités
  ProcessSandbox := TSandboxedProcess.Create;

  // Couche 3 : Limitation de ressources
  Limits.MaxMemoryMB := 100;
  Limits.MaxCPUPercent := 25;
  Limits.MaxProcesses := 1;
  JobSandbox := TJobSandbox.Create(Limits);

  try
    // Exécuter avec toutes les protections
    ProcessSandbox.Execute(PluginPath);
  finally
    JobSandbox.Free;
    ProcessSandbox.Free;
    FileSandbox.Free;
  end;
end;
```

### 3. Timeouts et limites

Toujours imposer des limites temporelles et de ressources.

```pascal
type
  TSandboxConfig = record
    MaxExecutionSeconds: Cardinal;
    MaxMemoryMB: Cardinal;
    MaxCPUPercent: Cardinal;
    MaxDiskUsageMB: Cardinal;
    MaxNetworkBandwidthKB: Cardinal;
  end;

const
  DEFAULT_SANDBOX_CONFIG: TSandboxConfig = (
    MaxExecutionSeconds: 30;
    MaxMemoryMB: 512;
    MaxCPUPercent: 50;
    MaxDiskUsageMB: 100;
    MaxNetworkBandwidthKB: 1024;
  );
```

### 4. Logging et monitoring

Enregistrer toutes les actions dans le sandbox.

```pascal
type
  TSandboxLogger = class
  private
    FLogFile: TextFile;
  public
    constructor Create(const LogFileName: string);
    destructor Destroy; override;

    procedure LogAccess(const Resource: string; Allowed: Boolean);
    procedure LogViolation(const Details: string);
    procedure LogResourceUsage(Memory, CPU: Cardinal);
  end;

procedure TSandboxLogger.LogAccess(const Resource: string; Allowed: Boolean);  
var
  Status: string;
begin
  if Allowed then
    Status := 'ALLOWED'
  else
    Status := 'DENIED';

  WriteLn(FLogFile, FormatDateTime('yyyy-mm-dd hh:nn:ss', Now), ' ',
          Status, ' - ', Resource);
  Flush(FLogFile);
end;

procedure TSandboxLogger.LogViolation(const Details: string);  
begin
  WriteLn(FLogFile, FormatDateTime('yyyy-mm-dd hh:nn:ss', Now), ' ',
          'VIOLATION - ', Details);
  Flush(FLogFile);

  // Optionnel : Envoyer une alerte
  // SendSecurityAlert(Details);
end;
```

### 5. Test et validation

Tester le sandbox avec des cas limites.

```pascal
procedure TestSandbox;  
var
  Sandbox: TFileSandbox;
begin
  Sandbox := TFileSandbox.Create(famNoAccess);
  try
    Sandbox.AddAllowedPath('C:\Safe\');

    // Test 1 : Accès autorisé
    Assert(Sandbox.CanRead('C:\Safe\file.txt'), 'Should allow safe path');

    // Test 2 : Accès refusé
    Assert(not Sandbox.CanRead('C:\Windows\System32\file.txt'), 'Should deny system path');

    // Test 3 : Tentative de contournement avec ".."
    Assert(not Sandbox.CanRead('C:\Safe\..\Windows\System32\file.txt'),
           'Should prevent path traversal');

    // Test 4 : Liens symboliques (si applicable)
    Assert(not Sandbox.CanRead('C:\Safe\link_to_system32\file.txt'),
           'Should prevent symlink escape');

    WriteLn('Tous les tests passés');
  finally
    Sandbox.Free;
  end;
end;
```

## Considérations de performance

### Impact du sandboxing

Le sandboxing ajoute toujours un coût en performance :

| Technique | Impact CPU | Impact Mémoire | Impact I/O | Complexité |
|-----------|------------|----------------|------------|------------|
| Vérifications applicatives | Faible (1-5%) | Négligeable | Négligeable | Faible |
| Processus séparés | Moyen (5-15%) | Moyen | Faible | Moyenne |
| Conteneurs (Docker) | Moyen (10-20%) | Moyen-Fort | Moyen | Moyenne |
| Machines virtuelles | Fort (20-50%) | Fort | Fort | Élevée |

### Optimisation

```pascal
type
  TSandboxCache = class
  private
    FPathCache: TStringList;
    FCacheHits: Integer;
    FCacheMisses: Integer;
  public
    function IsPathAllowed(const Path: string): Boolean;
    procedure AddToCache(const Path: string; Allowed: Boolean);
    procedure ClearCache;

    property CacheHits: Integer read FCacheHits;
    property CacheMisses: Integer read FCacheMisses;
  end;

// Mettre en cache les résultats de vérifications coûteuses
function TSandboxCache.IsPathAllowed(const Path: string): Boolean;  
var
  Index: Integer;
begin
  Index := FPathCache.IndexOf(Path);
  if Index >= 0 then
  begin
    Inc(FCacheHits);
    Result := Boolean(Integer(FPathCache.Objects[Index]));
  end
  else
  begin
    Inc(FCacheMisses);
    // Faire la vérification réelle
    Result := DoExpensivePathCheck(Path);
    AddToCache(Path, Result);
  end;
end;
```

## Checklist de sécurité

Avant de déployer une application avec sandboxing :

```
[ ] Permissions minimales définies (moindre privilège)
[ ] Tous les chemins validés et normalisés
[ ] Timeouts configurés sur toutes les opérations
[ ] Limites de ressources appliquées (CPU, mémoire, disque)
[ ] Logging des accès et violations activé
[ ] Tests de sécurité effectués (path traversal, symlinks, etc.)
[ ] Documentation des restrictions pour les utilisateurs
[ ] Plan de réponse aux incidents de sécurité
[ ] Mécanisme de mise à jour du sandbox sans redéploiement
[ ] Monitoring en production activé
[ ] Alertes configurées pour violations critiques
[ ] Tests de charge effectués avec le sandbox actif
[ ] Documentation de la configuration pour l'équipe
```

## Cas pratiques d'utilisation

### Cas 1 : Traitement de fichiers uploadés

Isoler le traitement de fichiers potentiellement dangereux uploadés par les utilisateurs.

```pascal
unit FileProcessingSandbox;

interface

uses
  Classes, SysUtils;

type
  TFileProcessor = class
  private
    FSandboxPath: string;
    FMaxFileSizeMB: Cardinal;
    FAllowedExtensions: TStringList;
    function ValidateFile(const FileName: string): Boolean;
    function SanitizeFileName(const FileName: string): string;
  public
    constructor Create(const SandboxPath: string);
    destructor Destroy; override;

    function ProcessUploadedFile(const SourceFile: string;
      out ResultFile: string): Boolean;

    property MaxFileSizeMB: Cardinal read FMaxFileSizeMB write FMaxFileSizeMB;
  end;

implementation

uses
  {$IFDEF WINDOWS}
  WindowsJobSandbox, FileSystemSandbox
  {$ENDIF}
  {$IFDEF LINUX}
  FirejailSandbox
  {$ENDIF};

constructor TFileProcessor.Create(const SandboxPath: string);  
begin
  inherited Create;
  FSandboxPath := SandboxPath;
  FMaxFileSizeMB := 10;
  FAllowedExtensions := TStringList.Create;

  // Extensions autorisées par défaut
  FAllowedExtensions.Add('.txt');
  FAllowedExtensions.Add('.pdf');
  FAllowedExtensions.Add('.jpg');
  FAllowedExtensions.Add('.png');

  // Créer le répertoire sandbox s'il n'existe pas
  if not DirectoryExists(FSandboxPath) then
    ForceDirectories(FSandboxPath);
end;

destructor TFileProcessor.Destroy;  
begin
  FAllowedExtensions.Free;
  inherited;
end;

function TFileProcessor.SanitizeFileName(const FileName: string): string;  
var
  i: Integer;
  BaseName, Ext: string;
begin
  // Extraire nom et extension
  BaseName := ChangeFileExt(ExtractFileName(FileName), '');
  Ext := ExtractFileExt(FileName);

  // Nettoyer le nom de base
  Result := '';
  for i := 1 to Length(BaseName) do
  begin
    if CharInSet(BaseName[i], ['a'..'z', 'A'..'Z', '0'..'9', '_', '-']) then
      Result := Result + BaseName[i]
    else
      Result := Result + '_';
  end;

  // Limiter la longueur
  if Length(Result) > 50 then
    Result := Copy(Result, 1, 50);

  // Ajouter timestamp pour unicité
  Result := Result + '_' + FormatDateTime('yyyymmddhhnnss', Now) + Ext;
end;

function TFileProcessor.ValidateFile(const FileName: string): Boolean;  
var
  FileSize: Int64;
  Ext: string;
begin
  Result := False;

  // Vérifier l'existence
  if not FileExists(FileName) then
  begin
    WriteLn('Erreur : Fichier introuvable');
    Exit;
  end;

  // Vérifier la taille
  FileSize := FileUtil.FileSize(FileName);
  if FileSize > FMaxFileSizeMB * 1024 * 1024 then
  begin
    WriteLn('Erreur : Fichier trop volumineux (max ', FMaxFileSizeMB, ' MB)');
    Exit;
  end;

  // Vérifier l'extension
  Ext := LowerCase(ExtractFileExt(FileName));
  if FAllowedExtensions.IndexOf(Ext) < 0 then
  begin
    WriteLn('Erreur : Extension non autorisée : ', Ext);
    Exit;
  end;

  Result := True;
end;

function TFileProcessor.ProcessUploadedFile(const SourceFile: string;
  out ResultFile: string): Boolean;
var
  SafeFileName: string;
  DestPath: string;
  {$IFDEF WINDOWS}
  JobSandbox: TJobSandbox;
  Limits: TResourceLimits;
  {$ENDIF}
  {$IFDEF LINUX}
  Sandbox: TFirejailSandbox;
  Args: TStringList;
  {$ENDIF}
begin
  Result := False;
  ResultFile := '';

  // Validation
  if not ValidateFile(SourceFile) then
    Exit;

  // Créer un nom de fichier sûr
  SafeFileName := SanitizeFileName(ExtractFileName(SourceFile));
  DestPath := IncludeTrailingPathDelimiter(FSandboxPath) + SafeFileName;

  try
    // Copier le fichier dans le sandbox
    if not CopyFile(PChar(SourceFile), PChar(DestPath), False) then
    begin
      WriteLn('Erreur : Impossible de copier le fichier');
      Exit;
    end;

    {$IFDEF WINDOWS}
    // Traiter avec limitations de ressources sous Windows
    Limits.MaxMemoryMB := 256;
    Limits.MaxCPUPercent := 50;
    Limits.MaxProcesses := 1;
    Limits.AllowNetworkAccess := False;

    JobSandbox := TJobSandbox.Create(Limits);
    try
      // Exemple : Scanner le fichier avec un antivirus
      // ou effectuer un traitement sécurisé
      if JobSandbox.CreateSandboxedProcess(
        Format('myprocessor.exe "%s"', [DestPath])) <> 0 then
      begin
        ResultFile := DestPath;
        Result := True;
      end;
    finally
      JobSandbox.Free;
    end;
    {$ENDIF}

    {$IFDEF LINUX}
    // Traiter avec Firejail sous Linux
    Sandbox := TFirejailSandbox.Create;
    Args := TStringList.Create;
    try
      Sandbox.Options.NoNetwork := True;
      Sandbox.Options.Private := False;
      Sandbox.Options.WhitelistPaths.Add(FSandboxPath);

      Args.Add(DestPath);

      if Sandbox.Execute('/usr/local/bin/myprocessor', Args) = 0 then
      begin
        ResultFile := DestPath;
        Result := True;
      end;
    finally
      Args.Free;
      Sandbox.Free;
    end;
    {$ENDIF}

  except
    on E: Exception do
    begin
      WriteLn('Erreur lors du traitement : ', E.Message);
      // Nettoyer en cas d'erreur
      if FileExists(DestPath) then
        DeleteFile(DestPath);
    end;
  end;
end;

end.
```

**Utilisation :**

```pascal
var
  Processor: TFileProcessor;
  UploadedFile, ProcessedFile: string;
begin
  Processor := TFileProcessor.Create('C:\MyApp\Sandbox\Uploads\');
  try
    Processor.MaxFileSizeMB := 5;

    UploadedFile := 'C:\Temp\user_upload.pdf';

    if Processor.ProcessUploadedFile(UploadedFile, ProcessedFile) then
    begin
      WriteLn('Fichier traité avec succès : ', ProcessedFile);
      // Maintenant on peut utiliser le fichier en toute sécurité
    end
    else
      WriteLn('Échec du traitement');
  finally
    Processor.Free;
  end;
end;
```

### Cas 2 : Exécution de scripts utilisateur

Permettre aux utilisateurs d'exécuter des scripts personnalisés de manière sécurisée.

```pascal
unit ScriptSandbox;

interface

uses
  Classes, SysUtils;

type
  TScriptLanguage = (slPython, slLua, slJavaScript);

  TScriptResult = record
    Success: Boolean;
    Output: string;
    Error: string;
    ExecutionTimeMS: Cardinal;
    MemoryUsedMB: Cardinal;
  end;

  TScriptSandbox = class
  private
    FTimeoutSeconds: Cardinal;
    FMaxMemoryMB: Cardinal;
    FWorkingDirectory: string;
    function GetInterpreterPath(Language: TScriptLanguage): string;
    function PrepareEnvironment(const ScriptContent: string): string;
    procedure CleanupEnvironment(const TempPath: string);
  public
    constructor Create;

    function ExecuteScript(const ScriptContent: string;
      Language: TScriptLanguage): TScriptResult;

    property TimeoutSeconds: Cardinal read FTimeoutSeconds write FTimeoutSeconds;
    property MaxMemoryMB: Cardinal read FMaxMemoryMB write FMaxMemoryMB;
  end;

implementation

uses
  Process, DateUtils
  {$IFDEF WINDOWS}, Windows{$ENDIF}
  {$IFDEF LINUX}, FirejailSandbox{$ENDIF};

constructor TScriptSandbox.Create;  
begin
  inherited Create;
  FTimeoutSeconds := 10;
  FMaxMemoryMB := 128;
  FWorkingDirectory := GetTempDir + 'scriptsandbox' + PathDelim;

  if not DirectoryExists(FWorkingDirectory) then
    ForceDirectories(FWorkingDirectory);
end;

function TScriptSandbox.GetInterpreterPath(Language: TScriptLanguage): string;  
begin
  case Language of
    slPython:
      {$IFDEF WINDOWS}
      Result := 'python.exe';
      {$ELSE}
      Result := '/usr/bin/python3';
      {$ENDIF}
    slLua:
      {$IFDEF WINDOWS}
      Result := 'lua.exe';
      {$ELSE}
      Result := '/usr/bin/lua';
      {$ENDIF}
    slJavaScript:
      {$IFDEF WINDOWS}
      Result := 'node.exe';
      {$ELSE}
      Result := '/usr/bin/node';
      {$ENDIF}
  else
    Result := '';
  end;
end;

function TScriptSandbox.PrepareEnvironment(const ScriptContent: string): string;  
var
  ScriptFile: TextFile;
  TempFileName: string;
begin
  // Créer un fichier temporaire unique
  TempFileName := FWorkingDirectory + FormatDateTime('yyyymmddhhnnsszzz', Now) + '.tmp';

  AssignFile(ScriptFile, TempFileName);
  try
    Rewrite(ScriptFile);
    Write(ScriptFile, ScriptContent);
  finally
    CloseFile(ScriptFile);
  end;

  Result := TempFileName;
end;

procedure TScriptSandbox.CleanupEnvironment(const TempPath: string);  
begin
  if FileExists(TempPath) then
    DeleteFile(TempPath);
end;

function TScriptSandbox.ExecuteScript(const ScriptContent: string;
  Language: TScriptLanguage): TScriptResult;
var
  ScriptFile: string;
  Process: TProcess;
  Output, Errors: TStringList;
  StartTime: TDateTime;
  InterpreterPath: string;
  {$IFDEF LINUX}
  Sandbox: TFirejailSandbox;
  Args: TStringList;
  {$ENDIF}
begin
  // Initialiser le résultat
  Result.Success := False;
  Result.Output := '';
  Result.Error := '';
  Result.ExecutionTimeMS := 0;
  Result.MemoryUsedMB := 0;

  InterpreterPath := GetInterpreterPath(Language);
  if InterpreterPath = '' then
  begin
    Result.Error := 'Interpréteur non trouvé pour ce langage';
    Exit;
  end;

  ScriptFile := PrepareEnvironment(ScriptContent);
  Output := TStringList.Create;
  Errors := TStringList.Create;

  try
    StartTime := Now;

    {$IFDEF WINDOWS}
    // Exécution Windows avec Job Object
    Process := TProcess.Create(nil);
    try
      Process.Executable := InterpreterPath;
      Process.Parameters.Add(ScriptFile);
      Process.Options := [poWaitOnExit, poUsePipes, poStderrToOutPut];

      // Note : Intégrer avec WindowsJobSandbox pour limites mémoire/CPU

      Process.Execute;

      // Collecter la sortie
      Output.LoadFromStream(Process.Output);

      Result.Success := (Process.ExitStatus = 0);
      Result.Output := Output.Text;
      Result.ExecutionTimeMS := MilliSecondsBetween(Now, StartTime);
    finally
      Process.Free;
    end;
    {$ENDIF}

    {$IFDEF LINUX}
    // Exécution Linux avec Firejail
    Sandbox := TFirejailSandbox.Create;
    Args := TStringList.Create;
    try
      // Configuration stricte
      Sandbox.Options.NoNetwork := True;
      Sandbox.Options.NoSound := True;
      Sandbox.Options.Private := True;
      Sandbox.Options.PrivateDev := True;

      // Autoriser uniquement le répertoire de travail
      Sandbox.Options.WhitelistPaths.Add(FWorkingDirectory);

      Args.Add(ScriptFile);

      if Sandbox.Execute(InterpreterPath, Args) = 0 then
      begin
        Result.Success := True;
        // Récupérer la sortie (nécessite de capturer stdout)
        Result.Output := 'Exécution réussie';
      end
      else
      begin
        Result.Error := 'Erreur d''exécution du script';
      end;

      Result.ExecutionTimeMS := MilliSecondsBetween(Now, StartTime);
    finally
      Args.Free;
      Sandbox.Free;
    end;
    {$ENDIF}

  except
    on E: Exception do
    begin
      Result.Success := False;
      Result.Error := E.Message;
    end;
  end;

  // Nettoyage
  CleanupEnvironment(ScriptFile);
  Output.Free;
  Errors.Free;
end;

end.
```

**Utilisation :**

```pascal
var
  Sandbox: TScriptSandbox;
  ScriptResult: TScriptResult;
  PythonScript: string;
begin
  PythonScript :=
    'import sys' + LineEnding +
    'print("Hello from sandboxed Python!")' + LineEnding +
    'print("Arguments:", sys.argv)' + LineEnding +
    'for i in range(5):' + LineEnding +
    '    print(f"Count: {i}")';

  Sandbox := TScriptSandbox.Create;
  try
    Sandbox.TimeoutSeconds := 5;
    Sandbox.MaxMemoryMB := 64;

    ScriptResult := Sandbox.ExecuteScript(PythonScript, slPython);

    if ScriptResult.Success then
    begin
      WriteLn('Script exécuté avec succès !');
      WriteLn('Sortie :');
      WriteLn(ScriptResult.Output);
      WriteLn('Temps : ', ScriptResult.ExecutionTimeMS, ' ms');
    end
    else
    begin
      WriteLn('Erreur :');
      WriteLn(ScriptResult.Error);
    end;
  finally
    Sandbox.Free;
  end;
end;
```

### Cas 3 : API de traitement d'images

Service web qui traite des images uploadées dans un environnement isolé.

```pascal
unit ImageProcessingAPI;

interface

uses
  Classes, SysUtils, fpjson, httpdefs, fphttpserver;

type
  TImageProcessingHandler = class
  private
    FSandboxPath: string;
    procedure ProcessImageRequest(ARequest: TRequest; AResponse: TResponse);
    function ValidateImage(const FilePath: string): Boolean;
    function ResizeImage(const InputPath, OutputPath: string;
      Width, Height: Integer): Boolean;
  public
    constructor Create(const SandboxPath: string);
    procedure HandleRequest(ARequest: TRequest; AResponse: TResponse);
  end;

implementation

uses
  Process, DockerSandbox;

constructor TImageProcessingHandler.Create(const SandboxPath: string);  
begin
  inherited Create;
  FSandboxPath := SandboxPath;

  if not DirectoryExists(FSandboxPath) then
    ForceDirectories(FSandboxPath);
end;

function TImageProcessingHandler.ValidateImage(const FilePath: string): Boolean;  
var
  Ext: string;
  FileSize: Int64;
begin
  Result := False;

  if not FileExists(FilePath) then
    Exit;

  // Vérifier l'extension
  Ext := LowerCase(ExtractFileExt(FilePath));
  if not (Ext = '.jpg') and not (Ext = '.jpeg') and
     not (Ext = '.png') and not (Ext = '.gif') then
    Exit;

  // Vérifier la taille (max 10 MB)
  FileSize := FileUtil.FileSize(FilePath);
  if FileSize > 10 * 1024 * 1024 then
    Exit;

  Result := True;
end;

function TImageProcessingHandler.ResizeImage(const InputPath, OutputPath: string;
  Width, Height: Integer): Boolean;
var
  Sandbox: TDockerSandbox;
  InputVolume, OutputVolume: string;
begin
  Result := False;

  // Utiliser Docker pour isoler le traitement d'image
  Sandbox := TDockerSandbox.Create('imagemagick:latest');
  try
    // Configuration sécurisée
    Sandbox.NetworkMode := 'none';
    Sandbox.MemoryLimit := '256m';
    Sandbox.CPULimit := '0.5';

    // Monter les volumes en lecture/écriture minimale
    InputVolume := ExtractFilePath(InputPath);
    OutputVolume := ExtractFilePath(OutputPath);

    Sandbox.AddVolume(InputVolume, '/input', True);  // Lecture seule
    Sandbox.AddVolume(OutputVolume, '/output', False); // Écriture

    // Commande ImageMagick pour redimensionner
    if Sandbox.Run(Format(
      'convert /input/%s -resize %dx%d /output/%s',
      [ExtractFileName(InputPath), Width, Height, ExtractFileName(OutputPath)]
    )) = 0 then
      Result := True;

  finally
    Sandbox.Free;
  end;
end;

procedure TImageProcessingHandler.ProcessImageRequest(ARequest: TRequest;
  AResponse: TResponse);
var
  InputFile, OutputFile: string;
  Width, Height: Integer;
  JSONResponse: TJSONObject;
begin
  JSONResponse := TJSONObject.Create;
  try
    try
      // Récupérer les paramètres
      InputFile := ARequest.QueryFields.Values['input'];
      Width := StrToIntDef(ARequest.QueryFields.Values['width'], 800);
      Height := StrToIntDef(ARequest.QueryFields.Values['height'], 600);

      // Validation
      if not ValidateImage(InputFile) then
      begin
        AResponse.Code := 400;
        JSONResponse.Add('error', 'Invalid image file');
        AResponse.Content := JSONResponse.AsJSON;
        Exit;
      end;

      // Générer le nom du fichier de sortie
      OutputFile := FSandboxPath +
        FormatDateTime('yyyymmddhhnnss', Now) + '_resized.jpg';

      // Traiter dans le sandbox
      if ResizeImage(InputFile, OutputFile, Width, Height) then
      begin
        AResponse.Code := 200;
        JSONResponse.Add('success', True);
        JSONResponse.Add('output', OutputFile);
      end
      else
      begin
        AResponse.Code := 500;
        JSONResponse.Add('error', 'Processing failed');
      end;

    except
      on E: Exception do
      begin
        AResponse.Code := 500;
        JSONResponse.Add('error', E.Message);
      end;
    end;

    AResponse.Content := JSONResponse.AsJSON;
    AResponse.ContentType := 'application/json';
  finally
    JSONResponse.Free;
  end;
end;

procedure TImageProcessingHandler.HandleRequest(ARequest: TRequest;
  AResponse: TResponse);
begin
  if ARequest.PathInfo = '/api/resize' then
    ProcessImageRequest(ARequest, AResponse)
  else
  begin
    AResponse.Code := 404;
    AResponse.Content := '{"error":"Not found"}';
  end;
end;

end.
```

## Dépannage et problèmes courants

### Problème 1 : Permissions insuffisantes

**Symptôme :** Le sandbox ne peut pas s'initialiser ou les processus échouent immédiatement.

**Solution :**

```pascal
procedure DiagnosePermissions;
{$IFDEF WINDOWS}
var
  Token: THandle;
  Elevation: TOKEN_ELEVATION;
  ReturnLength: DWORD;
{$ENDIF}
begin
  {$IFDEF WINDOWS}
  // Vérifier si on est administrateur
  if OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, Token) then
  begin
    try
      if GetTokenInformation(Token, TokenElevation, @Elevation,
         SizeOf(Elevation), ReturnLength) then
      begin
        if Elevation.TokenIsElevated = 0 then
          WriteLn('ATTENTION : Privilèges administrateur requis')
        else
          WriteLn('OK : Exécution avec privilèges élevés');
      end;
    finally
      CloseHandle(Token);
    end;
  end;
  {$ENDIF}

  {$IFDEF LINUX}
  // Vérifier si on est root
  if fpGetUID = 0 then
    WriteLn('ATTENTION : Exécution en tant que root (déconseillé)')
  else
    WriteLn('OK : Exécution en tant qu''utilisateur normal');
  {$ENDIF}
end;
```

### Problème 2 : Le processus sandboxé ne démarre pas

**Symptôme :** `CreateProcess` ou `Execute` échoue sans message d'erreur clair.

**Solution :**

```pascal
function DiagnoseProcessCreation(const Command: string): string;  
begin
  Result := '';

  // Vérifier l'existence de l'exécutable
  if not FileExists(Command) then
    Result := Result + 'Exécutable introuvable: ' + Command + LineEnding;

  // Vérifier les droits d'exécution
  {$IFDEF UNIX}
  if (fpAccess(PChar(Command), X_OK) <> 0) then
    Result := Result + 'Pas de droits d''exécution sur : ' + Command + LineEnding;
  {$ENDIF}

  // Vérifier les dépendances (DLL/SO)
  {$IFDEF WINDOWS}
  // Utiliser Dependency Walker ou équivalent
  Result := Result + 'Vérifier les DLL requises avec Dependency Walker' + LineEnding;
  {$ENDIF}

  {$IFDEF LINUX}
  // Utiliser ldd
  Result := Result + 'Exécuter : ldd ' + Command + LineEnding;
  {$ENDIF}

  if Result = '' then
    Result := 'Aucun problème détecté';
end;
```

### Problème 3 : Fuite de ressources

**Symptôme :** La mémoire ou les handles augmentent continuellement.

**Solution :**

```pascal
type
  TSandboxResourceMonitor = class
  private
    FProcessHandle: THandle;
    FPeakMemoryMB: Cardinal;
    FCurrentMemoryMB: Cardinal;
    procedure UpdateMemoryStats;
  public
    constructor Create(ProcessHandle: THandle);

    function GetCurrentMemory: Cardinal;
    function GetPeakMemory: Cardinal;
    function GetHandleCount: Cardinal;

    procedure LogResourceUsage;
  end;

procedure TSandboxResourceMonitor.UpdateMemoryStats;
{$IFDEF WINDOWS}
var
  MemCounters: TProcessMemoryCounters;
{$ENDIF}
begin
  {$IFDEF WINDOWS}
  MemCounters.cb := SizeOf(MemCounters);
  if GetProcessMemoryInfo(FProcessHandle, @MemCounters, SizeOf(MemCounters)) then
  begin
    FCurrentMemoryMB := MemCounters.WorkingSetSize div (1024 * 1024);
    FPeakMemoryMB := MemCounters.PeakWorkingSetSize div (1024 * 1024);
  end;
  {$ENDIF}
end;

procedure TSandboxResourceMonitor.LogResourceUsage;  
begin
  UpdateMemoryStats;
  WriteLn(Format('Mémoire courante: %d MB, Pic: %d MB, Handles: %d',
    [FCurrentMemoryMB, FPeakMemoryMB, GetHandleCount]));
end;
```

## Ressources et outils

### Windows

**Outils natifs :**
- **Process Monitor** : Surveiller les accès fichiers, registre, réseau
- **Process Explorer** : Visualiser les processus et leurs ressources
- **Windows Sandbox** : Environnement Windows jetable intégré (Windows 10 Pro+)

**Commandes utiles :**
```cmd
REM Créer un utilisateur limité  
net user sandboxuser /add  
net localgroup Users sandboxuser /add

REM Exécuter en tant qu'utilisateur différent  
runas /user:sandboxuser "program.exe"

REM Vérifier les privilèges  
whoami /priv
```

### Linux/Ubuntu

**Outils recommandés :**
- **Firejail** : `sudo apt install firejail`
- **AppArmor** : Profiles de sécurité
- **Seccomp-tools** : Pour créer des filtres seccomp
- **systemd-nspawn** : Conteneurs légers

**Commandes utiles :**
```bash
# Installer Firejail
sudo apt install firejail

# Exécuter avec isolation réseau
firejail --net=none firefox

# Créer un profil AppArmor
sudo aa-genprof /usr/bin/myapp

# Vérifier les capabilities
getcap /usr/bin/myapp

# Créer un utilisateur sandboxé
sudo useradd -m -s /bin/bash sandboxuser  
sudo su - sandboxuser
```

### Docker (Multi-plateforme)

```bash
# Construire une image de base sécurisée
FROM ubuntu:22.04  
RUN useradd -m -u 1000 sandbox  
USER sandbox  
WORKDIR /home/sandbox
# Pas de privilèges root

# Exécuter avec restrictions
docker run --rm \
  --memory="256m" \
  --cpus="0.5" \
  --network=none \
  --read-only \
  --security-opt=no-new-privileges \
  myapp:latest
```

## Conclusion

Le sandboxing et l'isolation sont des techniques essentielles pour sécuriser les applications FreePascal/Lazarus modernes. Les points clés à retenir :

### Principes fondamentaux

1. **Moindre privilège** : Ne donner que les permissions strictement nécessaires
2. **Défense en profondeur** : Combiner plusieurs couches de protection
3. **Isolation** : Séparer les composants sensibles des composants exposés
4. **Monitoring** : Surveiller et enregistrer toutes les actions

### Approche par plateforme

**Windows :**
- Tokens restreints et Jobs pour limitation de ressources
- Filesystem sandbox pour contrôle d'accès fichiers
- Windows Sandbox pour isolation complète

**Linux/Ubuntu :**
- Namespaces et cgroups pour isolation système
- Seccomp pour filtrage d'appels système
- Firejail pour solution clé en main
- Capabilities au lieu de root

**Multi-plateforme :**
- Docker pour isolation portable
- Abstraction via interfaces communes
- Tests sur toutes les plateformes cibles

### Mise en œuvre pratique

```pascal
// Pattern général recommandé
type
  TUnifiedSandbox = class
  private
    {$IFDEF WINDOWS}
    FWindowsSandbox: TWindowsJobSandbox;
    {$ENDIF}
    {$IFDEF LINUX}
    FLinuxSandbox: TFirejailSandbox;
    {$ENDIF}
  public
    constructor Create(Permissions: TSandboxPermissions);
    function Execute(const Command: string): Boolean;
  end;
```

### Recommandations finales

- **Commencez simple** : Validations et vérifications basiques d'abord
- **Testez rigoureusement** : Tests de sécurité et cas limites
- **Documentez** : Configuration et limitations pour les utilisateurs
- **Monitorer** : Logging et alertes en production
- **Mettez à jour** : Suivre les évolutions des technologies de sandboxing

Le sandboxing n'est pas une solution miracle, mais combiné avec d'autres pratiques de sécurité (validation d'entrées, authentification, chiffrement), il constitue une défense robuste contre les compromissions et les abus.

**Note importante** : Le sandboxing peut impacter les performances. Mesurez toujours cet impact et ajustez le niveau de protection en fonction du contexte d'utilisation et des menaces réelles.

⏭️ [Audit et conformité GDPR](/17-securite-cryptographie/10-audit-conformite-gdpr.md)
