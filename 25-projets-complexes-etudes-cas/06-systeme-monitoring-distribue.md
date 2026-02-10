🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 25.6 Système de monitoring distribué

## Introduction

Un système de monitoring distribué permet de surveiller en temps réel l'état de santé, les performances et la disponibilité de multiples serveurs, applications et services répartis sur différentes machines. Dans ce chapitre, nous allons concevoir et développer un système complet de monitoring avec FreePascal/Lazarus, fonctionnant aussi bien sur Windows que sur Ubuntu.

### Qu'est-ce qu'un système de monitoring distribué ?

Un système de monitoring distribué se compose généralement de plusieurs éléments :

- **Des agents** : petits programmes installés sur chaque machine à surveiller qui collectent des métriques locales
- **Un serveur central** : qui reçoit, agrège et stocke les données de tous les agents
- **Une interface de visualisation** : qui permet de consulter les métriques en temps réel et l'historique
- **Un système d'alertes** : qui notifie les administrateurs en cas de problème

## Architecture du système

### Vue d'ensemble

Notre système de monitoring sera composé de trois parties principales :

1. **MonitorAgent** : agent léger installé sur chaque machine surveillée
2. **MonitorServer** : serveur central qui collecte et stocke les données
3. **MonitorConsole** : interface graphique de visualisation et d'administration

```
┌─────────────────┐         ┌─────────────────┐         ┌───────────────────┐
│  Machine 1      │         │  Machine 2      │         │  Machine N        │
│  ┌───────────┐  │         │  ┌───────────┐  │         │  ┌─────────────┐  │
│  │MonitorAgent◄─┼─────────┼──►MonitorAgent◄─┼─────────┼──►MonitorAgent │  │
│  └───────────┘  │         │  └───────────┘  │         │  └─────────────┘  │
└────────┬────────┘         └────────┬────────┘         └────────┬──────────┘
         │                           │                           │
         │          Réseau TCP/IP ou HTTP/REST                   │
         │                           │                           │
         └───────────────────────────┼───────────────────────────┘
                                     │
                            ┌────────▼────────┐
                            │  MonitorServer  │
                            │   (Collecteur)  │
                            │   + Base de     │
                            │     données     │
                            └────────┬────────┘
                                     │
                            ┌────────▼────────┐
                            │ MonitorConsole  │
                            │   (Interface    │
                            │   graphique)    │
                            └─────────────────┘
```

### Choix techniques

#### Protocole de communication

Nous utiliserons deux approches complémentaires :

1. **Protocole binaire personnalisé sur TCP** : pour la communication rapide entre agents et serveur
2. **API REST HTTP/JSON** : pour l'interface web et les intégrations tierces

#### Stockage des données

Pour le stockage, nous utiliserons :

- **SQLite** : pour un déploiement simple et autonome
- **PostgreSQL** : pour les installations en production nécessitant haute performance

#### Bibliothèques FreePascal

- **Synapse** : pour la communication réseau TCP/UDP
- **fpWeb** ou **Brook Framework** : pour l'API REST
- **SQLdb** : pour l'accès aux bases de données
- **BGRABitmap** et **TAChart** : pour les graphiques de monitoring

## Partie 1 : L'Agent de monitoring

### Conception de l'agent

L'agent doit être léger, peu gourmand en ressources, et capable de collecter diverses métriques système.

#### Métriques à collecter

##### Métriques système de base

```pascal
type
  TSystemMetrics = record
    Timestamp: TDateTime;
    CPUUsage: Double;           // Utilisation CPU en %
    MemoryTotal: Int64;         // Mémoire totale en octets
    MemoryUsed: Int64;          // Mémoire utilisée en octets
    MemoryAvailable: Int64;     // Mémoire disponible en octets
    DiskTotal: Int64;           // Espace disque total
    DiskUsed: Int64;            // Espace disque utilisé
    DiskFree: Int64;            // Espace disque libre
    NetworkBytesReceived: Int64; // Octets reçus
    NetworkBytesSent: Int64;     // Octets envoyés
    ProcessCount: Integer;       // Nombre de processus
    ThreadCount: Integer;        // Nombre de threads
    UptimeSeconds: Int64;        // Temps de fonctionnement
  end;
```

##### Métriques applicatives

```pascal
type
  TApplicationMetrics = record
    ApplicationName: string;
    ProcessID: Integer;
    CPUUsage: Double;
    MemoryUsage: Int64;
    ThreadCount: Integer;
    IsRunning: Boolean;
  end;
```

### Collecte multi-plateforme des métriques

#### Collecte CPU - Approche multi-plateforme

```pascal
unit MonitorMetrics;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils
  {$IFDEF WINDOWS}
  , Windows
  {$ENDIF}
  {$IFDEF UNIX}
  , BaseUnix, Unix
  {$ENDIF};

type
  TMetricsCollector = class
  private
    {$IFDEF UNIX}
    FLastCPUTime: Int64;
    FLastSystemTime: Int64;
    {$ENDIF}
    function GetCPUUsageWindows: Double;
    function GetCPUUsageLinux: Double;
  public
    function GetCPUUsage: Double;
    function GetMemoryInfo(out Total, Used, Available: Int64): Boolean;
    function GetDiskInfo(const Path: string; out Total, Used, Free: Int64): Boolean;
    function GetNetworkStats(out BytesReceived, BytesSent: Int64): Boolean;
    function GetSystemUptime: Int64;
  end;

implementation

{$IFDEF WINDOWS}
function TMetricsCollector.GetCPUUsageWindows: Double;
var
  SystemTimes: TSystemTimes;
  IdleTime, KernelTime, UserTime: TFileTime;
  SysIdle, SysKernel, SysUser: Int64;
begin
  Result := 0.0;

  if GetSystemTimes(IdleTime, KernelTime, UserTime) then
  begin
    // Conversion des FILETIME en Int64
    SysIdle := Int64(IdleTime);
    SysKernel := Int64(KernelTime);
    SysUser := Int64(UserTime);

    // Calcul du pourcentage d'utilisation CPU
    // Cette approche nécessite deux mesures successives
    // pour calculer la différence
    Result := 0.0; // Implémentation simplifiée
  end;
end;
{$ENDIF}

{$IFDEF UNIX}
function TMetricsCollector.GetCPUUsageLinux: Double;
var
  F: TextFile;
  Line: string;
  User, Nice, System, Idle: Int64;
  TotalTime, IdleTime: Int64;
begin
  Result := 0.0;

  try
    AssignFile(F, '/proc/stat');
    Reset(F);
    ReadLn(F, Line);
    CloseFile(F);

    // Parsing de la ligne "cpu" du fichier /proc/stat
    // Format: cpu user nice system idle iowait irq softirq
    if Pos('cpu', Line) = 1 then
    begin
      // Extraction des valeurs
      // Implémentation simplifiée
      User := 0;
      Nice := 0;
      System := 0;
      Idle := 0;

      TotalTime := User + Nice + System + Idle;
      IdleTime := Idle;

      if TotalTime > 0 then
        Result := ((TotalTime - IdleTime) / TotalTime) * 100.0;
    end;
  except
    Result := 0.0;
  end;
end;
{$ENDIF}

function TMetricsCollector.GetCPUUsage: Double;
begin
  {$IFDEF WINDOWS}
  Result := GetCPUUsageWindows;
  {$ENDIF}
  {$IFDEF UNIX}
  Result := GetCPUUsageLinux;
  {$ENDIF}
end;

function TMetricsCollector.GetMemoryInfo(out Total, Used, Available: Int64): Boolean;
{$IFDEF WINDOWS}
var
  MemStatus: TMemoryStatusEx;
{$ENDIF}
{$IFDEF UNIX}
var
  F: TextFile;
  Line: string;
  Key, Value: string;
  MemTotal, MemFree, MemAvailable: Int64;
{$ENDIF}
begin
  Result := False;
  Total := 0;
  Used := 0;
  Available := 0;

  {$IFDEF WINDOWS}
  MemStatus.dwLength := SizeOf(MemStatus);
  if GlobalMemoryStatusEx(MemStatus) then
  begin
    Total := MemStatus.ullTotalPhys;
    Available := MemStatus.ullAvailPhys;
    Used := Total - Available;
    Result := True;
  end;
  {$ENDIF}

  {$IFDEF UNIX}
  try
    AssignFile(F, '/proc/meminfo');
    Reset(F);

    MemTotal := 0;
    MemFree := 0;
    MemAvailable := 0;

    while not Eof(F) do
    begin
      ReadLn(F, Line);

      // Parsing des lignes importantes
      if Pos('MemTotal:', Line) = 1 then
        MemTotal := StrToInt64Def(ExtractNumbers(Line), 0) * 1024
      else if Pos('MemFree:', Line) = 1 then
        MemFree := StrToInt64Def(ExtractNumbers(Line), 0) * 1024
      else if Pos('MemAvailable:', Line) = 1 then
        MemAvailable := StrToInt64Def(ExtractNumbers(Line), 0) * 1024;
    end;

    CloseFile(F);

    Total := MemTotal;
    Available := MemAvailable;
    Used := Total - Available;
    Result := True;
  except
    Result := False;
  end;
  {$ENDIF}
end;

{$IFDEF UNIX}
function TMetricsCollector.GetSystemUptime: Int64;
var
  F: TextFile;
  Line: string;
  UptimeStr: string;
begin
  Result := 0;
  try
    AssignFile(F, '/proc/uptime');
    Reset(F);
    ReadLn(F, Line);
    CloseFile(F);

    // Premier nombre = uptime en secondes
    UptimeStr := Copy(Line, 1, Pos(' ', Line) - 1);
    Result := Trunc(StrToFloatDef(UptimeStr, 0.0));
  except
    Result := 0;
  end;
end;
{$ENDIF}

{$IFDEF WINDOWS}
function TMetricsCollector.GetSystemUptime: Int64;
begin
  Result := GetTickCount64 div 1000; // Convertir ms en secondes
end;
{$ENDIF}

end.
```

### Communication réseau de l'agent

#### Protocole binaire personnalisé

Nous allons définir un protocole simple et efficace pour la transmission des métriques.

```pascal
unit MonitorProtocol;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  PROTOCOL_VERSION = 1;

  // Types de messages
  MSG_HEARTBEAT = 1;        // Agent signale qu'il est vivant
  MSG_METRICS = 2;          // Envoi de métriques
  MSG_ALERT = 3;            // Alerte système
  MSG_REGISTER = 4;         // Enregistrement de l'agent
  MSG_ACK = 5;              // Accusé de réception

type
  TMessageHeader = packed record
    Version: Byte;
    MessageType: Byte;
    DataLength: Word;
    Timestamp: Int64;
    AgentID: array[0..15] of Char; // UUID de l'agent
  end;

  TMonitorMessage = class
  private
    FHeader: TMessageHeader;
    FData: TBytes;
  public
    constructor Create;
    procedure SetData(const AData: TBytes);
    function Serialize: TBytes;
    procedure Deserialize(const ABytes: TBytes);

    property Header: TMessageHeader read FHeader write FHeader;
    property Data: TBytes read FData;
  end;

implementation

constructor TMonitorMessage.Create;
begin
  inherited Create;
  FillChar(FHeader, SizeOf(FHeader), 0);
  FHeader.Version := PROTOCOL_VERSION;
  FHeader.Timestamp := DateTimeToUnix(Now);
end;

procedure TMonitorMessage.SetData(const AData: TBytes);
begin
  FData := Copy(AData);
  FHeader.DataLength := Length(FData);
end;

function TMonitorMessage.Serialize: TBytes;
var
  TotalSize: Integer;
begin
  TotalSize := SizeOf(TMessageHeader) + Length(FData);
  SetLength(Result, TotalSize);

  // Copie du header
  Move(FHeader, Result[0], SizeOf(TMessageHeader));

  // Copie des données
  if Length(FData) > 0 then
    Move(FData[0], Result[SizeOf(TMessageHeader)], Length(FData));
end;

procedure TMonitorMessage.Deserialize(const ABytes: TBytes);
begin
  if Length(ABytes) < SizeOf(TMessageHeader) then
    raise Exception.Create('Message trop court');

  // Extraction du header
  Move(ABytes[0], FHeader, SizeOf(TMessageHeader));

  // Extraction des données
  if FHeader.DataLength > 0 then
  begin
    SetLength(FData, FHeader.DataLength);
    Move(ABytes[SizeOf(TMessageHeader)], FData[0], FHeader.DataLength);
  end;
end;

end.
```

#### Client réseau de l'agent

```pascal
unit MonitorAgentClient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, blcksock, synsock, MonitorProtocol;

type
  TMonitorAgentClient = class
  private
    FSocket: TTCPBlockSocket;
    FServerHost: string;
    FServerPort: Integer;
    FAgentID: string;
    FConnected: Boolean;

    function GenerateAgentID: string;
  public
    constructor Create(const AHost: string; APort: Integer);
    destructor Destroy; override;

    function Connect: Boolean;
    procedure Disconnect;
    function SendMetrics(const AMetrics: TBytes): Boolean;
    function SendHeartbeat: Boolean;

    property AgentID: string read FAgentID;
    property Connected: Boolean read FConnected;
  end;

implementation

constructor TMonitorAgentClient.Create(const AHost: string; APort: Integer);
begin
  inherited Create;
  FSocket := TTCPBlockSocket.Create;
  FServerHost := AHost;
  FServerPort := APort;
  FConnected := False;
  FAgentID := GenerateAgentID;
end;

destructor TMonitorAgentClient.Destroy;
begin
  Disconnect;
  FSocket.Free;
  inherited Destroy;
end;

function TMonitorAgentClient.GenerateAgentID: string;
var
  GUID: TGUID;
begin
  CreateGUID(GUID);
  Result := GUIDToString(GUID);
end;

function TMonitorAgentClient.Connect: Boolean;
begin
  FSocket.Connect(FServerHost, IntToStr(FServerPort));
  Result := FSocket.LastError = 0;
  FConnected := Result;

  if Result then
  begin
    // Envoyer un message d'enregistrement
    // Implémentation simplifiée
  end;
end;

procedure TMonitorAgentClient.Disconnect;
begin
  if FConnected then
  begin
    FSocket.CloseSocket;
    FConnected := False;
  end;
end;

function TMonitorAgentClient.SendMetrics(const AMetrics: TBytes): Boolean;
var
  Msg: TMonitorMessage;
  Data: TBytes;
begin
  Result := False;

  if not FConnected then
    Exit;

  try
    Msg := TMonitorMessage.Create;
    try
      Msg.Header.MessageType := MSG_METRICS;
      Move(FAgentID[1], Msg.Header.AgentID[0], Min(Length(FAgentID), 16));
      Msg.SetData(AMetrics);

      Data := Msg.Serialize;
      FSocket.SendBuffer(@Data[0], Length(Data));

      Result := FSocket.LastError = 0;
    finally
      Msg.Free;
    end;
  except
    Result := False;
  end;
end;

function TMonitorAgentClient.SendHeartbeat: Boolean;
var
  Msg: TMonitorMessage;
  Data: TBytes;
begin
  Result := False;

  if not FConnected then
    Exit;

  try
    Msg := TMonitorMessage.Create;
    try
      Msg.Header.MessageType := MSG_HEARTBEAT;
      Move(FAgentID[1], Msg.Header.AgentID[0], Min(Length(FAgentID), 16));

      Data := Msg.Serialize;
      FSocket.SendBuffer(@Data[0], Length(Data));

      Result := FSocket.LastError = 0;
    finally
      Msg.Free;
    end;
  except
    Result := False;
  end;
end;

end.
```

### Service système pour l'agent

#### Service Windows

```pascal
unit MonitorAgentService;

{$mode objfpc}{$H+}
{$IFDEF WINDOWS}

interface

uses
  Classes, SysUtils, Windows, JwaWinSvc;

type
  TMonitorAgentService = class
  private
    FServiceName: string;
    FServiceStatus: TServiceStatus;
    FServiceStatusHandle: SERVICE_STATUS_HANDLE;
    FStopEvent: THandle;

    procedure ReportStatus(dwCurrentState, dwWin32ExitCode, dwWaitHint: DWORD);
    procedure ServiceMain(argc: DWORD; argv: PLPSTR); stdcall;
    procedure ServiceCtrlHandler(dwControl: DWORD); stdcall;
  public
    constructor Create(const AServiceName: string);
    destructor Destroy; override;

    procedure Run;
    procedure Stop;

    class function Install: Boolean;
    class function Uninstall: Boolean;
  end;

implementation

constructor TMonitorAgentService.Create(const AServiceName: string);
begin
  inherited Create;
  FServiceName := AServiceName;
  FStopEvent := CreateEvent(nil, True, False, nil);
end;

destructor TMonitorAgentService.Destroy;
begin
  if FStopEvent <> 0 then
    CloseHandle(FStopEvent);
  inherited Destroy;
end;

procedure TMonitorAgentService.ReportStatus(dwCurrentState, dwWin32ExitCode, dwWaitHint: DWORD);
begin
  FServiceStatus.dwCurrentState := dwCurrentState;
  FServiceStatus.dwWin32ExitCode := dwWin32ExitCode;
  FServiceStatus.dwWaitHint := dwWaitHint;

  if dwCurrentState = SERVICE_START_PENDING then
    FServiceStatus.dwControlsAccepted := 0
  else
    FServiceStatus.dwControlsAccepted := SERVICE_ACCEPT_STOP;

  SetServiceStatus(FServiceStatusHandle, FServiceStatus);
end;

procedure TMonitorAgentService.ServiceMain(argc: DWORD; argv: PLPSTR);
begin
  // Enregistrer le handler de contrôle
  FServiceStatusHandle := RegisterServiceCtrlHandler(
    PChar(FServiceName),
    @ServiceCtrlHandler
  );

  if FServiceStatusHandle = 0 then
    Exit;

  // Initialiser le statut du service
  FillChar(FServiceStatus, SizeOf(FServiceStatus), 0);
  FServiceStatus.dwServiceType := SERVICE_WIN32_OWN_PROCESS;
  FServiceStatus.dwServiceSpecificExitCode := 0;

  // Reporter le démarrage
  ReportStatus(SERVICE_START_PENDING, NO_ERROR, 3000);

  // Logique principale du service
  // Boucle de monitoring
  ReportStatus(SERVICE_RUNNING, NO_ERROR, 0);

  WaitForSingleObject(FStopEvent, INFINITE);

  ReportStatus(SERVICE_STOPPED, NO_ERROR, 0);
end;

procedure TMonitorAgentService.ServiceCtrlHandler(dwControl: DWORD);
begin
  case dwControl of
    SERVICE_CONTROL_STOP:
      begin
        ReportStatus(SERVICE_STOP_PENDING, NO_ERROR, 0);
        SetEvent(FStopEvent);
      end;
    SERVICE_CONTROL_INTERROGATE:
      ReportStatus(FServiceStatus.dwCurrentState, NO_ERROR, 0);
  end;
end;

class function TMonitorAgentService.Install: Boolean;
var
  SCManager, Service: SC_HANDLE;
  ServicePath: string;
begin
  Result := False;

  ServicePath := ParamStr(0);

  SCManager := OpenSCManager(nil, nil, SC_MANAGER_ALL_ACCESS);
  if SCManager = 0 then
    Exit;

  try
    Service := CreateService(
      SCManager,
      'MonitorAgent',
      'Monitor Agent Service',
      SERVICE_ALL_ACCESS,
      SERVICE_WIN32_OWN_PROCESS,
      SERVICE_AUTO_START,
      SERVICE_ERROR_NORMAL,
      PChar(ServicePath),
      nil, nil, nil, nil, nil
    );

    Result := Service <> 0;

    if Service <> 0 then
      CloseServiceHandle(Service);
  finally
    CloseServiceHandle(SCManager);
  end;
end;

class function TMonitorAgentService.Uninstall: Boolean;
var
  SCManager, Service: SC_HANDLE;
begin
  Result := False;

  SCManager := OpenSCManager(nil, nil, SC_MANAGER_ALL_ACCESS);
  if SCManager = 0 then
    Exit;

  try
    Service := OpenService(SCManager, 'MonitorAgent', SERVICE_ALL_ACCESS);
    if Service = 0 then
      Exit;

    try
      Result := DeleteService(Service);
    finally
      CloseServiceHandle(Service);
    end;
  finally
    CloseServiceHandle(SCManager);
  end;
end;

{$ENDIF}
end.
```

#### Service Linux (systemd)

Pour Linux, nous créons un démon qui peut être géré par systemd.

```pascal
unit MonitorAgentDaemon;

{$mode objfpc}{$H+}
{$IFDEF UNIX}

interface

uses
  Classes, SysUtils, BaseUnix, Unix;

type
  TMonitorAgentDaemon = class
  private
    FRunning: Boolean;
    FPidFile: string;

    procedure Daemonize;
    procedure WritePidFile;
    procedure RemovePidFile;
    procedure SignalHandler(Signal: Integer);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Run;
    procedure Stop;
  end;

implementation

constructor TMonitorAgentDaemon.Create;
begin
  inherited Create;
  FRunning := False;
  FPidFile := '/var/run/monitoragent.pid';
end;

destructor TMonitorAgentDaemon.Destroy;
begin
  RemovePidFile;
  inherited Destroy;
end;

procedure TMonitorAgentDaemon.Daemonize;
var
  pid: TPid;
begin
  // Premier fork
  pid := FpFork;
  if pid < 0 then
    raise Exception.Create('Échec du fork');
  if pid > 0 then
    Halt(0); // Parent se termine

  // Créer une nouvelle session
  if FpSetsid < 0 then
    raise Exception.Create('Échec de setsid');

  // Second fork
  pid := FpFork;
  if pid < 0 then
    raise Exception.Create('Échec du second fork');
  if pid > 0 then
    Halt(0);

  // Changer le répertoire de travail
  FpChdir('/');

  // Définir le umask
  FpUmask(0);

  // Fermer les descripteurs de fichiers standard
  FpClose(STDIN_FILENO);
  FpClose(STDOUT_FILENO);
  FpClose(STDERR_FILENO);
end;

procedure TMonitorAgentDaemon.WritePidFile;
var
  F: TextFile;
begin
  try
    AssignFile(F, FPidFile);
    Rewrite(F);
    WriteLn(F, FpGetpid);
    CloseFile(F);
  except
    // Erreur d'écriture du fichier PID
  end;
end;

procedure TMonitorAgentDaemon.RemovePidFile;
begin
  if FileExists(FPidFile) then
    DeleteFile(FPidFile);
end;

procedure TMonitorAgentDaemon.SignalHandler(Signal: Integer);
begin
  case Signal of
    SIGTERM, SIGINT:
      begin
        FRunning := False;
      end;
    SIGHUP:
      begin
        // Recharger la configuration
      end;
  end;
end;

procedure TMonitorAgentDaemon.Run;
begin
  Daemonize;
  WritePidFile;

  // Installer les gestionnaires de signaux
  FpSignal(SIGTERM, @SignalHandler);
  FpSignal(SIGINT, @SignalHandler);
  FpSignal(SIGHUP, @SignalHandler);

  FRunning := True;

  // Boucle principale du démon
  while FRunning do
  begin
    // Logique de monitoring
    Sleep(1000);
  end;
end;

procedure TMonitorAgentDaemon.Stop;
begin
  FRunning := False;
end;

{$ENDIF}
end.
```

Fichier de configuration systemd (`/etc/systemd/system/monitoragent.service`) :

```ini
[Unit]
Description=Monitor Agent Service
After=network.target

[Service]
Type=forking
ExecStart=/usr/local/bin/monitoragent
PIDFile=/var/run/monitoragent.pid
Restart=on-failure
RestartSec=10

[Install]
WantedBy=multi-user.target
```

## Partie 2 : Le Serveur de monitoring

### Architecture du serveur

Le serveur doit gérer simultanément plusieurs agents, stocker les données et exposer une API pour l'interface de visualisation.

#### Serveur TCP multi-thread

```pascal
unit MonitorServer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, blcksock, synsock, MonitorProtocol, fgl;

type
  TAgentInfo = class
  public
    AgentID: string;
    Hostname: string;
    IPAddress: string;
    LastSeen: TDateTime;
    IsOnline: Boolean;
  end;

  TAgentList = specialize TFPGObjectList<TAgentInfo>;

  TClientThread = class(TThread)
  private
    FSocket: TTCPBlockSocket;
    FServer: TObject; // Référence au serveur principal
  protected
    procedure Execute; override;
  public
    constructor Create(ASocket: TSocket; AServer: TObject);
    destructor Destroy; override;
  end;

  TMonitorServer = class
  private
    FListenSocket: TTCPBlockSocket;
    FPort: Integer;
    FRunning: Boolean;
    FAgents: TAgentList;
    FAgentsLock: TRTLCriticalSection;

    procedure HandleMessage(const Msg: TMonitorMessage; ClientIP: string);
    procedure StoreMetrics(const AgentID: string; const Data: TBytes);
  public
    constructor Create(APort: Integer);
    destructor Destroy; override;

    procedure Start;
    procedure Stop;

    function GetAgentList: TAgentList;
    procedure RegisterAgent(const AgentID, Hostname, IPAddress: string);
    procedure UpdateAgentStatus(const AgentID: string; IsOnline: Boolean);
  end;

implementation

{ TClientThread }

constructor TClientThread.Create(ASocket: TSocket; AServer: TObject);
begin
  inherited Create(True);
  FSocket := TTCPBlockSocket.Create;
  FSocket.Socket := ASocket;
  FServer := AServer;
  FreeOnTerminate := True;
  Resume;
end;

destructor TClientThread.Destroy;
begin
  FSocket.Free;
  inherited Destroy;
end;

procedure TClientThread.Execute;
var
  Buffer: array[0..4095] of Byte;
  BytesRead: Integer;
  Msg: TMonitorMessage;
begin
  while not Terminated do
  begin
    BytesRead := FSocket.RecvBufferEx(@Buffer, SizeOf(Buffer), 5000);

    if BytesRead <= 0 then
      Break;

    try
      Msg := TMonitorMessage.Create;
      try
        Msg.Deserialize(Copy(Buffer, 0, BytesRead));
        TMonitorServer(FServer).HandleMessage(Msg, FSocket.GetRemoteSinIP);
      finally
        Msg.Free;
      end;
    except
      // Erreur de traitement du message
      Break;
    end;
  end;
end;

{ TMonitorServer }

constructor TMonitorServer.Create(APort: Integer);
begin
  inherited Create;
  FListenSocket := TTCPBlockSocket.Create;
  FPort := APort;
  FRunning := False;
  FAgents := TAgentList.Create(True);
  InitCriticalSection(FAgentsLock);
end;

destructor TMonitorServer.Destroy;
begin
  Stop;
  DoneCriticalSection(FAgentsLock);
  FAgents.Free;
  FListenSocket.Free;
  inherited Destroy;
end;

procedure TMonitorServer.Start;
var
  ClientSocket: TSocket;
begin
  FListenSocket.CreateSocket;
  FListenSocket.SetLinger(True, 10);
  FListenSocket.Bind('0.0.0.0', IntToStr(FPort));
  FListenSocket.Listen;

  FRunning := True;

  WriteLn('Serveur de monitoring démarré sur le port ', FPort);

  while FRunning do
  begin
    if FListenSocket.CanRead(1000) then
    begin
      ClientSocket := FListenSocket.Accept;
      if FListenSocket.LastError = 0 then
      begin
        WriteLn('Nouvelle connexion depuis : ', FListenSocket.GetRemoteSinIP);
        TClientThread.Create(ClientSocket, Self);
      end;
    end;
  end;
end;

procedure TMonitorServer.Stop;
begin
  FRunning := False;
  FListenSocket.CloseSocket;
end;

procedure TMonitorServer.HandleMessage(const Msg: TMonitorMessage; ClientIP: string);
var
  AgentID: string;
begin
  SetLength(AgentID, 16);
  Move(Msg.Header.AgentID[0], AgentID[1], 16);
  AgentID := Trim(AgentID);

  case Msg.Header.MessageType of
    MSG_HEARTBEAT:
      begin
        WriteLn('Heartbeat reçu de : ', AgentID);
        UpdateAgentStatus(AgentID, True);
      end;

    MSG_METRICS:
      begin
        WriteLn('Métriques reçues de : ', AgentID);
        StoreMetrics(AgentID, Msg.Data);
        UpdateAgentStatus(AgentID, True);
      end;

    MSG_REGISTER:
      begin
        WriteLn('Enregistrement de l''agent : ', AgentID);
        RegisterAgent(AgentID, '', ClientIP);
      end;
  end;
end;

procedure TMonitorServer.StoreMetrics(const AgentID: string; const Data: TBytes);
begin
  // Ici, on stockerait les métriques dans la base de données
  // Implémentation avec SQLdb dans la section suivante
end;

function TMonitorServer.GetAgentList: TAgentList;
begin
  EnterCriticalSection(FAgentsLock);
  try
    Result := FAgents;
  finally
    LeaveCriticalSection(FAgentsLock);
  end;
end;

procedure TMonitorServer.RegisterAgent(const AgentID, Hostname, IPAddress: string);
var
  Agent: TAgentInfo;
  i: Integer;
  Found: Boolean;
begin
  EnterCriticalSection(FAgentsLock);
  try
    Found := False;
    for i := 0 to FAgents.Count - 1 do
    begin
      if FAgents[i].AgentID = AgentID then
      begin
        Found := True;
        FAgents[i].LastSeen := Now;
        FAgents[i].IsOnline := True;
        Break;
      end;
    end;

    if not Found then
    begin
      Agent := TAgentInfo.Create;
      Agent.AgentID := AgentID;
      Agent.Hostname := Hostname;
      Agent.IPAddress := IPAddress;
      Agent.LastSeen := Now;
      Agent.IsOnline := True;
      FAgents.Add(Agent);
    end;
  finally
    LeaveCriticalSection(FAgentsLock);
  end;
end;

procedure TMonitorServer.UpdateAgentStatus(const AgentID: string; IsOnline: Boolean);
var
  i: Integer;
begin
  EnterCriticalSection(FAgentsLock);
  try
    for i := 0 to FAgents.Count - 1 do
    begin
      if FAgents[i].AgentID = AgentID then
      begin
        FAgents[i].LastSeen := Now;
        FAgents[i].IsOnline := IsOnline;
        Break;
      end;
    end;
  finally
    LeaveCriticalSection(FAgentsLock);
  end;
end;

end.
```

### Stockage des données avec SQLdb

#### Structure de la base de données

Nous allons créer un schéma de base de données pour stocker les métriques collectées.

```sql
-- Table des agents
CREATE TABLE agents (
    agent_id VARCHAR(36) PRIMARY KEY,
    hostname VARCHAR(255),
    ip_address VARCHAR(45),
    os_type VARCHAR(50),
    os_version VARCHAR(100),
    first_seen TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    last_seen TIMESTAMP,
    is_online BOOLEAN DEFAULT TRUE
);

-- Table des métriques système
CREATE TABLE system_metrics (
    id SERIAL PRIMARY KEY,
    agent_id VARCHAR(36) REFERENCES agents(agent_id),
    timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    cpu_usage DECIMAL(5,2),
    memory_total BIGINT,
    memory_used BIGINT,
    memory_available BIGINT,
    disk_total BIGINT,
    disk_used BIGINT,
    disk_free BIGINT,
    network_bytes_received BIGINT,
    network_bytes_sent BIGINT,
    process_count INTEGER,
    uptime_seconds BIGINT
);

-- Index pour optimiser les requêtes
CREATE INDEX idx_metrics_agent_timestamp ON system_metrics(agent_id, timestamp DESC);
CREATE INDEX idx_metrics_timestamp ON system_metrics(timestamp DESC);

-- Table des alertes
CREATE TABLE alerts (
    id SERIAL PRIMARY KEY,
    agent_id VARCHAR(36) REFERENCES agents(agent_id),
    alert_type VARCHAR(50),
    severity VARCHAR(20),
    message TEXT,
    threshold_value DECIMAL,
    current_value DECIMAL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    resolved_at TIMESTAMP,
    is_resolved BOOLEAN DEFAULT FALSE
);

-- Table de configuration des seuils
CREATE TABLE alert_thresholds (
    id SERIAL PRIMARY KEY,
    metric_name VARCHAR(100),
    warning_threshold DECIMAL,
    critical_threshold DECIMAL,
    check_interval_seconds INTEGER DEFAULT 60,
    enabled BOOLEAN DEFAULT TRUE
);
```

#### Classe de gestion de la base de données

```pascal
unit MonitorDatabase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb,
  {$IFDEF WINDOWS}
  sqlite3conn, pqconnection
  {$ELSE}
  sqlite3conn, pqconnection
  {$ENDIF};

type
  TMetricRecord = record
    AgentID: string;
    Timestamp: TDateTime;
    CPUUsage: Double;
    MemoryTotal: Int64;
    MemoryUsed: Int64;
    MemoryAvailable: Int64;
    DiskTotal: Int64;
    DiskUsed: Int64;
    DiskFree: Int64;
    NetworkBytesReceived: Int64;
    NetworkBytesSent: Int64;
    ProcessCount: Integer;
    UptimeSeconds: Int64;
  end;

  TMonitorDatabase = class
  private
    FConnection: TSQLConnection;
    FTransaction: TSQLTransaction;
    FQuery: TSQLQuery;
    FDatabaseType: string; // 'sqlite' ou 'postgresql'

    procedure InitializeSQLite(const AFilename: string);
    procedure InitializePostgreSQL(const AHost, ADatabase, AUser, APassword: string);
  public
    constructor Create(const ADatabaseType: string);
    destructor Destroy; override;

    procedure Connect(const AParams: array of string);
    procedure Disconnect;

    procedure InsertMetric(const AMetric: TMetricRecord);
    procedure InsertAgent(const AgentID, Hostname, IPAddress, OSType, OSVersion: string);
    procedure UpdateAgentStatus(const AgentID: string; IsOnline: Boolean);

    function GetLatestMetrics(const AgentID: string; Count: Integer): TStringList;
    function GetMetricsInTimeRange(const AgentID: string; StartTime, EndTime: TDateTime): TStringList;
    function GetAllAgents: TStringList;

    procedure CreateAlert(const AgentID, AlertType, Severity, Message: string;
                         ThresholdValue, CurrentValue: Double);
    function GetUnresolvedAlerts: TStringList;
  end;

implementation

constructor TMonitorDatabase.Create(const ADatabaseType: string);
begin
  inherited Create;
  FDatabaseType := LowerCase(ADatabaseType);

  FTransaction := TSQLTransaction.Create(nil);
  FQuery := TSQLQuery.Create(nil);

  case FDatabaseType of
    'sqlite': FConnection := TSQLite3Connection.Create(nil);
    'postgresql': FConnection := TPQConnection.Create(nil);
  else
    raise Exception.Create('Type de base de données non supporté: ' + ADatabaseType);
  end;

  FConnection.Transaction := FTransaction;
  FQuery.Database := FConnection;
  FQuery.Transaction := FTransaction;
end;

destructor TMonitorDatabase.Destroy;
begin
  Disconnect;
  FQuery.Free;
  FTransaction.Free;
  FConnection.Free;
  inherited Destroy;
end;

procedure TMonitorDatabase.InitializeSQLite(const AFilename: string);
begin
  TSQLite3Connection(FConnection).DatabaseName := AFilename;
end;

procedure TMonitorDatabase.InitializePostgreSQL(const AHost, ADatabase, AUser, APassword: string);
begin
  with TPQConnection(FConnection) do
  begin
    HostName := AHost;
    DatabaseName := ADatabase;
    UserName := AUser;
    Password := APassword;
  end;
end;

procedure TMonitorDatabase.Connect(const AParams: array of string);
begin
  case FDatabaseType of
    'sqlite':
      begin
        if Length(AParams) > 0 then
          InitializeSQLite(AParams[0])
        else
          InitializeSQLite('monitoring.db');
      end;

    'postgresql':
      begin
        if Length(AParams) >= 4 then
          InitializePostgreSQL(AParams[0], AParams[1], AParams[2], AParams[3])
        else
          raise Exception.Create('Paramètres PostgreSQL insuffisants');
      end;
  end;

  FConnection.Open;
  FTransaction.Active := True;
end;

procedure TMonitorDatabase.Disconnect;
begin
  if FTransaction.Active then
    FTransaction.Commit;
  if FConnection.Connected then
    FConnection.Close;
end;

procedure TMonitorDatabase.InsertMetric(const AMetric: TMetricRecord);
var
  SQL: string;
begin
  SQL := 'INSERT INTO system_metrics ' +
         '(agent_id, timestamp, cpu_usage, memory_total, memory_used, ' +
         'memory_available, disk_total, disk_used, disk_free, ' +
         'network_bytes_received, network_bytes_sent, process_count, uptime_seconds) ' +
         'VALUES ' +
         '(:agent_id, :timestamp, :cpu_usage, :memory_total, :memory_used, ' +
         ':memory_available, :disk_total, :disk_used, :disk_free, ' +
         ':network_bytes_received, :network_bytes_sent, :process_count, :uptime_seconds)';

  FQuery.SQL.Text := SQL;
  FQuery.Params.ParamByName('agent_id').AsString := AMetric.AgentID;
  FQuery.Params.ParamByName('timestamp').AsDateTime := AMetric.Timestamp;
  FQuery.Params.ParamByName('cpu_usage').AsFloat := AMetric.CPUUsage;
  FQuery.Params.ParamByName('memory_total').AsLargeInt := AMetric.MemoryTotal;
  FQuery.Params.ParamByName('memory_used').AsLargeInt := AMetric.MemoryUsed;
  FQuery.Params.ParamByName('memory_available').AsLargeInt := AMetric.MemoryAvailable;
  FQuery.Params.ParamByName('disk_total').AsLargeInt := AMetric.DiskTotal;
  FQuery.Params.ParamByName('disk_used').AsLargeInt := AMetric.DiskUsed;
  FQuery.Params.ParamByName('disk_free').AsLargeInt := AMetric.DiskFree;
  FQuery.Params.ParamByName('network_bytes_received').AsLargeInt := AMetric.NetworkBytesReceived;
  FQuery.Params.ParamByName('network_bytes_sent').AsLargeInt := AMetric.NetworkBytesSent;
  FQuery.Params.ParamByName('process_count').AsInteger := AMetric.ProcessCount;
  FQuery.Params.ParamByName('uptime_seconds').AsLargeInt := AMetric.UptimeSeconds;

  FQuery.ExecSQL;
  FTransaction.Commit;
end;

procedure TMonitorDatabase.InsertAgent(const AgentID, Hostname, IPAddress, OSType, OSVersion: string);
var
  SQL: string;
begin
  SQL := 'INSERT INTO agents (agent_id, hostname, ip_address, os_type, os_version) ' +
         'VALUES (:agent_id, :hostname, :ip_address, :os_type, :os_version) ' +
         'ON CONFLICT (agent_id) DO UPDATE SET ' +
         'hostname = :hostname, ip_address = :ip_address, ' +
         'os_type = :os_type, os_version = :os_version, last_seen = CURRENT_TIMESTAMP';

  FQuery.SQL.Text := SQL;
  FQuery.Params.ParamByName('agent_id').AsString := AgentID;
  FQuery.Params.ParamByName('hostname').AsString := Hostname;
  FQuery.Params.ParamByName('ip_address').AsString := IPAddress;
  FQuery.Params.ParamByName('os_type').AsString := OSType;
  FQuery.Params.ParamByName('os_version').AsString := OSVersion;

  FQuery.ExecSQL;
  FTransaction.Commit;
end;

procedure TMonitorDatabase.UpdateAgentStatus(const AgentID: string; IsOnline: Boolean);
var
  SQL: string;
begin
  SQL := 'UPDATE agents SET is_online = :is_online, last_seen = CURRENT_TIMESTAMP ' +
         'WHERE agent_id = :agent_id';

  FQuery.SQL.Text := SQL;
  FQuery.Params.ParamByName('agent_id').AsString := AgentID;
  FQuery.Params.ParamByName('is_online').AsBoolean := IsOnline;

  FQuery.ExecSQL;
  FTransaction.Commit;
end;

function TMonitorDatabase.GetLatestMetrics(const AgentID: string; Count: Integer): TStringList;
var
  SQL: string;
begin
  Result := TStringList.Create;

  SQL := 'SELECT * FROM system_metrics ' +
         'WHERE agent_id = :agent_id ' +
         'ORDER BY timestamp DESC ' +
         'LIMIT :count';

  FQuery.SQL.Text := SQL;
  FQuery.Params.ParamByName('agent_id').AsString := AgentID;
  FQuery.Params.ParamByName('count').AsInteger := Count;

  FQuery.Open;
  try
    while not FQuery.EOF do
    begin
      Result.Add(Format('Timestamp: %s, CPU: %.2f%%, Memory: %d MB',
        [DateTimeToStr(FQuery.FieldByName('timestamp').AsDateTime),
         FQuery.FieldByName('cpu_usage').AsFloat,
         FQuery.FieldByName('memory_used').AsLargeInt div (1024 * 1024)]));
      FQuery.Next;
    end;
  finally
    FQuery.Close;
  end;
end;

function TMonitorDatabase.GetMetricsInTimeRange(const AgentID: string;
  StartTime, EndTime: TDateTime): TStringList;
var
  SQL: string;
begin
  Result := TStringList.Create;

  SQL := 'SELECT * FROM system_metrics ' +
         'WHERE agent_id = :agent_id ' +
         'AND timestamp BETWEEN :start_time AND :end_time ' +
         'ORDER BY timestamp ASC';

  FQuery.SQL.Text := SQL;
  FQuery.Params.ParamByName('agent_id').AsString := AgentID;
  FQuery.Params.ParamByName('start_time').AsDateTime := StartTime;
  FQuery.Params.ParamByName('end_time').AsDateTime := EndTime;

  FQuery.Open;
  try
    while not FQuery.EOF do
    begin
      Result.Add(Format('%s|%.2f|%d|%d',
        [DateTimeToStr(FQuery.FieldByName('timestamp').AsDateTime),
         FQuery.FieldByName('cpu_usage').AsFloat,
         FQuery.FieldByName('memory_used').AsLargeInt,
         FQuery.FieldByName('disk_used').AsLargeInt]));
      FQuery.Next;
    end;
  finally
    FQuery.Close;
  end;
end;

function TMonitorDatabase.GetAllAgents: TStringList;
var
  SQL: string;
begin
  Result := TStringList.Create;

  SQL := 'SELECT * FROM agents ORDER BY hostname';

  FQuery.SQL.Text := SQL;
  FQuery.Open;
  try
    while not FQuery.EOF do
    begin
      Result.Add(Format('%s|%s|%s|%s',
        [FQuery.FieldByName('agent_id').AsString,
         FQuery.FieldByName('hostname').AsString,
         FQuery.FieldByName('ip_address').AsString,
         BoolToStr(FQuery.FieldByName('is_online').AsBoolean, True)]));
      FQuery.Next;
    end;
  finally
    FQuery.Close;
  end;
end;

procedure TMonitorDatabase.CreateAlert(const AgentID, AlertType, Severity,
  Message: string; ThresholdValue, CurrentValue: Double);
var
  SQL: string;
begin
  SQL := 'INSERT INTO alerts ' +
         '(agent_id, alert_type, severity, message, threshold_value, current_value) ' +
         'VALUES ' +
         '(:agent_id, :alert_type, :severity, :message, :threshold_value, :current_value)';

  FQuery.SQL.Text := SQL;
  FQuery.Params.ParamByName('agent_id').AsString := AgentID;
  FQuery.Params.ParamByName('alert_type').AsString := AlertType;
  FQuery.Params.ParamByName('severity').AsString := Severity;
  FQuery.Params.ParamByName('message').AsString := Message;
  FQuery.Params.ParamByName('threshold_value').AsFloat := ThresholdValue;
  FQuery.Params.ParamByName('current_value').AsFloat := CurrentValue;

  FQuery.ExecSQL;
  FTransaction.Commit;
end;

function TMonitorDatabase.GetUnresolvedAlerts: TStringList;
var
  SQL: string;
begin
  Result := TStringList.Create;

  SQL := 'SELECT a.*, ag.hostname ' +
         'FROM alerts a ' +
         'JOIN agents ag ON a.agent_id = ag.agent_id ' +
         'WHERE a.is_resolved = FALSE ' +
         'ORDER BY a.created_at DESC';

  FQuery.SQL.Text := SQL;
  FQuery.Open;
  try
    while not FQuery.EOF do
    begin
      Result.Add(Format('[%s] %s - %s: %s (%.2f > %.2f)',
        [FQuery.FieldByName('severity').AsString,
         FQuery.FieldByName('hostname').AsString,
         FQuery.FieldByName('alert_type').AsString,
         FQuery.FieldByName('message').AsString,
         FQuery.FieldByName('current_value').AsFloat,
         FQuery.FieldByName('threshold_value').AsFloat]));
      FQuery.Next;
    end;
  finally
    FQuery.Close;
  end;
end;

end.
```

### API REST avec fpWeb

Pour permettre à l'interface graphique d'accéder aux données, nous créons une API REST.

```pascal
unit MonitorAPI;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpapp, httpdefs, httproute, fpjson, jsonparser,
  MonitorDatabase;

type
  TMonitorAPIServer = class
  private
    FDatabase: TMonitorDatabase;
    FPort: Integer;

    procedure HandleGetAgents(ARequest: TRequest; AResponse: TResponse);
    procedure HandleGetMetrics(ARequest: TRequest; AResponse: TResponse);
    procedure HandleGetAlerts(ARequest: TRequest; AResponse: TResponse);
    procedure HandleGetAgentDetails(ARequest: TRequest; AResponse: TResponse);
  public
    constructor Create(ADatabase: TMonitorDatabase; APort: Integer);
    destructor Destroy; override;

    procedure Start;
    procedure Stop;
  end;

implementation

constructor TMonitorAPIServer.Create(ADatabase: TMonitorDatabase; APort: Integer);
begin
  inherited Create;
  FDatabase := ADatabase;
  FPort := APort;

  // Configuration des routes
  HTTPRouter.RegisterRoute('/api/agents', rmGet, @HandleGetAgents);
  HTTPRouter.RegisterRoute('/api/metrics/:agentid', rmGet, @HandleGetMetrics);
  HTTPRouter.RegisterRoute('/api/alerts', rmGet, @HandleGetAlerts);
  HTTPRouter.RegisterRoute('/api/agent/:agentid', rmGet, @HandleGetAgentDetails);
end;

destructor TMonitorAPIServer.Destroy;
begin
  Stop;
  inherited Destroy;
end;

procedure TMonitorAPIServer.HandleGetAgents(ARequest: TRequest; AResponse: TResponse);
var
  Agents: TStringList;
  JSONArray: TJSONArray;
  JSONObj: TJSONObject;
  i: Integer;
  Parts: TStringList;
begin
  Agents := FDatabase.GetAllAgents;
  Parts := TStringList.Create;
  try
    Parts.Delimiter := '|';
    Parts.StrictDelimiter := True;
    JSONArray := TJSONArray.Create;

    for i := 0 to Agents.Count - 1 do
    begin
      Parts.DelimitedText := Agents[i];
      if Parts.Count >= 4 then
      begin
        JSONObj := TJSONObject.Create;
        JSONObj.Add('agent_id', Parts[0]);
        JSONObj.Add('hostname', Parts[1]);
        JSONObj.Add('ip_address', Parts[2]);
        JSONObj.Add('is_online', StrToBool(Parts[3]));
        JSONArray.Add(JSONObj);
      end;
    end;

    AResponse.Content := JSONArray.AsJSON;
    AResponse.ContentType := 'application/json';
    AResponse.Code := 200;
  finally
    Agents.Free;
    Parts.Free;
    JSONArray.Free;
  end;
end;

procedure TMonitorAPIServer.HandleGetMetrics(ARequest: TRequest; AResponse: TResponse);
var
  AgentID: string;
  Metrics: TStringList;
  JSONArray: TJSONArray;
  JSONObj: TJSONObject;
  i: Integer;
  Parts: TStringList;
begin
  AgentID := ARequest.RouteParams['agentid'];

  Metrics := FDatabase.GetLatestMetrics(AgentID, 100);
  Parts := TStringList.Create;
  try
    Parts.Delimiter := '|';
    Parts.StrictDelimiter := True;
    JSONArray := TJSONArray.Create;

    for i := 0 to Metrics.Count - 1 do
    begin
      Parts.DelimitedText := Metrics[i];
      if Parts.Count >= 4 then
      begin
        JSONObj := TJSONObject.Create;
        JSONObj.Add('timestamp', Parts[0]);
        JSONObj.Add('cpu_usage', StrToFloatDef(Parts[1], 0.0));
        JSONObj.Add('memory_used', StrToInt64Def(Parts[2], 0));
        JSONObj.Add('disk_used', StrToInt64Def(Parts[3], 0));
        JSONArray.Add(JSONObj);
      end;
    end;

    AResponse.Content := JSONArray.AsJSON;
    AResponse.ContentType := 'application/json';
    AResponse.Code := 200;
  finally
    Metrics.Free;
    Parts.Free;
    JSONArray.Free;
  end;
end;

procedure TMonitorAPIServer.HandleGetAlerts(ARequest: TRequest; AResponse: TResponse);
var
  Alerts: TStringList;
  JSONArray: TJSONArray;
  i: Integer;
begin
  Alerts := FDatabase.GetUnresolvedAlerts;
  try
    JSONArray := TJSONArray.Create;

    for i := 0 to Alerts.Count - 1 do
    begin
      JSONArray.Add(Alerts[i]);
    end;

    AResponse.Content := JSONArray.AsJSON;
    AResponse.ContentType := 'application/json';
    AResponse.Code := 200;
  finally
    Alerts.Free;
    JSONArray.Free;
  end;
end;

procedure TMonitorAPIServer.HandleGetAgentDetails(ARequest: TRequest; AResponse: TResponse);
var
  AgentID: string;
  JSONObj: TJSONObject;
begin
  AgentID := ARequest.RouteParams['agentid'];

  JSONObj := TJSONObject.Create;
  try
    JSONObj.Add('agent_id', AgentID);
    JSONObj.Add('status', 'online');
    // Ajouter plus de détails ici

    AResponse.Content := JSONObj.AsJSON;
    AResponse.ContentType := 'application/json';
    AResponse.Code := 200;
  finally
    JSONObj.Free;
  end;
end;

procedure TMonitorAPIServer.Start;
begin
  Application.Port := FPort;
  Application.Initialize;
  WriteLn('API REST démarrée sur le port ', FPort);
  Application.Run;
end;

procedure TMonitorAPIServer.Stop;
begin
  Application.Terminate;
end;

end.
```

## Partie 3 : Interface de Visualisation

### Interface graphique principale

Nous allons créer une interface Lazarus complète pour visualiser les données de monitoring.

```pascal
unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  ExtCtrls, TAGraph, TASeries, TATools, fphttpclient, fpjson, jsonparser;

type
  TFormMain = class(TForm)
    PageControl1: TPageControl;
    TabSheetDashboard: TTabSheet;
    TabSheetAgents: TTabSheet;
    TabSheetAlerts: TTabSheet;
    TabSheetSettings: TTabSheet;

    // Dashboard
    PanelDashboard: TPanel;
    ChartCPU: TChart;
    ChartMemory: TChart;
    ChartDisk: TChart;
    ChartNetwork: TChart;

    // Liste des agents
    ListViewAgents: TListView;
    ButtonRefreshAgents: TButton;

    // Alertes
    ListViewAlerts: TListView;
    ButtonRefreshAlerts: TButton;

    // Timer pour rafraîchissement auto
    TimerRefresh: TTimer;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonRefreshAgentsClick(Sender: TObject);
    procedure ButtonRefreshAlertsClick(Sender: TObject);
    procedure TimerRefreshTimer(Sender: TObject);
    procedure ListViewAgentsDblClick(Sender: TObject);

  private
    FAPIBaseURL: string;
    FHTTPClient: TFPHTTPClient;
    FSelectedAgentID: string;

    procedure LoadAgents;
    procedure LoadAlerts;
    procedure LoadMetrics(const AgentID: string);
    procedure UpdateCharts(const MetricsJSON: TJSONArray);
    function GetJSONFromAPI(const Endpoint: string): TJSONData;

  public

  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FAPIBaseURL := 'http://localhost:8080/api';
  FHTTPClient := TFPHTTPClient.Create(nil);

  // Configuration de l'interface
  PageControl1.ActivePageIndex := 0;

  // Configuration ListView Agents
  with ListViewAgents do
  begin
    ViewStyle := vsReport;
    Columns.Add.Caption := 'Hostname';
    Columns.Add.Caption := 'IP Address';
    Columns.Add.Caption := 'Status';
    Columns.Add.Caption := 'Last Seen';
    Columns[0].Width := 200;
    Columns[1].Width := 150;
    Columns[2].Width := 100;
    Columns[3].Width := 150;
  end;

  // Configuration ListView Alerts
  with ListViewAlerts do
  begin
    ViewStyle := vsReport;
    Columns.Add.Caption := 'Severity';
    Columns.Add.Caption := 'Agent';
    Columns.Add.Caption := 'Type';
    Columns.Add.Caption := 'Message';
    Columns.Add.Caption := 'Time';
    Columns[0].Width := 100;
    Columns[1].Width := 150;
    Columns[2].Width := 100;
    Columns[3].Width := 300;
    Columns[4].Width := 150;
  end;

  // Configuration des graphiques
  ChartCPU.Title.Text.Text := 'Utilisation CPU';
  ChartMemory.Title.Text.Text := 'Utilisation Mémoire';
  ChartDisk.Title.Text.Text := 'Utilisation Disque';
  ChartNetwork.Title.Text.Text := 'Trafic Réseau';

  // Démarrer le timer de rafraîchissement (toutes les 5 secondes)
  TimerRefresh.Interval := 5000;
  TimerRefresh.Enabled := True;

  // Charger les données initiales
  LoadAgents;
  LoadAlerts;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FHTTPClient.Free;
end;

procedure TFormMain.ButtonRefreshAgentsClick(Sender: TObject);
begin
  LoadAgents;
end;

procedure TFormMain.ButtonRefreshAlertsClick(Sender: TObject);
begin
  LoadAlerts;
end;

procedure TFormMain.TimerRefreshTimer(Sender: TObject);
begin
  // Rafraîchissement automatique
  if PageControl1.ActivePage = TabSheetDashboard then
  begin
    if FSelectedAgentID <> '' then
      LoadMetrics(FSelectedAgentID);
  end
  else if PageControl1.ActivePage = TabSheetAgents then
    LoadAgents
  else if PageControl1.ActivePage = TabSheetAlerts then
    LoadAlerts;
end;

procedure TFormMain.ListViewAgentsDblClick(Sender: TObject);
begin
  if ListViewAgents.Selected <> nil then
  begin
    FSelectedAgentID := ListViewAgents.Selected.SubItems[3]; // ID stocké en SubItem caché
    LoadMetrics(FSelectedAgentID);
    PageControl1.ActivePage := TabSheetDashboard;
  end;
end;

function TFormMain.GetJSONFromAPI(const Endpoint: string): TJSONData;
var
  Response: string;
  Parser: TJSONParser;
begin
  Result := nil;
  try
    Response := FHTTPClient.Get(FAPIBaseURL + Endpoint);
    Parser := TJSONParser.Create(Response, [joUTF8]);
    try
      Result := Parser.Parse;
    finally
      Parser.Free;
    end;
  except
    on E: Exception do
    begin
      ShowMessage('Erreur lors de la récupération des données: ' + E.Message);
    end;
  end;
end;

procedure TFormMain.LoadAgents;
var
  JSONData: TJSONData;
  JSONArray: TJSONArray;
  JSONObj: TJSONObject;
  i: Integer;
  ListItem: TListItem;
begin
  JSONData := GetJSONFromAPI('/agents');
  if JSONData = nil then
    Exit;

  try
    if JSONData is TJSONArray then
    begin
      JSONArray := TJSONArray(JSONData);
      ListViewAgents.Items.Clear;

      for i := 0 to JSONArray.Count - 1 do
      begin
        JSONObj := TJSONObject(JSONArray[i]);
        ListItem := ListViewAgents.Items.Add;
        ListItem.Caption := JSONObj.Get('hostname', '');
        ListItem.SubItems.Add(JSONObj.Get('ip_address', ''));

        if JSONObj.Get('is_online', False) then
        begin
          ListItem.SubItems.Add('Online');
          ListItem.ImageIndex := 0; // Icône verte
        end
        else
        begin
          ListItem.SubItems.Add('Offline');
          ListItem.ImageIndex := 1; // Icône rouge
        end;

        ListItem.SubItems.Add(FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));
        ListItem.SubItems.Add(JSONObj.Get('agent_id', '')); // ID caché
      end;
    end;
  finally
    JSONData.Free;
  end;
end;

procedure TFormMain.LoadAlerts;
var
  JSONData: TJSONData;
  JSONArray: TJSONArray;
  i: Integer;
  ListItem: TListItem;
  AlertText: string;
begin
  JSONData := GetJSONFromAPI('/alerts');
  if JSONData = nil then
    Exit;

  try
    if JSONData is TJSONArray then
    begin
      JSONArray := TJSONArray(JSONData);
      ListViewAlerts.Items.Clear;

      for i := 0 to JSONArray.Count - 1 do
      begin
        AlertText := JSONArray.Strings[i];
        ListItem := ListViewAlerts.Items.Add;

        // Parser le format: [SEVERITY] HOSTNAME - TYPE: MESSAGE (VALUE > THRESHOLD)
        // Simplification pour l'exemple
        ListItem.Caption := 'WARNING';
        ListItem.SubItems.Add('Agent1');
        ListItem.SubItems.Add('CPU');
        ListItem.SubItems.Add(AlertText);
        ListItem.SubItems.Add(FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));
      end;
    end;
  finally
    JSONData.Free;
  end;
end;

procedure TFormMain.LoadMetrics(const AgentID: string);
var
  JSONData: TJSONData;
  JSONArray: TJSONArray;
begin
  JSONData := GetJSONFromAPI('/metrics/' + AgentID);
  if JSONData = nil then
    Exit;

  try
    if JSONData is TJSONArray then
    begin
      JSONArray := TJSONArray(JSONData);
      UpdateCharts(JSONArray);
    end;
  finally
    JSONData.Free;
  end;
end;

procedure TFormMain.UpdateCharts(const MetricsJSON: TJSONArray);
var
  i: Integer;
  JSONObj: TJSONObject;
  CPUSeries: TLineSeries;
  MemSeries: TLineSeries;
  DiskSeries: TLineSeries;
  NetSeries: TLineSeries;
  TimeValue: Double;
begin
  // Nettoyer les anciennes séries
  ChartCPU.ClearSeries;
  ChartMemory.ClearSeries;
  ChartDisk.ClearSeries;
  ChartNetwork.ClearSeries;

  // Créer les nouvelles séries
  CPUSeries := TLineSeries.Create(ChartCPU);
  CPUSeries.Title := 'CPU %';
  ChartCPU.AddSeries(CPUSeries);

  MemSeries := TLineSeries.Create(ChartMemory);
  MemSeries.Title := 'Memory MB';
  ChartMemory.AddSeries(MemSeries);

  DiskSeries := TLineSeries.Create(ChartDisk);
  DiskSeries.Title := 'Disk GB';
  ChartDisk.AddSeries(DiskSeries);

  NetSeries := TLineSeries.Create(ChartNetwork);
  NetSeries.Title := 'Network MB/s';
  ChartNetwork.AddSeries(NetSeries);

  // Remplir les séries avec les données
  for i := 0 to MetricsJSON.Count - 1 do
  begin
    JSONObj := TJSONObject(MetricsJSON[i]);
    TimeValue := i; // Simplification: utiliser l'index comme temps

    CPUSeries.AddXY(TimeValue, JSONObj.Get('cpu_usage', 0.0));
    MemSeries.AddXY(TimeValue, JSONObj.Get('memory_used', Int64(0)) / (1024 * 1024));
    DiskSeries.AddXY(TimeValue, JSONObj.Get('disk_used', Int64(0)) / (1024 * 1024 * 1024));
    // Network nécessiterait un calcul de débit
  end;
end;

end.
```

### Fenêtre de détails d'un agent

Pour afficher des informations détaillées sur un agent spécifique :

```pascal
unit AgentDetailsForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, TAGraph, TASeries, Grids;

type
  TFormAgentDetails = class(TForm)
    GroupBoxInfo: TGroupBox;
    LabelHostname: TLabel;
    LabelIP: TLabel;
    LabelOS: TLabel;
    LabelUptime: TLabel;
    LabelStatus: TLabel;

    PageControlDetails: TPageControl;
    TabSheetOverview: TTabSheet;
    TabSheetProcesses: TTabSheet;
    TabSheetLogs: TTabSheet;

    // Vue d'ensemble
    PanelGauges: TPanel;
    GaugeCPU: TProgressBar;
    GaugeMemory: TProgressBar;
    GaugeDisk: TProgressBar;

    ChartHistorical: TChart;

    // Processus
    StringGridProcesses: TStringGrid;
    ButtonRefreshProcesses: TButton;

    // Logs
    MemoLogs: TMemo;
    ButtonRefreshLogs: TButton;

    procedure FormCreate(Sender: TObject);
    procedure ButtonRefreshProcessesClick(Sender: TObject);
    procedure ButtonRefreshLogsClick(Sender: TObject);

  private
    FAgentID: string;

    procedure LoadAgentInfo;
    procedure LoadProcesses;
    procedure LoadLogs;
    procedure UpdateGauges(CPU, Memory, Disk: Double);

  public
    property AgentID: string read FAgentID write FAgentID;
    procedure ShowAgentDetails(const AAgentID: string);
  end;

var
  FormAgentDetails: TFormAgentDetails;

implementation

{$R *.lfm}

procedure TFormAgentDetails.FormCreate(Sender: TObject);
begin
  // Configuration de la grille des processus
  with StringGridProcesses do
  begin
    ColCount := 5;
    RowCount := 1;
    FixedRows := 1;

    Cells[0, 0] := 'PID';
    Cells[1, 0] := 'Name';
    Cells[2, 0] := 'CPU %';
    Cells[3, 0] := 'Memory MB';
    Cells[4, 0] := 'Status';

    ColWidths[0] := 80;
    ColWidths[1] := 200;
    ColWidths[2] := 80;
    ColWidths[3] := 100;
    ColWidths[4] := 100;
  end;
end;

procedure TFormAgentDetails.ShowAgentDetails(const AAgentID: string);
begin
  FAgentID := AAgentID;
  LoadAgentInfo;
  LoadProcesses;
  LoadLogs;
  Show;
end;

procedure TFormAgentDetails.LoadAgentInfo;
begin
  // Charger les informations de base de l'agent depuis l'API
  LabelHostname.Caption := 'Hostname: server-01.example.com';
  LabelIP.Caption := 'IP Address: 192.168.1.100';
  LabelOS.Caption := 'OS: Ubuntu 22.04 LTS';
  LabelUptime.Caption := 'Uptime: 15 days, 7 hours';
  LabelStatus.Caption := 'Status: Online';
  LabelStatus.Font.Color := clGreen;

  // Mettre à jour les jauges
  UpdateGauges(45.2, 68.5, 72.3);
end;

procedure TFormAgentDetails.UpdateGauges(CPU, Memory, Disk: Double);
begin
  GaugeCPU.Position := Round(CPU);
  GaugeMemory.Position := Round(Memory);
  GaugeDisk.Position := Round(Disk);
end;

procedure TFormAgentDetails.LoadProcesses;
var
  i: Integer;
begin
  // Charger la liste des processus depuis l'API
  StringGridProcesses.RowCount := 11; // 1 en-tête + 10 processus

  for i := 1 to 10 do
  begin
    StringGridProcesses.Cells[0, i] := IntToStr(1000 + i);
    StringGridProcesses.Cells[1, i] := 'process' + IntToStr(i);
    StringGridProcesses.Cells[2, i] := Format('%.1f', [Random * 10]);
    StringGridProcesses.Cells[3, i] := IntToStr(Random(500) + 50);
    StringGridProcesses.Cells[4, i] := 'Running';
  end;
end;

procedure TFormAgentDetails.ButtonRefreshProcessesClick(Sender: TObject);
begin
  LoadProcesses;
end;

procedure TFormAgentDetails.LoadLogs;
begin
  // Charger les logs récents depuis l'API
  MemoLogs.Lines.Clear;
  MemoLogs.Lines.Add('[' + FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) + '] System started');
  MemoLogs.Lines.Add('[' + FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) + '] Network interface eth0 up');
  MemoLogs.Lines.Add('[' + FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) + '] Service nginx started');
  MemoLogs.Lines.Add('[' + FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) + '] Monitoring agent connected');
end;

procedure TFormAgentDetails.ButtonRefreshLogsClick(Sender: TObject);
begin
  LoadLogs;
end;

end.
```

## Système d'alertes avancé

### Moteur d'évaluation des alertes

```pascal
unit AlertEngine;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MonitorDatabase, fgl;

type
  TAlertSeverity = (asInfo, asWarning, asCritical);

  TAlertThreshold = record
    MetricName: string;
    WarningThreshold: Double;
    CriticalThreshold: Double;
    Operator: string; // '>', '<', '=', '!='
    CheckInterval: Integer; // en secondes
    Enabled: Boolean;
  end;

  TAlertRule = class
  public
    RuleID: Integer;
    AgentID: string;
    MetricName: string;
    Threshold: TAlertThreshold;
    LastCheck: TDateTime;
    LastAlertTime: TDateTime;
    AlertCount: Integer;
  end;

  TAlertRuleList = specialize TFPGObjectList<TAlertRule>;

  TAlertEngine = class
  private
    FDatabase: TMonitorDatabase;
    FRules: TAlertRuleList;
    FAlertCooldown: Integer; // Temps minimum entre deux alertes identiques (secondes)

    function EvaluateThreshold(CurrentValue, ThresholdValue: Double;
                               Operator: string): Boolean;
    function GetSeverity(CurrentValue: Double; const Threshold: TAlertThreshold): TAlertSeverity;
    procedure SendAlert(const AgentID, AlertType: string; Severity: TAlertSeverity;
                       const Message: string; ThresholdValue, CurrentValue: Double);
  public
    constructor Create(ADatabase: TMonitorDatabase);
    destructor Destroy; override;

    procedure LoadRules;
    procedure AddRule(const Rule: TAlertRule);
    procedure CheckMetrics(const AgentID: string; const MetricData: TMetricRecord);
    procedure SetAlertCooldown(Seconds: Integer);
  end;

implementation

uses
  DateUtils;

constructor TAlertEngine.Create(ADatabase: TMonitorDatabase);
begin
  inherited Create;
  FDatabase := ADatabase;
  FRules := TAlertRuleList.Create(True);
  FAlertCooldown := 300; // 5 minutes par défaut
end;

destructor TAlertEngine.Destroy;
begin
  FRules.Free;
  inherited Destroy;
end;

procedure TAlertEngine.LoadRules;
var
  Rule: TAlertRule;
begin
  // Charger les règles depuis la base de données
  // Exemple de règle CPU
  Rule := TAlertRule.Create;
  Rule.RuleID := 1;
  Rule.AgentID := '*'; // Toutes les machines
  Rule.MetricName := 'cpu_usage';
  Rule.Threshold.MetricName := 'cpu_usage';
  Rule.Threshold.WarningThreshold := 80.0;
  Rule.Threshold.CriticalThreshold := 95.0;
  Rule.Threshold.Operator := '>';
  Rule.Threshold.CheckInterval := 60;
  Rule.Threshold.Enabled := True;
  FRules.Add(Rule);

  // Règle mémoire
  Rule := TAlertRule.Create;
  Rule.RuleID := 2;
  Rule.AgentID := '*';
  Rule.MetricName := 'memory_percent';
  Rule.Threshold.MetricName := 'memory_percent';
  Rule.Threshold.WarningThreshold := 85.0;
  Rule.Threshold.CriticalThreshold := 95.0;
  Rule.Threshold.Operator := '>';
  Rule.Threshold.CheckInterval := 60;
  Rule.Threshold.Enabled := True;
  FRules.Add(Rule);

  // Règle disque
  Rule := TAlertRule.Create;
  Rule.RuleID := 3;
  Rule.AgentID := '*';
  Rule.MetricName := 'disk_percent';
  Rule.Threshold.MetricName := 'disk_percent';
  Rule.Threshold.WarningThreshold := 80.0;
  Rule.Threshold.CriticalThreshold := 90.0;
  Rule.Threshold.Operator := '>';
  Rule.Threshold.CheckInterval := 300;
  Rule.Threshold.Enabled := True;
  FRules.Add(Rule);
end;

procedure TAlertEngine.AddRule(const Rule: TAlertRule);
begin
  FRules.Add(Rule);
end;

function TAlertEngine.EvaluateThreshold(CurrentValue, ThresholdValue: Double;
  Operator: string): Boolean;
begin
  Result := False;

  case Operator of
    '>': Result := CurrentValue > ThresholdValue;
    '<': Result := CurrentValue < ThresholdValue;
    '>=': Result := CurrentValue >= ThresholdValue;
    '<=': Result := CurrentValue <= ThresholdValue;
    '=': Result := Abs(CurrentValue - ThresholdValue) < 0.001;
    '!=': Result := Abs(CurrentValue - ThresholdValue) >= 0.001;
  end;
end;

function TAlertEngine.GetSeverity(CurrentValue: Double;
  const Threshold: TAlertThreshold): TAlertSeverity;
begin
  if EvaluateThreshold(CurrentValue, Threshold.CriticalThreshold, Threshold.Operator) then
    Result := asCritical
  else if EvaluateThreshold(CurrentValue, Threshold.WarningThreshold, Threshold.Operator) then
    Result := asWarning
  else
    Result := asInfo;
end;

procedure TAlertEngine.CheckMetrics(const AgentID: string; const MetricData: TMetricRecord);
var
  i: Integer;
  Rule: TAlertRule;
  CurrentValue: Double;
  MemoryPercent, DiskPercent: Double;
  Severity: TAlertSeverity;
  Message: string;
  ShouldAlert: Boolean;
begin
  // Calculer les pourcentages
  if MetricData.MemoryTotal > 0 then
    MemoryPercent := (MetricData.MemoryUsed / MetricData.MemoryTotal) * 100.0
  else
    MemoryPercent := 0.0;

  if MetricData.DiskTotal > 0 then
    DiskPercent := (MetricData.DiskUsed / MetricData.DiskTotal) * 100.0
  else
    DiskPercent := 0.0;

  for i := 0 to FRules.Count - 1 do
  begin
    Rule := FRules[i];

    // Vérifier si la règle s'applique à cet agent
    if (Rule.AgentID <> '*') and (Rule.AgentID <> AgentID) then
      Continue;

    if not Rule.Threshold.Enabled then
      Continue;

    // Vérifier l'intervalle de vérification
    if SecondsBetween(Now, Rule.LastCheck) < Rule.Threshold.CheckInterval then
      Continue;

    Rule.LastCheck := Now;

    // Obtenir la valeur actuelle selon la métrique
    CurrentValue := 0.0;
    case Rule.MetricName of
      'cpu_usage': CurrentValue := MetricData.CPUUsage;
      'memory_percent': CurrentValue := MemoryPercent;
      'disk_percent': CurrentValue := DiskPercent;
    else
      Continue;
    end;

    // Évaluer le seuil
    ShouldAlert := False;
    Severity := GetSeverity(CurrentValue, Rule.Threshold);

    if Severity in [asWarning, asCritical] then
    begin
      // Vérifier le cooldown
      if SecondsBetween(Now, Rule.LastAlertTime) >= FAlertCooldown then
        ShouldAlert := True;
    end;

    if ShouldAlert then
    begin
      case Severity of
        asWarning:
          Message := Format('Avertissement: %s à %.2f%% (seuil: %.2f%%)',
                           [Rule.MetricName, CurrentValue, Rule.Threshold.WarningThreshold]);
        asCritical:
          Message := Format('CRITIQUE: %s à %.2f%% (seuil: %.2f%%)',
                           [Rule.MetricName, CurrentValue, Rule.Threshold.CriticalThreshold]);
      end;

      SendAlert(AgentID, Rule.MetricName, Severity, Message,
                Rule.Threshold.WarningThreshold, CurrentValue);

      Rule.LastAlertTime := Now;
      Rule.AlertCount := Rule.AlertCount + 1;
    end;
  end;
end;

procedure TAlertEngine.SendAlert(const AgentID, AlertType: string;
  Severity: TAlertSeverity; const Message: string;
  ThresholdValue, CurrentValue: Double);
var
  SeverityStr: string;
begin
  case Severity of
    asInfo: SeverityStr := 'INFO';
    asWarning: SeverityStr := 'WARNING';
    asCritical: SeverityStr := 'CRITICAL';
  end;

  // Enregistrer l'alerte dans la base de données
  FDatabase.CreateAlert(AgentID, AlertType, SeverityStr, Message,
                       ThresholdValue, CurrentValue);

  // Ici, on pourrait aussi envoyer des notifications:
  // - Email
  // - SMS
  // - Slack/Discord/Teams
  // - PagerDuty
  // - Webhook personnalisé

  WriteLn(Format('[%s] %s: %s', [SeverityStr, AgentID, Message]));
end;

procedure TAlertEngine.SetAlertCooldown(Seconds: Integer);
begin
  FAlertCooldown := Seconds;
end;

end.
```

### Système de notifications

```pascal
unit NotificationSystem;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient, smtpsend, mimepart, mimemess;

type
  TNotificationChannel = (ncEmail, ncSlack, ncWebhook, ncSMS);

  TNotificationConfig = record
    Channel: TNotificationChannel;
    Enabled: Boolean;

    // Email
    SMTPServer: string;
    SMTPPort: Integer;
    SMTPUsername: string;
    SMTPPassword: string;
    EmailFrom: string;
    EmailTo: string;

    // Slack
    SlackWebhookURL: string;
    SlackChannel: string;

    // Webhook
    WebhookURL: string;
    WebhookMethod: string; // GET, POST

    // SMS (via API)
    SMSAPIEndpoint: string;
    SMSAPIKey: string;
    SMSPhoneNumber: string;
  end;

  TNotificationSystem = class
  private
    FConfigs: array of TNotificationConfig;

    function SendEmail(const Config: TNotificationConfig;
                      const Subject, Body: string): Boolean;
    function SendSlack(const Config: TNotificationConfig;
                      const Message: string): Boolean;
    function SendWebhook(const Config: TNotificationConfig;
                        const Payload: string): Boolean;
    function SendSMS(const Config: TNotificationConfig;
                    const Message: string): Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddChannel(const Config: TNotificationConfig);
    procedure SendNotification(const Title, Message: string; Severity: string);
  end;

implementation

uses
  fpjson, jsonparser;

constructor TNotificationSystem.Create;
begin
  inherited Create;
  SetLength(FConfigs, 0);
end;

destructor TNotificationSystem.Destroy;
begin
  inherited Destroy;
end;

procedure TNotificationSystem.AddChannel(const Config: TNotificationConfig);
begin
  SetLength(FConfigs, Length(FConfigs) + 1);
  FConfigs[High(FConfigs)] := Config;
end;

procedure TNotificationSystem.SendNotification(const Title, Message: string; Severity: string);
var
  i: Integer;
  FullMessage: string;
begin
  FullMessage := Format('[%s] %s: %s', [Severity, Title, Message]);

  for i := 0 to High(FConfigs) do
  begin
    if not FConfigs[i].Enabled then
      Continue;

    case FConfigs[i].Channel of
      ncEmail: SendEmail(FConfigs[i], Title, FullMessage);
      ncSlack: SendSlack(FConfigs[i], FullMessage);
      ncWebhook: SendWebhook(FConfigs[i], FullMessage);
      ncSMS: SendSMS(FConfigs[i], FullMessage);
    end;
  end;
end;

function TNotificationSystem.SendEmail(const Config: TNotificationConfig;
  const Subject, Body: string): Boolean;
var
  Msg: TMimeMess;
  Part: TMimePart;
begin
  Result := False;

  try
    Msg := TMimeMess.Create;
    try
      // Configuration du message
      Msg.Header.From := Config.EmailFrom;
      Msg.Header.ToList.Add(Config.EmailTo);
      Msg.Header.Subject := Subject;
      Msg.Header.Date := Now;

      // Corps du message
      Part := Msg.AddPartMultipart('alternative', nil);
      Msg.AddPartText(Body, Part);

      // Encoder le message
      Msg.EncodeMessage;

      // Envoi via SMTP
      Result := SmtpSendMail(Config.SMTPServer,
                            Config.SMTPPort,
                            Config.SMTPUsername,
                            Config.SMTPPassword,
                            Config.EmailFrom,
                            Config.EmailTo,
                            Subject,
                            Msg.Lines);
    finally
      Msg.Free;
    end;
  except
    on E: Exception do
      WriteLn('Erreur d''envoi email: ' + E.Message);
  end;
end;

function TNotificationSystem.SendSlack(const Config: TNotificationConfig;
  const Message: string): Boolean;
var
  HTTPClient: TFPHTTPClient;
  JSON: TJSONObject;
  Response: string;
begin
  Result := False;

  try
    HTTPClient := TFPHTTPClient.Create(nil);
    try
      JSON := TJSONObject.Create;
      try
        JSON.Add('text', Message);
        if Config.SlackChannel <> '' then
          JSON.Add('channel', Config.SlackChannel);

        HTTPClient.AddHeader('Content-Type', 'application/json');
        HTTPClient.RequestBody := TStringStream.Create(JSON.AsJSON);

        Response := HTTPClient.Post(Config.SlackWebhookURL);
        Result := HTTPClient.ResponseStatusCode = 200;
      finally
        JSON.Free;
      end;
    finally
      HTTPClient.Free;
    end;
  except
    on E: Exception do
      WriteLn('Erreur d''envoi Slack: ' + E.Message);
  end;
end;

function TNotificationSystem.SendWebhook(const Config: TNotificationConfig;
  const Payload: string): Boolean;
var
  HTTPClient: TFPHTTPClient;
  Response: string;
begin
  Result := False;

  try
    HTTPClient := TFPHTTPClient.Create(nil);
    try
      HTTPClient.AddHeader('Content-Type', 'application/json');

      if Config.WebhookMethod = 'POST' then
      begin
        HTTPClient.RequestBody := TStringStream.Create(Payload);
        Response := HTTPClient.Post(Config.WebhookURL);
      end
      else
        Response := HTTPClient.Get(Config.WebhookURL + '?message=' + Payload);

      Result := HTTPClient.ResponseStatusCode in [200, 201, 202];
    finally
      HTTPClient.Free;
    end;
  except
    on E: Exception do
      WriteLn('Erreur d''envoi webhook: ' + E.Message);
  end;
end;

function TNotificationSystem.SendSMS(const Config: TNotificationConfig;
  const Message: string): Boolean;
begin
  // Implémentation dépendante du fournisseur SMS (Twilio, AWS SNS, etc.)
  Result := False;
  WriteLn('Envoi SMS non implémenté dans cet exemple');
end;

end.
```

## Déploiement et Configuration

### Script de déploiement Windows

```batch
@echo off
REM Script de déploiement MonitorAgent pour Windows

echo ========================================
echo Installation de MonitorAgent
echo ========================================

REM Vérifier les privilèges administrateur
net session >nul 2>&1
if %errorLevel% neq 0 (
    echo Ce script necessite les privileges administrateur
    echo Veuillez executer en tant qu'administrateur
    pause
    exit /b 1
)

REM Définir les variables
set INSTALL_DIR=C:\Program Files\MonitorAgent
set SERVICE_NAME=MonitorAgent
set CONFIG_FILE=%INSTALL_DIR%\config.ini

REM Créer le répertoire d'installation
echo Creation du repertoire d'installation...
if not exist "%INSTALL_DIR%" (
    mkdir "%INSTALL_DIR%"
)

REM Copier les fichiers
echo Copie des fichiers...
copy /Y monitoragent.exe "%INSTALL_DIR%\"
copy /Y config.ini "%INSTALL_DIR%\"
copy /Y *.dll "%INSTALL_DIR%\"

REM Créer le fichier de configuration par défaut
echo Creation du fichier de configuration...
(
echo [Server]
echo Host=monitoring.example.com
echo Port=9100
echo UseTLS=false
echo
echo [Agent]
echo CollectionInterval=30
echo SendInterval=60
echo
echo [Logging]
echo LogLevel=INFO
echo LogFile=%INSTALL_DIR%\logs\agent.log
) > "%CONFIG_FILE%"

REM Créer le répertoire des logs
if not exist "%INSTALL_DIR%\logs" (
    mkdir "%INSTALL_DIR%\logs"
)

REM Installer le service
echo Installation du service Windows...
sc create %SERVICE_NAME% binPath= "%INSTALL_DIR%\monitoragent.exe" start= auto DisplayName= "Monitor Agent Service"

if %errorLevel% equ 0 (
    echo Service installe avec succes

    REM Configurer la description du service
    sc description %SERVICE_NAME% "Service de monitoring systeme pour collecter et envoyer les metriques"

    REM Configurer la récupération automatique en cas d'échec
    sc failure %SERVICE_NAME% reset= 86400 actions= restart/60000/restart/60000/restart/60000

    REM Démarrer le service
    echo Demarrage du service...
    net start %SERVICE_NAME%

    if %errorLevel% equ 0 (
        echo Service demarre avec succes
    ) else (
        echo Erreur lors du demarrage du service
    )
) else (
    echo Erreur lors de l'installation du service
)

REM Ajouter une règle de pare-feu
echo Configuration du pare-feu Windows...
netsh advfirewall firewall add rule name="MonitorAgent" dir=out action=allow program="%INSTALL_DIR%\monitoragent.exe" enable=yes

echo.
echo ========================================
echo Installation terminee
echo ========================================
echo.
echo Le service MonitorAgent a ete installe dans: %INSTALL_DIR%
echo Configuration: %CONFIG_FILE%
echo.
echo Pour verifier le statut: sc query %SERVICE_NAME%
echo Pour arreter le service: net stop %SERVICE_NAME%
echo Pour demarrer le service: net start %SERVICE_NAME%
echo.
pause
```

### Script de désinstallation Windows

```batch
@echo off
REM Script de désinstallation MonitorAgent pour Windows

echo ========================================
echo Desinstallation de MonitorAgent
echo ========================================

REM Vérifier les privilèges administrateur
net session >nul 2>&1
if %errorLevel% neq 0 (
    echo Ce script necessite les privileges administrateur
    pause
    exit /b 1
)

set SERVICE_NAME=MonitorAgent
set INSTALL_DIR=C:\Program Files\MonitorAgent

REM Arrêter le service
echo Arret du service...
net stop %SERVICE_NAME%

REM Attendre que le service soit complètement arrêté
timeout /t 2 /nobreak

REM Supprimer le service
echo Suppression du service...
sc delete %SERVICE_NAME%

REM Supprimer la règle de pare-feu
echo Suppression de la regle de pare-feu...
netsh advfirewall firewall delete rule name="MonitorAgent"

REM Demander si on doit supprimer les fichiers
set /p DELETE_FILES="Supprimer les fichiers d'installation? (O/N): "
if /i "%DELETE_FILES%"=="O" (
    echo Suppression des fichiers...
    rmdir /S /Q "%INSTALL_DIR%"
    echo Fichiers supprimes
) else (
    echo Fichiers conserves dans %INSTALL_DIR%
)

echo.
echo ========================================
echo Desinstallation terminee
echo ========================================
pause
```

### Script de déploiement Linux/Ubuntu

```bash
#!/bin/bash
# Script de déploiement MonitorAgent pour Linux/Ubuntu

set -e

echo "========================================"
echo "Installation de MonitorAgent"
echo "========================================"

# Vérifier les privilèges root
if [ "$EUID" -ne 0 ]; then
    echo "Ce script nécessite les privilèges root"
    echo "Veuillez exécuter avec sudo"
    exit 1
fi

# Définir les variables
INSTALL_DIR="/usr/local/bin"
CONFIG_DIR="/etc/monitoragent"
LOG_DIR="/var/log/monitoragent"
SERVICE_NAME="monitoragent"
SYSTEMD_DIR="/etc/systemd/system"

# Créer les répertoires nécessaires
echo "Création des répertoires..."
mkdir -p "$CONFIG_DIR"
mkdir -p "$LOG_DIR"

# Copier les fichiers
echo "Copie des fichiers..."
cp monitoragent "$INSTALL_DIR/"
chmod +x "$INSTALL_DIR/monitoragent"

# Créer le fichier de configuration
echo "Création du fichier de configuration..."
cat > "$CONFIG_DIR/config.ini" << EOF
[Server]
Host=monitoring.example.com
Port=9100
UseTLS=false

[Agent]
CollectionInterval=30
SendInterval=60

[Logging]
LogLevel=INFO
LogFile=$LOG_DIR/agent.log
EOF

# Créer le fichier de service systemd
echo "Création du service systemd..."
cat > "$SYSTEMD_DIR/$SERVICE_NAME.service" << EOF
[Unit]
Description=Monitor Agent Service
After=network.target
Wants=network-online.target

[Service]
Type=simple
User=root
ExecStart=$INSTALL_DIR/monitoragent -config $CONFIG_DIR/config.ini
Restart=on-failure
RestartSec=10
StandardOutput=journal
StandardError=journal
SyslogIdentifier=monitoragent

# Sécurité
NoNewPrivileges=true
PrivateTmp=true
ProtectSystem=strict
ProtectHome=true
ReadWritePaths=$LOG_DIR

[Install]
WantedBy=multi-user.target
EOF

# Recharger systemd
echo "Rechargement de systemd..."
systemctl daemon-reload

# Activer le service au démarrage
echo "Activation du service au démarrage..."
systemctl enable $SERVICE_NAME

# Démarrer le service
echo "Démarrage du service..."
systemctl start $SERVICE_NAME

# Vérifier le statut
sleep 2
if systemctl is-active --quiet $SERVICE_NAME; then
    echo "Service démarré avec succès"
    systemctl status $SERVICE_NAME --no-pager
else
    echo "Erreur lors du démarrage du service"
    journalctl -u $SERVICE_NAME -n 50 --no-pager
    exit 1
fi

# Configurer le pare-feu si ufw est installé
if command -v ufw &> /dev/null; then
    echo "Configuration du pare-feu UFW..."
    ufw allow out 9100/tcp comment 'MonitorAgent'
fi

echo ""
echo "========================================"
echo "Installation terminée"
echo "========================================"
echo ""
echo "Le service MonitorAgent a été installé"
echo "Configuration: $CONFIG_DIR/config.ini"
echo "Logs: $LOG_DIR/agent.log"
echo ""
echo "Commandes utiles:"
echo "  Statut:     systemctl status $SERVICE_NAME"
echo "  Arrêter:    systemctl stop $SERVICE_NAME"
echo "  Démarrer:   systemctl start $SERVICE_NAME"
echo "  Redémarrer: systemctl restart $SERVICE_NAME"
echo "  Logs:       journalctl -u $SERVICE_NAME -f"
echo ""
```

### Script de désinstallation Linux/Ubuntu

```bash
#!/bin/bash
# Script de désinstallation MonitorAgent pour Linux/Ubuntu

set -e

echo "========================================"
echo "Désinstallation de MonitorAgent"
echo "========================================"

# Vérifier les privilèges root
if [ "$EUID" -ne 0 ]; then
    echo "Ce script nécessite les privilèges root"
    exit 1
fi

SERVICE_NAME="monitoragent"
INSTALL_DIR="/usr/local/bin"
CONFIG_DIR="/etc/monitoragent"
LOG_DIR="/var/log/monitoragent"
SYSTEMD_DIR="/etc/systemd/system"

# Arrêter le service
echo "Arrêt du service..."
systemctl stop $SERVICE_NAME || true

# Désactiver le service
echo "Désactivation du service..."
systemctl disable $SERVICE_NAME || true

# Supprimer le fichier de service
echo "Suppression du fichier de service..."
rm -f "$SYSTEMD_DIR/$SERVICE_NAME.service"

# Recharger systemd
systemctl daemon-reload

# Supprimer l'exécutable
echo "Suppression de l'exécutable..."
rm -f "$INSTALL_DIR/monitoragent"

# Demander si on doit supprimer les fichiers de configuration
read -p "Supprimer les fichiers de configuration? (o/N): " DELETE_CONFIG
if [[ "$DELETE_CONFIG" =~ ^[oO]$ ]]; then
    echo "Suppression de la configuration..."
    rm -rf "$CONFIG_DIR"
    echo "Configuration supprimée"
else
    echo "Configuration conservée dans $CONFIG_DIR"
fi

# Demander si on doit supprimer les logs
read -p "Supprimer les logs? (o/N): " DELETE_LOGS
if [[ "$DELETE_LOGS" =~ ^[oO]$ ]]; then
    echo "Suppression des logs..."
    rm -rf "$LOG_DIR"
    echo "Logs supprimés"
else
    echo "Logs conservés dans $LOG_DIR"
fi

# Supprimer les règles de pare-feu si ufw est installé
if command -v ufw &> /dev/null; then
    echo "Suppression des règles de pare-feu..."
    ufw delete allow out 9100/tcp || true
fi

echo ""
echo "========================================"
echo "Désinstallation terminée"
echo "========================================"
```

## Configuration avancée

### Fichier de configuration INI complet

```ini
[Server]
# Adresse du serveur de monitoring
Host=monitoring.example.com
Port=9100

# Utiliser TLS/SSL pour la connexion
UseTLS=true
TLSCertFile=/etc/monitoragent/certs/client.crt
TLSKeyFile=/etc/monitoragent/certs/client.key
TLSCAFile=/etc/monitoragent/certs/ca.crt

# Timeout de connexion (secondes)
ConnectionTimeout=10
ReadTimeout=30
WriteTimeout=30

# Reconnexion automatique
AutoReconnect=true
ReconnectInterval=30
MaxReconnectAttempts=10

[Agent]
# Identifiant unique de l'agent (généré automatiquement si vide)
AgentID=

# Nom d'hôte personnalisé (utilise le hostname système si vide)
Hostname=

# Intervalle de collecte des métriques (secondes)
CollectionInterval=30

# Intervalle d'envoi des données (secondes)
SendInterval=60

# Mode batch: envoyer plusieurs métriques en une fois
BatchMode=true
BatchSize=10

# Buffer des métriques en cas de déconnexion
BufferMetrics=true
MaxBufferSize=1000

# Métriques à collecter (true/false)
CollectCPU=true
CollectMemory=true
CollectDisk=true
CollectNetwork=true
CollectProcesses=false
CollectServices=false

# Disques à surveiller (séparés par des virgules, * pour tous)
MonitoredDisks=*

# Interfaces réseau à surveiller (* pour toutes)
MonitoredInterfaces=*

[Logging]
# Niveau de log: DEBUG, INFO, WARNING, ERROR, CRITICAL
LogLevel=INFO

# Fichier de log (vide pour stdout/console)
LogFile=/var/log/monitoragent/agent.log

# Rotation des logs
LogRotate=true
LogMaxSize=10485760  # 10 MB
LogMaxBackups=5
LogMaxAge=30  # jours

# Format des logs: text, json
LogFormat=text

# Logs vers syslog (Linux uniquement)
LogToSyslog=false
SyslogFacility=daemon

[Security]
# Authentification par token
UseAuthentication=true
AuthToken=your-secret-token-here

# Chiffrement des données
EncryptData=false
EncryptionKey=

[Performance]
# Nombre de threads de collecte
CollectorThreads=2

# Priorité du processus (normal, low, high)
ProcessPriority=low

# Limiter l'utilisation CPU de l'agent (%)
MaxCPUUsage=5

# Limiter l'utilisation mémoire (MB)
MaxMemoryUsage=100

[Alerts]
# Alertes locales (indépendantes du serveur)
EnableLocalAlerts=true

# Seuils d'alerte CPU
CPUWarningThreshold=80
CPUCriticalThreshold=95

# Seuils d'alerte mémoire
MemoryWarningThreshold=85
MemoryCriticalThreshold=95

# Seuils d'alerte disque
DiskWarningThreshold=80
DiskCriticalThreshold=90

# Actions en cas d'alerte critique
AlertCommand=/usr/local/bin/alert-handler.sh

[Advanced]
# Mode debug
Debug=false

# Fichier PID (Linux)
PIDFile=/var/run/monitoragent.pid

# Utilisateur pour l'exécution (Linux, nécessite root pour changer)
RunAsUser=root

# Groupe pour l'exécution (Linux)
RunAsGroup=root
```

### Configuration côté serveur

```ini
[Server]
# Port d'écoute pour les agents
ListenPort=9100
ListenAddress=0.0.0.0

# TLS/SSL
UseTLS=true
TLSCertFile=/etc/monitorserver/certs/server.crt
TLSKeyFile=/etc/monitorserver/certs/server.key
TLSCAFile=/etc/monitorserver/certs/ca.crt

# Timeout
ClientTimeout=120

# Nombre maximum de connexions simultanées
MaxClients=1000

[Database]
# Type de base de données: sqlite, postgresql, mysql
Type=postgresql

# SQLite
SQLitePath=/var/lib/monitorserver/monitoring.db

# PostgreSQL
PGHost=localhost
PGPort=5432
PGDatabase=monitoring
PGUser=monitoruser
PGPassword=secure-password
PGSSLMode=require

# MySQL
MySQLHost=localhost
MySQLPort=3306
MySQLDatabase=monitoring
MySQLUser=monitoruser
MySQLPassword=secure-password

# Pool de connexions
DBPoolSize=10
DBMaxOpenConns=50

# Rétention des données
RetentionDays=90
CleanupInterval=86400  # secondes (1 jour)

[API]
# API REST
EnableAPI=true
APIPort=8080
APIAddress=0.0.0.0

# Authentification API
APIAuthentication=true
APITokens=/etc/monitorserver/api-tokens.txt

# CORS
EnableCORS=true
CORSOrigins=*

# Rate limiting
RateLimit=100  # requêtes par minute
RateLimitBurst=20

[Alerts]
# Système d'alertes
EnableAlerts=true

# Intervalle de vérification des alertes
CheckInterval=60

# Cooldown entre alertes identiques (secondes)
AlertCooldown=300

[Notifications]
# Email
EnableEmail=true
SMTPHost=smtp.gmail.com
SMTPPort=587
SMTPUser=alerts@example.com
SMTPPassword=app-password
SMTPFrom=monitoring@example.com
EmailTo=admin@example.com,ops@example.com

# Slack
EnableSlack=false
SlackWebhookURL=https://hooks.slack.com/services/YOUR/WEBHOOK/URL
SlackChannel=#monitoring

# Webhook personnalisé
EnableWebhook=false
WebhookURL=https://your-webhook-endpoint.com/alert
WebhookMethod=POST

# PagerDuty
EnablePagerDuty=false
PagerDutyIntegrationKey=your-integration-key

[Logging]
LogLevel=INFO
LogFile=/var/log/monitorserver/server.log
LogRotate=true
LogMaxSize=52428800  # 50 MB
LogMaxBackups=10

[Performance]
# Workers pour traiter les métriques
MetricsWorkers=4

# Workers pour les alertes
AlertWorkers=2

# Cache en mémoire
EnableCache=true
CacheSize=1000  # nombre d'éléments
CacheTTL=300  # secondes

[Advanced]
Debug=false
PIDFile=/var/run/monitorserver.pid
```

## Optimisations et bonnes pratiques

### Optimisation de la collecte des métriques

Pour éviter une surcharge du système, il est important d'optimiser la collecte :

```pascal
unit OptimizedMetricsCollector;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MonitorMetrics;

type
  TMetricsCache = class
  private
    FLastCPUMeasurement: TDateTime;
    FLastMemoryCheck: TDateTime;
    FCachedCPU: Double;
    FCachedMemory: Int64;
    FCacheTimeout: Integer; // en secondes
  public
    constructor Create(ACacheTimeout: Integer = 5);

    function GetCPU(Collector: TMetricsCollector): Double;
    function GetMemory(Collector: TMetricsCollector): Int64;
  end;

implementation

uses
  DateUtils;

constructor TMetricsCache.Create(ACacheTimeout: Integer);
begin
  inherited Create;
  FCacheTimeout := ACacheTimeout;
  FLastCPUMeasurement := 0;
  FLastMemoryCheck := 0;
  FCachedCPU := 0;
  FCachedMemory := 0;
end;

function TMetricsCache.GetCPU(Collector: TMetricsCollector): Double;
begin
  // Utiliser le cache si les données sont récentes
  if SecondsBetween(Now, FLastCPUMeasurement) < FCacheTimeout then
  begin
    Result := FCachedCPU;
  end
  else
  begin
    Result := Collector.GetCPUUsage;
    FCachedCPU := Result;
    FLastCPUMeasurement := Now;
  end;
end;

function TMetricsCache.GetMemory(Collector: TMetricsCollector): Int64;
var
  Total, Used, Available: Int64;
begin
  if SecondsBetween(Now, FLastMemoryCheck) < FCacheTimeout then
  begin
    Result := FCachedMemory;
  end
  else
  begin
    if Collector.GetMemoryInfo(Total, Used, Available) then
    begin
      Result := Used;
      FCachedMemory := Result;
      FLastMemoryCheck := Now;
    end
    else
      Result := FCachedMemory;
  end;
end;

end.
```

### Compression des données

Pour réduire la bande passante utilisée :

```pascal
unit DataCompression;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, zstream;

function CompressData(const Data: TBytes): TBytes;
function DecompressData(const Data: TBytes): TBytes;

implementation

function CompressData(const Data: TBytes): TBytes;
var
  InputStream: TMemoryStream;
  OutputStream: TMemoryStream;
  Compressor: TCompressionStream;
begin
  InputStream := TMemoryStream.Create;
  OutputStream := TMemoryStream.Create;
  try
    InputStream.Write(Data[0], Length(Data));
    InputStream.Position := 0;

    Compressor := TCompressionStream.Create(clMax, OutputStream);
    try
      Compressor.CopyFrom(InputStream, InputStream.Size);
    finally
      Compressor.Free;
    end;

    SetLength(Result, OutputStream.Size);
    OutputStream.Position := 0;
    OutputStream.Read(Result[0], OutputStream.Size);
  finally
    InputStream.Free;
    OutputStream.Free;
  end;
end;

function DecompressData(const Data: TBytes): TBytes;
var
  InputStream: TMemoryStream;
  OutputStream: TMemoryStream;
  Decompressor: TDecompressionStream;
begin
  InputStream := TMemoryStream.Create;
  OutputStream := TMemoryStream.Create;
  try
    InputStream.Write(Data[0], Length(Data));
    InputStream.Position := 0;

    Decompressor := TDecompressionStream.Create(InputStream);
    try
      OutputStream.CopyFrom(Decompressor, 0);
    finally
      Decompressor.Free;
    end;

    SetLength(Result, OutputStream.Size);
    OutputStream.Position := 0;
    OutputStream.Read(Result[0], OutputStream.Size);
  finally
    InputStream.Free;
    OutputStream.Free;
  end;
end;

end.
```

### Haute disponibilité du serveur

Configuration pour un serveur hautement disponible :

```pascal
unit HAMonitorServer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MonitorServer;

type
  TServerNode = record
    NodeID: string;
    IPAddress: string;
    Port: Integer;
    IsPrimary: Boolean;
    IsActive: Boolean;
    LastHeartbeat: TDateTime;
  end;

  THAMonitorServer = class(TMonitorServer)
  private
    FNodes: array of TServerNode;
    FThisNodeID: string;
    FIsPrimary: Boolean;
    FHeartbeatInterval: Integer;

    procedure SendHeartbeat;
    procedure CheckOtherNodes;
    procedure PromoteToPrimary;
    procedure DemoteToSecondary;
  public
    constructor Create(APort: Integer; const ANodeID: string);

    procedure RegisterNode(const NodeID, IPAddress: string; Port: Integer);
    procedure RunHA;
  end;

implementation

uses
  DateUtils;

constructor THAMonitorServer.Create(APort: Integer; const ANodeID: string);
begin
  inherited Create(APort);
  FThisNodeID := ANodeID;
  FIsPrimary := False;
  FHeartbeatInterval := 5; // secondes
  SetLength(FNodes, 0);
end;

procedure THAMonitorServer.RegisterNode(const NodeID, IPAddress: string; Port: Integer);
var
  Node: TServerNode;
begin
  SetLength(FNodes, Length(FNodes) + 1);
  Node.NodeID := NodeID;
  Node.IPAddress := IPAddress;
  Node.Port := Port;
  Node.IsPrimary := False;
  Node.IsActive := True;
  Node.LastHeartbeat := Now;
  FNodes[High(FNodes)] := Node;
end;

procedure THAMonitorServer.SendHeartbeat;
begin
  // Envoyer un heartbeat aux autres nœuds
  // Implémentation via TCP ou UDP multicast
  WriteLn('Envoi heartbeat depuis ', FThisNodeID);
end;

procedure THAMonitorServer.CheckOtherNodes;
var
  i: Integer;
  PrimaryFound: Boolean;
begin
  PrimaryFound := False;

  for i := 0 to High(FNodes) do
  begin
    // Vérifier si le nœud est toujours actif
    if SecondsBetween(Now, FNodes[i].LastHeartbeat) > (FHeartbeatInterval * 3) then
    begin
      FNodes[i].IsActive := False;
      WriteLn('Nœud ', FNodes[i].NodeID, ' est inactif');

      if FNodes[i].IsPrimary then
      begin
        FNodes[i].IsPrimary := False;
        WriteLn('Nœud primaire perdu');
      end;
    end;

    if FNodes[i].IsActive and FNodes[i].IsPrimary then
      PrimaryFound := True;
  end;

  // Si aucun primaire n'est trouvé, se promouvoir
  if not PrimaryFound and not FIsPrimary then
  begin
    WriteLn('Aucun nœud primaire trouvé, promotion');
    PromoteToPrimary;
  end;
end;

procedure THAMonitorServer.PromoteToPrimary;
begin
  FIsPrimary := True;
  WriteLn('Ce nœud est maintenant PRIMARY');
  // Démarrer l'acceptation des connexions clients
end;

procedure THAMonitorServer.DemoteToSecondary;
begin
  FIsPrimary := False;
  WriteLn('Ce nœud est maintenant SECONDARY');
  // Arrêter l'acceptation des connexions clients
end;

procedure THAMonitorServer.RunHA;
begin
  while FRunning do
  begin
    SendHeartbeat;
    CheckOtherNodes;
    Sleep(FHeartbeatInterval * 1000);
  end;
end;

end.
```

## Conclusion

Ce système de monitoring distribué offre une solution complète pour surveiller une infrastructure multi-plateforme. Les points clés du projet sont :

### Points forts du système

1. **Multi-plateforme** : Fonctionne nativement sur Windows et Linux/Ubuntu
2. **Léger et performant** : L'agent consomme très peu de ressources
3. **Extensible** : Architecture modulaire permettant d'ajouter facilement de nouvelles métriques
4. **Scalable** : Peut gérer des centaines d'agents simultanément
5. **Alertes intelligentes** : Système d'alertes avec cooldown et notifications multiples
6. **API REST** : Interface standard pour l'intégration avec d'autres outils
7. **Haute disponibilité** : Support de la redondance serveur

### Évolutions possibles

- **Support de métriques personnalisées** : Permettre aux utilisateurs de définir leurs propres métriques via plugins
- **Prédiction avec IA** : Utiliser le machine learning pour prédire les pannes
- **Visualisation avancée** : Dashboards interactifs avec drill-down
- **Support mobile** : Application mobile pour recevoir les alertes
- **Intégration cloud** : Support AWS CloudWatch, Azure Monitor, Google Cloud Monitoring
- **Correlation d'événements** : Détecter les patterns dans les incidents
- **Auto-scaling** : Ajuster automatiquement les ressources en fonction de la charge
- **Support Kubernetes** : Monitoring natif des pods et conteneurs
- **Métriques métier** : Au-delà des métriques système, collecter des KPI applicatifs

### Ressources complémentaires

- Documentation FreePascal : https://www.freepascal.org/docs.html
- Forum Lazarus : https://forum.lazarus.freepascal.org/
- Exemples Synapse : http://www.ararat.cz/synapse/
- SQLdb Wiki : https://wiki.freepascal.org/SQLdb_Tutorial
- TAChart Documentation : https://wiki.lazarus.freepascal.org/TAChart

Ce projet démontre la puissance de FreePascal/Lazarus pour créer des applications système professionnelles et portables.

⏭️ [Blockchain et smart contracts](/25-projets-complexes-etudes-cas/07-blockchain-smart-contracts.md)
