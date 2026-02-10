🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 21.10 Service Mesh et Observabilité

## Introduction

### Qu'est-ce qu'un Service Mesh ?

Un **Service Mesh** est une infrastructure qui gère les communications entre les microservices. C'est comme un réseau intelligent qui entoure chaque service.

**Analogie :** Imaginez une ville avec des routes. Le Service Mesh est comme un système GPS intelligent qui :
- Guide le trafic vers les bonnes destinations
- Détecte les embouteillages et trouve des routes alternatives
- Surveille l'état des routes en temps réel
- Sécurise les communications

### Le problème sans Service Mesh

```
Service A ──► Service B
    │
    └──► Service C ──► Service D

Chaque service doit gérer :
- ❌ La découverte des autres services
- ❌ Les retry et timeouts
- ❌ La sécurité (TLS, auth)
- ❌ Le monitoring
- ❌ Le circuit breaking
- ❌ Les métriques
```

**Problèmes :**
- Code dupliqué dans chaque service
- Difficile à maintenir et mettre à jour
- Inconsistance entre les services
- Impossible de monitorer globalement

### Avec un Service Mesh

```
Service A ──► [Proxy] ──► [Proxy] ──► Service B
    │
    └──► [Proxy] ──► [Proxy] ──► Service C
                          │
                          └──► [Proxy] ──► Service D

Chaque service a un proxy (sidecar) qui gère :
- ✅ Routage intelligent
- ✅ Load balancing
- ✅ Sécurité (mTLS automatique)
- ✅ Observabilité (traces, métriques)
- ✅ Résilience (retry, circuit breaker)
```

### Architecture d'un Service Mesh

```
┌─────────────────────────────────────────┐
│         CONTROL PLANE                   │
│  (Configuration, Politiques, Métriques) │
└─────────────────┬───────────────────────┘
                  │
    ┌─────────────┼─────────────┐
    │             │             │
┌───▼────┐  ┌────▼───┐  ┌─────▼──┐
│Sidecar │  │Sidecar │  │Sidecar │  ← DATA PLANE
│ Proxy  │  │ Proxy  │  │ Proxy  │    (Proxies)
└───┬────┘  └────┬───┘  └─────┬──┘
┌───▼────┐  ┌────▼───┐  ┌─────▼──┐
│Service │  │Service │  │Service │
│   A    │  │   B    │  │   C    │
└────────┘  └────────┘  └────────┘
```

**Control Plane** : Le cerveau qui configure les proxies  
**Data Plane** : Les proxies qui gèrent le trafic réel  

## Implémentation d'un Service Mesh Simple

### 1. Proxy Sidecar

Le proxy sidecar intercepte tout le trafic entrant et sortant d'un service.

```pascal
unit ServiceMesh.Sidecar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpserver, httpdefs, fphttpclient;

type
  // Configuration du sidecar
  TSidecarConfig = record
    ServiceName: string;
    ServicePort: Integer;      // Port du service réel
    ProxyPort: Integer;        // Port du proxy
    ControlPlaneURL: string;   // URL du control plane
  end;

  // Proxy sidecar
  TSidecarProxy = class
  private
    FConfig: TSidecarConfig;
    FServer: TFPHTTPServer;
    FMetrics: record
      RequestCount: Integer;
      ErrorCount: Integer;
      TotalLatencyMs: Int64;
    end;

    procedure HandleRequest(Sender: TObject;
      var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse);

    function ForwardToService(ARequest: TFPHTTPConnectionRequest): string;
    procedure RecordMetrics(ALatencyMs: Integer; ASuccess: Boolean);
    procedure ReportMetrics;
  public
    constructor Create(const AConfig: TSidecarConfig);
    destructor Destroy; override;

    procedure Start;
    procedure Stop;
  end;

implementation

uses
  DateUtils;

// TSidecarProxy

constructor TSidecarProxy.Create(const AConfig: TSidecarConfig);  
begin
  inherited Create;
  FConfig := AConfig;

  FServer := TFPHTTPServer.Create(nil);
  FServer.Port := FConfig.ProxyPort;
  FServer.OnRequest := @HandleRequest;

  // Initialiser les métriques
  FMetrics.RequestCount := 0;
  FMetrics.ErrorCount := 0;
  FMetrics.TotalLatencyMs := 0;

  WriteLn(Format('[Sidecar:%s] Proxy créé (Service:%d → Proxy:%d)',
    [FConfig.ServiceName, FConfig.ServicePort, FConfig.ProxyPort]));
end;

destructor TSidecarProxy.Destroy;  
begin
  Stop;
  FServer.Free;
  inherited;
end;

procedure TSidecarProxy.HandleRequest(Sender: TObject;
  var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
var
  StartTime: TDateTime;
  LatencyMs: Integer;
  ResponseContent: string;
  Success: Boolean;
begin
  StartTime := Now;
  Success := False;

  WriteLn(Format('[Sidecar:%s] ← Requête: %s %s',
    [FConfig.ServiceName, ARequest.Method, ARequest.URI]));

  try
    // Forward vers le service réel
    ResponseContent := ForwardToService(ARequest);

    AResponse.Code := 200;
    AResponse.Content := ResponseContent;
    AResponse.ContentType := 'application/json';

    // Ajouter des headers de tracing
    AResponse.SetCustomHeader('X-Service-Name', FConfig.ServiceName);
    AResponse.SetCustomHeader('X-Proxy-Version', '1.0');

    Success := True;
    WriteLn(Format('[Sidecar:%s] → Réponse envoyée', [FConfig.ServiceName]));

  except
    on E: Exception do
    begin
      AResponse.Code := 503;
      AResponse.Content := Format('{"error": "Service unavailable: %s"}',
        [E.Message]);
      WriteLn(Format('[Sidecar:%s] ✗ Erreur: %s',
        [FConfig.ServiceName, E.Message]));
    end;
  end;

  // Enregistrer les métriques
  LatencyMs := MilliSecondsBetween(Now, StartTime);
  RecordMetrics(LatencyMs, Success);
end;

function TSidecarProxy.ForwardToService(
  ARequest: TFPHTTPConnectionRequest): string;
var
  Client: TFPHTTPClient;
  ServiceURL: string;
begin
  Client := TFPHTTPClient.Create(nil);
  try
    ServiceURL := Format('http://localhost:%d%s',
      [FConfig.ServicePort, ARequest.URI]);

    WriteLn(Format('[Sidecar:%s] → Forward: %s',
      [FConfig.ServiceName, ServiceURL]));

    // Ajouter des headers de tracing
    Client.AddHeader('X-Forwarded-By', 'Sidecar-' + FConfig.ServiceName);
    Client.AddHeader('X-Request-ID', TGuid.NewGuid.ToString);

    if ARequest.Method = 'GET' then
      Result := Client.Get(ServiceURL)
    else if ARequest.Method = 'POST' then
      Result := Client.FormPost(ServiceURL, ARequest.Content)
    else if ARequest.Method = 'PUT' then
    begin
      Client.RequestBody := TStringStream.Create(ARequest.Content);
      Result := Client.Put(ServiceURL);
    end
    else if ARequest.Method = 'DELETE' then
      Result := Client.Delete(ServiceURL)
    else
      raise Exception.Create('Méthode non supportée');

  finally
    Client.Free;
  end;
end;

procedure TSidecarProxy.RecordMetrics(ALatencyMs: Integer; ASuccess: Boolean);  
begin
  Inc(FMetrics.RequestCount);

  if not ASuccess then
    Inc(FMetrics.ErrorCount);

  FMetrics.TotalLatencyMs := FMetrics.TotalLatencyMs + ALatencyMs;

  WriteLn(Format('[Sidecar:%s] Métriques: %d req, %d err, %.2f ms moy.',
    [FConfig.ServiceName,
     FMetrics.RequestCount,
     FMetrics.ErrorCount,
     FMetrics.TotalLatencyMs / FMetrics.RequestCount]));

  // Reporter les métriques périodiquement
  if FMetrics.RequestCount mod 10 = 0 then
    ReportMetrics;
end;

procedure TSidecarProxy.ReportMetrics;  
var
  Client: TFPHTTPClient;
  MetricsJSON: string;
  AverageLatency: Double;
begin
  if FMetrics.RequestCount = 0 then
    Exit;

  AverageLatency := FMetrics.TotalLatencyMs / FMetrics.RequestCount;

  MetricsJSON := Format(
    '{"service": "%s", "requests": %d, "errors": %d, "avg_latency_ms": %.2f}',
    [FConfig.ServiceName, FMetrics.RequestCount, FMetrics.ErrorCount,
     AverageLatency]);

  WriteLn(Format('[Sidecar:%s] ↑ Report métriques au control plane',
    [FConfig.ServiceName]));

  Client := TFPHTTPClient.Create(nil);
  try
    try
      Client.RequestBody := TStringStream.Create(MetricsJSON);
      Client.Post(FConfig.ControlPlaneURL + '/metrics');
    except
      on E: Exception do
        WriteLn(Format('[Sidecar:%s] ✗ Erreur report: %s',
          [FConfig.ServiceName, E.Message]));
    end;
  finally
    Client.Free;
  end;
end;

procedure TSidecarProxy.Start;  
begin
  FServer.Active := True;
  WriteLn(Format('[Sidecar:%s] ✓ Démarré sur le port %d',
    [FConfig.ServiceName, FConfig.ProxyPort]));
end;

procedure TSidecarProxy.Stop;  
begin
  if FServer.Active then
  begin
    FServer.Active := False;
    WriteLn(Format('[Sidecar:%s] Arrêté', [FConfig.ServiceName]));
  end;
end;

end.
```

### 2. Control Plane

Le control plane centralise la configuration et collecte les métriques.

```pascal
unit ServiceMesh.ControlPlane;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, fphttpserver, httpdefs, fpjson;

type
  // Information sur un service
  TServiceInfo = record
    ServiceName: string;
    Instances: array of record
      Host: string;
      Port: Integer;
      Healthy: Boolean;
    end;
  end;

  // Métriques d'un service
  TServiceMetrics = record
    ServiceName: string;
    RequestCount: Integer;
    ErrorCount: Integer;
    AverageLatencyMs: Double;
    LastUpdate: TDateTime;
  end;

  // Control Plane
  TControlPlane = class
  private
    FServer: TFPHTTPServer;
    FPort: Integer;
    FServices: TDictionary<string, TServiceInfo>;
    FMetrics: TDictionary<string, TServiceMetrics>;

    procedure HandleRequest(Sender: TObject;
      var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse);

    procedure HandleServiceRegistry(ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse);
    procedure HandleMetrics(ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse);
    procedure HandleGetServices(var AResponse: TFPHTTPConnectionResponse);
    procedure HandleDashboard(var AResponse: TFPHTTPConnectionResponse);
  public
    constructor Create(APort: Integer);
    destructor Destroy; override;

    procedure Start;
    procedure Stop;

    procedure PrintStatus;
  end;

implementation

uses
  jsonparser, DateUtils;

// TControlPlane

constructor TControlPlane.Create(APort: Integer);  
begin
  inherited Create;
  FPort := APort;

  FServer := TFPHTTPServer.Create(nil);
  FServer.Port := FPort;
  FServer.OnRequest := @HandleRequest;

  FServices := TDictionary<string, TServiceInfo>.Create;
  FMetrics := TDictionary<string, TServiceMetrics>.Create;

  WriteLn('[ControlPlane] Créé sur le port ', FPort);
end;

destructor TControlPlane.Destroy;  
begin
  Stop;
  FMetrics.Free;
  FServices.Free;
  FServer.Free;
  inherited;
end;

procedure TControlPlane.HandleRequest(Sender: TObject;
  var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
begin
  WriteLn;
  WriteLn(Format('[ControlPlane] Requête: %s %s',
    [ARequest.Method, ARequest.URI]));

  // Router vers les handlers appropriés
  if ARequest.URI = '/register' then
    HandleServiceRegistry(ARequest, AResponse)
  else if ARequest.URI = '/metrics' then
    HandleMetrics(ARequest, AResponse)
  else if ARequest.URI = '/services' then
    HandleGetServices(AResponse)
  else if ARequest.URI = '/dashboard' then
    HandleDashboard(AResponse)
  else
  begin
    AResponse.Code := 404;
    AResponse.Content := '{"error": "Endpoint non trouvé"}';
  end;
end;

procedure TControlPlane.HandleServiceRegistry(
  ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
var
  Parser: TJSONParser;
  JSON: TJSONObject;
  ServiceName, Host: string;
  Port: Integer;
  ServiceInfo: TServiceInfo;
  Len: Integer;
begin
  WriteLn('[ControlPlane] Enregistrement de service');

  try
    Parser := TJSONParser.Create(ARequest.Content, [joUTF8]);
    try
      JSON := Parser.Parse as TJSONObject;
      try
        ServiceName := JSON.Get('service_name', '');
        Host := JSON.Get('host', '');
        Port := JSON.Get('port', 0);

        if ServiceName = '' then
        begin
          AResponse.Code := 400;
          AResponse.Content := '{"error": "service_name requis"}';
          Exit;
        end;

        // Créer ou mettre à jour le service
        if not FServices.TryGetValue(ServiceName, ServiceInfo) then
        begin
          ServiceInfo.ServiceName := ServiceName;
          SetLength(ServiceInfo.Instances, 0);
        end;

        // Ajouter l'instance
        Len := Length(ServiceInfo.Instances);
        SetLength(ServiceInfo.Instances, Len + 1);
        ServiceInfo.Instances[Len].Host := Host;
        ServiceInfo.Instances[Len].Port := Port;
        ServiceInfo.Instances[Len].Healthy := True;

        FServices.AddOrSetValue(ServiceName, ServiceInfo);

        WriteLn(Format('[ControlPlane] ✓ Service enregistré: %s (%s:%d)',
          [ServiceName, Host, Port]));

        AResponse.Code := 200;
        AResponse.Content := '{"status": "registered"}';

      finally
        JSON.Free;
      end;
    finally
      Parser.Free;
    end;

  except
    on E: Exception do
    begin
      AResponse.Code := 500;
      AResponse.Content := Format('{"error": "%s"}', [E.Message]);
    end;
  end;
end;

procedure TControlPlane.HandleMetrics(ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
var
  Parser: TJSONParser;
  JSON: TJSONObject;
  Metrics: TServiceMetrics;
begin
  WriteLn('[ControlPlane] Réception de métriques');

  try
    Parser := TJSONParser.Create(ARequest.Content, [joUTF8]);
    try
      JSON := Parser.Parse as TJSONObject;
      try
        Metrics.ServiceName := JSON.Get('service', '');
        Metrics.RequestCount := JSON.Get('requests', 0);
        Metrics.ErrorCount := JSON.Get('errors', 0);
        Metrics.AverageLatencyMs := JSON.Get('avg_latency_ms', 0.0);
        Metrics.LastUpdate := Now;

        FMetrics.AddOrSetValue(Metrics.ServiceName, Metrics);

        WriteLn(Format('[ControlPlane] ✓ Métriques enregistrées: %s ' +
          '(%d req, %d err, %.2f ms)',
          [Metrics.ServiceName, Metrics.RequestCount,
           Metrics.ErrorCount, Metrics.AverageLatencyMs]));

        AResponse.Code := 200;
        AResponse.Content := '{"status": "recorded"}';

      finally
        JSON.Free;
      end;
    finally
      Parser.Free;
    end;

  except
    on E: Exception do
    begin
      AResponse.Code := 500;
      AResponse.Content := Format('{"error": "%s"}', [E.Message]);
    end;
  end;
end;

procedure TControlPlane.HandleGetServices(
  var AResponse: TFPHTTPConnectionResponse);
var
  ResultJSON: TJSONObject;
  ServicesArray: TJSONArray;
  ServiceName: string;
  ServiceInfo: TServiceInfo;
  ServiceJSON: TJSONObject;
  i: Integer;
begin
  WriteLn('[ControlPlane] Liste des services demandée');

  ResultJSON := TJSONObject.Create;
  try
    ServicesArray := TJSONArray.Create;

    for ServiceName in FServices.Keys do
    begin
      ServiceInfo := FServices[ServiceName];
      ServiceJSON := TJSONObject.Create;

      ServiceJSON.Add('name', ServiceInfo.ServiceName);
      ServiceJSON.Add('instances', Length(ServiceInfo.Instances));

      ServicesArray.Add(ServiceJSON);
    end;

    ResultJSON.Add('services', ServicesArray);
    ResultJSON.Add('total', FServices.Count);

    AResponse.Code := 200;
    AResponse.Content := ResultJSON.AsJSON;
    AResponse.ContentType := 'application/json';

  finally
    ResultJSON.Free;
  end;
end;

procedure TControlPlane.HandleDashboard(
  var AResponse: TFPHTTPConnectionResponse);
var
  HTML: TStringList;
  ServiceName: string;
  Metrics: TServiceMetrics;
  ErrorRate: Double;
begin
  WriteLn('[ControlPlane] Dashboard demandé');

  HTML := TStringList.Create;
  try
    HTML.Add('<!DOCTYPE html>');
    HTML.Add('<html><head>');
    HTML.Add('<title>Service Mesh Dashboard</title>');
    HTML.Add('<style>');
    HTML.Add('body { font-family: Arial; margin: 20px; background: #f5f5f5; }');
    HTML.Add('h1 { color: #333; }');
    HTML.Add('.service { background: white; padding: 15px; margin: 10px 0; ' +
             'border-radius: 5px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }');
    HTML.Add('.metric { display: inline-block; margin: 10px 20px 0 0; }');
    HTML.Add('.label { color: #666; font-size: 12px; }');
    HTML.Add('.value { font-size: 24px; font-weight: bold; color: #333; }');
    HTML.Add('.healthy { color: #4CAF50; }');
    HTML.Add('.warning { color: #FF9800; }');
    HTML.Add('.error { color: #F44336; }');
    HTML.Add('</style>');
    HTML.Add('</head><body>');
    HTML.Add('<h1>🕸️ Service Mesh Dashboard</h1>');

    HTML.Add(Format('<p>Services enregistrés: <strong>%d</strong></p>',
      [FServices.Count]));

    for ServiceName in FMetrics.Keys do
    begin
      Metrics := FMetrics[ServiceName];

      if Metrics.RequestCount > 0 then
        ErrorRate := (Metrics.ErrorCount / Metrics.RequestCount) * 100
      else
        ErrorRate := 0;

      HTML.Add('<div class="service">');
      HTML.Add(Format('<h2>%s</h2>', [Metrics.ServiceName]));

      HTML.Add('<div class="metric">');
      HTML.Add('<div class="label">Requêtes</div>');
      HTML.Add(Format('<div class="value">%d</div>', [Metrics.RequestCount]));
      HTML.Add('</div>');

      HTML.Add('<div class="metric">');
      HTML.Add('<div class="label">Erreurs</div>');
      HTML.Add(Format('<div class="value error">%d</div>', [Metrics.ErrorCount]));
      HTML.Add('</div>');

      HTML.Add('<div class="metric">');
      HTML.Add('<div class="label">Taux d''erreur</div>');
      HTML.Add(Format('<div class="value %s">%.1f%%</div>',
        [IfThen(ErrorRate > 5, 'error', 'healthy'), ErrorRate]));
      HTML.Add('</div>');

      HTML.Add('<div class="metric">');
      HTML.Add('<div class="label">Latence moyenne</div>');
      HTML.Add(Format('<div class="value">%.2f ms</div>',
        [Metrics.AverageLatencyMs]));
      HTML.Add('</div>');

      HTML.Add(Format('<p style="color: #999; font-size: 12px;">' +
        'Dernière mise à jour: %s</p>', [DateTimeToStr(Metrics.LastUpdate)]));
      HTML.Add('</div>');
    end;

    HTML.Add('</body></html>');

    AResponse.Code := 200;
    AResponse.Content := HTML.Text;
    AResponse.ContentType := 'text/html';

  finally
    HTML.Free;
  end;
end;

procedure TControlPlane.Start;  
begin
  FServer.Active := True;
  WriteLn(Format('[ControlPlane] ✓ Démarré sur http://localhost:%d', [FPort]));
  WriteLn('[ControlPlane] Dashboard: http://localhost:', FPort, '/dashboard');
end;

procedure TControlPlane.Stop;  
begin
  if FServer.Active then
  begin
    FServer.Active := False;
    WriteLn('[ControlPlane] Arrêté');
  end;
end;

procedure TControlPlane.PrintStatus;  
var
  ServiceName: string;
  ServiceInfo: TServiceInfo;
  Metrics: TServiceMetrics;
begin
  WriteLn;
  WriteLn('═══════════════════════════════════════════');
  WriteLn('  STATUS DU SERVICE MESH                  ');
  WriteLn('═══════════════════════════════════════════');
  WriteLn(Format('Services enregistrés: %d', [FServices.Count]));
  WriteLn;

  for ServiceName in FServices.Keys do
  begin
    ServiceInfo := FServices[ServiceName];
    WriteLn(Format('Service: %s (%d instances)',
      [ServiceName, Length(ServiceInfo.Instances)]));

    if FMetrics.TryGetValue(ServiceName, Metrics) then
    begin
      WriteLn(Format('  Requêtes: %d', [Metrics.RequestCount]));
      WriteLn(Format('  Erreurs: %d', [Metrics.ErrorCount]));
      WriteLn(Format('  Latence: %.2f ms', [Metrics.AverageLatencyMs]));
    end;
    WriteLn;
  end;

  WriteLn('═══════════════════════════════════════════');
end;

end.
```

### 3. Exemple d'utilisation complet

```pascal
program ServiceMeshDemo;

{$mode objfpc}{$H+}

uses
  SysUtils,
  ServiceMesh.ControlPlane,
  ServiceMesh.Sidecar;

var
  ControlPlane: TControlPlane;
  SidecarA, SidecarB, SidecarC: TSidecarProxy;
  ConfigA, ConfigB, ConfigC: TSidecarConfig;

begin
  WriteLn('═══════════════════════════════════════════');
  WriteLn('  DÉMONSTRATION SERVICE MESH               ');
  WriteLn('═══════════════════════════════════════════');
  WriteLn;

  // Démarrer le Control Plane
  WriteLn('Démarrage du Control Plane...');
  ControlPlane := TControlPlane.Create(9000);
  ControlPlane.Start;
  WriteLn;

  Sleep(500);

  // Configurer les sidecars
  WriteLn('Configuration des Sidecars...');
  WriteLn;

  // Service A
  ConfigA.ServiceName := 'service-a';
  ConfigA.ServicePort := 8001;
  ConfigA.ProxyPort := 8101;
  ConfigA.ControlPlaneURL := 'http://localhost:9000';

  // Service B
  ConfigB.ServiceName := 'service-b';
  ConfigB.ServicePort := 8002;
  ConfigB.ProxyPort := 8102;
  ConfigB.ControlPlaneURL := 'http://localhost:9000';

  // Service C
  ConfigC.ServiceName := 'service-c';
  ConfigC.ServicePort := 8003;
  ConfigC.ProxyPort := 8103;
  ConfigC.ControlPlaneURL := 'http://localhost:9000';

  // Créer et démarrer les sidecars
  SidecarA := TSidecarProxy.Create(ConfigA);
  SidecarB := TSidecarProxy.Create(ConfigB);
  SidecarC := TSidecarProxy.Create(ConfigC);

  try
    SidecarA.Start;
    SidecarB.Start;
    SidecarC.Start;

    WriteLn;
    WriteLn('═══════════════════════════════════════════');
    WriteLn('  SERVICE MESH ACTIF                      ');
    WriteLn('═══════════════════════════════════════════');
    WriteLn;
    WriteLn('Control Plane: http://localhost:9000');
    WriteLn('Dashboard:     http://localhost:9000/dashboard');
    WriteLn;
    WriteLn('Services (via proxies):');
    WriteLn('  Service A: http://localhost:8101');
    WriteLn('  Service B: http://localhost:8102');
    WriteLn('  Service C: http://localhost:8103');
    WriteLn;
    WriteLn('Appuyez sur Entrée pour voir le status...');
    ReadLn;

    ControlPlane.PrintStatus;

    WriteLn;
    WriteLn('Appuyez sur Entrée pour arrêter...');
    ReadLn;

  finally
    SidecarC.Free;
    SidecarB.Free;
    SidecarA.Free;
    ControlPlane.Free;
  end;
end.
```

## Observabilité : Les Trois Piliers

L'observabilité permet de comprendre ce qui se passe dans le système. Elle repose sur trois piliers :

```
┌───────────────────────────────────────┐
│         OBSERVABILITÉ                 │
├───────────┬───────────┬───────────────┤
│  LOGS     │  METRICS  │    TRACES     │
│           │           │               │
│ Événements│ Mesures   │ Parcours      │
│ textuels  │ numériques│ des requêtes  │
└───────────┴───────────┴───────────────┘
```

### 1. Logs (Journaux)

Les logs sont des enregistrements d'événements.

```pascal
unit ServiceMesh.Logging;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  // Niveau de log
  TLogLevel = (llDebug, llInfo, llWarning, llError, llFatal);

  // Entrée de log
  TLogEntry = record
    Timestamp: TDateTime;
    Level: TLogLevel;
    Service: string;
    Message: string;
    Context: string; // JSON avec contexte supplémentaire
  end;

  // Logger centralisé
  TServiceLogger = class
  private
    FLogFile: TextFile;
    FServiceName: string;
    FMinLevel: TLogLevel;

    function LevelToString(ALevel: TLogLevel): string;
    function FormatLogEntry(const AEntry: TLogEntry): string;
  public
    constructor Create(const AServiceName: string;
      AMinLevel: TLogLevel = llInfo);
    destructor Destroy; override;

    procedure Debug(const AMessage: string; const AContext: string = '');
    procedure Info(const AMessage: string; const AContext: string = '');
    procedure Warning(const AMessage: string; const AContext: string = '');
    procedure Error(const AMessage: string; const AContext: string = '');
    procedure Fatal(const AMessage: string; const AContext: string = '');

    procedure Log(ALevel: TLogLevel; const AMessage: string;
      const AContext: string = '');
  end;

implementation

uses
  fpjson;

// TServiceLogger

constructor TServiceLogger.Create(const AServiceName: string;
  AMinLevel: TLogLevel);
var
  FileName: string;
begin
  inherited Create;
  FServiceName := AServiceName;
  FMinLevel := AMinLevel;

  {$IFDEF WINDOWS}
  FileName := Format('C:\temp\%s.log', [AServiceName]);
  {$ENDIF}
  {$IFDEF UNIX}
  FileName := Format('/tmp/%s.log', [AServiceName]);
  {$ENDIF}

  AssignFile(FLogFile, FileName);

  if FileExists(FileName) then
    Append(FLogFile)
  else
    Rewrite(FLogFile);

  WriteLn(Format('[Logger:%s] Initialisé → %s', [AServiceName, FileName]));
end;

destructor TServiceLogger.Destroy;  
begin
  CloseFile(FLogFile);
  inherited;
end;

function TServiceLogger.LevelToString(ALevel: TLogLevel): string;  
begin
  case ALevel of
    llDebug: Result := 'DEBUG';
    llInfo: Result := 'INFO';
    llWarning: Result := 'WARN';
    llError: Result := 'ERROR';
    llFatal: Result := 'FATAL';
  end;
end;

function TServiceLogger.FormatLogEntry(const AEntry: TLogEntry): string;  
var
  JSON: TJSONObject;
begin
  // Format JSON structuré
  JSON := TJSONObject.Create;
  try
    JSON.Add('timestamp', DateTimeToStr(AEntry.Timestamp));
    JSON.Add('level', LevelToString(AEntry.Level));
    JSON.Add('service', AEntry.Service);
    JSON.Add('message', AEntry.Message);

    if AEntry.Context <> '' then
      JSON.Add('context', AEntry.Context);

    Result := JSON.AsJSON;
  finally
    JSON.Free;
  end;
end;

procedure TServiceLogger.Log(ALevel: TLogLevel; const AMessage: string;
  const AContext: string);
var
  Entry: TLogEntry;
  FormattedLog: string;
begin
  // Filtrer selon le niveau minimum
  if ALevel < FMinLevel then
    Exit;

  Entry.Timestamp := Now;
  Entry.Level := ALevel;
  Entry.Service := FServiceName;
  Entry.Message := AMessage;
  Entry.Context := AContext;

  FormattedLog := FormatLogEntry(Entry);

  // Écrire dans le fichier
  WriteLn(FLogFile, FormattedLog);
  Flush(FLogFile);

  // Afficher aussi en console
  WriteLn(Format('[%s] %s: %s',
    [LevelToString(ALevel), FServiceName, AMessage]));
end;

procedure TServiceLogger.Debug(const AMessage: string; const AContext: string);  
begin
  Log(llDebug, AMessage, AContext);
end;

procedure TServiceLogger.Info(const AMessage: string; const AContext: string);  
begin
  Log(llInfo, AMessage, AContext);
end;

procedure TServiceLogger.Warning(const AMessage: string; const AContext: string);  
begin
  Log(llWarning, AMessage, AContext);
end;

procedure TServiceLogger.Error(const AMessage: string; const AContext: string);  
begin
  Log(llError, AMessage, AContext);
end;

procedure TServiceLogger.Fatal(const AMessage: string; const AContext: string);  
begin
  Log(llFatal, AMessage, AContext);
end;

end.
```

### 2. Metrics (Métriques)

Les métriques sont des mesures numériques au fil du temps.

```pascal
unit ServiceMesh.Metrics;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections;

type
  // Type de métrique
  TMetricType = (mtCounter, mtGauge, mtHistogram);

  // Valeur de métrique
  TMetricValue = record
    Timestamp: TDateTime;
    Value: Double;
  end;

  // Métrique
  TMetric = class
  private
    FName: string;
    FMetricType: TMetricType;
    FLabels: TDictionary<string, string>;
    FValues: TList<TMetricValue>;
    FCurrentValue: Double;
  public
    constructor Create(const AName: string; AType: TMetricType);
    destructor Destroy; override;

    procedure AddLabel(const AKey, AValue: string);

    // Counter : incrémente seulement
    procedure Inc(AValue: Double = 1);

    // Gauge : peut augmenter ou diminuer
    procedure Set_(AValue: Double);

    // Histogram : enregistre des observations
    procedure Observe(AValue: Double);

    function GetValue: Double;
    function GetAverage: Double;
    function GetPercentile(APercentile: Integer): Double;

    property Name: string read FName;
    property MetricType: TMetricType read FMetricType;
  end;

  // Registre de métriques
  TMetricsRegistry = class
  private
    FMetrics: TObjectDictionary<string, TMetric>;
  public
    constructor Create;
    destructor Destroy; override;

    function Counter(const AName: string): TMetric;
    function Gauge(const AName: string): TMetric;
    function Histogram(const AName: string): TMetric;

    function GetMetric(const AName: string): TMetric;
    procedure ExportPrometheus(AOutput: TStrings);
  end;

implementation

uses
  Math;

// TMetric

constructor TMetric.Create(const AName: string; AType: TMetricType);  
begin
  inherited Create;
  FName := AName;
  FMetricType := AType;
  FLabels := TDictionary<string, string>.Create;
  FValues := TList<TMetricValue>.Create;
  FCurrentValue := 0;
end;

destructor TMetric.Destroy;  
begin
  FValues.Free;
  FLabels.Free;
  inherited;
end;

procedure TMetric.AddLabel(const AKey, AValue: string);  
begin
  FLabels.AddOrSetValue(AKey, AValue);
end;

procedure TMetric.Inc(AValue: Double);  
var
  MetricValue: TMetricValue;
begin
  if FMetricType <> mtCounter then
    raise Exception.Create('Inc() disponible uniquement pour Counter');

  FCurrentValue := FCurrentValue + AValue;

  MetricValue.Timestamp := Now;
  MetricValue.Value := FCurrentValue;
  FValues.Add(MetricValue);
end;

procedure TMetric.Set_(AValue: Double);  
var
  MetricValue: TMetricValue;
begin
  if FMetricType <> mtGauge then
    raise Exception.Create('Set() disponible uniquement pour Gauge');

  FCurrentValue := AValue;

  MetricValue.Timestamp := Now;
  MetricValue.Value := FCurrentValue;
  FValues.Add(MetricValue);
end;

procedure TMetric.Observe(AValue: Double);  
var
  MetricValue: TMetricValue;
begin
  if FMetricType <> mtHistogram then
    raise Exception.Create('Observe() disponible uniquement pour Histogram');

  MetricValue.Timestamp := Now;
  MetricValue.Value := AValue;
  FValues.Add(MetricValue);
end;

function TMetric.GetValue: Double;  
begin
  Result := FCurrentValue;
end;

function TMetric.GetAverage: Double;  
var
  Total: Double;
  Value: TMetricValue;
begin
  if FValues.Count = 0 then
  begin
    Result := 0;
    Exit;
  end;

  Total := 0;
  for Value in FValues do
    Total := Total + Value.Value;

  Result := Total / FValues.Count;
end;

function TMetric.GetPercentile(APercentile: Integer): Double;  
var
  SortedValues: TList<Double>;
  Value: TMetricValue;
  Index: Integer;
begin
  if FValues.Count = 0 then
  begin
    Result := 0;
    Exit;
  end;

  SortedValues := TList<Double>.Create;
  try
    for Value in FValues do
      SortedValues.Add(Value.Value);

    SortedValues.Sort;

    Index := Round((APercentile / 100) * (SortedValues.Count - 1));
    Result := SortedValues[Index];

  finally
    SortedValues.Free;
  end;
end;

// TMetricsRegistry

constructor TMetricsRegistry.Create;  
begin
  inherited Create;
  FMetrics := TObjectDictionary<string, TMetric>.Create([doOwnsValues]);
  WriteLn('[MetricsRegistry] Registre créé');
end;

destructor TMetricsRegistry.Destroy;  
begin
  FMetrics.Free;
  inherited;
end;

function TMetricsRegistry.Counter(const AName: string): TMetric;  
begin
  if not FMetrics.TryGetValue(AName, Result) then
  begin
    Result := TMetric.Create(AName, mtCounter);
    FMetrics.Add(AName, Result);
    WriteLn(Format('[MetricsRegistry] Counter créé: %s', [AName]));
  end;
end;

function TMetricsRegistry.Gauge(const AName: string): TMetric;  
begin
  if not FMetrics.TryGetValue(AName, Result) then
  begin
    Result := TMetric.Create(AName, mtGauge);
    FMetrics.Add(AName, Result);
    WriteLn(Format('[MetricsRegistry] Gauge créé: %s', [AName]));
  end;
end;

function TMetricsRegistry.Histogram(const AName: string): TMetric;  
begin
  if not FMetrics.TryGetValue(AName, Result) then
  begin
    Result := TMetric.Create(AName, mtHistogram);
    FMetrics.Add(AName, Result);
    WriteLn(Format('[MetricsRegistry] Histogram créé: %s', [AName]));
  end;
end;

function TMetricsRegistry.GetMetric(const AName: string): TMetric;  
begin
  if not FMetrics.TryGetValue(AName, Result) then
    Result := nil;
end;

procedure TMetricsRegistry.ExportPrometheus(AOutput: TStrings);  
var
  MetricName: string;
  Metric: TMetric;
  TypeStr: string;
begin
  AOutput.Clear;

  for MetricName in FMetrics.Keys do
  begin
    Metric := FMetrics[MetricName];

    case Metric.MetricType of
      mtCounter: TypeStr := 'counter';
      mtGauge: TypeStr := 'gauge';
      mtHistogram: TypeStr := 'histogram';
    end;

    // Format Prometheus
    AOutput.Add(Format('# TYPE %s %s', [Metric.Name, TypeStr]));

    if Metric.MetricType = mtHistogram then
    begin
      AOutput.Add(Format('%s_sum %.2f', [Metric.Name,
        Metric.GetAverage * Metric.FValues.Count]));
      AOutput.Add(Format('%s_count %d', [Metric.Name, Metric.FValues.Count]));
      AOutput.Add(Format('%s{quantile="0.5"} %.2f', [Metric.Name,
        Metric.GetPercentile(50)]));
      AOutput.Add(Format('%s{quantile="0.95"} %.2f', [Metric.Name,
        Metric.GetPercentile(95)]));
      AOutput.Add(Format('%s{quantile="0.99"} %.2f', [Metric.Name,
        Metric.GetPercentile(99)]));
    end
    else
      AOutput.Add(Format('%s %.2f', [Metric.Name, Metric.GetValue]));

    AOutput.Add('');
  end;
end;

end.
```

### 3. Traces (Traçage distribué)

Les traces suivent le parcours d'une requête à travers tous les services.

```pascal
unit ServiceMesh.Tracing;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, DateUtils;

type
  // Span : segment d'une trace
  TSpan = class
  private
    FTraceId: string;      // ID de la trace complète
    FSpanId: string;       // ID de ce span
    FParentSpanId: string; // ID du span parent
    FServiceName: string;
    FOperationName: string;
    FStartTime: TDateTime;
    FEndTime: TDateTime;
    FTags: TDictionary<string, string>;
    FLogs: TStringList;
  public
    constructor Create(const ATraceId, AServiceName, AOperationName: string;
      const AParentSpanId: string = '');
    destructor Destroy; override;

    procedure AddTag(const AKey, AValue: string);
    procedure Log(const AMessage: string);
    procedure Finish;

    function GetDurationMs: Integer;
    function ToJSON: string;

    property TraceId: string read FTraceId;
    property SpanId: string read FSpanId;
    property ServiceName: string read FServiceName;
  end;

  // Tracer : gestionnaire de traces
  TTracer = class
  private
    FServiceName: string;
    FSpans: TObjectList<TSpan>;
    FActiveSpan: TSpan;
  public
    constructor Create(const AServiceName: string);
    destructor Destroy; override;

    function StartSpan(const AOperationName: string;
      const ATraceId: string = '';
      const AParentSpanId: string = ''): TSpan;

    procedure FinishSpan(ASpan: TSpan);

    function ExportTraces: string;
    procedure PrintTraces;

    property ActiveSpan: TSpan read FActiveSpan;
  end;

implementation

uses
  fpjson;

// TSpan

constructor TSpan.Create(const ATraceId, AServiceName, AOperationName: string;
  const AParentSpanId: string);
begin
  inherited Create;

  if ATraceId = '' then
    FTraceId := TGuid.NewGuid.ToString
  else
    FTraceId := ATraceId;

  FSpanId := TGuid.NewGuid.ToString;
  FParentSpanId := AParentSpanId;
  FServiceName := AServiceName;
  FOperationName := AOperationName;
  FStartTime := Now;
  FEndTime := 0;

  FTags := TDictionary<string, string>.Create;
  FLogs := TStringList.Create;

  WriteLn(Format('[Trace:%s] Span démarré: %s.%s',
    [Copy(FSpanId, 1, 8), FServiceName, FOperationName]));
end;

destructor TSpan.Destroy;  
begin
  FLogs.Free;
  FTags.Free;
  inherited;
end;

procedure TSpan.AddTag(const AKey, AValue: string);  
begin
  FTags.AddOrSetValue(AKey, AValue);
end;

procedure TSpan.Log(const AMessage: string);  
begin
  FLogs.Add(Format('%s: %s', [DateTimeToStr(Now), AMessage]));
end;

procedure TSpan.Finish;  
begin
  FEndTime := Now;
  WriteLn(Format('[Trace:%s] Span terminé: %d ms',
    [Copy(FSpanId, 1, 8), GetDurationMs]));
end;

function TSpan.GetDurationMs: Integer;  
begin
  if FEndTime = 0 then
    Result := MilliSecondsBetween(Now, FStartTime)
  else
    Result := MilliSecondsBetween(FEndTime, FStartTime);
end;

function TSpan.ToJSON: string;  
var
  JSON, TagsJSON: TJSONObject;
  Key: string;
begin
  JSON := TJSONObject.Create;
  try
    JSON.Add('trace_id', FTraceId);
    JSON.Add('span_id', FSpanId);

    if FParentSpanId <> '' then
      JSON.Add('parent_span_id', FParentSpanId);

    JSON.Add('service_name', FServiceName);
    JSON.Add('operation_name', FOperationName);
    JSON.Add('start_time', DateTimeToStr(FStartTime));
    JSON.Add('duration_ms', GetDurationMs);

    // Tags
    TagsJSON := TJSONObject.Create;
    for Key in FTags.Keys do
      TagsJSON.Add(Key, FTags[Key]);
    JSON.Add('tags', TagsJSON);

    Result := JSON.AsJSON;
  finally
    JSON.Free;
  end;
end;

// TTracer

constructor TTracer.Create(const AServiceName: string);  
begin
  inherited Create;
  FServiceName := AServiceName;
  FSpans := TObjectList<TSpan>.Create(True);
  FActiveSpan := nil;

  WriteLn(Format('[Tracer:%s] Tracer créé', [AServiceName]));
end;

destructor TTracer.Destroy;  
begin
  FSpans.Free;
  inherited;
end;

function TTracer.StartSpan(const AOperationName: string;
  const ATraceId: string; const AParentSpanId: string): TSpan;
var
  TraceId, ParentId: string;
begin
  // Déterminer le TraceId
  if ATraceId <> '' then
    TraceId := ATraceId
  else if FActiveSpan <> nil then
    TraceId := FActiveSpan.TraceId
  else
    TraceId := '';

  // Déterminer le ParentSpanId
  if AParentSpanId <> '' then
    ParentId := AParentSpanId
  else if FActiveSpan <> nil then
    ParentId := FActiveSpan.SpanId
  else
    ParentId := '';

  Result := TSpan.Create(TraceId, FServiceName, AOperationName, ParentId);
  FSpans.Add(Result);
  FActiveSpan := Result;
end;

procedure TTracer.FinishSpan(ASpan: TSpan);  
begin
  if ASpan <> nil then
    ASpan.Finish;
end;

function TTracer.ExportTraces: string;  
var
  JSON: TJSONArray;
  Span: TSpan;
  Parser: TJSONParser;
begin
  JSON := TJSONArray.Create;
  try
    for Span in FSpans do
    begin
      Parser := TJSONParser.Create(Span.ToJSON, [joUTF8]);
      try
        JSON.Add(Parser.Parse);
      finally
        Parser.Free;
      end;
    end;

    Result := JSON.AsJSON;
  finally
    JSON.Free;
  end;
end;

procedure TTracer.PrintTraces;  
var
  Span: TSpan;
  Indent: string;
  Level: Integer;
begin
  WriteLn;
  WriteLn('═══════════════════════════════════════════');
  WriteLn(Format('  TRACES - %s', [FServiceName]));
  WriteLn('═══════════════════════════════════════════');

  Level := 0;
  for Span in FSpans do
  begin
    if Span.FParentSpanId = '' then
      Level := 0
    else
      Level := 1;

    Indent := StringOfChar(' ', Level * 2);

    WriteLn(Format('%s└─ %s.%s (%d ms)',
      [Indent, Span.ServiceName, Span.FOperationName, Span.GetDurationMs]));
    WriteLn(Format('%s   TraceID: %s', [Indent, Copy(Span.TraceId, 1, 16)]));
    WriteLn(Format('%s   SpanID:  %s', [Indent, Copy(Span.SpanId, 1, 16)]));
  end;

  WriteLn('═══════════════════════════════════════════');
end;

end.
```

## Exemple Complet avec Observabilité

Voici un exemple qui combine tous les aspects de l'observabilité :

```pascal
unit ServiceMesh.ObservableService;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpserver, httpdefs,
  ServiceMesh.Logging,
  ServiceMesh.Metrics,
  ServiceMesh.Tracing;

type
  // Service avec observabilité complète
  TObservableService = class
  private
    FServiceName: string;
    FPort: Integer;
    FServer: TFPHTTPServer;

    // Observabilité
    FLogger: TServiceLogger;
    FMetrics: TMetricsRegistry;
    FTracer: TTracer;

    procedure HandleRequest(Sender: TObject;
      var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse);

    procedure HandleMetricsEndpoint(var AResponse: TFPHTTPConnectionResponse);
  public
    constructor Create(const AServiceName: string; APort: Integer);
    destructor Destroy; override;

    procedure Start;
    procedure Stop;

    property Logger: TServiceLogger read FLogger;
    property Metrics: TMetricsRegistry read FMetrics;
    property Tracer: TTracer read FTracer;
  end;

implementation

uses
  DateUtils;

// TObservableService

constructor TObservableService.Create(const AServiceName: string; APort: Integer);  
begin
  inherited Create;
  FServiceName := AServiceName;
  FPort := APort;

  FServer := TFPHTTPServer.Create(nil);
  FServer.Port := FPort;
  FServer.OnRequest := @HandleRequest;

  // Initialiser l'observabilité
  FLogger := TServiceLogger.Create(AServiceName);
  FMetrics := TMetricsRegistry.Create;
  FTracer := TTracer.Create(AServiceName);

  // Créer les métriques de base
  FMetrics.Counter('http_requests_total');
  FMetrics.Counter('http_errors_total');
  FMetrics.Histogram('http_request_duration_ms');
  FMetrics.Gauge('service_up');

  FLogger.Info('Service créé', Format('{"port": %d}', [APort]));
end;

destructor TObservableService.Destroy;  
begin
  FLogger.Info('Service détruit');

  FTracer.Free;
  FMetrics.Free;
  FLogger.Free;
  FServer.Free;

  inherited;
end;

procedure TObservableService.HandleRequest(Sender: TObject;
  var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
var
  StartTime: TDateTime;
  DurationMs: Integer;
  Span: TSpan;
  TraceId: string;
begin
  StartTime := Now;

  // Extraire le TraceId du header (ou créer un nouveau)
  TraceId := ARequest.GetCustomHeader('X-Trace-ID');

  // Démarrer un span pour cette requête
  Span := FTracer.StartSpan('http_request', TraceId);
  try
    Span.AddTag('http.method', ARequest.Method);
    Span.AddTag('http.url', ARequest.URI);
    Span.AddTag('http.client', ARequest.RemoteAddress);

    FLogger.Info('Requête reçue',
      Format('{"method": "%s", "uri": "%s", "trace_id": "%s"}',
        [ARequest.Method, ARequest.URI, Span.TraceId]));

    // Endpoint spécial pour les métriques
    if ARequest.URI = '/metrics' then
    begin
      HandleMetricsEndpoint(AResponse);
    end
    else
    begin
      // Traitement normal
      AResponse.Code := 200;
      AResponse.Content := Format(
        '{"service": "%s", "message": "Hello from %s!", "trace_id": "%s"}',
        [FServiceName, FServiceName, Span.TraceId]);
      AResponse.ContentType := 'application/json';

      // Ajouter le TraceId au header de réponse
      AResponse.SetCustomHeader('X-Trace-ID', Span.TraceId);
    end;

    Span.AddTag('http.status_code', IntToStr(AResponse.Code));

    // Métriques
    FMetrics.Counter('http_requests_total').Inc;

    DurationMs := MilliSecondsBetween(Now, StartTime);
    FMetrics.Histogram('http_request_duration_ms').Observe(DurationMs);

    FLogger.Info('Requête traitée',
      Format('{"duration_ms": %d, "status": %d}', [DurationMs, AResponse.Code]));

  except
    on E: Exception do
    begin
      Span.AddTag('error', 'true');
      Span.Log('Exception: ' + E.Message);

      FMetrics.Counter('http_errors_total').Inc;
      FLogger.Error('Erreur traitement requête',
        Format('{"error": "%s"}', [E.Message]));

      AResponse.Code := 500;
      AResponse.Content := Format('{"error": "%s"}', [E.Message]);
    end;
  end;

  Span.Finish;
end;

procedure TObservableService.HandleMetricsEndpoint(
  var AResponse: TFPHTTPConnectionResponse);
var
  MetricsOutput: TStringList;
begin
  MetricsOutput := TStringList.Create;
  try
    FMetrics.ExportPrometheus(MetricsOutput);

    AResponse.Code := 200;
    AResponse.Content := MetricsOutput.Text;
    AResponse.ContentType := 'text/plain';
  finally
    MetricsOutput.Free;
  end;
end;

procedure TObservableService.Start;  
begin
  FServer.Active := True;
  FMetrics.Gauge('service_up').Set_(1);

  FLogger.Info('Service démarré',
    Format('{"port": %d}', [FPort]));

  WriteLn(Format('[%s] ✓ Démarré sur http://localhost:%d',
    [FServiceName, FPort]));
  WriteLn(Format('[%s] Métriques: http://localhost:%d/metrics',
    [FServiceName, FPort]));
end;

procedure TObservableService.Stop;  
begin
  if FServer.Active then
  begin
    FServer.Active := False;
    FMetrics.Gauge('service_up').Set_(0);

    FLogger.Info('Service arrêté');
    WriteLn(Format('[%s] Arrêté', [FServiceName]));
  end;
end;

end.
```

### Programme de démonstration

```pascal
program ObservabilityDemo;

{$mode objfpc}{$H+}

uses
  SysUtils,
  ServiceMesh.ObservableService;

var
  ServiceA, ServiceB: TObservableService;

begin
  WriteLn('═══════════════════════════════════════════');
  WriteLn('  DÉMONSTRATION OBSERVABILITÉ              ');
  WriteLn('═══════════════════════════════════════════');
  WriteLn;

  WriteLn('Création des services avec observabilité complète...');
  WriteLn;

  ServiceA := TObservableService.Create('service-a', 8001);
  ServiceB := TObservableService.Create('service-b', 8002);

  try
    ServiceA.Start;
    ServiceB.Start;

    WriteLn;
    WriteLn('═══════════════════════════════════════════');
    WriteLn('  SERVICES ACTIFS                         ');
    WriteLn('═══════════════════════════════════════════');
    WriteLn;
    WriteLn('Les services incluent:');
    WriteLn('  ✓ Logs structurés (JSON)');
    WriteLn('  ✓ Métriques Prometheus');
    WriteLn('  ✓ Tracing distribué');
    WriteLn;
    WriteLn('Testez avec:');
    WriteLn('  curl http://localhost:8001/');
    WriteLn('  curl http://localhost:8001/metrics');
    WriteLn('  curl http://localhost:8002/');
    WriteLn;
    WriteLn('Appuyez sur Entrée pour voir les traces...');
    ReadLn;

    // Afficher les traces
    ServiceA.Tracer.PrintTraces;
    ServiceB.Tracer.PrintTraces;

    WriteLn;
    WriteLn('Appuyez sur Entrée pour arrêter...');
    ReadLn;

  finally
    ServiceB.Free;
    ServiceA.Free;
  end;
end.
```

## Visualisation avec Grafana et Prometheus

### 1. Export Prometheus

Le format Prometheus est le standard pour les métriques :

```
# TYPE http_requests_total counter
http_requests_total 1247

# TYPE http_errors_total counter
http_errors_total 12

# TYPE http_request_duration_ms histogram
http_request_duration_ms_sum 125478.50  
http_request_duration_ms_count 1247  
http_request_duration_ms{quantile="0.5"} 85.00  
http_request_duration_ms{quantile="0.95"} 250.00  
http_request_duration_ms{quantile="0.99"} 450.00

# TYPE service_up gauge
service_up 1
```

### 2. Configuration Prometheus

```yaml
# prometheus.yml
global:
  scrape_interval: 15s

scrape_configs:
  - job_name: 'service-mesh'
    static_configs:
      - targets:
          - 'localhost:8001'
          - 'localhost:8002'
          - 'localhost:8003'
        labels:
          environment: 'production'
```

### 3. Dashboards Grafana

Les dashboards typiques incluent :

**Dashboard Service Overview :**
- Taux de requêtes par service
- Taux d'erreurs (%)
- Latence P50, P95, P99
- Nombre d'instances actives

**Dashboard Détails Service :**
- Timeline des requêtes
- Distribution des latences (histogramme)
- Top 10 endpoints les plus lents
- Erreurs par type

**Dashboard Traces :**
- Durée totale des traces
- Nombre de spans par trace
- Services impliqués
- Chemins critiques (critical path)

## Corrélation des Données

La vraie puissance de l'observabilité vient de la **corrélation** entre logs, métriques et traces.

### 1. Corrélation Logs ↔ Traces

```pascal
unit ServiceMesh.Correlation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  ServiceMesh.Logging,
  ServiceMesh.Tracing;

type
  // Logger avec corrélation de traces
  TCorrelatedLogger = class(TServiceLogger)
  private
    FCurrentTraceId: string;
  public
    procedure SetTraceContext(const ATraceId, ASpanId: string);
    procedure ClearTraceContext;

    procedure Info(const AMessage: string; const AContext: string = ''); override;
    procedure Error(const AMessage: string; const AContext: string = ''); override;
  end;

implementation

uses
  fpjson;

// TCorrelatedLogger

procedure TCorrelatedLogger.SetTraceContext(const ATraceId, ASpanId: string);  
begin
  FCurrentTraceId := ATraceId;
  WriteLn(Format('[CorrelatedLogger] Contexte trace: %s',
    [Copy(ATraceId, 1, 16)]));
end;

procedure TCorrelatedLogger.ClearTraceContext;  
begin
  FCurrentTraceId := '';
end;

procedure TCorrelatedLogger.Info(const AMessage: string; const AContext: string);  
var
  EnrichedContext: TJSONObject;
  Parser: TJSONParser;
  OriginalContext: TJSONObject;
begin
  // Enrichir le contexte avec le TraceId
  EnrichedContext := TJSONObject.Create;
  try
    if AContext <> '' then
    begin
      Parser := TJSONParser.Create(AContext, [joUTF8]);
      try
        OriginalContext := Parser.Parse as TJSONObject;
        try
          // Copier le contexte original
          EnrichedContext.Add('original', OriginalContext.Clone);
        finally
          OriginalContext.Free;
        end;
      finally
        Parser.Free;
      end;
    end;

    // Ajouter le TraceId
    if FCurrentTraceId <> '' then
      EnrichedContext.Add('trace_id', FCurrentTraceId);

    inherited Info(AMessage, EnrichedContext.AsJSON);

  finally
    EnrichedContext.Free;
  end;
end;

procedure TCorrelatedLogger.Error(const AMessage: string; const AContext: string);  
var
  EnrichedContext: TJSONObject;
  Parser: TJSONParser;
  OriginalContext: TJSONObject;
begin
  EnrichedContext := TJSONObject.Create;
  try
    if AContext <> '' then
    begin
      Parser := TJSONParser.Create(AContext, [joUTF8]);
      try
        OriginalContext := Parser.Parse as TJSONObject;
        try
          EnrichedContext.Add('original', OriginalContext.Clone);
        finally
          OriginalContext.Free;
        end;
      finally
        Parser.Free;
      end;
    end;

    if FCurrentTraceId <> '' then
      EnrichedContext.Add('trace_id', FCurrentTraceId);

    inherited Error(AMessage, EnrichedContext.AsJSON);

  finally
    EnrichedContext.Free;
  end;
end;

end.
```

### 2. Exemple d'utilisation corrélée

```pascal
program CorrelationDemo;

{$mode objfpc}{$H+}

uses
  SysUtils,
  ServiceMesh.Correlation,
  ServiceMesh.Tracing,
  ServiceMesh.Metrics;

procedure SimulateRequest;  
var
  Logger: TCorrelatedLogger;
  Tracer: TTracer;
  Metrics: TMetricsRegistry;
  Span: TSpan;
  StartTime: TDateTime;
  DurationMs: Integer;
begin
  Logger := TCorrelatedLogger.Create('api-service');
  Tracer := TTracer.Create('api-service');
  Metrics := TMetricsRegistry.Create;

  try
    // Démarrer une trace
    Span := Tracer.StartSpan('process_order');
    try
      // Configurer le contexte de corrélation
      Logger.SetTraceContext(Span.TraceId, Span.SpanId);
      StartTime := Now;

      // Simuler le traitement
      Logger.Info('Début du traitement de commande',
        '{"order_id": "ORD-12345"}');

      Span.AddTag('order_id', 'ORD-12345');
      Span.Log('Validation de la commande');

      Sleep(Random(200) + 50);

      // Simuler un appel à un autre service
      Logger.Info('Appel au service de paiement');
      Span.Log('Appel service paiement');

      Sleep(Random(150) + 50);

      // Simuler une erreur aléatoire
      if Random(10) = 0 then
      begin
        Logger.Error('Erreur lors du paiement',
          '{"error_code": "PAYMENT_FAILED"}');
        Span.AddTag('error', 'true');
        Metrics.Counter('order_errors_total').Inc;
        raise Exception.Create('Paiement échoué');
      end;

      Logger.Info('Commande traitée avec succès');

      // Enregistrer les métriques
      DurationMs := MilliSecondsBetween(Now, StartTime);
      Metrics.Histogram('order_duration_ms').Observe(DurationMs);
      Metrics.Counter('orders_total').Inc;

    finally
      Span.Finish;
      Logger.ClearTraceContext;
    end;

    WriteLn;
    WriteLn('✓ Requête traitée - Trace ID: ', Copy(Span.TraceId, 1, 16));
    WriteLn('  Durée: ', Span.GetDurationMs, ' ms');
    WriteLn('  Consultez les logs pour voir la corrélation complète');

  finally
    Metrics.Free;
    Tracer.Free;
    Logger.Free;
  end;
end;

var
  i: Integer;

begin
  WriteLn('═══════════════════════════════════════════');
  WriteLn('  DÉMONSTRATION CORRÉLATION                ');
  WriteLn('  Logs + Traces + Métriques                ');
  WriteLn('═══════════════════════════════════════════');
  WriteLn;

  Randomize;

  WriteLn('Simulation de 5 requêtes avec corrélation complète...');
  WriteLn;

  for i := 1 to 5 do
  begin
    WriteLn(Format('Requête #%d:', [i]));
    try
      SimulateRequest;
    except
      on E: Exception do
        WriteLn('✗ Erreur: ', E.Message);
    end;
    WriteLn;
    Sleep(500);
  end;

  WriteLn('═══════════════════════════════════════════');
  WriteLn('Tous les événements ont été corrélés !');
  WriteLn('Chaque log contient le trace_id correspondant.');
  WriteLn('═══════════════════════════════════════════');
  WriteLn;
  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
```

## Alerting (Alertes)

Les alertes notifient automatiquement l'équipe en cas de problème.

### 1. Système d'alertes

```pascal
unit ServiceMesh.Alerting;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections;

type
  // Sévérité de l'alerte
  TAlertSeverity = (asInfo, asWarning, asCritical);

  // État de l'alerte
  TAlertState = (asOK, asFiring, asResolved);

  // Alerte
  TAlert = class
  private
    FName: string;
    FSeverity: TAlertSeverity;
    FState: TAlertState;
    FMessage: string;
    FStartTime: TDateTime;
    FResolveTime: TDateTime;
  public
    constructor Create(const AName: string; ASeverity: TAlertSeverity;
      const AMessage: string);

    procedure Fire;
    procedure Resolve;

    property Name: string read FName;
    property Severity: TAlertSeverity read FSeverity;
    property State: TAlertState read FState;
    property Message: string read FMessage;
  end;

  // Règle d'alerte
  TAlertRule = class
  private
    FName: string;
    FCondition: string;
    FThreshold: Double;
    FSeverity: TAlertSeverity;
    FDurationSeconds: Integer;
    FLastCheckTime: TDateTime;
    FConditionMet: Boolean;
    FConditionMetSince: TDateTime;
  public
    constructor Create(const AName, ACondition: string;
      AThreshold: Double; ASeverity: TAlertSeverity;
      ADurationSeconds: Integer = 60);

    function Evaluate(AValue: Double): Boolean;

    property Name: string read FName;
  end;

  // Gestionnaire d'alertes
  TAlertManager = class
  private
    FRules: TObjectList<TAlertRule>;
    FActiveAlerts: TObjectList<TAlert>;
    FNotificationCallback: procedure(AAlert: TAlert) of object;

    procedure SendNotification(AAlert: TAlert);
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddRule(ARule: TAlertRule);
    procedure CheckMetric(const ARuleName: string; AValue: Double);

    procedure SetNotificationCallback(ACallback: procedure(AAlert: TAlert) of object);

    procedure PrintAlerts;

    property ActiveAlerts: TObjectList<TAlert> read FActiveAlerts;
  end;

implementation

uses
  DateUtils;

// TAlert

constructor TAlert.Create(const AName: string; ASeverity: TAlertSeverity;
  const AMessage: string);
begin
  inherited Create;
  FName := AName;
  FSeverity := ASeverity;
  FMessage := AMessage;
  FState := asOK;
end;

procedure TAlert.Fire;  
begin
  if FState <> asFiring then
  begin
    FState := asFiring;
    FStartTime := Now;
    WriteLn(Format('[Alert] 🔥 ALERTE: %s - %s', [FName, FMessage]));
  end;
end;

procedure TAlert.Resolve;  
begin
  if FState = asFiring then
  begin
    FState := asResolved;
    FResolveTime := Now;
    WriteLn(Format('[Alert] ✓ Résolue: %s', [FName]));
  end;
end;

// TAlertRule

constructor TAlertRule.Create(const AName, ACondition: string;
  AThreshold: Double; ASeverity: TAlertSeverity; ADurationSeconds: Integer);
begin
  inherited Create;
  FName := AName;
  FCondition := ACondition;
  FThreshold := AThreshold;
  FSeverity := ASeverity;
  FDurationSeconds := ADurationSeconds;
  FConditionMet := False;
end;

function TAlertRule.Evaluate(AValue: Double): Boolean;  
var
  ConditionNowMet: Boolean;
  DurationSeconds: Int64;
begin
  Result := False;

  // Évaluer la condition
  if FCondition = '>' then
    ConditionNowMet := AValue > FThreshold
  else if FCondition = '<' then
    ConditionNowMet := AValue < FThreshold
  else if FCondition = '>=' then
    ConditionNowMet := AValue >= FThreshold
  else if FCondition = '<=' then
    ConditionNowMet := AValue <= FThreshold
  else
    ConditionNowMet := False;

  // Gérer la durée
  if ConditionNowMet then
  begin
    if not FConditionMet then
    begin
      // Condition vient d'être remplie
      FConditionMet := True;
      FConditionMetSince := Now;
    end
    else
    begin
      // Vérifier si la durée est atteinte
      DurationSeconds := SecondsBetween(Now, FConditionMetSince);
      if DurationSeconds >= FDurationSeconds then
        Result := True;
    end;
  end
  else
  begin
    // Condition non remplie, réinitialiser
    FConditionMet := False;
  end;

  FLastCheckTime := Now;
end;

// TAlertManager

constructor TAlertManager.Create;  
begin
  inherited Create;
  FRules := TObjectList<TAlertRule>.Create(True);
  FActiveAlerts := TObjectList<TAlert>.Create(True);
  FNotificationCallback := nil;

  WriteLn('[AlertManager] Gestionnaire d''alertes créé');
end;

destructor TAlertManager.Destroy;  
begin
  FActiveAlerts.Free;
  FRules.Free;
  inherited;
end;

procedure TAlertManager.AddRule(ARule: TAlertRule);  
begin
  FRules.Add(ARule);
  WriteLn(Format('[AlertManager] Règle ajoutée: %s (%s %.2f)',
    [ARule.Name, ARule.FCondition, ARule.FThreshold]));
end;

procedure TAlertManager.CheckMetric(const ARuleName: string; AValue: Double);  
var
  Rule: TAlertRule;
  Alert: TAlert;
  Found: Boolean;
begin
  // Trouver la règle
  for Rule in FRules do
  begin
    if Rule.Name = ARuleName then
    begin
      // Évaluer la règle
      if Rule.Evaluate(AValue) then
      begin
        // Vérifier si l'alerte existe déjà
        Found := False;
        for Alert in FActiveAlerts do
        begin
          if Alert.Name = Rule.Name then
          begin
            Found := True;
            Break;
          end;
        end;

        // Créer une nouvelle alerte si nécessaire
        if not Found then
        begin
          Alert := TAlert.Create(Rule.Name, Rule.FSeverity,
            Format('Seuil dépassé: %.2f %s %.2f',
              [AValue, Rule.FCondition, Rule.FThreshold]));
          Alert.Fire;
          FActiveAlerts.Add(Alert);
          SendNotification(Alert);
        end;
      end
      else
      begin
        // Résoudre l'alerte si elle existe
        for Alert in FActiveAlerts do
        begin
          if (Alert.Name = Rule.Name) and (Alert.State = asFiring) then
          begin
            Alert.Resolve;
            SendNotification(Alert);
          end;
        end;
      end;

      Exit;
    end;
  end;
end;

procedure TAlertManager.SendNotification(AAlert: TAlert);  
begin
  if Assigned(FNotificationCallback) then
    FNotificationCallback(AAlert);
end;

procedure TAlertManager.SetNotificationCallback(
  ACallback: procedure(AAlert: TAlert) of object);
begin
  FNotificationCallback := ACallback;
end;

procedure TAlertManager.PrintAlerts;  
var
  Alert: TAlert;
  SeverityStr: string;
begin
  WriteLn;
  WriteLn('═══════════════════════════════════════════');
  WriteLn('  ALERTES ACTIVES                         ');
  WriteLn('═══════════════════════════════════════════');

  if FActiveAlerts.Count = 0 then
  begin
    WriteLn('Aucune alerte active');
  end
  else
  begin
    for Alert in FActiveAlerts do
    begin
      if Alert.State = asFiring then
      begin
        case Alert.Severity of
          asInfo: SeverityStr := 'INFO';
          asWarning: SeverityStr := 'WARNING';
          asCritical: SeverityStr := 'CRITICAL';
        end;

        WriteLn(Format('[%s] %s', [SeverityStr, Alert.Name]));
        WriteLn(Format('  Message: %s', [Alert.Message]));
        WriteLn(Format('  Depuis: %s', [DateTimeToStr(Alert.FStartTime)]));
        WriteLn;
      end;
    end;
  end;

  WriteLn('═══════════════════════════════════════════');
end;

end.
```

### 2. Exemple d'utilisation des alertes

```pascal
program AlertingDemo;

{$mode objfpc}{$H+}

uses
  SysUtils,
  ServiceMesh.Alerting;

procedure OnAlert(AAlert: TAlert);  
begin
  WriteLn;
  WriteLn('📧 NOTIFICATION ENVOYÉE:');
  WriteLn('  Alerte: ', AAlert.Name);
  WriteLn('  Message: ', AAlert.Message);
  WriteLn;
end;

var
  AlertManager: TAlertManager;
  HighErrorRateRule, HighLatencyRule: TAlertRule;
  i: Integer;
  ErrorRate, Latency: Double;

begin
  WriteLn('═══════════════════════════════════════════');
  WriteLn('  DÉMONSTRATION ALERTING                   ');
  WriteLn('═══════════════════════════════════════════');
  WriteLn;

  AlertManager := TAlertManager.Create;
  try
    // Configurer le callback de notification
    AlertManager.SetNotificationCallback(@OnAlert);

    // Définir les règles d'alerte
    HighErrorRateRule := TAlertRule.Create(
      'high_error_rate',
      '>',
      5.0,           // Seuil: 5%
      asCritical,
      10             // Pendant 10 secondes
    );

    HighLatencyRule := TAlertRule.Create(
      'high_latency',
      '>',
      500.0,         // Seuil: 500ms
      asWarning,
      10             // Pendant 10 secondes
    );

    AlertManager.AddRule(HighErrorRateRule);
    AlertManager.AddRule(HighLatencyRule);

    WriteLn;
    WriteLn('Simulation de métriques avec alertes...');
    WriteLn;

    Randomize;

    // Simuler 30 secondes de métriques
    for i := 1 to 30 do
    begin
      // Simuler des métriques
      ErrorRate := Random * 10;  // 0-10%
      Latency := Random * 800;   // 0-800ms

      WriteLn(Format('Seconde %d: ErrorRate=%.1f%%, Latency=%.0fms',
        [i, ErrorRate, Latency]));

      // Vérifier les règles
      AlertManager.CheckMetric('high_error_rate', ErrorRate);
      AlertManager.CheckMetric('high_latency', Latency);

      Sleep(1000);
    end;

    WriteLn;
    AlertManager.PrintAlerts;

  finally
    AlertManager.Free;
  end;

  WriteLn;
  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
```

## Service Mesh en Production

### Bonnes Pratiques

**✅ À faire :**

1. **Tracer toutes les requêtes**
```pascal
// ✅ BON: Trace ID sur chaque requête
Span := Tracer.StartSpan('handle_request', TraceId);  
try
  // Traitement
finally
  Span.Finish;
end;
```

2. **Logger avec structure**
```pascal
// ✅ BON: Logs JSON structurés
Logger.Info('Order processed',
  '{"order_id": "123", "amount": 99.99, "customer_id": "C456"}');
```

3. **Métriques pertinentes**
```pascal
// ✅ BON: Métriques métier
Metrics.Counter('orders_completed').Inc;  
Metrics.Histogram('order_value').Observe(Amount);
```

4. **Alertes intelligentes**
```pascal
// ✅ BON: Alertes avec durée
AlertRule.Create('high_error_rate', '>', 5.0, asCritical, 60);
// Alerte si > 5% pendant 60 secondes
```

5. **Corrélation complète**
```pascal
// ✅ BON: Trace ID partout
Logger.SetTraceContext(TraceId, SpanId);  
Request.SetHeader('X-Trace-ID', TraceId);
```

**❌ À éviter :**

1. **Logs non structurés**
```pascal
// ❌ MAUVAIS: Logs en texte libre
WriteLn('Order 123 processed for customer C456 amount 99.99');
// Difficile à parser et analyser
```

2. **Pas de context**
```pascal
// ❌ MAUVAIS: Log sans contexte
Logger.Error('Payment failed');
// Impossible de retrouver la requête concernée
```

3. **Trop de métriques**
```pascal
// ❌ MAUVAIS: Métrique par utilisateur
Metrics.Counter('user_' + UserId + '_requests').Inc;
// Explosion du nombre de métriques
```

4. **Alertes trop sensibles**
```pascal
// ❌ MAUVAIS: Alerte immédiate
AlertRule.Create('any_error', '>', 0, asCritical, 0);
// Trop de fausses alertes
```

### Architecture Complète en Production

```
┌─────────────────────────────────────────────────┐
│             OBSERVABILITY STACK                 │
├─────────────────────────────────────────────────┤
│                                                 │
│  ┌──────────────┐  ┌──────────────┐             │
│  │   Grafana    │  │  AlertManager│             │
│  │  (Dashboards)│  │  (Alertes)   │             │
│  └──────┬───────┘  └──────┬───────┘             │
│         │                 │                     │
│  ┌──────▼───────┐  ┌──────▼───────┐             │
│  │  Prometheus  │  │     Loki     │             │
│  │  (Métriques) │  │    (Logs)    │             │
│  └──────┬───────┘  └──────┬───────┘             │
│         │                 │                     │
│  ┌──────▼─────────────────▼─────────┐           │
│  │          Jaeger/Tempo            │           │
│  │          (Traces)                │           │
│  └──────┬───────────────────────────┘           │
│         │                                       │
└─────────┼───────────────────────────────────────┘
          │
┌─────────▼───────────────────────────────────────┐
│             SERVICE MESH                        │
├─────────────────────────────────────────────────┤
│                                                 │
│  ┌──────────────────────────────────────┐       │
│  │        Control Plane                 │       │
│  │  (Configuration, Service Discovery)  │       │
│  └──────────┬───────────────────────────┘       │
│             │                                   │
│    ┌────────┼────────┬────────┬────────┐        │
│    │        │        │        │        │        │
│  ┌─▼───┐  ┌─▼───┐  ┌─▼───┐  ┌─▼───┐  ┌─▼───┐    │
│  │Proxy│  │Proxy│  │Proxy│  │Proxy│  │Proxy│    │
│  └─┬───┘  └─┬───┘  └──┬──┘  └──┬──┘  └───┬─┘    │
│  ┌─▼──┐  ┌──▼──┐  ┌───▼─┐  ┌───▼─┐  ┌────▼┐     │
│  │Svc │  │Svc  │  │Svc  │  │Svc  │  │Svc  │     │
│  │ A  │  │ B   │  │ C   │  │ D   │  │ E   │     │
│  └────┘  └─────┘  └─────┘  └─────┘  └─────┘     │
│                                                 │
└─────────────────────────────────────────────────┘
```

### Métriques Clés à Surveiller

**Golden Signals (Signaux d'Or) :**

1. **Latency (Latence)** : Temps de réponse
   - P50, P95, P99
   - Par endpoint
   - Tendance

2. **Traffic (Trafic)** : Volume de requêtes
   - Requêtes/seconde
   - Par service
   - Pics et creux

3. **Errors (Erreurs)** : Taux d'erreur
   - Pourcentage d'erreurs
   - Par type d'erreur
   - Evolution

4. **Saturation** : Utilisation des ressources
   - CPU, Mémoire
   - Connexions
   - File d'attente

### RED Method (Method d'Observabilité)

Pour chaque service, suivre :

- **R**ate : Nombre de requêtes par seconde
- **E**rrors : Nombre d'erreurs par seconde
- **D**uration : Distribution du temps de réponse

```pascal
// Exemple d'implémentation RED
Metrics.Counter('http_requests_total').Inc;              // Rate  
if IsError then
  Metrics.Counter('http_errors_total').Inc;              // Errors
Metrics.Histogram('http_duration_seconds').Observe(Dur); // Duration
```

## Conclusion

Le Service Mesh et l'Observabilité sont essentiels pour gérer des architectures microservices en production. Voici les points clés à retenir :

### Service Mesh

**Rôles principaux :**
- Communication service-à-service sécurisée et fiable
- Load balancing et service discovery automatiques
- Résilience (retry, circuit breaker, timeout)
- Monitoring et observabilité intégrés

**Architecture :**
- **Control Plane** : Configuration centralisée
- **Data Plane** : Proxies (sidecars) par service
- Communication via proxies, pas directement

**Avantages :**
- Code métier déchargé des préoccupations techniques
- Configuration centralisée et cohérente
- Visibilité complète du trafic
- Sécurité renforcée (mTLS automatique)

### Observabilité

**Les trois piliers :**

1. **Logs** : Enregistrer les événements
   - Format structuré (JSON)
   - Niveaux appropriés
   - Contexte riche

2. **Metrics** : Mesurer les performances
   - Counter, Gauge, Histogram
   - Format Prometheus
   - Agrégation dans le temps

3. **Traces** : Suivre les requêtes
   - Trace ID unique
   - Spans imbriqués
   - Visualisation du chemin

**Corrélation :**
- Lier logs, métriques et traces
- Trace ID dans tous les logs
- Investigation rapide des problèmes

**Alerting :**
- Règles basées sur les métriques
- Seuils et durées appropriés
- Notifications automatiques
- Résolution suivie

### En Production

**Points critiques :**
- Surveiller les Golden Signals (Latency, Traffic, Errors, Saturation)
- Alertes intelligentes avec durée minimale
- Dashboards pour tous les niveaux (overview, détails, debug)
- Rétention appropriée des données (métriques, logs, traces)

**Stack typique :**
- **Prometheus** pour les métriques
- **Loki** ou **Elasticsearch** pour les logs
- **Jaeger** ou **Tempo** pour les traces
- **Grafana** pour la visualisation
- **AlertManager** pour les alertes

Un Service Mesh bien configuré avec une observabilité complète permet de gérer des centaines de microservices en production avec confiance !

---

⏭️ [Architecture cloud-native](/21-architecture-logicielle-avancee/11-architecture-cloud-native.md)
