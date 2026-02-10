🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 21.11 Architecture Cloud-Native

## Introduction

### Qu'est-ce que le Cloud-Native ?

Le **Cloud-Native** est une approche de développement conçue spécifiquement pour tirer parti du cloud computing. Ce n'est pas simplement déplacer une application vers le cloud, c'est la concevoir différemment dès le départ.

**Analogie :** C'est comme la différence entre :
- **Traditionnel** : Une maison avec des fondations en béton (difficile à déplacer)
- **Cloud-Native** : Une maison modulaire (peut être démontée, déplacée, agrandie facilement)

### Définition de la CNCF

La **Cloud Native Computing Foundation** définit le Cloud-Native ainsi :

```
Applications Cloud-Native :
├─ Conteneurisées (Docker, etc.)
├─ Orchestrées dynamiquement (Kubernetes)
├─ Orientées microservices
└─ Déclaratives et résilientes
```

### Les 12 Facteurs (12-Factor App)

Les applications Cloud-Native suivent 12 principes fondamentaux :

```
1.  Base de code unique (One codebase)
2.  Dépendances explicites
3.  Configuration dans l'environnement
4.  Services externes (backing services)
5.  Build, release, run séparés
6.  Processus sans état (stateless)
7.  Port binding (exposition via port)
8.  Concurrence via processus
9.  Démarrage rapide et arrêt gracieux
10. Parité dev/prod
11. Logs comme flux d'événements
12. Tâches admin comme processus
```

## Principe 1 : Configuration Externalisée

La configuration ne doit **jamais** être dans le code. Elle doit venir de l'environnement.

### 1. Mauvaise approche

```pascal
// ❌ MAUVAIS : Configuration en dur dans le code
const
  DATABASE_HOST = 'localhost';
  DATABASE_PORT = 5432;
  DATABASE_NAME = 'myapp';
  API_KEY = 'secret123';  // ← Dangereux !
```

### 2. Bonne approche : Variables d'environnement

```pascal
unit CloudNative.Config;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils;

type
  // Configuration de l'application
  TAppConfig = class
  private
    FDatabaseHost: string;
    FDatabasePort: Integer;
    FDatabaseName: string;
    FDatabaseUser: string;
    FDatabasePassword: string;
    FApiKey: string;
    FLogLevel: string;
    FPort: Integer;

    function GetEnv(const AName: string; const ADefault: string = ''): string;
    function GetEnvInt(const AName: string; ADefault: Integer): Integer;
  public
    constructor Create;

    procedure LoadFromEnvironment;
    procedure Validate;
    procedure Print;

    property DatabaseHost: string read FDatabaseHost;
    property DatabasePort: Integer read FDatabasePort;
    property DatabaseName: string read FDatabaseName;
    property DatabaseUser: string read FDatabaseUser;
    property DatabasePassword: string read FDatabasePassword;
    property ApiKey: string read FApiKey;
    property LogLevel: string read FLogLevel;
    property Port: Integer read FPort;
  end;

implementation

// TAppConfig

constructor TAppConfig.Create;  
begin
  inherited Create;
  LoadFromEnvironment;
end;

function TAppConfig.GetEnv(const AName: string; const ADefault: string): string;  
begin
  Result := GetEnvironmentVariable(AName);
  if Result = '' then
    Result := ADefault;
end;

function TAppConfig.GetEnvInt(const AName: string; ADefault: Integer): Integer;  
var
  Value: string;
begin
  Value := GetEnvironmentVariable(AName);
  if Value = '' then
    Result := ADefault
  else
    Result := StrToIntDef(Value, ADefault);
end;

procedure TAppConfig.LoadFromEnvironment;  
begin
  WriteLn('[Config] Chargement de la configuration depuis l''environnement...');

  // Base de données
  FDatabaseHost := GetEnv('DB_HOST', 'localhost');
  FDatabasePort := GetEnvInt('DB_PORT', 5432);
  FDatabaseName := GetEnv('DB_NAME', 'myapp');
  FDatabaseUser := GetEnv('DB_USER', 'postgres');
  FDatabasePassword := GetEnv('DB_PASSWORD', '');

  // API
  FApiKey := GetEnv('API_KEY', '');

  // Application
  FLogLevel := GetEnv('LOG_LEVEL', 'info');
  FPort := GetEnvInt('PORT', 8080);

  WriteLn('[Config] ✓ Configuration chargée');
end;

procedure TAppConfig.Validate;  
begin
  WriteLn('[Config] Validation de la configuration...');

  if FDatabasePassword = '' then
    WriteLn('[Config] ⚠️  WARNING: DB_PASSWORD non défini');

  if FApiKey = '' then
    raise Exception.Create('API_KEY requis');

  WriteLn('[Config] ✓ Configuration valide');
end;

procedure TAppConfig.Print;  
begin
  WriteLn;
  WriteLn('═══════════════════════════════════════════');
  WriteLn('  CONFIGURATION                           ');
  WriteLn('═══════════════════════════════════════════');
  WriteLn('Database:');
  WriteLn('  Host: ', FDatabaseHost);
  WriteLn('  Port: ', FDatabasePort);
  WriteLn('  Name: ', FDatabaseName);
  WriteLn('  User: ', FDatabaseUser);
  WriteLn('  Password: ', IfThen(FDatabasePassword <> '', '***', '(non défini)'));
  WriteLn;
  WriteLn('API:');
  WriteLn('  Key: ', IfThen(FApiKey <> '', '***', '(non défini)'));
  WriteLn;
  WriteLn('Application:');
  WriteLn('  Port: ', FPort);
  WriteLn('  Log Level: ', FLogLevel);
  WriteLn('═══════════════════════════════════════════');
end;

end.
```

### 3. Utilisation avec Docker

```dockerfile
# Dockerfile
FROM fpc:3.2.2

WORKDIR /app  
COPY . .

RUN fpc -O2 myapp.pas

# Variables d'environnement avec valeurs par défaut
ENV PORT=8080  
ENV LOG_LEVEL=info  
ENV DB_HOST=localhost  
ENV DB_PORT=5432

EXPOSE 8080

CMD ["./myapp"]
```

```yaml
# docker-compose.yml
version: '3.8'

services:
  app:
    build: .
    ports:
      - "8080:8080"
    environment:
      DB_HOST: postgres
      DB_PORT: 5432
      DB_NAME: myapp
      DB_USER: appuser
      DB_PASSWORD: ${DB_PASSWORD}  # Depuis .env
      API_KEY: ${API_KEY}
    depends_on:
      - postgres

  postgres:
    image: postgres:15
    environment:
      POSTGRES_DB: myapp
      POSTGRES_USER: appuser
      POSTGRES_PASSWORD: ${DB_PASSWORD}
    volumes:
      - postgres_data:/var/lib/postgresql/data

volumes:
  postgres_data:
```

```bash
# .env (ne JAMAIS commiter ce fichier)
DB_PASSWORD=SuperSecretPassword123!  
API_KEY=my-secret-api-key-xyz
```

## Principe 2 : Stateless (Sans État)

Les applications Cloud-Native doivent être **sans état** : aucune donnée utilisateur stockée localement.

### Pourquoi Stateless ?

```
┌─────────────────────────────────────┐
│  AVEC ÉTAT (Problématique)          │
├─────────────────────────────────────┤
│                                     │
│  User ──► Instance 1 (Session A)    │
│           │                         │
│           └─ Données en mémoire     │
│                                     │
│  User ──► Instance 2 (Session ?)    │
│           │                         │
│           └─ Pas de données !       │
│                                     │
│  ❌ Load balancing impossible       │
│  ❌ Scaling difficile               │
│  ❌ Perte de données si crash       │
└─────────────────────────────────────┘

┌─────────────────────────────────────┐
│  SANS ÉTAT (Cloud-Native)           │
├─────────────────────────────────────┤
│                                     │
│  User ──► Instance 1 ──► Redis      │
│           │                         │
│           └─ Pas de mémoire locale  │
│                                     │
│  User ──► Instance 2 ──► Redis      │
│           │              (Session)  │
│           └─ Lit depuis Redis       │
│                                     │
│  ✓ Load balancing facile            │
│  ✓ Scaling horizontal               │
│  ✓ Résilience                       │
└─────────────────────────────────────┘
```

### 1. Session externe avec Redis

```pascal
unit CloudNative.Session;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient, fpjson, jsonparser;

type
  // Gestionnaire de session avec Redis
  TSessionManager = class
  private
    FRedisHost: string;
    FRedisPort: Integer;
    FSessionTTL: Integer; // Time To Live en secondes

    function RedisSet(const AKey, AValue: string; ATTL: Integer): Boolean;
    function RedisGet(const AKey: string): string;
    function RedisDelete(const AKey: string): Boolean;
  public
    constructor Create(const ARedisHost: string; ARedisPort: Integer;
      ASessionTTL: Integer = 3600);

    function CreateSession(const AUserId: string): string;
    function GetSession(const ASessionId: string): TJSONObject;
    procedure UpdateSession(const ASessionId: string; AData: TJSONObject);
    procedure DeleteSession(const ASessionId: string);

    function IsValidSession(const ASessionId: string): Boolean;
  end;

implementation

uses
  base64;

// TSessionManager

constructor TSessionManager.Create(const ARedisHost: string;
  ARedisPort: Integer; ASessionTTL: Integer);
begin
  inherited Create;
  FRedisHost := ARedisHost;
  FRedisPort := ARedisPort;
  FSessionTTL := ASessionTTL;

  WriteLn(Format('[SessionManager] Connecté à Redis %s:%d',
    [FRedisHost, FRedisPort]));
end;

function TSessionManager.RedisSet(const AKey, AValue: string;
  ATTL: Integer): Boolean;
var
  Client: TFPHTTPClient;
  URL: string;
  Response: string;
begin
  Result := False;

  // Simplification : en production utiliser une vraie bibliothèque Redis
  Client := TFPHTTPClient.Create(nil);
  try
    try
      URL := Format('http://%s:%d/set?key=%s&value=%s&ttl=%d',
        [FRedisHost, FRedisPort, AKey,
         EncodeStringBase64(AValue), ATTL]);

      Response := Client.Get(URL);
      Result := Pos('OK', Response) > 0;

    except
      on E: Exception do
      begin
        WriteLn('[SessionManager] Erreur Redis SET: ', E.Message);
        Result := False;
      end;
    end;
  finally
    Client.Free;
  end;
end;

function TSessionManager.RedisGet(const AKey: string): string;  
var
  Client: TFPHTTPClient;
  URL: string;
begin
  Result := '';

  Client := TFPHTTPClient.Create(nil);
  try
    try
      URL := Format('http://%s:%d/get?key=%s',
        [FRedisHost, FRedisPort, AKey]);

      Result := Client.Get(URL);

      if Result <> '' then
        Result := DecodeStringBase64(Result);

    except
      on E: Exception do
      begin
        WriteLn('[SessionManager] Erreur Redis GET: ', E.Message);
        Result := '';
      end;
    end;
  finally
    Client.Free;
  end;
end;

function TSessionManager.RedisDelete(const AKey: string): Boolean;  
var
  Client: TFPHTTPClient;
  URL: string;
begin
  Result := False;

  Client := TFPHTTPClient.Create(nil);
  try
    try
      URL := Format('http://%s:%d/del?key=%s',
        [FRedisHost, FRedisPort, AKey]);

      Client.Get(URL);
      Result := True;

    except
      on E: Exception do
      begin
        WriteLn('[SessionManager] Erreur Redis DEL: ', E.Message);
        Result := False;
      end;
    end;
  finally
    Client.Free;
  end;
end;

function TSessionManager.CreateSession(const AUserId: string): string;  
var
  SessionData: TJSONObject;
  SessionId: string;
  G: TGUID;
begin
  // Générer un ID de session unique
  CreateGUID(G);
  SessionId := 'session:' + GUIDToString(G);

  // Créer les données de session
  SessionData := TJSONObject.Create;
  try
    SessionData.Add('user_id', AUserId);
    SessionData.Add('created_at', DateTimeToStr(Now));
    SessionData.Add('last_access', DateTimeToStr(Now));

    // Stocker dans Redis avec TTL
    if RedisSet(SessionId, SessionData.AsJSON, FSessionTTL) then
    begin
      WriteLn(Format('[SessionManager] ✓ Session créée: %s pour user %s',
        [Copy(SessionId, 1, 20) + '...', AUserId]));
      Result := SessionId;
    end
    else
      raise Exception.Create('Impossible de créer la session');

  finally
    SessionData.Free;
  end;
end;

function TSessionManager.GetSession(const ASessionId: string): TJSONObject;  
var
  SessionJSON: string;
  Parser: TJSONParser;
begin
  SessionJSON := RedisGet(ASessionId);

  if SessionJSON = '' then
  begin
    WriteLn('[SessionManager] Session non trouvée: ', ASessionId);
    Result := nil;
    Exit;
  end;

  Parser := TJSONParser.Create(SessionJSON, [joUTF8]);
  try
    Result := Parser.Parse as TJSONObject;
  finally
    Parser.Free;
  end;
end;

procedure TSessionManager.UpdateSession(const ASessionId: string;
  AData: TJSONObject);
begin
  // Mettre à jour last_access
  AData.Delete('last_access');
  AData.Add('last_access', DateTimeToStr(Now));

  // Sauvegarder avec renouvellement du TTL
  RedisSet(ASessionId, AData.AsJSON, FSessionTTL);

  WriteLn('[SessionManager] ✓ Session mise à jour: ', ASessionId);
end;

procedure TSessionManager.DeleteSession(const ASessionId: string);  
begin
  if RedisDelete(ASessionId) then
    WriteLn('[SessionManager] ✓ Session supprimée: ', ASessionId)
  else
    WriteLn('[SessionManager] ✗ Erreur suppression session: ', ASessionId);
end;

function TSessionManager.IsValidSession(const ASessionId: string): Boolean;  
var
  SessionData: TJSONObject;
begin
  SessionData := GetSession(ASessionId);
  Result := SessionData <> nil;

  if Result then
    SessionData.Free;
end;

end.
```

### 2. Application Stateless

```pascal
unit CloudNative.StatelessApp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpserver, httpdefs,
  CloudNative.Session;

type
  // Application web stateless
  TStatelessApp = class
  private
    FServer: TFPHTTPServer;
    FSessionManager: TSessionManager;
    FPort: Integer;

    procedure HandleRequest(Sender: TObject;
      var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse);

    procedure HandleLogin(ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse);
    procedure HandleProfile(ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse);
    procedure HandleLogout(ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse);

    function ExtractSessionId(ARequest: TFPHTTPConnectionRequest): string;
  public
    constructor Create(APort: Integer; ASessionManager: TSessionManager);
    destructor Destroy; override;

    procedure Start;
    procedure Stop;
  end;

implementation

uses
  fpjson;

// TStatelessApp

constructor TStatelessApp.Create(APort: Integer;
  ASessionManager: TSessionManager);
begin
  inherited Create;
  FPort := APort;
  FSessionManager := ASessionManager;

  FServer := TFPHTTPServer.Create(nil);
  FServer.Port := FPort;
  FServer.OnRequest := @HandleRequest;

  WriteLn('[StatelessApp] Application créée (port ', FPort, ')');
end;

destructor TStatelessApp.Destroy;  
begin
  Stop;
  FServer.Free;
  inherited;
end;

function TStatelessApp.ExtractSessionId(
  ARequest: TFPHTTPConnectionRequest): string;
var
  Cookie: string;
  StartPos: Integer;
begin
  Result := '';
  Cookie := ARequest.GetCustomHeader('Cookie');

  if Cookie <> '' then
  begin
    StartPos := Pos('session_id=', Cookie);
    if StartPos > 0 then
    begin
      Result := Copy(Cookie, StartPos + 11, 100);
      // Extraire jusqu'au prochain ;
      StartPos := Pos(';', Result);
      if StartPos > 0 then
        Result := Copy(Result, 1, StartPos - 1);
    end;
  end;
end;

procedure TStatelessApp.HandleRequest(Sender: TObject;
  var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
begin
  WriteLn;
  WriteLn(Format('[StatelessApp] %s %s', [ARequest.Method, ARequest.URI]));

  // Router
  if ARequest.URI = '/login' then
    HandleLogin(ARequest, AResponse)
  else if ARequest.URI = '/profile' then
    HandleProfile(ARequest, AResponse)
  else if ARequest.URI = '/logout' then
    HandleLogout(ARequest, AResponse)
  else
  begin
    AResponse.Code := 404;
    AResponse.Content := '{"error": "Not found"}';
  end;

  AResponse.ContentType := 'application/json';
end;

procedure TStatelessApp.HandleLogin(ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
var
  SessionId: string;
begin
  // Simuler l'authentification (en production: vérifier user/password)
  SessionId := FSessionManager.CreateSession('user123');

  // Définir le cookie de session
  AResponse.SetCustomHeader('Set-Cookie',
    Format('session_id=%s; HttpOnly; Path=/', [SessionId]));

  AResponse.Code := 200;
  AResponse.Content := Format('{"message": "Logged in", "session_id": "%s"}',
    [SessionId]);

  WriteLn('[StatelessApp] ✓ Utilisateur connecté');
end;

procedure TStatelessApp.HandleProfile(ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
var
  SessionId: string;
  SessionData: TJSONObject;
  UserId: string;
begin
  SessionId := ExtractSessionId(ARequest);

  if SessionId = '' then
  begin
    AResponse.Code := 401;
    AResponse.Content := '{"error": "Non authentifié"}';
    WriteLn('[StatelessApp] ✗ Session manquante');
    Exit;
  end;

  // Récupérer la session depuis Redis
  SessionData := FSessionManager.GetSession(SessionId);

  if SessionData = nil then
  begin
    AResponse.Code := 401;
    AResponse.Content := '{"error": "Session invalide ou expirée"}';
    WriteLn('[StatelessApp] ✗ Session invalide');
    Exit;
  end;

  try
    UserId := SessionData.Get('user_id', '');

    // Mettre à jour la session
    FSessionManager.UpdateSession(SessionId, SessionData);

    AResponse.Code := 200;
    AResponse.Content := Format(
      '{"user_id": "%s", "message": "Profile data"}', [UserId]);

    WriteLn('[StatelessApp] ✓ Profil consulté pour user ', UserId);

  finally
    SessionData.Free;
  end;
end;

procedure TStatelessApp.HandleLogout(ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
var
  SessionId: string;
begin
  SessionId := ExtractSessionId(ARequest);

  if SessionId <> '' then
  begin
    FSessionManager.DeleteSession(SessionId);
    WriteLn('[StatelessApp] ✓ Utilisateur déconnecté');
  end;

  // Supprimer le cookie
  AResponse.SetCustomHeader('Set-Cookie',
    'session_id=; Max-Age=0; Path=/');

  AResponse.Code := 200;
  AResponse.Content := '{"message": "Logged out"}';
end;

procedure TStatelessApp.Start;  
begin
  FServer.Active := True;
  WriteLn(Format('[StatelessApp] ✓ Démarrée sur http://localhost:%d', [FPort]));
end;

procedure TStatelessApp.Stop;  
begin
  if FServer.Active then
  begin
    FServer.Active := False;
    WriteLn('[StatelessApp] Arrêtée');
  end;
end;

end.
```

## Principe 3 : Health Checks

Les applications Cloud-Native doivent exposer des endpoints de santé pour l'orchestrateur.

### Types de Health Checks

```
┌────────────────────────────────────┐
│  LIVENESS PROBE                    │
│  "Est-ce que je suis vivant ?"     │
│                                    │
│  ✓ Répond → OK                     │
│  ✗ Ne répond pas → REDÉMARRER      │
└────────────────────────────────────┘

┌────────────────────────────────────┐
│  READINESS PROBE                   │
│  "Suis-je prêt à recevoir du       │
│   trafic ?"                        │
│                                    │
│  ✓ Prêt → Envoyer du trafic        │
│  ✗ Pas prêt → Attendre             │
└────────────────────────────────────┘

┌────────────────────────────────────┐
│  STARTUP PROBE                     │
│  "Ai-je fini de démarrer ?"        │
│                                    │
│  ✓ Démarré → Activer autres checks │
│  ✗ Pas démarré → Patienter         │
└────────────────────────────────────┘
```

### 1. Implémentation des Health Checks

```pascal
unit CloudNative.Health;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, fpjson;

type
  // État de santé
  THealthStatus = (hsUp, hsDown, hsDegraded);

  // Vérification de santé
  THealthCheck = class
  private
    FName: string;
    FStatus: THealthStatus;
    FMessage: string;
    FLastCheck: TDateTime;
  public
    constructor Create(const AName: string);

    function Check: THealthStatus; virtual; abstract;
    function ToJSON: TJSONObject;

    property Name: string read FName;
    property Status: THealthStatus read FStatus write FStatus;
    property Message: string read FMessage write FMessage;
  end;

  // Health Check : Base de données
  TDatabaseHealthCheck = class(THealthCheck)
  private
    FConnectionString: string;
  public
    constructor Create(const AConnectionString: string);

    function Check: THealthStatus; override;
  end;

  // Health Check : Service externe
  TExternalServiceHealthCheck = class(THealthCheck)
  private
    FURL: string;
    FTimeout: Integer;
  public
    constructor Create(const AName, AURL: string; ATimeout: Integer = 5000);

    function Check: THealthStatus; override;
  end;

  // Gestionnaire de santé
  THealthManager = class
  private
    FChecks: TObjectList<THealthCheck>;
    FIsReady: Boolean;
    FStartTime: TDateTime;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddCheck(ACheck: THealthCheck);

    function CheckLiveness: THealthStatus;
    function CheckReadiness: THealthStatus;
    function CheckStartup: THealthStatus;

    function GetHealthReport: TJSONObject;

    procedure SetReady(AReady: Boolean);
  end;

implementation

uses
  DateUtils, fphttpclient;

// THealthCheck

constructor THealthCheck.Create(const AName: string);  
begin
  inherited Create;
  FName := AName;
  FStatus := hsDown;
  FMessage := '';
end;

function THealthCheck.ToJSON: TJSONObject;  
var
  StatusStr: string;
begin
  case FStatus of
    hsUp: StatusStr := 'UP';
    hsDown: StatusStr := 'DOWN';
    hsDegraded: StatusStr := 'DEGRADED';
  end;

  Result := TJSONObject.Create;
  Result.Add('name', FName);
  Result.Add('status', StatusStr);
  Result.Add('message', FMessage);
  Result.Add('last_check', DateTimeToStr(FLastCheck));
end;

// TDatabaseHealthCheck

constructor TDatabaseHealthCheck.Create(const AConnectionString: string);  
begin
  inherited Create('database');
  FConnectionString := AConnectionString;
end;

function TDatabaseHealthCheck.Check: THealthStatus;  
begin
  FLastCheck := Now;

  try
    // Simuler une connexion DB
    // En production: vraie connexion et requête simple
    WriteLn('[HealthCheck:DB] Vérification connexion...');

    Sleep(Random(100));

    if Random(100) < 95 then  // 95% de succès
    begin
      FStatus := hsUp;
      FMessage := 'Database connected';
      WriteLn('[HealthCheck:DB] ✓ UP');
    end
    else
    begin
      FStatus := hsDegraded;
      FMessage := 'Database slow';
      WriteLn('[HealthCheck:DB] ⚠️  DEGRADED');
    end;

  except
    on E: Exception do
    begin
      FStatus := hsDown;
      FMessage := E.Message;
      WriteLn('[HealthCheck:DB] ✗ DOWN: ', E.Message);
    end;
  end;

  Result := FStatus;
end;

// TExternalServiceHealthCheck

constructor TExternalServiceHealthCheck.Create(const AName, AURL: string;
  ATimeout: Integer);
begin
  inherited Create(AName);
  FURL := AURL;
  FTimeout := ATimeout;
end;

function TExternalServiceHealthCheck.Check: THealthStatus;  
var
  Client: TFPHTTPClient;
begin
  FLastCheck := Now;

  Client := TFPHTTPClient.Create(nil);
  try
    try
      Client.ConnectTimeout := FTimeout;
      Client.IOTimeout := FTimeout;

      WriteLn(Format('[HealthCheck:%s] Vérification %s...', [FName, FURL]));

      Client.Get(FURL);

      FStatus := hsUp;
      FMessage := Format('Service responding (HTTP %d)',
        [Client.ResponseStatusCode]);
      WriteLn(Format('[HealthCheck:%s] ✓ UP', [FName]));

    except
      on E: Exception do
      begin
        FStatus := hsDown;
        FMessage := E.Message;
        WriteLn(Format('[HealthCheck:%s] ✗ DOWN: %s', [FName, E.Message]));
      end;
    end;

  finally
    Client.Free;
  end;

  Result := FStatus;
end;

// THealthManager

constructor THealthManager.Create;  
begin
  inherited Create;
  FChecks := TObjectList<THealthCheck>.Create(True);
  FIsReady := False;
  FStartTime := Now;

  WriteLn('[HealthManager] Gestionnaire créé');
end;

destructor THealthManager.Destroy;  
begin
  FChecks.Free;
  inherited;
end;

procedure THealthManager.AddCheck(ACheck: THealthCheck);  
begin
  FChecks.Add(ACheck);
  WriteLn(Format('[HealthManager] Check ajouté: %s', [ACheck.Name]));
end;

function THealthManager.CheckLiveness: THealthStatus;  
begin
  // Liveness: juste vérifier que l'app répond
  Result := hsUp;
  WriteLn('[HealthManager] Liveness: UP');
end;

function THealthManager.CheckReadiness: THealthStatus;  
var
  Check: THealthCheck;
  AllUp: Boolean;
begin
  if not FIsReady then
  begin
    WriteLn('[HealthManager] Readiness: NOT READY (manuel)');
    Result := hsDown;
    Exit;
  end;

  AllUp := True;

  // Vérifier tous les checks
  for Check in FChecks do
  begin
    if Check.Check <> hsUp then
      AllUp := False;
  end;

  if AllUp then
  begin
    Result := hsUp;
    WriteLn('[HealthManager] Readiness: READY');
  end
  else
  begin
    Result := hsDegraded;
    WriteLn('[HealthManager] Readiness: DEGRADED');
  end;
end;

function THealthManager.CheckStartup: THealthStatus;  
var
  ElapsedSeconds: Int64;
begin
  // Startup: vérifier que l'app a fini de démarrer
  ElapsedSeconds := SecondsBetween(Now, FStartTime);

  if ElapsedSeconds < 5 then
  begin
    Result := hsDown;
    WriteLn('[HealthManager] Startup: En cours... (', ElapsedSeconds, 's)');
  end
  else
  begin
    Result := hsUp;
    WriteLn('[HealthManager] Startup: COMPLÉTÉ');
  end;
end;

function THealthManager.GetHealthReport: TJSONObject;  
var
  Check: THealthCheck;
  ChecksArray: TJSONArray;
  StatusStr: string;
  OverallStatus: THealthStatus;
begin
  Result := TJSONObject.Create;

  // Déterminer le statut global
  OverallStatus := CheckReadiness;

  case OverallStatus of
    hsUp: StatusStr := 'UP';
    hsDown: StatusStr := 'DOWN';
    hsDegraded: StatusStr := 'DEGRADED';
  end;

  Result.Add('status', StatusStr);
  Result.Add('uptime_seconds', SecondsBetween(Now, FStartTime));

  // Détails des checks
  ChecksArray := TJSONArray.Create;
  for Check in FChecks do
  begin
    Check.Check;
    ChecksArray.Add(Check.ToJSON);
  end;

  Result.Add('checks', ChecksArray);
end;

procedure THealthManager.SetReady(AReady: Boolean);  
begin
  FIsReady := AReady;
  WriteLn(Format('[HealthManager] Ready status: %s',
    [BoolToStr(AReady, True)]));
end;

end.
```

### 2. Application avec Health Checks

```pascal
unit CloudNative.HealthyApp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpserver, httpdefs,
  CloudNative.Health;

type
  // Application avec health checks
  THealthyApp = class
  private
    FServer: TFPHTTPServer;
    FHealthManager: THealthManager;
    FPort: Integer;

    procedure HandleRequest(Sender: TObject;
      var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse);

    procedure HandleLiveness(var AResponse: TFPHTTPConnectionResponse);
    procedure HandleReadiness(var AResponse: TFPHTTPConnectionResponse);
    procedure HandleHealth(var AResponse: TFPHTTPConnectionResponse);
  public
    constructor Create(APort: Integer);
    destructor Destroy; override;

    procedure Start;
    procedure Stop;

    property HealthManager: THealthManager read FHealthManager;
  end;

implementation

// THealthyApp

constructor THealthyApp.Create(APort: Integer);  
begin
  inherited Create;
  FPort := APort;

  FServer := TFPHTTPServer.Create(nil);
  FServer.Port := FPort;
  FServer.OnRequest := @HandleRequest;

  FHealthManager := THealthManager.Create;

  WriteLn('[HealthyApp] Application créée');
end;

destructor THealthyApp.Destroy;  
begin
  Stop;
  FHealthManager.Free;
  FServer.Free;
  inherited;
end;

procedure THealthyApp.HandleRequest(Sender: TObject;
  var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
begin
  WriteLn(Format('[HealthyApp] %s %s', [ARequest.Method, ARequest.URI]));

  if ARequest.URI = '/health/live' then
    HandleLiveness(AResponse)
  else if ARequest.URI = '/health/ready' then
    HandleReadiness(AResponse)
  else if ARequest.URI = '/health' then
    HandleHealth(AResponse)
  else
  begin
    AResponse.Code := 200;
    AResponse.Content := '{"message": "Hello from Cloud-Native App!"}';
    AResponse.ContentType := 'application/json';
  end;
end;

procedure THealthyApp.HandleLiveness(var AResponse: TFPHTTPConnectionResponse);  
var
  Status: THealthStatus;
begin
  Status := FHealthManager.CheckLiveness;

  if Status = hsUp then
  begin
    AResponse.Code := 200;
    AResponse.Content := '{"status": "UP"}';
  end
  else
  begin
    AResponse.Code := 503;
    AResponse.Content := '{"status": "DOWN"}';
  end;

  AResponse.ContentType := 'application/json';
end;

procedure THealthyApp.HandleReadiness(var AResponse: TFPHTTPConnectionResponse);  
var
  Status: THealthStatus;
begin
  Status := FHealthManager.CheckReadiness;

  if Status = hsUp then
  begin
    AResponse.Code := 200;
    AResponse.Content := '{"status": "READY"}';
  end
  else
  begin
    AResponse.Code := 503;
    AResponse.Content := '{"status": "NOT_READY"}';
  end;

  AResponse.ContentType := 'application/json';
end;

procedure THealthyApp.HandleHealth(var AResponse: TFPHTTPConnectionResponse);  
var
  Report: TJSONObject;
begin
  Report := FHealthManager.GetHealthReport;
  try
    AResponse.Code := 200;
    AResponse.Content := Report.AsJSON;
    AResponse.ContentType := 'application/json';
  finally
    Report.Free;
  end;
end;

procedure THealthyApp.Start;  
begin
  FServer.Active := True;
  WriteLn(Format('[HealthyApp] ✓ Démarrée sur http://localhost:%d', [FPort]));
  WriteLn('[HealthyApp] Endpoints:');
  WriteLn('  /health/live  - Liveness probe');
  WriteLn('  /health/ready - Readiness probe');
  WriteLn('  /health       - Rapport complet');
end;

procedure THealthyApp.Stop;  
begin
  if FServer.Active then
  begin
    FServer.Active := False;
    WriteLn('[HealthyApp] Arrêtée');
  end;
end;

end.
```

### 3. Configuration Kubernetes

```yaml
# deployment.yaml
apiVersion: apps/v1  
kind: Deployment  
metadata:
  name: myapp
spec:
  replicas: 3
  selector:
    matchLabels:
      app: myapp
  template:
    metadata:
      labels:
        app: myapp
    spec:
      containers:
      - name: myapp
        image: myapp:1.0
        ports:
        - containerPort: 8080

        # Variables d'environnement
        env:
        - name: DB_HOST
          value: "postgres-service"
        - name: DB_PORT
          value: "5432"
        - name: DB_NAME
          valueFrom:
            secretKeyRef:
              name: db-secret
              key: database
        - name: DB_PASSWORD
          valueFrom:
            secretKeyRef:
              name: db-secret
              key: password

        # Liveness Probe
        livenessProbe:
          httpGet:
            path: /health/live
            port: 8080
          initialDelaySeconds: 10
          periodSeconds: 10
          timeoutSeconds: 5
          failureThreshold: 3

        # Readiness Probe
        readinessProbe:
          httpGet:
            path: /health/ready
            port: 8080
          initialDelaySeconds: 5
          periodSeconds: 5
          timeoutSeconds: 3
          failureThreshold: 2

        # Startup Probe
        startupProbe:
          httpGet:
            path: /health/live
            port: 8080
          initialDelaySeconds: 0
          periodSeconds: 2
          failureThreshold: 30

        # Ressources
        resources:
          requests:
            memory: "64Mi"
            cpu: "100m"
          limits:
            memory: "128Mi"
            cpu: "500m"
```

## Principe 4 : Graceful Shutdown

Une application Cloud-Native doit s'arrêter proprement.

### Problème sans Graceful Shutdown

```
Signal SIGTERM ──► Application
                   │
                   └─ Arrêt immédiat ✗
                      ├─ Requêtes en cours perdues
                      ├─ Connexions DB brutalement fermées
                      └─ Données potentiellement corrompues
```

### Solution : Graceful Shutdown

```
Signal SIGTERM ──► Application
                   │
                   ├─ 1. Stop d'accepter nouvelles requêtes
                   ├─ 2. Terminer requêtes en cours
                   ├─ 3. Fermer connexions proprement
                   ├─ 4. Flush des logs/métriques
                   └─ 5. Sortie propre ✓
```

### Implémentation

```pascal
unit CloudNative.GracefulShutdown;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BaseUnix;

type
  // Callback de shutdown
  TShutdownCallback = procedure of object;

  // Gestionnaire de shutdown
  TShutdownManager = class
  private
    FCallbacks: array of TShutdownCallback;
    FShuttingDown: Boolean;
    FGracePeriodSeconds: Integer;

    procedure HandleSignal(Signal: LongInt);
  public
    constructor Create(AGracePeriodSeconds: Integer = 30);
    destructor Destroy; override;

    procedure RegisterCallback(ACallback: TShutdownCallback);
    procedure Start;

    function IsShuttingDown: Boolean;
  end;

var
  GlobalShutdownManager: TShutdownManager;

implementation

uses
  DateUtils;

// Handler de signal global
procedure SignalHandler(Signal: cInt); cdecl;  
begin
  if Assigned(GlobalShutdownManager) then
    GlobalShutdownManager.HandleSignal(Signal);
end;

// TShutdownManager

constructor TShutdownManager.Create(AGracePeriodSeconds: Integer);  
begin
  inherited Create;
  FShuttingDown := False;
  FGracePeriodSeconds := AGracePeriodSeconds;
  SetLength(FCallbacks, 0);

  WriteLn('[ShutdownManager] Créé (grace period: ', AGracePeriodSeconds, 's)');
end;

destructor TShutdownManager.Destroy;  
begin
  inherited;
end;

procedure TShutdownManager.RegisterCallback(ACallback: TShutdownCallback);  
var
  Len: Integer;
begin
  Len := Length(FCallbacks);
  SetLength(FCallbacks, Len + 1);
  FCallbacks[Len] := ACallback;

  WriteLn('[ShutdownManager] Callback enregistré');
end;

procedure TShutdownManager.HandleSignal(Signal: LongInt);  
var
  Callback: TShutdownCallback;
  StartTime: TDateTime;
  ElapsedSeconds: Int64;
begin
  WriteLn;
  WriteLn('═══════════════════════════════════════════');
  WriteLn('  GRACEFUL SHUTDOWN INITIÉ                ');
  WriteLn('═══════════════════════════════════════════');
  WriteLn(Format('Signal reçu: %d', [Signal]));
  WriteLn;

  FShuttingDown := True;
  StartTime := Now;

  // Exécuter tous les callbacks
  for Callback in FCallbacks do
  begin
    try
      Callback;
    except
      on E: Exception do
        WriteLn('[ShutdownManager] Erreur callback: ', E.Message);
    end;
  end;

  ElapsedSeconds := SecondsBetween(Now, StartTime);

  WriteLn;
  WriteLn('═══════════════════════════════════════════');
  WriteLn(Format('  SHUTDOWN COMPLÉTÉ (%ds)                ', [ElapsedSeconds]));
  WriteLn('═══════════════════════════════════════════');

  Halt(0);
end;

procedure TShutdownManager.Start;  
begin
  {$IFDEF UNIX}
  // Enregistrer les handlers de signaux
  FpSignal(SIGTERM, @SignalHandler);
  FpSignal(SIGINT, @SignalHandler);
  WriteLn('[ShutdownManager] ✓ Handlers de signaux installés');
  {$ENDIF}
end;

function TShutdownManager.IsShuttingDown: Boolean;  
begin
  Result := FShuttingDown;
end;

initialization
  GlobalShutdownManager := nil;

end.
```

### Application avec Graceful Shutdown

```pascal
unit CloudNative.GracefulApp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpserver, httpdefs, Generics.Collections,
  CloudNative.GracefulShutdown;

type
  // Requête en cours
  TActiveRequest = record
    RequestId: string;
    StartTime: TDateTime;
    Path: string;
  end;

  // Application avec arrêt gracieux
  TGracefulApp = class
  private
    FServer: TFPHTTPServer;
    FPort: Integer;
    FShutdownManager: TShutdownManager;
    FActiveRequests: TList<TActiveRequest>;
    FAcceptingRequests: Boolean;

    procedure HandleRequest(Sender: TObject;
      var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse);

    procedure OnShutdown;
    procedure WaitForActiveRequests;
  public
    constructor Create(APort: Integer);
    destructor Destroy; override;

    procedure Start;
    procedure Stop;
  end;

implementation

uses
  DateUtils;

// TGracefulApp

constructor TGracefulApp.Create(APort: Integer);  
begin
  inherited Create;
  FPort := APort;
  FAcceptingRequests := True;

  FServer := TFPHTTPServer.Create(nil);
  FServer.Port := FPort;
  FServer.OnRequest := @HandleRequest;

  FActiveRequests := TList<TActiveRequest>.Create;

  FShutdownManager := TShutdownManager.Create(30);
  FShutdownManager.RegisterCallback(@OnShutdown);
  GlobalShutdownManager := FShutdownManager;

  WriteLn('[GracefulApp] Application créée');
end;

destructor TGracefulApp.Destroy;  
begin
  Stop;
  FActiveRequests.Free;
  FShutdownManager.Free;
  FServer.Free;
  inherited;
end;

procedure TGracefulApp.HandleRequest(Sender: TObject;
  var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
var
  ActiveReq: TActiveRequest;
  ProcessingTime: Integer;
  G: TGUID;
begin
  // Refuser nouvelles requêtes si en shutdown
  if not FAcceptingRequests then
  begin
    AResponse.Code := 503;
    AResponse.Content := '{"error": "Service shutting down"}';
    AResponse.ContentType := 'application/json';
    WriteLn('[GracefulApp] ✗ Requête refusée (shutdown en cours)');
    Exit;
  end;

  // Enregistrer la requête active
  CreateGUID(G);
  ActiveReq.RequestId := GUIDToString(G);
  ActiveReq.StartTime := Now;
  ActiveReq.Path := ARequest.URI;
  FActiveRequests.Add(ActiveReq);

  WriteLn(Format('[GracefulApp] → Requête %s: %s (actives: %d)',
    [Copy(ActiveReq.RequestId, 1, 8), ActiveReq.Path, FActiveRequests.Count]));

  try
    // Simuler du traitement
    ProcessingTime := Random(3000);
    Sleep(ProcessingTime);

    AResponse.Code := 200;
    AResponse.Content := Format(
      '{"message": "Processed", "duration_ms": %d, "request_id": "%s"}',
      [ProcessingTime, ActiveReq.RequestId]);
    AResponse.ContentType := 'application/json';

    WriteLn(Format('[GracefulApp] ✓ Requête %s complétée (%dms)',
      [Copy(ActiveReq.RequestId, 1, 8), ProcessingTime]));

  finally
    // Retirer de la liste des requêtes actives
    FActiveRequests.Remove(ActiveReq);
  end;
end;

procedure TGracefulApp.WaitForActiveRequests;  
var
  WaitCount: Integer;
  Req: TActiveRequest;
begin
  WaitCount := 0;

  while (FActiveRequests.Count > 0) and (WaitCount < 30) do
  begin
    WriteLn(Format('[GracefulApp] Attente de %d requête(s) en cours...',
      [FActiveRequests.Count]));

    for Req in FActiveRequests do
      WriteLn(Format('  - %s: %s (depuis %ds)',
        [Copy(Req.RequestId, 1, 8), Req.Path,
         SecondsBetween(Now, Req.StartTime)]));

    Sleep(1000);
    Inc(WaitCount);
  end;

  if FActiveRequests.Count > 0 then
    WriteLn(Format('[GracefulApp] ⚠️  %d requête(s) forcées à s''arrêter',
      [FActiveRequests.Count]))
  else
    WriteLn('[GracefulApp] ✓ Toutes les requêtes terminées');
end;

procedure TGracefulApp.OnShutdown;  
begin
  WriteLn('[GracefulApp] Début du shutdown gracieux...');
  WriteLn;

  // Étape 1: Arrêter d'accepter de nouvelles requêtes
  WriteLn('[GracefulApp] 1. Arrêt acceptation nouvelles requêtes');
  FAcceptingRequests := False;

  // Étape 2: Attendre que les requêtes en cours se terminent
  WriteLn('[GracefulApp] 2. Attente fin des requêtes en cours');
  WaitForActiveRequests;

  // Étape 3: Arrêter le serveur
  WriteLn('[GracefulApp] 3. Arrêt du serveur HTTP');
  Stop;

  // Étape 4: Fermer les connexions (DB, etc.)
  WriteLn('[GracefulApp] 4. Fermeture des connexions');
  // En production: fermer DB, Redis, etc.
  Sleep(500);

  // Étape 5: Flush des logs/métriques
  WriteLn('[GracefulApp] 5. Flush logs et métriques');
  Sleep(200);

  WriteLn('[GracefulApp] ✓ Shutdown gracieux complété');
end;

procedure TGracefulApp.Start;  
begin
  FServer.Active := True;
  FShutdownManager.Start;

  WriteLn(Format('[GracefulApp] ✓ Démarrée sur http://localhost:%d', [FPort]));
  WriteLn('[GracefulApp] Utilisez Ctrl+C pour tester le graceful shutdown');
end;

procedure TGracefulApp.Stop;  
begin
  if FServer.Active then
  begin
    FServer.Active := False;
    WriteLn('[GracefulApp] Serveur arrêté');
  end;
end;

end.
```

## Principe 5 : Observabilité Intégrée

Les applications Cloud-Native doivent exposer des métriques, logs et traces.

### Structured Logging

```pascal
unit CloudNative.StructuredLogging;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson;

type
  // Logger structuré pour Cloud-Native
  TStructuredLogger = class
  private
    FServiceName: string;
    FEnvironment: string;
    FVersion: string;

    function CreateBaseObject: TJSONObject;
  public
    constructor Create(const AServiceName, AEnvironment,
      AVersion: string);

    procedure LogInfo(const AMessage: string; AData: TJSONObject = nil);
    procedure LogError(const AMessage: string; AError: Exception;
      AData: TJSONObject = nil);
    procedure LogRequest(const AMethod, APath: string; AStatusCode,
      ADurationMs: Integer);
  end;

implementation

// TStructuredLogger

constructor TStructuredLogger.Create(const AServiceName, AEnvironment,
  AVersion: string);
begin
  inherited Create;
  FServiceName := AServiceName;
  FEnvironment := AEnvironment;
  FVersion := AVersion;
end;

function TStructuredLogger.CreateBaseObject: TJSONObject;  
begin
  Result := TJSONObject.Create;
  Result.Add('timestamp', FormatDateTime('yyyy-mm-dd"T"hh:nn:ss.zzz"Z"', Now));
  Result.Add('service', FServiceName);
  Result.Add('environment', FEnvironment);
  Result.Add('version', FVersion);
end;

procedure TStructuredLogger.LogInfo(const AMessage: string;
  AData: TJSONObject);
var
  LogEntry: TJSONObject;
begin
  LogEntry := CreateBaseObject;
  try
    LogEntry.Add('level', 'INFO');
    LogEntry.Add('message', AMessage);

    if AData <> nil then
      LogEntry.Add('data', AData.Clone);

    // Écrire en JSON (stdout pour collecte par log aggregator)
    WriteLn(LogEntry.AsJSON);

  finally
    LogEntry.Free;
  end;
end;

procedure TStructuredLogger.LogError(const AMessage: string;
  AError: Exception; AData: TJSONObject);
var
  LogEntry: TJSONObject;
  ErrorObj: TJSONObject;
begin
  LogEntry := CreateBaseObject;
  try
    LogEntry.Add('level', 'ERROR');
    LogEntry.Add('message', AMessage);

    ErrorObj := TJSONObject.Create;
    ErrorObj.Add('type', AError.ClassName);
    ErrorObj.Add('message', AError.Message);
    LogEntry.Add('error', ErrorObj);

    if AData <> nil then
      LogEntry.Add('data', AData.Clone);

    WriteLn(LogEntry.AsJSON);

  finally
    LogEntry.Free;
  end;
end;

procedure TStructuredLogger.LogRequest(const AMethod, APath: string;
  AStatusCode, ADurationMs: Integer);
var
  LogEntry: TJSONObject;
  RequestObj: TJSONObject;
begin
  LogEntry := CreateBaseObject;
  try
    LogEntry.Add('level', 'INFO');
    LogEntry.Add('message', 'HTTP Request');

    RequestObj := TJSONObject.Create;
    RequestObj.Add('method', AMethod);
    RequestObj.Add('path', APath);
    RequestObj.Add('status_code', AStatusCode);
    RequestObj.Add('duration_ms', ADurationMs);
    LogEntry.Add('http', RequestObj);

    WriteLn(LogEntry.AsJSON);

  finally
    LogEntry.Free;
  end;
end;

end.
```

## Principe 6 : Immutabilité

Les conteneurs Cloud-Native sont **immutables** : on ne les modifie pas, on les remplace.

### Mauvaise approche

```bash
# ❌ MAUVAIS : Modifier un conteneur en cours
docker exec myapp apt-get update  
docker exec myapp apt-get install -y new-package  
docker exec myapp service myapp restart

# Problème : changements perdus au prochain redémarrage
```

### Bonne approche

```dockerfile
# ✅ BON : Build une nouvelle image
FROM fpc:3.2.2

WORKDIR /app

# Dépendances
RUN apt-get update && \
    apt-get install -y libssl-dev && \
    rm -rf /var/lib/apt/lists/*

# Code
COPY src/ /app/src/  
RUN fpc -O2 /app/src/myapp.pas

# Configuration
COPY config/ /app/config/

# User non-root
RUN useradd -m -u 1000 appuser  
USER appuser

# Health check
HEALTHCHECK --interval=30s --timeout=3s --start-period=5s \
  CMD curl -f http://localhost:8080/health/live || exit 1

EXPOSE 8080

CMD ["./myapp"]
```

### Versioning des images

```bash
# ✅ BON : Tags sémantiques
docker build -t myapp:1.2.3 .  
docker build -t myapp:1.2 .  
docker build -t myapp:1 .  
docker build -t myapp:latest .

# Déploiement
kubectl set image deployment/myapp myapp=myapp:1.2.3
```

## Déploiement Cloud-Native Complet

### 1. Application complète

```pascal
program CloudNativeApp;

{$mode objfpc}{$H+}

uses
  SysUtils,
  CloudNative.Config,
  CloudNative.Health,
  CloudNative.GracefulApp,
  CloudNative.StructuredLogging;

var
  Config: TAppConfig;
  App: TGracefulApp;
  Logger: TStructuredLogger;
  HealthCheck: TDatabaseHealthCheck;

begin
  WriteLn('═══════════════════════════════════════════');
  WriteLn('  APPLICATION CLOUD-NATIVE                ');
  WriteLn('═══════════════════════════════════════════');
  WriteLn;

  // Charger la configuration
  Config := TAppConfig.Create;
  try
    Config.Validate;
    Config.Print;

    // Logger structuré
    Logger := TStructuredLogger.Create(
      'myapp',
      GetEnvironmentVariable('ENVIRONMENT'),
      '1.2.3'
    );

    try
      Logger.LogInfo('Application démarrage');

      // Créer l'application
      App := TGracefulApp.Create(Config.Port);
      try
        // Ajouter health checks
        HealthCheck := TDatabaseHealthCheck.Create(
          Format('host=%s port=%d dbname=%s',
            [Config.DatabaseHost, Config.DatabasePort, Config.DatabaseName])
        );

        App.HealthManager.AddCheck(HealthCheck);

        // Marquer comme prêt après initialisation
        Sleep(2000);  // Simuler init
        App.HealthManager.SetReady(True);

        Logger.LogInfo('Application prête');

        // Démarrer
        App.Start;

        WriteLn;
        WriteLn('Application running. Press Ctrl+C to stop.');
        WriteLn;

        // Attendre indéfiniment (signals gérés par ShutdownManager)
        while True do
          Sleep(1000);

      finally
        App.Free;
      end;

    finally
      Logger.Free;
    end;

  finally
    Config.Free;
  end;
end.
```

### 2. Structure du projet

```
project/
├── src/
│   ├── myapp.pas
│   ├── CloudNative.Config.pas
│   ├── CloudNative.Health.pas
│   ├── CloudNative.GracefulShutdown.pas
│   └── CloudNative.StructuredLogging.pas
├── Dockerfile
├── docker-compose.yml
├── k8s/
│   ├── deployment.yaml
│   ├── service.yaml
│   ├── configmap.yaml
│   └── secret.yaml
├── .dockerignore
├── .gitignore
└── README.md
```

### 3. Kubernetes Manifests complets

```yaml
# k8s/configmap.yaml
apiVersion: v1  
kind: ConfigMap  
metadata:
  name: myapp-config
data:
  LOG_LEVEL: "info"
  ENVIRONMENT: "production"
  DB_HOST: "postgres-service"
  DB_PORT: "5432"
  DB_NAME: "myapp"
```

```yaml
# k8s/secret.yaml
apiVersion: v1  
kind: Secret  
metadata:
  name: myapp-secret
type: Opaque  
stringData:
  DB_PASSWORD: "SuperSecretPassword123!"
  API_KEY: "my-secret-api-key"
```

```yaml
# k8s/service.yaml
apiVersion: v1  
kind: Service  
metadata:
  name: myapp-service
spec:
  selector:
    app: myapp
  ports:
  - protocol: TCP
    port: 80
    targetPort: 8080
  type: LoadBalancer
```

```yaml
# k8s/deployment.yaml
apiVersion: apps/v1  
kind: Deployment  
metadata:
  name: myapp
  labels:
    app: myapp
    version: "1.2.3"
spec:
  replicas: 3
  strategy:
    type: RollingUpdate
    rollingUpdate:
      maxSurge: 1
      maxUnavailable: 0
  selector:
    matchLabels:
      app: myapp
  template:
    metadata:
      labels:
        app: myapp
        version: "1.2.3"
    spec:
      containers:
      - name: myapp
        image: myapp:1.2.3
        imagePullPolicy: Always
        ports:
        - containerPort: 8080
          name: http

        # Configuration
        envFrom:
        - configMapRef:
            name: myapp-config
        env:
        - name: PORT
          value: "8080"
        - name: DB_USER
          value: "appuser"
        - name: DB_PASSWORD
          valueFrom:
            secretKeyRef:
              name: myapp-secret
              key: DB_PASSWORD
        - name: API_KEY
          valueFrom:
            secretKeyRef:
              name: myapp-secret
              key: API_KEY

        # Probes
        livenessProbe:
          httpGet:
            path: /health/live
            port: 8080
          initialDelaySeconds: 10
          periodSeconds: 10

        readinessProbe:
          httpGet:
            path: /health/ready
            port: 8080
          initialDelaySeconds: 5
          periodSeconds: 5

        # Ressources
        resources:
          requests:
            memory: "64Mi"
            cpu: "100m"
          limits:
            memory: "128Mi"
            cpu: "500m"

        # Sécurité
        securityContext:
          runAsNonRoot: true
          runAsUser: 1000
          readOnlyRootFilesystem: true
          allowPrivilegeEscalation: false
```

### 4. Déploiement avec CI/CD

```yaml
# .github/workflows/deploy.yml
name: Build and Deploy

on:
  push:
    branches: [ main ]
    tags: [ 'v*' ]

env:
  REGISTRY: ghcr.io
  IMAGE_NAME: ${{ github.repository }}

jobs:
  build:
    runs-on: ubuntu-latest
    permissions:
      contents: read
      packages: write

    steps:
    - name: Checkout
      uses: actions/checkout@v3

    - name: Set up Docker Buildx
      uses: docker/setup-buildx-action@v2

    - name: Log in to Container Registry
      uses: docker/login-action@v2
      with:
        registry: ${{ env.REGISTRY }}
        username: ${{ github.actor }}
        password: ${{ secrets.GITHUB_TOKEN }}

    - name: Extract metadata
      id: meta
      uses: docker/metadata-action@v4
      with:
        images: ${{ env.REGISTRY }}/${{ env.IMAGE_NAME }}
        tags: |
          type=ref,event=branch
          type=ref,event=pr
          type=semver,pattern={{version}}
          type=semver,pattern={{major}}.{{minor}}

    - name: Build and push
      uses: docker/build-push-action@v4
      with:
        context: .
        push: true
        tags: ${{ steps.meta.outputs.tags }}
        labels: ${{ steps.meta.outputs.labels }}
        cache-from: type=gha
        cache-to: type=gha,mode=max

  deploy:
    needs: build
    runs-on: ubuntu-latest
    if: github.ref == 'refs/heads/main'

    steps:
    - name: Checkout
      uses: actions/checkout@v3

    - name: Set up kubectl
      uses: azure/setup-kubectl@v3

    - name: Configure kubectl
      run: |
        echo "${{ secrets.KUBECONFIG }}" > kubeconfig
        export KUBECONFIG=kubeconfig

    - name: Deploy to Kubernetes
      run: |
        kubectl apply -f k8s/configmap.yaml
        kubectl apply -f k8s/secret.yaml
        kubectl apply -f k8s/deployment.yaml
        kubectl apply -f k8s/service.yaml
        kubectl rollout status deployment/myapp
```

## Scaling et Auto-scaling

### 1. Horizontal Pod Autoscaler (HPA)

```yaml
# k8s/hpa.yaml
apiVersion: autoscaling/v2  
kind: HorizontalPodAutoscaler  
metadata:
  name: myapp-hpa
spec:
  scaleTargetRef:
    apiVersion: apps/v1
    kind: Deployment
    name: myapp
  minReplicas: 3
  maxReplicas: 10
  metrics:
  - type: Resource
    resource:
      name: cpu
      target:
        type: Utilization
        averageUtilization: 70
  - type: Resource
    resource:
      name: memory
      target:
        type: Utilization
        averageUtilization: 80
  behavior:
    scaleDown:
      stabilizationWindowSeconds: 300
      policies:
      - type: Percent
        value: 50
        periodSeconds: 60
    scaleUp:
      stabilizationWindowSeconds: 0
      policies:
      - type: Percent
        value: 100
        periodSeconds: 15
      - type: Pods
        value: 2
        periodSeconds: 15
      selectPolicy: Max
```

### 2. Vertical Pod Autoscaler (VPA)

```yaml
# k8s/vpa.yaml
apiVersion: autoscaling.k8s.io/v1  
kind: VerticalPodAutoscaler  
metadata:
  name: myapp-vpa
spec:
  targetRef:
    apiVersion: apps/v1
    kind: Deployment
    name: myapp
  updatePolicy:
    updateMode: "Auto"
  resourcePolicy:
    containerPolicies:
    - containerName: myapp
      minAllowed:
        cpu: 50m
        memory: 32Mi
      maxAllowed:
        cpu: 1
        memory: 256Mi
```

## Résilience et Patterns Avancés

### 1. Circuit Breaker Pattern

```pascal
unit CloudNative.Resilience;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils;

type
  // Type de fonction retournant Boolean (TBooleanFunc n'existe pas en FPC)
  TBooleanFunc = function: Boolean of object;

  // État du circuit breaker
  TCircuitState = (csClose, csOpen, csHalfOpen);

  // Circuit Breaker pour appels externes
  TCircuitBreaker = class
  private
    FServiceName: string;
    FState: TCircuitState;
    FFailureCount: Integer;
    FFailureThreshold: Integer;
    FTimeoutSeconds: Integer;
    FLastFailureTime: TDateTime;
    FSuccessCountInHalfOpen: Integer;

    function ShouldAttemptCall: Boolean;
    procedure RecordSuccess;
    procedure RecordFailure;
  public
    constructor Create(const AServiceName: string;
      AFailureThreshold: Integer = 5;
      ATimeoutSeconds: Integer = 60);

    function Execute(AOperation: TBooleanFunc): Boolean;

    property State: TCircuitState read FState;
  end;

  // Retry avec backoff exponentiel
  TRetryPolicy = class
  private
    FMaxRetries: Integer;
    FInitialDelayMs: Integer;
    FMaxDelayMs: Integer;

    function CalculateDelay(AAttempt: Integer): Integer;
  public
    constructor Create(AMaxRetries: Integer = 3;
      AInitialDelayMs: Integer = 1000;
      AMaxDelayMs: Integer = 10000);

    function ExecuteWithRetry(AOperation: TBooleanFunc): Boolean;
  end;

implementation

// TCircuitBreaker

constructor TCircuitBreaker.Create(const AServiceName: string;
  AFailureThreshold, ATimeoutSeconds: Integer);
begin
  inherited Create;
  FServiceName := AServiceName;
  FState := csClose;
  FFailureCount := 0;
  FFailureThreshold := AFailureThreshold;
  FTimeoutSeconds := ATimeoutSeconds;
  FSuccessCountInHalfOpen := 0;

  WriteLn(Format('[CircuitBreaker:%s] Créé (seuil: %d, timeout: %ds)',
    [FServiceName, AFailureThreshold, ATimeoutSeconds]));
end;

function TCircuitBreaker.ShouldAttemptCall: Boolean;  
var
  ElapsedSeconds: Int64;
begin
  case FState of
    csClose:
      Result := True;

    csOpen:
    begin
      ElapsedSeconds := SecondsBetween(Now, FLastFailureTime);
      if ElapsedSeconds >= FTimeoutSeconds then
      begin
        WriteLn(Format('[CircuitBreaker:%s] Passage en HALF-OPEN',
          [FServiceName]));
        FState := csHalfOpen;
        FSuccessCountInHalfOpen := 0;
        Result := True;
      end
      else
        Result := False;
    end;

    csHalfOpen:
      Result := True;
  end;
end;

procedure TCircuitBreaker.RecordSuccess;  
begin
  case FState of
    csClose:
      FFailureCount := 0;

    csHalfOpen:
    begin
      Inc(FSuccessCountInHalfOpen);
      if FSuccessCountInHalfOpen >= 3 then
      begin
        WriteLn(Format('[CircuitBreaker:%s] ✓ Retour en CLOSED',
          [FServiceName]));
        FState := csClose;
        FFailureCount := 0;
      end;
    end;
  end;
end;

procedure TCircuitBreaker.RecordFailure;  
begin
  FLastFailureTime := Now;

  case FState of
    csClose:
    begin
      Inc(FFailureCount);
      if FFailureCount >= FFailureThreshold then
      begin
        WriteLn(Format('[CircuitBreaker:%s] ✗ Passage en OPEN',
          [FServiceName]));
        FState := csOpen;
      end;
    end;

    csHalfOpen:
    begin
      WriteLn(Format('[CircuitBreaker:%s] ✗ Retour en OPEN',
        [FServiceName]));
      FState := csOpen;
      FSuccessCountInHalfOpen := 0;
    end;
  end;
end;

function TCircuitBreaker.Execute(AOperation: TBooleanFunc): Boolean;  
begin
  if not ShouldAttemptCall then
  begin
    WriteLn(Format('[CircuitBreaker:%s] Appel bloqué (circuit OPEN)',
      [FServiceName]));
    Result := False;
    Exit;
  end;

  try
    Result := AOperation();

    if Result then
      RecordSuccess
    else
      RecordFailure;

  except
    on E: Exception do
    begin
      WriteLn(Format('[CircuitBreaker:%s] Exception: %s',
        [FServiceName, E.Message]));
      RecordFailure;
      Result := False;
    end;
  end;
end;

// TRetryPolicy

constructor TRetryPolicy.Create(AMaxRetries, AInitialDelayMs,
  AMaxDelayMs: Integer);
begin
  inherited Create;
  FMaxRetries := AMaxRetries;
  FInitialDelayMs := AInitialDelayMs;
  FMaxDelayMs := AMaxDelayMs;
end;

function TRetryPolicy.CalculateDelay(AAttempt: Integer): Integer;  
begin
  // Backoff exponentiel : delay = initial * 2^attempt
  Result := FInitialDelayMs * (1 shl AAttempt);

  if Result > FMaxDelayMs then
    Result := FMaxDelayMs;

  // Ajouter du jitter (±20%)
  Result := Result + Random(Result div 5) - (Result div 10);
end;

function TRetryPolicy.ExecuteWithRetry(AOperation: TBooleanFunc): Boolean;  
var
  Attempt: Integer;
  Delay: Integer;
begin
  for Attempt := 0 to FMaxRetries do
  begin
    try
      Result := AOperation();

      if Result then
      begin
        if Attempt > 0 then
          WriteLn(Format('[RetryPolicy] ✓ Succès après %d tentatives',
            [Attempt + 1]));
        Exit;
      end;

    except
      on E: Exception do
        WriteLn(Format('[RetryPolicy] Tentative %d échouée: %s',
          [Attempt + 1, E.Message]));
    end;

    if Attempt < FMaxRetries then
    begin
      Delay := CalculateDelay(Attempt);
      WriteLn(Format('[RetryPolicy] Nouvelle tentative dans %dms...', [Delay]));
      Sleep(Delay);
    end;
  end;

  WriteLn(Format('[RetryPolicy] ✗ Échec après %d tentatives', [FMaxRetries + 1]));
  Result := False;
end;

end.
```

### 2. Bulkhead Pattern

```pascal
unit CloudNative.Bulkhead;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs;

type
  // Procédure sans paramètre (TProc n'existe pas en FPC)
  TSimpleProc = procedure of object;

  // Bulkhead : isolation des ressources
  TBulkhead = class
  private
    FName: string;
    FMaxConcurrent: Integer;
    FSemaphore: TSemaphore;
    FActiveCount: Integer;
    FRejectedCount: Integer;
  public
    constructor Create(const AName: string; AMaxConcurrent: Integer);
    destructor Destroy; override;

    function TryAcquire: Boolean;
    procedure Release;

    function Execute(AOperation: TSimpleProc): Boolean;

    property ActiveCount: Integer read FActiveCount;
    property RejectedCount: Integer read FRejectedCount;
  end;

implementation

// TBulkhead

constructor TBulkhead.Create(const AName: string; AMaxConcurrent: Integer);  
begin
  inherited Create;
  FName := AName;
  FMaxConcurrent := AMaxConcurrent;
  FSemaphore := TSemaphore.Create(nil, AMaxConcurrent, AMaxConcurrent, '');
  FActiveCount := 0;
  FRejectedCount := 0;

  WriteLn(Format('[Bulkhead:%s] Créé (max concurrent: %d)',
    [FName, AMaxConcurrent]));
end;

destructor TBulkhead.Destroy;  
begin
  FSemaphore.Free;
  inherited;
end;

function TBulkhead.TryAcquire: Boolean;  
begin
  Result := FSemaphore.WaitFor(0) = wrSignaled;

  if Result then
  begin
    InterlockedIncrement(FActiveCount);
    WriteLn(Format('[Bulkhead:%s] Slot acquis (actifs: %d/%d)',
      [FName, FActiveCount, FMaxConcurrent]));
  end
  else
  begin
    InterlockedIncrement(FRejectedCount);
    WriteLn(Format('[Bulkhead:%s] ✗ Rejeté (limite atteinte)', [FName]));
  end;
end;

procedure TBulkhead.Release;  
begin
  FSemaphore.Release;
  InterlockedDecrement(FActiveCount);
  WriteLn(Format('[Bulkhead:%s] Slot libéré (actifs: %d/%d)',
    [FName, FActiveCount, FMaxConcurrent]));
end;

function TBulkhead.Execute(AOperation: TSimpleProc): Boolean;  
begin
  Result := TryAcquire;

  if not Result then
    Exit;

  try
    AOperation();
  finally
    Release;
  end;
end;

end.
```

## Monitoring et Observabilité en Production

### 1. Métriques Prometheus

```pascal
unit CloudNative.Prometheus;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpserver, httpdefs;

type
  // Exposer des métriques Prometheus
  TPrometheusExporter = class
  private
    FServer: TFPHTTPServer;
    FPort: Integer;

    procedure HandleRequest(Sender: TObject;
      var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse);
  public
    constructor Create(APort: Integer);
    destructor Destroy; override;

    procedure Start;
    procedure Stop;
  end;

implementation

// TPrometheusExporter

constructor TPrometheusExporter.Create(APort: Integer);  
begin
  inherited Create;
  FPort := APort;

  FServer := TFPHTTPServer.Create(nil);
  FServer.Port := FPort;
  FServer.OnRequest := @HandleRequest;

  WriteLn('[PrometheusExporter] Créé sur le port ', FPort);
end;

destructor TPrometheusExporter.Destroy;  
begin
  Stop;
  FServer.Free;
  inherited;
end;

procedure TPrometheusExporter.HandleRequest(Sender: TObject;
  var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
var
  Metrics: TStringList;
begin
  Metrics := TStringList.Create;
  try
    // Format Prometheus
    Metrics.Add('# HELP http_requests_total Total HTTP requests');
    Metrics.Add('# TYPE http_requests_total counter');
    Metrics.Add('http_requests_total{method="GET",status="200"} 1247');
    Metrics.Add('http_requests_total{method="POST",status="201"} 523');
    Metrics.Add('');

    Metrics.Add('# HELP http_request_duration_seconds HTTP request latency');
    Metrics.Add('# TYPE http_request_duration_seconds histogram');
    Metrics.Add('http_request_duration_seconds_bucket{le="0.1"} 856');
    Metrics.Add('http_request_duration_seconds_bucket{le="0.5"} 1654');
    Metrics.Add('http_request_duration_seconds_bucket{le="1.0"} 1745');
    Metrics.Add('http_request_duration_seconds_bucket{le="+Inf"} 1770');
    Metrics.Add('http_request_duration_seconds_sum 892.45');
    Metrics.Add('http_request_duration_seconds_count 1770');
    Metrics.Add('');

    Metrics.Add('# HELP app_info Application information');
    Metrics.Add('# TYPE app_info gauge');
    Metrics.Add('app_info{version="1.2.3",environment="production"} 1');

    AResponse.Code := 200;
    AResponse.Content := Metrics.Text;
    AResponse.ContentType := 'text/plain; version=0.0.4';

  finally
    Metrics.Free;
  end;
end;

procedure TPrometheusExporter.Start;  
begin
  FServer.Active := True;
  WriteLn(Format('[PrometheusExporter] ✓ Démarré sur http://localhost:%d/metrics',
    [FPort]));
end;

procedure TPrometheusExporter.Stop;  
begin
  if FServer.Active then
  begin
    FServer.Active := False;
    WriteLn('[PrometheusExporter] Arrêté');
  end;
end;

end.
```

### 2. Service Monitor pour Prometheus Operator

```yaml
# k8s/servicemonitor.yaml
apiVersion: monitoring.coreos.com/v1  
kind: ServiceMonitor  
metadata:
  name: myapp
  labels:
    app: myapp
spec:
  selector:
    matchLabels:
      app: myapp
  endpoints:
  - port: metrics
    interval: 30s
    path: /metrics
```

## Bonnes Pratiques Cloud-Native

### ✅ À faire

1. **Configuration externalisée**
```pascal
// ✅ BON
Config := TAppConfig.Create;  
Config.LoadFromEnvironment;
```

2. **Logs structurés**
```pascal
// ✅ BON
Logger.LogInfo('Order created',
  '{"order_id": "123", "amount": 99.99}');
```

3. **Health checks complets**
```pascal
// ✅ BON
HealthManager.AddCheck(TDatabaseHealthCheck.Create);  
HealthManager.AddCheck(TRedisHealthCheck.Create);
```

4. **Graceful shutdown**
```pascal
// ✅ BON
ShutdownManager.RegisterCallback(@OnShutdown);
// Attendre fin requêtes en cours
```

5. **Résilience**
```pascal
// ✅ BON
CircuitBreaker.Execute(function: Boolean  
begin
  Result := CallExternalService;
end);
```

### ❌ À éviter

1. **Configuration en dur**
```pascal
// ❌ MAUVAIS
const DB_PASSWORD = 'secret123';
```

2. **État local**
```pascal
// ❌ MAUVAIS
var GlobalSession: TSession;  // En mémoire
```

3. **Arrêt brutal**
```pascal
// ❌ MAUVAIS
Halt(0);  // Sans attendre les requêtes
```

4. **Logs non structurés**
```pascal
// ❌ MAUVAIS
WriteLn('Order 123 created for user John amount 99.99');
```

5. **Pas de limites de ressources**
```yaml
# ❌ MAUVAIS
resources: {}  # Pas de limits
```

## Checklist Application Cloud-Native

### Configuration
- [ ] Variables d'environnement pour toute config
- [ ] Secrets gérés via Kubernetes Secrets
- [ ] Validation de la configuration au démarrage
- [ ] Valeurs par défaut raisonnables

### Stateless
- [ ] Aucune donnée utilisateur en mémoire locale
- [ ] Session dans Redis/base de données
- [ ] Aucun fichier écrit localement (sauf logs)
- [ ] Horizontal scaling possible

### Health Checks
- [ ] Endpoint /health/live (liveness)
- [ ] Endpoint /health/ready (readiness)
- [ ] Vérification des dépendances (DB, Redis, etc.)
- [ ] Temps de réponse < 1 seconde

### Graceful Shutdown
- [ ] Gestion des signaux SIGTERM
- [ ] Arrêt d'accepter nouvelles requêtes
- [ ] Attente fin des requêtes en cours
- [ ] Fermeture propre des connexions
- [ ] Timeout configuré (30s recommandé)

### Observabilité
- [ ] Logs structurés en JSON
- [ ] Métriques Prometheus exposées
- [ ] Tracing distribué (trace ID)
- [ ] Correlation logs/traces/métriques

### Sécurité
- [ ] Exécution en utilisateur non-root
- [ ] Système de fichiers read-only
- [ ] Pas de privilèges élevés
- [ ] Scan de vulnérabilités des images

### Ressources
- [ ] Requests définis (CPU/Memory)
- [ ] Limits définis (CPU/Memory)
- [ ] HPA configuré si nécessaire
- [ ] PodDisruptionBudget pour production

### Résilience
- [ ] Circuit breaker pour appels externes
- [ ] Retry avec backoff exponentiel
- [ ] Timeout sur toutes les opérations
- [ ] Dégradation gracieuse

### Build et Déploiement
- [ ] Images immutables avec tags sémantiques
- [ ] Multi-stage builds pour images légères
- [ ] Pipeline CI/CD automatisé
- [ ] Rollback facile en cas de problème

## Conclusion

L'architecture Cloud-Native transforme la façon dont nous construisons et déployons des applications. Les points clés à retenir :

### Principes Fondamentaux

**12-Factor App :**
- Configuration dans l'environnement
- Dépendances explicites
- Processus stateless
- Build/Release/Run séparés
- Logs comme flux d'événements

**Conteneurisation :**
- Images Docker immutables
- Un processus par conteneur
- Versioning sémantique
- Registres de conteneurs

**Orchestration Kubernetes :**
- Déploiement déclaratif
- Auto-scaling (HPA/VPA)
- Self-healing automatique
- Rolling updates sans downtime

### Observabilité Complète

**Les trois piliers :**
- **Logs** : Structurés en JSON
- **Metrics** : Format Prometheus
- **Traces** : Distributed tracing

**Health Checks :**
- Liveness : "Suis-je vivant ?"
- Readiness : "Puis-je recevoir du trafic ?"
- Startup : "Ai-je fini de démarrer ?"

### Résilience

**Patterns essentiels :**
- Circuit Breaker
- Retry avec backoff
- Bulkhead (isolation)
- Timeout sur tout
- Graceful shutdown

### Avantages Cloud-Native

**Scalabilité :**
- Horizontal scaling automatique
- Adapté à la charge en temps réel
- Utilisation optimale des ressources

**Résilience :**
- Auto-healing des pods défaillants
- Isolation des pannes
- Dégradation gracieuse

**Agilité :**
- Déploiements fréquents et sûrs
- Rollback instantané
- A/B testing et canary releases

**Coût :**
- Payer uniquement les ressources utilisées
- Optimisation automatique
- Multi-cloud possible

### En Production

Une application Cloud-Native bien conçue :
- Se déploie en secondes
- Scale automatiquement selon la charge
- Se répare toute seule en cas de panne
- Expose des métriques complètes
- S'arrête proprement sans perdre de requêtes

Le Cloud-Native n'est pas juste une technologie, c'est une **philosophie** de développement qui permet de construire des systèmes distribués fiables, scalables et observables !

---

⏭️ [DevOps et Déploiement Multi-OS](/22-devops-deploiement-multi-os/README.md)
