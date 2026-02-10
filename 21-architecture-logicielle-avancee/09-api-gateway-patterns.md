🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 21.9 API Gateway Patterns

## Introduction

Une **API Gateway** est un point d'entrée unique pour toutes les requêtes venant des clients vers vos microservices. C'est comme un réceptionniste d'hôtel qui dirige les visiteurs vers les bons services.

### Le problème sans API Gateway

Imaginez une application e-commerce avec plusieurs microservices :

```
Client Mobile ──┐
                ├──► Service Produits (port 8001)
Client Web ────┤──► Service Commandes (port 8002)
                ├──► Service Paiements (port 8003)
Client API ────┘──► Service Utilisateurs (port 8004)
```

**Problèmes :**
- Le client doit connaître tous les services et leurs adresses
- Chaque client doit gérer l'authentification pour chaque service
- Impossible de faire des changements sans impacter les clients
- Pas de point central pour la sécurité, le monitoring, etc.

### Avec une API Gateway

```
                    ┌──► Service Produits
                    │
Client ──► Gateway ─┼──► Service Commandes
                    │
                    ├──► Service Paiements
                    │
                    └──► Service Utilisateurs
```

**Avantages :**
- Un seul point d'entrée pour les clients
- Authentification centralisée
- Routage intelligent des requêtes
- Transformation et agrégation des réponses
- Cache, rate limiting, monitoring centralisé

**Analogie :** L'API Gateway est comme un guichet unique dans une administration. Au lieu de courir entre différents bureaux, vous allez à un guichet qui s'occupe de tout pour vous.

## Structure de Base d'une API Gateway

### 1. Gateway Minimal

```pascal
unit APIGateway.Core;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpserver, httpdefs, fpjson;

type
  // Configuration d'une route
  TRouteConfig = record
    Path: string;           // /api/products
    Method: string;         // GET, POST, etc.
    TargetService: string;  // http://localhost:8001
    TargetPath: string;     // /products
  end;

  // Gateway de base
  TAPIGateway = class
  private
    FServer: TFPHTTPServer;
    FRoutes: array of TRouteConfig;
    FPort: Integer;

    procedure HandleRequest(Sender: TObject;
      var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse);

    function FindRoute(const APath, AMethod: string): Integer;
    function ForwardRequest(const ARoute: TRouteConfig;
      ARequest: TFPHTTPConnectionRequest): string;
  public
    constructor Create(APort: Integer);
    destructor Destroy; override;

    procedure AddRoute(const APath, AMethod, ATargetService,
      ATargetPath: string);
    procedure Start;
    procedure Stop;

    property Port: Integer read FPort;
  end;

implementation

uses
  fphttpclient;

// TAPIGateway

constructor TAPIGateway.Create(APort: Integer);  
begin
  inherited Create;
  FPort := APort;
  FServer := TFPHTTPServer.Create(nil);
  FServer.Port := FPort;
  FServer.OnRequest := @HandleRequest;

  SetLength(FRoutes, 0);

  WriteLn(Format('[Gateway] Créé sur le port %d', [FPort]));
end;

destructor TAPIGateway.Destroy;  
begin
  Stop;
  FServer.Free;
  inherited;
end;

procedure TAPIGateway.AddRoute(const APath, AMethod, ATargetService,
  ATargetPath: string);
var
  Route: TRouteConfig;
  Len: Integer;
begin
  Route.Path := APath;
  Route.Method := AMethod;
  Route.TargetService := ATargetService;
  Route.TargetPath := ATargetPath;

  Len := Length(FRoutes);
  SetLength(FRoutes, Len + 1);
  FRoutes[Len] := Route;

  WriteLn(Format('[Gateway] Route ajoutée: %s %s -> %s%s',
    [AMethod, APath, ATargetService, ATargetPath]));
end;

function TAPIGateway.FindRoute(const APath, AMethod: string): Integer;  
var
  i: Integer;
begin
  Result := -1;

  for i := 0 to High(FRoutes) do
  begin
    if (FRoutes[i].Path = APath) and (FRoutes[i].Method = AMethod) then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

function TAPIGateway.ForwardRequest(const ARoute: TRouteConfig;
  ARequest: TFPHTTPConnectionRequest): string;
var
  Client: TFPHTTPClient;
  TargetURL: string;
begin
  Client := TFPHTTPClient.Create(nil);
  try
    TargetURL := ARoute.TargetService + ARoute.TargetPath;

    WriteLn(Format('[Gateway] Forwarding: %s %s', [ARoute.Method, TargetURL]));

    if ARoute.Method = 'GET' then
      Result := Client.Get(TargetURL)
    else if ARoute.Method = 'POST' then
      Result := Client.FormPost(TargetURL, ARequest.Content)
    else if ARoute.Method = 'PUT' then
    begin
      Client.RequestBody := TStringStream.Create(ARequest.Content);
      Result := Client.Put(TargetURL);
    end
    else if ARoute.Method = 'DELETE' then
      Result := Client.Delete(TargetURL)
    else
      Result := '{"error": "Méthode non supportée"}';

  finally
    Client.Free;
  end;
end;

procedure TAPIGateway.HandleRequest(Sender: TObject;
  var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
var
  RouteIndex: Integer;
  ResponseContent: string;
begin
  WriteLn;
  WriteLn(Format('[Gateway] Requête: %s %s', [ARequest.Method, ARequest.URI]));

  // Chercher la route
  RouteIndex := FindRoute(ARequest.URI, ARequest.Method);

  if RouteIndex = -1 then
  begin
    // Route non trouvée
    AResponse.Code := 404;
    AResponse.Content := '{"error": "Route non trouvée"}';
    WriteLn('[Gateway] ✗ Route non trouvée');
  end
  else
  begin
    try
      // Transférer la requête au service
      ResponseContent := ForwardRequest(FRoutes[RouteIndex], ARequest);

      AResponse.Code := 200;
      AResponse.Content := ResponseContent;
      AResponse.ContentType := 'application/json';

      WriteLn('[Gateway] ✓ Réponse envoyée');

    except
      on E: Exception do
      begin
        AResponse.Code := 500;
        AResponse.Content := Format('{"error": "%s"}', [E.Message]);
        WriteLn(Format('[Gateway] ✗ Erreur: %s', [E.Message]));
      end;
    end;
  end;
end;

procedure TAPIGateway.Start;  
begin
  FServer.Active := True;
  WriteLn(Format('[Gateway] Démarré sur http://localhost:%d', [FPort]));
end;

procedure TAPIGateway.Stop;  
begin
  if FServer.Active then
  begin
    FServer.Active := False;
    WriteLn('[Gateway] Arrêté');
  end;
end;

end.
```

### 2. Exemple d'utilisation simple

```pascal
program SimpleGateway;

{$mode objfpc}{$H+}

uses
  SysUtils,
  APIGateway.Core;

var
  Gateway: TAPIGateway;

begin
  WriteLn('═══════════════════════════════════════════');
  WriteLn('  API Gateway - Exemple Simple            ');
  WriteLn('═══════════════════════════════════════════');
  WriteLn;

  Gateway := TAPIGateway.Create(8080);
  try
    // Configuration des routes
    Gateway.AddRoute('/api/products', 'GET',
      'http://localhost:8001', '/products');

    Gateway.AddRoute('/api/orders', 'GET',
      'http://localhost:8002', '/orders');

    Gateway.AddRoute('/api/orders', 'POST',
      'http://localhost:8002', '/orders');

    Gateway.AddRoute('/api/users', 'GET',
      'http://localhost:8003', '/users');

    // Démarrer le gateway
    Gateway.Start;

    WriteLn;
    WriteLn('Gateway actif. Appuyez sur Entrée pour arrêter...');
    ReadLn;

  finally
    Gateway.Free;
  end;
end.
```

## Pattern 1 : Authentification Centralisée

L'un des rôles principaux de la Gateway est de gérer l'authentification une seule fois pour tous les services.

### 1. Module d'authentification

```pascal
unit APIGateway.Auth;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, base64;

type
  // Token JWT simplifié
  TAuthToken = record
    UserId: string;
    Username: string;
    Roles: array of string;
    ExpiresAt: TDateTime;
  end;

  // Gestionnaire d'authentification
  TAuthManager = class
  private
    FSecretKey: string;

    function ParseToken(const AToken: string): TAuthToken;
    function IsTokenExpired(const AToken: TAuthToken): Boolean;
  public
    constructor Create(const ASecretKey: string);

    function ValidateToken(const AToken: string): Boolean;
    function ExtractToken(const AAuthHeader: string): string;
    function GetUserFromToken(const AToken: string): string;
    function HasRole(const AToken: string; const ARole: string): Boolean;
  end;

implementation

uses
  DateUtils;

// TAuthManager

constructor TAuthManager.Create(const ASecretKey: string);  
begin
  inherited Create;
  FSecretKey := ASecretKey;
  WriteLn('[Auth] Gestionnaire d''authentification initialisé');
end;

function TAuthManager.ExtractToken(const AAuthHeader: string): string;  
begin
  // Format: "Bearer token123456"
  if Pos('Bearer ', AAuthHeader) = 1 then
    Result := Copy(AAuthHeader, 8, Length(AAuthHeader) - 7)
  else
    Result := '';
end;

function TAuthManager.ParseToken(const AToken: string): TAuthToken;  
var
  Decoded: string;
  JSON: TJSONObject;
  Parser: TJSONParser;
  RolesArray: TJSONArray;
  i: Integer;
begin
  // Simulation de décodage JWT (version simplifiée)
  try
    // En production, utiliser une vraie bibliothèque JWT
    Decoded := DecodeStringBase64(AToken);

    Parser := TJSONParser.Create(Decoded, [joUTF8]);
    try
      JSON := Parser.Parse as TJSONObject;
      try
        Result.UserId := JSON.Get('user_id', '');
        Result.Username := JSON.Get('username', '');

        // Lire les rôles
        if JSON.Find('roles') <> nil then
        begin
          RolesArray := JSON.Get('roles', TJSONArray(nil)) as TJSONArray;
          SetLength(Result.Roles, RolesArray.Count);

          for i := 0 to RolesArray.Count - 1 do
            Result.Roles[i] := RolesArray.Strings[i];
        end;

        // Expiration (timestamp)
        Result.ExpiresAt := UnixToDateTime(JSON.Get('exp', 0));

      finally
        JSON.Free;
      end;
    finally
      Parser.Free;
    end;

  except
    on E: Exception do
    begin
      WriteLn(Format('[Auth] Erreur parsing token: %s', [E.Message]));
      Result.UserId := '';
    end;
  end;
end;

function TAuthManager.IsTokenExpired(const AToken: TAuthToken): Boolean;  
begin
  Result := Now > AToken.ExpiresAt;
end;

function TAuthManager.ValidateToken(const AToken: string): Boolean;  
var
  ParsedToken: TAuthToken;
begin
  Result := False;

  if AToken = '' then
  begin
    WriteLn('[Auth] Token vide');
    Exit;
  end;

  ParsedToken := ParseToken(AToken);

  if ParsedToken.UserId = '' then
  begin
    WriteLn('[Auth] Token invalide');
    Exit;
  end;

  if IsTokenExpired(ParsedToken) then
  begin
    WriteLn('[Auth] Token expiré');
    Exit;
  end;

  WriteLn(Format('[Auth] ✓ Token valide pour: %s', [ParsedToken.Username]));
  Result := True;
end;

function TAuthManager.GetUserFromToken(const AToken: string): string;  
var
  ParsedToken: TAuthToken;
begin
  ParsedToken := ParseToken(AToken);
  Result := ParsedToken.UserId;
end;

function TAuthManager.HasRole(const AToken: string; const ARole: string): Boolean;  
var
  ParsedToken: TAuthToken;
  Role: string;
begin
  Result := False;
  ParsedToken := ParseToken(AToken);

  for Role in ParsedToken.Roles do
  begin
    if Role = ARole then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

end.
```

### 2. Gateway avec authentification

```pascal
unit APIGateway.Secure;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpserver, httpdefs,
  APIGateway.Core, APIGateway.Auth;

type
  // Gateway sécurisée avec authentification
  TSecureAPIGateway = class(TAPIGateway)
  private
    FAuthManager: TAuthManager;
    FPublicPaths: array of string; // Routes sans authentification

    function IsPublicPath(const APath: string): Boolean;
    function CheckAuthentication(ARequest: TFPHTTPConnectionRequest): Boolean;
  protected
    procedure HandleRequest(Sender: TObject;
      var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse); override;
  public
    constructor Create(APort: Integer; const ASecretKey: string);
    destructor Destroy; override;

    procedure AddPublicRoute(const APath: string);

    property AuthManager: TAuthManager read FAuthManager;
  end;

implementation

// TSecureAPIGateway

constructor TSecureAPIGateway.Create(APort: Integer; const ASecretKey: string);  
begin
  inherited Create(APort);
  FAuthManager := TAuthManager.Create(ASecretKey);
  SetLength(FPublicPaths, 0);

  WriteLn('[SecureGateway] Gateway sécurisée créée');
end;

destructor TSecureAPIGateway.Destroy;  
begin
  FAuthManager.Free;
  inherited;
end;

procedure TSecureAPIGateway.AddPublicRoute(const APath: string);  
var
  Len: Integer;
begin
  Len := Length(FPublicPaths);
  SetLength(FPublicPaths, Len + 1);
  FPublicPaths[Len] := APath;

  WriteLn(Format('[SecureGateway] Route publique: %s', [APath]));
end;

function TSecureAPIGateway.IsPublicPath(const APath: string): Boolean;  
var
  PublicPath: string;
begin
  Result := False;

  for PublicPath in FPublicPaths do
  begin
    if APath = PublicPath then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

function TSecureAPIGateway.CheckAuthentication(
  ARequest: TFPHTTPConnectionRequest): Boolean;
var
  AuthHeader: string;
  Token: string;
begin
  AuthHeader := ARequest.GetCustomHeader('Authorization');

  if AuthHeader = '' then
  begin
    WriteLn('[SecureGateway] ✗ Header Authorization manquant');
    Result := False;
    Exit;
  end;

  Token := FAuthManager.ExtractToken(AuthHeader);
  Result := FAuthManager.ValidateToken(Token);
end;

procedure TSecureAPIGateway.HandleRequest(Sender: TObject;
  var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
begin
  WriteLn;
  WriteLn(Format('[SecureGateway] Requête: %s %s',
    [ARequest.Method, ARequest.URI]));

  // Vérifier si la route nécessite l'authentification
  if not IsPublicPath(ARequest.URI) then
  begin
    if not CheckAuthentication(ARequest) then
    begin
      AResponse.Code := 401;
      AResponse.Content := '{"error": "Non authentifié"}';
      AResponse.ContentType := 'application/json';
      WriteLn('[SecureGateway] ✗ Requête rejetée - Non authentifié');
      Exit;
    end;
  end;

  // Authentification OK, traiter normalement
  inherited HandleRequest(Sender, ARequest, AResponse);
end;

end.
```

## Pattern 2 : Rate Limiting

Le rate limiting limite le nombre de requêtes qu'un client peut faire dans un temps donné. C'est comme un videur qui limite l'entrée dans un club.

### 1. Rate Limiter

```pascal
unit APIGateway.RateLimit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, DateUtils;

type
  // Information de rate limit par client
  TRateLimitInfo = record
    RequestCount: Integer;
    WindowStart: TDateTime;
  end;

  // Gestionnaire de rate limiting
  TRateLimiter = class
  private
    FLimits: TDictionary<string, TRateLimitInfo>; // ClientId -> Info
    FMaxRequests: Integer;     // Nombre max de requêtes
    FWindowSeconds: Integer;   // Fenêtre de temps en secondes

    procedure ResetWindowIfNeeded(const AClientId: string);
  public
    constructor Create(AMaxRequests, AWindowSeconds: Integer);
    destructor Destroy; override;

    function IsAllowed(const AClientId: string): Boolean;
    function GetRemainingRequests(const AClientId: string): Integer;
  end;

implementation

// TRateLimiter

constructor TRateLimiter.Create(AMaxRequests, AWindowSeconds: Integer);  
begin
  inherited Create;
  FMaxRequests := AMaxRequests;
  FWindowSeconds := AWindowSeconds;
  FLimits := TDictionary<string, TRateLimitInfo>.Create;

  WriteLn(Format('[RateLimit] Limites: %d requêtes / %d secondes',
    [FMaxRequests, FWindowSeconds]));
end;

destructor TRateLimiter.Destroy;  
begin
  FLimits.Free;
  inherited;
end;

procedure TRateLimiter.ResetWindowIfNeeded(const AClientId: string);  
var
  Info: TRateLimitInfo;
  ElapsedSeconds: Int64;
begin
  if not FLimits.TryGetValue(AClientId, Info) then
  begin
    // Nouveau client
    Info.RequestCount := 0;
    Info.WindowStart := Now;
    FLimits.Add(AClientId, Info);
    Exit;
  end;

  // Vérifier si la fenêtre est expirée
  ElapsedSeconds := SecondsBetween(Now, Info.WindowStart);

  if ElapsedSeconds >= FWindowSeconds then
  begin
    // Nouvelle fenêtre
    Info.RequestCount := 0;
    Info.WindowStart := Now;
    FLimits.AddOrSetValue(AClientId, Info);
  end;
end;

function TRateLimiter.IsAllowed(const AClientId: string): Boolean;  
var
  Info: TRateLimitInfo;
begin
  ResetWindowIfNeeded(AClientId);

  Info := FLimits[AClientId];

  if Info.RequestCount >= FMaxRequests then
  begin
    WriteLn(Format('[RateLimit] ✗ Limite atteinte pour: %s', [AClientId]));
    Result := False;
  end
  else
  begin
    Inc(Info.RequestCount);
    FLimits.AddOrSetValue(AClientId, Info);

    WriteLn(Format('[RateLimit] ✓ Requête %d/%d pour: %s',
      [Info.RequestCount, FMaxRequests, AClientId]));

    Result := True;
  end;
end;

function TRateLimiter.GetRemainingRequests(const AClientId: string): Integer;  
var
  Info: TRateLimitInfo;
begin
  ResetWindowIfNeeded(AClientId);

  if FLimits.TryGetValue(AClientId, Info) then
    Result := FMaxRequests - Info.RequestCount
  else
    Result := FMaxRequests;
end;

end.
```

### 2. Gateway avec rate limiting

```pascal
unit APIGateway.RateLimited;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpserver, httpdefs,
  APIGateway.Secure, APIGateway.RateLimit;

type
  // Gateway avec rate limiting
  TRateLimitedGateway = class(TSecureAPIGateway)
  private
    FRateLimiter: TRateLimiter;

    function GetClientId(ARequest: TFPHTTPConnectionRequest): string;
  protected
    procedure HandleRequest(Sender: TObject;
      var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse); override;
  public
    constructor Create(APort: Integer; const ASecretKey: string;
      AMaxRequests, AWindowSeconds: Integer);
    destructor Destroy; override;
  end;

implementation

// TRateLimitedGateway

constructor TRateLimitedGateway.Create(APort: Integer; const ASecretKey: string;
  AMaxRequests, AWindowSeconds: Integer);
begin
  inherited Create(APort, ASecretKey);
  FRateLimiter := TRateLimiter.Create(AMaxRequests, AWindowSeconds);

  WriteLn('[RateLimitedGateway] Gateway avec rate limiting créée');
end;

destructor TRateLimitedGateway.Destroy;  
begin
  FRateLimiter.Free;
  inherited;
end;

function TRateLimitedGateway.GetClientId(
  ARequest: TFPHTTPConnectionRequest): string;
var
  AuthHeader, Token: string;
begin
  // Utiliser le user ID du token si authentifié
  AuthHeader := ARequest.GetCustomHeader('Authorization');

  if AuthHeader <> '' then
  begin
    Token := AuthManager.ExtractToken(AuthHeader);
    Result := AuthManager.GetUserFromToken(Token);
  end
  else
  begin
    // Sinon, utiliser l'IP du client
    Result := ARequest.RemoteAddress;
  end;
end;

procedure TRateLimitedGateway.HandleRequest(Sender: TObject;
  var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
var
  ClientId: string;
  Remaining: Integer;
begin
  ClientId := GetClientId(ARequest);

  // Vérifier le rate limit
  if not FRateLimiter.IsAllowed(ClientId) then
  begin
    Remaining := FRateLimiter.GetRemainingRequests(ClientId);

    AResponse.Code := 429; // Too Many Requests
    AResponse.Content := Format(
      '{"error": "Trop de requêtes", "remaining": %d}', [Remaining]);
    AResponse.ContentType := 'application/json';

    // Headers de rate limit
    AResponse.SetCustomHeader('X-RateLimit-Remaining', IntToStr(Remaining));

    WriteLn('[RateLimitedGateway] ✗ Requête rejetée - Rate limit dépassé');
    Exit;
  end;

  // Rate limit OK, continuer
  inherited HandleRequest(Sender, ARequest, AResponse);
end;

end.
```

## Pattern 3 : Response Aggregation

L'agrégation combine les réponses de plusieurs services en une seule réponse pour le client.

### 1. Agrégateur de réponses

```pascal
unit APIGateway.Aggregator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, fphttpclient, Generics.Collections;

type
  // Requête vers un service
  TServiceRequest = record
    ServiceName: string;
    URL: string;
    Method: string;
  end;

  // Réponse d'un service
  TServiceResponse = record
    ServiceName: string;
    Success: Boolean;
    Content: string;
    ErrorMessage: string;
  end;

  // Agrégateur
  TResponseAggregator = class
  private
    function CallService(const ARequest: TServiceRequest): TServiceResponse;
  public
    function AggregateResponses(const ARequests: array of TServiceRequest): TJSONObject;
  end;

implementation

// TResponseAggregator

function TResponseAggregator.CallService(
  const ARequest: TServiceRequest): TServiceResponse;
var
  Client: TFPHTTPClient;
begin
  Result.ServiceName := ARequest.ServiceName;
  Result.Success := False;

  Client := TFPHTTPClient.Create(nil);
  try
    try
      WriteLn(Format('[Aggregator] Appel: %s -> %s',
        [ARequest.ServiceName, ARequest.URL]));

      if ARequest.Method = 'GET' then
        Result.Content := Client.Get(ARequest.URL)
      else if ARequest.Method = 'POST' then
        Result.Content := Client.FormPost(ARequest.URL, '')
      else
        Result.Content := '{}';

      Result.Success := True;
      WriteLn(Format('[Aggregator] ✓ Réponse de %s', [ARequest.ServiceName]));

    except
      on E: Exception do
      begin
        Result.Success := False;
        Result.ErrorMessage := E.Message;
        WriteLn(Format('[Aggregator] ✗ Erreur %s: %s',
          [ARequest.ServiceName, E.Message]));
      end;
    end;

  finally
    Client.Free;
  end;
end;

function TResponseAggregator.AggregateResponses(
  const ARequests: array of TServiceRequest): TJSONObject;
var
  Request: TServiceRequest;
  Response: TServiceResponse;
  Responses: TList<TServiceResponse>;
  ServiceData: TJSONObject;
  Parser: TJSONParser;
  i: Integer;
begin
  Result := TJSONObject.Create;
  Responses := TList<TServiceResponse>.Create;
  try
    WriteLn('[Aggregator] Agrégation de', Length(ARequests), 'services');

    // Appeler tous les services
    for Request in ARequests do
    begin
      Response := CallService(Request);
      Responses.Add(Response);
    end;

    // Construire la réponse agrégée
    for i := 0 to Responses.Count - 1 do
    begin
      Response := Responses[i];

      if Response.Success then
      begin
        // Parser le JSON de la réponse
        try
          Parser := TJSONParser.Create(Response.Content, [joUTF8]);
          try
            ServiceData := Parser.Parse as TJSONObject;
            Result.Add(Response.ServiceName, ServiceData);
          finally
            Parser.Free;
          end;

        except
          on E: Exception do
          begin
            ServiceData := TJSONObject.Create;
            ServiceData.Add('error', 'Réponse invalide');
            Result.Add(Response.ServiceName, ServiceData);
          end;
        end;
      end
      else
      begin
        // Erreur
        ServiceData := TJSONObject.Create;
        ServiceData.Add('error', Response.ErrorMessage);
        Result.Add(Response.ServiceName, ServiceData);
      end;
    end;

    WriteLn('[Aggregator] ✓ Agrégation terminée');

  finally
    Responses.Free;
  end;
end;

end.
```

### 2. Exemple d'utilisation de l'agrégateur

```pascal
program AggregatorDemo;

{$mode objfpc}{$H+}

uses
  SysUtils, fpjson,
  APIGateway.Aggregator;

var
  Aggregator: TResponseAggregator;
  Requests: array[0..2] of TServiceRequest;
  Result: TJSONObject;

begin
  WriteLn('═══════════════════════════════════════════');
  WriteLn('  Démonstration Agrégation de Réponses    ');
  WriteLn('═══════════════════════════════════════════');
  WriteLn;

  Aggregator := TResponseAggregator.Create;
  try
    // Configurer les requêtes vers différents services
    Requests[0].ServiceName := 'products';
    Requests[0].URL := 'http://localhost:8001/products/123';
    Requests[0].Method := 'GET';

    Requests[1].ServiceName := 'reviews';
    Requests[1].URL := 'http://localhost:8002/reviews/product/123';
    Requests[1].Method := 'GET';

    Requests[2].ServiceName := 'inventory';
    Requests[2].URL := 'http://localhost:8003/inventory/123';
    Requests[2].Method := 'GET';

    // Agréger les réponses
    Result := Aggregator.AggregateResponses(Requests);
    try
      WriteLn;
      WriteLn('Réponse agrégée:');
      WriteLn(Result.FormatJSON);

    finally
      Result.Free;
    end;

  finally
    Aggregator.Free;
  end;

  WriteLn;
  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
```

**Exemple de réponse agrégée :**

```json
{
  "products": {
    "id": "123",
    "name": "Laptop HP",
    "price": 899.99
  },
  "reviews": {
    "average": 4.5,
    "count": 127,
    "recent": [...]
  },
  "inventory": {
    "available": 15,
    "warehouse": "Paris"
  }
}
```

## Pattern 4 : Circuit Breaker

Le Circuit Breaker empêche d'appeler un service défaillant de manière répétée. C'est comme un disjoncteur électrique qui coupe le courant en cas de problème.

### 1. États du Circuit Breaker

```
    CLOSED                    OPEN                    HALF-OPEN
(Service OK)            (Service KO)           (Test de récupération)
     │                        │                         │
     │ Trop d'erreurs        │ Timeout dépassé        │ Succès
     └──────────────────────►│◄────────────────────────┘
                              │                         │
                              └─────────────────────────┘
                                     Échec
```

**CLOSED (Fermé)** : Le trafic passe normalement. Les erreurs sont comptées.

**OPEN (Ouvert)** : Le circuit est ouvert, les requêtes échouent immédiatement sans appeler le service.

**HALF-OPEN (Semi-ouvert)** : Période de test, quelques requêtes passent pour vérifier si le service est rétabli.

### 2. Implémentation du Circuit Breaker

```pascal
unit APIGateway.CircuitBreaker;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils;

type
  // État du circuit breaker
  TCircuitState = (csClose, csOpen, csHalfOpen);

  // Circuit Breaker
  TCircuitBreaker = class
  private
    FServiceName: string;
    FState: TCircuitState;

    // Configuration
    FFailureThreshold: Integer;    // Nombre d'échecs avant ouverture
    FTimeoutSeconds: Integer;      // Temps avant half-open
    FSuccessThreshold: Integer;    // Succès nécessaires en half-open

    // Compteurs
    FFailureCount: Integer;
    FSuccessCount: Integer;
    FLastFailureTime: TDateTime;

    procedure RecordSuccess;
    procedure RecordFailure;
    function ShouldAttemptReset: Boolean;
  public
    constructor Create(const AServiceName: string;
      AFailureThreshold: Integer = 5;
      ATimeoutSeconds: Integer = 60;
      ASuccessThreshold: Integer = 2);

    function IsCallAllowed: Boolean;
    procedure OnCallSuccess;
    procedure OnCallFailure;

    function GetStateString: string;

    property ServiceName: string read FServiceName;
    property State: TCircuitState read FState;
  end;

implementation

// TCircuitBreaker

constructor TCircuitBreaker.Create(const AServiceName: string;
  AFailureThreshold, ATimeoutSeconds, ASuccessThreshold: Integer);
begin
  inherited Create;
  FServiceName := AServiceName;
  FState := csClose;

  FFailureThreshold := AFailureThreshold;
  FTimeoutSeconds := ATimeoutSeconds;
  FSuccessThreshold := ASuccessThreshold;

  FFailureCount := 0;
  FSuccessCount := 0;

  WriteLn(Format('[CircuitBreaker] Créé pour %s (seuil: %d échecs, timeout: %ds)',
    [AServiceName, AFailureThreshold, ATimeoutSeconds]));
end;

function TCircuitBreaker.GetStateString: string;  
begin
  case FState of
    csClose: Result := 'CLOSED';
    csOpen: Result := 'OPEN';
    csHalfOpen: Result := 'HALF-OPEN';
  end;
end;

function TCircuitBreaker.ShouldAttemptReset: Boolean;  
var
  ElapsedSeconds: Int64;
begin
  if FState <> csOpen then
  begin
    Result := False;
    Exit;
  end;

  ElapsedSeconds := SecondsBetween(Now, FLastFailureTime);
  Result := ElapsedSeconds >= FTimeoutSeconds;
end;

function TCircuitBreaker.IsCallAllowed: Boolean;  
begin
  case FState of
    csClose:
    begin
      // Circuit fermé : tout passe
      Result := True;
    end;

    csOpen:
    begin
      // Circuit ouvert : vérifier si on peut tester
      if ShouldAttemptReset then
      begin
        WriteLn(Format('[CircuitBreaker] %s: Passage en HALF-OPEN', [FServiceName]));
        FState := csHalfOpen;
        FSuccessCount := 0;
        Result := True;
      end
      else
      begin
        WriteLn(Format('[CircuitBreaker] %s: ✗ Appel bloqué (circuit OPEN)',
          [FServiceName]));
        Result := False;
      end;
    end;

    csHalfOpen:
    begin
      // Half-open : autoriser pour tester
      Result := True;
    end;
  end;
end;

procedure TCircuitBreaker.RecordSuccess;  
begin
  FFailureCount := 0;

  case FState of
    csClose:
    begin
      // Rien à faire, tout va bien
    end;

    csHalfOpen:
    begin
      Inc(FSuccessCount);

      WriteLn(Format('[CircuitBreaker] %s: Succès %d/%d en HALF-OPEN',
        [FServiceName, FSuccessCount, FSuccessThreshold]));

      if FSuccessCount >= FSuccessThreshold then
      begin
        WriteLn(Format('[CircuitBreaker] %s: ✓ Passage en CLOSED', [FServiceName]));
        FState := csClose;
        FSuccessCount := 0;
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

      WriteLn(Format('[CircuitBreaker] %s: Échec %d/%d',
        [FServiceName, FFailureCount, FFailureThreshold]));

      if FFailureCount >= FFailureThreshold then
      begin
        WriteLn(Format('[CircuitBreaker] %s: ✗ Passage en OPEN', [FServiceName]));
        FState := csOpen;
      end;
    end;

    csHalfOpen:
    begin
      WriteLn(Format('[CircuitBreaker] %s: ✗ Échec en HALF-OPEN, retour en OPEN',
        [FServiceName]));
      FState := csOpen;
      FSuccessCount := 0;
    end;
  end;
end;

procedure TCircuitBreaker.OnCallSuccess;  
begin
  WriteLn(Format('[CircuitBreaker] %s: ✓ Appel réussi', [FServiceName]));
  RecordSuccess;
end;

procedure TCircuitBreaker.OnCallFailure;  
begin
  WriteLn(Format('[CircuitBreaker] %s: ✗ Appel échoué', [FServiceName]));
  RecordFailure;
end;

end.
```

### 3. Gateway avec Circuit Breaker

```pascal
unit APIGateway.WithCircuitBreaker;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, fphttpserver, httpdefs,
  APIGateway.Core, APIGateway.CircuitBreaker;

type
  // Gateway avec circuit breaker
  TCircuitBreakerGateway = class(TAPIGateway)
  private
    FCircuitBreakers: TDictionary<string, TCircuitBreaker>;

    function GetOrCreateCircuitBreaker(const AServiceName: string): TCircuitBreaker;
  protected
    function ForwardRequest(const ARoute: TRouteConfig;
      ARequest: TFPHTTPConnectionRequest): string; override;
  public
    constructor Create(APort: Integer);
    destructor Destroy; override;
  end;

implementation

uses
  fphttpclient;

// TCircuitBreakerGateway

constructor TCircuitBreakerGateway.Create(APort: Integer);  
begin
  inherited Create(APort);
  FCircuitBreakers := TDictionary<string, TCircuitBreaker>.Create;

  WriteLn('[CBGateway] Gateway avec Circuit Breaker créée');
end;

destructor TCircuitBreakerGateway.Destroy;  
var
  CB: TCircuitBreaker;
begin
  for CB in FCircuitBreakers.Values do
    CB.Free;
  FCircuitBreakers.Free;

  inherited;
end;

function TCircuitBreakerGateway.GetOrCreateCircuitBreaker(
  const AServiceName: string): TCircuitBreaker;
begin
  if not FCircuitBreakers.TryGetValue(AServiceName, Result) then
  begin
    Result := TCircuitBreaker.Create(AServiceName);
    FCircuitBreakers.Add(AServiceName, Result);
  end;
end;

function TCircuitBreakerGateway.ForwardRequest(const ARoute: TRouteConfig;
  ARequest: TFPHTTPConnectionRequest): string;
var
  CircuitBreaker: TCircuitBreaker;
  Client: TFPHTTPClient;
  TargetURL: string;
begin
  CircuitBreaker := GetOrCreateCircuitBreaker(ARoute.TargetService);

  // Vérifier si l'appel est autorisé
  if not CircuitBreaker.IsCallAllowed then
  begin
    Result := Format('{"error": "Service %s temporairement indisponible (Circuit %s)"}',
      [ARoute.TargetService, CircuitBreaker.GetStateString]);
    Exit;
  end;

  // Tenter l'appel
  Client := TFPHTTPClient.Create(nil);
  try
    TargetURL := ARoute.TargetService + ARoute.TargetPath;

    try
      WriteLn(Format('[CBGateway] Appel: %s (Circuit: %s)',
        [TargetURL, CircuitBreaker.GetStateString]));

      if ARoute.Method = 'GET' then
        Result := Client.Get(TargetURL)
      else if ARoute.Method = 'POST' then
        Result := Client.FormPost(TargetURL, ARequest.Content)
      else if ARoute.Method = 'PUT' then
      begin
        Client.RequestBody := TStringStream.Create(ARequest.Content);
        Result := Client.Put(TargetURL);
      end
      else if ARoute.Method = 'DELETE' then
        Result := Client.Delete(TargetURL)
      else
        Result := '{"error": "Méthode non supportée"}';

      // Succès
      CircuitBreaker.OnCallSuccess;

    except
      on E: Exception do
      begin
        // Échec
        CircuitBreaker.OnCallFailure;
        Result := Format('{"error": "%s"}', [E.Message]);
      end;
    end;

  finally
    Client.Free;
  end;
end;

end.
```

## Pattern 5 : Request/Response Transformation

La Gateway peut transformer les requêtes et réponses pour adapter les formats entre clients et services.

### 1. Transformateur de requêtes

```pascal
unit APIGateway.Transformer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson;

type
  // Interface de transformation
  IRequestTransformer = interface
    ['{A1B2C3D4-E5F6-7890-ABCD-EF1234567890}']
    function TransformRequest(const AContent: string): string;
  end;

  IResponseTransformer = interface
    ['{B2C3D4E5-F6A7-8901-BCDE-F12345678901}']
    function TransformResponse(const AContent: string): string;
  end;

  // Transformation XML vers JSON
  TXMLToJSONTransformer = class(TInterfacedObject, IRequestTransformer)
  public
    function TransformRequest(const AContent: string): string;
  end;

  // Transformation JSON vers XML
  TJSONToXMLTransformer = class(TInterfacedObject, IResponseTransformer)
  public
    function TransformResponse(const AContent: string): string;
  end;

  // Enrichissement de requête
  TRequestEnricher = class(TInterfacedObject, IRequestTransformer)
  private
    FAdditionalData: TJSONObject;
  public
    constructor Create(AAdditionalData: TJSONObject);
    destructor Destroy; override;

    function TransformRequest(const AContent: string): string;
  end;

  // Filtrage de réponse
  TResponseFilter = class(TInterfacedObject, IResponseTransformer)
  private
    FFieldsToKeep: array of string;
  public
    constructor Create(const AFieldsToKeep: array of string);

    function TransformResponse(const AContent: string): string;
  end;

implementation

uses
  jsonparser;

// TXMLToJSONTransformer

function TXMLToJSONTransformer.TransformRequest(const AContent: string): string;  
begin
  // Implémentation simplifiée
  // En production, utiliser une vraie bibliothèque XML
  WriteLn('[Transformer] Conversion XML → JSON');

  // Simulation
  Result := '{"converted": "from_xml"}';
end;

// TJSONToXMLTransformer

function TJSONToXMLTransformer.TransformResponse(const AContent: string): string;  
begin
  WriteLn('[Transformer] Conversion JSON → XML');

  // Simulation
  Result := '<response><converted>from_json</converted></response>';
end;

// TRequestEnricher

constructor TRequestEnricher.Create(AAdditionalData: TJSONObject);  
begin
  inherited Create;
  FAdditionalData := AAdditionalData;
end;

destructor TRequestEnricher.Destroy;  
begin
  FAdditionalData.Free;
  inherited;
end;

function TRequestEnricher.TransformRequest(const AContent: string): string;  
var
  Parser: TJSONParser;
  RequestJSON, EnrichedJSON: TJSONObject;
  Key: string;
begin
  WriteLn('[Transformer] Enrichissement de la requête');

  try
    // Parser le JSON original
    Parser := TJSONParser.Create(AContent, [joUTF8]);
    try
      RequestJSON := Parser.Parse as TJSONObject;
      try
        // Ajouter les données supplémentaires
        for Key in FAdditionalData.Keys do
        begin
          RequestJSON.Add(Key, FAdditionalData.Get(Key, ''));
          WriteLn(Format('[Transformer] Ajout: %s', [Key]));
        end;

        Result := RequestJSON.AsJSON;

      finally
        RequestJSON.Free;
      end;
    finally
      Parser.Free;
    end;

  except
    on E: Exception do
    begin
      WriteLn(Format('[Transformer] Erreur: %s', [E.Message]));
      Result := AContent; // Retourner l'original en cas d'erreur
    end;
  end;
end;

// TResponseFilter

constructor TResponseFilter.Create(const AFieldsToKeep: array of string);  
var
  i: Integer;
begin
  inherited Create;
  SetLength(FFieldsToKeep, Length(AFieldsToKeep));

  for i := 0 to High(AFieldsToKeep) do
    FFieldsToKeep[i] := AFieldsToKeep[i];
end;

function TResponseFilter.TransformResponse(const AContent: string): string;  
var
  Parser: TJSONParser;
  OriginalJSON, FilteredJSON: TJSONObject;
  Field: string;
begin
  WriteLn('[Transformer] Filtrage de la réponse');

  try
    Parser := TJSONParser.Create(AContent, [joUTF8]);
    try
      OriginalJSON := Parser.Parse as TJSONObject;
      try
        FilteredJSON := TJSONObject.Create;
        try
          // Copier uniquement les champs autorisés
          for Field in FFieldsToKeep do
          begin
            if OriginalJSON.Find(Field) <> nil then
            begin
              FilteredJSON.Add(Field, OriginalJSON.Get(Field, ''));
              WriteLn(Format('[Transformer] Conservé: %s', [Field]));
            end;
          end;

          Result := FilteredJSON.AsJSON;

        finally
          FilteredJSON.Free;
        end;
      finally
        OriginalJSON.Free;
      end;
    finally
      Parser.Free;
    end;

  except
    on E: Exception do
    begin
      WriteLn(Format('[Transformer] Erreur: %s', [E.Message]));
      Result := AContent;
    end;
  end;
end;

end.
```

### 2. Exemple d'utilisation des transformateurs

```pascal
program TransformerDemo;

{$mode objfpc}{$H+}

uses
  SysUtils, fpjson,
  APIGateway.Transformer;

procedure DemoEnrichissement;  
var
  Enricher: TRequestEnricher;
  AdditionalData: TJSONObject;
  OriginalRequest, EnrichedRequest: string;
begin
  WriteLn('═══════════════════════════════════════════');
  WriteLn('  Démonstration Enrichissement             ');
  WriteLn('═══════════════════════════════════════════');
  WriteLn;

  // Données à ajouter
  AdditionalData := TJSONObject.Create;
  AdditionalData.Add('timestamp', DateTimeToStr(Now));
  AdditionalData.Add('gateway_version', '1.0');
  AdditionalData.Add('client_ip', '192.168.1.100');

  Enricher := TRequestEnricher.Create(AdditionalData);
  try
    OriginalRequest := '{"user_id": "123", "action": "purchase"}';

    WriteLn('Requête originale:');
    WriteLn(OriginalRequest);
    WriteLn;

    EnrichedRequest := Enricher.TransformRequest(OriginalRequest);

    WriteLn('Requête enrichie:');
    WriteLn(EnrichedRequest);

  finally
    Enricher.Free;
  end;
end;

procedure DemoFiltrage;  
var
  Filter: TResponseFilter;
  OriginalResponse, FilteredResponse: string;
  FieldsToKeep: array[0..2] of string;
begin
  WriteLn;
  WriteLn('═══════════════════════════════════════════');
  WriteLn('  Démonstration Filtrage                   ');
  WriteLn('═══════════════════════════════════════════');
  WriteLn;

  FieldsToKeep[0] := 'id';
  FieldsToKeep[1] := 'name';
  FieldsToKeep[2] := 'price';

  Filter := TResponseFilter.Create(FieldsToKeep);
  try
    OriginalResponse := '{"id": "123", "name": "Laptop", "price": 899.99, ' +
                        '"internal_cost": 600.00, "supplier_id": "SUP-456"}';

    WriteLn('Réponse originale (avec données internes):');
    WriteLn(OriginalResponse);
    WriteLn;

    FilteredResponse := Filter.TransformResponse(OriginalResponse);

    WriteLn('Réponse filtrée (données publiques uniquement):');
    WriteLn(FilteredResponse);

  finally
    Filter.Free;
  end;
end;

begin
  DemoEnrichissement;
  WriteLn;
  WriteLn('Appuyez sur Entrée pour continuer...');
  ReadLn;

  DemoFiltrage;
  WriteLn;
  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
```

## Pattern 6 : Caching

Le cache permet de stocker temporairement les réponses pour améliorer les performances.

### 1. Cache simple

```pascal
unit APIGateway.Cache;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, DateUtils;

type
  // Entrée de cache
  TCacheEntry = record
    Content: string;
    Timestamp: TDateTime;
    Hits: Integer;
  end;

  // Gestionnaire de cache
  TCacheManager = class
  private
    FCache: TDictionary<string, TCacheEntry>;
    FTTLSeconds: Integer;  // Time To Live
    FMaxSize: Integer;

    function IsExpired(const AEntry: TCacheEntry): Boolean;
    procedure CleanupExpired;
    procedure EnforceMaxSize;
    function GenerateKey(const AMethod, APath: string): string;
  public
    constructor Create(ATTLSeconds: Integer = 300; AMaxSize: Integer = 100);
    destructor Destroy; override;

    function Get(const AMethod, APath: string; out AContent: string): Boolean;
    procedure Put(const AMethod, APath, AContent: string);
    procedure Invalidate(const AMethod, APath: string);
    procedure Clear;

    function GetStats: string;
  end;

implementation

// TCacheManager

constructor TCacheManager.Create(ATTLSeconds, AMaxSize: Integer);  
begin
  inherited Create;
  FCache := TDictionary<string, TCacheEntry>.Create;
  FTTLSeconds := ATTLSeconds;
  FMaxSize := AMaxSize;

  WriteLn(Format('[Cache] Initialisé (TTL: %ds, Max: %d entrées)',
    [FTTLSeconds, FMaxSize]));
end;

destructor TCacheManager.Destroy;  
begin
  FCache.Free;
  inherited;
end;

function TCacheManager.GenerateKey(const AMethod, APath: string): string;  
begin
  Result := AMethod + ':' + APath;
end;

function TCacheManager.IsExpired(const AEntry: TCacheEntry): Boolean;  
var
  ElapsedSeconds: Int64;
begin
  ElapsedSeconds := SecondsBetween(Now, AEntry.Timestamp);
  Result := ElapsedSeconds > FTTLSeconds;
end;

procedure TCacheManager.CleanupExpired;  
var
  Key: string;
  Entry: TCacheEntry;
  KeysToRemove: TList<string>;
begin
  KeysToRemove := TList<string>.Create;
  try
    // Identifier les entrées expirées
    for Key in FCache.Keys do
    begin
      Entry := FCache[Key];
      if IsExpired(Entry) then
        KeysToRemove.Add(Key);
    end;

    // Supprimer
    for Key in KeysToRemove do
    begin
      FCache.Remove(Key);
      WriteLn(Format('[Cache] Entrée expirée supprimée: %s', [Key]));
    end;

  finally
    KeysToRemove.Free;
  end;
end;

procedure TCacheManager.EnforceMaxSize;  
var
  Key: string;
  OldestKey: string;
  OldestTime: TDateTime;
  Entry: TCacheEntry;
begin
  while FCache.Count > FMaxSize do
  begin
    // Trouver l'entrée la plus ancienne
    OldestKey := '';
    OldestTime := Now;

    for Key in FCache.Keys do
    begin
      Entry := FCache[Key];
      if Entry.Timestamp < OldestTime then
      begin
        OldestTime := Entry.Timestamp;
        OldestKey := Key;
      end;
    end;

    if OldestKey <> '' then
    begin
      FCache.Remove(OldestKey);
      WriteLn(Format('[Cache] Entrée LRU supprimée: %s', [OldestKey]));
    end;
  end;
end;

function TCacheManager.Get(const AMethod, APath: string;
  out AContent: string): Boolean;
var
  Key: string;
  Entry: TCacheEntry;
begin
  CleanupExpired;

  Key := GenerateKey(AMethod, APath);
  Result := FCache.TryGetValue(Key, Entry);

  if Result then
  begin
    if IsExpired(Entry) then
    begin
      FCache.Remove(Key);
      Result := False;
      WriteLn(Format('[Cache] ✗ MISS (expiré): %s', [Key]));
    end
    else
    begin
      // Hit!
      Inc(Entry.Hits);
      FCache.AddOrSetValue(Key, Entry);
      AContent := Entry.Content;
      WriteLn(Format('[Cache] ✓ HIT: %s (hits: %d)', [Key, Entry.Hits]));
    end;
  end
  else
  begin
    WriteLn(Format('[Cache] ✗ MISS: %s', [Key]));
  end;
end;

procedure TCacheManager.Put(const AMethod, APath, AContent: string);  
var
  Key: string;
  Entry: TCacheEntry;
begin
  Key := GenerateKey(AMethod, APath);

  Entry.Content := AContent;
  Entry.Timestamp := Now;
  Entry.Hits := 0;

  FCache.AddOrSetValue(Key, Entry);
  WriteLn(Format('[Cache] ✓ Mise en cache: %s', [Key]));

  EnforceMaxSize;
end;

procedure TCacheManager.Invalidate(const AMethod, APath: string);  
var
  Key: string;
begin
  Key := GenerateKey(AMethod, APath);

  if FCache.Remove(Key) then
    WriteLn(Format('[Cache] ✓ Invalidé: %s', [Key]))
  else
    WriteLn(Format('[Cache] Entrée non trouvée: %s', [Key]));
end;

procedure TCacheManager.Clear;  
begin
  FCache.Clear;
  WriteLn('[Cache] ✓ Cache vidé');
end;

function TCacheManager.GetStats: string;  
var
  TotalHits: Integer;
  Entry: TCacheEntry;
begin
  TotalHits := 0;

  for Entry in FCache.Values do
    TotalHits := TotalHits + Entry.Hits;

  Result := Format('Entrées: %d, Hits totaux: %d', [FCache.Count, TotalHits]);
end;

end.
```

### 2. Gateway avec cache

```pascal
unit APIGateway.Cached;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpserver, httpdefs,
  APIGateway.Core, APIGateway.Cache;

type
  // Gateway avec cache
  TCachedGateway = class(TAPIGateway)
  private
    FCacheManager: TCacheManager;
    FCacheableMethods: array of string;

    function IsCacheable(const AMethod: string): Boolean;
  protected
    procedure HandleRequest(Sender: TObject;
      var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse); override;
  public
    constructor Create(APort: Integer; ACacheTTL: Integer = 300);
    destructor Destroy; override;

    procedure AddCacheableMethod(const AMethod: string);
  end;

implementation

// TCachedGateway

constructor TCachedGateway.Create(APort, ACacheTTL: Integer);  
begin
  inherited Create(APort);
  FCacheManager := TCacheManager.Create(ACacheTTL);
  SetLength(FCacheableMethods, 0);

  // GET est cacheable par défaut
  AddCacheableMethod('GET');

  WriteLn('[CachedGateway] Gateway avec cache créée');
end;

destructor TCachedGateway.Destroy;  
begin
  WriteLn('[CachedGateway] Stats finales: ' + FCacheManager.GetStats);
  FCacheManager.Free;
  inherited;
end;

procedure TCachedGateway.AddCacheableMethod(const AMethod: string);  
var
  Len: Integer;
begin
  Len := Length(FCacheableMethods);
  SetLength(FCacheableMethods, Len + 1);
  FCacheableMethods[Len] := AMethod;

  WriteLn(Format('[CachedGateway] Méthode cacheable: %s', [AMethod]));
end;

function TCachedGateway.IsCacheable(const AMethod: string): Boolean;  
var
  Method: string;
begin
  Result := False;

  for Method in FCacheableMethods do
  begin
    if Method = AMethod then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

procedure TCachedGateway.HandleRequest(Sender: TObject;
  var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
var
  CachedContent: string;
  RouteIndex: Integer;
  ResponseContent: string;
begin
  WriteLn;
  WriteLn(Format('[CachedGateway] Requête: %s %s',
    [ARequest.Method, ARequest.URI]));

  // Vérifier le cache si méthode cacheable
  if IsCacheable(ARequest.Method) then
  begin
    if FCacheManager.Get(ARequest.Method, ARequest.URI, CachedContent) then
    begin
      // Réponse depuis le cache
      AResponse.Code := 200;
      AResponse.Content := CachedContent;
      AResponse.ContentType := 'application/json';
      AResponse.SetCustomHeader('X-Cache', 'HIT');
      Exit;
    end;
  end;

  // Pas en cache, appeler le service
  RouteIndex := FindRoute(ARequest.URI, ARequest.Method);

  if RouteIndex = -1 then
  begin
    AResponse.Code := 404;
    AResponse.Content := '{"error": "Route non trouvée"}';
  end
  else
  begin
    try
      ResponseContent := ForwardRequest(FRoutes[RouteIndex], ARequest);

      AResponse.Code := 200;
      AResponse.Content := ResponseContent;
      AResponse.ContentType := 'application/json';
      AResponse.SetCustomHeader('X-Cache', 'MISS');

      // Mettre en cache si applicable
      if IsCacheable(ARequest.Method) then
        FCacheManager.Put(ARequest.Method, ARequest.URI, ResponseContent);

    except
      on E: Exception do
      begin
        AResponse.Code := 500;
        AResponse.Content := Format('{"error": "%s"}', [E.Message]);
      end;
    end;
  end;
end;

end.
```

## Pattern 7 : Service Discovery

Le Service Discovery permet à la Gateway de découvrir automatiquement les services disponibles et leurs adresses.

### 1. Registry de services

```pascal
unit APIGateway.ServiceRegistry;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections;

type
  // Information sur un service
  TServiceInfo = record
    ServiceName: string;
    Host: string;
    Port: Integer;
    HealthEndpoint: string;
    IsHealthy: Boolean;
    LastHealthCheck: TDateTime;
  end;

  // Registre de services
  TServiceRegistry = class
  private
    FServices: TDictionary<string, TList<TServiceInfo>>;
    FCurrentIndex: TDictionary<string, Integer>; // Pour load balancing

    function CheckServiceHealth(const AService: TServiceInfo): Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure RegisterService(const AServiceName, AHost: string;
      APort: Integer; const AHealthEndpoint: string = '/health');
    procedure UnregisterService(const AServiceName, AHost: string; APort: Integer);

    function GetService(const AServiceName: string): TServiceInfo;
    function GetAllServices(const AServiceName: string): TList<TServiceInfo>;

    procedure HealthCheck(const AServiceName: string);
    procedure HealthCheckAll;

    procedure PrintRegistry;
  end;

implementation

uses
  fphttpclient;

// TServiceRegistry

constructor TServiceRegistry.Create;  
begin
  inherited Create;
  FServices := TDictionary<string, TList<TServiceInfo>>.Create;
  FCurrentIndex := TDictionary<string, Integer>.Create;

  WriteLn('[ServiceRegistry] Registre de services créé');
end;

destructor TServiceRegistry.Destroy;  
var
  ServiceList: TList<TServiceInfo>;
begin
  for ServiceList in FServices.Values do
    ServiceList.Free;

  FServices.Free;
  FCurrentIndex.Free;
  inherited;
end;

procedure TServiceRegistry.RegisterService(const AServiceName, AHost: string;
  APort: Integer; const AHealthEndpoint: string);
var
  ServiceList: TList<TServiceInfo>;
  ServiceInfo: TServiceInfo;
begin
  // Créer la liste pour ce service si elle n'existe pas
  if not FServices.TryGetValue(AServiceName, ServiceList) then
  begin
    ServiceList := TList<TServiceInfo>.Create;
    FServices.Add(AServiceName, ServiceList);
    FCurrentIndex.Add(AServiceName, 0);
  end;

  // Créer l'info du service
  ServiceInfo.ServiceName := AServiceName;
  ServiceInfo.Host := AHost;
  ServiceInfo.Port := APort;
  ServiceInfo.HealthEndpoint := AHealthEndpoint;
  ServiceInfo.IsHealthy := True;
  ServiceInfo.LastHealthCheck := Now;

  ServiceList.Add(ServiceInfo);

  WriteLn(Format('[ServiceRegistry] ✓ Service enregistré: %s (%s:%d)',
    [AServiceName, AHost, APort]));
end;

procedure TServiceRegistry.UnregisterService(const AServiceName, AHost: string;
  APort: Integer);
var
  ServiceList: TList<TServiceInfo>;
  i: Integer;
  Service: TServiceInfo;
begin
  if FServices.TryGetValue(AServiceName, ServiceList) then
  begin
    for i := ServiceList.Count - 1 downto 0 do
    begin
      Service := ServiceList[i];
      if (Service.Host = AHost) and (Service.Port = APort) then
      begin
        ServiceList.Delete(i);
        WriteLn(Format('[ServiceRegistry] ✓ Service désenregistré: %s (%s:%d)',
          [AServiceName, AHost, APort]));
        Exit;
      end;
    end;
  end;
end;

function TServiceRegistry.CheckServiceHealth(const AService: TServiceInfo): Boolean;  
var
  Client: TFPHTTPClient;
  URL: string;
begin
  Result := False;
  Client := TFPHTTPClient.Create(nil);
  try
    try
      URL := Format('http://%s:%d%s',
        [AService.Host, AService.Port, AService.HealthEndpoint]);

      Client.Get(URL);

      // Si on arrive ici sans exception, le service est OK
      Result := True;

    except
      // Service inaccessible
      Result := False;
    end;

  finally
    Client.Free;
  end;
end;

function TServiceRegistry.GetService(const AServiceName: string): TServiceInfo;  
var
  ServiceList: TList<TServiceInfo>;
  CurrentIndex: Integer;
  Service: TServiceInfo;
  Attempts: Integer;
begin
  if not FServices.TryGetValue(AServiceName, ServiceList) then
    raise Exception.CreateFmt('Service non trouvé: %s', [AServiceName]);

  if ServiceList.Count = 0 then
    raise Exception.CreateFmt('Aucune instance disponible pour: %s', [AServiceName]);

  // Round-robin load balancing
  CurrentIndex := FCurrentIndex[AServiceName];
  Attempts := 0;

  repeat
    Service := ServiceList[CurrentIndex];

    // Incrémenter pour la prochaine fois
    CurrentIndex := (CurrentIndex + 1) mod ServiceList.Count;
    FCurrentIndex.AddOrSetValue(AServiceName, CurrentIndex);

    // Retourner si le service est sain
    if Service.IsHealthy then
    begin
      WriteLn(Format('[ServiceRegistry] Service sélectionné: %s:%d',
        [Service.Host, Service.Port]));
      Result := Service;
      Exit;
    end;

    Inc(Attempts);

  until Attempts >= ServiceList.Count;

  // Aucun service sain trouvé
  raise Exception.CreateFmt('Aucune instance saine pour: %s', [AServiceName]);
end;

function TServiceRegistry.GetAllServices(const AServiceName: string): TList<TServiceInfo>;  
var
  ServiceList: TList<TServiceInfo>;
begin
  Result := TList<TServiceInfo>.Create;

  if FServices.TryGetValue(AServiceName, ServiceList) then
  begin
    Result.AddRange(ServiceList);
  end;
end;

procedure TServiceRegistry.HealthCheck(const AServiceName: string);  
var
  ServiceList: TList<TServiceInfo>;
  i: Integer;
  Service: TServiceInfo;
  IsHealthy: Boolean;
begin
  if not FServices.TryGetValue(AServiceName, ServiceList) then
    Exit;

  WriteLn(Format('[ServiceRegistry] Health check: %s', [AServiceName]));

  for i := 0 to ServiceList.Count - 1 do
  begin
    Service := ServiceList[i];
    IsHealthy := CheckServiceHealth(Service);

    Service.IsHealthy := IsHealthy;
    Service.LastHealthCheck := Now;
    ServiceList[i] := Service;

    if IsHealthy then
      WriteLn(Format('  ✓ %s:%d OK', [Service.Host, Service.Port]))
    else
      WriteLn(Format('  ✗ %s:%d KO', [Service.Host, Service.Port]));
  end;
end;

procedure TServiceRegistry.HealthCheckAll;  
var
  ServiceName: string;
begin
  WriteLn('[ServiceRegistry] Health check global');

  for ServiceName in FServices.Keys do
    HealthCheck(ServiceName);
end;

procedure TServiceRegistry.PrintRegistry;  
var
  ServiceName: string;
  ServiceList: TList<TServiceInfo>;
  Service: TServiceInfo;
begin
  WriteLn;
  WriteLn('═══════════════════════════════════════════');
  WriteLn('  REGISTRE DES SERVICES                   ');
  WriteLn('═══════════════════════════════════════════');

  for ServiceName in FServices.Keys do
  begin
    ServiceList := FServices[ServiceName];
    WriteLn(Format('Service: %s (%d instances)', [ServiceName, ServiceList.Count]));

    for Service in ServiceList do
    begin
      WriteLn(Format('  - %s:%d [%s]',
        [Service.Host, Service.Port,
         BoolToStr(Service.IsHealthy, 'Sain', 'KO')]));
    end;

    WriteLn;
  end;

  WriteLn('═══════════════════════════════════════════');
end;

end.
```

### 2. Gateway avec Service Discovery

```pascal
unit APIGateway.WithDiscovery;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpserver, httpdefs,
  APIGateway.Core, APIGateway.ServiceRegistry;

type
  // Configuration de route avec discovery
  TDiscoveryRouteConfig = record
    Path: string;
    Method: string;
    ServiceName: string;  // Nom logique du service
    TargetPath: string;
  end;

  // Gateway avec service discovery
  TDiscoveryGateway = class(TAPIGateway)
  private
    FServiceRegistry: TServiceRegistry;
    FDiscoveryRoutes: array of TDiscoveryRouteConfig;

    function FindDiscoveryRoute(const APath, AMethod: string): Integer;
  protected
    procedure HandleRequest(Sender: TObject;
      var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse); override;
  public
    constructor Create(APort: Integer);
    destructor Destroy; override;

    procedure AddDiscoveryRoute(const APath, AMethod, AServiceName,
      ATargetPath: string);

    property ServiceRegistry: TServiceRegistry read FServiceRegistry;
  end;

implementation

uses
  fphttpclient;

// TDiscoveryGateway

constructor TDiscoveryGateway.Create(APort: Integer);  
begin
  inherited Create(APort);
  FServiceRegistry := TServiceRegistry.Create;
  SetLength(FDiscoveryRoutes, 0);

  WriteLn('[DiscoveryGateway] Gateway avec Service Discovery créée');
end;

destructor TDiscoveryGateway.Destroy;  
begin
  FServiceRegistry.Free;
  inherited;
end;

procedure TDiscoveryGateway.AddDiscoveryRoute(const APath, AMethod,
  AServiceName, ATargetPath: string);
var
  Route: TDiscoveryRouteConfig;
  Len: Integer;
begin
  Route.Path := APath;
  Route.Method := AMethod;
  Route.ServiceName := AServiceName;
  Route.TargetPath := ATargetPath;

  Len := Length(FDiscoveryRoutes);
  SetLength(FDiscoveryRoutes, Len + 1);
  FDiscoveryRoutes[Len] := Route;

  WriteLn(Format('[DiscoveryGateway] Route ajoutée: %s %s -> Service:%s%s',
    [AMethod, APath, AServiceName, ATargetPath]));
end;

function TDiscoveryGateway.FindDiscoveryRoute(const APath,
  AMethod: string): Integer;
var
  i: Integer;
begin
  Result := -1;

  for i := 0 to High(FDiscoveryRoutes) do
  begin
    if (FDiscoveryRoutes[i].Path = APath) and
       (FDiscoveryRoutes[i].Method = AMethod) then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

procedure TDiscoveryGateway.HandleRequest(Sender: TObject;
  var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
var
  RouteIndex: Integer;
  Route: TDiscoveryRouteConfig;
  ServiceInfo: TServiceInfo;
  Client: TFPHTTPClient;
  TargetURL: string;
  ResponseContent: string;
begin
  WriteLn;
  WriteLn(Format('[DiscoveryGateway] Requête: %s %s',
    [ARequest.Method, ARequest.URI]));

  RouteIndex := FindDiscoveryRoute(ARequest.URI, ARequest.Method);

  if RouteIndex = -1 then
  begin
    AResponse.Code := 404;
    AResponse.Content := '{"error": "Route non trouvée"}';
    Exit;
  end;

  Route := FDiscoveryRoutes[RouteIndex];

  try
    // Obtenir une instance du service via le registry
    ServiceInfo := FServiceRegistry.GetService(Route.ServiceName);

    // Construire l'URL complète
    TargetURL := Format('http://%s:%d%s',
      [ServiceInfo.Host, ServiceInfo.Port, Route.TargetPath]);

    WriteLn(Format('[DiscoveryGateway] Forward vers: %s', [TargetURL]));

    // Appeler le service
    Client := TFPHTTPClient.Create(nil);
    try
      if Route.Method = 'GET' then
        ResponseContent := Client.Get(TargetURL)
      else if Route.Method = 'POST' then
        ResponseContent := Client.FormPost(TargetURL, ARequest.Content)
      else
        ResponseContent := '{"error": "Méthode non supportée"}';

      AResponse.Code := 200;
      AResponse.Content := ResponseContent;
      AResponse.ContentType := 'application/json';

    finally
      Client.Free;
    end;

  except
    on E: Exception do
    begin
      AResponse.Code := 503; // Service Unavailable
      AResponse.Content := Format('{"error": "Service indisponible: %s"}',
        [E.Message]);
      WriteLn(Format('[DiscoveryGateway] ✗ Erreur: %s', [E.Message]));
    end;
  end;
end;

end.
```

### 3. Exemple complet avec Service Discovery

```pascal
program ServiceDiscoveryDemo;

{$mode objfpc}{$H+}

uses
  SysUtils,
  APIGateway.WithDiscovery;

var
  Gateway: TDiscoveryGateway;

begin
  WriteLn('═══════════════════════════════════════════');
  WriteLn('  Gateway avec Service Discovery          ');
  WriteLn('═══════════════════════════════════════════');
  WriteLn;

  Gateway := TDiscoveryGateway.Create(8080);
  try
    // Enregistrer les instances de services
    WriteLn('Enregistrement des services...');
    WriteLn;

    // Service Products - 3 instances
    Gateway.ServiceRegistry.RegisterService('products', 'localhost', 8001);
    Gateway.ServiceRegistry.RegisterService('products', 'localhost', 8002);
    Gateway.ServiceRegistry.RegisterService('products', 'localhost', 8003);

    // Service Orders - 2 instances
    Gateway.ServiceRegistry.RegisterService('orders', 'localhost', 9001);
    Gateway.ServiceRegistry.RegisterService('orders', 'localhost', 9002);

    // Service Users - 1 instance
    Gateway.ServiceRegistry.RegisterService('users', 'localhost', 7001);

    // Configurer les routes avec discovery
    Gateway.AddDiscoveryRoute('/api/products', 'GET', 'products', '/products');
    Gateway.AddDiscoveryRoute('/api/orders', 'GET', 'orders', '/orders');
    Gateway.AddDiscoveryRoute('/api/orders', 'POST', 'orders', '/orders');
    Gateway.AddDiscoveryRoute('/api/users', 'GET', 'users', '/users');

    // Afficher le registre
    Gateway.ServiceRegistry.PrintRegistry;

    // Health check initial
    WriteLn;
    WriteLn('Health check initial...');
    Gateway.ServiceRegistry.HealthCheckAll;

    // Démarrer
    Gateway.Start;

    WriteLn;
    WriteLn('Gateway actif avec load balancing automatique');
    WriteLn('Les requêtes seront distribuées entre les instances disponibles');
    WriteLn;
    WriteLn('Appuyez sur Entrée pour arrêter...');
    ReadLn;

  finally
    Gateway.Free;
  end;
end.
```

## Pattern 8 : Gateway Composé (API Composition)

Ce pattern permet de composer plusieurs appels de services en une seule réponse enrichie.

### 1. Compositeur d'API

```pascal
unit APIGateway.Composer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson,
  APIGateway.Aggregator;

type
  // Définition d'une composition
  TCompositionRule = record
    OutputField: string;        // Nom du champ dans la réponse finale
    ServiceName: string;        // Service à appeler
    ServicePath: string;        // Chemin du service
    DependsOn: string;          // Champ requis d'une réponse précédente
    PathParameter: string;      // Paramètre à injecter dans le path
  end;

  // Compositeur d'API
  TAPIComposer = class
  private
    FAggregator: TResponseAggregator;

    function ExtractValue(AJson: TJSONObject; const APath: string): string;
    function ReplaceParameter(const APath, AParam, AValue: string): string;
  public
    constructor Create;
    destructor Destroy; override;

    function Compose(const ARules: array of TCompositionRule;
      const AInitialParams: TJSONObject): TJSONObject;
  end;

implementation

uses
  jsonparser;

// TAPIComposer

constructor TAPIComposer.Create;  
begin
  inherited Create;
  FAggregator := TResponseAggregator.Create;
  WriteLn('[Composer] API Composer créé');
end;

destructor TAPIComposer.Destroy;  
begin
  FAggregator.Free;
  inherited;
end;

function TAPIComposer.ExtractValue(AJson: TJSONObject;
  const APath: string): string;
begin
  // Extraction simple (à améliorer avec JsonPath)
  Result := AJson.Get(APath, '');
end;

function TAPIComposer.ReplaceParameter(const APath, AParam,
  AValue: string): string;
begin
  Result := StringReplace(APath, '{' + AParam + '}', AValue, [rfReplaceAll]);
end;

function TAPIComposer.Compose(const ARules: array of TCompositionRule;
  const AInitialParams: TJSONObject): TJSONObject;
var
  Rule: TCompositionRule;
  Requests: array of TServiceRequest;
  IntermediateResults: TJSONObject;
  Parser: TJSONParser;
  ServiceResponse: TJSONObject;
  DependencyValue: string;
  ActualPath: string;
  i: Integer;
begin
  Result := TJSONObject.Create;
  IntermediateResults := TJSONObject.Create;
  try
    WriteLn('[Composer] Début composition...');

    // Ajouter les paramètres initiaux
    for i := 0 to AInitialParams.Count - 1 do
    begin
      IntermediateResults.Add(
        AInitialParams.Names[i],
        AInitialParams.Items[i].Clone);
    end;

    // Traiter chaque règle
    for Rule in ARules do
    begin
      WriteLn(Format('[Composer] Traitement: %s', [Rule.OutputField]));

      ActualPath := Rule.ServicePath;

      // Vérifier les dépendances
      if Rule.DependsOn <> '' then
      begin
        DependencyValue := ExtractValue(IntermediateResults, Rule.DependsOn);

        if DependencyValue = '' then
        begin
          WriteLn(Format('[Composer] ✗ Dépendance manquante: %s',
            [Rule.DependsOn]));
          Continue;
        end;

        // Remplacer le paramètre dans le path
        if Rule.PathParameter <> '' then
          ActualPath := ReplaceParameter(ActualPath, Rule.PathParameter,
            DependencyValue);
      end;

      // Préparer la requête
      SetLength(Requests, 1);
      Requests[0].ServiceName := Rule.ServiceName;
      Requests[0].URL := Rule.ServiceName + ActualPath;
      Requests[0].Method := 'GET';

      // Appeler le service
      ServiceResponse := FAggregator.AggregateResponses(Requests);
      try
        // Extraire la réponse du service
        if ServiceResponse.Find(Rule.ServiceName) <> nil then
        begin
          Result.Add(Rule.OutputField,
            ServiceResponse.Get(Rule.ServiceName, TJSONObject(nil)).Clone);

          // Stocker pour les dépendances futures
          IntermediateResults.Add(Rule.OutputField,
            ServiceResponse.Get(Rule.ServiceName, TJSONObject(nil)).Clone);

          WriteLn(Format('[Composer] ✓ %s ajouté', [Rule.OutputField]));
        end;

      finally
        ServiceResponse.Free;
      end;
    end;

    WriteLn('[Composer] ✓ Composition terminée');

  finally
    IntermediateResults.Free;
  end;
end;

end.
```

### 2. Exemple de composition

```pascal
program ComposerDemo;

{$mode objfpc}{$H+}

uses
  SysUtils, fpjson,
  APIGateway.Composer;

var
  Composer: TAPIComposer;
  Rules: array[0..2] of TCompositionRule;
  InitialParams: TJSONObject;
  Result: TJSONObject;

begin
  WriteLn('═══════════════════════════════════════════');
  WriteLn('  Démonstration API Composition            ');
  WriteLn('═══════════════════════════════════════════');
  WriteLn;

  Composer := TAPIComposer.Create;
  try
    // Paramètres initiaux
    InitialParams := TJSONObject.Create;
    InitialParams.Add('product_id', '123');

    // Règle 1: Obtenir les infos produit
    Rules[0].OutputField := 'product';
    Rules[0].ServiceName := 'http://localhost:8001';
    Rules[0].ServicePath := '/products/{id}';
    Rules[0].DependsOn := 'product_id';
    Rules[0].PathParameter := 'id';

    // Règle 2: Obtenir les reviews (dépend du produit)
    Rules[1].OutputField := 'reviews';
    Rules[1].ServiceName := 'http://localhost:8002';
    Rules[1].ServicePath := '/reviews/product/{id}';
    Rules[1].DependsOn := 'product_id';
    Rules[1].PathParameter := 'id';

    // Règle 3: Obtenir le stock (dépend du produit)
    Rules[2].OutputField := 'inventory';
    Rules[2].ServiceName := 'http://localhost:8003';
    Rules[2].ServicePath := '/inventory/{id}';
    Rules[2].DependsOn := 'product_id';
    Rules[2].PathParameter := 'id';

    // Composer la réponse
    Result := Composer.Compose(Rules, InitialParams);
    try
      WriteLn;
      WriteLn('Réponse composée:');
      WriteLn(Result.FormatJSON);

    finally
      Result.Free;
    end;

    InitialParams.Free;

  finally
    Composer.Free;
  end;

  WriteLn;
  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
```

**Exemple de réponse composée :**

```json
{
  "product": {
    "id": "123",
    "name": "Laptop HP",
    "price": 899.99,
    "category": "Electronics"
  },
  "reviews": {
    "average_rating": 4.5,
    "total_reviews": 127,
    "recent": [
      {"rating": 5, "comment": "Excellent!"},
      {"rating": 4, "comment": "Très bon rapport qualité/prix"}
    ]
  },
  "inventory": {
    "available_quantity": 15,
    "warehouse": "Paris",
    "next_restock": "2025-10-15"
  }
}
```

## Pattern 9 : Backends For Frontends (BFF)

Le pattern BFF consiste à créer une Gateway spécifique pour chaque type de client (mobile, web, etc.).

### 1. Structure BFF

```pascal
unit APIGateway.BFF;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson,
  APIGateway.Core, APIGateway.Transformer;

type
  // Type de client
  TClientType = (ctMobileApp, ctWebApp, ctThirdPartyAPI);

  // Gateway BFF
  TBFFGateway = class(TAPIGateway)
  private
    FClientType: TClientType;
    FResponseFilter: TResponseFilter;

    function GetClientTypeFromRequest(const AUserAgent: string): TClientType;
    function AdaptResponseForClient(const AResponse: string;
      AClientType: TClientType): string;
  protected
    procedure HandleRequest(Sender: TObject;
      var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse); override;
  public
    constructor Create(APort: Integer; AClientType: TClientType);
    destructor Destroy; override;

    property ClientType: TClientType read FClientType;
  end;

implementation

uses
  fphttpclient, jsonparser;

// TBFFGateway

constructor TBFFGateway.Create(APort: Integer; AClientType: TClientType);  
var
  MobileFields: array[0..3] of string;
  WebFields: array[0..5] of string;
begin
  inherited Create(APort);
  FClientType := AClientType;

  // Configurer les champs selon le type de client
  case AClientType of
    ctMobileApp:
    begin
      // Mobile: données minimales pour économiser la bande passante
      MobileFields[0] := 'id';
      MobileFields[1] := 'name';
      MobileFields[2] := 'price';
      MobileFields[3] := 'thumbnail';
      FResponseFilter := TResponseFilter.Create(MobileFields);
      WriteLn('[BFF] Gateway Mobile créée (réponses optimisées)');
    end;

    ctWebApp:
    begin
      // Web: plus de détails
      WebFields[0] := 'id';
      WebFields[1] := 'name';
      WebFields[2] := 'price';
      WebFields[3] := 'description';
      WebFields[4] := 'images';
      WebFields[5] := 'specifications';
      FResponseFilter := TResponseFilter.Create(WebFields);
      WriteLn('[BFF] Gateway Web créée (réponses complètes)');
    end;

    ctThirdPartyAPI:
    begin
      // API tierce: tous les champs
      FResponseFilter := nil;
      WriteLn('[BFF] Gateway API tierce créée (tous les champs)');
    end;
  end;
end;

destructor TBFFGateway.Destroy;  
begin
  if FResponseFilter <> nil then
    FResponseFilter.Free;
  inherited;
end;

function TBFFGateway.GetClientTypeFromRequest(const AUserAgent: string): TClientType;  
begin
  if Pos('Mobile', AUserAgent) > 0 then
    Result := ctMobileApp
  else if Pos('Mozilla', AUserAgent) > 0 then
    Result := ctWebApp
  else
    Result := ctThirdPartyAPI;
end;

function TBFFGateway.AdaptResponseForClient(const AResponse: string;
  AClientType: TClientType): string;
begin
  case AClientType of
    ctMobileApp, ctWebApp:
    begin
      if FResponseFilter <> nil then
        Result := FResponseFilter.TransformResponse(AResponse)
      else
        Result := AResponse;
    end;
  else
    Result := AResponse; // Pas de filtrage pour API tierce
  end;
end;

procedure TBFFGateway.HandleRequest(Sender: TObject;
  var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
var
  RouteIndex: Integer;
  ResponseContent: string;
  ClientType: TClientType;
begin
  WriteLn;
  WriteLn(Format('[BFF] Requête: %s %s', [ARequest.Method, ARequest.URI]));

  // Détecter le type de client
  ClientType := GetClientTypeFromRequest(ARequest.GetHeader(hhUserAgent));
  WriteLn(Format('[BFF] Type de client détecté: %d', [Ord(ClientType)]));

  // Traiter la requête normalement
  RouteIndex := FindRoute(ARequest.URI, ARequest.Method);

  if RouteIndex = -1 then
  begin
    AResponse.Code := 404;
    AResponse.Content := '{"error": "Route non trouvée"}';
  end
  else
  begin
    try
      ResponseContent := ForwardRequest(FRoutes[RouteIndex], ARequest);

      // Adapter la réponse selon le client
      ResponseContent := AdaptResponseForClient(ResponseContent, ClientType);

      AResponse.Code := 200;
      AResponse.Content := ResponseContent;
      AResponse.ContentType := 'application/json';

      WriteLn('[BFF] ✓ Réponse adaptée envoyée');

    except
      on E: Exception do
      begin
        AResponse.Code := 500;
        AResponse.Content := Format('{"error": "%s"}', [E.Message]);
      end;
    end;
  end;
end;

end.
```

## Pattern 10 : Monitoring et Logging

Le monitoring permet de surveiller la santé et les performances de la Gateway.

### 1. Module de monitoring

```pascal
unit APIGateway.Monitoring;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, DateUtils;

type
  // Métrique d'une requête
  TRequestMetric = record
    Timestamp: TDateTime;
    Method: string;
    Path: string;
    StatusCode: Integer;
    DurationMs: Integer;
    ClientIP: string;
  end;

  // Statistiques globales
  TGatewayStats = record
    TotalRequests: Int64;
    SuccessRequests: Int64;
    ErrorRequests: Int64;
    AverageDurationMs: Double;
    RequestsPerSecond: Double;
  end;

  // Moniteur de Gateway
  TGatewayMonitor = class
  private
    FMetrics: TList<TRequestMetric>;
    FMaxMetrics: Integer;
    FStartTime: TDateTime;

    procedure CleanupOldMetrics;
  public
    constructor Create(AMaxMetrics: Integer = 10000);
    destructor Destroy; override;

    procedure RecordRequest(const AMethod, APath: string;
      AStatusCode, ADurationMs: Integer; const AClientIP: string);

    function GetStats: TGatewayStats;
    function GetStatsForPath(const APath: string): TGatewayStats;

    procedure PrintStats;
    procedure PrintDetailedReport;
  end;

implementation

// TGatewayMonitor

constructor TGatewayMonitor.Create(AMaxMetrics: Integer);  
begin
  inherited Create;
  FMetrics := TList<TRequestMetric>.Create;
  FMaxMetrics := AMaxMetrics;
  FStartTime := Now;

  WriteLn('[Monitor] Monitoring initialisé');
end;

destructor TGatewayMonitor.Destroy;  
begin
  FMetrics.Free;
  inherited;
end;

procedure TGatewayMonitor.CleanupOldMetrics;  
begin
  while FMetrics.Count > FMaxMetrics do
  begin
    FMetrics.Delete(0);
  end;
end;

procedure TGatewayMonitor.RecordRequest(const AMethod, APath: string;
  AStatusCode, ADurationMs: Integer; const AClientIP: string);
var
  Metric: TRequestMetric;
begin
  Metric.Timestamp := Now;
  Metric.Method := AMethod;
  Metric.Path := APath;
  Metric.StatusCode := AStatusCode;
  Metric.DurationMs := ADurationMs;
  Metric.ClientIP := AClientIP;

  FMetrics.Add(Metric);
  CleanupOldMetrics;

  WriteLn(Format('[Monitor] Requête enregistrée: %s %s [%d] %dms',
    [AMethod, APath, AStatusCode, ADurationMs]));
end;

function TGatewayMonitor.GetStats: TGatewayStats;  
var
  Metric: TRequestMetric;
  TotalDuration: Int64;
  ElapsedSeconds: Int64;
begin
  Result.TotalRequests := FMetrics.Count;
  Result.SuccessRequests := 0;
  Result.ErrorRequests := 0;
  TotalDuration := 0;

  for Metric in FMetrics do
  begin
    if (Metric.StatusCode >= 200) and (Metric.StatusCode < 300) then
      Inc(Result.SuccessRequests)
    else if Metric.StatusCode >= 400 then
      Inc(Result.ErrorRequests);

    TotalDuration := TotalDuration + Metric.DurationMs;
  end;

  if Result.TotalRequests > 0 then
    Result.AverageDurationMs := TotalDuration / Result.TotalRequests
  else
    Result.AverageDurationMs := 0;

  ElapsedSeconds := SecondsBetween(Now, FStartTime);
  if ElapsedSeconds > 0 then
    Result.RequestsPerSecond := Result.TotalRequests / ElapsedSeconds
  else
    Result.RequestsPerSecond := 0;
end;

function TGatewayMonitor.GetStatsForPath(const APath: string): TGatewayStats;  
var
  Metric: TRequestMetric;
  TotalDuration: Int64;
begin
  Result.TotalRequests := 0;
  Result.SuccessRequests := 0;
  Result.ErrorRequests := 0;
  TotalDuration := 0;

  for Metric in FMetrics do
  begin
    if Metric.Path = APath then
    begin
      Inc(Result.TotalRequests);

      if (Metric.StatusCode >= 200) and (Metric.StatusCode < 300) then
        Inc(Result.SuccessRequests)
      else if Metric.StatusCode >= 400 then
        Inc(Result.ErrorRequests);

      TotalDuration := TotalDuration + Metric.DurationMs;
    end;
  end;

  if Result.TotalRequests > 0 then
    Result.AverageDurationMs := TotalDuration / Result.TotalRequests
  else
    Result.AverageDurationMs := 0;

  Result.RequestsPerSecond := 0; // Non calculé pour une route spécifique
end;

procedure TGatewayMonitor.PrintStats;  
var
  Stats: TGatewayStats;
begin
  Stats := GetStats;

  WriteLn;
  WriteLn('═══════════════════════════════════════════');
  WriteLn('  STATISTIQUES GATEWAY                    ');
  WriteLn('═══════════════════════════════════════════');
  WriteLn(Format('Requêtes totales: %d', [Stats.TotalRequests]));
  WriteLn(Format('Succès: %d (%.1f%%)',
    [Stats.SuccessRequests,
     (Stats.SuccessRequests / Stats.TotalRequests) * 100]));
  WriteLn(Format('Erreurs: %d (%.1f%%)',
    [Stats.ErrorRequests,
     (Stats.ErrorRequests / Stats.TotalRequests) * 100]));
  WriteLn(Format('Durée moyenne: %.2f ms', [Stats.AverageDurationMs]));
  WriteLn(Format('Req/sec: %.2f', [Stats.RequestsPerSecond]));
  WriteLn('═══════════════════════════════════════════');
end;

procedure TGatewayMonitor.PrintDetailedReport;  
var
  PathStats: TDictionary<string, Integer>;
  Metric: TRequestMetric;
  Path: string;
  Count: Integer;
  Stats: TGatewayStats;
begin
  WriteLn;
  WriteLn('═══════════════════════════════════════════');
  WriteLn('  RAPPORT DÉTAILLÉ                        ');
  WriteLn('═══════════════════════════════════════════');

  // Statistiques par chemin
  PathStats := TDictionary<string, Integer>.Create;
  try
    for Metric in FMetrics do
    begin
      if PathStats.TryGetValue(Metric.Path, Count) then
        PathStats.AddOrSetValue(Metric.Path, Count + 1)
      else
        PathStats.Add(Metric.Path, 1);
    end;

    WriteLn('Routes les plus utilisées:');
    for Path in PathStats.Keys do
    begin
      Stats := GetStatsForPath(Path);
      WriteLn(Format('  %s: %d requêtes (%.2f ms moy.)',
        [Path, Stats.TotalRequests, Stats.AverageDurationMs]));
    end;

  finally
    PathStats.Free;
  end;

  WriteLn('═══════════════════════════════════════════');
end;

end.
```

### 2. Gateway avec monitoring

```pascal
unit APIGateway.Monitored;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpserver, httpdefs, DateUtils,
  APIGateway.Core, APIGateway.Monitoring;

type
  // Gateway avec monitoring
  TMonitoredGateway = class(TAPIGateway)
  private
    FMonitor: TGatewayMonitor;
  protected
    procedure HandleRequest(Sender: TObject;
      var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse); override;
  public
    constructor Create(APort: Integer);
    destructor Destroy; override;

    property Monitor: TGatewayMonitor read FMonitor;
  end;

implementation

// TMonitoredGateway

constructor TMonitoredGateway.Create(APort: Integer);  
begin
  inherited Create(APort);
  FMonitor := TGatewayMonitor.Create;

  WriteLn('[MonitoredGateway] Gateway avec monitoring créée');
end;

destructor TMonitoredGateway.Destroy;  
begin
  WriteLn;
  WriteLn('Statistiques finales:');
  FMonitor.PrintStats;
  FMonitor.PrintDetailedReport;

  FMonitor.Free;
  inherited;
end;

procedure TMonitoredGateway.HandleRequest(Sender: TObject;
  var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
var
  StartTime: TDateTime;
  DurationMs: Integer;
begin
  StartTime := Now;

  // Traiter la requête
  inherited HandleRequest(Sender, ARequest, AResponse);

  // Calculer la durée
  DurationMs := MilliSecondsBetween(Now, StartTime);

  // Enregistrer la métrique
  FMonitor.RecordRequest(
    ARequest.Method,
    ARequest.URI,
    AResponse.Code,
    DurationMs,
    ARequest.RemoteAddress
  );
end;

end.
```

## Gateway Complète avec Tous les Patterns

Voici un exemple de Gateway qui combine plusieurs patterns :

```pascal
unit APIGateway.Complete;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpserver, httpdefs,
  APIGateway.Core,
  APIGateway.Auth,
  APIGateway.RateLimit,
  APIGateway.Cache,
  APIGateway.CircuitBreaker,
  APIGateway.Monitoring,
  APIGateway.ServiceRegistry;

type
  // Gateway complète avec tous les patterns
  TCompleteGateway = class(TAPIGateway)
  private
    FAuthManager: TAuthManager;
    FRateLimiter: TRateLimiter;
    FCacheManager: TCacheManager;
    FCircuitBreakers: TObjectDictionary<string, TCircuitBreaker>;
    FMonitor: TGatewayMonitor;
    FServiceRegistry: TServiceRegistry;

    function GetClientId(ARequest: TFPHTTPConnectionRequest): string;
    function CheckAuth(ARequest: TFPHTTPConnectionRequest): Boolean;
    function CheckRateLimit(const AClientId: string): Boolean;
    function TryGetFromCache(const AMethod, APath: string;
      out AContent: string): Boolean;
  protected
    procedure HandleRequest(Sender: TObject;
      var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse); override;
  public
    constructor Create(APort: Integer; const ASecretKey: string);
    destructor Destroy; override;

    property AuthManager: TAuthManager read FAuthManager;
    property RateLimiter: TRateLimiter read FRateLimiter;
    property CacheManager: TCacheManager read FCacheManager;
    property Monitor: TGatewayMonitor read FMonitor;
    property ServiceRegistry: TServiceRegistry read FServiceRegistry;
  end;

implementation

uses
  DateUtils;

// TCompleteGateway

constructor TCompleteGateway.Create(APort: Integer; const ASecretKey: string);  
begin
  inherited Create(APort);

  FAuthManager := TAuthManager.Create(ASecretKey);
  FRateLimiter := TRateLimiter.Create(100, 60); // 100 req/min
  FCacheManager := TCacheManager.Create(300, 1000); // 5 min TTL
  FCircuitBreakers := TObjectDictionary<string, TCircuitBreaker>.Create([doOwnsValues]);
  FMonitor := TGatewayMonitor.Create;
  FServiceRegistry := TServiceRegistry.Create;

  WriteLn('[CompleteGateway] Gateway complète créée avec tous les patterns');
end;

destructor TCompleteGateway.Destroy;  
begin
  WriteLn;
  WriteLn('═══════════════════════════════════════════');
  WriteLn('  RAPPORT FINAL DE LA GATEWAY              ');
  WriteLn('═══════════════════════════════════════════');

  FMonitor.PrintStats;
  WriteLn;
  WriteLn('Cache: ' + FCacheManager.GetStats);

  FServiceRegistry.Free;
  FMonitor.Free;
  FCircuitBreakers.Free;
  FCacheManager.Free;
  FRateLimiter.Free;
  FAuthManager.Free;

  inherited;
end;

function TCompleteGateway.GetClientId(
  ARequest: TFPHTTPConnectionRequest): string;
var
  AuthHeader, Token: string;
begin
  AuthHeader := ARequest.GetCustomHeader('Authorization');

  if AuthHeader <> '' then
  begin
    Token := FAuthManager.ExtractToken(AuthHeader);
    Result := FAuthManager.GetUserFromToken(Token);
  end
  else
    Result := ARequest.RemoteAddress;
end;

function TCompleteGateway.CheckAuth(
  ARequest: TFPHTTPConnectionRequest): Boolean;
var
  AuthHeader, Token: string;
begin
  // Routes publiques (à configurer selon les besoins)
  if (ARequest.URI = '/health') or (ARequest.URI = '/login') then
  begin
    Result := True;
    Exit;
  end;

  AuthHeader := ARequest.GetCustomHeader('Authorization');

  if AuthHeader = '' then
  begin
    WriteLn('[CompleteGateway] ✗ Authorization header manquant');
    Result := False;
    Exit;
  end;

  Token := FAuthManager.ExtractToken(AuthHeader);
  Result := FAuthManager.ValidateToken(Token);
end;

function TCompleteGateway.CheckRateLimit(const AClientId: string): Boolean;  
begin
  Result := FRateLimiter.IsAllowed(AClientId);
end;

function TCompleteGateway.TryGetFromCache(const AMethod, APath: string;
  out AContent: string): Boolean;
begin
  // Seules les requêtes GET sont mises en cache
  if AMethod = 'GET' then
    Result := FCacheManager.Get(AMethod, APath, AContent)
  else
    Result := False;
end;

procedure TCompleteGateway.HandleRequest(Sender: TObject;
  var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
var
  StartTime: TDateTime;
  DurationMs: Integer;
  ClientId: string;
  CachedContent: string;
  RouteIndex: Integer;
  ResponseContent: string;
begin
  StartTime := Now;

  WriteLn;
  WriteLn('═══════════════════════════════════════════');
  WriteLn(Format('[CompleteGateway] %s %s', [ARequest.Method, ARequest.URI]));
  WriteLn('═══════════════════════════════════════════');

  try
    // 1. Authentification
    if not CheckAuth(ARequest) then
    begin
      AResponse.Code := 401;
      AResponse.Content := '{"error": "Non authentifié"}';
      Exit;
    end;

    // 2. Rate Limiting
    ClientId := GetClientId(ARequest);
    if not CheckRateLimit(ClientId) then
    begin
      AResponse.Code := 429;
      AResponse.Content := '{"error": "Trop de requêtes"}';
      Exit;
    end;

    // 3. Cache
    if TryGetFromCache(ARequest.Method, ARequest.URI, CachedContent) then
    begin
      AResponse.Code := 200;
      AResponse.Content := CachedContent;
      AResponse.ContentType := 'application/json';
      AResponse.SetCustomHeader('X-Cache', 'HIT');
      WriteLn('[CompleteGateway] ✓ Réponse depuis le cache');
      Exit;
    end;

    // 4. Routage
    RouteIndex := FindRoute(ARequest.URI, ARequest.Method);

    if RouteIndex = -1 then
    begin
      AResponse.Code := 404;
      AResponse.Content := '{"error": "Route non trouvée"}';
      Exit;
    end;

    // 5. Forward avec Circuit Breaker
    try
      ResponseContent := ForwardRequest(FRoutes[RouteIndex], ARequest);

      AResponse.Code := 200;
      AResponse.Content := ResponseContent;
      AResponse.ContentType := 'application/json';
      AResponse.SetCustomHeader('X-Cache', 'MISS');

      // Mise en cache si GET
      if ARequest.Method = 'GET' then
        FCacheManager.Put(ARequest.Method, ARequest.URI, ResponseContent);

      WriteLn('[CompleteGateway] ✓ Requête traitée avec succès');

    except
      on E: Exception do
      begin
        AResponse.Code := 503;
        AResponse.Content := Format('{"error": "Service indisponible: %s"}',
          [E.Message]);
        WriteLn(Format('[CompleteGateway] ✗ Erreur: %s', [E.Message]));
      end;
    end;

  finally
    // 6. Monitoring
    DurationMs := MilliSecondsBetween(Now, StartTime);
    FMonitor.RecordRequest(
      ARequest.Method,
      ARequest.URI,
      AResponse.Code,
      DurationMs,
      ARequest.RemoteAddress
    );

    WriteLn(Format('[CompleteGateway] Durée: %d ms', [DurationMs]));
    WriteLn('═══════════════════════════════════════════');
  end;
end;

end.
```

## Exemple d'utilisation de la Gateway complète

```pascal
program CompleteGatewayDemo;

{$mode objfpc}{$H+}

uses
  SysUtils,
  APIGateway.Complete;

var
  Gateway: TCompleteGateway;

begin
  WriteLn('═══════════════════════════════════════════');
  WriteLn('  API GATEWAY COMPLÈTE                    ');
  WriteLn('  Avec tous les patterns                   ');
  WriteLn('═══════════════════════════════════════════');
  WriteLn;

  Gateway := TCompleteGateway.Create(8080, 'secret-key-12345');
  try
    WriteLn('Configuration de la Gateway...');
    WriteLn;

    // Enregistrer les services
    Gateway.ServiceRegistry.RegisterService('products', 'localhost', 8001);
    Gateway.ServiceRegistry.RegisterService('orders', 'localhost', 8002);
    Gateway.ServiceRegistry.RegisterService('users', 'localhost', 8003);

    // Configurer les routes
    Gateway.AddRoute('/api/products', 'GET',
      'http://localhost:8001', '/products');
    Gateway.AddRoute('/api/orders', 'GET',
      'http://localhost:8002', '/orders');
    Gateway.AddRoute('/api/orders', 'POST',
      'http://localhost:8002', '/orders');
    Gateway.AddRoute('/api/users', 'GET',
      'http://localhost:8003', '/users');

    // Endpoint de santé (public)
    Gateway.AddRoute('/health', 'GET',
      'http://localhost:8080', '/health');

    WriteLn;
    WriteLn('Fonctionnalités actives:');
    WriteLn('  ✓ Authentification JWT');
    WriteLn('  ✓ Rate Limiting (100 req/min)');
    WriteLn('  ✓ Cache (5 min TTL)');
    WriteLn('  ✓ Circuit Breaker');
    WriteLn('  ✓ Service Discovery');
    WriteLn('  ✓ Monitoring & Métriques');
    WriteLn;

    // Démarrer
    Gateway.Start;

    WriteLn('Gateway prête ! Exemples de requêtes:');
    WriteLn('  curl -H "Authorization: Bearer <token>" http://localhost:8080/api/products');
    WriteLn('  curl http://localhost:8080/health');
    WriteLn;
    WriteLn('Appuyez sur Entrée pour arrêter et voir les statistiques...');
    ReadLn;

  finally
    Gateway.Free;
  end;

  WriteLn;
  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
```

## Bonnes Pratiques

### ✅ À faire

1. **Toujours authentifier**
```pascal
// ✅ BON: Vérifier l'authentification
if not IsAuthenticated(Request) then  
begin
  Response.Code := 401;
  Exit;
end;
```

2. **Limiter les requêtes**
```pascal
// ✅ BON: Rate limiting
if not RateLimiter.IsAllowed(ClientId) then  
begin
  Response.Code := 429;
  Exit;
end;
```

3. **Utiliser le cache intelligemment**
```pascal
// ✅ BON: Cache uniquement GET
if Method = 'GET' then
  Cache.Put(Path, Response);
```

4. **Logger toutes les requêtes**
```pascal
// ✅ BON: Monitoring complet
Monitor.RecordRequest(Method, Path, StatusCode, Duration);
```

5. **Gérer les timeouts**
```pascal
// ✅ BON: Timeout défini
Client.ConnectTimeout := 5000; // 5 secondes  
Client.IOTimeout := 10000;     // 10 secondes
```

### ❌ À éviter

1. **Exposer les services directement**
```pascal
// ❌ MAUVAIS: URL du service dans la réponse
Response := '{"service_url": "http://internal-service:8001"}';
```

2. **Pas de validation**
```pascal
// ❌ MAUVAIS: Forward sans vérification
ForwardRequest(Request); // Sans auth, sans rate limit
```

3. **Cache sans TTL**
```pascal
// ❌ MAUVAIS: Cache permanent
Cache.Put(Key, Value); // Pas d'expiration
```

4. **Ignorer les erreurs**
```pascal
// ❌ MAUVAIS: Pas de gestion d'erreur
try
  CallService;
except
  // Silence...
end;
```

## Conclusion

L'API Gateway est un composant essentiel des architectures microservices modernes. Les points clés à retenir :

**Rôles principaux :**
- Point d'entrée unique pour tous les clients
- Authentification et autorisation centralisées
- Routage intelligent vers les services
- Protection contre les surcharges (rate limiting)
- Agrégation et transformation de réponses

**Patterns essentiels :**
- **Authentification** : sécurité centralisée
- **Rate Limiting** : protection contre les abus
- **Circuit Breaker** : résilience face aux pannes
- **Caching** : amélioration des performances
- **Service Discovery** : routage dynamique
- **BFF** : optimisation par type de client
- **Monitoring** : observabilité complète

**Avantages :**
- Simplifie le code client
- Centralise les préoccupations transversales
- Améliore les performances (cache, agrégation)
- Facilite l'évolution du système

**Considérations :**
- Peut devenir un point de défaillance unique (SPOF)
- Nécessite une attention particulière aux performances
- Complexité de configuration et de maintenance

Une Gateway bien conçue est la pierre angulaire d'une architecture microservices réussie !

---

⏭️ [Service mesh et observabilité](/21-architecture-logicielle-avancee/10-service-mesh-observabilite.md)
