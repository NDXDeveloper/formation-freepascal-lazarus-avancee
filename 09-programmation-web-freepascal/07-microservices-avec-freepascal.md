🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 9.7 Microservices avec FreePascal

## Introduction

Les microservices représentent une architecture où une application est décomposée en plusieurs services indépendants, chacun étant responsable d'une fonctionnalité spécifique. Cette approche offre de nombreux avantages : scalabilité, maintenabilité, déploiement indépendant, et choix technologiques flexibles.

FreePascal, avec sa performance et sa capacité à créer des exécutables autonomes, est particulièrement adapté pour développer des microservices légers et efficaces.

## 9.7.1 Architecture Microservices : Principes fondamentaux

### Comparaison : Monolithe vs Microservices

**Application Monolithique :**

```
┌─────────────────────────────────────────┐
│         Application Monolithique        │
│                                         │
│  ┌──────────┐  ┌──────────┐  ┌───────┐  │
│  │   UI     │  │ Business │  │  Data │  │
│  │  Layer   │  │  Logic   │  │ Layer │  │
│  └──────────┘  └──────────┘  └───────┘  │
│                                         │
│  Tout est dans une seule application    │
└─────────────────────────────────────────┘
```

**Architecture Microservices :**

```
┌──────────────┐    ┌──────────────┐    ┌──────────────┐
│   Service    │    │   Service    │    │   Service    │
│  Utilisateur │◄──►│   Commande   │◄──►│   Paiement   │
│              │    │              │    │              │
│  Port: 8001  │    │  Port: 8002  │    │  Port: 8003  │
└──────┬───────┘    └──────┬───────┘    └──────┬───────┘
       │                   │                   │
       └───────────────────┴───────────────────┘
                           │
                    ┌──────▼───────┐
                    │  API Gateway │
                    │  Port: 8000  │
                    └──────────────┘
```

### Caractéristiques des microservices

**Avantages :**
- **Indépendance** : Chaque service peut être développé et déployé séparément
- **Scalabilité** : Possibilité de scaler uniquement les services nécessaires
- **Résilience** : La panne d'un service n'affecte pas les autres
- **Flexibilité technologique** : Chaque service peut utiliser sa propre stack
- **Équipes autonomes** : Des équipes différentes peuvent travailler sur des services différents

**Inconvénients :**
- **Complexité** : Plus difficile à gérer qu'un monolithe
- **Communication réseau** : Latence et fiabilité
- **Cohérence des données** : Transactions distribuées complexes
- **Débogage** : Plus difficile de tracer les problèmes
- **Déploiement** : Infrastructure plus complexe

## 9.7.2 Premier microservice avec fpWeb

### Structure d'un microservice simple

Créons un microservice basique de gestion d'utilisateurs :

```pascal
program UserService;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, fphttpapp, httpdefs, httproute, fpjson, fpjsonrpc;

type
  { TUser - Modèle de données utilisateur }
  TUser = class
  public
    ID: Integer;
    Username: string;
    Email: string;
    CreatedAt: TDateTime;
    function ToJSON: TJSONObject;
  end;

function TUser.ToJSON: TJSONObject;  
begin
  Result := TJSONObject.Create;
  Result.Add('id', ID);
  Result.Add('username', Username);
  Result.Add('email', Email);
  Result.Add('created_at', FormatDateTime('yyyy-mm-dd hh:nn:ss', CreatedAt));
end;

{ Gestionnaire de routes }
procedure GetUsers(ARequest: TRequest; AResponse: TResponse);  
var
  Users: TJSONArray;
  User: TUser;
begin
  Users := TJSONArray.Create;
  try
    // Simuler des données (en production, lire depuis une base de données)
    User := TUser.Create;
    User.ID := 1;
    User.Username := 'john_doe';
    User.Email := 'john@example.com';
    User.CreatedAt := Now;
    Users.Add(User.ToJSON);
    User.Free;

    User := TUser.Create;
    User.ID := 2;
    User.Username := 'jane_smith';
    User.Email := 'jane@example.com';
    User.CreatedAt := Now;
    Users.Add(User.ToJSON);
    User.Free;

    // Répondre avec JSON
    AResponse.ContentType := 'application/json';
    AResponse.Content := Users.AsJSON;
    AResponse.SendResponse;
  finally
    Users.Free;
  end;
end;

procedure GetUserByID(ARequest: TRequest; AResponse: TResponse);  
var
  UserID: Integer;
  User: TUser;
  JSONResponse: TJSONObject;
begin
  // Récupérer l'ID depuis l'URL
  UserID := StrToIntDef(ARequest.RouteParams['id'], 0);

  if UserID <= 0 then
  begin
    AResponse.Code := 400;
    AResponse.Content := '{"error": "Invalid user ID"}';
    AResponse.SendResponse;
    Exit;
  end;

  // Simuler la récupération d'un utilisateur
  User := TUser.Create;
  try
    User.ID := UserID;
    User.Username := 'user_' + IntToStr(UserID);
    User.Email := 'user' + IntToStr(UserID) + '@example.com';
    User.CreatedAt := Now;

    JSONResponse := User.ToJSON;
    try
      AResponse.ContentType := 'application/json';
      AResponse.Content := JSONResponse.AsJSON;
      AResponse.SendResponse;
    finally
      JSONResponse.Free;
    end;
  finally
    User.Free;
  end;
end;

procedure CreateUser(ARequest: TRequest; AResponse: TResponse);  
var
  InputJSON: TJSONObject;
  Username, Email: string;
  NewUser: TUser;
  ResponseJSON: TJSONObject;
begin
  try
    // Parser le JSON reçu
    InputJSON := GetJSON(ARequest.Content) as TJSONObject;
    try
      Username := InputJSON.Get('username', '');
      Email := InputJSON.Get('email', '');

      // Valider les données
      if (Username = '') or (Email = '') then
      begin
        AResponse.Code := 400;
        AResponse.Content := '{"error": "Username and email are required"}';
        AResponse.SendResponse;
        Exit;
      end;

      // Créer l'utilisateur (en production, enregistrer en base)
      NewUser := TUser.Create;
      try
        NewUser.ID := Random(10000); // ID aléatoire pour la démo
        NewUser.Username := Username;
        NewUser.Email := Email;
        NewUser.CreatedAt := Now;

        ResponseJSON := NewUser.ToJSON;
        try
          AResponse.Code := 201; // Created
          AResponse.ContentType := 'application/json';
          AResponse.Content := ResponseJSON.AsJSON;
          AResponse.SendResponse;
        finally
          ResponseJSON.Free;
        end;
      finally
        NewUser.Free;
      end;
    finally
      InputJSON.Free;
    end;
  except
    on E: Exception do
    begin
      AResponse.Code := 400;
      AResponse.Content := Format('{"error": "%s"}', [E.Message]);
      AResponse.SendResponse;
    end;
  end;
end;

procedure HealthCheck(ARequest: TRequest; AResponse: TResponse);  
var
  HealthJSON: TJSONObject;
begin
  HealthJSON := TJSONObject.Create;
  try
    HealthJSON.Add('status', 'healthy');
    HealthJSON.Add('service', 'user-service');
    HealthJSON.Add('timestamp', FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));
    HealthJSON.Add('version', '1.0.0');

    AResponse.ContentType := 'application/json';
    AResponse.Content := HealthJSON.AsJSON;
    AResponse.SendResponse;
  finally
    HealthJSON.Free;
  end;
end;

begin
  // Enregistrer les routes
  HTTPRouter.RegisterRoute('/health', @HealthCheck);
  HTTPRouter.RegisterRoute('/api/users', @GetUsers);
  HTTPRouter.RegisterRoute('/api/users/:id', @GetUserByID);
  HTTPRouter.RegisterRoute('/api/users', rmPost, @CreateUser);

  // Configurer le serveur
  Application.Port := 8001;
  Application.Title := 'User Microservice';

  WriteLn('User Service démarré sur le port 8001');
  WriteLn('Endpoints disponibles :');
  WriteLn('  GET  /health           - Health check');
  WriteLn('  GET  /api/users        - Liste des utilisateurs');
  WriteLn('  GET  /api/users/:id    - Utilisateur par ID');
  WriteLn('  POST /api/users        - Créer un utilisateur');
  WriteLn('');
  WriteLn('Appuyez sur Ctrl+C pour arrêter le service');

  // Démarrer le serveur
  Application.Initialize;
  Application.Run;
end.
```

### Compilation et exécution

```bash
# Compiler le microservice
fpc UserService.pas

# Exécuter
./UserService

# Tester avec curl
curl http://localhost:8001/health  
curl http://localhost:8001/api/users  
curl http://localhost:8001/api/users/1  
curl -X POST http://localhost:8001/api/users \
  -H "Content-Type: application/json" \
  -d '{"username":"alice","email":"alice@example.com"}'
```

## 9.7.3 Communication entre microservices

### Client HTTP pour appeler d'autres services

```pascal
unit ServiceClient;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fphttpclient, fpjson;

type
  TServiceClient = class
  private
    FBaseURL: string;
    FTimeout: Integer;
  public
    constructor Create(const BaseURL: string; Timeout: Integer = 5000);
    function Get(const Endpoint: string): TJSONData;
    function Post(const Endpoint: string; const Data: TJSONObject): TJSONData;
    function Put(const Endpoint: string; const Data: TJSONObject): TJSONData;
    function Delete(const Endpoint: string): Boolean;
    property BaseURL: string read FBaseURL write FBaseURL;
  end;

implementation

constructor TServiceClient.Create(const BaseURL: string; Timeout: Integer);  
begin
  inherited Create;
  FBaseURL := BaseURL;
  FTimeout := Timeout;
end;

function TServiceClient.Get(const Endpoint: string): TJSONData;  
var
  HTTPClient: TFPHTTPClient;
  Response: string;
begin
  Result := nil;
  HTTPClient := TFPHTTPClient.Create(nil);
  try
    HTTPClient.ConnectTimeout := FTimeout;
    HTTPClient.IOTimeout := FTimeout;

    Response := HTTPClient.Get(FBaseURL + Endpoint);

    if Response <> '' then
      Result := GetJSON(Response);
  finally
    HTTPClient.Free;
  end;
end;

function TServiceClient.Post(const Endpoint: string; const Data: TJSONObject): TJSONData;  
var
  HTTPClient: TFPHTTPClient;
  Response: string;
  RequestBody: TStringStream;
begin
  Result := nil;
  HTTPClient := TFPHTTPClient.Create(nil);
  try
    HTTPClient.ConnectTimeout := FTimeout;
    HTTPClient.IOTimeout := FTimeout;

    RequestBody := TStringStream.Create(Data.AsJSON);
    try
      HTTPClient.RequestBody := RequestBody;
      HTTPClient.AddHeader('Content-Type', 'application/json');

      Response := HTTPClient.Post(FBaseURL + Endpoint);

      if Response <> '' then
        Result := GetJSON(Response);
    finally
      RequestBody.Free;
    end;
  finally
    HTTPClient.Free;
  end;
end;

function TServiceClient.Put(const Endpoint: string; const Data: TJSONObject): TJSONData;  
var
  HTTPClient: TFPHTTPClient;
  Response: string;
  RequestBody: TStringStream;
begin
  Result := nil;
  HTTPClient := TFPHTTPClient.Create(nil);
  try
    HTTPClient.ConnectTimeout := FTimeout;
    HTTPClient.IOTimeout := FTimeout;

    RequestBody := TStringStream.Create(Data.AsJSON);
    try
      HTTPClient.RequestBody := RequestBody;
      HTTPClient.AddHeader('Content-Type', 'application/json');

      Response := HTTPClient.Put(FBaseURL + Endpoint);

      if Response <> '' then
        Result := GetJSON(Response);
    finally
      RequestBody.Free;
    end;
  finally
    HTTPClient.Free;
  end;
end;

function TServiceClient.Delete(const Endpoint: string): Boolean;  
var
  HTTPClient: TFPHTTPClient;
begin
  Result := False;
  HTTPClient := TFPHTTPClient.Create(nil);
  try
    HTTPClient.ConnectTimeout := FTimeout;
    HTTPClient.IOTimeout := FTimeout;

    HTTPClient.Delete(FBaseURL + Endpoint);
    Result := HTTPClient.ResponseStatusCode = 204;
  finally
    HTTPClient.Free;
  end;
end;

end.
```

### Service de commandes appelant le service utilisateur

```pascal
program OrderService;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, fphttpapp, httpdefs, httproute, fpjson, ServiceClient;

type
  TOrder = class
  public
    ID: Integer;
    UserID: Integer;
    ProductName: string;
    Quantity: Integer;
    TotalPrice: Double;
    CreatedAt: TDateTime;
    function ToJSON: TJSONObject;
  end;

function TOrder.ToJSON: TJSONObject;  
begin
  Result := TJSONObject.Create;
  Result.Add('id', ID);
  Result.Add('user_id', UserID);
  Result.Add('product_name', ProductName);
  Result.Add('quantity', Quantity);
  Result.Add('total_price', TotalPrice);
  Result.Add('created_at', FormatDateTime('yyyy-mm-dd hh:nn:ss', CreatedAt));
end;

procedure CreateOrder(ARequest: TRequest; AResponse: TResponse);  
var
  InputJSON: TJSONObject;
  UserID: Integer;
  ProductName: string;
  Quantity: Integer;
  UserClient: TServiceClient;
  UserData: TJSONData;
  NewOrder: TOrder;
  ResponseJSON: TJSONObject;
begin
  try
    InputJSON := GetJSON(ARequest.Content) as TJSONObject;
    try
      UserID := InputJSON.Get('user_id', 0);
      ProductName := InputJSON.Get('product_name', '');
      Quantity := InputJSON.Get('quantity', 0);

      if (UserID <= 0) or (ProductName = '') or (Quantity <= 0) then
      begin
        AResponse.Code := 400;
        AResponse.Content := '{"error": "Invalid order data"}';
        AResponse.SendResponse;
        Exit;
      end;

      // Vérifier que l'utilisateur existe en appelant le service utilisateur
      UserClient := TServiceClient.Create('http://localhost:8001');
      try
        UserData := UserClient.Get('/api/users/' + IntToStr(UserID));

        if not Assigned(UserData) then
        begin
          AResponse.Code := 404;
          AResponse.Content := '{"error": "User not found"}';
          AResponse.SendResponse;
          Exit;
        end;
        UserData.Free;
      finally
        UserClient.Free;
      end;

      // Créer la commande
      NewOrder := TOrder.Create;
      try
        NewOrder.ID := Random(10000);
        NewOrder.UserID := UserID;
        NewOrder.ProductName := ProductName;
        NewOrder.Quantity := Quantity;
        NewOrder.TotalPrice := Quantity * 29.99; // Prix fictif
        NewOrder.CreatedAt := Now;

        ResponseJSON := NewOrder.ToJSON;
        try
          AResponse.Code := 201;
          AResponse.ContentType := 'application/json';
          AResponse.Content := ResponseJSON.AsJSON;
          AResponse.SendResponse;
        finally
          ResponseJSON.Free;
        end;
      finally
        NewOrder.Free;
      end;
    finally
      InputJSON.Free;
    end;
  except
    on E: Exception do
    begin
      AResponse.Code := 500;
      AResponse.Content := Format('{"error": "%s"}', [E.Message]);
      AResponse.SendResponse;
    end;
  end;
end;

procedure HealthCheck(ARequest: TRequest; AResponse: TResponse);  
var
  HealthJSON: TJSONObject;
begin
  HealthJSON := TJSONObject.Create;
  try
    HealthJSON.Add('status', 'healthy');
    HealthJSON.Add('service', 'order-service');
    HealthJSON.Add('timestamp', FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));

    AResponse.ContentType := 'application/json';
    AResponse.Content := HealthJSON.AsJSON;
    AResponse.SendResponse;
  finally
    HealthJSON.Free;
  end;
end;

begin
  HTTPRouter.RegisterRoute('/health', @HealthCheck);
  HTTPRouter.RegisterRoute('/api/orders', rmPost, @CreateOrder);

  Application.Port := 8002;
  Application.Title := 'Order Microservice';

  WriteLn('Order Service démarré sur le port 8002');
  WriteLn('Endpoints disponibles :');
  WriteLn('  GET  /health           - Health check');
  WriteLn('  POST /api/orders       - Créer une commande');
  WriteLn('');

  Application.Initialize;
  Application.Run;
end.
```

## 9.7.4 API Gateway

### Rôle de l'API Gateway

L'API Gateway est le point d'entrée unique pour tous les clients. Il :
- Route les requêtes vers les microservices appropriés
- Gère l'authentification centralisée
- Applique les limites de débit (rate limiting)
- Agrège les réponses de plusieurs services
- Gère les CORS et la sécurité

```pascal
program APIGateway;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, fphttpapp, httpdefs, httproute, fpjson, ServiceClient;

type
  TGateway = class
  private
    FUserServiceURL: string;
    FOrderServiceURL: string;
    function IsAuthenticated(ARequest: TRequest): Boolean;
  public
    constructor Create;
    procedure RouteToUserService(ARequest: TRequest; AResponse: TResponse);
    procedure RouteToOrderService(ARequest: TRequest; AResponse: TResponse);
    procedure GetUserOrders(ARequest: TRequest; AResponse: TResponse);
  end;

constructor TGateway.Create;  
begin
  inherited Create;
  FUserServiceURL := 'http://localhost:8001';
  FOrderServiceURL := 'http://localhost:8002';
end;

function TGateway.IsAuthenticated(ARequest: TRequest): Boolean;  
var
  AuthHeader: string;
begin
  // Vérification simple du token (en production, utiliser JWT)
  AuthHeader := ARequest.GetCustomHeader('Authorization');
  Result := (AuthHeader <> '') and (Copy(AuthHeader, 1, 7) = 'Bearer ');
end;

procedure TGateway.RouteToUserService(ARequest: TRequest; AResponse: TResponse);  
var
  Client: TServiceClient;
  Response: TJSONData;
  Endpoint: string;
  InputJSON: TJSONObject;
  Method: string;
begin
  // Vérifier l'authentification pour les opérations sensibles
  if (ARequest.Method = 'POST') and not IsAuthenticated(ARequest) then
  begin
    AResponse.Code := 401;
    AResponse.Content := '{"error": "Unauthorized"}';
    AResponse.SendResponse;
    Exit;
  end;

  Client := TServiceClient.Create(FUserServiceURL);
  try
    // Extraire le endpoint en retirant le préfixe /gateway
    Endpoint := StringReplace(ARequest.PathInfo, '/gateway', '', []);

    Method := ARequest.Method;
    if Method = 'GET' then
      Response := Client.Get(Endpoint)
    else if Method = 'POST' then
    begin
      InputJSON := GetJSON(ARequest.Content) as TJSONObject;
      try
        Response := Client.Post(Endpoint, InputJSON);
      finally
        InputJSON.Free;
      end;
    end
    else
      Response := nil;

    if Assigned(Response) then
    try
      AResponse.ContentType := 'application/json';
      AResponse.Content := Response.AsJSON;
      AResponse.SendResponse;
    finally
      Response.Free;
    end
    else
    begin
      AResponse.Code := 500;
      AResponse.Content := '{"error": "Service unavailable"}';
      AResponse.SendResponse;
    end;
  finally
    Client.Free;
  end;
end;

procedure TGateway.RouteToOrderService(ARequest: TRequest; AResponse: TResponse);  
var
  Client: TServiceClient;
  Response: TJSONData;
  Endpoint: string;
  InputJSON: TJSONObject;
begin
  if not IsAuthenticated(ARequest) then
  begin
    AResponse.Code := 401;
    AResponse.Content := '{"error": "Unauthorized"}';
    AResponse.SendResponse;
    Exit;
  end;

  Client := TServiceClient.Create(FOrderServiceURL);
  try
    Endpoint := StringReplace(ARequest.PathInfo, '/gateway', '', []);

    if ARequest.Method = 'POST' then
    begin
      InputJSON := GetJSON(ARequest.Content) as TJSONObject;
      try
        Response := Client.Post(Endpoint, InputJSON);
      finally
        InputJSON.Free;
      end;
    end
    else
      Response := nil;

    if Assigned(Response) then
    try
      AResponse.ContentType := 'application/json';
      AResponse.Content := Response.AsJSON;
      AResponse.SendResponse;
    finally
      Response.Free;
    end
    else
    begin
      AResponse.Code := 500;
      AResponse.Content := '{"error": "Service unavailable"}';
      AResponse.SendResponse;
    end;
  finally
    Client.Free;
  end;
end;

procedure TGateway.GetUserOrders(ARequest: TRequest; AResponse: TResponse);  
var
  UserClient, OrderClient: TServiceClient;
  UserID: Integer;
  UserData, OrdersData: TJSONData;
  CombinedResponse: TJSONObject;
begin
  if not IsAuthenticated(ARequest) then
  begin
    AResponse.Code := 401;
    AResponse.Content := '{"error": "Unauthorized"}';
    AResponse.SendResponse;
    Exit;
  end;

  UserID := StrToIntDef(ARequest.RouteParams['id'], 0);

  UserClient := TServiceClient.Create(FUserServiceURL);
  OrderClient := TServiceClient.Create(FOrderServiceURL);
  try
    // Récupérer l'utilisateur
    UserData := UserClient.Get('/api/users/' + IntToStr(UserID));

    // Récupérer les commandes de l'utilisateur
    OrdersData := OrderClient.Get('/api/orders?user_id=' + IntToStr(UserID));

    if Assigned(UserData) and Assigned(OrdersData) then
    try
      // Combiner les données
      CombinedResponse := TJSONObject.Create;
      try
        CombinedResponse.Add('user', UserData.Clone);
        CombinedResponse.Add('orders', OrdersData.Clone);

        AResponse.ContentType := 'application/json';
        AResponse.Content := CombinedResponse.AsJSON;
        AResponse.SendResponse;
      finally
        CombinedResponse.Free;
      end;
    finally
      if Assigned(UserData) then UserData.Free;
      if Assigned(OrdersData) then OrdersData.Free;
    end
    else
    begin
      AResponse.Code := 404;
      AResponse.Content := '{"error": "Data not found"}';
      AResponse.SendResponse;
    end;
  finally
    UserClient.Free;
    OrderClient.Free;
  end;
end;

var
  Gateway: TGateway;

begin
  Gateway := TGateway.Create;

  // Routes vers les services
  HTTPRouter.RegisterRoute('/gateway/api/users*', @Gateway.RouteToUserService);
  HTTPRouter.RegisterRoute('/gateway/api/orders*', @Gateway.RouteToOrderService);

  // Route d'agrégation
  HTTPRouter.RegisterRoute('/gateway/api/users/:id/orders', @Gateway.GetUserOrders);

  Application.Port := 8000;
  Application.Title := 'API Gateway';

  WriteLn('API Gateway démarré sur le port 8000');
  WriteLn('Routes disponibles :');
  WriteLn('  /gateway/api/users/*       - Service utilisateurs');
  WriteLn('  /gateway/api/orders/*      - Service commandes');
  WriteLn('  /gateway/api/users/:id/orders - Données agrégées');
  WriteLn('');

  Application.Initialize;
  Application.Run;
end.
```

## 9.7.5 Service Discovery

### Problème du hardcoding des URLs

Dans les exemples précédents, les URLs des services sont codées en dur. En production, les services peuvent changer d'adresse, être répliqués, ou tomber en panne.

### Registre de services simple

```pascal
unit ServiceRegistry;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, DateUtils, Generics.Collections, SyncObjs;

type
  TServiceInfo = class
  public
    Name: string;
    Host: string;
    Port: Integer;
    HealthCheckURL: string;
    LastHeartbeat: TDateTime;
    IsHealthy: Boolean;
    function GetURL: string;
  end;

  TServiceRegistry = class
  private
    FServices: TObjectDictionary<string, TObjectList<TServiceInfo>>;
    FLock: TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    procedure RegisterService(const ServiceName, Host: string; Port: Integer);
    procedure DeregisterService(const ServiceName, Host: string; Port: Integer);
    function GetService(const ServiceName: string): TServiceInfo;
    function GetAllInstances(const ServiceName: string): TObjectList<TServiceInfo>;
    procedure UpdateHeartbeat(const ServiceName, Host: string; Port: Integer);
    procedure MarkUnhealthy(const ServiceName, Host: string; Port: Integer);
  end;

implementation

function TServiceInfo.GetURL: string;  
begin
  Result := Format('http://%s:%d', [Host, Port]);
end;

constructor TServiceRegistry.Create;  
begin
  inherited Create;
  FServices := TObjectDictionary<string, TObjectList<TServiceInfo>>.Create([doOwnsValues]);
  FLock := TCriticalSection.Create;
end;

destructor TServiceRegistry.Destroy;  
begin
  FServices.Free;
  FLock.Free;
  inherited Destroy;
end;

procedure TServiceRegistry.RegisterService(const ServiceName, Host: string; Port: Integer);  
var
  ServiceList: TObjectList<TServiceInfo>;
  ServiceInfo: TServiceInfo;
begin
  FLock.Enter;
  try
    if not FServices.TryGetValue(ServiceName, ServiceList) then
    begin
      ServiceList := TObjectList<TServiceInfo>.Create(True);
      FServices.Add(ServiceName, ServiceList);
    end;

    ServiceInfo := TServiceInfo.Create;
    ServiceInfo.Name := ServiceName;
    ServiceInfo.Host := Host;
    ServiceInfo.Port := Port;
    ServiceInfo.HealthCheckURL := Format('http://%s:%d/health', [Host, Port]);
    ServiceInfo.LastHeartbeat := Now;
    ServiceInfo.IsHealthy := True;

    ServiceList.Add(ServiceInfo);

    WriteLn(Format('[Registry] Service enregistré: %s à %s:%d',
                   [ServiceName, Host, Port]));
  finally
    FLock.Leave;
  end;
end;

procedure TServiceRegistry.DeregisterService(const ServiceName, Host: string; Port: Integer);  
var
  ServiceList: TObjectList<TServiceInfo>;
  i: Integer;
begin
  FLock.Enter;
  try
    if FServices.TryGetValue(ServiceName, ServiceList) then
    begin
      for i := ServiceList.Count - 1 downto 0 do
      begin
        if (ServiceList[i].Host = Host) and (ServiceList[i].Port = Port) then
        begin
          ServiceList.Delete(i);
          WriteLn(Format('[Registry] Service désenregistré: %s à %s:%d',
                        [ServiceName, Host, Port]));
          Break;
        end;
      end;
    end;
  finally
    FLock.Leave;
  end;
end;

function TServiceRegistry.GetService(const ServiceName: string): TServiceInfo;  
var
  ServiceList: TObjectList<TServiceInfo>;
  i, RandomIndex: Integer;
  HealthyServices: TList<Integer>;
begin
  Result := nil;
  FLock.Enter;
  try
    if FServices.TryGetValue(ServiceName, ServiceList) and (ServiceList.Count > 0) then
    begin
      // Round-robin simple : trouver les services sains
      HealthyServices := TList<Integer>.Create;
      try
        for i := 0 to ServiceList.Count - 1 do
        begin
          if ServiceList[i].IsHealthy and
             (SecondsBetween(Now, ServiceList[i].LastHeartbeat) < 30) then
            HealthyServices.Add(i);
        end;

        if HealthyServices.Count > 0 then
        begin
          // Choisir un service aléatoirement parmi les services sains (Load Balancing simple)
          RandomIndex := Random(HealthyServices.Count);
          Result := ServiceList[HealthyServices[RandomIndex]];
        end;
      finally
        HealthyServices.Free;
      end;
    end;
  finally
    FLock.Leave;
  end;
end;

function TServiceRegistry.GetAllInstances(const ServiceName: string): TObjectList<TServiceInfo>;  
begin
  Result := nil;
  FLock.Enter;
  try
    if FServices.ContainsKey(ServiceName) then
      Result := FServices[ServiceName];
  finally
    FLock.Leave;
  end;
end;

procedure TServiceRegistry.UpdateHeartbeat(const ServiceName, Host: string; Port: Integer);  
var
  ServiceList: TObjectList<TServiceInfo>;
  i: Integer;
begin
  FLock.Enter;
  try
    if FServices.TryGetValue(ServiceName, ServiceList) then
    begin
      for i := 0 to ServiceList.Count - 1 do
      begin
        if (ServiceList[i].Host = Host) and (ServiceList[i].Port = Port) then
        begin
          ServiceList[i].LastHeartbeat := Now;
          ServiceList[i].IsHealthy := True;
          Break;
        end;
      end;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TServiceRegistry.MarkUnhealthy(const ServiceName, Host: string; Port: Integer);  
var
  ServiceList: TObjectList<TServiceInfo>;
  i: Integer;
begin
  FLock.Enter;
  try
    if FServices.TryGetValue(ServiceName, ServiceList) then
    begin
      for i := 0 to ServiceList.Count - 1 do
      begin
        if (ServiceList[i].Host = Host) and (ServiceList[i].Port = Port) then
        begin
          ServiceList[i].IsHealthy := False;
          WriteLn(Format('[Registry] Service marqué comme non sain: %s à %s:%d',
                        [ServiceName, Host, Port]));
          Break;
        end;
      end;
    end;
  finally
    FLock.Leave;
  end;
end;

end.
```

### Service Registry REST API

```pascal
program RegistryService;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, fphttpapp, httpdefs, httproute, fpjson, ServiceRegistry;

var
  Registry: TServiceRegistry;

procedure RegisterServiceEndpoint(ARequest: TRequest; AResponse: TResponse);  
var
  InputJSON: TJSONObject;
  ServiceName, Host: string;
  Port: Integer;
begin
  try
    InputJSON := GetJSON(ARequest.Content) as TJSONObject;
    try
      ServiceName := InputJSON.Get('service_name', '');
      Host := InputJSON.Get('host', '');
      Port := InputJSON.Get('port', 0);

      if (ServiceName = '') or (Host = '') or (Port <= 0) then
      begin
        AResponse.Code := 400;
        AResponse.Content := '{"error": "Invalid registration data"}';
        AResponse.SendResponse;
        Exit;
      end;

      Registry.RegisterService(ServiceName, Host, Port);

      AResponse.Code := 201;
      AResponse.ContentType := 'application/json';
      AResponse.Content := '{"status": "registered"}';
      AResponse.SendResponse;
    finally
      InputJSON.Free;
    end;
  except
    on E: Exception do
    begin
      AResponse.Code := 500;
      AResponse.Content := Format('{"error": "%s"}', [E.Message]);
      AResponse.SendResponse;
    end;
  end;
end;

procedure DiscoverServiceEndpoint(ARequest: TRequest; AResponse: TResponse);  
var
  ServiceName: string;
  ServiceInfo: TServiceInfo;
  ResponseJSON: TJSONObject;
begin
  ServiceName := ARequest.RouteParams['name'];

  ServiceInfo := Registry.GetService(ServiceName);

  if Assigned(ServiceInfo) then
  begin
    ResponseJSON := TJSONObject.Create;
    try
      ResponseJSON.Add('service_name', ServiceInfo.Name);
      ResponseJSON.Add('host', ServiceInfo.Host);
      ResponseJSON.Add('port', ServiceInfo.Port);
      ResponseJSON.Add('url', ServiceInfo.GetURL);

      AResponse.ContentType := 'application/json';
      AResponse.Content := ResponseJSON.AsJSON;
      AResponse.SendResponse;
    finally
      ResponseJSON.Free;
    end;
  end
  else
  begin
    AResponse.Code := 404;
    AResponse.Content := '{"error": "Service not found"}';
    AResponse.SendResponse;
  end;
end;

procedure HeartbeatEndpoint(ARequest: TRequest; AResponse: TResponse);  
var
  InputJSON: TJSONObject;
  ServiceName, Host: string;
  Port: Integer;
begin
  try
    InputJSON := GetJSON(ARequest.Content) as TJSONObject;
    try
      ServiceName := InputJSON.Get('service_name', '');
      Host := InputJSON.Get('host', '');
      Port := InputJSON.Get('port', 0);

      Registry.UpdateHeartbeat(ServiceName, Host, Port);

      AResponse.ContentType := 'application/json';
      AResponse.Content := '{"status": "ok"}';
      AResponse.SendResponse;
    finally
      InputJSON.Free;
    end;
  except
    on E: Exception do
    begin
      AResponse.Code := 500;
      AResponse.Content := Format('{"error": "%s"}', [E.Message]);
      AResponse.SendResponse;
    end;
  end;
end;

begin
  Registry := TServiceRegistry.Create;

  HTTPRouter.RegisterRoute('/registry/register', rmPost, @RegisterServiceEndpoint);
  HTTPRouter.RegisterRoute('/registry/discover/:name', @DiscoverServiceEndpoint);
  HTTPRouter.RegisterRoute('/registry/heartbeat', rmPost, @HeartbeatEndpoint);

  Application.Port := 8500;
  Application.Title := 'Service Registry';

  WriteLn('Service Registry démarré sur le port 8500');
  WriteLn('Endpoints :');
  WriteLn('  POST /registry/register       - Enregistrer un service');
  WriteLn('  GET  /registry/discover/:name - Découvrir un service');
  WriteLn('  POST /registry/heartbeat      - Heartbeat');
  WriteLn('');

  Application.Initialize;
  Application.Run;

  Registry.Free;
end.
```

## 9.7.6 Gestion des erreurs et résilience

### Circuit Breaker Pattern

Le Circuit Breaker empêche les appels répétés à un service défaillant, permettant une récupération plus rapide.

```pascal
unit CircuitBreaker;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, SyncObjs, DateUtils;

type
  TCircuitState = (csClosed, csOpen, csHalfOpen);

  TCircuitBreaker = class
  private
    FServiceName: string;
    FState: TCircuitState;
    FFailureCount: Integer;
    FFailureThreshold: Integer;
    FTimeout: Integer; // Secondes avant de tester à nouveau
    FLastFailureTime: TDateTime;
    FLock: TCriticalSection;
    function ShouldAttemptReset: Boolean;
  public
    constructor Create(const ServiceName: string;
                      FailureThreshold: Integer = 5;
                      TimeoutSeconds: Integer = 60);
    destructor Destroy; override;
    function CanExecute: Boolean;
    procedure RecordSuccess;
    procedure RecordFailure;
    function GetState: TCircuitState;
    property ServiceName: string read FServiceName;
  end;

implementation

constructor TCircuitBreaker.Create(const ServiceName: string;
                                   FailureThreshold: Integer;
                                   TimeoutSeconds: Integer);
begin
  inherited Create;
  FServiceName := ServiceName;
  FState := csClosed;
  FFailureCount := 0;
  FFailureThreshold := FailureThreshold;
  FTimeout := TimeoutSeconds;
  FLastFailureTime := 0;
  FLock := TCriticalSection.Create;
end;

destructor TCircuitBreaker.Destroy;  
begin
  FLock.Free;
  inherited Destroy;
end;

function TCircuitBreaker.ShouldAttemptReset: Boolean;  
begin
  Result := SecondsBetween(Now, FLastFailureTime) >= FTimeout;
end;

function TCircuitBreaker.CanExecute: Boolean;  
begin
  FLock.Enter;
  try
    case FState of
      csClosed:
        Result := True;

      csOpen:
        begin
          if ShouldAttemptReset then
          begin
            WriteLn(Format('[CircuitBreaker] %s: Passage en Half-Open', [FServiceName]));
            FState := csHalfOpen;
            Result := True;
          end
          else
            Result := False;
        end;

      csHalfOpen:
        Result := True;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TCircuitBreaker.RecordSuccess;  
begin
  FLock.Enter;
  try
    FFailureCount := 0;

    if FState = csHalfOpen then
    begin
      WriteLn(Format('[CircuitBreaker] %s: Réinitialisation - Passage en Closed', [FServiceName]));
      FState := csClosed;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TCircuitBreaker.RecordFailure;  
begin
  FLock.Enter;
  try
    Inc(FFailureCount);
    FLastFailureTime := Now;

    if FState = csHalfOpen then
    begin
      WriteLn(Format('[CircuitBreaker] %s: Échec en Half-Open - Réouverture', [FServiceName]));
      FState := csOpen;
      FFailureCount := 0;
    end
    else if (FState = csClosed) and (FFailureCount >= FFailureThreshold) then
    begin
      WriteLn(Format('[CircuitBreaker] %s: Seuil atteint (%d échecs) - Circuit ouvert',
                    [FServiceName, FFailureCount]));
      FState := csOpen;
      FFailureCount := 0;
    end;
  finally
    FLock.Leave;
  end;
end;

function TCircuitBreaker.GetState: TCircuitState;  
begin
  FLock.Enter;
  try
    Result := FState;
  finally
    FLock.Leave;
  end;
end;

end.
```

### Client HTTP avec Circuit Breaker

```pascal
unit ResilientServiceClient;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fphttpclient, fpjson, CircuitBreaker;

type
  TResilientServiceClient = class
  private
    FBaseURL: string;
    FCircuitBreaker: TCircuitBreaker;
    FTimeout: Integer;
    FRetryCount: Integer;
  public
    constructor Create(const BaseURL: string; const ServiceName: string);
    destructor Destroy; override;
    function Get(const Endpoint: string): TJSONData;
    function Post(const Endpoint: string; const Data: TJSONObject): TJSONData;
    property RetryCount: Integer read FRetryCount write FRetryCount;
  end;

implementation

constructor TResilientServiceClient.Create(const BaseURL: string;
                                           const ServiceName: string);
begin
  inherited Create;
  FBaseURL := BaseURL;
  FTimeout := 5000;
  FRetryCount := 3;
  FCircuitBreaker := TCircuitBreaker.Create(ServiceName, 5, 60);
end;

destructor TResilientServiceClient.Destroy;  
begin
  FCircuitBreaker.Free;
  inherited Destroy;
end;

function TResilientServiceClient.Get(const Endpoint: string): TJSONData;  
var
  HTTPClient: TFPHTTPClient;
  Response: string;
  Attempt: Integer;
  Success: Boolean;
begin
  Result := nil;

  // Vérifier si le circuit breaker autorise l'exécution
  if not FCircuitBreaker.CanExecute then
  begin
    WriteLn(Format('[Client] Circuit ouvert pour %s - Requête bloquée',
                  [FCircuitBreaker.ServiceName]));
    Exit;
  end;

  Success := False;
  HTTPClient := TFPHTTPClient.Create(nil);
  try
    HTTPClient.ConnectTimeout := FTimeout;
    HTTPClient.IOTimeout := FTimeout;

    for Attempt := 1 to FRetryCount do
    begin
      try
        Response := HTTPClient.Get(FBaseURL + Endpoint);

        if Response <> '' then
          Result := GetJSON(Response);

        Success := True;
        FCircuitBreaker.RecordSuccess;
        Break;
      except
        on E: Exception do
        begin
          WriteLn(Format('[Client] Échec tentative %d/%d: %s',
                        [Attempt, FRetryCount, E.Message]));

          if Attempt = FRetryCount then
          begin
            FCircuitBreaker.RecordFailure;
            raise;
          end;

          Sleep(1000 * Attempt); // Backoff exponentiel
        end;
      end;
    end;
  finally
    HTTPClient.Free;
  end;
end;

function TResilientServiceClient.Post(const Endpoint: string;
                                      const Data: TJSONObject): TJSONData;
var
  HTTPClient: TFPHTTPClient;
  Response: string;
  RequestBody: TStringStream;
  Attempt: Integer;
  Success: Boolean;
begin
  Result := nil;

  if not FCircuitBreaker.CanExecute then
  begin
    WriteLn(Format('[Client] Circuit ouvert pour %s - Requête bloquée',
                  [FCircuitBreaker.ServiceName]));
    Exit;
  end;

  Success := False;
  HTTPClient := TFPHTTPClient.Create(nil);
  try
    HTTPClient.ConnectTimeout := FTimeout;
    HTTPClient.IOTimeout := FTimeout;

    for Attempt := 1 to FRetryCount do
    begin
      try
        RequestBody := TStringStream.Create(Data.AsJSON);
        try
          HTTPClient.RequestBody := RequestBody;
          HTTPClient.AddHeader('Content-Type', 'application/json');

          Response := HTTPClient.Post(FBaseURL + Endpoint);

          if Response <> '' then
            Result := GetJSON(Response);

          Success := True;
          FCircuitBreaker.RecordSuccess;
          Break;
        finally
          RequestBody.Free;
        end;
      except
        on E: Exception do
        begin
          WriteLn(Format('[Client] Échec tentative %d/%d: %s',
                        [Attempt, FRetryCount, E.Message]));

          if Attempt = FRetryCount then
          begin
            FCircuitBreaker.RecordFailure;
            raise;
          end;

          Sleep(1000 * Attempt);
        end;
      end;
    end;
  finally
    HTTPClient.Free;
  end;
end;

end.
```

## 9.7.7 Gestion de la configuration

### Configuration centralisée

```pascal
unit ConfigService;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, DateUtils, fpjson, fphttpclient;

type
  TConfigManager = class
  private
    FConfigServerURL: string;
    FServiceName: string;
    FEnvironment: string;
    FCache: TJSONObject;
    FCacheTime: TDateTime;
    FCacheDuration: Integer; // En secondes
    function FetchConfig: TJSONObject;
  public
    constructor Create(const ConfigServerURL, ServiceName, Environment: string);
    destructor Destroy; override;
    function GetString(const Key, DefaultValue: string): string;
    function GetInteger(const Key: string; DefaultValue: Integer): Integer;
    function GetBoolean(const Key: string; DefaultValue: Boolean): Boolean;
    procedure Refresh;
    property CacheDuration: Integer read FCacheDuration write FCacheDuration;
  end;

implementation

constructor TConfigManager.Create(const ConfigServerURL, ServiceName,
                                  Environment: string);
begin
  inherited Create;
  FConfigServerURL := ConfigServerURL;
  FServiceName := ServiceName;
  FEnvironment := Environment;
  FCache := nil;
  FCacheTime := 0;
  FCacheDuration := 300; // 5 minutes par défaut

  // Charger la configuration initiale
  Refresh;
end;

destructor TConfigManager.Destroy;  
begin
  if Assigned(FCache) then
    FCache.Free;
  inherited Destroy;
end;

function TConfigManager.FetchConfig: TJSONObject;  
var
  HTTPClient: TFPHTTPClient;
  Response: string;
  URL: string;
begin
  Result := nil;
  HTTPClient := TFPHTTPClient.Create(nil);
  try
    URL := Format('%s/config/%s/%s', [FConfigServerURL, FServiceName, FEnvironment]);

    try
      Response := HTTPClient.Get(URL);
      if Response <> '' then
        Result := GetJSON(Response) as TJSONObject;
    except
      on E: Exception do
      begin
        WriteLn(Format('[Config] Erreur lors de la récupération: %s', [E.Message]));
        // En cas d'erreur, utiliser le cache existant
        if Assigned(FCache) then
          Result := TJSONObject(FCache.Clone);
      end;
    end;
  finally
    HTTPClient.Free;
  end;
end;

procedure TConfigManager.Refresh;  
var
  NewConfig: TJSONObject;
begin
  // Vérifier si le cache est toujours valide
  if Assigned(FCache) and (SecondsBetween(Now, FCacheTime) < FCacheDuration) then
    Exit;

  NewConfig := FetchConfig;

  if Assigned(NewConfig) then
  begin
    if Assigned(FCache) then
      FCache.Free;

    FCache := NewConfig;
    FCacheTime := Now;
    WriteLn('[Config] Configuration mise à jour');
  end;
end;

function TConfigManager.GetString(const Key, DefaultValue: string): string;  
begin
  Refresh;

  if Assigned(FCache) and (FCache.IndexOfName(Key) >= 0) then
    Result := FCache.Get(Key, DefaultValue)
  else
    Result := DefaultValue;
end;

function TConfigManager.GetInteger(const Key: string; DefaultValue: Integer): Integer;  
begin
  Refresh;

  if Assigned(FCache) and (FCache.IndexOfName(Key) >= 0) then
    Result := FCache.Get(Key, DefaultValue)
  else
    Result := DefaultValue;
end;

function TConfigManager.GetBoolean(const Key: string; DefaultValue: Boolean): Boolean;  
begin
  Refresh;

  if Assigned(FCache) and (FCache.IndexOfName(Key) >= 0) then
    Result := FCache.Get(Key, DefaultValue)
  else
    Result := DefaultValue;
end;

end.
```

### Utilisation de la configuration

```pascal
program ConfigurableService;

{$mode objfpc}{$H+}

uses
  SysUtils, fphttpapp, httpdefs, httproute, ConfigService;

var
  Config: TConfigManager;

procedure HandleRequest(ARequest: TRequest; AResponse: TResponse);  
var
  MaxConnections: Integer;
  EnableLogging: Boolean;
  DatabaseURL: string;
begin
  // Lire la configuration
  MaxConnections := Config.GetInteger('max_connections', 100);
  EnableLogging := Config.GetBoolean('enable_logging', True);
  DatabaseURL := Config.GetString('database_url', 'localhost:5432');

  if EnableLogging then
    WriteLn(Format('[Service] Requête traitée - Max connections: %d', [MaxConnections]));

  AResponse.Content := Format('Service configuré: DB=%s, MaxConn=%d',
                             [DatabaseURL, MaxConnections]);
  AResponse.SendResponse;
end;

begin
  // Initialiser la configuration
  {$IFDEF PRODUCTION}
  Config := TConfigManager.Create('http://config-server:8600', 'my-service', 'production');
  {$ELSE}
  Config := TConfigManager.Create('http://localhost:8600', 'my-service', 'development');
  {$ENDIF}

  try
    HTTPRouter.RegisterRoute('/api/data', @HandleRequest);

    Application.Port := Config.GetInteger('service_port', 8001);
    Application.Title := 'Configurable Service';

    WriteLn(Format('Service démarré sur le port %d', [Application.Port]));

    Application.Initialize;
    Application.Run;
  finally
    Config.Free;
  end;
end.
```

## 9.7.8 Logging distribué

### Logger centralisé

```pascal
unit DistributedLogger;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpjson, fphttpclient, SyncObjs;

type
  TLogLevel = (llDebug, llInfo, llWarning, llError, llCritical);

  TLogEntry = class
  public
    Timestamp: TDateTime;
    Level: TLogLevel;
    ServiceName: string;
    ServiceInstance: string;
    Message: string;
    TraceID: string;
    Metadata: TJSONObject;
    function ToJSON: TJSONObject;
  end;

  TDistributedLogger = class
  private
    FLogServerURL: string;
    FServiceName: string;
    FServiceInstance: string;
    FQueue: TThreadList;
    FFlushThread: TThread;
    FRunning: Boolean;
    procedure FlushLogs;
  public
    constructor Create(const LogServerURL, ServiceName, ServiceInstance: string);
    destructor Destroy; override;
    procedure Log(Level: TLogLevel; const Message: string; const TraceID: string = '');
    procedure Debug(const Message: string; const TraceID: string = '');
    procedure Info(const Message: string; const TraceID: string = '');
    procedure Warning(const Message: string; const TraceID: string = '');
    procedure Error(const Message: string; const TraceID: string = '');
    procedure Critical(const Message: string; const TraceID: string = '');
  end;

implementation

function TLogEntry.ToJSON: TJSONObject;  
const
  LevelNames: array[TLogLevel] of string = ('DEBUG', 'INFO', 'WARNING', 'ERROR', 'CRITICAL');
begin
  Result := TJSONObject.Create;
  Result.Add('timestamp', FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Timestamp));
  Result.Add('level', LevelNames[Level]);
  Result.Add('service', ServiceName);
  Result.Add('instance', ServiceInstance);
  Result.Add('message', Message);
  if TraceID <> '' then
    Result.Add('trace_id', TraceID);
end;

constructor TDistributedLogger.Create(const LogServerURL, ServiceName,
                                      ServiceInstance: string);
begin
  inherited Create;
  FLogServerURL := LogServerURL;
  FServiceName := ServiceName;
  FServiceInstance := ServiceInstance;
  FQueue := TThreadList.Create;
  FRunning := True;

  // Créer un thread pour l'envoi asynchrone des logs
  // (implémentation simplifiée - en production, utiliser un vrai thread)
end;

destructor TDistributedLogger.Destroy;  
begin
  FRunning := False;
  FlushLogs;
  FQueue.Free;
  inherited Destroy;
end;

procedure TDistributedLogger.FlushLogs;  
var
  HTTPClient: TFPHTTPClient;
  LogList: TList;
  LogArray: TJSONArray;
  i: Integer;
  LogEntry: TLogEntry;
  RequestBody: TStringStream;
begin
  LogList := FQueue.LockList;
  try
    if LogList.Count = 0 then
      Exit;

    LogArray := TJSONArray.Create;
    try
      for i := 0 to LogList.Count - 1 do
      begin
        LogEntry := TLogEntry(LogList[i]);
        LogArray.Add(LogEntry.ToJSON);
        LogEntry.Free;
      end;
      LogList.Clear;

      // Envoyer les logs au serveur
      HTTPClient := TFPHTTPClient.Create(nil);
      try
        RequestBody := TStringStream.Create(LogArray.AsJSON);
        try
          HTTPClient.RequestBody := RequestBody;
          HTTPClient.AddHeader('Content-Type', 'application/json');
          HTTPClient.Post(FLogServerURL + '/logs');
        finally
          RequestBody.Free;
        end;
      finally
        HTTPClient.Free;
      end;
    finally
      LogArray.Free;
    end;
  finally
    FQueue.UnlockList;
  end;
end;

procedure TDistributedLogger.Log(Level: TLogLevel; const Message: string;
                                 const TraceID: string);
var
  LogEntry: TLogEntry;
  LogList: TList;
begin
  LogEntry := TLogEntry.Create;
  LogEntry.Timestamp := Now;
  LogEntry.Level := Level;
  LogEntry.ServiceName := FServiceName;
  LogEntry.ServiceInstance := FServiceInstance;
  LogEntry.Message := Message;
  LogEntry.TraceID := TraceID;

  LogList := FQueue.LockList;
  try
    LogList.Add(LogEntry);

    // Flush si la queue est pleine
    if LogList.Count >= 100 then
      FlushLogs;
  finally
    FQueue.UnlockList;
  end;
end;

procedure TDistributedLogger.Debug(const Message: string; const TraceID: string);  
begin
  Log(llDebug, Message, TraceID);
end;

procedure TDistributedLogger.Info(const Message: string; const TraceID: string);  
begin
  Log(llInfo, Message, TraceID);
end;

procedure TDistributedLogger.Warning(const Message: string; const TraceID: string);  
begin
  Log(llWarning, Message, TraceID);
end;

procedure TDistributedLogger.Error(const Message: string; const TraceID: string);  
begin
  Log(llError, Message, TraceID);
end;

procedure TDistributedLogger.Critical(const Message: string; const TraceID: string);  
begin
  Log(llCritical, Message, TraceID);
end;

end.
```

## 9.7.9 Traçabilité distribuée (Distributed Tracing)

### Génération et propagation de Trace ID

```pascal
unit DistributedTracing;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, DateUtils;

type
  TTraceContext = class
  private
    FTraceID: string;
    FSpanID: string;
    FParentSpanID: string;
    FServiceName: string;
  public
    constructor Create(const ServiceName: string);
    constructor CreateWithParent(const ServiceName, TraceID, ParentSpanID: string);
    function GenerateSpanID: string;
    property TraceID: string read FTraceID;
    property SpanID: string read FSpanID;
    property ParentSpanID: string read FParentSpanID;
    property ServiceName: string read FServiceName;
  end;

  TSpan = class
  private
    FTraceID: string;
    FSpanID: string;
    FParentSpanID: string;
    FServiceName: string;
    FOperationName: string;
    FStartTime: TDateTime;
    FEndTime: TDateTime;
    FTags: TStringList;
  public
    constructor Create(const Context: TTraceContext; const OperationName: string);
    destructor Destroy; override;
    procedure Finish;
    procedure AddTag(const Key, Value: string);
    function GetDuration: Int64; // En millisecondes
  end;

function GenerateTraceID: string;  
function GenerateSpanID: string;

implementation

function GenerateTraceID: string;  
var
  GUID: TGUID;
begin
  CreateGUID(GUID);
  Result := GUIDToString(GUID);
  Result := StringReplace(Result, '-', '', [rfReplaceAll]);
  Result := StringReplace(Result, '{', '', [rfReplaceAll]);
  Result := StringReplace(Result, '}', '', [rfReplaceAll]);
  Result := LowerCase(Result);
end;

function GenerateSpanID: string;  
begin
  Result := IntToHex(Random(MaxInt), 16);
end;

constructor TTraceContext.Create(const ServiceName: string);  
begin
  inherited Create;
  FServiceName := ServiceName;
  FTraceID := GenerateTraceID;
  FSpanID := GenerateSpanID;
  FParentSpanID := '';
end;

constructor TTraceContext.CreateWithParent(const ServiceName, TraceID,
                                           ParentSpanID: string);
begin
  inherited Create;
  FServiceName := ServiceName;
  FTraceID := TraceID;
  FParentSpanID := ParentSpanID;
  FSpanID := GenerateSpanID;
end;

function TTraceContext.GenerateSpanID: string;  
begin
  Result := IntToHex(Random(MaxInt), 16);
end;

constructor TSpan.Create(const Context: TTraceContext; const OperationName: string);  
begin
  inherited Create;
  FTraceID := Context.TraceID;
  FSpanID := Context.SpanID;
  FParentSpanID := Context.ParentSpanID;
  FServiceName := Context.ServiceName;
  FOperationName := OperationName;
  FStartTime := Now;
  FTags := TStringList.Create;
end;

destructor TSpan.Destroy;  
begin
  FTags.Free;
  inherited Destroy;
end;

procedure TSpan.Finish;  
begin
  FEndTime := Now;

  // En production, envoyer le span à un système de tracing (Jaeger, Zipkin)
  WriteLn(Format('[Trace] %s - %s: %d ms (TraceID: %s, SpanID: %s)',
                [FServiceName, FOperationName, GetDuration, FTraceID, FSpanID]));
end;

procedure TSpan.AddTag(const Key, Value: string);  
begin
  FTags.Add(Format('%s=%s', [Key, Value]));
end;

function TSpan.GetDuration: Int64;  
begin
  if FEndTime = 0 then
    Result := MilliSecondsBetween(Now, FStartTime)
  else
    Result := MilliSecondsBetween(FEndTime, FStartTime);
end;

end.
```

### Utilisation du tracing dans les microservices

```pascal
program TracedService;

{$mode objfpc}{$H+}

uses
  SysUtils, fphttpapp, httpdefs, httproute, fpjson,
  DistributedTracing, ServiceClient;

procedure HandleTracedRequest(ARequest: TRequest; AResponse: TResponse);  
var
  TraceID, ParentSpanID: string;
  Context: TTraceContext;
  Span: TSpan;
  Client: TServiceClient;
  UserData: TJSONData;
begin
  // Extraire le contexte de tracing depuis les headers
  TraceID := ARequest.GetCustomHeader('X-Trace-ID');
  ParentSpanID := ARequest.GetCustomHeader('X-Span-ID');

  // Créer le contexte de tracing
  if TraceID <> '' then
    Context := TTraceContext.CreateWithParent('order-service', TraceID, ParentSpanID)
  else
    Context := TTraceContext.Create('order-service');

  try
    // Créer un span pour cette opération
    Span := TSpan.Create(Context, 'ProcessOrder');
    try
      Span.AddTag('http.method', ARequest.Method);
      Span.AddTag('http.url', ARequest.URI);

      // Appeler un autre service avec propagation du contexte
      Client := TServiceClient.Create('http://localhost:8001');
      try
        // Ajouter les headers de tracing
        // (Dans une vraie implémentation, modifier TServiceClient pour supporter cela)
        UserData := Client.Get('/api/users/1');

        if Assigned(UserData) then
        begin
          Span.AddTag('user.found', 'true');
          UserData.Free;
        end
        else
          Span.AddTag('user.found', 'false');
      finally
        Client.Free;
      end;

      AResponse.Content := '{"status": "ok"}';
      AResponse.SendResponse;

      Span.AddTag('http.status', IntToStr(AResponse.Code));
      Span.Finish;
    finally
      Span.Free;
    end;
  finally
    Context.Free;
  end;
end;

begin
  HTTPRouter.RegisterRoute('/api/process', @HandleTracedRequest);

  Application.Port := 8002;
  Application.Initialize;
  Application.Run;
end.
```

## 9.7.10 Déploiement avec Docker

### Dockerfile pour un microservice FreePascal

**Windows (Dockerfile.windows) :**

```dockerfile
# Dockerfile pour Windows
FROM mcr.microsoft.com/windows/servercore:ltsc2019

# Installer FreePascal
RUN powershell -Command \
    Invoke-WebRequest -Uri "https://sourceforge.net/projects/freepascal/files/Win32/3.2.2/fpc-3.2.2.i386-win32.exe" \
    -OutFile "fpc-setup.exe"; \
    Start-Process -FilePath "fpc-setup.exe" -ArgumentList "/VERYSILENT" -Wait

# Copier le code source
COPY . /app  
WORKDIR /app

# Compiler le service
RUN fpc UserService.pas

# Exposer le port
EXPOSE 8001

# Démarrer le service
CMD ["UserService.exe"]
```

**Ubuntu (Dockerfile) :**

```dockerfile
# Dockerfile pour Linux/Ubuntu
FROM ubuntu:22.04

# Installer FreePascal
RUN apt-get update && \
    apt-get install -y fpc && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

# Créer le répertoire de travail
WORKDIR /app

# Copier le code source
COPY . .

# Compiler le service
RUN fpc -O3 UserService.pas

# Exposer le port
EXPOSE 8001

# Créer un utilisateur non-root
RUN useradd -m -u 1000 appuser && \
    chown -R appuser:appuser /app

USER appuser

# Démarrer le service
CMD ["./UserService"]
```

### Docker Compose pour l'orchestration

```yaml
version: '3.8'

services:
  registry:
    build:
      context: ./registry
      dockerfile: Dockerfile
    ports:
      - "8500:8500"
    networks:
      - microservices
    healthcheck:
      test: ["CMD", "curl", "-f", "http://localhost:8500/health"]
      interval: 10s
      timeout: 5s
      retries: 3

  user-service:
    build:
      context: ./user-service
      dockerfile: Dockerfile
    ports:
      - "8001:8001"
    environment:
      - SERVICE_NAME=user-service
      - REGISTRY_URL=http://registry:8500
      - DB_HOST=postgres
      - DB_PORT=5432
    depends_on:
      - registry
      - postgres
    networks:
      - microservices
    deploy:
      replicas: 2
      restart_policy:
        condition: on-failure

  order-service:
    build:
      context: ./order-service
      dockerfile: Dockerfile
    ports:
      - "8002:8002"
    environment:
      - SERVICE_NAME=order-service
      - REGISTRY_URL=http://registry:8500
      - USER_SERVICE_URL=http://user-service:8001
    depends_on:
      - registry
      - user-service
    networks:
      - microservices
    deploy:
      replicas: 2

  api-gateway:
    build:
      context: ./gateway
      dockerfile: Dockerfile
    ports:
      - "8000:8000"
    environment:
      - REGISTRY_URL=http://registry:8500
    depends_on:
      - registry
      - user-service
      - order-service
    networks:
      - microservices

  postgres:
    image: postgres:15
    environment:
      POSTGRES_DB: microservices
      POSTGRES_USER: admin
      POSTGRES_PASSWORD: secret
    volumes:
      - postgres-data:/var/lib/postgresql/data
    networks:
      - microservices

  redis:
    image: redis:7-alpine
    networks:
      - microservices

networks:
  microservices:
    driver: bridge

volumes:
  postgres-data:
```

### Scripts de déploiement

**build-all.sh (Linux/Ubuntu) :**

```bash
#!/bin/bash

echo "Compilation de tous les microservices..."

# User Service
cd user-service  
fpc -O3 UserService.pas  
cd ..

# Order Service
cd order-service  
fpc -O3 OrderService.pas  
cd ..

# API Gateway
cd gateway  
fpc -O3 APIGateway.pas  
cd ..

# Registry
cd registry  
fpc -O3 RegistryService.pas  
cd ..

echo "Compilation terminée!"
```

**build-all.bat (Windows) :**

```batch
@echo off
echo Compilation de tous les microservices...

cd user-service  
fpc -O3 UserService.pas  
cd ..

cd order-service  
fpc -O3 OrderService.pas  
cd ..

cd gateway  
fpc -O3 APIGateway.pas  
cd ..

cd registry  
fpc -O3 RegistryService.pas  
cd ..

echo Compilation terminée!
```

## 9.7.11 Monitoring et métriques

### Exposition des métriques Prometheus

```pascal
unit PrometheusMetrics;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, SyncObjs;

type
  TMetricType = (mtCounter, mtGauge, mtHistogram);

  TMetric = class
  private
    FName: string;
    FHelp: string;
    FMetricType: TMetricType;
    FValue: Double;
    FLock: TCriticalSection;
  public
    constructor Create(const Name, Help: string; MetricType: TMetricType);
    destructor Destroy; override;
    procedure Inc(Value: Double = 1.0);
    procedure Dec(Value: Double = 1.0);
    procedure SetValue(Value: Double);
    function GetValue: Double;
    function FormatMetric: string;
  end;

  TMetricsRegistry = class
  private
    FMetrics: TStringList;
    FLock: TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    function RegisterCounter(const Name, Help: string): TMetric;
    function RegisterGauge(const Name, Help: string): TMetric;
    function GetMetric(const Name: string): TMetric;
    function ExportMetrics: string;
  end;

implementation

constructor TMetric.Create(const Name, Help: string; MetricType: TMetricType);  
begin
  inherited Create;
  FName := Name;
  FHelp := Help;
  FMetricType := MetricType;
  FValue := 0;
  FLock := TCriticalSection.Create;
end;

destructor TMetric.Destroy;  
begin
  FLock.Free;
  inherited Destroy;
end;

procedure TMetric.Inc(Value: Double);  
begin
  FLock.Enter;
  try
    FValue := FValue + Value;
  finally
    FLock.Leave;
  end;
end;

procedure TMetric.Dec(Value: Double);  
begin
  FLock.Enter;
  try
    FValue := FValue - Value;
  finally
    FLock.Leave;
  end;
end;

procedure TMetric.SetValue(Value: Double);  
begin
  FLock.Enter;
  try
    FValue := Value;
  finally
    FLock.Leave;
  end;
end;

function TMetric.GetValue: Double;  
begin
  FLock.Enter;
  try
    Result := FValue;
  finally
    FLock.Leave;
  end;
end;

function TMetric.FormatMetric: string;  
const
  TypeNames: array[TMetricType] of string = ('counter', 'gauge', 'histogram');
begin
  Result := Format('# HELP %s %s' + LineEnding +
                  '# TYPE %s %s' + LineEnding +
                  '%s %.2f' + LineEnding,
                  [FName, FHelp, FName, TypeNames[FMetricType], FName, GetValue]);
end;

constructor TMetricsRegistry.Create;  
begin
  inherited Create;
  FMetrics := TStringList.Create;
  FMetrics.Sorted := True;
  FMetrics.OwnsObjects := True;
  FLock := TCriticalSection.Create;
end;

destructor TMetricsRegistry.Destroy;  
begin
  FMetrics.Free;
  FLock.Free;
  inherited Destroy;
end;

function TMetricsRegistry.RegisterCounter(const Name, Help: string): TMetric;  
begin
  FLock.Enter;
  try
    Result := TMetric.Create(Name, Help, mtCounter);
    FMetrics.AddObject(Name, Result);
  finally
    FLock.Leave;
  end;
end;

function TMetricsRegistry.RegisterGauge(const Name, Help: string): TMetric;  
begin
  FLock.Enter;
  try
    Result := TMetric.Create(Name, Help, mtGauge);
    FMetrics.AddObject(Name, Result);
  finally
    FLock.Leave;
  end;
end;

function TMetricsRegistry.GetMetric(const Name: string): TMetric;  
var
  Index: Integer;
begin
  Result := nil;
  FLock.Enter;
  try
    if FMetrics.Find(Name, Index) then
      Result := TMetric(FMetrics.Objects[Index]);
  finally
    FLock.Leave;
  end;
end;

function TMetricsRegistry.ExportMetrics: string;  
var
  i: Integer;
  Metric: TMetric;
begin
  Result := '';
  FLock.Enter;
  try
    for i := 0 to FMetrics.Count - 1 do
    begin
      Metric := TMetric(FMetrics.Objects[i]);
      Result := Result + Metric.FormatMetric;
    end;
  finally
    FLock.Leave;
  end;
end;

end.
```

### Service avec métriques intégrées

```pascal
program MonitoredService;

{$mode objfpc}{$H+}

uses
  SysUtils, DateUtils, fphttpapp, httpdefs, httproute, fpjson, PrometheusMetrics;

var
  Metrics: TMetricsRegistry;
  RequestCounter: TMetric;
  ActiveRequests: TMetric;
  ErrorCounter: TMetric;

procedure MetricsEndpoint(ARequest: TRequest; AResponse: TResponse);  
begin
  AResponse.ContentType := 'text/plain; version=0.0.4';
  AResponse.Content := Metrics.ExportMetrics;
  AResponse.SendResponse;
end;

procedure HandleRequest(ARequest: TRequest; AResponse: TResponse);  
var
  StartTime: TDateTime;
  Duration: Int64;
begin
  StartTime := Now;
  RequestCounter.Inc;
  ActiveRequests.Inc;

  try
    // Simuler un traitement
    Sleep(Random(100));

    AResponse.Content := '{"status": "ok"}';
    AResponse.SendResponse;
  except
    on E: Exception do
    begin
      ErrorCounter.Inc;
      AResponse.Code := 500;
      AResponse.Content := Format('{"error": "%s"}', [E.Message]);
      AResponse.SendResponse;
    end;
  end;

  ActiveRequests.Dec;

  Duration := MilliSecondsBetween(Now, StartTime);
  WriteLn(Format('[Request] Traité en %d ms', [Duration]));
end;

begin
  Metrics := TMetricsRegistry.Create;

  // Enregistrer les métriques
  RequestCounter := Metrics.RegisterCounter('http_requests_total',
                                           'Total des requêtes HTTP');
  ActiveRequests := Metrics.RegisterGauge('http_requests_active',
                                         'Requêtes HTTP en cours');
  ErrorCounter := Metrics.RegisterCounter('http_errors_total',
                                         'Total des erreurs HTTP');

  // Routes
  HTTPRouter.RegisterRoute('/metrics', @MetricsEndpoint);
  HTTPRouter.RegisterRoute('/api/data', @HandleRequest);

  Application.Port := 8001;
  Application.Title := 'Monitored Service';

  WriteLn('Service démarré sur le port 8001');
  WriteLn('Métriques disponibles sur /metrics');

  Application.Initialize;
  Application.Run;

  Metrics.Free;
end.
```

## 9.7.12 Patterns de communication asynchrone

### Message Queue avec un broker simple

```pascal
unit MessageBroker;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Generics.Collections, SyncObjs;

type
  TMessage = class
  public
    ID: string;
    Topic: string;
    Payload: string;
    Timestamp: TDateTime;
    constructor Create(const ATopic, APayload: string);
  end;

  TMessageHandler = procedure(const Message: TMessage) of object;

  TSubscriber = class
  public
    ID: string;
    Handler: TMessageHandler;
  end;

  TMessageBroker = class
  private
    FTopics: TObjectDictionary<string, TObjectList<TSubscriber>>;
    FMessageQueue: TThreadList;
    FLock: TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Publish(const Topic, Payload: string);
    function Subscribe(const Topic: string; Handler: TMessageHandler): string;
    procedure Unsubscribe(const SubscriberID: string);
    procedure ProcessMessages;
  end;

implementation

constructor TMessage.Create(const ATopic, APayload: string);  
begin
  inherited Create;
  ID := GenerateRandomString(16);
  Topic := ATopic;
  Payload := APayload;
  Timestamp := Now;
end;

constructor TMessageBroker.Create;  
begin
  inherited Create;
  FTopics := TObjectDictionary<string, TObjectList<TSubscriber>>.Create([doOwnsValues]);
  FMessageQueue := TThreadList.Create;
  FLock := TCriticalSection.Create;
end;

destructor TMessageBroker.Destroy;  
begin
  FTopics.Free;
  FMessageQueue.Free;
  FLock.Free;
  inherited Destroy;
end;

procedure TMessageBroker.Publish(const Topic, Payload: string);  
var
  Message: TMessage;
  MessageList: TList;
begin
  Message := TMessage.Create(Topic, Payload);

  MessageList := FMessageQueue.LockList;
  try
    MessageList.Add(Message);
  finally
    FMessageQueue.UnlockList;
  end;

  WriteLn(Format('[Broker] Message publié sur %s', [Topic]));
end;

function TMessageBroker.Subscribe(const Topic: string;
                                  Handler: TMessageHandler): string;
var
  SubscriberList: TObjectList<TSubscriber>;
  Subscriber: TSubscriber;
begin
  FLock.Enter;
  try
    if not FTopics.TryGetValue(Topic, SubscriberList) then
    begin
      SubscriberList := TObjectList<TSubscriber>.Create(True);
      FTopics.Add(Topic, SubscriberList);
    end;

    Subscriber := TSubscriber.Create;
    Subscriber.ID := GenerateRandomString(16);
    Subscriber.Handler := Handler;

    SubscriberList.Add(Subscriber);
    Result := Subscriber.ID;

    WriteLn(Format('[Broker] Abonnement créé pour %s', [Topic]));
  finally
    FLock.Leave;
  end;
end;

procedure TMessageBroker.Unsubscribe(const SubscriberID: string);  
var
  TopicPair: TPair<string, TObjectList<TSubscriber>>;
  i: Integer;
begin
  FLock.Enter;
  try
    for TopicPair in FTopics do
    begin
      for i := TopicPair.Value.Count - 1 downto 0 do
      begin
        if TopicPair.Value[i].ID = SubscriberID then
        begin
          TopicPair.Value.Delete(i);
          WriteLn(Format('[Broker] Désabonnement effectué: %s', [SubscriberID]));
          Exit;
        end;
      end;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TMessageBroker.ProcessMessages;  
var
  MessageList: TList;
  Message: TMessage;
  i: Integer;
  SubscriberList: TObjectList<TSubscriber>;
  j: Integer;
begin
  MessageList := FMessageQueue.LockList;
  try
    for i := MessageList.Count - 1 downto 0 do
    begin
      Message := TMessage(MessageList[i]);

      FLock.Enter;
      try
        if FTopics.TryGetValue(Message.Topic, SubscriberList) then
        begin
          for j := 0 to SubscriberList.Count - 1 do
          begin
            try
              SubscriberList[j].Handler(Message);
            except
              on E: Exception do
                WriteLn(Format('[Broker] Erreur dans le handler: %s', [E.Message]));
            end;
          end;
        end;
      finally
        FLock.Leave;
      end;

      Message.Free;
      MessageList.Delete(i);
    end;
  finally
    FMessageQueue.UnlockList;
  end;
end;

end.
```

### Exemple d'utilisation du broker

```pascal
program AsyncService;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, MessageBroker;

type
  TOrderService = class
  private
    FBroker: TMessageBroker;
  public
    constructor Create(ABroker: TMessageBroker);
    procedure CreateOrder(const UserID: Integer; const Product: string);
    procedure OnPaymentProcessed(const Message: TMessage);
  end;

  TPaymentService = class
  private
    FBroker: TMessageBroker;
  public
    constructor Create(ABroker: TMessageBroker);
    procedure OnOrderCreated(const Message: TMessage);
  end;

constructor TOrderService.Create(ABroker: TMessageBroker);  
begin
  inherited Create;
  FBroker := ABroker;

  // S'abonner aux événements de paiement
  FBroker.Subscribe('payment.processed', @OnPaymentProcessed);
end;

procedure TOrderService.CreateOrder(const UserID: Integer; const Product: string);  
var
  OrderData: string;
begin
  WriteLn(Format('[OrderService] Création commande: User=%d, Product=%s',
                [UserID, Product]));

  // Publier un événement
  OrderData := Format('{"user_id": %d, "product": "%s"}', [UserID, Product]);
  FBroker.Publish('order.created', OrderData);
end;

procedure TOrderService.OnPaymentProcessed(const Message: TMessage);  
begin
  WriteLn(Format('[OrderService] Paiement traité: %s', [Message.Payload]));
end;

constructor TPaymentService.Create(ABroker: TMessageBroker);  
begin
  inherited Create;
  FBroker := ABroker;

  // S'abonner aux événements de commande
  FBroker.Subscribe('order.created', @OnOrderCreated);
end;

procedure TPaymentService.OnOrderCreated(const Message: TMessage);  
begin
  WriteLn(Format('[PaymentService] Traitement paiement pour: %s', [Message.Payload]));

  // Simuler le traitement du paiement
  Sleep(1000);

  // Publier l'événement de paiement traité
  FBroker.Publish('payment.processed', '{"status": "success", "order_id": "123"}');
end;

var
  Broker: TMessageBroker;
  OrderService: TOrderService;
  PaymentService: TPaymentService;

begin
  WriteLn('Démarrage des services asynchrones...');
  WriteLn('');

  Broker := TMessageBroker.Create;
  try
    OrderService := TOrderService.Create(Broker);
    PaymentService := TPaymentService.Create(Broker);

    try
      // Créer une commande
      OrderService.CreateOrder(1, 'Laptop');

      // Traiter les messages
      Broker.ProcessMessages;

      WriteLn('');
      WriteLn('Appuyez sur Entrée pour terminer...');
      ReadLn;
    finally
      OrderService.Free;
      PaymentService.Free;
    end;
  finally
    Broker.Free;
  end;
end.
```

## 9.7.13 Bonnes pratiques et recommandations

### Principes de conception

**1. Single Responsibility Principle**
- Chaque microservice doit avoir une responsabilité unique et bien définie
- Un service = un domaine métier

**2. Indépendance des données**
- Chaque service gère sa propre base de données
- Pas d'accès direct aux bases de données d'autres services
- Communication uniquement via API

**3. Tolérance aux pannes**
```pascal
// Implémenter des mécanismes de fallback
function GetUserWithFallback(UserID: Integer): TUser;  
begin
  try
    Result := UserClient.GetUser(UserID);
  except
    on E: Exception do
    begin
      WriteLn('[Fallback] Utilisation du cache pour l''utilisateur ', UserID);
      Result := GetUserFromCache(UserID);
    end;
  end;
end;
```

**4. Observabilité**
- Logs structurés avec contexte
- Métriques pour chaque service
- Tracing distribué pour suivre les requêtes
- Health checks sur tous les services

### Architecture multi-plateforme

**Considérations Windows :**
```pascal
{$IFDEF WINDOWS}
uses
  Windows;

procedure ConfigureWindowsService;  
begin
  // Configuration spécifique Windows
  SetProcessWorkingSetSize(GetCurrentProcess, $FFFFFFFF, $FFFFFFFF);
end;
{$ENDIF}
```

**Considérations Ubuntu/Linux :**
```pascal
{$IFDEF LINUX}
uses
  BaseUnix;

procedure ConfigureLinuxService;  
begin
  // Configuration spécifique Linux
  FpSetrlimit(RLIMIT_NOFILE, 65536); // Augmenter limite fichiers ouverts
end;
{$ENDIF}
```

### Liste de contrôle pour la production

**Sécurité :**
- ✅ Authentification entre services (mTLS ou tokens JWT)
- ✅ Chiffrement des communications (HTTPS)
- ✅ Validation des entrées
- ✅ Gestion sécurisée des secrets
- ✅ Rate limiting sur toutes les API

**Performance :**
- ✅ Connection pooling pour les bases de données
- ✅ Caching des données fréquemment accédées
- ✅ Compression des réponses HTTP
- ✅ Timeouts configurés sur toutes les requêtes

**Fiabilité :**
- ✅ Health checks sur tous les services
- ✅ Circuit breakers pour les appels externes
- ✅ Retry avec backoff exponentiel
- ✅ Graceful shutdown
- ✅ Réplication des services critiques

## Conclusion

Les microservices avec FreePascal offrent de nombreux avantages :

**Points forts :**
- Exécutables autonomes et légers
- Performance native excellente
- Faible consommation mémoire
- Compilation rapide
- Portabilité Windows/Linux

**Défis à relever :**
- Écosystème de bibliothèques moins riche que d'autres langages
- Courbe d'apprentissage pour l'architecture distribuée
- Complexité opérationnelle accrue

**Quand utiliser des microservices :**
- Applications complexes nécessitant une scalabilité fine
- Équipes multiples travaillant sur différentes fonctionnalités
- Besoins de déploiement indépendant
- Forte charge avec des patterns d'utilisation variables

**Quand rester monolithique :**
- Petites applications ou MVP
- Équipe réduite
- Domaine métier simple et stable
- Infrastructure limitée

FreePascal se révèle être un excellent choix pour développer des microservices performants et efficaces, particulièrement adaptés aux environnements contraints en ressources ou nécessitant des performances optimales.

⏭️ [Pas2JS - Transpilation vers JavaScript](/09-programmation-web-freepascal/08-pas2js-transpilation-javascript.md)
