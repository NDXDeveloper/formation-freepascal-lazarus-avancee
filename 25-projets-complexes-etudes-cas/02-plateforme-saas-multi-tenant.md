🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 25.2 Plateforme SaaS multi-tenant

## Introduction

Une plateforme **SaaS** (Software as a Service) multi-tenant est une application hébergée dans le cloud qui sert plusieurs clients (tenants) à partir d'une seule instance de l'application. Chaque client a l'impression d'avoir sa propre application dédiée, alors qu'en réalité tous partagent la même infrastructure.

### Qu'est-ce qu'un tenant ?

Un **tenant** (locataire) est une organisation cliente qui utilise votre plateforme SaaS. Par exemple :
- Une entreprise qui utilise votre logiciel de comptabilité
- Une école qui utilise votre plateforme de gestion scolaire
- Un commerce qui utilise votre système de point de vente

### Pourquoi le multi-tenant ?

**Avantages :**
- **Économies d'échelle** : Une seule infrastructure pour tous les clients
- **Maintenance simplifiée** : Une mise à jour déploie pour tous les clients
- **Rentabilité** : Moins de serveurs, moins de coûts
- **Déploiement rapide** : Nouveaux clients actifs en quelques minutes

**Défis :**
- **Isolation des données** : Les données d'un client ne doivent jamais être visibles par un autre
- **Performance** : Un client gourmand ne doit pas ralentir les autres
- **Personnalisation** : Chaque client peut avoir des besoins spécifiques
- **Sécurité renforcée** : Une faille peut affecter tous les clients

## Architecture générale d'une plateforme SaaS multi-tenant

### Les trois modèles d'architecture multi-tenant

#### 1. Base de données partagée, schéma partagé

Tous les tenants utilisent la même base de données et les mêmes tables. Chaque enregistrement contient un identifiant de tenant.

```pascal
// Structure de table avec TenantID
CREATE TABLE Customers (
  ID INTEGER PRIMARY KEY,
  TenantID INTEGER NOT NULL,  -- Identifie le tenant
  Name VARCHAR(100),
  Email VARCHAR(100),
  CONSTRAINT FK_Tenant FOREIGN KEY (TenantID) REFERENCES Tenants(ID)
);

// Toutes les requêtes incluent le filtre TenantID
SELECT * FROM Customers WHERE TenantID = ?
```

**Avantages :**
- Maximum d'économies (ressources partagées)
- Simple à maintenir
- Excellente densité de clients

**Inconvénients :**
- Risque de fuite de données si erreur de code
- Performance dégradée avec beaucoup de données
- Difficile de personnaliser par tenant

#### 2. Base de données partagée, schéma séparé

Chaque tenant a son propre schéma (ensemble de tables) dans la même base de données.

```pascal
// Chaque tenant a son schéma
-- Tenant 1 : schema_tenant1.Customers
-- Tenant 2 : schema_tenant2.Customers

// Connexion dynamique au bon schéma
procedure TDataModule.ConnectToTenant(ATenantID: Integer);  
begin
  FConnection.Schema := 'schema_tenant' + IntToStr(ATenantID);
end;
```

**Avantages :**
- Meilleure isolation des données
- Possibilité de personnaliser le schéma par tenant
- Bon équilibre coût/isolation

**Inconvénients :**
- Plus complexe à gérer
- Limites du nombre de schémas selon le SGBD
- Migrations plus complexes

#### 3. Base de données séparée par tenant

Chaque tenant a sa propre base de données complète.

```pascal
// Gestion des connexions multiples
type
  TTenantConnectionManager = class
  private
    FConnections: TDictionary<Integer, TSQLConnection>;
  public
    function GetConnection(ATenantID: Integer): TSQLConnection;
    procedure CreateTenantDatabase(ATenantID: Integer);
  end;
```

**Avantages :**
- Isolation maximale
- Performance prévisible par tenant
- Facilite la migration d'un client vers un serveur dédié
- Sauvegardes et restaurations individuelles

**Inconvénients :**
- Coût plus élevé
- Maintenance plus complexe
- Limites de scalabilité

### Recommandation pour débuter

Pour une plateforme SaaS avec FreePascal/Lazarus, nous recommandons le **modèle 1 (base partagée, schéma partagé)** pour commencer, car il est :
- Plus simple à implémenter
- Moins coûteux
- Suffisant pour la plupart des cas d'usage

Vous pourrez évoluer vers le modèle 3 pour les clients premium si nécessaire.

## Composants essentiels d'une plateforme SaaS

### 1. Système d'authentification multi-tenant

```pascal
unit TenantAuthentication;

interface

uses
  Classes, SysUtils, SQLdb;

type
  TTenant = class
  private
    FID: Integer;
    FSubdomain: string;
    FCompanyName: string;
    FIsActive: Boolean;
    FPlanType: string; // 'Free', 'Pro', 'Enterprise'
  public
    property ID: Integer read FID write FID;
    property Subdomain: string read FSubdomain write FSubdomain;
    property CompanyName: string read FCompanyName write FCompanyName;
    property IsActive: Boolean read FIsActive write FIsActive;
    property PlanType: string read FPlanType write FPlanType;
  end;

  TUser = class
  private
    FID: Integer;
    FTenantID: Integer;
    FEmail: string;
    FPasswordHash: string;
    FRole: string; // 'Admin', 'User', 'ReadOnly'
  public
    property ID: Integer read FID write FID;
    property TenantID: Integer read FTenantID write FTenantID;
    property Email: string read FEmail write FEmail;
    property Role: string read FRole write FRole;
  end;

  TTenantAuthService = class
  private
    FConnection: TSQLConnection;
    function HashPassword(const APassword: string): string;
  public
    constructor Create(AConnection: TSQLConnection);

    // Identifier le tenant depuis le sous-domaine
    function GetTenantBySubdomain(const ASubdomain: string): TTenant;

    // Authentifier un utilisateur
    function AuthenticateUser(ATenantID: Integer;
                            const AEmail, APassword: string): TUser;

    // Créer un nouveau tenant (inscription)
    function CreateTenant(const ASubdomain, ACompanyName,
                         AAdminEmail, APassword: string): TTenant;
  end;

implementation

uses
  DCPsha256, DCPbase64; // Pour le hashing sécurisé

constructor TTenantAuthService.Create(AConnection: TSQLConnection);  
begin
  inherited Create;
  FConnection := AConnection;
end;

function TTenantAuthService.HashPassword(const APassword: string): string;  
var
  Hash: TDCP_sha256;
begin
  Hash := TDCP_sha256.Create(nil);
  try
    Hash.Init;
    Hash.UpdateStr(APassword);
    Result := Hash.Final;
  finally
    Hash.Free;
  end;
end;

function TTenantAuthService.GetTenantBySubdomain(
  const ASubdomain: string): TTenant;
var
  Query: TSQLQuery;
begin
  Result := nil;
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FConnection;
    Query.SQL.Text :=
      'SELECT ID, Subdomain, CompanyName, IsActive, PlanType ' +
      'FROM Tenants WHERE Subdomain = :subdomain';
    Query.ParamByName('subdomain').AsString := LowerCase(ASubdomain);
    Query.Open;

    if not Query.EOF then
    begin
      Result := TTenant.Create;
      Result.ID := Query.FieldByName('ID').AsInteger;
      Result.Subdomain := Query.FieldByName('Subdomain').AsString;
      Result.CompanyName := Query.FieldByName('CompanyName').AsString;
      Result.IsActive := Query.FieldByName('IsActive').AsBoolean;
      Result.PlanType := Query.FieldByName('PlanType').AsString;
    end;
  finally
    Query.Free;
  end;
end;

function TTenantAuthService.AuthenticateUser(
  ATenantID: Integer; const AEmail, APassword: string): TUser;
var
  Query: TSQLQuery;
  PasswordHash: string;
begin
  Result := nil;
  PasswordHash := HashPassword(APassword);

  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FConnection;
    Query.SQL.Text :=
      'SELECT ID, TenantID, Email, Role ' +
      'FROM Users ' +
      'WHERE TenantID = :tenantid ' +
      '  AND Email = :email ' +
      '  AND PasswordHash = :hash ' +
      '  AND IsActive = 1';
    Query.ParamByName('tenantid').AsInteger := ATenantID;
    Query.ParamByName('email').AsString := LowerCase(AEmail);
    Query.ParamByName('hash').AsString := PasswordHash;
    Query.Open;

    if not Query.EOF then
    begin
      Result := TUser.Create;
      Result.ID := Query.FieldByName('ID').AsInteger;
      Result.TenantID := Query.FieldByName('TenantID').AsInteger;
      Result.Email := Query.FieldByName('Email').AsString;
      Result.Role := Query.FieldByName('Role').AsString;
    end;
  finally
    Query.Free;
  end;
end;

function TTenantAuthService.CreateTenant(
  const ASubdomain, ACompanyName, AAdminEmail, APassword: string): TTenant;
var
  Query: TSQLQuery;
  TenantID: Integer;
begin
  // Vérifier que le sous-domaine est disponible
  if Assigned(GetTenantBySubdomain(ASubdomain)) then
    raise Exception.Create('Ce sous-domaine est déjà utilisé');

  Query := TSQLQuery.Create(nil);
  try
    FConnection.StartTransaction;
    try
      // Créer le tenant
      Query.Database := FConnection;
      Query.SQL.Text :=
        'INSERT INTO Tenants (Subdomain, CompanyName, PlanType, IsActive, CreatedAt) ' +
        'VALUES (:subdomain, :company, ''Free'', 1, :now)';
      Query.ParamByName('subdomain').AsString := LowerCase(ASubdomain);
      Query.ParamByName('company').AsString := ACompanyName;
      Query.ParamByName('now').AsDateTime := Now;
      Query.ExecSQL;

      // Récupérer l'ID du tenant créé
      Query.SQL.Text := 'SELECT LAST_INSERT_ID() AS ID'; // MySQL
      // Pour PostgreSQL : 'SELECT currval(''tenants_id_seq'') AS ID'
      Query.Open;
      TenantID := Query.FieldByName('ID').AsInteger;
      Query.Close;

      // Créer l'utilisateur administrateur
      Query.SQL.Text :=
        'INSERT INTO Users (TenantID, Email, PasswordHash, Role, IsActive) ' +
        'VALUES (:tenantid, :email, :hash, ''Admin'', 1)';
      Query.ParamByName('tenantid').AsInteger := TenantID;
      Query.ParamByName('email').AsString := LowerCase(AAdminEmail);
      Query.ParamByName('hash').AsString := HashPassword(APassword);
      Query.ExecSQL;

      FConnection.Commit;

      // Retourner le tenant créé
      Result := GetTenantBySubdomain(ASubdomain);
    except
      FConnection.Rollback;
      raise;
    end;
  finally
    Query.Free;
  end;
end;

end.
```

### 2. Contexte de tenant (Tenant Context)

Le contexte de tenant assure que toutes les opérations sont limitées au tenant actuel.

```pascal
unit TenantContext;

interface

uses
  Classes, SysUtils;

type
  TTenantContextManager = class
  private
    class var FCurrentTenantID: Integer;
    class var FCurrentUserID: Integer;
    class var FCurrentUserRole: string;
  public
    class procedure SetContext(ATenantID, AUserID: Integer;
                              const ARole: string);
    class procedure ClearContext;
    class function GetCurrentTenantID: Integer;
    class function GetCurrentUserID: Integer;
    class function GetCurrentUserRole: string;
    class function IsAdmin: Boolean;
  end;

implementation

class procedure TTenantContextManager.SetContext(
  ATenantID, AUserID: Integer; const ARole: string);
begin
  FCurrentTenantID := ATenantID;
  FCurrentUserID := AUserID;
  FCurrentUserRole := ARole;
end;

class procedure TTenantContextManager.ClearContext;  
begin
  FCurrentTenantID := 0;
  FCurrentUserID := 0;
  FCurrentUserRole := '';
end;

class function TTenantContextManager.GetCurrentTenantID: Integer;  
begin
  if FCurrentTenantID = 0 then
    raise Exception.Create('Aucun contexte de tenant défini');
  Result := FCurrentTenantID;
end;

class function TTenantContextManager.GetCurrentUserID: Integer;  
begin
  Result := FCurrentUserID;
end;

class function TTenantContextManager.GetCurrentUserRole: string;  
begin
  Result := FCurrentUserRole;
end;

class function TTenantContextManager.IsAdmin: Boolean;  
begin
  Result := FCurrentUserRole = 'Admin';
end;

end.
```

### 3. Couche d'accès aux données avec isolation automatique

```pascal
unit TenantDataAccess;

interface

uses
  Classes, SysUtils, SQLdb, TenantContext;

type
  TTenantAwareQuery = class(TSQLQuery)
  private
    FAutoFilterTenant: Boolean;
  protected
    procedure InternalOpen; override;
  public
    constructor Create(AOwner: TComponent); override;
    property AutoFilterTenant: Boolean read FAutoFilterTenant
                                       write FAutoFilterTenant default True;
  end;

  TTenantDataService = class
  private
    FConnection: TSQLConnection;
    function CreateQuery: TTenantAwareQuery;
  public
    constructor Create(AConnection: TSQLConnection);

    // Opérations CRUD avec isolation automatique
    function GetRecords(const ATableName: string;
                       const AWhereClause: string = ''): TSQLQuery;
    function GetRecordByID(const ATableName: string;
                          AID: Integer): TSQLQuery;
    procedure InsertRecord(const ATableName: string;
                          AFields: TStrings; AValues: array of Variant);
    procedure UpdateRecord(const ATableName: string; AID: Integer;
                          AFields: TStrings; AValues: array of Variant);
    procedure DeleteRecord(const ATableName: string; AID: Integer);
  end;

implementation

{ TTenantAwareQuery }

constructor TTenantAwareQuery.Create(AOwner: TComponent);  
begin
  inherited Create(AOwner);
  FAutoFilterTenant := True;
end;

procedure TTenantAwareQuery.InternalOpen;  
var
  OriginalSQL: string;
  TenantID: Integer;
begin
  // Injecter automatiquement le filtre TenantID si activé
  if FAutoFilterTenant then
  begin
    TenantID := TTenantContextManager.GetCurrentTenantID;
    OriginalSQL := SQL.Text;

    // Ajouter le filtre TenantID si pas déjà présent
    if (Pos('WHERE', UpperCase(OriginalSQL)) > 0) and
       (Pos('TENANTID', UpperCase(OriginalSQL)) = 0) then
    begin
      // Ajouter AND TenantID = ?
      SQL.Text := StringReplace(OriginalSQL, ' WHERE ',
        ' WHERE TenantID = ' + IntToStr(TenantID) + ' AND ',
        [rfIgnoreCase]);
    end
    else if (Pos('WHERE', UpperCase(OriginalSQL)) = 0) and
            (Pos('TENANTID', UpperCase(OriginalSQL)) = 0) then
    begin
      // Ajouter WHERE TenantID = ?
      if Pos('ORDER BY', UpperCase(OriginalSQL)) > 0 then
        SQL.Text := StringReplace(OriginalSQL, ' ORDER BY ',
          ' WHERE TenantID = ' + IntToStr(TenantID) + ' ORDER BY ',
          [rfIgnoreCase])
      else
        SQL.Text := OriginalSQL + ' WHERE TenantID = ' + IntToStr(TenantID);
    end;
  end;

  inherited InternalOpen;
end;

{ TTenantDataService }

constructor TTenantDataService.Create(AConnection: TSQLConnection);  
begin
  inherited Create;
  FConnection := AConnection;
end;

function TTenantDataService.CreateQuery: TTenantAwareQuery;  
begin
  Result := TTenantAwareQuery.Create(nil);
  Result.Database := FConnection;
end;

function TTenantDataService.GetRecords(const ATableName, AWhereClause: string): TSQLQuery;  
var
  Query: TTenantAwareQuery;
  SQL: string;
begin
  Query := CreateQuery;
  SQL := 'SELECT * FROM ' + ATableName;
  if AWhereClause <> '' then
    SQL := SQL + ' WHERE ' + AWhereClause;
  Query.SQL.Text := SQL;
  Query.Open;
  Result := Query;
end;

function TTenantDataService.GetRecordByID(const ATableName: string;
  AID: Integer): TSQLQuery;
var
  Query: TTenantAwareQuery;
begin
  Query := CreateQuery;
  Query.SQL.Text := Format('SELECT * FROM %s WHERE ID = :id', [ATableName]);
  Query.ParamByName('id').AsInteger := AID;
  Query.Open;
  Result := Query;
end;

procedure TTenantDataService.InsertRecord(const ATableName: string;
  AFields: TStrings; AValues: array of Variant);
var
  Query: TTenantAwareQuery;
  SQL, FieldList, ValueList: string;
  i: Integer;
begin
  // Toujours ajouter TenantID
  FieldList := 'TenantID';
  ValueList := IntToStr(TTenantContextManager.GetCurrentTenantID);

  for i := 0 to AFields.Count - 1 do
  begin
    FieldList := FieldList + ', ' + AFields[i];
    ValueList := ValueList + ', :' + AFields[i];
  end;

  SQL := Format('INSERT INTO %s (%s) VALUES (%s)',
                [ATableName, FieldList, ValueList]);

  Query := CreateQuery;
  try
    Query.SQL.Text := SQL;
    for i := 0 to AFields.Count - 1 do
      Query.ParamByName(AFields[i]).Value := AValues[i];
    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

procedure TTenantDataService.UpdateRecord(const ATableName: string;
  AID: Integer; AFields: TStrings; AValues: array of Variant);
var
  Query: TTenantAwareQuery;
  SQL, SetClause: string;
  i: Integer;
begin
  SetClause := '';
  for i := 0 to AFields.Count - 1 do
  begin
    if i > 0 then
      SetClause := SetClause + ', ';
    SetClause := SetClause + AFields[i] + ' = :' + AFields[i];
  end;

  SQL := Format('UPDATE %s SET %s WHERE ID = :id', [ATableName, SetClause]);

  Query := CreateQuery;
  try
    Query.SQL.Text := SQL;
    Query.ParamByName('id').AsInteger := AID;
    for i := 0 to AFields.Count - 1 do
      Query.ParamByName(AFields[i]).Value := AValues[i];
    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

procedure TTenantDataService.DeleteRecord(const ATableName: string; AID: Integer);  
var
  Query: TTenantAwareQuery;
begin
  Query := CreateQuery;
  try
    Query.SQL.Text := Format('DELETE FROM %s WHERE ID = :id', [ATableName]);
    Query.ParamByName('id').AsInteger := AID;
    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

end.
```

## Architecture Web multi-tenant avec fpWeb

### Serveur HTTP avec gestion des sous-domaines

```pascal
program SaaSServer;

{$mode objfpc}{$H+}

uses
  fphttpapp, httpdefs, httproute, fpjson, jsonparser,
  SysUtils, Classes,
  TenantAuthentication, TenantContext, TenantDataAccess;

type
  { TRequestHandler }
  TRequestHandler = class
  private
    FAuthService: TTenantAuthService;
    FDataService: TTenantDataService;
    function ExtractSubdomain(const AHost: string): string;
    function ExtractBearerToken(ARequest: TRequest): string;
    procedure ValidateAuthentication(ARequest: TRequest);
  public
    constructor Create;
    destructor Destroy; override;

    procedure HandleLogin(ARequest: TRequest; AResponse: TResponse);
    procedure HandleRegister(ARequest: TRequest; AResponse: TResponse);
    procedure HandleGetCustomers(ARequest: TRequest; AResponse: TResponse);
    procedure HandleCreateCustomer(ARequest: TRequest; AResponse: TResponse);
  end;

{ TRequestHandler }

constructor TRequestHandler.Create;  
begin
  inherited Create;
  // Initialiser les services
  FAuthService := TTenantAuthService.Create(GetDatabaseConnection);
  FDataService := TTenantDataService.Create(GetDatabaseConnection);
end;

destructor TRequestHandler.Destroy;  
begin
  FAuthService.Free;
  FDataService.Free;
  inherited Destroy;
end;

function TRequestHandler.ExtractSubdomain(const AHost: string): string;  
var
  Parts: TStringList;
begin
  // Extraire le sous-domaine depuis l'URL
  // Ex: "client1.monapp.com" -> "client1"
  Parts := TStringList.Create;
  try
    Parts.Delimiter := '.';
    Parts.DelimitedText := AHost;
    if Parts.Count > 2 then
      Result := Parts[0]
    else
      Result := '';
  finally
    Parts.Free;
  end;
end;

function TRequestHandler.ExtractBearerToken(ARequest: TRequest): string;  
var
  AuthHeader: string;
begin
  AuthHeader := ARequest.GetCustomHeader('Authorization');
  if Copy(AuthHeader, 1, 7) = 'Bearer ' then
    Result := Copy(AuthHeader, 8, Length(AuthHeader))
  else
    Result := '';
end;

procedure TRequestHandler.ValidateAuthentication(ARequest: TRequest);  
var
  Token, Subdomain: string;
  Tenant: TTenant;
  User: TUser;
begin
  // Extraire le sous-domaine
  Subdomain := ExtractSubdomain(ARequest.Host);
  if Subdomain = '' then
    raise Exception.Create('Sous-domaine invalide');

  // Récupérer le tenant
  Tenant := FAuthService.GetTenantBySubdomain(Subdomain);
  if not Assigned(Tenant) then
    raise Exception.Create('Tenant introuvable');
  if not Tenant.IsActive then
    raise Exception.Create('Tenant désactivé');

  // Valider le token JWT (simplifié ici)
  Token := ExtractBearerToken(ARequest);
  if Token = '' then
    raise Exception.Create('Authentification requise');

  // Décoder le token et récupérer l'utilisateur
  // (implémentation JWT omise pour simplifier)
  User := GetUserFromToken(Token);

  // Définir le contexte
  TTenantContextManager.SetContext(Tenant.ID, User.ID, User.Role);
end;

procedure TRequestHandler.HandleLogin(ARequest: TRequest; AResponse: TResponse);  
var
  JSON, ResponseJSON: TJSONObject;
  Subdomain, Email, Password: string;
  Tenant: TTenant;
  User: TUser;
  Token: string;
begin
  try
    // Parser le JSON de la requête
    JSON := GetJSON(ARequest.Content) as TJSONObject;
    try
      Email := JSON.Get('email', '');
      Password := JSON.Get('password', '');

      // Extraire le sous-domaine
      Subdomain := ExtractSubdomain(ARequest.Host);
      Tenant := FAuthService.GetTenantBySubdomain(Subdomain);

      if not Assigned(Tenant) then
      begin
        AResponse.Code := 404;
        AResponse.Content := '{"error": "Tenant introuvable"}';
        Exit;
      end;

      // Authentifier l'utilisateur
      User := FAuthService.AuthenticateUser(Tenant.ID, Email, Password);

      if not Assigned(User) then
      begin
        AResponse.Code := 401;
        AResponse.Content := '{"error": "Identifiants invalides"}';
        Exit;
      end;

      // Générer un token JWT (simplifié)
      Token := GenerateJWT(User);

      // Réponse avec le token
      ResponseJSON := TJSONObject.Create;
      try
        ResponseJSON.Add('token', Token);
        ResponseJSON.Add('user_id', User.ID);
        ResponseJSON.Add('email', User.Email);
        ResponseJSON.Add('role', User.Role);

        AResponse.Code := 200;
        AResponse.ContentType := 'application/json';
        AResponse.Content := ResponseJSON.AsJSON;
      finally
        ResponseJSON.Free;
      end;
    finally
      JSON.Free;
    end;
  except
    on E: Exception do
    begin
      AResponse.Code := 500;
      AResponse.Content := Format('{"error": "%s"}', [E.Message]);
    end;
  end;
end;

procedure TRequestHandler.HandleRegister(ARequest: TRequest; AResponse: TResponse);  
var
  JSON, ResponseJSON: TJSONObject;
  Subdomain, CompanyName, Email, Password: string;
  Tenant: TTenant;
begin
  try
    JSON := GetJSON(ARequest.Content) as TJSONObject;
    try
      Subdomain := JSON.Get('subdomain', '');
      CompanyName := JSON.Get('company_name', '');
      Email := JSON.Get('email', '');
      Password := JSON.Get('password', '');

      // Créer le nouveau tenant
      Tenant := FAuthService.CreateTenant(Subdomain, CompanyName, Email, Password);

      ResponseJSON := TJSONObject.Create;
      try
        ResponseJSON.Add('success', True);
        ResponseJSON.Add('subdomain', Tenant.Subdomain);
        ResponseJSON.Add('message', 'Compte créé avec succès');

        AResponse.Code := 201;
        AResponse.ContentType := 'application/json';
        AResponse.Content := ResponseJSON.AsJSON;
      finally
        ResponseJSON.Free;
      end;
    finally
      JSON.Free;
    end;
  except
    on E: Exception do
    begin
      AResponse.Code := 400;
      AResponse.Content := Format('{"error": "%s"}', [E.Message]);
    end;
  end;
end;

procedure TRequestHandler.HandleGetCustomers(ARequest: TRequest; AResponse: TResponse);  
var
  Query: TSQLQuery;
  JSONArray: TJSONArray;
  JSONObject: TJSONObject;
begin
  try
    // Valider l'authentification et établir le contexte
    ValidateAuthentication(ARequest);

    // Récupérer les clients (automatiquement filtrés par tenant)
    Query := FDataService.GetRecords('Customers');
    try
      JSONArray := TJSONArray.Create;
      try
        while not Query.EOF do
        begin
          JSONObject := TJSONObject.Create;
          JSONObject.Add('id', Query.FieldByName('ID').AsInteger);
          JSONObject.Add('name', Query.FieldByName('Name').AsString);
          JSONObject.Add('email', Query.FieldByName('Email').AsString);
          JSONArray.Add(JSONObject);
          Query.Next;
        end;

        AResponse.Code := 200;
        AResponse.ContentType := 'application/json';
        AResponse.Content := JSONArray.AsJSON;
      finally
        JSONArray.Free;
      end;
    finally
      Query.Free;
    end;
  except
    on E: Exception do
    begin
      AResponse.Code := 500;
      AResponse.Content := Format('{"error": "%s"}', [E.Message]);
    end;
  end;
end;

procedure TRequestHandler.HandleCreateCustomer(ARequest: TRequest; AResponse: TResponse);  
var
  JSON: TJSONObject;
  Fields: TStringList;
  Values: array of Variant;
begin
  try
    ValidateAuthentication(ARequest);

    JSON := GetJSON(ARequest.Content) as TJSONObject;
    try
      Fields := TStringList.Create;
      try
        Fields.Add('Name');
        Fields.Add('Email');
        Fields.Add('Phone');

        SetLength(Values, 3);
        Values[0] := JSON.Get('name', '');
        Values[1] := JSON.Get('email', '');
        Values[2] := JSON.Get('phone', '');

        // Le TenantID sera ajouté automatiquement
        FDataService.InsertRecord('Customers', Fields, Values);

        AResponse.Code := 201;
        AResponse.Content := '{"success": true, "message": "Client créé"}';
      finally
        Fields.Free;
      end;
    finally
      JSON.Free;
    end;
  except
    on E: Exception do
    begin
      AResponse.Code := 400;
      AResponse.Content := Format('{"error": "%s"}', [E.Message]);
    end;
  end;
end;

var
  Handler: TRequestHandler;

begin
  Handler := TRequestHandler.Create;
  try
    // Configurer les routes
    HTTPRouter.RegisterRoute('/api/auth/login', rmPost, @Handler.HandleLogin);
    HTTPRouter.RegisterRoute('/api/auth/register', rmPost, @Handler.HandleRegister);
    HTTPRouter.RegisterRoute('/api/customers', rmGet, @Handler.HandleGetCustomers);
    HTTPRouter.RegisterRoute('/api/customers', rmPost, @Handler.HandleCreateCustomer);

    // Démarrer le serveur
    Application.Title := 'SaaS Multi-tenant Server';
    Application.Port := 8080;
    Application.Initialize;
    WriteLn('Serveur démarré sur le port 8080');
    Application.Run;
  finally
    Handler.Free;
  end;
end.
```

## Gestion des plans tarifaires (Subscription Plans)

Une plateforme SaaS propose généralement plusieurs niveaux de service (plans). Voici comment les gérer :

### Structure de la base de données pour les plans

```sql
-- Table des plans disponibles
CREATE TABLE SubscriptionPlans (
  ID INTEGER PRIMARY KEY AUTO_INCREMENT,
  Name VARCHAR(50) NOT NULL,           -- 'Free', 'Pro', 'Enterprise'
  DisplayName VARCHAR(100) NOT NULL,    -- 'Plan Gratuit', 'Plan Pro', etc.
  MonthlyPrice DECIMAL(10,2) NOT NULL,
  YearlyPrice DECIMAL(10,2) NOT NULL,
  MaxUsers INTEGER NOT NULL,            -- Nombre max d'utilisateurs
  MaxStorage BIGINT NOT NULL,           -- En octets
  HasAPIAccess BOOLEAN DEFAULT 0,
  HasPrioritySupport BOOLEAN DEFAULT 0,
  Features TEXT,                        -- JSON des fonctionnalités
  IsActive BOOLEAN DEFAULT 1
);

-- Table des souscriptions des tenants
CREATE TABLE TenantSubscriptions (
  ID INTEGER PRIMARY KEY AUTO_INCREMENT,
  TenantID INTEGER NOT NULL,
  PlanID INTEGER NOT NULL,
  BillingCycle VARCHAR(20) NOT NULL,    -- 'monthly', 'yearly'
  Status VARCHAR(20) NOT NULL,          -- 'active', 'cancelled', 'expired'
  StartDate DATETIME NOT NULL,
  EndDate DATETIME,
  LastBillingDate DATETIME,
  NextBillingDate DATETIME,
  Amount DECIMAL(10,2) NOT NULL,
  FOREIGN KEY (TenantID) REFERENCES Tenants(ID),
  FOREIGN KEY (PlanID) REFERENCES SubscriptionPlans(ID)
);

-- Table de l'utilisation des ressources
CREATE TABLE TenantUsage (
  ID INTEGER PRIMARY KEY AUTO_INCREMENT,
  TenantID INTEGER NOT NULL,
  UsageDate DATE NOT NULL,
  ActiveUsers INTEGER DEFAULT 0,
  StorageUsed BIGINT DEFAULT 0,
  APICallsCount INTEGER DEFAULT 0,
  FOREIGN KEY (TenantID) REFERENCES Tenants(ID),
  UNIQUE KEY unique_tenant_date (TenantID, UsageDate)
);
```

### Service de gestion des plans

```pascal
unit SubscriptionService;

interface

uses
  Classes, SysUtils, SQLdb, fpjson;

type
  TSubscriptionPlan = class
  private
    FID: Integer;
    FName: string;
    FDisplayName: string;
    FMonthlyPrice: Currency;
    FYearlyPrice: Currency;
    FMaxUsers: Integer;
    FMaxStorage: Int64;
    FHasAPIAccess: Boolean;
    FHasPrioritySupport: Boolean;
    FFeatures: TJSONObject;
  public
    destructor Destroy; override;
    property ID: Integer read FID write FID;
    property Name: string read FName write FName;
    property DisplayName: string read FDisplayName write FDisplayName;
    property MonthlyPrice: Currency read FMonthlyPrice write FMonthlyPrice;
    property YearlyPrice: Currency read FYearlyPrice write FYearlyPrice;
    property MaxUsers: Integer read FMaxUsers write FMaxUsers;
    property MaxStorage: Int64 read FMaxStorage write FMaxStorage;
    property HasAPIAccess: Boolean read FHasAPIAccess write FHasAPIAccess;
    property HasPrioritySupport: Boolean read FHasPrioritySupport write FHasPrioritySupport;
    property Features: TJSONObject read FFeatures;
  end;

  TSubscriptionManager = class
  private
    FConnection: TSQLConnection;
  public
    constructor Create(AConnection: TSQLConnection);

    // Récupérer les plans disponibles
    function GetAvailablePlans: TList;

    // Récupérer le plan d'un tenant
    function GetTenantPlan(ATenantID: Integer): TSubscriptionPlan;

    // Changer le plan d'un tenant
    procedure UpgradePlan(ATenantID, ANewPlanID: Integer;
                         const ABillingCycle: string);

    // Vérifier si un tenant peut effectuer une action
    function CanAddUser(ATenantID: Integer): Boolean;
    function CanUseFeature(ATenantID: Integer;
                          const AFeatureName: string): Boolean;
    function GetStorageLimit(ATenantID: Integer): Int64;

    // Enregistrer l'utilisation
    procedure RecordUsage(ATenantID: Integer;
                         AActiveUsers: Integer;
                         AStorageUsed: Int64;
                         AAPICalls: Integer);
  end;

implementation

uses
  DateUtils, jsonparser;

{ TSubscriptionPlan }

destructor TSubscriptionPlan.Destroy;  
begin
  if Assigned(FFeatures) then
    FFeatures.Free;
  inherited Destroy;
end;

{ TSubscriptionManager }

constructor TSubscriptionManager.Create(AConnection: TSQLConnection);  
begin
  inherited Create;
  FConnection := AConnection;
end;

function TSubscriptionManager.GetAvailablePlans: TList;  
var
  Query: TSQLQuery;
  Plan: TSubscriptionPlan;
begin
  Result := TList.Create;
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FConnection;
    Query.SQL.Text :=
      'SELECT ID, Name, DisplayName, MonthlyPrice, YearlyPrice, ' +
      '       MaxUsers, MaxStorage, HasAPIAccess, HasPrioritySupport, Features ' +
      'FROM SubscriptionPlans ' +
      'WHERE IsActive = 1 ' +
      'ORDER BY MonthlyPrice';
    Query.Open;

    while not Query.EOF do
    begin
      Plan := TSubscriptionPlan.Create;
      Plan.ID := Query.FieldByName('ID').AsInteger;
      Plan.Name := Query.FieldByName('Name').AsString;
      Plan.DisplayName := Query.FieldByName('DisplayName').AsString;
      Plan.MonthlyPrice := Query.FieldByName('MonthlyPrice').AsCurrency;
      Plan.YearlyPrice := Query.FieldByName('YearlyPrice').AsCurrency;
      Plan.MaxUsers := Query.FieldByName('MaxUsers').AsInteger;
      Plan.MaxStorage := Query.FieldByName('MaxStorage').AsLargeInt;
      Plan.HasAPIAccess := Query.FieldByName('HasAPIAccess').AsBoolean;
      Plan.HasPrioritySupport := Query.FieldByName('HasPrioritySupport').AsBoolean;

      if not Query.FieldByName('Features').IsNull then
        Plan.FFeatures := GetJSON(Query.FieldByName('Features').AsString) as TJSONObject;

      Result.Add(Plan);
      Query.Next;
    end;
  finally
    Query.Free;
  end;
end;

function TSubscriptionManager.GetTenantPlan(ATenantID: Integer): TSubscriptionPlan;  
var
  Query: TSQLQuery;
begin
  Result := nil;
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FConnection;
    Query.SQL.Text :=
      'SELECT p.ID, p.Name, p.DisplayName, p.MonthlyPrice, p.YearlyPrice, ' +
      '       p.MaxUsers, p.MaxStorage, p.HasAPIAccess, p.HasPrioritySupport, p.Features ' +
      'FROM SubscriptionPlans p ' +
      'INNER JOIN TenantSubscriptions s ON p.ID = s.PlanID ' +
      'WHERE s.TenantID = :tenantid ' +
      '  AND s.Status = ''active'' ' +
      'ORDER BY s.StartDate DESC ' +
      'LIMIT 1';
    Query.ParamByName('tenantid').AsInteger := ATenantID;
    Query.Open;

    if not Query.EOF then
    begin
      Result := TSubscriptionPlan.Create;
      Result.ID := Query.FieldByName('ID').AsInteger;
      Result.Name := Query.FieldByName('Name').AsString;
      Result.DisplayName := Query.FieldByName('DisplayName').AsString;
      Result.MonthlyPrice := Query.FieldByName('MonthlyPrice').AsCurrency;
      Result.YearlyPrice := Query.FieldByName('YearlyPrice').AsCurrency;
      Result.MaxUsers := Query.FieldByName('MaxUsers').AsInteger;
      Result.MaxStorage := Query.FieldByName('MaxStorage').AsLargeInt;
      Result.HasAPIAccess := Query.FieldByName('HasAPIAccess').AsBoolean;
      Result.HasPrioritySupport := Query.FieldByName('HasPrioritySupport').AsBoolean;

      if not Query.FieldByName('Features').IsNull then
        Result.FFeatures := GetJSON(Query.FieldByName('Features').AsString) as TJSONObject;
    end;
  finally
    Query.Free;
  end;
end;

procedure TSubscriptionManager.UpgradePlan(ATenantID, ANewPlanID: Integer;
  const ABillingCycle: string);
var
  Query: TSQLQuery;
  Plan: TSubscriptionPlan;
  Amount: Currency;
begin
  // Récupérer le nouveau plan
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FConnection;
    Query.SQL.Text :=
      'SELECT MonthlyPrice, YearlyPrice FROM SubscriptionPlans WHERE ID = :planid';
    Query.ParamByName('planid').AsInteger := ANewPlanID;
    Query.Open;

    if Query.EOF then
      raise Exception.Create('Plan introuvable');

    if ABillingCycle = 'yearly' then
      Amount := Query.FieldByName('YearlyPrice').AsCurrency
    else
      Amount := Query.FieldByName('MonthlyPrice').AsCurrency;

    Query.Close;

    FConnection.StartTransaction;
    try
      // Annuler l'ancienne souscription
      Query.SQL.Text :=
        'UPDATE TenantSubscriptions ' +
        'SET Status = ''cancelled'', EndDate = :now ' +
        'WHERE TenantID = :tenantid AND Status = ''active''';
      Query.ParamByName('tenantid').AsInteger := ATenantID;
      Query.ParamByName('now').AsDateTime := Now;
      Query.ExecSQL;

      // Créer la nouvelle souscription
      Query.SQL.Text :=
        'INSERT INTO TenantSubscriptions ' +
        '(TenantID, PlanID, BillingCycle, Status, StartDate, NextBillingDate, Amount) ' +
        'VALUES (:tenantid, :planid, :cycle, ''active'', :now, :next, :amount)';
      Query.ParamByName('tenantid').AsInteger := ATenantID;
      Query.ParamByName('planid').AsInteger := ANewPlanID;
      Query.ParamByName('cycle').AsString := ABillingCycle;
      Query.ParamByName('now').AsDateTime := Now;

      if ABillingCycle = 'yearly' then
        Query.ParamByName('next').AsDateTime := IncYear(Now, 1)
      else
        Query.ParamByName('next').AsDateTime := IncMonth(Now, 1);

      Query.ParamByName('amount').AsCurrency := Amount;
      Query.ExecSQL;

      FConnection.Commit;
    except
      FConnection.Rollback;
      raise;
    end;
  finally
    Query.Free;
  end;
end;

function TSubscriptionManager.CanAddUser(ATenantID: Integer): Boolean;  
var
  Query: TSQLQuery;
  Plan: TSubscriptionPlan;
  CurrentUsers: Integer;
begin
  Result := False;
  Plan := GetTenantPlan(ATenantID);
  try
    if not Assigned(Plan) then
      Exit;

    // Compter les utilisateurs actifs
    Query := TSQLQuery.Create(nil);
    try
      Query.Database := FConnection;
      Query.SQL.Text :=
        'SELECT COUNT(*) AS UserCount ' +
        'FROM Users ' +
        'WHERE TenantID = :tenantid AND IsActive = 1';
      Query.ParamByName('tenantid').AsInteger := ATenantID;
      Query.Open;

      CurrentUsers := Query.FieldByName('UserCount').AsInteger;
      Result := CurrentUsers < Plan.MaxUsers;
    finally
      Query.Free;
    end;
  finally
    Plan.Free;
  end;
end;

function TSubscriptionManager.CanUseFeature(ATenantID: Integer;
  const AFeatureName: string): Boolean;
var
  Plan: TSubscriptionPlan;
begin
  Result := False;
  Plan := GetTenantPlan(ATenantID);
  try
    if not Assigned(Plan) then
      Exit;

    // Vérifier si la fonctionnalité est dans le plan
    if Assigned(Plan.Features) then
      Result := Plan.Features.Get(AFeatureName, False);

    // Vérifications spéciales
    if AFeatureName = 'api_access' then
      Result := Plan.HasAPIAccess
    else if AFeatureName = 'priority_support' then
      Result := Plan.HasPrioritySupport;
  finally
    Plan.Free;
  end;
end;

function TSubscriptionManager.GetStorageLimit(ATenantID: Integer): Int64;  
var
  Plan: TSubscriptionPlan;
begin
  Result := 0;
  Plan := GetTenantPlan(ATenantID);
  try
    if Assigned(Plan) then
      Result := Plan.MaxStorage;
  finally
    Plan.Free;
  end;
end;

procedure TSubscriptionManager.RecordUsage(ATenantID, AActiveUsers: Integer;
  AStorageUsed: Int64; AAPICalls: Integer);
var
  Query: TSQLQuery;
begin
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FConnection;

    // Insertion ou mise à jour de l'utilisation du jour
    Query.SQL.Text :=
      'INSERT INTO TenantUsage (TenantID, UsageDate, ActiveUsers, StorageUsed, APICallsCount) ' +
      'VALUES (:tenantid, :date, :users, :storage, :calls) ' +
      'ON DUPLICATE KEY UPDATE ' +
      '  ActiveUsers = :users, ' +
      '  StorageUsed = :storage, ' +
      '  APICallsCount = APICallsCount + :calls';

    Query.ParamByName('tenantid').AsInteger := ATenantID;
    Query.ParamByName('date').AsDate := Date;
    Query.ParamByName('users').AsInteger := AActiveUsers;
    Query.ParamByName('storage').AsLargeInt := AStorageUsed;
    Query.ParamByName('calls').AsInteger := AAPICalls;
    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

end.
```

## Gestion des fichiers et du stockage multi-tenant

### Service de stockage de fichiers

```pascal
unit TenantFileStorage;

interface

uses
  Classes, SysUtils, SQLdb;

type
  TFileStorageService = class
  private
    FConnection: TSQLConnection;
    FBasePath: string;
    function GetTenantPath(ATenantID: Integer): string;
    function CalculateDirectorySize(const APath: string): Int64;
  public
    constructor Create(AConnection: TSQLConnection; const ABasePath: string);

    // Opérations sur les fichiers
    function UploadFile(ATenantID: Integer; const AFileName: string;
                       AStream: TStream): string;
    function DownloadFile(ATenantID: Integer; const AFileID: string;
                         AStream: TStream): Boolean;
    procedure DeleteFile(ATenantID: Integer; const AFileID: string);
    function GetFileList(ATenantID: Integer): TStringList;

    // Gestion du quota
    function GetUsedStorage(ATenantID: Integer): Int64;
    function CheckQuota(ATenantID: Integer; AFileSize: Int64): Boolean;
  end;

implementation

uses
  TenantContext, SubscriptionService, MD5;

{ TFileStorageService }

constructor TFileStorageService.Create(AConnection: TSQLConnection;
  const ABasePath: string);
begin
  inherited Create;
  FConnection := AConnection;
  FBasePath := IncludeTrailingPathDelimiter(ABasePath);

  // Créer le répertoire de base si nécessaire
  if not DirectoryExists(FBasePath) then
    ForceDirectories(FBasePath);
end;

function TFileStorageService.GetTenantPath(ATenantID: Integer): string;  
begin
  // Organiser les fichiers par tenant
  Result := FBasePath + 'tenant_' + IntToStr(ATenantID) + PathDelim;

  if not DirectoryExists(Result) then
    ForceDirectories(Result);
end;

function TFileStorageService.CalculateDirectorySize(const APath: string): Int64;  
var
  SearchRec: TSearchRec;
begin
  Result := 0;

  if FindFirst(APath + '*', faAnyFile, SearchRec) = 0 then
  begin
    repeat
      if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
      begin
        if (SearchRec.Attr and faDirectory) = faDirectory then
          Result := Result + CalculateDirectorySize(APath + SearchRec.Name + PathDelim)
        else
          Result := Result + SearchRec.Size;
      end;
    until FindNext(SearchRec) <> 0;
    FindClose(SearchRec);
  end;
end;

function TFileStorageService.UploadFile(ATenantID: Integer;
  const AFileName: string; AStream: TStream): string;
var
  TenantPath, FileID, FilePath: string;
  FileStream: TFileStream;
  Query: TSQLQuery;
begin
  // Vérifier le quota avant l'upload
  if not CheckQuota(ATenantID, AStream.Size) then
    raise Exception.Create('Quota de stockage dépassé');

  // Générer un ID unique pour le fichier
  FileID := MD5Print(MD5String(AFileName + DateTimeToStr(Now)));

  // Obtenir le chemin du tenant
  TenantPath := GetTenantPath(ATenantID);
  FilePath := TenantPath + FileID;

  // Sauvegarder le fichier
  FileStream := TFileStream.Create(FilePath, fmCreate);
  try
    AStream.Position := 0;
    FileStream.CopyFrom(AStream, AStream.Size);
  finally
    FileStream.Free;
  end;

  // Enregistrer dans la base de données
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FConnection;
    Query.SQL.Text :=
      'INSERT INTO TenantFiles (TenantID, FileID, FileName, FileSize, UploadDate, FilePath) ' +
      'VALUES (:tenantid, :fileid, :filename, :size, :date, :path)';
    Query.ParamByName('tenantid').AsInteger := ATenantID;
    Query.ParamByName('fileid').AsString := FileID;
    Query.ParamByName('filename').AsString := AFileName;
    Query.ParamByName('size').AsLargeInt := AStream.Size;
    Query.ParamByName('date').AsDateTime := Now;
    Query.ParamByName('path').AsString := FilePath;
    Query.ExecSQL;
  finally
    Query.Free;
  end;

  Result := FileID;
end;

function TFileStorageService.DownloadFile(ATenantID: Integer;
  const AFileID: string; AStream: TStream): Boolean;
var
  Query: TSQLQuery;
  FilePath: string;
  FileStream: TFileStream;
begin
  Result := False;
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FConnection;
    Query.SQL.Text :=
      'SELECT FilePath FROM TenantFiles ' +
      'WHERE TenantID = :tenantid AND FileID = :fileid';
    Query.ParamByName('tenantid').AsInteger := ATenantID;
    Query.ParamByName('fileid').AsString := AFileID;
    Query.Open;

    if not Query.EOF then
    begin
      FilePath := Query.FieldByName('FilePath').AsString;

      if FileExists(FilePath) then
      begin
        FileStream := TFileStream.Create(FilePath, fmOpenRead);
        try
          AStream.CopyFrom(FileStream, FileStream.Size);
          Result := True;
        finally
          FileStream.Free;
        end;
      end;
    end;
  finally
    Query.Free;
  end;
end;

procedure TFileStorageService.DeleteFile(ATenantID: Integer; const AFileID: string);  
var
  Query: TSQLQuery;
  FilePath: string;
begin
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FConnection;

    // Récupérer le chemin du fichier
    Query.SQL.Text :=
      'SELECT FilePath FROM TenantFiles ' +
      'WHERE TenantID = :tenantid AND FileID = :fileid';
    Query.ParamByName('tenantid').AsInteger := ATenantID;
    Query.ParamByName('fileid').AsString := AFileID;
    Query.Open;

    if not Query.EOF then
    begin
      FilePath := Query.FieldByName('FilePath').AsString;
      Query.Close;

      // Supprimer de la base de données
      Query.SQL.Text :=
        'DELETE FROM TenantFiles ' +
        'WHERE TenantID = :tenantid AND FileID = :fileid';
      Query.ParamByName('tenantid').AsInteger := ATenantID;
      Query.ParamByName('fileid').AsString := AFileID;
      Query.ExecSQL;

      // Supprimer le fichier physique
      if FileExists(FilePath) then
        DeleteFile(FilePath);
    end;
  finally
    Query.Free;
  end;
end;

function TFileStorageService.GetFileList(ATenantID: Integer): TStringList;  
var
  Query: TSQLQuery;
begin
  Result := TStringList.Create;
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FConnection;
    Query.SQL.Text :=
      'SELECT FileID, FileName, FileSize, UploadDate ' +
      'FROM TenantFiles ' +
      'WHERE TenantID = :tenantid ' +
      'ORDER BY UploadDate DESC';
    Query.ParamByName('tenantid').AsInteger := ATenantID;
    Query.Open;

    while not Query.EOF do
    begin
      Result.Add(Format('%s|%s|%d|%s', [
        Query.FieldByName('FileID').AsString,
        Query.FieldByName('FileName').AsString,
        Query.FieldByName('FileSize').AsLargeInt,
        DateTimeToStr(Query.FieldByName('UploadDate').AsDateTime)
      ]));
      Query.Next;
    end;
  finally
    Query.Free;
  end;
end;

function TFileStorageService.GetUsedStorage(ATenantID: Integer): Int64;  
var
  TenantPath: string;
begin
  TenantPath := GetTenantPath(ATenantID);
  Result := CalculateDirectorySize(TenantPath);
end;

function TFileStorageService.CheckQuota(ATenantID: Integer; AFileSize: Int64): Boolean;  
var
  SubscriptionMgr: TSubscriptionManager;
  StorageLimit, UsedStorage: Int64;
begin
  SubscriptionMgr := TSubscriptionManager.Create(FConnection);
  try
    StorageLimit := SubscriptionMgr.GetStorageLimit(ATenantID);
    UsedStorage := GetUsedStorage(ATenantID);

    Result := (UsedStorage + AFileSize) <= StorageLimit;
  finally
    SubscriptionMgr.Free;
  end;
end;

end.
```

## Système de permissions et rôles

### Gestion des permissions granulaires

```pascal
unit TenantPermissions;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, SQLdb, Generics.Collections;

type
  TPermission = class
  private
    FID: Integer;
    FName: string;
    FDescription: string;
    FResource: string;  // 'customers', 'invoices', 'reports', etc.
    FAction: string;    // 'create', 'read', 'update', 'delete'
  public
    property ID: Integer read FID write FID;
    property Name: string read FName write FName;
    property Description: string read FDescription write FDescription;
    property Resource: string read FResource write FResource;
    property Action: string read FAction write FAction;
  end;

  TRole = class
  private
    FID: Integer;
    FTenantID: Integer;
    FName: string;
    FDescription: string;
    FPermissions: TList;
  public
    constructor Create;
    destructor Destroy; override;
    property ID: Integer read FID write FID;
    property TenantID: Integer read FTenantID write FTenantID;
    property Name: string read FName write FName;
    property Description: string read FDescription write FDescription;
    property Permissions: TList read FPermissions;
  end;

  TPermissionManager = class
  private
    FConnection: TSQLConnection;
    FPermissionCache: TDictionary<Integer, TList>; // UserID -> Permissions
  public
    constructor Create(AConnection: TSQLConnection);
    destructor Destroy; override;

    // Gestion des rôles
    function CreateRole(ATenantID: Integer; const AName, ADescription: string): TRole;
    function GetRole(ATenantID, ARoleID: Integer): TRole;
    procedure AssignPermissionToRole(ARoleID, APermissionID: Integer);

    // Gestion des utilisateurs
    procedure AssignRoleToUser(AUserID, ARoleID: Integer);
    function GetUserPermissions(AUserID: Integer): TList;

    // Vérification des permissions
    function UserCan(AUserID: Integer; const AResource, AAction: string): Boolean;
    procedure RequirePermission(AUserID: Integer; const AResource, AAction: string);
  end;

implementation

uses
  TenantContext;

{ TRole }

constructor TRole.Create;  
begin
  inherited Create;
  FPermissions := TList.Create;
end;

destructor TRole.Destroy;  
var
  i: Integer;
begin
  for i := 0 to FPermissions.Count - 1 do
    TPermission(FPermissions[i]).Free;
  FPermissions.Free;
  inherited Destroy;
end;

{ TPermissionManager }

constructor TPermissionManager.Create(AConnection: TSQLConnection);  
begin
  inherited Create;
  FConnection := AConnection;
  FPermissionCache := TDictionary<Integer, TList>.Create;
end;

destructor TPermissionManager.Destroy;  
var
  PermList: TList;
begin
  // Libérer le cache
  for PermList in FPermissionCache.Values do
  begin
    // Les permissions dans le cache sont des références
    // Ne pas les libérer ici
    PermList.Free;
  end;
  FPermissionCache.Free;
  inherited Destroy;
end;

function TPermissionManager.CreateRole(ATenantID: Integer;
  const AName, ADescription: string): TRole;
var
  Query: TSQLQuery;
begin
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FConnection;
    Query.SQL.Text :=
      'INSERT INTO Roles (TenantID, Name, Description) ' +
      'VALUES (:tenantid, :name, :description)';
    Query.ParamByName('tenantid').AsInteger := ATenantID;
    Query.ParamByName('name').AsString := AName;
    Query.ParamByName('description').AsString := ADescription;
    Query.ExecSQL;

    // Récupérer l'ID du rôle créé
    Query.SQL.Text := 'SELECT LAST_INSERT_ID() AS ID';
    Query.Open;

    Result := TRole.Create;
    Result.ID := Query.FieldByName('ID').AsInteger;
    Result.TenantID := ATenantID;
    Result.Name := AName;
    Result.Description := ADescription;
  finally
    Query.Free;
  end;
end;

function TPermissionManager.GetRole(ATenantID, ARoleID: Integer): TRole;  
var
  Query: TSQLQuery;
  Permission: TPermission;
begin
  Result := nil;
  Query := TSQLQuery.Create(nil);
  try
    // Récupérer le rôle
    Query.Database := FConnection;
    Query.SQL.Text :=
      'SELECT ID, TenantID, Name, Description ' +
      'FROM Roles ' +
      'WHERE ID = :roleid AND TenantID = :tenantid';
    Query.ParamByName('roleid').AsInteger := ARoleID;
    Query.ParamByName('tenantid').AsInteger := ATenantID;
    Query.Open;

    if not Query.EOF then
    begin
      Result := TRole.Create;
      Result.ID := Query.FieldByName('ID').AsInteger;
      Result.TenantID := Query.FieldByName('TenantID').AsInteger;
      Result.Name := Query.FieldByName('Name').AsString;
      Result.Description := Query.FieldByName('Description').AsString;

      Query.Close;

      // Récupérer les permissions associées
      Query.SQL.Text :=
        'SELECT p.ID, p.Name, p.Description, p.Resource, p.Action ' +
        'FROM Permissions p ' +
        'INNER JOIN RolePermissions rp ON p.ID = rp.PermissionID ' +
        'WHERE rp.RoleID = :roleid';
      Query.ParamByName('roleid').AsInteger := ARoleID;
      Query.Open;

      while not Query.EOF do
      begin
        Permission := TPermission.Create;
        Permission.ID := Query.FieldByName('ID').AsInteger;
        Permission.Name := Query.FieldByName('Name').AsString;
        Permission.Description := Query.FieldByName('Description').AsString;
        Permission.Resource := Query.FieldByName('Resource').AsString;
        Permission.Action := Query.FieldByName('Action').AsString;

        Result.Permissions.Add(Permission);
        Query.Next;
      end;
    end;
  finally
    Query.Free;
  end;
end;

procedure TPermissionManager.AssignPermissionToRole(ARoleID, APermissionID: Integer);  
var
  Query: TSQLQuery;
begin
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FConnection;
    Query.SQL.Text :=
      'INSERT INTO RolePermissions (RoleID, PermissionID) ' +
      'VALUES (:roleid, :permid) ' +
      'ON DUPLICATE KEY UPDATE RoleID = RoleID'; // Ignorer si existe déjà
    Query.ParamByName('roleid').AsInteger := ARoleID;
    Query.ParamByName('permid').AsInteger := APermissionID;
    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

procedure TPermissionManager.AssignRoleToUser(AUserID, ARoleID: Integer);  
var
  Query: TSQLQuery;
begin
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FConnection;
    Query.SQL.Text :=
      'INSERT INTO UserRoles (UserID, RoleID) ' +
      'VALUES (:userid, :roleid) ' +
      'ON DUPLICATE KEY UPDATE RoleID = :roleid';
    Query.ParamByName('userid').AsInteger := AUserID;
    Query.ParamByName('roleid').AsInteger := ARoleID;
    Query.ExecSQL;

    // Invalider le cache pour cet utilisateur
    if FPermissionCache.ContainsKey(AUserID) then
      FPermissionCache.Remove(AUserID);
  finally
    Query.Free;
  end;
end;

function TPermissionManager.GetUserPermissions(AUserID: Integer): TList;  
var
  Query: TSQLQuery;
  Permission: TPermission;
begin
  // Vérifier le cache
  if FPermissionCache.ContainsKey(AUserID) then
  begin
    Result := FPermissionCache[AUserID];
    Exit;
  end;

  Result := TList.Create;
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FConnection;
    Query.SQL.Text :=
      'SELECT DISTINCT p.ID, p.Name, p.Description, p.Resource, p.Action ' +
      'FROM Permissions p ' +
      'INNER JOIN RolePermissions rp ON p.ID = rp.PermissionID ' +
      'INNER JOIN UserRoles ur ON rp.RoleID = ur.RoleID ' +
      'WHERE ur.UserID = :userid';
    Query.ParamByName('userid').AsInteger := AUserID;
    Query.Open;

    while not Query.EOF do
    begin
      Permission := TPermission.Create;
      Permission.ID := Query.FieldByName('ID').AsInteger;
      Permission.Name := Query.FieldByName('Name').AsString;
      Permission.Description := Query.FieldByName('Description').AsString;
      Permission.Resource := Query.FieldByName('Resource').AsString;
      Permission.Action := Query.FieldByName('Action').AsString;

      Result.Add(Permission);
      Query.Next;
    end;
  finally
    Query.Free;
  end;

  // Mettre en cache
  FPermissionCache.Add(AUserID, Result);
end;

function TPermissionManager.UserCan(AUserID: Integer;
  const AResource, AAction: string): Boolean;
var
  Permissions: TList;
  i: Integer;
  Permission: TPermission;
begin
  Result := False;
  Permissions := GetUserPermissions(AUserID);

  for i := 0 to Permissions.Count - 1 do
  begin
    Permission := TPermission(Permissions[i]);
    if (Permission.Resource = AResource) and (Permission.Action = AAction) then
    begin
      Result := True;
      Break;
    end;
  end;
end;

procedure TPermissionManager.RequirePermission(AUserID: Integer;
  const AResource, AAction: string);
begin
  if not UserCan(AUserID, AResource, AAction) then
    raise Exception.CreateFmt('Permission refusée: %s.%s', [AResource, AAction]);
end;

end.
```

## Monitoring et métriques pour SaaS

### Service de collecte de métriques

```pascal
unit TenantMetrics;

interface

uses
  Classes, SysUtils, SQLdb, DateUtils;

type
  TMetricType = (mtCounter, mtGauge, mtHistogram);

  TMetric = record
    TenantID: Integer;
    MetricName: string;
    MetricType: TMetricType;
    Value: Double;
    Timestamp: TDateTime;
    Tags: string; // Format JSON: {"key": "value", ...}
  end;

  TMetricsCollector = class
  private
    FConnection: TSQLConnection;
    FBuffer: array of TMetric;
    FBufferSize: Integer;
    procedure FlushBuffer;
  public
    constructor Create(AConnection: TSQLConnection);
    destructor Destroy; override;

    // Enregistrer des métriques
    procedure RecordCounter(ATenantID: Integer; const AName: string;
                           AValue: Double; const ATags: string = '');
    procedure RecordGauge(ATenantID: Integer; const AName: string;
                         AValue: Double; const ATags: string = '');
    procedure RecordHistogram(ATenantID: Integer; const AName: string;
                             AValue: Double; const ATags: string = '');

    // Requêtes de métriques
    function GetMetricValue(ATenantID: Integer; const AName: string;
                           AStartDate, AEndDate: TDateTime): Double;
    function GetMetricSeries(ATenantID: Integer; const AName: string;
                            AStartDate, AEndDate: TDateTime;
                            const AInterval: string): TStringList;

    // Métriques système
    procedure RecordAPICall(ATenantID: Integer; const AEndpoint: string;
                           AResponseTime: Integer; AStatusCode: Integer);
    procedure RecordPageView(ATenantID: Integer; const APage: string);
    procedure RecordError(ATenantID: Integer; const AError: string);
  end;

implementation

uses
  fpjson, jsonparser;

{ TMetricsCollector }

constructor TMetricsCollector.Create(AConnection: TSQLConnection);  
begin
  inherited Create;
  FConnection := AConnection;
  FBufferSize := 0;
  SetLength(FBuffer, 100); // Buffer de 100 métriques
end;

destructor TMetricsCollector.Destroy;  
begin
  FlushBuffer;
  inherited Destroy;
end;

procedure TMetricsCollector.FlushBuffer;  
var
  Query: TSQLQuery;
  i: Integer;
  TypeStr: string;
begin
  if FBufferSize = 0 then
    Exit;

  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FConnection;

    FConnection.StartTransaction;
    try
      for i := 0 to FBufferSize - 1 do
      begin
        case FBuffer[i].MetricType of
          mtCounter: TypeStr := 'counter';
          mtGauge: TypeStr := 'gauge';
          mtHistogram: TypeStr := 'histogram';
        end;

        Query.SQL.Text :=
          'INSERT INTO Metrics (TenantID, MetricName, MetricType, Value, Timestamp, Tags) ' +
          'VALUES (:tenantid, :name, :type, :value, :ts, :tags)';
        Query.ParamByName('tenantid').AsInteger := FBuffer[i].TenantID;
        Query.ParamByName('name').AsString := FBuffer[i].MetricName;
        Query.ParamByName('type').AsString := TypeStr;
        Query.ParamByName('value').AsFloat := FBuffer[i].Value;
        Query.ParamByName('ts').AsDateTime := FBuffer[i].Timestamp;
        Query.ParamByName('tags').AsString := FBuffer[i].Tags;
        Query.ExecSQL;
      end;

      FConnection.Commit;
      FBufferSize := 0;
    except
      FConnection.Rollback;
      raise;
    end;
  finally
    Query.Free;
  end;
end;

procedure TMetricsCollector.RecordCounter(ATenantID: Integer;
  const AName: string; AValue: Double; const ATags: string);
begin
  if FBufferSize >= Length(FBuffer) then
    FlushBuffer;

  FBuffer[FBufferSize].TenantID := ATenantID;
  FBuffer[FBufferSize].MetricName := AName;
  FBuffer[FBufferSize].MetricType := mtCounter;
  FBuffer[FBufferSize].Value := AValue;
  FBuffer[FBufferSize].Timestamp := Now;
  FBuffer[FBufferSize].Tags := ATags;

  Inc(FBufferSize);
end;

procedure TMetricsCollector.RecordGauge(ATenantID: Integer;
  const AName: string; AValue: Double; const ATags: string);
begin
  if FBufferSize >= Length(FBuffer) then
    FlushBuffer;

  FBuffer[FBufferSize].TenantID := ATenantID;
  FBuffer[FBufferSize].MetricName := AName;
  FBuffer[FBufferSize].MetricType := mtGauge;
  FBuffer[FBufferSize].Value := AValue;
  FBuffer[FBufferSize].Timestamp := Now;
  FBuffer[FBufferSize].Tags := ATags;

  Inc(FBufferSize);
end;

procedure TMetricsCollector.RecordHistogram(ATenantID: Integer;
  const AName: string; AValue: Double; const ATags: string);
begin
  if FBufferSize >= Length(FBuffer) then
    FlushBuffer;

  FBuffer[FBufferSize].TenantID := ATenantID;
  FBuffer[FBufferSize].MetricName := AName;
  FBuffer[FBufferSize].MetricType := mtHistogram;
  FBuffer[FBufferSize].Value := AValue;
  FBuffer[FBufferSize].Timestamp := Now;
  FBuffer[FBufferSize].Tags := ATags;

  Inc(FBufferSize);
end;

function TMetricsCollector.GetMetricValue(ATenantID: Integer;
  const AName: string; AStartDate, AEndDate: TDateTime): Double;
var
  Query: TSQLQuery;
begin
  Result := 0;
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FConnection;
    Query.SQL.Text :=
      'SELECT SUM(Value) AS Total ' +
      'FROM Metrics ' +
      'WHERE TenantID = :tenantid ' +
      '  AND MetricName = :name ' +
      '  AND Timestamp BETWEEN :start AND :end';
    Query.ParamByName('tenantid').AsInteger := ATenantID;
    Query.ParamByName('name').AsString := AName;
    Query.ParamByName('start').AsDateTime := AStartDate;
    Query.ParamByName('end').AsDateTime := AEndDate;
    Query.Open;

    if not Query.EOF then
      Result := Query.FieldByName('Total').AsFloat;
  finally
    Query.Free;
  end;
end;

function TMetricsCollector.GetMetricSeries(ATenantID: Integer;
  const AName: string; AStartDate, AEndDate: TDateTime;
  const AInterval: string): TStringList;
var
  Query: TSQLQuery;
  GroupByClause: string;
begin
  Result := TStringList.Create;

  // Déterminer le GROUP BY selon l'intervalle
  if AInterval = 'hour' then
    GroupByClause := 'DATE_FORMAT(Timestamp, ''%Y-%m-%d %H:00:00'')'
  else if AInterval = 'day' then
    GroupByClause := 'DATE(Timestamp)'
  else if AInterval = 'month' then
    GroupByClause := 'DATE_FORMAT(Timestamp, ''%Y-%m-01'')'
  else
    GroupByClause := 'Timestamp';

  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FConnection;
    Query.SQL.Text := Format(
      'SELECT %s AS Period, SUM(Value) AS Total ' +
      'FROM Metrics ' +
      'WHERE TenantID = :tenantid ' +
      '  AND MetricName = :name ' +
      '  AND Timestamp BETWEEN :start AND :end ' +
      'GROUP BY Period ' +
      'ORDER BY Period', [GroupByClause]);
    Query.ParamByName('tenantid').AsInteger := ATenantID;
    Query.ParamByName('name').AsString := AName;
    Query.ParamByName('start').AsDateTime := AStartDate;
    Query.ParamByName('end').AsDateTime := AEndDate;
    Query.Open;

    while not Query.EOF do
    begin
      Result.Add(Format('%s=%f', [
        Query.FieldByName('Period').AsString,
        Query.FieldByName('Total').AsFloat
      ]));
      Query.Next;
    end;
  finally
    Query.Free;
  end;
end;

procedure TMetricsCollector.RecordAPICall(ATenantID: Integer;
  const AEndpoint: string; AResponseTime, AStatusCode: Integer);
var
  Tags: TJSONObject;
begin
  Tags := TJSONObject.Create;
  try
    Tags.Add('endpoint', AEndpoint);
    Tags.Add('status_code', AStatusCode);

    RecordCounter(ATenantID, 'api.calls', 1, Tags.AsJSON);
    RecordHistogram(ATenantID, 'api.response_time', AResponseTime, Tags.AsJSON);
  finally
    Tags.Free;
  end;
end;

procedure TMetricsCollector.RecordPageView(ATenantID: Integer; const APage: string);  
var
  Tags: TJSONObject;
begin
  Tags := TJSONObject.Create;
  try
    Tags.Add('page', APage);
    RecordCounter(ATenantID, 'page.views', 1, Tags.AsJSON);
  finally
    Tags.Free;
  end;
end;

procedure TMetricsCollector.RecordError(ATenantID: Integer; const AError: string);  
var
  Tags: TJSONObject;
begin
  Tags := TJSONObject.Create;
  try
    Tags.Add('error_type', AError);
    RecordCounter(ATenantID, 'errors', 1, Tags.AsJSON);
  finally
    Tags.Free;
  end;
end;

end.
```

## Système de notification multi-canal

### Service de notifications

```pascal
unit TenantNotifications;

interface

uses
  Classes, SysUtils, SQLdb;

type
  TNotificationChannel = (ncEmail, ncSMS, ncPush, ncInApp, ncWebhook);
  TNotificationPriority = (npLow, npNormal, npHigh, npUrgent);

  TNotification = class
  private
    FID: Integer;
    FTenantID: Integer;
    FUserID: Integer;
    FTitle: string;
    FMessage: string;
    FChannel: TNotificationChannel;
    FPriority: TNotificationPriority;
    FData: string; // JSON avec données supplémentaires
    FIsRead: Boolean;
    FCreatedAt: TDateTime;
    FSentAt: TDateTime;
  public
    property ID: Integer read FID write FID;
    property TenantID: Integer read FTenantID write FTenantID;
    property UserID: Integer read FUserID write FUserID;
    property Title: string read FTitle write FTitle;
    property Message: string read FMessage write FMessage;
    property Channel: TNotificationChannel read FChannel write FChannel;
    property Priority: TNotificationPriority read FPriority write FPriority;
    property Data: string read FData write FData;
    property IsRead: Boolean read FIsRead write FIsRead;
    property CreatedAt: TDateTime read FCreatedAt write FCreatedAt;
    property SentAt: TDateTime read FSentAt write FSentAt;
  end;

  TNotificationService = class
  private
    FConnection: TSQLConnection;
    function ChannelToString(AChannel: TNotificationChannel): string;
    function StringToChannel(const AStr: string): TNotificationChannel;
    function PriorityToString(APriority: TNotificationPriority): string;
  public
    constructor Create(AConnection: TSQLConnection);

    // Créer et envoyer des notifications
    function SendNotification(ATenantID, AUserID: Integer;
                             const ATitle, AMessage: string;
                             AChannel: TNotificationChannel;
                             APriority: TNotificationPriority = npNormal;
                             const AData: string = ''): Integer;

    procedure SendBulkNotification(ATenantID: Integer;
                                  AUserIDs: array of Integer;
                                  const ATitle, AMessage: string;
                                  AChannel: TNotificationChannel);

    // Gestion des notifications
    function GetUserNotifications(AUserID: Integer;
                                 AUnreadOnly: Boolean = False): TList;
    procedure MarkAsRead(ANotificationID: Integer);
    procedure MarkAllAsRead(AUserID: Integer);
    function GetUnreadCount(AUserID: Integer): Integer;

    // Envoi effectif selon le canal
    procedure SendEmailNotification(ATenantID, AUserID: Integer;
                                   const ATitle, AMessage: string);
    procedure SendPushNotification(ATenantID, AUserID: Integer;
                                  const ATitle, AMessage: string);
  end;

implementation

uses
  TenantContext;

{ TNotificationService }

constructor TNotificationService.Create(AConnection: TSQLConnection);  
begin
  inherited Create;
  FConnection := AConnection;
end;

function TNotificationService.ChannelToString(AChannel: TNotificationChannel): string;  
begin
  case AChannel of
    ncEmail: Result := 'email';
    ncSMS: Result := 'sms';
    ncPush: Result := 'push';
    ncInApp: Result := 'in_app';
    ncWebhook: Result := 'webhook';
  end;
end;

function TNotificationService.StringToChannel(const AStr: string): TNotificationChannel;  
begin
  if AStr = 'email' then
    Result := ncEmail
  else if AStr = 'sms' then
    Result := ncSMS
  else if AStr = 'push' then
    Result := ncPush
  else if AStr = 'webhook' then
    Result := ncWebhook
  else
    Result := ncInApp;
end;

function TNotificationService.PriorityToString(APriority: TNotificationPriority): string;  
begin
  case APriority of
    npLow: Result := 'low';
    npNormal: Result := 'normal';
    npHigh: Result := 'high';
    npUrgent: Result := 'urgent';
  end;
end;

function TNotificationService.SendNotification(ATenantID, AUserID: Integer;
  const ATitle, AMessage: string; AChannel: TNotificationChannel;
  APriority: TNotificationPriority; const AData: string): Integer;
var
  Query: TSQLQuery;
begin
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FConnection;
    Query.SQL.Text :=
      'INSERT INTO Notifications (TenantID, UserID, Title, Message, Channel, Priority, Data, IsRead, CreatedAt) ' +
      'VALUES (:tenantid, :userid, :title, :message, :channel, :priority, :data, 0, :now)';
    Query.ParamByName('tenantid').AsInteger := ATenantID;
    Query.ParamByName('userid').AsInteger := AUserID;
    Query.ParamByName('title').AsString := ATitle;
    Query.ParamByName('message').AsString := AMessage;
    Query.ParamByName('channel').AsString := ChannelToString(AChannel);
    Query.ParamByName('priority').AsString := PriorityToString(APriority);
    Query.ParamByName('data').AsString := AData;
    Query.ParamByName('now').AsDateTime := Now;
    Query.ExecSQL;

    Query.SQL.Text := 'SELECT LAST_INSERT_ID() AS ID';
    Query.Open;
    Result := Query.FieldByName('ID').AsInteger;

    // Envoyer selon le canal
    case AChannel of
      ncEmail: SendEmailNotification(ATenantID, AUserID, ATitle, AMessage);
      ncPush: SendPushNotification(ATenantID, AUserID, ATitle, AMessage);
      ncInApp: ; // Déjà enregistré en base
      // Autres canaux à implémenter
    end;
  finally
    Query.Free;
  end;
end;

procedure TNotificationService.SendBulkNotification(ATenantID: Integer;
  AUserIDs: array of Integer; const ATitle, AMessage: string;
  AChannel: TNotificationChannel);
var
  i: Integer;
begin
  for i := Low(AUserIDs) to High(AUserIDs) do
    SendNotification(ATenantID, AUserIDs[i], ATitle, AMessage, AChannel);
end;

function TNotificationService.GetUserNotifications(AUserID: Integer;
  AUnreadOnly: Boolean): TList;
var
  Query: TSQLQuery;
  Notification: TNotification;
  WhereClause: string;
begin
  Result := TList.Create;
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FConnection;

    WhereClause := 'WHERE UserID = :userid';
    if AUnreadOnly then
      WhereClause := WhereClause + ' AND IsRead = 0';

    Query.SQL.Text :=
      'SELECT ID, TenantID, UserID, Title, Message, Channel, Priority, ' +
      '       Data, IsRead, CreatedAt, SentAt ' +
      'FROM Notifications ' +
      WhereClause + ' ' +
      'ORDER BY CreatedAt DESC ' +
      'LIMIT 100';
    Query.ParamByName('userid').AsInteger := AUserID;
    Query.Open;

    while not Query.EOF do
    begin
      Notification := TNotification.Create;
      Notification.ID := Query.FieldByName('ID').AsInteger;
      Notification.TenantID := Query.FieldByName('TenantID').AsInteger;
      Notification.UserID := Query.FieldByName('UserID').AsInteger;
      Notification.Title := Query.FieldByName('Title').AsString;
      Notification.Message := Query.FieldByName('Message').AsString;
      Notification.Channel := StringToChannel(Query.FieldByName('Channel').AsString);
      Notification.Data := Query.FieldByName('Data').AsString;
      Notification.IsRead := Query.FieldByName('IsRead').AsBoolean;
      Notification.CreatedAt := Query.FieldByName('CreatedAt').AsDateTime;

      if not Query.FieldByName('SentAt').IsNull then
        Notification.SentAt := Query.FieldByName('SentAt').AsDateTime;

      Result.Add(Notification);
      Query.Next;
    end;
  finally
    Query.Free;
  end;
end;

procedure TNotificationService.MarkAsRead(ANotificationID: Integer);  
var
  Query: TSQLQuery;
begin
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FConnection;
    Query.SQL.Text :=
      'UPDATE Notifications SET IsRead = 1 WHERE ID = :id';
    Query.ParamByName('id').AsInteger := ANotificationID;
    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

procedure TNotificationService.MarkAllAsRead(AUserID: Integer);  
var
  Query: TSQLQuery;
begin
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FConnection;
    Query.SQL.Text :=
      'UPDATE Notifications SET IsRead = 1 ' +
      'WHERE UserID = :userid AND IsRead = 0';
    Query.ParamByName('userid').AsInteger := AUserID;
    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

function TNotificationService.GetUnreadCount(AUserID: Integer): Integer;  
var
  Query: TSQLQuery;
begin
  Result := 0;
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FConnection;
    Query.SQL.Text :=
      'SELECT COUNT(*) AS UnreadCount ' +
      'FROM Notifications ' +
      'WHERE UserID = :userid AND IsRead = 0';
    Query.ParamByName('userid').AsInteger := AUserID;
    Query.Open;

    if not Query.EOF then
      Result := Query.FieldByName('UnreadCount').AsInteger;
  finally
    Query.Free;
  end;
end;

procedure TNotificationService.SendEmailNotification(ATenantID, AUserID: Integer;
  const ATitle, AMessage: string);
begin
  // Implémentation de l'envoi d'email
  // Utiliser SMTP avec Synapse ou Indy
  // À personnaliser selon les besoins

  WriteLn(Format('Envoi email à l''utilisateur %d du tenant %d', [AUserID, ATenantID]));
  WriteLn('Titre: ', ATitle);
  WriteLn('Message: ', AMessage);
end;

procedure TNotificationService.SendPushNotification(ATenantID, AUserID: Integer;
  const ATitle, AMessage: string);
begin
  // Implémentation de l'envoi de notification push
  // Utiliser Firebase Cloud Messaging ou similaire

  WriteLn(Format('Envoi push à l''utilisateur %d du tenant %d', [AUserID, ATenantID]));
  WriteLn('Titre: ', ATitle);
  WriteLn('Message: ', AMessage);
end;

end.
```

## Gestion des sauvegardes multi-tenant

### Service de sauvegarde

```pascal
unit TenantBackup;

interface

uses
  Classes, SysUtils, SQLdb, Process;

type
  TBackupType = (btFull, btIncremental, btDifferential);
  TBackupStatus = (bsPending, bsRunning, bsCompleted, bsFailed);

  TBackupJob = class
  private
    FID: Integer;
    FTenantID: Integer;
    FBackupType: TBackupType;
    FStatus: TBackupStatus;
    FStartTime: TDateTime;
    FEndTime: TDateTime;
    FFilePath: string;
    FFileSize: Int64;
    FErrorMessage: string;
  public
    property ID: Integer read FID write FID;
    property TenantID: Integer read FTenantID write FTenantID;
    property BackupType: TBackupType read FBackupType write FBackupType;
    property Status: TBackupStatus read FStatus write FStatus;
    property StartTime: TDateTime read FStartTime write FStartTime;
    property EndTime: TDateTime read FEndTime write FEndTime;
    property FilePath: string read FFilePath write FFilePath;
    property FileSize: Int64 read FFileSize write FFileSize;
    property ErrorMessage: string read FErrorMessage write FErrorMessage;
  end;

  TBackupService = class
  private
    FConnection: TSQLConnection;
    FBackupPath: string;
    function GetTenantBackupPath(ATenantID: Integer): string;
    function BackupTypeToString(AType: TBackupType): string;
    function StringToBackupType(const AStr: string): TBackupType;
    function StatusToString(AStatus: TBackupStatus): string;
  public
    constructor Create(AConnection: TSQLConnection; const ABackupPath: string);

    // Créer une sauvegarde
    function CreateBackup(ATenantID: Integer;
                         ABackupType: TBackupType = btFull): TBackupJob;

    // Restaurer depuis une sauvegarde
    procedure RestoreBackup(ABackupID: Integer);

    // Gestion des sauvegardes
    function GetBackupHistory(ATenantID: Integer; ALimit: Integer = 10): TList;
    procedure DeleteBackup(ABackupID: Integer);
    procedure ScheduleAutomaticBackup(ATenantID: Integer;
                                     const ASchedule: string);

    // Vérification et maintenance
    function VerifyBackup(ABackupID: Integer): Boolean;
    procedure CleanOldBackups(ATenantID: Integer; ADaysToKeep: Integer);
  end;

implementation

uses
  TenantContext, DateUtils;

{ TBackupService }

constructor TBackupService.Create(AConnection: TSQLConnection;
  const ABackupPath: string);
begin
  inherited Create;
  FConnection := AConnection;
  FBackupPath := IncludeTrailingPathDelimiter(ABackupPath);

  if not DirectoryExists(FBackupPath) then
    ForceDirectories(FBackupPath);
end;

function TBackupService.GetTenantBackupPath(ATenantID: Integer): string;  
begin
  Result := FBackupPath + 'tenant_' + IntToStr(ATenantID) + PathDelim;

  if not DirectoryExists(Result) then
    ForceDirectories(Result);
end;

function TBackupService.BackupTypeToString(AType: TBackupType): string;  
begin
  case AType of
    btFull: Result := 'full';
    btIncremental: Result := 'incremental';
    btDifferential: Result := 'differential';
  end;
end;

function TBackupService.StringToBackupType(const AStr: string): TBackupType;  
begin
  if AStr = 'incremental' then
    Result := btIncremental
  else if AStr = 'differential' then
    Result := btDifferential
  else
    Result := btFull;
end;

function TBackupService.StatusToString(AStatus: TBackupStatus): string;  
begin
  case AStatus of
    bsPending: Result := 'pending';
    bsRunning: Result := 'running';
    bsCompleted: Result := 'completed';
    bsFailed: Result := 'failed';
  end;
end;

function TBackupService.CreateBackup(ATenantID: Integer;
  ABackupType: TBackupType): TBackupJob;
var
  Query: TSQLQuery;
  BackupPath, BackupFile, Command: string;
  Process: TProcess;
  BackupID: Integer;
  SearchRec: TSearchRec;
begin
  Result := TBackupJob.Create;
  Result.TenantID := ATenantID;
  Result.BackupType := ABackupType;
  Result.Status := bsRunning;
  Result.StartTime := Now;

  // Enregistrer le job de sauvegarde
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FConnection;
    Query.SQL.Text :=
      'INSERT INTO BackupJobs (TenantID, BackupType, Status, StartTime) ' +
      'VALUES (:tenantid, :type, :status, :start)';
    Query.ParamByName('tenantid').AsInteger := ATenantID;
    Query.ParamByName('type').AsString := BackupTypeToString(ABackupType);
    Query.ParamByName('status').AsString := StatusToString(bsRunning);
    Query.ParamByName('start').AsDateTime := Result.StartTime;
    Query.ExecSQL;

    Query.SQL.Text := 'SELECT LAST_INSERT_ID() AS ID';
    Query.Open;
    BackupID := Query.FieldByName('ID').AsInteger;
    Result.ID := BackupID;
    Query.Close;

    try
      // Préparer le chemin de sauvegarde
      BackupPath := GetTenantBackupPath(ATenantID);
      BackupFile := BackupPath + Format('backup_%d_%s_%s.sql', [
        ATenantID,
        BackupTypeToString(ABackupType),
        FormatDateTime('yyyymmdd_hhnnss', Now)
      ]);

      // Créer la commande mysqldump (ou pg_dump pour PostgreSQL)
      {$IFDEF WINDOWS}
      Command := Format('mysqldump --user=%s --password=%s --host=%s --result-file="%s" ' +
                       '--where="TenantID=%d" %s',
                       [DBUser, DBPassword, DBHost, BackupFile, ATenantID, DBName]);
      {$ELSE}
      Command := Format('mysqldump -u %s -p%s -h %s -r "%s" ' +
                       '--where="TenantID=%d" %s',
                       [DBUser, DBPassword, DBHost, BackupFile, ATenantID, DBName]);
      {$ENDIF}

      // Exécuter la sauvegarde
      Process := TProcess.Create(nil);
      try
        Process.CommandLine := Command;
        Process.Options := Process.Options + [poWaitOnExit, poUsePipes];
        Process.Execute;

        if Process.ExitStatus = 0 then
        begin
          // Compresser la sauvegarde
          CompressFile(BackupFile, BackupFile + '.gz');
          DeleteFile(BackupFile);
          BackupFile := BackupFile + '.gz';

          Result.Status := bsCompleted;
          Result.FilePath := BackupFile;
          // FileSize() FPC prend un fichier ouvert, pas un chemin
          if FindFirst(BackupFile, faAnyFile, SearchRec) = 0 then
          begin
            Result.FileSize := SearchRec.Size;
            FindClose(SearchRec);
          end;
        end
        else
        begin
          Result.Status := bsFailed;
          Result.ErrorMessage := 'Échec de la commande mysqldump';
        end;
      finally
        Process.Free;
      end;

    except
      on E: Exception do
      begin
        Result.Status := bsFailed;
        Result.ErrorMessage := E.Message;
      end;
    end;

    // Mettre à jour le job
    Result.EndTime := Now;

    Query.SQL.Text :=
      'UPDATE BackupJobs ' +
      'SET Status = :status, EndTime = :end, FilePath = :path, ' +
      '    FileSize = :size, ErrorMessage = :error ' +
      'WHERE ID = :id';
    Query.ParamByName('status').AsString := StatusToString(Result.Status);
    Query.ParamByName('end').AsDateTime := Result.EndTime;
    Query.ParamByName('path').AsString := Result.FilePath;
    Query.ParamByName('size').AsLargeInt := Result.FileSize;
    Query.ParamByName('error').AsString := Result.ErrorMessage;
    Query.ParamByName('id').AsInteger := BackupID;
    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

procedure TBackupService.RestoreBackup(ABackupID: Integer);  
var
  Query: TSQLQuery;
  BackupFile, Command: string;
  Process: TProcess;
  TenantID: Integer;
begin
  Query := TSQLQuery.Create(nil);
  try
    // Récupérer les informations de la sauvegarde
    Query.Database := FConnection;
    Query.SQL.Text :=
      'SELECT TenantID, FilePath FROM BackupJobs ' +
      'WHERE ID = :id AND Status = ''completed''';
    Query.ParamByName('id').AsInteger := ABackupID;
    Query.Open;

    if Query.EOF then
      raise Exception.Create('Sauvegarde introuvable ou incomplète');

    TenantID := Query.FieldByName('TenantID').AsInteger;
    BackupFile := Query.FieldByName('FilePath').AsString;
    Query.Close;

    if not FileExists(BackupFile) then
      raise Exception.Create('Fichier de sauvegarde introuvable');

    // Décompresser si nécessaire
    if ExtractFileExt(BackupFile) = '.gz' then
    begin
      DecompressFile(BackupFile, ChangeFileExt(BackupFile, ''));
      BackupFile := ChangeFileExt(BackupFile, '');
    end;

    // Créer la commande de restauration
    {$IFDEF WINDOWS}
    Command := Format('mysql --user=%s --password=%s --host=%s %s < "%s"',
                     [DBUser, DBPassword, DBHost, DBName, BackupFile]);
    {$ELSE}
    Command := Format('mysql -u %s -p%s -h %s %s < "%s"',
                     [DBUser, DBPassword, DBHost, DBName, BackupFile]);
    {$ENDIF}

    // Exécuter la restauration
    Process := TProcess.Create(nil);
    try
      Process.CommandLine := Command;
      Process.Options := Process.Options + [poWaitOnExit];
      Process.Execute;

      if Process.ExitStatus <> 0 then
        raise Exception.Create('Échec de la restauration');
    finally
      Process.Free;
    end;

    // Nettoyer le fichier décompressé
    if ExtractFileExt(BackupFile) <> '.gz' then
      DeleteFile(BackupFile);
  finally
    Query.Free;
  end;
end;

function TBackupService.GetBackupHistory(ATenantID: Integer;
  ALimit: Integer): TList;
var
  Query: TSQLQuery;
  Job: TBackupJob;
begin
  Result := TList.Create;
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FConnection;
    Query.SQL.Text :=
      'SELECT ID, TenantID, BackupType, Status, StartTime, EndTime, ' +
      '       FilePath, FileSize, ErrorMessage ' +
      'FROM BackupJobs ' +
      'WHERE TenantID = :tenantid ' +
      'ORDER BY StartTime DESC ' +
      'LIMIT :limit';
    Query.ParamByName('tenantid').AsInteger := ATenantID;
    Query.ParamByName('limit').AsInteger := ALimit;
    Query.Open;

    while not Query.EOF do
    begin
      Job := TBackupJob.Create;
      Job.ID := Query.FieldByName('ID').AsInteger;
      Job.TenantID := Query.FieldByName('TenantID').AsInteger;
      Job.BackupType := StringToBackupType(Query.FieldByName('BackupType').AsString);
      Job.Status := bsCompleted; // Simplification
      Job.StartTime := Query.FieldByName('StartTime').AsDateTime;

      if not Query.FieldByName('EndTime').IsNull then
        Job.EndTime := Query.FieldByName('EndTime').AsDateTime;

      Job.FilePath := Query.FieldByName('FilePath').AsString;
      Job.FileSize := Query.FieldByName('FileSize').AsLargeInt;
      Job.ErrorMessage := Query.FieldByName('ErrorMessage').AsString;

      Result.Add(Job);
      Query.Next;
    end;
  finally
    Query.Free;
  end;
end;

procedure TBackupService.DeleteBackup(ABackupID: Integer);  
var
  Query: TSQLQuery;
  FilePath: string;
begin
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FConnection;

    // Récupérer le chemin du fichier
    Query.SQL.Text := 'SELECT FilePath FROM BackupJobs WHERE ID = :id';
    Query.ParamByName('id').AsInteger := ABackupID;
    Query.Open;

    if not Query.EOF then
    begin
      FilePath := Query.FieldByName('FilePath').AsString;
      Query.Close;

      // Supprimer le fichier
      if FileExists(FilePath) then
        DeleteFile(FilePath);

      // Supprimer l'enregistrement
      Query.SQL.Text := 'DELETE FROM BackupJobs WHERE ID = :id';
      Query.ParamByName('id').AsInteger := ABackupID;
      Query.ExecSQL;
    end;
  finally
    Query.Free;
  end;
end;

procedure TBackupService.ScheduleAutomaticBackup(ATenantID: Integer;
  const ASchedule: string);
var
  Query: TSQLQuery;
begin
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FConnection;
    Query.SQL.Text :=
      'INSERT INTO BackupSchedules (TenantID, Schedule, IsActive) ' +
      'VALUES (:tenantid, :schedule, 1) ' +
      'ON DUPLICATE KEY UPDATE Schedule = :schedule, IsActive = 1';
    Query.ParamByName('tenantid').AsInteger := ATenantID;
    Query.ParamByName('schedule').AsString := ASchedule; // Ex: "0 2 * * *" (cron)
    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

function TBackupService.VerifyBackup(ABackupID: Integer): Boolean;  
var
  Query: TSQLQuery;
  FilePath: string;
  SearchRec: TSearchRec;
begin
  Result := False;
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FConnection;
    Query.SQL.Text :=
      'SELECT FilePath, FileSize FROM BackupJobs ' +
      'WHERE ID = :id AND Status = ''completed''';
    Query.ParamByName('id').AsInteger := ABackupID;
    Query.Open;

    if not Query.EOF then
    begin
      FilePath := Query.FieldByName('FilePath').AsString;

      // Vérifier l'existence et la taille du fichier
      // FileSize() FPC prend un fichier ouvert, pas un chemin
      if FileExists(FilePath) and (FindFirst(FilePath, faAnyFile, SearchRec) = 0) then
      begin
        Result := SearchRec.Size = Query.FieldByName('FileSize').AsLargeInt;
        FindClose(SearchRec);
      end;
    end;
  finally
    Query.Free;
  end;
end;

procedure TBackupService.CleanOldBackups(ATenantID: Integer; ADaysToKeep: Integer);  
var
  Query: TSQLQuery;
  CutoffDate: TDateTime;
begin
  CutoffDate := IncDay(Now, -ADaysToKeep);

  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FConnection;

    // Supprimer les fichiers physiques
    Query.SQL.Text :=
      'SELECT ID, FilePath FROM BackupJobs ' +
      'WHERE TenantID = :tenantid AND StartTime < :cutoff';
    Query.ParamByName('tenantid').AsInteger := ATenantID;
    Query.ParamByName('cutoff').AsDateTime := CutoffDate;
    Query.Open;

    while not Query.EOF do
    begin
      if FileExists(Query.FieldByName('FilePath').AsString) then
        DeleteFile(Query.FieldByName('FilePath').AsString);
      Query.Next;
    end;

    Query.Close;

    // Supprimer les enregistrements
    Query.SQL.Text :=
      'DELETE FROM BackupJobs ' +
      'WHERE TenantID = :tenantid AND StartTime < :cutoff';
    Query.ParamByName('tenantid').AsInteger := ATenantID;
    Query.ParamByName('cutoff').AsDateTime := CutoffDate;
    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

end.
```

## Gestion des webhooks et événements

### Système d'événements

```pascal
unit TenantWebhooks;

interface

uses
  Classes, SysUtils, SQLdb, fphttpclient, fpjson;

type
  TWebhookEvent = (
    weUserCreated,
    weUserUpdated,
    weUserDeleted,
    weDataCreated,
    weDataUpdated,
    weDataDeleted,
    wePaymentReceived,
    weSubscriptionChanged,
    weCustomEvent
  );

  TWebhook = class
  private
    FID: Integer;
    FTenantID: Integer;
    FURL: string;
    FEvents: string; // JSON array des événements
    FSecret: string;
    FIsActive: Boolean;
  public
    property ID: Integer read FID write FID;
    property TenantID: Integer read FTenantID write FTenantID;
    property URL: string read FURL write FURL;
    property Events: string read FEvents write FEvents;
    property Secret: string read FSecret write FSecret;
    property IsActive: Boolean read FIsActive write FIsActive;
  end;

  TWebhookManager = class
  private
    FConnection: TSQLConnection;
    function EventToString(AEvent: TWebhookEvent): string;
    function GenerateSignature(const APayload, ASecret: string): string;
  public
    constructor Create(AConnection: TSQLConnection);

    // Gestion des webhooks
    function RegisterWebhook(ATenantID: Integer; const AURL: string;
                            AEvents: array of TWebhookEvent): Integer;
    procedure UpdateWebhook(AWebhookID: Integer; const AURL: string;
                           AEvents: array of TWebhookEvent);
    procedure DeleteWebhook(AWebhookID: Integer);
    function GetTenantWebhooks(ATenantID: Integer): TList;

    // Déclenchement des événements
    procedure TriggerEvent(ATenantID: Integer; AEvent: TWebhookEvent;
                          const AData: TJSONObject);
    procedure SendWebhook(AWebhook: TWebhook; AEvent: TWebhookEvent;
                         const AData: TJSONObject);

    // Historique et retry
    procedure LogWebhookCall(AWebhookID: Integer; AEvent: TWebhookEvent;
                            const APayload: string; AStatusCode: Integer;
                            const AResponse: string);
    procedure RetryFailedWebhooks;
  end;

implementation

uses
  TenantContext, DCPsha256, DCPbase64, jsonparser;

{ TWebhookManager }

constructor TWebhookManager.Create(AConnection: TSQLConnection);  
begin
  inherited Create;
  FConnection := AConnection;
end;

function TWebhookManager.EventToString(AEvent: TWebhookEvent): string;  
begin
  case AEvent of
    weUserCreated: Result := 'user.created';
    weUserUpdated: Result := 'user.updated';
    weUserDeleted: Result := 'user.deleted';
    weDataCreated: Result := 'data.created';
    weDataUpdated: Result := 'data.updated';
    weDataDeleted: Result := 'data.deleted';
    wePaymentReceived: Result := 'payment.received';
    weSubscriptionChanged: Result := 'subscription.changed';
    weCustomEvent: Result := 'custom.event';
  end;
end;

function TWebhookManager.GenerateSignature(const APayload, ASecret: string): string;  
var
  Hash: TDCP_sha256;
  Digest: array[0..31] of Byte;
begin
  Hash := TDCP_sha256.Create(nil);
  try
    Hash.Init;
    Hash.UpdateStr(APayload + ASecret);
    Hash.Final(Digest);
    Result := 'sha256=' + LowerCase(Hash.Final);
  finally
    Hash.Free;
  end;
end;

function TWebhookManager.RegisterWebhook(ATenantID: Integer;
  const AURL: string; AEvents: array of TWebhookEvent): Integer;
var
  Query: TSQLQuery;
  EventsArray: TJSONArray;
  i: Integer;
  Secret: string;
begin
  // Générer un secret unique
  Secret := GenerateRandomString(32);

  // Créer le tableau JSON des événements
  EventsArray := TJSONArray.Create;
  try
    for i := Low(AEvents) to High(AEvents) do
      EventsArray.Add(EventToString(AEvents[i]));

    Query := TSQLQuery.Create(nil);
    try
      Query.Database := FConnection;
      Query.SQL.Text :=
        'INSERT INTO Webhooks (TenantID, URL, Events, Secret, IsActive) ' +
        'VALUES (:tenantid, :url, :events, :secret, 1)';
      Query.ParamByName('tenantid').AsInteger := ATenantID;
      Query.ParamByName('url').AsString := AURL;
      Query.ParamByName('events').AsString := EventsArray.AsJSON;
      Query.ParamByName('secret').AsString := Secret;
      Query.ExecSQL;

      Query.SQL.Text := 'SELECT LAST_INSERT_ID() AS ID';
      Query.Open;
      Result := Query.FieldByName('ID').AsInteger;
    finally
      Query.Free;
    end;
  finally
    EventsArray.Free;
  end;
end;

procedure TWebhookManager.UpdateWebhook(AWebhookID: Integer;
  const AURL: string; AEvents: array of TWebhookEvent);
var
  Query: TSQLQuery;
  EventsArray: TJSONArray;
  i: Integer;
begin
  EventsArray := TJSONArray.Create;
  try
    for i := Low(AEvents) to High(AEvents) do
      EventsArray.Add(EventToString(AEvents[i]));

    Query := TSQLQuery.Create(nil);
    try
      Query.Database := FConnection;
      Query.SQL.Text :=
        'UPDATE Webhooks SET URL = :url, Events = :events ' +
        'WHERE ID = :id';
      Query.ParamByName('url').AsString := AURL;
      Query.ParamByName('events').AsString := EventsArray.AsJSON;
      Query.ParamByName('id').AsInteger := AWebhookID;
      Query.ExecSQL;
    finally
      Query.Free;
    end;
  finally
    EventsArray.Free;
  end;
end;

procedure TWebhookManager.DeleteWebhook(AWebhookID: Integer);  
var
  Query: TSQLQuery;
begin
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FConnection;
    Query.SQL.Text := 'DELETE FROM Webhooks WHERE ID = :id';
    Query.ParamByName('id').AsInteger := AWebhookID;
    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

function TWebhookManager.GetTenantWebhooks(ATenantID: Integer): TList;  
var
  Query: TSQLQuery;
  Webhook: TWebhook;
begin
  Result := TList.Create;
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FConnection;
    Query.SQL.Text :=
      'SELECT ID, TenantID, URL, Events, Secret, IsActive ' +
      'FROM Webhooks ' +
      'WHERE TenantID = :tenantid AND IsActive = 1';
    Query.ParamByName('tenantid').AsInteger := ATenantID;
    Query.Open;

    while not Query.EOF do
    begin
      Webhook := TWebhook.Create;
      Webhook.ID := Query.FieldByName('ID').AsInteger;
      Webhook.TenantID := Query.FieldByName('TenantID').AsInteger;
      Webhook.URL := Query.FieldByName('URL').AsString;
      Webhook.Events := Query.FieldByName('Events').AsString;
      Webhook.Secret := Query.FieldByName('Secret').AsString;
      Webhook.IsActive := Query.FieldByName('IsActive').AsBoolean;

      Result.Add(Webhook);
      Query.Next;
    end;
  finally
    Query.Free;
  end;
end;

procedure TWebhookManager.TriggerEvent(ATenantID: Integer;
  AEvent: TWebhookEvent; const AData: TJSONObject);
var
  Webhooks: TList;
  i: Integer;
  Webhook: TWebhook;
  EventsArray: TJSONArray;
  j: Integer;
  EventName: string;
begin
  Webhooks := GetTenantWebhooks(ATenantID);
  try
    EventName := EventToString(AEvent);

    for i := 0 to Webhooks.Count - 1 do
    begin
      Webhook := TWebhook(Webhooks[i]);

      // Vérifier si le webhook écoute cet événement
      EventsArray := GetJSON(Webhook.Events) as TJSONArray;
      try
        for j := 0 to EventsArray.Count - 1 do
        begin
          if EventsArray.Strings[j] = EventName then
          begin
            SendWebhook(Webhook, AEvent, AData);
            Break;
          end;
        end;
      finally
        EventsArray.Free;
      end;
    end;
  finally
    // Libérer les webhooks
    for i := 0 to Webhooks.Count - 1 do
      TWebhook(Webhooks[i]).Free;
    Webhooks.Free;
  end;
end;

procedure TWebhookManager.SendWebhook(AWebhook: TWebhook;
  AEvent: TWebhookEvent; const AData: TJSONObject);
var
  HTTPClient: TFPHTTPClient;
  Payload: TJSONObject;
  PayloadStr: string;
  Signature: string;
  Response: string;
  StatusCode: Integer;
begin
  // Créer le payload
  Payload := TJSONObject.Create;
  try
    Payload.Add('event', EventToString(AEvent));
    Payload.Add('timestamp', DateTimeToStr(Now));
    Payload.Add('tenant_id', AWebhook.TenantID);
    Payload.Add('data', AData.Clone as TJSONObject);

    PayloadStr := Payload.FormatJSON;

    // Générer la signature
    Signature := GenerateSignature(PayloadStr, AWebhook.Secret);

    // Envoyer la requête HTTP
    HTTPClient := TFPHTTPClient.Create(nil);
    try
      HTTPClient.AddHeader('Content-Type', 'application/json');
      HTTPClient.AddHeader('X-Webhook-Signature', Signature);
      HTTPClient.AddHeader('X-Webhook-Event', EventToString(AEvent));

      try
        HTTPClient.RequestBody := TStringStream.Create(PayloadStr);
        Response := HTTPClient.Post(AWebhook.URL);
        StatusCode := HTTPClient.ResponseStatusCode;
      except
        on E: Exception do
        begin
          Response := E.Message;
          StatusCode := 0;
        end;
      end;

      // Logger l'appel
      LogWebhookCall(AWebhook.ID, AEvent, PayloadStr, StatusCode, Response);
    finally
      HTTPClient.Free;
    end;
  finally
    Payload.Free;
  end;
end;

procedure TWebhookManager.LogWebhookCall(AWebhookID: Integer;
  AEvent: TWebhookEvent; const APayload: string; AStatusCode: Integer;
  const AResponse: string);
var
  Query: TSQLQuery;
begin
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FConnection;
    Query.SQL.Text :=
      'INSERT INTO WebhookLogs (WebhookID, Event, Payload, StatusCode, Response, CreatedAt) ' +
      'VALUES (:webhookid, :event, :payload, :status, :response, :now)';
    Query.ParamByName('webhookid').AsInteger := AWebhookID;
    Query.ParamByName('event').AsString := EventToString(AEvent);
    Query.ParamByName('payload').AsString := APayload;
    Query.ParamByName('status').AsInteger := AStatusCode;
    Query.ParamByName('response').AsString := Copy(AResponse, 1, 1000); // Limiter la taille
    Query.ParamByName('now').AsDateTime := Now;
    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

procedure TWebhookManager.RetryFailedWebhooks;  
var
  Query: TSQLQuery;
  WebhookID: Integer;
  Event: string;
  Payload: TJSONObject;
begin
  // Récupérer les webhooks échoués des dernières 24h
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FConnection;
    Query.SQL.Text :=
      'SELECT DISTINCT wl.WebhookID, wl.Event, wl.Payload ' +
      'FROM WebhookLogs wl ' +
      'INNER JOIN Webhooks w ON wl.WebhookID = w.ID ' +
      'WHERE wl.StatusCode NOT BETWEEN 200 AND 299 ' +
      '  AND wl.CreatedAt > DATE_SUB(NOW(), INTERVAL 24 HOUR) ' +
      '  AND w.IsActive = 1';
    Query.Open;

    while not Query.EOF do
    begin
      WebhookID := Query.FieldByName('WebhookID').AsInteger;
      Event := Query.FieldByName('Event').AsString;
      Payload := GetJSON(Query.FieldByName('Payload').AsString) as TJSONObject;

      try
        // Retry logique à implémenter
        // (exponential backoff, limite de tentatives, etc.)
      finally
        Payload.Free;
      end;

      Query.Next;
    end;
  finally
    Query.Free;
  end;
end;

end.
```

## Configuration et personnalisation par tenant

### Système de configuration

```pascal
unit TenantConfiguration;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, SQLdb, fpjson, Generics.Collections;

type
  TConfigScope = (csGlobal, csTenant, csUser);

  TConfigValue = class
  private
    FKey: string;
    FValue: string;
    FValueType: string; // 'string', 'integer', 'boolean', 'json'
    FScope: TConfigScope;
    FDescription: string;
  public
    property Key: string read FKey write FKey;
    property Value: string read FValue write FValue;
    property ValueType: string read FValueType write FValueType;
    property Scope: TConfigScope read FScope write FScope;
    property Description: string read FDescription write FDescription;

    function AsString: string;
    function AsInteger: Integer;
    function AsBoolean: Boolean;
    function AsJSON: TJSONObject;
  end;

  TConfigurationManager = class
  private
    FConnection: TSQLConnection;
    FCache: TDictionary<string, TConfigValue>;
    function BuildCacheKey(ATenantID, AUserID: Integer; const AKey: string): string;
  public
    constructor Create(AConnection: TSQLConnection);
    destructor Destroy; override;

    // Lecture de configuration
    function GetConfig(ATenantID: Integer; const AKey: string;
                      const ADefault: string = ''): string;
    function GetUserConfig(ATenantID, AUserID: Integer; const AKey: string;
                          const ADefault: string = ''): string;
    function GetGlobalConfig(const AKey: string;
                            const ADefault: string = ''): string;

    // Écriture de configuration
    procedure SetConfig(ATenantID: Integer; const AKey, AValue: string;
                       const AValueType: string = 'string');
    procedure SetUserConfig(ATenantID, AUserID: Integer; const AKey, AValue: string);
    procedure SetGlobalConfig(const AKey, AValue: string);

    // Gestion du cache
    procedure ClearCache;
    procedure InvalidateCache(ATenantID: Integer);

    // Configuration en masse
    function GetAllTenantConfig(ATenantID: Integer): TJSONObject;
    procedure ImportConfig(ATenantID: Integer; AConfig: TJSONObject);
  end;

implementation

uses
  TenantContext, jsonparser;

{ TConfigValue }

function TConfigValue.AsString: string;  
begin
  Result := FValue;
end;

function TConfigValue.AsInteger: Integer;  
begin
  Result := StrToIntDef(FValue, 0);
end;

function TConfigValue.AsBoolean: Boolean;  
begin
  Result := (LowerCase(FValue) = 'true') or (FValue = '1');
end;

function TConfigValue.AsJSON: TJSONObject;  
begin
  if FValueType = 'json' then
    Result := GetJSON(FValue) as TJSONObject
  else
    Result := nil;
end;

{ TConfigurationManager }

constructor TConfigurationManager.Create(AConnection: TSQLConnection);  
begin
  inherited Create;
  FConnection := AConnection;
  FCache := TDictionary<string, TConfigValue>.Create;
end;

destructor TConfigurationManager.Destroy;  
begin
  ClearCache;
  FCache.Free;
  inherited Destroy;
end;

function TConfigurationManager.BuildCacheKey(ATenantID, AUserID: Integer;
  const AKey: string): string;
begin
  if AUserID > 0 then
    Result := Format('user_%d_%d_%s', [ATenantID, AUserID, AKey])
  else if ATenantID > 0 then
    Result := Format('tenant_%d_%s', [ATenantID, AKey])
  else
    Result := Format('global_%s', [AKey]);
end;

function TConfigurationManager.GetConfig(ATenantID: Integer;
  const AKey, ADefault: string): string;
var
  Query: TSQLQuery;
  CacheKey: string;
  ConfigValue: TConfigValue;
begin
  Result := ADefault;
  CacheKey := BuildCacheKey(ATenantID, 0, AKey);

  // Vérifier le cache
  if FCache.TryGetValue(CacheKey, ConfigValue) then
  begin
    Result := ConfigValue.Value;
    Exit;
  end;

  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FConnection;
    Query.SQL.Text :=
      'SELECT Value, ValueType FROM TenantConfiguration ' +
      'WHERE TenantID = :tenantid AND ConfigKey = :key';
    Query.ParamByName('tenantid').AsInteger := ATenantID;
    Query.ParamByName('key').AsString := AKey;
    Query.Open;

    if not Query.EOF then
    begin
      Result := Query.FieldByName('Value').AsString;

      // Mettre en cache
      ConfigValue := TConfigValue.Create;
      ConfigValue.Key := AKey;
      ConfigValue.Value := Result;
      ConfigValue.ValueType := Query.FieldByName('ValueType').AsString;
      ConfigValue.Scope := csTenant;

      FCache.Add(CacheKey, ConfigValue);
    end;
  finally
    Query.Free;
  end;
end;

function TConfigurationManager.GetUserConfig(ATenantID, AUserID: Integer;
  const AKey, ADefault: string): string;
var
  Query: TSQLQuery;
  CacheKey: string;
  ConfigValue: TConfigValue;
begin
  Result := ADefault;
  CacheKey := BuildCacheKey(ATenantID, AUserID, AKey);

  if FCache.TryGetValue(CacheKey, ConfigValue) then
  begin
    Result := ConfigValue.Value;
    Exit;
  end;

  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FConnection;
    Query.SQL.Text :=
      'SELECT Value, ValueType FROM UserConfiguration ' +
      'WHERE TenantID = :tenantid AND UserID = :userid AND ConfigKey = :key';
    Query.ParamByName('tenantid').AsInteger := ATenantID;
    Query.ParamByName('userid').AsInteger := AUserID;
    Query.ParamByName('key').AsString := AKey;
    Query.Open;

    if not Query.EOF then
    begin
      Result := Query.FieldByName('Value').AsString;

      ConfigValue := TConfigValue.Create;
      ConfigValue.Key := AKey;
      ConfigValue.Value := Result;
      ConfigValue.ValueType := Query.FieldByName('ValueType').AsString;
      ConfigValue.Scope := csUser;

      FCache.Add(CacheKey, ConfigValue);
    end
    else
    begin
      // Fallback sur la configuration du tenant
      Result := GetConfig(ATenantID, AKey, ADefault);
    end;
  finally
    Query.Free;
  end;
end;

function TConfigurationManager.GetGlobalConfig(const AKey, ADefault: string): string;  
var
  Query: TSQLQuery;
  CacheKey: string;
  ConfigValue: TConfigValue;
begin
  Result := ADefault;
  CacheKey := BuildCacheKey(0, 0, AKey);

  if FCache.TryGetValue(CacheKey, ConfigValue) then
  begin
    Result := ConfigValue.Value;
    Exit;
  end;

  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FConnection;
    Query.SQL.Text :=
      'SELECT Value, ValueType FROM GlobalConfiguration WHERE ConfigKey = :key';
    Query.ParamByName('key').AsString := AKey;
    Query.Open;

    if not Query.EOF then
    begin
      Result := Query.FieldByName('Value').AsString;

      ConfigValue := TConfigValue.Create;
      ConfigValue.Key := AKey;
      ConfigValue.Value := Result;
      ConfigValue.ValueType := Query.FieldByName('ValueType').AsString;
      ConfigValue.Scope := csGlobal;

      FCache.Add(CacheKey, ConfigValue);
    end;
  finally
    Query.Free;
  end;
end;

procedure TConfigurationManager.SetConfig(ATenantID: Integer;
  const AKey, AValue, AValueType: string);
var
  Query: TSQLQuery;
  CacheKey: string;
begin
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FConnection;
    Query.SQL.Text :=
      'INSERT INTO TenantConfiguration (TenantID, ConfigKey, Value, ValueType, UpdatedAt) ' +
      'VALUES (:tenantid, :key, :value, :type, :now) ' +
      'ON DUPLICATE KEY UPDATE Value = :value, ValueType = :type, UpdatedAt = :now';
    Query.ParamByName('tenantid').AsInteger := ATenantID;
    Query.ParamByName('key').AsString := AKey;
    Query.ParamByName('value').AsString := AValue;
    Query.ParamByName('type').AsString := AValueType;
    Query.ParamByName('now').AsDateTime := Now;
    Query.ExecSQL;

    // Invalider le cache
    CacheKey := BuildCacheKey(ATenantID, 0, AKey);
    if FCache.ContainsKey(CacheKey) then
      FCache.Remove(CacheKey);
  finally
    Query.Free;
  end;
end;

procedure TConfigurationManager.SetUserConfig(ATenantID, AUserID: Integer;
  const AKey, AValue: string);
var
  Query: TSQLQuery;
  CacheKey: string;
begin
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FConnection;
    Query.SQL.Text :=
      'INSERT INTO UserConfiguration (TenantID, UserID, ConfigKey, Value, UpdatedAt) ' +
      'VALUES (:tenantid, :userid, :key, :value, :now) ' +
      'ON DUPLICATE KEY UPDATE Value = :value, UpdatedAt = :now';
    Query.ParamByName('tenantid').AsInteger := ATenantID;
    Query.ParamByName('userid').AsInteger := AUserID;
    Query.ParamByName('key').AsString := AKey;
    Query.ParamByName('value').AsString := AValue;
    Query.ParamByName('now').AsDateTime := Now;
    Query.ExecSQL;

    CacheKey := BuildCacheKey(ATenantID, AUserID, AKey);
    if FCache.ContainsKey(CacheKey) then
      FCache.Remove(CacheKey);
  finally
    Query.Free;
  end;
end;

procedure TConfigurationManager.SetGlobalConfig(const AKey, AValue: string);  
var
  Query: TSQLQuery;
  CacheKey: string;
begin
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FConnection;
    Query.SQL.Text :=
      'INSERT INTO GlobalConfiguration (ConfigKey, Value, UpdatedAt) ' +
      'VALUES (:key, :value, :now) ' +
      'ON DUPLICATE KEY UPDATE Value = :value, UpdatedAt = :now';
    Query.ParamByName('key').AsString := AKey;
    Query.ParamByName('value').AsString := AValue;
    Query.ParamByName('now').AsDateTime := Now;
    Query.ExecSQL;

    CacheKey := BuildCacheKey(0, 0, AKey);
    if FCache.ContainsKey(CacheKey) then
      FCache.Remove(CacheKey);
  finally
    Query.Free;
  end;
end;

procedure TConfigurationManager.ClearCache;  
var
  ConfigValue: TConfigValue;
begin
  for ConfigValue in FCache.Values do
    ConfigValue.Free;
  FCache.Clear;
end;

procedure TConfigurationManager.InvalidateCache(ATenantID: Integer);  
var
  Key: string;
  KeysToRemove: TStringList;
  ConfigValue: TConfigValue;
begin
  KeysToRemove := TStringList.Create;
  try
    for Key in FCache.Keys do
    begin
      if Pos(Format('tenant_%d_', [ATenantID]), Key) = 1 then
        KeysToRemove.Add(Key);
    end;

    for Key in KeysToRemove do
    begin
      if FCache.TryGetValue(Key, ConfigValue) then
      begin
        ConfigValue.Free;
        FCache.Remove(Key);
      end;
    end;
  finally
    KeysToRemove.Free;
  end;
end;

function TConfigurationManager.GetAllTenantConfig(ATenantID: Integer): TJSONObject;  
var
  Query: TSQLQuery;
begin
  Result := TJSONObject.Create;
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FConnection;
    Query.SQL.Text :=
      'SELECT ConfigKey, Value, ValueType FROM TenantConfiguration ' +
      'WHERE TenantID = :tenantid';
    Query.ParamByName('tenantid').AsInteger := ATenantID;
    Query.Open;

    while not Query.EOF do
    begin
      if Query.FieldByName('ValueType').AsString = 'json' then
        Result.Add(Query.FieldByName('ConfigKey').AsString,
                  GetJSON(Query.FieldByName('Value').AsString))
      else
        Result.Add(Query.FieldByName('ConfigKey').AsString,
                  Query.FieldByName('Value').AsString);
      Query.Next;
    end;
  finally
    Query.Free;
  end;
end;

procedure TConfigurationManager.ImportConfig(ATenantID: Integer;
  AConfig: TJSONObject);
var
  i: Integer;
  Key, Value: string;
begin
  for i := 0 to AConfig.Count - 1 do
  begin
    Key := AConfig.Names[i];

    if AConfig.Items[i] is TJSONObject then
      Value := TJSONObject(AConfig.Items[i]).AsJSON
    else
      Value := AConfig.Items[i].AsString;

    SetConfig(ATenantID, Key, Value);
  end;
end;

end.
```

## Exemple d'application complète

### Programme principal du serveur SaaS

```pascal
program SaaSPlatform;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils, Classes,
  fphttpapp, httpdefs, httproute,
  // Nos modules
  TenantAuthentication,
  TenantContext,
  TenantDataAccess,
  TenantPermissions,
  SubscriptionService,
  TenantFileStorage,
  TenantMetrics,
  TenantNotifications,
  TenantBackup,
  TenantWebhooks,
  TenantConfiguration;

type
  TSaaSApplication = class
  private
    FAuthService: TTenantAuthService;
    FDataService: TTenantDataService;
    FPermissionMgr: TPermissionManager;
    FSubscriptionMgr: TSubscriptionManager;
    FFileStorage: TFileStorageService;
    FMetrics: TMetricsCollector;
    FNotifications: TNotificationService;
    FBackup: TBackupService;
    FWebhooks: TWebhookManager;
    FConfig: TConfigurationManager;

    procedure InitializeServices;
    procedure RegisterRoutes;
    procedure ShutdownServices;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Run;
  end;

{ TSaaSApplication }

constructor TSaaSApplication.Create;  
begin
  inherited Create;
  InitializeServices;
  RegisterRoutes;
end;

destructor TSaaSApplication.Destroy;  
begin
  ShutdownServices;
  inherited Destroy;
end;

procedure TSaaSApplication.InitializeServices;  
var
  Connection: TSQLConnection;
begin
  // Initialiser la connexion à la base de données
  Connection := CreateDatabaseConnection;

  // Initialiser tous les services
  FAuthService := TTenantAuthService.Create(Connection);
  FDataService := TTenantDataService.Create(Connection);
  FPermissionMgr := TPermissionManager.Create(Connection);
  FSubscriptionMgr := TSubscriptionManager.Create(Connection);
  FFileStorage := TFileStorageService.Create(Connection, './data/files');
  FMetrics := TMetricsCollector.Create(Connection);
  FNotifications := TNotificationService.Create(Connection);
  FBackup := TBackupService.Create(Connection, './data/backups');
  FWebhooks := TWebhookManager.Create(Connection);
  FConfig := TConfigurationManager.Create(Connection);

  WriteLn('Tous les services ont été initialisés avec succès');
end;

procedure TSaaSApplication.RegisterRoutes;  
begin
  // Routes d'authentification
  HTTPRouter.RegisterRoute('/api/auth/login', rmPost, @HandleLogin);
  HTTPRouter.RegisterRoute('/api/auth/register', rmPost, @HandleRegister);
  HTTPRouter.RegisterRoute('/api/auth/logout', rmPost, @HandleLogout);

  // Routes de gestion des utilisateurs
  HTTPRouter.RegisterRoute('/api/users', rmGet, @HandleGetUsers);
  HTTPRouter.RegisterRoute('/api/users', rmPost, @HandleCreateUser);
  HTTPRouter.RegisterRoute('/api/users/:id', rmPut, @HandleUpdateUser);
  HTTPRouter.RegisterRoute('/api/users/:id', rmDelete, @HandleDeleteUser);

  // Routes de données métier
  HTTPRouter.RegisterRoute('/api/customers', rmAll, @HandleCustomers);
  HTTPRouter.RegisterRoute('/api/invoices', rmAll, @HandleInvoices);
  HTTPRouter.RegisterRoute('/api/products', rmAll, @HandleProducts);

  // Routes de gestion des fichiers
  HTTPRouter.RegisterRoute('/api/files', rmGet, @HandleGetFiles);
  HTTPRouter.RegisterRoute('/api/files/upload', rmPost, @HandleUploadFile);
  HTTPRouter.RegisterRoute('/api/files/:id', rmGet, @HandleDownloadFile);
  HTTPRouter.RegisterRoute('/api/files/:id', rmDelete, @HandleDeleteFile);

  // Routes d'administration
  HTTPRouter.RegisterRoute('/api/admin/tenants', rmGet, @HandleGetTenants);
  HTTPRouter.RegisterRoute('/api/admin/metrics', rmGet, @HandleGetMetrics);
  HTTPRouter.RegisterRoute('/api/admin/backups', rmPost, @HandleCreateBackup);

  // Routes de configuration
  HTTPRouter.RegisterRoute('/api/config', rmGet, @HandleGetConfig);
  HTTPRouter.RegisterRoute('/api/config', rmPut, @HandleSetConfig);

  // Routes de notifications
  HTTPRouter.RegisterRoute('/api/notifications', rmGet, @HandleGetNotifications);
  HTTPRouter.RegisterRoute('/api/notifications/:id/read', rmPost, @HandleMarkNotificationRead);

  // Routes de webhooks
  HTTPRouter.RegisterRoute('/api/webhooks', rmGet, @HandleGetWebhooks);
  HTTPRouter.RegisterRoute('/api/webhooks', rmPost, @HandleCreateWebhook);
  HTTPRouter.RegisterRoute('/api/webhooks/:id', rmDelete, @HandleDeleteWebhook);

  WriteLn('Routes enregistrées avec succès');
end;

procedure TSaaSApplication.ShutdownServices;  
begin
  FConfig.Free;
  FWebhooks.Free;
  FBackup.Free;
  FNotifications.Free;
  FMetrics.Free;
  FFileStorage.Free;
  FSubscriptionMgr.Free;
  FPermissionMgr.Free;
  FDataService.Free;
  FAuthService.Free;

  WriteLn('Tous les services ont été arrêtés');
end;

procedure TSaaSApplication.Run;  
begin
  Application.Title := 'Plateforme SaaS Multi-tenant';
  Application.Port := 8080;
  Application.Threaded := True;

  WriteLn('==============================================');
  WriteLn('Plateforme SaaS Multi-tenant');
  WriteLn('==============================================');
  WriteLn('Serveur démarré sur le port 8080');
  WriteLn('Appuyez sur Ctrl+C pour arrêter');
  WriteLn('==============================================');

  Application.Initialize;
  Application.Run;
end;

var
  SaaSApp: TSaaSApplication;

begin
  try
    SaaSApp := TSaaSApplication.Create;
    try
      SaaSApp.Run;
    finally
      SaaSApp.Free;
    end;
  except
    on E: Exception do
      WriteLn('Erreur fatale: ', E.Message);
  end;
end.
```

## Bonnes pratiques et considérations importantes

### 1. Sécurité

**Isolation des données**
- Toujours filtrer par `TenantID` dans toutes les requêtes
- Utiliser des requêtes préparées pour éviter les injections SQL
- Valider systématiquement le contexte du tenant avant toute opération

**Authentification et autorisation**
- Utiliser JWT avec rotation des secrets
- Implémenter un système de permissions granulaires
- Logger tous les accès et modifications sensibles

**Protection contre les attaques**
- Rate limiting par tenant et par utilisateur
- Protection CSRF pour les formulaires
- Validation stricte des entrées utilisateur
- Chiffrement des données sensibles au repos

### 2. Performance

**Optimisation des requêtes**
- Indexer systématiquement la colonne `TenantID`
- Utiliser des index composites : `(TenantID, autres_colonnes)`
- Mettre en cache les configurations et permissions

**Gestion de la charge**
- Implémenter un système de queue pour les tâches lourdes
- Utiliser la pagination pour toutes les listes
- Limiter les quotas selon le plan de souscription

### 3. Scalabilité

**Architecture**
- Séparer les services stateless (API) et stateful (base de données)
- Utiliser un load balancer pour distribuer la charge
- Prévoir la possibilité de sharding par tenant

**Monitoring**
- Surveiller les métriques par tenant
- Alerter en cas de dépassement de quotas
- Analyser les performances par tenant

### 4. Maintenance

**Sauvegardes**
- Sauvegardes automatiques quotidiennes
- Tests réguliers de restauration
- Conservation des sauvegardes selon le plan

**Mises à jour**
- Déploiement progressif (rolling updates)
- Tests en environnement de staging
- Plan de rollback en cas de problème

### 5. Conformité

**RGPD et protection des données**
- Droit à l'oubli : possibilité de supprimer toutes les données d'un tenant
- Export des données au format standard
- Consentement explicite pour le traitement des données
- Journal d'audit des accès aux données personnelles

**Disponibilité**
- SLA (Service Level Agreement) par plan
- Plan de reprise d'activité (disaster recovery)
- Redondance des données critiques

## Conclusion

Une plateforme SaaS multi-tenant bien conçue avec FreePascal/Lazarus nécessite :

1. **Une architecture solide** avec isolation stricte des données
2. **Des services modulaires** pour faciliter la maintenance
3. **Une sécurité renforcée** à tous les niveaux
4. **Une surveillance active** des métriques et performances
5. **Une scalabilité préparée** dès la conception

Le code présenté dans ce tutoriel constitue une base solide pour développer votre propre plateforme SaaS. Adaptez-le selon vos besoins spécifiques et n'oubliez pas de tester exhaustivement chaque composant.

### Points clés à retenir

- **Multi-tenant = isolation des données** : Chaque requête doit filtrer par TenantID
- **Sécurité first** : Authentification, autorisation, validation, chiffrement
- **Scalabilité** : Penser dès le début à la croissance
- **Monitoring** : Surveiller les métriques pour détecter les problèmes
- **Flexibilité** : Architecture modulaire pour évoluer facilement

### Ressources complémentaires

Pour aller plus loin :
- Documentation FreePascal : https://www.freepascal.org/docs.html
- Forum Lazarus : https://forum.lazarus.freepascal.org/
- Exemples de code : https://wiki.freepascal.org/
- Patterns SaaS : Martin Fowler's architectural patterns

Bonne chance dans le développement de votre plateforme SaaS multi-tenant avec FreePascal/Lazarus !

⏭️ [Système de trading haute fréquence](/25-projets-complexes-etudes-cas/03-systeme-trading-haute-frequence.md)
