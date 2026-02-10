🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 21.2 Microservices et architectures distribuées

## Introduction aux microservices

### Qu'est-ce qu'une architecture microservices ?

Une **architecture microservices** décompose une application en petits services indépendants, chacun responsable d'une fonctionnalité métier spécifique. Contrairement à une application monolithique où tout le code est dans un seul programme, les microservices sont des applications autonomes qui communiquent entre elles.

**Comparaison visuelle :**

```
MONOLITHE                          MICROSERVICES
┌─────────────────────┐           ┌──────────┐  ┌──────────┐
│                     │           │ Service  │  │ Service  │
│   Toute l'app       │           │ Clients  │  │ Commandes│
│   dans un seul      │    VS     └────┬─────┘  └────┬─────┘
│   exécutable        │                │             │
│                     │           ┌────┴───────┐  ┌──┴───────┐
└─────────────────────┘           │ Service    │  │ Service  │
                                  │ Facturation│  │ Produits │
                                  └────────────┘  └──────────┘
```

### Pourquoi utiliser des microservices ?

**Avantages :**
- **Déploiement indépendant** : modifier un service sans toucher aux autres
- **Technologie libre** : chaque service peut utiliser sa propre base de données
- **Scalabilité ciblée** : augmenter uniquement les services sous charge
- **Résilience** : une panne n'affecte pas tout le système
- **Équipes autonomes** : chaque équipe gère son service

**Inconvénients :**
- **Complexité** : plus de services = plus de coordination
- **Réseau** : latence et gestion des erreurs de communication
- **Données distribuées** : pas de transactions simples
- **Déploiement** : nécessite de l'outillage (Docker, Kubernetes)

### FreePascal et les microservices

FreePascal est excellent pour les microservices grâce à :
- **Compilation native** : services légers et rapides
- **Faible consommation mémoire** : idéal pour multiplier les instances
- **Démarrage instantané** : pas de machine virtuelle à initialiser
- **Portabilité** : même code sur Windows et Linux
- **Stabilité** : pas de garbage collection imprévisible

## Concepts fondamentaux

### 1. Service indépendant

Chaque microservice est une application complète avec :
- Son propre processus
- Sa propre base de données (optionnel mais recommandé)
- Son API pour communiquer
- Son cycle de déploiement

```pascal
// Service Catalogue de Produits
program ServiceCatalogue;

{$mode objfpc}{$H+}

uses
  fphttpapp, httpdefs, httproute, fpjson, sqldb;

type
  TServiceCatalogue = class
  private
    FConnexionDB: TSQLConnection;
  public
    constructor Create;
    destructor Destroy; override;

    procedure ObtenirProduit(ARequest: TRequest; AResponse: TResponse);
    procedure ListerProduits(ARequest: TRequest; AResponse: TResponse);
  end;

constructor TServiceCatalogue.Create;
begin
  // Initialisation connexion base de données
  FConnexionDB := TSQLConnection.Create(nil);
  FConnexionDB.DatabaseName := 'catalogue_db';
  // Configuration spécifique au service
end;

procedure TServiceCatalogue.ObtenirProduit(ARequest: TRequest; AResponse: TResponse);
var
  ProduitId: string;
  JSON: TJSONObject;
begin
  ProduitId := ARequest.RouteParams['id'];

  // Logique métier du service
  JSON := TJSONObject.Create;
  try
    JSON.Add('id', ProduitId);
    JSON.Add('nom', 'Produit exemple');
    JSON.Add('prix', 29.99);

    AResponse.ContentType := 'application/json';
    AResponse.Content := JSON.AsJSON;
  finally
    JSON.Free;
  end;
end;

// Point d'entrée du service
var
  Service: TServiceCatalogue;
begin
  Service := TServiceCatalogue.Create;
  try
    HTTPRouter.RegisterRoute('/produits/:id', rmGet, @Service.ObtenirProduit);
    HTTPRouter.RegisterRoute('/produits', rmGet, @Service.ListerProduits);

    Application.Port := 8001;  // Port spécifique au service
    Application.Initialize;
    WriteLn('Service Catalogue démarré sur le port 8001');
    Application.Run;
  finally
    Service.Free;
  end;
end.
```

### 2. Communication entre services

Les microservices communiquent principalement via :

#### A. API REST (HTTP/JSON)

Le mode le plus courant et simple.

```pascal
unit ServiceClient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient, fpjson, jsonparser;

type
  TClientServiceCatalogue = class
  private
    FBaseURL: string;
    FHTTPClient: TFPHTTPClient;
  public
    constructor Create(const ABaseURL: string);
    destructor Destroy; override;

    function ObtenirProduit(const AProduitId: string): TJSONObject;
    function ListerProduits: TJSONArray;
  end;

implementation

constructor TClientServiceCatalogue.Create(const ABaseURL: string);
begin
  inherited Create;
  FBaseURL := ABaseURL;
  FHTTPClient := TFPHTTPClient.Create(nil);
end;

destructor TClientServiceCatalogue.Destroy;
begin
  FHTTPClient.Free;
  inherited;
end;

function TClientServiceCatalogue.ObtenirProduit(const AProduitId: string): TJSONObject;
var
  URL: string;
  Reponse: string;
  Parser: TJSONParser;
begin
  URL := FBaseURL + '/produits/' + AProduitId;

  try
    Reponse := FHTTPClient.Get(URL);
    Parser := TJSONParser.Create(Reponse, [joUTF8]);
    try
      Result := Parser.Parse as TJSONObject;
    finally
      Parser.Free;
    end;
  except
    on E: Exception do
    begin
      WriteLn('Erreur lors de l''appel au service catalogue: ', E.Message);
      raise;
    end;
  end;
end;

function TClientServiceCatalogue.ListerProduits: TJSONArray;
var
  URL: string;
  Reponse: string;
  Parser: TJSONParser;
begin
  URL := FBaseURL + '/produits';
  Reponse := FHTTPClient.Get(URL);

  Parser := TJSONParser.Create(Reponse, [joUTF8]);
  try
    Result := Parser.Parse as TJSONArray;
  finally
    Parser.Free;
  end;
end;

end.
```

#### B. Messages asynchrones (Message Queue)

Pour la communication asynchrone et la résilience.

```pascal
unit MessageQueue;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson;

type
  // Message générique
  TMessage = class
  private
    FType: string;
    FPayload: TJSONObject;
    FTimestamp: TDateTime;
    FId: string;
  public
    constructor Create(const AType: string; APayload: TJSONObject);
    destructor Destroy; override;

    property Id: string read FId;
    property MessageType: string read FType;
    property Payload: TJSONObject read FPayload;
    property Timestamp: TDateTime read FTimestamp;
  end;

  // Interface de publication
  IPublisherMessage = interface
    ['{A1B2C3D4-E5F6-7890-ABCD-EF1234567890}']
    procedure Publier(const AMessage: TMessage);
  end;

  // Interface de consommation
  IConsommateurMessage = interface
    ['{B2C3D4E5-F678-90AB-CDEF-123456789ABC}']
    procedure Consommer(const AMessage: TMessage);
  end;

  // Implémentation simple en mémoire (pour test)
  TMessageQueueMemoire = class(TInterfacedObject, IPublisherMessage)
  private
    FMessages: TThreadList;
    FConsommateurs: TList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Publier(const AMessage: TMessage);
    procedure AbonnerConsommateur(const AConsommateur: IConsommateurMessage);
    procedure Traiter;
  end;

implementation

uses
  Generics.Collections;

constructor TMessage.Create(const AType: string; APayload: TJSONObject);
begin
  inherited Create;
  FId := TGuid.NewGuid.ToString;
  FType := AType;
  FPayload := APayload;
  FTimestamp := Now;
end;

destructor TMessage.Destroy;
begin
  FPayload.Free;
  inherited;
end;

constructor TMessageQueueMemoire.Create;
begin
  inherited Create;
  FMessages := TThreadList.Create;
  FConsommateurs := TList.Create;
end;

destructor TMessageQueueMemoire.Destroy;
begin
  FMessages.Free;
  FConsommateurs.Free;
  inherited;
end;

procedure TMessageQueueMemoire.Publier(const AMessage: TMessage);
var
  Liste: TList;
begin
  Liste := FMessages.LockList;
  try
    Liste.Add(AMessage);
    WriteLn(Format('Message publié: %s (Type: %s)',
      [AMessage.Id, AMessage.MessageType]));
  finally
    FMessages.UnlockList;
  end;
end;

procedure TMessageQueueMemoire.AbonnerConsommateur(
  const AConsommateur: IConsommateurMessage);
begin
  FConsommateurs.Add(Pointer(AConsommateur));
end;

procedure TMessageQueueMemoire.Traiter;
var
  Liste: TList;
  Message: TMessage;
  i: Integer;
  Consommateur: IConsommateurMessage;
begin
  Liste := FMessages.LockList;
  try
    if Liste.Count = 0 then Exit;

    // Traiter le premier message
    Message := TMessage(Liste.First);
    Liste.Delete(0);
  finally
    FMessages.UnlockList;
  end;

  // Distribuer aux consommateurs
  for i := 0 to FConsommateurs.Count - 1 do
  begin
    Consommateur := IConsommateurMessage(FConsommateurs[i]);
    try
      Consommateur.Consommer(Message);
    except
      on E: Exception do
        WriteLn('Erreur consommateur: ', E.Message);
    end;
  end;

  Message.Free;
end;

end.
```

**Utilisation dans un service :**

```pascal
type
  TServiceCommande = class(TInterfacedObject, IConsommateurMessage)
  private
    FPublisher: IPublisherMessage;
  public
    constructor Create(const APublisher: IPublisherMessage);

    procedure CreerCommande(const AClientId, AProduitId: string; AQuantite: Integer);
    procedure Consommer(const AMessage: TMessage);
  end;

procedure TServiceCommande.CreerCommande(
  const AClientId, AProduitId: string; AQuantite: Integer);
var
  Payload: TJSONObject;
  Message: TMessage;
begin
  // Créer la commande localement
  // ...

  // Publier un événement pour notifier les autres services
  Payload := TJSONObject.Create;
  Payload.Add('commandeId', TGuid.NewGuid.ToString);
  Payload.Add('clientId', AClientId);
  Payload.Add('produitId', AProduitId);
  Payload.Add('quantite', AQuantite);

  Message := TMessage.Create('CommandeCreee', Payload);
  FPublisher.Publier(Message);
end;

procedure TServiceCommande.Consommer(const AMessage: TMessage);
begin
  if AMessage.MessageType = 'StockMisAJour' then
  begin
    // Réagir à la mise à jour de stock
    WriteLn('Stock mis à jour reçu');
  end;
end;
```

### 3. Service Discovery (Découverte de services)

Dans un environnement avec plusieurs instances de services, il faut un mécanisme pour les trouver.

```pascal
unit ServiceRegistry;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections;

type
  TServiceInfo = record
    Nom: string;
    Hote: string;
    Port: Integer;
    Sante: string;  // 'healthy', 'unhealthy'
    DerniereVerification: TDateTime;
  end;

  TRegistreServices = class
  private
    FServices: TDictionary<string, TList<TServiceInfo>>;
    FLock: TRTLCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Enregistrer(const AInfo: TServiceInfo);
    procedure Desenregistrer(const ANom, AHote: string; APort: Integer);
    function Decouvrir(const ANom: string): TServiceInfo;
    function DecouvrirTous(const ANom: string): TArray<TServiceInfo>;
    procedure VerifierSante;
  end;

implementation

uses
  fphttpclient;

constructor TRegistreServices.Create;
begin
  inherited Create;
  FServices := TDictionary<string, TList<TServiceInfo>>.Create;
  InitCriticalSection(FLock);
end;

destructor TRegistreServices.Destroy;
var
  Liste: TList<TServiceInfo>;
begin
  EnterCriticalSection(FLock);
  try
    for Liste in FServices.Values do
      Liste.Free;
    FServices.Free;
  finally
    LeaveCriticalSection(FLock);
  end;
  DoneCriticalSection(FLock);
  inherited;
end;

procedure TRegistreServices.Enregistrer(const AInfo: TServiceInfo);
var
  Liste: TList<TServiceInfo>;
  Info: TServiceInfo;
begin
  EnterCriticalSection(FLock);
  try
    if not FServices.TryGetValue(AInfo.Nom, Liste) then
    begin
      Liste := TList<TServiceInfo>.Create;
      FServices.Add(AInfo.Nom, Liste);
    end;

    Info := AInfo;
    Info.DerniereVerification := Now;
    Liste.Add(Info);

    WriteLn(Format('Service enregistré: %s (%s:%d)',
      [AInfo.Nom, AInfo.Hote, AInfo.Port]));
  finally
    LeaveCriticalSection(FLock);
  end;
end;

function TRegistreServices.Decouvrir(const ANom: string): TServiceInfo;
var
  Services: TArray<TServiceInfo>;
  Service: TServiceInfo;
  MeilleurService: TServiceInfo;
  Trouve: Boolean;
begin
  Services := DecouvrirTous(ANom);

  if Length(Services) = 0 then
    raise Exception.CreateFmt('Aucun service "%s" disponible', [ANom]);

  // Sélectionner un service sain (round-robin simple ici)
  Trouve := False;
  for Service in Services do
  begin
    if Service.Sante = 'healthy' then
    begin
      MeilleurService := Service;
      Trouve := True;
      Break;
    end;
  end;

  if not Trouve then
    raise Exception.CreateFmt('Aucun service "%s" en bonne santé', [ANom]);

  Result := MeilleurService;
end;

function TRegistreServices.DecouvrirTous(const ANom: string): TArray<TServiceInfo>;
var
  Liste: TList<TServiceInfo>;
begin
  EnterCriticalSection(FLock);
  try
    if FServices.TryGetValue(ANom, Liste) then
      Result := Liste.ToArray
    else
      SetLength(Result, 0);
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TRegistreServices.VerifierSante;
var
  Paire: TPair<string, TList<TServiceInfo>>;
  Liste: TList<TServiceInfo>;
  i: Integer;
  Service: TServiceInfo;
  Client: TFPHTTPClient;
  URL: string;
begin
  Client := TFPHTTPClient.Create(nil);
  try
    EnterCriticalSection(FLock);
    try
      for Paire in FServices do
      begin
        Liste := Paire.Value;
        for i := 0 to Liste.Count - 1 do
        begin
          Service := Liste[i];
          URL := Format('http://%s:%d/health', [Service.Hote, Service.Port]);

          try
            Client.Get(URL);
            Service.Sante := 'healthy';
          except
            Service.Sante := 'unhealthy';
          end;

          Service.DerniereVerification := Now;
          Liste[i] := Service;
        end;
      end;
    finally
      LeaveCriticalSection(FLock);
    end;
  finally
    Client.Free;
  end;
end;

end.
```

### 4. API Gateway (Passerelle API)

L'API Gateway est le point d'entrée unique pour tous les clients.

```pascal
program APIGateway;

{$mode objfpc}{$H+}

uses
  fphttpapp, httpdefs, httproute, fphttpclient, fpjson, jsonparser,
  ServiceRegistry;

type
  TAPIGateway = class
  private
    FRegistre: TRegistreServices;
    FHTTPClient: TFPHTTPClient;

    function AppelerService(const ANomService, APath: string): string;
  public
    constructor Create(const ARegistre: TRegistreServices);
    destructor Destroy; override;

    procedure RouteVersProduits(ARequest: TRequest; AResponse: TResponse);
    procedure RouteVersCommandes(ARequest: TRequest; AResponse: TResponse);
    procedure RouteVersClients(ARequest: TRequest; AResponse: TResponse);
  end;

constructor TAPIGateway.Create(const ARegistre: TRegistreServices);
begin
  inherited Create;
  FRegistre := ARegistre;
  FHTTPClient := TFPHTTPClient.Create(nil);
end;

destructor TAPIGateway.Destroy;
begin
  FHTTPClient.Free;
  inherited;
end;

function TAPIGateway.AppelerService(const ANomService, APath: string): string;
var
  ServiceInfo: TServiceInfo;
  URL: string;
begin
  // Découvrir le service
  ServiceInfo := FRegistre.Decouvrir(ANomService);

  // Construire l'URL
  URL := Format('http://%s:%d%s', [ServiceInfo.Hote, ServiceInfo.Port, APath]);

  // Appeler le service
  try
    Result := FHTTPClient.Get(URL);
  except
    on E: Exception do
    begin
      WriteLn('Erreur appel service: ', E.Message);
      raise;
    end;
  end;
end;

procedure TAPIGateway.RouteVersProduits(ARequest: TRequest; AResponse: TResponse);
var
  Reponse: string;
begin
  try
    Reponse := AppelerService('service-produits', ARequest.PathInfo);
    AResponse.ContentType := 'application/json';
    AResponse.Content := Reponse;
  except
    on E: Exception do
    begin
      AResponse.Code := 503; // Service Unavailable
      AResponse.Content := Format('{"error": "%s"}', [E.Message]);
    end;
  end;
end;

procedure TAPIGateway.RouteVersCommandes(ARequest: TRequest; AResponse: TResponse);
var
  Reponse: string;
begin
  try
    Reponse := AppelerService('service-commandes', ARequest.PathInfo);
    AResponse.ContentType := 'application/json';
    AResponse.Content := Reponse;
  except
    on E: Exception do
    begin
      AResponse.Code := 503;
      AResponse.Content := Format('{"error": "%s"}', [E.Message]);
    end;
  end;
end;

procedure TAPIGateway.RouteVersClients(ARequest: TRequest; AResponse: TResponse);
var
  Reponse: string;
begin
  try
    Reponse := AppelerService('service-clients', ARequest.PathInfo);
    AResponse.ContentType := 'application/json';
    AResponse.Content := Reponse;
  except
    on E: Exception do
    begin
      AResponse.Code := 503;
      AResponse.Content := Format('{"error": "%s"}', [E.Message]);
    end;
  end;
end;

var
  Registre: TRegistreServices;
  Gateway: TAPIGateway;
begin
  Registre := TRegistreServices.Create;
  Gateway := TAPIGateway.Create(Registre);
  try
    // Enregistrer les routes
    HTTPRouter.RegisterRoute('/api/produits*', rmAll, @Gateway.RouteVersProduits);
    HTTPRouter.RegisterRoute('/api/commandes*', rmAll, @Gateway.RouteVersCommandes);
    HTTPRouter.RegisterRoute('/api/clients*', rmAll, @Gateway.RouteVersClients);

    Application.Port := 8080;
    Application.Initialize;
    WriteLn('API Gateway démarré sur le port 8080');
    Application.Run;
  finally
    Gateway.Free;
    Registre.Free;
  end;
end.
```

## Patterns essentiels des microservices

### 1. Circuit Breaker (Disjoncteur)

Protège contre les cascades de pannes en coupant temporairement les appels à un service défaillant.

```pascal
unit CircuitBreaker;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils;

type
  TEtatCircuit = (ecFerme, ecOuvert, ecSemiOuvert);

  TCircuitBreaker = class
  private
    FEtat: TEtatCircuit;
    FNombreEchecs: Integer;
    FSeuilEchecs: Integer;
    FDelaiReouverture: Integer; // en secondes
    FDerniereOuverture: TDateTime;
    FLock: TRTLCriticalSection;
  public
    constructor Create(ASeuilEchecs: Integer = 5; ADelaiReouverture: Integer = 60);
    destructor Destroy; override;

    function Executer(AOperation: TFunc<string>): string;
    procedure Reinitialiser;

    property Etat: TEtatCircuit read FEtat;
  end;

  ECircuitOuvertException = class(Exception);

implementation

constructor TCircuitBreaker.Create(ASeuilEchecs: Integer; ADelaiReouverture: Integer);
begin
  inherited Create;
  FEtat := ecFerme;
  FNombreEchecs := 0;
  FSeuilEchecs := ASeuilEchecs;
  FDelaiReouverture := ADelaiReouverture;
  InitCriticalSection(FLock);
end;

destructor TCircuitBreaker.Destroy;
begin
  DoneCriticalSection(FLock);
  inherited;
end;

function TCircuitBreaker.Executer(AOperation: TFunc<string>): string;
begin
  EnterCriticalSection(FLock);
  try
    // Circuit ouvert : refuser l'appel
    if FEtat = ecOuvert then
    begin
      // Vérifier si le délai de réouverture est passé
      if SecondsBetween(Now, FDerniereOuverture) >= FDelaiReouverture then
      begin
        WriteLn('Circuit passe en semi-ouvert (test)');
        FEtat := ecSemiOuvert;
      end
      else
        raise ECircuitOuvertException.Create('Circuit ouvert - service indisponible');
    end;
  finally
    LeaveCriticalSection(FLock);
  end;

  // Tenter l'opération
  try
    Result := AOperation();

    // Succès : réinitialiser ou fermer le circuit
    EnterCriticalSection(FLock);
    try
      if FEtat = ecSemiOuvert then
      begin
        WriteLn('Circuit fermé - service récupéré');
        FEtat := ecFerme;
      end;
      FNombreEchecs := 0;
    finally
      LeaveCriticalSection(FLock);
    end;
  except
    on E: Exception do
    begin
      // Échec : incrémenter le compteur
      EnterCriticalSection(FLock);
      try
        Inc(FNombreEchecs);
        WriteLn(Format('Échec %d/%d', [FNombreEchecs, FSeuilEchecs]));

        if (FNombreEchecs >= FSeuilEchecs) or (FEtat = ecSemiOuvert) then
        begin
          FEtat := ecOuvert;
          FDerniereOuverture := Now;
          WriteLn('Circuit ouvert - trop d''échecs');
        end;
      finally
        LeaveCriticalSection(FLock);
      end;
      raise;
    end;
  end;
end;

procedure TCircuitBreaker.Reinitialiser;
begin
  EnterCriticalSection(FLock);
  try
    FEtat := ecFerme;
    FNombreEchecs := 0;
    WriteLn('Circuit réinitialisé manuellement');
  finally
    LeaveCriticalSection(FLock);
  end;
end;

end.
```

**Utilisation :**

```pascal
var
  CircuitBreaker: TCircuitBreaker;
  Client: TFPHTTPClient;

function AppelerServiceProtege: string;
begin
  Result := Client.Get('http://service-externe/api/data');
end;

begin
  CircuitBreaker := TCircuitBreaker.Create(5, 60);
  Client := TFPHTTPClient.Create(nil);
  try
    try
      Result := CircuitBreaker.Executer(@AppelerServiceProtege);
      WriteLn('Succès: ', Result);
    except
      on E: ECircuitOuvertException do
        WriteLn('Service temporairement indisponible');
      on E: Exception do
        WriteLn('Erreur: ', E.Message);
    end;
  finally
    Client.Free;
    CircuitBreaker.Free;
  end;
end;
```

### 2. Retry Pattern (Réessai avec backoff)

Réessayer automatiquement les appels échoués avec un délai croissant.

```pascal
unit RetryPolicy;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TRetryPolicy = class
  private
    FNombreMaxTentatives: Integer;
    FDelaiInitial: Integer; // millisecondes
    FFacteurMultiplication: Double;
  public
    constructor Create(ANombreTentatives: Integer = 3;
                      ADelaiInitial: Integer = 100;
                      AFacteur: Double = 2.0);

    function ExecuterAvecRetry<T>(AOperation: TFunc<T>): T;
  end;

implementation

uses
  DateUtils;

constructor TRetryPolicy.Create(ANombreTentatives: Integer;
  ADelaiInitial: Integer; AFacteur: Double);
begin
  inherited Create;
  FNombreMaxTentatives := ANombreTentatives;
  FDelaiInitial := ADelaiInitial;
  FFacteurMultiplication := AFacteur;
end;

function TRetryPolicy.ExecuterAvecRetry<T>(AOperation: TFunc<T>): T;
var
  Tentative: Integer;
  Delai: Integer;
  DerniereException: Exception;
begin
  DerniereException := nil;
  Delai := FDelaiInitial;

  for Tentative := 1 to FNombreMaxTentatives do
  begin
    try
      Result := AOperation();
      Exit; // Succès
    except
      on E: Exception do
      begin
        DerniereException := E;
        WriteLn(Format('Tentative %d/%d échouée: %s',
          [Tentative, FNombreMaxTentatives, E.Message]));

        if Tentative < FNombreMaxTentatives then
        begin
          WriteLn(Format('Nouvelle tentative dans %d ms', [Delai]));
          Sleep(Delai);
          Delai := Round(Delai * FFacteurMultiplication);
        end;
      end;
    end;
  end;

  // Toutes les tentatives ont échoué
  if Assigned(DerniereException) then
    raise Exception.CreateFmt('Échec après %d tentatives: %s',
      [FNombreMaxTentatives, DerniereException.Message]);
end;

end.
```

### 3. Saga Pattern (Transactions distribuées)

Coordonner des transactions à travers plusieurs services avec compensation en cas d'échec.

```pascal
unit SagaPattern;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections;

type
  // Étape d'une saga
  TSagaEtape = class
  private
    FNom: string;
    FActionPrincipale: TProc;
    FActionCompensation: TProc;
    FExecutee: Boolean;
  public
    constructor Create(const ANom: string;
                      AAction: TProc;
                      ACompensation: TProc);

    procedure Executer;
    procedure Compenser;

    property Nom: string read FNom;
    property Executee: Boolean read FExecutee;
  end;

  // Orchestrateur de saga
  TSagaOrchestrator = class
  private
    FEtapes: TObjectList<TSagaEtape>;
    FEtapeExecutees: TList<TSagaEtape>;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AjouterEtape(AEtape: TSagaEtape);
    function Executer: Boolean;
    procedure Compenser;
  end;

implementation

constructor TSagaEtape.Create(const ANom: string;
  AAction: TProc; ACompensation: TProc);
begin
  inherited Create;
  FNom := ANom;
  FActionPrincipale := AAction;
  FActionCompensation := ACompensation;
  FExecutee := False;
end;

procedure TSagaEtape.Executer;
begin
  WriteLn('Exécution étape: ', FNom);
  FActionPrincipale();
  FExecutee := True;
end;

procedure TSagaEtape.Compenser;
begin
  if FExecutee then
  begin
    WriteLn('Compensation étape: ', FNom);
    FActionCompensation();
  end;
end;

constructor TSagaOrchestrator.Create;
begin
  inherited Create;
  FEtapes := TObjectList<TSagaEtape>.Create(True);
  FEtapeExecutees := TList<TSagaEtape>.Create;
end;

destructor TSagaOrchestrator.Destroy;
begin
  FEtapeExecutees.Free;
  FEtapes.Free;
  inherited;
end;

procedure TSagaOrchestrator.AjouterEtape(AEtape: TSagaEtape);
begin
  FEtapes.Add(AEtape);
end;

function TSagaOrchestrator.Executer: Boolean;
var
  Etape: TSagaEtape;
begin
  Result := True;
  FEtapeExecutees.Clear;

  for Etape in FEtapes do
  begin
    try
      Etape.Executer;
      FEtapeExecutees.Add(Etape);
    except
      on E: Exception do
      begin
        WriteLn('Erreur dans étape ', Etape.Nom, ': ', E.Message);
        WriteLn('Démarrage compensation...');
        Compenser;
        Result := False;
        Exit;
      end;
    end;
  end;

  WriteLn('Saga complétée avec succès');
end;

procedure TSagaOrchestrator.Compenser;
var
  i: Integer;
begin
  // Compenser dans l'ordre inverse
  for i := FEtapeExecutees.Count - 1 downto 0 do
  begin
    try
      TSagaEtape(FEtapeExecutees[i]).Compenser;
    except
      on E: Exception do
        WriteLn('Erreur compensation: ', E.Message);
    end;
  end;
  WriteLn('Compensation terminée');
end;

end.
```

**Exemple d'utilisation - Transaction de commande distribuée :**

```pascal
procedure ExempleCommandeSaga;
var
  Saga: TSagaOrchestrator;
  CommandeId: string;
  ReservationId: string;
  PaiementId: string;

  procedure ReserverStock;
  begin
    WriteLn('Réservation du stock...');
    // Appel au service de stock
    ReservationId := AppelerServiceStock('reserver', CommandeId);
    if ReservationId = '' then
      raise Exception.Create('Impossible de réserver le stock');
  end;

  procedure AnnulerReservationStock;
  begin
    WriteLn('Annulation de la réservation...');
    AppelerServiceStock('annuler', ReservationId);
  end;

  procedure TraiterPaiement;
  begin
    WriteLn('Traitement du paiement...');
    // Appel au service de paiement
    PaiementId := AppelerServicePaiement('payer', CommandeId);
    if PaiementId = '' then
      raise Exception.Create('Paiement refusé');
  end;

  procedure RemboursePaiement;
  begin
    WriteLn('Remboursement...');
    AppelerServicePaiement('rembourser', PaiementId);
  end;

  procedure CreerCommande;
  begin
    WriteLn('Création de la commande...');
    // Appel au service de commande
    CommandeId := AppelerServiceCommande('creer');
  end;

  procedure SupprimerCommande;
  begin
    WriteLn('Suppression de la commande...');
    AppelerServiceCommande('supprimer', CommandeId);
  end;

begin
  Saga := TSagaOrchestrator.Create;
  try
    // Définir les étapes de la saga
    Saga.AjouterEtape(TSagaEtape.Create(
      'Créer commande',
      @CreerCommande,
      @SupprimerCommande
    ));

    Saga.AjouterEtape(TSagaEtape.Create(
      'Réserver stock',
      @ReserverStock,
      @AnnulerReservationStock
    ));

    Saga.AjouterEtape(TSagaEtape.Create(
      'Traiter paiement',
      @TraiterPaiement,
      @RemboursePaiement
    ));

    // Exécuter la saga
    if Saga.Executer then
      WriteLn('Commande créée avec succès: ', CommandeId)
    else
      WriteLn('Échec de la création de commande');
  finally
    Saga.Free;
  end;
end;
```

## Gestion des données distribuées

### 1. Database per Service (Base par service)

Chaque service possède sa propre base de données.

```pascal
// Service Commandes - utilise PostgreSQL
unit ServiceCommandes.Database;

interface

uses
  SQLDB, PQConnection;

type
  TDatabaseCommandes = class
  private
    FConnection: TPQConnection;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SauvegarderCommande(const ACommande: TCommande);
    function ChargerCommande(const AId: string): TCommande;
  end;

implementation

constructor TDatabaseCommandes.Create;
begin
  FConnection := TPQConnection.Create(nil);
  FConnection.HostName := 'localhost';
  FConnection.DatabaseName := 'commandes_db';
  FConnection.UserName := 'commandes_user';
  FConnection.Password := 'commandes_pass';
end;

// Service Produits - utilise SQLite
unit ServiceProduits.Database;

interface

uses
  SQLDB, SQLite3Conn;

type
  TDatabaseProduits = class
  private
    FConnection: TSQLite3Connection;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SauvegarderProduit(const AProduit: TProduit);
    function ChargerProduit(const AId: string): TProduit;
  end;

implementation

constructor TDatabaseProduits.Create;
begin
  FConnection := TSQLite3Connection.Create(nil);
  FConnection.DatabaseName := 'produits.db';
end;
```

### 2. Event Sourcing (Flux d'événements)

Stocker tous les changements comme une séquence d'événements.

```pascal
unit EventSourcing;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, Generics.Collections;

type
  // Événement de domaine
  TEvenementDomaine = class
  private
    FId: string;
    FAgregatId: string;
    FType: string;
    FDonnees: TJSONObject;
    FTimestamp: TDateTime;
    FVersion: Integer;
  public
    constructor Create(const AAgregatId, AType: string;
                      ADonnees: TJSONObject; AVersion: Integer);
    destructor Destroy; override;

    property Id: string read FId;
    property AgregatId: string read FAgregatId;
    property TypeEvenement: string read FType;
    property Donnees: TJSONObject read FDonnees;
    property Timestamp: TDateTime read FTimestamp;
    property Version: Integer read FVersion;
  end;

  // Store d'événements
  IEventStore = interface
    ['{D4E5F6A7-B8C9-D0E1-F2A3-B4C5D6E7F8A9}']
    procedure Ajouter(const AEvenement: TEvenementDomaine);
    function ChargerEvenements(const AAgregatId: string): TList<TEvenementDomaine>;
    function ChargerEvenements(const AAgregatId: string;
                              AVersionMin, AVersionMax: Integer): TList<TEvenementDomaine>;
  end;

  // Implémentation en mémoire (pour démo)
  TEventStoreMemoire = class(TInterfacedObject, IEventStore)
  private
    FEvenements: TObjectList<TEvenementDomaine>;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Ajouter(const AEvenement: TEvenementDomaine);
    function ChargerEvenements(const AAgregatId: string): TList<TEvenementDomaine>;
    function ChargerEvenements(const AAgregatId: string;
                              AVersionMin, AVersionMax: Integer): TList<TEvenementDomaine>;
  end;

  // Agrégat reconstituable depuis les événements
  TAgregatEventSource = class
  private
    FId: string;
    FVersion: Integer;
    FEvenementsPendants: TObjectList<TEvenementDomaine>;
  protected
    procedure AppliquerEvenement(const AEvenement: TEvenementDomaine); virtual; abstract;
    procedure AjouterEvenement(const AType: string; ADonnees: TJSONObject);
  public
    constructor Create(const AId: string);
    destructor Destroy; override;

    procedure ChargerDepuisHistorique(const AEvenements: TList<TEvenementDomaine>);
    function ObtenirEvenementsPendants: TList<TEvenementDomaine>;
    procedure MarquerEvenementsSauvegardes;

    property Id: string read FId;
    property Version: Integer read FVersion;
  end;

implementation

constructor TEvenementDomaine.Create(const AAgregatId, AType: string;
  ADonnees: TJSONObject; AVersion: Integer);
begin
  inherited Create;
  FId := TGuid.NewGuid.ToString;
  FAgregatId := AAgregatId;
  FType := AType;
  FDonnees := ADonnees;
  FTimestamp := Now;
  FVersion := AVersion;
end;

destructor TEvenementDomaine.Destroy;
begin
  FDonnees.Free;
  inherited;
end;

constructor TEventStoreMemoire.Create;
begin
  inherited Create;
  FEvenements := TObjectList<TEvenementDomaine>.Create(True);
end;

destructor TEventStoreMemoire.Destroy;
begin
  FEvenements.Free;
  inherited;
end;

procedure TEventStoreMemoire.Ajouter(const AEvenement: TEvenementDomaine);
begin
  FEvenements.Add(AEvenement);
  WriteLn(Format('Événement ajouté: %s (Agrégat: %s, Version: %d)',
    [AEvenement.TypeEvenement, AEvenement.AgregatId, AEvenement.Version]));
end;

function TEventStoreMemoire.ChargerEvenements(const AAgregatId: string): TList<TEvenementDomaine>;
var
  Evt: TEvenementDomaine;
begin
  Result := TList<TEvenementDomaine>.Create;
  for Evt in FEvenements do
  begin
    if Evt.AgregatId = AAgregatId then
      Result.Add(Evt);
  end;
end;

function TEventStoreMemoire.ChargerEvenements(const AAgregatId: string;
  AVersionMin, AVersionMax: Integer): TList<TEvenementDomaine>;
var
  Evt: TEvenementDomaine;
begin
  Result := TList<TEvenementDomaine>.Create;
  for Evt in FEvenements do
  begin
    if (Evt.AgregatId = AAgregatId) and
       (Evt.Version >= AVersionMin) and
       (Evt.Version <= AVersionMax) then
      Result.Add(Evt);
  end;
end;

constructor TAgregatEventSource.Create(const AId: string);
begin
  inherited Create;
  FId := AId;
  FVersion := 0;
  FEvenementsPendants := TObjectList<TEvenementDomaine>.Create(False);
end;

destructor TAgregatEventSource.Destroy;
begin
  FEvenementsPendants.Free;
  inherited;
end;

procedure TAgregatEventSource.AjouterEvenement(const AType: string; ADonnees: TJSONObject);
var
  Evt: TEvenementDomaine;
begin
  Inc(FVersion);
  Evt := TEvenementDomaine.Create(FId, AType, ADonnees, FVersion);
  FEvenementsPendants.Add(Evt);
  AppliquerEvenement(Evt);
end;

procedure TAgregatEventSource.ChargerDepuisHistorique(const AEvenements: TList<TEvenementDomaine>);
var
  Evt: TEvenementDomaine;
begin
  for Evt in AEvenements do
  begin
    AppliquerEvenement(Evt);
    FVersion := Evt.Version;
  end;
end;

function TAgregatEventSource.ObtenirEvenementsPendants: TList<TEvenementDomaine>;
begin
  Result := TList<TEvenementDomaine>.Create;
  Result.AddRange(FEvenementsPendants);
end;

procedure TAgregatEventSource.MarquerEvenementsSauvegardes;
begin
  FEvenementsPendants.Clear;
end;

end.
```

**Exemple d'agrégat avec Event Sourcing :**

```pascal
type
  TCompteEventSource = class(TAgregatEventSource)
  private
    FTitulaire: string;
    FSolde: Currency;
  protected
    procedure AppliquerEvenement(const AEvenement: TEvenementDomaine); override;
  public
    procedure Ouvrir(const ATitulaire: string; ASoldeInitial: Currency);
    procedure Crediter(AMontant: Currency);
    procedure Debiter(AMontant: Currency);

    property Titulaire: string read FTitulaire;
    property Solde: Currency read FSolde;
  end;

procedure TCompteEventSource.AppliquerEvenement(const AEvenement: TEvenementDomaine);
begin
  if AEvenement.TypeEvenement = 'CompteOuvert' then
  begin
    FTitulaire := AEvenement.Donnees.Get('titulaire', '');
    FSolde := AEvenement.Donnees.Get('soldeInitial', 0.0);
  end
  else if AEvenement.TypeEvenement = 'CompteCredite' then
  begin
    FSolde := FSolde + AEvenement.Donnees.Get('montant', 0.0);
  end
  else if AEvenement.TypeEvenement = 'CompteDebite' then
  begin
    FSolde := FSolde - AEvenement.Donnees.Get('montant', 0.0);
  end;
end;

procedure TCompteEventSource.Ouvrir(const ATitulaire: string; ASoldeInitial: Currency);
var
  Donnees: TJSONObject;
begin
  Donnees := TJSONObject.Create;
  Donnees.Add('titulaire', ATitulaire);
  Donnees.Add('soldeInitial', ASoldeInitial);
  AjouterEvenement('CompteOuvert', Donnees);
end;

procedure TCompteEventSource.Crediter(AMontant: Currency);
var
  Donnees: TJSONObject;
begin
  if AMontant <= 0 then
    raise Exception.Create('Montant doit être positif');

  Donnees := TJSONObject.Create;
  Donnees.Add('montant', AMontant);
  AjouterEvenement('CompteCredite', Donnees);
end;

procedure TCompteEventSource.Debiter(AMontant: Currency);
var
  Donnees: TJSONObject;
begin
  if AMontant <= 0 then
    raise Exception.Create('Montant doit être positif');
  if FSolde < AMontant then
    raise Exception.Create('Solde insuffisant');

  Donnees := TJSONObject.Create;
  Donnees.Add('montant', AMontant);
  AjouterEvenement('CompteDebite', Donnees);
end;

// Utilisation
var
  EventStore: IEventStore;
  Compte: TCompteEventSource;
  Evenements: TList<TEvenementDomaine>;
  Evt: TEvenementDomaine;
begin
  EventStore := TEventStoreMemoire.Create;

  // Créer un nouveau compte
  Compte := TCompteEventSource.Create('compte-123');
  try
    Compte.Ouvrir('Jean Dupont', 1000);
    Compte.Crediter(500);
    Compte.Debiter(200);

    // Sauvegarder les événements
    Evenements := Compte.ObtenirEvenementsPendants;
    try
      for Evt in Evenements do
        EventStore.Ajouter(Evt);
      Compte.MarquerEvenementsSauvegardes;
    finally
      Evenements.Free;
    end;

    WriteLn('Solde actuel: ', Compte.Solde:0:2);
  finally
    Compte.Free;
  end;

  // Reconstruire le compte depuis l'historique
  Compte := TCompteEventSource.Create('compte-123');
  try
    Evenements := EventStore.ChargerEvenements('compte-123');
    try
      Compte.ChargerDepuisHistorique(Evenements);
      WriteLn('Compte reconstruit - Solde: ', Compte.Solde:0:2);
    finally
      Evenements.Free;
    end;
  finally
    Compte.Free;
  end;
end;
```

## Déploiement multi-plateforme

### 1. Conteneurisation avec Docker

**Dockerfile pour un service FreePascal :**

```dockerfile
# Dockerfile
FROM ubuntu:22.04

# Installation FreePascal
RUN apt-get update && \
    apt-get install -y fpc && \
    apt-get clean

# Créer le répertoire de l'application
WORKDIR /app

# Copier le code source
COPY src/ ./src/
COPY compile.sh ./

# Compiler le service
RUN chmod +x compile.sh && ./compile.sh

# Port exposé
EXPOSE 8080

# Lancer le service
CMD ["./bin/service"]
```

**Script de compilation :**

```bash
#!/bin/bash
# compile.sh

fpc -O3 -Xs -XX \
    -Fu./src/units \
    -Fi./src/includes \
    -FU./obj \
    -o./bin/service \
    ./src/main.pas
```

**docker-compose.yml pour plusieurs services :**

```yaml
version: '3.8'

services:
  api-gateway:
    build: ./gateway
    ports:
      - "8080:8080"
    depends_on:
      - service-produits
      - service-commandes
    environment:
      - SERVICE_PRODUITS_URL=http://service-produits:8001
      - SERVICE_COMMANDES_URL=http://service-commandes:8002

  service-produits:
    build: ./produits
    ports:
      - "8001:8001"
    environment:
      - DB_HOST=db-produits
      - DB_NAME=produits
    depends_on:
      - db-produits

  service-commandes:
    build: ./commandes
    ports:
      - "8002:8002"
    environment:
      - DB_HOST=db-commandes
      - DB_NAME=commandes
    depends_on:
      - db-commandes

  db-produits:
    image: postgres:15
    environment:
      - POSTGRES_DB=produits
      - POSTGRES_USER=produits_user
      - POSTGRES_PASSWORD=produits_pass
    volumes:
      - produits-data:/var/lib/postgresql/data

  db-commandes:
    image: postgres:15
    environment:
      - POSTGRES_DB=commandes
      - POSTGRES_USER=commandes_user
      - POSTGRES_PASSWORD=commandes_pass
    volumes:
      - commandes-data:/var/lib/postgresql/data

volumes:
  produits-data:
  commandes-data:
```

### 2. Configuration multi-environnement

```pascal
unit Configuration;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser;

type
  TConfiguration = class
  private
    FEnvironnement: string;
    FPortHTTP: Integer;
    FDatabaseHost: string;
    FDatabaseName: string;
    FServiceURLs: TStringList;

    procedure ChargerDepuisFichier(const AFichier: string);
    procedure ChargerDepuisEnvironnement;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Charger;

    property Environnement: string read FEnvironnement;
    property PortHTTP: Integer read FPortHTTP;
    property DatabaseHost: string read FDatabaseHost;
    property DatabaseName: string read FDatabaseName;
    property ServiceURLs: TStringList read FServiceURLs;
  end;

implementation

constructor TConfiguration.Create;
begin
  inherited Create;
  FServiceURLs := TStringList.Create;
  FEnvironnement := 'development';
end;

destructor TConfiguration.Destroy;
begin
  FServiceURLs.Free;
  inherited;
end;

procedure TConfiguration.ChargerDepuisFichier(const AFichier: string);
var
  JSON: TJSONObject;
  Parser: TJSONParser;
  FileStream: TFileStream;
  JSONString: string;
begin
  if not FileExists(AFichier) then Exit;

  FileStream := TFileStream.Create(AFichier, fmOpenRead);
  try
    SetLength(JSONString, FileStream.Size);
    FileStream.Read(JSONString[1], FileStream.Size);

    Parser := TJSONParser.Create(JSONString, [joUTF8]);
    try
      JSON := Parser.Parse as TJSONObject;
      try
        FPortHTTP := JSON.Get('port', 8080);
        FDatabaseHost := JSON.Get('database.host', 'localhost');
        FDatabaseName := JSON.Get('database.name', 'mydb');
      finally
        JSON.Free;
      end;
    finally
      Parser.Free;
    end;
  finally
    FileStream.Free;
  end;
end;

procedure TConfiguration.ChargerDepuisEnvironnement;
begin
  // Variables d'environnement prioritaires
  FEnvironnement := GetEnvironmentVariable('APP_ENV');
  if FEnvironnement = '' then
    FEnvironnement := 'development';

  FPortHTTP := StrToIntDef(GetEnvironmentVariable('HTTP_PORT'), FPortHTTP);

  if GetEnvironmentVariable('DB_HOST') <> '' then
    FDatabaseHost := GetEnvironmentVariable('DB_HOST');

  if GetEnvironmentVariable('DB_NAME') <> '' then
    FDatabaseName := GetEnvironmentVariable('DB_NAME');

  // URLs des services
  if GetEnvironmentVariable('SERVICE_PRODUITS_URL') <> '' then
    FServiceURLs.Values['produits'] := GetEnvironmentVariable('SERVICE_PRODUITS_URL');
end;

procedure TConfiguration.Charger;
var
  FichierConfig: string;
begin
  // Charger depuis fichier selon l'environnement
  FichierConfig := Format('config.%s.json', [FEnvironnement]);
  ChargerDepuisFichier(FichierConfig);

  // Surcharger avec variables d'environnement
  ChargerDepuisEnvironnement;

  WriteLn('Configuration chargée:');
  WriteLn('  Environnement: ', FEnvironnement);
  WriteLn('  Port: ', FPortHTTP);
  WriteLn('  Base de données: ', FDatabaseHost, '/', FDatabaseName);
end;

end.
```

## Monitoring et Observabilité

### 1. Health Checks (Vérifications de santé)

```pascal
unit HealthCheck;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, SQLDB;

type
  THealthStatus = (hsHealthy, hsDegraded, hsUnhealthy);

  THealthCheck = class
  private
    FNom: string;
    FStatut: THealthStatus;
    FMessage: string;
  public
    constructor Create(const ANom: string);

    property Nom: string read FNom;
    property Statut: THealthStatus read FStatut write FStatut;
    property Message: string read FMessage write FMessage;
  end;

  THealthCheckService = class
  private
    FChecks: TList;
    FDatabaseConnection: TSQLConnection;
  public
    constructor Create(AConnection: TSQLConnection);
    destructor Destroy; override;

    function VerifierDatabase: THealthCheck;
    function VerifierDisque: THealthCheck;
    function VerifierMemoire: THealthCheck;
    function VerifierTout: TJSONObject;
  end;

implementation

uses
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  {$IFDEF UNIX}
  BaseUnix,
  {$ENDIF}
  DateUtils;

constructor THealthCheck.Create(const ANom: string);
begin
  inherited Create;
  FNom := ANom;
  FStatut := hsHealthy;
  FMessage := 'OK';
end;

constructor THealthCheckService.Create(AConnection: TSQLConnection);
begin
  inherited Create;
  FChecks := TList.Create;
  FDatabaseConnection := AConnection;
end;

destructor THealthCheckService.Destroy;
var
  i: Integer;
begin
  for i := 0 to FChecks.Count - 1 do
    THealthCheck(FChecks[i]).Free;
  FChecks.Free;
  inherited;
end;

function THealthCheckService.VerifierDatabase: THealthCheck;
var
  Query: TSQLQuery;
begin
  Result := THealthCheck.Create('database');

  try
    Query := TSQLQuery.Create(nil);
    try
      Query.Database := FDatabaseConnection;
      Query.SQL.Text := 'SELECT 1';
      Query.Open;
      Query.Close;

      Result.Statut := hsHealthy;
      Result.Message := 'Base de données accessible';
    finally
      Query.Free;
    end;
  except
    on E: Exception do
    begin
      Result.Statut := hsUnhealthy;
      Result.Message := 'Erreur: ' + E.Message;
    end;
  end;
end;

function THealthCheckService.VerifierDisque: THealthCheck;
{$IFDEF WINDOWS}
var
  FreeBytesAvailable, TotalNumberOfBytes, TotalNumberOfFreeBytes: Int64;
  PourcentageLibre: Double;
{$ENDIF}
begin
  Result := THealthCheck.Create('disk');

  {$IFDEF WINDOWS}
  if GetDiskFreeSpaceEx('C:\', FreeBytesAvailable,
                        TotalNumberOfBytes, @TotalNumberOfFreeBytes) then
  begin
    PourcentageLibre := (FreeBytesAvailable / TotalNumberOfBytes) * 100;

    if PourcentageLibre < 10 then
    begin
      Result.Statut := hsUnhealthy;
      Result.Message := Format('Espace disque critique: %.1f%%', [PourcentageLibre]);
    end
    else if PourcentageLibre < 20 then
    begin
      Result.Statut := hsDegraded;
      Result.Message := Format('Espace disque faible: %.1f%%', [PourcentageLibre]);
    end
    else
    begin
      Result.Statut := hsHealthy;
      Result.Message := Format('Espace disque OK: %.1f%%', [PourcentageLibre]);
    end;
  end;
  {$ENDIF}

  {$IFDEF UNIX}
  // Implémentation Unix avec statvfs
  Result.Statut := hsHealthy;
  Result.Message := 'Vérification disque non implémentée sur Unix';
  {$ENDIF}
end;

function THealthCheckService.VerifierMemoire: THealthCheck;
{$IFDEF WINDOWS}
var
  MemStatus: TMemoryStatus;
  PourcentageUtilisee: Double;
{$ENDIF}
begin
  Result := THealthCheck.Create('memory');

  {$IFDEF WINDOWS}
  MemStatus.dwLength := SizeOf(TMemoryStatus);
  GlobalMemoryStatus(MemStatus);
  PourcentageUtilisee := MemStatus.dwMemoryLoad;

  if PourcentageUtilisee > 90 then
  begin
    Result.Statut := hsUnhealthy;
    Result.Message := Format('Mémoire critique: %.0f%%', [PourcentageUtilisee]);
  end
  else if PourcentageUtilisee > 80 then
  begin
    Result.Statut := hsDegraded;
    Result.Message := Format('Mémoire élevée: %.0f%%', [PourcentageUtilisee]);
  end
  else
  begin
    Result.Statut := hsHealthy;
    Result.Message := Format('Mémoire OK: %.0f%%', [PourcentageUtilisee]);
  end;
  {$ENDIF}

  {$IFDEF UNIX}
  // Implémentation Unix
  Result.Statut := hsHealthy;
  Result.Message := 'Vérification mémoire non implémentée sur Unix';
  {$ENDIF}
end;

function THealthCheckService.VerifierTout: TJSONObject;
var
  CheckDB, CheckDisk, CheckMem: THealthCheck;
  StatutGlobal: THealthStatus;
  Checks: TJSONArray;
  CheckJSON: TJSONObject;
begin
  Result := TJSONObject.Create;
  Checks := TJSONArray.Create;

  StatutGlobal := hsHealthy;

  // Vérifier la base de données
  CheckDB := VerifierDatabase;
  CheckJSON := TJSONObject.Create;
  CheckJSON.Add('name', CheckDB.Nom);
  CheckJSON.Add('status', GetEnumName(TypeInfo(THealthStatus), Ord(CheckDB.Statut)));
  CheckJSON.Add('message', CheckDB.Message);
  Checks.Add(CheckJSON);
  if CheckDB.Statut > StatutGlobal then
    StatutGlobal := CheckDB.Statut;
  CheckDB.Free;

  // Vérifier le disque
  CheckDisk := VerifierDisque;
  CheckJSON := TJSONObject.Create;
  CheckJSON.Add('name', CheckDisk.Nom);
  CheckJSON.Add('status', GetEnumName(TypeInfo(THealthStatus), Ord(CheckDisk.Statut)));
  CheckJSON.Add('message', CheckDisk.Message);
  Checks.Add(CheckJSON);
  if CheckDisk.Statut > StatutGlobal then
    StatutGlobal := CheckDisk.Statut;
  CheckDisk.Free;

  // Vérifier la mémoire
  CheckMem := VerifierMemoire;
  CheckJSON := TJSONObject.Create;
  CheckJSON.Add('name', CheckMem.Nom);
  CheckJSON.Add('status', GetEnumName(TypeInfo(THealthStatus), Ord(CheckMem.Statut)));
  CheckJSON.Add('message', CheckMem.Message);
  Checks.Add(CheckJSON);
  if CheckMem.Statut > StatutGlobal then
    StatutGlobal := CheckMem.Statut;
  CheckMem.Free;

  // Résultat global
  Result.Add('status', GetEnumName(TypeInfo(THealthStatus), Ord(StatutGlobal)));
  Result.Add('timestamp', FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', Now));
  Result.Add('checks', Checks);
end;

end.
```

**Endpoint de santé dans le service :**

```pascal
procedure TMonService.EndpointHealth(ARequest: TRequest; AResponse: TResponse);
var
  HealthCheck: THealthCheckService;
  ResultJSON: TJSONObject;
begin
  HealthCheck := THealthCheckService.Create(FDatabaseConnection);
  try
    ResultJSON := HealthCheck.VerifierTout;
    try
      AResponse.ContentType := 'application/json';

      // Définir le code HTTP selon le statut
      if ResultJSON.Get('status', '') = 'hsHealthy' then
        AResponse.Code := 200
      else if ResultJSON.Get('status', '') = 'hsDegraded' then
        AResponse.Code := 200  // Toujours 200 mais signale la dégradation
      else
        AResponse.Code := 503; // Service Unavailable

      AResponse.Content := ResultJSON.AsJSON;
    finally
      ResultJSON.Free;
    end;
  finally
    HealthCheck.Free;
  end;
end;
```

### 2. Métriques et Logging

```pascal
unit Metrics;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, SyncObjs;

type
  // Compteur simple
  TCounter = class
  private
    FNom: string;
    FValeur: Int64;
    FLock: TCriticalSection;
  public
    constructor Create(const ANom: string);
    destructor Destroy; override;

    procedure Incrementer(ADelta: Int64 = 1);
    function ObtenirValeur: Int64;

    property Nom: string read FNom;
  end;

  // Jauge (valeur qui monte et descend)
  TGauge = class
  private
    FNom: string;
    FValeur: Double;
    FLock: TCriticalSection;
  public
    constructor Create(const ANom: string);
    destructor Destroy; override;

    procedure Definir(AValeur: Double);
    procedure Incrementer(ADelta: Double = 1);
    procedure Decrementer(ADelta: Double = 1);
    function ObtenirValeur: Double;

    property Nom: string read FNom;
  end;

  // Histogramme (pour mesurer les durées)
  THistogram = class
  private
    FNom: string;
    FValeurs: TList<Double>;
    FLock: TCriticalSection;
  public
    constructor Create(const ANom: string);
    destructor Destroy; override;

    procedure Observer(AValeur: Double);
    function ObtenirMoyenne: Double;
    function ObtenirMin: Double;
    function ObtenirMax: Double;
    function ObtenirPercentile(APercentile: Double): Double;

    property Nom: string read FNom;
  end;

  // Registre de métriques
  TMetricsRegistry = class
  private
    FCounters: TObjectDictionary<string, TCounter>;
    FGauges: TObjectDictionary<string, TGauge>;
    FHistograms: TObjectDictionary<string, THistogram>;
    FLock: TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;

    function ObtenirCounter(const ANom: string): TCounter;
    function ObtenirGauge(const ANom: string): TGauge;
    function ObtenirHistogram(const ANom: string): THistogram;

    function ExporterPrometheus: string;
  end;

implementation

uses
  Math;

// TCounter
constructor TCounter.Create(const ANom: string);
begin
  inherited Create;
  FNom := ANom;
  FValeur := 0;
  FLock := TCriticalSection.Create;
end;

destructor TCounter.Destroy;
begin
  FLock.Free;
  inherited;
end;

procedure TCounter.Incrementer(ADelta: Int64);
begin
  FLock.Enter;
  try
    Inc(FValeur, ADelta);
  finally
    FLock.Leave;
  end;
end;

function TCounter.ObtenirValeur: Int64;
begin
  FLock.Enter;
  try
    Result := FValeur;
  finally
    FLock.Leave;
  end;
end;

// TGauge
constructor TGauge.Create(const ANom: string);
begin
  inherited Create;
  FNom := ANom;
  FValeur := 0;
  FLock := TCriticalSection.Create;
end;

destructor TGauge.Destroy;
begin
  FLock.Free;
  inherited;
end;

procedure TGauge.Definir(AValeur: Double);
begin
  FLock.Enter;
  try
    FValeur := AValeur;
  finally
    FLock.Leave;
  end;
end;

procedure TGauge.Incrementer(ADelta: Double);
begin
  FLock.Enter;
  try
    FValeur := FValeur + ADelta;
  finally
    FLock.Leave;
  end;
end;

procedure TGauge.Decrementer(ADelta: Double);
begin
  FLock.Enter;
  try
    FValeur := FValeur - ADelta;
  finally
    FLock.Leave;
  end;
end;

function TGauge.ObtenirValeur: Double;
begin
  FLock.Enter;
  try
    Result := FValeur;
  finally
    FLock.Leave;
  end;
end;

// THistogram
constructor THistogram.Create(const ANom: string);
begin
  inherited Create;
  FNom := ANom;
  FValeurs := TList<Double>.Create;
  FLock := TCriticalSection.Create;
end;

destructor THistogram.Destroy;
begin
  FValeurs.Free;
  FLock.Free;
  inherited;
end;

procedure THistogram.Observer(AValeur: Double);
begin
  FLock.Enter;
  try
    FValeurs.Add(AValeur);

    // Garder seulement les 1000 dernières valeurs
    if FValeurs.Count > 1000 then
      FValeurs.Delete(0);
  finally
    FLock.Leave;
  end;
end;

function THistogram.ObtenirMoyenne: Double;
var
  Somme: Double;
  Valeur: Double;
begin
  FLock.Enter;
  try
    if FValeurs.Count = 0 then
      Exit(0);

    Somme := 0;
    for Valeur in FValeurs do
      Somme := Somme + Valeur;

    Result := Somme / FValeurs.Count;
  finally
    FLock.Leave;
  end;
end;

function THistogram.ObtenirMin: Double;
var
  Valeur: Double;
begin
  FLock.Enter;
  try
    if FValeurs.Count = 0 then
      Exit(0);

    Result := FValeurs[0];
    for Valeur in FValeurs do
      if Valeur < Result then
        Result := Valeur;
  finally
    FLock.Leave;
  end;
end;

function THistogram.ObtenirMax: Double;
var
  Valeur: Double;
begin
  FLock.Enter;
  try
    if FValeurs.Count = 0 then
      Exit(0);

    Result := FValeurs[0];
    for Valeur in FValeurs do
      if Valeur > Result then
        Result := Valeur;
  finally
    FLock.Leave;
  end;
end;

function THistogram.ObtenirPercentile(APercentile: Double): Double;
var
  ValeursTries: TList<Double>;
  Index: Integer;
begin
  FLock.Enter;
  try
    if FValeurs.Count = 0 then
      Exit(0);

    ValeursTries := TList<Double>.Create;
    try
      ValeursTries.AddRange(FValeurs);
      ValeursTries.Sort;

      Index := Trunc((APercentile / 100) * (ValeursTries.Count - 1));
      Result := ValeursTries[Index];
    finally
      ValeursTries.Free;
    end;
  finally
    FLock.Leave;
  end;
end;

// TMetricsRegistry
constructor TMetricsRegistry.Create;
begin
  inherited Create;
  FCounters := TObjectDictionary<string, TCounter>.Create([doOwnsValues]);
  FGauges := TObjectDictionary<string, TGauge>.Create([doOwnsValues]);
  FHistograms := TObjectDictionary<string, THistogram>.Create([doOwnsValues]);
  FLock := TCriticalSection.Create;
end;

destructor TMetricsRegistry.Destroy;
begin
  FHistograms.Free;
  FGauges.Free;
  FCounters.Free;
  FLock.Free;
  inherited;
end;

function TMetricsRegistry.ObtenirCounter(const ANom: string): TCounter;
begin
  FLock.Enter;
  try
    if not FCounters.TryGetValue(ANom, Result) then
    begin
      Result := TCounter.Create(ANom);
      FCounters.Add(ANom, Result);
    end;
  finally
    FLock.Leave;
  end;
end;

function TMetricsRegistry.ObtenirGauge(const ANom: string): TGauge;
begin
  FLock.Enter;
  try
    if not FGauges.TryGetValue(ANom, Result) then
    begin
      Result := TGauge.Create(ANom);
      FGauges.Add(ANom, Result);
    end;
  finally
    FLock.Leave;
  end;
end;

function TMetricsRegistry.ObtenirHistogram(const ANom: string): THistogram;
begin
  FLock.Enter;
  try
    if not FHistograms.TryGetValue(ANom, Result) then
    begin
      Result := THistogram.Create(ANom);
      FHistograms.Add(ANom, Result);
    end;
  finally
    FLock.Leave;
  end;
end;

function TMetricsRegistry.ExporterPrometheus: string;
var
  Builder: TStringBuilder;
  Paire: TPair<string, TCounter>;
  PaireGauge: TPair<string, TGauge>;
  PaireHisto: TPair<string, THistogram>;
begin
  Builder := TStringBuilder.Create;
  try
    FLock.Enter;
    try
      // Exporter les counters
      for Paire in FCounters do
      begin
        Builder.AppendFormat('# TYPE %s counter'#10, [Paire.Key]);
        Builder.AppendFormat('%s %d'#10, [Paire.Key, Paire.Value.ObtenirValeur]);
      end;

      // Exporter les gauges
      for PaireGauge in FGauges do
      begin
        Builder.AppendFormat('# TYPE %s gauge'#10, [PaireGauge.Key]);
        Builder.AppendFormat('%s %.2f'#10, [PaireGauge.Key, PaireGauge.Value.ObtenirValeur]);
      end;

      // Exporter les histogrammes
      for PaireHisto in FHistograms do
      begin
        Builder.AppendFormat('# TYPE %s histogram'#10, [PaireHisto.Key]);
        Builder.AppendFormat('%s_sum %.2f'#10,
          [PaireHisto.Key, PaireHisto.Value.ObtenirMoyenne]);
        Builder.AppendFormat('%s_min %.2f'#10,
          [PaireHisto.Key, PaireHisto.Value.ObtenirMin]);
        Builder.AppendFormat('%s_max %.2f'#10,
          [PaireHisto.Key, PaireHisto.Value.ObtenirMax]);
        Builder.AppendFormat('%s{quantile="0.50"} %.2f'#10,
          [PaireHisto.Key, PaireHisto.Value.ObtenirPercentile(50)]);
        Builder.AppendFormat('%s{quantile="0.95"} %.2f'#10,
          [PaireHisto.Key, PaireHisto.Value.ObtenirPercentile(95)]);
        Builder.AppendFormat('%s{quantile="0.99"} %.2f'#10,
          [PaireHisto.Key, PaireHisto.Value.ObtenirPercentile(99)]);
      end;
    finally
      FLock.Leave;
    end;

    Result := Builder.ToString;
  finally
    Builder.Free;
  end;
end;

end.
```

**Utilisation des métriques dans un service :**

```pascal
type
  TMonService = class
  private
    FMetrics: TMetricsRegistry;

    procedure TraiterRequete(ARequest: TRequest; AResponse: TResponse);
    procedure EndpointMetrics(ARequest: TRequest; AResponse: TResponse);
  public
    constructor Create;
    destructor Destroy; override;
  end;

constructor TMonService.Create;
begin
  inherited Create;
  FMetrics := TMetricsRegistry.Create;
end;

procedure TMonService.TraiterRequete(ARequest: TRequest; AResponse: TResponse);
var
  Debut: TDateTime;
  Duree: Double;
  Counter: TCounter;
  Histogram: THistogram;
begin
  Debut := Now;

  // Incrémenter le compteur de requêtes
  Counter := FMetrics.ObtenirCounter('http_requests_total');
  Counter.Incrementer;

  try
    // Traiter la requête
    AResponse.Content := '{"message": "OK"}';
    AResponse.Code := 200;

    // Incrémenter les requêtes réussies
    Counter := FMetrics.ObtenirCounter('http_requests_success');
    Counter.Incrementer;
  except
    on E: Exception do
    begin
      // Incrémenter les erreurs
      Counter := FMetrics.ObtenirCounter('http_requests_errors');
      Counter.Incrementer;
      raise;
    end;
  end;

  // Mesurer la durée
  Duree := MilliSecondsBetween(Now, Debut);
  Histogram := FMetrics.ObtenirHistogram('http_request_duration_ms');
  Histogram.Observer(Duree);
end;

procedure TMonService.EndpointMetrics(ARequest: TRequest; AResponse: TResponse);
begin
  AResponse.ContentType := 'text/plain; version=0.0.4';
  AResponse.Content := FMetrics.ExporterPrometheus;
end;
```

### 3. Logging structuré

```pascal
unit StructuredLogging;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, DateUtils;

type
  TLogLevel = (llDebug, llInfo, llWarning, llError, llFatal);

  TStructuredLogger = class
  private
    FServiceName: string;
    FEnvironment: string;

    function LogLevelToString(ALevel: TLogLevel): string;
  public
    constructor Create(const AServiceName, AEnvironment: string);

    procedure Log(ALevel: TLogLevel; const AMessage: string;
                 AContext: TJSONObject = nil);
    procedure Debug(const AMessage: string; AContext: TJSONObject = nil);
    procedure Info(const AMessage: string; AContext: TJSONObject = nil);
    procedure Warning(const AMessage: string; AContext: TJSONObject = nil);
    procedure Error(const AMessage: string; AContext: TJSONObject = nil);
    procedure Fatal(const AMessage: string; AContext: TJSONObject = nil);
  end;

implementation

constructor TStructuredLogger.Create(const AServiceName, AEnvironment: string);
begin
  inherited Create;
  FServiceName := AServiceName;
  FEnvironment := AEnvironment;
end;

function TStructuredLogger.LogLevelToString(ALevel: TLogLevel): string;
begin
  case ALevel of
    llDebug: Result := 'DEBUG';
    llInfo: Result := 'INFO';
    llWarning: Result := 'WARNING';
    llError: Result := 'ERROR';
    llFatal: Result := 'FATAL';
  end;
end;

procedure TStructuredLogger.Log(ALevel: TLogLevel; const AMessage: string;
  AContext: TJSONObject);
var
  LogEntry: TJSONObject;
  i: Integer;
begin
  LogEntry := TJSONObject.Create;
  try
    // Champs standards
    LogEntry.Add('timestamp', FormatDateTime('yyyy-mm-dd"T"hh:nn:ss.zzz"Z"', Now));
    LogEntry.Add('level', LogLevelToString(ALevel));
    LogEntry.Add('message', AMessage);
    LogEntry.Add('service', FServiceName);
    LogEntry.Add('environment', FEnvironment);

    // Ajouter le contexte additionnel
    if Assigned(AContext) then
    begin
      for i := 0 to AContext.Count - 1 do
        LogEntry.Add(AContext.Names[i], AContext.Items[i].Clone);
    end;

    // Écrire en JSON sur stdout
    WriteLn(LogEntry.AsJSON);
  finally
    LogEntry.Free;
  end;
end;

procedure TStructuredLogger.Debug(const AMessage: string; AContext: TJSONObject);
begin
  Log(llDebug, AMessage, AContext);
end;

procedure TStructuredLogger.Info(const AMessage: string; AContext: TJSONObject);
begin
  Log(llInfo, AMessage, AContext);
end;

procedure TStructuredLogger.Warning(const AMessage: string; AContext: TJSONObject);
begin
  Log(llWarning, AMessage, AContext);
end;

procedure TStructuredLogger.Error(const AMessage: string; AContext: TJSONObject);
begin
  Log(llError, AMessage, AContext);
end;

procedure TStructuredLogger.Fatal(const AMessage: string; AContext: TJSONObject);
begin
  Log(llFatal, AMessage, AContext);
end;

end.
```

**Utilisation du logger :**

```pascal
var
  Logger: TStructuredLogger;
  Context: TJSONObject;
begin
  Logger := TStructuredLogger.Create('service-commandes', 'production');
  try
    // Log simple
    Logger.Info('Service démarré');

    // Log avec contexte
    Context := TJSONObject.Create;
    try
      Context.Add('user_id', 'user-123');
      Context.Add('order_id', 'order-456');
      Context.Add('amount', 99.99);
      Logger.Info('Commande créée', Context);
    finally
      Context.Free;
    end;

    // Log d'erreur
    try
      // Opération qui peut échouer
    except
      on E: Exception do
      begin
        Context := TJSONObject.Create;
        try
          Context.Add('error_class', E.ClassName);
          Context.Add('error_message', E.Message);
          Logger.Error('Échec traitement commande', Context);
        finally
          Context.Free;
        end;
      end;
    end;
  finally
    Logger.Free;
  end;
end;
```

## Sécurité dans les microservices

### 1. Authentification JWT

```pascal
unit JWTAuth;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, base64, DCPsha256, HMAC;

type
  TJWTToken = class
  private
    FHeader: TJSONObject;
    FPayload: TJSONObject;
    FSignature: string;

    function EncodeBase64URL(const AData: string): string;
    function DecodeBase64URL(const AData: string): string;
    function GenerateSignature(const AData, ASecret: string): string;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetClaim(const AKey, AValue: string);
    procedure SetClaim(const AKey: string; AValue: Integer);
    procedure SetExpiration(ASeconds: Integer);

    function Generate(const ASecret: string): string;
    class function Verify(const AToken, ASecret: string): TJWTToken;

    function GetClaim(const AKey: string): string;
    function IsExpired: Boolean;
  end;

implementation

uses
  DateUtils;

constructor TJWTToken.Create;
begin
  inherited Create;
  FHeader := TJSONObject.Create;
  FHeader.Add('alg', 'HS256');
  FHeader.Add('typ', 'JWT');

  FPayload := TJSONObject.Create;
end;

destructor TJWTToken.Destroy;
begin
  FPayload.Free;
  FHeader.Free;
  inherited;
end;

function TJWTToken.EncodeBase64URL(const AData: string): string;
begin
  Result := EncodeStringBase64(AData);
  // Rendre compatible URL
  Result := StringReplace(Result, '+', '-', [rfReplaceAll]);
  Result := StringReplace(Result, '/', '_', [rfReplaceAll]);
  Result := StringReplace(Result, '=', '', [rfReplaceAll]);
end;

function TJWTToken.DecodeBase64URL(const AData: string): string;
var
  Data: string;
  Padding: Integer;
begin
  Data := AData;
  // Restaurer les caractères standard
  Data := StringReplace(Data, '-', '+', [rfReplaceAll]);
  Data := StringReplace(Data, '_', '/', [rfReplaceAll]);

  // Ajouter le padding si nécessaire
  Padding := Length(Data) mod 4;
  if Padding > 0 then
    Data := Data + StringOfChar('=', 4 - Padding);

  Result := DecodeStringBase64(Data);
end;

function TJWTToken.GenerateSignature(const AData, ASecret: string): string;
var
  HMAC: THMAC;
  Hash: TBytes;
begin
  HMAC := THMAC.Create(SHA256, ASecret);
  try
    Hash := HMAC.ComputeHMAC(AData);
    SetLength(Result, Length(Hash));
    Move(Hash[0], Result[1], Length(Hash));
    Result := EncodeBase64URL(Result);
  finally
    HMAC.Free;
  end;
end;

procedure TJWTToken.SetClaim(const AKey, AValue: string);
begin
  FPayload.Add(AKey, AValue);
end;

procedure TJWTToken.SetClaim(const AKey: string; AValue: Integer);
begin
  FPayload.Add(AKey, AValue);
end;

procedure TJWTToken.SetExpiration(ASeconds: Integer);
var
  ExpirationTime: Int64;
begin
  ExpirationTime := DateTimeToUnix(Now) + ASeconds;
  FPayload.Add('exp', ExpirationTime);
end;

function TJWTToken.Generate(const ASecret: string): string;
var
  HeaderEncoded, PayloadEncoded: string;
  DataToSign: string;
begin
  HeaderEncoded := EncodeBase64URL(FHeader.AsJSON);
  PayloadEncoded := EncodeBase64URL(FPayload.AsJSON);

  DataToSign := HeaderEncoded + '.' + PayloadEncoded;
  FSignature := GenerateSignature(DataToSign, ASecret);

  Result := DataToSign + '.' + FSignature;
end;

class function TJWTToken.Verify(const AToken, ASecret: string): TJWTToken;
var
  Parts: TStringList;
  DataToSign: string;
  ExpectedSignature: string;
  HeaderJSON, PayloadJSON: string;
  Parser: TJSONParser;
begin
  Parts := TStringList.Create;
  try
  Parts.Delimiter := '.';
  Parts.StrictDelimiter := True;
  Parts.DelimitedText := AToken;
  if Parts.Count <> 3 then
    raise Exception.Create('Token JWT invalide');

  Result := TJWTToken.Create;
  try
    // Vérifier la signature
    DataToSign := Parts[0] + '.' + Parts[1];
    ExpectedSignature := Result.GenerateSignature(DataToSign, ASecret);

    if ExpectedSignature <> Parts[2] then
      raise Exception.Create('Signature JWT invalide');

    // Décoder le header
    HeaderJSON := Result.DecodeBase64URL(Parts[0]);
    Parser := TJSONParser.Create(HeaderJSON, [joUTF8]);
    try
      Result.FHeader.Free;
      Result.FHeader := Parser.Parse as TJSONObject;
    finally
      Parser.Free;
    end;

    // Décoder le payload
    PayloadJSON := Result.DecodeBase64URL(Parts[1]);
    Parser := TJSONParser.Create(PayloadJSON, [joUTF8]);
    try
      Result.FPayload.Free;
      Result.FPayload := Parser.Parse as TJSONObject;
    finally
      Parser.Free;
    end;

    Result.FSignature := Parts[2];

    // Vérifier l'expiration
    if Result.IsExpired then
      raise Exception.Create('Token JWT expiré');
  except
    Result.Free;
    raise;
  end;
  finally
    Parts.Free;
  end;
end;

function TJWTToken.GetClaim(const AKey: string): string;
begin
  Result := FPayload.Get(AKey, '');
end;

function TJWTToken.IsExpired: Boolean;
var
  Exp: Int64;
  Now: Int64;
begin
  Exp := FPayload.Get('exp', 0);
  if Exp = 0 then
    Exit(False); // Pas d'expiration

  Now := DateTimeToUnix(SysUtils.Now);
  Result := Now >= Exp;
end;

end.
```

**Middleware d'authentification :**

```pascal
type
  TAuthMiddleware = class
  private
    FSecretKey: string;
  public
    constructor Create(const ASecretKey: string);

    function Authenticate(ARequest: TRequest; AResponse: TResponse): Boolean;
  end;

implementation

constructor TAuthMiddleware.Create(const ASecretKey: string);
begin
  inherited Create;
  FSecretKey := ASecretKey;
end;

function TAuthMiddleware.Authenticate(ARequest: TRequest; AResponse: TResponse): Boolean;
var
  AuthHeader: string;
  Token: string;
  JWT: TJWTToken;
begin
  Result := False;

  // Récupérer le header Authorization
  AuthHeader := ARequest.GetCustomHeader('Authorization');

  if AuthHeader = '' then
  begin
    AResponse.Code := 401;
    AResponse.Content := '{"error": "Token manquant"}';
    Exit;
  end;

  // Vérifier le format "Bearer <token>"
  if Copy(AuthHeader, 1, 7) <> 'Bearer ' then
  begin
    AResponse.Code := 401;
    AResponse.Content := '{"error": "Format token invalide"}';
    Exit;
  end;

  Token := Copy(AuthHeader, 8, Length(AuthHeader));

  // Vérifier et décoder le token
  try
    JWT := TJWTToken.Verify(Token, FSecretKey);
    try
      // Stocker les informations utilisateur dans la requête
      ARequest.CustomData.Values['user_id'] := JWT.GetClaim('user_id');
      ARequest.CustomData.Values['role'] := JWT.GetClaim('role');
      Result := True;
    finally
      JWT.Free;
    end;
  except
    on E: Exception do
    begin
      AResponse.Code := 401;
      AResponse.Content := Format('{"error": "%s"}', [E.Message]);
      Exit;
    end;
  end;
end;

end.
```

**Utilisation du middleware :**

```pascal
procedure TMonService.RouteProtegee(ARequest: TRequest; AResponse: TResponse);
var
  Auth: TAuthMiddleware;
  UserId: string;
begin
  Auth := TAuthMiddleware.Create(FSecretKey);
  try
    // Vérifier l'authentification
    if not Auth.Authenticate(ARequest, AResponse) then
      Exit; // Le middleware a déjà défini la réponse d'erreur

    // L'utilisateur est authentifié
    UserId := ARequest.CustomData.Values['user_id'];

    // Traiter la requête
    AResponse.Content := Format('{"message": "Bienvenue %s"}', [UserId]);
  finally
    Auth.Free;
  end;
end;
```

### 2. Rate Limiting (Limitation de débit)

```pascal
unit RateLimiting;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, SyncObjs, DateUtils;

type
  TRateLimitInfo = record
    RequetesRestantes: Integer;
    ResetAt: TDateTime;
  end;

  TRateLimiter = class
  private
    FLimiteParMinute: Integer;
    FRequetes: TDictionary<string, TList<TDateTime>>;
    FLock: TCriticalSection;

    procedure NettoyerVieuxEnregistrements;
  public
    constructor Create(ALimiteParMinute: Integer);
    destructor Destroy; override;

    function EstAutorise(const AIdentifiant: string): Boolean;
    function ObtenirInfo(const AIdentifiant: string): TRateLimitInfo;
  end;

implementation

constructor TRateLimiter.Create(ALimiteParMinute: Integer);
begin
  inherited Create;
  FLimiteParMinute := ALimiteParMinute;
  FRequetes := TDictionary<string, TList<TDateTime>>.Create;
  FLock := TCriticalSection.Create;
end;

destructor TRateLimiter.Destroy;
var
  Liste: TList<TDateTime>;
begin
  FLock.Enter;
  try
    for Liste in FRequetes.Values do
      Liste.Free;
    FRequetes.Free;
  finally
    FLock.Leave;
  end;
  FLock.Free;
  inherited;
end;

procedure TRateLimiter.NettoyerVieuxEnregistrements;
var
  Paire: TPair<string, TList<TDateTime>>;
  i: Integer;
  LimiteTemps: TDateTime;
begin
  LimiteTemps := IncMinute(Now, -1);

  for Paire in FRequetes do
  begin
    i := 0;
    while i < Paire.Value.Count do
    begin
      if Paire.Value[i] < LimiteTemps then
        Paire.Value.Delete(i)
      else
        Inc(i);
    end;
  end;
end;

function TRateLimiter.EstAutorise(const AIdentifiant: string): Boolean;
var
  Liste: TList<TDateTime>;
  LimiteTemps: TDateTime;
  Compteur: Integer;
begin
  FLock.Enter;
  try
    NettoyerVieuxEnregistrements;

    // Récupérer ou créer la liste pour cet identifiant
    if not FRequetes.TryGetValue(AIdentifiant, Liste) then
    begin
      Liste := TList<TDateTime>.Create;
      FRequetes.Add(AIdentifiant, Liste);
    end;

    // Compter les requêtes dans la dernière minute
    LimiteTemps := IncMinute(Now, -1);
    Compteur := 0;
    for i := 0 to Liste.Count - 1 do
      if Liste[i] >= LimiteTemps then
        Inc(Compteur);

    // Vérifier la limite
    Result := Compteur < FLimiteParMinute;

    if Result then
      Liste.Add(Now);
  finally
    FLock.Leave;
  end;
end;

function TRateLimiter.ObtenirInfo(const AIdentifiant: string): TRateLimitInfo;
var
  Liste: TList<TDateTime>;
  LimiteTemps: TDateTime;
  Compteur: Integer;
  PlusAncienne: TDateTime;
begin
  FLock.Enter;
  try
    Result.RequetesRestantes := FLimiteParMinute;
    Result.ResetAt := IncMinute(Now, 1);

    if FRequetes.TryGetValue(AIdentifiant, Liste) then
    begin
      LimiteTemps := IncMinute(Now, -1);
      Compteur := 0;
      PlusAncienne := Now;

      for i := 0 to Liste.Count - 1 do
      begin
        if Liste[i] >= LimiteTemps then
        begin
          Inc(Compteur);
          if Liste[i] < PlusAncienne then
            PlusAncienne := Liste[i];
        end;
      end;

      Result.RequetesRestantes := FLimiteParMinute - Compteur;
      if Result.RequetesRestantes < 0 then
        Result.RequetesRestantes := 0;

      Result.ResetAt := IncMinute(PlusAncienne, 1);
    end;
  finally
    FLock.Leave;
  end;
end;

end.
```

**Middleware de rate limiting :**

```pascal
type
  TRateLimitMiddleware = class
  private
    FRateLimiter: TRateLimiter;

    function ExtraireIdentifiant(ARequest: TRequest): string;
  public
    constructor Create(ALimiteParMinute: Integer);
    destructor Destroy; override;

    function VerifierLimite(ARequest: TRequest; AResponse: TResponse): Boolean;
  end;

implementation

constructor TRateLimitMiddleware.Create(ALimiteParMinute: Integer);
begin
  inherited Create;
  FRateLimiter := TRateLimiter.Create(ALimiteParMinute);
end;

destructor TRateLimitMiddleware.Destroy;
begin
  FRateLimiter.Free;
  inherited;
end;

function TRateLimitMiddleware.ExtraireIdentifiant(ARequest: TRequest): string;
var
  JWT: TJWTToken;
  Token: string;
begin
  // Essayer d'utiliser l'ID utilisateur du JWT
  Token := ARequest.GetCustomHeader('Authorization');
  if Copy(Token, 1, 7) = 'Bearer ' then
  begin
    try
      JWT := TJWTToken.Verify(Copy(Token, 8, Length(Token)), FSecretKey);
      try
        Result := JWT.GetClaim('user_id');
      finally
        JWT.Free;
      end;
    except
      // Si le JWT est invalide, utiliser l'IP
      Result := ARequest.RemoteAddr;
    end;
  end
  else
    // Utiliser l'adresse IP comme identifiant
    Result := ARequest.RemoteAddr;
end;

function TRateLimitMiddleware.VerifierLimite(ARequest: TRequest;
  AResponse: TResponse): Boolean;
var
  Identifiant: string;
  Info: TRateLimitInfo;
begin
  Identifiant := ExtraireIdentifiant(ARequest);

  Result := FRateLimiter.EstAutorise(Identifiant);
  Info := FRateLimiter.ObtenirInfo(Identifiant);

  // Ajouter les headers de rate limit
  AResponse.SetCustomHeader('X-RateLimit-Limit', IntToStr(FRateLimiter.FLimiteParMinute));
  AResponse.SetCustomHeader('X-RateLimit-Remaining', IntToStr(Info.RequetesRestantes));
  AResponse.SetCustomHeader('X-RateLimit-Reset',
    IntToStr(DateTimeToUnix(Info.ResetAt)));

  if not Result then
  begin
    AResponse.Code := 429; // Too Many Requests
    AResponse.Content := '{"error": "Trop de requêtes. Réessayez plus tard."}';
  end;
end;

end.
```

## Tests des microservices

### 1. Tests unitaires

```pascal
unit TestServiceCommandes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  ServiceCommandes, Domain.Entities.Commande;

type
  TTestServiceCommandes = class(TTestCase)
  private
    FService: TServiceCommandes;
    FMockRepository: IRepositoryCommande;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCreerCommande;
    procedure TestCreerCommandeSansLignes;
    procedure TestAnnulerCommande;
    procedure TestCalculerTotal;
  end;

implementation

uses
  MockRepository;

procedure TTestServiceCommandes.SetUp;
begin
  FMockRepository := TMockRepositoryCommande.Create;
  FService := TServiceCommandes.Create(FMockRepository);
end;

procedure TTestServiceCommandes.TearDown;
begin
  FService.Free;
end;

procedure TTestServiceCommandes.TestCreerCommande;
var
  CommandeId: string;
  Commande: TCommande;
begin
  // Arrange
  CommandeId := 'test-123';

  // Act
  Commande := FService.CreerCommande('client-1', [
    TProduitInfo.Create('prod-1', 2),
    TProduitInfo.Create('prod-2', 1)
  ]);

  // Assert
  AssertNotNull('La commande doit être créée', Commande);
  AssertEquals('La commande doit avoir 2 lignes', 2, Commande.NombreLignes);
  AssertTrue('Le repository doit avoir sauvegardé',
    TMockRepositoryCommande(FMockRepository).ASauvegarde);
end;

procedure TTestServiceCommandes.TestCreerCommandeSansLignes;
begin
  // Assert
  AssertException('Doit lever une exception',
    ECommandeException,
    procedure
    begin
      FService.CreerCommande('client-1', []);
    end
  );
end;

procedure TTestServiceCommandes.TestAnnulerCommande;
var
  Commande: TCommande;
begin
  // Arrange
  Commande := TCommande.Create('client-1');
  Commande.AjouterLigne(TProduit.Create('prod-1'), 1);
  Commande.Valider;
  FMockRepository.Ajouter(Commande);

  // Act
  FService.AnnulerCommande(Commande.Id);

  // Assert
  AssertEquals('La commande doit être annulée',
    TStatutCommande.Annulee,
    Commande.Statut);
end;

procedure TTestServiceCommandes.TestCalculerTotal;
var
  Commande: TCommande;
  Total: Currency;
begin
  // Arrange
  Commande := TCommande.Create('client-1');
  Commande.AjouterLigne(TProduit.Create('prod-1', 10.0), 2); // 20.0
  Commande.AjouterLigne(TProduit.Create('prod-2', 5.0), 3);  // 15.0

  // Act
  Total := FService.CalculerTotal(Commande);

  // Assert
  AssertEquals('Le total doit être 35.0', 35.0, Total, 0.01);
end;

initialization
  RegisterTest(TTestServiceCommandes);

end.
```

### 2. Tests d'intégration

```pascal
unit TestIntegrationAPI;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  fphttpclient, fpjson, jsonparser;

type
  TTestIntegrationAPI = class(TTestCase)
  private
    FClient: TFPHTTPClient;
    FBaseURL: string;

    function AppelGET(const APath: string): TJSONObject;
    function AppelPOST(const APath: string; ABody: TJSONObject): TJSONObject;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestHealthCheck;
    procedure TestCreerCommandeAPI;
    procedure TestAuthenticationRequise;
    procedure TestRateLimit;
  end;

implementation

procedure TTestIntegrationAPI.SetUp;
begin
  FClient := TFPHTTPClient.Create(nil);
  FBaseURL := 'http://localhost:8080'; // URL du service de test
end;

procedure TTestIntegrationAPI.TearDown;
begin
  FClient.Free;
end;

function TTestIntegrationAPI.AppelGET(const APath: string): TJSONObject;
var
  Response: string;
  Parser: TJSONParser;
begin
  Response := FClient.Get(FBaseURL + APath);
  Parser := TJSONParser.Create(Response, [joUTF8]);
  try
    Result := Parser.Parse as TJSONObject;
  finally
    Parser.Free;
  end;
end;

function TTestIntegrationAPI.AppelPOST(const APath: string;
  ABody: TJSONObject): TJSONObject;
var
  Response: string;
  Parser: TJSONParser;
begin
  FClient.RequestBody := TStringStream.Create(ABody.AsJSON);
  FClient.AddHeader('Content-Type', 'application/json');

  Response := FClient.Post(FBaseURL + APath);

  Parser := TJSONParser.Create(Response, [joUTF8]);
  try
    Result := Parser.Parse as TJSONObject;
  finally
    Parser.Free;
  end;
end;

procedure TTestIntegrationAPI.TestHealthCheck;
var
  Response: TJSONObject;
begin
  Response := AppelGET('/health');
  try
    AssertEquals('Le service doit être healthy',
      'healthy',
      Response.Get('status', ''));
  finally
    Response.Free;
  end;
end;

procedure TTestIntegrationAPI.TestCreerCommandeAPI;
var
  Body, Response: TJSONObject;
  CommandeId: string;
begin
  // Préparer le corps de la requête
  Body := TJSONObject.Create;
  try
    Body.Add('client_id', 'client-test');
    Body.Add('produits', TJSONArray.Create(['prod-1', 'prod-2']));
    Body.Add('quantites', TJSONArray.Create([2, 1]));

    // Appeler l'API
    Response := AppelPOST('/api/commandes', Body);
    try
      AssertTrue('La réponse doit contenir un ID',
        Response.Find('commande_id') <> nil);
      CommandeId := Response.Get('commande_id', '');
      AssertNotEquals('L''ID ne doit pas être vide', '', CommandeId);
    finally
      Response.Free;
    end;
  finally
    Body.Free;
  end;
end;

procedure TTestIntegrationAPI.TestAuthenticationRequise;
begin
  try
    AppelGET('/api/commandes/private');
    Fail('Devrait lever une exception 401');
  except
    on E: EHTTPClient do
      AssertEquals('Code 401 attendu', 401, E.StatusCode);
  end;
end;

procedure TTestIntegrationAPI.TestRateLimit;
var
  i: Integer;
  RaisedException: Boolean;
begin
  RaisedException := False;

  // Faire beaucoup de requêtes rapidement
  for i := 1 to 100 do
  begin
    try
      AppelGET('/api/test');
    except
      on E: EHTTPClient do
      begin
        if E.StatusCode = 429 then
        begin
          RaisedException := True;
          Break;
        end;
      end;
    end;
  end;

  AssertTrue('Le rate limiting doit bloquer après trop de requêtes',
    RaisedException);
end;

initialization
  RegisterTest(TTestIntegrationAPI);

end.
```

## Architecture complète d'un système microservices

### Structure d'un projet complet

```
ProjetMicroservices/
├── common/                    # Code partagé
│   ├── domain/               # Entités de domaine communes
│   ├── infrastructure/       # Utilitaires communs
│   │   ├── logging.pas
│   │   ├── metrics.pas
│   │   ├── circuitbreaker.pas
│   │   └── retrypolicy.pas
│   └── contracts/            # Interfaces de communication
│       ├── messages.pas
│       └── dtos.pas
│
├── services/
│   ├── api-gateway/          # Point d'entrée unique
│   │   ├── src/
│   │   │   ├── main.pas
│   │   │   ├── routing.pas
│   │   │   └── middleware/
│   │   ├── config/
│   │   └── Dockerfile
│   │
│   ├── service-produits/     # Service catalogue
│   │   ├── src/
│   │   │   ├── main.pas
│   │   │   ├── api/
│   │   │   ├── domain/
│   │   │   └── infrastructure/
│   │   ├── config/
│   │   ├── tests/
│   │   └── Dockerfile
│   │
│   ├── service-commandes/    # Service commandes
│   │   ├── src/
│   │   ├── config/
│   │   ├── tests/
│   │   └── Dockerfile
│   │
│   ├── service-paiements/    # Service paiements
│   │   ├── src/
│   │   ├── config/
│   │   ├── tests/
│   │   └── Dockerfile
│   │
│   └── service-notifications/ # Service notifications
│       ├── src/
│       ├── config/
│       ├── tests/
│       └── Dockerfile
│
├── infrastructure/
│   ├── docker-compose.yml    # Orchestration locale
│   ├── kubernetes/           # Déploiement K8s
│   │   ├── deployments/
│   │   ├── services/
│   │   └── ingress/
│   └── monitoring/
│       ├── prometheus.yml
│       └── grafana/
│
├── scripts/
│   ├── build-all.sh         # Compilation de tous les services
│   ├── deploy.sh            # Déploiement
│   └── test-all.sh          # Tests complets
│
└── docs/
    ├── architecture.md
    └── api/
```

### Script de build automatisé

```bash
#!/bin/bash
# build-all.sh

set -e  # Arrêter en cas d'erreur

echo "=== Compilation des microservices FreePascal ==="

# Couleurs pour les messages
GREEN='\033[0;32m'
RED='\033[0;31m'
NC='\033[0m' # No Color

# Fonction de compilation
compile_service() {
    local service=$1
    echo -e "${GREEN}Compilation de $service...${NC}"

    cd services/$service

    # Nettoyer
    rm -rf obj bin
    mkdir -p obj bin

    # Compiler
    fpc -O3 -Xs -XX \
        -Fu../../common/domain \
        -Fu../../common/infrastructure \
        -Fu../../common/contracts \
        -Fu./src \
        -FU./obj \
        -o./bin/$service \
        ./src/main.pas

    if [ $? -eq 0 ]; then
        echo -e "${GREEN}✓ $service compilé avec succès${NC}"
    else
        echo -e "${RED}✗ Erreur compilation $service${NC}"
        exit 1
    fi

    cd ../..
}

# Compiler tous les services
compile_service "api-gateway"
compile_service "service-produits"
compile_service "service-commandes"
compile_service "service-paiements"
compile_service "service-notifications"

echo -e "${GREEN}=== Tous les services sont compilés ===${NC}"

# Construire les images Docker
echo "=== Construction des images Docker ==="
docker-compose -f infrastructure/docker-compose.yml build

echo -e "${GREEN}=== Build terminé avec succès ===${NC}"
```

### Configuration Kubernetes

```yaml
# kubernetes/deployments/service-produits.yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: service-produits
  labels:
    app: service-produits
spec:
  replicas: 3
  selector:
    matchLabels:
      app: service-produits
  template:
    metadata:
      labels:
        app: service-produits
    spec:
      containers:
      - name: service-produits
        image: monregistry/service-produits:latest
        ports:
        - containerPort: 8001
        env:
        - name: DB_HOST
          value: "postgres-produits"
        - name: DB_NAME
          value: "produits"
        - name: DB_USER
          valueFrom:
            secretKeyRef:
              name: db-credentials
              key: username
        - name: DB_PASSWORD
          valueFrom:
            secretKeyRef:
              name: db-credentials
              key: password
        livenessProbe:
          httpGet:
            path: /health
            port: 8001
          initialDelaySeconds: 30
          periodSeconds: 10
        readinessProbe:
          httpGet:
            path: /health
            port: 8001
          initialDelaySeconds: 5
          periodSeconds: 5
        resources:
          requests:
            memory: "64Mi"
            cpu: "100m"
          limits:
            memory: "128Mi"
            cpu: "200m"
---
apiVersion: v1
kind: Service
metadata:
  name: service-produits
spec:
  selector:
    app: service-produits
  ports:
  - protocol: TCP
    port: 8001
    targetPort: 8001
  type: ClusterIP
```

## Bonnes pratiques des microservices avec FreePascal

### 1. Conception des services

**Principe de responsabilité unique :**
- Chaque service doit avoir une seule raison de changer
- Limiter les dépendances entre services
- Définir des frontières claires (Bounded Contexts)

**Exemple de découpage :**
```
❌ Mauvais : Service "Gestion"
  - Gère les produits, commandes, clients, factures

✅ Bon : Services séparés
  - Service Catalogue : gère uniquement les produits
  - Service Commandes : gère uniquement les commandes
  - Service Clients : gère uniquement les clients
  - Service Facturation : gère uniquement les factures
```

### 2. Communication entre services

**Préférer l'asynchrone pour la communication inter-services :**

```pascal
// ❌ Mauvais : Appel synchrone en cascade
procedure TraiterCommande(Commande: TCommande);
begin
  // Appel synchrone 1
  VerifierStock(Commande.Produits);

  // Appel synchrone 2
  TraiterPaiement(Commande.Montant);

  // Appel synchrone 3
  EnvoyerNotification(Commande.ClientId);
end;

// ✅ Bon : Communication asynchrone par événements
procedure TraiterCommande(Commande: TCommande);
begin
  // Valider la commande localement
  Commande.Valider;
  Repository.Sauvegarder(Commande);

  // Publier un événement
  PublierEvenement(TEvenementCommandeValidee.Create(Commande));

  // Les autres services réagiront de manière asynchrone
end;
```

### 3. Gestion des erreurs

**Implémenter la résilience :**

```pascal
function AppelerServiceAvecResilience(const URL: string): string;
var
  CircuitBreaker: TCircuitBreaker;
  Retry: TRetryPolicy;
begin
  CircuitBreaker := TCircuitBreaker.Create(5, 60);
  Retry := TRetryPolicy.Create(3, 100, 2.0);
  try
    Result := CircuitBreaker.Executer(
      function: string
      begin
        Result := Retry.ExecuterAvecRetry<string>(
          function: string
          begin
            Result := HTTPClient.Get(URL);
          end
        );
      end
    );
  finally
    Retry.Free;
    CircuitBreaker.Free;
  end;
end;
```

### 4. Versioning des APIs

**Utiliser le versioning dans l'URL :**

```pascal
// Enregistrer plusieurs versions de l'API
HTTPRouter.RegisterRoute('/api/v1/produits', rmGet, @GetProduitsV1);
HTTPRouter.RegisterRoute('/api/v2/produits', rmGet, @GetProduitsV2);

// Ou dans le header
procedure GetProduits(ARequest: TRequest; AResponse: TResponse);
var
  Version: string;
begin
  Version := ARequest.GetCustomHeader('API-Version');

  if Version = 'v1' then
    TraiterRequeteV1(ARequest, AResponse)
  else if Version = 'v2' then
    TraiterRequeteV2(ARequest, AResponse)
  else
    TraiterRequeteV2(ARequest, AResponse); // Version par défaut
end;
```

### 5. Documentation des APIs

**Générer une documentation OpenAPI/Swagger :**

```pascal
function GenererSpecificationOpenAPI: TJSONObject;
var
  Spec: TJSONObject;
  Paths: TJSONObject;
begin
  Spec := TJSONObject.Create;

  Spec.Add('openapi', '3.0.0');
  Spec.Add('info', TJSONObject.Create([
    'title', 'API Service Produits',
    'version', '1.0.0',
    'description', 'API de gestion du catalogue produits'
  ]));

  Paths := TJSONObject.Create;

  // Définir les endpoints
  Paths.Add('/produits', TJSONObject.Create([
    'get', TJSONObject.Create([
      'summary', 'Liste tous les produits',
      'responses', TJSONObject.Create([
        '200', TJSONObject.Create([
          'description', 'Liste des produits'
        ])
      ])
    ])
  ]));

  Spec.Add('paths', Paths);
  Result := Spec;
end;

// Endpoint pour servir la spec
procedure EndpointOpenAPI(ARequest: TRequest; AResponse: TResponse);
var
  Spec: TJSONObject;
begin
  Spec := GenererSpecificationOpenAPI;
  try
    AResponse.ContentType := 'application/json';
    AResponse.Content := Spec.FormatJSON;
  finally
    Spec.Free;
  end;
end;
```

## Conclusion

Les microservices avec FreePascal offrent une architecture moderne et performante pour les systèmes distribués complexes. Les points clés à retenir :

### Avantages de FreePascal pour les microservices

1. **Performance** : Compilation native = services rapides et légers
2. **Faible empreinte mémoire** : Idéal pour multiplier les instances
3. **Démarrage instantané** : Pas de JVM ou runtime à charger
4. **Portabilité** : Un même code tourne sur Windows et Linux
5. **Stabilité** : Pas de garbage collection imprévisible

### Défis à relever

1. **Écosystème moins riche** que Java ou .NET
2. **Nécessite d'implémenter soi-même** certains patterns
3. **Communauté plus petite** pour l'aide et les bibliothèques
4. **Documentation moins abondante** sur les patterns modernes

### Quand utiliser des microservices ?

**✅ Utilisez des microservices quand :**
- L'application est complexe avec plusieurs domaines métier distincts
- Vous avez plusieurs équipes travaillant en parallèle
- Différentes parties du système ont des besoins de scalabilité différents
- Vous devez déployer indépendamment certaines fonctionnalités
- Vous acceptez la complexité opérationnelle supplémentaire

**❌ Évitez les microservices quand :**
- Votre application est simple et monolithique suffit
- Vous avez une petite équipe (< 5 personnes)
- Vous débutez le projet (commencez monolithique, migrez si nécessaire)
- Vous n'avez pas l'infrastructure pour gérer des services distribués
- Les performances de communication réseau sont critiques

### Patterns essentiels à maîtriser

1. **API Gateway** : Point d'entrée unique
2. **Service Discovery** : Localiser dynamiquement les services
3. **Circuit Breaker** : Prévenir les cascades de pannes
4. **Saga Pattern** : Transactions distribuées
5. **Event Sourcing** : Historique complet des changements
6. **CQRS** : Séparer lecture et écriture
7. **Health Checks** : Vérifier l'état des services
8. **Distributed Tracing** : Suivre les requêtes entre services

### Évolution progressive

**Approche recommandée : Monolithe modulaire → Microservices**

```pascal
// Étape 1 : Monolithe bien structuré
Program MonolitheModulaire;
uses
  ModuleProduits,    // Module bien isolé
  ModuleCommandes,   // Module bien isolé
  ModuleClients;     // Module bien isolé
begin
  // Tout dans un seul processus
end.

// Étape 2 : Extraire un service à la fois
Program ServiceProduits;
uses
  ModuleProduits;  // Réutiliser le module existant
begin
  // Maintenant un service indépendant
end.
```

### Checklist de démarrage d'un projet microservices

**Architecture :**
- [ ] Définir les bounded contexts (limites des services)
- [ ] Choisir le style de communication (REST, événements, gRPC)
- [ ] Concevoir le schéma de données (base par service ou partagée)
- [ ] Planifier la gestion des transactions distribuées

**Infrastructure :**
- [ ] Configurer Docker pour la conteneurisation
- [ ] Mettre en place un registre de services
- [ ] Configurer un API Gateway
- [ ] Préparer l'orchestration (Docker Compose, Kubernetes)

**Observabilité :**
- [ ] Implémenter le logging structuré
- [ ] Configurer les métriques (Prometheus)
- [ ] Mettre en place les health checks
- [ ] Configurer le tracing distribué

**Sécurité :**
- [ ] Implémenter l'authentification (JWT)
- [ ] Configurer TLS/SSL entre services
- [ ] Mettre en place le rate limiting
- [ ] Gérer les secrets de manière sécurisée

**Testing :**
- [ ] Tests unitaires de chaque service
- [ ] Tests d'intégration des APIs
- [ ] Tests de contrat entre services
- [ ] Tests de charge et de résilience

**CI/CD :**
- [ ] Pipeline de build automatisé
- [ ] Tests automatisés dans le pipeline
- [ ] Déploiement automatisé
- [ ] Rollback automatique en cas d'erreur

### Exemple complet minimal

Voici un service minimal mais complet incluant les bonnes pratiques :

```pascal
program ServiceMinimal;

{$mode objfpc}{$H+}

uses
  fphttpapp, httpdefs, httproute, fpjson,
  Configuration, StructuredLogging, Metrics, HealthCheck, JWTAuth;

type
  TServiceMinimal = class
  private
    FConfig: TConfiguration;
    FLogger: TStructuredLogger;
    FMetrics: TMetricsRegistry;
    FHealthCheck: THealthCheckService;

    procedure EndpointHealth(AReq: TRequest; AResp: TResponse);
    procedure EndpointMetrics(AReq: TRequest; AResp: TResponse);
    procedure EndpointAPI(AReq: TRequest; AResp: TResponse);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Demarrer;
  end;

constructor TServiceMinimal.Create;
begin
  inherited Create;

  // Configuration
  FConfig := TConfiguration.Create;
  FConfig.Charger;

  // Logging
  FLogger := TStructuredLogger.Create(
    'service-minimal',
    FConfig.Environnement
  );
  FLogger.Info('Initialisation du service');

  // Métriques
  FMetrics := TMetricsRegistry.Create;

  // Health check
  FHealthCheck := THealthCheckService.Create(nil);
end;

destructor TServiceMinimal.Destroy;
begin
  FLogger.Info('Arrêt du service');
  FHealthCheck.Free;
  FMetrics.Free;
  FLogger.Free;
  FConfig.Free;
  inherited;
end;

procedure TServiceMinimal.EndpointHealth(AReq: TRequest; AResp: TResponse);
var
  Result: TJSONObject;
begin
  Result := FHealthCheck.VerifierTout;
  try
    AResp.ContentType := 'application/json';
    AResp.Content := Result.AsJSON;

    if Result.Get('status', '') = 'healthy' then
      AResp.Code := 200
    else
      AResp.Code := 503;
  finally
    Result.Free;
  end;
end;

procedure TServiceMinimal.EndpointMetrics(AReq: TRequest; AResp: TResponse);
begin
  AResp.ContentType := 'text/plain';
  AResp.Content := FMetrics.ExporterPrometheus;
end;

procedure TServiceMinimal.EndpointAPI(AReq: TRequest; AResp: TResponse);
var
  Debut: TDateTime;
  Duree: Double;
  Context: TJSONObject;
  Counter: TCounter;
  Histogram: THistogram;
begin
  Debut := Now;

  // Métriques
  Counter := FMetrics.ObtenirCounter('api_requests_total');
  Counter.Incrementer;

  try
    // Traiter la requête
    AResp.ContentType := 'application/json';
    AResp.Content := '{"message": "Service minimal FreePascal"}';

    // Logger
    Context := TJSONObject.Create;
    try
      Context.Add('method', AReq.Method);
      Context.Add('path', AReq.PathInfo);
      Context.Add('status', 200);
      FLogger.Info('Requête traitée', Context);
    finally
      Context.Free;
    end;

  except
    on E: Exception do
    begin
      Counter := FMetrics.ObtenirCounter('api_requests_errors');
      Counter.Incrementer;

      Context := TJSONObject.Create;
      try
        Context.Add('error', E.Message);
        FLogger.Error('Erreur traitement requête', Context);
      finally
        Context.Free;
      end;

      AResp.Code := 500;
      AResp.Content := Format('{"error": "%s"}', [E.Message]);
    end;
  end;

  // Mesurer la durée
  Duree := MilliSecondsBetween(Now, Debut);
  Histogram := FMetrics.ObtenirHistogram('api_request_duration_ms');
  Histogram.Observer(Duree);
end;

procedure TServiceMinimal.Demarrer;
begin
  // Enregistrer les routes
  HTTPRouter.RegisterRoute('/health', rmGet, @EndpointHealth);
  HTTPRouter.RegisterRoute('/metrics', rmGet, @EndpointMetrics);
  HTTPRouter.RegisterRoute('/api/*', rmAll, @EndpointAPI);

  // Configurer et démarrer le serveur
  Application.Port := FConfig.PortHTTP;
  Application.Initialize;

  FLogger.Info('Service démarré', TJSONObject.Create([
    'port', FConfig.PortHTTP,
    'environment', FConfig.Environnement
  ]));

  Application.Run;
end;

var
  Service: TServiceMinimal;
begin
  Service := TServiceMinimal.Create;
  try
    Service.Demarrer;
  finally
    Service.Free;
  end;
end.
```

### Ressources pour aller plus loin

**Livres recommandés :**
- "Building Microservices" par Sam Newman
- "Microservices Patterns" par Chris Richardson
- "Domain-Driven Design" par Eric Evans

**Outils pour FreePascal :**
- **mORMot** : Framework SOA/REST complet pour FreePascal
- **Brook Framework** : Framework web léger et performant
- **Synapse** : Bibliothèque réseau complète
- **ZeosLib** : Accès multi-bases de données

**Monitoring et observabilité :**
- **Prometheus** : Collecte de métriques
- **Grafana** : Visualisation des métriques
- **ELK Stack** : Centralisation des logs
- **Jaeger/Zipkin** : Tracing distribué

**Orchestration :**
- **Docker Compose** : Développement local
- **Kubernetes** : Production à grande échelle
- **Docker Swarm** : Alternative plus simple à K8s

### Métriques de succès d'une architecture microservices

**Indicateurs techniques :**
- Temps de déploiement < 10 minutes par service
- Taux de disponibilité > 99.9% par service
- Temps de réponse P95 < 100ms
- Taux d'erreur < 0.1%

**Indicateurs organisationnels :**
- Autonomie des équipes (pas de dépendances de déploiement)
- Fréquence de déploiement (plusieurs fois par jour)
- Temps de récupération après incident (MTTR < 1h)
- Scalabilité indépendante par service

### Anti-patterns à éviter

**1. Microservices trop petits (Nanoservices)**
```pascal
// ❌ Trop granulaire
Service GetUserName
Service GetUserEmail
Service GetUserAge

// ✅ Granularité appropriée
Service UserManagement
```

**2. Base de données partagée**
```pascal
// ❌ Tous les services accèdent à la même DB
ServiceA → [Base de données unique] ← ServiceB

// ✅ Base par service
ServiceA → [DB A]
ServiceB → [DB B]
```

**3. Couplage fort entre services**
```pascal
// ❌ ServiceA connaît trop ServiceB
if ServiceB.GetStatus = 'available' then
  ServiceB.DoSomething;

// ✅ Communication par événements
PublishEvent('UserCreated');
// ServiceB réagit s'il est intéressé
```

**4. Absence de résilience**
```pascal
// ❌ Pas de gestion d'erreur
Result := HTTPClient.Get(ServiceURL);

// ✅ Avec résilience
Result := CircuitBreaker.Execute(
  function: string
  begin
    Result := RetryPolicy.Execute(
      function: string
      begin
        Result := HTTPClient.Get(ServiceURL);
      end
    );
  end
);
```

### Feuille de route d'apprentissage

**Niveau 1 - Fondamentaux (2-3 mois)**
1. Maîtriser FreePascal et la programmation orientée objet
2. Comprendre HTTP/REST et JSON
3. Apprendre les bases de données et SQL
4. Découvrir Docker et les conteneurs

**Niveau 2 - Patterns de base (3-4 mois)**
1. Implémenter un monolithe bien structuré
2. Créer des APIs REST simples
3. Mettre en place logging et monitoring
4. Apprendre les tests automatisés

**Niveau 3 - Microservices (4-6 mois)**
1. Découper un monolithe en services
2. Implémenter API Gateway
3. Gérer la communication asynchrone
4. Mettre en place Service Discovery

**Niveau 4 - Patterns avancés (6-12 mois)**
1. Event Sourcing et CQRS
2. Saga Pattern pour les transactions
3. Circuit Breaker et résilience
4. Observabilité complète

**Niveau 5 - Production (continu)**
1. Orchestration avec Kubernetes
2. CI/CD automatisé
3. Monitoring et alerting
4. Optimisation des performances

### Conclusion finale

Les microservices représentent une évolution naturelle des architectures logicielles pour répondre aux besoins modernes de scalabilité, de déploiement continu et d'organisation en équipes autonomes.

**FreePascal est un excellent choix** pour implémenter des microservices grâce à :
- Sa **performance native** comparable au C/C++
- Sa **faible consommation de ressources**
- Sa **portabilité** Windows/Linux sans compromis
- Sa **stabilité** en production

Cependant, les microservices ne sont **pas une solution miracle**. Ils apportent leur lot de complexité qu'il faut maîtriser :
- Distribution réseau
- Cohérence des données
- Débogage distribué
- Opérations complexes

**Recommandation finale :** Commencez simple avec un monolithe bien structuré. Migrez progressivement vers les microservices uniquement quand vous en avez réellement besoin et que vous avez les compétences et l'infrastructure pour les gérer correctement.

**La réussite d'une architecture microservices** repose plus sur l'organisation, les processus et la culture d'équipe que sur la technologie elle-même. FreePascal vous donne les outils techniques, à vous de construire l'organisation adaptée.

Bonne chance dans votre voyage vers les architectures distribuées ! 🚀

⏭️ [Event Sourcing et CQRS](/21-architecture-logicielle-avancee/03-event-sourcing-cqrs.md)
