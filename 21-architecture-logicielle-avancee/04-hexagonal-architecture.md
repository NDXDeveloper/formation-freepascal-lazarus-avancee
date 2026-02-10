🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 21.4 Hexagonal Architecture

## Introduction

L'**Architecture Hexagonale**, également appelée **Ports et Adaptateurs** (Ports and Adapters), est un pattern architectural créé par Alistair Cockburn en 2005. Son objectif principal est d'isoler complètement la logique métier de tout ce qui est externe : interface utilisateur, base de données, services tiers, etc.

### Pourquoi "Hexagonale" ?

Le nom vient de la représentation graphique en forme d'hexagone, mais le nombre de côtés n'a pas d'importance. L'hexagone symbolise simplement que l'application a plusieurs "faces" pour communiquer avec l'extérieur.

```
                    ┌─────────────┐
                    │  Interface  │
                    │     Web     │
                    └──────┬──────┘
                           │
           ┌───────────────┼───────────────┐
           │               │               │
      ┌────▼────┐    ┌─────▼──────┐  ┌────▼────┐
      │  Port   │    │            │  │  Port   │
      │   UI    │───►│   DOMAINE  │◄─┤   DB    │
      └─────────┘    │   MÉTIER   │  └─────────┘
                     │  (Hexagone) │
                     └──────┬──────┘
                            │
                       ┌────▼────┐
                       │  Port   │
                       │  Email  │
                       └─────────┘
```

### Principe fondamental

**Le domaine métier ne doit dépendre de rien d'externe. Tout dépend du domaine.**

```pascal
// ❌ Mauvais : le domaine dépend de la base de données
unit Domain.Client;  
uses
  SQLDB; // ← Dépendance externe !

type
  TClient = class
  private
    FConnection: TSQLConnection; // ← Couplage fort
  end;

// ✅ Bon : le domaine ne dépend de rien
unit Domain.Client;
// Aucun uses externe !

type
  TClient = class
  private
    FNom: string;
    FEmail: string;
  public
    procedure ChangerEmail(const ANouvelEmail: string);
  end;
```

## Les trois couches de l'architecture hexagonale

### 1. Le Domaine (Centre de l'hexagone)

Le **domaine** contient toute la logique métier pure, sans aucune dépendance externe.

**Contient :**
- Entités métier
- Value Objects
- Règles métier
- Interfaces des ports

**Ne contient PAS :**
- Code de base de données
- Code d'interface utilisateur
- Frameworks
- Bibliothèques externes

```pascal
unit Domain.Entities.Commande;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections;

type
  TStatutCommande = (scBrouillon, scValidee, scExpediee, scLivree, scAnnulee);

  TLigneCommande = class
  private
    FProduitId: string;
    FQuantite: Integer;
    FPrixUnitaire: Currency;
  public
    constructor Create(const AProduitId: string; AQuantite: Integer;
                      APrixUnitaire: Currency);

    function CalculerSousTotal: Currency;

    property ProduitId: string read FProduitId;
    property Quantite: Integer read FQuantite write FQuantite;
    property PrixUnitaire: Currency read FPrixUnitaire;
  end;

  TCommande = class
  private
    FId: string;
    FClientId: string;
    FLignes: TObjectList<TLigneCommande>;
    FStatut: TStatutCommande;
    FDateCreation: TDateTime;

    procedure ValiderReglesMetier;
  public
    constructor Create(const AClientId: string);
    destructor Destroy; override;

    // Opérations métier
    procedure AjouterLigne(const AProduitId: string; AQuantite: Integer;
                          APrixUnitaire: Currency);
    procedure SupprimerLigne(const AProduitId: string);
    procedure Valider;
    procedure Expedier;
    procedure Livrer;
    procedure Annuler;

    function CalculerTotal: Currency;
    function PeutEtreModifiee: Boolean;

    property Id: string read FId;
    property ClientId: string read FClientId;
    property Statut: TStatutCommande read FStatut;
    property DateCreation: TDateTime read FDateCreation;
  end;

implementation

constructor TLigneCommande.Create(const AProduitId: string; AQuantite: Integer;
  APrixUnitaire: Currency);
begin
  inherited Create;
  FProduitId := AProduitId;
  FQuantite := AQuantite;
  FPrixUnitaire := APrixUnitaire;
end;

function TLigneCommande.CalculerSousTotal: Currency;  
begin
  Result := FQuantite * FPrixUnitaire;
end;

constructor TCommande.Create(const AClientId: string);  
begin
  inherited Create;
  FId := TGuid.NewGuid.ToString;
  FClientId := AClientId;
  FLignes := TObjectList<TLigneCommande>.Create(True);
  FStatut := scBrouillon;
  FDateCreation := Now;
end;

destructor TCommande.Destroy;  
begin
  FLignes.Free;
  inherited;
end;

procedure TCommande.ValiderReglesMetier;  
begin
  // Règle : une commande doit avoir au moins une ligne
  if FLignes.Count = 0 then
    raise Exception.Create('Une commande doit contenir au moins une ligne');

  // Règle : le total doit être positif
  if CalculerTotal <= 0 then
    raise Exception.Create('Le total de la commande doit être positif');
end;

procedure TCommande.AjouterLigne(const AProduitId: string; AQuantite: Integer;
  APrixUnitaire: Currency);
var
  Ligne: TLigneCommande;
begin
  if not PeutEtreModifiee then
    raise Exception.Create('Cette commande ne peut plus être modifiée');

  if AQuantite <= 0 then
    raise Exception.Create('La quantité doit être positive');

  Ligne := TLigneCommande.Create(AProduitId, AQuantite, APrixUnitaire);
  FLignes.Add(Ligne);
end;

procedure TCommande.SupprimerLigne(const AProduitId: string);  
var
  i: Integer;
begin
  if not PeutEtreModifiee then
    raise Exception.Create('Cette commande ne peut plus être modifiée');

  for i := FLignes.Count - 1 downto 0 do
  begin
    if FLignes[i].ProduitId = AProduitId then
    begin
      FLignes.Delete(i);
      Exit;
    end;
  end;
end;

procedure TCommande.Valider;  
begin
  if FStatut <> scBrouillon then
    raise Exception.Create('Seule une commande en brouillon peut être validée');

  ValiderReglesMetier;
  FStatut := scValidee;
end;

procedure TCommande.Expedier;  
begin
  if FStatut <> scValidee then
    raise Exception.Create('Seule une commande validée peut être expédiée');

  FStatut := scExpediee;
end;

procedure TCommande.Livrer;  
begin
  if FStatut <> scExpediee then
    raise Exception.Create('Seule une commande expédiée peut être livrée');

  FStatut := scLivree;
end;

procedure TCommande.Annuler;  
begin
  if FStatut in [scExpediee, scLivree] then
    raise Exception.Create('Une commande expédiée ou livrée ne peut être annulée');

  FStatut := scAnnulee;
end;

function TCommande.CalculerTotal: Currency;  
var
  Ligne: TLigneCommande;
begin
  Result := 0;
  for Ligne in FLignes do
    Result := Result + Ligne.CalculerSousTotal;
end;

function TCommande.PeutEtreModifiee: Boolean;  
begin
  Result := FStatut = scBrouillon;
end;

end.
```

### 2. Les Ports (Interfaces)

Les **ports** sont des interfaces qui définissent comment le domaine communique avec l'extérieur.

**Deux types de ports :**

**a) Ports primaires (Driving Ports)** : Ce que l'application offre au monde extérieur
```
UI/API → Port Primaire → Domaine
```

**b) Ports secondaires (Driven Ports)** : Ce dont le domaine a besoin de l'extérieur
```
Domaine → Port Secondaire → DB/Service
```

```pascal
unit Domain.Ports;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections,
  Domain.Entities.Commande;

type
  // ========================================
  // PORTS PRIMAIRES (entrées dans le domaine)
  // ========================================

  // Service d'application : cas d'usage
  IServiceCommande = interface
    ['{A1B2C3D4-E5F6-7890-ABCD-EF1234567890}']
    function CreerCommande(const AClientId: string): string;
    procedure AjouterProduit(const ACommandeId, AProduitId: string;
                            AQuantite: Integer);
    procedure ValiderCommande(const ACommandeId: string);
    function ObtenirCommande(const ACommandeId: string): TCommande;
  end;

  // ========================================
  // PORTS SECONDAIRES (sorties du domaine)
  // ========================================

  // Port : Persistance des commandes
  IRepositoryCommande = interface
    ['{B2C3D4E5-F6A7-8901-BCDE-F12345678901}']
    procedure Sauvegarder(const ACommande: TCommande);
    function ChargerParId(const AId: string): TCommande;
    function Existe(const AId: string): Boolean;
    procedure Supprimer(const AId: string);
  end;

  // Port : Obtenir les prix des produits
  IServicePrix = interface
    ['{C3D4E5F6-A7B8-9012-CDEF-123456789012}']
    function ObtenirPrix(const AProduitId: string): Currency;
  end;

  // Port : Notifications
  IServiceNotification = interface
    ['{D4E5F6A7-B8C9-0123-DEF1-234567890123}']
    procedure EnvoyerConfirmationCommande(const ACommande: TCommande);
    procedure EnvoyerNotificationExpedition(const ACommande: TCommande);
  end;

implementation

end.
```

### 3. Les Adaptateurs (Implémentations)

Les **adaptateurs** sont les implémentations concrètes des ports. Ils font le lien entre le domaine et le monde extérieur.

#### a) Adaptateur primaire : Interface Web

```pascal
unit Adapters.Primary.WebAPI;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpapp, httpdefs, httproute, fpjson,
  Domain.Ports;

type
  TCommandeWebAPI = class
  private
    FService: IServiceCommande;

    procedure RouteCreerCommande(ARequest: TRequest; AResponse: TResponse);
    procedure RouteAjouterProduit(ARequest: TRequest; AResponse: TResponse);
    procedure RouteValider(ARequest: TRequest; AResponse: TResponse);
    procedure RouteObtenirCommande(ARequest: TRequest; AResponse: TResponse);
  public
    constructor Create(const AService: IServiceCommande);

    procedure EnregistrerRoutes;
  end;

implementation

uses
  jsonparser;

constructor TCommandeWebAPI.Create(const AService: IServiceCommande);  
begin
  inherited Create;
  FService := AService;
end;

procedure TCommandeWebAPI.EnregistrerRoutes;  
begin
  HTTPRouter.RegisterRoute('/api/commandes', rmPost, @RouteCreerCommande);
  HTTPRouter.RegisterRoute('/api/commandes/:id/produits', rmPost, @RouteAjouterProduit);
  HTTPRouter.RegisterRoute('/api/commandes/:id/valider', rmPost, @RouteValider);
  HTTPRouter.RegisterRoute('/api/commandes/:id', rmGet, @RouteObtenirCommande);
end;

procedure TCommandeWebAPI.RouteCreerCommande(ARequest: TRequest; AResponse: TResponse);  
var
  JSON: TJSONObject;
  Parser: TJSONParser;
  ClientId: string;
  CommandeId: string;
  Result: TJSONObject;
begin
  try
    // Parser le JSON de la requête
    Parser := TJSONParser.Create(ARequest.Content, [joUTF8]);
    try
      JSON := Parser.Parse as TJSONObject;
      try
        ClientId := JSON.Get('client_id', '');

        if ClientId = '' then
        begin
          AResponse.Code := 400;
          AResponse.Content := '{"error": "client_id requis"}';
          Exit;
        end;

        // Appeler le service du domaine
        CommandeId := FService.CreerCommande(ClientId);

        // Réponse
        Result := TJSONObject.Create;
        try
          Result.Add('commande_id', CommandeId);
          Result.Add('message', 'Commande créée avec succès');

          AResponse.Code := 201;
          AResponse.ContentType := 'application/json';
          AResponse.Content := Result.AsJSON;
        finally
          Result.Free;
        end;
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

procedure TCommandeWebAPI.RouteAjouterProduit(ARequest: TRequest; AResponse: TResponse);  
var
  CommandeId: string;
  JSON: TJSONObject;
  Parser: TJSONParser;
  ProduitId: string;
  Quantite: Integer;
begin
  try
    CommandeId := ARequest.RouteParams['id'];

    Parser := TJSONParser.Create(ARequest.Content, [joUTF8]);
    try
      JSON := Parser.Parse as TJSONObject;
      try
        ProduitId := JSON.Get('produit_id', '');
        Quantite := JSON.Get('quantite', 0);

        // Appeler le service du domaine
        FService.AjouterProduit(CommandeId, ProduitId, Quantite);

        AResponse.Code := 200;
        AResponse.Content := '{"message": "Produit ajouté"}';
      finally
        JSON.Free;
      end;
    finally
      Parser.Free;
    end;
  except
    on E: Exception do
    begin
      AResponse.Code := 400;
      AResponse.Content := Format('{"error": "%s"}', [E.Message]);
    end;
  end;
end;

procedure TCommandeWebAPI.RouteValider(ARequest: TRequest; AResponse: TResponse);  
var
  CommandeId: string;
begin
  try
    CommandeId := ARequest.RouteParams['id'];

    // Appeler le service du domaine
    FService.ValiderCommande(CommandeId);

    AResponse.Code := 200;
    AResponse.Content := '{"message": "Commande validée"}';
  except
    on E: Exception do
    begin
      AResponse.Code := 400;
      AResponse.Content := Format('{"error": "%s"}', [E.Message]);
    end;
  end;
end;

procedure TCommandeWebAPI.RouteObtenirCommande(ARequest: TRequest; AResponse: TResponse);  
var
  CommandeId: string;
  Commande: TCommande;
  JSON: TJSONObject;
begin
  try
    CommandeId := ARequest.RouteParams['id'];

    // Appeler le service du domaine
    Commande := FService.ObtenirCommande(CommandeId);
    try
      // Convertir en JSON
      JSON := TJSONObject.Create;
      try
        JSON.Add('id', Commande.Id);
        JSON.Add('client_id', Commande.ClientId);
        JSON.Add('statut', Ord(Commande.Statut));
        JSON.Add('total', Commande.CalculerTotal);

        AResponse.Code := 200;
        AResponse.ContentType := 'application/json';
        AResponse.Content := JSON.AsJSON;
      finally
        JSON.Free;
      end;
    finally
      Commande.Free;
    end;
  except
    on E: Exception do
    begin
      AResponse.Code := 404;
      AResponse.Content := Format('{"error": "%s"}', [E.Message]);
    end;
  end;
end;

end.
```

#### b) Adaptateur primaire : Interface Console

```pascal
unit Adapters.Primary.Console;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Domain.Ports;

type
  TCommandeConsole = class
  private
    FService: IServiceCommande;

    procedure AfficherMenu;
    procedure TraiterChoix(AChoix: Integer);
    procedure CreerNouvelleCommande;
    procedure AjouterProduitCommande;
    procedure ValiderCommande;
  public
    constructor Create(const AService: IServiceCommande);

    procedure Executer;
  end;

implementation

constructor TCommandeConsole.Create(const AService: IServiceCommande);  
begin
  inherited Create;
  FService := AService;
end;

procedure TCommandeConsole.AfficherMenu;  
begin
  WriteLn;
  WriteLn('=== GESTION DES COMMANDES ===');
  WriteLn('1. Créer une nouvelle commande');
  WriteLn('2. Ajouter un produit à une commande');
  WriteLn('3. Valider une commande');
  WriteLn('4. Quitter');
  WriteLn;
  Write('Votre choix: ');
end;

procedure TCommandeConsole.TraiterChoix(AChoix: Integer);  
begin
  case AChoix of
    1: CreerNouvelleCommande;
    2: AjouterProduitCommande;
    3: ValiderCommande;
  end;
end;

procedure TCommandeConsole.CreerNouvelleCommande;  
var
  ClientId: string;
  CommandeId: string;
begin
  WriteLn;
  WriteLn('--- Nouvelle commande ---');
  Write('ID du client: ');
  ReadLn(ClientId);

  try
    CommandeId := FService.CreerCommande(ClientId);
    WriteLn('✓ Commande créée: ', CommandeId);
  except
    on E: Exception do
      WriteLn('✗ Erreur: ', E.Message);
  end;
end;

procedure TCommandeConsole.AjouterProduitCommande;  
var
  CommandeId, ProduitId: string;
  Quantite: Integer;
begin
  WriteLn;
  WriteLn('--- Ajouter un produit ---');
  Write('ID de la commande: ');
  ReadLn(CommandeId);
  Write('ID du produit: ');
  ReadLn(ProduitId);
  Write('Quantité: ');
  ReadLn(Quantite);

  try
    FService.AjouterProduit(CommandeId, ProduitId, Quantite);
    WriteLn('✓ Produit ajouté');
  except
    on E: Exception do
      WriteLn('✗ Erreur: ', E.Message);
  end;
end;

procedure TCommandeConsole.ValiderCommande;  
var
  CommandeId: string;
begin
  WriteLn;
  WriteLn('--- Valider une commande ---');
  Write('ID de la commande: ');
  ReadLn(CommandeId);

  try
    FService.ValiderCommande(CommandeId);
    WriteLn('✓ Commande validée');
  except
    on E: Exception do
      WriteLn('✗ Erreur: ', E.Message);
  end;
end;

procedure TCommandeConsole.Executer;  
var
  Choix: Integer;
  Continuer: Boolean;
begin
  Continuer := True;

  while Continuer do
  begin
    AfficherMenu;
    ReadLn(Choix);

    if Choix = 4 then
      Continuer := False
    else
      TraiterChoix(Choix);
  end;

  WriteLn('Au revoir !');
end;

end.
```

#### c) Adaptateur secondaire : Persistance SQL

```pascal
unit Adapters.Secondary.SQLRepository;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SQLDB,
  Domain.Ports, Domain.Entities.Commande;

type
  TCommandeRepositorySQL = class(TInterfacedObject, IRepositoryCommande)
  private
    FConnection: TSQLConnection;

    function MapperVersCommande(AQuery: TSQLQuery): TCommande;
    procedure MapperVersBase(const ACommande: TCommande; AQuery: TSQLQuery);
  public
    constructor Create(AConnection: TSQLConnection);

    procedure Sauvegarder(const ACommande: TCommande);
    function ChargerParId(const AId: string): TCommande;
    function Existe(const AId: string): Boolean;
    procedure Supprimer(const AId: string);
  end;

implementation

constructor TCommandeRepositorySQL.Create(AConnection: TSQLConnection);  
begin
  inherited Create;
  FConnection := AConnection;
end;

procedure TCommandeRepositorySQL.Sauvegarder(const ACommande: TCommande);  
var
  Query: TSQLQuery;
begin
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FConnection;

    if Existe(ACommande.Id) then
    begin
      // UPDATE
      Query.SQL.Text :=
        'UPDATE commandes SET ' +
        '  client_id = :client_id, ' +
        '  statut = :statut, ' +
        '  date_creation = :date_creation ' +
        'WHERE id = :id';
    end
    else
    begin
      // INSERT
      Query.SQL.Text :=
        'INSERT INTO commandes (id, client_id, statut, date_creation) ' +
        'VALUES (:id, :client_id, :statut, :date_creation)';
    end;

    MapperVersBase(ACommande, Query);
    Query.ExecSQL;

    // Sauvegarder les lignes (simplifié ici)
    // Dans un cas réel, il faudrait gérer les lignes également

  finally
    Query.Free;
  end;
end;

function TCommandeRepositorySQL.ChargerParId(const AId: string): TCommande;  
var
  Query: TSQLQuery;
begin
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FConnection;
    Query.SQL.Text := 'SELECT * FROM commandes WHERE id = :id';
    Query.ParamByName('id').AsString := AId;
    Query.Open;

    if Query.EOF then
      raise Exception.CreateFmt('Commande %s non trouvée', [AId]);

    Result := MapperVersCommande(Query);
  finally
    Query.Free;
  end;
end;

function TCommandeRepositorySQL.Existe(const AId: string): Boolean;  
var
  Query: TSQLQuery;
begin
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FConnection;
    Query.SQL.Text := 'SELECT COUNT(*) as cnt FROM commandes WHERE id = :id';
    Query.ParamByName('id').AsString := AId;
    Query.Open;

    Result := Query.FieldByName('cnt').AsInteger > 0;
  finally
    Query.Free;
  end;
end;

procedure TCommandeRepositorySQL.Supprimer(const AId: string);  
var
  Query: TSQLQuery;
begin
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FConnection;
    Query.SQL.Text := 'DELETE FROM commandes WHERE id = :id';
    Query.ParamByName('id').AsString := AId;
    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

function TCommandeRepositorySQL.MapperVersCommande(AQuery: TSQLQuery): TCommande;  
var
  ClientId: string;
begin
  ClientId := AQuery.FieldByName('client_id').AsString;
  Result := TCommande.Create(ClientId);

  // Mapper les autres champs
  // (Simpllifié ici)
end;

procedure TCommandeRepositorySQL.MapperVersBase(const ACommande: TCommande;
  AQuery: TSQLQuery);
begin
  AQuery.ParamByName('id').AsString := ACommande.Id;
  AQuery.ParamByName('client_id').AsString := ACommande.ClientId;
  AQuery.ParamByName('statut').AsInteger := Ord(ACommande.Statut);
  AQuery.ParamByName('date_creation').AsDateTime := ACommande.DateCreation;
end;

end.
```

#### d) Adaptateur secondaire : Persistance en mémoire (pour tests)

```pascal
unit Adapters.Secondary.MemoryRepository;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections,
  Domain.Ports, Domain.Entities.Commande;

type
  TCommandeRepositoryMemoire = class(TInterfacedObject, IRepositoryCommande)
  private
    FCommandes: TObjectDictionary<string, TCommande>;

    function ClonerCommande(const ACommande: TCommande): TCommande;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Sauvegarder(const ACommande: TCommande);
    function ChargerParId(const AId: string): TCommande;
    function Existe(const AId: string): Boolean;
    procedure Supprimer(const AId: string);
  end;

implementation

constructor TCommandeRepositoryMemoire.Create;  
begin
  inherited Create;
  FCommandes := TObjectDictionary<string, TCommande>.Create([doOwnsValues]);
end;

destructor TCommandeRepositoryMemoire.Destroy;  
begin
  FCommandes.Free;
  inherited;
end;

function TCommandeRepositoryMemoire.ClonerCommande(const ACommande: TCommande): TCommande;  
begin
  // Cloner la commande pour éviter les modifications externes
  // (Simplification - dans un cas réel, faire un vrai clone)
  Result := TCommande.Create(ACommande.ClientId);
end;

procedure TCommandeRepositoryMemoire.Sauvegarder(const ACommande: TCommande);  
var
  Clone: TCommande;
begin
  Clone := ClonerCommande(ACommande);
  FCommandes.AddOrSetValue(ACommande.Id, Clone);
  WriteLn(Format('[Repository] Commande %s sauvegardée en mémoire', [ACommande.Id]));
end;

function TCommandeRepositoryMemoire.ChargerParId(const AId: string): TCommande;  
var
  CommandeStock: TCommande;
begin
  if not FCommandes.TryGetValue(AId, CommandeStock) then
    raise Exception.CreateFmt('Commande %s non trouvée', [AId]);

  Result := ClonerCommande(CommandeStock);
  WriteLn(Format('[Repository] Commande %s chargée depuis mémoire', [AId]));
end;

function TCommandeRepositoryMemoire.Existe(const AId: string): Boolean;  
begin
  Result := FCommandes.ContainsKey(AId);
end;

procedure TCommandeRepositoryMemoire.Supprimer(const AId: string);  
begin
  FCommandes.Remove(AId);
  WriteLn(Format('[Repository] Commande %s supprimée', [AId]));
end;

end.
```

## Service d'application (Use Case)

Le **service d'application** orchestre les opérations métier. Il fait partie du domaine mais coordonne les interactions.

```pascal
unit Application.Services.CommandeService;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Domain.Ports, Domain.Entities.Commande;

type
  TServiceCommande = class(TInterfacedObject, IServiceCommande)
  private
    FRepository: IRepositoryCommande;
    FServicePrix: IServicePrix;
    FServiceNotification: IServiceNotification;
  public
    constructor Create(const ARepository: IRepositoryCommande;
                      const AServicePrix: IServicePrix;
                      const AServiceNotification: IServiceNotification);

    function CreerCommande(const AClientId: string): string;
    procedure AjouterProduit(const ACommandeId, AProduitId: string;
                            AQuantite: Integer);
    procedure ValiderCommande(const ACommandeId: string);
    function ObtenirCommande(const ACommandeId: string): TCommande;
  end;

implementation

constructor TServiceCommande.Create(const ARepository: IRepositoryCommande;
  const AServicePrix: IServicePrix;
  const AServiceNotification: IServiceNotification);
begin
  inherited Create;
  FRepository := ARepository;
  FServicePrix := AServicePrix;
  FServiceNotification := AServiceNotification;
end;

function TServiceCommande.CreerCommande(const AClientId: string): string;  
var
  Commande: TCommande;
begin
  WriteLn(Format('[Service] Création commande pour client %s', [AClientId]));

  // Créer l'entité de domaine
  Commande := TCommande.Create(AClientId);
  try
    // Sauvegarder via le port
    FRepository.Sauvegarder(Commande);

    Result := Commande.Id;
    WriteLn(Format('[Service] Commande %s créée', [Result]));
  finally
    Commande.Free;
  end;
end;

procedure TServiceCommande.AjouterProduit(const ACommandeId, AProduitId: string;
  AQuantite: Integer);
var
  Commande: TCommande;
  Prix: Currency;
begin
  WriteLn(Format('[Service] Ajout produit %s (x%d) à commande %s',
    [AProduitId, AQuantite, ACommandeId]));

  // Charger la commande
  Commande := FRepository.ChargerParId(ACommandeId);
  try
    // Obtenir le prix via le port
    Prix := FServicePrix.ObtenirPrix(AProduitId);

    // Opération métier
    Commande.AjouterLigne(AProduitId, AQuantite, Prix);

    // Sauvegarder
    FRepository.Sauvegarder(Commande);

    WriteLn('[Service] Produit ajouté avec succès');
  finally
    Commande.Free;
  end;
end;

procedure TServiceCommande.ValiderCommande(const ACommandeId: string);  
var
  Commande: TCommande;
begin
  WriteLn(Format('[Service] Validation commande %s', [ACommandeId]));

  // Charger la commande
  Commande := FRepository.ChargerParId(ACommandeId);
  try
    // Opération métier
    Commande.Valider;

    // Sauvegarder
    FRepository.Sauvegarder(Commande);

    // Notifier via le port
    FServiceNotification.EnvoyerConfirmationCommande(Commande);

    WriteLn('[Service] Commande validée avec succès');
  finally
    Commande.Free;
  end;
end;

function TServiceCommande.ObtenirCommande(const ACommandeId: string): TCommande;  
begin
  WriteLn(Format('[Service] Récupération commande %s', [ACommandeId]));
  Result := FRepository.ChargerParId(ACommandeId);
end;

end.
```

## Adaptateurs secondaires supplémentaires

### Service de prix (mock pour démo)

```pascal
unit Adapters.Secondary.MockPrixService;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Domain.Ports;

type
  TMockServicePrix = class(TInterfacedObject, IServicePrix)
  private
    FPrix: TStringList;
  public
    constructor Create;
    destructor Destroy; override;

    function ObtenirPrix(const AProduitId: string): Currency;

    // Méthode helper pour configurer les prix (pour les tests)
    procedure DefinirPrix(const AProduitId: string; APrix: Currency);
  end;

implementation

constructor TMockServicePrix.Create;  
begin
  inherited Create;
  FPrix := TStringList.Create;
  FPrix.Sorted := True;

  // Prix par défaut
  DefinirPrix('prod-1', 10.00);
  DefinirPrix('prod-2', 25.50);
  DefinirPrix('prod-3', 99.99);
end;

destructor TMockServicePrix.Destroy;  
begin
  FPrix.Free;
  inherited;
end;

function TMockServicePrix.ObtenirPrix(const AProduitId: string): Currency;  
var
  Index: Integer;
begin
  Index := FPrix.IndexOf(AProduitId);

  if Index >= 0 then
  begin
    Result := StrToCurr(FPrix.ValueFromIndex[Index]);
    WriteLn(Format('[ServicePrix] Prix de %s: %.2f €', [AProduitId, Result]));
  end
  else
  begin
    WriteLn(Format('[ServicePrix] Produit %s non trouvé, prix par défaut', [AProduitId]));
    Result := 0.0;
  end;
end;

procedure TMockServicePrix.DefinirPrix(const AProduitId: string; APrix: Currency);  
begin
  FPrix.Values[AProduitId] := CurrToStr(APrix);
end;

end.
```

### Service de notification (mock pour démo)

```pascal
unit Adapters.Secondary.MockNotificationService;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Domain.Ports, Domain.Entities.Commande;

type
  TMockServiceNotification = class(TInterfacedObject, IServiceNotification)
  public
    procedure EnvoyerConfirmationCommande(const ACommande: TCommande);
    procedure EnvoyerNotificationExpedition(const ACommande: TCommande);
  end;

implementation

procedure TMockServiceNotification.EnvoyerConfirmationCommande(const ACommande: TCommande);  
begin
  WriteLn('═══════════════════════════════════════');
  WriteLn('    CONFIRMATION DE COMMANDE           ');
  WriteLn('═══════════════════════════════════════');
  WriteLn('Commande N°: ', ACommande.Id);
  WriteLn('Client: ', ACommande.ClientId);
  WriteLn('Total: ', ACommande.CalculerTotal:0:2, ' €');
  WriteLn('Votre commande a été validée !');
  WriteLn('═══════════════════════════════════════');
end;

procedure TMockServiceNotification.EnvoyerNotificationExpedition(const ACommande: TCommande);  
begin
  WriteLn('═══════════════════════════════════════');
  WriteLn('    NOTIFICATION D''EXPÉDITION         ');
  WriteLn('═══════════════════════════════════════');
  WriteLn('Commande N°: ', ACommande.Id);
  WriteLn('Votre commande a été expédiée !');
  WriteLn('═══════════════════════════════════════');
end;

end.
```

## Configuration et Bootstrap

Le **bootstrap** assemble tous les composants (câblage des dépendances).

```pascal
unit Bootstrap;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Domain.Ports,
  Application.Services.CommandeService,
  Adapters.Secondary.MemoryRepository,
  Adapters.Secondary.MockPrixService,
  Adapters.Secondary.MockNotificationService;

type
  TApplicationContainer = class
  private
    // Adaptateurs secondaires
    FRepository: IRepositoryCommande;
    FServicePrix: IServicePrix;
    FServiceNotification: IServiceNotification;

    // Services d'application
    FServiceCommande: IServiceCommande;
  public
    constructor Create;

    // Accesseurs pour les adaptateurs primaires
    property ServiceCommande: IServiceCommande read FServiceCommande;
  end;

implementation

constructor TApplicationContainer.Create;  
begin
  inherited Create;

  WriteLn('=== Bootstrap de l''application ===');
  WriteLn;

  // Instancier les adaptateurs secondaires
  WriteLn('Initialisation des adaptateurs secondaires...');
  FRepository := TCommandeRepositoryMemoire.Create;
  FServicePrix := TMockServicePrix.Create;
  FServiceNotification := TMockServiceNotification.Create;

  // Instancier les services d'application (avec injection de dépendances)
  WriteLn('Initialisation des services d''application...');
  FServiceCommande := TServiceCommande.Create(
    FRepository,
    FServicePrix,
    FServiceNotification
  );

  WriteLn('Bootstrap terminé');
  WriteLn;
end;

end.
```

## Programme principal - Démonstration complète

```pascal
program HexagonalDemo;

{$mode objfpc}{$H+}

uses
  SysUtils,
  Bootstrap,
  Adapters.Primary.Console;

var
  Container: TApplicationContainer;
  Console: TCommandeConsole;
begin
  WriteLn('═══════════════════════════════════════════════════');
  WriteLn('   Démonstration Architecture Hexagonale          ');
  WriteLn('═══════════════════════════════════════════════════');
  WriteLn;

  try
    // Bootstrap : créer et câbler tous les composants
    Container := TApplicationContainer.Create;
    try
      // Choisir l'adaptateur primaire (ici: Console)
      // On pourrait tout aussi bien utiliser l'adaptateur Web
      Console := TCommandeConsole.Create(Container.ServiceCommande);
      try
        Console.Executer;
      finally
        Console.Free;
      end;
    finally
      Container.Free;
    end;

  except
    on E: Exception do
      WriteLn('ERREUR FATALE: ', E.Message);
  end;

  WriteLn;
  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
```

## Tests unitaires avec l'architecture hexagonale

L'architecture hexagonale facilite énormément les tests car le domaine est isolé.

```pascal
unit Tests.Domain.Commande;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  Domain.Entities.Commande;

type
  TTestCommande = class(TTestCase)
  published
    procedure TestCreationCommande;
    procedure TestAjouterLigne;
    procedure TestAjouterLigneQuantiteNegative;
    procedure TestValiderCommandeVide;
    procedure TestValiderCommandeOK;
    procedure TestModifierCommandeValidee;
    procedure TestCalculerTotal;
  end;

implementation

procedure TTestCommande.TestCreationCommande;  
var
  Commande: TCommande;
begin
  Commande := TCommande.Create('client-123');
  try
    AssertNotNull('La commande doit être créée', Commande);
    AssertEquals('Client correct', 'client-123', Commande.ClientId);
    AssertEquals('Statut initial', Ord(scBrouillon), Ord(Commande.Statut));
  finally
    Commande.Free;
  end;
end;

procedure TTestCommande.TestAjouterLigne;  
var
  Commande: TCommande;
begin
  Commande := TCommande.Create('client-123');
  try
    Commande.AjouterLigne('prod-1', 2, 10.0);

    AssertEquals('Total correct', 20.0, Commande.CalculerTotal, 0.01);
  finally
    Commande.Free;
  end;
end;

procedure TTestCommande.TestAjouterLigneQuantiteNegative;  
var
  Commande: TCommande;
begin
  Commande := TCommande.Create('client-123');
  try
    // Doit lever une exception
    AssertException('Quantité négative interdite',
      Exception,
      procedure
      begin
        Commande.AjouterLigne('prod-1', -1, 10.0);
      end
    );
  finally
    Commande.Free;
  end;
end;

procedure TTestCommande.TestValiderCommandeVide;  
var
  Commande: TCommande;
begin
  Commande := TCommande.Create('client-123');
  try
    // Valider une commande vide doit échouer
    AssertException('Commande vide non validable',
      Exception,
      procedure
      begin
        Commande.Valider;
      end
    );
  finally
    Commande.Free;
  end;
end;

procedure TTestCommande.TestValiderCommandeOK;  
var
  Commande: TCommande;
begin
  Commande := TCommande.Create('client-123');
  try
    Commande.AjouterLigne('prod-1', 2, 10.0);
    Commande.Valider;

    AssertEquals('Statut validé', Ord(scValidee), Ord(Commande.Statut));
  finally
    Commande.Free;
  end;
end;

procedure TTestCommande.TestModifierCommandeValidee;  
var
  Commande: TCommande;
begin
  Commande := TCommande.Create('client-123');
  try
    Commande.AjouterLigne('prod-1', 2, 10.0);
    Commande.Valider;

    // Tenter d'ajouter une ligne après validation
    AssertException('Modification impossible après validation',
      Exception,
      procedure
      begin
        Commande.AjouterLigne('prod-2', 1, 5.0);
      end
    );
  finally
    Commande.Free;
  end;
end;

procedure TTestCommande.TestCalculerTotal;  
var
  Commande: TCommande;
begin
  Commande := TCommande.Create('client-123');
  try
    Commande.AjouterLigne('prod-1', 2, 10.0);  // 20.0
    Commande.AjouterLigne('prod-2', 3, 5.0);   // 15.0

    AssertEquals('Total correct', 35.0, Commande.CalculerTotal, 0.01);
  finally
    Commande.Free;
  end;
end;

initialization
  RegisterTest(TTestCommande);

end.
```

### Tests du service avec mocks

```pascal
unit Tests.Application.ServiceCommande;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  Domain.Ports,
  Application.Services.CommandeService,
  Adapters.Secondary.MemoryRepository,
  Adapters.Secondary.MockPrixService,
  Adapters.Secondary.MockNotificationService;

type
  TTestServiceCommande = class(TTestCase)
  private
    FService: IServiceCommande;
    FRepository: IRepositoryCommande;
    FServicePrix: IServicePrix;
    FServiceNotification: IServiceNotification;
  protected
    procedure SetUp; override;
  published
    procedure TestCreerCommande;
    procedure TestAjouterProduit;
    procedure TestValiderCommande;
  end;

implementation

procedure TTestServiceCommande.SetUp;  
begin
  // Créer les mocks
  FRepository := TCommandeRepositoryMemoire.Create;
  FServicePrix := TMockServicePrix.Create;
  FServiceNotification := TMockServiceNotification.Create;

  // Créer le service
  FService := TServiceCommande.Create(
    FRepository,
    FServicePrix,
    FServiceNotification
  );
end;

procedure TTestServiceCommande.TestCreerCommande;  
var
  CommandeId: string;
begin
  CommandeId := FService.CreerCommande('client-123');

  AssertNotEquals('ID non vide', '', CommandeId);
  AssertTrue('Commande existe', FRepository.Existe(CommandeId));
end;

procedure TTestServiceCommande.TestAjouterProduit;  
var
  CommandeId: string;
  Commande: TCommande;
begin
  // Créer une commande
  CommandeId := FService.CreerCommande('client-123');

  // Ajouter un produit
  FService.AjouterProduit(CommandeId, 'prod-1', 2);

  // Vérifier
  Commande := FRepository.ChargerParId(CommandeId);
  try
    AssertTrue('Total > 0', Commande.CalculerTotal > 0);
  finally
    Commande.Free;
  end;
end;

procedure TTestServiceCommande.TestValiderCommande;  
var
  CommandeId: string;
  Commande: TCommande;
begin
  // Créer une commande et ajouter un produit
  CommandeId := FService.CreerCommande('client-123');
  FService.AjouterProduit(CommandeId, 'prod-1', 2);

  // Valider
  FService.ValiderCommande(CommandeId);

  // Vérifier
  Commande := FRepository.ChargerParId(CommandeId);
  try
    AssertEquals('Statut validé', Ord(scValidee), Ord(Commande.Statut));
  finally
    Commande.Free;
  end;
end;

initialization
  RegisterTest(TTestServiceCommande);

end.
```

## Organisation des fichiers

```
MonProjet/
├── Domain/                        ← Cœur de l'hexagone
│   ├── Entities/
│   │   ├── Commande.pas
│   │   ├── Client.pas
│   │   └── Produit.pas
│   ├── ValueObjects/
│   │   ├── Email.pas
│   │   └── Adresse.pas
│   └── Ports.pas                 ← Interfaces
│
├── Application/                   ← Services d'application
│   └── Services/
│       ├── CommandeService.pas
│       └── ClientService.pas
│
├── Adapters/                      ← Adaptateurs (couche externe)
│   ├── Primary/                   ← Driving adapters
│   │   ├── WebAPI.pas
│   │   ├── Console.pas
│   │   └── Desktop/
│   │       └── FormMain.pas
│   │
│   └── Secondary/                 ← Driven adapters
│       ├── SQLRepository.pas
│       ├── MemoryRepository.pas
│       ├── MockPrixService.pas
│       └── EmailService.pas
│
├── Tests/                         ← Tests unitaires
│   ├── Domain/
│   │   └── TestCommande.pas
│   └── Application/
│       └── TestServiceCommande.pas
│
├── Bootstrap.pas                  ← Configuration et DI
└── Main.pas                       ← Point d'entrée
```

## Avantages de l'architecture hexagonale

### 1. Testabilité maximale

```pascal
// Test du domaine sans aucune dépendance externe
procedure TestLogiquePure;  
var
  Commande: TCommande;
begin
  Commande := TCommande.Create('client-123');
  try
    Commande.AjouterLigne('prod-1', 2, 10.0);
    Assert(Commande.CalculerTotal = 20.0);
  finally
    Commande.Free;
  end;
end;
```

### 2. Changement d'infrastructure facile

```pascal
// Passer de la mémoire à SQL sans toucher au domaine
var
  Container: TApplicationContainer;
begin
  // Configuration DEV : mémoire
  Container.Repository := TCommandeRepositoryMemoire.Create;

  // Configuration PROD : SQL
  Container.Repository := TCommandeRepositorySQL.Create(Connection);

  // Le reste du code reste identique !
end;
```

### 3. Multiples interfaces utilisateur

```pascal
// Même domaine, plusieurs UI
begin
  // Web
  WebAPI := TCommandeWebAPI.Create(Container.ServiceCommande);

  // Console
  Console := TCommandeConsole.Create(Container.ServiceCommande);

  // Desktop
  FormMain := TFormMain.Create(Self, Container.ServiceCommande);
end;
```

### 4. Indépendance technologique

Le domaine ne sait pas :
- Quelle base de données est utilisée (SQL, NoSQL, fichiers)
- Quelle interface est utilisée (Web, Console, Desktop, Mobile)
- Quels frameworks sont utilisés
- Comment les notifications sont envoyées

### 5. Évolution progressive

```pascal
// Commencer simple
Repository := TCommandeRepositoryMemoire.Create;

// Puis migrer vers du réel
Repository := TCommandeRepositorySQL.Create(Connection);

// Puis optimiser
Repository := TCommandeRepositoryCached.Create(
  TCommandeRepositorySQL.Create(Connection),
  Cache
);
```

## Variantes et patterns associés

### 1. Clean Architecture (Uncle Bob)

Clean Architecture est une évolution de l'architecture hexagonale avec des couches plus définies :

```
┌─────────────────────────────────────┐
│  Frameworks & Drivers (UI, DB)      │ ← Adaptateurs
├─────────────────────────────────────┤
│  Interface Adapters (Controllers)   │ ← Adaptateurs primaires
├─────────────────────────────────────┤
│  Use Cases (Application Services)   │ ← Services d'application
├─────────────────────────────────────┤
│  Entities (Domain)                  │ ← Domaine pur
└─────────────────────────────────────┘
```

**Règle de dépendance :** Les dépendances pointent toujours vers l'intérieur.

### 2. Onion Architecture

Similaire mais avec une emphase sur les couches concentriques :

```
        ┌──────────────────┐
        │  Infrastructure  │
     ┌──┴──────────────────┴──┐
     │   Application Services │
  ┌──┴────────────────────────┴───┐
  │      Domain Services          │
┌─┴───────────────────────────────┴─┐
│          Domain Model             │
└───────────────────────────────────┘
```

### 3. DDD + Hexagonal

Combiner DDD et architecture hexagonale :

```pascal
// Bounded Context = un hexagone
unit BoundedContext.Commandes;

// Agrégats dans le domaine
type
  TCommande = class(TAggregateRoot)
    // Logique métier
  end;

// Repository comme port secondaire
type
  IRepositoryCommande = interface
    // Port
  end;
```

## Bonnes pratiques

### 1. Le domaine ne doit jamais dépendre de l'infrastructure

```pascal
// ❌ MAUVAIS
unit Domain.Commande;  
uses
  SQLDB, // ← INTERDIT !
  fpjson; // ← INTERDIT !

// ✅ BON
unit Domain.Commande;
// Aucun uses externe, seulement Domain.*
```

### 2. Utiliser l'inversion de dépendances

```pascal
// Le domaine définit l'interface
type
  IRepository = interface
    procedure Sauvegarder(const AEntite: TEntite);
  end;

// L'infrastructure l'implémente
type
  TRepositorySQL = class(TInterfacedObject, IRepository)
    procedure Sauvegarder(const AEntite: TEntite);
  end;
```

### 3. Garder les adaptateurs simples

```pascal
// L'adaptateur traduit, il ne contient PAS de logique métier
procedure TWebAPI.RouteCreer(AReq: TRequest; AResp: TResponse);  
begin
  // ✅ Juste de la traduction
  ClientId := JSON.Get('client_id');
  CommandeId := FService.CreerCommande(ClientId);
  AResp.Content := Format('{"id": "%s"}', [CommandeId]);

  // ❌ PAS de logique métier ici !
  // if ClientId.StartsWith('VIP') then ...
end;
```

### 4. Un port = une responsabilité

```pascal
// ❌ Port trop large
type
  IRepository = interface
    procedure Sauvegarder(const A: TAny);
    procedure Envoyer Email(const A: TAny);
    procedure Logger(const A: TAny);
  end;

// ✅ Ports séparés
type
  IRepository = interface
    procedure Sauvegarder(const A: TEntite);
  end;

  IEmailService = interface
    procedure Envoyer(const AEmail: TEmail);
  end;

  ILogger = interface
    procedure Log(const AMessage: string);
  end;
```

### 5. Tester le domaine de manière isolée

```pascal
// Tests sans aucune dépendance externe
procedure TestDomaine;  
var
  Commande: TCommande;
begin
  // Pas de base de données
  // Pas de serveur web
  // Pas de fichiers
  // Juste le domaine pur

  Commande := TCommande.Create('client-123');
  Commande.AjouterLigne('prod-1', 2, 10.0);
  Assert(Commande.CalculerTotal = 20.0);
end;
```

## Quand utiliser l'architecture hexagonale ?

### ✅ Utilisez l'architecture hexagonale quand :

- **Logique métier complexe** : Beaucoup de règles métier à isoler
- **Long terme** : Projet qui va durer et évoluer
- **Multiples interfaces** : Web + Mobile + Desktop + API
- **Testabilité critique** : Tests automatisés essentiels
- **Équipe grande** : Permet de travailler en parallèle
- **Changements fréquents** : Infrastructure qui peut changer

### ❌ Évitez l'architecture hexagonale quand :

- **CRUD simple** : Application basique sans logique métier
- **Prototype rapide** : MVP à livrer rapidement
- **Petite équipe** : Overhead trop important
- **Projet court** : Ne sera pas maintenu longtemps
- **Une seule interface** : Pas besoin de flexibilité
- **Pas de tests** : L'isolation n'apporte rien

## Conclusion

L'**architecture hexagonale** est un pattern puissant pour créer des applications maintenables et évolutives en isolant complètement la logique métier.

### Points clés à retenir :

1. **Le domaine au centre** : Aucune dépendance externe
2. **Ports = interfaces** : Définissent les frontières
3. **Adaptateurs = implémentations** : Connectent au monde réel
4. **Testabilité maximale** : Le domaine se teste facilement
5. **Flexibilité** : Changer d'infrastructure sans toucher au métier

### Bénéfices avec FreePascal :

- **Interfaces natives** : FreePascal gère parfaitement les interfaces
- **Typage fort** : Garantit les contrats
- **Pas de framework magique** : Architecture explicite et claire
- **Performance** : Compilation native = rapidité
- **Portabilité** : Même code Windows/Linux

L'architecture hexagonale demande un **investissement initial** mais paye sur le long terme en facilitant l'évolution, les tests et la maintenance. Elle est particulièrement adaptée aux systèmes d'entreprise complexes où la logique métier est riche et critique. 🎯

⏭️ [Dependency Injection containers](/21-architecture-logicielle-avancee/05-dependency-injection-containers.md)
