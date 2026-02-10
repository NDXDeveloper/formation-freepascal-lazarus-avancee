🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 21.3 Event Sourcing et CQRS

## Introduction

**Event Sourcing** et **CQRS** (Command Query Responsibility Segregation) sont deux patterns architecturaux puissants qui transforment la manière dont nous concevons et stockons les données. Bien qu'ils soient souvent utilisés ensemble, ce sont des concepts distincts qui peuvent être appliqués indépendamment.

### Vue d'ensemble

**Event Sourcing** : Au lieu de stocker l'état actuel, on stocke tous les événements qui ont conduit à cet état.

**CQRS** : On sépare les opérations de lecture (Query) des opérations d'écriture (Command).

**Analogie bancaire :**
```
APPROCHE TRADITIONNELLE (État actuel uniquement)
┌─────────────────┐
│ Compte: 1500 €  │  ← On voit seulement le solde actuel
└─────────────────┘

EVENT SOURCING (Historique complet)
┌──────────────────────────────────┐
│ 01/01 - Ouverture: +1000 €       │
│ 05/01 - Dépôt: +500 €            │
│ 10/01 - Retrait: -200 €          │
│ 15/01 - Dépôt: +200 €            │
│ = Solde actuel: 1500 €           │
└──────────────────────────────────┘
```

### Pourquoi utiliser Event Sourcing et CQRS ?

**Avantages :**
- **Audit complet** : Traçabilité totale de tous les changements
- **Time travel** : Reconstruire l'état à n'importe quel moment
- **Débogage** : Comprendre comment on est arrivé à un bug
- **Performance** : Optimiser séparément lecture et écriture
- **Business Intelligence** : Analyser les comportements
- **Conformité** : Répondre aux exigences réglementaires

**Inconvénients :**
- **Complexité** : Plus difficile à comprendre et implémenter
- **Stockage** : Plus d'espace nécessaire
- **Cohérence éventuelle** : Délai entre écriture et lecture
- **Courbe d'apprentissage** : Nouveau paradigme à maîtriser

## Event Sourcing détaillé

### Concept fondamental

Au lieu de faire des UPDATE en base de données, on enregistre des **événements immuables** qui décrivent ce qui s'est passé.

```pascal
// ❌ Approche traditionnelle (CRUD)
UPDATE comptes SET solde = 1500 WHERE id = 'compte-123';

// ✅ Event Sourcing
INSERT INTO evenements (id, type, donnees, timestamp) VALUES
  ('evt-1', 'CompteOuvert', '{"solde_initial": 1000}', '2024-01-01'),
  ('evt-2', 'ArgentDepose', '{"montant": 500}', '2024-01-05'),
  ('evt-3', 'ArgentRetire', '{"montant": 200}', '2024-01-10');
```

### Les événements

Un **événement** représente un fait passé, quelque chose qui s'est déjà produit et qui ne peut pas être annulé.

**Caractéristiques d'un événement :**
- **Immuable** : Ne peut jamais être modifié ou supprimé
- **Passé** : Nommé au passé (CompteOuvert, non OuvrirCompte)
- **Factuel** : Décrit ce qui s'est réellement passé
- **Daté** : Contient un timestamp précis

```pascal
unit EventSourcing.Events;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, DateUtils;

type
  // Classe de base pour tous les événements
  TEvenementBase = class
  private
    FId: string;
    FAgregatId: string;
    FTimestamp: TDateTime;
    FVersion: Integer;
  public
    constructor Create(const AAgregatId: string; AVersion: Integer);

    // Convertir en JSON pour la persistence
    function VersJSON: TJSONObject; virtual; abstract;

    // Reconstruire depuis JSON
    class function DepuisJSON(AJSON: TJSONObject): TEvenementBase; virtual; abstract;

    property Id: string read FId;
    property AgregatId: string read FAgregatId;
    property Timestamp: TDateTime read FTimestamp;
    property Version: Integer read FVersion;
  end;

implementation

constructor TEvenementBase.Create(const AAgregatId: string; AVersion: Integer);
begin
  inherited Create;
  FId := TGuid.NewGuid.ToString;
  FAgregatId := AAgregatId;
  FTimestamp := Now;
  FVersion := AVersion;
end;

end.
```

### Événements de domaine concrets

```pascal
unit Domain.Events.Compte;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, EventSourcing.Events;

type
  // Événement : Compte ouvert
  TEvenementCompteOuvert = class(TEvenementBase)
  private
    FTitulaire: string;
    FSoldeInitial: Currency;
  public
    constructor Create(const ACompteId, ATitulaire: string;
                      ASoldeInitial: Currency; AVersion: Integer);

    function VersJSON: TJSONObject; override;
    class function DepuisJSON(AJSON: TJSONObject): TEvenementBase; override;

    property Titulaire: string read FTitulaire;
    property SoldeInitial: Currency read FSoldeInitial;
  end;

  // Événement : Argent déposé
  TEvenementArgentDepose = class(TEvenementBase)
  private
    FMontant: Currency;
    FDescription: string;
  public
    constructor Create(const ACompteId: string; AMontant: Currency;
                      const ADescription: string; AVersion: Integer);

    function VersJSON: TJSONObject; override;
    class function DepuisJSON(AJSON: TJSONObject): TEvenementBase; override;

    property Montant: Currency read FMontant;
    property Description: string read FDescription;
  end;

  // Événement : Argent retiré
  TEvenementArgentRetire = class(TEvenementBase)
  private
    FMontant: Currency;
    FDescription: string;
  public
    constructor Create(const ACompteId: string; AMontant: Currency;
                      const ADescription: string; AVersion: Integer);

    function VersJSON: TJSONObject; override;
    class function DepuisJSON(AJSON: TJSONObject): TEvenementBase; override;

    property Montant: Currency read FMontant;
    property Description: string read FDescription;
  end;

implementation

// TEvenementCompteOuvert

constructor TEvenementCompteOuvert.Create(const ACompteId, ATitulaire: string;
  ASoldeInitial: Currency; AVersion: Integer);
begin
  inherited Create(ACompteId, AVersion);
  FTitulaire := ATitulaire;
  FSoldeInitial := ASoldeInitial;
end;

function TEvenementCompteOuvert.VersJSON: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.Add('id', Id);
  Result.Add('agregat_id', AgregatId);
  Result.Add('type', 'CompteOuvert');
  Result.Add('timestamp', FormatDateTime('yyyy-mm-dd"T"hh:nn:ss.zzz', Timestamp));
  Result.Add('version', Version);
  Result.Add('titulaire', FTitulaire);
  Result.Add('solde_initial', FSoldeInitial);
end;

class function TEvenementCompteOuvert.DepuisJSON(AJSON: TJSONObject): TEvenementBase;
var
  Evt: TEvenementCompteOuvert;
begin
  Evt := TEvenementCompteOuvert.Create(
    AJSON.Get('agregat_id', ''),
    AJSON.Get('titulaire', ''),
    AJSON.Get('solde_initial', 0.0),
    AJSON.Get('version', 1)
  );
  Result := Evt;
end;

// TEvenementArgentDepose

constructor TEvenementArgentDepose.Create(const ACompteId: string;
  AMontant: Currency; const ADescription: string; AVersion: Integer);
begin
  inherited Create(ACompteId, AVersion);
  FMontant := AMontant;
  FDescription := ADescription;
end;

function TEvenementArgentDepose.VersJSON: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.Add('id', Id);
  Result.Add('agregat_id', AgregatId);
  Result.Add('type', 'ArgentDepose');
  Result.Add('timestamp', FormatDateTime('yyyy-mm-dd"T"hh:nn:ss.zzz', Timestamp));
  Result.Add('version', Version);
  Result.Add('montant', FMontant);
  Result.Add('description', FDescription);
end;

class function TEvenementArgentDepose.DepuisJSON(AJSON: TJSONObject): TEvenementBase;
begin
  Result := TEvenementArgentDepose.Create(
    AJSON.Get('agregat_id', ''),
    AJSON.Get('montant', 0.0),
    AJSON.Get('description', ''),
    AJSON.Get('version', 1)
  );
end;

// TEvenementArgentRetire

constructor TEvenementArgentRetire.Create(const ACompteId: string;
  AMontant: Currency; const ADescription: string; AVersion: Integer);
begin
  inherited Create(ACompteId, AVersion);
  FMontant := AMontant;
  FDescription := ADescription;
end;

function TEvenementArgentRetire.VersJSON: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.Add('id', Id);
  Result.Add('agregat_id', AgregatId);
  Result.Add('type', 'ArgentRetire');
  Result.Add('timestamp', FormatDateTime('yyyy-mm-dd"T"hh:nn:ss.zzz', Timestamp));
  Result.Add('version', Version);
  Result.Add('montant', FMontant);
  Result.Add('description', FDescription);
end;

class function TEvenementArgentRetire.DepuisJSON(AJSON: TJSONObject): TEvenementBase;
begin
  Result := TEvenementArgentRetire.Create(
    AJSON.Get('agregat_id', ''),
    AJSON.Get('montant', 0.0),
    AJSON.Get('description', ''),
    AJSON.Get('version', 1)
  );
end;

end.
```

### Event Store (Magasin d'événements)

L'**Event Store** est la base de données qui stocke tous les événements. C'est le cœur de l'Event Sourcing.

```pascal
unit EventSourcing.EventStore;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, EventSourcing.Events, fpjson;

type
  // Interface du Event Store
  IEventStore = interface
    ['{A1B2C3D4-E5F6-4789-A0B1-C2D3E4F5A6B7}']

    // Ajouter un ou plusieurs événements
    procedure AjouterEvenement(const AEvenement: TEvenementBase);
    procedure AjouterEvenements(const AEvenements: TArray<TEvenementBase>);

    // Charger tous les événements d'un agrégat
    function ChargerEvenements(const AAgregatId: string): TList<TEvenementBase>;

    // Charger les événements depuis une version
    function ChargerEvenementsDepuis(const AAgregatId: string;
                                     AVersionMin: Integer): TList<TEvenementBase>;

    // Obtenir la dernière version d'un agrégat
    function ObtenirDerniereVersion(const AAgregatId: string): Integer;
  end;

  // Implémentation en mémoire (pour tests et démo)
  TEventStoreMemoire = class(TInterfacedObject, IEventStore)
  private
    FEvenements: TObjectList<TEvenementBase>;
    FLock: TRTLCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AjouterEvenement(const AEvenement: TEvenementBase);
    procedure AjouterEvenements(const AEvenements: TArray<TEvenementBase>);
    function ChargerEvenements(const AAgregatId: string): TList<TEvenementBase>;
    function ChargerEvenementsDepuis(const AAgregatId: string;
                                    AVersionMin: Integer): TList<TEvenementBase>;
    function ObtenirDerniereVersion(const AAgregatId: string): Integer;
  end;

implementation

constructor TEventStoreMemoire.Create;
begin
  inherited Create;
  FEvenements := TObjectList<TEvenementBase>.Create(True);
  InitCriticalSection(FLock);
end;

destructor TEventStoreMemoire.Destroy;
begin
  DoneCriticalSection(FLock);
  FEvenements.Free;
  inherited;
end;

procedure TEventStoreMemoire.AjouterEvenement(const AEvenement: TEvenementBase);
begin
  EnterCriticalSection(FLock);
  try
    FEvenements.Add(AEvenement);
    WriteLn(Format('[EventStore] Événement ajouté: %s (Version: %d)',
      [AEvenement.ClassName, AEvenement.Version]));
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TEventStoreMemoire.AjouterEvenements(const AEvenements: TArray<TEvenementBase>);
var
  Evt: TEvenementBase;
begin
  for Evt in AEvenements do
    AjouterEvenement(Evt);
end;

function TEventStoreMemoire.ChargerEvenements(const AAgregatId: string): TList<TEvenementBase>;
var
  Evt: TEvenementBase;
begin
  Result := TList<TEvenementBase>.Create;

  EnterCriticalSection(FLock);
  try
    for Evt in FEvenements do
    begin
      if Evt.AgregatId = AAgregatId then
        Result.Add(Evt);
    end;
  finally
    LeaveCriticalSection(FLock);
  end;

  WriteLn(Format('[EventStore] Chargé %d événements pour %s',
    [Result.Count, AAgregatId]));
end;

function TEventStoreMemoire.ChargerEvenementsDepuis(const AAgregatId: string;
  AVersionMin: Integer): TList<TEvenementBase>;
var
  Evt: TEvenementBase;
begin
  Result := TList<TEvenementBase>.Create;

  EnterCriticalSection(FLock);
  try
    for Evt in FEvenements do
    begin
      if (Evt.AgregatId = AAgregatId) and (Evt.Version >= AVersionMin) then
        Result.Add(Evt);
    end;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

function TEventStoreMemoire.ObtenirDerniereVersion(const AAgregatId: string): Integer;
var
  Evt: TEvenementBase;
begin
  Result := 0;

  EnterCriticalSection(FLock);
  try
    for Evt in FEvenements do
    begin
      if (Evt.AgregatId = AAgregatId) and (Evt.Version > Result) then
        Result := Evt.Version;
    end;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

end.
```

### Agrégat avec Event Sourcing

Un **agrégat** dans Event Sourcing se reconstruit en appliquant tous ses événements.

```pascal
unit Domain.Aggregates.Compte;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections,
  EventSourcing.Events, Domain.Events.Compte;

type
  TCompte = class
  private
    FId: string;
    FTitulaire: string;
    FSolde: Currency;
    FVersion: Integer;
    FEvenementsPendants: TObjectList<TEvenementBase>;

    // Appliquer un événement à l'état interne
    procedure AppliquerEvenement(const AEvenement: TEvenementBase);

    // Ajouter un événement (non encore persisté)
    procedure AjouterEvenement(const AEvenement: TEvenementBase);
  public
    constructor Create(const AId: string);
    destructor Destroy; override;

    // Opérations métier (génèrent des événements)
    procedure Ouvrir(const ATitulaire: string; ASoldeInitial: Currency);
    procedure Deposer(AMontant: Currency; const ADescription: string);
    procedure Retirer(AMontant: Currency; const ADescription: string);

    // Event Sourcing
    procedure ChargerDepuisHistorique(const AEvenements: TList<TEvenementBase>);
    function ObtenirEvenementsPendants: TList<TEvenementBase>;
    procedure MarquerEvenementsSauvegardes;

    property Id: string read FId;
    property Titulaire: string read FTitulaire;
    property Solde: Currency read FSolde;
    property Version: Integer read FVersion;
  end;

implementation

constructor TCompte.Create(const AId: string);
begin
  inherited Create;
  FId := AId;
  FVersion := 0;
  FSolde := 0;
  FEvenementsPendants := TObjectList<TEvenementBase>.Create(False);
end;

destructor TCompte.Destroy;
begin
  FEvenementsPendants.Free;
  inherited;
end;

procedure TCompte.AjouterEvenement(const AEvenement: TEvenementBase);
begin
  FEvenementsPendants.Add(AEvenement);
  AppliquerEvenement(AEvenement);
end;

procedure TCompte.AppliquerEvenement(const AEvenement: TEvenementBase);
begin
  // Appliquer les changements selon le type d'événement
  if AEvenement is TEvenementCompteOuvert then
  begin
    FTitulaire := TEvenementCompteOuvert(AEvenement).Titulaire;
    FSolde := TEvenementCompteOuvert(AEvenement).SoldeInitial;
    WriteLn(Format('[Compte] Ouvert pour %s avec %.2f €', [FTitulaire, FSolde]));
  end
  else if AEvenement is TEvenementArgentDepose then
  begin
    FSolde := FSolde + TEvenementArgentDepose(AEvenement).Montant;
    WriteLn(Format('[Compte] Dépôt de %.2f € - Nouveau solde: %.2f €',
      [TEvenementArgentDepose(AEvenement).Montant, FSolde]));
  end
  else if AEvenement is TEvenementArgentRetire then
  begin
    FSolde := FSolde - TEvenementArgentRetire(AEvenement).Montant;
    WriteLn(Format('[Compte] Retrait de %.2f € - Nouveau solde: %.2f €',
      [TEvenementArgentRetire(AEvenement).Montant, FSolde]));
  end;

  FVersion := AEvenement.Version;
end;

procedure TCompte.Ouvrir(const ATitulaire: string; ASoldeInitial: Currency);
var
  Evt: TEvenementCompteOuvert;
begin
  // Validation
  if FVersion > 0 then
    raise Exception.Create('Le compte est déjà ouvert');

  if ASoldeInitial < 0 then
    raise Exception.Create('Le solde initial ne peut être négatif');

  // Créer et appliquer l'événement
  Evt := TEvenementCompteOuvert.Create(FId, ATitulaire, ASoldeInitial, FVersion + 1);
  AjouterEvenement(Evt);
end;

procedure TCompte.Deposer(AMontant: Currency; const ADescription: string);
var
  Evt: TEvenementArgentDepose;
begin
  // Validation
  if AMontant <= 0 then
    raise Exception.Create('Le montant doit être positif');

  if FVersion = 0 then
    raise Exception.Create('Le compte n''est pas ouvert');

  // Créer et appliquer l'événement
  Evt := TEvenementArgentDepose.Create(FId, AMontant, ADescription, FVersion + 1);
  AjouterEvenement(Evt);
end;

procedure TCompte.Retirer(AMontant: Currency; const ADescription: string);
var
  Evt: TEvenementArgentRetire;
begin
  // Validation
  if AMontant <= 0 then
    raise Exception.Create('Le montant doit être positif');

  if FVersion = 0 then
    raise Exception.Create('Le compte n''est pas ouvert');

  if FSolde < AMontant then
    raise Exception.Create('Solde insuffisant');

  // Créer et appliquer l'événement
  Evt := TEvenementArgentRetire.Create(FId, AMontant, ADescription, FVersion + 1);
  AjouterEvenement(Evt);
end;

procedure TCompte.ChargerDepuisHistorique(const AEvenements: TList<TEvenementBase>);
var
  Evt: TEvenementBase;
begin
  WriteLn(Format('[Compte] Reconstruction depuis %d événements', [AEvenements.Count]));

  for Evt in AEvenements do
    AppliquerEvenement(Evt);

  WriteLn(Format('[Compte] État final: Titulaire=%s, Solde=%.2f €, Version=%d',
    [FTitulaire, FSolde, FVersion]));
end;

function TCompte.ObtenirEvenementsPendants: TList<TEvenementBase>;
begin
  Result := TList<TEvenementBase>.Create;
  Result.AddRange(FEvenementsPendants);
end;

procedure TCompte.MarquerEvenementsSauvegardes;
begin
  FEvenementsPendants.Clear;
end;

end.
```

### Repository avec Event Sourcing

```pascal
unit Domain.Repositories.CompteRepository;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections,
  EventSourcing.EventStore, EventSourcing.Events,
  Domain.Aggregates.Compte;

type
  ICompteRepository = interface
    ['{B2C3D4E5-F6A7-4890-B1C2-D3E4F5A6B7C8}']
    procedure Sauvegarder(const ACompte: TCompte);
    function Charger(const AId: string): TCompte;
    function Existe(const AId: string): Boolean;
  end;

  TCompteRepository = class(TInterfacedObject, ICompteRepository)
  private
    FEventStore: IEventStore;
  public
    constructor Create(const AEventStore: IEventStore);

    procedure Sauvegarder(const ACompte: TCompte);
    function Charger(const AId: string): TCompte;
    function Existe(const AId: string): Boolean;
  end;

implementation

constructor TCompteRepository.Create(const AEventStore: IEventStore);
begin
  inherited Create;
  FEventStore := AEventStore;
end;

procedure TCompteRepository.Sauvegarder(const ACompte: TCompte);
var
  Evenements: TList<TEvenementBase>;
  Evt: TEvenementBase;
begin
  // Récupérer les événements non sauvegardés
  Evenements := ACompte.ObtenirEvenementsPendants;
  try
    if Evenements.Count = 0 then
    begin
      WriteLn('[Repository] Aucun événement à sauvegarder');
      Exit;
    end;

    WriteLn(Format('[Repository] Sauvegarde de %d événements', [Evenements.Count]));

    // Sauvegarder tous les événements dans l'Event Store
    for Evt in Evenements do
      FEventStore.AjouterEvenement(Evt);

    // Marquer comme sauvegardés
    ACompte.MarquerEvenementsSauvegardes;

    WriteLn('[Repository] Sauvegarde réussie');
  finally
    Evenements.Free;
  end;
end;

function TCompteRepository.Charger(const AId: string): TCompte;
var
  Evenements: TList<TEvenementBase>;
begin
  WriteLn(Format('[Repository] Chargement du compte %s', [AId]));

  // Charger tous les événements de l'Event Store
  Evenements := FEventStore.ChargerEvenements(AId);
  try
    if Evenements.Count = 0 then
      raise Exception.CreateFmt('Compte %s non trouvé', [AId]);

    // Créer le compte et le reconstruire
    Result := TCompte.Create(AId);
    try
      Result.ChargerDepuisHistorique(Evenements);
    except
      Result.Free;
      raise;
    end;
  finally
    Evenements.Free;
  end;
end;

function TCompteRepository.Existe(const AId: string): Boolean;
begin
  Result := FEventStore.ObtenirDerniereVersion(AId) > 0;
end;

end.
```

### Utilisation complète

```pascal
program DemoEventSourcing;

{$mode objfpc}{$H+}

uses
  SysUtils,
  EventSourcing.EventStore,
  Domain.Aggregates.Compte,
  Domain.Repositories.CompteRepository;

var
  EventStore: IEventStore;
  Repository: ICompteRepository;
  Compte: TCompte;
  CompteRecharge: TCompte;
begin
  WriteLn('=== Démonstration Event Sourcing ===');
  WriteLn;

  // Créer l'Event Store
  EventStore := TEventStoreMemoire.Create;
  Repository := TCompteRepository.Create(EventStore);

  try
    // === SCÉNARIO 1: Créer un nouveau compte ===
    WriteLn('--- Création d''un nouveau compte ---');
    Compte := TCompte.Create('compte-123');
    try
      Compte.Ouvrir('Jean Dupont', 1000);
      Compte.Deposer(500, 'Salaire');
      Compte.Retirer(200, 'Courses');
      Compte.Deposer(300, 'Remboursement');

      WriteLn(Format('Solde final: %.2f €', [Compte.Solde]));

      // Sauvegarder (persister les événements)
      Repository.Sauvegarder(Compte);
    finally
      Compte.Free;
    end;

    WriteLn;
    WriteLn('--- Rechargement du compte depuis l''historique ---');

    // === SCÉNARIO 2: Recharger le compte ===
    CompteRecharge := Repository.Charger('compte-123');
    try
      WriteLn(Format('Compte rechargé: Titulaire=%s, Solde=%.2f €',
        [CompteRecharge.Titulaire, CompteRecharge.Solde]));

      // Continuer les opérations
      CompteRecharge.Retirer(100, 'Retrait DAB');
      Repository.Sauvegarder(CompteRecharge);

      WriteLn(Format('Nouveau solde: %.2f €', [CompteRecharge.Solde]));
    finally
      CompteRecharge.Free;
    end;

  except
    on E: Exception do
      WriteLn('ERREUR: ', E.Message);
  end;

  WriteLn;
  WriteLn('=== Fin de la démonstration ===');
  ReadLn;
end.
```

## CQRS (Command Query Responsibility Segregation)

### Concept fondamental

**CQRS** sépare les opérations d'écriture (Commands) des opérations de lecture (Queries).

```
APPROCHE TRADITIONNELLE
┌───────────────────────┐
│  Application          │
│  ┌────────────────┐   │
│  │  Modèle Unique │   │ ← Même modèle pour lire et écrire
│  └────────────────┘   │
│          │            │
│  ┌───────▼──────────┐ │
│  │  Base de données │
│  └──────────────────┘ │
└───────────────────────┘

CQRS
┌──────────────────────────────────────┐
│  Application                         │
│  ┌──────────┐      ┌──────────────┐  │
│  │ Commands │      │    Queries   │  │
│  │ (Écriture│      │   (Lecture)  │  │
│  └────┬─────┘      └──────┬───────┘  │
│       │                   │          │
│  ┌────▼──────┐      ┌─────▼───────┐  │
│  │  DB Write │      │   DB Read   │  │
│  │ (Normalisé│      │(Dénormalisé)│  │
│  └───────────┘      └─────────────┘  │
└──────────────────────────────────────┘
```

### Commands (Commandes)

Une **Command** représente une intention de changer l'état du système. C'est une demande qui peut échouer.

**Caractéristiques d'une commande :**
- **Impérative** : Nommée à l'impératif (OuvrirCompte, DeposerArgent)
- **Peut échouer** : Peut être rejetée (validation, règles métier)
- **Orientée tâche** : Exprime une action à effectuer
- **Retourne void** : Ne retourne pas de données (ou juste un ID/statut)

```pascal
unit CQRS.Commands;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  // Classe de base pour toutes les commandes
  TCommandeBase = class
  private
    FId: string;
    FTimestamp: TDateTime;
  public
    constructor Create;

    property Id: string read FId;
    property Timestamp: TDateTime read FTimestamp;
  end;

  // Commandes concrètes pour les comptes
  TCommandeOuvrirCompte = class(TCommandeBase)
  private
    FCompteId: string;
    FTitulaire: string;
    FSoldeInitial: Currency;
  public
    constructor Create(const ACompteId, ATitulaire: string; ASoldeInitial: Currency);

    property CompteId: string read FCompteId;
    property Titulaire: string read FTitulaire;
    property SoldeInitial: Currency read FSoldeInitial;
  end;

  TCommandeDeposerArgent = class(TCommandeBase)
  private
    FCompteId: string;
    FMontant: Currency;
    FDescription: string;
  public
    constructor Create(const ACompteId: string; AMontant: Currency;
                      const ADescription: string);

    property CompteId: string read FCompteId;
    property Montant: Currency read FMontant;
    property Description: string read FDescription;
  end;

  TCommandeRetirerArgent = class(TCommandeBase)
  private
    FCompteId: string;
    FMontant: Currency;
    FDescription: string;
  public
    constructor Create(const ACompteId: string; AMontant: Currency;
                      const ADescription: string);

    property CompteId: string read FCompteId;
    property Montant: Currency read FMontant;
    property Description: string read FDescription;
  end;

implementation

constructor TCommandeBase.Create;
begin
  inherited Create;
  FId := TGuid.NewGuid.ToString;
  FTimestamp := Now;
end;

constructor TCommandeOuvrirCompte.Create(const ACompteId, ATitulaire: string;
  ASoldeInitial: Currency);
begin
  inherited Create;
  FCompteId := ACompteId;
  FTitulaire := ATitulaire;
  FSoldeInitial := ASoldeInitial;
end;

constructor TCommandeDeposerArgent.Create(const ACompteId: string;
  AMontant: Currency; const ADescription: string);
begin
  inherited Create;
  FCompteId := ACompteId;
  FMontant := AMontant;
  FDescription := ADescription;
end;

constructor TCommandeRetirerArgent.Create(const ACompteId: string;
  AMontant: Currency; const ADescription: string);
begin
  inherited Create;
  FCompteId := ACompteId;
  FMontant := AMontant;
  FDescription := ADescription;
end;

end.
```

### Command Handlers (Gestionnaires de commandes)

Les **Command Handlers** traitent les commandes et modifient l'état du système.

```pascal
unit CQRS.CommandHandlers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  CQRS.Commands,
  Domain.Aggregates.Compte,
  Domain.Repositories.CompteRepository;

type
  // Interface générique pour les handlers
  ICommandHandler<T: TCommandeBase> = interface
    ['{C3D4E5F6-A7B8-4901-C2D3-E4F5A6B7C8D9}']
    procedure Traiter(const ACommande: T);
  end;

  // Handler : Ouvrir un compte
  THandlerOuvrirCompte = class(TInterfacedObject, ICommandHandler<TCommandeOuvrirCompte>)
  private
    FRepository: ICompteRepository;
  public
    constructor Create(const ARepository: ICompteRepository);
    procedure Traiter(const ACommande: TCommandeOuvrirCompte);
  end;

  // Handler : Déposer de l'argent
  THandlerDeposerArgent = class(TInterfacedObject, ICommandHandler<TCommandeDeposerArgent>)
  private
    FRepository: ICompteRepository;
  public
    constructor Create(const ARepository: ICompteRepository);
    procedure Traiter(const ACommande: TCommandeDeposerArgent);
  end;

  // Handler : Retirer de l'argent
  THandlerRetirerArgent = class(TInterfacedObject, ICommandHandler<TCommandeRetirerArgent>)
  private
    FRepository: ICompteRepository;
  public
    constructor Create(const ARepository: ICompteRepository);
    procedure Traiter(const ACommande: TCommandeRetirerArgent);
  end;

implementation

// THandlerOuvrirCompte

constructor THandlerOuvrirCompte.Create(const ARepository: ICompteRepository);
begin
  inherited Create;
  FRepository := ARepository;
end;

procedure THandlerOuvrirCompte.Traiter(const ACommande: TCommandeOuvrirCompte);
var
  Compte: TCompte;
begin
  WriteLn(Format('[CommandHandler] Traitement: OuvrirCompte pour %s',
    [ACommande.Titulaire]));

  // Vérifier que le compte n'existe pas déjà
  if FRepository.Existe(ACommande.CompteId) then
    raise Exception.Create('Ce compte existe déjà');

  // Créer et initialiser le compte
  Compte := TCompte.Create(ACommande.CompteId);
  try
    Compte.Ouvrir(ACommande.Titulaire, ACommande.SoldeInitial);

    // Sauvegarder
    FRepository.Sauvegarder(Compte);

    WriteLn('[CommandHandler] Compte ouvert avec succès');
  finally
    Compte.Free;
  end;
end;

// THandlerDeposerArgent

constructor THandlerDeposerArgent.Create(const ARepository: ICompteRepository);
begin
  inherited Create;
  FRepository := ARepository;
end;

procedure THandlerDeposerArgent.Traiter(const ACommande: TCommandeDeposerArgent);
var
  Compte: TCompte;
begin
  WriteLn(Format('[CommandHandler] Traitement: DeposerArgent %.2f € sur %s',
    [ACommande.Montant, ACommande.CompteId]));

  // Charger le compte
  Compte := FRepository.Charger(ACommande.CompteId);
  try
    // Effectuer le dépôt
    Compte.Deposer(ACommande.Montant, ACommande.Description);

    // Sauvegarder
    FRepository.Sauvegarder(Compte);

    WriteLn('[CommandHandler] Dépôt effectué avec succès');
  finally
    Compte.Free;
  end;
end;

// THandlerRetirerArgent

constructor THandlerRetirerArgent.Create(const ARepository: ICompteRepository);
begin
  inherited Create;
  FRepository := ARepository;
end;

procedure THandlerRetirerArgent.Traiter(const ACommande: TCommandeRetirerArgent);
var
  Compte: TCompte;
begin
  WriteLn(Format('[CommandHandler] Traitement: RetirerArgent %.2f € sur %s',
    [ACommande.Montant, ACommande.CompteId]));

  // Charger le compte
  Compte := FRepository.Charger(ACommande.CompteId);
  try
    // Effectuer le retrait
    Compte.Retirer(ACommande.Montant, ACommande.Description);

    // Sauvegarder
    FRepository.Sauvegarder(Compte);

    WriteLn('[CommandHandler] Retrait effectué avec succès');
  finally
    Compte.Free;
  end;
end;

end.
```

### Queries (Requêtes)

Une **Query** demande des informations sans modifier l'état du système.

**Caractéristiques d'une requête :**
- **Interrogative** : Obtenir des données
- **Idempotente** : Peut être exécutée plusieurs fois sans effet de bord
- **Optimisée pour la lecture** : Peut utiliser des vues dénormalisées
- **Retourne des données** : DTO (Data Transfer Object)

```pascal
unit CQRS.Queries;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections;

type
  // DTO (Data Transfer Object) pour la lecture
  TCompteDTO = record
    Id: string;
    Titulaire: string;
    Solde: Currency;
    DateOuverture: TDateTime;
    NombreTransactions: Integer;
  end;

  TTransactionDTO = record
    Id: string;
    CompteId: string;
    Type_: string; // 'Depot' ou 'Retrait'
    Montant: Currency;
    Description: string;
    Date: TDateTime;
  end;

  // Requêtes
  TQueryObtenirCompte = class
  private
    FCompteId: string;
  public
    constructor Create(const ACompteId: string);
    property CompteId: string read FCompteId;
  end;

  TQueryListerComptes = class
  private
    FPageNumber: Integer;
    FPageSize: Integer;
  public
    constructor Create(APageNumber: Integer = 1; APageSize: Integer = 20);
    property PageNumber: Integer read FPageNumber;
    property PageSize: Integer read FPageSize;
  end;

  TQueryObtenirTransactions = class
  private
    FCompteId: string;
    FDateDebut: TDateTime;
    FDateFin: TDateTime;
  public
    constructor Create(const ACompteId: string;
                      ADateDebut, ADateFin: TDateTime);
    property CompteId: string read FCompteId;
    property DateDebut: TDateTime read FDateDebut;
    property DateFin: TDateTime read FDateFin;
  end;

implementation

constructor TQueryObtenirCompte.Create(const ACompteId: string);
begin
  inherited Create;
  FCompteId := ACompteId;
end;

constructor TQueryListerComptes.Create(APageNumber, APageSize: Integer);
begin
  inherited Create;
  FPageNumber := APageNumber;
  FPageSize := APageSize;
end;

constructor TQueryObtenirTransactions.Create(const ACompteId: string;
  ADateDebut, ADateFin: TDateTime);
begin
  inherited Create;
  FCompteId := ACompteId;
  FDateDebut := ADateDebut;
  FDateFin := ADateFin;
end;

end.
```

### Query Handlers (Gestionnaires de requêtes)

Les **Query Handlers** récupèrent les données depuis le modèle de lecture.

```pascal
unit CQRS.QueryHandlers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections,
  CQRS.Queries,
  CQRS.ReadModel;

type
  // Interface générique pour les query handlers
  IQueryHandler<TQuery, TResult> = interface
    ['{D4E5F6A7-B8C9-4012-D3E4-F5A6B7C8D9E0}']
    function Executer(const AQuery: TQuery): TResult;
  end;

  // Handler : Obtenir un compte
  THandlerObtenirCompte = class(TInterfacedObject,
    IQueryHandler<TQueryObtenirCompte, TCompteDTO>)
  private
    FReadModel: ICompteReadModel;
  public
    constructor Create(const AReadModel: ICompteReadModel);
    function Executer(const AQuery: TQueryObtenirCompte): TCompteDTO;
  end;

  // Handler : Lister les comptes
  THandlerListerComptes = class(TInterfacedObject,
    IQueryHandler<TQueryListerComptes, TList<TCompteDTO>>)
  private
    FReadModel: ICompteReadModel;
  public
    constructor Create(const AReadModel: ICompteReadModel);
    function Executer(const AQuery: TQueryListerComptes): TList<TCompteDTO>;
  end;

  // Handler : Obtenir les transactions
  THandlerObtenirTransactions = class(TInterfacedObject,
    IQueryHandler<TQueryObtenirTransactions, TList<TTransactionDTO>>)
  private
    FReadModel: ICompteReadModel;
  public
    constructor Create(const AReadModel: ICompteReadModel);
    function Executer(const AQuery: TQueryObtenirTransactions): TList<TTransactionDTO>;
  end;

implementation

// THandlerObtenirCompte

constructor THandlerObtenirCompte.Create(const AReadModel: ICompteReadModel);
begin
  inherited Create;
  FReadModel := AReadModel;
end;

function THandlerObtenirCompte.Executer(const AQuery: TQueryObtenirCompte): TCompteDTO;
begin
  WriteLn(Format('[QueryHandler] Exécution: ObtenirCompte %s', [AQuery.CompteId]));
  Result := FReadModel.ObtenirCompte(AQuery.CompteId);
end;

// THandlerListerComptes

constructor THandlerListerComptes.Create(const AReadModel: ICompteReadModel);
begin
  inherited Create;
  FReadModel := AReadModel;
end;

function THandlerListerComptes.Executer(const AQuery: TQueryListerComptes): TList<TCompteDTO>;
begin
  WriteLn(Format('[QueryHandler] Exécution: ListerComptes (page %d)',
    [AQuery.PageNumber]));
  Result := FReadModel.ListerComptes(AQuery.PageNumber, AQuery.PageSize);
end;

// THandlerObtenirTransactions

constructor THandlerObtenirTransactions.Create(const AReadModel: ICompteReadModel);
begin
  inherited Create;
  FReadModel := AReadModel;
end;

function THandlerObtenirTransactions.Executer(
  const AQuery: TQueryObtenirTransactions): TList<TTransactionDTO>;
begin
  WriteLn(Format('[QueryHandler] Exécution: ObtenirTransactions pour %s',
    [AQuery.CompteId]));
  Result := FReadModel.ObtenirTransactions(
    AQuery.CompteId,
    AQuery.DateDebut,
    AQuery.DateFin
  );
end;

end.
```

### Read Model (Modèle de lecture)

Le **Read Model** est optimisé pour les requêtes. Il peut être dénormalisé et maintenu à jour via les événements.

```pascal
unit CQRS.ReadModel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections,
  CQRS.Queries,
  EventSourcing.Events;

type
  // Interface du modèle de lecture
  ICompteReadModel = interface
    ['{E5F6A7B8-C9D0-4123-E4F5-A6B7C8D9E0F1}']
    function ObtenirCompte(const ACompteId: string): TCompteDTO;
    function ListerComptes(APage, APageSize: Integer): TList<TCompteDTO>;
    function ObtenirTransactions(const ACompteId: string;
                                 ADateDebut, ADateFin: TDateTime): TList<TTransactionDTO>;

    // Mise à jour depuis les événements
    procedure MettreAJourDepuisEvenement(const AEvenement: TEvenementBase);
  end;

  // Implémentation en mémoire (pour démo)
  TCompteReadModelMemoire = class(TInterfacedObject, ICompteReadModel)
  private
    FComptes: TDictionary<string, TCompteDTO>;
    FTransactions: TObjectList<TTransactionDTO>;
    FLock: TRTLCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;

    function ObtenirCompte(const ACompteId: string): TCompteDTO;
    function ListerComptes(APage, APageSize: Integer): TList<TCompteDTO>;
    function ObtenirTransactions(const ACompteId: string;
                                 ADateDebut, ADateFin: TDateTime): TList<TTransactionDTO>;

    procedure MettreAJourDepuisEvenement(const AEvenement: TEvenementBase);
  end;

implementation

uses
  Domain.Events.Compte, Math;

constructor TCompteReadModelMemoire.Create;
begin
  inherited Create;
  FComptes := TDictionary<string, TCompteDTO>.Create;
  FTransactions := TObjectList<TTransactionDTO>.Create(False);
  InitCriticalSection(FLock);
end;

destructor TCompteReadModelMemoire.Destroy;
begin
  DoneCriticalSection(FLock);
  FTransactions.Free;
  FComptes.Free;
  inherited;
end;

function TCompteReadModelMemoire.ObtenirCompte(const ACompteId: string): TCompteDTO;
begin
  EnterCriticalSection(FLock);
  try
    if not FComptes.TryGetValue(ACompteId, Result) then
      raise Exception.CreateFmt('Compte %s non trouvé dans le read model', [ACompteId]);
  finally
    LeaveCriticalSection(FLock);
  end;
end;

function TCompteReadModelMemoire.ListerComptes(APage, APageSize: Integer): TList<TCompteDTO>;
var
  TousLesComptes: TArray<TCompteDTO>;
  Debut, Fin, i: Integer;
  Compte: TCompteDTO;
begin
  Result := TList<TCompteDTO>.Create;

  EnterCriticalSection(FLock);
  try
    TousLesComptes := FComptes.Values.ToArray;

    // Pagination
    Debut := (APage - 1) * APageSize;
    Fin := Min(Debut + APageSize - 1, Length(TousLesComptes) - 1);

    for i := Debut to Fin do
      Result.Add(TousLesComptes[i]);
  finally
    LeaveCriticalSection(FLock);
  end;
end;

function TCompteReadModelMemoire.ObtenirTransactions(const ACompteId: string;
  ADateDebut, ADateFin: TDateTime): TList<TTransactionDTO>;
var
  Transaction: TTransactionDTO;
begin
  Result := TList<TTransactionDTO>.Create;

  EnterCriticalSection(FLock);
  try
    for Transaction in FTransactions do
    begin
      if (Transaction.CompteId = ACompteId) and
         (Transaction.Date >= ADateDebut) and
         (Transaction.Date <= ADateFin) then
        Result.Add(Transaction);
    end;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TCompteReadModelMemoire.MettreAJourDepuisEvenement(const AEvenement: TEvenementBase);
var
  Compte: TCompteDTO;
  Transaction: TTransactionDTO;
begin
  EnterCriticalSection(FLock);
  try
    // Mettre à jour selon le type d'événement
    if AEvenement is TEvenementCompteOuvert then
    begin
      // Créer un nouveau compte dans le read model
      Compte.Id := AEvenement.AgregatId;
      Compte.Titulaire := TEvenementCompteOuvert(AEvenement).Titulaire;
      Compte.Solde := TEvenementCompteOuvert(AEvenement).SoldeInitial;
      Compte.DateOuverture := AEvenement.Timestamp;
      Compte.NombreTransactions := 0;

      FComptes.AddOrSetValue(Compte.Id, Compte);

      WriteLn(Format('[ReadModel] Compte créé: %s', [Compte.Id]));
    end
    else if AEvenement is TEvenementArgentDepose then
    begin
      // Mettre à jour le solde
      if FComptes.TryGetValue(AEvenement.AgregatId, Compte) then
      begin
        Compte.Solde := Compte.Solde + TEvenementArgentDepose(AEvenement).Montant;
        Inc(Compte.NombreTransactions);
        FComptes.AddOrSetValue(Compte.Id, Compte);

        // Ajouter la transaction
        Transaction.Id := AEvenement.Id;
        Transaction.CompteId := AEvenement.AgregatId;
        Transaction.Type_ := 'Depot';
        Transaction.Montant := TEvenementArgentDepose(AEvenement).Montant;
        Transaction.Description := TEvenementArgentDepose(AEvenement).Description;
        Transaction.Date := AEvenement.Timestamp;
        FTransactions.Add(Transaction);

        WriteLn(Format('[ReadModel] Dépôt enregistré: %.2f €', [Transaction.Montant]));
      end;
    end
    else if AEvenement is TEvenementArgentRetire then
    begin
      // Mettre à jour le solde
      if FComptes.TryGetValue(AEvenement.AgregatId, Compte) then
      begin
        Compte.Solde := Compte.Solde - TEvenementArgentRetire(AEvenement).Montant;
        Inc(Compte.NombreTransactions);
        FComptes.AddOrSetValue(Compte.Id, Compte);

        // Ajouter la transaction
        Transaction.Id := AEvenement.Id;
        Transaction.CompteId := AEvenement.AgregatId;
        Transaction.Type_ := 'Retrait';
        Transaction.Montant := TEvenementArgentRetire(AEvenement).Montant;
        Transaction.Description := TEvenementArgentRetire(AEvenement).Description;
        Transaction.Date := AEvenement.Timestamp;
        FTransactions.Add(Transaction);

        WriteLn(Format('[ReadModel] Retrait enregistré: %.2f €', [Transaction.Montant]));
      end;
    end;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

end.
```

### Event Bus pour synchroniser les modèles

Un **Event Bus** permet de propager les événements du modèle d'écriture vers le modèle de lecture.

```pascal
unit CQRS.EventBus;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections,
  EventSourcing.Events;

type
  // Souscripteur d'événements
  IEventSubscriber = interface
    ['{F6A7B8C9-D0E1-4234-F5A6-B7C8D9E0F1A2}']
    procedure TraiterEvenement(const AEvenement: TEvenementBase);
  end;

  // Bus d'événements
  TEventBus = class
  private
    FSubscribers: TList<IEventSubscriber>;
    FLock: TRTLCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Abonner(const ASubscriber: IEventSubscriber);
    procedure Desabonner(const ASubscriber: IEventSubscriber);
    procedure Publier(const AEvenement: TEvenementBase);
  end;

implementation

constructor TEventBus.Create;
begin
  inherited Create;
  FSubscribers := TList<IEventSubscriber>.Create;
  InitCriticalSection(FLock);
end;

destructor TEventBus.Destroy;
begin
  DoneCriticalSection(FLock);
  FSubscribers.Free;
  inherited;
end;

procedure TEventBus.Abonner(const ASubscriber: IEventSubscriber);
begin
  EnterCriticalSection(FLock);
  try
    if not FSubscribers.Contains(ASubscriber) then
    begin
      FSubscribers.Add(ASubscriber);
      WriteLn('[EventBus] Nouveau souscripteur enregistré');
    end;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TEventBus.Desabonner(const ASubscriber: IEventSubscriber);
begin
  EnterCriticalSection(FLock);
  try
    FSubscribers.Remove(ASubscriber);
    WriteLn('[EventBus] Souscripteur retiré');
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TEventBus.Publier(const AEvenement: TEvenementBase);
var
  Subscriber: IEventSubscriber;
begin
  WriteLn(Format('[EventBus] Publication événement: %s', [AEvenement.ClassName]));

  EnterCriticalSection(FLock);
  try
    for Subscriber in FSubscribers do
    begin
      try
        Subscriber.TraiterEvenement(AEvenement);
      except
        on E: Exception do
          WriteLn(Format('[EventBus] Erreur subscriber: %s', [E.Message]));
      end;
    end;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

end.
```

### Projection : Mettre à jour le Read Model

Une **Projection** écoute les événements et met à jour le modèle de lecture.

```pascal
unit CQRS.Projections;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  EventSourcing.Events,
  CQRS.EventBus,
  CQRS.ReadModel;

type
  // Projection pour maintenir le read model à jour
  TCompteProjection = class(TInterfacedObject, IEventSubscriber)
  private
    FReadModel: ICompteReadModel;
  public
    constructor Create(const AReadModel: ICompteReadModel);

    procedure TraiterEvenement(const AEvenement: TEvenementBase);
  end;

implementation

constructor TCompteProjection.Create(const AReadModel: ICompteReadModel);
begin
  inherited Create;
  FReadModel := AReadModel;
end;

procedure TCompteProjection.TraiterEvenement(const AEvenement: TEvenementBase);
begin
  WriteLn(Format('[Projection] Traitement événement: %s', [AEvenement.ClassName]));

  // Déléguer au read model
  FReadModel.MettreAJourDepuisEvenement(AEvenement);
end;

end.
```

### Démonstration complète CQRS + Event Sourcing

```pascal
program DemoCQRS;

{$mode objfpc}{$H+}

uses
  SysUtils, Generics.Collections,
  EventSourcing.EventStore,
  EventSourcing.Events,
  Domain.Repositories.CompteRepository,
  CQRS.Commands,
  CQRS.CommandHandlers,
  CQRS.Queries,
  CQRS.QueryHandlers,
  CQRS.ReadModel,
  CQRS.EventBus,
  CQRS.Projections;

var
  EventStore: IEventStore;
  WriteRepository: ICompteRepository;
  ReadModel: ICompteReadModel;
  EventBus: TEventBus;
  Projection: TCompteProjection;

  // Command handlers
  HandlerOuvrir: THandlerOuvrirCompte;
  HandlerDeposer: THandlerDeposerArgent;
  HandlerRetirer: THandlerRetirerArgent;

  // Query handlers
  HandlerObtenirCompte: THandlerObtenirCompte;
  HandlerListerComptes: THandlerListerComptes;

  // Commands et Queries
  CmdOuvrir: TCommandeOuvrirCompte;
  CmdDeposer: TCommandeDeposerArgent;
  CmdRetirer: TCommandeRetirerArgent;
  QueryCompte: TQueryObtenirCompte;
  QueryListe: TQueryListerComptes;

  // Résultats
  CompteDTO: TCompteDTO;
  ListeComptes: TList<TCompteDTO>;
  Compte: TCompteDTO;

begin
  WriteLn('=== Démonstration CQRS + Event Sourcing ===');
  WriteLn;

  try
    // === CONFIGURATION ===
    WriteLn('--- Configuration du système ---');

    // Côté écriture
    EventStore := TEventStoreMemoire.Create;
    WriteRepository := TCompteRepository.Create(EventStore);

    // Côté lecture
    ReadModel := TCompteReadModelMemoire.Create;

    // Event Bus pour synchroniser
    EventBus := TEventBus.Create;
    Projection := TCompteProjection.Create(ReadModel);
    EventBus.Abonner(Projection);

    // Command handlers
    HandlerOuvrir := THandlerOuvrirCompte.Create(WriteRepository);
    HandlerDeposer := THandlerDeposerArgent.Create(WriteRepository);
    HandlerRetirer := THandlerRetirerArgent.Create(WriteRepository);

    // Query handlers
    HandlerObtenirCompte := THandlerObtenirCompte.Create(ReadModel);
    HandlerListerComptes := THandlerListerComptes.Create(ReadModel);

    WriteLn('Configuration terminée');
    WriteLn;

    // === SCÉNARIO : Écriture (Commands) ===
    WriteLn('--- CÔTÉ ÉCRITURE (Commands) ---');
    WriteLn;

    // Commande 1 : Ouvrir un compte
    WriteLn('1. Ouvrir un compte');
    CmdOuvrir := TCommandeOuvrirCompte.Create('compte-123', 'Jean Dupont', 1000);
    try
      HandlerOuvrir.Traiter(CmdOuvrir);

      // Publier les événements vers le read model
      // (Dans un système réel, ceci serait automatique)
      EventBus.Publier(TEvenementCompteOuvert.Create('compte-123', 'Jean Dupont', 1000, 1));
    finally
      CmdOuvrir.Free;
    end;
    WriteLn;

    // Commande 2 : Déposer de l'argent
    WriteLn('2. Déposer 500 €');
    CmdDeposer := TCommandeDeposerArgent.Create('compte-123', 500, 'Salaire');
    try
      HandlerDeposer.Traiter(CmdDeposer);
      EventBus.Publier(TEvenementArgentDepose.Create('compte-123', 500, 'Salaire', 2));
    finally
      CmdDeposer.Free;
    end;
    WriteLn;

    // Commande 3 : Retirer de l'argent
    WriteLn('3. Retirer 200 €');
    CmdRetirer := TCommandeRetirerArgent.Create('compte-123', 200, 'Courses');
    try
      HandlerRetirer.Traiter(CmdRetirer);
      EventBus.Publier(TEvenementArgentRetire.Create('compte-123', 200, 'Courses', 3));
    finally
      CmdRetirer.Free;
    end;
    WriteLn;

    // === SCÉNARIO : Lecture (Queries) ===
    WriteLn('--- CÔTÉ LECTURE (Queries) ---');
    WriteLn;

    // Query 1 : Obtenir un compte
    WriteLn('1. Obtenir le détail du compte');
    QueryCompte := TQueryObtenirCompte.Create('compte-123');
    try
      CompteDTO := HandlerObtenirCompte.Executer(QueryCompte);

      WriteLn('Résultat:');
      WriteLn('  ID: ', CompteDTO.Id);
      WriteLn('  Titulaire: ', CompteDTO.Titulaire);
      WriteLn('  Solde: ', CompteDTO.Solde:0:2, ' €');
      WriteLn('  Nombre transactions: ', CompteDTO.NombreTransactions);
    finally
      QueryCompte.Free;
    end;
    WriteLn;

    // Query 2 : Lister tous les comptes
    WriteLn('2. Lister tous les comptes');
    QueryListe := TQueryListerComptes.Create(1, 10);
    try
      ListeComptes := HandlerListerComptes.Executer(QueryListe);
      try
        WriteLn('Résultat: ', ListeComptes.Count, ' compte(s) trouvé(s)');
        for Compte in ListeComptes do
        begin
          WriteLn('  - ', Compte.Titulaire, ': ', Compte.Solde:0:2, ' €');
        end;
      finally
        ListeComptes.Free;
      end;
    finally
      QueryListe.Free;
    end;

    WriteLn;
    WriteLn('=== Fin de la démonstration ===');

  except
    on E: Exception do
      WriteLn('ERREUR: ', E.Message);
  end;

  ReadLn;
end.
```

**Sortie attendue :**
```
=== Démonstration CQRS + Event Sourcing ===

--- Configuration du système ---
[EventBus] Nouveau souscripteur enregistré
Configuration terminée

--- CÔTÉ ÉCRITURE (Commands) ---

1. Ouvrir un compte
[CommandHandler] Traitement: OuvrirCompte pour Jean Dupont
[Compte] Ouvert pour Jean Dupont avec 1000.00 €
[Repository] Sauvegarde de 1 événements
[EventStore] Événement ajouté: TEvenementCompteOuvert (Version: 1)
[EventBus] Publication événement: TEvenementCompteOuvert
[Projection] Traitement événement: TEvenementCompteOuvert
[ReadModel] Compte créé: compte-123

2. Déposer 500 €
[CommandHandler] Traitement: DeposerArgent 500.00 € sur compte-123
[Compte] Dépôt de 500.00 € - Nouveau solde: 1500.00 €
[ReadModel] Dépôt enregistré: 500.00 €

3. Retirer 200 €
[CommandHandler] Traitement: RetirerArgent 200.00 € sur compte-123
[Compte] Retrait de 200.00 € - Nouveau solde: 1300.00 €
[ReadModel] Retrait enregistré: 200.00 €

--- CÔTÉ LECTURE (Queries) ---

1. Obtenir le détail du compte
[QueryHandler] Exécution: ObtenirCompte compte-123
Résultat:
  ID: compte-123
  Titulaire: Jean Dupont
  Solde: 1300.00 €
  Nombre transactions: 2

2. Lister tous les comptes
[QueryHandler] Exécution: ListerComptes (page 1)
Résultat: 1 compte(s) trouvé(s)
  - Jean Dupont: 1300.00 €

=== Fin de la démonstration ===
```

## Patterns avancés

### 1. Snapshots (Instantanés)

Pour éviter de rejouer tous les événements à chaque fois, on peut créer des **snapshots** périodiques.

```pascal
unit EventSourcing.Snapshot;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson;

type
  // Snapshot de l'état d'un agrégat
  TSnapshot = class
  private
    FAgregatId: string;
    FVersion: Integer;
    FTimestamp: TDateTime;
    FData: TJSONObject;
  public
    constructor Create(const AAgregatId: string; AVersion: Integer;
                      AData: TJSONObject);
    destructor Destroy; override;

    property AgregatId: string read FAgregatId;
    property Version: Integer read FVersion;
    property Timestamp: TDateTime read FTimestamp;
    property Data: TJSONObject read FData;
  end;

  // Store de snapshots
  ISnapshotStore = interface
    ['{A7B8C9D0-E1F2-4345-A6B7-C8D9E0F1A2B3}']
    procedure Sauvegarder(const ASnapshot: TSnapshot);
    function Charger(const AAgregatId: string): TSnapshot;
    function ObtenirDerniereVersion(const AAgregatId: string): Integer;
  end;

  TSnapshotStoreMemoire = class(TInterfacedObject, ISnapshotStore)
  private
    FSnapshots: TObjectDictionary<string, TSnapshot>;
    FLock: TRTLCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Sauvegarder(const ASnapshot: TSnapshot);
    function Charger(const AAgregatId: string): TSnapshot;
    function ObtenirDerniereVersion(const AAgregatId: string): Integer;
  end;

implementation

uses
  Generics.Collections;

constructor TSnapshot.Create(const AAgregatId: string; AVersion: Integer;
  AData: TJSONObject);
begin
  inherited Create;
  FAgregatId := AAgregatId;
  FVersion := AVersion;
  FTimestamp := Now;
  FData := AData;
end;

destructor TSnapshot.Destroy;
begin
  FData.Free;
  inherited;
end;

constructor TSnapshotStoreMemoire.Create;
begin
  inherited Create;
  FSnapshots := TObjectDictionary<string, TSnapshot>.Create([doOwnsValues]);
  InitCriticalSection(FLock);
end;

destructor TSnapshotStoreMemoire.Destroy;
begin
  DoneCriticalSection(FLock);
  FSnapshots.Free;
  inherited;
end;

procedure TSnapshotStoreMemoire.Sauvegarder(const ASnapshot: TSnapshot);
begin
  EnterCriticalSection(FLock);
  try
    FSnapshots.AddOrSetValue(ASnapshot.AgregatId, ASnapshot);
    WriteLn(Format('[SnapshotStore] Snapshot sauvegardé: %s (Version: %d)',
      [ASnapshot.AgregatId, ASnapshot.Version]));
  finally
    LeaveCriticalSection(FLock);
  end;
end;

function TSnapshotStoreMemoire.Charger(const AAgregatId: string): TSnapshot;
begin
  EnterCriticalSection(FLock);
  try
    if not FSnapshots.TryGetValue(AAgregatId, Result) then
      Result := nil;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

function TSnapshotStoreMemoire.ObtenirDerniereVersion(const AAgregatId: string): Integer;
var
  Snapshot: TSnapshot;
begin
  Snapshot := Charger(AAgregatId);
  if Assigned(Snapshot) then
    Result := Snapshot.Version
  else
    Result := 0;
end;

end.
```

**Utilisation des snapshots :**

```pascal
procedure ChargerCompteAvecSnapshot(const ACompteId: string);
var
  Snapshot: TSnapshot;
  Evenements: TList<TEvenementBase>;
  Compte: TCompte;
  VersionSnapshot: Integer;
begin
  // Charger le dernier snapshot
  Snapshot := SnapshotStore.Charger(ACompteId);

  if Assigned(Snapshot) then
  begin
    // Reconstruire depuis le snapshot
    Compte := TCompte.DepuisSnapshot(Snapshot);
    VersionSnapshot := Snapshot.Version;

    WriteLn(Format('Snapshot chargé à la version %d', [VersionSnapshot]));

    // Charger seulement les événements après le snapshot
    Evenements := EventStore.ChargerEvenementsDepuis(ACompteId, VersionSnapshot + 1);
  end
  else
  begin
    // Pas de snapshot, créer un nouveau compte
    Compte := TCompte.Create(ACompteId);

    // Charger tous les événements
    Evenements := EventStore.ChargerEvenements(ACompteId);
  end;

  try
    // Appliquer les événements restants
    Compte.ChargerDepuisHistorique(Evenements);

    // Créer un nouveau snapshot tous les 10 événements
    if Compte.Version mod 10 = 0 then
    begin
      Snapshot := Compte.CreerSnapshot;
      SnapshotStore.Sauvegarder(Snapshot);
    end;
  finally
    Evenements.Free;
  end;
end;
```

### 2. Event Versioning (Versionnement des événements)

Les événements doivent évoluer sans casser les anciens.

```pascal
unit EventSourcing.EventVersioning;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson,
  EventSourcing.Events;

type
  // Convertisseur d'événements
  IEventUpgrader = interface
    ['{B8C9D0E1-F2A3-4456-B7C8-D9E0F1A2B3C4}']
    function PeutUpgrader(const AType: string; AVersion: Integer): Boolean;
    function Upgrader(AJSON: TJSONObject): TJSONObject;
  end;

  // Upgrader V1 → V2 pour CompteOuvert
  TUpgraderCompteOuvertV1VersV2 = class(TInterfacedObject, IEventUpgrader)
  public
    function PeutUpgrader(const AType: string; AVersion: Integer): Boolean;
    function Upgrader(AJSON: TJSONObject): TJSONObject;
  end;

implementation

function TUpgraderCompteOuvertV1VersV2.PeutUpgrader(const AType: string;
  AVersion: Integer): Boolean;
begin
  Result := (AType = 'CompteOuvert') and (AVersion = 1);
end;

function TUpgraderCompteOuvertV1VersV2.Upgrader(AJSON: TJSONObject): TJSONObject;
begin
  Result := TJSONObject.Create;

  // Copier les champs existants
  Result.Add('id', AJSON.Get('id', ''));
  Result.Add('agregat_id', AJSON.Get('agregat_id', ''));
  Result.Add('type', AJSON.Get('type', ''));
  Result.Add('timestamp', AJSON.Get('timestamp', ''));
  Result.Add('titulaire', AJSON.Get('titulaire', ''));
  Result.Add('solde_initial', AJSON.Get('solde_initial', 0.0));

  // Ajouter les nouveaux champs de la V2
  Result.Add('devise', 'EUR'); // Nouveau champ avec valeur par défaut
  Result.Add('type_compte', 'COURANT'); // Nouveau champ

  // Mettre à jour la version
  Result.Add('version_schema', 2);

  WriteLn('[EventUpgrader] Événement upgradé de V1 à V2');
end;

end.
```

### 3. Process Manager (Saga)

Un **Process Manager** coordonne plusieurs agrégats en réponse à des événements.

```pascal
unit CQRS.ProcessManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  EventSourcing.Events,
  CQRS.EventBus,
  CQRS.Commands,
  CQRS.CommandHandlers;

type
  // Process Manager pour un processus métier complexe
  TProcessTransfert = class(TInterfacedObject, IEventSubscriber)
  private
    FHandlerDebiter: THandlerRetirerArgent;
    FHandlerCrediter: THandlerDeposerArgent;
    FTransfertsEnCours: TDictionary<string, string>; // TransfertId -> État
  public
    constructor Create(const AHandlerDebiter: THandlerRetirerArgent;
                      const AHandlerCrediter: THandlerDeposerArgent);
    destructor Destroy; override;

    procedure TraiterEvenement(const AEvenement: TEvenementBase);
    procedure InitierTransfert(const ACompteSource, ACompteDestination: string;
                              AMontant: Currency);
  end;

implementation

uses
  Generics.Collections,
  Domain.Events.Compte;

constructor TProcessTransfert.Create(const AHandlerDebiter: THandlerRetirerArgent;
  const AHandlerCrediter: THandlerDeposerArgent);
begin
  inherited Create;
  FHandlerDebiter := AHandlerDebiter;
  FHandlerCrediter := AHandlerCrediter;
  FTransfertsEnCours := TDictionary<string, string>.Create;
end;

destructor TProcessTransfert.Destroy;
begin
  FTransfertsEnCours.Free;
  inherited;
end;

procedure TProcessTransfert.InitierTransfert(const ACompteSource,
  ACompteDestination: string; AMontant: Currency);
var
  TransfertId: string;
  CmdRetrait: TCommandeRetirerArgent;
begin
  TransfertId := TGuid.NewGuid.ToString;

  WriteLn(Format('[ProcessManager] Initiation transfert %s: %.2f € de %s vers %s',
    [TransfertId, AMontant, ACompteSource, ACompteDestination]));

  // Étape 1 : Débiter le compte source
  CmdRetrait := TCommandeRetirerArgent.Create(
    ACompteSource,
    AMontant,
    'Transfert ' + TransfertId
  );
  try
    FHandlerDebiter.Traiter(CmdRetrait);

    // Mémoriser l'état
    FTransfertsEnCours.Add(TransfertId, 'DEBITE:' + ACompteDestination);
  finally
    CmdRetrait.Free;
  end;
end;

procedure TProcessTransfert.TraiterEvenement(const AEvenement: TEvenementBase);
var
  TransfertId, CompteDestination: string;
  InfoTransfert: string;
  P: Integer;
  CmdDepot: TCommandeDeposerArgent;
begin
  // Réagir aux événements de retrait pour compléter le transfert
  if AEvenement is TEvenementArgentRetire then
  begin
    // Vérifier si c'est un de nos transferts
    for TransfertId in FTransfertsEnCours.Keys do
    begin
      if Pos(TransfertId, TEvenementArgentRetire(AEvenement).Description) > 0 then
      begin
        WriteLn(Format('[ProcessManager] Étape 1 complétée pour %s', [TransfertId]));

        // Récupérer le compte destination (format: "DEBITE:CompteId")
        InfoTransfert := FTransfertsEnCours[TransfertId];
        P := Pos(':', InfoTransfert);
        if P > 0 then
        begin
          CompteDestination := Copy(InfoTransfert, P + 1, Length(InfoTransfert));

          // Étape 2 : Créditer le compte destination
          CmdDepot := TCommandeDeposerArgent.Create(
            CompteDestination,
            TEvenementArgentRetire(AEvenement).Montant,
            'Transfert ' + TransfertId
          );
          try
            FHandlerCrediter.Traiter(CmdDepot);

            WriteLn(Format('[ProcessManager] Transfert %s complété', [TransfertId]));
            FTransfertsEnCours.Remove(TransfertId);
          finally
            CmdDepot.Free;
          end;
        end;
      end;
    end;
  end;
end;

end.
```

## Bonnes pratiques

### 1. Nommer les événements au passé

```pascal
// ✅ Bon
TEvenementCompteOuvert
TEvenementArgentDepose
TEvenementCommandeValidee

// ❌ Mauvais
TEvenementOuvrirCompte
TEvenementDeposer
TEvenementValiderCommande
```

### 2. Événements immuables

```pascal
// ✅ Bon : propriétés en lecture seule
type
  TEvenement = class
  private
    FMontant: Currency;
  public
    constructor Create(AMontant: Currency);
    property Montant: Currency read FMontant; // Pas de write
  end;

// ❌ Mauvais : propriétés modifiables
type
  TEvenement = class
  public
    Montant: Currency; // Public, modifiable
  end;
```

### 3. Séparer vraiment lecture et écriture

```pascal
// ✅ Bon : modèles séparés
type
  // Modèle d'écriture (domaine)
  TCompte = class
    procedure Deposer(AMontant: Currency);
  end;

  // Modèle de lecture (DTO)
  TCompteDTO = record
    Id: string;
    Solde: Currency;
    // Champs dénormalisés pour optimiser les lectures
    NombreTotalTransactions: Integer;
    DateDerniereOperation: TDateTime;
  end;

// ❌ Mauvais : même modèle pour tout
type
  TCompte = class
    procedure Deposer(AMontant: Currency);
    function GetSolde: Currency; // Mélange écriture/lecture
  end;
```

### 4. Gérer la cohérence éventuelle

```pascal
// Le read model n'est pas immédiatement à jour
procedure DeposerEtAfficher(const ACompteId: string);
var
  Cmd: TCommandeDeposerArgent;
  Query: TQueryObtenirCompte;
  Compte: TCompteDTO;
begin
  // Dépôt (écriture)
  Cmd := TCommandeDeposerArgent.Create(ACompteId, 100, 'Test');
  try
    CommandHandler.Traiter(Cmd);
  finally
    Cmd.Free;
  end;

  // ⚠️ Attention : le read model n'est peut-être pas encore à jour !
  Sleep(100); // Attendre la propagation (pas idéal)

  // Lecture
  Query := TQueryObtenirCompte.Create(ACompteId);
  try
    Compte := QueryHandler.Executer(Query);
    WriteLn('Nouveau solde: ', Compte.Solde:0:2);
  finally
    Query.Free;
  end;
end;
```

### 5. Versioning des événements

```pascal
// Toujours inclure un numéro de version de schéma
type
  TEvenement = class
  private
    FVersionSchema: Integer;
  public
    property VersionSchema: Integer read FVersionSchema;
  end;

// Gérer les anciennes versions
procedure ChargerEvenement(AJSON: TJSONObject);
var
  Version: Integer;
begin
  Version := AJSON.Get('version_schema', 1);

  case Version of
    1: ChargerV1(AJSON);
    2: ChargerV2(AJSON);
    else
      raise Exception.CreateFmt('Version %d non supportée', [Version]);
  end;
end;
```

## Quand utiliser Event Sourcing et CQRS ?

### ✅ Utilisez Event Sourcing quand :

- **Audit complet requis** : Finance, santé, secteurs réglementés
- **Analyse temporelle** : Business Intelligence, machine learning
- **Débogage complexe** : "Comment sommes-nous arrivés à cet état ?"
- **Conformité** : RGPD (droit à l'oubli peut être complexe)
- **Événements métier importants** : E-commerce, logistique

### ✅ Utilisez CQRS quand :

- **Lectures et écritures déséquilibrées** : Beaucoup plus de lectures que d'écritures
- **Modèles différents** : La structure de lecture est très différente de l'écriture
- **Scalabilité** : Besoin de scaler lecture et écriture indépendamment
- **Performance** : Optimisations spécifiques pour chaque côté

### ❌ Évitez Event Sourcing/CQRS quand :

- **Application CRUD simple** : Surcharge inutile
- **Petite équipe** : Complexité trop élevée
- **Cohérence forte requise** : Les données doivent être immédiatement cohérentes
- **Prototypage rapide** : Ralentit le développement initial
- **Pas de besoin d'historique** : Seul l'état actuel importe

## Comparaison avec approche traditionnelle

| Aspect | Traditionnel (CRUD) | Event Sourcing + CQRS |
|--------|---------------------|----------------------|
| **Complexité** | ⭐⭐ Simple | ⭐⭐⭐⭐⭐ Complexe |
| **Audit** | ⭐⭐ Limité | ⭐⭐⭐⭐⭐ Complet |
| **Performance lecture** | ⭐⭐⭐ Moyenne | ⭐⭐⭐⭐⭐ Excellente |
| **Performance écriture** | ⭐⭐⭐⭐ Bonne | ⭐⭐⭐ Moyenne |
| **Cohérence** | ⭐⭐⭐⭐⭐ Forte | ⭐⭐⭐ Éventuelle |
| **Debugging** | ⭐⭐ Difficile | ⭐⭐⭐⭐ Facile |
| **Stockage** | ⭐⭐⭐⭐ Efficace | ⭐⭐ Volumineux |
| **Courbe d'apprentissage** | ⭐⭐ Facile | ⭐⭐⭐⭐⭐ Difficile |

## Conclusion

**Event Sourcing** et **CQRS** sont des patterns puissants mais complexes qui transforment radicalement la façon de concevoir les applications.

### Points clés à retenir :

1. **Event Sourcing** stocke tous les changements comme des événements immuables
2. **CQRS** sépare les opérations de lecture et d'écriture
3. Les deux peuvent être utilisés **indépendamment** ou **ensemble**
4. Apportent un **audit complet** et une **scalabilité** excellente
5. Introduisent de la **complexité** et de la **cohérence éventuelle**

### Recommandations :

- **Commencez simple** : CRUD traditionnel pour MVP
- **Ajoutez Event Sourcing** si l'audit est critique
- **Ajoutez CQRS** si la performance de lecture est problématique
- **Formez votre équipe** : Ces patterns demandent de l'expertise
- **Documentez** : L'architecture doit être claire pour tous

**FreePascal est parfaitement adapté** pour implémenter ces patterns grâce à :
- Son typage fort qui modélise bien les événements
- Ses interfaces pour l'abstraction
- Sa performance native pour gérer le volume d'événements
- Sa gestion mémoire maîtrisée

Ces patterns ne sont **pas nécessaires pour tous les projets**, mais quand les besoins métier le justifient, ils offrent des capacités uniques d'audit, d'analyse et de scalabilité que les approches traditionnelles ne peuvent fournir. 🎯

⏭️ [Hexagonal Architecture](/21-architecture-logicielle-avancee/04-hexagonal-architecture.md)
