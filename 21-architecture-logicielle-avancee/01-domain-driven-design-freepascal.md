🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 21.1 Domain-Driven Design avec FreePascal

## Introduction au Domain-Driven Design

Le **Domain-Driven Design** (DDD), ou conception pilotée par le domaine, est une approche de développement logiciel qui place le métier et sa logique au cœur du système. Créée par Eric Evans dans son livre fondateur "Domain-Driven Design: Tackling Complexity in the Heart of Software" (2003), cette méthodologie vise à créer un langage commun entre développeurs et experts métier.

### Pourquoi utiliser DDD ?

Dans les projets complexes, la difficulté principale n'est pas technique mais conceptuelle : comprendre et modéliser correctement le domaine métier. DDD propose des outils et des patterns pour :

- **Aligner le code avec le métier** : le code reflète fidèlement les concepts métier
- **Gérer la complexité** : diviser un grand système en parties compréhensibles
- **Faciliter la communication** : créer un vocabulaire partagé entre tous les acteurs
- **Maintenir l'évolutivité** : adapter facilement le système aux changements métier

### FreePascal et DDD : un mariage pertinent

FreePascal est particulièrement adapté au DDD grâce à :

- Son **typage fort** qui permet de modéliser précisément les concepts métier
- Ses **classes et interfaces** pour structurer le domaine
- Sa **gestion mémoire** claire qui évite les surprises
- Sa **compilation native** pour des performances optimales
- Sa **portabilité** Windows/Linux pour des systèmes d'entreprise

## Concepts fondamentaux du DDD

### 1. Le Langage Ubiquitaire (Ubiquitous Language)

Le langage ubiquitaire est un vocabulaire partagé entre développeurs et experts métier. Chaque terme doit avoir **une seule signification** comprise par tous.

**Exemple dans le domaine bancaire :**
- ❌ Mauvais : `TData`, `TInfo`, `TItem`
- ✅ Bon : `TCompte`, `TTransaction`, `TClient`

```pascal
// Le code utilise exactement les termes métier
type
  TCompte = class
  private
    FNumero: string;
    FSolde: Currency;
  public
    procedure Crediter(Montant: Currency);
    procedure Debiter(Montant: Currency);
  end;
```

### 2. Les Entités (Entities)

Une **entité** est un objet défini par son **identité** plutôt que par ses attributs. Deux entités sont différentes même si elles ont les mêmes valeurs.

**Caractéristiques d'une entité :**
- Possède un identifiant unique
- A un cycle de vie
- Peut changer d'état au fil du temps

```pascal
type
  TClient = class
  private
    FId: TGUID;              // Identité unique
    FNom: string;
    FPrenom: string;
    FEmail: string;
    FDateInscription: TDateTime;
  public
    constructor Create(const AId: TGUID; const ANom, APrenom: string);

    property Id: TGUID read FId;
    property Nom: string read FNom write FNom;
    property Email: string read FEmail write FEmail;

    // Les clients sont égaux si leurs IDs sont égaux
    function Equals(Obj: TObject): Boolean; override;
  end;

implementation

constructor TClient.Create(const AId: TGUID; const ANom, APrenom: string);  
begin
  inherited Create;
  FId := AId;
  FNom := ANom;
  FPrenom := APrenom;
  FDateInscription := Now;
end;

function TClient.Equals(Obj: TObject): Boolean;  
begin
  if Obj is TClient then
    Result := IsEqualGUID(FId, TClient(Obj).Id)
  else
    Result := False;
end;
```

### 3. Les Value Objects (Objets Valeur)

Un **value object** est défini par ses attributs, pas par une identité. Deux objets valeur sont égaux s'ils ont les mêmes valeurs.

**Caractéristiques d'un value object :**
- Pas d'identité
- Immutable (ne change jamais après création)
- Égalité basée sur les valeurs
- Peut être partagé librement

```pascal
type
  TAdresse = class
  private
    FRue: string;
    FVille: string;
    FCodePostal: string;
    FPays: string;
  public
    constructor Create(const ARue, AVille, ACodePostal, APays: string);

    // Propriétés en lecture seule = immutable
    property Rue: string read FRue;
    property Ville: string read FVille;
    property CodePostal: string read FCodePostal;
    property Pays: string read FPays;

    // Égalité basée sur toutes les valeurs
    function Equals(Obj: TObject): Boolean; override;
    function ToString: string; override;
  end;

implementation

constructor TAdresse.Create(const ARue, AVille, ACodePostal, APays: string);  
begin
  inherited Create;
  FRue := ARue;
  FVille := AVille;
  FCodePostal := ACodePostal;
  FPays := APays;
end;

function TAdresse.Equals(Obj: TObject): Boolean;  
var
  Other: TAdresse;
begin
  Result := False;
  if Obj is TAdresse then
  begin
    Other := TAdresse(Obj);
    Result := (FRue = Other.Rue) and
              (FVille = Other.Ville) and
              (FCodePostal = Other.CodePostal) and
              (FPays = Other.Pays);
  end;
end;

function TAdresse.ToString: string;  
begin
  Result := Format('%s, %s %s, %s', [FRue, FCodePostal, FVille, FPays]);
end;
```

### 4. Les Agrégats (Aggregates)

Un **agrégat** est un groupe d'objets liés traités comme une unité. Un agrégat possède :

- Une **racine d'agrégat** (Aggregate Root) : le seul point d'entrée
- Des **frontières** : définit ce qui est dedans ou dehors
- Des **règles d'invariance** : garanties de cohérence

```pascal
type
  // Racine d'agrégat : Commande
  TCommande = class
  private
    FId: TGUID;
    FClient: TClient;
    FLignes: TObjectList<TLigneCommande>;  // Entités internes
    FStatut: TStatutCommande;
    FDateCreation: TDateTime;

    // Méthode privée : validation des règles métier
    procedure ValiderReglesMetier;
  public
    constructor Create(const AClient: TClient);
    destructor Destroy; override;

    // Opérations métier via la racine uniquement
    procedure AjouterLigne(const AProduit: TProduit; AQuantite: Integer);
    procedure SupprimerLigne(const ANumeroLigne: Integer);
    procedure Valider;
    procedure Annuler;

    function CalculerTotal: Currency;

    property Id: TGUID read FId;
    property Statut: TStatutCommande read FStatut;
  end;

  // Entité interne à l'agrégat
  TLigneCommande = class
  private
    FNumero: Integer;
    FProduit: TProduit;
    FQuantite: Integer;
    FPrixUnitaire: Currency;
  public
    constructor Create(ANumero: Integer; const AProduit: TProduit;
                      AQuantite: Integer);

    function CalculerSousTotal: Currency;

    property Numero: Integer read FNumero;
    property Quantite: Integer read FQuantite write FQuantite;
  end;

implementation

constructor TCommande.Create(const AClient: TClient);  
begin
  inherited Create;
  FId := TGUID.NewGuid;
  FClient := AClient;
  FLignes := TObjectList<TLigneCommande>.Create(True);
  FStatut := TStatutCommande.Brouillon;
  FDateCreation := Now;
end;

destructor TCommande.Destroy;  
begin
  FLignes.Free;
  inherited;
end;

procedure TCommande.AjouterLigne(const AProduit: TProduit; AQuantite: Integer);  
var
  Ligne: TLigneCommande;
begin
  // Règle métier : ne peut modifier qu'une commande en brouillon
  if FStatut <> TStatutCommande.Brouillon then
    raise ECommandeException.Create('Impossible de modifier une commande validée');

  // Règle métier : quantité positive
  if AQuantite <= 0 then
    raise ECommandeException.Create('La quantité doit être positive');

  Ligne := TLigneCommande.Create(FLignes.Count + 1, AProduit, AQuantite);
  FLignes.Add(Ligne);
end;

procedure TCommande.Valider;  
begin
  // Règle métier : au moins une ligne
  if FLignes.Count = 0 then
    raise ECommandeException.Create('Une commande doit contenir au moins une ligne');

  // Règle métier : statut cohérent
  if FStatut <> TStatutCommande.Brouillon then
    raise ECommandeException.Create('Seule une commande en brouillon peut être validée');

  FStatut := TStatutCommande.Validee;
end;

function TCommande.CalculerTotal: Currency;  
var
  Ligne: TLigneCommande;
begin
  Result := 0;
  for Ligne in FLignes do
    Result := Result + Ligne.CalculerSousTotal;
end;
```

**Principe clé :** On n'accède jamais directement aux lignes de commande depuis l'extérieur. Toutes les opérations passent par la racine (`TCommande`), qui garantit la cohérence.

### 5. Les Services de Domaine

Un **service de domaine** contient de la logique métier qui ne rentre naturellement dans aucune entité ou value object.

**Quand créer un service de domaine ?**
- L'opération concerne plusieurs agrégats
- L'opération n'a pas d'état propre
- L'opération est décrite avec un verbe du métier

```pascal
type
  // Interface du service
  IServiceTransfert = interface
    ['{B5E3D1A2-8F4C-4E5D-A1B2-C3D4E5F6A7B8}']
    procedure TransfererArgent(
      const ACompteSource: TCompte;
      const ACompteDestination: TCompte;
      AMontant: Currency);
  end;

  // Implémentation du service de domaine
  TServiceTransfert = class(TInterfacedObject, IServiceTransfert)
  private
    FRepositoryCompte: IRepositoryCompte;
    FJournalEvenements: IJournalEvenements;
  public
    constructor Create(
      const ARepoCompte: IRepositoryCompte;
      const AJournal: IJournalEvenements);

    procedure TransfererArgent(
      const ACompteSource: TCompte;
      const ACompteDestination: TCompte;
      AMontant: Currency);
  end;

implementation

procedure TServiceTransfert.TransfererArgent(
  const ACompteSource: TCompte;
  const ACompteDestination: TCompte;
  AMontant: Currency);
begin
  // Règle métier : montant positif
  if AMontant <= 0 then
    raise ETransfertException.Create('Le montant doit être positif');

  // Règle métier : comptes différents
  if ACompteSource.Numero = ACompteDestination.Numero then
    raise ETransfertException.Create('Les comptes doivent être différents');

  // Opération coordonnée sur deux agrégats
  ACompteSource.Debiter(AMontant);
  ACompteDestination.Crediter(AMontant);

  // Persistence et événement
  FRepositoryCompte.Sauvegarder(ACompteSource);
  FRepositoryCompte.Sauvegarder(ACompteDestination);

  FJournalEvenements.Publier(
    TEvenementTransfert.Create(ACompteSource, ACompteDestination, AMontant)
  );
end;
```

### 6. Les Repositories (Dépôts)

Un **repository** gère la persistance et la récupération des agrégats. Il fait abstraction de la base de données.

**Caractéristiques d'un repository :**
- Une interface pour chaque racine d'agrégat
- Simule une collection en mémoire
- Cache les détails de stockage

```pascal
type
  // Interface du repository (couche domaine)
  IRepositoryCommande = interface
    ['{C1D2E3F4-5A6B-7C8D-9E0F-1A2B3C4D5E6F}']
    procedure Ajouter(const ACommande: TCommande);
    procedure Sauvegarder(const ACommande: TCommande);
    procedure Supprimer(const AId: TGUID);
    function TrouverParId(const AId: TGUID): TCommande;
    function TrouverParClient(const AClientId: TGUID): TList<TCommande>;
  end;

  // Implémentation concrète (couche infrastructure)
  TRepositoryCommandeSQL = class(TInterfacedObject, IRepositoryCommande)
  private
    FConnection: TSQLConnection;

    function MapperVersCommande(ADataSet: TDataSet): TCommande;
    procedure MapperVersDataSet(const ACommande: TCommande; ADataSet: TDataSet);
  public
    constructor Create(AConnection: TSQLConnection);

    procedure Ajouter(const ACommande: TCommande);
    procedure Sauvegarder(const ACommande: TCommande);
    procedure Supprimer(const AId: TGUID);
    function TrouverParId(const AId: TGUID): TCommande;
    function TrouverParClient(const AClientId: TGUID): TList<TCommande>;
  end;

implementation

function TRepositoryCommandeSQL.TrouverParId(const AId: TGUID): TCommande;  
var
  Query: TSQLQuery;
begin
  Result := nil;
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FConnection;
    Query.SQL.Text :=
      'SELECT * FROM Commandes WHERE Id = :Id';
    Query.ParamByName('Id').AsString := GUIDToString(AId);
    Query.Open;

    if not Query.EOF then
      Result := MapperVersCommande(Query);
  finally
    Query.Free;
  end;
end;

procedure TRepositoryCommandeSQL.Sauvegarder(const ACommande: TCommande);  
var
  Query: TSQLQuery;
begin
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FConnection;

    // UPDATE si existe, INSERT sinon
    Query.SQL.Text :=
      'UPDATE Commandes SET ' +
      '  ClientId = :ClientId, ' +
      '  Statut = :Statut, ' +
      '  DateModification = :DateModif ' +
      'WHERE Id = :Id';

    MapperVersDataSet(ACommande, Query);
    Query.ExecSQL;

    // Sauvegarder aussi les lignes de commande...
  finally
    Query.Free;
  end;
end;
```

### 7. Les Factories (Fabriques)

Une **factory** encapsule la logique complexe de création d'objets.

```pascal
type
  TCommandeFactory = class
  public
    class function CreerCommande(
      const AClient: TClient;
      const AProduits: array of TProduit;
      const AQuantites: array of Integer): TCommande;

    class function ReconstituerDepuisBase(
      const AId: TGUID;
      const AClient: TClient;
      const ALignes: TList<TLigneCommande>;
      AStatut: TStatutCommande): TCommande;
  end;

implementation

class function TCommandeFactory.CreerCommande(
  const AClient: TClient;
  const AProduits: array of TProduit;
  const AQuantites: array of Integer): TCommande;
var
  i: Integer;
begin
  // Validation
  if Length(AProduits) <> Length(AQuantites) then
    raise ECommandeException.Create('Tableaux de tailles différentes');

  // Création
  Result := TCommande.Create(AClient);
  try
    for i := Low(AProduits) to High(AProduits) do
      Result.AjouterLigne(AProduits[i], AQuantites[i]);
  except
    Result.Free;
    raise;
  end;
end;
```

## Architecture en couches DDD avec FreePascal

### Organisation typique d'un projet DDD

```
MonProjet/
├── Domain/              ← Cœur métier (aucune dépendance)
│   ├── Entities/
│   ├── ValueObjects/
│   ├── Aggregates/
│   ├── Services/
│   ├── Repositories/    (interfaces uniquement)
│   └── Exceptions/
├── Application/         ← Use cases et orchestration
│   ├── UseCases/
│   ├── DTOs/
│   └── Services/
├── Infrastructure/      ← Implémentations techniques
│   ├── Persistence/     (repositories SQL)
│   ├── External/        (APIs tierces)
│   └── Messaging/
└── Presentation/        ← Interface utilisateur
    ├── GUI/             (Lazarus LCL)
    ├── Web/             (fpWeb)
    └── CLI/
```

### Exemple : Structure de fichiers FreePascal

```pascal
// Domain/Entities/Commande.pas
unit Domain.Entities.Commande;

interface

type
  TCommande = class
    // ... implémentation
  end;

implementation

end.

// Domain/Repositories/IRepositoryCommande.pas
unit Domain.Repositories.IRepositoryCommande;

interface

type
  IRepositoryCommande = interface
    // ... méthodes
  end;

implementation

end.

// Infrastructure/Persistence/RepositoryCommandeSQL.pas
unit Infrastructure.Persistence.RepositoryCommandeSQL;

interface

uses
  Domain.Entities.Commande,
  Domain.Repositories.IRepositoryCommande,
  SQLDB;

type
  TRepositoryCommandeSQL = class(TInterfacedObject, IRepositoryCommande)
    // ... implémentation
  end;

implementation

end.
```

### Injection de dépendances en FreePascal

```pascal
type
  // Container d'injection de dépendances simple
  TServiceContainer = class
  private
    FServices: TDictionary<string, IInterface>;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Enregistrer(const ANom: string; const AService: IInterface);
    function Resoudre(const ANom: string): IInterface;
  end;

// Configuration au démarrage
procedure ConfigurerServices(Container: TServiceContainer);  
var
  Connection: TSQLConnection;
  RepoCommande: IRepositoryCommande;
  ServiceCommande: IServiceCommande;
begin
  // Infrastructure
  Connection := TSQLConnection.Create(nil);
  Connection.DatabaseName := 'MaBase';

  RepoCommande := TRepositoryCommandeSQL.Create(Connection);
  Container.Enregistrer('IRepositoryCommande', RepoCommande);

  // Application
  ServiceCommande := TServiceCommande.Create(RepoCommande);
  Container.Enregistrer('IServiceCommande', ServiceCommande);
end;
```

## Patterns DDD avancés en FreePascal

### Domain Events (Événements de Domaine)

Les événements de domaine représentent des faits métier significatifs.

```pascal
type
  // Événement de base
  IEvenementDomaine = interface
    ['{D1E2F3A4-B5C6-D7E8-F9A0-B1C2D3E4F5A6}']
    function ObtenirDateOccurrence: TDateTime;
    function ObtenirType: string;
  end;

  // Événement concret
  TEvenementCommandeValidee = class(TInterfacedObject, IEvenementDomaine)
  private
    FCommandeId: TGUID;
    FClientId: TGUID;
    FMontantTotal: Currency;
    FDateOccurrence: TDateTime;
  public
    constructor Create(const ACommande: TCommande);

    function ObtenirDateOccurrence: TDateTime;
    function ObtenirType: string;

    property CommandeId: TGUID read FCommandeId;
    property MontantTotal: Currency read FMontantTotal;
  end;

  // Gestionnaire d'événements
  IEcouteurEvenement = interface
    ['{E2F3A4B5-C6D7-E8F9-A0B1-C2D3E4F5A6B7}']
    procedure Gerer(const AEvenement: IEvenementDomaine);
  end;

  // Dispatcher d'événements
  TDispatcherEvenements = class
  private
    FEcouteurs: TDictionary<string, TList<IEcouteurEvenement>>;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AbonnerEcouteur(const ATypeEvenement: string;
                             const AEcouteur: IEcouteurEvenement);
    procedure PublierEvenement(const AEvenement: IEvenementDomaine);
  end;
```

### Specifications (Spécifications)

Le pattern Specification encapsule les règles métier réutilisables.

```pascal
type
  // Specification de base
  ISpecification<T> = interface
    ['{F3A4B5C6-D7E8-F9A0-B1C2-D3E4F5A6B7C8}']
    function EstSatisfaitePar(const AObjet: T): Boolean;
    function Et(const AAutre: ISpecification<T>): ISpecification<T>;
    function Ou(const AAutre: ISpecification<T>): ISpecification<T>;
    function Non: ISpecification<T>;
  end;

  // Specification concrète
  TSpecificationClientVIP = class(TInterfacedObject, ISpecification<TClient>)
  private
    FMontantMinimum: Currency;
  public
    constructor Create(AMontantMinimum: Currency);
    function EstSatisfaitePar(const AClient: TClient): Boolean;
    // Implémentation Et, Ou, Non...
  end;

  TSpecificationClientActif = class(TInterfacedObject, ISpecification<TClient>)
  public
    function EstSatisfaitePar(const AClient: TClient): Boolean;
  end;

// Utilisation
var
  SpecVIP, SpecActif, SpecCombinee: ISpecification<TClient>;
  Client: TClient;
begin
  SpecVIP := TSpecificationClientVIP.Create(10000);
  SpecActif := TSpecificationClientActif.Create;

  // Combinaison de spécifications
  SpecCombinee := SpecVIP.Et(SpecActif);

  if SpecCombinee.EstSatisfaitePar(Client) then
    // Appliquer remise VIP...
end;
```

## Bonnes pratiques DDD avec FreePascal

### 1. Protéger les invariants

```pascal
type
  TCompte = class
  private
    FSolde: Currency;
    FDecouvertAutorise: Currency;

    procedure ValiderSoldeMinimum(NouveauSolde: Currency);
  public
    procedure Debiter(Montant: Currency);

    property Solde: Currency read FSolde;  // Lecture seule !
  end;

procedure TCompte.ValiderSoldeMinimum(NouveauSolde: Currency);  
begin
  if NouveauSolde < -FDecouvertAutorise then
    raise ECompteException.Create('Découvert dépassé');
end;

procedure TCompte.Debiter(Montant: Currency);  
var
  NouveauSolde: Currency;
begin
  NouveauSolde := FSolde - Montant;
  ValiderSoldeMinimum(NouveauSolde);  // Validation avant modification
  FSolde := NouveauSolde;
end;
```

### 2. Utiliser des types fortement typés

```pascal
// ❌ Mauvais : types primitifs
procedure TraiterCommande(ClientId: Integer; Montant: Double);

// ✅ Bon : types métier
type
  TClientId = record
    Valeur: TGUID;
  end;

  TMontant = record
  private
    FValeur: Currency;
  public
    constructor Create(AValeur: Currency);
    property Valeur: Currency read FValeur;
  end;

procedure TraiterCommande(const ClientId: TClientId; const Montant: TMontant);
```

### 3. Rendre les value objects immutables

```pascal
type
  TEmail = class
  private
    FAdresse: string;

    constructor CreateInternal(const AAdresse: string);
  public
    class function Creer(const AAdresse: string): TEmail;

    property Adresse: string read FAdresse;  // Pas de setter

    function Equals(Obj: TObject): Boolean; override;
  end;

class function TEmail.Creer(const AAdresse: string): TEmail;  
begin
  // Validation
  if Pos('@', AAdresse) = 0 then
    raise EEmailInvalideException.Create('Email invalide');

  Result := TEmail.CreateInternal(AAdresse);
end;
```

### 4. Séparer les modèles de lecture et d'écriture

```pascal
// Modèle de domaine (écriture)
type
  TCommande = class
    // Logique métier complexe
  end;

// DTO de lecture (requêtes)
type
  TCommandeDTO = record
    Id: string;
    ClientNom: string;
    DateCommande: TDateTime;
    MontantTotal: Currency;
    StatutLibelle: string;
  end;

  IServiceLectureCommande = interface
    function ObtenirCommandesClient(const ClientId: TGUID): TArray<TCommandeDTO>;
    function ObtenirDetailCommande(const CommandeId: TGUID): TCommandeDTO;
  end;
```

## Conclusion

Le Domain-Driven Design avec FreePascal permet de créer des applications robustes et maintenables en :

1. **Alignant le code avec le métier** grâce au langage ubiquitaire
2. **Protégeant l'intégrité** via les agrégats et leurs invariants
3. **Isolant la complexité** dans des couches bien définies
4. **Facilitant l'évolution** par une architecture flexible

FreePascal offre tous les outils nécessaires (classes, interfaces, génériques) pour implémenter efficacement les patterns DDD, tout en bénéficiant de la performance et de la portabilité native.

**Points clés à retenir :**

- Les **entités** ont une identité, les **value objects** sont immutables
- Les **agrégats** protègent les règles métier
- Les **repositories** abstraient la persistance
- Les **services de domaine** orchestrent les opérations complexes
- L'architecture en couches maintient la séparation des responsabilités

Le DDD n'est pas nécessaire pour tous les projets, mais pour les systèmes complexes avec une logique métier riche, il offre une structure éprouvée qui améliore considérablement la qualité et la pérennité du code.

⏭️ [Microservices et architectures distribuées](/21-architecture-logicielle-avancee/02-microservices-architectures-distribuees.md)
